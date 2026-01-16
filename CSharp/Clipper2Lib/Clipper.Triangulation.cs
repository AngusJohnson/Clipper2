/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  13 December 2025                                                *
* Release   :  BETA RELEASE                                                    *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2025                                         *
* Purpose   :  Constrained Delaunay Triangulation                              *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************/

using System;
using System.Collections.Generic;

#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif
{
  public enum TriangulateResult { success, fail, noPolygons, pathsIntersect }

  // -------------------------------------------------------------------------
  // Internal triangulation helpers
  // -------------------------------------------------------------------------

  internal enum EdgeKind { loose, ascend, descend } // ascend & descend are 'fixed' edges
  internal enum IntersectKind { none, collinear, intersect }
  internal enum EdgeContainsResult { neither, left, right }

  internal class Vertex2
  {
    public Point64 pt;
    public List<Edge> edges = new List<Edge>();
    public bool innerLM = false;

    public Vertex2(Point64 p64)
    {
      pt = p64;
      edges. Capacity = 2;
    }
  }

  internal class Edge
  {
      public Vertex2 vL = null!;
      public Vertex2 vR = null!;
      public Vertex2 vB = null!;
      public Vertex2 vT = null!;
      public EdgeKind kind = EdgeKind.loose;
      public Triangle? triA = null;
      public Triangle? triB = null;
      public bool isActive = false;
      public Edge? nextE = null;
      public Edge? prevE = null;
  }

  internal class Triangle
  {
    public Edge[] edges = new Edge[3];

    public Triangle(Edge e1, Edge e2, Edge e3)
    {
      edges[0] = e1;
      edges[1] = e2;
      edges[2] = e3;
    }
  }

  // -------------------------------------------------------------------------
  // Delaunay class declaration & implementation
  // -------------------------------------------------------------------------

  internal class Delaunay
  {
    private readonly List<Vertex2> allVertices = new List<Vertex2>();
    private readonly List<Edge> allEdges = new List<Edge>();
    private readonly List<Triangle> allTriangles = new List<Triangle>();
    private readonly Stack<Edge> pendingDelaunayStack = new Stack<Edge>();
    private readonly Stack<Edge> horzEdgeStack = new Stack<Edge>();
    private readonly Stack<Vertex2> locMinStack = new Stack<Vertex2>();
    private readonly bool useDelaunay;
    private Edge? firstActive = null;
    private Vertex2? lowermostVertex = null;

    public Delaunay(bool delaunay = true)
    {
      useDelaunay = delaunay;
    }

    private void AddPath(Path64 path)
    {
      int len = path.Count;
      if (len == 0) return;

      int i0 = 0, iPrev, iNext;

      if (!FindLocMinIdx(path, len, ref i0)) return;

      iPrev = Prev(i0, len);
      while (path[iPrev].Equals(path[i0]))
        iPrev = Prev(iPrev, len);

      iNext = Next(i0, len);

      int i = i0;
      while (InternalClipper.CrossProductSign(path[iPrev], path[i], path[iNext]) == 0)
      {
        FindLocMinIdx(path, len, ref i);
        if (i == i0) return; // entirely collinear path
        iPrev = (int) Prev(i, len);
        while (path[iPrev].Equals(path[i]))
          iPrev = Prev(iPrev, len);
        iNext = Next(i, len);
      }

      int vert_cnt = allVertices.Count;
      Vertex2 v0 = new Vertex2(path[i]);
      allVertices.Add(v0);

      if (LeftTurning(path[iPrev], path[i], path[iNext]))
        v0.innerLM = true;

      Vertex2 vPrev = v0;
      i = iNext;

      for (; ; )
      {
        // vPrev is a locMin here
        locMinStack.Push(vPrev);
        // ? update lowermostVertex ...
        if (lowermostVertex == null ||
          vPrev.pt.Y > lowermostVertex.pt.Y ||
          (vPrev.pt.Y == lowermostVertex.pt.Y &&
          vPrev.pt.X < lowermostVertex.pt.X))
            lowermostVertex = vPrev;

        iNext = Next(i, len);
        if (InternalClipper.CrossProductSign(vPrev.pt, path[i], path[iNext]) == 0)
        {
          i = iNext;
          continue;
        }

        // ascend up next bound to LocMax
        while (path[i].Y <= vPrev.pt.Y)
        {
          Vertex2 v = new Vertex2(path[i]);
          allVertices.Add(v);
          CreateEdge(vPrev, v, EdgeKind.ascend);
          vPrev = v;
          i = iNext;
          iNext = Next(i, len);

          while (InternalClipper.CrossProductSign(vPrev.pt, path[i], path[iNext]) == 0)
          {
            i = iNext;
            iNext = Next(i, len);
          }
        }

        // Now at a locMax, so descend to next locMin
        Vertex2 vPrevPrev = vPrev;
        while (i != i0 && path[i].Y >= vPrev.pt.Y)
        {
          Vertex2 v = new Vertex2(path[i]);
          allVertices.Add(v);
          CreateEdge(v, vPrev, EdgeKind.descend);
          vPrevPrev = vPrev;
          vPrev = v;
          i = iNext;
          iNext = Next(i, len);

          while (InternalClipper.CrossProductSign(vPrev.pt, path[i], path[iNext]) == 0)
          {
            i = iNext;
            iNext = Next(i, len);
          }
        }

        // now at the next locMin
        if (i == i0) break;
        if (LeftTurning(vPrevPrev.pt, vPrev.pt, path[i]))
          vPrev.innerLM = true;
      }

      CreateEdge(v0, vPrev, EdgeKind.descend);

      // finally, ignore this path if is not a polygon or too small
      len = allVertices.Count - vert_cnt;
      int idx = vert_cnt;
      if (len < 3 || (len == 3 &&
          ((DistSqr(allVertices[idx].pt, allVertices[idx + 1].pt) <= 1) ||
           (DistSqr(allVertices[idx + 1].pt, allVertices[idx + 2].pt) <= 1) ||
           (DistSqr(allVertices[idx + 2].pt, allVertices[idx].pt) <= 1))))
      {
        for (int j = vert_cnt; j < allVertices.Count; ++j)
          allVertices[j].edges.Clear(); // flag to ignore
      }
    }

    private bool AddPaths(Paths64 paths)
    {
      int totalVertexCount = 0;
      foreach (Path64 path in paths)
        totalVertexCount += path.Count;
      if (totalVertexCount == 0) return false;

      allVertices.Capacity = allVertices.Count + totalVertexCount;
      allEdges.Capacity = allEdges.Count + totalVertexCount;

      foreach (Path64 path in paths)
        AddPath(path);

      return allVertices.Count > 2;
    }

    private void CleanUp()
    {
      allVertices.Clear();
      allEdges.Clear();
      allTriangles.Clear();
      pendingDelaunayStack.Clear();
      horzEdgeStack.Clear();
      locMinStack.Clear();

      firstActive = null;
      lowermostVertex = null;
    }

    private bool FixupEdgeIntersects()
    {
      // precondition - edgeList must be sorted - ascending on edge.vL.pt.X

      for (int i1 = 0; i1 < allEdges.Count; ++i1)
      {
        Edge e1 = allEdges[i1];
        for (int i2 = i1 + 1; i2 < allEdges.Count; ++i2)
        {
          Edge e2 = allEdges[i2];
          if (e2.vL.pt.X >= e1.vR.pt.X)
            break;

          if (e2.vT.pt.Y < e1.vB.pt.Y && e2.vB.pt.Y > e1.vT.pt.Y &&
              SegsIntersect(e2.vL.pt, e2.vR.pt, e1.vL.pt, e1.vR.pt) == IntersectKind.intersect)
          {
            if (!RemoveIntersection(e2, e1))
              return false;
          }
        }
      }
      return true;
    }

    private void MergeDupOrCollinearVertices()
    {
      if (allVertices.Count < 2) return;

      int v1Index = 0;
      for (int v2Index = 1; v2Index < allVertices.Count; ++v2Index)
      {
        Vertex2 v1 = allVertices[v1Index];
        Vertex2 v2 = allVertices[v2Index];

        if (!v1.pt.Equals(v2.pt))
        {
          v1Index = v2Index;
          continue;
        }

        // merge v1 & v2
        if (!v1.innerLM || !v2.innerLM)
          v1.innerLM = false;

        foreach (Edge e in v2.edges)
        {
          if (e.vB == v2) e.vB = v1; else e.vT = v1;
          if (e.vL == v2) e.vL = v1; else e.vR = v1;
        }

        v1.edges.AddRange(v2.edges);
        v2.edges.Clear();

        // excluding horizontals, if pv.edges contains two edges
        // that are collinear and share the same bottom coords
        // but have different lengths, split the longer edge at
        // the top of the shorter edge ...
        for (int iE = 0; iE < v1.edges.Count; ++iE)
        {
          Edge e1 = v1.edges[iE];
          if (IsHorizontal(e1) || e1.vB != v1) continue;

          for (int iE2 = iE + 1; iE2 < v1.edges.Count; ++iE2)
          {
            Edge e2 = v1.edges[iE2];
            if (e2.vB != v1 || e1.vT.pt.Y == e2.vT.pt.Y ||
                InternalClipper.CrossProductSign(e1.vT.pt, v1.pt, e2.vT.pt) != 0)
              continue;

            // parallel edges from v1 up
            if (e1.vT.pt.Y < e2.vT.pt.Y) SplitEdge(e1, e2);
            else SplitEdge(e2, e1);
            break; // only two can be collinear
          }
        }
      }
    }

    private void SplitEdge(Edge longE, Edge shortE)
    {
      Vertex2 oldT = longE.vT;
      Vertex2 newT = shortE.vT;

      RemoveEdgeFromVertex(oldT, longE);

      longE.vT = newT;
      if (longE.vL == oldT) longE.vL = newT; else longE.vR = newT;

      newT.edges.Add(longE);

      CreateEdge(newT, oldT, longE.kind);
    }

    private bool RemoveIntersection(Edge e1, Edge e2)
    {
      Vertex2 v = e1.vL;
      Edge tmpE = e2;

      double d = ShortestDistFromSegment(e1.vL.pt, e2.vL.pt, e2.vR.pt);
      double d2 = ShortestDistFromSegment(e1.vR.pt, e2.vL.pt, e2.vR.pt);
      if (d2 < d) { d = d2; v = e1.vR; }

      d2 = ShortestDistFromSegment(e2.vL.pt, e1.vL.pt, e1.vR.pt);
      if (d2 < d) { d = d2; tmpE = e1; v = e2.vL; }

      d2 = ShortestDistFromSegment(e2.vR.pt, e1.vL.pt, e1.vR.pt);
      if (d2 < d) { d = d2; tmpE = e1; v = e2.vR; }

      if (d > 1.0)
        return false; // not a simple rounding intersection

      Vertex2 v2 = tmpE.vT;
      RemoveEdgeFromVertex(v2, tmpE);

      if (tmpE.vL == v2) tmpE.vL = v; else tmpE.vR = v;
      tmpE.vT = v;
      v.edges.Add(tmpE);
      v.innerLM = false;

      if (tmpE.vB.innerLM && GetLocMinAngle(tmpE.vB) <= 0)
        tmpE.vB.innerLM = false;

      CreateEdge(v, v2, tmpE.kind);
      return true;
    }

    private Edge CreateEdge(Vertex2 v1, Vertex2 v2, EdgeKind k)
    {
      Edge res = new Edge();
      allEdges.Add(res);

      if (v1.pt.Y == v2.pt.Y)
      {
        res.vB = v1;
        res.vT = v2;
      }
      else if (v1.pt.Y < v2.pt.Y)
      {
        res.vB = v2;
        res.vT = v1;
      }
      else
      {
        res.vB = v1;
        res.vT = v2;
      }

      if (v1.pt.X <= v2.pt.X)
      {
        res.vL = v1;
        res.vR = v2;
      }
      else
      {
        res.vL = v2;
        res.vR = v1;
      }

      res.kind = k;
      v1.edges.Add(res);
      v2.edges.Add(res);

      if (k == EdgeKind.loose)
      {
        pendingDelaunayStack.Push(res);
        AddEdgeToActives(res);
      }

      return res;
    }

    private Triangle CreateTriangle(Edge e1, Edge e2, Edge e3)
    {
      Triangle tri = new Triangle(e1, e2, e3);
      allTriangles.Add(tri);

      for (int i = 0; i < 3; ++i)
      {
        Edge e = tri.edges[i];
        if (e.triA != null)
        {
          e.triB = tri;
          RemoveEdgeFromActives(e);
        }
        else
        {
          e.triA = tri;
          if (!IsLooseEdge(e))
            RemoveEdgeFromActives(e);
        }
      }
      return tri;
    }

    private void ForceLegal(Edge edge)
    {
      if (edge.triA == null || edge.triB == null) return;

      Vertex2? vertA = null;
      Vertex2? vertB = null;

      Edge?[] edgesA = new Edge?[3];
      Edge?[] edgesB = new Edge?[3];
      edgesA[0] = null;
      edgesB[0] = null;

      for (int i = 0; i < 3; ++i)
      {
        if (edge.triA!.edges[i] == edge) continue;
        Edge e = edge.triA.edges[i];
        switch (EdgeContains(e, edge.vL))
        {
          case EdgeContainsResult.left:
            edgesA[1] = e;
            vertA = e.vR;
            break;
          case EdgeContainsResult.right:
            edgesA[1] = e;
            vertA = e.vL;
            break;
          default:
            edgesB[1] = e;
            break;
        }
      }

      for (int i = 0; i < 3; ++i)
      {
        if (edge.triB!.edges[i] == edge) continue;
        Edge e = edge.triB.edges[i];
        switch (EdgeContains(e, edge.vL))
        {
          case EdgeContainsResult.left:
            edgesA[2] = e;
            vertB = e.vR;
            break;
          case EdgeContainsResult.right:
            edgesA[2] = e;
            vertB = e.vL;
            break;
          default:
            edgesB[2] = e;
            break;
        }
      }

      if (vertA == null || vertB == null) return;

      if (InternalClipper.CrossProductSign(vertA.pt, edge.vL.pt, edge.vR.pt) == 0)
        return;

      double ictResult = InCircleTest(vertA.pt, edge.vL.pt, edge.vR.pt, vertB.pt);
      if (ictResult == 0 ||
          (RightTurning(vertA.pt, edge.vL.pt, edge.vR.pt) == (ictResult < 0)))
        return;

      edge.vL = vertA;
      edge.vR = vertB;

      edge.triA!.edges[0] = edge;
      for (int i = 1; i < 3; ++i)
      {
        Edge eAi = edgesA[i]!;
        edge.triA.edges[i] = eAi;
        if (IsLooseEdge(eAi))
          pendingDelaunayStack.Push(eAi);

        if (eAi.triA == edge.triA || eAi.triB == edge.triA) continue;

        if (eAi.triA == edge.triB)
          eAi.triA = edge.triA;
        else if (eAi.triB == edge.triB)
          eAi.triB = edge.triA;
        else
          throw new InvalidOperationException("oops");
      }

      edge.triB!.edges[0] = edge;
      for (int i = 1; i < 3; ++i)
      {
        Edge eBi = edgesB[i]!;
        edge.triB.edges[i] = eBi;
        if (IsLooseEdge(eBi))
          pendingDelaunayStack.Push(eBi);

        if (eBi.triA == edge.triB || eBi.triB == edge.triB) continue;

        if (eBi.triA == edge.triA)
          eBi.triA = edge.triB;
        else if (eBi.triB == edge.triA)
          eBi.triB = edge.triB;
        else
          throw new InvalidOperationException("oops");
      }
    }

    private Edge? CreateInnerLocMinLooseEdge(Vertex2 vAbove)
    {
      if (firstActive == null) return null;

      long xAbove = vAbove.pt.X;
      long yAbove = vAbove.pt.Y;

      Edge? e = firstActive;
      Edge? eBelow = null;
      double bestD = -1.0;

      while (e != null)
      {
        if (e.vL.pt.X <= xAbove && e.vR.pt.X >= xAbove &&
            e.vB.pt.Y >= yAbove && e.vB != vAbove && e.vT != vAbove &&
            !LeftTurning(e.vL.pt, vAbove.pt, e.vR.pt))
        {
          double d = ShortestDistFromSegment(vAbove.pt, e.vL.pt, e.vR.pt);
          if (eBelow == null || d < bestD)
          {
            eBelow = e;
            bestD = d;
          }
        }
        e = e.nextE;
      }

      if (eBelow == null) return null;

      Vertex2 vBest = (eBelow.vT.pt.Y <= yAbove) ? eBelow.vB : eBelow.vT;
      long xBest = vBest.pt.X;
      long yBest = vBest.pt.Y;

      e = firstActive;
      if (xBest < xAbove)
      {
        while (e != null)
        {
          if (e.vR.pt.X > xBest && e.vL.pt.X < xAbove &&
              e.vB.pt.Y > yAbove && e.vT.pt.Y < yBest &&
              SegsIntersect(e.vB.pt, e.vT.pt, vBest.pt, vAbove.pt) == IntersectKind.intersect)
          {
            vBest = (e.vT.pt.Y > yAbove) ? e.vT : e.vB;
            xBest = vBest.pt.X;
            yBest = vBest.pt.Y;
          }
          e = e.nextE;
        }
      }
      else
      {
        while (e != null)
        {
          if (e.vR.pt.X < xBest && e.vL.pt.X > xAbove &&
              e.vB.pt.Y > yAbove && e.vT.pt.Y < yBest &&
              SegsIntersect(e.vB.pt, e.vT.pt, vBest.pt, vAbove.pt) == IntersectKind.intersect)
          {
            vBest = (e.vT.pt.Y > yAbove) ? e.vT : e.vB;
            xBest = vBest.pt.X;
            yBest = vBest.pt.Y;
          }
          e = e.nextE;
        }
      }

      return CreateEdge(vBest, vAbove, EdgeKind.loose);
    }

    private Edge? HorizontalBetween(Vertex2 v1, Vertex2 v2)
    {
      long y = v1.pt.Y;
      long l, r;

      if (v1.pt.X > v2.pt.X)
      {
        l = v2.pt.X;
        r = v1.pt.X;
      }
      else
      {
        l = v1.pt.X;
        r = v2.pt.X;
      }

      Edge? res = firstActive;
      while (res != null)
      {
        if (res.vL.pt.Y == y && res.vR.pt.Y == y &&
            res.vL.pt.X >= l && res.vR.pt.X <= r &&
            (res.vL.pt.X != l || res.vL.pt.X != r))
          break;

        res = res.nextE;
      }
      return res;
    }

    private void DoTriangulateLeft(Edge edge, Vertex2 pivot, long minY)
    {
      Vertex2? vAlt = null;
      Edge? eAlt = null;

      Vertex2 v = (edge.vB == pivot) ? edge.vT : edge.vB;

      foreach (Edge e in pivot.edges)
      {
        if (e == edge || !e.isActive) continue;

        Vertex2 vX = (e.vT == pivot) ? e.vB : e.vT;
        if (vX == v) continue;

        int cps = InternalClipper.CrossProductSign(v.pt, pivot.pt, vX.pt);
        if (cps == 0)
        {
          if ((v.pt.X > pivot.pt.X) == (pivot.pt.X > vX.pt.X)) continue;
        }
        else if (cps > 0 || (vAlt != null && !LeftTurning(vX.pt, pivot.pt, vAlt.pt)))
          continue;

        vAlt = vX;
        eAlt = e;
      }

      if (vAlt == null || vAlt.pt.Y < minY || eAlt == null) return;

      if (vAlt.pt.Y < pivot.pt.Y)
      {
        if (IsLeftEdge(eAlt)) return;
      }
      else if (vAlt.pt.Y > pivot.pt.Y)
      {
        if (IsRightEdge(eAlt)) return;
      }

      Edge? eX = FindLinkingEdge(vAlt, v, (vAlt.pt.Y < v.pt.Y));
      if (eX == null)
      {
        if (vAlt.pt.Y == v.pt.Y && v.pt.Y == minY &&
            HorizontalBetween(vAlt, v) != null)
          return;

        eX = CreateEdge(vAlt, v, EdgeKind.loose);
      }

      CreateTriangle(edge, eAlt, eX);

      if (!EdgeCompleted(eX))
        DoTriangulateLeft(eX, vAlt, minY);
    }

    private void DoTriangulateRight(Edge edge, Vertex2 pivot, long minY)
    {
      Vertex2? vAlt = null;
      Edge? eAlt = null;

      Vertex2 v = (edge.vB == pivot) ? edge.vT : edge.vB;

      foreach (Edge e in pivot.edges)
      {
        if (e == edge || !e.isActive) continue;

        Vertex2 vX = (e.vT == pivot) ? e.vB : e.vT;
        if (vX == v) continue;

        int cps = InternalClipper.CrossProductSign(v.pt, pivot.pt, vX.pt);
        if (cps == 0)
        {
          if ((v.pt.X > pivot.pt.X) == (pivot.pt.X > vX.pt.X)) continue;
        }
        else if (cps < 0 || (vAlt != null && !RightTurning(vX.pt, pivot.pt, vAlt.pt)))
          continue;

        vAlt = vX;
        eAlt = e;
      }

      if (vAlt == null || vAlt.pt.Y < minY || eAlt == null) return;

      if (vAlt.pt.Y < pivot.pt.Y)
      {
        if (IsRightEdge(eAlt)) return;
      }
      else if (vAlt.pt.Y > pivot.pt.Y)
      {
        if (IsLeftEdge(eAlt)) return;
      }

      Edge? eX = FindLinkingEdge(vAlt, v, (vAlt.pt.Y > v.pt.Y));
      if (eX == null)
      {
        if (vAlt.pt.Y == v.pt.Y && v.pt.Y == minY &&
            HorizontalBetween(vAlt, v) != null)
          return;

        eX = CreateEdge(vAlt, v, EdgeKind.loose);
      }

      CreateTriangle(edge, eX, eAlt);

      if (!EdgeCompleted(eX))
        DoTriangulateRight(eX, vAlt, minY);
    }

    private void AddEdgeToActives(Edge edge)
    {
      if (edge.isActive) return;

      edge.prevE = null;
      edge.nextE = firstActive;
      edge.isActive = true;

      if (firstActive != null)
        firstActive.prevE = edge;

      firstActive = edge;
    }

    private void RemoveEdgeFromActives(Edge edge)
    {
      RemoveEdgeFromVertex(edge.vB, edge);
      RemoveEdgeFromVertex(edge.vT, edge);

      Edge? prev = edge.prevE;
      Edge? next = edge.nextE;

      if (next != null) next.prevE = prev;
      if (prev != null) prev.nextE = next;

      edge.isActive = false;
      if (firstActive == edge) firstActive = next;
    }

    internal TriangulateResult Execute(Paths64 paths, out Paths64 sol)
    {
      sol = new Paths64();

      if (!AddPaths(paths))
      {
        return TriangulateResult.noPolygons;
      }

      // if necessary fix path orientation because the algorithm 
      // expects clockwise outer paths and counter-clockwise inner paths
      if (lowermostVertex!.innerLM)
      {
        // the orientation of added paths must be wrong, so
        // 1. reverse innerLM flags ...
        Vertex2 lm;
        while (locMinStack.Count > 0)
        {
          lm = locMinStack.Pop();
          lm.innerLM = !lm.innerLM;
        }
        // 2. swap edge kinds
        foreach (Edge e in allEdges)
          if (e.kind == EdgeKind.ascend)
            e.kind = EdgeKind.descend;
          else
            e.kind = EdgeKind.ascend;
      }
      else
      {
        // path orientation is fine so ...
        while (locMinStack.Count > 0)
          locMinStack.Pop();
      }

      allEdges.Sort((a, b) => a.vL.pt.X.CompareTo(b.vL.pt.X));

      if (!FixupEdgeIntersects())
      {
        CleanUp();
        return TriangulateResult.pathsIntersect;
      }

      allVertices.Sort((a, b) =>
      {
        if (a.pt.Y == b.pt.Y)
          return a.pt.X.CompareTo(b.pt.X);
        return b.pt.Y.CompareTo(a.pt.Y);
      });

      MergeDupOrCollinearVertices();

      long currY = allVertices[0].pt.Y;

      foreach (Vertex2 v in allVertices)
      {
        if (v.edges.Count == 0) continue;

        if (v.pt.Y != currY)
        {
          while (locMinStack.Count > 0)
          {
            Vertex2 lm = locMinStack.Pop();
            Edge? e = CreateInnerLocMinLooseEdge(lm);
            if (e == null)
            {
              CleanUp();
              return TriangulateResult.fail;
            }

            if (IsHorizontal(e))
            {
              if (e.vL == e.vB)
                DoTriangulateLeft(e, e.vB, currY);
              else
                DoTriangulateRight(e, e.vB, currY);
            }
            else
            {
              DoTriangulateLeft(e, e.vB, currY);
              if (!EdgeCompleted(e))
                DoTriangulateRight(e, e.vB, currY);
            }

            AddEdgeToActives(lm.edges[0]);
            AddEdgeToActives(lm.edges[1]);
          }

          while (horzEdgeStack.Count > 0)
          {
            Edge e = horzEdgeStack.Pop();
            if (EdgeCompleted(e)) continue;

            if (e.vB == e.vL)
            {
              if (IsLeftEdge(e))
                DoTriangulateLeft(e, e.vB, currY);
            }
            else
            {
              if (IsRightEdge(e))
                DoTriangulateRight(e, e.vB, currY);
            }
          }

          currY = v.pt.Y;
        }

        for (int i = v.edges.Count - 1; i >= 0; --i)
        {
          if (i >= v.edges.Count) continue;

          Edge e = v.edges[i];
          if (EdgeCompleted(e) || IsLooseEdge(e)) continue;

          if (v == e.vB)
          {
            if (IsHorizontal(e))
              horzEdgeStack.Push(e);

            if (!v.innerLM)
              AddEdgeToActives(e);
          }
          else
          {
            if (IsHorizontal(e))
              horzEdgeStack.Push(e);
            else if (IsLeftEdge(e))
              DoTriangulateLeft(e, e.vB, v.pt.Y);
            else
              DoTriangulateRight(e, e.vB, v.pt.Y);
          }
        }

        if (v.innerLM)
          locMinStack.Push(v);
      }

      while (horzEdgeStack.Count > 0)
      {
        Edge e = horzEdgeStack.Pop();
        if (!EdgeCompleted(e) && e.vB == e.vL)
          DoTriangulateLeft(e, e.vB, currY);
      }

      if (useDelaunay)
      {
        while (pendingDelaunayStack.Count > 0)
        {
          Edge e = pendingDelaunayStack.Pop();
          ForceLegal(e);
        }
      }

      sol = new Paths64(allTriangles.Count);
      foreach (Triangle tri in allTriangles)
      {
        Path64 p = PathFromTriangle(tri);
        int cps = InternalClipper.CrossProductSign(p[0], p[1], p[2]);
        if (cps == 0) continue;
        if (cps < 0) p.Reverse();
        sol.Add(p);
      }

      CleanUp();
      return TriangulateResult.success;
    }

    // ---------------------------------------------------------------------
    // Static / helper functions (C# equivalents of C++ free functions)
    // ---------------------------------------------------------------------

    private static bool IsLooseEdge(Edge e)
    {
      return e.kind == EdgeKind.loose;
    }

    private static bool IsLeftEdge(Edge e)
    {
      return e.kind == EdgeKind.ascend;
    }

    private static bool IsRightEdge(Edge e)
    {
      return e.kind == EdgeKind.descend;
    }

    private static bool IsHorizontal(Edge e)
    {
      return e.vB.pt.Y == e.vT.pt.Y;
    }

    private static bool LeftTurning(in Point64 p1, in Point64 p2, in Point64 p3)
    {
      return InternalClipper.CrossProductSign(p1, p2, p3) < 0;
    }

    private static bool RightTurning(in Point64 p1, in Point64 p2, in Point64 p3)
    {
      return InternalClipper.CrossProductSign(p1, p2, p3) > 0;
    }

    private static bool EdgeCompleted(Edge edge)
    {
      if (edge.triA == null) return false;
      if (edge.triB != null) return true;
      return edge.kind != EdgeKind.loose;
    }

    private static EdgeContainsResult EdgeContains(Edge edge, Vertex2 v)
    {
      if (edge.vL == v) return EdgeContainsResult.left;
      if (edge.vR == v) return EdgeContainsResult.right;
      return EdgeContainsResult.neither;
    }

    private static double GetAngle(in Point64 a, in Point64 b, in Point64 c)
    {
      double abx = (double) (b.X - a.X);
      double aby = (double) (b.Y - a.Y);
      double bcx = (double) (b.X - c.X);
      double bcy = (double) (b.Y - c.Y);
      double dp = abx * bcx + aby * bcy;
      double cp = abx * bcy - aby * bcx;
      return Math.Atan2(cp, dp);
    }

    private static double GetLocMinAngle(Vertex2 v)
    {
      int asc, des;
      if (v.edges[0].kind == EdgeKind.ascend)
      {
        asc = 0;
        des = 1;
      }
      else
      {
        des = 0;
        asc = 1;
      }
      return GetAngle(v.edges[des].vT.pt, v.pt, v.edges[asc].vT.pt);
    }

    private static void RemoveEdgeFromVertex(Vertex2 vert, Edge edge)
    {
      int idx = vert.edges.IndexOf(edge);
      if (idx < 0) throw new InvalidOperationException("oops!");
      vert.edges.RemoveAt(idx);
    }

    private static bool FindLocMinIdx(Path64 path, int len, ref int idx)
    {
      if (len < 3) return false;
      int i0 = idx;
      int n = (idx + 1) % len;

      while (path[n].Y <= path[idx].Y)
      {
        idx = n;
        n = (n + 1) % len;
        if (idx == i0) return false;
      }

      while (path[n].Y >= path[idx].Y)
      {
        idx = n;
        n = (n + 1) % len;
      }

      return true;
    }

    private static int Prev(int idx, int len)
    {
      if (idx == 0) return len - 1;
      return (idx - 1);
    }

    private static int Next(int idx, int len)
    {
      idx = ((idx + 1) % len);
      return idx;
    }

    private static Edge? FindLinkingEdge(Vertex2 vert1, Vertex2 vert2, bool preferAscending)
    {
      Edge? res = null;
      foreach (Edge e in vert1.edges)
      {
        if (e.vL == vert2 || e.vR == vert2)
        {
          if (e.kind == EdgeKind.loose ||
              ((e.kind == EdgeKind.ascend) == preferAscending))
            return e;
          res = e;
        }
      }
      return res;
    }

    private static Path64 PathFromTriangle(Triangle tri)
    {
      Path64 res = new Path64(3)
            {
                tri.edges[0].vL.pt,
                tri.edges[0].vR.pt
            };
      Edge e = tri.edges[1];
      if (e.vL.pt.Equals(res[0]) || e.vL.pt.Equals(res[1]))
        res.Add(e.vR.pt);
      else
        res.Add(e.vL.pt);
      return res;
    }

    private static double InCircleTest(in Point64 ptA, in Point64 ptB, in Point64 ptC, in Point64 ptD)
    {
      double m00 = (double) (ptA.X - ptD.X);
      double m01 = (double) (ptA.Y - ptD.Y);
      double m02 = Sqr(m00) + Sqr(m01);

      double m10 = (double) (ptB.X - ptD.X);
      double m11 = (double) (ptB.Y - ptD.Y);
      double m12 = Sqr(m10) + Sqr(m11);

      double m20 = (double) (ptC.X - ptD.X);
      double m21 = (double) (ptC.Y - ptD.Y);
      double m22 = Sqr(m20) + Sqr(m21);

      return m00 * (m11 * m22 - m21 * m12) -
             m10 * (m01 * m22 - m21 * m02) +
             m20 * (m01 * m12 - m11 * m02);
    }

    private static double ShortestDistFromSegment(in Point64 pt, in Point64 segPt1, in Point64 segPt2)
    {
      double dx = (double) (segPt2.X - segPt1.X);
      double dy = (double) (segPt2.Y - segPt1.Y);

      double ax = (double) (pt.X - segPt1.X);
      double ay = (double) (pt.Y - segPt1.Y);

      double qNum = ax * dx + ay * dy;
      double denom = Sqr(dx) + Sqr(dy);

      if (qNum < 0)
        return DistanceSqr(pt, segPt1);
      if (qNum > denom)
        return DistanceSqr(pt, segPt2);

      return Sqr(ax * dy - dx * ay) / denom;
    }

    private static IntersectKind SegsIntersect(Point64 s1a, Point64 s1b, Point64 s2a, Point64 s2b)
    {
      //ignore segments sharing an end-point
      if (s1a == s2a || s1a == s2b || s1b == s2b) return IntersectKind.none;

      double dy1 = (double) (s1b.Y - s1a.Y);
      double dx1 = (double) (s1b.X - s1a.X);
      double dy2 = (double) (s2b.Y - s2a.Y);
      double dx2 = (double) (s2b.X - s2a.X);

      double cp = dy1 * dx2 - dy2 * dx1;
      if (cp == 0) return IntersectKind.collinear;

      double t = ((double) (s1a.X - s2a.X) * dy2 -
                  (double) (s1a.Y - s2a.Y) * dx2);
      if (t >= 0)
      {
        if (cp < 0 || t >= cp) return IntersectKind.none;
      }
      else
      {
        if (cp > 0 || t <= cp) return IntersectKind.none;
      }

      t = ((double) (s1a.X - s2a.X) * dy1 -
           (double) (s1a.Y - s2a.Y) * dx1);
      if (t >= 0)
      {
        if (cp > 0 && t < cp) return IntersectKind.intersect;
      }
      else
      {
        if (cp < 0 && t > cp) return IntersectKind.intersect;
      }

      return IntersectKind.none;
    }

    private static double DistSqr(in Point64 pt1, in Point64 pt2)
    {
      return Sqr((double) (pt1.X - pt2.X)) + Sqr((double) (pt1.Y - pt2.Y));
    }

    // These are placeholders to be wired to your Clipper2 C# math helpers.
    // Replace with actual implementations if different.
    private static double Sqr(double v)
    {
      return v * v;
    }

    private static double DistanceSqr(in Point64 a, in Point64 b)
    {
      double dx = (double) (a.X - b.X);
      double dy = (double) (a.Y - b.Y);
      return dx * dx + dy * dy;
    }
  }

}
