/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  27 April 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  This is the main polygon clipping module                        *
* Thanks    :  Special thanks to Thong Nguyen, Guus Kuiper, Phil Stopford,     *
*           :  and Daniel Gosnell for their invaluable assistance with C#.     *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#nullable enable
using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Clipper2Lib
{

  // Vertex: a pre-clipping data structure. It is used to separate polygons
  // into ascending and descending 'bounds' (or sides) that start at local
  // minima and ascend to a local maxima, before descending again.
  [Flags]
  public enum PointInPolygonResult
  {
    IsOn = 0,
    IsInside = 1,
    IsOutside = 2
  };

  [Flags]
  internal enum VertexFlags
  {
    None = 0,
    OpenStart = 1,
    OpenEnd = 2,
    LocalMax = 4,
    LocalMin = 8
  };

  internal class Vertex
  {
    public readonly Point64 pt;
    public Vertex? next;
    public Vertex? prev;
    public VertexFlags flags;

    public Vertex(Point64 pt, VertexFlags flags, Vertex? prev)
    {
      this.pt = pt;
      this.flags = flags;
      next = null;
      this.prev = prev;
    }
  };

  internal readonly struct LocalMinima
  {
    public readonly Vertex vertex;
    public readonly PathType polytype;
    public readonly bool isOpen;

    public LocalMinima(Vertex vertex, PathType polytype, bool isOpen = false)
    {
      this.vertex = vertex;
      this.polytype = polytype;
      this.isOpen = isOpen;
    }

    public static bool operator ==(LocalMinima lm1, LocalMinima lm2)
    {
      return ReferenceEquals(lm1.vertex, lm2.vertex);
    }

    public static bool operator !=(LocalMinima lm1, LocalMinima lm2)
    {
      return !(lm1 == lm2);
    }

    public override bool Equals(object? obj)
    {
      return obj is LocalMinima minima && this == minima;
    }

    public override int GetHashCode()
    {
      return vertex.GetHashCode();
    }
  };

  // IntersectNode: a structure representing 2 intersecting edges.
  // Intersections must be sorted so they are processed from the largest
  // Y coordinates to the smallest while keeping edges adjacent.
  internal readonly struct IntersectNode
  {
    public readonly Point64 pt;
    public readonly Active edge1;
    public readonly Active edge2;

    public IntersectNode(Point64 pt, Active edge1, Active edge2)
    {
      this.pt = pt;
      this.edge1 = edge1;
      this.edge2 = edge2;
    }
  };

  internal struct LocMinSorter : IComparer<LocalMinima>
  {
    public readonly int Compare(LocalMinima locMin1, LocalMinima locMin2)
    {
      return locMin2.vertex.pt.Y.CompareTo(locMin1.vertex.pt.Y);
    }
  }

  // OutPt: vertex data structure for clipping solutions
  internal class OutPt
  {
    public Point64 pt;
    public OutPt? next;
    public OutPt prev;
    public OutRec outrec;
    public HorzSegment? horz;

    public OutPt(Point64 pt, OutRec outrec)
    {
      this.pt = pt;
      this.outrec = outrec;
      next = this;
      prev = this;
      horz = null;
    }
  };

  internal enum JoinWith { None, Left, Right };
  internal enum HorzPosition { Bottom, Middle, Top };


  // OutRec: path data structure for clipping solutions
  internal class OutRec
  {
    public int idx;
    public OutRec? owner;
    public Active? frontEdge;
    public Active? backEdge;
    public OutPt? pts;
    public PolyPathBase? polypath;
    public Rect64 bounds;
    public Path64 path = new Path64();
    public bool isOpen;
    public List<int>? splits;
    public OutRec? recursiveSplit;
  };

  internal class HorzSegment
  {
    public OutPt? leftOp;
    public OutPt? rightOp;
    public bool leftToRight;
    public HorzSegment(OutPt op)
    {
      leftOp = op;
      rightOp = null;
      leftToRight = true;
    }
  }

  internal class HorzJoin
  {
    public OutPt? op1;
    public OutPt? op2;
    public HorzJoin(OutPt ltor, OutPt rtol)
    {
      op1 = ltor;
      op2 = rtol;
    }
  }

  ///////////////////////////////////////////////////////////////////
  // Important: UP and DOWN here are premised on Y-axis positive down
  // displays, which is the orientation used in Clipper's development.
  ///////////////////////////////////////////////////////////////////

  internal class Active
  {
    public Point64 bot;
    public Point64 top;
    public long curX; // current (updated at every new scanline)
    public double dx;
    public int windDx; // 1 or -1 depending on winding direction
    public int windCount;
    public int windCount2; // winding count of the opposite polytype
    public OutRec? outrec;

    // AEL: 'active edge list' (Vatti's AET - active edge table)
    //     a linked list of all edges (from left to right) that are present
    //     (or 'active') within the current scanbeam (a horizontal 'beam' that
    //     sweeps from bottom to top over the paths in the clipping operation).
    public Active? prevInAEL;
    public Active? nextInAEL;

    // SEL: 'sorted edge list' (Vatti's ST - sorted table)
    //     linked list used when sorting edges into their new positions at the
    //     top of scanbeams, but also (re)used to process horizontals.
    public Active? prevInSEL;
    public Active? nextInSEL;
    public Active? jump;
    public Vertex? vertexTop;
    public LocalMinima localMin; // the bottom of an edge 'bound' (also Vatti)
    internal bool isLeftBound;
    internal JoinWith joinWith;
  };

  internal static class ClipperEngine
  {
    internal static void AddLocMin(Vertex vert, PathType polytype, bool isOpen,
      List<LocalMinima> minimaList)
    {
      // make sure the vertex is added only once ...
      if ((vert.flags & VertexFlags.LocalMin) != VertexFlags.None) return;
      vert.flags |= VertexFlags.LocalMin;

      LocalMinima lm = new LocalMinima(vert, polytype, isOpen);
      minimaList.Add(lm);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static void EnsureCapacity<T>(this List<T> list, int minCapacity)
    {
      if(list.Capacity < minCapacity)
        list.Capacity = minCapacity;
    }

    internal static void AddPathsToVertexList(Paths64 paths, PathType polytype, bool isOpen,
      List<LocalMinima> minimaList, List<Vertex> vertexList)
    {
      int totalVertCnt = 0;
      foreach (Path64 path in paths) totalVertCnt += path.Count;
      vertexList.EnsureCapacity(vertexList.Count + totalVertCnt);

      foreach (Path64 path in paths)
      {
        Vertex? v0 = null, prev_v = null, curr_v;
        foreach (Point64 pt in path)
        {
          if (v0 == null)
          {
            v0 = new Vertex(pt, VertexFlags.None, null);
            vertexList.Add(v0);
            prev_v = v0;
          }
          else if (prev_v!.pt != pt) // ie skips duplicates
          {
            curr_v = new Vertex(pt, VertexFlags.None, prev_v);
            vertexList.Add(curr_v);
            prev_v.next = curr_v;
            prev_v = curr_v;
          }
        }
        if (prev_v == null || prev_v.prev == null) continue;
        if (!isOpen && prev_v.pt == v0!.pt) prev_v = prev_v.prev;
        prev_v.next = v0;
        v0!.prev = prev_v;
        if (!isOpen && prev_v.next == prev_v) continue;

        // OK, we have a valid path
        bool going_up, going_up0;
        if (isOpen)
        {
          curr_v = v0.next;
          while (curr_v != v0 && curr_v!.pt.Y == v0.pt.Y)
            curr_v = curr_v.next;
          going_up = curr_v.pt.Y <= v0.pt.Y;
          if (going_up)
          {
            v0.flags = VertexFlags.OpenStart;
            AddLocMin(v0, polytype, true, minimaList);
          }
          else
            v0.flags = VertexFlags.OpenStart | VertexFlags.LocalMax;
        }
        else // closed path
        {
          prev_v = v0.prev;
          while (prev_v != v0 && prev_v!.pt.Y == v0.pt.Y)
            prev_v = prev_v.prev;
          if (prev_v == v0)
            continue; // only open paths can be completely flat
          going_up = prev_v.pt.Y > v0.pt.Y;
        }

        going_up0 = going_up;
        prev_v = v0;
        curr_v = v0.next;
        while (curr_v != v0)
        {
          if (curr_v!.pt.Y > prev_v.pt.Y && going_up)
          {
            prev_v.flags |= VertexFlags.LocalMax;
            going_up = false;
          }
          else if (curr_v.pt.Y < prev_v.pt.Y && !going_up)
          {
            going_up = true;
            AddLocMin(prev_v, polytype, isOpen, minimaList);
          }
          prev_v = curr_v;
          curr_v = curr_v.next;
        }

        if (isOpen)
        {
          prev_v.flags |= VertexFlags.OpenEnd;
          if (going_up)
            prev_v.flags |= VertexFlags.LocalMax;
          else
            AddLocMin(prev_v, polytype, isOpen, minimaList);
        }
        else if (going_up != going_up0)
        {
          if (going_up0) AddLocMin(prev_v, polytype, false, minimaList);
          else prev_v.flags |= VertexFlags.LocalMax;
        }
      }
    }
  }

  public class ReuseableDataContainer64
  {
    internal readonly List<LocalMinima> _minimaList;
    internal readonly List<Vertex> _vertexList;
    public ReuseableDataContainer64()
    {
      _minimaList = new List<LocalMinima>();
      _vertexList = new List<Vertex>();
    }
    public void Clear()
    {
      _minimaList.Clear();
      _vertexList.Clear();
    }

    public void AddPaths(Paths64 paths, PathType pt, bool isOpen)
    {
      ClipperEngine.AddPathsToVertexList(paths, pt, isOpen, _minimaList, _vertexList);
    }
  }

  public class ClipperBase
  {
    private ClipType _cliptype;
    private FillRule _fillrule;
    private Active? _actives;
    private Active? _sel;
    private readonly List<LocalMinima> _minimaList;
    private readonly List<IntersectNode> _intersectList;
    private readonly List<Vertex> _vertexList;
    private readonly List<OutRec> _outrecList;
    private readonly List<long> _scanlineList;
    private readonly List<HorzSegment> _horzSegList;
    private readonly List<HorzJoin> _horzJoinList;
    private int _currentLocMin;
    private long _currentBotY;
    private bool _isSortedMinimaList;
    private bool _hasOpenPaths;
    internal bool _using_polytree;
    internal bool _succeeded;
    public bool PreserveCollinear { get; set; }
    public bool ReverseSolution { get; set; }

#if USINGZ
    public delegate void ZCallback64(Point64 bot1, Point64 top1,
        Point64 bot2, Point64 top2, ref Point64 intersectPt);

    public long DefaultZ { get; set; }
    protected ZCallback64? _zCallback;
#endif
    public ClipperBase()
    {
      _minimaList = new List<LocalMinima>();
      _intersectList = new List<IntersectNode>();
      _vertexList = new List<Vertex>();
      _outrecList = new List<OutRec>();
      _scanlineList = new List<long>();
      _horzSegList = new List<HorzSegment>();
      _horzJoinList = new List<HorzJoin>();
      PreserveCollinear = true;
    }

#if USINGZ
    private bool XYCoordsEqual(Point64 pt1, Point64 pt2)
    {
      return (pt1.X == pt2.X && pt1.Y == pt2.Y);
    }
    
    private void SetZ(Active e1, Active e2, ref Point64 intersectPt)
    {
      if (_zCallback == null) return;

      // prioritize subject vertices over clip vertices
      // and pass the subject vertices before clip vertices in the callback
      if (GetPolyType(e1) == PathType.Subject)
      {
        if (XYCoordsEqual(intersectPt, e1.bot))
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, e1.bot.Z);
        else if (XYCoordsEqual(intersectPt, e1.top))
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, e1.top.Z);
        else if (XYCoordsEqual(intersectPt, e2.bot))
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, e2.bot.Z);
        else if (XYCoordsEqual(intersectPt, e2.top))
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, e2.top.Z);
        else
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, DefaultZ);
        _zCallback(e1.bot, e1.top, e2.bot, e2.top, ref intersectPt);
      }
      else
      {
        if (XYCoordsEqual(intersectPt, e2.bot))
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, e2.bot.Z);
        else if (XYCoordsEqual(intersectPt, e2.top))
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, e2.top.Z);
        else if (XYCoordsEqual(intersectPt, e1.bot))
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, e1.bot.Z);
        else if (XYCoordsEqual(intersectPt, e1.top))
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, e1.top.Z);
        else
          intersectPt = new Point64(intersectPt.X, intersectPt.Y, DefaultZ);
        _zCallback(e2.bot, e2.top, e1.bot, e1.top, ref intersectPt);
      }
    }
#endif

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsOdd(int val)
    {
      return ((val & 1) != 0);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsHotEdge(Active ae)
    {
      return ae.outrec != null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsOpen(Active ae)
    {
      return ae.localMin.isOpen;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsOpenEnd(Active ae)
    {
      return ae.localMin.isOpen && IsOpenEnd(ae.vertexTop!);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsOpenEnd(Vertex v)
    {
      return (v.flags & (VertexFlags.OpenStart | VertexFlags.OpenEnd)) != VertexFlags.None;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Active? GetPrevHotEdge(Active ae)
    {
      Active? prev = ae.prevInAEL;
      while (prev != null && (IsOpen(prev) || !IsHotEdge(prev)))
        prev = prev.prevInAEL;
      return prev;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsFront(Active ae)
    {
      return (ae == ae.outrec!.frontEdge);
    }

    /*******************************************************************************
    *  Dx:                             0(90deg)                                    *
    *                                  |                                           *
    *               +inf (180deg) <--- o --. -inf (0deg)                          *
    *******************************************************************************/

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static double GetDx(Point64 pt1, Point64 pt2)
    {
      double dy = pt2.Y - pt1.Y;
      if (dy != 0)
        return (pt2.X - pt1.X) / dy;
      if (pt2.X > pt1.X)
        return double.NegativeInfinity;
      return double.PositiveInfinity;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static long TopX(Active ae, long currentY)
    {
      if ((currentY == ae.top.Y) || (ae.top.X == ae.bot.X)) return ae.top.X;
      if (currentY == ae.bot.Y) return ae.bot.X;

      // use MidpointRounding.ToEven in order to explicitly match the nearbyint behaviour on the C++ side
      return ae.bot.X + (long) Math.Round(ae.dx * (currentY - ae.bot.Y), MidpointRounding.ToEven);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsHorizontal(Active ae)
    {
      return (ae.top.Y == ae.bot.Y);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsHeadingRightHorz(Active ae)
    {
      return (double.IsNegativeInfinity(ae.dx));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsHeadingLeftHorz(Active ae)
    {
      return (double.IsPositiveInfinity(ae.dx));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void SwapActives(ref Active ae1, ref Active ae2)
    {
      (ae2, ae1) = (ae1, ae2);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PathType GetPolyType(Active ae)
    {
      return ae.localMin.polytype;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsSamePolyType(Active ae1, Active ae2)
    {
      return ae1.localMin.polytype == ae2.localMin.polytype;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void SetDx(Active ae)
    {
      ae.dx = GetDx(ae.bot, ae.top);
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Vertex NextVertex(Active ae)
    {
      if (ae.windDx > 0)
        return ae.vertexTop!.next!;
      return ae.vertexTop!.prev!;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Vertex PrevPrevVertex(Active ae)
    {
      if (ae.windDx > 0)
        return ae.vertexTop!.prev!.prev!;
      return ae.vertexTop!.next!.next!;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsMaxima(Vertex vertex)
    {
      return ((vertex.flags & VertexFlags.LocalMax) != VertexFlags.None);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsMaxima(Active ae)
    {
      return IsMaxima(ae.vertexTop!);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Active? GetMaximaPair(Active ae)
    {
      Active? ae2;
      ae2 = ae.nextInAEL;
      while (ae2 != null)
      {
        if (ae2.vertexTop == ae.vertexTop) return ae2; // Found!
        ae2 = ae2.nextInAEL;
      }
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Vertex? GetCurrYMaximaVertex_Open(Active ae)
    {
      Vertex? result = ae.vertexTop;
      if (ae.windDx > 0)
        while (result!.next!.pt.Y == result.pt.Y &&
          ((result.flags & (VertexFlags.OpenEnd |
          VertexFlags.LocalMax)) == VertexFlags.None))
          result = result.next;
      else
        while (result!.prev!.pt.Y == result.pt.Y &&
          ((result.flags & (VertexFlags.OpenEnd |
          VertexFlags.LocalMax)) == VertexFlags.None))
          result = result.prev;
      if (!IsMaxima(result)) result = null; // not a maxima
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Vertex? GetCurrYMaximaVertex(Active ae)
    {
      Vertex? result = ae.vertexTop;
      if (ae.windDx > 0)
        while (result!.next!.pt.Y == result.pt.Y) result = result.next;
      else
        while (result!.prev!.pt.Y == result.pt.Y) result = result.prev;
      if (!IsMaxima(result)) result = null; // not a maxima
      return result;
    }

    private struct IntersectListSort : IComparer<IntersectNode>
    {
      public readonly int Compare(IntersectNode a, IntersectNode b)
      {
        if (a.pt.Y == b.pt.Y)
        {
          if (a.pt.X == b.pt.X) return 0;
          return (a.pt.X < b.pt.X) ? -1 : 1;
        }
        return (a.pt.Y > b.pt.Y) ? -1 : 1;
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void SetSides(OutRec outrec, Active startEdge, Active endEdge)
    {
      outrec.frontEdge = startEdge;
      outrec.backEdge = endEdge;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void SwapOutrecs(Active ae1, Active ae2)
    {
      OutRec? or1 = ae1.outrec; // at least one edge has 
      OutRec? or2 = ae2.outrec; // an assigned outrec
      if (or1 == or2)
      {
        Active? ae = or1!.frontEdge;
        or1.frontEdge = or1.backEdge;
        or1.backEdge = ae;
        return;
      }

      if (or1 != null)
      {
        if (ae1 == or1.frontEdge)
          or1.frontEdge = ae2;
        else
          or1.backEdge = ae2;
      }

      if (or2 != null)
      {
        if (ae2 == or2.frontEdge)
          or2.frontEdge = ae1;
        else
          or2.backEdge = ae1;
      }

      ae1.outrec = or2;
      ae2.outrec = or1;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void SetOwner(OutRec outrec, OutRec newOwner)
    {
      //precondition1: new_owner is never null
      while (newOwner.owner != null && newOwner.owner.pts == null)
        newOwner.owner = newOwner.owner.owner;

      //make sure that outrec isn't an owner of newOwner
      OutRec? tmp = newOwner;
      while (tmp != null && tmp != outrec) 
        tmp = tmp.owner;
      if (tmp != null) 
        newOwner.owner = outrec.owner;
      outrec.owner = newOwner;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static double Area(OutPt op)
    {
      // https://en.wikipedia.org/wiki/Shoelace_formula
      double area = 0.0;
      OutPt op2 = op;
      do
      {
        area += (double) (op2.prev.pt.Y + op2.pt.Y) *
          (op2.prev.pt.X - op2.pt.X);
        op2 = op2.next!;
      } while (op2 != op);
      return area * 0.5;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static double AreaTriangle(Point64 pt1, Point64 pt2, Point64 pt3)
    {
      return (double) (pt3.Y + pt1.Y) * (pt3.X - pt1.X) +
        (double) (pt1.Y + pt2.Y) * (pt1.X - pt2.X) +
        (double) (pt2.Y + pt3.Y) * (pt2.X - pt3.X);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static OutRec? GetRealOutRec(OutRec? outRec)
    {
      while ((outRec != null) && (outRec.pts == null))
        outRec = outRec.owner;
      return outRec;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsValidOwner(OutRec? outRec, OutRec? testOwner)
    {
      while ((testOwner != null) && (testOwner != outRec))
        testOwner = testOwner.owner;
      return testOwner == null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void UncoupleOutRec(Active ae)
    {
      OutRec? outrec = ae.outrec;
      if (outrec == null) return;
      outrec.frontEdge!.outrec = null;
      outrec.backEdge!.outrec = null;
      outrec.frontEdge = null;
      outrec.backEdge = null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool OutrecIsAscending(Active hotEdge)
    {
      return (hotEdge == hotEdge.outrec!.frontEdge);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void SwapFrontBackSides(OutRec outrec)
    {
      // while this proc. is needed for open paths
      // it's almost never needed for closed paths
      Active ae2 = outrec.frontEdge!;
      outrec.frontEdge = outrec.backEdge;
      outrec.backEdge = ae2;
      outrec.pts = outrec.pts!.next;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool EdgesAdjacentInAEL(IntersectNode inode)
    {
      return (inode.edge1.nextInAEL == inode.edge2) || (inode.edge1.prevInAEL == inode.edge2);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    protected void ClearSolutionOnly()
    {
      while (_actives != null) DeleteFromAEL(_actives);
      _scanlineList.Clear();
      DisposeIntersectNodes();
      _outrecList.Clear();
      _horzSegList.Clear();
      _horzJoinList.Clear();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Clear()
    {
      ClearSolutionOnly();
      _minimaList.Clear();
      _vertexList.Clear();
      _currentLocMin = 0;
      _isSortedMinimaList = false;
      _hasOpenPaths = false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    protected void Reset()
    {
      if (!_isSortedMinimaList)
      {
        _minimaList.Sort(new LocMinSorter());
        _isSortedMinimaList = true;
      }

      _scanlineList.EnsureCapacity(_minimaList.Count);
      for (int i = _minimaList.Count - 1; i >= 0; i--)
        _scanlineList.Add(_minimaList[i].vertex.pt.Y);

      _currentBotY = 0;
      _currentLocMin = 0;
      _actives = null;
      _sel = null;
      _succeeded = true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void InsertScanline(long y)
    {
      int index = _scanlineList.BinarySearch(y);
      if (index >= 0) return;
      index = ~index;
      _scanlineList.Insert(index, y);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool PopScanline(out long y)
    {
      int cnt = _scanlineList.Count - 1;
      if (cnt < 0)
      {
        y = 0;
        return false;
      }

      y = _scanlineList[cnt];
      _scanlineList.RemoveAt(cnt--);
      while (cnt >= 0 && y == _scanlineList[cnt])
        _scanlineList.RemoveAt(cnt--);
      return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool HasLocMinAtY(long y)
    {
      return (_currentLocMin < _minimaList.Count && _minimaList[_currentLocMin].vertex.pt.Y == y);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private LocalMinima PopLocalMinima()
    {
      return _minimaList[_currentLocMin++];
    }
   
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddSubject(Path64 path)
    {
      AddPath(path, PathType.Subject);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddOpenSubject(Path64 path)
    {
      AddPath(path, PathType.Subject, true);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddClip(Path64 path)
    {
      AddPath(path, PathType.Clip);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    protected void AddPath(Path64 path, PathType polytype, bool isOpen = false)
    {
      Paths64 tmp = new Paths64(1) { path };
      AddPaths(tmp, polytype, isOpen);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    protected void AddPaths(Paths64 paths, PathType polytype, bool isOpen = false)
    {
      if (isOpen) _hasOpenPaths = true;
      _isSortedMinimaList = false;
      ClipperEngine.AddPathsToVertexList(paths, polytype, isOpen, _minimaList, _vertexList);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    protected void AddReuseableData(ReuseableDataContainer64 reuseableData)
    {
      if (reuseableData._minimaList.Count == 0) return;
      // nb: reuseableData will continue to own the vertices, so it's important
      // that the reuseableData object isn't destroyed before the Clipper object
      // that's using the data.
      _isSortedMinimaList = false;
      foreach (LocalMinima lm in reuseableData._minimaList)
      {
        _minimaList.Add(new LocalMinima(lm.vertex, lm.polytype, lm.isOpen));
        if (lm.isOpen) _hasOpenPaths = true;
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool IsContributingClosed(Active ae)
    {
      switch (_fillrule)
      {
        case FillRule.Positive:
          if (ae.windCount != 1) return false;
          break;
        case FillRule.Negative:
          if (ae.windCount != -1) return false;
          break;
        case FillRule.NonZero:
          if (Math.Abs(ae.windCount) != 1) return false;
          break;
      }

      switch (_cliptype)
      {
        case ClipType.Intersection:
          return _fillrule switch
          {
            FillRule.Positive => ae.windCount2 > 0,
            FillRule.Negative => ae.windCount2 < 0,
            _ => ae.windCount2 != 0,
          };

        case ClipType.Union:
          return _fillrule switch
          {
            FillRule.Positive => ae.windCount2 <= 0,
            FillRule.Negative => ae.windCount2 >= 0,
            _ => ae.windCount2 == 0,
          };

        case ClipType.Difference:
          bool result = _fillrule switch
          {
            FillRule.Positive => (ae.windCount2 <= 0),
            FillRule.Negative => (ae.windCount2 >= 0),
            _ => (ae.windCount2 == 0),
          };
          return (GetPolyType(ae) == PathType.Subject) ? result : !result;

        case ClipType.Xor:
          return true; // XOr is always contributing unless open

        default:
          return false;
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool IsContributingOpen(Active ae)
    {
      bool isInClip, isInSubj;
      switch (_fillrule)
      {
        case FillRule.Positive:
          isInSubj = ae.windCount > 0;
          isInClip = ae.windCount2 > 0;
          break;
        case FillRule.Negative:
          isInSubj = ae.windCount < 0;
          isInClip = ae.windCount2 < 0;
          break;
        default:
          isInSubj = ae.windCount != 0;
          isInClip = ae.windCount2 != 0;
          break;
      }

      bool result = _cliptype switch
      {
        ClipType.Intersection => isInClip,
        ClipType.Union => !isInSubj && !isInClip,
        _ => !isInClip
      };
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void SetWindCountForClosedPathEdge(Active ae)
    {
      // Wind counts refer to polygon regions not edges, so here an edge's WindCnt
      // indicates the higher of the wind counts for the two regions touching the
      // edge. (nb: Adjacent regions can only ever have their wind counts differ by
      // one. Also, open paths have no meaningful wind directions or counts.)

      Active? ae2 = ae.prevInAEL;
      // find the nearest closed path edge of the same PolyType in AEL (heading left)
      PathType pt = GetPolyType(ae);
      while (ae2 != null && (GetPolyType(ae2) != pt || IsOpen(ae2))) ae2 = ae2.prevInAEL;

      if (ae2 == null)
      {
        ae.windCount = ae.windDx;
        ae2 = _actives;
      }
      else if (_fillrule == FillRule.EvenOdd)
      {
        ae.windCount = ae.windDx;
        ae.windCount2 = ae2.windCount2;
        ae2 = ae2.nextInAEL;
      }
      else
      {
        // NonZero, positive, or negative filling here ...
        // when e2's WindCnt is in the SAME direction as its WindDx,
        // then polygon will fill on the right of 'e2' (and 'e' will be inside)
        // nb: neither e2.WindCnt nor e2.WindDx should ever be 0.
        if (ae2.windCount * ae2.windDx < 0)
        {
          // opposite directions so 'ae' is outside 'ae2' ...
          if (Math.Abs(ae2.windCount) > 1)
          {
            // outside prev poly but still inside another.
            if (ae2.windDx * ae.windDx < 0)
              // reversing direction so use the same WC
              ae.windCount = ae2.windCount;
            else
              // otherwise keep 'reducing' the WC by 1 (i.e. towards 0) ...
              ae.windCount = ae2.windCount + ae.windDx;
          }
          else
            // now outside all polys of same polytype so set own WC ...
            ae.windCount = (IsOpen(ae) ? 1 : ae.windDx);
        }
        else
        {
          //'ae' must be inside 'ae2'
          if (ae2.windDx * ae.windDx < 0)
            // reversing direction so use the same WC
            ae.windCount = ae2.windCount;
          else
            // otherwise keep 'increasing' the WC by 1 (i.e. away from 0) ...
            ae.windCount = ae2.windCount + ae.windDx;
        }

        ae.windCount2 = ae2.windCount2;
        ae2 = ae2.nextInAEL; // i.e. get ready to calc WindCnt2
      }

      // update windCount2 ...
      if (_fillrule == FillRule.EvenOdd)
        while (ae2 != ae)
        {
          if (GetPolyType(ae2!) != pt && !IsOpen(ae2!))
            ae.windCount2 = (ae.windCount2 == 0 ? 1 : 0);
          ae2 = ae2!.nextInAEL;
        }
      else
        while (ae2 != ae)
        {
          if (GetPolyType(ae2!) != pt && !IsOpen(ae2!))
            ae.windCount2 += ae2!.windDx;
          ae2 = ae2!.nextInAEL;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void SetWindCountForOpenPathEdge(Active ae)
    {
      Active? ae2 = _actives;
      if (_fillrule == FillRule.EvenOdd)
      {
        int cnt1 = 0, cnt2 = 0;
        while (ae2 != ae)
        {
          if (GetPolyType(ae2!) == PathType.Clip)
            cnt2++;
          else if (!IsOpen(ae2!))
            cnt1++;
          ae2 = ae2!.nextInAEL;
        }

        ae.windCount = (IsOdd(cnt1) ? 1 : 0);
        ae.windCount2 = (IsOdd(cnt2) ? 1 : 0);
      }
      else
      {
        while (ae2 != ae)
        {
          if (GetPolyType(ae2!) == PathType.Clip)
            ae.windCount2 += ae2!.windDx;
          else if (!IsOpen(ae2!))
            ae.windCount += ae2!.windDx;
          ae2 = ae2!.nextInAEL;
        }
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsValidAelOrder(Active resident, Active newcomer)
    {
      if (newcomer.curX != resident.curX)
        return newcomer.curX > resident.curX;

      // get the turning direction  a1.top, a2.bot, a2.top
      double d = InternalClipper.CrossProduct(resident.top, newcomer.bot, newcomer.top);
      if (d != 0) return (d < 0);

      // edges must be collinear to get here

      // for starting open paths, place them according to
      // the direction they're about to turn
      if (!IsMaxima(resident) && (resident.top.Y > newcomer.top.Y))
      {
        return InternalClipper.CrossProduct(newcomer.bot,
          resident.top, NextVertex(resident).pt) <= 0;
      }

      if (!IsMaxima(newcomer) && (newcomer.top.Y > resident.top.Y))
      {
        return InternalClipper.CrossProduct(newcomer.bot,
          newcomer.top, NextVertex(newcomer).pt) >= 0;
      }

      long y = newcomer.bot.Y;
      bool newcomerIsLeft = newcomer.isLeftBound;

      if (resident.bot.Y != y || resident.localMin.vertex.pt.Y != y)
        return newcomer.isLeftBound;
      // resident must also have just been inserted
      if (resident.isLeftBound != newcomerIsLeft)
        return newcomerIsLeft;
      if (InternalClipper.IsCollinear(PrevPrevVertex(resident).pt,
            resident.bot, resident.top)) return true;
      // compare turning direction of the alternate bound
      return (InternalClipper.CrossProduct(PrevPrevVertex(resident).pt,
        newcomer.bot, PrevPrevVertex(newcomer).pt) > 0) == newcomerIsLeft;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void InsertLeftEdge(Active ae)
    {
      Active ae2;

      if (_actives == null)
      {
        ae.prevInAEL = null;
        ae.nextInAEL = null;
        _actives = ae;
      }
      else if (!IsValidAelOrder(_actives, ae))
      {
        ae.prevInAEL = null;
        ae.nextInAEL = _actives;
        _actives.prevInAEL = ae;
        _actives = ae;
      }
      else
      {
        ae2 = _actives;
        while (ae2.nextInAEL != null && IsValidAelOrder(ae2.nextInAEL, ae))
          ae2 = ae2.nextInAEL;
        //don't separate joined edges
        if (ae2.joinWith == JoinWith.Right) ae2 = ae2.nextInAEL!;
        ae.nextInAEL = ae2.nextInAEL;
        if (ae2.nextInAEL != null) ae2.nextInAEL.prevInAEL = ae;
        ae.prevInAEL = ae2;
        ae2.nextInAEL = ae;
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void InsertRightEdge(Active ae, Active ae2)
    {
      ae2.nextInAEL = ae.nextInAEL;
      if (ae.nextInAEL != null) ae.nextInAEL.prevInAEL = ae2;
      ae2.prevInAEL = ae;
      ae.nextInAEL = ae2;
    }

    private void InsertLocalMinimaIntoAEL(long botY)
    {
      LocalMinima localMinima;
      Active? leftBound, rightBound;
      // Add any local minima (if any) at BotY ...
      // NB horizontal local minima edges should contain locMin.vertex.prev
      while (HasLocMinAtY(botY))
      {
        localMinima = PopLocalMinima();
        if ((localMinima.vertex.flags & VertexFlags.OpenStart) != VertexFlags.None)
        {
          leftBound = null;
        }
        else
        {
          leftBound = new Active
          {
            bot = localMinima.vertex.pt,
            curX = localMinima.vertex.pt.X,
            windDx = -1,
            vertexTop = localMinima.vertex.prev,
            top = localMinima.vertex.prev!.pt,
            outrec = null,
            localMin = localMinima
          };
          SetDx(leftBound);
        }

        if ((localMinima.vertex.flags & VertexFlags.OpenEnd) != VertexFlags.None)
        {
          rightBound = null;
        }
        else
        {
          rightBound = new Active
          {
            bot = localMinima.vertex.pt,
            curX = localMinima.vertex.pt.X,
            windDx = 1,
            vertexTop = localMinima.vertex.next, // i.e. ascending
            top = localMinima.vertex.next!.pt,
            outrec = null,
            localMin = localMinima
          };
          SetDx(rightBound);
        }

        // Currently LeftB is just the descending bound and RightB is the ascending.
        // Now if the LeftB isn't on the left of RightB then we need swap them.
        if (leftBound != null && rightBound != null)
        {
          if (IsHorizontal(leftBound))
          {
            if (IsHeadingRightHorz(leftBound)) SwapActives(ref leftBound, ref rightBound);
          }
          else if (IsHorizontal(rightBound))
          {
            if (IsHeadingLeftHorz(rightBound)) SwapActives(ref leftBound, ref rightBound);
          }
          else if (leftBound.dx < rightBound.dx)
            SwapActives(ref leftBound, ref rightBound);
          //so when leftBound has windDx == 1, the polygon will be oriented
          //counter-clockwise in Cartesian coords (clockwise with inverted Y).
        }
        else if (leftBound == null)
        {
          leftBound = rightBound;
          rightBound = null;
        }

        bool contributing;
        leftBound!.isLeftBound = true;
        InsertLeftEdge(leftBound);

        if (IsOpen(leftBound))
        {
          SetWindCountForOpenPathEdge(leftBound);
          contributing = IsContributingOpen(leftBound);
        }
        else
        {
          SetWindCountForClosedPathEdge(leftBound);
          contributing = IsContributingClosed(leftBound);
        }

        if (rightBound != null)
        {
          rightBound.windCount = leftBound.windCount;
          rightBound.windCount2 = leftBound.windCount2;
          InsertRightEdge(leftBound, rightBound); ///////

          if (contributing)
          {
            AddLocalMinPoly(leftBound, rightBound, leftBound.bot, true);
            if (!IsHorizontal(leftBound))
              CheckJoinLeft(leftBound, leftBound.bot);
          }

          while (rightBound.nextInAEL != null &&
                 IsValidAelOrder(rightBound.nextInAEL, rightBound))
          {
            IntersectEdges(rightBound, rightBound.nextInAEL, rightBound.bot);
            SwapPositionsInAEL(rightBound, rightBound.nextInAEL);
          }

          if (IsHorizontal(rightBound))
            PushHorz(rightBound);
          else
          {
            CheckJoinRight(rightBound, rightBound.bot);
            InsertScanline(rightBound.top.Y);
          }
        }
        else if (contributing)
          StartOpenPath(leftBound, leftBound.bot);

        if (IsHorizontal(leftBound))
          PushHorz(leftBound);
        else
          InsertScanline(leftBound.top.Y);
      } // while (HasLocMinAtY())
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void PushHorz(Active ae)
    {
      ae.nextInSEL = _sel;
      _sel = ae;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool PopHorz(out Active? ae)
    {
      ae = _sel;
      if (_sel == null) return false;
      _sel = _sel.nextInSEL;
      return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private OutPt AddLocalMinPoly(Active ae1, Active ae2, Point64 pt, bool isNew = false)
    {
      OutRec outrec = NewOutRec();
      ae1.outrec = outrec;
      ae2.outrec = outrec;

      if (IsOpen(ae1))
      {
        outrec.owner = null;
        outrec.isOpen = true;
        if (ae1.windDx > 0)
          SetSides(outrec, ae1, ae2);
        else
          SetSides(outrec, ae2, ae1);
      }
      else
      {
        outrec.isOpen = false;
        Active? prevHotEdge = GetPrevHotEdge(ae1);
        // e.windDx is the winding direction of the **input** paths
        // and unrelated to the winding direction of output polygons.
        // Output orientation is determined by e.outrec.frontE which is
        // the ascending edge (see AddLocalMinPoly).
        if (prevHotEdge != null)
        {
          if (_using_polytree)
            SetOwner(outrec, prevHotEdge.outrec!);
          outrec.owner = prevHotEdge.outrec;
          if (OutrecIsAscending(prevHotEdge) == isNew)
            SetSides(outrec, ae2, ae1);
          else
            SetSides(outrec, ae1, ae2);
        }
        else
        {
          outrec.owner = null;
          if (isNew)
            SetSides(outrec, ae1, ae2);
          else
            SetSides(outrec, ae2, ae1);
        }
      }

      OutPt op = new OutPt(pt, outrec);
      outrec.pts = op;
      return op;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private OutPt? AddLocalMaxPoly(Active ae1, Active ae2, Point64 pt)
    {
      if (IsJoined(ae1)) Split(ae1, pt);
      if (IsJoined(ae2)) Split(ae2, pt);

      if (IsFront(ae1) == IsFront(ae2))
      {
        if (IsOpenEnd(ae1))
          SwapFrontBackSides(ae1.outrec!);
        else if (IsOpenEnd(ae2))
          SwapFrontBackSides(ae2.outrec!);
        else
        {
          _succeeded = false;
          return null;
        }
      }

      OutPt result = AddOutPt(ae1, pt);
      if (ae1.outrec == ae2.outrec)
      {
        OutRec outrec = ae1.outrec!;
        outrec.pts = result;

        if (_using_polytree)
        {
          Active? e = GetPrevHotEdge(ae1);
          if (e == null)
            outrec.owner = null;
          else
            SetOwner(outrec, e.outrec!);
          // nb: outRec.owner here is likely NOT the real
          // owner but this will be fixed in DeepCheckOwner()
        }
        UncoupleOutRec(ae1);
      }
      // and to preserve the winding orientation of outrec ...
      else if (IsOpen(ae1))
      {
        if (ae1.windDx < 0)
          JoinOutrecPaths(ae1, ae2);
        else
          JoinOutrecPaths(ae2, ae1);
      }
      else if (ae1.outrec!.idx < ae2.outrec!.idx)
        JoinOutrecPaths(ae1, ae2);
      else
        JoinOutrecPaths(ae2, ae1);
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void JoinOutrecPaths(Active ae1, Active ae2)
    {
      // join ae2 outrec path onto ae1 outrec path and then delete ae2 outrec path
      // pointers. (NB Only very rarely do the joining ends share the same coords.)
      OutPt p1Start = ae1.outrec!.pts!;
      OutPt p2Start = ae2.outrec!.pts!;
      OutPt p1End = p1Start.next!;
      OutPt p2End = p2Start.next!;
      if (IsFront(ae1))
      {
        p2End.prev = p1Start;
        p1Start.next = p2End;
        p2Start.next = p1End;
        p1End.prev = p2Start;
        ae1.outrec.pts = p2Start;
        // nb: if IsOpen(e1) then e1 & e2 must be a 'maximaPair'
        ae1.outrec.frontEdge = ae2.outrec.frontEdge;
        if (ae1.outrec.frontEdge != null)
          ae1.outrec.frontEdge!.outrec = ae1.outrec;
      }
      else
      {
        p1End.prev = p2Start;
        p2Start.next = p1End;
        p1Start.next = p2End;
        p2End.prev = p1Start;

        ae1.outrec.backEdge = ae2.outrec.backEdge;
        if (ae1.outrec.backEdge != null)
          ae1.outrec.backEdge!.outrec = ae1.outrec;
      }

      // after joining, the ae2.OutRec must contains no vertices ...
      ae2.outrec.frontEdge = null;
      ae2.outrec.backEdge = null;
      ae2.outrec.pts = null;
      SetOwner(ae2.outrec, ae1.outrec);

      if (IsOpenEnd(ae1))
      {
        ae2.outrec.pts = ae1.outrec.pts;
        ae1.outrec.pts = null;
      }

      // and ae1 and ae2 are maxima and are about to be dropped from the Actives list.
      ae1.outrec = null;
      ae2.outrec = null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static OutPt AddOutPt(Active ae, Point64 pt)
    {

      // Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
      // opFront[.Prev]* ~~~> opBack & opBack == opFront.Next
      OutRec outrec = ae.outrec!;
      bool toFront = IsFront(ae);
      OutPt opFront = outrec.pts!;
      OutPt opBack = opFront.next!;

      if (toFront && (pt == opFront.pt)) return opFront;
      else if (!toFront && (pt == opBack.pt)) return opBack;

      OutPt newOp = new OutPt(pt, outrec);
      opBack.prev = newOp;
      newOp.prev = opFront;
      newOp.next = opBack;
      opFront.next = newOp;
      if (toFront) outrec.pts = newOp;
      return newOp;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private OutRec NewOutRec()
    {
      OutRec result = new OutRec
      {
        idx = _outrecList.Count
      };
      _outrecList.Add(result);
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private OutPt StartOpenPath(Active ae, Point64 pt)
    {
      OutRec outrec = NewOutRec();
      outrec.isOpen = true;
      if (ae.windDx > 0)
      {
        outrec.frontEdge = ae;
        outrec.backEdge = null;
      }
      else
      {
        outrec.frontEdge = null;
        outrec.backEdge = ae;
      }

      ae.outrec = outrec;
      OutPt op = new OutPt(pt, outrec);
      outrec.pts = op;
      return op;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void UpdateEdgeIntoAEL(Active ae)
    {
      ae.bot = ae.top;
      ae.vertexTop = NextVertex(ae);
      ae.top = ae.vertexTop!.pt;
      ae.curX = ae.bot.X;
      SetDx(ae);

      if (IsJoined(ae)) Split(ae, ae.bot);

      if (IsHorizontal(ae)) 
      {
        if (!IsOpen(ae)) TrimHorz(ae, PreserveCollinear);
        return; 
      }
      InsertScanline(ae.top.Y);

      CheckJoinLeft(ae, ae.bot);
      CheckJoinRight(ae, ae.bot, true); // (#500)
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Active? FindEdgeWithMatchingLocMin(Active e)
    {
      Active? result = e.nextInAEL;
      while (result != null)
      {
        if (result.localMin == e.localMin) return result;
        if (!IsHorizontal(result) && e.bot != result.bot) result = null;
        else result = result.nextInAEL;
      }
      result = e.prevInAEL;
      while (result != null)
      {
        if (result.localMin == e.localMin) return result;
        if (!IsHorizontal(result) && e.bot != result.bot) return null;
        result = result.prevInAEL;
      }
      return result;
    }

    private void IntersectEdges(Active ae1, Active ae2, Point64 pt)
    {
      OutPt? resultOp = null;
      // MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
      if (_hasOpenPaths && (IsOpen(ae1) || IsOpen(ae2)))
      {
        if (IsOpen(ae1) && IsOpen(ae2)) return;
        // the following line avoids duplicating quite a bit of code
        if (IsOpen(ae2)) SwapActives(ref ae1, ref ae2);
        if (IsJoined(ae2)) Split(ae2, pt); // needed for safety

        if (_cliptype == ClipType.Union)
        {
          if (!IsHotEdge(ae2)) return;
        }
        else if (ae2.localMin.polytype == PathType.Subject) return;

        switch (_fillrule)
        {
          case FillRule.Positive:
            if (ae2.windCount != 1) return; 
            break;
          case FillRule.Negative:
            if (ae2.windCount != -1) return; 
            break;
          default:
            if (Math.Abs(ae2.windCount) != 1) return; 
            break;
        }

        // toggle contribution ...
        if (IsHotEdge(ae1))
        {
          resultOp = AddOutPt(ae1, pt);
#if USINGZ
          SetZ(ae1, ae2, ref resultOp.pt);
#endif
          if (IsFront(ae1))
            ae1.outrec!.frontEdge = null;
          else
            ae1.outrec!.backEdge = null;
          ae1.outrec = null;
        }

        // horizontal edges can pass under open paths at a LocMins
        else if (pt == ae1.localMin.vertex.pt &&
          !IsOpenEnd(ae1.localMin.vertex))
        {
          // find the other side of the LocMin and
          // if it's 'hot' join up with it ...
          Active? ae3 = FindEdgeWithMatchingLocMin(ae1);
          if (ae3 != null && IsHotEdge(ae3))
          {
            ae1.outrec = ae3.outrec;
            if (ae1.windDx > 0)
              SetSides(ae3.outrec!, ae1, ae3);
            else
              SetSides(ae3.outrec!, ae3, ae1);
            return;
          }

          resultOp = StartOpenPath(ae1, pt);
        }
        else
          resultOp = StartOpenPath(ae1, pt);

#if USINGZ
        SetZ(ae1, ae2, ref resultOp.pt);
#endif
        return;
      }

      // MANAGING CLOSED PATHS FROM HERE ON
      if (IsJoined(ae1)) Split(ae1, pt);
      if (IsJoined(ae2)) Split(ae2, pt);

      // UPDATE WINDING COUNTS...

      int oldE1WindCount, oldE2WindCount;
      if (ae1.localMin.polytype == ae2.localMin.polytype)
      {
        if (_fillrule == FillRule.EvenOdd)
        {
          oldE1WindCount = ae1.windCount;
          ae1.windCount = ae2.windCount;
          ae2.windCount = oldE1WindCount;
        }
        else
        {
          if (ae1.windCount + ae2.windDx == 0)
            ae1.windCount = -ae1.windCount;
          else
            ae1.windCount += ae2.windDx;
          if (ae2.windCount - ae1.windDx == 0)
            ae2.windCount = -ae2.windCount;
          else
            ae2.windCount -= ae1.windDx;
        }
      }
      else
      {
        if (_fillrule != FillRule.EvenOdd)
          ae1.windCount2 += ae2.windDx;
        else
          ae1.windCount2 = (ae1.windCount2 == 0 ? 1 : 0);
        if (_fillrule != FillRule.EvenOdd)
          ae2.windCount2 -= ae1.windDx;
        else
          ae2.windCount2 = (ae2.windCount2 == 0 ? 1 : 0);
      }

      switch (_fillrule)
      {
        case FillRule.Positive:
          oldE1WindCount = ae1.windCount;
          oldE2WindCount = ae2.windCount;
          break;
        case FillRule.Negative:
          oldE1WindCount = -ae1.windCount;
          oldE2WindCount = -ae2.windCount;
          break;
        default:
          oldE1WindCount = Math.Abs(ae1.windCount);
          oldE2WindCount = Math.Abs(ae2.windCount);
          break;
      }

      bool e1WindCountIs0or1 = oldE1WindCount == 0 || oldE1WindCount == 1;
      bool e2WindCountIs0or1 = oldE2WindCount == 0 || oldE2WindCount == 1;

      if ((!IsHotEdge(ae1) && !e1WindCountIs0or1) || 
        (!IsHotEdge(ae2) && !e2WindCountIs0or1)) return;

      // NOW PROCESS THE INTERSECTION ...

      // if both edges are 'hot' ...
      if (IsHotEdge(ae1) && IsHotEdge(ae2))
      {
        if ((oldE1WindCount != 0 && oldE1WindCount != 1) || (oldE2WindCount != 0 && oldE2WindCount != 1) ||
            (ae1.localMin.polytype != ae2.localMin.polytype && _cliptype != ClipType.Xor))
        {
          resultOp = AddLocalMaxPoly(ae1, ae2, pt);
#if USINGZ
          if (resultOp != null)
            SetZ(ae1, ae2, ref resultOp.pt);
#endif
        }
        else if (IsFront(ae1) || (ae1.outrec == ae2.outrec))
        {
          // this 'else if' condition isn't strictly needed but
          // it's sensible to split polygons that ony touch at
          // a common vertex (not at common edges).
          resultOp = AddLocalMaxPoly(ae1, ae2, pt);
#if USINGZ
          OutPt op2 = AddLocalMinPoly(ae1, ae2, pt);
          if (resultOp != null)
            SetZ(ae1, ae2, ref resultOp.pt);
          SetZ(ae1, ae2, ref op2.pt);
#else
          AddLocalMinPoly(ae1, ae2, pt);
#endif
        }
        else
        {
          // can't treat as maxima & minima
          resultOp = AddOutPt(ae1, pt);
#if USINGZ
          OutPt op2 = AddOutPt(ae2, pt);
          SetZ(ae1, ae2, ref resultOp.pt);
          SetZ(ae1, ae2, ref op2.pt);
#else
          AddOutPt(ae2, pt);
#endif
          SwapOutrecs(ae1, ae2);
        }
      }

      // if one or other edge is 'hot' ...
      else if (IsHotEdge(ae1))
      {
        resultOp = AddOutPt(ae1, pt);
#if USINGZ
        SetZ(ae1, ae2, ref resultOp.pt);
#endif
        SwapOutrecs(ae1, ae2);
      }
      else if (IsHotEdge(ae2))
      {
        resultOp = AddOutPt(ae2, pt);
#if USINGZ
        SetZ(ae1, ae2, ref resultOp.pt);
#endif
        SwapOutrecs(ae1, ae2);
      }

      // neither edge is 'hot'
      else
      {
        long e1Wc2, e2Wc2;
        switch (_fillrule)
        {
          case FillRule.Positive:
            e1Wc2 = ae1.windCount2;
            e2Wc2 = ae2.windCount2;
            break;
          case FillRule.Negative:
            e1Wc2 = -ae1.windCount2;
            e2Wc2 = -ae2.windCount2;
            break;
          default:
            e1Wc2 = Math.Abs(ae1.windCount2);
            e2Wc2 = Math.Abs(ae2.windCount2);
            break;
        }

        if (!IsSamePolyType(ae1, ae2))
        {
          resultOp = AddLocalMinPoly(ae1, ae2, pt);
#if USINGZ
          SetZ(ae1, ae2, ref resultOp.pt);
#endif
        }
        else if (oldE1WindCount == 1 && oldE2WindCount == 1)
        {
          resultOp = null; 
          switch (_cliptype)
          {
            case ClipType.Union:
              if (e1Wc2 > 0 && e2Wc2 > 0) return;
              resultOp = AddLocalMinPoly(ae1, ae2, pt);
              break;

            case ClipType.Difference:
              if (((GetPolyType(ae1) == PathType.Clip) && (e1Wc2 > 0) && (e2Wc2 > 0)) ||
                  ((GetPolyType(ae1) == PathType.Subject) && (e1Wc2 <= 0) && (e2Wc2 <= 0)))
              {
                resultOp = AddLocalMinPoly(ae1, ae2, pt);
              }

              break;

            case ClipType.Xor:
              resultOp = AddLocalMinPoly(ae1, ae2, pt);
              break;

            default: // ClipType.Intersection:
              if (e1Wc2 <= 0 || e2Wc2 <= 0) return;
              resultOp = AddLocalMinPoly(ae1, ae2, pt);
              break;
          }
#if USINGZ
          if (resultOp != null) SetZ(ae1, ae2, ref resultOp.pt);
#endif
        }
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DeleteFromAEL(Active ae)
    {
      Active? prev = ae.prevInAEL;
      Active? next = ae.nextInAEL;
      if (prev == null && next == null && (ae != _actives)) return; // already deleted
      if (prev != null)
        prev.nextInAEL = next;
      else
        _actives = next;
      if (next != null) next.prevInAEL = prev;
      // delete &ae;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void AdjustCurrXAndCopyToSEL(long topY)
    {
      Active? ae = _actives;
      _sel = ae;
      while (ae != null)
      {
        ae.prevInSEL = ae.prevInAEL;
        ae.nextInSEL = ae.nextInAEL;
        ae.jump = ae.nextInSEL;
        if (ae.joinWith == JoinWith.Left)
          ae.curX = ae.prevInAEL!.curX; // this also avoids complications
        else
          ae.curX = TopX(ae, topY);
        // NB don't update ae.curr.Y yet (see AddNewIntersectNode)
        ae = ae.nextInAEL;
      }
    }

    protected void ExecuteInternal(ClipType ct, FillRule fillRule)
    {
      if (ct == ClipType.None) return;
      _fillrule = fillRule;
      _cliptype = ct;
      Reset();
      if (!PopScanline(out long y)) return;
      while (_succeeded)
      {
        InsertLocalMinimaIntoAEL(y);
        Active? ae;
        while (PopHorz(out ae)) DoHorizontal(ae!);
        if (_horzSegList.Count > 0)
        {
          ConvertHorzSegsToJoins();
          _horzSegList.Clear();
        }
        _currentBotY = y; // bottom of scanbeam
        if (!PopScanline(out y))
          break; // y new top of scanbeam
        DoIntersections(y);
        DoTopOfScanbeam(y);
        while (PopHorz(out ae)) DoHorizontal(ae!);
      }
      if (_succeeded) ProcessHorzJoins(); 
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoIntersections(long topY)
    {
      if (BuildIntersectList(topY))
      {
        ProcessIntersectList();
        DisposeIntersectNodes();
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DisposeIntersectNodes()
    {
      _intersectList.Clear();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void AddNewIntersectNode(Active ae1, Active ae2, long topY)
    {
      if (!InternalClipper.GetSegmentIntersectPt(
        ae1.bot, ae1.top, ae2.bot, ae2.top, out Point64 ip))
          ip = new Point64(ae1.curX, topY);

      if (ip.Y > _currentBotY || ip.Y < topY)
      {
        double absDx1 = Math.Abs(ae1.dx);
        double absDx2 = Math.Abs(ae2.dx);
        if (absDx1 > 100 && absDx2 > 100)
        {
          if (absDx1 > absDx2)
            ip = InternalClipper.GetClosestPtOnSegment(ip, ae1.bot, ae1.top);
          else
            ip = InternalClipper.GetClosestPtOnSegment(ip, ae2.bot, ae2.top);
        }
        else if (absDx1 > 100)
          ip = InternalClipper.GetClosestPtOnSegment(ip, ae1.bot, ae1.top);
        else if (absDx2 > 100)
          ip = InternalClipper.GetClosestPtOnSegment(ip, ae2.bot, ae2.top);
        else
        {
          if (ip.Y < topY) ip.Y = topY;
          else ip.Y = _currentBotY;
          if (absDx1 < absDx2) ip.X = TopX(ae1, ip.Y);
          else ip.X = TopX(ae2, ip.Y);
        }
      }
      IntersectNode node = new IntersectNode(ip, ae1, ae2);
      _intersectList.Add(node);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Active? ExtractFromSEL(Active ae)
    {
      Active? res = ae.nextInSEL;
      if (res != null)
        res.prevInSEL = ae.prevInSEL;
      ae.prevInSEL!.nextInSEL = res;
      return res;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void Insert1Before2InSEL(Active ae1, Active ae2)
    {
      ae1.prevInSEL = ae2.prevInSEL;
      if (ae1.prevInSEL != null)
        ae1.prevInSEL.nextInSEL = ae1;
      ae1.nextInSEL = ae2;
      ae2.prevInSEL = ae1;
    }

    private bool BuildIntersectList(long topY)
    {
      if (_actives == null || _actives.nextInAEL == null) return false;

      // Calculate edge positions at the top of the current scanbeam, and from this
      // we will determine the intersections required to reach these new positions.
      AdjustCurrXAndCopyToSEL(topY);

      // Find all edge intersections in the current scanbeam using a stable merge
      // sort that ensures only adjacent edges are intersecting. Intersect info is
      // stored in FIntersectList ready to be processed in ProcessIntersectList.
      // Re merge sorts see https://stackoverflow.com/a/46319131/359538

      Active? left = _sel, right, lEnd, rEnd, currBase, prevBase, tmp;

      while (left!.jump != null)
      {
        prevBase = null;
        while (left != null && left.jump != null)
        {
          currBase = left;
          right = left.jump;
          lEnd = right;
          rEnd = right.jump;
          left.jump = rEnd;
          while (left != lEnd && right != rEnd)
          {
            if (right!.curX < left!.curX)
            {
              tmp = right.prevInSEL!;
              for (; ; )
              {
                AddNewIntersectNode(tmp, right, topY);
                if (tmp == left) break;
                tmp = tmp.prevInSEL!;
              }

              tmp = right;
              right = ExtractFromSEL(tmp);
              lEnd = right;
              Insert1Before2InSEL(tmp, left);
              if (left == currBase)
              {
                currBase = tmp;
                currBase.jump = rEnd;
                if (prevBase == null) _sel = currBase;
                else prevBase.jump = currBase;
              }
            }
            else left = left.nextInSEL;
          }

          prevBase = currBase;
          left = rEnd;
        }
        left = _sel;
      }

      return _intersectList.Count > 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void ProcessIntersectList()
    {
      // We now have a list of intersections required so that edges will be
      // correctly positioned at the top of the scanbeam. However, it's important
      // that edge intersections are processed from the bottom up, but it's also
      // crucial that intersections only occur between adjacent edges.

      // First we do a quicksort so intersections proceed in a bottom up order ...
      _intersectList.Sort(new IntersectListSort());

      // Now as we process these intersections, we must sometimes adjust the order
      // to ensure that intersecting edges are always adjacent ...
      for (int i = 0; i < _intersectList.Count; ++i)
      {
        if (!EdgesAdjacentInAEL(_intersectList[i]))
        {
          int j = i + 1;
          while (!EdgesAdjacentInAEL(_intersectList[j])) j++;
          // swap
          (_intersectList[j], _intersectList[i]) =
            (_intersectList[i], _intersectList[j]);
        }

        IntersectNode node = _intersectList[i];
        IntersectEdges(node.edge1, node.edge2, node.pt);
        SwapPositionsInAEL(node.edge1, node.edge2);

        node.edge1.curX = node.pt.X;
        node.edge2.curX = node.pt.X;
        CheckJoinLeft(node.edge2, node.pt, true);
        CheckJoinRight(node.edge1, node.pt, true);
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void SwapPositionsInAEL(Active ae1, Active ae2)
    {
      // preconditon: ae1 must be immediately to the left of ae2
      Active? next = ae2.nextInAEL;
      if (next != null) next.prevInAEL = ae1;
      Active? prev = ae1.prevInAEL;
      if (prev != null) prev.nextInAEL = ae2;
      ae2.prevInAEL = prev;
      ae2.nextInAEL = ae1;
      ae1.prevInAEL = ae2;
      ae1.nextInAEL = next;
      if (ae2.prevInAEL == null) _actives = ae2;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool ResetHorzDirection(Active horz, Vertex? vertexMax,
        out long leftX, out long rightX)
    {
      if (horz.bot.X == horz.top.X)
      {
        // the horizontal edge is going nowhere ...
        leftX = horz.curX;
        rightX = horz.curX;
        Active? ae = horz.nextInAEL;
        while (ae != null && ae.vertexTop != vertexMax)
          ae = ae.nextInAEL;
        return ae != null;
      }

      if (horz.curX < horz.top.X)
      {
        leftX = horz.curX;
        rightX = horz.top.X;
        return true;
      }
      leftX = horz.top.X;
      rightX = horz.curX;
      return false; // right to left
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void TrimHorz(Active horzEdge, bool preserveCollinear)
    {
      bool wasTrimmed = false;
      Point64 pt = NextVertex(horzEdge).pt;

      while (pt.Y == horzEdge.top.Y)
      {
        // always trim 180 deg. spikes (in closed paths)
        // but otherwise break if preserveCollinear = true
        if (preserveCollinear &&
        (pt.X < horzEdge.top.X) != (horzEdge.bot.X < horzEdge.top.X))
          break;

        horzEdge.vertexTop = NextVertex(horzEdge);
        horzEdge.top = pt;
        wasTrimmed = true;
        if (IsMaxima(horzEdge)) break;
        pt = NextVertex(horzEdge).pt;
      }
      if (wasTrimmed) SetDx(horzEdge); // +/-infinity
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void AddToHorzSegList(OutPt op)
    {
      if (op.outrec.isOpen) return;
      _horzSegList.Add(new HorzSegment(op));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private OutPt GetLastOp(Active hotEdge)
    { 
      OutRec outrec = hotEdge.outrec!;
      return (hotEdge == outrec.frontEdge) ?
        outrec.pts! : outrec.pts!.next!;
    }

private void DoHorizontal(Active horz)
    /*******************************************************************************
     * Notes: Horizontal edges (HEs) at scanline intersections (i.e. at the top or    *
     * bottom of a scanbeam) are processed as if layered.The order in which HEs     *
     * are processed doesn't matter. HEs intersect with the bottom vertices of      *
     * other HEs[#] and with non-horizontal edges [*]. Once these intersections     *
     * are completed, intermediate HEs are 'promoted' to the next edge in their     *
     * bounds, and they in turn may be intersected[%] by other HEs.                 *
     *                                                                              *
     * eg: 3 horizontals at a scanline:    /   |                     /           /  *
     *              |                     /    |     (HE3)o ========%========== o   *
     *              o ======= o(HE2)     /     |         /         /                *
     *          o ============#=========*======*========#=========o (HE1)           *
     *         /              |        /       |       /                            *
     *******************************************************************************/
    {
      Point64 pt;
      bool horzIsOpen = IsOpen(horz);
      long Y = horz.bot.Y;

      Vertex? vertex_max = horzIsOpen ?
        GetCurrYMaximaVertex_Open(horz) :
        GetCurrYMaximaVertex(horz);

      bool isLeftToRight =
        ResetHorzDirection(horz, vertex_max, out long leftX, out long rightX);

      if (IsHotEdge(horz))
      {
#if USINGZ
        OutPt op = AddOutPt(horz, new Point64(horz.curX, Y, horz.bot.Z));
#else
        OutPt op = AddOutPt(horz, new Point64(horz.curX, Y));
#endif
        AddToHorzSegList(op);
      }

      for (; ; )
      {
        // loops through consec. horizontal edges (if open)
        Active? ae = isLeftToRight ? horz.nextInAEL : horz.prevInAEL;

        while (ae != null)
        {
          if (ae.vertexTop == vertex_max)
          {
            // do this first!!
            if (IsHotEdge(horz) && IsJoined(ae)) Split(ae, ae.top);

            if (IsHotEdge(horz))
            {
              while (horz.vertexTop != vertex_max)
              {
                AddOutPt(horz, horz.top);
                UpdateEdgeIntoAEL(horz);
              }
              if (isLeftToRight)
                AddLocalMaxPoly(horz, ae, horz.top);
              else
                AddLocalMaxPoly(ae, horz, horz.top);
            }
            DeleteFromAEL(ae);
            DeleteFromAEL(horz);
            return;
          }

          // if horzEdge is a maxima, keep going until we reach
          // its maxima pair, otherwise check for break conditions
          if (vertex_max != horz.vertexTop || IsOpenEnd(horz))
          {
            // otherwise stop when 'ae' is beyond the end of the horizontal line
            if ((isLeftToRight && ae.curX > rightX) ||
                (!isLeftToRight && ae.curX < leftX)) break;

            if (ae.curX == horz.top.X && !IsHorizontal(ae))
            {
              pt = NextVertex(horz).pt;

              // to maximize the possibility of putting open edges into
              // solutions, we'll only break if it's past HorzEdge's end
              if (IsOpen(ae) && !IsSamePolyType(ae, horz) && !IsHotEdge(ae))
              {
                if ((isLeftToRight && (TopX(ae, pt.Y) > pt.X)) ||
                  (!isLeftToRight && (TopX(ae, pt.Y) < pt.X))) break;
              }
              // otherwise for edges at horzEdge's end, only stop when horzEdge's
              // outslope is greater than e's slope when heading right or when
              // horzEdge's outslope is less than e's slope when heading left.
              else if ((isLeftToRight && (TopX(ae, pt.Y) >= pt.X)) ||
                  (!isLeftToRight && (TopX(ae, pt.Y) <= pt.X))) break;
            }
          }

          pt = new Point64(ae.curX, Y);

          if (isLeftToRight)
          {
            IntersectEdges(horz, ae, pt);
            SwapPositionsInAEL(horz, ae);
            CheckJoinLeft(ae, pt);
            horz.curX = ae.curX;
            ae = horz.nextInAEL;
          }
          else
          {
            IntersectEdges(ae, horz, pt);
            SwapPositionsInAEL(ae, horz);
            CheckJoinRight(ae, pt);
            horz.curX = ae.curX;
            ae = horz.prevInAEL;
          }

          if (IsHotEdge(horz))
            AddToHorzSegList(GetLastOp(horz));

        } // we've reached the end of this horizontal

        // check if we've finished looping
        // through consecutive horizontals
        if (horzIsOpen && IsOpenEnd(horz)) // ie open at top
        {
          if (IsHotEdge(horz))
          {
            AddOutPt(horz, horz.top);
            if (IsFront(horz))
              horz.outrec!.frontEdge = null;
            else
              horz.outrec!.backEdge = null;
            horz.outrec = null;
          }
          DeleteFromAEL(horz);
          return;
        }
        else if (NextVertex(horz).pt.Y != horz.top.Y)
          break;

        //still more horizontals in bound to process ...
        if (IsHotEdge(horz))
          AddOutPt(horz, horz.top);

        UpdateEdgeIntoAEL(horz);

        isLeftToRight = ResetHorzDirection(horz,
          vertex_max, out leftX, out rightX);

      } // end for loop and end of (possible consecutive) horizontals

      if (IsHotEdge(horz)) 
      {
        OutPt op = AddOutPt(horz, horz.top);
        AddToHorzSegList(op);
      }

      UpdateEdgeIntoAEL(horz); // this is the end of an intermediate horiz.
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoTopOfScanbeam(long y)
    {
      _sel = null; // sel_ is reused to flag horizontals (see PushHorz below)
      Active? ae = _actives;
      while (ae != null)
      {
        // NB 'ae' will never be horizontal here
        if (ae.top.Y == y)
        {
          ae.curX = ae.top.X;
          if (IsMaxima(ae))
          {
            ae = DoMaxima(ae); // TOP OF BOUND (MAXIMA)
            continue;
          }

          // INTERMEDIATE VERTEX ...
          if (IsHotEdge(ae))
            AddOutPt(ae, ae.top);
          UpdateEdgeIntoAEL(ae);
          if (IsHorizontal(ae))
            PushHorz(ae); // horizontals are processed later
        }
        else // i.e. not the top of the edge
          ae.curX = TopX(ae, y);

        ae = ae.nextInAEL;
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private Active? DoMaxima(Active ae)
    {
      Active? prevE;
      Active? nextE, maxPair;
      prevE = ae.prevInAEL;
      nextE = ae.nextInAEL;

      if (IsOpenEnd(ae))
      {
        if (IsHotEdge(ae)) AddOutPt(ae, ae.top);
        if (!IsHorizontal(ae))
        {
          if (IsHotEdge(ae))
          {
            if (IsFront(ae))
              ae.outrec!.frontEdge = null;
            else
              ae.outrec!.backEdge = null;
            ae.outrec = null;
          }
          DeleteFromAEL(ae);
        }
        return nextE;
      }

      maxPair = GetMaximaPair(ae);
      if (maxPair == null) return nextE; // eMaxPair is horizontal

      if (IsJoined(ae)) Split(ae, ae.top);
      if (IsJoined(maxPair)) Split(maxPair, maxPair.top);

      // only non-horizontal maxima here.
      // process any edges between maxima pair ...
      while (nextE != maxPair)
      {
        IntersectEdges(ae, nextE!, ae.top);
        SwapPositionsInAEL(ae, nextE!);
        nextE = ae.nextInAEL;
      }

      if (IsOpen(ae))
      {
        if (IsHotEdge(ae))
          AddLocalMaxPoly(ae, maxPair, ae.top);
        DeleteFromAEL(maxPair);
        DeleteFromAEL(ae);
        return (prevE != null ? prevE.nextInAEL : _actives);
      }

      // here ae.nextInAel == ENext == EMaxPair ...
      if (IsHotEdge(ae))
        AddLocalMaxPoly(ae, maxPair, ae.top);

      DeleteFromAEL(ae);
      DeleteFromAEL(maxPair);
      return (prevE != null ? prevE.nextInAEL : _actives);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsJoined(Active e)
    {
      return e.joinWith != JoinWith.None;
    }

    private void Split(Active e, Point64 currPt)
    {
      if (e.joinWith == JoinWith.Right)
      {
        e.joinWith = JoinWith.None;
        e.nextInAEL!.joinWith = JoinWith.None;
        AddLocalMinPoly(e, e.nextInAEL, currPt, true);
      }
      else
      {
        e.joinWith = JoinWith.None;
        e.prevInAEL!.joinWith = JoinWith.None;
        AddLocalMinPoly(e.prevInAEL, e, currPt, true);
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CheckJoinLeft(Active e,
      Point64 pt, bool checkCurrX = false)
    {
      Active? prev = e.prevInAEL;
      if (prev == null || 
        !IsHotEdge(e) || !IsHotEdge(prev) || 
        IsHorizontal(e) || IsHorizontal(prev) ||
        IsOpen(e) || IsOpen(prev)) return;
      if ((pt.Y < e.top.Y + 2 || pt.Y < prev.top.Y + 2) &&  // avoid trivial joins
        ((e.bot.Y > pt.Y) || (prev.bot.Y > pt.Y))) return;  // (#490)

      if (checkCurrX)
      {
        if (Clipper.PerpendicDistFromLineSqrd(pt, prev.bot, prev.top) > 0.25) return;
      }
      else if (e.curX != prev.curX) return;
      if (!InternalClipper.IsCollinear(e.top, pt, prev.top)) return;

      if (e.outrec!.idx == prev.outrec!.idx)
        AddLocalMaxPoly(prev, e, pt);
      else if (e.outrec.idx < prev.outrec.idx)
        JoinOutrecPaths(e, prev);
      else
        JoinOutrecPaths(prev, e);
      prev.joinWith = JoinWith.Right;
      e.joinWith = JoinWith.Left;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CheckJoinRight(Active e, 
      Point64 pt, bool checkCurrX = false)
    {
      Active? next = e.nextInAEL;
      if (next == null || 
        !IsHotEdge(e) || !IsHotEdge(next) || 
        IsHorizontal(e) || IsHorizontal(next) ||
        IsOpen(e) || IsOpen(next)) return; 
      if ((pt.Y < e.top.Y + 2 || pt.Y < next.top.Y + 2) &&  // avoid trivial joins
        ((e.bot.Y > pt.Y) || (next.bot.Y > pt.Y)))  return; // (#490)

      if (checkCurrX)
      {
        if (Clipper.PerpendicDistFromLineSqrd(pt, next.bot, next.top) > 0.25) return;
      }
      else if (e.curX != next.curX) return;
      if (!InternalClipper.IsCollinear(e.top, pt, next.top)) return;

      if (e.outrec!.idx == next.outrec!.idx)
        AddLocalMaxPoly(e, next, pt);
      else if (e.outrec.idx < next.outrec.idx)
        JoinOutrecPaths(e, next);
      else
        JoinOutrecPaths(next, e);
      e.joinWith = JoinWith.Right;
      next.joinWith = JoinWith.Left;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void FixOutRecPts(OutRec outrec)
    {
      OutPt op = outrec.pts!;
      do
      {
        op.outrec = outrec;
        op = op.next!;
      } while (op != outrec.pts);
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool SetHorzSegHeadingForward(HorzSegment hs, OutPt opP, OutPt opN)
    {
      if (opP.pt.X == opN.pt.X) return false;
      if (opP.pt.X < opN.pt.X)
      {
        hs.leftOp = opP;
        hs.rightOp = opN;
        hs.leftToRight = true;
      }
      else
      {
        hs.leftOp = opN;
        hs.rightOp = opP;
        hs.leftToRight = false;
      }
      return true;
    }

    private static bool UpdateHorzSegment(HorzSegment hs)
    {
      OutPt op = hs.leftOp!;
      OutRec outrec = GetRealOutRec(op.outrec)!;
      bool outrecHasEdges = outrec.frontEdge != null;
      long curr_y = op.pt.Y;
      OutPt opP = op, opN = op;
      if (outrecHasEdges)
      {
        OutPt opA = outrec.pts!, opZ = opA.next!;
        while (opP != opZ && opP.prev.pt.Y == curr_y)
          opP = opP.prev;
        while (opN != opA && opN.next!.pt.Y == curr_y)
          opN = opN.next;
      }
      else
      {
        while (opP.prev != opN && opP.prev.pt.Y == curr_y)
          opP = opP.prev;
        while (opN.next != opP && opN.next!.pt.Y == curr_y)
          opN = opN.next;
      }
      bool result =
        SetHorzSegHeadingForward(hs, opP, opN) &&
        hs.leftOp!.horz == null;

      if (result)
        hs.leftOp!.horz = hs;
      else
        hs.rightOp = null; // (for sorting)
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static OutPt DuplicateOp(OutPt op, bool insert_after)
    {
      OutPt result = new OutPt(op.pt, op.outrec);
      if (insert_after)
      {
        result.next = op.next;
        result.next!.prev = result;
        result.prev = op;
        op.next = result;
      }
      else
      {
        result.prev = op.prev;
        result.prev.next = result;
        result.next = op;
        op.prev = result;
      }
      return result;
    }

    private int HorzSegSort(HorzSegment? hs1, HorzSegment? hs2)
    {
      if (hs1 == null || hs2 == null) return 0;
      if (hs1.rightOp == null)
      {
        return hs2.rightOp == null ? 0 : 1;
      }
      else if (hs2.rightOp == null)
        return -1;
      else
        return hs1.leftOp!.pt.X.CompareTo(hs2.leftOp!.pt.X);
    }

    private void ConvertHorzSegsToJoins()
    {
      int k = 0;
      foreach (HorzSegment hs in _horzSegList)
        if (UpdateHorzSegment(hs)) k++;
      if (k < 2) return;
      _horzSegList.Sort(HorzSegSort);

      for (int i = 0; i < k -1; i++)
      {
        HorzSegment hs1 = _horzSegList[i];
        // for each HorzSegment, find others that overlap
        for (int j = i + 1; j < k; j++)
        {
          HorzSegment hs2 = _horzSegList[j];
          if ((hs2.leftOp!.pt.X >= hs1.rightOp!.pt.X) || 
            (hs2.leftToRight == hs1.leftToRight) ||
            (hs2.rightOp!.pt.X <= hs1.leftOp!.pt.X)) continue;
          long curr_y = hs1.leftOp.pt.Y;
          if ((hs1).leftToRight)
          {
            while (hs1.leftOp.next!.pt.Y == curr_y &&
              hs1.leftOp.next.pt.X <= hs2.leftOp.pt.X)
              hs1.leftOp = hs1.leftOp.next;
            while (hs2.leftOp.prev.pt.Y == curr_y &&
              hs2.leftOp.prev.pt.X <= hs1.leftOp.pt.X)
              (hs2).leftOp = (hs2).leftOp.prev;
            HorzJoin join = new HorzJoin(
              DuplicateOp((hs1).leftOp, true),
              DuplicateOp((hs2).leftOp, false));
            _horzJoinList.Add(join);
          }
          else
          {
            while (hs1.leftOp.prev.pt.Y == curr_y &&
              hs1.leftOp.prev.pt.X <= hs2.leftOp.pt.X)
              hs1.leftOp = hs1.leftOp.prev;
            while (hs2.leftOp.next!.pt.Y == curr_y &&
              hs2.leftOp.next.pt.X <= (hs1).leftOp.pt.X)
              hs2.leftOp = (hs2).leftOp.next;
            HorzJoin join = new HorzJoin(
              DuplicateOp((hs2).leftOp, true),
              DuplicateOp((hs1).leftOp, false));
            _horzJoinList.Add(join);
          }
        }
      } 
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Path64 GetCleanPath(OutPt op)
    {
      Path64 result = new Path64();
      OutPt op2 = op;
      while (op2.next != op &&
        ((op2.pt.X == op2.next!.pt.X && op2.pt.X == op2.prev.pt.X) ||
          (op2.pt.Y == op2.next.pt.Y && op2.pt.Y == op2.prev.pt.Y))) op2 = op2.next;
      result.Add(op2.pt);
      OutPt prevOp = op2;
      op2 = op2.next;
      while (op2 != op)
      {
        if ((op2.pt.X != op2.next!.pt.X || op2.pt.X != prevOp.pt.X) &&
          (op2.pt.Y != op2.next.pt.Y || op2.pt.Y != prevOp.pt.Y))
        {
          result.Add(op2.pt);
          prevOp = op2;
        }
        op2 = op2.next;
      }
      return result;
    }


    private static PointInPolygonResult PointInOpPolygon(Point64 pt, OutPt op)
    {
      if (op == op.next || op.prev == op.next)
        return PointInPolygonResult.IsOutside;

      OutPt op2 = op;
      do
      {
        if (op.pt.Y != pt.Y) break;
        op = op.next!;
      } while (op != op2);
      if (op.pt.Y == pt.Y) // not a proper polygon
        return PointInPolygonResult.IsOutside;

      // must be above or below to get here
      bool isAbove = op.pt.Y < pt.Y, startingAbove = isAbove;
      int val = 0;

      op2 = op.next!;
      while (op2 != op)
      {
        if (isAbove)
          while (op2 != op && op2.pt.Y < pt.Y) op2 = op2.next!;
        else
          while (op2 != op && op2.pt.Y > pt.Y) op2 = op2.next!;
        if (op2 == op) break;

        // must have touched or crossed the pt.Y horizonal
        // and this must happen an even number of times

        if (op2.pt.Y == pt.Y) // touching the horizontal
        {
          if (op2.pt.X == pt.X || (op2.pt.Y == op2.prev.pt.Y &&
            (pt.X < op2.prev.pt.X) != (pt.X < op2.pt.X)))
            return PointInPolygonResult.IsOn;
          op2 = op2.next!;
          if (op2 == op) break;
          continue;
        }

        if (op2.pt.X <= pt.X || op2.prev.pt.X <= pt.X)
        {          
          if ((op2.prev.pt.X < pt.X && op2.pt.X < pt.X))
            val = 1 - val; // toggle val
          else
          {
            double d = InternalClipper.CrossProduct(op2.prev.pt, op2.pt, pt);
            if (d == 0) return PointInPolygonResult.IsOn;
            if ((d < 0) == isAbove) val = 1 - val;
          } 
        }
        isAbove = !isAbove;
        op2 = op2.next!;
      }

      if (isAbove != startingAbove)
      {
        double d = InternalClipper.CrossProduct(op2.prev.pt, op2.pt, pt);
        if (d == 0) return PointInPolygonResult.IsOn;
        if ((d < 0) == isAbove) val = 1 - val;
      }

      if (val == 0) return PointInPolygonResult.IsOutside;
      else return PointInPolygonResult.IsInside;
    }

    private static bool Path1InsidePath2(OutPt op1, OutPt op2)
    {
      // we need to make some accommodation for rounding errors
      // so we won't jump if the first vertex is found outside
      PointInPolygonResult result;
      int outside_cnt = 0;
      OutPt op = op1;
      do
      {
        result = PointInOpPolygon(op.pt, op2);
        if (result == PointInPolygonResult.IsOutside) ++outside_cnt;
        else if (result == PointInPolygonResult.IsInside) --outside_cnt;
        op = op.next!;
      } while (op != op1 && Math.Abs(outside_cnt) < 2);
      if (Math.Abs(outside_cnt) > 1) return (outside_cnt < 0);
      // since path1's location is still equivocal, check its midpoint
      Point64 mp = GetBounds(GetCleanPath(op1)).MidPoint();
      Path64 path2 = GetCleanPath(op2);
      return InternalClipper.PointInPolygon(mp, path2) != PointInPolygonResult.IsOutside;
    }

    private void MoveSplits(OutRec fromOr, OutRec toOr)
    {
      if (fromOr.splits == null) return;
      toOr.splits ??= new List<int>();
      foreach (int i in fromOr.splits)
        toOr.splits.Add(i);
      fromOr.splits = null;
    }

    private void ProcessHorzJoins()
    {
      foreach (HorzJoin j in _horzJoinList)
      {
        OutRec or1 = GetRealOutRec(j.op1!.outrec)!;
        OutRec or2 = GetRealOutRec(j.op2!.outrec)!;

        OutPt op1b = j.op1.next!;
        OutPt op2b = j.op2.prev;
        j.op1.next = j.op2;
        j.op2.prev = j.op1;
        op1b.prev = op2b;
        op2b.next = op1b;

        if (or1 == or2) // 'join' is really a split
        {
          or2 = NewOutRec();
          or2.pts = op1b;
          FixOutRecPts(or2);

          //if or1->pts has moved to or2 then update or1->pts!!
          if (or1.pts!.outrec == or2)
          {
            or1.pts = j.op1;
            or1.pts.outrec = or1;
          }

          if (_using_polytree)  //#498, #520, #584, D#576, #618
          {
            if (Path1InsidePath2(or1.pts, or2.pts))
            {
              //swap or1's & or2's pts
              (or2.pts, or1.pts) = (or1.pts, or2.pts);
              FixOutRecPts(or1);
              FixOutRecPts(or2);
              //or2 is now inside or1
              or2.owner = or1;
            }
            else if (Path1InsidePath2(or2.pts, or1.pts))
              or2.owner = or1;
            else
              or2.owner = or1.owner;

            or1.splits ??= new List<int>();
            or1.splits.Add(or2.idx);
          }
          else
            or2.owner = or1;
        }
        else
        {
          or2.pts = null;
          if (_using_polytree)
          {
            SetOwner(or2, or1);
            MoveSplits(or2, or1); //#618
          }
          else
            or2.owner = or1;
        }
      }
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool PtsReallyClose(Point64 pt1, Point64 pt2)
    {
      return (Math.Abs(pt1.X - pt2.X) < 2) && (Math.Abs(pt1.Y - pt2.Y) < 2);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsVerySmallTriangle(OutPt op)
	  {
		  return op.next!.next == op.prev &&
			(PtsReallyClose(op.prev.pt, op.next.pt) ||
				PtsReallyClose(op.pt, op.next.pt) ||
				PtsReallyClose(op.pt, op.prev.pt));
	  }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsValidClosedPath(OutPt? op)
    {
      return (op != null && op.next != op &&
        (op.next != op.prev || !IsVerySmallTriangle(op)));
    }

    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static OutPt? DisposeOutPt(OutPt op)
    {
      OutPt? result = (op.next == op ? null : op.next);
      op.prev.next = op.next;
      op.next!.prev = op.prev;
      // op == null;
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CleanCollinear(OutRec? outrec)
    {
      outrec = GetRealOutRec(outrec);
      
      if (outrec == null || outrec.isOpen) return;

      if(!IsValidClosedPath(outrec.pts))
      {
        outrec.pts = null;
        return;
      }

      OutPt startOp = outrec.pts!;
      OutPt? op2 = startOp;
      for (; ; )
      {
        // NB if preserveCollinear == true, then only remove 180 deg. spikes
        if ((InternalClipper.IsCollinear(op2!.prev.pt, op2.pt, op2.next!.pt)) &&
          ((op2.pt == op2.prev.pt) || (op2.pt == op2.next.pt) || !PreserveCollinear ||
          (InternalClipper.DotProduct(op2.prev.pt, op2.pt, op2.next.pt) < 0)))
        {
          if (op2 == outrec.pts)
            outrec.pts = op2.prev;
          op2 = DisposeOutPt(op2);
          if (!IsValidClosedPath(op2))
          {
            outrec.pts = null;
            return;
          }
          startOp = op2!;
          continue;
        }
        op2 = op2.next;
        if (op2 == startOp) break;
      }
      FixSelfIntersects(outrec);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoSplitOp(OutRec outrec, OutPt splitOp)
    {
      // splitOp.prev <=> splitOp &&
      // splitOp.next <=> splitOp.next.next are intersecting
      OutPt prevOp = splitOp.prev;
      OutPt nextNextOp = splitOp.next!.next!;
      outrec.pts = prevOp;

      InternalClipper.GetSegmentIntersectPt(
          prevOp.pt, splitOp.pt, splitOp.next.pt, nextNextOp.pt, out Point64 ip);

#if USINGZ
      if (_zCallback != null)
        _zCallback(prevOp.pt, splitOp.pt, splitOp.next.pt, nextNextOp.pt, ref ip);
#endif

      double area1 = Area(prevOp);
      double absArea1 = Math.Abs(area1);
      
      if (absArea1 < 2)
      {
        outrec.pts = null;
        return;
      }

      double area2 = AreaTriangle(ip, splitOp.pt, splitOp.next.pt);
      double absArea2 = Math.Abs(area2);

      // de-link splitOp and splitOp.next from the path
      // while inserting the intersection point
      if (ip == prevOp.pt || ip == nextNextOp.pt)
      {
        nextNextOp.prev = prevOp;
        prevOp.next = nextNextOp;
      }
      else
      {
        OutPt newOp2 = new OutPt(ip, outrec) { prev = prevOp, next = nextNextOp };        
        nextNextOp.prev = newOp2;
        prevOp.next = newOp2;
      }

      // nb: area1 is the path's area *before* splitting, whereas area2 is
      // the area of the triangle containing splitOp & splitOp.next.
      // So the only way for these areas to have the same sign is if
      // the split triangle is larger than the path containing prevOp or
      // if there's more than one self=intersection.
      if (absArea2 > 1 &&
          (absArea2 > absArea1 ||
           ((area2 > 0) == (area1 > 0))))
      {
        OutRec newOutRec = NewOutRec();
        newOutRec.owner = outrec.owner;
        splitOp.outrec = newOutRec;
        splitOp.next.outrec = newOutRec;

        OutPt newOp = new OutPt(ip, newOutRec) { prev = splitOp.next, next = splitOp };
        newOutRec.pts = newOp;
        splitOp.prev = newOp;
        splitOp.next.next = newOp;

        if (_using_polytree)
        {
          if (Path1InsidePath2(prevOp, newOp))
          {
            newOutRec.splits ??= new List<int>();
            newOutRec.splits.Add(outrec.idx);
          }
          else
          {
            outrec.splits ??= new List<int>();
            outrec.splits.Add(newOutRec.idx);
          }
        }
      }
      //else { splitOp = null; splitOp.next = null; }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void FixSelfIntersects(OutRec outrec)
    {
      OutPt op2 = outrec.pts!;
      for (; ; )
      {
        // triangles can't self-intersect
        if (op2.prev == op2.next!.next) break;
        if (InternalClipper.SegsIntersect(op2.prev.pt,
                op2.pt, op2.next.pt, op2.next.next!.pt))
        {
          DoSplitOp(outrec, op2);
          if (outrec.pts == null) return;
          op2 = outrec.pts;
          continue;
        }
        else
          op2 = op2.next;
        if (op2 == outrec.pts) break;
      }
    }

    internal static bool BuildPath(OutPt? op, bool reverse, bool isOpen, Path64 path)
    {
      if (op == null || op.next == op || (!isOpen && op.next == op.prev)) return false;
      path.Clear();

      Point64 lastPt;
      OutPt op2;
      if (reverse)
      {
        lastPt = op.pt;
        op2 = op.prev;
      }
      else
      {
        op = op.next!;
        lastPt = op.pt;
        op2 = op.next!;
      }
      path.Add(lastPt);
        
      while (op2 != op)
      {
        if (op2.pt != lastPt)
        {
          lastPt = op2.pt;
          path.Add(lastPt);
        }
        if (reverse)
          op2 = op2.prev;
        else
          op2 = op2.next!;
      }

      if (path.Count == 3 && !isOpen && IsVerySmallTriangle(op2)) return false;
      else return true;
    }

    protected bool BuildPaths(Paths64 solutionClosed, Paths64 solutionOpen)
    {
      solutionClosed.Clear();
      solutionOpen.Clear();
      solutionClosed.EnsureCapacity(_outrecList.Count);
      solutionOpen.EnsureCapacity(_outrecList.Count);
      
      int i = 0;
      // _outrecList.Count is not static here because
      // CleanCollinear can indirectly add additional OutRec
      while (i < _outrecList.Count)
      {
        OutRec outrec = _outrecList[i++];
        if (outrec.pts == null) continue;

        Path64 path = new Path64();
        if (outrec.isOpen)
        {
          if (BuildPath(outrec.pts, ReverseSolution, true, path))
              solutionOpen.Add(path);
        }
        else
        {
          CleanCollinear(outrec);
          // closed paths should always return a Positive orientation
          // except when ReverseSolution == true
          if (BuildPath(outrec.pts, ReverseSolution, false, path))
            solutionClosed.Add(path);
        }
      }
      return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Rect64 GetBounds(Path64 path)
	  {
		  if (path.Count == 0) return new Rect64();
      Rect64 result = Clipper.InvalidRect64;
		  foreach (Point64 pt in path)
		  {
			  if (pt.X < result.left) result.left = pt.X;
			  if (pt.X > result.right) result.right = pt.X;
			  if (pt.Y < result.top) result.top = pt.Y;
			  if (pt.Y > result.bottom) result.bottom = pt.Y;
		  }
		  return result;
	  }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool CheckBounds(OutRec outrec)
    {
      if (outrec.pts == null) return false;
      if (!outrec.bounds.IsEmpty()) return true;
      CleanCollinear(outrec);
      if (outrec.pts == null ||
        !BuildPath(outrec.pts, ReverseSolution, false, outrec.path))
          return false;
      outrec.bounds = GetBounds(outrec.path);
      return true;
    }

    private bool CheckSplitOwner(OutRec outrec, List<int>? splits)
    {
      foreach (int i in splits!)
      {
        OutRec? split = GetRealOutRec(_outrecList[i]);
        if (split == null || split == outrec || split.recursiveSplit == outrec) continue;
        split.recursiveSplit = outrec; //#599
        if (split.splits != null && CheckSplitOwner(outrec, split.splits)) return true;
        if (IsValidOwner(outrec, split) && 
          CheckBounds(split) && 
          split.bounds.Contains(outrec.bounds) &&
          Path1InsidePath2(outrec.pts!, split.pts!))
        {
          outrec.owner = split; //found in split
          return true;
        }
      }
      return false;
    }
    private void RecursiveCheckOwners(OutRec outrec, PolyPathBase polypath)
    {
      // pre-condition: outrec will have valid bounds
      // post-condition: if a valid path, outrec will have a polypath

      if (outrec.polypath != null || outrec.bounds.IsEmpty()) return;

      while (outrec.owner != null)
      {
        if (outrec.owner.splits != null && 
          CheckSplitOwner(outrec, outrec.owner.splits)) break; 
        else if (outrec.owner.pts != null && CheckBounds(outrec.owner) &&
          Path1InsidePath2(outrec.pts!, outrec.owner.pts!)) break;
        outrec.owner = outrec.owner.owner;
      }

      if (outrec.owner != null)
      {
        if (outrec.owner.polypath == null)
          RecursiveCheckOwners(outrec.owner, polypath);
        outrec.polypath = outrec.owner.polypath!.AddChild(outrec.path); 
      }
      else
        outrec.polypath = polypath.AddChild(outrec.path);
    }

    protected void BuildTree(PolyPathBase polytree, Paths64 solutionOpen)
    {
      polytree.Clear();
      solutionOpen.Clear();
      if (_hasOpenPaths)
        solutionOpen.EnsureCapacity(_outrecList.Count);

      int i = 0;
      // _outrecList.Count is not static here because
      // CheckBounds below can indirectly add additional
      // OutRec (via FixOutRecPts & CleanCollinear)
      while (i < _outrecList.Count)
      {
        OutRec outrec = _outrecList[i++];
        if (outrec.pts == null) continue;

        if (outrec.isOpen)
        {
          Path64 open_path = new Path64();
          if (BuildPath(outrec.pts, ReverseSolution, true, open_path))
            solutionOpen.Add(open_path);
          continue;
        }
        if (CheckBounds(outrec))
          RecursiveCheckOwners(outrec, polytree);
      }
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Rect64 GetBounds()
    {
      Rect64 bounds = Clipper.InvalidRect64;
      foreach (Vertex t in _vertexList)
      {
        Vertex v = t;
        do
        {
          if (v.pt.X < bounds.left) bounds.left = v.pt.X;
          if (v.pt.X > bounds.right) bounds.right = v.pt.X;
          if (v.pt.Y < bounds.top) bounds.top = v.pt.Y;
          if (v.pt.Y > bounds.bottom) bounds.bottom = v.pt.Y;
          v = v.next!;
        } while (v != t);
      }
      return bounds.IsEmpty() ? new Rect64(0, 0, 0, 0) : bounds;
    }

  } // ClipperBase class


  public class Clipper64 : ClipperBase
  {
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal new void AddPath(Path64 path, PathType polytype, bool isOpen = false)
    {
      base.AddPath(path, polytype, isOpen);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public new void AddReuseableData(ReuseableDataContainer64 reuseableData)
    {
      base.AddReuseableData(reuseableData);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal new void AddPaths(Paths64 paths, PathType polytype, bool isOpen = false)
    {
      base.AddPaths(paths, polytype, isOpen);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddSubject(Paths64 paths)
    {
      AddPaths(paths, PathType.Subject);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddOpenSubject(Paths64 paths)
    {
      AddPaths(paths, PathType.Subject, true);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddClip(Paths64 paths)
    {
      AddPaths(paths, PathType.Clip);
    }

    public bool Execute(ClipType clipType, FillRule fillRule,
        Paths64 solutionClosed, Paths64 solutionOpen)
    {
      solutionClosed.Clear();
      solutionOpen.Clear();
      try
      {
        ExecuteInternal(clipType, fillRule);
        BuildPaths(solutionClosed, solutionOpen);
      }
      catch
      {
        _succeeded = false;
      }

      ClearSolutionOnly();
      return _succeeded;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public bool Execute(ClipType clipType, FillRule fillRule, Paths64 solutionClosed)
    {
      return Execute(clipType, fillRule, solutionClosed, new Paths64());
    }

    public bool Execute(ClipType clipType, FillRule fillRule, PolyTree64 polytree, Paths64 openPaths)
    {
      polytree.Clear();
      openPaths.Clear();
      _using_polytree = true;
      try
      {
        ExecuteInternal(clipType, fillRule);
        BuildTree(polytree, openPaths);
      }
      catch
      {
        _succeeded = false;
      }

      ClearSolutionOnly();
      return _succeeded;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public bool Execute(ClipType clipType, FillRule fillRule, PolyTree64 polytree)
    {
      return Execute(clipType, fillRule, polytree, new Paths64());
    }

#if USINGZ
    public ZCallback64? ZCallback {
      get { return this._zCallback; }
      set { this._zCallback = value; } 
    }
#endif

  } // Clipper64 class

  public class ClipperD : ClipperBase
  {
    private readonly string precision_range_error = "Error: Precision is out of range.";

    private readonly double _scale;
    private readonly double _invScale;

#if USINGZ
    public delegate void ZCallbackD(PointD bot1, PointD top1,
        PointD bot2, PointD top2, ref PointD intersectPt);

    public ZCallbackD? ZCallback { get; set; }

    private void CheckZCallback()
    {
      if (ZCallback != null)
        _zCallback = ZCB;
      else
        _zCallback = null;
    }
#endif

    public ClipperD(int roundingDecimalPrecision = 2)
    {
      if (roundingDecimalPrecision < -8 || roundingDecimalPrecision > 8)
        throw new ClipperLibException(precision_range_error);
      _scale = Math.Pow(10, roundingDecimalPrecision);
      _invScale = 1 / _scale;
    }

#if USINGZ
    private void ZCB(Point64 bot1, Point64 top1,
        Point64 bot2, Point64 top2, ref Point64 intersectPt)
    {
      // de-scale (x & y)
      // temporarily convert integers to their initial float values
      // this will slow clipping marginally but will make it much easier
      // to understand the coordinates passed to the callback function
      PointD tmp = Clipper.ScalePointD(intersectPt, _invScale);
      //do the callback
      ZCallback?.Invoke(
        Clipper.ScalePointD(bot1, _invScale),
        Clipper.ScalePointD(top1, _invScale),
        Clipper.ScalePointD(bot2, _invScale),
        Clipper.ScalePointD(top2, _invScale), ref tmp);
      intersectPt = new Point64(intersectPt.X,
          intersectPt.Y, tmp.z);
    }
#endif

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddPath(PathD path, PathType polytype, bool isOpen = false)
    {
      base.AddPath(Clipper.ScalePath64(path, _scale), polytype, isOpen);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddPaths(PathsD paths, PathType polytype, bool isOpen = false)
    {
      base.AddPaths(Clipper.ScalePaths64(paths, _scale), polytype, isOpen);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddSubject(PathD path)
    {
      AddPath(path, PathType.Subject);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddOpenSubject(PathD path)
    {
      AddPath(path, PathType.Subject, true);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddClip(PathD path)
    {
      AddPath(path, PathType.Clip);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddSubject(PathsD paths)
    {
      AddPaths(paths, PathType.Subject);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddOpenSubject(PathsD paths)
    {
      AddPaths(paths, PathType.Subject, true);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AddClip(PathsD paths)
    {
      AddPaths(paths, PathType.Clip);
    }

    public bool Execute(ClipType clipType, FillRule fillRule,
        PathsD solutionClosed, PathsD solutionOpen)
    {
      Paths64 solClosed64 = new Paths64(), solOpen64 = new Paths64();
#if USINGZ
      CheckZCallback();
#endif

      bool success = true;
      solutionClosed.Clear();
      solutionOpen.Clear();
      try
      {
        ExecuteInternal(clipType, fillRule);
        BuildPaths(solClosed64, solOpen64);
      }
      catch
      {
        success = false;
      }

      ClearSolutionOnly();
      if (!success) return false;

      solutionClosed.EnsureCapacity(solClosed64.Count);
      foreach (Path64 path in solClosed64)
        solutionClosed.Add(Clipper.ScalePathD(path, _invScale));
      solutionOpen.EnsureCapacity(solOpen64.Count);
      foreach (Path64 path in solOpen64)
        solutionOpen.Add(Clipper.ScalePathD(path, _invScale));

      return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public bool Execute(ClipType clipType, FillRule fillRule, PathsD solutionClosed)
    {
      return Execute(clipType, fillRule, solutionClosed, new PathsD());
    }

    public bool Execute(ClipType clipType, FillRule fillRule, PolyTreeD polytree, PathsD openPaths)
    {
      polytree.Clear();
      openPaths.Clear();
      _using_polytree = true;
      (polytree as PolyPathD).Scale = _scale;
#if USINGZ
      CheckZCallback();
#endif
      Paths64 oPaths = new Paths64();
      bool success = true;
      try
      {
        ExecuteInternal(clipType, fillRule);
        BuildTree(polytree, oPaths);
      }
      catch
      {
        success = false;
      }
      ClearSolutionOnly();
      if (!success) return false;
      if (oPaths.Count > 0)
      {
        openPaths.EnsureCapacity(oPaths.Count);
        foreach (Path64 path in oPaths)
          openPaths.Add(Clipper.ScalePathD(path, _invScale));
      }

      return true;
    }

    public bool Execute(ClipType clipType, FillRule fillRule, PolyTreeD polytree)
    {
      return Execute(clipType, fillRule, polytree, new PathsD());
    }
  } // ClipperD class

  public abstract class PolyPathBase : IEnumerable
  {
    internal PolyPathBase? _parent;
    internal List<PolyPathBase> _childs = new List<PolyPathBase>();

    public IEnumerator GetEnumerator()
    {
      return new NodeEnumerator(_childs);
    }
    private class NodeEnumerator : IEnumerator
    {
      private int position = -1;
      private readonly List<PolyPathBase> _nodes;

      [MethodImpl(MethodImplOptions.AggressiveInlining)]
      public NodeEnumerator(List<PolyPathBase> nodes)
      {
        _nodes = new List<PolyPathBase>(nodes);
      }

      
      [MethodImpl(MethodImplOptions.AggressiveInlining)]
      public bool MoveNext()
      {
        position++;
        return (position < _nodes.Count);
      }

      [MethodImpl(MethodImplOptions.AggressiveInlining)]
      public void Reset()
      {
        position = -1;
      }

      public object Current
      {
        get
        {
          if (position < 0 || position >= _nodes.Count)
            throw new InvalidOperationException();
          return _nodes[position];
        }
      }

    };

    public bool IsHole => GetIsHole();

    public PolyPathBase(PolyPathBase? parent = null) { _parent = parent; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private int GetLevel()
    {
      int result = 0;
      PolyPathBase? pp = _parent;
      while (pp != null) { ++result; pp = pp._parent; }
      return result;
    }

    public int Level => GetLevel();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool GetIsHole()
    {
      int lvl = GetLevel();
      return lvl != 0 && (lvl & 1) == 0;
    }

    public int Count => _childs.Count;
    public abstract PolyPathBase AddChild(Path64 p);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Clear()
    {
      _childs.Clear();
    }

    internal string ToStringInternal(int idx, int level)
    {
      string result = "", padding = "", plural = "s";
      if (_childs.Count == 1) plural = "";
      padding = padding.PadLeft(level * 2);
      if ((level & 1) == 0)
        result += string.Format("{0}+- hole ({1}) contains {2} nested polygon{3}.\n", padding, idx, _childs.Count, plural);
      else
        result += string.Format("{0}+- polygon ({1}) contains {2} hole{3}.\n", padding, idx, _childs.Count, plural);

      for (int i = 0; i < Count; i++)
        if (_childs[i].Count > 0)
          result += _childs[i].ToStringInternal(i, level +1);
      return result;
    }

    public override string ToString()
    {
      if (Level > 0) return ""; //only accept tree root 
      string plural = "s";
      if (_childs.Count == 1) plural = "";
      string result = string.Format("Polytree with {0} polygon{1}.\n", _childs.Count, plural);
      for (int i = 0; i < Count; i++)
        if (_childs[i].Count > 0)
          result += _childs[i].ToStringInternal(i, 1);
      return result + '\n';
    }

} // PolyPathBase class

public class PolyPath64 : PolyPathBase
  {
    public Path64? Polygon { get; private set; } // polytree root's polygon == null

    public PolyPath64(PolyPathBase? parent = null) : base(parent) {}

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public override PolyPathBase AddChild(Path64 p)
    {
      PolyPathBase newChild = new PolyPath64(this);
      (newChild as PolyPath64)!.Polygon = p;
      _childs.Add(newChild);
      return newChild;
    }

    public PolyPath64 this[int index]
    {
      get
      {
        if (index < 0 || index >= _childs.Count)
          throw new InvalidOperationException();
        return (PolyPath64) _childs[index];
      }
    }

    public PolyPath64 Child(int index)
    {
      if (index < 0 || index >= _childs.Count)
        throw new InvalidOperationException();
      return (PolyPath64) _childs[index];
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public double Area()
    {
      double result = Polygon == null ? 0 : Clipper.Area(Polygon);
      foreach (PolyPathBase polyPathBase in _childs)
      {
        PolyPath64 child = (PolyPath64) polyPathBase;
        result += child.Area();
      }
      return result;
    }
  }
  public class PolyPathD : PolyPathBase
  {
    internal double Scale { get; set; }
    public PathD? Polygon { get; private set; }

    public PolyPathD(PolyPathBase? parent = null) : base(parent) {}

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public override PolyPathBase AddChild(Path64 p)
    {
      PolyPathBase newChild = new PolyPathD(this);
      (newChild as PolyPathD)!.Scale = Scale;
      (newChild as PolyPathD)!.Polygon = Clipper.ScalePathD(p, 1 / Scale);
      _childs.Add(newChild);
      return newChild;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public PolyPathBase AddChild(PathD p)
    {
      PolyPathBase newChild = new PolyPathD(this);
      (newChild as PolyPathD)!.Scale = Scale;
      (newChild as PolyPathD)!.Polygon = p;
      _childs.Add(newChild);
      return newChild;
    }

    [IndexerName("Child")]
    public PolyPathD this[int index]
    {
      get
      {
        if (index < 0 || index >= _childs.Count)
          throw new InvalidOperationException();
        return (PolyPathD) _childs[index];
      }
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public double Area()
    {
      double result = Polygon == null ? 0 : Clipper.Area(Polygon);
      foreach (PolyPathBase polyPathBase in _childs)
      {
        PolyPathD child = (PolyPathD) polyPathBase;
        result += child.Area();
      }
      return result;
    }
  }

  public class PolyTree64 : PolyPath64 {}

  public class PolyTreeD : PolyPathD
  {
    public new double Scale => base.Scale;
  }

  public class ClipperLibException : Exception
  {
    public ClipperLibException(string description) : base(description) {}
  }
} // namespace