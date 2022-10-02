/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - ver.1.0.5                                            *
* Date      :  2 October 2022                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
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
  internal struct IntersectNode
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
    public int Compare(LocalMinima locMin1, LocalMinima locMin2)
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
    public Joiner? joiner;

    public OutPt(Point64 pt, OutRec outrec)
    {
      this.pt = pt;
      this.outrec = outrec;
      next = this;
      prev = this;
      joiner = null;
    }
  };

  // OutRec: path data structure for clipping solutions
  internal class OutRec
  {
    public int idx;
    public OutRec? owner;
    public List<OutRec>? splits;
    public Active? frontEdge;
    public Active? backEdge;
    public OutPt? pts;
    public PolyPathBase? polypath;
    public Rect64 bounds;
    public Path64 path;
    public bool isOpen;
    public OutRec() 
    { 
      bounds = new Rect64(); 
      path = new Path64(); 
    }
  };

  // Joiner: structure used in merging "touching" solution polygons
  internal class Joiner
  {
    public int idx;
    public OutPt op1;
    public OutPt? op2;
    public Joiner? next1;
    public Joiner? next2;
    public Joiner? nextH;

    public Joiner(OutPt op1, OutPt? op2, Joiner? nextH)
    {
      idx = -1;
      this.nextH = nextH;
      this.op1 = op1;
      this.op2 = op2;
      next1 = op1.joiner;
      op1.joiner = this;

      if (op2 != null)
      {
        next2 = op2.joiner;
        op2.joiner = this;
      }
      else
        next2 = null;
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
  };

  public class ClipperBase
  {
    private ClipType _cliptype;
    private FillRule _fillrule;
    private Active? _actives;
    private Active? _sel;
    private Joiner? _horzJoiners;
    private readonly List<LocalMinima> _minimaList;
    private readonly List<IntersectNode> _intersectList;
    private readonly List<Vertex> _vertexList;
    private readonly List<OutRec> _outrecList;
    private readonly List<Joiner?> _joinerList;
    private readonly List<long> _scanlineList;
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

    protected ZCallback64? _zCallback;
#endif
    public ClipperBase()
    {
      _minimaList = new List<LocalMinima>();
      _intersectList = new List<IntersectNode>();
      _vertexList = new List<Vertex>();
      _outrecList = new List<OutRec>();
      _joinerList = new List<Joiner?>();
      _scanlineList = new List<long>();
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
      return ae.bot.X + (long) Math.Round(ae.dx * (currentY - ae.bot.Y));
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
    private static Point64 GetIntersectPoint(Active ae1, Active ae2)
    {
      double b1, b2;
      if (InternalClipper.IsAlmostZero(ae1.dx - ae2.dx)) return ae1.top;

      if (InternalClipper.IsAlmostZero(ae1.dx))
      {
        if (IsHorizontal(ae2)) return new Point64(ae1.bot.X, ae2.bot.Y);
        b2 = ae2.bot.Y - (ae2.bot.X / ae2.dx);
        return new Point64(ae1.bot.X, (long) Math.Round(ae1.bot.X / ae2.dx + b2));
      }

      if (InternalClipper.IsAlmostZero(ae2.dx))
      {
        if (IsHorizontal(ae1)) return new Point64(ae2.bot.X, ae1.bot.Y);
        b1 = ae1.bot.Y - (ae1.bot.X / ae1.dx);
        return new Point64(ae2.bot.X, (long) Math.Round(ae2.bot.X / ae1.dx + b1));
      }
      b1 = ae1.bot.X - ae1.bot.Y * ae1.dx;
      b2 = ae2.bot.X - ae2.bot.Y * ae2.dx;
      double q = (b2 - b1) / (ae1.dx - ae2.dx);
      return (Math.Abs(ae1.dx) < Math.Abs(ae2.dx))
        //? new Point64((long) Math.Round(ae1.dx * q + b1), (long) Math.Round(q))
        //: new Point64((long) Math.Round(ae2.dx * q + b2), (long) Math.Round(q));
        ? new Point64((long)(ae1.dx * q + b1), (long)(q))
        : new Point64((long)(ae2.dx * q + b2), (long)(q));
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

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Active? GetHorzMaximaPair(Active horz, Vertex maxVert)
    {
      // we can't be sure whether the MaximaPair is on the left or right, so ...
      Active? result = horz.prevInAEL;
      while (result != null && result.curX >= maxVert.pt.X)
      {
        if (result.vertexTop == maxVert) return result;  // Found!
        result = result.prevInAEL;
      }
      result = horz.nextInAEL;
      while (result != null && TopX(result, horz.top.Y) <= maxVert.pt.X)
      {
        if (result.vertexTop == maxVert) return result;  // Found!
        result = result.nextInAEL;
      }
      return null;
    }

    private struct IntersectListSort : IComparer<IntersectNode>
    {
      public int Compare(IntersectNode a, IntersectNode b)
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
    private static double Area(OutPt op)
    {
      // https://en.wikipedia.org/wiki/Shoelace_formula
      double area = 0.0;
      OutPt op2 = op;
      do
      {
        area += (double)(op2.prev.pt.Y + op2.pt.Y) * 
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
    protected void ClearSolution()
    {
      while (_actives != null) DeleteFromAEL(_actives);
      _scanlineList.Clear();
      DisposeIntersectNodes();
      _joinerList.Clear();
      _horzJoiners = null;
      _outrecList.Clear();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Clear()
    {
      ClearSolution();
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

      _scanlineList.Capacity = _minimaList.Count;
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
    private void AddLocMin(Vertex vert, PathType polytype, bool isOpen)
    {
      // make sure the vertex is added only once ...
      if ((vert.flags & VertexFlags.LocalMin) != VertexFlags.None) return;
      vert.flags |= VertexFlags.LocalMin;

      LocalMinima lm = new LocalMinima(vert, polytype, isOpen);
      _minimaList.Add(lm);
    }

    protected void AddPathsToVertexList(Paths64 paths, PathType polytype, bool isOpen)
    {
      int totalVertCnt = 0;
      foreach (Path64 path in paths) totalVertCnt += path.Count;
      _vertexList.Capacity = _vertexList.Count + totalVertCnt;

      foreach (Path64 path in paths) 
      {
        Vertex? v0 = null, prev_v = null, curr_v;
        foreach (Point64 pt in path)
        {
          if (v0 == null)
          {
            v0 = new Vertex(pt, VertexFlags.None, null);
            _vertexList.Add(v0);
            prev_v = v0;
          }
          else if (prev_v!.pt != pt) // ie skips duplicates
          {
            curr_v = new Vertex(pt, VertexFlags.None, prev_v);
            _vertexList.Add(curr_v);
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
            AddLocMin(v0, polytype, true);
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
            AddLocMin(prev_v, polytype, isOpen);
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
            AddLocMin(prev_v, polytype, isOpen);
        }
        else if (going_up != going_up0)
        {
          if (going_up0) AddLocMin(prev_v, polytype, false);
          else prev_v.flags |= VertexFlags.LocalMax;
        }
      }
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
      AddPathsToVertexList(paths, polytype, isOpen);
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
          return (GetPolyType(ae) == PathType.Subject)? result : !result;

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
      if (InternalClipper.CrossProduct(PrevPrevVertex(resident).pt,
            resident.bot, resident.top) == 0) return true;
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
            if (!IsHorizontal(leftBound) && TestJoinWithPrev1(leftBound))
            {
              OutPt op = AddOutPt(leftBound.prevInAEL!, leftBound.bot);
              AddJoin(op, leftBound.outrec!.pts!);
            }
          }

          while (rightBound.nextInAEL != null &&
                 IsValidAelOrder(rightBound.nextInAEL, rightBound))
          {
            IntersectEdges(rightBound, rightBound.nextInAEL, rightBound.bot);
            SwapPositionsInAEL(rightBound, rightBound.nextInAEL);
          }

          if (!IsHorizontal(rightBound) && TestJoinWithNext1(rightBound))
          {
            OutPt op = AddOutPt(rightBound.nextInAEL!, rightBound.bot);
            AddJoin(rightBound.outrec!.pts!, op);
          }

          if (IsHorizontal(rightBound))
            PushHorz(rightBound);
          else
            InsertScanline(rightBound.top.Y);
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
    private static bool TestJoinWithPrev1(Active e)
    {
      // this is marginally quicker than TestJoinWithPrev2
      // but can only be used when e.PrevInAEL.currX is accurate
      return IsHotEdge(e) && !IsOpen(e) &&
             (e.prevInAEL != null) && (e.prevInAEL.curX == e.curX) &&
             IsHotEdge(e.prevInAEL) && !IsOpen(e.prevInAEL) &&
             (InternalClipper.CrossProduct(e.prevInAEL.top, e.bot, e.top) == 0);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool TestJoinWithPrev2(Active e, Point64 currPt)
    {
      return IsHotEdge(e) && !IsOpen(e) &&
             (e.prevInAEL != null) && !IsOpen(e.prevInAEL) &&
             IsHotEdge(e.prevInAEL) && (e.prevInAEL.top.Y < e.bot.Y) &&
             (Math.Abs(TopX(e.prevInAEL, currPt.Y) - currPt.X) < 2) &&
             (InternalClipper.CrossProduct(e.prevInAEL.top, currPt, e.top) == 0);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool TestJoinWithNext1(Active e)
    {
      // this is marginally quicker than TestJoinWithNext2
      // but can only be used when e.NextInAEL.currX is accurate
      return IsHotEdge(e) && !IsOpen(e) &&
             (e.nextInAEL != null) && (e.nextInAEL.curX == e.curX) &&
             IsHotEdge(e.nextInAEL) && !IsOpen(e.nextInAEL) &&
             (InternalClipper.CrossProduct(e.nextInAEL.top, e.bot, e.top) == 0);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool TestJoinWithNext2(Active e, Point64 currPt)
    {
      return IsHotEdge(e) && !IsOpen(e) &&
             (e.nextInAEL != null) && !IsOpen(e.nextInAEL) &&
             IsHotEdge(e.nextInAEL) && (e.nextInAEL.top.Y < e.bot.Y) &&
             (Math.Abs(TopX(e.nextInAEL, currPt.Y) - currPt.X) < 2) &&
             (InternalClipper.CrossProduct(e.nextInAEL.top, currPt, e.top) == 0);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private OutPt AddLocalMinPoly(Active ae1, Active ae2, Point64 pt, bool isNew = false)
    {
      OutRec outrec = new OutRec();
      _outrecList.Add(outrec);
      outrec.idx = _outrecList.Count - 1;
      outrec.pts = null;
      outrec.polypath = null;
      ae1.outrec = outrec;
      ae2.outrec = outrec;

      // Setting the owner and inner/outer states (above) is an essential
      // precursor to setting edge 'sides' (ie left and right sides of output
      // polygons) and hence the orientation of output paths ...

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
        UncoupleOutRec(ae1);
        if (!IsOpen(ae1))
          CleanCollinear(outrec);
        result = outrec.pts;

        outrec.owner = GetRealOutRec(outrec.owner);
        if (_using_polytree && outrec.owner is { frontEdge: null })
            outrec.owner = GetRealOutRec(outrec.owner.owner);
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

      // an owner must have a lower idx otherwise
      // it won't be a valid owner
      if (ae2.outrec.owner != null &&
        ae2.outrec.owner.idx < ae1.outrec.idx)
      {
        if (ae1.outrec.owner == null || ae2.outrec.owner.idx < ae1.outrec.owner.idx)
            ae1.outrec.owner = ae2.outrec.owner;
      }

      // after joining, the ae2.OutRec must contains no vertices ...
      ae2.outrec.frontEdge = null;
      ae2.outrec.backEdge = null;
      ae2.outrec.pts = null;
      ae2.outrec.owner = ae1.outrec; // this may be redundant

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
      OutPt newOp;

      // Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
      // opFront[.Prev]* ~~~> opBack & opBack == opFront.Next
      OutRec outrec = ae.outrec!;
      bool toFront = IsFront(ae);
      OutPt opFront = outrec.pts!;
      OutPt opBack = opFront.next!;

      if (toFront && (pt == opFront.pt)) newOp = opFront;
      else if (!toFront && (pt == opBack.pt)) newOp = opBack;
      else
      {
        newOp = new OutPt(pt, outrec);
        opBack.prev = newOp;
        newOp.prev = opFront;
        newOp.next = opBack;
        opFront.next = newOp;
        if (toFront) outrec.pts = newOp;
      }
      return newOp;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private OutPt StartOpenPath(Active ae, Point64 pt)
    {
      OutRec outrec = new OutRec();
      _outrecList.Add(outrec);
      outrec.idx = _outrecList.Count - 1;
      outrec.owner = null;
      outrec.isOpen = true;
      outrec.pts = null;
      outrec.polypath = null;
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
      if (IsHorizontal(ae)) return;
      InsertScanline(ae.top.Y);
      if (TestJoinWithPrev1(ae))
      {
        OutPt op1 = AddOutPt(ae.prevInAEL!, ae.bot);
        OutPt op2 = AddOutPt(ae, ae.bot);
        AddJoin(op1, op2);
      }
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


    private OutPt? IntersectEdges(Active ae1, Active ae2, Point64 pt)
    {
      OutPt? resultOp = null;

      // MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
      if (_hasOpenPaths && (IsOpen(ae1) || IsOpen(ae2)))
      {
        if (IsOpen(ae1) && IsOpen(ae2)) return null;
        // the following line avoids duplicating quite a bit of code
        if (IsOpen(ae2)) SwapActives(ref ae1, ref ae2);

        if (_cliptype == ClipType.Union)
        {
          if (!IsHotEdge(ae2)) return null;
        }
        else if (ae2.localMin.polytype == PathType.Subject) 
          return null;

        switch (_fillrule)
        {
          case FillRule.Positive:
            if (ae2.windCount != 1) return null; break;
          case FillRule.Negative:
            if (ae2.windCount != -1) return null; break;
          default:
            if (Math.Abs(ae2.windCount) != 1) return null; break;
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
            return ae3.outrec!.pts;
          }

          resultOp = StartOpenPath(ae1, pt);
        }
        else
          resultOp = StartOpenPath(ae1, pt);

#if USINGZ
        SetZ(ae1, ae2, ref resultOp.pt);
#endif
        return resultOp;
      }

      // MANAGING CLOSED PATHS FROM HERE ON

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

      if ((!IsHotEdge(ae1) && !e1WindCountIs0or1) || (!IsHotEdge(ae2) && !e2WindCountIs0or1)) return null;

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
          OutPt op2 = AddLocalMinPoly(ae1, ae2, pt);
#if USINGZ
          if (resultOp != null)
            SetZ(ae1, ae2, ref resultOp.pt);
          SetZ(ae1, ae2, ref op2.pt);
#endif
          if (resultOp != null && resultOp.pt == op2.pt &&
            !IsHorizontal(ae1) && !IsHorizontal(ae2) &&
            (InternalClipper.CrossProduct(ae1.bot, resultOp.pt, ae2.bot) == 0))
            AddJoin(resultOp, op2);
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
              if (e1Wc2 > 0 && e2Wc2 > 0) return null;
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
              if (e1Wc2 <= 0 || e2Wc2 <= 0) return null;
              resultOp = AddLocalMinPoly(ae1, ae2, pt);
              break;
          }
#if USINGZ
          if (resultOp != null) SetZ(ae1, ae2, ref resultOp.pt);
#endif
        }
      }

      return resultOp;
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
        ConvertHorzTrialsToJoins();
        _currentBotY = y; // bottom of scanbeam
        if (!PopScanline(out y))
          break; // y new top of scanbeam
        DoIntersections(y);
        DoTopOfScanbeam(y);
        while (PopHorz(out ae)) DoHorizontal(ae!);
      }

      if (_succeeded) ProcessJoinList();
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
      Point64 pt = GetIntersectPoint(ae1, ae2);

      // rounding errors can occasionally place the calculated intersection
      // point either below or above the scanbeam, so check and correct ...
      if (pt.Y > _currentBotY)
      {
        // ae.curr.y is still the bottom of scanbeam
        // use the more vertical of the 2 edges to derive pt.x ...
        if (Math.Abs(ae1.dx) < Math.Abs(ae2.dx))
          pt = new Point64(TopX(ae1, _currentBotY), _currentBotY);
        else
          pt = new Point64(TopX(ae2, _currentBotY), _currentBotY);
      }
      else if (pt.Y < topY)
      {
        // topY is at the top of the scanbeam
        if (ae1.top.Y == topY)
          pt = new Point64(ae1.top.X, topY);
        else if (ae2.top.Y == topY)
          pt = new Point64(ae2.top.X, topY);
        else if (Math.Abs(ae1.dx) < Math.Abs(ae2.dx))
          pt = new Point64(ae1.curX, topY);
        else
          pt = new Point64(ae2.curX, topY);
      }

      IntersectNode node = new IntersectNode(pt, ae1, ae2);
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

        if (TestJoinWithPrev2(node.edge2, node.pt))
        {
          OutPt op1 = AddOutPt(node.edge2.prevInAEL!, node.pt);
          OutPt op2 = AddOutPt(node.edge2, node.pt);
          if (op1 != op2) AddJoin(op1, op2);
        }
        else if (TestJoinWithNext2(node.edge1, node.pt))
        {
          OutPt op1 = AddOutPt(node.edge1, node.pt);
          OutPt op2 = AddOutPt(node.edge1.nextInAEL!, node.pt);
          if (op1 != op2) AddJoin(op1, op2);
        }
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
    private static bool ResetHorzDirection(Active horz, Active? maxPair,
        out long leftX, out long rightX)
    {
      if (horz.bot.X == horz.top.X)
      {
        // the horizontal edge is going nowhere ...
        leftX = horz.curX;
        rightX = horz.curX;
        Active? ae = horz.nextInAEL;
        while (ae != null && ae != maxPair) ae = ae.nextInAEL;
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
    private static bool HorzIsSpike(Active horz)
    {
      Point64 nextPt = NextVertex(horz).pt;
      return (horz.bot.X < horz.top.X) != (horz.top.X < nextPt.X);
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

      Vertex? vertex_max = null;
      Active? maxPair = null;

      if (!horzIsOpen)
      {
        vertex_max = GetCurrYMaximaVertex(horz);
        if (vertex_max != null)
        {
          maxPair = GetHorzMaximaPair(horz, vertex_max);
          // remove 180 deg.spikes and also simplify
          // consecutive horizontals when PreserveCollinear = true
          if (vertex_max != horz.vertexTop)
            TrimHorz(horz, PreserveCollinear);
        }
      }

      bool isLeftToRight =
        ResetHorzDirection(horz, maxPair, out long leftX, out long rightX);

      if (IsHotEdge(horz))
        AddOutPt(horz, new Point64(horz.curX, Y));

      OutPt? op;
      for (; ; )
      {
        if (horzIsOpen && IsMaxima(horz) && !IsOpenEnd(horz))
        {
          vertex_max = GetCurrYMaximaVertex(horz);
          if (vertex_max != null)
            maxPair = GetHorzMaximaPair(horz, vertex_max);
        }

        // loops through consec. horizontal edges (if open)
        Active? ae;
        if (isLeftToRight) ae = horz.nextInAEL;
        else ae = horz.prevInAEL;

        while (ae != null)
        {
          if (ae == maxPair)
          {
            if (IsHotEdge(horz))
            {
              while (horz.vertexTop != ae.vertexTop)
              {
                AddOutPt(horz, horz.top);
                UpdateEdgeIntoAEL(horz);
              }
              op = AddLocalMaxPoly(horz, ae, horz.top);
              if (op != null && !IsOpen(horz) && op.pt == horz.top)
                AddTrialHorzJoin(op);
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
              if (isLeftToRight)
              {
                // with open paths we'll only break once past horz's end
                if (IsOpen(ae) && !IsSamePolyType(ae, horz) && !IsHotEdge(ae))
                {
                  if (TopX(ae, pt.Y) > pt.X) break;
                }
                // otherwise we'll only break when horz's outslope is greater than e's
                else if (TopX(ae, pt.Y) >= pt.X) break;
              }
              else
              {
                // with open paths we'll only break once past horz's end
                if (IsOpen(ae) && !IsSamePolyType(ae, horz) && !IsHotEdge(ae))
                {
                  if (TopX(ae, pt.Y) < pt.X) break;
                }
                // otherwise we'll only break when horz's outslope is greater than e's
                else if (TopX(ae, pt.Y) <= pt.X) break;
              }
            }
          }

          pt = new Point64(ae.curX, Y);

          if (isLeftToRight)
          {
            op = IntersectEdges(horz, ae, pt);
            SwapPositionsInAEL(horz, ae);

            if (IsHotEdge(horz) && op != null && 
              !IsOpen(horz) && op.pt == pt) 
              AddTrialHorzJoin(op);

            if (!IsHorizontal(ae) && TestJoinWithPrev1(ae))
            {
              op = AddOutPt(ae.prevInAEL!, pt);
              OutPt op2 = AddOutPt(ae, pt);
              AddJoin(op, op2);
            }

            horz.curX = ae.curX;
            ae = horz.nextInAEL;
          }
          else
          {
            op = IntersectEdges(ae, horz, pt);
            SwapPositionsInAEL(ae, horz);

            if (IsHotEdge(horz) && op != null &&
              !IsOpen(horz) && op.pt == pt)
              AddTrialHorzJoin(op);

            if (!IsHorizontal(ae) && TestJoinWithNext1(ae))
            {
              op = AddOutPt(ae, pt);
              OutPt op2 = AddOutPt(ae.nextInAEL!, pt);
              AddJoin(op, op2);
            }

            horz.curX = ae.curX;
            ae = horz.prevInAEL;
          }
        } // we've reached the end of this horizontal

        // check if we've finished looping through consecutive horizontals
        if (horzIsOpen && IsOpenEnd(horz))
        {
          if (IsHotEdge(horz))
          {
            AddOutPt(horz, horz.top);
            if (IsFront(horz))
              horz.outrec!.frontEdge = null;
            else
              horz.outrec!.backEdge = null;
          }
          horz.outrec = null;
          DeleteFromAEL(horz); // ie open at top
          return;
        }

        if (NextVertex(horz).pt.Y != horz.top.Y) break;


        // there must be a following (consecutive) horizontal
        if (IsHotEdge(horz))
          AddOutPt(horz, horz.top);
        UpdateEdgeIntoAEL(horz);

        if (PreserveCollinear && HorzIsSpike(horz))
          TrimHorz(horz, true);

        isLeftToRight = ResetHorzDirection(horz, maxPair, out leftX, out rightX);

      } // end for loop and end of (possible consecutive) horizontals

      if (IsHotEdge(horz))
      {
        op = AddOutPt(horz, horz.top);
        if (!IsOpen(horz))
          AddTrialHorzJoin(op);
      }
      else
        op = null;

      if ((horzIsOpen && !IsOpenEnd(horz)) ||
        (!horzIsOpen && vertex_max != horz.vertexTop))
      {
        UpdateEdgeIntoAEL(horz); // this is the end of an intermediate horiz.
        if (IsOpen(horz)) return;

        if (isLeftToRight && TestJoinWithNext1(horz))
        {
          OutPt op2 = AddOutPt(horz.nextInAEL!, horz.bot);
          AddJoin(op!, op2);
        }
        else if (!isLeftToRight && TestJoinWithPrev1(horz))
        {
          OutPt op2 = AddOutPt(horz.prevInAEL!, horz.bot);
          AddJoin(op2, op!);
        }
      }
      else if (IsHotEdge(horz)) 
        AddLocalMaxPoly(horz, maxPair!, horz.top);
      else
      {
        DeleteFromAEL(maxPair!);
        DeleteFromAEL(horz);
      }
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
    private static bool IsValidPath(OutPt op)
    {
      return (op.next != op);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool AreReallyClose(Point64 pt1, Point64 pt2)
    {
      return (Math.Abs(pt1.X - pt2.X) < 2) && (Math.Abs(pt1.Y - pt2.Y) < 2);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsValidClosedPath(OutPt? op)
    {
      return (op != null && 
        op.next != op && op.next != op.prev &&
        // also treat inconsequential polygons as invalid
        !(op.next!.next == op.prev &&
        (AreReallyClose(op.pt, op.next.pt) ||
        AreReallyClose(op.pt, op.prev.pt))));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool ValueBetween(long val, long end1, long end2)
    {
      // NB accommodates axis aligned between where end1 == end2
      return ((val != end1) == (val != end2)) &&
        ((val > end1) == (val < end2));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool ValueEqualOrBetween(long val, long end1, long end2)
    {
      return (val == end1) || (val == end2) || ((val > end1) == (val < end2));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool PointEqualOrBetween(Point64 pt, Point64 corner1, Point64 corner2)
    {
      // NB points may not be collinear
      return
        ValueEqualOrBetween(pt.X, corner1.X, corner2.X) &&
        ValueEqualOrBetween(pt.Y, corner1.Y, corner2.Y);
    }

    private static bool PointBetween(Point64 pt, Point64 corner1, Point64 corner2)
    {
      // NB points may not be collinear
      return
        ValueBetween(pt.X, corner1.X, corner2.X) &&
        ValueBetween(pt.Y, corner1.Y, corner2.Y);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool CollinearSegsOverlap(Point64 seg1a, Point64 seg1b,
      Point64 seg2a, Point64 seg2b)
    {
      // precondition: seg1 and seg2 are collinear      
      if (seg1a.X == seg1b.X)
      {
        if (seg2a.X != seg1a.X || seg2a.X != seg2b.X) return false;
      }
      else if (seg1a.X < seg1b.X)
      {
        if (seg2a.X < seg2b.X)
        {
          if (seg2a.X >= seg1b.X || seg2b.X <= seg1a.X) return false;
        }
        else
        {
          if (seg2b.X >= seg1b.X || seg2a.X <= seg1a.X) return false;
        }
      }
      else
      {
        if (seg2a.X < seg2b.X)
        {
          if (seg2a.X >= seg1a.X || seg2b.X <= seg1b.X) return false;
        }
        else
        {
          if (seg2b.X >= seg1a.X || seg2a.X <= seg1b.X) return false;
        }
      }

      if (seg1a.Y == seg1b.Y)
      {
        if (seg2a.Y != seg1a.Y || seg2a.Y != seg2b.Y) return false;
      }
      else if (seg1a.Y < seg1b.Y)
      {
        if (seg2a.Y < seg2b.Y)
        {
          if (seg2a.Y >= seg1b.Y || seg2b.Y <= seg1a.Y) return false;
        }
        else
        {
          if (seg2b.Y >= seg1b.Y || seg2a.Y <= seg1a.Y) return false;
        }
      }
      else
      {
        if (seg2a.Y < seg2b.Y)
        {
          if (seg2a.Y >= seg1a.Y || seg2b.Y <= seg1b.Y) return false;
        }
        else
        {
          if (seg2b.Y >= seg1a.Y || seg2a.Y <= seg1b.Y) return false;
        }
      }
      return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool HorzEdgesOverlap(long x1a, long x1b, long x2a, long x2b)
    {
      const long minOverlap = 2;
      if (x1a > x1b + minOverlap)
      {
        if (x2a > x2b + minOverlap)
          return !((x1a <= x2b) || (x2a <= x1b));
        return !((x1a <= x2a) || (x2b <= x1b));
      }

      if (x1b > x1a + minOverlap)
      {
        if (x2a > x2b + minOverlap)
          return !((x1b <= x2b) || (x2a <= x1a));
        return !((x1b <= x2a) || (x2b <= x1a));
      }
      return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Joiner? GetHorzTrialParent(OutPt op)
    {
      Joiner? joiner = op.joiner;
      while (joiner != null)
      {
        if (joiner.op1 == op)
        {
          if (joiner.next1 != null &&
              joiner.next1.idx < 0) return joiner;
          joiner = joiner.next1;
        }
        else
        {
          if (joiner.next2 != null &&
              joiner.next2.idx < 0) return joiner;
          joiner = joiner.next1;
        }
      }
      return joiner;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool OutPtInTrialHorzList(OutPt op)
    {
      return op.joiner != null &&
       ((op.joiner.idx < 0) || GetHorzTrialParent(op) != null);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool ValidateClosedPathEx(ref OutPt? op)
    {
      if (IsValidClosedPath(op)) return true;
      if (op != null)
        SafeDisposeOutPts(ref op);
      return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static OutPt InsertOp(Point64 pt, OutPt insertAfter)
    {
      OutPt result = new OutPt(pt, insertAfter.outrec)
      { next = insertAfter.next };
      insertAfter.next!.prev = result;
      insertAfter.next = result;
      result.prev = insertAfter;
      return result;
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
    private void SafeDisposeOutPts(ref OutPt op)
    {
      OutRec? outRec = GetRealOutRec(op.outrec);
      if (outRec!.frontEdge != null)
        outRec.frontEdge.outrec = null;
      if (outRec.backEdge != null)
        outRec.backEdge.outrec = null;

      op.prev.next = null;
      OutPt? op2 = op;
      while (op2 != null)
      {
        SafeDeleteOutPtJoiners(op2);
        op2 = op2.next;
      }
      outRec.pts = null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void SafeDeleteOutPtJoiners(OutPt op)
    {
      Joiner? joiner = op.joiner;
      if (joiner == null) return;

      while (joiner != null)
      {
        if (joiner.idx < 0)
          DeleteTrialHorzJoin(op);
        else if (_horzJoiners != null)
        {
          if (OutPtInTrialHorzList(joiner.op1))
            DeleteTrialHorzJoin(joiner.op1);
          if (OutPtInTrialHorzList(joiner.op2!))
            DeleteTrialHorzJoin(joiner.op2!);
          DeleteJoin(joiner);
        }
        else
          DeleteJoin(joiner);
        joiner = op.joiner;
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void AddTrialHorzJoin(OutPt op)
    {
      // make sure 'op' isn't added more than once
      if (!op.outrec.isOpen && !OutPtInTrialHorzList(op))
        _horzJoiners = new Joiner(op, null, _horzJoiners);

    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Joiner? FindTrialJoinParent(ref Joiner joiner, OutPt op)
    {
      Joiner? parent = joiner;
      while (parent != null)
      {
        if (op == parent.op1)
        {
          if (parent.next1 != null && parent.next1.idx < 0)
          {
            joiner = parent.next1;
            return parent;
          }
          parent = parent.next1;
        }
        else
        {
          if (parent.next2 != null && parent.next2.idx < 0)
          {
            joiner = parent.next2;
            return parent;
          }
          parent = parent.next2;
        }
      }
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DeleteTrialHorzJoin(OutPt op)
    {
      if (_horzJoiners == null) return;

      Joiner? joiner = op.joiner;
      Joiner? parentH, parentOp = null;
      while (joiner != null)
      {
        if (joiner.idx < 0)
        {
          // first remove joiner from FHorzTrials
          if (joiner == _horzJoiners)
            _horzJoiners = joiner.nextH;
          else
          {
            parentH = _horzJoiners;
            while (parentH!.nextH != joiner)
              parentH = parentH.nextH;
            parentH.nextH = joiner.nextH;
          }

          // now remove joiner from op's joiner list
          if (parentOp == null)
          {
            // joiner must be first one in list
            op.joiner = joiner.next1;
            // joiner == null;
            joiner = op.joiner;
          }
          else
          {
            // the trial joiner isn't first
            if (op == parentOp.op1)
              parentOp.next1 = joiner.next1;
            else
              parentOp.next2 = joiner.next1;
            // joiner = null;
            joiner = parentOp;
          }
        }
        else
        {
          // not a trial join so look further along the linked list
          parentOp = FindTrialJoinParent(ref joiner, op);
          if (parentOp == null) break;
        }
        // loop in case there's more than one trial join
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool GetHorzExtendedHorzSeg(ref OutPt op, out OutPt op2)
    {
      OutRec outRec = GetRealOutRec(op.outrec)!;
      op2 = op;
      if (outRec.frontEdge != null)
      {
        while (op.prev != outRec.pts &&
          op.prev.pt.Y == op.pt.Y) op = op.prev;
        while (op2 != outRec.pts &&
          op2.next!.pt.Y == op2.pt.Y) op2 = op2.next;
        return op2 != op;
      }

      while (op.prev != op2 && op.prev.pt.Y == op.pt.Y)
        op = op.prev;
      while (op2.next != op && op2.next!.pt.Y == op2.pt.Y)
        op2 = op2.next;
      return op2 != op && op2.next != op;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void ConvertHorzTrialsToJoins()
    {
      while (_horzJoiners != null)
      {
        Joiner? joiner = _horzJoiners;
        _horzJoiners = _horzJoiners.nextH;
        OutPt op1a = joiner.op1;
        if (op1a.joiner == joiner)
        {
          op1a.joiner = joiner.next1;
        }
        else
        {
          Joiner joinerParent = FindJoinParent(joiner, op1a);
          if (joinerParent.op1 == op1a)
            joinerParent.next1 = joiner.next1;
          else
            joinerParent.next2 = joiner.next1;
        }
        // joiner = null;

        if (!GetHorzExtendedHorzSeg(ref op1a, out OutPt op1b))
        {
          if (op1a.outrec.frontEdge == null)
            CleanCollinear(op1a.outrec);
          continue;
        }

        OutPt op2a;
        bool joined = false;
        joiner = _horzJoiners;
        while (joiner != null)
        {
          op2a = joiner.op1;
          if (GetHorzExtendedHorzSeg(ref op2a, out OutPt op2b) &&
            HorzEdgesOverlap(op1a.pt.X, op1b.pt.X, op2a.pt.X, op2b.pt.X))
          {
            // overlap found so promote to a 'real' join
            joined = true;
            if (op1a.pt == op2b.pt)
              AddJoin(op1a, op2b);
            else if (op1b.pt == op2a.pt)
              AddJoin(op1b, op2a);
            else if (op1a.pt == op2a.pt)
              AddJoin(op1a, op2a);
            else if (op1b.pt == op2b.pt)
              AddJoin(op1b, op2b);
            else if (ValueBetween(op1a.pt.X, op2a.pt.X, op2b.pt.X))
              AddJoin(op1a, InsertOp(op1a.pt, op2a));
            else if (ValueBetween(op1b.pt.X, op2a.pt.X, op2b.pt.X))
              AddJoin(op1b, InsertOp(op1b.pt, op2a));
            else if (ValueBetween(op2a.pt.X, op1a.pt.X, op1b.pt.X))
              AddJoin(op2a, InsertOp(op2a.pt, op1a));
            else if (ValueBetween(op2b.pt.X, op1a.pt.X, op1b.pt.X))
              AddJoin(op2b, InsertOp(op2b.pt, op1a));
            break;
          }
          joiner = joiner.nextH;
        }
        if (!joined)
          CleanCollinear(op1a.outrec);
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void AddJoin(OutPt op1, OutPt op2)
    {
      if ((op1.outrec == op2.outrec) && ((op1 == op2) ||
        // unless op1.next or op1.prev crosses the start-end divide
        // don't waste time trying to join adjacent vertices
        ((op1.next == op2) && (op1 != op1.outrec.pts)) ||
        ((op2.next == op1) && (op2 != op1.outrec.pts)))) return;

      Joiner joiner = new Joiner(op1, op2, null) {idx = _joinerList.Count};
      _joinerList.Add(joiner);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Joiner FindJoinParent(Joiner joiner, OutPt op)
    {
      Joiner result = op.joiner!;
      for (; ; )
      {
        if (op == result.op1)
        {
          if (result.next1 == joiner) return result;
          result = result.next1!;
        }
        else
        {
          if (result.next2 == joiner) return result;
          result = result.next2!;
        }
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DeleteJoin(Joiner joiner)
    {
      // This method deletes a single join, and it doesn't check for or
      // delete trial horz. joins. For that, use the following method.
      OutPt op1 = joiner.op1, op2 = joiner.op2!;

      Joiner parentJnr;
      if (op1.joiner != joiner)
      {
        parentJnr = FindJoinParent(joiner, op1);
        if (parentJnr.op1 == op1)
          parentJnr.next1 = joiner.next1;
        else
          parentJnr.next2 = joiner.next1;
      }
      else
        op1.joiner = joiner.next1;

      if (op2.joiner != joiner)
      {
        parentJnr = FindJoinParent(joiner, op2);
        if (parentJnr.op1 == op2)
          parentJnr.next1 = joiner.next2;
        else
          parentJnr.next2 = joiner.next2;
      }
      else
        op2.joiner = joiner.next2;

      _joinerList[joiner.idx] = null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void ProcessJoinList()
    {
      // NB can't use foreach here because list may 
      // contain nulls which can't be enumerated
      for (int i = 0; i < _joinerList.Count; i++)
      {
        Joiner? j = _joinerList[i];
        if (j == null) continue;
        OutRec outrec = ProcessJoin(j);
        CleanCollinear(outrec);
      }
      _joinerList.Clear();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool CheckDisposeAdjacent(ref OutPt op, OutPt guard, OutRec outRec)
    {
      bool result = false;
      while (op.prev != op)
      {
        if (op.pt == op.prev.pt && op != guard &&
          op.prev.joiner != null && op.joiner == null)
        {
          if (op == outRec.pts) outRec.pts = op.prev;
          op = DisposeOutPt(op)!;
          op = op.prev;
        }
        else
          break;
      }

      while (op.next != op)
      {
        if (op.pt == op.next!.pt && op != guard &&
        op.next.joiner != null && op.joiner == null)
        {
          if (op == outRec.pts) outRec.pts = op.prev;
          op = DisposeOutPt(op)!;
          op = op.prev;
        }
        else
          break;
      }
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static double DistanceFromLineSqrd(Point64 pt, Point64 linePt1, Point64 linePt2)
    {
      // perpendicular distance of point (x0,y0) = (a*x0 + b*y0 + C)/Sqrt(a*a + b*b)
      // where ax + by +c = 0 is the equation of the line
      // see https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
      double a = (linePt1.Y - linePt2.Y);
      double b = (linePt2.X - linePt1.X);
      double c = a * linePt1.X + b * linePt1.Y;
      double q = a * pt.X + b * pt.Y - c;
      return (q * q) / (a * a + b * b);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static double DistanceSqr(Point64 pt1, Point64 pt2)
    {
      return (double) (pt1.X - pt2.X) * (pt1.X - pt2.X) +
             (double) (pt1.Y - pt2.Y) * (pt1.Y - pt2.Y);
    }

    private OutRec ProcessJoin(Joiner j)
    {
      OutPt op1 = j.op1, op2 = j.op2!;
      OutRec or1 = GetRealOutRec(op1.outrec)!;
      OutRec or2 = GetRealOutRec(op2.outrec)!;
      DeleteJoin(j);

      if (or2.pts == null) return or1;
      if (!IsValidClosedPath(op2))
      {
        SafeDisposeOutPts(ref op2);
        return or1;
      }
      if ((or1.pts == null) || !IsValidClosedPath(op1))
      {
        SafeDisposeOutPts(ref op1);
        return or2;
      }
      if (or1 == or2 &&
          ((op1 == op2) || (op1.next == op2) || (op1.prev == op2))) return or1;

      CheckDisposeAdjacent(ref op1, op2, or1);
      CheckDisposeAdjacent(ref op2, op1, or2);
      if (op1.next == op2 || op2.next == op1) return or1;

      OutRec result = or1;
      for (; ; )
      {
        if (!IsValidPath(op1) || !IsValidPath(op2) ||
          (or1 == or2 && (op1.prev == op2 || op1.next == op2))) return or1;

        if (op1.prev.pt == op2.next!.pt ||
            ((InternalClipper.CrossProduct(op1.prev.pt, op1.pt, op2.next.pt) == 0) &&
             CollinearSegsOverlap(op1.prev.pt, op1.pt, op2.pt, op2.next.pt)))
        {
          if (or1 == or2)
          {
            // SPLIT REQUIRED
            // make sure op1.prev and op2.next match positions
            // by inserting an extra vertex if needed
            if (op1.prev.pt != op2.next.pt)
            {
              if (PointEqualOrBetween(op1.prev.pt, op2.pt, op2.next.pt))
                op2.next = InsertOp(op1.prev.pt, op2);
              else
                op1.prev = InsertOp(op2.next.pt, op1.prev);
            }

            // current              to     new
            // op1.p[opA] >>> op1   ...    opA \   / op1
            // op2.n[opB] <<< op2   ...    opB /   \ op2
            OutPt opA = op1.prev, opB = op2.next;
            opA.next = opB;
            opB.prev = opA;
            op1.prev = op2;
            op2.next = op1;
            CompleteSplit(op1, opA, or1);
          }
          else
          {
            // JOIN, NOT SPLIT
            OutPt opA = op1.prev, opB = op2.next;
            opA.next = opB;
            opB.prev = opA;
            op1.prev = op2;
            op2.next = op1;

            //SafeDeleteOutPtJoiners(op2);
            //DisposeOutPt(op2);

            if (or1.idx < or2.idx)
            {
              or1.pts = op1;
              or2.pts = null;
              if (or1.owner != null &&
                (or2.owner == null || or2.owner.idx < or1.owner.idx))
              {
                or1.owner = or2.owner;
              }
              or2.owner = or1;
            }
            else
            {
              result = or2;
              or2.pts = op1;
              or1.pts = null;
              if (or2.owner != null &&
                (or1.owner == null || or1.owner.idx < or2.owner.idx))
              {
                or2.owner = or1.owner;
              }
              or1.owner = or2;
            }
          }
          break;
        }
        if (op1.next!.pt == op2.prev.pt ||
                 ((InternalClipper.CrossProduct(op1.next.pt, op2.pt, op2.prev.pt) == 0) &&
                  CollinearSegsOverlap(op1.next.pt, op1.pt, op2.pt, op2.prev.pt)))
        {
          if (or1 == or2) 
          {
            // SPLIT REQUIRED
            // make sure op2.prev and op1.next match positions
            // by inserting an extra vertex if needed
            if (op2.prev.pt != op1.next.pt)
            {
              if (PointEqualOrBetween(op2.prev.pt, op1.pt, op1.next.pt))
                op1.next = InsertOp(op2.prev.pt, op1);
              else
                op2.prev = InsertOp(op1.next.pt, op2.prev);
            }

            // current              to     new
            // op2.p[opA] >>> op2   ...    opA \   / op2
            // op1.n[opB] <<< op1   ...    opB /   \ op1
            OutPt opA = op2.prev, opB = op1.next;
            opA.next = opB;
            opB.prev = opA;
            op2.prev = op1;
            op1.next = op2;
            CompleteSplit(op1, opA, or1);
          }
          else
          {
            // JOIN, NOT SPLIT
            OutPt opA = op1.next, opB = op2.prev;
            opA.prev = opB;
            opB.next = opA;
            op1.next = op2;
            op2.prev = op1;

            //SafeDeleteOutPtJoiners(op2);
            //DisposeOutPt(op2);

            if (or1.idx < or2.idx)
            {
              or1.pts = op1;
              or2.pts = null;
              if (or1.owner != null &&
                (or2.owner == null || or2.owner.idx < or1.owner.idx))
              {
                or1.owner = or2.owner;
              }
              or2.owner = or1;
            }
            else
            {
              result = or2;
              or2.pts = op1;
              or1.pts = null;
              if (or2.owner != null &&
                (or1.owner == null || or1.owner.idx < or2.owner.idx))
              {
                or2.owner = or1.owner;
              }
              or1.owner = or2;
            }
          }
          break;
        }

        if (PointBetween(op1.next.pt, op2.pt, op2.prev.pt) &&
                 DistanceFromLineSqrd(op1.next.pt, op2.pt, op2.prev.pt) < 2.01)
        {
          InsertOp(op1.next.pt, op2.prev);
          continue;
        }
        if (PointBetween(op2.next.pt, op1.pt, op1.prev.pt) &&
                 DistanceFromLineSqrd(op2.next.pt, op1.pt, op1.prev.pt) < 2.01)
        {
          InsertOp(op2.next.pt, op1.prev);
          continue;
        }
        if (PointBetween(op1.prev.pt, op2.pt, op2.next.pt) &&
                 DistanceFromLineSqrd(op1.prev.pt, op2.pt, op2.next.pt) < 2.01)
        {
          InsertOp(op1.prev.pt, op2);
          continue;
        }
        if (PointBetween(op2.prev.pt, op1.pt, op1.next.pt) &&
                 DistanceFromLineSqrd(op2.prev.pt, op1.pt, op1.next.pt) < 2.01)
        {
          InsertOp(op2.prev.pt, op1);
          continue;
        }

        // something odd needs tidying up
        if (CheckDisposeAdjacent(ref op1, op2, or1)) continue;
        if (CheckDisposeAdjacent(ref op2, op1, or1)) continue;
        if (op1.prev.pt != op2.next!.pt &&
          (DistanceSqr(op1.prev.pt, op2.next.pt) < 2.01))
        {
          op1.prev.pt = op2.next.pt;
          continue;
        }
        if (op1.next!.pt != op2.prev.pt &&
          (DistanceSqr(op1.next.pt, op2.prev.pt) < 2.01))
        {
          op2.prev.pt = op1.next.pt;
          continue;
        }
        // OK, there doesn't seem to be a way to join after all
        // so just tidy up the polygons
        or1.pts = op1;
        if (or2 != or1)
        {
          or2.pts = op2;
          CleanCollinear(or2);
        }
        break;
      }
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void UpdateOutrecOwner(OutRec outrec)
    {
      OutPt opCurr = outrec.pts!;
      for (;;)
      {
        opCurr.outrec = outrec;
        opCurr = opCurr.next!;
        if (opCurr == outrec.pts) return;
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CompleteSplit(OutPt? op1, OutPt? op2, OutRec outrec)
    {
      double area1 = Area(op1!);
      double area2 = Area(op2!);
      bool signs_change = (area1 > 0) == (area2 < 0);

      // delete trivial splits (with zero or almost zero areas)
      if (area1 == 0 || (signs_change && Math.Abs(area1) < 2))
      {
        SafeDisposeOutPts(ref op1!);
        outrec.pts = op2;
      }
      else if (area2 == 0 || (signs_change && Math.Abs(area2) < 2))
      {
        SafeDisposeOutPts(ref op2!);
        outrec.pts = op1;
      }
      else
      {
        OutRec newOr = new OutRec() { idx = _outrecList.Count };
        _outrecList.Add(newOr);
        newOr.polypath = null;

        if (_using_polytree)
        {
          outrec.splits ??= new List<OutRec>();
          outrec.splits.Add(newOr);
        }

        if (Math.Abs(area1) >= Math.Abs(area2))
        {
          outrec.pts = op1;
          newOr.pts = op2;
        }
        else
        {
          outrec.pts = op2;
          newOr.pts = op1;
        }

        if ((area1 > 0) == (area2 > 0))
          newOr.owner = outrec.owner;
        else
          newOr.owner = outrec;

        UpdateOutrecOwner(newOr);
        CleanCollinear(newOr);
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CleanCollinear(OutRec? outrec)
    {
      outrec = GetRealOutRec(outrec);
      if (outrec == null || outrec.isOpen || 
        outrec.frontEdge != null || !ValidateClosedPathEx(ref outrec.pts)) 
          return;

      OutPt startOp = outrec.pts!;
      OutPt? op2 = startOp;
      for (; ; )
      {
        if (op2!.joiner != null) return;
        // NB if preserveCollinear == true, then only remove 180 deg. spikes
        if ((InternalClipper.CrossProduct(op2.prev.pt, op2.pt, op2.next!.pt) == 0) &&
          ((op2.pt == op2.prev.pt) || (op2.pt == op2.next.pt) || !PreserveCollinear ||
          (InternalClipper.DotProduct(op2.prev.pt, op2.pt, op2.next.pt) < 0)))
        {
          if (op2 == outrec.pts)
            outrec.pts = op2.prev;
          op2 = DisposeOutPt(op2);
          if (!ValidateClosedPathEx(ref op2))
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
      FixSelfIntersects(ref outrec.pts!);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private OutPt DoSplitOp(ref OutPt outRecOp, OutPt splitOp)
    {
      OutPt prevOp = splitOp.prev, nextNextOp = splitOp.next!.next!;
      OutPt result = prevOp;
      InternalClipper.GetIntersectPoint(
          prevOp.pt, splitOp.pt, splitOp.next.pt, nextNextOp.pt, out PointD ipD);
      Point64 ip = new Point64(ipD);

      double area1 = Area(outRecOp);
      double area2 = AreaTriangle(ip, splitOp.pt, splitOp.next.pt);

      if (ip == prevOp.pt || ip == nextNextOp.pt)
      {
        nextNextOp.prev = prevOp;
        prevOp.next = nextNextOp;
      }
      else
      {
        OutPt newOp2 = new OutPt(ip, prevOp.outrec) { prev = prevOp, next = nextNextOp };
        nextNextOp.prev = newOp2;
        prevOp.next = newOp2;
      }

      SafeDeleteOutPtJoiners(splitOp.next);
      SafeDeleteOutPtJoiners(splitOp);

      if ((Math.Abs(area2) >= 1) &&
          ((Math.Abs(area2) > Math.Abs(area1)) ||
           ((area2 > 0) == (area1 > 0))))
      {
        OutRec newOutRec = new OutRec()
        { idx = _outrecList.Count };
        _outrecList.Add(newOutRec);
        newOutRec.owner = prevOp.outrec.owner;
        newOutRec.polypath = null;
        splitOp.outrec = newOutRec;
        splitOp.next.outrec = newOutRec;

        OutPt newOp = new OutPt(ip, newOutRec) { prev = splitOp.next, next = splitOp };
        newOutRec.pts = newOp;
        splitOp.prev = newOp;
        splitOp.next.next = newOp;
      }
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void FixSelfIntersects(ref OutPt op)
    {
      if (!IsValidClosedPath(op)) return;
      OutPt op2 = op;
      for (; ; )
      {
        // triangles can't self-intersect
        if (op2.prev == op2.next!.next) break;
        if (InternalClipper.SegmentsIntersect(op2.prev.pt,
                op2.pt, op2.next.pt, op2.next.next!.pt))
        {
          if (op2 == op || op2.next == op) op = op2.prev;
          op2 = DoSplitOp(ref op, op2);
          op = op2;
          continue;
        }

        op2 = op2.next;

        if (op2 == op) break;
      }
    }

    internal static bool BuildPath(OutPt op, bool reverse, bool isOpen, Path64 path)
    {
      if (op.next == op || (!isOpen && op.next == op.prev)) return false;
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
      return true;
    }


    protected bool BuildPaths(Paths64 solutionClosed, Paths64 solutionOpen)
    {
      solutionClosed.Clear();
      solutionOpen.Clear();
      solutionClosed.Capacity = _outrecList.Count;
      solutionOpen.Capacity = _outrecList.Count;

      foreach (OutRec outrec in _outrecList)
      {
        if (outrec.pts == null) continue;

        Path64 path = new Path64();
        if (outrec.isOpen)
        {
          if (BuildPath(outrec.pts!, ReverseSolution, true, path))
              solutionOpen.Add(path);
        }
        else
        {
          // closed paths should always return a Positive orientation
          // except when ReverseSolution == true
          if (BuildPath(outrec.pts!, ReverseSolution, false, path))
            solutionClosed.Add(path);
        }
      }
      return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool Path1InsidePath2(OutRec or1, OutRec or2)
    {
      PointInPolygonResult result;
      OutPt op = or1.pts!;
      do
      {
        result = InternalClipper.PointInPolygon(op.pt, or2.path);
        if (result != PointInPolygonResult.IsOn) break;
        op = op.next!;
      } while (op != or1.pts);
      if (result == PointInPolygonResult.IsOn)
        return Area(op) < Area(or2.pts!);
      return result == PointInPolygonResult.IsInside;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Rect64 GetBounds(Path64 path)
	  {
		  if (path.Count == 0) return new Rect64();
      Rect64 result = new Rect64(long.MaxValue, long.MaxValue, -long.MaxValue, -long.MaxValue);
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
    private bool DeepCheckOwner(OutRec outrec, OutRec owner)
	  {
      if (owner.bounds.IsEmpty()) 
        owner.bounds = GetBounds(owner.path);
      bool isInsideOwnerBounds = owner.bounds.Contains(outrec.bounds);

      // while looking for the correct owner, check the owner's
      // splits **before** checking the owner itself because
      // splits can occur internally, and checking the owner
      // first would miss the inner split's true ownership
      if (owner.splits != null)
        foreach (OutRec asplit in owner.splits!)
        {
          OutRec? split = GetRealOutRec(asplit);
          if (split == null || split.idx <= owner.idx || split == outrec) continue;
          if (split.splits != null && DeepCheckOwner(outrec, split)) return true;

          if (split.path.Count == 0) 
            BuildPath(split.pts!, ReverseSolution, false, split.path);
          if (split.bounds.IsEmpty()) split.bounds = GetBounds(split.path);

          if (split.bounds.Contains(outrec.bounds) && Path1InsidePath2(outrec, split))
			    {
				    outrec.owner = split;
				    return true;
			    }
		    }

      // only continue when not inside recursion
      if (owner != outrec.owner) return false;

      for (;;)
      {
        if (isInsideOwnerBounds && Path1InsidePath2(outrec, outrec.owner!))
          return true;
        
        outrec.owner = outrec.owner!.owner;
        if (outrec.owner == null) return false;
        isInsideOwnerBounds = outrec.owner.bounds.Contains(outrec.bounds);
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    protected bool BuildTree(PolyPathBase polytree, Paths64 solutionOpen)
    {
      polytree.Clear();
      solutionOpen.Clear();
      solutionOpen.Capacity = _outrecList.Count;

      for (int i = 0; i < _outrecList.Count; i++)
      {
        OutRec outrec = _outrecList[i];
        if (outrec.pts == null) continue;

        if (outrec.isOpen)
        {
          Path64 open_path = new Path64();
          if (BuildPath(outrec.pts!, ReverseSolution, true, open_path))
            solutionOpen.Add(open_path);
          continue;
        }

        if (!BuildPath(outrec.pts!, ReverseSolution, false, outrec.path)) continue;
        if (outrec.bounds.IsEmpty()) outrec.bounds = GetBounds(outrec.path);
        outrec.owner = GetRealOutRec(outrec.owner);
        if (outrec.owner != null)
          DeepCheckOwner(outrec, outrec.owner);

        // swap order if outer/owner paths are preceeded by their inner paths
        if (outrec.owner != null && outrec.owner.idx > outrec.idx)
        {
          int j = outrec.owner.idx;
          outrec.owner.idx = i;
          outrec.idx = j;
          _outrecList[i] = _outrecList[j];
          _outrecList[j] = outrec;
          outrec = _outrecList[i];
          outrec.owner = GetRealOutRec(outrec.owner);
          BuildPath(outrec.pts!, ReverseSolution, false, outrec.path);
          if (outrec.bounds.IsEmpty()) outrec.bounds = GetBounds(outrec.path);
          if (outrec.owner != null)
            DeepCheckOwner(outrec, outrec.owner);
        }

        PolyPathBase ownerPP;
        if (outrec.owner != null && outrec.owner.polypath != null)
          ownerPP = outrec.owner.polypath;
        else
          ownerPP = polytree;
        outrec.polypath = ownerPP.AddChild(outrec.path);
      }
      return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Rect64 GetBounds()
    {
      Rect64 bounds = Clipper.MaxInvalidRect64;
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

      ClearSolution();
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

      ClearSolution();
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
        throw new ClipperLibException("Error - RoundingDecimalPrecision exceeds the allowed range.");
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
      PointD tmp = new PointD(intersectPt);
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

      ClearSolution();
      if (!success) return false;

      solutionClosed.Capacity = solClosed64.Count;
      foreach (Path64 path in solClosed64)
        solutionClosed.Add(Clipper.ScalePathD(path, _invScale));
      solutionOpen.Capacity = solOpen64.Count;
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
      (polytree as PolyPathD).Scale = _scale;
#if USINGZ
      CheckZCallback();
#endif
      openPaths.Clear();
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
      ClearSolution();
      if (!success) return false;
      if (oPaths.Count > 0)
      {
        openPaths.Capacity = oPaths.Count;        
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

    public PolyPathEnum GetEnumerator()
    {
      return new PolyPathEnum(_childs);
    }
    IEnumerator IEnumerable.GetEnumerator()
    {
      return GetEnumerator();
    }

    public bool IsHole => GetIsHole();

    public PolyPathBase(PolyPathBase? parent = null) { _parent = parent; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool GetIsHole()
    {
      bool result = true;
      PolyPathBase? pp = _parent;
      while (pp != null)
      {
        result = !result;
        pp = pp._parent;
      }

      return result;
    }

    public int Count => _childs.Count;

    internal abstract PolyPathBase AddChild(Path64 p);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Clear()
    {
      _childs.Clear();
    }
  } // PolyPathBase class

  public class PolyPathEnum : IEnumerator
  {
    public List<PolyPathBase> _ppbList;
    private int position = -1;
    public PolyPathEnum(List<PolyPathBase> childs)
    {
      _ppbList = childs;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public bool MoveNext()
    {
      position++;
      return (position < _ppbList.Count);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Reset()
    {
      position = -1;
    }

    public PolyPathBase Current
    {
      get
      {
        if (position < 0 || position >= _ppbList.Count)
          throw new InvalidOperationException();
        return _ppbList[position];
      }
    }

    object IEnumerator.Current => Current;

  }

  public class PolyPath64 : PolyPathBase
  {
    public Path64? Polygon { get; private set; } // polytree root's polygon == null

    public PolyPath64(PolyPathBase? parent = null) : base(parent) {}

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal override PolyPathBase AddChild(Path64 p)
    {
      PolyPathBase newChild = new PolyPath64(this);
      (newChild as PolyPath64)!.Polygon = p;
      _childs.Add(newChild);
      return newChild;
    }

    [IndexerName("Child")]
    public PolyPath64 this[int index]
    {
      get {
        if (index < 0 || index >= _childs.Count)
          throw new InvalidOperationException();
        return (PolyPath64) _childs[index]; 
      }
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
    internal override PolyPathBase AddChild(Path64 p)
    {
      PolyPathBase newChild = new PolyPathD(this);
      (newChild as PolyPathD)!.Scale = Scale;
      (newChild as PolyPathD)!.Polygon = Clipper.ScalePathD(p, 1 / Scale);
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