/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  7 May 2024                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  FAST rectangular clipping                                       *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#nullable enable
using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Clipper2Lib
{
  public class OutPt2
  {
    public OutPt2? next;
    public OutPt2? prev;

    public Point64 pt;
    public int ownerIdx;
    public List<OutPt2?>? edge;
    public OutPt2(Point64 pt) 
    {
      this.pt = pt;
    }
  }

  public class RectClip64
  {
    protected enum Location
    {
      left, top, right, bottom, inside
    };

    readonly protected Rect64 rect_;
    readonly protected Point64 mp_;
    readonly protected Path64 rectPath_;
    protected Rect64 pathBounds_;
    protected List<OutPt2?> results_;
    protected List<OutPt2?>[] edges_;
    protected int currIdx_;
    internal RectClip64(Rect64 rect)
    {
      currIdx_ = -1;
      rect_ = rect;
      mp_ = rect.MidPoint();
      rectPath_ = rect_.AsPath();
      results_ = new List<OutPt2?>();
      edges_ = new List<OutPt2?>[8];
      for (int i = 0; i < 8; i++)
        edges_[i] = new List<OutPt2?>();
    }

    internal OutPt2 Add(Point64 pt, bool startingNewPath = false)
    {  // this method is only called by InternalExecute.
       // Later splitting and rejoining won't create additional op's,
       // though they will change the (non-storage) fResults count.
      int currIdx = results_.Count;
      OutPt2 result;
      if ((currIdx == 0) || startingNewPath)
      {
        result = new OutPt2(pt);
        results_.Add(result);
        result.ownerIdx = currIdx;
        result.prev = result;
        result.next = result;
      }
      else
      {
        currIdx--;
        OutPt2? prevOp = results_[currIdx];
        if (prevOp!.pt == pt) return prevOp;
        result = new OutPt2(pt)
        {
          ownerIdx = currIdx,
          next = prevOp.next
        };
        prevOp.next!.prev = result;
        prevOp.next = result;
        result.prev = prevOp;
        results_[currIdx] = result;
      }
      return result;
    }

    private static bool Path1ContainsPath2(Path64 path1, Path64 path2)
    {
      // nb: occasionally, due to rounding, path1 may 
      // appear (momentarily) inside or outside path2.
      int ioCount = 0;
      foreach (Point64 pt in path2)
      {
        PointInPolygonResult pip = 
          InternalClipper.PointInPolygon(pt, path1);
        switch(pip)
        {
          case PointInPolygonResult.IsInside:
            ioCount--; break;
          case PointInPolygonResult.IsOutside:
            ioCount++; break;
        }
        if (Math.Abs(ioCount) > 1) break;
      }
      return ioCount <= 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsClockwise(Location prev, Location curr, 
      Point64 prevPt, Point64 currPt, Point64 rectMidPoint)
    {
      if (AreOpposites(prev, curr))
        return InternalClipper.CrossProduct(prevPt, rectMidPoint, currPt) < 0;
      else
        return HeadingClockwise(prev, curr);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool AreOpposites(Location prev, Location curr)
    {
      return Math.Abs((int)prev - (int) curr) == 2;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool HeadingClockwise(Location prev, Location curr)
    {
      return ((int) prev + 1) % 4 == (int) curr;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Location GetAdjacentLocation(Location loc, bool isClockwise)
    {
      int delta = (isClockwise) ? 1 : 3;
      return (Location)(((int) loc + delta) % 4);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static OutPt2? UnlinkOp(OutPt2 op)
    {
      if (op.next == op) return null;
      op.prev!.next = op.next;
      op.next!.prev = op.prev;
      return op.next;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static OutPt2? UnlinkOpBack(OutPt2 op)
    {
      if (op.next == op) return null;
      op.prev!.next = op.next;
      op.next!.prev = op.prev;
      return op.prev;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static uint GetEdgesForPt(Point64 pt, Rect64 rec)
    {
      uint result = 0;
      if (pt.X == rec.left) result = 1;
      else if (pt.X == rec.right) result = 4;
      if (pt.Y == rec.top) result += 2;
      else if (pt.Y == rec.bottom) result += 8;
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsHeadingClockwise(Point64 pt1, Point64 pt2, int edgeIdx)
    {
      return edgeIdx switch
      {
        0 => pt2.Y < pt1.Y,
        1 => pt2.X > pt1.X,
        2 => pt2.Y > pt1.Y,
        _ => pt2.X < pt1.X,
      };
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool HasHorzOverlap(Point64 left1, Point64 right1,
      Point64 left2, Point64 right2)
    {
      return (left1.X < right2.X) && (right1.X > left2.X);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool HasVertOverlap(Point64 top1, Point64 bottom1,
      Point64 top2, Point64 bottom2)
    {
      return (top1.Y < bottom2.Y) && (bottom1.Y > top2.Y);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void AddToEdge(List<OutPt2?> edge, OutPt2 op)
    {
      if (op.edge != null) return;
      op.edge = edge;
      edge.Add(op);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void UncoupleEdge(OutPt2 op)
    {
      if (op.edge == null) return;
      for (int i = 0; i < op.edge.Count; i++)
      {
        OutPt2? op2 = op.edge[i];
        if (op2 == op)
        {
          op.edge[i] = null;
          break;
        }
      }
      op.edge = null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void SetNewOwner(OutPt2 op, int newIdx)
    {
      op.ownerIdx = newIdx;
      OutPt2 op2 = op.next!;
      while (op2 != op)
      {
        op2.ownerIdx = newIdx;
        op2 = op2.next!;
      }
    }

    private void AddCorner(Location prev, Location curr)
    {
      if (HeadingClockwise(prev, curr))
        Add(rectPath_[(int) prev]);
      else
        Add(rectPath_[(int) curr]);
    }

    private void AddCorner(ref Location loc, bool isClockwise)
    {
      if (isClockwise)
      {
        Add(rectPath_[(int) loc]);
        loc = GetAdjacentLocation(loc, true);
      }
      else
      {
        loc = GetAdjacentLocation(loc, false);
        Add(rectPath_[(int) loc]);
      }
    }

    static protected bool GetLocation(Rect64 rec, Point64 pt, out Location loc) 
    {
      if (pt.X == rec.left && pt.Y >= rec.top && pt.Y <= rec.bottom)
      {
        loc = Location.left; return false; // pt on rec
      }
      if (pt.X == rec.right && pt.Y >= rec.top && pt.Y <= rec.bottom)
      {
        loc = Location.right; return false; // pt on rec
      }
      if (pt.Y == rec.top && pt.X >= rec.left && pt.X <= rec.right)
      {
        loc = Location.top; return false; // pt on rec
      }
      if (pt.Y == rec.bottom && pt.X >= rec.left && pt.X <= rec.right)
      {
        loc = Location.bottom; return false; // pt on rec
      }
      if (pt.X < rec.left) loc = Location.left;
      else if (pt.X > rec.right) loc = Location.right;
      else if (pt.Y < rec.top)  loc = Location.top; 
      else if (pt.Y > rec.bottom) loc = Location.bottom;
      else loc = Location.inside;
      return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsHorizontal(Point64 pt1, Point64 pt2)
    {
      return pt1.Y == pt2.Y;
    }

    private static bool GetSegmentIntersection(Point64 p1,
    Point64 p2, Point64 p3, Point64 p4, out Point64 ip)
    {
      double res1 = InternalClipper.CrossProduct(p1, p3, p4);
      double res2 = InternalClipper.CrossProduct(p2, p3, p4);
      if (res1 == 0)
      {
        ip = p1;
        if (res2 == 0) return false; // segments are collinear
        else if (p1 == p3 || p1 == p4) return true;
        //else if (p2 == p3 || p2 == p4) { ip = p2; return true; }
        else if (IsHorizontal(p3, p4)) return ((p1.X > p3.X) == (p1.X < p4.X));
        else return ((p1.Y > p3.Y) == (p1.Y < p4.Y));
      }
      else if (res2 == 0)
      {
        ip = p2;
        if (p2 == p3 || p2 == p4) return true;
        else if (IsHorizontal(p3, p4)) return ((p2.X > p3.X) == (p2.X < p4.X));
        else return ((p2.Y > p3.Y) == (p2.Y < p4.Y));
      }

      if ((res1 > 0) == (res2 > 0))
      {
        ip = new Point64(0, 0);
        return false;
      }

      double res3 = InternalClipper.CrossProduct(p3, p1, p2);
      double res4 = InternalClipper.CrossProduct(p4, p1, p2);
      if (res3 == 0)
      {
        ip = p3;
        if (p3 == p1 || p3 == p2) return true;
        else if (IsHorizontal(p1, p2)) return ((p3.X > p1.X) == (p3.X < p2.X));
        else return ((p3.Y > p1.Y) == (p3.Y < p2.Y));
      }
      else if (res4 == 0)
      {
        ip = p4;
        if (p4 == p1 || p4 == p2) return true;
        else if (IsHorizontal(p1, p2)) return ((p4.X > p1.X) == (p4.X < p2.X));
        else return ((p4.Y > p1.Y) == (p4.Y < p2.Y));
      }
      if ((res3 > 0) == (res4 > 0)) 
      {
        ip = new Point64(0, 0);
        return false;
      }

      // segments must intersect to get here
      return InternalClipper.GetSegmentIntersectPt(p1, p2, p3, p4, out ip);
    }
  

    static protected bool GetIntersection(Path64 rectPath, Point64 p, Point64 p2, ref Location loc, out Point64 ip)
    {
      // gets the pt of intersection between rectPath and segment(p, p2) that's closest to 'p'
      // when result == false, loc will remain unchanged
      ip = new Point64(); 
      switch (loc)
      {
        case Location.left:
          if (GetSegmentIntersection(p, p2, rectPath[0], rectPath[3], out ip)) 
            return true;
          else if (p.Y < rectPath[0].Y && GetSegmentIntersection(p, p2, rectPath[0], rectPath[1], out ip))
          { 
            loc = Location.top;
            return true;
          }
          else if (GetSegmentIntersection(p, p2, rectPath[2], rectPath[3], out ip))
          { 
            loc = Location.bottom;
            return true;
          }
          else return false;

        case Location.right:
          if (GetSegmentIntersection(p, p2, rectPath[1], rectPath[2], out ip))
            return true;
          else if (p.Y < rectPath[0].Y && GetSegmentIntersection(p, p2, rectPath[0], rectPath[1], out ip))
          { 
            loc = Location.top;
            return true;
          }
          else if (GetSegmentIntersection(p, p2, rectPath[2], rectPath[3], out ip))
          { 
            loc = Location.bottom;
            return true;
          }
          else return false;

        case Location.top:
          if (GetSegmentIntersection(p, p2, rectPath[0], rectPath[1], out ip)) 
            return true;
          else if (p.X < rectPath[0].X && GetSegmentIntersection(p, p2, rectPath[0], rectPath[3], out ip))
          { 
            loc = Location.left;
            return true;
          }
          else if (p.X > rectPath[1].X && GetSegmentIntersection(p, p2, rectPath[1], rectPath[2], out ip))
          { 
            loc = Location.right;
            return true;
          }
          else return false;

        case Location.bottom:
          if (GetSegmentIntersection(p, p2, rectPath[2], rectPath[3], out ip)) 
            return true;
          else if (p.X < rectPath[3].X && GetSegmentIntersection(p, p2, rectPath[0], rectPath[3], out ip))
          {
            loc = Location.left;
            return true;
          }
          else if (p.X > rectPath[2].X && GetSegmentIntersection(p, p2, rectPath[1], rectPath[2], out ip))
          {
            loc = Location.right;
            return true;
          }
          else return false;

        default:
          if (GetSegmentIntersection(p, p2, rectPath[0], rectPath[3], out ip))
          {
            loc = Location.left;
            return true;
          }
          else if (GetSegmentIntersection(p, p2, rectPath[0], rectPath[1], out ip))
          {
            loc = Location.top;
            return true;
          }
          else if (GetSegmentIntersection(p, p2, rectPath[1], rectPath[2], out ip))
          {
            loc = Location.right;
            return true;
          }
          else if (GetSegmentIntersection(p, p2, rectPath[2], rectPath[3], out ip))
          {
            loc = Location.bottom;
            return true;
          }
          else return false;
      }
    }

    protected void GetNextLocation(Path64 path,
      ref Location loc, ref int i, int highI)
    {
      switch (loc)
      {
        case Location.left:
          {
            while (i <= highI && path[i].X <= rect_.left) i++;
            if (i > highI) break;
            if (path[i].X >= rect_.right) loc = Location.right;
            else if (path[i].Y <= rect_.top) loc = Location.top;
            else if (path[i].Y >= rect_.bottom) loc = Location.bottom;
            else loc = Location.inside;
          }
          break;

        case Location.top:
          {
            while (i <= highI && path[i].Y <= rect_.top) i++;
            if (i > highI) break;
            if (path[i].Y >= rect_.bottom) loc = Location.bottom;
            else if (path[i].X <= rect_.left) loc = Location.left;
            else if (path[i].X >= rect_.right) loc = Location.right;
            else loc = Location.inside;
          }
          break;

        case Location.right:
          {
            while (i <= highI && path[i].X >= rect_.right) i++;
            if (i > highI) break;
            if (path[i].X <= rect_.left) loc = Location.left;
            else if (path[i].Y <= rect_.top) loc = Location.top;
            else if (path[i].Y >= rect_.bottom) loc = Location.bottom;
            else loc = Location.inside;
          }
          break;

        case Location.bottom:
          {
            while (i <= highI && path[i].Y >= rect_.bottom) i++;
            if (i > highI) break;
            if (path[i].Y <= rect_.top) loc = Location.top;
            else if (path[i].X <= rect_.left) loc = Location.left;
            else if (path[i].X >= rect_.right) loc = Location.right;
            else loc = Location.inside;
          }
          break;

        case Location.inside:
          {
            while (i <= highI)
            {
              if (path[i].X < rect_.left) loc = Location.left;
              else if (path[i].X > rect_.right) loc = Location.right;
              else if (path[i].Y > rect_.bottom) loc = Location.bottom;
              else if (path[i].Y < rect_.top) loc = Location.top;
              else
              {
                Add(path[i]);
                i++;
                continue;
              }
              break;
            }
          }
          break;
      } // switch
    }

    private void ExecuteInternal(Path64 path)
    {
      if (path.Count < 3 || rect_.IsEmpty()) return;      
      List<Location> startLocs = new List<Location>();
      
      Location firstCross = Location.inside;
      Location crossingLoc = firstCross, prev = firstCross;

      int i, highI = path.Count - 1;
      if (!GetLocation(rect_, path[highI], out Location loc))
      {
        i = highI - 1;
        while (i >= 0 && !GetLocation(rect_, path[i], out prev)) i--;
        if (i < 0)
        {
          foreach(Point64 pt in path) { Add(pt); }
          return;
        }
        if (prev == Location.inside) loc = Location.inside;
      }
      Location startingLoc = loc;

      ///////////////////////////////////////////////////
      i = 0;
      while (i <= highI)
      {
        prev = loc;
        Location prevCrossLoc = crossingLoc;
        GetNextLocation(path, ref loc, ref i, highI);
        if (i > highI) break;

        Point64 prevPt = (i == 0) ? path[highI] : path[i - 1];
        crossingLoc = loc;
        if (!GetIntersection(rectPath_, 
          path[i], prevPt, ref crossingLoc, out Point64 ip))
        {
          // ie remaining outside
          if (prevCrossLoc == Location.inside)
          {
            bool isClockw = IsClockwise(prev, loc, prevPt, path[i], mp_);
            do
            {
              startLocs.Add(prev);
              prev = GetAdjacentLocation(prev, isClockw);
            } while (prev != loc);
            crossingLoc = prevCrossLoc; // still not crossed 
          }

          else if (prev != Location.inside && prev != loc)
          {
            bool isClockw = IsClockwise(prev, loc, prevPt, path[i], mp_);
            do
            {
              AddCorner(ref prev, isClockw);
            } while (prev != loc);
          }
          ++i;
          continue;
        }

        ////////////////////////////////////////////////////
        // we must be crossing the rect boundary to get here
        ////////////////////////////////////////////////////

        if (loc == Location.inside) // path must be entering rect
        {
          if (firstCross == Location.inside)
          {
            firstCross = crossingLoc;
            startLocs.Add(prev);
          }
          else if (prev != crossingLoc)
          {
            bool isClockw = IsClockwise(prev, crossingLoc, prevPt, path[i], mp_);
            do
            {
              AddCorner(ref prev, isClockw);
            } while (prev != crossingLoc);
          }
        }
        else if (prev != Location.inside)
        {
          // passing right through rect. 'ip' here will be the second 
          // intersect pt but we'll also need the first intersect pt (ip2)
          loc = prev;
          GetIntersection(rectPath_, 
            prevPt, path[i], ref loc, out Point64 ip2);
          if (prevCrossLoc != Location.inside && prevCrossLoc != loc) //#597
            AddCorner(prevCrossLoc, loc);

          if (firstCross == Location.inside)
          {
            firstCross = loc;
            startLocs.Add(prev);
          }

          loc = crossingLoc;
          Add(ip2);
          if (ip == ip2)
          {
            // it's very likely that path[i] is on rect
            GetLocation(rect_, path[i], out loc);
            AddCorner(crossingLoc, loc);
            crossingLoc = loc;
            continue;
          }
        }
        else // path must be exiting rect
        {
          loc = crossingLoc;
          if (firstCross == Location.inside)
            firstCross = crossingLoc;
        }

        Add(ip);
      } //while i <= highI
      ///////////////////////////////////////////////////

      if (firstCross == Location.inside)
      {
        // path never intersects
        if (startingLoc != Location.inside)
        {
          if (pathBounds_.Contains(rect_) &&
            Path1ContainsPath2(path, rectPath_))
          {
            for (int j = 0; j < 4; j++)
            {
              Add(rectPath_[j]);
              AddToEdge(edges_[j * 2], results_[0]!);
            }
          }
        }
      }
      else if (loc != Location.inside && 
        (loc != firstCross || startLocs.Count > 2))
      {
        if (startLocs.Count > 0)
        {
          prev = loc;
          foreach (Location loc2 in startLocs)
          {
            if (prev == loc2) continue;
            AddCorner(ref prev, HeadingClockwise(prev, loc2));
            prev = loc2;
          }
          loc = prev;
        }
        if (loc != firstCross)
          AddCorner(ref loc, HeadingClockwise(loc, firstCross));
      }
    }

    public Paths64 Execute(Paths64 paths)
    {
      Paths64 result = new Paths64();
      if (rect_.IsEmpty()) return result;
      foreach (Path64 path in paths)
      {
        if (path.Count < 3) continue;
        pathBounds_ = Clipper.GetBounds(path);
        if (!rect_.Intersects(pathBounds_))
          continue; // the path must be completely outside fRect
        else if (rect_.Contains(pathBounds_))
        {
          // the path must be completely inside rect_
          result.Add(path);
          continue;
        }
        ExecuteInternal(path);
        CheckEdges();
        for (int i = 0; i < 4; ++i)
          TidyEdgePair(i, edges_[i * 2], edges_[i * 2 + 1]);

        foreach (OutPt2? op in results_)
        {
          Path64 tmp = GetPath(op);
          if (tmp.Count > 0) result.Add(tmp);
        }

        //clean up after every loop
        results_.Clear();
        for (int i = 0; i < 8; i++)
          edges_[i].Clear();
      }
      return result;
    }

    private void CheckEdges()
    {
      for (int i = 0; i < results_.Count; i++)
      {
        OutPt2? op = results_[i], op2 = op;
        if (op == null) continue;
        do
        {
          if (InternalClipper.IsCollinear(
            op2!.prev!.pt, op2.pt, op2.next!.pt))
          {
            if (op2 == op)
            {
              op2 = UnlinkOpBack(op2);
              if (op2 == null) break;
              op = op2.prev;
            }
            else
            {
              op2 = UnlinkOpBack(op2);
              if (op2 == null) break;
            }
          }
          else
            op2 = op2.next;
        } while (op2 != op);

        if (op2 == null)
        {
          results_[i] = null;
          continue;
        }
        results_[i] = op2; // safety first

        uint edgeSet1 = GetEdgesForPt(op!.prev!.pt, rect_);
        op2 = op;
        do
        {
          uint edgeSet2 = GetEdgesForPt(op2!.pt, rect_);
          if (edgeSet2 != 0 && op2.edge == null)
          {
            uint combinedSet = (edgeSet1 & edgeSet2);
            for (int j = 0; j < 4; ++j)
            {
              if ((combinedSet & (1 << j)) != 0)
              {
                if (IsHeadingClockwise(op2.prev!.pt, op2.pt, j))
                  AddToEdge(edges_[j * 2], op2);
                else
                  AddToEdge(edges_[j * 2 + 1], op2);
              }
            }
          }
          edgeSet1 = edgeSet2;
          op2 = op2.next;
        } while (op2 != op);
      }
    }

    private void TidyEdgePair(int idx, List<OutPt2?> cw, List<OutPt2?> ccw)
    {
      if (ccw.Count == 0) return;
      bool isHorz = ((idx == 1) || (idx == 3));
      bool cwIsTowardLarger = ((idx == 1) || (idx == 2));
      int i = 0, j = 0;
      OutPt2? p1, p2, p1a, p2a, op, op2;

      while (i < cw.Count)
      {
        p1 = cw[i];
        if (p1 == null || p1.next == p1.prev)
        {
          cw[i++] = null;
          j = 0;
          continue;
        }

        int jLim = ccw.Count;
        while (j < jLim &&
          (ccw[j] == null || ccw[j]!.next == ccw[j]!.prev)) ++j;

        if (j == jLim)
        {
          ++i;
          j = 0;
          continue;
        }

        if (cwIsTowardLarger)
        {
          // p1 >>>> p1a;
          // p2 <<<< p2a;
          p1 = cw[i]!.prev!;
          p1a = cw[i];
          p2 = ccw[j];
          p2a = ccw[j]!.prev!;
        }
        else
        {
          // p1 <<<< p1a;
          // p2 >>>> p2a;
          p1 = cw[i];
          p1a = cw[i]!.prev!;
          p2 = ccw[j]!.prev!;
          p2a = ccw[j];
        }

        if ((isHorz && !HasHorzOverlap(p1!.pt, p1a!.pt, p2!.pt, p2a!.pt)) ||
          (!isHorz && !HasVertOverlap(p1!.pt, p1a!.pt, p2!.pt, p2a!.pt)))
        {
          ++j;
          continue;
        }

        // to get here we're either splitting or rejoining
        bool isRejoining = cw[i]!.ownerIdx != ccw[j]!.ownerIdx;

        if (isRejoining)
        {
          results_[p2!.ownerIdx] = null;
          SetNewOwner(p2, p1!.ownerIdx);
        }

        // do the split or re-join
        if (cwIsTowardLarger)
        {
          // p1 >> | >> p1a;
          // p2 << | << p2a;
          p1!.next = p2;
          p2!.prev = p1;
          p1a!.prev = p2a;
          p2a!.next = p1a;
        }
        else
        {
          // p1 << | << p1a;
          // p2 >> | >> p2a;
          p1!.prev = p2;
          p2!.next = p1;
          p1a!.next = p2a;
          p2a!.prev = p1a;
        }

        if (!isRejoining)
        {
          int new_idx = results_.Count;
          results_.Add(p1a);
          SetNewOwner(p1a, new_idx);
        }

        if (cwIsTowardLarger)
        {
          op = p2;
          op2 = p1a;
        }
        else
        {
          op = p1;
          op2 = p2a;
        }
        results_[op.ownerIdx] = op;
        results_[op2.ownerIdx] = op2;

        // and now lots of work to get ready for the next loop

        bool opIsLarger, op2IsLarger;
        if (isHorz) // X
        {
          opIsLarger = op.pt.X > op.prev!.pt.X;
          op2IsLarger = op2.pt.X > op2.prev!.pt.X;
        }
        else       // Y
        {
          opIsLarger = op.pt.Y > op.prev!.pt.Y;
          op2IsLarger = op2.pt.Y > op2.prev!.pt.Y;
        }

        if ((op.next == op.prev) ||
          (op.pt == op.prev.pt))
        {
          if (op2IsLarger == cwIsTowardLarger)
          {
            cw[i] = op2;
            ccw[j++] = null;
          }
          else
          {
            ccw[j] = op2;
            cw[i++] = null;
          }
        }
        else if ((op2.next == op2.prev) ||
          (op2.pt == op2.prev.pt))
        {
          if (opIsLarger == cwIsTowardLarger)
          {
            cw[i] = op;
            ccw[j++] = null;
          }
          else
          {
            ccw[j] = op;
            cw[i++] = null;
          }
        }
        else if (opIsLarger == op2IsLarger)
        {
          if (opIsLarger == cwIsTowardLarger)
          {
            cw[i] = op;
            UncoupleEdge(op2);
            AddToEdge(cw, op2);
            ccw[j++] = null;
          }
          else
          {
            cw[i++] = null;
            ccw[j] = op2;
            UncoupleEdge(op);
            AddToEdge(ccw, op);
            j = 0;
          }
        }
        else
        {
          if (opIsLarger == cwIsTowardLarger)
            cw[i] = op;
          else
            ccw[j] = op;
          if (op2IsLarger == cwIsTowardLarger)
            cw[i] = op2;
          else
            ccw[j] = op2;
        }
      }
    }

    private Path64 GetPath(OutPt2? op)
    { 
      Path64 result = new Path64();
      if (op == null || op.prev == op.next) return result;
      OutPt2? op2 = op.next;
      while (op2 != null && op2 != op)
      {
        if (InternalClipper.IsCollinear(
          op2.prev!.pt, op2.pt, op2.next!.pt))
        {
          op = op2.prev;
          op2 = UnlinkOp(op2);
        }
        else
          op2 = op2.next;
      }
      if (op2 == null) return new Path64();

      result.Add(op.pt);
      op2 = op.next;
      while (op2 != op)
      {
        result.Add(op2!.pt);
        op2 = op2.next;
      }
      return result;
    }

  } // RectClip class

  public class RectClipLines64 : RectClip64
  {
    internal RectClipLines64(Rect64 rect) : base(rect) { }

    public new Paths64 Execute(Paths64 paths)
    {
      Paths64 result = new Paths64();
      if (rect_.IsEmpty()) return result;
      foreach (Path64 path in paths)
      {
        if (path.Count < 2) continue;
        pathBounds_ = Clipper.GetBounds(path);
        if (!rect_.Intersects(pathBounds_))
          continue; // the path must be completely outside fRect
        // Apart from that, we can't be sure whether the path
        // is completely outside or completed inside or intersects
        // fRect, simply by comparing path bounds with fRect.
        ExecuteInternal(path);

        foreach (OutPt2? op in results_)
        {
          Path64 tmp = GetPath(op);
          if (tmp.Count > 0) result.Add(tmp);
        }

        //clean up after every loop
        results_.Clear();
        for(int i = 0; i < 8; i++)
          edges_[i].Clear();
      }
      return result;
    }

    private Path64 GetPath(OutPt2? op)
    {
      Path64 result = new Path64();
      if (op == null || op == op.next) return result;
      op = op.next; // starting at path beginning 
      result.Add(op!.pt);
      OutPt2 op2 = op.next!;
      while (op2 != op)
      {
        result.Add(op2.pt);
        op2 = op2.next!;
      }
      return result;
    }

    private void ExecuteInternal(Path64 path)
    {
      results_.Clear();
      if (path.Count < 2 || rect_.IsEmpty()) return;

      Location prev = Location.inside;
      int i = 1, highI = path.Count - 1;
      if (!GetLocation(rect_, path[0], out Location loc))
      {
        while (i <= highI && !GetLocation(rect_, path[i], out prev)) i++;
        if (i > highI)
        {
          foreach (Point64 pt in path) Add(pt);
          return;
        }                   
        if (prev == Location.inside) loc = Location.inside;
        i = 1;
      }
      if (loc == Location.inside) Add(path[0]);

      ///////////////////////////////////////////////////
      while (i <= highI)
      {
        prev = loc;
        GetNextLocation(path, ref loc, ref i, highI);
        if (i > highI) break;
        Point64 prevPt = path[i - 1];

        Location crossingLoc = loc;
        if (!GetIntersection(rectPath_, path[i], prevPt, ref crossingLoc, out Point64 ip))
        {
          // ie remaining outside (& crossingLoc still == loc)
          ++i;
          continue;
        }

        ////////////////////////////////////////////////////
        // we must be crossing the rect boundary to get here
        ////////////////////////////////////////////////////

        if (loc == Location.inside) // path must be entering rect
        {
          Add(ip, true);
        }
        else if (prev != Location.inside)
        {
          // passing right through rect. 'ip' here will be the second 
          // intersect pt but we'll also need the first intersect pt (ip2)
          crossingLoc = prev;
          GetIntersection(rectPath_, prevPt, path[i], ref crossingLoc, out Point64 ip2);
          Add(ip2, true);
          Add(ip);
        }
        else // path must be exiting rect
        {
          Add(ip);
        }
      } //while i <= highI
      ///////////////////////////////////////////////////      
    } // RectClipLines.ExecuteInternal

  } // RectClipLines class

} // namespace