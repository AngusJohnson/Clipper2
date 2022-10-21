/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  21 October 2022                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  FAST rectangular clipping                                       *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.NetworkInformation;

namespace Clipper2Lib
{
    public class RectClip
  {
    protected enum Location
    {
      left, top, right, bottom, inside
    };

    readonly protected Rect64 rect_;
    readonly protected Point64 mp_;
    readonly protected Path64 rectPath_;
    protected Path64 result_;
    protected Location firstCross_;
    protected List<Location> startLocs_ = new List<Location>();

    internal RectClip(Rect64 rect)
    {
      rect_ = rect;
      mp_ = rect.MidPoint();
      rectPath_ = rect_.AsPath();
      result_ = new Path64();
      firstCross_ = Location.inside;
    }

    private static PointInPolygonResult Path1ContainsPath2(Path64 path1, Path64 path2)
    {
      PointInPolygonResult result = PointInPolygonResult.IsOn;
      foreach (Point64 pt in path2)
      {
        result = Clipper.PointInPolygon(pt, path1);
        if (result != PointInPolygonResult.IsOn) break;
      }
      return result;
    }

    private static bool IsClockwise(Location prev, Location curr, 
      Point64 prevPt, Point64 currPt, Point64 rectMidPoint)
    {
      if (AreOpposites(prev, curr))
        return InternalClipper.CrossProduct(prevPt, rectMidPoint, currPt) < 0;
      else
        return HeadingClockwise(prev, curr);
    }

    private static bool AreOpposites(Location prev, Location curr)
    {
      return Math.Abs((int)prev - (int) curr) == 2;
    }

    private static bool HeadingClockwise(Location prev, Location curr)
    {
      return ((int) prev + 1) % 4 == (int) curr;
    }

    private static Location GetAdjacentLocation(Location loc, bool isClockwise)
    {
      int delta = (isClockwise) ? 1 : 3;
      return (Location)(((int) loc + delta) % 4);
    }

    private void AddCorner(Location prev, Location curr)
    {
      if (HeadingClockwise(prev, curr))
        result_.Add(rectPath_[(int) prev]);
      else
        result_.Add(rectPath_[(int) curr]);
    }

    private void AddCorner(ref Location loc, bool isClockwise)
    {
      if (isClockwise)
      {
        result_.Add(rectPath_[(int) loc]);
        loc = GetAdjacentLocation(loc, true);
      }
      else
      {
        loc = GetAdjacentLocation(loc, false);
        result_.Add(rectPath_[(int) loc]);
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

    static protected bool GetIntersection(Path64 rectPath, Point64 p, Point64 p2, ref Location loc, out Point64 ip)
    {
      // gets the pt of intersection between rectPath and segment(p, p2) that's closest to 'p'
      // when result == false, loc will remain unchanged
      ip = new Point64(); 
      switch (loc)
      {
        case Location.left:
          if (InternalClipper.SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[0], rectPath[3], out ip);
          }
          else if (p.Y < rectPath[0].Y &&
            InternalClipper.SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[0], rectPath[1], out ip);
            loc = Location.top;
          }
          else if (InternalClipper.SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[2], rectPath[3], out ip);
            loc = Location.bottom;
          }
          else return false;
          break;

        case Location.right:
          if (InternalClipper.SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[1], rectPath[2], out ip);
          }
          else if (p.Y < rectPath[0].Y &&
            InternalClipper.SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[0], rectPath[1], out ip);
            loc = Location.top;
          }
          else if (InternalClipper.SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[2], rectPath[3], out ip);
            loc = Location.bottom;
          }
          else return false;
          break;

        case Location.top:
          if (InternalClipper.SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[0], rectPath[1], out ip);
          }
          else if (p.X < rectPath[0].X &&
            InternalClipper.SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[0], rectPath[3], out ip);
            loc = Location.left;
          }
          else if (p.X > rectPath[1].X &&
            InternalClipper.SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[1], rectPath[2], out ip);
            loc = Location.right;
          }
          else return false;
          break;

        case Location.bottom:
          if (InternalClipper.SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[2], rectPath[3], out ip);
          }
          else if (p.X < rectPath[3].X &&
            InternalClipper.SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[0], rectPath[3], out ip);
            loc = Location.left;
          }
          else if (p.X > rectPath[2].X &&
            InternalClipper.SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[1], rectPath[2], out ip);
            loc = Location.right;
          }
          else return false;
          break;

        case Location.inside:
          if (InternalClipper.SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[0], rectPath[3], out ip);
            loc = Location.left;
          }
          else if (InternalClipper.SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[0], rectPath[1], out ip);
            loc = Location.top;
          }
          else if (InternalClipper.SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[1], rectPath[2], out ip);
            loc = Location.right;
          }
          else if (InternalClipper.SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true))
          {
            InternalClipper.GetIntersectPoint64(p, p2, rectPath[2], rectPath[3], out ip);
            loc = Location.bottom;
          }
          else return false;
          break;
      }
      return true;
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
                result_.Add(path[i]);
                i++;
                continue;
              }
              break;
            }
          }
          break;
      } // switch
    }

    internal Path64 ExecuteInternal(Path64 path)
    {
      if (path.Count < 3 || rect_.IsEmpty()) return new Path64();

      result_.Clear();
      startLocs_.Clear();
      int i = 0, highI = path.Count - 1;
      firstCross_ = Location.inside;
      Location crossingLoc = Location.inside, prev;
      if (!GetLocation(rect_, path[highI], out Location loc))
      {
        prev = loc;
        i = highI - 1;
        while (i >= 0 && !GetLocation(rect_, path[i], out prev)) i--;
        if (i < 0) return path;
        if (prev == Location.inside) loc = Location.inside;
        i = 0;
      }
      Location startingLoc = loc;

      ///////////////////////////////////////////////////
      while (i <= highI)
      {
        prev = loc;
        Location prevCrossLoc = crossingLoc;
        GetNextLocation(path, ref loc, ref i, highI);
        if (i > highI) break;

        Point64 prevPt = (i == 0) ? path[highI] : path[i - 1];
        crossingLoc = loc;
        if (!GetIntersection(rectPath_, path[i], prevPt, ref crossingLoc, out Point64 ip))
        {
          // ie remaining outside (& crossingLoc still == loc)

          if (prevCrossLoc == Location.inside)
          {
            bool isClockw = IsClockwise(prev, loc, prevPt, path[i], mp_);
            do
            {
              startLocs_.Add(prev);
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
          if (firstCross_ == Location.inside)
          {
            firstCross_ = crossingLoc;
            startLocs_.Add(prev);
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
          GetIntersection(rectPath_, prevPt, path[i], ref loc, out Point64 ip2);
          if (prevCrossLoc != Location.inside)
            AddCorner(prevCrossLoc, loc);

          if (firstCross_ == Location.inside)
          {
            firstCross_ = loc;
            startLocs_.Add(prev);
          }

          loc = crossingLoc;
          result_.Add(ip2);
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
          if (firstCross_ == Location.inside)
            firstCross_ = crossingLoc;
        }

        result_.Add(ip);
      } //while i <= highI
        ///////////////////////////////////////////////////

      // path must be entering rect
      if (firstCross_ == Location.inside)
      {
        if (startingLoc == Location.inside) return path;
        Rect64 tmp_rect = Clipper.GetBounds(path);
        if (tmp_rect.Contains(rect_) &&
          Path1ContainsPath2(path, rectPath_) 
            != PointInPolygonResult.IsOutside) return rectPath_;
        return new Path64();        
      }

      if (loc != Location.inside && 
        (loc != firstCross_ || startLocs_.Count > 2))
      {
        if (startLocs_.Count > 0)
        {
          prev = loc;
          foreach (Location loc2 in startLocs_)
          {
            if (prev == loc2) continue;
            AddCorner(ref prev, HeadingClockwise(prev, loc2));
            prev = loc2;
          }
          loc = prev;
        }
        if (loc != firstCross_)
          AddCorner(ref loc, HeadingClockwise(loc, firstCross_));
      }

      if (result_.Count < 3) return new Path64();

      // finally, tidy up result
      int k = 0, len = result_.Count;
      Point64 lastPt = result_[len -1];
      Path64 result = new Path64(len) { result_[0] };
      foreach (Point64 pt in result_.Skip(1))
      {
        if (InternalClipper.CrossProduct(lastPt, result[k], pt) != 0)
        {
          lastPt = result[k++];
          result.Add(pt);
        }
        else
          result[k] = pt;
      }

      if (k < 2) 
        result.Clear();
      else if (InternalClipper.CrossProduct(result[0], result[k - 1], result[k]) == 0)
        result.RemoveAt(result.Count - 1);
      return result;
    }

    internal Paths64 ExecuteInternal(Paths64 paths)
    {
      Paths64 result = new Paths64(paths.Count);
      foreach(Path64 path in paths) 
        if (rect_.Intersects(Clipper.GetBounds(path)))
          result.Add(ExecuteInternal(path)); 
      return result;
    }
  } // RectClip class

  public class RectClipLines : RectClip
  {
    internal RectClipLines(Rect64 rect) : base(rect) { }

    internal new Paths64 ExecuteInternal(Path64 path)
    {
      result_.Clear();
      Paths64 result = new Paths64();
      if (path.Count < 2 || rect_.IsEmpty()) return result;

      Location prev = Location.inside;
      int i = 1, highI = path.Count - 1;
      if (!GetLocation(rect_, path[0], out Location loc))
      {
        while (i <= highI && !GetLocation(rect_, path[i], out prev)) i++;
        if (i > highI)
        {
          result.Add(path);
          return result;
        }                   
        if (prev == Location.inside) loc = Location.inside;
        i = 1;
      }
      if (loc == Location.inside) result_.Add(path[0]);

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
          result_.Add(ip);
        }
        else if (prev != Location.inside)
        {
          // passing right through rect. 'ip' here will be the second 
          // intersect pt but we'll also need the first intersect pt (ip2)
          crossingLoc = prev;
          GetIntersection(rectPath_, prevPt, path[i], ref crossingLoc, out Point64 ip2);
          result_.Add(ip2);
          result_.Add(ip);
          result.Add(result_);
          result_ = new Path64();
        }
        else // path must be exiting rect
        {
          result_.Add(ip);
          result.Add(result_);
          result_ = new Path64();
        }
      } //while i <= highI
        ///////////////////////////////////////////////////

      if (result_.Count > 1)
      {
        result.Add(result_);
        result_ = new Path64();
      }
      return result;
    } // RectClipOpen.ExecuteInternal

  } // RectClipOpen class

} // namespace