/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  5 January 2023                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* Purpose   :  Core structures and functions for the Clipper Library           *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#nullable enable
using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Threading;

namespace Clipper2Lib
{
  public struct Point64
  {
    public long X;
    public long Y;

#if USINGZ
    public long Z;

    public Point64(Point64 pt)
    {
      X = pt.X;
      Y = pt.Y;
      Z = pt.Z;
    }

    public Point64(Point64 pt, double scale)
    {
      X = (long) Math.Round(pt.X * scale);
      Y = (long) Math.Round(pt.Y * scale);
      Z = (long) Math.Round(pt.Z * scale);
    }
    
    public Point64(long x, long y, long z = 0)
    {
      X = x;
      Y = y;
      Z = z;
    }

    public Point64(double x, double y, double z = 0.0)
    {
      X = (long) Math.Round(x);
      Y = (long) Math.Round(y);
      Z = (long) Math.Round(z);
    }

    public Point64(PointD pt)
    {
      X = (long) Math.Round(pt.x);
      Y = (long) Math.Round(pt.y);
      Z = pt.z;
    }

    public Point64(PointD pt, double scale)
    {
      X = (long) Math.Round(pt.x * scale);
      Y = (long) Math.Round(pt.y * scale);
      Z = pt.z;
    }

    public static bool operator ==(Point64 lhs, Point64 rhs)
    {
      return lhs.X == rhs.X && lhs.Y == rhs.Y;
    }

    public static bool operator !=(Point64 lhs, Point64 rhs)
    {
      return lhs.X != rhs.X || lhs.Y != rhs.Y;
    }

    public static Point64 operator +(Point64 lhs, Point64 rhs)
    {
      return new Point64(lhs.X + rhs.X, lhs.Y + rhs.Y, lhs.Z + rhs.Z);
    }

    public static Point64 operator -(Point64 lhs, Point64 rhs)
    {
      return new Point64(lhs.X - rhs.X, lhs.Y - rhs.Y, lhs.Z - rhs.Z);
    }

    public override string ToString()
    {
      return $"{X},{Y},{Z} "; // nb: trailing space
    }

#else
    public Point64(Point64 pt)
    {
      X = pt.X;
      Y = pt.Y;
    }

    public Point64(long x, long y)
    {
      X = x;
      Y = y;
    }

    public Point64(double x, double y)
    {
      X = (long) Math.Round(x);
      Y = (long) Math.Round(y);
    }

    public Point64(PointD pt)
    {
      X = (long) Math.Round(pt.x);
      Y = (long) Math.Round(pt.y);
    }

    public Point64(Point64 pt, double scale)
    {
      X = (long) Math.Round(pt.X * scale);
      Y = (long) Math.Round(pt.Y * scale);
    }

    public Point64(PointD pt, double scale)
    {
      X = (long) Math.Round(pt.x * scale);
      Y = (long) Math.Round(pt.y * scale);
    }

    public static bool operator ==(Point64 lhs, Point64 rhs)
    {
      return lhs.X == rhs.X && lhs.Y == rhs.Y;
    }

    public static bool operator !=(Point64 lhs, Point64 rhs)
    {
      return lhs.X != rhs.X || lhs.Y != rhs.Y;
    }

    public static Point64 operator +(Point64 lhs, Point64 rhs)
    {
      return new Point64(lhs.X + rhs.X, lhs.Y + rhs.Y);
    }

    public static Point64 operator -(Point64 lhs, Point64 rhs)
    {
      return new Point64(lhs.X - rhs.X, lhs.Y - rhs.Y);
    }
    public override string ToString()
    {
      return $"{X},{Y} "; // nb: trailing space
    }

#endif
    public override bool Equals(object? obj)
    {
      if (obj != null && obj is Point64 p)
        return this == p;
      return false;
    }

    public override int GetHashCode() { return 0; }
  }

  public struct PointD
  {
    public double x;
    public double y;

#if USINGZ
    public long z;

    public PointD(PointD pt)
    {
      x = pt.x;
      y = pt.y;
      z = pt.z;
    }

    public PointD(Point64 pt)
    {
      x = pt.X;
      y = pt.Y;
      z = pt.Z;
    }

    public PointD(Point64 pt, double scale)
    {
      x = pt.X * scale;
      y = pt.Y * scale;
      z = pt.Z;
    }

    public PointD(PointD pt, double scale)
    {
      x = pt.x * scale;
      y = pt.y * scale;
      z = pt.z;
    }

    public PointD(long x, long y, long z = 0)
    {
      this.x = x;
      this.y = y;
      this.z = z;
    }

    public PointD(double x, double y, long z = 0)
    {
      this.x = x;
      this.y = y;
      this.z = z;
    }

    public override string ToString()
    {
      return $"{x:F},{y:F},{z} ";
    }

#else
    public PointD(PointD pt)
    {
      x = pt.x;
      y = pt.y;
    }

    public PointD(Point64 pt)
    {
      x = pt.X;
      y = pt.Y;
    }

    public PointD(PointD pt, double scale)
    {
      x = pt.x * scale;
      y = pt.y * scale;
    }

    public PointD(Point64 pt, double scale)
    {
      x = pt.X * scale;
      y = pt.Y * scale;
    }

    public PointD(long x, long y)
    {
      this.x = x;
      this.y = y;
    }

    public PointD(double x, double y)
    {
      this.x = x;
      this.y = y;
    }

    public override string ToString()
    {
      return $"{x:F},{y:F} ";
    }

#endif
    public static bool operator ==(PointD lhs, PointD rhs)
    {
      return InternalClipper.IsAlmostZero(lhs.x - rhs.x) && 
        InternalClipper.IsAlmostZero(lhs.y - rhs.y);
    }

    public static bool operator !=(PointD lhs, PointD rhs)
    {
      return !InternalClipper.IsAlmostZero(lhs.x - rhs.x) || 
        !InternalClipper.IsAlmostZero(lhs.y - rhs.y);
    }

    public override bool Equals(object? obj)
    {
      if (obj != null && obj is PointD p)
        return this == p;
      return false;
    }

    public void Negate() { x = -x; y = -y; }

    public override int GetHashCode() { return 0; }
  }

  public struct Rect64
  {
    public long left;
    public long top;
    public long right;
    public long bottom;

    public Rect64(long l, long t, long r, long b)
    {
      left = l;
      top = t;
      right = r;
      bottom = b;
    }

    public Rect64(Rect64 rec)
    {
      left = rec.left;
      top = rec.top;
      right = rec.right;
      bottom = rec.bottom;
    }

    public long Width
    {
      get => right - left;
      set => right = left + value;
    }

    public long Height
    {
      get => bottom - top;
      set => bottom = top + value;
    }

    public bool IsEmpty()
    {
      return bottom <= top || right <= left;
    }

    public Point64 MidPoint()
    {
      return new Point64((left + right) /2, (top + bottom)/2);
    }

    public bool Contains(Point64 pt)
    {
      return pt.X > left && pt.X < right &&
        pt.Y > top && pt.Y < bottom;
    }

    public bool Contains(Rect64 rec)
    {
      return rec.left >= left && rec.right <= right &&
        rec.top >= top && rec.bottom <= bottom;
    }

    public bool Intersects(Rect64 rec)
    {
      return (Math.Max(left, rec.left) < Math.Min(right, rec.right)) &&
        (Math.Max(top, rec.top) < Math.Min(bottom, rec.bottom));
    }

    public Path64 AsPath()
    {
      Path64 result = new Path64(4)
      {
        new Point64(left, top),
        new Point64(right, top),
        new Point64(right, bottom),
        new Point64(left, bottom)
      };
      return result;
    }

  }

  public struct RectD
  {
    public double left;
    public double top;
    public double right;
    public double bottom;

    public RectD(double l, double t, double r, double b)
    {
      left = l;
      top = t;
      right = r;
      bottom = b;
    }

    public RectD(RectD rec)
    {
      left = rec.left;
      top = rec.top;
      right = rec.right;
      bottom = rec.bottom;
    }

    public double Width
    {
      get => right - left;
      set => right = left + value;
    }

    public double Height
    {
      get => bottom - top;
      set => bottom = top + value;
    }

    public bool IsEmpty()
    {
      return bottom <= top || right <= left;
    }

    public PointD MidPoint()
    {
      return new PointD((left + right) / 2, (top + bottom) / 2);
    }

    public bool Contains(PointD pt)
    {
      return pt.x > left && pt.x < right &&
        pt.y > top && pt.y < bottom;
    }

    public bool Contains(RectD rec)
    {
      return rec.left >= left && rec.right <= right &&
        rec.top >= top && rec.bottom <= bottom;
    }

    public bool Intersects(RectD rec)
    {
      return (Math.Max(left, rec.left) < Math.Min(right, rec.right)) &&
        (Math.Max(top, rec.top) < Math.Min(bottom, rec.bottom));
    }

    public PathD AsPath()
    {
      PathD result = new PathD(4)
      {
        new PointD(left, top),
        new PointD(right, top),
        new PointD(right, bottom),
        new PointD(left, bottom)
      };
      return result;
    }

  }

  public class Path64 : List<Point64> 
  {
    private Path64() : base() { }
    public Path64(int capacity = 0) : base(capacity) { }
    public Path64(IEnumerable<Point64> path) : base(path) { }
    public override string ToString()
    {
      string s = "";
      foreach (Point64 p in this)
        s = s + p.ToString() + " ";
      return s;
    }
  }
  public class Paths64 : List<Path64>
  {
    private Paths64() : base() { }
    public Paths64(int capacity = 0) : base(capacity) { }
    public Paths64(IEnumerable<Path64> paths) : base(paths) { }
    public override string ToString()
    {
      string s = "";
      foreach (Path64 p in this)
        s = s + p.ToString() + "\n";
      return s;
    }
  }

  public class PathD : List<PointD>
  {
    private PathD() : base() { }
    public PathD(int capacity = 0) : base(capacity) { }
    public PathD(IEnumerable<PointD> path) : base(path) { }
    public override string ToString()
    {
      string s = "";
      foreach (PointD p in this)
        s = s + p.ToString() + " ";
      return s;
    }
  }

  public class PathsD : List<PathD>
  {
    private PathsD() : base() { }
    public PathsD(int capacity = 0) : base(capacity) { }
    public PathsD(IEnumerable<PathD> paths) : base(paths) { }
    public override string ToString()
    {
      string s = "";
      foreach (PathD p in this)
        s = s + p.ToString() + "\n";
      return s;
    }
  }

  // Note: all clipping operations except for Difference are commutative.
  public enum ClipType
  {
    None,
    Intersection,
    Union,
    Difference,
    Xor
  };

  public enum PathType
  {
    Subject,
    Clip
  };

  // By far the most widely used filling rules for polygons are EvenOdd
  // and NonZero, sometimes called Alternate and Winding respectively.
  // https://en.wikipedia.org/wiki/Nonzero-rule
  public enum FillRule
  {
    EvenOdd,
    NonZero,
    Positive,
    Negative
  };

  // PointInPolygon
  internal enum PipResult
  {
    Inside,
    Outside,
    OnEdge
  };

  public static class InternalClipper
  {
    internal const long MaxInt64 = 9223372036854775807;
    internal const long MaxCoord = MaxInt64 / 4;
    internal const double max_coord = MaxCoord;
    internal const double min_coord = -MaxCoord;
    internal const long Invalid64 = MaxInt64;

    internal const double floatingPointTolerance = 1E-12;
    internal const double defaultMinimumEdgeLength = 0.1;

    private static readonly string
      precision_range_error = "Error: Precision is out of range.";

    internal static void CheckPrecision(int precision)
    {
      if (precision < -8 || precision > 8)
        throw new Exception(precision_range_error);
    }

    internal static bool IsAlmostZero(double value)
    {
      return (Math.Abs(value) <= floatingPointTolerance);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static double CrossProduct(Point64 pt1, Point64 pt2, Point64 pt3)
    {
      // typecast to double to avoid potential int overflow
      return ((double) (pt2.X - pt1.X) * (pt3.Y - pt2.Y) -
              (double) (pt2.Y - pt1.Y) * (pt3.X - pt2.X));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static double DotProduct(Point64 pt1, Point64 pt2, Point64 pt3)
    {
      // typecast to double to avoid potential int overflow
      return ((double) (pt2.X - pt1.X) * (pt3.X - pt2.X) +
              (double) (pt2.Y - pt1.Y) * (pt3.Y - pt2.Y));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static double CrossProduct(PointD vec1, PointD vec2)
    {
      return (vec1.y * vec2.x - vec2.y * vec1.x);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static double DotProduct(PointD vec1, PointD vec2)
    {
      return (vec1.x * vec2.x + vec1.y * vec2.y);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static long CheckCastInt64(double val)
    {
      if ((val >= max_coord) || (val <= min_coord)) return Invalid64;
      return (long)Math.Round(val);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool GetIntersectPt(Point64 ln1a,
      Point64 ln1b, Point64 ln2a, Point64 ln2b, out Point64 ip)
    {
      double dy1 = (ln1b.Y - ln1a.Y);
      double dx1 = (ln1b.X - ln1a.X);
      double dy2 = (ln2b.Y - ln2a.Y);
      double dx2 = (ln2b.X - ln2a.X);
      double cp = dy1 * dx2 - dy2 * dx1;
      if (cp == 0.0)
      {
        ip = new Point64();
        return false;
      }
      double qx = dx1 * ln1a.Y - dy1 * ln1a.X;
      double qy = dx2 * ln2a.Y - dy2 * ln2a.X;
      ip = new Point64(
        CheckCastInt64((dx1 * qy - dx2 * qx) / cp),
        CheckCastInt64((dy1 * qy - dy2 * qx) / cp));
      return (ip.X != Invalid64 && ip.Y != Invalid64);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool GetIntersectPoint(Point64 ln1a,
      Point64 ln1b, Point64 ln2a, Point64 ln2b, out PointD ip)
    {
      double dy1 = (ln1b.Y - ln1a.Y);
      double dx1 = (ln1b.X - ln1a.X);
      double dy2 = (ln2b.Y - ln2a.Y);
      double dx2 = (ln2b.X - ln2a.X);
      double q1 = dy1 * ln1a.X - dx1 * ln1a.Y;
      double q2 = dy2 * ln2a.X - dx2 * ln2a.Y;
      double cross_prod = dy1 * dx2 - dy2 * dx1;
      if (cross_prod == 0.0)
      {
        ip = new PointD();
        return false;
      }
      ip = new PointD(
        (dx2 * q1 - dx1 * q2) / cross_prod,
        (dy2 * q1 - dy1 * q2) / cross_prod);
      return true;
    }
    internal static bool SegsIntersect(Point64 seg1a, 
      Point64 seg1b, Point64 seg2a, Point64 seg2b, bool inclusive = false)
    {
      if (inclusive)
      {
        double res1 = CrossProduct(seg1a, seg2a, seg2b);
        double res2 = CrossProduct(seg1b, seg2a, seg2b);
        if (res1 * res2 > 0) return false;
        double res3 = CrossProduct(seg2a, seg1a, seg1b);
        double res4 = CrossProduct(seg2b, seg1a, seg1b);
        if (res3 * res4 > 0) return false;
        // ensure NOT collinear
        return (res1 != 0 || res2 != 0 || res3 != 0 || res4 != 0);
      }
      else
      {
        return (CrossProduct(seg1a, seg2a, seg2b) * 
          CrossProduct(seg1b, seg2a, seg2b) < 0) &&
          (CrossProduct(seg2a, seg1a, seg1b) * 
          CrossProduct(seg2b, seg1a, seg1b) < 0);
      }
    }
    public static Point64 GetClosestPtOnSegment(Point64 offPt,
    Point64 seg1, Point64 seg2)
    {
      if (seg1.X == seg2.X && seg1.Y == seg2.Y) return seg1;
      double dx = (seg2.X - seg1.X);
      double dy = (seg2.Y - seg1.Y);
      double q = ((offPt.X - seg1.X) * dx +
        (offPt.Y - seg1.Y) * dy) / ((dx*dx) + (dy*dy));
      if (q < 0) q = 0; else if (q > 1) q = 1;
      return new Point64(
        seg1.X + Math.Round(q * dx), seg1.Y + Math.Round(q* dy));
    }

    public static PointInPolygonResult PointInPolygon(Point64 pt, Path64 polygon)
    {
      int len = polygon.Count, i = len - 1;

      if (len < 3) return PointInPolygonResult.IsOutside;

      while (i >= 0 && polygon[i].Y == pt.Y) --i;
      if (i < 0) return PointInPolygonResult.IsOutside;

      int val = 0;
      bool isAbove = polygon[i].Y < pt.Y;
      i = 0;

      while (i < len)
      {
        if (isAbove)
        {
          while (i < len && polygon[i].Y < pt.Y) i++;
          if (i == len) break;
        }
        else
        {
          while (i < len && polygon[i].Y > pt.Y) i++;
          if (i == len) break;
        }

        Point64 prev;

        Point64 curr = polygon[i];
        if (i > 0) prev = polygon[i - 1];
        else prev = polygon[len - 1];

        if (curr.Y == pt.Y)
        {
          if (curr.X == pt.X || (curr.Y == prev.Y &&
            ((pt.X < prev.X) != (pt.X < curr.X))))
            return PointInPolygonResult.IsOn;
          i++;
          continue;
        }

        if (pt.X < curr.X && pt.X < prev.X)
        {
          // we're only interested in edges crossing on the left
        }
        else if (pt.X > prev.X && pt.X > curr.X)
        {
          val = 1 - val; // toggle val
        }
        else
        {
          double d = CrossProduct(prev, curr, pt);
          if (d == 0) return PointInPolygonResult.IsOn;
          if ((d < 0) == isAbove) val = 1 - val;
        }
        isAbove = !isAbove;
        i++;
      }
      if (val == 0)
        return PointInPolygonResult.IsOutside;
      return PointInPolygonResult.IsInside;
    }

  } // InternalClipper

} // namespace