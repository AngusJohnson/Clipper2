/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  10 October 2024                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  Core structures and functions for the Clipper Library           *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Runtime.CompilerServices;

namespace Clipper2Lib;

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
    X = (long) Math.Round(pt.X * scale, MidpointRounding.AwayFromZero);
    Y = (long) Math.Round(pt.Y * scale, MidpointRounding.AwayFromZero);
    Z = (long) Math.Round(pt.Z * scale, MidpointRounding.AwayFromZero);
  }

  public Point64(long x, long y, long z = 0)
  {
    X = x;
    Y = y;
    Z = z;
  }

  public Point64(double x, double y, double z = 0.0)
  {
    X = (long) Math.Round(x, MidpointRounding.AwayFromZero);
    Y = (long) Math.Round(y, MidpointRounding.AwayFromZero);
    Z = (long) Math.Round(z, MidpointRounding.AwayFromZero);
  }

  public Point64(PointD pt)
  {
    X = (long) Math.Round(pt.X, MidpointRounding.AwayFromZero);
    Y = (long) Math.Round(pt.Y, MidpointRounding.AwayFromZero);
    Z = pt.Z;
  }

  public Point64(PointD pt, double scale)
  {
    X = (long) Math.Round(pt.X * scale, MidpointRounding.AwayFromZero);
    Y = (long) Math.Round(pt.Y * scale, MidpointRounding.AwayFromZero);
    Z = pt.Z;
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

  public readonly override string ToString()
  {
    return $"{X},{Y},{Z} "; // nb: trailing space
  }

#else

  public Point64(long x, long y)
  {
    X = x;
    Y = y;
  }

  public Point64(Point64 pt) : this(pt.X, pt.Y)
  {
  }

  public Point64(double x, double y)
  {
    X = (long) Math.Round(x, MidpointRounding.AwayFromZero);
    Y = (long) Math.Round(y, MidpointRounding.AwayFromZero);
  }

  public Point64(PointD pt) : this(pt.X, pt.Y)
  {
  }

  public Point64(Point64 pt, double scale) : this(pt.X * scale, pt.Y * scale)
  {
  }

  public Point64(PointD pt, double scale) : this(pt.X * scale, pt.Y * scale)
  {
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
  public readonly override string ToString()
  {
    return $"{X},{Y} "; // nb: trailing space
  }

#endif
  public readonly override bool Equals(object? obj)
  {
    if (obj != null && obj is Point64 p)
      return this == p;
    return false;
  }

  public readonly override int GetHashCode()
  {
    return HashCode.Combine(X, Y); //#599
  }
}

public struct PointD
{
  public double X;
  public double Y;

#if USINGZ
  public long Z;

  public PointD(PointD pt)
  {
    X = pt.X;
    Y = pt.Y;
    Z = pt.Z;
  }

  public PointD(Point64 pt)
  {
    X = pt.X;
    Y = pt.Y;
    Z = pt.Z;
  }

  public PointD(Point64 pt, double scale)
  {
    X = pt.X * scale;
    Y = pt.Y * scale;
    Z = pt.Z;
  }

  public PointD(PointD pt, double scale)
  {
    X = pt.X * scale;
    Y = pt.Y * scale;
    Z = pt.Z;
  }

  public PointD(long x, long y, long z = 0)
  {
    X = x;
    Y = y;
    Z = z;
  }

  public PointD(double x, double y, long z = 0)
  {
    X = x;
    Y = y;
    Z = z;
  }

  public readonly string ToString(int precision = 2)
  {
    return string.Format($"{{0:F{precision}}},{{1:F{precision}}},{{2:D}}", X, Y, Z);
  }

#else

  public PointD(double x, double y)
  {
    X = x;
    Y = y;
  }

  public PointD(PointD pt) : this(pt.X, pt.Y)
  {
  }

  public PointD(Point64 pt) : this(pt.X, pt.Y)
  {
  }

  public PointD(PointD pt, double scale) : this(pt.X * scale, pt.Y * scale)
  {
  }

  public PointD(Point64 pt, double scale) : this(pt.X * scale, pt.Y * scale)
  {
  }

  public readonly string ToString(int precision = 2)
  {
    return string.Format($"{{0:F{precision}}},{{1:F{precision}}}", X, Y);
  }

#endif
  public static bool operator ==(PointD lhs, PointD rhs)
  {
    return InternalClipper.IsAlmostZero(lhs.X - rhs.X) &&
      InternalClipper.IsAlmostZero(lhs.Y - rhs.Y);
  }

  public static bool operator !=(PointD lhs, PointD rhs)
  {
    return !InternalClipper.IsAlmostZero(lhs.X - rhs.X) ||
      !InternalClipper.IsAlmostZero(lhs.Y - rhs.Y);
  }

  public readonly override bool Equals(object? obj)
  {
    if (obj != null && obj is PointD p)
      return this == p;
    return false;
  }

  public void Negate() { X = -X; Y = -Y; }

  public readonly override int GetHashCode()
  {
    return HashCode.Combine(X, Y); //#599
  }
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

  public Rect64(bool isValid)
  {
    if (isValid)
    {
      left = 0; top = 0; right = 0; bottom = 0;
    }
    else
    {
      left = long.MaxValue; top = long.MaxValue;
      right = long.MinValue; bottom = long.MinValue;
    }
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
    readonly get => right - left;
    set => right = left + value;
  }

  public long Height
  {
    readonly get => bottom - top;
    set => bottom = top + value;
  }

  public readonly bool IsEmpty()
  {
    return bottom <= top || right <= left;
  }

  public readonly bool IsValid()
  {
    return left < long.MaxValue;
  }

  public readonly Point64 MidPoint()
  {
    return new Point64((left + right) / 2, (top + bottom) / 2);
  }

  public readonly bool Contains(Point64 pt)
  {
    return pt.X > left && pt.X < right &&
      pt.Y > top && pt.Y < bottom;
  }

  public readonly bool Contains(Rect64 rec)
  {
    return rec.left >= left && rec.right <= right &&
      rec.top >= top && rec.bottom <= bottom;
  }

  public readonly bool Intersects(Rect64 rec)
  {
    return (Math.Max(left, rec.left) <= Math.Min(right, rec.right)) &&
      (Math.Max(top, rec.top) <= Math.Min(bottom, rec.bottom));
  }

  public readonly Path64 AsPath()
  {
    return
    [
      new Point64(left, top),
      new Point64(right, top),
      new Point64(right, bottom),
      new Point64(left, bottom)
    ];
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

  public RectD(bool isValid)
  {
    if (isValid)
    {
      left = 0; top = 0; right = 0; bottom = 0;
    }
    else
    {
      left = double.MaxValue; top = double.MaxValue;
      right = -double.MaxValue; bottom = -double.MaxValue;
    }
  }
  public double Width
  {
    readonly get => right - left;
    set => right = left + value;
  }

  public double Height
  {
    readonly get => bottom - top;
    set => bottom = top + value;
  }

  public readonly bool IsEmpty()
  {
    return bottom <= top || right <= left;
  }

  public readonly PointD MidPoint()
  {
    return new PointD((left + right) / 2, (top + bottom) / 2);
  }

  public readonly bool Contains(PointD pt)
  {
    return pt.X > left && pt.X < right &&
      pt.Y > top && pt.Y < bottom;
  }

  public readonly bool Contains(RectD rec)
  {
    return rec.left >= left && rec.right <= right &&
      rec.top >= top && rec.bottom <= bottom;
  }

  public readonly bool Intersects(RectD rec)
  {
    return (Math.Max(left, rec.left) < Math.Min(right, rec.right)) &&
      (Math.Max(top, rec.top) < Math.Min(bottom, rec.bottom));
  }

  public readonly PathD AsPath()
  {
    return
    [
      new PointD(left, top),
      new PointD(right, top),
      new PointD(right, bottom),
      new PointD(left, bottom)
    ];
  }
}

public class Path64 : List<Point64>
{
  public Path64() : base() { }
  public Path64(int capacity = 0) : base(capacity) { }
  public Path64(IEnumerable<Point64> path) : base(path) { }
  public override string ToString()
  {
    var s = "";
    foreach (var p in this)
      s = s + p.ToString() + ", ";
    if (s != "") s = s.Remove(s.Length - 2);
    return s;
  }
}

public class Paths64 : List<Path64>
{
  public Paths64() : base() { }
  public Paths64(int capacity = 0) : base(capacity) { }
  public Paths64(IEnumerable<Path64> paths) : base(paths) { }
  public override string ToString()
  {
    var s = "";
    foreach (var p in this)
      s = s + p + "\n";
    return s;
  }
}

public class PathD : List<PointD>
{
  public PathD() : base() { }
  public PathD(int capacity = 0) : base(capacity) { }
  public PathD(IEnumerable<PointD> path) : base(path) { }
  public string ToString(int precision = 2)
  {
    var s = "";
    foreach (var p in this)
      s = s + p.ToString(precision) + ", ";
    if (s != "") s = s.Remove(s.Length - 2);
    return s;
  }
}

public class PathsD : List<PathD>
{
  public PathsD() : base() { }
  public PathsD(int capacity = 0) : base(capacity) { }
  public PathsD(IEnumerable<PathD> paths) : base(paths) { }
  public string ToString(int precision = 2)
  {
    var s = "";
    foreach (var p in this)
      s = s + p.ToString(precision) + "\n";
    return s;
  }
}

// Note: all clipping operations except for Difference are commutative.
public enum ClipType
{
  NoClip,
  Intersection,
  Union,
  Difference,
  Xor
}

public enum PathType
{
  Subject,
  Clip
}

// By far the most widely used filling rules for polygons are EvenOdd
// and NonZero, sometimes called Alternate and Winding respectively.
// https://en.wikipedia.org/wiki/Nonzero-rule
public enum FillRule
{
  EvenOdd,
  NonZero,
  Positive,
  Negative
}

// PointInPolygon
internal enum PipResult
{
  Inside,
  Outside,
  OnEdge
}

public static class InternalClipper
{
  internal const long MaxInt64 = 9223372036854775807;
  internal const long MaxCoord = MaxInt64 / 4;
  internal const double max_coord = MaxCoord;
  internal const double min_coord = -MaxCoord;
  internal const long Invalid64 = MaxInt64;

  internal const double defaultArcTolerance = 0.25;
  internal const double floatingPointTolerance = 1E-12;
  internal const double defaultMinimumEdgeLength = 0.1;

  private const string precision_range_error = "Error: Precision is out of range.";

  public static double CrossProduct(Point64 pt1, Point64 pt2, Point64 pt3)
  {
    // typecast to double to avoid potential int overflow
    return ((double) (pt2.X - pt1.X) * (pt3.Y - pt2.Y)) -
            ((double) (pt2.Y - pt1.Y) * (pt3.X - pt2.X));
  }

#if USINGZ
  public static Path64 SetZ(Path64 path, long Z)
  {
    var result = new Path64(path.Count);
    foreach (var pt in path) result.Add(new Point64(pt.X, pt.Y, Z));
    return result;
  }
#endif

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  internal static void CheckPrecision(int precision)
  {
    if (precision < -8 || precision > 8)
      throw new Exception(precision_range_error);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  internal static bool IsAlmostZero(double value)
  {
    return Math.Abs(value) <= floatingPointTolerance;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  internal static int TriSign(long x) // returns 0, 1 or -1
  {
    if (x < 0) return -1;
    return x > 1 ? 1 : 0;
  }

  public struct MultiplyUInt64Result
  {
    public ulong lo64;
    public ulong hi64;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static MultiplyUInt64Result MultiplyUInt64(ulong a, ulong b) // #834,#835
  {
    var x1 = (a & 0xFFFFFFFF) * (b & 0xFFFFFFFF);
    var x2 = ((a >> 32) * (b & 0xFFFFFFFF)) + (x1 >> 32);
    var x3 = ((a & 0xFFFFFFFF) * (b >> 32)) + (x2 & 0xFFFFFFFF);
    MultiplyUInt64Result result;
    result.lo64 = (x3 & 0xFFFFFFFF) << 32 | (x1 & 0xFFFFFFFF);
    result.hi64 = ((a >> 32) * (b >> 32)) + (x2 >> 32) + (x3 >> 32);
    return result;
  }

  // returns true if (and only if) a * b == c * d
  internal static bool ProductsAreEqual(long a, long b, long c, long d)
  {
    // nb: unsigned values will be needed for CalcOverflowCarry()
    var absA = (ulong) Math.Abs(a);
    var absB = (ulong) Math.Abs(b);
    var absC = (ulong) Math.Abs(c);
    var absD = (ulong) Math.Abs(d);

    var mul_ab = MultiplyUInt64(absA, absB);
    var mul_cd = MultiplyUInt64(absC, absD);

    // nb: it's important to differentiate 0 values here from other values
    var sign_ab = TriSign(a) * TriSign(b);
    var sign_cd = TriSign(c) * TriSign(d);

    return mul_ab.lo64 == mul_cd.lo64 && mul_ab.hi64 == mul_cd.hi64 && sign_ab == sign_cd;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  internal static bool IsCollinear(Point64 pt1, Point64 sharedPt, Point64 pt2)
  {
    var a = sharedPt.X - pt1.X;
    var b = pt2.Y - sharedPt.Y;
    var c = sharedPt.Y - pt1.Y;
    var d = pt2.X - sharedPt.X;
    // When checking for collinearity with very large coordinate values
    // then ProductsAreEqual is more accurate than using CrossProduct.
    return ProductsAreEqual(a, b, c, d);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  internal static double DotProduct(Point64 pt1, Point64 pt2, Point64 pt3)
  {
    // typecast to double to avoid potential int overflow
    return ((double) (pt2.X - pt1.X) * (pt3.X - pt2.X)) +
            ((double) (pt2.Y - pt1.Y) * (pt3.Y - pt2.Y));
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  internal static double CrossProduct(PointD vec1, PointD vec2)
  {
    return (vec1.Y * vec2.X) - (vec2.Y * vec1.X);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  internal static double DotProduct(PointD vec1, PointD vec2)
  {
    return (vec1.X * vec2.X) + (vec1.Y * vec2.Y);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  internal static long CheckCastInt64(double val)
  {
    if ((val >= max_coord) || (val <= min_coord)) return Invalid64;
    return (long) Math.Round(val, MidpointRounding.AwayFromZero);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static bool GetSegmentIntersectPt(Point64 ln1a,
    Point64 ln1b, Point64 ln2a, Point64 ln2b, out Point64 ip)
  {
    double dy1 = (ln1b.Y - ln1a.Y);
    double dx1 = (ln1b.X - ln1a.X);
    double dy2 = (ln2b.Y - ln2a.Y);
    double dx2 = (ln2b.X - ln2a.X);
    var det = (dy1 * dx2) - (dy2 * dx1);
    if (det == 0.0)
    {
      ip = new Point64();
      return false;
    }

    var t = (((ln1a.X - ln2a.X) * dy2) - ((ln1a.Y - ln2a.Y) * dx2)) / det;
    if (t <= 0.0) ip = ln1a;
    else if (t >= 1.0) ip = ln1b;
    else
    {
      // avoid using constructor (and rounding too) as they affect performance //664
      ip.X = (long) (ln1a.X + (t * dx1));
      ip.Y = (long) (ln1a.Y + (t * dy1));
#if USINGZ
      ip.Z = 0;
#endif
    }
    return true;
  }

  internal static bool SegsIntersect(Point64 seg1a,
    Point64 seg1b, Point64 seg2a, Point64 seg2b, bool inclusive = false)
  {
    if (!inclusive)
      return (CrossProduct(seg1a, seg2a, seg2b) *
               CrossProduct(seg1b, seg2a, seg2b) < 0) &&
             (CrossProduct(seg2a, seg1a, seg1b) *
               CrossProduct(seg2b, seg1a, seg1b) < 0);
    var res1 = CrossProduct(seg1a, seg2a, seg2b);
    var res2 = CrossProduct(seg1b, seg2a, seg2b);
    if (res1 * res2 > 0) return false;
    var res3 = CrossProduct(seg2a, seg1a, seg1b);
    var res4 = CrossProduct(seg2b, seg1a, seg1b);
    if (res3 * res4 > 0) return false;
    // ensure NOT collinear
    return res1 != 0 || res2 != 0 || res3 != 0 || res4 != 0;
  }
  public static Point64 GetClosestPtOnSegment(Point64 offPt,
  Point64 seg1, Point64 seg2)
  {
    if (seg1.X == seg2.X && seg1.Y == seg2.Y) return seg1;
    double dx = (seg2.X - seg1.X);
    double dy = (seg2.Y - seg1.Y);
    var q = (((offPt.X - seg1.X) * dx) +
      ((offPt.Y - seg1.Y) * dy)) / ((dx * dx) + (dy * dy));
    if (q < 0) q = 0; else if (q > 1) q = 1;
    return new Point64(
      // use MidpointRounding.ToEven in order to explicitly match the nearbyint behaviour on the C++ side
      seg1.X + Math.Round(q * dx, MidpointRounding.ToEven),
      seg1.Y + Math.Round(q * dy, MidpointRounding.ToEven)
    );
  }

  public static PointInPolygonResult PointInPolygon(Point64 pt, Path64 polygon)
  {
    int len = polygon.Count, start = 0;
    if (len < 3) return PointInPolygonResult.IsOutside;

    while (start < len && polygon[start].Y == pt.Y) start++;
    if (start == len) return PointInPolygonResult.IsOutside;

    double d;
    bool isAbove = polygon[start].Y < pt.Y, startingAbove = isAbove;
    int val = 0, i = start + 1, end = len;
    while (true)
    {
      if (i == end)
      {
        if (end == 0 || start == 0) break;
        end = start;
        i = 0;
      }

      if (isAbove)
      {
        while (i < end && polygon[i].Y < pt.Y) i++;
      }
      else
      {
        while (i < end && polygon[i].Y > pt.Y) i++;
      }

      if (i == end) continue;

      Point64 curr = polygon[i], prev;
      if (i > 0) prev = polygon[i - 1];
      else prev = polygon[len - 1];

      if (curr.Y == pt.Y)
      {
        if (curr.X == pt.X || (curr.Y == prev.Y &&
          ((pt.X < prev.X) != (pt.X < curr.X))))
          return PointInPolygonResult.IsOn;
        i++;
        if (i == start) break;
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
        d = CrossProduct(prev, curr, pt);
        if (d == 0) return PointInPolygonResult.IsOn;
        if ((d < 0) == isAbove) val = 1 - val;
      }
      isAbove = !isAbove;
      i++;
    }

    if (isAbove == startingAbove) return val == 0 ? PointInPolygonResult.IsOutside : PointInPolygonResult.IsInside;
    if (i == len) i = 0;
    d = i == 0 ? CrossProduct(polygon[len - 1], polygon[0], pt) : CrossProduct(polygon[i - 1], polygon[i], pt);
    if (d == 0) return PointInPolygonResult.IsOn;
    if ((d < 0) == isAbove) val = 1 - val;

    return val == 0 ? PointInPolygonResult.IsOutside : PointInPolygonResult.IsInside;
  }
} // InternalClipper
