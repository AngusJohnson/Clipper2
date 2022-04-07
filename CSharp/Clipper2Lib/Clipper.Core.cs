/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (release candidate 1) - also known as Clipper2             *
* Date      :  6 April 2022                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Core structures and functions for the Clipper Library           *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;

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
      Z = (long) Math.Round(pt.z);
    }

    public static bool operator ==(Point64 lhs, Point64 rhs)
    {
      return lhs.X == rhs.X && lhs.Y == rhs.Y;
    }

    public static bool operator !=(Point64 lhs, Point64 rhs)
    {
      return lhs.X != rhs.X || lhs.Y != rhs.Y;
    }

#else
    public Point64(Point64 pt)
    {
      this.X = pt.X;
      this.Y = pt.Y;
    }

    public Point64(long x, long y)
    {
      this.X = x;
      this.Y = y;
    }

    public Point64(double x, double y)
    {
      this.X = (long) Math.Round(x);
      this.Y = (long) Math.Round(y);
    }

    public Point64(PointD pt)
    {
      this.X = (long) Math.Round(pt.x);
      this.Y = (long) Math.Round(pt.y);
    }

    public static bool operator ==(Point64 lhs, Point64 rhs)
    {
      return lhs.X == rhs.X && lhs.Y == rhs.Y;
    }

    public static bool operator !=(Point64 lhs, Point64 rhs)
    {
      return lhs.X != rhs.X || lhs.Y != rhs.Y;
    }

#endif
    public override bool Equals(object obj)
    {
      if (obj is Point64 p)
        return this == p;
      else
        return false;
    }

    public override string ToString()
    {
      return $"({X},{Y})";
    }

    public override int GetHashCode() => 0;
  }

  public struct PointD
  {
    public double x;
    public double y;

#if USINGZ
    public double z;

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

    public PointD(long x, long y, long z = 0)
    {
      this.x = x;
      this.y = y;
      this.z = z;
    }

    public PointD(double x, double y, double z = 0)
    {
      this.x = x;
      this.y = y;
      this.z = z;
    }

#else
    public PointD(PointD pt)
    {
      this.x = pt.x;
      this.y = pt.y;
    }

    public PointD(Point64 pt)
    {
      this.x = pt.X;
      this.y = pt.Y;
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

#endif

    private static bool IsAlmostZero(double value)
    {
      return (Math.Abs(value) <= 1E-15);
    }

    public static bool operator ==(PointD lhs, PointD rhs)
    {
      return IsAlmostZero(lhs.x - rhs.x) && IsAlmostZero(lhs.y - rhs.y);
    }

    public static bool operator !=(PointD lhs, PointD rhs)
    {
      return !IsAlmostZero(lhs.x - rhs.x) || !IsAlmostZero(lhs.y - rhs.y);
    }

    public override bool Equals(object obj)
    {
      if (obj is PointD p)
        return this == p;
      else
        return false;
    }

    public override string ToString()
    {
      return $"{x:F},{y:F} ";
    }

    public override int GetHashCode() => 0;
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
  }

  //Note: all clipping operations except for Difference are commutative.
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

  //By far the most widely used filling rules for polygons are EvenOdd
  //and NonZero, sometimes called Alternate and Winding respectively.
  //https://en.wikipedia.org/wiki/Nonzero-rule
  public enum FillRule
  {
    EvenOdd,
    NonZero,
    Positive,
    Negative
  };

  //PointInPolygon
  enum PipResult
  {
    Inside,
    Outside,
    OnEdge
  };

  enum OutRecState
  {
    Undefined,
    Open,
    Outer,
    Inner
  };

  public class InternalClipperFunc
  {
    public const double floatingPointTolerance = 1E-15;
    public const double defaultMinimumEdgeLength = 0.1;

    public static double CrossProduct(Point64 pt1, Point64 pt2, Point64 pt3)
    {
      //typecast to double to avoid potential int overflow
      return ((double) (pt2.X - pt1.X) * (pt3.Y - pt2.Y) -
              (double) (pt2.Y - pt1.Y) * (pt3.X - pt2.X));
    }

    public static double DotProduct(Point64 pt1, Point64 pt2, Point64 pt3)
    {
      //typecast to double to avoid potential int overflow
      return ((double) (pt2.X - pt1.X) * (pt3.X - pt2.X) +
              (double) (pt2.Y - pt1.Y) * (pt3.Y - pt2.Y));
    }

    public static double DotProduct(PointD vec1, PointD vec2)
    {
      return (vec1.x * vec2.x + vec1.y * vec2.y);
    }

    public static bool GetIntersectPoint(Point64 ln1a, Point64 ln1b, Point64 ln2a, Point64 ln2b, out PointD ip)
    {
      ip = new PointD();
      double m1, b1, m2, b2;
      if (ln1b.X == ln1a.X)
      {
        if (ln2b.X == ln2a.X) return false;
        m2 = (double) (ln2b.Y - ln2a.Y) / (ln2b.X - ln2a.X);
        b2 = ln2a.Y - m2 * ln2a.X;
        ip.x = ln1a.X;
        ip.y = m2 * ln1a.X + b2;
      }
      else if (ln2b.X == ln2a.X)
      {
        m1 = (double) (ln1b.Y - ln1a.Y) / (ln1b.X - ln1a.X);
        b1 = ln1a.Y - m1 * ln1a.X;
        ip.x = ln2a.X;
        ip.y = m1 * ln2a.X + b1;
      }
      else
      {
        m1 = (double) (ln1b.Y - ln1a.Y) / (ln1b.X - ln1a.X);
        b1 = ln1a.Y - m1 * ln1a.X;
        m2 = (double) (ln2b.Y - ln2a.Y) / (ln2b.X - ln2a.X);
        b2 = ln2a.Y - m2 * ln2a.X;
        if (Math.Abs(m1 - m2) > floatingPointTolerance)
        {
          ip.x = (double) (b2 - b1) / (m1 - m2);
          ip.y = m1 * ip.x + b1;
        }
        else
        {
          ip.x = (double) (ln1a.X + ln1b.X) / 2;
          ip.y = (double) (ln1a.Y + ln1b.Y) / 2;
        }
      }

      return true;
    }

    public static bool SegmentsIntersect(Point64 seg1a, Point64 seg1b, Point64 seg2a, Point64 seg2b)
    {
      double dx1 = seg1a.X - seg1b.X;
      double dy1 = seg1a.Y - seg1b.Y;
      double dx2 = seg2a.X - seg2b.X;
      double dy2 = seg2a.Y - seg2b.Y;
      return (((dy1 * (seg2a.X - seg1a.X) - dx1 * (seg2a.Y - seg1a.Y)) *
                  (dy1 * (seg2b.X - seg1a.X) - dx1 * (seg2b.Y - seg1a.Y)) < 0) &&
              ((dy2 * (seg1a.X - seg2a.X) - dx2 * (seg1a.Y - seg2a.Y)) *
                  (dy2 * (seg1b.X - seg2a.X) - dx2 * (seg1b.Y - seg2a.Y)) < 0));
    }
  } //InternalClipperFuncs
} //namespace