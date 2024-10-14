/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  10 October 2024                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  This module contains simple functions that will likely cover    *
*              most polygon boolean and offsetting needs, while also avoiding  *
*              the inherent complexities of the other modules.                 *
* Thanks    :  Special thanks to Thong Nguyen, Guus Kuiper, Phil Stopford,     *
*           :  and Daniel Gosnell for their invaluable assistance with C#.     *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Runtime.CompilerServices;

namespace Clipper2Lib;

// PRE-COMPILER CONDITIONAL ...
// USINGZ: For user defined Z-coordinates. See Clipper.SetZ

public static class Clipper
{
  private static Rect64 invalidRect64 = new Rect64(false);
  public static Rect64 InvalidRect64 => invalidRect64;

  private static RectD invalidRectD = new RectD(false);
  public static RectD InvalidRectD => invalidRectD;

  public static Paths64 Intersect(Paths64 subject, Paths64 clip, FillRule fillRule)
  {
    return BooleanOp(ClipType.Intersection, subject, clip, fillRule);
  }

  public static PathsD Intersect(PathsD subject, PathsD clip,
    FillRule fillRule, int precision = 2)
  {
    return BooleanOp(ClipType.Intersection,
      subject, clip, fillRule, precision);
  }

  public static Paths64 Union(Paths64 subject, FillRule fillRule)
  {
    return BooleanOp(ClipType.Union, subject, null, fillRule);
  }

  public static Paths64 Union(Paths64 subject, Paths64 clip, FillRule fillRule)
  {
    return BooleanOp(ClipType.Union, subject, clip, fillRule);
  }

  public static PathsD Union(PathsD subject, FillRule fillRule)
  {
    return BooleanOp(ClipType.Union, subject, null, fillRule);
  }

  public static PathsD Union(PathsD subject, PathsD clip,
    FillRule fillRule, int precision = 2)
  {
    return BooleanOp(ClipType.Union,
      subject, clip, fillRule, precision);
  }

  public static Paths64 Difference(Paths64 subject, Paths64 clip, FillRule fillRule)
  {
    return BooleanOp(ClipType.Difference, subject, clip, fillRule);
  }

  public static PathsD Difference(PathsD subject, PathsD clip,
    FillRule fillRule, int precision = 2)
  {
    return BooleanOp(ClipType.Difference,
      subject, clip, fillRule, precision);
  }

  public static Paths64 Xor(Paths64 subject, Paths64 clip, FillRule fillRule)
  {
    return BooleanOp(ClipType.Xor, subject, clip, fillRule);
  }

  public static PathsD Xor(PathsD subject, PathsD clip,
    FillRule fillRule, int precision = 2)
  {
    return BooleanOp(ClipType.Xor,
      subject, clip, fillRule, precision);
  }

  public static Paths64 BooleanOp(ClipType clipType,
    Paths64? subject, Paths64? clip, FillRule fillRule)
  {
    var solution = new Paths64();
    if (subject == null) return solution;
    var c = new Clipper64();
    c.AddPaths(subject, PathType.Subject);
    if (clip != null)
      c.AddPaths(clip, PathType.Clip);
    c.Execute(clipType, fillRule, solution);
    return solution;
  }

  public static void BooleanOp(ClipType clipType,
    Paths64? subject, Paths64? clip,
    PolyTree64 polytree, FillRule fillRule)
  {
    if (subject == null) return;
    var c = new Clipper64();
    c.AddPaths(subject, PathType.Subject);
    if (clip != null)
      c.AddPaths(clip, PathType.Clip);
    c.Execute(clipType, fillRule, polytree);
  }

  public static PathsD BooleanOp(ClipType clipType, PathsD subject, PathsD? clip,
    FillRule fillRule, int precision = 2)
  {
    var solution = new PathsD();
    var c = new ClipperD(precision);
    c.AddSubject(subject);
    if (clip != null)
      c.AddClip(clip);
    c.Execute(clipType, fillRule, solution);
    return solution;
  }

  public static void BooleanOp(ClipType clipType,
    PathsD? subject, PathsD? clip,
    PolyTreeD polytree, FillRule fillRule, int precision = 2)
  {
    if (subject == null) return;
    var c = new ClipperD(precision);
    c.AddPaths(subject, PathType.Subject);
    if (clip != null)
      c.AddPaths(clip, PathType.Clip);
    c.Execute(clipType, fillRule, polytree);
  }

  public static Paths64 InflatePaths(Paths64 paths, double delta, JoinType joinType,
    EndType endType, double miterLimit = 2.0)
  {
    var co = new ClipperOffset(miterLimit);
    co.AddPaths(paths, joinType, endType);
    var solution = new Paths64();
    co.Execute(delta, solution);
    return solution;
  }

  public static PathsD InflatePaths(PathsD paths, double delta, JoinType joinType,
    EndType endType, double miterLimit = 2.0, int precision = 2)
  {
    InternalClipper.CheckPrecision(precision);
    var scale = Math.Pow(10, precision);
    var tmp = ScalePaths64(paths, scale);
    var co = new ClipperOffset(miterLimit);
    co.AddPaths(tmp, joinType, endType);
    co.Execute(delta * scale, tmp); // reuse 'tmp' to receive (scaled) solution
    return ScalePathsD(tmp, 1 / scale);
  }

  public static Paths64 RectClip(Rect64 rect, Paths64 paths)
  {
    if (rect.IsEmpty() || paths.Count == 0) return [];
    var rc = new RectClip64(rect);
    return rc.Execute(paths);
  }

  public static Paths64 RectClip(Rect64 rect, Path64 path)
  {
    if (rect.IsEmpty() || path.Count == 0) return [];
    var tmp = new Paths64 { path };
    return RectClip(rect, tmp);
  }

  public static PathsD RectClip(RectD rect, PathsD paths, int precision = 2)
  {
    InternalClipper.CheckPrecision(precision);
    if (rect.IsEmpty() || paths.Count == 0) return [];
    var scale = Math.Pow(10, precision);
    var r = ScaleRect(rect, scale);
    var tmpPath = ScalePaths64(paths, scale);
    var rc = new RectClip64(r);
    tmpPath = rc.Execute(tmpPath);
    return ScalePathsD(tmpPath, 1 / scale);
  }

  public static PathsD RectClip(RectD rect, PathD path, int precision = 2)
  {
    if (rect.IsEmpty() || path.Count == 0) return [];
    var tmp = new PathsD { path };
    return RectClip(rect, tmp, precision);
  }
  public static Paths64 RectClipLines(Rect64 rect, Paths64 paths)
  {
    if (rect.IsEmpty() || paths.Count == 0) return [];
    var rc = new RectClipLines64(rect);
    return rc.Execute(paths);
  }

  public static Paths64 RectClipLines(Rect64 rect, Path64 path)
  {
    if (rect.IsEmpty() || path.Count == 0) return [];
    var tmp = new Paths64 { path };
    return RectClipLines(rect, tmp);
  }

  public static PathsD RectClipLines(RectD rect,
    PathsD paths, int precision = 2)
  {
    InternalClipper.CheckPrecision(precision);
    if (rect.IsEmpty() || paths.Count == 0) return [];
    var scale = Math.Pow(10, precision);
    var r = ScaleRect(rect, scale);
    var tmpPath = ScalePaths64(paths, scale);
    var rc = new RectClipLines64(r);
    tmpPath = rc.Execute(tmpPath);
    return ScalePathsD(tmpPath, 1 / scale);
  }
  public static PathsD RectClipLines(RectD rect, PathD path, int precision = 2)
  {
    if (rect.IsEmpty() || path.Count == 0) return [];
    var tmp = new PathsD { path };
    return RectClipLines(rect, tmp, precision);
  }
  public static Paths64 MinkowskiSum(Path64 pattern, Path64 path, bool isClosed)
  {
    return Minkowski.Sum(pattern, path, isClosed);
  }

  public static PathsD MinkowskiSum(PathD pattern, PathD path, bool isClosed)
  {
    return Minkowski.Sum(pattern, path, isClosed);
  }

  public static Paths64 MinkowskiDiff(Path64 pattern, Path64 path, bool isClosed)
  {
    return Minkowski.Diff(pattern, path, isClosed);
  }

  public static PathsD MinkowskiDiff(PathD pattern, PathD path, bool isClosed)
  {
    return Minkowski.Diff(pattern, path, isClosed);
  }

  public static double Area(Path64 path)
  {
    // https://en.wikipedia.org/wiki/Shoelace_formula
    var a = 0.0;
    var cnt = path.Count;
    if (cnt < 3) return 0.0;
    var prevPt = path[cnt - 1];
    foreach (var pt in path)
    {
      a += (double) (prevPt.Y + pt.Y) * (prevPt.X - pt.X);
      prevPt = pt;
    }
    return a * 0.5;
  }

  public static double Area(Paths64 paths)
  {
    var a = 0.0;
    foreach (var path in paths)
      a += Area(path);
    return a;
  }

  public static double Area(PathD path)
  {
    var a = 0.0;
    var cnt = path.Count;
    if (cnt < 3) return 0.0;
    var prevPt = path[cnt - 1];
    foreach (var pt in path)
    {
      a += (prevPt.Y + pt.Y) * (prevPt.X - pt.X);
      prevPt = pt;
    }
    return a * 0.5;
  }

  public static double Area(PathsD paths)
  {
    var a = 0.0;
    foreach (var path in paths)
      a += Area(path);
    return a;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static bool IsPositive(Path64 poly)
  {
    return Area(poly) >= 0;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static bool IsPositive(PathD poly)
  {
    return Area(poly) >= 0;
  }

  public static string Path64ToString(Path64 path)
  {
    var result = "";
    foreach (var pt in path)
      result += pt.ToString();
    return result + '\n';
  }
  public static string Paths64ToString(Paths64 paths)
  {
    var result = "";
    foreach (var path in paths)
      result += Path64ToString(path);
    return result;
  }
  public static string PathDToString(PathD path)
  {
    var result = "";
    foreach (var pt in path)
      result += pt.ToString();
    return result + '\n';
  }
  public static string PathsDToString(PathsD paths)
  {
    var result = "";
    foreach (var path in paths)
      result += PathDToString(path);
    return result;
  }
  public static Path64 OffsetPath(Path64 path, long dx, long dy)
  {
    var result = new Path64(path.Count);
    foreach (var pt in path)
      result.Add(new Point64(pt.X + dx, pt.Y + dy));
    return result;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static Point64 ScalePoint64(Point64 pt, double scale)
  {
    var result = new Point64()
    {
      X = (long) Math.Round(pt.X * scale, MidpointRounding.AwayFromZero),
      Y = (long) Math.Round(pt.Y * scale, MidpointRounding.AwayFromZero),
#if USINGZ
      Z = pt.Z
#endif
    };
    return result;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static PointD ScalePointD(Point64 pt, double scale)
  {
    var result = new PointD()
    {
      X = pt.X * scale,
      Y = pt.Y * scale,
#if USINGZ
      Z = pt.Z,
#endif
    };
    return result;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static Rect64 ScaleRect(RectD rec, double scale)
  {
    var result = new Rect64()
    {
      left = (long) (rec.left * scale),
      top = (long) (rec.top * scale),
      right = (long) (rec.right * scale),
      bottom = (long) (rec.bottom * scale)
    };
    return result;
  }

  public static Path64 ScalePath(Path64 path, double scale)
  {
    if (InternalClipper.IsAlmostZero(scale - 1)) return path;
    var result = new Path64(path.Count);
#if USINGZ
    foreach (var pt in path)
      result.Add(new Point64(pt.X * scale, pt.Y * scale, pt.Z));
#else
    foreach (Point64 pt in path)
      result.Add(new Point64(pt.X * scale, pt.Y * scale));
#endif
    return result;
  }

  public static Paths64 ScalePaths(Paths64 paths, double scale)
  {
    if (InternalClipper.IsAlmostZero(scale - 1)) return paths;
    var result = new Paths64(paths.Count);
    foreach (var path in paths)
      result.Add(ScalePath(path, scale));
    return result;
  }

  public static PathD ScalePath(PathD path, double scale)
  {
    if (InternalClipper.IsAlmostZero(scale - 1)) return path;
    var result = new PathD(path.Count);
    foreach (var pt in path)
      result.Add(new PointD(pt, scale));
    return result;
  }

  public static PathsD ScalePaths(PathsD paths, double scale)
  {
    if (InternalClipper.IsAlmostZero(scale - 1)) return paths;
    var result = new PathsD(paths.Count);
    foreach (var path in paths)
      result.Add(ScalePath(path, scale));
    return result;
  }

  // Unlike ScalePath, both ScalePath64 & ScalePathD also involve type conversion
  public static Path64 ScalePath64(PathD path, double scale)
  {
    var cnt = path.Count;
    var res = new Path64(cnt);
    foreach (var pt in path)
      res.Add(new Point64(pt, scale));
    return res;
  }

  public static Paths64 ScalePaths64(PathsD paths, double scale)
  {
    var cnt = paths.Count;
    var res = new Paths64(cnt);
    foreach (var path in paths)
      res.Add(ScalePath64(path, scale));
    return res;
  }

  public static PathD ScalePathD(Path64 path, double scale)
  {
    var cnt = path.Count;
    var res = new PathD(cnt);
    foreach (var pt in path)
      res.Add(new PointD(pt, scale));
    return res;
  }

  public static PathsD ScalePathsD(Paths64 paths, double scale)
  {
    var cnt = paths.Count;
    var res = new PathsD(cnt);
    foreach (var path in paths)
      res.Add(ScalePathD(path, scale));
    return res;
  }

  // The static functions Path64 and PathD convert path types without scaling
  public static Path64 Path64(PathD path)
  {
    var result = new Path64(path.Count);
    foreach (var pt in path)
      result.Add(new Point64(pt));
    return result;
  }

  public static Paths64 Paths64(PathsD paths)
  {
    var result = new Paths64(paths.Count);
    foreach (var path in paths)
      result.Add(Path64(path));
    return result;
  }

  public static PathsD PathsD(Paths64 paths)
  {
    var result = new PathsD(paths.Count);
    foreach (var path in paths)
      result.Add(PathD(path));
    return result;
  }

  public static PathD PathD(Path64 path)
  {
    var result = new PathD(path.Count);
    foreach (var pt in path)
      result.Add(new PointD(pt));
    return result;
  }

  public static Path64 TranslatePath(Path64 path, long dx, long dy)
  {
    var result = new Path64(path.Count);
    foreach (var pt in path)
      result.Add(new Point64(pt.X + dx, pt.Y + dy));
    return result;
  }

  public static Paths64 TranslatePaths(Paths64 paths, long dx, long dy)
  {
    var result = new Paths64(paths.Count);
    foreach (var path in paths)
      result.Add(OffsetPath(path, dx, dy));
    return result;
  }

  public static PathD TranslatePath(PathD path, double dx, double dy)
  {
    var result = new PathD(path.Count);
    foreach (var pt in path)
      result.Add(new PointD(pt.X + dx, pt.Y + dy));
    return result;
  }

  public static PathsD TranslatePaths(PathsD paths, double dx, double dy)
  {
    var result = new PathsD(paths.Count);
    foreach (var path in paths)
      result.Add(TranslatePath(path, dx, dy));
    return result;
  }

  public static Path64 ReversePath(Path64 path)
  {
    var result = new Path64(path);
    result.Reverse();
    return result;
  }

  public static PathD ReversePath(PathD path)
  {
    var result = new PathD(path);
    result.Reverse();
    return result;
  }

  public static Paths64 ReversePaths(Paths64 paths)
  {
    var result = new Paths64(paths.Count);
    foreach (var t in paths)
      result.Add(ReversePath(t));

    return result;
  }

  public static PathsD ReversePaths(PathsD paths)
  {
    var result = new PathsD(paths.Count);
    foreach (var path in paths)
      result.Add(ReversePath(path));
    return result;
  }

  public static Rect64 GetBounds(Path64 path)
  {
    var result = InvalidRect64;
    foreach (var pt in path)
    {
      if (pt.X < result.left) result.left = pt.X;
      if (pt.X > result.right) result.right = pt.X;
      if (pt.Y < result.top) result.top = pt.Y;
      if (pt.Y > result.bottom) result.bottom = pt.Y;
    }
    return result.left == long.MaxValue ? new Rect64() : result;
  }

  public static Rect64 GetBounds(Paths64 paths)
  {
    var result = InvalidRect64;
    foreach (var path in paths)
      foreach (var pt in path)
      {
        if (pt.X < result.left) result.left = pt.X;
        if (pt.X > result.right) result.right = pt.X;
        if (pt.Y < result.top) result.top = pt.Y;
        if (pt.Y > result.bottom) result.bottom = pt.Y;
      }
    return result.left == long.MaxValue ? new Rect64() : result;
  }

  public static RectD GetBounds(PathD path)
  {
    var result = InvalidRectD;
    foreach (var pt in path)
    {
      if (pt.X < result.left) result.left = pt.X;
      if (pt.X > result.right) result.right = pt.X;
      if (pt.Y < result.top) result.top = pt.Y;
      if (pt.Y > result.bottom) result.bottom = pt.Y;
    }
    return Math.Abs(result.left - double.MaxValue) < InternalClipper.floatingPointTolerance ? new RectD() : result;
  }

  public static RectD GetBounds(PathsD paths)
  {
    var result = InvalidRectD;
    foreach (var path in paths)
      foreach (var pt in path)
      {
        if (pt.X < result.left) result.left = pt.X;
        if (pt.X > result.right) result.right = pt.X;
        if (pt.Y < result.top) result.top = pt.Y;
        if (pt.Y > result.bottom) result.bottom = pt.Y;
      }
    return Math.Abs(result.left - double.MaxValue) < InternalClipper.floatingPointTolerance ? new RectD() : result;
  }

  public static Path64 MakePath(int[] arr)
  {
    var len = arr.Length / 2;
    var p = new Path64(len);
    for (var i = 0; i < len; i++)
      p.Add(new Point64(arr[i * 2], arr[i * 2 + 1]));
    return p;
  }

  public static Path64 MakePath(long[] arr)
  {
    var len = arr.Length / 2;
    var p = new Path64(len);
    for (var i = 0; i < len; i++)
      p.Add(new Point64(arr[i * 2], arr[i * 2 + 1]));
    return p;
  }

  public static PathD MakePath(double[] arr)
  {
    var len = arr.Length / 2;
    var p = new PathD(len);
    for (var i = 0; i < len; i++)
      p.Add(new PointD(arr[i * 2], arr[i * 2 + 1]));
    return p;
  }

#if USINGZ
  public static Path64 MakePathZ(long[] arr)
  {
    var len = arr.Length / 3;
    var p = new Path64(len);
    for (var i = 0; i < len; i++)
      p.Add(new Point64(arr[i * 3], arr[i * 3 + 1], arr[i * 3 + 2]));
    return p;
  }
  public static PathD MakePathZ(double[] arr)
  {
    var len = arr.Length / 3;
    var p = new PathD(len);
    for (var i = 0; i < len; i++)
      p.Add(new PointD(arr[i * 3], arr[i * 3 + 1], (long)arr[i * 3 + 2]));
    return p;
  }
#endif

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static double Sqr(double val)
  {
    return val * val;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static double Sqr(long val)
  {
    return (double) val * (double) val;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static double DistanceSqr(Point64 pt1, Point64 pt2)
  {
    return Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static Point64 MidPoint(Point64 pt1, Point64 pt2)
  {
    return new Point64((pt1.X + pt2.X) / 2, (pt1.Y + pt2.Y) / 2);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static PointD MidPoint(PointD pt1, PointD pt2)
  {
    return new PointD((pt1.X + pt2.X) / 2, (pt1.Y + pt2.Y) / 2);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static void InflateRect(ref Rect64 rec, int dx, int dy)
  {
    rec.left -= dx;
    rec.right += dx;
    rec.top -= dy;
    rec.bottom += dy;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static void InflateRect(ref RectD rec, double dx, double dy)
  {
    rec.left -= dx;
    rec.right += dx;
    rec.top -= dy;
    rec.bottom += dy;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static bool PointsNearEqual(PointD pt1, PointD pt2, double distanceSqrd)
  {
    return Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y) < distanceSqrd;
  }

  public static PathD StripNearDuplicates(PathD path,
      double minEdgeLenSqrd, bool isClosedPath)
  {
    var cnt = path.Count;
    var result = new PathD(cnt);
    if (cnt == 0) return result;
    var lastPt = path[0];
    result.Add(lastPt);
    for (var i = 1; i < cnt; i++)
      if (!PointsNearEqual(lastPt, path[i], minEdgeLenSqrd))
      {
        lastPt = path[i];
        result.Add(lastPt);
      }

    if (isClosedPath && PointsNearEqual(lastPt, result[0], minEdgeLenSqrd))
    {
      result.RemoveAt(result.Count - 1);
    }

    return result;
  }

  public static Path64 StripDuplicates(Path64 path, bool isClosedPath)
  {
    var cnt = path.Count;
    var result = new Path64(cnt);
    if (cnt == 0) return result;
    var lastPt = path[0];
    result.Add(lastPt);
    for (var i = 1; i < cnt; i++)
      if (lastPt != path[i])
      {
        lastPt = path[i];
        result.Add(lastPt);
      }
    if (isClosedPath && lastPt == result[0])
      result.RemoveAt(result.Count - 1);
    return result;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private static void AddPolyNodeToPaths(PolyPath64 polyPath, Paths64 paths)
  {
    if (polyPath.Polygon!.Count > 0)
      paths.Add(polyPath.Polygon);
    for (var i = 0; i < polyPath.Count; i++)
      AddPolyNodeToPaths((PolyPath64) polyPath._childs[i], paths);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static Paths64 PolyTreeToPaths64(PolyTree64 polyTree)
  {
    var result = new Paths64();
    for (var i = 0; i < polyTree.Count; i++)
      AddPolyNodeToPaths((PolyPath64) polyTree._childs[i], result);
    return result;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static void AddPolyNodeToPathsD(PolyPathD polyPath, PathsD paths)
  {
    if (polyPath.Polygon!.Count > 0)
      paths.Add(polyPath.Polygon);
    for (var i = 0; i < polyPath.Count; i++)
      AddPolyNodeToPathsD((PolyPathD) polyPath._childs[i], paths);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static PathsD PolyTreeToPathsD(PolyTreeD polyTree)
  {
    var result = new PathsD();
    foreach (PolyPathD polyPathBase in polyTree)
    {
      var p = (PolyPathD) polyPathBase;
      AddPolyNodeToPathsD(p, result);
    }

    return result;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static double PerpendicDistFromLineSqrd(PointD pt, PointD line1, PointD line2)
  {
    var a = pt.X - line1.X;
    var b = pt.Y - line1.Y;
    var c = line2.X - line1.X;
    var d = line2.Y - line1.Y;
    if (c == 0 && d == 0) return 0;
    return Sqr(a * d - c * b) / (c * c + d * d);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  public static double PerpendicDistFromLineSqrd(Point64 pt, Point64 line1, Point64 line2)
  {
    var a = (double) pt.X - line1.X;
    var b = (double) pt.Y - line1.Y;
    var c = (double) line2.X - line1.X;
    var d = (double) line2.Y - line1.Y;
    if (c == 0 && d == 0) return 0;
    return Sqr(a * d - c * b) / (c * c + d * d);
  }

  internal static void RDP(Path64 path, int begin, int end, double epsSqrd, List<bool> flags)
  {
    while (true)
    {
      var idx = 0;
      double max_d = 0;
      while (end > begin && path[begin] == path[end]) flags[end--] = false;
      for (var i = begin + 1; i < end; ++i)
      {
        // PerpendicDistFromLineSqrd - avoids expensive Sqrt()
        var d = PerpendicDistFromLineSqrd(path[i], path[begin], path[end]);
        if (d <= max_d) continue;
        max_d = d;
        idx = i;
      }

      if (max_d <= epsSqrd) return;
      flags[idx] = true;
      if (idx > begin + 1) RDP(path, begin, idx, epsSqrd, flags);
      if (idx < end - 1)
      {
        begin = idx;
        continue;
      }

      break;
    }
  }

  public static Path64 RamerDouglasPeucker(Path64 path, double epsilon)
  {
    var len = path.Count;
    if (len < 5) return path;
    var flags = new List<bool>(new bool[len]) { [0] = true, [len - 1] = true };
    RDP(path, 0, len - 1, Sqr(epsilon), flags);
    var result = new Path64(len);
    for (var i = 0; i < len; ++i)
      if (flags[i]) result.Add(path[i]);
    return result;
  }

  public static Paths64 RamerDouglasPeucker(Paths64 paths, double epsilon)
  {
    var result = new Paths64(paths.Count);
    foreach (var path in paths)
      result.Add(RamerDouglasPeucker(path, epsilon));
    return result;
  }

  internal static void RDP(PathD path, int begin, int end, double epsSqrd, List<bool> flags)
  {
    while (true)
    {
      var idx = 0;
      double max_d = 0;
      while (end > begin && path[begin] == path[end]) flags[end--] = false;
      for (var i = begin + 1; i < end; ++i)
      {
        // PerpendicDistFromLineSqrd - avoids expensive Sqrt()
        var d = PerpendicDistFromLineSqrd(path[i], path[begin], path[end]);
        if (d <= max_d) continue;
        max_d = d;
        idx = i;
      }

      if (max_d <= epsSqrd) return;
      flags[idx] = true;
      if (idx > begin + 1) RDP(path, begin, idx, epsSqrd, flags);
      if (idx < end - 1)
      {
        begin = idx;
        continue;
      }

      break;
    }
  }

  public static PathD RamerDouglasPeucker(PathD path, double epsilon)
  {
    var len = path.Count;
    if (len < 5) return path;
    var flags = new List<bool>(new bool[len]) { [0] = true, [len - 1] = true };
    RDP(path, 0, len - 1, Sqr(epsilon), flags);
    var result = new PathD(len);
    for (var i = 0; i < len; ++i)
      if (flags[i]) result.Add(path[i]);
    return result;
  }

  public static PathsD RamerDouglasPeucker(PathsD paths, double epsilon)
  {
    var result = new PathsD(paths.Count);
    foreach (var path in paths)
      result.Add(RamerDouglasPeucker(path, epsilon));
    return result;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private static int GetNext(int current, int high, ref bool[] flags)
  {
    ++current;
    while (current <= high && flags[current]) ++current;
    if (current <= high) return current;
    current = 0;
    while (flags[current]) ++current;
    return current;
  }

  private static int GetPrior(int current, int high, ref bool[] flags)
  {
    if (current == 0) current = high;
    else --current;
    while (current > 0 && flags[current]) --current;
    if (!flags[current]) return current;
    current = high;
    while (flags[current]) --current;
    return current;
  }

  public static Path64 SimplifyPath(Path64 path,
  double epsilon, bool isClosedPath = true)
  {
    int len = path.Count, high = len - 1;
    var epsSqr = Sqr(epsilon);
    if (len < 4) return path;

    var flags = new bool[len];
    var dsq = new double[len];
    var curr = 0;

    if (isClosedPath)
    {
      dsq[0] = PerpendicDistFromLineSqrd(path[0], path[high], path[1]);
      dsq[high] = PerpendicDistFromLineSqrd(path[high], path[0], path[high - 1]);
    }
    else
    {
      dsq[0] = double.MaxValue;
      dsq[high] = double.MaxValue;
    }

    for (var i = 1; i < high; ++i)
      dsq[i] = PerpendicDistFromLineSqrd(path[i], path[i - 1], path[i + 1]);

    for (; ; )
    {
      if (dsq[curr] > epsSqr)
      {
        var start = curr;
        do
        {
          curr = GetNext(curr, high, ref flags);
        } while (curr != start && dsq[curr] > epsSqr);
        if (curr == start) break;
      }

      var prev = GetPrior(curr, high, ref flags);
      var next = GetNext(curr, high, ref flags);
      if (next == prev) break;

      int prior2;
      if (dsq[next] < dsq[curr])
      {
        prior2 = prev;
        prev = curr;
        curr = next;
        next = GetNext(next, high, ref flags);
      }
      else
        prior2 = GetPrior(prev, high, ref flags);

      flags[curr] = true;
      curr = next;
      next = GetNext(next, high, ref flags);
      if (isClosedPath || ((curr != high) && (curr != 0)))
        dsq[curr] = PerpendicDistFromLineSqrd(path[curr], path[prev], path[next]);
      if (isClosedPath || ((prev != 0) && (prev != high)))
        dsq[prev] = PerpendicDistFromLineSqrd(path[prev], path[prior2], path[curr]);
    }
    var result = new Path64(len);
    for (var i = 0; i < len; i++)
      if (!flags[i]) result.Add(path[i]);
    return result;
  }

  public static Paths64 SimplifyPaths(Paths64 paths,
    double epsilon, bool isClosedPaths = true)
  {
    var result = new Paths64(paths.Count);
    foreach (var path in paths)
      result.Add(SimplifyPath(path, epsilon, isClosedPaths));
    return result;
  }

  public static PathD SimplifyPath(PathD path,
    double epsilon, bool isClosedPath = true)
  {
    int len = path.Count, high = len - 1;
    var epsSqr = Sqr(epsilon);
    if (len < 4) return path;

    var flags = new bool[len];
    var dsq = new double[len];
    var curr = 0;
    if (isClosedPath)
    {
      dsq[0] = PerpendicDistFromLineSqrd(path[0], path[high], path[1]);
      dsq[high] = PerpendicDistFromLineSqrd(path[high], path[0], path[high - 1]);
    }
    else
    {
      dsq[0] = double.MaxValue;
      dsq[high] = double.MaxValue;
    }
    for (var i = 1; i < high; ++i)
      dsq[i] = PerpendicDistFromLineSqrd(path[i], path[i - 1], path[i + 1]);

    for (; ; )
    {
      if (dsq[curr] > epsSqr)
      {
        var start = curr;
        do
        {
          curr = GetNext(curr, high, ref flags);
        } while (curr != start && dsq[curr] > epsSqr);
        if (curr == start) break;
      }

      var prev = GetPrior(curr, high, ref flags);
      var next = GetNext(curr, high, ref flags);
      if (next == prev) break;

      int prior2;
      if (dsq[next] < dsq[curr])
      {
        prior2 = prev;
        prev = curr;
        curr = next;
        next = GetNext(next, high, ref flags);
      }
      else
        prior2 = GetPrior(prev, high, ref flags);

      flags[curr] = true;
      curr = next;
      next = GetNext(next, high, ref flags);
      if (isClosedPath || ((curr != high) && (curr != 0)))
        dsq[curr] = PerpendicDistFromLineSqrd(path[curr], path[prev], path[next]);
      if (isClosedPath || ((prev != 0) && (prev != high)))
        dsq[prev] = PerpendicDistFromLineSqrd(path[prev], path[prior2], path[curr]);
    }
    var result = new PathD(len);
    for (var i = 0; i < len; i++)
      if (!flags[i]) result.Add(path[i]);
    return result;
  }

  public static PathsD SimplifyPaths(PathsD paths,
    double epsilon, bool isClosedPath = true)
  {
    var result = new PathsD(paths.Count);
    foreach (var path in paths)
      result.Add(SimplifyPath(path, epsilon, isClosedPath));
    return result;
  }

  public static Path64 TrimCollinear(Path64 path, bool isOpen = false)
  {
    var len = path.Count;
    var i = 0;
    if (!isOpen)
    {
      while (i < len - 1 &&
        InternalClipper.IsCollinear(path[len - 1], path[i], path[i + 1])) i++;
      while (i < len - 1 && InternalClipper.IsCollinear(path[len - 2], path[len - 1], path[i])) len--;
    }

    if (len - i < 3)
    {
      if (!isOpen || len < 2 || path[0] == path[1])
        return [];
      return path;
    }

    var result = new Path64(len - i);
    var last = path[i];
    result.Add(last);
    for (i++; i < len - 1; i++)
    {
      if (InternalClipper.IsCollinear(last, path[i], path[i + 1])) continue;
      last = path[i];
      result.Add(last);
    }

    if (isOpen)
      result.Add(path[len - 1]);
    else if (!InternalClipper.IsCollinear(last, path[len - 1], result[0]))
      result.Add(path[len - 1]);
    else
    {
      while (result.Count > 2 && InternalClipper.IsCollinear(
               result[result.Count - 1], result[result.Count - 2], result[0]))
      {
        result.RemoveAt(result.Count - 1);
      }
      if (result.Count < 3)
        result.Clear();
    }
    return result;
  }

  public static PathD TrimCollinear(PathD path, int precision, bool isOpen = false)
  {
    InternalClipper.CheckPrecision(precision);
    var scale = Math.Pow(10, precision);
    var p = ScalePath64(path, scale);
    p = TrimCollinear(p, isOpen);
    return ScalePathD(p, 1 / scale);
  }

  public static PointInPolygonResult PointInPolygon(Point64 pt, Path64 polygon)
  {
    return InternalClipper.PointInPolygon(pt, polygon);
  }

  public static PointInPolygonResult PointInPolygon(PointD pt,
    PathD polygon, int precision = 2)
  {
    InternalClipper.CheckPrecision(precision);
    var scale = Math.Pow(10, precision);
    var p = new Point64(pt, scale);
    var path = ScalePath64(polygon, scale);
    return InternalClipper.PointInPolygon(p, path);
  }

  public static Path64 Ellipse(Point64 center,
    double radiusX, double radiusY = 0, int steps = 0)
  {
    if (radiusX <= 0) return [];
    if (radiusY <= 0) radiusY = radiusX;
    if (steps <= 2)
      steps = (int) Math.Ceiling(Math.PI * Math.Sqrt((radiusX + radiusY) / 2));

    var si = Math.Sin(2 * Math.PI / steps);
    var co = Math.Cos(2 * Math.PI / steps);
    double dx = co, dy = si;
    var result = new Path64(steps) { new Point64(center.X + radiusX, center.Y) };
    for (var i = 1; i < steps; ++i)
    {
      result.Add(new Point64(center.X + radiusX * dx, center.Y + radiusY * dy));
      var x = dx * co - dy * si;
      dy = dy * co + dx * si;
      dx = x;
    }
    return result;
  }

  public static PathD Ellipse(PointD center,
    double radiusX, double radiusY = 0, int steps = 0)
  {
    if (radiusX <= 0) return [];
    if (radiusY <= 0) radiusY = radiusX;
    if (steps <= 2)
      steps = (int) Math.Ceiling(Math.PI * Math.Sqrt((radiusX + radiusY) / 2));

    var si = Math.Sin(2 * Math.PI / steps);
    var co = Math.Cos(2 * Math.PI / steps);
    double dx = co, dy = si;
    var result = new PathD(steps) { new PointD(center.X + radiusX, center.Y) };
    for (var i = 1; i < steps; ++i)
    {
      result.Add(new PointD(center.X + radiusX * dx, center.Y + radiusY * dy));
      var x = dx * co - dy * si;
      dy = dy * co + dx * si;
      dx = x;
    }
    return result;
  }

  private static void ShowPolyPathStructure(PolyPath64 pp, int level)
  {
    var spaces = new string(' ', level * 2);
    var caption = (pp.IsHole ? "Hole " : "Outer ");
    if (pp.Count == 0)
    {
      Console.WriteLine(spaces + caption);
    }
    else
    {
      Console.WriteLine(spaces + caption + $"({pp.Count})");
      foreach (PolyPath64 child in pp) { ShowPolyPathStructure(child, level + 1); }
    }
  }

  public static void ShowPolyTreeStructure(PolyTree64 polytree)
  {
    Console.WriteLine("Polytree Root");
    foreach (PolyPath64 child in polytree) { ShowPolyPathStructure(child, 1); }
  }

  private static void ShowPolyPathStructure(PolyPathD pp, int level)
  {
    var spaces = new string(' ', level * 2);
    var caption = (pp.IsHole ? "Hole " : "Outer ");
    if (pp.Count == 0)
    {
      Console.WriteLine(spaces + caption);
    }
    else
    {
      Console.WriteLine(spaces + caption + $"({pp.Count})");
      foreach (PolyPathD child in pp) { ShowPolyPathStructure(child, level + 1); }
    }
  }

  public static void ShowPolyTreeStructure(PolyTreeD polytree)
  {
    Console.WriteLine("Polytree Root");
    foreach (PolyPathD child in polytree) { ShowPolyPathStructure(child, 1); }
  }
} // Clipper
