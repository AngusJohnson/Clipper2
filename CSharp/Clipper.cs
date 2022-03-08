/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (release candidate 1) - also known as Clipper2             *
* Date      :  8 March 2022                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This module contains simple functions that will likely cover    *
*              most polygon boolean and offsetting needs, while also avoiding  *
*              the inherent complexities of the other modules.                 *
* Thanks    :  Special thanks to Thong Nguyen (https://nguyen.mn/) and to      *
*              Guus Kuiper (https://www.guuskuiper.nl/) for their invaluable   *
*              assistance in this C# Clipper port.                             *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/


using System.Collections.Generic;

namespace ClipperLib2
{
	using Path64 = List<Point64>;
	using Paths64 = List<List<Point64>>;
	using PathD = List<PointD>;
	using PathsD = List<List<PointD>>;

  public class ClipperFunc
  {
    public static Paths64 Intersect(Paths64 subject, Paths64 clip, FillRule fillRule)
    {
      return BooleanOp(ClipType.Intersection, fillRule, subject, clip);
    }

    public static PathsD Intersect(PathsD subject, PathsD clip, FillRule fillRule)
    {
      return BooleanOp(ClipType.Intersection, fillRule, subject, clip);
    }

    public static Paths64 Union(Paths64 subject, Paths64 clip, FillRule fillRule)
    {
      return BooleanOp(ClipType.Union, fillRule, subject, clip);
    }

    public static PathsD Union(PathsD subject, PathsD clip, FillRule fillRule)
    {
      return BooleanOp(ClipType.Union, fillRule, subject, clip);
    }

    public static Paths64 Difference(Paths64 subject, Paths64 clip, FillRule fillRule)
    {
      return BooleanOp(ClipType.Difference, fillRule, subject, clip);
    }

    public static PathsD Difference(PathsD subject, PathsD clip, FillRule fillRule)
    {
      return BooleanOp(ClipType.Difference, fillRule, subject, clip);
    }

    public static Paths64 Xor(Paths64 subject, Paths64 clip, FillRule fillRule)
    {
      return BooleanOp(ClipType.Xor, fillRule, subject, clip);
    }

    public static PathsD Xor(PathsD subject, PathsD clip, FillRule fillRule)
    {
      return BooleanOp(ClipType.Xor, fillRule, subject, clip);
    }

    public static Paths64 BooleanOp(ClipType clipType, FillRule fillRule, Paths64 subject, Paths64 clip)
    {
      if (subject == null) return null;
      Paths64 solution = new Paths64();
      Clipper c = new Clipper();
      c.AddPaths(subject, PathType.Subject);
      if (clip != null)
        c.AddPaths(clip, PathType.Clip);
      c.Execute(clipType, fillRule, solution);
      return solution;
    }

    public static PathsD BooleanOp(ClipType clipType, FillRule fillRule, PathsD subject, PathsD clip)
    {
      PathsD solution = new PathsD();
      ClipperD c = new ClipperD();
      c.AddSubject(subject);
      c.AddClip(clip);
      c.Execute(clipType, fillRule, solution);
      return solution;
    }

    public static Paths64 InflatePaths(Paths64 paths, double delta, bool isOpen)
    {
      ClipperOffset co = new ClipperOffset();
      if (isOpen)
        co.AddPaths(paths, JoinType.Round, EndType.Round);
      else
        co.AddPaths(paths, JoinType.Round, EndType.Polygon);
      PathsD tmp = co.Execute(delta);
      return Paths(tmp);
    }

    public static PathsD InflatePaths(PathsD paths, double delta, bool isOpen)
    {
      ClipperOffset co = new ClipperOffset();
      if (isOpen)
        co.AddPaths(paths, JoinType.Round, EndType.Round);
      else
        co.AddPaths(paths, JoinType.Round, EndType.Polygon);
      return co.Execute(delta);
    }

    public static double Area(Path64 path)
    {
      double a = 0.0;
      int cnt = path.Count, j = cnt - 1;
      if (cnt < 3) return 0.0;
      for (int i = 0; i < cnt; i++)
      {
        double d = (path[j].X + path[i].X);
        a += d * (path[j].Y - path[i].Y);
        j = i;
      }
      return -a * 0.5;
    }

    public static double Area(Paths64 paths)
    {
      double a = 0.0;
      int cnt = paths.Count;
      for (int i = 0; i < cnt; i++)
        a += Area(paths[i]);
      return a;
    }

    public static double Area(PathD path)
    {
      double a = 0.0;
      int cnt = path.Count, j = cnt - 1;
      if (cnt < 3) return 0.0;
      for (int i = 0; i < cnt; i++)
      {
        double d = (path[j].x + path[i].x);
        a += d * (path[j].y - path[i].y);
        j = i;
      }
      return -a * 0.5;
    }

    public static double Area(PathsD paths)
    {
      double a = 0.0;
      int cnt = paths.Count;
      for (int i = 0; i < cnt; i++)
        a += Area(paths[i]);
      return a;
    }

    public static bool IsClockwise(Path64 poly)
    {
      return Area(poly) >= 0;
    }

    public static bool IsClockwise(PathD poly)
    {
      return Area(poly) >= 0;
    }

    public static Path64 ReversePath(Path64 path)
    {
      int cntMin1 = path.Count - 1;
      Path64 result = new Path64(cntMin1 + 1);
      for (int i = 0; i <= cntMin1; i++)
        result.Add(path[cntMin1 - i]);
      return result;
    }

    public static PathD ReversePath(PathD path)
    {
      int cntMin1 = path.Count - 1;
      PathD result = new PathD(cntMin1 + 1);
      for (int i = 0; i <= cntMin1; i++)
        result.Add(path[cntMin1 - i]);
      return result;
    }

    public static Paths64 ReversePaths(Paths64 paths)
    {
      int cnt = paths.Count;
      Paths64 result = new Paths64(cnt);
      for (int i = 0; i < cnt; i++)
        result.Add(ReversePath(paths[i]));
      return result;
    }

    public static PathsD ReversePaths(PathsD paths)
    {
      int cnt = paths.Count;
      PathsD result = new PathsD(cnt);
      for (int i = 0; i < cnt; i++)
        result.Add(ReversePath(paths[i]));
      return result;
    }

    public static Path64 Path(PathD path)
    {
      int cnt = path.Count;
      Path64 res = new Path64(cnt);
      for (int i = 0; i < cnt; i++)
        res.Add(new Point64(path[i]));
      return res;
    }

    public static Paths64 Paths(PathsD paths)
    {
      int cnt = paths.Count;
      Paths64 res = new Paths64(cnt);
      for (int i = 0; i < cnt; i++)
        res.Add(Path(paths[i]));
      return res;
    }

    public static Rect64 GetBounds(Paths64 paths)
    {
      int i = 0, cnt = paths.Count;
      while (i < cnt && paths[i].Count == 0) i++;
      if (i == cnt) return new Rect64(0, 0, 0, 0);
      Rect64 result = new Rect64(
        paths[i][0].X, paths[i][0].Y,
        paths[i][0].X, paths[i][0].Y);

      for (; i < cnt; i++)
      {
        for (int j = 0; j < paths[i].Count; j++)
        {
          if (paths[i][j].X < result.left) result.left = paths[i][j].X;
          else if (paths[i][j].X > result.right) result.right = paths[i][j].X;
          if (paths[i][j].Y < result.top) result.top = paths[i][j].Y;
          else if (paths[i][j].Y > result.bottom) result.bottom = paths[i][j].Y;
        }
      }
      return result;
    }

    public static RectD GetBounds(PathsD paths)
    {
      int i = 0, cnt = paths.Count;
      while (i < cnt && paths[i].Count == 0) i++;
      if (i == cnt) return new RectD(0, 0, 0, 0);
      RectD result = new RectD(
        paths[i][0].x, paths[i][0].y,
        paths[i][0].x, paths[i][0].y);

      for (; i < cnt; i++)
      {
        for (int j = 0; j < paths[i].Count; j++)
        {
          if (paths[i][j].x < result.left) result.left = paths[i][j].x;
          else if (paths[i][j].x > result.right) result.right = paths[i][j].x;
          if (paths[i][j].y < result.top) result.top = paths[i][j].y;
          else if (paths[i][j].y > result.bottom) result.bottom = paths[i][j].y;
        }
      }
      return result;
    }

    public static PointD ScalePoint(Point64 pt, double scale)
    {
      PointD result = new PointD()
      {
        x = pt.X * scale,
        y = pt.Y * scale,
      #if USINGZ
        z = pt.Z * scale
      #endif
      };
      return result;
    }

    public static Path64 ScalePath(PathD path, double scale)
    {
      if (scale == 0) scale = 1;
      int cnt = path.Count;
      Path64 res = new Path64(cnt);
      for (int i = 0; i < cnt; i++)
        res.Add(new Point64(path[i].x * scale, path[i].y * scale));
      return res;
    }

    public static Paths64 ScalePaths(PathsD paths, double scale)
    {
      if (scale == 0) scale = 1;
      int cnt = paths.Count;
      Paths64 res = new Paths64(cnt);
      for (int i = 0; i < cnt; i++)
        res.Add(ScalePath(paths[i], scale));
      return res;
    }

    public static PathD ScalePath(Path64 path, double scale)
    {
      if (scale == 0) scale = 1;
      int cnt = path.Count;
      PathD res = new PathD(cnt);
      for (int i = 0; i < cnt; i++)
        res.Add(new PointD(path[i].X * scale, path[i].Y * scale));
      return res;
    }

    public static PathsD ScalePaths(Paths64 paths, double scale)
    {
      if (scale == 0) scale = 1;
      int cnt = paths.Count;
      PathsD res = new PathsD(cnt);
      for (int i = 0; i < cnt; i++)
        res.Add(ScalePath(paths[i], scale));
      return res;
    }

    public static void ScalePaths(Paths64 paths, double scale, ref PathsD pathOut)
    {
      if (scale == 0) scale = 1;
      int cnt = paths.Count;
      pathOut.Clear();
      pathOut.Capacity = cnt;
      for (int i = 0; i < cnt; i++)
        pathOut.Add(ScalePath(paths[i], scale));
    }

    public static PathD PathD(Path64 path)
    {
      int cnt = path.Count;
      PathD res = new PathD(cnt);
      for (int i = 0; i < cnt; i++)
        res.Add(new PointD(path[i].X, path[i].Y));
      return res;
    }

    public static void PathD(Path64 path, ref PathD pathOut)
    {
      int cnt = path.Count;
      pathOut.Clear();
      pathOut.Capacity = cnt;
      for (int i = 0; i < cnt; i++)
        pathOut.Add(new PointD(path[i].X, path[i].Y));
    }

    public static PathsD PathsD(Paths64 paths)
    {
      int cnt = paths.Count;
      PathsD res = new PathsD(cnt);
      for (int i = 0; i < cnt; i++)
        res.Add(PathD(paths[i]));
      return res;
    }

    public static void PathsD(Paths64 paths, ref PathsD pathsOut)
    {
      int cnt = paths.Count;
      pathsOut.Clear();
      pathsOut.Capacity = cnt;
      for (int i = 0; i < cnt; i++)
        pathsOut.Add(PathD(paths[i]));
    }

    public static double Sqr(double value) { return value * value; }

    public static bool PointsNearEqual(PointD pt1, PointD pt2, double distanceSqrd)
    {
      return Sqr(pt1.x - pt2.x) + Sqr(pt1.y - pt2.y) < distanceSqrd;
    }

    public static PathD StripNearDuplicates(PathD path, double minEdgeLength, bool isClosedPath)
    {
      int cnt = path.Count;
      PathD result = new PathD(cnt);
      if (cnt == 0) return result;
      PointD lastPt = path[0];
      result.Add(lastPt);
      double minLengthSqrd = minEdgeLength * minEdgeLength;
      for (int i = 1; i < cnt; i++)
        if (!PointsNearEqual(lastPt, path[i], minLengthSqrd))
        {
          lastPt = path[i];
          result.Add(lastPt);
        }
      if (isClosedPath && PointsNearEqual(lastPt, result[0], minLengthSqrd)) result.RemoveAt(result.Count - 1);
      return result;
    }

    private static void AddPolyNodeToPaths(PolyPath polyPath, Paths64 paths)
    {
      if (polyPath.Polygon.Count > 0) paths.Add(polyPath.Polygon);
      for (int i = 0; i < polyPath.ChildCount; i++)
        AddPolyNodeToPaths((PolyPath)polyPath._childs[i], paths);
    }

    public static Paths64 PolyTreeToPaths(PolyTree polyTree)
    {
      Paths64 result = new Paths64();
      for (int i = 0; i < polyTree.ChildCount; i++)
        AddPolyNodeToPaths((PolyPath)polyTree._childs[i], result);
      return result;
    }

    public static void AddPolyNodeToPathsD(PolyPathD polyPath, PathsD paths)
    {
      if (polyPath.Polygon.Count > 0) paths.Add(polyPath.Polygon);
      for (int i = 0; i < polyPath.ChildCount; i++)
        AddPolyNodeToPathsD((PolyPathD)polyPath._childs[i], paths);
    }

    public static PathsD PolyTreeToPathsD(PolyTreeD polyTree)
    {
      PathsD result = new PathsD();
      for (int i = 0; i < polyTree.ChildCount; i++)
        AddPolyNodeToPathsD((PolyPathD)polyTree._childs[i], result);
      return result;
    }

  }

} //namespace