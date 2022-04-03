/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - also known as Clipper2                            *
* Date      :  3 April 2022                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Offsets both open and closed paths (ie polylines & polygons).   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Collections.Generic;

namespace Clipper2Lib
{
	using Path64 = List<Point64>;
	using Paths64 = List<List<Point64>>;
	using PathD = List<PointD>;
	using PathsD = List<List<PointD>>;

  public enum JoinType { Square, Round, Miter };
  public enum EndType { Polygon, Joined, Butt, Square, Round };

  internal class PathGroup
  {
    internal PathsD _inPaths;
    internal PathD _outPath;
    internal PathsD _outPaths;
    internal JoinType _joinType;
    internal EndType _endType;
    internal bool _pathsReversed;
    public PathGroup(PathsD paths, JoinType joinType, EndType endType = EndType.Polygon) {
      _inPaths = paths;
      _joinType = joinType;
      _endType = endType;
      _outPaths = new PathsD();
      _pathsReversed = false;
    }
  }

  public class ClipperOffset
  {
    private readonly List<PathGroup> _pathGroups = new List<PathGroup>();
    private readonly PathD _normals = new PathD();
    private double _delta, _tmpLimit, _stepsPerRad;
    private JoinType _joinType;
    private double _minLenSqrd;
    public double ArcTolerance { get; set; }
    public bool MergeGroups { get; set; }
    public double MiterLimit { get; set; }
    public int RoundingDecimalPrecision { get; set; }

    private const double TwoPi = Math.PI * 2;
    private const double DefaultArcTolerance = 0.25;

    public ClipperOffset(double miterLimit = 2.0, 
      double arcTolerance = 0.0, int roundingDecimalPrecision = 2)
    {
      MiterLimit = miterLimit;
      ArcTolerance = arcTolerance;
      MergeGroups = true;
      RoundingDecimalPrecision = roundingDecimalPrecision;
    }

    public void Clear()
    {
      _pathGroups.Clear();
    }

    public void AddPath(Path64 path, JoinType joinType, EndType endType)
    {
      int cnt = path.Count;
      if (cnt == 0) return;
      PathsD pp = new PathsD() { ClipperFunc.PathD(path) };
      AddPaths(pp, joinType, endType);
    }

    public void AddPaths(Paths64 paths, JoinType joinType, EndType endType)
    {
      int cnt = paths.Count;
      if (cnt == 0) return;
      _pathGroups.Add(new PathGroup(ClipperFunc.PathsD(paths), joinType, endType));
    }

    public void AddPath(PathD path, JoinType joinType, EndType endType)
    {
      int cnt = path.Count;
      if (cnt == 0) return;
      PathsD pp = new PathsD(1) { path };
      AddPaths(pp, joinType, endType);
    }

    public void AddPaths(PathsD paths, JoinType joinType, EndType endType)
    {
      int cnt = paths.Count;
      if (cnt == 0) return;
      _pathGroups.Add(new PathGroup(paths, joinType, endType));
    }

    public PathsD Execute(double delta)
    {
      double scale = Math.Pow(10, RoundingDecimalPrecision);
      _minLenSqrd = 1/(scale * scale);

      PathsD solution = new PathsD();
      if (Math.Abs(delta) < _minLenSqrd)
      {
        foreach (PathGroup group in _pathGroups)
          foreach (PathD path in group._inPaths)
            solution.Add(path);
        return solution;
      }

      _tmpLimit = (MiterLimit <= 1 ? 2.0 : 2.0 / ClipperFunc.Sqr(MiterLimit));

      foreach (PathGroup group in _pathGroups)
      {
        DoGroupOffset(group, delta);
        foreach (PathD path in group._outPaths)
          solution.Add(path);
      }

      if (MergeGroups && _pathGroups.Count > 0)
      {
        //clean up self-intersections ...
        ClipperD c = new ClipperD(RoundingDecimalPrecision);
        c.AddSubject(solution);
        if (_pathGroups[0]._pathsReversed)
          c.Execute(ClipType.Union, FillRule.Negative, solution);
        else
          c.Execute(ClipType.Union, FillRule.Positive, solution);
      }
      return solution;
    }

    internal static PointD GetUnitNormal(PointD pt1, PointD pt2)
    {
      double dx = (pt2.x - pt1.x);
      double dy = (pt2.y - pt1.y);
      if ((dx == 0) && (dy == 0)) return new PointD();

      double f = 1.0 / Math.Sqrt(dx * dx + dy * dy);
      dx *= f;
      dy *= f;

      return new PointD(dy, -dx);
    }

    private int GetLowestPolygonIdx(PathsD paths)
    {
      PointD lp = new PointD(0, -double.MaxValue);
      int result = -1;
      for (int i = 0; i < paths.Count; i++)
      {
        PathD p = paths[i];
        for (int j = 0; j < p.Count; j++)
          if (p[j].y < lp.y) continue;
          else if (p[j].y > lp.y || p[j].x < lp.x)
          {
            result = i;
            lp = p[j];
          }
      }
      return result;
    }

    private void DoSquare(PathGroup group, PathD path, int j, int k)
    {
      if (_delta > 0) 
      {
        group._outPath.Add(new PointD(
          path[j].x + _delta* (_normals[k].x - _normals[k].y),
          path[j].y + _delta* (_normals[k].y + _normals[k].x)));
        group._outPath.Add(new PointD(
          path[j].x + _delta* (_normals[j].x + _normals[j].y),
          path[j].y + _delta* (_normals[j].y - _normals[j].x)));
      } else
      {
        group._outPath.Add(new PointD(
          path[j].x + _delta* (_normals[k].x + _normals[k].y),
          path[j].y + _delta* (_normals[k].y - _normals[k].x)));
        group._outPath.Add(new PointD(
          path[j].x + _delta* (_normals[j].x - _normals[j].y),
          path[j].y + _delta* (_normals[j].y + _normals[j].x)));
      }
    }

    private void DoMiter(PathGroup group, PathD path, int j, int k, double cosA)
    { 
      double q = _delta / (cosA +1);
      group._outPath.Add(new PointD(
        path[j].x + (_normals[k].x + _normals[j].x) * q,
        path[j].y + (_normals[k].y + _normals[j].y) * q));
    }

    private void DoRound(PathGroup group, PointD pt, PointD normal1, PointD normal2, double angle)
    {
      //even though angle may be negative this is a convex join
      PointD pt2 = new PointD(normal2.x * _delta, normal2.y * _delta);
      int steps = (int)Math.Round(_stepsPerRad * Math.Abs(angle) + 0.5);

      group._outPath.Add(new PointD(pt.x + pt2.x, pt.y + pt2.y));
      if (steps > 0)
      {
        double stepSin = Math.Sin(angle / steps);
        double stepCos = Math.Cos(angle / steps);
        for (int i = 0; i < steps; i++)
        {
          pt2 = new PointD(pt2.x * stepCos - stepSin * pt2.y,
            pt2.x * stepSin + pt2.y * stepCos);
          group._outPath.Add(new PointD(pt.x + pt2.x, pt.y + pt2.y));
        }
      }
      pt2.x = normal1.x * _delta;
      pt2.y = normal1.y * _delta;
      group._outPath.Add(new PointD(pt.x + pt2.x, pt.y + pt2.y));
    }

    private void BuildNormals(PathD path) {
      int cnt = path.Count;
      _normals.Clear();
      _normals.Capacity = cnt;

      for (int i = 0; i < cnt - 1; i++)
        _normals.Add(GetUnitNormal(path[i], path[i + 1]));
      _normals.Add(GetUnitNormal(path[cnt - 1], path[0]));
    }

    private void OffsetPoint(PathGroup group, PathD path, int j, ref int k) 
    { 
      //A: angle between adjoining edges (on left side WRT winding direction).
      //A == 0 deg (or A == 360 deg): collinear edges heading in same direction
      //A == 180 deg: collinear edges heading in opposite directions (ie a 'spike')
      //sin(A) < 0: convex on left.
      //cos(A) > 0: angles on both left and right sides > 90 degrees
      double sinA = _normals[k].x * _normals[j].y - _normals[j].x * _normals[k].y;
      if (sinA > 1.0) sinA = 1.0;
      else if (sinA < -1.0) sinA = -1.0;

      if (sinA * _delta < 0) // a concave offset
      {
        PointD p1 = new PointD(
          path[j].x + _normals[k].x * _delta,
          path[j].y + _normals[k].y * _delta);
        PointD p2 = new PointD(
          path[j].x + _normals[j].x * _delta,
          path[j].y + _normals[j].y * _delta);
        group._outPath.Add(p1);
        if (!ClipperFunc.PointsNearEqual(p1, p2, _minLenSqrd))
        {
          group._outPath.Add(path[j]); //this aids with clipping removal later
          group._outPath.Add(p2);
        }
      }
      else
      {
        double cosA = InternalClipperFunc.DotProduct(_normals[j], _normals[k]);
        switch (_joinType)
        {
          case JoinType.Miter:
            if (1 + cosA < _tmpLimit) DoSquare(group, path, j, k);
            else DoMiter(group, path, j, k, cosA);
            break;
          case JoinType.Square:
            if (cosA >= 0) DoMiter(group, path, j, k, cosA);
            else DoSquare(group, path, j, k);
            break;
          default:
            DoRound(group, path[j], _normals[j], _normals[k], Math.Atan2(sinA, cosA));
            break;
        }
      }
      k = j;
    }

    private void OffsetPolygon(PathGroup group, PathD path)
    {
      group._outPath = new PathD();
      int cnt = path.Count, prev = cnt - 1;
      for (int i = 0; i < cnt; i++)
        OffsetPoint(group, path, i, ref prev);
      group._outPaths.Add(group._outPath);
    }

    private void OffsetOpenJoined(PathGroup group, PathD path)
    {
      OffsetPolygon(group, path);
      path = ClipperFunc.ReversePath(path);
      BuildNormals(path);
      OffsetPolygon(group, path);
    }

    private void OffsetOpenPath(PathGroup group, PathD path, EndType endType)
    {
      group._outPath = new PathD();
      int cnt = path.Count -1, k = 0;
      for (int i = 1; i < cnt; i++)
        OffsetPoint(group, path, i, ref k);
      cnt++;

      _normals[cnt - 1] = new PointD(-_normals[cnt - 2].x, -_normals[cnt - 2].y);

      switch (endType)
      {
        case EndType.Butt:
          group._outPath.Add(new PointD(
            path[cnt - 1].x + _normals[cnt - 2].x * _delta,
            path[cnt - 1].y + _normals[cnt - 2].y * _delta));
          group._outPath.Add(new PointD(
            path[cnt - 1].x - _normals[cnt - 2].x * _delta,
            path[cnt - 1].y - _normals[cnt - 2].y * _delta));
          break;
        case EndType.Round:
          DoRound(group, path[cnt - 1], _normals[cnt - 1], _normals[cnt - 2], Math.PI);
          break;
        default:
          DoSquare(group, path, cnt - 1, cnt - 2);
          break;
      }

      //reverse normals ...
      for (int i = cnt - 2; i > 0; i--)
        _normals[i] = new PointD(-_normals[i - 1].x, -_normals[i - 1].y);
      _normals[0] = new PointD(-_normals[1].x, -_normals[1].y);

      k = cnt - 1;
      for (int i = cnt - 2; i > 0; i--)
        OffsetPoint(group, path, i, ref k);

      //now cap the start ...
      switch (endType)
      {
        case EndType.Butt:
          group._outPath.Add(new PointD(
            path[0].x + _normals[1].x * _delta,
            path[0].y + _normals[1].y * _delta));
          group._outPath.Add(new PointD(
            path[0].x - _normals[1].x * _delta,
            path[0].y - _normals[1].y * _delta));
          break;
        case EndType.Round:
          DoRound(group, path[0], _normals[0], _normals[1], Math.PI);
          break;
        default:
          DoSquare(group, path, 0, 1);
          break;
      }
      group._outPaths.Add(group._outPath);
    }

    private bool IsFullyOpenEndType(EndType et) 
    {
      return (et != EndType.Polygon) && (et != EndType.Joined);
    }

    private void DoGroupOffset(PathGroup group, double delta)
    {
      if (group._endType != EndType.Polygon) delta = Math.Abs(delta) / 2;
      bool isClosedPaths = !IsFullyOpenEndType(group._endType);

      if (isClosedPaths)
      {
        //th  e lowermost polygon must be an outer polygon. So we can use that as the
        //designated orientation for outer polygons (needed for tidy-up clipping)
        int lowestIdx = GetLowestPolygonIdx(group._inPaths);
        if (lowestIdx < 0)  return;
        if (ClipperFunc.Area(group._inPaths[lowestIdx]) < 0)
        {
          //this is more efficient than literally reversing paths
          group._pathsReversed = true; 
          delta = -delta;
        }
      }

#if REVERSE_ORIENTATION
      _delta = delta;
#else
      _delta = -delta;
#endif
      double absDelta = Math.Abs(_delta);
      _joinType = group._joinType;

      double arcTol = (ArcTolerance > InternalClipperFunc.floatingPointTolerance ? 
        ArcTolerance : 
        Math.Log10(2 + absDelta) * DefaultArcTolerance); //empirically derived

      //calculate a sensible number of steps (for 360 deg for the given offset
      if (group._joinType == JoinType.Round || group._endType == EndType.Round)
      {
        //get steps per 180 degrees (see offset_triginometry2.svg)
        double steps = Math.PI / Math.Acos(1 - arcTol / absDelta);
        _stepsPerRad = steps / TwoPi;
      }

      foreach (PathD p in group._inPaths)
      {
        PathD path = ClipperFunc.StripNearDuplicates(p, _minLenSqrd, isClosedPaths);
        int cnt = path.Count;
        if (cnt == 0 || (cnt < 3 && !IsFullyOpenEndType(group._endType))) continue;

        if (cnt == 1)
        {
          group._outPath = new PathD();
          //single vertex so build a circle or square ...
          if (group._endType == EndType.Round)
          {
            DoRound(group, path[0], new PointD(1.0, 0.0), new PointD(-1.0, 0.0), TwoPi);
          }
          else
          {
            group._outPath.Capacity = 4;
            group._outPath.Add(new PointD(path[0].x - _delta, path[0].y - _delta));
            group._outPath.Add(new PointD(path[0].x + _delta, path[0].y - _delta));
            group._outPath.Add(new PointD(path[0].x + _delta, path[0].y + _delta));
            group._outPath.Add(new PointD(path[0].x - _delta, path[0].y + _delta));
          }
          group._outPaths.Add(group._outPath);
        }
        else
        {
          BuildNormals(path);
          if (group._endType == EndType.Polygon) OffsetPolygon(group, path);
          else if (group._endType == EndType.Joined) OffsetOpenJoined(group, path);
          else OffsetOpenPath(group, path, group._endType);
        }

      }

      if (!MergeGroups)
      {
        //clean up self-intersections ...
        ClipperD c = new ClipperD(RoundingDecimalPrecision);
        c.AddSubject(group._outPaths);
        if (group._pathsReversed)
          c.Execute(ClipType.Union, FillRule.Negative, group._outPaths);
        else
          c.Execute(ClipType.Union, FillRule.Positive, group._outPaths);
      }
    }
  }

} //namespace