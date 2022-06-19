/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - also known as Clipper2                            *
* Date      :  19 June 2022                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Offsets both open and closed paths (i.e. polylines & polygons). *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Clipper2Lib
{

  using Path64 = List<Point64>;
  using Paths64 = List<List<Point64>>;
  using PathD = List<PointD>;
  using PathsD = List<List<PointD>>;
  public enum JoinType
  {
    Square,
    Round,
    Miter
  };

  public enum EndType
  {
    Polygon,
    Joined,
    Butt,
    Square,
    Round
  };

  internal class PathGroup
  {
    internal Paths64 _inPaths;
    internal Path64 _outPath;
    internal Paths64 _outPaths;
    internal JoinType _joinType;
    internal EndType _endType;
    internal bool _pathsReversed;

    public PathGroup(Paths64 paths, JoinType joinType, EndType endType = EndType.Polygon)
    {
      _inPaths = new Paths64(paths);
      _joinType = joinType;
      _endType = endType;
      _outPath = new Path64();
      _outPaths = new Paths64();
      _pathsReversed = false;
    }
  }

  public class ClipperOffset
  {
    private readonly List<PathGroup> _pathGroups = new List<PathGroup>();
    private readonly PathD _normals = new PathD();
    private readonly Paths64 solution = new Paths64();
    private double _delta, _tmpLimit, _stepsPerRad;
    private JoinType _joinType;
    public double ArcTolerance { get; set; }
    public bool MergeGroups { get; set; }
    public double MiterLimit { get; set; }
    public bool PreserveCollinear { get; set; }
    public bool ReverseSolution { get; set; }

    private const double TwoPi = Math.PI * 2;
    private const double DefaultArcTolerance = 0.25;

    public ClipperOffset(double miterLimit = 2.0, 
      double arcTolerance = 0.0, bool 
      preserveCollinear = false, bool reverseSolution = false)
    {
      MiterLimit = miterLimit;
      ArcTolerance = arcTolerance;
      MergeGroups = true;
      PreserveCollinear = preserveCollinear;
      ReverseSolution = reverseSolution;
    }

    public void Clear()
    {
      _pathGroups.Clear();
    }

    public void AddPath(Path64 path, JoinType joinType, EndType endType)
    {
      int cnt = path.Count;
      if (cnt == 0) return;
      Paths64 pp = new Paths64(1) { path };
      AddPaths(pp, joinType, endType);
    }

    public void AddPaths(Paths64 paths, JoinType joinType, EndType endType)
    {
      int cnt = paths.Count;
      if (cnt == 0) return;
      _pathGroups.Add(new PathGroup(paths, joinType, endType));
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
      _pathGroups.Add(new PathGroup(ClipperFunc.Paths64(paths), joinType, endType));
    }

    public Paths64 Execute(double delta)
    {
      solution.Clear();
      if (Math.Abs(delta) < DefaultArcTolerance)
      {
        foreach (PathGroup group in _pathGroups)
          foreach (Path64 path in group._inPaths)
            solution.Add(path);
        return solution;
      }

      _tmpLimit = (MiterLimit <= 1 ? 2.0 : 2.0 / ClipperFunc.Sqr(MiterLimit));

      foreach (PathGroup group in _pathGroups)
        DoGroupOffset(group, delta);

      if (MergeGroups && _pathGroups.Count > 0)
      {
        //clean up self-intersections ...
        Clipper c = new Clipper(false)
        {
          PreserveCollinear = PreserveCollinear,
          //the solution should retain the orientation of the input
          ReverseSolution = ReverseSolution != _pathGroups[0]._pathsReversed
        };
        c.AddSubject(solution);
        if (_pathGroups[0]._pathsReversed)
          c.Execute(ClipType.Union, FillRule.Negative, solution);
        else
          c.Execute(ClipType.Union, FillRule.Positive, solution);
      }
      return solution;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static PointD GetUnitNormal(Point64 pt1, Point64 pt2)
    {
      double dx = (pt2.X - pt1.X);
      double dy = (pt2.Y - pt1.Y);
      if ((dx == 0) && (dy == 0)) return new PointD();

      double f = 1.0 / Math.Sqrt(dx * dx + dy * dy);
      dx *= f;
      dy *= f;

      return new PointD(dy, -dx);
    }

    private int GetLowestPolygonIdx(Paths64 paths)
    {
      Point64 lp = new Point64(0, long.MinValue);
      int result = -1;
      for (int i = 0; i < paths.Count; i++)
      {
        Path64 p = paths[i];
        for (int j = 0; j < p.Count; j++)
        {
          if (p[j].Y < lp.Y) continue;
          else if (p[j].Y > lp.Y || p[j].X < lp.X)
          {
            result = i;
            lp = p[j];
          }
        }
      }
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoSquare(PathGroup group, Path64 path, int j, int k)
    {
      if (_delta > 0)
      {
        group._outPath.Add(new Point64(
            path[j].X + _delta * (_normals[k].x - _normals[k].y),
            path[j].Y + _delta * (_normals[k].y + _normals[k].x)));
        group._outPath.Add(new Point64(
            path[j].X + _delta * (_normals[j].x + _normals[j].y),
            path[j].Y + _delta * (_normals[j].y - _normals[j].x)));
      }
      else
      {
        group._outPath.Add(new Point64(
            path[j].X + _delta * (_normals[k].x + _normals[k].y),
            path[j].Y + _delta * (_normals[k].y - _normals[k].x)));
        group._outPath.Add(new Point64(
            path[j].X + _delta * (_normals[j].x - _normals[j].y),
            path[j].Y + _delta * (_normals[j].y + _normals[j].x)));
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoMiter(PathGroup group, Path64 path, int j, int k, double cosA)
    {
      double q = _delta / (cosA + 1);
      group._outPath.Add(new Point64(
          path[j].X + (_normals[k].x + _normals[j].x) * q,
          path[j].Y + (_normals[k].y + _normals[j].y) * q));
    }

    private void DoRound(PathGroup group, Point64 pt, PointD normal1, PointD normal2, double angle)
    {
      //even though angle may be negative this is a convex join
      PointD pt2 = new PointD(normal2.x * _delta, normal2.y * _delta);
      int steps = (int) Math.Round(_stepsPerRad * Math.Abs(angle) + 0.501);
      group._outPath.Add(new Point64(pt.X + pt2.x, pt.Y + pt2.y));
      double stepSin = Math.Sin(angle / steps);
      double stepCos = Math.Cos(angle / steps);
      for (int i = 0; i < steps; i++)
      {
        pt2 = new PointD(pt2.x * stepCos - stepSin * pt2.y,
            pt2.x * stepSin + pt2.y * stepCos);
        group._outPath.Add(new Point64(pt.X + pt2.x, pt.Y + pt2.y));
      }
      group._outPath.Add(
        new Point64(pt.X + normal1.x * _delta, pt.Y + normal1.y * _delta));
    }

    private void BuildNormals(Path64 path)
    {
      int cnt = path.Count;
      _normals.Clear();
      _normals.Capacity = cnt;

      for (int i = 0; i < cnt - 1; i++)
        _normals.Add(GetUnitNormal(path[i], path[i + 1]));
      _normals.Add(GetUnitNormal(path[cnt - 1], path[0]));
    }

    private void OffsetPoint(PathGroup group, Path64 path, int j, ref int k)
    {
      //A: angle between adjoining edges (on left side WRT winding direction).
      //A == 0 deg (or A == 360 deg): collinear edges heading in same direction
      //A == 180 deg: collinear edges heading in opposite directions (i.e. a 'spike')
      //sin(A) < 0: convex on left.
      //cos(A) > 0: angles on both left and right sides > 90 degrees
      double sinA = _normals[k].x * _normals[j].y - _normals[j].x * _normals[k].y;
      if (sinA > 1.0) sinA = 1.0;
      else if (sinA < -1.0) sinA = -1.0;

      if (sinA * _delta < 0) // a concave offset
      {
        Point64 p1 = new Point64(
            path[j].X + _normals[k].x * _delta,
            path[j].Y + _normals[k].y * _delta);
        Point64 p2 = new Point64(
            path[j].X + _normals[j].x * _delta,
            path[j].Y + _normals[j].y * _delta);
        group._outPath.Add(p1);
        if (p1 != p2)
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

    private void OffsetPolygon(PathGroup group, Path64 path)
    {
      group._outPath = new Path64();
      int cnt = path.Count, prev = cnt - 1;
      for (int i = 0; i < cnt; i++)
        OffsetPoint(group, path, i, ref prev);
      group._outPaths.Add(group._outPath);
    }

    private void OffsetOpenJoined(PathGroup group, Path64 path)
    {
      OffsetPolygon(group, path);
      path = ClipperFunc.ReversePath(path);
      BuildNormals(path);
      OffsetPolygon(group, path);
    }

    private void OffsetOpenPath(PathGroup group, Path64 path, EndType endType)
    {
      group._outPath = new Path64();
      int cnt = path.Count - 1, k = 0;
      for (int i = 1; i < cnt; i++)
        OffsetPoint(group, path, i, ref k);
      cnt++;

      _normals[cnt - 1] = new PointD(-_normals[cnt - 2].x, -_normals[cnt - 2].y);

      switch (endType)
      {
        case EndType.Butt:
          group._outPath.Add(new Point64(
              path[cnt - 1].X + _normals[cnt - 2].x * _delta,
              path[cnt - 1].Y + _normals[cnt - 2].y * _delta));
          group._outPath.Add(new Point64(
              path[cnt - 1].X - _normals[cnt - 2].x * _delta,
              path[cnt - 1].Y - _normals[cnt - 2].y * _delta));
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
          group._outPath.Add(new Point64(
              path[0].X + _normals[1].x * _delta,
              path[0].Y + _normals[1].y * _delta));
          group._outPath.Add(new Point64(
              path[0].X - _normals[1].x * _delta,
              path[0].Y - _normals[1].y * _delta));
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
        //the lowermost polygon must be an outer polygon. So we can use that as the
        //designated orientation for outer polygons (needed for tidy-up clipping)
        int lowestIdx = GetLowestPolygonIdx(group._inPaths);
        if (lowestIdx < 0) return;
        //nb: don't use the default orientation here ...
        double area = ClipperFunc.Area(group._inPaths[lowestIdx], false);
        if (area == 0) return;
        group._pathsReversed = (area < 0);
        if (group._pathsReversed)
          delta = -delta;
      }
      else
        group._pathsReversed = false;

      _delta = delta;
      double absDelta = Math.Abs(_delta);
      _joinType = group._joinType;

      double arcTol = (ArcTolerance > InternalClipperFunc.floatingPointTolerance
          ? ArcTolerance
          : Math.Log10(2 + absDelta) * DefaultArcTolerance); //empirically derived

      //calculate a sensible number of steps (for 360 deg for the given offset
      if (group._joinType == JoinType.Round || group._endType == EndType.Round)
      {
        //get steps per 180 degrees (see offset_triginometry2.svg)
        _stepsPerRad = Math.PI / Math.Acos(1 - arcTol / absDelta) / TwoPi;
      }

      foreach (Path64 p in group._inPaths)
      {
        Path64 path = ClipperFunc.StripDuplicates(p, isClosedPaths);
        int cnt = path.Count;
        if (cnt == 0 || (cnt < 3 && !IsFullyOpenEndType(group._endType))) continue;

        if (cnt == 1)
        {
          group._outPath = new Path64();
          //single vertex so build a circle or square ...
          if (group._endType == EndType.Round)
          {
            DoRound(group, path[0], new PointD(1.0, 0.0), new PointD(-1.0, 0.0), TwoPi);
          }
          else
          {
            group._outPath.Capacity = 4;
            group._outPath.Add(new Point64(path[0].X - _delta, path[0].Y - _delta));
            group._outPath.Add(new Point64(path[0].X + _delta, path[0].Y - _delta));
            group._outPath.Add(new Point64(path[0].X + _delta, path[0].Y + _delta));
            group._outPath.Add(new Point64(path[0].X - _delta, path[0].Y + _delta));
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
        //clean up self-intersections
        Clipper c = new Clipper(false)
        {
          PreserveCollinear = PreserveCollinear,
          //the solution should retain the orientation of the input
          ReverseSolution = ReverseSolution != group._pathsReversed
        };
        c.AddSubject(group._outPaths);
        if (group._pathsReversed)
          c.Execute(ClipType.Union, FillRule.Negative, group._outPaths);
        else
          c.Execute(ClipType.Union, FillRule.Positive, group._outPaths);
      }
      solution.AddRange(group._outPaths);
      group._outPaths.Clear();
    }
  }

} //namespace