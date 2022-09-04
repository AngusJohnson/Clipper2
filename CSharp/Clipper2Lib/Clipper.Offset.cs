/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - ver.1.0.3                                            *
* Date      :  20 August 2022                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Path Offset (Inflate/Shrink)                                    *
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
      _pathGroups.Add(new PathGroup(Clipper.Paths64(paths), joinType, endType));
    }

    public Paths64 Execute(double delta)
    {
      solution.Clear();
      if (Math.Abs(delta) < 0.5)
      {
        foreach (PathGroup group in _pathGroups)
          foreach (Path64 path in group._inPaths)
            solution.Add(path);
        return solution;
      }

      _tmpLimit = (MiterLimit <= 1 ? 2.0 : 2.0 / Clipper.Sqr(MiterLimit));

      foreach (PathGroup group in _pathGroups)
        DoGroupOffset(group, delta);

      if (MergeGroups && _pathGroups.Count > 0)
      {
        // clean up self-intersections ...
        Clipper64 c = new Clipper64()
        {
          PreserveCollinear = PreserveCollinear,
          // the solution should retain the orientation of the input
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

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
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
          if (p[j].Y > lp.Y || p[j].X < lp.X)
          {
            result = i;
            lp = p[j];
          }
        }
      }
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private PointD TranslatePoint(PointD pt, double dx, double dy)
    {
	    return new PointD(pt.x + dx, pt.y + dy);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private PointD ReflectPoint(PointD pt, PointD pivot)
    {
      return new PointD(pivot.x + (pivot.x - pt.x), pivot.y + (pivot.y - pt.y));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool AlmostZero(double value, double epsilon = 0.001)
    {
      return Math.Abs(value) < epsilon;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private double Hypotenuse(double x, double y)
    {
      return Math.Sqrt(Math.Pow(x, 2) + Math.Pow(y, 2));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private PointD NormalizeVector(PointD vec)
    {
	    double h = Hypotenuse(vec.x, vec.y);
	    if (AlmostZero(h)) return new PointD(0,0);
        double inverseHypot = 1 / h;
	    return new PointD(vec.x* inverseHypot, vec.y* inverseHypot);
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private PointD GetAvgUnitVector(PointD vec1, PointD vec2)
    {
	    return NormalizeVector(new PointD(vec1.x + vec2.x, vec1.y + vec2.y));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private PointD IntersectPoint(PointD pt1a, PointD pt1b, PointD pt2a, PointD pt2b)
    {
      if (pt1a.x == pt1b.x) //vertical
      {
        if (pt2a.x == pt2b.x) return new PointD(0, 0);
        double m2 = (pt2b.y - pt2a.y) / (pt2b.x - pt2a.x);
        double b2 = pt2a.y - m2 * pt2a.x;
        return new PointD(pt1a.x, m2* pt1a.x + b2);
      }

      if (pt2a.x == pt2b.x) //vertical
      {
        double m1 = (pt1b.y - pt1a.y) / (pt1b.x - pt1a.x);
        double b1 = pt1a.y - m1 * pt1a.x;
        return new PointD(pt2a.x, m1* pt2a.x + b1);
      }
      else
      {
        double m1 = (pt1b.y - pt1a.y) / (pt1b.x - pt1a.x);
        double b1 = pt1a.y - m1 * pt1a.x;
        double m2 = (pt2b.y - pt2a.y) / (pt2b.x - pt2a.x);
        double b2 = pt2a.y - m2 * pt2a.x;
        if (m1 == m2) return new PointD(0, 0);
        double x = (b2 - b1) / (m1 - m2);
        return new PointD(x, m1 * x + b1);
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoSquare(PathGroup group, Path64 path, int j, int k)
    {
      // square off at delta distance from original vertex
      PointD vec, pt, ptQ, pt1, pt2, pt3, pt4;

      // using the reciprocal of unit normals (as unit vectors)
      // get the average unit vector ...
      vec = GetAvgUnitVector(
        new PointD(-_normals[k].y, _normals[k].x),
        new PointD(_normals[j].y, -_normals[j].x));

      // now offset the original vertex delta units along unit vector
      ptQ = new PointD(path[j]);
      ptQ = TranslatePoint(ptQ, _delta * vec.x, _delta * vec.y);

      // get perpendicular vertices
      pt1 = TranslatePoint(ptQ, _delta * vec.y, _delta * -vec.x);
      pt2 = TranslatePoint(ptQ, _delta * -vec.y, _delta * vec.x);
      // get 2 vertices along one edge offset
      pt3 = new PointD(
        path[k].X + _normals[k].x * _delta,
        path[k].Y + _normals[k].y * _delta);
      pt4 = new PointD(
        path[j].X + _normals[k].x * _delta,
        path[j].Y + _normals[k].y * _delta);

      // get the intersection point
      pt = IntersectPoint(pt1, pt2, pt3, pt4);
      group._outPath.Add(new Point64(pt));
      //get the second intersect point through reflecion
      group._outPath.Add(new Point64(ReflectPoint(pt, ptQ)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoMiter(PathGroup group, Path64 path, int j, int k, double cosA)
    {
      double q = _delta / (cosA + 1);
      group._outPath.Add(new Point64(
          path[j].X + (_normals[k].x + _normals[j].x) * q,
          path[j].Y + (_normals[k].y + _normals[j].y) * q));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoRound(PathGroup group, Point64 pt, PointD normal1, PointD normal2, double angle)
    {
      // even though angle may be negative this is a convex join
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

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
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
      // Let A = change in angle where edges join
      // A == 0: ie no change in angle (flat join)
      // A == PI: edges 'spike'
      // sin(A) < 0: right turning
      // cos(A) < 0: change in angle is more than 90 degree
      double sinA = InternalClipper.CrossProduct(_normals[j], _normals[k]);
      double cosA = InternalClipper.DotProduct(_normals[j], _normals[k]);
      if (sinA > 1.0) sinA = 1.0;
      else if (sinA < -1.0) sinA = -1.0;

      // when there's almost no angle of deviation or it's concave
      if ((AlmostZero(sinA) && cosA > 0) || (sinA * _delta < 0))
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
          group._outPath.Add(path[j]); // this aids with clipping removal later
          group._outPath.Add(p2);
        }
      }
      else //convex
      {
        if (_joinType == JoinType.Round)
          DoRound(group, path[j], _normals[j], _normals[k], Math.Atan2(sinA, cosA));
        // else miter when the angle isn't too acute (and hence exceed ML)
        else if (_joinType == JoinType.Miter && cosA > _tmpLimit - 1)
          DoMiter(group, path, j, k, cosA);
        // else only square angles that deviate > 90 degrees
        else if (cosA < -0.001)
          DoSquare(group, path, j, k);
        else
          // don't square shallow angles that are safe to miter
          DoMiter(group, path, j, k, cosA);
      }

      k = j;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void OffsetPolygon(PathGroup group, Path64 path)
    {
      group._outPath = new Path64();
      int cnt = path.Count, prev = cnt - 1;
      for (int i = 0; i < cnt; i++)
        OffsetPoint(group, path, i, ref prev);
      group._outPaths.Add(group._outPath);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void OffsetOpenJoined(PathGroup group, Path64 path)
    {
      OffsetPolygon(group, path);
      path = Clipper.ReversePath(path);
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

      // reverse normals ...
      for (int i = cnt - 2; i > 0; i--)
        _normals[i] = new PointD(-_normals[i - 1].x, -_normals[i - 1].y);
      _normals[0] = new PointD(-_normals[1].x, -_normals[1].y);

      k = cnt - 1;
      for (int i = cnt - 2; i > 0; i--)
        OffsetPoint(group, path, i, ref k);

      // now cap the start ...
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

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
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
        // the lowermost polygon must be an outer polygon. So we can use that as the
        // designated orientation for outer polygons (needed for tidy-up clipping)
        int lowestIdx = GetLowestPolygonIdx(group._inPaths);
        if (lowestIdx < 0) return;
        // nb: don't use the default orientation here ...
        double area = Clipper.Area(group._inPaths[lowestIdx]);
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

      // calculate a sensible number of steps (for 360 deg for the given offset
      if (group._joinType == JoinType.Round || group._endType == EndType.Round)
      {
        double arcTol = ArcTolerance > 0.01 ?
              ArcTolerance :
              Math.Log10(2 + absDelta) * 0.25; // empirically derived
        // get steps per 180 degrees (see offset_triginometry2.svg)
        _stepsPerRad = Math.PI / Math.Acos(1 - arcTol / absDelta) / TwoPi;
      }

      foreach (Path64 p in group._inPaths)
      {
        Path64 path = Clipper.StripDuplicates(p, isClosedPaths);
        int cnt = path.Count;
        if (cnt == 0 || (cnt < 3 && !IsFullyOpenEndType(group._endType))) continue;

        if (cnt == 1)
        {
          group._outPath = new Path64();
          // single vertex so build a circle or square ...
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
        // clean up self-intersections
        Clipper64 c = new Clipper64()
        {
          PreserveCollinear = PreserveCollinear,
          // the solution should retain the orientation of the input
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

} // namespace