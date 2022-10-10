/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - ver.1.0.5                                            *
* Date      :  5 October 2022                                                  *
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

  public class ClipperOffset
  {

    private class Group
    {
      internal Paths64 _inPaths;
      internal Path64 _outPath;
      internal Paths64 _outPaths;
      internal JoinType _joinType;
      internal EndType _endType;
      internal bool _pathsReversed;

      public Group(Paths64 paths, JoinType joinType, EndType endType = EndType.Polygon)
      {
        _inPaths = new Paths64(paths);
        _joinType = joinType;
        _endType = endType;
        _outPath = new Path64();
        _outPaths = new Paths64();
        _pathsReversed = false;
      }
    }

    private readonly List<Group> _pathGroups = new List<Group>();
    private readonly PathD _normals = new PathD();
    private readonly Paths64 solution = new Paths64();
    private double _group_delta, _abs_group_delta, _tmpLimit, _stepsPerRad;
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
      _pathGroups.Add(new Group(paths, joinType, endType));
    }

    public Paths64 Execute(double delta)
    {
      solution.Clear();
      if (Math.Abs(delta) < 0.5)
      {
        foreach (Group group in _pathGroups)
          foreach (Path64 path in group._inPaths)
            solution.Add(path);
        return solution;
      }

      _tmpLimit = (MiterLimit <= 1 ? 2.0 : 2.0 / Clipper.Sqr(MiterLimit));

      foreach (Group group in _pathGroups)
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
    private static int GetLowestPolygonIdx(Paths64 paths)
    {
      Point64 lp = new Point64(0, long.MinValue);
      int result = -1;
      for (int i = 0; i < paths.Count; i++)
        foreach (Point64 pt in paths[i])
        {
          if (pt.Y < lp.Y || (pt.Y == lp.Y && pt.X >= lp.X)) continue;
          result = i;
          lp = pt;
        }
      return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PointD TranslatePoint(PointD pt, double dx, double dy)
    {
	    return new PointD(pt.x + dx, pt.y + dy);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PointD ReflectPoint(PointD pt, PointD pivot)
    {
      return new PointD(pivot.x + (pivot.x - pt.x), pivot.y + (pivot.y - pt.y));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool AlmostZero(double value, double epsilon = 0.001)
    {
      return Math.Abs(value) < epsilon;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static double Hypotenuse(double x, double y)
    {
      return Math.Sqrt(Math.Pow(x, 2) + Math.Pow(y, 2));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PointD NormalizeVector(PointD vec)
    {
	    double h = Hypotenuse(vec.x, vec.y);
	    if (AlmostZero(h)) return new PointD(0,0);
        double inverseHypot = 1 / h;
	    return new PointD(vec.x* inverseHypot, vec.y* inverseHypot);
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PointD GetAvgUnitVector(PointD vec1, PointD vec2)
    {
	    return NormalizeVector(new PointD(vec1.x + vec2.x, vec1.y + vec2.y));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PointD IntersectPoint(PointD pt1a, PointD pt1b, PointD pt2a, PointD pt2b)
    {
      if (InternalClipper.IsAlmostZero(pt1a.x - pt1b.x)) //vertical
      {
        if (InternalClipper.IsAlmostZero(pt2a.x - pt2b.x)) return new PointD(0, 0);
        double m2 = (pt2b.y - pt2a.y) / (pt2b.x - pt2a.x);
        double b2 = pt2a.y - m2 * pt2a.x;
        return new PointD(pt1a.x, m2* pt1a.x + b2);
      }

      if (InternalClipper.IsAlmostZero(pt2a.x - pt2b.x)) //vertical
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
        if (InternalClipper.IsAlmostZero(m1 - m2)) return new PointD(0, 0);
        double x = (b2 - b1) / (m1 - m2);
        return new PointD(x, m1 * x + b1);
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private Point64 GetPerpendic(Point64 pt, PointD norm)
    {
      return new Point64(pt.X + norm.x * _group_delta,
        pt.Y + norm.y * _group_delta);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private PointD GetPerpendicD(Point64 pt, PointD norm)
    {
      return new PointD(pt.X + norm.x * _group_delta,
        pt.Y + norm.y * _group_delta);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoSquare(Group group, Path64 path, int j, int k)
    {
      PointD vec;
      if (j == k)
      {
        vec = new PointD(_normals[0].y, -_normals[0].x);
      }
      else
      {
        vec = GetAvgUnitVector(
          new PointD(-_normals[k].y, _normals[k].x),
          new PointD(_normals[j].y, -_normals[j].x));
      }
      
      // now offset the original vertex delta units along unit vector
      PointD ptQ = new PointD(path[j]);
      ptQ = TranslatePoint(ptQ, _abs_group_delta * vec.x, _abs_group_delta * vec.y);

      // get perpendicular vertices
      PointD pt1 = TranslatePoint(ptQ, _group_delta * vec.y, _group_delta * -vec.x);
      PointD pt2 = TranslatePoint(ptQ, _group_delta * -vec.y, _group_delta * vec.x);
      // get 2 vertices along one edge offset
      PointD pt3 = GetPerpendicD(path[k], _normals[k]);

      if (j == k)
      {
        PointD pt4 = new PointD(
          pt3.x + vec.x * _group_delta,
          pt3.y + vec.y * _group_delta);
        PointD pt = IntersectPoint(pt1, pt2, pt3, pt4);
        //get the second intersect point through reflecion
        group._outPath.Add(new Point64(ReflectPoint(pt, ptQ)));
        group._outPath.Add(new Point64(pt));
      }
      else
      {
        PointD pt4 = GetPerpendicD(path[j], _normals[k]);
        PointD pt = IntersectPoint(pt1, pt2, pt3, pt4);
        group._outPath.Add(new Point64(pt));
        //get the second intersect point through reflecion
        group._outPath.Add(new Point64(ReflectPoint(pt, ptQ)));
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoMiter(Group group, Path64 path, int j, int k, double cosA)
    {
      double q = _group_delta / (cosA + 1);
      group._outPath.Add(new Point64(
          path[j].X + (_normals[k].x + _normals[j].x) * q,
          path[j].Y + (_normals[k].y + _normals[j].y) * q));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoRound(Group group, Path64 path, int j, int k, double angle)
    {
      // even though angle may be negative this is a convex join
      Point64 pt = path[j];
      PointD pt2 = new PointD(_normals[k].x * _group_delta, _normals[k].y * _group_delta);
      if (j == k) pt2.Negate();

      int steps = (int) Math.Ceiling(_stepsPerRad * Math.Abs(angle));
      double stepSin = Math.Sin(angle / steps);
      double stepCos = Math.Cos(angle / steps);

      group._outPath.Add(new Point64(pt.X + pt2.x, pt.Y + pt2.y));
      for (int i = 0; i < steps; i++)
      {
        pt2 = new PointD(pt2.x * stepCos - stepSin * pt2.y,
            pt2.x * stepSin + pt2.y * stepCos);
        group._outPath.Add(new Point64(pt.X + pt2.x, pt.Y + pt2.y));
      }
      group._outPath.Add(GetPerpendic(pt, _normals[j]));
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

    private void OffsetPoint(Group group, Path64 path, int j, ref int k)
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
      bool almostNoAngle = (AlmostZero(sinA) && cosA > 0); 
      if (almostNoAngle || (sinA * _group_delta < 0))
      {
        group._outPath.Add(GetPerpendic(path[j], _normals[k]));
        if (!almostNoAngle) group._outPath.Add(path[j]);
        group._outPath.Add(GetPerpendic(path[j], _normals[j]));
      }
      else //convex
      {
        if (_joinType == JoinType.Round)
          DoRound(group, path, j, k, Math.Atan2(sinA, cosA));
        else if (_joinType == JoinType.Miter)
        {
          // miter unless the angle is so acute the miter would exceeds ML
          if (cosA > _tmpLimit - 1) DoMiter(group, path, j, k, cosA);
          else DoSquare(group, path, j, k);
        }
        // don't bother squaring angles that deviate < ~20 degrees because
        // squaring will be indistinguishable from mitering and just be a lot slower
        else if (cosA > 0.9)
          DoMiter(group, path, j, k, cosA); 
        else
          DoSquare(group, path, j, k);
      }

      k = j;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void OffsetPolygon(Group group, Path64 path)
    {
      group._outPath = new Path64();
      int cnt = path.Count, prev = cnt - 1;
      for (int i = 0; i < cnt; i++)
        OffsetPoint(group, path, i, ref prev);
      group._outPaths.Add(group._outPath);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void OffsetOpenJoined(Group group, Path64 path)
    {
      OffsetPolygon(group, path);
      path = Clipper.ReversePath(path);
      BuildNormals(path);
      OffsetPolygon(group, path);
    }

    private void OffsetOpenPath(Group group, Path64 path, EndType endType)
    {
      group._outPath = new Path64();
      int highI = path.Count - 1;

      // do the line start cap
      switch (endType)
      {
        case EndType.Butt:
          group._outPath.Add(new Point64(
              path[0].X - _normals[0].x * _group_delta,
              path[0].Y - _normals[0].y * _group_delta));
          group._outPath.Add(GetPerpendic(path[0], _normals[0]));
          break;
        case EndType.Round:
          DoRound(group, path, 0, 0, Math.PI);
          break;
        default:
          DoSquare(group, path, 0, 0);
          break;
      }

      // offset the left side going forward
      for (int i = 1, k = 0; i < highI; i++)
        OffsetPoint(group, path, i, ref k);

      // reverse normals ...
      for (int i = highI; i > 0; i--)
        _normals[i] = new PointD(-_normals[i - 1].x, -_normals[i - 1].y);
      _normals[0] = _normals[highI];

      // do the line end cap
      switch (endType)
      {
        case EndType.Butt:
          group._outPath.Add(new Point64(
              path[highI].X - _normals[highI].x * _group_delta,
              path[highI].Y - _normals[highI].y * _group_delta));
          group._outPath.Add(GetPerpendic(path[highI], _normals[highI]));
          break;
        case EndType.Round:
          DoRound(group, path, highI, highI, Math.PI);
          break;
        default:
          DoSquare(group, path, highI, highI);
          break;
      }

      // offset the left side going back
      for (int i = highI, k = 0; i > 0; i--)
        OffsetPoint(group, path, i, ref k);

      group._outPaths.Add(group._outPath);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsFullyOpenEndType(EndType et)
    {
      return (et != EndType.Polygon) && (et != EndType.Joined);
    }

    private void DoGroupOffset(Group group, double delta)
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

      _group_delta = delta;
      _abs_group_delta = Math.Abs(_group_delta);
      _joinType = group._joinType;

      // calculate a sensible number of steps (for 360 deg for the given offset
      if (group._joinType == JoinType.Round || group._endType == EndType.Round)
      {
        double arcTol = ArcTolerance > 0.01 ?
              ArcTolerance :
              Math.Log10(2 + _abs_group_delta) * 0.25; // empirically derived
        // get steps per 180 degrees (see offset_triginometry2.svg)
        _stepsPerRad = Math.PI / Math.Acos(1 - arcTol / _abs_group_delta) / TwoPi;
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
            double r = _abs_group_delta;
            if (group._endType == EndType.Polygon) r *= 0.5;
            group._outPath = Clipper.Ellipse(path[0], r, r);
          }
          else
          {
            int d = (int) Math.Ceiling(_group_delta);
            Rect64 r = new Rect64(path[0].X - d, path[0].Y - d, 
              path[0].X - d, path[0].Y - d);
            group._outPath = r.AsPath();
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