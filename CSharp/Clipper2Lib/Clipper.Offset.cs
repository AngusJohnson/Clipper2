/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  17 April 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
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
    Miter,
    Square,
    Bevel,
    Round
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
      internal Paths64 inPaths;
      internal JoinType joinType;
      internal EndType endType;
      internal bool pathsReversed;
      internal int lowestPathIdx;

      public Group(Paths64 paths, JoinType joinType, EndType endType = EndType.Polygon)
      {
        this.joinType = joinType;
        this.endType = endType;

        bool isJoined = ((endType == EndType.Polygon) || (endType == EndType.Joined));
        inPaths = new Paths64(paths.Count);
        foreach(Path64 path in paths)
          inPaths.Add(Clipper.StripDuplicates(path, isJoined));

        if (endType == EndType.Polygon)
        {
          lowestPathIdx = GetLowestPathIdx(inPaths);
          // the lowermost path must be an outer path, so if its orientation is negative,
          // then flag that the whole group is 'reversed' (will negate delta etc.)
          // as this is much more efficient than reversing every path.
          pathsReversed = (lowestPathIdx >= 0) && (Clipper.Area(inPaths[lowestPathIdx]) < 0);
        }
        else
        {
          lowestPathIdx = -1;
          pathsReversed = false;
        }
      }
    }

    private static readonly double Tolerance = 1.0E-12;

    private readonly List<Group> _groupList = new List<Group>();
    private Path64 pathOut = new Path64();
    private readonly PathD _normals = new PathD();
    private Paths64 _solution = new Paths64();
    private PolyTree64? _solutionTree;

    private double _groupDelta; //*0.5 for open paths; *-1.0 for negative areas
    private double _delta;
    private double _mitLimSqr;
    private double _stepsPerRad;
    private double _stepSin;
    private double _stepCos;
    private JoinType _joinType;
    private EndType _endType;
    public double ArcTolerance { get; set; }
    public bool MergeGroups { get; set; }
    public double MiterLimit { get; set; }
    public bool PreserveCollinear { get; set; }
    public bool ReverseSolution { get; set; }

    public delegate double DeltaCallback64(Path64 path,
      PathD path_norms, int currPt, int prevPt);
    public ClipperOffset.DeltaCallback64? DeltaCallback { get; set; }

#if USINGZ
    internal void ZCB(Point64 bot1, Point64 top1,
        Point64 bot2, Point64 top2, ref Point64 ip)
    {
      if (bot1.Z != 0 &&
        ((bot1.Z == bot2.Z) || (bot1.Z == top2.Z))) ip.Z = bot1.Z;
      else if (bot2.Z != 0 && bot2.Z == top1.Z) ip.Z = bot2.Z;
      else if (top1.Z != 0 && top1.Z == top2.Z) ip.Z = top1.Z;
      else ZCallback?.Invoke(bot1, top1, bot2, top2, ref ip);
    }
    public ClipperBase.ZCallback64? ZCallback { get; set; }
#endif
    public ClipperOffset(double miterLimit = 2.0,
      double arcTolerance = 0.0, bool
      preserveCollinear = false, bool reverseSolution = false)
    {
      MiterLimit = miterLimit;
      ArcTolerance = arcTolerance;
      MergeGroups = true;
      PreserveCollinear = preserveCollinear;
      ReverseSolution = reverseSolution;
#if USINGZ
      ZCallback = null;
#endif
    }
    public void Clear()
    {
      _groupList.Clear();
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
      _groupList.Add(new Group(paths, joinType, endType));
    }

    private int CalcSolutionCapacity()
    {
      int result = 0;
      foreach (Group g in _groupList)
        result += (g.endType == EndType.Joined) ? g.inPaths.Count * 2 : g.inPaths.Count;
      return result;
    }

    internal bool CheckPathsReversed()
    {
      bool result = false;
      foreach (Group g in _groupList)
        if (g.endType == EndType.Polygon)
        {
          result = g.pathsReversed;
          break;
        }
      return result;
    }

    private void ExecuteInternal(double delta)
    {
      if (_groupList.Count == 0) return;
      _solution.EnsureCapacity(CalcSolutionCapacity());

      // make sure the offset delta is significant
      if (Math.Abs(delta) < 0.5)
      {
        foreach (Group group in _groupList)
          foreach (Path64 path in group.inPaths)
            _solution.Add(path);
        return;
      }

      _delta = delta;
      _mitLimSqr = (MiterLimit <= 1 ?
        2.0 : 2.0 / Clipper.Sqr(MiterLimit));

      foreach (Group group in _groupList)
        DoGroupOffset(group);

      if (_groupList.Count == 0) return;

      bool pathsReversed = CheckPathsReversed();
      FillRule fillRule = pathsReversed ? FillRule.Negative : FillRule.Positive;

      // clean up self-intersections ...
      Clipper64 c = new Clipper64();
      c.PreserveCollinear = PreserveCollinear;
      // the solution should retain the orientation of the input
      c.ReverseSolution = ReverseSolution != pathsReversed;
#if USINGZ
      c.ZCallback = ZCB;
#endif
      c.AddSubject(_solution);
      if (_solutionTree != null)
        c.Execute(ClipType.Union, fillRule, _solutionTree);
      else
        c.Execute(ClipType.Union, fillRule, _solution);

    }

    public void Execute(double delta, Paths64 solution)
    {
      solution.Clear();
      _solution = solution;
      ExecuteInternal(delta);
    }

    public void Execute(double delta, PolyTree64 solutionTree)
    {
      solutionTree.Clear();
      _solutionTree = solutionTree;
      _solution.Clear();
      ExecuteInternal(delta);
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

    public void Execute(DeltaCallback64 deltaCallback, Paths64 solution)
    {
      DeltaCallback = deltaCallback;
      Execute(1.0, solution);
    }    
    
    internal static int GetLowestPathIdx(Paths64 paths)
    {
      int result = -1;
      Point64 botPt = new Point64(long.MaxValue, long.MinValue);
      for (int i = 0; i < paths.Count; ++i)
      {
        foreach (Point64 pt in paths[i])
		    {
          if ((pt.Y < botPt.Y) ||
            ((pt.Y == botPt.Y) && (pt.X >= botPt.X))) continue;
          result = i;
          botPt.X = pt.X;
          botPt.Y = pt.Y;
        }
      }
	    return result;
    }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PointD TranslatePoint(PointD pt, double dx, double dy)
    {
#if USINGZ
      return new PointD(pt.x + dx, pt.y + dy, pt.z);
#else
      return new PointD(pt.x + dx, pt.y + dy);
#endif
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PointD ReflectPoint(PointD pt, PointD pivot)
    {
#if USINGZ
      return new PointD(pivot.x + (pivot.x - pt.x), pivot.y + (pivot.y - pt.y), pt.z);
#else
      return new PointD(pivot.x + (pivot.x - pt.x), pivot.y + (pivot.y - pt.y));
#endif
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
#if USINGZ
      return new Point64(pt.X + norm.x * _groupDelta,
        pt.Y + norm.y * _groupDelta, pt.Z);
#else
      return new Point64(pt.X + norm.x * _groupDelta,
        pt.Y + norm.y * _groupDelta);
#endif
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private PointD GetPerpendicD(Point64 pt, PointD norm)
    {
#if USINGZ
      return new PointD(pt.X + norm.x * _groupDelta,
        pt.Y + norm.y * _groupDelta, pt.Z);
#else
      return new PointD(pt.X + norm.x * _groupDelta,
        pt.Y + norm.y * _groupDelta);
#endif
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoBevel(Path64 path, int j, int k)
    {
      Point64 pt1, pt2;
      if (j == k)
      {
        double absDelta = Math.Abs(_groupDelta);
#if USINGZ
        pt1 = new Point64(
          path[j].X - absDelta * _normals[j].x, 
          path[j].Y - absDelta * _normals[j].y, path[j].Z);
        pt2 = new Point64(
          path[j].X + absDelta * _normals[j].x, 
          path[j].Y + absDelta * _normals[j].y, path[j].Z);
#else
        pt1 = new Point64(
          path[j].X - absDelta * _normals[j].x,
          path[j].Y - absDelta * _normals[j].y);
        pt2 = new Point64(
          path[j].X + absDelta * _normals[j].x,
          path[j].Y + absDelta * _normals[j].y);
#endif
      }
      else
      {
#if USINGZ
        pt1 = new Point64(
          path[j].X + _groupDelta * _normals[k].x,
          path[j].Y + _groupDelta * _normals[k].y, path[j].Z);
        pt2 = new Point64(
          path[j].X + _groupDelta * _normals[j].x,
          path[j].Y + _groupDelta * _normals[j].y, path[j].Z);
#else
        pt1 = new Point64(
          path[j].X + _groupDelta * _normals[k].x,
          path[j].Y + _groupDelta * _normals[k].y);
        pt2 = new Point64(
          path[j].X + _groupDelta * _normals[j].x,
          path[j].Y + _groupDelta * _normals[j].y);
#endif
      }
      pathOut.Add(pt1);
      pathOut.Add(pt2);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoSquare(Path64 path, int j, int k)
    {
      PointD vec;
      if (j == k)
      {
        vec = new PointD(_normals[j].y, -_normals[j].x);
      }
      else
      {
        vec = GetAvgUnitVector(
          new PointD(-_normals[k].y, _normals[k].x),
          new PointD(_normals[j].y, -_normals[j].x));
      }

      double absDelta = Math.Abs(_groupDelta);
      // now offset the original vertex delta units along unit vector
      PointD ptQ = new PointD(path[j]);
      ptQ = TranslatePoint(ptQ, absDelta * vec.x, absDelta * vec.y);

      // get perpendicular vertices
      PointD pt1 = TranslatePoint(ptQ, _groupDelta * vec.y, _groupDelta * -vec.x);
      PointD pt2 = TranslatePoint(ptQ, _groupDelta * -vec.y, _groupDelta * vec.x);
      // get 2 vertices along one edge offset
      PointD pt3 = GetPerpendicD(path[k], _normals[k]);

      if (j == k)
      {
        PointD pt4 = new PointD(
          pt3.x + vec.x * _groupDelta,
          pt3.y + vec.y * _groupDelta);
        PointD pt = IntersectPoint(pt1, pt2, pt3, pt4);
#if USINGZ
        pt.z = ptQ.z;
#endif    
        //get the second intersect point through reflecion
        pathOut.Add(new Point64(ReflectPoint(pt, ptQ)));
        pathOut.Add(new Point64(pt));
      }
      else
      {
        PointD pt4 = GetPerpendicD(path[j], _normals[k]);
        PointD pt = IntersectPoint(pt1, pt2, pt3, pt4);
#if USINGZ
        pt.z = ptQ.z;
#endif
        pathOut.Add(new Point64(pt));
        //get the second intersect point through reflecion
        pathOut.Add(new Point64(ReflectPoint(pt, ptQ)));
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoMiter(Path64 path, int j, int k, double cosA)
    {
      double q = _groupDelta / (cosA + 1);
#if USINGZ
      pathOut.Add(new Point64(
          path[j].X + (_normals[k].x + _normals[j].x) * q,
          path[j].Y + (_normals[k].y + _normals[j].y) * q,
          path[j].Z));
#else
      pathOut.Add(new Point64(
          path[j].X + (_normals[k].x + _normals[j].x) * q,
          path[j].Y + (_normals[k].y + _normals[j].y) * q));
#endif
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void DoRound(Path64 path, int j, int k, double angle)
    {
      if (DeltaCallback != null)
      {
        // when DeltaCallback is assigned, _groupDelta won't be constant,
        // so we'll need to do the following calculations for *every* vertex.
        double absDelta = Math.Abs(_groupDelta);
        double arcTol = ArcTolerance > 0.01 ?
          ArcTolerance :
          Math.Log10(2 + absDelta) * InternalClipper.defaultArcTolerance;
        double stepsPer360 = Math.PI / Math.Acos(1 - arcTol / absDelta);
        _stepSin = Math.Sin((2 * Math.PI) / stepsPer360);
        _stepCos = Math.Cos((2 * Math.PI) / stepsPer360);
        if (_groupDelta < 0.0) _stepSin = -_stepSin;
        _stepsPerRad = stepsPer360 / (2 * Math.PI);
      }

      Point64 pt = path[j];
      PointD offsetVec = new PointD(_normals[k].x * _groupDelta, _normals[k].y * _groupDelta);
      if (j == k) offsetVec.Negate();
#if USINGZ
      pathOut.Add(new Point64(pt.X + offsetVec.x, pt.Y + offsetVec.y, pt.Z));
#else
      pathOut.Add(new Point64(pt.X + offsetVec.x, pt.Y + offsetVec.y));
#endif
      int steps = (int) Math.Ceiling(_stepsPerRad * Math.Abs(angle));
      for (int i = 1; i < steps; i++) // ie 1 less than steps
      {
        offsetVec = new PointD(offsetVec.x * _stepCos - _stepSin * offsetVec.y,
            offsetVec.x * _stepSin + offsetVec.y * _stepCos);
#if USINGZ
        pathOut.Add(new Point64(pt.X + offsetVec.x, pt.Y + offsetVec.y, pt.Z));
#else
        pathOut.Add(new Point64(pt.X + offsetVec.x, pt.Y + offsetVec.y));
#endif
      }
      pathOut.Add(GetPerpendic(pt, _normals[j]));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void BuildNormals(Path64 path)
    {
      int cnt = path.Count;
      _normals.Clear();
      _normals.EnsureCapacity(cnt);

      for (int i = 0; i < cnt - 1; i++)
        _normals.Add(GetUnitNormal(path[i], path[i + 1]));
      _normals.Add(GetUnitNormal(path[cnt - 1], path[0]));
    }

    private void OffsetPoint(Group group, Path64 path, int j, ref int k)
    {
      if (path[j] == path[k]) { k = j; return; }

      // Let A = change in angle where edges join
      // A == 0: ie no change in angle (flat join)
      // A == PI: edges 'spike'
      // sin(A) < 0: right turning
      // cos(A) < 0: change in angle is more than 90 degree
      double sinA = InternalClipper.CrossProduct(_normals[j], _normals[k]);
      double cosA = InternalClipper.DotProduct(_normals[j], _normals[k]);
      if (sinA > 1.0) sinA = 1.0;
      else if (sinA < -1.0) sinA = -1.0;

      if (DeltaCallback != null)
      { 
        _groupDelta = DeltaCallback(path, _normals, j, k);
        if (group.pathsReversed) _groupDelta = -_groupDelta;
      }
      if (Math.Abs(_groupDelta) < Tolerance)
      {
        pathOut.Add(path[j]);
        return;
      }

      if (cosA > -0.999 && (sinA * _groupDelta < 0)) // test for concavity first (#593)
      {
        // is concave
        pathOut.Add(GetPerpendic(path[j], _normals[k]));
        // this extra point is the only simple way to ensure that path reversals
        // (ie over-shrunk paths) are fully cleaned out with the trailing union op.
        // However it's probably safe to skip this whenever an angle is almost flat.
        if (cosA < 0.99) pathOut.Add(path[j]); // (#405)
        pathOut.Add(GetPerpendic(path[j], _normals[j]));
      }
      else if ((cosA > 0.999) && (_joinType != JoinType.Round))
      {
        // almost straight - less than 2.5 degree (#424, #482, #526 & #724) 
        DoMiter(path, j, k, cosA);
      }
      else if (_joinType == JoinType.Miter)
      {
        // miter unless the angle is sufficiently acute to exceed ML
        if (cosA > _mitLimSqr - 1) DoMiter(path, j, k, cosA);
        else DoSquare(path, j, k);
      }
      else if (_joinType == JoinType.Round)
        DoRound(path, j, k, Math.Atan2(sinA, cosA));
      else if (_joinType == JoinType.Bevel)
        DoBevel(path, j, k);
      else
        DoSquare(path, j, k);

      k = j;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void OffsetPolygon(Group group, Path64 path)
    {
      pathOut = new Path64();
      int cnt = path.Count, prev = cnt - 1;
      for (int i = 0; i < cnt; i++)
        OffsetPoint(group, path, i, ref prev);
      _solution.Add(pathOut);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void OffsetOpenJoined(Group group, Path64 path)
    {
      OffsetPolygon(group, path);
      path = Clipper.ReversePath(path);
      BuildNormals(path);
      OffsetPolygon(group, path);
    }

    private void OffsetOpenPath(Group group, Path64 path)
    {
      pathOut = new Path64();
      int highI = path.Count - 1;

      if (DeltaCallback != null) 
        _groupDelta = DeltaCallback(path, _normals, 0, 0);

      // do the line start cap
      if (Math.Abs(_groupDelta) < Tolerance)
        pathOut.Add(path[0]);
      else
        switch (_endType)
        {
          case EndType.Butt:
            DoBevel(path, 0, 0);
            break;
          case EndType.Round:
            DoRound(path, 0, 0, Math.PI);
            break;
          default:
            DoSquare(path, 0, 0);
            break;
        }

      // offset the left side going forward
      for (int i = 1, k = 0; i < highI; i++)
        OffsetPoint(group, path, i, ref k);

      // reverse normals ...
      for (int i = highI; i > 0; i--)
        _normals[i] = new PointD(-_normals[i - 1].x, -_normals[i - 1].y);
      _normals[0] = _normals[highI];

      if (DeltaCallback != null)
        _groupDelta = DeltaCallback(path, _normals, highI, highI);
      // do the line end cap
      if (Math.Abs(_groupDelta) < Tolerance)
        pathOut.Add(path[highI]);
      else
        switch (_endType)
        {
          case EndType.Butt:
            DoBevel(path, highI, highI);
            break;
          case EndType.Round:
            DoRound(path, highI, highI, Math.PI);
            break;
          default:
            DoSquare(path, highI, highI);
            break;
        }

      // offset the left side going back
      for (int i = highI -1, k = highI; i > 0; i--)
        OffsetPoint(group, path, i, ref k);

      _solution.Add(pathOut);
    }

    private void DoGroupOffset(Group group)
    {
      if (group.endType == EndType.Polygon)
      {
        // a straight path (2 points) can now also be 'polygon' offset 
        // where the ends will be treated as (180 deg.) joins
        if (group.lowestPathIdx < 0) _delta = Math.Abs(_delta);
        _groupDelta = (group.pathsReversed) ? -_delta : _delta;
      }
      else
        _groupDelta = Math.Abs(_delta);

      double absDelta = Math.Abs(_groupDelta);

      _joinType = group.joinType;
      _endType = group.endType;

      if (group.joinType == JoinType.Round || group.endType == EndType.Round)
      {
        // calculate the number of steps required to approximate a circle
        // (see http://www.angusj.com/clipper2/Docs/Trigonometry.htm)
        // arcTol - when arc_tolerance_ is undefined (0) then curve imprecision
        // will be relative to the size of the offset (delta). Obviously very
        //large offsets will almost always require much less precision.
        double arcTol = ArcTolerance > 0.01 ?
          ArcTolerance :              
          Math.Log10(2 + absDelta) * InternalClipper.defaultArcTolerance; 
        double stepsPer360 = Math.PI / Math.Acos(1 - arcTol / absDelta);
        _stepSin = Math.Sin((2 * Math.PI) / stepsPer360);
        _stepCos = Math.Cos((2 * Math.PI) / stepsPer360);
        if (_groupDelta < 0.0) _stepSin = -_stepSin;
        _stepsPerRad = stepsPer360 / (2 * Math.PI);
      }

      using List<Path64>.Enumerator pathIt = group.inPaths.GetEnumerator();
      while (pathIt.MoveNext())
      {
        Path64 p = pathIt.Current!;

        pathOut = new Path64();
        int cnt = p.Count;

        if (cnt == 1)
        {
          Point64 pt = p[0];

          if (DeltaCallback != null)
          {
            _groupDelta = DeltaCallback(p, _normals, 0, 0);
            if (group.pathsReversed) _groupDelta = -_groupDelta;
            absDelta = Math.Abs(_groupDelta);
          }

          // single vertex so build a circle or square ...
          if (group.endType == EndType.Round)
          {
            double r = absDelta;
            int steps = (int) Math.Ceiling(_stepsPerRad * 2 * Math.PI);
            pathOut = Clipper.Ellipse(pt, r, r, steps);
#if USINGZ
            pathOut = InternalClipper.SetZ(pathOut, pt.Z);
#endif
          }
          else
          {
            int d = (int) Math.Ceiling(_groupDelta);
            Rect64 r = new Rect64(pt.X - d, pt.Y - d, pt.X + d, pt.Y + d);
            pathOut = r.AsPath();
#if USINGZ
            pathOut = InternalClipper.SetZ(pathOut, pt.Z);
#endif
          }
          _solution.Add(pathOut);
          continue;
        } // end of offsetting a single point 


        if (cnt == 2 && group.endType == EndType.Joined)
          _endType = (group.joinType == JoinType.Round) ?
            EndType.Round :
            EndType.Square;

        BuildNormals(p);
        if (_endType == EndType.Polygon) OffsetPolygon(group, p);
        else if (_endType == EndType.Joined) OffsetOpenJoined(group, p);
        else OffsetOpenPath(group, p);
      }
    }
  }

} // namespace