/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  24 July 2024                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  Path Offset (Inflate/Shrink)                                    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Runtime.CompilerServices;

namespace Clipper2Lib;

public enum JoinType
{
  Miter,
  Square,
  Bevel,
  Round
}

public enum EndType
{
  Polygon,
  Joined,
  Butt,
  Square,
  Round
}

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

      var isJoined = ((endType == EndType.Polygon) || (endType == EndType.Joined));
      inPaths = new Paths64(paths.Count);
      foreach (var path in paths)
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

  private const double Tolerance = 1.0E-12;

  private readonly List<Group> _groupList = [];
  private Path64 pathOut = [];
  private readonly PathD _normals = [];
  private Paths64 _solution = [];
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
  public DeltaCallback64? DeltaCallback { get; set; }

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
    var cnt = path.Count;
    if (cnt == 0) return;
    var pp = new Paths64(1) { path };
    AddPaths(pp, joinType, endType);
  }

  public void AddPaths(Paths64 paths, JoinType joinType, EndType endType)
  {
    var cnt = paths.Count;
    if (cnt == 0) return;
    _groupList.Add(new Group(paths, joinType, endType));
  }

  private int CalcSolutionCapacity()
  {
    var result = 0;
    foreach (var g in _groupList)
      result += (g.endType == EndType.Joined) ? g.inPaths.Count * 2 : g.inPaths.Count;
    return result;
  }

  internal bool CheckPathsReversed()
  {
    var result = false;
    foreach (var g in _groupList)
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
      foreach (var group in _groupList)
        foreach (var path in group.inPaths)
          _solution.Add(path);
      return;
    }

    _delta = delta;
    _mitLimSqr = (MiterLimit <= 1 ?
      2.0 : 2.0 / Clipper.Sqr(MiterLimit));

    foreach (var group in _groupList)
      DoGroupOffset(group);

    if (_groupList.Count == 0) return;

    var pathsReversed = CheckPathsReversed();
    var fillRule = pathsReversed ? FillRule.Negative : FillRule.Positive;

    // clean up self-intersections ...
    var c = new Clipper64
    {
      PreserveCollinear = PreserveCollinear, // the solution should retain the orientation of the input
      ReverseSolution = ReverseSolution != pathsReversed
    };
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

    var f = 1.0 / Math.Sqrt(dx * dx + dy * dy);
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
    var result = -1;
    var botPt = new Point64(long.MaxValue, long.MinValue);
    for (var i = 0; i < paths.Count; ++i)
    {
      foreach (var pt in paths[i])
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
    return new PointD(pt.X + dx, pt.Y + dy, pt.Z);
#else
    return new PointD(pt.X + dx, pt.Y + dy);
#endif
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private static PointD ReflectPoint(PointD pt, PointD pivot)
  {
#if USINGZ
    return new PointD(pivot.X + (pivot.X - pt.X), pivot.Y + (pivot.Y - pt.Y), pt.Z);
#else
    return new PointD(pivot.X + (pivot.X - pt.X), pivot.Y + (pivot.Y - pt.Y));
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
    var h = Hypotenuse(vec.X, vec.Y);
    if (AlmostZero(h)) return new PointD(0, 0);
    var inverseHypot = 1 / h;
    return new PointD(vec.X * inverseHypot, vec.Y * inverseHypot);
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private static PointD GetAvgUnitVector(PointD vec1, PointD vec2)
  {
    return NormalizeVector(new PointD(vec1.X + vec2.X, vec1.Y + vec2.Y));
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private static PointD IntersectPoint(PointD pt1a, PointD pt1b, PointD pt2a, PointD pt2b)
  {
    if (InternalClipper.IsAlmostZero(pt1a.X - pt1b.X)) //vertical
    {
      if (InternalClipper.IsAlmostZero(pt2a.X - pt2b.X)) return new PointD(0, 0);
      var m2 = (pt2b.Y - pt2a.Y) / (pt2b.X - pt2a.X);
      var b2 = pt2a.Y - m2 * pt2a.X;
      return new PointD(pt1a.X, m2 * pt1a.X + b2);
    }

    if (InternalClipper.IsAlmostZero(pt2a.X - pt2b.X)) //vertical
    {
      var m1 = (pt1b.Y - pt1a.Y) / (pt1b.X - pt1a.X);
      var b1 = pt1a.Y - m1 * pt1a.X;
      return new PointD(pt2a.X, m1 * pt2a.X + b1);
    }
    else
    {
      var m1 = (pt1b.Y - pt1a.Y) / (pt1b.X - pt1a.X);
      var b1 = pt1a.Y - m1 * pt1a.X;
      var m2 = (pt2b.Y - pt2a.Y) / (pt2b.X - pt2a.X);
      var b2 = pt2a.Y - m2 * pt2a.X;
      if (InternalClipper.IsAlmostZero(m1 - m2)) return new PointD(0, 0);
      var x = (b2 - b1) / (m1 - m2);
      return new PointD(x, m1 * x + b1);
    }
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private Point64 GetPerpendic(Point64 pt, PointD norm)
  {
#if USINGZ
    return new Point64(pt.X + norm.X * _groupDelta,
      pt.Y + norm.Y * _groupDelta, pt.Z);
#else
    return new Point64(pt.X + norm.X * _groupDelta,
      pt.Y + norm.Y * _groupDelta);
#endif
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private PointD GetPerpendicD(Point64 pt, PointD norm)
  {
#if USINGZ
    return new PointD(pt.X + norm.X * _groupDelta,
      pt.Y + norm.Y * _groupDelta, pt.Z);
#else
    return new PointD(pt.X + norm.X * _groupDelta,
      pt.Y + norm.Y * _groupDelta);
#endif
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private void DoBevel(Path64 path, int j, int k)
  {
    Point64 pt1, pt2;
    if (j == k)
    {
      var absDelta = Math.Abs(_groupDelta);
#if USINGZ
      pt1 = new Point64(
        path[j].X - absDelta * _normals[j].X, 
        path[j].Y - absDelta * _normals[j].Y, path[j].Z);
      pt2 = new Point64(
        path[j].X + absDelta * _normals[j].X, 
        path[j].Y + absDelta * _normals[j].Y, path[j].Z);
#else
      pt1 = new Point64(
        path[j].X - absDelta * _normals[j].X,
        path[j].Y - absDelta * _normals[j].Y);
      pt2 = new Point64(
        path[j].X + absDelta * _normals[j].X,
        path[j].Y + absDelta * _normals[j].Y);
#endif
    }
    else
    {
#if USINGZ
      pt1 = new Point64(
        path[j].X + _groupDelta * _normals[k].X,
        path[j].Y + _groupDelta * _normals[k].Y, path[j].Z);
      pt2 = new Point64(
        path[j].X + _groupDelta * _normals[j].X,
        path[j].Y + _groupDelta * _normals[j].Y, path[j].Z);
#else
      pt1 = new Point64(
        path[j].X + _groupDelta * _normals[k].X,
        path[j].Y + _groupDelta * _normals[k].Y);
      pt2 = new Point64(
        path[j].X + _groupDelta * _normals[j].X,
        path[j].Y + _groupDelta * _normals[j].Y);
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
      vec = new PointD(_normals[j].Y, -_normals[j].X);
    }
    else
    {
      vec = GetAvgUnitVector(
        new PointD(-_normals[k].Y, _normals[k].X),
        new PointD(_normals[j].Y, -_normals[j].X));
    }

    var absDelta = Math.Abs(_groupDelta);
    // now offset the original vertex delta units along unit vector
    var ptQ = new PointD(path[j]);
    ptQ = TranslatePoint(ptQ, absDelta * vec.X, absDelta * vec.Y);

    // get perpendicular vertices
    var pt1 = TranslatePoint(ptQ, _groupDelta * vec.Y, _groupDelta * -vec.X);
    var pt2 = TranslatePoint(ptQ, _groupDelta * -vec.Y, _groupDelta * vec.X);
    // get 2 vertices along one edge offset
    var pt3 = GetPerpendicD(path[k], _normals[k]);

    if (j == k)
    {
      var pt4 = new PointD(
        pt3.X + vec.X * _groupDelta,
        pt3.Y + vec.Y * _groupDelta);
      var pt = IntersectPoint(pt1, pt2, pt3, pt4);
#if USINGZ
      pt.Z = ptQ.Z;
#endif    
      //get the second intersect point through reflecion
      pathOut.Add(new Point64(ReflectPoint(pt, ptQ)));
      pathOut.Add(new Point64(pt));
    }
    else
    {
      var pt4 = GetPerpendicD(path[j], _normals[k]);
      var pt = IntersectPoint(pt1, pt2, pt3, pt4);
#if USINGZ
      pt.Z = ptQ.Z;
#endif
      pathOut.Add(new Point64(pt));
      //get the second intersect point through reflecion
      pathOut.Add(new Point64(ReflectPoint(pt, ptQ)));
    }
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private void DoMiter(Path64 path, int j, int k, double cosA)
  {
    var q = _groupDelta / (cosA + 1);
#if USINGZ
    pathOut.Add(new Point64(
        path[j].X + (_normals[k].X + _normals[j].X) * q,
        path[j].Y + (_normals[k].Y + _normals[j].Y) * q,
        path[j].Z));
#else
    pathOut.Add(new Point64(
        path[j].X + (_normals[k].X + _normals[j].X) * q,
        path[j].Y + (_normals[k].Y + _normals[j].Y) * q));
#endif
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private void DoRound(Path64 path, int j, int k, double angle)
  {
    if (DeltaCallback != null)
    {
      // when DeltaCallback is assigned, _groupDelta won't be constant,
      // so we'll need to do the following calculations for *every* vertex.
      var absDelta = Math.Abs(_groupDelta);
      var arcTol = ArcTolerance > 0.01 ?
        ArcTolerance :
        Math.Log10(2 + absDelta) * InternalClipper.defaultArcTolerance;
      var stepsPer360 = Math.PI / Math.Acos(1 - arcTol / absDelta);
      _stepSin = Math.Sin((2 * Math.PI) / stepsPer360);
      _stepCos = Math.Cos((2 * Math.PI) / stepsPer360);
      if (_groupDelta < 0.0) _stepSin = -_stepSin;
      _stepsPerRad = stepsPer360 / (2 * Math.PI);
    }

    var pt = path[j];
    var offsetVec = new PointD(_normals[k].X * _groupDelta, _normals[k].Y * _groupDelta);
    if (j == k) offsetVec.Negate();
#if USINGZ
    pathOut.Add(new Point64(pt.X + offsetVec.X, pt.Y + offsetVec.Y, pt.Z));
#else
    pathOut.Add(new Point64(pt.X + offsetVec.X, pt.Y + offsetVec.Y));
#endif
    var steps = (int) Math.Ceiling(_stepsPerRad * Math.Abs(angle));
    for (var i = 1; i < steps; i++) // ie 1 less than steps
    {
      offsetVec = new PointD(offsetVec.X * _stepCos - _stepSin * offsetVec.Y,
          offsetVec.X * _stepSin + offsetVec.Y * _stepCos);
#if USINGZ
      pathOut.Add(new Point64(pt.X + offsetVec.X, pt.Y + offsetVec.Y, pt.Z));
#else
      pathOut.Add(new Point64(pt.X + offsetVec.X, pt.Y + offsetVec.Y));
#endif
    }
    pathOut.Add(GetPerpendic(pt, _normals[j]));
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private void BuildNormals(Path64 path)
  {
    var cnt = path.Count;
    _normals.Clear();
    if (cnt == 0) return;
    _normals.EnsureCapacity(cnt);
    for (var i = 0; i < cnt - 1; i++)
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
    var sinA = InternalClipper.CrossProduct(_normals[j], _normals[k]);
    var cosA = InternalClipper.DotProduct(_normals[j], _normals[k]);
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
      // by far the simplest way to construct concave joins, especially those joining very 
      // short segments, is to insert 3 points that produce negative regions. These regions 
      // will be removed later by the finishing union operation. This is also the best way 
      // to ensure that path reversals (ie over-shrunk paths) are removed.
      pathOut.Add(GetPerpendic(path[j], _normals[k]));

      // when the angle is almost flat (cos_a ~= 1), it's safe to skip this middle point
      if (cosA < 0.999) pathOut.Add(path[j]); // (#405, #873)

      pathOut.Add(GetPerpendic(path[j], _normals[j]));
    }
    else if ((cosA > 0.999) && (_joinType != JoinType.Round))
    {
      // almost straight - less than 2.5 degree (#424, #482, #526 & #724) 
      DoMiter(path, j, k, cosA);
    }
    else switch (_joinType)
      {
        // miter unless the angle is sufficiently acute to exceed ML
        case JoinType.Miter when cosA > _mitLimSqr - 1:
          DoMiter(path, j, k, cosA);
          break;
        case JoinType.Miter:
          DoSquare(path, j, k);
          break;
        case JoinType.Round:
          DoRound(path, j, k, Math.Atan2(sinA, cosA));
          break;
        case JoinType.Bevel:
          DoBevel(path, j, k);
          break;
        default:
          DoSquare(path, j, k);
          break;
      }

    k = j;
  }

  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  private void OffsetPolygon(Group group, Path64 path)
  {
    pathOut = [];
    int cnt = path.Count, prev = cnt - 1;
    for (var i = 0; i < cnt; i++)
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
    pathOut = [];
    var highI = path.Count - 1;

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
    for (var i = highI; i > 0; i--)
      _normals[i] = new PointD(-_normals[i - 1].X, -_normals[i - 1].Y);
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
    for (int i = highI - 1, k = highI; i > 0; i--)
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

    var absDelta = Math.Abs(_groupDelta);

    _joinType = group.joinType;
    _endType = group.endType;

    if (group.joinType == JoinType.Round || group.endType == EndType.Round)
    {
      // calculate the number of steps required to approximate a circle
      // (see http://www.angusj.com/clipper2/Docs/Trigonometry.htm)
      // arcTol - when arc_tolerance_ is undefined (0) then curve imprecision
      // will be relative to the size of the offset (delta). Obviously very
      //large offsets will almost always require much less precision.
      var arcTol = ArcTolerance > 0.01 ?
        ArcTolerance :
        Math.Log10(2 + absDelta) * InternalClipper.defaultArcTolerance;
      var stepsPer360 = Math.PI / Math.Acos(1 - arcTol / absDelta);
      _stepSin = Math.Sin((2 * Math.PI) / stepsPer360);
      _stepCos = Math.Cos((2 * Math.PI) / stepsPer360);
      if (_groupDelta < 0.0) _stepSin = -_stepSin;
      _stepsPerRad = stepsPer360 / (2 * Math.PI);
    }

    using var pathIt = group.inPaths.GetEnumerator();
    while (pathIt.MoveNext())
    {
      var p = pathIt.Current!;

      pathOut = [];
      var cnt = p.Count;

      switch (cnt)
      {
        case 1:
          {
            var pt = p[0];

            if (DeltaCallback != null)
            {
              _groupDelta = DeltaCallback(p, _normals, 0, 0);
              if (group.pathsReversed) _groupDelta = -_groupDelta;
              absDelta = Math.Abs(_groupDelta);
            }

            // single vertex so build a circle or square ...
            if (group.endType == EndType.Round)
            {
              var steps = (int) Math.Ceiling(_stepsPerRad * 2 * Math.PI);
              pathOut = Clipper.Ellipse(pt, absDelta, absDelta, steps);
#if USINGZ
          pathOut = InternalClipper.SetZ(pathOut, pt.Z);
#endif
            }
            else
            {
              var d = (int) Math.Ceiling(_groupDelta);
              var r = new Rect64(pt.X - d, pt.Y - d, pt.X + d, pt.Y + d);
              pathOut = r.AsPath();
#if USINGZ
          pathOut = InternalClipper.SetZ(pathOut, pt.Z);
#endif
            }
            _solution.Add(pathOut);
            continue; // end of offsetting a single point 
          }
        case 2 when group.endType == EndType.Joined:
          _endType = (group.joinType == JoinType.Round) ?
            EndType.Round :
            EndType.Square;
          break;
      }

      BuildNormals(p);
      switch (_endType)
      {
        case EndType.Polygon:
          OffsetPolygon(group, p);
          break;
        case EndType.Joined:
          OffsetOpenJoined(group, p);
          break;
        default:
          OffsetOpenPath(group, p);
          break;
      }
    }
  }
}
