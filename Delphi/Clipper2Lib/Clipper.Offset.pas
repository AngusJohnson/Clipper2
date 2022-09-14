unit Clipper.Offset;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - ver.1.0.4                                            *
* Date      :  12 September 2022                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Path Offset (Inflate/Shrink)                                    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$I Clipper.inc}

interface

uses
  Classes, Clipper.Core;

type

  TJoinType = (jtSquare, jtRound, jtMiter);
  TEndType = (etPolygon, etJoined, etButt, etSquare, etRound);
  // etButt   : offsets both sides of a path, with square blunt ends
  // etSquare : offsets both sides of a path, with square extended ends
  // etRound  : offsets both sides of a path, with round extended ends
  // etJoined : offsets both sides of a path, with joined ends
  // etPolygon: offsets only one side of a closed path

  TGroup = class
	  paths     : TPaths64;
    reversed  : Boolean;
	  joinType  : TJoinType;
	  endType   : TEndType;
    constructor Create(jt: TJoinType; et: TEndType);
  end;

  TClipperOffset = class
  private
    fGrpDelta    : Double;
    fAbsGrpDelta : Double;
    fMinLenSqrd  : double;
    fJoinType    : TJoinType;
    fTmpLimit    : Double;
    fMiterLimit  : Double;
    fArcTolerance: Double;
    fStepsPerRad : Double;
    fNorms       : TPathD;
    fInGroups    : TList;
    fMergeGroups : Boolean;
    fInPath      : TPath64;
    fOutPath     : TPath64;
    fOutPaths    : TPaths64;
    fOutPathLen  : Integer;
    fSolution    : TPaths64;
    fPreserveCollinear  : Boolean;
    fReverseSolution    : Boolean;
    procedure AddPoint(x,y: double); overload;
    procedure AddPoint(const pt: TPoint64); overload;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure DoSquare(j, k: Integer);
    procedure DoMiter(j, k: Integer; cosAplus1: Double);
    procedure DoRound(j, k: integer; angle: double);
    procedure OffsetPoint(j: Integer; var k: integer);

    procedure BuildNormals;
    procedure DoGroupOffset(group: TGroup; groupDelta: double);
    procedure OffsetPolygon;
    procedure OffsetOpenJoined;
    procedure OffsetOpenPath(endType: TEndType);
  public
    constructor Create(miterLimit: double = 2.0;
      arcTolerance: double = 0.0;
      PreserveCollinear: Boolean = False;
      ReverseSolution: Boolean = False);
    destructor Destroy; override;
    procedure AddPath(const path: TPath64;
      joinType: TJoinType; endType: TEndType);
    procedure AddPaths(const paths: TPaths64;
      joinType: TJoinType; endType: TEndType);
    procedure Clear;
    function Execute(delta: Double): TPaths64;

    // MiterLimit: needed for mitered offsets (see offset_triginometry3.svg)
    property MiterLimit: Double read fMiterLimit write fMiterLimit;
    // ArcTolerance: needed for rounded offsets (See offset_triginometry2.svg)
    property ArcTolerance: Double read fArcTolerance write fArcTolerance;
    // MergeGroups: A path group is one or more paths added via the AddPath or
    // AddPaths methods. By default these path groups will be offset
    // independently of other groups and this may cause overlaps (intersections).
    // However, when MergeGroups is enabled, any overlapping offsets will be
    // merged (via a clipping union operation) to remove overlaps.
    property MergeGroups: Boolean read fMergeGroups write fMergeGroups;
    property PreserveCollinear: Boolean
      read fPreserveCollinear write fPreserveCollinear;
    property ReverseSolution: Boolean
      read fReverseSolution write fReverseSolution;
  end;

implementation

uses
  Math, Clipper.Engine;

const
  TwoPi     : Double = 2 * PI;
  InvTwoPi  : Double = 1/(2 * PI);

//------------------------------------------------------------------------------
//  Miscellaneous offset support functions
//------------------------------------------------------------------------------

function DotProduct(const vec1, vec2: TPointD): double;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := vec1.X * vec2.X + vec1.Y * vec2.Y;
end;
//------------------------------------------------------------------------------

function ValueAlmostZero(val: double; epsilon: double = 0.001): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Abs(val) < epsilon;
end;
//------------------------------------------------------------------------------

function NormalizeVector(const vec: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  h, inverseHypot: Double;
begin
  h := Hypot(vec.X, vec.Y);
  if ValueAlmostZero(h) then
  begin
    Result := NullPointD;
    Exit;
  end;
  inverseHypot := 1 / h;
  Result.X := vec.X * inverseHypot;
  Result.Y := vec.Y * inverseHypot;
end;
//------------------------------------------------------------------------------

function GetAvgUnitVector(const vec1, vec2: TPointD): TPointD;
begin
  Result := NormalizeVector(PointD(vec1.X + vec2.X, vec1.Y + vec2.Y));
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TPoint64): TPointD;
var
  dx, dy, inverseHypot: Double;
begin
  if (pt2.X = pt1.X) and (pt2.Y = pt1.Y) then
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;

  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  inverseHypot := 1 / Hypot(dx, dy);
  dx := dx * inverseHypot;
  dy := dy * inverseHypot;
  Result.X := dy;
  Result.Y := -dx
end;
//------------------------------------------------------------------------------

function GetLowestPolygonIdx(const paths: TPaths64): integer;
var
  i,j: integer;
  lp: TPoint64;
  p: TPath64;
begin
	Result := -1;
  lp := Point64(0, -MaxInt64);
	for i := 0 to High(paths) do
	begin
		p := paths[i];
		for j := 0 to High(p) do
    begin
      if (p[j].Y < lp.Y) or
        ((p[j].Y = lp.Y) and (p[j].X >= lp.X)) then Continue;
      Result := i;
      lp := p[j];
    end;
  end;
end;
//------------------------------------------------------------------------------

function UnsafeGet(List: TList; Index: Integer): Pointer;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := List.List[Index];
end;

//------------------------------------------------------------------------------
// TGroup methods
//------------------------------------------------------------------------------

constructor TGroup.Create(jt: TJoinType; et: TEndType);
begin
  Self.joinType := jt;
  Self.endType := et;
end;

//------------------------------------------------------------------------------
// TClipperOffset methods
//------------------------------------------------------------------------------

constructor TClipperOffset.Create(miterLimit: double;
  arcTolerance: double; PreserveCollinear: Boolean;
  ReverseSolution: Boolean);
begin
  fMergeGroups  := true;
  fMiterLimit   := MiterLimit;
  fArcTolerance := ArcTolerance;
  fInGroups     := TList.Create;
  fPreserveCollinear := preserveCollinear;
  fReverseSolution := ReverseSolution;
end;
//------------------------------------------------------------------------------

destructor TClipperOffset.Destroy;
begin
  Clear;
  fInGroups.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Clear;
var
  i: integer;
begin
  for i := 0 to fInGroups.Count -1 do
    TGroup(UnsafeGet(fInGroups, i)).Free;
  fInGroups.Clear;
  fSolution := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPath(const path: TPath64;
  joinType: TJoinType; endType: TEndType);
var
  paths: TPaths64;
begin
  if not assigned(path) then Exit;
  SetLength(paths, 1);
  paths[0] := path;
  AddPaths(Paths, joinType, endType);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPaths(const paths: TPaths64;
  joinType: TJoinType; endType: TEndType);
var
  group: TGroup;
begin
  if Length(paths) = 0 then Exit;
  group := TGroup.Create(joinType, endType);
  AppendPaths(group.paths, paths);
  fInGroups.Add(group);
end;
//------------------------------------------------------------------------------

function GetPerpendic(const pt: TPoint64; const norm: TPointD; delta: double): TPoint64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := Point64(pt.X + norm.X * delta, pt.Y + norm.Y * delta);
end;
//------------------------------------------------------------------------------

function GetPerpendicD(const pt: TPoint64; const norm: TPointD; delta: double): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := PointD(pt.X + norm.X * delta, pt.Y + norm.Y * delta);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoGroupOffset(group: TGroup; groupDelta: double);
var
  i, len, lowestIdx: Integer;
  r, arcTol, area, steps: Double;
  IsClosedPaths: Boolean;
begin
  if group.endType <> etPolygon then
    groupDelta := Abs(groupDelta) * 0.5;

  IsClosedPaths := (group.endType in [etPolygon, etJoined]);
  if IsClosedPaths then
  begin
    // the lowermost polygon must be an outer polygon. So we can use that as the
    // designated orientation for outer polygons (needed for tidy-up clipping)
    lowestIdx := GetLowestPolygonIdx(group.paths);
    if lowestIdx < 0 then Exit;
    // nb: don't use the default orientation here ...
    area := Clipper.Core.Area(group.paths[lowestIdx]);
    if area = 0 then Exit;
    group.reversed := (area < 0);
    if group.reversed then groupDelta := -groupDelta;
  end else
    group.reversed := false;

  fGrpDelta := groupDelta;
  fAbsGrpDelta := Abs(fGrpDelta);
  fJoinType := group.joinType;

  if fArcTolerance > 0 then
    arcTol := fArcTolerance else
    arcTol := Log10(2 + fAbsGrpDelta) * 0.25; // empirically derived

  // calculate a sensible number of steps (for 360 deg for the given offset
  if (group.joinType = jtRound) or (group.endType = etRound) then
  begin
    // get steps per 180 degrees (see offset_triginometry2.svg)
    steps := PI / ArcCos(1 - arcTol / fAbsGrpDelta);
    fStepsPerRad := steps  * InvTwoPi;
  end;

  fOutPaths := nil;
  for i := 0 to High(group.paths) do
  begin
    fInPath := StripDuplicates(group.paths[i], IsClosedPaths);
    len := Length(fInPath);
    if (fInPath = nil) or
      ((group.endType in [etPolygon, etJoined]) and (len < 3)) then Continue;

    fNorms := nil;
    fOutPath := nil;
    fOutPathLen := 0;

		//if a single vertex then build a circle or a square ...
    if len = 1 then
    begin
      if (group.endType = etRound) then
      begin
        r := fAbsGrpDelta;
				if (group.endType = etPolygon) then
          r := r * 0.5;
        with fInPath[0] do
          fOutPath := Path64(Ellipse(RectD(X-r, Y-r, X+r, Y+r)));
      end else
      begin
        SetLength(fOutPath, 4);
        with fInPath[0] do
        begin
          fOutPath[0] := Point64(X-fGrpDelta,Y-fGrpDelta);
          fOutPath[1] := Point64(X+fGrpDelta,Y-fGrpDelta);
          fOutPath[2] := Point64(X+fGrpDelta,Y+fGrpDelta);
          fOutPath[3] := Point64(X-fGrpDelta,Y+fGrpDelta);
        end;
      end;
      AppendPath(fOutPaths, fOutPath);
      Continue;
    end else
    begin
      BuildNormals;
      if group.endType = etPolygon then
        OffsetPolygon
      else if group.endType = etJoined then
        OffsetOpenJoined
      else
        OffsetOpenPath(group.endType);
    end;

    if fOutPathLen = 0 then Continue;
    SetLength(fOutPath, fOutPathLen);
    AppendPath(fOutPaths, fOutPath);
  end;

  if not fMergeGroups then
  begin
    // clean up self-intersections ...
    with TClipper64.Create do
    try
      PreserveCollinear := fPreserveCollinear;
      // the solution should retain the orientation of the input
      ReverseSolution := fReverseSolution <> group.reversed;
      AddSubject(fOutPaths);
      if group.reversed then
        Execute(ctUnion, frNegative, fOutPaths) else
        Execute(ctUnion, frPositive, fOutPaths);
    finally
      free;
    end;
  end;
  // finally copy the working 'outPaths' to the solution
  AppendPaths(fSolution, fOutPaths);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.BuildNormals;
var
  i, len: integer;
begin
  len := Length(fInPath);
  SetLength(fNorms, len);
  for i := 0 to len-2 do
    fNorms[i] := GetUnitNormal(fInPath[i], fInPath[i+1]);
  fNorms[len -1] := GetUnitNormal(fInPath[len -1], fInPath[0]);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPolygon;
var
  i,j: integer;
begin
  j := high(fInPath);
  for i := 0 to high(fInPath) do
    OffsetPoint(i, j);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenJoined;
begin
  OffsetPolygon;
  SetLength(fOutPath, fOutPathLen);
  AppendPath(fOutPaths, fOutPath);
  fOutPath := nil;
  fOutPathLen := 0;
  fInPath := ReversePath(fInPath);
  BuildNormals;
  OffsetPolygon;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenPath(endType: TEndType);
var
  i, k, highI: integer;
begin
  highI := high(fInPath);

 // do the line start cap
  case endType of
    etButt:
      begin
        with fInPath[0] do AddPoint(Point64(
          X - fNorms[0].X *fGrpDelta,
          Y - fNorms[0].Y *fGrpDelta));
        with fInPath[0] do AddPoint(Point64(
          X + fNorms[0].X *fGrpDelta,
          Y + fNorms[0].Y *fGrpDelta));
      end;
    etRound: DoRound(0,0, PI);
    else DoSquare(0, 0);
  end;

  // offset the left side going forward
  k := 0;
  for i := 1 to highI -1 do //nb: -1 is important
    OffsetPoint(i, k);
  AddPoint(GetPerpendic(fInPath[highI], fNorms[highI-1], fGrpDelta));

  // reverse the normals ...
  for i := HighI downto 1 do
  begin
    fNorms[i].X := -fNorms[i-1].X;
    fNorms[i].Y := -fNorms[i-1].Y;
  end;
  fNorms[0].X := fNorms[highI].X;
  fNorms[0].Y := fNorms[highI].Y;

 // do the line end cap
  case endType of
    etButt:
      begin
        with fInPath[highI] do AddPoint(Point64(
          X - fNorms[highI].X *fGrpDelta,
          Y - fNorms[highI].Y *fGrpDelta));
        with fInPath[highI] do AddPoint(Point64(
          X + fNorms[highI].X *fGrpDelta,
          Y + fNorms[highI].Y *fGrpDelta));
      end;
    etRound: DoRound(highI,highI, PI);
    else DoSquare(highI, highI);
  end;

  // offset the left side going back
  k := 0;
  for i := highI downto 1 do //and stop at 1!
    OffsetPoint(i, k);
end;
//------------------------------------------------------------------------------

function TClipperOffset.Execute(delta: Double): TPaths64;
var
  i: integer;
  group: TGroup;
begin
  fSolution := nil;
  Result := nil;
  if fInGroups.Count = 0 then Exit;

  fMinLenSqrd := 1;
  if abs(delta) < Tolerance then
  begin
    // if delta == 0, just copy paths to Result
    for i := 0 to fInGroups.Count -1 do
    begin
      group := TGroup(UnsafeGet(fInGroups, i));
      AppendPaths(fSolution, group.paths);
    end;
    Result := fSolution;
    Exit;
  end;

  // Miter Limit: see offset_triginometry3.svg
  if fMiterLimit > 1 then
    fTmpLimit := 2 / Sqr(fMiterLimit) else
    fTmpLimit := 2.0;

  // nb: delta will depend on whether paths are polygons or open
  for i := 0 to fInGroups.Count -1 do
  begin
    group := TGroup(UnsafeGet(fInGroups, i));
    DoGroupOffset(group, delta);
  end;

  if fMergeGroups and (fInGroups.Count > 0) then
  begin
    // clean up self-intersections ...
    with TClipper64.Create do
    try
      PreserveCollinear := fPreserveCollinear;
      // the solution should retain the orientation of the input

      ReverseSolution :=
        fReverseSolution <> TGroup(fInGroups[0]).reversed;
      AddSubject(fSolution);
      if TGroup(UnsafeGet(fInGroups, 0)).reversed then
        Execute(ctUnion, frNegative, fSolution) else
        Execute(ctUnion, frPositive, fSolution);
    finally
      free;
    end;
  end;
  Result := fSolution;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPoint(x,y: double);
const
  BuffLength = 32;
var
  pt: TPoint64;
begin
  pt := Point64(Round(x),Round(y));
  if fOutPathLen = length(fOutPath) then
    SetLength(fOutPath, fOutPathLen + BuffLength);
  if (fOutPathLen > 0) and
    PointsEqual(fOutPath[fOutPathLen-1], pt) then Exit;
  fOutPath[fOutPathLen] := pt;
  Inc(fOutPathLen);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPoint(const pt: TPoint64);
begin
  AddPoint(pt.X, pt.Y);
end;
//------------------------------------------------------------------------------

function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD): TPointD;
var
  m1,b1,m2,b2: double;
begin
  result := NullPointD;
  //see http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
  if (ln1B.X = ln1A.X) then
  begin
    if (ln2B.X = ln2A.X) then exit; //parallel lines
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    Result.X := ln1A.X;
    Result.Y := m2*ln1A.X + b2;
  end
  else if (ln2B.X = ln2A.X) then
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    Result.X := ln2A.X;
    Result.Y := m1*ln2A.X + b1;
  end else
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    if m1 = m2 then exit; //parallel lines
    Result.X := (b2 - b1)/(m1 - m2);
    Result.Y := m1 * Result.X + b1;
  end;
end;
//------------------------------------------------------------------------------

function ReflectPoint(const pt, pivot: TPointD): TPointD;
begin
  Result.X := pivot.X + (pivot.X - pt.X);
  Result.Y := pivot.Y + (pivot.Y - pt.Y);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoSquare(j, k: Integer);
var
  vec, pt1,pt2,pt3,pt4, pt,ptQ : TPointD;
begin
  // using the reciprocal of unit normals (as unit vectors)
  // get the average unit vector ...

  if k <> j then
    vec := GetAvgUnitVector(
          PointD(-fNorms[k].Y, fNorms[k].X),
          PointD(fNorms[j].Y, -fNorms[j].X))
  else if j = k then
  begin
    vec.X := fNorms[0].Y;     //squaring a line end
    vec.Y := -fNorms[0].X;
  end;

  // now offset the original vertex delta units along unit vector
  ptQ := PointD(fInPath[j]);
  ptQ := TranslatePoint(ptQ, fAbsGrpDelta * vec.X, fAbsGrpDelta * vec.Y);

  // get perpendicular vertices
  pt1 := TranslatePoint(ptQ, fGrpDelta * vec.Y, fGrpDelta * -vec.X);
  pt2 := TranslatePoint(ptQ, fGrpDelta * -vec.Y, fGrpDelta * vec.X);
  // get 2 vertices along one edge offset
  pt3 := GetPerpendicD(fInPath[k], fNorms[k], fGrpDelta);

  if (j = k) then
  begin
    pt4.X := pt3.X + vec.X * fGrpDelta;
    pt4.Y := pt3.Y + vec.Y * fGrpDelta;
  end else
    pt4 := GetPerpendicD(fInPath[j], fNorms[k], fGrpDelta);

  // get the intersection point
  pt := IntersectPoint(pt1, pt2, pt3, pt4);

  if (j = k) then
  begin
    //reverse order of AddPoint when this is the line start
    with ReflectPoint(pt, ptQ) do AddPoint(X, Y);
    AddPoint(pt.X, pt.Y);
  end else
  begin
    AddPoint(pt.X, pt.Y);
    //get the second intersect point through reflecion
    with ReflectPoint(pt, ptQ) do AddPoint(X, Y);
  end;

end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoMiter(j, k: Integer; cosAplus1: Double);
var
  q: Double;
begin
  // see offset_triginometry4.svg
  q := fGrpDelta / cosAplus1;
  AddPoint(fInPath[j].X + (fNorms[k].X + fNorms[j].X)*q,
    fInPath[j].Y + (fNorms[k].Y + fNorms[j].Y)*q);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoRound(j, k: Integer; angle: double);
var
  i, steps: Integer;
  stepSin, stepCos: double;
  pt: TPoint64;
   pt2: TPointD;
begin
	// nb: even though angle may be negative this is a convex join
  pt := fInPath[j];
  pt2 := PointD(fNorms[k].X * fGrpDelta, fNorms[k].Y * fGrpDelta);

  steps := Ceil(fStepsPerRad * abs(angle));
  GetSinCos(angle / steps, stepSin, stepCos);
  pt2 := PointD(fNorms[k].X * fGrpDelta, fNorms[k].Y * fGrpDelta);

  if j = k then
  begin
    AddPoint(pt.X - pt2.X, pt.Y - pt2.Y);
    for i := 0 to steps -1 do
    begin
      pt2 := PointD(pt2.X * stepCos - stepSin * pt2.Y,
        pt2.X * stepSin + pt2.Y * stepCos);
      AddPoint(pt.X - pt2.X, pt.Y - pt2.Y);
    end;
  end else
  begin
    AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);
    for i := 0 to steps -1 do
    begin
      pt2 := PointD(pt2.X * stepCos - stepSin * pt2.Y,
        pt2.X * stepSin + pt2.Y * stepCos);
      AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);
    end;
    pt2 := PointD(fNorms[j].X * fGrpDelta, fNorms[j].Y * fGrpDelta);
    AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPoint(j: Integer; var k: integer);
var
  sinA, cosA: Double;
  almostNoAngle: Boolean;
begin
  // Let A = change in angle where edges join
  // A == 0: ie no change in angle (flat join)
  // A == PI: edges 'spike'
  // sin(A) < 0: right turning
  // cos(A) < 0: change in angle is more than 90 degree
  sinA := CrossProduct(fNorms[k], fNorms[j]);
  cosA := DotProduct(fNorms[j], fNorms[k]);
  if (sinA > 1.0) then sinA := 1.0
  else if (sinA < -1.0) then sinA := -1.0;

  almostNoAngle := ValueAlmostZero(cosA - 1);
  // when there's almost no angle of deviation or it's concave
  if almostNoAngle or (sinA * fGrpDelta < 0) then
  begin
    //concave
    AddPoint(GetPerpendic(fInPath[j], fNorms[k], fGrpDelta));
    // create a simple self-intersection that will be cleaned up later
    if not almostNoAngle then AddPoint(fInPath[j]);
    AddPoint(GetPerpendic(fInPath[j], fNorms[j], fGrpDelta));
  end
  else // convex offset
  begin
    if (fJoinType = jtRound) then
      DoRound(j, k, ArcTan2(sinA, cosA))
    // only miter when the angle isn't too acute (and exceeds ML)
    else if (fJoinType = jtMiter) and (cosA > fTmpLimit -1) then
      DoMiter(j, k, 1 + cosA)
    // only do squaring when the angle of deviation > 90 degrees
    else if (cosA < -0.001) then
      DoSquare(j, k)
    else
      // don't square shallow angles that are safe to miter
      DoMiter(j, k, 1 + cosA);
  end;
  k := j;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
