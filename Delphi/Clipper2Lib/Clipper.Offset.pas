unit Clipper.Offset;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  8 April 2023                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* Purpose   :  Path Offset (Inflate/Shrink)                                    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$I Clipper.inc}

interface

uses
  Classes, Clipper.Core, Clipper.Engine;

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
    fDelta        : Double;
    fGroupDelta   : Double; //*0.5 for open paths; *-1.0 for neg areas
    fAbsGrpDelta  : Double;
    fMinLenSqrd   : double;
    fJoinType     : TJoinType;
    fEndType      : TEndType;
    fTmpLimit     : Double;
    fMiterLimit   : Double;
    fArcTolerance : Double;
    fStepsPerRad  : Double;
    fStepSin      : Double;
    fStepCos      : Double;
    fNorms        : TPathD;
    fGroupList    : TListEx;
    fInPath       : TPath64;
    fOutPath      : TPath64;
    fOutPaths     : TPaths64;
    fOutPathLen   : Integer;
    fSolution     : TPaths64;
    fPreserveCollinear  : Boolean;
    fReverseSolution    : Boolean;
{$IFDEF USINGZ}
    fZCallback64 : TZCallback64;
    procedure AddPoint(x,y: double; z: Int64); overload;
{$ELSE}
    procedure AddPoint(x,y: double); overload;
{$ENDIF}
    procedure AddPoint(const pt: TPoint64); overload;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure DoSquare(j, k: Integer);
    procedure DoMiter(j, k: Integer; cosA: Double);
    procedure DoRound(j, k: integer; angle: double);
    procedure OffsetPoint(j: Integer; var k: integer);

    procedure BuildNormals;
    procedure DoGroupOffset(group: TGroup);
    procedure OffsetPolygon;
    procedure OffsetOpenJoined;
    procedure OffsetOpenPath;
    procedure ExecuteInternal(delta: Double);
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
    procedure Execute(delta: Double; out solution: TPaths64); overload;
    procedure Execute(delta: Double; polytree: TPolyTree64); overload;

    // MiterLimit: needed for mitered offsets (see offset_triginometry3.svg)
    property MiterLimit: Double read fMiterLimit write fMiterLimit;
    // ArcTolerance: needed for rounded offsets (See offset_triginometry2.svg)
    property ArcTolerance: Double read fArcTolerance write fArcTolerance;
    property PreserveCollinear: Boolean
      read fPreserveCollinear write fPreserveCollinear;
    property ReverseSolution: Boolean
      read fReverseSolution write fReverseSolution;
{$IFDEF USINGZ}
    property ZCallback: TZCallback64 read fZCallback64 write fZCallback64;
{$ENDIF}
  end;

implementation

uses
  Math;

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
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  if (dx = 0) and (dy = 0) then
  begin
    Result.X := 0;
    Result.Y := 0;
  end else
  begin
    inverseHypot := 1 / Hypot(dx, dy);
    Result.X := dy * inverseHypot;
    Result.Y := -dx * inverseHypot; //ie left side of vector
  end;
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
  fMiterLimit   := MiterLimit;
  fArcTolerance := ArcTolerance;
  fGroupList    := TListEx.Create;
  fPreserveCollinear := preserveCollinear;
  fReverseSolution := ReverseSolution;
end;
//------------------------------------------------------------------------------

destructor TClipperOffset.Destroy;
begin
  Clear;
  fGroupList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Clear;
var
  i: integer;
begin
  for i := 0 to fGroupList.Count -1 do
    TGroup(fGroupList[i]).Free;
  fGroupList.Clear;
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
  fGroupList.Add(group);
end;
//------------------------------------------------------------------------------

function GetPerpendic(const pt: TPoint64; const norm: TPointD; delta: double): TPoint64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := Point64(pt.X + norm.X * delta, pt.Y + norm.Y * delta);
{$IFDEF USINGZ}
  result.Z := pt.Z;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function GetPerpendicD(const pt: TPoint64; const norm: TPointD; delta: double): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := PointD(pt.X + norm.X * delta, pt.Y + norm.Y * delta);
{$IFDEF USINGZ}
  result.Z := pt.Z;
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoGroupOffset(group: TGroup);
var
  i,j, len, lowestIdx: Integer;
  r, stepsPer360, arcTol, area: Double;
  rec: TRect64;
  isJoined: Boolean;
begin
  if group.endType = etPolygon then
  begin
    // the lowermost polygon must be an outer polygon. So we can use that as the
    // designated orientation for outer polygons (needed for tidy-up clipping)
    lowestIdx := GetLowestPolygonIdx(group.paths);
    if lowestIdx < 0 then Exit;
    // nb: don't use the default orientation here ...
    area := Clipper.Core.Area(group.paths[lowestIdx]);
    //if area = 0 then Exit; // this is probably unhelpful (#430)
    group.reversed := (area < 0);
    if group.reversed then fGroupDelta := -fDelta
    else fGroupDelta := fDelta;
  end else
  begin
    group.reversed := false;
    fGroupDelta := Abs(fDelta) * 0.5;
  end;
  fAbsGrpDelta := Abs(fGroupDelta);
  fJoinType := group.joinType;
  fEndType := group.endType;

  // calculate a sensible number of steps (for 360 deg for the given offset
  if (group.joinType = jtRound) or (group.endType = etRound) then
  begin
		// arcTol - when fArcTolerance is undefined (0), the amount of
		// curve imprecision that's allowed is based on the size of the
		// offset (delta). Obviously very large offsets will almost always
		// require much less precision. See also offset_triginometry2.svg
    if fArcTolerance > 0.01 then
      arcTol := Min(fAbsGrpDelta, fArcTolerance) else
      arcTol := Log10(2 + fAbsGrpDelta) * 0.25; // empirically derived
    //http://www.angusj.com/clipper2/Docs/Trigonometry.htm
    stepsPer360 := Pi / ArcCos(1 - arcTol / fAbsGrpDelta);
		if (stepsPer360 > fAbsGrpDelta * Pi) then
			stepsPer360 := fAbsGrpDelta * Pi;  // avoid excessive precision
    fStepSin := sin(TwoPi/stepsPer360);
    fStepCos := cos(TwoPi/stepsPer360);
		if (fGroupDelta < 0.0) then fStepSin := -fStepSin;
    fStepsPerRad := stepsPer360 / TwoPi;
  end;

  fOutPaths := nil;
  isJoined := fEndType in [etPolygon, etJoined];
  for i := 0 to High(group.paths) do
  begin
    fInPath := StripDuplicates(group.paths[i], IsJoined);
    len := Length(fInPath);
    if (len = 0) or ((len < 3) and (fEndType = etPolygon)) then
      Continue;

    fNorms := nil;
    fOutPath := nil;
    fOutPathLen := 0;

		//if a single vertex then build a circle or a square ...
    if len = 1 then
    begin
      if fGroupDelta < 1 then Continue;
      if (group.endType = etRound) then
      begin
        r := fAbsGrpDelta;
        with fInPath[0] do
        begin
          fOutPath := Path64(Ellipse(RectD(X-r, Y-r, X+r, Y+r)));
{$IFDEF USINGZ}
          for j := 0 to high(fOutPath) do
            fOutPath[j].Z := Z;
{$ENDIF}
        end;
      end else
      begin
        j := Round(fGroupDelta);
        with fInPath[0] do
        begin
          rec := Rect64(X -j, Y -j, X+j, Y+j);
          fOutPath := rec.AsPath;
{$IFDEF USINGZ}
          for j := 0 to high(fOutPath) do
            fOutPath[j].Z := Z;
{$ENDIF}
        end
      end;
      AppendPath(fOutPaths, fOutPath);
      Continue;
    end else
    begin
      if (len = 2) and (group.endType = etJoined) then
      begin
        if fJoinType = jtRound then
          fEndType := etRound else
          fEndType := etSquare;
      end;

      BuildNormals;
      if fEndType = etPolygon then OffsetPolygon
      else if fEndType = etJoined then OffsetOpenJoined
      else OffsetOpenPath;
    end;

    if fOutPathLen = 0 then Continue;
    SetLength(fOutPath, fOutPathLen);
    AppendPath(fOutPaths, fOutPath);
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

  // Rebuild normals // BuildNormals;
  fNorms := ReversePath(fNorms);
  fNorms := ShiftPath(fNorms, 1);
  fNorms := NegatePath(fNorms);

  OffsetPolygon;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenPath;
var
  i, k, highI: integer;
begin
  highI := high(fInPath);

 // do the line start cap
  case fEndType of
    etButt:
      begin
{$IFDEF USINGZ}
        with fInPath[0] do AddPoint(Point64(
          X - fNorms[0].X * fGroupDelta,
          Y - fNorms[0].Y * fGroupDelta,
          Z));
{$ELSE}
        with fInPath[0] do AddPoint(Point64(
          X - fNorms[0].X * fGroupDelta,
          Y - fNorms[0].Y * fGroupDelta));
{$ENDIF}
        AddPoint(GetPerpendic(fInPath[0], fNorms[0], fGroupDelta));
      end;
    etRound: DoRound(0,0, PI);
    else DoSquare(0, 0);
  end;

  // offset the left side going forward
  k := 0;
  for i := 1 to highI -1 do //nb: -1 is important
    OffsetPoint(i, k);

  // reverse the normals ...
  for i := HighI downto 1 do
  begin
    fNorms[i].X := -fNorms[i-1].X;
    fNorms[i].Y := -fNorms[i-1].Y;
  end;
  fNorms[0] := fNorms[highI];

 // do the line end cap
  case fEndType of
    etButt:
      begin
{$IFDEF USINGZ}
        with fInPath[highI] do AddPoint(Point64(
          X - fNorms[highI].X *fGroupDelta,
          Y - fNorms[highI].Y *fGroupDelta,
          Z));
{$ELSE}
        with fInPath[highI] do AddPoint(Point64(
          X - fNorms[highI].X *fGroupDelta,
          Y - fNorms[highI].Y *fGroupDelta));
{$ENDIF}
        AddPoint(GetPerpendic(fInPath[highI], fNorms[highI], fGroupDelta));
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

procedure TClipperOffset.ExecuteInternal(delta: Double);
var
  i: integer;
  group: TGroup;
begin
  fSolution := nil;
  if fGroupList.Count = 0 then Exit;

  fMinLenSqrd := 1;
  if abs(delta) < Tolerance then
  begin
    // if delta == 0, just copy paths to Result
    for i := 0 to fGroupList.Count -1 do
    begin
      group := TGroup(fGroupList[i]);
      AppendPaths(fSolution, group.paths);
    end;
    Exit;
  end;

  fDelta := delta;
  // Miter Limit: see offset_triginometry3.svg
  if fMiterLimit > 1 then
    fTmpLimit := 2 / Sqr(fMiterLimit) else
    fTmpLimit := 2.0;

  // nb: delta will depend on whether paths are polygons or open
  for i := 0 to fGroupList.Count -1 do
  begin
    group := TGroup(fGroupList[i]);
    DoGroupOffset(group);
  end;

  // clean up self-intersections ...
  with TClipper64.Create do
  try
    PreserveCollinear := fPreserveCollinear;
    // the solution should retain the orientation of the input
    ReverseSolution :=
      fReverseSolution <> TGroup(fGroupList[0]).reversed;
    AddSubject(fSolution);
    if TGroup(fGroupList[0]).reversed then
      Execute(ctUnion, frNegative, fSolution) else
      Execute(ctUnion, frPositive, fSolution);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Execute(delta: Double; out solution: TPaths64);
begin
  fSolution := nil;
  solution := nil;
  ExecuteInternal(delta);
  if fGroupList.Count = 0 then Exit;

  // clean up self-intersections ...
  with TClipper64.Create do
  try
    PreserveCollinear := fPreserveCollinear;
    // the solution should retain the orientation of the input
    ReverseSolution :=
      fReverseSolution <> TGroup(fGroupList[0]).reversed;
    AddSubject(fSolution);
    if TGroup(fGroupList[0]).reversed then
      Execute(ctUnion, frNegative, solution) else
      Execute(ctUnion, frPositive, solution);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Execute(delta: Double; polytree: TPolyTree64);
var
  dummy: TPaths64;
begin
  fSolution := nil;
  if not Assigned(polytree) then
    Raise EClipper2LibException(rsClipper_PolyTreeErr);

  ExecuteInternal(delta);

  // clean up self-intersections ...
  with TClipper64.Create do
  try
    PreserveCollinear := fPreserveCollinear;
    // the solution should retain the orientation of the input
    ReverseSolution :=
      fReverseSolution <> TGroup(fGroupList[0]).reversed;
    AddSubject(fSolution);
    if TGroup(fGroupList[0]).reversed then
      Execute(ctUnion, frNegative, polytree, dummy) else
      Execute(ctUnion, frPositive, polytree, dummy);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
procedure TClipperOffset.AddPoint(x,y: double; z: Int64);
{$ELSE}
procedure TClipperOffset.AddPoint(x,y: double);
{$ENDIF}
const
  BuffLength = 32;
var
  pt: TPoint64;
begin
{$IFDEF USINGZ}
  pt := Point64(Round(x),Round(y), z);
{$ELSE}
  pt := Point64(Round(x),Round(y));
{$ENDIF}
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
{$IFDEF USINGZ}
  AddPoint(pt.X, pt.Y, pt.Z);
{$ELSE}
  AddPoint(pt.X, pt.Y);
{$ENDIF}
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
{$IFDEF USINGZ}
  Result.Z := pt.Z;
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoSquare(j, k: Integer);
var
  vec, pt1,pt2,pt3,pt4, pt,ptQ : TPointD;
begin
  if k = j then
  begin
    vec.X := fNorms[0].Y;     //squaring a line end
    vec.Y := -fNorms[0].X;
  end else
  begin
    // using the reciprocal of unit normals (as unit vectors)
    // get the average unit vector ...
    vec := GetAvgUnitVector(
      PointD(-fNorms[k].Y, fNorms[k].X),
      PointD(fNorms[j].Y, -fNorms[j].X));
  end;

  // now offset the original vertex delta units along unit vector
  ptQ := PointD(fInPath[j]);
  ptQ := TranslatePoint(ptQ, fAbsGrpDelta * vec.X, fAbsGrpDelta * vec.Y);

  // get perpendicular vertices
  pt1 := TranslatePoint(ptQ, fGroupDelta * vec.Y, fGroupDelta * -vec.X);
  pt2 := TranslatePoint(ptQ, fGroupDelta * -vec.Y, fGroupDelta * vec.X);

  // get 2 vertices along one edge offset
  pt3 := GetPerpendicD(fInPath[k], fNorms[k], fGroupDelta);

  if (j = k) then
  begin
    pt4.X := pt3.X + vec.X * fGroupDelta;
    pt4.Y := pt3.Y + vec.Y * fGroupDelta;
    // get the intersection point
    pt := IntersectPoint(pt1, pt2, pt3, pt4);
{$IFDEF USINGZ}
    with ReflectPoint(pt, ptQ) do AddPoint(X, Y, Z);
    AddPoint(pt.X, pt.Y, pt.Z);
{$ELSE}
    with ReflectPoint(pt, ptQ) do AddPoint(X, Y);
    AddPoint(pt.X, pt.Y);
{$ENDIF}
  end else
  begin
    pt4 := GetPerpendicD(fInPath[j], fNorms[k], fGroupDelta);
    // get the intersection point
    pt := IntersectPoint(pt1, pt2, pt3, pt4);
{$IFDEF USINGZ}
    AddPoint(pt.X, pt.Y, ptQ.Z);
    //get the second intersect point through reflecion
    with ReflectPoint(pt, ptQ) do AddPoint(X, Y, ptQ.Z);
{$ELSE}
    AddPoint(pt.X, pt.Y);
    //get the second intersect point through reflecion
    with ReflectPoint(pt, ptQ) do AddPoint(X, Y);
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoMiter(j, k: Integer; cosA: Double);
var
  q: Double;
begin
  // see offset_triginometry4.svg
  q := fGroupDelta / (cosA +1);
{$IFDEF USINGZ}
  AddPoint(fInPath[j].X + (fNorms[k].X + fNorms[j].X)*q,
    fInPath[j].Y + (fNorms[k].Y + fNorms[j].Y)*q,
    fInPath[j].Z);
{$ELSE}
  AddPoint(fInPath[j].X + (fNorms[k].X + fNorms[j].X)*q,
    fInPath[j].Y + (fNorms[k].Y + fNorms[j].Y)*q);
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoRound(j, k: Integer; angle: double);
var
  i, steps: Integer;
  pt: TPoint64;
  offDist: TPointD;
begin
	// nb: angles may be negative but this will always be a convex join
  pt := fInPath[j];
  offDist := ScalePoint(fNorms[k], fGroupDelta);
  if j = k then  offDist := Negate(offDist);
{$IFDEF USINGZ}
  AddPoint(pt.X + offDist.X, pt.Y + offDist.Y, pt.Z);
{$ELSE}
  AddPoint(pt.X + offDist.X, pt.Y + offDist.Y);
{$ENDIF}
  steps := Ceil(fStepsPerRad * abs(angle)); // #448, #456
  for i := 2 to steps do
  begin
    offDist := PointD(offDist.X * fStepCos - fStepSin * offDist.Y,
      offDist.X * fStepSin + offDist.Y * fStepCos);
{$IFDEF USINGZ}
    AddPoint(pt.X + offDist.X, pt.Y + offDist.Y, pt.Z);
{$ELSE}
    AddPoint(pt.X + offDist.X, pt.Y + offDist.Y);
{$ENDIF}
  end;
  AddPoint(GetPerpendic(pt, fNorms[j], fGroupDelta));
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPoint(j: Integer; var k: integer);
var
  sinA, cosA: Double;
begin
  if PointsEqual(fInPath[j], fInPath[k]) then
  begin
    k := j;
    Exit;
  end;

  // Let A = change in angle where edges join
  // A == 0: ie no change in angle (flat join)
  // A == PI: edges 'spike'
  // sin(A) < 0: right turning
  // cos(A) < 0: change in angle is more than 90 degree
  sinA := CrossProduct(fNorms[k], fNorms[j]);
  cosA := DotProduct(fNorms[j], fNorms[k]);
  if (sinA > 1.0) then sinA := 1.0
  else if (sinA < -1.0) then sinA := -1.0;


  if (cosA > -0.99) and (sinA * fGroupDelta < 0) then
  begin
    // is concave
    AddPoint(GetPerpendic(fInPath[j], fNorms[k], fGroupDelta));
    // this extra point is the only (simple) way to ensure that
    // path reversals are fully cleaned with the trailing clipper
    AddPoint(fInPath[j]); // (#405)
    AddPoint(GetPerpendic(fInPath[j], fNorms[j], fGroupDelta));
  end
  else if (fJoinType = jtMiter) then
  begin
    // miter unless the angle is so acute the miter would exceeds ML
    if (cosA > fTmpLimit -1) then DoMiter(j, k, cosA)
    else DoSquare(j, k);
  end
  else if (cosA > 0.9998) then
		// almost straight - less than 1 degree (#424)
    DoMiter(j, k, cosA)
  else if (cosA > 0.99) or (fJoinType = jtSquare) then
		//angle less than 8 degrees or squared joins
    DoSquare(j, k)
  else
    DoRound(j, k, ArcTan2(sinA, cosA));

  k := j;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
