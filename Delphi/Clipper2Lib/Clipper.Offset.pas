unit Clipper.Offset;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  15 November 2023                                                *
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

  TJoinType = (jtMiter, jtSquare, jtBevel, jtRound);
  //jtSquare: Joins are 'squared' at exactly the offset distance (complex code)
  //jtBevel : offset distances vary depending on the angle (simple code, faster)

  TEndType = (etPolygon, etJoined, etButt, etSquare, etRound);
  // etButt   : offsets both sides of a path, with square blunt ends
  // etSquare : offsets both sides of a path, with square extended ends
  // etRound  : offsets both sides of a path, with round extended ends
  // etJoined : offsets both sides of a path, with joined ends
  // etPolygon: offsets only one side of a closed path

  TDeltaCallback64 = function (const path: TPath64;
    const path_norms: TPathD; currIdx, prevIdx: integer): double of object;

  TRect64Array = array of TRect64;

  TGroup = class
	  paths     : TPaths64;
	  joinType  : TJoinType;
	  endType   : TEndType;
    reversed  : Boolean;
    lowestPathIdx: integer;
    boundsList: TRect64Array;
    constructor Create(const pathsIn: TPaths64; jt: TJoinType; et: TEndType);
  end;

  TClipperOffset = class
  private
    fDelta        : Double;
    fGroupDelta   : Double; //*0.5 for open paths; *-1.0 for neg areas
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
    fOutPathLen   : Integer;
    fSolution     : TPaths64;
    fSolutionLen  : Integer;
    fSolutionTree : TPolyTree64;
    fPreserveCollinear  : Boolean;
    fReverseSolution    : Boolean;
    fDeltaCallback64    : TDeltaCallback64;
{$IFDEF USINGZ}
    fZCallback64 : TZCallback64;
    procedure AddPoint(x,y: double; z: Int64); overload;
{$ELSE}
    procedure AddPoint(x,y: double); overload;
{$ENDIF}
    procedure AddPoint(const pt: TPoint64); overload;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure DoSquare(j, k: Integer);
    procedure DoBevel(j, k: Integer);
    procedure DoMiter(j, k: Integer; cosA: Double);
    procedure DoRound(j, k: integer; angle: double);
    procedure OffsetPoint(j: Integer; var k: integer);

    procedure BuildNormals;
    procedure DoGroupOffset(group: TGroup);
    procedure OffsetPolygon;
    procedure OffsetOpenJoined;
    procedure OffsetOpenPath;
    function CalcSolutionCapacity: integer;
    procedure UpdateSolution; {$IFDEF INLINING} inline; {$ENDIF}

    function CheckReverseOrientation: Boolean;
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
    procedure Execute(DeltaCallback: TDeltaCallback64; out solution: TPaths64); overload;

    // MiterLimit: needed for mitered offsets (see offset_triginometry3.svg)
    property MiterLimit: Double read fMiterLimit write fMiterLimit;
    // ArcTolerance: needed for rounded offsets (See offset_triginometry2.svg)
    property ArcTolerance: Double read fArcTolerance write fArcTolerance;
    property PreserveCollinear: Boolean
      read fPreserveCollinear write fPreserveCollinear;
    property ReverseSolution: Boolean
      read fReverseSolution write fReverseSolution;
    property DeltaCallback: TDeltaCallback64 read
      fDeltaCallback64 write fDeltaCallback64;
{$IFDEF USINGZ}
    property ZCallback: TZCallback64 read fZCallback64 write fZCallback64;
{$ENDIF}
  end;

implementation

uses
  Math;

resourcestring
  rsClipper_CoordRangeError =
    'Offsetting will exceed the valid coordinate range';

const
  TwoPi     : Double = 2 * PI;
  InvTwoPi  : Double = 1/(2 * PI);

//------------------------------------------------------------------------------
//  Miscellaneous offset support functions
//------------------------------------------------------------------------------

function GetMultiBounds(const paths: TPaths64; endType: TEndType): TRect64Array;
var
  i,j, len, len2, minPathLen: integer;
  path: TPath64;
  pt1, pt: TPoint64;
  r: TRect64;
begin
  if endType = etPolygon then
	  minPathLen := 3 else
    minPathLen := 1;
  len := Length(paths);
  SetLength(Result, len);
	for i := 0 to len -1 do
	begin
    path := paths[i];
    len2 := Length(path);
		if len2 < minPathLen then
		begin
      Result[i] := InvalidRect64;
			continue;
    end;
    pt1 := path[0];
    r := Rect64(pt1.X, pt1.Y, pt1.X, pt1.Y);
	  for j := 1 to len2 -1 do
    begin
      pt := path[j];
			if (pt.y > r.bottom) then r.bottom := pt.y
			else if (pt.y < r.top) then r.top := pt.y;
			if (pt.x > r.right) then r.right := pt.x
			else if (pt.x < r.left) then r.left := pt.x;
    end;
    Result[i] := r;
	end;
end;
//------------------------------------------------------------------------------

function ValidateBounds(const boundsList: TRect64Array; delta: double): Boolean;
var
  i: integer;
  iDelta, big, small: Int64;
begin
  Result := false;
	iDelta := Round(delta);
  big := MaxCoord - iDelta;
  small := MinCoord + iDelta;
	for i := 0 to High(boundsList) do
    with boundsList[i] do
    begin
      if not IsValid then continue; // skip invalid paths
      if (left < small) or (right > big) or
        (top < small) or (bottom > big) then Exit;
    end;
  Result := true;
end;
//------------------------------------------------------------------------------

function GetLowestClosedPathIdx(const boundsList: TRect64Array): integer;
var
  i: integer;
  botPt: TPoint64;
begin
	Result := -1;
	botPt := Point64(MaxInt64, MinInt64);
	for i := 0 to High(boundsList) do
    with boundsList[i] do
    begin
      if not IsValid or IsEmpty then Continue;
      if (bottom > botPt.y) or
        ((bottom = botPt.Y) and (left < botPt.X)) then
      begin
        botPt := Point64(left, bottom);
        Result := i;
      end;
    end;
end;
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

constructor TGroup.Create(const pathsIn: TPaths64; jt: TJoinType; et: TEndType);
var
  i, len: integer;
  isJoined: boolean;
begin
  Self.joinType := jt;
  Self.endType := et;

  isJoined := et in [etPolygon, etJoined];
  len := Length(pathsIn);
  SetLength(paths, len);
  for i := 0 to len -1 do
    paths[i] := StripDuplicates(pathsIn[i], isJoined);

	boundsList := GetMultiBounds(paths, et);
  if (et = etPolygon) then
  begin
	  lowestPathIdx := GetLowestClosedPathIdx(boundsList);
    reversed := (lowestPathIdx >= 0) and (
      Area(pathsIn[lowestPathIdx]) < 0);
  end else
  begin
    lowestPathIdx := -1;
    reversed := false;
  end;
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
  fSolutionLen := 0;
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
  group := TGroup.Create(paths, joinType, endType);
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
  i,j, len, steps: Integer;
  r, stepsPer360, arcTol: Double;
  absDelta: double;
  rec: TRect64;
  pt0: TPoint64;
begin

  if group.endType = etPolygon then
  begin
    if (group.lowestPathIdx < 0) then Exit;
		//if (area == 0) return; // probably unhelpful (#430)
    if group.reversed then
      fGroupDelta := -fDelta else
      fGroupDelta := fDelta;
  end else
  begin
    fGroupDelta := Abs(fDelta);// * 0.5;
  end;

  absDelta := Abs(fGroupDelta);
	if not ValidateBounds(group.boundsList, absDelta) then
    Raise EClipper2LibException(rsClipper_CoordRangeError);

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
      arcTol := Min(absDelta, fArcTolerance) else
      arcTol := Log10(2 + absDelta) * 0.25; // empirically derived
    //http://www.angusj.com/clipper2/Docs/Trigonometry.htm
    stepsPer360 := Pi / ArcCos(1 - arcTol / absDelta);
		if (stepsPer360 > absDelta * Pi) then
			stepsPer360 := absDelta * Pi;  // avoid excessive precision
    fStepSin := sin(TwoPi/stepsPer360);
    fStepCos := cos(TwoPi/stepsPer360);
		if (fGroupDelta < 0.0) then fStepSin := -fStepSin;
    fStepsPerRad := stepsPer360 / TwoPi;
  end;

  for i := 0 to High(group.paths) do
  begin
    if not group.boundsList[i].IsValid then Continue;

    fInPath := group.paths[i];
    fNorms := nil;

		//if a single vertex then build a circle or a square ...
    len := Length(fInPath);
    if len = 1 then
    begin
      if fGroupDelta < 1 then Continue;
      pt0 := fInPath[0];
      if (group.endType = etRound) then
      begin
        r := absDelta;
        steps := Ceil(fStepsPerRad * TwoPi); //#617
        fOutPath := Path64(Ellipse(
          RectD(pt0.X-r, pt0.Y-r, pt0.X+r, pt0.Y+r), steps));
{$IFDEF USINGZ}
        for j := 0 to high(fOutPath) do
          fOutPath[j].Z := pt0.Z;
{$ENDIF}
      end else
      begin
        j := Round(absDelta);
        rec := Rect64(pt0.X -j, pt0.Y -j, pt0.X+j, pt0.Y+j);
        fOutPath := rec.AsPath;
{$IFDEF USINGZ}
        for j := 0 to high(fOutPath) do
          fOutPath[j].Z := pt0.Z;
{$ENDIF}
      end;
      UpdateSolution;
      Continue;
    end;

		// when shrinking, then make sure the path can shrink that far
    if (fGroupDelta < 0) and
      (Min(group.boundsList[i].Width, group.boundsList[i].Height) <
        fGroupDelta *2) then Continue;

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

procedure TClipperOffset.UpdateSolution;
begin
  if fOutPathLen = 0 then Exit;
  SetLength(fOutPath, fOutPathLen);
  fSolution[fSolutionLen] := fOutPath;
  inc(fSolutionLen);
  fOutPath := nil;
  fOutPathLen := 0;
end;
//------------------------------------------------------------------------------

function TClipperOffset.CalcSolutionCapacity: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to fGroupList.Count -1 do
    with TGroup(fGroupList[i]) do
    if endType = etJoined then
      inc(Result, Length(paths) *2) else
      inc(Result, Length(paths));
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPolygon;
var
  i,j: integer;
begin
  j := high(fInPath);
  for i := 0 to high(fInPath) do
    OffsetPoint(i, j);
  UpdateSolution;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenJoined;
begin
  OffsetPolygon;
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

  if Assigned(fDeltaCallback64) then
    fGroupDelta := fDeltaCallback64(fInPath, fNorms, 0, 0);

  // do the line start cap
  if Abs(fGroupDelta) < Tolerance then
  begin
    AddPoint(fInPath[0]);
  end else
  case fEndType of
    etButt: DoBevel(0, 0);
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

 if Assigned(fDeltaCallback64) then
    fGroupDelta := fDeltaCallback64(fInPath, fNorms, highI, highI);
  if Abs(fGroupDelta) < Tolerance then
  begin
    AddPoint(fInPath[highI]);
  end else
  case fEndType of
    etButt: DoBevel(highI, highI);
    etRound: DoRound(highI,highI, PI);
    else DoSquare(highI, highI);
  end;

  // offset the left side going back
  k := 0;
  for i := highI downto 1 do //and stop at 1!
    OffsetPoint(i, k);

  UpdateSolution;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.ExecuteInternal(delta: Double);
var
  i,j: integer;
  group: TGroup;
  pathsReversed: Boolean;
  fillRule: TFillRule;
  dummy: TPaths64;
begin
  fSolution := nil;
  fSolutionLen := 0;
  if fGroupList.Count = 0 then Exit;
  SetLength(fSolution, CalcSolutionCapacity);

  fMinLenSqrd := 1;
  if abs(delta) < Tolerance then
  begin
    // if delta == 0, just copy paths to Result
    for i := 0 to fGroupList.Count -1 do
    begin
      group := TGroup(fGroupList[i]);
      for j := 0 to High(group.paths) do
      begin
        fSolution[fSolutionLen] := group.paths[i];
        inc(fSolutionLen);
      end;
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
  SetLength(fSolution, fSolutionLen);

  pathsReversed := CheckReverseOrientation();
  if pathsReversed then
    fillRule := frNegative else
    fillRule := frPositive;

  // clean up self-intersections ...
  with TClipper64.Create do
  try
    PreserveCollinear := fPreserveCollinear;
    // the solution should retain the orientation of the input
    ReverseSolution := fReverseSolution <> pathsReversed;
    AddSubject(fSolution);
    if assigned(fSolutionTree) then
      Execute(ctUnion, fillRule, fSolutionTree, dummy);
      Execute(ctUnion, fillRule, fSolution);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function TClipperOffset.CheckReverseOrientation: Boolean;
var
  i: integer;
begin
  Result := false;
  // find the orientation of the first closed path
  for i := 0 to fGroupList.Count -1 do
    with TGroup(fGroupList[i]) do
      if endType = etPolygon then
      begin
        Result := reversed;
        break;
      end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Execute(delta: Double; out solution: TPaths64);
begin
  solution := nil;
  fSolutionTree := nil;
  if fGroupList.Count = 0 then Exit;
  ExecuteInternal(delta);
  solution := fSolution;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Execute(DeltaCallback: TDeltaCallback64; out solution: TPaths64);
begin
  fDeltaCallback64 := DeltaCallback;
  Execute(1.0, solution);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Execute(delta: Double; polytree: TPolyTree64);
begin
  if not Assigned(polytree) then
    Raise EClipper2LibException(rsClipper_PolyTreeErr);
  fSolutionTree := polytree;
  fSolutionTree.Clear;
  ExecuteInternal(delta);
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

procedure TClipperOffset.DoBevel(j, k: Integer);
var
  absDelta: double;
begin
  if k = j then
  begin
		absDelta :=  abs(fGroupDelta);
		AddPoint(
      fInPath[j].x - absDelta * fNorms[j].x,
      fInPath[j].y - absDelta * fNorms[j].y);
		AddPoint(
      fInPath[j].x + absDelta * fNorms[j].x,
      fInPath[j].y + absDelta * fNorms[j].y);
  end else
  begin
		AddPoint(
      fInPath[j].x + fGroupDelta * fNorms[k].x,
      fInPath[j].y + fGroupDelta * fNorms[k].y);
		AddPoint(
      fInPath[j].x + fGroupDelta * fNorms[j].x,
      fInPath[j].y + fGroupDelta * fNorms[j].y);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoSquare(j, k: Integer);
var
  vec, pt1,pt2,pt3,pt4, pt,ptQ : TPointD;
  absDelta: double;
begin
  if k = j then
  begin
    vec.X := fNorms[j].Y;     //squaring a line end
    vec.Y := -fNorms[j].X;
  end else
  begin
    // using the reciprocal of unit normals (as unit vectors)
    // get the average unit vector ...
    vec := GetAvgUnitVector(
      PointD(-fNorms[k].Y, fNorms[k].X),
      PointD(fNorms[j].Y, -fNorms[j].X));
  end;

  absDelta := Abs(fGroupDelta);
  // now offset the original vertex delta units along unit vector
  ptQ := PointD(fInPath[j]);
  ptQ := TranslatePoint(ptQ, absDelta * vec.X, absDelta * vec.Y);

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
  absDelta, arcTol, stepsPer360: double;
  pt: TPoint64;
  offDist: TPointD;
begin

  if Assigned(fDeltaCallback64) then
  begin
    // when fDeltaCallback64 is assigned, fGroupDelta won't be constant,
    // so we'll need to do the following calculations for *every* vertex.
    absDelta := Abs(fGroupDelta);
    if fArcTolerance > 0.01 then
      arcTol := Min(absDelta, fArcTolerance) else
      arcTol := Log10(2 + absDelta) * 0.25; // empirically derived
    //http://www.angusj.com/clipper2/Docs/Trigonometry.htm
    stepsPer360 := Pi / ArcCos(1 - arcTol / absDelta);
		if (stepsPer360 > absDelta * Pi) then
			stepsPer360 := absDelta * Pi;  // avoid excessive precision
    fStepSin := sin(TwoPi/stepsPer360);
    fStepCos := cos(TwoPi/stepsPer360);
		if (fGroupDelta < 0.0) then fStepSin := -fStepSin;
    fStepsPerRad := stepsPer360 / TwoPi;
  end;

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

  if Assigned(fDeltaCallback64) then
  begin
    fGroupDelta := fDeltaCallback64(fInPath, fNorms, j, k);
    if TGroup(fGroupList[0]).reversed then fGroupDelta := -fGroupDelta;
  end;

  if Abs(fGroupDelta) <= Tolerance then
  begin
    AddPoint(fInPath[j]);
    Exit;
  end;

  //test for concavity first (#593)
  if (cosA > -0.99) and (sinA * fGroupDelta < 0) then
  begin
    // is concave
    AddPoint(GetPerpendic(fInPath[j], fNorms[k], fGroupDelta));
    // this extra point is the only (simple) way to ensure that
    // path reversals are fully cleaned with the trailing clipper
    AddPoint(fInPath[j]); // (#405)
    AddPoint(GetPerpendic(fInPath[j], fNorms[j], fGroupDelta));
  end
  else if (cosA > 0.999) then
    // almost straight - less than 2.5 degree (#424, #526)
    DoMiter(j, k, cosA)
  else if (fJoinType = jtMiter) then
  begin
    // miter unless the angle is so acute the miter would exceeds ML
    if (cosA > fTmpLimit -1) then DoMiter(j, k, cosA)
    else DoSquare(j, k);
  end
  else if (cosA > 0.99) or (fJoinType = jtBevel) then
		// ie > 2.5 deg (see above) but less than ~8 deg ( acos(0.99) )
    DoBevel(j, k)
  else if (fJoinType = jtRound) then
    DoRound(j, k, ArcTan2(sinA, cosA))
  else
    DoSquare(j, k);

  k := j;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
