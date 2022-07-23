unit Clipper.Offset;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - beta                                                 *
* Date      :  23 July 2022                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Offset paths and clipping solutions                             *
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

  TPathGroup = class
	  paths     : TPaths64;
    reversed  : Boolean;
	  joinType  : TJoinType;
	  endType   : TEndType;
    constructor Create(jt: TJoinType; et: TEndType);
  end;

  TClipperOffset = class
  private
    fDelta       : Double;
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
    procedure DoGroupOffset(pathGroup: TPathGroup; delta: double);
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
  result := (vec1.X * vec2.X + vec1.Y * vec2.Y);
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
			if (p[j].Y < lp.Y) then continue
      else if ((p[j].Y > lp.Y) or (p[j].X < lp.X)) then
      begin
				Result := i;
				lp := p[j];
			end;
  end;
end;
//------------------------------------------------------------------------------

function CopyPaths(const paths: TPathsD): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Copy(paths[i], 0, Length(paths[i]));
end;

//------------------------------------------------------------------------------
// TPathGroup methods
//------------------------------------------------------------------------------

constructor TPathGroup.Create(jt: TJoinType; et: TEndType);
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
    TPathGroup(fInGroups[i]).Free;
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
  group: TPathGroup;
begin
  if Length(paths) = 0 then Exit;
  group := TPathGroup.Create(joinType, endType);
  AppendPaths(group.paths, paths);
  fInGroups.Add(group);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoGroupOffset(pathGroup: TPathGroup; delta: double);
var
  i, len, lowestIdx: Integer;
  r, absDelta, arcTol, area, steps: Double;
  IsClosedPaths: Boolean;
begin
  if pathgroup.endType <> etPolygon then
    delta := Abs(delta) * 0.5;

  IsClosedPaths := (pathgroup.endType in [etPolygon, etJoined]);
  if IsClosedPaths then
  begin
    // the lowermost polygon must be an outer polygon. So we can use that as the
    // designated orientation for outer polygons (needed for tidy-up clipping)
    lowestIdx := GetLowestPolygonIdx(pathgroup.paths);
    if lowestIdx < 0 then Exit;
    // nb: don't use the default orientation here ...
    area := Clipper.Core.Area(pathgroup.paths[lowestIdx]);
    if area = 0 then Exit;
    pathgroup.reversed := (area < 0);
    if pathgroup.reversed then delta := -delta;
  end else
    pathgroup.reversed := false;

  fDelta := delta;
  absDelta := Abs(fDelta);
  fJoinType := pathGroup.joinType;

  if fArcTolerance > 0 then
    arcTol := fArcTolerance else
    arcTol := Log10(2 + absDelta) * 0.25; // empirically derived

  // calculate a sensible number of steps (for 360 deg for the given offset
  if (pathgroup.joinType = jtRound) or (pathgroup.endType = etRound) then
  begin
    // get steps per 180 degrees (see offset_triginometry2.svg)
    steps := PI / ArcCos(1 - arcTol / absDelta);
    fStepsPerRad := steps  * InvTwoPi;
  end;

  fOutPaths := nil;
  for i := 0 to High(pathgroup.paths) do
  begin
    fInPath := StripDuplicates(pathgroup.paths[i], IsClosedPaths);
    len := Length(fInPath);
    if (fInPath = nil) or
      ((pathGroup.endType in [etPolygon, etJoined]) and (len < 3)) then Continue;

    fNorms := nil;
    fOutPath := nil;
    fOutPathLen := 0;

		//if a single vertex then build a circle or a square ...
    if len = 1 then
    begin
      if (pathgroup.endType = etRound) then
      begin
        r := absDelta;
				if (pathGroup.endType = etPolygon) then
          r := r * 0.5;
        with fInPath[0] do
          fOutPath := Path64(Ellipse(RectD(X-r, Y-r, X+r, Y+r)));
      end else
      begin
        SetLength(fOutPath, 4);
        with fInPath[0] do
        begin
          fOutPath[0] := Point64(X-fDelta,Y-fDelta);
          fOutPath[1] := Point64(X+fDelta,Y-fDelta);
          fOutPath[2] := Point64(X+fDelta,Y+fDelta);
          fOutPath[3] := Point64(X-fDelta,Y+fDelta);
        end;
      end;
      AppendPath(fOutPaths, fOutPath);
      Continue;
    end else
    begin
      BuildNormals;
      if pathgroup.endType = etPolygon then
      begin
        OffsetPolygon;
      end
      else if pathgroup.endType = etJoined then
      begin
        OffsetOpenJoined;
      end else
        OffsetOpenPath(pathgroup.endType);
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
      ReverseSolution := fReverseSolution <> pathGroup.reversed;
      AddSubject(fOutPaths);
      if pathGroup.reversed then
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

  procedure DoButtEnd(highI: integer);
  begin
    AddPoint(fInPath[highI].X + fNorms[highI-1].X *fDelta,
      fInPath[highI].Y + fNorms[highI-1].Y * fDelta);
    AddPoint(fInPath[highI].X - fNorms[highI-1].X *fDelta,
      fInPath[highI].Y - fNorms[highI-1].Y * fDelta);
  end;

  procedure DoButtStart;
  begin
    AddPoint(fInPath[0].X + fNorms[1].X *fDelta,
      fInPath[0].Y + fNorms[1].Y * fDelta);
    AddPoint(fInPath[0].X - fNorms[1].X *fDelta,
      fInPath[0].Y - fNorms[1].Y * fDelta);
  end;

var
  i, k, highI: integer;
begin
  highI := high(fInPath);
  k := 0;
  for i := 1 to highI -1 do
    OffsetPoint(i, k);

  k := highI -1;
  fNorms[highI].X := -fNorms[k].X;
  fNorms[highI].Y := -fNorms[k].Y;

 // cap the end first ...
  case endType of
    etButt: DoButtEnd(highI);
    etRound: DoRound(highI, k, PI);
    else DoSquare(highI, k);
  end;

  // reverse normals ...
  for i := highI -1 downto 1 do
  begin
    fNorms[i].X := -fNorms[i-1].X;
    fNorms[i].Y := -fNorms[i-1].Y;
  end;
  fNorms[0].X := -fNorms[1].X;
  fNorms[0].Y := -fNorms[1].Y;
  k := highI;
  for i := highI -1 downto 1 do
    OffsetPoint(i, k);

  // now cap the start ...
  case endType of
    etButt: DoButtStart;
    etRound: DoRound(0, 1, PI);
    else doSquare(0, 1);
  end;
end;
//------------------------------------------------------------------------------

function TClipperOffset.Execute(delta: Double): TPaths64;
var
  i: integer;
  group: TPathGroup;
begin
  fSolution := nil;
  Result := nil;
  if fInGroups.Count = 0 then Exit;

  fMinLenSqrd := 1;
  if abs(delta) < Tolerance then
  begin
    // if delta ~= 0, just copy paths to Result
    for i := 0 to fInGroups.Count -1 do
      with TPathGroup(fInGroups[i]) do
          AppendPaths(fSolution, paths);
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
    group := TPathGroup(fInGroups[i]);
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
        fReverseSolution <> TPathGroup(fInGroups[0]).reversed;
      AddSubject(fSolution);
      if TPathGroup(fInGroups[0]).reversed then
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

procedure TClipperOffset.DoSquare(j, k: Integer);
begin
  // Two vertices, one using the prior offset's (k) normal one the current (j).
  // Do a 'normal' offset (by delta) and then another by 'de-normaling' the
  // normal hence parallel to the direction of the respective edges.
  if (fDelta > 0) then
  begin
    AddPoint(
      fInPath[j].X + fDelta * (fNorms[k].X - fNorms[k].Y),
      fInPath[j].Y + fDelta * (fNorms[k].Y + fNorms[k].X));

    AddPoint(
      fInPath[j].X + fDelta * (fNorms[j].X + fNorms[j].Y),
      fInPath[j].Y + fDelta * (fNorms[j].Y - fNorms[j].X));
  end else
  begin
    AddPoint(
      fInPath[j].X + fDelta * (fNorms[k].X + fNorms[k].Y),
      fInPath[j].Y + fDelta * (fNorms[k].Y - fNorms[k].X));
    AddPoint(
      fInPath[j].X + fDelta * (fNorms[j].X - fNorms[j].Y),
      fInPath[j].Y + fDelta * (fNorms[j].Y + fNorms[j].X));
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoMiter(j, k: Integer; cosAplus1: Double);
var
  q: Double;
begin
  // see offset_triginometry4.svg
  q := fDelta / cosAplus1;
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
	// even though angle may be negative this is a convex join
  pt := fInPath[j];
  pt2 := PointD(fNorms[k].X * fDelta, fNorms[k].Y * fDelta);
  AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);

  steps := Round(fStepsPerRad * abs(angle) + 0.501);
  GetSinCos(angle / steps, stepSin, stepCos);
  for i := 0 to steps -1 do
  begin
    pt2 := PointD(pt2.X * stepCos - stepSin * pt2.Y,
      pt2.X * stepSin + pt2.Y * stepCos);
    AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);
  end;
  pt2 := PointD(fNorms[j].X * fDelta, fNorms[j].Y * fDelta);
  AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPoint(j: Integer; var k: integer);
var
  sinA, cosA: Double;
  p1, p2: TPoint64;
begin
  // A: angle between adjoining edges (on left side WRT winding direction).
  // A == 0 deg (or A == 360 deg): collinear edges heading in same direction
  // A == 180 deg: collinear edges heading in opposite directions (ie a 'spike')
  // sin(A) < 0: convex on left.
  // cos(A) > 0: angles on both left and right sides > 90 degrees
  sinA := (fNorms[k].X * fNorms[j].Y - fNorms[j].X * fNorms[k].Y);

  if (sinA > 1.0) then sinA := 1.0
  else if (sinA < -1.0) then sinA := -1.0;

  if sinA * fDelta < 0 then // ie a concave offset
  begin
    p1 := Point64(
      fInPath[j].X + fNorms[k].X * fDelta,
      fInPath[j].Y + fNorms[k].Y * fDelta);
    p2:= Point64(
      fInPath[j].X + fNorms[j].X * fDelta,
      fInPath[j].Y + fNorms[j].Y * fDelta);
    AddPoint(p1);
    if not PointsEqual(p1, p2) then
    begin
      AddPoint(fInPath[j]); // this aids with clipping removal later
      AddPoint(p2);
    end;
  end else
  begin
    cosA := DotProduct(fNorms[j], fNorms[k]);
    // convex offsets here ...
    case fJoinType of
      jtMiter:
        // see offset_triginometry3.svg
        if (1 + cosA < fTmpLimit) then
          DoSquare(j, k) else
          DoMiter(j, k, 1 + cosA);
      jtSquare:
        begin
          // angles >= 90 deg. don't need squaring
          if cosA >= 0 then
            DoMiter(j, k, 1 + cosA) else
            DoSquare(j, k);
        end
      else
        DoRound(j, k, ArcTan2(sinA, cosA));
    end;
  end;
  k := j;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
