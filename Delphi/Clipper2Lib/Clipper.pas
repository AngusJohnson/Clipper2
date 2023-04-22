unit Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  21 April 2023                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* Purpose   :  This module provides a simple interface to the Clipper Library  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Math, SysUtils, Classes,
  Clipper.Core, Clipper.Engine, Clipper.Offset, Clipper.RectClip;

// Redeclare here a number of structures defined in
// other units so those units won't need to be declared
// just to use the following functions.
type
  TClipper    = Clipper.Engine.TClipper64;
  TClipper64  = Clipper.Engine.TClipper64;
  TPoint64    = Clipper.Core.TPoint64;
  TRect64     = Clipper.Core.TRect64;
  TPath64     = Clipper.Core.TPath64;
  TPaths64    = Clipper.Core.TPaths64;
  TPointD     = Clipper.Core.TPointD;
  TRectD      = Clipper.Core.TRectD;
  TPathD      = Clipper.Core.TPathD;
  TPathsD     = Clipper.Core.TPathsD;
  TFillRule   = Clipper.Core.TFillRule;
  TPolyTree64 = Clipper.Engine.TPolyTree64;
  TPolyTreeD  = Clipper.Engine.TPolyTreeD;
  TJoinType   = Clipper.Offset.TJoinType;
  TEndType    = Clipper.Offset.TEndType;

  TArrayOfInt64 = array of Int64;
const
  frEvenOdd   = Clipper.Core.frEvenOdd;
  frNonZero   = Clipper.Core.frNonZero;
  frPositive  = Clipper.Core.frPositive;
  frNegative  = Clipper.Core.frNegative;
  jtSquare    = Clipper.Offset.jtSquare;
  jtRound     = Clipper.Offset.jtRound;
  jtMiter     = Clipper.Offset.jtMiter;
  etPolygon   = Clipper.Offset.etPolygon;
  etJoined    = Clipper.Offset.etJoined;
  etButt      = Clipper.Offset.etButt;
  etSquare    = Clipper.Offset.etSquare;
  etRound     = Clipper.Offset.etRound;

  ctNone          = Clipper.Core.ctNone;
  ctIntersection  = Clipper.Core.ctIntersection;
  ctUnion         = Clipper.Core.ctUnion;
  ctDifference    = Clipper.Core.ctDifference;
  ctXor           = Clipper.Core.ctXor;

function BooleanOp(clipType: TClipType;
  const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64; overload;
function BooleanOp(clipType: TClipType; const subjects, clips:
  TPathsD; fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
procedure BooleanOp(clipType: TClipType; const subjects, clips: TPaths64;
  fillRule: TFillRule; polytree: TPolyTree64); overload;

function Intersect(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Union(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Union(const subjects: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Difference(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function XOR_(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;

function Intersect(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Union(const subjects: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Union(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Difference(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function XOR_(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;

function InflatePaths(const paths: TPaths64; delta: Double;
  jt: TJoinType = jtRound; et: TEndType = etPolygon;
  MiterLimit: double = 2.0; ArcTolerance: double = 0.0): TPaths64; overload;
function InflatePaths(const paths: TPathsD; delta: Double;
  jt: TJoinType = jtRound; et: TEndType = etPolygon;
  miterLimit: double = 2.0; precision: integer = 2;
  ArcTolerance: double = 0.0): TPathsD; overload;

// RectClip: for closed paths only (otherwise use RectClipLines)
//           much faster when only clipping convex polygons
function ExecuteRectClip(const rect: TRect64; const path: TPath64;
  convexOnly: Boolean = false): TPath64; overload;
function ExecuteRectClip(const rect: TRect64; const paths: TPaths64;
  convexOnly: Boolean = false): TPaths64; overload;
function ExecuteRectClip(const rect: TRectD; const path: TPathD;
  convexOnly: Boolean = false; precision: integer = 2): TPathD; overload;
function ExecuteRectClip(const rect: TRectD; const paths: TPathsD;
  convexOnly: Boolean = false; precision: integer = 2): TPathsD; overload;

function ExecuteRectClipLines(const rect: TRect64;
  const path: TPath64): TPaths64; overload;
function ExecuteRectClipLines(const rect: TRect64;
  const paths: TPaths64): TPaths64; overload;
function ExecuteRectClipLines(const rect: TRectD; const path: TPathD;
  precision: integer = 2): TPathsD; overload;
function ExecuteRectClipLines(const rect: TRectD; const paths: TPathsD;
  precision: integer = 2): TPathsD; overload;

function TranslatePath(const path: TPath64; dx, dy: Int64): TPath64; overload;
function TranslatePath(const path: TPathD; dx, dy: double): TPathD; overload;
function TranslatePaths(const paths: TPaths64; dx, dy: Int64): TPaths64; overload;
function TranslatePaths(const paths: TPathsD; dx, dy: double): TPathsD; overload;

function MinkowskiSum(const pattern, path: TPath64;
  pathIsClosed: Boolean): TPaths64; overload;
function MinkowskiSum(const pattern, path: TPathD;
  pathIsClosed: Boolean): TPathsD; overload;

function PolyTreeToPaths64(PolyTree: TPolyTree64): TPaths64;
function PolyTreeToPathsD(PolyTree: TPolyTreeD): TPathsD;

//ShowPolyTreeStructure: only useful when debugging
procedure ShowPolyTreeStructure(polytree: TPolyTree64; strings: TStrings); overload;
procedure ShowPolyTreeStructure(polytree: TPolyTreeD; strings: TStrings); overload;

function MakePath(const ints: array of Int64): TPath64; overload;
function MakePathD(const dbls: array of double): TPathD; overload;

function TrimCollinear(const p: TPath64;
  isOpenPath: Boolean = false): TPath64; overload;
function TrimCollinear(const path: TPathD;
  precision: integer; isOpenPath: Boolean = false): TPathD; overload;

function PointInPolygon(const pt: TPoint64; const polygon: TPath64):
  TPointInPolygonResult; {$IFDEF INLINE} inline; {$ENDIF}

function SimplifyPath(const path: TPath64;
  epsilon: double; isClosedPath: Boolean = false): TPath64;
  {$IFDEF INLINE} inline; {$ENDIF}
function SimplifyPaths(const paths: TPaths64;
  epsilon: double; isClosedPaths: Boolean = false): TPaths64;
  {$IFDEF INLINE} inline; {$ENDIF}

implementation

uses
  Clipper.Minkowski;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
function MakePath(const ints: array of Int64): TPath64;
var
  i, len: integer;
begin
  len := length(ints) div 3;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := ints[i*3];
    Result[i].Y := ints[i*3 +1];
    Result[i].z := ints[i*3 +2];
  end;
end;
//------------------------------------------------------------------------------

function MakePathD(const dbls: array of double): TPathD; overload;
var
  i, len: integer;
begin
  len := length(dbls) div 3;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := dbls[i*3];
    Result[i].Y := dbls[i*3 +1];
    Result[i].Z := Round(dbls[i*3 +2]);
  end;
end;
//------------------------------------------------------------------------------
{$ELSE}

function MakePath(const ints: array of Int64): TPath64;
var
  i, len: integer;
begin
  len := length(ints) div 2;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := ints[i*2];
    Result[i].Y := ints[i*2 +1];
  end;
end;
//------------------------------------------------------------------------------

function MakePathD(const dbls: array of double): TPathD; overload;
var
  i, len: integer;
begin
  len := length(dbls) div 2;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := dbls[i*2];
    Result[i].Y := dbls[i*2 +1];
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure AddPolyNodeToPaths(Poly: TPolyPath64; var Paths: TPaths64);
var
  i: Integer;
begin
  if (Length(Poly.Polygon) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Polygon;
  end;
  for i := 0 to Poly.Count - 1 do
    AddPolyNodeToPaths(Poly[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPaths64(PolyTree: TPolyTree64): TPaths64;
begin
  Result := nil;
  AddPolyNodeToPaths(PolyTree, Result);
end;
//------------------------------------------------------------------------------

procedure AddPolyNodeToPathsD(Poly: TPolyPathD; var Paths: TPathsD);
var
  i: Integer;
begin
  if (Length(Poly.Polygon) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Polygon;
  end;
  for i := 0 to Poly.Count - 1 do
    AddPolyNodeToPathsD(Poly[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPathsD(PolyTree: TPolyTreeD): TPathsD;
begin
  Result := nil;
  AddPolyNodeToPathsD(PolyTree, Result);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function BooleanOp(clipType: TClipType;
  const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  with TClipper64.Create do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function BooleanOp(clipType: TClipType; const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  with TClipperD.Create(decimalPrec) do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure BooleanOp(clipType: TClipType; const subjects, clips: TPaths64;
  fillRule: TFillRule; polytree: TPolyTree64);
var
  dummy: TPaths64;
begin
  with TClipper64.Create do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, polytree, dummy);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function Intersect(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctIntersection, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctUnion, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Union(const subjects: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctUnion, subjects, nil, fillRule);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctDifference, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctXor, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Intersect(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctIntersection, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctUnion, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Union(const subjects: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctUnion, subjects, nil, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctDifference, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctXor, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPaths64; delta: Double;
  jt: TJoinType; et: TEndType; MiterLimit: double;
  ArcTolerance: double): TPaths64;
var
  co: TClipperOffset;
begin
  co := TClipperOffset.Create(MiterLimit, ArcTolerance);
  try
    co.AddPaths(paths, jt, et);
    co.Execute(delta, Result);
  finally
    co.free;
  end;
end;
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPathsD; delta: Double;
  jt: TJoinType; et: TEndType; miterLimit: double;
  precision: integer; ArcTolerance: double): TPathsD;
var
  pp: TPaths64;
  scale, invScale: double;
begin
  CheckPrecisionRange(precision);
  scale := Power(10, precision);
  invScale := 1/scale;
  pp := ScalePaths(paths, scale, scale);

  with TClipperOffset.Create(miterLimit, ArcTolerance) do
  try
    AddPaths(pp, jt, et);
    Execute(delta * scale, pp); // reuse pp to receive the solution.
  finally
    free;
  end;
  Result := ScalePathsD(pp, invScale, invScale);
end;
//------------------------------------------------------------------------------

function ExecuteRectClip(const rect: TRect64;
  const path: TPath64; convexOnly: Boolean): TPath64;
var
  paths: TPaths64;
begin
  SetLength(paths, 1);
  paths[0] := path;
  paths := ExecuteRectClip(rect, paths, convexOnly);
  if Assigned(paths) then
    Result := paths[0] else
    Result := nil;
end;
//------------------------------------------------------------------------------

function ExecuteRectClip(const rect: TRect64;
  const paths: TPaths64; convexOnly: Boolean): TPaths64;
begin
  Result := nil;
  if rect.IsEmpty then Exit;
  with TRectClip.Create(rect) do
  try
    Result := Execute(paths, convexOnly);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function ExecuteRectClip(const rect: TRectD; const path: TPathD;
  convexOnly: Boolean; precision: integer): TPathD;
var
  scale: double;
  tmpPath: TPath64;
  rec: TRect64;
begin
  Result := nil;
  if not rect.Intersects(GetBounds(path)) then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));
  tmpPath := ScalePath(path, scale);
  tmpPath := ExecuteRectClip(rec, tmpPath, convexOnly);
  Result := ScalePathD(tmpPath, 1/scale);
end;
//------------------------------------------------------------------------------

function ExecuteRectClip(const rect: TRectD; const paths: TPathsD;
  convexOnly: Boolean; precision: integer): TPathsD;
var
  scale: double;
  tmpPaths: TPaths64;
  rec: TRect64;
begin
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));

  tmpPaths := ScalePaths(paths, scale);
  with TRectClip.Create(rec) do
  try
    tmpPaths := Execute(tmpPaths);
  finally
    Free;
  end;
  Result := ScalePathsD(tmpPaths, 1/scale);
end;
//------------------------------------------------------------------------------

function ExecuteRectClipLines(const rect: TRect64; const path: TPath64): TPaths64;
var
  tmp: TPaths64;
begin
  Result := nil;
  SetLength(tmp, 1);
  tmp[0] := path;
  with TRectClipLines.Create(rect) do
  try
    Result := Execute(tmp);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function ExecuteRectClipLines(const rect: TRect64; const paths: TPaths64): TPaths64;
begin
  Result := nil;
  if rect.IsEmpty then Exit;
  with TRectClipLines.Create(rect) do
  try
    Result := Execute(paths);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function ExecuteRectClipLines(const rect: TRectD;
  const path: TPathD; precision: integer): TPathsD;
var
  scale: double;
  tmpPath: TPath64;
  tmpPaths: TPaths64;
  rec: TRect64;
begin
  Result := nil;
  if not rect.Intersects(GetBounds(path)) then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));
  tmpPath := ScalePath(path, scale);
  tmpPaths := ExecuteRectClipLines(rec, tmpPath);
  Result := ScalePathsD(tmpPaths, 1/scale);
end;
//------------------------------------------------------------------------------

function ExecuteRectClipLines(const rect: TRectD; const paths: TPathsD;
  precision: integer = 2): TPathsD;
var
  scale: double;
  tmpPaths: TPaths64;
  rec: TRect64;
begin
  Result := nil;
  if rect.IsEmpty then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));
  tmpPaths := ScalePaths(paths, scale);
  with TRectClipLines.Create(rec) do
  try
    tmpPaths := Execute(tmpPaths);
  finally
    Free;
  end;
  Result := ScalePathsD(tmpPaths, 1/scale);
end;
//------------------------------------------------------------------------------

function TranslatePath(const path: TPath64; dx, dy: Int64): TPath64;
var
  i, len: integer;
begin
  len := length(path);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x + dx;
    result[i].y := path[i].y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TranslatePath(const path: TPathD; dx, dy: double): TPathD;
var
  i, len: integer;
begin
  len := length(path);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x + dx;
    result[i].y := path[i].y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TranslatePaths(const paths: TPaths64; dx, dy: Int64): TPaths64;
var
  i, len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i] := TranslatePath(paths[i], dx, dy);
  end;
end;
//------------------------------------------------------------------------------

function TranslatePaths(const paths: TPathsD; dx, dy: double): TPathsD;
var
  i, len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i] := TranslatePath(paths[i], dx, dy);
  end;
end;
//------------------------------------------------------------------------------

function MinkowskiSum(const pattern, path: TPath64;
  pathIsClosed: Boolean): TPaths64;
begin
 Result := Clipper.Minkowski.MinkowskiSum(pattern, path, pathIsClosed);
end;
//------------------------------------------------------------------------------

function MinkowskiSum(const pattern, path: TPathD;
  pathIsClosed: Boolean): TPathsD;
begin
 Result := Clipper.Minkowski.MinkowskiSum(pattern, path, pathIsClosed);
end;
//------------------------------------------------------------------------------

procedure ShowPolyPathStructure64(pp: TPolyPath64; level: integer; strings: TStrings);
var
  i: integer;
  spaces, caption: string;
begin
  spaces := StringOfChar(' ', level * 2);
  if pp.IsHole then
    caption := 'Hole' else
    caption := 'Outer';
  if (pp.Count > 0) then
  begin
    strings.Add(Format('%s%s (%d)',[spaces, caption, pp.Count]));
    for i := 0 to pp.Count -1 do
      ShowPolyPathStructure64(pp.child[i], level + 1, strings);
  end else
    strings.Add(spaces + caption);
end;
//------------------------------------------------------------------------------

procedure ShowPolyTreeStructure(polytree: TPolyTree64; strings: TStrings);
var
  i: integer;
begin
  strings.Add('Polytree Root');
  for i := 0 to polytree.Count -1 do
    ShowPolyPathStructure64(polytree[i], 1, strings);
end;
//------------------------------------------------------------------------------

procedure ShowPolyPathStructureD(pp: TPolyPathD; level: integer; strings: TStrings);
var
  i: integer;
  spaces, caption: string;
begin
  spaces := StringOfChar(' ', level * 2);
  if pp.IsHole then
    caption := 'Hole ' else
    caption := 'Outer ';
  if (pp.Count > 0) then
  begin
    strings.Add(Format('%s%s (%d)',[spaces + caption, pp.Count]));
    for i := 0 to pp.Count -1 do
      ShowPolyPathStructureD(pp.child[i], level + 1, strings);
  end else
    strings.Add(spaces + caption);
end;
//------------------------------------------------------------------------------

procedure ShowPolyTreeStructure(polytree: TPolyTreeD; strings: TStrings);
var
  i: integer;
begin
  strings.Add('Polytree Root');
  for i := 0 to polytree.Count -1 do
    ShowPolyPathStructureD(polytree[i], 1, strings);
end;
//------------------------------------------------------------------------------


function TrimCollinear(const p: TPath64; isOpenPath: Boolean = false): TPath64;
var
  i,j, len: integer;
begin
  len := Length(p);

  i := 0;
  if not isOpenPath then
  begin
    while (i < len -1) and
      (CrossProduct(p[len -1], p[i], p[i+1]) = 0) do inc(i);
    while (i < len -1) and
      (CrossProduct(p[len -2], p[len -1], p[i]) = 0) do dec(len);
  end;
  if (len - i < 3) then
  begin
    if not isOpenPath or (len < 2) or PointsEqual(p[0], p[1]) then
      Result := nil else
      Result := p;
    Exit;
  end;

  SetLength(Result, len -i);

  Result[0] := p[i];
  j := 0;
  for i := i+1 to len -2 do
    if CrossProduct(result[j], p[i], p[i+1]) <> 0 then
    begin
      inc(j);
      result[j] := p[i];
    end;

  if isOpenPath then
  begin
    inc(j);
    result[j] := p[len-1];
  end
  else if CrossProduct(result[j], p[len-1], result[0]) <> 0 then
  begin
    inc(j);
    result[j] := p[len-1];
  end else
  begin
    while (j > 1) and
      (CrossProduct(result[j-1], result[j], result[0]) = 0) do dec(j);
    if j < 2 then j := -1;
  end;
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function TrimCollinear(const path: TPathD;
  precision: integer; isOpenPath: Boolean = false): TPathD;
var
  p: TPath64;
  scale: double;
begin
  scale := power(10, precision);
  p := ScalePath(path, scale);
  p := TrimCollinear(p, isOpenPath);
  Result := ScalePathD(p, 1/scale);
end;
//------------------------------------------------------------------------------

function PointInPolygon(const pt: TPoint64;
  const polygon: TPath64): TPointInPolygonResult;
begin
  Result := Clipper.Core.PointInPolygon(pt, polygon);
end;
//------------------------------------------------------------------------------

function PerpendicDistFromLineSqrd(const pt, line1, line2: TPoint64): double;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  a,b,c,d: double;
begin
  a := pt.X - line1.X;
  b := pt.Y - line1.Y;
  c := line2.X - line1.X;
  d := line2.Y - line1.Y;
  if (c = 0) and (d = 0) then
    result := 0 else
    result := Sqr(a * d - c * b) / (c * c + d * d);
end;
//------------------------------------------------------------------------------

function GetNext(current, high: integer; var flags: array of Boolean): integer;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := current +1;
  while (Result <= high) and flags[Result] do inc(Result);
  if (Result <= high) then Exit;
  Result := 0;
  while (flags[Result]) do inc(Result);
end;

function GetPrior(current, high: integer; var flags: array of Boolean): integer;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := current;
  if (Result = 0) then Result := high
  else dec(Result);
  while (Result > 0) and flags[Result] do dec(Result);
  if not flags[Result] then Exit;
  Result := high;
  while flags[Result] do dec(Result);
end;

function SimplifyPath(const path: TPath64;
  epsilon: double; isClosedPath: Boolean = false): TPath64;
var
  i,j, len, high: integer;
  curr, prev, start, prev2, next, next2: integer;
  epsSqr: double;
  flags: array of boolean;
  dsq: array of double;
begin
  Result := nil;
  len := Length(path);
  if (len < 4) then Exit;;
  high := len -1;
  epsSqr := Sqr(epsilon);
  SetLength(flags, len);
  SetLength(dsq, len);

  curr := 0;
  if (isClosedPath) then
  begin
    dsq[0] := PerpendicDistFromLineSqrd(path[0], path[high], path[1]);
    dsq[high] := PerpendicDistFromLineSqrd(path[high], path[0], path[high - 1]);
  end else
  begin
    dsq[0] := MaxDouble;
    dsq[high] := MaxDouble;
  end;

  for i := 1 to high -1 do
    dsq[i] := PerpendicDistFromLineSqrd(path[i], path[i - 1], path[i + 1]);

  while true do
  begin
    if (dsq[curr] > epsSqr) then
    begin
      start := curr;
      repeat
        curr := GetNext(curr, high, flags);
      until (curr = start) or (dsq[curr] < epsSqr);
      if (curr = start) then break;
    end;

    prev := GetPrior(curr, high, flags);
    next := GetNext(curr, high, flags);
    if (next = prev) then break;

    if (dsq[next] < dsq[curr]) then
    begin
      flags[next] := true;
      next := GetNext(next, high, flags);
      next2 := GetNext(next, high, flags);
      dsq[curr] := PerpendicDistFromLineSqrd(
        path[curr], path[prev], path[next]);
      if (next <> high) or isClosedPath then
        dsq[next] := PerpendicDistFromLineSqrd(
          path[next], path[curr], path[next2]);
      curr := next;
    end else
    begin
      flags[curr] := true;
      curr := next;
      next := GetNext(next, high, flags);
      prev2 := GetPrior(prev, high, flags);
      dsq[curr] := PerpendicDistFromLineSqrd(
        path[curr], path[prev], path[next]);
      if (prev <> 0) or isClosedPath then
        dsq[prev] := PerpendicDistFromLineSqrd(
          path[prev], path[prev2], path[curr]);
    end;
  end;
  j := 0;
  SetLength(Result, len);
  for i := 0 to High do
    if not flags[i] then
    begin
      Result[j] := path[i];
      inc(j);
    end;
  SetLength(Result, j);
end;

function SimplifyPaths(const paths: TPaths64;
  epsilon: double; isClosedPaths: Boolean = false): TPaths64;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    result[i] := SimplifyPath(paths[i], epsilon, isClosedPaths);
end;

end.

