unit Clipper.Core;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - ver.1.0.3                                            *
* Date      :  20 August 2022                                                  *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Core Clipper Library module                                     *
*              Contains structures and functions used throughout the library   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$I Clipper.inc}

interface

uses
  SysUtils, Math;

type
  PPoint64  = ^TPoint64;
  TPoint64  = record
    X, Y: Int64;
{$IFDEF USINGZ}
    Z: Int64;
{$ENDIF}
  end;

  PPointD   = ^TPointD;
  TPointD   = record
    X, Y: double;
{$IFDEF USINGZ}
    Z: Int64;
{$ENDIF}
  end;

  // Path: a simple data structure representing a series of vertices, whether
  // open (poly-line) or closed (polygon). Paths may be simple or complex (self
  // intersecting). For simple polygons, consisting of a single non-intersecting
  // path, path orientation is unimportant. However, for complex polygons and
  // for overlapping polygons, various 'filling rules' define which regions will
  // be inside (filled) and which will be outside (unfilled).

  TPath64  = array of TPoint64;
  TPaths64 = array of TPath64;
  TArrayOfPaths = array of TPaths64;

  TPathD = array of TPointD;
  TPathsD = array of TPathD;
  TArrayOfPathsD = array of TPathsD;

  // The most commonly used filling rules for polygons are EvenOdd and NonZero.
  // https://en.wikipedia.org/wiki/Even-odd_rule
  // https://en.wikipedia.org/wiki/Nonzero-rule
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);

  TArrayOfBoolean = array of Boolean;
  TArrayOfInteger = array of Integer;
  TArrayOfDouble = array of double;

  TRect64 = {$IFDEF RECORD_METHODS}record{$ELSE}object{$ENDIF}
  private
    function GetWidth: Int64; {$IFDEF INLINING} inline; {$ENDIF}
    function GetHeight: Int64; {$IFDEF INLINING} inline; {$ENDIF}
    function GetIsEmpty: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function GetMidPoint: TPoint64; {$IFDEF INLINING} inline; {$ENDIF}
  public
    Left   : Int64;
    Top    : Int64;
    Right  : Int64;
    Bottom : Int64;
    function Contains(const pt: TPoint64): Boolean; overload;
    function Contains(const rec: TRect64): Boolean; overload;
    property Width: Int64 read GetWidth;
    property Height: Int64 read GetHeight;
    property IsEmpty: Boolean read GetIsEmpty;
    property MidPoint: TPoint64 read GetMidPoint;
  end;

  TRectD = {$ifdef RECORD_METHODS}record{$else}object{$endif}
  private
    function GetWidth: double; {$IFDEF INLINING} inline; {$ENDIF}
    function GetHeight: double; {$IFDEF INLINING} inline; {$ENDIF}
    function GetIsEmpty: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function GetMidPoint: TPointD; {$IFDEF INLINING} inline; {$ENDIF}
  public
    Left   : double;
    Top    : double;
    Right  : double;
    Bottom : double;
    function PtInside(const pt: TPointD): Boolean;
    property Width: double read GetWidth;
    property Height: double read GetHeight;
    property IsEmpty: Boolean read GetIsEmpty;
    property MidPoint: TPointD read GetMidPoint;
  end;

  TClipType = (ctNone, ctIntersection, ctUnion, ctDifference, ctXor);

  TPointInPolygonResult = (pipInside, pipOutside, pipOn);

  EClipperLibException = class(Exception);

function Area(const path: TPath64): Double; overload;
function Area(const paths: TPaths64): Double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Area(const path: TPathD): Double; overload;
function Area(const paths: TPathsD): Double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function IsPositive(const path: TPath64): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function IsPositive(const path: TPathD): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function CrossProduct(const pt1, pt2, pt3: TPoint64): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function CrossProduct(const pt1, pt2, pt3: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function CrossProduct(vec1x, vec1y, vec2x, vec2y: double): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function DotProduct(const pt1, pt2, pt3: TPoint64): double;
  {$IFDEF INLINING} inline; {$ENDIF}

function DistanceSqr(const pt1, pt2: TPoint64): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function DistanceSqr(const pt1, pt2: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function DistanceFromLineSqrd(const pt, linePt1, linePt2: TPoint64): double; overload;
function DistanceFromLineSqrd(const pt, linePt1, linePt2: TPointD): double; overload;

function SegmentsIntersect(const s1a, s1b, s2a, s2b: TPoint64): boolean;

function PointsEqual(const pt1, pt2: TPoint64): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function PointsNearEqual(const pt1, pt2: TPointD; distanceSqrd: double): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}

{$IFDEF USINGZ}
function Point64(const X, Y: Int64; Z: Int64 = 0): TPoint64; overload;
{$IFDEF INLINING} inline; {$ENDIF}
function Point64(const X, Y: Double; Z: Int64 = 0): TPoint64; overload;
{$IFDEF INLINING} inline; {$ENDIF}
function PointD(const X, Y: Double; Z: Int64 = 0): TPointD; overload;
{$IFDEF INLINING} inline; {$ENDIF}
{$ELSE}
function Point64(const X, Y: Int64): TPoint64; overload; {$IFDEF INLINING} inline; {$ENDIF}
function Point64(const X, Y: Double): TPoint64; overload; {$IFDEF INLINING} inline; {$ENDIF}
function PointD(const X, Y: Double): TPointD; overload; {$IFDEF INLINING} inline; {$ENDIF}
{$ENDIF}

function Point64(const pt: TPointD): TPoint64; overload; {$IFDEF INLINING} inline; {$ENDIF}
function PointD(const pt: TPoint64): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Rect64(const left, top, right, bottom: Int64): TRect64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Rect64(const recD: TRectD): TRect64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function RectD(const left, top, right, bottom: double): TRectD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function RectD(const rec64: TRect64): TRectD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function GetBounds(const paths: TArrayOfPaths): TRect64; overload;
function GetBounds(const paths: TPaths64): TRect64; overload;
function GetBounds(const paths: TPathsD): TRectD; overload;
function GetBounds(const path: TPath64): TRect64; overload;

function TranslatePoint(const pt: TPoint64; dx, dy: Int64): TPoint64; overload;
function TranslatePoint(const pt: TPointD; dx, dy: double): TPointD; overload;

procedure RotatePt(var pt: TPointD; const center: TPointD; sinA, cosA: double);
procedure RotatePath(var path: TPathD; const center: TPointD; sinA, cosA: double);

procedure InflateRect(var rec: TRect64; dx, dy: Int64); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
procedure InflateRect(var rec: TRectD; dx, dy: double); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function  UnionRect(const rec, rec2: TRect64): TRect64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function  UnionRect(const rec, rec2: TRectD): TRectD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function  RotateRect(const rec: TRect64; angleRad: double): TRect64; overload;
function  RotateRect(const rec: TRectD; angleRad: double): TRectD; overload;
procedure OffsetRect(var rec: TRect64; dx, dy: Int64); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
procedure OffsetRect(var rec: TRectD; dx, dy: double); overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function ScalePoint(const pt: TPoint64; scale: double): TPointD;
  {$IFDEF INLINING} inline; {$ENDIF}

function ScalePath(const path: TPath64; sx, sy: double): TPath64; overload;
function ScalePath(const path: TPathD; sx, sy: double): TPath64; overload;
function ScalePath(const path: TPath64; scale: double): TPath64; overload;
function ScalePath(const path: TPathD; scale: double): TPath64; overload;

function ScalePathD(const path: TPath64; sx, sy: double): TPathD; overload;
function ScalePathD(const path: TPathD; sx, sy: double): TPathD; overload;
function ScalePathD(const path: TPath64; scale: double): TPathD; overload;
function ScalePathD(const path: TPathD; scale: double): TPathD; overload;

function ScalePaths(const paths: TPaths64; sx, sy: double): TPaths64; overload;
function ScalePaths(const paths: TPathsD; sx, sy: double): TPaths64; overload;
function ScalePaths(const paths: TPaths64; scale: double): TPaths64; overload;
function ScalePaths(const paths: TPathsD; scale: double): TPaths64; overload;

function ScalePathsD(const paths: TPaths64; sx, sy: double): TPathsD; overload;
function ScalePathsD(const paths: TPathsD; sx, sy: double): TPathsD; overload;
function ScalePathsD(const paths: TPaths64; scale: double): TPathsD; overload;
function ScalePathsD(const paths: TPathsD; scale: double): TPathsD; overload;

function Path64(const pathD: TPathD): TPath64;
function PathD(const path: TPath64): TPathD;
function Paths64(const pathsD: TPathsD): TPaths64;
function PathsD(const paths: TPaths64): TPathsD;

function StripDuplicates(const path: TPath64; isClosedPath: Boolean = false): TPath64;
function StripNearDuplicates(const path: TPathD;
  minLenSqrd: double; isClosedPath: Boolean): TPathD;

function ValueBetween(val, end1, end2: Int64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
function ValueEqualOrBetween(val, end1, end2: Int64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}

function ReversePath(const path: TPath64): TPath64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function ReversePath(const path: TPathD): TPathD; overload;
function ReversePaths(const paths: TPaths64): TPaths64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function ReversePaths(const paths: TPathsD): TPathsD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

procedure AppendPoint(var path: TPath64; const pt: TPoint64); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
procedure AppendPoint(var path: TPathD; const pt: TPointD); overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function AppendPoints(const path, extra: TPath64): TPath64;
  {$IFDEF INLINING} inline; {$ENDIF}

procedure AppendPath(var paths: TPaths64; const extra: TPath64); overload;
procedure AppendPath(var paths: TPathsD; const extra: TPathD); overload;
procedure AppendPaths(var paths: TPaths64; const extra: TPaths64); overload;
procedure AppendPaths(var paths: TPathsD; const extra: TPathsD); overload;

function ArrayOfPathsToPaths(const ap: TArrayOfPaths): TPaths64;
function GetIntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPoint64): TPointD;

function PointInPolygon(const pt: TPoint64; const polygon: TPath64): TPointInPolygonResult;

function RamerDouglasPeucker(const path: TPath64; epsilon: double): TPath64; overload;
function RamerDouglasPeucker(const paths: TPaths64; epsilon: double): TPaths64; overload;

procedure GetSinCos(angle: double; out sinA, cosA: double);
function Ellipse(const rec: TRect64; steps: integer = 0): TPath64; overload;
function Ellipse(const rec: TRectD; steps: integer = 0): TPathD; overload;

const
  MaxInt64    = 9223372036854775807;
  NullPointD  : TPointD = (X: 0; Y: 0);
  NullRect64  : TRect64 = (left: 0; top: 0; right: 0; Bottom: 0);
  NullRectD   : TRectD = (left: 0; top: 0; right: 0; Bottom: 0);
  Tolerance   : Double = 1.0E-15;

implementation

//------------------------------------------------------------------------------
// TRect64 methods ...
//------------------------------------------------------------------------------

function TRect64.GetWidth: Int64;
begin
  result := right - left;
end;
//------------------------------------------------------------------------------

function TRect64.GetHeight: Int64;
begin
  result := bottom - top;
end;
//------------------------------------------------------------------------------

function TRect64.GetIsEmpty: Boolean;
begin
  result := (bottom <= top) or (right <= left);
end;
//------------------------------------------------------------------------------

function TRect64.GetMidPoint: TPoint64;
begin
  result := Point64((Left + Right) div 2, (Top + Bottom) div 2);
end;
//------------------------------------------------------------------------------

function TRect64.Contains(const pt: TPoint64): Boolean;
begin
  result := (pt.X > Left) and (pt.X < Right) and
    (pt.Y > Top) and (pt.Y < Bottom);
end;
//------------------------------------------------------------------------------

function TRect64.Contains(const rec: TRect64): Boolean;
begin
  result := (rec.Left >= Left) and (rec.Right <= Right) and
    (rec.Top >= Top) and (rec.Bottom <= Bottom);
end;

//------------------------------------------------------------------------------
// TRectD methods ...
//------------------------------------------------------------------------------

function TRectD.GetWidth: double;
begin
  result := right - left;
end;
//------------------------------------------------------------------------------

function TRectD.GetHeight: double;
begin
  result := bottom - top;
end;
//------------------------------------------------------------------------------

function TRectD.GetIsEmpty: Boolean;
begin
  result := (bottom <= top) or (right <= left);
end;
//------------------------------------------------------------------------------

function TRectD.GetMidPoint: TPointD;
begin
  result := PointD((Left + Right) *0.5, (Top + Bottom) *0.5);
end;
//------------------------------------------------------------------------------

function TRectD.PtInside(const pt: TPointD): Boolean;
begin
  result := (pt.X > Left) and (pt.X < Right) and
    (pt.Y > Top) and (pt.Y < Bottom);
end;

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

procedure RaiseError(const msg: string); {$IFDEF INLINING} inline; {$ENDIF}
begin
  raise EClipperLibException.Create(msg);
end;
//------------------------------------------------------------------------------

function PointsEqual(const pt1, pt2: TPoint64): Boolean;
begin
  Result := (pt1.X = pt2.X) and (pt1.Y = pt2.Y);
end;
//------------------------------------------------------------------------------

function PointsNearEqual(const pt1, pt2: TPointD; distanceSqrd: double): Boolean;
begin
  Result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y) < distanceSqrd;
end;
//------------------------------------------------------------------------------

function StripDuplicates(const path: TPath64; isClosedPath: Boolean): TPath64;
var
  i,j, len: integer;
begin
  len := length(path);
  SetLength(Result, len);
  if len = 0 then Exit;
  Result[0] := path[0];
  j := 0;
  for i := 1 to len -1 do
    if not PointsEqual(Result[j], path[i]) then
    begin
      inc(j);
      Result[j] := path[i];
    end;
  if isClosedPath and PointsEqual(Result[0], path[j]) then dec(j);
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function StripNearDuplicates(const path: TPathD;
  minLenSqrd: double; isClosedPath: Boolean): TPathD;
var
  i,j, len: integer;
begin
  len := length(path);
  SetLength(Result, len);
  if len = 0 then Exit;

  Result[0] := path[0];
  j := 0;
  for i := 1 to len -1 do
    if not PointsNearEqual(Result[j], path[i], minLenSqrd) then
    begin
      inc(j);
      Result[j] := path[i];
    end;

  if isClosedPath and
    PointsNearEqual(Result[j], Result[0], minLenSqrd) then dec(j);
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function ValueBetween(val, end1, end2: Int64): Boolean;
begin
  // nb: accommodates axis aligned between where end1 == end2
  Result := ((val <> end1) = (val <> end2)) and
    ((val > end1) = (val < end2));
end;
//------------------------------------------------------------------------------

function ValueEqualOrBetween(val, end1, end2: Int64): Boolean;
begin
  Result := (val = end1) or (val = end2) or
    (val > end1) = (val < end2);
end;
//------------------------------------------------------------------------------

function ScalePoint(const pt: TPoint64; scale: double): TPointD;
begin
  Result.X := pt.X * scale;
  Result.Y := pt.Y * scale;
{$IFDEF USINGZ}
  Result.Z := pt.Z;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPath64; sx, sy: double): TPath64;
var
  i,len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(path);
  setlength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].X := Round(path[i].X * sx);
    result[i].Y := Round(path[i].Y * sy);
  end;
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPathD; sx, sy: double): TPath64;
var
  i,j, len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(path);
  setlength(result, len);
  if len = 0 then Exit;
  j := 1;
  result[0].X := Round(path[0].X * sx);
  result[0].Y := Round(path[0].Y * sy);
  for i := 1 to len -1 do
  begin
    result[j].X := Round(path[i].X * sx);
    result[j].Y := Round(path[i].Y * sy);
    if (result[j].X <> result[j-1].X) or
      (result[j].Y <> result[j-1].Y) then inc(j);
  end;
  setlength(result, j);
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPath64; scale: double): TPath64;
var
  i,j, len: integer;
begin
  len := length(path);
  setlength(result, len);
  if len = 0 then Exit;
  j := 1;
  result[0].X := Round(path[0].X * scale);
  result[0].Y := Round(path[0].Y * scale);
  for i := 1 to len -1 do
  begin
    result[j].X := Round(path[i].X * scale);
    result[j].Y := Round(path[i].Y * scale);
    if (result[j].X <> result[j-1].X) or
      (result[j].Y <> result[j-1].Y) then inc(j);
  end;
  setlength(result, j);
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPathD; scale: double): TPath64;
var
  i,len: integer;
begin
  len := length(path);
  setlength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].X := Round(path[i].X * scale);
    result[i].Y := Round(path[i].Y * scale);
  end;
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPaths64; sx, sy: double): TPaths64;
var
  i,len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(paths);
  setlength(result, len);
  for i := 0 to len -1 do
    result[i] := ScalePath(paths[i], sx, sy);
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPathsD; sx, sy: double): TPaths64;
var
  i,len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(paths);
  setlength(result, len);
  for i := 0 to len -1 do
    result[i] := ScalePath(paths[i], sx, sy);
end;
//------------------------------------------------------------------------------

function ScalePathD(const path: TPath64; sx, sy: double): TPathD;
var
  i: integer;
begin
  setlength(result, length(path));
  for i := 0 to high(path) do
  begin
    result[i].X := path[i].X * sx;
    result[i].Y := path[i].Y * sy;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathD(const path: TPathD; sx, sy: double): TPathD;
var
  i: integer;
begin
  setlength(result, length(path));
  for i := 0 to high(path) do
  begin
    result[i].X := path[i].X * sx;
    result[i].Y := path[i].Y * sy;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathD(const path: TPath64; scale: double): TPathD;
var
  i: integer;
begin
  setlength(result, length(path));
  for i := 0 to high(path) do
  begin
    result[i].X := path[i].X * scale;
    result[i].Y := path[i].Y * scale;
{$IFDEF USINGZ}
    result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function ScalePathD(const path: TPathD; scale: double): TPathD;
var
  i: integer;
begin
  setlength(result, length(path));
  for i := 0 to high(path) do
  begin
    result[i].X := path[i].X * scale;
    result[i].Y := path[i].Y * scale;
{$IFDEF USINGZ}
    result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPaths64; sx, sy: double): TPathsD;
var
  i,j: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := (paths[i][j].X * sx);
      result[i][j].Y := (paths[i][j].Y * sy);
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPathsD; sx, sy: double): TPathsD;
var
  i,j: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X * sx;
      result[i][j].Y := paths[i][j].Y * sy;
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPaths64; scale: double): TPaths64;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := Round(paths[i][j].X * scale);
      result[i][j].Y := Round(paths[i][j].Y * scale);
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPathsD; scale: double): TPaths64;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := Round(paths[i][j].X * scale);
      result[i][j].Y := Round(paths[i][j].Y * scale);
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPaths64; scale: double): TPathsD; overload;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X * scale;
      result[i][j].Y := paths[i][j].Y * scale;
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPathsD; scale: double): TPathsD; overload;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X * scale;
      result[i][j].Y := paths[i][j].Y * scale;
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function Path64(const pathD: TPathD): TPath64;
var
  i, len: integer;
begin
  len := Length(pathD);
  setLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := Round(pathD[i].X);
    Result[i].Y := Round(pathD[i].Y);
  end;
end;
//------------------------------------------------------------------------------

function PathD(const path: TPath64): TPathD;
var
  i, len: integer;
begin
  len := Length(path);
  setLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := path[i].X;
    Result[i].Y := path[i].Y;
  end;
end;
//------------------------------------------------------------------------------

function Paths64(const pathsD: TPathsD): TPaths64;
var
  i, len: integer;
begin
  len := Length(pathsD);
  setLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Path64(pathsD[i]);
end;
//------------------------------------------------------------------------------

function PathsD(const paths: TPaths64): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  setLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := PathD(paths[i]);
end;
//------------------------------------------------------------------------------

function ReversePath(const path: TPath64): TPath64;
var
  i, highI: Integer;
begin
  highI := high(path);
  SetLength(Result, highI +1);
  for i := 0 to highI do
    Result[i] := path[highI - i];
end;
//------------------------------------------------------------------------------

function ReversePath(const path: TPathD): TPathD;
var
  i, highI: Integer;
begin
  highI := high(path);
  SetLength(Result, highI +1);
  for i := 0 to highI do
    Result[i] := path[highI - i];
end;
//------------------------------------------------------------------------------

function ReversePaths(const paths: TPaths64): TPaths64;
var
  i, j, highJ: Integer;
begin
  i := length(paths);
  SetLength(Result, i);
  for i := 0 to i -1 do
  begin
    highJ := high(paths[i]);
    SetLength(Result[i], highJ+1);
    for j := 0 to highJ do
      Result[i][j] := paths[i][highJ - j];
  end;
end;
//------------------------------------------------------------------------------

function ReversePaths(const paths: TPathsD): TPathsD;
var
  i, j, highJ: Integer;
begin
  i := length(paths);
  SetLength(Result, i);
  for i := 0 to i -1 do
  begin
    highJ := high(paths[i]);
    SetLength(Result[i], highJ+1);
    for j := 0 to highJ do
      Result[i][j] := paths[i][highJ - j];
  end;
end;
//------------------------------------------------------------------------------

procedure AppendPoint(var path: TPath64; const pt: TPoint64);
var
  len: Integer;
begin
  len := length(path);
  SetLength(path, len +1);
  path[len] := pt;
end;
//------------------------------------------------------------------------------

function AppendPoints(const path, extra: TPath64): TPath64;
var
  len1, len2: Integer;
begin
  len1 := length(path);
  len2 := length(extra);
  SetLength(Result, len1 + len2);
  if len1 > 0 then
    Move(path[0], Result[0], len1 * sizeOf(TPoint64));
  if len2 > 0 then
    Move(extra[0], Result[len1], len2 * sizeOf(TPoint64));
end;
//------------------------------------------------------------------------------

procedure AppendPoint(var path: TPathD; const pt: TPointD);
var
  len: Integer;
begin
  len := length(path);
  SetLength(path, len +1);
  path[len] := pt;
end;
//------------------------------------------------------------------------------

procedure AppendPath(var paths: TPaths64; const extra: TPath64);
var
  len: Integer;
begin
  len := length(paths);
  SetLength(paths, len +1);
  paths[len] := extra;
end;
//------------------------------------------------------------------------------

procedure AppendPath(var paths: TPathsD; const extra: TPathD);
var
  len: Integer;
begin
  len := length(paths);
  SetLength(paths, len +1);
  paths[len] := extra;
end;
//------------------------------------------------------------------------------

procedure AppendPaths(var paths: TPaths64; const extra: TPaths64);
var
  i, len1, len2: Integer;
begin
  len1 := length(paths);
  len2 := length(extra);
  SetLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1 + i] := extra[i];
end;
//------------------------------------------------------------------------------

procedure AppendPaths(var paths: TPathsD; const extra: TPathsD);
var
  i, len1, len2: Integer;
begin
  len1 := length(paths);
  len2 := length(extra);
  SetLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1 + i] := extra[i];
end;
//------------------------------------------------------------------------------

function ArrayOfPathsToPaths(const ap: TArrayOfPaths): TPaths64;
var
  i,j,k, len, cnt: integer;
begin
  cnt := 0;
  len := length(ap);
  for i := 0 to len -1 do
    inc(cnt, length(ap[i]));
  k := 0;
  setlength(result, cnt);
  for i := 0 to len -1 do
    for j := 0 to length(ap[i]) -1 do
    begin
      result[k] := ap[i][j];
      inc(k);
    end;
end;
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
function Point64(const X, Y: Int64; Z: Int64): TPoint64;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;
//------------------------------------------------------------------------------

function Point64(const X, Y: Double; Z: Int64): TPoint64;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
  Result.Z := Z;
end;
//------------------------------------------------------------------------------

function PointD(const X, Y: Double; Z: Int64): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;
//------------------------------------------------------------------------------

function Point64(const pt: TPointD): TPoint64;
begin
  Result.X := Round(pt.X);
  Result.Y := Round(pt.Y);
  Result.Z := pt.Z;
end;
//------------------------------------------------------------------------------

function PointD(const pt: TPoint64): TPointD;
begin
  Result.X := pt.X;
  Result.Y := pt.Y;
  Result.Z := pt.Z;
end;
//------------------------------------------------------------------------------

{$ELSE}

function Point64(const X, Y: Int64): TPoint64;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function Point64(const X, Y: Double): TPoint64;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
end;
//------------------------------------------------------------------------------

function PointD(const X, Y: Double): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function Point64(const pt: TPointD): TPoint64;
begin
  Result.X := Round(pt.X);
  Result.Y := Round(pt.Y);
end;
//------------------------------------------------------------------------------

function PointD(const pt: TPoint64): TPointD;
begin
  Result.X := pt.X;
  Result.Y := pt.Y;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function Rect64(const left, top, right, bottom: Int64): TRect64;
begin
  Result.Left   := left;
  Result.Top    := top;
  Result.Right  := right;
  Result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function Rect64(const recD: TRectD): TRect64;
begin
  Result.Left   := Floor(recD.left);
  Result.Top    := Floor(recD.top);
  Result.Right  := Ceil(recD.right);
  Result.Bottom := Ceil(recD.bottom);
end;
//------------------------------------------------------------------------------

function RectD(const left, top, right, bottom: double): TRectD;
begin
  Result.Left   := left;
  Result.Top    := top;
  Result.Right  := right;
  Result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function RectD(const rec64: TRect64): TRectD; overload;
begin
  Result.Left   := rec64.left;
  Result.Top    := rec64.top;
  Result.Right  := rec64.right;
  Result.Bottom := rec64.bottom;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TArrayOfPaths): TRect64; overload;
var
  i,j,k: Integer;
  p: PPoint64;
begin
  Result := Rect64(MaxInt64, MaxInt64, -MaxInt64, -MaxInt64);
  for i := 0 to High(paths) do
    for j := 0 to High(paths[i]) do
      if Assigned(paths[i][j]) then
      begin
        p := @paths[i][j][0];
        for k := 0 to High(paths[i][j]) do
        begin
          if p.X < Result.Left then Result.Left := p.X;
          if p.X > Result.Right then Result.Right := p.X;
          if p.Y < Result.Top then Result.Top := p.Y;
          if p.Y > Result.Bottom then Result.Bottom := p.Y;
          inc(p);
        end;
      end;
  if Result.Left > Result.Right then Result := NullRect64;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TPaths64): TRect64;
var
  i,j: Integer;
  p: PPoint64;
begin
  Result := Rect64(MaxInt64, MaxInt64, -MaxInt64, -MaxInt64);
  for i := 0 to High(paths) do
    if Assigned(paths[i]) then
    begin
      p := @paths[i][0];
      for j := 0 to High(paths[i]) do
      begin
        if p.X < Result.Left then Result.Left := p.X;
        if p.X > Result.Right then Result.Right := p.X;
        if p.Y < Result.Top then Result.Top := p.Y;
        if p.Y > Result.Bottom then Result.Bottom := p.Y;
        inc(p);
      end;
    end;
  if Result.Left > Result.Right then Result := NullRect64;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TPathsD): TRectD;
var
  i,j: Integer;
  p: PPointD;
begin
  Result := RectD(MaxDouble, MaxDouble, -MaxDouble, -MaxDouble);
  for i := 0 to High(paths) do
    if Assigned(paths[i]) then
    begin
      p := @paths[i][0];
      for j := 0 to High(paths[i]) do
      begin
        if p.X < Result.Left then Result.Left := p.X;
        if p.X > Result.Right then Result.Right := p.X;
        if p.Y < Result.Top then Result.Top := p.Y;
        if p.Y > Result.Bottom then Result.Bottom := p.Y;
        inc(p);
      end;
    end;
  if Result.Left >= Result.Right then Result := nullRectD;
end;
//------------------------------------------------------------------------------

function GetBounds(const path: TPath64): TRect64;
var
  i, len: Integer;
  p: PPoint64;
begin
  len := Length(path);
  if len = 0 then
  begin
    Result := NullRect64;
    Exit;
  end;

  Result := Rect64(MaxInt64, MaxInt64, -MaxInt64, -MaxInt64);
  p := @path[0];
  for i := 0 to High(path) do
  begin
    if p.X < Result.Left then Result.Left := p.X;
    if p.X > Result.Right then Result.Right := p.X;
    if p.Y < Result.Top then Result.Top := p.Y;
    if p.Y > Result.Bottom then Result.Bottom := p.Y;
    inc(p);
  end;
end;
//------------------------------------------------------------------------------

function TranslatePoint(const pt: TPoint64; dx, dy: Int64): TPoint64;
begin
  Result.X := pt.X + dx;
  Result.Y := pt.Y + dy;
end;
//------------------------------------------------------------------------------

function TranslatePoint(const pt: TPointD; dx, dy: double): TPointD;
begin
  Result.X := pt.X + dx;
  Result.Y := pt.Y + dy;
end;
//------------------------------------------------------------------------------

procedure InflateRect(var rec: TRect64; dx, dy: Int64);
begin
  dec(rec.Left, dx);
  inc(rec.Right, dx);
  dec(rec.Top, dy);
  inc(rec.Bottom, dy);
end;
//------------------------------------------------------------------------------

procedure InflateRect(var rec: TRectD; dx, dy: double);
begin
  rec.Left := rec.Left - dx;
  rec.Right := rec.Right + dx;
  rec.Top := rec.Top - dy;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

procedure RotatePt(var pt: TPointD; const center: TPointD; sinA, cosA: double);
var
  tmpX, tmpY: double;
begin
  tmpX := pt.X-center.X;
  tmpY := pt.Y-center.Y;
  pt.X := tmpX * cosA - tmpY * sinA + center.X;
  pt.Y := tmpX * sinA + tmpY * cosA + center.Y;
end;
//------------------------------------------------------------------------------

procedure RotatePath(var path: TPathD; const center: TPointD; sinA, cosA: double);
var
  i: integer;
begin
  for i := 0 to High(path) do
    RotatePt(path[i], center, sinA, cosA);
end;
//------------------------------------------------------------------------------

function RotateRect(const rec: TRectD; angleRad: double): TRectD;
var
  i: integer;
  sinA, cosA: double;
  cp: TPointD;
  pts: TPathD;
begin
  setLength(pts, 4);
  sinA := Sin(-angleRad);
  cosA := cos(-angleRad);
  cp.X := (rec.Right + rec.Left) / 2;
  cp.Y := (rec.Bottom + rec.Top) / 2;
  pts[0] := PointD(rec.Left, rec.Top);
  pts[1] := PointD(rec.Right, rec.Top);
  pts[2] := PointD(rec.Left, rec.Bottom);
  pts[3] := PointD(rec.Right, rec.Bottom);
  for i := 0 to 3 do RotatePt(pts[i], cp, sinA, cosA);
  result.Left := pts[0].X;
  result.Right := result.Left;
  result.Top := pts[0].Y;
  result.Bottom := result.Top;
  for i := 1 to 3 do
  begin
    if pts[i].X < result.Left then result.Left := pts[i].X;
    if pts[i].Y < result.Top then result.Top := pts[i].Y;
    if pts[i].X > result.Right then result.Right := pts[i].X;
    if pts[i].Y > result.Bottom then result.Bottom := pts[i].Y;
  end;
end;
//------------------------------------------------------------------------------

function RotateRect(const rec: TRect64; angleRad: double): TRect64;
var
  recD: TRectD;
begin
  recD := RectD(rec.Left, rec.Top, rec.Right, rec.Bottom);
  recD := RotateRect(recD, angleRad);
  result.Left := Floor(recD.Left);
  result.Top := Floor(recD.Top);
  result.Right := Ceil(recD.Right);
  result.Bottom := Ceil(recD.Bottom);
end;
//------------------------------------------------------------------------------

procedure OffsetRect(var rec: TRect64; dx, dy: Int64);
begin
  inc(rec.Left, dx); inc(rec.Top, dy);
  inc(rec.Right, dx); inc(rec.Bottom, dy);
end;
//------------------------------------------------------------------------------

procedure OffsetRect(var rec: TRectD; dx, dy: double);
begin
  rec.Left   := rec.Left   + dx;
  rec.Right  := rec.Right  + dx;
  rec.Top    := rec.Top    + dy;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

function UnionRect(const rec, rec2: TRect64): TRect64;
begin
  // nb: don't use rec.IsEmpty as this will
  // reject open axis-aligned flat paths
  if (rec.Width <= 0) and (rec.Height <= 0) then result := rec2
  else if (rec2.Width <= 0) and (rec2.Height <= 0) then result := rec
  else
  begin
    result.Left := min(rec.Left, rec2.Left);
    result.Right := max(rec.Right, rec2.Right);
    result.Top := min(rec.Top, rec2.Top);
    result.Bottom := max(rec.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function UnionRect(const rec, rec2: TRectD): TRectD;
begin
  // nb: don't use rec.IsEmpty as this will
  // reject open axis-aligned flat paths
  if (rec.Width <= 0) and (rec.Height <= 0) then result := rec2
  else if (rec2.Width <= 0) and (rec2.Height <= 0) then result := rec
  else
  begin
    result.Left := min(rec.Left, rec2.Left);
    result.Right := max(rec.Right, rec2.Right);
    result.Top := min(rec.Top, rec2.Top);
    result.Bottom := max(rec.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function Area(const path: TPath64): Double;
var
  i, highI: Integer;
  d: double;
  p1,p2: PPoint64;
begin
  // shoelace formula
  Result := 0.0;
  highI := High(path);
  if highI < 2 then Exit;
  p1 := @path[highI];
  p2 := @path[0];
  for i := 0 to highI do
  begin
    d := (p1.Y + p2.Y); // needed for Delphi7
    Result := Result + d * (p1.X - p2.X);
    p1 := p2; inc(p2);
  end;
  Result := Result * 0.5;
end;
//------------------------------------------------------------------------------

function Area(const paths: TPaths64): Double;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(paths) do
    Result := Result + Area(paths[i]);
end;
//------------------------------------------------------------------------------

function Area(const path: TPathD): Double;
var
  i, highI: Integer;
  p1,p2: PPointD;
begin
  // https://en.wikipedia.org/wiki/Shoelace_formula
  Result := 0.0;
  highI := High(path);
  if highI < 2 then Exit;
  p1 := @path[highI];
  p2 := @path[0];
  for i := 0 to highI do
  begin
    Result := Result + (p1.Y + p2.Y) * (p1.X - p2.X);
    p1 := p2; inc(p2);
  end;
  Result := Result * 0.5;
end;
//------------------------------------------------------------------------------

function Area(const paths: TPathsD): Double;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(paths) do
    Result := Result + Area(paths[i]);
end;
//------------------------------------------------------------------------------

function IsPositive(const path: TPath64): Boolean;
begin
  Result := (Area(path) >= 0);
end;
//------------------------------------------------------------------------------

function IsPositive(const path: TPathD): Boolean;
begin
  Result := (Area(path) >= 0);
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPoint64): double;
begin
  result := CrossProduct(
    pt2.X - pt1.X, pt2.Y - pt1.Y,
    pt3.X - pt2.X, pt3.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPointD): double;
begin
  result := CrossProduct(
    pt2.X - pt1.X, pt2.Y - pt1.Y,
    pt3.X - pt2.X, pt3.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function CrossProduct(vec1x, vec1y, vec2x, vec2y: double): double;
begin
  result := (vec1x * vec2y - vec1y * vec2x);
end;
//------------------------------------------------------------------------------

function DotProduct(const pt1, pt2, pt3: TPoint64): double;
var
  x1,x2,y1,y2: double; // avoids potential int overflow
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt3.X - pt2.X;
  y2 := pt3.Y - pt2.Y;
  result := (x1 * x2 + y1 * y2);
end;
//------------------------------------------------------------------------------

function SqrInt64(val: Int64): double; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := val; // force conversion
  Result := Result * Result;
end;
//------------------------------------------------------------------------------

function DistanceSqr(const pt1, pt2: TPoint64): double;
begin
  Result := SqrInt64(pt1.X - pt2.X) + SqrInt64(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function DistanceSqr(const pt1, pt2: TPointD): double;
begin
  Result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function DistanceFromLineSqrd(const pt, linePt1, linePt2: TPoint64): double;
var
  a,b,c: double;
begin
  // perpendicular distance of point (x0,y0) = (a*x0 + b*y0 + C)/Sqrt(a*a + b*b)
  // where ax + by +c = 0 is the equation of the line
  // see https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
	a := (linePt1.Y - linePt2.Y);
	b := (linePt2.X - linePt1.X);
	c := a * linePt1.X + b * linePt1.Y;
	c := a * pt.x + b * pt.y - c;
	Result := (c * c) / (a * a + b * b);
end;
//---------------------------------------------------------------------------

function DistanceFromLineSqrd(const pt, linePt1, linePt2: TPointD): double;
var
  a,b,c: double;
begin
	a := (linePt1.Y - linePt2.Y);
	b := (linePt2.X - linePt1.X);
	c := a * linePt1.X + b * linePt1.Y;
	c := a * pt.x + b * pt.y - c;
	Result := (c * c) / (a * a + b * b);
end;
//---------------------------------------------------------------------------

function CleanPath(const path: TPath64): TPath64;
var
  i,j, len: integer;
  prev: TPoint64;
begin
  Result := nil;
  len := Length(path);
  while (len > 2) and
   (CrossProduct(path[len-2], path[len-1], path[0]) = 0) do dec(len);
  SetLength(Result, len);
  if (len < 2) then Exit;
  prev := path[len -1];
  j := 0;
  for i := 0 to len -2 do
  begin
    if CrossProduct(prev, path[i], path[i+1]) = 0 then Continue;
    Result[j] := path[i];
    inc(j);
    prev := path[i];
  end;
  Result[j] := path[len -1];
  SetLength(Result, j+1);
end;
//------------------------------------------------------------------------------

function SegmentsIntersect(const s1a, s1b, s2a, s2b: TPoint64): boolean;
begin
  // nb: result excludes overlapping collinear segments
  result := (CrossProduct(s1a, s2a, s2b) * CrossProduct(s1b, s2a, s2b) < 0) and
    (CrossProduct(s2a, s1a, s1b) * CrossProduct(s2b, s1a, s1b) < 0);
end;
//------------------------------------------------------------------------------

function GetIntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPoint64): TPointD;
var
  m1,b1,m2,b2: double;
begin
  // see http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
  if (ln1B.X = ln1A.X) then
  begin
    if (ln2B.X = ln2A.X) then exit; // parallel lines
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
    if Abs(m1 - m2) > 1.0E-15 then
    begin
      Result.X := (b2 - b1)/(m1 - m2);
      Result.Y := m1 * Result.X + b1;
    end else
    begin
      Result.X := (ln1a.X + ln1b.X) * 0.5;
      Result.Y := (ln1a.Y + ln1b.Y) * 0.5;
    end;
  end;
end;
//------------------------------------------------------------------------------

function PointInPolygon(const pt: TPoint64;
  const polygon: TPath64): TPointInPolygonResult;
var
  i, len, val: Integer;
  isAbove: Boolean;
  d: Double; // used to avoid integer overflow
  curr, prev, first, stop: PPoint64;
begin
  result := pipOutside;
  len := Length(polygon);
  if len < 3 then Exit;

  i := len -1;
  first := @polygon[0];

  while (i >= 0) and (polygon[i].Y = pt.Y) do dec(i);
  if i < 0 then Exit;
  isAbove := polygon[i].Y < pt.Y;

  Result := pipOn;
  stop := @polygon[len -1];
  inc(stop); // stop is just past the last point

  curr := first;
  val := 0;

  while (curr <> stop) do
  begin
    if isAbove then
    begin
      while (curr <> stop) and (curr.Y < pt.Y) do inc(curr);
      if (curr = stop) then break;
    end else
    begin
      while (curr <> stop) and (curr.Y > pt.Y) do inc(curr);
      if (curr = stop) then break;
    end;

    if curr = first then
      prev := stop else
      prev := curr;
    dec(prev);

    if (curr.Y = pt.Y) then
    begin
      if (curr.X = pt.X) or ((curr.Y = prev.Y) and
        ((pt.X < prev.X) <> (pt.X < curr.X))) then Exit;
      inc(curr);
      Continue;
    end;

    if (pt.X < curr.X) and (pt.X < prev.X) then
      // we're only interested in edges crossing on the left
    else if((pt.X > prev.X) and (pt.X > curr.X)) then
      val := 1 - val // toggle val
    else
    begin
      d := CrossProduct(prev^, curr^, pt);
      if d = 0 then Exit; // ie point on path
      if (d < 0) = isAbove then val := 1 - val;
    end;

    isAbove := not isAbove;
    inc(curr);
  end;
  if val = 0 then
     result := pipOutside else
     result := pipInside;
end;
//------------------------------------------------------------------------------

procedure GetSinCos(angle: double; out sinA, cosA: double);
  {$IFDEF INLINE} inline; {$ENDIF}
{$IFNDEF FPC}
var s, c: extended;
{$ENDIF}
begin
{$IFDEF FPC}
  Math.SinCos(angle, sinA, cosA);
{$ELSE}
  Math.SinCos(angle, s, c);
  sinA := s; cosA := c;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRect64; steps: integer): TPath64;
begin
  Result := Path64(Ellipse(RectD(rec), steps));
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRectD; steps: integer): TPathD;
var
  i: Integer;
  sinA, cosA: double;
  centre, radius, delta: TPointD;
begin
  result := nil;
  if rec.IsEmpty then Exit;
  with rec do
  begin
    centre := rec.MidPoint;
    radius := PointD(Width * 0.5, Height  * 0.5);
  end;
  if (steps < 3) then
    steps := Ceil(PI * sqrt(rec.width + rec.height));
  GetSinCos(2 * Pi / Steps, sinA, cosA);
  delta.x := cosA; delta.y := sinA;
  SetLength(Result, Steps);
  Result[0] := PointD(centre.X + radius.X, centre.Y);
  for i := 1 to steps -1 do
  begin
    Result[i] := PointD(centre.X + radius.X * delta.x,
      centre.Y + radius.y * delta.y);
    delta :=  PointD(delta.X * cosA - delta.Y * sinA,
      delta.Y * cosA + delta.X * sinA);
  end; // rotates clockwise
end;
//------------------------------------------------------------------------------

function PerpendicDistFromLineSqrd(const pt, line1, line2: TPoint64): double;
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

procedure RDP(const path: TPath64; startIdx, endIdx: integer;
  epsilonSqrd: double; var boolArray: TArrayOfBoolean); overload;
var
  i, idx: integer;
  d, maxD: double;
begin
  idx := 0;
  maxD := 0;
	while (endIdx > startIdx) and
    PointsEqual(path[startIdx], path[endIdx]) do
    begin
      boolArray[endIdx] := false;
      dec(endIdx);
    end;
  for i := startIdx +1 to endIdx -1 do
  begin
    // PerpendicDistFromLineSqrd - avoids expensive Sqrt()
    d := PerpendicDistFromLineSqrd(path[i], path[startIdx], path[endIdx]);
    if d <= maxD then Continue;
    maxD := d;
    idx := i;
  end;
  if maxD < epsilonSqrd then Exit;
  boolArray[idx] := true;
  if idx > startIdx + 1 then RDP(path, startIdx, idx, epsilonSqrd, boolArray);
  if endIdx > idx + 1 then RDP(path, idx, endIdx, epsilonSqrd, boolArray);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const path: TPath64; epsilon: double): TPath64;
var
  i,j, len: integer;
  boolArray: TArrayOfBoolean;
begin
  len := length(path);
  if len < 5 then
  begin
    result := Copy(path, 0, len);
    Exit;
  end;
  SetLength(boolArray, len); // already zero initialized
  boolArray[0] := true;
  boolArray[len -1] := true;
  RDP(path, 0, len -1, Sqr(epsilon), boolArray);
  j := 0;
  SetLength(Result, len);
  for i := 0 to len -1 do
    if boolArray[i] then
    begin
      Result[j] := path[i];
      inc(j);
    end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const paths: TPaths64; epsilon: double): TPaths64;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := RamerDouglasPeucker(paths[i], epsilon);
end;
//------------------------------------------------------------------------------

end.

