unit Clipper2DllCore;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  12 August 2024                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Math;

type
{$IFDEF USINGZ}
    Ztype   = type Int64; //double; //
    PZtype  = ^Ztype;
{$ENDIF}

  PPoint64  = ^TPoint64;
  TPoint64  = record
    X, Y: Int64;
{$IFDEF USINGZ}
    Z: Ztype;
{$ENDIF}
  end;

  PPointD   = ^TPointD;
  TPointD   = record
    X, Y: double;
{$IFDEF USINGZ}
    Z: Ztype;
{$ENDIF}
  end;

  // The most commonly used filling rules for polygons are EvenOdd and NonZero.
  // https://en.wikipedia.org/wiki/Even-odd_rule
  // https://en.wikipedia.org/wiki/Nonzero-rule
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);
  TClipType = (ctNone, ctIntersection, ctUnion, ctDifference, ctXor);

  TArrayOfBoolean = array of Boolean;
  TArrayOfInt64 = array of Int64;
  TArrayOfDouble = array of double;

  TRect64 = record
  public
    Left   : Int64;
    Top    : Int64;
    Right  : Int64;
    Bottom : Int64;
  end;

  TRectD = record
  public
    Left   : double;
    Top    : double;
    Right  : double;
    Bottom : double;
  end;

  CInt64arr   = array[0..$FFFF] of Int64;
  PCInt64arr  = ^CInt64arr;
  CPath64     = PCInt64arr;
  CPaths64    = PCInt64arr;
  CPolyPath64 = PCInt64arr;
  CPolytree64 = PCInt64arr;

  CDblarr     = array[0..$FFFF] of Double;
  PCDblarr    = ^CDblarr;
  CPathD      = PCDblarr;
  CPathsD     = PCDblarr;
  CPolyPathD  = PCDblarr;
  CPolytreeD  = PCDblarr;

{$IFDEF USINGZ}
type
  TZCallback64_DLL = procedure (const bot1, top1, bot2, top2: TPoint64;
    var intersectPt: TPoint64);
  TZCallbackD_DLL = procedure (const bot1, top1, bot2, top2: TPointD;
    var intersectPt: TPointD);
{$ENDIF}

const
{$IFDEF WIN64}
  {$IFDEF USINGZ}
  CLIPPER2_DLL = 'Clipper2_Z_64.dll';
  {$ELSE}
  CLIPPER2_DLL = 'Clipper2_64.dll';
  {$ENDIF}
{$ELSE}
  {$IFDEF USINGZ}
  CLIPPER2_DLL = 'Clipper2_Z_32.dll';
  {$ELSE}
  CLIPPER2_DLL = 'Clipper2_32.dll';
  {$ENDIF}
{$ENDIF}

{$IFDEF USINGZ}
  VERTEX_FIELD_CNT = 3;
{$ELSE}
  VERTEX_FIELD_CNT = 2;
{$ENDIF}

  Intersection = 1; Union = 2; Difference =3; Xor_ = 4;
  EvenOdd = 0; NonZero = 1; Positive = 2; Negative = 3;
  POINT_FIELD_CNT = {$IFDEF USINGZ} 3 {$ELSE} 2 {$ENDIF};

////////////////////////////////////////////////////////
// Clipper2 DLL functions
////////////////////////////////////////////////////////

function Version(): PAnsiChar; cdecl;
  external CLIPPER2_DLL name 'Version';

procedure DisposeExportedArray64(var cps: PCInt64arr); cdecl;
  external CLIPPER2_DLL name 'DisposeArray64';
procedure DisposeExportedArrayD(var cp: PCDblarr); cdecl;
  external CLIPPER2_DLL name 'DisposeArrayD';

function BooleanOp64(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPaths64; const subjects_open: CPaths64;
  const clips: CPaths64; out solution: CPaths64;
  out solution_open: CPaths64;
  preserve_collinear: boolean = true;
  reverse_solution: boolean = false): integer;  cdecl;
  external CLIPPER2_DLL name 'BooleanOp64';
function BooleanOp_PolyTree64(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPaths64; const subjects_open: CPaths64;
  const clips: CPaths64; out solution: CPolyTree64;
  out solution_open: CPaths64;
  preserve_collinear: boolean = true;
  reverse_solution: boolean = false): integer; cdecl;
  external CLIPPER2_DLL name 'BooleanOp_PolyTree64';

function BooleanOpD(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPathsD; const subjects_open: CPathsD;
  const clips: CPathsD; out solution: CPathsD; out solution_open: CPathsD;
  precision: integer = 2;
  preserve_collinear: boolean = true;
  reverse_solution: boolean = false): integer; cdecl;
  external CLIPPER2_DLL name 'BooleanOpD';
function BooleanOp_PolyTreeD(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPathsD; const subjects_open: CPathsD;
  const clips: CPathsD; out solution: CPolyTreeD; out solution_open: CPathsD;
  precision: integer = 2;
  preserve_collinear: boolean = true;
  reverse_solution: boolean = false): integer; cdecl;
  external CLIPPER2_DLL name 'BooleanOp_PolyTreeD';
function InflatePaths64(const paths: CPaths64;
  delta: double; jointype, endtype: UInt8; miter_limit: double = 2.0;
  arc_tolerance: double = 0.0;
  reverse_solution: Boolean = false): CPaths64; cdecl;
  external CLIPPER2_DLL name 'InflatePaths64';
function InflatePathsD(const paths: CPathsD;
  delta: double; jointype, endtype: UInt8; precision: integer = 2;
  miter_limit: double = 2.0; arc_tolerance: double = 0.0;
  reverse_solution: Boolean = false): CPathsD; cdecl;
  external CLIPPER2_DLL name 'InflatePathsD';

function RectClip64(const rect: TRect64; const paths: CPaths64;
  convexOnly: Boolean = false): CPaths64; cdecl;
  external CLIPPER2_DLL name 'RectClip64';
function RectClipD(const rect: TRectD; const paths: CPathsD;
  precision: integer = 2; convexOnly: Boolean = false): CPathsD; cdecl;
  external CLIPPER2_DLL name 'RectClipD';
function RectClipLines64(const rect: TRect64;
  const paths: CPaths64): CPaths64; cdecl;
  external CLIPPER2_DLL name 'RectClipLines64';
function RectClipLinesD(const rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPathsD; cdecl;
  external CLIPPER2_DLL name 'RectClipLinesD';

{$IFDEF USINGZ}
procedure SetZCallback64(zCallback64: TZCallback64_DLL); cdecl;
  external CLIPPER2_DLL name 'SetZCallback64';
procedure SetZCallbackD(zCallbackD: TZCallbackD_DLL); cdecl;
  external CLIPPER2_DLL name 'SetZCallbackD';
{$ENDIF}

function Rect64(const left, top, right, bottom: Int64): TRect64;
function RectD(const left, top, right, bottom: double): TRectD;
function IsValidRect64(const rec: TRect64): Boolean;
function IsValidRectD(const rec: TRectD): Boolean;

function GetBounds(paths: CPathsD): TRectD;
function MakeCPaths64(const coords: array of Int64): CPaths64;
function MakeCPathsD(const coords: array of double): CPathsD;

{$IFDEF USINGZ}
function PointD(const X, Y: Double; Z: ZType = 0): TPointD;
{$ELSE}
function PointD(const X, Y: Double): TPointD;
{$ENDIF}

procedure DisposeLocalArray64(cp: PCInt64arr);
procedure DisposeLocalArrayD(cp: PCDblarr);

implementation

{$IFDEF USINGZ}
function PointD(const X, Y: Double; Z: ZType = 0): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;
{$ELSE}
function PointD(const X, Y: Double): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
end;
{$ENDIF}
//------------------------------------------------------------------------------

function Rect64(const left, top, right, bottom: Int64): TRect64;
begin
  Result.Left   := left;
  Result.Top    := top;
  Result.Right  := right;
  Result.Bottom := bottom;
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

function IsValidRect64(const rec: TRect64): Boolean;
begin
  Result := (rec.Left <= rec.Right) and (rec.Top  < rec.Bottom);
end;
//------------------------------------------------------------------------------

function IsValidRectD(const rec: TRectD): Boolean;
begin
  Result := (rec.Left <= rec.Right) and (rec.Top  < rec.Bottom);
end;
//------------------------------------------------------------------------------

function GetBounds(paths: CPathsD): TRectD;
var
  i,j, idx          : Integer;
  pathsCnt, pathLen : Integer;
  x,y: double;
begin
  Result := RectD(MaxDouble, MaxDouble, -MaxDouble, -MaxDouble);
  pathsCnt := Round(paths[1]);
  idx := 2;
  for i := 0 to pathsCnt -1 do
  begin
    pathLen := Round(paths[idx]);
    inc(idx, 2);
    for j := 0 to pathLen -1 do
    begin
      x := paths[idx]; inc(idx);
      y := paths[idx]; inc(idx);
{$IFDEF USINGZ}
      inc(idx);
{$ENDIF}
      if (x < Result.left) then Result.left := x;
      if (x > Result.right) then Result.right := x;
      if (y < Result.top) then Result.top := y;
      if (y > Result.bottom) then Result.bottom := y;
    end;
  end;
  if not IsValidRectD(Result) then
    Result := RectD(0,0,0,0);
end;
//------------------------------------------------------------------------------

function MakeCPaths64(const coords: array of Int64): CPaths64;
var
  i, arrayLen, pathLen: integer;
begin
  pathLen := length(coords) div VERTEX_FIELD_CNT;
  arrayLen := length(coords) + 4;
  GetMem(Result, arrayLen * sizeOf(Int64));
  Result[0] := arrayLen;
  Result[1] := 1;
  Result[2] := pathLen;
  Result[3] := 0;
  if pathLen > 0 then
    Move(coords[0], Result[4], length(coords) * SizeOf(Int64));
end;
//------------------------------------------------------------------------------

function MakeCPathsD(const coords: array of double): CPathsD;
var
  i, arrayLen, pathLen: integer;
begin
  pathLen := length(coords) div VERTEX_FIELD_CNT;
  arrayLen := length(coords) + 4;
  GetMem(Result, arrayLen * SizeOf(double));
  Result[0] := arrayLen;
  Result[1] := 1;
  Result[2] := pathLen;
  Result[3] := 0;
  if pathLen > 0 then
    Move(coords[0], Result[4], length(coords) * SizeOf(double));
end;
//------------------------------------------------------------------------------

////////////////////////////////////////////////////////
// functions related to Clipper2 DLL structures
// including path format conversion functions
////////////////////////////////////////////////////////

procedure DisposeLocalArray64(cp: PCInt64arr);
begin
  FreeMem(cp);
end;
//-----------------------------------------------------

procedure DisposeLocalArrayD(cp: PCDblarr);
begin
  FreeMem(cp);
end;
//-----------------------------------------------------

end.
