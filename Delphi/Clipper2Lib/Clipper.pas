unit Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  7 May 2022                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This module provides a simple interface to the Clipper Library  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Math, Clipper.Core, Clipper.Engine, Clipper.Offset;

//Redeclare here a number of structures defined in
//other units so those units won't need to be declared
//just to use the following functions.
type
  TPoint64    = Clipper.Core.TPoint64;
  TRect64     = Clipper.Core.TRect64;
  TPath64     = Clipper.Core.TPath64;
  TPaths64    = Clipper.Core.TPaths64;
  TPointD     = Clipper.Core.TPointD;
  TRectD      = Clipper.Core.TRectD;
  TPathD      = Clipper.Core.TPathD;
  TPathsD     = Clipper.Core.TPathsD;
  TFillRule   = Clipper.Core.TFillRule;
  TPolyTree   = Clipper.Engine.TPolyTree;
  TPolyTreeD  = Clipper.Engine.TPolyTreeD;
  TJoinType   = Clipper.Offset.TJoinType;
  TEndType    = Clipper.Offset.TEndType;
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

function BooleanOp(clipType: TClipType; fillRule: TFillRule;
  const subjects, clips: TPaths64): TPaths64; overload;
function BooleanOp(clipType: TClipType; fillRule: TFillRule;
  const subjects, clips: TPathsD; decimalPrec: integer = 2): TPathsD; overload;

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
  jt: TJoinType = jtRound; et: TEndType = etPolygon): TPaths64; overload;
function InflatePaths(const paths: TPathsD; delta: Double;
  jt: TJoinType = jtRound; et: TEndType = etPolygon): TPathsD; overload;

function MinkowskiSum(const pattern, path: TPath64;
  pathIsClosed: Boolean): TPaths64;

function PolyTreeToPaths(PolyTree: TPolyTree): TPaths64;
function PolyTreeDToPathsD(PolyTree: TPolyTreeD): TPathsD;

function MakePath(const ints: TArrayOfInteger): TPath64; overload;
function MakePath(const dbls: TArrayOfDouble): TPathD; overload;

implementation

uses
  Clipper.Minkowski;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function MakePath(const ints: TArrayOfInteger): TPath64;
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

function MakePath(const dbls: TArrayOfDouble): TPathD; overload;
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

procedure AddPolyNodeToPaths(Poly: TPolyPath; var Paths: TPaths64);
var
  i: Integer;
begin
  if (Length(Poly.Polygon) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Polygon;
  end;
  for i := 0 to Poly.ChildCount - 1 do
    AddPolyNodeToPaths(TPolyPath(Poly.Child[i]), Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPaths(PolyTree: TPolyTree): TPaths64;
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
  for i := 0 to Poly.ChildCount - 1 do
    AddPolyNodeToPathsD(TPolyPathD(Poly.Child[i]), Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeDToPathsD(PolyTree: TPolyTreeD): TPathsD;
begin
  Result := nil;
  AddPolyNodeToPathsD(PolyTree, Result);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function BooleanOp(clipType: TClipType; fillRule: TFillRule;
  const subjects, clips: TPaths64): TPaths64;
begin
  with TClipper.Create do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function BooleanOp(clipType: TClipType; fillRule: TFillRule;
  const subjects, clips: TPathsD; decimalPrec: integer = 2): TPathsD;
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

function Intersect(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctIntersection, fillRule, subjects, clips);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctUnion, fillRule, subjects, clips);
end;
//------------------------------------------------------------------------------

function Union(const subjects: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctUnion, fillRule, subjects, nil);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctDifference, fillRule, subjects, clips);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctXor, fillRule, subjects, clips);
end;
//------------------------------------------------------------------------------

function Intersect(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctIntersection, fillRule, subjects, clips, decimalPrec);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctUnion, fillRule, subjects, clips, decimalPrec);
end;
//------------------------------------------------------------------------------

function Union(const subjects: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctUnion, fillRule, subjects, nil, decimalPrec);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctDifference, fillRule, subjects, clips, decimalPrec);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctXor, fillRule, subjects, clips, decimalPrec);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPaths64; delta: Double;
  jt: TJoinType; et: TEndType): TPaths64;
var
  pp: TPathsD;
const
  scale = 100; invScale = 0.01;
begin
  pp := ScalePathsD(paths, scale, scale);
  with TClipperOffset.Create do
  try
    AddPaths(pp, jt, et);
    pp := Execute(delta * scale);
  finally
    free;
  end;
  Result := ScalePaths(pp, invScale, invScale);
end;
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPathsD; delta: Double;
  jt: TJoinType; et: TEndType): TPathsD;
var
  co: TClipperOffset;
begin
  co := TClipperOffset.Create();
  try
    co.AddPaths(paths, jt, et);
    Result := co.Execute(delta);
  finally
    co.free;
  end;
end;
//------------------------------------------------------------------------------

function MinkowskiSum(const pattern, path: TPath64;
  pathIsClosed: Boolean): TPaths64;
begin
 Result := Clipper.Minkowski.MinkowskiSum(pattern, path, pathIsClosed);
end;
//------------------------------------------------------------------------------

end.

