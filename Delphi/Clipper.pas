unit Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (release candidate 1)                                      *
* Date      :  14 February 2022                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This module provides a simple interface to the Clipper Library  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Math, Clipper.Core, Clipper.Engine, Clipper.Offset;

type
  TPoint64    = Clipper.Core.TPoint64;
  TRect64     = Clipper.Core.TRect64;
  TPath       = Clipper.Core.TPath64;
  TPaths      = Clipper.Core.TPaths64;

  TPointD     = Clipper.Core.TPointD;
  TRectD      = Clipper.Core.TRectD;
  TPathD      = Clipper.Core.TPathD;
  TPathsD     = Clipper.Core.TPathsD;

  TPolyTree   = Clipper.Engine.TPolyTree;
  TPolyTreeD  = Clipper.Engine.TPolyTreeD;

  TFillRule = Clipper.Core.TFillRule;

function BooleanOp(clipType: TClipType; fillRule: TFillRule;
  const subjects, clips: TPaths): TPaths; overload;
function BooleanOp(clipType: TClipType; fillRule: TFillRule;
  const subjects, clips: TPathsD; decimalPrec: integer = 2): TPathsD; overload;

function Intersect(const subjects, clips: TPaths;
  fillRule: TFillRule): TPaths; overload;
function Union(const subjects, clips: TPaths;
  fillRule: TFillRule): TPaths; overload;
function Difference(const subjects, clips: TPaths;
  fillRule: TFillRule): TPaths; overload;
function XOR_(const subjects, clips: TPaths;
  fillRule: TFillRule): TPaths; overload;

function Intersect(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Union(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Difference(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function XOR_(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;

function InflatePaths(const paths: TPaths; delta: Double;
  jt: TJoinType = jtRound; et: TEndType = etPolygon): TPaths; overload;
function InflatePaths(const paths: TPathsD; delta: Double;
  jt: TJoinType = jtRound; et: TEndType = etPolygon): TPathsD; overload;


function PolyTreeToPaths(PolyTree: TPolyTree): TPaths;
function PolyTreeDToPathsD(PolyTree: TPolyTreeD): TPathsD;

implementation

procedure AddPolyNodeToPaths(Poly: TPolyPath; var Paths: TPaths);
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

function PolyTreeToPaths(PolyTree: TPolyTree): TPaths;
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
  const subjects, clips: TPaths): TPaths;
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
var
  scale: Double;
begin
  scale := Power(10, decimalPrec);
  with TClipperD.Create(scale) do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function Intersect(const subjects, clips: TPaths; fillRule: TFillRule): TPaths;
begin
  Result := BooleanOp(ctIntersection, fillRule, subjects, clips);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPaths; fillRule: TFillRule): TPaths;
begin
  Result := BooleanOp(ctUnion, fillRule, subjects, clips);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPaths; fillRule: TFillRule): TPaths;
begin
  Result := BooleanOp(ctDifference, fillRule, subjects, clips);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPaths; fillRule: TFillRule): TPaths;
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

function InflatePaths(const paths: TPaths; delta: Double;
  jt: TJoinType; et: TEndType): TPaths;
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
//------------------------------------------------------------------------------

end.

