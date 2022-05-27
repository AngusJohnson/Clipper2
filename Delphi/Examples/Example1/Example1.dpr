program Example1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  Clipper in '..\..\Clipper2Lib\Clipper.pas',
  Clipper.Core in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper.Engine in '..\..\Clipper2Lib\Clipper.Engine.pas',
  Clipper.SVG in '..\..\Utils\Clipper.SVG.pas',
  ClipMisc in '..\..\Utils\ClipMisc.pas';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure DoRandomPaths1(const caption: string; maxWidth, MaxHeight, edgeCount: integer;
  fillRule: TFillRule);
var
  subj,clip, sol: TPaths64;
begin
  //make 2 random self-intersecting paths of 'edgeCount' length
  setLength(subj, 1);
  subj[0] := MakeRandomPath(maxWidth, maxHeight, edgeCount);
  setLength(clip, 1);
  clip[0] := MakeRandomPath(maxWidth, maxHeight, edgeCount);
  sol := Intersect(subj, clip, fillRule);

  with SimpleClipperSvgWriter.Create(fillRule) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile(caption + '.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure Star7EvenOdd;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakeNPointedStar(Rect64(0,0,500,500), 7);
  sol := Union(subj, frEvenOdd);

  with SimpleClipperSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('Star7EvenOdd.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure Star7NonZero;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakeNPointedStar(Rect64(0,0,500,500), 7);
  sol := Union(subj, frNonZero);

  with SimpleClipperSvgWriter.Create(frNonZero) do
  try
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('Star7NonZero.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure StarCircleUnion;
var
  subj, clip, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakeNPointedStar(Rect64(0,0,500,500), 7);
  SetLength(clip, 1);
  clip[0] := Ellipse(Rect64(100,100,400,400));
  sol := Union(subj, clip, frNonZero);

  with SimpleClipperSvgWriter.Create(frNonZero) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('StarCircleUnion.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure StarCircleIntersectEO;
var
  subj, clip, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakeNPointedStar(Rect64(0,0,500,500), 7);
  SetLength(clip, 1);
  clip[0] := Ellipse(Rect64(100,100,400,400));
  sol := Intersect(subj, clip, frEvenOdd);

  with SimpleClipperSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('StarCircleIntesectEO.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure InflateClosedCircle;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := Ellipse(Rect64(0,0,450,450), 7);
  sol := InflatePaths(subj, 25, jtRound, etPolygon);

  with SimpleClipperSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('InflateClosedCircle.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure InflateJoinedOpenCircle;
var
  subjOpen, sol: TPaths64;
begin
  SetLength(subjOpen, 1);
  subjOpen[0] := Ellipse(Rect64(0,0,450,450), 7);
  sol := InflatePaths(subjOpen, 25, jtRound, etJoined);

  with SimpleClipperSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(subjOpen, true, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('InflateOpenJoinedCircle.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure InflateOpenCircle;
var
  subjOpen, sol: TPaths64;
begin
  SetLength(subjOpen, 1);
  subjOpen[0] := Ellipse(Rect64(0,0,450,450), 7);
  sol := InflatePaths(subjOpen, 25, jtRound, etRound);

  with SimpleClipperSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(subjOpen, true, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('InflateOpenCircle.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure MinkowskiSum1;
var
  circle, paths, sol: TPaths64;
begin
  SetLength(circle, 1);
  circle[0] := Ellipse(Rect64(-10,-10,10,10));
  SetLength(paths, 2);
  paths[0] := MakePath([40,40, 100,160, 160,40]);     //triangle
  paths[1] := MakePath([0,0, 200,0, 200,200, 0,200]); //square

  sol := MinkowskiSum(circle[0], paths[0], true);
  AppendPaths(sol, MinkowskiSum(circle[0], paths[1], true));

  with SimpleClipperSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(paths, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(circle, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('MinkowskiSum.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

begin
  Randomize;

  WriteLn('RandomPaths 1');
  DoRandomPaths1('Random20 EvenOdd', 800,600, 50, frEvenOdd);

  WriteLn('Star5 EvenOdd');
  Star7EvenOdd;

  WriteLn('Star5 NonZero');
  Star7NonZero;

  WriteLn('StarCircle Union');
  StarCircleUnion;

  WriteLn('StarCircle Intersect EO');
  StarCircleIntersectEO;

  WriteLn('Inflate Closed (polygon) Circle');
  InflateClosedCircle;

  WriteLn('Inflate Joined Open Circle');
  InflateJoinedOpenCircle;

  WriteLn('Inflate Open Circle');
  InflateOpenCircle;

  WriteLn('Minkowski Sum');
  MinkowskiSum1;

  ShellExecute(0, 'open',PChar(ExtractFilePath(paramstr(0))), nil, nil, SW_SHOW);
end.
