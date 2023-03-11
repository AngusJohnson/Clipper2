program Example2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Math,

  Clipper.Core in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper.Engine in '..\..\Clipper2Lib\Clipper.Engine.pas',
  Clipper.Offset in '..\..\Clipper2Lib\Clipper.Offset.pas',
  Clipper.Minkowski in '..\..\Clipper2Lib\Clipper.Minkowski.pas',
  Clipper in '..\..\Clipper2Lib\Clipper.pas',

  ClipMisc in '..\..\Utils\ClipMisc.pas',
  Clipper.SVG in '..\..\Utils\Clipper.SVG.pas';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure DoRandomPaths(const caption: string; maxWidth, MaxHeight, edgeCount: integer;
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

  with TSvgWriter.Create(fillRule) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\' + caption + '.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure Star5EvenOdd;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakeNPointedStar(Rect64(0,0,500,500), 5);
  sol := Union(subj, frEvenOdd);

  with TSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\Star5EvenOdd.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure Star5NonZero;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakeNPointedStar(Rect64(0,0,500,500), 5);
  sol := Union(subj, frNonZero);

  with TSvgWriter.Create(frNonZero) do
  try
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\Star5NonZero.svg');
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
  subj[0] := MakeNPointedStar(Rect64(0,0,500,500), 5);
  SetLength(clip, 1);
  clip[0] := ClipMisc.Ellipse(Rect64(100,100,400,400));
  sol := Union(subj, clip, frNonZero);

  with TSvgWriter.Create(frNonZero) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\StarCircleUnion.svg');
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
  subj[0] := MakeNPointedStar(Rect64(0,0,500,500), 5);
  SetLength(clip, 1);
  clip[0] := ClipMisc.Ellipse(Rect64(100,100,400,400));
  sol := Intersect(subj, clip, frEvenOdd);

  with TSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\StarCircleIntesectEO.svg');
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
  subj[0] := ClipMisc.Ellipse(Rect64(0,0,450,450), 7);
  sol := InflatePaths(subj, 25, jtRound, etPolygon);
  with TSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\InflateClosedCircle.svg');
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
  subjOpen[0] := ClipMisc.Ellipse(Rect64(0,0,450,450), 7);
  sol := InflatePaths(subjOpen, -25, jtRound, etJoined);

  with TSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(subjOpen, true, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\InflateOpenJoinedCircle.svg');
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
  subjOpen[0] := ClipMisc.Ellipse(Rect64(0,0,450,450), 7);
  sol := InflatePaths(subjOpen, 25, jtRound, etRound);

  with TSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(subjOpen, true, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\InflateOpenCircle.svg');
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
  circle[0] := ClipMisc.Ellipse(Rect64(-10,-10,10,10));
  SetLength(paths, 2);
  paths[0] := MakePath([40,40, 100,160, 160,40]);     //triangle
  paths[1] := MakePath([0,0, 200,0, 200,200, 0,200]); //square

  sol := Clipper.MinkowskiSum(circle[0], paths[0], true);
  AppendPaths(sol, Clipper.MinkowskiSum(circle[0], paths[1], true));

  with TSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(paths, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(circle, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\MinkowskiSum.svg');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

var
  directory: string;
begin
  Randomize;
  directory := ExtractFilePath(Paramstr(0));
  directory := directory + '\SVG';
  if not DirectoryExists(directory) then
    MkDir(directory);

  WriteLn('RandomPaths 1');
  DoRandomPaths('Random20 EvenOdd', 800,600, 50, frNonZero);

  WriteLn('Star5 EvenOdd');
  Star5EvenOdd;

  WriteLn('Star5 NonZero');
  Star5NonZero;

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

//  WriteLn('Minkowski Sum');
//  MinkowskiSum1;

  ShellExecute(0, 'open',PChar(directory), nil, nil, SW_SHOW);
end.
