program Example1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  Diagnostics,
  Clipper in '..\Clipper.pas',
  Clipper.SVG in '..\Clipper.SVG.pas';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure MakeSvgAndDisplay(const filename, caption: string;
  fillrule: TFillRule;
  const subj, subOpen, clip, solution: TPaths64);
begin
  with SimpleClipperSvgWriter.Create(fillrule) do
  try
    AddText(caption, 10, 20);
    if Assigned(subj) then
      AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    if Assigned(subOpen) then
      AddPaths(subOpen, true, $1000BBFF, $CC0099FF, 1.5);
    if Assigned(clip) then
      AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    if Assigned(solution) then
      AddPaths(solution, false, $2000FF00, $FF003300, 1.5);
    SaveToFile(filename);
  finally
    free;
  end;
  ShellExecute(0, nil, PChar(filename), Nil, Nil, SW_NORMAL);
end;
//------------------------------------------------------------------------------

function MakeRandomPath(maxWidth, maxHeight, count: Integer): TPath64;
var
  i: Integer;
begin
  setlength(Result, count);
  for i := 0 to count -1 do
    with Result[i] do
    begin
      X := Random(maxWidth);
      Y := Random(maxHeight);
    end;
end;
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
  MakeSvgAndDisplay(caption + '.svg',
    caption, fillRule, subj, nil, clip, sol);
end;
//------------------------------------------------------------------------------

procedure UnionBlocks;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 11);
  subj[0] := MakePath([ 100,100,200,100,200,200,100,200 ]);
  subj[1] := MakePath([ 200,100,300,100,300,200,200,200 ]);
  subj[2] := MakePath([ 300,100,400,100,400,200,300,200 ]);
  subj[3] := MakePath([ 400,100,500,100,500,200,400,200 ]);
  subj[4] := MakePath([ 500,100,600,100,600,200,500,200 ]);
  subj[5] := MakePath([ 100,200,200,200,200,300,100,300 ]);
  subj[6] := MakePath([ 300,200,400,200,400,300,300,300 ]);
  subj[7] := MakePath([ 100,300,200,300,200,400,100,400 ]);
  subj[8] := MakePath([ 200,300,300,300,300,400,200,400 ]);
  subj[9] := MakePath([ 300,300,400,300,400,400,300,400 ]);
  subj[10] := MakePath([ 400,300,500,300,500,400,400,400 ]);
  sol := Union(subj, frNonZero);
  MakeSvgAndDisplay('Blocks.svg',
    'Blocks', frNonZero, subj, nil, nil, sol);
end;
//------------------------------------------------------------------------------

procedure JoiningEdges;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakePath([ 450,200,400,300,750,300,
    100,450,150,250,600,150,750,300,250,300,350,350,600,300 ]);
  sol := Union(subj, frEvenOdd);
  MakeSvgAndDisplay('JoiningEdges.svg',
    'JoiningEdges', frNonZero, subj, nil, nil, sol);
end;
//------------------------------------------------------------------------------

procedure Star5EvenOdd;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakePath([500, 250, 50, 395, 325, 10, 325, 490, 50, 105]);
  sol := Union(subj, frEvenOdd);
  MakeSvgAndDisplay('Star5EvenOdd.svg',
    'Star5 EvenOdd', frEvenOdd, subj, nil, nil, sol);
end;
//------------------------------------------------------------------------------

procedure Star5NonZero;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakePath([500, 250, 50, 395, 325, 10, 325, 490, 50, 105]);
  sol := Union(subj, frNonZero);
  MakeSvgAndDisplay('Star5NonZero.svg',
    'Star5 NonZero', frNonZero, subj, nil, nil, sol);
end;
//------------------------------------------------------------------------------

procedure StarCircleUnion;
var
  subj, clip, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakePath([500,250,50,395,325,10,325,490,50,105]);
  SetLength(clip, 1);
  clip[0] := MakePath([400,250,345,365,215,395,115,315,115,185,215,105,345,135]);
  sol := Union(subj, clip, frNonZero);
  MakeSvgAndDisplay('StarCircleUnion.svg',
    'StarCircle Union', frNonZero, subj, nil, clip, sol);
end;
//------------------------------------------------------------------------------

procedure StarCircleIntersectEO;
var
  subj, clip, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakePath([500,250,50,395,325,10,325,490,50,105]);
  SetLength(clip, 1);
  clip[0] := MakePath([400,250,345,365,215,395,115,315,115,185,215,105,345,135]);
  sol := Intersect(subj, clip, frEvenOdd);
  MakeSvgAndDisplay('StarCircleIntesectEO.svg',
    'StarCircle Intesect EvenOdd', frEvenOdd, subj, nil, clip, sol);
end;
//------------------------------------------------------------------------------

procedure InflateClosedCircle;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := MakePath([400,250,345,365,215,395,115,315,115,185,215,105,345,135]);
  sol := InflatePaths(subj, 25, jtRound, etPolygon);
  MakeSvgAndDisplay('InflateClosedCircle.svg',
    'Inflate Closed (polygon) Circle', frEvenOdd, subj, nil, nil, sol);
end;
//------------------------------------------------------------------------------

procedure InflateJoinedOpenCircle;
var
  subjOpen, sol: TPaths64;
begin
  SetLength(subjOpen, 1);
  subjOpen[0] := MakePath([400,250,345,365,215,395,115,315,115,185,215,105,345,135]);
  sol := InflatePaths(subjOpen, 25, jtRound, etJoined);
  MakeSvgAndDisplay('InflateOpenJoinedCircle.svg',
    'Inflate Open and Joined Circle', frEvenOdd, nil, subjOpen, nil, sol);
end;
//------------------------------------------------------------------------------

procedure InflateOpenCircle;
var
  subjOpen, sol: TPaths64;
begin
  SetLength(subjOpen, 1);
  subjOpen[0] := MakePath([400,250,345,365,215,395,115,315,115,185,215,105,345,135]);
  sol := InflatePaths(subjOpen, 25, jtRound, etRound);
  MakeSvgAndDisplay('InflateOpenCircle.svg',
    'Inflate Open Circle', frEvenOdd, nil, subjOpen, nil, sol);
end;
//------------------------------------------------------------------------------

var
  s: string;
begin
  Randomize;

  WriteLn('RandomPaths 1');
  DoRandomPaths1('Random20 EvenOdd', 800,600, 20, frEvenOdd);

  WriteLn('RandomPaths 2');
  DoRandomPaths1('Random20 NonZero', 800,600, 20, frNonZero);

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

  WriteLn('UnionBlocks');
  UnionBlocks;

  //WriteLn('JoiningEdges');
  //JoiningEdges;

  WriteLn('');
  WriteLn('Finished. Press Enter to exit.');
  ReadLn(s);
end.
