program Example2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  SysUtils,
  Clipper.Core in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper in '..\..\Clipper2Lib\Clipper.pas',
  ClipMisc in '..\..\Utils\ClipMisc.pas',
  Clipper.SVG in '..\..\Utils\Clipper.SVG.pas';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure DeflateRabbit;
var
  p, sol: TPathsD;
begin
  sol := GetPathsFromVerySimpleSVG('rabbit.svg');
  sol := SimplifyPaths(sol, 0.25);
  p := sol;
  while (Length(p) > 0) do
  begin
    p := InflatePaths(p, -5, jtRound, etPolygon);
    // SimplifyPaths is not essential but is **highly recommended**
    // because it speeds up the loop by removing tiny artefacts
    p := SimplifyPaths(p, 0.25);
    AppendPaths(sol, p);
  end;
  DisplayAsSvg('DeflatedRabbit', frEvenOdd, sol, nil, sol, true);
end;
//------------------------------------------------------------------------------

procedure InflateClosed5PtCircle;
var
  subj, sol: TPaths64;
begin
  SetLength(subj, 1);
  subj[0] := Ellipse(Rect64(0, 0, 450, 450), 5);
  sol := InflatePaths(subj, 25, jtRound, etPolygon);
  DisplayAsSvg('Inflate 1', frEvenOdd, subj, nil, sol);
end;
//------------------------------------------------------------------------------

procedure InflateJoinedOpen5PtCircle;
var
  subjOpen, sol: TPaths64;
begin
  SetLength(subjOpen, 1);
  subjOpen[0] := Ellipse(Rect64(0, 0, 450, 450), 5);
  sol := InflatePaths(subjOpen, 12.5, jtRound, etJoined);
  DisplayAsSvg_Open('Inflate 2', frEvenOdd, true, subjOpen, nil, sol, false);
end;
//------------------------------------------------------------------------------

procedure InflateOpen5PtCircle;
var
  subjOpen, sol: TPaths64;
begin
  SetLength(subjOpen, 1);
  subjOpen[0] := Ellipse(Rect64(0, 0, 450, 450), 5);
  sol := InflatePaths(subjOpen, 12.5, jtRound, etRound);
  DisplayAsSvg_Open('Inflate 3',
    frEvenOdd, false, subjOpen, nil, sol, false);
end;
//------------------------------------------------------------------------------

procedure MinkowskiSum1;
var
  circle, paths, sol: TPaths64;
begin
  SetLength(circle, 1);
  circle[0] := Ellipse(Rect64(-13,-13,13,13));
  SetLength(paths, 2);
  paths[0] := MakePath([40,40, 100,160, 160,40]);     //triangle
  paths[1] := MakePath([0,0, 200,0, 200,200, 0,200]); //square

  sol := Clipper.MinkowskiSum(circle[0], paths[0], true);
  AppendPaths(sol, Clipper.MinkowskiSum(circle[0], paths[1], true));
  DisplayAsSvg_Open('MinkowskiSum', frEvenOdd, false, paths, circle, sol, false);
end;
//------------------------------------------------------------------------------

begin
  if not DirectoryExists('.\SVG') then
    MkDir('SVG');
  InflateClosed5PtCircle;
  InflateJoinedOpen5PtCircle;
  InflateOpen5PtCircle;
  MinkowskiSum1;
  DeflateRabbit;
end.
