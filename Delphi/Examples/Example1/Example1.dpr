program Example1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  Clipper             in '..\..\Clipper2Lib\Clipper.pas',
  Clipper.Core        in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper.Offset      in '..\..\Clipper2Lib\Clipper.Offset.pas',
  Clipper.Engine      in '..\..\Clipper2Lib\Clipper.Engine.pas',
  Clipper.Minkowski   in '..\..\Clipper2Lib\Clipper.Minkowski.pas',
  Clipper.SVG         in '..\..\Utils\Clipper.SVG.pas',
  ClipMisc            in '..\..\Utils\ClipMisc.pas';

var
  subj, clip, solution: TPaths64;
const
  displayWidth  = 800;
  displayHeight = 600;
  edgeCount     = 100;
  fillRule      = frNonZero;
begin
  Randomize;

  //make 2 random self-intersecting paths
  setLength(subj, 1);
  setLength(clip, 1);
  subj[0] := MakeRandomPath(displayWidth, displayHeight, edgeCount);
  clip[0] := MakeRandomPath(displayWidth, displayHeight, edgeCount);

  solution := Intersect(subj, clip, fillRule);
  //solution := Union(subj, clip, fillRule);
  //solution := Difference(subj, clip, fillRule);

  with TSvgWriter.Create(fillRule) do
  try
    AddPaths(subj, false, $2000BBFF, $800033FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF3300, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    SaveToFile('Sample1.svg', 800,600, 40);
  finally
    free;
  end;
  ShellExecute(0, 'open','Sample1.svg', nil, nil, SW_SHOW);

  setLength(subj, 1);
  subj[0] := MakePath([40,60, 20,20, 180,20, 180,70, 25,150, 20,180, 180,180]);
  solution := InflatePaths(subj, 20, jtMiter, etSquare, 3);

  with TSvgWriter.Create(fillRule) do
  try
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Miter Joins; Square Ends', 10, 210);

    subj := Clipper.TranslatePaths(subj, 210, 0);
    solution := InflatePaths(subj, 20, jtSquare, etSquare, 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Square Joins; Square Ends', 220, 210);

    subj := Clipper.TranslatePaths(subj, 210, 0);
    solution := InflatePaths(subj, 20, jtBevel, etButt, 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Bevel Joins; Butt Ends', 430, 210);

    subj := Clipper.TranslatePaths(subj, 210, 0);
    solution := InflatePaths(subj, 20, jtRound, etRound, 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Round Joins; Round Ends', 640, 210);
    SaveToFile('offsets.svg', 800,600, 40);
  finally
    free;
  end;
  ShellExecute(0, 'open','offsets.svg', nil, nil, SW_SHOW);

end.
