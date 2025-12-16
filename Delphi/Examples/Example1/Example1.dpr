program Example1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Clipper             in '..\..\Clipper2Lib\Clipper.pas',
  Clipper.Core        in '..\..\Clipper2Lib\Clipper.Core.pas',
  ClipMisc            in '..\..\Utils\ClipMisc.pas',
  Clipper.SVG         in '..\..\Utils\Clipper.SVG.pas';

const
  edgeCount = 50;
var
  subj, clip, solution: TPathsD;
begin

  SetLength(subj, 1);
  subj[0] := MakePathD([200,100, 20,158, 130,4, 130,196, 20,42]);
  SetLength(clip, 1);
  clip[0] := MakePathD([196,126, 8,136, 154,16, 104,200, 38,24]);
  solution := Intersect( subj, clip, frNonZero);
  DisplayAsSvg('Intersect (nonzero)', frNonZero, subj, clip, solution);

  Randomize;

  // make 2 random self-intersecting paths and clip them
  // using Intersection(), Union() and Difference()

  setLength(subj, 1);
  setLength(clip, 1);
  subj[0] := MakeRandomPathD(800, 600, edgeCount);
  clip[0] := MakeRandomPathD(800, 600, edgeCount);


  solution := Intersect(subj, clip, frNonZero);
  DisplayAsSvg('Random Intersect (nonzero)', frNonZero, subj, clip, solution);

  solution := Union(subj, clip, frNonZero);
  DisplayAsSvg('Random Union (nonzero)', frNonZero, subj, clip, solution);

  solution := Difference(subj, clip, frNonZero);
  DisplayAsSvg('Random Difference (nonzero)', frNonZero, subj, clip, solution);

  // inflate an open path using different join and end styles ...

  with TSvgWriter.Create(frNonZero) do
  try
    setLength(subj, 1);
    subj[0] := MakePathD([40,60, 20,20, 180,20, 180,70, 25,150, 20,180, 180,180]);
    solution := InflatePaths(subj, 20, jtMiter, etSquare, 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Miter Joins; Square Ends', 10, 220);

    subj := Clipper.TranslatePaths(subj, 210, 0);
    solution := InflatePaths(subj, 20, jtSquare, etSquare, 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Square Joins; Square Ends', 220, 220);

    subj := Clipper.TranslatePaths(subj, 210, 0);
    solution := InflatePaths(subj, 20, jtBevel, etButt, 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Bevel Joins; Butt Ends', 430, 220);

    subj := Clipper.TranslatePaths(subj, 210, 0);
    solution := InflatePaths(subj, 20, jtRound, etRound, 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Round Joins; Round Ends', 640, 220);

    SaveToFile('offsets.svg', 800,600, 40);
  finally
    free;
  end;
  ShellExecute(0, 'open','offsets.svg', nil, nil, SW_SHOW);

end.
