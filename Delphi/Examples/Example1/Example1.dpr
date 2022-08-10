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
  setLength(subj, 1);
  setLength(clip, 1);
  //make 2 random self-intersecting paths
  subj[0] := MakeRandomPath(displayWidth, displayHeight, edgeCount);
  clip[0] := MakeRandomPath(displayWidth, displayHeight, edgeCount);

  solution := Intersect(subj, clip, fillRule);
  //solution := Union(subj, clip, fillRule);
  //solution := Difference(subj, clip, fillRule);

  with TSimpleClipperSvgWriter.Create(fillRule) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(solution, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('Sample1.svg');
  finally
    free;
  end;
  ShellExecute(0, 'open','Sample1.svg', nil, nil, SW_SHOW);

end.
