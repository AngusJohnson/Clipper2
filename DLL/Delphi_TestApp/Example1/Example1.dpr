program Example1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  Clipper.DLL in '..\..\Delphi_interface\Clipper.DLL.pas',
  Clipper.DLL.Utils in '..\..\Delphi_interface\Utils\Clipper.DLL.Utils.pas',
  Clipper.DLL.SVG in '..\..\Delphi_interface\SVG\Clipper.DLL.SVG.pas';

var
  subj, clip, solution, sol_open: CPaths64;
  path: CPath64;
  i, j: integer;
const
  displayWidth  = 800;
  displayHeight = 600;
  edgeCount     = 100;
  fillRule      = TFillRule.NonZero;
begin
  Randomize;

  subj :=   MakeRandomPaths(displayWidth, displayHeight, [edgeCount]);
  clip := MakeRandomPaths(displayWidth, displayHeight, [edgeCount]);

  BooleanOp64(TClipType.Intersection, fillrule, subj, nil, clip, solution, sol_open);

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

  DisposeExternalCPaths64(solution);
  DisposeExternalCPaths64(sol_open);

  subj.Free;

  subj := TPaths64.Create([7]);

  subj.FirstPath.Vertex[0].SetValues(40, 60);
  subj.FirstPath.Vertex[1].SetValues(20, 20);
  subj.FirstPath.Vertex[2].SetValues(180, 20);
  subj.FirstPath.Vertex[3].SetValues(180, 70);
  subj.FirstPath.Vertex[4].SetValues(25, 150);
  subj.FirstPath.Vertex[5].SetValues(20, 180);
  subj.FirstPath.Vertex[6].SetValues(180, 180);

  solution := InflatePaths64(subj, 20, TJointype.Miter, TEndType.Square, 3);

  with TSvgWriter.Create(fillRule) do
  try
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Miter Joins; Square Ends', 10, 210);

    for i := 0 to subj.Count-1 do
    begin
      path := subj.Path[i];
      for j := 0 to path.VertexCount-1 do
        Path.Vertex[j].x := Path.Vertex[j].x + 210;
    end;

    solution := InflatePaths64(subj, 20, TJointype.Square, TEndType.Square, 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths(solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Square Joins; Square Ends', 220, 210);

    for i := 0 to subj.Count-1 do
    begin
      path := subj.Path[i];
      for j := 0 to path.VertexCount-1 do
        Path.Vertex[j].x := Path.Vertex[j].x + 210;
    end;

    solution := InflatePaths64(subj, 20, TJointype.Bevel, TEndType.Butt  , 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Bevel Joins; Butt Ends', 430, 210);

    for i := 0 to subj.Count-1 do
    begin
      path := subj.Path[i];
      for j := 0 to path.VertexCount-1 do
        Path.Vertex[j].x := Path.Vertex[j].x + 210;
    end;

    solution := InflatePaths64(subj, 20, TJointype.Round, TEndType.Round  , 3);
    AddPaths(subj, true, $2000BBFF, $800033FF, 0.8);
    AddPaths( solution, false, $2000FF00, $FF006600, 1.2, false);
    AddText('Round Joins; Round Ends', 640, 210);
    SaveToFile('offsets.svg', 800,600, 40);
  finally
    free;
  end;
  ShellExecute(0, 'open','offsets.svg', nil, nil, SW_SHOW);
  subj.Free;
  clip.Free;
end.
