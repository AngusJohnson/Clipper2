program Example2;

{$I Clipper.DLL.inc}
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

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure DoRandomPaths(const caption: string; maxWidth, MaxHeight, edgeCount: integer;
  fillRule: TFillRule);
var
  subj,clip, sol, sol_open: CPaths64;
begin
  //make 2 random self-intersecting paths of 'edgeCount' length
  subj := MakeRandomPaths(maxWidth, maxHeight, [edgeCount]);
  clip := MakeRandomPaths(maxWidth, maxHeight, [edgeCount]);

  Clipper.DLL.BooleanOp64(TCLipType.Intersection, fillRule, subj, nil, clip, sol, sol_open);
  with TSvgWriter.Create(fillRule) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\' + caption + '.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol);
  DisposeExternalCPaths64(sol_open);


  Subj.Free;
  clip.Free;
end;
//------------------------------------------------------------------------------

procedure Star5EvenOdd;
var
  subj, sol, sol_open: CPaths64;
begin


  subj := TPaths64.Create([5]);

  FillWithStar(subj.FirstPath, TRect64.Create(0, 0, 500, 500));
  Clipper.DLL.BooleanOp64(TClipType.Union, TFillRule.EvenOdd, subj, nil, nil, sol, sol_open);

  with TSvgWriter.Create(TFillRule.EvenOdd) do
  try
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\Star5EvenOdd.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol);
  DisposeExternalCPaths64(sol_open);

  Subj.Free;

end;
//------------------------------------------------------------------------------

procedure Star5NonZero;
var
  subj, sol, sol_open: CPaths64;
begin
  subj := TPaths64.Create([5]);
  FillWithStar(subj.FirstPath, TRect64.Create(0, 0, 500, 500));
  Clipper.DLL.BooleanOp64(TClipType.Union, TFillRule.NonZero, subj, nil, nil, sol, sol_open);


  with TSvgWriter.Create(TFillRule.NonZero) do
  try
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\Star5NonZero.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol);
  DisposeExternalCPaths64(sol_open);

  Subj.Free;
end;
//------------------------------------------------------------------------------

procedure StarCircleUnion;
var
  subj, clip, sol, sol_open: CPaths64;
  SegmentCount: integer;
begin

  subj := TPaths64.Create([5]);
  FillWithStar(subj.FirstPath, TRect64.Create(0, 0, 500, 500));

  SegmentCount := Round(Pi * Sqrt(500 + 500));
  clip := TPaths64.Create([SegmentCount]);
  FillWithEllipse(clip.FirstPath, TRect64.Create(100, 100, 400, 400));


  Clipper.DLL.BooleanOp64(TClipType.Union, TFillRule.NonZero, subj, nil, clip, sol, sol_open);

  with TSvgWriter.Create(TFillRule.NonZero) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\StarCircleUnion.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol);
  DisposeExternalCPaths64(sol_open);

  subj.Free;
  clip.Free;
end;
//------------------------------------------------------------------------------

procedure StarCircleIntersectEO;
var
  subj, clip, sol, sol_open: CPaths64;
  SegmentCount: integer;
begin

  subj := TPaths64.Create([5]);
  FillWithStar(subj.FirstPath, TRect64.Create(0, 0, 500, 500));

  SegmentCount := Max(4, Round(Pi * Sqrt(500 + 500)));
  clip := TPaths64.Create([SegmentCount]);
  FillWithEllipse(clip.FirstPath, TRect64.Create(100, 100, 400, 400));


  Clipper.DLL.BooleanOp64(TClipType.Intersection, TFillRule.EvenOdd, subj, nil, clip, sol, sol_open);

  with TSvgWriter.Create(TFillRule.EvenOdd) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\StarCircleIntesectEO.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol);
  DisposeExternalCPaths64(sol_open);

  subj.Free;
  clip.Free;
end;


//------------------------------------------------------------------------------

procedure InflateClosedCircle;
var
  subj, sol: CPaths64;
begin
  subj := TPaths64.Create([7]);
  FillWithEllipse(subj.FirstPath, TRect64.Create(0, 0, 450 ,450));

  sol := Clipper.DLL.InflatePaths64(subj, 25, TJoinType.Round, TEndType.Polygon);

  with TSvgWriter.Create(TFillRule.EvenOdd	) do
  try
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\InflateClosedCircle.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol);
  subj.Free;
end;
//------------------------------------------------------------------------------

procedure InflateJoinedOpenCircle;
var
  subjOpen, sol: CPaths64;
begin
  subjOpen := TPaths64.Create([7]);
  FillWithEllipse(subjOpen.FirstPath, TRect64.Create(0, 0, 450 ,450));

  sol := Clipper.DLL.InflatePaths64(subjOpen, -25, TJoinType.Round, TEndType.Joined);

  with TSvgWriter.Create(TFillRule.EvenOdd) do
  try
    AddPaths(subjOpen, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\InflateOpenJoinedCircle.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol);
  subjOpen.Free;
end;


//------------------------------------------------------------------------------

procedure InflateOpenCircle;
var
  subjOpen, sol: CPaths64;
begin
  subjOpen := TPaths64.Create([7]);
  FillWithEllipse(subjOpen.FirstPath, TRect64.Create(0, 0, 450 ,450));

  sol := Clipper.DLL.InflatePaths64(subjOpen, 25, TJoinType.Round, TEndType.Round);

  with TSvgWriter.Create(TFillRule.EvenOdd) do
  try
    AddPaths(subjOpen, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('.\SVG\InflateOpenCircle.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol);
  subjOpen.Free;
end;

//------------------------------------------------------------------------------

procedure MinkowskiSum1;
var
  circle, paths, sol1, sol2: CPaths64;
  path: CPath64;
  SegmentCount: integer;
begin
  SegmentCount := Round(Pi * Sqrt(20 {h} + 20 {w})); //

  circle := TPaths64.Create([segmentCount]);
  FillWithEllipse(circle.FirstPath, TRect64.Create(-10, -10, 10, 10));


  paths := TPaths64.Create([3, 4]);
  path := paths.FirstPath;
  path.Vertex[0].SetValues(40, 40);
  path.Vertex[1].SetValues(100, 160);
  path.Vertex[2].SetValues(160, 40);


  Path := paths.Path[1];
  path.Vertex[0].SetValues(0, 0);
  path.Vertex[1].SetValues(200, 0);
  path.Vertex[2].SetValues(200, 200);
  path.Vertex[3].SetValues(0, 200);


  sol1 := Clipper.DLL.MinkowskiSum64(circle.FirstPath, Paths.FirstPath, true);
  sol2 := Clipper.DLL.MinkowskiSum64(circle.FirstPath, Paths.Path[1], true);

  with TSvgWriter.Create(TFillRule.EvenOdd) do
  try
    AddPaths(paths, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(circle, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(sol1, false, $2000FF00, $FF006600, 0.8);
    AddPaths(sol2, false, $2000FF00, $FF006600, 0.8);

    SaveToFile('.\SVG\MinkowskiSum.svg');
  finally
    free;
  end;

  DisposeExternalCPaths64(sol1);
  DisposeExternalCPaths64(sol2);

  circle.Free;
  Paths.Free;
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
  DoRandomPaths('Random20 EvenOdd', 800,600, 50, TFillRule.NonZero	);

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
