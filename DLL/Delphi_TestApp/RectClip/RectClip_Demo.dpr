program RectClip_Demo;

{$I Clipper.DLL.inc}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,

  Clipper.DLL in '..\Delphi_headers\Clipper.DLL.pas',
  Clipper.DLL.SVG in '..\Delphi_headers\SVG\Clipper.DLL.SVG.pas',
  Clipper.DLL.Data in '..\Delphi_headers\Clipper.DLL.Data.pas',
  Clipper.DLL.Utils in '..\Delphi_headers\Utils\Clipper.DLL.Utils.pas';

const
  width   = 800;
  height  = 600;
  fillrule: TFillRule = TFillRule.EvenOdd;

  procedure TestEllipse(radius, count: integer);
  var
    i: integer;
    sub, clp,  sol: CPaths64;
    ellipse_base, ellipse: CPath64;
    rec: TRect64;
    segmentcount: integer;
    NumberOfVerticesPerPath: TArray<integer>;
   j: Integer;
    dx, dy: integer;
  const
    margin: integer = 100;
  begin
    segmentcount := round(pi * sqrt(2*radius));

    setLength(NumberOfVerticesPerPath, count);
    for i := 0 to Count-1 do
      NumberOfVerticesPerPath[i] := segmentcount;


    sub := TPaths64.Create(NumberOfVerticesPerPath);

    ellipse_base := sub.FirstPath;
    FillWithEllipse(ellipse_base, TRect64.Create(0, 0, radius, radius)); // this base ellipse is also part of the pool of ellipses and moved after the others

    for i := 1 to count -1 do
    begin

      ellipse := sub.path[i];

      dx := Random(width - radius);
      dy := Random(height - radius);

      for j := 0 to segmentcount-1 do
        ellipse.Vertex[j].SetValues(ellipse_base.Vertex[j].X + dx, ellipse_base.Vertex[j].Y + dy);
    end;

    // also move the first ellipse
    dx := Random(width - radius);
    dy := Random(height - radius);

    for j := 0 to segmentcount-1 do
      ellipse_base.Vertex[j].SetValues(ellipse_base.Vertex[j].X + dx, ellipse_base.Vertex[j].Y + dy);

    Rec := TRect64.Create(margin, margin, width - margin, height - margin);
    clp := TPaths64.Create([4]);
    FillWithRectData(clp.FirstPath, Rec);

    sol := Clipper.DLL.RectClip64(rec, sub);
    //sol := RectClip(rec, sub);

    //display
    with TSvgWriter.Create(fillrule) do
    try
      AddPaths(sub, false, $100066FF, $400066FF, 1);
      AddPaths(clp, false, $10FFAA00, $FFFF0000, 1);
      AddPaths(sol, false, $8066FF66, $FF006600, 1);
      SaveToFile('RectClip1.svg', width, height);
    finally
      Free;
    end;

    DisposeExternalCPaths64(sol);
    sub.Free;
    clp.Free;

    ShellExecute(0, 'open','RectClip1.svg', nil, nil, SW_SHOW);
  end;

  {$IFDEF CLIPPER_DOUBLE_COORDINATES}
  procedure TestRandomPoly(count: integer);
  var
    sub, clp, sol: CPathsD;
    rec: TRectD;
  const
    margin: integer = 100;
  begin
    rec := TRectD.Create(margin, margin, width - margin, height - margin);

    clp := TPathsD.Create([4]);
    FillWithRectDataD(clp.FirstPath, rec);


    sub := MakeRandomPathsD(width, height, [count]);

    sol := Clipper.DLL.RectClipD(rec, sub);

    //display
    with TSvgWriter.Create(fillrule) do
    try
      AddPaths(sub, false, $100066FF, $400066FF, 1);
      AddPaths(clp, false, $10FFAA00, $FFFF0000, 1);
      AddPaths(sol, false, $8066FF66, $FF006600, 1);
      SaveToFile('RectClip2.svg', width, height);
    finally
      Free;
    end;
    ShellExecute(0, 'open','RectClip2.svg', nil, nil, SW_SHOW);

    DisposeExternalCPathsD(sol);
    clp.Free;
    sub.Free;
  end;

  procedure TestLineClip(lineLength: integer);
  var
    sub, clp, sol: CPathsD;
    rec: TRectD;
  const
    margin: integer = 100;
  begin

    clp := TPathsD.Create([4]);
    rec := TRectD.Create(margin, margin, width - margin, height - margin);
    FillWithRectDataD(clp.FirstPath, rec);

    sub := MakeRandomPathsD(width, height, [linelength]);
    sol := Clipper.DLL.RectClipD(rec, sub);

    //display
    with TSvgWriter.Create(fillrule) do
    try
      AddPaths(sub, true, $0, $AA0066FF, 1);
      AddPaths(clp, false, $10FFAA00, $FFFF0000, 1);
      AddPaths(sol, true, $0, $FF006600, 2.0);

      SaveToFile('RectClipQ.svg', width, height);
    finally
      Free;
    end;
    ShellExecute(0, 'open','RectClipQ.svg', nil, nil, SW_SHOW);

    DisposeExternalCPathsD(sol);
    clp.Free;
    sub.Free;


  end;
 {$ENDIF}

begin
  Randomize;
  TestEllipse(100, 100);
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  TestRandomPoly(59);
  TestLineClip(59);
{$ENDIF}

end.
