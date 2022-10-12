program RectClip_Demo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Clipper in '..\..\Clipper2Lib\Clipper.pas',
  Clipper.Core in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper.SVG in '..\..\Utils\Clipper.SVG.pas',
  Timer in '..\..\Utils\Timer.pas',
  ClipMisc in '..\..\Utils\ClipMisc.pas';

const
  width   = 800;
  height  = 600;
  fillrule: TFillRule = frEvenOdd;//frNonZero;//

  procedure TestEllipse(radius, count: integer);
  var
    i: integer;
    sub, clp, sol: TPaths64;
    rec: TRect64;
  begin
    SetLength(clp, 1);
    clp[0] := Clipper.Core.Ellipse(Rect64(0, 0, radius, radius));
    SetLength(sub, count);
    for i := 0 to count -1 do
      sub[i] := TranslatePath(clp[0],
        Random(width - radius), Random(height - radius));

    rec := Rect64(200, 200, width - 200, height - 200);
    clp[0] := rec.AsPath;
    sol := RectClip(rec, sub);

    //display
    with TSimpleClipperSvgWriter.Create(fillrule) do
    try
      AddPaths(sub, false, $100066FF, $400066FF, 1);
      AddPaths(clp, false, $10FFAA00, $FFFF0000, 1);
      AddPaths(sol, false, $8066FF66, $FF006600, 1);
      SaveToFile('RectClip1.svg', width, height);
    finally
      Free;
    end;
    ShellExecute(0, 'open','RectClip1.svg', nil, nil, SW_SHOW);
  end;

  procedure TestRandomPoly(count: integer);
  var
    sub, clp, sol: TPathsD;
    rec: TRectD;
  begin
    rec := RectD(200, 200, width - 200, height - 200);
    SetLength(clp, 1);
    clp[0] := rec.AsPath;
    SetLength(sub, 1);
    sub[0] := PathD(MakeRandomPath(width, height, count));
    sol := RectClip(rec, sub);

    //display
    with TSimpleClipperSvgWriter.Create(fillrule) do
    try
      AddPaths(sub, false, $100066FF, $400066FF, 1);
      AddPaths(clp, false, $10FFAA00, $FFFF0000, 1);
      AddPaths(sol, false, $8066FF66, $FF006600, 1);
      SaveToFile('RectClip2.svg', width, height);
    finally
      Free;
    end;
    ShellExecute(0, 'open','RectClip2.svg', nil, nil, SW_SHOW);
  end;

begin
  Randomize;
  TestEllipse(100, 100);
  TestRandomPoly(19);

end.
