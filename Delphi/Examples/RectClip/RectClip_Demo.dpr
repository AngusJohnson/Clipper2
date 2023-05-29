program RectClip_Demo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Clipper.SVG in '..\..\Utils\Clipper.SVG.pas',
  Clipper.Core in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper.Engine in '..\..\Clipper2Lib\Clipper.Engine.pas',
  Clipper.Minkowski in '..\..\Clipper2Lib\Clipper.Minkowski.pas',
  Clipper.Offset in '..\..\Clipper2Lib\Clipper.Offset.pas',
  Clipper.RectClip in '..\..\Clipper2Lib\Clipper.RectClip.pas',
  Clipper in '..\..\Clipper2Lib\Clipper.pas',
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
  const
    margin: integer = 100;
  begin
    SetLength(clp, 1);
    clp[0] := ClipMisc.Ellipse(Rect64(0, 0, radius, radius));
    SetLength(sub, count);
    for i := 0 to count -1 do
      sub[i] := TranslatePath(clp[0],
        Random(width - radius), Random(height - radius));

    rec := Rect64(margin, margin, width - margin, height - margin);
    clp[0] := rec.AsPath;
    sol := RectClip(rec, sub);

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
    ShellExecute(0, 'open','RectClip1.svg', nil, nil, SW_SHOW);
  end;

  procedure TestRandomPoly(count: integer);
  var
    sub, clp, sol: TPathsD;
    rec: TRectD;
  const
    margin: integer = 100;
  begin
    rec := RectD(margin, margin, width - margin, height - margin);
    SetLength(clp, 1);
    clp[0] := rec.AsPath;
    SetLength(sub, 1);
    sub[0] := PathD(MakeRandomPath(width, height, count));
    sol := RectClip(rec, sub);

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
  end;

  procedure TestLineClip(lineLength: integer);
  var
    sub, clp, sol: TPathsD;
    rec: TRectD;
  const
    margin: integer = 100;
  begin
    SetLength(clp, 1);
    rec := RectD(margin, margin, width - margin, height - margin);
    clp[0] := rec.AsPath;

    SetLength(sub, 1);
    sub[0] := MakeRandomPathD(width, height, lineLength);

    sol := RectClipLines(rec, sub);

    //display
    with TSvgWriter.Create(fillrule) do
    try
      AddPaths(sub, true, $0, $AA0066FF, 1);
      //AddPaths(sub, false, $100066FF, $400066FF, 1);

      AddPaths(clp, false, $10FFAA00, $FFFF0000, 1);

      AddPaths(sol, true, $0, $FF006600, 2.0);
      //AddPaths(sol, false, $8066FF66, $FF006600, 1.0);

      SaveToFile('RectClipQ.svg', width, height);
    finally
      Free;
    end;
    ShellExecute(0, 'open','RectClipQ.svg', nil, nil, SW_SHOW);
  end;

begin
  Randomize;
  TestEllipse(100, 100);
  TestRandomPoly(59);
  TestLineClip(59);

end.
