program RectClip_Demo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Clipper.Core in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper in '..\..\Clipper2Lib\Clipper.pas',
  ClipMisc in '..\..\Utils\ClipMisc.pas',
  Clipper.SVG in '..\..\Utils\Clipper.SVG.pas';

const
  margin = 100;
  width = 600;
  height = 600;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TestEllipse(radius, count: integer);
var
  i: integer;
  sub, clp, sol: TPaths64;
  rec: TRect64;
begin
  SetLength(clp, 1);
  clp[0] := Ellipse(Rect64(0, 0, radius, radius));
  SetLength(sub, count);
  for i := 0 to count -1 do
    sub[i] := TranslatePath(clp[0],
      Random(width - radius), Random(height - radius));

  rec := Rect64(margin, margin, width - margin, height - margin);
  clp[0] := rec.AsPath;
  sol := RectClip(rec, sub);
  DisplayAsSvg('RectClip Ellipses', frEvenOdd, sub, clp, sol);
end;
//------------------------------------------------------------------------------

procedure TestRandomPoly(count: integer);
var
  sub, clp, sol: TPathsD;
  rec: TRectD;
begin
  rec := RectD(margin, margin, width - margin, height - margin);
  SetLength(clp, 1);
  clp[0] := rec.AsPath;
  SetLength(sub, 1);
  sub[0] := PathD(MakeRandomPath(width, height, count));
  sol := RectClip(rec, sub);
  DisplayAsSvg('RectClip Random Polygons', frEvenOdd, sub, clp, sol);
end;
//------------------------------------------------------------------------------

procedure TestLineClip(lineLength: integer);
var
  sub, clp, sol: TPathsD;
  rec: TRectD;
begin
  SetLength(clp, 1);
  rec := RectD(margin, margin, width - margin, height - margin);
  clp[0] := rec.AsPath;
  SetLength(sub, 1);
  sub[0] := MakeRandomPathD(width, height, lineLength);
  sol := RectClipLines(rec, sub);
  DisplayAsSvg_Open('RectClip Open Paths', frEvenOdd, false, sub, clp, sol, true);
end;

//------------------------------------------------------------------------------
// main entry
//------------------------------------------------------------------------------

begin
  Randomize;
  TestEllipse(100, 100);
  TestRandomPoly(59);
  TestLineClip(59);

end.
