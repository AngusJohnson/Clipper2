program ClipperTri;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  Clipper.Core in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper in '..\..\Clipper2Lib\Clipper.pas',
  Clipper.SVG in '..\..\Utils\Clipper.SVG.pas',
  ClipMisc in '..\..\Utils\ClipMisc.pas';

//------------------------------------------------------------------------------
// Console application - main entry
//------------------------------------------------------------------------------

var
  sol : TPathsD;
  pp: TPathsD;
  inFilename, outFilename: string;
const
  svgFolder = '..\..\..\CPP\Examples\Triangulation\TriSamples\';
begin
  inFilename := svgFolder + 'coral3.svg';
  if not FileExists(inFilename) then Exit;
  outFilename := 'coral3t.svg';
  pp := GetPathsFromVerySimpleSVG(inFilename);
  if Triangulate(pp, 0, sol, true) = trSuccess then
    DisplayAsSvg_MultiColor(outFilename, frNonZero, pp, sol);
end.
//------------------------------------------------------------------------------

