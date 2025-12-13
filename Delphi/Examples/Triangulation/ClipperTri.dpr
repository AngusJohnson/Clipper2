program ClipperTri;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  Clipper.Core in '..\..\Clipper2Lib\Clipper.Core.pas',
  Clipper in '..\..\Clipper2Lib\Clipper.pas',
  Clipper.Triangulation in '..\..\Clipper2Lib\Clipper.Triangulation.pas',
  Clipper.SVG in '..\..\Utils\Clipper.SVG.pas';


//------------------------------------------------------------------------------
// GetPathsFromSvgFile() - gets paths from a *simple* SVG file
//------------------------------------------------------------------------------

const
  space = #32;
  comma = ',';
  decPoint = '.';

function SkipOptionalComma(var c: PChar; endC: PChar): Boolean;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  if (c^ = comma) then inc(c);
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function SkipBlanks(var c: PChar; endC: PChar): Boolean;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function ParseNum(var c: PChar; endC: PChar; out val: double): Boolean;
var
  decPos: integer;
  isNeg: Boolean;
  start: PChar;
begin
  Result := false;

  //skip white space
  while (c < endC) and (c^ <= space) do inc(c);
  if (c = endC) then Exit;

  decPos := -1;
  isNeg := c^ = '-';
  if isNeg then inc(c);

  val := 0;
  start := c;
  while c < endC do
  begin
    if Ord(c^) = Ord(decPoint) then
    begin
      if decPos >= 0 then break;
      decPos := 0;
    end
    else if (c^ < '0') or (c^ > '9') then
      break
    else
    begin
      val := val *10 + Ord(c^) - Ord('0');
      if decPos >= 0 then inc(decPos);
    end;
    inc(c);
  end;
  Result := c > start;
  if not Result then Exit;

  if decPos > 0 then val := val * Power(10, -decPos);
  if isNeg then val := -val;
end;
//------------------------------------------------------------------------------

function GetPathsFromSvgFile(const svgFilename: string): TPathsD;
var
  i: integer;
  svgText: string;
  x, y: double;
  c, endC: PChar;
  currCnt, currCap: integer;
  path: TPathD;

  procedure AddPoint(x,y: double);
  begin
    if currCnt = currCap then
    begin
      currCap := currCap + 256;
      SetLength(path, currCap);
    end;
    path[currCnt] := PointD(x,y);
    inc(currCnt);
  end;

  procedure AddPath;
  var
    cnt: integer;
  begin
    if currCnt = 0 then Exit;
    cnt := Length(Result);
    SetLength(Result, cnt +1);
    SetLength(path, currCnt);
    Result[cnt] := path;
    // reset path
    path := nil;
    currCnt := 0;
    currCap := 0;
  end;

begin
  Result := nil;
  with TStringList.Create do
  try
    LoadFromFile(svgFilename);
    svgText := Text;
  finally
    free;
  end;
  i := Pos('path d="M', svgText);
  if i = 0 then Exit;
  inc(i, 9);
  c := @svgText[i];
  endC := c + Length(svgText) - i +1;

  currCnt := 0; currCap := 0;
  ParseNum(c, endC, x);
  SkipOptionalComma(c, endC);
  if not ParseNum(c, endC, y) then Exit;
  SkipOptionalComma(c, endC);
  AddPoint(x, y);
  while c < endC do
  begin
    if not SkipBlanks(c, endC) then Break;
    if c^ = 'L' then Inc(c)
    else if (c^ = 'Z') or (c^ = 'M') then
    begin
      AddPath;
      if (c^ = 'Z') then
      begin
        Inc(c);
        // start next path
        if not SkipBlanks(c, endC) then Break;
      end;
      if c^ = 'M' then
      begin
        Inc(c);
        if not ParseNum(c, endC, x) then break;
        SkipOptionalComma(c, endC);
        if not ParseNum(c, endC, y) then break;
      end;
      AddPoint(x, y);
      Continue;
    end;
    ParseNum(c, endC, x);
    SkipOptionalComma(c, endC);
    if not ParseNum(c, endC, y) then Break;
    SkipOptionalComma(c, endC);
    AddPoint(x, y);
  end;
  AddPath;
end;

//------------------------------------------------------------------------------
// SavePathsAsSvg()
//------------------------------------------------------------------------------

function RandomColor: Cardinal; inline;
begin
  Result := Cardinal(Random($1000000)) or $FF000000;
end;
//------------------------------------------------------------------------------

type
  TReplaceFile = (rfSkip, rfOverwrite);

procedure SavePathsAsSvg(const svgFilename: string;
  const sub, sol: TPathsD; replace: TReplaceFile;
  showCoords: Boolean = false); overload;
var
  i: integer;
  svg: TSvgWriter;
begin
  if (replace = rfSkip) and FileExists(svgFilename) then Exit;
  svg := TSvgWriter.Create(frNonZero, 'Verdana', 8);
  svg.AddPaths(sub, false, $20000000, $FF000000, 1.5, showCoords);

  Randomize;
  for i := 0 to High(sol) do
    svg.AddPath(sol[i], false, RandomColor, $20000000, 1.0, false);

  svg.SaveToFile(svgFilename, 1100, 700, 50);
  svg.Free;
end;

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
  pp := GetPathsFromSvgFile(inFilename);
  if Triangulate(pp, 0, sol, true) = trSuccess then
  begin
    SavePathsAsSvg(outFilename, pp, sol, rfOverwrite, false);
    ShellExecute(0, nil, PChar(outFilename), nil, nil, 0);
  end;
end.
//------------------------------------------------------------------------------

