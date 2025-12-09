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
  decPoint = '.';

function ParseNum(var c: PChar; endC: PChar; skipComma: Boolean; out val: double): Boolean;
var
  decPos: integer;
  isNeg: Boolean;
  start: PChar;
begin
  Result := false;

  //skip white space +/- single comma
  if skipComma then
  begin
    while (c < endC) and (c^ <= space) do inc(c);
    if (c^ = ',') then inc(c);
  end;
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

function SkipBlanks(var c: PChar; endC: PChar): Boolean;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  Result := (c < endC);
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
  ParseNum(c, endC, false, x);
  if not ParseNum(c, endC, true, y) then Exit;
  AddPoint(x, y);
  while c < endC do
  begin
    if not SkipBlanks(c, endC) then Break;
    if c^ = 'L' then Inc(c)
    else if c^ = 'Z' then
    begin
      AddPath;
      Inc(c);
      // start next path
      if not SkipBlanks(c, endC) then Break;
      if c^ <> 'M' then break;
      Inc(c);
      if not ParseNum(c, endC, false, x) then break;
      if not ParseNum(c, endC, true,  y) then break;
      AddPoint(x, y);
      Continue;
    end;
    ParseNum(c, endC, false, x);
    if not ParseNum(c, endC, true, y) then Break;
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
begin
  inFilename := '.\clipper2.svg';
  outFilename := '.\clipper2_tri.svg';
  if not FileExists(inFilename) then Exit;
  pp := GetPathsFromSvgFile(inFilename);
  WriteLn(PathsToString(pp, 0));

  if Triangulate(pp, 0, sol, true) = trSuccess then
  begin
    SavePathsAsSvg(outFilename, pp, sol, rfOverwrite, false);
    ShellExecute(0, nil, PChar(outFilename), nil, nil, 0);
  end;
end.
//------------------------------------------------------------------------------

