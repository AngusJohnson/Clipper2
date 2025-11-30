program ClipperTri;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  Clipper in 'Clipper.pas',
  Clipper.Core in 'Clipper.Core.pas',
  Clipper.SVG in 'Clipper.SVG.pas',
  Delaunay in 'Delaunay.pas';

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

function SkipBlanksAndComma(var c: PChar; endC: PChar): Boolean;
begin
  Result := SkipBlanks(c, endC);
  if not Result or (c^ <> ',') then Exit;
  inc(c);
  Result := SkipBlanks(c, endC);
end;
//------------------------------------------------------------------------------

function PeekNextChar(var c: PChar; endC: PChar): Char;
begin
  SkipBlanks(c, endC);
  Result := c^;
end;
//------------------------------------------------------------------------------

procedure SkipNextChar(var c: PChar);
begin
  inc(c);
end;
//------------------------------------------------------------------------------

function GetPathsFromSvgFile(const svgFilename: string): TPathsD;
var
  i: integer;
  svgText: string;
  x, y: double;
  startC, endC: PChar;
  currCnt, currCap: integer;
  path: TPathD;
  c: Char;

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
  startC := @svgText[i];
  endC := startC + Length(svgText) - i +1;

  currCnt := 0; currCap := 0;
  ParseNum(startC, endC, false, x);
  if not ParseNum(startC, endC, true, y) then Exit;
  AddPoint(x, y);
  while startC < endC do
  begin
    c := PeekNextChar(startC, endC);
    if c = 'L' then SkipNextChar(startC)
    else if c = 'Z' then
    begin
      AddPath;
      SkipNextChar(startC);
      // start next path
      c := PeekNextChar(startC, endC);
      if c <> 'M' then break;
      SkipNextChar(startC);
      ParseNum(startC, endC, false, x);
      if not ParseNum(startC, endC, true, y) then break;
      AddPoint(x, y);
      Continue;
    end;
    ParseNum(startC, endC, false, x);
    if not ParseNum(startC, endC, true, y) then Break;
    AddPoint(x, y);
  end;
  AddPath;
end;

//------------------------------------------------------------------------------
// SavePathsAsSvg()
//------------------------------------------------------------------------------

type
  TReplaceFile = (rfSkip, rfOverwrite);

  THsl = packed record
    hue  : byte;
    sat  : byte;
    lum  : byte;
    alpha: byte;
  end;

  TARGB = packed record
    case boolean of
      false: (B: Byte; G: Byte; R: Byte; A: Byte);
      true : (Color: Cardinal);
  end;

function HslToRgb(hslColor: THsl): Cardinal;
var
  rgba: TARGB absolute result;
  hsl: THsl absolute hslColor;
  c, x, m, a: Integer;
begin
  c := ((255 - abs(2 * hsl.lum - 255)) * hsl.sat) shr 8;
  a := 252 - (hsl.hue mod 85) * 6;
  x := (c * (255 - abs(a))) shr 8;
  m := hsl.lum - c shr 1;
  rgba.A := hsl.alpha;
  case (hsl.hue * 6) shr 8 of
    0: begin rgba.R := c + m; rgba.G := x + m; rgba.B := 0 + m; end;
    1: begin rgba.R := x + m; rgba.G := c + m; rgba.B := 0 + m; end;
    2: begin rgba.R := 0 + m; rgba.G := c + m; rgba.B := x + m; end;
    3: begin rgba.R := 0 + m; rgba.G := x + m; rgba.B := c + m; end;
    4: begin rgba.R := x + m; rgba.G := 0 + m; rgba.B := c + m; end;
    5: begin rgba.R := c + m; rgba.G := 0 + m; rgba.B := x + m; end;
  end;
end;
//------------------------------------------------------------------------------

function RainbowColor(fraction: double; luminance, alpha: byte): Cardinal;
var
  hsl: THsl;
begin
  if (fraction < 0) or (fraction > 1) then
    fraction := frac(fraction);

  hsl.hue := Round(fraction * 255);
  hsl.sat := 255;
  hsl.lum := luminance;
  hsl.alpha := alpha;
  Result := HslToRgb(hsl);
end;
//------------------------------------------------------------------------------

procedure SavePathsAsSvg(const svgFilename: string;
  const sub, sol: TPathsD; replace: TReplaceFile;
  showCoords: Boolean = false); overload;
var
  i,j: integer;
  svg: TSvgWriter;
begin
  if (replace = rfSkip) and FileExists(svgFilename) then Exit;
  svg := TSvgWriter.Create(frNonZero, 'Verdana', 8);
  svg.AddPaths(sub, false, $20000099, $80000033, 1.0, showCoords);

  Randomize;
  for i := 0 to High(sol) do
  begin
    j := Random(256);
    svg.AddPath(sol[i], false, RainbowColor(j/256, 196, 255),
      $80000000, 1.0, false);
  end;

  svg.AddPaths(sol, false, $20009900, $80003300, 1.0, false);
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
  //WriteLn(PathsToString(pp, 0));

  sol := Triangulate(pp, 0, true);
  SavePathsAsSvg(outFilename, pp, sol, rfOverwrite, false);
  ShellExecute(0, nil, PChar(outFilename), nil, nil, 0);
end.
//------------------------------------------------------------------------------

