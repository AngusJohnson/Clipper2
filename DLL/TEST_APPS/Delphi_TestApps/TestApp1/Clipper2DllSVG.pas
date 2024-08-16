 unit Clipper2DllSVG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  12 August 2024                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Windows, ShellApi,
  Classes, SysUtils, Math, Clipper2DllCore;

const
  black   = $FF000000;
  white   = $FFFFFFFF;
  maroon  = $FF800000;
  navy    = $FF000080;
  blue    = $FF0000FF;
  red     = $FFFF0000;
  green   = $FF008000;
  yellow  = $FFFFFF00;
  lime    = $FF00FF00;
  fuscia  = $FFFF00FF;
  aqua    = $FF00FFFF;

  displayWidth = 400;
  displayHeight = 400;

type

{$IFDEF RECORD_METHODS}
  TCoordStyle = record
{$ELSE}
  TCoordStyle = object
{$ENDIF}
    FontName: string;
    FontSize: integer;
    FontColor: cardinal;
    constructor Create(const afontname: string;
      afontsize: integer; afontcolor: Cardinal);
  end;

  TArrayOfString = array of string;
  TArrayOfInteger = array of Integer;

  PTextInfo = ^TTextInfo;
{$IFDEF RECORD_METHODS}
  TTextInfo = record
{$ELSE}
  TTextInfo = object
{$ENDIF}
    x,y : double;
    text: string;
    fontSize: integer;
    fontColor: Cardinal;
    Bold: Boolean;
    constructor Create(atext: string; _x, _y: double;
      afontsize: integer = 12;
      afontcolor: Cardinal = black;
      aBold: Boolean = false);
  end;

  PPolyInfo = ^TPolyInfo;
  TPolyInfo = record
    paths     : CPathsD;
    BrushClr  : Cardinal;
    PenClr    : Cardinal;
    PenWidth  : double;
    ShowCoords: Boolean;
    ShowFrac  : Boolean;
    IsOpen    : Boolean;
    dashes    : TArrayOfInteger;
  end;

  PCircleInfo = ^TCircleInfo;
  TCircleInfo = record
    center    : TPointD;
    radius    : double;
    BrushClr  : Cardinal;
    PenClr    : Cardinal;
    PenWidth  : double;
  end;

  TSvgWriter = class
  private
    fFillRule   : TFillRule;
    fCoordStyle : TCoordStyle;
    fPolyInfos  : TList;
    fCircleInfos: TList;
    fTextInfos  : TList;
    function GetBounds: TRectD;
  public
    constructor Create(fillRule: TFillRule;
      const coordFontName: string = 'Verdana';
      coordFontSize: integer = 9;
      coordFontColor: Cardinal = black);
    destructor Destroy; override;

    procedure AddPaths(const paths: CPaths64; isOpen: Boolean;
      brushColor, penColor: Cardinal;
      penWidth: double; showCoords: Boolean = false); overload;

    procedure AddPaths(const paths: CPathsD; isOpen: Boolean;
      brushColor, penColor: Cardinal;
      penWidth: double; showCoords: Boolean = false); overload;

    procedure AddCircle(const center: TPoint64; radius: double;
      brushColor, penColor: Cardinal; penWidth: double); overload;
    procedure AddCircle(const center: TPointD; radius: double;
      brushColor, penColor: Cardinal; penWidth: double); overload;
    procedure AddText(text: string; x,y: double;
      fontSize: integer = 14; fontClr: Cardinal = black; bold: Boolean = false);

    function SaveToFile(const filename: string;
      maxWidth: integer = 0; maxHeight: integer = 0;
      margin: integer = 20): Boolean;
    procedure ClearPaths;
    procedure ClearText;
    procedure ClearAll;
  end;

  procedure AddSubjects(svg: TSvgWriter; const paths: CPaths64; showCoords: Boolean = false); overload;
  procedure AddOpenSubjects(svg: TSvgWriter; const paths: CPaths64); overload;
  procedure AddClips(svg: TSvgWriter; const paths: CPaths64; showCoords: Boolean = false); overload;
  procedure AddSolution(svg: TSvgWriter; const paths: CPaths64; showCoords: Boolean = false); overload;
  procedure AddOpenSolution(svg: TSvgWriter; const paths: CPaths64); overload;

  procedure SaveSvg(svg: TSvgWriter; const filename: string;
    width: integer = 0; height: integer = 0; margin: integer = 0); overload;

  procedure AddSubjects(svg: TSvgWriter; const paths: CPathsD; showCoords: Boolean = false); overload;
  procedure AddOpenSubjects(svg: TSvgWriter; const paths: CPathsD); overload;
  procedure AddClips(svg: TSvgWriter; const paths: CPathsD; showCoords: Boolean = false); overload;
  procedure AddSolution(svg: TSvgWriter; const paths: CPathsD; showCoords: Boolean= false); overload;
  procedure AddOpenSolution(svg: TSvgWriter; const paths: CPathsD); overload;

  procedure DisplaySVG(const sub, subo, clp, sol, solo: CPaths64;
    const svgName: string; width: integer = displayWidth;
    height: integer = displayHeight; showCoords: Boolean = false); overload;
  procedure DisplaySVG(const sub, subo, clp, sol, solo: CPathsD;
    const svgName: string; width: integer = displayWidth;
    height: integer = displayHeight; showCoords: Boolean = false); overload;


implementation

const
  MaxRect: TRectD  = (left: MaxDouble;
    Top: MaxDouble; Right: -MaxDouble; Bottom: -MaxDouble);

  svg_header: string =
      '<svg width="%dpx" height="%dpx" viewBox="0 0 %0:d %1:d" ' +
      'version="1.1" xmlns="http://www.w3.org/2000/svg">';
  svg_path_format: string = '"'+#10+'    style="fill:%s;' +
        ' fill-opacity:%1.2f; fill-rule:%s; stroke:%s;' +
        ' stroke-opacity:%1.2f; stroke-width:%1.2f;"/>'#10;
  svg_path_format2: string = '"'+#10+
        '   style="fill:none; stroke:%s; ' +
        'stroke-opacity:%1.2f; stroke-width:%1.2f; %s"/>'#10;

function ColorToHtml(color: Cardinal): string;
begin
  Result := Format('#%6.6x', [color and $FFFFFF]);;
end;
//------------------------------------------------------------------------------

function GetAlpha(clr: Cardinal): double;
begin
  Result := (clr shr 24) / 255;
end;
//------------------------------------------------------------------------------

function CopyCPaths64(cpaths: CPaths64): CPaths64;
var
  arrayLen, byteSpace: int64;
begin
  arrayLen := cpaths[0];
  byteSpace := arrayLen * SizeOf(Int64);
  GetMem(Result, byteSpace);
  Move(cpaths[0], Result[0], byteSpace);
end;
//------------------------------------------------------------------------------

function CopyCPathsD(cpaths: CPathsD): CPathsD;
var
  arrayLen, byteSpace: int64;
begin
  arrayLen := Round(cpaths[0]);
  byteSpace := arrayLen * SizeOf(double);
  GetMem(Result, byteSpace);
  Move(cpaths[0], Result[0], byteSpace);
end;
//------------------------------------------------------------------------------

function CopyConvertCPath64(cpaths: CPaths64): CPathsD;
var
  i: integer;
  arrayLen, byteSpace: int64;
begin
  arrayLen := cpaths[0];
  byteSpace := arrayLen * SizeOf(double);
  GetMem(Result, byteSpace);
  for i := 0 to arrayLen -1 do Result[i] := cpaths[i];
end;
//------------------------------------------------------------------------------

constructor TCoordStyle.Create(const afontname: string;
  afontsize: integer; afontcolor: Cardinal);
begin
  Self.FontName   := afontname;
  Self.FontSize   := afontsize;
  Self.FontColor  := afontcolor;
end;
//------------------------------------------------------------------------------

constructor TTextInfo.Create(atext: string; _x, _y: double;
  afontsize: integer = 12; afontcolor: Cardinal = black;
  aBold: Boolean = false);
begin
  self.x := _x;
  self.y := _y;
  self.text := text;
  self.fontSize := afontsize;
  self.fontColor := afontcolor;
  Self.Bold       := aBold;
end;
//------------------------------------------------------------------------------

constructor TSvgWriter.Create(fillRule: TFillRule;
  const coordFontName: string = 'Verdana';
  coordFontSize: integer = 9;
  coordFontColor: Cardinal = black);
begin
  fFillRule := fillRule;
  fCoordStyle.FontName := coordFontName;
  fCoordStyle.FontSize := coordFontSize;
  fCoordStyle.FontColor := coordFontColor;
  fPolyInfos := TList.Create;
  fCircleInfos := TList.Create;
  fTextInfos := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TSvgWriter.Destroy;
begin
  ClearAll;
  fPolyInfos.Free;
  fCircleInfos.Free;
  fTextInfos.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.AddPaths(const paths: CPaths64;
  isOpen: Boolean; brushColor, penColor: Cardinal;
  penWidth: double; showCoords: Boolean = false);
var
  pi: PPolyInfo;
begin
  if paths = nil then Exit;
  new(pi);
  pi.paths      := CopyConvertCPath64(paths);
  pi.BrushClr   := brushColor;
  pi.PenClr     := penColor;
  pi.PenWidth   := penWidth;
  pi.ShowCoords := showCoords;
  pi.ShowFrac   := false;
  pi.IsOpen     := isOpen;
  fPolyInfos.Add(pi);
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.AddPaths(const paths: CPathsD; isOpen: Boolean;
  brushColor, penColor: Cardinal;
  penWidth: double; showCoords: Boolean = false);
var
  pi: PPolyInfo;
begin
  if paths = nil then Exit;
  new(pi);
  pi.paths      := CopyCPathsD(paths);
  pi.BrushClr   := brushColor;
  pi.PenClr     := penColor;
  pi.PenWidth   := penWidth;
  pi.ShowCoords := showCoords;
  pi.ShowFrac   := true;
  pi.IsOpen := isOpen;
  fPolyInfos.Add(pi);
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.AddCircle(const center: TPoint64;
  radius: double; brushColor, penColor: Cardinal; penWidth: double);
var
  ci: PCircleInfo;
begin
  new(ci);
  ci.center := PointD(center.X, center.Y);
  ci.radius := radius;
  ci.BrushClr := brushColor;
  ci.PenClr   := penColor;
  ci.PenWidth := penWidth;
  fCircleInfos.Add(ci);
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.AddCircle(const center: TPointD;
  radius: double; brushColor, penColor: Cardinal; penWidth: double);
var
  ci: PCircleInfo;
begin
  new(ci);
  ci.center := center;
  ci.radius := radius;
  ci.BrushClr := brushColor;
  ci.PenClr   := penColor;
  ci.PenWidth := penWidth;
  fCircleInfos.Add(ci);
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.AddText(text: string; x,y: double;
  fontSize: integer; fontClr: Cardinal; bold: Boolean);
var
  ti: PTextInfo;
begin
  new(ti);
  ti.x := x;
  ti.y := y;
  ti.text := text;
  ti.fontSize := fontSize;
  ti.fontColor := fontClr;
  ti.Bold := bold;
  fTextInfos.Add(ti);
end;
//------------------------------------------------------------------------------

function TSvgWriter.GetBounds: TRectD;
var
  i: integer;
  bounds: TRectD;
begin
  Result := MaxRect;
  for i := 0 to fPolyInfos.Count -1 do
    with PPolyInfo(fPolyInfos[i])^ do
    begin
      bounds := Clipper2DllCore.GetBounds(paths);
      if not IsValidRectD(bounds) then Continue;
      if (bounds.left < Result.Left) then Result.Left := bounds.Left;
      if (bounds.right> Result.Right) then Result.Right := bounds.Right;
      if (bounds.top < Result.Top) then Result.Top := bounds.Top;
      if (bounds.bottom > Result.Bottom) then Result.Bottom := bounds.Bottom;
    end;
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.ClearPaths;
var
  i: integer;
begin
  for i := 0 to fPolyInfos.Count -1 do
  begin
    Dispose(PPolyInfo(fPolyInfos[i]).paths);
    Dispose(PPolyInfo(fPolyInfos[i]));
  end;
  fPolyInfos.Clear;

  for i := 0 to fCircleInfos.Count -1 do
    Dispose(PCircleInfo(fCircleInfos[i]));
  fCircleInfos.Clear;
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.ClearText;
var
  i: integer;
begin
  for i := 0 to fTextInfos.Count -1 do
    Dispose(PTextInfo(fTextInfos[i]));
  fTextInfos.Clear;
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.ClearAll;
begin
  ClearText;
  ClearPaths;
end;
//------------------------------------------------------------------------------

function TSvgWriter.SaveToFile(const filename: string;
  maxWidth: integer = 0; maxHeight: integer = 0; margin: integer = 20): Boolean;
var
  i, j, k, idx      : integer;
  frac              : integer;
  showCo            : boolean;
  showFr            : boolean;
  x,y               : double;
{$IFDEF USINGZ}
  z                 : double;
{$ENDIF}
  bounds            : TRectD;
  scale             : double;
  offsetX, offsetY  : integer;
  s                 : string;
  sInline, dashStr  : string;
  sl                : TStringList;
  pathsCnt, pathLen : integer;
  formatSettings: TFormatSettings;
const
  fillRuleStr: array[boolean] of string = ('evenodd', 'nonzero');
  boldFont: array[boolean] of string = ('normal', '700');
  gap: integer = 5;

  procedure Add(const s: string);
  begin
    sl.Add( sInline + s);
    sInline := '';
  end;

  procedure AddInline(const s: string);
  begin
    sInline := sInline + s;
  end;

begin
  Result := false;

{$IF NOT Defined(fpc) AND (CompilerVersion > 19)}   //Delphi XE +
  formatSettings := TFormatSettings.Create;
{$IFEND}
  formatSettings.DecimalSeparator := '.';

  // adjust margin
  if (margin < 20) then margin := 20;
  showCo := false;
  showFr := false;
  for i := 0 to fPolyInfos.Count -1 do
    with PPolyInfo(fPolyInfos[i])^ do
    if ShowCoords then
    begin
      showCo := true;
      if showFrac then showFr := true;

    end;
  if showCo then
  begin
    if showFr then
      inc(margin, Abs(fCoordStyle.FontSize *4)) else
      inc(margin, Abs(fCoordStyle.FontSize *2));
  end;

  // get scale and offset
  bounds := GetBounds;
  if (bounds.Left >= bounds.Right) or
    (bounds.Top >= bounds.Bottom) then Exit;

  scale := 1.0;
  if (maxWidth > 0) and (maxHeight > 0) then
    scale := 1.0 / Max((bounds.right - bounds.left) /
      (maxWidth - margin * 2), (bounds.bottom - bounds.top) /
      (maxHeight - margin * 2));

  offsetX := margin - Round(bounds.left * scale);
  offsetY := margin - Round(bounds.top * scale);

  // write SVG
  sl := TStringList.Create;
  try
    if (maxWidth <= 0) or (maxHeight <= 0) then
       Add(Format(svg_header,
       [Round(bounds.right - bounds.left) + margin * 2,
        Round(bounds.bottom - bounds.top) + margin * 2], formatSettings))
    else
      Add(Format(svg_header, [maxWidth, maxHeight], formatSettings));

    for i := 0 to fPolyInfos.Count -1 do
      with PPolyInfo(fPolyInfos[i])^ do
      begin
        AddInline('  <path d="');
        pathsCnt := Round(paths[1]);
        idx := 2;
        for j := 0 to pathsCnt -1 do
        begin
          pathLen := Round(paths[idx]); inc(idx, 2);
          if (pathLen < 2) or (not IsOpen and (pathLen < 3)) then
          begin
            inc(idx, pathLen * VERTEX_FIELD_CNT);
            Continue;
          end;
          x := paths[idx]; inc(idx);
          y := paths[idx]; inc(idx);
{$IFDEF USINGZ}
            inc(idx);
{$ENDIF}
          AddInline(Format('M %1.2f %1.2f L ',
            [x * scale + offsetX, y * scale + offsetY], formatSettings));
          for k := 1 to pathLen -1 do
          begin
            x := paths[idx]; inc(idx);
            y := paths[idx]; inc(idx);
{$IFDEF USINGZ}
            inc(idx);
{$ENDIF}
            AddInline(Format('%1.2f %1.2f ',
              [x * scale + offsetX, y * scale + offsetY], formatSettings));

          end;
          if not IsOpen then AddInline('Z');
        end;

        if not IsOpen then
          Add(Format(svg_path_format,
            [ColorToHtml(BrushClr), GetAlpha(BrushClr),
            fillRuleStr[fFillRule = frNonZero],
            ColorToHtml(PenClr), GetAlpha(PenClr), PenWidth], formatSettings))
        else
        begin
          dashStr := '';;
          if Length(dashes) > 0 then
          begin
            s := '';
            for k := 0 to High(dashes) do
              s := s + IntToStr(dashes[k]) + ' ';
            dashStr := 'stroke-dasharray: ' + s + ';';
          end;
          Add(Format(svg_path_format2,
              [ColorToHtml(PenClr), GetAlpha(PenClr),
              PenWidth, dashStr], formatSettings));
        end;

        if (ShowCoords) then
        begin
          if ShowFrac then frac := 2 else frac := 0;
          with fCoordStyle do
            Add(Format('<g font-family="%s" font-size="%d" fill="%s">',
              [FontName, FontSize, ColorToHtml(FontColor)], formatSettings));

          idx := 2;
          for j := 0 to pathsCnt -1 do
          begin
            pathLen := Round(paths[idx]); inc(idx, 2);
            for k := 0 to pathLen -1 do
            begin
              x := paths[idx]; inc(idx);
              y := paths[idx]; inc(idx);
  {$IFDEF USINGZ}
              z := paths[idx]; inc(idx);
              Add(Format('  <text x="%1.2f" y="%1.2f">%1.*f,%1.*f,%1.*f</text>',
              [x * scale + offsetX, y * scale + offsetY, frac, x, frac, y, frac, z],
              formatSettings));
  {$ELSE}
              Add(Format('  <text x="%1.2f" y="%1.2f">%1.*f,%1.*f</text>',
              [x * scale + offsetX, y * scale + offsetY, frac, x, frac, y],
              formatSettings));
  {$ENDIF}
            end;
            Add('</g>'#10);
          end;
        end;

      end;


      for i := 0 to fCircleInfos.Count -1 do
        with PCircleInfo(fCircleInfos[i])^ do
        begin
          if GetAlpha(BrushClr) > 0.1 then
            Add(Format('  <circle cx="%1.2f" cy="%1.2f" r="%1.2f" '+
              'stroke="%s" stroke-width="%1.2f" '+
              'fill="%s" opacity="%1.2f" />',
              [center.X * scale + offsetX, center.Y * scale + offsetY,
              radius, ColorToHtml(PenClr), PenWidth,
              ColorToHtml(BrushClr), GetAlpha(BrushClr)], formatSettings))
          else
            Add(Format('  <circle cx="%1.2f" cy="%1.2f" r="%1.2f" '+
              'stroke="%s" stroke-width="%1.2f" '+
              'fill="none" opacity="%1.2f" />',
              [center.X * scale + offsetX, center.Y * scale + offsetY,
              radius, ColorToHtml(PenClr), PenWidth,
              GetAlpha(PenClr)], formatSettings));
        end;

    for i := 0 to fTextInfos.Count -1 do
      with PTextInfo(fTextInfos[i])^ do
      begin
        Add(Format(
        '  <g font-family="Verdana" font-style="normal" ' +
        'font-weight="%s" font-size="%d" fill="%s">' +
        '<text x="%1.2f" y="%1.2f">%s</text></g>',
        [boldFont[Bold], Round(fontSize * scale),
        ColorToHtml(fontColor),
        (x) * scale + offsetX,
        (y) * scale + offsetY, text], formatSettings));
      end;

    Add('</svg>'#10);
    sl.SaveToFile(filename);
    Result := true;
  finally
    sl.free;
  end;
end;
//------------------------------------------------------------------------------

procedure AddSubjects(svg: TSvgWriter; const paths: CPaths64; showCoords: Boolean = false);
begin
  svg.AddPaths(paths, false, $200099FF, $800066FF, 1.0, showCoords);
end;
//------------------------------------------------------------------------------

procedure AddOpenSubjects(svg: TSvgWriter; const paths: CPaths64);
begin
  svg.AddPaths(paths, true, $0, $800066FF, 2.2);
end;
//------------------------------------------------------------------------------

procedure AddClips(svg: TSvgWriter; const paths: CPaths64; showCoords: Boolean = false);
begin
  svg.AddPaths(paths, false, $10FF9900, $80FF6600, 1.0, showCoords);
end;
//------------------------------------------------------------------------------

procedure AddSolution(svg: TSvgWriter; const paths: CPaths64; showCoords: Boolean);
begin
  svg.AddPaths(paths, false, $8066FF66, $FF006600, 1.0, showCoords);
end;
//------------------------------------------------------------------------------

procedure AddOpenSolution(svg: TSvgWriter; const paths: CPaths64);
begin
  svg.AddPaths(paths, true, $0, $FF006600, 1.5);
end;
//------------------------------------------------------------------------------

procedure SaveSvg(svg: TSvgWriter; const filename: string;
  width: integer = 0; height: integer = 0; margin: integer = 0);
begin
  svg.SaveToFile(filename, width, height, margin);
end;
//------------------------------------------------------------------------------

procedure AddSubjects(svg: TSvgWriter; const paths: CPathsD; showCoords: Boolean = false);
begin
  svg.AddPaths(paths, false, $200099FF, $800066FF, 1.0, showCoords);
end;
//------------------------------------------------------------------------------

procedure AddOpenSubjects(svg: TSvgWriter; const paths: CPathsD);
begin
  svg.AddPaths(paths, true, $0, $400066FF, 2.2);
end;
//------------------------------------------------------------------------------

procedure AddClips(svg: TSvgWriter; const paths: CPathsD; showCoords: Boolean = false);
begin
  svg.AddPaths(paths, false, $10FF9900, $80FF6600, 1.0, showCoords);
end;
//------------------------------------------------------------------------------

procedure AddSolution(svg: TSvgWriter; const paths: CPathsD; showCoords: Boolean);
begin
  svg.AddPaths(paths, false, $8066FF66, $FF006600, 1.0, showCoords);
end;
//------------------------------------------------------------------------------

procedure AddOpenSolution(svg: TSvgWriter; const paths: CPathsD);
begin
  svg.AddPaths(paths, true, $0, $FF006600, 1.5);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure ShowSvg(const svgFilename: string);
begin
  ShellExecute(0, 'open',PChar(svgFilename), nil, nil, SW_SHOW);
end;
//------------------------------------------------------------------------------

procedure DisplaySVG(const sub, subo, clp, sol, solo: CPathsD;
  const svgName: string; width: integer = displayWidth;
  height: integer = displayHeight; showCoords: Boolean = false);
var
  i,j, idx, pathsCnt, pathLen: integer;
  x,y,z: double;
  svg: TSvgWriter;
begin
  svg := TSvgWriter.Create(TFillRule.frNonZero);
  try
    AddSubjects(svg, sub);
    AddOpenSubjects(svg, subo);
    AddClips(svg, clp);
    AddSolution(svg, sol, showCoords);
{$IFDEF USINGZ}
    if Assigned(sol) then
    begin
      pathsCnt := Round(sol[1]);
      idx := 2;
      for i := 0 to pathsCnt -1 do
      begin
        pathLen := Round(sol[idx]); inc(idx, 2);
        for j := 0 to pathLen -1 do
        begin
          x := sol[idx]; inc(idx);
          y := sol[idx]; inc(idx);
          z := sol[idx]; inc(idx);
          if z <= 0 then Continue;
          svg.AddCircle(PointD(x,y), 3, $FFFFFF00, $FF000000, 1);
          svg.AddText(format('%1.0n',[z]), X +1, Y, 3);
        end;
      end;
    end;
{$ENDIF}
    AddOpenSolution(svg, solo);
    SaveSvg(svg, svgName, width, height);
    ShowSvg(svgName);
  finally
    svg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DisplaySVG(const sub, subo, clp, sol, solo: CPaths64;
  const svgName: string; width: integer = displayWidth;
  height: integer = displayHeight; showCoords: Boolean = false);
var
  i,j, idx, pathsCnt, pathLen: integer;
  x,y,z: Int64;
  svg: TSvgWriter;
begin
  svg := TSvgWriter.Create(frNonZero);
  try
    AddSubjects(svg, sub);
    AddOpenSubjects(svg, subo);
    AddClips(svg, clp);
    AddSolution(svg, sol, showCoords);
{$IFDEF USINGZ}
    if Assigned(sol) then
    begin
      pathsCnt := sol[1];
      idx := 2;
      for i := 0 to pathsCnt -1 do
      begin
        pathLen := sol[idx]; inc(idx, 2);
        for j := 0 to pathLen -1 do
        begin
          x := sol[idx]; inc(idx);
          y := sol[idx]; inc(idx);
          z := sol[idx]; inc(idx);
          if z <= 0 then Continue;
          svg.AddCircle(PointD(x,y), 3, $FFFFFF00, $FF000000, 1);
          svg.AddText(format('%d',[z]), X +1, Y, 3);
        end;
      end;
    end;
{$ENDIF}
    AddOpenSolution(svg, solo);
    SaveSvg(svg, svgName, width, height);
    ShowSvg(svgName);
  finally
    svg.Free;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.

