unit Clipper.DLL.SVG;

(* ******************************************************************************
  * Author    :  Jasper Schellingerhout                                         *
  * Date      :  12 June 2024                                                   *
  * License   :  http://www.boost.org/LICENSE_1_0.txt                           *
  * Heavily based on code written by Angus Johnson for the Clipper2 library     *
  *  released under the same license                                            *
  **************************************************************************** *)

{$I Clipper.DLL.inc}

interface
uses
  Classes, Clipper.Dll.Enums, Clipper.DLL.Data;

const
  black = $FF000000;
  white = $FFFFFFFF;
  maroon = $FF800000;
  navy = $FF000080;
  blue = $FF0000FF;
  red = $FFFF0000;
  green = $FF008000;
  yellow = $FFFFFF00;
  lime = $FF00FF00;
  fuscia = $FFFF00FF;
  aqua = $FF00FFFF;

type
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  TPointD = Clipper.Dll.Data.TVertexD;
{$ENDIF}

  TCoordStyle = record
    FontName: string;
    FontSize: integer;
    FontColor: cardinal;
    constructor Create(const afontname: string; afontsize: integer; afontcolor: cardinal);
  end;

  PTextInfo = ^TTextInfo;
  TTextInfo = record
    x, y: integer;
    text: string;
    FontSize: integer;
    FontColor: cardinal;
    Bold: Boolean;
    constructor Create(atext: string; _x, _y: integer; afontsize: integer = 12; afontcolor: cardinal = black;
      aBold: Boolean = false);
  end;

  PPolyInfo = ^TPolyInfo;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
   TSVGBounds = TRectD;
{$ELSE}
  TSVGBounds = Record
     left, right, top, bottom: double;
     function IsEmpty: boolean;
     constructor Create(left, right, top, bottom: double);

  end;
{$ENDIF}

{$SCOPEDENUMS ON}
  TPolyInfo = record
  type
     TPathType = ( Paths64, PolyTree64 {$IFDEF CLIPPER_DOUBLE_COORDINATES},PathsD, PolyTreeD {$ENDIF});

  private
    function GetPaths64: CPaths64;
    function GetPolyTree64: CPolyTree64;
    procedure SetPaths64(const Value: CPaths64);
    procedure SetPolyTree64(const Value: CPolyTree64);
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
    function GetPathsD: CPathsD;
    function GetPolyTreeD: CPolyTreeD;
    procedure SetPathsD(const Value: CPathsD);
    procedure SetPolyTreeD(const Value: CPolyTreeD);

{$ENDIF}

  public
    BrushClr: cardinal;
    PenClr: cardinal;
    PenWidth: double;
    ShowCoords: Boolean;
    ShowFrac: Boolean;
    IsOpen: Boolean;
    dashes: TArray<integer>;

    function GetBounds: TSVGBounds;
    procedure ExpandBounds(var Bounds: TSVGBounds);

    function GetFirstPath: Pointer; // use UseDouble to cast
    function GetNextPath(PrePath: Pointer): Pointer;

    function GetPathLength(Path: pointer): integer;
    function GetPathX(Path: pointer; index: integer): double;
    function GetPathY(Path: pointer; index: integer): double;

    property paths64: CPaths64 read GetPaths64 write SetPaths64;
    property polyTree64: CPolyTree64 read GetPolyTree64 write SetPolyTree64;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
    property pathsD: CPathsD read GetPathsD write SetPathsD;
    property polyTreeD: CPolyTreeD read GetPolyTreeD write SetPolyTreeD;
{$ENDIF}

    case PathType: TPathType of

      TPathType.Paths64:
        (Fpaths64: CPaths64);

      TPathType.PolyTree64:
        (FpolyTree64: CPolyTree64);
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
        TPathType.PathsD:
        (FpathsD: CPathsD);

      TPathType.PolyTreeD:
        (FpolyTreeD: CPolyTreeD);
{$ENDIF}

  end;


  TSvgWriter = class
  private
    fFillRule: TFillRule;
    fCoordStyle: TCoordStyle;
    fPolyInfos: TList;
    fCircleInfos: TList;
    fTextInfos: TList;

    function GetBounds: TSVGBounds;
  public
    constructor Create(fillRule: TFillRule; const coordFontName: string = 'Verdana'; coordFontSize: integer = 9;
      coordFontColor: cardinal = black);
    destructor Destroy; override;

    procedure AddPaths(const paths: CPaths64; IsOpen: Boolean; brushColor, penColor: cardinal; PenWidth: double;
      ShowCoords: Boolean = false); overload;

    procedure AddPolyTree(const tree: CPolyTree64; brushColor, penColor: cardinal; PenWidth: double;
      ShowCoords: Boolean = false); overload;

    procedure AddDashedPath(const paths: CPaths64; penColor: cardinal; PenWidth: double;
      const dashes: TArray<Integer>); overload;

  {$IFDEF CLIPPER_DOUBLE_COORDINATES}
    procedure AddPaths(const paths: CPathsD; IsOpen: Boolean; brushColor, penColor: cardinal; PenWidth: double;
      ShowCoords: Boolean = false); overload;

    procedure AddPolyTree(const tree: CPolyTreeD; brushColor, penColor: cardinal; PenWidth: double;
      ShowCoords: Boolean = false); overload;

    procedure AddDashedPath(const paths: CPathsD; penColor: cardinal; PenWidth: double;
      const dashes: TArray<Integer>); overload;
  {$ENDIF}


    procedure AddText(text: string; x, y: integer; FontSize: integer = 14; fontClr: cardinal = black;
      Bold: Boolean = false);

    function SaveToFile(const filename: string; maxWidth: integer = 0; maxHeight: integer = 0;
      margin: integer = 20): Boolean;

    procedure ClearPaths;
    procedure ClearText;
    procedure ClearAll;
  end;

procedure AddSubject(SVG: TSvgWriter; const paths: CPaths64; ShowCoords: Boolean = false); overload;
procedure AddOpenSubject(SVG: TSvgWriter; const paths: CPaths64); overload;
procedure AddClip(SVG: TSvgWriter; const paths: CPaths64; ShowCoords: Boolean = false); overload;
procedure AddSolution(SVG: TSvgWriter; const paths: CPaths64; ShowCoords: Boolean = false); overload;
procedure AddSolution(SVG: TSvgWriter; const tree: CPolyTree64; ShowCoords: Boolean = false); overload;
procedure AddOpenSolution(SVG: TSvgWriter; const paths: CPaths64); overload;

procedure SaveSvg(SVG: TSvgWriter; const filename: string; width: integer = 0; height: integer = 0;
  margin: integer = 0); overload;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure AddSubject(SVG: TSvgWriter; const paths: CPathsD; ShowCoords: Boolean = false); overload;
procedure AddOpenSubject(SVG: TSvgWriter; const paths: CPathsD); overload;
procedure AddClip(SVG: TSvgWriter; const paths: CPathsD; ShowCoords: Boolean = false); overload;
procedure AddSolution(SVG: TSvgWriter; const paths: CPathsD; ShowCoords: Boolean = false); overload;
procedure AddSolution(SVG: TSvgWriter; const tree: CPolyTreeD; ShowCoords: Boolean = false); overload;
procedure AddOpenSolution(SVG: TSvgWriter; const paths: CPathsD); overload;
{$ENDIF}
implementation
uses
  SysUtils, System.Math;

const
//  MaxRect: TRectD = (left: MaxDouble; Top: MaxDouble; Right: - MaxDouble; Bottom: - MaxDouble);
//
  svg_header: string = '<svg width="%dpx" height="%dpx" viewBox="0 0 %0:d %1:d" ' +
    'version="1.1" xmlns="http://www.w3.org/2000/svg">';
  svg_path_format: string = '"' + #10 + '    style="fill:%s;' + ' fill-opacity:%1.2f; fill-rule:%s; stroke:%s;' +
    ' stroke-opacity:%1.2f; stroke-width:%1.2f;"/>'#10;
  svg_path_format2: string = '"' + #10 + '   style="fill:none; stroke:%s; ' +
    'stroke-opacity:%1.2f; stroke-width:%1.2f; %s"/>'#10;

function ColorToHtml(color: cardinal): string;
begin
  Result := Format('#%6.6x', [color and $FFFFFF]);;
end;

function GetAlpha(clr: cardinal): double;
begin
  Result := (clr shr 24) / 255;
end;

constructor TCoordStyle.Create(const afontname: string; afontsize: integer; afontcolor: cardinal);
begin
  Self.FontName := afontname;
  Self.FontSize := afontsize;
  Self.FontColor := afontcolor;
end;

constructor TTextInfo.Create(atext: string; _x, _y: integer; afontsize: integer = 12; afontcolor: cardinal = black;
  aBold: Boolean = false);
begin
  Self.x := _x;
  Self.y := _y;
  Self.text := text;
  Self.FontSize := afontsize;
  Self.FontColor := afontcolor;
  Self.Bold := aBold;
end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure TSvgWriter.AddPolyTree(const tree: CPolyTreeD;  brushColor, penColor: cardinal; PenWidth: double; ShowCoords: Boolean);
var
  pi: PPolyInfo;
begin
  if tree.ChildCount = 0 then
    Exit;

  new(pi);
  pi.polyTreeD := tree;
  pi.BrushClr := brushColor;
  pi.PenClr := penColor;
  pi.PenWidth := PenWidth;
  pi.ShowCoords := ShowCoords;
  pi.ShowFrac := false;
  pi.IsOpen := false;
  fPolyInfos.Add(pi);
end;
{$ENDIF}

procedure TSvgWriter.AddPolyTree(const tree: CPolyTree64; brushColor, penColor: cardinal; PenWidth: double; ShowCoords: Boolean);
var
  pi: PPolyInfo;
begin
  if tree.ChildCount = 0 then
    Exit;

  new(pi);
  pi.polyTree64 := tree;
  pi.BrushClr := brushColor;
  pi.PenClr := penColor;
  pi.PenWidth := PenWidth;
  pi.ShowCoords := ShowCoords;
  pi.ShowFrac := false;
  pi.IsOpen := false;
  fPolyInfos.Add(pi);
end;
//

constructor TSvgWriter.Create(fillRule: TFillRule; const coordFontName: string = 'Verdana'; coordFontSize: integer = 9;
  coordFontColor: cardinal = black);
begin
  fFillRule := fillRule;
  fCoordStyle.FontName := coordFontName;
  fCoordStyle.FontSize := coordFontSize;
  fCoordStyle.FontColor := coordFontColor;
  fPolyInfos := TList.Create;
  fCircleInfos := TList.Create;
  fTextInfos := TList.Create;
end;

destructor TSvgWriter.Destroy;
begin
  ClearAll;
  fPolyInfos.Free;
  fCircleInfos.Free;
  fTextInfos.Free;
  inherited;
end;

procedure TSvgWriter.AddPaths(const paths: CPaths64; IsOpen: Boolean; brushColor, penColor: cardinal; PenWidth: double;
  ShowCoords: Boolean = false);
var
  pi: PPolyInfo;
begin
  if (paths = nil) or (paths.Count = 0) then
    Exit;

  new(pi);
  pi.paths64 := paths;
  pi.BrushClr := brushColor;
  pi.PenClr := penColor;
  pi.PenWidth := PenWidth;
  pi.ShowCoords := ShowCoords;
  pi.ShowFrac := false;
  pi.IsOpen := IsOpen;
  fPolyInfos.Add(pi);
end;
//

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure TSvgWriter.AddPaths(const paths: CPathsD; IsOpen: Boolean; brushColor, penColor: cardinal; PenWidth: double;
  ShowCoords: Boolean = false);
var
  pi: PPolyInfo;
begin
  if (paths = nil) or (paths.Count = 0) then
    Exit;

  new(pi);

  pi.pathsD := paths;
  pi.BrushClr := brushColor;
  pi.PenClr := penColor;
  pi.PenWidth := PenWidth;
  pi.ShowCoords := ShowCoords;
  pi.ShowFrac := true;
  pi.IsOpen := IsOpen;
  fPolyInfos.Add(pi);
end;
{$ENDIF}

procedure TSvgWriter.AddDashedPath(const paths: CPaths64; penColor: cardinal; PenWidth: double;
  const dashes: TArray<Integer>);
var
  pi: PPolyInfo;
  dLen: integer;
begin
  dLen := Length(dashes);
  if dLen = 0 then
    Exit;

  new(pi);
  pi.paths64 := paths;
  pi.BrushClr := 0;
  pi.PenClr := penColor;
  pi.PenWidth := PenWidth;
  pi.ShowCoords := false;
  pi.ShowFrac := false;
  pi.IsOpen := true;
  pi.dashes := dashes;

  fPolyInfos.Add(pi);
end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure TSvgWriter.AddDashedPath(const paths: CPathsD; penColor: cardinal; PenWidth: double;
  const dashes: TArray<Integer>);
var
  pi: PPolyInfo;
  dLen: integer;
begin
  dLen := Length(dashes);
  if dLen = 0 then
    Exit;
  new(pi);

  pi.pathsD := paths;
  pi.BrushClr := 0;
  pi.PenClr := penColor;
  pi.PenWidth := PenWidth;
  pi.ShowCoords := false;
  pi.ShowFrac := true;
  pi.IsOpen := true;
  pi.dashes := dashes;

  // pi.dashes := Copy(dashes, 0, Length(dashes));
  fPolyInfos.Add(pi);
end;
{$ENDIF}

procedure TSvgWriter.AddText(text: string; x, y: integer; FontSize: integer; fontClr: cardinal; Bold: Boolean);
var
  ti: PTextInfo;
begin
  new(ti);
  ti.x := x;
  ti.y := y;
  ti.text := text;
  ti.FontSize := FontSize;
  ti.FontColor := fontClr;
  ti.Bold := Bold;
  fTextInfos.Add(ti);
end;


function TSvgWriter.GetBounds: TSVGBounds;
var
  i: integer;

begin

  Result := PPolyInfo(fPolyInfos[0]).GetBounds;
  for i := 1 to fPolyInfos.Count-1 do
     PPolyInfo(fPolyInfos[i]).ExpandBounds(result);

end;

procedure TSvgWriter.ClearPaths;
var
  i: integer;
begin
  for i := 0 to fPolyInfos.Count - 1 do
    Dispose(PPolyInfo(fPolyInfos[i]));
  fPolyInfos.Clear;

end;

procedure TSvgWriter.ClearText;
var
  i: integer;
begin
  for i := 0 to fTextInfos.Count - 1 do
    Dispose(PTextInfo(fTextInfos[i]));
  fTextInfos.Clear;
end;

procedure TSvgWriter.ClearAll;
begin
  ClearText;
  ClearPaths;
end;

function TSvgWriter.SaveToFile(const filename: string; maxWidth: integer = 0; maxHeight: integer = 0;
  margin: integer = 20): Boolean;
var
  i,// j,
  k: integer;
  frac: integer;
  showCo: Boolean;
  showFr: Boolean;
  x, y: double;
  bounds: TSVGBounds;
  scale: double;
  offsetX, offsetY: integer;
  s: string;
  sInline, dashStr: string;
  sl: TStringList;
  formatSettings: TFormatSettings;
  Path: Pointer;
  PathLen: integer;
const
  fillRuleStr: array [Boolean] of string = ('evenodd', 'nonzero');
  boldFont: array [Boolean] of string = ('normal', '700');
  gap: integer = 5;

  procedure Add(const s: string);
  begin
    sl.Add(sInline + s);
    sInline := '';
  end;

  procedure AddInline(const s: string);
  begin
    sInline := sInline + s;
  end;

begin
  Result := false;

{$IF NOT Defined(fpc) AND (CompilerVersion > 19)}   // Delphi XE +
  formatSettings := TFormatSettings.Create;
{$IFEND}
  formatSettings.DecimalSeparator := '.';

  // adjust margin
  if (margin < 20) then
    margin := 20;
  showCo := false;
  showFr := false;
  for i := 0 to fPolyInfos.Count - 1 do
    with PPolyInfo(fPolyInfos[i])^ do
      if ShowCoords then
      begin
        showCo := true;
        if ShowFrac then
          showFr := true;

      end;
  if showCo then
  begin
    if showFr then
      inc(margin, Abs(fCoordStyle.FontSize * 4))
    else
      inc(margin, Abs(fCoordStyle.FontSize * 2));
  end;

  // get scale and offset
  bounds := GetBounds;

  if bounds.IsEmpty then
    Exit;

  scale := 1.0;
  if (maxWidth > 0) and (maxHeight > 0) then
    scale := 1.0 / Max((bounds.Right - bounds.left) / (maxWidth - margin * 2),
      (bounds.Bottom - bounds.Top) / (maxHeight - margin * 2));

  offsetX := margin - Round(bounds.left * scale);
  offsetY := margin - Round(bounds.Top * scale);

  // write SVG
  sl := TStringList.Create;
  try
    if (maxWidth <= 0) or (maxHeight <= 0) then
      Add(Format(svg_header, [Round(bounds.Right - bounds.left) + margin * 2, Round(bounds.Bottom - bounds.Top) + margin
        * 2], formatSettings))
    else
      Add(Format(svg_header, [maxWidth, maxHeight], formatSettings));

    for i := 0 to fPolyInfos.Count - 1 do
      with PPolyInfo(fPolyInfos[i])^ do
      begin
        AddInline('  <path d="');


        Path := GetFirstPath();

        while Path <> nil do
        begin
          try
            PathLen := GetPathLength(Path);

            if PathLen < 2 then
              Continue;

            if not IsOpen and (PathLen < 3) then
              Continue;

            AddInline(Format('M %1.2f %1.2f L ',
              [GetPathX(Path, 0) * scale + offsetX,
               GetPathY(Path, 0) * scale + offsetY], formatSettings));

            for k := 1 to PathLen - 1 do
              AddInline(Format('%1.2f %1.2f ',
                [GetPathX(Path, k) * scale + offsetX,
                 GetPathY(Path, k) * scale + offsetY], formatSettings));

            if not IsOpen then
              AddInline('Z');

          finally
            Path := GetNextPath(Path);
          end;
        end;  // j

        if not IsOpen then
          Add(Format(svg_path_format, [ColorToHtml(BrushClr), GetAlpha(BrushClr), fillRuleStr[fFillRule = TFillRule.NonZero],
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
          Add(Format(svg_path_format2, [ColorToHtml(PenClr), GetAlpha(PenClr), PenWidth, dashStr], formatSettings));
        end;

        if (ShowCoords) then
        begin
          if ShowFrac then
            frac := 2
          else
            frac := 0;
          with fCoordStyle do
            Add(Format('<g font-family="%s" font-size="%d" fill="%s">', [FontName, FontSize, ColorToHtml(FontColor)],
              formatSettings));

          Path := GetFirstPath();
          while Path <> nil do
          begin
            for k := 0 to GetPathLength(Path)-1 do
            begin
              x := GetPathX(Path, k);
              y := GetPathY(Path, k);
              Add(Format('  <text x="%1.2f" y="%1.2f">%1.*f,%1.*f</text>', [x * scale + offsetX, y * scale + offsetY,
                  frac, x, frac, y], formatSettings));
            end;
           Path := GetNextPath(Path);
          end; // j

          Add('</g>'#10);
        end;
      end;

    for i := 0 to fTextInfos.Count - 1 do
      with PTextInfo(fTextInfos[i])^ do
      begin
        Add(Format('  <g font-family="Verdana" font-style="normal" ' + 'font-weight="%s" font-size="%d" fill="%s">' +
          '<text x="%1.2f" y="%1.2f">%s</text></g>', [boldFont[Bold], Round(FontSize * scale), ColorToHtml(FontColor),
          (x) * scale + offsetX, (y) * scale + offsetY, text], formatSettings));
      end;

    Add('</svg>'#10);
    sl.SaveToFile(filename);
    Result := true;
  finally
    sl.Free;
  end;
end;

procedure AddSubject(SVG: TSvgWriter; const paths: CPaths64; ShowCoords: Boolean = false);
begin
  SVG.AddPaths(paths, false, $200099FF, $800066FF, 1.0, ShowCoords);
end;

procedure AddOpenSubject(SVG: TSvgWriter; const paths: CPaths64);
begin
  SVG.AddPaths(paths, true, $0, $800066FF, 2.2);
end;

procedure AddClip(SVG: TSvgWriter; const paths: CPaths64; ShowCoords: Boolean = false);
begin
  SVG.AddPaths(paths, false, $10FF9900, $80FF6600, 1.0, ShowCoords);
end;

procedure AddSolution(SVG: TSvgWriter; const paths: CPaths64; ShowCoords: Boolean);
begin
  SVG.AddPaths(paths, false, $8066FF66, $FF006600, 1.0, ShowCoords);
end;

procedure AddSolution(SVG: TSvgWriter; const tree: CPolyTree64; ShowCoords: Boolean = false);
begin
  SVG.AddPolyTree(tree, $8066FF66, $FF006600, 1.0, ShowCoords);
end;


procedure AddOpenSolution(SVG: TSvgWriter; const paths: CPaths64);
begin
  SVG.AddPaths(paths, true, $0, $FF006600, 1.5);
end;

procedure SaveSvg(SVG: TSvgWriter; const filename: string; width: integer = 0; height: integer = 0;
  margin: integer = 0);
begin
  SVG.SaveToFile(filename, width, height, margin);
end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure AddSubject(SVG: TSvgWriter; const paths: CPathsD; ShowCoords: Boolean = false);
begin
  SVG.AddPaths(paths, false, $200099FF, $800066FF, 1.0, ShowCoords);
end;

procedure AddOpenSubject(SVG: TSvgWriter; const paths: CPathsD);
begin
  SVG.AddPaths(paths, true, $0, $400066FF, 2.2);
end;

procedure AddClip(SVG: TSvgWriter; const paths: CPathsD; ShowCoords: Boolean = false);
begin
  SVG.AddPaths(paths, false, $10FF9900, $80FF6600, 1.0, ShowCoords);
end;

procedure AddSolution(SVG: TSvgWriter; const paths: CPathsD; ShowCoords: Boolean);
begin
  SVG.AddPaths(paths, false, $8066FF66, $FF006600, 1.0, ShowCoords);
end;

procedure AddSolution(SVG: TSvgWriter; const tree: CPolyTreeD; ShowCoords: Boolean = false);
begin
  SVG.AddPolyTree(tree, $8066FF66, $FF006600, 1.0, ShowCoords);
end;


procedure AddOpenSolution(SVG: TSvgWriter; const paths: CPathsD);
begin
  SVG.AddPaths(paths, true, $0, $FF006600, 1.5);
end;
{$ENDIF}


{ TPolyInfo }

procedure TPolyInfo.ExpandBounds(var Bounds: TSVGBounds);
var
  Bounds64: TRect64;
begin
  case PathType of

    TPathType.Paths64, TPathType.PolyTree64:
      begin
        if PathType =  TPathType.Paths64 then
          Bounds64 := Fpaths64.GetBounds
        else
          Bounds64 := FpolyTree64.GetBounds;

        if Bounds64.left < Bounds.Left then
          Bounds.Left := Bounds64.left;

        if Bounds64.right > Bounds.right then
          Bounds.right := Bounds64.right;

        if Bounds64.top < Bounds.top then   //y is down positive
          Bounds.top := Bounds64.top;

        if Bounds64.bottom > Bounds.bottom then
          Bounds.bottom := Bounds64.bottom;
      end;


{$IFDEF CLIPPER_DOUBLE_COORDINATES}
    TPathType.PathsD:
      FpathsD.ExpandBounds(Bounds);

    TPathType.PolyTreeD:
      FpolyTreeD.ExpandBounds(Bounds);

{$ENDIF}
  end;

end;


function TPolyInfo.GetBounds: TSVGBounds;
var
  Bounds64: TRect64;
begin
  case PathType of

    TPathType.Paths64, TPathType.PolyTree64:
      begin
        if PathType =  TPathType.Paths64 then
          Bounds64 := Fpaths64.GetBounds
        else
          Bounds64 := FpolyTree64.GetBounds;

        result := TSVGBounds.Create(Bounds64.left, Bounds64.right, Bounds64.top, Bounds64.bottom);
      end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
    TPathType.PathsD:
      result := FpathsD.GetBounds;

    TPathType.PolyTreeD:
      result := FpolyTreeD.GetBounds;

{$ENDIF}
  end;

end;


function TPolyInfo.GetFirstPath: Pointer;
begin
  case PathType of
    TPathType.Paths64:
      result := Fpaths64.FirstPath;


    TPathType.PolyTree64:
      result := FpolyTree64.FirstPath;
  {$IFDEF CLIPPER_DOUBLE_COORDINATES}
    TPathType.PathsD:
      result := FpathsD.FirstPath;

    TPathType.PolyTreeD:
      result := FpolyTreeD.FirstPath;

  {$ENDIF}
  else
    result :=  nil;
  end;
end;

function TPolyInfo.GetNextPath(PrePath: Pointer): Pointer;
begin
   case PathType of
    TPathType.Paths64:
      result := Fpaths64.NextPath(PrePath);

    TPathType.PolyTree64:
      result := FpolyTree64.NextPath(PrePath);
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
    TPathType.PathsD:
      result := FpathsD.NextPath(PrePath);

    TPathType.PolyTreeD:
      result := FpolyTreeD.NextPath(PrePath);

{$ENDIF}
  else
    result := nil;
  end;
end;

function TPolyInfo.GetPathLength(Path: pointer): integer;
begin
  case PathType of


    TPathType.Paths64, TPathType.PolyTree64:
      result := CPath64(Path).VertexCount;
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
    TPathType.PathsD, TPathType.PolyTreeD:
      result := CPathD(Path).VertexCount;
{$ENDIF}
  else
    result := 0;
  end;
end;

function TPolyInfo.GetPaths64: CPaths64;
begin
  result := FPaths64;
end;

function TPolyInfo.GetPathX(Path: pointer; index: integer): double;
begin

  case PathType of

    TPathType.Paths64, TPathType.PolyTree64:
       result := CPath64(Path).Vertex[index].X;
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
    TPathType.PathsD, TPathType.PolyTreeD:
       result := CPathD(Path).Vertex[index].X;
 {$ENDIF}

  else
    result := 0;
  end;
end;

function TPolyInfo.GetPathY(Path: pointer; index: integer): double;
begin
  case PathType of
    TPathType.Paths64, TPathType.PolyTree64:
       result := CPath64(Path).Vertex[index].Y;
 {$IFDEF CLIPPER_DOUBLE_COORDINATES}
    TPathType.PathsD, TPathType.PolyTreeD:
       result := CPathD(Path).Vertex[index].Y;
 {$ENDIF}
  else
    result := 0;
  end;

end;

function TPolyInfo.GetPolyTree64: CPolyTree64;
begin
  result :=  FPolyTree64;
end;

procedure TPolyInfo.SetPaths64(const Value: CPaths64);
begin
  PathType := TPathType.Paths64;
  FPaths64 := Value;

end;



procedure TPolyInfo.SetPolyTree64(const Value: CPolyTree64);
begin
  PathType := TPathType.PolyTree64;
  FPolyTree64 := Value;
end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
function TPolyInfo.GetPathsD: CPathsD;
begin
  result := FPathsD;
end;


function TPolyInfo.GetPolyTreeD: CPolyTreeD;
begin
  result :=  FPolyTreeD;
end;

procedure TPolyInfo.SetPathsD(const Value: CPathsD);
begin
  PathType := TPathType.PathsD;
  FPathsD := Value;
end;

procedure TPolyInfo.SetPolyTreeD(const Value: CPolyTreeD);
begin
  PathType := TPathType.PolyTreeD;
  FPolyTreeD := Value;

end;

{$ELSE}
{ TSVGBounds }

constructor TSVGBounds.Create(left, right, top, bottom: double);
begin
  self.left := left;
  self.right := right;
  self.top := top;
  self.bottom := bottom;

end;

function TSVGBounds.IsEmpty: boolean;
begin
   result := (bottom <= top) or (right <= left); // y axis down
end;
{$ENDIF}








end.
