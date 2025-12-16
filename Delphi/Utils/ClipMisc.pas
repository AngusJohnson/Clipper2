unit ClipMisc;

interface

uses
  SysUtils, Classes, Windows, Math, ShellAPI, Clipper.Core;

function ScaleAndOffset(const paths: TPathsD; scale: single; offset: Clipper.Core.TPointD): TPathsD;

function MakeRandomPath(maxWidth, maxHeight, count: Integer;
  margin: Integer = 0): TPath64; overload;

function MakeRandomPathD(maxWidth, maxHeight, count: Integer;
  margin: Integer = 10): TPathD;

function MakeNPointedStar(const rec: TRect64;
  points: integer = 5): TPath64; overload;

function PointInPath(const pt: TPointD; const path: TPathD): Boolean;

procedure DisplayAsSvg(const caption: string;
  fillRule: TFillRule; const subj, clip, sol: TPaths64;
  hideCaption: Boolean = false); overload;

procedure DisplayAsSvg(const caption: string;
  fillRule: TFillRule; const subj, clip, sol: TPathsD;
  hideCaption: Boolean = false); overload;

procedure DisplayAsSvg_Open(const caption: string;
  fillRule: TFillRule; isJoined: Boolean;
  const openSub, clip, sol: TPaths64; solIsOpen: Boolean;
  hideCaption: Boolean = false); overload;

procedure DisplayAsSvg_Open(const caption: string;
  fillRule: TFillRule; isJoined: Boolean;
  const openSub, clip, sol: TPathsD; solIsOpen: Boolean;
  hideCaption: Boolean = false); overload;

procedure DisplayAsSvg_MultiColor(const caption: string;
  fillRule: TFillRule; const subj, sol: TPathsD);

implementation

uses
  Clipper, Clipper.SVG;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure DisplayAsSvg(const caption: string;
  fillRule: TFillRule; const subj, clip, sol: TPaths64;
  hideCaption: Boolean = false);
var
  filename: string;
  r: TRect64;
begin
  ForceDirectories('.\SVG');
  filename := format('.\SVG\%s.SVG',[caption]);
  with TSvgWriter.Create(fillRule) do
  try
    if Assigned(subj) then
      AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    if Assigned(clip) then
      AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    if Assigned(sol) then
      AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    r := GetBounds(sol);
    if not hideCaption then
      AddText(caption, r.Left + 20, r.Top + 20);
    SaveToFile(filename);
  finally
    free;
  end;
  ShellExecute(0, 'open', PChar(filename), nil, nil, SW_SHOW);
end;
//------------------------------------------------------------------------------

procedure DisplayAsSvg(const caption: string;
  fillRule: TFillRule; const subj, clip, sol: TPathsD;
  hideCaption: Boolean = false);
var
  filename: string;
  r: TRectD;
begin
  ForceDirectories('.\SVG');
  filename := format('.\SVG\%s.SVG',[caption]);
  with TSvgWriter.Create(fillRule) do
  try
    if Assigned(subj) then
      AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    if Assigned(clip) then
      AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    if Assigned(sol) then
      AddPaths(sol, false, $2000FF00, $FF006600, 0.8);
    r := GetBounds(sol);
    if not hideCaption then
      AddText(caption, Round(r.Left) + 20, Round(r.Top) + 20);
    SaveToFile(filename);
  finally
    free;
  end;
  ShellExecute(0, 'open', PChar(filename), nil, nil, SW_SHOW);
end;
//------------------------------------------------------------------------------

procedure DisplayAsSvg_Open(const caption: string;
  fillRule: TFillRule; isJoined: Boolean;
  const openSub, clip, sol: TPaths64; solIsOpen: Boolean;
  hideCaption: Boolean = false);
var
  filename: string;
  r: TRect64;
begin
  ForceDirectories('.\SVG');
  filename := format('.\SVG\%s.SVG',[caption]);
  with TSvgWriter.Create(fillRule) do
  try
    if Assigned(openSub) then
      AddPaths(openSub, not isJoined, $0, $800099FF, 0.8);
    if Assigned(clip) then
      AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    if Assigned(sol) then
    begin
      if solIsOpen then
        AddPaths(sol, false, $0, $FF006600, 1.5) else
        AddPaths(sol, false, $2000FF00, $FF006600, 1.0);
    end;
    r := GetBounds(sol);
    if not hideCaption then
      AddText(caption, r.Left + 20, r.Top + 20);
    SaveToFile(filename);
  finally
    free;
  end;
  ShellExecute(0, 'open', PChar(filename), nil, nil, SW_SHOW);
end;
//------------------------------------------------------------------------------

procedure DisplayAsSvg_Open(const caption: string;
  fillRule: TFillRule; isJoined: Boolean;
  const openSub, clip, sol: TPathsD; solIsOpen: Boolean;
  hideCaption: Boolean);
var
  filename: string;
  r: TRectD;
begin
  ForceDirectories('.\SVG');
  filename := format('.\SVG\%s.SVG',[caption]);
  with TSvgWriter.Create(fillRule) do
  try
    if Assigned(openSub) then
      AddPaths(openSub, not isJoined, $0, $800099FF, 0.8);
    if Assigned(clip) then
      AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    if Assigned(sol) then
    begin
      if solIsOpen then
        AddPaths(sol, false, $0, $FF006600, 1.5) else
        AddPaths(sol, false, $2000FF00, $FF006600, 1.0);
    end;
    r := GetBounds(sol);
    if not hideCaption then
      AddText(caption, Round(r.Left) + 20, Round(r.Top) + 20);
    SaveToFile(filename);
  finally
    free;
  end;
  ShellExecute(0, 'open', PChar(filename), nil, nil, SW_SHOW);
end;
//------------------------------------------------------------------------------

function RandomColor: Cardinal; inline;
begin
  Result := Cardinal(Random($1000000)) or $FF000000;
end;
//------------------------------------------------------------------------------

procedure DisplayAsSvg_MultiColor(const caption: string;
  fillRule: TFillRule; const subj, sol: TPathsD);
var
  i: integer;
  filename: string;
begin
  ForceDirectories('.\SVG');
  filename := format('.\SVG\%s.SVG',[caption]);
  with TSvgWriter.Create(fillRule) do
  try
    if Assigned(subj) then
      AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    if Assigned(sol) then
    begin
      Randomize;
      for i := 0 to High(sol) do
        AddPath(sol[i], false, RandomColor, $20000000, 1.0, false);
    end;
    SaveToFile(filename);
  finally
    free;
  end;
  ShellExecute(0, 'open', PChar(filename), nil, nil, SW_SHOW);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function ScaleAndOffset(const paths: TPathsD; scale: single; offset: Clipper.Core.TPointD): TPathsD;
var
  i, j, len: Integer;
begin
  len := length(paths);
  if len = 0 then Exit;
  setlength(Result, len);

  for i := 0 to len -1 do
  begin
    len := length(paths[i]);
    setlength(Result[i], len);
    for j := 0 to len -1 do
    begin
      Result[i][j].X := offset.X + round(paths[i][j].X * scale);
      Result[i][j].Y := offset.Y + round(paths[i][j].Y * scale);
    end;
  end;
end;
//------------------------------------------------------------------------------

function MakeRandomPath(maxWidth, maxHeight, count: Integer;
  margin: Integer): TPath64;
var
  i: Integer;
begin
  setlength(Result, count);
  for i := 0 to count -1 do with Result[i] do
  begin
    X := Random(maxWidth - 2 * margin) + margin;
    Y := Random(maxHeight - 2 * margin) + margin;
  end;
end;
//------------------------------------------------------------------------------

function MakeRandomPathD(maxWidth, maxHeight, count: Integer;
  margin: Integer = 10): TPathD;
var
  tmp: TPath64;
begin
  tmp := MakeRandomPath(maxWidth, maxHeight, count, margin);
  Result := PathD(tmp);
end;
//------------------------------------------------------------------------------

function MakeNPointedStar(const rec: TRect64; points: integer = 5): TPath64;
var
  i,j, jump, len: integer;
  tmp: TPath64;
begin
  if not Odd(points) then dec(points);
  if (points < 5) then points := 5;
  tmp := Clipper.Core.Ellipse(rec, points);
  len := Length(tmp);
  SetLength(Result, len);
  jump := len div 2;
  j := 0;
  for i := 0 to len -1 do
  begin
    Result[i] := tmp[j mod len];
    inc(j, jump);
  end;
end;
//------------------------------------------------------------------------------

function PointInPath(const pt: TPointD; const path: TPathD): Boolean;
var
  i: integer;
begin
  Result := true;
  for i := 0 to high(path) do
    if Clipper.Core.PointsNearEqual(pt, path[i], 0.001) then Exit;
  Result := false;
end;
//------------------------------------------------------------------------------

end.
