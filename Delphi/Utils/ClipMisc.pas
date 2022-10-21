unit ClipMisc;

interface

uses
  SysUtils, Types, Classes, Math, Clipper, Clipper.Core;

function PathToString(const path: TPath64): string;

function ScaleAndOffset(const paths: TPathsD; scale: single; offset: Clipper.Core.TPointD): TPathsD;

function MakeRandomPath(maxWidth, maxHeight, count: Integer;
  margin: Integer = 0): TPath64; overload;
function MakeRandomPathD(maxWidth, maxHeight, count: Integer;
  margin: Integer = 10): TPathD;

function Ellipse(const rec: TRect64; steps: integer = 0;
    reverse_orientation: Boolean = false): TPath64; overload;
function Ellipse(const rec: TRectD; steps: integer = 0;
    reverse_orientation: Boolean = false): TPathD; overload;

function MakeNPointedStar(const rec: TRect64;
  points: integer = 5): TPath64; overload;

function PointInPath(const pt: TPointD; const path: TPathD): Boolean;

implementation

function PathToString(const path: TPath64): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to high(path) do
    Result := Result + format('%d,%d ',[path[i].X,path[i].Y]);
  Result := Result + #13#10;
end;

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

procedure GetSinCos(angle: double; out sinA, cosA: double);
{$IFDEF INLINE} inline; {$ENDIF}
{$IFNDEF FPC}
var s, c: extended;
{$ENDIF}
begin
{$IFDEF FPC}
  Math.SinCos(angle, sinA, cosA);
{$ELSE}
  Math.SinCos(angle, s, c);
  sinA := s; cosA := c;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRect64; steps: integer;
  reverse_orientation: Boolean): TPath64;
var
  tmp: TPathD;
begin
  tmp := Ellipse(RectD(rec), steps, reverse_orientation);
  Result := Path64(tmp);
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRectD; steps: integer;
  reverse_orientation: Boolean): TPathD;
var
  i: Integer;
  sinA, cosA: double;
  centre, radius, delta: TPointD;
begin
  result := nil;
  if rec.IsEmpty then Exit;
  with rec do
  begin
    centre := PointD((Left+Right) * 0.5, (Top+Bottom)  * 0.5);
    radius := PointD(Width * 0.5, Height  * 0.5);
  end;
  if steps < 4 then
    steps := Max(4, Round(Pi * Sqrt(rec.width + rec.height)));
  if reverse_orientation then
    GetSinCos(-2 * Pi / Steps, sinA, cosA) else
    GetSinCos(2 * Pi / Steps, sinA, cosA);
  delta.x := cosA; delta.y := sinA;
  SetLength(Result, Steps);
  Result[0] := PointD(centre.X + radius.X, centre.Y);
  for i := 1 to steps -1 do
  begin
    Result[i] := PointD(centre.X + radius.X * delta.x,
      centre.Y + radius.y * delta.y);
    delta :=  PointD(delta.X * cosA - delta.Y * sinA,
      delta.Y * cosA + delta.X * sinA);
  end;
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
