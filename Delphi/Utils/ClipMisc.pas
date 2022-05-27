unit ClipMisc;

interface

uses
  SysUtils, Classes, Math, Clipper, Clipper.Core;

function MakeRandomPath(maxWidth, maxHeight, count: Integer;
  margin: Integer = 0): TPath64; overload;
function MakeRandomPathD(maxWidth, maxHeight, count: Integer;
  margin: Integer = 10): TPathD;

function Ellipse(const rec: TRect64; steps: integer = 0): TPath64; overload;
function Ellipse(const rec: TRectD; steps: integer = 0): TPathD; overload;

function MakeNPointedStar(const rec: TRect64;
  points: integer = 5): TPath64; overload;

implementation

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

function Ellipse(const rec: TRect64; steps: integer): TPath64;
var
  i: Integer;
  r, sinA, cosA: double;
  centre, radius, delta: TPointD;
begin
  result := nil;
  if rec.IsEmpty then Exit;
  with rec do
  begin
    centre := PointD((Left+Right) * 0.5, (Top+Bottom)  * 0.5);
    radius := PointD(Width * 0.5, Height  * 0.5);
  end;
  r := (rec.width + rec.height)/2;
  if steps < 4 then
    steps := Max(4, Round(Pi * Sqrt(r)));
  GetSinCos(2 * Pi / Steps, sinA, cosA);
  delta.x := cosA; delta.y := sinA;
  SetLength(Result, Steps);
  Result[0] := Point64(Round(centre.X + radius.X), Round(centre.Y));
  for i := 1 to steps -1 do
  begin
    Result[i] := Point64(Round(centre.X + radius.X * delta.x),
      Round(centre.Y + radius.y * delta.y));
    delta :=  PointD(delta.X * cosA - delta.Y * sinA,
      delta.Y * cosA + delta.X * sinA);
  end;
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRectD; steps: integer): TPathD;
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
  i,j,k, len: integer;
  tmp: TPath64;
begin
  if not Odd(points) then dec(points);
  if (points < 5) then points := 5;
  tmp := Ellipse(rec, points);
  len := Length(tmp);
  SetLength(Result, len);
  Result[0]:= tmp[0];
  j := len div 2;
  k := j;
  for i := 1 to len -1 do
  begin
    Result[i] := tmp[k mod len];
    inc(k, j);
  end;
end;
//------------------------------------------------------------------------------

end.
