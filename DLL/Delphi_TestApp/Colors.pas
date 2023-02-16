unit Colors;

interface

uses
  Windows,
  SysUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type
  TColor32 = type Cardinal;

  TARGB = packed record
    case boolean of
      false: (B: Byte; G: Byte; R: Byte; A: Byte);
      true : (Color: TColor32);
  end;

  THsl = packed record
    hue  : byte; sat  : byte; lum  : byte; alpha: byte;
  end;

function RainbowColor(fraction: double; luminance: byte = 128): TColor32;

implementation

function HslToRgb(hslColor: THsl): TColor32;
var
  rgba: TARGB absolute result;
  hsl: THsl absolute hslColor;
  c, x, m, a: Integer;
begin
  //formula from https://www.rapidtables.com/convert/color/hsl-to-rgb.html
  c := ((255 - abs(2 * hsl.lum - 255)) * hsl.sat) shr 8;
  a := 252 - (hsl.hue mod 85) * 6;
  x := (c * (255 - abs(a))) shr 8;
  m := hsl.lum - c div 2;
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

function RainbowColor(fraction: double; luminance: byte = 128): TColor32;
var
  hsl: THsl;
begin
  if (fraction < 0) or (fraction > 1) then
    fraction := frac(fraction);

  hsl.hue := Round(fraction * 255);
  hsl.sat := 255;
  hsl.lum := luminance;
  hsl.alpha := 255;
  Result := HslToRgb(hsl);
end;
//------------------------------------------------------------------------------

end.
