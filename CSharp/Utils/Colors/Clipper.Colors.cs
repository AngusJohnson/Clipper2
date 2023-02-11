/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  11 February 2023                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.IO;
using System.Runtime.InteropServices;

namespace Clipper2Lib
{

  public static class Colors
  {
    public struct Hsl
    {
      public byte alpha = 0;
      public byte hue = 0;
      public byte sat = 0;
      public byte lum = 0;
      public Hsl(byte a, byte h, byte s, byte l)
      { alpha = a; hue = h; sat = s; lum = l; }
    }

    [StructLayout(LayoutKind.Explicit)]
    public struct Color32
    {
      [FieldOffset(0)]
      public byte b = 0;
      [FieldOffset(1)]
      public byte g = 0;
      [FieldOffset(2)]
      public byte r = 0;
      [FieldOffset(3)]
      public byte a = 0;
      [FieldOffset(0)]
      public UInt32 color;
      public Color32(uint val = 0) { color = val; }
    }

    public static Color32 HslToRgb(Hsl hsl)
    {
      long c, x, m, a;
      c = ((255 - Math.Abs(2 * hsl.lum - 255)) * hsl.sat) >> 8;
      a = 252 - (hsl.hue % 85) * 6;
      x = (c * (255 - Math.Abs(a))) >> 8;
      m = hsl.lum - c / 2;
      Color32 result = new Color32();
      result.a = hsl.alpha;
      switch ((hsl.hue * 6) >> 8)
      {
        case 0: { result.r = (byte) (c + m); result.g = (byte) (x + m); result.b = (byte) (0 + m); break; };
        case 1: { result.r = (byte) (x + m); result.g = (byte) (c + m); result.b = (byte) (0 + m); break; };
        case 2: { result.r = (byte) (0 + m); result.g = (byte) (c + m); result.b = (byte) (x + m); break; };
        case 3: { result.r = (byte) (0 + m); result.g = (byte) (x + m); result.b = (byte) (c + m); break; };
        case 4: { result.r = (byte) (x + m); result.g = (byte) (0 + m); result.b = (byte) (c + m); break; };
        case 5: { result.r = (byte) (c + m); result.g = (byte) (0 + m); result.b = (byte) (x + m); break; };
      }
      return result;
    }

    public static Color32 Rainbow(
      double frac, byte luminance = 128, byte alpha = 255)
    {
      frac = (double) (frac - (int) (frac));
      Hsl hsl = new Hsl(alpha, (byte) (frac * 255), 255, luminance);
      Color32 result = HslToRgb(hsl);
      return result;
    }
  }
  
}
