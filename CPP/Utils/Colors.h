#ifndef CLIPPER_COLORS_H
#define CLIPPER_COLORS_H

#include <cstdlib>

struct Hsl {
  uint8_t alpha = 0;
  uint8_t hue = 0;
  uint8_t sat = 0;
  uint8_t lum = 0;
  Hsl() {};
  Hsl(uint8_t a, uint8_t h, uint8_t s, uint8_t l) :
    alpha(a), hue(h), sat(s), lum(l) {};
};

union Color32 {
  struct {
    uint8_t b : 8;
    uint8_t g : 8;
    uint8_t r : 8;
    uint8_t a : 8;
  } argb;
  uint32_t color;
};

Color32 HslToRgb(Hsl hsl)
{
  int c, x, m, a;
  c = ((255 - std::abs(2 * hsl.lum - 255)) * hsl.sat) >> 8;
  a = 252 - (hsl.hue % 85) * 6;
  x = (c * (255 - std::abs(a))) >> 8;
  m = hsl.lum - c / 2;
  Color32 result{};
  result.argb.a = hsl.alpha;
  switch ((hsl.hue * 6) >> 8)
  {
  case 0: { result.argb.r = c + m; result.argb.g = x + m; result.argb.b = 0 + m; break; };
    case 1: { result.argb.r = x + m; result.argb.g = c + m; result.argb.b = 0 + m; break; };
    case 2: { result.argb.r = 0 + m; result.argb.g = c + m; result.argb.b = x + m; break; };
    case 3: { result.argb.r = 0 + m; result.argb.g = x + m; result.argb.b = c + m; break; };
    case 4: { result.argb.r = x + m; result.argb.g = 0 + m; result.argb.b = c + m; break; };
    case 5: { result.argb.r = c + m; result.argb.g = 0 + m; result.argb.b = x + m; break; };
  }
  return result;
}

uint32_t RainbowColor(double frac, 
  uint8_t luminance = 128, uint8_t alpha = 255)
{
  frac = static_cast<double>(frac - static_cast<int>(frac));
  Hsl hsl(alpha, static_cast<uint8_t>(frac * 255), 255, luminance);
  Color32 result = HslToRgb(hsl);
  return result.color;
}

#endif // CLIPPER_COLORS_H
