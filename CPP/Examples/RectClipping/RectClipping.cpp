

#include <cstdint>
#include <cstdlib>
#include <sstream>
#include <fstream>
#include <string>
#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/ClipFileLoad.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/Colors.h"
#include "../../Utils/Timer.h"

using namespace std;
using namespace Clipper2Lib;

void System(const std::string& filename);
void PressEnterToExit();

void DoEllipses(int cnt);
void DoRectangles(int cnt);
void DoRandomPoly(int count);
void MeasurePerformance(int min, int max, int step);

const int width = 800, height = 600, margin = 120;


int main(int argc, char* argv[])
{
  srand((unsigned)time(0));
  
  DoEllipses(500);      
  DoRectangles(500);    
  //DoRandomPoly(21);     
  //MeasurePerformance(1000, 5000, 1000);  
  PressEnterToExit();
}

PathI MakeRandomEllipse(int minWidth, int minHeight, int maxWidth, int maxHeight,
  int maxRight, int maxBottom)
{
  int w = maxWidth > minWidth ? minWidth + rand() % (maxWidth - minWidth) : minWidth;
  int h = maxHeight > minHeight ? minHeight + rand() % (maxHeight - minHeight) : minHeight;
  int l = rand() % (maxRight - w);
  int t = rand() % (maxBottom - h);
  return Ellipse(RectI(l, t, l + w, t + h));
}

void DoEllipses(int cnt)
{
  PathsI sub, clp, sol, store;

  RectI rect = RectI(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());
  for (int i = 0; i < cnt; ++i)
    sub.push_back(MakeRandomEllipse(10, 10, 100, 100, width, height));
      
  //////////////////////////////////
  sol = RectClip(rect, sub);
  //////////////////////////////////

  FillRule fr = FillRule::EvenOdd;
  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x100066FF, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  svg.SaveToFile("rectclip1.svg", 800, 600, 0);
  System("rectclip1.svg");
}

PathI MakeRandomRectangle(int minWidth, int minHeight, int maxWidth, int maxHeight,
  int maxRight, int maxBottom)
{
  int w = maxWidth > minWidth ? minWidth + rand() % (maxWidth - minWidth): minWidth;
  int h = maxHeight > minHeight ? minHeight + rand() % (maxHeight - minHeight): minHeight;
  int l = rand() % (maxRight - w);
  int t = rand() % (maxBottom - h);
  PathI result;
  result.reserve(4);
  result.push_back(PointI(l, t));
  result.push_back(PointI(l+w, t));
  result.push_back(PointI(l+w, t+h));
  result.push_back(PointI(l, t+h));
  return result;
}

void DoRectangles(int cnt)
{
  PathsI sub, clp, sol, store;
  RectI rect = RectI(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());
  for (int i = 0; i < cnt; ++i)
    sub.push_back(MakeRandomRectangle(10, 10, 100, 100, width, height));

  sol = RectClip(rect, sub);

  FillRule fr = FillRule::EvenOdd;
  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x100066FF, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  svg.SaveToFile("rectclip2.svg", 800, 600, 0);
  System("rectclip2.svg");
}

PathI MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  PathI result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(PointI(rand() % width, rand() % height));
  return result;
}

PathS MakeRandomPolyD(int width, int height, unsigned vertCnt)
{
  PathS result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(PointS(rand() % width, rand() % height));
  return result;
}

void DoRandomPoly(int count)
{
  PathsS sub, clp, sol;

  // generate random poly
  RectS rect = RectS(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());
  sub.push_back(MakeRandomPolyD(width, height, count));

  //////////////////////////////////
  sol = RectClip(rect, sub, false);
  //////////////////////////////////

  FillRule fr = FillRule::EvenOdd;
  Scalar frac = sol.size() ? 1.0 / sol.size() : 1.0;
  Scalar cum_frac = 0;
  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x100066FF, 0x800066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0x80FF0000, 1, false);
  //svg.AddPaths(sol, false, fr, 0x30AAFF00, 0xFF00FF00, 1, false);
  for (const PathS& sol_path : sol)
  {
    uint32_t c = RainbowColor(cum_frac, 64);
    cum_frac += frac;
    uint32_t c2 = (c & 0xFFFFFF) | 0x20000000;
    svg.AddPath(sol_path, false, fr, c2, c, 1.2, false);
  }
  svg.SaveToFile("rectclip3.svg", width, height, 0);
  System("rectclip3.svg");
}

void MeasurePerformance(int min, int max, int step)
{
  FillRule fr = FillRule::EvenOdd;
  PathsI sub, clp, sol, store;
  RectI rect = RectI(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());

  for (int cnt = min; cnt <= max; cnt += step)
  {
    sub.clear();
    sub.push_back(MakeRandomPoly(width, height, cnt));

    std::cout << std::endl << cnt << " random poly" << std::endl;
    {
      Timer t("ClipperI: ");
      sol = Intersect(sub, clp, fr);
    }

    {
      Timer t("RectClip: ");
      sol = RectClip(rect, sub);
    }

  }

  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x200066FF, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  //svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  Scalar frac = sol.size() ? 1.0 / sol.size() : 1.0;
  Scalar cum_frac = 0;
  for (const PathI& sol_path : sol)
  {
    uint32_t c = RainbowColor(cum_frac, 64);
    cum_frac += frac;
    uint32_t c2 = (c & 0xFFFFFF) | 0x20000000;
    svg.AddPath(sol_path, false, fr, c2, c, 1.2, false);
  }
  svg.SaveToFile("RectClipQ2.svg", 800, 600, 0);
  System("RectClipQ2.svg");
}

void System(const std::string& filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}

void PressEnterToExit()
{
  std::string s;
  std::cout << std::endl << "Press Enter to exit" << std::endl;
  std::getline(std::cin, s);
}



