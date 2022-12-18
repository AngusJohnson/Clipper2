

#include <cstdint>

#include <cstdlib>
#include <sstream>
#include <fstream>
#include <string>
#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/Timer.h"

using namespace std;
using namespace Clipper2Lib;

void System(const std::string& filename);
void PressEnterToExit();

void DoEllipses();
void DoRectangles();
void DoRandomPoly(int count);
void MeasurePerformance(int min, int max, int step);
void MeasureLineClippingPerformance(int line_length);

const int width = 1600, height = 1200, margin = 200;

int main(int argc, char* argv[])
{
  srand((unsigned)time(0));
  DoEllipses();
  DoRectangles();
  DoRandomPoly(31);
  //MeasurePerformance(500, 2500, 500);
  //MeasureLineClippingPerformance(250);
}

Path64 MakeRandomEllipse(int minWidth, int minHeight, int maxWidth, int maxHeight,
  int maxRight, int maxBottom)
{
  int w = maxWidth > minWidth ? minWidth + rand() % (maxWidth - minWidth) : minWidth;
  int h = maxHeight > minHeight ? minHeight + rand() % (maxHeight - minHeight) : minHeight;
  int l = rand() % (maxRight - w);
  int t = rand() % (maxBottom - h);
  return Ellipse(Rect64(l, t, l + w, t + h));
}

void DoEllipses()
{
  Paths64 sub, clp, sol, store;

  const int cnt = 1000;
  Rect64 rect = Rect64(margin, margin, width - margin, height - margin);
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

Path64 MakeRandomRectangle(int minWidth, int minHeight, int maxWidth, int maxHeight,
  int maxRight, int maxBottom)
{
  int w = maxWidth > minWidth ? minWidth + rand() % (maxWidth - minWidth): minWidth;
  int h = maxHeight > minHeight ? minHeight + rand() % (maxHeight - minHeight): minHeight;
  int l = rand() % (maxRight - w);
  int t = rand() % (maxBottom - h);
  Path64 result;
  result.reserve(4);
  result.push_back(Point64(l, t));
  result.push_back(Point64(l+w, t));
  result.push_back(Point64(l+w, t+h));
  result.push_back(Point64(l, t+h));
  return result;
}

void DoRectangles()
{
  Paths64 sub, clp, sol, store;
  const int cnt = 1000;
  Rect64 rect = Rect64(margin, margin, width - margin, height - margin);
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

Path64 MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  Path64 result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Point64(rand() % width, rand() % height));
  return result;
}

PathD MakeRandomPolyD(int width, int height, unsigned vertCnt)
{
  PathD result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(PointD(rand() % width, rand() % height));
  return result;
}

void DoRandomPoly(int count)
{
  PathsD sub_open, clp, sol_open, store;
  RectD rect;

  // generate random poly
  rect = RectD(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());
  sub_open.push_back(MakeRandomPolyD(width, height, count));

  //////////////////////////////////
  sol_open = RectClipLines(rect, sub_open);
  //////////////////////////////////

  FillRule fr = FillRule::EvenOdd;
  SvgWriter svg;
  svg.AddPaths(sub_open, true, fr, 0x0, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol_open, true, fr, 0x0, 0xFF006600, 2.2, false);
  svg.SaveToFile("rectclip3.svg", 800, 600, 0);
  System("rectclip3.svg");
}

void MeasurePerformance(int min, int max, int step)
{
  FillRule fr = FillRule::EvenOdd;
  Paths64 sub, clp, sol, store;
  Rect64 rect;

  for (int cnt = min; cnt <= max; cnt += step)
  {
    // generate random poly
    Rect64 rect = Rect64(margin, margin, width - margin, height - margin);
    clp.push_back(rect.AsPath());
    sub.clear();

    for (int i = 0; i < cnt; ++i)
      sub.push_back(MakeRandomEllipse(100, 100, 100, 100, width, height));

    std::cout << std::endl << cnt << " ellipses" << std::endl;
    {
      Timer t("Clipper64: ");
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
  svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  svg.SaveToFile("RectClipQ.svg", 800, 600, 0);
  System("RectClipQ.svg");

  PressEnterToExit();
}

void MeasureLineClippingPerformance(int line_length)
{
  FillRule fr = FillRule::EvenOdd;
  Paths64 sub_open, clp, sol_open, store;

  // generate random poly
  Rect64 rect = Rect64(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());

  sub_open.push_back(MakeRandomPoly(width, height, line_length));

  std::cout << std::endl << "Measuring line clipping performance" << std::endl;
  std::cout << "line length: " << line_length << std::endl;
  {
    Timer t("Clipper64: ");
    Paths64 dummy;
    Clipper64 c;
    c.AddOpenSubject(sub_open);
    c.AddClip(clp);
    c.Execute(ClipType::Intersection, fr, dummy, sol_open);
    sol_open = Intersect(sub_open, clp, fr);
  }

  {
    Timer t("RectClip: ");
    sol_open = RectClipLines(rect, sub_open);
  }

  SvgWriter svg;
  svg.AddPaths(sub_open, true, fr, 0x0, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol_open, true, fr, 0x0, 0xFF006600, 2.0, false);
  svg.SaveToFile("RectClipL.svg", 800, 600, 0);
  System("RectClipL.svg");

  PressEnterToExit();
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



