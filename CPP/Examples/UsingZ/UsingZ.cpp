
#include <cstdlib>
#include <cstdint>
#include <cmath>
#include <string>
#include <functional>
#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "clipper2/clipper.engine.h"
#include "clipper2/clipper.triangulation.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace std;
using namespace Clipper2Lib;


void System(const std::string &filename);
void Test1_64();
void Test1_Double();
void Test2_Double();

// use the Z callback to flag intersections by setting z = 1;

class MyClass {
public:
  
  // Point64 callback - see TestingZ_Int64()
  void myZCB(const Point64& e1bot, const Point64& e1top,
    const Point64& e2bot, const Point64& e2top, Point64& pt)
  {
    pt.z = 1;
  }

  // PointD callback - see TestingZ_Double()
  void myZCBD(const PointD& e1bot, const PointD& e1top,
    const PointD& e2bot, const PointD& e2top, PointD& pt)
  {
    pt.z = 1;
  }
};

int main(int argc, char* argv[])
{
  Test1_64();
  Test1_Double();
  Test2_Double();
}

static uint32_t ByteToRainbowColor(uint8_t b)
{
  uint8_t b2;
  switch (b / 43)
  {
    //0..42
  case 0: b2 = (b - 0) * 6; return 0xFFFF0000 | (b2 << 8);    // 0xFFFF0000 -> 0xFFFFFF00 (red..yellow)
    //43..85
  case 1: b2 = (85 - b) * 6; return 0xFF00FF00 | (b2 << 16);  // 0xFFFFFF00 -> 0xFF00FF00 (yellow..lime)
    //86..128
  case 2: b2 = (b - 86) * 6; return 0xFF00FF00 | b2;          // 0xFF00FF00 -> 0xFF00FFFF (lime..aqua)
    //129..171
  case 3: b2 = (171 - b) * 6; return 0xFF0000FF | (b2 << 8);  // 0xFF00FFFF -> 0xFF0000FF (aqua..blue)  
    //172..214
  case 4: b2 = (b - 172) * 6; return 0xFF0000FF | (b2 << 16); // 0xFF0000FF -> 0xFFFF00FF (blue..fuschia)     
    //215..255
  default: b2 = (255 - b) * 6; return 0xFFFF0000 | b2;        // 0xFF0000FF -> 0xFFFF00FF (fuschia..red)     
  }
}

static void DisplayAsSvg(const string& filename, 
  const Paths64* subject, const Paths64* clip, const Paths64* solution)
{
  SvgWriter svg;
  if (subject)
    SvgAddSubject(svg, *subject, FillRule::NonZero);
  if (clip)
    SvgAddClip(svg, *clip, FillRule::NonZero);
  if (solution)
    SvgAddSolution(svg, *solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 320, 320, 0);
  System(filename);
}

static void DisplayAsSvg(const string& filename, 
  const PathsD* subject, const PathsD* clip, const PathsD* solution, bool multi_color = false)
{
  SvgWriter svg;
  if (subject)
    SvgAddSubject(svg, *subject, FillRule::NonZero);
  if (clip)
    SvgAddClip(svg, *clip, FillRule::NonZero);
  if (solution)
  {
    if (multi_color)
    {
#ifdef USINGZ
      for (const PathD& path : *solution)
      {
        // set color using the average 'z' for each triangle
        uint8_t d = (path.size() == 3) ? (path[0].z + path[1].z + path[2].z) / 3.0 : 128;        
        svg.AddPath(path, false, FillRule::NonZero, ByteToRainbowColor(d), 0x80808080, 0.8, false);
      }
#else
      // just set a random color
      SvgAddRCSolution(svg, *solution, FillRule::NonZero, false);
#endif
    }
    else
      SvgAddSolution(svg, *solution, FillRule::NonZero, false);
  }
  SvgSaveToFile(svg, filename, 320, 320, 0);
  System(filename);
}

void Test1_64()
{

  Paths64 subject, solution;
  MyClass mc;
  Clipper64 c64;

  subject.push_back(MakePath({ 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
  c64.AddSubject(subject);
  c64.SetZCallback(
    std::bind(&MyClass::myZCB, mc, std::placeholders::_1,
      std::placeholders::_2, std::placeholders::_3,
      std::placeholders::_4, std::placeholders::_5));
  c64.Execute(ClipType::Union, FillRule::NonZero, solution);

  Paths64 ellipses;
  if (solution.size() > 0) 
  {
    // draw circles around intersection points - flagged by z == 1
    double r = 3.0;
    for (const Point64& pt : solution[0])
      if (pt.z == 1)
      {
        ellipses.push_back(Ellipse(Rect64(pt.x - r, pt.y - r, pt.x + r, pt.y + r), 11));
      }
  }
  DisplayAsSvg("TestingZ1_64.svg", &subject, &ellipses, &solution);
}

void Test1_Double()
{
  PathsD subject, solution;
  MyClass mc;
  ClipperD c;

  subject.push_back(MakePathD({ 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
  c.AddSubject(subject);
  c.SetZCallback(
    std::bind(&MyClass::myZCBD, mc, std::placeholders::_1,
      std::placeholders::_2, std::placeholders::_3,
      std::placeholders::_4, std::placeholders::_5));
  c.Execute(ClipType::Union, FillRule::NonZero, solution);

  PathsD ellipses;
  if (solution.size() > 0)
  {
    // draw circles around intersection points
    double r = 3.0;
    for (const PointD& pt : solution[0])
      if (pt.z == 1)
        ellipses.push_back(Ellipse(RectD(pt.x - r, pt.y - r,
          pt.x + r, pt.y + r), 11));
  }
  DisplayAsSvg("TestingZ1_D.svg", &subject, &ellipses, &solution, false);
}

void Test2_Double()
{
  SvgReader sr = SvgReader(".\\TriSamples\\coral3.svg");
  PathsD subject = sr.paths, sol;
  RectD r = GetBounds(subject);
  PointD mp = r.MidPoint();
  double d = (mp.y - r.top) / 255;
  // for each point in subject, set its 'z' as a 
  // relative distance fron 'mp' (scaled to 255)
  for (PathD& path : subject)
    for (PointD& pt : path)
      pt.z = Distance(pt, mp) / d;
  Triangulate(subject, 0, sol, true);
  DisplayAsSvg("coral3_t2.svg", nullptr, nullptr, &sol, true);
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
