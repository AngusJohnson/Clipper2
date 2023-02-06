
#include <cstdlib>
#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace std;
using namespace Clipper2Lib;


void System(const std::string &filename);
void TestingZ_Int64();
void TestingZ_Double();

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
  //TestingZ_Int64();
  TestingZ_Double();
}

void TestingZ_Int64()
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

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  if (solution.size() > 0) {
    // draw circles around intersection points - flagged by z == 1
    PathsD ellipses;
    double r = 3.0;
    for (const Point64& pt : solution[0])
      if (pt.z == 1)
      {
        ellipses.push_back(Ellipse(RectD(pt.x - r, pt.y - r, pt.x + r, pt.y + r), 11));
      }
    SvgAddClip(svg, ellipses, FillRule::NonZero);
  }
  SvgSaveToFile(svg, "usingz_int64.svg", 800, 600, 20);
  System("usingz_int64.svg");
}

void TestingZ_Double()
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

  SvgWriter svg;
  SvgAddSubject(svg, subject, FillRule::NonZero);
  if (solution.size() > 0) 
  {
    // draw circles around intersection points
    PathsD ellipses;
    double r = 3.0;
    for (const PointD& pt : solution[0])
      if (pt.z == 1)
        ellipses.push_back(Ellipse(RectD(pt.x - r, pt.y - r, 
          pt.x + r, pt.y + r), 11));

    SvgAddSolution(svg, ellipses, FillRule::NonZero, false);
  }
  SvgSaveToFile(svg, "usingz_double.svg", 320, 320, 0);
  System("usingz_double.svg");
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
