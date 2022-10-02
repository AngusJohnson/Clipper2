
#include <cstdlib>
#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace std;
using namespace Clipper2Lib;


void System(const std::string &filename);
void TestInt64();
void TestDouble();

class MyClass {
public:
  // Point64 callback - see TestInt64()
  void myZCB(const Point64& e1bot, const Point64& e1top,
    const Point64& e2bot, const Point64& e2top, Point64& pt)
  {
    pt.z = 1;
  }

  // PointD callback - see TestDouble()
  void myZCBD(const PointD& e1bot, const PointD& e1top,
    const PointD& e2bot, const PointD& e2top, PointD& pt)
  {
    pt.z = 1;
  }
};

int main(int argc, char* argv[])
{
  TestInt64();
  TestDouble();
}
//---------------------------------------------------------------------------

void TestInt64()
{

  Paths64 subject, solution;
  MyClass mc;
  Clipper64 c64;

  subject.push_back(MakePath("100, 50, 10, 79, 65, 2, 65, 98, 10, 21 "));
  c64.AddSubject(subject);
  c64.SetZCallback(
    std::bind(&MyClass::myZCB, mc, std::placeholders::_1,
      std::placeholders::_2, std::placeholders::_3,
      std::placeholders::_4, std::placeholders::_5));

  c64.Execute(ClipType::Union, FillRule::NonZero, solution);

  /*
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
  SvgSaveToFile(svg, "using_z_64.svg", 800, 600, 20);
  System("using_z_64.svg");
  */
}

void TestDouble()
{

  PathsD subject, solution;
  MyClass mc;
  ClipperD c;

  subject.push_back(MakePathD("100, 50, 10, 79, 65, 2, 65, 98, 10, 21 "));
  c.AddSubject(subject);
  c.SetZCallback(
    std::bind(&MyClass::myZCBD, mc, std::placeholders::_1,
      std::placeholders::_2, std::placeholders::_3,
      std::placeholders::_4, std::placeholders::_5));

  c.Execute(ClipType::Union, FillRule::NonZero, solution);

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  if (solution.size() > 0) {
    // draw circles around intersection points - flagged by z == 1
    PathsD ellipses;
    double r = 5.0;
    for (const PointD& pt : solution[0])
      if (pt.z == 1)
      {
        ellipses.push_back(Ellipse(RectD(pt.x - r, pt.y - r, pt.x + r, pt.y + r), 11));
      }
    SvgAddClip(svg, ellipses, FillRule::NonZero);
  }
  SvgSaveToFile(svg, "using_z_d.svg", 800, 600, 20);
  System("using_z_d.svg");
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
