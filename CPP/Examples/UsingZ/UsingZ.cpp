
#include <cstdlib>
#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace std;
using namespace Clipper2Lib;


void System(const std::string &filename);
void TestingZ_Int64();
void TestingZ_Scalar();

// use the Z callback to flag intersections by setting z = 1;

class MyClass {
public:
  
  // PointI callback - see TestingZ_Int64()
  void myZCB(const PointI& e1bot, const PointI& e1top,
    const PointI& e2bot, const PointI& e2top, PointI& pt)
  {
    pt.z = 1;
  }

  // PointS callback - see TestingZ_Scalar()
  void myZCBD(const PointS& e1bot, const PointS& e1top,
    const PointS& e2bot, const PointS& e2top, PointS& pt)
  {
    pt.z = 1;
  }
};

int main(int argc, char* argv[])
{
  //TestingZ_Int64();
  TestingZ_Scalar();
}

void TestingZ_Int64()
{

  PathsI subject, solution;
  MyClass mc;
  ClipperI c64;

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
    PathsS ellipses;
    Scalar r = 3.0;
    for (const PointI& pt : solution[0])
      if (pt.z == 1)
      {
        ellipses.push_back(Ellipse(RectS(pt.x - r, pt.y - r, pt.x + r, pt.y + r), 11));
      }
    SvgAddClip(svg, ellipses, FillRule::NonZero);
  }
  SvgSaveToFile(svg, "usingz_int64.svg", 800, 600, 20);
  System("usingz_int64.svg");
}

void TestingZ_Scalar()
{
  PathsS subject, solution;
  MyClass mc;
  ClipperS c;

  subject.push_back(MakePathS({ 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
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
    PathsS ellipses;
    Scalar r = 3.0;
    for (const PointS& pt : solution[0])
      if (pt.z == 1)
        ellipses.push_back(Ellipse(RectS(pt.x - r, pt.y - r, 
          pt.x + r, pt.y + r), 11));

    SvgAddSolution(svg, ellipses, FillRule::NonZero, false);
  }
  SvgSaveToFile(svg, "usingz_Scalar.svg", 320, 320, 0);
  System("usingz_Scalar.svg");
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
