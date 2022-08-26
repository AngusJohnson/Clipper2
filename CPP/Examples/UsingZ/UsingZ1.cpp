
#include <cstdlib>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace std;
using namespace Clipper2Lib;


void System(const std::string &filename);

class MyClass {
public:
  void myZCB(const Point64& e1bot, const Point64& e1top,
    const Point64& e2bot, const Point64& e2top, Point64& pt)
  {
    pt.z = 1;
  }
};

int main(int argc, char* argv[])
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

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  if (solution.size() > 0) {
    // draw circles around intersection points
    PathsD ellipses;
    for (const Point64& pt : solution[0])
      if (pt.z == 1)
      {
        ellipses.push_back(Ellipse(RectD(pt.x - 3, pt.y - 3, pt.x + 3, pt.y + 3), 11));
      }
    SvgAddClip(svg, ellipses, FillRule::NonZero);
  }
  SvgSaveToFile(svg, "solution_off.svg", 800, 600, 20);
  System("solution_off.svg");
}
//---------------------------------------------------------------------------

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
