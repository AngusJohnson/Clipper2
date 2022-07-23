
#include <cstdlib>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace std;
using namespace Clipper2Lib;


void System(const std::string &filename);

int main(int argc, char* argv[])
{
  Paths64 subject, clip, ignored, solution;
  ClipType ct = ClipType::Intersection;;
  FillRule fr = FillRule::EvenOdd;
  
  //triangle offset - with large miter
  Paths64 p, pp;
  p.push_back(MakePath("30, 150, 60, 350, 0, 350"));
  pp.insert(pp.end(), p.begin(), p.end());

  for (int i = 0; i < 5; ++i)
  {
    //nb: the following '10' parameter greatly increases miter limit
    p = InflatePaths(p, 5, JoinType::Miter, EndType::Polygon, 10);
    pp.insert(pp.end(), p.begin(), p.end());
  }

  //rectangle offset - both squared and rounded
  p.clear();
  p.push_back(MakePath("100,30, 340,30, 340,230, 100,230"));
  pp.insert(pp.end(), p.begin(), p.end());
  //nb: using the ClipperOffest class directly here to control 
  //different join types within the same offset operation
  ClipperOffset co;
  co.AddPaths(p, JoinType::Miter, EndType::Joined);
  p = TranslatePaths(p, 120, 100);
  pp.insert(pp.end(), p.begin(), p.end());
  co.AddPaths(p, JoinType::Round, EndType::Joined);
  p = co.Execute(20);
  pp.insert(pp.end(), p.begin(), p.end());

  SvgWriter svg;
  SvgAddSolution(svg, Paths64ToPathsD(pp), fr, false);
  SvgSaveToFile(svg, "solution_off.svg", 800, 600, 20);
  System("solution_off.svg");

  // Because ClipperOffset uses integer coordinates,
  // you'll need to scale coordinates when you 
  // want/need fractional values ...
  const double scale = 100;

  SvgReader svg_reader;
  svg_reader.LoadFromFile("./rabbit.svg");
  p = ScalePaths<int64_t, double>(svg_reader.GetPaths(), scale);          //scale up

  pp.clear();
  pp.reserve(p.size());
  pp.insert(pp.end(), p.begin(), p.end());

  while (p.size())
  {
    //nb: don't forget to scale the delta offset too!
    p = InflatePaths(p, -2.5 * scale, JoinType::Round, EndType::Polygon);
    //RamerDouglasPeucker - not essential but
    //speeds up the loop and also tidies up the result
    p = RamerDouglasPeucker(p, 0.025 * scale);
    pp.reserve(pp.size() + p.size());
    copy(p.begin(), p.end(), back_inserter(pp));
  }

  svg.Clear();
  SvgAddSolution(svg, ScalePaths<double, int64_t>(pp, 1/scale), fr, false);   //scale back down
  SvgSaveToFile(svg, "solution_off2.svg", 450, 720, 0);
  System("solution_off2.svg");

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
