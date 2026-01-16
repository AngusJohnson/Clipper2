
#include <cstdlib>
#include <cstdint>
#include <string>
#include <algorithm>
#include <iterator>
#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "clipper2/clipper.offset.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace std;
using namespace Clipper2Lib;

void System(const std::string& filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}

static void DisplayAsSvgImage(const std::string& caption, FillRule fillrule,
  const PathsD* subject, const PathsD* clip, const PathsD* solution)
{
  const std::string filename = caption + ".SVG";
  SvgWriter svg;
  if (subject)
    SvgAddSubject(svg, *subject, fillrule);
  if (clip)
    SvgAddClip(svg, *clip, fillrule);
  if (solution)
      SvgAddSolution(svg, *solution, fillrule, false);
  SvgSaveToFile(svg, filename, 400, 400, 10);
  System(filename);
}

static void DoRabbit()
{
  SvgReader svg_reader("./rabbit.svg");
  PathsD p = svg_reader.paths;
  PathsD solution(p);
  while (p.size())
  {
    p = InflatePaths(p, -5, JoinType::Round, EndType::Polygon);
    // SimplifyPaths is not essential but is **highly recommended**
    // because it speeds up the loop by removing tiny artefacts
    p = SimplifyPaths(p, 0.25);
    copy(p.begin(), p.end(), back_inserter(solution));
  }
  DisplayAsSvgImage("rabbit_offset", FillRule::EvenOdd, nullptr, nullptr, &solution);
}

static void DoSimpleShapes()
{
  // OPEN_PATHS SVG:

  PathsD op1, op2;
  FillRule fr2 = FillRule::EvenOdd;
  SvgWriter svg2;
  op1.push_back(MakePathD({ 80,60, 20,20, 180,20, 180,70, 25,150, 20,180, 180,180 }));
  op2 = InflatePaths(op1, 15, JoinType::Miter, EndType::Square, 3);
  SvgAddOpenSubject(svg2, op1, fr2, false);
  SvgAddSolution(svg2, op2, fr2, false);
  SvgAddCaption(svg2, "Miter Joins; Square Ends", 20, 210);
  op1 = TranslatePaths<double>(op1, 210, 0);
  op2 = InflatePaths(op1, 15, JoinType::Square, EndType::Square);
  SvgAddOpenSubject(svg2, op1, fr2, false);
  SvgAddSolution(svg2, op2, fr2, false);
  SvgAddCaption(svg2, "Square Joins; Square Ends", 230, 210);
  op1 = TranslatePaths<double>(op1, 210, 0);
  op2 = InflatePaths(op1, 15, JoinType::Bevel, EndType::Butt, 3);
  SvgAddOpenSubject(svg2, op1, fr2, false);
  SvgAddSolution(svg2, op2, fr2, false);
  SvgAddCaption(svg2, "Bevel Joins; Butt Ends", 440, 210);
  op1 = TranslatePaths<double>(op1, 210, 0);
  op2 = InflatePaths(op1, 15, JoinType::Round, EndType::Round);
  SvgAddOpenSubject(svg2, op1, fr2, false);
  SvgAddSolution(svg2, op2, fr2, false);
  SvgAddCaption(svg2, "Round Joins; Round Ends", 650, 210);
  SvgSaveToFile(svg2, "open_paths.svg", 800, 600, 20);
  System("open_paths.svg");

  // POLYGON JOINTYPES SVG:

  // 1. triangle offset - with large miter
  int err = 0, scale = 100;
  PathsD p, solution;
  p.push_back(MakePathD({ 30,150, 60,350, 0,350 }));
  solution.insert(solution.end(), p.begin(), p.end());
  for (int i = 0; i < 5; ++i)
  {
    p = InflatePaths(p, 5, JoinType::Miter, EndType::Polygon, 10);
    solution.insert(solution.end(), p.begin(), p.end());
  }
  
  // 2. open rectangles offset bevelled, squared & rounded ...

  p.clear();
  p.push_back(MakePathD({ 100,30, 340,30, 340,230, 100,230 }));
  p.push_back(TranslatePath<double>(p[0], 60, 50));
  p.push_back(TranslatePath<double>(p[1], 60, 50));

  SvgWriter svg;
  SvgAddOpenSubject(svg, p);
  SvgAddCaption(svg, "Bevelled", 100, 15);
  SvgAddCaption(svg, "Squared", 160, 65);
  SvgAddCaption(svg, "Rounded", 220, 115);

  // nb: we must use the ClipperOffest class directly if we want to
  // perform different join types within the same offset operation
  // ClipperOffset only supports int64_t coords so, if we want better than unit
  // precision, we have to scale manually. (InflatePaths does this scaling internally)
  ClipperOffset co; 
  p = ScalePaths<double, double>(p, scale, err);
  // AddPaths - paths must be int64_t paths
  co.AddPath(TransformPath<int64_t, double>(p[0]), JoinType::Bevel, EndType::Joined);
  co.AddPath(TransformPath<int64_t, double>(p[1]), JoinType::Square, EndType::Joined);
  co.AddPath(TransformPath<int64_t, double>(p[2]), JoinType::Round, EndType::Joined);
  Paths64 sol64;
  co.Execute(scale * 10, sol64); // ClipperOffset solutions must be int64_t
  
  // de-scale and append to solution ...
  p = ScalePaths<double, int64_t>(sol64, 1.0 / scale, err);
  solution.insert(solution.end(), p.begin(), p.end());
  
  string filename = "polygon_jointypes.svg";
  SvgAddSolution(svg, solution, FillRule::EvenOdd, false);
  SvgSaveToFile(svg, filename, 800, 600, 20);
  System(filename);
}

int main(int argc, char* argv[])
{
  DoSimpleShapes();
  DoRabbit();
}

