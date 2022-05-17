
#include <cstdlib>
#include "../../Clipper2Lib/clipper.h"
#include "../clipper.svg.h"

using namespace std;
using namespace Clipper2Lib;

inline bool FileExists(const std::string& name)
{
  struct stat buffer;
  return (stat(name.c_str(), &buffer) == 0);
}
//------------------------------------------------------------------------------

inline void SvgAddCaption(SvgWriter& svg, const string& caption, int x, int y)
{
  svg.AddText(caption, 0xFF000000, 14, x, y);
}
//---------------------------------------------------------------------------

inline void SvgAddSubject(SvgWriter& svg, PathsD& path, 
  bool is_closed = true, bool is_joined = true)
{
  if (!is_closed)
    svg.AddPaths(path, !is_joined, 0x0, 0xCCB3B3DA, 0.8, false);
  else
    svg.AddPaths(path, false, 0x1800009C, 0xCCB3B3DA, 0.8, false);
}
//---------------------------------------------------------------------------

inline void SvgAddClip(SvgWriter& svg, PathsD& path)
{
    svg.AddPaths(path, false, 0x129C0000, 0xCCFFA07A, 0.8, false);
}
//---------------------------------------------------------------------------

inline void SvgAddSolution(SvgWriter& svg, PathsD& path,
  bool show_coords, bool is_closed = true, bool is_joined = true)
{
  if (!is_closed)
    svg.AddPaths(path, !is_joined, 0x0, 0xFF003300, 0.8, show_coords);
  else
    svg.AddPaths(path, false, 0xFF80ff9C, 0xFF003300, 0.8, show_coords);
}
//---------------------------------------------------------------------------

inline void SvgSaveToFile(SvgWriter& svg, 
  const string& filename, FillRule fill_rule,
  int max_width = 0, int max_height = 0, int margin = 0)
{
  if (FileExists(filename)) remove(filename.c_str());
  svg.fill_rule = fill_rule;
  svg.SetCoordsStyle("Verdana", 0xFF0000AA, 9);
  svg.SaveToFile(filename, max_width, max_height, margin);
}
//---------------------------------------------------------------------------


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
  p.push_back(MakePath("100,0, 340,0, 340,200, 100,200"));
  pp.insert(pp.end(), p.begin(), p.end());
  //nb: using the ClipperOffest class directly here to control 
  //different join types within the same offset operation
  ClipperOffset co;
  co.AddPaths(p, JoinType::Miter, EndType::Joined);
  p = OffsetPaths(p, 120, 100);
  pp.insert(pp.end(), p.begin(), p.end());
  co.AddPaths(p, JoinType::Round, EndType::Joined);
  p = co.Execute(20);
  pp.insert(pp.end(), p.begin(), p.end());

  SvgWriter svg;
  SvgAddSolution(svg, Paths64ToPathsD(pp), false);
  SvgSaveToFile(svg, "solution_off.svg", fr, 800, 600, 20);
  system("solution_off.svg");

  // Because ClipperOffset uses integer coordinates,
  // you'll need to scale coordinates when you 
  // want/need fractional values ...
  const double scale = 100;

  SvgReader svg_reader;
  svg_reader.LoadFromFile("./rabbit.svg");
  p = PathsDToPaths64(svg_reader.GetPaths());
  p = ScalePaths(p, scale);                                 //scale up
  pp.clear();
  pp.insert(pp.end(), p.begin(), p.end());

  while (p.size())
  {
    //nb: don't forget to scale the delta offset too!
    p = InflatePaths(p, -2.5 * scale, JoinType::Round, EndType::Polygon);
    //RamerDouglasPeucker - not essential but
    //speeds up the loop and also tidies up the result
    p = RamerDouglasPeucker(p, 0.25 * scale);
    pp.insert(pp.end(), p.begin(), p.end());
  }

  svg.Clear();
  SvgAddSolution(svg, ScalePathsD(pp, 1/scale), false);      //scale back down
  SvgSaveToFile(svg, "solution_off2.svg", fr, 450, 720, 0);
  system("solution_off2.svg");

}
//---------------------------------------------------------------------------
