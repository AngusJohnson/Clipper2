#include <cstdlib>
#include <string>

#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace Clipper2Lib;

void DoSimpleTest(bool show_solution_coords = false);
PathI MakeRandomPoly(int width, int height, unsigned vertCnt);
void System(const std::string &filename);

int main()
{  
  DoSimpleTest();    
}

inline PathI MakeStar(const PointI& center, int radius, int points)
{
  if (!(points % 2)) --points;
  if (points < 5) points = 5;
  PathI tmp = Ellipse<Integer>(center, radius, radius, points);
  PathI result;
  result.reserve(points);
  result.push_back(tmp[0]);
  for (int i = points - 1, j = i / 2; j;)
  {
    result.push_back(tmp[j--]);
    result.push_back(tmp[i--]);
  }
  return result;
}


void DoSimpleTest(bool show_solution_coords)
{
  PathsI tmp, solution;
  FillRule fr = FillRule::NonZero;

  PathsI subject, clip;
  subject.push_back(MakeStar(PointI(225, 225), 220, 9));
  clip.push_back(Ellipse<Integer>(PointI(225,225), 150, 150));  
  
  //Intersect both shapes and then 'inflate' result -10 (ie deflate)
  solution = Intersect(subject, clip, fr);
  solution = InflatePaths(solution, -10, JoinType::Round, EndType::Polygon);

  SvgWriter svg;
  SvgAddSubject(svg, subject, fr);
  SvgAddClip(svg, clip, fr);
  SvgAddSolution(svg, solution, fr, false);
  SvgSaveToFile(svg, "solution.svg", 450, 450, 10);
  System("solution.svg");
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
