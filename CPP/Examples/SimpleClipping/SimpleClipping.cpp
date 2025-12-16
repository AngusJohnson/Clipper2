#include <cstdlib>
#include <string>

#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"

using namespace Clipper2Lib;

static void DisplayAsSvgImage(const std::string& caption, FillRule fillrule,
  const Paths64& subject, const Paths64& clip, const Paths64& solution)
{
  const std::string filename = caption + ".SVG";
  SvgWriter svg;
  SvgAddSubject(svg, subject, fillrule);
  SvgAddClip(svg, clip, fillrule);
  SvgAddSolution(svg, solution, fillrule, false);
  SvgSaveToFile(svg, filename, 400, 400, 10);
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}

int main()
{  
  // intersect a star and another modestly rotated star
  Paths64 subject, clip, solution;
  subject.push_back({ MakePath({200,100, 20,158, 130,4, 130,196, 20,42}) });
  clip.push_back({ MakePath({196,126, 8,136, 154,16, 104,200, 38,24}) });
  solution = Intersect(subject, clip, FillRule::NonZero);
  DisplayAsSvgImage("Intersect Paths", FillRule::NonZero, subject, clip, solution);
}

