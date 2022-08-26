#include <cstdlib>
#include <string>
#include <chrono> 

#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/ClipFileLoad.h"
#include "../../Utils/ClipFileSave.h"
#include "../../Utils/Timer.h"

using namespace Clipper2Lib;

// forward declarations
void DoDiamonds();
void DoCircles();
void System(const std::string &filename);

int main()
{
  std::cout.imbue(std::locale(""));
  srand((unsigned)time(0));

  std::cout << std::endl << "This should take less than a minute ... " << std::endl;
  DoDiamonds();
  std::cout << std::endl << "And now the next test ... " << std::endl;
  DoCircles();
#ifdef _DEBUG
  std::string s;
  std::cout  << std::endl << "All done. Press Enter to continue" << std::endl;
  std::getline(std::cin, s);
#endif
  return 0;
}


void DoDiamonds()
{
  Path64 shape = MakePath("5,0, 10,5, 5,10, 0,5");
  Paths64 subjects, solution;
  int w = 900, h = 600;
  for (int i = 0; i < h; i += 2)
  {
    for (int j = 0; j < w; ++j)
    {
      // move right 5 & up or down 5 ...
      shape = TranslatePath(shape, 5, (j & 1) == 0 ? 5 : -5);
      // skip random diamonds ...
      if (rand() % 4 != 1)
        subjects.push_back(shape);
    }
    shape = TranslatePath(shape, -w * 5, 10);
  }
  solution = Union(subjects, FillRule::NonZero);
  // tidy up the solution
  //solution = Union(solution, FillRule::NonZero);

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "solution1.svg", 900, 600, 10);
  System("solution1.svg");
}

void DoCircles()
{
  // create a small pseudo circle that has 11 edges
  PathD shape = Ellipse(RectD(0, 0, 10, 10), 11);

  PathsD subjects, solution;
  int w = 900, h = 600;

  for (int j = 0; j < 25000; ++j)
  {
    // add a copy of shape that's been moved right and down a random amount
    subjects.push_back(TranslatePath(shape,
      rand() % (w - 10), rand() % (h - 10)));
  }
  solution = Union(subjects, FillRule::NonZero);

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "solution2.svg", 900, 600, 10);
  System("solution2.svg");
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
