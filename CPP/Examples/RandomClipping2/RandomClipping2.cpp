#include <cstdlib>
#include <string>
#include <chrono> 

#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/ClipFileLoad.h"
#include "../../Utils/ClipFileSave.h"
#include "../../Utils/Timer.h"

using namespace Clipper2Lib;

void DoDiamonds();
void DoSquares();
void DoCircles();
void System(const std::string &filename);

int main()
{
  std::cout.imbue(std::locale(""));
  srand((unsigned)time(0));

  std::cout << std::endl << "This shouldn't take long ... " << std::endl;
  DoCircles();
  std::cout << std::endl << "And just a few more seconds ... " << std::endl;
  DoDiamonds();
  std::cout << std::endl << "And just a few more seconds ... " << std::endl;
  DoSquares();
#ifdef _DEBUG
  std::string s;
  std::cout  << std::endl << "All done. Press Enter to continue" << std::endl;
  std::getline(std::cin, s);
#endif
  return 0;
}


void DoDiamonds()
{
  static const int size = 20, size2 = size * 2;;
  Path64 shape;
  shape.push_back(Point64(size, 0));
  shape.push_back(Point64(size2, size));
  shape.push_back(Point64(size, size2));
  shape.push_back(Point64(0, size));

  Paths64 subjects, solution;
  int w = 900, h = 600;
  for (int i = 0; i < h / size; i += 2)
  {
    for (int j = 0; j < w / size; ++j)
    {
      // move right size units & up or down size units ...
      shape = TranslatePath(shape, size, (j & 1) == 0 ? size : -size);
      // skip numerous diamond shapes ...
      if (rand() % 4 != 1) subjects.push_back(shape);
    }
    shape = TranslatePath(shape, static_cast<int64_t>(-w / size) * size, size);
  }
  solution = Union(subjects, FillRule::NonZero);
  // tidy up the solution
  //solution = Union(solution, FillRule::NonZero);

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "solution1.svg", 900, 600, 10);
  System("solution1.svg");
}

void DoSquares()
{
  static const int size = 20;
  Path64 shape;
  shape.push_back(Point64(0, 0));
  shape.push_back(Point64(size, 0));
  shape.push_back(Point64(size, size));
  shape.push_back(Point64(0, size));

  Paths64 subjects, solution;
  int w = 900, h = 600;
  for (int i = 0; i < h / size; ++i)
  {
    for (int j = 0; j < w / size; ++j)
    {
      shape = TranslatePath(shape, size, 0);
      // skip numerous squares ...
      if (rand() % 4 != 1) subjects.push_back(shape);
    }
    shape = TranslatePath(shape, static_cast<int64_t>(-w / size) * size, size);
  }
  solution = Union(subjects, FillRule::NonZero);
  // tidy up the solution
  //solution = Union(solution, FillRule::NonZero);

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "solution2.svg", 900, 600, 10);
  System("solution2.svg");
}

void DoCircles()
{
  // create a small pseudo circle that has 11 edges
  PathD shape = Ellipse(RectD(0, 0, 50, 50), 31);

  PathsD subjects, solution;
  int w = 900, h = 600;

  for (int j = 0; j < 350; ++j)
  {
    // add a copy of shape that's been moved right and down a random amount
    subjects.push_back(TranslatePath(shape,
      rand() % (w - 50), rand() % (h - 50)));
  }
  solution = Union(subjects, FillRule::NonZero);

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "solution3.svg", 900, 600, 10);
  System("solution3.svg");
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
