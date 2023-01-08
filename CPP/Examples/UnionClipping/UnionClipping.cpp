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
void DoTriangles();

int main()
{
  std::cout.imbue(std::locale(""));
  srand((unsigned)time(0));

  DoDiamonds();
  DoSquares();
  DoCircles();
  DoTriangles();

#ifdef _DEBUG
  //std::string s;
  //std::cout  << std::endl << "All done. Press Enter to continue" << std::endl;
  //std::getline(std::cin, s);
#endif
  return 0;
}

void System(const std::string& filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}

void DoDiamonds()
{
  static const int size = 10, size2 = size * 2;;
  static const int w = 800, h = 600;
  Path64 shape;
  shape.push_back(Point64(size, 0));
  shape.push_back(Point64(size2, size));
  shape.push_back(Point64(size, size2));
  shape.push_back(Point64(0, size));

  Paths64 subjects, solution;
  
  for (int i = 0; i < h / size; i += 2)
  {
    for (int j = 0; j < w / size; ++j)
    {
      shape = TranslatePath(shape, size, (j & 1) == 0 ? size : -size);
      if (rand() % 7) subjects.push_back(shape);
    }
    shape = TranslatePath(shape, static_cast<int64_t>(-w / size) * size, size*2);
  }

  solution = Union(subjects, FillRule::NonZero);

  SvgWriter svg;
  SvgAddSubject(svg, subjects, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "solution1.svg", w, h, 10);
  System("solution1.svg");
}

void DoSquares()
{
  static const int w = 800, h = 600;
  static const int size = 10;
  Path64 shape;
  shape.push_back(Point64(0, 0));
  shape.push_back(Point64(size, 0));
  shape.push_back(Point64(size, size));
  shape.push_back(Point64(0, size));
  Paths64 subjects, solution;
  ClipType cliptype = ClipType::Union;
  FillRule fillrule = FillRule::NonZero;

  for (int i = 0; i < h / size; ++i)
  {
    for (int j = 0; j < w / size; ++j)
    {
      if (rand() % 4 != 1) subjects.push_back(shape);
      shape = TranslatePath(shape, size, 0);
    }
    shape = TranslatePath(shape, static_cast<int64_t>(-w / size) * size, size);
  }
  //SaveTest("squares.txt", false, &subjects, nullptr, nullptr, 0, 0, ClipType::Union, FillRule::NonZero);

  solution = Union(subjects, fillrule);
  
  SvgWriter svg;
  SvgAddSubject(svg, subjects, fillrule);
  SvgAddSolution(svg, solution, fillrule, false);
  SvgSaveToFile(svg, "solution2.svg", 800, 600, 10);
  System("solution2.svg");
}

void DoCircles()
{
  // create a small circle with 31 vertices
  PathD shape = Ellipse(RectD(0, 0, 35, 35), 31);

  PathsD subjects, solution;
  int w = 800, h = 600;

  for (int j = 0; j < 550; ++j)
  {
    subjects.push_back(TranslatePath(shape,
      rand() % (w - 50), rand() % (h - 50)));
  }

  solution = Union(subjects, FillRule::NonZero);

  SvgWriter svg;
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "solution3.svg", 800, 600, 10);
  System("solution3.svg");
}

void DoTriangles()
{
  static const int w = 800, h = 600;
  static const int size = 10;
  Path64 tri1;
  tri1.push_back(Point64(0, 0));
  tri1.push_back(Point64(size * 2, 0));
  tri1.push_back(Point64(size, size * 2));
  Path64 tri2;
  tri2.push_back(Point64(size * 2, 0));
  tri2.push_back(Point64(size, size * 2));
  tri2.push_back(Point64(size * 3, size * 2));

  Paths64 subjects, solution;
  ClipType cliptype = ClipType::Union;
  FillRule fillrule = FillRule::NonZero;

  for (int i = 0; i < h / size / 2; ++i)
  {
    for (int j = 0; j < w / size / 2; ++j)
    {
      if (rand() % 5 != 1) subjects.push_back(tri1);
      if (rand() % 5 != 1) subjects.push_back(tri2);
      tri1 = TranslatePath(tri1, size * 2, 0);
      tri2 = TranslatePath(tri2, size * 2, 0);
    }
    tri1 = TranslatePath(tri1, static_cast<int64_t>(-w / size) * size, size * 2);
    tri2 = TranslatePath(tri2, static_cast<int64_t>(-w / size) * size, size * 2);
  }

  solution = Union(subjects, fillrule);

  SvgWriter svg;
  SvgAddSubject(svg, subjects, fillrule);
  SvgAddSolution(svg, solution, fillrule, false);
  SvgSaveToFile(svg, "solution4.svg", 800, 600, 10);
  System("solution4.svg");
}
