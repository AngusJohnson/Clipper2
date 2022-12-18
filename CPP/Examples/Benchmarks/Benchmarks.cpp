#include <cstdlib>
#include <string>
#include <chrono> 
 
#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/ClipFileLoad.h"
#include "../../Utils/ClipFileSave.h"
#include "../../Utils/Timer.h"

using namespace Clipper2Lib;

const int display_width = 800, display_height = 600;

void DoBenchmark(int edge_cnt_start, int edge_cnt_end, int increment);
Path64 MakeRandomPoly(int width, int height, unsigned vertCnt);
void System(const std::string &filename);

int main()
{  
  srand((unsigned)time(0));
  DoBenchmark(1000, 5000, 1000);

  std::cout << std::endl;
//#ifdef _DEBUG
  std::string s;
  std::cout << "Press Enter to continue" << std::endl;
  std::getline(std::cin, s);
//#endif
  return 0;
}

inline Path64 MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  Path64 result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Point64(rand() % width, rand() % height));
  return result;
}

void DoBenchmark(int edge_cnt_start, int edge_cnt_end, int increment)
{
  ClipType ct = ClipType::Intersection;
  FillRule fr = FillRule::NonZero;//EvenOdd;//Positive;//

  Paths64 subject, clip, solution;
  std::cout << std::endl << "Complex Polygons Benchmark:  " << std::endl;
  for (int i = edge_cnt_start; i <= edge_cnt_end; i += increment)
  {
    subject.clear();
    clip.clear();
    subject.push_back(MakeRandomPoly(800, 600, i));
    clip.push_back(MakeRandomPoly(800, 600, i));
    //SaveToFile("benchmark_test.txt", subject, clip, ct_benchmark, fr_benchmark);

    std::cout << "Edge Count: " << i << " = ";
    {
      Timer t;
      solution = BooleanOp(ct, fr, subject, clip);
      if (solution.empty()) break;
    }
  }

  SvgWriter svg;
  SvgAddSubject(svg, subject, fr);
  SvgAddClip(svg, clip, fr);
  SvgAddSolution(svg, solution, fr, false);
  SvgSaveToFile(svg, "solution3.svg", display_width, display_height, 20);
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
