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

void RecheckLastBenchmark(bool use_polytree);
void DoBenchmark(int edge_cnt_start, int edge_cnt_end, 
  int increment, bool test_polytree = false);
Path64 MakeRandomPoly(int width, int height, unsigned vertCnt);
void System(const std::string &filename);

int main()
{  
  bool test_polytree = false;//true;// 
  srand((unsigned)time(0));
  DoBenchmark(1000, 7000, 1000, test_polytree);
  //RecheckLastBenchmark(test_polytree);

  std::cout << std::endl;
//#ifdef _DEBUG
  std::string s;
  std::cout << "Press Enter to continue" << std::endl;
  std::getline(std::cin, s);
//#endif
  return 0;
}

void RecheckLastBenchmark(bool use_polytree)
{
  ClipType ct;
  FillRule fr;

  Paths64 subject, subj_open, clip, solution;
  int64_t area, count;
  PolyTree64 polytree;

  std::ifstream test("benchmark_test.txt");
  if (!test.good())
  {
    std::cout << "Unable to find or open test file." << std::endl;
    return;
  }

  bool success = false;
  std::cout << "Checking last benchmark ..." << std::endl;
  LoadTestNum(test, 1, subject, subj_open, clip, area, count, ct, fr);
  if (use_polytree)
  {
    BooleanOp(ct, fr, subject, clip, polytree);
    success = polytree.Count();
  }
  else
  {
    solution = BooleanOp(ct, fr, subject, clip);
    success = solution.size();
  }
  if (success)
    std::cout << "It's OK." << std::endl;
  else
    std::cout << "It failed (again)." << std::endl;
}

inline Path64 MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  Path64 result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Point64(rand() % width, rand() % height));
  return result;
}

void DoBenchmark(int edge_cnt_start, int edge_cnt_end, 
  int increment, bool test_polytree)
{
  ClipType ct = ClipType::Intersection;
  FillRule fr = FillRule::NonZero;//EvenOdd;//Positive;//

  Paths64 subject, clip, solution;
  PolyTree64 polytree;
  std::cout << std::endl << "Complex Polygons Benchmark:  " << std::endl;
  for (int i = edge_cnt_start; i <= edge_cnt_end; i += increment)
  {
    subject.clear();
    clip.clear();
    subject.push_back(MakeRandomPoly(800, 600, i));
    clip.push_back(MakeRandomPoly(800, 600, i));
    SaveTest( "benchmark_test.txt", false, &subject, nullptr, &clip, 0, 0, ct, fr);

    std::cout << "Edge Count: " << i << " = ";
    {
      if (test_polytree)
      {
        polytree.Clear();
        Timer t;
        BooleanOp(ct, fr, subject, clip, polytree);
        if (!polytree.Count()) break;
      }
      else
      {
        Timer t;
        solution = BooleanOp(ct, fr, subject, clip);
        if (solution.empty()) break;
      }
    }
  }

  if (test_polytree) solution = PolyTreeToPaths64(polytree);
    
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
