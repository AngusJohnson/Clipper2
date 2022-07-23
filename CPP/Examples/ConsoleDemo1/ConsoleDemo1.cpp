#include <cstdlib>
#include <string>
#include <chrono> 

#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/ClipFileLoad.h"
#include "../../Utils/ClipFileSave.h"
#include "../../Utils/Timer.h"

using namespace Clipper2Lib;

const int display_width = 800, display_height = 600;
enum class TestType { All, Simple, Benchmark, MemoryLeak};

Path64 MakeRandomPoly(int width, int height, unsigned vertCnt);
void DoSimpleTest(bool show_solution_coords = false);
void DoBenchmark(int edge_cnt_start, int edge_cnt_end, int increment);
void DoMemoryLeakTest();
void System(const std::string &filename);

int main()
{
  std::cout.imbue(std::locale(""));
  srand((unsigned)time(0));

  //////////////////////////////////////////////////////////////////////////
  //test_type options:  Simple; Benchmark; All; MemoryLeak; 
  TestType test_type = TestType::All;
  //////////////////////////////////////////////////////////////////////////

  switch (test_type)
  {
  case TestType::All:

  case TestType::Simple:
    DoSimpleTest();
    if (test_type == TestType::Simple) break;

  case TestType::Benchmark:
    std::cout << std::endl << "==========" << std::endl;
    std::cout << "Benchmarks" << std::endl;
    std::cout << "==========" << std::endl;
    DoBenchmark(1000, 5000, 1000);
    if (test_type == TestType::Benchmark) break;

  case TestType::MemoryLeak:
    DoMemoryLeakTest();
    break;

  }

  std::cout << std::endl;
#ifdef _DEBUG
  std::string s;
  std::cout << "Press Enter to continue" << std::endl;
  std::getline(std::cin, s);
#endif
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

inline Path64 MakeStar(const Point64& center, int radius, int points)
{
  if (!(points % 2)) --points;
  if (points < 5) points = 5;
  Path64 tmp = Ellipse<int64_t>(center, radius, radius, points);
  Path64 result; 
  result.reserve(points);
  result.push_back(tmp[0]);
  for (int i = points -1, j = i / 2; j;)
  {
    result.push_back(tmp[j--]);
    result.push_back(tmp[i--]);
  }
  return result;
}

void DoSimpleTest(bool show_solution_coords)
{
  Paths64 tmp, solution;
  FillRule fr = FillRule::NonZero;

  //SVG IMAGE #1
  //Use Minkowski to draw a stylised "C2"
  
  SvgWriter svg;
  //create a circular shape to use as the 'paint brush' shape
  Path64 pattern = Ellipse<int64_t>(Point64(), 25, 25);
  //or alternatively create a diagonal brush shape
  //Path64 pattern = MakePath("-13,20,  -11,26, 13,-20  11,-26");

  //design "C2" as the drawing path
  //first design "C" 
  Path64 path = Ellipse<int64_t>(Point64(240,225), 200, 200);
  path.erase(path.begin(), path.begin() + 4);
  path.erase(path.end() - 4, path.end());
  //use MinkowskiSum to 'draw' the "C" using the pattern
  solution = MinkowskiSum(pattern, path, false);
  SvgAddSolution(svg, solution, fr, false);
  //and design "2"
  path = Ellipse<int64_t>(Point64(240, 180), 75, 65);
  std::rotate(path.begin(), path.begin() + 6, path.end());
  path.erase(path.begin(), path.begin() +9);
  path.push_back(Point64(190, 249));
  path.push_back(Point64(190, 325));
  path.push_back(Point64(315, 325));
  //use MinkowskiSum to 'draw' the "2" using the pattern
  solution = MinkowskiSum(pattern, path, false);
  //save and display
  SvgAddSolution(svg, solution, fr, false);
  SvgSaveToFile(svg, "solution1.svg", 450, 450, 10);
  System("solution1.svg");

  //SVG IMAGE #2
  //intersect a 9 point star with a circle

  Paths64 subject, clip;
  subject.push_back(MakeStar(Point64(225, 225), 220, 9));
  clip.push_back(Ellipse<int64_t>(Point64(225,225), 150, 150));  
  //SaveTest("debug.txt", false, &subject, NULL, &clip, 0, 0, ClipType::Intersection, fr);
  
  //Intersect both shapes and then 'inflate' result -10 (ie deflate)
  solution = Intersect(subject, clip, fr);
  solution = InflatePaths(solution, -10, JoinType::Round, EndType::Polygon);

  SvgWriter svg2;
  SvgAddSubject(svg2, subject, fr);
  SvgAddClip(svg2, clip, fr);
  SvgAddSolution(svg2, solution, fr, false);
  SvgSaveToFile(svg2, "solution2.svg", 450, 450, 10);
  System("solution2.svg");
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

void DoMemoryLeakTest()
{
#ifdef _WIN32
  int edge_cnt = 1000;

  Paths64 subject, clip;
  subject.push_back(MakeRandomPoly(800, 600, edge_cnt));
  clip.push_back(MakeRandomPoly(800, 600, edge_cnt));

  _CrtMemState sOld {}, sNew {}, sDiff {};
  _CrtMemCheckpoint(&sOld); //take a snapshot
  {
    Paths64 solution = Intersect(subject, clip, FillRule::NonZero);
  }
  _CrtMemCheckpoint(&sNew); //take another snapshot (outside code block)
  if (_CrtMemDifference(&sDiff, &sOld, &sNew)) // check for a difference
  {
    std::cout << std::endl << "Memory leaks!" << std::endl;
    //_CrtMemDumpStatistics(&sDiff);
  }
  else
  {
    std::cout << std::endl << "No memory leaks detected :)" << std::endl << std::endl;
  }
#endif
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
