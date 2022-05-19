#include <cstdlib>
#include <string>
#include <chrono> 

#include "../../Clipper2Lib/clipper.h"
#include "../clipper.svg.utils.h"
#include "../../Utils/ClipFileLoad.h"
#include "../../Utils/Timer.h"

using namespace Clipper2Lib;

const int display_width = 800, display_height = 600;
enum class TestType { All, Simple, TestFile, Benchmark, MemoryLeak};

inline Path64 MakeRandomPoly(int width, int height, unsigned vertCnt);
void DoSimpleTest(bool show_solution_coords = false);
void DoTestsFromFile(const std::string& filename,
  const int start_num, const int end_num,
  bool svg_draw, bool show_solution_coords = false);
void DoBenchmark(int edge_cnt_start, int edge_cnt_end, int increment = 1000);
void DoMemoryLeakTest();

int main(int argc, char* argv[])
{
  std::cout.imbue(std::locale(""));

  //////////////////////////////////////////////////////////////////////////
  TestType test_type = TestType::Simple;//Benchmark;//All;//MemoryLeak;//TestFile;//
  //////////////////////////////////////////////////////////////////////////

  srand((unsigned)time(0));

  switch (test_type)
  {
  case TestType::All:
  case TestType::Simple:
    DoSimpleTest();
    if (test_type == TestType::Simple) break;

  case TestType::TestFile:
    DoTestsFromFile("../../../Tests/tests.txt", 1, 200, false);
    //or test just one of the samples in tests.txt 
    //DoTestsFromFile("../../Tests/tests.txt", 16, 16, true, true);
    if (test_type == TestType::TestFile) break;

  case TestType::Benchmark:
    std::cout << std::endl << "==========" << std::endl;
    std::cout << "Benchmarks" << std::endl;
    std::cout << "==========" << std::endl;
    DoBenchmark(1000, 3000);
    if (test_type == TestType::Benchmark) break;

  case TestType::MemoryLeak:
    DoMemoryLeakTest();
    break;

  }

  std::cout << std::endl;
#ifdef _DEBUG
  std::cout << "Press any key to continue" << std::endl;
  const char c = _getch();
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

void DoSimpleTest(bool show_solution_coords)
{
  Paths64 subject, clip, ignored, solution;
  FillRule fr = FillRule::NonZero;

  //Minkowski
  Path64 path = MakePath("0,0, 200,0, 200 100, 0,100, 0,200, 200,200 ");
  Path64 pattern = Ellipse<int64_t>(Point64(), 20, 20);
  solution = Paths64(MinkowskiSum(pattern, path, false));

  SvgWriter svg;
  SvgAddSolution(svg, solution, false);
  SvgSaveToFile(svg, "solution1.svg", fr, display_width, display_height, 20);
  system("solution1.svg");
  solution.clear();

  //Intersection plus inflating
  subject.push_back(MakePath("500, 250, 50, 395, 325, 10, 325, 490, 50, 105"));
  clip.push_back(Ellipse<int64_t>(Point64(250,250), 150, 150));
  //SaveToFile("simple.txt", subject, clip, ct, fr);
  solution = Intersect(subject, clip, fr);
  solution = InflatePaths(solution, -10, JoinType::Round, EndType::Polygon);

  SvgWriter svg2;
  SvgAddSubject(svg2, subject);
  SvgAddClip(svg2, clip);
  SvgAddSolution(svg2, solution, false);
  SvgSaveToFile(svg2, "solution2.svg", fr, display_width, display_height, 20);
  system("solution2.svg");
}

void DoTestsFromFile(const std::string& filename,
  const int start_num, const int end_num, bool svg_draw, bool show_solution_coords)
{
  std::ifstream ifs(filename);
  if (!ifs) return;

  svg_draw = svg_draw && (end_num - start_num <= 50);
  std::cout << std::endl << "Processing stored tests (from " << start_num <<
    " to " << end_num << ")" << std::endl << std::endl;

  for (int i = start_num; i <= end_num; ++i)
  {
    Paths64 subject, subject_open, clip;
    Paths64 solution, solution_open;
    ClipType ct;
    FillRule fr;
    int64_t area, count;

    if (LoadTestNum(ifs, i, false, subject, subject_open, clip, 
      area, count, ct, fr)) 
    {
      Clipper64 c;
      c.AddSubject(subject);
      c.AddOpenSubject(subject_open);
      c.AddClip(clip);
      c.Execute(ct, fr, solution, solution_open);
      int64_t area2 = static_cast<int64_t>(Area(solution));
      int64_t count2 = solution.size();
      int64_t count_diff = abs(count2 - count);
      if (count && count_diff > 2 && count_diff/ static_cast<double>(count) > 0.02)
        std::cout << "  Test " << i << " counts differ: Saved val= " <<
        count << "; New val=" << count2 << std::endl;
      int64_t area_diff = std::abs(area2 - area);
      if (area && (area_diff > 2) && (area_diff/static_cast<double>(area)) > 0.02)
        std::cout << "  Test " << i << " areas differ: Saved val= " <<
        area << "; New val=" << area2 << std::endl;
      if (svg_draw)
      {
        std::string filename2 = "test_" + std::to_string(i) + ".svg";

        SvgWriter svg;
        SvgAddSubject(svg, subject);
        SvgAddSubject(svg, subject_open, false);
        SvgAddClip(svg, clip);
        SvgAddSolution(svg, solution, show_solution_coords);
        SvgAddSolution(svg, solution_open, show_solution_coords, false);
        SvgSaveToFile(svg, filename2, fr, display_width, display_height, 20);
        system(filename2.c_str());
      }
    }
    else break;
  }
  std::cout << std::endl;
}

void DoBenchmark(int edge_cnt_start, int edge_cnt_end, int increment)
{
  ClipType ct_benchmark = ClipType::Intersection;
  FillRule fr_benchmark = FillRule::NonZero;//EvenOdd;//

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
      Timer t("");
      solution = BooleanOp(ct_benchmark, fr_benchmark, subject, clip);
      if (solution.empty()) break;
    }
  }
  
  SvgWriter svg;
  SvgAddSubject(svg, subject);
  SvgAddClip(svg, clip);
  SvgAddSolution(svg, solution, false);
  SvgSaveToFile(svg, "solution3.svg", 
    fr_benchmark, display_width, display_height, 20);
  system("solution3.svg");
}

void DoMemoryLeakTest()
{
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
}
