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
enum class TestType { All, Simple, SavedTests, Benchmark, MemoryLeak};

Path64 MakeRandomPoly(int width, int height, unsigned vertCnt);
void DoSimpleTest(bool show_solution_coords = false);
void RunSavedTests(const std::string& filename,
  const int start_num, const int end_num,
  bool svg_draw, bool show_solution_coords);
void DoBenchmark(int edge_cnt_start, int edge_cnt_end, int increment);
void DoMemoryLeakTest();

int main()
{
  std::cout.imbue(std::locale(""));
  srand((unsigned)time(0));

  //////////////////////////////////////////////////////////////////////////
  //test_type options:  Simple; Benchmark; All; MemoryLeak; SavedTests;
  TestType test_type = TestType::Simple;
  //////////////////////////////////////////////////////////////////////////

  switch (test_type)
  {
  case TestType::All:
  case TestType::Simple:
    DoSimpleTest();
    if (test_type == TestType::Simple) break;

  case TestType::SavedTests:
    RunSavedTests("../../../Tests/tests.txt", 1, 0xFFFF, false, false);
    //or test just one of the samples in tests.txt 
    //DoTestsFromFile("../../Tests/tests.txt", 16, 16, true, true);
    if (test_type == TestType::SavedTests) break;

  case TestType::Benchmark:
    std::cout << std::endl << "==========" << std::endl;
    std::cout << "Benchmarks" << std::endl;
    std::cout << "==========" << std::endl;
    DoBenchmark(1000, 3000, 1000);
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
  //Path64 pattern = Ellipse<int64_t>(Point64(), 25, 25);
  //or alternatively create a diagonal brush shape
  Path64 pattern = MakePath("-13,20,  -11,26, 13,-20  11,-26");

  //design "C2" as the drawing path
  //first design "C" 
  Path64 path = Ellipse<int64_t>(Point64(240,225), 200, 200);
  path.erase(path.begin(), path.begin() + 4);
  path.erase(path.end() - 4, path.end());
  //use MinkowskiSum to 'draw' the "C" using the pattern
  solution = MinkowskiSum(pattern, path, false);
  SvgAddSolution(svg, solution, false);
  //and design "2"
  path = Ellipse<int64_t>(Point64(240, 180), 75, 65);
  std::reverse(path.begin(), path.end());
  std::rotate(path.begin(), path.begin() + 6, path.end());
  path.erase(path.begin(), path.begin() +9);
  path.push_back(Point64(190, 249));
  path.push_back(Point64(190, 325));
  path.push_back(Point64(315, 325));
  //use MinkowskiSum to 'draw' the "2" using the pattern
  solution = MinkowskiSum(pattern, path, false);
  //save and display
  SvgAddSolution(svg, solution, false);
  SvgSaveToFile(svg, "solution1.svg", fr, 450, 450, 10);
  system("solution1.svg");

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
  SvgAddSubject(svg2, subject);
  SvgAddClip(svg2, clip);
  SvgAddSolution(svg2, solution, false);
  SvgSaveToFile(svg2, "solution2.svg", fr, 450, 450, 10);
  system("solution2.svg");
}

void RunSavedTests(const std::string& filename,
  const int start_num, const int end_num, bool svg_draw, bool show_solution_coords)
{
  std::ifstream ifs(filename);
  if (!ifs) return;

  svg_draw = svg_draw && (end_num - start_num <= 50);
  std::cout << std::endl << "Running stored tests (from " << start_num <<
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
        std::cout << "  Test " << i << " path counts differ: Saved val= " <<
        count << "; New val=" << count2 << std::endl;
      int64_t area_diff = std::abs(area2 - area);
      if (area && (area_diff > 2) && (area_diff/static_cast<double>(area)) > 0.02)
        std::cout << "  Test " << i << " path areas differ: Saved val= " <<
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
