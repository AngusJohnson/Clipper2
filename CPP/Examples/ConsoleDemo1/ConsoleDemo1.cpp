#include <cstdlib>
#include <iostream>
#include <algorithm> 
#include <string>
#include <conio.h>
#include "windows.h"

#include "../../Clipper2Lib/clipper.h"
#include "../../Clipper2Lib/clipper.core.h"

#include "../clipper.svg.h"
#include "../clipper.svg.utils.h"

#include "../../Utils/TextFileLoad.h"
#include "../../Utils/TextFileSave.h"

using namespace std;
using namespace Clipper2Lib;

const int display_width = 800, display_height = 600;
enum class TestType { All, Simple, TestFile, Benchmark, MemoryLeak, ErrorFile};

//------------------------------------------------------------------------------
// Windows Timer
//------------------------------------------------------------------------------

struct WinTimer {
private:
  _LARGE_INTEGER qpf = { 0,0 }, qpc1 = { 0,0 }, qpc2 = { 0,0 };
  string _time_text = "";
  
  void init()
  {
    QueryPerformanceFrequency(&qpf);
    QueryPerformanceCounter(&qpc1);
  }

public:
  explicit WinTimer() { init(); }
  
  explicit WinTimer(const string& caption, const string& time_text = "") :
     _time_text(time_text)
  {
    init();
    if (caption != "") std::cout << caption << endl;
  }

  ~WinTimer()
  {
    QueryPerformanceCounter(&qpc2);
    std::cout << _time_text <<
      static_cast<int64_t>((qpc2.QuadPart - qpc1.QuadPart) * 1000 / qpf.QuadPart) <<
      " msecs" << endl;
  }
};

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

inline Path64 MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  Path64 result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Point64(rand() % width, rand() % height));
  return result;
}
//---------------------------------------------------------------------------

inline Path64 MakeRectangle(int boxWidth, int boxHeight)
{
  Path64 result;
  result.reserve(4);
  result.push_back(Point64(0, 0));
  result.push_back(Point64(boxWidth, 0));
  result.push_back(Point64(boxWidth, boxHeight));
  result.push_back(Point64(0, boxHeight));
  return result;
}
//---------------------------------------------------------------------------

Path64 RandomOffset(const Path64& path, int maxWidth, int maxHeight)
{
  int dx = rand() % (maxWidth);
  int dy = rand() % (maxHeight);
  Path64 result;
  result.reserve(path.size());
  for (Point64 pt : path)
    result.push_back(Point64(pt.x + dx, pt.y +dy));
  return result;
}
//---------------------------------------------------------------------------

void DoErrorFile(const string& filename)
{
  Paths64 subject, clip, ignored, solution;
  ClipType ct;
  FillRule fr;
  int64_t area, count;

  ifstream ifs(filename);
  if (!ifs) return;
  LoadTest(ifs, subject, ignored, clip, area, count, ct, fr);
  ifs.close();

  solution = BooleanOp(ct, fr, subject, clip);
  SvgWriter svg;
  SvgAddSubject(svg, subject);
  SvgAddClip(svg, clip);
  SvgAddSolution(svg, solution, false);
  SvgSaveToFile(svg, "error.svg", fr, display_width, display_height, 20);
  system("error.svg");

  return;
}
//------------------------------------------------------------------------------

void DoSimpleTest(bool show_solution_coords = false)
{
  Paths64 subject, clip, ignored, solution;
  ClipType ct = ClipType::Intersection;;
  FillRule fr = FillRule::NonZero;

  //Minkowski
  Path64 path = MakePath("0, 0, 100, 200, 200, 0 ");
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
//------------------------------------------------------------------------------

void DoTestsFromFile(const string& filename, 
  const int start_num, const int end_num, bool svg_draw, bool show_solution_coords = false)
{
  ifstream ifs(filename);
  if (!ifs) return;

  svg_draw = svg_draw && (end_num - start_num <= 50);
  cout << endl << "Processing stored tests (from " << start_num <<
    " to " << end_num << ")" << endl << endl;

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
        cout << "  Test " << i << " counts differ: Saved val= " <<
        count << "; New val=" << count2 << endl;
      int64_t area_diff = std::abs(area2 - area);
      if (area && (area_diff > 2) && (area_diff/static_cast<double>(area)) > 0.02)
        cout << "  Test " << i << " areas differ: Saved val= " <<
        area << "; New val=" << area2 << endl;
      if (svg_draw)
      {
        string filename2 = "test_" + to_string(i) + ".svg";

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
  cout << endl;
}
//------------------------------------------------------------------------------

void DoBenchmark1(int edge_cnt_start, int edge_cnt_end, int increment = 1000)
{
  ClipType ct_benchmark = ClipType::Intersection;
  FillRule fr_benchmark = FillRule::NonZero;//EvenOdd;//

  Paths64 subject, clip, solution;

  cout << endl << "Complex Polygons Benchmark:  " << endl;
  for (int i = edge_cnt_start; i <= edge_cnt_end; i += increment)
  {
    subject.clear();
    clip.clear();
    subject.push_back(MakeRandomPoly(800, 600, i));
    clip.push_back(MakeRandomPoly(800, 600, i));
    //SaveToFile("benchmark_test.txt", subject, clip, ct_benchmark, fr_benchmark);

    cout << "Edge Count: " << i << " = ";
    {
      WinTimer t("");
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
//------------------------------------------------------------------------------

void DoBenchmark2(int edge_cnt_start, int edge_cnt_end, int increment = 1000)
{
  FillRule fr_benchmark = FillRule::EvenOdd;
  Paths64 subject, solution;

  cout << endl << "Starting Rectangles Benchmark:  " << endl;
  const int test_width = 7680, test_height = 4320;
  for (int i = edge_cnt_start; i <= edge_cnt_end; i += increment)
  {
    subject.clear();
    subject.reserve(i);
    solution.clear();
    Path64 rectangle = MakeRectangle(100, 100);
    for (int j = 0; j < i; ++j)
      subject.push_back(RandomOffset(rectangle, test_width - 10, test_height - 10));
    //SaveToFile("benchmark_test2.txt", subject, clip, ClipType::Union, fr_benchmark);    
    cout << "Rectangle count: " << i << " = ";
    {
      WinTimer t;
      solution = Union(subject, fr_benchmark);
      if (solution.empty()) break;
    }
  }
  //nb: chrome struggles with very large SVG files (>50MB) 
  SvgWriter svg;
  SvgAddSolution(svg, solution, false);
  SvgSaveToFile(svg, "solution4.svg",
    fr_benchmark, display_width, display_height, 20);
  system("solution4.svg");
}
//------------------------------------------------------------------------------

void DoMemoryLeakTest()
{
  int edge_cnt = 1000;

  Paths64 subject, clip, solution, empty_path;
  subject.push_back(MakeRandomPoly(800, 600, edge_cnt));
  clip.push_back(MakeRandomPoly(800, 600, edge_cnt));

  _CrtMemState sOld, sNew, sDiff;
  _CrtMemCheckpoint(&sOld); //take a snapshot

  solution = Intersect(subject, clip, FillRule::NonZero);
  //display the intersection
  SvgWriter svg;
  SvgAddSolution(svg, solution, false);
  SvgSaveToFile(svg, "solution.svg",
    FillRule::NonZero, display_width, display_height, 20);
  system("solution.svg");

  //clean up ready for memory check 
  solution.clear();
  solution.shrink_to_fit();

  _CrtMemCheckpoint(&sNew); //take another snapshot 
  if (_CrtMemDifference(&sDiff, &sOld, &sNew)) // is there is a difference
  {
    cout << endl << "Memory leaks! (See debugger output.)" << endl;
    OutputDebugString(L"-----------_CrtMemDumpStatistics ---------\r\n");
    _CrtMemDumpStatistics(&sDiff);
  }
  else
  {
    cout << endl << "No memory leaks detected :)" << endl << endl;
  }
}

//------------------------------------------------------------------------------
// Main entry point ...
//------------------------------------------------------------------------------

int main(int argc, char* argv[])
{
  //////////////////////////////////////////////////////////////////////////
  TestType test_type = TestType::Simple;//TestFile;//All;//Benchmark;//MemoryLeak;//ErrorFile;//
  //////////////////////////////////////////////////////////////////////////

  srand((unsigned)time(0));

  switch (test_type)
  {
  case TestType::ErrorFile:
    DoErrorFile("./error.txt");
    break;

  case TestType::All:
  case TestType::Simple:
    DoSimpleTest();
    if (test_type == TestType::Simple) break;

  case TestType::TestFile:
    DoTestsFromFile("../../../Tests/tests.txt", 1, 200, false);
    //or test just one of the samples in tests.txt 
    //DoTestsFromFile("../../Tests/tests.txt", 16, 16, true, true);
    if (test_type == TestType::TestFile) break;

  case TestType::MemoryLeak:
    DoMemoryLeakTest();
    if (test_type == TestType::MemoryLeak) break;

  case TestType::Benchmark:
    cout << endl  << "==========" << endl;
    cout          << "Benchmarks" << endl;
    cout          << "==========" << endl;
    DoBenchmark1(1000, 5000);
    DoBenchmark2(10000, 30000, 10000);
    break;
  }

  cout << endl;
#ifdef _DEBUG
  cout << "Press any key to continue" << endl;
  const char c = _getch();
#endif
  return 0;
}
//---------------------------------------------------------------------------
