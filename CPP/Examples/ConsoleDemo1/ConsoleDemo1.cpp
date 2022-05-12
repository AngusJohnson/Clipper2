#include <cstdlib>
#include <iostream>
#include <algorithm> 
#include <string>
#include <conio.h>
#include "windows.h"

#include "../../Clipper2Lib/clipper.h"
#include "../clipper.svg.h"

#include "../../Utils/TextFileLoader.h"

using namespace std;
using namespace Clipper2Lib;

const int display_width = 800, display_height = 600;
enum class TestType { All, Simple, TestFile, Benchmark, MemoryLeak, ErrorFile};

//------------------------------------------------------------------------------
// Timer
//------------------------------------------------------------------------------

struct Timer {
private:
  _LARGE_INTEGER qpf = { 0,0 }, qpc1 = { 0,0 }, qpc2 = { 0,0 };
  string _time_text = "";
  
  void init()
  {
    QueryPerformanceFrequency(&qpf);
    QueryPerformanceCounter(&qpc1);
  }

public:
  explicit Timer() { init(); }
  
  explicit Timer(const string& caption, const string& time_text = "") :
     _time_text(time_text)
  {
    init();
    if (caption != "") std::cout << caption << endl;
  }

  ~Timer()
  {
    QueryPerformanceCounter(&qpc2);
    std::cout << _time_text <<
      static_cast<int64_t>((qpc2.QuadPart - qpc1.QuadPart) * 1000 / qpf.QuadPart) <<
      " msecs" << endl;
  }
};

//------------------------------------------------------------------------------
// Functions save clipping operations to text files
//------------------------------------------------------------------------------

void PathsToOstream(Paths64& paths, std::ostream &stream)
{
  for (Paths64::iterator paths_it = paths.begin(); paths_it != paths.end(); ++paths_it)
  {
    //watch out for empty paths
    if (paths_it->begin() == paths_it->end()) continue;
    Path64::iterator path_it, path_it_last;
    for (path_it = paths_it->begin(), path_it_last = --paths_it->end(); 
      path_it != path_it_last; ++path_it)
        stream << *path_it << ", ";
    stream << *path_it_last << endl;
  }
}
//------------------------------------------------------------------------------

void SaveToFile(const string &filename, 
  Paths64 &subj, Paths64 &clip, 
  ClipType ct, FillRule fr)
{
  string cliptype;
  string fillrule;

  switch (ct) 
  {
  case ClipType::None: cliptype = "NONE"; break;
  case ClipType::Intersection: cliptype = "INTERSECTION"; break;
  case ClipType::Union: cliptype = "UNION"; break;
  case ClipType::Difference: cliptype = "DIFFERENCE"; break;
  case ClipType::Xor: cliptype = "XOR"; break;
  }

  switch (fr) 
  {
  case FillRule::EvenOdd: fillrule = "EVENODD"; break;
  case FillRule::NonZero : fillrule = "NONZERO"; break;
  }

  std::ofstream file;
  file.open(filename);
  file << "CAPTION: " << cliptype << " " << fillrule << endl;
  file << "CLIPTYPE: " << cliptype << endl;
  file << "FILLRULE: " << fillrule << endl;
  file << "SUBJECTS" << endl;
  PathsToOstream(subj, file);
  file << "CLIPS" << endl;
  PathsToOstream(clip, file);
  file.close();
}

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

Path64 Ellipse(const Rect64& rec)
{
  if (rec.IsEmpty()) return Path64();
  Point64 centre = Point64((rec.right + rec.left) / 2, (rec.bottom + rec.top) / 2);
  Point64 radii = Point64(rec.Width() /2, rec.Height() /2);
  int steps = static_cast<int>(PI * sqrt((radii.x + radii.y)/2));
  double si = std::sin(2 * PI / steps);
  double co = std::cos(2 * PI / steps);
  double dx = co, dy = si;
  Path64 result;
  result.reserve(steps);
  result.push_back(Point64(centre.x + radii.x, centre.y));
  for (int i = 1; i < steps; ++i)
  {
    result.push_back(Point64(centre.x + static_cast<int64_t>(radii.x * dx), 
      centre.y + static_cast<int64_t>(radii.y * dy)));
    double x = dx * co - dy * si;
    dy = dy * co + dx * si;
    dx = x;
  }
  return result;
}
//---------------------------------------------------------------------------

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

inline Path64 MakeEllipse(int boxWidth, int boxHeight)
{
  return Ellipse(Rect64(0, 0, boxWidth, boxHeight));
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

inline void SaveToSVG(const string &filename, int max_width, int max_height,
  const Paths64* subj, const Paths64* subj_open, const Paths64* clip, 
  const Paths64* solution, const Paths64* solution_open,
  FillRule fill_rule, bool show_coords = false)
{
  SvgWriter svg;
  svg.fill_rule = fill_rule;
  svg.SetCoordsStyle("Verdana", 0xFF0000AA, 9);
  //svg.AddCaption("Clipper demo ...", 0xFF000000, 14, 20, 20);

  if (subj)
    svg.AddPaths(*subj, false, 0x1800009C, 0xCCB3B3DA, 0.8, show_coords);
  if (subj_open)
    svg.AddPaths(*subj_open, true, 0x0, 0xFFD3D3DA, 1.0, show_coords);
  if (clip)
    svg.AddPaths(*clip, false, 0x129C0000, 0xCCFFA07A, 0.8, show_coords);
  if (solution)
  {
    svg.AddPaths(*solution, false, 0xFF80ff9C, 0xFF003300, 0.8, show_coords);
    //for (Paths::const_iterator i = solution.cbegin(); i != solution.cend(); ++i)
    //  if (Area(*i) < 0) svg.AddPath((*i), false, 0x0, 0xFFFF0000, 0.8, show_coords);
  }
  if (solution_open)
    svg.AddPaths(*solution_open, true, 0x0, 0xFF000000, 1.0, show_coords);
  svg.SaveToFile(filename, max_width, max_height, 80);
}
//------------------------------------------------------------------------------

void DoErrorFile(const string& filename)
{
  Paths64 subject, clip, ignored, solution;
  ClipType ct;
  FillRule fr;
  int64_t area, count;

  ifstream ifs(filename);
  if (!ifs) return;
  GetTestNum(ifs, 0, true, subject, ignored, 
    clip, area, count, ct, fr);
  ifs.close();

  solution = BooleanOp(ct, fr, subject, clip);

  SaveToSVG("solution.svg", display_width, display_height,
    &subject, NULL, &clip, &solution, NULL, fr, false);
  system("solution.svg");
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
  Path64 pattern = Ellipse(Rect64(-20, -20, 20, 20));
  solution = Paths64(MinkowskiSum(pattern, path, false));
  SaveToFile("temp.txt", solution, clip, ClipType::Union, fr);
  SaveToSVG("solution1.svg", display_width, display_height,
    NULL, NULL, NULL, &solution, NULL, fr, show_solution_coords);
  system("solution1.svg");
  solution.clear();

  //Intersection plus inflating
  subject.push_back(MakePath("500, 250, 50, 395, 325, 10, 325, 490, 50, 105"));
  clip.push_back(Ellipse(Rect64(100, 100, 400, 400)));
  SaveToFile("simple.txt", subject, clip, ct, fr);
  solution = Intersect(subject, clip, fr);
  solution = InflatePaths(solution, -10, JoinType::Round, EndType::Polygon);
  SaveToSVG("solution2.svg", display_width, display_height,
    &subject, NULL, &clip, &solution, NULL, fr, show_solution_coords);
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

    if (GetTestNum(ifs, i, false, subject, subject_open, clip, 
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
        SaveToSVG(filename2, display_width, display_height,
          &subject, &subject_open, &clip, &solution, &solution_open, 
          fr, show_solution_coords);
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
      Timer t("");
      solution = BooleanOp(ct_benchmark, fr_benchmark, subject, clip);
      if (solution.empty()) break;
    }
  }
  SaveToSVG("solution3.svg", display_width, display_height,
    &subject, NULL, &clip, &solution, NULL, fr_benchmark, false);
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
      Timer t;
      solution = Union(subject, fr_benchmark);
      if (solution.empty()) break;
    }
  }
  //nb: chrome struggles with very large SVG files (>50MB) 
  SaveToSVG("solution4.svg", display_width, display_height,
    NULL, NULL, NULL, &solution, NULL, fr_benchmark, false);
  system("solution4.svg");
}
//------------------------------------------------------------------------------

void DoBenchmark3(int ellipse_cnt_start, int ellipse_cnt_end, int increment = 1000)
{
  FillRule fr_benchmark = FillRule::NonZero;//EvenOdd;//

  Paths64 subject, solution;

  cout << endl << "Starting Circles Benchmark:  " << endl;
  const int test_width = 7680, test_height = 4320;

  Path64 ellipse = MakeEllipse(60, 60);

  for (int i = ellipse_cnt_start; i <= ellipse_cnt_end; i += increment)
  {
    solution.clear();
    subject.reserve(i);

    for (int j = 0; j < i; ++j)
      subject.push_back(RandomOffset(ellipse, test_width - 100, test_height - 100));
    //SaveToFile("benchmark_test3.txt", subject, clip, ClipType::Union, fr_benchmark);

    cout << "Ellipse count: " << i << " = ";
    {
      Timer t;
      solution = Union(subject, fr_benchmark);
      if (solution.empty()) break;
    }
  }
  SaveToSVG("solution5.svg", display_width, display_height,
    NULL, NULL, NULL, &solution, NULL, fr_benchmark, false);
  system("solution5.svg");

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
  SaveToSVG("solution.svg", display_width, display_height,
    &subject, NULL, &clip, &solution, NULL, FillRule::NonZero, false);
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
  TestType test_type = TestType::Simple;//All;//TestFile;//Benchmark;//MemoryLeak;//ErrorFile;//
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
    DoBenchmark3(10000, 30000, 10000);
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
