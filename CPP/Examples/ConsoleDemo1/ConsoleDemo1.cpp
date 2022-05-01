
#include <cstdlib>
#include <iostream>
#include <algorithm> 
#include <fstream>
#include <sstream>
#include <string>
#include <conio.h>
#include "windows.h"

#include "..\Clipper2Lib\clipper.h"
#include "..\clipper.svg.h"

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
public:
  explicit Timer() { QueryPerformanceFrequency(&qpf); }
  void Start() { QueryPerformanceCounter(&qpc1); }
  int64_t Elapsed_Millisecs()
  {
    QueryPerformanceCounter(&qpc2);
    return static_cast<int64_t>((qpc2.QuadPart - qpc1.QuadPart) * 1000 / qpf.QuadPart);
  }
};

//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

bool GetNumericValue(const string& line, string::const_iterator &s_it, int64_t &value)
{
  value = 0;
  while (s_it != line.cend() && *s_it == ' ') ++s_it;
  if (s_it == line.cend()) return false;
  bool is_neg = (*s_it == '-');
  if (is_neg) ++s_it;
  string::const_iterator s_it2 = s_it;
  while (s_it != line.cend() && *s_it >= '0' && *s_it <= '9') 
  {
    value = value * 10 + (int64_t)(*s_it++) - 48;
  }

  if (s_it == s_it2) return false; //no value
  //trim trailing space and a comma if present
  while (s_it != line.cend() && *s_it == ' ') ++s_it;
  if (s_it != line.cend() && *s_it == ',') ++s_it;
  if (is_neg) value = -value;
  return true;
}
//------------------------------------------------------------------------------

bool GetPath(const string& line, Paths64& paths)
{
  Path64 p;
  int64_t x = 0, y = 0;  
  string::const_iterator s_it = line.cbegin();
  while (GetNumericValue(line, s_it, x) && GetNumericValue(line, s_it, y))
    p.push_back(Point64(x, y));
  if (p.empty()) return false;
  paths.push_back(p);
  return true;
}
//------------------------------------------------------------------------------

bool GetPaths(stringstream& ss, Paths64& paths)
{
  bool line_found = true;
  stringstream::pos_type pos;
  paths.clear();
  std::string line;
  while (line_found)
  {
    pos = ss.tellg();
    if (!getline(ss, line)) line_found = false;
    else if (!GetPath(line, paths)) break;
  }
  //go to the beginning of the line just read
  ss.seekg(pos, ios_base::beg);
  return line_found;
}
//------------------------------------------------------------------------------

bool GetTestNum(ifstream &source, int test_num, bool seek_from_start,
  Paths64 &subj, Paths64 &subj_open, Paths64 &clip, 
  int64_t& area, int64_t& count, ClipType &ct, FillRule &fr)
{
  string line;
  bool found = false;
  if (seek_from_start) source.seekg(0, ios_base::beg);
  stringstream::pos_type last_read_line_pos = source.tellg();
  while (std::getline(source, line))
  {
    size_t line_pos = line.find("CAPTION:");
    if (line_pos == string::npos) continue;
    
    string::const_iterator s_it = (line.cbegin() + 8);
    int64_t num;
    if (test_num > 0 && GetNumericValue(line, s_it, num))
    {
      if (num > test_num) return false;
      if (num != test_num) continue;
    }

    found = true;
    subj.clear(); subj_open.clear(); clip.clear();
    while (std::getline(source, line))
    {            
      if (line.find("CAPTION:") != string::npos)
      {
        source.seekg(last_read_line_pos, ios_base::beg);
        return (!subj.empty() || !subj_open.empty() || !clip.empty());
      }
      last_read_line_pos = source.tellg();

      if (line.find("INTERSECTION") != string::npos) 
      {
        ct = ClipType::Intersection; continue;
      }
      else if (line.find("UNION") != string::npos) 
      {
        ct = ClipType::Union; continue;
      }
      else if (line.find("DIFFERENCE") != string::npos) 
      {
        ct = ClipType::Difference; continue;
      }
      else if (line.find("XOR") != string::npos) 
      {
        ct = ClipType::Xor; continue;
      }

      if (line.find("EVENODD") != string::npos) 
      {
        fr = FillRule::EvenOdd; continue;
      }
      else if (line.find("NONZERO") != string::npos) 
      {
        fr = FillRule::NonZero ; continue;
      }
      else if (line.find("POSITIVE") != string::npos) 
      {
        fr = FillRule::Positive; continue;
      }
      else if (line.find("NEGATIVE") != string::npos)
      {
        fr = FillRule::Negative; continue;
      }
      
      else if (line.find("SOL_AREA") != string::npos)
      {
        s_it = (line.cbegin() + 10);
        GetNumericValue(line, s_it, area); continue;
      }
      else if (line.find("SOL_COUNT") != string::npos)
      {
        s_it = (line.cbegin() + 11);
        GetNumericValue(line, s_it, count); continue;
      }

      if (line.find("SUBJECTS_OPEN") != string::npos) 
      {
        while (getline(source, line) && GetPath(line, subj_open));
      }
      else if (line.find("SUBJECTS") != string::npos) 
      {
        while (getline(source, line) && GetPath(line, subj));
      }
      if (line.find("CLIPS") != string::npos) 
      {
        while (getline(source, line) && GetPath(line, clip));
      }
    } //inner while still lines (found)
  } //outer while still lines (not found)
  return found;
}

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

std::string FormatMillisecs(int64_t value)
{
  std::string sValue;
  if (value >= 1000)
  {
    if (value >= 100000)
      sValue = " secs";
    else if (value >= 10000)
      sValue = "." + to_string((value % 1000)/100) + " secs";
    else
      sValue = "." + to_string((value % 1000) /10) + " secs";
    value /= 1000;
  }
  else sValue = " msecs";
  while (value >= 1000)
  {
    sValue = "," + to_string(value % 1000) + sValue;
    value /= 1000;
  }
  sValue = to_string(value) + sValue;
  return sValue;
}
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
  const Paths64 &subj, const Paths64 &subj_open,
  const Paths64 &clip, const Paths64 &solution,
  const Paths64 &solution_open,
  FillRule fill_rule, bool show_coords = false)
{
  SvgWriter svg;
  svg.fill_rule = fill_rule;
  svg.SetCoordsStyle("Verdana", 0xFF0000AA, 9);
  //svg.AddCaption("Clipper demo ...", 0xFF000000, 14, 20, 20);

  svg.AddPaths(subj, false, 0x1200009C, 0xCCD3D3DA, 0.8, show_coords);
  svg.AddPaths(subj_open, true, 0x0, 0xFFD3D3DA, 1.0, show_coords);
  svg.AddPaths(clip, false, 0x129C0000, 0xCCFFA07A, 0.8, show_coords);
  svg.AddPaths(solution, false, 0xFF80ff9C, 0xFF003300, 0.8, show_coords);
  //for (Paths::const_iterator i = solution.cbegin(); i != solution.cend(); ++i)
  //  if (Area(*i) < 0) svg.AddPath((*i), false, 0x0, 0xFFFF0000, 0.8, show_coords);
  svg.AddPaths(solution_open, true, 0x0, 0xFF000000, 1.0, show_coords);
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
    subject, ignored, clip, solution, ignored, fr, false);
  system("solution.svg");
  return;
}

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
    ignored, ignored, ignored, solution, ignored, fr, show_solution_coords);
  system("solution1.svg");

  solution.clear();
  //Intersection plus inflating
  subject.push_back(MakePath("500, 250, 50, 395, 325, 10, 325, 490, 50, 105"));
  clip.push_back(Ellipse(Rect64(100, 100, 400, 400)));
  SaveToFile("simple.txt", subject, clip, ct, fr);
  solution = Intersect(subject, clip, fr);
  solution = InflatePaths(solution, 15, JoinType::Round, EndType::Polygon);
  SaveToSVG("solution2.svg", display_width, display_height,
    subject, ignored, clip, solution, ignored, fr, show_solution_coords);
  system("solution2.svg");
}
//------------------------------------------------------------------------------

void DoTestsFromFile(const string& filename, 
  const int start_num, const int end_num, bool svg_draw, bool show_solution_coords = false)
{
  ifstream ifs(filename);
  if (!ifs) return;

  svg_draw = svg_draw && (end_num - start_num <= 50);

  cout << endl << "Running stored tests (from " << start_num <<
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
        cout << "Test " << i << " counts differ: Saved val= " <<
        count << "; New val=" << count2 << endl;
      int64_t area_diff = std::abs(area2 - area);
      if (area && (area_diff > 2) && (area_diff/static_cast<double>(area)) > 0.02)
        cout << "Test " << i << " areas differ: Saved val= " <<
        area << "; New val=" << area2 << endl;
      if (svg_draw)
      {
        string filename2 = "test_" + to_string(i) + ".svg";
        SaveToSVG(filename2, display_width, display_height,
          subject, subject_open, clip, solution, solution_open, 
          fr, show_solution_coords);
        system(filename2.c_str());
      }
    }
    else break;
  }
  cout << endl << "Fnished" << endl << endl;
}
//------------------------------------------------------------------------------

void DoBenchmark1(int edge_cnt_start, int edge_cnt_end, int increment = 1000)
{
  ClipType ct_benchmark = ClipType::Intersection;
  FillRule fr_benchmark = FillRule::NonZero;//EvenOdd;//

  Paths64 subject, clip, solution;

  cout << "\nStarting Clipper2 Benchmark Test1:  " << endl << endl;
  for (int i = edge_cnt_start; i <= edge_cnt_end; i += increment)
  {
    subject.clear();
    clip.clear();
    subject.push_back(MakeRandomPoly(800, 600, i));
    clip.push_back(MakeRandomPoly(800, 600, i));
    //SaveToFile("benchmark_test.txt", subject, clip, ct_benchmark, fr_benchmark);
    cout << "Edges: " << i << endl;
    Timer t;
    t.Start();
    solution = BooleanOp(ct_benchmark, fr_benchmark, subject, clip);
    if (solution.empty()) break;
    int64_t msecs = t.Elapsed_Millisecs();
    cout << FormatMillisecs(msecs) << endl << endl;
  }
  SaveToSVG("solution3.svg", 1200, 800,
    clip, clip, clip, solution, clip, fr_benchmark, false);
  system("solution3.svg");
}
//------------------------------------------------------------------------------

void DoBenchmark2(int edge_cnt_start, int edge_cnt_end, int increment = 1000)
{
  FillRule fr_benchmark = FillRule::EvenOdd;//NonZero;//
  Paths64 subject, clip, solution;

  cout << "\nStarting Clipper2 Benchmark Test2:  " << endl << endl;
  const int test_width = 7680, test_height = 4320;
  for (int i = edge_cnt_start; i <= edge_cnt_end; i += increment)
  {
    subject.clear();
    subject.reserve(i);
    clip.clear();
    solution.clear();
    Path64 rectangle = MakeRectangle(10, 10);
    for (int j = 0; j < i; ++j)
      subject.push_back(RandomOffset(rectangle, test_width - 10, test_height - 10));
    //SaveToFile("benchmark_test2.txt", subject, clip, ClipType::Union, fr_benchmark);    
    cout << "Rect count: " << i << endl;
    Timer t;
    t.Start();
    solution = Union(subject, fr_benchmark);
    if (solution.empty()) break;
    int64_t msecs = t.Elapsed_Millisecs();
    cout << FormatMillisecs(msecs) << endl << endl;
  }
  //chrome struggles with very large SVG files (>50MB) 
  //SaveToSVG("solution4.svg", 1200, 800,
  //  clip, clip, clip, solution, clip, fr_benchmark, false);
  //system("solution4.svg");
}
//------------------------------------------------------------------------------

void DoBenchmark3(int ellipse_cnt_start, int ellipse_cnt_end, int increment = 1000)
{
  ClipType ct_benchmark = ClipType::Intersection;//Union;//
  FillRule fr_benchmark = FillRule::EvenOdd;//NonZero;//

  Paths64 subject, clip, solution;

  cout << "\nStarting Clipper2 Benchmark Test3:  " << endl << endl;
  const int test_width = 3840, test_height = 2160;
  clip.clear();

  Path64 ellipse = MakeEllipse(80, 80);

  for (int i = ellipse_cnt_start; i <= ellipse_cnt_end; i += increment)
  {
    solution.clear();
    subject.clear();
    subject.reserve(i);

    for (int j = 0; j < i; ++j)
      subject.push_back(RandomOffset(ellipse, test_width - 100, test_height - 100));
    //SaveToFile("benchmark_test3.txt", subject, clip, ClipType::Union, fr_benchmark);

    cout << "Ellipse count: " << i << endl;
    Timer t;
    t.Start();
    solution = Union(subject, fr_benchmark);
    if (solution.empty()) break;
    int64_t msecs = t.Elapsed_Millisecs();
    cout << FormatMillisecs(msecs) << endl << endl;
  }

  SaveToSVG("solution5.svg", 960, 720,
    clip, clip, clip, solution, clip, fr_benchmark, false);
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
    subject, empty_path, clip, solution, empty_path,
    FillRule::NonZero, false);
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
    cout << "No memory leaks detected :)" << endl << endl;
  }
}

//------------------------------------------------------------------------------
// Main entry point ...
//------------------------------------------------------------------------------

int main(int argc, char* argv[])
{
  //////////////////////////////////////////////////////////////////////////
  TestType test_type = TestType::All;//TestFile;//Benchmark;//Simple;//MemoryLeak;//ErrorFile;//
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
    if (test_type != TestType::All) break;
  case TestType::TestFile:
    DoTestsFromFile("../../Tests/tests.txt", 1, 200, false);
    //or test just one of the samples in tests.txt 
    //DoTestsFromFile("../../Tests/tests.txt", 16, 16, true, true);
    if (test_type != TestType::All) break;
  case TestType::MemoryLeak:
    DoMemoryLeakTest();
    if (test_type != TestType::All) break;
  case TestType::Benchmark:
    DoBenchmark1(1000, 5000);
    DoBenchmark2(250000, 350000, 50000);
    DoBenchmark3(10000, 20000, 10000);
    break;
  }

#ifdef _DEBUG
  cout << "Press any key to continue" << endl;
  const char c = _getch();
#endif
  return 0;
}
//---------------------------------------------------------------------------
