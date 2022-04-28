
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
enum class TestType { Simple, TestFile, Benchmark, MemoryLeak };

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
  if (s_it != line.cend() && *s_it == ',') ++s_it;
  if (is_neg) value = -value;
  return true;
}
//------------------------------------------------------------------------------

bool GetPath(const string &line, Paths64 &paths)
{
  Path64 p;
  int64_t x = 0, y = 0;
  string::const_iterator s_it = line.cbegin();
  while (GetNumericValue(line, s_it, x) && GetNumericValue(line, s_it, y))
    p.push_back(Clipper2Lib::Point64(x, y));
  if (p.empty()) return false; 
  paths.push_back(p);
  return true;
}
//------------------------------------------------------------------------------

bool GetTestNum(ifstream &source, int test_num, bool seek_from_start,
  Paths64 &subj, Paths64 &subj_open, Paths64 &clip, 
  int64_t& area, int64_t& count,
  Clipper2Lib::ClipType &ct, Clipper2Lib::FillRule &fr)
{
  string line;
  bool found = false;
  std::streampos last_read_line_pos = source.tellg();
  if (seek_from_start) source.seekg(0, ios_base::beg);
  while (std::getline(source, line))
  {
    size_t line_pos = line.find("CAPTION:");
    if (line_pos == string::npos) continue;
    
    string::const_iterator s_it = (line.cbegin() + 8);
    int64_t num;
    if (!GetNumericValue(line, s_it, num)) continue;
    if (num > test_num) return false;
    if (num != test_num) continue;

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
        ct = Clipper2Lib::ClipType::Intersection; continue;
      }
      else if (line.find("UNION") != string::npos) 
      {
        ct = Clipper2Lib::ClipType::Union; continue;
      }
      else if (line.find("DIFFERENCE") != string::npos) 
      {
        ct = Clipper2Lib::ClipType::Difference; continue;
      }
      else if (line.find("XOR") != string::npos) 
      {
        ct = Clipper2Lib::ClipType::Xor; continue;
      }

      if (line.find("EVENODD") != string::npos) 
      {
        fr = Clipper2Lib::FillRule::EvenOdd; continue;
      }
      else if (line.find("NONZERO") != string::npos) 
      {
        fr = Clipper2Lib::FillRule::NonZero ; continue;
      }
      else if (line.find("POSITIVE") != string::npos) 
      {
        fr = Clipper2Lib::FillRule::Positive; continue;
      }
      else if (line.find("NEGATIVE") != string::npos)
      {
        fr = Clipper2Lib::FillRule::Negative; continue;
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

void GetPaths(stringstream &ss, Paths64 &paths)
{
  for (;;) 
  { 
    //for each path (line) ...
    Path64 p;
    for (;;) { //for each point
      int64_t x, y;
      char char_buf;
      int c = ss.peek();
      if (c == EOF) return;
      if (c < ' ') { //assume one or more newline chars
        ss.read(&char_buf, 1);
        break;
      }
      if (!(c == '-' || (c >= '0' && c <= '9'))) return;
      if (!(ss >> x)) return; //oops!
      c = ss.peek();
      if (c != ',') return;
      ss.read(&char_buf, 1); //gobble comma
      if (!(ss >> y)) return; //oops!
      p.push_back(Clipper2Lib::Point64(x, y));
      c = ss.peek();
      if (c != ' ') break;
      ss.read(&char_buf, 1); //gobble space
    }
    if (p.size() > 2) paths.push_back(p);
    p.clear();
  }
}
//------------------------------------------------------------------------------

bool LoadFromFile(const string &filename, 
  Paths64 &subj, Paths64 &clip, 
  Clipper2Lib::ClipType &ct, Clipper2Lib::FillRule &fr)
{
  subj.clear();
  clip.clear();
  ifstream file(filename);
	if (!file.is_open()) return false;
  stringstream ss;
  ss << file.rdbuf();
  file.close();

  string line;
  bool caption_found = false;
  for (;;)
  {
    if (!getline(ss, line)) return caption_found;
    if (!caption_found) 
    {
      caption_found = line.find("CAPTION: ") != std::string::npos;
      continue; //ie keep going until caption is found
    }

    if (line.find("CLIPTYPE:") != std::string::npos)
    {
      if (line.find("INTERSECTION") != std::string::npos)
        ct = Clipper2Lib::ClipType::Intersection;
      else if (line.find("UNION") != std::string::npos)
        ct = Clipper2Lib::ClipType::Union;
      else if (line.find("DIFFERENCE") != std::string::npos)
        ct = Clipper2Lib::ClipType::Difference;
      else
        ct = Clipper2Lib::ClipType::Xor;
    }
    else if (line.find("FILLRULE:") != std::string::npos)
    {
      if (line.find("EVENODD") != std::string::npos)
        fr = Clipper2Lib::FillRule::EvenOdd;
      else
        fr = Clipper2Lib::FillRule::NonZero;
    }
    else if (line.find("SUBJECTS") != std::string::npos) GetPaths(ss, subj);
    else if (line.find("CLIPS") != std::string::npos) GetPaths(ss, clip);
    else if (line.find("CAPTION:") != std::string::npos) break;
    else if (!subj.empty() && !clip.empty()) return true;
  }
  return !subj.empty() || !clip.empty();
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
  Clipper2Lib::ClipType ct, Clipper2Lib::FillRule fr)
{
  string cliptype;
  string fillrule;

  switch (ct) 
  {
  case Clipper2Lib::ClipType::None: cliptype = "NONE"; break;
  case Clipper2Lib::ClipType::Intersection: cliptype = "INTERSECTION"; break;
  case Clipper2Lib::ClipType::Union: cliptype = "UNION"; break;
  case Clipper2Lib::ClipType::Difference: cliptype = "DIFFERENCE"; break;
  case Clipper2Lib::ClipType::Xor: cliptype = "XOR"; break;
  }

  switch (fr) 
  {
  case Clipper2Lib::FillRule::EvenOdd: fillrule = "EVENODD"; break;
  case Clipper2Lib::FillRule::NonZero : fillrule = "NONZERO"; break;
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
    result.push_back(Clipper2Lib::Point64(rand() * width, rand() * height));
  return result;
}
//---------------------------------------------------------------------------

inline void SaveToSVG(const string &filename, int max_width, int max_height, 
  const Paths64 &subj, const Paths64 &subj_open,
  const Paths64 &clip, const Paths64 &solution,
  const Paths64 &solution_open,
  Clipper2Lib::FillRule fill_rule, bool show_coords = false)
{
  Clipper2Lib::SvgWriter svg;
  svg.fill_rule = fill_rule;
  svg.SetCoordsStyle("Verdana", 0xFF0000AA, 9);
  //svg.AddCaption("Clipper demo ...", 0xFF000000, 14, 20, 20);

  svg.AddPaths(subj, false, 0x1200009C, 0xCCD3D3DA, 0.8, show_coords);
  svg.AddPaths(subj_open, true, 0x0, 0xFFD3D3DA, 1.0, show_coords);
  svg.AddPaths(clip, false, 0x129C0000, 0xCCFFA07A, 0.8, show_coords);
  svg.AddPaths(solution, false, 0x6080ff9C, 0xFF003300, 0.8, show_coords);
  //for (Paths::const_iterator i = solution.cbegin(); i != solution.cend(); ++i)
  //  if (Area(*i) < 0) svg.AddPath((*i), false, 0x0, 0xFFFF0000, 0.8, show_coords);
  svg.AddPaths(solution_open, true, 0x0, 0xFF000000, 1.0, show_coords);
  svg.SaveToFile(filename, max_width, max_height, 80);
}
//------------------------------------------------------------------------------

void DoSimple()
{
  Paths64 subject, subject_open, clip;
  Paths64 solution, solution_open;
  bool show_solution_coords = false;

  ////Minkowski
  //Path64 path = MakePath("0, 0, 100, 200, 200, 0 ");
  //Path64 pattern = Ellipse(Rect64(-20, -20, 20, 20)); 
  //solution = Paths64(MinkowskiSum(pattern, path, false));
  //SaveToFile("temp.txt", solution, clip, ClipType::Union, FillRule::NonZero);
  //SaveToSVG("solution.svg", display_width, display_height,
  //  solution, subject, subject, solution, subject,
  //  fr_simple, show_solution_coords);
  //system("solution.svg");

  //Intersection plus inflating
  subject.push_back(MakePath("500, 250, 50, 395, 325, 10, 325, 490, 50, 105"));
  clip.push_back(Ellipse(Rect64(100, 100, 400, 400)));
  //SaveToFile("simple.txt", subject, clip, ct_simple, fr_simple);
  solution = Intersect(subject, clip, FillRule::NonZero);
  solution = InflatePaths(solution, 15, JoinType::Round, EndType::Polygon);
  SaveToSVG("solution.svg", display_width, display_height,
    subject, subject_open, clip, solution, solution_open, 
    FillRule::NonZero, show_solution_coords);
  system("solution.svg");
}
//------------------------------------------------------------------------------

void DoTestsFromFile(const string& filename, 
  const int start_num, const int end_num, bool svg_draw)
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
    Clipper2Lib::ClipType ct;
    Clipper2Lib::FillRule fr;
    int64_t area, count;

    if (GetTestNum(ifs, i, false, subject, subject_open, clip, 
      area, count, ct, fr)) 
    {
      Clipper2Lib::Clipper64 c;
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
          subject, subject_open, clip, solution, solution_open, fr, false);
        system(filename2.c_str());
      }
    }
    else break;
  }
  cout << endl << "Fnished" << endl << endl;
}
//------------------------------------------------------------------------------

void DoBenchmark(int edge_cnt_start, int edge_cnt_end, int increment = 1000)
{
  Clipper2Lib::ClipType ct_benchmark = Clipper2Lib::ClipType::Intersection;//Union;//
  Clipper2Lib::FillRule fr_benchmark = Clipper2Lib::FillRule::NonZero;//EvenOdd;//

  Paths64 subject, clip, solution;

  cout << "\nStarting Clipper2 Benchmarks:  " << endl << endl;
  for (int i = edge_cnt_start; i <= edge_cnt_end; i += increment)
  {
    subject.clear();
    clip.clear();
    subject.push_back(MakeRandomPoly(display_width, display_height, i));
    clip.push_back(MakeRandomPoly(display_width, display_height, i));
    //SaveToFile("benchmark_test.txt", subject, clip, ct_benchmark, fr_benchmark);

    cout << "Edges: " << i << endl;
    Timer t;
    t.Start();
    solution = Intersect(subject, clip, FillRule::NonZero);
    int64_t msecs = t.Elapsed_Millisecs();
    cout << FormatMillisecs(msecs) << endl << endl;
  }
}
//------------------------------------------------------------------------------

void DoMemoryLeakTest()
{
  int edge_cnt = 1000;

  Paths64 subject, clip, solution, empty_path;
  subject.push_back(MakeRandomPoly(display_width, display_height, edge_cnt));
  clip.push_back(MakeRandomPoly(display_width, display_height, edge_cnt));

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
    cout << "Memory leaks detected! See debugger output." << endl;
    OutputDebugString(L"-----------_CrtMemDumpStatistics ---------\r\n");
    _CrtMemDumpStatistics(&sDiff);
  }
  else
  {
    cout << "No memory leaks detected!" << endl;
  }
}

//------------------------------------------------------------------------------
// Main entry point ...
//------------------------------------------------------------------------------

int main(int argc, char* argv[])
{
  //////////////////////////////////////////////////////////////////////////
  TestType test_type = TestType::Simple;//Benchmark;//MemoryLeak;//TestFile;//
  //////////////////////////////////////////////////////////////////////////

  srand((unsigned)time(0));

  switch (test_type)
  {
  case TestType::Simple:
    DoSimple();
    break;
  case TestType::TestFile:
    DoTestsFromFile("../../Tests/tests.txt", 1, 200, false);
    break;
  case TestType::MemoryLeak:
    DoMemoryLeakTest();
    break;
  case TestType::Benchmark: 
    DoBenchmark(1000, 7000);
    break;
  }

  cout << "Press any key to continue" << endl;
  const char c = _getch();
  return 0;
}
//---------------------------------------------------------------------------
