#include "benchmark/benchmark.h"
#include "clipper2/clipper.h"
#include "CommonUtils.h"
#include "ClipFileLoad.h"
#include <iostream>
#include <cstdlib>
#include <ctime>

using namespace Clipper2Lib;

enum ConsoleTextColor {
  reset = 0,
  //normal text colors ...
  red = 31, green = 32, yellow = 33, blue = 34, magenta = 35, cyan = 36, white = 37,
  //bold text colors ...
  red_bold = 91, green_bold = 92, yellow_bold = 93, blue_bold = 94,
  magenta_bold = 95, cyan_bold = 96, white_bold = 97
};

//////////////////////////////////////////////////////////////////////////////////////
// SetConsoleTextColor: a simple class to adjust Console Text Colors (Windows & Linux)
//////////////////////////////////////////////////////////////////////////////////////

struct SetConsoleTextColor
{
private:
  ConsoleTextColor _color;
public:
  SetConsoleTextColor(ConsoleTextColor color) : _color(color) {};

  static friend std::ostream& operator<< (std::ostream& out, SetConsoleTextColor const& scc)
  {
    return out << "\x1B[" << scc._color << "m";
  }
};
//////////////////////////////////////////////////////////////////////////////////////


typedef std::function<PointInPolygonResult(const Point64&, const Path64&)> PipFunction;

/////////////////////////////////////////////////////////
// PIP1: This is the current Clipper2 PointInPolygon code
/////////////////////////////////////////////////////////
inline PointInPolygonResult PIP1(const Point64& pt, const Path64& polygon)
{
  int val = 0;
  typename Path64::const_iterator cbegin = polygon.cbegin(), first = cbegin, curr, prev;
  typename Path64::const_iterator cend = polygon.cend();

  while (first != cend && first->y == pt.y) ++first;
  if (first == cend) // not a proper polygon
    return PointInPolygonResult::IsOutside;

  bool is_above = first->y < pt.y, starting_above = is_above;
  curr = first + 1;
  while (true)
  {
    if (curr == cend)
    {
      if (cend == first || first == cbegin) break;
      cend = first;
      curr = cbegin;
    }

    if (is_above)
    {
      while (curr != cend && curr->y < pt.y) ++curr;
      if (curr == cend) continue;
    }
    else
    {
      while (curr != cend && curr->y > pt.y) ++curr;
      if (curr == cend) continue;
    }

    if (curr == cbegin)
      prev = polygon.cend() - 1;
    else
      prev = curr - 1;

    if (curr->y == pt.y)
    {
      if (curr->x == pt.x ||
        (curr->y == prev->y &&
          ((pt.x < prev->x) != (pt.x < curr->x))))
        return PointInPolygonResult::IsOn;
      ++curr;
      if (curr == first) break;
      continue;
    }

    if (pt.x < curr->x && pt.x < prev->x)
    {
      // we're only interested in edges crossing on the left
    }
    else if (pt.x > prev->x && pt.x > curr->x)
      val = 1 - val; // toggle val
    else
    {
      double d = CrossProduct(*prev, *curr, pt);
      if (d == 0) return PointInPolygonResult::IsOn;
      if ((d < 0) == is_above) val = 1 - val;
    }
    is_above = !is_above;
    ++curr;
  }

  if (is_above != starting_above)
  {
    cend = polygon.cend();
    if (curr == cend) curr = cbegin;
    if (curr == cbegin) prev = cend - 1;
    else prev = curr - 1;
    double d = CrossProduct(*prev, *curr, pt);
    if (d == 0) return PointInPolygonResult::IsOn;
    if ((d < 0) == is_above) val = 1 - val;
  }

  return (val == 0) ?
    PointInPolygonResult::IsOutside :
    PointInPolygonResult::IsInside;
}


/////////////////////////////////////////////////////////
// PIP2: This is a not fully tested modification of the
// current Clipper2 PointInPolygon code. It's a little
// simpler and also marginally faster.
/////////////////////////////////////////////////////////
inline PointInPolygonResult PIP2(const Point64& pt, const Path64& polygon)
{
  if (!polygon.size()) return PointInPolygonResult::IsOutside;
  Path64::const_iterator cend = polygon.cend();
  Path64::const_iterator last = cend - 1;
  Path64::const_iterator first = polygon.cbegin();
  Path64::const_iterator curr = first;
  Path64::const_iterator prev = last;

  bool is_above;
  if (prev->y == pt.y)
  {
    if (pt == *prev) return PointInPolygonResult::IsOn;
    if ((curr->y == pt.y) &&  ((curr->x == pt.x) ||
      ((pt.x > prev->x) == (pt.x < curr->x))))
        return PointInPolygonResult::IsOn;
    Path64::const_reverse_iterator  pr = polygon.crbegin() +1;
    while (pr != polygon.crend() && pr->y == pt.y) ++pr;
    is_above = pr == polygon.crend() || pr->y < pt.y;
  }
  else is_above = prev->y < pt.y;

  int val = 0;
  while (curr != cend)
  {
    if (is_above)
    {
      while (curr != cend && curr->y < pt.y) ++curr;
      if (curr == cend) break;
    }
    else
    {
      while (curr != cend && curr->y > pt.y) ++curr;
      if (curr == cend) break;
    }

    prev = (curr == first) ? last : curr - 1;
    if (curr->y == pt.y)
    {
      if ((curr->x == pt.x) || ((curr->y == prev->y) &&
        ((pt.x > prev->x) == (pt.x < curr->x))))
          return PointInPolygonResult::IsOn;
      ++curr;
      continue;
    }

    if (pt.x < curr->x && pt.x < prev->x)
    {
      // we're only interested in edges crossing on the left
    }
    else if (pt.x > prev->x && pt.x > curr->x)
      ++val;
    else
    {
      double d = CrossProduct(*prev, *curr, pt); //avoids integer overflow
      if (d == 0) return PointInPolygonResult::IsOn;
      if ((d < 0) == is_above) ++val;
    }
    is_above = !is_above;
    ++curr;
  }

  return (val % 2) ?
    PointInPolygonResult::IsInside :
    PointInPolygonResult::IsOutside;
}

/////////////////////////////////////////////////////////
// PIP3: An entirely different algorithm for comparision.
// "Optimal Reliable Point-in-Polygon Test and
// Differential Coding Boolean Operations on Polygons"
// by Jianqiang Hao et al.
// Symmetry 2018, 10(10), 477; https://doi.org/10.3390/sym10100477
/////////////////////////////////////////////////////////
static PointInPolygonResult PIP3(const Point64&pt, const Path64&path)
{
  if (!path.size()) return PointInPolygonResult::IsOutside;
  int64_t x1, y1, x2, y2;
  int k = 0;
  Path64::const_iterator itPrev = path.cend() - 1;
  Path64::const_iterator itCurr = path.cbegin();
  for ( ; itCurr != path.cend(); ++itCurr)
  {
    y1 = itPrev->y - pt.y;
    y2 = itCurr->y - pt.y;
    if (((y1 < 0) && (y2 < 0)) || ((y1 > 0) && (y2 > 0)))
    {
      itPrev = itCurr;
      continue;
    }

    x1 = itPrev->x - pt.x;
    x2 = itCurr->x - pt.x;
    if ((y1 <= 0) && (y2 > 0))
    {
      //double f = double(x1) * y2 - double(x2) * y1; // avoids int overflow
      int64_t f = x1 * y2 - x2 * y1;
      if (f > 0) ++k;
      else if (f == 0) return PointInPolygonResult::IsOn;
    }
    else if ((y1 > 0) && (y2 <= 0))
    {
      int64_t f = x1 * y2 - x2 * y1;
      if (f < 0) ++k;
      else if (f == 0) return PointInPolygonResult::IsOn;
    }
    else if (((y2 == 0) && (y1 < 0)) || ((y1 == 0) && (y2 < 0)))
    {
      int64_t f = x1 * y2 - x2 * y1;
      if (f == 0) return PointInPolygonResult::IsOn;
    }
    else if ((y1 == 0) && (y2 == 0) &&
      (((x2 <= 0) && (x1 >= 0)) || ((x1 <= 0) && (x2 >= 0))))
        return PointInPolygonResult::IsOn;
    itPrev = itCurr;
  }
  if (k % 2) return PointInPolygonResult::IsInside;
  return PointInPolygonResult::IsOutside;
}


/////////////////////////////////////////////////////////
// global data structures
/////////////////////////////////////////////////////////

const Path64 points_of_interest_outside = 
  MakePath({ 21887,10420, 21726,10825, 21662,10845, 21617,10890 });
const Path64 points_of_interest_inside =
  MakePath({ 21887,10430, 21843,10520, 21810,10686, 21900,10461 });

Point64 mp;
Paths64 paths;
std::vector < std::vector<PointInPolygonResult> > pipResults;


/////////////////////////////////////////////////////////
// Benchmark callback functions
/////////////////////////////////////////////////////////

static void BM_PIP1(benchmark::State& state)
{
  int64_t idx = state.range(0);
  for (auto _ : state)
    pipResults[0][idx] = PIP1(mp, paths[idx]);

}

static void BM_PIP2(benchmark::State& state)
{
  int64_t idx = state.range(0);
  for (auto _ : state)
    pipResults[1][idx] = PIP2(mp, paths[idx]);
}

static void BM_PIP3(benchmark::State& state)
{
  int64_t idx = state.range(0);
  for (auto _ : state)
    pipResults[2][idx] = PIP3(mp, paths[idx]);
}

/////////////////////////////////////////////////////////
// Miscellaneous functions
/////////////////////////////////////////////////////////

static void CustomArguments(benchmark::internal::Benchmark* b)
{
  for (int i = 0; i < paths.size(); ++i) b->Args({ i });
}

inline PipFunction GetPIPFunc(int index)
{
  PipFunction result;
  switch (index)
  {
    case 0: result = PIP1; break;
    case 1: result = PIP2; break;
    case 2: result = PIP3; break;
    default: throw "oops! - wrong function!";
  }
  return result;
}

/////////////////////////////////////////////////////////
// Error checking functions
/////////////////////////////////////////////////////////

static void DoErrorTest1_internal(const Path64& pts_of_int, const Paths64& paths,
  PipFunction pip_func, PointInPolygonResult expected)
{
  Path64 error_points;

  for (Point64 poi : pts_of_int)
  {
    size_t inside_cnt = 0;
    for (const Path64& path : paths)
      if (pip_func(poi, path) == PointInPolygonResult::IsInside) ++inside_cnt;
    switch (expected)
    {
    case PointInPolygonResult::IsInside:
      if (inside_cnt != 1) error_points.push_back(poi); break;
    case PointInPolygonResult::IsOutside:
      if (inside_cnt) error_points.push_back(poi); break;
    }
  }

  if (error_points.size())
  {
    size_t high_error = error_points.size() - 1;
    std::cout << SetConsoleTextColor(red_bold) << " Errors at ";
    for (size_t i = 0; i < high_error; ++i) std::cout << "(" << error_points[i] << "), ";
    std::cout << "(" << error_points[high_error] << ")." << SetConsoleTextColor(reset) << std::endl;
  }
  else
    std::cout << " No errors found." << std::endl;
}

static void DoErrorTest1(int index)
{
  PipFunction pip_func = GetPIPFunc(index);

  std::cout << SetConsoleTextColor(green_bold) <<
    "Testing PIP" << index +1 << "/outside:" << SetConsoleTextColor(reset);
  DoErrorTest1_internal(points_of_interest_outside, paths,
    pip_func, PointInPolygonResult::IsOutside);

  std::cout << SetConsoleTextColor(green_bold) <<
    "Testing PIP" << index +1 << "/inside :" << SetConsoleTextColor(reset);
  DoErrorTest1_internal(points_of_interest_inside, paths,
    pip_func, PointInPolygonResult::IsInside);
}

static void DoErrorTest2(int index)
{
  PipFunction pip_func = GetPIPFunc(index);

  std::vector<size_t> errors;
  std::cout << SetConsoleTextColor(green_bold) <<
    "Testing PIP" << index +1 << SetConsoleTextColor(reset) <<":";
  for (size_t i = 0; i < paths.size(); ++i)
    if (pip_func(mp, paths[i]) != pipResults[0][i]) errors.push_back(i);
  if (errors.size())
  {
    size_t high_error = errors.size() - 1;
    std::cout << SetConsoleTextColor(red_bold) << " Error in ";
    for (size_t i = 0; i < high_error; ++i)
      std::cout << errors[i] << " and ";
    std::cout << errors[high_error] << "." <<
      SetConsoleTextColor(reset) << std::endl;
  }
  else
    std::cout << " No errors found." << std::endl;
}


/////////////////////////////////////////////////////////
// Main Entry
/////////////////////////////////////////////////////////

int main(int argc, char** argv)
{
  std::cout << SetConsoleTextColor(cyan_bold) <<
    "Simple error checks ..." << SetConsoleTextColor(reset) <<
    std::endl;

  //////////////////////////////////////////////////////////////
  // 1. Very basic error testing 
  //////////////////////////////////////////////////////////////

  std::cout << std::endl << SetConsoleTextColor(yellow_bold) <<
    "Tests for errors #1:" << SetConsoleTextColor(reset) << std::endl << 
    "(Reusing 'TestPolytreeHoles' tests)" << std::endl << std::endl;

  // 1a. use const paths (PolytreeHoleOwner2.txt) with changing points of interest

  Paths64 subject, subject_open, clip;
  int64_t _, __;
  ClipType ___;
  FillRule ____;
  const std::string test_file = "../../../../../Tests/PolytreeHoleOwner2.txt";
  if (!FileExists(test_file)) return 1;
  std::ifstream ifs(test_file);
  if (!ifs || !ifs.good()) return 1;
  LoadTestNum(ifs, 1, paths, subject_open, clip, _, __, ___, ____);
  ifs.close();
    
  for (int i = 0; i < 3; ++i) DoErrorTest1(i);

  // 1b. Use a const point of interest (10,10) against various paths

  std::cout << std::endl << SetConsoleTextColor(yellow_bold) <<
    "Tests for errors #2:" << SetConsoleTextColor(reset) << std::endl <<
    "(Testing with 'unusual' polygons)" << std::endl << std::endl;

  mp = Point64(10, 10);
  paths.clear();
  pipResults.clear();
  pipResults.resize(1);
  paths.push_back({}); // ie test an empty path
  pipResults[0].push_back(PointInPolygonResult::IsOutside);
  paths.push_back(MakePath({ 100,10, 200,10 }));
  pipResults[0].push_back(PointInPolygonResult::IsOutside);
  paths.push_back(MakePath({ 100,10, 200,10, 10,10, 20,20 }));
  pipResults[0].push_back(PointInPolygonResult::IsOn);
  paths.push_back(MakePath({ 10,10 }));
  pipResults[0].push_back(PointInPolygonResult::IsOn);
  paths.push_back(MakePath({ 100,10 }));
  pipResults[0].push_back(PointInPolygonResult::IsOutside);
  paths.push_back(MakePath({ 100,10, 110,20, 200,10, 10,10, 20,20 }));
  pipResults[0].push_back(PointInPolygonResult::IsOn);
  paths.push_back(MakePath({ 100,10, 110,20, 200,10, 20,20 }));
  pipResults[0].push_back(PointInPolygonResult::IsOutside);
  paths.push_back(MakePath({ 200,0, 0,0, 10,20, 200,0, 20,0 }));
  pipResults[0].push_back(PointInPolygonResult::IsInside);
  paths.push_back(MakePath({ 0,0, 20,20, 100,0 }));
  pipResults[0].push_back(PointInPolygonResult::IsOn);

  for (int i = 0; i < 3; ++i) DoErrorTest2(i);
  std::cout << std::endl;

  // 2. Benchmark functions

  std::cout << std::endl << SetConsoleTextColor(cyan_bold) <<
    "Benchmarking ..." << SetConsoleTextColor(reset) << std::endl;
  std::cout << "Note: function performance varies depending on the proportion of edges" << 
    std::endl << "that intersect with an imaginary horizontal line passing through the" <<
    std::endl << "point of interest." << std::endl << std::endl;

  unsigned int width = 600000, height = 400000, count = 10000000;
  mp = Point64(width / 2, height / 2);


  std::cout << std::endl << SetConsoleTextColor(yellow_bold) <<
    "Benchmarks 1:" << SetConsoleTextColor(reset) << std::endl;

  paths.clear();
  for (int i = 0; i < 5; ++i)
    paths.push_back(Ellipse(mp, width / 2.0, height / 2.0, count));
  std::cout << "A single elliptical path (" <<
    width << " x " << height << ")" << std::endl <<
    "Edge count =  " << count << ". " << std::endl <<
    "Point (" << mp << ")" << std::endl << std::endl;

  pipResults.clear();
  pipResults.resize(3);
  for (size_t i = 0; i < 3; ++i) pipResults[i].resize(paths.size());

  benchmark::Initialize(0, nullptr);
  BENCHMARK(BM_PIP1)->Apply(CustomArguments); // current Clipper2
  BENCHMARK(BM_PIP2)->Apply(CustomArguments); // modified Clipper2
  BENCHMARK(BM_PIP3)->Apply(CustomArguments); // Hao et al. (2018)
  benchmark::RunSpecifiedBenchmarks(benchmark::CreateDefaultDisplayReporter());
  
  std::cout << std::endl << std::endl << SetConsoleTextColor(yellow_bold) <<
    "Benchmarks 2:" << SetConsoleTextColor(reset) << std::endl;

  std::cout << "A random self-intersecting polygon (" <<
    width << " x " << height << ")" << std::endl <<
    "Edge count =  " << count << ". " << std::endl <<
    "Point (" << mp << ")" << std::endl << std::endl;

  paths.clear();
  for (int i = 0; i < 5; ++i)
    paths.push_back(MakeRandomPoly(width, height, count));

  pipResults.clear();
  pipResults.resize(3);
  for (size_t i = 0; i < 3; ++i) pipResults[i].resize(paths.size());

  // rerun benchmarks using different polygons
  benchmark::RunSpecifiedBenchmarks(benchmark::CreateDefaultDisplayReporter());

  std::cout << std::endl;
  // compare results to ensure they all agree :)
  const std::string bad_filename = "test_pip_";
  for (size_t i = 0; i < pipResults[0].size(); ++i)
  {
    if ((pipResults[0][i] == pipResults[1][i]) &&
      (pipResults[0][i] == pipResults[2][i])) continue;

    if (pipResults[0][i] != pipResults[1][i])
      std::cout << "PIP2 returned the " << SetConsoleTextColor(red_bold) << "wrong " <<
      SetConsoleTextColor(reset) << "result:" << std::endl;
    if (pipResults[0][i] != pipResults[2][i])
      std::cout << "PIP3 returned the " << SetConsoleTextColor(red_bold) << "wrong " <<
      SetConsoleTextColor(reset) << "result:" << std::endl;

    std::cout << "Problematic PIP path saved to - " << bad_filename << i << ".txt" << std::endl;
    std::ofstream of(bad_filename);
    of << paths[i] << std::endl;
    of.close();
    break;
  }

}
