#include "benchmark/benchmark.h"
#include "clipper2/clipper.h"
#include "CommonUtils.h"
#include <iostream>
#include <cstdlib>

using namespace Clipper2Lib;
using benchmark::State;

// PIP1: This is the current Clipper2 PointInPolygon code
template <typename T>
inline PointInPolygonResult PIP1(const Point<T>& pt, const Path<T>& polygon)
{
  int val = 0;
  typename Path<T>::const_iterator cbegin = polygon.cbegin(), first = cbegin, curr, prev;
  typename Path<T>::const_iterator cend = polygon.cend();

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


// PIP2: This is a not fully tested modification of the current 
// Clipper2 PointInPolygon code. It's a little simpler and it's
// also marginally faster.
template <typename T>
inline PointInPolygonResult PIP2(const Point<T>& pt, const Path<T>& polygon)
{
  if (!polygon.size()) return PointInPolygonResult::IsOutside;
  Path<T>::const_iterator cend = polygon.cend();
  Path<T>::const_iterator last = cend - 1;
  Path<T>::const_iterator first = polygon.cbegin();
  Path<T>::const_iterator curr = first;
  Path<T>::const_iterator prev = last;

  bool is_above;
  if (prev->y == pt.y)
  {
    if (pt == *prev) return PointInPolygonResult::IsOn;
    if ((curr->y == pt.y) &&  ((curr->x == pt.x) || 
      ((pt.x > prev->x) == (pt.x < curr->x))))
        return PointInPolygonResult::IsOn;
    Path<T>::const_reverse_iterator  pr = polygon.crbegin() +1;
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

// PIP3: An entirely different algorithm for comparision.
// "Optimal Reliable Point-in-Polygon Test and
// Differential Coding Boolean Operations on Polygons"
// by Jianqiang Hao et al.
// Symmetry 2018, 10(10), 477; https://doi.org/10.3390/sym10100477
template <typename T>
static PointInPolygonResult PIP3(const Point<T> &pt, const Path<T> &path)
{
  T x1, y1, x2, y2;
  int k = 0;
  Path<T>::const_iterator itPrev = path.cend() - 1;
  Path<T>::const_iterator itCurr = path.cbegin();
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


// globals
Paths64 paths;
Point64 mp;
std::vector<PointInPolygonResult> pipResults;
PointInPolygonResult pip1 = PointInPolygonResult::IsOn;
PointInPolygonResult pip2 = PointInPolygonResult::IsOn;
PointInPolygonResult pip3 = PointInPolygonResult::IsOn;


static void BM_PIP1(benchmark::State& state)
{
  for (auto _ : state) 
  {
    pip1 = PIP1(mp, paths[state.range(0)]);
  }
}

static void BM_PIP2(benchmark::State& state)
{
  for (auto _ : state)
  {
    pip2 = PIP2(mp, paths[state.range(0)]);
  }
}

static void BM_PIP3(benchmark::State& state)
{
  for (auto _ : state)
  {
    pip3 = PIP3(mp, paths[state.range(0)]);
  }
}

static void CustomArguments(benchmark::internal::Benchmark* b)
{
  for (int i = 0; i < paths.size(); ++i) b->Args({ i });
}

enum ConsoleTextColor { 
  reset = 0, 
  //normal text colors ...
  red = 31, green = 32, yellow = 33, blue = 34, magenta = 35, cyan = 36, white = 37,
  //bold text colors ...
  red_bold = 91, green_bold = 92, yellow_bold = 93, blue_bold = 94, 
  magenta_bold = 95, cyan_bold = 96, white_bold = 97
};

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

static void DoErrorTest(int index)
{
  PointInPolygonResult(*pip_func)(const Point64&, const Path64&);
  switch (index)
  {
    case 1: pip_func = PIP1; break;
    case 2: pip_func = PIP2; break;
    case 3: pip_func = PIP2; break;
    default: throw "oops! - wrong function!";
  }
  std::vector<size_t> errors;
  std::cout << SetConsoleTextColor(green_bold) <<
    "Testing PIP" << index << SetConsoleTextColor(reset) <<":";
  for (size_t i = 0; i < paths.size(); ++i)
    if (pip_func(mp, paths[i]) != pipResults[i]) errors.push_back(i);
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

int main(int argc, char** argv) {

  enum DoTests { do_error_test_only, do_benchmark_only, do_all_tests };
  const DoTests do_tests = do_all_tests;// do_error_test_only;// 

  if (do_tests != do_benchmark_only)
  {
    // stress test PIP2 with unusual polygons
    mp = Point64(10, 10);

    paths.push_back({});
    pipResults.push_back(PointInPolygonResult::IsOutside);
    paths.push_back(MakePath({ 100,10, 200,10 }));
    pipResults.push_back(PointInPolygonResult::IsOutside);
    paths.push_back(MakePath({ 100,10, 200,10, 10,10, 20,20 }));
    pipResults.push_back(PointInPolygonResult::IsOn);
    paths.push_back(MakePath({ 10,10 }));
    pipResults.push_back(PointInPolygonResult::IsOn);
    paths.push_back(MakePath({ 100,10 }));
    pipResults.push_back(PointInPolygonResult::IsOutside);
    paths.push_back(MakePath({ 100,10, 110,20, 200,10, 10,10, 20,20 }));
    pipResults.push_back(PointInPolygonResult::IsOn);
    paths.push_back(MakePath({ 100,10, 110,20, 200,10, 20,20 }));
    pipResults.push_back(PointInPolygonResult::IsOutside);
    paths.push_back(MakePath({ 200,0, 0,0, 10,20, 200,0, 20,0 }));
    pipResults.push_back(PointInPolygonResult::IsInside);
    paths.push_back(MakePath({ 0,0, 20,20, 100,0 }));
    pipResults.push_back(PointInPolygonResult::IsOn);

    std::cout << "Error Tests:" << std::endl << std::endl;

    DoErrorTest(1);
    DoErrorTest(2);
    DoErrorTest(3);
    std::cout << std::endl;

    if (do_tests != do_all_tests)
    {
      std::string _;
      std::getline(std::cin, _);
      return 0;
    }
  }

  if (do_tests == do_error_test_only) return 0;

  std::cout << "Benchmarks 1: " << 
   "benchmarking PIP on a single elliptical path" << std::endl << std::endl;

  // compare 3 PIP algorithms
  const int width = 600000, height = 400000;
  mp = Point64(width / 2, height / 2);
  paths.clear();
  paths.push_back(Ellipse(mp, 10000, 6000, 10000));
  std::vector<int64_t> args{ 0 };
  benchmark::Initialize(&argc, argv);
  BENCHMARK(BM_PIP1)->Args(args);  // current Clipper2
  BENCHMARK(BM_PIP2)->Args(args);  // modified Clipper2
  BENCHMARK(BM_PIP3)->Args(args);  // Hao et al. (2018)
  benchmark::RunSpecifiedBenchmarks(benchmark::CreateDefaultDisplayReporter());
  benchmark::ClearRegisteredBenchmarks();
  benchmark::Shutdown();

  std::cout <<  std::endl << std::endl << 
    "Setting up before Benchmarks 2 ..." << 
    std::endl << std::endl;

  paths.clear();
  srand((unsigned)time(0));
  for (int i = 0, count = 10000; i < 5; ++i, count *= 10)
    paths.push_back(MakeRandomPoly(width, height, count));

  std::cout << "Benchmarks 2: " <<
    "benchmarking PIP using a single self-intersecting polygon" << std::endl << std::endl;

  benchmark::Initialize(&argc, argv);
  BENCHMARK(BM_PIP1)->Apply(CustomArguments); // current Clipper2
  BENCHMARK(BM_PIP2)->Apply(CustomArguments); // modified Clipper2
  BENCHMARK(BM_PIP3)->Apply(CustomArguments); // Hao et al. (2018)
  benchmark::RunSpecifiedBenchmarks(benchmark::CreateDefaultDisplayReporter());

  if (pip2 != pip1 || pip3 != pip1)
  {
    if (pip2 != pip1)
      std::cout << "PIP2 result is wrong!!!";
    else
      std::cout << "PIP3 result is wrong!!!";
    std::cout << paths[2] << std::endl << std::endl;
    std::string _;
    std::getline(std::cin, _);
    return 1;
  }

  return 0;
}
