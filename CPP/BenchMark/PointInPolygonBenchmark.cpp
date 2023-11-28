#include "benchmark/benchmark.h"
#include "clipper2/clipper.h"
#include "CommonUtils.h"
#include <iostream>
#include <cstdlib>

using namespace Clipper2Lib;
using benchmark::State;

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


template <typename T>
inline PointInPolygonResult PIP2(const Point<T>& pt, const Path<T>& polygon)
{
  if (!polygon.size()) return PointInPolygonResult::IsOutside;
  Path<T>::const_iterator cend = polygon.cend();
  Path<T>::const_iterator prev = cend - 1;
  Path<T>::const_iterator curr = polygon.cbegin();

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
      while (curr != cend && curr->y < pt.y) { prev = curr;  ++curr; }
      if (curr == cend) break;
    }
    else
    {
      while (curr != cend && curr->y > pt.y) { prev = curr;  ++curr; }
      if (curr == cend) break;
    }
  
    if (curr->y == pt.y)
    {
      if ((curr->x == pt.x) || ((curr->y == prev->y) &&
        ((pt.x > prev->x) == (pt.x < curr->x))))
          return PointInPolygonResult::IsOn;
      prev = curr;  
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
      double d = CrossProduct(*prev, *curr, pt);
      if (d == 0) return PointInPolygonResult::IsOn;
      if ((d < 0) == is_above) ++val;
    }
    is_above = !is_above;
    prev = curr;
    ++curr;
  }

  return (val % 2) ? PointInPolygonResult::IsInside : PointInPolygonResult::IsOutside;
}


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


Paths64 paths;
Point64 mp;
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

enum DoTests { do_stress_test_only, do_benchmark_only, do_all_tests };

int main(int argc, char** argv) {

  const DoTests do_tests = do_all_tests;

  if (do_tests != do_benchmark_only)
  {
    // stress test PIP2 with unusual polygons
    mp = Point64(10, 10);
    std::vector<PointInPolygonResult> pipResults;

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

    std::cout << "Stress testing PIP1 for errors: ";
    for (size_t i = 0; i < paths.size(); ++i)
      if (PIP1(mp, paths[i]) != pipResults[i])
        std::cout << " (" << i << ")";
    std::cout << std::endl;
    std::cout << "Stress testing PIP2 for errors: ";
    for (size_t i = 0; i < paths.size(); ++i)
      if (PIP2(mp, paths[i]) != pipResults[i])
        std::cout << " (" << i << ")";
    std::cout << std::endl;
    std::cout << "Stress testing PIP3 for errors: ";
    for (size_t i = 0; i < paths.size(); ++i)
      if (PIP3(mp, paths[i]) != pipResults[i])
        std::cout << " (" << i << ")";
    std::cout << std::endl << std::endl;

    if (do_tests != do_all_tests)
    {
      std::string _;
      std::getline(std::cin, _);
      return 0;
    }
  }

  if (do_tests == do_stress_test_only) return 0;

  // compare 3 PIP algorithms
  const int width = 600000, height = 400000;
  mp = Point64(width / 2, height / 2);
  paths.clear();
  srand((unsigned)time(0));
  for (int i = 0, count = 10000; i < 5; ++i, count *= 10)
    paths.push_back(MakeRandomPoly(width, height, count));

  benchmark::Initialize(&argc, argv);
  BENCHMARK(BM_PIP1)->Apply(CustomArguments); // current Clipper2
  BENCHMARK(BM_PIP2)->Apply(CustomArguments); // modified Clipper2
  BENCHMARK(BM_PIP3)->Apply(CustomArguments); // Hao et al. (2018)
  benchmark::RunSpecifiedBenchmarks();

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
