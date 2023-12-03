#include "benchmark/benchmark.h"
#include "clipper2/clipper.h"
#include "CommonUtils.h"
#include <iostream>

using namespace Clipper2Lib;

// globals 
Paths64 test_paths;

// Previous (slow) StripDuplicates function - copies path
template <typename T>
inline Path<T> StripDuplicates1(const Path<T> &path, bool is_closed_path) 
{  
  if (path.size() == 0) return Path<T>();
  Path<T> result;
  result.reserve(path.size());
  typename Path<T>::const_iterator path_iter = path.cbegin();
  Point<T> first_pt = *path_iter++, last_pt = first_pt;
  result.push_back(first_pt);
  for (; path_iter != path.cend(); ++path_iter) {
    if (*path_iter != last_pt) {
      last_pt = *path_iter;
      result.push_back(last_pt);
    }
  }
  if (!is_closed_path)
    return result;
  while (result.size() > 1 && result.back() == first_pt)
    result.pop_back();
  return result;
}

// Current StripDuplicates function - modifies the path in-place (ie avoids copying)
template<typename T>
inline void StripDuplicates2(Path<T>& path, bool is_closed_path)
{
  path.erase(std::unique(path.begin(), path.end()), path.end());
  if (is_closed_path)
    while (path.size() > 1 && path.back() == path.front()) path.pop_back();
}

static void StripDuplicates_OLD(benchmark::State &state)
{
  for (auto _ : state) 
  {
    for (Path64& p: test_paths)
      p = StripDuplicates1(p, true);
  }
}

static void StripDuplicates_NEW(benchmark::State &state) 
{
  for (auto _ : state) {
    for (Path64& p : test_paths)
      StripDuplicates2(p, true); 
  }
}


int main(int argc, char** argv)
{  
  const size_t max_paths = 5;
  const int width = 6000, height = 4000, count = 10000;
  test_paths.reserve(max_paths);
  for (size_t i = 1; i <= max_paths; ++i)
    test_paths.push_back(MakeRandomPoly(width, height, count));

  benchmark::Initialize(0, nullptr);
  BENCHMARK(StripDuplicates_OLD);
  BENCHMARK(StripDuplicates_NEW);
  benchmark::RunSpecifiedBenchmarks(benchmark::CreateDefaultDisplayReporter());
}
