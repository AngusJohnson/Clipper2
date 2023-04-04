#include "benchmark/benchmark.h"
#include "clipper2/clipper.h"
#include "Utils/CommonUtils.h"
#include <iostream>

static void CustomArguments(benchmark::internal::Benchmark *b) {
  for (int i = 5; i <= 6; ++i)
    for (int j = 5; j <= 6; j *= 8)
      for (int k = 5; k <= 10; k++)
        b->Args({i, j, k});
}

template <typename T>
inline Clipper2Lib::Path<T>
StripDuplicatesCopyVersion(const Clipper2Lib::Path<T> &path,
                           bool is_closed_path) {
  using namespace Clipper2Lib;
  if (path.size() == 0)
    return Path<T>();
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

template <typename T>
inline Clipper2Lib::Paths<T>
StripDuplicatesCopyVersion(const Clipper2Lib::Paths<T> &paths,
                           bool is_closed_path) {
  using namespace Clipper2Lib;
  Paths<T> result;
  result.reserve(paths.size());
  for (typename Paths<T>::const_iterator paths_citer = paths.cbegin();
       paths_citer != paths.cend(); ++paths_citer) {
    result.push_back(StripDuplicatesCopyVersion(*paths_citer, is_closed_path));
  }
  return result;
}

static void BM_StripDuplicatesCopyVersion(benchmark::State &state) {
  using namespace Clipper2Lib;
  Paths64 op1;

  for (auto _ : state) {
    state.PauseTiming();
    int width = state.range(0);
    int height = state.range(1);
    int count = state.range(2);
    op1.push_back(MakeRandomPoly(width, height, count));
    state.ResumeTiming();
    StripDuplicatesCopyVersion(op1, true);
  }
}

static void BM_StripDuplicates(benchmark::State &state) {
  using namespace Clipper2Lib;
  Paths64 op1;
  for (auto _ : state) {
    state.PauseTiming();
    int width = state.range(0);
    int height = state.range(1);
    int count = state.range(2);
    op1.push_back(MakeRandomPoly(width, height, count));
    state.ResumeTiming();
    StripDuplicates(op1, true);
  }
}

// Register the function as a benchmark
BENCHMARK(BM_StripDuplicatesCopyVersion)->Apply(CustomArguments);
BENCHMARK(BM_StripDuplicates)->Apply(CustomArguments);
// Run the benchmark
BENCHMARK_MAIN();