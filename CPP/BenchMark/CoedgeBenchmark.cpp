#include "ClipFileLoad.h"
#include "ClipFileSave.h"
#include "CommonUtils.h"
#include "benchmark/benchmark.h"
#include "clipper.svg.utils.h"
#include "clipper2/clipper.h"
#include <iostream>

static void CustomArguments(benchmark::internal::Benchmark *b) {
  for (int x_length = 10; x_length <= 5000; x_length *= 2)
    for (int y_length = 1; y_length <= 1; y_length *= 10)
      b->Args({x_length, y_length});
}

static void SaveSvg(const Clipper2Lib::Paths64 &subject,
                    const Clipper2Lib::Paths64 &solution,
                    const Clipper2Lib::FillRule &filling_rule) {
  using namespace Clipper2Lib;
  SvgWriter svg;
  SvgAddSubject(svg, subject, filling_rule);
  SvgAddSolution(svg, solution, filling_rule, false);
  SvgSaveToFile(svg, "solution.svg", 500, 500);
}

Clipper2Lib::Paths64 MakeCoedgePoly(int x_num, int y_num,
                                    int coedge_length = 5) {
  using namespace Clipper2Lib;
  Paths64 result;
  int x_width = x_num * coedge_length * 2;
  int y_height = y_num * coedge_length * 2;
  // the horizontal long rectangle
  result.push_back(
      MakePath({0, 0, x_width, 0, x_width, coedge_length, 0, coedge_length}));
  result.push_back(MakePath({0, -2 * coedge_length, x_width, -2 * coedge_length,
                             x_width, -coedge_length, 0, -coedge_length}));
  // fill the horizontal small rectangles
  for (int i = 0; i < x_width / coedge_length; i += 2)
    result.push_back(MakePath(
        {i * coedge_length, -coedge_length, (i + 1) * coedge_length,
         -coedge_length, (i + 1) * coedge_length, 0, (i)*coedge_length, 0}));

  // the vertical long rectangle
  result.push_back(MakePath(
      {0, 2 * coedge_length, coedge_length, 2 * coedge_length, coedge_length,
       coedge_length + y_height, 0, coedge_length + y_height}));
  result.push_back(
      MakePath({2 * coedge_length, 2 * coedge_length, 3 * coedge_length,
                2 * coedge_length, 3 * coedge_length, coedge_length + y_height,
                2 * coedge_length, coedge_length + y_height}));
  // fill the vertical small rectangles
  for (int j = 2; j < y_height / coedge_length; j += 2)
    result.push_back(
        MakePath({coedge_length, j * coedge_length, 2 * coedge_length,
                  j * coedge_length, 2 * coedge_length, (j + 1) * coedge_length,
                  coedge_length, (j + 1) * coedge_length}));

  return result;
}

static void BM_CoedgeInput(benchmark::State &state) {
  using namespace Clipper2Lib;
  Paths64 subject, solution;

  FillRule fill_rule = FillRule::NonZero;

  for (auto _ : state) {
    state.PauseTiming();
    int x_num = state.range(0);
    int y_num = state.range(1);
    subject = MakeCoedgePoly(x_num, y_num);
    state.ResumeTiming();
    solution = Union(subject, fill_rule);
  }
  state.SetComplexityN(state.range(0) * state.range(1));
  SaveSvg(subject, solution, fill_rule);
}

// Register the function as a benchmark
BENCHMARK(BM_CoedgeInput)
    ->Apply(CustomArguments)
    ->Complexity(benchmark::oNSquared);
// Run the benchmark
BENCHMARK_MAIN();