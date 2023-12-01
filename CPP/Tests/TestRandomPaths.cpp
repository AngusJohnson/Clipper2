#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include <fstream>
#include <random>
int GenerateRandomInt(std::default_random_engine& rng, int min_value, int max_value)
{
  if (min_value == max_value)
    return min_value;
  std::uniform_int_distribution<int> distribution(min_value, max_value);
  return distribution(rng);
}
Clipper2Lib::Paths64 GenerateRandomPaths(std::default_random_engine& rng, int min_path_count, int max_complexity)
{
  std::uniform_int_distribution<int> first_point_coordinate_distribution(-max_complexity, max_complexity * 2);
  std::uniform_int_distribution<int> difference_to_previous_point_distribution(-5, 5);
  const int path_count = GenerateRandomInt(rng, min_path_count, max_complexity);
  Clipper2Lib::Paths64 result(path_count);
  for (int path = 0; path < path_count; ++path)
  {
    const int min_point_count = 0;
    const int path_length = GenerateRandomInt(rng, min_point_count, std::max(min_point_count, max_complexity));
    auto& result_path = result[path];
    result_path.reserve(path_length);
    for (int point = 0; point < path_length; ++point) {
      if (result_path.empty()) {
        result_path.emplace_back(
          first_point_coordinate_distribution(rng),
          first_point_coordinate_distribution(rng)
        );
      }
      else {
        const auto& previous_point = result_path.back();
        result_path.emplace_back(
          previous_point.x + difference_to_previous_point_distribution(rng),
          previous_point.y + difference_to_previous_point_distribution(rng)
        );
      }
    }
  }
  return result;
}
TEST(Clipper2Tests, TestRandomPaths)
{
  std::default_random_engine rng(42);
#if DEBUG
  for (int i = 0; i < 10; ++i)
#else
  for (int i = 0; i < 750; ++i)
#endif
  {
    const auto max_complexity = std::max(1, i / 10);
    const auto subject      = GenerateRandomPaths(rng, 1, max_complexity);
    const auto subject_open = GenerateRandomPaths(rng, 0, max_complexity);
    const auto clip         = GenerateRandomPaths(rng, 0, max_complexity);
    const Clipper2Lib::ClipType ct = static_cast<Clipper2Lib::ClipType>(GenerateRandomInt(rng, 0, 4));
    const Clipper2Lib::FillRule fr = static_cast<Clipper2Lib::FillRule>(GenerateRandomInt(rng, 0, 3));
    //SaveInputToFile(subject, subject_open, clip, ct, fr);
    Clipper2Lib::Paths64 solution, solution_open;
    Clipper2Lib::Clipper64 c;
    c.AddSubject(subject);
    c.AddOpenSubject(subject_open);
    c.AddClip(clip);
    c.Execute(ct, fr, solution, solution_open);
    const int64_t area_paths = static_cast<int64_t>(Area(solution));
    const int64_t count_paths = solution.size() + solution_open.size();
    Clipper2Lib::PolyTree64 solution_polytree;
    Clipper2Lib::Paths64 solution_polytree_open;
    Clipper2Lib::Clipper64 clipper_polytree;
    clipper_polytree.AddSubject(subject);
    clipper_polytree.AddOpenSubject(subject_open);
    clipper_polytree.AddClip(clip);
    clipper_polytree.Execute(ct, fr, solution_polytree, solution_polytree_open);
    const auto solution_polytree_paths = PolyTreeToPaths64(solution_polytree);
    const int64_t area_polytree = static_cast<int64_t>(Area(solution_polytree_paths));
    const int64_t count_polytree = solution_polytree_paths.size() + solution_polytree_open.size();
    EXPECT_EQ(area_paths, area_polytree);
    // polytree does an additional bounds check on each path
    // and discards paths with empty bounds, so count_polytree
    // may on occasions be slightly less than count_paths even
    // though areas match
    //EXPECT_LE(count_paths - count_polytree, 2);
  }
}