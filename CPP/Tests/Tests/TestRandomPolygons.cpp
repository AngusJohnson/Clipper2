#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include <fstream>
#include <random>

int GenerateRandomInteger(std::default_random_engine& rng, int min_value, int max_value)
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

  const int path_count = GenerateRandomInteger(rng, min_path_count, max_complexity);
  Clipper2Lib::Paths64 result(path_count);

  for (int path = 0; path < path_count; ++path)
  {
    const int min_point_count = 3;
    const int path_length = GenerateRandomInteger(rng, min_point_count, std::max(min_point_count, max_complexity));
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

std::string ToString(Clipper2Lib::ClipType ct)
{
  switch (ct)
  {
  case Clipper2Lib::ClipType::None:         return "NONE";
  case Clipper2Lib::ClipType::Intersection: return "INTERSECTION";
  case Clipper2Lib::ClipType::Union:        return "UNION";
  case Clipper2Lib::ClipType::Difference:   return "DIFFERENCE";
  case Clipper2Lib::ClipType::Xor:          return "XOR";
  default: throw std::runtime_error("Unexpected clip type: " + std::to_string(static_cast<int>(ct)));
  }
}

std::string ToString(Clipper2Lib::FillRule fr)
{
  switch (fr)
  {
  case Clipper2Lib::FillRule::EvenOdd:  return "EVENODD";
  case Clipper2Lib::FillRule::NonZero:  return "NONZERO";
  case Clipper2Lib::FillRule::Positive: return "POSITIVE";
  case Clipper2Lib::FillRule::Negative: return "NEGATIVE";
  default: throw std::runtime_error("Unexpected fill rule: " + std::to_string(static_cast<int>(fr)));
  }
}

void SaveInputToFile(
  const Clipper2Lib::Paths64& subject,
  const Clipper2Lib::Paths64& subject_open,
  const Clipper2Lib::Paths64& clip,
  Clipper2Lib::ClipType ct,
  Clipper2Lib::FillRule fr
)
{
  std::ofstream out("RandomPolygons.txt");
  out << "CAPTION: 1." << std::endl;
  out << "CLIPTYPE: " << ToString(ct) << std::endl;
  out << "FILLRULE: " << ToString(fr) << std::endl;

  const auto writePaths = [&out](const Clipper2Lib::Paths64& paths) {
    for (const auto& path : paths) {
      for (const auto& point : path) {
        out << point.x << "," << point.y << " ";
      }
      out << std::endl;
    }
  };

  out << "SUBJECTS" << std::endl;
  writePaths(subject);

  if (!subject_open.empty())
  {
    out << "SUBJECTS_OPEN" << std::endl;
    writePaths(subject_open);
  }

  if (!clip.empty())
  {
    out << "CLIPS" << std::endl;
    writePaths(clip);
  }
}

TEST(Clipper2Tests, TestRandomPolygons)
{
  std::default_random_engine rng(42);

  for (int i = 0; i < 750; ++i)
  {
    const auto max_complexity = std::max(1, i / 10);

    const auto subject      = GenerateRandomPaths(rng, 1, max_complexity);
    const auto subject_open = GenerateRandomPaths(rng, 0, max_complexity);
    const auto clip         = GenerateRandomPaths(rng, 0, max_complexity);

    const Clipper2Lib::ClipType ct = static_cast<Clipper2Lib::ClipType>(GenerateRandomInteger(rng, 0, 4));
    const Clipper2Lib::FillRule fr = static_cast<Clipper2Lib::FillRule>(GenerateRandomInteger(rng, 0, 3));

    SaveInputToFile(subject, subject_open, clip, ct, fr);

    Clipper2Lib::Paths64 solution, solution_open;
    Clipper2Lib::Clipper64 c;
    c.AddSubject(subject);
    c.AddOpenSubject(subject_open);
    c.AddClip(clip);
    c.Execute(ct, fr, solution, solution_open);

    const auto area_paths = static_cast<int64_t>(Area(solution));
    const auto count_paths = solution.size() + solution_open.size();

    Clipper2Lib::PolyTree64 solution_polytree;
    Clipper2Lib::Paths64 solution_polytree_open;
    Clipper2Lib::Clipper64 clipper_polytree;
    clipper_polytree.AddSubject(subject);
    clipper_polytree.AddOpenSubject(subject_open);
    clipper_polytree.AddClip(clip);
    clipper_polytree.Execute(ct, fr, solution_polytree, solution_polytree_open);

    const auto solution_polytree_paths = PolyTreeToPaths(solution_polytree);
    const auto area_polytree = static_cast<int64_t>(Area(solution_polytree_paths));
    const auto count_polytree = solution_polytree_paths.size() + solution_polytree_open.size();

    EXPECT_EQ(area_paths, area_polytree);
    EXPECT_EQ(count_paths, count_polytree);
  }
}