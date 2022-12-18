#include <cstdlib>
#include <string>
#include <chrono> 
#include <random>

#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/ClipFileLoad.h"
#include "../../Utils/ClipFileSave.h"
#include "../../Utils/Timer.h"

using namespace Clipper2Lib;

const int display_width = 800, display_height = 600, max_paths = 10;

void DoRandomTest();
void System(const std::string &filename);

int main()
{  
  srand((unsigned)time(0));
  DoRandomTest();
  return 0;
}

int GenerateRandomNumber(std::default_random_engine& rng, int min_value, int max_value)
{
  if (min_value >= max_value) return min_value;
  std::uniform_int_distribution<int> distribution(min_value, max_value);
  return distribution(rng);
}

Paths64 GenerateRandomPaths(std::default_random_engine& rng,  int path_count, int edge_count)
{
  if (!path_count) return Paths64();
  // with fewer paths, keep them closer to the center of the display ...
  double center_x = display_width / 2.0; 
  double center_y = display_height / 2.0;
  double dx = center_x / 3.0 * (static_cast<double>(path_count) / max_paths);
  double dy = center_y / 3.0 * (static_cast<double>(path_count) / max_paths);
  std::normal_distribution<double> first_point_coordinate_x(0, dx);
  std::normal_distribution<double> first_point_coordinate_y(0, dy);
  std::uniform_int_distribution<int> orthogonal_dist_next_point(-100, 100);

  Clipper2Lib::Paths64 result(path_count);

  for (int path = 0; path < path_count; ++path)
  {
    const int path_length = edge_count;
    auto& result_path = result[path];
    result_path.reserve(path_length);

    for (int point = 0; point < path_length; ++point) {
      if (result_path.empty()) {
        result_path.emplace_back(
          center_x + first_point_coordinate_x(rng),
          center_y + first_point_coordinate_y(rng));
      }
      else {
        const auto& previous_point = result_path.back();
        result_path.emplace_back(
          previous_point.x + orthogonal_dist_next_point(rng),
          previous_point.y + orthogonal_dist_next_point(rng));
      }
    }
  }
  return result;
}

std::string ClipTypeToString(ClipType ct)
{
  switch (ct)
  {
  case ClipType::Union: return "Union";
  case ClipType::Difference: return "Difference";
  case ClipType::Xor: return "Xor";
  default: return "Intersection";
  }
}

std::string FillRuleToString(FillRule fr)
{
  switch (fr)
  {
  case FillRule::NonZero: return "NonZero";
  case FillRule::Positive: return "Positive";
  case FillRule::Negative: return "Negative";
  default: return "EvenOdd";
  }
}

void RandomTest(int path_count)
{
  unsigned seed = (unsigned)std::chrono::system_clock::now().time_since_epoch().count();
  std::default_random_engine rng(seed);
  
  ClipType cliptype =
    static_cast<Clipper2Lib::ClipType>(GenerateRandomNumber(rng, 0, 3) + 1);
  FillRule fillrule =
    static_cast<Clipper2Lib::FillRule>(GenerateRandomNumber(rng, 0, 1));

  int max_edges = path_count * 3;
  path_count = std::min(max_paths, path_count);

  Paths64 subj, subj_open, clip, sol, sol_open;
  // generate exactly path_count subjects, between 0 & path_count open subjects, 
  // and between 1 & path_count clips 
  subj = GenerateRandomPaths(rng, path_count, GenerateRandomNumber(rng, 3, max_edges));
  subj_open = GenerateRandomPaths(rng, 
    GenerateRandomNumber(rng, 0, path_count), GenerateRandomNumber(rng, 3, max_edges));
  clip = GenerateRandomPaths(rng, 
    GenerateRandomNumber(rng, 1, path_count), GenerateRandomNumber(rng, 3, max_edges));

  //SaveTest("random.txt", false, &subj, &subj_open, &clip, 0, 0, cliptype, fillrule);
  //int64_t area, cnt;
  //std::ifstream ifs("random2.txt");
  //LoadTestNum(ifs, 1, subj, subj_open, clip, area, cnt, cliptype, fillrule);

  Clipper64 c64;
  c64.AddSubject(subj);
  c64.AddOpenSubject(subj_open);
  c64.AddClip(clip);
  if (!c64.Execute(cliptype, fillrule, sol, sol_open)) return;

  SvgWriter svg;
  SvgAddSubject(svg, subj, fillrule);
  SvgAddOpenSubject(svg, subj_open, fillrule);
  SvgAddClip(svg, clip, fillrule);
  //if (fillrule == FillRule::Negative)
  //  for (auto& path : sol) std::reverse(path.begin(), path.end());
  SvgAddSolution(svg, sol, fillrule, false);
  SvgAddOpenSolution(svg, sol_open, fillrule, false);
  SvgAddCaption(svg, 
    ClipTypeToString(cliptype) + ", " + FillRuleToString(fillrule), 20, 20);
  std::string filename = "random_" + std::to_string(path_count) + ".svg";
  SvgSaveToFile(svg, filename, 800, 600, 10);
  System(filename);
}

void DoRandomTest()
{
  for (int path_cnt = 1; path_cnt <= 10; ++path_cnt)
    RandomTest(path_cnt);
}

void System(const std::string& filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
