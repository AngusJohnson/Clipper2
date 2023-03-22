#include <gtest/gtest.h>
#include "clipper2/clipper.offset.h"
#include "ClipFileLoad.h"
#include <algorithm>

TEST(Clipper2Tests, TestOffsets) {

  std::ifstream ifs("Offsets.txt");

  for (int test_number = 1; test_number <= 2; ++test_number)
  {
    Clipper2Lib::ClipperOffset co;

    Clipper2Lib::Paths64 subject, subject_open, clip;
    Clipper2Lib::Paths64 solution, solution_open;
    Clipper2Lib::ClipType ct = Clipper2Lib::ClipType::None;
    Clipper2Lib::FillRule fr = Clipper2Lib::FillRule::NonZero;
    int64_t stored_area = 0, stored_count = 0;

    ASSERT_TRUE(LoadTestNum(ifs, test_number, subject, subject_open, clip, stored_area, stored_count, ct, fr));

    co.AddPaths(subject, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
    Clipper2Lib::Paths64 outputs;
    co.Execute(1, outputs);

    // is the sum total area of the solution is positive
    const auto outer_is_positive = Clipper2Lib::Area(outputs) > 0;

    // there should be exactly one exterior path
    const auto is_positive_func = Clipper2Lib::IsPositive<int64_t>;
    const auto is_positive_count = std::count_if(
      outputs.begin(), outputs.end(), is_positive_func);
    const auto is_negative_count =
      outputs.size() - is_positive_count;
    if (outer_is_positive)
      EXPECT_EQ(is_positive_count, 1);
    else
      EXPECT_EQ(is_negative_count, 1);
  }
}

Clipper2Lib::Point64 MidPoint(const Clipper2Lib::Point64& p1, const Clipper2Lib::Point64& p2)
{
  Clipper2Lib::Point64 result;
  result.x = (p1.x + p2.x) / 2;
  result.y = (p1.y + p2.y) / 2;
  return result;
}

TEST(Clipper2Tests, TestOffsets2) { // see #448 & #456

  double scale = 10, delta = 10 * scale, arc_tol = 0.25 * scale;

  Clipper2Lib::Paths64 subject, solution;
  Clipper2Lib::ClipperOffset c;
  subject.push_back(Clipper2Lib::MakePath({ 50,50, 100,50, 100,150, 50,150, 0,100 }));

  int err;
  subject = Clipper2Lib::ScalePaths<int64_t, int64_t>(subject, scale, err);

  c.AddPaths(subject, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
  c.ArcTolerance(arc_tol);
  c.Execute(delta, solution);

  double min_dist = delta * 2, max_dist = 0;
  for (auto subjPt : subject[0])
  {
    Clipper2Lib::Point64 prevPt = solution[0][solution[0].size() - 1];
    for (auto pt : solution[0])
    {
      Clipper2Lib::Point64 mp = MidPoint(prevPt, pt);
      double d = Clipper2Lib::Distance(mp, subjPt);
      if (d < delta * 2)
      {
        if (d < min_dist) min_dist = d;
        if (d > max_dist) max_dist = d;
      }
      prevPt = pt;
    }
  }

  EXPECT_GE(min_dist + 1, delta - arc_tol); // +1 for rounding errors
  EXPECT_LE(solution[0].size(), 21); 
}