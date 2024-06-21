#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include "ClipFileLoad.h"

using Clipper2Lib::Scalar;

TEST(Clipper2Tests, TestMultipleLines) {
  std::ifstream ifs("Lines.txt");
  ASSERT_TRUE(ifs.good());
  int test_number = 1;
  while (true)
  {
    Clipper2Lib::PathsI subject, subject_open, clip;
    Clipper2Lib::PathsI solution, solution_open;
    Clipper2Lib::ClipType ct;
    Clipper2Lib::FillRule fr;
    Integer area, count;
    if (!LoadTestNum(ifs, test_number,
      subject, subject_open, clip, area, count, ct, fr)) break;
    Clipper2Lib::ClipperI c;
    c.AddSubject(subject);
    c.AddOpenSubject(subject_open);
    c.AddClip(clip);
    EXPECT_TRUE(c.Execute(ct, fr, solution, solution_open));
    const Integer count2 = solution.size() + solution_open.size();
    const Integer count_diff = std::abs(count2 - count);
    const Scalar relative_count_diff = count ?
      count_diff / static_cast<Scalar>(count) :
      0;
    if (test_number == 1)
    {
      EXPECT_EQ(solution.size(), 1);
      if (solution.size() > 0)
      {
        EXPECT_EQ(solution[0].size(), 6);
        EXPECT_TRUE(IsPositive(solution[0]));
      }
      EXPECT_EQ(solution_open.size(), 1);
      if (solution_open.size() > 0)
      {
        EXPECT_EQ(solution_open[0].size(), 2);
        if (solution_open[0].size() > 0)
        {
          //expect vertex closest to input path's start
          EXPECT_EQ(solution_open[0][0].y, 6);
        }
      }
    }
    else
    {
      EXPECT_LE(count_diff, 8);
      EXPECT_LE(relative_count_diff, 0.1);
    }
    ++test_number;
  }
  ifs.close();
  EXPECT_GE(test_number, 17);
}