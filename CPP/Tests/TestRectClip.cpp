#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include "ClipFileLoad.h"

TEST(Clipper2Tests, TestRectClip)
{
  std::ifstream ifs("RectClip.txt");

  ASSERT_TRUE(ifs);
  ASSERT_TRUE(ifs.good());

  int test_number = 1;
  while (true)
  {
    Clipper2Lib::Paths64 subject, subject_open, clip;
    Clipper2Lib::Paths64 solution, solution_open;
    Clipper2Lib::ClipType ct;
    Clipper2Lib::FillRule fr;
    int64_t stored_area, stored_count;

    if (!LoadTestNum(ifs, test_number, 
      subject, subject_open, clip, stored_area, stored_count, ct, fr)) break;

    const auto rect = Clipper2Lib::Bounds(clip);

    const auto result = Clipper2Lib::RectClip(rect, subject);

    EXPECT_TRUE(result.empty());

    ++test_number;
  }
  EXPECT_GE(test_number, 1);
}