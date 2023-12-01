#include <gtest/gtest.h>
#include "clipper2/clipper.h"
using namespace Clipper2Lib;
TEST(Clipper2Tests, TestNegativeOrientation) {
  Paths64 subjects, clips, solution;
  //also test MakePath using specified skip_chars (useful when pasting coords)
  subjects.push_back(MakePath({ 0,0, 0,100, 100,100, 100,0 }));
  subjects.push_back(MakePath({ 10,10, 10,110, 110,110, 110,10 }));
  EXPECT_FALSE(IsPositive(subjects[0]));
  EXPECT_FALSE(IsPositive(subjects[1]));
  clips.push_back(MakePath({ 50,50, 50,150, 150,150, 150,50 }));
  EXPECT_FALSE(IsPositive(clips[0]));
  solution = Union(subjects, clips, FillRule::Negative);
  ASSERT_EQ(solution.size(), 1);
  EXPECT_EQ(solution[0].size(), 12);
}