#include <gtest/gtest.h>
#include "clipper2/clipper.offset.h"
#include "ClipFileLoad.h"

TEST(Clipper2Tests, TestOffsettingOrientation1) {
  const Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({ 0,0, 0,5, 5,5, 5,0 }) };
  Clipper2Lib::Paths64 solution = Clipper2Lib::InflatePaths(subject, 1, 
    Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
  ASSERT_EQ(solution.size(), 1);
  //when offsetting, output orientation should match input
  EXPECT_TRUE(Clipper2Lib::IsPositive(subject[0]) == Clipper2Lib::IsPositive(solution[0]));
}

TEST(Clipper2Tests, TestOffsettingOrientation2) {

  const Clipper2Lib::Paths64 subject = {
    Clipper2Lib::MakePath({20, 220, 280, 220, 280, 280, 20, 280}),
    Clipper2Lib::MakePath({0, 200, 0, 300, 300, 300, 300, 200})
  };
  Clipper2Lib::ClipperOffset co;
  co.ReverseSolution(true); // could also assign using a parameter in ClipperOffset's constructor
  co.AddPaths(subject, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
  Clipper2Lib::Paths64 solution;
  co.Execute(5, solution);

  ASSERT_EQ(solution.size(), 2);
  // when offsetting, output orientation should match input EXCEPT when ReverseSolution == true
  // HOWEVER, input path order MANY NOT MATCH output path order, for example when inner paths (holes)
  // are defined before their container outer paths (as above).
  EXPECT_TRUE(Clipper2Lib::IsPositive(subject[1]) != Clipper2Lib::IsPositive(solution[0]));
}

