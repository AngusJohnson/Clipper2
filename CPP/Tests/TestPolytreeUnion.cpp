#include <gtest/gtest.h>
#include "clipper2/clipper.h"

using namespace Clipper2Lib;

TEST(Clipper2Tests, TestPolytreeUnion) {
  Paths64 subject;
  subject.push_back(MakePath({ 0,0, 0,5, 5,5, 5,0 }));
  subject.push_back(MakePath({ 1,1, 1,6, 6,6, 6,1 }));
  Clipper64 clipper;
  clipper.AddSubject(subject);
  PolyTree64 solution;
  Paths64 open_paths;
  if (IsPositive(subject[0]))
    clipper.Execute(ClipType::Union,
      FillRule::Positive, solution, open_paths);
  else
  {
    //because clipping ops normally return Positive solutions
    clipper.ReverseSolution(true);
    clipper.Execute(ClipType::Union,
      FillRule::Negative, solution, open_paths);
  }
  EXPECT_EQ(open_paths.size(), 0);
  ASSERT_EQ(solution.Count(), 1);
  EXPECT_EQ(solution[0]->Polygon().size(), 8);
  EXPECT_EQ(IsPositive(subject[0]), IsPositive(solution[0]->Polygon()));
}

TEST(Clipper2Tests, TestPolytreeUnion2) { // #987
  Paths64 subject;
  subject.push_back(MakePath({ 534, 1024, 534, -800, 1026, -800, 1026, 1024 }));
  subject.push_back(MakePath({ 1, 1024, 8721, 1024, 8721, 1920, 1, 1920 }));
  subject.push_back(MakePath({ 30, 1024, 30, -800, 70, -800, 70, 1024 }));
  subject.push_back(MakePath({ 1, 1024, 1, -1024, 3841, -1024, 3841, 1024 }));
  subject.push_back(MakePath({ 3900, -1024, 6145, -1024, 6145, 1024, 3900, 1024 }));
  subject.push_back(MakePath({ 5884, 1024, 5662, 1024, 5662, -1024, 5884, -1024 }));
  subject.push_back(MakePath({ 534, 1024, 200, 1024, 200, -800, 534, -800 }));
  subject.push_back(MakePath({ 200, -800, 200, 1024, 70, 1024, 70, -800 }));
  subject.push_back(MakePath({ 1200, 1920, 1313, 1920, 1313, -800, 1200, -800 }));
  subject.push_back(MakePath({ 6045, -800, 6045, 1024, 5884, 1024, 5884, -800 }));

  Clipper64 clipper;
  clipper.AddSubject(subject);
  PolyTree64 solution;
  Paths64 open_paths;
    clipper.Execute(ClipType::Union, FillRule::EvenOdd, solution, open_paths);

  ASSERT_EQ(solution.Count(), 1);
  EXPECT_EQ(solution[0]->Count(), 1);
}