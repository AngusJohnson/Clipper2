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