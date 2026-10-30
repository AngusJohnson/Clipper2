#include <gtest/gtest.h>
#include "clipper2/clipper.h"
using namespace Clipper2Lib;
TEST(Clipper2Tests, TestPolyTreeIntersection)
{
    Clipper64 clipper;
    Paths64 subject;
    subject.push_back(MakePath({ 0,0, 0,5, 5,5, 5,0 }));
    clipper.AddSubject(subject);
    Paths64 clip;
    clip.push_back(MakePath({ 1,1,  1,6,  6,6,  6,1 }));
    clipper.AddClip (clip);
    PolyTree64 solution;
    Paths64 open_paths;
    if (IsPositive(subject[0]))
      clipper.Execute(ClipType::Intersection,
        FillRule::Positive, solution, open_paths);
    else
      clipper.Execute(ClipType::Intersection,
        FillRule::Negative, solution, open_paths);
    EXPECT_EQ(open_paths.size(), 0);
    ASSERT_EQ(solution.Count(), 1);
    EXPECT_EQ(solution[0]->Polygon().size(), 4);
}

TEST(Clipper2Tests, TestPolyTreeCyclicSplits)
{
    // emptied OutRecs whose splits form a cycle used to recurse forever in CheckSplitOwner
    Paths64 clip;
    clip.push_back(MakePath({ 3,0, 3,5, 0,3, 5,3, 0,5, 5,3, 0,3, 0,0, 0,3, 5,3, 0,0, 3,3, 2,3 }));
    Clipper64 clipper;
    clipper.PreserveCollinear(false);
    clipper.AddClip(clip);
    PolyTree64 solution;
    Paths64 open_paths;
    EXPECT_TRUE(clipper.Execute(ClipType::Xor, FillRule::EvenOdd, solution, open_paths));
}
