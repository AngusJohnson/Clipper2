#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/ClipFileLoad.h"

using namespace Clipper2Lib;

TEST(Clipper2Tests, TestSimpleOpenPath) 
{
    Paths64 subject, subject_open, clip;
    Paths64 solution, solution_open;
    
    subject.push_back(MakePath("5, 4, 8, 4, 8, 8, 5, 8"));
    subject_open.push_back(MakePath("6, 7, 6, 5"));
    clip.push_back(MakePath("7, 9, 4, 9, 4, 6, 7, 6"));
    
    ClipType ct = ClipType::Difference;
    FillRule fr = FillRule::EvenOdd;

    Clipper64 c;
    c.AddSubject(subject);
    c.AddOpenSubject(subject_open);
    c.AddClip(clip);
    ASSERT_TRUE(c.Execute(ct, fr, solution, solution_open));

    EXPECT_EQ(solution.size(), 1);
    if (solution.size() > 0)
      EXPECT_EQ(solution[0].size(), 6);
    EXPECT_EQ(solution_open.size(), 1);
    if (solution_open.size() > 0)
      EXPECT_EQ(solution_open[0].size(), 2);
}