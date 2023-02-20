#include <gtest/gtest.h>
#include "clipper2/clipper.h"

using namespace Clipper2Lib;

TEST(Clipper2Tests, TestPolytreeHoles3)
{
  
  Paths64 subject, clip, sol;
  PolyTree64 solution;
  Clipper64 c;
  subject.push_back(MakePath({ 1072,501, 1072,501, 1072,539, 1072,539, 1072,539, 870,539,
    870,539, 870,539, 870,520, 894,520, 898,524, 911,524, 915,520, 915,520, 936,520,
    940,524, 953,524, 957,520, 957,520, 978,520, 983,524, 995,524, 1000,520, 1021,520,
    1025,524, 1038,524, 1042,520, 1038,516, 1025,516, 1021,520, 1000,520, 995,516,
    983,516, 978,520, 957,520, 953,516, 940,516, 936,520, 915,520, 911,516, 898,516,
    894,520, 870,520, 870,516, 870,501, 870,501, 870,501, 1072,501 }));

  clip.push_back(MakePath({ 870,501, 971,501, 971,539, 870,539 }));

  c.AddSubject(subject);
  c.AddClip(clip);
  c.Execute(ClipType::Intersection, FillRule::NonZero, solution);
  EXPECT_TRUE(solution.Count() == 1 && solution[0]->Count() == 2);
}