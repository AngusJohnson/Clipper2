#include <gtest/gtest.h>
#include "clipper2/clipper.h"

using namespace Clipper2Lib;

TEST(Clipper2Tests, TestPolytreeHoles4)
{
  Paths64 subject;
  PolyTree64 solution;
  Clipper64 c;
  subject.push_back(MakePath({ 50,500, 50,300, 100,300, 100,350, 150,350, 
    150,250, 200,250, 200,450, 350,450, 350,200, 400,200, 400,225, 450,225, 
    450,175, 400,175, 400,200, 350,200, 350,175, 200,175, 200,250, 150,250, 
    150,200, 100,200, 100,300, 50,300, 50,125, 500,125, 500,500 }));
  subject.push_back(MakePath({ 250,425, 250,375,  300,375, 300,425}));

  c.AddSubject(subject);
  c.Execute(ClipType::Union, FillRule::NonZero, solution);
  // Polytree root
  //   +- Polygon with 3 holes.
  //      +- Hole with 1 nested polygon.
  //         +-Polygon
  //      +- Hole
  //      +- Hole
  EXPECT_TRUE(solution.Count() == 1 && solution[0]->Count() == 3);
}