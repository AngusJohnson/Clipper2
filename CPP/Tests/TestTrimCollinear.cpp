#include <gtest/gtest.h>
#include "clipper2/clipper.h"
using namespace Clipper2Lib;
TEST(Clipper2Tests, TestTrimCollinear) {
  PathI input1 =
    MakePath({ 10,10, 10,10, 50,10, 100,10, 100,100, 10,100, 10,10, 20,10 });
  PathI output1 = TrimCollinear(input1, false);
  EXPECT_EQ(output1.size(), 4);
  PathI input2 =
    MakePath({ 10,10, 10,10, 100,10, 100,100, 10,100, 10,10, 10,10 });
  PathI output2 = TrimCollinear(input2, true);
  EXPECT_EQ(output2.size(), 5);
  PathI input3 = MakePath({ 10,10, 10,50, 10,10, 50,10,50,50,
    50,10, 70,10, 70,50, 70,10, 50,10, 100,10, 100,50, 100,10 });
  PathI output3 = TrimCollinear(input3);
  EXPECT_EQ(output3.size(), 0);
  PathI input4 = MakePath({ 2,3, 3,4, 4,4, 4,5, 7,5,
    8,4, 8,3, 9,3, 8,3, 7,3, 6,3, 5,3, 4,3, 3,3, 2,3 });
  PathI output4a = TrimCollinear(input4);
  PathI output4b = TrimCollinear(output4a);
  int area4a = static_cast<int>(Area(output4a));
  int area4b = static_cast<int>(Area(output4b));
  EXPECT_EQ(output4a.size(), 7);
  EXPECT_EQ(area4a, -9);
  EXPECT_EQ(output4a.size(), output4b.size());
  EXPECT_EQ(area4a, area4b);
}