#include "clipper2/clipper.h"
#include <gtest/gtest.h>
using namespace Clipper2Lib;
TEST(Clipper2Tests, TestSimplifyPath) {
  Path64 input1 = MakePath({
      0,0, 1,1, 0,20, 0,21, 1,40, 0,41, 0,60, 0,61, 0,80, 1,81, 0,100
  });
  Path64 output1 = SimplifyPath(input1, 2, false);
  EXPECT_EQ(Length(output1), 100);
  EXPECT_EQ(output1.size(), 2);
}