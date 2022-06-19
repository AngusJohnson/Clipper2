#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"

using namespace Clipper2Lib;

TEST(Clipper2Tests, TestTrimCollinear) {
      
  Path64 input1 = MakePath("10,10, 10,10, 50,10, 100,10, 100,100, 10,100, 10,10, 20,10");
  Path64 output1 = TrimCollinear(input1, false);
  EXPECT_EQ(output1.size(), 4);

  Path64 input2 = MakePath("10,10, 10,10, 100,10, 100,100, 10,100, 10,10, 10,10");
  Path64 output2 = TrimCollinear(input2, true);
  EXPECT_EQ(output2.size(), 5);

  Path64 input3 = MakePath("10,10, 10,50, 10,10, 50,10, \
    50,50, 50,10, 100,10, 100,100, 100,10");
  Path64 output3 = TrimCollinear(input3);
  EXPECT_EQ(output3.size(), 0);

}