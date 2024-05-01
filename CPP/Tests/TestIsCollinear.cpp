#include <gtest/gtest.h>
#include "clipper2/clipper.core.h"

TEST(Clipper2Tests, TestIsCollinear) {
  // a large integer not representable by double
  const int64_t i = 9007199254740993;

  const Clipper2Lib::Point64 pt1(0, 0);
  const Clipper2Lib::Point64 sharedPt(i, i * 10);
  const Clipper2Lib::Point64 pt2(i * 10, i * 100);

  EXPECT_TRUE(IsCollinear(pt1, sharedPt, pt2));
}
