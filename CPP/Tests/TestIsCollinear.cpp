#include <gtest/gtest.h>
#include "clipper2/clipper.h"

TEST(Clipper2Tests, TestIsCollinear) {
  // a large integer not representable by double
  const int64_t i = 9007199254740993;

  const Clipper2Lib::Point64 pt1(0, 0);
  const Clipper2Lib::Point64 sharedPt(i, i * 10);
  const Clipper2Lib::Point64 pt2(i * 10, i * 100);

  EXPECT_TRUE(IsCollinear(pt1, sharedPt, pt2));
}

TEST(Clipper2Tests, TestIsCollinear2) {
  // see https://github.com/AngusJohnson/Clipper2/issues/831
  const int64_t i = 0x4000000000000;
  const Clipper2Lib::Path64 subject = {
    Clipper2Lib::Point64(-i, -i),
    Clipper2Lib::Point64( i, -i),
    Clipper2Lib::Point64(-i,  i),
    Clipper2Lib::Point64( i,  i)
  };
  Clipper2Lib::Clipper64 clipper;
  clipper.AddSubject({ subject });
  Clipper2Lib::Paths64 solution;
  clipper.Execute(Clipper2Lib::ClipType::Union, Clipper2Lib::FillRule::EvenOdd, solution);
  EXPECT_EQ(solution.size(), 2);
}
