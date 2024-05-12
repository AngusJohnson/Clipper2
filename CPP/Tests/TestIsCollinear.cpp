#include <gtest/gtest.h>
#include "clipper2/clipper.h"

TEST(Clipper2Tests, TestCarryCalculation) {
  EXPECT_EQ(Clipper2Lib::Multiply(0x51eaed81157de061, 0x3a271fb2745b6fe9).carry, 0x129bbebdfae0464e);
  EXPECT_EQ(Clipper2Lib::Multiply(0x3a271fb2745b6fe9, 0x51eaed81157de061).carry, 0x129bbebdfae0464e);
  EXPECT_EQ(Clipper2Lib::Multiply(0xc2055706a62883fa, 0x26c78bc79c2322cc).carry, 0x1d640701d192519b);
  EXPECT_EQ(Clipper2Lib::Multiply(0x26c78bc79c2322cc, 0xc2055706a62883fa).carry, 0x1d640701d192519b);
  EXPECT_EQ(Clipper2Lib::Multiply(0x874ddae32094b0de, 0x9b1559a06fdf83e0).carry, 0x51f76c49563e5bfe);
  EXPECT_EQ(Clipper2Lib::Multiply(0x9b1559a06fdf83e0, 0x874ddae32094b0de).carry, 0x51f76c49563e5bfe);
  EXPECT_EQ(Clipper2Lib::Multiply(0x81fb3ad3636ca900, 0x239c000a982a8da4).carry, 0x12148e28207b83a3);
  EXPECT_EQ(Clipper2Lib::Multiply(0x239c000a982a8da4, 0x81fb3ad3636ca900).carry, 0x12148e28207b83a3);
  EXPECT_EQ(Clipper2Lib::Multiply(0x4be0b4c5d2725c44, 0x990cd6db34a04c30).carry, 0x2d5d1a4183fd6165);
  EXPECT_EQ(Clipper2Lib::Multiply(0x990cd6db34a04c30, 0x4be0b4c5d2725c44).carry, 0x2d5d1a4183fd6165);
  EXPECT_EQ(Clipper2Lib::Multiply(0x978ec0c0433c01f6, 0x2df03d097966b536).carry, 0x1b3251d91fe272a5);
  EXPECT_EQ(Clipper2Lib::Multiply(0x2df03d097966b536, 0x978ec0c0433c01f6).carry, 0x1b3251d91fe272a5);
  EXPECT_EQ(Clipper2Lib::Multiply(0x49c5cbbcfd716344, 0xc489e3b34b007ad3).carry, 0x38a32c74c8c191a4);
  EXPECT_EQ(Clipper2Lib::Multiply(0xc489e3b34b007ad3, 0x49c5cbbcfd716344).carry, 0x38a32c74c8c191a4);
  EXPECT_EQ(Clipper2Lib::Multiply(0xd3361cdbeed655d5, 0x1240da41e324953a).carry, 0x0f0f4fa11e7e8f2a);
  EXPECT_EQ(Clipper2Lib::Multiply(0x1240da41e324953a, 0xd3361cdbeed655d5).carry, 0x0f0f4fa11e7e8f2a);
  EXPECT_EQ(Clipper2Lib::Multiply(0x51b854f8e71b0ae0, 0x6f8d438aae530af5).carry, 0x239c04ee3c8cc248);
  EXPECT_EQ(Clipper2Lib::Multiply(0x6f8d438aae530af5, 0x51b854f8e71b0ae0).carry, 0x239c04ee3c8cc248);
  EXPECT_EQ(Clipper2Lib::Multiply(0xbbecf7dbc6147480, 0xbb0f73d0f82e2236).carry, 0x895170f4e9a216a7);
  EXPECT_EQ(Clipper2Lib::Multiply(0xbb0f73d0f82e2236, 0xbbecf7dbc6147480).carry, 0x895170f4e9a216a7);
}

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
