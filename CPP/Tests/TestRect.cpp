#include <gtest/gtest.h>
#include "clipper2/clipper.core.h"

using namespace Clipper2Lib;

TEST(Clipper2Tests, TestRectOpPlus)
{
  {
    Rect64 lhs = Rect64::InvalidRect();
    Rect64 rhs(-1, -1, 10, 10);
    {
      Rect64 sum = lhs + rhs;
      EXPECT_EQ(rhs, sum);
    }
    {
      std::swap(lhs, rhs);
      Rect64 sum = lhs + rhs;
      EXPECT_EQ(lhs, sum);
    }
  }
  {
    Rect64 lhs = Rect64::InvalidRect();
    Rect64 rhs(1, 1, 10, 10);
    {
      Rect64 sum = lhs + rhs;
      EXPECT_EQ(rhs, sum);
    }
    {
      std::swap(lhs, rhs);
      Rect64 sum = lhs + rhs;
      EXPECT_EQ(lhs, sum);
    }
  }
  {
    Rect64 lhs(0, 0, 1, 1);
    Rect64 rhs(-1, -1, 0, 0);
    Rect64 expected(-1, -1, 1, 1);
    {
      Rect64 sum = lhs + rhs;
      EXPECT_EQ(expected, sum);
    }
    {
      std::swap(lhs, rhs);
      Rect64 sum = lhs + rhs;
      EXPECT_EQ(expected, sum);
    }
  }
  {
    Rect64 lhs(-10, -10, -1, -1);
    Rect64 rhs(1, 1, 10, 10);
    Rect64 expected(-10, -10, 10, 10);
    {
      Rect64 sum = lhs + rhs;
      EXPECT_EQ(expected, sum);
    }
    {
      std::swap(lhs, rhs);
      Rect64 sum = lhs + rhs;
      EXPECT_EQ(expected, sum);
    }
  }
}