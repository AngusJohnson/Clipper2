#include <gtest/gtest.h>
#include "clipper.h"

using namespace Clipper2Lib;

TEST(Clipper2Tests, TestPointInit)
{
    Point64 a(0, 1);
    Point64 b = {0, 1};
    Point64 c = a;
    Point64 d(a);
    Point64 e; // Compiles
    // Point64 f(1); // Doesn't compile, because I can't know what to expect? both X&Y are 1? or just X?

    auto create = [](int64_t x, int64_t y) -> Point64
    { return {x, y}; };
    Point64 g = create(0, 1);

    ASSERT_EQ(a, b);
    ASSERT_EQ(b, c);
    ASSERT_EQ(c, d);
    ASSERT_NE(d, e);
    ASSERT_EQ(d, g);
}

TEST(Clipper2Tests, TestPathInit)
{
    Path64 a = {{0, 0}, {100, 100}};
    Path64 b({{0, 0}, {100, 100}});
    Path64 c(b);
    Path64 d = c;
    Path64 e; // compiles

    ASSERT_EQ(a, b);
    ASSERT_EQ(b, c);
    ASSERT_EQ(c, d);
}


TEST(Clipper2Tests, TestPathsInit)
{
    Paths64 a = {{{0, 0}, {100, 100}}};
    Paths64 b({{{0, 0}, {100, 100}}});
    Paths64 c(b);
    Paths64 d = c;
    Paths64 e; // compiles

    ASSERT_EQ(a, b);
    ASSERT_EQ(b, c);
    ASSERT_EQ(c, d);
}