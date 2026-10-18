#include <gtest/gtest.h>
#include <algorithm>
#include <array>
#include <cstdlib>

#include "clipper2/clipper.h"
using namespace Clipper2Lib;

namespace {

struct BoundaryTransform {
  const char* name;
  bool transpose;
  bool reflect_x;
  bool reflect_y;
  bool reverse;
  int64_t dx;
  int64_t dy;
};

struct BoundaryCase {
  const char* name;
  Rect64 rect;
  Path64 subject;
  Point64 vertical_hit;
  Point64 horizontal_hit;
  Point64 corner;
};

bool IsOnVerticalBoundary(const Point64& pt, const Rect64& rect)
{
  return (pt.x == rect.left || pt.x == rect.right) &&
    pt.y > rect.top && pt.y < rect.bottom;
}

bool IsOnHorizontalBoundary(const Point64& pt, const Rect64& rect)
{
  return (pt.y == rect.top || pt.y == rect.bottom) &&
    pt.x > rect.left && pt.x < rect.right;
}

Point64 ApplyTransform(Point64 pt, Rect64 rect, const BoundaryTransform& t)
{
  if (t.transpose)
  {
    pt = Point64(pt.y, pt.x);
    rect = Rect64(rect.top, rect.left, rect.bottom, rect.right);
  }
  if (t.reflect_x) pt.x = rect.left + rect.right - pt.x;
  if (t.reflect_y) pt.y = rect.top + rect.bottom - pt.y;
  return Point64(pt.x + t.dx, pt.y + t.dy);
}

Rect64 ApplyTransform(Rect64 rect, const BoundaryTransform& t)
{
  if (t.transpose)
    rect = Rect64(rect.top, rect.left, rect.bottom, rect.right);
  if (t.dx || t.dy)
  {
    rect.left += t.dx;
    rect.top += t.dy;
    rect.right += t.dx;
    rect.bottom += t.dy;
  }
  return rect;
}

BoundaryCase MakeBoundaryCase(const BoundaryTransform& t)
{
  const Rect64 base_rect(19669914, -390330085, 24669914, -385330085);
  const Path64 base_subject{
    Point64(8821540, -379573701),
    Point64(27274660, -333900624),
    Point64(72947737, -352353745),
    Point64(54494616, -398026821),
  };
  BoundaryCase result{ t.name, ApplyTransform(base_rect, t), Path64(), Point64(), Point64(),
    ApplyTransform(Point64(24669914, -385330085), base_rect, t) };
  const Point64 hit1 = ApplyTransform(Point64(24669914, -385976859), base_rect, t);
  const Point64 hit2 = ApplyTransform(Point64(23069090, -385330085), base_rect, t);
  result.vertical_hit = IsOnVerticalBoundary(hit1, result.rect) ? hit1 : hit2;
  result.horizontal_hit = IsOnHorizontalBoundary(hit1, result.rect) ? hit1 : hit2;
  result.subject.reserve(base_subject.size());
  for (const Point64& pt : base_subject)
    result.subject.push_back(ApplyTransform(pt, base_rect, t));
  if (t.reverse) std::reverse(result.subject.begin(), result.subject.end());
  return result;
}

// Rounding after reflection or in high-precision mode can change the free
// coordinate by one unit; the coordinate on the rectangle side must be exact.
bool NearVerticalHit(const Point64& actual, const Point64& expected)
{
  return actual.x == expected.x &&
    std::llabs(actual.y - expected.y) <= 1;
}

bool NearHorizontalHit(const Point64& actual, const Point64& expected)
{
  return actual.y == expected.y &&
    std::llabs(actual.x - expected.x) <= 1;
}

void ExpectBoundaryClip(const Paths64& solution,
  const BoundaryCase& tc, bool is_polygon)
{
  ASSERT_EQ(solution.size(), 1u) << tc.name;
  const Path64& path = solution.front();
  ASSERT_EQ(path.size(), is_polygon ? 3u : 2u) << tc.name;

  size_t vertical_hits = 0, horizontal_hits = 0, corner_hits = 0;
  for (const Point64& pt : path)
  {
    if (NearVerticalHit(pt, tc.vertical_hit)) ++vertical_hits;
    else if (NearHorizontalHit(pt, tc.horizontal_hit)) ++horizontal_hits;
    else if (is_polygon && pt == tc.corner) ++corner_hits;
    else ADD_FAILURE() << tc.name;
  }
  EXPECT_EQ(vertical_hits, 1u) << tc.name;
  EXPECT_EQ(horizontal_hits, 1u) << tc.name;
  if (is_polygon) EXPECT_EQ(corner_hits, 1u) << tc.name;
}

constexpr std::array<BoundaryTransform, 5> kBoundaryTransforms{{
  { "seed", false, false, false, false, 0, 0 },
  { "reversed", false, false, false, true, 0, 0 },
  { "reflect-x", false, true, false, false, 0, 0 },
  { "transposed-translated", true, false, false, false, 7000000, -9000000 },
  { "transposed-reflect-y-translated", true, false, true, false, 7000000, -9000000 },
}};

} // namespace

TEST(Clipper2Tests, TestRectClip)
{
  Paths64 sub, clp, sol;
  Rect64 rect = Rect64(100, 100, 700, 500);
  clp.push_back(rect.AsPath());
  sub.push_back(MakePath({ 100,100, 700,100, 700,500, 100,500 }));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(sub));
  sub.clear();
  sub.push_back(MakePath({ 110,110, 700,100, 700,500, 100,500 }));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(sub));
  sub.clear();
  sub.push_back(MakePath({ 90,90, 700,100, 700,500, 100,500 }));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(clp));
  sub.clear();
  sub.push_back(MakePath({ 110,110, 690,110, 690,490, 110,490 }));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(sub));
  sub.clear();
  clp.clear();
  rect = Rect64(390, 290, 410, 310);
  clp.push_back(rect.AsPath());
  sub.push_back(MakePath({ 410,290, 500,290, 500,310, 410,310 }));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(sol.empty());
  sub.clear();
  sub.push_back(MakePath({ 430,290, 470,330, 390,330 }));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(sol.empty());
  sub.clear();
  sub.push_back(MakePath({ 450,290, 480,330, 450,330 }));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(sol.empty());
  sub.clear();
  sub.push_back(MakePath({ 208,66, 366,112, 402,303,
    234,332, 233,262, 243,140, 215,126, 40,172 }));
  rect = Rect64(237, 164, 322, 248);
  sol = RectClip(rect, sub);
  const auto solBounds = GetBounds(sol);
  EXPECT_EQ(solBounds.Width(), rect.Width());
  EXPECT_EQ(solBounds.Height(), rect.Height());
}
TEST(Clipper2Tests, TestRectClip2) //#597
{
  Clipper2Lib::Rect64 rect(54690, 0, 65628, 6000);
  Clipper2Lib::Paths64 subject {{{700000, 6000}, { 0, 6000 }, { 0, 5925 }, { 700000, 5925 }}};
  Clipper2Lib::Paths64 solution = Clipper2Lib::RectClip(rect, subject);
  //std::cout << solution << std::endl;
  EXPECT_TRUE(solution.size() == 1 && solution[0].size() == 4);
}
TEST(Clipper2Tests, TestRectClip3) //#637
{
  Rect64 r(-1800000000LL, -137573171LL, -1741475021LL, 3355443LL);
  Paths64 subject, clip, solution;
  subject.push_back(MakePath({ -1800000000LL, 10005000LL,
    -1800000000LL, -5000LL, -1789994999LL, -5000LL, -1789994999LL, 10005000LL }));
  clip.push_back(r.AsPath());
  solution = RectClip(r, subject);
  //std::cout << solution << std::endl;
  EXPECT_TRUE(solution.size() == 1);
}

TEST(Clipper2Tests, TestRectClipOrientation) //#864
{
  const Rect64 rect(1222, 1323, 3247, 3348);
  const Path64 subject = MakePath({ 375,1680, 1915,4716, 5943,586, 3987,152 });
  RectClip64 clip(rect);
  const auto solution = clip.Execute({ subject });
  ASSERT_EQ(solution.size(), 1);
  EXPECT_EQ(IsPositive(subject), IsPositive(solution.front()));
}

TEST(Clipper2Tests, TestRectClipBoundaryIntersections) //#1095
{
  for (const auto& t : kBoundaryTransforms)
  {
    const BoundaryCase tc = MakeBoundaryCase(t);
    SCOPED_TRACE(tc.name);
    const Paths64 solution = RectClip(tc.rect, Paths64{ tc.subject });
    ASSERT_NO_FATAL_FAILURE(ExpectBoundaryClip(solution, tc, true));
    EXPECT_EQ(IsPositive(tc.subject), IsPositive(solution.front())) << tc.name;
  }
}

TEST(Clipper2Tests, TestRectClipLinesBoundaryIntersections) //#1095
{
  for (const auto& t : kBoundaryTransforms)
  {
    const BoundaryCase tc = MakeBoundaryCase(t);
    SCOPED_TRACE(tc.name);
    const Path64 edge{ tc.subject.back(), tc.subject.front() };
    const Paths64 solution = RectClipLines(tc.rect, edge);
    ASSERT_NO_FATAL_FAILURE(ExpectBoundaryClip(solution, tc, false));
  }
}

TEST(Clipper2Tests, TestRectClipLinesControls)
{
  const Rect64 rect(10, 10, 20, 20);

  EXPECT_TRUE(RectClipLines(rect, Path64{ Point64(0, 0), Point64(10, 10) }).empty());
  EXPECT_TRUE(RectClipLines(rect, Path64{ Point64(0, 5), Point64(30, 5) }).empty());

  const Paths64 corners = RectClipLines(rect, Path64{ Point64(0, 0), Point64(30, 30) });
  ASSERT_EQ(corners.size(), 1u);
  EXPECT_EQ(corners.front(), (Path64{ Point64(10, 10), Point64(20, 20) }));

  const Paths64 boundary = RectClipLines(rect, Path64{ Point64(20, 12), Point64(20, 18) });
  ASSERT_EQ(boundary.size(), 1u);
  EXPECT_EQ(boundary.front(), (Path64{ Point64(20, 12), Point64(20, 18) }));

  EXPECT_TRUE(RectClipLines(Rect64(10, 10, 10, 20),
    Path64{ Point64(5, 15), Point64(15, 15) }).empty());
  EXPECT_TRUE(RectClip(Rect64(10, 10, 20, 10),
    Paths64{ Path64{ Point64(5, 5), Point64(25, 5), Point64(25, 15) } }).empty());
}
