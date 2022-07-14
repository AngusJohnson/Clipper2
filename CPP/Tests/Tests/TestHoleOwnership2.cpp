#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/ClipFileLoad.h"

using namespace Clipper2Lib;


bool PolyPathFullyContainsChildren(const PolyPath64& pp)
{
  for (auto child : pp.childs())
  {
    for (const Point64& pt : child->polygon())
      if (PointInPolygon(pt, pp.polygon()) == PointInPolygonResult::IsOutside)
        return false;

    if (child->ChildCount() > 0 && !PolyPathFullyContainsChildren(*child))
      return false;
  }
  return true;
}

bool PolytreeFullyContainsChildren(const PolyTree64& polytree)
{
  for (const PolyPath64* child : polytree.childs())
    if (child->ChildCount() > 0 && !PolyPathFullyContainsChildren(*child))
      return false;
  return true;
}

void PolyPathContainsPoint(const PolyPath64& pp, const Point64 pt, int& counter)
{
  if (pp.polygon().size() > 0)
  {
    if (PointInPolygon(pt, pp.polygon()) != PointInPolygonResult::IsOutside)
    {
      if (pp.IsHole()) --counter;
      else  ++counter;
    }
  }
  for (auto child : pp.childs())
    PolyPathContainsPoint(*child, pt, counter);
}

bool PolytreeContainsPoint(const PolyPath64& pp, const Point64 pt)
{
  int counter = 0;
  for (auto child : pp.childs())
    PolyPathContainsPoint(*child, pt, counter);
  EXPECT_GE(counter, 0); //ie 'pt' can't be inside more holes than outers
  return counter != 0;
}

void GetPolyPathArea(const PolyPath64& pp, double& area)
{
  area += Area(pp.polygon());
  for (auto child : pp.childs())
    GetPolyPathArea(*child, area);
}

double GetPolytreeArea(const PolyPath64& pp)
{
  double result = 0;
  for (auto child : pp.childs())
    GetPolyPathArea(*child, result);
  return result;
}

TEST(Clipper2Tests, TestPolytreeHoleOwnership2)
{
#ifdef _WIN32
  std::ifstream ifs("../../../Tests/PolytreeHoleOwner2.txt");
#else
  std::ifstream ifs("PolytreeHoleOwner2.txt");
#endif

  ASSERT_TRUE(ifs);
  ASSERT_TRUE(ifs.good());

  Paths64 subject, subject_open, clip;
  ClipType ct;
  FillRule fr;
  int64_t area, count;

  ASSERT_TRUE(LoadTestNum(ifs, 1, false, subject, subject_open, clip, area, count, ct, fr));

  Point64 point_of_interest_1(21887, 10420);

  std::vector<Point64> points_of_interest_2 = {
      Point64(21887, 10430),
      Point64(21843, 10520)
  };

  // check that the first point of interest is not inside a **subject**
  for (const auto& path : subject)
  {
    const auto result = PointInPolygon(point_of_interest_1, path);
    EXPECT_EQ(result, PointInPolygonResult::IsOutside);
  }

  // check that each of the second points of interest is inside only one **subject**
  for (const auto& point_of_interest_2 : points_of_interest_2)
  {
    int poi2_counter = 0;
    for (const auto& path : subject)
    {
      if (PointInPolygon(point_of_interest_2, path) == PointInPolygonResult::IsInside)
        ++poi2_counter;
    }
    EXPECT_EQ(poi2_counter, 1);
  }

  PolyTree64 solution;
  Paths64 solution_open;
  Clipper64 c;
  c.AddSubject(subject);
  c.AddOpenSubject(subject_open);
  c.AddClip(clip);
  c.Execute(ct, FillRule::Negative, solution, solution_open);

  const auto solution_paths = PolyTreeToPaths(solution);

  ASSERT_FALSE(solution_paths.empty());

  const auto subject_area = Area(subject, true);
  const double polytree_area = GetPolytreeArea(solution);
  const auto solution_paths_area = Area(solution_paths);

  const bool polytree_contains_poi_1 =
    PolytreeContainsPoint(solution, point_of_interest_1);

  // the code below tests:
  // 1. the total area of the solution should slightly smaller than the total area of the subject paths, and
  // 2. the area of the paths returned from PolyTreeToPaths matches the Polytree's area
  // 3. the first "point of interest" should **not** be inside the polytree
  // 4. check that all child polygons are inside their parents
  // 5. each of the second "points of interest" should be inside the polytree

  // 1. check subject vs solution areas
  EXPECT_LT(solution_paths_area, subject_area);
  EXPECT_GT(solution_paths_area, (subject_area * 0.95)); //ie no more than 5% smaller

  // 2. check area from PolyTreeToPaths function matches the polytree's area
  EXPECT_NEAR(polytree_area, solution_paths_area, 0.0001);

  // 3. the first point of interest is inside a hole and hence 
  // should not be inside the solution's filling region
  EXPECT_FALSE(polytree_contains_poi_1);

  // 4. check that all children are inside their parents
  EXPECT_TRUE(PolytreeFullyContainsChildren(solution));

  // 5. check that each of the second points of interest is inside the solution
  for (const auto& point_of_interest_2 : points_of_interest_2)
  {
    const bool polytree_contains_poi_2 =
      PolytreeContainsPoint(solution, point_of_interest_2);
    EXPECT_TRUE(polytree_contains_poi_2);
  }
}