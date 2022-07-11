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

  Point64 point_of_interest(21887, 10420);

  // check that the point of interest is not inside any subject
  for (const auto& path : subject) {
    const auto result = PointInPolygon(point_of_interest, path);
    EXPECT_EQ(result, PointInPolygonResult::IsOutside);
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

  const bool polytree_contains_poi =
    PolytreeContainsPoint(solution, point_of_interest);

  // the code below tests:
  // 1. the total area of the solution should slightly smaller than the total area of the subject paths, and
  // 2. the area of the paths returned from PolyTreeToPaths matches the Polytree's area
  // 3. the "point of interest" should **not** be inside the polytree
  // 4. check that all child polygons are inside their parents

  // 1. check subject vs solution areas
  EXPECT_LT(solution_paths_area, subject_area);
  EXPECT_GT(solution_paths_area, (subject_area * 0.98)); //ie no more than 2% smaller
  
  // 2. check area from PolyTreeToPaths function matches the polytree's area
  EXPECT_NEAR(polytree_area, solution_paths_area, 0.0001);

  // 3. check that the point of interest was inside a hole and hence 
  // the point of interest is not inside the solution's filling region
  EXPECT_FALSE(polytree_contains_poi);

  // 4. check that all children are inside their parents
  EXPECT_TRUE(PolytreeFullyContainsChildren(solution));
}