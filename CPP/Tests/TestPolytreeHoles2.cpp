#include <gtest/gtest.h>
#include "clipper.h"
#include "ClipFileLoad.h"

using namespace Clipper2Lib;


void PolyPathContainsPoint(const PolyPath64& pp, const Point64 pt, int& counter)
{
  if (pp.Polygon().size() > 0)
  {
    if (PointInPolygon(pt, pp.Polygon()) != PointInPolygonResult::IsOutside)
    {
      if (pp.IsHole()) --counter;
      else  ++counter;
    }
  }
  for (auto child : pp)
    PolyPathContainsPoint(*child, pt, counter);
}

bool PolytreeContainsPoint(const PolyPath64& pp, const Point64 pt)
{
  int counter = 0;
  for (auto child : pp)
    PolyPathContainsPoint(*child, pt, counter);
  EXPECT_GE(counter, 0); //ie 'pt' can't be inside more holes than outers
  return counter != 0;
}

void GetPolyPathArea(const PolyPath64& pp, double& area)
{
  area += Area(pp.Polygon());
  for (auto child : pp)
    GetPolyPathArea(*child, area);
}

double GetPolytreeArea(const PolyPath64& pp)
{
  double result = 0;
  for (auto child : pp)
    GetPolyPathArea(*child, result);
  return result;
}

TEST(Clipper2Tests, TestPolytreeHoles2)
{
  std::ifstream ifs("PolytreeHoleOwner2.txt");

  ASSERT_TRUE(ifs);
  ASSERT_TRUE(ifs.good());

  Paths64 subject, subject_open, clip;
  ClipType ct;
  FillRule fr;
  int64_t area, count;

  ASSERT_TRUE(LoadTestNum(ifs, 1, subject, subject_open, clip, area, count, ct, fr));

  const std::vector<Point64> points_of_interest_outside = {
     Point64(21887, 10420),
     Point64(21726, 10825),
     Point64(21662, 10845),
     Point64(21617, 10890)
  };

  // confirm that each 'points_of_interest_outside' is outside every subject,
  for (const auto& poi_outside : points_of_interest_outside)
  {
    int outside_subject_count = 0;
    for (const auto& path : subject)
      if (PointInPolygon(poi_outside, path) != PointInPolygonResult::IsOutside)
        ++outside_subject_count;
    EXPECT_EQ(outside_subject_count, 0);
  }

  const std::vector<Point64> points_of_interest_inside = {
     Point64(21887, 10430),
     Point64(21843, 10520),
     Point64(21810, 10686),
     Point64(21900, 10461)
  };

  // confirm that each 'points_of_interest_inside' is inside a subject,
  // and inside only one subject (to exclude possible subject holes)
  for (const auto& poi_inside : points_of_interest_inside)
  {
    int inside_subject_count = 0;
    for (const auto& path : subject)
    {
      if (PointInPolygon(poi_inside, path) != PointInPolygonResult::IsOutside)
        ++inside_subject_count;
    }
    EXPECT_EQ(inside_subject_count, 1);
  }

  PolyTree64 solution_tree;
  Paths64 solution_open;
  Clipper64 c;
  c.AddSubject(subject);
  c.AddOpenSubject(subject_open);
  c.AddClip(clip);
  c.Execute(ct, FillRule::Negative, solution_tree, solution_open);

  const auto solution_paths = PolyTreeToPaths(solution_tree);

  ASSERT_FALSE(solution_paths.empty());

  const double subject_area         = -Area(subject); //negate (see fillrule)
  const double solution_tree_area   = GetPolytreeArea(solution_tree);
  const double solution_paths_area  = Area(solution_paths);

  // 1a. check solution_paths_area  is smaller than subject_area
  EXPECT_LT(solution_paths_area, subject_area);
  // 1b. but not too much smaller
  EXPECT_GT(solution_paths_area, (subject_area * 0.95)); 

  // 2. check solution_tree's area matches solution_paths' area
  EXPECT_NEAR(solution_tree_area, solution_paths_area, 0.0001);

  // 3. check that all children are inside their parents
  EXPECT_TRUE(CheckPolytreeFullyContainsChildren(solution_tree));

  // 4. confirm all 'point_of_interest_outside' are outside polytree
  for (const auto& poi_outside : points_of_interest_outside)
    EXPECT_FALSE(PolytreeContainsPoint(solution_tree, poi_outside));

  // 5. confirm all 'point_of_interest_inside' are inside polytree
  for (const auto& poi_inside : points_of_interest_inside)
    EXPECT_TRUE(PolytreeContainsPoint(solution_tree, poi_inside));

}