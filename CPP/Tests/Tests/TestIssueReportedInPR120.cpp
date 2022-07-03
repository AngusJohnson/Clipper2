#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/ClipFileLoad.h"

TEST(Clipper2Tests, TestIssueReportedInPR120)
{
#ifdef _WIN32
  std::ifstream ifs("../../../Tests/PR120.txt");
#else
  std::ifstream ifs("PR120.txt");
#endif

  ASSERT_TRUE(ifs);
  ASSERT_TRUE(ifs.good());

  Clipper2Lib::Paths64 subject, subject_open, clip;
  Clipper2Lib::ClipType ct;
  Clipper2Lib::FillRule fr;
  int64_t area, count;

  ASSERT_TRUE(LoadTestNum(ifs, 1, false, subject, subject_open, clip, area, count, ct, fr));

  Clipper2Lib::Point64 point_of_interest(21887, 10420);

  // check that the point of interest is not inside any subject
  for (const auto& path : subject) {
    const auto result = Clipper2Lib::PointInPolygon(point_of_interest, path);
    EXPECT_EQ(result, Clipper2Lib::PointInPolyResult::IsOutside);
  }

  Clipper2Lib::PolyTree64 solution;
  Clipper2Lib::Paths64 solution_open;
  Clipper2Lib::Clipper64 c;
  c.AddSubject(subject);
  c.AddOpenSubject(subject_open);
  c.AddClip(clip);
  c.Execute(ct, Clipper2Lib::FillRule::Negative, solution, solution_open);

  const auto solution_paths = PolyTreeToPaths(solution);

  ASSERT_FALSE(solution_paths.empty());

  const auto subject_area = Clipper2Lib::Area(subject, true);
  const auto solution_paths_area = Clipper2Lib::Area(solution_paths);

  // the total area of the union should not be greater than the total area of individual subjects
  EXPECT_LE(solution_paths_area, subject_area);

  // the below code iterates the solution, testing two separate things:
  // 1. the total area of the tree solution should be similar to the total area of the paths solution, and
  // 2. the "point of interest" defined above should be inside as many holes as non-holes

  double solution_tree_area = 0;     // 1. initialize accumulator for the total area
  int point_of_interest_counter = 0; // 2. initialize the "point of interest" counter

  std::deque<const Clipper2Lib::PolyTree64*> queue;

  queue.push_back(&solution); // initialize the iteration

  while (!queue.empty()) {
    const auto* polytree = queue.front();
    queue.pop_front();

    const auto& polygon = polytree->polygon();
    if (!polygon.empty()) {
      // it looks like this IsHole() call does not give the desired results
      const auto is_hole = polytree->IsHole();
      // interestingly, using the below line instead of IsHole() as above appears to help:
      // const auto is_hole = !Clipper2Lib::IsPositive(polygon);

      // 1. calculate area
      const auto polygon_area = std::abs(Clipper2Lib::Area(polygon));
      solution_tree_area += is_hole ? -polygon_area : polygon_area;

      // 2. see if the point of interest is inside this path
      const auto point_in_polygon_result = Clipper2Lib::PointInPolygon(point_of_interest, polygon);
      if (point_in_polygon_result == Clipper2Lib::PointInPolyResult::IsInside) {
        point_of_interest_counter += is_hole ? -1 : 1;
      }
    }

    for (const auto* child : polytree->childs()) {
      queue.push_back(child);
    }
  }

  // 1. check the areas
  EXPECT_LT(solution_tree_area, subject_area);
  EXPECT_NEAR(solution_tree_area, solution_paths_area, 0.0);

  // 2. check that the point of interest was inside as many holes as non-holes
  EXPECT_EQ(point_of_interest_counter, 0);
}