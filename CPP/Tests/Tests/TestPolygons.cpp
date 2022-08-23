#include <gtest/gtest.h>
#include "clipper.h"
#include "ClipFileLoad.h"

inline Clipper2Lib::PathD MakeRandomPath(int width, int height, unsigned vertCnt)
{
  Clipper2Lib::PathD result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Clipper2Lib::PointD(double(rand()) / RAND_MAX * width, double(rand()) / RAND_MAX * height));
  return result;
}

TEST(Clipper2Tests, TestMultiplePolygons)
{
  std::ifstream ifs("Polygons.txt");


  ASSERT_TRUE(ifs);
  ASSERT_TRUE(ifs.good());

  int test_number = 1;

  while (true)
  {
    Clipper2Lib::Paths64 subject, subject_open, clip;
    Clipper2Lib::Paths64 solution, solution_open;
    Clipper2Lib::ClipType ct;
    Clipper2Lib::FillRule fr;
    int64_t area, count;

    if (!LoadTestNum(ifs, test_number, 
      subject, subject_open, clip, area, count, ct, fr)) break;
   
    Clipper2Lib::Clipper64 c;
    c.AddSubject(subject);
    c.AddOpenSubject(subject_open);
    c.AddClip(clip);
    c.Execute(ct, fr, solution, solution_open);

    const int64_t area2 = static_cast<int64_t>(Area(solution));
    const int64_t count2 = static_cast<int64_t>(solution.size() + solution_open.size());
    const int64_t count_diff = std::abs(count2 - count);
    const int64_t area_diff = std::abs(area2 - area);
    const double relative_count_diff = count ? count_diff / static_cast<double>(count) : 0;
    const double relative_area_diff = area ? area_diff / static_cast<double>(area) : 0;

    if (test_number == 1)
    {
      EXPECT_EQ(count_diff, 0);
      EXPECT_EQ(area_diff, 0);
      EXPECT_EQ(solution.size(), 1);
      //clipping solutions should always be positive irrespective of orientation
      EXPECT_TRUE(IsPositive(solution[0]));
    }
    else if (test_number < 7 || test_number == 8 || test_number == 10)
    {
      EXPECT_EQ(count, count2);
      EXPECT_EQ(area, area2);
    }
    else if (test_number < 14)
    {
      EXPECT_EQ(count, count2);
      EXPECT_LE(count_diff, 1);
      EXPECT_LE(relative_count_diff, 0.01);
    }
    else if (test_number == 22)
    {
      EXPECT_LE(count2, 2);
      EXPECT_LE(area2, 2);
    }
    else if (test_number == 23)
    {
      EXPECT_LE(count2, 1);
      EXPECT_LE(area2, 2);
    }
    else if (test_number == 27)
    {
      EXPECT_EQ(count_diff, 2);
      EXPECT_EQ(area, area2);
    }
    else if (test_number == 53 || test_number == 54)
    {
      EXPECT_EQ(count, count2);
      EXPECT_LE(relative_area_diff, 0.0056);
    }
    else if (test_number == 64 || test_number == 94)
    {
      EXPECT_EQ(count, count2);
      EXPECT_LE(relative_area_diff, 0.014);
    }
    else if (test_number == 66)
    {
      EXPECT_EQ(count, count2);
      EXPECT_LE(relative_area_diff, 0.022);
    }
    else if (test_number == 102)
    {
      EXPECT_LE(count_diff, 1);
      EXPECT_EQ(area_diff, 0);
    }
    else if (test_number < 160)
    {
      if (count > 50) EXPECT_LE(relative_count_diff, 0.02);
      else EXPECT_LE(count_diff, 2);
      if (area > 0)  EXPECT_LE(relative_area_diff, 0.035);
      else           EXPECT_EQ(area, 0);
    }
    else if (test_number == 168)
    {
      EXPECT_LE(count_diff, 9);
      EXPECT_LE(relative_count_diff, 0.1);
      EXPECT_LE(relative_area_diff, 0.0005);
    }
    else if (test_number == 183)
    {
      EXPECT_LE(count_diff, 2);
      EXPECT_EQ(area_diff, 0);
    }
    else
    {
      EXPECT_LE(count_diff, 8);
      if (count_diff > 1) 
        EXPECT_LE(relative_count_diff, 0.1);
      EXPECT_LE(relative_area_diff, 0.0005);
    }

    // Make sure that the polytree variant gives results similar to the paths-only version.
    Clipper2Lib::PolyTree64 solution_polytree;
    Clipper2Lib::Paths64 solution_polytree_open;
    Clipper2Lib::Clipper64 clipper_polytree;
    clipper_polytree.AddSubject(subject);
    clipper_polytree.AddOpenSubject(subject_open);
    clipper_polytree.AddClip(clip);
    clipper_polytree.Execute(ct, fr, solution_polytree, solution_polytree_open);

    const auto solution_polytree_paths = PolyTreeToPaths(solution_polytree);

    const int64_t area3 = static_cast<int64_t>(Area(solution_polytree_paths));
    const auto count3 = solution_polytree_paths.size() + solution_polytree_open.size();

    EXPECT_EQ(area2, area3);
    EXPECT_EQ(count2, count3);

    ++test_number;
  }
  EXPECT_GE(test_number, 188);


  Clipper2Lib::PathsD subjd, clipd, solutiond;
  Clipper2Lib::FillRule frd = Clipper2Lib::FillRule::NonZero;

  subjd.push_back(MakeRandomPath(800, 600, 100));
  clipd.push_back(MakeRandomPath(800, 600, 100));
  solutiond = Clipper2Lib::Intersect(subjd, clipd, Clipper2Lib::FillRule::NonZero);
  EXPECT_GE(solutiond.size(), 1);

}