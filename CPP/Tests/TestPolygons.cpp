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

template <size_t N>
inline bool IsInList(int num, const int (&intArray)[N])
{  
  const int* list = &intArray[0];
  for (int cnt = N; cnt; --cnt)
    if (num == *list++) return true;
  return false;
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
    int64_t stored_area, stored_count;

    if (!LoadTestNum(ifs, test_number, 
      subject, subject_open, clip, stored_area, stored_count, ct, fr)) break;

    // check Paths64 solutions
    Clipper2Lib::Clipper64 c;
    c.AddSubject(subject);
    c.AddOpenSubject(subject_open);
    c.AddClip(clip);
    c.Execute(ct, fr, solution, solution_open);

    const int64_t measured_area = static_cast<int64_t>(Area(solution));
    const int64_t measured_count = static_cast<int64_t>(solution.size() + solution_open.size());    
    const int64_t count_diff = std::abs(measured_count - stored_count);
    const int64_t area_diff = std::abs(measured_area - stored_area);

    // check the polytree variant too
    Clipper2Lib::PolyTree64 solution_polytree;
    Clipper2Lib::Paths64 solution_polytree_open;
    Clipper2Lib::Clipper64 clipper_polytree;
    clipper_polytree.AddSubject(subject);
    clipper_polytree.AddOpenSubject(subject_open);
    clipper_polytree.AddClip(clip);
    clipper_polytree.Execute(ct, fr, solution_polytree, solution_polytree_open);
    
    const int64_t measured_area_pt = 
      static_cast<int64_t>(solution_polytree.Area());
    const auto solution_polytree_paths = PolyTreeToPaths(solution_polytree);
    const int64_t measured_count_pt = static_cast<int64_t>(solution_polytree_paths.size());

    if (test_number == 23)
    {
      EXPECT_LE(count_diff, 4);
    }
    else if (test_number == 27)
    {
      EXPECT_LE(count_diff, 2);
    }
    else if (IsInList(test_number, 
      { 18, 32, 42, 43, 45, 87, 102, 103, 111, 118, 183 }))
    {
      EXPECT_LE(count_diff, 1);
    }
    else if (test_number >= 120)
    {
      if (stored_count > 0)
        EXPECT_LE(count_diff/ stored_count, 0.02);
    }
    else if (stored_count > 0) 
      EXPECT_EQ(count_diff, 0);

    if (IsInList(test_number,
      { 22, 23, 24 }))
    {
      EXPECT_LE(area_diff, 8);
    }
    else if (stored_area > 0 && area_diff > 100)
    {
      EXPECT_LE(area_diff/stored_area, 0.02);
    }

    EXPECT_EQ(measured_area, measured_area_pt);
    EXPECT_EQ(measured_count, measured_count_pt);

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