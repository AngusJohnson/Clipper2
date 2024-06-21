#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include "ClipFileLoad.h"

using Clipper2Lib::Scalar;

inline Clipper2Lib::PathS MakeRandomPath(int width, int height, unsigned vertCnt)
{
  Clipper2Lib::PathS result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Clipper2Lib::PointS(Scalar(rand()) / RAND_MAX * width, Scalar(rand()) / RAND_MAX * height));
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
  ASSERT_TRUE(ifs.good());
  const int start_num = 1;
  const int end_num = 1000;
  int test_number = start_num;
  while (test_number <= end_num)
  {
    Clipper2Lib::PathsI subject, subject_open, clip;
    Clipper2Lib::PathsI solution, solution_open;
    Clipper2Lib::ClipType ct;
    Clipper2Lib::FillRule fr;
    Integer stored_area, stored_count;
    if (!LoadTestNum(ifs, test_number,
      subject, subject_open, clip, stored_area, stored_count, ct, fr)) break;
    // check PathsI solutions
    Clipper2Lib::ClipperI c;
    c.AddSubject(subject);
    c.AddOpenSubject(subject_open);
    c.AddClip(clip);
    c.Execute(ct, fr, solution, solution_open);
    const Integer measured_area = static_cast<Integer>(Area(solution));
    const Integer measured_count = static_cast<Integer>(solution.size() + solution_open.size());
    // check the polytree variant too
    Clipper2Lib::PolyTreeI solution_polytree;
    Clipper2Lib::PathsI solution_polytree_open;
    Clipper2Lib::ClipperI clipper_polytree;
    clipper_polytree.AddSubject(subject);
    clipper_polytree.AddOpenSubject(subject_open);
    clipper_polytree.AddClip(clip);
    clipper_polytree.Execute(ct, fr, solution_polytree, solution_polytree_open);
    const Integer measured_area_polytree =
      static_cast<Integer>(solution_polytree.Area());
    const auto solution_polytree_paths = PolyTreeToPathsI(solution_polytree);
    const Integer measured_count_polytree =
      static_cast<Integer>(solution_polytree_paths.size());
    // check polygon counts
    if (stored_count <= 0)
      ; // skip count
    else if (IsInList(test_number, { 120, 121, 130, 138,
      140, 148, 163, 165, 166, 167, 168, 172, 175, 178, 180 }))
      EXPECT_NEAR(measured_count, stored_count, 5) << " in test " << test_number;
    else if (IsInList(test_number, { 27, 181 }))
      EXPECT_NEAR(measured_count, stored_count, 2) << " in test " << test_number;
    else if (test_number >= 120 && test_number <= 184)
      EXPECT_NEAR(measured_count, stored_count, 2) << " in test " << test_number;
    else if (IsInList(test_number, { 23, 45, 87, 102, 111, 113, 191 }))
      EXPECT_NEAR(measured_count, stored_count, 1) << " in test " << test_number;
    else
      EXPECT_EQ(measured_count, stored_count) << " in test " << test_number;
    // check polygon areas
    if (stored_area <= 0)
      ; // skip area
    else if (IsInList(test_number, { 19, 22, 23, 24 }))
      EXPECT_NEAR(measured_area, stored_area, 0.5 * measured_area) << " in test " << test_number;
    else if (test_number == 193)
      EXPECT_NEAR(measured_area, stored_area, 0.2 * measured_area) << " in test " << test_number;
    else if (test_number == 63)
      EXPECT_NEAR(measured_area, stored_area, 0.1 * measured_area) << " in test " << test_number;
    else if (test_number == 16)
      EXPECT_NEAR(measured_area, stored_area, 0.075 * measured_area) << " in test " << test_number;
    else if (test_number == 26)
      EXPECT_NEAR(measured_area, stored_area, 0.05 * measured_area) << " in test " << test_number;
    else if (IsInList(test_number, { 15, 52, 53, 54, 59, 60, 64, 117, 119, 184 }))
      EXPECT_NEAR(measured_area, stored_area, 0.02 * measured_area) << " in test " << test_number;
    else
      EXPECT_NEAR(measured_area, stored_area, 0.01 * measured_area) << " in test " << test_number;
    EXPECT_EQ(measured_count, measured_count_polytree)
      << " in test " << test_number;
    EXPECT_EQ(measured_area, measured_area_polytree)
      << " in test " << test_number;
    ++test_number;
  }
  //EXPECT_GE(test_number, 188);
  Clipper2Lib::PathsS subjd, clipd, solutiond;
  Clipper2Lib::FillRule frd = Clipper2Lib::FillRule::NonZero;
}
TEST(Clipper2Tests, TestHorzSpikes) //#720
{
  Clipper2Lib::PathsI paths = {
    Clipper2Lib::MakePath({1600,0, 1600,100, 2050,100, 2050,300, 450,300, 450, 0}),
    Clipper2Lib::MakePath({1800,200, 1800,100, 1600,100, 2000,100, 2000,200}) };
  std::cout << paths << std::endl;
  Clipper2Lib::ClipperI c;
  c.AddSubject(paths);
  c.Execute(Clipper2Lib::ClipType::Union, Clipper2Lib::FillRule::NonZero, paths);
  EXPECT_GE(paths.size(), 1);
}

TEST(Clipper2Tests, TestCollinearOnMacOs) //#777
{
  Clipper2Lib::PathsI subject;
  subject.push_back(Clipper2Lib::MakePath({ 0, -453054451,0, -433253797,-455550000, 0 }));
  subject.push_back(Clipper2Lib::MakePath({ 0, -433253797,0, 0,-455550000, 0 }));
  Clipper2Lib::ClipperI clipper;
  clipper.PreserveCollinear(false);
  clipper.AddSubject(subject);
  Clipper2Lib::PathsI solution;
  clipper.Execute(Clipper2Lib::ClipType::Union, Clipper2Lib::FillRule::NonZero, solution);
  ASSERT_EQ(solution.size(), 1);
  EXPECT_EQ(solution[0].size(), 3);
  EXPECT_EQ(IsPositive(subject[0]), IsPositive(solution[0]));
}