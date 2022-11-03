#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include "ClipFileLoad.h"
#include <fstream> 

//#define log_tests
#ifdef log_tests
void Log(std::ofstream& os, int testnum, int count, int count_diff,
  int area, int area_diff, double apd)
{
  os << "test: " << testnum << ", count: " << count << " (" <<
    count_diff << "), area: " << area << " (" << area_diff <<
    ") " << apd << std::endl;
}
#endif

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

#ifdef log_tests
  std::ofstream log("../polygon.log");
  log << std::setprecision(4);
#endif


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
    const int64_t count_diff = stored_count <= 0 ? 0 : std::abs(measured_count - stored_count);
    const int64_t area_diff = stored_area <= 0 ? 0 : std::abs(measured_area - stored_area);
    double area_diff_ratio = (area_diff == 0) ? 0 : std::fabs((double)(area_diff) / measured_area);
    
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
    const auto solution_polytree_paths = PolyTreeToPaths64(solution_polytree);
    const int64_t measured_count_pt = static_cast<int64_t>(solution_polytree_paths.size());

#ifdef log_tests
    bool log_this_test = false;
#endif

    // check polygon counts
    if (test_number == 27)
    {
      EXPECT_LE(count_diff, 2);
    }    
    else if (IsInList(test_number, { 37, 43, 87, 102, 111, 118, 183 }))
    {
      EXPECT_LE(count_diff, 1);
    }
    else if (test_number >= 120)
    {
      if (stored_count > 0)
      {
#ifdef log_tests
        if ((double)count_diff / stored_count > 0.025)
          log_this_test = true;
#endif
        EXPECT_LE((double)count_diff / stored_count, 0.025);
      }
    }
    else if (stored_count > 0)
    {
      EXPECT_EQ(count_diff, 0);
    }

    // check polygon areas
    if (IsInList(test_number,
      { 22, 23, 24 }))
    {
      EXPECT_LE(area_diff, 8);
    }
    else if (stored_area > 0 && area_diff > 100)
    {
#ifdef log_tests
      if ((double)area_diff / stored_area > 0.005)
        log_this_test = true;
#endif
      EXPECT_LE((double)area_diff/stored_area, 0.005);
    }

    EXPECT_EQ(measured_area, measured_area_pt);
    EXPECT_EQ(measured_count, measured_count_pt);

#ifdef log_tests
    if (log_this_test)
      Log(log, test_number, measured_count, count_diff,
        measured_area, area_diff, area_diff_ratio);
#endif

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