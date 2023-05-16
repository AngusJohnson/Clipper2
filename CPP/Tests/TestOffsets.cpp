#include <gtest/gtest.h>
#include "clipper2/clipper.offset.h"
#include "ClipFileLoad.h"
#include <algorithm>

TEST(Clipper2Tests, TestOffsets) { 

  std::ifstream ifs("Offsets.txt");

  for (int test_number = 1; test_number <= 2; ++test_number)
  {
    Clipper2Lib::ClipperOffset co;

    Clipper2Lib::Paths64 subject, subject_open, clip;
    Clipper2Lib::Paths64 solution, solution_open;
    Clipper2Lib::ClipType ct = Clipper2Lib::ClipType::None;
    Clipper2Lib::FillRule fr = Clipper2Lib::FillRule::NonZero;
    int64_t stored_area = 0, stored_count = 0;

    ASSERT_TRUE(LoadTestNum(ifs, test_number, subject, subject_open, clip, stored_area, stored_count, ct, fr));

    co.AddPaths(subject, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
    Clipper2Lib::Paths64 outputs;
    co.Execute(1, outputs);

    // is the sum total area of the solution is positive
    const auto outer_is_positive = Clipper2Lib::Area(outputs) > 0;

    // there should be exactly one exterior path
    const auto is_positive_func = Clipper2Lib::IsPositive<int64_t>;
    const auto is_positive_count = std::count_if(
      outputs.begin(), outputs.end(), is_positive_func);
    const auto is_negative_count =
      outputs.size() - is_positive_count;
    if (outer_is_positive)
      EXPECT_EQ(is_positive_count, 1);
    else
      EXPECT_EQ(is_negative_count, 1);
  }
}


Clipper2Lib::Point64 MidPoint(const Clipper2Lib::Point64& p1, const Clipper2Lib::Point64& p2)
{
  Clipper2Lib::Point64 result;
  result.x = (p1.x + p2.x) / 2;
  result.y = (p1.y + p2.y) / 2;
  return result;
}

TEST(Clipper2Tests, TestOffsets2) { // see #448 & #456

  double scale = 10, delta = 10 * scale, arc_tol = 0.25 * scale;

  Clipper2Lib::Paths64 subject, solution;
  Clipper2Lib::ClipperOffset c;
  subject.push_back(Clipper2Lib::MakePath({ 50,50, 100,50, 100,150, 50,150, 0,100 }));

  int err;
  subject = Clipper2Lib::ScalePaths<int64_t, int64_t>(subject, scale, err);

  c.AddPaths(subject, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
  c.ArcTolerance(arc_tol);
  c.Execute(delta, solution);

  double min_dist = delta * 2, max_dist = 0;
  for (auto subjPt : subject[0])
  {
    Clipper2Lib::Point64 prevPt = solution[0][solution[0].size() - 1];
    for (auto pt : solution[0])
    {
      Clipper2Lib::Point64 mp = MidPoint(prevPt, pt);
      double d = Clipper2Lib::Distance(mp, subjPt);
      if (d < delta * 2)
      {
        if (d < min_dist) min_dist = d;
        if (d > max_dist) max_dist = d;
      }
      prevPt = pt;
    }
  }

  EXPECT_GE(min_dist + 1, delta - arc_tol); // +1 for rounding errors
  EXPECT_LE(solution[0].size(), 21); 
}

TEST(Clipper2Tests, TestOffsets3) // see #424
{

  Clipper2Lib::Paths64 subjects = {{
    {1525311078, 1352369439},
    {1526632284, 1366692987},
    {1519397110, 1367437476},
    {1520246456, 1380177674},
    {1520613458, 1385913385},
    {1517383844, 1386238444},
    {1517771817, 1392099983},
    {1518233190, 1398758441},
    {1518421934, 1401883197},
    {1518694564, 1406612275},
    {1520267428, 1430289121},
    {1520770744, 1438027612},
    {1521148232, 1443438264},
    {1521441833, 1448964260},
    {1521683005, 1452518932},
    {1521819320, 1454374912},
    {1527943004, 1454154711},
    {1527649403, 1448523858},
    {1535901696, 1447989084},
    {1535524209, 1442788147},
    {1538953052, 1442463089},
    {1541553521, 1442242888},
    {1541459149, 1438855987},
    {1538764308, 1439076188},
    {1538575565, 1436832236},
    {1538764308, 1436832236},
    {1536509870, 1405374956},
    {1550497874, 1404347351},
    {1550214758, 1402428457},
    {1543818445, 1402868859},
    {1543734559, 1402124370},
    {1540672717, 1402344571},
    {1540473487, 1399995761},
    {1524996506, 1400981422},
    {1524807762, 1398223667},
    {1530092585, 1397898609},
    {1531675935, 1397783265},
    {1531392819, 1394920653},
    {1529809469, 1395025510},
    {1529348096, 1388880855},
    {1531099218, 1388660654},
    {1530826588, 1385158410},
    {1532955197, 1384938209},
    {1532661596, 1379003269},
    {1532472852, 1376235028},
    {1531277476, 1376350372},
    {1530050642, 1361806623},
    {1599487345, 1352704983},
    {1602758902, 1378489467},
    {1618990858, 1376350372},
    {1615058698, 1344085688},
    {1603230761, 1345700495},
    {1598648484, 1346329641},
    {1598931599, 1348667965},
    {1596698132, 1348993024},
    {1595775386, 1342722540} }};

  Clipper2Lib::Paths64 solution = InflatePaths(subjects, -209715,
    Clipper2Lib::JoinType::Miter, Clipper2Lib::EndType::Polygon);

  EXPECT_LE(solution[0].size() - subjects[0].size(), 1);
}

TEST(Clipper2Tests, TestOffsets4) // see #482
{
  Clipper2Lib::Paths64 paths = { { {0, 0}, {20000, 200},
    {40000, 0}, {40000, 50000}, {0, 50000}, {0, 0}} };
  Clipper2Lib::Paths64 solution = Clipper2Lib::InflatePaths(paths, -5000,
    Clipper2Lib::JoinType::Square, Clipper2Lib::EndType::Polygon);
  std::cout << solution[0].size() << std::endl;

  EXPECT_EQ(solution[0].size(), 5);

  paths = { { {0, 0}, {20000, 400},
    {40000, 0}, {40000, 50000}, {0, 50000}, {0, 0}} };
  solution = Clipper2Lib::InflatePaths(paths, -5000,
    Clipper2Lib::JoinType::Square, Clipper2Lib::EndType::Polygon);
  std::cout << solution[0].size() << std::endl;

  EXPECT_EQ(solution[0].size(), 5);

  paths = { { {0, 0}, {20000, 400},
    {40000, 0}, {40000, 50000}, {0, 50000}, {0, 0}} };
  solution = Clipper2Lib::InflatePaths(paths, -5000,
    Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
  std::cout << solution[0].size() << std::endl;

  EXPECT_EQ(solution[0].size(), 5);

  paths = { { {0, 0}, {20000, 1500},
    {40000, 0}, {40000, 50000}, {0, 50000}, {0, 0}} };
  solution = Clipper2Lib::InflatePaths(paths, -5000,
    Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
  std::cout << solution[0].size() << std::endl;

  EXPECT_GT(solution[0].size(), 6);
}
