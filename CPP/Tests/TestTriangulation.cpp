#include <gtest/gtest.h>
#include <algorithm>
#include <cmath>
#include "clipper2/clipper.h"

inline void ValidTriangulation(const Clipper2Lib::Paths64& subject, const Clipper2Lib::Paths64& solution, size_t triangleCount, double area)
{
  // Right amount of triangles
  ASSERT_EQ(solution.size(), triangleCount);
  // Every triangle has three points
  for (const Clipper2Lib::Path64& triangle : solution) 
  {
    EXPECT_EQ(triangle.size(), 3);
  }
  EXPECT_TRUE(Xor(solution, subject, Clipper2Lib::FillRule::NonZero).empty());
  // It has the right area
  EXPECT_EQ(std::abs(Area(solution)), area);
}

// Confirms that an empty input produces an empty output
TEST(Clipper2Tests, TestTriangulateEmptyInput) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 subject, solution;
    EXPECT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay), Clipper2Lib::TriangulateResult::no_polygons);
    EXPECT_TRUE(solution.empty());
  }
}

// Confirms the triangulation of a triangle is the original triangle
TEST(Clipper2Tests, TestTriangulateTriangle) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({0,0, 0,100, 100,0}) };
    Clipper2Lib::Paths64 solution;
    ASSERT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay), Clipper2Lib::TriangulateResult::success);
    // One triangle returned
    ASSERT_NO_FATAL_FAILURE(ValidTriangulation(subject, solution, 1, 5000));
    // The vertices are exactly the same three points as the input
    EXPECT_TRUE(std::is_permutation(solution[0].begin(), solution[0].end(),
    subject[0].begin(), subject[0].end()))
    << " got " << solution[0] << " expected " << subject[0];
  }
}

// Makes sure a square is split in two along a diagonal
TEST(Clipper2Tests, TestTriangulateSquare) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({0,0, 100,0, 100,100, 0,100}) };
    Clipper2Lib::Paths64 solution;
    ASSERT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay), Clipper2Lib::TriangulateResult::success);
    ASSERT_NO_FATAL_FAILURE(ValidTriangulation(subject, solution, 2, 10000));
  }
}

// A square with decimal corners
TEST(Clipper2Tests, TestTriangulateDecimalSquare) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::PathsD subject = { Clipper2Lib::MakePathD({0.5,0.5, 100.5,0.5, 100.5,100.5, 0.5,100.5}) };
    Clipper2Lib::PathsD solution;
    ASSERT_EQ(Clipper2Lib::Triangulate(subject, 1, solution, delaunay), Clipper2Lib::TriangulateResult::success);
    // Two triangles returned
    ASSERT_EQ(solution.size(), 2);
    // Triangles exactly merge to form the square
    Clipper2Lib::PathsD difference = Clipper2Lib::Xor(solution, subject, Clipper2Lib::FillRule::NonZero, 1);
    EXPECT_NEAR(Area(difference), 0, 0.000001);
    // It has the right area
    EXPECT_NEAR(std::abs(Area(solution)), 10000, 0.000001);
  }
}

// Checks an L shape 
// (0,400)----(100,400)
//   |          |
//   |          |
//   |          |
//   |      (100,100)---(200,100)
//   |                      |
// (0,0)-----------------(200,0)
TEST(Clipper2Tests, TestTriangulateLShape) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({0,0, 200,0, 200,100, 100,100, 100,400, 0,400}) };
    Clipper2Lib::Paths64 solution;
    ASSERT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay),Clipper2Lib::TriangulateResult::success);
    ASSERT_NO_FATAL_FAILURE(ValidTriangulation(subject, solution, 4, 50000));
  }
}

// Checks a square with a square hole
// (0,300)------------------(300,300)
//   |                          |
//   |   (100,200)──(200,200)   |
//   |      |           |       |
//   |   (100,100)──(200,100)   |
//   |                          |
// (0,0)---------------------(300,0)
TEST(Clipper2Tests, TestTriangulateSquareWithHole) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 outerSquare = { Clipper2Lib::MakePath({0,0, 300,0, 300,300, 0,300}) };
    Clipper2Lib::Paths64 innerSquare = { Clipper2Lib::MakePath({100,100, 200,100, 200,200, 100,200}) };
    Clipper2Lib::Paths64 subject = Clipper2Lib::Difference(outerSquare, innerSquare, Clipper2Lib::FillRule::NonZero);
    Clipper2Lib::Paths64 solution;
    ASSERT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay), Clipper2Lib::TriangulateResult::success);
    ASSERT_NO_FATAL_FAILURE(ValidTriangulation(subject, solution, 8, 80000));
  }
}

// Checks a square with a square hole, now with extra points
TEST(Clipper2Tests, TestTriangulateSquareHoleExtraPoints) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({0,0, 150,0, 300,0, 300,150, 300,300, 0,300}), 
                                     Clipper2Lib::MakePath({100,100, 100,200, 150,200, 200,200, 200,100})};
    Clipper2Lib::Paths64 solution;
    ASSERT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay), Clipper2Lib::TriangulateResult::success);
    ASSERT_NO_FATAL_FAILURE(ValidTriangulation(subject, solution, 8, 80000));
  }
}

// Checks a square with a square hole, now with duplicate points
TEST(Clipper2Tests, TestTriangulateSquareHoleDuplicatePoints) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({0,0, 0,0, 300,0, 300,300, 300,300, 0,300}), 
                                     Clipper2Lib::MakePath({100,100, 100,200, 100,200, 200,200, 200,100, 200,100})};
    Clipper2Lib::Paths64 solution;
    ASSERT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay), Clipper2Lib::TriangulateResult::success);
    ASSERT_NO_FATAL_FAILURE(ValidTriangulation(subject, solution, 8, 80000));
  }
}

// Tries to triangulate a bowtie on 4 vertices. Which should fail.
TEST(Clipper2Tests, TestTriangulateBowtie) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({0,0, 100,100, 100,0, 0,100}) };
    Clipper2Lib::Paths64 solution;
    EXPECT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay), Clipper2Lib::TriangulateResult::paths_intersect);
    EXPECT_TRUE(solution.empty());
  }
}

// Collinear points
TEST(Clipper2Tests, TestTriangulateCollinear) //#1076
{
  for (bool delaunay : {false, true})
  {
    SCOPED_TRACE(delaunay ? "delaunay = true" : "delaunay = false");
    Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({0,0, 100,0, 200,0, 300,0}) };
    Clipper2Lib::Paths64 solution;
    EXPECT_EQ(Clipper2Lib::Triangulate(subject, solution, delaunay), Clipper2Lib::TriangulateResult::no_polygons);
    EXPECT_TRUE(solution.empty());
  }
}


// Delaunay triangulation maximizes angles so we should split a rhombus on the short diagonal
TEST(Clipper2Tests, TestTriangulateRhombusDelaunay) //#1076
{
  Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({100,0, 50,200, 100,400, 150,200}) };
  Clipper2Lib::Paths64 solution;
  ASSERT_EQ(Clipper2Lib::Triangulate(subject, solution, true), Clipper2Lib::TriangulateResult::success);
  ASSERT_NO_FATAL_FAILURE(ValidTriangulation(subject, solution, 2, 20000));
  // Both triangles share the short diagonal
  const Clipper2Lib::Point64 delaunayEdgeStart(50, 200);
  const Clipper2Lib::Point64 delaunayEdgeEnd(150, 200);
  for (const Clipper2Lib::Path64& triangle : solution)
  {
    EXPECT_EQ(std::count(triangle.begin(), triangle.end(), delaunayEdgeStart), 1);
    EXPECT_EQ(std::count(triangle.begin(), triangle.end(), delaunayEdgeEnd), 1);
  }
}


// The Delaunay triangulation should match one computed independently in Ipe
TEST(Clipper2Tests, TestTriangulateMatchesIpeDelaunay) //#1076
{
  Clipper2Lib::Paths64 subject = { Clipper2Lib::MakePath({64,64, 112,160, 192,160, 256,208, 368,128, 288,48, 208,80, 176,48}) };
  // Ipe's Delaunay Triangles
  Clipper2Lib::Paths64 expected = 
  {
    Clipper2Lib::MakePath({64,64, 112,160, 176,48}),
    Clipper2Lib::MakePath({112,160, 176,48,  208,80}),
    Clipper2Lib::MakePath({112,160, 192,160, 208,80}),
    Clipper2Lib::MakePath({192,160, 208,80, 256,208}),
    Clipper2Lib::MakePath({368,128, 288,48, 256,208}),
    Clipper2Lib::MakePath({208,80, 288,48, 256,208}),
  };
  Clipper2Lib::Paths64 solution;
  ASSERT_EQ(Clipper2Lib::Triangulate(subject, solution, true), Clipper2Lib::TriangulateResult::success);
  ASSERT_NO_FATAL_FAILURE(ValidTriangulation(subject, solution, 6, 28288));
  // All the expected triangles are there one time each
  for (const Clipper2Lib::Path64& expectedTriangle : expected)
  {
    int matches = 0;
    for (const Clipper2Lib::Path64& triangle : solution)
    {
      if (std::is_permutation(triangle.begin(), triangle.end(),expectedTriangle.begin(), expectedTriangle.end()))
        ++matches;
    }
    EXPECT_EQ(matches, 1) << " expected triangle " << expectedTriangle;
  }
}
