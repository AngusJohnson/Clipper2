#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "clipper2/clipper.export.h"
using namespace Clipper2Lib;
static bool CreatePolyPath64FromCPolyPath(CPolyPath64& v, PolyPath64& owner)
{
  int64_t poly_len = *v++, child_count = *v++;
  if (!poly_len) return false;
  Path64 path;
  path.reserve(poly_len);
  for (size_t i = 0; i < poly_len; ++i)
  {
    int64_t x = *v++, y = *v++;
    path.push_back(Point64(x,y));
  }
  PolyPath64* new_owner = owner.AddChild(path);
  for (size_t i = 0; i < child_count; ++i)
    CreatePolyPath64FromCPolyPath(v, *new_owner);
  return true;
}
static bool BuildPolyTree64FromCPolyTree(CPolyTree64 tree, PolyTree64& result)
{
  result.Clear();
  int64_t* v = tree;
  int64_t array_len = *v++, child_count = *v++;
  for (size_t i = 0; i < child_count; ++i)
    if (!CreatePolyPath64FromCPolyPath(v, result)) return false;
  return true;
}
static bool CreatePolyPathDFromCPolyPath(CPolyPathD& v, PolyPathD& owner)
{
  int64_t poly_len = *v++, child_count = *v++;
  if (!poly_len) return false;
  PathD path;
  path.reserve(poly_len);
  for (size_t i = 0; i < poly_len; ++i)
  {
    int64_t x = *v++, y = *v++;
    path.push_back(PointD(x, y));
  }
  PolyPathD* new_owner = owner.AddChild(path);
  for (size_t i = 0; i < child_count; ++i)
    CreatePolyPathDFromCPolyPath(v, *new_owner);
  return true;
}
static bool BuildPolyTreeDFromCPolyTree(CPolyTreeD tree, PolyTreeD& result)
{
  result.Clear();
  double* v = tree;
  int64_t array_len = *v++, child_count = *v++;
  for (size_t i = 0; i < child_count; ++i)
    if (!CreatePolyPathDFromCPolyPath(v, result)) return false;
  return true;
}
TEST(Clipper2Tests, ExportHeader64)
{
  uint8_t None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
  uint8_t EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;
  Paths64 subj, clip, solution;
  //subj.push_back(MakeRandomPoly(600, 400, 25));
  //clip.push_back(MakeRandomPoly(600, 400, 25));
  for (int i = 1; i < 6; ++i)
    subj.push_back(MakePath({ -i*20,-i * 20, i * 20,-i * 20, i * 20,i * 20, -i * 20,i * 20 }));
  clip.push_back(MakePath({ -90,-120,90,-120, 90,120, -90,120 }));
  CPaths64 c_subj_open = nullptr, c_sol = nullptr, c_sol_open = nullptr;
  // Note: while CreateCPaths64 isn't exported in clipper.export.h, it can still
  // be used here because we're simply statically compiling clipper.export.h.
  // Normally clipper.export.h will be compiled into a DLL/so so it can be called
  // by non C++ applications. If CreateCPaths64 was an exported function and it
  // was called by a non C++ application, it would crash that application.
  CPaths64 c_subj = CreateCPaths(subj);
  CPaths64 c_clip = CreateCPaths(clip);
  BooleanOp64(Intersection, EvenOdd, c_subj, c_subj_open, c_clip, c_sol, c_sol_open);
  solution = ConvertCPaths(c_sol);
  //clean up !!!
  delete[] c_subj;
  delete[] c_clip;
  DisposeArray64(c_sol);
  DisposeArray64(c_sol_open);
  EXPECT_EQ(solution.size(), 5);
}
TEST(Clipper2Tests, ExportHeaderD)
{
  uint8_t None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
  uint8_t EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;
  PathsD subj, clip, solution;
  //subj.push_back(MakeRandomPolyD(600, 400, 25));
  //clip.push_back(MakeRandomPolyD(600, 400, 25));
  for (int i = 1; i < 6; ++i)
    subj.push_back(MakePathD({ -i * 20,-i * 20, i * 20,-i * 20, i * 20,i * 20, -i * 20,i * 20 }));
  clip.push_back(MakePathD({ -90,-120,90,-120, 90,120, -90,120 }));
  CPathsD c_subj_open = nullptr, c_sol = nullptr, c_sol_open = nullptr;
  // Note: while CreateCPathsD isn't exported in clipper.export.h, it can still
  // be used here because we're simply statically compiling clipper.export.h.
  // Normally clipper.export.h will be compiled into a DLL/so so it can be called
  // by non C++ applications. If CreateCPathsD was an exported function and it
  // was called by a non C++ application, it would crash that application.
  CPathsD c_subj = CreateCPaths(subj);
  CPathsD c_clip = CreateCPaths(clip);
  BooleanOpD(Intersection, EvenOdd, c_subj, c_subj_open, c_clip, c_sol, c_sol_open);
  solution = ConvertCPaths(c_sol);
  //clean up !!!
  delete[] c_subj;
  delete[] c_clip;
  DisposeArrayD(c_sol);
  DisposeArrayD(c_sol_open);
  EXPECT_EQ(solution.size(), 5);
}
TEST(Clipper2Tests, ExportHeaderTree64)
{
  uint8_t None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
  uint8_t EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;
  Paths64 subj, clip, solution;
  for (int i = 1; i < 6; ++i)
    subj.push_back(MakePath({ -i * 20,-i * 20, i * 20,-i * 20, i * 20,i * 20, -i * 20,i * 20 }));
  clip.push_back(MakePath({ -90,-120,90,-120, 90,120, -90,120 }));
  CPaths64 c_subj_open = nullptr, c_sol = nullptr, c_sol_open = nullptr;
  // Note: while CreateCPaths64 isn't exported in clipper.export.h, it can still
  // be used here because we're statically compiling clipper.export.h.
  // More likely, clipper.export.h will be compiled into a DLL/so so it can be
  // called by non C++ applications. If CreateCPaths64 was an exported function
  // and it was called by a non C++ application, it would crash that application.
  CPaths64 c_subj = CreateCPaths(subj);
  CPaths64 c_clip = CreateCPaths(clip);
  int64_t* c_sol_tree = nullptr;
  BooleanOp_PolyTree64(Intersection, EvenOdd, c_subj, c_subj_open, c_clip, c_sol_tree, c_sol_open);
  PolyTree64 sol_tree;
  // convert CPolyTree64 to PolyTree64
  BuildPolyTree64FromCPolyTree(c_sol_tree, sol_tree);
  // convert PolyTree64 to Paths64
  solution = PolyTreeToPaths64(sol_tree);
  //clean up !!!
  delete[] c_subj;
  delete[] c_clip;
  DisposeArray64(c_sol_tree);
  DisposeArray64(c_sol_open);
  PolyPath64* pp = &sol_tree;
  for (int i = 0; i < 4; ++i)
  {
    EXPECT_TRUE(pp->Count() == 1); pp = pp->Child(0);
  }
}
TEST(Clipper2Tests, ExportHeaderTreeD)
{
  uint8_t None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
  uint8_t EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;
  PathsD subj, clip, solution;
  for (int i = 1; i < 6; ++i)
    subj.push_back(MakePathD({ -i * 20,-i * 20, i * 20,-i * 20, i * 20,i * 20, -i * 20,i * 20 }));
  clip.push_back(MakePathD({ -90,-120,90,-120, 90,120, -90,120 }));
  CPathsD c_subj_open = nullptr, c_sol = nullptr, c_sol_open = nullptr;
  // Note: while CreateCPathsD isn't exported in clipper.export.h, it can still
  // be used here because we're statically compiling clipper.export.h.
  // More likely, clipper.export.h will be compiled into a DLL/so so it can be
  // called by non C++ applications. If CreateCPathsD was an exported function
  // and it was called by a non C++ application, it would crash that application.
  CPathsD c_subj = CreateCPaths(subj);
  CPathsD c_clip = CreateCPaths(clip);
  static const int precision = 4;
  CPolyPathD c_sol_tree = nullptr;
  BooleanOp_PolyTreeD(Intersection, EvenOdd, c_subj, c_subj_open, c_clip, c_sol_tree, c_sol_open, precision);
  PolyTreeD sol_tree;
  // convert CPolyTreeD to PolyTreeD
  BuildPolyTreeDFromCPolyTree(c_sol_tree, sol_tree);
  // convert PolyTreeD to PathsD
  solution = PolyTreeToPathsD(sol_tree);
  //clean up !!!
  delete[] c_subj;
  delete[] c_clip;
  DisposeArrayD(c_sol_tree);
  DisposeArrayD(c_sol_open);
  PolyPathD* pp = &sol_tree;
  for (int i = 0; i < 4; ++i)
  {
    EXPECT_TRUE(pp->Count() == 1); pp = pp->Child(0);
  }
}