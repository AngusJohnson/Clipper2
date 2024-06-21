#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "clipper2/clipper.export.h"
using namespace Clipper2Lib;
static bool CreatePolyPath64FromCPolyPath(CPolyPathI& v, PolyPathI& owner)
{
  Integer poly_len = *v++, child_count = *v++;
  if (!poly_len) return false;
  PathI path;
  path.reserve(poly_len);
  for (size_t i = 0; i < poly_len; ++i)
  {
    Integer x = *v++, y = *v++;
    path.push_back(PointI(x,y));
  }
  PolyPathI* new_owner = owner.AddChild(path);
  for (size_t i = 0; i < child_count; ++i)
    CreatePolyPath64FromCPolyPath(v, *new_owner);
  return true;
}
static bool BuildPolyTree64FromCPolyTree(CPolyTreeI tree, PolyTreeI& result)
{
  result.Clear();
  Integer* v = tree;
  Integer array_len = *v++, child_count = *v++;
  for (size_t i = 0; i < child_count; ++i)
    if (!CreatePolyPath64FromCPolyPath(v, result)) return false;
  return true;
}
static bool CreatePolyPathDFromCPolyPath(CPolyPathS& v, PolyPathS& owner)
{
  Integer poly_len = *v++, child_count = *v++;
  if (!poly_len) return false;
  PathS path;
  path.reserve(poly_len);
  for (size_t i = 0; i < poly_len; ++i)
  {
    Integer x = *v++, y = *v++;
    path.push_back(PointS(x, y));
  }
  PolyPathS* new_owner = owner.AddChild(path);
  for (size_t i = 0; i < child_count; ++i)
    CreatePolyPathDFromCPolyPath(v, *new_owner);
  return true;
}
static bool BuildPolyTreeDFromCPolyTree(CPolyTreeS tree, PolyTreeD& result)
{
  result.Clear();
  Scalar* v = tree;
  Integer array_len = *v++, child_count = *v++;
  for (size_t i = 0; i < child_count; ++i)
    if (!CreatePolyPathDFromCPolyPath(v, result)) return false;
  return true;
}
TEST(Clipper2Tests, ExportHeader64)
{
  uint8_t None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
  uint8_t EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;
  PathsI subj, clip, solution;
  //subj.push_back(MakeRandomPoly(600, 400, 25));
  //clip.push_back(MakeRandomPoly(600, 400, 25));
  for (int i = 1; i < 6; ++i)
    subj.push_back(MakePath({ -i*20,-i * 20, i * 20,-i * 20, i * 20,i * 20, -i * 20,i * 20 }));
  clip.push_back(MakePath({ -90,-120,90,-120, 90,120, -90,120 }));
  CPathsI c_subj_open = nullptr, c_sol = nullptr, c_sol_open = nullptr;
  // Note: while CreateCPaths64 isn't exported in clipper.export.h, it can still
  // be used here because we're simply statically compiling clipper.export.h.
  // Normally clipper.export.h will be compiled into a DLL/so so it can be called
  // by non C++ applications. If CreateCPaths64 was an exported function and it
  // was called by a non C++ application, it would crash that application.
  CPathsI c_subj = CreateCPaths(subj);
  CPathsI c_clip = CreateCPaths(clip);
  BooleanOpI(Intersection, EvenOdd, c_subj, c_subj_open, c_clip, c_sol, c_sol_open);
  solution = ConvertCPaths(c_sol);
  //clean up !!!
  delete[] c_subj;
  delete[] c_clip;
  DisposeArrayI(c_sol);
  DisposeArrayI(c_sol_open);
  EXPECT_EQ(solution.size(), 5);
}
TEST(Clipper2Tests, ExportHeaderD)
{
  uint8_t None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
  uint8_t EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;
  PathsS subj, clip, solution;
  //subj.push_back(MakeRandomPolyD(600, 400, 25));
  //clip.push_back(MakeRandomPolyD(600, 400, 25));
  for (int i = 1; i < 6; ++i)
    subj.push_back(MakePathS({ -i * 20,-i * 20, i * 20,-i * 20, i * 20,i * 20, -i * 20,i * 20 }));
  clip.push_back(MakePathS({ -90,-120,90,-120, 90,120, -90,120 }));
  CPathsS c_subj_open = nullptr, c_sol = nullptr, c_sol_open = nullptr;
  // Note: while CreateCPathsD isn't exported in clipper.export.h, it can still
  // be used here because we're simply statically compiling clipper.export.h.
  // Normally clipper.export.h will be compiled into a DLL/so so it can be called
  // by non C++ applications. If CreateCPathsD was an exported function and it
  // was called by a non C++ application, it would crash that application.
  CPathsS c_subj = CreateCPaths(subj);
  CPathsS c_clip = CreateCPaths(clip);
  BooleanOpS(Intersection, EvenOdd, c_subj, c_subj_open, c_clip, c_sol, c_sol_open);
  solution = ConvertCPaths(c_sol);
  //clean up !!!
  delete[] c_subj;
  delete[] c_clip;
  DisposeArrayS(c_sol);
  DisposeArrayS(c_sol_open);
  EXPECT_EQ(solution.size(), 5);
}
TEST(Clipper2Tests, ExportHeaderTree64)
{
  uint8_t None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
  uint8_t EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;
  PathsI subj, clip, solution;
  for (int i = 1; i < 6; ++i)
    subj.push_back(MakePath({ -i * 20,-i * 20, i * 20,-i * 20, i * 20,i * 20, -i * 20,i * 20 }));
  clip.push_back(MakePath({ -90,-120,90,-120, 90,120, -90,120 }));
  CPathsI c_subj_open = nullptr, c_sol = nullptr, c_sol_open = nullptr;
  // Note: while CreateCPaths64 isn't exported in clipper.export.h, it can still
  // be used here because we're statically compiling clipper.export.h.
  // More likely, clipper.export.h will be compiled into a DLL/so so it can be
  // called by non C++ applications. If CreateCPaths64 was an exported function
  // and it was called by a non C++ application, it would crash that application.
  CPathsI c_subj = CreateCPaths(subj);
  CPathsI c_clip = CreateCPaths(clip);
  Integer* c_sol_tree = nullptr;
  BooleanOp_PolyTreeI(Intersection, EvenOdd, c_subj, c_subj_open, c_clip, c_sol_tree, c_sol_open);
  PolyTreeI sol_tree;
  // convert CPolyTreeI to PolyTreeI
  BuildPolyTree64FromCPolyTree(c_sol_tree, sol_tree);
  // convert PolyTreeI to PathsI
  solution = PolyTreeToPathsI(sol_tree);
  //clean up !!!
  delete[] c_subj;
  delete[] c_clip;
  DisposeArrayI(c_sol_tree);
  DisposeArrayI(c_sol_open);
  PolyPathI* pp = &sol_tree;
  for (int i = 0; i < 4; ++i)
  {
    EXPECT_TRUE(pp->Count() == 1); pp = pp->Child(0);
  }
}
TEST(Clipper2Tests, ExportHeaderTreeD)
{
  uint8_t None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
  uint8_t EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;
  PathsS subj, clip, solution;
  for (int i = 1; i < 6; ++i)
    subj.push_back(MakePathS({ -i * 20,-i * 20, i * 20,-i * 20, i * 20,i * 20, -i * 20,i * 20 }));
  clip.push_back(MakePathS({ -90,-120,90,-120, 90,120, -90,120 }));
  CPathsS c_subj_open = nullptr, c_sol = nullptr, c_sol_open = nullptr;
  // Note: while CreateCPathsD isn't exported in clipper.export.h, it can still
  // be used here because we're statically compiling clipper.export.h.
  // More likely, clipper.export.h will be compiled into a DLL/so so it can be
  // called by non C++ applications. If CreateCPathsD was an exported function
  // and it was called by a non C++ application, it would crash that application.
  CPathsS c_subj = CreateCPaths(subj);
  CPathsS c_clip = CreateCPaths(clip);
  static const int precision = 4;
  CPolyPathS c_sol_tree = nullptr;
  BooleanOp_PolyTreeS(Intersection, EvenOdd, c_subj, c_subj_open, c_clip, c_sol_tree, c_sol_open, precision);
  PolyTreeD sol_tree;
  // convert CPolyTreeS to PolyTreeD
  BuildPolyTreeDFromCPolyTree(c_sol_tree, sol_tree);
  // convert PolyTreeD to PathsS
  solution = PolyTreeToPathsS(sol_tree);
  //clean up !!!
  delete[] c_subj;
  delete[] c_clip;
  DisposeArrayS(c_sol_tree);
  DisposeArrayS(c_sol_open);
  PolyPathS* pp = &sol_tree;
  for (int i = 0; i < 4; ++i)
  {
    EXPECT_TRUE(pp->Count() == 1); pp = pp->Child(0);
  }
}