/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  14 May 2024                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  This module exports the Clipper2 Library (ie DLL/so)            *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/


/*
 Boolean clipping:
 cliptype: None=0, Intersection=1, Union=2, Difference=3, Xor=4
 fillrule: EvenOdd=0, NonZero=1, Positive=2, Negative=3

 Polygon offsetting (inflate/deflate):
 jointype: Square=0, Bevel=1, Round=2, Miter=3
 endtype: Polygon=0, Joined=1, Butt=2, Square=3, Round=4

The path structures used extensively in other parts of this library are all
based on std::vector classes. Since C++ classes can't be accessed by other
languages, these paths are converted into very simple array data structures 
(of either Integer for CPathI or Scalar for CPathS) that can be parsed by 
just about any programming language.

CPathI and CPathS:
These are arrays of consecutive x and y path coordinates preceeded by
a pair of values containing the path's length (N) and a 0 value.
__________________________________
|counter|coord1|coord2|...|coordN|
|N, 0   |x1, y1|x2, y2|...|xN, yN|
__________________________________

CPathsI and CPathsS:
These are also arrays containing any number of consecutive CPathI or
CPathS  structures. But preceeding these consecutive paths, there is pair of
values that contain the total length of the array structure (A) and the 
number of CPathI or CPathS it contains (C). The space these structures will
occupy in memory = A * sizeof(Integer) or  A * sizeof(Scalar) respectively. 
_______________________________
|counter|path1|path2|...|pathC|
|A  , C |                     |
_______________________________

CPolytree64 and CPolytreeD:
These are also arrays consisting of CPolyPath structures that represent
individual paths in a tree structure. However, the very first (ie top)
CPolyPath is just the tree container that doesn't have a path. And because
of that, its structure will be very slightly different from the remaining
CPolyPath. This difference will be discussed below.

CPolyPathI and CPolyPathS:
These are simple arrays consisting of a series of path coordinates followed
by any number of child (ie nested) CPolyPath. Preceeding these are two values
indicating the length of the path (N) and the number of child CPolyPath (C).
____________________________________________________________
|counter|coord1|coord2|...|coordN| child1|child2|...|childC|
|N  , C |x1, y1|x2, y2|...|xN, yN|                         |
____________________________________________________________

As mentioned above, the very first CPolyPath structure is just a container
that owns (both directly and indirectly) every other CPolyPath in the tree.
Since this first CPolyPath has no path, instead of a path length, its very
first value will contain the total length of the CPolytree array (not its
total bytes length).

Again, all theses exported structures (CPathsI, CPathsS, CPolyTreeI & 
CPolyTreeS) are arrays of either type Integer or Scalar, and the first 
value in these arrays will always be the length of that array.

These array structures are allocated in heap memory which will eventually
need to be released. However, since applications dynamically linking to 
these functions may use different memory managers, the only safe way to 
free up this memory is to use the exported DisposeArrayI and 
DisposeArrayS functions (see below).
*/


#ifndef CLIPPER2_EXPORT_H
#define CLIPPER2_EXPORT_H

#include <cstdlib>
#include <vector>

#include "clipper2/clipper.core.h"
#include "clipper2/clipper.engine.h"
#include "clipper2/clipper.offset.h"
#include "clipper2/clipper.rectclip.h"

namespace Clipper2Lib {

typedef Integer* CPathI;
typedef Integer* CPathsI;
typedef Scalar*  CPathS;
typedef Scalar*  CPathsS;

typedef Integer* CPolyPathI;
typedef Integer* CPolyTreeI;
typedef Scalar* CPolyPathS;
typedef Scalar* CPolyTreeS;

template <typename T>
struct CRect {
  T left;
  T top;
  T right;
  T bottom;
};

typedef CRect<Integer> CRectI;
typedef CRect<Scalar> CRectS;

template <typename T>
inline bool CRectIsEmpty(const CRect<T>& rect)
{
  return (rect.right <= rect.left) || (rect.bottom <= rect.top);
}

template <typename T>
inline Rect<T> CRectToRect(const CRect<T>& rect)
{
  Rect<T> result;
  result.left = rect.left;
  result.top = rect.top;
  result.right = rect.right;
  result.bottom = rect.bottom;
  return result;
}

#ifdef _WIN32
  #define EXTERN_DLL_EXPORT extern "C" __declspec(dllexport)
#else
  #define EXTERN_DLL_EXPORT extern "C"
#endif


//////////////////////////////////////////////////////
// EXPORTED FUNCTION DECLARATIONS
//////////////////////////////////////////////////////

EXTERN_DLL_EXPORT const char* Version();

EXTERN_DLL_EXPORT void DisposeArrayI(Integer*& p)
{
  delete[] p;
}

EXTERN_DLL_EXPORT void DisposeArrayS(Scalar*& p)
{
  delete[] p;
}

EXTERN_DLL_EXPORT int BooleanOpI(uint8_t cliptype,
  uint8_t fillrule, const CPathsI subjects,
  const CPathsI subjects_open, const CPathsI clips,
  CPathsI& solution, CPathsI& solution_open,
  bool preserve_collinear = true, bool reverse_solution = false);

EXTERN_DLL_EXPORT int BooleanOp_PolyTreeI(uint8_t cliptype,
  uint8_t fillrule, const CPathsI subjects,
  const CPathsI subjects_open, const CPathsI clips,
  CPolyTreeI& sol_tree, CPathsI& solution_open,
  bool preserve_collinear = true, bool reverse_solution = false);

EXTERN_DLL_EXPORT int BooleanOpS(uint8_t cliptype,
  uint8_t fillrule, const CPathsS subjects,
  const CPathsS subjects_open, const CPathsS clips,
  CPathsS& solution, CPathsS& solution_open, int precision = 2,
  bool preserve_collinear = true, bool reverse_solution = false);

EXTERN_DLL_EXPORT int BooleanOp_PolyTreeS(uint8_t cliptype,
  uint8_t fillrule, const CPathsS subjects,
  const CPathsS subjects_open, const CPathsS clips,
  CPolyTreeS& solution, CPathsS& solution_open, int precision = 2,
  bool preserve_collinear = true, bool reverse_solution = false);

EXTERN_DLL_EXPORT CPathsI InflatePathsI(const CPathsI paths,
  Scalar delta, uint8_t jointype, uint8_t endtype,
  Scalar miter_limit = 2.0, Scalar arc_tolerance = 0.0,
  bool reverse_solution = false);
EXTERN_DLL_EXPORT CPathsS InflatePathsS(const CPathsS paths,
  Scalar delta, uint8_t jointype, uint8_t endtype,
  int precision = 2, Scalar miter_limit = 2.0,
  Scalar arc_tolerance = 0.0, bool reverse_solution = false);

// RectClip & RectClipLines:
EXTERN_DLL_EXPORT CPathsI RectClipI(const CRectI& rect,
  const CPathsI paths);
EXTERN_DLL_EXPORT CPathsS RectClipS(const CRectS& rect,
  const CPathsS paths, int precision = 2);
EXTERN_DLL_EXPORT CPathsI RectClipLinesI(const CRectI& rect,
  const CPathsI paths);
EXTERN_DLL_EXPORT CPathsS RectClipLinesS(const CRectS& rect,
  const CPathsS paths, int precision = 2);

//////////////////////////////////////////////////////
// INTERNAL FUNCTIONS
//////////////////////////////////////////////////////

template <typename T>
static void GetPathCountAndCPathsArrayLen(const Paths<T>& paths,
  size_t& cnt, size_t& array_len)
{
  array_len = 2;
  cnt = 0;
  for (const Path<T>& path : paths)
    if (path.size())
    {
      array_len += path.size() * 2 + 2;
      ++cnt;
    }
}

static size_t GetPolyPathIArrayLen(const PolyPathI& pp)
{
  size_t result = 2; // poly_length + child_count
  result += pp.Polygon().size() * 2;
  //plus nested children :)
  for (size_t i = 0; i < pp.Count(); ++i)
    result += GetPolyPathIArrayLen(*pp[i]);
  return result;
}

static void GetPolytreeCountAndCStorageSize(const PolyTreeI& tree,
  size_t& cnt, size_t& array_len)
{
  cnt = tree.Count(); // nb: top level count only
  array_len = GetPolyPathIArrayLen(tree);
}

template <typename T>
static T* CreateCPaths(const Paths<T>& paths)
{
  size_t cnt = 0, array_len = 0;
  GetPathCountAndCPathsArrayLen(paths, cnt, array_len);
  T* result = new T[array_len], * v = result;
  *v++ = array_len;
  *v++ = cnt;
  for (const Path<T>& path : paths)
  {
    if (!path.size()) continue;
    *v++ = path.size();
    *v++ = 0;
    for (const Point<T>& pt : path)
    {
      *v++ = pt.x;
      *v++ = pt.y;
    }
  }
  return result;
}


CPathsS CreateCPathsDFromPathsI(const PathsI& paths, Scalar scale)
{
  if (!paths.size()) return nullptr;
  size_t cnt, array_len;
  GetPathCountAndCPathsArrayLen(paths, cnt, array_len);
  CPathsS result = new Scalar[array_len], v = result;
  *v++ = (Scalar)array_len;
  *v++ = (Scalar)cnt;
  for (const PathI& path : paths)
  {
    if (!path.size()) continue;
    *v = (Scalar)path.size();
    ++v; *v++ = 0;
    for (const PointI& pt : path)
    {
      *v++ = pt.x * scale;
      *v++ = pt.y * scale;
    }
  }
  return result;
}

template <typename T>
static Path<T> ConvertCPath(T* path)
{
  Path<T> result;
  if (!path) return result;
  T* v = path;
  size_t cnt = static_cast<size_t>(*v);
  v += 2; // skip 0 value
  result.reserve(cnt);
  for (size_t j = 0; j < cnt; ++j)
  {
    T x = *v++, y = *v++;
    result.push_back(Point<T>(x, y));
  }
  return result;
}

template <typename T>
static Paths<T> ConvertCPaths(T* paths)
{
  Paths<T> result;
  if (!paths) return result;
  T* v = paths; ++v;
  size_t cnt = static_cast<size_t>(*v++);
  result.reserve(cnt);
  for (size_t i = 0; i < cnt; ++i)
  {
    size_t cnt2 = static_cast<size_t>(*v);
    v += 2;
    Path<T> path;
    path.reserve(cnt2);
    for (size_t j = 0; j < cnt2; ++j)
    {
      T x = *v++, y = *v++;
      path.push_back(Point<T>(x, y));
    }
    result.push_back(path);
  }
  return result;
}


static PathsI ConvertCPathsDToPathsI(const CPathsS paths, Scalar scale)
{
  PathsI result;
  if (!paths) return result;
  Scalar* v = paths;
  ++v; // skip the first value (0)
  size_t cnt = static_cast<size_t>(*v++);
  result.reserve(cnt);
  for (size_t i = 0; i < cnt; ++i)
  {
    size_t cnt2 = static_cast<size_t>(*v);
    v += 2;
    PathI path;
    path.reserve(cnt2);
    for (size_t j = 0; j < cnt2; ++j)
    {
      Scalar x = *v++ * scale;
      Scalar y = *v++ * scale;
      path.push_back(PointI(x, y));
    }
    result.push_back(path);
  }
  return result;
}

template <typename T>
static void CreateCPolyPath(const PolyPathI* pp, T*& v, T scale)
{
  *v++ = static_cast<T>(pp->Polygon().size());
  *v++ = static_cast<T>(pp->Count());
  for (const PointI& pt : pp->Polygon())
  {
    *v++ = static_cast<T>(pt.x * scale);
    *v++ = static_cast<T>(pt.y * scale);
  }
  for (size_t i = 0; i < pp->Count(); ++i)
    CreateCPolyPath(pp->Child(i), v, scale);
}

template <typename T>
static T* CreateCPolyTree(const PolyTreeI& tree, T scale)
{
  if (scale == 0) scale = 1;
  size_t cnt, array_len;
  GetPolytreeCountAndCStorageSize(tree, cnt, array_len);
  if (!cnt) return nullptr;
  // allocate storage
  T* result = new T[array_len];
  T* v = result;

  *v++ = static_cast<T>(array_len);
  *v++ = static_cast<T>(tree.Count());
  for (size_t i = 0; i < tree.Count(); ++i)
    CreateCPolyPath(tree.Child(i), v, scale);
  return result;
}

//////////////////////////////////////////////////////
// EXPORTED FUNCTION DEFINITIONS
//////////////////////////////////////////////////////

EXTERN_DLL_EXPORT const char* Version()
{
  return CLIPPER2_VERSION;
}

EXTERN_DLL_EXPORT int BooleanOpI(uint8_t cliptype,
  uint8_t fillrule, const CPathsI subjects,
  const CPathsI subjects_open, const CPathsI clips,
  CPathsI& solution, CPathsI& solution_open,
  bool preserve_collinear, bool reverse_solution)
{
  if (cliptype > static_cast<uint8_t>(ClipType::Xor)) return -4;
  if (fillrule > static_cast<uint8_t>(FillRule::Negative)) return -3;

  PathsI sub, sub_open, clp, sol, sol_open;
  sub       = ConvertCPaths(subjects);
  sub_open  = ConvertCPaths(subjects_open);
  clp       = ConvertCPaths(clips);

  ClipperI clipper;
  clipper.PreserveCollinear(preserve_collinear);
  clipper.ReverseSolution(reverse_solution);
  if (sub.size() > 0) clipper.AddSubject(sub);
  if (sub_open.size() > 0) clipper.AddOpenSubject(sub_open);
  if (clp.size() > 0) clipper.AddClip(clp);
  if (!clipper.Execute(ClipType(cliptype), FillRule(fillrule), sol, sol_open))
    return -1; // clipping bug - should never happen :)
  solution = CreateCPaths(sol);
  solution_open = CreateCPaths(sol_open);
  return 0; //success !!
}

EXTERN_DLL_EXPORT int BooleanOp_PolyTreeI(uint8_t cliptype,
  uint8_t fillrule, const CPathsI subjects,
  const CPathsI subjects_open, const CPathsI clips,
  CPolyTreeI& sol_tree, CPathsI& solution_open,
  bool preserve_collinear, bool reverse_solution)
{
  if (cliptype > static_cast<uint8_t>(ClipType::Xor)) return -4;
  if (fillrule > static_cast<uint8_t>(FillRule::Negative)) return -3;
  PathsI sub, sub_open, clp, sol_open;
  sub = ConvertCPaths(subjects);
  sub_open = ConvertCPaths(subjects_open);
  clp = ConvertCPaths(clips);

  PolyTreeI tree;
  ClipperI clipper;
  clipper.PreserveCollinear(preserve_collinear);
  clipper.ReverseSolution(reverse_solution);
  if (sub.size() > 0) clipper.AddSubject(sub);
  if (sub_open.size() > 0) clipper.AddOpenSubject(sub_open);
  if (clp.size() > 0) clipper.AddClip(clp);
  if (!clipper.Execute(ClipType(cliptype), FillRule(fillrule), tree, sol_open))
    return -1; // clipping bug - should never happen :)

  sol_tree = CreateCPolyTree(tree, (Integer)1);
  solution_open = CreateCPaths(sol_open);
  return 0; //success !!
}

EXTERN_DLL_EXPORT int BooleanOpS(uint8_t cliptype,
  uint8_t fillrule, const CPathsS subjects,
  const CPathsS subjects_open, const CPathsS clips,
  CPathsS& solution, CPathsS& solution_open, int precision,
  bool preserve_collinear, bool reverse_solution)
{
  if (precision < -8 || precision > 8) return -5;
  if (cliptype > static_cast<uint8_t>(ClipType::Xor)) return -4;
  if (fillrule > static_cast<uint8_t>(FillRule::Negative)) return -3;
  const Scalar scale = std::pow(10, precision);

  PathsI sub, sub_open, clp, sol, sol_open;
  sub       = ConvertCPathsDToPathsI(subjects, scale);
  sub_open  = ConvertCPathsDToPathsI(subjects_open, scale);
  clp       = ConvertCPathsDToPathsI(clips, scale);

  ClipperI clipper;
  clipper.PreserveCollinear(preserve_collinear);
  clipper.ReverseSolution(reverse_solution);
  if (sub.size() > 0) clipper.AddSubject(sub);
  if (sub_open.size() > 0) clipper.AddOpenSubject(sub_open);
  if (clp.size() > 0) clipper.AddClip(clp);
  if (!clipper.Execute(ClipType(cliptype),
    FillRule(fillrule), sol, sol_open)) return -1;
  solution = CreateCPathsDFromPathsI(sol, 1 / scale);
  solution_open = CreateCPathsDFromPathsI(sol_open, 1 / scale);
  return 0;
}

EXTERN_DLL_EXPORT int BooleanOp_PolyTreeS(uint8_t cliptype,
  uint8_t fillrule, const CPathsS subjects,
  const CPathsS subjects_open, const CPathsS clips,
  CPolyTreeS& solution, CPathsS& solution_open, int precision,
  bool preserve_collinear, bool reverse_solution)
{
  if (precision < -8 || precision > 8) return -5;
  if (cliptype > static_cast<uint8_t>(ClipType::Xor)) return -4;
  if (fillrule > static_cast<uint8_t>(FillRule::Negative)) return -3;

  Scalar scale = std::pow(10, precision);

  int err = 0;
  PathsI sub, sub_open, clp, sol_open;
  sub = ConvertCPathsDToPathsI(subjects, scale);
  sub_open = ConvertCPathsDToPathsI(subjects_open, scale);
  clp = ConvertCPathsDToPathsI(clips, scale);

  PolyTreeI tree;
  ClipperI clipper;
  clipper.PreserveCollinear(preserve_collinear);
  clipper.ReverseSolution(reverse_solution);
  if (sub.size() > 0) clipper.AddSubject(sub);
  if (sub_open.size() > 0) clipper.AddOpenSubject(sub_open);
  if (clp.size() > 0) clipper.AddClip(clp);
  if (!clipper.Execute(ClipType(cliptype), FillRule(fillrule), tree, sol_open))
    return -1; // clipping bug - should never happen :)

  solution = CreateCPolyTree(tree, 1/scale);
  solution_open = CreateCPathsDFromPathsI(sol_open, 1 / scale);
  return 0; //success !!
}

EXTERN_DLL_EXPORT CPathsI InflatePathsI(const CPathsI paths,
  Scalar delta, uint8_t jointype, uint8_t endtype, Scalar miter_limit,
  Scalar arc_tolerance, bool reverse_solution)
{
  PathsI pp;
  pp = ConvertCPaths(paths);
  ClipperOffset clip_offset( miter_limit,
    arc_tolerance, reverse_solution);
  clip_offset.AddPaths(pp, JoinType(jointype), EndType(endtype));
  PathsI result;
  clip_offset.Execute(delta, result);
  return CreateCPaths(result);
}

EXTERN_DLL_EXPORT CPathsS InflatePathsS(const CPathsS paths,
  Scalar delta, uint8_t jointype, uint8_t endtype,
  int precision, Scalar miter_limit,
  Scalar arc_tolerance, bool reverse_solution)
{
  if (precision < -8 || precision > 8 || !paths) return nullptr;

  const Scalar scale = std::pow(10, precision);
  ClipperOffset clip_offset(miter_limit, arc_tolerance, reverse_solution);
  PathsI pp = ConvertCPathsDToPathsI(paths, scale);
  clip_offset.AddPaths(pp, JoinType(jointype), EndType(endtype));
  PathsI result;
  clip_offset.Execute(delta * scale, result);

  return CreateCPathsDFromPathsI(result, 1 / scale);
}

EXTERN_DLL_EXPORT CPathsI RectClipI(const CRectI& rect, const CPathsI paths)
{
  if (CRectIsEmpty(rect) || !paths) return nullptr;
  RectI r64 = CRectToRect(rect);
  class RectClipI rc(r64);
  PathsI pp = ConvertCPaths(paths);
  PathsI result = rc.Execute(pp);
  return CreateCPaths(result);
}

EXTERN_DLL_EXPORT CPathsS RectClipS(const CRectS& rect, const CPathsS paths, int precision)
{
  if (CRectIsEmpty(rect) || !paths) return nullptr;
  if (precision < -8 || precision > 8) return nullptr;
  const Scalar scale = std::pow(10, precision);

  RectS r = CRectToRect(rect);
  RectI rec = ScaleRect<Integer, Scalar>(r, scale);
  PathsI pp = ConvertCPathsDToPathsI(paths, scale);
  class RectClipI rc(rec);
  PathsI result = rc.Execute(pp);

  return CreateCPathsDFromPathsI(result, 1 / scale);
}

EXTERN_DLL_EXPORT CPathsI RectClipLinesI(const CRectI& rect,
  const CPathsI paths)
{
  if (CRectIsEmpty(rect) || !paths) return nullptr;
  RectI r = CRectToRect(rect);
  class RectClipLinesI rcl (r);
  PathsI pp = ConvertCPaths(paths);
  PathsI result = rcl.Execute(pp);
  return CreateCPaths(result);
}

EXTERN_DLL_EXPORT CPathsS RectClipLinesS(const CRectS& rect,
  const CPathsS paths, int precision)
{
  if (CRectIsEmpty(rect) || !paths) return nullptr;
  if (precision < -8 || precision > 8) return nullptr;

  const Scalar scale = std::pow(10, precision);
  RectI r = ScaleRect<Integer, Scalar>(CRectToRect(rect), scale);
  class RectClipLinesI rcl(r);
  PathsI pp = ConvertCPathsDToPathsI(paths, scale);
  PathsI result = rcl.Execute(pp);
  return CreateCPathsDFromPathsI(result, 1 / scale);
}

EXTERN_DLL_EXPORT CPathsI MinkowskiSumI(const CPathI& cpattern, const CPathI& cpath, bool is_closed)
{
  PathI path = ConvertCPath(cpath);
  PathI pattern = ConvertCPath(cpattern);
  PathsI solution = MinkowskiSum(pattern, path, is_closed);
  return CreateCPaths(solution);
}

EXTERN_DLL_EXPORT CPathsI MinkowskiDiff64(const CPathI& cpattern, const CPathI& cpath, bool is_closed)
{
  PathI path = ConvertCPath(cpath);
  PathI pattern = ConvertCPath(cpattern);
  PathsI solution = MinkowskiDiff(pattern, path, is_closed);
  return CreateCPaths(solution);
}

}  // end Clipper2Lib namespace

#endif  // CLIPPER2_EXPORT_H
