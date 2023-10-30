/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  29 October 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* Purpose   :  This module exports the Clipper2 Library (ie DLL/so)            *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

// =============================================================================
// CAUTION: THE FOLLOWING DATA STRUCTURES HAVE RECENTLY BEEN CHANGED (ver 1.2.4) 
// =============================================================================
// 
// The path structures that are used extensively in other parts of this library 
// are all based on std::vector classes. And unfortunately, C++ classes can't 
// be accessed by other languages. So all these (std::vector based) paths must 
// be converted into simple C data structures that can be understood by just
// about any programming language. And these C style data structures are mostly 
// just simple arrays. 
//
// Path64 and PathD are converted into arrays of int64_t or double values 
// respectively, representing consecutive x and y coordinates.
// However preceeding each array is single x,y pair that contains the
// path's length in the x value (and the 'y' value = 0).
// __________________________________
// |counter|coord1|coord2|...|coordN|
// |N, 0   |x1, y1|x2, y2|...|xN, yN|
// __________________________________
//
// CPaths64 and CPathsD:
// These structures are very similar to their respecitve path structures. These 
// structures not only contain any number of consecutive CPath64 or CPathD 
// structures but, preceeding these paths, there is an extra x,y pair of values 
// that contains the path count. However, in this case the x value = array length, 
// and the y value contains the count (ie the number of following paths).
// _______________________________
// |counter|path1|path2|...|pathN|
// |len, N |     |     |...|pathN|
// _______________________________
//
// 
// CPolytree64 and CPolytreeD:
// These are both simple arrays (of int64_t or double respectively). Otherwise 
// their structures are identical, and consist of series of CPolyPath structures.
// The CPolyPath structure is as follows:
//   Usually polygon length (N) except for the very first CPolyPath entry that 
//     contains the array size. (The top-most CPolyPath never contains a polygon.)
//   ChildCount (C)
//   N * x,y coordinates in Polygon
//   C * Nested child CPolyPath structures


#ifndef CLIPPER2_EXPORT_H
#define CLIPPER2_EXPORT_H

#include <cstdlib>
#include <vector>

#include "clipper2/clipper.core.h"
#include "clipper2/clipper.engine.h"
#include "clipper2/clipper.offset.h"
#include "clipper2/clipper.rectclip.h"

namespace Clipper2Lib {

typedef int64_t* CPath64;
typedef int64_t* CPaths64;
typedef double*  CPathD;
typedef double*  CPathsD;

typedef int64_t* CPolyPath64;
typedef int64_t* CPolyTree64;
typedef double* CPolyPathD;
typedef double* CPolyTreeD;

template <typename T>
struct CRect {
  T left;
  T top;
  T right;
  T bottom;
};

typedef CRect<int64_t> CRect64;
typedef CRect<double> CRectD;

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

// Most of the exported functions below return data in structures that 
// has been allocated in heap memory. Eventually this memory will 
// need to be released using one of the following 'DisposeExported...' 
// functions.  (This may be the only safe way to release this memory 
// since the executable accessing these exported functions may use 
// a different memory manager (MM)., And allocating memory using one 
// MM and releasing the same memory in another will cause problems.)
EXTERN_DLL_EXPORT void DisposeExportedCPaths64(CPaths64& p)
{
  delete[] p;
}
EXTERN_DLL_EXPORT void DisposeExportedCPathsD(CPathsD& p)
{
  delete[] p;
}
EXTERN_DLL_EXPORT void DisposeExportedCPolyTree64(CPolyTree64& p)
{
  delete[] p;
}
EXTERN_DLL_EXPORT void DisposeExportedCPolyTreeD(CPolyTreeD& p)
{
  delete[] p;
}


// Boolean clipping:
// cliptype: None=0, Intersection=1, Union=2, Difference=3, Xor=4
// fillrule: EvenOdd=0, NonZero=1, Positive=2, Negative=3
EXTERN_DLL_EXPORT int BooleanOp64(uint8_t cliptype,
  uint8_t fillrule, const CPaths64 subjects,
  const CPaths64 subjects_open, const CPaths64 clips,
  CPaths64& solution, CPaths64& solution_open,
  bool preserve_collinear = true, bool reverse_solution = false);

EXTERN_DLL_EXPORT int BooleanOp_PolyTree64(uint8_t cliptype,
  uint8_t fillrule, const CPaths64 subjects,
  const CPaths64 subjects_open, const CPaths64 clips,
  CPolyTree64& sol_tree, CPaths64& solution_open,
  bool preserve_collinear = true, bool reverse_solution = false);

EXTERN_DLL_EXPORT int BooleanOpD(uint8_t cliptype,
  uint8_t fillrule, const CPathsD subjects,
  const CPathsD subjects_open, const CPathsD clips,
  CPathsD& solution, CPathsD& solution_open, int precision = 2,
  bool preserve_collinear = true, bool reverse_solution = false);

EXTERN_DLL_EXPORT int BooleanOp_PolyTreeD(uint8_t cliptype,
  uint8_t fillrule, const CPathsD subjects,
  const CPathsD subjects_open, const CPathsD clips,
  CPolyTreeD& solution, CPathsD& solution_open, int precision = 2,
  bool preserve_collinear = true, bool reverse_solution = false);

// Polygon offsetting (inflate/deflate):
// jointype: Square=0, Round=1, Miter=2
// endtype: Polygon=0, Joined=1, Butt=2, Square=3, Round=4
EXTERN_DLL_EXPORT CPaths64 InflatePaths64(const CPaths64 paths,
  double delta, uint8_t jointype, uint8_t endtype, 
  double miter_limit = 2.0, double arc_tolerance = 0.0, 
  bool reverse_solution = false);
EXTERN_DLL_EXPORT CPathsD InflatePathsD(const CPathsD paths,
  double delta, uint8_t jointype, uint8_t endtype,
  int precision = 2, double miter_limit = 2.0,
  double arc_tolerance = 0.0, bool reverse_solution = false);

// RectClip & RectClipLines:
EXTERN_DLL_EXPORT CPaths64 RectClip64(const CRect64& rect,
  const CPaths64 paths);
EXTERN_DLL_EXPORT CPathsD RectClipD(const CRectD& rect,
  const CPathsD paths, int precision = 2);
EXTERN_DLL_EXPORT CPaths64 RectClipLines64(const CRect64& rect,
  const CPaths64 paths);
EXTERN_DLL_EXPORT CPathsD RectClipLinesD(const CRectD& rect,
  const CPathsD paths, int precision = 2);

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

static size_t GetPolyPath64ArrayLen(const PolyPath64& pp)
{
  size_t result = 2; // poly_length + child_count
  result += pp.Polygon().size() * 2;
  //plus nested children :)
  for (size_t i = 0; i < pp.Count(); ++i)
    result += GetPolyPath64ArrayLen(*pp[i]);
  return result;
}

static void GetPolytreeCountAndCStorageSize(const PolyTree64& tree, 
  size_t& cnt, size_t& array_len)
{
  cnt = tree.Count(); // nb: top level count only 
  array_len = GetPolyPath64ArrayLen(tree);
}

template <typename T>
static T* CreateCPaths(const Paths<T>& paths)
{
  size_t cnt, array_len;
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


CPathsD CreateCPathsDFromPaths64(const Paths64& paths, double scale)
{
  if (!paths.size()) return nullptr;
  size_t cnt, array_len;
  GetPathCountAndCPathsArrayLen(paths, cnt, array_len);
  CPathsD result = new double[array_len], v = result;
  *v++ = (double)array_len;
  *v++ = (double)cnt;
  for (const Path64& path : paths)
  {
    if (!path.size()) continue;
    *v = (double)path.size();
    ++v; *v++ = 0;
    for (const Point64& pt : path)
    {
      *v++ = pt.x * scale;
      *v++ = pt.y * scale;
    }
  }
  return result;
}

template <typename T>
static Paths<T> ConvertCPaths(T* paths)
{
  Paths<T> result;
  if (!paths) return result;
  T* v = paths; ++v;
  size_t cnt = *v++;
  result.reserve(cnt);
  for (size_t i = 0; i < cnt; ++i)
  {
    size_t cnt2 = *v;
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


static Paths64 ConvertCPathsDToPaths64(const CPathsD paths, double scale)
{
  Paths64 result;
  if (!paths) return result;
  double* v = paths; 
  ++v; // skip the first value (0)
  int64_t cnt = (int64_t)*v++;
  result.reserve(cnt);
  for (int i = 0; i < cnt; ++i)
  {
    int64_t cnt2 = (int64_t)*v;
    v += 2;
    Path64 path;
    path.reserve(cnt2);
    for (int j = 0; j < cnt2; ++j)
    {
      double x = *v++ * scale;
      double y = *v++ * scale;
      path.push_back(Point64(x, y));
    }
    result.push_back(path);
  }
  return result;
}

template <typename T>
static void CreateCPolyPath(const PolyPath64* pp, T*& v, T scale)
{
  *v++ = static_cast<T>(pp->Polygon().size());
  *v++ = static_cast<T>(pp->Count());
  for (const Point64& pt : pp->Polygon())
  {
    *v++ = static_cast<T>(pt.x * scale);
    *v++ = static_cast<T>(pt.y * scale);
  }
  for (size_t i = 0; i < pp->Count(); ++i)
    CreateCPolyPath(pp->Child(i), v, scale);
}

template <typename T>
static T* CreateCPolyTree(const PolyTree64& tree, T scale)
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

EXTERN_DLL_EXPORT int BooleanOp64(uint8_t cliptype, 
  uint8_t fillrule, const CPaths64 subjects,
  const CPaths64 subjects_open, const CPaths64 clips,
  CPaths64& solution, CPaths64& solution_open,
  bool preserve_collinear, bool reverse_solution)
{
  if (cliptype > static_cast<uint8_t>(ClipType::Xor)) return -4;
  if (fillrule > static_cast<uint8_t>(FillRule::Negative)) return -3;
  
  Paths64 sub, sub_open, clp, sol, sol_open;
  sub       = ConvertCPaths(subjects);
  sub_open  = ConvertCPaths(subjects_open);
  clp       = ConvertCPaths(clips);

  Clipper64 clipper;
  clipper.PreserveCollinear = preserve_collinear;
  clipper.ReverseSolution = reverse_solution;
  if (sub.size() > 0) clipper.AddSubject(sub);
  if (sub_open.size() > 0) clipper.AddOpenSubject(sub_open);
  if (clp.size() > 0) clipper.AddClip(clp);
  if (!clipper.Execute(ClipType(cliptype), FillRule(fillrule), sol, sol_open)) 
    return -1; // clipping bug - should never happen :)
  solution = CreateCPaths(sol);
  solution_open = CreateCPaths(sol_open);
  return 0; //success !!
}

EXTERN_DLL_EXPORT int BooleanOp_PolyTree64(uint8_t cliptype,
  uint8_t fillrule, const CPaths64 subjects,
  const CPaths64 subjects_open, const CPaths64 clips,
  CPolyTree64& sol_tree, CPaths64& solution_open,
  bool preserve_collinear, bool reverse_solution)
{
  if (cliptype > static_cast<uint8_t>(ClipType::Xor)) return -4;
  if (fillrule > static_cast<uint8_t>(FillRule::Negative)) return -3;
  Paths64 sub, sub_open, clp, sol_open;
  sub = ConvertCPaths(subjects);
  sub_open = ConvertCPaths(subjects_open);
  clp = ConvertCPaths(clips);

  PolyTree64 tree;
  Clipper64 clipper;
  clipper.PreserveCollinear = preserve_collinear;
  clipper.ReverseSolution = reverse_solution;
  if (sub.size() > 0) clipper.AddSubject(sub);
  if (sub_open.size() > 0) clipper.AddOpenSubject(sub_open);
  if (clp.size() > 0) clipper.AddClip(clp);
  if (!clipper.Execute(ClipType(cliptype), FillRule(fillrule), tree, sol_open))
    return -1; // clipping bug - should never happen :)

  sol_tree = CreateCPolyTree(tree, (int64_t)1);
  solution_open = CreateCPaths(sol_open);
  return 0; //success !!
}

EXTERN_DLL_EXPORT int BooleanOpD(uint8_t cliptype,
  uint8_t fillrule, const CPathsD subjects,
  const CPathsD subjects_open, const CPathsD clips,
  CPathsD& solution, CPathsD& solution_open, int precision,
  bool preserve_collinear, bool reverse_solution)
{
  if (precision < -8 || precision > 8) return -5;
  if (cliptype > static_cast<uint8_t>(ClipType::Xor)) return -4;
  if (fillrule > static_cast<uint8_t>(FillRule::Negative)) return -3;
  const double scale = std::pow(10, precision);

  Paths64 sub, sub_open, clp, sol, sol_open;
  sub       = ConvertCPathsDToPaths64(subjects, scale);
  sub_open  = ConvertCPathsDToPaths64(subjects_open, scale);
  clp       = ConvertCPathsDToPaths64(clips, scale);

  Clipper64 clipper;
  clipper.PreserveCollinear = preserve_collinear;
  clipper.ReverseSolution = reverse_solution;
  if (sub.size() > 0) clipper.AddSubject(sub);
  if (sub_open.size() > 0) clipper.AddOpenSubject(sub_open);
  if (clp.size() > 0) clipper.AddClip(clp);
  if (!clipper.Execute(ClipType(cliptype),
    FillRule(fillrule), sol, sol_open)) return -1;
  solution = CreateCPathsDFromPaths64(sol, 1 / scale);
  solution_open = CreateCPathsDFromPaths64(sol_open, 1 / scale);
  return 0;
}

EXTERN_DLL_EXPORT int BooleanOp_PolyTreeD(uint8_t cliptype,
  uint8_t fillrule, const CPathsD subjects,
  const CPathsD subjects_open, const CPathsD clips,
  CPolyTreeD& solution, CPathsD& solution_open, int precision,
  bool preserve_collinear, bool reverse_solution)
{
  if (precision < -8 || precision > 8) return -5;
  if (cliptype > static_cast<uint8_t>(ClipType::Xor)) return -4;
  if (fillrule > static_cast<uint8_t>(FillRule::Negative)) return -3;
  
  double scale = std::pow(10, precision);

  int err = 0;
  Paths64 sub, sub_open, clp, sol_open;
  sub = ConvertCPathsDToPaths64(subjects, scale);
  sub_open = ConvertCPathsDToPaths64(subjects_open, scale);
  clp = ConvertCPathsDToPaths64(clips, scale);

  PolyTree64 tree;
  Clipper64 clipper;
  clipper.PreserveCollinear = preserve_collinear;
  clipper.ReverseSolution = reverse_solution;
  if (sub.size() > 0) clipper.AddSubject(sub);
  if (sub_open.size() > 0) clipper.AddOpenSubject(sub_open);
  if (clp.size() > 0) clipper.AddClip(clp);
  if (!clipper.Execute(ClipType(cliptype), FillRule(fillrule), tree, sol_open))
    return -1; // clipping bug - should never happen :)

  solution = CreateCPolyTree(tree, 1/scale);
  solution_open = CreateCPathsDFromPaths64(sol_open, 1 / scale);
  return 0; //success !!
}

EXTERN_DLL_EXPORT CPaths64 InflatePaths64(const CPaths64 paths,
  double delta, uint8_t jointype, uint8_t endtype, double miter_limit,
  double arc_tolerance, bool reverse_solution)
{
  Paths64 pp;
  pp = ConvertCPaths(paths);
  ClipperOffset clip_offset( miter_limit, 
    arc_tolerance, reverse_solution);
  clip_offset.AddPaths(pp, JoinType(jointype), EndType(endtype));
  Paths64 result; 
  clip_offset.Execute(delta, result);
  return CreateCPaths(result);
}

EXTERN_DLL_EXPORT CPathsD InflatePathsD(const CPathsD paths,
  double delta, uint8_t jointype, uint8_t endtype,
  int precision, double miter_limit,
  double arc_tolerance, bool reverse_solution)
{
  if (precision < -8 || precision > 8 || !paths) return nullptr;
  const double scale = std::pow(10, precision);
  ClipperOffset clip_offset(miter_limit, arc_tolerance, reverse_solution);
  Paths64 pp = ConvertCPathsDToPaths64(paths, scale);
  clip_offset.AddPaths(pp, JoinType(jointype), EndType(endtype));
  Paths64 result;
  clip_offset.Execute(delta * scale, result);
  return CreateCPathsDFromPaths64(result, 1/scale);
}

EXTERN_DLL_EXPORT CPaths64 RectClip64(const CRect64& rect, const CPaths64 paths)
{
  if (CRectIsEmpty(rect) || !paths) return nullptr;
  Rect64 r64 = CRectToRect(rect);
  class RectClip64 rc(r64);
  Paths64 pp = ConvertCPaths(paths);
  Paths64 result = rc.Execute(pp);
  return CreateCPaths(result);
}

EXTERN_DLL_EXPORT CPathsD RectClipD(const CRectD& rect, const CPathsD paths, int precision)
{
  if (CRectIsEmpty(rect) || !paths) return nullptr;
  if (precision < -8 || precision > 8) return nullptr;
  const double scale = std::pow(10, precision);

  RectD r = CRectToRect(rect);
  Rect64 rec = ScaleRect<int64_t, double>(r, scale);
  Paths64 pp = ConvertCPathsDToPaths64(paths, scale);
  class RectClip64 rc(rec);
  Paths64 result = rc.Execute(pp);
  return CreateCPathsDFromPaths64(result, 1/scale);
}

EXTERN_DLL_EXPORT CPaths64 RectClipLines64(const CRect64& rect,
  const CPaths64 paths)
{
  if (CRectIsEmpty(rect) || !paths) return nullptr;
  Rect64 r = CRectToRect(rect);
  class RectClipLines64 rcl (r);
  Paths64 pp = ConvertCPaths(paths);
  Paths64 result = rcl.Execute(pp);
  return CreateCPaths(result);
}

EXTERN_DLL_EXPORT CPathsD RectClipLinesD(const CRectD& rect,
  const CPathsD paths, int precision)
{
  if (CRectIsEmpty(rect) || !paths) return nullptr;
  if (precision < -8 || precision > 8) return nullptr;
  const double scale = std::pow(10, precision);
  Rect64 r = ScaleRect<int64_t, double>(CRectToRect(rect), scale);
  class RectClipLines64 rcl(r);
  Paths64 pp = ConvertCPathsDToPaths64(paths, scale);
  Paths64 result = rcl.Execute(pp);
  return CreateCPathsDFromPaths64(result, 1/scale);
}

}  // end Clipper2Lib namespace
  
#endif  // CLIPPER2_EXPORT_H
