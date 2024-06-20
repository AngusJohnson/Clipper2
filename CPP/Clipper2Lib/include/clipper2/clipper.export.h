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
(of either int64_t for CPath64 or double for CPathD) that can be parsed by 
just about any programming language.

The letters in the diagrams below can be interpreted as follows
N: Number of vertices
A: Array size
C: Child count

CPath64 and CPathD:
These are arrays of consecutive x, y and optional z path coordinates 
preceeded by a pair of values containing the path's length (N) and a 0 value.
____________________________________________________
|counter|   coord1   |   coord2   |...|   coordN   |
|N, 0   |x1, y1, (z1)|x2, y2, (z2)|...|xN, yN, (zN)|
____________________________________________________

z-coordinates are output only if both the USINGZ pre-processor
identifier exists

Below we will see that CPath is a special case of the more general CPolyPath

CPaths64 and CPathsD:
These are also arrays containing any number of consecutive CPath64 or
CPathD  structures. But preceeding these consecutive paths, there is pair of
values that contain the total length of the array structure (A) and the 
number of CPath64 or CPathD it contains (C). The space these structures will
occupy in memory = A * sizeof(int64_t) or A * sizeof(double) respectively. 
_______________________________
|counter|path1|path2|...|pathC|
|A  , C |     |     |   |     |
_______________________________

Below we will see that CPaths is a special case of the more general CPolyTree

CPolyPath64 and CPolyPathD:
These are simple arrays consisting of a series of path coordinates followed
by any number of child (ie nested) CPolyPath. Preceeding these are two values
indicating the length of the path (N) and the number of child CPolyPath (C).
_____________________________________________________________________________
|counter|    coord1  |   coord2   |...|   coordN   |child1|child2|...|childC|
|N  , C |x1, y1, (z1)|x2, y2, (z2)|...|xN, yN, (zN)|      |      |   |      |
_____________________________________________________________________________

CPath is a special case of CPolyPath with C = 0 and no children after the 
coordinates.

CPolyTree64 and CPolyTreeD:
CPolyTree has an identical structure to CPaths, but the children are the more 
generalized CPolyPath. We can think of CPaths as a special case of CPolyTree 
with only one level of children
___________________________________________
|counter|polypath1|polypath2|...|polypathC|
|A  , C |         |         |   |         |
___________________________________________

CPolyTree and CPaths own the memory of the paths they contain. In the case
of CPolyTree it is the memory of every CPolyPath in the tree. Just like for 
CPaths the first value (A) represents the number of elements in the array, 
and the size in bytes is A * sizeof(int64_t) or A * sizeof(double). 

The value C represents the number of direct children meaning the number of 
top-level CPolyPaths only.

Again, all theses exported structures (CPaths64, CPathsD, CPolyTree64 & 
CPolyTreeD) are arrays of either type int64_t or double, and the first 
value in these arrays will always be the length of that array.

These array structures are allocated in heap memory which will eventually
need to be released. However, since applications dynamically linking to 
these functions may use different memory managers, the only safe way to 
free up this memory is to use the exported DisposeArray64 and 
DisposeArrayD functions (see below).
*/


#ifndef CLIPPER2_EXPORT_H
#define CLIPPER2_EXPORT_H

#include <cstdlib>
#include <vector>
#if __cplusplus >= 202002L // Check for C++20 or later
    #include <bit> //we have a polyfill for earlier C++ below
#endif

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

EXTERN_DLL_EXPORT void DisposeArray64(int64_t*& p)
{
  delete[] p;
}

EXTERN_DLL_EXPORT void DisposeArrayD(double*& p)
{
  delete[] p;
}

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

EXTERN_DLL_EXPORT CPaths64 InflatePaths64(const CPaths64 paths,
  double delta, uint8_t jointype, uint8_t endtype,
  double miter_limit = 2.0, double arc_tolerance = 0.0,
  bool reverse_solution = false);

EXTERN_DLL_EXPORT CPathsD InflatePathsD(const CPathsD paths,
  double delta, uint8_t jointype, uint8_t endtype,
  int precision = 2, double miter_limit = 2.0,
  double arc_tolerance = 0.0, bool reverse_solution = false);

EXTERN_DLL_EXPORT CPaths64 InflatePath64(const CPath64 path,
    double delta, uint8_t jointype, uint8_t endtype,
    double miter_limit = 2.0, double arc_tolerance = 0.0,
    bool reverse_solution = false);

EXTERN_DLL_EXPORT CPathsD InflatePathD(const CPathD path,
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

///////////////// C++ 20 Polyfill ////////////////////
#if __cplusplus >= 202002L // Check for C++20 or later
// C++20 or later: Use std::bit_cast
template<class To, class From>
constexpr bit_cast(const From& src) noexcept {
    return std::bit_cast<To>(src);
}
#else
// from https://en.cppreference.com/w/cpp/numeric/bit_cast (see possible implementation)
template<class To, class From>
std::enable_if_t<
    sizeof(To) == sizeof(From) &&
    std::is_trivially_copyable_v<From> &&
    std::is_trivially_copyable_v<To>,
    To>

bit_cast(const From& src) noexcept
{
    To dst;
    std::memcpy(&dst, &src, sizeof(To));
    return dst;
}

#endif


//////////////////////////////////////////////////////
// INTERNAL FUNCTIONS
//////////////////////////////////////////////////////

#ifdef USINGZ
constexpr int EXPORT_VERTEX_DIMENSIONALITY = 3;
#else    
constexpr int EXPORT_VERTEX_DIMENSIONALITY  = 2;
#endif 

template <typename T>
static void GetPathCountAndCPathsArrayLen(const Paths<T>& paths,
  size_t& cnt, size_t& array_len)
{
  array_len = 2;
  cnt = 0;
  for (const Path<T>& path : paths)
    if (path.size())
    {
      array_len += path.size() * EXPORT_VERTEX_DIMENSIONALITY + 2;
      ++cnt;
    }
}

static size_t GetPolyPath64ArrayLen(const PolyPath64& pp)
{
  size_t result = 2; // poly_length + child_count
  result += pp.Polygon().size() * EXPORT_VERTEX_DIMENSIONALITY;
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

#ifdef USINGZ
      *v++ = bit_cast<T>(pt.z);
#endif

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

#ifdef USINGZ
      *v++ = bit_cast<double>(pt.z); //needs a polyfill for 17
#endif

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

#ifdef USINGZ
      auto z = bit_cast<z_type>(*v++);
      result.push_back(Point<T>(x, y, z));
#else  
      result.push_back(Point<T>(x, y));
#endif
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
 
#ifdef USINGZ
      auto z = bit_cast<z_type>(*v++);
      path.push_back(Point<T>(x, y, z));
#else
      path.push_back(Point<T>(x, y));
#endif

    }
    result.push_back(path);
  }
  return result;
}

static Path64 ConvertCPathsDToPath64(const CPathD path, double scale)
{
    Path64 result;
    if (!path) return result;
    double* v = path;
    size_t cnt = static_cast<size_t>(*v);
    v += 2; // skip 0 value
    result.reserve(cnt);
    for (size_t j = 0; j < cnt; ++j)
    {
        double x = *v++ * scale;
        double y = *v++ * scale;

#ifdef USINGZ
        auto z = bit_cast<z_type>(*v++);
        result.push_back(Point64(x, y, z));
#else  
        result.push_back(Point64(x, y));
#endif
    }
    return result;
}


static Paths64 ConvertCPathsDToPaths64(const CPathsD paths, double scale)
{
  Paths64 result;
  if (!paths) return result;
  double* v = paths;
  ++v; // skip the first value (0)
  size_t cnt = static_cast<size_t>(*v++);
  result.reserve(cnt);
  for (size_t i = 0; i < cnt; ++i)
  {
    size_t cnt2 = static_cast<size_t>(*v);
    v += 2;
    Path64 path;
    path.reserve(cnt2);
    for (size_t j = 0; j < cnt2; ++j)
    {
      double x = *v++ * scale;
      double y = *v++ * scale;

#ifdef USINGZ
      auto z = bit_cast<z_type>(*v++);
      path.push_back(Point64(x, y, z));
#else
      path.push_back(Point64(x, y));
#endif
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

#ifdef USINGZ   
    *v++ = bit_cast<T>(pt.z); // raw memory copy
#endif

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
  clipper.PreserveCollinear(preserve_collinear);
  clipper.ReverseSolution(reverse_solution);
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
  clipper.PreserveCollinear(preserve_collinear);
  clipper.ReverseSolution(reverse_solution);
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
  clipper.PreserveCollinear(preserve_collinear);
  clipper.ReverseSolution(reverse_solution);
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

  return CreateCPathsDFromPaths64(result, 1 / scale);
}


EXTERN_DLL_EXPORT CPaths64 InflatePath64(const CPath64 path,
    double delta, uint8_t jointype, uint8_t endtype, double miter_limit,
    double arc_tolerance, bool reverse_solution)
{
    Path64 pp;
    pp = ConvertCPath(path);
    ClipperOffset clip_offset(miter_limit,
        arc_tolerance, reverse_solution);
    clip_offset.AddPath(pp, JoinType(jointype), EndType(endtype));
    Paths64 result;
    clip_offset.Execute(delta, result);
    return CreateCPaths(result);
}

EXTERN_DLL_EXPORT CPathsD InflatePathD(const CPathD path,
    double delta, uint8_t jointype, uint8_t endtype,
    int precision, double miter_limit,
    double arc_tolerance, bool reverse_solution)
{
    if (precision < -8 || precision > 8 || !path) return nullptr;

    const double scale = std::pow(10, precision);
    ClipperOffset clip_offset(miter_limit, arc_tolerance, reverse_solution);
    Path64 pp = ConvertCPathsDToPath64(path, scale);
    clip_offset.AddPath(pp, JoinType(jointype), EndType(endtype));
    Paths64 result;
    clip_offset.Execute(delta * scale, result);

    return CreateCPathsDFromPaths64(result, 1 / scale);
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

  return CreateCPathsDFromPaths64(result, 1 / scale);
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
  return CreateCPathsDFromPaths64(result, 1 / scale);
}

EXTERN_DLL_EXPORT CPaths64 MinkowskiSum64(const CPath64& cpattern, const CPath64& cpath, bool is_closed)
{
  Path64 path = ConvertCPath(cpath);
  Path64 pattern = ConvertCPath(cpattern);
  Paths64 solution = MinkowskiSum(pattern, path, is_closed);
  return CreateCPaths(solution);
}

EXTERN_DLL_EXPORT CPaths64 MinkowskiDiff64(const CPath64& cpattern, const CPath64& cpath, bool is_closed)
{
  Path64 path = ConvertCPath(cpath);
  Path64 pattern = ConvertCPath(cpattern);
  Paths64 solution = MinkowskiDiff(pattern, path, is_closed);
  return CreateCPaths(solution);
}

#ifdef USINGZ
typedef void (*DLLZCallback)(const Point64& e1bot, const Point64& e1top, const Point64& e2bot, const Point64& e2top, Point64& pt);

EXTERN_DLL_EXPORT void SetDefaultZCallback(DLLZCallback callback)
{
    if (callback)
        ClipperBase::DefaultZCallback = [callback](const Point64& e1bot, const Point64& e1top, const Point64 e2bot, const Point64& e2top, Point64& pt)
        { callback(e1bot, e1top, e2bot, e2top, pt);  };
    else
        ClipperBase::DefaultZCallback = nullptr;
}  
#endif

}
#endif  // CLIPPER2_EXPORT_H
