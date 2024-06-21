/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  27 April 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  This module provides a simple interface to the Clipper Library  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_H
#define CLIPPER_H

#include <cstdlib>
#include <type_traits>
#include <vector>

#include "clipper2/clipper.core.h"
#include "clipper2/clipper.engine.h"
#include "clipper2/clipper.offset.h"
#include "clipper2/clipper.minkowski.h"
#include "clipper2/clipper.rectclip.h"

namespace Clipper2Lib {

  inline PathsI BooleanOp(ClipType cliptype, FillRule fillrule,
    const PathsI& subjects, const PathsI& clips)
  {
    PathsI result;
    ClipperI clipper;
    clipper.AddSubject(subjects);
    clipper.AddClip(clips);
    clipper.Execute(cliptype, fillrule, result);
    return result;
  }

  inline void BooleanOp(ClipType cliptype, FillRule fillrule,
    const PathsI& subjects, const PathsI& clips, PolyTreeI& solution)
  {
    PathsI sol_open;
    ClipperI clipper;
    clipper.AddSubject(subjects);
    clipper.AddClip(clips);
    clipper.Execute(cliptype, fillrule, solution, sol_open);
  }

  inline PathsS BooleanOp(ClipType cliptype, FillRule fillrule,
    const PathsS& subjects, const PathsS& clips, int precision = 2)
  {
    int error_code = 0;
    CheckPrecisionRange(precision, error_code);
    PathsS result;
    if (error_code) return result;
    ClipperS clipper(precision);
    clipper.AddSubject(subjects);
    clipper.AddClip(clips);
    clipper.Execute(cliptype, fillrule, result);
    return result;
  }

  inline void BooleanOp(ClipType cliptype, FillRule fillrule,
    const PathsS& subjects, const PathsS& clips,
    PolyTreeD& polytree, int precision = 2)
  {
    polytree.Clear();
    int error_code = 0;
    CheckPrecisionRange(precision, error_code);
    if (error_code) return;
    ClipperS clipper(precision);
    clipper.AddSubject(subjects);
    clipper.AddClip(clips);
    clipper.Execute(cliptype, fillrule, polytree);
  }

  inline PathsI Intersect(const PathsI& subjects, const PathsI& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Intersection, fillrule, subjects, clips);
  }

  inline PathsS Intersect(const PathsS& subjects, const PathsS& clips, FillRule fillrule, int decimal_prec = 2)
  {
    return BooleanOp(ClipType::Intersection, fillrule, subjects, clips, decimal_prec);
  }

  inline PathsI Union(const PathsI& subjects, const PathsI& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Union, fillrule, subjects, clips);
  }

  inline PathsS Union(const PathsS& subjects, const PathsS& clips, FillRule fillrule, int decimal_prec = 2)
  {
    return BooleanOp(ClipType::Union, fillrule, subjects, clips, decimal_prec);
  }

  inline PathsI Union(const PathsI& subjects, FillRule fillrule)
  {
    PathsI result;
    ClipperI clipper;
    clipper.AddSubject(subjects);
    clipper.Execute(ClipType::Union, fillrule, result);
    return result;
  }

  inline PathsS Union(const PathsS& subjects, FillRule fillrule, int precision = 2)
  {
    PathsS result;
    int error_code = 0;
    CheckPrecisionRange(precision, error_code);
    if (error_code) return result;
    ClipperS clipper(precision);
    clipper.AddSubject(subjects);
    clipper.Execute(ClipType::Union, fillrule, result);
    return result;
  }

  inline PathsI Difference(const PathsI& subjects, const PathsI& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Difference, fillrule, subjects, clips);
  }

  inline PathsS Difference(const PathsS& subjects, const PathsS& clips, FillRule fillrule, int decimal_prec = 2)
  {
    return BooleanOp(ClipType::Difference, fillrule, subjects, clips, decimal_prec);
  }

  inline PathsI Xor(const PathsI& subjects, const PathsI& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Xor, fillrule, subjects, clips);
  }

  inline PathsS Xor(const PathsS& subjects, const PathsS& clips, FillRule fillrule, int decimal_prec = 2)
  {
    return BooleanOp(ClipType::Xor, fillrule, subjects, clips, decimal_prec);
  }

  inline PathsI InflatePaths(const PathsI& paths, Scalar delta,
    JoinType jt, EndType et, Scalar miter_limit = 2.0,
    Scalar arc_tolerance = 0.0)
  {
    if (!delta) return paths;
    ClipperOffset clip_offset(miter_limit, arc_tolerance);
    clip_offset.AddPaths(paths, jt, et);
    PathsI solution;
    clip_offset.Execute(delta, solution);
    return solution;
  }

  inline PathsS InflatePaths(const PathsS& paths, Scalar delta,
    JoinType jt, EndType et, Scalar miter_limit = 2.0,
    int precision = 2, Scalar arc_tolerance = 0.0)
  {
    int error_code = 0;
    CheckPrecisionRange(precision, error_code);
    if (!delta) return paths;
    if (error_code) return PathsS();
    const Scalar scale = (Scalar)std::pow(10, precision);
    ClipperOffset clip_offset(miter_limit, arc_tolerance);
    clip_offset.AddPaths(ScalePaths<Integer,Scalar>(paths, scale, error_code), jt, et);
    if (error_code) return PathsS();
    PathsI solution;
    clip_offset.Execute(delta * scale, solution);
    return ScalePaths<Scalar, Integer>(solution, 1 / scale, error_code);
  }

  template <typename T>
  inline Path<T> TranslatePath(const Path<T>& path, T dx, T dy)
  {
    Path<T> result;
    result.reserve(path.size());
    std::transform(path.begin(), path.end(), back_inserter(result),
      [dx, dy](const auto& pt) { return Point<T>(pt.x + dx, pt.y +dy); });
    return result;
  }

  inline PathI TranslatePath(const PathI& path, Integer dx, Integer dy)
  {
    return TranslatePath<Integer>(path, dx, dy);
  }

  inline PathS TranslatePath(const PathS& path, Scalar dx, Scalar dy)
  {
    return TranslatePath<Scalar>(path, dx, dy);
  }

  template <typename T>
  inline Paths<T> TranslatePaths(const Paths<T>& paths, T dx, T dy)
  {
    Paths<T> result;
    result.reserve(paths.size());
    std::transform(paths.begin(), paths.end(), back_inserter(result),
      [dx, dy](const auto& path) { return TranslatePath(path, dx, dy); });
    return result;
  }

  inline PathsI TranslatePaths(const PathsI& paths, Integer dx, Integer dy)
  {
    return TranslatePaths<Integer>(paths, dx, dy);
  }

  inline PathsS TranslatePaths(const PathsS& paths, Scalar dx, Scalar dy)
  {
    return TranslatePaths<Scalar>(paths, dx, dy);
  }

  inline PathsI RectClip(const RectI& rect, const PathsI& paths)
  {
    if (rect.IsEmpty() || paths.empty()) return PathsI();
    RectClipI rc(rect);
    return rc.Execute(paths);
  }

  inline PathsI RectClip(const RectI& rect, const PathI& path)
  {
    if (rect.IsEmpty() || path.empty()) return PathsI();
    RectClipI rc(rect);
    return rc.Execute(PathsI{ path });
  }

  inline PathsS RectClip(const RectS& rect, const PathsS& paths, int precision = 2)
  {
    if (rect.IsEmpty() || paths.empty()) return PathsS();
    int error_code = 0;
    CheckPrecisionRange(precision, error_code);
    if (error_code) return PathsS();
    const Scalar scale = (Scalar)std::pow(10, precision);
    RectI r = ScaleRect<Integer, Scalar>(rect, scale);
    RectClipI rc(r);
    PathsI pp = ScalePaths<Integer, Scalar>(paths, scale, error_code);
    if (error_code) return PathsS(); // ie: error_code result is lost
    return ScalePaths<Scalar, Integer>(
      rc.Execute(pp), 1 / scale, error_code);
  }

  inline PathsS RectClip(const RectS& rect, const PathS& path, int precision = 2)
  {
    return RectClip(rect, PathsS{ path }, precision);
  }

  inline PathsI RectClipLines(const RectI& rect, const PathsI& lines)
  {
    if (rect.IsEmpty() || lines.empty()) return PathsI();
    RectClipLinesI rcl(rect);
    return rcl.Execute(lines);
  }

  inline PathsI RectClipLines(const RectI& rect, const PathI& line)
  {
    return RectClipLines(rect, PathsI{ line });
  }

  inline PathsS RectClipLines(const RectS& rect, const PathsS& lines, int precision = 2)
  {
    if (rect.IsEmpty() || lines.empty()) return PathsS();
    int error_code = 0;
    CheckPrecisionRange(precision, error_code);
    if (error_code) return PathsS();
    const Scalar scale = (Scalar)std::pow(10, precision);
    RectI r = ScaleRect<Integer, Scalar>(rect, scale);
    RectClipLinesI rcl(r);
    PathsI p = ScalePaths<Integer, Scalar>(lines, scale, error_code);
    if (error_code) return PathsS();
    p = rcl.Execute(p);
    return ScalePaths<Scalar, Integer>(p, 1 / scale, error_code);
  }

  inline PathsS RectClipLines(const RectS& rect, const PathS& line, int precision = 2)
  {
    return RectClipLines(rect, PathsS{ line }, precision);
  }

  namespace details
  {

    inline void PolyPathToPathsI(const PolyPathI& polypath, PathsI& paths)
    {
      paths.push_back(polypath.Polygon());
      for (const auto& child : polypath)
        PolyPathToPathsI(*child, paths);
    }

    inline void PolyPathToPathsS(const PolyPathS& polypath, PathsS& paths)
    {
      paths.push_back(polypath.Polygon());
      for (const auto& child : polypath)
        PolyPathToPathsS(*child, paths);
    }

    inline bool PolyPathIContainsChildren(const PolyPathI& pp)
    {
      for (const auto& child : pp)
      {
        // return false if this child isn't fully contained by its parent

        // checking for a single vertex outside is a bit too crude since
        // it doesn't account for rounding errors. It's better to check
        // for consecutive vertices found outside the parent's polygon.

        int outsideCnt = 0;
        for (const PointI& pt : child->Polygon())
        {
          PointInPolygonResult result = PointInPolygon(pt, pp.Polygon());
          if (result == PointInPolygonResult::IsInside) --outsideCnt;
          else if (result == PointInPolygonResult::IsOutside) ++outsideCnt;
          if (outsideCnt > 1) return false;
          else if (outsideCnt < -1) break;
        }

        // now check any nested children too
        if (child->Count() > 0 && !PolyPathIContainsChildren(*child))
          return false;
      }
      return true;
    }

    static void OutlinePolyPath(std::ostream& os,
      size_t idx, bool isHole, size_t count, const std::string& preamble)
    {
      std::string plural = (count == 1) ? "." : "s.";
      if (isHole)
        os << preamble << "+- Hole (" << idx << ") contains " << count <<
        " nested polygon" << plural << std::endl;
      else
        os << preamble << "+- Polygon (" << idx << ") contains " << count <<
          " hole" << plural << std::endl;
    }

    static void OutlinePolyPathI(std::ostream& os, const PolyPathI& pp,
      size_t idx, std::string preamble)
    {
      OutlinePolyPath(os, idx, pp.IsHole(), pp.Count(), preamble);
      for (size_t i = 0; i < pp.Count(); ++i)
        if (pp.Child(i)->Count())
          details::OutlinePolyPathI(os, *pp.Child(i), i, preamble + "  ");
    }

    static void OutlinePolyPathS(std::ostream& os, const PolyPathS& pp,
      size_t idx, std::string preamble)
    {
      OutlinePolyPath(os, idx, pp.IsHole(), pp.Count(), preamble);
      for (size_t i = 0; i < pp.Count(); ++i)
        if (pp.Child(i)->Count())
          details::OutlinePolyPathS(os, *pp.Child(i), i, preamble + "  ");
    }

    template<typename T, typename U>
    inline constexpr void MakePathGeneric(const T an_array,
      size_t array_size, std::vector<U>& result)
    {
      result.reserve(array_size / 2);
      for (size_t i = 0; i < array_size; i +=2)
#ifdef USINGZ
        result.push_back( U{ an_array[i], an_array[i + 1], 0} );
#else
        result.push_back( U{ an_array[i], an_array[i + 1]} );
#endif
    }

  } // end details namespace

  inline std::ostream& operator<< (std::ostream& os, const PolyTreeI& pp)
  {
    std::string plural = (pp.Count() == 1) ? " polygon." : " polygons.";
    os << std::endl << "Polytree with " << pp.Count() << plural << std::endl;
      for (size_t i = 0; i < pp.Count(); ++i)
        if (pp.Child(i)->Count())
          details::OutlinePolyPathI(os, *pp.Child(i), i, "  ");
    os << std::endl << std::endl;
    return os;
  }

  inline std::ostream& operator<< (std::ostream& os, const PolyTreeD& pp)
  {
    std::string plural = (pp.Count() == 1) ? " polygon." : " polygons.";
    os << std::endl << "Polytree with " << pp.Count() << plural << std::endl;
    for (size_t i = 0; i < pp.Count(); ++i)
      if (pp.Child(i)->Count())
        details::OutlinePolyPathS(os, *pp.Child(i), i, "  ");
    os << std::endl << std::endl;
    if (!pp.Level()) os << std::endl;
    return os;
  }

  inline PathsI PolyTreeToPathsI(const PolyTreeI& polytree)
  {
    PathsI result;
    for (const auto& child : polytree)
      details::PolyPathToPathsI(*child, result);
    return result;
  }

  inline PathsS PolyTreeToPathsS(const PolyTreeD& polytree)
  {
    PathsS result;
    for (const auto& child : polytree)
      details::PolyPathToPathsS(*child, result);
    return result;
  }

  inline bool CheckPolytreeFullyContainsChildren(const PolyTreeI& polytree)
  {
    for (const auto& child : polytree)
      if (child->Count() > 0 &&
        !details::PolyPathIContainsChildren(*child))
          return false;
    return true;
  }

  template<typename T,
    typename std::enable_if<
      std::is_integral<T>::value &&
      !std::is_same<char, T>::value, bool
    >::type = true>
  inline PathI MakePath(const std::vector<T>& list)
  {
    const auto size = list.size() - list.size() % 2;
    if (list.size() != size)
      DoError(non_pair_error_i);  // non-fatal without exception handling
    PathI result;
    details::MakePathGeneric(list, size, result);
    return result;
  }

  template<typename T, std::size_t N,
    typename std::enable_if<
      std::is_integral<T>::value &&
      !std::is_same<char, T>::value, bool
    >::type = true>
  inline PathI MakePath(const T(&list)[N])
  {
    // Make the compiler error on unpaired value (i.e. no runtime effects).
    static_assert(N % 2 == 0, "MakePath requires an even number of arguments");
    PathI result;
    details::MakePathGeneric(list, N, result);
    return result;
  }

  template<typename T,
    typename std::enable_if<
      std::is_arithmetic<T>::value &&
      !std::is_same<char, T>::value, bool
    >::type = true>
  inline PathS MakePathS(const std::vector<T>& list)
  {
    const auto size = list.size() - list.size() % 2;
    if (list.size() != size)
      DoError(non_pair_error_i);  // non-fatal without exception handling
    PathS result;
    details::MakePathGeneric(list, size, result);
    return result;
  }

  template<typename T, std::size_t N,
    typename std::enable_if<
      std::is_arithmetic<T>::value &&
      !std::is_same<char, T>::value, bool
    >::type = true>
  inline PathS MakePathS(const T(&list)[N])
  {
    // Make the compiler error on unpaired value (i.e. no runtime effects).
    static_assert(N % 2 == 0, "MakePath requires an even number of arguments");
    PathS result;
    details::MakePathGeneric(list, N, result);
    return result;
  }

#ifdef USINGZ
  template<typename T2, std::size_t N>
  inline PathI MakePathZ(const T2(&list)[N])
  {
    static_assert(N % 3 == 0 && std::numeric_limits<T2>::is_integer,
      "MakePathZ requires integer values in multiples of 3");
    std::size_t size = N / 3;
    PathI result(size);
    for (size_t i = 0; i < size; ++i)
      result[i] = PointI(list[i * 3],
        list[i * 3 + 1], list[i * 3 + 2]);
    return result;
  }

  template<typename T2, std::size_t N>
  inline PathS MakePathZS(const T2(&list)[N])
  {
    static_assert(N % 3 == 0,
      "MakePathZS requires values in multiples of 3");
    std::size_t size = N / 3;
    PathS result(size);
    if constexpr (std::numeric_limits<T2>::is_integer)
      for (size_t i = 0; i < size; ++i)
        result[i] = PointS(list[i * 3],
          list[i * 3 + 1], list[i * 3 + 2]);
    else
      for (size_t i = 0; i < size; ++i)
        result[i] = PointS(list[i * 3], list[i * 3 + 1],
          static_cast<Integer>(list[i * 3 + 2]));
    return result;
  }
#endif

  inline PathI TrimCollinear(const PathI& p, bool is_open_path = false)
  {
    size_t len = p.size();
    if (len < 3)
    {
      if (!is_open_path || len < 2 || p[0] == p[1]) return PathI();
      else return p;
    }

    PathI dst;
    dst.reserve(len);
    PathI::const_iterator srcIt = p.cbegin(), prevIt, stop = p.cend() - 1;

    if (!is_open_path)
    {
      while (srcIt != stop && IsCollinear(*stop, *srcIt, *(srcIt + 1)))
        ++srcIt;
      while (srcIt != stop && IsCollinear(*(stop - 1), *stop, *srcIt))
        --stop;
      if (srcIt == stop) return PathI();
    }

    prevIt = srcIt++;
    dst.push_back(*prevIt);
    for (; srcIt != stop; ++srcIt)
    {
      if (!IsCollinear(*prevIt, *srcIt, *(srcIt + 1)))
      {
        prevIt = srcIt;
        dst.push_back(*prevIt);
      }
    }

    if (is_open_path)
      dst.push_back(*srcIt);
    else if (!IsCollinear(*prevIt, *stop, dst[0]))
      dst.push_back(*stop);
    else
    {
      while (dst.size() > 2 &&
        IsCollinear(dst[dst.size() - 1], dst[dst.size() - 2], dst[0]))
          dst.pop_back();
      if (dst.size() < 3) return PathI();
    }
    return dst;
  }

  inline PathS TrimCollinear(const PathS& path, int precision, bool is_open_path = false)
  {
    int error_code = 0;
    CheckPrecisionRange(precision, error_code);
    if (error_code) return PathS();
    const Scalar scale = (Scalar)std::pow(10, precision);
    PathI p = ScalePath<Integer, Scalar>(path, scale, error_code);
    if (error_code) return PathS();
    p = TrimCollinear(p, is_open_path);
    return ScalePath<Scalar, Integer>(p, 1/scale, error_code);
  }

  template <typename T>
  inline Scalar Distance(const Point<T> pt1, const Point<T> pt2)
  {
    return std::sqrt(DistanceSqr(pt1, pt2));
  }

  template <typename T>
  inline Scalar Length(const Path<T>& path, bool is_closed_path = false)
  {
    Scalar result = 0.0;
    if (path.size() < 2) return result;
    auto it = path.cbegin(), stop = path.end() - 1;
    for (; it != stop; ++it)
      result += Distance(*it, *(it + 1));
    if (is_closed_path)
      result += Distance(*stop, *path.cbegin());
    return result;
  }


  template <typename T>
  inline bool NearCollinear(const Point<T>& pt1, const Point<T>& pt2, const Point<T>& pt3, Scalar sin_sqrd_min_angle_rads)
  {
    Scalar cp = std::abs(CrossProduct(pt1, pt2, pt3));
    return (cp * cp) / (DistanceSqr(pt1, pt2) * DistanceSqr(pt2, pt3)) < sin_sqrd_min_angle_rads;
  }

  template <typename T>
  inline Path<T> Ellipse(const Rect<T>& rect, int steps = 0)
  {
    return Ellipse(rect.MidPoint(),
      static_cast<Scalar>(rect.Width()) *0.5,
      static_cast<Scalar>(rect.Height()) * 0.5, steps);
  }

  template <typename T>
  inline Path<T> Ellipse(const Point<T>& center,
    Scalar radiusX, Scalar radiusY = 0, int steps = 0)
  {
    if (radiusX <= 0) return Path<T>();
    if (radiusY <= 0) radiusY = radiusX;
    if (steps <= 2)
      steps = static_cast<int>(PI * sqrt((radiusX + radiusY) / 2));

    Scalar si = std::sin(2 * PI / steps);
    Scalar co = std::cos(2 * PI / steps);
    Scalar dx = co, dy = si;
    Path<T> result;
    result.reserve(steps);
    result.push_back(Point<T>(center.x + radiusX, static_cast<Scalar>(center.y)));
    for (int i = 1; i < steps; ++i)
    {
      result.push_back(Point<T>(center.x + radiusX * dx, center.y + radiusY * dy));
      Scalar x = dx * co - dy * si;
      dy = dy * co + dx * si;
      dx = x;
    }
    return result;
  }

  inline size_t GetNext(size_t current, size_t high,
    const std::vector<bool>& flags)
  {
    ++current;
    while (current <= high && flags[current]) ++current;
    if (current <= high) return current;
    current = 0;
    while (flags[current]) ++current;
    return current;
  }

  inline size_t GetPrior(size_t current, size_t high,
    const std::vector<bool>& flags)
  {
    if (current == 0) current = high;
    else --current;
    while (current > 0 && flags[current]) --current;
    if (!flags[current]) return current;
    current = high;
    while (flags[current]) --current;
    return current;
  }

  template <typename T>
  inline Path<T> SimplifyPath(const Path<T> &path,
    Scalar epsilon, bool isClosedPath = true)
  {
    const size_t len = path.size(), high = len -1;
    const Scalar epsSqr = Sqr(epsilon);
    if (len < 4) return Path<T>(path);

    std::vector<bool> flags(len);
    std::vector<Scalar> distSqr(len);
    size_t prior = high, curr = 0, start, next, prior2;
    if (isClosedPath)
    {
      distSqr[0] = PerpendicDistFromLineSqrd(path[0], path[high], path[1]);
      distSqr[high] = PerpendicDistFromLineSqrd(path[high], path[0], path[high - 1]);
    }
    else
    {
      distSqr[0] = MAX_SCALAR;
      distSqr[high] = MAX_SCALAR;
    }
    for (size_t i = 1; i < high; ++i)
      distSqr[i] = PerpendicDistFromLineSqrd(path[i], path[i - 1], path[i + 1]);

    for (;;)
    {
      if (distSqr[curr] > epsSqr)
      {
        start = curr;
        do
        {
          curr = GetNext(curr, high, flags);
        } while (curr != start && distSqr[curr] > epsSqr);
        if (curr == start) break;
      }

      prior = GetPrior(curr, high, flags);
      next = GetNext(curr, high, flags);
      if (next == prior) break;

      // flag for removal the smaller of adjacent 'distances'
      if (distSqr[next] < distSqr[curr])
      {
        prior2 = prior;
        prior = curr;
        curr = next;
        next = GetNext(next, high, flags);
      }
      else
        prior2 = GetPrior(prior, high, flags);

      flags[curr] = true;
      curr = next;
      next = GetNext(next, high, flags);

      if (isClosedPath || ((curr != high) && (curr != 0)))
        distSqr[curr] = PerpendicDistFromLineSqrd(path[curr], path[prior], path[next]);
      if (isClosedPath || ((prior != 0) && (prior != high)))
        distSqr[prior] = PerpendicDistFromLineSqrd(path[prior], path[prior2], path[curr]);
    }
    Path<T> result;
    result.reserve(len);
    for (typename Path<T>::size_type i = 0; i < len; ++i)
      if (!flags[i]) result.push_back(path[i]);
    return result;
  }

  template <typename T>
  inline Paths<T> SimplifyPaths(const Paths<T> &paths,
    Scalar epsilon, bool isClosedPath = true)
  {
    Paths<T> result;
    result.reserve(paths.size());
    for (const auto& path : paths)
      result.push_back(SimplifyPath(path, epsilon, isClosedPath));
    return result;
  }

  template <typename T>
  inline void RDP(const Path<T> path, std::size_t begin,
    std::size_t end, Scalar epsSqrd, std::vector<bool>& flags)
  {
    typename Path<T>::size_type idx = 0;
    Scalar max_d = 0;
    while (end > begin && path[begin] == path[end]) flags[end--] = false;
    for (typename Path<T>::size_type i = begin + 1; i < end; ++i)
    {
      // PerpendicDistFromLineSqrd - avoids expensive Sqrt()
      Scalar d = PerpendicDistFromLineSqrd(path[i], path[begin], path[end]);
      if (d <= max_d) continue;
      max_d = d;
      idx = i;
    }
    if (max_d <= epsSqrd) return;
    flags[idx] = true;
    if (idx > begin + 1) RDP(path, begin, idx, epsSqrd, flags);
    if (idx < end - 1) RDP(path, idx, end, epsSqrd, flags);
  }

  template <typename T>
  inline Path<T> RamerDouglasPeucker(const Path<T>& path, Scalar epsilon)
  {
    const typename Path<T>::size_type len = path.size();
    if (len < 5) return Path<T>(path);
    std::vector<bool> flags(len);
    flags[0] = true;
    flags[len - 1] = true;
    RDP(path, 0, len - 1, Sqr(epsilon), flags);
    Path<T> result;
    result.reserve(len);
    for (typename Path<T>::size_type i = 0; i < len; ++i)
      if (flags[i])
        result.push_back(path[i]);
    return result;
  }

  template <typename T>
  inline Paths<T> RamerDouglasPeucker(const Paths<T>& paths, Scalar epsilon)
  {
    Paths<T> result;
    result.reserve(paths.size());
    std::transform(paths.begin(), paths.end(), back_inserter(result),
      [epsilon](const auto& path)
      { return RamerDouglasPeucker<T>(path, epsilon); });
    return result;
  }

}  // end Clipper2Lib namespace

#endif  // CLIPPER_H
