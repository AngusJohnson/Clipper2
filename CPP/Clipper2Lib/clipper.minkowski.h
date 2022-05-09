/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  28 April 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Minkowski Sum and Difference                                    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_MINKOWSKI_H
#define CLIPPER_MINKOWSKI_H

#include <cstdlib>
#include <vector>
#include <string>

#include "clipper.core.h"
#include "clipper.engine.h"
#include "clipper.offset.h"

namespace Clipper2Lib 
{

  namespace detail
  {
    static Paths64 Minkowski(const Path64& pattern, const Path64& path, bool isSum, bool isClosed)
    {
      using PointType = Path64::value_type;
      size_t delta = isClosed ? 0 : 1;
      size_t patLen = pattern.size(), pathLen = path.size();
      if (patLen == 0 || pathLen == 0) return Paths64();
      Paths64 tmp;
      tmp.reserve(pathLen);

      for (Point64 pt : path)
      {
        Path64 path2;
        path2.reserve(pattern.size());
        if (isSum)
          for (const Point64& pt2 : pattern) 
              path2.emplace_back(point_mutable_traits<PointType>::construct(
                  point_traits<PointType>::get(pt, 0) + point_traits<PointType>::get(pt2, 0), 
                  point_traits<PointType>::get(pt, 1) + point_traits<PointType>::get(pt2, 0)));
        else
          for (const Point64& pt2 : pattern)
            path2.emplace_back(point_mutable_traits<PointType>::construct(
                point_traits<PointType>::get(pt, 0) - point_traits<PointType>::get(pt2, 0),
                point_traits<PointType>::get(pt, 1) - point_traits<PointType>::get(pt2, 0)));
        tmp.push_back(path2);
      }

      Paths64 result;
      result.reserve((pathLen - delta) * patLen);
      size_t g = isClosed ? pathLen - 1 : 0;
      for (size_t h = patLen - 1, i = delta; i < pathLen; ++i)
      {
        for (size_t j = 0; j < patLen; j++)
        {
          Path64 quad;
          quad.reserve(4);
          {
            quad.push_back(tmp[g][h]);
            quad.push_back(tmp[i][h]);
            quad.push_back(tmp[i][j]);
            quad.push_back(tmp[g][j]);
          };
          if (!IsClockwise(quad))
            std::reverse(quad.begin(), quad.end());
          result.push_back(quad);
          h = j;
        }
        g = i;
      }
      return result;
    }

    inline Paths64 Union(const Paths64& subjects, FillRule fillrule)
    {
      Paths64 result;
      Clipper64 clipper;
      clipper.AddSubject(subjects);
      clipper.Execute(ClipType::Union, fillrule, result);
      return result;
    }

  } //namespace internal

  static Paths64 MinkowskiSum(const Path64& pattern, const Path64& path, bool isClosed)
  {
    return detail::Union(detail::Minkowski(pattern, path, true, isClosed), FillRule::NonZero);
  }

  static PathsD MinkowskiSum(const PathD& pattern, const PathD& path, bool isClosed, int decimalPlaces = 2)
  {
    double scale = pow(10, decimalPlaces);
    Path64 pat64 = PathDToPath64(pattern, scale), path64 = PathDToPath64(path, scale);
    Paths64 tmp = detail::Union(detail::Minkowski(pat64, path64, true, isClosed), FillRule::NonZero);
    return Paths64ToPathsD(tmp, 1 / scale);
  }

  static Paths64 MinkowskiDiff(const Path64& pattern, const Path64& path, bool isClosed)
  {
    return detail::Union(detail::Minkowski(pattern, path, false, isClosed), FillRule::NonZero);
  }

  static PathsD MinkowskiDiff(const PathD& pattern, const PathD& path, bool isClosed, int decimalPlaces = 2)
  {
    double scale = pow(10, decimalPlaces);
    Path64 pat64 = PathDToPath64(pattern, scale), path64 = PathDToPath64(path, scale);
    Paths64 tmp = detail::Union(detail::Minkowski(pat64, path64, false, isClosed), FillRule::NonZero);
    return Paths64ToPathsD(tmp, 1 / scale);
  }

} //Clipper2Lib namespace

#endif  // CLIPPER_MINKOWSKI_H
