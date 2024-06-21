/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  1 November 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* Purpose   :  Minkowski Sum and Difference                                    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_MINKOWSKI_H
#define CLIPPER_MINKOWSKI_H

#include <cstdlib>
#include <vector>
#include <string>
#include "clipper2/clipper.core.h"

namespace Clipper2Lib
{

  namespace detail
  {
    inline PathsI Minkowski(const PathI& pattern, const PathI& path, bool isSum, bool isClosed)
    {
      size_t delta = isClosed ? 0 : 1;
      size_t patLen = pattern.size(), pathLen = path.size();
      if (patLen == 0 || pathLen == 0) return PathsI();
      PathsI tmp;
      tmp.reserve(pathLen);

      if (isSum)
      {
        for (const PointI& p : path)
        {
          PathI path2(pattern.size());
          std::transform(pattern.cbegin(), pattern.cend(),
            path2.begin(), [p](const PointI& pt2) {return p + pt2; });
          tmp.push_back(path2);
        }
      }
      else
      {
        for (const PointI& p : path)
        {
          PathI path2(pattern.size());
          std::transform(pattern.cbegin(), pattern.cend(),
            path2.begin(), [p](const PointI& pt2) {return p - pt2; });
          tmp.push_back(path2);
        }
      }

      PathsI result;
      result.reserve((pathLen - delta) * patLen);
      size_t g = isClosed ? pathLen - 1 : 0;
      for (size_t h = patLen - 1, i = delta; i < pathLen; ++i)
      {
        for (size_t j = 0; j < patLen; j++)
        {
          PathI quad;
          quad.reserve(4);
          {
            quad.push_back(tmp[g][h]);
            quad.push_back(tmp[i][h]);
            quad.push_back(tmp[i][j]);
            quad.push_back(tmp[g][j]);
          };
          if (!IsPositive(quad))
            std::reverse(quad.begin(), quad.end());
          result.push_back(quad);
          h = j;
        }
        g = i;
      }
      return result;
    }

    inline PathsI Union(const PathsI& subjects, FillRule fillrule)
    {
      PathsI result;
      ClipperI clipper;
      clipper.AddSubject(subjects);
      clipper.Execute(ClipType::Union, fillrule, result);
      return result;
    }

  } // namespace internal

  inline PathsI MinkowskiSum(const PathI& pattern, const PathI& path, bool isClosed)
  {
    return detail::Union(detail::Minkowski(pattern, path, true, isClosed), FillRule::NonZero);
  }

  inline PathsS MinkowskiSum(const PathS& pattern, const PathS& path, bool isClosed, int decimalPlaces = 2)
  {
    int error_code = 0;
    Scalar scale = (Scalar)pow(10, decimalPlaces);
    PathI pat64 = ScalePath<Integer, Scalar>(pattern, scale, error_code);
    PathI path64 = ScalePath<Integer, Scalar>(path, scale, error_code);
    PathsI tmp = detail::Union(detail::Minkowski(pat64, path64, true, isClosed), FillRule::NonZero);
    return ScalePaths<Scalar, Integer>(tmp, 1 / scale, error_code);
  }

  inline PathsI MinkowskiDiff(const PathI& pattern, const PathI& path, bool isClosed)
  {
    return detail::Union(detail::Minkowski(pattern, path, false, isClosed), FillRule::NonZero);
  }

  inline PathsS MinkowskiDiff(const PathS& pattern, const PathS& path, bool isClosed, int decimalPlaces = 2)
  {
    int error_code = 0;
    Scalar scale = (Scalar)pow(10, decimalPlaces);
    PathI pat64 = ScalePath<Integer, Scalar>(pattern, scale, error_code);
    PathI path64 = ScalePath<Integer, Scalar>(path, scale, error_code);
    PathsI tmp = detail::Union(detail::Minkowski(pat64, path64, false, isClosed), FillRule::NonZero);
    return ScalePaths<Scalar, Integer>(tmp, 1 / scale, error_code);
  }

} // Clipper2Lib namespace

#endif  // CLIPPER_MINKOWSKI_H
