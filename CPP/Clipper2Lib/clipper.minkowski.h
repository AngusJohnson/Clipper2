/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  27 April 2022                                                   *
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

#include "clipper.h"
#include "clipper.core.h"

namespace Clipper2Lib 
{

  static Paths64 MinkowskiInternal(const Path64& pattern, const Path64& path, bool isSum, bool isClosed)
    {
      size_t delta = isClosed ? 0 : 1;
      size_t patLen = pattern.size(), pathLen = path.size();
      if (patLen == 0 || pathLen == 0) return Paths64();
      Paths64 tmp;
      tmp.reserve(pathLen);


      for(Path64::const_iterator path_citer = path.cbegin();
        path_citer != path.cend(); ++path_citer)
      {
        Path64 path2;
        path2.reserve(pattern.size());
        for (Path64::const_iterator pat_citer = pattern.cbegin();
          pat_citer != pattern.cend(); ++pat_citer)
        {
          if (isSum)
            path2.push_back(*path_citer + *pat_citer);
          else
            path2.push_back(*path_citer - *pat_citer);
        }
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

    static Paths64 Sum(const Path64& pattern, const Path64& path, bool isClosed)
    {
      return Union(MinkowskiInternal(pattern, path, true, isClosed), FillRule::NonZero);
    }

    static PathsD Sum(const PathD& pattern, const PathD& path, bool isClosed, int decimalPlaces = 2)
    {
      double scale = pow(10, decimalPlaces);
      Path64 pat64 = PathDToPath64(pattern, scale), path64 = PathDToPath64(path, scale);
      Paths64 tmp = Union(MinkowskiInternal(pat64, path64, true, isClosed), FillRule::NonZero);
      return Paths64ToPathsD(tmp, 1 / scale);
    }

    static Paths64 Diff(const Path64& pattern, const Path64& path, bool isClosed)
    {
      return Union(MinkowskiInternal(pattern, path, false, isClosed), FillRule::NonZero);
    }

    static PathsD Diff(const PathD& pattern, const PathD& path, bool isClosed, int decimalPlaces = 2)
    {
      double scale = pow(10, decimalPlaces);
      Path64 pat64 = PathDToPath64(pattern, scale), path64 = PathDToPath64(path, scale);
      Paths64 tmp = Union(MinkowskiInternal(pat64, path64, false, isClosed), FillRule::NonZero);
      return Paths64ToPathsD(tmp, 1 / scale);
    }

}  //namespace

#endif  // CLIPPER_MINKOWSKI_H
