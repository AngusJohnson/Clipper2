/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - also known as Clipper2                            *
* Date      :  9 June 2022                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Minkowski Sum and Difference                                    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#nullable enable
using System;
using System.Collections.Generic;

namespace Clipper2Lib
{
  using Path64 = List<Point64>;
  using Paths64 = List<List<Point64>>;
  using PathD = List<PointD>;
  using PathsD = List<List<PointD>>;
  public class Minkowski
  {
    private static Paths64 MinkowskiInternal(Path64 pattern, Path64 path, bool isSum, bool isClosed)
    {
      int delta = isClosed ? 0 : 1;
      int patLen = pattern.Count, pathLen = path.Count;
      Paths64 tmp = new Paths64(pathLen);

      foreach (Point64 pathPt in path)
      {
        Path64 path2 = new Path64(patLen);
        if (isSum)
        {
          foreach (Point64 basePt in pattern)
            path2.Add(pathPt + basePt);
        }
        else
        {
          foreach (Point64 basePt in pattern)
            path2.Add(pathPt - basePt);
        }
        tmp.Add(path2);
      }

      Paths64 result = new Paths64((pathLen - delta) * patLen);
      int g = isClosed ? pathLen - 1 : 0;

      int h = patLen - 1;
      for (int i = delta; i < pathLen; i++)
      {
        for (int j = 0; j < patLen; j++)
        {
          Path64 quad = new Path64(4)
          {
            tmp[g][h], tmp[i][h], tmp[i][j], tmp[g][j]
          };
          if (!ClipperFunc.IsPositive(quad))
            result.Add(ClipperFunc.ReversePath(quad));
          else
            result.Add(quad);
          h = j;
        }
        g = i;
      }
      return result;
    }

    public static Paths64 Sum(Path64 pattern, Path64 path, bool isClosed)
    {
      return ClipperFunc.Union(MinkowskiInternal(pattern, path, true, isClosed), FillRule.NonZero);
    }

    public static PathsD Sum(PathD pattern, PathD path, bool isClosed, int decimalPlaces = 2)
    {
      double scale = Math.Pow(10, decimalPlaces);
      Paths64 tmp = ClipperFunc.Union(MinkowskiInternal(ClipperFunc.ScalePath64(pattern, scale),
        ClipperFunc.ScalePath64(path, scale), true, isClosed), FillRule.NonZero);
      return ClipperFunc.ScalePathsD(tmp, 1 / scale);
    }

    public static Paths64 Diff(Path64 pattern, Path64 path, bool isClosed)
    {
      return ClipperFunc.Union(MinkowskiInternal(pattern, path, false, isClosed), FillRule.NonZero);
    }

    public static PathsD Diff(PathD pattern, PathD path, bool isClosed, int decimalPlaces = 2)
    {
      double scale = Math.Pow(10, decimalPlaces);
      Paths64 tmp = ClipperFunc.Union(MinkowskiInternal(ClipperFunc.ScalePath64(pattern, scale),
        ClipperFunc.ScalePath64(path, scale), false, isClosed), FillRule.NonZero);
      return ClipperFunc.ScalePathsD(tmp, 1 / scale);
    }

  }

} //namespace