/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (release candidate 1) - also known as Clipper2             *
* Date      :  11 April 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Minkowski Addition and Difference                               *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Clipper2Lib
{
  using Path64 = List<Point64>;
  using PathD = List<PointD>;
  using Paths64 = List<List<Point64>>;
  using PathsD = List<List<PointD>>;
  public class Minkowski
  {
    private static Paths64 MinkowskiInternal(Path64 pattern, Path64 path, bool isSum, bool isClosed)
    {
      int delta = isClosed ? 0 : -1;
      int patLen = pattern.Count, pathLen = path.Count;
      Paths64 tmp = new Paths64(pathLen);
      foreach (Point64 pathPt in path)
      {
        Path64 path2 = new Path64(patLen);
        foreach (Point64 basePt in pattern)
        {
          if (isSum)
            path2.Add(pathPt + basePt);
          else
            path2.Add(pathPt - basePt);
        }
        tmp.Add(path2);
      }
      Paths64 result = new Paths64((pathLen + delta) * patLen);
      int g = isClosed ? pathLen - 1 : 0;

      int h = patLen - 1;
      for (int i = -delta; i < pathLen; i++)
      {
        int k = (i + delta) * patLen;
        for (int j = 0; j < patLen; j++) 
        {
          Path64 quad = new Path64(4);
          quad.Add(tmp[g][h]);
          quad.Add(tmp[i][h]);
          quad.Add(tmp[i][j]);
          quad.Add(tmp[g][j]);
          if (!ClipperFunc.IsClockwise(quad))
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
      return ClipperFunc.Union(MinkowskiInternal(pattern, path, true, isClosed), null, FillRule.NonZero);
    }

    public static PathsD Sum(PathD pattern, PathD path, bool isClosed, int decimalPlaces = 2)
    {
      double scale = Math.Pow(10, decimalPlaces);
      Paths64 tmp = ClipperFunc.Union(MinkowskiInternal(ClipperFunc.Path64(pattern, scale),
        ClipperFunc.Path64(path, scale), true, isClosed), null, FillRule.NonZero);
      return ClipperFunc.PathsD(tmp, 1 / scale);
    }

    public static Paths64 Diff(Path64 pattern, Path64 path, bool isClosed)
    {
      return ClipperFunc.Union(MinkowskiInternal(pattern, path, false, isClosed), null, FillRule.NonZero);
    }

    public static PathsD Diff(PathD pattern, PathD path, bool isClosed, int decimalPlaces = 2)
    {
      double scale = Math.Pow(10, decimalPlaces);
      Paths64 tmp = ClipperFunc.Union(MinkowskiInternal(ClipperFunc.Path64(pattern, scale),
        ClipperFunc.Path64(path, scale), false, isClosed), null, FillRule.NonZero);
      return ClipperFunc.PathsD(tmp, 1 / scale);
    }

  }

} //namespace