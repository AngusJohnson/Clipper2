/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  10 October 2024                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  Minkowski Sum and Difference                                    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

namespace Clipper2Lib;

public static class Minkowski
{
  private static Paths64 MinkowskiInternal(Path64 pattern, Path64 path, bool isSum, bool isClosed)
  {
    var delta = isClosed ? 0 : 1;
    int patLen = pattern.Count, pathLen = path.Count;
    var tmp = new Paths64(pathLen);

    foreach (var pathPt in path)
    {
      var path2 = new Path64(patLen);
      if (isSum)
      {
        foreach (var basePt in pattern)
          path2.Add(pathPt + basePt);
      }
      else
      {
        foreach (var basePt in pattern)
          path2.Add(pathPt - basePt);
      }
      tmp.Add(path2);
    }

    var result = new Paths64((pathLen - delta) * patLen);
    var g = isClosed ? pathLen - 1 : 0;

    var h = patLen - 1;
    for (var i = delta; i < pathLen; i++)
    {
      for (var j = 0; j < patLen; j++)
      {
        var quad = new Path64(4)
        {
          tmp[g][h], tmp[i][h], tmp[i][j], tmp[g][j]
        };
        if (!Clipper.IsPositive(quad))
          result.Add(Clipper.ReversePath(quad));
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
    return Clipper.Union(MinkowskiInternal(pattern, path, true, isClosed), FillRule.NonZero);
  }

  public static PathsD Sum(PathD pattern, PathD path, bool isClosed, int decimalPlaces = 2)
  {
    var scale = Math.Pow(10, decimalPlaces);
    var tmp = Clipper.Union(MinkowskiInternal(Clipper.ScalePath64(pattern, scale),
      Clipper.ScalePath64(path, scale), true, isClosed), FillRule.NonZero);
    return Clipper.ScalePathsD(tmp, 1 / scale);
  }

  public static Paths64 Diff(Path64 pattern, Path64 path, bool isClosed)
  {
    return Clipper.Union(MinkowskiInternal(pattern, path, false, isClosed), FillRule.NonZero);
  }

  public static PathsD Diff(PathD pattern, PathD path, bool isClosed, int decimalPlaces = 2)
  {
    var scale = Math.Pow(10, decimalPlaces);
    var tmp = Clipper.Union(MinkowskiInternal(Clipper.ScalePath64(pattern, scale),
      Clipper.ScalePath64(path, scale), false, isClosed), FillRule.NonZero);
    return Clipper.ScalePathsD(tmp, 1 / scale);
  }
}
