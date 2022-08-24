using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;

namespace Clipper2Lib.UnitTests
{
  using Paths64 = List<List<Point64>>;

  [TestClass]
  public class TestPolygons
  {

    [TestMethod]
    public void TestClosedPaths()
    {
      int i = 0;
      while (true)
      {
        i++;
        Clipper64 c64 = new();
        Paths64 subj = new(), subj_open = new (), clip = new();
        Paths64 solution = new(), solution_open = new();

        if (!ClipperFileIO.LoadTestNum("..\\..\\..\\..\\..\\..\\Tests\\Polygons.txt",
          i, subj, subj_open, clip, out ClipType clipType, out FillRule fillrule, 
          out long area, out int count, out _))
        {          
          Assert.IsTrue(i > 180, string.Format("Loading test polygon {0} failed.", i));
          break;
        }

        c64.AddSubject(subj);
        c64.AddOpenSubject(subj_open);
        c64.AddClip(clip);
        c64.Execute(clipType, fillrule, solution, solution_open);

        if (area > 0)
        {
          double area2 = Clipper.Area(solution);
          double a = area / area2;
          Assert.IsTrue(Math.Abs(area - area2) < 2 || (a > 0.995 && a < 1.005),
            string.Format("Incorrect area in test {0}", i));
        }

        if (count > 0 && Math.Abs(solution.Count - count) > 2 && 
            (double)Math.Abs(solution.Count - count)/count > 0.02)
        {
          Assert.IsTrue(Math.Abs(solution.Count - count) < 4,
            string.Format("Incorrect count in test {0}", i));
        }

      } //bottom of num loop

    }
  }
}
