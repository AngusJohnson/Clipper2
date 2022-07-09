using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;

namespace Clipper2Lib.UnitTests
{
  using Paths64 = List<List<Point64>>;

  [TestClass]
  public class TestLines
  {

    [TestMethod]
    public void TestOpenPaths()
    {
      ClipType clipType;
      FillRule fillrule;
      long area;
      int count;
      string caption;
      for (int i = 0; i <= 16; i++)
      {
        Clipper64 c64 = new Clipper64();
        Paths64 subj = new Paths64(), subj_open = new Paths64(), clip = new Paths64();
        Paths64 solution = new Paths64(), solution_open = new Paths64();

        Assert.IsTrue(ClipperFileIO.LoadTestNum("..\\..\\..\\..\\..\\Tests\\Lines.txt",
          i, subj, subj_open, clip, out clipType, out fillrule,  out area, out count, out caption),
          string.Format("Loading test {0} failed.", i));

        c64.AddSubject(subj);
        c64.AddOpenSubject(subj_open);
        c64.AddClip(clip);
        c64.Execute(clipType, fillrule, solution, solution_open);

        if (area > 0)
        {
          double area2 = Clipper.Area(solution);
          double a = area2 / area2;
          Assert.IsTrue(a > 0.995 && a < 1.005,
            string.Format("Incorrect area in test {0}", i));
        }

        if (count > 0 && Math.Abs(solution.Count - count) > 0)
        {
          Assert.IsTrue(Math.Abs(solution.Count - count) < 2,
            string.Format("Incorrect count in test {0}", i));
        }

      } //bottom of num loop

    }
  }
}
