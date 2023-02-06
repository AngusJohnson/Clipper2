using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Clipper2Lib.UnitTests
{

  [TestClass]
  public class TestPolygons
  {

    private static bool IsInList(int num, int[] list)
    {
      foreach (int i in list) if (i == num) return true;
      return false;
    }

    [TestMethod]
    public void TestClosedPaths()
    {
      int testNum = 0;
      while (true)
      {
        testNum++;
        Clipper64 c64 = new();
        Paths64 subj = new(), subj_open = new (), clip = new();
        Paths64 solution = new(), solution_open = new();

        if (!ClipperFileIO.LoadTestNum("..\\..\\..\\..\\..\\..\\Tests\\Polygons.txt",
          testNum, subj, subj_open, clip, out ClipType clipType, out FillRule fillrule, 
          out long storedArea, out int storedCount, out _))
        {          
          Assert.IsTrue(testNum > 180, string.Format("Loading test polygon {0} failed.", testNum));
          break;
        }

        c64.AddSubject(subj);
        c64.AddOpenSubject(subj_open);
        c64.AddClip(clip);
        c64.Execute(clipType, fillrule, solution, solution_open);
        int measuredCount = solution.Count;
        long measuredArea = (long)Clipper.Area(solution);
        int countDiff = storedCount > 0 ? Math.Abs(storedCount - measuredCount) : 0;
        long areaDiff = storedArea > 0 ? Math.Abs(storedArea - measuredArea) : 0;
        double areaDiffRatio = storedArea <= 0 ? 0 : (double) areaDiff / storedArea;

        // check polygon counts
        if (storedCount > 0)
        {
          if (IsInList(testNum, new int[] { 140, 150, 165, 166, 172, 173, 176, 177, 179 }))
          {
            Assert.IsTrue(countDiff <= 9);
          }
          else if (testNum >= 120)
          {
            Assert.IsTrue(countDiff <= 6);
          }
          else if (IsInList(testNum, new int[] { 27, 121, 126 }))
            Assert.IsTrue(countDiff <= 2);
          else if (IsInList(testNum, new int[] { 23, 37, 43, 45, 87, 102, 111, 118, 119 }))
            Assert.IsTrue(countDiff <= 1);
          else
            Assert.IsTrue(countDiff == 0);
        }

        // check polygon areas
        if (storedArea > 0)
        {
          if (IsInList(testNum, new int[] { 19, 22, 23, 24 }))
            Assert.IsTrue(areaDiffRatio <= 0.5);
          else if (testNum == 193)
            Assert.IsTrue(areaDiffRatio <= 0.25);
          else if (testNum == 63)
            Assert.IsTrue(areaDiffRatio <= 0.1);
          else if (testNum == 16)
            Assert.IsTrue(areaDiffRatio <= 0.075);
          else if (IsInList(testNum, new int[] { 15, 26 }))
            Assert.IsTrue(areaDiffRatio <= 0.05);
          else if (IsInList(testNum, new int[] { 52, 53, 54, 59, 60, 64, 117, 118, 119, 184 }))
            Assert.IsTrue(areaDiffRatio <= 0.02);
          else
            Assert.IsTrue(areaDiffRatio <= 0.01);
        }

      } //bottom of num loop

    }
  }
}
