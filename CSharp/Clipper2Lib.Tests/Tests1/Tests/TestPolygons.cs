using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Clipper2Lib.UnitTests
{

  [TestClass]
  public class TestPolygons
  {

    private bool IsInList(int num, int[] list)
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
        int measuredCount = solution.Count();
        long measuredArea = (long)Clipper.Area(solution);
        int countDiff = storedCount > 0 ? Math.Abs(storedCount - measuredCount) : 0;
        long areaDiff = storedArea > 0 ? Math.Abs(storedArea - measuredArea) : 0;

        if (testNum == 23)
        {
          Assert.IsTrue(countDiff <= 4);
        }
        else if (testNum == 27)
        {
          Assert.IsTrue(countDiff <= 2);
        }
        else if (IsInList(testNum,
          new int[] {18, 32, 42, 43, 45, 87, 102, 103, 111, 118, 183 }))
        {
          Assert.IsTrue(countDiff <= 1);
        }
        else if (testNum >= 120)
        {
          if (storedCount > 0)
            Assert.IsTrue(countDiff / storedCount <= 0.02);
        }
        else if (storedCount > 0)
          Assert.IsTrue(countDiff == 0);

        if (IsInList(testNum, new int[] { 22,23,24 }))
        {
          Assert.IsTrue(areaDiff <= 8);
        }
        else if (storedArea > 0 && areaDiff > 100)
        {
          Assert.IsTrue(areaDiff / storedArea <= 0.02);
        }
      } //bottom of num loop

    }
  }
}
