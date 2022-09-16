using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Clipper2Lib.UnitTests
{

  [TestClass]
  public class TestPolytree
  {
    private void PolyPathContainsPoint(PolyPath64 pp, Point64 pt, ref int counter)
    {
      if (Clipper.PointInPolygon(pt, pp.Polygon!) != PointInPolygonResult.IsOutside)
      {
        if (pp.IsHole) --counter; else ++counter;
      }
      for (int i = 0; i < pp.Count; i++)
      {
        PolyPath64 child = (PolyPath64) pp[i];
        PolyPathContainsPoint(child, pt, ref counter);
      } 
    }

    private bool PolytreeContainsPoint(PolyTree64 pp, Point64 pt)
    {
      int counter = 0;
      for (int i = 0; i < pp.Count; i++)
      {
        PolyPath64 child = (PolyPath64) pp[i];
        PolyPathContainsPoint(child, pt, ref counter);
      }
      Assert.IsTrue(counter >= 0, "Polytree has too many holes");
      return counter != 0;
    }

  private bool PolyPathFullyContainsChildren(PolyPath64 pp)
    {
      foreach (PolyPath64 child in pp.Cast<PolyPath64>())
      {
        foreach (Point64 pt in child.Polygon!)
          if (Clipper.PointInPolygon(pt, pp.Polygon!) == PointInPolygonResult.IsOutside)
            return false;
        if (child.Count > 0 && !PolyPathFullyContainsChildren(child))
          return false;
      }
      return true;
    }

    private bool CheckPolytreeFullyContainsChildren(PolyTree64 polytree)
    {
      for (int i = 0; i < polytree.Count; i++)
      {
        PolyPath64 child = (PolyPath64) polytree[i];
        if (child.Count > 0 && !PolyPathFullyContainsChildren(child))
          return false;
      }    
      return true;
    }

  [TestMethod]
    public void TestPolytree2()
    {
      Paths64 subject = new(), subjectOpen = new(), clip = new();

      Assert.IsTrue(ClipperFileIO.LoadTestNum("..\\..\\..\\..\\..\\..\\Tests\\PolytreeHoleOwner2.txt",
        1, subject, subjectOpen, clip, out ClipType cliptype, out FillRule fillrule, 
        out _, out _, out _),
          "Unable to read PolytreeHoleOwner2.txt");

      PolyTree64 solutionTree = new();
      Paths64 solution_open = new();
      Clipper64 clipper = new();

      Path64 pointsOfInterestOutside = new()
      {
        new Point64(21887, 10420),
        new Point64(21726, 10825),
        new Point64(21662, 10845),
        new Point64(21617, 10890)
      };

      foreach (Point64 pt in pointsOfInterestOutside)
      {
        foreach (Path64 path in subject)
        {
          Assert.IsTrue(Clipper.PointInPolygon(pt, path) == PointInPolygonResult.IsOutside, 
            "outside point of interest found inside subject");
        }
      }

      Path64 pointsOfInterestInside = new()
      {
        new Point64(21887, 10430),
        new Point64(21843, 10520),
        new Point64(21810, 10686),
        new Point64(21900, 10461)
      };

      foreach (Point64 pt in pointsOfInterestInside)
      {
        int poi_inside_counter = 0;
        foreach (Path64 path in subject)
        {
          if (Clipper.PointInPolygon(pt, path) == PointInPolygonResult.IsInside)
            poi_inside_counter++;
        }
        Assert.IsTrue(poi_inside_counter == 1,
          string.Format("poi_inside_counter - expected 1 but got {0}", poi_inside_counter));
      }

      clipper.AddSubject(subject);
      clipper.AddOpenSubject(subjectOpen);
      clipper.AddClip(clip);
      clipper.Execute(cliptype, fillrule, solutionTree, solution_open);

      Paths64 solutionPaths = Clipper.PolyTreeToPaths64(solutionTree);
      double a1 = Clipper.Area(solutionPaths), a2 = solutionTree.Area();

      Assert.IsTrue(a1 > 330000, 
        string.Format("solution has wrong area - value expected: 331,052; value returned; {0} ", a1));

      Assert.IsTrue(Math.Abs(a1 - a2) < 0.0001,
        string.Format("solution tree has wrong area - value expected: {0}; value returned; {1} ", a1, a2));

      Assert.IsTrue(CheckPolytreeFullyContainsChildren(solutionTree),
        "The polytree doesn't properly contain its children");

      foreach (Point64 pt in pointsOfInterestOutside)
        Assert.IsFalse(PolytreeContainsPoint(solutionTree, pt),
          "The polytree indicates it contains a point that it should not contain");

      foreach (Point64 pt in pointsOfInterestInside)
        Assert.IsTrue(PolytreeContainsPoint(solutionTree, pt),
          "The polytree indicates it does not contain a point that it should contain");

    }
  }
}
