using Microsoft.VisualStudio.TestTools.UnitTesting;
using Clipper2Lib;

namespace Tests1.Tests
{

  [TestClass]
  public class TestPolytree
  {
    private static void PolyPathContainsPoint(PolyPath64 pp, Point64 pt, ref int counter)
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

    private static bool PolytreeContainsPoint(PolyTree64 pp, Point64 pt)
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

  private static bool PolyPathFullyContainsChildren(PolyPath64 pp)
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

    private static bool CheckPolytreeFullyContainsChildren(PolyTree64 polytree)
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

  [TestMethod]
    public void TestPolytree3()
    {
      Paths64 subject = new()
      {
        Clipper.MakePath(new int[] {1588700, -8717600,
        1616200, -8474800, 1588700, -8474800 }),
        Clipper.MakePath(new int[] { 13583800,-15601600,
        13582800,-15508500, 13555300,-15508500, 13555500,-15182200,
        13010900,-15185400 }),
        Clipper.MakePath(new int[] { 956700, -3092300, 1152600,
        3147400, 25600, 3151700 }),
        Clipper.MakePath(new int[] {
        22575900,-16604000, 31286800,-12171900,
        31110200,4882800, 30996200,4826300, 30414400,5447400, 30260000,5391500,
        29662200,5805400, 28844500,5337900, 28435000,5789300, 27721400,5026400,
        22876300,5034300, 21977700,4414900, 21148000,4654700, 20917600,4653400,
        19334300,12411000, -2591700,12177200, 53200,3151100, -2564300,12149800,
        7819400,4692400, 10116000,5228600, 6975500,3120100, 7379700,3124700,
        11037900,596200, 12257000,2587800, 12257000,596200, 15227300,2352700,
        18444400,1112100, 19961100,5549400, 20173200,5078600, 20330000,5079300,
        20970200,4544300, 20989600,4563700, 19465500,1112100, 21611600,4182100,
        22925100,1112200, 22952700,1637200, 23059000,1112200, 24908100,4181200,
        27070100,3800600, 27238000,3800700, 28582200,520300, 29367800,1050100,
        29291400,179400, 29133700,360700, 29056700,312600, 29121900,332500,
        29269900,162300, 28941400,213100, 27491300,-3041500, 27588700,-2997800,
        22104900,-16142800, 13010900,-15603000, 13555500,-15182200,
        13555300,-15508500, 13582800,-15508500, 13583100,-15154700,
        1588700,-8822800, 1588700,-8379900, 1588700,-8474800, 1616200,-8474800,
        1003900,-630100, 1253300,-12284500, 12983400,-16239900}),
        Clipper.MakePath(new int[] { 198200, 12149800, 1010600, 12149800, 1011500, 11859600 }),
        Clipper.MakePath(new int[] { 21996700, -7432000, 22096700, -7432000, 22096700, -7332000 })
      };
      PolyTree64 solutionTree = new();

      Clipper64 clipper = new();
      clipper.AddSubject(subject);
      clipper.Execute(ClipType.Union, FillRule.NonZero, solutionTree);

      Assert.IsTrue(solutionTree.Count == 1 && solutionTree[0].Count == 2
        && solutionTree[0][1].Count == 1, "Incorrect PolyTree nesting.");


    } // end TESTMETHOD TestPolytree3

  } // end TestClass

}
