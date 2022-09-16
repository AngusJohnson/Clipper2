using Microsoft.VisualStudio.TestTools.UnitTesting;

// USINGZ compiler directive should have been set in project properties

namespace Clipper2Lib.UnitTests
{

  [TestClass]
  public class TestingZ_1
  {

    public class ClipperTest
    {
      public void MyCallback64(Point64 bot1, Point64 top1,
          Point64 bot2, Point64 top2, ref Point64 intersectPt)
      {
        intersectPt.Z = 1;
      }

      public void MyCallbackD(PointD bot1, PointD top1,
          PointD bot2, PointD top2, ref PointD intersectPt)
      {
        intersectPt.z = 1;
      }
    }

    [TestMethod]
    public void TestSubjUnion64()
    {
      Paths64 solution = new Paths64();
      Paths64 subject = new Paths64();
      subject.Add(Clipper.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));

      Clipper64 c64 = new Clipper64();
      ClipperTest ct = new ClipperTest();

      c64.ZCallback = ct.MyCallback64;
      c64.AddSubject(subject);
      c64.Execute(ClipType.Union, FillRule.NonZero, solution);

      //check that the Z for every second vertex == 1
      Assert.IsTrue(solution.Count == 1 && solution[0].Count == 10);
      for (int i = 0; i < solution[0].Count; i++)
      {
        if ((i & 1) == 1)
          Assert.IsTrue(solution[0][i].Z == 0);
        else
          Assert.IsTrue(solution[0][i].Z == 1);
      }
    }

    [TestMethod]
    public void TestSubjClipUnion64()
    {
      Paths64 solution = new Paths64();
      Paths64 subject = new Paths64();
      Paths64 clip = new Paths64();
      // union two triangles
      subject.Add(Clipper.MakePath(new int[] { 10, 30, 80, 30, 45, 90 }));
      clip.Add(Clipper.MakePath(new int[] { 10, 70, 80, 70, 45, 10 }));

      Clipper64 c64 = new Clipper64();
      ClipperTest ct = new ClipperTest();

      c64.ZCallback = ct.MyCallback64;
      c64.AddSubject(subject);
      c64.AddClip(clip);
      c64.Execute(ClipType.Union, FillRule.NonZero, solution);

      //check that the Z for every second vertex == 1
      Assert.IsTrue(solution.Count == 1 && solution[0].Count == 12);
      for (int i = 0; i < solution[0].Count; i++)
      {
        if ((i & 1) == 1)
          Assert.IsTrue(solution[0][i].Z == 0);
        else
          Assert.IsTrue(solution[0][i].Z == 1);
      }
    }

    [TestMethod]
    public void TestSubjUnionD()
    {
      PathsD solution = new PathsD();
      PathsD subject = new PathsD();
      subject.Add(Clipper.MakePath(new double[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));

      ClipperD cD = new ClipperD();
      ClipperTest ct = new ClipperTest();

      cD.ZCallback = ct.MyCallbackD;
      cD.AddSubject(subject);
      cD.Execute(ClipType.Union, FillRule.NonZero, solution);

      //check that the Z for every second vertex == 1
      Assert.IsTrue(solution.Count == 1 && solution[0].Count == 10);
      for (int i = 0; i < solution[0].Count; i++)
      {
        if ((i & 1) == 1)
          Assert.IsTrue(solution[0][i].z == 0);
        else
          Assert.IsTrue(solution[0][i].z == 1);
      }
    }

    [TestMethod]
    public void TestSubjClipUnionD()
    {
      PathsD solution = new PathsD();
      PathsD subject = new PathsD();
      PathsD clip = new PathsD();
      // union two triangles
      subject.Add(Clipper.MakePath(new double[] { 10, 30, 80, 30, 45, 90 }));
      clip.Add(Clipper.MakePath(new double[] { 10, 70, 80, 70, 45, 10 }));

      ClipperD cD = new ClipperD();
      ClipperTest ct = new ClipperTest();

      cD.ZCallback = ct.MyCallbackD;
      cD.AddSubject(subject);
      cD.AddClip(clip);
      cD.Execute(ClipType.Union, FillRule.NonZero, solution);

      //check that the Z for every second vertex == 1
      Assert.IsTrue(solution.Count == 1 && solution[0].Count == 12);
      for (int i = 0; i < solution[0].Count; i++)
      {
        if ((i & 1) == 1)
          Assert.IsTrue(solution[0][i].z == 0);
        else
          Assert.IsTrue(solution[0][i].z == 1);
      }
    }

    [TestMethod]
    public void TestMysteryD()
    {
      ClipperD c = new ClipperD(5);
      var bitePoly = new PathD()
      {
        new PointD(5, 5, 5), new PointD(10, 5, 5), new PointD(10, 10, 5), new PointD(5, 10, 5)
      };

      var surfacePoly = new PathD()
      {
        new PointD(0, 0, 5), new PointD(15, 0, 5), new PointD(15, 15, 5), new PointD(0, 15, 5)
      };

      c.AddSubject(surfacePoly);
      c.AddClip(bitePoly);
      PathsD solution = new PathsD();

      c.Execute(ClipType.Difference, FillRule.EvenOdd, solution);
      Console.WriteLine(Clipper.PathsDToString(solution));
    }

  }
}
