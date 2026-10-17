using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Clipper2Lib.UnitTests
{

  [TestClass]
  public class TestOffsets
  {

    [TestMethod]
    public void TestOffsetEmpty()
    {
      Paths64 solution = new();

      ClipperOffset offset = new ClipperOffset();
      offset.Execute(10, solution);
    }

    private static void AssertBounds(Paths64 paths,
      long left, long top, long right, long bottom)
    {
      Rect64 bounds = Clipper.GetBounds(paths);
      Assert.AreEqual(left, bounds.left);
      Assert.AreEqual(top, bounds.top);
      Assert.AreEqual(right, bounds.right);
      Assert.AreEqual(bottom, bounds.bottom);
    }

    [TestMethod]
    public void TestOffsetMultipleRoundPolygons()
    {
      Paths64 subjects = new()
      {
        Clipper.MakePath(new long[] { 0, 0, 10, 0, 10, 10, 0, 10 }),
        Clipper.MakePath(new long[] { 30, 0, 40, 0, 40, 10, 30, 10 })
      };
      ClipperOffset offset = new();
      offset.AddPaths(subjects, JoinType.Round, EndType.Polygon);
      Paths64 solution = new();

      offset.Execute(2, solution);

      Assert.AreEqual(2, solution.Count);
      AssertBounds(solution, -2, -2, 42, 12);
      Assert.IsTrue(Math.Abs(Clipper.Area(solution)) > 200);
    }

    [TestMethod]
    public void TestOffsetOpenJoinedPath()
    {
      Paths64 subject = new()
      {
        Clipper.MakePath(new long[] { 0, 0, 20, 0, 20, 20, 0, 20 })
      };
      ClipperOffset offset = new();
      offset.AddPaths(subject, JoinType.Round, EndType.Joined);
      Paths64 solution = new();

      offset.Execute(2, solution);

      Assert.IsTrue(solution.Count > 0);
      AssertBounds(solution, -2, -2, 22, 22);
      Assert.IsTrue(Math.Abs(Clipper.Area(solution)) > 0);
    }

    [TestMethod]
    public void TestOffsetMultipleOpenPathsWithRoundCaps()
    {
      Paths64 subjects = new()
      {
        Clipper.MakePath(new long[] { 0, 0, 20, 0 }),
        Clipper.MakePath(new long[] { 0, 20, 20, 20 })
      };
      ClipperOffset offset = new();
      offset.AddPaths(subjects, JoinType.Round, EndType.Round);
      Paths64 solution = new();

      offset.Execute(2, solution);

      Assert.AreEqual(2, solution.Count);
      AssertBounds(solution, -2, -2, 22, 22);
      foreach (Path64 path in solution)
        Assert.IsTrue(path.Count > 4);
    }

    [TestMethod]
    public void TestOffsetSinglePointRoundAndSquare()
    {
      Paths64 subject = new()
      {
        new Path64 { new Point64(0, 0) }
      };
      Paths64 roundSolution = Clipper.InflatePaths(
        subject, 10, JoinType.Round, EndType.Round);
      Paths64 squareSolution = Clipper.InflatePaths(
        subject, 10, JoinType.Square, EndType.Square);

      Assert.AreEqual(1, roundSolution.Count);
      Assert.IsTrue(roundSolution[0].Count > 4);
      AssertBounds(roundSolution, -10, -10, 10, 10);

      Assert.AreEqual(1, squareSolution.Count);
      Assert.AreEqual(4, squareSolution[0].Count);
      AssertBounds(squareSolution, -10, -10, 10, 10);
      Assert.AreEqual(400, Math.Abs(Clipper.Area(squareSolution)));
    }
  }
}
