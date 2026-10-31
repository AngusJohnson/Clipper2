using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;

#if USINGZ
using Clipper2ZLib;
namespace Clipper2ZLib.UnitTests
#else
using Clipper2Lib;
namespace Clipper2Lib.UnitTests
#endif
{
  [TestClass]
  public class TestStripeIndex
  {
    private static Path64 MakeSquare(long size)
    {
      return new Path64
      {
        new Point64(0, 0), new Point64(size, 0),
        new Point64(size, size), new Point64(0, size)
      };
    }

    // A many-edged ring so the index spans several stripes (sqrt(edges) > 1).
    private static Path64 MakeCircle(long r, int n)
    {
      Path64 p = new Path64(n);

      for (int i = 0; i < n; i++)
      {
        double a = 2.0 * Math.PI * i / n;
        p.Add(new Point64((long) Math.Round(r * Math.Cos(a)),
          (long) Math.Round(r * Math.Sin(a))));
      }

      return p;
    }

    // Independent boundary ground truth, so non-boundary points assert exact equality
    // rather than skipping on IsOn (which would hide a spurious IsOn).
    private static bool IsOnAnySegment(Path64 poly, Point64 pt)
    {
      for (int i = 0, j = poly.Count - 1; i < poly.Count; j = i++)
      {
        Point64 a = poly[j];
        Point64 b = poly[i];

        if (InternalClipper.CrossProductSign(a, b, pt) != 0) continue;

        if (pt.X >= Math.Min(a.X, b.X) && pt.X <= Math.Max(a.X, b.X) &&
          pt.Y >= Math.Min(a.Y, b.Y) && pt.Y <= Math.Max(a.Y, b.Y))
          return true;
      }

      return false;
    }

    // Deterministic LCG so the fuzz is reproducible.
    private sealed class Lcg
    {
      private ulong _state;

      public Lcg(ulong seed) { _state = seed; }

      public long Next(long lo, long hi)
      {
        _state = _state * 6364136223846793005UL + 1442695040888963407UL;
        ulong span = (ulong) (hi - lo + 1);
        return lo + (long) ((_state >> 33) % span);
      }
    }

    [TestMethod]
    public void InsidePoint()
    {
      StripeIndex index = new StripeIndex();
      index.BuildFromPath(MakeSquare(100));

      Assert.AreEqual(PointInPolygonResult.IsInside, index.PointInPolygon(new Point64(50, 50)));
    }

    [TestMethod]
    public void OutsidePoint()
    {
      StripeIndex index = new StripeIndex();
      index.BuildFromPath(MakeSquare(100));

      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(150, 50)));
      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(-10, 50)));
      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(50, 200)));
      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(50, -5)));
    }

    [TestMethod]
    public void OnEdge()
    {
      StripeIndex index = new StripeIndex();
      index.BuildFromPath(MakeSquare(100));

      Assert.AreEqual(PointInPolygonResult.IsOn, index.PointInPolygon(new Point64(50, 0)));   // bottom (horizontal)
      Assert.AreEqual(PointInPolygonResult.IsOn, index.PointInPolygon(new Point64(100, 50))); // right (vertical)
    }

    [TestMethod]
    public void OnVertex()
    {
      StripeIndex index = new StripeIndex();
      index.BuildFromPath(MakeSquare(100));

      Assert.AreEqual(PointInPolygonResult.IsOn, index.PointInPolygon(new Point64(0, 0)));
      Assert.AreEqual(PointInPolygonResult.IsOn, index.PointInPolygon(new Point64(100, 100)));
    }

    [TestMethod]
    public void CollinearExtensionIsNotOn()
    {
      // Collinear with an edge's line but past the segment, sharing its stripe: not IsOn.
      StripeIndex index = new StripeIndex();
      index.BuildFromPath(MakeSquare(100));

      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(200, 0)));
      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(-50, 0)));
      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(100, 200)));
    }

    [TestMethod]
    public void MatchesBruteForceOnConcavePolygon()
    {
      // A concave "C" shape exercises the parity logic on a non-convex ring.
      Path64 poly = new Path64
      {
        new Point64(0, 0), new Point64(100, 0), new Point64(100, 30), new Point64(30, 30),
        new Point64(30, 70), new Point64(100, 70), new Point64(100, 100), new Point64(0, 100)
      };

      StripeIndex index = new StripeIndex();
      index.BuildFromPath(poly);

      for (long y = -5; y <= 105; y += 3)
      {
        for (long x = -5; x <= 105; x += 3)
        {
          Point64 pt = new Point64(x, y);
          PointInPolygonResult actual = index.PointInPolygon(pt);

          if (IsOnAnySegment(poly, pt))
            Assert.AreEqual(PointInPolygonResult.IsOn, actual, $"boundary at ({x},{y})");
          else
            Assert.AreEqual(Clipper.PointInPolygon(pt, poly), actual, $"mismatch at ({x},{y})");
        }
      }
    }

    [TestMethod]
    public void DegeneratePathsAreOutside()
    {
      StripeIndex empty = new StripeIndex();
      empty.BuildFromPath(new Path64());
      Assert.IsTrue(empty.Empty);
      Assert.AreEqual(PointInPolygonResult.IsOutside, empty.PointInPolygon(new Point64(0, 0)));

      StripeIndex segment = new StripeIndex();
      segment.BuildFromPath(new Path64 { new Point64(0, 0), new Point64(10, 0) });
      Assert.AreEqual(PointInPolygonResult.IsOutside, segment.PointInPolygon(new Point64(5, 0)));
    }

    [TestMethod]
    public void MultiStripeMatchesBruteForce()
    {
      Path64 poly = MakeCircle(10000, 256);
      Assert.IsTrue(poly.Count > 64); // ensures sqrt(edges) yields several stripes

      StripeIndex index = new StripeIndex();
      index.BuildFromPath(poly);

      Lcg rng = new Lcg(0x9E3779B97F4A7C15UL);
      int checked_ = 0;

      for (int i = 0; i < 20000; i++)
      {
        Point64 pt = new Point64(rng.Next(-10200, 10200), rng.Next(-10200, 10200));
        PointInPolygonResult actual = index.PointInPolygon(pt);

        if (IsOnAnySegment(poly, pt))
          Assert.AreEqual(PointInPolygonResult.IsOn, actual, $"boundary at ({pt.X},{pt.Y})");
        else
        {
          Assert.AreEqual(Clipper.PointInPolygon(pt, poly), actual, $"mismatch at ({pt.X},{pt.Y})");
          ++checked_;
        }
      }

      Assert.IsTrue(checked_ > 1000); // exercised enough non-boundary points
    }

    [TestMethod]
    public void AllHorizontalRingHasNoInterior()
    {
      // Zero-area ring (all vertices on one row) has no interior. Half-open convention
      // keeps horizontal edges out of parity, so nothing fabricates an inside.
      Path64 poly = new Path64
      {
        new Point64(0, 10), new Point64(50, 10), new Point64(100, 10)
      };

      StripeIndex index = new StripeIndex();
      index.BuildFromPath(poly);

      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(50, 0)));
      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(50, 20)));
      Assert.AreEqual(PointInPolygonResult.IsOutside, index.PointInPolygon(new Point64(200, 10)));
    }

    [TestMethod]
    public void BoundaryRowPointsMatchBruteForce()
    {
      // Points on the y_min, y_max and vertex rows stress half-open parity and stripe clamping.
      Path64 poly = new Path64
      {
        new Point64(0, 0), new Point64(100, 0), new Point64(60, 50),
        new Point64(100, 100), new Point64(0, 100), new Point64(40, 50)
      };

      StripeIndex index = new StripeIndex();
      index.BuildFromPath(poly);

      foreach (long y in new long[] { 0, 50, 100 })
      {
        for (long x = -10; x <= 110; x++)
        {
          Point64 pt = new Point64(x, y);
          PointInPolygonResult actual = index.PointInPolygon(pt);

          if (IsOnAnySegment(poly, pt))
            Assert.AreEqual(PointInPolygonResult.IsOn, actual, $"boundary at ({x},{y})");
          else
            Assert.AreEqual(Clipper.PointInPolygon(pt, poly), actual, $"mismatch at ({x},{y})");
        }
      }
    }

    // BuildFromRing is internal (takes OutPt); the PolyTree differentials below exercise it.

    [TestMethod]
    public void AdversarialCombRingDeclinesIndexing()
    {
      // Every edge spans the full Y range, landing in every stripe, so the index declines
      // and stays empty (callers fall back to brute force).
      const long height = 100000;
      Path64 comb = new Path64();

      for (int i = 0; i < 200; i++)
        comb.Add(new Point64(i, (i & 1) == 1 ? height : 0));

      StripeIndex index = new StripeIndex();
      index.BuildFromPath(comb);

      Assert.IsTrue(index.Empty);
    }

    // --- PolyTree nesting differentials -----------------------------------

    // FNV-1a hash of a polygon's coordinates, so the tree signature distinguishes
    // nodes with the same vertex count and area but different geometry.
    private static ulong HashPolygon(Path64 poly)
    {
      ulong h = 1469598103934665603UL;

      foreach (Point64 pt in poly)
      {
        h = (h ^ (ulong) pt.X) * 1099511628211UL;
        h = (h ^ (ulong) pt.Y) * 1099511628211UL;
      }

      return h;
    }

    // Depth-first signature (level, hole flag, vertex count, coord hash per node) for
    // exact tree comparison.
    private static void SerializeTree(PolyPath64 node, StringBuilder sb)
    {
      for (int i = 0; i < node.Count; i++)
      {
        PolyPath64 c = node[i];
        sb.Append(c.Level);
        sb.Append(c.IsHole ? 'H' : 'S');
        sb.Append(c.Polygon!.Count);
        sb.Append(':');
        sb.Append(HashPolygon(c.Polygon!));
        sb.Append(';');
        SerializeTree(c, sb);
      }
    }

    private static string UnionTreeSignature(Paths64 subject, int threshold)
    {
      Clipper64 clipper = new Clipper64();
      clipper.PipIndexThreshold = threshold;
      clipper.AddSubject(subject);

      PolyTree64 tree = new PolyTree64();
      Paths64 open = new Paths64();
      clipper.Execute(ClipType.Union, FillRule.EvenOdd, tree, open);

      StringBuilder sb = new StringBuilder();
      SerializeTree(tree, sb);
      return sb.ToString();
    }

    [TestMethod]
    public void NestedPolyTreeIdenticalWithAndWithoutIndex()
    {
      // Concentric circles under EvenOdd deeply nest solids and holes, exercising owner
      // determination.
      Paths64 subject = new Paths64
      {
        MakeCircle(10000, 220), MakeCircle(7500, 200),
        MakeCircle(5000, 180), MakeCircle(2500, 160)
      };

      string brute = UnionTreeSignature(subject, int.MaxValue); // force brute force
      string indexed = UnionTreeSignature(subject, 1);          // force stripe index

      Assert.IsFalse(string.IsNullOrEmpty(brute));
      Assert.IsTrue(brute.Contains('H')); // confirm the tree really nests
      Assert.AreEqual(brute, indexed);
    }

    [TestMethod]
    public void IndexNotStaleAcrossReusedClipper()
    {
      // Clipper64 pools OutRec across Execute calls; if the cached index isn't cleared on
      // reuse, the second run reads an index built from the first run's geometry.
      Paths64 first = new Paths64
      {
        MakeCircle(10000, 220), MakeCircle(7500, 200), MakeCircle(5000, 180)
      };
      Paths64 second = new Paths64
      {
        MakeCircle(8000, 200), MakeCircle(5500, 180), MakeCircle(3000, 160)
      };

      Clipper64 reused = new Clipper64();
      reused.PipIndexThreshold = 1; // force the index on both runs

      PolyTree64 t1 = new PolyTree64();
      Paths64 open1 = new Paths64();
      reused.AddSubject(first);
      reused.Execute(ClipType.Union, FillRule.EvenOdd, t1, open1);

      reused.Clear();

      PolyTree64 t2 = new PolyTree64();
      Paths64 open2 = new Paths64();
      reused.AddSubject(second);
      reused.Execute(ClipType.Union, FillRule.EvenOdd, t2, open2);

      StringBuilder reusedSig = new StringBuilder();
      SerializeTree(t2, reusedSig);

      // A fresh clipper on the same second input is the ground truth.
      string freshSig = UnionTreeSignature(second, 1);

      Assert.IsFalse(string.IsNullOrEmpty(reusedSig.ToString()));
      Assert.AreEqual(freshSig, reusedSig.ToString());
    }

    // --- Real-world stress differentials ----------------------------------
    // Large fractured polygons, one parent owning 1000+ children. Skip if data files absent.

    private sealed class TreeStats
    {
      public int Nodes;
      public int MaxChildren;
      public int Holes;
    }

    private static void GatherStats(PolyPath64 node, TreeStats s)
    {
      if (node.Count > s.MaxChildren) s.MaxChildren = node.Count;

      for (int i = 0; i < node.Count; i++)
      {
        PolyPath64 c = node[i];
        ++s.Nodes;

        if (c.IsHole) ++s.Holes;

        GatherStats(c, s);
      }
    }

    // Walk up to the repo Tests folder so data files resolve regardless of build layout.
    private static string? FindTestFile(string name)
    {
      DirectoryInfo? dir = new DirectoryInfo(Directory.GetCurrentDirectory());

      while (dir != null)
      {
        string candidate = Path.Combine(dir.FullName, "Tests", name);
        if (File.Exists(candidate)) return candidate;

        dir = dir.Parent;
      }

      return null;
    }

    private static string TreeSignature(Paths64 subj, Paths64 clip,
      ClipType ct, FillRule fr, int threshold)
    {
      Clipper64 clipper = new Clipper64();
      clipper.PipIndexThreshold = threshold;
      clipper.AddSubject(subj);
      clipper.AddClip(clip);

      PolyTree64 tree = new PolyTree64();
      Paths64 open = new Paths64();
      clipper.Execute(ct, fr, tree, open);

      StringBuilder sb = new StringBuilder();
      SerializeTree(tree, sb);
      return sb.ToString();
    }

    [TestMethod]
    public void SubtractTreeIdenticalWithAndWithoutIndex()
    {
      string? file = FindTestFile("clipper_subtract.txt");
      if (file == null) Assert.Inconclusive("clipper_subtract.txt not present");

      Paths64 subj = new Paths64(), subjOpen = new Paths64(), clip = new Paths64();
      Assert.IsTrue(ClipperFileIO.LoadTestNum(file!, 1, subj, subjOpen, clip,
        out ClipType ct, out FillRule fr, out _, out _, out _),
        "failed to load clipper_subtract.txt");

      Clipper64 clipper = new Clipper64();
      clipper.PipIndexThreshold = int.MaxValue;
      clipper.AddSubject(subj);
      clipper.AddClip(clip);
      PolyTree64 bruteTree = new PolyTree64();
      Paths64 open = new Paths64();
      clipper.Execute(ct, fr, bruteTree, open);

      TreeStats stats = new TreeStats();
      GatherStats(bruteTree, stats);
      Assert.IsTrue(stats.MaxChildren > 1000, $"max_children={stats.MaxChildren}"); // heavy single-parent nesting
      Assert.IsTrue(stats.Holes > 0);

      StringBuilder sb = new StringBuilder();
      SerializeTree(bruteTree, sb);
      string brute = sb.ToString();
      string indexed = TreeSignature(subj, clip, ct, fr, 1);

      Assert.IsFalse(string.IsNullOrEmpty(brute));
      Assert.AreEqual(brute, indexed);
    }

    [TestMethod]
    public void InflateUnionTreeIdenticalWithAndWithoutIndex()
    {
      string? file = FindTestFile("clipper_inflate.txt");
      if (file == null) Assert.Inconclusive("clipper_inflate.txt not present");

      Paths64 subj = new Paths64(), subjOpen = new Paths64(), clip = new Paths64();
      Assert.IsTrue(ClipperFileIO.LoadTestNum(file!, 1, subj, subjOpen, clip,
        out _, out _, out _, out _, out _),
        "failed to load clipper_inflate.txt");

      // OFFSET case; union its subjects to exercise nesting on the large real polygon.
      Clipper64 clipper = new Clipper64();
      clipper.PipIndexThreshold = int.MaxValue;
      clipper.AddSubject(subj);
      PolyTree64 bruteTree = new PolyTree64();
      Paths64 open = new Paths64();
      clipper.Execute(ClipType.Union, FillRule.NonZero, bruteTree, open);

      TreeStats stats = new TreeStats();
      GatherStats(bruteTree, stats);
      Assert.IsTrue(stats.MaxChildren > 500, $"max_children={stats.MaxChildren}"); // substantial nesting
      Assert.IsTrue(stats.Holes > 0);

      StringBuilder sb = new StringBuilder();
      SerializeTree(bruteTree, sb);
      string brute = sb.ToString();
      // Use the production default threshold; the subtract case forces the index (threshold 1).
      string indexed = TreeSignature(subj, new Paths64(), ClipType.Union, FillRule.NonZero, 64);

      Assert.IsFalse(string.IsNullOrEmpty(brute));
      Assert.AreEqual(brute, indexed);
    }
  }
}
