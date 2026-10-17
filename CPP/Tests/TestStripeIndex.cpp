#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include "clipper2/clipper.engine.h"
#include "clipper2/clipper.core.h"
#include "ClipFileLoad.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using namespace Clipper2Lib;

namespace
{
  Path64 MakeSquare(int64_t size)
  {
    const int64_t z = 0;
    return { Point64(z, z), Point64(size, z), Point64(size, size), Point64(z, size) };
  }

  // Many-edged ring so the index spans several stripes.
  Path64 MakeCircle(int64_t r, int n)
  {
    const double pi = 3.14159265358979323846;
    Path64 p;
    p.reserve(n);

    for (int i = 0; i < n; ++i)
    {
      const double a = 2.0 * pi * i / n;
      p.push_back(Point64(static_cast<int64_t>(std::llround(r * std::cos(a))),
        static_cast<int64_t>(std::llround(r * std::sin(a)))));
    }

    return p;
  }

  // Heap-allocated closed OutPt ring for exercising StripeIndex::BuildFromRing.
  struct OutPtRing
  {
    std::vector<OutPt*> nodes;

    explicit OutPtRing(const Path64& path)
    {
      OutPt* prev = nullptr;

      for (const Point64& pt : path)
      {
        OutPt* op = new OutPt(pt, nullptr);
        nodes.push_back(op);

        if (prev)
        {
          prev->next = op;
          op->prev = prev;
        }

        prev = op;
      }

      if (!nodes.empty())
      {
        nodes.back()->next = nodes.front();
        nodes.front()->prev = nodes.back();
      }
    }

    OutPt* head() const { return nodes.empty() ? nullptr : nodes.front(); }

    ~OutPtRing()
    {
      for (OutPt* op : nodes) delete op;
    }

    OutPtRing(const OutPtRing&) = delete;
    OutPtRing& operator=(const OutPtRing&) = delete;
  };

  // Independent ground truth for "pt on boundary" so fuzz can assert exact equality
  // off-boundary rather than skipping every IsOn (which would mask a spurious IsOn).
  bool IsOnAnySegment(const Path64& poly, const Point64& pt)
  {
    for (size_t i = 0, j = poly.size() - 1; i < poly.size(); j = i++)
    {
      const Point64& a = poly[j];
      const Point64& b = poly[i];

      if (CrossProductSign(a, b, pt) != 0) continue;

      if (pt.x >= std::min(a.x, b.x) && pt.x <= std::max(a.x, b.x) &&
        pt.y >= std::min(a.y, b.y) && pt.y <= std::max(a.y, b.y))
        return true;
    }

    return false;
  }

  // Deterministic LCG so the fuzz is reproducible.
  struct Lcg
  {
    uint64_t state;
    explicit Lcg(uint64_t seed): state(seed) {}
    int64_t Next(int64_t lo, int64_t hi)
    {
      state = state * 6364136223846793005ULL + 1442695040888963407ULL;
      const uint64_t span = static_cast<uint64_t>(hi - lo + 1);
      return lo + static_cast<int64_t>((state >> 33) % span);
    }
  };
}

TEST(StripeIndexTests, InsidePoint)
{
  StripeIndex index;
  index.BuildFromPath(MakeSquare(100));

  EXPECT_EQ(index.PointInPolygon(Point64(50, 50)), PointInPolygonResult::IsInside);
}

TEST(StripeIndexTests, OutsidePoint)
{
  StripeIndex index;
  index.BuildFromPath(MakeSquare(100));

  EXPECT_EQ(index.PointInPolygon(Point64(150, 50)), PointInPolygonResult::IsOutside);
  EXPECT_EQ(index.PointInPolygon(Point64(-10, 50)), PointInPolygonResult::IsOutside);
  EXPECT_EQ(index.PointInPolygon(Point64(50, 200)), PointInPolygonResult::IsOutside);
  EXPECT_EQ(index.PointInPolygon(Point64(50, -5)), PointInPolygonResult::IsOutside);
}

TEST(StripeIndexTests, OnEdge)
{
  StripeIndex index;
  index.BuildFromPath(MakeSquare(100));

  EXPECT_EQ(index.PointInPolygon(Point64(50, 0)), PointInPolygonResult::IsOn);   // bottom (horizontal) edge
  EXPECT_EQ(index.PointInPolygon(Point64(100, 50)), PointInPolygonResult::IsOn); // right (vertical) edge
}

TEST(StripeIndexTests, OnVertex)
{
  StripeIndex index;
  index.BuildFromPath(MakeSquare(100));

  EXPECT_EQ(index.PointInPolygon(Point64(0, 0)), PointInPolygonResult::IsOn);
  EXPECT_EQ(index.PointInPolygon(Point64(100, 100)), PointInPolygonResult::IsOn);
}

TEST(StripeIndexTests, CollinearExtensionIsNotOn)
{
  // Points on an edge's infinite line but off the segment must not read IsOn,
  // even when they share that edge's stripe.
  StripeIndex index;
  index.BuildFromPath(MakeSquare(100));

  EXPECT_EQ(index.PointInPolygon(Point64(200, 0)), PointInPolygonResult::IsOutside);
  EXPECT_EQ(index.PointInPolygon(Point64(-50, 0)), PointInPolygonResult::IsOutside);
  EXPECT_EQ(index.PointInPolygon(Point64(100, 200)), PointInPolygonResult::IsOutside); // above right edge line
}

TEST(StripeIndexTests, MatchesBruteForceOnConcavePolygon)
{
  // A concave "C" shape exercises the parity logic on a non-convex ring.
  const Path64 poly = {
    Point64(0, 0), Point64(100, 0), Point64(100, 30), Point64(30, 30),
    Point64(30, 70), Point64(100, 70), Point64(100, 100), Point64(0, 100)
  };

  StripeIndex index;
  index.BuildFromPath(poly);

  for (int64_t y = -5; y <= 105; y += 3)
  {
    for (int64_t x = -5; x <= 105; x += 3)
    {
      const Point64 pt(x, y);
      const PointInPolygonResult actual = index.PointInPolygon(pt);

      if (IsOnAnySegment(poly, pt))
        EXPECT_EQ(actual, PointInPolygonResult::IsOn) << "boundary at (" << x << "," << y << ")";
      else
        EXPECT_EQ(actual, PointInPolygon(pt, poly)) << "mismatch at (" << x << "," << y << ")";
    }
  }
}

TEST(StripeIndexTests, DegeneratePathsAreOutside)
{
  const int64_t z = 0;

  StripeIndex empty;
  empty.BuildFromPath(Path64{});
  EXPECT_TRUE(empty.Empty());
  EXPECT_EQ(empty.PointInPolygon(Point64(z, z)), PointInPolygonResult::IsOutside);

  StripeIndex segment;
  segment.BuildFromPath(Path64{ Point64(z, z), Point64(int64_t(10), z) });
  EXPECT_EQ(segment.PointInPolygon(Point64(int64_t(5), z)), PointInPolygonResult::IsOutside);
}

TEST(StripeIndexTests, MultiStripeMatchesBruteForce)
{
  const Path64 poly = MakeCircle(10000, 256);
  ASSERT_GT(poly.size(), 64u); // ensures sqrt(edges) yields several stripes

  StripeIndex index;
  index.BuildFromPath(poly);

  Lcg rng(0x9E3779B97F4A7C15ULL);
  int checked = 0;

  for (int i = 0; i < 20000; ++i)
  {
    const Point64 pt(rng.Next(-10200, 10200), rng.Next(-10200, 10200));
    const PointInPolygonResult actual = index.PointInPolygon(pt);

    if (IsOnAnySegment(poly, pt))
    {
      ASSERT_EQ(actual, PointInPolygonResult::IsOn) << "boundary at (" << pt.x << "," << pt.y << ")";
    }
    else
    {
      ASSERT_EQ(actual, PointInPolygon(pt, poly)) << "mismatch at (" << pt.x << "," << pt.y << ")";
      ++checked;
    }
  }

  EXPECT_GT(checked, 1000); // sanity: the loop actually exercised non-boundary points
}

TEST(StripeIndexTests, AllHorizontalRingHasNoInterior)
{
  // Zero-area ring (all vertices on one row) has no interior; half-open parity keeps
  // horizontal edges from fabricating an inside.
  const Path64 poly = {
    Point64(int64_t(0), int64_t(10)), Point64(int64_t(50), int64_t(10)),
    Point64(int64_t(100), int64_t(10))
  };

  StripeIndex index;
  index.BuildFromPath(poly);

  EXPECT_EQ(index.PointInPolygon(Point64(int64_t(50), int64_t(0))), PointInPolygonResult::IsOutside);
  EXPECT_EQ(index.PointInPolygon(Point64(int64_t(50), int64_t(20))), PointInPolygonResult::IsOutside);
  EXPECT_EQ(index.PointInPolygon(Point64(int64_t(200), int64_t(10))), PointInPolygonResult::IsOutside);
}

TEST(StripeIndexTests, BoundaryRowPointsMatchBruteForce)
{
  // Points on the y_min, y_max and vertex rows stress half-open parity and clamping.
  const Path64 poly = {
    Point64(int64_t(0), int64_t(0)), Point64(int64_t(100), int64_t(0)),
    Point64(int64_t(60), int64_t(50)), Point64(int64_t(100), int64_t(100)),
    Point64(int64_t(0), int64_t(100)), Point64(int64_t(40), int64_t(50))
  };

  StripeIndex index;
  index.BuildFromPath(poly);

  for (int64_t y : { int64_t(0), int64_t(50), int64_t(100) })
  {
    for (int64_t x = -10; x <= 110; ++x)
    {
      const Point64 pt(x, y);
      const PointInPolygonResult actual = index.PointInPolygon(pt);

      if (IsOnAnySegment(poly, pt))
        EXPECT_EQ(actual, PointInPolygonResult::IsOn) << "boundary at (" << x << "," << y << ")";
      else
        EXPECT_EQ(actual, PointInPolygon(pt, poly)) << "mismatch at (" << x << "," << y << ")";
    }
  }
}

TEST(StripeIndexTests, BuildFromRingMatchesBuildFromPath)
{
  const Path64 poly = MakeCircle(5000, 97); // odd count, ensures distinct edges
  OutPtRing ring(poly);

  StripeIndex from_path;
  from_path.BuildFromPath(poly);

  StripeIndex from_ring;
  from_ring.BuildFromRing(ring.head());

  EXPECT_EQ(from_path.EdgeCount(), from_ring.EdgeCount());

  Lcg rng(0xD1B54A32D192ED03ULL);

  for (int i = 0; i < 5000; ++i)
  {
    const Point64 pt(rng.Next(-5200, 5200), rng.Next(-5200, 5200));
    EXPECT_EQ(from_ring.PointInPolygon(pt), from_path.PointInPolygon(pt))
      << "ring vs path mismatch at (" << pt.x << "," << pt.y << ")";
  }
}

TEST(StripeIndexTests, AdversarialCombRingDeclinesIndexing)
{
  // Tall zigzag where every edge spans the full Y range, so each lands in every
  // stripe. The index declines it (leaves itself empty) rather than lose to brute force.
  const int64_t height = 100000;
  Path64 comb;

  for (int i = 0; i < 200; ++i)
    comb.push_back(Point64(static_cast<int64_t>(i), (i & 1) ? height : static_cast<int64_t>(0)));

  StripeIndex index;
  index.BuildFromPath(comb);

  EXPECT_TRUE(index.Empty());
}

namespace
{
  // FNV-1a hash of coordinates so the signature distinguishes same-size, different geometry.
  uint64_t HashPolygon(const Path64& poly)
  {
    uint64_t h = 1469598103934665603ULL;

    for (const Point64& pt : poly)
    {
      h = (h ^ static_cast<uint64_t>(pt.x)) * 1099511628211ULL;
      h = (h ^ static_cast<uint64_t>(pt.y)) * 1099511628211ULL;
    }

    return h;
  }

  // Depth-first PolyTree signature (level, hole flag, vertex count, coord hash per node).
  void SerializeTree(const PolyPath64& node, std::string& out)
  {
    for (size_t i = 0; i < node.Count(); ++i)
    {
      const PolyPath64* c = node[i];
      out += std::to_string(c->Level());
      out += c->IsHole() ? "H" : "S";
      out += std::to_string(c->Polygon().size());
      out += ":";
      out += std::to_string(HashPolygon(c->Polygon()));
      out += ";";
      SerializeTree(*c, out);
    }
  }

  std::string UnionTreeSignature(const Paths64& subject, size_t threshold)
  {
    Clipper64 clipper;
    clipper.PipIndexThreshold(threshold);
    clipper.AddSubject(subject);

    PolyTree64 tree;
    Paths64 open;
    clipper.Execute(ClipType::Union, FillRule::EvenOdd, tree, open);

    std::string sig;
    SerializeTree(tree, sig);
    return sig;
  }
}

TEST(StripeIndexTests, NestedPolyTreeIdenticalWithAndWithoutIndex)
{
  // Concentric circles nest as alternating solids and holes, exercising the
  // owner determination the index accelerates.
  const Paths64 subject = {
    MakeCircle(10000, 220), MakeCircle(7500, 200),
    MakeCircle(5000, 180), MakeCircle(2500, 160)
  };

  const std::string brute = UnionTreeSignature(subject, SIZE_MAX); // force brute-force
  const std::string indexed = UnionTreeSignature(subject, 1);      // force stripe index

  EXPECT_FALSE(brute.empty());
  EXPECT_NE(brute.find('H'), std::string::npos); // sanity: the tree really nests
  EXPECT_EQ(indexed, brute);
}

// Real-world stress cases exercise nesting where one parent owns >1000 children.
// They skip if the data file is absent, and run only in the non-Z build (nesting is
// USINGZ-independent) to keep the suite fast.
#ifndef USINGZ
namespace
{
  struct TreeStats
  {
    size_t nodes = 0;
    size_t max_children = 0;
    size_t holes = 0;
  };

  void GatherStats(const PolyPath64& node, TreeStats& s)
  {
    if (node.Count() > s.max_children) s.max_children = node.Count();

    for (size_t i = 0; i < node.Count(); ++i)
    {
      const PolyPath64* c = node[i];
      ++s.nodes;

      if (c->IsHole()) ++s.holes;

      GatherStats(*c, s);
    }
  }

  bool LoadRealWorld(const std::string& name, Paths64& subj, Paths64& clip,
    ClipType& ct, FillRule& fr)
  {
    std::ifstream ifs(name);

    if (!ifs.good()) return false;

    Paths64 subj_open;
    int64_t area = 0, count = 0;
    return LoadTestNum(ifs, 1, subj, subj_open, clip, area, count, ct, fr);
  }

  std::string TreeSignature(const Paths64& subj, const Paths64& clip,
    ClipType ct, FillRule fr, size_t threshold)
  {
    Clipper64 clipper;
    clipper.PipIndexThreshold(threshold);
    clipper.AddSubject(subj);
    clipper.AddClip(clip);

    PolyTree64 tree;
    Paths64 open;
    clipper.Execute(ct, fr, tree, open);

    std::string sig;
    SerializeTree(tree, sig);
    return sig;
  }

  // Times only Execute, excluding serialization, to isolate the accelerated path.
  double TimeTreeBuildMs(const Paths64& subj, const Paths64& clip,
    ClipType ct, FillRule fr, size_t threshold)
  {
    Clipper64 clipper;
    clipper.PipIndexThreshold(threshold);
    clipper.AddSubject(subj);
    clipper.AddClip(clip);

    PolyTree64 tree;
    Paths64 open;

    const auto start = std::chrono::steady_clock::now();
    clipper.Execute(ct, fr, tree, open);
    const auto end = std::chrono::steady_clock::now();

    EXPECT_GT(tree.Count(), 0u);
    return std::chrono::duration<double, std::milli>(end - start).count();
  }
}

TEST(StripeIndexRealWorld, SubtractTreeIdenticalWithAndWithoutIndex)
{
  Paths64 subj, clip;
  ClipType ct = ClipType::NoClip;
  FillRule fr = FillRule::NonZero;

  if (!LoadRealWorld("clipper_subtract.txt", subj, clip, ct, fr))
    GTEST_SKIP() << "clipper_subtract.txt not present";

  Clipper64 clipper;
  clipper.PipIndexThreshold(SIZE_MAX);
  clipper.AddSubject(subj);
  clipper.AddClip(clip);
  PolyTree64 brute_tree;
  Paths64 open;
  clipper.Execute(ct, fr, brute_tree, open);

  TreeStats stats;
  GatherStats(brute_tree, stats);
  EXPECT_GT(stats.max_children, 1000u); // confirms heavy single-parent nesting
  EXPECT_GT(stats.holes, 0u);

  std::string brute;
  SerializeTree(brute_tree, brute);
  const std::string indexed = TreeSignature(subj, clip, ct, fr, 1);

  EXPECT_FALSE(brute.empty());
  EXPECT_EQ(indexed, brute);
}

TEST(StripeIndexRealWorld, InflateUnionTreeIdenticalWithAndWithoutIndex)
{
  Paths64 subj, clip;
  ClipType ct = ClipType::NoClip;
  FillRule fr = FillRule::NonZero;

  if (!LoadRealWorld("clipper_inflate.txt", subj, clip, ct, fr))
    GTEST_SKIP() << "clipper_inflate.txt not present";

  // The file is an OFFSET case; union its subjects to exercise nesting.
  Clipper64 clipper;
  clipper.PipIndexThreshold(SIZE_MAX);
  clipper.AddSubject(subj);
  PolyTree64 brute_tree;
  Paths64 open;
  clipper.Execute(ClipType::Union, FillRule::NonZero, brute_tree, open);

  TreeStats stats;
  GatherStats(brute_tree, stats);
  EXPECT_GT(stats.max_children, 500u); // confirms substantial single-parent nesting
  EXPECT_GT(stats.holes, 0u);

  std::string brute;
  SerializeTree(brute_tree, brute);
  // Validate the production default threshold; subtract case covers threshold 1.
  const std::string indexed = TreeSignature(subj, {}, ClipType::Union, FillRule::NonZero, 64);

  EXPECT_FALSE(brute.empty());
  EXPECT_EQ(indexed, brute);
}

TEST(StripeIndexRealWorld, IndexFasterOnLargeNesting)
{
  Paths64 subj, clip;
  ClipType ct = ClipType::NoClip;
  FillRule fr = FillRule::NonZero;

  if (!LoadRealWorld("clipper_subtract.txt", subj, clip, ct, fr))
    GTEST_SKIP() << "clipper_subtract.txt not present";

  // Warm up caches and the allocator before timing.
  TimeTreeBuildMs(subj, clip, ct, fr, SIZE_MAX);
  TimeTreeBuildMs(subj, clip, ct, fr, 64);

  double brute_best = 1e18;
  double index_best = 1e18;

  for (int i = 0; i < 3; ++i)
  {
    brute_best = std::min(brute_best, TimeTreeBuildMs(subj, clip, ct, fr, SIZE_MAX));
    index_best = std::min(index_best, TimeTreeBuildMs(subj, clip, ct, fr, 64));
  }

  std::cout << "[ PERF     ] subtract PolyTree brute=" << brute_best
    << "ms default-index=" << index_best << "ms" << std::endl;

  // Skip when the baseline is too small to time reliably (noise dominates).
  if (brute_best < 20.0)
    GTEST_SKIP() << "brute baseline too small to compare reliably";

  // Require a clear win with margin, best-of-N so the assertion does not flake on noise.
  EXPECT_LT(index_best, brute_best * 0.9);
}
#endif // !USINGZ
