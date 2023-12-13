#include "benchmark/benchmark.h"
#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "CommonUtils.h"
#include "ClipFileLoad.h"
#include <iostream>
#include <iomanip> 
#include <cstdlib>
#include <random>

using namespace Clipper2Lib;

enum ConsoleTextColor {
  reset = 0,
  //normal text colors ...
  red = 31, green = 32, yellow = 33, blue = 34, magenta = 35, cyan = 36, white = 37,
  //bold text colors ...
  red_bold = 91, green_bold = 92, yellow_bold = 93, blue_bold = 94,
  magenta_bold = 95, cyan_bold = 96, white_bold = 97
};

//////////////////////////////////////////////////////////////////////////////////////
// SetConsoleTextColor: a simple class to adjust Console Text Colors (Windows & Linux)
//////////////////////////////////////////////////////////////////////////////////////

struct SetConsoleTextColor
{
private:
  ConsoleTextColor _color;
public:
  SetConsoleTextColor(ConsoleTextColor color) : _color(color) {};

  static friend std::ostream& operator<< (std::ostream& out, SetConsoleTextColor const& scc)
  {
    return out << "\x1B[" << scc._color << "m";
  }
};
//////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////
// Miscellaneous functions
/////////////////////////////////////////////////////////

double GetSineAbc(const Point64& a, const Point64& b, const Point64& c)
{
  double dpB = DotProduct(a, b, c);
  double SqrCosB = dpB * dpB / (DistanceSqr(a, b) * DistanceSqr(b, c));
  double cos2B = SqrCosB * 2 - 1; // trig. itentity
  return std::sqrt(1 - cos2B);    // sin(B) = Sqrt(1-cos(2B))
}

static inline Point64 MakeRandomPoint(int64_t min_val, int64_t max_val)
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<int64_t> x(min_val, max_val);
  std::uniform_int_distribution<int64_t> y(min_val, max_val);
  return Point64(x(gen), y(gen));
}

typedef std::function<bool(const Point64&, const Point64&,
  const Point64&, const Point64&, Point64&)> GipFunction;

/////////////////////////////////////////////////////////
// GIP_Current: This is the current Clipper2 GetIntersectPoint code
/////////////////////////////////////////////////////////
static bool GIP_Current(const Point64& ln1a, const Point64& ln1b,
  const Point64& ln2a, const Point64& ln2b, Point64& ip)
{
  double dx1 = static_cast<double>(ln1b.x - ln1a.x);
  double dy1 = static_cast<double>(ln1b.y - ln1a.y);
  double dx2 = static_cast<double>(ln2b.x - ln2a.x);
  double dy2 = static_cast<double>(ln2b.y - ln2a.y);

  double det = dy1 * dx2 - dy2 * dx1;
  if (det == 0.0) return false;
  double t = ((double)(ln1a.x - ln2a.x) * dy2 - (double)(ln1a.y - ln2a.y) * dx2) / det;
  if (t <= 0.0) ip = ln1a;
  else if (t >= 1.0) ip = ln1b;
  else
  {
    ip.x = static_cast<int64_t>(ln1a.x + t * dx1);
    ip.y = static_cast<int64_t>(ln1a.y + t * dy1);
  }
  return true;
}

/////////////////////////////////////////////////////////
// GIP_Func_F: This is mathVertexLineLineIntersection_F
// https://github.com/AngusJohnson/Clipper2/issues/317#issuecomment-1314023253
/////////////////////////////////////////////////////////
#define CC_MIN(x,y) ((x)>(y)?(y):(x))
#define CC_MAX(x,y) ((x)<(y)?(y):(x))
static bool GIP_Func_F(const Point64& ln1a, const Point64& ln1b,
  const Point64& ln2a, const Point64& ln2b, Point64& ip)
{
  double ln1dy = (double)(ln1b.y - ln1a.y);
  double ln1dx = (double)(ln1a.x - ln1b.x);
  double ln2dy = (double)(ln2b.y - ln2a.y);
  double ln2dx = (double)(ln2a.x - ln2b.x);
  double det = (ln2dy * ln1dx) - (ln1dy * ln2dx);
  if (det == 0.0) return false;
  int64_t bb0minx = CC_MIN(ln1a.x, ln1b.x);
  int64_t bb0miny = CC_MIN(ln1a.y, ln1b.y);
  int64_t bb0maxx = CC_MAX(ln1a.x, ln1b.x);
  int64_t bb0maxy = CC_MAX(ln1a.y, ln1b.y);
  int64_t bb1minx = CC_MIN(ln2a.x, ln2b.x);
  int64_t bb1miny = CC_MIN(ln2a.y, ln2b.y);
  int64_t bb1maxx = CC_MAX(ln2a.x, ln2b.x);
  int64_t bb1maxy = CC_MAX(ln2a.y, ln2b.y);
  int64_t originx = (CC_MIN(bb0maxx, bb1maxx) + CC_MAX(bb0minx, bb1minx)) >> 1;
  int64_t originy = (CC_MIN(bb0maxy, bb1maxy) + CC_MAX(bb0miny, bb1miny)) >> 1;
  double ln0c = (ln1dy * (double)(ln1a.x - originx)) + (ln1dx * (double)(ln1a.y - originy));
  double ln1c = (ln2dy * (double)(ln2a.x - originx)) + (ln2dx * (double)(ln2a.y - originy));
  double hitx = ((ln1dx * ln1c) - (ln2dx * ln0c)) / det;
  double hity = ((ln2dy * ln0c) - (ln1dy * ln1c)) / det;
  ip.x = originx + (int64_t)nearbyint(hitx);
  ip.y = originy + (int64_t)nearbyint(hity);
  return true;
}

/////////////////////////////////////////////////////////
// GIP_F_Mod: Modified GIP_Func_F (see above).
// Replaces nearbyint with static_cast. Surprisingly, while
// this function is a little faster here than GIP_Func_F, 
// it's much slower than GIP_Func_F when using it as a
// GetIntersectPoint() replacement in clipper.core.h.
/////////////////////////////////////////////////////////
#define CC_MIN(x,y) ((x)>(y)?(y):(x))
#define CC_MAX(x,y) ((x)<(y)?(y):(x))
static bool GIP_F_Mod(const Point64& ln1a, const Point64& ln1b,
  const Point64& ln2a, const Point64& ln2b, Point64& ip)
{
  double ln1dy = (double)(ln1b.y - ln1a.y);
  double ln1dx = (double)(ln1a.x - ln1b.x);
  double ln2dy = (double)(ln2b.y - ln2a.y);
  double ln2dx = (double)(ln2a.x - ln2b.x);
  double det = (ln2dy * ln1dx) - (ln1dy * ln2dx);
  if (det == 0.0) return false;
  int64_t bb0minx = CC_MIN(ln1a.x, ln1b.x);
  int64_t bb0miny = CC_MIN(ln1a.y, ln1b.y);
  int64_t bb0maxx = CC_MAX(ln1a.x, ln1b.x);
  int64_t bb0maxy = CC_MAX(ln1a.y, ln1b.y);
  int64_t bb1minx = CC_MIN(ln2a.x, ln2b.x);
  int64_t bb1miny = CC_MIN(ln2a.y, ln2b.y);
  int64_t bb1maxx = CC_MAX(ln2a.x, ln2b.x);
  int64_t bb1maxy = CC_MAX(ln2a.y, ln2b.y);
  int64_t originx = (CC_MIN(bb0maxx, bb1maxx) + CC_MAX(bb0minx, bb1minx)) >> 1;
  int64_t originy = (CC_MIN(bb0maxy, bb1maxy) + CC_MAX(bb0miny, bb1miny)) >> 1;
  double ln0c = (ln1dy * (double)(ln1a.x - originx)) + (ln1dx * (double)(ln1a.y - originy));
  double ln1c = (ln2dy * (double)(ln2a.x - originx)) + (ln2dx * (double)(ln2a.y - originy));
  double hitx = ((ln1dx * ln1c) - (ln2dx * ln0c)) / det;
  double hity = ((ln2dy * ln0c) - (ln1dy * ln1c)) / det;
  ip.x = originx + static_cast<int64_t>(hitx);
  ip.y = originy + static_cast<int64_t>(hity);
  return true;
}


struct TestRecord
{
public:
  Point64 actual, pt1, pt2, pt3, pt4;
  std::vector<Point64> results;
  TestRecord(int participants, const Point64& intersect_pt,
    const Point64& p1, const Point64& p2, const Point64& p3, const Point64& p4) :
    actual(intersect_pt), pt1(p1), pt2(p2), pt3(p3), pt4(p4) {
    results.resize(participants);
  };
};

typedef std::vector<TestRecord>::iterator test_iter;

// global data 
std::vector<TestRecord> tests;


static inline GipFunction GetGipFunc(int index)
{
  GipFunction result;
  switch (index)
  {
  case 0: result = GIP_Current; break;
  case 1: result = GIP_Func_F; break;
  case 2: result = GIP_F_Mod; break;
  default: throw "oops! - wrong function!";
  }
  return result;
}

static inline std::string GetGipFuncName(int index)
{
  std::string result;
  switch (index)
  {
  case 0: result = "GIP_Current"; break;
  case 1: result = "GIP_Func_F "; break;
  case 2: result = "GIP_F_Mod  "; break;
  default: throw "oops!";
  }
  return result;
}

/////////////////////////////////////////////////////////
// Benchmark callback functions
/////////////////////////////////////////////////////////

static void BM_GIP_Current(benchmark::State& state)
{
  Point64 ip;
  for (auto _ : state)
  {
    for (test_iter cit = tests.begin(); cit != tests.end(); ++cit)
    {
      GIP_Current((*cit).pt1, (*cit).pt2, (*cit).pt3, (*cit).pt4, ip);
      (*cit).results[0] = ip;
    }
  }
}

static void BM_GIP_Func_F(benchmark::State& state)
{
  Point64 ip;
  for (auto _ : state)
  {
    for (test_iter cit = tests.begin(); cit != tests.end(); ++cit)
    {
      GIP_Func_F((*cit).pt1, (*cit).pt2, (*cit).pt3, (*cit).pt4, ip);
      (*cit).results[1] = ip;
    }
  }
}

static void BM_GIP_F_Mod(benchmark::State& state)
{
  Point64 ip;
  for (auto _ : state)
  {
    for (test_iter cit = tests.begin(); cit != tests.end(); ++cit)
    {
      GIP_F_Mod((*cit).pt1, (*cit).pt2, (*cit).pt3, (*cit).pt4, ip);
      (*cit).results[2] = ip;
    }
  }
}

/////////////////////////////////////////////////////////
// Main Entry
/////////////////////////////////////////////////////////

int main(int argc, char** argv)
{

  const int participants = 3;

  //setup benchmarking ...
  benchmark::Initialize(0, nullptr);
  BENCHMARK(BM_GIP_Current);
  BENCHMARK(BM_GIP_Func_F);
  BENCHMARK(BM_GIP_F_Mod);

  bool first_pass = true;
  for (int current_pow10 = 12; current_pow10 <= 18; ++current_pow10)
  {
    // create multiple TestRecords containing segment pairs that intersect 
    // at their midpoints, while using random coordinates that are 
    // restricted to the specified power of 10 range
    int64_t max_coord = static_cast<int64_t>(pow(10, current_pow10));
    for (int64_t i = 0; i < 100000; ++i)
    {
      Point64 ip1 = MakeRandomPoint(-max_coord, max_coord);
      Point64 ip2 = MakeRandomPoint(-max_coord, max_coord);
      Point64 actual = MidPoint(ip1, ip2);
      Point64 ip3 = MakeRandomPoint(-max_coord, max_coord);
      Point64 ip4 = ReflectPoint(ip3, actual);
      Point64 _;

      // the closer segments are to collinear, the less accurate are
      // calculations that determine intersection points.
      // So exclude segments that are **almost** collinear. eg. sin(1deg) ~= 0.017
      if (std::abs(GetSineAbc(ip1, actual, ip3)) < 0.017) continue;
      // alternatively, only exclude segments that are collinear
      //if (!CrossProduct(ip1, actual, ip3)) continue;
      tests.push_back(TestRecord(participants, actual, ip1, ip2, ip3, ip4));
    }

    if (first_pass)
    {
      // only benchmark the GetIntersectPoint functions once because changing
      // the maximum range of coordinates won't affect function performance.
      first_pass = false;
      std::cout << std::endl << SetConsoleTextColor(green_bold) <<
        "Benchmark GetIntersectPoint performance ... " << SetConsoleTextColor(reset) <<
        std::endl << std::endl;
      // using each test function in the callback functions above, benchmarking 
      // will calculate and store intersect points for each TestRecord 
      benchmark::RunSpecifiedBenchmarks(benchmark::CreateDefaultDisplayReporter());

      std::cout << std::endl << std::endl << SetConsoleTextColor(green_bold) <<
        "Compare function accuracy ..." << SetConsoleTextColor(reset) << std::endl <<
        "and show how it deteriorates when using very large coordinate ranges." << std::endl <<
        "Distance error is the distance between the calculated and actual intersection points." << std::endl <<
        "The largest errors will occur whenever intersecting segments are almost collinear." << std::endl;
    }
    else
    {
      for (int i = 0; i < participants; ++i)
      {
        // although we're not benchmarking, we still need to collect the calculated
        // intersect points of each TestRecord for each participating function.
        Point64 ip;
        GipFunction gip_func = GetGipFunc(i);
        for (test_iter cit = tests.begin(); cit != tests.end(); ++cit)
        {
          gip_func((*cit).pt1, (*cit).pt2, (*cit).pt3, (*cit).pt4, ip);
          (*cit).results[i] = ip;
        }
      }
    }

    double avg_dists[participants] = { 0 };
    double worst_dists[participants] = { 0 };

    for (test_iter cit = tests.begin(); cit != tests.end(); ++cit)
      for (int i = 0; i < participants; ++i)
      {
        double dist = Distance((*cit).actual, (*cit).results[i]);
        avg_dists[i] += dist;
        if (dist > worst_dists[i])  worst_dists[i] = dist;
      }

    std::cout << std::endl << SetConsoleTextColor(cyan_bold) <<
      "Coordinate ranges between  +/-10^" << current_pow10 <<
      SetConsoleTextColor(reset) << std::endl;

    for (int i = 0; i < participants; ++i)
    {
      avg_dists[i] /= tests.size();
      std::cout << std::fixed << GetGipFuncName(i) <<
        ": average distance error = " << std::setprecision(2) << avg_dists[i] <<
        "; largest dist. = " << std::setprecision(0) << worst_dists[i] << std::endl;
    }
    tests.clear();
  }
  return 0;
}
