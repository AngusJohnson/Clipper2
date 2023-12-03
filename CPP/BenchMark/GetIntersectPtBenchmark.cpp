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


typedef std::function<bool(const Point64&, const Point64&,
  const Point64&, const Point64&, Point64&)> GipFunction;

/////////////////////////////////////////////////////////
// GIP1: This is the current Clipper2 PointInPolygon code
/////////////////////////////////////////////////////////
static bool GIP1(const Point64& ln1a, const Point64& ln1b,
  const Point64& ln2a, const Point64& ln2b, Point64& ip)
{
  double dx1 = static_cast<double>(ln1b.x - ln1a.x);
  double dy1 = static_cast<double>(ln1b.y - ln1a.y);
  double dx2 = static_cast<double>(ln2b.x - ln2a.x);
  double dy2 = static_cast<double>(ln2b.y - ln2a.y);

  double det = dy1 * dx2 - dy2 * dx1;
  if (det == 0.0) return false;
  double t = ((double)(ln1a.x - ln2a.x) * dy2 - (double)(ln1a.y - ln2a.y) * dx2) / det;
  if (t <= 0.0) ip = ln1a;        // ?? check further (see also #568)
  else if (t >= 1.0) ip = ln1b;   // ?? check further
  else
  {
    ip.x = static_cast<int64_t>(ln1a.x + t * dx1);
    ip.y = static_cast<int64_t>(ln1a.y + t * dy1);
  }
  return true;
}

/////////////////////////////////////////////////////////
// GIP2: This is mathVertexLineLineIntersection_F
// https://github.com/AngusJohnson/Clipper2/issues/317#issuecomment-1314023253
/////////////////////////////////////////////////////////
#define CC_MIN(x,y) ((x)>(y)?(y):(x))
#define CC_MAX(x,y) ((x)<(y)?(y):(x))
#define DETECT_HIT_OVERFLOW (0)
static bool GIP2(const Point64& ln1a, const Point64& ln1b,
  const Point64& ln2a, const Point64& ln2b, Point64& ip)
{
  double ln1dy = (double)(ln1b.y - ln1a.y);
  double ln1dx = (double)(ln1a.x - ln1b.x);
  double ln2dy = (double)(ln2b.y - ln2a.y);
  double ln2dx = (double)(ln2a.x - ln2b.x);
  double det = (ln2dy * ln1dx) - (ln1dy * ln2dx);
  if (det == 0.0) return 0;
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
#if DETECT_HIT_OVERFLOW
  if (fmax(fabs((double)originx + hitx),
    fabs((double)originy + hity)) >= (double)(INT64_MAX - 1))  return 0;
#endif
  ip.x = originx + (int64_t)nearbyint(hitx);
  ip.y = originy + (int64_t)nearbyint(hity);
  return 1;
}

/////////////////////////////////////////////////////////
// GIP3: mathVertexLineLineIntersection_F but without calling nearbyint
// https://github.com/AngusJohnson/Clipper2/issues/317#issuecomment-1314023253
/////////////////////////////////////////////////////////
#define CC_MIN(x,y) ((x)>(y)?(y):(x))
#define CC_MAX(x,y) ((x)<(y)?(y):(x))
#define DETECT_HIT_OVERFLOW (0)
static bool GIP3(const Point64& ln1a, const Point64& ln1b,
  const Point64& ln2a, const Point64& ln2b, Point64& ip)
{
  double ln1dy = (double)(ln1b.y - ln1a.y);
  double ln1dx = (double)(ln1a.x - ln1b.x);
  double ln2dy = (double)(ln2b.y - ln2a.y);
  double ln2dx = (double)(ln2a.x - ln2b.x);
  double det = (ln2dy * ln1dx) - (ln1dy * ln2dx);
  if (det == 0.0) return 0;
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
#if DETECT_HIT_OVERFLOW
  if (fmax(fabs((double)originx + hitx), 
    fabs((double)originy + hity)) >= (double)(INT64_MAX - 1))  return 0;
#endif
  ip.x = originx + static_cast<int64_t>(hitx);
  ip.y = originy + static_cast<int64_t>(hity);  
  //ip.x = originx + (int64_t)nearbyint(hitx);
  //ip.y = originy + (int64_t)nearbyint(hity);
  return 1;
}

struct TestRecord
{
public:
  Point64 ideal, pt1, pt2, pt3, pt4;
  Path64 results;
  TestRecord(int participants, const Point64& ip,
    const Point64& p1, const Point64& p2, const Point64& p3, const Point64& p4) :
    ideal(ip), pt1(p1), pt2(p2), pt3(p3), pt4(p4) { results.resize(participants); };
};

typedef std::unique_ptr<TestRecord> TestRecord_ptr;

// global data 
std::vector<TestRecord_ptr> tests;
typedef std::vector<TestRecord_ptr>::const_iterator test_iter;

inline GipFunction GetGipFunc(int index)
{
  GipFunction result;
  switch (index)
  {
    case 0: result = GIP1; break;
    case 1: result = GIP3; break;
    //case 2: result = GIP3; break;
    default: throw "oops! - wrong function!";
  }
  return result;
}

static inline Point64 ReflectPoint(const Point64& pt, const Point64& pivot)
{
  return Point64(pivot.x + (pivot.x - pt.x), pivot.y + (pivot.y - pt.y));
}

static inline Point64 MidPoint(const Point64& p1, const Point64& p2)
{
  Point64 result;
  result.x = (p1.x + p2.x) / 2;
  result.y = (p1.y + p2.y) / 2;
  return result;
}

static inline Point64 MakeRandomPoint(int64_t min_x, int64_t max_x, int64_t min_y, int64_t max_y)
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<int64_t> x(min_x, max_x);
  std::uniform_int_distribution<int64_t> y(min_y, max_y);
  return Point64(x(gen), y(gen));
}

/////////////////////////////////////////////////////////
// Benchmark callback functions
/////////////////////////////////////////////////////////

static void BM_GIP(benchmark::State& state)
{
  Point64 ip;
  int idx = (int)state.range(0);
  GipFunction gip_func = GetGipFunc(idx);
  for (auto _ : state)
  {
    test_iter cit = tests.cbegin();
    for (; cit != tests.cend(); ++cit)
    {
      gip_func((*cit)->pt1, (*cit)->pt2, (*cit)->pt3, (*cit)->pt4, ip);
      (*cit)->results[idx] = ip;
    }
  }
}

/////////////////////////////////////////////////////////
// Main Entry
/////////////////////////////////////////////////////////

int main(int argc, char** argv)
{

  const int participants = 2;
  //setup benchmarking ...
  benchmark::Initialize(0, nullptr);
  BENCHMARK(BM_GIP)->Args(std::vector<int64_t>{0});
  BENCHMARK(BM_GIP)->Args(std::vector<int64_t>{1});

  bool first_pass = true;
  for (int power10 = 8; power10 <= 18; power10 += 2)
  {
    int64_t max_coord = static_cast<int64_t>(pow(10, power10));
    for (int64_t i = 0; i < 10000; ++i)
    {
      Point64 ip1 = MakeRandomPoint(-max_coord, max_coord, -max_coord, max_coord);
      Point64 ip2 = MakeRandomPoint(-max_coord, max_coord, -max_coord, max_coord);
      Point64 ip = MidPoint(ip1, ip2);
      Point64 ip3 = MakeRandomPoint(-max_coord, max_coord, -max_coord, max_coord);
      Point64 ip4 = ReflectPoint(ip3, ip);
      Point64 _;
      // excluding any segments that are collinear
      if (GIP1(ip1, ip2, ip3, ip4, _))
        tests.push_back(std::make_unique<TestRecord>(participants, ip, ip1, ip2, ip3, ip4));
    }

    // only benchmark the GetIntersectPoint functions once because 
    // the size of coordinate values won't affect their performance.
    if (first_pass)
    {
      first_pass = false;
      std::cout << std::endl << SetConsoleTextColor(green_bold) <<
        "Benchmarking GetIntersectPoint performance ... " << SetConsoleTextColor(reset) << 
        std::endl << std::endl;
      benchmark::RunSpecifiedBenchmarks(benchmark::CreateDefaultDisplayReporter());

      std::cout << std::endl << std::endl << SetConsoleTextColor(green_bold) <<
        "Now comparing function accuracy ..." << SetConsoleTextColor(white_bold) << std::endl << 
        "and showing how it deteriorates when using very large coordinate ranges." <<
        SetConsoleTextColor(reset) << std::endl << 
        "Distance error is the distance between the calculated and actual intersection points." << std::endl << 
        "nb: The largest errors will occur whenever intersecting edges are very close to collinear." << std::endl;
    } 
    else
    {
      for (int i = 0; i < participants; ++i)
      {
        Point64 ip;
        GipFunction gip_func = GetGipFunc(i);
        test_iter cit = tests.cbegin();
        for (; cit != tests.cend(); ++cit)
        {
          gip_func((*cit)->pt1, (*cit)->pt2, (*cit)->pt3, (*cit)->pt4, ip);
          (*cit)->results[i] = ip;
        }
      }
    }

    double avg_dists[participants] = { 0 };
    double worst_dists[participants] = { 0 };

    std::vector<TestRecord_ptr>::const_iterator cit = tests.cbegin();
    for (; cit != tests.cend(); ++cit)
      for (int i = 0; i < participants; ++i)
      {
        double dist = Distance((*cit)->ideal, (*cit)->results[i]);
        avg_dists[i] += dist;
        if (dist > worst_dists[i])  worst_dists[i] = dist;
      }


    std::cout << std::endl << SetConsoleTextColor(cyan_bold) <<
      "Coordinate ranges between  +/- 10^" << power10 << 
      SetConsoleTextColor(reset) << std::endl;

    for (int i = 0; i < participants; ++i)
    {
      avg_dists[i] /= tests.size();
      std::cout << std::fixed << "GIP" << i << ": average distance error = " << std::setprecision(2) <<
        avg_dists[i] << "; largest dist. = " << std::setprecision(0) << worst_dists[i] << std::endl;
    }
    tests.clear();
  }
  return 0;
}
