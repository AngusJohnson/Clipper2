#include "benchmark/benchmark.h"
#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "CommonUtils.h"
#include "ClipFileLoad.h"
#include <iomanip> 
#include <cstdlib>
#include <random>

using namespace Clipper2Lib;

enum TextColor {
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
  TextColor _color;
public:
  SetConsoleTextColor(TextColor color) : _color(color) {};

  static friend std::ostream& operator<< (std::ostream& out, SetConsoleTextColor const& scc)
  {
    return out << "\x1B[" << scc._color << "m";
  }
};

//////////////////////////////////////////////////////////////////////////////////////
// Int128 - class that very minimally supports 128bit integer math
//////////////////////////////////////////////////////////////////////////////////////

class Int128
{
public:
  uint64_t lo;
  int64_t hi;

  Int128(int64_t _lo = 0)
  {
    lo = (uint64_t)_lo;
    if (_lo < 0)  hi = -1; else hi = 0;
  }

  Int128(const Int128& val) : lo(val.lo), hi(val.hi) {}

  Int128(const int64_t& _hi, const uint64_t& _lo) : lo(_lo), hi(_hi) {}

  Int128& operator = (const int64_t& val)
  {
    lo = (uint64_t)val;
    if (val < 0) hi = -1; else hi = 0;
    return *this;
  }

  bool operator == (const Int128& val) const
  {
    return (hi == val.hi && lo == val.lo);
  }

  bool operator != (const Int128& val) const
  {
    return !(*this == val);
  }

  bool operator > (const Int128& val) const
  {
    return (hi != val.hi) ? hi > val.hi: lo > val.lo;
  }

  bool operator < (const Int128& val) const
  {
    return (hi != val.hi) ? hi < val.hi : lo < val.lo;
  }

  bool operator >= (const Int128& val) const
  {
    return !(*this < val);
  }

  bool operator <= (const Int128& val) const
  {
    return !(*this > val);
  }

  bool is_zero() const
  {
    return (hi == 0 && lo == 0);
  }

  bool is_negative() const
  {
    return (hi < 0);
  }

  Int128& operator += (const Int128& rhs)
  {
    hi += rhs.hi;
    lo += rhs.lo;
    if (lo < rhs.lo) hi++;
    return *this;
  }

  Int128 operator + (const Int128& rhs) const
  {
    Int128 result(*this);
    result += rhs;
    return result;
  }

  Int128& operator -= (const Int128& rhs)
  {
    *this += -rhs;
    return *this;
  }

  Int128 operator - (const Int128& rhs) const
  {
    Int128 result(*this);
    result -= rhs;
    return result;
  }

  Int128 operator-() const //unary negation
  {
    return  (lo == 0) ? Int128(-hi, 0) : Int128(~hi, ~lo + 1);
  }

  void negate()
  {
    if (lo == 0) hi = -hi;
    else { hi = ~hi; lo = ~lo + 1; }
  }

  operator double() const
  {
    const double shift64 = 18446744073709551616.0; //2^64
    if (hi < 0)
    {
      if (lo == 0) return (double)hi * shift64;
      else return -(double)(~lo + ~hi * shift64);
    }
    else
      return (double)(lo + hi * shift64);
  }
};

static inline Int128 multiply(int64_t lhs, int64_t rhs)
{
  bool negate = (lhs < 0) != (rhs < 0);

  if (lhs < 0) lhs = -lhs;
  uint64_t int1Hi = uint64_t(lhs) >> 32;
  uint64_t int1Lo = uint64_t(lhs & 0xFFFFFFFF);

  if (rhs < 0) rhs = -rhs;
  uint64_t int2Hi = uint64_t(rhs) >> 32;
  uint64_t int2Lo = uint64_t(rhs & 0xFFFFFFFF);

  uint64_t a = int1Hi * int2Hi;
  uint64_t b = int1Lo * int2Lo;
  uint64_t c = int1Hi * int2Lo + int1Lo * int2Hi;

  Int128 result;
  result.hi = a + (c >> 32);
  result.lo = c << 32;
  result.lo += b;
  if (result.lo < b) result.hi++;
  return negate ? -result : result;
};

static int64_t divide(Int128 dividend, Int128 divisor)
{
  // this function assumes that the parameter values will 
  // generate a result that fits into a 64bit integer.
  bool negate = (divisor.hi < 0) != (dividend.hi < 0);
  if (dividend.hi < 0) dividend = -dividend;
  if (divisor.hi < 0) divisor = -divisor;
  if (divisor.lo == 0 && divisor.hi == 0)
    throw "Int128: divide by zero error";

  if (dividend == divisor) return negate ? -1 : 1;
  if (divisor > dividend) return 0;

  Int128 cntr = Int128(1);
  while (divisor.hi >= 0 && divisor <= dividend)
  {
    divisor.hi <<= 1;
    if ((int64_t)divisor.lo < 0) divisor.hi++;
    divisor.lo <<= 1;

    cntr.hi <<= 1;
    if ((int64_t)cntr.lo < 0) cntr.hi++;
    cntr.lo <<= 1;
  }
  divisor.lo >>= 1;
  if (divisor.hi & 1)
    divisor.lo |= 0x8000000000000000LL;
  divisor.hi >>= 1;

  cntr.lo >>= 1;
  if (cntr.hi & 1)
    cntr.lo |= 0x8000000000000000LL;
  cntr.hi >>= 1;

  Int128 result = Int128(0);
  while (cntr.hi != 0 || cntr.lo != 0)
  {
    if (dividend >= divisor)
    {
      dividend -= divisor;
      result.hi |= cntr.hi;
      result.lo |= cntr.lo;
    }
    divisor.lo >>= 1;
    if (divisor.hi & 1)
      divisor.lo |= 0x8000000000000000LL;
    divisor.hi >>= 1;

    cntr.lo >>= 1;
    if (cntr.hi & 1)
      cntr.lo |= 0x8000000000000000LL;
    cntr.hi >>= 1;
  }
  if (result.hi || (int64_t)result.lo < 0) 
    return negate ? INT64_MIN : INT64_MAX;
  else
    return negate ? -(int64_t)result.lo : result.lo;
}

static inline int64_t muldiv(Int128 lhs, int64_t rhs, Int128 divisor)
{
  // this function assumes that the parameter values will 
  // generate a result that fits into a 64bit integer.
  int64_t sign = (lhs.is_negative() != divisor.is_negative()) != (rhs < 0) ? -2 : 2;
  if (lhs.is_negative()) lhs.negate();
  if (divisor.is_negative()) divisor.negate();
  if (rhs < 0) rhs = -rhs;

  // if 'lhs' is very large, then 'divisor' will be very large too
  while (lhs.hi && divisor.hi)
  {
    // divide dividend and divisor by 2 ...
    lhs.lo >>= 1;
    if (lhs.hi & 1)
      lhs.lo |= 0x8000000000000000LL;
    lhs.hi >>= 1;
    divisor.lo >>= 1;
    if (divisor.hi & 1)
      divisor.lo |= 0x8000000000000000LL;
    divisor.hi >>= 1;
  }

  lhs.lo >>= 1; // divide by 2 to avoid casting a 'sign' bit
  Int128 result = multiply((int64_t)lhs.lo, rhs);
  result.hi += lhs.hi * rhs;
  return divide(result, divisor) * sign; // and multiplies by 2
};

/////////////////////////////////////////////////////////
// Several GetIntersectPoint functions for testing
/////////////////////////////////////////////////////////

const int number_of_test_functions = 4;

typedef std::function<bool(const Point64&, const Point64&,
  const Point64&, const Point64&, Point64&)> GipFunction;

// GIP_Current: This is Clipper2's current GetIntersectPoint.
// It's definitely the fastest function, but its accuracy declines 
// a little when using very large 64bit integers (eg +/-10e17).
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

// GIP_Func_F: This is mathVertexLineLineIntersection_F
// https://github.com/AngusJohnson/Clipper2/issues/317#issuecomment-1314023253
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

// GIP_F_Mod: GIP_Func_F except replaces nearbyint with static casts.
// Surprisingly, while this function is faster that GIP_Func_F here, 
// it's much slower than both GIP_Func_F and GIP_Current when using it 
// as a replacement for GetIntersectPoint() in clipper.core.h.
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

// GIP_128: GetIntersectPoint using 128bit integer precision 
// This function is the most precise, but it's also very slow.
static bool GIP_128(const Point64& ln1a, const Point64& ln1b,
  const Point64& ln2a, const Point64& ln2b, Point64& ip)
{
  int64_t dx1 = ln1b.x - ln1a.x;
  int64_t dy1 = ln1b.y - ln1a.y;
  int64_t dx2 = ln2b.x - ln2a.x;
  int64_t dy2 = ln2b.y - ln2a.y;
  Int128 det = multiply(dy1, dx2) - multiply(dy2, dx1);
  if (det.is_zero()) return false;

  Int128 t_num = multiply(ln1a.x - ln2a.x, dy2) - multiply(ln1a.y - ln2a.y, dx2);
  bool is_negative = t_num.is_negative() != det.is_negative();
  if (t_num.is_zero() || is_negative)
    ip = ln1a;
  else if (t_num.is_negative() == -t_num > -det)
    ip = ln1b;
  else
  {
    ip.x = ln1a.x + muldiv(t_num, dx1, det);
    ip.y = ln1a.y + muldiv(t_num, dy1, det);
  }
  return true;
}

static inline GipFunction GetGipFunc(int64_t index)
{
  switch (index)
  {
  case 0: return GIP_Current; 
  case 1: return GIP_Func_F;
  case 2: return GIP_F_Mod;
  case 3: return GIP_128; 
  default: throw "Invalid function!";
  }
}

static inline std::string GetGipFuncName(int64_t index)
{
  switch (index)
  {
  case 0: return "GIP_Current";
  case 1: return "GIP_Func_F "; 
  case 2: return "GIP_F_Mod  "; 
  case 3: return "GIP_128    "; 
  default: throw "Invalid function!";
  }
}

/////////////////////////////////////////////////////////
// Other miscellaneous functions
/////////////////////////////////////////////////////////

double GetSineFrom3Points(const Point64& a, const Point64& b, const Point64& c)
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

/////////////////////////////////////////////////////////
// global data storage
/////////////////////////////////////////////////////////

struct TestRecord
{
public:
  Point64 actual, pt1, pt2, pt3, pt4;
  std::vector<Point64> results;
  TestRecord(const Point64& intersect_pt,
    const Point64& p1, const Point64& p2, const Point64& p3, const Point64& p4) :
    actual(intersect_pt), pt1(p1), pt2(p2), pt3(p3), pt4(p4) {
    results.resize(number_of_test_functions);
  };
};

std::vector<TestRecord> tests;
typedef std::vector<TestRecord>::iterator test_iter;

/////////////////////////////////////////////////////////
// Benchmark callback functions
/////////////////////////////////////////////////////////

static void BM_GIP(benchmark::State& state)
{
  int64_t idx = state.range(0);
  state.SetLabel(GetGipFuncName(idx));
  GipFunction func = GetGipFunc(idx);
  for (auto _ : state)
  {
    for (test_iter test = tests.begin(); test != tests.end(); ++test)
    {
      Point64 ip;
      func(test->pt1, test->pt2, test->pt3, test->pt4, ip);
      test->results[idx] = ip;
    }
  }
}

static void CustomArguments(benchmark::internal::Benchmark* b)
{
  for (int i = 0; i < number_of_test_functions; ++i) 
    b->Args({ i });
}

/////////////////////////////////////////////////////////
// Main Entry
/////////////////////////////////////////////////////////

int main(int argc, char** argv)
{
  //setup benchmarking ...
  benchmark::Initialize(0, nullptr);
  BENCHMARK(BM_GIP)->Apply(CustomArguments);

  // the closer test segments are to collinear, the less accurate 
  // calculations will be in determining their intersection points.
  const double min_angle_degrees = 0.5;
  const double sine_min_angle = std::sin(min_angle_degrees *PI / 180.0);

  bool first_pass = true;
  for (int current_pow10 = 12; current_pow10 <= 18; ++current_pow10)
  {
    // using random coordinates that are restricted to the specified 
    // power of 10 range, create multiple TestRecords containing 
    // segment pairs that intersect at their midpoints
    int64_t max_coord = static_cast<int64_t>(pow(10, current_pow10));
    for (int64_t i = 0; i < 100000; ++i)
    {
      Point64 ip1 = MakeRandomPoint(-max_coord, max_coord);
      Point64 ip2 = MakeRandomPoint(-max_coord, max_coord);
      Point64 actual = MidPoint(ip1, ip2);
      Point64 ip3 = MakeRandomPoint(-max_coord, max_coord);
      Point64 ip4 = ReflectPoint(ip3, actual);

      // Exclude segments that are **almost** collinear.
      if (std::abs(GetSineFrom3Points(ip1, actual, ip3)) < sine_min_angle) continue;      
      // Alternatively, just exclude segments that are collinear
      //if (!CrossProduct(ip1, actual, ip3)) continue;

      tests.push_back(TestRecord(actual, ip1, ip2, ip3, ip4));
    }

    if (first_pass)
    {
      // only benchmark the GetIntersectPoint functions once because changing
      // the maximum range of coordinates won't affect function performance.
      first_pass = false;
      std::cout << std::endl << SetConsoleTextColor(green_bold) <<
        "Benchmark GetIntersectPoint performance ... " << SetConsoleTextColor(reset) <<
        std::endl << std::endl;
      benchmark::RunSpecifiedBenchmarks();

      std::cout << std::endl << std::endl << SetConsoleTextColor(green_bold) <<
        "Compare function accuracy ..." << SetConsoleTextColor(reset) << std::endl <<
        "and show how it deteriorates when using very large coordinate ranges." << std::endl <<
        "Distance error is the distance between the calculated and actual intersection points." << std::endl <<
        "(The largest errors will occur whenever the segments are close to collinear.)" << std::endl;
    }
    else
    {
      for (int i = 0; i < number_of_test_functions; ++i)
      {
        // although we're not benchmarking, we still need to collect the calculated
        // intersect points of each TestRecord for each participating function.
        // (In first_pass above, benchmark::RunSpecifiedBenchmarks() does this internally.)
        Point64 ip;
        GipFunction gip_func = GetGipFunc(i);
        for (test_iter test = tests.begin(); test != tests.end(); ++test)
        {
          gip_func(test->pt1, test->pt2, test->pt3, test->pt4, ip);
          test->results[i] = ip;
        }
      }
    }

    double avg_dists[number_of_test_functions] = { 0 };
    double worst_dists[number_of_test_functions] = { 0 };

    for (test_iter test = tests.begin(); test != tests.end(); ++test)
      for (int i = 0; i < number_of_test_functions; ++i)
      {
        double dist = Distance(test->actual, test->results[i]);
        avg_dists[i] += dist;
        if (dist > worst_dists[i])  worst_dists[i] = dist;
      }

    std::cout << std::endl << SetConsoleTextColor(cyan_bold) <<
      "Coordinate ranges between  +/-10^" << current_pow10 <<
      SetConsoleTextColor(reset) << std::endl;

    for (int i = 0; i < number_of_test_functions; ++i)
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
