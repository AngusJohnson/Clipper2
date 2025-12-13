//#define _CRTDBG_MAP_ALLOC
//#include <stdlib.h>
//#include <crtdbg.h>

#include <cstdlib>
#include <string>
#include <chrono> 
#include <cstdint>

#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.utils.h"
#include "clipper2/clipper.triangulation.h"

using namespace Clipper2Lib;
using namespace std;

///////////////////////////////////////////////////////////////////////////////
// GetPathsFromSvgFile 
///////////////////////////////////////////////////////////////////////////////

const int display_width = 800, display_height = 600;
const char space = 32, comma = ',', decimal = '.';

static bool SkipBlanks(istreambuf_iterator<char>& it, istreambuf_iterator<char>& eos)
{
  while (it != eos && *it <= space) ++it;
  return it != eos;
}

static bool FindString(istreambuf_iterator<char>& it, istreambuf_iterator<char> eos, const string& str)
{  
 // returns the iterator at the first char past the found 'str'
  size_t highI = str.length() -1;
  while (it != eos)
  {
    it = std::find(it, eos, str[0]);
    for (int i = 1; i <= highI; ++i)
    {
      if (it == eos || *(++it) != str[i]) break;
      if (i == highI) { ++it;  return true; } // found!
    }
  }
    return false;
}

bool GetValue(istreambuf_iterator<char>& it, istreambuf_iterator<char>& eos,
  bool skipTrailingComma, double& value)
{
  value = 0;
  if (!SkipBlanks(it, eos)) return false;

  bool is_neg = (*it == '-');
  if (is_neg) ++it;
  int fraction = 1;
  if (*it < '0' || *it > '9') return false;

  while (it != eos && *it >= '0' && *it <= '9')
  {
    value = value * 10 + static_cast<int64_t>(*it++) - 48;
  }
  if (it != eos && *it == decimal)
  {
    ++it; // skip decimal
    while (it != eos && *it >= '0' && *it <= '9')
    {
      value = value * 10 + static_cast<int64_t>(*it++) - 48;
      fraction *= 10;
    }
  }

  SkipBlanks(it, eos);
  if (skipTrailingComma && it != eos && *it == comma)
  {
    ++it;
    SkipBlanks(it, eos);
  }
  value /= fraction;
  if (is_neg) value = -value;
  return true;
}

PathsD GetPathsFromSvgFile(const string  svgFilename)
{
  ifstream svgStream(svgFilename);
  if (!svgStream) return PathsD();
  istreambuf_iterator<char> it(svgStream), eos;
  PathsD res;

  if (FindString(it, eos, "path d=\""))
  {
    PointD firstPt = PointD(0, 0);
    bool stop = false;
    while (!stop)
    {
      // starting a path
      PathD p;
      double x = 0, y = 0;
      if (!SkipBlanks(it, eos)) break;
      if (*it == 'M')
      {
        ++it; // skip M
        stop = !GetValue(it, eos, true, firstPt.x) || !GetValue(it, eos, true, firstPt.y);
      } 
      if (stop) break;
      p.push_back(firstPt);
      for (;;)
      {
        if (!SkipBlanks(it, eos)) break;
        if (*it == 'L') ++it; // skip L
        else if (*it == 'M' || *it == 'Z') break; // end of current path
        stop = !GetValue(it, eos, true, x) || !GetValue(it, eos, true, y);
        if (stop) break;
        p.push_back(PointD(x, y));
      }
      if (p.size() > 2) res.push_back(p);

      if (stop) break;
      else if (it != eos  && *it == 'Z') ++it; // prepare for next path
      else break;
      firstPt = PointD(x, y);
    }
  }
  svgStream.close();
  return res;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

void System(const std::string& filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}


inline PathD MakeRandomPoly(unsigned vertCnt, int width, int height)
{
  PathD result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(PointD(rand() % width, rand() % height));
  return result;
}

inline PathsD MakeRandomPolys(unsigned polyCount, unsigned vertCnt, int width, int height)
{
  PathsD result;
  result.reserve(polyCount);
  result.push_back(MakeRandomPoly(vertCnt, width, height));
  return result;
}

static string TriangulateResultAsString(TriangulateResult tr)
{
  switch (tr)
  {
    case TriangulateResult::fail: return "Fail!";
    case TriangulateResult::no_polygons: return "Paths are not polygons!";
    case TriangulateResult::paths_intersect: return "Paths intersect!";
    default: return "Success!";
  }
}

void DisplaySolution(const string filename, const PathsD& sol, bool multicolor)
{
  SvgWriter svg;
  //SvgAddSubject(svg, sub, FillRule::NonZero);
  if (multicolor)
    SvgAddRCSolution(svg, sol, FillRule::NonZero, false);
  else
    SvgAddSolution(svg, sol, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400, 0);
  System(filename);
}

///////////////////////////////////////////////////////////////////////////////
// main 
///////////////////////////////////////////////////////////////////////////////

enum class Test { single_test, range_test, random, sample_1, sample_2};

int main()
{  
  //////////////////////////////////////////////////////////////////////
  Test t = Test::sample_1; // Test::single_test; // Test::range_test;  // Test::clipper; //  
  int minRange = 44, maxRange = 52;
  bool doDelaunay = true;
  bool multicolor = true;
  //////////////////////////////////////////////////////////////////////

  TriangulateResult tr;
  PathsD subject, sol;
  string folder = ".\\TriSamples\\";
  srand((unsigned)time(0));

  switch (t)
  {

    case Test::single_test:
    {
      string filename = "Test" + to_string(maxRange) + ".svg";
      if (!FileExists(folder + filename)) break;
      PathsD subject = GetPathsFromSvgFile(folder + filename), sol;
      //std::cout << subject << endl;
      Triangulate(subject, 0, sol, doDelaunay);
      DisplaySolution(filename, sol, multicolor);
      break;
    }

    case Test::range_test:
    {
      for (int i = minRange; i <= maxRange; ++i)
      {
        string filename = "Test" + to_string(i) + ".svg";
        if (!FileExists(folder + filename)) break;
        PathsD subject = GetPathsFromSvgFile(folder + filename), sol;
        tr = Triangulate(subject, 0, sol, doDelaunay);
        switch (tr)
        {
          case  TriangulateResult::success:
          case TriangulateResult::fail:
          {
            std::cout << i << " " << TriangulateResultAsString(tr) << endl;
            DisplaySolution(filename, sol, multicolor);
            break; // break inner case
          }
          default: break;
        }
        if (tr != TriangulateResult::success) break; // break for() loop
      }
      break; // break case Test::range_test
    }

    case Test::random:
    {
      subject = MakeRandomPolys(1, 25, 800, 600);
      // The Triangulate function below only accepts **non-intersecting** paths, so ...
      subject = Union(subject, FillRule::NonZero);
      std::cout << subject << endl;
      tr = Triangulate(subject, 0, sol, doDelaunay);
      DisplaySolution(".\\random.svg", sol, multicolor);
      break;
    }

    case Test::sample_1:
    {
      subject = GetPathsFromSvgFile(folder + "coral3.svg");
      Triangulate(subject, 0, sol, true);
      DisplaySolution(".\\coral3t.svg", sol, multicolor);
      break;
    }

    case Test::sample_2:
    {
      PathsD subject = { MakePathD({
        44.1,114.0, 11.9,114.0, 13.9,72.9, 0.0,59.9, 0.0,53.2, 12.2,41.0, 16.8,41.0, 10.1,37.2, 8.0,19.9,
        11.3,6.7, 20.0,1.9, 20.0,0.0, 34.6,0.0, 44.9,6.8, 48.0,29.1, 45.7,37.3, 39.2,41.0, 43.8,41.0,
        56.0,53.2, 56.0,59.9, 42.0,72.9
        }) };      
      Triangulate(subject, 0, sol, true);
      DisplaySolution(".\\sample2.svg", sol, multicolor);
      break;
    }
  }

  //#ifdef _DEBUG
  PathD* p = new PathD();
  if (_CrtDumpMemoryLeaks())
    std::cout << "Memory leak detected!" << std::endl;

  std::cout << "Press Enter to continue" << std::endl;

  std::string s;
  std::getline(std::cin, s);
  //#endif
  return 0;
}

