#include <cstdlib>
#include <string>
#include <ctime> 
#include <iostream> 

#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"
#include "clipper2/clipper.triangulation.h"

using namespace Clipper2Lib;
using namespace std;

void System(const std::string& filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}


static PathD MakeRandomPoly(unsigned vertCnt, int width, int height)
{
  PathD result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(PointD(rand() % width, rand() % height));
  return result;
}

static PathsD MakeRandomPolys(unsigned polyCount, unsigned vertCnt, int width, int height)
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

static void DisplaySolution(const string filename, const PathsD& sol, bool multicolor)
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

enum class Test { single_test, range_test, random, sample_1, sample_2, sample_3};

int main()
{  
  //////////////////////////////////////////////////////////////////////
  Test t = Test::sample_1; //Test::random; //Test::single_test; //Test::range_test; //Test::clipper; //  
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
      SvgReader sr = SvgReader(folder + filename);
      PathsD subject = sr.paths, sol;
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
        SvgReader sr = SvgReader(folder + filename);
        PathsD subject = sr.paths, sol;
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
      //std::cout << subject << endl;
      tr = Triangulate(subject, 0, sol, doDelaunay);
      DisplaySolution("random.svg", sol, multicolor);
      break;
    }

    case Test::sample_1:
    {
      SvgReader sr = SvgReader(folder + "coral3.svg");
      PathsD subject = sr.paths;
      Triangulate(subject, 0, sol, true);
      DisplaySolution("coral3_t.svg", sol, multicolor);
      break;
    }

    case Test::sample_2:
    {
      SvgReader sr = SvgReader("rabbit.svg");
      PathsD subject = sr.paths;
      Triangulate(subject, 0, sol, true);
      DisplaySolution("rabbit_t.svg", sol, multicolor);
      break;
    }

    case Test::sample_3:
    {
      PathsD subject = { MakePathD({
        44.1,114.0, 11.9,114.0, 13.9,72.9, 0.0,59.9, 0.0,53.2, 12.2,41.0, 16.8,41.0, 10.1,37.2, 8.0,19.9,
        11.3,6.7, 20.0,1.9, 20.0,0.0, 34.6,0.0, 44.9,6.8, 48.0,29.1, 45.7,37.3, 39.2,41.0, 43.8,41.0,
        56.0,53.2, 56.0,59.9, 42.0,72.9
        }) };      
      Triangulate(subject, 0, sol, true);
      DisplaySolution("sample3.svg", sol, multicolor);
      break;
    }
  }

  //#ifdef _DEBUG
  PathD* p = new PathD();
  std::cout << "Press Enter to continue" << std::endl;
  std::string s;
  std::getline(std::cin, s);
  //#endif
  return 0;
}

