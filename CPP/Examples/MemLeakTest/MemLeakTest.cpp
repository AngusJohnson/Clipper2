#include <cstdlib>
#include <string>
#include <chrono> 
 
#include "clipper2/clipper.h"

using namespace Clipper2Lib;

const int display_width = 800, display_height = 600;

void DoMemoryLeakTest();
Path64 MakeRandomPoly(int width, int height, unsigned vertCnt);
void System(const std::string &filename);

int main()
{  
  std::cout.imbue(std::locale(""));
  srand((unsigned)time(0));
  DoMemoryLeakTest();

  std::cout << std::endl;
//#ifdef _DEBUG
  std::string s;
  std::cout << "Press Enter to continue" << std::endl;
  std::getline(std::cin, s);
//#endif
  return 0;
}


inline Path64 MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  Path64 result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Point64(rand() % width, rand() % height));
  return result;
}

void DoMemoryLeakTest()
{
#ifdef _WIN32
  int edge_cnt = 1000;

  Paths64 subject, clip;
  subject.push_back(MakeRandomPoly(800, 600, edge_cnt));
  clip.push_back(MakeRandomPoly(800, 600, edge_cnt));

  _CrtMemState sOld {}, sNew {}, sDiff {};
  _CrtMemCheckpoint(&sOld); //take a snapshot
  {
    Paths64 solution = Intersect(subject, clip, FillRule::NonZero);
  }
  _CrtMemCheckpoint(&sNew); //take another snapshot (outside code block)
  if (_CrtMemDifference(&sDiff, &sOld, &sNew)) // check for a difference
  {
    std::cout << std::endl << "Memory leaks!" << std::endl;
    //_CrtMemDumpStatistics(&sDiff);
  }
  else
  {
    std::cout << std::endl << "No memory leaks detected :)" << std::endl << std::endl;
  }
#endif
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
