#ifndef __COMMONUTILS_H__
#define __COMMONUTILS_H__

#include <cstdlib>
#include <random>
#include "clipper2/clipper.h"

Clipper2Lib::Path64 MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> w(0, width);
  std::uniform_int_distribution<> h(0, height);

  using namespace Clipper2Lib;
  Path64 result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Point64(w(gen), h(gen)));
  return result;
}

Clipper2Lib::PathD MakeRandomPolyD(int width, int height, unsigned vertCnt)
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> w(0, width);
  std::uniform_int_distribution<> h(0, height);

  using namespace Clipper2Lib;
  PathD result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(PointD(w(gen), h(gen)));
  return result;
}

#endif