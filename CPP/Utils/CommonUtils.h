#include <cstdlib>
#include "clipper2/clipper.h"
#ifndef __COMMONUTILS_H__
#define __COMMONUTILS_H__

Clipper2Lib::Path64 MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  using namespace Clipper2Lib;
  Path64 result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Point64(std::rand() % width, std::rand() % height));
  return result;
}

#endif