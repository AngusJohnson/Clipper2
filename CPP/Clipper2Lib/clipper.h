/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  26 April 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This module provides a simple interface to the Clipper Library  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_H
#define CLIPPER_H

#include <cstdlib>
#include <vector>
#include <string>

#include "clipper.core.h"
#include "clipper.engine.h"
#include "clipper.offset.h"
#include "clipper.minkowski.h"

namespace Clipper2Lib {

  inline Paths64 BooleanOp(ClipType cliptype, FillRule fillrule,
    const Paths64& subjects, const Paths64& clips)
  {
    Paths64 result;
    Clipper64 clipper;
    clipper.AddSubject(subjects);
    clipper.AddClip(clips);
    clipper.Execute(cliptype, fillrule, result);
    return result;
  }

  inline PathsD BooleanOp(ClipType cliptype, FillRule fillrule,
    const PathsD& subjects, const PathsD& clips)
  {
    PathsD result;
    ClipperD clipper;
    clipper.AddSubject(subjects);
    clipper.AddClip(clips);
    clipper.Execute(cliptype, fillrule, result);
    return result;
  }

  inline Paths64 Intersect(const Paths64& subjects, const Paths64& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Intersection, fillrule, subjects, clips);
  }
  
  inline PathsD Intersect(const PathsD& subjects, const PathsD& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Intersection, fillrule, subjects, clips);
  }

  inline Paths64 Union(const Paths64& subjects, const Paths64& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Union, fillrule, subjects, clips);
  }

  inline PathsD Union(const PathsD& subjects, const PathsD& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Union, fillrule, subjects, clips);
  }

  inline Paths64 Union(const Paths64& subjects, FillRule fillrule)
  {
    Paths64 result;
    Clipper64 clipper;
    clipper.AddSubject(subjects);
    clipper.Execute(ClipType::Union, fillrule, result);
    return result;
  }

  inline PathsD Union(const PathsD& subjects, FillRule fillrule)
  {
    PathsD result;
    ClipperD clipper;
    clipper.AddSubject(subjects);
    clipper.Execute(ClipType::Union, fillrule, result);
    return result;
  }

  inline Paths64 Difference(const Paths64& subjects, const Paths64& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Difference, fillrule, subjects, clips);
  }

  inline PathsD Difference(const PathsD& subjects, const PathsD& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Difference, fillrule, subjects, clips);
  }

  inline Paths64 Xor(const Paths64& subjects, const Paths64& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Xor, fillrule, subjects, clips);
  }

  inline PathsD Xor(const PathsD& subjects, const PathsD& clips, FillRule fillrule)
  {
    return BooleanOp(ClipType::Xor, fillrule, subjects, clips);
  }

  static Path64 MakePath(const std::string& s)
  {
    Path64 result;
    std::string::const_iterator s_iter = s.cbegin(), s_iter2;
    while (s_iter != s.cend())
    {
      int64_t y = 0, x = 0;
      bool isNeg;
      while (s_iter != s.cend() && (int64_t)(*s_iter) < 33) ++s_iter;
      if (s_iter == s.cend()) break;
  
      //get x ...
      isNeg = (int64_t)(*s_iter) == 45;
      if (isNeg) ++s_iter;
      if (s_iter == s.cend() || (int64_t)(*s_iter) < 48 || (int64_t)(*s_iter) > 57) break;
      s_iter2 = s_iter;
      while (s_iter2 != s.cend() && (int64_t)(*s_iter2) > 47 && (int64_t)(*s_iter2) < 58)
      {
        x = x * 10 + ((int64_t)(*s_iter2++) - 48);
      }
      if (s_iter2 == s.cend()) break;
      if (isNeg) x = -x;

      //skip space or comma between x & y ...
      s_iter = s_iter2;
      while (s_iter != s.cend() && 
        ((int64_t)(*s_iter) == 32 || (int64_t)(*s_iter) == 44)) ++s_iter;

      //get y ...
      if (s_iter == s.cend()) break;
      isNeg = (int64_t)(*s_iter) == 45;
      if (isNeg) ++s_iter;
      if (s_iter == s.cend() || (int64_t)(*s_iter) < 48 || (int64_t)(*s_iter) > 57) break;
      s_iter2 = s_iter;
      while (s_iter2 != s.cend() && (int64_t)(*s_iter2) > 47 && (int64_t)(*s_iter2) < 58)
      {
        y = y * 10 + ((int64_t)(*s_iter2++) - 48);
      }
      if (isNeg) y = -y;
      result.push_back(Point64(x, y));

      //skip trailing space, comma ...
      s_iter = s_iter2;
      while (s_iter != s.cend() &&
        ((int64_t)(*s_iter) < 33 || (int64_t)(*s_iter) == 44)) ++s_iter;
    }
    return result;
  }

}  //namespace

#endif  // CLIPPER_H
