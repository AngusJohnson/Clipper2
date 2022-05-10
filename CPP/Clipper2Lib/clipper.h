/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  2 May 2022                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This module provides a simple interface to the Clipper Library  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_H
#define CLIPPER_H

#include <cstdlib>
#include <vector>

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

  inline Path64 ScalePath(const Path64 path, double scale)
  {
    using PointType      = Path64::value_type;
    using CoordinateType = PointType::coordinate_type;
    Path64 result;
    result.reserve(path.size());
    for (const Point64 pt : path)
      result.emplace_back(point_traits<PointType>::construct(CoordinateType(point_traits<PointType>::get(pt, 0) * scale), CoordinateType(point_traits<PointType>::get(pt, 1) * scale)));
    return result;
  }

  inline Paths64 ScalePaths(const Paths64 paths, double scale)
  {
    Paths64 result;
    result.reserve(paths.size());
    for (const Path64 path : paths)
      result.push_back(ScalePath(path, scale));
    return result;
  }


  namespace details {

    static void AddPolyNodeToPaths(const PolyPath64& polytree, Paths64& paths)
    {
      if (!polytree.Path().empty())
        paths.push_back(polytree.Path());
      for (PolyPath64* child : polytree.childs)
        AddPolyNodeToPaths(*child, paths);
    }

  } //details namespace 

  inline Paths64 PolyTreeToPaths(const PolyTree64& polytree)
  {
    Paths64 result;
    details::AddPolyNodeToPaths(polytree, result);
    return result;
  }

}  //Clipper2Lib namespace

#endif  // CLIPPER_H
