/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  16 June 2022                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef svgutillib_h
#define svgutillib_h

#include <cstdlib>
#include <string>
#include "clipper2/clipper.h"
#include "clipper.svg.h"
#ifndef _WIN32
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

namespace Clipper2Lib {

  static const unsigned subj_brush_clr = 0x1800009C;
  static const unsigned subj_stroke_clr = 0xFFB3B3DA;
  static const unsigned clip_brush_clr = 0x129C0000;
  static const unsigned clip_stroke_clr = 0xCCFFA07A;
  static const unsigned solution_brush_clr = 0x4466FF66;

  inline bool FileExists(const std::string& name)
  {
    struct stat buffer;
    return (stat(name.c_str(), &buffer) == 0);
  }

  inline void SvgAddCaption(SvgWriter& svg, const std::string& caption, int x, int y)
  {
    svg.AddText(caption, 0xFF000000, 14, x, y);
  }


  //nb: SVG only supports fill rules NonZero and EvenOdd
  //    so while we can clip using Positive and Negative
  //    we can't displaying these paths accurately in SVG
  //    without (safely) changing the fill rule

  inline void SvgAddSubject(SvgWriter& svg, const PathsD& path, FillRule fillrule)
  {
    if (svg.Fill_Rule() == FillRule::Positive ||
      svg.Fill_Rule() == FillRule::Negative)
    {
      svg.AddPaths(path, false, fillrule, 0x0, subj_stroke_clr, 0.8, false);
      PathsD tmp = Union(path, svg.Fill_Rule());
      svg.AddPaths(tmp, false, fillrule, subj_brush_clr, subj_stroke_clr, 0.8, false);
    }
    else
      svg.AddPaths(path, false, fillrule, subj_brush_clr, subj_stroke_clr, 0.8, false);
  }


  inline void SvgAddSubject(SvgWriter& svg, const Paths64& paths, FillRule fillrule)
  {
    PathsD tmp = TransformPaths<double, int64_t>(paths);
    svg.AddPaths(tmp, false, fillrule, subj_brush_clr, subj_stroke_clr, 0.8, false);
  }


  inline void SvgAddOpenSubject(SvgWriter& svg,
    const PathsD& path, FillRule fillrule = FillRule::EvenOdd, bool is_joined = false)
  {
    if (is_joined)
      svg.AddPaths(path, false, fillrule, subj_brush_clr, subj_stroke_clr, 1.3, false);
    else
      svg.AddPaths(path, true, fillrule, 0x0, subj_stroke_clr, 1.3, false);
  }


  inline void SvgAddOpenSubject(SvgWriter& svg,
    const Paths64& path, FillRule fillrule = FillRule::EvenOdd, bool is_joined = false)
  {
    svg.AddPaths(TransformPaths<double, int64_t>(path),
      !is_joined, fillrule, 0x0, 0xCCB3B3DA, 1.3, false);
  }


  inline void SvgAddClip(SvgWriter& svg, const PathsD& path, FillRule fillrule)
  {
    svg.AddPaths(path, false, fillrule, clip_brush_clr, clip_stroke_clr, 0.8, false);
  }


  inline void SvgAddClip(SvgWriter& svg, const Paths64& paths, FillRule fillrule)
  {
    PathsD tmp = TransformPaths<double, int64_t>(paths);
    svg.AddPaths(tmp, false, fillrule, clip_brush_clr, clip_stroke_clr, 0.8, false);
  }


  inline void SvgAddSolution(SvgWriter& svg, const PathsD &path, FillRule fillrule, bool show_coords)
  {
    svg.AddPaths(path, false, fillrule, solution_brush_clr, 0xFF003300, 1.2, show_coords);
  }


  inline void SvgAddSolution(SvgWriter& svg, const Paths64 &path, FillRule fillrule, bool show_coords)
  {
    svg.AddPaths(TransformPaths<double, int64_t>(path),
      false, fillrule, solution_brush_clr, 0xFF003300, 1.0, show_coords);
  }


  inline void SvgAddOpenSolution(SvgWriter& svg, const PathsD& path, FillRule fillrule,
    bool show_coords, bool is_joined = false)
  {
    svg.AddPaths(path, !is_joined, fillrule, 0x0, 0xFF006600, 1.8, show_coords);
  }


  inline void SvgAddOpenSolution(SvgWriter& svg, const Paths64& path,
    FillRule fillrule, bool show_coords, bool is_joined = false)
  {
    svg.AddPaths(TransformPaths<double, int64_t>(path),
      !is_joined, fillrule, 0x0, 0xFF006600, 1.8, show_coords);
  }


  inline void SvgSaveToFile(SvgWriter& svg,
    const std::string& filename,
    int max_width = 0, int max_height = 0, int margin = 0)
  {
    if (FileExists(filename)) remove(filename.c_str());
    svg.SetCoordsStyle("Verdana", 0xFF0000AA, 7);
    svg.SaveToFile(filename, max_width, max_height, margin);
  }
}

#endif //svgutillib_h
