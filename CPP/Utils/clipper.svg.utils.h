/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  16 May 2022                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef svgutillib_h
#define svgutillib_h

#include <cstdlib>
#include <string>
#include "../Clipper2Lib/clipper.h"
#include "./clipper.svg.h"
#ifndef WIN32
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

namespace Clipper2Lib {

  inline bool FileExists(const std::string& name)
  {
    struct stat buffer;
    return (stat(name.c_str(), &buffer) == 0);
  }

  inline void SvgAddCaption(SvgWriter& svg, const std::string& caption, int x, int y)
  {
    svg.AddText(caption, 0xFF000000, 14, x, y);
  }


  inline void SvgAddSubject(SvgWriter& svg, const PathsD& path)
  {
    svg.AddPaths(path, false, 0x1800009C, 0xCCB3B3DA, 0.8, false);
  }


  inline void SvgAddSubject(SvgWriter& svg, const Paths64& path)
  {
    svg.AddPaths(TransformPaths<double, int64_t>(path),
      false, 0x1800009C, 0xCCB3B3DA, 0.8, false);
  }


  inline void SvgAddOpenSubject(SvgWriter& svg,
    const PathsD& path, bool is_joined = false)
  {
    svg.AddPaths(path, !is_joined, 0x1800009C, 0xCCB3B3DA, 1.3, false);
  }


  inline void SvgAddOpenSubject(SvgWriter& svg,
    const Paths64& path, bool is_joined = false)
  {
    svg.AddPaths(TransformPaths<double, int64_t>(path),
      !is_joined, 0x0, 0xCCB3B3DA, 1.3, false);
  }


  inline void SvgAddClip(SvgWriter& svg, const PathsD& path)
  {
    svg.AddPaths(path, false, 0x129C0000, 0xCCFFA07A, 0.8, false);
  }


  inline void SvgAddClip(SvgWriter& svg, const Paths64& path)
  {
    svg.AddPaths(TransformPaths<double, int64_t>(path),
      false, 0x129C0000, 0xCCFFA07A, 0.8, false);
  }


  inline void SvgAddSolution(SvgWriter& svg, const PathsD& path, bool show_coords)
  {
    svg.AddPaths(path, false, 0xFF80ff9C, 0xFF003300, 0.8, show_coords);
  }


  inline void SvgAddSolution(SvgWriter& svg, const Paths64& path, bool show_coords)
  {
    svg.AddPaths(TransformPaths<double, int64_t>(path),
      false, 0xFF80ff9C, 0xFF003300, 0.8, show_coords);
  }


  inline void SvgAddOpenSolution(SvgWriter& svg, const PathsD& path,
    bool show_coords, bool is_joined = false)
  {
    svg.AddPaths(path, !is_joined, 0x0, 0xFF006600, 1.8, show_coords);
  }


  inline void SvgAddOpenSolution(SvgWriter& svg, const Paths64& path,
    bool show_coords, bool is_joined = false)
  {
    svg.AddPaths(TransformPaths<double, int64_t>(path),
      !is_joined, 0x0, 0xFF006600, 1.8, show_coords);
  }


  inline void SvgSaveToFile(SvgWriter& svg,
    const std::string& filename, FillRule fill_rule,
    int max_width = 0, int max_height = 0, int margin = 0)
  {
    if (FileExists(filename)) remove(filename.c_str());
    svg.fill_rule = fill_rule;
    svg.SetCoordsStyle("Verdana", 0xFF0000AA, 9);
    svg.SaveToFile(filename, max_width, max_height, margin);
  }

}

#endif //svgutillib_h
