/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  16 September 2022                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.IO;

namespace Clipper2Lib
{
  public static class SvgUtils
  {
    public static void AddCaption(SvgWriter svg, string caption, int x, int y)
    {
      svg.AddText(caption, x, y, 14);
    }

    public static void AddSubject(SvgWriter svg, Path64 path)
    {
      Paths64 paths = new Paths64();
      paths.Add(path);
      svg.AddClosedPaths(paths, 0x1800009C, 0xAAB3B3DA, 0.8);
    }
    public static void AddSubject(SvgWriter svg, PathD path)
    {
      PathsD paths = new PathsD();
      paths.Add(path);
      svg.AddClosedPaths(paths, 0x1800009C, 0xAAB3B3DA, 0.8);
    }

    public static void AddSubject(SvgWriter svg, Paths64 paths)
    {
      svg.AddClosedPaths(paths, 0x1800009C, 0xAAB3B3DA, 0.8);
    }
    public static void AddOpenSubject(SvgWriter svg, Paths64 paths)
    {
        svg.AddOpenPaths(paths, 0xAAB3B3DA, 0.8);
    }

    public static void AddSubject(SvgWriter svg, PathsD paths)
    {
        svg.AddClosedPaths(paths, 0x1800009C, 0xAAB3B3DA, 0.8);
    }

    public static void AddOpenSubject(SvgWriter svg, PathsD paths)
    {
        svg.AddOpenPaths(paths, 0xAAB3B3DA, 1.2);
    }

    public static void AddClip(SvgWriter svg, Path64 path)
    {
      Paths64 paths = new Paths64();
      paths.Add(path);
      svg.AddClosedPaths(paths, 0x129C0000, 0xCCFFA07A, 0.8);
    }

    public static void AddClip(SvgWriter svg, PathD path)
    {
      PathsD paths = new PathsD();
      paths.Add(path);
      svg.AddClosedPaths(paths, 0x129C0000, 0xCCFFA07A, 0.8);
    }

    public static void AddClip(SvgWriter svg, Paths64 paths)
    {
      svg.AddClosedPaths(paths, 0x129C0000, 0xCCFFA07A, 0.8);
    }

    public static void AddClip(SvgWriter svg, PathsD paths)
    {
      svg.AddClosedPaths(paths, 0x129C0000, 0xCCFFA07A, 0.8);
    }

    public static void AddSolution(SvgWriter svg, Paths64 paths,
      bool show_coords, bool is_closed = true, bool is_joined = true)
    {
        svg.AddClosedPaths(paths, 0x4080ff9C, 0xFF003300, 1.5, show_coords);
    }

    public static void AddOpenSolution(SvgWriter svg, Paths64 paths, bool show_coords)
    {
        svg.AddOpenPaths(paths, 0xFF003300, 2.2, show_coords);
    }

    public static void AddSolution(SvgWriter svg, PathsD paths, bool show_coords)
    {
        svg.AddClosedPaths(paths, 0x4080ff9C, 0xFF003300, 1.5, show_coords);
    }

    public static void AddOpenSolution(SvgWriter svg, PathsD paths, bool show_coords)
    {
        svg.AddOpenPaths(paths, 0xFF003300, 2.2, show_coords);
    }

    public static void SaveToFile(SvgWriter svg,
      string filename, FillRule fill_rule,
      int max_width = 0, int max_height = 0, int margin = 0)
    {
      if (File.Exists(filename)) File.Delete(filename);
      svg.FillRule = fill_rule;
      svg.SaveToFile(filename, max_width, max_height, margin);
    }

  }

}
