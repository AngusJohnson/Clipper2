/*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Date      :  16 May 2022                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Diagnostics;
using Clipper2Lib;

namespace ClipperDemo1
{

  using Path64 = List<Point64>;
  using Paths64 = List<List<Point64>>;
  using PathD = List<PointD>;
  using PathsD = List<List<PointD>>;

  public class Application
  {

    const int margin = 20;
    const int displayWidth = 800, displayHeight = 600;

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------

    public static void Main()
    {
      //triangle offset - with large miter
      Paths64 p = new Paths64();
      p.Add(ClipperFunc.MakePath(new int []{ 30, 150, 60, 350, 0, 350}));
      Paths64 pp = new Paths64();
      pp.AddRange(p);

      for (int i = 0; i < 5; ++i)
      {
        //nb: the following '10' parameter greatly increases miter limit
        p = ClipperFunc.InflatePaths(p, 5, JoinType.Miter, EndType.Polygon, 10);
        pp.AddRange(p);
      }

      //rectangle offset - both squared and rounded
      p.Clear();
      p.Add(ClipperFunc.MakePath(new int[] { 100, 0, 340, 0, 340, 200, 100, 200 }));
      pp.AddRange(p);
      //nb: using the ClipperOffest class directly here to control 
      //different join types within the same offset operation
      ClipperOffset co = new ClipperOffset();
      co.AddPaths(p, JoinType.Miter, EndType.Joined);
      p = ClipperFunc.OffsetPaths(p, 120, 100);
      pp.AddRange(p);
      co.AddPaths(p, JoinType.Round, EndType.Joined);
      p = co.Execute(20);
      pp.AddRange(p);

      SimpleClipperSvgWriter svg = new SimpleClipperSvgWriter();
      SvgAddSolution(svg, pp, false);
      SvgSaveToFile(svg, "../../../inflate.svg", FillRule.EvenOdd, 800, 600, 20);
      OpenFile("../../../inflate.svg");

      // Because ClipperOffset uses integer coordinates,
      // you'll need to scale coordinates when you 
      // want/need fractional values ...
      const double scale = 100;

      p = LoadPathsFromResource("InflateDemo.rabbit.bin");
      p = ClipperFunc.ScalePaths(p, scale);                    //scale up
      pp.Clear();
      pp.AddRange(p);

      while (p.Count > 0)
      {
        //don't forget to scale the delta offset
        p = ClipperFunc.InflatePaths(p, -5 * scale, JoinType.Round, EndType.Polygon);
        //RamerDouglasPeucker - not essential but
        //speeds up the loop and also tidies up the result
        p = ClipperFunc.RamerDouglasPeucker(p, 0.25 * scale);
        pp.AddRange(p);
      }
      PathsD ppp = ClipperFunc.ScalePathsD(pp, 1/scale);       //scale back down
      svg.ClearAll();
      SvgAddSolution(svg, ppp, false);
      SvgSaveToFile(svg, "../../../rabbit2.svg", FillRule.EvenOdd, 450, 720, 10);
      OpenFile("../../../rabbit2.svg");

    } //end Main()
    //------------------------------------------------------------------------------

    internal static void OpenFile(string filename)
    {
      string path = Path.GetFullPath(filename);
      if (!File.Exists(path)) return;
      Process p = new Process();
      p.StartInfo = new ProcessStartInfo(path) { UseShellExecute = true };
      p.Start();
    }

    internal static void SvgAddCaption(SimpleClipperSvgWriter svg, string caption, int x, int y)
    {
      svg.AddText(caption, x, y, 14, 0xFF000000);
    }

    internal static void SvgAddSubject(SimpleClipperSvgWriter svg, Paths64 paths,
      bool is_closed = true, bool is_joined = true)
    {
      if (!is_closed)
        svg.AddPaths(paths, !is_joined, 0x0, 0xCCB3B3DA, 0.8, false);
      else
        svg.AddPaths(paths, false, 0x1800009C, 0xCCB3B3DA, 0.8, false);
    }

    internal static void SvgAddClip(SimpleClipperSvgWriter svg, Paths64 paths)
    {
      svg.AddPaths(paths, false, 0x129C0000, 0xCCFFA07A, 0.8, false);
    }

    internal static void SvgAddSolution(SimpleClipperSvgWriter svg, Paths64 paths,
      bool show_coords, bool is_closed = true, bool is_joined = true)
    {
      if (!is_closed)
        svg.AddPaths(paths, !is_joined, 0x0, 0xFF003300, 0.8, show_coords);
      else
        svg.AddPaths(paths, false, 0xFF80ff9C, 0xFF003300, 0.8, show_coords);
    }

    internal static void SvgAddSolution(SimpleClipperSvgWriter svg, PathsD paths,
      bool show_coords, bool is_closed = true, bool is_joined = true)
    {
      if (!is_closed)
        svg.AddPaths(paths, !is_joined, 0x0, 0xFF003300, 0.8, show_coords);
      else
        svg.AddPaths(paths, false, 0xFF80ff9C, 0xFF003300, 0.8, show_coords);
    }

    internal static void SvgSaveToFile(SimpleClipperSvgWriter svg,
      string filename, FillRule fill_rule, 
      int max_width = 0, int max_height = 0, int margin = 0)
    {
      if (File.Exists(filename)) File.Delete(filename);
      svg.FillRule = fill_rule;
      svg.SaveToFile(filename, max_width, max_height, margin);
    }

    public static Paths64 LoadPathsFromResource(string resourceName)
    {
      using Stream stream = Assembly.GetExecutingAssembly().
        GetManifestResourceStream(resourceName);
      if (stream == null) return new Paths64();
      using BinaryReader reader = new BinaryReader(stream);
      int len = reader.ReadInt32();
      Paths64 result = new Paths64(len);
      for (int i = 0; i < len; i++)
      {
        int len2 = reader.ReadInt32();
        Path64 p = new Path64(len2);
        for (int j = 0; j < len2; j++)
        {
          long X = reader.ReadInt64();
          long Y = reader.ReadInt64();
          p.Add(new Point64(X, Y));
        }
        result.Add(p);
      }
      return result;
    }
  
  } //end Application

} //namespace
