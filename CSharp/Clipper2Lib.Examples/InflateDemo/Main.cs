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
using Clipper2Lib;

namespace ClipperDemo1
{

  using Path64 = List<Point64>;
  using Paths64 = List<List<Point64>>;
  using PathsD = List<List<PointD>>;

  public class Application
  {
    public static void Main()
    {
      //triangle offset - with large miter
      Paths64 p = new Paths64();
      p.Add(Clipper.MakePath(new int []{ 30, 150, 60, 350, 0, 350}));
      Paths64 pp = new Paths64();
      pp.AddRange(p);

      for (int i = 0; i < 5; ++i)
      {
        //nb: the following '10' parameter greatly increases miter limit
        p = Clipper.InflatePaths(p, 5, JoinType.Miter, EndType.Polygon, 10);
        pp.AddRange(p);
      }

      //rectangle offset - both squared and rounded
      p.Clear();
      p.Add(Clipper.MakePath(new int[] { 100, 0, 340, 0, 340, 200, 100, 200 }));
      pp.AddRange(p);
      //nb: using the ClipperOffest class directly here to control 
      //different join types within the same offset operation
      ClipperOffset co = new ClipperOffset();
      co.AddPaths(p, JoinType.Miter, EndType.Joined);
      p = Clipper.TranslatePaths(p, 120, 100);
      pp.AddRange(p);
      co.AddPaths(p, JoinType.Round, EndType.Joined);
      p = co.Execute(20);
      pp.AddRange(p);

      SimpleSvgWriter svg = new SimpleSvgWriter();
      SvgUtils.AddSolution(svg, pp, false);
      SvgUtils.SaveToFile(svg, "../../../inflate.svg", FillRule.EvenOdd, 800, 600, 20);
      ClipperFileIO.OpenFileWithDefaultApp("../../../inflate.svg");

      // Because ClipperOffset uses integer coordinates,
      // you'll need to scale coordinates when you 
      // want/need fractional values ...
      const double scale = 100;

      p = LoadPathsFromResource("InflateDemo.rabbit.bin");
      p = Clipper.ScalePaths(p, scale);                    //scale up
      pp.Clear();
      pp.AddRange(p);

      while (p.Count > 0)
      {
        //don't forget to scale the delta offset
        p = Clipper.InflatePaths(p, -2.5 * scale, JoinType.Round, EndType.Polygon);
        //RamerDouglasPeucker - not essential but not only 
        //speeds up the loop but also tidies the result
        p = Clipper.RamerDouglasPeucker(p, 0.025 * scale);
        pp.AddRange(p);
      }
      PathsD ppp = Clipper.ScalePathsD(pp, 1/scale);       //scale back down
      svg.ClearAll();
      SvgUtils.AddSolution(svg, ppp, false);
      SvgUtils.SaveToFile(svg, "../../../rabbit2.svg", FillRule.EvenOdd, 450, 720, 10);
      ClipperFileIO.OpenFileWithDefaultApp("../../../rabbit2.svg");

    } //end Main()
    //------------------------------------------------------------------------------

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
