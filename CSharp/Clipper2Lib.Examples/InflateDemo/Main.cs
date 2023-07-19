/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  19 July 2023                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.IO;
using System.Reflection;
using Clipper2Lib;

namespace ClipperDemo1
{

  public class Application
  {

    public static void Main()
    {
      DoSimpleShapes();
      DoRabbit();
      DoVariableOffset();
    }

    public static void DoSimpleShapes()
    {
      //triangle offset - with large miter
      Paths64 p = new() { Clipper.MakePath(new int[] { 30, 150, 60, 350, 0, 350 }) };
      Paths64 pp = new();
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
      ClipperOffset co = new();
      co.AddPaths(p, JoinType.Square, EndType.Joined);
      p = Clipper.TranslatePaths(p, 120, 100);
      pp.AddRange(p);
      co.AddPaths(p, JoinType.Round, EndType.Joined);
      co.Execute(20, p);
      pp.AddRange(p);

      string filename = "../../../inflate.svg";
      SvgWriter svg = new();
      SvgUtils.AddSolution(svg, pp, false);
      SvgUtils.SaveToFile(svg, filename, FillRule.EvenOdd, 800, 600, 20);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public static void DoRabbit()
    {
      PathsD pd = LoadPathsFromResource("InflateDemo.rabbit.bin");

      PathsD solution = new (pd);
      while (pd.Count > 0)
      {
        // and don't forget to scale the delta offset
        pd = Clipper.InflatePaths(pd, -2.5, JoinType.Round, EndType.Polygon);
        // SimplifyPaths - is not essential but it not only 
        // speeds up the loop but it also tidies the result
        pd = Clipper.SimplifyPaths(pd, 0.2);
        solution.AddRange(pd);
      }

      string filename = "../../../rabbit.svg";
      SvgWriter svg = new ();
      SvgUtils.AddSolution(svg, solution, false);
      SvgUtils.SaveToFile(svg, filename, FillRule.EvenOdd, 450, 720, 10);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public static PathsD LoadPathsFromResource(string resourceName)
    {
      using Stream stream = Assembly.GetExecutingAssembly().
        GetManifestResourceStream(resourceName);
      if (stream == null) return new PathsD();
      using BinaryReader reader = new (stream);
      int len = reader.ReadInt32();
      PathsD result = new (len);
      for (int i = 0; i < len; i++)
      {
        int len2 = reader.ReadInt32();
        PathD p = new (len2);
        for (int j = 0; j < len2; j++)
        {
          long X = reader.ReadInt64();
          long Y = reader.ReadInt64();
          p.Add(new PointD(X, Y));
        }
        result.Add(p);
      }
      return result;
    }

    public static void DoVariableOffset()
    {
      Paths64 p = new() { Clipper.MakePath(new int[] { 0,50, 20,50, 40,50, 60,50, 80,50, 100,50 }) };
      Paths64 solution = new();
      ClipperOffset co = new();
      co.AddPaths(p, JoinType.Square, EndType.Butt);
      co.Execute(
        delegate (Path64 path, PathD path_norms, int currPt, int prevPt) 
        { return currPt* currPt + 10; } , solution);

      string filename = "../../../variable_offset.svg";
      SvgWriter svg = new();
      SvgUtils.AddOpenSubject(svg, p);
      SvgUtils.AddSolution(svg, solution, true);
      SvgUtils.SaveToFile(svg, filename, FillRule.EvenOdd, 500, 500, 60);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

  } //end Application

} //namespace
