/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  16 December 2025                                                *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2025                                         *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************/

using System.IO;
using System.Reflection;
#if USINGZ
using Clipper2ZLib;
#else
using Clipper2Lib;
#endif

namespace InflateDemo
{

  public static class Application
  {

    public static void Main()
    {
      DoRabbit();
      DoSimpleShapes();      
      DoVariableOffset();
    }

    public static void DoSimpleShapes()
    {
      SvgWriter svg = new();

      //TRIANGLE OFFSET - WITH LARGE MITER

      PathsD pp = new() { Clipper.MakePath(new double[] { 30,150, 60,350, 0,350 }) };
      PathsD solution = new();
      for (int i = 0; i < 5; ++i)
      {
        //nb: the last parameter here (10) greatly increases miter limit
        pp = Clipper.InflatePaths(pp, 5, JoinType.Miter, EndType.Polygon, 10);
        solution.AddRange(pp);
      }
      SvgUtils.AddSolution(svg, solution, false);

      // RECTANGLE OFFSET - BEVEL, SQUARED AND ROUNDED

      solution.Clear();
      solution.Add(Clipper.MakePath(new double[] { 100, 0, 340, 0, 340, 200, 100, 200 }));
      solution.Add(Clipper.TranslatePath(solution[0], 60, 50));
      solution.Add(Clipper.TranslatePath(solution[1], 60, 50));
      SvgUtils.AddOpenSubject(svg, solution);

      // nb: rather than using InflatePaths(), we have to use the 
      // ClipperOffest class directly because we want to perform
      // different join types in a single offset operation
      ClipperOffset co = new();
      // because ClipperOffset only accepts Int64 paths, scale them 
      // so the de-scaled offset result will have greater precision
      double scale = 100;
      Paths64 pp64 = Clipper.ScalePaths64(solution, scale);
      co.AddPath(pp64[0], JoinType.Bevel, EndType.Joined);
      co.AddPath(pp64[1], JoinType.Square, EndType.Joined);
      co.AddPath(pp64[2], JoinType.Round, EndType.Joined);
      co.Execute(10 * scale, pp64);
      // now de-scale the offset solution
      solution = Clipper.ScalePathsD(pp64, 1 / scale);

      const string filename = "../../../inflate.svg";
      SvgUtils.AddSolution(svg, solution, false);
      SvgUtils.AddCaption(svg, "Beveled join", 100, -17);
      SvgUtils.AddCaption(svg, "Squared join", 160, 33);
      SvgUtils.AddCaption(svg, "Rounded join", 220, 83);
      SvgUtils.SaveToFile(svg, filename, FillRule.EvenOdd, 800, 600, 40);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public static void DisplaySolutionAsSvg(string filename, PathsD solution)
    {
      SvgWriter svg = new();
      SvgUtils.AddSolution(svg, solution, false);
      SvgUtils.SaveToFile(svg, filename, FillRule.EvenOdd, 450, 720, 10);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public static void DoRabbit()
    {
      if (!File.Exists("..\\..\\..\\rabbit.svg")) return;
      SvgReader sr = new("..\\..\\..\\rabbit.svg");
      PathsD pp  = sr.Paths;
      PathsD solution = new (pp);
      while (pp.Count > 0)
      {
        pp = Clipper.InflatePaths(pp, -5, JoinType.Round, EndType.Polygon);
        // SimplifyPaths - is recommended here as it removes tiny 
        // offsetting artefacts and speeds up this while loop
        pp = Clipper.SimplifyPaths(pp, 0.25);
        solution.AddRange(pp);
      }
      DisplaySolutionAsSvg("..\\..\\..\\rabbit_inflate.svg", solution);
    }
    public static void DoVariableOffset()
    {
      Paths64 p = new() { Clipper.MakePath(new int[] { 0,50, 20,50, 40,50, 60,50, 80,50, 100,50 }) };
      Paths64 solution = new();
      ClipperOffset co = new();
      co.AddPaths(p, JoinType.Square, EndType.Butt);
      co.Execute(
        (path, path_norms, currPt, prevPt) => currPt * currPt + 10, solution);

      const string filename = "../../../variable_offset.svg";
      SvgWriter svg = new();
      SvgUtils.AddOpenSubject(svg, p);
      SvgUtils.AddSolution(svg, solution, true);
      SvgUtils.SaveToFile(svg, filename, FillRule.EvenOdd, 500, 500, 60);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

  } //end Application

} //namespace
