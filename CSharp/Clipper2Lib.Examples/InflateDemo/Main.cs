/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  24 September 2023                                               *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************/

using System.IO;
using System.Reflection;
using Clipper2Lib;

namespace ClipperDemo1
{

  public static class Application
  {

    public static void Main()
    {
      DoSimpleShapes();
      DoRabbit();
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
        pd = Clipper.SimplifyPaths(pd, 0.25);
        solution.AddRange(pd);
      }

      const string filename = "../../../rabbit.svg";
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
