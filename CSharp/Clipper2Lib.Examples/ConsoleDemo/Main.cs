/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  1 January 2023                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Reflection;
using System.IO;
using Clipper2Lib;
using System;

namespace ClipperDemo1
{
  public class Application
  {
    public static void Main()
    {
      //ClipTestPolys();

      //LoopThruTestPolygons();             // summary test all
      //LoopThruTestPolygons(110, 113);     // display test range
      //LoopThruTestPolygons(46);           // display one

      SquaresTest(true);
      TrianglesTest(true);
      DiamondsTest(true);
    }

    public static Paths64 Polytree_Union(Paths64 subjects, FillRule fillrule)
    {
      // of course this function is inefficient, 
      // but it's purpose is simply to test polytrees.
      PolyTree64 polytree = new PolyTree64();
      Clipper.BooleanOp(ClipType.Union, subjects, null, polytree, fillrule);
      return Clipper.PolyTreeToPaths64(polytree);
    }

    public static void SquaresTest(bool test_polytree = false)
    {
      const int size = 10;
      const int w = 800, h = 600;
      FillRule fillrule = FillRule.NonZero;

      Path64 shape = Clipper.MakePath(new int[] { 0, 0, size, 0, size, size, 0, size });
      Paths64 subjects = new(), solution;
      Random rand = new();
      for (int i = 0; i < h / size; ++i)
      {
        for (int j = 0; j < w / size; ++j)
        {
          shape = Clipper.TranslatePath(shape, size, 0);
          if (rand.Next(5) != 0) subjects.Add(shape);
        }
        shape = Clipper.TranslatePath(shape, (-w / size) * size, size);
      }

      if (test_polytree)
        solution = Polytree_Union(subjects, fillrule);
      else
        solution = Clipper.Union(subjects, fillrule);

      SvgWriter svg = new SvgWriter();
      SvgUtils.AddSubject(svg, subjects);
      SvgUtils.AddSolution(svg, solution, false);
      string filename = @"..\..\..\squares.svg";
      SvgUtils.SaveToFile(svg, filename, fillrule, w, h, 10);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public static void TrianglesTest(bool test_polytree = false)
    {
      const int size = 10;
      const int w = 800, h = 600;
      FillRule fillrule = FillRule.NonZero;

      Path64 tri1 = Clipper.MakePath(new int[] { 0,0, size * 2,0, size,size * 2 });
      Path64 tri2 = Clipper.MakePath(new int[] { size * 2, 0, size, size * 2, size*3, size*2 });

      Paths64 subjects = new(), solution;
      Random rand = new();
      for (int i = 0; i < h / size / 2; ++i)
      {
        for (int j = 0; j < w / size / 2; ++j)
        {
          if (rand.Next(4) != 0) subjects.Add(tri1);
          if (rand.Next(4) != 0) subjects.Add(tri2);
          tri1 = Clipper.TranslatePath(tri1, size * 2, 0);
          tri2 = Clipper.TranslatePath(tri2, size * 2, 0);
        }
        tri1 = Clipper.TranslatePath(tri1, (-w / size) * size, size * 2);
        tri2 = Clipper.TranslatePath(tri2, (-w / size) * size, size * 2);
      }

      if (test_polytree)
        solution = Polytree_Union(subjects, fillrule);
      else
        solution = Clipper.Union(subjects, fillrule);

      SvgWriter svg = new SvgWriter();
      SvgUtils.AddSubject(svg, subjects);
      SvgUtils.AddSolution(svg, solution, false);
      string filename = @"..\..\..\triangles.svg";
      SvgUtils.SaveToFile(svg, filename, fillrule, w, h, 10);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public static void DiamondsTest(bool test_polytree = false)
    {
      const int size = 10;
      const int w = 800, h = 600;
      FillRule fillrule = FillRule.NonZero;

      Path64 shape = Clipper.MakePath(new int[] { size, 0, size * 2, size, size, size * 2, 0, size });
      Paths64 subjects = new(), solution;
      Random rand = new();
      for (int i = 0; i < h / size / 2; ++i)
      {
        for (int j = 0; j < w / size; ++j)
        {
          if (rand.Next(7) != 0) subjects.Add(shape);
          if ((j & 1) == 0)
            shape = Clipper.TranslatePath(shape, size, size);
          else
            shape = Clipper.TranslatePath(shape, size, -size);
        }
        shape = Clipper.TranslatePath(shape, (-w / size) * size, size * 2);
      }

      if (test_polytree)
        solution = Polytree_Union(subjects, fillrule);
      else
        solution = Clipper.Union(subjects, fillrule);

      SvgWriter svg = new SvgWriter();
      SvgUtils.AddSubject(svg, subjects);
      SvgUtils.AddSolution(svg, solution, false);
      string filename = @"..\..\..\diamonds.svg";
      SvgUtils.SaveToFile(svg, filename, fillrule, w, h, 10);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public static void LoopThruTestPolygons(int start = 0, int end = 0)
    {
      Paths64 subject = new Paths64();
      Paths64 subject_open = new Paths64();
      Paths64 clip = new Paths64();
      Paths64 solution = new Paths64();
      Paths64 solution_open = new Paths64();
      ClipType ct;
      FillRule fr;
      bool do_all = (start == 0 && end == 0);
      if (do_all) { start = 1; end = 0xFFFF; }
      else if (end == 0) end = start;

      if (do_all)
        Console.WriteLine("\nCount and area differences (expected vs measured):\n");
        int test_number = start;
      for (; test_number <= end; ++test_number)
      {
        if (!ClipperFileIO.LoadTestNum(@"..\..\..\..\..\..\Tests\Polygons.txt", 
          test_number, subject, subject_open, clip, 
          out ct, out fr, out long area, out int cnt, out _)) break;
        Clipper64 c64 = new Clipper64();
        c64.AddSubject(subject);
        c64.AddOpenSubject(subject_open);
        c64.AddClip(clip);
        if (!c64.Execute(ct, fr, solution, solution_open)) return;

        if (do_all)
        {
          int measuredCnt = solution.Count;
          double measuredArea = Clipper.Area(solution);

          double count_diff = (cnt <= 0) ? 0 :
            Math.Abs( measuredCnt / (double)cnt - 1.0);

          double area_diff = area <= 0 ? 0 : Math.Abs(measuredArea / area -1.0);

          if (count_diff > 0.05)
            Console.WriteLine(string.Format("{0}: count {1} vs {2}", test_number, cnt, measuredCnt));
          if (area_diff > 0.1)
            Console.WriteLine(string.Format("{0}: area {1} vs {2}", test_number, area, measuredArea));

          // don't display when looping through every test
          continue; 
        }

        SvgWriter svg = new SvgWriter();
        SvgUtils.AddSubject(svg, subject);
        SvgUtils.AddClip(svg, clip);
        if (fr == FillRule.Negative)
          solution = Clipper.ReversePaths(solution);
        SvgUtils.AddSolution(svg, solution, false);
        SvgUtils.AddCaption(svg, test_number.ToString(), 20, 20);
        string filename = @"..\..\..\poly" + (test_number - 1).ToString() + ".svg";
        SvgUtils.SaveToFile(svg, filename, fr, 800, 600, 10);
        ClipperFileIO.OpenFileWithDefaultApp(filename);
      }

      if (do_all)
      {
        Console.WriteLine(string.Format("\ntest ended at polygon {0}.\n", test_number));
        Console.ReadKey();
      }
    }

    public static Paths64 LoadPathsFromResource(string resourceName)
    {
      using Stream stream = Assembly.GetExecutingAssembly().
        GetManifestResourceStream(resourceName);
      if (stream == null) return new Paths64();
      using BinaryReader reader = new (stream);
      int len = reader.ReadInt32();
      Paths64 result = new (len);
      for (int i = 0; i < len; i++)
      {
        int len2 = reader.ReadInt32();
        Path64 p = new (len2);
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

    public static void ClipTestPolys()
    {
      FillRule fillrule = FillRule.NonZero;
      Paths64 subject = LoadPathsFromResource("ConsoleDemo.subj.bin");
      Paths64 clip = LoadPathsFromResource("ConsoleDemo.clip.bin");
      Paths64 solution = Clipper.Intersect(subject, clip, fillrule);

      SvgWriter svg = new();
      SvgUtils.AddSubject(svg, subject);
      SvgUtils.AddClip(svg, clip);
      SvgUtils.AddSolution(svg, solution, false);
      SvgUtils.SaveToFile(svg, "..\\..\\..\\clipperD.svg", fillrule, 800, 600, 20);
      ClipperFileIO.OpenFileWithDefaultApp("..\\..\\..\\clipperD.svg");
    }


  } //end Application
} //namespace
