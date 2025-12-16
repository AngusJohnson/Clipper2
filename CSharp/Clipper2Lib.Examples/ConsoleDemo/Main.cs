/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  16 December 2025                                                *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2025                                         *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************/

#if USINGZ
using Clipper2ZLib;
#else
using Clipper2Lib;
#endif
using System;

namespace ConsoleDemo
{
  public static class Application
  {
    public static void Main()
    {
      SimpleIntersect();
      SimpleInflate();

      //bool test_polytree = false;
      //SquaresTest(test_polytree);
      //TrianglesTest(test_polytree);
      //DiamondsTest(test_polytree);
    }

    public static Paths64 Polytree_Union(Paths64 subjects, FillRule fillrule)
    {
      // of course this function is inefficient, 
      // but it's purpose is simply to test polytrees.
      PolyTree64 polytree = new();
      Clipper.BooleanOp(ClipType.Union, subjects, null, polytree, fillrule);
      return Clipper.PolyTreeToPaths64(polytree);
    }

    public static Paths64 FillImageWithShape(Path64 shape, int skipFreq)
    {
      Paths64 result = new();
      Random rand = new();
      int h = 600, w = 800, s = 10;
      for (int i = 0; i < h / s; ++i)
      {
        for (int j = 0; j < w / s; ++j)
        {
          shape = Clipper.TranslatePath(shape, s, 0);
          if (rand.Next(skipFreq) != 0) result.Add(shape);
        }
        shape = Clipper.TranslatePath(shape, (-w / s) * s, s);
      }
      return result;
    }


    public static void SquaresTest(bool test_polytree = false)
    {
      Path64 shape = Clipper.MakePath(new int[] { 0, 0, 10, 0, 10, 10, 0, 10 });
      Paths64 subjects = FillImageWithShape(shape, 5);
      Paths64 solution;
      if (test_polytree)
        solution = Polytree_Union(subjects, FillRule.NonZero);
      else
        solution = Clipper.Union(subjects, FillRule.NonZero);
      DisplayAsSvgImage("Squares", FillRule.NonZero, subjects, null, solution);
    }

    public static void TrianglesTest(bool test_polytree = false)
    {    
      Path64 tri1 = Clipper.MakePath(new int[] { 0,0,  20,0,  10,20 });
      Path64 tri2 = Clipper.MakePath(new int[] { 20,0, 10,20, 30,20 });

      Paths64 subjects = new(), solution;
      Random rand = new();
      for (int i = 0; i < 600 / 10 / 2; ++i)
      {
        for (int j = 0; j < 800 / 10 / 2; ++j)
        {
          if (rand.Next(4) != 0) subjects.Add(tri1);
          if (rand.Next(4) != 0) subjects.Add(tri2);
          tri1 = Clipper.TranslatePath(tri1, 20, 0);
          tri2 = Clipper.TranslatePath(tri2, 20, 0);

        }
        tri1 = Clipper.TranslatePath(tri1, (-800 / 10) * 10, 20);
        tri2 = Clipper.TranslatePath(tri2, (-800 / 10) * 10, 20);
      }

      if (test_polytree)
        solution = Polytree_Union(subjects, FillRule.NonZero);
      else
        solution = Clipper.Union(subjects, FillRule.NonZero);
      DisplayAsSvgImage("Triangles", FillRule.NonZero, subjects, null, solution);
    }

    public static void DiamondsTest(bool test_polytree = false)
    {
      Path64 shape = Clipper.MakePath(new int[] { 10, 0, 20, 10, 10, 20, 0, 10 });
      Paths64 subjects = FillImageWithShape(shape, 7);
      Paths64 solution;
      if (test_polytree)
        solution = Polytree_Union(subjects, FillRule.NonZero);
      else
        solution = Clipper.Union(subjects, FillRule.NonZero);
      DisplayAsSvgImage("Diamonds", FillRule.NonZero, subjects, null, solution);
    }

    public static void SimpleIntersect()
    {
      const FillRule fillrule = FillRule.NonZero;
      Paths64 subject = new() { Clipper.MakePath(new int[] { 200, 100, 20, 158, 130, 4, 130, 196, 20, 42 }) };
      Paths64 clip = new() { Clipper.MakePath(new int[] { 196, 126, 8, 136, 154, 16, 104, 200, 38, 24 }) };
      Paths64 solution = Clipper.Intersect(subject, clip, fillrule);
      DisplayAsSvgImage("Intersect", fillrule, subject, clip, solution);
    }

    public static void SimpleInflate()
    {
      const FillRule fillrule = FillRule.NonZero;
      Paths64 subject = new() { Clipper.Ellipse(new Point64(100,100), 100, 100, 5) };
      Paths64 solution = Clipper.InflatePaths(subject, 12.5, JoinType.Round, EndType.Joined);
      DisplayAsSvgImage_Open("Inflate", fillrule, subject, null, solution);
    }

    public static void DisplayAsSvgImage(string caption, FillRule fillrule,
      Paths64 subject, Paths64 clip, Paths64 solution)
    {
      string filename = @".\" + caption + ".svg";
      SvgWriter svg = new();
      SvgUtils.AddSubject(svg, subject);
      if (clip != null)
        SvgUtils.AddClip(svg, clip);
      SvgUtils.AddSolution(svg, solution, false);
      SvgUtils.SaveToFile(svg, filename, fillrule, 0, 0, 20);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public static void DisplayAsSvgImage_Open(string caption, FillRule fillrule,
      Paths64 openSubject, Paths64 clip, Paths64 solution)
    {
      string filename = @".\" + caption + ".svg";
      SvgWriter svg = new();
      SvgUtils.AddOpenSubject(svg, openSubject);
      if (clip != null)
        SvgUtils.AddClip(svg, clip);
      SvgUtils.AddSolution(svg, solution, false);
      SvgUtils.SaveToFile(svg, filename, fillrule, 0, 0, 20);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

  } //end Application
} //namespace
