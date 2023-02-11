/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  11 February 2023                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.IO;
using Clipper2Lib;

namespace ClipperDemo1
{
  public class Application
  {
    ////////////////////
    public const int edgeCount      = 19;
    public const int displayWidth   = 500;
    public const int displayHeight  = 500;
    public const int rectInsetDist  = 200;
    ////////////////////

    public static void Main()
    {
      DoRandomPoly(/* true to repeat last random */);
    }

    public static void DoRandomPoly(bool loadPrevious = false)
    {
      Random rand = new();
      Paths64 sub, clp, sol;
      Rect64 rec;

      if (loadPrevious && File.Exists(".\\store.txt"))
      {
        StreamReader reader;
        try
        { reader = new StreamReader(".\\store.txt"); }
        catch { return; }
        string s = reader.ReadLine();
        sub = ClipperFileIO.PathFromStr(s);
        s = reader.ReadLine();
        clp = ClipperFileIO.PathFromStr(s);
        rec = Clipper.GetBounds(clp);
      }
      else
      {
        rec = new(
          rectInsetDist, 
          rectInsetDist, 
          displayWidth - rectInsetDist, 
          displayHeight - rectInsetDist);

        clp = new() { rec.AsPath() };
        sub = new() { MakeRandomPath(displayWidth, displayHeight, edgeCount, rand) };

        // save - useful when debugging 
        StreamWriter writer;
        try
        { writer = new StreamWriter(".\\store.txt", false); }
        catch { return; }

        foreach (Point64 pt in sub[0])
          writer.Write("{0},{1} ", pt.X, pt.Y);
        writer.Write("\r\n");
        foreach (Point64 pt in clp[0])
          writer.Write("{0},{1} ", pt.X, pt.Y);
        writer.Write("\r\n");
        writer.Close();
      }

      /////////////////////////////////////////////////
      sol = Clipper.RectClip(rec, sub);
      /////////////////////////////////////////////////

      SvgWriter svg = new (FillRule.NonZero);
      SvgUtils.AddSubject(svg, sub);
      SvgUtils.AddClip(svg, clp);

      //SvgUtils.AddSolution(svg, sol, false);
      if (sol.Count > 0)
      {
        double frac = 1.0 / sol.Count;
        double cumFrac = frac;
        foreach (Path64 path in sol)
        {
          Colors.Color32 c = Colors.Rainbow(cumFrac, 64);
          cumFrac += frac;
          Colors.Color32 c2 = new Colors.Color32(
            (c.color & 0xFFFFFF) | 0x20000000);
          svg.AddPath(path, false, c2.color, c.color, 1.2, false);
        }
      }

      svg.SaveToFile("../../../rectclip.svg", 800, 600, 20);
      ClipperFileIO.OpenFileWithDefaultApp("../../../rectclip.svg");
    }

    private static Point64 MakeRandomPt(int maxWidth, int maxHeight, Random rand)
    {
      long x = rand.Next(maxWidth);
      long y = rand.Next(maxHeight);
      return new Point64(x, y);
    }
    private static PointD MakeRandomPtD(int maxWidth, int maxHeight, Random rand)
    {
      double x = rand.Next(maxWidth);
      double y = rand.Next(maxHeight);
      return new PointD(x, y);
    }

    public static Path64 MakeRandomPath(int width, int height, int count, Random rand)
    {
      Path64 result = new(count);
      for (int i = 0; i < count; ++i)
        result.Add(MakeRandomPt(width, height, rand));
      return result;
    }

    public static PathD MakeRandomPathD(int width, int height, int count, Random rand)
    {
      PathD result = new(count);
      for (int i = 0; i < count; ++i)
        result.Add(MakeRandomPtD(width, height, rand));
      return result;
    }

  } //end Application

} //namespace
