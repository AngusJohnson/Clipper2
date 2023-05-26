/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  11 February 2023                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.IO;
using System.Reflection.Metadata.Ecma335;
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
      // although RectClip isn't really designed for 
      // complex  self-intersecting polygons, it still 
      // handles them pretty well.      
      DoRandomPoly(/* true == repeat last random */);
    }

    public static bool SaveBackup(string filename, Paths64 sub, Paths64 clp)
    {
      StreamWriter writer;
      try
      { writer = new StreamWriter(filename, false); }
      catch { return false; }

      foreach (Point64 pt in sub[0])
        writer.Write("{0},{1} ", pt.X, pt.Y);
      writer.Write("\r\n");
      foreach (Point64 pt in clp[0])
        writer.Write("{0},{1} ", pt.X, pt.Y);
      writer.Write("\r\n");
      writer.Close();
      return true; 
    }

    public static bool RestoreBackup(string filename, out Paths64 sub, out Paths64 clp)
    {
      sub = null;
      clp = null;
      if (!File.Exists(filename)) return false;
      try
      {
        StreamReader reader = new StreamReader(filename);
        string s = reader.ReadLine();
        sub = ClipperFileIO.PathFromStr(s);
        s = reader.ReadLine();
        clp = ClipperFileIO.PathFromStr(s);
      }
      catch { return false; }
      return true;
    }

      public static void DoRandomPoly(bool loadPrevious = false)
    {
      Random rand = new();
      Paths64 sub = new Paths64(), clp = new Paths64(), sol;
      Rect64 rec = new(rectInsetDist, rectInsetDist,
        displayWidth - rectInsetDist, displayHeight - rectInsetDist);

      if (loadPrevious)
      {
        if (!RestoreBackup(".\\backup.txt", out sub, out clp)) return;
        rec = Clipper.GetBounds(clp);
      }
      else      
      {
        clp.Add(rec.AsPath());
        sub.Add(MakeRandomPath(displayWidth, displayHeight, edgeCount, rand));
        if (!SaveBackup(".\\backup.txt", sub, clp)) return;
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
          svg.AddClosedPath(path, c2.color, c.color, 1.2);
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

    public static Path64 MakeRandomPath(int width, int height, int count, Random rand)
    {
      Path64 result = new(count);
      for (int i = 0; i < count; ++i)
        result.Add(MakeRandomPt(width, height, rand));
      return result;
    }

  } //end Application

} //namespace
