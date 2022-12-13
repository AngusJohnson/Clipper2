/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  16 September 2022                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.IO;
using Clipper2Lib;

namespace ClipperDemo1
{
  public class Application
  {

    public static void Main()
    {
      DoRandomPoly(true);
    }

    public static void DoRandomPoly(bool makeNewPoly)
    {
      Random rand = new();
      Paths64 subjOpen, clip, solOpen;
      Rect64 rec;

      if (makeNewPoly)
      {
        ////////////////////
        const int count = 75;
        ////////////////////

        rec = new(100, 100, 700, 500);
        clip = new() { rec.AsPath() };
        subjOpen = new() { MakeRandomPath(800, 600, count, rand) };

        // save - useful when debugging 
        StreamWriter writer;
        try
        { writer = new StreamWriter(".\\store.txt", false); }
        catch { return; }
        foreach (Point64 pt in subjOpen[0])
          writer.Write("{0},{1} ", pt.X, pt.Y);
        writer.Write("\r\n");
        foreach (Point64 pt in clip[0])
          writer.Write("{0},{1} ", pt.X, pt.Y);
        writer.Write("\r\n");
        writer.Close();
      }
      else
      {
        if (!File.Exists(".\\store.txt")) return;

        StreamReader reader;
        try
        { reader = new StreamReader(".\\store.txt"); }
        catch { return; }
        string s = reader.ReadLine();
        subjOpen = ClipperFileIO.PathFromStr(s);
        s = reader.ReadLine();
        clip = ClipperFileIO.PathFromStr(s);
        rec = Clipper.GetBounds(clip);
      }

      /////////////////////////////////////////////////
      solOpen = Clipper.RectClipLines(rec, subjOpen);
      /////////////////////////////////////////////////

      SvgWriter svg = new ();
      SvgUtils.AddSubject(svg, subjOpen, false);
      SvgUtils.AddClip(svg, clip);
      SvgUtils.AddSolution(svg, solOpen, false, false, false);
      SvgUtils.SaveToFile(svg, "../../../rectclip.svg", FillRule.EvenOdd, 800, 600, 20);
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
