/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  17 July 2022                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Collections.Generic;
using System.Reflection;
using System.IO;
using Clipper2Lib;

namespace ClipperDemo1
{
  public class Application
  {

    public static void Main()
    {
      ClipSimpleShapes();
      ClipTestPolys();
    }
    public static void ClipSimpleShapes()
    {
      Paths64 subject = new ();
      Paths64 clip = new ();
      FillRule fillrule = FillRule.NonZero;

      subject.Add(Clipper.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
      clip.Add(Clipper.MakePath(new int[] { 98, 63, 4, 68, 77, 8, 52, 100, 19, 12 }));
      Paths64 solution = Clipper.Intersect(subject, clip, fillrule);

      SimpleSvgWriter svg = new ();
      SvgUtils.AddSubject(svg, subject);
      SvgUtils.AddClip(svg, clip);
      SvgUtils.SaveToFile(svg, "..\\..\\..\\clipperA.svg", fillrule, 400, 300, 20);
      ClipperFileIO.OpenFileWithDefaultApp("..\\..\\..\\clipperA.svg");

      svg.ClearAll();
      SvgUtils.AddSubject(svg, subject);
      SvgUtils.AddClip(svg, clip);
      SvgUtils.AddSolution(svg, solution, false);
      SvgUtils.SaveToFile(svg, "..\\..\\..\\clipperB.svg", fillrule, 400, 300, 20);
      ClipperFileIO.OpenFileWithDefaultApp("..\\..\\..\\clipperB.svg");
    }

    public static void ClipTestPolys()
    {
      FillRule fillrule = FillRule.NonZero;
      Paths64 subject = LoadPathsFromResource("ConsoleDemo.subj.bin");
      Paths64 clip = LoadPathsFromResource("ConsoleDemo.clip.bin");
      Paths64 solution = Clipper.Intersect(subject, clip, fillrule);

      SimpleSvgWriter svg = new ();
      SvgUtils.AddSubject(svg, subject);
      SvgUtils.AddClip(svg, clip);
      SvgUtils.SaveToFile(svg, "..\\..\\..\\clipperC.svg", fillrule, 800, 600, 20);
      ClipperFileIO.OpenFileWithDefaultApp("..\\..\\..\\clipperC.svg");

      svg.ClearAll();
      SvgUtils.AddSubject(svg, subject);
      SvgUtils.AddClip(svg, clip);
      SvgUtils.AddSolution(svg, solution, false);
      SvgUtils.SaveToFile(svg, "..\\..\\..\\clipperD.svg", fillrule, 800, 600, 20);
      ClipperFileIO.OpenFileWithDefaultApp("..\\..\\..\\clipperD.svg");
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

  } //end Application
} //namespace
