/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  12 December 2025                                                *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2025                                         *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************/

using System.Reflection;
using System.IO;
using System.Xml.Linq;
using System.Net.Security;
using System.Linq;
using static System.Formats.Asn1.AsnWriter;
using System.ComponentModel.DataAnnotations;





#if USINGZ
using Clipper2ZLib;
#else
using Clipper2Lib;
#endif
using System;

namespace TriangulationDemo
{
  public static class Application
  {


    ///////////////////////////////////////////////////////////////////////////////
    // GetPathsFromSvgFile 
    ///////////////////////////////////////////////////////////////////////////////

    const int display_width = 800, display_height = 600;
    const char space = ' ', comma = ',', decimalPoint = '.';

    public static bool SkipBlanks(ref string s)
    {
      int i = 0;
      while (i < s.Length && s[i] <= space) i++;
      if (i == s.Length) return false;
      if (i > 0) s = s.Remove(0, i);
      return true;
    }
    public static bool SkipChrPlusBlanks(ref string s)
    {
      int i = 1;
      while (i < s.Length && s[i] <= space) i++;
      if (i >= s.Length) return false;
      s = s.Remove(0, i);
      return true;
    }

    public static bool SkipOptionalComma(ref string s)
    {
      int i = 0;
      while (i < s.Length && s[i] <= space) i++;
      if (i >= s.Length) return false;
      if (s[i] == comma)
      {
        i++;
        while (i < s.Length && s[i] <= space) i++;
      }
      if (i == 0) return false;  
      s = s.Remove(0, i);
      return true;
    }

    public static bool GetValue(ref string s, out double val)
    { 
      val = 0;
      int sLen = s.Length;
      bool is_neg = (s[0] == '-');
      int i = is_neg ? 1 : 0;
      int fraction = 1;
      if (s[i] < '0' || s[i] > '9') return false;
      while (i < sLen && s[i] >= '0' && s[i] <= '9')
      {
        val = val * 10 + (s[i] - 48);
        i++;
      }
      if (i < sLen && s[i] == decimalPoint)
      {
        i++; //skip decimal
        while (i < sLen && s[i] >= '0' && s[i] <= '9')
        {
          val = val * 10 + (s[i] - 48);
          fraction *= 10;
        }
      }
      s = s.Remove(0, i);
      val /= fraction;
      if (is_neg) val = -val;
      return true;
    }

    public static PathsD GetPathsFromSvgFile(string filename)
    {
      string svg;
      PathsD result = new PathsD();
      StreamReader reader;
      try
      {
        reader = new StreamReader(filename);
        svg = reader.ReadToEnd();       
      }
      catch
      {
        return result;
      }

      int idx = svg.IndexOf("path d=\"");
      if (idx != -1)
      {
        svg = svg.Remove(0, idx + 8);
        PointD firstPt = new PointD(0, 0);
        bool stop = false;
        while (!stop)
        {
          PathD p = new PathD();
          double x = 0, y = 0;
          if (!SkipBlanks(ref svg)) break;
          if (svg[0] == 'M')
          {
            if (!SkipChrPlusBlanks(ref svg)) break;

            stop = !GetValue(ref svg, out firstPt.x) ||
              !SkipOptionalComma(ref svg) ||
              !GetValue(ref svg, out firstPt.y);

            SkipOptionalComma(ref svg);
          }
          if (stop) break;
          p.Add(firstPt);
          for (; ; )
          {
            if (!SkipBlanks(ref svg)) break;
            if (svg[0] == 'L')
              SkipChrPlusBlanks(ref svg);
            else if (svg[0] == 'M' || svg[0] == 'Z')
            {
              SkipChrPlusBlanks(ref svg);
              break;
            }
            stop = !GetValue(ref svg, out x) ||
              !SkipOptionalComma(ref svg) ||
              !GetValue(ref svg, out y);
            if (stop) break;
            p.Add(new PointD(x, y));
            SkipOptionalComma(ref svg);
          }
          if (p.Count > 2) result.Add(p);
          if (stop) break;
          firstPt = new PointD(x, y);
        }
      }
      return result;
    }

  public static void Display(PathsD solution, string filename)
  { 
    SvgWriter svg = new();
    SvgUtils.AddRCSolution(svg, solution, false);
    SvgUtils.SaveToFile(svg, filename, FillRule.NonZero, 800, 600, 20);
    ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public enum Test
    {
      single,
      range,
      all,
      sample_1
    }
    public static void Main()
    {
      /////////////////////////
      Test test = Test.sample_1; // Test.range; // Test.all; // Test.single;
      int rangeStart = 44, rangeEnd = 52;
      /////////////////////////

      const string tmpFolder = ".\\tmp";
      if (!Directory.Exists(tmpFolder)) Directory.CreateDirectory(tmpFolder);
      const string svgFolder = "..\\..\\..\\..\\..\\..\\CPP\\Examples\\Triangulation\\TriSamples\\";
      PathsD subject, solution;
      string srcFile, dstFile;
      switch (test)
      { 
        case Test.single:
          {
            string TestFile = $"Test{rangeEnd}.svg";
            srcFile = svgFolder + TestFile;
            dstFile = tmpFolder + TestFile;

            PathsD pp = GetPathsFromSvgFile(srcFile);
            if (Clipper.Triangulate(pp, 0, out solution) == TriangulateResult.success) 
            Display(solution, dstFile);

            break; 
          }
        case Test.range:
          {
            for (int i = rangeStart; i <= rangeEnd; i++)
            {
              string TestFile = $"Test{i}.svg";
              srcFile = svgFolder + TestFile;
              dstFile = tmpFolder + TestFile;
              if (!File.Exists(srcFile)) continue;

              PathsD pp = GetPathsFromSvgFile(srcFile);
              TriangulateResult tr = Clipper.Triangulate(pp, 0, out solution);
              if (tr != TriangulateResult.success) break;
              Display(solution, dstFile);
            }
            break;
          }

        case Test.all:
          {
            int i = 1, failCount = 0;
            for ( ; ; )
            {
              string TestFile = $"Test{i}.svg";
              srcFile = svgFolder + TestFile;
              dstFile = tmpFolder + TestFile;
              if (!File.Exists(srcFile)) break;

              PathsD pp = GetPathsFromSvgFile(srcFile);
              PathsD sol;
              TriangulateResult tr = Clipper.Triangulate(pp, 0, out solution);
              switch (tr)
              {
                case TriangulateResult.success:
                  Console.Write($"{i} ");
                  break;
                case TriangulateResult.fail:
                  Console.Write($"{i} (failed) ");
                  failCount++;
                  break;
              }
              if (tr == TriangulateResult.no_polygons || 
                tr == TriangulateResult.paths_intersect)
                  break;
              i++;
            }
            Console.WriteLine($"\n\n{failCount} tests failed. Press any key to exit.");
            Console.ReadKey();
            break;
          }

        case Test.sample_1:
          {
            string TestFile = "coral3.svg";
            srcFile = svgFolder + TestFile;
            subject = GetPathsFromSvgFile(srcFile);
            if (Clipper.Triangulate(subject, 0, out solution) == TriangulateResult.success) 
              Display(solution, tmpFolder + TestFile);
            break;
          }
      }

    }

  } //end Application
} //namespace
