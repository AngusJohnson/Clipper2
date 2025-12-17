/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  16 December 2025                                                *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2025                                         *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************/

using System.IO;

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
    public static void Display(PathsD solution, string filename)
    { 
      SvgWriter svg = new();
      SvgUtils.AddRCSolution(svg, solution, false);
      SvgUtils.SaveToFile(svg, filename, FillRule.NonZero, 800, 600, 20);
      ClipperFileIO.OpenFileWithDefaultApp(filename);
    }

    public enum Test { single, range, all, sample_1 }
    public static void Main()
    {
      /////////////////////////
      Test test = Test.sample_1; // Test.all; // Test.single; // Test.range; // 
      int rangeStart = 44, rangeEnd = 52;
      /////////////////////////

      const string tmpFolder = ".\\tmp";
      if (!Directory.Exists(tmpFolder)) Directory.CreateDirectory(tmpFolder);
      const string svgFolder = "..\\..\\..\\..\\..\\..\\CPP\\Examples\\Triangulation\\TriSamples\\";
      PathsD solution;
      string srcFile, dstFile;
      switch (test)
      { 
        case Test.single:
          {
            string TestFile = $"Test{rangeEnd}.svg";
            srcFile = svgFolder + TestFile;
            dstFile = tmpFolder + TestFile;

            SvgReader sr = new(srcFile);
            PathsD pp = sr.Paths;
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

              SvgReader sr = new(srcFile);
              PathsD pp = sr.Paths;
              TriangulateResult tr = Clipper.Triangulate(pp, 0, out PathsD sol);
              if (tr != TriangulateResult.success) break;
              Display(sol, dstFile);
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
              if (!File.Exists(srcFile)) break;

              SvgReader sr = new(srcFile);
              PathsD pp = sr.Paths;
              TriangulateResult tr = Clipper.Triangulate(pp, 0, out _);
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
              if (tr == TriangulateResult.noPolygons || 
                tr == TriangulateResult.pathsIntersect)
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
            SvgReader sr = new(srcFile);
            PathsD pp = sr.Paths;
            if (Clipper.Triangulate(pp, 0, out solution) == TriangulateResult.success) 
              Display(solution, tmpFolder + TestFile);
            break;
          }
      }

    }

  } //end Application
} //namespace
