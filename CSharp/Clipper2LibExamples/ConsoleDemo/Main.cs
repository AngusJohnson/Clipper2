/*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Date      :  8 May 2022                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.IO;
using Clipper2Lib;

namespace ClipperDemo1
{

  using Path64 = List<Point64>;
  using Paths64 = List<List<Point64>>;
  using PathD = List<PointD>;
  using PathsD = List<List<PointD>>;

  public class Application
  {

    const int margin = 20;
    const int displayWidth = 800, displayHeight = 600;

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------

    private enum DoCase { simple, timedRandom, 
      fromSimpleFile, fromTestFile, fromResource };

    public static void Main()
    {
      Paths64 subj = new Paths64();
      Paths64 subj_open = new Paths64();
      Paths64 clip = new Paths64();
      Paths64 sol = new Paths64();
      Paths64 sol_open = new Paths64();
      long area2;
      string caption = "";
      Random rand = new Random();

      ClipType clipType = ClipType.Intersection;
      FillRule fillrule = FillRule.EvenOdd;
      bool displaySolutionCoords = false;//true;//
      int edgeCount = 1000; //for timed random simple benchmark
      /////////////////////////////////////////////////////////////////////////
      // choose your poison here ...
      DoCase testCase = DoCase.simple;//fromResource;//timedRandom;//fromTestFile;//fromSimpleFile;//simple;//
      /////////////////////////////////////////////////////////////////////////

      switch (testCase)
      {
        case DoCase.simple:
          fillrule = FillRule.NonZero;
          subj.Add(ClipperFunc.MakePath(new int[] { 100,50, 10,79, 65,2, 65,98, 10,21 }));
          clip.Add(ClipperFunc.MakePath(new int[] { 98,63, 4,68, 77,8, 52,100, 19,12 }));
          sol = ClipperFunc.Intersect(subj, clip, fillrule);
          break;

        case DoCase.fromResource:
          subj = LoadPathsFromResource("ConsoleDemo.subj.bin");
          clip = LoadPathsFromResource("ConsoleDemo.clip.bin");
          Clipper c2 = new Clipper();
          c2.AddPaths(subj, PathType.Subject);
          c2.AddPaths(clip, PathType.Clip);
          c2.Execute(clipType, fillrule, sol);
          break;

        case DoCase.fromSimpleFile:
          if (!ClipperFileIO.LoadTestNum("..\\..\\..\\sample1.txt", 1, subj, subj_open,
            clip, out clipType, out fillrule, out area2, out caption)) return;
          Clipper c3 = new Clipper();
          c3.AddPaths(subj, PathType.Subject);
          c3.AddPaths(subj_open, PathType.Subject, true);
          c3.AddPaths(clip, PathType.Clip);
          c3.Execute(clipType, fillrule, sol, sol_open);
          break;

        case DoCase.timedRandom:

          fillrule = FillRule.NonZero;
          int loopCnt = 1;
          int edgeCnt = edgeCount;
          Stopwatch stopwatch = new Stopwatch();
          for (int i = 0; i < loopCnt; i++)
          {
            subj.Clear(); clip.Clear(); sol.Clear(); sol_open.Clear();
            subj.Add(MakeRandomPath(800, 600, edgeCnt, rand));
            clip.Add(MakeRandomPath(800, 600, edgeCnt, rand));
            ClipperFileIO.SaveClippingOp("..\\..\\random.txt", subj, null, clip,
              ClipType.Intersection, fillrule, false);

            stopwatch.Start(); /////////////////////////////////// start
            Clipper c4 = new Clipper();
            c4.AddSubject(subj);
            c4.AddClip(clip);
            c4.Execute(ClipType.Intersection, fillrule, sol);
            stopwatch.Stop(); //////////////////////////////////// stop
          }
          Console.WriteLine("\n\nExecution time was " +
            stopwatch.ElapsedMilliseconds + " millisecs.\n\n");
          Console.ReadKey();
          displaySolutionCoords = false;
          break;

        case DoCase.fromTestFile:
          int cnt = 0;
          int numStart = 101, numEnd = 120;//222;
          for (int i = numStart; i <= numEnd; i++)
          {
            if (!ClipperFileIO.LoadTestNum("..\\..\\..\\tests.txt", i, subj, subj_open,
                clip, out clipType, out fillrule, out area2, out caption)) break;
            cnt++;

            Clipper c5 = new Clipper();
            c5.AddSubject(subj);
            c5.AddOpenSubject(subj_open);
            c5.AddClip(clip);
            c5.Execute(clipType, fillrule, sol, sol_open);
            if (area2 > 0)
            {
              double area3 = ClipperFunc.Area(sol);
              double a = area3 / area2;
              if (Math.Abs(area2 - (long) area3) > 1 && (a < 0.995 || a > 1.005))
                Console.Write("Check areas in {0}, {1} != {2}!\n\n", i, area2, (long) area3);
            }

            if (numEnd >= numStart && numEnd - numStart <= 25)
            {
              ClipperFileIO.CreateDisplaySvg(string.Format("..\\..\\clipper{0}.svg", i),
               caption + ' ' + clipType.ToString() + ' ' + fillrule.ToString(),
               subj, subj_open, clip, sol, sol_open,
                fillrule, displaySolutionCoords);
            }

          } //bottom of num loop

          Console.Write("{0} tests completed successfully.\n\n", cnt);
          Console.ReadKey();
          return;
      }

      ClipperFileIO.CreateDisplaySvg("..\\..\\..\\clipperA.svg", 
        caption, subj, subj_open, clip, null, null,
        fillrule, displaySolutionCoords);
      ClipperFileIO.CreateDisplaySvg("..\\..\\..\\clipperB.svg", 
        caption, subj, subj_open, clip, sol, sol_open,
        fillrule, displaySolutionCoords);

      GC.Collect();
      GC.WaitForPendingFinalizers();

    } //end Main()
    //------------------------------------------------------------------------------
    private static Point64 MakeRandomPt(int maxWidth, int maxHeight, Random rand)
    {
      long x  = rand.Next(maxWidth);
      long y = rand.Next(maxHeight);
      return new Point64(x,y);
    }
    //---------------------------------------------------------------------

    public static Path64 MakeRandomPath(int width, int height, int count, Random rand)
    {
      rand.Next();
      Path64 result = new Path64(count);
      for (int i = 0; i < count; ++i)
        result.Add(MakeRandomPt(width, height, rand));
      return result;
    }
    //---------------------------------------------------------------------

    private static PointD MakeRandomPtD(int maxWidth, int maxHeight, Random rand)
    {
      int mw = maxWidth * 100, mh = maxHeight * 100;
      PointD pt = new PointD(rand.Next(mw) / 100.0, rand.Next(mh) / 100.0);
      return pt;
    }
    //---------------------------------------------------------------------

    public static PathD MakeRandomPathD(int width, int height, int count, Random rand)
    {
      rand.Next();
      PathD result = new PathD(count);
      for (int i = 0; i < count; ++i)
        result.Add(MakeRandomPtD(width, height, rand));
      return result;
    }
    //---------------------------------------------------------------------

    public static Paths64 LoadPathsFromResource(string resourceName)
    {
      using Stream stream = Assembly.GetExecutingAssembly().
        GetManifestResourceStream(resourceName);
      if (stream == null) return new Paths64();
      using BinaryReader reader = new BinaryReader(stream);
      int len = reader.ReadInt32();
      Paths64 result = new Paths64(len);
      for (int i = 0; i < len; i++)
      {
        int len2 = reader.ReadInt32();
        Path64 p = new Path64(len2);
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
    //-----------------------------------------------------------------------

#if USINGZ
    public static void ZFillFunc(Point64 p1, Point64 p2, Point64 p3, Point64 p4, ref Point64 p5)
    {
      p5.Z = -1;
    }
#endif

  } //end Application

} //namespace
