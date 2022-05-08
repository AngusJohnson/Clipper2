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
using System.IO;
using System.Reflection;
using System.Diagnostics;
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
          if (!LoadTestNum("..\\..\\..\\sample1.txt", 1, subj, subj_open,
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
            SaveClippingOp("..\\..\\random.txt", subj, null, clip,
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
            if (!LoadTestNum("..\\..\\..\\tests.txt", i, subj, subj_open,
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
              CreateDisplaySvg(string.Format("..\\..\\clipper{0}.svg", i),
               caption + ' ' + clipType.ToString() + ' ' + fillrule.ToString(),
               subj, subj_open, clip, sol, sol_open,
                fillrule, displaySolutionCoords);
            }

          } //bottom of num loop

          Console.Write("{0} tests completed successfully.\n\n", cnt);
          Console.ReadKey();
          return;
      }

      CreateDisplaySvg("..\\..\\..\\clipperA.svg", caption, subj, subj_open, clip, null, null,
        fillrule, displaySolutionCoords);
      CreateDisplaySvg("..\\..\\..\\clipperB.svg", caption, subj, subj_open, clip, sol, sol_open,
        fillrule, displaySolutionCoords);

      GC.Collect();
      GC.WaitForPendingFinalizers();

    } //end Main()
    //------------------------------------------------------------------------------

    private static Paths64 PathFromStr(string s)
    {
      if (s == null) return null;
      Path64 p = new Path64();
      Paths64 pp = new Paths64();
      int len = s.Length, i = 0, j;
      while (i < len)
      {
        bool isNeg;
        while ((int)s[i] < 33 && i < len) i++;
        if (i >= len) break;
        //get X ...
        isNeg = (int)s[i] == 45;
        if (isNeg) i++;
        if (i >= len || (int)s[i] < 48 || (int)s[i] > 57) break;
        j = i + 1;
        while (j < len && (int)s[j] > 47 && (int)s[j] < 58) j++;
        if (!long.TryParse(s[i..j], out long x)) break;
        if (isNeg) x = -x;
        //skip space or comma between X & Y ...
        i = j;
        while (i < len && ((int)s[i] == 32 || (int)s[i] == 44)) i++;
        //get Y ...
        if (i >= len) break;
        isNeg = (int)s[i] == 45;
        if (isNeg) i++;
        if (i >= len || (int)s[i] < 48 || (int)s[i] > 57) break;
        j = i + 1;
        while (j < len && (int)s[j] > 47 && (int)s[j] < 58) j++;
        if (!long.TryParse(s[i..j], out long y)) break;
        if (isNeg) y = -y;
        p.Add(new Point64(x, y));
        //skip trailing space, comma ...
        i = j;
        int nlCnt = 0;
        while (i < len && ((int)s[i] < 33 || (int)s[i] == 44))
        {
          if (i >= len) break;
          if ((int)s[i] == 10)
          {
            nlCnt++;
            if (nlCnt == 2)
            {
              if (p.Count > 2) pp.Add(p);
              p = new Path64();
            }
          }
          i++;
        }
      }
      if (p.Count > 2) pp.Add(p);
      return pp;
    }
    //------------------------------------------------------------------------------

    private static bool LoadTestNum(string filename, int num, 
      Paths64 subj, Paths64 subj_open, Paths64 clip, 
      out ClipType ct, out FillRule fillRule, out long area, out string caption)
    {
      if (subj == null) subj = new Paths64(); else subj.Clear();
      if (subj_open == null) subj_open = new Paths64(); else subj_open.Clear();
      if (clip == null) clip = new Paths64(); else clip.Clear();
      ct = ClipType.Intersection;
      fillRule = FillRule.EvenOdd;
      bool numFound = false, result = false;
      int GetIdx;
      string numstr = num.ToString();
      caption = "";
      area = 0;
      StreamReader reader = new StreamReader(filename);
      if (reader == null) return false;
      while (true)
      {
        string s = reader.ReadLine();
        if (s == null) break;

        if (s.IndexOf("CAPTION: ") == 0)
        {
          numFound = (num == 0) || (s.IndexOf(numstr) > 0);
          if (numFound) { caption = s[9..];  result = true; }
          continue;
        }

        if (!numFound) continue;

        if (s.IndexOf("CLIPTYPE: ") == 0)
        {
          if (s.IndexOf("INTERSECTION") > 0) ct = ClipType.Intersection;
          else if (s.IndexOf("UNION") > 0) ct = ClipType.Union;
          else if (s.IndexOf("DIFFERENCE") > 0) ct = ClipType.Difference;
          else ct = ClipType.Xor;
          continue;
        }

        if (s.IndexOf("FILLTYPE: ") == 0 ||
          s.IndexOf("FILLRULE: ") == 0)
        {
          if (s.IndexOf("EVENODD") > 0) fillRule = FillRule.EvenOdd;
          else fillRule = FillRule.NonZero;
          continue;
        }

        if (s.IndexOf("AREA: ") == 0)
        {
          area = long.Parse(s[6..]);
          continue;
        }

        if (s.IndexOf("SUBJECTS_OPEN") == 0) GetIdx = 2;
        else if (s.IndexOf("SUBJECTS") == 0) GetIdx = 1;
        else if (s.IndexOf("CLIPS") == 0) GetIdx = 3;
        else continue;

        while (true)
        {
          s = reader.ReadLine();
          if (s == null) break;
          Paths64 paths = PathFromStr(s); //0 or 1 path
          if (paths == null || paths.Count == 0)
          {
            if (GetIdx == 3) return result;
            else if (s.IndexOf("SUBJECTS_OPEN") == 0) GetIdx = 2;
            else if (s.IndexOf("CLIPS") == 0) GetIdx = 3;
            else return result;
            continue;
          }
          if (GetIdx == 1) subj.Add(paths[0]);
          else if (GetIdx == 2) subj_open.Add(paths[0]);
          else clip.Add(paths[0]);
        }
      }
      return result;
    }
    //-----------------------------------------------------------------------

    public static void SaveClippingOp(string filename, Paths64 subj, 
      Paths64 subj_open, Paths64 clip, ClipType ct, FillRule fillRule, bool append)
    {
      StreamWriter writer = new StreamWriter(filename, append);
      if (writer == null) return;
      writer.Write("CAPTION: 1. \r\n");
      writer.Write("CLIPTYPE: {0}\r\n", ct.ToString().ToUpper());
      writer.Write("FILLRULE: {0}\r\n", fillRule.ToString().ToUpper());
      if (subj != null && subj.Count > 0)
      {
        writer.Write("SUBJECTS\r\n");
        foreach (Path64 p in subj)
        {
          foreach (Point64 ip in p)
            writer.Write("{0},{1} ", ip.X, ip.Y);
          writer.Write("\r\n");
        }
      }
      if (subj_open != null && subj_open.Count > 0)
      {
        writer.Write("SUBJECTS_OPEN\r\n");
        foreach (Path64 p in subj_open)
        {
          foreach (Point64 ip in p)
            writer.Write("{0},{1} ", ip.X, ip.Y);
          writer.Write("\r\n");
        }
      }
      if (clip != null && clip.Count > 0)
      {
        writer.Write("CLIPS\r\n");
        foreach (Path64 p in clip)
        {
          foreach (Point64 ip in p)
            writer.Write("{0},{1} ", ip.X, ip.Y);
          writer.Write("\r\n");
        }
      }
      writer.Close();
    }
    //-----------------------------------------------------------------------

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

    static void SaveToBinFile(string filename, Paths64 paths)
    {
      FileStream filestream = new FileStream(filename, FileMode.Create);
      BinaryWriter writer = new BinaryWriter(filestream);
      if (writer == null) return;
      writer.Write(paths.Count);
      foreach (Path64 path in paths)
      {
        writer.Write(path.Count);
        foreach (Point64 pt in path)
        {
          writer.Write(pt.X);
          writer.Write(pt.Y);
        }
      }
      writer.Close();
    }
    //------------------------------------------------------------------------------

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

    public static Paths64 AffineTranslatePaths(Paths64 paths, long dx, long dy)
    {
      Paths64 result = new Paths64(paths.Count);
      foreach (Path64 path in paths)
      {
        Path64 p = new Path64(path.Count);
        foreach (Point64 pt in path)
          p.Add(new Point64(pt.X + dx, pt.Y + dy));
        result.Add(p);
      }
      return result;
    }
    //-----------------------------------------------------------------------

    internal static void OpenFile(string filename)
    {
      string path = Path.GetFullPath(filename);
      if (!File.Exists(path)) return;
      Process p = new Process();
      p.StartInfo = new ProcessStartInfo(path) { UseShellExecute = true };
      p.Start();
    }
    //-----------------------------------------------------------------------

    private static void CreateDisplaySvg(string afilename, string caption, 
      Paths64 subj, Paths64 subj_open, Paths64 clip, Paths64 sol, Paths64 sol_open, 
      FillRule fillrule,  bool displaySolutionCoords)
    {
      afilename = Path.GetFullPath(afilename);
      if (File.Exists(afilename)) File.Delete(afilename);
      SimpleClipperSvgWriter svg = new SimpleClipperSvgWriter(fillrule);
      if (caption != "")
        svg.AddText(caption, margin, margin, 14, SimpleClipperSvgWriter.navy);
      if (subj != null)
        svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 0.8);
      if (subj_open != null)
        svg.AddPaths(subj_open, true, 0, 0x66AA0000, 1.2);
      if (clip != null)
        svg.AddPaths(clip, false, 0x11996600, 0x55993300, 0.8);
      if (sol != null)
        svg.AddPaths(sol, false, 0x4000FF00, 0x80000000, 1.2, 
          displaySolutionCoords && sol.Count < 100);
      if (sol_open != null)
        svg.AddPaths(sol_open, true, 0, 0xFF00AAAA, 3.0);
      svg.SaveToFile(afilename, displayWidth, displayHeight, margin);
      OpenFile(afilename);
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
