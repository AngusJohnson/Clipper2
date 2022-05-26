/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  22 May 2022                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.IO;
using System.Diagnostics;

namespace Clipper2Lib
{

  using Path64 = List<Point64>;
  using PathD = List<PointD>;
  using Paths64 = List<List<Point64>>;
  using PathsD = List<List<PointD>>;

  public class ClipperFileIO
  {
    const int margin = 20;
    const int displayWidth = 800;
    const int displayHeight = 600;
    public static Paths64 PathFromStr(string s)
    {
      if (s == null) return null;
      Path64 p = new Path64();
      Paths64 pp = new Paths64();
      int len = s.Length, i = 0, j;
      while (i < len)
      {
        bool isNeg;
        while ((int) s[i] < 33 && i < len) i++;
        if (i >= len) break;
        //get X ...
        isNeg = (int) s[i] == 45;
        if (isNeg) i++;
        if (i >= len || (int) s[i] < 48 || (int) s[i] > 57) break;
        j = i + 1;
        while (j < len && (int) s[j] > 47 && (int) s[j] < 58) j++;
        if (!long.TryParse(s[i..j], out long x)) break;
        if (isNeg) x = -x;
        //skip space or comma between X & Y ...
        i = j;
        while (i < len && ((int) s[i] == 32 || (int) s[i] == 44)) i++;
        //get Y ...
        if (i >= len) break;
        isNeg = (int) s[i] == 45;
        if (isNeg) i++;
        if (i >= len || (int) s[i] < 48 || (int) s[i] > 57) break;
        j = i + 1;
        while (j < len && (int) s[j] > 47 && (int) s[j] < 58) j++;
        if (!long.TryParse(s[i..j], out long y)) break;
        if (isNeg) y = -y;
        p.Add(new Point64(x, y));
        //skip trailing space, comma ...
        i = j;
        int nlCnt = 0;
        while (i < len && ((int) s[i] < 33 || (int) s[i] == 44))
        {
          if (i >= len) break;
          if ((int) s[i] == 10)
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

    public static bool LoadTestNum(string filename, int num,
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
          if (numFound) { caption = s[9..]; result = true; }
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
          else if (s.IndexOf("POSITIVE") > 0) fillRule = FillRule.Positive;
          else if (s.IndexOf("NEGATIVE") > 0) fillRule = FillRule.Negative;
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
            writer.Write(ip.ToString());
          writer.Write("\r\n");
        }
      }
      writer.Close();
    }

    public static void SaveToBinFile(string filename, Paths64 paths)
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

    public static void OpenFile(string filename)
    {
      string path = Path.GetFullPath(filename);
      if (!File.Exists(path)) return;
      Process p = new Process();
      p.StartInfo = new ProcessStartInfo(path) { UseShellExecute = true };
      p.Start();
    }

    public static void CreateDisplaySvg(string afilename, string caption,
      Paths64 subj, Paths64 subj_open, Paths64 clip, Paths64 sol, Paths64 sol_open,
      FillRule fillrule, bool displaySolutionCoords)
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
  }
}
