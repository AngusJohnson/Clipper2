/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  16 September 2022                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.IO;
using System.Diagnostics;

namespace Clipper2Lib
{

  public static class ClipperFileIO
  {
    public static Paths64 PathFromStr(string? s)
    {
      if (s == null) return new Paths64();
      Path64 p = new Path64();
      Paths64 pp = new Paths64();
      int len = s.Length, i = 0, j;
      while (i < len)
      {
        bool isNeg;
        while (s[i] < 33 && i < len) i++;
        if (i >= len) break;
        //get X ...
        isNeg = s[i] == 45;
        if (isNeg) i++;
        if (i >= len || s[i] < 48 || s[i] > 57) break;
        j = i + 1;
        while (j < len && s[j] > 47 && s[j] < 58) j++;
        if (!long.TryParse(s.Substring(i, j - i), out long x)) break;
        if (isNeg) x = -x;
        //skip space or comma between X & Y ...
        i = j;
        while (i < len && (s[i] == 32 || s[i] == 44)) i++;
        //get Y ...
        if (i >= len) break;
        isNeg = s[i] == 45;
        if (isNeg) i++;
        if (i >= len || s[i] < 48 || s[i] > 57) break;
        j = i + 1;
        while (j < len && s[j] > 47 && s[j] < 58) j++;
        if (!long.TryParse(s.Substring(i,j-i), out long y)) break;
        if (isNeg) y = -y;
        p.Add(new Point64(x, y));
        //skip trailing space, comma ...
        i = j;
        int nlCnt = 0;
        while (i < len && (s[i] < 33 || s[i] == 44))
        {
          if (i >= len) break;
          if (s[i] == 10)
          {
            nlCnt++;
            if (nlCnt == 2)
            {
              if (p.Count > 0) pp.Add(p);
              p = new Path64();
            }
          }
          i++;
        }
      }
      if (p.Count > 0) pp.Add(p);
      return pp;
    }
    //------------------------------------------------------------------------------

    public static bool LoadTestNum(string filename, int num,
      Paths64? subj, Paths64? subj_open, Paths64? clip,
      out ClipType ct, out FillRule fillRule, out long area, out int count, out string caption)
    {
      if (subj == null) subj = new Paths64(); else subj.Clear();
      if (subj_open == null) subj_open = new Paths64(); else subj_open.Clear();
      if (clip == null) clip = new Paths64(); else clip.Clear();
      ct = ClipType.Intersection;
      fillRule = FillRule.EvenOdd;
      bool result = false;
      int GetIdx;
      if (num < 1) num = 1;
      caption = "";
      area = 0;
      count = 0;
      StreamReader reader;
      try
      {
        reader = new StreamReader(filename);
      }
      catch
      {
        return false;
      }
      while (true)
      {
        string? s = reader.ReadLine();
        if (s == null) break;
        
        if (s.IndexOf("CAPTION: ", StringComparison.Ordinal) == 0)
        {
          num--;
          if (num != 0) continue;
          caption = s.Substring(9);
          result = true;
          continue;
        }

        if (num > 0) continue;

        if (s.IndexOf("CLIPTYPE: ", StringComparison.Ordinal) == 0)
        {
          if (s.IndexOf("INTERSECTION", StringComparison.Ordinal) > 0) ct = ClipType.Intersection;
          else if (s.IndexOf("UNION", StringComparison.Ordinal) > 0) ct = ClipType.Union;
          else if (s.IndexOf("DIFFERENCE", StringComparison.Ordinal) > 0) ct = ClipType.Difference;
          else ct = ClipType.Xor;
          continue;
        }

        if (s.IndexOf("FILLTYPE: ", StringComparison.Ordinal) == 0 ||
            s.IndexOf("FILLRULE: ", StringComparison.Ordinal) == 0)
        {
          if (s.IndexOf("EVENODD", StringComparison.Ordinal) > 0) fillRule = FillRule.EvenOdd;
          else if (s.IndexOf("POSITIVE", StringComparison.Ordinal) > 0) fillRule = FillRule.Positive;
          else if (s.IndexOf("NEGATIVE", StringComparison.Ordinal) > 0) fillRule = FillRule.Negative;
          else fillRule = FillRule.NonZero;
          continue;
        }

        if (s.IndexOf("SOL_AREA: ", StringComparison.Ordinal) == 0)
        {
          area = long.Parse(s.Substring(10));
          continue;
        }

        if (s.IndexOf("SOL_COUNT: ", StringComparison.Ordinal) == 0)
        {
          count = int.Parse(s.Substring(11));
          continue;
        }

        if (s.IndexOf("SUBJECTS_OPEN", StringComparison.Ordinal) == 0) GetIdx = 2;
        else if (s.IndexOf("SUBJECTS", StringComparison.Ordinal) == 0) GetIdx = 1;
        else if (s.IndexOf("CLIPS", StringComparison.Ordinal) == 0) GetIdx = 3;
        else continue;

        while (true)
        {
          s = reader.ReadLine();
          if (s == null) break;
          Paths64? paths = PathFromStr(s); //0 or 1 path
          if (paths == null || paths.Count == 0)
          {
            if (GetIdx == 3) return result;
            if (s.IndexOf("SUBJECTS_OPEN", StringComparison.Ordinal) == 0) GetIdx = 2;
            else if (s.IndexOf("CLIPS", StringComparison.Ordinal) == 0) GetIdx = 3;
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

    public static void SaveClippingOp(string filename, Paths64? subj,
      Paths64? subj_open, Paths64? clip, ClipType ct, FillRule fillRule, bool append)
    {
      StreamWriter writer;
      try
      {
        writer = new StreamWriter(filename, append);
      }
      catch
      {
        return;
      }
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
      FileStream filestream;
      try
      {
        filestream = new FileStream(filename, FileMode.Create);
      }
      catch
      {
        return;
      }
      BinaryWriter writer;
      try
      {
        writer = new BinaryWriter(filestream);
      }
      catch
      {
        return;
      }
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

    public static void OpenFileWithDefaultApp(string filename)
    {
      string path = Path.GetFullPath(filename);
      if (!File.Exists(path)) return;
      Process p = new Process() { StartInfo = new ProcessStartInfo(path) { UseShellExecute = true } };
      p.Start();
    }

  }
}
