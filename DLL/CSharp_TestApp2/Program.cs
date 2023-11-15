/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  26 October 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Diagnostics;
using System.Runtime.InteropServices;
using Clipper2Lib;

namespace ClipperDllDemo
{
  public class Application
  {

    // Define miscellaneous functions ////////////////////////////
    public static void OpenFileWithDefaultApp(string filename)
    {
      string path = Path.GetFullPath(filename);
      if (!File.Exists(path)) return;
      Process p = new() { StartInfo = new ProcessStartInfo(path) { UseShellExecute = true } };
      p.Start();
    }

    public static Path64 MakePath(int[] arr)
    {
      int len = arr.Length / 2;
      Path64 p = new(len);
      for (int i = 0; i < len; i++)
        p.Add(new Point64(arr[i * 2], arr[i * 2 + 1]));
      return p;
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

    static Path64 GetPath64FromCPath(long[] cpaths, ref int idx)
    {
      int cnt = (int)cpaths[idx]; idx += 2;
      Path64 result = new(cnt);
      for (int i = 0; i < cnt; i++)
      {
        long x = cpaths[idx++];
        long y = cpaths[idx++];
        result.Add(new Point64(x, y));
      }
      return result;
    }

    static Paths64 GetPaths64FromCPaths(long[] cpaths)
    {
      int cnt = (int)cpaths[1], idx = 2;
      Paths64 result = new(cnt);
      for (int i = 0; i < cnt; i++)
        result.Add(GetPath64FromCPath(cpaths, ref idx));
      return result;
    }

    static long[] CreateCPaths64(Paths64 pp)
    {
      int len = pp.Count, len2 = 2;
      for (int i = 0; i < len; i++)
        if (pp[i].Count > 0)
          len2 += pp[i].Count * 2 + 2;
      long[] result = new long[len2];
      result[0] = 0;
      result[1] = len;
      int rPos = 2;
      for (int i = 0; i < len; i++)
      {
        len2 = pp[i].Count;
        if (len2 == 0) continue;
        result[rPos++] = len2;
        result[rPos++] = 0;
        for (int j = 0; j < len2; j++)
        {
          result[rPos++] = pp[i][j].X;
          result[rPos++] = pp[i][j].Y;
        }
      }
      return result;
    }

    public static long[]? GetPathsFromIntPtr(IntPtr paths)
    {
      if (paths == IntPtr.Zero) return null;
      long[] len = new long[1];
      Marshal.Copy(paths, len, 0, 1);
      long[] result = new long[len[0]];
      Marshal.Copy(paths, result, 0, (int)len[0]);
      return result; 
    }

    // Define DLL exported functions /////////////////////

    public const string clipperDll = @"..\..\..\..\Clipper2_64.dll";

    [DllImport(clipperDll, EntryPoint = "Version", CallingConvention = CallingConvention.Cdecl)]
    static extern IntPtr Version();

    [DllImport(clipperDll, EntryPoint = "BooleanOp64", CallingConvention = CallingConvention.Cdecl)]
    static extern int BooleanOp64(byte clipType, byte fillRule,
      long[] subject, long[]? subOpen, long[]? clip,
      out IntPtr solution, out IntPtr solOpen, bool preserveCollinear, bool reverseSolution);

    [DllImport(clipperDll, EntryPoint = "DisposeArray64", CallingConvention = CallingConvention.Cdecl)]
    static extern void DisposeArray64(ref IntPtr paths);

    static readonly byte None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
    static readonly byte EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;

    public static void Main()
    {

      //string? ver = Marshal.PtrToStringAnsi(Version());
      //Console.WriteLine(ver + "\n");

      long timeMsec;
      Random rand = new();

      ////////////////////////////////////////////////////////////////////////
      int edgeCount = 2500;
      ////////////////////////////////////////////////////////////////////////

      Paths64 subject = new() { MakeRandomPath(600,400, edgeCount, rand)};
      Paths64 clip = new() { MakeRandomPath(600, 400, edgeCount, rand) };

      //////////////////////////////////////////////////////////////////////
      // Use Dynamically Linked C++ compiled library (ie use the DLL)
      // NB: time will include ALL the overhead of swapping path structures 
      Stopwatch sw1 = Stopwatch.StartNew();
      long[] cSubject = CreateCPaths64(subject);
      long[] cClip = CreateCPaths64(clip);
      if (BooleanOp64(Intersection, NonZero, cSubject,
        null, cClip, out IntPtr cSol, out IntPtr cSolOpen, false, false) != 0)
          return;

      long[]? cSolution = GetPathsFromIntPtr(cSol);
      if (cSolution == null) return;
      DisposeArray64(ref cSol);
      DisposeArray64(ref cSolOpen);
      Paths64 solution = GetPaths64FromCPaths(cSolution);
      sw1.Stop();
      timeMsec = sw1.ElapsedMilliseconds;
      Console.WriteLine($"Time using DLL (C++ code): {timeMsec} ms");
      //////////////////////////////////////////////////////////////////////

      //////////////////////////////////////////////////////////////////////
      // Use Clipper2's statically linked C# compiled library
      Stopwatch sw2 = Stopwatch.StartNew();
      Clipper.Intersect(subject, clip, FillRule.NonZero);
      sw2.Stop();
      timeMsec = sw2.ElapsedMilliseconds;
      Console.WriteLine($"Time using C# code       : {timeMsec} ms");
      //////////////////////////////////////////////////////////////////////

      string fileName = "../../../clipper2_dll.svg";
      SvgWriter svg = new(FillRule.NonZero);
      SvgUtils.AddSubject(svg, subject);
      SvgUtils.AddClip(svg, clip);
      SvgUtils.AddSolution(svg, solution, false);
      svg.SaveToFile(fileName, 800, 600, 20);
      OpenFileWithDefaultApp(fileName);

      Console.WriteLine("Press any key to exit ... ");
      Console.ReadKey();
    }

  } //end Application
} //namespace
