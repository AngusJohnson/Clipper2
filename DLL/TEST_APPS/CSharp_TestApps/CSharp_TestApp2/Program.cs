/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  14 August 2024                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Diagnostics;
using static Clipper2Dll.Clipper2DllCore;
using static Clipper2Dll.SvgWriterUtils;
using Clipper2Lib;

namespace ClipperDllDemo
{
  public class Application
  {
    private static void MakeRandomCPaths<T>(int width, int height, int count,
      Random rand, out T[] result)
    {
      T[] coords = new T[count * VERTEX_FIELD_CNT];
      int idx = 0;
      for (int i = 0; i < count; i++)
      {
        coords[idx++] = (T)Convert.ChangeType(rand.Next(width), typeof(T));
        coords[idx++] = (T)Convert.ChangeType(rand.Next(height), typeof(T));
#if USINGZ
        coords[idx++] = (T)Convert.ChangeType(0, typeof(T));
#endif
      }

      long arrayLen = count * VERTEX_FIELD_CNT + 4;
      result = new T[arrayLen];
      result[0] = (T)Convert.ChangeType(arrayLen, typeof(T));
      result[1] = (T)Convert.ChangeType(1, typeof(T));
      result[2] = (T)Convert.ChangeType(count, typeof(T));
      result[3] = (T)Convert.ChangeType(0, typeof(T));
      coords.CopyTo(result, 4);
    }

    private static void ConvertCPathsToPaths64(long[] cpaths, out Paths64 result)
    {
      if (cpaths[1] != 1) 
        throw new Exception("This function assumes cpaths contains only a single path");
      result = new Paths64();
      long pathLen = cpaths[2];
      Path64 path = new Path64((int)pathLen);
      int idx = 4;
      for (int i = 0; i < pathLen; i++)
      {
        long x = cpaths[idx++];
        long y = cpaths[idx++];
#if USINGZ
        long z = cpaths[idx++];
        path.Add(new Point64(x, y, z));
#else
        path.Add(new Point64(x, y));
#endif
      }
      result.Add(path);
    }

    public static void Main()
    {

      //string? ver = Marshal.PtrToStringAnsi(Version());
      //Console.WriteLine(ver + "\n");

      long timeMsec;
      Random rand = new();
      const int edgeCount = 2500;

      long[] cSubjects;
      MakeRandomCPaths(600, 400, edgeCount, rand, out cSubjects);
      long[] cClips;
      MakeRandomCPaths(600, 400, edgeCount, rand, out cClips);
      
      Paths64 solution;
      ConvertCPathsToPaths64(cSubjects, out Paths64 subjects);
      ConvertCPathsToPaths64(cClips, out Paths64 clips);

      //////////////////////////////////////////////////////////////////////
      // Use Dynamically Linked C++ compiled library (ie use the DLL)
      //////////////////////////////////////////////////////////////////////
      Stopwatch sw1 = Stopwatch.StartNew();
      if (BooleanOp64((int)Clipper2Dll.Clipper2DllCore.ClipType.Intersection, 
        (int)Clipper2Dll.Clipper2DllCore.FillRule.NonZero, 
        cSubjects, null, cClips, out IntPtr cSol, out IntPtr cSolOpen, false, false) != 0)
          return;

      long[]? cSolution = GetArrayFromIntPtr<long>(cSol);
      if (cSolution == null) return;
      DisposeArray64(ref cSol);
      DisposeArray64(ref cSolOpen);
      sw1.Stop();
      timeMsec = sw1.ElapsedMilliseconds;
      Console.WriteLine($"Time using DLL (C++ code): {timeMsec} ms");

      string fileName = "../../../clipper2_dll.svg";
      Clipper2Dll.SvgWriter svg = new(Clipper2Dll.Clipper2DllCore.FillRule.NonZero);
      AddSubjects(svg, cSubjects);
      AddClips(svg, cClips);
      AddSolution(svg, cSolution, false);
      svg.SaveToFile(fileName, 800, 600, 20);
      OpenFileWithDefaultApp(fileName);

      //////////////////////////////////////////////////////////////////////
      // Use Clipper2's statically linked C# compiled library
      //////////////////////////////////////////////////////////////////////

      Stopwatch sw2 = Stopwatch.StartNew();
      solution = Clipper.Intersect(subjects, clips, Clipper2Lib.FillRule.NonZero);
      sw2.Stop();
      timeMsec = sw2.ElapsedMilliseconds;
      Console.WriteLine($"Time using C# code       : {timeMsec} ms");
      //////////////////////////////////////////////////////////////////////

      //Console.WriteLine("Press any key to exit ... ");
      //Console.ReadKey();
    }

  } //end Application
} //namespace
