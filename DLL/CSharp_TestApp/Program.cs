/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  26 October 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Runtime.InteropServices;

namespace ClipperDllDemo
{
  public class Application
  {

    // Define essential Clipper2 structures ///////////////////////
    public struct Point64
    {
      public long X;
      public long Y;
      public Point64(long x, long y)
      {
        X = x;
        Y = y;
      }
      public readonly override string ToString()
      {
        return $"{X},{Y} "; 
      }
    }

    public struct Rect64
    {
      public long left;
      public long top;
      public long right;
      public long bottom;
      public Rect64(long l, long t, long r, long b)
      {
        left = l;
        top = t;
        right = r;
        bottom = b;
      }
    }

    public class Path64 : List<Point64>
    {
      private Path64() : base() { }
      public Path64(int capacity = 0) : base(capacity) { }
      public Path64(IEnumerable<Point64> path) : base(path) { }
      public override string ToString()
      {
        string s = "";
        foreach (Point64 p in this)
          s = s + p.ToString() + " ";
        return s;
      }
    }

    public class Paths64 : List<Path64>
    {
      private Paths64() : base() { }
      public Paths64(int capacity = 0) : base(capacity) { }
      public Paths64(IEnumerable<Path64> paths) : base(paths) { }
      public override string ToString()
      {
        string s = "";
        foreach (Path64 p in this)
          s = s + p.ToString() + "\n";
        return s;
      }
    }

    // Define miscellaneous functions ////////////////////////////
    public static Path64 MakePath(int[] arr)
    {
      int len = arr.Length / 2;
      Path64 p = new(len);
      for (int i = 0; i < len; i++)
        p.Add(new Point64(arr[i * 2], arr[i * 2 + 1]));
      return p;
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

    public const string clipperDll = @"..\..\..\Clipper2_64.dll";

    [DllImport(clipperDll, EntryPoint = "Version", CallingConvention = CallingConvention.Cdecl)]
    static extern IntPtr Version();

    [DllImport(clipperDll, EntryPoint = "BooleanOp64", CallingConvention = CallingConvention.Cdecl)]
    static extern int BooleanOp64(byte clipType, byte fillRule,
      long[] subject, long[]? subOpen, long[]? clip,
      out IntPtr solution, out IntPtr solOpen, bool preserveCollinear, bool reverseSolution);

    [DllImport(clipperDll, EntryPoint = "DisposeExportedCPaths64", CallingConvention = CallingConvention.Cdecl)]
    static extern void DisposeExportedCPaths64(ref IntPtr paths);

    static readonly byte None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
    static readonly byte EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;

    public static void Main()
    {

      string? ver = Marshal.PtrToStringAnsi(Version());
      Console.WriteLine(ver +"\n");

      Paths64 subject = new() {MakePath(new int[] { 0, 0, 100, 0, 100, 100, 0, 100 })};
      Paths64 clip = new() {MakePath(new int[] { 20, 20, 120, 20, 120, 120, 20, 120 })};

      long[] cSubject = CreateCPaths64(subject);
      long[] cClip = CreateCPaths64(clip);

      int result = BooleanOp64(Intersection, EvenOdd, cSubject, 
        null, cClip, out IntPtr cSol, out IntPtr cSolOpen, false, false);

      if (result != 0) return;

      long[]? cSolution = GetPathsFromIntPtr(cSol);
      DisposeExportedCPaths64(ref cSol);
      DisposeExportedCPaths64(ref cSolOpen);
      if (cSolution == null) return;
      
      Paths64 solution = GetPaths64FromCPaths(cSolution);
      Console.WriteLine(solution.ToString());
      Console.ReadLine();
    }

  } //end Application
} //namespace
