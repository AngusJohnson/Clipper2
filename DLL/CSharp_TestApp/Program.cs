/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  27 October 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Runtime.InteropServices;
using static ClipperDllDemo.Application;

namespace ClipperDllDemo
{
  public class Application
  {

    // Define path structures somewhat similar to those in Clipper2

    // nb: We don't need to use path structures that are similar Clipper2's 
    // structures, but it does demonstrate how to parse the CPaths64 and CPathsD 
    // structures in cipper.export.h.

    public class Point<T>
    {
      public T X;
      public T Y;
      public Point(T x, T y)
      {
        X = x;
        Y = y;
      }
      public string ToString(int precision = 0)
      {
        if (typeof(T) == typeof(long)) // ignore precision
          return $"{X},{Y} ";
        else
          return string.Format($"{{0:F{precision}}},{{1:F{precision}}} ", X, Y);
      }
    }

    public struct Rect<T>
    {
      public T left;
      public T top;
      public T right;
      public T bottom;
      public Rect(T l, T t, T r, T b)
      {
        left = l; top = t; right = r; bottom = b;
      }
    }

    public class Path<T> : List<Point<T>>
    {
      private Path() : base() { }
      public Path(int capacity = 0) : base(capacity) { }
      public Path(IEnumerable<Point<T>> path) : base(path) { }
      public string ToString(int precision = 0)
      {
        string s = "";
        foreach (Point<T> p in this)
          s = s + p.ToString(precision) + " ";
        return s;
      }
    }

    public class Paths<T> : List<Path<T>>
    {
      private Paths() : base() { }
      public Paths(int capacity = 0) : base(capacity) { }
      public Paths(IEnumerable<Path<T>> paths) : base(paths) { }
      public string ToString(int precision = 0)
      {
        string s = "";
        foreach (Path<T> p in this)
          s = s + p.ToString(precision) + "\n";
        return s;
      }
    }

    // Miscellaneous functions ////////////////////////////
    public static Path<T> MakePath<T>(T[] arr)
    {
      int len = arr.Length / 2;
      Path<T> p = new(len);
      for (int i = 0; i < len; i++)
        p.Add(new Point<T>(arr[i * 2], arr[i * 2 + 1]));
      return p;
    }
    static Path<T> GetPathFromCPath<T>(T[] cpath, ref int idx)
    {
      int cnt = Convert.ToInt32(cpath[idx]);
      idx += 2;
      Path<T> result = new(cnt);
      for (int i = 0; i < cnt; i++)
      {
        T x = cpath[idx++];
        T y = cpath[idx++];
        result.Add(new Point<T>(x, y));
      }
      return result;
    }
    static Paths<T> GetPathsFromCPaths<T>(T[] cpaths)
    {
      int cnt = Convert.ToInt32(cpaths[1]);
      int idx = 2;
      Paths<T> result = new(cnt);
      for (int i = 0; i < cnt; i++)
        result.Add(GetPathFromCPath<T>(cpaths, ref idx));
      return result;
    }
    static T[] CreateCPaths<T>(Paths<T> pp)
    {
      int len = pp.Count, len2 = 2;
      for (int i = 0; i < len; i++)
        if (pp[i].Count > 0)
          len2 += pp[i].Count * 2 + 2;
      T[] result = new T[len2];
      result[0] = (T)Convert.ChangeType(0, typeof(T));
      result[1] = (T)Convert.ChangeType(len, typeof(T));
      int rPos = 2;
      for (int i = 0; i < len; i++)
      {
        len2 = pp[i].Count;
        if (len2 == 0) continue;
        result[rPos++] = (T)Convert.ChangeType(len2, typeof(T));
        result[rPos++] = (T)Convert.ChangeType(0, typeof(T));
        for (int j = 0; j < len2; j++)
        {
          result[rPos++] = pp[i][j].X;
          result[rPos++] = pp[i][j].Y;
        }
      }
      return result;
    }

    public static void DisplayPolyPath<T>(T[] polypath, ref int idx, bool isHole, string spaceIndent)
    {
      int polyCnt = Convert.ToInt32(polypath[idx++]);
      int childCnt = Convert.ToInt32(polypath[idx++]);
      string preamble = isHole ? "Hole: " : (spaceIndent == "") ? "Polygon: " : "Nested Polygon: ";
      Console.Write(spaceIndent + preamble);
      spaceIndent += "  ";
      for (int i = 0; i < polyCnt; i++)
      {
        Point<T> pt = new Point<T>(polypath[idx++], polypath[idx++]);
        Console.Write(pt.ToString(2));
      }
      Console.Write("\n");
      for (int i = 0; i < childCnt; i++)
        DisplayPolyPath<T>(polypath, ref idx, !isHole, spaceIndent);
    }

    public static void DisplayPolytree<T>(T[] polytree)
    {
      int cnt = Convert.ToInt32(polytree[1]);
      int idx = 2;
      for (int i = 0; i < cnt; i++) 
        DisplayPolyPath<T>(polytree, ref idx, false, "");
      Console.Write("\n");
    }

    public static T[]? GetArrayFromIntPtr<T>(IntPtr paths)
    {
      if (paths == IntPtr.Zero) return null;
      if (typeof(T) == typeof(long))
      {
        long[] len = new long[1];
        Marshal.Copy(paths, len, 0, 1);
        long[] res = new long[(int)len[0]];
        Marshal.Copy(paths, res, 0, (int)len[0]);
        return res as T[];
      }
      else if (typeof(T) == typeof(double))
      {
        double[] len = new double[1];
        Marshal.Copy(paths, len, 0, 1);
        double[] res = new double[(int)len[0]];
        Marshal.Copy(paths, res, 0, (int)len[0]);
        return res as T[];
      }
      else return null;
    }
  
    // DLL exported function definitions /////////////////////

    public const string clipperDll = @"..\..\..\..\Clipper2_64.dll";

    [DllImport(clipperDll, EntryPoint = "Version", CallingConvention = CallingConvention.Cdecl)]
    static extern IntPtr Version();

    [DllImport(clipperDll, EntryPoint = "BooleanOp64", CallingConvention = CallingConvention.Cdecl)]
    static extern Int32 BooleanOp64(byte clipType, byte fillRule,
      long[] subjects, long[]? openSubs, long[]? clips,
      out IntPtr solution, out IntPtr openSol, bool preserveCollinear, bool reverseSolution);

    [DllImport(clipperDll, EntryPoint = "BooleanOpD", CallingConvention = CallingConvention.Cdecl)]
    static extern Int32 BooleanOpD(byte clipType, byte fillRule,
      double[] subjects, double[]? openSubs, double[]? clips,
      out IntPtr solution, out IntPtr openSol, Int32 precision, bool preserveCollinear, bool reverseSolution);

    // DisposeExported(): since all these functions behave identically ...
    [DllImport(clipperDll, EntryPoint = "DisposeExportedCPaths64", CallingConvention = CallingConvention.Cdecl)]
    static extern void DisposeExportedIntPtr(ref IntPtr intptr);

    [DllImport(clipperDll, EntryPoint = "BooleanOp_PolyTree64", CallingConvention = CallingConvention.Cdecl)]
    static extern Int32 BooleanOp_PolyTree64(byte cliptype,
      byte fillrule, long[] subjects, long[]? openSubs, long[]? clips,
      out IntPtr solTree, out IntPtr openSol,
      bool preserve_collinear, bool reverse_solution);

    [DllImport(clipperDll, EntryPoint = "BooleanOp_PolyTreeD", CallingConvention = CallingConvention.Cdecl)]
    static extern Int32 BooleanOp_PolyTreeD(byte cliptype,
      byte fillrule, double[] subjects, double[]? openSubs, double[]? clips,
      out IntPtr solTree, out IntPtr openSol, Int32 precision,
      bool preserve_collinear, bool reverse_solution);

    static readonly byte None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
    static readonly byte EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;


    /// Main Entry /////////////////////////////////////////////////////////////////////
    public static void Main()
    {

      //string? ver = Marshal.PtrToStringAnsi(Version());
      //Console.WriteLine(ver +"\n");

      // test BooleanOp64() ////////////////////////////////////////////////////////////
      Paths<long> subject = new() {MakePath<long>(new long[] { 0, 0, 100, 0, 100, 100, 0, 100 })};
      Paths<long> clip = new() {MakePath(new long[] { 20, 20, 120, 20, 120, 120, 20, 120 })};
      long[] cSubject = CreateCPaths<long>(subject);
      long[] cClip = CreateCPaths<long>(clip);

      int result = BooleanOp64(Intersection, NonZero, cSubject, 
        null, cClip, out IntPtr cSol, out IntPtr cSolOpen, false, false);
      if (result != 0) return;

      long[]? cSolution = GetArrayFromIntPtr<long>(cSol);
      DisposeExportedIntPtr(ref cSol);
      DisposeExportedIntPtr(ref cSolOpen);
      if (cSolution == null) return;
      Paths<long> solution = GetPathsFromCPaths<long>(cSolution);

      Console.WriteLine(solution.ToString());
      //////////////////////////////////////////////////////////////////////////////////

      // test BooleanOpD() /////////////////////////////////////////////////////////////
      Paths<double> subjectD = new() { MakePath(new double[] { 0, 0, 100, 0, 100, 100, 0, 100 }) };
      Paths<double> clipD = new() { MakePath(new double[] { 20, 20, 120, 20, 120, 120, 20, 120 }) };
      double[] cSubjectD = CreateCPaths<double>(subjectD);
      double[] cClipD = CreateCPaths<double>(clipD);

      int resultD = BooleanOpD(Intersection, NonZero, cSubjectD,
        null, cClipD, out IntPtr cSolD, out IntPtr cSolOpenD, 2, false, false);
      if (resultD != 0) return;

      double[]? cSolutionD = GetArrayFromIntPtr<double>(cSolD);
      DisposeExportedIntPtr(ref cSolD);
      DisposeExportedIntPtr(ref cSolOpenD);
      if (cSolutionD == null) return;
      Paths<double> solutionD = GetPathsFromCPaths<double>(cSolutionD);

      Console.WriteLine(solutionD.ToString(0));
      //////////////////////////////////////////////////////////////////////////////////


      // test BooleanOp_PolyTree64() ///////////////////////////////////////////////////
      Paths<long> subject3 = new(), clip3 = new();

      for (int i = 1; i < 6; ++i)
        subject3.Add(MakePath(new long[] { -i * 20, -i * 20, i * 20, -i * 20, i * 20, i * 20, -i * 20, i * 20 }));
      clip3.Add(MakePath(new long[] { -90, -120, 90, -120, 90, 120, -90, 120 }));

      long[] cSubject3 = CreateCPaths<long>(subject3);
      long[] cClip3 = CreateCPaths<long>(clip3);

      int result3 = BooleanOp_PolyTree64(Intersection, EvenOdd, cSubject3, null, cClip3,
        out IntPtr cSol_pt64, out IntPtr cSolOpen_pt64, false, false);
      if (result3 != 0) return;

      long[]? cPolyTree64 = GetArrayFromIntPtr<long>(cSol_pt64);
      DisposeExportedIntPtr(ref cSol_pt64);
      DisposeExportedIntPtr(ref cSolOpen_pt64);
      if (cPolyTree64 == null) return;

      DisplayPolytree<long>(cPolyTree64);
      //////////////////////////////////////////////////////////////////////////////////

      // test BooleanOp_PolyTreeD() ///////////////////////////////////////////////////
      Paths<double> subject4 = new(), clip4 = new();

      for (int i = 1; i < 6; ++i)
        subject4.Add(MakePath(new double[] { -i * 20, -i * 20, i * 20, -i * 20, i * 20, i * 20, -i * 20, i * 20 }));
      clip4.Add(MakePath(new double[] { -90, -120, 90, -120, 90, 120, -90, 120 }));

      double[] cSubject4 = CreateCPaths<double>(subject4);
      double[] cClip4 = CreateCPaths<double>(clip4);

      int result4 = BooleanOp_PolyTreeD(Intersection, EvenOdd, cSubject4, null, cClip4,
        out IntPtr cSol_ptD, out IntPtr cSolOpen_ptD, 2, false, false);
      if (result4 != 0) return;

      double[]? cPolyTreeD = GetArrayFromIntPtr<double>(cSol_ptD);
      DisposeExportedIntPtr(ref cSol_ptD);
      DisposeExportedIntPtr(ref cSolOpen_ptD);
      if (cPolyTreeD == null) return;

      DisplayPolytree<double>(cPolyTreeD);
      //////////////////////////////////////////////////////////////////////////////////

      Console.WriteLine("Press ENTER to exit ... ");
      Console.ReadLine();
    }

  } //end Application
} //namespace
