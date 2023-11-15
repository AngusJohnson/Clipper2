/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  29 October 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Runtime.InteropServices;

namespace ClipperDllDemo
{

  public class Application
  {

    // CreateCPaths: The CPaths<T> structure is defined in
    // clipper.export.h and is a simple array of long[] or
    // double[] that represents a number of path contours.

    static T[]? CreateCPath<T>(T[] coords)
    {      
      int pathLen = coords.Length / 2;
      if (pathLen == 0) return null;
      int arrayLen = pathLen * 2 + 2;
      T[] result = new T[arrayLen];
      result[0] = (T)Convert.ChangeType(pathLen, typeof(T));
      result[1] = (T)Convert.ChangeType(0, typeof(T));
      coords.CopyTo(result, 2);
      return result;  
    }

    static T[] CreateCPaths<T>(List<T[]> listOfCPath)
    {
      int pathCount = listOfCPath.Count();
      int arrayLen = 2;
      foreach (T[] path in listOfCPath)
        arrayLen += path.Length;
      T[] result = new T[arrayLen];

      result[0] = (T)Convert.ChangeType(arrayLen, typeof(T));
      result[1] = (T)Convert.ChangeType(pathCount, typeof(T));

      int idx = 2;
      foreach (T[] cpath in listOfCPath)
      {
        cpath.CopyTo(result, idx);
        idx += cpath.Length;
      }
      return result;
    }

    // or create a cpaths array that contains just 1 path
    static T[] CreateCPaths<T>(T[] coords)
    {
      int pathLen = coords.Length / 2;
      int arrayLen = pathLen *2 + 2 + 2;
      T[] result = new T[arrayLen];

      result[0] = (T)Convert.ChangeType(arrayLen, typeof(T));
      result[1] = (T)Convert.ChangeType(1, typeof(T)); // 1 path

      result[2] = (T)Convert.ChangeType(pathLen, typeof(T));
      result[3] = (T)Convert.ChangeType(0, typeof(T));

      coords.CopyTo(result, 4);
      return result;
    }

    public static string XyCoordsAsString<T>(T X, T Y, int precision = 0)
    {
      if (typeof(T) == typeof(long)) // ignore precision
        return $"{X},{Y} ";
      else
        return string.Format($"{{0:F{precision}}},{{1:F{precision}}} ", X, Y);
    }

    public static void DisplayCPath<T>(T[] cpaths, ref int idx, string spaceIndent)
    {
      int vertexCnt = Convert.ToInt32(cpaths[idx]);
      idx += 2;
      for (int i = 0; i < vertexCnt; i++)
        Console.Write(spaceIndent + 
          XyCoordsAsString<T>(cpaths[idx++], cpaths[idx++], 2));
      Console.Write("\n");
    }

    public static void DisplayCPaths<T>(T[]? cpaths, string spaceIndent)
    {
      if (cpaths == null) return;
      int pathCnt = Convert.ToInt32(cpaths[1]);
      int idx = 2;
      for (int i = 0; i < pathCnt; i++)
        DisplayCPath<T>(cpaths, ref idx, spaceIndent);
    }

    // Note: The CPolyTree<T> structure defined in clipper.export.h is 
    // a simple array of T that contains any number of nested path contours.

    public static void DisplayPolyPath<T>(T[] polypath, 
      ref int idx, bool isHole, string spaceIndent, int precision)
    {
      int polyCnt = Convert.ToInt32(polypath[idx++]);
      int childCnt = Convert.ToInt32(polypath[idx++]);
      string preamble = isHole ? "Hole: " : (spaceIndent == "") ? 
        "Polygon: " : "Nested Polygon: ";
      Console.Write(spaceIndent + preamble);
      spaceIndent += "  ";
      for (int i = 0; i < polyCnt; i++)
        Console.Write(XyCoordsAsString<T>(polypath[idx++], polypath[idx++], precision));
      Console.Write("\n");
      for (int i = 0; i < childCnt; i++)
        DisplayPolyPath<T>(polypath, ref idx, !isHole, spaceIndent, precision);
    }

    public static void DisplayPolytree<T>(T[] polytree, int precision)
    {
      int cnt = Convert.ToInt32(polytree[1]);
      int idx = 2;
      for (int i = 0; i < cnt; i++) 
        DisplayPolyPath<T>(polytree, ref idx, false, "  ", precision);
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

    const string clipperDll = @"..\..\..\..\Clipper2_64.dll";

    [DllImport(clipperDll, EntryPoint = 
      "Version", CallingConvention = CallingConvention.Cdecl)]
    static extern IntPtr Version();

    [DllImport(clipperDll, EntryPoint = 
      "BooleanOp64", CallingConvention = CallingConvention.Cdecl)]
    static extern Int32 BooleanOp64(byte clipType, byte fillRule,
      long[] subjects, long[]? openSubs, long[]? clips,
      out IntPtr solution, out IntPtr openSol, bool preserveCollinear, bool reverseSolution);

    [DllImport(clipperDll, EntryPoint = 
      "BooleanOpD", CallingConvention = CallingConvention.Cdecl)]
    static extern Int32 BooleanOpD(byte clipType, byte fillRule,
      double[] subjects, double[]? openSubs, double[]? clips,
      out IntPtr solution, out IntPtr openSol, Int32 precision, bool preserveCollinear, bool reverseSolution);

    [DllImport(clipperDll, EntryPoint =
      "DisposeArray64", CallingConvention = CallingConvention.Cdecl)]
    static extern void DisposeArray64(ref IntPtr intptr);

    // DisposeExported(): since all these functions behave identically ...
    [DllImport(clipperDll, EntryPoint =
      "DisposeArrayD", CallingConvention = CallingConvention.Cdecl)]
    static extern void DisposeArrayD(ref IntPtr intptr);

    [DllImport(clipperDll, EntryPoint = 
      "BooleanOp_PolyTree64", CallingConvention = CallingConvention.Cdecl)]
    static extern Int32 BooleanOp_PolyTree64(byte cliptype,
      byte fillrule, long[] subjects, long[]? openSubs, long[]? clips,
      out IntPtr solTree, out IntPtr openSol,
      bool preserve_collinear, bool reverse_solution);

    [DllImport(clipperDll, EntryPoint = 
      "BooleanOp_PolyTreeD", CallingConvention = CallingConvention.Cdecl)]
    static extern Int32 BooleanOp_PolyTreeD(byte cliptype,
      byte fillrule, double[] subjects, double[]? openSubs, double[]? clips,
      out IntPtr solTree, out IntPtr openSol, Int32 precision,
      bool preserve_collinear, bool reverse_solution);
    

    public static readonly byte None = 0, Intersection = 1, Union = 2, Difference = 3, Xor = 4;
    public static readonly byte EvenOdd = 0, NonZero = 1, Positive = 2, Negative = 3;


    /// Main Entry ////////////////////////////////////////////////////////////
    public static void Main()
    {

      //string? ver = Marshal.PtrToStringAnsi(Version());
      //Console.WriteLine(ver +"\n");

      // test BooleanOp64() ///////////////////////////////////////////////////
      Console.WriteLine("BooleanOp64:");
      long[] cSubject = CreateCPaths(new long[] { 0, 0, 100, 0, 100, 100, 0, 100 });
      long[] cClip = CreateCPaths(new long[] { 20, 20, 120, 20, 120, 120, 20, 120 });
      
      if (BooleanOp64(Intersection, NonZero, cSubject,
        null, cClip, out IntPtr cSol, out IntPtr cSolOpen, false, false) != 0) return;

      long[]? cSolution = GetArrayFromIntPtr<long>(cSol);
      // clean up unmanaged memory
      DisposeArray64(ref cSol);
      DisposeArray64(ref cSolOpen);

      DisplayCPaths(cSolution, "  ");
      /////////////////////////////////////////////////////////////////////////

      // test BooleanOpD() ////////////////////////////////////////////////////
      Console.WriteLine("BooleanOpD:");
      double[] cSubjectD = CreateCPaths(new double[] { 0, 0, 100, 0, 100, 100, 0, 100 });
      double[] cClipD = CreateCPaths(new double[] { 20, 20, 120, 20, 120, 120, 20, 120 });
      int resultD = BooleanOpD(Intersection, NonZero, cSubjectD,
        null, cClipD, out IntPtr cSolD, out IntPtr cSolOpenD, 2, false, false);
      if (resultD != 0) return;
      double[]? cSolutionD = GetArrayFromIntPtr<double>(cSolD);
      // clean up unmanaged memory
      DisposeArrayD(ref cSolD);
      DisposeArrayD(ref cSolOpenD);

      DisplayCPaths(cSolutionD, "  ");
      /////////////////////////////////////////////////////////////////////////



      // test BooleanOp_PolyTree64() //////////////////////////////////////////
      Console.WriteLine("BooleanOp_PolyTree64:");

      List<long[]> subList = new(5);
      for (int i = 1; i < 6; ++i)
        subList.Add(CreateCPath(new long[] { 
          -i * 20, -i * 20, i * 20, -i * 20, i * 20, i * 20, -i * 20, i * 20 })!);

      long[] cSubject3 = CreateCPaths(subList);
      long[] cClip3 = CreateCPaths(new long[] { -90, -120, 90, -120, 90, 120, -90, 120 });

      int result3 = BooleanOp_PolyTree64(Intersection, EvenOdd, cSubject3, null, cClip3,
        out IntPtr cSol_pt64, out IntPtr cSolOpen_pt64, false, false);
      if (result3 != 0) return;

      long[]? cPolyTree64 = GetArrayFromIntPtr<long>(cSol_pt64);
      // clean up unmanaged memory
      DisposeArray64(ref cSol_pt64);
      DisposeArray64(ref cSolOpen_pt64);

      if (cPolyTree64 == null) return;
      DisplayPolytree<long>(cPolyTree64, 2);
      /////////////////////////////////////////////////////////////////////////


      // test BooleanOp_PolyTreeD() ///////////////////////////////////////////
      Console.WriteLine("BooleanOp_PolyTreeD:");

      List<double[]> subList2 = new(5);
      for (int i = 1; i < 6; ++i)
        subList2.Add(CreateCPath(new double[] { 
          -i * 20, -i * 20, i * 20, -i * 20, i * 20, i * 20, -i * 20, i * 20 })!);

      double[] cSubject4 = CreateCPaths(subList2);
      double[] cClip4 = CreateCPaths(new double[] { -90, -120, 90, -120, 90, 120, -90, 120 });

      int result4 = BooleanOp_PolyTreeD(Intersection, EvenOdd, cSubject4, null, cClip4,
        out IntPtr cSol_ptD, out IntPtr cSolOpen_ptD, 2, false, false);
      if (result4 != 0) return;

      double[]? cPolyTreeD = GetArrayFromIntPtr<double>(cSol_ptD);

      // clean up unmanaged memory
      DisposeArrayD(ref cSol_ptD);
      DisposeArrayD(ref cSolOpen_ptD);

      if (cPolyTreeD == null) return;
      DisplayPolytree<double>(cPolyTreeD, 2);
      /////////////////////////////////////////////////////////////////////////


      Console.WriteLine("\nPress any key to exit ... ");
      Console.ReadKey();
    }

  } //end Application
} //namespace
