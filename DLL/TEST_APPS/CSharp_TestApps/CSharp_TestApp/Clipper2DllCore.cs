using System.Drawing;
using System.Runtime.InteropServices;
using static Clipper2Dll.Clipper2DllCore;

using ztype = System.Int64;
using Point64 = Clipper2Dll.Clipper2DllCore.Point<long>;
using PointD = Clipper2Dll.Clipper2DllCore.Point<double>;
using Rect64 = Clipper2Dll.Clipper2DllCore.Rect<long>;
using RectD = Clipper2Dll.Clipper2DllCore.Rect<double>;

namespace Clipper2Dll
{

  public static class Clipper2DllCore
  {
    /////////////////////////////////////////////////////////////////////////
    // (Very abbreviated) Clipper2 structures 
    /////////////////////////////////////////////////////////////////////////
    public enum FillRule { EvenOdd, NonZero, Positive, Negative };
    public enum ClipType { None, Intersection, Union, Difference, Xor };

#if USINGZ
    public static long VERTEX_FIELD_CNT = 3;

    public struct Point<T>
    {
      public T X;
      public T Y;
      public ztype Z;

      public Point(Point<T> pt)
      {
        X = pt.X;
        Y = pt.Y;
        Z = pt.Z;
      }

      public Point(T x, T y, ztype z = 0)
      {
        X = x;
        Y = y;
        Z = z;
      }
    }
#else
    
    public static long VERTEX_FIELD_CNT = 2;

    public struct Point<T>
    {
      public T X;
      public T Y;

      public Point(Point<T> pt)
      {
        X = pt.X;
        Y = pt.Y;
      }

      public Point(T x, T y)
      {
        X = x;
        Y = y;
      }
    }
#endif

    public struct Rect<T> where T : IComparable
    {
      public T left;
      public T top;
      public T right;
      public T bottom;
      public Rect(T l, T t, T r, T b)
      {
        left = l;
        top = t;
        right = r;
        bottom = b;
      }
      public bool IsEmpty()
      {        
        return left.CompareTo(right) >= 0 || top.CompareTo(bottom) >= 0;
      }
      public double Width()
      {
        if (IsEmpty()) return 0;
        dynamic r = right;
        dynamic l = left;
        return r - l;
      }
      public double Height()
      {
        if (IsEmpty()) return 0;
        dynamic b = bottom;
        dynamic t = top;
        return b - t;
      }
    }

    public static RectD InvalidRectD =
      new RectD(double.MaxValue, double.MaxValue, -double.MaxValue, -double.MaxValue);

    /////////////////////////////////////////////////////////////////////////
    // Clipper2 DLL - exported functions
    /////////////////////////////////////////////////////////////////////////

#if USINGZ
    const string clipperDll = @"..\..\..\..\..\Clipper2_Z_64.dll";
#else
    const string clipperDll = @"..\..\..\..\..\Clipper2_64.dll";
#endif

    [DllImport(clipperDll, EntryPoint =
      "Version", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr Version();

    [DllImport(clipperDll, EntryPoint =
      "BooleanOp64", CallingConvention = CallingConvention.Cdecl)]
    public static extern Int32 BooleanOp64(byte clipType, byte fillRule,
      long[] subjects, long[]? openSubs, long[]? clips,
      out IntPtr solution, out IntPtr openSol, bool preserveCollinear, bool reverseSolution);

    [DllImport(clipperDll, EntryPoint =
      "BooleanOpD", CallingConvention = CallingConvention.Cdecl)]
    public static extern Int32 BooleanOpD(byte clipType, byte fillRule,
      double[] subjects, double[]? openSubs, double[]? clips,
      out IntPtr solution, out IntPtr openSol, Int32 precision, bool preserveCollinear, bool reverseSolution);

    [DllImport(clipperDll, EntryPoint =
      "DisposeArray64", CallingConvention = CallingConvention.Cdecl)]
    public static extern void DisposeArray64(ref IntPtr intptr);

    // DisposeExported(): since all these functions behave identically ...
    [DllImport(clipperDll, EntryPoint =
      "DisposeArrayD", CallingConvention = CallingConvention.Cdecl)]
    public static extern void DisposeArrayD(ref IntPtr intptr);

    [DllImport(clipperDll, EntryPoint =
      "BooleanOp_PolyTree64", CallingConvention = CallingConvention.Cdecl)]
    public static extern Int32 BooleanOp_PolyTree64(byte cliptype,
      byte fillrule, long[] subjects, long[]? openSubs, long[]? clips,
      out IntPtr solTree, out IntPtr openSol,
      bool preserve_collinear, bool reverse_solution);

    [DllImport(clipperDll, EntryPoint =
      "BooleanOp_PolyTreeD", CallingConvention = CallingConvention.Cdecl)]
    public static extern Int32 BooleanOp_PolyTreeD(byte cliptype,
      byte fillrule, double[] subjects, double[]? openSubs, double[]? clips,
      out IntPtr solTree, out IntPtr openSol, Int32 precision,
      bool preserve_collinear, bool reverse_solution);


#if USINGZ    
    public delegate void DLLZCallback64(Point64 e1bot, Point64 e1top, Point64 e2bot, Point64 e2top, ref Point64 ip);

    [DllImport(clipperDll, EntryPoint =
      "SetZCallback64", CallingConvention = CallingConvention.Cdecl)]
    public static extern void SetZCallback64(DLLZCallback64 callback);

    public delegate void DLLZCallbackD(PointD e1bot, PointD e1top, PointD e2bot, PointD e2top, ref PointD ip);

    [DllImport(clipperDll, EntryPoint =
      "SetZCallbackD", CallingConvention = CallingConvention.Cdecl)]
    public static extern void SetZCallbackD(DLLZCallbackD callback);
#endif

    /////////////////////////////////////////////////////////////////////////
    // Clipper2 DLL support functions
    /////////////////////////////////////////////////////////////////////////

    // CreateCPaths: The CPaths<T> structure is defined in
    // clipper.export.h and is a simple array of long[] or
    // double[] that represents a number of path contours.

    public static T[]? CreateCPath<T>(T[] coords)
    {
      long pathLen = coords.Length / VERTEX_FIELD_CNT;
      if (pathLen == 0) return null;
      long arrayLen = pathLen * VERTEX_FIELD_CNT + 2;
      T[] result = new T[arrayLen];
      result[0] = (T)Convert.ChangeType(pathLen, typeof(T));
      result[1] = (T)Convert.ChangeType(0, typeof(T));
      coords.CopyTo(result, 2);
      return result;
    }

    public static T[] CreateCPathsFromCPathList<T>(List<T[]> listOfCPath)
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
    public static T[] CreateCPathsFromCoords<T>(T[] coords)
    {
      long pathLen = coords.Length / VERTEX_FIELD_CNT;
      long arrayLen = pathLen * VERTEX_FIELD_CNT + 4;
      T[] result = new T[arrayLen];

      result[0] = (T)Convert.ChangeType(arrayLen, typeof(T));
      result[1] = (T)Convert.ChangeType(1, typeof(T)); // 1 path
      result[2] = (T)Convert.ChangeType(pathLen, typeof(T));
      result[3] = (T)Convert.ChangeType(0, typeof(T));

      coords.CopyTo(result, 4);
      return result;
    }

#if USINGZ
    public static string VertexAsString<T>(T X, T Y, T Z, int precision)
    {
      if (typeof(T) == typeof(long)) // ignore precision
        return $"{X},{Y},{Z} ";
      else
        return string.Format($"{{0:F{precision}}},{{1:F{precision}}},{{2:F{precision}}} ", X, Y, Z);
    }
#else
    public static string VertexAsString<T>(T X, T Y, int precision)
    {
      if (typeof(T) == typeof(long)) // ignore precision
        return $"{X},{Y} ";
      else
        return string.Format($"{{0:F{precision}}},{{1:F{precision}}} ", X, Y);
    }
#endif

    public static void LogCPath<T>(T[] cpaths, ref int idx, string spaceIndent)
    {
      int vertexCnt = Convert.ToInt32(cpaths[idx]);
      idx += 2;
      for (int i = 0; i < vertexCnt; i++)
#if USINGZ
        Console.Write(spaceIndent + VertexAsString<T>(cpaths[idx++], cpaths[idx++], cpaths[idx++], 2));
#else
        Console.Write(spaceIndent + VertexAsString<T>(cpaths[idx++], cpaths[idx++], 2));
#endif
      Console.Write("\n");
    }

    public static void LogCPaths<T>(T[]? cpaths, string spaceIndent)
    {
      if (cpaths == null) return;
      int pathCnt = Convert.ToInt32(cpaths[1]);
      int idx = 2;
      for (int i = 0; i < pathCnt; i++)
        LogCPath<T>(cpaths, ref idx, spaceIndent);
    }

    // Note: The CPolyTree<T> structure defined in clipper.export.h is 
    // a simple array of T that contains any number of nested path contours.

    private static void CPolypathCounter<T>(T[] cpolypath, 
      ref long idx, ref int pathCount, ref long arrayLen)
    {
      // returns the vertex count of each contained polygon
      int vertexCount = Convert.ToInt32(cpolypath[idx++]);
      int childCount = Convert.ToInt32(cpolypath[idx++]);
      arrayLen += vertexCount * VERTEX_FIELD_CNT + 2;
      pathCount++;
      idx += vertexCount * VERTEX_FIELD_CNT;
      for (int i = 0; i < childCount; i++)
        CPolypathCounter(cpolypath, ref idx, ref pathCount, ref arrayLen);
    }
    private static void ConvertCPolypathToCPaths<T>(T[] cpolypath, ref long idx, 
      T[] result, ref int resultIdx)
    {
      T vertexCount = cpolypath[idx++]!;
      int vertCnt = (int)Convert.ChangeType(vertexCount, typeof(int));
      int childCount = Convert.ToInt32(cpolypath[idx++]);
      result[resultIdx++] = vertexCount; 
      result[resultIdx++] = (T)Convert.ChangeType(0, typeof(T));
      // parse path
      for (int i = 0; i < vertCnt; i++)
      {
        result[resultIdx++] = cpolypath[idx++]; // x
        result[resultIdx++] = cpolypath[idx++]; // y
#if USINGZ
        result[resultIdx++] = cpolypath[idx++]; // z
#endif
      }
      // parse children
      for (int i = 0; i < childCount; i++)
        ConvertCPolypathToCPaths(cpolypath, ref idx, result, ref resultIdx);
    }

    public static void ConvertCPolytreeToCPaths<T>(T[] cpolytree, out T[] result)
    {
      if (cpolytree == null)
      {
        result = new T[0];
        return;
      }
      int topPathsCount = Convert.ToInt32(cpolytree[1]);
      long idx = 2; 
      int pathCount = 0; 
      long arrayLen = 2;
      for (int i = 0; i < topPathsCount; i++)
        CPolypathCounter(cpolytree, ref idx, ref pathCount, ref arrayLen);
      result = new T[arrayLen];
      result[0] = (T)Convert.ChangeType(arrayLen, typeof(T));
      result[1] = (T)Convert.ChangeType(pathCount, typeof(T));
      int resultIdx = 2;
      idx = 2;
      for (int i = 0; i < topPathsCount; i++)
        ConvertCPolypathToCPaths(cpolytree, ref idx, result, ref resultIdx);
    }

    public static void LogPolyPath<T>(T[] polypath,
      ref int idx, bool isHole, string spaceIndent, int precision)
    {
      int polyCnt = Convert.ToInt32(polypath[idx++]);
      int childCnt = Convert.ToInt32(polypath[idx++]);
      string preamble = isHole ? "Hole: " : (spaceIndent == "") ?
        "Polygon: " : "Nested Polygon: ";
      Console.Write(spaceIndent + preamble);
      spaceIndent += "  ";
      for (int i = 0; i < polyCnt; i++)
#if USINGZ
        Console.Write(VertexAsString<T>(polypath[idx++], polypath[idx++], polypath[idx++], precision));
#else
      Console.Write(VertexAsString<T>(polypath[idx++], polypath[idx++], precision));
#endif
      Console.Write("\n");
      for (int i = 0; i < childCnt; i++)
        LogPolyPath<T>(polypath, ref idx, !isHole, spaceIndent, precision);
    }

    public static void LogPolytree<T>(T[] polytree, int precision)
    {
      int cnt = Convert.ToInt32(polytree[1]);
      int idx = 2;
      for (int i = 0; i < cnt; i++)
        LogPolyPath<T>(polytree, ref idx, false, "  ", precision);
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

    public static void ConvertArrayOfLongs(long[] longs, out double[] result)
    {
      result = new double[longs.Length];
      for (int i = 0; i < longs.Length; i++)
        result[i] = longs[i];
    }

    public static double StaticCastLongToDouble(long val)
    {
      byte[] b = BitConverter.GetBytes(val);
      return BitConverter.ToDouble(b, 0);
    }
    public static long StaticCastDoubleToLong(double val)
    {
      byte[] b = BitConverter.GetBytes(val);
      return BitConverter.ToInt64(b, 0);
    }

  }
}
