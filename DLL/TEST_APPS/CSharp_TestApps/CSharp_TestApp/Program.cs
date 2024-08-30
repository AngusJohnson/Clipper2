/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  29 October 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using Clipper2Dll;
using System.Globalization;
using System.Runtime.InteropServices;
using static Clipper2Dll.Clipper2DllCore;
using static Clipper2Dll.SvgWriterUtils;

using ztype = System.Int64;
using Point64 = Clipper2Dll.Clipper2DllCore.Point<long>;
using PointD = Clipper2Dll.Clipper2DllCore.Point<double>;

namespace ClipperDllDemo
{

  public class Application
  {

#if USINGZ    
    private static long counter = 0;
    private static void MyCallBack64(Point64 e1bot, Point64 e1top, Point64 e2bot, Point64 e2top, ref Point64 ip)
    {
      ip.Z = counter++;
    }

    private static void MyCallBackD(PointD e1bot, PointD e1top, PointD e2bot, PointD e2top, ref PointD ip)
    {
      ip.Z = StaticCastDoubleToLong((double)counter++);
    }
#endif

    private static void DrawSVG(string filename, long[]? cSubjects, long[]? cClips, long[]? cSolution)
    {
      SvgWriter svg = new SvgWriter();
      if (cSubjects != null)
        AddSubjects(svg, cSubjects);
      if (cClips != null)
        AddClips(svg, cClips);
      if (cSolution != null)
      {
        AddSolution(svg, cSolution, false);
#if USINGZ
        long pathsCnt = cSolution[1];
        long idx = 2;
        for (long i = 0; i < pathsCnt; i++)
        {
          long pathLen = cSolution[idx];
          idx += 2;
          for (long j = 0; j < pathLen; j++)
          {
            long x = cSolution[idx++];
            long y = cSolution[idx++];
            long z = cSolution[idx++];
            if (z <= 10) continue;
            svg.AddText(string.Format(NumberFormatInfo.InvariantInfo, "{0}", z), x +1, y - 3, 9);
            svg.AddCircle(x, y, 3, 0x40FFFF00, 0xFF000000, 2);
          }
        }
#endif
      }
      svg.SaveToFile(filename, 400, 400);
      OpenFileWithDefaultApp(filename);
    }

    private static void DrawSVG(string filename, double[] cSubjects, double[] cClips, double[] cSolution)
    {
      SvgWriter svg = new SvgWriter();
      AddSubjects(svg, cSubjects);
      AddClips(svg, cClips);
      if (cSolution.Length > 0)
      {
        AddSolution(svg, cSolution, false);
#if USINGZ
        long pathsCnt = Convert.ToInt64(cSolution[1]);
        long idx = 2;
        for (long i = 0; i < pathsCnt; i++)
        {
          long pathLen = Convert.ToInt64(cSolution[idx]);
          idx += 2;
          for (long j = 0; j < pathLen; j++)
          {
            double x = cSolution[idx++];
            double y = cSolution[idx++];
            double z = cSolution[idx++];
            if (z <= 10) continue;
            svg.AddText(string.Format(NumberFormatInfo.InvariantInfo, "{0}", z), x + 1, y - 3, 9);
            svg.AddCircle(x, y, 3, 0x40FFFF00, 0xFF000000, 2);
          }
        }
#endif
      }
      svg.SaveToFile(filename, 400, 400);
      OpenFileWithDefaultApp(filename);
    }

    /// Main Entry ////////////////////////////////////////////////////////////
    public static void Main()
    {
      //string? ver = Marshal.PtrToStringAnsi(Version());
      //Console.WriteLine(ver + "\n");

#if USINGZ    
      SetZCallback64(MyCallBack64);
      SetZCallbackD(MyCallBackD);
#endif

      /////////////////////////////////////////////////////////////////////////
      // test BooleanOp64() ///////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////

#if USINGZ
      long[] cSubject = CreateCPathsFromCoords(new long[] { 0,0,1, 100,0,2, 100,100,3, 0,100,4 });
      long[] cClip = CreateCPathsFromCoords(new long[] { 20,20,1, 120,20,2, 120,120,3, 20,120,4 });
      counter = 11;
#else
      long[] cSubject = CreateCPathsFromCoords(new long[] { 0,0, 100,0, 100,100, 0,100 });
      long[] cClip = CreateCPathsFromCoords(new long[] { 20,20, 120,20, 120,120, 20,120 });
#endif

      if (BooleanOp64((int)ClipType.Intersection, (int)FillRule.NonZero, cSubject,
        null, cClip, out IntPtr cSol, out IntPtr cSolOpen, false, false) != 0) return;

      long[]? cSolution = GetArrayFromIntPtr<long>(cSol);

      //Console.WriteLine("BooleanOp64:");
      //LogCPaths(cSolution, "  ");

      // clean up unmanaged memory
      DisposeArray64(ref cSol);
      DisposeArray64(ref cSolOpen);

      DrawSVG(@"..\..\..\rectangles.svg", cSubject, cClip, cSolution);

      /////////////////////////////////////////////////////////////////////////
      // test BooleanOpD() ////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////

#if USINGZ
      double[] cSubjectD = CreateCPathsFromCoords(new double[] { 0,0,1, 100,0,2, 100,100,3, 0,100,4 });
      double[] cClipD = CreateCPathsFromCoords(new double[] { 20,20,1, 120,20,2, 120,120,3, 20,120,4 });
      counter = 21;
#else
      double[] cSubjectD = CreateCPathsFromCoords(new double[] { 0,0, 100,0, 100,100, 0,100 });
      double[] cClipD = CreateCPathsFromCoords(new double[] { 20,20, 120,20, 120,120, 20,120 });
#endif
      int resultD = BooleanOpD((int)ClipType.Intersection, (int)FillRule.NonZero, cSubjectD,
        null, cClipD, out IntPtr cSolD, out IntPtr cSolOpenD, 2, false, false);
      if (resultD != 0) return;
      double[]? cSolutionD = GetArrayFromIntPtr<double>(cSolD);
      
      //Console.WriteLine("BooleanOpD:");
      //LogCPaths(cSolutionD, "  ");

      // clean up unmanaged memory
      DisposeArrayD(ref cSolD);
      DisposeArrayD(ref cSolOpenD);

      DrawSVG(@"..\..\..\rectangles2.svg", cSubjectD, cClipD, cSolutionD!);

      /////////////////////////////////////////////////////////////////////////
      // test BooleanOp_PolyTree64() //////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////

      // create arrays of x,y coords that define 5 successively 
      // larger rectangles centered on the origin ...
      List<long[]> cRectangles = new(5);
#if USINGZ
      for (int i = 1; i < 6; ++i)
        cRectangles.Add(CreateCPath(new long[] {
          -i*20,-i*20,1, i*20,-i*20,2, i*20,i*20,3, -i*20,i*20,4 })!);
      long[] cSubjects = CreateCPathsFromCPathList(cRectangles);
      long[] cClips = CreateCPathsFromCoords(
        new long[] { -90,-120,11, 90,-120,12, 90,120,13, -90,120,14 });
      counter = 31;
#else
      for (int i = 1; i < 6; ++i)
        cRectangles.Add(CreateCPath(new long[] { 
          -i*20,-i*20, i*20,-i*20, i*20,i*20, -i*20,i*20 })!);
      long[] cSubjects = CreateCPathsFromCPathList(cRectangles);
      long[] cClips = CreateCPathsFromCoords(new long[] { -90,-120, 90,-120, 90,120, -90,120 });
#endif

      int result3 = BooleanOp_PolyTree64((int)ClipType.Intersection, 
        (int)FillRule.EvenOdd, cSubjects, null, cClips,
        out IntPtr cSol_pt64, out IntPtr cSolOpen_pt64, false, false);
      if (result3 != 0) return;

      long[]? cPolyTree64 = GetArrayFromIntPtr<long>(cSol_pt64);
      // clean up unmanaged memory
      DisposeArray64(ref cSol_pt64);
      DisposeArray64(ref cSolOpen_pt64);

      if (cPolyTree64 == null) return;
      //Console.WriteLine("BooleanOp_PolyTree64:");
      //LogPolytree<long>(cPolyTree64, 2);

      ConvertCPolytreeToCPaths(cPolyTree64, out long[] solution5);
      DrawSVG(@"..\..\..\polytree64.svg", cSubjects, cClips, solution5);

      /////////////////////////////////////////////////////////////////////////
      // test BooleanOp_PolyTreeD() ///////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////

      List<double[]> subList2 = new(5);
#if USINGZ
      for (int i = 1; i < 6; ++i)
        subList2.Add(CreateCPath(new double[] {
          -i*20,-i*20,1, i*20,-i*20,2, i*20,i*20,3, -i*20,i*20,4 })!);
      double[] cSubject4 = CreateCPathsFromCPathList(subList2);
      double[] cClip4 = CreateCPathsFromCoords(new double[] { -90, -120, 11, 90, -120, 12, 90, 120, 13, -90, 120, 14 });
      counter = 41;
#else
      for (int i = 1; i < 6; ++i)
        subList2.Add(CreateCPath(new double[] { 
          -i*20,-i*20, i*20,-i*20, i*20,i*20, -i*20,i*20 })!);
      double[] cSubject4 = CreateCPathsFromCPathList(subList2);
      double[] cClip4 = CreateCPathsFromCoords(new double[] { -90,-120, 90,-120, 90,120, -90,120 });
#endif

      int result4 = BooleanOp_PolyTreeD((int)ClipType.Intersection, 
        (int)FillRule.EvenOdd, cSubject4, null, cClip4,
        out IntPtr cSol_ptD, out IntPtr cSolOpen_ptD, 2, false, false);
      if (result4 != 0) return;

      double[]? cPolyTreeD = GetArrayFromIntPtr<double>(cSol_ptD);

      // clean up unmanaged memory
      DisposeArrayD(ref cSol_ptD);
      DisposeArrayD(ref cSolOpen_ptD);

      if (cPolyTreeD == null) return;

      //Console.WriteLine("BooleanOp_PolyTreeD:");
      //LogPolytree<double>(cPolyTreeD, 2);

      ConvertCPolytreeToCPaths(cPolyTreeD, out double[] solution6);
      DrawSVG(@"..\..\..\polytreeD.svg", cSubject4, cClip4, solution6);

      /////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////

      //Console.WriteLine("\nPress any key to exit ... ");
      //Console.ReadKey();
    }

  } //end Application
} //end namespace
