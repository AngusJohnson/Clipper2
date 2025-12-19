/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  18 December 2025                                                *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2025                                         *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************/

using Clipper2Dll;
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

    private static void DrawSVG_MultiColor(string filename, long[]? cSolution)
    {
      SvgWriter svg = new SvgWriter();
      if (cSolution == null) return;
      AddSolution_MultiColor(svg, cSolution, false);
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


    public static void TestBooleanOp64()
    {
#if USINGZ
        long[] cSubject = CreateCPathsFromCoords(new long[] { 0, 0, 1, 100, 0, 2, 100, 100, 3, 0, 100, 4 });
        long[] cClip = CreateCPathsFromCoords(new long[] { 30, 30, 1, 130, 30, 2, 130, 130, 3, 30, 130, 4 });
        counter = 11;
#else
        long[] cSubject = CreateCPathsFromCoords(new long[] { 0,0, 100,0, 100,100, 0,100 });
        long[] cClip = CreateCPathsFromCoords(new long[] { 30,30, 130,30, 130,130, 30,130 });
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
    }


    public static void TestBooleanOpD()
    {
#if USINGZ
      double[] cSubjectD = CreateCPathsFromCoords(new double[] { 0, 0, 1, 100, 0, 2, 100, 100, 3, 0, 100, 4 });
      double[] cClipD = CreateCPathsFromCoords(new double[] { 30, 30, 1, 130, 30, 2, 130, 130, 3, 30, 130, 4 });
      counter = 21;
#else
      double[] cSubjectD = CreateCPathsFromCoords(new double[] { 0,0, 100,0, 100,100, 0,100 });
      double[] cClipD = CreateCPathsFromCoords(new double[] { 30,30, 130,30, 130,130, 30,130 });
#endif
      int resultD = BooleanOpD((int)ClipType.Intersection, (int)FillRule.NonZero, cSubjectD,
        null, cClipD, out IntPtr cSolD, out IntPtr cSolOpenD, 2, false, false);
      if (resultD != 0) return;
      double[]? cSolutionD = GetArrayFromIntPtr<double>(cSolD);

      // clean up unmanaged memory
      DisposeArrayD(ref cSolD);
      DisposeArrayD(ref cSolOpenD);

      DrawSVG(@"..\..\..\rectangles2.svg", cSubjectD, cClipD, cSolutionD!);
    }

    public static void TestBooleanOp_Polytree64()
    {
      // create arrays of x,y coords that define 5 successively 
      // larger rectangles centered on the origin ...
      List<long[]> cRectangles = new(5);
#if USINGZ
      for (int i = 1; i < 6; ++i)
        cRectangles.Add(CreateCPath(new long[] {
          -i*20,-i*20,1, i*20,-i*20,2, i*20,i*20,3, -i*20,i*20,4 })!);
      long[] cSubjects = CreateCPathsFromCPathList(cRectangles);
      long[] cClips = CreateCPathsFromCoords(
        new long[] { -90, -120, 11, 90, -120, 12, 90, 120, 13, -90, 120, 14 });
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
    }

    
    public static void TestBooleanOp_PolytreeD() 
    {
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
    }

    public static void TestTriangulate()
    {
      // 'coral3' outline ...
      long arrLen = 2 + (2 + 248 * 2) + (2 + 28 * 2) + (2 + 20 * 2);
      long[] subjects =
      {
        arrLen, 3,  // array length, 3 polygon contours (1 outer path + 2 holes)
        // outer path
        248, 0,     // outer path length, discarded 0 value
        5685, 2435, 5720, 2510, 5700, 2600, 5570, 2770, 5380, 2920, 5210, 2990, 5110, 2990, 5050, 2970, 4970,
        2930, 4850, 2895, 4770, 2920, 4700, 3040, 4570, 3430, 4440, 3910, 4370, 4140, 4270, 4330, 4100, 4500,
        3870, 4660, 3700, 4820, 3660, 4910, 3680, 5010, 3730, 5070, 3790, 5100, 3960, 5110, 4140, 5070, 4300,
        5000, 4520, 4840, 4730, 4600, 5110, 4010, 5390, 3550, 5470, 3485, 5530, 3495, 5560, 3530, 5580, 3710,
        5540, 3900, 5340, 4310, 5080, 4690, 4850, 4990, 4740, 5110, 4570, 5220, 4160, 5420, 3730, 5560, 3400,
        5620, 3060, 5610, 2710, 5570, 2380, 5500, 2070, 5370, 1770, 5150, 1520, 4880, 1060, 4290, 670, 3930,
        490, 3740, 370, 3520, 270, 3150, 270, 2980, 310, 2920, 370, 2880, 450, 2890, 510, 2940, 580, 3130,
        640, 3370, 730, 3560, 1040, 3930, 1210, 4050, 1300, 4060, 1370, 4020, 1460, 3880, 1440, 3730, 1340,
        3570, 1190, 3390, 830, 3030, 690, 2850, 590, 2660, 520, 2460, 510, 2370, 560, 2310, 630, 2310, 690,
        2360, 780, 2570, 880, 2760, 940, 2810, 1000, 2810, 1050, 2760, 1070, 2710, 1040, 2570, 820, 2240, 600,
        1900, 570, 1770, 590, 1710, 640, 1670, 730, 1650, 800, 1680, 930, 1840, 1140, 2290, 1220, 2480, 1260,
        2690, 1360, 3110, 1710, 4010, 1780, 4270, 1850, 4560, 1950, 4790, 2030, 4840, 2120, 4830, 2220, 4780,
        2290, 4720, 2340, 4540, 2300, 4310, 2200, 4050, 1930, 3480, 1800, 3210, 1720, 2950, 1630, 2720, 1500,
        2470, 1220, 1960, 1120, 1730, 1070, 1510, 1100, 1340, 1150, 1270, 1240, 1210, 1340, 1200, 1430, 1260,
        1500, 1390, 1530, 1580, 1560, 1910, 1620, 2280, 1680, 2430, 1750, 2560, 1850, 2630, 1980, 2640, 2090,
        2590, 2160, 2480, 2180, 2340, 2170, 2170, 2110, 1800, 2050, 1450, 2020, 1280, 1950, 1090, 1770, 700,
        1700, 530, 1680, 370, 1720, 260, 1820, 190, 1930, 200, 2000, 290, 2080, 660, 2150, 1050, 2230, 1180,
        2350, 1230, 2430, 1200, 2480, 1110, 2530, 860, 2580, 620, 2630, 560, 2710, 550, 2830, 630, 2870, 760,
        2860, 930, 2800, 1120, 2620, 1540, 2500, 1930, 2360, 2510, 2300, 2810, 2270, 3100, 2250, 3580, 2290,
        3790, 2340, 3850, 2420, 3890, 2500, 3880, 2560, 3840, 2650, 3670, 2760, 3250, 2830, 2950, 2870, 2640,
        2940, 2030, 3110, 1270, 3100, 1020, 3060, 740, 3060, 510, 3100, 430, 3190, 380, 3320, 380, 3390, 430,
        3430, 510, 3440, 610, 3440, 840, 3460, 930, 3520, 990, 3630, 1020, 3750, 980, 3830, 890, 3870, 770, 3920,
        460, 3950, 320, 4000, 210, 4110, 160, 4270, 170, 4340, 220, 4370, 300, 4330, 540, 4230, 810, 4170, 1070,
        4130, 1550, 4160, 1750, 4210, 1820, 4280, 1860, 4360, 1860, 4420, 1820, 4510, 1680, 4570, 1470, 4610,
        1230, 4650, 980, 4710, 780, 4790, 650, 4850, 630, 4930, 635, 5040, 700, 5090, 810, 5090, 950, 5060, 1110,
        4950, 1460, 4810, 1770, 4690, 1920, 4510, 2050, 4380, 2170, 4360, 2240, 4380, 2300, 4430, 2350, 4500,
        2370, 4670, 2320, 5010, 2110, 5150, 1950, 5270, 1730, 5390, 1570, 5450, 1550, 5520, 1560, 5570, 1610,
        5590, 1680, 5550, 1800, 5310, 2110, 5050, 2440, 5000, 2590, 5010, 2660, 5050, 2720, 5120, 2740, 5200,
        2710, 5380, 2570, 5560, 2420, 5630, 2410,
        // hole 1
        28, 0,        // inner path length, discarded 0 value
        3190, 2270, 3150, 2840, 3010, 3410, 2860, 3850, 2730, 4200, 2670, 4500, 2710, 4580, 2810, 4640, 2950, 
        4650, 3060, 4630, 3200, 4520, 3300, 4380, 3360, 4200, 3400, 4010, 3450, 3600, 3530, 3210, 3770, 2730, 
        3870, 2480, 3930, 2230, 3960, 2000, 3960, 1750, 3910, 1540, 3850, 1460, 3770, 1410, 3640, 1410, 3530, 
        1490, 3340, 1740, 3230, 1990,
        // hole 2     // inner path length, discarded 0 value
        20, 0,
        4240, 2700, 4050, 2800, 3900, 2970, 3810, 3160, 3710, 3580, 3550, 4080, 3530, 4310, 3560, 4390, 3640, 
        4430, 3870, 4390, 4040, 4290, 4150, 4130, 4230, 3940, 4320, 3490, 4380, 3260, 4460, 3050, 4540, 2880, 
        4550, 2770, 4510, 2700, 4440, 2670 };

      IntPtr sol = Triangulate64(subjects, true);
      long []? cSol64 = GetArrayFromIntPtr<long>(sol);

      DrawSVG_MultiColor(@"..\..\..\coral3_t.svg", cSol64);
      DisposeArray64(ref sol); // disposes DLL assigned memory
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

      TestBooleanOp64();
      TestBooleanOpD();
      TestBooleanOp_Polytree64();
      TestBooleanOp_PolytreeD();
      TestTriangulate();

      //Console.WriteLine("\nPress any key to exit ... ");
      //Console.ReadKey();
    }

  } //end Application
} //end namespace
