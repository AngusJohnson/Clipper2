/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  24 March 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System.Collections.Generic;
using System.Reflection;
using System.IO;
using System;
using System.Security.Cryptography;
using System.Xml.Linq;
using System.Runtime.InteropServices;
using System.Diagnostics;

using Clipper2Lib;

namespace UsingZTestApp
{
  public class Application
  {
    public class MyCallbacks
    {
      public int cnt = 0;
      public void MyCallback64(Point64 _, Point64 __,
        Point64 ___, Point64 ____, ref Point64 intersectPt)
      {
        cnt--;
        intersectPt.Z = cnt;
      }

      public void MyCallbackD(PointD _, PointD __,
          PointD ___, PointD ____, ref PointD intersectPt)
      {
        cnt--;
        intersectPt.z = cnt;
      }
    }

    public static void Main()
    {
      PathsD solution = new();
      PathsD subject = new()
    {
      Clipper.MakePathZ(new double[] { 100,50,1, 10,79,2, 65,2,3, 65,98,4, 10,21,5 })
    };

      ClipperD clipperD = new ();
      MyCallbacks cb = new ();
      clipperD.ZCallback = cb.MyCallbackD;
      clipperD.AddSubject(subject);
      clipperD.Execute(ClipType.Union, FillRule.NonZero, solution);

      solution = Clipper.InflatePaths(solution, -3, JoinType.Miter, EndType.Polygon);
      Console.WriteLine(solution.ToString(0));

      SvgWriter svg = new (FillRule.NonZero);
      SvgUtils.AddSubject(svg, subject);
      SvgUtils.AddSolution(svg, solution, true);

      PathsD ellipses = new ();
      for (int i = 0; i < solution[0].Count; i++)
      {
        if (solution[0][i].z < 0)
          ellipses.Add(Clipper.Ellipse(
            new PointD(solution[0][i].x, solution[0][i].y), 4));
        svg.AddText(solution[0][i].z.ToString(), 
          solution[0][i].x, solution[0][i].y, 12, 0xFF000000);
      }
      svg.AddClosedPaths(ellipses, 0x20FF0000, 0xFFFF0000, 1);
      svg.SaveToFile("usingz.svg", 300, 300);
      OpenFileWithDefaultApp("usingz.svg");
    }

    public static void OpenFileWithDefaultApp(string filename)
    {
      string path = Path.GetFullPath(filename);
      if (!File.Exists(path)) return;
      Process p = new ()
      {
        StartInfo = new ProcessStartInfo(path) { UseShellExecute = true }
      };
      p.Start();
    }

  }
}