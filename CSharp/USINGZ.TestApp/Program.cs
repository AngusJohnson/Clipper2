/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  24 January 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
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

public class Application
{
  public class MyCallbacks
  {
    public void MyCallback64(Point64 bot1, Point64 top1,
      Point64 bot2, Point64 top2, ref Point64 intersectPt)
    {
      intersectPt.Z = 1;
    }

    public void MyCallbackD(PointD bot1, PointD top1,
        PointD bot2, PointD top2, ref PointD intersectPt)
    {
      intersectPt.z = 1;
    }
  }

  public static void Main()
  {
    PathsD solution = new PathsD();
    PathsD subject = new PathsD();
    subject.Add(Clipper.MakePath(new double[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));

    ClipperD clipperD = new ClipperD();
    MyCallbacks cb = new MyCallbacks();
    clipperD.ZCallback = cb.MyCallbackD;
    clipperD.AddSubject(subject);
    clipperD.Execute(ClipType.Union, FillRule.NonZero, solution);


    Console.WriteLine(solution.ToString(0)); 

    SvgWriter svg= new SvgWriter(FillRule.NonZero);
    SvgUtils.AddSubject(svg, subject);
    SvgUtils.AddSolution(svg, solution, false);

    PathsD ellipses = new PathsD();
    for (int i = 0; i < solution[0].Count; i++)
    {
      if (solution[0][i].z == 1)
        ellipses.Add(Clipper.Ellipse(
          new PointD(solution[0][i].x, solution[0][i].y), 4));
    }
    svg.AddClosedPaths(ellipses, 0x20FF0000, 0xFFFF0000, 1);
    svg.SaveToFile("usingz.svg", 300, 300);
    OpenFileWithDefaultApp("usingz.svg");
  }

  public static void OpenFileWithDefaultApp(string filename)
  {
    string path = Path.GetFullPath(filename);
    if (!File.Exists(path)) return;
    Process p = new Process() 
    { 
      StartInfo = new ProcessStartInfo(path) { UseShellExecute = true } 
    };
    p.Start();
  }

}
