using Clipper2Lib;
using System.Collections.Generic;

namespace Clipper2LibExample
{
  using Paths64 = List<List<Point64>>;
  internal class Program
  {
    static void Main(string[] args)
    {
      Paths64 subj = new Paths64
            {
                ClipperFunc.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 })
            };
      Paths64 clip = new Paths64
            {
                ClipperFunc.MakePath(new int[] { 80, 50, 69, 73, 43, 79, 23, 63, 23, 37, 43, 21, 69, 27 })
            };

      SimpleClipperSvgWriter svg = new SimpleClipperSvgWriter(FillRule.EvenOdd);

      Paths64 solution1 = ClipperFunc.Union(subj, null, FillRule.EvenOdd);
      svg.FillRule = FillRule.EvenOdd;
      svg.AddText("Sample 1 - union; even", 0, 0, 14);
      svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 0.8);
      svg.AddPaths(solution1, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution1.svg", 800, 600);

      Paths64 solution2 = ClipperFunc.Union(subj, null, FillRule.NonZero);
      svg.ClearAll();
      svg.FillRule = FillRule.NonZero;
      svg.AddText("Sample 2 - union; nonzero", 0, 0, 14);
      svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 0.8);
      svg.AddPaths(solution2, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution2.svg", 800, 600);

      Paths64 solution3 = ClipperFunc.Union(subj, clip, FillRule.NonZero);
      svg.ClearAll();
      svg.FillRule = FillRule.NonZero;
      svg.AddText("Sample 3 - union; nonzero", 0, 0, 14);
      svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 0.8);
      svg.AddPaths(clip, false, 0x11996600, 0x55993300, 0.8);
      svg.AddPaths(solution3, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution3.svg", 800, 600);

      Paths64 solution4 = ClipperFunc.Intersect(subj, clip, FillRule.NonZero);
      svg.ClearAll();
      svg.FillRule = FillRule.NonZero;
      svg.AddText("Sample 4 - intersect; nonzero", 0, 0, 14);
      svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 0.8);
      svg.AddPaths(clip, false, 0x11996600, 0x55993300, 0.8);
      svg.AddPaths(solution4, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution4.svg", 800, 600);

      Paths64 solution5 = ClipperFunc.Intersect(subj, clip, FillRule.EvenOdd);
      svg.ClearAll();
      svg.FillRule = FillRule.EvenOdd;
      svg.AddText("Sample 5 - intersect; evenodd", 0, 0, 14);
      svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 0.8);
      svg.AddPaths(clip, false, 0x11996600, 0x55993300, 0.8);
      svg.AddPaths(solution5, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution5.svg", 800, 600);

      subj.Clear();
      subj.Add(ClipperFunc.MakePath(new int[] { 93, 50, 77, 84, 40, 92, 11, 69, 11, 31, 40, 8, 77, 16 }));

      Paths64 solution6 = ClipperFunc.InflatePaths(subj, 5.0, JoinType.Miter, EndType.Polygon);
      svg.ClearAll();
      svg.AddText("Sample 6 - inflate:+5; end:polygon", 0, 20, 14);
      svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 1.8);
      svg.AddPaths(solution6, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution6.svg", 800, 600);

      Paths64 solution7 = ClipperFunc.InflatePaths(subj, -5.0, JoinType.Miter, EndType.Polygon);
      svg.ClearAll();
      svg.AddText("Sample 7 - inflate:-5; end:polygon", 0, 20, 14);
      svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 0.8);
      svg.AddPaths(solution7, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution7.svg", 800, 600);


      Paths64 solution8 = ClipperFunc.InflatePaths(subj, 10.0, JoinType.Miter, EndType.Square);
      svg.ClearAll();
      svg.AddText("Sample 8 - inflate:10; end:square", 0, 20, 14);
      svg.AddPaths(subj, true, 0x110066FF, 0x33000099, 1.8);
      svg.AddPaths(solution8, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution8.svg", 800, 600);

      Paths64 solution9 = ClipperFunc.InflatePaths(subj, 10.0, JoinType.Miter, EndType.Joined);
      svg.ClearAll();
      svg.AddText("Sample 9 - inflate:10; end:joined", 0, 20, 14);
      svg.AddPaths(subj, true, 0x110066FF, 0x33000099, 1.8);
      svg.AddPaths(solution9, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile("../../../solution9.svg", 800, 600);

    }
  }
}
