using System.Collections.Generic;
using Clipper2Lib;

namespace Clipper2LibExample
{
  using Paths64 = List<List<Point64>>;
  internal class Program
  {

    internal static void MakeSvg(string caption, string filename,
      Paths64 subj, Paths64 openSubj, Paths64 clip,
      Paths64 solution, FillRule fill)
    {
      SimpleClipperSvgWriter svg = new SimpleClipperSvgWriter(fill);
      svg.AddText(caption, 0, 25, 14);
      if (subj != null)
        svg.AddPaths(subj, false, 0x110066FF, 0x33000099, 0.8);
      if (openSubj != null)
        svg.AddPaths(openSubj, true, 0x110066FF, 0x33000099, 1.8);
      if (clip != null)
        svg.AddPaths(clip, false, 0x11996600, 0x55993300, 0.8);
      if (solution != null)
        svg.AddPaths(solution, false, 0x4000FF00, 0x80000000, 1.2, true);
      svg.SaveToFile(filename, 800, 600);
    }

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
      MakeSvg("Sample 1 - union; even", "../../../solution1.svg",
        subj, null, null, solution1, FillRule.EvenOdd);

      Paths64 solution2 = ClipperFunc.Union(subj, null, FillRule.NonZero);
      MakeSvg("Sample 2 - union; nonzero", "../../../solution2.svg",
        subj, null, null, solution2, FillRule.NonZero);

      Paths64 solution3 = ClipperFunc.Union(subj, clip, FillRule.NonZero);
      MakeSvg("Sample 3 - union; nonzero", "../../../solution3.svg",
        subj, null, clip, solution3, FillRule.NonZero);

      Paths64 solution4 = ClipperFunc.Intersect(subj, clip, FillRule.NonZero);
      MakeSvg("Sample 4 - intersect; nonzero", "../../../solution4.svg",
        subj, null, clip, solution4, FillRule.NonZero);

      Paths64 solution5 = ClipperFunc.Intersect(subj, clip, FillRule.EvenOdd);
      MakeSvg("Sample 5 - intersect; evenodd", "../../../solution5.svg",
        subj, null, clip, solution5, FillRule.EvenOdd);

      subj.Clear();
      subj.Add(ClipperFunc.MakePath(new int[] { 93, 50, 77, 84, 40, 92, 11, 69, 11, 31, 40, 8, 77, 16 }));

      Paths64 solution6 = ClipperFunc.InflatePaths(subj, 5.0, JoinType.Miter, EndType.Polygon);
      MakeSvg("Sample 6 - inflate:+5; end:polygon", "../../../solution6.svg",
        subj, null, null, solution6, FillRule.EvenOdd);

      Paths64 solution7 = ClipperFunc.InflatePaths(subj, -5.0, JoinType.Miter, EndType.Polygon);
      MakeSvg("Sample 7 - inflate:-5; end:polygon", "../../../solution7.svg",
        subj, null, null, solution7, FillRule.EvenOdd);

      Paths64 solution8 = ClipperFunc.InflatePaths(subj, 10.0, JoinType.Miter, EndType.Square);
      MakeSvg("Sample 8 - inflate:10; end:square", "../../../solution8.svg",
        null, subj, null, solution8, FillRule.EvenOdd);

      Paths64 solution9 = ClipperFunc.InflatePaths(subj, 10.0, JoinType.Miter, EndType.Joined);
      MakeSvg("Sample 9 - inflate:10; end:joined", "../../../solution9.svg",
        null, subj, null, solution9, FillRule.EvenOdd);

    }
  }
}
