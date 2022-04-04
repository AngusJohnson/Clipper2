using System.Diagnostics;
using Clipper2Lib;
using Path64 = System.Collections.Generic.List<Clipper2Lib.Point64>;
using Paths64 = System.Collections.Generic.List<System.Collections.Generic.List<Clipper2Lib.Point64>>;

namespace Clipper2LibExample
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Stopwatch sw = Stopwatch.StartNew();
            //code main entry 
            Paths64 subj = new Paths64();
            subj.Add(ClipperFunc.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
            Paths64 clip = new Paths64();
            clip.Add(ClipperFunc.MakePath(new int[] { 80, 50, 69, 73, 43, 79, 23, 63, 23, 37, 43, 21, 69, 27 }));
            /*Paths64 solution1 = ClipperFunc.Union(subj, null, FillRule.EvenOdd);
            Paths64 solution2 = ClipperFunc.Union(subj, null, FillRule.NonZero);
            Paths64 solution3 = ClipperFunc.Union(subj, clip, FillRule.NonZero);
            Paths64 solution4 = ClipperFunc.Intersect(subj, clip, FillRule.NonZero);
            Paths64 solution5 = ClipperFunc.Intersect(subj, clip, FillRule.EvenOdd);

            subj.Clear();
            subj.Add(ClipperFunc.MakePath(new int[] { 93, 50, 77, 84, 40, 92, 11, 69, 11, 31, 40, 8, 77, 16 }));
            Paths64 solution6 = ClipperFunc.InflatePaths(subj, 5.0, JoinType.Miter, EndType.Polygon);
            Paths64 solution7 = ClipperFunc.InflatePaths(subj, -5.0, JoinType.Miter, EndType.Polygon);
            Paths64 solution8 = ClipperFunc.InflatePaths(subj, 10.0, JoinType.Miter, EndType.Square);*/

            while (sw.ElapsedMilliseconds < 10_000)
            {
                Paths64 solution9 = ClipperFunc.InflatePaths(subj, 10.0, JoinType.Miter, EndType.Joined);
            }
        }
    }
}
