using System;
using BenchmarkDotNet;
using BenchmarkDotNet.Attributes;
using Clipper2Lib;
using Path64 = System.Collections.Generic.List<Clipper2Lib.Point64>;
using Paths64 = System.Collections.Generic.List<System.Collections.Generic.List<Clipper2Lib.Point64>>;


namespace Clipper2Lib.Benchmark
{
    [MemoryDiagnoser]
    public class Benchmarks
    {
        private Paths64 _subj;
        private Paths64 _clip;
        private Paths64 _pathInflate;

        [GlobalSetup]
        public void GlobalSetup()
        {
            //code main entry 
            _subj = new Paths64
            {
                ClipperFunc.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 })
            };
            _clip = new Paths64
            {
                ClipperFunc.MakePath(new int[] { 80, 50, 69, 73, 43, 79, 23, 63, 23, 37, 43, 21, 69, 27 })
            };
            _pathInflate = new Paths64
            {
                ClipperFunc.MakePath(new int[] { 93, 50, 77, 84, 40, 92, 11, 69, 11, 31, 40, 8, 77, 16 })
            };
        }

        [Benchmark]
        public void Union_Null_Clip_EvenOdd()
        {
            Paths64 solution1 = ClipperFunc.Union(_subj, null, FillRule.EvenOdd);
        }

        [Benchmark]
        public void Union_Null_Clip_NonZero()
        {
            Paths64 solution2 = ClipperFunc.Union(_subj, null, FillRule.NonZero);
        }

        [Benchmark]
        public void Union_Clip_NonZero()
        {
            Paths64 solution3 = ClipperFunc.Union(_subj, _clip, FillRule.NonZero);
        }

        [Benchmark]
        public void Intersect_Clip_NonZero()
        {
            Paths64 solution4 = ClipperFunc.Intersect(_subj, _clip, FillRule.NonZero);
        }

        [Benchmark]
        public void Intersect_Clip_EvenOdd()
        {
            Paths64 solution5 = ClipperFunc.Intersect(_subj, _clip, FillRule.EvenOdd);
        }

        [Benchmark]
        public void InflatePaths_Delta5()
        {
            Paths64 solution6 = ClipperFunc.InflatePaths(_pathInflate, 5.0, JoinType.Miter, EndType.Polygon);
        }


        [Benchmark]
        public void InflatePaths_DeltaMinus5()
        {
            Paths64 solution7 = ClipperFunc.InflatePaths(_pathInflate, -5.0, JoinType.Miter, EndType.Polygon);
        }

        [Benchmark]
        public void InflatePaths_Delta10()
        {
            Paths64 solution8 = ClipperFunc.InflatePaths(_pathInflate, 10.0, JoinType.Miter, EndType.Square);
        }

        [Benchmark]
        public void InflatePaths_DeltaMinus10()
        {
            Paths64 solution9 = ClipperFunc.InflatePaths(_pathInflate, 10.0, JoinType.Miter, EndType.Joined);
        }
    }
}
