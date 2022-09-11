using System;
using System.Collections.Generic;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Jobs;

namespace Clipper2Lib.Benchmark
{
  using Path64 = List<Point64>;
  using Paths64 = List<List<Point64>>;

  public class FastConfig : ManualConfig
    {
      public FastConfig()
      {
        Add(DefaultConfig.Instance); 
        AddJob(Job.Default
            .WithLaunchCount(1)
            .WithWarmupCount(1) 
            .WithIterationCount(1)
        );
      }
    }

    [MemoryDiagnoser]
    [Config(typeof(FastConfig))] //comment out for marginally more accurate results
    public class Benchmarks
    {
        private Paths64 _subj;
        private Paths64 _clip;
        private Paths64 _solution;
        private const int DisplayWidth = 800;
        private const int DisplayHeight = 600;

        [Params(1000, 2000, 3000, 4000, 5000/*, 6000, 7000, 8000*/)]
        public int EdgeCount { get; set; }

        [GlobalSetup]
        public void GlobalSetup()
        {
            Random rand = new Random();

            _subj = new Paths64();
            _clip = new Paths64();
            _solution = new Paths64();

            _subj.Add(MakeRandomPath(DisplayWidth, DisplayHeight, EdgeCount, rand));
            _clip.Add(MakeRandomPath(DisplayWidth, DisplayHeight, EdgeCount, rand));
        }

        [Benchmark]
        public void Intersection_N()
        {
            Clipper64 c = new Clipper64();
            c.AddSubject(_subj);
            c.AddClip(_clip);
            c.Execute(ClipType.Intersection, FillRule.NonZero, _solution);
        }

/*
        [Benchmark]
        public void Union_N()
        {
            Clipper c = new Clipper();
            c.AddSubject(_subj);
            c.AddClip(_clip);
            c.Execute(ClipType.Union, FillRule.NonZero, _solution);
        }

        [Benchmark]
        public void Difference_N()
        {
            Clipper c = new Clipper();
            c.AddSubject(_subj);
            c.AddClip(_clip);
            c.Execute(ClipType.Difference, FillRule.NonZero, _solution);
        }

        [Benchmark]
        public void Xor_N()
        {
            Clipper c = new Clipper();
            c.AddSubject(_subj);
            c.AddClip(_clip);
            c.Execute(ClipType.Xor, FillRule.NonZero, _solution);
        }
*/
        private static Point64 MakeRandomPt(int maxWidth, int maxHeight, Random rand)
        {
            long x = rand.Next(maxWidth);
            var y = rand.Next(maxHeight);
            return new Point64(x, y);
        }

        public static Path64 MakeRandomPath(int width, int height, int count, Random rand)
        {
            Path64 result = new Path64(count);
            for (int i = 0; i < count; ++i)
                result.Add(MakeRandomPt(width, height, rand));
            return result;
        }
    }
}
