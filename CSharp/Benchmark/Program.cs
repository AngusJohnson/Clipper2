using BenchmarkDotNet.Running;

#if USINGZ
namespace Clipper2ZLib.Benchmark
#else
namespace Clipper2Lib.Benchmark
#endif
{
  public static class Program
    {        
        public static void Main()
        {
            BenchmarkRunner.Run<Benchmarks>();
        }
    }
}