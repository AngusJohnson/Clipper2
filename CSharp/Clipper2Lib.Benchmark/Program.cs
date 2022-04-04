using BenchmarkDotNet.Running;

namespace Clipper2Lib.Benchmark
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var summary = BenchmarkRunner.Run<Benchmarks>();
        }
    }
}