using BenchmarkDotNet.Running;

namespace Clipper2Lib.Benchmark
{
    public class Program
    {        
        public static void Main()
        {
            BenchmarkRunner.Run<Benchmarks>();
        }
    }
}