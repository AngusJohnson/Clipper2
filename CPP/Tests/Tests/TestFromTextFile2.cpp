#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/ClipFileLoad.h"

#ifdef _WIN32
#include <Windows.h> // for IsDebuggerPresent()
#endif // _WIN32

TEST(Clipper2Tests, TestFromTextFile2) {
    std::ifstream ifs("../../../Tests/Tests2.txt");
    ASSERT_TRUE(ifs);
    ASSERT_TRUE(ifs.good());
        
    Clipper2Lib::Paths64 subject, subject_open, clip;
    Clipper2Lib::PolyTree64 solution;
    Clipper2Lib::Paths64 solution_open;
    Clipper2Lib::ClipType ct;
    Clipper2Lib::FillRule fr;
    int64_t area, count;

    for (auto test_number : { 1, 2, 3 })
    {
        bool success = false;
        if (LoadTestNum(ifs, test_number, false, subject, subject_open, clip, area, count, ct, fr))
        {
            Clipper2Lib::Clipper64 c;
            c.AddSubject(subject);
            c.AddOpenSubject(subject_open);
            c.AddClip(clip);

            const auto t0 = std::chrono::steady_clock::now();

            c.Execute(ct, fr, solution, solution_open);

            const auto t1 = std::chrono::steady_clock::now();
            const auto seconds_elapsed = std::chrono::duration_cast<std::chrono::seconds>(t1 - t0).count();

#ifdef _WIN32
#ifdef NDEBUG
            if (!IsDebuggerPresent())
            {
                EXPECT_LT(seconds_elapsed, 60);
            }
#endif // NDEBUG
#endif // _WIN32

            // For the time being at least, there are no test criteria here for the results.

            success = true;
        }
        EXPECT_TRUE(success);
    }
}