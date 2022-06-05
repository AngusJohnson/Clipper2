#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/ClipFileLoad.h"

TEST(Clipper2Tests, TestFromTextFile2) {
    std::ifstream ifs("../../../Tests/Tests2.txt");
    ASSERT_TRUE(ifs);
    ASSERT_TRUE(ifs.good());
        
    Clipper2Lib::Paths64 subject, subject_open, clip;
    Clipper2Lib::Paths64 solution_open;
    Clipper2Lib::PolyTree64 solution;
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
            c.Execute(ct, fr, solution, solution_open);
            // For the time being at least, there are no test criteria here for the results.
            success = true;
        }
        EXPECT_TRUE(success);
    }
}