#include <gtest/gtest.h>
#include "clipper2/clipper.offset.h"
#include "ClipFileLoad.h"
#include <algorithm>

TEST(Clipper2Tests, TestOffsets) {

    std::ifstream ifs("Offsets.txt");

    for (int test_number = 1; test_number <= 2; ++test_number)
    {
        Clipper2Lib::ClipperOffset co;

        Clipper2Lib::Paths64 subject, subject_open, clip;
        Clipper2Lib::Paths64 solution, solution_open;
        Clipper2Lib::ClipType ct = Clipper2Lib::ClipType::None;
        Clipper2Lib::FillRule fr = Clipper2Lib::FillRule::NonZero;
        int64_t stored_area = 0, stored_count = 0;

        ASSERT_TRUE(LoadTestNum(ifs, test_number, subject, subject_open, clip, stored_area, stored_count, ct, fr));

        co.AddPaths(subject, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
        const auto outputs = co.Execute(1);

        // is the sum total area of the solution is positive
        const auto outer_is_positive = Clipper2Lib::Area(outputs) > 0;

        // there should be exactly one exterior path
        const auto is_positive_func = Clipper2Lib::IsPositive<int64_t>;
        const auto is_positive_count = std::count_if(
          outputs.begin(), outputs.end(), is_positive_func);
        const auto is_negative_count = 
          outputs.size() - is_positive_count;
        if (outer_is_positive)
          EXPECT_EQ(is_positive_count, 1);
        else
          EXPECT_EQ(is_negative_count, 1);
    }
}