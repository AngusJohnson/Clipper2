#include <gtest/gtest.h>
#include "clipper2/clipper.offset.h"
#include "ClipFileLoad.h"
#include <algorithm>

TEST(Clipper2Tests, TestOffsets) {

    std::ifstream ifs("Offsets.txt");

    Clipper2Lib::ClipperOffset co;

    Clipper2Lib::Paths64 subject, subject_open, clip;
    Clipper2Lib::Paths64 solution, solution_open;
    Clipper2Lib::ClipType ct;
    Clipper2Lib::FillRule fr;
    int64_t stored_area, stored_count;    

    ASSERT_TRUE(LoadTestNum(ifs, 1, subject, subject_open, clip, stored_area, stored_count, ct, fr));

    co.AddPaths(subject, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
    const auto outputs = co.Execute(1);

    // there should be exactly one exterior path
    const auto is_hole = Clipper2Lib::IsPositive<int64_t>;
    const auto hole_count = std::count_if(outputs.begin(), outputs.end(), is_hole);
    const auto exterior_count = outputs.size() - hole_count;
    EXPECT_EQ(exterior_count, 1);
}