#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/ClipFileLoad.h"

TEST(Clipper2Tests, TestFromTextFile) {
    std::ifstream ifs("../../../Tests/Tests.txt");
    ASSERT_TRUE(ifs);
    ASSERT_TRUE(ifs.good());

    int test_number = 1;

    while (true) {
        Clipper2Lib::Paths64 subject, subject_open, clip;
        Clipper2Lib::Paths64 solution, solution_open;
        Clipper2Lib::ClipType ct;
        Clipper2Lib::FillRule fr;
        int64_t area, count;

        if (LoadTestNum(ifs, test_number, false, subject, subject_open, clip, area, count, ct, fr)) 
        {
            Clipper2Lib::Clipper64 c;
            c.AddSubject(subject);
            c.AddOpenSubject(subject_open);
            c.AddClip(clip);
            c.Execute(ct, fr, solution, solution_open);

            const int64_t area2 = static_cast<int64_t>(Area(solution));
            const int64_t count2 = solution.size() + solution_open.size();
            const int64_t count_diff = std::abs(count2 - count);
            const int64_t area_diff = std::abs(area2 - area);
            const double relative_count_diff = count ? count_diff / static_cast<double>(count): 0;
            const double relative_area_diff = area ? area_diff / static_cast<double>(area): 0;

            // TODO: perhaps it would be better to specify the individual tolerances in the text file
            if (test_number < 7 || test_number == 8 || test_number == 10) {
                EXPECT_EQ(count, count2);
                EXPECT_EQ(area, area2);
            }
            else if (test_number < 14) {
                EXPECT_EQ(count, count2);
                EXPECT_LE(count_diff, 1);
                EXPECT_LE(relative_count_diff, 0.01);
            }
            else if (test_number == 23) {
                EXPECT_EQ(count, count2);
                EXPECT_LE(area_diff, 2);
            }
            else if (test_number == 27) {
                EXPECT_EQ(count_diff, 2);
                EXPECT_EQ(area, area2);
            }
            else if (test_number == 53 || test_number == 54) {
                EXPECT_EQ(count, count2);
                EXPECT_LE(relative_area_diff, 0.0056);
            }
            else if (test_number == 64 || test_number == 94) {
                EXPECT_EQ(count, count2);
                EXPECT_LE(relative_area_diff, 0.014);
            }
            else if (test_number == 66) {
                EXPECT_EQ(count, count2);
                EXPECT_LE(relative_area_diff, 0.022);
            }
            else if (test_number == 102) {
                EXPECT_LE(count_diff, 1);
                EXPECT_EQ(area_diff, 0);
            }
            else if (test_number < 160) {
                EXPECT_LE(count_diff, 2);
                if (count > 0) EXPECT_LE(relative_count_diff, 0.02);
                else           EXPECT_EQ(count2, 0);
                if (area > 0)  EXPECT_LE(relative_area_diff, 0.035);
                else           EXPECT_EQ(area, 0);
            }
            else if (test_number == 183) {
                EXPECT_LE(count_diff, 1);
                EXPECT_EQ(area, area2);
            }
            else if (test_number == 202) {
#ifndef REVERSE_ORIENTATION
              // with REVERSE_ORIENTATION, both 
              // polygons will appear to have negative 
              // orientation and will be ignored
              EXPECT_EQ(count, count2);
              EXPECT_EQ(area, area2);
#endif 
            }
            else {
                EXPECT_LE(count_diff, 8);
                EXPECT_LE(relative_count_diff, 0.1);
                EXPECT_LE(relative_area_diff, 0.0005);
            }
            ++test_number;
        }
        else {
            break;
        }
    }

    //EXPECT_GE(test_number, 188);
    EXPECT_GE(test_number, 10);
}