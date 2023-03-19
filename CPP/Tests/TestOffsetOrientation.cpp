#include <gtest/gtest.h>
#include "clipper2/clipper.offset.h"

TEST(Clipper2Tests, TestOffsettingOrientation) {
    Clipper2Lib::ClipperOffset co;

    const Clipper2Lib::Path64 input = {
        Clipper2Lib::Point64(0, 0),
        Clipper2Lib::Point64(0, 5),
        Clipper2Lib::Point64(5, 5),
        Clipper2Lib::Point64(5, 0)
    };

    co.AddPath(input, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);
    Clipper2Lib::Paths64 outputs;
    co.Execute(1, outputs);

    ASSERT_EQ(outputs.size(), 1);
    //when offsetting, output orientation should match input
    EXPECT_TRUE(Clipper2Lib::IsPositive(input) == Clipper2Lib::IsPositive(outputs[0]));
}