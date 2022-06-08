#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.offset.h"

TEST(Clipper2Tests, TestOrientationAfterOffsetting) {
    Clipper2Lib::ClipperOffset clipper;

    const Clipper2Lib::Path64 input = {
        Clipper2Lib::Point64(0, 0),
        Clipper2Lib::Point64(0, 5),
        Clipper2Lib::Point64(5, 5),
        Clipper2Lib::Point64(5, 0)
    };

    clipper.AddPath(input, Clipper2Lib::JoinType::Round, Clipper2Lib::EndType::Polygon);

    const auto outputs = clipper.Execute(1);

    ASSERT_EQ(outputs.size(), 1);

    const auto& output = outputs.front();

    EXPECT_EQ(Clipper2Lib::IsClockwise(input), Clipper2Lib::IsClockwise(output));
}
