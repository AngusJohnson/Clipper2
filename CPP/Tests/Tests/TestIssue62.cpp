#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"

TEST(Clipper2Tests, TestIssue62) {
    Clipper2Lib::Clipper clipper;

    const Clipper2Lib::Path64 clip = {
        Clipper2Lib::Point64(-1, -1),
        Clipper2Lib::Point64(-1,  4),
        Clipper2Lib::Point64( 4,  4),
        Clipper2Lib::Point64( 4, -1)
    };

    const Clipper2Lib::Path64 subject = {
        Clipper2Lib::Point64(0, 3),
        Clipper2Lib::Point64(3, 3),
        Clipper2Lib::Point64(3, 0),
        Clipper2Lib::Point64(2, 0),
        Clipper2Lib::Point64(1, 0),
        Clipper2Lib::Point64(0, 0)
    };

    clipper.AddClip   ({ clip });
    clipper.AddSubject({ subject });

    Clipper2Lib::PolyTree64 solution;
    Clipper2Lib::Paths64 open_paths;

    clipper.Execute(Clipper2Lib::ClipType::Intersection, Clipper2Lib::FillRule::Positive, solution, open_paths);

    EXPECT_EQ(open_paths.size(), 0);
    ASSERT_EQ(solution.ChildCount(), 1);

    const auto& first_child = solution.childs.front()->polygon;
    EXPECT_NE(
        std::find(
            first_child.begin(),
            first_child.end(),
            Clipper2Lib::Point64(0, 0)
        ),
        first_child.end()
    );
}
