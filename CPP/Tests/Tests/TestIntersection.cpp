#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"

TEST(Clipper2Tests, TestBasicIntersection) {
    Clipper2Lib::Clipper clipper;

    const Clipper2Lib::Path64 a = {
        Clipper2Lib::Point64(0, 0),
        Clipper2Lib::Point64(0, 5),
        Clipper2Lib::Point64(5, 5),
        Clipper2Lib::Point64(5, 0)
    };

    const Clipper2Lib::Path64 b = {
        Clipper2Lib::Point64(1, 1),
        Clipper2Lib::Point64(1, 6),
        Clipper2Lib::Point64(6, 6),
        Clipper2Lib::Point64(6, 1)
    };

    clipper.AddSubject({ a });
    clipper.AddClip   ({ b });

    Clipper2Lib::PolyTree64 solution;
    Clipper2Lib::Paths64 open_paths;

#ifdef REVERSE_ORIENTATION
    clipper.Execute(Clipper2Lib::ClipType::Intersection, Clipper2Lib::FillRule::Negative, solution, open_paths);
#else 
    clipper.Execute(Clipper2Lib::ClipType::Intersection, Clipper2Lib::FillRule::Positive, solution, open_paths);
#endif 

    EXPECT_EQ(open_paths.size(), 0);
    ASSERT_EQ(solution.ChildCount(), 1);
    EXPECT_EQ(solution.childs.front()->polygon.size(), 4);
}
