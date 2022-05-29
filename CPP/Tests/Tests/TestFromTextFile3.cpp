#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/ClipFileLoad.h"

struct ResultRegion {
    ResultRegion(const Clipper2Lib::PolyPath64* exterior)
        : exterior(exterior) {}

    const Clipper2Lib::PolyPath64* const exterior;
    std::vector<const Clipper2Lib::PolyPath64*> holes;
};

std::vector<ResultRegion> ExtractResults(
    const Clipper2Lib::PolyTree64& node,
    const std::vector<ResultRegion>& previous = std::vector<ResultRegion>()
) {
    auto result = previous;

    if (node.IsHole()) {
        // associate the hole to its parent
        const Clipper2Lib::PolyPath64* const parent = node.parent();
        const auto is_parent = [&parent](const auto& i) {
            return i.exterior == parent;
        };
        const auto i = std::find_if(result.begin(), result.end(), is_parent);
        assert(i != result.end());
        if (i != result.end()) {
            i->holes.push_back(&node);
        }
    }
    else {
        if (!node.polygon.empty()) {
            result.emplace_back(&node);
        }
    }

    for (const auto* child : node.childs) {
        result = ExtractResults(*child, result);
    }

    return result;
};

TEST(Clipper2Tests, TestFromTextFile3) {
    std::ifstream ifs("../../../Tests/Tests3.txt");
    ASSERT_TRUE(ifs);
    ASSERT_TRUE(ifs.good());
        
    Clipper2Lib::Paths64 subject, subject_open, clip;
    Clipper2Lib::PolyTree64 solution;
    Clipper2Lib::Paths64 solution_open;
    Clipper2Lib::ClipType ct;
    Clipper2Lib::FillRule fr;
    int64_t area, count;

    bool success = false;
    ASSERT_TRUE(LoadTestNum(ifs, 1, false, subject, subject_open, clip, area, count, ct, fr));

    Clipper2Lib::Clipper64 c;
    c.AddSubject(subject);
    c.AddOpenSubject(subject_open);
    c.AddClip(clip);
    c.Execute(ct, fr, solution, solution_open);

    const auto results = ExtractResults(solution);

    for (const auto& result : results) {
        const auto parent_bounds = Clipper2Lib::Bounds({ result.exterior->polygon });
        for (const auto& hole : result.holes) {
            const auto hole_bounds = Clipper2Lib::Bounds({ hole->polygon });

            // the bounding rect of the hole should at least intersect the bounding rect of the parent
            EXPECT_GE(hole_bounds.right,  parent_bounds.left);
            EXPECT_LE(hole_bounds.left,   parent_bounds.right);
            EXPECT_GE(hole_bounds.bottom, parent_bounds.top);
            EXPECT_LE(hole_bounds.top,    parent_bounds.bottom);

            // moreover, the bounding rect of the hole should not extend outside the bounding rect of the parent
            EXPECT_GE(hole_bounds.left,   parent_bounds.left);
            EXPECT_LE(hole_bounds.right,  parent_bounds.right);
            EXPECT_GE(hole_bounds.top,    parent_bounds.top);
            EXPECT_LE(hole_bounds.bottom, parent_bounds.bottom);
        }
    }
}