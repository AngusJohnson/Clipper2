#include <gtest/gtest.h>
#include "../../Clipper2Lib/clipper.h"
#include "../../Utils/ClipFileLoad.h"

using namespace Clipper2Lib;

//CheckChildrenAreInsideOwner: 
//this is fully recursive because holes can also contain children
bool CheckChildrenAreInsideOwner(const PolyPath64& outer)
{
  //nb: the outermost PolyPath has an empty polygon
  if (!outer.polygon().empty() && outer.ChildCount() > 0)
  {
    //test PointInPolygon with each child's midpoint
    for (const PolyPath64* child : outer.childs())
    {
      Rect64 child_rec = Bounds(child->polygon());
      if (PointInPolygon(child_rec.MidPoint(), 
        outer.polygon()) != PointInPolygonResult::IsInside) 
          return false;
    }
  }
  for (const PolyPath64* child : outer.childs())
    if (!CheckChildrenAreInsideOwner(*child)) return false;
  return true;
}

TEST(Clipper2Tests, TestPolytreeHoleOwnership1)
{
#ifdef _WIN32
  std::ifstream ifs("../../../Tests/PolytreeHoleOwner.txt");
#else
  std::ifstream ifs("PolytreeHoleOwner.txt");
#endif
  ASSERT_TRUE(ifs);
  ASSERT_TRUE(ifs.good());

  Paths64 subject, subject_open, clip;
  PolyTree64 solution;
  Paths64 solution_open;
  ClipType ct;
  FillRule fr;
  int64_t area, count;

  bool success = false;
  ASSERT_TRUE(LoadTestNum(ifs, 1, false, 
    subject, subject_open, clip, area, count, ct, fr));

  Clipper64 c;
  c.AddSubject(subject);
  c.AddOpenSubject(subject_open);
  c.AddClip(clip);
  c.Execute(ct, fr, solution, solution_open);

  EXPECT_TRUE(CheckChildrenAreInsideOwner(solution));

}
