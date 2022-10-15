#include <gtest/gtest.h>
#include "clipper2/clipper.h"
#include "ClipFileLoad.h"

using namespace Clipper2Lib;

TEST(Clipper2Tests, TestRectClip)
{
  Paths64 sub, clp, sol;
  Rect64 rect = Rect64(100,100, 700, 500);
  clp.push_back(rect.AsPath());

  sub.push_back(MakePath("100,100, 700,100, 700,500, 100,500"));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(sub));

  sub.clear();
  sub.push_back(MakePath("110,110, 700,100, 700,500, 100,500"));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(sub));

  sub.clear();
  sub.push_back(MakePath("90,90, 700,100, 700,500, 100,500"));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(clp));

  sub.clear();
  sub.push_back(MakePath("90,90, 710,90, 710,510, 90,510"));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(clp));

  sub.clear();
  sub.push_back(MakePath("110,110, 690,110, 690,490, 110,490"));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(Area(sol) == Area(sub));

  sub.clear();
  clp.clear();
  rect = Rect64(390, 290, 410, 310);
  clp.push_back(rect.AsPath());
  sub.push_back(MakePath("410,290 500,290 500,310 410,310"));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(sol.empty());

  sub.clear();
  sub.push_back(MakePath("430,290 470,330 390,330"));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(sol.empty());

  sub.clear();
  sub.push_back(MakePath("450,290 480,330 450,330"));
  sol = RectClip(rect, sub);
  EXPECT_TRUE(sol.empty());

  sub.clear();
  sub.push_back(MakePath("2089,662 3661,1126 4029,3038 2345,3329 2332,2629 2433,1401 2150,1263 403,1721"));
  rect = Rect64(2379, 1646, 3221, 2488);
  sol = RectClip(rect, sub);
  const auto solBounds = Bounds(sol);
  EXPECT_EQ(solBounds.Width(), rect.Width());
  EXPECT_EQ(solBounds.Height(), rect.Height());
}