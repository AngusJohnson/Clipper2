#include <iostream>

#include "clipper2/clipper.h"
#include "clipper2/clipper.core.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/CommonUtils.h"

using namespace Clipper2Lib;

void System(const std::string& filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}

void test1() {

  Integer const scale = 10;
  Scalar delta = 10 * scale;

  ClipperOffset co;
  co.SetDeltaCallback([delta](const PathI& path,
    const PathS& path_norms, size_t curr_idx, size_t prev_idx)
    {
      // gradually scale down the offset to a minimum of 25% of delta
      Scalar high = static_cast<Scalar>(path.size() - 1) * 1.25;
      return (high - curr_idx) / high * delta;
    });

  PathsI subject{ Ellipse(RectI(0, 0, 200 * scale, 180 * scale)) };
  subject[0].resize(subject[0].size() * 0.9);

  co.AddPaths(subject, JoinType::Miter, EndType::Round);
  PathsI solution;
  co.Execute(1.0, solution);

  std::string filename = "test1.svg";
  SvgWriter svg;
  SvgAddOpenSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400);
  System(filename);
}

void test2() {

  Integer const scale = 10;
  Scalar delta = 10 * scale;

  ClipperOffset co;
  co.SetDeltaCallback([delta](const PathI& path,
    const PathS& path_norms, size_t curr_idx, size_t prev_idx) {
      // calculate offset based on distance from the middle of the path
      Scalar mid_idx = static_cast<Scalar>(path.size()) / 2.0;
      return delta * ((Scalar)1.0 - (Scalar)0.70 * (std::fabs(curr_idx - mid_idx) / mid_idx));
    });

  PathsI subject{ Ellipse(RectI(0, 0, 200 * scale, 180 * scale)) };
  subject[0].resize(subject[0].size() * 0.9);

  co.AddPaths(subject, JoinType::Miter, EndType::Round);
  PathsI solution;
  co.Execute(1.0, solution);

  std::string filename = "test2.svg";
  SvgWriter svg;
  SvgAddOpenSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400);
  System(filename);
}

void test3() {

  Scalar radius = (Scalar)5000.0;
  PathsI subject = { Ellipse(RectI(-radius, -radius, radius, radius), 200) };

  ClipperOffset co;
  co.AddPaths(subject, JoinType::Miter, EndType::Polygon);

  co.SetDeltaCallback([radius](const PathI& path,
    const PathS& path_norms, size_t curr_idx, size_t prev_idx) {
      // when multiplying the x & y of edge unit normal vectors, the value will be 
      // largest (0.5) when edges are at 45 deg. and least (-0.5) at negative 45 deg.
      Scalar delta = path_norms[curr_idx].y * path_norms[curr_idx].x;
      return radius * ((Scalar)0.5) + radius * delta;
    });

  //  solution
  PathsI solution;
  co.Execute(1.0, solution);

  std::string filename = "test3.svg";
  SvgWriter svg;
  SvgAddSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400);
  System(filename);
}

void test4() {

  Integer const scale = 100;
  PathsI solution;
  PathsI subject = { Ellipse(ScaleRect<Integer,Integer>(RectI(10, 10, 50, 50), scale)) };

  ClipperOffset co;
  co.AddPaths(subject, JoinType::Round, EndType::Round);
  co.Execute(
    [scale](const PathI& path,
      const PathS& path_norms, size_t curr_idx, size_t prev_idx) {
        //Scalar vertex_sin_a = CrossProduct(path_norms[curr_idx], path_norms[prev_idx]);
        //Scalar vertex_cos_a = DotProduct(path_norms[curr_idx], path_norms[prev_idx]);
        //Scalar vertex_angle = std::atan2(vertex_sin_a, vertex_cos_a);
        //Scalar edge_angle = std::atan2(path_norms[curr_idx].y, path_norms[curr_idx].x);
        Scalar sin_edge = path_norms[curr_idx].y;
        return Sqr(sin_edge) * 3 * scale; }
  , solution);

  std::string filename = "test4.svg";
  SvgWriter svg;
  SvgAddOpenSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400);
  System(filename);
}

void test5() {

  PathsI solution;
  PathsI subject = { MakePath({0,0, 20,0, 40,0, 60,0, 80,0, 100,0}) };

  ClipperOffset co;
  co.AddPaths(subject, JoinType::Round, EndType::Butt);
  co.Execute(
    [](const PathI& path,
      const PathS& path_norms, size_t curr_idx, size_t prev_idx) {
        return Scalar(curr_idx * curr_idx + 10); }
  , solution);

  SvgWriter svg;
  std::string filename = "test5.svg";
  SvgAddOpenSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400);
  System(filename);
}


int main() {

  test1();
  test2();
  test3();
  test4();
  test5();
  return 0;
}