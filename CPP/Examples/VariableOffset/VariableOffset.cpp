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

  int64_t const scale = 10;
  double delta = 10 * scale;

  ClipperOffset co;
  co.SetDeltaCallback([delta](const Path64& path,
    const PathD& path_norms, size_t curr_idx, size_t prev_idx)
    {
      // gradually scale down the offset to a minimum of 25% of delta
      double high = static_cast<double>(path.size() - 1) * 1.25;
      return (high - curr_idx) / high * delta;
    });

  Paths64 subject{ Ellipse(Rect64(0, 0, 200 * scale, 180 * scale)) };
  subject[0].resize(subject[0].size() * 0.9);

  co.AddPaths(subject, JoinType::Miter, EndType::Round);
  Paths64 solution;
  co.Execute(1.0, solution);

  std::string filename = "test1.svg";
  SvgWriter svg;
  SvgAddOpenSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400);
  System(filename);
}

void test2() {

  int64_t const scale = 10;
  double delta = 10 * scale;

  ClipperOffset co;
  co.SetDeltaCallback([delta](const Path64& path,
    const PathD& path_norms, size_t curr_idx, size_t prev_idx) {
      // calculate offset based on distance from the middle of the path
      double mid_idx = static_cast<double>(path.size()) / 2.0;
      return delta * (1.0 - 0.70 * (std::fabs(curr_idx - mid_idx) / mid_idx));
    });

  Paths64 subject{ Ellipse(Rect64(0, 0, 200 * scale, 180 * scale)) };
  subject[0].resize(subject[0].size() * 0.9);

  co.AddPaths(subject, JoinType::Miter, EndType::Round);
  Paths64 solution;
  co.Execute(1.0, solution);

  std::string filename = "test2.svg";
  SvgWriter svg;
  SvgAddOpenSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400);
  System(filename);
}

void test3() {

  double radius = 5000.0;
  Paths64 subject = { Ellipse(Rect64(-radius, -radius, radius, radius), 200) };

  ClipperOffset co;
  co.AddPaths(subject, JoinType::Miter, EndType::Polygon);

  co.SetDeltaCallback([radius](const Path64& path,
    const PathD& path_norms, size_t curr_idx, size_t prev_idx) {
      // when multiplying the x & y of edge unit normal vectors, the value will be 
      // largest (0.5) when edges are at 45 deg. and least (-0.5) at negative 45 deg.
      double delta = path_norms[curr_idx].y * path_norms[curr_idx].x;
      return radius * 0.5 + radius * delta;
    });

  //  solution
  Paths64 solution;
  co.Execute(1.0, solution);

  std::string filename = "test3.svg";
  SvgWriter svg;
  SvgAddSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, filename, 400, 400);
  System(filename);
}

void test4() {

  int64_t const scale = 100;
  Paths64 solution;
  Paths64 subject = { Ellipse(ScaleRect<int64_t,int64_t>(Rect64(10, 10, 50, 50), scale)) };

  ClipperOffset co;
  co.AddPaths(subject, JoinType::Round, EndType::Round);
  co.Execute(
    [scale](const Path64& path,
      const PathD& path_norms, size_t curr_idx, size_t prev_idx) {
        //double vertex_sin_a = CrossProduct(path_norms[curr_idx], path_norms[prev_idx]);
        //double vertex_cos_a = DotProduct(path_norms[curr_idx], path_norms[prev_idx]);
        //double vertex_angle = std::atan2(vertex_sin_a, vertex_cos_a);
        //double edge_angle = std::atan2(path_norms[curr_idx].y, path_norms[curr_idx].x);
        double sin_edge = path_norms[curr_idx].y;
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

  Paths64 solution;
  Paths64 subject = { MakePath({0,0, 20,0, 40,0, 60,0, 80,0, 100,0}) };

  ClipperOffset co;
  co.AddPaths(subject, JoinType::Round, EndType::Butt);
  co.Execute(
    [](const Path64& path,
      const PathD& path_norms, size_t curr_idx, size_t prev_idx) {
        return double(curr_idx * curr_idx + 10); }
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