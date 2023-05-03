#include <iostream>

#include "clipper2/clipper.h"
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
  ClipperOffset co;
  double radius = 5000.0;
  Paths64 subject = { Ellipse(Rect64(-radius, -radius, radius, radius), 200) };
  co.AddPaths(subject, JoinType::Miter, EndType::Polygon);

  co.SetDeltaCallback([radius](const Path64& path, size_t pt_idx) {
    return double(path[pt_idx].x / radius * path[pt_idx].y / radius) * 1000;
  });

  //  solution
  Paths64 solution;
  co.Execute(1.0, solution);

  SvgWriter svg;
  SvgAddSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "variadic_offset1.svg", 400, 300);
  System("variadic_offset1.svg");
}

void test2() {

  ClipperOffset co;
  int64_t const scale = 10;
  double delta = 10 * scale;

  co.SetDeltaCallback([delta](const Path64& path, size_t pt_idx) {
    // gradually scale down the offset to minimum 25% of delta
    double high = static_cast<double>(path.size() -1) * 1.25;
    return (high - pt_idx)/ high * delta;
  });

  Path64 ellipse = Ellipse(Rect64(0, 0, 200 * scale, 180 * scale));
  size_t el_size = ellipse.size() * 0.9;
  ellipse.resize(el_size);
  Paths64 subject = { ellipse };

  co.AddPaths(subject, JoinType::Miter, EndType::Round);
  Paths64 solution;
  co.Execute(1.0, solution);
  //solution = SimplifyPaths(solution, 0.5 * scale);

  SvgWriter svg;
  SvgAddOpenSubject(svg, subject, FillRule::NonZero);
  SvgAddSolution(svg, solution, FillRule::NonZero, false);
  SvgSaveToFile(svg, "variadic_offset2.svg", 400, 300);
  System("variadic_offset2.svg");
}

int test3() {
  Clipper2Lib::ClipperOffset co;

  auto pth = Clipper2Lib::MakePath({
      0,
      0,
      50,
      0,
      50,
      100,
      0,
      100,
      0,
      0,
  });
  co.AddPath(pth, Clipper2Lib::JoinType::Miter, Clipper2Lib::EndType::Polygon);

  co.SetDeltaCallback([](const Clipper2Lib::Path64 &path, size_t pt_idx) {
    auto pt = path[pt_idx];
    auto delta = (pt.x ^ 2 + pt.y ^ 2) / 10.0;
    std::cout << "Point: " << pt.x << ", " << pt.y << ", delta:" << delta
              << std::endl;
    return delta;
  });

  //  solution
  Clipper2Lib::Paths64 solution;
  co.Execute(1.0, solution);

  std::cout << "Solution: " << std::endl;
  for (auto &pth : solution) {
    for (auto &pt : pth) {
      std::cout << pt.x << ", " << pt.y << std::endl;
    }
  }

  return 0;
}

int main() {
  test1();
  test2();
  test3();
  return 0;
}