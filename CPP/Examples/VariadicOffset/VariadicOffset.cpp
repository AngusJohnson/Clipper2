#include <iostream>

#include "clipper2/clipper.h"

int main() {
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