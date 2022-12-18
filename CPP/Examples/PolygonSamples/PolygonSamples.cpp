#include <cstdlib>
#include <string>
#include <chrono> 
#include <random>
 
#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/ClipFileLoad.h"
#include "../../Utils/ClipFileSave.h"

using namespace Clipper2Lib;

const int display_width = 800, display_height = 600;

void DoLoopThruPolygons(int start = 0, int end = 0);
void System(const std::string &filename);

int main()
{  
  std::cout.imbue(std::locale(""));
  srand((unsigned)time(0));

  DoLoopThruPolygons(87,90); //polygons: 1..194

  std::cout << std::endl;
//#ifdef _DEBUG
  std::string s;
  std::cout << "Press Enter to continue" << std::endl;
  std::getline(std::cin, s);
//#endif
  return 0;
}

void DoLoopThruPolygons(int start, int end)
{
  Paths64 subject, subject_open, clip, solution, solution_open;
  int64_t stored_area, stored_count;
  ClipType ct;
  FillRule fr;
  bool do_all = (start == 0 && end == 0);
  if (do_all) { start = 1; end = 0xFFFF; }
  else if (end == 0) end = start;

  std::ifstream ifs("Polygons.txt");
  for (int test_number = start; test_number <= end; ++test_number)
  {
    if (!LoadTestNum(ifs, test_number,
      subject, subject_open, clip, stored_area, stored_count, ct, fr)) break;
    Clipper64 c64;
    c64.AddSubject(subject);
    c64.AddOpenSubject(subject_open);
    c64.AddClip(clip);
    if (!c64.Execute(ct, fr, solution, solution_open)) return;

    //int area = (int)Area(solution);
    //int count = (int)(solution.size());
    //std::cout << test_number -1 << " " << count << " " << area << std::endl;
    //solution = BooleanOp(ct, fr, subject, clip);
    if (do_all) continue;

    SvgWriter svg;
    SvgAddSubject(svg, subject, fr);
    SvgAddClip(svg, clip, fr);
    if (fr == FillRule::Negative) 
      for (auto& path: solution) std::reverse(path.begin(), path.end());
    SvgAddSolution(svg, solution, fr, false);
    SvgAddCaption(svg, std::to_string(test_number), 20, 20);
    std::string filename = "poly" + std::to_string(test_number - 1) + ".svg";
    SvgSaveToFile(svg, filename, 800, 600, 10);
    System(filename);
  }
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
