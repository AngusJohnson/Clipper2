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

  //DoLoopThruPolygons();         // check all
  //DoLoopThruPolygons(46);       // display one
  DoLoopThruPolygons(110,113);    // display a range

  return 0;
}

void DoLoopThruPolygons(int start, int end)
{
  Paths64 subject, subject_open, clip, solution, solution_open;
  int64_t stored_area, stored_count;
  ClipType ct;
  FillRule fr;
  bool first_fail = true;
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

    if (do_all)
    {
      double area = (double)Area(solution);
      double area_diff = (double)stored_area <= 0 ?
        0 :
        std::fabs((area / (double)stored_area) - 1.0);
      int count = (int)(solution.size());
      double count_diff = stored_count <= 0 ? 
        0 : 
        std::abs(count - stored_count)/(double)stored_count;
      if (count_diff > 0.02 || (area_diff > 0.1))
      {
        if (first_fail)
        {
          std::cout << "\nCount and area differences (expected vs measured):\n" << std::endl;
          first_fail = false;
        }
        if (count_diff > 0.02)
          std::cout << test_number << ": counts " << stored_count << " vs " << count << std::endl;
        if (area_diff > 0.1)
          std::cout << test_number << ": areas  " << stored_area << " vs " << area << std::endl;
      }
      continue;
    }
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

  if (!do_all) return;
  std::cout << std::endl;
  std::string s;
  std::cout << "Press Enter to continue" << std::endl;
  std::getline(std::cin, s);
}

void System(const std::string &filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}
