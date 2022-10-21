
#include <cstdlib>
#include <sstream>
#include <fstream>
#include <string>
#include "clipper2/clipper.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/Timer.h"

using namespace std;
using namespace Clipper2Lib;

void System(const std::string& filename);
void PressEnterToExit();
void SavePaths(const std::string& filename, const Paths64& paths);
void LoadPaths(const std::string& filename, Paths64& paths);

void DoEllipses();
void DoRectangles();
void DoRandomPoly(int count);
void MeasurePerformance(int min, int max, int step);

const int width = 1600, height = 1200, margin = 200;

int main(int argc, char* argv[])
{
  srand((unsigned)time(0));

  DoEllipses();
  DoRectangles();
  DoRandomPoly(31);
  //MeasurePerformance(500, 2500, 500);
}

Path64 MakeRandomEllipse(int minWidth, int minHeight, int maxWidth, int maxHeight,
  int maxRight, int maxBottom)
{
  int w = maxWidth > minWidth ? minWidth + rand() % (maxWidth - minWidth) : minWidth;
  int h = maxHeight > minHeight ? minHeight + rand() % (maxHeight - minHeight) : minHeight;
  int l = rand() % (maxRight - w);
  int t = rand() % (maxBottom - h);
  return Ellipse(Rect64(l, t, l + w, t + h));
}

void DoEllipses()
{
  Paths64 sub, clp, sol, store;

  const int cnt = 1000;
  Rect64 rect = Rect64(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());
  for (int i = 0; i < cnt; ++i)
    sub.push_back(MakeRandomEllipse(10, 10, 100, 100, width, height));
      
  //////////////////////////////////
  sol = RectClip(rect, sub);
  //////////////////////////////////

  FillRule fr = FillRule::EvenOdd;
  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x100066FF, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  svg.SaveToFile("rectclip1.svg", 800, 600, 0);
  System("rectclip1.svg");
}

Path64 MakeRandomRectangle(int minWidth, int minHeight, int maxWidth, int maxHeight,
  int maxRight, int maxBottom)
{
  int w = maxWidth > minWidth ? minWidth + rand() % (maxWidth - minWidth): minWidth;
  int h = maxHeight > minHeight ? minHeight + rand() % (maxHeight - minHeight): minHeight;
  int l = rand() % (maxRight - w);
  int t = rand() % (maxBottom - h);
  Path64 result;
  result.reserve(4);
  result.push_back(Point64(l, t));
  result.push_back(Point64(l+w, t));
  result.push_back(Point64(l+w, t+h));
  result.push_back(Point64(l, t+h));
  return result;
}

void DoRectangles()
{
  Paths64 sub, clp, sol, store;
  const int cnt = 1000;
  Rect64 rect = Rect64(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());
  for (int i = 0; i < cnt; ++i)
    sub.push_back(MakeRandomRectangle(10, 10, 100, 100, width, height));

  sol = RectClip(rect, sub);

  FillRule fr = FillRule::EvenOdd;
  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x100066FF, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  svg.SaveToFile("rectclip2.svg", 800, 600, 0);
  System("rectclip2.svg");
}

PathD MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  PathD result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(PointD(rand() % width, rand() % height));
  return result;
}

void DoRandomPoly(int count)
{
  PathsD sub_open, clp, sol_open, store;
  RectD rect;

  // generate random poly
  rect = RectD(margin, margin, width - margin, height - margin);
  clp.push_back(rect.AsPath());
  sub_open.push_back(MakeRandomPoly(width, height, count));

  //////////////////////////////////
  sol_open = RectClipLines(rect, sub_open);
  //////////////////////////////////

  FillRule fr = FillRule::EvenOdd;
  SvgWriter svg;
  svg.AddPaths(sub_open, true, fr, 0x0, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol_open, true, fr, 0x0, 0xFF006600, 2.2, false);
  svg.SaveToFile("rectclip3.svg", 800, 600, 0);
  System("rectclip3.svg");
}

void MeasurePerformance(int min, int max, int step)
{
  FillRule fr = FillRule::EvenOdd;
  Paths64 sub, clp, sol, store;
  Rect64 rect;

  for (int cnt = min; cnt <= max; cnt += step)
  {
    // generate random poly
    Rect64 rect = Rect64(margin, margin, width - margin, height - margin);
    clp.push_back(rect.AsPath());
    sub.clear();

    for (int i = 0; i < cnt; ++i)
      sub.push_back(MakeRandomEllipse(100, 100, 100, 100, width, height));

    std::cout << std::endl << cnt << " ellipses" << std::endl;
    {
      Timer t("Clipper64: ");
      sol = Intersect(sub, clp, fr);
    }

    {
      Timer t("RectClip: ");
      sol = RectClip(rect, sub);
    }
  }

  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x200066FF, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  svg.SaveToFile("RectClipQ.svg", 800, 600, 0);
  System("RectClipQ.svg");

  PressEnterToExit();
}


void System(const std::string& filename)
{
#ifdef _WIN32
  system(filename.c_str());
#else
  system(("firefox " + filename).c_str());
#endif
}

void SavePaths(const std::string& filename, const Paths64& paths)
{
  std::ofstream stream;
  stream.open(filename, std::ios::trunc);

  for (Paths64::const_iterator paths_it = paths.cbegin();
    paths_it != paths.cend(); ++paths_it)
  {
    //watch out for empty paths
    if (paths_it->cbegin() == paths_it->cend()) continue;
    Path64::const_iterator path_it, path_it_last;
    for (path_it = paths_it->cbegin(), path_it_last = --paths_it->cend();
      path_it != path_it_last; ++path_it)
      stream << *path_it << " ";
    stream << *path_it_last << endl;
  }
}

bool GetInt(string::const_iterator& s_it,
  const string::const_iterator& it_end, int64_t& value)
{
  value = 0;
  while (s_it != it_end && *s_it == ' ') ++s_it;
  if (s_it == it_end) return false;
  bool is_neg = (*s_it == '-');
  if (is_neg) ++s_it;
  string::const_iterator s_it2 = s_it;
  while (s_it != it_end && *s_it >= '0' && *s_it <= '9')
  {
    value = value * 10 + static_cast<int64_t>(*s_it++) - 48;
  }

  if (s_it == s_it2) return false; //no value
  //trim trailing space and a comma if present
  while (s_it != it_end && *s_it == ' ') ++s_it;
  if (s_it != it_end && *s_it == ',') ++s_it;
  if (is_neg) value = -value;
  return true;
}

bool GetPath(const string& line, Paths64& paths)
{
  Path64 p;
  int64_t x = 0, y = 0;
  string::const_iterator s_it = line.cbegin(), s_end = line.cend();
  while (GetInt(s_it, s_end, x) && GetInt(s_it, s_end, y))
    p.push_back(Point64(x, y));
  if (p.empty()) return false;
  paths.push_back(p);
  return true;
}

void LoadPaths(const std::string& filename, Paths64& paths)
{
  std::ifstream source;
  source.open(filename);
  if (!source) return;

  while (true)
  {
    string line;
    stringstream::pos_type last_read_line_pos = source.tellg();
    if (getline(source, line) && GetPath(line, paths))
      continue;
    source.seekg(last_read_line_pos, ios_base::beg);
    break;
  }
  source.close();
}

void PressEnterToExit()
{
  std::string s;
  std::cout << std::endl << "Press Enter to exit" << std::endl;
  std::getline(std::cin, s);
}



