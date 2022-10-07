
#include <cstdlib>
#include <sstream>
#include <fstream>
#include <string>
#include "clipper2/clipper.h"
#include "clipper2/clipper.rectclip.h"
#include "../../Utils/clipper.svg.h"
#include "../../Utils/clipper.svg.utils.h"
#include "../../Utils/Timer.h"

using namespace std;
using namespace Clipper2Lib;

Path64 MakeRandomPoly(int width, int height, unsigned vertCnt);
void System(const std::string& filename);
void SavePaths(const std::string& filename, const Paths64& paths);
void LoadPaths(const std::string& filename, Paths64& paths);

void DoEllipses();
void DoRandomPoly();

const int width = 800, height = 600;
Paths64 sub, clp, sol, store;

int main(int argc, char* argv[])
{
  srand((unsigned)time(0));

  DoEllipses();
  DoRandomPoly();

  //std::string s;
  //std::cout << "Press Enter to continue" << std::endl;
  //std::getline(std::cin, s);
}

void DoEllipses()
{
  clp.clear();
  sub.clear();

  const int cnt = 100;
  const int radius = 100;
  Path64 ellipse = Ellipse(Rect64(0, 0, radius, radius));
  Rect64 rect = Rect64(100, 100, width - 100, height - 100);
  clp.push_back(rect.AsPath());
  for (int i = 0; i < cnt; ++i)
    sub.push_back(TranslatePath(ellipse, 
      rand() % (width - radius), rand() % (height - radius)));
  
  sol = RectClip64(rect, sub);

  FillRule fr = FillRule::EvenOdd;
  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x100066FF, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  svg.SaveToFile("rectclip1.svg", width, height, 0);
  System("rectclip1.svg");
}

void DoRandomPoly()
{
  const int cnt = 29;
  clp.clear();
  sub.clear();

  Rect64 rect = Rect64(200, 200, width - 200, height - 200);
  clp.push_back(rect.AsPath());
  sub.push_back(MakeRandomPoly(width, height, cnt));
  // save to file
  store.push_back(sub[0]);
  store.push_back(clp[0]);
  SavePaths("rectclip.txt", store);

  // alternatively, load from file
  //LoadPaths("rectclip.txt", store);
  //sub.push_back(store[0]);
  //clp.push_back(store[1]);
  //rect = Bounds(clp);

  sol = RectClip64(rect, sub);

  FillRule fr = FillRule::EvenOdd;
  SvgWriter svg;
  svg.AddPaths(sub, false, fr, 0x100066FF, 0x400066FF, 1, false);
  svg.AddPaths(clp, false, fr, 0x10FFAA00, 0xFFFF0000, 1, false);
  svg.AddPaths(sol, false, fr, 0x8066FF66, 0xFF006600, 1, false);
  svg.SaveToFile("rectclip2.svg", width, height, 0);
  System("rectclip2.svg");
}


Path64 MakeRandomPoly(int width, int height, unsigned vertCnt)
{
  Path64 result;
  result.reserve(vertCnt);
  for (unsigned i = 0; i < vertCnt; ++i)
    result.push_back(Point64(rand() % width, rand() % height));
  return result;
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
  stream.open(filename);

  for (Paths64::const_iterator paths_it = paths.cbegin();
    paths_it != paths.cend(); ++paths_it)
  {
    //watch out for empty paths
    if (paths_it->cbegin() == paths_it->cend()) continue;
    Path64::const_iterator path_it, path_it_last;
    for (path_it = paths_it->cbegin(), path_it_last = --paths_it->cend();
      path_it != path_it_last; ++path_it)
      stream << *path_it << ", ";
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


