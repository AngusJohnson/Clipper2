//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#include "ClipFileLoad.h"
#include <sstream>

using namespace std;
using namespace Clipper2Lib;

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

void GetPaths(ifstream& source, Paths64& paths)
{
  while (true)
  {
    string line;
    stringstream::pos_type last_read_line_pos = source.tellg();
    if (getline(source, line) && GetPath(line, paths))
      continue;
    last_read_line_pos -= 1; // workaround for LF vs LFCR (#764)
    source.seekg(last_read_line_pos, ios_base::beg);
    break;
  }
}

bool LoadTestNum(ifstream &source, int test_num,
  Paths64 &subj, Paths64 &subj_open, Paths64 &clip,
  int64_t& area, int64_t& count, ClipType &ct, FillRule &fr)
{
  string line;
  area = 0; count = 0;
  if (test_num <= 0) test_num = 1;
  source.seekg(0, ios_base::beg);
  subj.clear(); subj_open.clear(); clip.clear();

  while (getline(source, line))
  {
    if (test_num)
    {
      if (line.find("CAPTION:") != string::npos) --test_num;
      continue;
    }

    if (line.find("CAPTION:") != string::npos) break; // ie don't go beyond current test

    else if (line.find("INTERSECTION") != string::npos)
      ct = ClipType::Intersection;
    else if (line.find("UNION") != string::npos)
      ct = ClipType::Union;
    else if (line.find("DIFFERENCE") != string::npos)
      ct = ClipType::Difference;
    else if (line.find("XOR") != string::npos)
      ct = ClipType::Xor;
    else if (line.find("EVENODD") != string::npos)
      fr = FillRule::EvenOdd;
    else if (line.find("NONZERO") != string::npos)
      fr = FillRule::NonZero ;
    else if (line.find("POSITIVE") != string::npos)
      fr = FillRule::Positive;
    else if (line.find("NEGATIVE") != string::npos)
      fr = FillRule::Negative;
    else if (line.find("SOL_AREA") != string::npos)
    {
      string::const_iterator s_it, s_end = line.cend();
      s_it = (line.cbegin() + 10);
      GetInt(s_it, s_end, area);
    }
    else if (line.find("SOL_COUNT") != string::npos)
    {
      string::const_iterator s_it, s_end = line.cend();
      s_it = (line.cbegin() + 11);
      GetInt(s_it, s_end, count);
    }
    else if (line.find("SUBJECTS_OPEN") != string::npos)
    {
      GetPaths(source, subj_open);
    }
    else if (line.find("SUBJECTS") != string::npos)
    {
      GetPaths(source, subj);
    }
    else if (line.find("CLIPS") != string::npos)
    {
      GetPaths(source, clip);
    }
  }
  return !test_num;
}
