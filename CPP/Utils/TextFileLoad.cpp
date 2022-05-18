//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#include "TextFileLoad.h"
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
    value = value * 10 + (int64_t)(*s_it++) - 48;
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

bool GetPaths(stringstream& ss, Paths64& paths)
{
  bool line_found = true;
  stringstream::pos_type pos;
  paths.clear();
  std::string line;
  while (line_found)
  {
    pos = ss.tellg();
    if (!getline(ss, line)) line_found = false;
    else if (!GetPath(line, paths)) break;
  }
  //go to the beginning of the line just read
  ss.seekg(pos, ios_base::beg);
  return line_found;
}

bool LoadTestNum(ifstream &source, int test_num, bool seek_from_start,
  Paths64 &subj, Paths64 &subj_open, Paths64 &clip, 
  int64_t& area, int64_t& count, ClipType &ct, FillRule &fr)
{
  string line;
  bool found = false;
  if (seek_from_start) source.seekg(0, ios_base::beg);
  stringstream::pos_type last_read_line_pos = source.tellg();
  while (std::getline(source, line))
  {
    size_t line_pos = line.find("CAPTION:");
    if (line_pos == string::npos) continue;

    string::const_iterator s_it = (line.cbegin() + 8), s_end = line.cend();
    int64_t num;
    if (test_num > 0 && GetInt(s_it, s_end, num))
    {
      if (num > test_num) return false;
      if (num != test_num) continue;
    }

    found = true;
    subj.clear(); subj_open.clear(); clip.clear();
    while (std::getline(source, line))
    {            
      s_end = line.cend();

      if (line.find("CAPTION:") != string::npos)
      {
        source.seekg(last_read_line_pos, ios_base::beg);
        return (!subj.empty() || !subj_open.empty() || !clip.empty());
      }
      last_read_line_pos = source.tellg();

      if (line.find("INTERSECTION") != string::npos) 
      {
        ct = ClipType::Intersection; continue;
      }
      else if (line.find("UNION") != string::npos) 
      {
        ct = ClipType::Union; continue;
      }
      else if (line.find("DIFFERENCE") != string::npos) 
      {
        ct = ClipType::Difference; continue;
      }
      else if (line.find("XOR") != string::npos) 
      {
        ct = ClipType::Xor; continue;
      }

      if (line.find("EVENODD") != string::npos) 
      {
        fr = FillRule::EvenOdd; continue;
      }
      else if (line.find("NONZERO") != string::npos) 
      {
        fr = FillRule::NonZero ; continue;
      }
      else if (line.find("POSITIVE") != string::npos) 
      {
        fr = FillRule::Positive; continue;
      }
      else if (line.find("NEGATIVE") != string::npos)
      {
        fr = FillRule::Negative; continue;
      }
      
      else if (line.find("SOL_AREA") != string::npos)
      {
        s_it = (line.cbegin() + 10);
        GetInt(s_it, s_end, area); 
        continue;
      }
      else if (line.find("SOL_COUNT") != string::npos)
      {
        s_it = (line.cbegin() + 11);
        GetInt(s_it, s_end, count);
        continue;
      }

      if (line.find("SUBJECTS_OPEN") != string::npos) 
      {
        while (getline(source, line) && GetPath(line, subj_open));
      }
      else if (line.find("SUBJECTS") != string::npos) 
      {
        while (getline(source, line) && GetPath(line, subj));
      }
      if (line.find("CLIPS") != string::npos) 
      {
        while (getline(source, line) && GetPath(line, clip));
      }
    } //inner while still lines (found)
  } //outer while still lines (not found)
  return found;
}
