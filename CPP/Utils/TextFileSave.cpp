//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#include "TextFileLoad.h"
#include "TextFileSave.h"
#include <string>
#include <sstream>

using namespace std;
using namespace Clipper2Lib;

//------------------------------------------------------------------------------
// Quick and Dirty BMH Search
//------------------------------------------------------------------------------

class BMH_Search 
{

private:
  int  jump_;
  int  needle_len_, needle_len_less1, haystack_len;
  int  shift[255];
  char case_table[255];
  char *needle_ {};
  char *haystack_ {};
  char *cur, *end, *last_found;

  void SetHayStack(std::ifstream& stream)
  {
    ClearHaystack();
    stream.seekg(0, ios::end);
    haystack_len = (int)stream.tellg();
    stream.seekg(0, ios::beg);
    haystack_ = new char[haystack_len];
    stream.read(haystack_, haystack_len);
    cur = haystack_;
    end = cur + haystack_len;
  }

  void SetHayStack(const char* buffer, size_t buff_len)
  {
    ClearHaystack();
    this->haystack_len = buff_len;
    haystack_ = new char[buff_len];
    memcpy(haystack_, buffer, buff_len);
    cur = haystack_;
    end = cur + buff_len;
  }

public:

  BMH_Search(std::ifstream& textfile, const std::string pattern = "")
  {
    SetHayStack(textfile);
    if (pattern.size() > 0) SetNeedle(pattern);
  }

  BMH_Search(const char* haystack, size_t length, const std::string needle = "")
  {
    SetHayStack(haystack, length);
    if (needle.size() > 0) SetNeedle(needle);
  }

  ~BMH_Search()
  {
    ClearHaystack();
    ClearNeedle();
  }

  void Reset()
  {
    cur = haystack_; 
    last_found = NULL;
  }

  void SetNeedle(const std::string& pattern)
  {
    ClearNeedle();
    needle_len_ = (int)pattern.size();
    if (!needle_len_) return;
    needle_len_less1 = needle_len_ - 1;
    needle_ = new char[needle_len_];
    pattern.copy(needle_, needle_len_);

    for (int &i : shift) i = needle_len_;
    for (size_t j = 0; j < needle_len_less1; ++j)
      shift[(int)pattern[j]] = needle_len_less1 - j;
    jump_ = shift[(int)pattern[needle_len_less1]];
    shift[(int)pattern[needle_len_less1]] = 0;
  }

  inline void ClearNeedle()
  {
    if (needle_) delete[] needle_;
    needle_ = {};
  }

  inline void ClearHaystack()
  {
    if (haystack_) delete[] haystack_;
    haystack_ = {};
  }

  bool FindNext()
  {
    while (cur < end)
    {
      int i = shift[(int)*cur];        //compare last byte first
      if (!i)                          //last byte matches
      {
        char* j = cur - needle_len_less1;
        
        while (i < needle_len_less1 && 
          needle_[i] == *(j + i)) ++i;

        if (i == needle_len_less1)
        {
          ++cur;
          last_found = j;
          return true;
        }
        else
          cur += jump_;
      }
      else
        cur += i;
    }
    return false;
  }

  inline char* LastFound() { return last_found; }

  inline char* NextEndLine()
  {    
    cur = last_found + needle_len_;
    while (cur < end && *cur >= ' ') ++cur;
    return cur;
  }

}; //BMH_Search class


void PathsToStream(Paths64& paths, std::ostream& stream)
{
  for (Paths64::iterator paths_it = paths.begin(); paths_it != paths.end(); ++paths_it)
  {
    //watch out for empty paths
    if (paths_it->begin() == paths_it->end()) continue;
    Path64::iterator path_it, path_it_last;
    for (path_it = paths_it->begin(), path_it_last = --paths_it->end();
      path_it != path_it_last; ++path_it)
      stream << *path_it << ", ";
    stream << *path_it_last << endl;
  }
}

bool SaveTest(const std::string& filename, bool append,
  Clipper2Lib::Paths64& subj, Clipper2Lib::Paths64& subj_open, Clipper2Lib::Paths64& clip,
  int64_t area, int64_t count, Clipper2Lib::ClipType& ct, Clipper2Lib::FillRule& fr)
{
  string line;
  bool found = false;
  int last_cap_pos = 0, curr_cap_pos = 0;
  int64_t last_text_no = 0;
  if (append && FileExists(filename)) //get the number of the preceeding test
  {
    ifstream file;
    file.open(filename, std::ios::binary);
 
    BMH_Search bmh = BMH_Search(file, "CAPTION:");
    while (bmh.FindNext()) ;
    if (bmh.LastFound())
    {
      line.assign(bmh.LastFound()+8, bmh.NextEndLine());
      string::const_iterator s_it = line.cbegin(), s_end = line.cend();
      GetInt(s_it, s_end, last_text_no);
    }
  } 
  else if (FileExists(filename)) 
    remove(filename.c_str());

  last_text_no++;

  std::ofstream source;
  if (append && FileExists(filename))
    source.open(filename, ios_base::app | ios_base::ate);
  else
    source.open(filename);

  string cliptype_string;
  switch (ct)
  {
  case ClipType::None: cliptype_string = "NONE"; break;
  case ClipType::Intersection: cliptype_string = "INTERSECTION"; break;
  case ClipType::Union: cliptype_string = "UNION"; break;
  case ClipType::Difference: cliptype_string = "DIFFERENCE"; break;
  case ClipType::Xor: cliptype_string = "XOR"; break;
  }

  string fillrule_string;
  switch (fr)
  {
    case FillRule::EvenOdd: fillrule_string = "EVENODD"; break;
    case FillRule::NonZero: fillrule_string = "NONZERO"; break;
    case FillRule::Positive: fillrule_string = "POSITIVE"; break;
    case FillRule::Negative: fillrule_string = "NEGATIVE"; break;
  }

  source << "CAPTION: " << last_text_no <<"." << endl;
  source << "CLIPTYPE: " << cliptype_string << endl;
  source << "FILLRULE: " << fillrule_string << endl;
  source << "SOL_AREA: " << area << endl;
  source << "SOL_COUNT: " << count << endl;
  source << "SUBJECTS" << endl;
  PathsToStream(subj, source);
  if (clip.size())
  {
    source << "CLIPS" << endl;
    PathsToStream(clip, source);
  }
  source << endl;
  source.close();
  return true;
}
