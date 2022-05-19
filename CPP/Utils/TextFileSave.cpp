//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#include "TextFileSave.h"
#include "TextFileLoad.h"
#include <sstream>

using namespace std;
using namespace Clipper2Lib;

//------------------------------------------------------------------------------
// Boyer Moore Horspool Search
//------------------------------------------------------------------------------

class BMH_Search 
{
private:
  uint8_t case_table[256];
  unsigned needle_len_, needle_len_less1;
  size_t haystack_len;
  uint8_t shift[256];
  uint8_t jump_;
  uint8_t* needle_;
  uint8_t* needle_ic_;
  char *haystack_;
  char *cur, *end, *last_found;

  void SetHayStack(std::ifstream& stream)
  {
    ClearHaystack();
    stream.seekg(0, ios::end);
    haystack_len = stream.tellg();
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

  void Init()
  {    
    cur = nullptr; end = nullptr; last_found = nullptr;
    //case blind table
    for (int c = 0; c < 0x61; ++c) case_table[c] = c;
    for (int c = 0x61; c < 0x7B; ++c) case_table[c] = c - 0x20;
    for (int c = 0x7B; c < 256; ++c) case_table[c] = c;
  }

  bool FindNext_CaseSensitive()
  {
    while (cur < end)
    {
      uint8_t i = shift[*cur];  //compare last byte first
      if (!i)                   //last byte matches if i == 0
      {
        char* j = cur - needle_len_less1;
        while (i < needle_len_less1 && needle_[i] == *(j + i)) ++i;
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

  bool FindNext_IgnoreCase()
  {
    while (cur < end)
    {
      uint8_t i = shift[case_table[*cur]];
      if (!i)                          
      {
        char* j = cur - needle_len_less1;
        while (i < needle_len_less1 &&
          needle_ic_[i] == case_table[*(j + i)]) ++i;
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

public:

  explicit BMH_Search(std::ifstream& stream, 
    const std::string& needle = "", bool ignore_case = true)
  {
    Init();
    IgnoreCase = ignore_case;
    SetHayStack(stream);
    if (needle.size() > 0) SetNeedle(needle);
  }

  BMH_Search(const char* haystack, size_t length, 
    const std::string& needle = "", bool ignore_case = true)
  {
    Init();
    IgnoreCase = ignore_case;
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
    last_found = nullptr;
  }

  class {
    bool value = true;
  public:
    bool& operator = (const bool& val) { return value = val; }
    operator bool() const { return value; }
  } IgnoreCase;

  void SetNeedle(const std::string& needle)
  {
    ClearNeedle();
    needle_len_ = (int)needle.size();
    if (!needle_len_) return;

    //case sensitive needle
    needle_len_less1 = needle_len_ - 1;
    needle_ = new uint8_t[needle_len_];
    needle.copy(reinterpret_cast<char*>(needle_), needle_len_);
    
    //case insensitive needle
    needle_ic_ = new uint8_t[needle_len_];
    std::memcpy(needle_ic_, needle_, needle_len_);
    uint8_t* c = needle_ic_;
    for (uint8_t i = 0; i < needle_len_; ++i)
      *c = case_table[*c++];

    std::fill(std::begin(shift), std::begin(shift) + 256, needle_len_);
    for (uint8_t j = 0; j < needle_len_less1; ++j)
      shift[needle_[j]] = needle_len_less1 - j;
    jump_ = shift[needle_[needle_len_less1]];
    shift[needle_[needle_len_less1]] = 0;
  }

  inline void ClearNeedle()
  {
    if (needle_) delete[] needle_;
    if (needle_ic_) delete[] needle_ic_;
    needle_ = nullptr;
    needle_ic_ = nullptr;
  }

  inline void ClearHaystack()
  {
    if (haystack_) delete[] haystack_;
    haystack_ = nullptr;
  }

  bool FindNext()
  {
    if (IgnoreCase)
      return FindNext_IgnoreCase();
    else
      return FindNext_CaseSensitive();
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
  int64_t area, int64_t count, Clipper2Lib::ClipType ct, Clipper2Lib::FillRule fr)
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
