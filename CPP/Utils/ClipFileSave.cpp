//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#include "ClipFileSave.h"
#include <sstream>
#include <cstring>

namespace Clipper2Lib {

 using namespace std;

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
  std::vector<uint8_t> needle_;
  std::vector<uint8_t> needle_ic_;
  char *haystack_ = nullptr;
  char *current, *end, *last_found;
  bool case_sensitive_;

  void SetHayStack(std::ifstream& stream)
  {
    ClearHaystack();
    stream.seekg(0, ios::end);
    haystack_len = static_cast<size_t>(stream.tellg());
    stream.seekg(0, ios::beg);
    haystack_ = new char[haystack_len];
    stream.read(haystack_, haystack_len);
    current = haystack_;
    end = current + haystack_len;
  }

  void SetHayStack(const char* buffer, size_t buff_len)
  {
    ClearHaystack();
    this->haystack_len = buff_len;
    haystack_ = new char[buff_len];
    memcpy(haystack_, buffer, buff_len);
    current = haystack_;
    end = current + buff_len;
  }

  void Init()
  {    
    case_sensitive_ = false;
    current = nullptr; end = nullptr; last_found = nullptr;
  }

  bool FindNext_CaseSensitive()
  {
    while (current < end)
    {
      uint8_t i = shift[static_cast<unsigned>(*current)];  //compare last byte first
      if (!i)                   //last byte matches if i == 0
      {
        char* j = current - needle_len_less1;
        while (i < needle_len_less1 && needle_[i] == *(j + i)) ++i;
        if (i == needle_len_less1)
        {
          ++current;
          last_found = j;
          return true;
        }
        else
          current += jump_;
      }
      else
        current += i;
    }
    return false;
  }

  bool FindNext_IgnoreCase()
  {
    while (current < end)
    {
      uint8_t i = shift[case_table[static_cast<unsigned>(*current)]];
      if (!i)                          
      {
        char* j = current - needle_len_less1;
        while (i < needle_len_less1 &&
          needle_ic_[i] == case_table[static_cast<unsigned>(*(j + i))]) ++i;
        if (i == needle_len_less1)
        {
          ++current;
          last_found = j;
          return true;
        }
        else
          current += jump_;
      }
      else
        current += i;
    }
    return false;
  }

public:

  explicit BMH_Search(std::ifstream& stream, 
    const std::string& needle = "")
  {
    //case blind table
    for (int c = 0; c < 0x61; ++c) case_table[c] = c;
    for (int c = 0x61; c < 0x7B; ++c) case_table[c] = c - 0x20;
    for (int c = 0x7B; c < 256; ++c) case_table[c] = c;


    Init();
    SetHayStack(stream);
    if (needle.size() > 0) SetNeedle(needle);
  }

  BMH_Search(const char* haystack, size_t length, 
    const std::string& needle = "")
  {
    Init();
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
    current = haystack_; 
    last_found = nullptr;
  }

  void SetNeedle(const std::string& needle)
  {
    ClearNeedle();
    needle_len_ = static_cast<int>(needle.size());
    if (!needle_len_) return;

    //case sensitive needle
    needle_len_less1 = needle_len_ - 1;
    needle_.clear();
    needle_.reserve(needle_len_);
    for (const char& c : needle) needle_.push_back(static_cast<uint8_t>(c));
    
    //case insensitive needle
    needle_ic_ = needle_;
    for (std::vector< uint8_t>::iterator ui = needle_ic_.begin(); ui != needle_ic_.end(); ++ui)
      *ui = case_table[*ui];

    std::fill(std::begin(shift), std::begin(shift) + 256, needle_len_);
    for (uint8_t j = 0; j < needle_len_less1; ++j)
      shift[needle_[j]] = needle_len_less1 - j;

    jump_ = shift[needle_[needle_len_less1]];
    shift[needle_[needle_len_less1]] = 0;
  }

  inline void ClearNeedle()
  {
    needle_.clear();
    needle_ic_.clear();
  }

  inline void ClearHaystack()
  {
    if (haystack_) delete[] haystack_;
    haystack_ = nullptr;
  }

  void CaseSensitive(bool value) { case_sensitive_ = value; };

  bool FindNext()
  {
    if (case_sensitive_)
      return FindNext_CaseSensitive();
    else
      return FindNext_IgnoreCase();
  }

  bool FindFirst()
  {
    Reset();
    return FindNext();
  }

  inline char* Base() { return haystack_; }
  inline char* LastFound() { return last_found; }
  inline size_t LastFoundOffset() { return last_found - haystack_; }

  inline char* FindNextEndLine()
  {    
    current = last_found + needle_len_;
    while (current < end && 
      *current != char(13) && *current != char(10)) 
        ++current;
    return current;
  }

}; //BMH_Search class


void PathsToStream(const Paths64& paths, std::ostream& stream)
{
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

static bool GetInt(string::const_iterator& s_it,
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

bool SaveTest(const std::string& filename, bool append,
  const Paths64* subj, const Paths64* subj_open, const Paths64* clip, 
  int64_t area, int64_t count, ClipType ct, FillRule fr)
{
  string line;
  bool found = false;
  int last_cap_pos = 0, curr_cap_pos = 0;
  int64_t last_test_no = 0;
  if (append && FileExists(filename)) //get the number of the preceeding test
  {
    ifstream file;
    file.open(filename, std::ios::binary);
    if (!file || !file.good()) return false;
    BMH_Search bmh = BMH_Search(file, "CAPTION:");
    while (bmh.FindNext()) ;
    if (bmh.LastFound())
    {
      line.assign(bmh.LastFound()+8, bmh.FindNextEndLine());
      string::const_iterator s_it = line.cbegin(), s_end = line.cend();
      GetInt(s_it, s_end, last_test_no);
    }
  } 
  else if (FileExists(filename)) 
    remove(filename.c_str());

  ++last_test_no;

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

  source << "CAPTION: " << last_test_no <<"." << endl;
  source << "CLIPTYPE: " << cliptype_string << endl;
  source << "FILLRULE: " << fillrule_string << endl;
  source << "SOL_AREA: " << area << endl;
  source << "SOL_COUNT: " << count << endl;
  if (subj)
  {
    source << "SUBJECTS" << endl;
    PathsToStream(*subj, source);
  }
  if (subj_open)
  {
    source << "SUBJECTS_OPEN" << endl;
    PathsToStream(*subj_open, source);
  }
  if (clip && clip->size())
  {
    source << "CLIPS" << endl;
    PathsToStream(*clip, source);
  }
  source << endl;
  source.close();
  return true;
}

} //end namespace
