//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#include <string>
#include <fstream>
#include <sstream>

#include "../../Clipper2Lib/clipper.h"

bool GetNumericValue(const std::string& line, std::string::const_iterator &s_it, int64_t &value);

bool GetPath(const std::string& line, Clipper2Lib::Paths64& paths);

bool GetPaths(std::stringstream& ss, Clipper2Lib::Paths64& paths);

bool GetTestNum(std::ifstream &source, int test_num, bool seek_from_start,
  Clipper2Lib::Paths64 &subj, Clipper2Lib::Paths64 &subj_open, Clipper2Lib::Paths64 &clip, 
  int64_t& area, int64_t& count, Clipper2Lib::ClipType &ct, Clipper2Lib::FillRule &fr);
