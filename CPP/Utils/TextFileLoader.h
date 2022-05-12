//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#include <fstream>

#include "../Clipper2Lib/clipper.h"

bool GetTestNum(std::ifstream &source, int test_num, bool seek_from_start,
  Clipper2Lib::Paths64 &subj, Clipper2Lib::Paths64 &subj_open, Clipper2Lib::Paths64 &clip, 
  int64_t& area, int64_t& count, Clipper2Lib::ClipType &ct, Clipper2Lib::FillRule &fr);
