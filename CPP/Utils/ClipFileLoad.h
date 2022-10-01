//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#ifndef CLIPPER_TEST_LOAD_H
#define CLIPPER_TEST_LOAD_H

#include <fstream>
#include <string>
#include "clipper2/clipper.h"
#ifndef _WIN32
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

inline bool FileExists(const std::string& name)
{
  struct stat buffer;
  return (stat(name.c_str(), &buffer) == 0);
}

bool LoadTestNum(std::ifstream& source, int test_num,
  Clipper2Lib::Paths64& subj, Clipper2Lib::Paths64& subj_open, Clipper2Lib::Paths64& clip,
  int64_t& area, int64_t& count, Clipper2Lib::ClipType& ct, Clipper2Lib::FillRule& fr);

static bool LoadTest(std::ifstream& source,
  Clipper2Lib::Paths64& subj, Clipper2Lib::Paths64& subj_open, Clipper2Lib::Paths64& clip,
  int64_t& area, int64_t& count, Clipper2Lib::ClipType& ct, Clipper2Lib::FillRule& fr)
{
  return LoadTestNum(source, 1, subj, subj_open, clip, area, count, ct, fr);
}

#endif //CLIPPER_TEST_LOAD_H
