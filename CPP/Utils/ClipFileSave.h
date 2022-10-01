//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#ifndef CLIPPER_TEST_SAVE_H
#define CLIPPER_TEST_SAVE_H

#include "clipper2/clipper.h"

bool SaveTest(const std::string& filename, bool append,
  Clipper2Lib::Paths64* subj, Clipper2Lib::Paths64* subj_open, Clipper2Lib::Paths64* clip,
  int64_t area, int64_t count, Clipper2Lib::ClipType ct, Clipper2Lib::FillRule fr);

#endif //CLIPPER_TEST_SAVE_H
