//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#ifndef CLIPPER_TEST_SAVE_H
#define CLIPPER_TEST_SAVE_H

#include "ClipFileLoad.h"

namespace Clipper2Lib {

  bool SaveTest(const std::string& filename, bool append,
    const Paths64* subj, const Paths64* subj_open, const Paths64* clip, 
    int64_t area, int64_t count, ClipType ct, FillRule fr);

} //end namespace

#endif //CLIPPER_TEST_SAVE_H
