//------------------------------------------------------------------------------
// Functions load clipping operations from text files
//------------------------------------------------------------------------------

#ifndef CLIPPER_TEST_SAVE_H
#define CLIPPER_TEST_SAVE_H

#include "ClipFileLoad.h"

namespace Clipper2Lib {

  bool SaveTest(const std::string& filename, bool append,
    const PathsI* subj, const PathsI* subj_open, const PathsI* clip, 
    Integer area, Integer count, ClipType ct, FillRule fr);

} //end namespace

#endif //CLIPPER_TEST_SAVE_H
