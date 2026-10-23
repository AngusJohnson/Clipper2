/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  3 May 2026                                                      *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2026                                         *
* Purpose   :  Build-time check that the public headers compile cleanly with   *
*              CLIPPER2_NO_IOSTREAM defined.                                   *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************/

#include "clipper2/clipper.h"

namespace {
Clipper2Lib::Paths64 _p64;
Clipper2Lib::Rect64  _r64;
}  // namespace
