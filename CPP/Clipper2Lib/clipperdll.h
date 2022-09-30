#ifndef CLIPPERDLL_H
#define CLIPPERDLL_H
/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - ver.1.0.4                                            *
* Date      :  30 September 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Core Clipper Library structures and functions                   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER2_DLL
#  if defined(_WIN32)
#    ifdef CLIPPER2_DLL_EXPORT
#      define CLIPPER2_DLL __declspec(dllexport)
#    else
#      define CLIPPER2_DLL __declspec(dllimport)
#    endif
#  else
#    if __GNUC__ >= 4
#      define CLIPPER2_DLL __attribute__((visibility("default")))
#    else
#      define CLIPPER2_DLL
#    endif
#  endif
#endif

#endif
