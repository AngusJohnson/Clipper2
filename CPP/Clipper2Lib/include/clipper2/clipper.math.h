/*********************************************************************************
* Author    :  Angus Johnson                                                     *
* Date      :  24 March 2024                                                     *
* Website   :  http://www.angusj.com                                             *
* Copyright :  Angus Johnson 2010-2024                                           *
* Purpose   :  Possible customization point to avoid platform dependent results  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                              *
*********************************************************************************/

#ifndef CLIPPER_MATH_H_
#define CLIPPER_MATH_H_

#include <cmath>

namespace Clipper2Lib {

inline double Sin(double x)
{
	return std::sin(x);
}

inline double Cos(double x)
{
	return std::cos(x);
}

inline double ACos(double x)
{
	return std::acos(x);
}

inline double Log10(double x)
{
	return std::log10(x);
}

inline double ATan2(double x, double y)
{
	return std::atan2(x, y);
}

// See https://stackoverflow.com/a/32436148/359538
// This overload must not cause overflow or underflow at intermediate stages of the computation.
inline double HypotSafe(double x, double y)
{
	return std::hypot(x, y);
}

// See https://stackoverflow.com/a/32436148/359538
// This overload can cause overflow or underflow at intermediate stages of the computation.
inline double HypotUnsafe(double x, double y)
{	
	return std::sqrt(x * x + y * y);
}
}  // end Clipper2Lib namespace
#endif /* CLIPPER_MATH_H_ */