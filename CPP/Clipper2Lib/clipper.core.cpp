/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  28 April 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Core Clipper Library structures and functions                   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#include <cstdlib>
#include <cmath>
#include <vector>
#include <string>
#include "clipper.core.h"

namespace Clipper2Lib {

	using StrConstIter = std::string::const_iterator;

	inline bool GetInt(StrConstIter& iter, const StrConstIter end_iter, int64_t& val)
	{
		val = 0;
		bool is_neg = *iter == '-';
		if (is_neg) ++iter;
		std::string::const_iterator start_iter = iter;
		while (iter != end_iter &&
			((*iter >= '0') && (*iter <= '9')))
		{
			val = val * 10 + ((int64_t)(*iter++) - '0');
		}
		if (is_neg) val = -val;
		return (iter != start_iter);
	}

	inline bool GetFloat(StrConstIter& iter, const StrConstIter end_iter, double& val)
	{
		val = 0;
		bool is_neg = *iter == '-';
		if (is_neg) ++iter;
		int dec_pos = -1;
		std::string::const_iterator start_iter = iter;
		while (iter != end_iter && (*iter == '.' ||
			((*iter >= '0') && (*iter <= '9'))))
		{
			if (*iter == '.')
			{
				if (dec_pos >= 0) return false;
				dec_pos = 0;
				++iter;
				continue;
			}

			if (dec_pos >= 0) dec_pos++;
			val = val * 10 + ((int64_t)(*iter++) - '0');
		}
		if (iter == start_iter || dec_pos == 0) return false;
		if (dec_pos > 0)
			val *= std::pow(10, -dec_pos);
		return true;
	}

	inline void SkipWhiteSpace(StrConstIter& iter, const StrConstIter end_iter)
	{
		while (iter != end_iter && *iter <= ' ') ++iter;
	}

	inline void SkipSpacesWithOptionalComma(StrConstIter& iter, const StrConstIter end_iter)
	{
		int comma_cnt = 0;
		while (iter != end_iter && (*iter == ' ' || *iter == ','))
		{
			if (*iter == ',')
			{
				if (comma_cnt > 0) return;
				++comma_cnt;
			}
			++iter;
		}
	}

	Path64 MakePath(const std::string& s)
	{
		Path64 result;
		StrConstIter s_iter = s.cbegin();
		SkipWhiteSpace(s_iter, s.cend());
		while (s_iter != s.cend())
		{
			int64_t y = 0, x = 0;
			if (!GetInt(s_iter, s.cend(), x)) break;
			SkipSpacesWithOptionalComma(s_iter, s.cend());
			if (!GetInt(s_iter, s.cend(), y)) break;
			result.push_back(Point64(x, y));
			SkipSpacesWithOptionalComma(s_iter, s.cend());
		}
		return result;
	}

	PathD MakePathD(const std::string& s)
	{
		PathD result;
		StrConstIter s_iter = s.cbegin();
		SkipWhiteSpace(s_iter, s.cend());
		while (s_iter != s.cend())
		{
			double y = 0, x = 0;
			if (!GetFloat(s_iter, s.cend(), x)) break;
			SkipSpacesWithOptionalComma(s_iter, s.cend());
			if (!GetFloat(s_iter, s.cend(), y)) break;
			result.push_back(PointD(x, y));
			SkipSpacesWithOptionalComma(s_iter, s.cend());
		}
		return result;
	}

}  //namespace
