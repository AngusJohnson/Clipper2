/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  1 May 2022                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Core Clipper Library structures and functions                   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_CORE_H
#define CLIPPER_CORE_H

#include <cassert>
#include <cstdlib>
#include <cmath>
#include <vector>
#include <string>
#include <iostream>

namespace Clipper2Lib {

// ClipperFlags traits.
// Currently only used for configuring the default contour orientation.
struct DefaultClipperFlags {
	static constexpr const bool reverse_orientation = false;
};

// Point ------------------------------------------------------------------------

template <typename T>
struct Point {
	using coordinate_type = T;
	T x;
	T y;

	explicit Point() {}

	static Point construct(T x, T y)
	{
		Point p; p.x = x; p.y = y; 
		return p;
	}

	friend std::ostream& operator<<(std::ostream& os, const Point& point)
	{
		os << point.x << "," << point.y;
		return os;
	}
};

template <typename T>
struct Point3 {
	using coordinate_type = T;
	T x;
	T y;
	T z;

	explicit Point3() {}

	friend std::ostream& operator<<(std::ostream& os, const Point3& point)
	{
		os << point.x << "," << point.y << "," << point.z;
		return os;
	}

	static Point3 construct(T x, T y)
	{
		Point3 p; p.x = x; p.y = y;
		return p;
	}
};

// Losely modeled after boost::polygon::point_traits
template <typename PointType>
struct point_traits {
	typedef PointType point_type;
	typedef typename point_type::coordinate_type coordinate_type;
	static constexpr const bool has_z = false;

	static coordinate_type get(const point_type& point, int i) {
		assert(i == 0 || i == 1);
		return i == 0 ? point.x : point.y;
	}

	static void set(point_type& point, int i, coordinate_type value) {
		assert(i == 0 || i == 1);
		(i == 0 ? point.x : point.y) = value;
	}

	static point_type construct(coordinate_type x, coordinate_type y) {
		return point_type::construct(x, y);
	}
};

using Point64 = Point<int64_t>;
using PointD = Point<double>;
using Path64 = std::vector<Point64>;
using PathD = std::vector< PointD>;
using Paths64 = std::vector< Path64>;
using PathsD = std::vector< PathD>;


/////////////////////////////////////////////////////////////////////////////////////
//                                 Implementation                                  //
/////////////////////////////////////////////////////////////////////////////////////

namespace detail 
{
	template<typename PointType>
	inline auto GetX(const PointType& p)
	{
		return point_traits<PointType>::get(p, 0);
	}

	template<typename PointType>
	inline auto GetY(const PointType& p)
	{
		return point_traits<PointType>::get(p, 1);
	}

	template <typename PointType>
	inline bool EqualsXY(const PointType& p1, const PointType& p2) {
		return GetX(p1) == GetX(p2) && GetY(p1) == GetY(p2);
	}

	template <typename PointType>
	inline PointType Construct(const typename PointType::coordinate_type x, const typename PointType::coordinate_type y) {
		return point_traits<PointType>::construct(x, y);
	}
}

template<typename T>
inline double Sqr(T val)
{
	return static_cast<double>(val) * static_cast<double>(val);
}

template<typename PointType>
inline bool NearEqual(const PointType& p1, const PointType& p2, double max_dist_sqrd)
{
	using namespace detail;
	return Sqr<double>(GetX(p1) - GetX(p2)) + Sqr<double>(GetY(p1) - GetY(p2)) < max_dist_sqrd;
}

static PathD StripNearEqual(const PathD& path, double max_dist_sqrd, bool is_closed_path)
{
	if (path.size() == 0) return PathD();
	PathD result;
	result.reserve(path.size());
	PathD::const_iterator path_iter = path.cbegin();
	PointD first_pt = *path_iter++, last_pt = first_pt;
	result.push_back(first_pt);
	for (; path_iter != path.cend(); ++path_iter)
	{
		if (!NearEqual(*path_iter, last_pt, max_dist_sqrd))
		{
			last_pt = *path_iter;
			result.push_back(last_pt);
		}
	}
	if (!is_closed_path) return result;
	while (!result.empty() &&
		NearEqual(result.back(), first_pt, max_dist_sqrd)) result.pop_back();
	return result;
}

static PathsD StripNearEqual(const PathsD& paths, double max_dist_sqrd, bool is_closed_path)
{
	PathsD result;
	result.reserve(paths.size());
	for (PathsD::const_iterator paths_citer = paths.cbegin();
		paths_citer != paths.cend(); ++paths_citer)
	{
		result.push_back(StripNearEqual(*paths_citer, max_dist_sqrd, is_closed_path));
	}
	return result;
}

inline Path64 PathDToPath64(const PathD& path, double scale = 1)
{
	Path64 result;
	result.reserve(path.size());
	PathD::const_iterator path_iter;
	for (path_iter = path.cbegin(); path_iter != path.cend(); ++path_iter)
		result.push_back(point_traits<Point64>::construct(static_cast<int64_t>((*path_iter).x * scale),
			static_cast<int64_t>((*path_iter).y * scale)));
	return result;
}

inline PathD Path64ToPathD(const Path64& path, double scale = 1)
{
	PathD result;
	result.reserve(path.size());
	Path64::const_iterator path_iter;
	for (path_iter = path.cbegin(); path_iter != path.cend(); ++path_iter)
		result.emplace_back(point_traits<PointD>::construct(detail::GetX(*path_iter) * scale, detail::GetY(*path_iter) * scale));
	return result;
}

inline Paths64 PathsDToPaths64(const PathsD& paths, double scale = 1)
{
	Paths64 result;
	result.reserve(paths.size());
	PathsD::const_iterator paths_iter;
	for (paths_iter = paths.cbegin(); paths_iter != paths.cend(); ++paths_iter)
		result.push_back(PathDToPath64((*paths_iter), scale));
	return result;
}

inline PathsD Paths64ToPathsD(const Paths64& paths, double scale = 1)
{
	PathsD result;
	result.reserve(paths.size());
	Paths64::const_iterator paths_iter;
	for (paths_iter = paths.cbegin(); paths_iter != paths.cend(); ++paths_iter)
		result.push_back(Path64ToPathD((*paths_iter), scale));
	return result;
}

// Rect ------------------------------------------------------------------------

template <typename T>
struct Rect;

using Rect64 = Rect<int64_t>;
using RectD = Rect<double>;

template <typename T>
struct Rect {
	T left;
	T top;
	T right;
	T bottom;

	Rect() :
		left(0),
		top(0),
		right(0),
		bottom(0) {}

	Rect(T l, T t, T r, T b) :
		left(l),
		top(t),
		right(r),
		bottom(b) {}


	T Width() const { return right - left; }
	T Height() const { return bottom - top; }
	void Width(T width) { right = left + width; }
	void Height(T height) { bottom = top + height; }

	void Scale(double scale) { 
		left *= scale; 
		top *= scale;
		right *= scale;
		bottom *= scale;
	}

	bool IsEmpty() const { return bottom <= top || right <= left; };

	friend std::ostream &operator<<(std::ostream &os, const Rect<T> &rect) {
		os << "("
		   << rect.left << "," << rect.top << "," << rect.right << "," << rect.bottom
		   << ")";
		return os;
	}
};

// clipper2Exception ---------------------------------------------------------

class Clipper2Exception : public std::exception {
public:
	explicit Clipper2Exception(const char *description) :
		m_descr(description) {}
	virtual const char *what() const throw() { return m_descr.c_str(); }

private:
	std::string m_descr;
};

// Miscellaneous ------------------------------------------------------------


template <typename PointType>
inline double CrossProduct(const PointType& pt1, const PointType& pt2, const PointType& pt3) {
	using namespace detail;
	return  static_cast<double>(GetX(pt2) - GetX(pt1)) * static_cast<double>(GetY(pt3) - GetY(pt2)) 
		  - static_cast<double>(GetY(pt2) - GetY(pt1)) * static_cast<double>(GetX(pt3) - GetX(pt2));
}

template <typename PointType>
inline double DotProduct(const PointType& pt1, const PointType& pt2, const PointType& pt3) {
	using namespace detail;
	return static_cast<double>(GetX(pt2) - GetX(pt1)) * static_cast<double>(GetX(pt3) - GetX(pt2)) +
		   static_cast<double>(GetY(pt2) - GetY(pt1)) * static_cast<double>(GetY(pt3) - GetY(pt2));
}

template <typename PointType>
inline double DotProduct(const PointType& vec1, const PointType& vec2)
{
	using namespace detail;
	return static_cast<double>(GetX(vec1) * GetX(vec2)) + 
		   static_cast<double>(GetY(vec1) * GetY(vec2));
}

template <typename PointType>
inline double DistanceSqr(const PointType pt1, const PointType pt2) {
	using namespace detail;
	return Sqr(GetX(pt1) - GetX(pt2)) + 
		   Sqr(GetY(pt1) - GetY(pt2));
}

template <typename PointType>
inline double DistanceFromLineSqrd(const PointType &pt, const PointType &ln1, const PointType &ln2)
{
	//perpendicular distance of point (x³,y³) = (Ax³ + By³ + C)/Sqrt(A² + B²)
	//see http://en.wikipedia.org/wiki/Perpendicular_distance
	using namespace detail;
	double A = static_cast<double>(GetY(ln1) - GetY(ln2));
	double B = static_cast<double>(GetX(ln2) - GetX(ln1));
	double C = A * GetX(ln1) + B * GetY(ln1);
	C = A * GetX(pt) + B * GetY(pt) - C;
	return (C * C) / (A * A + B * B);
}

template <typename T>
bool NearCollinear(const Point<T> &pt1, const Point<T> &pt2, const Point<T> &pt3, double sin_sqrd_min_angle_rads)
{
	double cp = std::abs(CrossProduct(pt1, pt2, pt3));
	return (cp * cp) / (DistanceSqr(pt1, pt2) * DistanceSqr(pt2, pt3)) < sin_sqrd_min_angle_rads;
}

template<typename PointType, typename ClipperFlags>
inline double Area(const std::vector<PointType>& path)
{
	if (path.size() == 0) return 0.0;
	double a = 0.0;
	std::vector<PointType>::const_iterator path_iter, path_iter_last = --path.cend();
	for (path_iter = path.cbegin(); path_iter != path.cend();
		path_iter_last = path_iter, ++path_iter)
	{
		a += static_cast<double>(path_iter_last->y - path_iter->y) *
			(path_iter_last->x + path_iter->x);
	}
	if constexpr (ClipperFlags::reverse_orientation)
		return a * -0.5;
	else
		return a * 0.5;
}

template<typename PointType, typename ClipperFlags>
inline double Area(const std::vector<std::vector<PointType>>& paths)
{
	double a = 0.0;
	for (const auto &path : paths)
	{
		a += Area<PointType, ClipperFlags>(path);
	}
	return a;
}

template<typename PointType, typename ClipperFlags>
inline bool IsClockwise(const std::vector<PointType>& poly)
{
	return Area<PointType, ClipperFlags>(poly) >= 0;
}

inline Path64 MakePath(const std::string& s);
inline PathD MakePathD(const std::string& s);

namespace core::detail
{
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
} // namespace core::detail

inline Path64 MakePath(const std::string& s)
{
	using namespace core::detail;

	Path64 result;
	StrConstIter s_iter = s.cbegin();
	SkipWhiteSpace(s_iter, s.cend());
	while (s_iter != s.cend())
	{
		int64_t y = 0, x = 0;
		if (!GetInt(s_iter, s.cend(), x)) break;
		SkipSpacesWithOptionalComma(s_iter, s.cend());
		if (!GetInt(s_iter, s.cend(), y)) break;
		result.push_back(point_traits<Point64>::construct(x, y));
		SkipSpacesWithOptionalComma(s_iter, s.cend());
	}
	return result;
}

inline PathD MakePathD(const std::string& s)
{
	using namespace core::detail;

	PathD result;
	StrConstIter s_iter = s.cbegin();
	SkipWhiteSpace(s_iter, s.cend());
	while (s_iter != s.cend())
	{
		double y = 0, x = 0;
		if (!GetFloat(s_iter, s.cend(), x)) break;
		SkipSpacesWithOptionalComma(s_iter, s.cend());
		if (!GetFloat(s_iter, s.cend(), y)) break;
		result.push_back(point_traits<PointD>::construct(x, y));
		SkipSpacesWithOptionalComma(s_iter, s.cend());
	}
	return result;
}

}  //namespace

#endif  // CLIPPER_CORE_H
