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

//#define REVERSE_ORIENTATION
//#define USINGZ

// Point ------------------------------------------------------------------------

template <typename T>
struct Point {
	using coordinate_type = T;
	T x;
	T y;

	explicit Point() {}

#ifdef USINGZ
	T z;

	explicit Point() : x(0), y(0), z(0) {};

	template <typename T2>
	explicit Point(const T2 x_ = 0, const T2 y_ = 0) :
		x(static_cast<T>(x_)), y(static_cast<T>(y_)), z(0) { };

	template <typename T2>
	explicit Point<T>(const Point<T2>& p)
	{
		if (std::numeric_limits<T>::is_integer &&
			!std::numeric_limits<T2>::is_integer)
		{
			x = static_cast<T>(std::round(p.x));
			y = static_cast<T>(std::round(p.y));
			z = static_cast<T>(std::round(p.z));
		}
		else
		{
			x = static_cast<T>(p.x);
			y = static_cast<T>(p.y);
			z = static_cast<T>(p.z);
		}
	}

	friend std::ostream& operator<<(std::ostream& os, const Point& point)
	{
		os << point.x << "," << point.y << "," << point.z;
		return os;
	}

#else

/*
	explicit Point() : x(0), y(0) {};

	template <typename T2>
	explicit Point(const T2 x_ = 0, const T2 y_ = 0) :
		x(static_cast<T>(x_)), y(static_cast<T>(y_)) {};

	template <typename T2>
	explicit Point<T>(const Point<T2>& p)
	{
		if (std::numeric_limits<T>::is_integer &&
			!std::numeric_limits<T2>::is_integer)
		{
			x = static_cast<T>(std::round(p.x));
			y = static_cast<T>(std::round(p.y));
		}
		else
		{
			x = static_cast<T>(p.x);
			y = static_cast<T>(p.y);
		}
	}
*/
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

#endif
/*
	friend bool operator==(const Point &a, const Point &b) 
	{
		return a.x == b.x && a.y == b.y;
	}

	friend bool operator!=(const Point& a, const Point& b)
	{
		return !(a == b);
	}

	Point operator * (const double scale) const
	{
		return Point(x * scale, y * scale);
	}

	inline Point<T> operator-() const
	{
		return Point<T>(-x,-y);
	}

	inline Point operator+(const Point &b) const
	{
		return Point(x+b.x, y+b.y);
	}

	inline Point operator-(const Point &b) const
	{
		return Point(x-b.x, y-b.y);
	}
*/
};

//    template <>
//    struct geometry_concept<Slic3r::Point> { using type = point_concept; };

	// Losely modeled after boost::polygon::point_traits
	template <typename PointType>
	struct point_traits {
		typedef PointType point_type;
		typedef typename point_type::coordinate_type coordinate_type;

		static coordinate_type get(const point_type& point, int i) {
			assert(i == 0 || i == 1);
			return i == 0 ? point.x : point.y;
		}
	};

	template <typename PointType>
	struct point_mutable_traits {
		typedef PointType point_type;
		typedef typename point_type::coordinate_type coordinate_type;

		static void set(point_type& point, int i, coordinate_type value) {
			assert(i == 0 || i == 1);
			(i == 0 ? point.x : point.y) = value;
		}

		static point_type construct(coordinate_type x, coordinate_type y) {
			return point_type::construct(x, y);
		}
	};

	template <typename PointType>
	inline bool xy_equals(const PointType& p1, const PointType& p2) {
		return point_traits<PointType>::get(p1, 0) == point_traits<PointType>::get(p2, 0) && point_traits<PointType>::get(p1, 1) == point_traits<PointType>::get(p2, 1);
	}

using Point64 = Point<int64_t>;
using PointD = Point<double>;
using Path64 = std::vector<Point64>;
using PathD = std::vector< PointD>;
using Paths64 = std::vector< Path64>;
using PathsD = std::vector< PathD>;

template<typename T>
inline double Sqr(T val)
{
	return static_cast<double>(val) * static_cast<double>(val);
}

inline bool NearEqual(const Point<double>& p1, const Point<double>& p2, double max_dist_sqrd)
{
	return Sqr(p1.x - p2.x) + Sqr(p1.y - p2.y) < max_dist_sqrd;
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
		result.push_back(point_mutable_traits<Point64>::construct(static_cast<int64_t>((*path_iter).x * scale),
			static_cast<int64_t>((*path_iter).y * scale)));
	return result;
}

inline PathD Path64ToPathD(const Path64& path, double scale = 1)
{
	PathD result;
	result.reserve(path.size());
	Path64::const_iterator path_iter;
	for (path_iter = path.cbegin(); path_iter != path.cend(); ++path_iter)
		result.emplace_back(point_mutable_traits<PointD>::construct(point_traits<Point64>::get(*path_iter, 0) * scale, point_traits<Point64>::get(*path_iter, 1) * scale));
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


template <typename T>
inline double CrossProduct(const Point<T>& pt1, const Point<T>& pt2, const Point<T>& pt3) {
	return (static_cast<double>(pt2.x - pt1.x) * static_cast<double>(pt3.y - 
		pt2.y) - static_cast<double>(pt2.y - pt1.y) * static_cast<double>(pt3.x - pt2.x));
}

template <typename T>
inline double DotProduct(const Point<T>& pt1, const Point<T>& pt2, const Point<T>& pt3) {
	return (static_cast<double>(pt2.x - pt1.x) * static_cast<double>(pt3.x - pt2.x) + 
		static_cast<double>(pt2.y - pt1.y) * static_cast<double>(pt3.y - pt2.y));
}

template <typename T>
inline double DotProduct(const Point<T>& vec1, const Point<T>& vec2)
{
	return static_cast<double>(vec1.x * vec2.x) + static_cast<double>(vec1.y * vec2.y);
}

template <typename T>
inline double DistanceSqr(const Point<T> pt1, const Point<T> pt2) {
	return std::pow(pt1.x - pt2.x, 2.0) + std::pow(pt1.y - pt2.y, 2.0);
}

template <typename T>
inline double DistanceFromLineSqrd(const Point<T> &pt, const Point<T> &ln1, const Point<T> &ln2)
{
	//perpendicular distance of point (x³,y³) = (Ax³ + By³ + C)/Sqrt(A² + B²)
	//see http://en.wikipedia.org/wiki/Perpendicular_distance
	double A = static_cast<double>(ln1.y - ln2.y);
	double B = static_cast<double>(ln2.x - ln1.x);
	double C = A * ln1.x + B * ln1.y;
	C = A * pt.x + B * pt.y - C;
	return (C * C) / (A * A + B * B);
}

template <typename T>
bool NearCollinear(const Point<T> &pt1, const Point<T> &pt2, const Point<T> &pt3, double sin_sqrd_min_angle_rads)
{
	double cp = std::abs(CrossProduct(pt1, pt2, pt3));
	return (cp * cp) / (DistanceSqr(pt1, pt2) * DistanceSqr(pt2, pt3)) < sin_sqrd_min_angle_rads;
}

inline double Area(const Path64& path)
{
	if (path.size() == 0) return 0.0;
	double a = 0.0;
	Path64::const_iterator path_iter, path_iter_last = --path.cend();
	for (path_iter = path.cbegin(); path_iter != path.cend();
		path_iter_last = path_iter, ++path_iter)
	{
		a += static_cast<double>(path_iter_last->y - path_iter->y) *
			(path_iter_last->x + path_iter->x);
	}
#ifdef REVERSE_ORIENTATION
	return a * -0.5;
#else
	return a * 0.5;
#endif
}

inline double Area(const Paths64& paths)
{
	double a = 0.0;
	for (Paths64::const_iterator paths_iter = paths.cbegin();
		paths_iter != paths.cend(); ++paths_iter)
	{
		a += Area(*paths_iter);
	}
	return a;
}

inline double Area(const PathD& path)
{
	{
		if (path.size() == 0) return 0.0;
		double a = 0.0;
		PathD::const_iterator path_iter, path_iter_last = --path.cend();
		for (path_iter = path.cbegin(); path_iter != path.cend();
			path_iter_last = path_iter, ++path_iter)
		{
			a += static_cast<double>(path_iter_last->y - path_iter->y) *
				(path_iter_last->x + path_iter->x);
		}
#ifdef REVERSE_ORIENTATION
		return a * -0.5;
#else
		return a * 0.5;
#endif
	}
}

inline double Area(const PathsD& paths)
{
	double a = 0.0;
	for (PathsD::const_iterator paths_iter = paths.cbegin();
		paths_iter != paths.cend(); ++paths_iter)
	{
		a += Area(*paths_iter);
	}
	return a;
}

inline bool IsClockwise(const Path64& poly)
{
	return Area(poly) >= 0;
}

inline bool IsClockwise(const PathD& poly)
{
	return Area(poly) >= 0;
}

Path64 MakePath(const std::string& s);
PathD MakePathD(const std::string& s);

}  //namespace

#endif  // CLIPPER_CORE_H
