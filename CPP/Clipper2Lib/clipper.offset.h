/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  28 April 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Polygon offsetting                                              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_OFFSET_H_
#define CLIPPER_OFFSET_H_

#include "clipper.core.h"

namespace Clipper2Lib {

enum class JoinType { Square, Round, Miter };

enum class EndType {Polygon, Joined, Butt, Square, Round};
//Butt   : offsets both sides of a path, with square blunt ends
//Square : offsets both sides of a path, with square extended ends
//Round  : offsets both sides of a path, with round extended ends
//Joined : offsets both sides of a path, with joined ends
//Polygon: offsets only one side of a closed path

class PathGroup {
public:
	PathsD paths_in_;
	PathsD paths_out_;
	PathD path_;
	bool is_reversed = false;
	JoinType join_type;
	EndType end_type;
	PathGroup(const PathsD &paths, JoinType join_type, EndType end_type):
		paths_in_(paths), join_type(join_type), end_type(end_type) {}
};

template<typename ClipperFlags>
class ClipperOffset {
private:
	double delta_ = 0.0;
	double temp_lim_ = 0.0;
	double steps_per_rad_ = 0.0;
	PathD norms;
	std::vector<PathGroup> groups_;
	JoinType join_type_ = JoinType::Square;
	
	double arc_tolerance_ = 0.0;
	bool merge_groups_ = true;
	double miter_limit_ = 0.0;
	//int rounding_precision_ = 0;
	double min_len_sqr = 0;

	void DoSquare(PathGroup& group, const PathD& path, size_t j, size_t k);
	void DoMiter(PathGroup& group, const PathD& path, size_t j, size_t k, double cos_a);
	void DoRound(PathGroup& group, const PointD& pt, const PointD& norm1, const PointD& norm2, double angle);
	void BuildNormals(const PathD& path);
	void OffsetPolygon(PathGroup& group, PathD& path);
	void OffsetOpenJoined(PathGroup& group, PathD& path);
	void OffsetOpenPath(PathGroup& group, PathD& path, EndType endType);
	void OffsetPoint(PathGroup& group, PathD& path, size_t j, size_t& k);
	void DoGroupOffset(PathGroup &group, double delta);
public:
	ClipperOffset(double miter_limit = 2.0, 
		double arc_tolerance = 0.0) :
		miter_limit_(miter_limit), arc_tolerance_(arc_tolerance) {};

	~ClipperOffset() { Clear(); };

	void AddPath(const PathD &p, JoinType jt_, EndType et_);
	void AddPaths(const PathsD &p, JoinType jt_, EndType et_);
	void Clear() { groups_.clear(); norms.clear(); };
	
	PathsD Execute(double delta);

	//ArcTolerance: needed for rounded offsets (See offset_triginometry2.svg)
	void ArcTolerance(double arc_tolerance) { arc_tolerance_ = arc_tolerance; }
	//MergeGroups: A path group is one or more paths added via the AddPath or
	//AddPaths methods. By default these path groups will be offset
	//independently of other groups and this may cause overlaps (intersections).
	//However, when MergeGroups is enabled, any overlapping offsets will be
	//merged (via a clipping union operation) to remove overlaps.
	void MergeGroups(bool merge_groups) { merge_groups_ = merge_groups; }
};

template<typename ClipperFlags = DefaultClipperFlags>
inline Paths64 InflatePaths(const Paths64& paths, double delta, JoinType jt, EndType et);

template<typename ClipperFlags = DefaultClipperFlags>
inline PathsD InflatePaths(const PathsD& paths, double delta, JoinType jt, EndType et);


/////////////////////////////////////////////////////////////////////////////////////
//                                 Implementation                                  //
/////////////////////////////////////////////////////////////////////////////////////

namespace offset::detail {

using Clipper2Lib::detail::Construct;
using Clipper2Lib::detail::EqualsXY;

static constexpr const double default_arc_tolerance = 0.25;
static constexpr const double floating_point_tolerance = 1e-12;
//------------------------------------------------------------------------------
// Miscellaneous methods
//------------------------------------------------------------------------------

inline PointD ConstructD(double x, double y)
{
	return detail::Construct<PointD>(x, y);
}

inline int GetLowestPolygonIdx(const PathsD& paths)
{
	int lp_idx = -1;
	auto lp = ConstructD(0, 0);
	for (size_t i = 0; i < static_cast<int>(paths.size()); ++i)
		if (paths[i].size() > 0) {
			lp_idx = (int)i;
			lp = paths[i][0];
			break;
		}
	if (lp_idx < 0) return lp_idx;

	for (int i = lp_idx; i < static_cast<int>(paths.size()); ++i)
	{
		PathD p = paths[i];
		for (size_t j = 0; j < p.size(); j++) {
			if (p[j].y > lp.y || (p[j].y == lp.y && p[j].x < lp.x)) {
				lp_idx = i;
				lp = p[j];
			}
		}
	}
	return lp_idx;
}

inline PointD GetUnitNormal(const PointD pt1, const PointD pt2)
{
	double dx, dy, inverse_hypot;
	if (detail::EqualsXY(pt1, pt2)) return ConstructD(0.0, 0.0);
	dx = pt2.x - pt1.x;
	dy = pt2.y - pt1.y;
	inverse_hypot = 1.0 / hypot(dx, dy);
	dx *= inverse_hypot;
	dy *= inverse_hypot;
	return ConstructD(dy, -dx);
}

inline bool IsFullOpenEndType(EndType et)
{
	return (et != EndType::Polygon) && (et != EndType::Joined);
}

} // namespace offset::detail

//------------------------------------------------------------------------------
// ClipperOffset methods
//------------------------------------------------------------------------------

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::AddPath(const PathD &path, JoinType jt_, EndType et_)
{
	PathsD paths;
	paths.push_back(path);
	AddPaths(paths, jt_, et_);
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::AddPaths(const PathsD &paths, JoinType jt_, EndType et_)
{
	if (paths.size() == 0) return;
	groups_.push_back(PathGroup(paths, jt_, et_));
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::BuildNormals(const PathD& path) 
{
	using namespace offset::detail;

	norms.clear();
	norms.reserve(path.size());
	if (path.size() == 0) return;
	PathD::const_iterator path_iter, path_last_iter = --path.cend();
	for (path_iter = path.cbegin(); path_iter != path_last_iter; ++path_iter)
		norms.push_back(GetUnitNormal(*path_iter,*(path_iter +1)));
	norms.push_back(GetUnitNormal(*path_last_iter, *(path.cbegin())));
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::DoSquare(PathGroup& group, const PathD& path, size_t j, size_t k)
{
	using namespace offset::detail;

	if (delta_ > 0)
	{
		group.path_.push_back(ConstructD(
			path[j].x + delta_ * (norms[k].x - norms[k].y),
			path[j].y + delta_ * (norms[k].y + norms[k].x)));
		group.path_.push_back(ConstructD(
			path[j].x + delta_ * (norms[j].x + norms[j].y),
			path[j].y + delta_ * (norms[j].y - norms[j].x)));
	}
	else
	{
		group.path_.push_back(ConstructD(
			path[j].x + delta_ * (norms[k].x + norms[k].y),
			path[j].y + delta_ * (norms[k].y - norms[k].x)));
		group.path_.push_back(ConstructD(
			path[j].x + delta_ * (norms[j].x - norms[j].y),
			path[j].y + delta_ * (norms[j].y + norms[j].x)));
	}
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::DoMiter(PathGroup& group, const PathD& path, size_t j, size_t k, double cos_a)
{
	using namespace offset::detail;

	double q = delta_ / (cos_a + 1);
	group.path_.push_back(ConstructD(
		path[j].x + (norms[k].x + norms[j].x) * q,
		path[j].y + (norms[k].y + norms[j].y) * q));
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::DoRound(PathGroup& group, const PointD& pt,
	const PointD& norm1, const PointD& norm2, double angle)
{
	using namespace offset::detail;

	//even though angle may be negative this is a convex join
	PointD pt2 = ConstructD(norm2.x * delta_, norm2.y * delta_);
	int steps = static_cast<int>(std::round(steps_per_rad_ * std::abs(angle) + 0.5));
	group.path_.push_back(ConstructD(pt.x + pt2.x, pt.y + pt2.y));
	if (steps > 0)
	{
		double step_sin = std::sin(angle / steps);
		double step_cos = std::cos(angle / steps);
		for (int i = 0; i < steps; i++)
		{
			pt2 = ConstructD(pt2.x * step_cos - step_sin * pt2.y,
				pt2.x * step_sin + pt2.y * step_cos);
			group.path_.push_back(ConstructD(pt.x + pt2.x, pt.y + pt2.y));
		}
	}
	pt2.x = norm1.x * delta_;
	pt2.y = norm1.y * delta_;
	group.path_.push_back(ConstructD(pt.x + pt2.x, pt.y + pt2.y));
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::OffsetPoint(PathGroup& group, PathD& path, size_t j, size_t& k)
{
	using namespace offset::detail;

	//A: angle between adjoining edges (on left side WRT winding direction).
	//A == 0 deg (or A == 360 deg): collinear edges heading in same direction
	//A == 180 deg: collinear edges heading in opposite directions (i.e. a 'spike')
	//sin(A) < 0: convex on left.
	//cos(A) > 0: angles on both left and right sides > 90 degrees
	double sin_a = norms[k].x * norms[j].y - norms[j].x * norms[k].y;
	if (sin_a > 1.0) sin_a = 1.0;
	else if (sin_a < -1.0) sin_a = -1.0;

	if (sin_a * delta_ < 0) // a concave offset
	{
		PointD p1 = ConstructD(
			path[j].x + norms[k].x * delta_,
			path[j].y + norms[k].y * delta_);
		PointD p2 = ConstructD(
			path[j].x + norms[j].x * delta_,
			path[j].y + norms[j].y * delta_);
		group.path_.push_back(p1);
		if (!NearEqual(p1, p2, min_len_sqr))
		{
			group.path_.push_back(path[j]); //this aids with clipping removal later
			group.path_.push_back(p2);
		}
	}
	else
	{
		double cos_a = DotProduct(norms[j], norms[k]);
		switch (join_type_)
		{
		case JoinType::Miter:
			if (1 + cos_a < temp_lim_) DoSquare(group, path, j, k);
			else DoMiter(group, path, j, k, cos_a);
			break;
		case JoinType::Square:
			if (cos_a >= 0) DoMiter(group, path, j, k, cos_a);
			else DoSquare(group, path, j, k);
			break;
		default:
			DoRound(group, path[j], norms[j], norms[k], std::atan2(sin_a, cos_a));
			break;
		}
	}
	k = j;
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::OffsetPolygon(PathGroup& group, PathD& path)
{
	group.path_.clear();
	for (size_t i = 0, j = path.size() -1; i < path.size(); j = i, ++i)
		OffsetPoint(group, path, i, j);
	group.paths_out_.push_back(group.path_);
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::OffsetOpenJoined(PathGroup& group, PathD& path)
{
	OffsetPolygon(group, path);
	std::reverse(path.begin(), path.end());
	BuildNormals(path);
	OffsetPolygon(group, path);
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::OffsetOpenPath(PathGroup& group, PathD& path, EndType end_type)
{
	using namespace offset::detail;

	group.path_.clear();
	for (size_t i = 1, j = 0; i < path.size() -1; j = i, ++i)
		OffsetPoint(group, path, i, j);
	size_t j = norms.size() - 1, k = j - 1;
	norms[j] = ConstructD(-norms[k].x, -norms[k].y);

	switch (end_type)
	{
	case EndType::Butt:
		group.path_.push_back(ConstructD(
			path[j].x + norms[k].x * delta_,
			path[j].y + norms[k].y * delta_));
		group.path_.push_back(ConstructD(
			path[j].x - norms[k].x * delta_,
			path[j].y - norms[k].y * delta_));
		break;
	case EndType::Round:
		if constexpr (ClipperFlags::reverse_orientation)
			DoRound(group, path[j], norms[j], norms[k], PI);
		else
			DoRound(group, path[j], norms[j], norms[k], -PI);
		break;
	default:
		DoSquare(group, path, j, k);
		break;
	}

	//reverse normals ...
	for (size_t i = k; i > 0; i--)
		norms[i] = ConstructD(-norms[i - 1].x, -norms[i - 1].y);
	norms[0] = ConstructD(-norms[1].x, -norms[1].y);

	for (size_t i = k; i > 0; i--)
		OffsetPoint(group, path, i, j);

	//now cap the start ...
	switch (end_type)
	{
	case EndType::Butt:
		group.path_.push_back(ConstructD(
			path[0].x + norms[1].x * delta_,
			path[0].y + norms[1].y * delta_));
		group.path_.push_back(ConstructD(
			path[0].x - norms[1].x * delta_,
			path[0].y - norms[1].y * delta_));
		break;
	case EndType::Round:
		if constexpr (ClipperFlags::reverse_orientation)
			DoRound(group, path[0], norms[0], norms[1], PI);
		else
			DoRound(group, path[0], norms[0], norms[1], -PI);
		break;
	default:
		DoSquare(group, path, 0, 1);
		break;
	}

	group.paths_out_.push_back(group.path_);
}

template<typename ClipperFlags>
void ClipperOffset<ClipperFlags>::DoGroupOffset(PathGroup& group, double delta)
{
	using namespace offset::detail;

	if (group.end_type != EndType::Polygon) delta = std::abs(delta) / 2;
	bool isClosedPaths = !IsFullOpenEndType(group.end_type);

	if (isClosedPaths)
	{
		//the lowermost polygon must be an outer polygon. So we can use that as the
		//designated orientation for outer polygons (needed for tidy-up clipping)
		int lowestIdx = GetLowestPolygonIdx(group.paths_in_);
		if (lowestIdx < 0) return;
		if (Area<PointD, ClipperFlags>(group.paths_in_[lowestIdx]) < 0)
		{
			//this is more efficient than literally reversing paths
			group.is_reversed = true;
			delta = -delta;
		}
	}

	if constexpr (ClipperFlags::reverse_orientation)
		delta_ = delta;
	else
		delta_ = -delta;
	double absDelta = std::abs(delta_);
	join_type_ = group.join_type;

	double arcTol = (arc_tolerance_ > floating_point_tolerance ? arc_tolerance_
		: std::log10(2 + absDelta) * default_arc_tolerance); //empirically derived

//calculate a sensible number of steps (for 360 deg for the given offset
	if (group.join_type == JoinType::Round || group.end_type == EndType::Round)
	{
		steps_per_rad_ = PI / std::acos(1 - arcTol / absDelta) / (PI *2);
	}

	bool is_closed_path = !IsFullOpenEndType(group.end_type);
	PathsD::const_iterator path_iter;
	for(path_iter = group.paths_in_.cbegin(); path_iter != group.paths_in_.cend(); ++path_iter)
	{
		PathD path = StripNearEqual(*path_iter, min_len_sqr, is_closed_path);
		size_t cnt = path.size();
		if (cnt == 0 || (cnt < 3 && is_closed_path)) continue;

		if (cnt == 1) //single point - only valid with open paths
		{
			group.path_ = PathD();
			//single vertex so build a circle or square ...
			if (group.end_type == EndType::Round)
			{
				DoRound(group, path[0], ConstructD(1.0, 0.0), ConstructD(-1.0, 0.0), PI *2);
			}
			else
			{
				group.path_.reserve(4);
				group.path_.push_back(ConstructD(path[0].x - delta_, path[0].y - delta_));
				group.path_.push_back(ConstructD(path[0].x + delta_, path[0].y - delta_));
				group.path_.push_back(ConstructD(path[0].x + delta_, path[0].y + delta_));
				group.path_.push_back(ConstructD(path[0].x - delta_, path[0].y + delta_));
			}
			group.paths_out_.push_back(group.path_);
		}
		else
		{
			BuildNormals(path);
			if (group.end_type == EndType::Polygon) OffsetPolygon(group, path);
			else if (group.end_type == EndType::Joined) OffsetOpenJoined(group, path);
			else OffsetOpenPath(group, path, group.end_type);
		}
	}

	if (!merge_groups_)
	{
		//clean up self-intersections ...
		ClipperD c(0);
		c.PreserveCollinear = false;
		c.AddSubject(group.paths_out_);
		if (group.is_reversed)
			c.Execute(ClipType::Union, FillRule::Negative, group.paths_out_);
		else
			c.Execute(ClipType::Union, FillRule::Positive, group.paths_out_);
	}
}

template<typename ClipperFlags>
PathsD ClipperOffset<ClipperFlags>::Execute(double delta)
{
	using namespace offset::detail;

	PathsD result = PathsD();
	min_len_sqr = default_arc_tolerance;
	if (std::abs(delta) < min_len_sqr)
	{
		//just copy paths since the delta is inconsequential
		std::vector<PathGroup>::const_iterator groups_citer;
		for (groups_citer = groups_.cbegin(); groups_citer != groups_.cend(); ++groups_citer)
		{
			PathsD::const_iterator paths_iter;
			for (paths_iter = groups_citer->paths_in_.cbegin();
				paths_iter != groups_citer->paths_in_.cend(); ++paths_iter)
				result.push_back(*paths_iter);
			return result;
		}
	}

	temp_lim_ = miter_limit_ <= 1 ? 2.0 : 2.0 / (miter_limit_ * miter_limit_);

	std::vector<PathGroup>::iterator groups_iter;
	for (groups_iter = groups_.begin(); groups_iter != groups_.end(); ++groups_iter)
	{
		DoGroupOffset(*groups_iter, delta);
		PathsD::const_iterator paths_citer;
		for (paths_citer = groups_iter->paths_out_.cbegin();
			paths_citer != groups_iter->paths_out_.cend(); ++paths_citer)
			result.push_back(*paths_citer);
	}

	if (merge_groups_ && groups_.size() > 0)
	{
		//clean up self-intersections ...
		ClipperD c(0);
		c.PreserveCollinear = false;
		c.AddSubject(result);
		if (groups_[0].is_reversed)
			c.Execute(ClipType::Union, FillRule::Negative, result);
		else
			c.Execute(ClipType::Union, FillRule::Positive, result);
	}
	return result;
}

template<typename ClipperFlags>
inline Paths64 InflatePaths(const Paths64& paths, double delta, JoinType jt, EndType et)
{
	using namespace offset::detail;

	const int precision = 2;
	const double scale = std::pow(10, precision);
	ClipperOffset<ClipperFlags> clip_offset;
	clip_offset.AddPaths(Paths64ToPathsD(paths, scale), jt, et);
	PathsD tmp = clip_offset.Execute(delta * scale);
	tmp = StripNearEqual(tmp, Sqr(scale), !IsFullOpenEndType(et));
	return PathsDToPaths64(tmp, 1 / scale);
}

template<typename ClipperFlags>
inline PathsD InflatePaths(const PathsD& paths, double delta, JoinType jt, EndType et)
{
	ClipperOffset<ClipperFlags> clip_offset;
	clip_offset.AddPaths(paths, jt, et);
	return clip_offset.Execute(delta);
}

} // namespace Clipper2Lib

#endif /* CLIPPER_OFFSET_H_ */
