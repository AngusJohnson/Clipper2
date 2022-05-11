/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  13 May 2022                                                     *
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
		arc_tolerance_(arc_tolerance), miter_limit_(miter_limit) {};

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

}
#endif /* CLIPPER_OFFSET_H_ */
