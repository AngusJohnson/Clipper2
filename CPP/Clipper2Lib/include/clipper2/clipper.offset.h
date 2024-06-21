/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  24 March 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  Path Offset (Inflate/Shrink)                                    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_OFFSET_H_
#define CLIPPER_OFFSET_H_

#include "clipper.core.h"
#include "clipper.engine.h"

namespace Clipper2Lib {

enum class JoinType { Square, Bevel, Round, Miter };
//Square : Joins are 'squared' at exactly the offset distance (more complex code)
//Bevel  : Similar to Square, but the offset distance varies with angle (simple code & faster)

enum class EndType {Polygon, Joined, Butt, Square, Round};
//Butt   : offsets both sides of a path, with square blunt ends
//Square : offsets both sides of a path, with square extended ends
//Round  : offsets both sides of a path, with round extended ends
//Joined : offsets both sides of a path, with joined ends
//Polygon: offsets only one side of a closed path

typedef std::function<Scalar(const PathI& path, const PathS& path_normals, size_t curr_idx, size_t prev_idx)> DeltaCallback64;

class ClipperOffset {
private:

	class Group {
	public:
		PathsI paths_in;
		int lowest_path_idx = -1;
		bool is_reversed = false;
		JoinType join_type;
		EndType end_type;
		Group(const PathsI& _paths, JoinType _join_type, EndType _end_type);
	};

	int   error_code_ = 0;
	Scalar delta_ = 0.0;
	Scalar group_delta_ = 0.0;
	Scalar temp_lim_ = 0.0;
	Scalar steps_per_rad_ = 0.0;
	Scalar step_sin_ = 0.0;
	Scalar step_cos_ = 0.0;
	PathS norms;
	PathI path_out;
	PathsI* solution = nullptr;
	PolyTreeI* solution_tree = nullptr;
	std::vector<Group> groups_;
	JoinType join_type_ = JoinType::Bevel;
	EndType end_type_ = EndType::Polygon;

	Scalar miter_limit_ = (Scalar)0.0;
	Scalar arc_tolerance_ = (Scalar)0.0;
	bool preserve_collinear_ = false;
	bool reverse_solution_ = false;

#ifdef USINGZ
	ZCallbackI zCallback64_ = nullptr;
	void ZCB(const PointI& bot1, const PointI& top1,
		const PointI& bot2, const PointI& top2, PointI& ip);
#endif
	DeltaCallback64 deltaCallback64_ = nullptr;
	size_t CalcSolutionCapacity();
	bool CheckReverseOrientation();
	void DoBevel(const PathI& path, size_t j, size_t k);
	void DoSquare(const PathI& path, size_t j, size_t k);
	void DoMiter(const PathI& path, size_t j, size_t k, Scalar cos_a);
	void DoRound(const PathI& path, size_t j, size_t k, Scalar angle);
	void BuildNormals(const PathI& path);
	void OffsetPolygon(Group& group, const PathI& path);
	void OffsetOpenJoined(Group& group, const PathI& path);
	void OffsetOpenPath(Group& group, const PathI& path);
	void OffsetPoint(Group& group, const PathI& path, size_t j, size_t k);
	void DoGroupOffset(Group &group);
	void ExecuteInternal(Scalar delta);
public:
	explicit ClipperOffset(Scalar miter_limit = 2.0,
		Scalar arc_tolerance = 0.0,
		bool preserve_collinear = false,
		bool reverse_solution = false) :
		miter_limit_(miter_limit), arc_tolerance_(arc_tolerance),
		preserve_collinear_(preserve_collinear),
		reverse_solution_(reverse_solution) { };

	~ClipperOffset() { Clear(); };

	int ErrorCode() const { return error_code_; };
	void AddPath(const PathI& path, JoinType jt_, EndType et_);
	void AddPaths(const PathsI& paths, JoinType jt_, EndType et_);
	void Clear() { groups_.clear(); norms.clear(); };
	
	void Execute(Scalar delta, PathsI& paths);
	void Execute(Scalar delta, PolyTreeI& polytree);
	void Execute(DeltaCallback64 delta_cb, PathsI& paths);

	Scalar MiterLimit() const { return miter_limit_; }
	void MiterLimit(Scalar miter_limit) { miter_limit_ = miter_limit; }

	//ArcTolerance: needed for rounded offsets (See offset_triginometry2.svg)
	Scalar ArcTolerance() const { return arc_tolerance_; }
	void ArcTolerance(Scalar arc_tolerance) { arc_tolerance_ = arc_tolerance; }

	bool PreserveCollinear() const { return preserve_collinear_; }
	void PreserveCollinear(bool preserve_collinear){preserve_collinear_ = preserve_collinear;}
	
	bool ReverseSolution() const { return reverse_solution_; }
	void ReverseSolution(bool reverse_solution) {reverse_solution_ = reverse_solution;}

#ifdef USINGZ
	void SetZCallback(ZCallbackI cb) { zCallback64_ = cb; }
#endif
	void SetDeltaCallback(DeltaCallback64 cb) { deltaCallback64_ = cb; }

};

}
#endif /* CLIPPER_OFFSET_H_ */
