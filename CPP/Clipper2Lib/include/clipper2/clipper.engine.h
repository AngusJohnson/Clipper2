/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  17 April 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  This is the main polygon clipping module                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_ENGINE_H
#define CLIPPER_ENGINE_H

#include <cstdlib>
#include <stdint.h> //#541
#include <iostream>
#include <queue>
#include <vector>
#include <functional>
#include <numeric>
#include <memory>

#include "clipper2/clipper.core.h"

namespace Clipper2Lib {

	struct Scanline;
	struct IntersectNode;
	struct Active;
	struct Vertex;
	struct LocalMinima;
	struct OutRec;
	struct HorzSegment;

	//Note: all clipping operations except for Difference are commutative.
	enum class ClipType { None, Intersection, Union, Difference, Xor };

	enum class PathType { Subject, Clip };
	enum class JoinWith { None, Left, Right };

	enum class VertexFlags : uint32_t {
		None = 0, OpenStart = 1, OpenEnd = 2, LocalMax = 4, LocalMin = 8
	};

	constexpr enum VertexFlags operator &(enum VertexFlags a, enum VertexFlags b)
	{
		return (enum VertexFlags)(uint32_t(a) & uint32_t(b));
	}

	constexpr enum VertexFlags operator |(enum VertexFlags a, enum VertexFlags b)
	{
		return (enum VertexFlags)(uint32_t(a) | uint32_t(b));
	}

	struct Vertex {
		PointI pt;
		Vertex* next = nullptr;
		Vertex* prev = nullptr;
		VertexFlags flags = VertexFlags::None;
	};

	struct OutPt {
		PointI pt;
		OutPt*	next = nullptr;
		OutPt*	prev = nullptr;
		OutRec* outrec;
		HorzSegment* horz = nullptr;

		OutPt(const PointI& pt_, OutRec* outrec_): pt(pt_), outrec(outrec_) {
			next = this;
			prev = this;
		}
	};

	class PolyPath;
	class PolyPathI;
	class PolyPathS;
	using PolyTreeI = PolyPathI;
	using PolyTreeD = PolyPathS;

	struct OutRec;
	typedef std::vector<OutRec*> OutRecList;

	//OutRec: contains a path in the clipping solution. Edges in the AEL will
	//have OutRec pointers assigned when they form part of the clipping solution.
	struct OutRec {
		size_t idx = 0;
		OutRec* owner = nullptr;
		Active* front_edge = nullptr;
		Active* back_edge = nullptr;
		OutPt* pts = nullptr;
		PolyPath* polypath = nullptr;
		OutRecList* splits = nullptr;
		OutRec* recursive_split = nullptr;
		RectI bounds = {};
		PathI path;
		bool is_open = false;

		~OutRec() {
			if (splits) delete splits;
			// nb: don't delete the split pointers
			// as these are owned by ClipperBase's outrec_list_
		};
	};

	///////////////////////////////////////////////////////////////////
	//Important: UP and DOWN here are premised on Y-axis positive down
	//displays, which is the orientation used in Clipper's development.
	///////////////////////////////////////////////////////////////////

	struct Active {
		PointI bot;
		PointI top;
		Integer curr_x = 0;		//current (updated at every new scanline)
		Scalar dx = 0.0;
		int wind_dx = 1;			//1 or -1 depending on winding direction
		int wind_cnt = 0;
		int wind_cnt2 = 0;		//winding count of the opposite polytype
		OutRec* outrec = nullptr;
		//AEL: 'active edge list' (Vatti's AET - active edge table)
		//     a linked list of all edges (from left to right) that are present
		//     (or 'active') within the current scanbeam (a horizontal 'beam' that
		//     sweeps from bottom to top over the paths in the clipping operation).
		Active* prev_in_ael = nullptr;
		Active* next_in_ael = nullptr;
		//SEL: 'sorted edge list' (Vatti's ST - sorted table)
		//     linked list used when sorting edges into their new positions at the
		//     top of scanbeams, but also (re)used to process horizontals.
		Active* prev_in_sel = nullptr;
		Active* next_in_sel = nullptr;
		Active* jump = nullptr;
		Vertex* vertex_top = nullptr;
		LocalMinima* local_min = nullptr;  // the bottom of an edge 'bound' (also Vatti)
		bool is_left_bound = false;
		JoinWith join_with = JoinWith::None;
	};

	struct LocalMinima {
		Vertex* vertex;
		PathType polytype;
		bool is_open;
		LocalMinima(Vertex* v, PathType pt, bool open) :
			vertex(v), polytype(pt), is_open(open){}
	};

	struct IntersectNode {
		PointI pt;
		Active* edge1;
		Active* edge2;
		IntersectNode() : pt(PointI(0,0)), edge1(NULL), edge2(NULL) {}
			IntersectNode(Active* e1, Active* e2, PointI& pt_) :
			pt(pt_), edge1(e1), edge2(e2) {}
	};

	struct HorzSegment {
		OutPt* left_op;
		OutPt* right_op = nullptr;
		bool left_to_right = true;
		HorzSegment() : left_op(nullptr) { }
		explicit HorzSegment(OutPt* op) : left_op(op) { }
	};

	struct HorzJoin {
		OutPt* op1 = nullptr;
		OutPt* op2 = nullptr;
		HorzJoin() {};
		explicit HorzJoin(OutPt* ltr, OutPt* rtl) : op1(ltr), op2(rtl) { }
	};

#ifdef USINGZ
		typedef std::function<void(const PointI& e1bot, const PointI& e1top,
		const PointI& e2bot, const PointI& e2top, PointI& pt)> ZCallbackI;

	typedef std::function<void(const PointS& e1bot, const PointS& e1top,
		const PointS& e2bot, const PointS& e2top, PointS& pt)> ZCallbackS;
#endif

	typedef std::vector<HorzSegment> HorzSegmentList;
	typedef std::unique_ptr<LocalMinima> LocalMinima_ptr;
	typedef std::vector<LocalMinima_ptr> LocalMinimaList;
	typedef std::vector<IntersectNode> IntersectNodeList;

	// ReuseableDataContainerI ------------------------------------------------

	class ReuseableDataContainerI {
	private:
		friend class ClipperBase;
		LocalMinimaList minima_list_;
		std::vector<Vertex*> vertex_lists_;
		void AddLocMin(Vertex& vert, PathType polytype, bool is_open);
	public:
		virtual ~ReuseableDataContainerI();
		void Clear();
		void AddPaths(const PathsI& paths, PathType polytype, bool is_open);
	};

	// ClipperBase -------------------------------------------------------------

	class ClipperBase {
	private:
		ClipType cliptype_ = ClipType::None;
		FillRule fillrule_ = FillRule::EvenOdd;
		FillRule fillpos = FillRule::Positive;
		Integer bot_y_ = 0;
		bool minima_list_sorted_ = false;
		bool using_polytree_ = false;
		Active* actives_ = nullptr;
		Active *sel_ = nullptr;
		LocalMinimaList minima_list_;		//pointers in case of memory reallocs
		LocalMinimaList::iterator current_locmin_iter_;
		std::vector<Vertex*> vertex_lists_;
		std::priority_queue<Integer> scanline_list_;
		IntersectNodeList intersect_nodes_;
    HorzSegmentList horz_seg_list_;
		std::vector<HorzJoin> horz_join_list_;
		void Reset();
		inline void InsertScanline(Integer y);
		inline bool PopScanline(Integer &y);
		inline bool PopLocalMinima(Integer y, LocalMinima*& local_minima);
		void DisposeAllOutRecs();
		void DisposeVerticesAndLocalMinima();
		void DeleteEdges(Active*& e);
		inline void AddLocMin(Vertex &vert, PathType polytype, bool is_open);
		bool IsContributingClosed(const Active &e) const;
		inline bool IsContributingOpen(const Active &e) const;
		void SetWindCountForClosedPathEdge(Active &edge);
		void SetWindCountForOpenPathEdge(Active &e);
		void InsertLocalMinimaIntoAEL(Integer bot_y);
		void InsertLeftEdge(Active &e);
		inline void PushHorz(Active &e);
		inline bool PopHorz(Active *&e);
		inline OutPt* StartOpenPath(Active &e, const PointI& pt);
		inline void UpdateEdgeIntoAEL(Active *e);
		void IntersectEdges(Active &e1, Active &e2, const PointI& pt);
		inline void DeleteFromAEL(Active &e);
		inline void AdjustCurrXAndCopyToSEL(const Integer top_y);
		void DoIntersections(const Integer top_y);
		void AddNewIntersectNode(Active &e1, Active &e2, const Integer top_y);
		bool BuildIntersectList(const Integer top_y);
		void ProcessIntersectList();
		void SwapPositionsInAEL(Active& edge1, Active& edge2);
		OutRec* NewOutRec();
		OutPt* AddOutPt(const Active &e, const PointI& pt);
		OutPt* AddLocalMinPoly(Active &e1, Active &e2,
			const PointI& pt, bool is_new = false);
		OutPt* AddLocalMaxPoly(Active &e1, Active &e2, const PointI& pt);
		void DoHorizontal(Active &horz);
		bool ResetHorzDirection(const Active &horz, const Vertex* max_vertex,
			Integer &horz_left, Integer &horz_right);
		void DoTopOfScanbeam(const Integer top_y);
		Active *DoMaxima(Active &e);
		void JoinOutrecPaths(Active &e1, Active &e2);
		void FixSelfIntersects(OutRec* outrec);
		void DoSplitOp(OutRec* outRec, OutPt* splitOp);

		inline void AddTrialHorzJoin(OutPt* op);
		void ConvertHorzSegsToJoins();
		void ProcessHorzJoins();

		void Split(Active& e, const PointI& pt);
		inline void CheckJoinLeft(Active& e,
			const PointI& pt, bool check_curr_x = false);
		inline void CheckJoinRight(Active& e,
			const PointI& pt, bool check_curr_x = false);
	protected:
		bool preserve_collinear_ = true;
		bool reverse_solution_ = false;
		int error_code_ = 0;
		bool has_open_paths_ = false;
		bool succeeded_ = true;
		OutRecList outrec_list_; //pointers in case list memory reallocated
		bool ExecuteInternal(ClipType ct, FillRule ft, bool use_polytrees);
		void CleanCollinear(OutRec* outrec);
		bool CheckBounds(OutRec* outrec);
		bool CheckSplitOwner(OutRec* outrec, OutRecList* splits);
		void RecursiveCheckOwners(OutRec* outrec, PolyPath* polypath);
#ifdef USINGZ
		ZCallbackI zCallback_ = nullptr;
		void SetZ(const Active& e1, const Active& e2, PointI& pt);
#endif
		void CleanUp();  // unlike Clear, CleanUp preserves added paths
		void AddPath(const PathI& path, PathType polytype, bool is_open);
		void AddPaths(const PathsI& paths, PathType polytype, bool is_open);
	public:
		virtual ~ClipperBase();
		int ErrorCode() const { return error_code_; };
		void PreserveCollinear(bool val) { preserve_collinear_ = val; };
		bool PreserveCollinear() const { return preserve_collinear_;};
		void ReverseSolution(bool val) { reverse_solution_ = val; };
		bool ReverseSolution() const { return reverse_solution_; };
		void Clear();
		void AddReuseableData(const ReuseableDataContainerI& reuseable_data);
#ifdef USINGZ
		Integer DefaultZ = 0;
#endif
	};

	// PolyPath / PolyTree --------------------------------------------------------

	//PolyTree: is intended as a READ-ONLY data structure for CLOSED paths returned
	//by clipping operations. While this structure is more complex than the
	//alternative Paths structure, it does preserve path 'ownership' - ie those
	//paths that contain (or own) other paths. This will be useful to some users.

	class PolyPath {
	protected:
		PolyPath* parent_;
	public:
		PolyPath(PolyPath* parent = nullptr): parent_(parent){}
		virtual ~PolyPath() {};
		//https://en.cppreference.com/w/cpp/language/rule_of_three
		PolyPath(const PolyPath&) = delete;
		PolyPath& operator=(const PolyPath&) = delete;

		unsigned Level() const
		{
			unsigned result = 0;
			const PolyPath* p = parent_;
			while (p) { ++result; p = p->parent_; }
			return result;
		}

		virtual PolyPath* AddChild(const PathI& path) = 0;

		virtual void Clear() = 0;
		virtual size_t Count() const { return 0; }

		const PolyPath* Parent() const { return parent_; }

		bool IsHole() const
		{
			unsigned lvl = Level();
			//Even levels except level 0
			return lvl && !(lvl & 1);
		}
	};

	typedef typename std::vector<std::unique_ptr<PolyPathI>> PolyPathIList;
	typedef typename std::vector<std::unique_ptr<PolyPathS>> PolyPathSList;

	class PolyPathI : public PolyPath {
	private:
		PolyPathIList childs_;
		PathI polygon_;
	public:
		explicit PolyPathI(PolyPathI* parent = nullptr) : PolyPath(parent) {}

		~PolyPathI() {
			childs_.resize(0);
		}

		PolyPathI* operator [] (size_t index) const
		{
			return childs_[index].get(); //std::unique_ptr
		}

		PolyPathI* Child(size_t index) const
		{
			return childs_[index].get();
		}

		PolyPathIList::const_iterator begin() const { return childs_.cbegin(); }
		PolyPathIList::const_iterator end() const { return childs_.cend(); }

		PolyPathI* AddChild(const PathI& path) override
		{
			auto p = std::make_unique<PolyPathI>(this);
			auto* result = childs_.emplace_back(std::move(p)).get();
			result->polygon_ = path;
			return result;
		}

		void Clear() override
		{
			childs_.resize(0);
		}

		size_t Count() const override
		{
			return childs_.size();
		}

		const PathI& Polygon() const { return polygon_; };

		Scalar Area() const
		{
			return std::accumulate(childs_.cbegin(), childs_.cend(),
				Clipper2Lib::Area<Integer>(polygon_),
				[](Scalar a, const auto& child) {return a + child->Area(); });
		}

	};

	class PolyPathS : public PolyPath {
	private:
		PolyPathSList childs_;
		Scalar scale_;
		PathS polygon_;
	public:
		explicit PolyPathS(PolyPathS* parent = nullptr) : PolyPath(parent)
		{
			scale_ = parent ? parent->scale_ : (Scalar)1.0;
		}

		~PolyPathS() {
			childs_.resize(0);
		}

		PolyPathS* operator [] (size_t index) const
		{
			return childs_[index].get();
		}

		PolyPathS* Child(size_t index) const
		{
			return childs_[index].get();
		}

		PolyPathSList::const_iterator begin() const { return childs_.cbegin(); }
		PolyPathSList::const_iterator end() const { return childs_.cend(); }

		void SetScale(Scalar value) { scale_ = value; }
		Scalar Scale() const { return scale_; }

		PolyPathS* AddChild(const PathI& path) override
		{
			int error_code = 0;
			auto p = std::make_unique<PolyPathS>(this);
			PolyPathS* result = childs_.emplace_back(std::move(p)).get();
			result->polygon_ = ScalePath<Scalar, Integer>(path, scale_, error_code);
			return result;
		}

		PolyPathS* AddChild(const PathS& path)
		{
			auto p = std::make_unique<PolyPathS>(this);
			PolyPathS* result = childs_.emplace_back(std::move(p)).get();
			result->polygon_ = path;
			return result;
		}

		void Clear() override
		{
			childs_.resize(0);
		}

		size_t Count() const override
		{
			return childs_.size();
		}

		const PathS& Polygon() const { return polygon_; };

		Scalar Area() const
		{
			return std::accumulate(childs_.begin(), childs_.end(),
				Clipper2Lib::Area<Scalar>(polygon_),
				[](Scalar a, const auto& child) {return a + child->Area(); });
		}
	};

	class ClipperI : public ClipperBase
	{
	private:
		void BuildPathsI(PathsI& solutionClosed, PathsI* solutionOpen);
		void BuildTreeI(PolyPathI& polytree, PathsI& open_paths);
	public:
#ifdef USINGZ
		void SetZCallback(ZCallbackI cb) { zCallback_ = cb; }
#endif

		void AddSubject(const PathsI& subjects)
		{
			AddPaths(subjects, PathType::Subject, false);
		}
		void AddOpenSubject(const PathsI& open_subjects)
		{
			AddPaths(open_subjects, PathType::Subject, true);
		}
		void AddClip(const PathsI& clips)
		{
			AddPaths(clips, PathType::Clip, false);
		}

		bool Execute(ClipType clip_type,
			FillRule fill_rule, PathsI& closed_paths)
		{
			PathsI dummy;
			return Execute(clip_type, fill_rule, closed_paths, dummy);
		}

		bool Execute(ClipType clip_type, FillRule fill_rule,
			PathsI& closed_paths, PathsI& open_paths)
		{
			closed_paths.clear();
			open_paths.clear();
			if (ExecuteInternal(clip_type, fill_rule, false))
					BuildPathsI(closed_paths, &open_paths);
			CleanUp();
			return succeeded_;
		}

		bool Execute(ClipType clip_type, FillRule fill_rule, PolyTreeI& polytree)
		{
			PathsI dummy;
			return Execute(clip_type, fill_rule, polytree, dummy);
		}

		bool Execute(ClipType clip_type,
			FillRule fill_rule, PolyTreeI& polytree, PathsI& open_paths)
		{
			if (ExecuteInternal(clip_type, fill_rule, true))
			{
				open_paths.clear();
				polytree.Clear();
				BuildTreeI(polytree, open_paths);
			}
			CleanUp();
			return succeeded_;
		}
	};

	class ClipperS : public ClipperBase {
	private:
		Scalar scale_ = (Scalar)1.0, invScale_ = (Scalar)1.0;
#ifdef USINGZ
		ZCallbackS zCallbackD_ = nullptr;
#endif
		void BuildPathsS(PathsS& solutionClosed, PathsS* solutionOpen);
		void BuildTreeS(PolyPathS& polytree, PathsS& open_paths);
	public:
		explicit ClipperS(int precision = 2) : ClipperBase()
		{
			CheckPrecisionRange(precision, error_code_);
			// to optimize scaling / descaling precision
			// set the scale to a power of Scalar's radix (2) (#25)
			scale_ = (Scalar)std::pow((Scalar)std::numeric_limits<Scalar>::radix,
				(Scalar)std::ilogb((Scalar)std::pow(10, precision)) + (Scalar)1);
			invScale_ = (Scalar)1 / scale_;
		}

#ifdef USINGZ
		void SetZCallback(ZCallbackS cb) { zCallbackD_ = cb; };

		void ZCB(const PointI& e1bot, const PointI& e1top,
			const PointI& e2bot, const PointI& e2top, PointI& pt)
		{
			// de-scale (x & y)
			// temporarily convert integers to their initial float values
			// this will slow clipping marginally but will make it much easier
			// to understand the coordinates passed to the callback function
			PointS tmp = PointS(pt) * invScale_;
			PointS e1b = PointS(e1bot) * invScale_;
			PointS e1t = PointS(e1top) * invScale_;
			PointS e2b = PointS(e2bot) * invScale_;
			PointS e2t = PointS(e2top) * invScale_;
			zCallbackD_(e1b,e1t, e2b, e2t, tmp);
			pt.z = tmp.z; // only update 'z'
		};

		void CheckCallback()
		{
			if(zCallbackD_)
				// if the user defined float point callback has been assigned
				// then assign the proxy callback function
				ClipperBase::zCallback_ =
					std::bind(&ClipperS::ZCB, this, std::placeholders::_1,
					std::placeholders::_2, std::placeholders::_3,
					std::placeholders::_4, std::placeholders::_5);
			else
				ClipperBase::zCallback_ = nullptr;
		}

#endif

		void AddSubject(const PathsS& subjects)
		{
			AddPaths(ScalePaths<Integer, Scalar>(subjects, scale_, error_code_), PathType::Subject, false);
		}

		void AddOpenSubject(const PathsS& open_subjects)
		{
			AddPaths(ScalePaths<Integer, Scalar>(open_subjects, scale_, error_code_), PathType::Subject, true);
		}

		void AddClip(const PathsS& clips)
		{
			AddPaths(ScalePaths<Integer, Scalar>(clips, scale_, error_code_), PathType::Clip, false);
		}

		bool Execute(ClipType clip_type, FillRule fill_rule, PathsS& closed_paths)
		{
			PathsS dummy;
			return Execute(clip_type, fill_rule, closed_paths, dummy);
		}

		bool Execute(ClipType clip_type,
			FillRule fill_rule, PathsS& closed_paths, PathsS& open_paths)
		{
#ifdef USINGZ
			CheckCallback();
#endif
			if (ExecuteInternal(clip_type, fill_rule, false))
			{
				BuildPathsS(closed_paths, &open_paths);
			}
			CleanUp();
			return succeeded_;
		}

		bool Execute(ClipType clip_type, FillRule fill_rule, PolyTreeD& polytree)
		{
			PathsS dummy;
			return Execute(clip_type, fill_rule, polytree, dummy);
		}

		bool Execute(ClipType clip_type,
			FillRule fill_rule, PolyTreeD& polytree, PathsS& open_paths)
		{
#ifdef USINGZ
			CheckCallback();
#endif
			if (ExecuteInternal(clip_type, fill_rule, true))
			{
				polytree.Clear();
				polytree.SetScale(invScale_);
				open_paths.clear();
				BuildTreeS(polytree, open_paths);
			}
			CleanUp();
			return succeeded_;
		}

	};

}  // namespace

#endif  // CLIPPER_ENGINE_H
