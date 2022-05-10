/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  3 May 2022                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This is the main polygon clipping module                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef clipper_engine_h
#define clipper_engine_h

#define CLIPPER2_VERSION "1.0.0"

#include <cstdlib>
#include <queue>
#include <stdexcept>
#include <vector>
#include "clipper.core.h"

namespace Clipper2Lib {

	static double const PI = 3.141592653589793238;

	//Note: all clipping operations except for Difference are commutative.
	enum class ClipType { None, Intersection, Union, Difference, Xor };

	enum class PathType { Subject, Clip };

	//By far the most widely used filling rules for polygons are EvenOdd
	//and NonZero, sometimes called Alternate and Winding respectively.
	//https://en.wikipedia.org/wiki/Nonzero-rule
	enum class FillRule { EvenOdd, NonZero, Positive, Negative };

	template <typename T>
	class PolyPath;

	using PolyPath64 = PolyPath<Point64>;
	using PolyPathD = PolyPath<PointD>;
	using PolyTree64 = PolyPath<Point64>;
	using PolyTreeD = PolyPath<PointD>;

	namespace engine::detail {
		template<typename PointType> struct IntersectNode;
		template<typename PointType> struct Joiner;
		template<typename PointType> struct Active;
		template<typename PointType> struct Vertex;
		template<typename PointType> struct LocalMinima;
		template<typename PointType> struct OutRec;

		enum class VertexFlags : uint32_t {
			None = 0, OpenStart = 1, OpenEnd = 2, LocalMax = 4, LocalMin = 8
		};

		constexpr enum VertexFlags operator &(enum VertexFlags a, enum VertexFlags b) {
			return (enum VertexFlags)(uint32_t(a) & uint32_t(b));
		};

		constexpr enum VertexFlags operator |(enum VertexFlags a, enum VertexFlags b) {
			return (enum VertexFlags)(uint32_t(a) | uint32_t(b));
		};

		template<typename PointType>
		struct Vertex {
			PointType pt { point_traits<PointType>::construct(0, 0) };
			Vertex<PointType>* next = NULL;
			Vertex<PointType>* prev = NULL;
			VertexFlags flags = VertexFlags::None;
		};

		template<typename PointType>
		struct OutPt {
			PointType pt;
			OutPt<PointType>*	next = NULL;
			OutPt<PointType>*	prev = NULL;
			OutRec<PointType>*  outrec;
			Joiner<PointType>*  joiner = NULL;

			OutPt(const PointType& pt_, OutRec<PointType>* outrec_): pt(pt_), outrec(outrec_) {
				next = this;
				prev = this;
			}
		};

		enum class OutRecState { Undefined = 0, Open = 1, Outer = 2, Inner = 4};

		//OutRec: contains a path in the clipping solution. Edges in the AEL will
		//have OutRec pointers assigned when they form part of the clipping solution.
		template<typename PointType>
		struct OutRec {
			size_t idx;
			OutRec* owner;
			Active<PointType>* front_edge;
			Active<PointType>* back_edge;
			OutPt<PointType>* pts;
			PolyPath<PointType>* polypath = NULL;
			OutRecState state = OutRecState::Undefined;
		};

		template<typename PointType>
		struct Active {
			PointType bot { point_traits<PointType>::construct(0, 0) };
			PointType top { point_traits<PointType>::construct(0, 0) };
			int64_t curr_x = 0;		//current (updated at every new scanline)
			double dx = 0.0;
			int wind_dx = 1;			//1 or -1 depending on winding diRect64on
			int wind_cnt = 0;
			int wind_cnt2 = 0;		//winding count of the opposite polytype
			OutRec<PointType>* outrec = NULL;
			//AEL: 'active edge list' (Vatti's AET - active edge table)
			//     a linked list of all edges (from left to right) that are present
			//     (or 'active') within the current scanbeam (a horizontal 'beam' that
			//     sweeps from bottom to top over the paths in the clipping operation).
			Active<PointType>* prev_in_ael = NULL;
			Active<PointType>* next_in_ael = NULL;
			//SEL: 'sorted edge list' (Vatti's ST - sorted table)
			//     linked list used when sorting edges into their new positions at the
			//     top of scanbeams, but also (re)used to process horizontals.
			Active<PointType>* prev_in_sel = NULL;
			Active<PointType>* next_in_sel = NULL;
			Active<PointType>* jump = NULL;
			Vertex<PointType>* vertex_top = NULL;
			LocalMinima<PointType>* local_min = NULL;  //the bottom of an edge 'bound' (also Vatti)
			bool is_left_bound = false;
		};

		template<typename PointType>
		struct LocalMinima {
			Vertex<PointType>* vertex;
			PathType polytype;
			bool is_open;
			LocalMinima(Vertex<PointType>* v, PathType pt, bool open) :
				vertex(v), polytype(pt), is_open(open){}
		};
	}

	// ClipperBase -------------------------------------------------------------

	template<typename PointType>
	struct EmptyZFillFunc {
		void operator()(const PointType& /* e1bot */, const PointType& /* e1top */,
			const PointType& /* e2bot */, const PointType& /* e2top */, PointType& /* pt */) {}
	};

	template<typename PointType, typename ZFillFunc = EmptyZFillFunc<PointType>, typename ClipperFlags = DefaultClipperFlags>
	class ClipperBase {
	public:
		using Path = std::vector<PointType>;
		using Paths = std::vector<Path>;

	private:
		using VertexFlags   = engine::detail::VertexFlags;
		using OutRecState   = engine::detail::OutRecState;
		using IntersectNode = engine::detail::IntersectNode<PointType>;
		using Joiner 		= engine::detail::Joiner<PointType>;
		using Active 		= engine::detail::Active<PointType>;
		using Vertex 		= engine::detail::Vertex<PointType>;
		using LocalMinima 	= engine::detail::LocalMinima<PointType>;
		using OutPt         = engine::detail::OutPt<PointType>;
		using OutRec 		= engine::detail::OutRec<PointType>;

		ClipType cliptype_ = ClipType::None;
		FillRule fillrule_ = FillRule::EvenOdd;
		int64_t bot_y_ = 0;
		bool error_found_ = false;
		bool has_open_paths_ = false;
		bool minima_list_sorted_ = false;
		Active* actives_ = NULL;
		Active* sel_ = NULL;
		Joiner* horz_joiners_ = NULL;
		std::vector<LocalMinima*> minima_list_;
		typename std::vector<LocalMinima*>::iterator loc_min_iter_;
		std::vector<Vertex*> vertex_lists_;
		std::priority_queue<int64_t> scanline_list_;
		std::vector<IntersectNode*> intersect_nodes_;
		std::vector<Joiner*> joiner_list_;
		void Reset();
		void InsertScanline(int64_t y);
		bool PopScanline(int64_t &y);
		bool PopLocalMinima(int64_t y, LocalMinima*&local_minima);
		void DisposeAllOutRecs();
		void DisposeVerticesAndLocalMinima();
		void AddLocMin(Vertex &vert, PathType polytype, bool is_open);
		bool IsContributingClosed(const Active&e) const;
		inline bool IsContributingOpen(const Active&e) const;
		void SetWindCountForClosedPathEdge(Active&edge);
		void SetWindCountForOpenPathEdge(Active&e);
		void InsertLocalMinimaIntoAEL(int64_t bot_y);
		void InsertLeftEdge(Active&e);
		inline void PushHorz(Active&e);
		inline bool PopHorz(Active*&e);
		inline OutPt* StartOpenPath(Active&e, const PointType& pt);
		inline void UpdateEdgeIntoAEL(Active*e);
		OutPt* IntersectEdges(Active&e1, Active&e2, const PointType& pt);
		inline void DeleteFromAEL(Active&e);
		inline void AdjustCurrXAndCopyToSEL(const int64_t top_y);
		void DoIntersections(const int64_t top_y);
		void DisposeIntersectNodes();
		void AddNewIntersectNode(Active&e1, Active&e2, const int64_t top_y);
		bool BuildIntersectList(const int64_t top_y);
		void ProcessIntersectList();
		void SwapPositionsInAEL(Active& edge1, Active& edge2);
		OutPt* AddOutPt(const Active&e, const PointType& pt);
		bool TestJoinWithPrev1(const Active& e, int64_t curr_y);
		bool TestJoinWithPrev2(const Active& e, const PointType& curr_pt);
		bool TestJoinWithNext1(const Active& e, int64_t curr_y);
		bool TestJoinWithNext2(const Active& e, const PointType& curr_pt);

		OutPt* AddLocalMinPoly(Active&e1, Active&e2,
			const PointType& pt, bool is_new = false);
		OutPt* AddLocalMaxPoly(Active&e1, Active&e2, const PointType& pt);
		void DoHorizontal(Active&horz);
		bool ResetHorzDiRect64on(const Active&horz, const Active*max_pair,
			int64_t &horz_left, int64_t &horz_right);
		void DoTopOfScanbeam(const int64_t top_y);
		Active*DoMaxima(Active &e);
		void JoinOutrecPaths(Active &e1, Active &e2);
		bool FixSides(Active& e, Active& e2);
		void CompleteSplit(OutPt* op1, OutPt* op2, OutRec& outrec);
		bool ValidateClosedPathEx(OutRec* outrec);
		void CleanCollinear(OutRec* outrec);
		void FixSelfIntersects(OutRec* outrec);
		OutPt* DoSplitOp(OutPt* outRecOp, OutPt* splitOp);
		Joiner* GetHorzTrialParent(const OutPt* op);
		bool OutPtInTrialHorzList(OutPt* op);
		void SafeDisposeOutPts(OutPt* op);
		void SafeDeleteOutPtJoiners(OutPt* op);
		void AddTrialHorzJoin(OutPt* op);
		void DeleteTrialHorzJoin(OutPt* op);
		void ConvertHorzTrialsToJoins();
		void AddJoin(OutPt* op1, OutPt* op2);
		void DeleteJoin(Joiner* joiner);
		void ProcessJoinerList();
		OutRec* ProcessJoin(Joiner* joiner);
		bool ExecuteInternal(ClipType ct, FillRule ft);
		void BuildPaths(Paths& solutionClosed, Paths* solutionOpen);
		void BuildTree(PolyPath<PointType>& polytree, Paths& open_paths);
		template<typename PointType> static inline auto GetX(const PointType& p) { return detail::GetX(p); }
		template<typename PointType> static inline auto GetY(const PointType& p) { return detail::GetY(p); }
		template<typename PointType> static inline bool EqualsXY(const PointType& p1, const PointType& p2) { return detail::EqualsXY(p1, p2); }
		void SetZ(const Active& e1, const Active& e2, PointType& pt);
		ZFillFunc zfill_func_;
		std::vector<OutRec*> outrec_list_;
	protected:
		void CleanUp();  //unlike Clear, CleanUp preserves added paths

	public:
		ClipperBase(ZFillFunc zfill_func = ZFillFunc{}) : zfill_func_(zfill_func) {}
		~ClipperBase();
		bool PreserveCollinear = true;
		void Clear();
		void AddPath(const Path& path, PathType polytype, bool is_open);
		void AddPaths(const Paths& paths, PathType polytype, bool is_open);

		bool Execute(ClipType clip_type,
			FillRule fill_rule, Paths& solution_closed);
		bool Execute(ClipType clip_type,
			FillRule fill_rule, Paths& solution_closed, Paths& solution_open);
		bool Execute(ClipType clip_type,
			FillRule fill_rule, PolyTree64& polytree, Paths& open_paths);

		void AddSubject(const Paths & subjects)
		{
			AddPaths(subjects, PathType::Subject, false);
		}
		void AddOpenSubject(const Paths & open_subjects)
		{
			AddPaths(open_subjects, PathType::Subject, true);
		}
		void AddClip(const Paths & clips)
		{
			AddPaths(clips, PathType::Clip, false);
		}
	};

	// PolyPath / PolyTree --------------------------------------------------------

	//PolyTree: is intended as a READ-ONLY data structure for CLOSED paths returned
	//by clipping operations. While this structure is more complex than the
	//alternative Paths structure, it does preserve path 'ownership' - ie those
	//paths that contain (or own) other paths. This will be useful to some users.

	template <typename PointType>
	class PolyPath {
	private:
		double scale_;
	protected:
		const PolyPath<PointType>* parent_;
		PolyPath(const PolyPath<PointType>* parent, 
			const std::vector<PointType>& path) : 
			parent_(parent), polygon(path), scale_(parent->scale_) {};
	public:
		std::vector<PointType> polygon;
		std::vector<PolyPath*> childs;

		explicit PolyPath(int precision = 0) //NB only for root node
		{  
			scale_ = std::pow(10, precision);
			parent_ = NULL;
		}

		virtual ~PolyPath() { Clear(); };

		void Clear() { 
			for (PolyPath<PointType>* child : childs) delete child;
			childs.resize(0); 
		}

		void reserve(size_t size)
		{
			if (size > childs.size()) childs.reserve(size);
		}

		PolyPath<PointType>* AddChild(const std::vector<PointType>& path)
		{
			childs.push_back(new PolyPath<PointType>(this, path));
			return childs.back();
		}

		size_t ChildCount() const { return childs.size(); }

		const PolyPath<PointType>* operator [] (size_t index) const { return childs[index]; }

		const PolyPath<PointType>* Parent() const { return parent_; };

		const std::vector<PointType>& Path() const { return polygon; };

		bool IsHole() const {
			PolyPath* pp = parent_;
			bool is_hole = pp;
			while (pp) {
				is_hole = !is_hole;
				pp = pp->parent_;
			}
			return is_hole;
		}
	};

	inline void Polytree64ToPolytreeD(const PolyPath64& polytree, PolyPathD& result);

	using Clipper64 = ClipperBase<Point64, EmptyZFillFunc<Point64>, DefaultClipperFlags>;

	class ClipperD : public Clipper64 {
	private:
		const double scale_;
	public:
		explicit ClipperD(int precision = 0) : scale_(std::pow(10, precision)) {}

		void AddSubject(const PathsD& subjects)
		{
			AddPaths(PathsDToPaths64(subjects, scale_), PathType::Subject, false);
		}

		void AddOpenSubject(const PathsD& open_subjects)
		{
			AddPaths(PathsDToPaths64(open_subjects, scale_), PathType::Subject, true);
		}

		void AddClip(const PathsD& clips)
		{
			AddPaths(PathsDToPaths64(clips, scale_), PathType::Clip, false);
		}

		bool Execute(ClipType clip_type, FillRule fill_rule, PathsD& closed_paths)
		{
			Paths closed_paths64;
			if (!Clipper64::Execute(clip_type, fill_rule, closed_paths64)) return false;
			closed_paths = Paths64ToPathsD(closed_paths64, 1 / scale_);
			return true;
		}

		bool Execute(ClipType clip_type,
			FillRule fill_rule, PathsD& closed_paths, PathsD& open_paths)
		{
			Paths closed_paths64;
			Paths open_paths64;
			if (!Clipper64::Execute(clip_type,
				fill_rule, closed_paths64, open_paths64)) return false;
			closed_paths = Paths64ToPathsD(closed_paths64, 1 / scale_);
			open_paths = Paths64ToPathsD(open_paths64, 1 / scale_);
			return true;
		}

		bool Execute(ClipType clip_type,
			FillRule fill_rule, PolyTreeD& polytree, Paths& open_paths)
		{
			PolyTree64 tree_result;
			if (!Clipper64::Execute(clip_type, fill_rule, tree_result, open_paths)) return false;;
			Polytree64ToPolytreeD(tree_result, polytree);
			return true;
		}

	};

	using Clipper = Clipper64;
}

/////////////////////////////////////////////////////////////////////////////////////
//                                 Implementation                                  //
/////////////////////////////////////////////////////////////////////////////////////

#include "detail/clipper_engine_detail.h"

namespace Clipper2Lib {

	//------------------------------------------------------------------------------
	// ClipperBase methods ...
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline ClipperBase<PointType, ZFillFunc, ClipperFlags>::~ClipperBase()
	{
		Clear();
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::CleanUp()
	{
		while (actives_) DeleteFromAEL(*actives_);
		scanline_list_ = std::priority_queue<int64_t>();
		DisposeIntersectNodes();
		DisposeAllOutRecs();
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::Clear()
	{
		CleanUp();
		DisposeVerticesAndLocalMinima();
		loc_min_iter_ = minima_list_.begin();
		minima_list_sorted_ = false;
		has_open_paths_ = false;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::Reset()
	{
		if (!minima_list_sorted_)
		{
			std::sort(minima_list_.begin(), minima_list_.end(), engine::detail::LocMinSorter<PointType>());
			minima_list_sorted_ = true;
		}
		std::vector<LocalMinima*>::const_reverse_iterator i;
		for (i = minima_list_.rbegin(); i != minima_list_.rend(); ++i)
			InsertScanline(GetY((*i)->vertex->pt));

		loc_min_iter_ = minima_list_.begin();
		actives_ = NULL;
		sel_ = NULL;
		error_found_ = false;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::SetZ(const Active& e1, const Active& e2, PointType& ip)
	{
		if constexpr (point_traits<PointType>::has_z) {
		//prioritize subject vertices over clip vertices
		//and pass the subject vertices before clip vertices in the callback
			auto CopyZ = [&ip](const PointType &p) {
				point_traits<PointType>::set(ip, 2, point_traits<PointType>::get(p, 2));
			};
			if (engine::detail::GetPolyType(e1) == PathType::Subject)
			{
				if (EqualsXY(ip, e1.bot)) CopyZ(e1.bot);
				else if (EqualsXY(ip, e1.top)) CopyZ(e1.top);
				else if (EqualsXY(ip, e2.bot)) CopyZ(e2.bot);
				else if (EqualsXY(ip, e2.top)) CopyZ(e2.top);
				else zfill_func_(e1.bot, e1.top, e2.bot, e2.top, ip);
			}
			else
			{
				if (EqualsXY(ip, e2.bot)) CopyZ(e2.bot);
				else if (EqualsXY(ip, e2.top)) CopyZ(e2.top);
				else if (EqualsXY(ip, e1.bot)) CopyZ(e1.bot);
				else if (EqualsXY(ip, e1.top)) CopyZ(e1.top);
				else zfill_func_(e2.bot, e2.top, e1.bot, e1.top, ip);
			}
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddPath(const Path& path, PathType polytype, bool is_open)
	{
		Paths tmp;
		tmp.push_back(path);
		AddPaths(tmp, polytype, is_open);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddPaths(const Paths& paths, PathType polytype, bool is_open)
	{
		if (is_open) has_open_paths_ = true;
		minima_list_sorted_ = false;

		Paths::size_type paths_cnt = paths.size();
		Path::size_type total_vertex_count = 0;
		for (const Path& path : paths) total_vertex_count += path.size();
		if (total_vertex_count == 0) return;
		auto *vertices = new Vertex[total_vertex_count], *v = vertices;
		for (const Path path : paths)
		{
			//for each path create a circular double linked list of vertices
			Vertex* v0 = v, * curr_v = v, * prev_v = NULL;
			int cnt = 0;
			for (const PointType &pt : path)
			{
				if (prev_v)
				{
					if (EqualsXY(prev_v->pt, pt)) continue; //ie skips duplicates
					prev_v->next = curr_v;
				}
				curr_v->prev = prev_v;
				curr_v->pt = pt;
				curr_v->flags = VertexFlags::None;
				prev_v = curr_v++;
				cnt++;
			}
			if (!prev_v || !prev_v->prev) continue;
			if (!is_open && EqualsXY(prev_v->pt, v0->pt))
				prev_v = prev_v->prev;
			prev_v->next = v0;
			v0->prev = prev_v;
			v = curr_v; //ie get ready for next path
			if (cnt < 2 || (cnt == 2 && !is_open)) continue;

			//now find and assign local minima
			bool going_up, going_up0;
			if (is_open)
			{
				curr_v = v0->next;
				while (curr_v != v0 && GetY(curr_v->pt) == GetY(v0->pt))
					curr_v = curr_v->next;
				going_up = GetY(curr_v->pt) <= GetY(v0->pt);
				if (going_up) {
					v0->flags = VertexFlags::OpenStart;
					AddLocMin(*v0, polytype, true);
				}
				else
					v0->flags = VertexFlags::OpenStart | VertexFlags::LocalMax;
			}
			else //closed path
			{
				prev_v = v0->prev;
				while (prev_v != v0 && GetY(prev_v->pt) == GetY(v0->pt))
					prev_v = prev_v->prev;
				if (prev_v == v0)
					continue; //only open paths can be completely flat
				going_up = GetY(prev_v->pt) > GetY(v0->pt);
			}

			going_up0 = going_up;
			prev_v = v0;
			curr_v = v0->next;
			while (curr_v != v0) 
			{
				if (GetY(curr_v->pt) > GetY(prev_v->pt) && going_up)
				{
					prev_v->flags = (prev_v->flags | VertexFlags::LocalMax);
					going_up = false;
				}
				else if (GetY(curr_v->pt) < GetY(prev_v->pt) && !going_up) {
					going_up = true;
					AddLocMin(*prev_v, polytype, is_open);
				}
				prev_v = curr_v;
				curr_v = curr_v->next;
			}
			
			if (is_open) 
			{
				prev_v->flags = prev_v->flags | VertexFlags::OpenEnd;
				if (going_up)
					prev_v->flags = prev_v->flags | VertexFlags::LocalMax;
				else
					AddLocMin(*prev_v, polytype, is_open);
			}
			else if (going_up != going_up0)
			{
				if (going_up0) AddLocMin(*prev_v, polytype, false);
				else prev_v->flags = prev_v->flags | VertexFlags::LocalMax;
			}
		} //end processing current path

		vertex_lists_.emplace_back(vertices);
	} //end AddPaths
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::InsertScanline(int64_t y) 
	{
		scanline_list_.push(y);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::PopScanline(int64_t &y)
	{
		if (scanline_list_.empty()) return false;
		y = scanline_list_.top();
		scanline_list_.pop();
		while (!scanline_list_.empty() && y == scanline_list_.top())
			scanline_list_.pop();  // Pop duplicates.
		return true;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::PopLocalMinima(int64_t y, LocalMinima*&local_minima)
	{
		if (loc_min_iter_ == minima_list_.end() || GetY((*loc_min_iter_)->vertex->pt) != y) return false;
		local_minima = (*loc_min_iter_++);
		return true;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DisposeAllOutRecs()
	{
		for (auto outrec :  outrec_list_) {
			if (outrec->pts) engine::detail::DisposeOutPts(*outrec);
			delete outrec;
		}
		outrec_list_.resize(0);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DisposeVerticesAndLocalMinima()
	{
		for (auto lm : minima_list_) delete lm;
		minima_list_.clear();
		for (auto v : vertex_lists_) delete [] v;
		vertex_lists_.clear();
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddLocMin(Vertex&vert, PathType polytype, bool is_open)
	{
		//make sure the vertex is added only once ...
		if ((VertexFlags::LocalMin & vert.flags) != VertexFlags::None) return;

		vert.flags = (vert.flags | VertexFlags::LocalMin);
		minima_list_.push_back(new LocalMinima(&vert, polytype, is_open));
	}
	//----------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::IsContributingClosed(const Active &e) const
	{
		switch (fillrule_) {
			case FillRule::NonZero :
				if (abs(e.wind_cnt) != 1) return false;
				break;
			case FillRule::Positive:
				if (e.wind_cnt != 1) return false;
				break;
			case FillRule::Negative:
				if (e.wind_cnt != -1) return false;
				break;
			case FillRule::EvenOdd:
				break;  
		}

		switch (cliptype_) {
		case ClipType::Intersection:
				switch (fillrule_) {
					case FillRule::EvenOdd:
					case FillRule::NonZero : return (e.wind_cnt2 != 0);
					case FillRule::Positive: return (e.wind_cnt2 > 0);
					case FillRule::Negative: return (e.wind_cnt2 < 0);
				}
				break;
			case ClipType::Union:
				switch (fillrule_) {
					case FillRule::EvenOdd:
					case FillRule::NonZero : return (e.wind_cnt2 == 0);
					case FillRule::Positive: return (e.wind_cnt2 <= 0);
					case FillRule::Negative: return (e.wind_cnt2 >= 0);
				}
				break;
			case ClipType::Difference:
				if (engine::detail::GetPolyType(e) == PathType::Subject)
					switch (fillrule_) {
						case FillRule::EvenOdd:
						case FillRule::NonZero : return (e.wind_cnt2 == 0);
						case FillRule::Positive: return (e.wind_cnt2 <= 0);
						case FillRule::Negative: return (e.wind_cnt2 >= 0);
					}
				else
					switch (fillrule_) {
						case FillRule::EvenOdd:
						case FillRule::NonZero : return (e.wind_cnt2 != 0);
						case FillRule::Positive: return (e.wind_cnt2 > 0);
						case FillRule::Negative: return (e.wind_cnt2 < 0);
					}
				break;
			case ClipType::Xor:
				return true;  //XOr is always contributing unless open
			default:
				return false; 
		}
		return false;  //we should never get here
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::IsContributingOpen(const Active&e) const
	{
		switch (cliptype_) {
			case ClipType::Intersection: return (e.wind_cnt2 != 0);
			case ClipType::Union: return (e.wind_cnt == 0 && e.wind_cnt2 == 0);
			case ClipType::Difference: return (e.wind_cnt2 == 0);
			case ClipType::Xor: return (e.wind_cnt != 0) != (e.wind_cnt2 != 0);
			default:
				return false;  // delphi2cpp translation note: no warnings
		}
		return false;  //stops compiler error
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::SetWindCountForClosedPathEdge(Active&e) {
		//Wind counts refer to polygon regions not edges, so here an edge's WindCnt
		//indicates the higher of the wind counts for the two regions touching the
		//edge. (NB Adjacent regions can only ever have their wind counts differ by
		//one. Also, open paths have no meaningful wind directions or counts.)

		using namespace engine::detail;

		Active*e2 = e.prev_in_ael;
		//find the nearest closed path edge of the same PolyType in AEL (heading left)
		PathType pt = GetPolyType(e);
		while (e2 && (GetPolyType(*e2) != pt || IsOpen(*e2))) e2 = e2->prev_in_ael;

		if (!e2) 
		{
			e.wind_cnt = e.wind_dx;
			e2 = actives_;
		} 
		else if (fillrule_ == FillRule::EvenOdd) 
		{
			e.wind_cnt = e.wind_dx;
			e.wind_cnt2 = e2->wind_cnt2;
			e2 = e2->next_in_ael;
		} 
		else 
		{
			//NonZero, positive, or negative filling here ...
			//if e's WindCnt is in the SAME direction as its WindDx, then polygon
			//filling will be on the right of 'e'.
			//NB neither e2.WindCnt nor e2.WindDx should ever be 0.
			if (e2->wind_cnt * e2->wind_dx < 0) {
				//opposite diRect64ons so 'e' is outside 'e2' ...
				if (abs(e2->wind_cnt) > 1) {
					//outside prev poly but still inside another.
					if (e2->wind_dx * e.wind_dx < 0)
						//reversing diRect64on so use the same WC
						e.wind_cnt = e2->wind_cnt;
					else
						//otherwise keep 'reducing' the WC by 1 (ie towards 0) ...
						e.wind_cnt = e2->wind_cnt + e.wind_dx;
				} else
					//now outside all polys of same polytype so set own WC ...
					e.wind_cnt = (IsOpen(e) ? 1 : e.wind_dx);
			} 
			else 
			{
				//'e' must be inside 'e2'
				if (e2->wind_dx * e.wind_dx < 0)
					//reversing diRect64on so use the same WC
					e.wind_cnt = e2->wind_cnt;
				else
					//otherwise keep 'increasing' the WC by 1 (ie away from 0) ...
					e.wind_cnt = e2->wind_cnt + e.wind_dx;
			}
			e.wind_cnt2 = e2->wind_cnt2;
			e2 = e2->next_in_ael;  //ie get ready to calc WindCnt2
		}

		//update wind_cnt2 ...
		if (fillrule_ == FillRule::EvenOdd)
			while (e2 != &e) {
				if (GetPolyType(*e2) != pt && !IsOpen(*e2))
					e.wind_cnt2 = (e.wind_cnt2 == 0 ? 1 : 0);
				e2 = e2->next_in_ael;
			}
		else
			while (e2 != &e) {
				if (GetPolyType(*e2) != pt && !IsOpen(*e2))
					e.wind_cnt2 += e2->wind_dx;
				e2 = e2->next_in_ael;
			}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::SetWindCountForOpenPathEdge(Active&e)
	{
		using namespace engine::detail;
		Active*e2 = actives_;
		if (fillrule_ == FillRule::EvenOdd) 
		{
			int cnt1 = 0, cnt2 = 0;
			while (e2 != &e) {
				if (GetPolyType(*e2) == PathType::Clip)
					cnt2++;
				else if (!IsOpen(*e2))
					cnt1++;
				e2 = e2->next_in_ael;
			}
			e.wind_cnt = (IsOdd(cnt1) ? 1 : 0);
			e.wind_cnt2 = (IsOdd(cnt2) ? 1 : 0);
		} 
		else 
		{
			while (e2 != &e) 
			{
				if (GetPolyType(*e2) == PathType::Clip)
					e.wind_cnt2 += e2->wind_dx;
				else if (!IsOpen(*e2))
					e.wind_cnt += e2->wind_dx;
				e2 = e2->next_in_ael;
			}
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::InsertLeftEdge(Active&e)
	{
		using namespace engine::detail;
		Active*e2;
		if (!actives_) {
			e.prev_in_ael = NULL;
			e.next_in_ael = NULL;
			actives_ = &e;
		} else if (!IsValidAelOrder<PointType, ClipperFlags>(*actives_, e)) {
			e.prev_in_ael = NULL;
			e.next_in_ael = actives_;
			actives_->prev_in_ael = &e;
			actives_ = &e;
		} else {
			e2 = actives_;
			while (e2->next_in_ael && IsValidAelOrder<PointType, ClipperFlags>(*e2->next_in_ael, e))
				e2 = e2->next_in_ael;
			e.next_in_ael = e2->next_in_ael;
			if (e2->next_in_ael) e2->next_in_ael->prev_in_ael = &e;
			e.prev_in_ael = e2;
			e2->next_in_ael = &e;
		}
	}
	//----------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::InsertLocalMinimaIntoAEL(int64_t bot_y) 
	{
		using namespace engine::detail;

		LocalMinima*local_minima;
		Active*left_bound, *right_bound;
		//Add any local minima (if any) at BotY ...
		//nb: horizontal local minima edges should contain locMin.vertex.prev

		while (PopLocalMinima(bot_y, local_minima)) {
			if ((local_minima->vertex->flags & VertexFlags::OpenStart) != VertexFlags::None) {
				left_bound = NULL;
			} 
			else 
			{
				left_bound = new Active();
				left_bound->bot = local_minima->vertex->pt;
				left_bound->curr_x = GetX(left_bound->bot);
				if constexpr (ClipperFlags::reverse_orientation)
					left_bound->wind_dx = -1;
				else
					left_bound->wind_dx = 1;
				left_bound->vertex_top = local_minima->vertex->prev;  //ie descending
				left_bound->top = left_bound->vertex_top->pt;
				left_bound->outrec = NULL;
				left_bound->local_min = local_minima;
				SetDx(*left_bound);
			}

			if ((local_minima->vertex->flags & VertexFlags::OpenEnd) != VertexFlags::None) {
				right_bound = NULL;
			} else {
				right_bound = new Active();
				right_bound->bot = local_minima->vertex->pt;
				right_bound->curr_x = GetX(right_bound->bot);
				if constexpr (ClipperFlags::reverse_orientation)
					right_bound->wind_dx = 1;
				else
					right_bound->wind_dx = -1,
				right_bound->vertex_top = local_minima->vertex->next;  //ie ascending
				right_bound->top = right_bound->vertex_top->pt;
				right_bound->outrec = NULL;
				right_bound->local_min = local_minima;
				SetDx(*right_bound);
			}

			//Currently LeftB is just the descending bound and RightB is the ascending.
			//Now if the LeftB isn't on the left of RightB then we need swap them.
			if (left_bound && right_bound) {
				if (IsHorizontal(*left_bound)) {
					if (IsHeadingRightHorz(*left_bound)) SwapActives(left_bound, right_bound);
				} else if (IsHorizontal(*right_bound)) {
					if (IsHeadingLeftHorz(*right_bound)) SwapActives(left_bound, right_bound);
				} else if (left_bound->dx < right_bound->dx)
					SwapActives(left_bound, right_bound);
			} else if (!left_bound) {
				left_bound = right_bound;
				right_bound = NULL;
			}

			bool contributing;
			left_bound->is_left_bound = true;
			InsertLeftEdge(*left_bound); 

			if (IsOpen(*left_bound))
			{
				SetWindCountForOpenPathEdge(*left_bound);
				contributing = IsContributingOpen(*left_bound);
			} 
			else 
			{
				SetWindCountForClosedPathEdge(*left_bound);
				contributing = IsContributingClosed(*left_bound);
			}

			if (right_bound) 
			{
				right_bound->is_left_bound = false;
				right_bound->wind_cnt = left_bound->wind_cnt;
				right_bound->wind_cnt2 = left_bound->wind_cnt2;
				InsertRightEdge(*left_bound, *right_bound);  ///////
				if (contributing)
				{
					AddLocalMinPoly(*left_bound, *right_bound, left_bound->bot, true);
					if (!IsHorizontal(*left_bound) && TestJoinWithPrev1(*left_bound, bot_y))
					{
						OutPt* op = AddOutPt(*left_bound->prev_in_ael, left_bound->bot);
						AddJoin(op, left_bound->outrec->pts);
					}
				}
				
				while (right_bound->next_in_ael &&
					IsValidAelOrder<PointType, ClipperFlags>(*right_bound->next_in_ael, *right_bound))
				{
					IntersectEdges(*right_bound, *right_bound->next_in_ael, right_bound->bot);
					SwapPositionsInAEL(*right_bound, *right_bound->next_in_ael);
				}

				if (!IsHorizontal(*right_bound) &&
					TestJoinWithNext1(*right_bound, bot_y))
				{
					OutPt* op = AddOutPt(*right_bound->next_in_ael, right_bound->bot);
					AddJoin(right_bound->outrec->pts, op);
				}

				if (IsHorizontal(*right_bound))
					PushHorz(*right_bound);
				else
					InsertScanline(GetY(right_bound->top));
			} 
			else if (contributing)
			{
				StartOpenPath(*left_bound, left_bound->bot);
			}

			if (IsHorizontal(*left_bound))
				PushHorz(*left_bound);
			else
				InsertScanline(GetY(left_bound->top));
		}  //while (PopLocalMinima())
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::PushHorz(Active&e)
	{
		e.next_in_sel = (sel_ ? sel_ : NULL);
		sel_ = &e;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::PopHorz(Active*&e)
	{
		e = sel_;
		if (!e) return false;
		sel_ = sel_->next_in_sel;
		return true;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::TestJoinWithPrev1(const Active& e, int64_t curr_y)
	{
		using namespace engine::detail;
		//this is marginally quicker than TestJoinWithPrev2
		//but can only be used when e.PrevInAEL.currX is accurate
		return IsHotEdge(e) && !IsOpen(e) &&
			e.prev_in_ael && e.prev_in_ael->curr_x == e.curr_x &&
			IsHotEdge(*e.prev_in_ael) && !IsOpen(*e.prev_in_ael) &&
			(curr_y - GetY(e.top) > 1) && (curr_y - GetY(e.prev_in_ael->top) > 1) &&
			(CrossProduct(e.prev_in_ael->top, e.bot, e.top) == 0);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::TestJoinWithPrev2(const Active& e, const PointType& curr_pt)
	{
		using namespace engine::detail;
		return IsHotEdge(e) && !IsOpen(e) &&
			e.prev_in_ael && !IsOpen(*e.prev_in_ael) &&
			IsHotEdge(*e.prev_in_ael) && (GetY(e.prev_in_ael->top) < GetY(e.bot)) &&
			(std::abs(TopX(*e.prev_in_ael, GetY(curr_pt)) - GetX(curr_pt)) < 2) &&
			(CrossProduct(e.prev_in_ael->top, curr_pt, e.top) == 0);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::TestJoinWithNext1(const Active& e, int64_t curr_y)
	{
		using namespace engine::detail;
		//this is marginally quicker than TestJoinWithNext2
		//but can only be used when e.NextInAEL.currX is accurate
		return IsHotEdge(e) && !IsOpen(e) &&
			e.next_in_ael && (e.next_in_ael->curr_x == e.curr_x) &&
			IsHotEdge(*e.next_in_ael) && !IsOpen(*e.next_in_ael) &&
			(curr_y - GetY(e.top) > 1) && (curr_y - GetY(e.next_in_ael->top) > 1) &&
			(CrossProduct(e.next_in_ael->top, e.bot, e.top) == 0);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::TestJoinWithNext2(const Active& e, const PointType& curr_pt)
	{
		using namespace engine::detail;
		return IsHotEdge(e) && !IsOpen(e) &&
			e.next_in_ael && !IsOpen(*e.next_in_ael) &&
			IsHotEdge(*e.next_in_ael) && (GetY(e.next_in_ael->top) < GetY(e.bot)) &&
			(std::abs(TopX(*e.next_in_ael, GetY(curr_pt)) - GetX(curr_pt)) < 2) &&
			(CrossProduct(e.next_in_ael->top, curr_pt, e.top) == 0);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::OutPt<PointType>* ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddLocalMinPoly(Active & e1, Active & e2,
		const PointType& pt, bool is_new)
	{
		using namespace engine::detail;

		auto *outrec = new OutRec();
		outrec->idx = (unsigned)outrec_list_.size();
		outrec_list_.push_back(outrec);
		outrec->pts = NULL;
		outrec->polypath = NULL;

		e1.outrec = outrec;
		SetOwnerAndInnerOuterState(e1);
		//flag when orientation needs to be rechecked later ...
		e2.outrec = outrec;

		if (!IsOpen(e1)) {
			//Setting the owner and inner/outer states (above) is an essential
			//precursor to setting edge 'sides' (ie left and right sides of output
			//polygons) and hence the orientation of output paths ...
			if (IsOuter(*outrec) == is_new)
				SetSides(*outrec, e1, e2);
			else
				SetSides(*outrec, e2, e1);
		}
		auto *op = new OutPt(pt, outrec);
		outrec->pts = op;
		return op;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::FixSides(Active& e, Active& e2)
	{
		using namespace engine::detail;

		if (ValidateClosedPathEx(e.outrec) && ValidateClosedPathEx(e2.outrec))
		{
			if (CheckFixInnerOuter(e) &&
				IsOuter(*e.outrec) == IsFront(e))
				SwapSides(*e.outrec);
			else if (CheckFixInnerOuter(e2) &&
				IsOuter(*e2.outrec) == IsFront(e2))
				SwapSides(*e2.outrec);
			else
				error_found_ = true;
		}
		else if (!e.outrec->pts)
		{
			if (ValidateClosedPathEx(e2.outrec))
				error_found_ = true;
			UncoupleOutRec(e);
			UncoupleOutRec(e2);
			//fixed, but there's nothing to terminate in AddLocalMaxPoly
		}
		else
			error_found_ = true;

		return !error_found_;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::OutPt<PointType>* ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddLocalMaxPoly(Active&e1, Active&e2, const PointType& pt)
	{
		using namespace engine::detail;

		if (IsFront(e1) == IsFront(e2))
		{
			if (IsOpen(e1))
				SwapSides(*e2.outrec);
			else if (!FixSides(e1, e2))
				return NULL;
		}

		OutPt* result = AddOutPt(e1, pt);
		if (e1.outrec == e2.outrec) {
			OutRec& outrec = *e1.outrec;
			outrec.pts = result;
			UncoupleOutRec(e1);
			if (!IsOpen(e1))
				CleanCollinear(&outrec);
			result = outrec.pts;
		}
		//and to preserve the winding orientation of outrec ...
		else if (e1.outrec->idx < e2.outrec->idx)
			JoinOutrecPaths(e1, e2);
		else
			JoinOutrecPaths(e2, e1);

		return result;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::JoinOutrecPaths(Active&e1, Active&e2)
	{
		using namespace engine::detail;

		//join e2 outrec path onto e1 outrec path and then delete e2 outrec path
		//pointers. (NB Only very rarely do the joining ends share the same coords.)
		OutPt *p1_st = e1.outrec->pts;
		OutPt *p2_st = e2.outrec->pts;
		OutPt *p1_end = p1_st->next;
		OutPt *p2_end = p2_st->next;
		if (IsFront(e1)) {
			p2_end->prev = p1_st;
			p1_st->next = p2_end;
			p2_st->next = p1_end;
			p1_end->prev = p2_st;
			e1.outrec->pts = p2_st;
			if (!IsOpen(e1))
			{
				e1.outrec->front_edge = e2.outrec->front_edge;
				e1.outrec->front_edge->outrec = e1.outrec;
			}
		} 
		else 
		{
			p1_end->prev = p2_st;
			p2_st->next = p1_end;
			p1_st->next = p2_end;
			p2_end->prev = p1_st;
			if (!IsOpen(e1))
			{
				e1.outrec->back_edge = e2.outrec->back_edge;
				e1.outrec->back_edge->outrec = e1.outrec;
			}
		}
		//after joining, the e2.OutRec must contains no vertices ...
		e2.outrec->front_edge = NULL;
		e2.outrec->back_edge = NULL;
		e2.outrec->pts = NULL;
		e2.outrec->owner = e1.outrec;

		if (IsOpenEnd(e1))
		{
			e2.outrec->pts = e1.outrec->pts;
			e1.outrec->pts = NULL;
		}
		
		//and e1 and e2 are maxima and are about to be dropped from the Actives list.
		e1.outrec = NULL;
		e2.outrec = NULL;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::OutPt<PointType>* ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddOutPt(const Active&e, const PointType& pt)
	{
		OutPt*new_op = NULL;

		//Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
		//op_front[.Prev]* ~~~> op_back & op_back == op_front.Next
		OutRec *outrec = e.outrec;
		bool to_front = engine::detail::IsFront(e);
		OutPt*op_front = outrec->pts;
		OutPt*op_back = op_front->next;

		if (to_front && EqualsXY(pt, op_front->pt))
			new_op = op_front;
		else if (!to_front && EqualsXY(pt, op_back->pt))
			new_op = op_back;
		else {
			new_op = new OutPt(pt, outrec);
			op_back->prev = new_op;
			new_op->prev = op_front;
			new_op->next = op_back;
			op_front->next = new_op;
			if (to_front) outrec->pts = new_op;
		}
		return new_op;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::ValidateClosedPathEx(OutRec* outrec)
	{
		if (engine::detail::IsValidClosedPath(outrec->pts)) return true;
		SafeDisposeOutPts(outrec->pts);
		return false;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::CleanCollinear(OutRec* outrec)
	{
		using namespace engine::detail;

		outrec = GetRealOutRec(outrec);
		if (!outrec || outrec->front_edge || !ValidateClosedPathEx(outrec)) return;

		OutPt *startOp = outrec->pts, *op2 = startOp;
		for (; ; )
		{
			if (op2->joiner) return;

			//NB if preserveCollinear == true, then only remove 180 deg. spikes
			if ((CrossProduct(op2->prev->pt, op2->pt, op2->next->pt) == 0) &&
			 (EqualsXY(op2->pt, op2->prev->pt) ||
					EqualsXY(op2->pt, op2->next->pt) || !PreserveCollinear ||
					DotProduct(op2->prev->pt, op2->pt, op2->next->pt) < 0))
			{
				
				if (op2 == outrec->pts) outrec->pts = op2->prev;

				op2 = DisposeOutPt(op2);
				if (!ValidateClosedPathEx(outrec))
				{
					outrec->pts = NULL;
					return;
				}
				startOp = op2;
				continue;
			}
			op2 = op2->next;
			if (op2 == startOp) break;
		}
		FixSelfIntersects(outrec);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::OutPt<PointType>* ClipperBase<PointType, ZFillFunc, ClipperFlags>::DoSplitOp(OutPt* outRecOp, OutPt* splitOp)
	{
		using namespace engine::detail;

		OutPt *prevOp = splitOp->prev, *nextNextOp = splitOp->next->next;
		OutPt *result = prevOp;
		PointD ipD = point_traits<PointD>::construct(0, 0);
		GetIntersectPoint(prevOp->pt, 
			splitOp->pt, splitOp->next->pt, nextNextOp->pt, ipD);
		using CoordType = typename PointType::coordinate_type;
		PointType ip { point_traits<PointType>::construct(CoordType(std::round(GetX(ipD))), CoordType(std::round(GetY(ipD)))) };
		zfill_func_(prevOp->pt, splitOp->pt, splitOp->next->pt, nextNextOp->pt, ip);
		double area1 = Area(outRecOp);
		double area2 = AreaTriangle(ip, splitOp->pt, splitOp->next->pt);

		if (EqualsXY(ip, prevOp->pt) || EqualsXY(ip, nextNextOp->pt))
		{
			nextNextOp->prev = prevOp;
			prevOp->next = nextNextOp;
		}
		else
		{
			auto *newOp2 = new OutPt(ip, prevOp->outrec);
			newOp2->prev = prevOp;
			newOp2->next = nextNextOp;
			nextNextOp->prev = newOp2;
			prevOp->next = newOp2;
		}

		SafeDeleteOutPtJoiners(splitOp->next);
		SafeDeleteOutPtJoiners(splitOp);

		double absArea2 = std::abs(area2);
		if ((absArea2 >= 1) &&
			((absArea2 > std::abs(abs(area1)) ||
				((area2 > 0) == (area1 > 0)))))
		{
			auto *newOutRec = new OutRec();
			newOutRec->idx = outrec_list_.size();
			outrec_list_.push_back(newOutRec);
			newOutRec->owner = prevOp->outrec->owner;
			newOutRec->state = prevOp->outrec->state;
			newOutRec->polypath = NULL;
			splitOp->outrec = newOutRec;
			splitOp->next->outrec = newOutRec;

			auto *newOp = new OutPt(ip, newOutRec);
			newOp->prev = splitOp->next;
			newOp->next = splitOp;
			newOutRec->pts = newOp;
			splitOp->prev = newOp;
			splitOp->next->next = newOp;
		} 
		else
		{
			delete splitOp->next;
			delete splitOp;
		}
		return result;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::FixSelfIntersects(OutRec* outrec)
	{
		OutPt* op2 = outrec->pts;
		for (; ; )
		{
			//3 edged polygons can't self-intersect
			if (op2->prev == op2->next->next) break;
			if (engine::detail::SegmentsIntersect(op2->prev->pt,
				op2->pt, op2->next->pt, op2->next->next->pt))
			{
				if (op2 == outrec->pts || op2->next == outrec->pts)
					outrec->pts = outrec->pts->prev;
				op2 = DoSplitOp(outrec->pts, op2);
				outrec->pts = op2;
				continue;
			}
			else
				op2 = op2->next;

			if (op2 == outrec->pts) break;
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::SafeDisposeOutPts(OutPt* op)
	{
		OutRec* outrec = engine::detail::GetRealOutRec(op->outrec);
		if (outrec->front_edge)
			outrec->front_edge->outrec = NULL;
		if (outrec->back_edge)
			outrec->back_edge->outrec = NULL;
		outrec->pts = NULL;

		op->prev->next = NULL;
		while (op)
		{
			SafeDeleteOutPtJoiners(op);
			OutPt* tmp = op->next;
			delete op;
			op = tmp;
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::CompleteSplit(OutPt* op1, OutPt* op2, OutRec& outrec)
	{
		using namespace engine::detail;

		double area1 = Area(op1);
		double area2 = Area(op2);
		if (std::abs(area1) < 1)
		{
			SafeDisposeOutPts(op1);
			outrec.pts = op2;
		}
		else if (std::abs(area2) < 1)
		{
			SafeDisposeOutPts(op2);
			outrec.pts = op1;
		}
		else
		{
			auto *newOr = new OutRec();
			newOr->idx = outrec_list_.size();
			outrec_list_.push_back(newOr);
			newOr->polypath = NULL;

			if (std::abs(area1) >= std::abs(area2))
			{
				outrec.pts = op1;
				newOr->pts = op2;
			}
			else
			{
				outrec.pts = op2;
				newOr->pts = op1;
			}

			if ((area1 > 0) == (area2 > 0))
			{
				newOr->owner = outrec.owner;
				newOr->state = outrec.state;
			}
			else
			{
				newOr->owner = &outrec;
				if (outrec.state == OutRecState::Outer)
					newOr->state = OutRecState::Inner;
				else
					newOr->state = OutRecState::Outer;
			}

			UpdateOutrecOwner(newOr);
			CleanCollinear(newOr);
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::OutPt<PointType>* ClipperBase<PointType, ZFillFunc, ClipperFlags>::StartOpenPath(Active&e, const PointType& pt) {
		auto *outrec = new OutRec();
		outrec->idx = outrec_list_.size();
		outrec_list_.push_back(outrec);
		outrec->owner = NULL;
		outrec->state = OutRecState::Open;
		outrec->pts = NULL;
		outrec->polypath = NULL;
		outrec->back_edge = NULL;
		outrec->front_edge = NULL;
		e.outrec = outrec;

		auto *op = new OutPt(pt, outrec);
		outrec->pts = op;
		return op;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::UpdateEdgeIntoAEL(Active*e) 
	{
		using namespace engine::detail;

		e->bot = e->top;
		e->vertex_top = NextVertex<PointType, ClipperFlags>(*e);
		e->top = e->vertex_top->pt;
		e->curr_x = GetX(e->bot);
		SetDx(*e);
		if (IsHorizontal(*e)) return;
		InsertScanline(GetY(e->top));
		if (TestJoinWithPrev1(*e, GetY(e->bot)))
		{
			OutPt* op1 = AddOutPt(*e->prev_in_ael, e->bot);
			OutPt* op2 = AddOutPt(*e, e->bot);
			AddJoin(op1, op2);
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::OutPt<PointType>*ClipperBase<PointType, ZFillFunc, ClipperFlags>::IntersectEdges(Active & e1, Active & e2, const PointType & pt)
	{
		using namespace engine::detail;

		OutPt* resultOp = NULL;

		//MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
		if (has_open_paths_ && (IsOpen(e1) || IsOpen(e2))) {
			if (IsOpen(e1) && IsOpen(e2)) return NULL;
			Active*edge_o, *edge_c;
			if (IsOpen(e1)) {
				edge_o = &e1;
				edge_c = &e2;
			} else {
				edge_o = &e2;
				edge_c = &e1;
			}

			switch (cliptype_) {
				case ClipType::Intersection:
				case ClipType::Difference:
					if (IsSamePolyType(*edge_o, *edge_c) || (abs(edge_c->wind_cnt) != 1)) return NULL;
					break;
				case ClipType::Union:
					if (IsHotEdge(*edge_o) != ((abs(edge_c->wind_cnt) != 1) ||
														(IsHotEdge(*edge_o) != (edge_c->wind_cnt != 0)))) return NULL;
					break;
				case ClipType::Xor:
					if (abs(edge_c->wind_cnt) != 1) return NULL;
					break;
				case ClipType::None:
					error_found_ = true;
					break;
			}	
			//toggle contribution ...
			if (IsHotEdge(*edge_o))
			{
				resultOp = AddOutPt(*edge_o, pt);
				SetZ(e1, e2, resultOp->pt);
				edge_o->outrec = NULL;
			} 
			else
			{
				resultOp = StartOpenPath(*edge_o, pt);
			}
			return resultOp;
		}

		//UPDATE WINDING COUNTS...

		int old_e1_windcnt, old_e2_windcnt;
		if (e1.local_min->polytype == e2.local_min->polytype) {
			if (fillrule_ == FillRule::EvenOdd) 
			{
				old_e1_windcnt = e1.wind_cnt;
				e1.wind_cnt = e2.wind_cnt;
				e2.wind_cnt = old_e1_windcnt;
			} 
			else 
			{
				if (e1.wind_cnt + e2.wind_dx == 0)
					e1.wind_cnt = -e1.wind_cnt;
				else
					e1.wind_cnt += e2.wind_dx;
				if (e2.wind_cnt - e1.wind_dx == 0)
					e2.wind_cnt = -e2.wind_cnt;
				else
					e2.wind_cnt -= e1.wind_dx;
			}
		} 
		else 
		{
			if (fillrule_ != FillRule::EvenOdd)
			{
				e1.wind_cnt2 += e2.wind_dx;
				e2.wind_cnt2 -= e1.wind_dx;
			}
			else
			{
				e1.wind_cnt2 = (e1.wind_cnt2 == 0 ? 1 : 0);
				e2.wind_cnt2 = (e2.wind_cnt2 == 0 ? 1 : 0);
			}
		}

		switch (fillrule_) {
			case FillRule::Positive:
				old_e1_windcnt = e1.wind_cnt;
				old_e2_windcnt = e2.wind_cnt;
				break;
			case FillRule::Negative:
				old_e1_windcnt = -e1.wind_cnt;
				old_e2_windcnt = -e2.wind_cnt;
				break;
			default:
				old_e1_windcnt = abs(e1.wind_cnt);
				old_e2_windcnt = abs(e2.wind_cnt);
				break;
		}

		const bool e1_windcnt_in_01 = old_e1_windcnt == 0 || old_e1_windcnt == 1;
		const bool e2_windcnt_in_01 = old_e2_windcnt == 0 || old_e2_windcnt == 1;

		if ((!IsHotEdge(e1) && !e1_windcnt_in_01) || (!IsHotEdge(e2) && !e2_windcnt_in_01)) {
			return NULL;
		}
		//NOW PROCESS THE INTERSECTION ...

		//if both edges are 'hot' ...
		if (IsHotEdge(e1) && IsHotEdge(e2)) {
			if ((old_e1_windcnt != 0 && old_e1_windcnt != 1) || (old_e2_windcnt != 0 && old_e2_windcnt != 1) ||
					(e1.local_min->polytype != e2.local_min->polytype && cliptype_ != ClipType::Xor)) 
			{
				resultOp = AddLocalMaxPoly(e1, e2, pt);
				if (resultOp) SetZ(e1, e2, resultOp->pt);
			}
			else if (IsFront(e1) || (e1.outrec == e2.outrec))
			{
				resultOp = AddLocalMaxPoly(e1, e2, pt);
				OutPt* op2 = AddLocalMinPoly(e1, e2, pt);
				if (resultOp) SetZ(e1, e2, resultOp->pt);
				SetZ(e1, e2, op2->pt);
				if (resultOp && EqualsXY(resultOp->pt, op2->pt) &&
					!IsHorizontal(e1) && !IsHorizontal(e2) &&
					(CrossProduct(e1.bot, resultOp->pt, e2.bot) == 0))
						AddJoin(resultOp, op2);
			}
			else 
			{
				resultOp = AddOutPt(e1, pt);
				OutPt* op2 = AddOutPt(e2, pt);
				SetZ(e1, e2, resultOp->pt);
				SetZ(e1, e2, op2->pt);
				SwapOutrecs(e1, e2);
			}
		}		
		else if (IsHotEdge(e1))
		{
			resultOp = AddOutPt(e1, pt);
			SetZ(e1, e2, resultOp->pt);
			SwapOutrecs(e1, e2);
		} 
		else if (IsHotEdge(e2))
		{
			resultOp = AddOutPt(e2, pt);
			SetZ(e1, e2, resultOp->pt);
			SwapOutrecs(e1, e2);
		} 
		else 
		{  
			int64_t e1Wc2, e2Wc2;
			switch (fillrule_) {
			case FillRule::Positive:
					e1Wc2 = e1.wind_cnt2;
					e2Wc2 = e2.wind_cnt2;
					break;
				case FillRule::Negative:
					e1Wc2 = -e1.wind_cnt2;
					e2Wc2 = -e2.wind_cnt2;
					break;
				default:
					e1Wc2 = abs(e1.wind_cnt2);
					e2Wc2 = abs(e2.wind_cnt2);
					break;
			}

			if (!IsSamePolyType(e1, e2))
			{
				resultOp = AddLocalMinPoly(e1, e2, pt, false);
				SetZ(e1, e2, resultOp->pt);
			}
			else if (old_e1_windcnt == 1 && old_e2_windcnt == 1)
			{
				resultOp = NULL;
				switch (cliptype_) 
				{
				case ClipType::Union:
					if (e1Wc2 <= 0 && e2Wc2 <= 0)
						resultOp = AddLocalMinPoly(e1, e2, pt, false);
					break;
				case ClipType::Difference:
					if (((GetPolyType(e1) == PathType::Clip) && (e1Wc2 > 0) && (e2Wc2 > 0)) ||
						((GetPolyType(e1) == PathType::Subject) && (e1Wc2 <= 0) && (e2Wc2 <= 0))) {
						resultOp = AddLocalMinPoly(e1, e2, pt, false);
					}
					break;
				case ClipType::Xor:
					resultOp = AddLocalMinPoly(e1, e2, pt, false);
					break;
				default:
						if (e1Wc2 > 0 && e2Wc2 > 0)
							resultOp = AddLocalMinPoly(e1, e2, pt, false);
						break;
				}
				if (resultOp) SetZ(e1, e2, resultOp->pt);
			}
		}
		return resultOp;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DeleteFromAEL(Active&e)
	{
		Active*prev = e.prev_in_ael;
		Active*next = e.next_in_ael;
		if (!prev && !next && (&e != actives_)) return;  //already deleted
		if (prev)
			prev->next_in_ael = next;
		else
			actives_ = next;
		if (next) next->prev_in_ael = prev;
		delete &e;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::AdjustCurrXAndCopyToSEL(const int64_t top_y)
	{
		Active*e = actives_;
		sel_ = e;
		while (e) {
			e->prev_in_sel = e->prev_in_ael;
			e->next_in_sel = e->next_in_ael;
			e->jump = e->next_in_sel;
			e->curr_x = engine::detail::TopX(*e, top_y);
			e = e->next_in_ael;
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::ExecuteInternal(ClipType ct, FillRule fillrule)
	{
		fillrule_ = fillrule;
		cliptype_ = ct;
		Reset();
		int64_t y;
		if (ct == ClipType::None || !PopScanline(y)) return true;

		while (!error_found_) {
			InsertLocalMinimaIntoAEL(y);
			Active*e;
			while (PopHorz(e)) DoHorizontal(*e);
			if (horz_joiners_) ConvertHorzTrialsToJoins();
			bot_y_ = y;  //bot_y_ == bottom of scanbeam
			if (!PopScanline(y)) break;  //y new top of scanbeam
			DoIntersections(y);
			DoTopOfScanbeam(y);
			while (PopHorz(e)) DoHorizontal(*e);
		}
		ProcessJoinerList();
		return !error_found_;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::Execute(ClipType clip_type,
		FillRule fill_rule, Paths &solution_closed)
	{
		solution_closed.clear();
		if (ExecuteInternal(clip_type, fill_rule))
			BuildPaths(solution_closed, NULL);
		CleanUp();
		return !error_found_;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::Execute(ClipType clip_type, FillRule fill_rule,
		Paths& solution_closed, Paths& solution_open) 
	{
		bool result = true;
		solution_closed.clear();
		solution_open.clear();
		if (ExecuteInternal(clip_type, fill_rule))
			BuildPaths(solution_closed, &solution_open);
		CleanUp();
		return !error_found_;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::Execute(ClipType clip_type,
		FillRule fill_rule, PolyTree64& polytree, Paths& solution_open)
	{
		polytree.Clear();
		solution_open.clear();
		if (ExecuteInternal(clip_type, fill_rule))
			BuildTree(polytree, solution_open);
		CleanUp();
		return !error_found_;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DoIntersections(const int64_t top_y) {
		if (BuildIntersectList(top_y)) {
			ProcessIntersectList();
			DisposeIntersectNodes();
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DisposeIntersectNodes()
	{
		for (auto node : intersect_nodes_) delete node;
		intersect_nodes_.resize(0);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddNewIntersectNode(Active&e1, Active&e2, int64_t top_y)
	{
		using namespace engine::detail;

		PointType pt = GetIntersectPoint(e1, e2);

		//rounding errors can occasionally place the calculated intersection
		//point either below or above the scanbeam, so check and correct ...
		if (GetY(pt) > bot_y_) {
			//e.curr.y is still the bottom of scanbeam
			point_traits<PointType>::set(pt, 1, bot_y_);
			//use the more vertical of the 2 edges to derive pt.x ...
			point_traits<PointType>::set(pt, 0, abs(e1.dx) < abs(e2.dx) ? TopX(e1, bot_y_) : TopX(e2, bot_y_));
		} else if (GetY(pt) < top_y) {
			//top_y is at the top of the scanbeam
			point_traits<PointType>::set(pt, 1, top_y);
			point_traits<PointType>::set(pt, 0,
				GetY(e1.top) == top_y ? GetX(e1.top) :
				GetY(e2.top) == top_y ? GetX(e2.top) :
				abs(e1.dx) < abs(e2.dx) ? e1.curr_x : e2.curr_x);
		}

		intersect_nodes_.push_back(new IntersectNode(&e1, &e2, pt));
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::BuildIntersectList(const int64_t top_y)
	{
		using namespace engine::detail;

		if (!actives_ || !actives_->next_in_ael) return false;

		//Calculate edge positions at the top of the current scanbeam, and from this
		//we will determine the intersections required to reach these new positions.
		AdjustCurrXAndCopyToSEL(top_y);

		//Find all edge intersections in the current scanbeam using a stable merge
		//sort that ensures only adjacent edges are intersecting. Intersect info is
		//stored in FIntersectList ready to be processed in ProcessIntersectList.
		//Re merge sorts see https://stackoverflow.com/a/46319131/359538

		Active*left = sel_, *right, *l_end, *r_end, *curr_base, *tmp;

		while (left && left->jump) 
		{
			Active* prev_base = NULL;
			while (left && left->jump)
			{
				curr_base = left;
				right = left->jump;
				l_end = right;
				r_end = right->jump;
				left->jump = r_end;
				while (left != l_end && right != r_end)
				{
					if (right->curr_x < left->curr_x)
					{
						tmp = right->prev_in_sel;
						for (; ; )
						{
							AddNewIntersectNode(*tmp, *right, top_y);
							if (tmp == left) break;
							tmp = tmp->prev_in_sel;
						}

						tmp = right;
						right = ExtractFromSEL(tmp);
						l_end = right;
						Insert1Before2InSEL(tmp, left);
						if (left == curr_base)
						{
							curr_base = tmp;
							curr_base->jump = r_end;
							if (!prev_base) sel_ = curr_base;
							else prev_base->jump = curr_base;
						}
					}
					else left = left->next_in_sel;
				}
				prev_base = curr_base;
				left = r_end;
			}
			left = sel_;
		}
		return intersect_nodes_.size() > 0;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::ProcessIntersectList()
	{
		using namespace engine::detail;

		//We now have a list of intersections required so that edges will be
		//correctly positioned at the top of the scanbeam. However, it's important
		//that edge intersections are processed from the bottom up, but it's also
		//crucial that intersections only occur between adjacent edges.

		//First we do a quicksort so intersections proceed in a bottom up order ...
		std::sort(intersect_nodes_.begin(), intersect_nodes_.end(), IntersectListSort<PointType>);
		//Now as we process these intersections, we must sometimes adjust the order
		//to ensure that intersecting edges are always adjacent ...
		
		std::vector<IntersectNode*>::iterator node_iter, node_iter2;
		for (node_iter = intersect_nodes_.begin();
			node_iter != intersect_nodes_.end();  ++node_iter)
		{
			if (!EdgesAdjacentInAEL(*(*node_iter)))
			{
				node_iter2 = node_iter + 1;
				while (node_iter2 != intersect_nodes_.end() && 
					!EdgesAdjacentInAEL(*(*node_iter2))) ++node_iter2;
				if (node_iter2 != intersect_nodes_.end())
					std::swap(*node_iter, *node_iter2);
			}

			IntersectNode* node = *node_iter;
			IntersectEdges(*node->edge1, *node->edge2, node->pt);
			SwapPositionsInAEL(*node->edge1, *node->edge2);

			if (TestJoinWithPrev2(*node->edge2, node->pt))
			{
				OutPt* op1 = AddOutPt(*node->edge2->prev_in_ael, node->pt);
				OutPt* op2 = AddOutPt(*node->edge2, node->pt);
				if (op1 != op2) AddJoin(op1, op2);
			}
			else if (TestJoinWithNext2(*node->edge1, node->pt))
			{
				OutPt* op1 = AddOutPt(*node->edge1, node->pt);
				OutPt* op2 = AddOutPt(*node->edge1->next_in_ael, node->pt);
				if (op1 != op2) AddJoin(op1, op2);
			}
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::SwapPositionsInAEL(Active&e1, Active&e2)
	{
		//preconditon: e1 must be immediately to the left of e2
		Active*next = e2.next_in_ael;
		if (next) next->prev_in_ael = &e1;
		Active*prev = e1.prev_in_ael;
		if (prev) prev->next_in_ael = &e2;
		e2.prev_in_ael = prev;
		e2.next_in_ael = &e1;
		e1.prev_in_ael = &e2;
		e1.next_in_ael = next;
		if (!e2.prev_in_ael) actives_ = &e2;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::ResetHorzDiRect64on(const Active&horz,
		const Active*max_pair, int64_t &horz_left, int64_t &horz_right)
	{
		if (GetX(horz.bot) == GetX(horz.top)) {
			//the horizontal edge is going nowhere ...
			horz_left = horz.curr_x;
			horz_right = horz.curr_x;
			Active*e = horz.next_in_ael;
			while (e && e != max_pair) e = e->next_in_ael;
			return e != NULL;
		} else if (horz.curr_x < GetX(horz.top)) {
			horz_left = horz.curr_x;
			horz_right = GetX(horz.top);
			return true;
		} else {
			horz_left = GetX(horz.top);
			horz_right = horz.curr_x;
			return false;  //right to left
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DoHorizontal(Active&horz)
	/*******************************************************************************
			* Notes: Horizontal edges (HEs) at scanline intersections (ie at the top or    *
			* bottom of a scanbeam) are processed as if layered.The order in which HEs     *
			* are processed doesn't matter. HEs intersect with the bottom vertices of      *
			* other HEs[#] and with non-horizontal edges [*]. Once these intersections     *
			* are completed, intermediate HEs are 'promoted' to the next edge in their     *
			* bounds, and they in turn may be intersected[%] by other HEs.                 *
			*                                                                              *
			* eg: 3 horizontals at a scanline:    /   |                     /           /  *
			*              |                     /    |     (HE3)o ========%========== o   *
			*              o ======= o(HE2)     /     |         /         /                *
			*          o ============#=========*======*========#=========o (HE1)           *
			*         /              |        /       |       /                            *
			*******************************************************************************/
	{
		using namespace engine::detail;

		PointType pt = point_traits<PointType>::construct(0, 0);
		bool horzIsOpen = IsOpen(horz);
		int64_t y = GetY(horz.bot);
		Active* max_pair = NULL;
		bool isMax = IsMaxima(horz);

		//remove 180 deg.spikes and also with closed paths and not PreserveCollinear
		//simplify consecutive horizontals into a 'single' edge ...
		if (!horzIsOpen && !isMax && TrimHorz<PointType, ClipperFlags>(horz, PreserveCollinear))
			isMax = IsMaxima(horz);

		if (isMax && !IsOpenEnd(horz))
			max_pair = GetMaximaPair(horz);

		int64_t horz_left, horz_right;
		bool is_left_to_right =
			ResetHorzDiRect64on(horz, max_pair, horz_left, horz_right);

		if (IsHotEdge(horz))
			AddOutPt(horz, point_traits<PointType>::construct(horz.curr_x, y));

		OutPt* op;
		while (true) {  //loops through consec. horizontal edges (if open)
			Active*e;
			if (is_left_to_right) e = horz.next_in_ael;
			else e = horz.prev_in_ael;

			while (e) {

				if (e == max_pair) 
				{
					if (IsHotEdge(horz))
					{
						if (is_left_to_right)
							op = AddLocalMaxPoly(horz, *e, horz.top);
						else
							op = AddLocalMaxPoly(*e, horz, horz.top);

						if (op && !IsOpen(horz) && EqualsXY(op->pt,horz.top))
							AddTrialHorzJoin(op);
					}
					DeleteFromAEL(*e);
					DeleteFromAEL(horz);
					return;
				}

				//if horzEdge is a maxima, keep going until we reach
				//its maxima pair, otherwise check for break conditions
				if (!isMax || IsOpenEnd(horz))
				{
					//otherwise stop when 'ae' is beyond the end of the horizontal line
					if ((is_left_to_right && e->curr_x > horz_right) ||
						(!is_left_to_right && e->curr_x < horz_left)) break;

					if (e->curr_x == GetX(horz.top) && !IsHorizontal(*e))
					{
						//for edges at horzEdge's end, only stop when horzEdge's
						//outslope is greater than e's slope when heading right or when
						pt = NextVertex<PointType, ClipperFlags>(horz)->pt;
						if ((is_left_to_right && TopX(*e, GetY(pt)) >= GetX(pt)) ||
							(!is_left_to_right && TopX(*e, GetY(pt)) <= GetX(pt))) break;
					}
				}

				pt = point_traits<PointType>::construct(e->curr_x, GetY(horz.bot));

				if (is_left_to_right) 
				{
					op = IntersectEdges(horz, *e, pt);
					SwapPositionsInAEL(horz, *e);


					if (IsHotEdge(horz) && op && !IsOpen(horz) && EqualsXY(op->pt, pt))
						AddTrialHorzJoin(op);

					if (!IsHorizontal(*e) && TestJoinWithPrev1(*e, y))
					{
						op = AddOutPt(*e->prev_in_ael, pt);
						OutPt* op2 = AddOutPt(*e, pt);
						AddJoin(op, op2);
					}

					horz.curr_x = e->curr_x;
					e = horz.next_in_ael;
				} 
				else 
				{
					op = IntersectEdges(*e, horz, pt);
					SwapPositionsInAEL(*e, horz);

					if (IsHotEdge(horz) && op &&
						!IsOpen(horz) && EqualsXY(op->pt, pt))
							AddTrialHorzJoin(op);

					if (!IsHorizontal(*e) && TestJoinWithNext1(*e, y))
					{
						op = AddOutPt(*e, pt);
						OutPt* op2 = AddOutPt(*e->next_in_ael, pt);
						AddJoin(op, op2);
					}

					horz.curr_x = e->curr_x;
					e = horz.prev_in_ael;
				}
			}

			//check if we've finished with (consecutive) horizontals ...
			if (isMax || GetY(NextVertex<PointType, ClipperFlags>(horz)->pt) != GetY(horz.top)) break;

			//still more horizontals in bound to process ...
			if (IsHotEdge(horz))
				AddOutPt(horz, horz.top);
			UpdateEdgeIntoAEL(&horz);
			isMax = IsMaxima(horz);

			if (!horzIsOpen && !isMax && TrimHorz<PointType, ClipperFlags>(horz, PreserveCollinear))
				isMax = IsMaxima(horz); //i.e. update after TrimHorz

			is_left_to_right = ResetHorzDiRect64on(horz, max_pair, horz_left, horz_right);

			if (isMax) max_pair = GetMaximaPair(horz);
		}

		if (IsHotEdge(horz))
		{
			op = AddOutPt(horz, horz.top);
			if (!IsOpen(horz))
				AddTrialHorzJoin(op);
		}
		else 
			op = NULL;

		if (!isMax)
		{
			UpdateEdgeIntoAEL(&horz); //this is the end of an intermediate horiz.
			if (IsOpen(horz)) return;

			if (is_left_to_right && TestJoinWithNext1(horz, y))
			{
				OutPt* op2 = AddOutPt(*horz.next_in_ael, horz.bot);
				AddJoin(op, op2);
			}
			else if (!is_left_to_right && TestJoinWithPrev1(horz, y))
			{
				OutPt* op2 = AddOutPt(*horz.prev_in_ael, horz.bot);
				AddJoin(op2, op);
			}
		}
		else if (!max_pair) DeleteFromAEL(horz); //i.e. open at top
		else if (IsHotEdge(horz)) AddLocalMaxPoly(horz, *max_pair, horz.top);
		else
		{
			DeleteFromAEL(*max_pair);
			DeleteFromAEL(horz);
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DoTopOfScanbeam(const int64_t y)
	{
		using namespace engine::detail;

		sel_ = NULL;  // sel_ is reused to flag horizontals (see PushHorz below)
		Active*e = actives_;
		while (e) 
		{
			//nb: 'e' will never be horizontal here
			if (GetY(e->top) == y)
			{
				e->curr_x = GetX(e->top);
				if (IsMaxima(*e))
				{
					e = DoMaxima(*e);  //TOP OF BOUND (MAXIMA)
					continue;
				} else 
				{
					//INTERMEDIATE VERTEX ...
					if (IsHotEdge(*e)) AddOutPt(*e, e->top);
					UpdateEdgeIntoAEL(e);
					if (IsHorizontal(*e))
						PushHorz(*e);  //horizontals are processed later
				}
			}
			else //i.e. not the top of the edge
				e->curr_x = TopX(*e, y);

			e = e->next_in_ael;
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::Active<PointType>* ClipperBase<PointType, ZFillFunc, ClipperFlags>::DoMaxima(Active&e) 
	{
		using namespace engine::detail;

		Active*next_e, *prev_e, *max_pair;
		prev_e = e.prev_in_ael;
		next_e = e.next_in_ael;
		if (IsOpenEnd(e))
		{
			if (IsHotEdge(e)) AddOutPt(e, e.top);
			if (!IsHorizontal(e))
			{
				if (IsHotEdge(e)) e.outrec = NULL;
				DeleteFromAEL(e);
			}
			return next_e;
		} 
		else 
		{
			max_pair = GetMaximaPair(e);
			if (!max_pair) return next_e;  //eMaxPair is horizontal
		}

		//only non-horizontal maxima here.
		//process any edges between maxima pair ...
		while (next_e != max_pair) {
			IntersectEdges(e, *next_e, e.top);
			SwapPositionsInAEL(e, *next_e);
			next_e = e.next_in_ael;
		}

		if (IsOpen(e))
		{
			if (IsHotEdge(e))
				AddLocalMaxPoly(e, *max_pair, e.top);
			DeleteFromAEL(*max_pair);
			DeleteFromAEL(e);
			return (prev_e ? prev_e->next_in_ael : actives_);
		}

		//here E.next_in_ael == ENext == EMaxPair ...
		if (IsHotEdge(e))
			AddLocalMaxPoly(e, *max_pair, e.top);

		DeleteFromAEL(e);
		DeleteFromAEL(*max_pair);
		return (prev_e ? prev_e->next_in_ael : actives_);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::SafeDeleteOutPtJoiners(OutPt* op)
	{
		Joiner* joiner = op->joiner;
		if (!joiner) return;

		while (joiner)
		{
			if (joiner->idx < 0)
				DeleteTrialHorzJoin(op);
			else if (horz_joiners_)
			{
				if (OutPtInTrialHorzList(joiner->op1))
					DeleteTrialHorzJoin(joiner->op1);
				if (OutPtInTrialHorzList(joiner->op2))
					DeleteTrialHorzJoin(joiner->op2);
				DeleteJoin(joiner);
			}
			else
				DeleteJoin(joiner);
			joiner = op->joiner;
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::Joiner<PointType>* ClipperBase<PointType, ZFillFunc, ClipperFlags>::GetHorzTrialParent(const OutPt* op)
	{
		Joiner* joiner = op->joiner;
		while (joiner)
		{
			if (joiner->op1 == op)
			{
				if (joiner->next1 && joiner->next1->idx < 0) return joiner;
				else joiner = joiner->next1;
			}
			else
			{
				if (joiner->next2 && joiner->next2->idx < 0) return joiner;
				else joiner = joiner->next1;
			}
		}
		return joiner;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline bool ClipperBase<PointType, ZFillFunc, ClipperFlags>::OutPtInTrialHorzList(OutPt* op)
	{
		return op->joiner && ((op->joiner->idx < 0) || GetHorzTrialParent(op));
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddTrialHorzJoin(OutPt* op)
	{
		//make sure 'op' isn't added more than once
		if (!OutPtInTrialHorzList(op))
			horz_joiners_ = new Joiner(op, NULL, horz_joiners_);
		}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DeleteTrialHorzJoin(OutPt* op)
	{
		if (!horz_joiners_) return;

		Joiner* joiner = op->joiner;
		Joiner* parentH, *parentOp = NULL;
		while (joiner)
		{
			if (joiner->idx < 0)
			{
				//first remove joiner from FHorzTrials
				if (joiner == horz_joiners_)
					horz_joiners_ = joiner->nextH;
				else
				{
					parentH = horz_joiners_;
					while (parentH->nextH != joiner)
						parentH = parentH->nextH;
					parentH->nextH = joiner->nextH;
				}

				//now remove joiner from op's joiner list
				if (!parentOp)
				{
					//joiner must be first one in list
					op->joiner = joiner->next1;
					delete joiner;
					joiner = op->joiner;
				}
				else
				{
					//the trial joiner isn't first
					if (op == parentOp->op1)
						parentOp->next1 = joiner->next1;
					else
						parentOp->next2 = joiner->next1;
					delete joiner;
					joiner = parentOp;
				}
			}
			else
			{
				//not a trial join so look further along the linked list
				parentOp = engine::detail::FindTrialJoinParent(joiner, op);
				if (!parentOp) break;
			}
			//loop in case there's more than one trial join
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::ConvertHorzTrialsToJoins()
	{
		using namespace engine::detail;

		while (horz_joiners_)
		{
			Joiner* joiner = horz_joiners_;
			horz_joiners_ = horz_joiners_->nextH;
			OutPt* op1a = joiner->op1;
			if (op1a->joiner == joiner)
			{
				op1a->joiner = joiner->next1;
			}
			else
			{
				Joiner* joinerParent = FindJoinParent(joiner, op1a);
				if (joinerParent->op1 == op1a)
					joinerParent->next1 = joiner->next1;
				else
					joinerParent->next2 = joiner->next1;
			}
			delete joiner;

			OutPt* op1b;
			if (!GetHorzExtendedHorzSeg(op1a, op1b))
			{
				CleanCollinear(op1a->outrec);
				continue;
			}

			bool joined = false;
			joiner = horz_joiners_;
			while (joiner)
			{
				OutPt* op2a = joiner->op1, * op2b;
				if (GetHorzExtendedHorzSeg(op2a, op2b) &&
					HorzEdgesOverlap(
						GetX(op1a->pt), GetX(op1b->pt), 
						GetX(op2a->pt), GetX(op2b->pt)))
				{
					//overlap found so promote to a 'real' join
					joined = true;
					if (EqualsXY(op1a->pt, op2b->pt))
						AddJoin(op1a, op2b);
					else if (EqualsXY(op1b->pt, op2a->pt))
						AddJoin(op1b, op2a);
					else if (EqualsXY(op1a->pt, op2a->pt))
						AddJoin(op1a, op2a);
					else if (EqualsXY(op1b->pt, op2b->pt))
						AddJoin(op1b, op2b);
					else if (ValueBetween(GetX(op1a->pt), GetX(op2a->pt), GetX(op2b->pt)))
						AddJoin(op1a, InsertOp(op1a->pt, op2a));
					else if (ValueBetween(GetX(op1b->pt), GetX(op2a->pt), GetX(op2b->pt)))
						AddJoin(op1b, InsertOp(op1b->pt, op2a));
					else if (ValueBetween(GetX(op2a->pt), GetX(op1a->pt), GetX(op1b->pt)))
						AddJoin(op2a, InsertOp(op2a->pt, op1a));
					else if (ValueBetween(GetX(op2b->pt), GetX(op1a->pt), GetX(op1b->pt)))
						AddJoin(op2b, InsertOp(op2b->pt, op1a));
					break;
				}
				joiner = joiner->nextH;
			}
			if (!joined) 
				CleanCollinear(op1a->outrec);
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::AddJoin(OutPt* op1, OutPt* op2)
	{
		if ((op1->outrec == op2->outrec) && ((op1 == op2) ||
			//unless op1.next or op1.prev crosses the start-end divide
			//don't waste time trying to join adjacent vertices
			((op1->next == op2) && (op1 != op1->outrec->pts)) ||
			((op2->next == op1) && (op2 != op1->outrec->pts)))) return;

		auto *j = new Joiner(op1, op2, NULL);
		j->idx = static_cast<int>(joiner_list_.size());
		joiner_list_.push_back(j);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::DeleteJoin(Joiner* joiner)
	{
		using namespace engine::detail;

		//This method deletes a single join, and it doesn't check for or
		//delete trial horz. joins. For that, use the following method.
		OutPt* op1 = joiner->op1, *op2 = joiner->op2;

		Joiner* parent_joiner;
		if (op1->joiner != joiner)
		{
			parent_joiner = FindJoinParent(joiner, op1);
			if (parent_joiner->op1 == op1)
				parent_joiner->next1 = joiner->next1;
			else
				parent_joiner->next2 = joiner->next1;
		}
		else
			op1->joiner = joiner->next1;

		if (op2->joiner != joiner)
		{
			parent_joiner = FindJoinParent(joiner, op2);
			if (parent_joiner->op1 == op2)
				parent_joiner->next1 = joiner->next2;
			else
				parent_joiner->next2 = joiner->next2;
		}
		else
			op2->joiner = joiner->next2;

		joiner_list_[joiner->idx] = NULL;
		delete joiner;
	}

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::ProcessJoinerList()
	{
		for (Joiner* j : joiner_list_)
		{
			if (!j)
			{
				continue;
			}
			else if (error_found_)
			{
				delete j;
			} else
			{
				OutRec* outrec = ProcessJoin(j);
				CleanCollinear(outrec);
			}
		}
		joiner_list_.resize(0);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline engine::detail::OutRec<PointType>* ClipperBase<PointType, ZFillFunc, ClipperFlags>::ProcessJoin(Joiner* joiner)
	{
		using namespace engine::detail;

		OutPt*op1 = joiner->op1,  *op2 = joiner->op2;
		OutRec* or1 = GetRealOutRec(op1->outrec);
		OutRec* or2 = GetRealOutRec(op2->outrec);
		DeleteJoin(joiner);

		if (or2->pts == NULL) return or1;
		else if (!IsValidClosedPath(op2))
		{
			CleanCollinear(or2);
			return or1;
		}
		else if ((or1->pts == NULL) || !IsValidClosedPath(op1))
		{
			CleanCollinear(or1);
			return or2;
		}
		else if (or1 == or2 &&
			((op1 == op2) || (op1->next == op2) || (op1->prev == op2))) return or1;

		CheckDisposeAdjacent(op1, op2, *or1);
		CheckDisposeAdjacent(op2, op1, *or2);
		if (op1->next == op2 || op2->next == op1) return or1;

		OutRec* result = or1;
		for (; ; )
		{
			if (!IsValidPath(op1) || !IsValidPath(op2) ||
				(or1 == or2 && (op1->prev == op2 || op1->next == op2))) return or1;

			if (EqualsXY(op1->prev->pt, op2->next->pt) ||
				((CrossProduct(op1->prev->pt, op1->pt, op2->next->pt) == 0) &&
					CollinearSegsOverlap(op1->prev->pt, op1->pt, op2->pt, op2->next->pt)))
			{
				if (or1 == or2)
				{
					//SPLIT REQUIRED
					//make sure op1.prev and op2.next match positions
					//by inserting an extra vertex if needed
					if (! EqualsXY(op1->prev->pt, op2->next->pt))
					{
						if (PointBetween(op1->prev->pt, op2->pt, op2->next->pt))
							op2->next = InsertOp(op1->prev->pt, op2);
						else
							op1->prev = InsertOp(op2->next->pt, op1->prev);
					}

					//current              to     new
					//op1.p[opA] >>> op1   ...    opA \   / op1
					//op2.n[opB] <<< op2   ...    opB /   \ op2
					OutPt *opA = op1->prev, *opB = op2->next;
					opA->next = opB;
					opB->prev = opA;
					op1->prev = op2;
					op2->next = op1;
					CompleteSplit(op1, opA, *or1);
				}
				else
				{
					//JOIN, NOT SPLIT
					OutPt *opA = op1->prev, *opB = op2->next;
					opA->next = opB;
					opB->prev = opA;
					op1->prev = op2;
					op2->next = op1;
					//this isn't essential but it's
					//easier to track ownership when it
					//always defers to the lower index
					if (or1->idx < or2->idx)
					{
						or1->pts = op1;
						or2->pts = NULL;
						or2->owner = or1;
					}
					else
					{
						result = or2;
						or2->pts = op1;
						or1->pts = NULL;
						or1->owner = or2;
					}
				}
				break;
			}
			else if (EqualsXY(op1->next->pt, op2->prev->pt) ||
				((CrossProduct(op1->next->pt, op2->pt, op2->prev->pt) == 0) &&
					CollinearSegsOverlap(op1->next->pt, op1->pt, op2->pt, op2->prev->pt)))
			{
				if (or1 == or2)
				{
					//SPLIT REQUIRED
					//make sure op2.prev and op1.next match positions
					//by inserting an extra vertex if needed
					if (! EqualsXY(op2->prev->pt, op1->next->pt))
					{
						if (PointBetween(op2->prev->pt, op1->pt, op1->next->pt))
							op1->next = InsertOp(op2->prev->pt, op1);
						else
							op2->prev = InsertOp(op1->next->pt, op2->prev);
					}

					//current              to     new
					//op2.p[opA] >>> op2   ...    opA \   / op2
					//op1.n[opB] <<< op1   ...    opB /   \ op1
					OutPt *opA = op2->prev, *opB = op1->next;
					opA->next = opB;
					opB->prev = opA;
					op2->prev = op1;
					op1->next = op2;
					CompleteSplit(op1, opA, *or1);
				}
				else
				{
					//JOIN, NOT SPLIT
					OutPt *opA = op1->next, *opB = op2->prev;
					opA->prev = opB;
					opB->next = opA;
					op1->next = op2;
					op2->prev = op1;
					if (or1->idx < or2->idx)
					{
						or1->pts = op1;
						or2->pts = NULL;
						or2->owner = or1;
					}
					else
					{
						result = or2;
						or2->pts = op1;
						or1->pts = NULL;
						or1->owner = or2;
					}
				}
				break;
			}
			else if (PointBetween(op1->next->pt, op2->pt, op2->prev->pt) &&
				DistanceFromLineSqrd(op1->next->pt, op2->pt, op2->prev->pt) < 2.01)
			{
				InsertOp(op1->next->pt, op2->prev);
				continue;
			}
			else if (PointBetween(op2->next->pt, op1->pt, op1->prev->pt) &&
				DistanceFromLineSqrd(op2->next->pt, op1->pt, op1->prev->pt) < 2.01)
			{
				InsertOp(op2->next->pt, op1->prev);
				continue;
			}
			else if (PointBetween(op1->prev->pt, op2->pt, op2->next->pt) &&
				DistanceFromLineSqrd(op1->prev->pt, op2->pt, op2->next->pt) < 2.01)
			{
				InsertOp(op1->prev->pt, op2);
				continue;
			}
			else if (PointBetween(op2->prev->pt, op1->pt, op1->next->pt) &&
				DistanceFromLineSqrd(op2->prev->pt, op1->pt, op1->next->pt) < 2.01)
			{
				InsertOp(op2->prev->pt, op1);
				continue;
			}

			//something odd needs tidying up
			if (CheckDisposeAdjacent(op1, op2, *or1)) continue;
			else if (CheckDisposeAdjacent(op2, op1, *or1)) continue;
			else if (! EqualsXY(op1->prev->pt, op2->next->pt) &&
				(DistanceSqr(op1->prev->pt, op2->next->pt) < 2.01))
			{
				op1->prev->pt = op2->next->pt;
				continue;
			}
			else if (! EqualsXY(op1->next->pt, op2->prev->pt) &&
				(DistanceSqr(op1->next->pt, op2->prev->pt) < 2.01))
			{
				op2->prev->pt = op1->next->pt;
				continue;
			}
			else
			{
				//OK, there doesn't seem to be a way to join after all
				//so just tidy up the polygons
				or1->pts = op1;
				if (or2 != or1)
				{
					or2->pts = op2;
					CleanCollinear(or2);
				}
				break;
			}
		}
		return result;

	}

	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::BuildPaths(Paths& solutionClosed, Paths* solutionOpen)
	{
		using namespace engine::detail;

		solutionClosed.resize(0);
		//solutionClosed.reserve(outrec_list_.size());
		if (solutionOpen)
		{
			solutionOpen->resize(0);
			solutionOpen->reserve(outrec_list_.size());
		}
		for(OutRec* outrec : outrec_list_)
		{
			if (outrec->pts == NULL) continue;
			Path path;
			if (solutionOpen && outrec->state == OutRecState::Open)
			{
				if (BuildPath<PointType, ClipperFlags>(*outrec->pts, true, path))
					solutionOpen->emplace_back(std::move(path));
				path.resize(0);
			}
			else
			{
				if (BuildPath<PointType, ClipperFlags>(*outrec->pts, false, path))
					solutionClosed.emplace_back(std::move(path));
				path.resize(0);
			}
				 
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ZFillFunc, typename ClipperFlags>
	inline void ClipperBase<PointType, ZFillFunc, ClipperFlags>::BuildTree(PolyPath<PointType>& polytree, Paths& open_paths)
	{
		using namespace engine::detail;

		polytree.Clear();
		open_paths.resize(0);
		if (has_open_paths_) 
			open_paths.reserve(outrec_list_.size());
		for (OutRec* outrec : outrec_list_)
		{
			if (!outrec) continue;

			//make sure outer/owner paths are processed before their inner paths
			//and swap OutRec order if necessary
			while (outrec->owner && outrec->owner->idx > outrec->idx)
			{
				OutRec* tmp = outrec->owner;
				outrec_list_[outrec->owner->idx] = outrec;
				outrec_list_[outrec->idx] = tmp;
				size_t tmp_idx = outrec->idx;
				outrec->idx = tmp->idx;
				tmp->idx = tmp_idx;
				outrec = tmp;
			}

			if (!outrec->pts) continue;

			Path path;
			bool is_open_path = IsOpen(*outrec);
			if (!BuildPath<PointType, ClipperFlags>(*outrec->pts, is_open_path, path)) continue;

			if (is_open_path)
			{
				open_paths.push_back(path);
				continue;
			}

			//update ownership ...
			while (outrec->owner && !outrec->owner->pts)
				outrec->owner = outrec->owner->owner;
			if (outrec->owner && outrec->owner->state == outrec->state)
			{
				if (IsOuter(*outrec)) outrec->owner = NULL;
				else outrec->owner = outrec->owner->owner;
			}

			PolyPath<PointType>* owner_polypath;
			if (outrec->owner && outrec->owner->polypath)
				owner_polypath = outrec->owner->polypath; 
			else
				owner_polypath = &polytree;

			outrec->polypath = owner_polypath->AddChild(path);
		}
	}

	inline void Polypath64ToPolypathD(const PolyPath64& polypath, PolyPathD& result)
	{
		for (size_t i = 0; i < polypath.ChildCount(); ++i)
		{
			const PolyPath64 *child = polypath[i];
			PolyPathD* res_child = result.AddChild(Path64ToPathD(child->Path()));
			Polypath64ToPolypathD(*child, *res_child);
		}
	}

	inline void Polytree64ToPolytreeD(const PolyPath64& polytree, PolyPathD& result)
	{
		result.Clear();
		Polypath64ToPolypathD(polytree, result);
	}

}  // namespace 

#endif  //clipper_engine_h
