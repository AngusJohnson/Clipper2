/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  4 May 2022                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This is the main polygon clipping module                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef clipper_engine_detail_h
#define clipper_engine_detail_h

#include <stdlib.h>
#include <cmath>
#include <stdexcept>
#include <vector>
#include <numeric>

namespace Clipper2Lib::engine::detail {

	static constexpr const double FloatingPointTolerance = 1.0e-12;

	using Clipper2Lib::detail::GetX;
	using Clipper2Lib::detail::GetY;
	using Clipper2Lib::detail::EqualsXY;
	using Clipper2Lib::detail::Construct;

	//Every closed path (or polygon) is made up of a series of vertices forming
	//edges that alternate between going up (relative to the Y-axis) and going
	//down. Edges consecutively going up or consecutively going down are called
	//'bounds' (or sides if they're simple polygons). 'Local Minima' refer to
	//vertices where descending bounds become ascending ones.

	template<typename PointType>
	struct IntersectNode {
		PointType pt;
		Active<PointType>* edge1;
		Active<PointType>* edge2;

		IntersectNode(Active<PointType>* e1, Active<PointType>* e2, PointType& pt_) :
			edge1(e1), edge2(e2), pt(pt_) {}
	};

	template<typename PointType>
	struct  Joiner {
		int			idx;
		OutPt<PointType>* op1;
		OutPt<PointType>* op2;
		Joiner* next1;
		Joiner* next2;
		Joiner* nextH;

		explicit Joiner(OutPt<PointType>* op1_, OutPt<PointType>* op2_, Joiner* nexth) :
			op1(op1_), op2(op2_), nextH(nexth)
		{
			idx = -1;
			next1 = op1->joiner;
			op1->joiner = this;

			if (op2)
			{
				next2 = op2->joiner;
				op2->joiner = this;
			}
			else
				next2 = nullptr;
		}
	};

	template<typename PointType>
	struct LocMinSorter {
		inline bool operator()(const LocalMinima<PointType>* locMin1, const LocalMinima<PointType>* locMin2)
		{
			if (GetY(locMin2->vertex->pt) != GetY(locMin1->vertex->pt))
				return GetY(locMin2->vertex->pt) < GetY(locMin1->vertex->pt);
			else
				return GetX(locMin2->vertex->pt) < GetX(locMin1->vertex->pt);
		}
	};

	inline bool IsOdd(int val)
	{
		return (val & 1) ? true : false;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsHotEdge(const Active<PointType>& e)
	{
		return (e.outrec);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsOpen(const Active<PointType>& e)
	{
		return (e.local_min->is_open);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsOpen(const OutRec<PointType>& outrec)
	{
		return (outrec.state == OutRecState::Open);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsOpenEnd(const Active<PointType>& ae)
	{
		return ae.local_min->is_open &&
			(ae.vertex_top->flags & (VertexFlags::OpenStart | VertexFlags::OpenEnd)) != VertexFlags::None;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline Active<PointType>* GetPrevHotEdge(const Active<PointType>& e)
	{
		Active<PointType>* prev = e.prev_in_ael;
		while (prev && (IsOpen(*prev) || !IsHotEdge(*prev)))
			prev = prev->prev_in_ael;
		return prev;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsOuter(const OutRec<PointType>& outrec)
	{
		return (outrec.state == OutRecState::Outer);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void SetAsOuter(OutRec<PointType>& outrec)
	{
		outrec.state = OutRecState::Outer;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsInner(const OutRec<PointType>& outrec)
	{
		return (outrec.state == OutRecState::Inner);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void SetAsInner(OutRec<PointType>& outrec)
	{
		outrec.state = OutRecState::Inner;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsFront(const Active<PointType>& e)
	{
		//the front edge will be the LEFT edge when it's an OUTER polygon
		//so that outer polygons will be orientated clockwise
		if (e.outrec->state == OutRecState::Open)
			return (e.wind_dx > 0);
		else
			return (&e == e.outrec->front_edge);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsInvalidPath(OutPt<PointType>* op)
	{
		return (!op || op->next == op);
	}
	//------------------------------------------------------------------------------

	/*******************************************************************************
		*  Dx:                             0(90deg)                                    *
		*                                  |                                           *
		*               +inf (180deg) <--- o ---> -inf (0deg)                          *
		*******************************************************************************/

	template<typename PointType>
	inline double GetDx(const PointType& pt1, const PointType& pt2)
	{
		double dy = double(GetY(pt2) - GetY(pt1));
		if (dy != 0)
			return double(GetX(pt2) - GetX(pt1)) / dy;
		else if (GetX(pt2) > GetX(pt1))
			return -DBL_MAX;
		else
			return DBL_MAX;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline int64_t TopX(const Active<PointType>& ae, const int64_t currentY)
	{
		if ((currentY == GetY(ae.top)) || (GetX(ae.top) == GetX(ae.bot))) 
			return GetX(ae.top);
		else if (currentY == GetY(ae.bot)) return GetX(ae.bot);
		else return GetX(ae.bot) + int64_t(std::round(ae.dx * (currentY - GetY(ae.bot))));
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsHorizontal(const Active<PointType>& e)
	{
		return (GetY(e.top) == GetY(e.bot));
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsHeadingRightHorz(const Active<PointType>& e)
	{
		return (e.dx == -DBL_MAX);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsHeadingLeftHorz(const Active<PointType>& e)
	{
		return (e.dx == DBL_MAX);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void SwapActives(Active<PointType>*& e1, Active<PointType>*& e2)
	{
		Active<PointType>* e = e1;
		e1 = e2;
		e2 = e;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline PathType GetPolyType(const Active<PointType>& e)
	{
		return e.local_min->polytype;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsSamePolyType(const Active<PointType>& e1, const Active<PointType>& e2)
	{
		return e1.local_min->polytype == e2.local_min->polytype;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline PointType GetIntersectPoint(const Active<PointType>& e1, const Active<PointType>& e2)
	{
		double b1, b2;
		if (e1.dx == e2.dx) return e1.top;

		if (e1.dx == 0)
		{
			if (IsHorizontal(e2)) return Construct<PointType>(GetX(e1.bot), GetY(e2.bot));
			b2 = GetY(e2.bot) - (GetX(e2.bot) / e2.dx);
			return Construct<PointType>(GetX(e1.bot), (int64_t)std::round(GetX(e1.bot) / e2.dx + b2));
		}
		else if (e2.dx == 0)
		{
			if (IsHorizontal(e1)) return Construct<PointType>(GetX(e2.bot), GetY(e1.bot));
			b1 = GetY(e1.bot) - (GetX(e1.bot) / e1.dx);
			return Construct<PointType>(GetX(e2.bot), (int64_t)std::round(GetX(e2.bot) / e1.dx + b1));
		}
		else
		{
			b1 = GetX(e1.bot) - GetY(e1.bot) * e1.dx;
			b2 = GetX(e2.bot) - GetY(e2.bot) * e2.dx;
			double q = (b2 - b1) / (e1.dx - e2.dx);
			return (abs(e1.dx) < abs(e2.dx)) ?
				Construct<PointType>(int64_t(std::round(e1.dx * q + b1)), (int64_t)std::round(q)) :
				Construct<PointType>(int64_t(std::round(e2.dx * q + b2)), (int64_t)std::round(q));
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool GetIntersectPoint(const PointType& ln1a, const PointType& ln1b,
		const PointType& ln2a, const PointType& ln2b, PointD& ip)
	{
		ip = Construct<PointD>(0, 0);
		double m1, b1, m2, b2;
		if (GetX(ln1b) == GetX(ln1a))
		{
			if (GetX(ln2b) == GetX(ln2a)) return false;
			m2 = static_cast<double>(GetY(ln2b) - GetY(ln2a)) /
				static_cast<double>(GetX(ln2b) - GetX(ln2a));
			b2 = GetY(ln2a) - m2 * GetX(ln2a);
			ip.x = static_cast<double>(GetX(ln1a));
			ip.y = m2 * GetX(ln1a) + b2;
		}
		else if (GetX(ln2b) == GetX(ln2a))
		{
			m1 = static_cast<double>(GetY(ln1b) - GetY(ln1a)) /
				static_cast<double>(GetX(ln1b) - GetX(ln1a));
			b1 = GetY(ln1a) - m1 * GetX(ln1a);
			ip.x = static_cast<double>(GetX(ln2a));
			ip.y = m1 * GetX(ln2a) + b1;
		}
		else
		{
			m1 = static_cast<double>(GetY(ln1b) - GetY(ln1a)) /
				static_cast<double>(GetX(ln1b) - GetX(ln1a));
			b1 = GetY(ln1a) - m1 * GetX(ln1a);
			m2 = static_cast<double>(GetY(ln2b) - GetY(ln2a)) /
				static_cast<double>(GetX(ln2b) - GetX(ln2a));
			b2 = GetY(ln2a) - m2 * GetX(ln2a);
			if (std::abs(m1 - m2) > FloatingPointTolerance)
			{
				ip.x = (b2 - b1) / (m1 - m2);
				ip.y = m1 * GetX(ip) + b1;
			}
			else
			{
				ip.x = double((GetX(ln1a) + GetY(ln1b)) / 2);
				ip.y = double((GetY(ln1a) + GetY(ln1b)) / 2);
			}
		}
		return true;
	}
	//---------------------------------------------------------------------------

	template<typename PointType>
	inline void SetDx(Active<PointType>& e)
	{
		e.dx = GetDx(e.bot, e.top);
	}
	//---------------------------------------------------------------------------

	template<typename PointType, typename ClipperFlags>
	inline Vertex<PointType>* NextVertex(const Active<PointType>& e)
	{
		bool take_next;
		if constexpr (ClipperFlags::reverse_orientation)
			take_next = e.wind_dx > 0;
		else
			take_next = e.wind_dx < 0;
		if (take_next)
			return e.vertex_top->next;
		else
			return e.vertex_top->prev;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ClipperFlags>
	inline Vertex<PointType>* PrevPrevVertex(const Active<PointType>& ae)
	{
		bool take_prev;
		if constexpr (ClipperFlags::reverse_orientation)
			take_prev = ae.wind_dx > 0;
		else
			take_prev = ae.wind_dx < 0;
		if (take_prev)
			return ae.vertex_top->prev->prev;
		else 
			return ae.vertex_top->next->next;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline Active<PointType>* ExtractFromSEL(Active<PointType>* ae)
	{
		Active<PointType>* res = ae->next_in_sel;
		if (res)
			res->prev_in_sel = ae->prev_in_sel;
		ae->prev_in_sel->next_in_sel = res;
		return res;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void Insert1Before2InSEL(Active<PointType>* ae1, Active<PointType>* ae2)
	{
		ae1->prev_in_sel = ae2->prev_in_sel;
		if (ae1->prev_in_sel)
			ae1->prev_in_sel->next_in_sel = ae1;
		ae1->next_in_sel = ae2;
		ae2->prev_in_sel = ae1;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsMaxima(const Active<PointType>& e)
	{
		return ((e.vertex_top->flags & VertexFlags::LocalMax) != VertexFlags::None);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline Active<PointType>* GetMaximaPair(const Active<PointType>& e)
	{
		Active<PointType>* e2;
		if (IsHorizontal(e))
		{
			//we can't be sure whether the MaximaPair is on the left or right, so ...
			e2 = e.prev_in_ael;
			while (e2 && e2->curr_x >= GetX(e.top))
			{
				if (e2->vertex_top == e.vertex_top) return e2;  //Found!
				e2 = e2->prev_in_ael;
			}
			e2 = e.next_in_ael;
			while (e2 && (TopX(*e2, GetY(e.top)) <= GetX(e.top)))
			{
				if (e2->vertex_top == e.vertex_top) return e2;  //Found!
				e2 = e2->next_in_ael;
			}
			return nullptr;
		}
		else
		{
			e2 = e.next_in_ael;
			while (e2)
			{
				if (e2->vertex_top == e.vertex_top) return e2;  //Found!
				e2 = e2->next_in_ael;
			}
			return nullptr;
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline int PointCount(OutPt<PointType>* op)
	{
		OutPt<PointType>* op2 = op;
		int cnt = 0;
		do
		{
			op2 = op2->next;
			++cnt;
		} while (op2 != op);
		return cnt;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline OutPt<PointType>* InsertOp(const PointType& pt, OutPt<PointType>* insertAfter)
	{
		OutPt<PointType>* result = new OutPt<PointType>(pt, insertAfter->outrec);
		result->next = insertAfter->next;
		insertAfter->next->prev = result;
		insertAfter->next = result;
		result->prev = insertAfter;
		return result;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline OutPt<PointType>* DisposeOutPt(OutPt<PointType>* op)
	{
		OutPt<PointType>* result = op->next;
		op->prev->next = op->next;
		op->next->prev = op->prev;
		delete op;
		return result;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void DisposeOutPts(OutRec<PointType>& outrec)
	{
		if (!outrec.pts) return;
		OutPt<PointType>* op2 = outrec.pts->next;
		while (op2 != outrec.pts)
		{
			OutPt<PointType>* tmp = op2->next;
			delete op2;
			op2 = tmp;
		}
		delete outrec.pts;
		outrec.pts = nullptr;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IntersectListSort(IntersectNode<PointType>* a, IntersectNode<PointType>* b)
	{
		//note different inequality tests ...
		return GetY(a->pt) == GetY(b->pt) ? 
			GetX(a->pt) < GetX(b->pt) : 
			GetY(a->pt) > GetY(b->pt);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void SetSides(OutRec<PointType>& outrec, Active<PointType>& start_edge, Active<PointType>& end_edge)
	{
		outrec.front_edge = &start_edge;
		outrec.back_edge = &end_edge;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void SwapOutrecs(Active<PointType>& e1, Active<PointType>& e2)
	{
		OutRec<PointType>* or1 = e1.outrec;
		OutRec<PointType>* or2 = e2.outrec;
		if (or1 == or2)
		{
			Active<PointType>* e = or1->front_edge;
			or1->front_edge = or1->back_edge;
			or1->back_edge = e;
			return;
		}
		if (or1)
		{
			if (&e1 == or1->front_edge)
				or1->front_edge = &e2;
			else
				or1->back_edge = &e2;
		}
		if (or2)
		{
			if (&e2 == or2->front_edge)
				or2->front_edge = &e1;
			else
				or2->back_edge = &e1;
		}
		e1.outrec = or2;
		e2.outrec = or1;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline double Area(OutPt<PointType>* op)
	{
		double area = 0.0;
		OutPt<PointType>* op2 = op;
		do
		{
			area += static_cast<double>(GetY(op2->pt) - GetY(op2->prev->pt)) *
				static_cast<double>(GetX(op2->pt) + GetX(op2->prev->pt));
			op2 = op2->next;
		} while (op2 != op);
		return area * 0.5;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void ReverseOutPts(OutPt<PointType>* op)
	{
		if (!op) return;

		OutPt<PointType>* op1 = op;
		OutPt<PointType>* op2;

		do
		{
			op2 = op1->next;
			op1->next = op1->prev;
			op1->prev = op2;
			op1 = op2;
		} while (op1 != op);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void SwapSides(OutRec<PointType>& outrec)
	{
		Active<PointType>* e2 = outrec.front_edge;
		outrec.front_edge = outrec.back_edge;
		outrec.back_edge = e2;
		outrec.pts = outrec.pts->next;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline OutRec<PointType>* GetRealOutRec(OutRec<PointType>* outRec)
	{
		while (outRec && !outRec->pts) outRec = outRec->owner;
		return outRec;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void UncoupleOutRec(Active<PointType> ae)
	{
		OutRec<PointType>* outrec = ae.outrec;
		if (!outrec) return;
		outrec->front_edge->outrec = nullptr;
		outrec->back_edge->outrec = nullptr;
		outrec->front_edge = nullptr;
		outrec->back_edge = nullptr;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool IsValidClosedPath(const OutPt<PointType>* op)
	{
		return (op && op->next != op && op->next != op->prev);
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool CheckFixInnerOuter(Active<PointType>& ae)
	{
		bool wasOuter = IsOuter(*ae.outrec), isOuter = true;

		Active<PointType>* ae2 = ae.prev_in_ael;
		while (ae2)
		{
			if (IsHotEdge(*ae2) && !IsOpen(*ae2)) isOuter = !isOuter;
			ae2 = ae2->prev_in_ael;
		}

		if (isOuter == wasOuter) return false;

		if (isOuter)
			SetAsOuter(*ae.outrec);
		else
			SetAsInner(*ae.outrec);

		//now check and fix ownership
		ae2 = GetPrevHotEdge(ae);
		if (isOuter)
		{
			if (ae2 && IsInner(*ae2->outrec))
				ae.outrec->owner = ae2->outrec;
			else
				ae.outrec->owner = nullptr;
		}
		else
		{
			if (!ae2)
				SetAsOuter(*ae.outrec);
			else if (IsInner(*ae2->outrec))
				ae.outrec->owner = ae2->outrec->owner;
			else
				ae.outrec->owner = ae2->outrec;
		}

		if ((Area(ae.outrec->pts) < 0.0) == isOuter)
			ReverseOutPts(ae.outrec->pts);
		return true;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void SetOwnerAndInnerOuterState(const Active<PointType>& e)
	{
		Active<PointType>* e2;
		OutRec<PointType>* outrec = e.outrec;

		if (IsOpen(e))
		{
			outrec->owner = nullptr;
			outrec->state = OutRecState::Open;
			return;
		}
		//set owner ...
		if (IsHeadingLeftHorz(e))
		{
			e2 = e.next_in_ael;  //ie assess state from opposite diRect64on
			while (e2 && (!IsHotEdge(*e2) || IsOpen(*e2)))
				e2 = e2->next_in_ael;
			if (!e2)
				outrec->owner = nullptr;
			else if ((e2->outrec->state == OutRecState::Outer) == (e2->outrec->front_edge == e2))
				outrec->owner = e2->outrec->owner;
			else
				outrec->owner = e2->outrec;
		}
		else
		{
			e2 = GetPrevHotEdge(e);
			while (e2 && (!IsHotEdge(*e2) || IsOpen(*e2)))
				e2 = e2->prev_in_ael;
			if (!e2)
				outrec->owner = nullptr;
			else if (IsOuter(*e2->outrec) == (e2->outrec->back_edge == e2))
				outrec->owner = e2->outrec->owner;
			else
				outrec->owner = e2->outrec;
		}
		//set inner/outer ...
		if (!outrec->owner || IsInner(*outrec->owner))
			outrec->state = OutRecState::Outer;
		else
			outrec->state = OutRecState::Inner;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool GetHorzExtendedHorzSeg(OutPt<PointType>*& op, OutPt<PointType>*& op2)
	{
		OutRec<PointType>* outrec = GetRealOutRec(op->outrec);
		op2 = op;
		if (outrec->front_edge)
		{
			while (op->prev != outrec->pts &&
				GetY(op->prev->pt) == GetY(op->pt)) op = op->prev;
			while (op2 != outrec->pts &&
				GetY(op2->next->pt) == GetY(op2->pt)) op2 = op2->next;
			return op2 != op;
		}
		else
		{
			while (op->prev != op2 && GetY(op->prev->pt) == GetY(op->pt))
				op = op->prev;
			while (op2->next != op && GetY(op2->next->pt) == GetY(op2->pt))
				op2 = op2->next;
			return op2 != op && op2->next != op;
		}
	}
	//------------------------------------------------------------------------------

	inline bool HorzEdgesOverlap(int64_t x1a, int64_t x1b, int64_t x2a, int64_t x2b)
	{
		const int64_t minOverlap = 2;
		if (x1a > x1b + minOverlap)
		{
			if (x2a > x2b + minOverlap)
				return !((x1a <= x2b) || (x2a <= x1b));
			else
				return !((x1a <= x2a) || (x2b <= x1b));
		}
		else if (x1b > x1a + minOverlap)
		{
			if (x2a > x2b + minOverlap)
				return !((x1b <= x2b) || (x2a <= x1a)); 
			else
				return !((x1b <= x2a) || (x2b <= x1a));
		}
		else
			return false;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ClipperFlags>
	inline bool TrimHorz(Active<PointType>& horzEdge, bool preserveCollinear)
	{
		bool result = false;
		PointType pt = NextVertex<PointType, ClipperFlags>(horzEdge)->pt;
		//trim 180 deg. spikes in closed paths
		while ((GetY(pt) == GetY(horzEdge.top)) && (!preserveCollinear ||
			((GetX(pt) < GetX(horzEdge.top)) == (GetX(horzEdge.bot) < GetX(horzEdge.top)))))
		{
			horzEdge.vertex_top = NextVertex<PointType, ClipperFlags>(horzEdge);
			horzEdge.top = pt;
			result = true;
			if (IsMaxima(horzEdge)) break;
			pt = NextVertex<PointType, ClipperFlags>(horzEdge)->pt;
		}

		if (result) SetDx(horzEdge); // +/-infinity
		return result;
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ClipperFlags>
	inline bool IsValidAelOrder(const Active<PointType>&a1, const Active<PointType>&a2)
	{
		//a2 is always the new edge being inserted
		if (a2.curr_x != a1.curr_x)
			return a2.curr_x > a1.curr_x;

		//get the turning direction  a1.top, a2.bot, a2.top
		double d = CrossProduct<PointType>(a1.top, a2.bot, a2.top);

		if (d < 0) return true;
		else if (d > 0) return false;

		//edges must be collinear to get here

		//for starting open paths, place them according to
		//the direction they're about to turn
		if (IsOpen(a1) && !IsMaxima(a1) && (GetY(a1.bot) <= GetY(a2.bot)) &&
			!IsSamePolyType(a1, a2) && (GetY(a1.top) > GetY(a2.top)))
			return CrossProduct<PointType>(a1.bot, a1.top, NextVertex<PointType, ClipperFlags>(a1)->pt) <= 0;
		else if (IsOpen(a2) && !IsMaxima(a2) && (GetY(a2.bot) <= GetY(a1.bot)) &&
			!IsSamePolyType(a1, a2) && (GetY(a2.top) > GetY(a1.top)))
			return CrossProduct<PointType>(a2.bot, a2.top, NextVertex<PointType, ClipperFlags>(a2)->pt) >= 0;

		int64_t a2botY = GetY(a2.bot);
		bool a1IsNewEdge = !IsOpen(a1) &&
			(GetY(a1.bot) == a2botY) && (GetY(a1.local_min->vertex->pt) == a2botY);
		if (a1IsNewEdge)
		{
			if (a1.is_left_bound != a2.is_left_bound)
				return a2.is_left_bound;
			else if (CrossProduct<PointType>(PrevPrevVertex<PointType, ClipperFlags>(a1)->pt, a1.bot, a1.top) == 0)
				return true; //a1 is a spike so effectively we can ignore it 
			else
				//compare turning direction of alternate bound
				return (CrossProduct<PointType>(PrevPrevVertex<PointType, ClipperFlags>(a1)->pt,
					a2.bot, PrevPrevVertex<PointType, ClipperFlags>(a2)->pt) > 0) == a2.is_left_bound;
		}
		return a2.is_left_bound;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void InsertRightEdge(Active<PointType>&e, Active<PointType>&e2)
	{
		e2.next_in_ael = e.next_in_ael;
		if (e.next_in_ael) e.next_in_ael->prev_in_ael = &e2;
		e2.prev_in_ael = &e;
		e.next_in_ael = &e2;
	}
	//----------------------------------------------------------------------

	template<typename PointType>
	inline bool SegmentsIntersect(const PointType& seg1a, const PointType& seg1b,
		const PointType& seg2a, const PointType& seg2b)
	{
		double dx1 = static_cast<double>(GetX(seg1a) - GetX(seg1b));
		double dy1 = static_cast<double>(GetY(seg1a) - GetY(seg1b));
		double dx2 = static_cast<double>(GetX(seg2a) - GetX(seg2b));
		double dy2 = static_cast<double>(GetY(seg2a) - GetY(seg2b));
		return (((dy1 * (GetX(seg2a) - GetX(seg1a)) - dx1 * (GetY(seg2a) - GetY(seg1a))) *
			(dy1 * (GetX(seg2b) - GetX(seg1a)) - dx1 * (GetY(seg2b) - GetY(seg1a))) < 0) &&
			((dy2 * (GetX(seg1a) - GetX(seg2a)) - dx2 * (GetY(seg1a) - GetY(seg2a))) *
				(dy2 * (GetX(seg1b) - GetX(seg2a)) - dx2 * (GetY(seg1b) - GetY(seg2a))) < 0));
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool CollinearSegsOverlap(const PointType& seg1a, const PointType& seg1b,
		const PointType& seg2a, const PointType& seg2b)
	{
		//precondition: seg1 and seg2 are collinear      
		if (GetX(seg1a) == GetX(seg1b))
		{
			if (GetX(seg2a) != GetX(seg1a) || GetX(seg2a) != GetX(seg2b)) return false;
		}
		else if (GetX(seg1a) < GetX(seg1b))
		{
			if (GetX(seg2a) < GetX(seg2b))
			{
				if (GetX(seg2a) >= GetX(seg1b) || GetX(seg2b) <= GetX(seg1a)) return false;
			}
			else
			{
				if (GetX(seg2b) >= GetX(seg1b) || GetX(seg2a) <= GetX(seg1a)) return false;
			}
		}
		else
		{
			if (GetX(seg2a) < GetX(seg2b))
			{
				if (GetX(seg2a) >= GetX(seg1a) || GetX(seg2b) <= GetX(seg1b)) return false;
			}
			else
			{
				if (GetX(seg2b) >= GetX(seg1a) || GetX(seg2a) <= GetX(seg1b)) return false;
			}
		}

		if (GetY(seg1a) == GetY(seg1b))
		{
			if (GetY(seg2a) != GetY(seg1a) || GetY(seg2a) != GetY(seg2b)) return false;
		}
		else if (GetY(seg1a) < GetY(seg1b))
		{
			if (GetY(seg2a) < GetY(seg2b))
			{
				if (GetY(seg2a) >= GetY(seg1b) || GetY(seg2b) <= GetY(seg1a)) return false;
			}
			else
			{
				if (GetY(seg2b) >= GetY(seg1b) || GetY(seg2a) <= GetY(seg1a)) return false;
			}
		}
		else
		{
			if (GetY(seg2a) < GetY(seg2b))
			{
				if (GetY(seg2a) >= GetY(seg1a) || GetY(seg2b) <= GetY(seg1b)) return false;
			}
			else
			{
				if (GetY(seg2b) >= GetY(seg1a) || GetY(seg2a) <= GetY(seg1b)) return false;
			}
		}
		return true;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline double AreaTriangle(const PointType& pt1, const PointType& pt2, const PointType& pt3)
	{
		return 0.5 * (GetX(pt1) * (GetY(pt2) - GetY(pt3)) +
			GetX(pt2) * (GetY(pt3) - GetY(pt1)) + GetX(pt3) * (GetY(pt1) - GetY(pt2)));
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline void UpdateOutrecOwner(OutRec<PointType>* outrec)
	{
		OutPt<PointType>* opCurr = outrec->pts;
		for (; ; )
		{
			opCurr->outrec = outrec;
			opCurr = opCurr->next;
			if (opCurr == outrec->pts) return;
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline Joiner<PointType>* FindTrialJoinParent(Joiner<PointType>*&joiner, const OutPt<PointType>* op)
	{
		Joiner<PointType>* parent = joiner;
		while (parent)
		{
			if (op == parent->op1)
			{
				if (parent->next1 && parent->next1->idx < 0)
				{
					joiner = parent->next1;
					return parent;
				}
				parent = parent->next1;
			}
			else
			{
				if (parent->next2 && parent->next2->idx < 0)
				{
					joiner = parent->next2;
					return parent;
				}
				parent = parent->next2;
			}
		}
		return nullptr;
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline Joiner<PointType>* FindJoinParent(const Joiner<PointType>* joiner, OutPt<PointType>* op)
	{
		Joiner<PointType>* result = op->joiner;
		for (; ; )
		{
			if (op == result->op1)
			{
				if (result->next1 == joiner) return result;
				else result = result->next1;
			}
			else
			{
				if (result->next2 == joiner) return result;
				else result = result->next2;
			}
		}
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool CheckDisposeAdjacent(OutPt<PointType>*& op, const OutPt<PointType>* guard, OutRec<PointType>& outRec)
	{
		bool result = false;
		while (op->prev != op)
		{
			if (EqualsXY(op->pt, op->prev->pt) && op != guard &&
				op->prev->joiner && !op->joiner)
			{
				if (op == outRec.pts) outRec.pts = op->prev;
				op = DisposeOutPt(op);
				op = op->prev;
			}
			else if (!op->prev->joiner && op->prev != guard &&
				(DistanceSqr(op->pt, op->prev->pt) < 2.1))
			{
				if (op->prev == outRec.pts) outRec.pts = op;
				DisposeOutPt(op->prev);
				result = true;
			}
			else
				break;
		}

		while (op->next != op)
		{
			if (EqualsXY(op->pt, op->next->pt) && op != guard &&
				op->next->joiner && !op->joiner)
			{
				if (op == outRec.pts) outRec.pts = op->prev;
				op = DisposeOutPt(op);
				op = op->prev;
			}
			else if (!op->next->joiner && op->next != guard &&
				(DistanceSqr(op->pt, op->next->pt) < 2.1))
			{
				if (op->next == outRec.pts) outRec.pts = op;
				DisposeOutPt(op->next);
				result = true;
			}
			else
				break;
		}
		return result;
	}
	//------------------------------------------------------------------------------

	inline bool ValueBetween(int64_t val, int64_t end1, int64_t end2)
	{
		//NB accommodates axis aligned between where end1 == end2
		return ((val != end1) == (val != end2)) &&
			((val > end1) == (val < end2));
	}
	//------------------------------------------------------------------------------

	inline bool ValueEqualOrBetween(int64_t val, int64_t end1, int64_t end2)
	{
		return (val == end1) || (val == end2) || ((val > end1) == (val < end2));
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool PointBetween(PointType pt, PointType corner1, PointType corner2)
	{
		//NB points may not be collinear
		return ValueEqualOrBetween(GetX(pt), GetX(corner1), GetX(corner2)) &&
			ValueEqualOrBetween(GetY(pt), GetY(corner1), GetY(corner2));
	}
	//------------------------------------------------------------------------------

	template<typename PointType>
	inline bool EdgesAdjacentInAEL(const IntersectNode<PointType>& inode)
	{
		return (inode.edge1->next_in_ael == inode.edge2) || (inode.edge1->prev_in_ael == inode.edge2);
	}
	//------------------------------------------------------------------------------

	template<typename PointType, typename ClipperFlags>
	inline bool BuildPath(OutPt<PointType>& op, bool isOpen, std::vector<PointType>& path)
	{
		int cnt = PointCount(&op);
		if (cnt < 2) return false;
		path.resize(0);
		path.reserve(cnt);
		PointType lastPt;
		OutPt<PointType> *op2;
		if constexpr (ClipperFlags::reverse_orientation) {
			op = op.next;
			lastPt = op.pt;
			path.push_back(lastPt);
			op2 = op.next;
		} else {
			lastPt = op.pt;
			path.push_back(lastPt);
			op2 = op.prev;
		}
		while (op2 != &op)
		{
			if (! EqualsXY<PointType>(op2->pt, lastPt))
			{
				lastPt = op2->pt;
				path.push_back(lastPt);
			}
			if constexpr (ClipperFlags::reverse_orientation)
				op2 = op2->next;
			else
				op2 = op2->prev;
		}
		return true;
	}

	template<typename PointType>
	inline bool IsValidPath(OutPt<PointType>* op)
	{
		return (op && op->next != op);
	}

} // namespace Clipper2Lib::engine::detail

#endif // clipper_engine_detail_h
