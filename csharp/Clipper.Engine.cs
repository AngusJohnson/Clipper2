/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (release candidate 1) - also known as Clipper2             *
* Date      :  27 February 2022                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This is the main polygon clipping module                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Collections.Generic;

namespace ClipperLib2
{

	using Path = List<Point64>;
	using Paths = List<List<Point64>>;
	using PathD = List<PointD>;
	using PathsD = List<List<PointD>>;

	enum VertexFlags
	{
		None = 0,
		OpenStart = 1,
		OpenEnd = 2,
		LocalMax = 4,
		LocalMin = 8
	};

	internal class Vertex
	{
		public Point64 pt;
		public Vertex next;
		public Vertex prev;
		public VertexFlags flags;
		public Vertex(Point64 pt, VertexFlags flags, Vertex prev)
		{
			this.pt = pt; this.flags = flags; next = null; this.prev = prev;
		}
	};

	//Every closed path (or polygon) is made up of a series of vertices forming
	//edges that alternate between going up (relative to the Y-axis) and going
	//down. Edges consecutively going up or consecutively going down are called
	//'bounds' (or sides if they're simple polygons). 'Local Minima' refer to
	//vertices where descending bounds become ascending ones.

	internal struct LocalMinima
	{
		public Vertex vertex;
		public PathType polytype;
		public bool is_open;
		public LocalMinima(Vertex vertex, PathType polytype, bool is_open = false)
		{
			this.vertex = vertex;
			this.polytype = polytype;
			this.is_open = is_open;
		}
	};

	internal struct IntersectNode
	{
		public Point64 pt;
		public Active edge1;
		public Active edge2;
		public IntersectNode(Point64 pt, Active edge1, Active edge2)
		{
			this.pt = pt;
			this.edge1 = edge1;
			this.edge2 = edge2;
		}
	};

	internal struct LocMinSorter : IComparer<LocalMinima>
	{
		public int Compare(LocalMinima locMin1, LocalMinima locMin2)
		{
			return locMin2.vertex.pt.Y.CompareTo(locMin1.vertex.pt.Y);
		}
	}

	internal class OutPt
	{
		public Point64 pt;
		public OutPt next;
		public OutPt prev;
		public OutPt(Point64 pt)
		{
			this.pt = pt;
			next = this;
			prev = this;
		}
	};

	//OutRec: contains a path in the clipping solution. Edges in the AEL will
	//have OutRec pointers assigned when they form part of the clipping solution.
	internal class OutRec
	{
		public int idx;
		public OutRec owner;
		public Active front_e;
		public Active back_e;
		public OutPt pts;
		public PolyPathBase polypath;
		public OutRecState state;
	};

	internal class Active
	{
		public Point64 bot;
		public Point64 top;
		public long curr_x;  //current (updated at every new scanline)
		public double dx;
		public int wind_dx;  //1 or -1 depending on winding direction
		public int wind_cnt;
		public int wind_cnt2;  //winding count of the opposite polytype
		public OutRec outrec;
		//AEL: 'active edge list' (Vatti's AET - active edge table)
		//     a linked list of all edges (from left to right) that are present
		//     (or 'active') within the current scanbeam (a horizontal 'beam' that
		//     sweeps from bottom to top over the paths in the clipping operation).
		public Active prev_in_ael;
		public Active next_in_ael;
		//SEL: 'sorted edge list' (Vatti's ST - sorted table)
		//     linked list used when sorting edges into their new positions at the
		//     top of scanbeams, but also (re)used to process horizontals.
		public Active prev_in_sel;
		public Active next_in_sel;
		public Active jump;
		public Vertex vertex_top;
		public LocalMinima local_min;  //the bottom of an edge 'bound' (also Vatti)
	};

	public class Clipper
	{

		private ClipType _cliptype;
		private FillRule _fillrule;
		private Active _actives;
		private Active _sel;
		private readonly List<LocalMinima> _minimaList;
		private readonly List<IntersectNode> _intersectList;
		private readonly List<Vertex> _vertexList;
		private readonly List<OutRec> _outrecList;
		private readonly List<long> _scanlineList;
		private Boolean _isSortedMinimaList;
		private Boolean _hasOpenPaths;
		private int _currentLocMin;
		private long _currentBotY;

		public Clipper()
		{
			_minimaList = new List<LocalMinima>();
			_intersectList = new List<IntersectNode>();
			_vertexList = new List<Vertex>();
			_outrecList = new List<OutRec>();
			_scanlineList = new List<long>();
		}

		static bool IsOdd(int val)
		{
			return ((val & 1) != 0);
		}

		static void SetCheckFlag(OutRec outrec)
		{
			if (outrec.state == OutRecState.Inner)
				outrec.state = OutRecState.InnerCheck;
			else if (outrec.state == OutRecState.Outer)
				outrec.state = OutRecState.OuterCheck;
		}

		static void UnsetCheckFlag(OutRec outrec)
		{
			if (outrec.state == OutRecState.InnerCheck)
				outrec.state = OutRecState.Inner;
			else if (outrec.state == OutRecState.OuterCheck)
				outrec.state = OutRecState.Outer;
		}

		private static bool IsHotEdge(Active ae)
		{
			return ae.outrec != null;
		}

		private static bool IsOpen(Active ae)
		{
			return ae.local_min.is_open;
		}

		private static bool IsOpenEnd(Active ae)
		{
			return ae.local_min.is_open && 
				(ae.vertex_top.flags & (VertexFlags.OpenStart | VertexFlags.OpenEnd)) != VertexFlags.None;
		}

		static Active GetPrevHotEdge(Active ae)
		{
			Active prev = ae.prev_in_ael;
			while (prev != null && (IsOpen(prev) || !IsHotEdge(prev)))
				prev = prev.prev_in_ael;
			return prev;
		}

		static bool IsOuter(OutRec outrec)
		{
			return (outrec.state == OutRecState.Outer || outrec.state == OutRecState.OuterCheck);
		}

		static void SetAsOuter(OutRec outrec)
		{
			outrec.state = OutRecState.Outer;
		}

		static bool IsInner(OutRec outrec)
		{
			return (outrec.state == OutRecState.Inner || outrec.state == OutRecState.InnerCheck);
		}

		static void SetAsInner(OutRec outrec)
		{
			outrec.state = OutRecState.Inner;
		}

		static bool IsFront(Active ae)
		{
			//the front edge will be the LEFT edge when it's an OUTER polygon
			//so that outer polygons will be orientated clockwise
			if (ae.outrec.state == OutRecState.Open)
				return (ae.wind_dx > 0);
			else
				return (ae == ae.outrec.front_e);
		}

		static bool IsInvalidPath(OutPt op)
		{
			return (op == null || op.next == op);
		}

		/*******************************************************************************
		*  Dx:                             0(90deg)                                    *
		*                                  |                                           *
		*               +inf (180deg) <--- o --. -inf (0deg)                          *
		*******************************************************************************/

		static double GetDx(Point64 pt1, Point64 pt2)
		{
			double dy = pt2.Y - pt1.Y;
			if (dy != 0)
				return (pt2.X - pt1.X) / dy;
			else if (pt2.X > pt1.X)
				return double.NegativeInfinity;
			else
				return double.PositiveInfinity;
		}

		static long TopX(Active ae, long currentY)
		{
			if ((currentY == ae.top.Y) || (ae.top.X == ae.bot.X))
				return ae.top.X;
			else
				return ae.bot.X + (long)Math.Round(ae.dx * (currentY - ae.bot.Y));
		}

		static bool IsHorizontal(Active ae)
		{
			return (ae.top.Y == ae.bot.Y);
		}

		static bool IsHeadingRightHorz(Active ae)
		{
			return (ae.dx == double.NegativeInfinity);
		}

		static bool IsHeadingLeftHorz(Active ae)
		{
			return (ae.dx == double.PositiveInfinity);
		}

		static void SwapActives(ref Active ae1, ref Active ae2)
		{
			Active ae = ae1;
			ae1 = ae2;
			ae2 = ae;
		}

		static PathType GetPolyType(Active ae)
		{
			return ae.local_min.polytype;
		}

		static bool IsSamePolyType(Active ae1, Active ae2)
		{
			return ae1.local_min.polytype == ae2.local_min.polytype;
		}

		static Point64 GetIntersectPoint(Active ae1, Active ae2)
		{
			double b1, b2;
			if (ae1.dx == ae2.dx) return ae1.top;

			if (ae1.dx == 0)
			{
				if (IsHorizontal(ae2)) return new Point64(ae1.bot.X, ae2.bot.Y);
				b2 = ae2.bot.Y - (ae2.bot.X / ae2.dx);
				return new Point64(ae1.bot.X, (long)Math.Round(ae1.bot.X / ae2.dx + b2));
			}
			else if (ae2.dx == 0)
			{
				if (IsHorizontal(ae1)) return new Point64(ae2.bot.X, ae1.bot.Y);
				b1 = ae1.bot.Y - (ae1.bot.X / ae1.dx);
				return new Point64(ae2.bot.X, (long)Math.Round(ae2.bot.X / ae1.dx + b1));
			}
			else
			{
				b1 = ae1.bot.X - ae1.bot.Y * ae1.dx;
				b2 = ae2.bot.X - ae2.bot.Y * ae2.dx;
				double q = (b2 - b1) / (ae1.dx - ae2.dx);
				return (Math.Abs(ae1.dx) < Math.Abs(ae2.dx)) ?
					new Point64((long)Math.Round(ae1.dx * q + b1), (long)Math.Round(q)) :
					new Point64((long)Math.Round(ae2.dx * q + b2), (long)Math.Round(q));
			}
		}

		static void SetDx(Active ae)
		{
			ae.dx = GetDx(ae.bot, ae.top);
		}

		static bool IsLeftBound(Active ae)
		{
			return ae.wind_dx > 0;
		}

		static Vertex NextVertex(Active ae)
		{
			if (IsLeftBound(ae))
				return ae.vertex_top.next;
			else
				return ae.vertex_top.prev;
		}

		static Vertex NextVertex(Vertex op, bool going_forward)
		{
			if (going_forward)
				return op.next;
			else
				return op.prev;
		}

		static bool IsClockwise(OutPt op)
		{
			return InternalClipperFunc.CrossProduct(op.prev.pt, op.pt, op.next.pt) >= 0;
		}

		static bool IsMaxima(Active ae)
		{
			return ((ae.vertex_top.flags & VertexFlags.LocalMax) != VertexFlags.None);
		}

		private Active GetMaximaPair(Active ae)
		{
			Active ae2;
			if (IsHorizontal(ae))
			{
				//we can't be sure whether the MaximaPair is on the left or right, so ...
				ae2 = ae.prev_in_ael;
				while (ae2 != null && ae2.curr_x >= ae.top.X)
				{
					if (ae2.vertex_top == ae.vertex_top) return ae2;  //Found!
					ae2 = ae2.prev_in_ael;
				}
				ae2 = ae.next_in_ael;
				while (ae2 != null && (TopX(ae2, ae.top.Y) <= ae.top.X))
				{
					if (ae2.vertex_top == ae.vertex_top) return ae2;  //Found!
					ae2 = ae2.next_in_ael;
				}
				return null;
			}
			else
			{
				ae2 = ae.next_in_ael;
				while (ae2 != null)
				{
					if (ae2.vertex_top == ae.vertex_top) return ae2;  //Found!
					ae2 = ae2.next_in_ael;
				}
				return null;
			}
		}

		static int PointCount(OutPt op)
		{
			if (op == null) return 0;
			OutPt p = op;
			int cnt = 0;
			do
			{
				cnt++;
				p = p.next;
			} while (p != op);
			return cnt;
		}

		static void RemoveOutPt(OutPt pp)
		{
			pp.prev.next = pp.next;
			pp.next.prev = pp.prev;
			//delete pp;
		}


		internal struct IntersectListSort : IComparer<IntersectNode>
		{
			public int Compare(IntersectNode a, IntersectNode b)
			{
				if (a.pt.Y == b.pt.Y)
				{
					return (a.pt.X < b.pt.X) ? -1 : 1;
				}
				else
				{
					return (a.pt.Y > b.pt.Y) ? -1 : 1;
				}
			}
		}

		static void SetSides(OutRec outrec, Active start_edge, Active end_edge)
		{
			outrec.front_e = start_edge;
			outrec.back_e = end_edge;
		}

		static void SwapOutrecs(Active ae1, Active ae2)
		{
			OutRec or1 = ae1.outrec;
			OutRec or2 = ae2.outrec;
			if (or1 == or2)
			{
				Active ae = or1.front_e;
				or1.front_e = or1.back_e;
				or1.back_e = ae;
				return;
			}
			if (or1 != null)
			{
				if (ae1 == or1.front_e)
					or1.front_e = ae2;
				else
					or1.back_e = ae2;
			}
			if (or2 != null)
			{
				if (ae2 == or2.front_e)
					or2.front_e = ae1;
				else
					or2.back_e = ae1;
			}
			ae1.outrec = or2;
			ae2.outrec = or1;
		}

		static double Area(OutPt op)
		{
			double area = 0.0, d;
			OutPt op2 = op;
			if (op2 != null)
			{
				do
				{
					d = (double)(op2.prev.pt.X + op2.pt.X);
					area += d * (op2.prev.pt.Y - op2.pt.Y);
					op2 = op2.next;
				} while (op2 != op);
			}
			return area * -0.5;  //positive areas for clockwise paths
		}

		static void ReverseOutPts(OutPt op)
		{
			if (op == null) return;

			OutPt op1 = op;
			OutPt op2;

			do
			{
				op2 = op1.next;
				op1.next = op1.prev;
				op1.prev = op2;
				op1 = op2;
			} while (op1 != op);
		}

		private bool RecheckInnerOuter(Active ae)
		{
			double area = Area(ae.outrec.pts);
			bool result = area != 0.0;
			if (!result) return result;  //returns false when area == 0

			bool was_outer = IsOuter(ae.outrec);
			bool is_outer = true;

			Active ae2 = ae.prev_in_ael;
			while (ae2 != null)
			{
				if (IsHotEdge(ae2) && !IsOpen(ae2)) is_outer = !is_outer;
				ae2 = ae2.prev_in_ael;
			}

			if (is_outer != was_outer)
			{
				if (is_outer)
					SetAsOuter(ae.outrec);
				else
					SetAsInner(ae.outrec);
			}

			ae2 = GetPrevHotEdge(ae);
			if (is_outer)
			{
				if (ae2 != null && IsInner(ae2.outrec))
					ae.outrec.owner = ae2.outrec;
				else
					ae.outrec.owner = null;
			}
			else
			{
				if (ae2 == null)
					SetAsOuter(ae.outrec);
				else if (IsInner(ae2.outrec))
					ae.outrec.owner = ae2.outrec.owner;
				else
					ae.outrec.owner = ae2.outrec;
			}

			if ((area > 0.0) != is_outer) ReverseOutPts(ae.outrec.pts);
			UnsetCheckFlag(ae.outrec);

			return result;
		}

		private static void SwapSides(OutRec outrec)
		{
			Active ae2 = outrec.front_e;
			outrec.front_e = outrec.back_e;
			outrec.back_e = ae2;
			outrec.pts = outrec.pts.next;
		}

		private bool FixSides(Active ae)
		{
			bool fix = !RecheckInnerOuter(ae) || ((IsOuter(ae.outrec)) != IsFront(ae));
			if (fix) SwapSides(ae.outrec);
			return fix;
		}

		private void SetOwnerAndInnerOuterState(Active ae)
		{
			Active ae2;
			OutRec outrec = ae.outrec;

			if (IsOpen(ae))
			{
				outrec.owner = null;
				outrec.state = OutRecState.Open;
				return;
			}
			//set owner ...
			if (IsHeadingLeftHorz(ae))
			{
				ae2 = ae.next_in_ael;  //ie assess state from opposite direction
				while (ae2 != null && (!IsHotEdge(ae2) || IsOpen(ae2)))
					ae2 = ae2.next_in_ael;
				if (ae2 == null)
					outrec.owner = null;
				else if ((ae2.outrec.state == OutRecState.Outer) == (ae2.outrec.front_e == ae2))
					outrec.owner = ae2.outrec.owner;
				else
					outrec.owner = ae2.outrec;
			}
			else
			{
				ae2 = GetPrevHotEdge(ae);
				while (ae2 != null && (!IsHotEdge(ae2) || IsOpen(ae2)))
					ae2 = ae2.prev_in_ael;
				if (ae2 == null)
					outrec.owner = null;
				else if (IsOuter(ae2.outrec) == (ae2.outrec.back_e == ae2))
					outrec.owner = ae2.outrec.owner;
				else
					outrec.owner = ae2.outrec;
			}
			//set inner/outer ...
			if (outrec.owner == null || IsInner(outrec.owner))
				outrec.state = OutRecState.Outer;
			else
				outrec.state = OutRecState.Inner;
		}

		private static bool EdgesAdjacentInAEL(IntersectNode inode)
		{
			return (inode.edge1.next_in_ael == inode.edge2) || (inode.edge1.prev_in_ael == inode.edge2);
		}

		protected void CleanUp()
		{
			while (_actives != null) DeleteFromAEL(_actives);
			_scanlineList.Clear();
			DisposeIntersectNodes();
			_outrecList.Clear();
		}

		public void Clear()
		{
			CleanUp();
			_minimaList.Clear();
			_vertexList.Clear();
			_currentLocMin = 0;
			_isSortedMinimaList = false;
			_hasOpenPaths = false;
		}

		protected void Reset()
		{
			if (!_isSortedMinimaList)
			{
				_minimaList.Sort(new LocMinSorter());
				_isSortedMinimaList = true;
			}
			_scanlineList.Capacity = _minimaList.Count;
			for (int i = _minimaList.Count - 1; i >= 0; i--)
				_scanlineList.Add(_minimaList[i].vertex.pt.Y);

			_currentBotY = 0;
			_currentLocMin = 0;
			_actives = null;
			_sel = null;
		}

		private void InsertScanline(long y)
		{
			var index = _scanlineList.BinarySearch(y);
			if (index >= 0) return;
			index = ~index;
			_scanlineList.Insert(index, y);
		}

		private bool PopScanline(out long y)
		{
			int cnt = _scanlineList.Count - 1;
			if (cnt < 0) { y = 0; return false; }
			y = _scanlineList[cnt];
			_scanlineList.RemoveAt(cnt--);
			while (cnt >= 0 && y == _scanlineList[cnt])
				_scanlineList.RemoveAt(cnt--);
			return true;
		}

		private bool HasLocMinAtY(long y)
		{
			return (_currentLocMin < _minimaList.Count && _minimaList[_currentLocMin].vertex.pt.Y == y);
		}
		LocalMinima PopLocalMinima()
		{
			return _minimaList[_currentLocMin++];
		}

		private void AddLocMin(Vertex vert, PathType polytype, bool is_open)
		{
			//make sure the vertex is added only once ...
			if ((vert.flags & VertexFlags.LocalMin) != VertexFlags.None) return;
			vert.flags |= VertexFlags.LocalMin;

			LocalMinima lm = new LocalMinima(vert, polytype, is_open);
			_minimaList.Add(lm);
		}

		private void AddPathToVertexList(Path path, PathType polytype, bool is_open)
		{
			int path_len = path.Count;
			if (!is_open)
			{
				while (path_len > 1 && (path[path_len - 1] == path[0])) --path_len;
				if (path_len < 2) return;
			}
			else if (path_len == 0) return;

			_vertexList.Capacity = _vertexList.Count + path_len;
			Vertex vCurr, vPrev, v0 = new Vertex(path[0], VertexFlags.None, null);
			_vertexList.Add(v0);
			vPrev = v0;
			for (int i = 1; i < path_len; i++)
			{
				vCurr = new Vertex(path[i], VertexFlags.None, vPrev);
				vPrev.next = vCurr;
				_vertexList.Add(vCurr);
				vPrev = vCurr;
			}
			v0.prev = vPrev;
			vPrev.next = v0;

			bool going_up, going_up0;
			if (is_open)
			{
				vCurr = v0.next;
				while (vCurr != v0 && vCurr.pt.Y == v0.pt.Y) vCurr = vCurr.next;
				going_up = vCurr.pt.Y <= v0.pt.Y;
				if (going_up)
				{
					v0.flags = VertexFlags.OpenStart;
					AddLocMin(v0, polytype, true);
				}
				else
					v0.flags = VertexFlags.OpenStart | VertexFlags.LocalMax;
			}
			else if (v0.pt.Y == vPrev.pt.Y)
			{
				vPrev = vPrev.prev;
				while (vPrev != v0 && vPrev.pt.Y == v0.pt.Y) vPrev = vPrev.prev;
				if (vPrev == v0) return; //ie a flat closed path
				going_up = vPrev.pt.Y < v0.pt.Y;       //ie direction leading up to v0
			}
			else
				going_up = v0.pt.Y < vPrev.pt.Y;   //ie direction leading up to v0

			going_up0 = going_up;
			vPrev = v0;
			vCurr = vPrev.next;
			//nb: polygon orientation is determined later (see InsertLocalMinimaIntoAEL).
			while (vCurr != v0)
			{
				if (vCurr.pt == vPrev.pt)
				{
					vCurr = vCurr.next;
					continue;
				}

				if (vPrev.next != vCurr)
				{
					//ie remove duplicates
					vPrev.next = vCurr;
					vCurr.prev = vPrev;
				}

				if (going_up && vCurr.pt.Y > vPrev.pt.Y)
				{
					vPrev.flags |= VertexFlags.LocalMax;
					going_up = false;
				}
				else if (!going_up && vCurr.pt.Y < vPrev.pt.Y)
				{
					AddLocMin(vPrev, polytype, is_open);
					going_up = true;
				}
				vPrev = vCurr;
				vCurr = vCurr.next;
			}
			//close the double-linked loop
			vPrev.next = v0;
			v0.prev = vPrev;

			if (is_open)
			{
				vPrev.flags |= VertexFlags.OpenEnd;
				if (going_up)
					vPrev.flags |= VertexFlags.LocalMax;
				else
					AddLocMin(vPrev, polytype, is_open);
			}
			else if (going_up != going_up0)
			{
				if (going_up0) AddLocMin(vPrev, polytype, is_open);
				else vPrev.flags |= VertexFlags.LocalMax;
			}
		}

		public void AddSubject(Path path, bool is_open = false)
		{
			AddPath(path, PathType.Subject, is_open);
		}

		public void AddClip(Path path)
		{
			AddPath(path, PathType.Clip, false);
		}

		internal void AddPath(Path path, PathType polytype, bool is_open = false)
		{
			if (is_open)
			{
				if (polytype == PathType.Clip)
					throw new ClipperLibException("AddPath: Only subject paths may be open.");
				_hasOpenPaths = true;
			}
			_isSortedMinimaList = false;
			AddPathToVertexList(path, polytype, is_open);
		}

		public void AddSubject(Paths paths, bool is_open = false)
		{
			AddPaths(paths, PathType.Subject, is_open);
		}

		public void AddClip(Paths paths)
		{
			AddPaths(paths, PathType.Clip, false);
		}

		internal void AddPaths(Paths paths, PathType polytype, bool is_open = false)
		{
			if (is_open)
			{
				if (polytype == PathType.Clip)
					throw new ClipperLibException("AddPath: Only subject paths may be open.");
				_hasOpenPaths = true;
			}
			_isSortedMinimaList = false;
			for (int i = 0; i < paths.Count; i++)
				AddPathToVertexList(paths[i], polytype, is_open);
		}



		private bool IsContributingClosed(Active ae)
		{
			switch (_fillrule)
			{
				case FillRule.NonZero:
					if (Math.Abs(ae.wind_cnt) != 1) return false;
					break;
				case FillRule.Positive:
					if (ae.wind_cnt != 1) return false;
					break;
				case FillRule.Negative:
					if (ae.wind_cnt != -1) return false;
					break;
				default:
					break;  // delphi2cpp translation note: no warnings
			}

			switch (_cliptype)
			{
				case ClipType.Intersection:
					switch (_fillrule)
					{
						case FillRule.EvenOdd:
						case FillRule.NonZero: return (ae.wind_cnt2 != 0);
						case FillRule.Positive: return (ae.wind_cnt2 > 0);
						case FillRule.Negative: return (ae.wind_cnt2 < 0);
					}
					break;
				case ClipType.Union:
					switch (_fillrule)
					{
						case FillRule.EvenOdd:
						case FillRule.NonZero: return (ae.wind_cnt2 == 0);
						case FillRule.Positive: return (ae.wind_cnt2 <= 0);
						case FillRule.Negative: return (ae.wind_cnt2 >= 0);
					}
					break;
				case ClipType.Difference:
					if (GetPolyType(ae) == PathType.Subject)
						switch (_fillrule)
						{
							case FillRule.EvenOdd:
							case FillRule.NonZero: return (ae.wind_cnt2 == 0);
							case FillRule.Positive: return (ae.wind_cnt2 <= 0);
							case FillRule.Negative: return (ae.wind_cnt2 >= 0);
						}
					else
						switch (_fillrule)
						{
							case FillRule.EvenOdd:
							case FillRule.NonZero: return (ae.wind_cnt2 != 0);
							case FillRule.Positive: return (ae.wind_cnt2 > 0);
							case FillRule.Negative: return (ae.wind_cnt2 < 0);
						}
					break;
				case ClipType.Xor:
					return true;  //XOr is always contributing unless open
				default:
					return false;  // delphi2cpp translation note: no warnings
			}
			return false;  //we should never get here
		}

		private bool IsContributingOpen(Active ae)
		{
			return _cliptype switch
			{
				ClipType.Intersection => (ae.wind_cnt2 != 0),
				ClipType.Union => (ae.wind_cnt == 0 && ae.wind_cnt2 == 0),
				ClipType.Difference => (ae.wind_cnt2 == 0),
				ClipType.Xor => (ae.wind_cnt != 0) != (ae.wind_cnt2 != 0),
				_ => false,
			};
		}

		private void SetWindCountForClosedPathEdge(Active ae)
		{
			//Wind counts refer to polygon regions not edges, so here an edge's WindCnt
			//indicates the higher of the wind counts for the two regions touching the
			//edge. (nb: Adjacent regions can only ever have their wind counts differ by
			//one. Also, open paths have no meaningful wind directions or counts.)

			Active ae2 = ae.prev_in_ael;
			//find the nearest closed path edge of the same PolyType in AEL (heading left)
			PathType pt = GetPolyType(ae);
			while (ae2 != null && (GetPolyType(ae2) != pt || IsOpen(ae2))) ae2 = ae2.prev_in_ael;

			if (ae2 == null)
			{
				ae.wind_cnt = ae.wind_dx;
				ae2 = _actives;
			}
			else if (_fillrule == FillRule.EvenOdd)
			{
				ae.wind_cnt = ae.wind_dx;
				ae.wind_cnt2 = ae2.wind_cnt2;
				ae2 = ae2.next_in_ael;
			}
			else
			{
				//NonZero, positive, or negative filling here ...
				//if ae's WindCnt is in the SAME direction as its WindDx, then polygon
				//filling will be on the right of 'ae'.
				//nb: neither ae2.WindCnt nor ae2.WindDx should ever be 0.
				if (ae2.wind_cnt * ae2.wind_dx < 0)
				{
					//opposite directions so 'ae' is outside 'ae2' ...
					if (Math.Abs(ae2.wind_cnt) > 1)
					{
						//outside prev poly but still inside another.
						if (ae2.wind_dx * ae.wind_dx < 0)
							//reversing direction so use the same WC
							ae.wind_cnt = ae2.wind_cnt;
						else
							//otherwise keep 'reducing' the WC by 1 (ie towards 0) ...
							ae.wind_cnt = ae2.wind_cnt + ae.wind_dx;
					}
					else
						//now outside all polys of same polytype so set own WC ...
						ae.wind_cnt = (IsOpen(ae) ? 1 : ae.wind_dx);
				}
				else
				{
					//'ae' must be inside 'ae2'
					if (ae2.wind_dx * ae.wind_dx < 0)
						//reversing direction so use the same WC
						ae.wind_cnt = ae2.wind_cnt;
					else
						//otherwise keep 'increasing' the WC by 1 (ie away from 0) ...
						ae.wind_cnt = ae2.wind_cnt + ae.wind_dx;
				}
				ae.wind_cnt2 = ae2.wind_cnt2;
				ae2 = ae2.next_in_ael;  //ie get ready to calc WindCnt2
			}

			//update wind_cnt2 ...
			if (_fillrule == FillRule.EvenOdd)
				while (ae2 != ae)
				{
					if (GetPolyType(ae2) != pt && !IsOpen(ae2))
						ae.wind_cnt2 = (ae.wind_cnt2 == 0 ? 1 : 0);
					ae2 = ae2.next_in_ael;
				}
			else
				while (ae2 != ae)
				{
					if (GetPolyType(ae2) != pt && !IsOpen(ae2))
						ae.wind_cnt2 += ae2.wind_dx;
					ae2 = ae2.next_in_ael;
				}
		}

		private void SetWindCountForOpenPathEdge(Active ae)
		{
			Active ae2 = _actives;
			if (_fillrule == FillRule.EvenOdd)
			{
				int cnt1 = 0, cnt2 = 0;
				while (ae2 != ae)
				{
					if (GetPolyType(ae2) == PathType.Clip)
						cnt2++;
					else if (!IsOpen(ae2))
						cnt1++;
					ae2 = ae2.next_in_ael;
				}
				ae.wind_cnt = (IsOdd(cnt1) ? 1 : 0);
				ae.wind_cnt2 = (IsOdd(cnt2) ? 1 : 0);
			}
			else
			{
				while (ae2 != ae)
				{
					if (GetPolyType(ae2) == PathType.Clip)
						ae.wind_cnt2 += ae2.wind_dx;
					else if (!IsOpen(ae2))
						ae.wind_cnt += ae2.wind_dx;
					ae2 = ae2.next_in_ael;
				}
			}
		}

		private bool IsValidAelOrder(Active a1, Active a2)
		{
			Point64 pt;
			Vertex op1, op2;
			double d;

			if (a2.curr_x != a1.curr_x)
				return a2.curr_x > a1.curr_x;

			if (a1.bot.Y < a2.bot.Y)
				d = InternalClipperFunc.CrossProduct(a1.bot, a1.top, a2.bot);
			else if (a2.bot.Y < a1.bot.Y)
				d = InternalClipperFunc.CrossProduct(a2.bot, a2.top, a1.bot);
			else d = 0.0;

			op1 = a1.vertex_top;
			op2 = a2.vertex_top;

			while (d == 0)
			{
				if (op1.pt == op2.pt)
				{
					pt = op1.pt;
					op1 = NextVertex(op1, IsLeftBound(a1));
					op2 = NextVertex(op2, IsLeftBound(a2));
				}
				else if (op1.pt.Y >= op2.pt.Y)
				{
					pt = op1.pt;
					op1 = NextVertex(op1, IsLeftBound(a1));
				}
				else
				{
					pt = op2.pt;
					op2 = NextVertex(op2, IsLeftBound(a2));
				}

				if (op1.pt.Y > pt.Y || op2.pt.Y > pt.Y)
					d = -1.0;
				else //force a break to avoid an endless loop
					d = InternalClipperFunc.CrossProduct(op1.pt, pt, op2.pt);
			}
			return d < 0;
		}

		private void InsertLeftEdge(Active ae)
		{
			Active ae2;

			if (_actives == null)
			{
				ae.prev_in_ael = null;
				ae.next_in_ael = null;
				_actives = ae;
			}
			else if (IsValidAelOrder(ae, _actives))
			{
				ae.prev_in_ael = null;
				ae.next_in_ael = _actives;
				_actives.prev_in_ael = ae;
				_actives = ae;
			}
			else
			{
				ae2 = _actives;
				while (ae2.next_in_ael != null && IsValidAelOrder(ae2.next_in_ael, ae))
					ae2 = ae2.next_in_ael;
				ae.next_in_ael = ae2.next_in_ael;
				if (ae2.next_in_ael != null) ae2.next_in_ael.prev_in_ael = ae;
				ae.prev_in_ael = ae2;
				ae2.next_in_ael = ae;
			}
		}

		private void InsertRightEdge(Active ae, Active ae2)
		{
			ae2.next_in_ael = ae.next_in_ael;
			if (ae.next_in_ael != null) ae.next_in_ael.prev_in_ael = ae2;
			ae2.prev_in_ael = ae;
			ae.next_in_ael = ae2;
		}

		private void InsertLocalMinimaIntoAEL(long bot_y)
		{
			LocalMinima local_minima;
			Active left_bound, right_bound;
			//Add any local minima (if any) at BotY ...
			//nb: horizontal local minima edges should contain locMin.vertex.prev
			while (HasLocMinAtY(bot_y))
			{
				local_minima = PopLocalMinima();
				if ((local_minima.vertex.flags & VertexFlags.OpenStart) != VertexFlags.None)
				{
					left_bound = null;
				}
				else
				{
					left_bound = new Active
					{
						bot = local_minima.vertex.pt,
						curr_x = local_minima.vertex.pt.X,
						vertex_top = local_minima.vertex.prev,  //ie descending
						top = local_minima.vertex.prev.pt,
						wind_dx = -1,
						outrec = null,
						local_min = local_minima
					};
					SetDx(left_bound);
				}

				if ((local_minima.vertex.flags & VertexFlags.OpenEnd) != VertexFlags.None)
				{
					right_bound = null;
				}
				else
				{
					right_bound = new Active
					{
						bot = local_minima.vertex.pt,
						curr_x = local_minima.vertex.pt.X,
						vertex_top = local_minima.vertex.next,  //ie ascending
						top = local_minima.vertex.next.pt,
						wind_dx = 1,
						outrec = null,
						local_min = local_minima
					};
					SetDx(right_bound);
				}

				//Currently LeftB is just the descending bound and RightB is the ascending.
				//Now if the LeftB isn't on the left of RightB then we need swap them.
				if (left_bound != null && right_bound != null)
				{
					if (IsHorizontal(left_bound))
					{
						if (IsHeadingRightHorz(left_bound)) SwapActives(ref left_bound, ref right_bound);
					}
					else if (IsHorizontal(right_bound))
					{
						if (IsHeadingLeftHorz(right_bound)) SwapActives(ref left_bound, ref right_bound);
					}
					else if (left_bound.dx < right_bound.dx)
						SwapActives(ref left_bound, ref right_bound);
				}
				else if (left_bound == null)
				{
					left_bound = right_bound;
					right_bound = null;
				}

				bool contributing;
				InsertLeftEdge(left_bound);  ///////
																		 //todo: further validation of position in AEL ???

				if (IsOpen(left_bound))
				{
					SetWindCountForOpenPathEdge(left_bound);
					contributing = IsContributingOpen(left_bound);
				}
				else
				{
					SetWindCountForClosedPathEdge(left_bound);
					contributing = IsContributingClosed(left_bound);
				}

				if (right_bound != null)
				{
					right_bound.wind_cnt = left_bound.wind_cnt;
					right_bound.wind_cnt2 = left_bound.wind_cnt2;
					InsertRightEdge(left_bound, right_bound);  ///////
					if (contributing)
						AddLocalMinPoly(left_bound, right_bound, left_bound.bot, true);
					if (IsHorizontal(right_bound))
						PushHorz(right_bound);
					else
						InsertScanline(right_bound.top.Y);
				}
				else if (contributing)
					StartOpenPath(left_bound, left_bound.bot);

				if (IsHorizontal(left_bound))
					PushHorz(left_bound);
				else
					InsertScanline(left_bound.top.Y);

			}  //while (HasLocMinAtY())
		}

		private void PushHorz(Active ae)
		{
			ae.next_in_sel = _sel;
			_sel = ae;
		}

		private bool PopHorz(out Active ae)
		{
			ae = _sel;
			if (ae == null) return false;
			_sel = _sel.next_in_sel;
			return true;
		}

		private void AddLocalMinPoly(Active ae1, Active ae2, Point64 pt, 
			bool is_new = false, bool orientation_check_required = false)
		{

			OutRec outrec = new OutRec();
			_outrecList.Add(outrec);
			outrec.idx = _outrecList.Count - 1;
			outrec.pts = null;
			//PolyTree = null

			ae1.outrec = outrec;
			SetOwnerAndInnerOuterState(ae1);
			//flag when orientation needs to be rechecked later ...
			if (orientation_check_required) SetCheckFlag(outrec);
			ae2.outrec = outrec;

			if (!IsOpen(ae1))
			{
				//Setting the owner and inner/outer states (above) is an essential
				//precursor to setting edge 'sides' (ie left and right sides of output
				//polygons) and hence the orientation of output paths ...
				if (IsOuter(outrec) == is_new)
					SetSides(outrec, ae1, ae2);
				else
					SetSides(outrec, ae2, ae1);
			}
			OutPt op = new OutPt(pt);
			outrec.pts = op;

			//nb: currently ae1.NextInAEL == ae2 but this could change immediately on return
		}

		private void AddLocalMaxPoly(Active ae1, Active ae2, Point64 pt)
		{
			if (!IsOpen(ae1) && (IsFront(ae1) == IsFront(ae2)))
				if (!FixSides(ae1)) FixSides(ae2);

			OutPt op = AddOutPt(ae1, pt);
			// AddOutPt(ae2, pt); //this may no Int64er be necessary

			if (ae1.outrec == ae2.outrec)
			{
				if (ae1.outrec.state == OutRecState.OuterCheck || ae1.outrec.state == OutRecState.InnerCheck)
					RecheckInnerOuter(ae1);

				//nb: IsClockwise() is generally faster than Area() but will occasionally
				//give false positives when there are tiny self-intersections at the top...
				if (IsOuter(ae1.outrec))
				{
					if (!IsClockwise(op) && (Area(op) < 0.0))
						ReverseOutPts(ae1.outrec.pts);
				}
				else
				{
					if (IsClockwise(op) && (Area(op) > 0.0))
						ReverseOutPts(ae1.outrec.pts);
				}
				ae1.outrec.front_e = null;
				ae1.outrec.back_e = null;
				ae1.outrec = null;
				ae2.outrec = null;
			}
			//and to preserve the winding orientation of outrec ...
			else if (ae1.outrec.idx < ae2.outrec.idx)
				JoinOutrecPaths(ae1, ae2);
			else
				JoinOutrecPaths(ae2, ae1);
		}

		private void JoinOutrecPaths(Active ae1, Active ae2)
		{
			if (IsFront(ae1) == IsFront(ae2))
			{
				//one or other 'side' must be wrong ...
				if (IsOpen(ae1))
					SwapSides(ae2.outrec);
				else if (!FixSides(ae1) && !FixSides(ae2))
					throw new ClipperLibException("Error in JoinOutrecPaths()");
				if (ae1.outrec.owner == ae2.outrec) ae1.outrec.owner = ae2.outrec.owner;
			}

			//join ae2 outrec path onto ae1 outrec path and then delete ae2 outrec path
			//pointers. (nb: Only very rarely do the joining ends share the same coords.)
			OutPt p1_st = ae1.outrec.pts;
			OutPt p2_st = ae2.outrec.pts;
			OutPt p1_end = p1_st.next;
			OutPt p2_end = p2_st.next;
			if (IsFront(ae1))
			{
				p2_end.prev = p1_st;
				p1_st.next = p2_end;
				p2_st.next = p1_end;
				p1_end.prev = p2_st;
				ae1.outrec.pts = p2_st;
				if (IsOpen(ae1))
				{
					ae1.outrec.pts = p2_st;
				}
				else
				{
					ae1.outrec.front_e = ae2.outrec.front_e;
					ae1.outrec.front_e.outrec = ae1.outrec;
				}
				//strip duplicates ...
				if ((p2_end != p2_st) && (p2_end.pt == p2_end.prev.pt))
					RemoveOutPt(p2_end);
			}
			else
			{
				p1_end.prev = p2_st;
				p2_st.next = p1_end;
				p1_st.next = p2_end;
				p2_end.prev = p1_st;
				if (IsOpen(ae1))
				{
					ae1.outrec.pts = p1_st;
				}
				else
				{
					ae1.outrec.back_e = ae2.outrec.back_e;
					ae1.outrec.back_e.outrec = ae1.outrec;
				}
				//strip duplicates ...
				if ((p1_end != p1_st) && (p1_end.pt == p1_end.prev.pt))
					RemoveOutPt(p1_end);
			}

			if ((ae1.outrec.pts.pt == ae1.outrec.pts.prev.pt) && !IsInvalidPath(ae1.outrec.pts))
				RemoveOutPt(ae1.outrec.pts.prev);

			//after joining, the ae2.OutRec must contains no vertices ...
			ae2.outrec.front_e = null;
			ae2.outrec.back_e = null;
			ae2.outrec.pts = null;
			ae2.outrec.owner = ae1.outrec;  //this may be redundant

			if (IsOpenEnd(ae1))
			{
				ae2.outrec.pts = ae1.outrec.pts;
				ae1.outrec.pts = null;
			}

			//and ae1 and ae2 are maxima and are about to be dropped from the Actives list.
			ae1.outrec = null;
			ae2.outrec = null;
		}

		private OutPt AddOutPt(Active ae, Point64 pt)
		{
			OutPt new_op;

			//Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
			//op_front[.Prev]* ~~~> op_back & op_back == op_front.Next
			OutRec outrec = ae.outrec;
			bool to_front = IsFront(ae);
			OutPt op_front = outrec.pts;
			OutPt op_back = op_front.next;

			if (to_front && (pt == op_front.pt))
				new_op = op_front;
			else if (!to_front && (pt == op_back.pt))
				new_op = op_back;
			else
			{
				new_op = new OutPt(pt);
				op_back.prev = new_op;
				new_op.prev = op_front;
				new_op.next = op_back;
				op_front.next = new_op;
				if (to_front) outrec.pts = new_op;
			}
			return new_op;
		}

		private void StartOpenPath(Active ae, Point64 pt)
		{

			OutRec outrec = new OutRec();
			_outrecList.Add(outrec);
			outrec.idx = _outrecList.Count - 1;
			outrec.owner = null;
			outrec.state = OutRecState.Open;
			outrec.pts = null;
			//outrec.PolyTree = null;
			outrec.back_e = null;
			outrec.front_e = null;

			ae.outrec = outrec;

			OutPt op = new OutPt(pt);
			outrec.pts = op;
		}

		private void UpdateEdgeIntoAEL(Active ae)
		{
			ae.bot = ae.top;
			ae.vertex_top = NextVertex(ae);
			ae.top = ae.vertex_top.pt;
			ae.curr_x = ae.bot.X;
			SetDx(ae);
			if (!IsHorizontal(ae)) InsertScanline(ae.top.Y);
		}

		private void IntersectEdges(Active ae1, Active ae2, 
			Point64 pt, bool orientation_check_required = false)
		{
			//MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
			if (_hasOpenPaths && (IsOpen(ae1) || IsOpen(ae2)))
			{
				if (IsOpen(ae1) && IsOpen(ae2)) return;
				Active edge_o, edge_c;
				if (IsOpen(ae1))
				{
					edge_o = ae1;
					edge_c = ae2;
				}
				else
				{
					edge_o = ae2;
					edge_c = ae1;
				}

				switch (_cliptype)
				{
					case ClipType.Intersection:
					case ClipType.Difference:
						if (IsSamePolyType(edge_o, edge_c) || (Math.Abs(edge_c.wind_cnt) != 1)) return;
						break;
					case ClipType.Union:
						if (IsHotEdge(edge_o) != ((Math.Abs(edge_c.wind_cnt) != 1) || (IsHotEdge(edge_o) != (edge_c.wind_cnt != 0)))) return;  //just works!
						break;
					case ClipType.Xor:
						if (Math.Abs(edge_c.wind_cnt) != 1) return;
						break;
					case ClipType.None:
						throw new ClipperLibException("Error in IntersectEdges - ClipType is None!");
				}
				//toggle contribution ...
				if (IsHotEdge(edge_o))
				{
					AddOutPt(edge_o, pt);
					edge_o.outrec = null;
				}
				else
					StartOpenPath(edge_o, pt);
				return;
			}

			//UPDATE WINDING COUNTS...

			int old_e1_windcnt, old_e2_windcnt;
			if (ae1.local_min.polytype == ae2.local_min.polytype)
			{
				if (_fillrule == FillRule.EvenOdd)
				{
					old_e1_windcnt = ae1.wind_cnt;
					ae1.wind_cnt = ae2.wind_cnt;
					ae2.wind_cnt = old_e1_windcnt;
				}
				else
				{
					if (ae1.wind_cnt + ae2.wind_dx == 0)
						ae1.wind_cnt = -ae1.wind_cnt;
					else
						ae1.wind_cnt += ae2.wind_dx;
					if (ae2.wind_cnt - ae1.wind_dx == 0)
						ae2.wind_cnt = -ae2.wind_cnt;
					else
						ae2.wind_cnt -= ae1.wind_dx;
				}
			}
			else
			{
				if (_fillrule != FillRule.EvenOdd)
					ae1.wind_cnt2 += ae2.wind_dx;
				else
					ae1.wind_cnt2 = (ae1.wind_cnt2 == 0 ? 1 : 0);
				if (_fillrule != FillRule.EvenOdd)
					ae2.wind_cnt2 -= ae1.wind_dx;
				else
					ae2.wind_cnt2 = (ae2.wind_cnt2 == 0 ? 1 : 0);
			}

			switch (_fillrule)
			{
				case FillRule.Positive:
					old_e1_windcnt = ae1.wind_cnt;
					old_e2_windcnt = ae2.wind_cnt;
					break;
				case FillRule.Negative:
					old_e1_windcnt = -ae1.wind_cnt;
					old_e2_windcnt = -ae2.wind_cnt;
					break;
				default:
					old_e1_windcnt = Math.Abs(ae1.wind_cnt);
					old_e2_windcnt = Math.Abs(ae2.wind_cnt);
					break;
			}

			bool e1_windcnt_in_01 = old_e1_windcnt == 0 || old_e1_windcnt == 1;
			bool e2_windcnt_in_01 = old_e2_windcnt == 0 || old_e2_windcnt == 1;

			if ((!IsHotEdge(ae1) && !e1_windcnt_in_01) || (!IsHotEdge(ae2) && !e2_windcnt_in_01))
			{
				return;
			}
			//NOW PROCESS THE INTERSECTION ...

			//if both edges are 'hot' ...
			if (IsHotEdge(ae1) && IsHotEdge(ae2))
			{
				if ((old_e1_windcnt != 0 && old_e1_windcnt != 1) || (old_e2_windcnt != 0 && old_e2_windcnt != 1) ||
					(ae1.local_min.polytype != ae2.local_min.polytype && _cliptype != ClipType.Xor))
				{
					AddLocalMaxPoly(ae1, ae2, pt);
				}
				else if (IsFront(ae1) || (ae1.outrec == ae2.outrec))
				{
					AddLocalMaxPoly(ae1, ae2, pt);
					AddLocalMinPoly(ae1, ae2, pt);
				}
				else
				{
					//right & left bounds touching, NOT maxima & minima ...
					AddOutPt(ae1, pt);
					AddOutPt(ae2, pt);
					SwapOutrecs(ae1, ae2);
				}
			}
			//if one or other edge is 'hot' ...
			else if (IsHotEdge(ae1))
			{
				AddOutPt(ae1, pt);
				SwapOutrecs(ae1, ae2);
			}
			else if (IsHotEdge(ae2))
			{
				AddOutPt(ae2, pt);
				SwapOutrecs(ae1, ae2);
			}
			else
			{  //neither edge is 'hot'
				long e1Wc2, e2Wc2;
				switch (_fillrule)
				{
					case FillRule.Positive:
						e1Wc2 = ae1.wind_cnt2;
						e2Wc2 = ae2.wind_cnt2;
						break;
					case FillRule.Negative:
						e1Wc2 = -ae1.wind_cnt2;
						e2Wc2 = -ae2.wind_cnt2;
						break;
					default:
						e1Wc2 = Math.Abs(ae1.wind_cnt2);
						e2Wc2 = Math.Abs(ae2.wind_cnt2);
						break;
				}

				if (!IsSamePolyType(ae1, ae2))
				{
					AddLocalMinPoly(ae1, ae2, pt, false, orientation_check_required);
				}
				else if (old_e1_windcnt == 1 && old_e2_windcnt == 1)
					switch (_cliptype)
					{
						case ClipType.Intersection:
							if (e1Wc2 > 0 && e2Wc2 > 0)
								AddLocalMinPoly(ae1, ae2, pt, false, orientation_check_required);
							break;
						case ClipType.Union:
							if (e1Wc2 <= 0 && e2Wc2 <= 0)
								AddLocalMinPoly(ae1, ae2, pt, false, orientation_check_required);
							break;
						case ClipType.Difference:
							if (((GetPolyType(ae1) == PathType.Clip) && (e1Wc2 > 0) && (e2Wc2 > 0)) ||
									((GetPolyType(ae1) == PathType.Subject) && (e1Wc2 <= 0) && (e2Wc2 <= 0)))
							{
								AddLocalMinPoly(ae1, ae2, pt, false, orientation_check_required);
							}
							break;
						case ClipType.Xor:
							AddLocalMinPoly(ae1, ae2, pt, false, orientation_check_required);
							break;
						default:
							break;  // delphi2cpp translation note: no warnings
					}
			}
		}

		private void DeleteFromAEL(Active ae)
		{
			Active prev = ae.prev_in_ael;
			Active next = ae.next_in_ael;
			if (prev == null && next == null && (ae != _actives)) return;  //already deleted
			if (prev != null)
				prev.next_in_ael = next;
			else
				_actives = next;
			if (next != null) next.prev_in_ael = prev;
			//delete &ae;
		}

		private void AdjustCurrXAndCopyToSEL(long top_y)
		{
			Active ae = _actives;
			_sel = ae;
			while (ae != null)
			{
				ae.prev_in_sel = ae.prev_in_ael;
				ae.next_in_sel = ae.next_in_ael;
				ae.jump = ae.next_in_sel;
				ae.curr_x = TopX(ae, top_y);
				//nb: don't update ae.curr.Y yet (see AddNewIntersectNode)
				ae = ae.next_in_ael;
			}
		}

		protected void ExecuteInternal(ClipType ct, FillRule fillRule)
		{
			if (ct == ClipType.None) return;
			_fillrule = fillRule;
			_cliptype = ct;
			Reset();
			if (!PopScanline(out long y)) return;
			for (; ; )
			{
				InsertLocalMinimaIntoAEL(y);
				Active ae;
				while (PopHorz(out ae)) DoHorizontal(ae);
				_currentBotY = y;  //bottom of scanbeam
				if (!PopScanline(out y))
					break;  //y new top of scanbeam
				DoIntersections(y);
				DoTopOfScanbeam(y);
				while (PopHorz(out ae)) DoHorizontal(ae);
			}
		}

		public bool Execute(ClipType clipType, FillRule fillRule, 
			out Paths solution_closed, out Paths solution_open)
		{
			try
			{
				ExecuteInternal(clipType, fillRule);
				BuildPaths(out solution_closed, out solution_open);
			}
			catch {
				solution_closed = null;
				solution_open = null;
			}
			CleanUp();
			return (solution_closed != null || solution_open != null); 
		}

		public bool Execute(ClipType clipType, FillRule fillRule, out Paths solution_closed)
		{
			return Execute(clipType, fillRule, out solution_closed, out _);
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PolyTree polytree, out Paths openPaths)
		{
			polytree.Clear();
			bool success = false;
			try
			{
				ExecuteInternal(clipType, fillRule);
				success = BuildTree(polytree, out openPaths);
			}
			catch { openPaths = null; }
			CleanUp();
			return success;
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PolyTree polytree)
    {
			return Execute(clipType, fillRule, polytree, out _);
    }

		void DoIntersections(long top_y)
		{
			if (BuildIntersectList(top_y))
			{
				ProcessIntersectList();
				DisposeIntersectNodes();
			}
		}

		private void DisposeIntersectNodes()
		{
			_intersectList.Clear();
		}

		private void AddNewIntersectNode(Active ae1, Active ae2, long top_y)
		{
			Point64 pt = GetIntersectPoint(ae1, ae2);

			//rounding errors can occasionally place the calculated intersection
			//point either below or above the scanbeam, so check and correct ...
			if (pt.Y > _currentBotY)
			{
				//ae.curr.y is still the bottom of scanbeam
				pt.Y = _currentBotY;
				//use the more vertical of the 2 edges to derive pt.x ...
				if (Math.Abs(ae1.dx) < Math.Abs(ae2.dx))
					pt.X = TopX(ae1, _currentBotY);
				else
					pt.X = TopX(ae2, _currentBotY);
			}
			else if (pt.Y < top_y)
			{
				//top_y is at the top of the scanbeam
				pt.Y = top_y;
				if (ae1.top.Y == top_y)
					pt.X = ae1.top.X;
				else if (ae2.top.Y == top_y)
					pt.X = ae2.top.X;
				else if (Math.Abs(ae1.dx) < Math.Abs(ae2.dx))
					pt.X = ae1.curr_x;
				else
					pt.X = ae2.curr_x;
			}

			IntersectNode node = new IntersectNode(pt, ae1, ae2);
			_intersectList.Add(node);
		}

		private Active ExtractFromSEL(Active ae)
		{
			Active res = ae.next_in_sel;
			if (res != null)
				res.prev_in_sel = ae.prev_in_sel;
			if (ae.prev_in_sel != null)
				ae.prev_in_sel.next_in_sel = res;
			return res;
		}

		private void Insert1Before2InSEL(Active ae1, Active ae2)
		{
			ae1.prev_in_sel = ae2.prev_in_sel;
			if (ae1.prev_in_sel != null)
				ae1.prev_in_sel.next_in_sel = ae1;
			ae1.next_in_sel = ae2;
			ae2.prev_in_sel = ae1;
		}

		private bool BuildIntersectList(long top_y)
		{
			if (_actives == null || _actives.next_in_ael == null) return false;

			//Calculate edge positions at the top of the current scanbeam, and from this
			//we will determine the intersections required to reach these new positions.
			AdjustCurrXAndCopyToSEL(top_y);

			//Find all edge intersections in the current scanbeam using a stable merge
			//sort that ensures only adjacent edges are intersecting. Intersect info is
			//stored in FIntersectList ready to be processed in ProcessIntersectList.
			//Re merge sorts see https://stackoverflow.com/a/46319131/359538

			Active left = _sel, right, l_end, r_end, curr_base, prev_base, tmp;

			while (left.jump != null)
			{
				prev_base = null;
				while (left != null && left.jump != null)
				{
					curr_base = left;
					right = left.jump;
					l_end = right;
					r_end = right.jump;
					left.jump = r_end;
					while (left != l_end && right != r_end)
					{
						if (right.curr_x < left.curr_x)
						{
							tmp = right.prev_in_sel;
							for (; ; )
							{
								AddNewIntersectNode(tmp, right, top_y);
								if (tmp == left) break;
								tmp = tmp.prev_in_sel;
							}
							tmp = right;
							right = ExtractFromSEL(tmp);
							l_end = right;
							Insert1Before2InSEL(tmp, left);
							if (left == curr_base)
							{
								curr_base = tmp;
								curr_base.jump = r_end;
								if (prev_base == null) _sel = curr_base;
								else prev_base.jump = curr_base;
							}
						}
						else left = left.next_in_sel;
					}
					prev_base = curr_base;
					left = r_end;
				}
				left = _sel;
			}
			return _intersectList.Count > 0;
		}

		private void ProcessIntersectList()
		{
			//We now have a list of intersections required so that edges will be
			//correctly positioned at the top of the scanbeam. However, it's important
			//that edge intersections are processed from the bottom up, but it's also
			//crucial that intersections only occur between adjacent edges.

			//First we do a quicksort so intersections proceed in a bottom up order ...
			_intersectList.Sort(new IntersectListSort());

			//Now as we process these intersections, we must sometimes adjust the order
			//to ensure that intersecting edges are always adjacent ...
			for (int i = 0; i < _intersectList.Count; ++i)
			{
				if (!EdgesAdjacentInAEL(_intersectList[i]))
				{
					int j = i + 1;
					while (j < _intersectList.Count && !EdgesAdjacentInAEL(_intersectList[j])) j++;
					if (j < _intersectList.Count)
					{
						IntersectNode n = _intersectList[i];
						_intersectList[i] = _intersectList[j];
						_intersectList[j] = n;
					}
				}

				//Occasionally a non-minima intersection is processed before its own
				//minima. This causes problems with orientation so we need to flag it ...
				IntersectNode node = _intersectList[i];
				bool flagged = (i < _intersectList.Count - 1) && (_intersectList[i + 1].pt.Y > node.pt.Y);
				IntersectEdges(node.edge1, node.edge2, node.pt, flagged);
				SwapPositionsInAEL(node.edge1, node.edge2);
			}
		}

		private void SwapPositionsInAEL(Active ae1, Active ae2)
		{
			//preconditon: ae1 must be immediately to the left of ae2
			Active next = ae2.next_in_ael;
			if (next != null) next.prev_in_ael = ae1;
			Active prev = ae1.prev_in_ael;
			if (prev != null) prev.next_in_ael = ae2;
			ae2.prev_in_ael = prev;
			ae2.next_in_ael = ae1;
			ae1.prev_in_ael = ae2;
			ae1.next_in_ael = next;
			if (ae2.prev_in_ael == null) _actives = ae2;
		}

		private bool ResetHorzDirection(Active horz, Active max_pair, out long horz_left, out long horz_right)
		{
			if (horz.bot.X == horz.top.X)
			{
				//the horizontal edge is going nowhere ...
				horz_left = horz.curr_x;
				horz_right = horz.curr_x;
				Active ae = horz.next_in_ael;
				while (ae != null && ae != max_pair) ae = ae.next_in_ael;
				return ae != null;
			}
			else if (horz.curr_x < horz.top.X)
			{
				horz_left = horz.curr_x;
				horz_right = horz.top.X;
				return true;
			}
			else
			{
				horz_left = horz.top.X;
				horz_right = horz.curr_x;
				return false;  //right to left
			}
		}

		private void DoHorizontal(Active horz)
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
			Point64 pt;
			//with closed paths, simplify consecutive horizontals into a 'single' edge ...
			bool horzIsOpen = IsOpen(horz);
			if (!horzIsOpen)
			{
				pt = horz.bot;
				while (!IsMaxima(horz) && NextVertex(horz).pt.Y == pt.Y)
					UpdateEdgeIntoAEL(horz);
				horz.bot = pt;
				horz.curr_x = pt.X;
				//update Dx in case of direction change ...
				if (horz.bot.X < horz.top.X)
					horz.dx = double.NegativeInfinity;
				else
					horz.dx = double.PositiveInfinity;
			}

			Active max_pair = null;
			bool is_max = IsMaxima(horz);
			if (is_max && !IsOpenEnd(horz))
				max_pair = GetMaximaPair(horz);

			bool is_left_to_right = ResetHorzDirection(horz, max_pair, out long horz_left, out long horz_right);

			if (IsHotEdge(horz))
				AddOutPt(horz, new Point64(horz.curr_x, horz.bot.Y));

			for (; ; )
			{  //loops through consec. horizontal edges (if open)

				Active ae;
				if (is_left_to_right) ae = horz.next_in_ael;
				else ae = horz.prev_in_ael;

				while (ae != null)
				{

					if (ae == max_pair)
					{
						if (IsHotEdge(horz))
						{
							if (is_left_to_right) AddLocalMaxPoly(horz, ae, horz.top);
							else AddLocalMaxPoly(ae, horz, horz.top);
						}
						DeleteFromAEL(ae);
						DeleteFromAEL(horz);
						return;
					}
					//if horzEdge is a maxima, keep going until we reach
					//its maxima pair, otherwise check for break conditions
					if (!is_max || IsOpenEnd(horz))
					{
						//otherwise stop when 'ae' is beyond the end of the horizontal line
						if ((is_left_to_right && ae.curr_x > horz_right) ||
						 (!is_left_to_right && ae.curr_x < horz_left)) break;

						if (ae.curr_x == horz.top.X && !IsHorizontal(ae))
						{
							//for edges at the end or horzEdge, keep going until horzEdge's
							//outslope is greater than ae's slope when heading right or until
							//horzEdge's outslope is less than ae's slope when heading left.
							pt = NextVertex(horz).pt;
							if ((is_left_to_right && TopX(ae, pt.Y) >= pt.X) ||
								(!is_left_to_right && TopX(ae, pt.Y) <= pt.X)) break;
						}
					} //!is_max

					pt = new Point64(ae.curr_x, horz.bot.Y);
					if (is_left_to_right)
					{
						IntersectEdges(horz, ae, pt);
						SwapPositionsInAEL(horz, ae);
						ae = horz.next_in_ael;
					}
					else
					{
						IntersectEdges(ae, horz, pt);
						SwapPositionsInAEL(ae, horz);
						ae = horz.prev_in_ael;
					}
				}

				//check if we've finished looping through consecutive horizontals
				if (is_max || NextVertex(horz).pt.Y != horz.top.Y) break;

				//this must be an open path with another horizontal

				UpdateEdgeIntoAEL(horz);
				is_left_to_right = ResetHorzDirection(horz, max_pair, out horz_left, out horz_right);
				is_max = IsMaxima(horz);

				if (is_max) max_pair = GetMaximaPair(horz);
				if (IsHotEdge(horz)) AddOutPt(horz, horz.bot);
			}

			if (IsHotEdge(horz)) AddOutPt(horz, horz.top);

			if (!IsOpen(horz)) UpdateEdgeIntoAEL(horz);  //this is the } of an intermediate horiz.
			else if (!is_max) UpdateEdgeIntoAEL(horz);
			else if (max_pair == null) DeleteFromAEL(horz); //ie open at top
			else if (IsHotEdge(horz)) AddLocalMaxPoly(horz, max_pair, horz.top);
			else
			{
				DeleteFromAEL(max_pair);
				DeleteFromAEL(horz);
			}
		}

		private void DoTopOfScanbeam(long y)
		{
			_sel = null;  // sel_ is reused to flag horizontals (see PushHorz below)
			Active ae = _actives;
			while (ae != null)
			{
				//nb: 'ae' will never be horizontal here
				if (ae.top.Y == y)
				{
					//the following helps to avoid micro self-intersections
					//with negligible impact on performance ...
					ae.curr_x = ae.top.X;
					if (ae.prev_in_ael != null && (ae.prev_in_ael.curr_x == ae.curr_x) &&
							(ae.prev_in_ael.bot.Y != y) && IsHotEdge(ae.prev_in_ael))
						AddOutPt(ae.prev_in_ael, ae.top);
					if (ae.next_in_ael != null && (ae.next_in_ael.curr_x == ae.curr_x) &&
							(ae.next_in_ael.top.Y != y) && IsHotEdge(ae.next_in_ael))
						AddOutPt(ae.next_in_ael, ae.top);

					if (IsMaxima(ae))
					{
						ae = DoMaxima(ae);  //TOP OF BOUND (MAXIMA)
						continue;
					}
					else
					{
						//INTERMEDIATE VERTEX ...
						UpdateEdgeIntoAEL(ae);
						if (IsHotEdge(ae))
							AddOutPt(ae, ae.bot);
						if (IsHorizontal(ae))
							PushHorz(ae);  //horizontals are processed later
					}
				}
				ae = ae.next_in_ael;
			}
		}

		private Active DoMaxima(Active ae)
		{
			Active next_e, prev_e, max_pair;
			prev_e = ae.prev_in_ael;
			next_e = ae.next_in_ael;
			if (IsOpenEnd(ae))
			{
				if (IsHotEdge(ae))
					AddOutPt(ae, ae.top);
				if (!IsHorizontal(ae))
				{
					if (IsHotEdge(ae)) ae.outrec = null;
					DeleteFromAEL(ae);
				}
				return next_e;
			}
			else
			{
				max_pair = GetMaximaPair(ae);
				if (max_pair == null) return next_e;  //eMaxPair is horizontal
			}

			//only non-horizontal maxima here.
			//process any edges between maxima pair ...
			while (next_e != max_pair)
			{
				IntersectEdges(ae, next_e, ae.top);
				SwapPositionsInAEL(ae, next_e);
				next_e = ae.next_in_ael;
			}

			if (IsOpen(ae))
			{
				if (IsHotEdge(ae))
				{
					if (max_pair != null)
						AddLocalMaxPoly(ae, max_pair, ae.top);
					else
						AddOutPt(ae, ae.top);
				}
				if (max_pair != null) DeleteFromAEL(max_pair);
				DeleteFromAEL(ae);
				return (prev_e != null ? prev_e.next_in_ael : _actives);
			}
			//here ae.next_in_ael == ENext == EMaxPair ...
			if (IsHotEdge(ae))
				AddLocalMaxPoly(ae, max_pair, ae.top);

			DeleteFromAEL(ae);
			DeleteFromAEL(max_pair);
			return (prev_e != null ? prev_e.next_in_ael : _actives);
		}

		private static void CleanPath(Path path)
		{
			Point64 prev;
			int cnt = path.Count;
			while (cnt > 2 && InternalClipperFunc.CrossProduct(path[cnt - 2], path[cnt - 1], path[0]) == 0) cnt--;
			if (cnt < path.Count) path.RemoveRange(cnt, path.Count - cnt);
			if (cnt < 2) return;
			prev = path[cnt - 1];
			int j = 0;
			for (int i = 1; i < cnt; i++)
			{
				if (InternalClipperFunc.CrossProduct(prev, path[i - 1], path[i]) == 0) continue;
				prev = path[i - 1];
				path[j++] = prev;
			}
			path[j++] = path[cnt - 1];
			if (j < path.Count) path.RemoveRange(j, path.Count - j);
		}

		private static int SelfIntersectIdx(Path path)
		{
			int cnt = path.Count;
			if (cnt < 4) return -1;
			for (int i = 0; i < cnt; i++)
				if (InternalClipperFunc.SegmentsIntersect(path[i], path[(i + 1) % cnt],
					path[(i + 2) % cnt], path[(i + 3) % cnt]))
					return i;
			return -1;
		}

		private static bool InternalSplitSelfIntersect(Path path, int idx, out Path path1, out Path path2)
		{
			path1 = null;
			path2 = null;
			int cnt = path.Count;
			if (idx < 0 || idx >= cnt) return false;
			if (!InternalClipperFunc.IntersectPoint(path[idx], path[(idx + 1) % cnt],
				path[(idx + 2) % cnt], path[(idx + 3) % cnt], out PointD ip)) return false;
			path2 = new Path { path[(idx + 1) % cnt], path[(idx + 2) % cnt], new Point64(ip.x, ip.y) };
			path1 = new Path(cnt - 1);
			for (int i = 0; i < cnt - 2; i++)
				path1.Add(path[(idx + 3 + i) % cnt]);
			path1.Add(new Point64(ip.x, ip.y));
			return true;
		}

		private bool SplitSelfIntersect(Path path, out Paths extras)
		{
			bool has_spit = false;
			extras = new Paths();
			double a1, a2;
			int j = SelfIntersectIdx(path);
			while (j >= 0 && InternalSplitSelfIntersect(path, j, out Path p, out Path extra))
			{
				has_spit = true;
				a1 = ClipperFunc.Area(p);
				a2 = ClipperFunc.Area(extra);
				if (Math.Abs(a1) < Math.Abs(a2))
				{
					path = extra;
					extra = p;
				}
				else
				{
					path = p;
				}
				if ((a1 != 0.0 && a2 != 0.0) &&
				 ((a1 > 0) == (a2 > 0))) extras.Add(extra);
				j = SelfIntersectIdx(path);
			}
			return has_spit;
		}

		internal bool BuildPath(OutPt op, bool is_open, out Path path, out Paths extras)
		{
			path = null; extras = null;
			int cnt = PointCount(op);
			if (cnt < 2) return false;
			path = new Path(cnt);
			Point64 last_pt = op.pt;
			path.Add(last_pt);
			op = op.next;
			for (int i = 1; i < cnt; i++)
			{
				if (op.pt != last_pt)
				{
					last_pt = op.pt;
					path.Add(last_pt);
				}
				op = op.next;
			}

			if (is_open) return path.Count > 1;

			CleanPath(path);
			SplitSelfIntersect(path, out extras);
			return path.Count > 2;
		}

		protected bool BuildPaths(out Paths solution_closed, out Paths solution_open)
		{
			bool built = false;
			solution_closed = new Paths();
			solution_open = new Paths();
			Path path;
			Paths extras;
			try
			{
				solution_closed.Capacity = _outrecList.Count;
				solution_open.Capacity = _outrecList.Count;

				for (int j = 0; j < _outrecList.Count; j++)
				{
					OutRec outrec = _outrecList[j];
					if (outrec.pts == null) continue;

					if (outrec.state == OutRecState.Open)
					{
						if (BuildPath(outrec.pts.next, true, out path, out extras))
							solution_open.Add(path);
					}
					else
					{
						if (BuildPath(outrec.pts.next, false, out path, out extras))
							solution_closed.Add(path);
						if (extras != null)
							for (int i = 0; i < extras.Count; i++)
								solution_closed.Add(extras[i]);
					}
				}
				built = true;
			}
			catch { }
			return built;
		}

		protected bool BuildTree(PolyPathBase polytree, out Paths solution_open)
		{
			polytree.Clear();
			solution_open = new Paths(_outrecList.Count);
			try
			{
				for (int i = 0; i < _outrecList.Count; i++)
				{
					OutRec outrec = _outrecList[i];

					//make sure outer/owner paths preceed their inner paths ...
					if (outrec.owner != null && outrec.owner.idx > outrec.idx) { 
						int j = outrec.owner.idx;
						outrec.idx = j;
						_outrecList[i] = _outrecList[j];
						_outrecList[j] = outrec;
						outrec = _outrecList[i];
						outrec.idx = i;
					}

					if (outrec.pts == null) continue;
					bool isOpenPath = outrec.state == OutRecState.Open;

					if (!BuildPath(outrec.pts.next, isOpenPath, out Path path, out Paths extras)) continue;
					
					if (isOpenPath) { 
						solution_open.Add(path);
						continue;
					}

					//update ownership ...
					while (outrec.owner != null && outrec.owner.pts == null)
						outrec.owner = outrec.owner.owner;
					if (outrec.owner != null && outrec.owner.state == outrec.state) {
						if (IsOuter(outrec)) outrec.owner = null;
						else outrec.owner = outrec.owner.owner;
					}

					PolyPathBase ownerPP;
					if (outrec.owner != null && outrec.owner.polypath != null)	
						ownerPP = outrec.owner.polypath;
					else
						ownerPP = polytree;

					outrec.polypath = ownerPP.AddChild(path);
					if (extras != null) 
					for (int j = 0; j <  extras.Count; j++) 
							ownerPP.AddChild(extras[j]);
				}
			}
			catch { return false; }
			return true;
		}

		public Rect64 GetBounds()
		{
			if (_vertexList.Count == 0) return new Rect64(0, 0, 0, 0);
			Rect64 bounds = new Rect64(long.MaxValue, long.MaxValue, long.MinValue, long.MinValue);

			for (int i = 0; i < _vertexList.Count; i++)
			{
				Vertex vStart = _vertexList[i], v = vStart;
				do
				{
					if (v.pt.X < bounds.left) bounds.left = v.pt.X;
					if (v.pt.X > bounds.right) bounds.right = v.pt.X;
					if (v.pt.Y < bounds.top) bounds.top = v.pt.Y;
					if (v.pt.Y > bounds.bottom) bounds.bottom = v.pt.Y;
					v = v.next;
				} while (v == vStart);
			}
			return bounds;
		}
	}

	public class ClipperD : Clipper
	{
		private const double defaultScale = 100.0;
		private readonly double _scale;
		public ClipperD(double scale = 0)
		{
			if (scale == 0) this._scale = defaultScale;
			else this._scale = scale;
		}

		public new void AddPath(Path _, PathType __, bool ___) =>
			throw new ClipperLibException("Error in ClipperD.AddPath - must use PathD parameter");
		public new void AddPaths(Paths _, PathType __, bool ___) =>
			throw new ClipperLibException("Error in ClipperD.AddPaths - must use PathsD parameter");
		public new void AddSubject(Path _, bool __) =>
			throw new ClipperLibException("Error in ClipperD.AddPaths - must use PathsD parameter");

		public void AddSubject(PathD path, bool is_open = false)
		{
			base.AddPath(ClipperFunc.ScalePath(path, _scale), PathType.Subject, is_open);
		}
		public void AddClip(PathD path)
		{
			base.AddPath(ClipperFunc.ScalePath(path, _scale), PathType.Clip, false);
		}

		public void AddSubject(PathsD paths, bool is_open = false)
		{
			base.AddPaths(ClipperFunc.ScalePaths(paths, _scale), PathType.Subject, is_open);
		}

		public void AddClip(PathsD paths)
		{
			base.AddPaths(ClipperFunc.ScalePaths(paths, _scale), PathType.Clip, false);
		}

		public new bool Execute(ClipType _, FillRule __, out Paths ___) =>
			throw new ClipperLibException("Error in ClipperD.Execute - must use PathsD parameter");

		public new bool Execute(ClipType _, FillRule __, out Paths ___, out Paths ____) =>
			throw new ClipperLibException("Error in ClipperD.Execute - must use PathsD parameter");

		public bool Execute(ClipType clipType, FillRule fillRule,
			out PathsD solution_closed, out PathsD solution_open)
		{
			double invScale = 1 / _scale;
			bool res = base.Execute(clipType, fillRule, out Paths sol_closed, out Paths sol_open);
			solution_closed = ClipperFunc.ScalePaths(sol_closed, invScale);
			solution_open = ClipperFunc.ScalePaths(sol_open, invScale);
			return res;
		}

		public bool Execute(ClipType clipType, FillRule fillRule, out PathsD solution_closed)
		{
			return Execute(clipType, fillRule, out solution_closed, out _);
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PolyTreeD polytree, out PathsD openPaths)
		{
			polytree.Clear();
			polytree._scale = _scale;
			double invScale = 1 / _scale;
			openPaths = null;
			Paths oPaths = null;
			bool success = false;
			try
			{
				ExecuteInternal(clipType, fillRule);
				success = BuildTree(polytree, out oPaths);
			}
			catch { }
			CleanUp();
			if (oPaths != null)
				openPaths = ClipperFunc.ScalePaths(oPaths, invScale);
			return success;
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PolyTreeD polytree)
		{
			return Execute(clipType, fillRule, polytree, out _);
		}

	}

	public abstract class PolyPathBase
	{
		internal PolyPathBase _parent;
		internal List<PolyPathBase> _childs = new List<PolyPathBase>();
		public bool IsHole { get => GetIsHole(); }

		public PolyPathBase(PolyPathBase parent = null) { _parent = parent; }
		private bool GetIsHole()
		{
			bool result = true;
			PolyPathBase pp = _parent;
			while (pp != null)
			{
				result = !result;
				pp = pp._parent;
			}
			return result;
		}

		public int ChildCount { get => _childs.Count; }

		internal abstract PolyPathBase AddChild(Path p);
	
		public PolyPathBase GetChild(int idx)
		{
			if (idx < 0 || idx >= ChildCount) return null;
			else return _childs[idx];
		}

		public void Clear()
    {
			_childs.Clear();
    }
	}

	public class PolyPath : PolyPathBase
	{
    public Path Polygon { get; private set; }
		public PolyPath(PolyPathBase parent = null) : base(parent) { }
		internal override PolyPathBase AddChild(Path p)
		{
			PolyPathBase newChild = new PolyPath(this);
			(newChild as PolyPath).Polygon = p;
			_childs.Add(newChild);
			return newChild;
		}
	}

	public class PolyPathD : PolyPathBase
	{
		internal double _scale;
		public PathD Polygon { get; private set; }
		public PolyPathD(PolyPathBase parent = null) : base(parent) { }
		internal override PolyPathBase AddChild(Path p)
		{
			PolyPathBase newChild = new PolyPathD(this);
			(newChild as PolyPathD)._scale = _scale;
			(newChild as PolyPathD).Polygon = ClipperFunc.ScalePath(p, 1/_scale);
			_childs.Add(newChild);
			return newChild;
		}
	}

	public class PolyTree : PolyPath { }

	public class PolyTreeD : PolyPathD
	{
		public double Scale { get { return _scale; } }
	}

	public class ClipperLibException : Exception
	{
		public ClipperLibException(string description) : base(description) { }
	}

} //namespace