/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - also known as Clipper2                            *
* Date      :  19 March 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This is the main polygon clipping module                        *
* Thanks    :  Special thanks to Thong Nguyen (https://nguyen.mn/) and to      *
*              Guus Kuiper (https://www.guuskuiper.nl/) for their invaluable   *
*              assistance in this C# Clipper port.                             *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
#if DEBUG
using System.Diagnostics;
#endif
using System.Collections.Generic;

namespace Clipper2Lib
{

	using Path64 = List<Point64>;
	using Paths64 = List<List<Point64>>;
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
		public bool isOpen;
		public LocalMinima(Vertex vertex, PathType polytype, bool isOpen = false)
		{
			this.vertex = vertex;
			this.polytype = polytype;
			this.isOpen = isOpen;
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
		public Active frontEdge;
		public Active backEdge;
		public OutPt pts;
		public PolyPathBase polypath;
		public OutRecState state;
	};

	internal class Active
	{
		public Point64 bot;
		public Point64 top;
		public long curX;  //current (updated at every new scanline)
		public double dx;
		public int windDx;  //1 or -1 depending on winding direction
		public int windCount;
		public int windCount2;  //winding count of the opposite polytype
		public OutRec outrec;
		//AEL: 'active edge list' (Vatti's AET - active edge table)
		//     a linked list of all edges (from left to right) that are present
		//     (or 'active') within the current scanbeam (a horizontal 'beam' that
		//     sweeps from bottom to top over the paths in the clipping operation).
		public Active prevInAEL;
		public Active nextInAEL;
		//SEL: 'sorted edge list' (Vatti's ST - sorted table)
		//     linked list used when sorting edges into their new positions at the
		//     top of scanbeams, but also (re)used to process horizontals.
		public Active prevInSEL;
		public Active nextInSEL;
		public Active jump;
		public Vertex vertexTop;
		public LocalMinima localMin;  //the bottom of an edge 'bound' (also Vatti)
	};

	public class ClipperBase
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
		private int _currentLocMin;
		private long _currentBotY;
		protected bool _isSortedMinimaList;
		protected bool _hasOpenPaths;
		public bool PreserveCollinear { get; set; }

#if USINGZ
		public delegate void ZCallback64(Point64 bot1, Point64 top1,
				Point64 bot2, Point64 top2, ref Point64 intersectPt);
    public ZCallback64 ZFillFunc { get; set; }
#endif

		public ClipperBase()
		{
			_minimaList = new List<LocalMinima>();
			_intersectList = new List<IntersectNode>();
			_vertexList = new List<Vertex>();
			_outrecList = new List<OutRec>();
			_scanlineList = new List<long>();
			PreserveCollinear = true;
		}

#if USINGZ
		private bool XYCoordsEqual(Point64 pt1, Point64 pt2)
    {
			return (pt1.X == pt2.X && pt1.Y == pt2.Y);
    }

		private void SetZ(Active e1, Active e2, ref Point64 intersectPt)
		{
			if (ZFillFunc == null) return;
			
			//prioritize subject vertices over clip vertices
			//and pass the subject vertices before clip vertices in the callback
			Active e3, e4;
			if (GetPolyType(e1) == PathType.Subject) 
			{
				e3 = e1;
				e4 = e2;
			}
      else 
			{ 
				e3 = e2;
				e4 = e1;
			}
			if (XYCoordsEqual(intersectPt, e3.bot)) intersectPt.Z = e3.bot.Z;
			else if (XYCoordsEqual(intersectPt, e3.top)) intersectPt.Z = e3.top.Z;
			else if (XYCoordsEqual(intersectPt, e4.bot)) intersectPt.Z = e4.bot.Z;
			else if (XYCoordsEqual(intersectPt, e4.top)) intersectPt.Z = e4.top.Z;
			ZFillFunc(e3.bot, e3.top, e4.bot, e4.top, ref intersectPt);
		}
#endif

		static bool IsOdd(int val)
		{
			return ((val & 1) != 0);
		}

		private static bool IsHotEdge(Active ae)
		{
			return ae.outrec != null;
		}

		private static bool IsOpen(Active ae)
		{
			return ae.localMin.isOpen;
		}

		private static bool IsOpenEnd(Active ae)
		{
			return ae.localMin.isOpen && 
				(ae.vertexTop.flags & (VertexFlags.OpenStart | VertexFlags.OpenEnd)) != VertexFlags.None;
		}

		static Active GetPrevHotEdge(Active ae)
		{
			Active prev = ae.prevInAEL;
			while (prev != null && (IsOpen(prev) || !IsHotEdge(prev)))
				prev = prev.prevInAEL;
			return prev;
		}

		static bool IsOuter(OutRec outrec)
		{
			return (outrec.state == OutRecState.Outer);
		}

		static void SetAsOuter(OutRec outrec)
		{
			outrec.state = OutRecState.Outer;
		}

		static bool IsInner(OutRec outrec)
		{
			return (outrec.state == OutRecState.Inner);
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
				return (ae.windDx > 0);
			else
				return (ae == ae.outrec.frontEdge);
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
				return  (pt2.X - pt1.X) / dy;
			else if (pt2.X > pt1.X)
				return double.NegativeInfinity;
			else
				return double.PositiveInfinity;
		}

		static long TopX(Active ae, long currentY)
		{
#if DEBUG
			Debug.Assert(ae.dx != double.NegativeInfinity && ae.dx != double.PositiveInfinity);
#endif
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
			return ae.localMin.polytype;
		}

		static bool IsSamePolyType(Active ae1, Active ae2)
		{
			return ae1.localMin.polytype == ae2.localMin.polytype;
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
			return ae.windDx > 0;
		}

		static Vertex NextVertex(Active ae)
		{
			if (IsLeftBound(ae))
				return ae.vertexTop.next;
			else
				return ae.vertexTop.prev;
		}

		static Vertex PrevVertex(Active ae)
		{
			if (IsLeftBound(ae))
				return ae.vertexTop.prev; 
			else
				return ae.vertexTop.next;
		}

		static bool IsMaxima(Active ae)
		{
			return ((ae.vertexTop.flags & VertexFlags.LocalMax) != VertexFlags.None);
		}

		private Active GetMaximaPair(Active ae)
		{
			Active ae2;
			if (IsHorizontal(ae))
			{
				//we can't be sure whether the MaximaPair is on the left or right, so ...
				ae2 = ae.prevInAEL;
				while (ae2 != null && ae2.curX >= ae.top.X)
				{
					if (ae2.vertexTop == ae.vertexTop) return ae2;  //Found!
					ae2 = ae2.prevInAEL;
				}
				ae2 = ae.nextInAEL;
				while (ae2 != null && (TopX(ae2, ae.top.Y) <= ae.top.X))
				{
					if (ae2.vertexTop == ae.vertexTop) return ae2;  //Found!
					ae2 = ae2.nextInAEL;
				}
			}
			else
			{
				ae2 = ae.nextInAEL;
				while (ae2 != null)
				{
					if (ae2.vertexTop == ae.vertexTop) return ae2;  //Found!
					ae2 = ae2.nextInAEL;
				}
			}
			return null;
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

		static void SetSides(OutRec outrec, Active startEdge, Active endEdge)
		{
			outrec.frontEdge = startEdge;
			outrec.backEdge = endEdge;
		}

		static void SwapOutrecs(Active ae1, Active ae2)
		{
			OutRec or1 = ae1.outrec;
			OutRec or2 = ae2.outrec;
			if (or1 == or2)
			{
				Active ae = or1.frontEdge;
				or1.frontEdge = or1.backEdge;
				or1.backEdge = ae;
				return;
			}
			if (or1 != null)
			{
				if (ae1 == or1.frontEdge)
					or1.frontEdge = ae2;
				else
					or1.backEdge = ae2;
			}
			if (or2 != null)
			{
				if (ae2 == or2.frontEdge)
					or2.frontEdge = ae1;
				else
					or2.backEdge = ae1;
			}
			ae1.outrec = or2;
			ae2.outrec = or1;
		}

		static double Area(OutPt op)
		{
			if (op == null) return 0.0;
			double area = 0.0;
			OutPt op2 = op;
			do
			{
				area += (double)(op2.prev.pt.Y + op2.pt.Y) * (op2.prev.pt.X - op2.pt.X);
				op2 = op2.next;
			} while (op2 != op);
			return area * 0.5;
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

		private void CheckFixInnerOuter(Active ae)
		{
			bool wasOuter = IsOuter(ae.outrec);
			bool isOuter = true;

			Active ae2 = ae.prevInAEL;
			while (ae2 != null)
			{
				if (IsHotEdge(ae2) && !IsOpen(ae2)) isOuter = !isOuter;
				ae2 = ae2.prevInAEL;
			}

			if (isOuter != wasOuter)
			{
				if (isOuter)
					SetAsOuter(ae.outrec);
				else
					SetAsInner(ae.outrec);
			}

			ae2 = GetPrevHotEdge(ae);
			if (isOuter)
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
			if ((Area(ae.outrec.pts) < 0.0) == isOuter) 
				ReverseOutPts(ae.outrec.pts);
		}

		private static void SwapSides(OutRec outrec)
		{
			Active ae2 = outrec.frontEdge;
			outrec.frontEdge = outrec.backEdge;
			outrec.backEdge = ae2;
			outrec.pts = outrec.pts.next;
		}

		private bool FixSides(Active ae)
		{
			if (ValidatePath(ref ae.outrec.pts)) {
				CheckFixInnerOuter(ae);
				if (IsOuter(ae.outrec) == IsFront(ae)) 
					return false; // ie not swapping sides
			}
			SwapSides(ae.outrec);
			return true;
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
				ae2 = ae.nextInAEL;  //ie assess state from opposite direction
				while (ae2 != null && (!IsHotEdge(ae2) || IsOpen(ae2)))
					ae2 = ae2.nextInAEL;
				if (ae2 == null)
					outrec.owner = null;
				else if ((ae2.outrec.state == OutRecState.Outer) == (ae2.outrec.frontEdge == ae2))
					outrec.owner = ae2.outrec.owner;
				else
					outrec.owner = ae2.outrec;
			}
			else
			{
				ae2 = GetPrevHotEdge(ae);
				while (ae2 != null && (!IsHotEdge(ae2) || IsOpen(ae2)))
					ae2 = ae2.prevInAEL;
				if (ae2 == null)
					outrec.owner = null;
				else if (IsOuter(ae2.outrec) == (ae2.outrec.backEdge == ae2))
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
			return (inode.edge1.nextInAEL == inode.edge2) || (inode.edge1.prevInAEL == inode.edge2);
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

		private void AddLocMin(Vertex vert, PathType polytype, bool isOpen)
		{
			//make sure the vertex is added only once ...
			if ((vert.flags & VertexFlags.LocalMin) != VertexFlags.None) return;
			vert.flags |= VertexFlags.LocalMin;

			LocalMinima lm = new LocalMinima(vert, polytype, isOpen);
			_minimaList.Add(lm);
		}

		protected void AddPathToVertexList(Path64 path, PathType polytype, bool isOpen)
		{
			int pathCnt = path.Count;
			if (!isOpen)
			{
				while (pathCnt > 1 && (path[pathCnt - 1] == path[0])) --pathCnt;
				if (pathCnt < 2) return;
			}
			else if (pathCnt == 0) return;

			_vertexList.Capacity = _vertexList.Count + pathCnt;
			Vertex vCurr, vPrev, v0 = new Vertex(path[0], VertexFlags.None, null);
			_vertexList.Add(v0);
			vPrev = v0;
			for (int i = 1; i < pathCnt; i++)
			{
				vCurr = new Vertex(path[i], VertexFlags.None, vPrev);
				vPrev.next = vCurr;
				_vertexList.Add(vCurr);
				vPrev = vCurr;
			}
			v0.prev = vPrev;
			vPrev.next = v0;

			bool goingUp, goingUp0;
			if (isOpen)
			{
				vCurr = v0.next;
				while (vCurr != v0 && vCurr.pt.Y == v0.pt.Y) vCurr = vCurr.next;
				goingUp = vCurr.pt.Y <= v0.pt.Y;
				if (goingUp)
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
				goingUp = v0.pt.Y < vPrev.pt.Y;       //ie direction leading up to v0
			}
			else
				goingUp = v0.pt.Y < vPrev.pt.Y;   //ie direction leading up to v0

			goingUp0 = goingUp;
			vPrev = v0;
			vCurr = v0.next;
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

				if (goingUp && vCurr.pt.Y > vPrev.pt.Y)
				{
					vPrev.flags |= VertexFlags.LocalMax;
					goingUp = false;
				}
				else if (!goingUp && vCurr.pt.Y < vPrev.pt.Y)
				{
					AddLocMin(vPrev, polytype, isOpen);
					goingUp = true;
				}
				vPrev = vCurr;
				vCurr = vCurr.next;
			}
			//close the double-linked loop
			vPrev.next = v0;
			v0.prev = vPrev;

			if (isOpen)
			{
				vPrev.flags |= VertexFlags.OpenEnd;
				if (goingUp)
					vPrev.flags |= VertexFlags.LocalMax;
				else
					AddLocMin(vPrev, polytype, isOpen);
			}
			else if (goingUp != goingUp0)
			{
				if (goingUp0) AddLocMin(vPrev, polytype, isOpen);
				else vPrev.flags |= VertexFlags.LocalMax;
			}
		}

		public void AddSubject(Path64 path)
		{
			AddPath(path, PathType.Subject, false);
		}

		public void AddOpenSubject(Path64 path)
		{
			AddPath(path, PathType.Subject, true);
		}

		public void AddClip(Path64 path)
		{
			AddPath(path, PathType.Clip, false);
		}

		protected void AddPath(Path64 path, PathType polytype, bool isOpen = false)
		{
			if (isOpen) _hasOpenPaths = true;
			_isSortedMinimaList = false;
			AddPathToVertexList(path, polytype, isOpen);
		}

		protected void AddPaths(Paths64 paths, PathType polytype, bool isOpen = false)
		{
			if (isOpen) _hasOpenPaths = true;
			_isSortedMinimaList = false;
			for (int i = 0; i < paths.Count; i++)
				AddPathToVertexList(paths[i], polytype, isOpen);
		}

		private bool IsContributingClosed(Active ae)
		{
			switch (_fillrule)
			{
				case FillRule.NonZero:
					if (Math.Abs(ae.windCount) != 1) return false;
					break;
				case FillRule.Positive:
					if (ae.windCount != 1) return false;
					break;
				case FillRule.Negative:
					if (ae.windCount != -1) return false;
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
						case FillRule.NonZero: return (ae.windCount2 != 0);
						case FillRule.Positive: return (ae.windCount2 > 0);
						case FillRule.Negative: return (ae.windCount2 < 0);
					}
					break;
				case ClipType.Union:
					switch (_fillrule)
					{
						case FillRule.EvenOdd:
						case FillRule.NonZero: return (ae.windCount2 == 0);
						case FillRule.Positive: return (ae.windCount2 <= 0);
						case FillRule.Negative: return (ae.windCount2 >= 0);
					}
					break;
				case ClipType.Difference:
					if (GetPolyType(ae) == PathType.Subject)
						switch (_fillrule)
						{
							case FillRule.EvenOdd:
							case FillRule.NonZero: return (ae.windCount2 == 0);
							case FillRule.Positive: return (ae.windCount2 <= 0);
							case FillRule.Negative: return (ae.windCount2 >= 0);
						}
					else
						switch (_fillrule)
						{
							case FillRule.EvenOdd:
							case FillRule.NonZero: return (ae.windCount2 != 0);
							case FillRule.Positive: return (ae.windCount2 > 0);
							case FillRule.Negative: return (ae.windCount2 < 0);
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
				ClipType.Intersection => (ae.windCount2 != 0),
				ClipType.Union => (ae.windCount == 0 && ae.windCount2 == 0),
				ClipType.Difference => (ae.windCount2 == 0),
				ClipType.Xor => (ae.windCount != 0) != (ae.windCount2 != 0),
				_ => false,
			};
		}

		private void SetWindCountForClosedPathEdge(Active ae)
		{
			//Wind counts refer to polygon regions not edges, so here an edge's WindCnt
			//indicates the higher of the wind counts for the two regions touching the
			//edge. (nb: Adjacent regions can only ever have their wind counts differ by
			//one. Also, open paths have no meaningful wind directions or counts.)

			Active ae2 = ae.prevInAEL;
			//find the nearest closed path edge of the same PolyType in AEL (heading left)
			PathType pt = GetPolyType(ae);
			while (ae2 != null && (GetPolyType(ae2) != pt || IsOpen(ae2))) ae2 = ae2.prevInAEL;

			if (ae2 == null)
			{
				ae.windCount = ae.windDx;
				ae2 = _actives;
			}
			else if (_fillrule == FillRule.EvenOdd)
			{
				ae.windCount = ae.windDx;
				ae.windCount2 = ae2.windCount2;
				ae2 = ae2.nextInAEL;
			}
			else
			{
				//NonZero, positive, or negative filling here ...
				//if ae's WindCnt is in the SAME direction as its WindDx, then polygon
				//filling will be on the right of 'ae'.
				//nb: neither ae2.WindCnt nor ae2.WindDx should ever be 0.
				if (ae2.windCount * ae2.windDx < 0)
				{
					//opposite directions so 'ae' is outside 'ae2' ...
					if (Math.Abs(ae2.windCount) > 1)
					{
						//outside prev poly but still inside another.
						if (ae2.windDx * ae.windDx < 0)
							//reversing direction so use the same WC
							ae.windCount = ae2.windCount;
						else
							//otherwise keep 'reducing' the WC by 1 (ie towards 0) ...
							ae.windCount = ae2.windCount + ae.windDx;
					}
					else
						//now outside all polys of same polytype so set own WC ...
						ae.windCount = (IsOpen(ae) ? 1 : ae.windDx);
				}
				else
				{
					//'ae' must be inside 'ae2'
					if (ae2.windDx * ae.windDx < 0)
						//reversing direction so use the same WC
						ae.windCount = ae2.windCount;
					else
						//otherwise keep 'increasing' the WC by 1 (ie away from 0) ...
						ae.windCount = ae2.windCount + ae.windDx;
				}
				ae.windCount2 = ae2.windCount2;
				ae2 = ae2.nextInAEL;  //ie get ready to calc WindCnt2
			}

			//update windCount2 ...
			if (_fillrule == FillRule.EvenOdd)
				while (ae2 != ae)
				{
					if (GetPolyType(ae2) != pt && !IsOpen(ae2))
						ae.windCount2 = (ae.windCount2 == 0 ? 1 : 0);
					ae2 = ae2.nextInAEL;
				}
			else
				while (ae2 != ae)
				{
					if (GetPolyType(ae2) != pt && !IsOpen(ae2))
						ae.windCount2 += ae2.windDx;
					ae2 = ae2.nextInAEL;
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
					ae2 = ae2.nextInAEL;
				}
				ae.windCount = (IsOdd(cnt1) ? 1 : 0);
				ae.windCount2 = (IsOdd(cnt2) ? 1 : 0);
			}
			else
			{
				while (ae2 != ae)
				{
					if (GetPolyType(ae2) == PathType.Clip)
						ae.windCount2 += ae2.windDx;
					else if (!IsOpen(ae2))
						ae.windCount += ae2.windDx;
					ae2 = ae2.nextInAEL;
				}
			}
		}

		private bool IsValidAelOrder(Active a1, Active a2)
		{
			if (a2.curX != a1.curX)
				return a2.curX > a1.curX;

			Point64 pt;
			if (a1.bot.Y < a2.bot.Y)
				pt = a1.bot; else
				pt = a2.bot;

			double d = InternalClipperFunc.CrossProduct(a1.top, pt, a2.top);
			if (d < 0) return true;
			else if (d > 0) return false;

			//edges must be collinear to get here

			//for starting open paths, place them according to
			//the direction they're about to turn
			if (IsOpen(a1) && !IsMaxima(a1) && (a1.bot.Y <= a2.bot.Y) &&
				!IsSamePolyType(a1, a2) && (a1.top.Y > a2.top.Y))
					return InternalClipperFunc.CrossProduct(
						a1.bot, a1.top, NextVertex(a1).pt) <= 0;
			else if (IsOpen(a2) && !IsMaxima(a2) && (a2.bot.Y <= a1.bot.Y) && 
				!IsSamePolyType(a1, a2) && (a2.top.Y > a1.top.Y))	
					return InternalClipperFunc.CrossProduct(
						a2.bot, a2.top, NextVertex(a2).pt) >= 0;

			bool a1IsNewEdge = !IsOpen(a1) && 
				(a1.bot.Y <= a2.bot.Y) && PrevVertex(a1) == a1.localMin.vertex;
			bool a2IsNewEdge = !IsOpen(a2) && 
				(a2.bot.Y <= a1.bot.Y) && PrevVertex(a2) == a2.localMin.vertex;

			if (IsHorizontal(a1))
			{
				if (IsHorizontal(a2)) return a1.top.X < a2.top.X;
				else return a1.top.X < a1.bot.X;
			}
			else if (IsHorizontal(a2)) return a2.top.X > a2.bot.X;
			//New *left edges* prefer placement to the right of collinear edges to
			//preserve proximity with their corresponding *right edges*. *Right edges*
			//likewise prefer left placement. With open edges and collinearity,
			//placement is undefined.
			else if (a2IsNewEdge) return IsLeftBound(a2);
			else if (a1IsNewEdge) return !IsLeftBound(a1);
			else return true; //open edge and undefined, let's stop here
		}

		private void InsertLeftEdge(Active ae)
		{
			Active ae2;

			if (_actives == null)
			{
				ae.prevInAEL = null;
				ae.nextInAEL = null;
				_actives = ae;
			}
			else if (IsValidAelOrder(ae, _actives))
			{
				ae.prevInAEL = null;
				ae.nextInAEL = _actives;
				_actives.prevInAEL = ae;
				_actives = ae;
			}
			else
			{
				ae2 = _actives;
				while (ae2.nextInAEL != null && IsValidAelOrder(ae2.nextInAEL, ae))
					ae2 = ae2.nextInAEL;
				ae.nextInAEL = ae2.nextInAEL;
				if (ae2.nextInAEL != null) ae2.nextInAEL.prevInAEL = ae;
				ae.prevInAEL = ae2;
				ae2.nextInAEL = ae;
			}
		}

		private void InsertRightEdge(Active ae, Active ae2)
		{
			ae2.nextInAEL = ae.nextInAEL;
			if (ae.nextInAEL != null) ae.nextInAEL.prevInAEL = ae2;
			ae2.prevInAEL = ae;
			ae.nextInAEL = ae2;
		}

		private void InsertLocalMinimaIntoAEL(long botY)
		{
			LocalMinima localMinima;
			Active leftBound, rightBound;
			//Add any local minima (if any) at BotY ...
			//nb: horizontal local minima edges should contain locMin.vertex.prev
			while (HasLocMinAtY(botY))
			{
				localMinima = PopLocalMinima();
				if ((localMinima.vertex.flags & VertexFlags.OpenStart) != VertexFlags.None)
				{
					leftBound = null;
				}
				else
				{
					leftBound = new Active
					{
						bot = localMinima.vertex.pt,
						curX = localMinima.vertex.pt.X,
						vertexTop = localMinima.vertex.prev,  //ie descending
						top = localMinima.vertex.prev.pt,
						windDx = -1,
						outrec = null,
						localMin = localMinima
					};
					SetDx(leftBound);
				}

				if ((localMinima.vertex.flags & VertexFlags.OpenEnd) != VertexFlags.None)
				{
					rightBound = null;
				}
				else
				{
					rightBound = new Active
					{
						bot = localMinima.vertex.pt,
						curX = localMinima.vertex.pt.X,
						vertexTop = localMinima.vertex.next,  //ie ascending
						top = localMinima.vertex.next.pt,
						windDx = 1,
						outrec = null,
						localMin = localMinima
					};
					SetDx(rightBound);
				}

				//Currently LeftB is just the descending bound and RightB is the ascending.
				//Now if the LeftB isn't on the left of RightB then we need swap them.
				if (leftBound != null && rightBound != null)
				{
					if (IsHorizontal(leftBound))
					{
						if (IsHeadingRightHorz(leftBound)) SwapActives(ref leftBound, ref rightBound);
					}
					else if (IsHorizontal(rightBound))
					{
						if (IsHeadingLeftHorz(rightBound)) SwapActives(ref leftBound, ref rightBound);
					}
					else if (leftBound.dx < rightBound.dx)
						SwapActives(ref leftBound, ref rightBound);
				}
				else if (leftBound == null)
				{
					leftBound = rightBound;
					rightBound = null;
				}

				bool contributing;
				InsertLeftEdge(leftBound);

				if (IsOpen(leftBound))
				{
					SetWindCountForOpenPathEdge(leftBound);
					contributing = IsContributingOpen(leftBound);
				}
				else
				{
					SetWindCountForClosedPathEdge(leftBound);
					contributing = IsContributingClosed(leftBound);
				}

				if (rightBound != null)
				{
					rightBound.windCount = leftBound.windCount;
					rightBound.windCount2 = leftBound.windCount2;
					InsertRightEdge(leftBound, rightBound);  ///////
					if (contributing)
						AddLocalMinPoly(leftBound, rightBound, leftBound.bot, true);
					if (IsHorizontal(rightBound))
						PushHorz(rightBound);
					else
						InsertScanline(rightBound.top.Y);
				}
				else if (contributing)
					StartOpenPath(leftBound, leftBound.bot);

				if (IsHorizontal(leftBound))
					PushHorz(leftBound);
				else
					InsertScanline(leftBound.top.Y);

			}  //while (HasLocMinAtY())
		}

		private void PushHorz(Active ae)
		{
			ae.nextInSEL = _sel;
			_sel = ae;
		}

		private bool PopHorz(out Active ae)
		{
			ae = _sel;
			if (ae == null) return false;
			_sel = _sel.nextInSEL;
			return true;
		}

		private OutPt AddLocalMinPoly(Active ae1, Active ae2, Point64 pt, bool isNew = false)
		{

			OutRec outrec = new OutRec();
			_outrecList.Add(outrec);
			outrec.idx = _outrecList.Count - 1;
			outrec.pts = null;
			outrec.polypath = null;

			ae1.outrec = outrec;
			SetOwnerAndInnerOuterState(ae1);
			//flag when orientation needs to be rechecked later ...
			ae2.outrec = outrec;

			if (!IsOpen(ae1))
			{
				//Setting the owner and inner/outer states (above) is an essential
				//precursor to setting edge 'sides' (ie left and right sides of output
				//polygons) and hence the orientation of output paths ...
				if (IsOuter(outrec) == isNew)
					SetSides(outrec, ae1, ae2);
				else
					SetSides(outrec, ae2, ae1);
			}
			OutPt op = new OutPt(pt);
			outrec.pts = op;
			return op;
		}

		private OutPt AddLocalMaxPoly(Active ae1, Active ae2, Point64 pt)
		{
			if (!IsOpen(ae1) && (IsFront(ae1) == IsFront(ae2)))
				if (!FixSides(ae1)) FixSides(ae2);
			OutPt op = AddOutPt(ae1, pt);
			if (ae1.outrec == ae2.outrec)
			{
				OutRec outrec = ae1.outrec;
				ae1.outrec.frontEdge = null;
				ae1.outrec.backEdge = null;
				ae1.outrec = null;
				ae2.outrec = null;
				if (IsOpen(ae1)) return op;

				CleanCollinear(ref outrec.pts, PreserveCollinear);
				if (outrec.pts != null) 
					FixSelfIntersects(ref outrec.pts);

				OutPt result = outrec.pts;
				if ((result != null) && (IsOuter(outrec) != (Area(result) > 0))) 
					ReverseOutPts(outrec.pts);
			}
			//and to preserve the winding orientation of outrec ...
			else if (ae1.outrec.idx < ae2.outrec.idx)
				JoinOutrecPaths(ae1, ae2);
			else
				JoinOutrecPaths(ae2, ae1);
			return op;
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
			OutPt p1Start = ae1.outrec.pts;
			OutPt p2Start = ae2.outrec.pts;
			OutPt p1End = p1Start.next;
			OutPt p2End = p2Start.next;
			if (IsFront(ae1))
			{
				p2End.prev = p1Start;
				p1Start.next = p2End;
				p2Start.next = p1End;
				p1End.prev = p2Start;
				ae1.outrec.pts = p2Start;
				if (IsOpen(ae1))
				{
					ae1.outrec.pts = p2Start;
				}
				else
				{
					ae1.outrec.frontEdge = ae2.outrec.frontEdge;
					ae1.outrec.frontEdge.outrec = ae1.outrec;
				}
				//strip duplicates ...
				if ((p2End != p2Start) && (p2End.pt == p2End.prev.pt))
					DeleteOp(p2End);
			}
			else
			{
				p1End.prev = p2Start;
				p2Start.next = p1End;
				p1Start.next = p2End;
				p2End.prev = p1Start;
				if (IsOpen(ae1))
				{
					ae1.outrec.pts = p1Start;
				}
				else
				{
					ae1.outrec.backEdge = ae2.outrec.backEdge;
					ae1.outrec.backEdge.outrec = ae1.outrec;
				}
				//strip duplicates ...
				if ((p1End != p1Start) && (p1End.pt == p1End.prev.pt))
					DeleteOp(p1End);
			}

			if ((ae1.outrec.pts.pt == ae1.outrec.pts.prev.pt) && !IsInvalidPath(ae1.outrec.pts))
				DeleteOp(ae1.outrec.pts.prev);

			//after joining, the ae2.OutRec must contains no vertices ...
			ae2.outrec.frontEdge = null;
			ae2.outrec.backEdge = null;
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
			OutPt newOp;

			//Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
			//opFront[.Prev]* ~~~> opBack & opBack == opFront.Next
			OutRec outrec = ae.outrec;
			bool toFront = IsFront(ae);
			OutPt opFront = outrec.pts;
			OutPt opBack = opFront.next;

			if (toFront && (pt == opFront.pt)) newOp = opFront;
			else if (!toFront && (pt == opBack.pt)) newOp = opBack;
			else
			{
				newOp = new OutPt(pt);
				opBack.prev = newOp;
				newOp.prev = opFront;
				newOp.next = opBack;
				opFront.next = newOp;
				if (toFront) outrec.pts = newOp;
			}
			return newOp;
		}

		private OutPt StartOpenPath(Active ae, Point64 pt)
		{

			OutRec outrec = new OutRec();
			_outrecList.Add(outrec);
			outrec.idx = _outrecList.Count - 1;
			outrec.owner = null;
			outrec.state = OutRecState.Open;
			outrec.pts = null;
			//outrec.PolyTree = null;
			outrec.backEdge = null;
			outrec.frontEdge = null;

			ae.outrec = outrec;

			OutPt op = new OutPt(pt);
			outrec.pts = op;
			return op;
		}

		private void UpdateEdgeIntoAEL(Active ae)
		{
			ae.bot = ae.top;
			ae.vertexTop = NextVertex(ae);
			ae.top = ae.vertexTop.pt;
			ae.curX = ae.bot.X;
			SetDx(ae);
			if (!IsHorizontal(ae)) InsertScanline(ae.top.Y);
		}

		private void IntersectEdges(Active ae1, Active ae2, Point64 pt)
		{
			//MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
			if (_hasOpenPaths && (IsOpen(ae1) || IsOpen(ae2)))
			{
				if (IsOpen(ae1) && IsOpen(ae2)) return;
				if (IsOpen(ae2)) 
						SwapActives(ref ae1, ref ae2);
				switch (_cliptype)
				{
					case ClipType.Intersection:
					case ClipType.Difference:
						if (IsSamePolyType(ae1, ae2) || (Math.Abs(ae2.windCount) != 1)) return;
						break;
					case ClipType.Union:
						if (IsHotEdge(ae1) != ((Math.Abs(ae2.windCount) != 1) || 
							(IsHotEdge(ae1) != (ae2.windCount != 0)))) return;  //just works!
						break;
					case ClipType.Xor:
						if (Math.Abs(ae2.windCount) != 1) return;
						break;
					case ClipType.None:
						throw new ClipperLibException("Error in IntersectEdges - ClipType is None!");
				}
				//toggle contribution ...
				if (IsHotEdge(ae1))
				{
					OutPt op = AddOutPt(ae1, pt);
#if USINGZ
					SetZ(ae1, ae2, ref op.pt);
#endif
					ae1.outrec = null;
				}
				else
				{
					OutPt op =  StartOpenPath(ae1, pt);
#if USINGZ
					SetZ(ae1, ae2, ref op.pt);
#endif
				}
				return;
			}

			//UPDATE WINDING COUNTS...

			int oldE1WindCount, oldE2WindCount;
			if (ae1.localMin.polytype == ae2.localMin.polytype)
			{
				if (_fillrule == FillRule.EvenOdd)
				{
					oldE1WindCount = ae1.windCount;
					ae1.windCount = ae2.windCount;
					ae2.windCount = oldE1WindCount;
				}
				else
				{
					if (ae1.windCount + ae2.windDx == 0)
						ae1.windCount = -ae1.windCount;
					else
						ae1.windCount += ae2.windDx;
					if (ae2.windCount - ae1.windDx == 0)
						ae2.windCount = -ae2.windCount;
					else
						ae2.windCount -= ae1.windDx;
				}
			}
			else
			{
				if (_fillrule != FillRule.EvenOdd)
					ae1.windCount2 += ae2.windDx;
				else
					ae1.windCount2 = (ae1.windCount2 == 0 ? 1 : 0);
				if (_fillrule != FillRule.EvenOdd)
					ae2.windCount2 -= ae1.windDx;
				else
					ae2.windCount2 = (ae2.windCount2 == 0 ? 1 : 0);
			}

			switch (_fillrule)
			{
				case FillRule.Positive:
					oldE1WindCount = ae1.windCount;
					oldE2WindCount = ae2.windCount;
					break;
				case FillRule.Negative:
					oldE1WindCount = -ae1.windCount;
					oldE2WindCount = -ae2.windCount;
					break;
				default:
					oldE1WindCount = Math.Abs(ae1.windCount);
					oldE2WindCount = Math.Abs(ae2.windCount);
					break;
			}

			bool e1WindCountIs0or1 = oldE1WindCount == 0 || oldE1WindCount == 1;
			bool e2WindCountIs0or1 = oldE2WindCount == 0 || oldE2WindCount == 1;

			if ((!IsHotEdge(ae1) && !e1WindCountIs0or1) || (!IsHotEdge(ae2) && !e2WindCountIs0or1)) return;
			
			//NOW PROCESS THE INTERSECTION ...

			//if both edges are 'hot' ...
			if (IsHotEdge(ae1) && IsHotEdge(ae2))
			{
				if ((oldE1WindCount != 0 && oldE1WindCount != 1) || (oldE2WindCount != 0 && oldE2WindCount != 1) ||
					(ae1.localMin.polytype != ae2.localMin.polytype && _cliptype != ClipType.Xor))
				{
					OutPt op = AddLocalMaxPoly(ae1, ae2, pt);
#if USINGZ
					SetZ(ae1, ae2, ref op.pt);
#endif
				}
				else if (IsFront(ae1) || (ae1.outrec == ae2.outrec))
				{
					OutPt op1 = AddLocalMaxPoly(ae1, ae2, pt);
					OutPt op2 = AddLocalMinPoly(ae1, ae2, pt);
#if USINGZ
					SetZ(ae1, ae2, ref op1.pt);
					SetZ(ae1, ae2, ref op2.pt);
#endif
				}
				else
				{
					//right & left bounds touching, NOT maxima & minima ...
					OutPt op1 = AddOutPt(ae1, pt);
					OutPt op2 = AddOutPt(ae2, pt);
#if USINGZ
					SetZ(ae1, ae2, ref op1.pt);
					SetZ(ae1, ae2, ref op2.pt);
#endif
					SwapOutrecs(ae1, ae2);
				}
			}
			//if one or other edge is 'hot' ...
			else if (IsHotEdge(ae1))
			{
				OutPt op = AddOutPt(ae1, pt);
#if USINGZ
				SetZ(ae1, ae2, ref op.pt);
#endif
				SwapOutrecs(ae1, ae2);
			}
			else if (IsHotEdge(ae2))
			{
				OutPt op = AddOutPt(ae2, pt);
#if USINGZ
				SetZ(ae1, ae2, ref op.pt);
#endif
				SwapOutrecs(ae1, ae2);
			}
			else
			{  //neither edge is 'hot'
				long e1Wc2, e2Wc2;
				switch (_fillrule)
				{
					case FillRule.Positive:
						e1Wc2 = ae1.windCount2;
						e2Wc2 = ae2.windCount2;
						break;
					case FillRule.Negative:
						e1Wc2 = -ae1.windCount2;
						e2Wc2 = -ae2.windCount2;
						break;
					default:
						e1Wc2 = Math.Abs(ae1.windCount2);
						e2Wc2 = Math.Abs(ae2.windCount2);
						break;
				}

				if (!IsSamePolyType(ae1, ae2))
				{
					OutPt op = AddLocalMinPoly(ae1, ae2, pt, false);
#if USINGZ
					SetZ(ae1, ae2, ref op.pt);
#endif
				}
				else if (oldE1WindCount == 1 && oldE2WindCount == 1)
				{
					OutPt op = null;
					switch (_cliptype)
					{
						case ClipType.Union:
							if (e1Wc2 > 0 && e2Wc2 > 0) return;
							op = AddLocalMinPoly(ae1, ae2, pt, false);
							break;

						case ClipType.Difference:
							if (((GetPolyType(ae1) == PathType.Clip) && (e1Wc2 > 0) && (e2Wc2 > 0)) ||
									((GetPolyType(ae1) == PathType.Subject) && (e1Wc2 <= 0) && (e2Wc2 <= 0)))
							{
								op = AddLocalMinPoly(ae1, ae2, pt, false);
							}
							break;

						case ClipType.Xor:
							op = AddLocalMinPoly(ae1, ae2, pt, false);
							break;

						default:  //ClipType.Intersection:
							if (e1Wc2 <= 0 || e2Wc2 <= 0) return;
							op = AddLocalMinPoly(ae1, ae2, pt, false);
							break;
					}
#if USINGZ
					if (op != null) SetZ(ae1, ae2, ref op.pt);
#endif

				}
			}
		}

		private void DeleteFromAEL(Active ae)
		{
			Active prev = ae.prevInAEL;
			Active next = ae.nextInAEL;
			if (prev == null && next == null && (ae != _actives)) return;  //already deleted
			if (prev != null)
				prev.nextInAEL = next;
			else
				_actives = next;
			if (next != null) next.prevInAEL = prev;
			//delete &ae;
		}

		private void AdjustCurrXAndCopyToSEL(long topY)
		{
			Active ae = _actives;
			_sel = ae;
			while (ae != null)
			{
				ae.prevInSEL = ae.prevInAEL;
				ae.nextInSEL = ae.nextInAEL;
				ae.jump = ae.nextInSEL;
				ae.curX = TopX(ae, topY);
				//nb: don't update ae.curr.Y yet (see AddNewIntersectNode)
				ae = ae.nextInAEL;
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

		void DoIntersections(long topY)
		{
			if (BuildIntersectList(topY))
			{
				ProcessIntersectList();
				DisposeIntersectNodes();
			}
		}

		private void DisposeIntersectNodes()
		{
			_intersectList.Clear();
		}

		private void AddNewIntersectNode(Active ae1, Active ae2, long topY)
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
			else if (pt.Y < topY)
			{
				//topY is at the top of the scanbeam
				pt.Y = topY;
				if (ae1.top.Y == topY)
					pt.X = ae1.top.X;
				else if (ae2.top.Y == topY)
					pt.X = ae2.top.X;
				else if (Math.Abs(ae1.dx) < Math.Abs(ae2.dx))
					pt.X = ae1.curX;
				else
					pt.X = ae2.curX;
			}

			IntersectNode node = new IntersectNode(pt, ae1, ae2);
			_intersectList.Add(node);
		}

		private Active ExtractFromSEL(Active ae)
		{
			Active res = ae.nextInSEL;
			if (res != null)
				res.prevInSEL = ae.prevInSEL;
			if (ae.prevInSEL != null)
				ae.prevInSEL.nextInSEL = res;
			return res;
		}

		private void Insert1Before2InSEL(Active ae1, Active ae2)
		{
			ae1.prevInSEL = ae2.prevInSEL;
			if (ae1.prevInSEL != null)
				ae1.prevInSEL.nextInSEL = ae1;
			ae1.nextInSEL = ae2;
			ae2.prevInSEL = ae1;
		}

		private bool BuildIntersectList(long topY)
		{
			if (_actives == null || _actives.nextInAEL == null) return false;

			//Calculate edge positions at the top of the current scanbeam, and from this
			//we will determine the intersections required to reach these new positions.
			AdjustCurrXAndCopyToSEL(topY);

			//Find all edge intersections in the current scanbeam using a stable merge
			//sort that ensures only adjacent edges are intersecting. Intersect info is
			//stored in FIntersectList ready to be processed in ProcessIntersectList.
			//Re merge sorts see https://stackoverflow.com/a/46319131/359538

			Active left = _sel, right, lEnd, rEnd, currBase, prevBase, tmp;

			while (left.jump != null)
			{
				prevBase = null;
				while (left != null && left.jump != null)
				{
					currBase = left;
					right = left.jump;
					lEnd = right;
					rEnd = right.jump;
					left.jump = rEnd;
					while (left != lEnd && right != rEnd)
					{
						if (right.curX < left.curX)
						{
							tmp = right.prevInSEL;
							for (; ; )
							{
								AddNewIntersectNode(tmp, right, topY);
								if (tmp == left) break;
								tmp = tmp.prevInSEL;
							}
							tmp = right;
							right = ExtractFromSEL(tmp);
							lEnd = right;
							Insert1Before2InSEL(tmp, left);
							if (left == currBase)
							{
								currBase = tmp;
								currBase.jump = rEnd;
								if (prevBase == null) _sel = currBase;
								else prevBase.jump = currBase;
							}
						}
						else left = left.nextInSEL;
					}
					prevBase = currBase;
					left = rEnd;
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

				IntersectNode node = _intersectList[i];
				IntersectEdges(node.edge1, node.edge2, node.pt);
				SwapPositionsInAEL(node.edge1, node.edge2);
			}
		}

		private void SwapPositionsInAEL(Active ae1, Active ae2)
		{
			//preconditon: ae1 must be immediately to the left of ae2
			Active next = ae2.nextInAEL;
			if (next != null) next.prevInAEL = ae1;
			Active prev = ae1.prevInAEL;
			if (prev != null) prev.nextInAEL = ae2;
			ae2.prevInAEL = prev;
			ae2.nextInAEL = ae1;
			ae1.prevInAEL = ae2;
			ae1.nextInAEL = next;
			if (ae2.prevInAEL == null) _actives = ae2;
		}

		private bool ResetHorzDirection(Active horz, Active maxPair, 
			out long leftX, out long rightX)
		{
			if (horz.bot.X == horz.top.X)
			{
				//the horizontal edge is going nowhere ...
				leftX = horz.curX;
				rightX = horz.curX;
				Active ae = horz.nextInAEL;
				while (ae != null && ae != maxPair) ae = ae.nextInAEL;
				return ae != null;
			}
			else if (horz.curX < horz.top.X)
			{
				leftX = horz.curX;
				rightX = horz.top.X;
				return true;
			}
			else
			{
				leftX = horz.top.X;
				rightX = horz.curX;
				return false;  //right to left
			}
		}

		private bool TrimHorz(Active horzEdge, bool preserveCollinear)
		{
			bool result = false;
			Point64 pt = NextVertex(horzEdge).pt;
			//trim 180 deg. spikes in closed paths
			while ((pt.Y == horzEdge.top.Y) && (!preserveCollinear ||
				((pt.X < horzEdge.top.X) == (horzEdge.bot.X < horzEdge.top.X))))
			{
				horzEdge.vertexTop = NextVertex(horzEdge);
				horzEdge.top = pt;
				result = true;
				if (IsMaxima(horzEdge)) break;
				pt = NextVertex(horzEdge).pt;
			}
			if (result) SetDx(horzEdge); // +/-infinity
			return result;
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
			bool horzIsOpen = IsOpen(horz);

			Active maxPair = null;
			bool isMax = IsMaxima(horz);

			//remove 180 deg.spikes and also with closed paths and not PreserveCollinear
			//simplify consecutive horizontals into a 'single' edge ...
			if (!horzIsOpen && !isMax && TrimHorz(horz, PreserveCollinear))
				isMax = IsMaxima(horz);
			
			if (isMax && !IsOpenEnd(horz))
				maxPair = GetMaximaPair(horz);

			bool isLeftToRight = 
				ResetHorzDirection(horz, maxPair, out long leftX, out long rightX);

			if (IsHotEdge(horz))
				AddOutPt(horz, new Point64(horz.curX, horz.bot.Y));

			for (; ; )
			{  //loops through consec. horizontal edges (if open)
				Active ae;
				if (isLeftToRight) ae = horz.nextInAEL;
				else ae = horz.prevInAEL;

				while (ae != null)
				{

					if (ae == maxPair)
					{
						if (IsHotEdge(horz))
						{
							if (isLeftToRight) AddLocalMaxPoly(horz, ae, horz.top);
							else AddLocalMaxPoly(ae, horz, horz.top);
						}
						DeleteFromAEL(ae);
						DeleteFromAEL(horz);
						return;
					}
					
					//if horzEdge is a maxima, keep going until we reach
					//its maxima pair, otherwise check for break conditions
					if (!isMax || IsOpenEnd(horz))
					{
						//otherwise stop when 'ae' is beyond the end of the horizontal line
						if ((isLeftToRight && ae.curX > rightX) ||
						 (!isLeftToRight && ae.curX < leftX)) break;

						if (ae.curX == horz.top.X && !IsHorizontal(ae))
						{
							//for edges at the end or horzEdge, keep going until horzEdge's
							//outslope is greater than ae's slope when heading right or until
							//horzEdge's outslope is less than ae's slope when heading left.
							pt = NextVertex(horz).pt;
							if ((isLeftToRight && TopX(ae, pt.Y) >= pt.X) ||
								(!isLeftToRight && TopX(ae, pt.Y) <= pt.X)) break;
						}
					} //!isMax

					pt = new Point64(ae.curX, horz.bot.Y);
					if (isLeftToRight)
					{
						IntersectEdges(horz, ae, pt);
						SwapPositionsInAEL(horz, ae);
						ae = horz.nextInAEL;
					}
					else
					{
						IntersectEdges(ae, horz, pt);
						SwapPositionsInAEL(ae, horz);
						ae = horz.prevInAEL;
					}
				}

				//check if we've finished looping through consecutive horizontals
				if (isMax || NextVertex(horz).pt.Y != horz.top.Y) break;

				UpdateEdgeIntoAEL(horz);
				isMax = IsMaxima(horz);

				if (!horzIsOpen && !isMax && TrimHorz(horz, PreserveCollinear)) 
					isMax = IsMaxima(horz); //ie update after TrimHorz

				isLeftToRight = ResetHorzDirection(horz, maxPair, out leftX, out rightX);

				if (isMax) maxPair = GetMaximaPair(horz);
				if (IsHotEdge(horz)) AddOutPt(horz, horz.bot);
			}

			if (IsHotEdge(horz)) AddOutPt(horz, horz.top);

			if (!IsOpen(horz)) UpdateEdgeIntoAEL(horz);  //this is the } of an intermediate horiz.
			else if (!isMax) UpdateEdgeIntoAEL(horz);
			else if (maxPair == null) DeleteFromAEL(horz); //ie open at top
			else if (IsHotEdge(horz)) AddLocalMaxPoly(horz, maxPair, horz.top);
			else
			{
				DeleteFromAEL(maxPair);
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
					ae.curX = ae.top.X;
					if (ae.prevInAEL != null && (ae.prevInAEL.curX == ae.curX) &&
							(ae.prevInAEL.bot.Y != y) && IsHotEdge(ae.prevInAEL))
						AddOutPt(ae.prevInAEL, ae.top);
					if (ae.nextInAEL != null && (ae.nextInAEL.curX == ae.curX) &&
							(ae.nextInAEL.top.Y != y) && IsHotEdge(ae.nextInAEL))
						AddOutPt(ae.nextInAEL, ae.top);

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
				else //ie not the top of the edge
					ae.curX = TopX(ae, y);

				ae = ae.nextInAEL;
			}
		}

		private Active DoMaxima(Active ae)
		{
			Active nextE, prevE, maxPair;
			prevE = ae.prevInAEL;
			nextE = ae.nextInAEL;
			if (IsOpenEnd(ae))
			{
				if (IsHotEdge(ae))
					AddOutPt(ae, ae.top);
				if (!IsHorizontal(ae))
				{
					if (IsHotEdge(ae)) ae.outrec = null;
					DeleteFromAEL(ae);
				}
				return nextE;
			}
			else
			{
				maxPair = GetMaximaPair(ae);
				if (maxPair == null) return nextE;  //eMaxPair is horizontal
			}

			//only non-horizontal maxima here.
			//process any edges between maxima pair ...
			while (nextE != maxPair)
			{
				IntersectEdges(ae, nextE, ae.top);
				SwapPositionsInAEL(ae, nextE);
				nextE = ae.nextInAEL;
			}

			if (IsOpen(ae))
			{
				if (IsHotEdge(ae))
				{
					if (maxPair != null)
						AddLocalMaxPoly(ae, maxPair, ae.top);
					else
						AddOutPt(ae, ae.top);
				}
				if (maxPair != null) DeleteFromAEL(maxPair);
				DeleteFromAEL(ae);
				return (prevE != null ? prevE.nextInAEL : _actives);
			}
			//here ae.nextInAel == ENext == EMaxPair ...
			if (IsHotEdge(ae))
				AddLocalMaxPoly(ae, maxPair, ae.top);

			DeleteFromAEL(ae);
			DeleteFromAEL(maxPair);
			return (prevE != null ? prevE.nextInAEL : _actives);
		}

		private static OutPt DeleteOp(OutPt op)
    {
			OutPt result = (op.next == op ? null : op.next);
			op.prev.next = op.next;
			op.next.prev = op.prev;
			return result;
		}

		private static bool ValidatePath(ref OutPt op)
		{
			if (op.next != op && op.next != op.prev) return true;
			op = null;
			return false; 
		}

		private static void CleanCollinear(ref OutPt op, bool preserveCollinear)
		{
			if (!ValidatePath(ref op)) return;
			OutPt startOp = op, op2 = op;
			for (; ; )
			{
				//nb: if preserveCollinear == true, then only remove 180 deg.spikes
				if ((InternalClipperFunc.CrossProduct(op2.prev.pt, op2.pt, op2.next.pt) == 0) &&
					(!preserveCollinear || (InternalClipperFunc.DotProduct(op2.prev.pt, op2.pt, op2.next.pt) < 0)))
				{
					if (op2 == op) op = op2.prev;
					op2 = DeleteOp(op2);
					if (!ValidatePath(ref op2))
					{
						op = null;
						return;
					}
					startOp = op2;
					continue;
				} 
				else
					op2 = op2.next;
				if (op2 == startOp) break;
			}
		}

		private static double AreaTriangle(Point64 pt1, Point64 pt2, Point64 pt3)
    {
			return 0.5 * (pt1.X - pt3.X) * (pt2.Y - pt1.Y) -
				(pt1.X - pt2.X) * (pt3.Y - pt1.Y);
		}

		private OutPt DoSplitOp(ref OutPt outRecOp, OutPt splitOp) 
		{
			OutPt prevOp = splitOp.prev, nextNextOp = splitOp.next.next;
			OutPt result = prevOp;
			InternalClipperFunc.GetIntersectPoint(
				prevOp.pt, splitOp.pt, splitOp.next.pt, nextNextOp.pt, out PointD ipD);
			Point64 ip = new Point64(ipD);
#if USINGZ
#endif
			double area1 = Area(outRecOp);
			double area2 = AreaTriangle(ip, splitOp.pt, splitOp.next.pt);

			if (ip == prevOp.pt || ip == nextNextOp.pt)
			{
				nextNextOp.prev = prevOp;
				prevOp.next = nextNextOp;
			}
			else
			{
				OutPt newOp = new OutPt(ip) { prev = prevOp, next = nextNextOp };
				nextNextOp.prev = newOp;
				prevOp.next = newOp;
			}

			if ((Math.Abs(area2) > (Math.Abs(area1))) ||
				((area2 > 0) == (area1 > 0))) 
			{

				OutRec newOutRec = new OutRec();
				_outrecList.Add(newOutRec);
				newOutRec.idx = _outrecList.Count - 1;
				newOutRec.polypath = null;

				OutPt newOp = new OutPt(ip) { prev = splitOp.next, next = splitOp };
				newOutRec.pts = newOp;
				splitOp.prev = newOp;
				splitOp.next.next = newOp;
			} 
			else
				splitOp = null;
			return result;
		}

		private void FixSelfIntersects(ref OutPt op)
    {
			//precondition:
			//the 'op' linked list's length must be at least 3
			OutPt op2 = op;
			for (; ; )
			{
				//3 edged polygons can't self-intersect
				if (op2.prev == op2.next.next) break;
				if (InternalClipperFunc.SegmentsIntersect(op2.prev.pt,
					op2.pt, op2.next.pt, op2.next.next.pt))
				{
					if (op2 == op || op2.next == op) op = op2.prev;
					op2 = DoSplitOp(ref op, op2);
					op = op2;
					continue;
				} else
					op2	= op2.next;
				if (op2 == op) break;
			}
		}

		internal bool BuildPath(OutPt op, bool isOpen, Path64 path)
		{
			int cnt = PointCount(op);
			if (cnt < 2) return false;
			path.Clear();
			Point64 lastPt = op.pt;
			path.Add(lastPt);
			op = op.next;
			for (int i = 1; i < cnt; i++)
			{
				if (op.pt != lastPt)
				{
					lastPt = op.pt;
					path.Add(lastPt);
				}
				op = op.next;
			}

			if (isOpen) return path.Count > 1;
			else return path.Count > 2;
		}

		protected bool BuildPaths(Paths64 solutionClosed, Paths64 solutionOpen)
		{
			bool built = false;
			solutionClosed.Clear();
			solutionOpen.Clear();
			try
			{
				solutionClosed.Capacity = _outrecList.Count;
				solutionOpen.Capacity = _outrecList.Count;

				for (int j = 0; j < _outrecList.Count; j++)
				{
					OutRec outrec = _outrecList[j];
					if (outrec.pts == null) continue;

					Path64 path = new Path64();
					if (outrec.state == OutRecState.Open)
					{
						if (BuildPath(outrec.pts.next, true, path))
							solutionOpen.Add(path);
					}
					else
					{
						if (BuildPath(outrec.pts.next, false, path))
							solutionClosed.Add(path);
					}
				}
				built = true;
			}
			catch { }
			return built;
		}

		protected bool BuildTree(PolyPathBase polytree, Paths64 solutionOpen)
		{
			polytree.Clear();
			solutionOpen.Clear();
			solutionOpen.Capacity = _outrecList.Count;
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

					Path64 path = new Path64(); 
					if (!BuildPath(outrec.pts.next, isOpenPath, path)) continue;
					
					if (isOpenPath) { 
						solutionOpen.Add(path);
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

	} //ClipperBase class


	public class Clipper : ClipperBase
  {

		public new void AddPath(Path64 path, PathType polytype, bool isOpen = false)
		{
			base.AddPath(path, polytype, isOpen);
		}

		public new void AddPaths(Paths64 paths, PathType polytype, bool isOpen = false)
    {
			base.AddPaths(paths, polytype, isOpen);
		}

		public void AddSubject(Paths64 paths)
		{
			AddPaths(paths, PathType.Subject, false);
		}

		public void AddOpenSubject(Paths64 paths)
		{
			AddPaths(paths, PathType.Subject, true);
		}

		public void AddClip(Paths64 paths)
		{
			AddPaths(paths, PathType.Clip, false);
		}

		public bool Execute(ClipType clipType, FillRule fillRule,
			Paths64 solutionClosed, Paths64 solutionOpen)
		{
			solutionClosed.Clear();
			solutionOpen.Clear();
			try
			{
				ExecuteInternal(clipType, fillRule);
				BuildPaths(solutionClosed, solutionOpen);
			}
			catch
			{
				solutionClosed = null;
				solutionOpen = null;
			}
			CleanUp();
			return (solutionClosed != null || solutionOpen != null);
		}

		public bool Execute(ClipType clipType, FillRule fillRule, Paths64 solutionClosed)
		{
			return Execute(clipType, fillRule, solutionClosed, new Paths64());
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PolyTree polytree, Paths64 openPaths)
		{
			polytree.Clear();
			openPaths.Clear();
			bool success = false;
			try
			{
				ExecuteInternal(clipType, fillRule);
				success = BuildTree(polytree, openPaths);
			}
			catch { }
			CleanUp();
			return success;
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PolyTree polytree)
		{
			return Execute(clipType, fillRule, polytree, new Paths64());
		}

	}  //Clipper class

	public class ClipperD : ClipperBase
	{
		private readonly double _scale;
		private readonly double _invScale;

#if USINGZ
		public delegate void ZCallbackD(PointD bot1, PointD top1,
				PointD bot2, PointD top2, ref PointD intersectPt);
		public ZCallbackD ZFillDFunc { get; set; }
	#endif

		public ClipperD(int roundingDecimalPrecision = 2)
		{
			if (roundingDecimalPrecision < -8 || roundingDecimalPrecision > 8)
				throw new ClipperLibException("Error - RoundingDecimalPrecision exceeds the allowed range.");
			_scale = Math.Pow(10, roundingDecimalPrecision);
			_invScale = 1 / _scale;
		}

	#if USINGZ
		private void ProxyZCallback(Point64 bot1, Point64 top1,
			Point64 bot2, Point64 top2, ref Point64 intersectPt)
    {
			//de-scale coordinates
			PointD tmp = ClipperFunc.ScalePoint(intersectPt, _invScale);
			ZFillDFunc(
					ClipperFunc.ScalePoint(bot1, _invScale),
					ClipperFunc.ScalePoint(top1, _invScale),
					ClipperFunc.ScalePoint(bot2, _invScale),
					ClipperFunc.ScalePoint(top2, _invScale), ref tmp);
			//re-scale
			intersectPt.Z = (long)Math.Round(tmp.z * _scale);
		}
	#endif

		public void AddPath(PathD path, PathType polytype, bool isOpen = false)
		{
			AddPath(ClipperFunc.ScalePath(path, _scale), polytype, isOpen);
		}

		public void AddPaths(PathsD paths, PathType polytype, bool isOpen = false)
		{
			AddPaths(ClipperFunc.ScalePaths(paths, _scale), polytype, isOpen);
		}
		public void AddSubject(PathD path)
		{
			AddPath(path, PathType.Subject, false);
		}

		public void AddOpenSubject(PathD path)
		{
			AddPath(path, PathType.Subject, true);
		}

		public void AddClip(PathD path)
		{
			AddPath(path, PathType.Clip, false);
		}

		public void AddSubject(PathsD paths)
		{
			AddPaths(paths, PathType.Subject, false);
		}

		public void AddOpenSubject(PathsD paths)
		{
			AddPaths(paths, PathType.Subject, true);
		}

		public void AddClip(PathsD paths)
		{
			AddPaths(paths, PathType.Clip, false);
		}

		public bool Execute(ClipType clipType, FillRule fillRule,
			PathsD solutionClosed, PathsD solutionOpen)
		{
			Paths64 solClosed64 = new Paths64(), solOpen64 = new Paths64();
		#if USINGZ
			ZCallback64 ZFillSaved = ZFillFunc;
			if (ZFillDFunc != null && ZFillFunc == null)
				ZFillFunc = ProxyZCallback;
		#endif

			bool res = false;
			solutionClosed.Clear();
			solutionOpen.Clear();
			try
			{
				ExecuteInternal(clipType, fillRule);
				res = BuildPaths(solClosed64, solOpen64);
			}
			catch
			{
				solutionClosed = null;
				solutionOpen = null;
			}
			CleanUp();

		#if USINGZ
			ZFillFunc = ZFillSaved;
		#endif
			ClipperFunc.ScalePaths(solClosed64, _invScale, ref solutionClosed);
			ClipperFunc.ScalePaths(solOpen64, _invScale, ref solutionOpen);
			return res;
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PathsD solutionClosed)
		{
			return Execute(clipType, fillRule, solutionClosed, new PathsD());
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PolyTreeD polytree, PathsD openPaths)
		{
			polytree.Clear();
			(polytree as PolyPathD).Scale = _scale;
		#if USINGZ
			ZCallback64 ZFillSaved = ZFillFunc;
			if (ZFillDFunc != null && ZFillFunc == null)
				ZFillFunc = ProxyZCallback;
		#endif
			openPaths.Clear();
			Paths64 oPaths = new Paths64();
			bool success = false;
			try
			{
				ExecuteInternal(clipType, fillRule);
				success = BuildTree(polytree, oPaths);
			}
			catch { }
		#if USINGZ
			ZFillFunc = ZFillSaved;
		#endif
			CleanUp();

			if (oPaths.Count > 0)
				ClipperFunc.ScalePaths(oPaths, _invScale, ref openPaths);
			return success;
		}

		public bool Execute(ClipType clipType, FillRule fillRule, PolyTreeD polytree)
		{
			return Execute(clipType, fillRule, polytree, new PathsD());
		}

	} //ClipperD class


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

		internal abstract PolyPathBase AddChild(Path64 p);
	
		public PolyPathBase GetChild(int idx)
		{
			if (idx < 0 || idx >= ChildCount) return null;
			else return _childs[idx];
		}

		public void Clear()
    {
			_childs.Clear();
    }

	} // PolyPathBase class

	public class PolyPath : PolyPathBase
	{
    public Path64 Polygon { get; private set; }
		public PolyPath(PolyPathBase parent = null) : base(parent) { }
		internal override PolyPathBase AddChild(Path64 p)
		{
			PolyPathBase newChild = new PolyPath(this);
			(newChild as PolyPath).Polygon = p;
			_childs.Add(newChild);
			return newChild;
		}
	}

	public class PolyPathD : PolyPathBase
	{
		internal double Scale { get; set; }
		public PathD Polygon { get; private set; }
		public PolyPathD(PolyPathBase parent = null) : base(parent) { }
		internal override PolyPathBase AddChild(Path64 p)
		{
			PolyPathBase newChild = new PolyPathD(this);
			(newChild as PolyPathD).Scale = Scale;
			(newChild as PolyPathD).Polygon = ClipperFunc.ScalePath(p, 1/Scale);
			_childs.Add(newChild);
			return newChild;
		}
	}

	public class PolyTree : PolyPath { }

	public class PolyTreeD : PolyPathD
	{
		public new double Scale { get => base.Scale; }
	}

	public class ClipperLibException : Exception
	{
		public ClipperLibException(string description) : base(description) { }
	}

} //namespace