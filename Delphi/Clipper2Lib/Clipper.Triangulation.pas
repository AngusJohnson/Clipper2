unit Clipper.Triangulation;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  13 December 2025                                                *
* Release   :  BETA RELEASE                                                    *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2025                                         *
* Purpose   :  Delaunay Triangulation                                          *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************)

interface

uses
  SysUtils, Math, Classes, Clipper.Core;

type
  TTriangulateResult = (trSuccess, trFail, trNoPolygons, trPathsIntersect);

  function Triangulate(const paths: TPaths64; out solution: TPaths64;
    useDelaunay: Boolean = true): TTriangulateResult; overload;
  function Triangulate(const paths: TPathsD;
    decPlaces: integer; out solution: TPathsD;
    useDelaunay: Boolean = true): TTriangulateResult; overload;

  // these functions are intended only for debugging
  function Triangulate(const paths: TPaths64; out solution, actives: TPaths64;
    useDelaunay: Boolean = true): TTriangulateResult; overload;
  function Triangulate(const paths: TPathsD; decPlaces: integer; out solution: TPathsD;
    out activeEdges: TPathsD; useDelaunay: Boolean): TTriangulateResult; overload;

implementation

uses Clipper;

type
  PVertex = ^TVertex;
  PEdge = ^TEdge;
  PTriangle = ^TTriangle;
  TDelaunay = class;

  TArrayOfVertex = array of PVertex;
  TArrayOfEdge = array of PEdge;
  TEdgeKind   = (ekLoose, ekAsc, ekDesc);

  TVertex = record
    pt        : TPoint64;
    edges     : TArrayOfEdge;
    innerLM   : Boolean;
  end;

  TEdge = record
    vL        : PVertex;
    vR        : PVertex;
    vT        : PVertex;
    vB        : PVertex;
    kind      : TEdgeKind;
    triangleA : PTriangle;
    triangleB : PTriangle;
    isActive  : Boolean;
    nextE     : PEdge;  // next in "active edge array" (AEL)
    prevE     : PEdge;  // prev in "active edge array" (AEL)
  end;

  TTriangle = record
  public
    edges               : array[0..2] of PEdge;
  end;

  PVertexArray = ^TVertexArray;
  TVertexArray = array of TVertex;
  TVertexArrays = array of TVertexArray;

  TEdgeStack = class(TStackEx)
  public
    function Pop(out edge: PEdge): Boolean;
  end;

  TVertexStack = class(TStackEx)
  public
    function Pop(out vertex: PVertex): Boolean;
  end;

  TDelaunay = class
  private
    vertexList      : TListEx;
    edgeList        : TListEx;
    triangleList    : TListEx;
    DelaunayPending : TEdgeStack;
    horzEdgeStack   : TEdgeStack; // used to delay horizontal edge processing
    locMinStack     : TVertexStack;
    fUseDelaunay    : Boolean;
    fActives        : PEdge;      // simple (unsorted) double-linked list
    lowestVertex    : PVertex;
    function FixupEdgeIntersects(edgeList: TListEx): Boolean;
    procedure RemoveIntersection(e1, e2: PEdge);
    procedure MergeDupOrCollinearVertices;
    function CreateInnerLocMinLooseEdge(vAbove: PVertex): PEdge;
    function HorizontalBetween(v1, v2: PVertex): PEdge;
    procedure DoTriangulateLeft(edge: PEdge; pivot: PVertex; minY: Int64);
    procedure DoTriangulateRight(edge: PEdge; pivot: PVertex; minY: Int64);
    function FindLinkingEdge(vert1, vert2: PVertex; preferAscending: Boolean): PEdge;
    function CreateEdge(v1, v2: PVertex; kind: TEdgeKind): PEdge;
    function CreateTriangle(e1, e2, e3: PEdge): PTriangle;
    procedure ForceLegal(edge: PEdge);
    procedure AddEdgeToActives(edge: PEdge);
    procedure RemoveEdgeFromActives(edge: PEdge);
  protected
    currentY  : Int64;
  public
    constructor Create(useDelaunay: Boolean = true);
    destructor Destroy; override;
    procedure Clear;
    procedure AddPath(const path: TPath64);
    procedure AddPaths(const paths: TPaths64);
    function Triangulate(out solution: TPaths64): TTriangulateResult;
  end;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function RoundUpNearestPower2(val: Cardinal): Cardinal; inline;
begin
  // precondition : val > 0
  Result := val -1;
  Result := Result or Result shr 1;
  Result := Result or Result shr 2;
  Result := Result or Result shr 4;
  Result := Result or Result shr 8;
  Result := Result or Result shr 16;
  inc(Result);
end;
//------------------------------------------------------------------------------

// CountActives - used for debugging
function CountActives(actives: PEdge): integer; inline;
begin
  Result := 0;
  while Assigned(actives) do
  begin
    inc(Result);
    actives := actives.nextE;
  end;
end;
//------------------------------------------------------------------------------

function IsLooseEdge(edge: PEdge): Boolean; inline;
begin
  Result := edge.kind = ekLoose;
end;
//------------------------------------------------------------------------------

function IsLeftEdge(edge: PEdge): Boolean; inline;
begin
  // left edge (bound) of a fill region
  // ie fills on the **right** side of the edge
  Result := (edge.kind = ekAsc);
end;
//------------------------------------------------------------------------------

function IsRightEdge(edge: PEdge): Boolean; inline;
begin
  // right edge (bound) of a fill region
  // but still fills on the **right** side of the edge
  Result := (edge.kind = ekDesc);
end;
//------------------------------------------------------------------------------

function IsHorizontal(e: PEdge): Boolean; inline; overload;
begin
  Result := e.vB.pt.Y = e.vT.pt.Y;
end;
//------------------------------------------------------------------------------

function LeftTurning(pt1, pt2, pt3: TPoint64): Boolean; inline; overload;
begin
  Result := CrossProductSign(pt1, pt2, pt3) < 0;
end;
//------------------------------------------------------------------------------

function LeftTurning(p1, p2, p3: PVertex): Boolean; inline; overload;
begin
  Result := CrossProductSign(p1.pt, p2.pt, p3.pt) < 0;
end;
//------------------------------------------------------------------------------

function RightTurning(p1, p2, p3: PVertex): Boolean; inline;
begin
  Result := CrossProductSign(p1.pt, p2.pt, p3.pt) > 0;
end;
//------------------------------------------------------------------------------

function EdgeCompleted(edge: PEdge): Boolean;
begin
  if not Assigned(edge.triangleA) then Result := false
  else if Assigned(edge.triangleB) then Result := true
  else Result := edge.kind <> ekLoose;
end;
//------------------------------------------------------------------------------

function GetAngle(const a, b, c: TPoint64): double;
var
  abx, aby, bcx, bcy: double;
  dp, cp: double;
begin
  //https://stackoverflow.com/a/3487062/359538
  abx := b.x - a.x; aby := b.y - a.y;
  bcx := b.x - c.x; bcy := b.y - c.y;
  dp := (abx * bcx + aby * bcy);
  cp := (abx * bcy - aby * bcx);
  Result := arctan2(cp, dp); //range between -Pi and Pi
end;
//------------------------------------------------------------------------------

function GetLocMinAngle(v: PVertex): double;
var
  des, asc: integer;
begin
  // todo - recheck the result's sign compared to left vs right turning
  // (currently assumes left turning => positive values)
  // precondition - this function is called before processing locMin.
  Assert(Length(v.edges) = 2);
  if v.edges[0].kind = ekAsc then
  begin
    asc := 0;
    des := 1;
  end else
  begin
    des := 0;
    asc := 1;
  end;
  // winding direction - descending to ascending
  Result := GetAngle(v.edges[des].vT.pt, v.pt, v.edges[asc].vT.pt);
end;
//------------------------------------------------------------------------------

type TSegIntersectResult = (sirNone, sirCollinear, sirIntersect);

function SegmentsIntersect(const s1a, s1b, s2a, s2b: TPoint64): TSegIntersectResult;
var
  dx1,dy1, dx2,dy2, t, cp: double;
begin
  Result := sirNone;

  //ignore segments sharing an end-point
  if PointsEqual(s1a, s2a) or
    PointsEqual(s1a, s2b) or
    PointsEqual(s1b, s2b) then Exit;

  dy1 := (s1b.y - s1a.y);
  dx1 := (s1b.x - s1a.x);
  dy2 := (s2b.y - s2a.y);
  dx2 := (s2b.x - s2a.x);
  cp  := dy1 * dx2 - dy2 * dx1;
  if cp = 0 then
  begin
    Result := sirCollinear;
    Exit;
  end;

  t := ((s1a.x-s2a.x) * dy2 - (s1a.y-s2a.y) * dx2);
  // nb: testing for t == 0 is unreliable due to float imprecision
  if (t >= 0) then
  begin
    if (cp < 0) or (t >= cp) then Exit;
  end else
  begin
    if (cp > 0) or (t <= cp) then Exit;
  end;

  // so far the *segment* 's1' intersects the *line* through 's2',
  // but now make sure it also intersects the *segment* 's2'
  t := ((s1a.x-s2a.x) * dy1 - (s1a.y-s2a.y) * dx1);
  if (t >= 0) then
  begin
    if (cp > 0) and (t < cp) then
      Result := sirIntersect;
  end else
  begin
    if (cp < 0) and (t > cp) then
      Result := sirIntersect;
  end;
end;
//------------------------------------------------------------------------------

function ShortestDistFromSegment(const pt, segPt1, segPt2: TPoint64): double;
var
  ax,ay : double;
  dx,dy : double;
  qNum  : double;
begin
  // precondition: segPt1 <> segPt2
  dx := (segPt2.X - segPt1.X);
  dy := (segPt2.Y - segPt1.Y);
  Assert((dx <> 0) or (dy <> 0)); // ie segPt1 <> segPt2

  ax := (pt.X - segPt1.X);
  ay := (pt.Y - segPt1.Y);
  //q = (ax * dx + ay * dy) / (dx * dx + dy * dy);
  qNum := ax * dx + ay * dy;
  if (qNum < 0) then // pt is closest to seq1
    Result := DistanceSqr(pt, segPt1)
  else if (qNum > (Sqr(dx) + Sqr(dy))) then // pt is closest to seq2
    Result := DistanceSqr(pt, segPt2)
  else
    Result := Sqr(ax * dy - dx * ay) / (dx * dx + dy * dy);
end;
//------------------------------------------------------------------------------

function PathFromTriangle(tri: PTriangle): TPath64;
begin
  SetLength(Result, 3);
  Result[0] := tri.edges[0].vL.pt;
  Result[1] := tri.edges[0].vR.pt;
  with tri.edges[1]^ do
    if PointsEqual(vL.pt, Result[0]) or PointsEqual(vL.pt, Result[1]) then
      Result[2] := vR.pt else
      Result[2] := vL.pt;
end;
//------------------------------------------------------------------------------

function InCircleTest(const ptA, ptB, ptC, ptD: TPoint64): double;
var
  m00, m01, m02, m10, m11, m12, m20, m21, m22: double;
begin
  // Return the determinant value of 3 x 3 matrix ...
  // | ax-dx    ay-dy    Sqr(ax-dx)+Sqr(ay-dy) |
  // | bx-dx    by-dy    Sqr(bx-dx)+Sqr(by-dy) |
  // | cx-dx    cy-dy    Sqr(cx-dx)+Sqr(cy-dy) |

  // The *sign* of the return value is determined by
  // the orientation (CW vs CCW) of ptA, ptB & ptC.

  // precondition - ptA, ptB & ptC make a *non-empty* triangle
  m00 := ptA.x - ptD.x;
  m01 := ptA.y - ptD.y;
  m02 := (sqr(m00) + sqr(m01));
  m10 := ptB.x - ptD.x;
  m11 := ptB.y - ptD.y;
  m12 := (sqr(m10) + sqr(m11));
  m20 := ptC.x - ptD.x;
  m21 := ptC.y - ptD.y;
  m22 := (sqr(m20) + sqr(m21));
  Result := m00 * (m11 * m22 - m21 * m12) -
            m10 * (m01 * m22 - m21 * m02) +
            m20 * (m01 * m12 - m11 * m02);
end;
//------------------------------------------------------------------------------

type
  TEcResult = (ecNeither, ecL, ecR);

function EdgeContains(edge: PEdge; v: PVertex): TEcResult; inline;
begin
  if edge.vL = v then Result := ecL
  else if edge.vR = v then Result := ecR
  else Result := ecNeither;
end;
//------------------------------------------------------------------------------

procedure AddEdgeToVertex(vert: PVertex; edge: PEdge);
var
  cnt: integer;
begin
  cnt := Length(vert.edges);
  SetLength(vert.edges, cnt +1);
  vert.edges[cnt] := edge;
end;
//------------------------------------------------------------------------------

procedure RemoveEdgeFromVertex(vert: PVertex; edge: PEdge);
var
  i, last: integer;
begin
  last := High(vert.edges);
  for i := 0 to last do
    if vert.edges[i] = edge then
    begin
      if i < last then
        Move(vert.edges[i +1], vert.edges[i], (last - i) * SizeOf(PEdge));
      SetLength(vert.edges, last);
      Break;
    end;
end;
//------------------------------------------------------------------------------

function GetSign(i: Int64): integer; inline;
begin
  if i > 0 then Result := 1
  else if i < 0 then Result := -1
  else Result := 0;
end;
//------------------------------------------------------------------------------

// this sort is only needed prior to edge intersection detection
function EdgeListSortFunc(item1, item2: Pointer): integer;
var
  e1    : PEdge absolute item1;
  e2    : PEdge absolute item2;
begin
  // sort edgeList to ascend on edge.vL.pt.X ...
  Result := GetSign(e1.vL.pt.X - e2.vL.pt.X);
end;
//------------------------------------------------------------------------------

function VertexListSortFunc(item1, item2: Pointer): Integer;
var
  v1    : PVertex absolute item1;
  v2    : PVertex absolute item2;
begin
  // Result > 0 indicates the correct order
  // Primary sort is descending Y.
  Result := GetSign(v2.pt.Y - v1.pt.Y);
  if (Result <> 0) or (v1 = v2) then Exit;
  // sorting on X is necessary to ensure that duplicate vertices
  // are detected and merged (see MergeDupOrCollinearVertices)
  Result := GetSign(v1.pt.X - v2.pt.X);
end;
//------------------------------------------------------------------------------

//function VertexXBetween(v: PVertex; x1, x2: Int64): Boolean;
//begin
//  if (x1 < x2) then
//  begin
//    Result := (v.pt.X > x1) and (v.pt.X < x2);
//  end else
//  begin
//    Result := (v.pt.X > x2) and (v.pt.X < x1);
//  end;
//end;
//------------------------------------------------------------------------------

function FindLocMinIdx(idx, len: integer; const p: TPath64): integer;
var
  i, n: integer;
begin
  Result := -1;
  if (len < 3) then Exit;
  i := idx;
  n := (i + 1) mod len;
  while (p[n].Y <= p[i].Y) do
  begin
    i := n;
    n := (n + 1) mod len;
    if (i = idx) then
      Exit; // fails if the path is completely horizontal
  end;
  while (p[n].Y >= p[i].Y) do
  begin
    i := n;
    n := (n + 1) mod len;
  end;
  Result := i;
end;

//------------------------------------------------------------------------------
// TEdgeStack
//------------------------------------------------------------------------------

function TEdgeStack.Pop(out edge: PEdge): Boolean;
begin
  Result := inherited Pop(Pointer(edge));
end;

//------------------------------------------------------------------------------
// TVertexStack
//------------------------------------------------------------------------------

function TVertexStack.Pop(out vertex: PVertex): Boolean;
begin
  Result := inherited Pop(Pointer(vertex));
end;

//------------------------------------------------------------------------------
// TDelaunay
//------------------------------------------------------------------------------

constructor TDelaunay.Create(useDelaunay: Boolean);
begin
  vertexList      := TListEx.Create;
  edgeList        := TListEx.Create;
  triangleList    := TListEx.Create;
  DelaunayPending := TEdgeStack.Create;
  horzEdgeStack   := TEdgeStack.Create;
  locMinStack     := TVertexStack.Create;
  fUseDelaunay    := useDelaunay;
  //lowestVertex    := nil;
end;
//------------------------------------------------------------------------------

destructor TDelaunay.Destroy;
begin
  Clear;
  vertexList.Free;
  edgeList.Free;
  triangleList.Free;
  DelaunayPending.Free;
  horzEdgeStack.Free;
  locMinStack.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.Clear;
var
  i: integer;
begin
  for i := 0 to vertexList.Count -1 do
    Dispose(PVertex(vertexList[i]));
  vertexList.Clear;

  for i := 0 to edgeList.Count -1 do
    Dispose(PEdge(edgeList[i]));
  edgeList.Clear;

  for i := 0 to triangleList.Count -1 do
    Dispose(PTriangle(triangleList[i]));
  triangleList.Clear;
  DelaunayPending.Clear;
  horzEdgeStack.Clear;
  locMinStack.Clear;
  fActives := nil;
end;
//------------------------------------------------------------------------------

function TDelaunay.FindLinkingEdge(vert1, vert2: PVertex;
  preferAscending: Boolean): PEdge;
var
  i: integer;
begin
  Result := nil;
  with vert1^ do
    for i := 0 to High(edges) do
      if (edges[i].vL = vert2) or (edges[i].vR = vert2) then
      begin
        Result := edges[i];
        if (Result.kind = ekLoose) or
          ((Result.kind = ekAsc) = preferAscending) then Exit;
      end;
end;
//------------------------------------------------------------------------------

function TDelaunay.CreateEdge(v1, v2: PVertex; kind: TEdgeKind): PEdge;
begin
  // with *fixed* edges, v1 is always lower than v2 except when
  // horizontal when it connects to a lower edge (or is a locMin).
  // ie: v1.pt.Y >= v2.pt.Y always
  New(Result);
  Result.isActive := false; // this ignored by loose edges

  AddEdgeToVertex(v1, Result);
  AddEdgeToVertex(v2, Result);

  Result.triangleA := nil;
  Result.triangleB := nil;
  Result.kind := kind;
  Result.prevE := nil;
  Result.nextE := nil;

  if kind = ekLoose then
  begin
    DelaunayPending.Push(Result);
    AddEdgeToActives(Result);
  end;
  edgeList.Add(Result);

  if (v1.pt.Y = v2.pt.Y) then
  begin
    // with fixed edges, v1 will always connects to the lower edge
    Result.vB := v1;
    Result.vT := v2;
  end
  else if v1.pt.Y < v2.pt.Y then
  begin
    Assert(kind = ekLoose);
    Result.vB := v2;
    Result.vT := v1;
  end else
  begin
    Result.vB := v1;
    Result.vT := v2;
  end;

  if v1.pt.X <= v2.pt.X then
  begin
    Result.vL := v1;
    Result.vR := v2;
  end else
  begin
    Result.vL := v2;
    Result.vR := v1;
  end;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.ForceLegal(edge: PEdge);
var
  i: integer;
  ictResult: double;
  vertA, vertB: PVertex;
  edgesA, edgesB: array[0..2] of PEdge;
begin
  // don't try to make empty triangles legal
  if not Assigned(edge.triangleA) or
    not Assigned(edge.triangleB) then
      Exit;

  // vertA will be assigned the vertex in edge's triangleA
  // that is NOT an end vertex of edge
  // Likewise, vertB will be assigned the vertex in edge's
  // triangleB that is NOT an end vertex of edge
  // If edge is rotated, vertA & vertB will become its end vertices.
  vertA := nil;
  vertB := nil;

  // Excluding 'edge', edgesA will contain two edges (one from
  // triangleA and one from triangleB) that touch edge.vL.
  // And edgesB will contain the two edges that touch edge.vR.
  edgesA[0] := nil; // unused
  edgesB[0] := nil; // unused

  with edge.triangleA^ do
  begin
    for i := 0 to 2 do
    begin
      if Edges[i] = edge then Continue;
      case EdgeContains(Edges[i], edge.vL) of
        ecL: begin edgesA[1] := Edges[i]; vertA := Edges[i].vR; end;
        ecR: begin edgesA[1] := Edges[i]; vertA := Edges[i].vL; end;
        else edgesB[1] := Edges[i];
      end;
    end;
  end;

  with edge.triangleB^ do
  begin
    for i := 0 to 2 do
    begin
      if Edges[i] = edge then Continue;
      case EdgeContains(Edges[i], edge.vL) of
        ecL: begin edgesA[2] := Edges[i]; vertB := Edges[i].vR; end;
        ecR: begin edgesA[2] := Edges[i]; vertB := Edges[i].vL; end;
        else edgesB[2] := Edges[i];
      end;
    end;
  end;

  // InCircleTest reqires edge.triangleA to be a valid triangle
  // if IsEmptyTriangle(edge.triangleA) then Exit; // slower
  if CrossProductIsZero(vertA.pt, edge.vL.pt, edge.vR.pt) then Exit;

  // ictResult - result sign is dependant on triangleA's orientation
  ictResult := InCircleTest(vertA.pt, edge.vL.pt, edge.vR.pt, vertB.pt);
  if (ictResult = 0) or // if on or out of circle then exit
    (RightTurning(vertA, edge.vL, edge.vR) = (ictResult < 0)) then
      Exit;

  // TRIANGLES HERE ARE **NOT** DELAUNAY COMPLIANT, SO MAKE THEM SO.

  // NOTE: ONCE WE BEGIN DELAUNAY COMPLIANCE, vL & vR WILL
  // NO LONGER REPRESENT LEFT AND RIGHT VERTEX ORIENTATION.
  // THIS IS MINOR PERFORMANCE EFFICIENCY IS SAFE AS LONG AS
  // THE TRIANGULATE() METHOD IS CALLED ONCE ONLY ON A GIVEN
  // SET OF PATHS

  edge.vL := vertA;
  edge.vR := vertB;

  edge.triangleA.edges[0] := edge;
  for i := 1 to 2 do
  begin
    edge.triangleA.edges[i] := edgesA[i];

    if IsLooseEdge(edgesA[i]) then
      DelaunayPending.Push(edgesA[i]);
    // since each edge has its own triangleA and triangleB, we have to be careful
    // to update the correct one ...
    if (edgesA[i].triangleA = edge.triangleA) or
      (edgesA[i].triangleB = edge.triangleA) then Continue;

    if (edgesA[i].triangleA = edge.triangleB) then
      edgesA[i].triangleA := edge.triangleA
    else if (edgesA[i].triangleB = edge.triangleB) then
      edgesA[i].triangleB := edge.triangleA
    else raise Exception.Create('oops')
  end;

  edge.triangleB.edges[0] := edge;
  for i := 1 to 2 do
  begin
    edge.triangleB.edges[i] := edgesB[i];
    if IsLooseEdge(edgesB[i]) then
      DelaunayPending.Push(edgesB[i]);

    if (edgesB[i].triangleA = edge.triangleB) or
      (edgesB[i].triangleB = edge.triangleB) then Continue;

    if (edgesB[i].triangleA = edge.triangleA) then
      edgesB[i].triangleA := edge.triangleB
    else if (edgesB[i].triangleB = edge.triangleA) then
      edgesB[i].triangleB := edge.triangleB
    else raise Exception.Create('oops')
  end;
end;
//------------------------------------------------------------------------------

function TDelaunay.CreateTriangle(e1, e2, e3: PEdge): PTriangle;
var
  i: integer;
begin
  new(Result);
  triangleList.Add(Result);

  Result.edges[0] := e1;
  Result.edges[1] := e2;
  Result.edges[2] := e3;

  // nb: only expire loose edges when both sides of these edges have triangles.
  for i := 0 to 2 do
    with Result.edges[i]^ do
      if Assigned(triangleA) then
      begin
        triangleB := Result;
        // this is the edge's second triangle hence no longer active
        RemoveEdgeFromActives(Result.edges[i]);
      end else
      begin
        triangleA := Result;
        // this is the edge's first triangle, so only remove
        // this edge from actives if it's a fixed edge.
        if kind <> ekLoose then
          RemoveEdgeFromActives(Result.edges[i]);
      end;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.AddEdgeToActives(edge: PEdge);
begin
  // nb: on occassions this method can get called twice for a given edge
  // This is because, in the Triangulate() method where vertex 'edges'
  // arrays are being parsed, edges can can be removed from the array
  // which changes the index of following edges.
  if edge.isActive then Exit;

  edge.prevE := nil;
  edge.nextE := fActives;
  edge.isActive := true;
  if Assigned(fActives) then
    fActives.prevE := edge;
  fActives := edge;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.RemoveEdgeFromActives(edge: PEdge);
var
  prev, next: PEdge;
begin
  // first, remove the edge from its vertices
  RemoveEdgeFromVertex(edge.vB, edge);
  RemoveEdgeFromVertex(edge.vT, edge);

  // now remove the edge from double linked list (AEL)
  prev := edge.prevE;
  next := edge.nextE;
  if Assigned(next) then next.prevE := prev;
  if Assigned(prev) then prev.nextE := next;
  edge.isActive := false;

  if fActives = edge then fActives := next;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.RemoveIntersection(e1, e2: PEdge);
var
  len: integer;
  d, d2: double;
  v, v2: PVertex;
  tmpE: PEdge;
begin
  // find which vertex is closest to the other segment
  // (ie not the vertex closest to the intersection point) ...
  v := e1.vL;
  tmpE := e2;
  d := ShortestDistFromSegment(e1.vL.pt, e2.vL.pt, e2.vR.pt);
  d2 := ShortestDistFromSegment(e1.vR.pt, e2.vL.pt, e2.vR.pt);
  if d2 < d then begin d := d2; v := e1.vR; end;
  d2 := ShortestDistFromSegment(e2.vL.pt, e1.vL.pt, e1.vR.pt);
  if d2 < d then begin d := d2; tmpE := e1; v := e2.vL; end;
  d2 := ShortestDistFromSegment(e2.vR.pt, e1.vL.pt, e1.vR.pt);
  if d2 < d then begin d := d2; tmpE := e1; v := e2.vR; end;
  if d > 1.000 then
  begin
    // Oops - this is not just a simple 'rounding' intersection
    Exit;
  end;
  // split 'tmpE' into 2 edges at 'v'
  v2 := tmpE.vT;
  RemoveEdgeFromVertex(v2, tmpE);
  // replace v2 in tmpE with v
  if tmpE.vL = v2 then
    tmpE.vL := v else
    tmpE.vR := v;
  tmpE.vT := v;
  len := Length(v.edges);
  SetLength(v.edges, len + 1);
  v.edges[len] := tmpE;
  v.innerLM := false; // #47
  // left turning is angle positive
  if tmpE.vB.innerLM and (GetLocMinAngle(tmpE.vB) <= 0) then
    tmpE.vB.innerLM := false; // #44, 52
  // finally create a new edge between v and v2 ...
  CreateEdge(v, v2, tmpE.kind);
end;
//------------------------------------------------------------------------------

function TDelaunay.FixupEdgeIntersects(edgeList: TListEx): Boolean;
var
  i, j: integer;
  currE, e: PEdge;
begin
  // precondition - edgeList must be sorted - ascending on edge.vL.pt.X
  Result := true;
  for i := 0 to edgeList.Count -1 do
  begin
    // nb: we can safely ignore edges newly created inside this for loop
    currE := PEdge(edgeList[i]);
    for j := i +1 to edgeList.Count -1 do
    begin
      e := PEdge(edgeList[j]);
      if (e.vL.pt.X >= currE.vR.pt.X) then
        Break; // all 'e' from now on are too far right so break inner loop

      // 'e' is inside currE's horizontal region. If 'e' is also inside
      // currE's vertical region only then check for an intersection ...
      if (e.vT.pt.Y < currE.vB.pt.Y) and (e.vB.pt.Y > currE.vT.pt.Y) and
        (SegmentsIntersect(e.vL.pt, e.vR.pt,
          currE.vL.pt, currE.vR.pt) = sirIntersect) then
            RemoveIntersection(e, currE);
      // collinear edges are managed in MergeDupOrCollinearVertices below
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.MergeDupOrCollinearVertices;

  procedure SplitEdge(longEdge, shortEdge: PEdge);
  var
    len         : integer;
    oldT, newT  : PVertex;
  begin
    oldT := longEdge.vT;
    newT := shortEdge.vT;

    // remove longEdge from longEdge.vT.edges
    RemoveEdgeFromVertex(oldT, longEdge);

    // shorten longEdge
    longEdge.vT := newT;
    if longEdge.vL = oldT then
      longEdge.vL := newT else
      longEdge.vR := newT;
    // add shortened longEdge to newT.edges
    len := Length(newT.edges);
    SetLength(newT.edges, len + 1);
    newT.edges[len] := longEdge;
    // and create a new edge betweem newV, oldT
    CreateEdge(newT, oldT, longEdge.kind);
  end;

var
  i,j,k, len1, len2, lenTot: integer;
  v, pv: PVertex;
  e1, e2: PEdge;
begin
  if vertexList.Count < 2 then Exit;
  i := 0;
  repeat
    pv := PVertex(vertexList[i]);
    inc(i);
  until Assigned(pv.edges);

  for i := i to vertexList.Count -1 do
  begin
    v := PVertex(vertexList[i]);
    if not Assigned(v.edges) then Continue;
    if PointsEqual(v.pt, pv.pt) then
    begin
      // merge v with pv ...
      if not pv.innerLM or not v.innerLM then
        pv.innerLM := false;

      len1 := Length(pv.edges);
      len2 := Length(v.edges);
      lenTot := len1 + len2;

      for j := 0 to len2 -1 do
        with v.edges[j]^ do
        begin
          if vB = v then vB := pv
          else vT := pv;
          if vL = v then vL := pv
          else vR := pv;
        end;
      SetLength(pv.edges, lenTot);
      Move(v.edges[0], pv.edges[len1], len2 * SizeOf(PEdge));
      v.edges := nil;

      // excluding horizontals, if pv.edges contains two edges
      // that are *collinear* and share the same bottom coords
      // but have different lengths, split the longer edge at
      // the top of the shorter edge ...
      for j := 0 to lenTot -2 do
      begin
        e1 := pv.edges[j];
        if IsHorizontal(e1) then Continue;
        if (e1.vB = pv) then
          for k := j + 1 to lenTot -1 do
          begin
            e2 := pv.edges[k];
            if (e2.vB = pv) and (e1.vT.pt.Y <> e2.vT.pt.Y) and
              CrossProductIsZero(e1.vT.pt, pv.pt, e2.vT.pt) then
            begin
              // parallel edges, both heading up from v1.pt.
              // split the longer edge at the top of the shorter edge.
              if e1.vT.pt.Y < e2.vT.pt.Y then
                SplitEdge(e1, e2)
              else
                SplitEdge(e2, e1);
              Break; // because only two edges can be collinear
            end;
          end;
      end;
    end else
    begin
      // current vertex 'v' is dissimilar to pv so update pv
      pv := v;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TDelaunay.CreateInnerLocMinLooseEdge(vAbove: PVertex): PEdge;
var
  e, eBelow: PEdge;
  vBest: PVertex;
  xAbove, yAbove, xBest, yBest: Int64;
  d, bestD: double;
begin
  Result := nil;
  if not Assigned(fActives) then Exit; // oops!!

  xAbove := vAbove.pt.X;
  yAbove := vAbove.pt.Y;

  // find the closest 'active' edge with a vertex that's not above vAbove
  e := fActives;
  eBelow := nil;
  bestD := -1.0;
  while Assigned(e) do
  begin
    // ignore 'e' when vL is greater than xAbove or vR is less than xAbove
    if (e.vL.pt.X <= xAbove) and (e.vR.pt.X >= xAbove) and

      // the following line is needed unless inserting
      // locMin edges into active edges is delayed ...
      (e.vL <> vAbove) and (e.vR <> vAbove) and

      not LeftTurning(e.vL, vAbove, e.vR) then
    begin
      d := ShortestDistFromSegment(vAbove.pt, e.vL.pt, e.vR.pt);
      if Assigned(eBelow) then // compare e with eBelow
      begin
        if (d >= bestD) then
        begin
          e := e.nextE;
          Continue;
        end;
        eBelow := e; // ie replace current eBelow candidate :)
        bestD := d;
      end else
      begin
        eBelow := e; // ie first candidate
        bestD := d
      end;
    end;
    e := e.nextE;
  end;
  if not Assigned(eBelow) then Exit; // oops!!

  // get the best vertex from 'eBelow'
  if eBelow.vT.pt.Y <= yAbove then
    vBest := eBelow.vB else
    vBest := eBelow.vT;

  xBest := vBest.pt.X;
  yBest := vBest.pt.Y;

  // make sure no edges intersect 'vAbove' and 'vBest' ...
  // todo: fActives is currently *unsorted* but consider making it
  // a tree structure based on each edge's left and right bounds
  e := fActives;
  if xBest < xAbove then
  begin
    while Assigned(e) do
    begin
      if (e.vR.pt.X > xBest) and (e.vL.pt.X < xAbove) and
         (e.vB.pt.Y > yAbove) and (e.vT.pt.Y < yBest) and
         (SegmentsIntersect(e.vB.pt, e.vT.pt,
         vBest.pt, vAbove.pt) = sirIntersect) then
      begin
        if e.vt.pt.Y > yAbove then
          vBest := e.vT else
          vBest := e.vB;
        xBest := vBest.pt.X;
        yBest := vBest.pt.Y;
      end;
      e := e.nextE;
    end;
  end else
  begin
    while Assigned(e) do
    begin
      if (e.vR.pt.X < xBest) and (e.vL.pt.X > xAbove) and
         (e.vB.pt.Y > yAbove) and (e.vT.pt.Y < yBest) and
         (SegmentsIntersect(e.vB.pt, e.vT.pt,
         vBest.pt, vAbove.pt) = sirIntersect) then
      begin
        if e.vt.pt.Y > yAbove  then
          vBest := e.vT else
          vBest := e.vB;
        xBest := vBest.pt.X;
        yBest := vBest.pt.Y;
      end;
      e := e.nextE;
    end;
  end;
  Result := CreateEdge(vBest, vAbove, ekLoose);
end;
//------------------------------------------------------------------------------

function TDelaunay.HorizontalBetween(v1, v2: PVertex): PEdge;
var
  l,r, y: Int64;
begin
  // ?? todo - keep a separate list of active horizontals (at current Y)
  Result := fActives;
  y := v1.pt.Y;
  if v1.pt.X > v2.pt.X then
  begin
    l := v2.pt.X;
    r := v1.pt.X;
  end else
  begin
    l := v1.pt.X;
    r := v2.pt.X;
  end;

  while Assigned(Result) do
  begin
    if (Result.vL.pt.Y = y) and (Result.vR.pt.Y = y) and
      (Result.vL.pt.X >= l) and (Result.vR.pt.X <= r) and
      ((Result.vL.pt.X <> l) or (Result.vL.pt.X <> r)) then Exit;
    Result := Result.nextE;
  end;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.DoTriangulateLeft(edge: PEdge; pivot: PVertex; minY: Int64);
var
  i         : integer;
  v, vAlt   : PVertex;
  vX        : PVertex;
  e, eX     : PEdge;
  eAlt      : PEdge;
  cps       : integer;
begin
  // precondition - pivot will be .vB unless edge is horizontal
  if edge.vB = pivot then
    v := edge.vT else
    v := edge.vB;
  vAlt := nil;
  eAlt := nil;

  for i := 0 to High(pivot.edges) do
    if pivot.edges[i] <> edge then
    begin
      e := pivot.edges[i];
      if not e.isActive then Continue;

      if e.vT = pivot then
        vX := e.vB else
        vX := e.vT;

      if vX = v then Continue;

      cps := CrossProductSign(v.pt, pivot.pt, vX.pt);
      if cps = 0 then //collinear paths
      begin
        // if pivot is between v and vX then continue;
        // nb: this is important for both horiz and non-horiz collinear
        if (v.pt.X > pivot.pt.X) = (pivot.pt.X > vX.pt.X) then
            Continue;
      end
      // else if right-turning or not the best edge, then continue
      else if (cps > 0) or
        (Assigned(vAlt) and not LeftTurning(vX, pivot, vAlt)) then
          Continue;

      vAlt := vX;
      eAlt := e;
    end;

  if not Assigned(vAlt) or (vAlt.pt.Y < minY) then Exit;

//  if (vAlt.pt.Y = v.pt.Y) then
//  begin
//    for i := 0 to locMinStack.Count -1 do
//      if VertexXBetween(PVertex(locMinStack[i]),
//        v.pt.X, vAlt.pt.X) then Exit;
//  end;

  // Don't triangulate **across** fixed edges
  if vAlt.pt.Y < pivot.pt.Y then
  begin
    if IsLeftEdge(eAlt) then Exit;
  end else if vAlt.pt.Y > pivot.pt.Y then
  begin
    if IsRightEdge(eAlt) then Exit;
  end;

  eX := FindLinkingEdge(vAlt, v, (vAlt.pt.Y < v.pt.Y));
  if not Assigned(eX) then
  begin
    // be very careful creating loose horizontals at minY
    if (vAlt.pt.Y = v.pt.Y) and (v.pt.Y = minY) and
      Assigned(HorizontalBetween(vAlt, v)) then Exit;
    eX := CreateEdge(vAlt, v, ekLoose);
  end;

  CreateTriangle(edge, eAlt, eX);
  if not EdgeCompleted(eX) then
    DoTriangulateLeft(eX, vAlt, minY);
end;
//------------------------------------------------------------------------------

procedure TDelaunay.DoTriangulateRight(edge: PEdge; pivot: PVertex; minY: Int64);
var
  i, cps    : integer;
  v, vAlt   : PVertex;
  vX        : PVertex;
  e, eX     : PEdge;
  eAlt      : PEdge;
begin
  // precondition - pivot will be .vB unless edge is horizontal
  if edge.vB = pivot then
    v := edge.vT else
    v := edge.vB;
  vAlt := nil;
  eAlt := nil;

  for i := 0 to High(pivot.edges) do
    if pivot.edges[i] <> edge then
    begin
      e := pivot.edges[i];
      if not e.isActive then Continue;

      if e.vT = pivot then
        vX := e.vB else
        vX := e.vT;

      if vX = v then Continue;

      cps := CrossProductSign(v.pt, pivot.pt, vX.pt);
      if cps = 0 then //collinear paths
      begin
        // if pivot is between v and vX then continue;
        // nb: this is important for both horiz and non-horiz collinear
        if (v.pt.X > pivot.pt.X) = (pivot.pt.X > vX.pt.X) then
            Continue;
      end
      // else if left-turning or not the best edge, then continue
      else if (cps < 0) or
        (Assigned(vAlt) and not RightTurning(vX, pivot, vAlt)) then
          Continue;

      vAlt := vX;
      eAlt := e;
    end;

  if not Assigned(vAlt) or (vAlt.pt.Y < minY) then Exit;

//  if (vAlt.pt.Y = v.pt.Y) then
//  begin
//    for i := 0 to locMinStack.Count -1 do
//      if VertexXBetween(PVertex(locMinStack[i]),
//        v.pt.X, vAlt.pt.X) then Exit;
//  end;

  // Don't triangulate **across** fixed edges
  if vAlt.pt.Y < pivot.pt.Y then
  begin
    if IsRightEdge(eAlt) then Exit;
  end else if vAlt.pt.Y > pivot.pt.Y then
  begin
    if IsLeftEdge(eAlt) then Exit;
  end;

  eX := FindLinkingEdge(vAlt, v, (vAlt.pt.Y > v.pt.Y));
  if not Assigned(eX) then
  begin
    // be very careful creating loose horizontals at minY
    if (vAlt.pt.Y = v.pt.Y) and (v.pt.Y = minY) and
      Assigned(HorizontalBetween(vAlt, v)) then Exit;
    eX := CreateEdge(vAlt, v, ekLoose);
  end;
  CreateTriangle(edge, eX, eAlt);
  if not EdgeCompleted(eX) then
    DoTriangulateRight(eX, vAlt, minY);
end;
//------------------------------------------------------------------------------

function TDelaunay.Triangulate(out solution: TPaths64): TTriangulateResult;
var
  i,j     : integer;
  cps     : integer;
  currY   : Int64;
  p, lm   : PVertex;
  e       : PEdge;
begin
  Result := TTriangulateResult.trSuccess;
  if vertexList.Count < 3 then
  begin
    Result := TTriangulateResult.trNoPolygons;
    Exit;
  end;

  // if necessary fix path orientation because the algorithm
  // expects clockwise outer paths and counter-clockwise inner paths
  if lowestvertex.innerLM then
  begin
    // the orientation of added paths must be wrong, so
    // 1. reverse innerLM flags ...
    while locMinStack.Pop(lm) do
      lm.innerLM := not lm.innerLM;
    // 2. swap edge kinds
    for i := 0 to edgeList.Count -1 do
      with PEdge(edgeList[i])^ do
        if kind = ekAsc then
          kind := ekDesc else
          kind := ekAsc;
  end else
  begin
    // path orientation is fine so ...
    locMinStack.Clear;
  end;

  // fix up any micro edge intersections that will break triangulation.
  // This fix-up is the only reason these edges need to be sorted,
  // and could be skipped if no intersections was guaranteed.

  edgeList.Sort(EdgeListSortFunc);
  if not FixupEdgeIntersects(edgeList) then
  begin
    Result := TTriangulateResult.trPathsIntersect;
    Exit;
  end;

  vertexList.Sort(VertexListSortFunc);
  MergeDupOrCollinearVertices;

  try
    currY := PVertex(vertexList[0]).pt.Y;
    for j := 0 to vertexList.Count -1 do
    begin
      p := PVertex(vertexList[j]);
      if not Assigned(p.edges) then
        Continue; // ignore merged vertices

      if p.pt.Y <> currY then
      begin

        // JOIN AN INNER LOCMIN WITH A SUITABLE EDGE BELOW
        while locMinStack.Pop(lm) do
        begin
          e := CreateInnerLocMinLooseEdge(lm);
          if not Assigned(e) then
          begin
            Result := TTriangulateResult.trFail;
            Break;
          end;
          if IsHorizontal(e) then
          begin
            if e.vL = e.vB then
              DoTriangulateLeft(e, e.vB, currY) else
              DoTriangulateRight(e, e.vB, currY);
          end else
          begin
            DoTriangulateLeft(e, e.vB, currY);
            if not EdgeCompleted(e) then
              DoTriangulateRight(e, e.vB, currY);
          end;

        end;
        if Result <> TTriangulateResult.trSuccess then Break;

        while horzEdgeStack.Pop(e) do
          if EdgeCompleted(e) then
            Continue
          else if (e.vB = e.vL) then // #45
          begin
            if IsLeftEdge(e) then
              DoTriangulateLeft(e, e.vB, currY)
          end else
            if IsRightEdge(e) then
              DoTriangulateRight(e, e.vB, currY);

        currY := p.pt.Y;
      end;

      for i := High(p.edges) downto 0 do
      begin

        // The following line may look superfluous, but within this loop
        // p.edges may be altered with additions and or deletions.
        // Also, we need to use a *descending* index which is safe because
        // any additions will be loose edges which are ignored here.
        if i > High(p.edges) then Continue;

        e := p.edges[i];
        if EdgeCompleted(e) or IsLooseEdge(e) then Continue;

        if p = e.vB then
        begin
          if IsHorizontal(e) then
            horzEdgeStack.Push(e);
          AddEdgeToActives(e);
        end else
        begin
          if IsHorizontal(e) then
            horzEdgeStack.Push(e)
          else if IsLeftEdge(e) then
            DoTriangulateLeft(e, e.vB, p.pt.Y)
          else
            DoTriangulateRight(e, e.vB, p.pt.Y);
        end;
      end;

      if p.innerLM then
        locMinStack.Push(p);
    end;

  except
    Result := TTriangulateResult.trFail;
    Exit;
  end;

  if (Result = TTriangulateResult.trSuccess) then
  begin
    while horzEdgeStack.Pop(e) do
      if not EdgeCompleted(e) and (e.vB = e.vL) then
        DoTriangulateLeft(e, e.vB, currY);

    // Convert triangles to Delaunay conforming
    if fUseDelaunay then
      while DelaunayPending.Pop(e) do ForceLegal(e);
  end;

  j := 0;
  SetLength(solution, triangleList.Count);
  for i := 0 to triangleList.Count -1 do
    with PTriangle(triangleList[i])^ do
    begin
      solution[j] := PathFromTriangle(PTriangle(triangleList[i]));
      cps := CrossProductSign(solution[j,0],solution[j,1],solution[j,2]);
      if cps = 0 then
        Continue  // skip any empty triangles
      else if cps < 0 then // ccw
        solution[j] := ReversePath(solution[j]);
      inc(j);
    end;
  SetLength(solution, j);
end;
//------------------------------------------------------------------------------

function PrevIdx(idx, len: integer): integer; inline;
begin
  if idx = 0 then
    Result := len -1 else
    Result := idx -1;
end;
//------------------------------------------------------------------------------

function NextIdx(idx, len: integer): integer; inline;
begin
  Result := (idx + 1) mod len;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.AddPath(const path: TPath64);

  function AddVertex(const pt: TPoint64): PVertex;
  begin
    new(Result);
    Result.pt := pt;
    Result.innerLM := false;
    vertexList.Add(Result);
  end;

  function Vert(idx: integer): PVertex;
  begin
    Result := PVertex(VertexList[idx]);
  end;

var
  i, i0, len, iPrev, iNext: integer;
  oldVertexListCnt: integer;
  v, v0, vPrev, vPrevPrev: PVertex;
begin
  oldVertexListCnt := vertexList.Count;
  len := Length(path);

  // find the first locMin
  i0 := FindLocMinIdx(0, len, path);
  if i0 < 0 then Exit; // an invalid path
  iPrev := PrevIdx(i0, len);
  while PointsEqual(path[iPrev], path[i0]) do
    iPrev := PrevIdx(iPrev, len);
  iNext := NextIdx(i0, len);

  // it is possible for a locMin here to simply be a
  // collinear spike that should be ignored, so ...
  i := i0;
  while CrossProductIsZero(path[iPrev], path[i], path[iNext]) do
  begin
    i := FindLocMinIdx(i, len, path);
    if i = i0 then Exit; // an entirely collinear path

    iPrev := PrevIdx(i, len);
    while PointsEqual(path[iPrev], path[i]) do
      iPrev := PrevIdx(iPrev, len);
    iNext := NextIdx(i, len);
  end;

  // we are now at the first legitimate locMin
  v0 := AddVertex(path[i]);
  if LeftTurning(path[iPrev], path[i], path[iNext]) then
    v0.innerLM := true;

  vPrev := v0;
  i := iNext;
  while true do
  begin
    // vPrev is a locMin here
    locMinStack.Push(vPrev);
    if not Assigned(lowestVertex) or
      (vPrev.pt.Y > lowestVertex.pt.Y) or
      ((vPrev.pt.Y = lowestVertex.pt.Y) and
      (vPrev.pt.X < lowestVertex.pt.X)) then
        lowestVertex := vPrev;

    iNext := NextIdx(i, len);
    if CrossProductIsZero(vPrev.pt, path[i], path[iNext]) then
    begin
      i := iNext;
      Continue;
    end;

    // ascend up next bound to LocMax
    while (path[i].Y <= vPrev.pt.Y) do
    begin
      v := AddVertex(path[i]);
      CreateEdge(vPrev, v, ekAsc);
      vPrev := v;
      i := iNext;
      iNext := NextIdx(i, len);

      while CrossProductIsZero(vPrev.pt, path[i], path[iNext]) do
      begin
        i := iNext;
        iNext := NextIdx(i, len);
      end;
    end;

    vPrevPrev := vPrev;
    // Now at a locMax, so descend to next locMin
    while (i <> i0) and (path[i].Y >= vPrev.pt.Y) do
    begin
      v := AddVertex(path[i]);
      CreateEdge(v, vPrev, ekDesc);
      vPrevPrev := vPrev;
      vPrev := v;
      i := iNext;
      iNext := NextIdx(i, len);

      while CrossProductIsZero(vPrev.pt, path[i], path[iNext]) do
      begin
        i := iNext;
        iNext := NextIdx(i, len);
      end;
    end;

    // now at the next locMin
    if (i = i0) then break; // break while(true) loop
    if LeftTurning(vPrevPrev.pt, vPrev.pt, path[i]) then
      vPrev.innerLM := true;
  end;
  CreateEdge(v0, vPrev, ekDesc);

  len := vertexList.Count - oldVertexListCnt;
  i := oldVertexListCnt;

  // make sure we still have a valid (ie not almost non-zero area) path
  if (len < 3) or       // too few vertices,
    ((len = 3) and      // or just a very tiny triangle
    ((DistanceSqr(Vert(i).pt, Vert(i+1).pt) <= 1) or
     (DistanceSqr(Vert(i+1).pt, Vert(i+2).pt) <= 1) or
     (DistanceSqr(Vert(i+2).pt, Vert(i).pt) <= 1))) then
  begin
    // flag the vertices added for this path as obsolete
    for i := oldVertexListCnt to vertexList.Count -1 do
      PVertex(vertexList[i]).edges := nil;
  end;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.AddPaths(const paths: TPaths64);
var
  i, j: integer;
begin
  j := vertexList.Count;
  for i := 0 to High(paths) do
    inc(j, Length(paths[i]));
  if j = 0 then Exit;
  j := RoundUpNearestPower2(j);
  if vertexList.Capacity < j then
    vertexList.Capacity := j;
  if edgeList.Capacity < j then
    edgeList.Capacity := j;

  for i := 0 to High(paths) do
    AddPath(paths[i]);
end;
//------------------------------------------------------------------------------

function Triangulate(const paths: TPaths64; out solution: TPaths64;
  useDelaunay: Boolean = true): TTriangulateResult;
var
  d: TDelaunay;
begin
  d := TDelaunay.Create(useDelaunay);
  try
    d.AddPaths(paths);
    Result := d.Triangulate(solution);
  finally
    d.Free;
  end;
end;
//------------------------------------------------------------------------------

function Triangulate(const paths: TPaths64;
  out solution, actives: TPaths64;
  useDelaunay: Boolean = true): TTriangulateResult;
var
  i: integer;
  d: TDelaunay;
  a: PEdge;
begin
  d := TDelaunay.Create(useDelaunay);
  try
    d.AddPaths(paths);
    Result := d.Triangulate(solution);

    a := d.fActives;
    i := CountActives(a);
    SetLength(actives, i);
    for i := 0 to i -1 do
    begin
      SetLength(actives[i], 2);
      actives[i][0] := a.vL.pt;
      actives[i][1] := a.vR.pt;
      a := a.nextE;
    end;
  finally
    d.Free;
  end;
end;
//------------------------------------------------------------------------------

function Triangulate(const paths: TPathsD;
  decPlaces: integer; out solution: TPathsD;
  useDelaunay: Boolean = true): TTriangulateResult; overload;
var
  scale: double;
  pp64, sol64: TPaths64;
begin
  if decPlaces <= 0 then scale := 1
  else if decPlaces > 8 then scale := Power(10, 8)
  else scale := Power(10, decPlaces);
  pp64 := ScalePaths(paths, scale);
  Result := Clipper.Triangulation.Triangulate(pp64, sol64, useDelaunay);
  if Result = TTriangulateResult.trSuccess then
    solution := ScalePathsD(sol64, 1/scale);
end;
//------------------------------------------------------------------------------

function Triangulate(const paths: TPathsD; decPlaces: integer;
  out solution: TPathsD; out activeEdges: TPathsD; useDelaunay: Boolean): TTriangulateResult;
var
  i: integer;
  d: TDelaunay;
  a: PEdge;
  scale: double;
  pp64: TPaths64;
begin
  if decPlaces <= 0 then scale := 1
  else if decPlaces > 8 then scale := Power(10, 8)
  else scale := Power(10, decPlaces);
  pp64 := ScalePaths(paths, scale);
  solution := nil;

  d := TDelaunay.Create(useDelaunay);
  try try
    d.AddPaths(pp64);
    Result := d.Triangulate(pp64);
    //if Result = trSuccess then
      solution := ScalePathsD(pp64, 1/scale);

    a := d.fActives;
    i := CountActives(a);
    SetLength(activeEdges, i);
    for i := 0 to i -1 do
    begin
      SetLength(activeEdges[i], 2);
      activeEdges[i][0] := PointD(a.vL.pt);
      activeEdges[i][1] := PointD(a.vR.pt);
      a := a.nextE;
    end;

  except
    Result := trFail;
    activeEdges := nil;
  end;
  finally
    d.Free;
  end;
end;
//------------------------------------------------------------------------------

end.
