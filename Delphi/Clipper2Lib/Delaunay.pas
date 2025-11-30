unit Delaunay;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  1 December 2025                                                 *
* Release   :  ALPHA RELEASE - ie expect bugs :)                               *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2010-2025                                         *
* Purpose   :  Delaunay Triangulation                                          *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************)

interface

uses
  SysUtils, Math, Classes, Clipper, Clipper.Core;

  function Triangulate(const paths: TPaths64;
    useDelaunay: Boolean = true): TPaths64; overload;
  function Triangulate(const paths: TPathsD;
    decPlaces: integer; delaunay: Boolean = true): TPathsD; overload;

  // this function that also returns activeEdges is intended only for debugging
  function Triangulate(const paths: TPathsD; decPlaces: integer;
    useDelaunay: Boolean; out activeEdges: TPathsD): TPathsD; overload;

implementation

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
    lenSqrd   : Double;
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
    vertexArrays  : TVertexArrays;
    looseVertices : TVertexArray;
    triangles     : TListEx;
    vertexList    : TListEx;
    fixedEdgeList : TListEx;
    looseEdgeList   : TListEx;
    DelaunayPending : TEdgeStack;
    horzEdgeStack   : TEdgeStack; // used to delay horizontal edge processing
    locMinStack     : TVertexStack;
    fUseDelaunay    : Boolean;
    fActives        : PEdge;      // simple (unsorted) double-linked list
    function ProcessVertArrayToNextLocMin(idx, len: integer;
      const va: TVertexArray): integer;
    function FixupEdgeIntersects(edgeList: TListEx): Boolean;
    procedure RemoveIntersection(e1, e2: PEdge);
    procedure MergeDupOrCollinearVertices;
    function CreateInnerLocMinLooseEdge(vAbove: PVertex): PEdge;
    function EdgeCompleted(edge: PEdge): Boolean;
    function HorizontalBetween(v1, v2: PVertex): PEdge;
    procedure DoTriangulateLeft(edge: PEdge; pivot: PVertex; minY: Int64);
    procedure DoTriangulateRight(edge: PEdge; pivot: PVertex; minY: Int64);
    function FindEdgeBetween(vert1, vert2: PVertex; preferAscending: Boolean): PEdge;
    function CreateEdge(v1, v2: PVertex; kind: TEdgeKind): PEdge;
    function CreateTriangle(e1, e2, e3: PEdge): PTriangle; // todo - as procedure???
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
    function Triangulate: TPaths64;
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

function LeftTurning(p1, p2, p3: PVertex): Boolean; inline; overload;
begin
  Result := CrossProductSign(p1.pt, p2.pt, p3.pt) < 0;
end;
//------------------------------------------------------------------------------

function RightTurning(p1, p2, p3: PVertex): Boolean; inline; overload;
begin
  Result := CrossProductSign(p1.pt, p2.pt, p3.pt) > 0;
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

  Result := sirNone;
  t := ((s1a.x-s2a.x) * dy2 - (s1a.y-s2a.y) * dx2);
  //ignore segments that 'intersect' at an end-point
  if (t = 0) then Exit;
  if (t > 0) then
  begin
    if (cp < 0) or (t >= cp) then Exit;
  end else
  begin
    if (cp > 0) or (t <= cp) then Exit;
  end;

  // so far the *segment* 's1' intersects the *line* through 's2',
  // but now make sure it also intersects the *segment* 's2'
  t := ((s1a.x-s2a.x) * dy1 - (s1a.y-s2a.y) * dx1);
  if (t = 0) then Exit;
  if (t > 0) then
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

function FixedEdgeListSortFunc(item1, item2: Pointer): Integer;
var
  e1    : PEdge absolute item1;
  e2    : PEdge absolute item2;
begin
  // sort edgeList to ascend on edge.vL.pt.X ...
  Result := e1.vL.pt.X - e2.vL.pt.X;
end;
//------------------------------------------------------------------------------

function VertexListSortFunc(item1, item2: Pointer): Integer;
var
  v1    : PVertex absolute item1;
  v2    : PVertex absolute item2;
begin
  // Result > 0 indicates the correct order
  Result := v2.pt.Y - v1.pt.Y;        // primary sort - descending Y
  if (Result <> 0) or (v1 = v2) then
    Exit;
  Result := v1.pt.X - v2.pt.X;        // secondary sort - ascending X
end;
//------------------------------------------------------------------------------

function GetVertex(idx, len: integer; const va: TVertexArray): PVertex; inline;
begin
  Result := @va[idx mod len];
end;
//------------------------------------------------------------------------------

function FindFirstLocMin(idx, len: integer; const va: TVertexArray): integer;
begin
  while GetVertex(idx +1, len, va).pt.Y <= GetVertex(idx, len, va).pt.Y do
    idx := (idx + 1) mod len;
  while GetVertex(idx +1, len, va).pt.Y >= GetVertex(idx, len, va).pt.Y do
    idx := (idx + 1) mod len;
  Result := idx;
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
  fixedEdgeList   := TListEx.Create;
  looseEdgeList   := TListEx.Create;
  triangles       := TListEx.Create;
  DelaunayPending := TEdgeStack.Create;
  horzEdgeStack   := TEdgeStack.Create;
  locMinStack   := TVertexStack.Create;
  fUseDelaunay  := useDelaunay;
end;
//------------------------------------------------------------------------------

destructor TDelaunay.Destroy;
begin
  Clear;
  vertexList.Free;
  fixedEdgeList.Free;
  looseEdgeList.Free;
  triangles.Free;
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

  vertexArrays := nil;
  looseVertices := nil;
  vertexList.Clear; // vertex storage was in vertexArrays

  for i := 0 to fixedEdgeList.Count -1 do
    Dispose(PEdge(fixedEdgeList[i]));
  fixedEdgeList.Clear;

  for i := 0 to looseEdgeList.Count -1 do
    Dispose(PEdge(looseEdgeList[i]));
  looseEdgeList.Clear;

  for i := 0 to triangles.Count -1 do
    Dispose(PTriangle(triangles[i]));
  triangles.Clear;
  DelaunayPending.Clear;
  horzEdgeStack.Clear;
  locMinStack.Clear;
  fActives := nil;
end;
//------------------------------------------------------------------------------

function TDelaunay.FindEdgeBetween(vert1, vert2: PVertex;
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
  Result.lenSqrd := DistanceSqr(v1.pt, v2.pt);
  Assert(Result.lenSqrd > 0);

  AddEdgeToVertex(v1, Result);
  AddEdgeToVertex(v2, Result);

  Result.triangleA := nil;
  Result.triangleB := nil;
  Result.kind := kind;
  Result.prevE := nil;
  Result.nextE := nil;

  if kind = ekLoose then
  begin
    looseEdgeList.Add(Result);
    DelaunayPending.Push(Result);
    AddEdgeToActives(Result);
  end else
    fixedEdgeList.Add(Result);

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
  triangles.Add(Result);

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
  tmpE.lenSqrd := DistanceSqr(tmpE.vB.pt, tmpE.vT.pt);
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
    longEdge.lenSqrd := DistanceSqr(longEdge.vB.pt, longEdge.vT.pt);
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
  pv := PVertex(vertexList[0]);
  for i := 1 to vertexList.Count -1 do
    if PointsEqual(PVertex(vertexList[i]).pt, pv.pt) then
    begin
      v := PVertex(vertexList[i]);
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
      pv := PVertex(vertexList[i]);
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
    if (e.vL.pt.X <= xAbove) and (e.vR.pt.X >= xAbove) and
      (e.vB.pt.Y >= yAbove) and
      (e.vB <> vAbove) and (e.vT <> vAbove) and
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

function TDelaunay.EdgeCompleted(edge: PEdge): Boolean;
begin
  if not Assigned(edge.triangleA) then Result := false
  else if Assigned(edge.triangleB) then Result := true
  else Result := edge.kind <> ekLoose;
end;
//------------------------------------------------------------------------------

function TDelaunay.HorizontalBetween(v1, v2: PVertex): PEdge;
var
  l,r, y: Int64;
begin
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
  // precondition - pivot must be one end of edge (Usually .vB)
  Assert(not EdgeCompleted(edge));
  vAlt := nil;
  eAlt := nil;
  if edge.vB = pivot then
    v := edge.vT else
    v := edge.vB;

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

  if not Assigned(vAlt) or (vAlt.pt.Y < minY) then
  begin
    Exit;
  end
  // Don't triangulate **across** fixed edges
  else if vAlt.pt.Y < pivot.pt.Y then
  begin
    if IsLeftEdge(eAlt) then Exit;
  end else if vAlt.pt.Y > pivot.pt.Y then
  begin
    if IsRightEdge(eAlt) then Exit;
  end;

  eX := FindEdgeBetween(vAlt, v, (vAlt.pt.Y < v.pt.Y));
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
  // precondition - pivot must be one end of edge (Usually .vB)
  Assert(not EdgeCompleted(edge));
  vAlt := nil;
  eAlt := nil;

  if edge.vB = pivot then
    v := edge.vT else
    v := edge.vB;

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

  if not Assigned(vAlt) or (vAlt.pt.Y < minY) then Exit

  // Don't triangulate **across** fixed edges
  else if vAlt.pt.Y < pivot.pt.Y then
  begin
    if IsRightEdge(eAlt) then Exit;
  end else if vAlt.pt.Y > pivot.pt.Y then
  begin
    if IsLeftEdge(eAlt) then Exit;
  end;

  eX := FindEdgeBetween(vAlt, v, (vAlt.pt.Y > v.pt.Y));
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

function TDelaunay.Triangulate: TPaths64;
var
  i, j    : integer;
  cps     : integer;
  currY   : integer;
  p, lm   : PVertex;
  e       : PEdge;
begin
  Result := nil;
  //TODO - return the solution as a var parameter and change
  // the function result to an integer error flag (0 = success)

  triangles.Clear;
  DelaunayPending.Clear;
  fActives := nil;
  if vertexList.Count < 3 then Exit;

  // fix up any micro edge intersections that will break triangulation.
  // This fix-up is the only reason these edges need to be sorted.
  fixedEdgeList.Sort(FixedEdgeListSortFunc);
  if not FixupEdgeIntersects(fixedEdgeList) then
    Exit; //oops!

  if not vertexList.Sorted then
  begin
    vertexList.Sort(VertexListSortFunc);
    MergeDupOrCollinearVertices;
  end;

  currY := PVertex(vertexList[0]).pt.Y;
  try
    for j := 0 to vertexList.Count -1 do
    begin
      p := PVertex(vertexList[j]);
      if not Assigned(p.edges) then Continue; // ie is a merged dup.

      if p.pt.Y <> currY then
      begin

        // JOIN AN INNER LOCMIN WITH A SUITABLE EDGE BELOW
        while locMinStack.Pop(lm) do
        begin
          e := CreateInnerLocMinLooseEdge(lm);
          Assert(Assigned(e));
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

          // and because adding locMin edges to Actives was delayed ..
          AddEdgeToActives(lm.edges[0]);
          AddEdgeToActives(lm.edges[1]);
        end;

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

        // the following line may look superfluous, but p.edges may be
        // altered (with additions and or deletions) during this 'for' loop.
        // That's why this line is necessary (and why we can't use an iterator).
        // Also, it is safe to use a descending index into the array because
        // any additions don't need to be re-processed within this loop.
        if i > High(p.edges) then Continue;

        e := p.edges[i];
        if EdgeCompleted(e) then
          Continue
        else if e.kind = ekLoose then
          Continue;

        if p = e.vB then
        begin
          if IsHorizontal(e) then
            horzEdgeStack.Push(e);
          // delay adding locMin edges to actives
          if not p.innerLM then
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

      if not p.innerLM then Continue;
      locMinStack.Push(p);
    end;

  except
    Exit; // ie don't create partial solutions
  end;

  while horzEdgeStack.Pop(e) do
    if not EdgeCompleted(e) and (e.vB = e.vL) then
      DoTriangulateLeft(e, e.vB, currY);

  if fUseDelaunay then
    // Convert triangles to Delaunay conforming
    while DelaunayPending.Pop(e) do ForceLegal(e);

  j := 0;
  SetLength(Result, triangles.Count);
  for i := 0 to triangles.Count -1 do
    with PTriangle(triangles[i])^ do
    begin
      Result[j] := PathFromTriangle(PTriangle(triangles[i]));
      cps := CrossProductSign(Result[j,0],Result[j,1],Result[j,2]);
      if cps = 0 then
        Continue  // skip any empty triangles
      else if cps < 0 then // ccw
        Result[j] := ReversePath(Result[j]);
      inc(j);
    end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function TDelaunay.ProcessVertArrayToNextLocMin(idx, len: integer;
  const va: TVertexArray): integer;
var
  nextI, prevI  : integer;
  v, nv, pv     : PVertex;
begin
  // precondition - 'idx' is the index of the previous locMin

  if idx = 0 then
    prevI := len -1 else
    prevI := idx -1;
  nextI := (idx + 1) mod len;
  v := @va[idx];
  nv := @va[nextI];
  pv := @va[prevI];
  if LeftTurning(pv, v, nv) then
    v.innerLM := true;
  Assert(v.pt.Y <> nv.pt.Y);
  CreateEdge(v, nv, ekAsc);

  idx := nextI;
  nextI := (idx + 1) mod len;
  // ascend to next locMax
  while va[nextI].pt.Y <= va[idx].pt.Y do
  begin
    CreateEdge(@va[idx], @va[nextI], ekAsc);
    idx := nextI;
    nextI := (idx + 1) mod len;
  end;
  // va[idx] is a locMax
  CreateEdge(@va[nextI], @va[idx], ekDesc);
  idx := nextI;
  nextI := (idx + 1) mod len;

  // descend to next locMin
  while va[nextI].pt.Y >= va[idx].pt.Y do
  begin
    CreateEdge(@va[nextI], @va[idx], ekDesc);
    idx := nextI;
    nextI := (idx + 1) mod len;
  end;
  Result := idx;
end;
//------------------------------------------------------------------------------

procedure TDelaunay.AddPath(const path: TPath64);

var
  oldPathCnt, oldVertCnt: integer;
  va: TVertexArray;

  procedure UndoAddPath;
  begin
    vertexList.Resize(oldVertCnt);
    SetLength(vertexArrays, oldPathCnt);
  end;

var
  i, i0, pathLen: integer;
  v, v0, vPrev: PVertex;
begin
  pathLen := Length(path);
  while (pathLen > 2) and PointsEqual(path[0], path[pathLen -1]) do
    dec(pathLen);
  if pathLen < 3 then Exit;

  oldPathCnt := Length(vertexArrays);
  oldVertCnt := vertexList.Count;
  SetLength(vertexArrays, oldPathCnt +1);
  SetLength(vertexArrays[oldPathCnt], pathLen);

  // assign coords to each vertex object, and link up .next and .prev
  va := vertexArrays[oldPathCnt];
  v0 := @va[0];

  vPrev := @va[pathLen -1]; // nb: must differ from va[0]
  vPrev.pt := path[pathLen -1];
  v := v0;
  for i := 0 to pathLen -1 do
  begin
    v.pt      := path[i];
    v.innerLM  := false;

    // skip duplicates and collinears
    if CrossProductIsZero(vPrev.pt,
      v.pt, path[(i + 1) mod pathLen]) then
        Continue;

    vertexList.Add(v);
    vPrev := v;
    inc(v);
  end;
  pathLen := vertexList.Count - oldVertCnt;

  // make sure we still have a valid (ie not almost non-zero area) path
  if (pathLen < 3) or       // too few vertices,
    ((pathLen = 3) and      // or just a very tiny triangle
    ((DistanceSqr(va[0].pt, va[1].pt) <= 1) or
     (DistanceSqr(va[1].pt, va[2].pt) <= 1) or
     (DistanceSqr(va[2].pt, va[0].pt) <= 1))) then
  begin
    UndoAddPath;
    Exit;
  end;

  // CAUTION: DON'T RESIZE 'va' BECAUSE THAT WOULD BREAK ALL
  // THE POINTERS IN VertexList, AS WELL AS vNext & vPrev!

  //find first ascending bound
  i := 1;
  while (i < pathLen) and (va[i].pt.Y >= va[i -1].pt.Y) do inc(i);
  if (i = pathLen) then
  begin
    dec(i);
    if (va[0].pt.Y >= va[i].pt.Y) then
    begin
      // path must be completely horizontal (ie with no area).
      UndoAddPath;
      Exit;
    end;
  end;

  // Assign vertex kinds (vkLocMin, vkAscend, vklocMax & vkDescend) ...
  i0 := FindFirstLocMin(i, pathLen, va);
  i := i0;
  while True do
  begin
    i := ProcessVertArrayToNextLocMin(i, pathLen, va);
    if i = i0 then break;
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
  if fixedEdgeList.Capacity < j then
    fixedEdgeList.Capacity := j;

  for i := 0 to High(paths) do
    AddPath(paths[i]);
end;
//------------------------------------------------------------------------------

function Triangulate(const paths: TPaths64; useDelaunay: Boolean = true): TPaths64;
var
  d: TDelaunay;
begin
  d := TDelaunay.Create(useDelaunay);
  try
    d.AddPaths(paths);
    Result := d.Triangulate;
  finally
    d.Free;
  end;
end;
//------------------------------------------------------------------------------

function Triangulate(const paths: TPathsD;
  decPlaces: integer; delaunay: Boolean = true): TPathsD;
var
  scale: double;
  pp64: TPaths64;
begin
  if decPlaces <= 0 then scale := 1
  else if decPlaces > 8 then scale := Power(10, 8)
  else scale := Power(10, decPlaces);
  pp64 := ScalePaths(paths, scale);
  pp64 := Triangulate(pp64, delaunay);
  Result := ScalePathsD(pp64, 1/scale);
end;
//------------------------------------------------------------------------------

function Triangulate(const paths: TPathsD; decPlaces: integer;
  useDelaunay: Boolean; out activeEdges: TPathsD): TPathsD;
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

  d := TDelaunay.Create(useDelaunay);
  try try
    d.AddPaths(pp64);
    pp64 := d.Triangulate;
    Result := ScalePathsD(pp64, 1/scale);

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
    Result := nil;
    activeEdges := nil;
  end;
  finally
    d.Free;
  end;
end;
//------------------------------------------------------------------------------

end.
