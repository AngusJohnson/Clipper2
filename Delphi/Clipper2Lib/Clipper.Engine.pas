unit Clipper.Engine;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - ver.1.0.4                                            *
* Date      :  10 September 2022                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This is the main polygon clipping module                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Classes, Math, Clipper.Core;

type
  //PathType:
  //  1. only subject paths may be open
  //  2. for closed paths, all boolean clipping operations except for
  //     Difference are commutative. (In other words, subjects and clips
  //     could be swapped and the same solution will be returned.)
  TPathType = (ptSubject, ptClip);

  // Vertex: a pre-clipping data structure. It is used to separate polygons
  // into ascending and descending 'bounds' (or sides) that start at local
  // minima and ascend to a local maxima, before descending again.

  TVertexFlag = (vfOpenStart, vfOpenEnd, vfLocMax, vfLocMin);
  TVertexFlags = set of TVertexFlag;

  PVertex = ^TVertex;
  TVertex = record
    pt    : TPoint64;
    next  : PVertex;
    prev  : PVertex;
    flags : TVertexFlags;
  end;

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    vertex    : PVertex;
    polytype  : TPathType;
    isOpen    : Boolean;
  end;

  // forward declarations
  POutRec = ^TOutRec;
  PJoiner = ^TJoiner;
  PActive = ^TActive;
  TPolyPathBase = class;
  TPolyTree64   = class;
  TPolyTreeD    = class;

  // OutPt: vertex data structure for clipping solutions
  POutPt = ^TOutPt;
  TOutPt = record
    pt       : TPoint64;
    next     : POutPt;
    prev     : POutPt;
    outrec   : POutRec;
    joiner   : PJoiner;
  end;

  TOutRecArray = array of POutRec;

  // OutRec: path data structure for clipping solutions
  TOutRec = record
    idx      : Integer;
    owner    : POutRec;
    splits   : TOutRecArray;
    frontE   : PActive;
    backE    : PActive;
    pts      : POutPt;
    polypath : TPolyPathBase;
    bounds   : TRect64;
    path     : TPath64;
    isOpen   : Boolean;
  end;

  // Joiner: structure used in merging "touching" solution polygons
  TJoiner = record
    idx   : integer;
    op1   : POutPt;
    op2   : POutPt;
    next1 : PJoiner;
    next2 : PJoiner;
    nextH : PJoiner;
  end;

  ///////////////////////////////////////////////////////////////////
  // Important: UP and DOWN here are premised on Y-axis positive down
  // displays, which is the orientation used in Clipper's development.
  ///////////////////////////////////////////////////////////////////

  // Active: represents an edge in the Active Edge Table (Vatti's AET)
  TActive = record
    bot      : TPoint64;
    top      : TPoint64;
    currX    : Int64;
    dx       : Double;        // inverse of edge slope (zero = vertical)
    windDx   : Integer;       // wind direction (ascending: +1; descending: -1)
    windCnt  : Integer;       // current wind count
    windCnt2 : Integer;       // current wind count of the opposite TPolyType
    outrec   : POutRec;
    // AEL: 'active edge list' (Vatti's AET - active edge table)
    //     a linked list of all edges (from left to right) that are present
    //     (or 'active') within the current scanbeam (a horizontal 'beam' that
    //     sweeps from bottom to top over the paths in the clipping operation).
    prevInAEL: PActive;
    nextInAEL: PActive;
    // SEL: 'sorted edge list' (Vatti's ST - sorted table)
    //     linked list used when sorting edges into their new positions at the
    //     top of scanbeams, but also (re)used to process horizontals.
    prevInSEL: PActive;
    nextInSEL: PActive;
    jump     : PActive;       // fast merge sorting (see BuildIntersectList())
    vertTop  : PVertex;
    locMin   : PLocalMinima;  // the bottom of an edge 'bound' (also Vatti)
    isLeftB  : Boolean;
  end;

  // IntersectNode: a structure representing 2 intersecting edges.
  // Intersections must be sorted so they are processed from the largest
  // Y coordinates to the smallest while keeping edges adjacent.
  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
    active1 : PActive;
    active2 : PActive;
    pt      : TPoint64;
  end;

  // Scanline: a virtual line representing current position
  // while processing edges using a "sweep line" algorithm.
  PScanLine = ^TScanLine;
  TScanLine = record
    y     : Int64;
    next  : PScanLine;
  end;

  {$IFDEF USINGZ}
  TZCallback64 = procedure (const bot1, top1, bot2, top2: TPoint64;
    var intersectPt: TPoint64) of object;
  TZCallbackD = procedure (const bot1, top1, bot2, top2: TPointD;
    var intersectPt: TPointD) of object;
  {$ENDIF}


  // ClipperBase: abstract base of Clipper class
  TClipperBase = class
  {$IFDEF STRICT}strict{$ENDIF} private
    FBotY               : Int64;
    FScanLine           : PScanLine;
    FCurrentLocMinIdx   : Integer;
    FClipType           : TClipType;
    FFillRule           : TFillRule;
    FPreserveCollinear  : Boolean;
    FIntersectList      : TList;
    FOutRecList         : TList;
    FLocMinList         : TList;
    FVertexArrayList    : TList;
    FJoinerList         : TList;
    // FActives: see AEL above
    FActives            : PActive;
    // FSel: see SEL above.
    //      BUT also used to store horz. edges for later processing
    FSel                : PActive;
    FHorzTrials         : PJoiner;
    FHasOpenPaths       : Boolean;
    FLocMinListSorted   : Boolean;
    FSucceeded          : Boolean;
    FReverseSolution    : Boolean;
  {$IFDEF USINGZ}
    fZCallback          : TZCallback64;
  {$ENDIF}
    procedure Reset;
    procedure InsertScanLine(const Y: Int64);
    function  PopScanLine(out Y: Int64): Boolean;
    function  PopLocalMinima(Y: Int64;
      out localMinima: PLocalMinima): Boolean;
    procedure DisposeScanLineList;
    procedure DisposeOutRecsAndJoiners;
    procedure DisposeVerticesAndLocalMinima;
    function  IsContributingClosed(e: PActive): Boolean;
    function  IsContributingOpen(e: PActive): Boolean;
    procedure SetWindCountForClosedPathEdge(e: PActive);
    procedure SetWindCountForOpenPathEdge(e: PActive);
    procedure InsertLocalMinimaIntoAEL(const botY: Int64);
    procedure InsertLeftEdge(e: PActive);
    procedure PushHorz(e: PActive); {$IFDEF INLINING} inline; {$ENDIF}
    function  PopHorz(out e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function  StartOpenPath(e: PActive; const pt: TPoint64): POutPt;
    procedure UpdateEdgeIntoAEL(var e: PActive);
    function  IntersectEdges(e1, e2: PActive; pt: TPoint64): POutPt;
    procedure DeleteFromAEL(e: PActive);
    procedure AdjustCurrXAndCopyToSEL(topY: Int64);
    procedure DoIntersections(const topY: Int64);
    procedure DisposeIntersectNodes;
    procedure AddNewIntersectNode(e1, e2: PActive; topY: Int64);
    function  BuildIntersectList(const topY: Int64): Boolean;
    procedure ProcessIntersectList;
    procedure SwapPositionsInAEL(e1, e2: PActive);
    function  AddOutPt(e: PActive; const pt: TPoint64): POutPt;
    function  AddLocalMinPoly(e1, e2: PActive;
      const pt: TPoint64; IsNew: Boolean = false): POutPt;
    function  AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64): POutPt;
    procedure JoinOutrecPaths(e1, e2: PActive);
    function  DoMaxima(e: PActive): PActive;
    procedure DoHorizontal(horzEdge: PActive);
    procedure DoTopOfScanbeam(Y: Int64);
    procedure UpdateOutrecOwner(outRec: POutRec);
    procedure AddTrialHorzJoin(op: POutPt);
    procedure DeleteTrialHorzJoin(op: POutPt);
    procedure ConvertHorzTrialsToJoins;
    procedure AddJoin(op1, op2: POutPt);
    procedure SafeDeleteOutPtJoiners(op: POutPt);
    procedure DeleteJoin(joiner: PJoiner);
    procedure ProcessJoinList;
    function  ProcessJoin(joiner: PJoiner): POutRec;
    function  ValidateClosedPathEx(var op: POutPt): Boolean;
    procedure CompleteSplit(op1, op2: POutPt; OutRec: POutRec);
    procedure SafeDisposeOutPts(var op: POutPt);
    procedure CleanCollinear(outRec: POutRec);
    procedure FixSelfIntersects(var op: POutPt);
  protected
    FUsingPolytree : Boolean;
    procedure AddPath(const path: TPath64;
      pathType: TPathType; isOpen: Boolean);
    procedure AddPaths(const paths: TPaths64;
      pathType: TPathType; isOpen: Boolean);
    procedure ClearSolution; // unlike Clear, CleanUp preserves added paths
    procedure ExecuteInternal(clipType: TClipType;
      fillRule: TFillRule; usingPolytree: Boolean);
    function DeepCheckOwner(outrec, owner: POutRec): Boolean;
    function  BuildPaths(out closedPaths, openPaths: TPaths64): Boolean;
    procedure BuildTree(polytree: TPolyPathBase; out openPaths: TPaths64);
  {$IFDEF USINGZ}
    procedure SetZ( e1, e2: PActive; var intersectPt: TPoint64);
    property  ZCallback : TZCallback64 read fZCallback write fZCallback;
  {$ENDIF}
    property  Succeeded : Boolean read FSucceeded;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function GetBounds: TRect64;
    property PreserveCollinear: Boolean read
      FPreserveCollinear write FPreserveCollinear;
    property ReverseSolution: Boolean read
      FReverseSolution write FReverseSolution;
  end;

  TClipper64 = class(TClipperBase) // for integer coordinates
  public
    procedure AddSubject(const subject: TPath64); overload;
    procedure AddSubject(const subjects: TPaths64); overload;
    procedure AddOpenSubject(const subject: TPath64); overload;
    procedure AddOpenSubject(const subjects: TPaths64); overload;
    procedure AddClip(const clip: TPath64); overload;
    procedure AddClip(const clips: TPaths64); overload;
    function  Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions: TPaths64): Boolean; overload; virtual;
    function  Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions, openSolutions: TPaths64): Boolean; overload; virtual;
    function  Execute(clipType: TClipType; fillRule: TFillRule;
      var solutionTree: TPolyTree64; out openSolutions: TPaths64): Boolean; overload; virtual;
  {$IFDEF USINGZ}
    property  ZCallback;
  {$ENDIF}
  end;

  // PolyPathBase: ancestor of TPolyPath and TPolyPathD
  TPolyPathBase = class
  {$IFDEF STRICT}strict{$ENDIF} private
    FParent     : TPolyPathBase;
    FChildList  : TList;
    function    GetChildCnt: Integer;
    function    GetIsHole: Boolean;
  protected
    function    GetChild(index: Integer): TPolyPathBase;
    function    AddChild(const path: TPath64): TPolyPathBase; virtual; abstract;
    property    ChildList: TList read FChildList;
    property    Parent: TPolyPathBase read FParent write FParent;
  public
    constructor Create;  virtual;
    destructor  Destroy; override;
    procedure   Clear; virtual;
    property    IsHole: Boolean read GetIsHole;
    property    Count: Integer read GetChildCnt;
    property    Child[index: Integer]: TPolyPathBase read GetChild; default;
  end;

  TPolyPath64 = class(TPolyPathBase)
  {$IFDEF STRICT}strict{$ENDIF} private
    FPath : TPath64;
    function    GetChild64(index: Integer): TPolyPath64;
  protected
    function AddChild(const path: TPath64): TPolyPathBase; override;
  public
    property Child[index: Integer]: TPolyPath64 read GetChild64; default;
    property Polygon: TPath64 read FPath;
  end;

  // PolyTree: is intended as a READ-ONLY data structure to receive closed path
  // solutions to clipping operations. While this structure is more complex than
  // the alternative TPaths structure, it does model path ownership (ie paths
  // that are contained by other paths). This will be useful to some users.
  TPolyTree64 = class(TPolyPath64);

  // FLOATING POINT POLYGON COORDINATES (D suffix to indicate double precision)
  // To preserve numerical robustness, clipping must be done using integer
  // coordinates. Consequently, polygons that are defined with floating point
  // coordinates will need these converted into integer values together with
  // scaling to achieve the desired floating point precision.

  TClipperD = class(TClipperBase) // for floating point coordinates
  {$IFDEF STRICT}strict{$ENDIF} private
    FScale: double;
    FInvScale: double;
  {$IFDEF USINGZ}
    fZCallback : TZCallbackD;
    procedure ZCB(const bot1, top1, bot2, top2: TPoint64;
      var intersectPt: TPoint64);
    procedure CheckCallback;
  {$ENDIF}
  public
    procedure AddSubject(const pathD: TPathD); overload;
    procedure AddSubject(const pathsD: TPathsD); overload;
    procedure AddOpenSubject(const pathD: TPathD); overload;
    procedure AddOpenSubject(const pathsD: TPathsD); overload;
    procedure AddClip(const pathD: TPathD); overload;
    procedure AddClip(const pathsD: TPathsD); overload;
    constructor Create(roundingDecimalPrecision: integer = 2);
      reintroduce; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions: TPathsD): Boolean; overload;
    function  Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions, openSolutions: TPathsD): Boolean; overload;
    function  Execute(clipType: TClipType; fillRule: TFillRule;
      var solutionsTree: TPolyTreeD; out openSolutions: TPathsD): Boolean; overload;
{$IFDEF USINGZ}
    property  ZCallback : TZCallbackD read fZCallback write fZCallback;
{$ENDIF}
  end;

  TPolyPathD = class(TPolyPathBase)
  {$IFDEF STRICT}strict{$ENDIF} private
    FPath   : TPathD;
    function  GetChildD(index: Integer): TPolyPathD;
  protected
    FScale  : double;
    function  AddChild(const path: TPath64): TPolyPathBase; override;
  public
    property  Polygon: TPathD read FPath;
    property Child[index: Integer]: TPolyPathD read GetChildD; default;
  end;

  TPolyTreeD = class(TPolyPathD)
  protected
    procedure SetScale(value: double); // alternative to friend class
  public
    property  Scale: double read FScale;
  end;

resourcestring
  rsClipper_RoundingErr = 'The decimal rounding value is invalid';

implementation

//OVERFLOWCHECKS OFF is a necessary workaround for a compiler bug that very
//occasionally reports incorrect overflow errors in Delphi versions before 10.2.
//see https://forums.embarcadero.com/message.jspa?messageID=871444
{$OVERFLOWCHECKS OFF}

resourcestring
  rsClipper_PolyTreeErr = 'The TPolyTree parameter must be assigned.';
  rsClipper_ClippingErr = 'Undefined clipping error';

const
  DefaultClipperDScale = 100;

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

function UnsafeGet(List: TList; Index: Integer): Pointer;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := List.List[Index];
end;
//------------------------------------------------------------------------------

function IsOpen(e: PActive): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.locMin.isOpen;
end;
//------------------------------------------------------------------------------

function IsOpenEnd(e: PActive): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.locMin.isOpen and
    (e.vertTop.flags * [vfOpenStart, vfOpenEnd] <> []);
end;
//------------------------------------------------------------------------------

function IsHotEdge(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := assigned(e.outrec);
end;
//------------------------------------------------------------------------------

function GetPrevHotEdge(e: PActive): PActive; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.prevInAEL;
  while assigned(Result) and (IsOpen(Result) or not IsHotEdge(Result)) do
    Result := Result.prevInAEL;
end;
//------------------------------------------------------------------------------

function IsFront(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e = e.outrec.frontE);
end;
//------------------------------------------------------------------------------

function IsValidPath(op: POutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := assigned(op) and (op.next <> op);
end;
//------------------------------------------------------------------------------

function AreReallyClose(const pt1, pt2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (abs(pt1.X - pt2.X) < 2) and (abs(pt1.Y - pt2.Y) < 2);
end;
//------------------------------------------------------------------------------

function IsValidClosedPath(op: POutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := assigned(op) and
    (op.next <> op) and (op.next <> op.prev) and not
			//also treat inconsequential polygons as invalid
      ((op.next.next = op.prev) and
        (AreReallyClose(op.pt, op.next.pt) or
        AreReallyClose(op.pt, op.prev.pt)));
end;
//------------------------------------------------------------------------------

(*******************************************************************************
*  Dx:                             0(90deg)                                    *
*                                  |                                           *
*               +inf (180deg) <--- o ---> -inf (0deg)                          *
*******************************************************************************)

function GetDx(const pt1, pt2: TPoint64): double;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dy: Int64;
begin
  dy := (pt2.Y - pt1.Y);
  if dy <> 0 then result := (pt2.X - pt1.X) / dy
  else if (pt2.X > pt1.X) then result := NegInfinity
  else result := Infinity;
end;
//------------------------------------------------------------------------------

function TopX(e: PActive; const currentY: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (currentY = e.top.Y) or (e.top.X = e.bot.X) then Result := e.top.X
  else if (currentY = e.bot.Y) then Result := e.bot.X
  else Result := e.bot.X + Round(e.dx*(currentY - e.bot.Y));
end;
//------------------------------------------------------------------------------

function IsHorizontal(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.top.Y = e.bot.Y);
end;
//------------------------------------------------------------------------------

function IsHeadingRightHorz(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.dx = NegInfinity);
end;
//------------------------------------------------------------------------------

function IsHeadingLeftHorz(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.dx = Infinity);
end;
//------------------------------------------------------------------------------

procedure SwapActives(var e1, e2: PActive); {$IFDEF INLINING} inline; {$ENDIF}
var
  e: PActive;
begin
  e := e1; e1 := e2; e2 := e;
end;
//------------------------------------------------------------------------------

function GetPolyType(const e: PActive): TPathType;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.locMin.polytype;
end;
//------------------------------------------------------------------------------

function IsSamePolyType(const e1, e2: PActive): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e1.locMin.polytype = e2.locMin.polytype;
end;
//------------------------------------------------------------------------------

function GetIntersectPoint(e1, e2: PActive): TPoint64;
var
  b1, b2, m: Double;
begin
  if (e1.dx = e2.dx) then
  begin
    Result := e1.top;
    Exit;
  end
  else if e1.dx = 0 then
  begin
    Result.X := e1.bot.X;
    if IsHorizontal(e2) then
      Result.Y := e2.bot.Y
    else
    begin
      with e2^ do b2 := bot.Y - (bot.X/dx);
      Result.Y := round(Result.X/e2.dx + b2);
    end;
  end
  else if e2.dx = 0 then
  begin
    Result.X := e2.bot.X;
    if IsHorizontal(e1) then
      Result.Y := e1.bot.Y
    else
    begin
      with e1^ do b1 := bot.Y - (bot.X/dx);
      Result.Y := round(Result.X/e1.dx + b1);
    end;
  end else
  begin
    with e1^ do b1 := bot.X - bot.Y * dx;
    with e2^ do b2 := bot.X - bot.Y * dx;
    m := (b2-b1)/(e1.dx - e2.dx);
    Result.Y := round(m);
    if Abs(e1.dx) < Abs(e2.dx) then
      Result.X := round(e1.dx * m + b1) else
      Result.X := round(e2.dx * m + b2);
  end;
end;
//------------------------------------------------------------------------------

procedure SetDx(e: PActive);  {$IFDEF INLINING} inline; {$ENDIF}
begin
  e.dx := GetDx(e.bot, e.top);
end;
//------------------------------------------------------------------------------

function IsLeftBound(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.isLeftB;
end;
//------------------------------------------------------------------------------

function NextVertex(e: PActive): PVertex; // ie heading (inverted Y-axis) "up"
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if e.windDx > 0 then
    Result := e.vertTop.next else
    Result := e.vertTop.prev;
end;
//------------------------------------------------------------------------------

//PrevPrevVertex: useful to get the (inverted Y-axis) top of the
//alternate edge (ie left or right bound) during edge insertion.
function PrevPrevVertex(e: PActive): PVertex;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if e.windDx > 0 then
    Result := e.vertTop.prev.prev else
    Result := e.vertTop.next.next;
end;
//------------------------------------------------------------------------------

function IsMaxima(vertex: PVertex): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := vfLocMax in vertex.flags;
end;
//------------------------------------------------------------------------------

function IsMaxima(e: PActive): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := vfLocMax in e.vertTop.flags;
end;
//------------------------------------------------------------------------------

function GetCurrYMaximaVertex(e: PActive): PVertex;
begin
  // nb: function not safe with open paths
  Result := e.vertTop;
  if e.windDx > 0 then
    while Result.next.pt.Y = Result.pt.Y do  Result := Result.next
  else
    while Result.prev.pt.Y = Result.pt.Y do  Result := Result.prev;
  if not IsMaxima(Result) then Result := nil; // not a maxima
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PActive): PActive;
begin
  Result := e.nextInAEL;
  while assigned(Result) do
  begin
    if Result.vertTop = e.vertTop then Exit;  // Found!
    Result := Result.nextInAEL;
  end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function GetHorzMaximaPair(horz: PActive; maxVert: PVertex): PActive;
begin
  // we can't be sure whether the MaximaPair is on the left or right, so ...
  Result := horz.prevInAEL;
  while assigned(Result) and (Result.currX >= maxVert.pt.X) do
  begin
    if Result.vertTop = maxVert then Exit;  // Found!
    Result := Result.prevInAEL;
  end;
  Result := horz.nextInAEL;
  while assigned(Result) and (TopX(Result, horz.top.Y) <= maxVert.pt.X) do
  begin
    if Result.vertTop = maxVert then Exit;  // Found!
    Result := Result.nextInAEL;
  end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function PointCount(pts: POutPt): Integer; {$IFDEF INLINING} inline; {$ENDIF}
var
  p: POutPt;
begin
  Result := 0;
  if not Assigned(pts) then Exit;
  p := pts;
  repeat
    Inc(Result);
    p := p.next;
  until p = pts;
end;
//------------------------------------------------------------------------------

function GetRealOutRec(outRec: POutRec): POutRec;
 {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outRec;
  while Assigned(Result) and not Assigned(Result.pts) do
    Result := Result.owner;
end;
//------------------------------------------------------------------------------

procedure UncoupleOutRec(e: PActive);
var
  outRec: POutRec;
begin
  if not Assigned(e.outrec) then Exit;
  outRec := e.outrec;
  outRec.frontE.outrec := nil;
  outRec.backE.outrec := nil;
  outRec.frontE := nil;
  outRec.backE := nil;
end;
//------------------------------------------------------------------------------

procedure AddPathsToVertexList(const paths: TPaths64;
  polyType: TPathType; isOpen: Boolean;
  vertexList, LocMinList: TList);
var
  i, j, len, totalVerts: integer;
  p: PPoint64;
  v, va0, vaCurr, vaPrev: PVertex;
  ascending, ascending0: Boolean;

  procedure AddLocMin(vert: PVertex);
  var
    lm: PLocalMinima;
  begin
    if vfLocMin in vert.flags then Exit;  // ie already added
    Include(vert.flags, vfLocMin);
    new(lm);
    lm.vertex := vert;
    lm.polytype := polyType;
    lm.isOpen := isOpen;
    LocMinList.Add(lm);                  // nb: sorted in Reset()
  end;
  //---------------------------------------------------------

begin
  // count the total (maximum) number of vertices required
  totalVerts := 0;
  for i := 0 to High(paths) do
    totalVerts := totalVerts + Length(paths[i]);
  if (totalVerts = 0) then Exit;
  // allocate memory
  GetMem(v, sizeof(TVertex) * totalVerts);
  vertexList.Add(v);

  for i := 0 to High(paths) do
  begin
    len := Length(paths[i]);
    if len = 0 then Continue;
    p := @paths[i][0];
    va0 := v; vaCurr := v;
    vaCurr.pt := p^;
    vaCurr.prev := nil;
    inc(p);
    vaCurr.flags := [];
    vaPrev := vaCurr;
    inc(vaCurr);
    for j := 1 to len -1 do
    begin
      if PointsEqual(vaPrev.pt, p^) then
      begin
        inc(p);
        Continue; // skips duplicates
      end;
      vaPrev.next := vaCurr;
      vaCurr.prev := vaPrev;
      vaCurr.pt := p^;
      vaCurr.flags := [];
      vaPrev := vaCurr;
      inc(vaCurr);
      inc(p);
    end;
    if not Assigned(vaPrev.prev) then Continue;
    if not isOpen and PointsEqual(vaPrev.pt, va0.pt) then
      vaPrev := vaPrev.prev;

    vaPrev.next := va0;
    va0.prev := vaPrev;
    v := vaCurr; // ie get ready for next path
    if isOpen and (va0.next = va0) then Continue;

    // now find and assign local minima
    if (isOpen) then
    begin
      vaCurr := va0.next;
      while (vaCurr <> va0) and (vaCurr.pt.Y = va0.pt.Y) do
        vaCurr := vaCurr.next;
      ascending := vaCurr.pt.Y <= va0.pt.Y;
      if (ascending) then
      begin
        va0.flags := [vfOpenStart];
        AddLocMin(va0);
      end
      else
        va0.flags := [vfOpenStart, vfLocMax];
    end else
    begin
      // closed path
      vaPrev := va0.prev;
      while (vaPrev <> va0) and (vaPrev.pt.Y = va0.pt.Y) do
        vaPrev := vaPrev.prev;
      if (vaPrev = va0) then
        Continue; // only open paths can be completely flat
      ascending := vaPrev.pt.Y > va0.pt.Y;
    end;

    ascending0 := ascending;
    vaPrev := va0;
    vaCurr := va0.next;
    while (vaCurr <> va0) do
    begin
      if (vaCurr.pt.Y > vaPrev.pt.Y) and ascending then
      begin
        Include(vaPrev.flags, vfLocMax);
        ascending := false;
      end
      else if (vaCurr.pt.Y < vaPrev.pt.Y) and not ascending then
      begin
        ascending := true;
        AddLocMin(vaPrev);
      end;
      vaPrev := vaCurr;
      vaCurr := vaCurr.next;
    end;

    if (isOpen) then
    begin
      Include(vaPrev.flags, vfOpenEnd);
      if ascending then
        Include(vaPrev.flags, vfLocMax) else
        AddLocMin(vaPrev);
    end
    else if (ascending <> ascending0) then
    begin
      if (ascending0) then AddLocMin(vaPrev)
      else Include(vaPrev.flags, vfLocMax);
    end;
  end;
end;
//------------------------------------------------------------------------------

function BuildPath(op: POutPt; reverse, isOpen: Boolean;
  out path: TPath64): Boolean;
var
  i,j, cnt: integer;
begin
  cnt := PointCount(op);
  if (cnt < 3) and (not isOpen or (Cnt < 2)) then
  begin
    Result := false;
    Exit;
  end;

  setLength(path, cnt);
  if reverse then
  begin
    path[0] := op.pt;
    op := op.prev;
  end else
  begin
    op := op.next;
    path[0] := op.pt;
    op := op.next;
  end;
  j := 0;
  for i := 0 to cnt -2 do
  begin
    if not PointsEqual(path[j], op.pt) then
    begin
      inc(j);
      path[j] := op.pt;
    end;
    if reverse then op := op.prev else op := op.next;
  end;

  setLength(path, j+1);
  if isOpen then
    Result := (j > 0) else
    Result := (j > 1);
end;
//------------------------------------------------------------------------------

function DisposeOutPt(op: POutPt): POutPt;
begin
  if op.next = op then
    Result := nil else
    Result := op.next;
  op.prev.next := op.next;
  op.next.prev := op.prev;
  Dispose(Op);
end;
//------------------------------------------------------------------------------

procedure DisposeOutPts(op: POutPt); {$IFDEF INLINING} inline; {$ENDIF}
var
  tmpPp: POutPt;
begin
  op.prev.next := nil;
  while Assigned(op) do
  begin
    tmpPp := op;
    op := op.next;
    Dispose(tmpPp);
  end;
end;
//------------------------------------------------------------------------------

function LocMinListSort(item1, item2: Pointer): Integer;
var
  q: Int64;
  lm1: PLocalMinima absolute item1;
  lm2: PLocalMinima absolute item2;
begin
  q := lm2.vertex.pt.Y - lm1.vertex.pt.Y;
  if q < 0 then
    Result := -1
  else if q > 0 then
    Result := 1
  else
  begin
    q := lm2.vertex.pt.X - lm1.vertex.pt.X;
    if q < 0 then Result := 1
    else if q > 0 then Result := -1
    else Result := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure SetSides(outRec: POutRec; startEdge, endEdge: PActive);
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  outRec.frontE := startEdge;
  outRec.backE := endEdge;
end;
//------------------------------------------------------------------------------

procedure SwapOutRecs(e1, e2: PActive);
var
  or1, or2: POutRec;
  e: PActive;
begin
  or1 := e1.outrec;
  or2 := e2.outrec;
  if (or1 = or2) then
  begin
    // nb: at least one edge is 'hot'
    e := or1.frontE;
    or1.frontE := or1.backE;
    or1.backE := e;
    Exit;
  end;
  if assigned(or1) then
  begin
    if e1 = or1.frontE then
      or1.frontE := e2 else
      or1.backE := e2;
  end;
  if assigned(or2) then
  begin
    if e2 = or2.frontE then
      or2.frontE := e1 else
      or2.backE := e1;
  end;
  e1.outrec := or2;
  e2.outrec := or1;
end;
//------------------------------------------------------------------------------

function Area(op: POutPt): Double;
var
  op2: POutPt;
  d: double;
begin
  // https://en.wikipedia.org/wiki/Shoelace_formula
  Result := 0;
  if not Assigned(op) then Exit;
  op2 := op;
  repeat
    d := (op2.prev.pt.Y + op2.pt.Y);
    Result := Result + d * (op2.prev.pt.X - op2.pt.X);
    op2 := op2.next;
  until op2 = op;
  Result := Result * 0.5;
end;
//------------------------------------------------------------------------------

function AreaTriangle(const pt1, pt2, pt3: TPoint64): double;
var
  d1,d2,d3,d4,d5,d6: double;
begin
  d1 := (pt3.y + pt1.y);
  d2 := (pt3.x - pt1.x);
  d3 := (pt1.y + pt2.y);
  d4 := (pt1.x - pt2.x);
  d5 := (pt2.y + pt3.y);
  d6 := (pt2.x - pt3.x);
  result := d1 * d2 + d3 *d4 + d5 *d6;
end;
//------------------------------------------------------------------------------

procedure ReverseOutPts(op: POutPt);
var
  op1, op2: POutPt;
begin
  if not Assigned(op) then Exit;
  op1 := op;
  repeat
    op2:= op1.next;
    op1.next := op1.prev;
    op1.prev := op2;
    op1 := op2;
  until op1 = op;
end;
//------------------------------------------------------------------------------

function OutrecIsAscending(hotEdge: PActive): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (hotEdge = hotEdge.outrec.frontE);
end;
//------------------------------------------------------------------------------

procedure SwapFrontBackSides(outRec: POutRec); {$IFDEF INLINING} inline; {$ENDIF}
var
  e2: PActive;
begin
  // while this proc. is needed for open paths
  // it's almost never needed for closed paths
  e2 := outRec.frontE;
  outRec.frontE := outRec.backE;
  outRec.backE := e2;
  outRec.pts := outRec.pts.next;
end;
//------------------------------------------------------------------------------

function EdgesAdjacentInAEL(node: PIntersectNode): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  active1, active2: PActive;
begin
  active1 := node.active1;
  active2 := node.active2;
  Result := (active1.nextInAEL = active2) or (active1.prevInAEL = active2);
end;
//------------------------------------------------------------------------------

function TestJoinWithPrev1(e: PActive): Boolean;
begin
  // this is marginally quicker than TestJoinWithPrev2
  // but can only be used when e.PrevInAEL.currX is accurate
  Result := IsHotEdge(e) and not IsOpen(e) and
    Assigned(e.prevInAEL) and (e.prevInAEL.currX = e.currX) and
    IsHotEdge(e.prevInAEL) and not IsOpen(e.prevInAEL) and
    (CrossProduct(e.prevInAEL.top, e.bot, e.top) = 0);
end;
//------------------------------------------------------------------------------

function TestJoinWithPrev2(e: PActive; const currPt: TPoint64): Boolean;
begin
  Result := IsHotEdge(e) and not IsOpen(e) and
    Assigned(e.prevInAEL) and not IsOpen(e.prevInAEL) and
    IsHotEdge(e.prevInAEL) and
    (Abs(TopX(e.prevInAEL, currPt.Y) - currPt.X) < 2) and
    (e.prevInAEL.top.Y < currPt.Y) and
    (CrossProduct(e.prevInAEL.top, currPt, e.top) = 0);
end;
//------------------------------------------------------------------------------

function TestJoinWithNext1(e: PActive): Boolean;
begin
  // this is marginally quicker than TestJoinWithNext2
  // but can only be used when e.NextInAEL.currX is accurate
  Result := IsHotEdge(e) and Assigned(e.nextInAEL) and
    IsHotEdge(e.nextInAEL) and not IsOpen(e) and
    not IsOpen(e.nextInAEL) and
    (e.nextInAEL.currX = e.currX) and
    (CrossProduct(e.nextInAEL.top, e.bot, e.top) = 0);
end;
//------------------------------------------------------------------------------

function TestJoinWithNext2(e: PActive; const currPt: TPoint64): Boolean;
begin
  Result := IsHotEdge(e) and Assigned(e.nextInAEL) and
    IsHotEdge(e.nextInAEL) and not IsOpen(e) and
    not IsOpen(e.nextInAEL) and
    (Abs(TopX(e.nextInAEL, currPt.Y) - currPt.X) < 2) and                   // safer
    (e.nextInAEL.top.Y < currPt.Y) and
    (CrossProduct(e.nextInAEL.top, currPt, e.top) = 0);
end;
//------------------------------------------------------------------------------

function GetHorzTrialParent(op: POutPt): PJoiner;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := op.joiner;
  while Assigned(Result) do
    if Result.op1 = op then
    begin
      if Assigned(Result.next1) and
        (Result.next1.idx < 0) then Exit
      else Result := Result.next1;
    end else
    begin
      if Assigned(Result.next2) and
        (Result.next2.idx < 0) then Exit
      else Result := Result.next1;
    end;
end;
//------------------------------------------------------------------------------

function MakeDummyJoiner(horz: POutPt; nextJoiner: PJoiner): PJoiner;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  new(Result);
  Result.idx := -1;
  Result.op1 := horz;
  Result.op2 := nil;
  Result.next1 := horz.joiner;
  horz.joiner := Result;
  Result.next2 := nil;
  Result.nextH := nextJoiner;
end;
//------------------------------------------------------------------------------

function OutPtInTrialHorzList(op: POutPt): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := Assigned(op.joiner) and
    ((op.joiner.idx < 0) or Assigned(GetHorzTrialParent(op)));
end;
//------------------------------------------------------------------------------

function InsertOp(const pt: TPoint64; insertAfter: POutPt): POutPt;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  new(Result);
  Result.pt := pt;
  Result.joiner := nil;
  Result.outrec := insertAfter.outrec;
  Result.next := insertAfter.next;
  insertAfter.next.prev := Result;
  insertAfter.next := Result;
  Result.prev := insertAfter;
end;

//------------------------------------------------------------------------------
// TClipperBase methods ...
//------------------------------------------------------------------------------

constructor TClipperBase.Create;
begin
  FLocMinList       := TList.Create;
  FOutRecList       := TList.Create;
  FJoinerList       := TList.Create;
  FIntersectList    := TList.Create;
  FVertexArrayList  := TList.Create;
  FPreserveCollinear  := true;
  FReverseSolution    := false;
end;
//------------------------------------------------------------------------------

destructor TClipperBase.Destroy;
begin
  Clear;
  FLocMinList.Free;
  FOutRecList.Free;
  FJoinerList.Free;
  FIntersectList.Free;
  FVertexArrayList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.ClearSolution;
var
  dummy: Int64;
begin
  try
    // in case of exceptions ...
    while assigned(FActives) do DeleteFromAEL(FActives);
    while assigned(FScanLine) do PopScanLine(dummy);
    DisposeIntersectNodes;

    DisposeScanLineList;
    DisposeOutRecsAndJoiners;
    FHorzTrials := nil;
  except
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Clear;
begin
  ClearSolution;
  DisposeVerticesAndLocalMinima;
  FCurrentLocMinIdx := 0;
  FLocMinListSorted := false;
  FHasOpenPaths := False;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Reset;
var
  i: Integer;
begin
  if not FLocMinListSorted then
  begin
    FLocMinList.Sort(LocMinListSort);
    FLocMinListSorted := true;
  end;

  for i := FLocMinList.Count -1 downto 0 do
    InsertScanLine(PLocalMinima(UnsafeGet(FLocMinList, i)).vertex.pt.Y);
  FCurrentLocMinIdx := 0;
  FActives := nil;
  FSel := nil;
  FSucceeded := true;
end;
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
function XYCoordsEqual(const pt1, pt2: TPoint64): Boolean;
begin
  Result := (pt1.X = pt2.X) and (pt1.Y = pt2.Y);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SetZ(e1, e2: PActive; var intersectPt: TPoint64);
begin
  if not Assigned(fZCallback) then Exit;

  // prioritize subject vertices over clip vertices
  // and pass the subject vertices before clip vertices in the callback
  if (GetPolyType(e1) = ptSubject) then
  begin
    if (XYCoordsEqual(intersectPt, e1.bot)) then intersectPt.Z := e1.bot.Z
    else if (XYCoordsEqual(intersectPt, e1.top)) then intersectPt.Z := e1.top.Z
    else if (XYCoordsEqual(intersectPt, e2.bot)) then intersectPt.Z := e2.bot.Z
    else if (XYCoordsEqual(intersectPt, e2.top)) then intersectPt.Z := e2.top.Z;
    fZCallback(e1.bot, e1.top, e2.bot, e2.top, intersectPt);
  end else
  begin
    if (XYCoordsEqual(intersectPt, e2.bot)) then intersectPt.Z := e2.bot.Z
    else if (XYCoordsEqual(intersectPt, e2.top)) then intersectPt.Z := e2.top.Z
    else if (XYCoordsEqual(intersectPt, e1.bot)) then intersectPt.Z := e1.bot.Z
    else if (XYCoordsEqual(intersectPt, e1.top)) then intersectPt.Z := e1.top.Z;
    fZCallback(e2.bot, e2.top, e1.bot, e1.top, intersectPt);
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure TClipperBase.InsertScanLine(const Y: Int64);
var
  newSl, sl: PScanLine;
begin
  // The scanline list is a single-linked list of all the Y coordinates of
  // subject and clip vertices in the clipping operation (sorted descending).
  // However, only scanline Y's at Local Minima are inserted before clipping
  // starts. While scanlines are removed sequentially during the sweep operation,
  // new scanlines are only inserted whenever edge bounds are updated. This keeps
  // the scanline list relatively short, optimising performance.
  if not Assigned(FScanLine) then
  begin
    new(newSl);
    newSl.y := Y;
    FScanLine := newSl;
    newSl.next := nil;
  end else if Y > FScanLine.y then
  begin
    new(newSl);
    newSl.y := Y;
    newSl.next := FScanLine;
    FScanLine := newSl;
  end else
  begin
    sl := FScanLine;
    while Assigned(sl.next) and (Y <= sl.next.y) do
      sl := sl.next;
    if Y = sl.y then Exit; // skip duplicates
    new(newSl);
    newSl.y := Y;
    newSl.next := sl.next;
    sl.next := newSl;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.PopScanLine(out Y: Int64): Boolean;
var
  sl: PScanLine;
begin
  Result := assigned(FScanLine);
  if not Result then Exit;
  Y := FScanLine.y;
  sl := FScanLine;
  FScanLine := FScanLine.next;
  dispose(sl);
end;
//------------------------------------------------------------------------------

function TClipperBase.PopLocalMinima(Y: Int64;
  out localMinima: PLocalMinima): Boolean;
begin
  Result := false;
  if FCurrentLocMinIdx = FLocMinList.Count then Exit;
  localMinima := PLocalMinima(UnsafeGet(FLocMinList, FCurrentLocMinIdx));
  if (localMinima.vertex.pt.Y = Y) then
  begin
    inc(FCurrentLocMinIdx);
    Result := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeScanLineList;
var
  sl: PScanLine;
begin
  while Assigned(FScanLine) do
  begin
    sl := FScanLine.next;
    Dispose(FScanLine);
    FScanLine := sl;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeOutRecsAndJoiners;
var
  i: Integer;
  outrec: POutRec;
begin
  // just in case joiners haven't already been disposed
  for i := 0 to FJoinerList.Count -1 do
    if Assigned(UnsafeGet(FJoinerList, i)) then
      Dispose(PJoiner(UnsafeGet(FJoinerList, i)));
  FJoinerList.Clear;
  FHorzTrials := nil;

  for i := 0 to FOutRecList.Count -1 do
  begin
    outrec := UnsafeGet(FOutRecList, i);
    if Assigned(outrec.pts) then DisposeOutPts(outrec.pts);
    Dispose(outrec);
  end;
  FOutRecList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeVerticesAndLocalMinima;
var
  i: Integer;
begin
  for i := 0 to FLocMinList.Count -1 do
    Dispose(PLocalMinima(UnsafeGet(FLocMinList, i)));
  FLocMinList.Clear;
  for i := 0 to FVertexArrayList.Count -1 do
    FreeMem(UnsafeGet(FVertexArrayList, i));
  FVertexArrayList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddPath(const path: TPath64;
  pathType: TPathType; isOpen: Boolean);
var
  pp: TPaths64;
begin
  SetLength(pp, 1);
  pp[0] := path;
  AddPaths(pp, pathType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddPaths(const paths: TPaths64;
  pathType: TPathType; isOpen: Boolean);
begin
  if isOpen then FHasOpenPaths := true;
  FLocMinListSorted := false;
  AddPathsToVertexList(paths, pathType, isOpen,
    FVertexArrayList, FLocMinList);
end;
//------------------------------------------------------------------------------

function TClipperBase.IsContributingClosed(e: PActive): Boolean;
begin
  Result := false;
  case FFillRule of
    frNonZero: if abs(e.windCnt) <> 1 then Exit;
    frPositive: if (e.windCnt <> 1) then Exit;
    frNegative: if (e.windCnt <> -1) then Exit;
  end;

  case FClipType of
    ctIntersection:
      case FFillRule of
        frPositive: Result := (e.windCnt2 > 0);
        frNegative: Result := (e.windCnt2 < 0);
        else Result := (e.windCnt2 <> 0);
      end;
    ctUnion:
      case FFillRule of
        frPositive: Result := (e.windCnt2 <= 0);
        frNegative: Result := (e.windCnt2 >= 0);
        else Result := (e.windCnt2 = 0);
      end;
    ctDifference:
      begin
        case FFillRule of
          frPositive: Result := (e.windCnt2 <= 0);
          frNegative: Result := (e.windCnt2 >= 0);
          else Result := (e.windCnt2 = 0);
        end;
        if GetPolyType(e) <> ptSubject then Result := not Result;
      end;
    ctXor:
        Result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.IsContributingOpen(e: PActive): Boolean;
var
  isInSubj, isInClip: Boolean;
begin
    case FFillRule of
      frPositive:
        begin
          isInSubj := e.windCnt > 0;
          isInClip := e.windCnt2 > 0;
        end;
      frNegative:
        begin
          isInSubj := e.windCnt < 0;
          isInClip := e.windCnt2 < 0;
        end;
      else
        begin
          isInSubj := e.windCnt <> 0;
          isInClip := e.windCnt2 <> 0;
        end;
    end;

    case FClipType of
      ctIntersection: Result := isInClip;
      ctUnion: Result := not isInSubj and not isInClip;
      else Result := not isInClip;
    end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SetWindCountForClosedPathEdge(e: PActive);
var
  e2: PActive;
begin
  // Wind counts refer to polygon regions not edges, so here an edge's WindCnt
  // indicates the higher of the wind counts for the two regions touching the
  // edge. (nb: Adjacent regions can only ever have their wind counts differ by
  // one. Also, open paths have no meaningful wind directions or counts.)

  e2 := e.prevInAEL;
  // find the nearest closed path edge of the same PolyType in AEL (heading left)
  while Assigned(e2) and (not IsSamePolyType(e2, e) or IsOpen(e2)) do
    e2 := e2.prevInAEL;

  if not Assigned(e2) then
  begin
    e.windCnt := e.windDx;
    e2 := FActives;
  end
  else if (FFillRule = frEvenOdd) then
  begin
    e.windCnt := e.windDx;
    e.windCnt2 := e2.windCnt2;
    e2 := e2.nextInAEL;
  end else
  begin
    // NonZero, positive, or negative filling here ...
    // when e2's WindCnt is in the SAME direction as its WindDx,
    // then polygon will fill on the right of 'e2' (and 'e' will be inside)
    // nb: neither e2.WindCnt nor e2.WindDx should ever be 0.
    if (e2.windCnt * e2.windDx < 0) then
    begin
      // opposite directions so 'e' is outside 'e2' ...
      if (Abs(e2.windCnt) > 1) then
      begin
        // outside prev poly but still inside another.
        if (e2.windDx * e.windDx < 0) then
          // reversing direction so use the same WC
          e.windCnt := e2.windCnt else
          // otherwise keep 'reducing' the WC by 1 (ie towards 0) ...
          e.windCnt := e2.windCnt + e.windDx;
      end
      // now outside all polys of same polytype so set own WC ...
      else e.windCnt := e.windDx;
    end else
    begin
      //'e' must be inside 'e2'
      if (e2.windDx * e.windDx < 0) then
        // reversing direction so use the same WC
        e.windCnt := e2.windCnt
      else
        // otherwise keep 'increasing' the WC by 1 (ie away from 0) ...
        e.windCnt := e2.windCnt + e.windDx;
    end;
    e.windCnt2 := e2.windCnt2;
    e2 := e2.nextInAEL;
  end;

  // update WindCnt2 ...
  if FFillRule = frEvenOdd then
    while (e2 <> e) do
    begin
      if IsSamePolyType(e2, e) or IsOpen(e2) then // do nothing
      else if e.windCnt2 = 0 then e.windCnt2 := 1
      else e.windCnt2 := 0;
      e2 := e2.nextInAEL;
    end
  else
    while (e2 <> e) do
    begin
      if not IsSamePolyType(e2, e) and not IsOpen(e2) then
        Inc(e.windCnt2, e2.windDx);
      e2 := e2.nextInAEL;
    end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SetWindCountForOpenPathEdge(e: PActive);
var
  e2: PActive;
  cnt1, cnt2: Integer;
begin
  e2 := FActives;
  if FFillRule = frEvenOdd then
  begin
    cnt1 := 0;
    cnt2 := 0;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(cnt2)
      else if not IsOpen(e2) then inc(cnt1);
      e2 := e2.nextInAEL;
    end;
    if Odd(cnt1) then e.windCnt := 1 else e.windCnt := 0;
    if Odd(cnt2) then e.windCnt2 := 1 else e.windCnt2 := 0;
  end else
  begin
    // if FClipType in [ctUnion, ctDifference] then e.WindCnt := e.WindDx;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(e.windCnt2, e2.windDx)
      else if not IsOpen(e2) then inc(e.windCnt, e2.windDx);
      e2 := e2.nextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function IsValidAelOrder(resident, newcomer: PActive): Boolean;
var
  botY: Int64;
  newcomerIsLeft: Boolean;
  d: double;
begin
  if newcomer.currX <> resident.currX then
  begin
    Result := newcomer.currX > resident.currX;
    Exit;
  end;

  // get the turning direction  a1.top, a2.bot, a2.top
  d := CrossProduct(resident.top, newcomer.bot, newcomer.top);
  if d <> 0 then
  begin
    Result := d < 0;
    Exit;
  end;

  // edges must be collinear to get here

  if not IsMaxima(resident) and
    (resident.top.Y > newcomer.top.Y) then
  begin
    Result := CrossProduct(newcomer.bot,
      resident.top, NextVertex(resident).pt) <= 0;
    Exit;
  end
  else if not IsMaxima(newcomer) and
    (newcomer.top.Y > resident.top.Y) then
  begin
    Result := CrossProduct(newcomer.bot,
      newcomer.top, NextVertex(newcomer).pt) >= 0;
    Exit;
  end;

  botY := newcomer.bot.Y;
  newcomerIsLeft := IsLeftBound(newcomer);

  if (resident.bot.Y <> botY) or
    (resident.locMin.vertex.pt.Y <> botY) then
      Result := newcomerIsLeft
  // resident must also have just been inserted
  else if IsLeftBound(resident) <> newcomerIsLeft then
    Result := newcomerIsLeft
  else if (CrossProduct(PrevPrevVertex(resident).pt,
    resident.bot, resident.top) = 0) then
      Result := true
  else
    // otherwise compare turning direction of the alternate bound
    Result := (CrossProduct(PrevPrevVertex(resident).pt,
      newcomer.bot, PrevPrevVertex(newcomer).pt) > 0) = newcomerIsLeft;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.InsertLeftEdge(e: PActive);
var
  e2: PActive;
begin
  if not Assigned(FActives) then
  begin
    e.prevInAEL := nil;
    e.nextInAEL := nil;
    FActives := e;
  end
  else if not IsValidAelOrder(FActives, e) then
  begin
    e.prevInAEL := nil;
    e.nextInAEL := FActives;
    FActives.prevInAEL := e;
    FActives := e;
  end else
  begin
    e2 := FActives;
    while Assigned(e2.nextInAEL) and IsValidAelOrder(e2.nextInAEL, e) do
      e2 := e2.nextInAEL;
    e.nextInAEL := e2.nextInAEL;
    if Assigned(e2.nextInAEL) then e2.nextInAEL.prevInAEL := e;
    e.prevInAEL := e2;
    e2.nextInAEL := e;
  end;
end;
//----------------------------------------------------------------------

procedure InsertRightEdge(e, e2: PActive);
begin
  e2.nextInAEL := e.nextInAEL;
  if Assigned(e.nextInAEL) then e.nextInAEL.prevInAEL := e2;
  e2.prevInAEL := e;
  e.nextInAEL := e2;
end;
//----------------------------------------------------------------------

procedure TClipperBase.InsertLocalMinimaIntoAEL(const botY: Int64);
var
  leftB, rightB: PActive;
  op: POutPt;
  locMin: PLocalMinima;
  contributing: Boolean;
begin
  // Add local minima (if any) at BotY ...
  // nb: horizontal local minima edges should contain locMin.Vertex.prev

  while PopLocalMinima(botY, locMin) do
  begin
    if (vfOpenStart in locMin.vertex.flags) then
    begin
      leftB := nil;
    end else
    begin
      new(leftB);
      FillChar(leftB^, sizeof(TActive), 0);
      leftB.locMin := locMin;
      leftB.outrec := nil;
      leftB.bot := locMin.vertex.pt;
      leftB.windDx := -1;
      leftB.vertTop := locMin.vertex.prev;
      leftB.top := leftB.vertTop.pt;
      leftB.currX := leftB.bot.X;
      SetDx(leftB);
    end;

    if (vfOpenEnd in locMin.vertex.flags) then
    begin
      rightB := nil;
    end else
    begin
      new(rightB);
      FillChar(rightB^, sizeof(TActive), 0);
      rightB.locMin := locMin;
      rightB.outrec := nil;
      rightB.bot := locMin.vertex.pt;
      rightB.windDx := 1;
      rightB.vertTop := locMin.vertex.next;
      rightB.top := rightB.vertTop.pt;
      rightB.currX := rightB.bot.X;
      SetDx(rightB);
    end;
    // Currently LeftB is just descending and RightB is ascending,
    // so now we swap them if LeftB isn't actually on the left.
    if assigned(leftB) and assigned(rightB) then
    begin
      if IsHorizontal(leftB) then
      begin
        if IsHeadingRightHorz(leftB) then SwapActives(leftB, rightB);
      end
      else if IsHorizontal(rightB) then
      begin
        if IsHeadingLeftHorz(rightB) then SwapActives(leftB, rightB);
      end
      else if (leftB.dx < rightB.dx) then SwapActives(leftB, rightB);
      //so when leftB has windDx == 1, the polygon will be oriented
      //counter-clockwise in Cartesian coords (clockwise with inverted Y).
    end
    else if not assigned(leftB) then
    begin
      leftB := rightB;
      rightB := nil;
    end;
    LeftB.isLeftB := true; // nb: we can't use winddx instead

    InsertLeftEdge(leftB);                   ////////////////

    if IsOpen(leftB) then
    begin
      SetWindCountForOpenPathEdge(leftB);
      contributing := IsContributingOpen(leftB);
    end else
    begin
      SetWindCountForClosedPathEdge(leftB);
      contributing := IsContributingClosed(leftB);
    end;

    if assigned(rightB) then
    begin
      rightB.windCnt := leftB.windCnt;
      rightB.windCnt2 := leftB.windCnt2;
      InsertRightEdge(leftB, rightB);        ////////////////

      if contributing then
      begin
        AddLocalMinPoly(leftB, rightB, leftB.bot, true);

        if not IsHorizontal(leftB) and
          TestJoinWithPrev1(leftB) then
        begin
          op := AddOutPt(leftB.prevInAEL, leftB.bot);
          AddJoin(op, leftB.outrec.pts);
        end;
      end;

      while Assigned(rightB.nextInAEL) and
        IsValidAelOrder(rightB.nextInAEL, rightB) do
      begin
        IntersectEdges(rightB, rightB.nextInAEL, rightB.bot);
        SwapPositionsInAEL(rightB, rightB.nextInAEL);
      end;

      if not IsHorizontal(rightB) and
        TestJoinWithNext1(rightB) then
      begin
        op := AddOutPt(rightB.nextInAEL, rightB.bot);
        AddJoin(rightB.outrec.pts, op);
      end;

      if IsHorizontal(rightB) then
        PushHorz(rightB) else
        InsertScanLine(rightB.top.Y);
    end
    else if contributing then
      StartOpenPath(leftB, leftB.bot);

    if IsHorizontal(leftB) then
      PushHorz(leftB) else
      InsertScanLine(leftB.top.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.PushHorz(e: PActive);
begin
  if assigned(FSel) then
    e.nextInSEL := FSel else
    e.nextInSEL := nil;
  FSel := e;
end;
//------------------------------------------------------------------------------

function TClipperBase.PopHorz(out e: PActive): Boolean;
begin
  Result := assigned(FSel);
  if not Result then Exit;
  e := FSel;
  FSel := FSel.nextInSEL;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddLocalMinPoly(e1, e2: PActive;
  const pt: TPoint64; IsNew: Boolean = false): POutPt;
var
  newOr: POutRec;
  prevHotEdge: PActive;
begin
  new(newOr);
  newOr.idx := FOutRecList.Add(newOr);
  newOr.pts := nil;
  newOr.splits := nil;
  newOr.polypath := nil;
  e1.outrec := newOr;
  e2.outrec := newOr;

  // Setting the owner and inner/outer states (above) is an essential
  // precursor to setting edge 'sides' (ie left and right sides of output
  // polygons) and hence the orientation of output paths ...

  if IsOpen(e1) then
  begin
    newOr.owner := nil;
    newOr.isOpen := true;
    if e1.windDx > 0 then
      SetSides(newOr, e1, e2) else
      SetSides(newOr, e2, e1);
  end else
  begin
    prevHotEdge := GetPrevHotEdge(e1);
    newOr.isOpen := false;
    // e.windDx is the winding direction of the **input** paths
    // and unrelated to the winding direction of output polygons.
    // Output orientation is determined by e.outrec.frontE which is
    // the ascending edge (see AddLocalMinPoly).
    if Assigned(prevHotEdge) then
    begin
      newOr.owner := prevHotEdge.outrec;
      if OutrecIsAscending(prevHotEdge) = isNew then
        SetSides(newOr, e2, e1) else
        SetSides(newOr, e1, e2);
    end else
    begin
      newOr.owner := nil;
      if isNew then
        SetSides(newOr, e1, e2) else
        SetSides(newOr, e2, e1);
    end;
  end;

  new(Result);
  newOr.pts := Result;
  Result.pt := pt;
  Result.joiner := nil;
  Result.outrec := newOr;
  Result.prev := Result;
  Result.next := Result;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SafeDisposeOutPts(var op: POutPt);
var
  tmpOp: POutPt;
  outRec: POutRec;
begin
  outRec := GetRealOutRec(op.outrec);
  if Assigned(outRec.frontE) then
    outRec.frontE.outrec := nil;
  if Assigned(outRec.backE) then
    outRec.backE.outrec := nil;
  op.prev.next := nil;
  while Assigned(op) do
  begin
    SafeDeleteOutPtJoiners(op);
    tmpOp := op;
    op := op.next;
    Dispose(tmpOp);
  end;
  outRec.pts := nil; //must do this last (due to var parameter)
end;
//------------------------------------------------------------------------------

procedure TClipperBase.CleanCollinear(outRec: POutRec);
var
  op2, startOp: POutPt;
begin
  outRec := GetRealOutRec(outRec);
  if not Assigned(outRec) or
    outRec.isOpen or
    Assigned(outRec.frontE) or
    not ValidateClosedPathEx(outRec.pts) then
      Exit;

  startOp := outRec.pts;
  op2 := startOp;
  while true do
  begin
    if Assigned(op2.joiner) then Exit;
    if (CrossProduct(op2.prev.pt, op2.pt, op2.next.pt) = 0) and
      (PointsEqual(op2.pt,op2.prev.pt) or
      PointsEqual(op2.pt,op2.next.pt) or
      not FPreserveCollinear or
      (DotProduct(op2.prev.pt, op2.pt, op2.next.pt) < 0)) then
    begin
      if op2 = outRec.pts then outRec.pts := op2.prev;
      op2 := DisposeOutPt(op2);
      if not ValidateClosedPathEx(op2) then
      begin
        outRec.pts := nil;
        Exit;
      end;
      startOp := op2;
      Continue;
    end;
    op2 := op2.next;
    if op2 = startOp then Break;
  end;
  FixSelfIntersects(outRec.pts);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.FixSelfIntersects(var op: POutPt);

  function DoSplitOp(splitOp: POutPt): POutPt;
  var
    newOp, newOp2, prevOp, nextNextOp: POutPt;
    ip: TPoint64;
    area1, area2: double;
    newOutRec: POutRec;
  begin
    prevOp := splitOp.prev;
    nextNextOp := splitOp.next.next;
    Result := prevOp;
    ip := Point64(Clipper.Core.GetIntersectPointD(
      prevOp.pt, splitOp.pt, splitOp.next.pt, nextNextOp.pt));
  {$IFDEF USINGZ}
    if Assigned(fZCallback) then
      fZCallback(prevOp.Pt, splitOp.Pt, splitOp.Next.Pt, nextNextOp.Pt, ip);
  {$ENDIF}
    area1 := Area(op);
    area2 := AreaTriangle(ip, splitOp.pt, splitOp.next.pt);

    if PointsEqual(ip, prevOp.pt) or
      PointsEqual(ip, nextNextOp.pt) then
    begin
      nextNextOp.prev := prevOp;
      prevOp.next := nextNextOp;
    end else
    begin
      new(newOp2);
      newOp2.pt := ip;
      newOp2.joiner := nil;
      newOp2.outrec := prevOp.outrec;
      newOp2.prev := prevOp;
      newOp2.next := nextNextOp;
      nextNextOp.prev := newOp2;
      prevOp.next := newOp2;
    end;

    SafeDeleteOutPtJoiners(splitOp.next);
    SafeDeleteOutPtJoiners(splitOp);

    if (Abs(area2) >= 1) and
      (((Abs(area2) > (Abs(area1))) or
      ((area2 > 0) = (area1 > 0)))) then
    begin
      new(newOutRec);
      FillChar(newOutRec^, SizeOf(TOutRec), 0);
      newOutRec.idx := FOutRecList.Add(newOutRec);
      newOutRec.owner := prevOp.outrec.owner;
      newOutRec.isOpen := false;
      newOutRec.polypath := nil;
      newOutRec.splits := nil;
      splitOp.outrec := newOutRec;
      splitOp.next.outrec := newOutRec;
      new(newOp);
      newOp.pt := ip;
      newOp.joiner := nil;
      newOp.outrec := newOutRec;
      newOp.prev := splitOp.next;
      newOp.next := splitOp;
      splitOp.prev := newOp;
      splitOp.next.next := newOp;
      newOutRec.pts := newOp;
    end else
    begin
      Dispose(splitOp.next);
      Dispose(splitOp);
    end;
  end;

var
  op2: POutPt;
begin
  op2 := op;
  while true do
  begin
    // triangles can't self-intersect
    if (op2.prev = op2.next.next) then
      Break
    else if SegmentsIntersect(op2.prev.pt, op2.pt,
      op2.next.pt, op2.next.next.pt) then
    begin
      if (op2 = op) or (op2.next = op) then
        op := op2.prev;
      op2 := DoSplitOp(op2);
      op := op2;
      Continue;
    end else
      op2 := op2.next;
    if (op2 = op) then Break;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64): POutPt;
var
  outRec: POutRec;
begin
  if (IsFront(e1) = IsFront(e2)) then
  begin
    if IsOpenEnd(e1) then
      SwapFrontBackSides(e1.outrec)
    else if IsOpenEnd(e2) then
      SwapFrontBackSides(e2.outrec)
    else
    begin
      FSucceeded := false;
      Result := nil;
      Exit;
    end;
  end;

  Result := AddOutPt(e1, pt);
  if (e1.outrec = e2.outrec) then
  begin
    outRec := e1.outrec;
    outRec.pts := Result;
    UncoupleOutRec(e1);
    if not IsOpen(e1) then CleanCollinear(outRec);
    Result := outRec.pts;
    outRec.owner := GetRealOutRec(outRec.owner);
    if FUsingPolytree and Assigned(outRec.owner) and
      not Assigned(outRec.owner.frontE) then
        outRec.owner := GetRealOutRec(outRec.owner.owner);
  end
  // and to preserve the winding orientation of Outrec ...
  else if IsOpen(e1) then
  begin
    if e1.windDx < 0 then
      JoinOutrecPaths(e1, e2) else
      JoinOutrecPaths(e2, e1);
  end
  else if e1.outrec.idx < e2.outrec.idx then
    JoinOutrecPaths(e1, e2)
  else
    JoinOutrecPaths(e2, e1);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.JoinOutrecPaths(e1, e2: PActive);
var
  p1_start, p1_end, p2_start, p2_end: POutPt;
begin
  // join e2 outrec path onto e1 outrec path and then delete e2 outrec path
  // pointers. (see joining_outpt.svg)
  p1_start :=  e1.outrec.pts;
  p2_start :=  e2.outrec.pts;
  p1_end := p1_start.next;
  p2_end := p2_start.next;

  if IsFront(e1) then
  begin
    p2_end.prev := p1_start;
    p1_start.next := p2_end;
    p2_start.next := p1_end;
    p1_end.prev := p2_start;
    e1.outrec.pts := p2_start;
    // nb: if IsOpen(e1) then e1 & e2 must be a 'maximaPair'
    e1.outrec.frontE := e2.outrec.frontE;
    if Assigned(e1.outrec.frontE) then
      e1.outrec.frontE.outrec := e1.outrec;
  end else
  begin
    p1_end.prev := p2_start;
    p2_start.next := p1_end;
    p1_start.next := p2_end;
    p2_end.prev := p1_start;

    e1.outrec.backE := e2.outrec.backE;
    if Assigned(e1.outrec.backE) then
      e1.outrec.backE.outrec := e1.outrec;
  end;

  // an owner must have a lower idx otherwise
  // it won't be a valid owner
  if assigned(e2.outrec.owner) and
    (e2.outrec.owner.idx < e1.outrec.idx) then
  begin
    if not assigned(e1.outrec.owner) or
      (e2.outrec.owner.idx < e1.outrec.owner.idx) then
        e1.outrec.owner := e2.outrec.owner;
  end;

  // after joining, the e2.OutRec mustn't contains vertices
  e2.outrec.frontE := nil;
  e2.outrec.backE := nil;
  e2.outrec.pts := nil;
  e2.outrec.owner := e1.outrec;

  if IsOpenEnd(e1) then
  begin
    e2.outrec.pts := e1.outrec.pts;
    e1.outrec.pts := nil;
  end;

  // and e1 and e2 are maxima and are about to be dropped from the Actives list.
  e1.outrec := nil;
  e2.outrec := nil;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddOutPt(e: PActive; const pt: TPoint64): POutPt;
var
  opFront, opBack: POutPt;
  toFront: Boolean;
  outrec: POutRec;
begin
  // Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
  // opFront[.Prev]* ~~~> opBack & opBack == opFront.Next
  outrec := e.outrec;
  toFront := IsFront(e);
  opFront := outrec.pts;
  opBack := opFront.next;
  if toFront and PointsEqual(pt, opFront.pt) then
    result := opFront
  else if not toFront and PointsEqual(pt, opBack.pt) then
    result := opBack
  else
  begin
    new(Result);
    Result.pt := pt;
    Result.joiner := nil;
    Result.outrec := outrec;
    opBack.prev := Result;
    Result.prev := opFront;
    Result.next := opBack;
    opFront.next := Result;
    if toFront then outrec.pts := Result;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddJoin(op1, op2: POutPt);
var
  joiner: PJoiner;
begin
  if (op1.outrec = op2.outrec) and ((op1 = op2) or
  // unless op1.next or op1.prev crosses the start-end divide
  // don't waste time trying to join adjacent vertices
  ((op1.next = op2) and (op1 <> op1.outrec.pts)) or
  ((op2.next = op1) and (op2 <> op1.outrec.pts))) then Exit;

  new(joiner);
  joiner.idx := FJoinerList.Add(joiner);
  joiner.op1 := op1;
  joiner.op2 := op2;
  joiner.nextH := nil;
  joiner.next1 := op1.joiner;
  joiner.next2 := op2.joiner;
  op1.joiner := joiner;
  op2.joiner := joiner;
end;
//------------------------------------------------------------------------------

function FindJoinParent(joiner: PJoiner; op: POutPt): PJoiner;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := op.joiner;
  while true do
  begin
    if (op = Result.op1) then
    begin
      if Result.next1 = joiner then Exit
      else Result := Result.next1;
    end else
    begin
      if Result.next2 = joiner then Exit
      else Result := Result.next2;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DeleteJoin(joiner: PJoiner);
var
  op1, op2: POutPt;
  parentJnr: PJoiner;
begin
  // This method deletes a single join, and it doesn't check for or
  // delete trial horz. joins. For that, use the following method.

  op1 := joiner.op1;
  op2 := joiner.op2;

  // both op1 and op2 can be associated with multiple joiners which
  // are chained together so we need to break and rejoin that chain

  if op1.joiner <> joiner then
  begin
    parentJnr := FindJoinParent(joiner, op1);
    if parentJnr.op1 = op1 then
      parentJnr.next1 := joiner.next1 else
      parentJnr.next2 := joiner.next1;
  end else
    op1.joiner := Joiner.next1;

  if op2.joiner <> joiner then
  begin
    parentJnr := FindJoinParent(joiner, op2);
    if parentJnr.op1 = op2 then
      parentJnr.next1 := joiner.next2 else
      parentJnr.next2 := joiner.next2;
  end else
    op2.joiner := joiner.next2;

  FJoinerList[joiner.idx] := nil;
  Dispose(joiner);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SafeDeleteOutPtJoiners(op: POutPt);
var
  joiner: PJoiner;
begin
  if not Assigned(op.joiner) then Exit;
  joiner := op.joiner;
  while Assigned(joiner) do
  begin
    if joiner.idx < 0 then
      DeleteTrialHorzJoin(op)
    else if Assigned(FHorzTrials) then
    begin
      if OutPtInTrialHorzList(joiner.op1) then
        DeleteTrialHorzJoin(joiner.op1);
      if OutPtInTrialHorzList(joiner.op2) then
        DeleteTrialHorzJoin(joiner.op2);
      DeleteJoin(joiner);
    end else
      DeleteJoin(joiner);

    joiner := op.joiner;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.ProcessJoinList;
var
  i: integer;
  joiner: PJoiner;
  outRec: POutRec;
begin
  for i := 0 to FJoinerList.Count -1 do
  begin
    if Assigned(UnsafeGet(FJoinerList, i)) then
    begin
      joiner := UnsafeGet(FJoinerList, i);
      outrec := ProcessJoin(joiner);
      CleanCollinear(outRec);
    end;
  end;
  FJoinerList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.UpdateOutrecOwner(outRec: POutRec);
var
  opCurr  : POutPt;
begin
  opCurr := outRec.pts;
  repeat
    opCurr.outrec := outRec;
    opCurr := opCurr.next;
  until opCurr = outRec.pts;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.CompleteSplit(op1, op2: POutPt; OutRec: POutRec);
var
  i: integer;
  area1, area2: double;
  signsChange: Boolean;
  newOr: POutRec;
begin
  area1 := Area(op1);
  area2 := Area(op2);
  signsChange := (area1 > 0) = (area2 < 0);

  // delete trivial splits (with zero or almost zero areas)
  if (area1 = 0) or (signsChange and (Abs(area1) < 2)) then
  begin
    SafeDisposeOutPts(op1);
    OutRec.pts := op2;
  end
  else if (area2 = 0) or (signsChange and (Abs(area2) < 2)) then
  begin
    SafeDisposeOutPts(op2);
    OutRec.pts := op1;
  end
  else
  begin
    new(newOr);
    FillChar(newOr^, SizeOf(TOutRec), 0);
    newOr.idx := FOutRecList.Add(newOr);
    newOr.polypath := nil;
    newOr.splits := nil;

    if (FUsingPolytree) then
    begin
      i := Length(OutRec.splits);
      SetLength(OutRec.splits, i +1);
      OutRec.splits[i] := newOr;
    end;

    if Abs(area1) >= Abs(area2) then
    begin
      OutRec.pts := op1;
      newOr.pts := op2;
    end else
    begin
      OutRec.pts := op2;
      newOr.pts := op1;
    end;

    if (area1 > 0) = (area2 > 0) then
      newOr.owner := OutRec.owner else
      newOr.owner := OutRec;

    UpdateOutrecOwner(newOr);
    CleanCollinear(newOr);
  end;
end;
//------------------------------------------------------------------------------

function CollinearSegsOverlap(const  seg1a, seg1b,
  seg2a, seg2b: TPoint64): Boolean;
begin
  // precondition: seg1 and seg2 are collinear
  Result := false;
  if (seg1a.X = seg1b.X) then
  begin
    if (seg2a.X <> seg1a.X) or (seg2a.X <> seg2b.X) then Exit;
  end
  else if (seg1a.X < seg1b.X) then
  begin
    if (seg2a.X < seg2b.X) then
    begin
      if (seg2a.X >= seg1b.X) or (seg2b.X <= seg1a.X) then Exit;
    end
    else
      if (seg2b.X >= seg1b.X) or (seg2a.X <= seg1a.X) then Exit;
  end else
  begin
    if (seg2a.X < seg2b.X) then
    begin
      if (seg2a.X >= seg1a.X) or (seg2b.X <= seg1b.X) then Exit;
    end else
      if (seg2b.X >= seg1a.X) or (seg2a.X <= seg1b.X) then Exit;
  end;

  if (seg1a.Y = seg1b.Y) then
  begin
    if (seg2a.Y <> seg1a.Y) or (seg2a.Y <> seg2b.Y) then Exit;
  end
  else if (seg1a.Y < seg1b.Y) then
  begin
    if (seg2a.Y < seg2b.Y) then
    begin
      if (seg2a.Y >= seg1b.Y) or (seg2b.Y <= seg1a.Y) then Exit;
    end else
      if (seg2b.Y >= seg1b.Y) or (seg2a.Y <= seg1a.Y) then Exit;
  end else
  begin
    if (seg2a.Y < seg2b.Y) then
    begin
      if (seg2a.Y >= seg1a.Y) or (seg2b.Y <= seg1b.Y) then Exit;
    end else
      if (seg2b.Y >= seg1a.Y) or (seg2a.Y <= seg1b.Y) then Exit;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function PointBetween(const pt, corner1, corner2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  // nb: points may not be collinear
  Result := ValueEqualOrBetween(pt.X, corner1.X, corner2.X) and
    ValueEqualOrBetween(pt.Y, corner1.Y, corner2.Y);
end;
//------------------------------------------------------------------------------

function CheckDisposeAdjacent(var op: POutPt; guard: POutPt;
  outRec: POutRec): Boolean;
begin
  Result := false;
  while (op.prev <> op) do
  begin
    if PointsEqual(op.pt, op.prev.pt) and
      (op <> guard) and Assigned(op.prev.joiner) and
      not Assigned(op.joiner) then
    begin
      if op = outRec.pts then outRec.pts := op.prev;
      op := DisposeOutPt(op);
      op := op.prev;
    end else
      break;
  end;
  while (op.next <> op) do
  begin
    if PointsEqual(op.pt, op.next.pt) and
      (op <> guard) and Assigned(op.next.joiner) and
      not Assigned(op.joiner) then
    begin
      if op = outRec.pts then outRec.pts := op.prev;
      op := DisposeOutPt(op);
      op := op.prev;
    end else
      break;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.ProcessJoin(joiner: PJoiner): POutRec;
var
  op1, op2: POutPt;
  opA, opB: POutPt;
  or1, or2: POutRec;
begin
  op1 := joiner.op1;
  op2 := joiner.op2;

  or1 := GetRealOutRec(op1.outrec);
  or2 := GetRealOutRec(op2.outrec);
  op1.outrec := or1;
  op2.outrec := or2;
  DeleteJoin(joiner);

  Result := or1;
  if not Assigned(or2.pts) then
    Exit
  else if not IsValidClosedPath(op2) then
  begin
    SafeDisposeOutPts(op2);
    Exit;
  end
  else if not Assigned(or1.pts) or
    not IsValidClosedPath(op1) then
  begin
    SafeDisposeOutPts(op1);
    Result := or2; // ie tidy or2 in calling function;
    Exit;
  end
  else if (or1 = or2) and ((op1 = op2) or
    (op1.next = op2) or (op1.prev = op2)) then
  begin
    Exit;
  end;

  CheckDisposeAdjacent(op1, op2, or1);
  CheckDisposeAdjacent(op2, op1, or2);
  if (op1.next = op2) or (op2.next = op1) then Exit;

  while True do
  begin
    if not IsValidPath(op1) or not IsValidPath(op2) or
      ((or1 = or2) and ((op1.prev = op2) or (op1.next = op2))) then Exit;

    if PointsEqual(op1.prev.pt, op2.next.pt) or
    ((CrossProduct(op1.prev.pt, op1.pt, op2.next.pt) = 0) and
      CollinearSegsOverlap(op1.prev.pt, op1.pt, op2.pt, op2.next.pt)) then
    begin
      if or1 = or2 then
      begin
        // SPLIT REQUIRED
        // make sure op1.prev and op2.next match positions
        // by inserting an extra vertex if needed
        if not PointsEqual(op1.prev.pt, op2.next.pt) then
        begin
          if PointBetween(op1.prev.pt, op2.pt, op2.next.pt) then
            op2.next := InsertOp(op1.prev.pt, op2) else
            op1.prev := InsertOp(op2.next.pt, op1.prev);
        end;
        // current              to     new
        // op1.p[opA] >>> op1   ...    opA \   / op1
        // op2.n[opB] <<< op2   ...    opB /   \ op2
        opA := op1.prev;
        opB := op2.next;
        opA.next := opB;
        opB.prev := opA;
        op1.prev := op2;
        op2.next := op1;
        CompleteSplit(op1, opA, or1);
      end else
      begin
        // JOIN, NOT SPLIT
        opA := op1.prev;
        opB := op2.next;
        opA.next := opB;
        opB.prev := opA;
        op1.prev := op2;
        op2.next := op1;

//        SafeDeleteOutPtJoiners(op2);
//        DisposeOutPt(op2);

        if (or1.idx < or2.idx) then
        begin
          or1.pts := op1;
          or2.pts := nil;
          if Assigned(or1.owner) and
            (not Assigned(or2.owner) or
							(or2.owner.idx < or1.owner.idx)) then
								or1.owner := or2.owner;
          or2.owner := or1
        end else
        begin
          or2.pts := op1;
          or1.pts := nil;
          if Assigned(or2.owner) and
            (not Assigned(or1.owner) or
							(or1.owner.idx < or2.owner.idx)) then
								or2.owner := or1.owner;
          or1.owner := or2;
        end;
      end;
      Break;
    end
    else if PointsEqual(op1.next.pt, op2.prev.pt) or
      ((CrossProduct(op1.next.pt, op2.pt, op2.prev.pt) = 0) and
       CollinearSegsOverlap(op1.next.pt, op1.pt, op2.pt, op2.prev.pt)) then
    begin
      if or1 = or2 then
      begin
        // SPLIT REQUIRED
        // make sure op2.prev and op1.next match positions
        // by inserting an extra vertex if needed
        if not PointsEqual(op1.next.pt, op2.prev.pt) then
        begin
          if PointBetween(op2.prev.pt, op1.pt, op1.next.pt) then
            op1.next := InsertOp(op2.prev.pt, op1) else
            op2.prev := InsertOp(op1.next.pt, op2.prev);
        end;
        // current              to     new
        // op2.p[opA] >>> op2   ...    opA \   / op2
        // op1.n[opB] <<< op1   ...    opB /   \ op1
        opA := op2.prev;
        opB := op1.next;
        opA.next := opB;
        opB.prev := opA;
        op2.prev := op1;
        op1.next := op2;
        CompleteSplit(op1, opA, or1);
      end else
      begin
        // JOIN, NOT SPLIT
        opA := op1.next;
        opB := op2.prev;
        opA.prev := opB;
        opB.next := opA;
        op1.next := op2;
        op2.prev := op1;

//        SafeDeleteOutPtJoiners(op2);
//        DisposeOutPt(op2);

        if or1.idx < or2.idx then
        begin
          or1.pts := op1;
          or2.pts := nil;
          if Assigned(or1.owner) and
            (not Assigned(or2.owner) or
							(or2.owner.idx < or1.owner.idx)) then
								or1.owner := or2.owner;
          or2.owner := or1;
        end else
        begin
          Result := or2;
          or2.pts := op1;
          or1.pts := nil;
          if Assigned(or2.owner) and
            (not Assigned(or1.owner) or
							(or1.owner.idx < or2.owner.idx)) then
								or2.owner := or1.owner;
          or1.owner := or2;
        end;
      end;
      Break;
    end
    else if PointBetween(op1.next.pt, op2.pt, op2.prev.pt) and
      (DistanceFromLineSqrd(op1.next.pt, op2.pt, op2.prev.pt) < 2.01) then
    begin
      InsertOp(op1.next.pt, op2.prev);
      Continue;
    end
    else if PointBetween(op2.next.pt, op1.pt, op1.prev.pt) and
      (DistanceFromLineSqrd(op2.next.pt, op1.pt, op1.prev.pt) < 2.01) then
    begin
      InsertOp(op2.next.pt, op1.prev);
      Continue;
    end
    else if PointBetween(op1.prev.pt, op2.pt, op2.next.pt) and
      (DistanceFromLineSqrd(op1.prev.pt, op2.pt, op2.next.pt) < 2.01) then
    begin
      InsertOp(op1.prev.pt, op2);
      Continue;
    end
    else if PointBetween(op2.prev.pt, op1.pt, op1.next.pt) and
      (DistanceFromLineSqrd(op2.prev.pt, op1.pt, op1.next.pt) < 2.01) then
    begin
      InsertOp(op2.prev.pt, op1);
      Continue;
    end;

    // something odd needs tidying up
    if CheckDisposeAdjacent(op1, op2, or1) then Continue
    else if CheckDisposeAdjacent(op2, op1, or1) then Continue
    else if not PointsEqual(op1.prev.pt, op2.next.pt) and
      (DistanceSqr(op1.prev.pt, op2.next.pt) < 2.01) then
    begin
      op1.prev.pt := op2.next.pt;
      Continue;
    end
    else if not PointsEqual(op1.next.pt, op2.prev.pt) and
      (DistanceSqr(op1.next.pt, op2.prev.pt) < 2.01) then
    begin
      op2.prev.pt := op1.next.pt;
      Continue;
    end else
    begin
      // OK, there doesn't seem to be a way to join afterall
      // so just tidy up the polygons
      or1.pts := op1;
      if or2 <> or1 then
      begin
        or2.pts := op2;
        CleanCollinear(or2);
      end;
      Break;
    end;
  end; // end while
end;
//------------------------------------------------------------------------------

function TClipperBase.StartOpenPath(e: PActive; const pt: TPoint64): POutPt;
var
  newOr: POutRec;
begin
  new(newOr);
  newOr.idx := FOutRecList.Add(newOr);
  newOr.owner := nil;
  newOr.isOpen := true;
  newOr.pts := nil;
  newOr.splits := nil;
  newOr.polypath := nil;
  if e.windDx > 0 then
  begin
    newOr.frontE := e;
    newOr.backE := nil;
  end else
  begin
    newOr.frontE := nil;
    newOr.backE := e;
  end;
  e.outrec := newOr;

  new(Result);
  newOr.pts := Result;
  Result.pt := pt;
  Result.joiner := nil;
  Result.prev := Result;
  Result.next := Result;
  Result.outrec := newOr;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.UpdateEdgeIntoAEL(var e: PActive);
var
  op1, op2: POutPt;
begin
  e.bot := e.top;
  e.vertTop := NextVertex(e);
  e.top := e.vertTop.pt;
  e.currX := e.bot.X;
  SetDx(e);
  if IsHorizontal(e) then Exit;
  InsertScanLine(e.top.Y);
  if TestJoinWithPrev1(e) then
  begin
    op1 := AddOutPt(e.prevInAEL, e.bot);
    op2 := AddOutPt(e, e.bot);
    AddJoin(op1, op2);
  end;
end;
//------------------------------------------------------------------------------

function FindEdgeWithMatchingLocMin(e: PActive): PActive;
begin
  Result := e.nextInAEL;
  while Assigned(Result) do
  begin
    if (Result.locMin = e.locMin) then Exit;
    if not IsHorizontal(Result) and
      not PointsEqual(e.bot, Result.bot) then Result := nil
    else Result := Result.nextInAEL;
  end;
  Result := e.prevInAEL;
  while Assigned(Result) do
  begin
    if (Result.locMin = e.locMin) then Exit;
    if not IsHorizontal(Result) and
      not PointsEqual(e.bot, Result.bot) then Result := nil
    else
      Result := Result.prevInAEL;
  end;
end;
//------------------------------------------------------------------------------

{$IFNDEF USINGZ}
{$HINTS OFF}
{$ENDIF}
function TClipperBase.IntersectEdges(e1, e2: PActive; pt: TPoint64): POutPt;
var
  e1WindCnt, e2WindCnt, e1WindCnt2, e2WindCnt2: Integer;
  e3: PActive;
  op2: POutPt;
begin
  Result := nil;

  // MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
  if FHasOpenPaths and (IsOpen(e1) or IsOpen(e2)) then
  begin
    if IsOpen(e1) and IsOpen(e2) then Exit;
    // the following line avoids duplicating quite a bit of code
    if IsOpen(e2) then SwapActives(e1, e2);

    case FClipType of
      ctUnion: if not IsHotEdge(e2) then Exit;
      else if e2.locMin.polytype = ptSubject then Exit;
    end;
    case FFillRule of
      frPositive: if e2.windCnt <> 1 then Exit;
      frNegative: if e2.windCnt <> -1 then Exit;
      else if (abs(e2.windCnt) <> 1) then Exit;
    end;

    // toggle contribution ...
    if IsHotEdge(e1) then
    begin
      Result := AddOutPt(e1, pt);
      if IsFront(e1) then
        e1.outrec.frontE := nil else
        e1.outrec.backE := nil;
      e1.outrec := nil;
    end
    // horizontal edges can pass under open paths at a LocMins
    else if PointsEqual(pt, e1.locMin.vertex.pt) and
      (e1.locMin.vertex.flags * [vfOpenStart, vfOpenEnd] = []) then
    begin
      // find the other side of the LocMin and
      // if it's 'hot' join up with it ...
      e3 := FindEdgeWithMatchingLocMin(e1);
      if assigned(e3) and IsHotEdge(e3) then
      begin
        e1.outrec := e3.outrec;
        if e1.windDx > 0 then
          SetSides(e3.outrec, e1, e3) else
          SetSides(e3.outrec, e3, e1);
        Result := e3.outrec.pts;
        Exit;
      end
      else
        Result := StartOpenPath(e1, pt);
    end
    else
      Result := StartOpenPath(e1, pt);

    {$IFDEF USINGZ}
    SetZ(e1, e2, Result.pt);
    {$ENDIF}
    Exit;
  end;

  // MANAGING CLOSED PATHS FROM HERE ON

  // FIRST, UPDATE WINDING COUNTS
  if IsSamePolyType(e1, e2) then
  begin
    if FFillRule = frEvenOdd then
    begin
      e1WindCnt := e1.windCnt;
      e1.windCnt := e2.windCnt;
      e2.windCnt := e1WindCnt;
    end else
    begin
      if e1.windCnt + e2.windDx = 0 then
        e1.windCnt := -e1.windCnt else
        Inc(e1.windCnt, e2.windDx);
      if e2.windCnt - e1.windDx = 0 then
        e2.windCnt := -e2.windCnt else
        Dec(e2.windCnt, e1.windDx);
    end;
  end else
  begin
    if FFillRule <> frEvenOdd then Inc(e1.windCnt2, e2.windDx)
    else if e1.windCnt2 = 0 then e1.windCnt2 := 1
    else e1.windCnt2 := 0;

    if FFillRule <> frEvenOdd then Dec(e2.windCnt2, e1.windDx)
    else if e2.windCnt2 = 0 then e2.windCnt2 := 1
    else e2.windCnt2 := 0;
  end;

  case FFillRule of
    frPositive:
      begin
        e1WindCnt := e1.windCnt;
        e2WindCnt := e2.windCnt;
      end;
    frNegative:
      begin
        e1WindCnt := -e1.windCnt;
        e2WindCnt := -e2.windCnt;
      end;
    else
      begin
        e1WindCnt := abs(e1.windCnt);
        e2WindCnt := abs(e2.windCnt);
      end;
  end;

  if (not IsHotEdge(e1) and not (e1WindCnt in [0,1])) or
    (not IsHotEdge(e2) and not (e2WindCnt in [0,1])) then Exit;

  // NOW PROCESS THE INTERSECTION

  // if both edges are 'hot' ...
  if IsHotEdge(e1) and IsHotEdge(e2) then
  begin
    if not (e1WindCnt in [0,1]) or not (e2WindCnt in [0,1]) or
      (not IsSamePolyType(e1, e2) and (fClipType <> ctXor)) then
    begin
      Result := AddLocalMaxPoly(e1, e2, pt);
      {$IFDEF USINGZ}
      if Assigned(Result) then SetZ(e1, e2, Result.pt);
      {$ENDIF}

    end else if IsFront(e1) or (e1.outrec = e2.outrec) then
    begin
      // this 'else if' condition isn't strictly needed but
      // it's sensible to split polygons that ony touch at
      // a common vertex (not at common edges).
      Result := AddLocalMaxPoly(e1, e2, pt);
      op2 := AddLocalMinPoly(e1, e2, pt);
      {$IFDEF USINGZ}
      if Assigned(Result) then SetZ(e1, e2, Result.pt);
      SetZ(e1, e2, op2.pt);
      {$ENDIF}
      if Assigned(Result) and PointsEqual(Result.pt, op2.pt) and
        not IsHorizontal(e1) and not IsHorizontal(e2) and
        (CrossProduct(e1.bot, Result.pt, e2.bot) = 0) then
          AddJoin(Result, op2);
    end else
    begin
      // can't treat as maxima & minima
      Result := AddOutPt(e1, pt);
      op2 := AddOutPt(e2, pt);
      {$IFDEF USINGZ}
      SetZ(e1, e2, Result.pt);
      SetZ(e1, e2, op2.pt);
      {$ENDIF}
      SwapOutRecs(e1, e2);
    end;
  end

  // if one or other edge is 'hot' ...
  else if IsHotEdge(e1) then
  begin
    Result := AddOutPt(e1, pt);
    {$IFDEF USINGZ}
    SetZ(e1, e2, Result.pt);
    {$ENDIF}
    SwapOutRecs(e1, e2);
  end
  else if IsHotEdge(e2) then
  begin
    Result := AddOutPt(e2, pt);
    {$IFDEF USINGZ}
    SetZ(e1, e2, Result.pt);
    {$ENDIF}
    SwapOutRecs(e1, e2);
  end

  // else neither edge is 'hot'
  else
  begin
    case FFillRule of
      frPositive:
        begin
          e1WindCnt2 := e1.windCnt2;
          e2WindCnt2 := e2.windCnt2;
        end;
      frNegative:
        begin
          e1WindCnt2 := -e1.windCnt2;
          e2WindCnt2 := -e2.windCnt2;
        end;
      else
        begin
          e1WindCnt2 := abs(e1.windCnt2);
          e2WindCnt2 := abs(e2.windCnt2);
        end;
    end;

    if not IsSamePolyType(e1, e2) then
    begin
      Result := AddLocalMinPoly(e1, e2, pt, false);
      {$IFDEF USINGZ}
      SetZ(e1, e2, Result.pt);
      {$ENDIF}
    end
    else if (e1WindCnt = 1) and (e2WindCnt = 1) then
    begin
      Result := nil;
      case FClipType of
        ctIntersection:
          if (e1WindCnt2 <= 0) or (e2WindCnt2 <= 0) then Exit
          else Result := AddLocalMinPoly(e1, e2, pt, false);
        ctUnion:
          if (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0) then
            Result := AddLocalMinPoly(e1, e2, pt, false);
        ctDifference:
          if ((GetPolyType(e1) = ptClip) and
                (e1WindCnt2 > 0) and (e2WindCnt2 > 0)) or
              ((GetPolyType(e1) = ptSubject) and
                (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0)) then
            Result := AddLocalMinPoly(e1, e2, pt, false);
        else // xOr
            Result := AddLocalMinPoly(e1, e2, pt, false);
      end;
      {$IFDEF USINGZ}
      if assigned(Result) then SetZ(e1, e2, Result.pt);
      {$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------
{$IFNDEF USINGZ}
{$HINTS ON}
{$ENDIF}

function TClipperBase.ValidateClosedPathEx(var op: POutPt): Boolean;
begin
  Result := IsValidClosedPath(op);
  if Result then Exit;
  if Assigned(op) then
    SafeDisposeOutPts(op);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DeleteFromAEL(e: PActive);
var
  aelPrev, aelNext: PActive;
begin
  aelPrev := e.prevInAEL;
  aelNext := e.nextInAEL;
  if not Assigned(aelPrev) and not Assigned(aelNext) and
    (e <> FActives) then Exit; // already deleted
  if Assigned(aelPrev) then aelPrev.nextInAEL := aelNext
  else FActives := aelNext;
  if Assigned(aelNext) then aelNext.prevInAEL := aelPrev;
  Dispose(e);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AdjustCurrXAndCopyToSEL(topY: Int64);
var
  e: PActive;
begin
  FSel := FActives;
  e := FActives;
  while Assigned(e) do
  begin
    e.prevInSEL := e.prevInAEL;
    e.nextInSEL := e.nextInAEL;
    e.jump := e.nextInSEL;
    e.currX := TopX(e, topY);
    e := e.nextInAEL;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.ExecuteInternal(clipType: TClipType;
  fillRule: TFillRule; usingPolytree: Boolean);
var
  Y: Int64;
  e: PActive;
begin
  if clipType = ctNone then Exit;
  FFillRule := fillRule;
  FClipType := clipType;
  Reset;
  if not PopScanLine(Y) then Exit;
  while FSucceeded do
  begin
    InsertLocalMinimaIntoAEL(Y);
    while PopHorz(e) do DoHorizontal(e);
    ConvertHorzTrialsToJoins;
    FBotY := Y;                       // FBotY == bottom of current scanbeam
    if not PopScanLine(Y) then Break; // Y     == top of current scanbeam
    DoIntersections(Y);
    DoTopOfScanbeam(Y);
    while PopHorz(e) do DoHorizontal(e);
  end;
  if FSucceeded then ProcessJoinList;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DoIntersections(const topY: Int64);
begin
  if BuildIntersectList(topY) then
  try
    ProcessIntersectList;
  finally
    DisposeIntersectNodes;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeIntersectNodes;
var
  i: Integer;
begin
  for i := 0 to FIntersectList.Count - 1 do
    Dispose(PIntersectNode(UnsafeGet(FIntersectList,i)));
  FIntersectList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddNewIntersectNode(e1, e2: PActive; topY: Int64);
var
  pt: TPoint64;
  node: PIntersectNode;
begin
  pt := GetIntersectPoint(e1, e2);
  // Rounding errors can occasionally place the calculated intersection
  // point either below or above the scanbeam, so check and correct ...
  if (pt.Y > FBotY) then
  begin
    // E.Curr.Y is still at the bottom of scanbeam here
    pt.Y := FBotY;
    // use the more vertical of the 2 edges to derive pt.X ...
    if (abs(e1.dx) < abs(e2.dx)) then
      pt.X := TopX(e1, FBotY) else
      pt.X := TopX(e2, FBotY);
  end
  else if pt.Y < topY then
  begin
    // TopY = top of scanbeam
    pt.Y := topY;
    if e1.top.Y = topY then
      pt.X := e1.top.X
    else if e2.top.Y = topY then
      pt.X := e2.top.X
    else if (abs(e1.dx) < abs(e2.dx)) then
      pt.X := e1.currX
    else
      pt.X := e2.currX;
  end;

  new(node);
  node.active1 := e1;
  node.active2 := e2;
  node.pt := pt;
  FIntersectList.Add(node);
end;
//------------------------------------------------------------------------------

function ExtractFromSEL(edge: PActive): PActive;
begin
  // nb: edge.PrevInSEL is always assigned
  Result := edge.nextInSEL;
  if Assigned(Result) then
    Result.prevInSEL := edge.prevInSEL;
  edge.prevInSEL.nextInSEL := Result;
end;
//------------------------------------------------------------------------------

procedure Insert1Before2InSEL(edge1, edge2: PActive);
begin
  edge1.prevInSEL := edge2.prevInSEL;
  if Assigned(edge1.prevInSEL) then
    edge1.prevInSEL.nextInSEL := edge1;
  edge1.nextInSEL := edge2;
  edge2.prevInSEL := edge1;
end;
//------------------------------------------------------------------------------

function TClipperBase.BuildIntersectList(const topY: Int64): Boolean;
var
  q, base,prevBase,left,right, lend, rend: PActive;
begin
  result := false;
  if not Assigned(FActives) or not Assigned(FActives.nextInAEL) then Exit;

  // Calculate edge positions at the top of the current scanbeam, and from this
  // we will determine the intersections required to reach these new positions.
  AdjustCurrXAndCopyToSEL(topY);

  // Find all edge intersections in the current scanbeam using a stable merge
  // sort that ensures only adjacent edges are intersecting. Intersect info is
  // stored in FIntersectList ready to be processed in ProcessIntersectList.
  left := FSel;
  while Assigned(left.jump) do
  begin
    prevBase := nil;
    while Assigned(left) and Assigned(left.jump) do
    begin
      base := left;
      right := left.jump;
      rend  := right.jump;
      left.jump := rend;
      lend := right; rend := right.jump;
      while (left <> lend) and (right <> rend) do
      begin
        if right.currX < left.currX then
        begin
          // save edge intersections
          q := right.prevInSEL;
          while true do
          begin
            AddNewIntersectNode(q, right, topY);
            if q = left then Break;
            q := q.prevInSEL;
          end;

          // now move the out of place edge on the right
          // to its new ordered place on the left.
          q := right;
          right := ExtractFromSEL(q); // ie returns the new right
          lend := right;
          Insert1Before2InSEL(q, left);
          if left = base then
          begin
            base := q;
            base.jump := rend;
            if Assigned(prevBase) then
              prevBase.jump := base else
              FSel := base;
          end;
        end else
          left := left.nextInSEL;
      end;
      prevBase := base;
      left := rend;
    end;
    left := FSel;
  end;
  result := FIntersectList.Count > 0;
end;
//------------------------------------------------------------------------------

function IntersectListSort(node1, node2: Pointer): Integer;
var
  pt1, pt2: PPoint64;
  i: Int64;
begin
  if node1 = node2 then
  begin
    Result := 0;
    Exit;
  end;
  pt1 := @PIntersectNode(node1).pt;
  pt2 := @PIntersectNode(node2).pt;
  i := pt2.Y - pt1.Y;
  // note to self - can't return int64 values :)
  if i > 0 then Result := 1
  else if i < 0 then Result := -1
  else if (pt1 = pt2) then Result := 0
  else
  begin
    // Sort by X too. Not essential, but it significantly
    // speeds up the secondary sort in ProcessIntersectList .
    i := pt1.X - pt2.X;
    if i > 0 then Result := 1
    else if i < 0 then Result := -1
    else Result := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.ProcessIntersectList;
var
  i: Integer;
  nodeQ: PIntersectNode;
  nodeI, nodeJ: ^PIntersectNode;
  op1, op2: POutpt;
begin
  // The list of required intersections now needs to be processed in a
  // specific order such that intersection points with the largest Y coords
  // are processed before those with the smallest Y coords. However,
  // it's critical that edges are adjacent at the time of intersection, but
  // that can only be checked during processing (when edge positions change).

  // First we do a quicksort so that intersections will be processed
  // mostly from largest Y to smallest
  FIntersectList.Sort(IntersectListSort);

  nodeI := @FIntersectList.List[0];
  for i := 0 to FIntersectList.Count - 1 do
  begin
    // during processing, make sure edges are adjacent before
    // proceeding, and swapping the order if they aren't adjacent.
    if not EdgesAdjacentInAEL(nodeI^) then
    begin
      nodeJ := nodeI;
      repeat
        inc(nodeJ);
      until EdgesAdjacentInAEL(nodeJ^);

      // now swap intersection order
      nodeQ := nodeI^;
      nodeI^ := nodeJ^;
      nodeJ^ := nodeQ;
    end;

    // now process the intersection
    with nodeI^^ do
    begin
      IntersectEdges(active1, active2, pt);
      SwapPositionsInAEL(active1, active2);

      if TestJoinWithPrev2(active2, pt) then
      begin
        op1 := AddOutPt(active2.prevInAEL, pt);
        op2 := AddOutPt(active2, pt);
        if op1 <> op2 then
          AddJoin(op1, op2);
      end
      else if TestJoinWithNext2(active1, pt) then
      begin
        op1 := AddOutPt(active1, pt);
        op2 := AddOutPt(active1.nextInAEL, pt);
        if op1 <> op2 then
          AddJoin(op1, op2);
      end;
    end;
    inc(nodeI);
  end;
  // Edges should once again be correctly ordered (left to right) in the AEL.
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SwapPositionsInAEL(e1, e2: PActive);
var
  prev, next: PActive;
begin
  // preconditon: e1 must be immediately prior to e2
  next := e2.nextInAEL;
  if Assigned(next) then next.prevInAEL := e1;
  prev := e1.prevInAEL;
  if Assigned(prev) then prev.nextInAEL := e2;
  e2.prevInAEL := prev;
  e2.nextInAEL := e1;
  e1.prevInAEL := e2;
  e1.nextInAEL := next;
  if not Assigned(e2.prevInAEL) then FActives := e2;
end;
//------------------------------------------------------------------------------

function HorzIsSpike(horzEdge: PActive): Boolean;
var
  nextPt: TPoint64;
begin
  nextPt := NextVertex(horzEdge).pt;
  Result := (nextPt.Y = horzEdge.top.Y) and
    (horzEdge.bot.X < horzEdge.top.X) <> (horzEdge.top.X < nextPt.X);
end;
//------------------------------------------------------------------------------

procedure TrimHorz(horzEdge: PActive; preserveCollinear: Boolean);
var
  pt: TPoint64;
  wasTrimmed: Boolean;
begin
  wasTrimmed := false;
  pt := NextVertex(horzEdge).pt;
  while (pt.Y = horzEdge.top.Y) do
  begin
    // always trim 180 deg. spikes (in closed paths)
    // but otherwise break if preserveCollinear = true
    if preserveCollinear and
    ((pt.X < horzEdge.top.X) <> (horzEdge.bot.X < horzEdge.top.X)) then
      break;

    horzEdge.vertTop := NextVertex(horzEdge);
    horzEdge.top := pt;
    wasTrimmed := true;
    if IsMaxima(horzEdge) then Break;
    pt := NextVertex(horzEdge).pt;
  end;
  if wasTrimmed then SetDx(horzEdge); // +/-infinity
end;
//------------------------------------------------------------------------------

function HorzEdgesOverlap(x1a, x1b, x2a, x2b: Int64): Boolean;
const
  minOverlap: Int64 = 2;
begin
  if x1a > x1b + minOverlap then
  begin
    if x2a > x2b + minOverlap then
      Result := not ((x1a <= x2b) or (x2a <= x1b)) else
      Result := not ((x1a <= x2a) or (x2b <= x1b));
  end
  else if x1b > x1a + minOverlap then
  begin
    if x2a > x2b + minOverlap then
      Result := not ((x1b <= x2b) or (x2a <= x1a)) else
      Result := not ((x1b <= x2a) or (x2b <= x1a));
  end else
    Result := false;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddTrialHorzJoin(op: POutPt);
begin
  // make sure 'op' isn't added more than once
  if not (op.outrec.isOpen) and not OutPtInTrialHorzList(op) then
    FHorzTrials := MakeDummyJoiner(op, FHorzTrials);
end;
//------------------------------------------------------------------------------

function FindTrialJoinParent(var joiner: PJoiner; op: POutPt): PJoiner;
begin
  Result := joiner;
  while Assigned(Result) do
  begin
    if (op = Result.op1) then
    begin
      if Assigned(Result.next1) and (Result.next1.idx < 0) then
      begin
        joiner := Result.next1;
        Exit;
      end;
      Result := Result.next1;
    end else
    begin
      if Assigned(Result.next2) and (Result.next2.idx < 0) then
      begin
        joiner := Result.next2;
        Exit;
      end;
      Result := Result.next2;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DeleteTrialHorzJoin(op: POutPt);
var
  joiner, parentOp, parentH: PJoiner;
begin
  if not Assigned(FHorzTrials) then Exit;
  joiner := op.joiner;
  parentOp := nil;
  while Assigned(joiner) do
  begin
    if (joiner.idx < 0) then
    begin
      // first remove joiner from FHorzTrials list
      if joiner = FHorzTrials then
        FHorzTrials := joiner.nextH
      else
      begin
        parentH := FHorzTrials;
        while parentH.nextH <> joiner do
          parentH := parentH.nextH;
        parentH.nextH := joiner.nextH;
      end;
      // now remove joiner from op's joiner list
      if not Assigned(parentOp) then
      begin
        // joiner must be first one in list
        op.joiner := joiner.next1;
        Dispose(joiner);
        joiner := op.joiner;
      end else
      begin
        // this trial joiner isn't op's first
        // nb: trial joiners only have a single 'op'
        if op = parentOp.op1 then
          parentOp.next1 := joiner.next1 else
          parentOp.next2 := joiner.next1; // never joiner.next2
        Dispose(joiner);
        joiner := parentOp;
      end;
      // loop in case there's more than one trial join
    end else
    begin
      // not a trial join but just to be sure there isn't one
      // a little deeper, look further along the linked list
      parentOp := FindTrialJoinParent(joiner, op);
      if not Assigned(parentOp) then Break;
    end;
  end;
end;
//------------------------------------------------------------------------------

function GetHorzExtendedHorzSeg(var op, op2: POutPt): Boolean;
var
  outRec: POutRec;
begin
  outRec := GetRealOutRec(op.outrec);
  op2 := op;
  if Assigned(outRec.frontE) then
  begin
    while (op.prev <> outRec.pts) and
      (op.prev.pt.Y = op.pt.Y) do op := op.prev;
    while (op2 <> outRec.pts) and
      (op2.next.pt.Y = op2.pt.Y) do op2 := op2.next;
    Result := (op2 <> op);
  end else
  begin
    while (op.prev <> op2) and
      (op.prev.pt.Y = op.pt.Y) do op := op.prev;
    while (op2.next <> op) and
      (op2.next.pt.Y = op2.pt.Y) do op2 := op2.next;
    Result := (op2 <> op) and (op2.next <> op);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.ConvertHorzTrialsToJoins;
var
  op1a, op1b, op2a, op2b: POutPt;
  joiner, joinerParent: PJoiner;
  joined: Boolean;
begin
  while Assigned(FHorzTrials) do
  begin
    joiner := FHorzTrials;
    FHorzTrials := FHorzTrials.nextH;
    op1a := joiner.op1;
    if op1a.joiner = joiner then
    begin
      op1a.joiner := joiner.next1;
    end else
    begin
      joinerParent := FindJoinParent(joiner, op1a);
      if joinerParent.op1 = op1a then
        joinerParent.next1 := joiner.next1 else
        joinerParent.next2 := joiner.next1;
    end;
    Dispose(joiner);

    if not GetHorzExtendedHorzSeg(op1a, op1b) then
    begin
      CleanCollinear(op1a.outrec);
      Continue;
    end;

    joined := false;
    joiner := FHorzTrials;
    while Assigned(joiner) do
    begin
      op2a := joiner.op1;
      if GetHorzExtendedHorzSeg(op2a, op2b) and
        HorzEdgesOverlap(op1a.pt.X, op1b.pt.X, op2a.pt.X, op2b.pt.X) then
      begin
        joined := true;
        // overlap found so promote to a 'real' join
        if PointsEqual(op1a.pt, op2b.pt) then
          AddJoin(op1a, op2b)
        else if PointsEqual(op1a.pt, op2a.pt) then
          AddJoin(op1a, op2a)
        else if PointsEqual(op1b.pt, op2a.pt) then
          AddJoin(op1b, op2a)
        else if PointsEqual(op1b.pt, op2b.pt) then
          AddJoin(op1b, op2b)
        else if ValueBetween(op1a.pt.X, op2a.pt.X, op2b.pt.X) then
          AddJoin(op1a, InsertOp(op1a.pt, op2a))
        else if ValueBetween(op1b.pt.X, op2a.pt.X, op2b.pt.X) then
          AddJoin(op1b, InsertOp(op1b.pt, op2a))
        else if ValueBetween(op2a.pt.X, op1a.pt.X, op1b.pt.X) then
          AddJoin(op2a, InsertOp(op2a.pt, op1a))
        else if ValueBetween(op2b.pt.X, op1a.pt.X, op1b.pt.X) then
          AddJoin(op2b, InsertOp(op2b.pt, op1a));
        Break;
      end;
      joiner := joiner.nextH;
    end;
    if not joined then
      CleanCollinear(op1a.outrec);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DoHorizontal(horzEdge: PActive);
var
  maxPair: PActive;
  horzLeft, horzRight: Int64;

  function ResetHorzDirection: Boolean;
  var
    e: PActive;
  begin
    if (horzEdge.bot.X = horzEdge.top.X) then
    begin
      // the horizontal edge is going nowhere ...
      horzLeft := horzEdge.currX;
      horzRight := horzEdge.currX;
      e := horzEdge.nextInAEL;
      while assigned(e) and (e <> maxPair) do
        e := e.nextInAEL;
      Result := assigned(e);
      // nb: this block isn't yet redundant
    end
    else if horzEdge.currX < horzEdge.top.X then
    begin
      horzLeft := horzEdge.currX;
      horzRight := horzEdge.top.X;
      Result := true;
    end else
    begin
      horzLeft := horzEdge.top.X;
      horzRight := horzEdge.currX;
      Result := false;
    end;
  end;
  //------------------------------------------------------------------------

var
  Y: Int64;
  e: PActive;
  pt: TPoint64;
  op, op2: POutPt;
  maxVertex: PVertex;
  isLeftToRight, horzIsOpen: Boolean;
begin
(*******************************************************************************
* Notes: Horizontal edges (HEs) at scanline intersections (ie at the top or    *
* bottom of a scanbeam) are processed as if layered. The order in which HEs    *
* are processed doesn't matter. HEs intersect with the bottom vertices of      *
* other HEs [#] and with non-horizontal edges [*]. Once these intersections    *
* are completed, intermediate HEs are 'promoted' to the next edge in their     *
* bounds, and they in turn may be intersected [%] by other HEs.                *
*                                                                              *
* eg: 3 horizontals at a scanline:  /   |                     /          /     *
*              |                   /    |    (HE3) o=========%==========o      *
*              o=======o (HE2)    /     |         /         /                  *
*         o============#=========*======*========#=========o (HE1)             *
*        /             |        /       |       /                              *
*******************************************************************************)

  horzIsOpen := IsOpen(horzEdge);
  Y := horzEdge.bot.Y;
  maxVertex := nil;
  maxPair := nil;

  if not horzIsOpen then
  begin
    maxVertex := GetCurrYMaximaVertex(horzEdge);
    if Assigned(maxVertex) then
    begin
      maxPair := GetHorzMaximaPair(horzEdge, maxVertex);
      // remove 180 deg.spikes and also simplify
      // consecutive horizontals when PreserveCollinear = true
      if (maxVertex <> horzEdge.vertTop) then
          TrimHorz(horzEdge, FPreserveCollinear);
    end;
  end;

  isLeftToRight := ResetHorzDirection;

  // nb: TrimHorz above hence not using Bot.X here
  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, Point64(horzEdge.currX, Y));

  while true do // loop through consec. horizontal edges
  begin

    if horzIsOpen and
      IsMaxima(horzEdge) and not IsOpenEnd(horzEdge) then
    begin
      maxVertex := GetCurrYMaximaVertex(horzEdge);
      if Assigned(maxVertex) then
        maxPair := GetHorzMaximaPair(horzEdge, maxVertex);
    end;

    if isLeftToRight  then
      e := horzEdge.nextInAEL else
      e := horzEdge.prevInAEL;

    while assigned(e) do
    begin
      if (e = maxPair) then
      begin
        if IsHotEdge(horzEdge) then
        begin
          while horzEdge.vertTop <> e.vertTop do
          begin
            AddOutPt(horzEdge, horzEdge.top);
            UpdateEdgeIntoAEL(horzEdge);
          end;
          op := AddLocalMaxPoly(e, horzEdge, horzEdge.top);
          if Assigned(op) and not IsOpen(horzEdge) and
            PointsEqual(op.pt, horzEdge.top) then
              AddTrialHorzJoin(op);
        end;
        // remove horzEdge's maxPair from AEL
        DeleteFromAEL(e);
        DeleteFromAEL(horzEdge);
        Exit;
      end;

      // if horzEdge is a maxima, keep going until we reach
      // its maxima pair, otherwise check for Break conditions
      if (maxVertex <> horzEdge.vertTop) or IsOpenEnd(horzEdge) then
      begin
        // otherwise stop when 'e' is beyond the end of the horizontal line
        if (isLeftToRight and (e.currX > horzRight)) or
          (not isLeftToRight and (e.currX < horzLeft)) then Break;

        if (e.currX = horzEdge.top.X) and not IsHorizontal(e) then
        begin
          pt := NextVertex(horzEdge).pt;

          // to maximize the possibility of putting open edges into
          // solutions, we'll only break if it's past HorzEdge's end
          if IsOpen(E) and not IsSamePolyType(E, horzEdge) and
            not IsHotEdge(e) then
          begin
            if (isLeftToRight and (TopX(E, pt.Y) > pt.X)) or
              (not isLeftToRight and (TopX(E, pt.Y) < pt.X)) then Break;
          end
          // otherwise for edges at horzEdge's end, only stop when horzEdge's
          // outslope is greater than e's slope when heading right or when
          // horzEdge's outslope is less than e's slope when heading left.
          else if (isLeftToRight and (TopX(E, pt.Y) >= pt.X)) or
              (not isLeftToRight and (TopX(E, pt.Y) <= pt.X)) then Break;
        end;
      end;

      pt := Point64(e.currX, Y);

      if (isLeftToRight) then
      begin
        op := IntersectEdges(horzEdge, e, pt);
        //nb: Op.outrec will differ from horzEdge.outrec when IsOpen(e)
        SwapPositionsInAEL(horzEdge, e);

        if IsHotEdge(horzEdge) and Assigned(op) and
          not IsOpen(horzEdge) and PointsEqual(op.pt, pt) then
            AddTrialHorzJoin(op);

        if not IsHorizontal(e) and
          TestJoinWithPrev1(e) then
        begin
          op := AddOutPt(e.prevInAEL, pt);
          op2 := AddOutPt(e, pt);
          AddJoin(op, op2);
        end;
        horzEdge.currX := e.currX;
        e := horzEdge.nextInAEL;
      end else
      begin
        op := IntersectEdges(e, horzEdge, pt);
        //nb: Op.outrec will differ from horzEdge.outrec when IsOpen(e)
        SwapPositionsInAEL(e, horzEdge);

        if IsHotEdge(horzEdge) and Assigned(op) and
          not IsOpen(horzEdge) and
          PointsEqual(op.pt, pt) then
            AddTrialHorzJoin(op);

        if not IsHorizontal(e) and
          TestJoinWithNext1(e) then
        begin
          op := AddOutPt(e, pt);
          op2 := AddOutPt(e.nextInAEL, pt);
          AddJoin(op, op2);
        end;
        horzEdge.currX := e.currX;
        e := horzEdge.prevInAEL;
      end;
    end; // we've reached the end of this horizontal

    // check if we've finished looping through consecutive horizontals
    if horzIsOpen and IsOpenEnd(horzEdge) then
    begin
      if IsHotEdge(horzEdge) then
      begin
        AddOutPt(horzEdge, horzEdge.top);
        if IsFront(horzEdge) then
          horzEdge.outrec.frontE := nil else
          horzEdge.outrec.backE := nil;
        horzEdge.outrec := nil;
      end;
      DeleteFromAEL(horzEdge); // ie open at top
      Exit;
    end
    else if (NextVertex(horzEdge).pt.Y <> horzEdge.top.Y) then
      Break;

    // there must be a following (consecutive) horizontal

    if IsHotEdge(horzEdge) then
      AddOutPt(horzEdge, horzEdge.top);
    UpdateEdgeIntoAEL(horzEdge);

    if PreserveCollinear and
      not horzIsOpen and HorzIsSpike(horzEdge) then
        TrimHorz(horzEdge, true);

    isLeftToRight := ResetHorzDirection;
  end; // end while horizontal

  if IsHotEdge(horzEdge) then
  begin
    op := AddOutPt(horzEdge, horzEdge.top);
    if not IsOpen(horzEdge) then
      AddTrialHorzJoin(op);
  end else
    op := nil;

  if (horzIsOpen and not IsOpenEnd(horzEdge)) or
    (not horzIsOpen and (maxVertex <> horzEdge.vertTop)) then
  begin
    UpdateEdgeIntoAEL(horzEdge); // this is the end of an intermediate horiz.
    if IsOpen(horzEdge) then Exit;

    if isLeftToRight and TestJoinWithNext1(horzEdge) then
    begin
      op2 := AddOutPt(horzEdge.nextInAEL, horzEdge.bot);
      AddJoin(op, op2);
    end
    else if not isLeftToRight and TestJoinWithPrev1(horzEdge) then
    begin
      op2 := AddOutPt(horzEdge.prevInAEL, horzEdge.bot);
      AddJoin(op2, op);
    end;

  end
  else if IsHotEdge(horzEdge) then
    AddLocalMaxPoly(horzEdge, maxPair, horzEdge.top)
  else
  begin
    DeleteFromAEL(maxPair);
    DeleteFromAEL(horzEdge);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DoTopOfScanbeam(Y: Int64);
var
  e: PActive;
begin
  // FSel is reused to flag horizontals (see PushHorz below)
  FSel := nil;
  e := FActives;
  while Assigned(e) do
  begin
    // nb: 'e' will never be horizontal here
    if (e.top.Y = Y) then
    begin
      e.currX := e.top.X;
      if IsMaxima(e) then
      begin
        e := DoMaxima(e);  // TOP OF BOUND (MAXIMA)
        Continue;
      end else
      begin
        // INTERMEDIATE VERTEX ...
        if IsHotEdge(e) then
          AddOutPt(e, e.top);
        UpdateEdgeIntoAEL(e);
        if IsHorizontal(e) then
          PushHorz(e);
      end;
    end else
      e.currX := TopX(e, Y);
    e := e.nextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.DoMaxima(e: PActive): PActive;
var
  eNext, ePrev, eMaxPair: PActive;
begin
  ePrev := e.prevInAEL;
  eNext := e.nextInAEL;
  Result := eNext;

  if IsOpenEnd(e) then
  begin
    if IsHotEdge(e) then AddOutPt(e, e.top);
    if not IsHorizontal(e) then
    begin
      if IsHotEdge(e) then
      begin
        if IsFront(e) then
          e.outrec.frontE := nil else
          e.outrec.backE := nil;
        e.outrec := nil;
      end;
      DeleteFromAEL(e);
    end;
    Exit;
  end else
  begin
    eMaxPair := GetMaximaPair(e);
    if not assigned(eMaxPair) then Exit; // EMaxPair is a horizontal ...
  end;

  // only non-horizontal maxima here.
  // process any edges between maxima pair ...
  while (eNext <> eMaxPair) do
  begin
    IntersectEdges(e, eNext, e.top);
    SwapPositionsInAEL(e, eNext);
    eNext := e.nextInAEL;
  end;

  if IsOpen(e) then
  begin
    // must be in the middle of an open path
    if IsHotEdge(e) then
      AddLocalMaxPoly(e, eMaxPair, e.top);
    DeleteFromAEL(eMaxPair);
    DeleteFromAEL(e);

    if assigned(ePrev) then
      Result := ePrev.nextInAEL else
      Result := FActives;
  end else
  begin
    // here E.NextInAEL == ENext == EMaxPair ...
    if IsHotEdge(e) then
      AddLocalMaxPoly(e, eMaxPair, e.top);

    DeleteFromAEL(e);
    DeleteFromAEL(eMaxPair);
    if assigned(ePrev) then
      Result := ePrev.nextInAEL else
      Result := FActives;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.BuildPaths(out closedPaths, openPaths: TPaths64): Boolean;
var
  i, cntClosed, cntOpen: Integer;
  outRec: POutRec;
begin
  try
    cntClosed := 0; cntOpen := 0;
    SetLength(closedPaths, FOutRecList.Count);
    if FHasOpenPaths then
      SetLength(openPaths, FOutRecList.Count);
    for i := 0 to FOutRecList.Count -1 do
    begin
      outRec := UnsafeGet(FOutRecList, i);
      if not assigned(outRec.pts) then Continue;

      if outRec.isOpen then
      begin
        if BuildPath(outRec.pts, FReverseSolution,
          true, openPaths[cntOpen]) then
            inc(cntOpen);
      end else
      begin
        // closed paths should always return a Positive orientation
        // except when ReverseSolution == true
        if BuildPath(outRec.pts, FReverseSolution,
          false, closedPaths[cntClosed]) then
            inc(cntClosed);
      end;
    end;
    SetLength(closedPaths, cntClosed);
    SetLength(openPaths, cntOpen);
    result := true;
  except
    result := false;
  end;
end;
//------------------------------------------------------------------------------

function Path1InsidePath2(const or1, or2: POutRec): Boolean;
var
  op: POutPt;
  pipResult: TPointInPolygonResult;
begin
  op := or1.pts;
  repeat
    pipResult := PointInPolygon(op.pt, or2.path);
    if pipResult <> pipOn then Break;
    op := op.next;
  until op = or1.pts;
  if (pipResult = pipOn) then
  begin
     Result := Area(op) < Area(or2.pts);
  end else
    Result := pipResult = pipInside;
end;
//------------------------------------------------------------------------------

function GetBounds(const path: TPath64): TRect64;
var
  i: integer;
  pX, pY: PInt64;
begin
  if Length(path) = 0 then
  begin
    Result := NullRect64;
    Exit;
  end;
  result := Rect64(MaxInt64, MaxInt64, -MaxInt64, -MaxInt64);
  pX := @path[0].X;
  pY := @path[0].Y;

  for i := 0 to High(path) do
  begin

    if (pX^ < result.left) then result.left := pX^;
    if (pX^ > result.right) then result.right := pX^;
    if (pY^ < result.top) then result.top := pY^;
    if (pY^ > result.bottom) then result.bottom := pY^;
    inc(pX, 2); inc(pY, 2);
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.DeepCheckOwner(outrec, owner: POutRec): Boolean;
var
  i: integer;
  split: POutRec;
  isInsideOwnerBounds: Boolean;
begin
  if (owner.bounds.IsEmpty) then
    owner.bounds := Clipper.Engine.GetBounds(owner.path);
  isInsideOwnerBounds := owner.bounds.Contains(outrec.bounds);

  // while looking for the correct owner, check the owner's
  // splits **before** checking the owner itself because
  // splits can occur internally, and checking the owner
  // first would miss the inner split's true ownership
  result := false;
  for i := 0 to High(owner.splits) do
  begin
    split :=GetRealOutRec(owner.splits[i]);
    if not Assigned(split) or
      (split.idx <= owner.idx) or (split = outrec) then
        Continue;

    if Assigned(split.splits) and DeepCheckOwner(outrec, split) then
    begin
      Result := true;
      Exit;
    end;

    if Length(split.path) = 0 then
      BuildPath(split.pts, FReverseSolution, false, split.path);
    if split.bounds.IsEmpty then
      split.bounds := Clipper.Engine.GetBounds(split.path);
    if split.bounds.Contains(OutRec.bounds) and
      Path1InsidePath2(OutRec, split) then
    begin
      outRec.owner := split;
      Result := true;
      Exit;
    end;
  end;

  // only continue when not inside recursion
  if (owner <> outrec.owner) then Exit;

  while true do
  begin
    if isInsideOwnerBounds and
      Path1InsidePath2(outrec, outrec.owner) then
    begin
      Result := true;
      Exit;
    end;
    outrec.owner := outrec.owner.owner;
    if not assigned(outrec.owner) then Exit;
    isInsideOwnerBounds := outrec.owner.bounds.Contains(outrec.bounds);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.BuildTree(polytree: TPolyPathBase; out openPaths: TPaths64);
var
  i,j         : Integer;
  cntOpen     : Integer;
  outRec      : POutRec;
  openPath    : TPath64;
  ownerPP     : TPolyPathBase;
begin
  try
    polytree.Clear;
    if FHasOpenPaths then
      setLength(openPaths, FOutRecList.Count);
    cntOpen := 0;

    for i := 0 to FOutRecList.Count -1 do
    begin
      outRec := UnsafeGet(FOutRecList, i);
      if not assigned(outRec.pts) then Continue;

      if outRec.isOpen then
      begin
        if BuildPath(outRec.pts,
          FReverseSolution, true, openPath) then
        begin
          openPaths[cntOpen] := openPath;
          inc(cntOpen);
        end;
        Continue;
      end;

      if not BuildPath(outRec.pts, FReverseSolution, false, outRec.path) then
        Continue;
      if outrec.bounds.IsEmpty then
        outrec.bounds := Clipper.Engine.GetBounds(outrec.path);
      outrec.owner := GetRealOutRec(outrec.owner);
      if assigned(outRec.owner) then
        DeepCheckOwner(outRec, outRec.owner);

			// swap the order when a child preceeds its owner
			// (because owners must preceed children in polytrees)
      if assigned(outRec.owner) and
        (outRec.owner.idx > outRec.idx) then
      begin
        j := outRec.owner.idx;
        outRec.idx := j;
        FOutRecList[i] := UnsafeGet(FOutRecList, j);
        FOutRecList[j] := outRec;
        outRec := UnsafeGet(FOutRecList, i);
        outRec.idx := i;
        outRec.owner := GetRealOutRec(outRec.owner);
        BuildPath(outRec.pts, FReverseSolution, false, outRec.path);
        if (outRec.bounds.IsEmpty) then
          outRec.bounds := Clipper.Engine.GetBounds(outRec.path);
        if Assigned(outRec.owner) then
          DeepCheckOwner(outRec, outRec.owner);
      end;

      if assigned(outRec.owner) and assigned(outRec.owner.polypath) then
        ownerPP := outRec.owner.polypath else
        ownerPP := polytree;

      outRec.polypath := ownerPP.AddChild(outRec.path);
    end;
    setLength(openPaths, cntOpen);
  except
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.GetBounds: TRect64;
var
  i: Integer;
  v, vStart: PVertex;
begin
  Result := Rect64(MaxInt64, MaxInt64, -MaxInt64, -MaxInt64);
  for i := 0 to FVertexArrayList.Count -1 do
  begin
    vStart := UnsafeGet(FVertexArrayList, i);
    v := vStart;
    repeat
      if v.pt.X < Result.Left then Result.Left := v.pt.X
      else if v.pt.X > Result.Right then Result.Right := v.pt.X;
      if v.pt.Y < Result.Top then Result.Top := v.pt.Y
      else if v.pt.Y > Result.Bottom then Result.Bottom := v.pt.Y;
      v := v.next;
    until v = vStart;
  end;
  if Result.Left > Result.Right then Result := NullRect64;
end;

//------------------------------------------------------------------------------
// TClipper methods
//------------------------------------------------------------------------------

procedure TClipper64.AddSubject(const subject: TPath64);
begin
  AddPath(subject, ptSubject, false);
end;
//------------------------------------------------------------------------------

procedure TClipper64.AddSubject(const subjects: TPaths64);
begin
  AddPaths(subjects, ptSubject, false);
end;
//------------------------------------------------------------------------------

procedure TClipper64.AddOpenSubject(const subject: TPath64);
begin
  AddPath(subject, ptSubject, true);
end;
//------------------------------------------------------------------------------

procedure TClipper64.AddOpenSubject(const subjects: TPaths64);
begin
  AddPaths(subjects, ptSubject, true);
end;
//------------------------------------------------------------------------------

procedure TClipper64.AddClip(const clip: TPath64);
begin
  AddPath(clip, ptClip, false);
end;
//------------------------------------------------------------------------------

procedure TClipper64.AddClip(const clips: TPaths64);
begin
  AddPaths(clips, ptClip, false);
end;
//------------------------------------------------------------------------------

function TClipper64.Execute(clipType: TClipType;
  fillRule: TFillRule; out closedSolutions: TPaths64): Boolean;
var
  dummy: TPaths64;
begin
  FUsingPolytree := false;
  closedSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule, false);
    BuildPaths(closedSolutions, dummy);
    Result := Succeeded;
  except
    Result := false;
  end;
  finally
    ClearSolution;
  end;
end;
//------------------------------------------------------------------------------

function TClipper64.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedSolutions, openSolutions: TPaths64): Boolean;
begin
  closedSolutions := nil;
  openSolutions := nil;
  FUsingPolytree := false;
  try try
    ExecuteInternal(clipType, fillRule, false);
    BuildPaths(closedSolutions, openSolutions);
    Result := Succeeded;
  except
    Result := false;
  end;
  finally
    ClearSolution;
  end;
end;
//------------------------------------------------------------------------------

function TClipper64.Execute(clipType: TClipType; fillRule: TFillRule;
  var solutionTree: TPolyTree64; out openSolutions: TPaths64): Boolean;
begin
  if not assigned(solutionTree) then
    Raise EClipperLibException(rsClipper_PolyTreeErr);
  solutionTree.Clear;
  FUsingPolytree := true;
  openSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule, true);
    BuildTree(solutionTree, openSolutions);
    Result := Succeeded;
  except
    Result := false;
  end;
  finally
    ClearSolution;
  end;
end;

//------------------------------------------------------------------------------
// TPolyPathBase methods
//------------------------------------------------------------------------------

constructor TPolyPathBase.Create;
begin
  FChildList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TPolyPathBase.Destroy;
begin
  Clear;
  FChildList.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

type
  PPolyPathBase = ^TPolyPathBase;

procedure TPolyPathBase.Clear;
var
  i: integer;
  ppb: PPolyPathBase;
begin
  if FChildList.Count = 0 then Exit;
  ppb := @FChildList.List[0];
  for i := 0 to FChildList.Count -1 do
  begin
    ppb^.Free;
    inc(ppb);
  end;
  FChildList.Clear;
end;
//------------------------------------------------------------------------------

function  TPolyPathBase.GetChild(index: Integer): TPolyPathBase;
begin
  if (index < 0) or (index >= FChildList.Count) then
    Result := nil else
    Result := FChildList[index];
end;
//------------------------------------------------------------------------------

function  TPolyPathBase.GetIsHole: Boolean;
var
  pp: TPolyPathBase;
begin
  pp := FParent;
  result := assigned(pp);
  if not Result then Exit;
  while assigned(pp) do
  begin
    result := not result;
    pp := pp.FParent;
  end;
end;
//------------------------------------------------------------------------------

function  TPolyPathBase.GetChildCnt: Integer;
begin
  Result := FChildList.Count;
end;

//------------------------------------------------------------------------------
//TPolyPath method
//------------------------------------------------------------------------------

function TPolyPath64.AddChild(const path: TPath64): TPolyPathBase;
begin
  Result := TPolyPath64.Create;
  Result.Parent := self;
  TPolyPath64(Result).FPath := path;;
  ChildList.Add(Result);
end;
//------------------------------------------------------------------------------

function TPolyPath64.GetChild64(index: Integer): TPolyPath64;
begin
  Result := TPolyPath64(GetChild(index));
end;

//------------------------------------------------------------------------------
// TClipperD methods
//------------------------------------------------------------------------------

constructor TClipperD.Create(roundingDecimalPrecision: integer);
begin
  inherited Create;
  if (roundingDecimalPrecision < -8) or
    (roundingDecimalPrecision > 8) then
      Raise EClipperLibException(rsClipper_RoundingErr);
  FScale := Math.Power(10, roundingDecimalPrecision);
  FInvScale := 1/FScale;
end;
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
procedure TClipperD.CheckCallback;
begin
  // only when the user defined ZCallback function has been assigned
  // do we assign the proxy callback ZCB to ClipperBase
  if Assigned(ZCallback) then
    inherited ZCallback := ZCB else
    inherited ZCallback := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperD.ZCB(const bot1, top1, bot2, top2: TPoint64;
  var intersectPt: TPoint64);
var
  tmp: TPointD;
begin
  if not assigned(fZCallback) then Exit;
  // de-scale (x & y)
  // temporarily convert integers to their initial float values
  // this will slow clipping marginally but will make it much easier
  // to understand the coordinates passed to the callback function
  tmp := ScalePoint(intersectPt, FInvScale);
  //do the callback
  fZCallback(
    ScalePoint(bot1, FInvScale),
    ScalePoint(top1, FInvScale),
    ScalePoint(bot2, FInvScale),
    ScalePoint(top2, FInvScale), tmp);
  intersectPt.Z := tmp.Z;
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure TClipperD.AddSubject(const pathD: TPathD);
var
  p: TPath64;
begin
  if FScale = 0 then FScale := DefaultClipperDScale;
  p := ScalePath(pathD, FScale);
  AddPath(p, ptSubject, false);
end;
//------------------------------------------------------------------------------

procedure TClipperD.AddSubject(const pathsD: TPathsD);
var
  pp: TPaths64;
begin
  if FScale = 0 then FScale := DefaultClipperDScale;
  pp := ScalePaths(pathsD, FScale);
  AddPaths(pp, ptSubject, false);
end;
//------------------------------------------------------------------------------

procedure TClipperD.AddOpenSubject(const pathD: TPathD);
var
  p: TPath64;
begin
  if FScale = 0 then FScale := DefaultClipperDScale;
  p := ScalePath(pathD, FScale);
  AddPath(p, ptSubject, true);
end;
//------------------------------------------------------------------------------

procedure TClipperD.AddOpenSubject(const pathsD: TPathsD);
var
  pp: TPaths64;
begin
  if FScale = 0 then FScale := DefaultClipperDScale;
  pp := ScalePaths(pathsD, FScale);
  AddPaths(pp, ptSubject, true);
end;
//------------------------------------------------------------------------------

procedure TClipperD.AddClip(const pathD: TPathD);
var
  p: TPath64;
begin
  if FScale = 0 then FScale := DefaultClipperDScale;
  p := ScalePath(pathD, FScale);
  AddPath(p, ptClip, false);
end;
//------------------------------------------------------------------------------

procedure TClipperD.AddClip(const pathsD: TPathsD);
var
  pp: TPaths64;
begin
  if FScale = 0 then FScale := DefaultClipperDScale;
  pp := ScalePaths(pathsD, FScale);
  AddPaths(pp, ptClip, false);
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedSolutions: TPathsD): Boolean;
var
  dummy: TPathsD;
begin
  Result := Execute(clipType, fillRule, closedSolutions, dummy);
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedSolutions, openSolutions: TPathsD): Boolean;
var
  solClosed, solOpen: TPaths64;
begin
{$IFDEF USINGZ}
    CheckCallback;
{$ENDIF}
  closedSolutions := nil;
  openSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule, false);
    Result := BuildPaths(solClosed, solOpen);
    if not Result then Exit;
    closedSolutions := ScalePathsD(solClosed, FInvScale);
    openSolutions := ScalePathsD(solOpen, FInvScale);
  except
    Result := false;
  end;
  finally
    ClearSolution;
  end;
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  var solutionsTree: TPolyTreeD; out openSolutions: TPathsD): Boolean;
var
  open_Paths: TPaths64;
begin
  if not assigned(solutionsTree) then
    Raise EClipperLibException(rsClipper_PolyTreeErr);
{$IFDEF USINGZ}
    CheckCallback;
{$ENDIF}
  solutionsTree.Clear;
  FUsingPolytree := true;
  solutionsTree.SetScale(fScale);
  openSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule, true);
    BuildTree(solutionsTree, open_Paths);
    openSolutions := ScalePathsD(open_Paths, FInvScale);
    Result := true;
  except
    Result := false;
  end;
  finally
    ClearSolution;
  end;
end;

//------------------------------------------------------------------------------
// TPolyPathD methods
//------------------------------------------------------------------------------

function TPolyPathD.AddChild(const path: TPath64): TPolyPathBase;
begin
  Result := TPolyPathD.Create;
  Result.Parent := self;
  TPolyPathD(Result).fScale := fScale;
  TPolyPathD(Result).FPath := ScalePathD(path, 1/FScale);
  ChildList.Add(Result);
end;
//------------------------------------------------------------------------------

function TPolyPathD.GetChildD(index: Integer): TPolyPathD;
begin
  Result := TPolyPathD(GetChild(index));
end;

//------------------------------------------------------------------------------
// TPolyTreeD
//------------------------------------------------------------------------------

procedure TPolyTreeD.SetScale(value: double);
begin
  FScale := value;
end;
//------------------------------------------------------------------------------

end.

