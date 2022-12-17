unit Clipper.Engine;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  18 December 2022                                                *
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

  PPLocalMinima = ^PLocalMinima;
  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    vertex    : PVertex;
    polytype  : TPathType;
    isOpen    : Boolean;
  end;

  TLocMinList = class(TListEx)
  public
    function Add: PLocalMinima;
    procedure Clear; override;
  end;

  // forward declarations
  PPOutRec = ^POutRec;
  POutRec = ^TOutRec;
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
  end;

  TOutRecArray  = array of POutRec;
  THorzPosition = (hpBottom, hpMiddle, hpTop);


  // OutRec: path data structure for clipping solutions
  TOutRec = record
    idx      : Integer;
    owner    : POutRec;
    frontE   : PActive;
    backE    : PActive;
    pts      : POutPt;
    polypath : TPolyPathBase;
    bounds   : TRect64;
    path     : TPath64;
    isOpen   : Boolean;
    horzDone : Boolean;
  end;

  TOutRecList = class(TListEx)
  public
    function Add: POutRec;
    procedure Clear; override;
  end;

  PHorzSegment = ^THorzSegment;
  THorzSegment = record
    outrec      : POutRec;
    leftOp      : POutPt;
    rightOp     : POutPt;
    leftToRight : Boolean;
    position    : THorzPosition;
  end;

  THorzSegList = class(TListEx)
  public
    procedure Clear; override;
    procedure Add(outrec: POutRec);
  end;

  ///////////////////////////////////////////////////////////////////
  // Important: UP and DOWN here are premised on Y-axis positive down
  // displays, which is the orientation used in Clipper's development.
  ///////////////////////////////////////////////////////////////////

  TJoinWith = (jwNone, jwLeft, jwRight);

  // Active: represents an edge in the Active Edge Table (Vatti's AET)
  TActive = record
    bot         : TPoint64;
    top         : TPoint64;
    currX       : Int64;     // x relative to *top* of current scanbeam
    dx          : Double;    // inverse of edge slope (zero = vertical)
    windDx      : Integer;   // wind direction (ascending: +1; descending: -1)
    windCnt     : Integer;   // current wind count
    windCnt2    : Integer;   // current wind count of the opposite TPolyType
    outrec      : POutRec;
    // AEL: 'active edge list' (Vatti's AET - active edge table)
    //     a linked list of all edges (from left to right) that are present
    //     (or 'active') within the current scanbeam (a horizontal 'beam' that
    //     sweeps from bottom to top over the paths in the clipping operation).
    prevInAEL   : PActive;
    nextInAEL   : PActive;
    // SEL: 'sorted edge list' (Vatti's ST - sorted table)
    //     linked list used when sorting edges into their new positions at the
    //     top of scanbeams, but also (re)used to process horizontals.
    prevInSEL   : PActive;
    nextInSEL   : PActive;
    jump        : PActive;   // fast merge sorting (see BuildIntersectList())
    vertTop     : PVertex;
    locMin      : PLocalMinima;  // the bottom of a 'bound' (also Vatti)
    isLeftB     : Boolean;
    joinedWith  : TJoinWith;
  end;

  // IntersectNode: a structure representing 2 intersecting edges.
  // Intersections must be sorted so they are processed from the largest
  // Y coordinates to the smallest while keeping edges adjacent.
  PPIntersectNode = ^PIntersectNode;
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


  // ClipperBase: abstract base
  TClipperBase = class
  {$IFDEF STRICT}strict{$ENDIF} private
    FBotY               : Int64;
    FScanLine           : PScanLine;
    FCurrentLocMinIdx   : Integer;
    FClipType           : TClipType;
    FFillRule           : TFillRule;
    FPreserveCollinear  : Boolean;
    FIntersectList      : TList;
    FOutRecList         : TOutRecList;
    FLocMinList         : TLocMinList;
    FHorzSegList        : THorzSegList;
    FVertexArrayList    : TList;
    // FActives: see AEL above
    FActives            : PActive;
    // FSel: see SEL above.
    // BUT also used to store horz. edges for later processing
    FSel                : PActive;
    FHasOpenPaths       : Boolean;
    FLocMinListSorted   : Boolean;
    FSucceeded          : Boolean;
    FReverseSolution    : Boolean;
  {$IFDEF USINGZ}
    fDefaultZ           : Int64;
    fZCallback          : TZCallback64;
  {$ENDIF}
    procedure Reset;
    procedure InsertScanLine(const Y: Int64);
    function  PopScanLine(out Y: Int64): Boolean;
    function  PopLocalMinima(Y: Int64;
      out localMinima: PLocalMinima): Boolean;
    procedure DisposeScanLineList;
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
    procedure DeleteEdges(var e: PActive);
    procedure DeleteFromAEL(e: PActive);
    procedure AdjustCurrXAndCopyToSEL(topY: Int64);
    procedure MergeHorzSegs(currY: Int64);
    procedure DoIntersections(const topY: Int64);
    procedure DisposeIntersectNodes;
    procedure AddNewIntersectNode(e1, e2: PActive; topY: Int64);
    function  BuildIntersectList(const topY: Int64): Boolean;
    procedure ProcessIntersectList;
    procedure SwapPositionsInAEL(e1, e2: PActive);
    function  AddOutPt(e: PActive; const pt: TPoint64): POutPt;
    procedure Split(e: PActive; const currPt: TPoint64);
    procedure CheckJoinLeft(e: PActive; const currPt: TPoint64);
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure CheckJoinRight(e: PActive; const currPt: TPoint64);
      {$IFDEF INLINING} inline; {$ENDIF}
    function  AddLocalMinPoly(e1, e2: PActive;
      const pt: TPoint64; IsNew: Boolean = false): POutPt;
    function  AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64): POutPt;
    procedure JoinOutrecPaths(e1, e2: PActive);
    function  DoMaxima(e: PActive): PActive;
    procedure DoHorizontal(horzEdge: PActive);
    procedure DoTopOfScanbeam(Y: Int64);
    function  ValidateClosedPathEx(var op: POutPt): Boolean;
    procedure SafeDisposeOutPts(var op: POutPt);
    procedure CleanCollinear(outRec: POutRec);
    procedure DoSplitOp(outrec: POutRec; splitOp: POutPt);
    procedure FixSelfIntersects(outrec: POutRec);
  protected
    FUsingPolytree : Boolean;
    procedure AddPath(const path: TPath64;
      pathType: TPathType; isOpen: Boolean);
    procedure AddPaths(const paths: TPaths64;
      pathType: TPathType; isOpen: Boolean);
    function ClearSolutionOnly: Boolean;
    procedure ExecuteInternal(clipType: TClipType;
      fillRule: TFillRule; usingPolytree: Boolean);
    function  BuildPaths(out closedPaths, openPaths: TPaths64): Boolean;
    procedure BuildTree(polytree: TPolyPathBase; out openPaths: TPaths64);
  {$IFDEF USINGZ}
    procedure SetZ( e1, e2: PActive; var intersectPt: TPoint64);
    property  ZCallback : TZCallback64 read fZCallback write fZCallback;
    property  DefaultZ : Int64 READ fDefaultZ write fDefaultZ;
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
    constructor Create(precision: integer = 2);
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
// TLocMinList class
//------------------------------------------------------------------------------

function TLocMinList.Add: PLocalMinima;
begin
  new(Result);
  inherited Add(Result);
end;
//------------------------------------------------------------------------------

procedure TLocMinList.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    Dispose(PLocalMinima(UnsafeGet(i)));
  inherited;
end;

//------------------------------------------------------------------------------
// TOutRecList class
//------------------------------------------------------------------------------

function TOutRecList.Add: POutRec;
begin
  new(Result);
  FillChar(Result^, SizeOf(TOutRec), 0);
  Result.idx := inherited Add(Result);
end;
//------------------------------------------------------------------------------

procedure TOutRecList.Clear;
var
  i: integer;
  por: POutRec;
  op, tmpPp: POutPt;
begin
  for i := 0 to Count -1 do
  begin
    por := UnsafeGet(i);
    if Assigned(por.pts) then
    begin
      op := por.pts;
      op.prev.next := nil;
      while Assigned(op) do
      begin
        tmpPp := op;
        op := op.next;
        Dispose(tmpPp);
      end;
    end;
    Dispose(por);
  end;
  inherited;
end;

//------------------------------------------------------------------------------
// THorzSegList
//------------------------------------------------------------------------------

procedure THorzSegList.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    Dispose(PHorzSegment(UnsafeGet(i)));
  inherited;
end;
//------------------------------------------------------------------------------

procedure THorzSegList.Add(outrec: POutRec);
var
  hs: PHorzSegment;
begin
  if outrec.isOpen then Exit;
  outrec.horzDone := false;
  new(hs);
  hs.outrec := outrec;
  inherited Add(hs);
end;

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

function GetRealOutRec(outRec: POutRec): POutRec;
 {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outRec;
  while Assigned(Result) and not Assigned(Result.pts) do
    Result := Result.owner;
end;
//------------------------------------------------------------------------------

function PtsReallyClose(const pt1, pt2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (abs(pt1.X - pt2.X) < 2) and (abs(pt1.Y - pt2.Y) < 2);
end;
//------------------------------------------------------------------------------

function IsVerySmallTriangle(op: POutPt): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  //also treat inconsequential polygons as invalid
  Result := (op.next.next = op.prev) and
    (PtsReallyClose(op.prev.pt, op.next.pt) or
    PtsReallyClose(op.pt, op.next.pt) or
    PtsReallyClose(op.pt, op.prev.pt));
end;
//------------------------------------------------------------------------------

function IsValidClosedPath(op: POutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := assigned(op) and (op.next <> op) and
    (op.next <> op.prev) and not IsVerySmallTriangle(op);
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

function IsJoined(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.joinedWith <> jwNone;
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

function GetCurrYMaximaVertexOpen(e: PActive): PVertex;
begin
  Result := e.vertTop;
  if e.windDx > 0 then
    while (Result.next.pt.Y = Result.pt.Y) and
      (Result.flags * [vfOpenEnd, vfLocMax] = []) do
        Result := Result.next
  else
    while (Result.prev.pt.Y = Result.pt.Y) and
      (Result.flags * [vfOpenEnd, vfLocMax] = []) do
        Result := Result.prev;
  if not IsMaxima(Result) then Result := nil; // not a maxima
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
  vertexList: TList; LocMinList: TLocMinList);
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
    lm := LocMinList.Add;
    lm.vertex := vert;
    lm.polytype := polyType;
    lm.isOpen := isOpen;
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
    if (len < 3) and (not isOpen or (len < 2)) then Continue;
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

  if (cnt = 3) and IsVerySmallTriangle(op) then
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

function HorzSegListSort(item1, item2: Pointer): Integer;
var
  q: Int64;
  h1: PHorzSegment absolute item1;
  h2: PHorzSegment absolute item2;
begin
  q := h2.leftOp.pt.X - h1.leftOp.pt.X;
  if q > 0 then Result := -1
  else if q < 0 then Result := 1
  else Result := 0;
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
// TClipperBase methods ...
//------------------------------------------------------------------------------

constructor TClipperBase.Create;
begin
  FLocMinList       := TLocMinList.Create(4);
  FOutRecList       := TOutRecList.Create(4);
  FIntersectList    := TList.Create;
  FVertexArrayList  := TList.Create;
  FHorzSegList      := THorzSegList.Create;
  FPreserveCollinear  := true;
  FReverseSolution    := false;
end;
//------------------------------------------------------------------------------

destructor TClipperBase.Destroy;
begin
  Clear;
  FLocMinList.Free;
  FOutRecList.Free;
  FIntersectList.Free;
  FHorzSegList.Free;
  FVertexArrayList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TClipperBase.ClearSolutionOnly: Boolean;
var
  dummy: Int64;
begin
  try
    // in case of exceptions ...
    DeleteEdges(FActives);
    while assigned(FScanLine) do PopScanLine(dummy);
    DisposeIntersectNodes;
    DisposeScanLineList;
    FOutRecList.Clear;
    FHorzSegList.Clear;
    Result := true;
  except
    Result := false;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Clear;
begin
  ClearSolutionOnly;
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
    InsertScanLine(PLocalMinima(FLocMinList.UnsafeGet(i)).vertex.pt.Y);
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
    else if (XYCoordsEqual(intersectPt, e2.top)) then intersectPt.Z := e2.top.Z
    else intersectPt.Z := fDefaultZ;
    fZCallback(e1.bot, e1.top, e2.bot, e2.top, intersectPt);
  end else
  begin
    if (XYCoordsEqual(intersectPt, e2.bot)) then intersectPt.Z := e2.bot.Z
    else if (XYCoordsEqual(intersectPt, e2.top)) then intersectPt.Z := e2.top.Z
    else if (XYCoordsEqual(intersectPt, e1.bot)) then intersectPt.Z := e1.bot.Z
    else if (XYCoordsEqual(intersectPt, e1.top)) then intersectPt.Z := e1.top.Z
    else intersectPt.Z := fDefaultZ;
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
  localMinima := PLocalMinima(FLocMinList.UnsafeGet(FCurrentLocMinIdx));
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

procedure TClipperBase.DisposeVerticesAndLocalMinima;
var
  i: Integer;
begin
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
  if (newcomer.currX <> resident.currX) then
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
    //don't separate joined edges
    if e2.joinedWith = jwRight then e2 := e2.nextInAEL;

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
  leftB, rightB, rbn: PActive;
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
      leftB.joinedWith := jwNone;
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
      rightB.joinedWith := jwNone;
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
        if not IsHorizontal(leftB) then
          CheckJoinLeft(leftB, leftB.bot);
      end;

      while Assigned(rightB.nextInAEL) and
        IsValidAelOrder(rightB.nextInAEL, rightB) do
      begin
        rbn := rightB.nextInAEL;
        IntersectEdges(rightB, rbn, rightB.bot);
        SwapPositionsInAEL(rightB, rightB.nextInAEL);
      end;

      if IsHorizontal(rightB) then
        PushHorz(rightB)
      else
      begin
        if IsHotEdge(rightB) then
          CheckJoinRight(rightB, rightB.bot);
        InsertScanLine(rightB.top.Y);
      end;
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
  newOr := FOutRecList.Add;
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
    not ValidateClosedPathEx(outRec.pts) then
      Exit;

  startOp := outRec.pts;
  op2 := startOp;
  while true do
  begin
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
  FixSelfIntersects(outRec);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DoSplitOp(outrec: POutRec; splitOp: POutPt);
var
  newOp, newOp2, prevOp, nextNextOp: POutPt;
  ip: TPoint64;
  area1, area2, absArea1, absArea2: double;
  newOutRec: POutRec;
begin
  // splitOp.prev -> splitOp &&
  // splitOp.next -> splitOp.next.next are intersecting
  prevOp := splitOp.prev;
  nextNextOp := splitOp.next.next;
  outrec.pts := prevOp;
  GetIntersectPoint(
    prevOp.pt, splitOp.pt, splitOp.next.pt, nextNextOp.pt, ip);
{$IFDEF USINGZ}
  if Assigned(fZCallback) then
    fZCallback(prevOp.Pt, splitOp.Pt, splitOp.Next.Pt, nextNextOp.Pt, ip);
{$ENDIF}
  area1 := Area(outrec.pts);
  absArea1 := abs(area1);

  if absArea1 < 2 then
  begin
    SafeDisposeOutPts(splitOp);
    Exit;
  end;

  // nb: area1 is the path's area *before* splitting, whereas area2 is
  // the area of the triangle containing splitOp & splitOp.next.
  // So the only way for these areas to have the same sign is if
  // the split triangle is larger than the path containing prevOp or
  // if there's more than one self=intersection.
  area2 := AreaTriangle(ip, splitOp.pt, splitOp.next.pt);
  absArea2 := abs(area2);

  // de-link splitOp and splitOp.next from the path
  // while inserting the intersection point
  if PointsEqual(ip, prevOp.pt) or
    PointsEqual(ip, nextNextOp.pt) then
  begin
    nextNextOp.prev := prevOp;
    prevOp.next := nextNextOp;
  end else
  begin
    new(newOp2);
    newOp2.pt := ip;
    newOp2.outrec := outrec;
    newOp2.prev := prevOp;
    newOp2.next := nextNextOp;
    nextNextOp.prev := newOp2;
    prevOp.next := newOp2;
  end;

  if (absArea2 > 1) and
    ((absArea2 > absArea1) or
    ((area2 > 0) = (area1 > 0))) then
  begin
    newOutRec := FOutRecList.Add;
    newOutRec.owner := outrec.owner;
    newOutRec.isOpen := false;

    splitOp.outrec := newOutRec;
    splitOp.next.outrec := newOutRec;
    new(newOp);
    newOp.pt := ip;
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
//------------------------------------------------------------------------------

procedure TClipperBase.FixSelfIntersects(outrec: POutRec);
var
  op2: POutPt;
begin
  op2 := outrec.pts;
  while true do
  begin
    // triangles can't self-intersect
    if (op2.prev = op2.next.next) then
      Break
    else if SegmentsIntersect(op2.prev.pt, op2.pt,
      op2.next.pt, op2.next.next.pt) then
    begin
      DoSplitOp(outrec, op2);
      if not assigned(outrec.pts) then Break;
      op2 := outrec.pts;
      Continue;
    end else
      op2 := op2.next;
    if (op2 = outrec.pts) then Break;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64): POutPt;
var
  outRec: POutRec;
begin

  if IsJoined(e1) then Split(e1, pt);
  if IsJoined(e2) then Split(e2, pt);

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
    if Assigned(outRec.owner) and
      not Assigned(outRec.owner.frontE) then
        outRec.owner := GetRealOutRec(outRec.owner.owner);
  end
  else if IsOpen(e1) then
  begin
    // preserve the winding orientation of Outrec
    if e1.windDx < 0 then
      JoinOutrecPaths(e1, e2) else
      JoinOutrecPaths(e2, e1);
  end
  else if e1.outrec.idx < e2.outrec.idx then
    JoinOutrecPaths(e1, e2)
  else
    JoinOutrecPaths(e2, e1);

  if not assigned(Result.outrec.pts) then
    Result.outrec := Result.outrec.owner;
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

procedure TClipperBase.Split(e: PActive; const currPt: TPoint64);
begin
  if e.joinedWith = jwRight then
  begin
    Assert(e.nextInAEL.joinedWith = jwLeft, 'oops!');
    e.nextInAEL.joinedWith := jwNone;
    e.joinedWith := jwNone;
    AddLocalMinPoly(e, e.nextInAEL, currPt, true);
  end else
  begin
    Assert(e.prevInAEL.joinedWith = jwRight, 'oops!');
    e.prevInAEL.joinedWith := jwNone;
    e.joinedWith := jwNone;
    AddLocalMinPoly(e.prevInAEL, e, currPt, true);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.CheckJoinLeft(e: PActive; const currPt: TPoint64);
begin
  if IsOpen(e) or not IsHotEdge(e) or
    not Assigned(e.prevInAEL) or IsOpen(e.prevInAEL) or
    not IsHotEdge(e.prevInAEL) or (e.currX <> e.prevInAEL.currX) or
    (currPt.Y <= e.top.Y) or (currPt.Y <= e.prevInAEL.top.Y) or
    (e.joinedWith <> jwNone) or
    (currPt.Y < e.top.Y +2) or (currPt.Y < e.prevInAEL.top.Y +2) or
    (CrossProduct(e.top, currPt, e.prevInAEL.top) <> 0) then Exit;

  if (e.outrec.idx = e.prevInAEL.outrec.idx) then
    AddLocalMaxPoly(e.prevInAEL, e, currPt)
  else if e.outrec.idx < e.prevInAEL.outrec.idx then
    JoinOutrecPaths(e, e.prevInAEL)
  else
    JoinOutrecPaths(e.prevInAEL, e);
  e.prevInAEL.joinedWith := jwRight;
  e.joinedWith := jwLeft;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.CheckJoinRight(e: PActive; const currPt: TPoint64);
begin
  if IsOpen(e) or not IsHotEdge(e) or
    not Assigned(e.nextInAEL) or IsOpen(e.nextInAEL) or
    not IsHotEdge(e.nextInAEL) or (e.currX <> e.nextInAEL.currX) or
    (currPt.Y <= e.top.Y) or (currPt.Y <= e.nextInAEL.top.Y) or
    (e.joinedWith <> jwNone) or
    (currPt.Y < e.top.Y +2) or (currPt.Y < e.nextInAEL.top.Y +2) or
    (CrossProduct(e.top, currPt, e.nextInAEL.top) <> 0) then Exit;

  if e.outrec.idx = e.nextInAEL.outrec.idx then
    AddLocalMaxPoly(e, e.nextInAEL, currPt)
  else if e.outrec.idx < e.nextInAEL.outrec.idx then
    JoinOutrecPaths(e, e.nextInAEL)
  else
    JoinOutrecPaths(e.nextInAEL, e);

  e.joinedWith := jwRight;
  e.nextInAEL.joinedWith := jwLeft;
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
  begin
    result := opFront;
  end
  else if not toFront and PointsEqual(pt, opBack.pt) then
  begin
    result := opBack;
  end else
  begin
    new(Result);
    Result.pt := pt;
    Result.outrec := outrec;
    opBack.prev := Result;
    Result.prev := opFront;
    Result.next := opBack;
    opFront.next := Result;
    if toFront then outrec.pts := Result;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.StartOpenPath(e: PActive; const pt: TPoint64): POutPt;
var
  newOr: POutRec;
begin
  newOr := FOutRecList.Add;
  newOr.isOpen := true;

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
  Result.prev := Result;
  Result.next := Result;
  Result.outrec := newOr;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.UpdateEdgeIntoAEL(var e: PActive);
begin
  e.bot := e.top;
  e.vertTop := NextVertex(e);
  e.top := e.vertTop.pt;
  e.currX := e.bot.X;
  SetDx(e);

  if IsJoined(e) then Split(e, e.bot);

  if IsHorizontal(e) then Exit;
  InsertScanLine(e.top.Y);

  CheckJoinLeft(e, e.bot);
  CheckJoinRight(e, e.bot);
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

    // e1 is open and e2 is closed

    if IsJoined(e2) then Split(e2, pt); // needed for safety

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
      // e1 is no longer 'hot'
    end
    // horizontal edges can pass under open paths at a LocMins
    else if PointsEqual(pt, e1.locMin.vertex.pt) and
      (e1.locMin.vertex.flags * [vfOpenStart, vfOpenEnd] = []) then
    begin
      //todo: recheck if this code block is still needed

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
  if IsJoined(e1) then Split(e1, pt);
  if IsJoined(e2) then Split(e2, pt);

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
      {$IFDEF USINGZ}
      op2 := AddLocalMinPoly(e1, e2, pt);
      if Assigned(Result) then SetZ(e1, e2, Result.pt);
      SetZ(e1, e2, op2.pt);
      {$ELSE}
      AddLocalMinPoly(e1, e2, pt);
      {$ENDIF}
    end else
    begin
      // can't treat as maxima & minima
      Result := AddOutPt(e1, pt);
      {$IFDEF USINGZ}
      op2 := AddOutPt(e2, pt);
      SetZ(e1, e2, Result.pt);
      SetZ(e1, e2, op2.pt);
      {$ELSE}
      AddOutPt(e2, pt);
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

procedure TClipperBase.DeleteEdges(var e: PActive);
var
  e2: PActive;
begin
  while Assigned(e) do
  begin
    e2 := e;
    e := e.nextInAEL;
    Dispose(e2);
  end;
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
    if (e.joinedWith = jwLeft) then
      e.currX := e.prevInAEL.currX // this also avoids complications
    else
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
    if FHorzSegList.Count > 0 then
    begin
      if FHorzSegList.Count > 1 then
        MergeHorzSegs(Y);
      FHorzSegList.Clear;
    end;
    FBotY := Y;                       // FBotY == bottom of current scanbeam
    if not PopScanLine(Y) then Break; // Y     == top of current scanbeam
    DoIntersections(Y);
    DoTopOfScanbeam(Y);
    while PopHorz(e) do DoHorizontal(e);
  end;
end;
//------------------------------------------------------------------------------

procedure FixOutRecPts(outrec: POutrec); overload;
var
  op: POutPt;
begin
  op := outrec.pts;
  repeat
    op.outrec := outrec;
    op := op.next;
  until op = outrec.pts;
end;
//------------------------------------------------------------------------------

procedure FixOutRecPts(op: POutPt; outrec: POutrec); overload;
var
  op2: POutPt;
begin
  op2 := op;
  repeat
    op2.outrec := outrec;
    op2 := op2.next;
  until op2 = op;
end;
//------------------------------------------------------------------------------

procedure nullop; begin end;

function UpdateHorzSegment(hs: PHorzSegment; currY: Int64): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  op, op2: POutPt;
  outrec: POutrec;
begin
  hs.outrec := GetRealOutRec(hs.outrec);
  outrec := hs.outrec;

  hs.outrec := nil; // temporary flag to skip
  Result := false;
  if outrec.horzDone then Exit;
  outrec.horzDone := true;

  op := outrec.pts;
  op2 := op.next;
  if op.pt.Y <> currY then op := op2
  else if op2.pt.Y <> currY then op2 := op;

  while (op.prev <> op2) and (op.prev.pt.Y = currY) do
      op := op.prev;
  while (op2.next <> op) and (op2.next.pt.Y = currY) do
    op2 := op2.next;

  if op.pt.X = op2.pt.X then Exit;

  // get horz position - whether bottom, middle or top of path
  if (op2.next = op) then
  begin
    // no vertices where Y <> currY so must be 'bottom'
    // but make sure it's not 'top' too, otherwise ignore
    if not Assigned(outrec.frontE) then Exit;

    hs.position := hpBottom;
  end else if Assigned(outrec.frontE) then
  begin
    // when hs.position == Middle then heading etc is
    // contextual, but we'll still need hs.leftOp for sorting
    hs.position := hpMiddle;
  end else
    hs.position := hpTop;

  // assign left to right heading, and hs.leftOp and hs.rightOp
  if hs.position = hpTop then
  begin
    hs.leftToRight := op.pt.X < op2.pt.X;
    while op.next <> op2 do DisposeOutPt(op.next);
    outrec.pts := op; // just in case previous one deleted
    if hs.leftToRight then
    begin
      hs.leftOp := op;
      hs.rightOp := op2;
    end else
    begin
      hs.leftOp := op2;
      hs.rightOp := op;
    end;
  end else
  begin
    if hs.position = hpBottom then
      while outrec.pts.prev <> outrec.pts.next do
        DisposeOutPt(outrec.pts.prev);
    hs.leftToRight := outrec.pts.pt.X > outrec.pts.next.pt.X;
    if hs.leftToRight then
    begin
      hs.leftOp := outrec.pts.next;
      hs.rightOp := outrec.pts;
    end else
    begin
      hs.leftOp := outrec.pts;
      hs.rightOp := outrec.pts.next;
    end;
  end;
  hs.outrec := outrec;
  Result := true;
end;
//------------------------------------------------------------------------------

function DoMiddleCheckPathStart(hs, compareTo: PHorzSegment): Boolean;
var
  op, op2: POutPt;
  currY: Int64;
begin
  Result := false;
  op := hs.outrec.pts;
  op2 := op;
  currY := op.pt.Y;
  while op2.prev.pt.Y = currY do op2 := op2.prev;
  if op2.pt.X = op.pt.X then Exit;

  hs.leftToRight := op.pt.X > op2.pt.X;
  if hs.leftToRight = compareTo.leftToRight then Exit;

  if hs.leftToRight then
  begin
    hs.leftOp := op2;
    hs.rightOp := op;
  end else
  begin
    hs.leftOp := op;
    hs.rightOp := op2;
  end;
  Result :=
    (hs.leftOp.pt.X < compareTo.rightOp.pt.X) and
    (hs.rightOp.pt.X > compareTo.leftOp.pt.X);
end;
//------------------------------------------------------------------------------

function DoMiddleCheckPathEnd(hs, compareTo: PHorzSegment): Boolean;
var
  op, op2: POutPt;
  currY: Int64;
begin
  Result := false;
  op := hs.outrec.pts.next;
  op2 := op;
  currY := op.pt.Y;
  while op2.next.pt.Y = currY do op2 := op2.next;
  if op2.pt.X = op.pt.X then Exit;

  hs.leftToRight := op2.pt.X > op.pt.X;
  if hs.leftToRight = compareTo.leftToRight then Exit;

  if hs.leftToRight then
  begin
    hs.leftOp := op;
    hs.rightOp := op2;
  end else
  begin
    hs.leftOp := op2;
    hs.rightOp := op;
  end;
  Result :=
    (hs.leftOp.pt.X < compareTo.rightOp.pt.X) and
    (hs.rightOp.pt.X > compareTo.leftOp.pt.X);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.MergeHorzSegs(currY: Int64);
var
  i,j : integer;
  hs1, hs2: PHorzSegment;
  left1, right1, left2, right2: POutPt;
  or1, or2: POutrec;
  e: PActive;
begin
  j := 0;
  // allow only **one** horizontal segment per outrec
  for i := 0 to FHorzSegList.Count -1 do
  begin
    hs1 := FHorzSegList.UnsafeGet(i);
    if UpdateHorzSegment(hs1, currY) then
    begin
      FHorzSegList.UnsafeSet(j, hs1);
      inc(j);
    end else
      Dispose(hs1);
  end;
  // resize and sort once redundant HorzSegments have been removed
  FHorzSegList.Resize(j);
  if j < 2 then Exit;
  FHorzSegList.Sort(HorzSegListSort);

  // for each HorzSegment, find others that overlap and merge
  for i := 0 to FHorzSegList.Count -2 do
  begin
    hs1 := FHorzSegList.UnsafeGet(i);
    or1 := hs1.outrec;
    if not Assigned(or1) then Continue;

    for j := i +1 to FHorzSegList.Count -1 do
    begin
      hs2 := FHorzSegList.UnsafeGet(j);
      or2 := hs2.outrec;
      if not Assigned(or2) then Continue;

      // when position == hpMiddle then orientation etc is contextual
      // and it's only safe to merge with hpBottom horizontals
      if (hs1.position = hpMiddle) then
      begin
        if (hs2.position <> hpBottom) or
          (or1.pts.next.pt.Y <> or1.pts.pt.Y) or
          (not DoMiddleCheckPathStart(hs1, hs2) and
          not DoMiddleCheckPathEnd(hs1, hs2)) then Continue;
      end
      else if (hs2.position = hpMiddle) then
      begin
        if (hs1.position <> hpBottom) or
          (or2.pts.next.pt.Y <> or2.pts.pt.Y) or
          (not DoMiddleCheckPathStart(hs2, hs1) and
          not DoMiddleCheckPathEnd(hs2, hs1)) then Continue;
      end else
      begin
        // if these horz segments don't partially overlap
        // then neither will subsequent ones
        if (hs2.leftOp.pt.X >= hs1.rightOp.pt.X) then Break;
        // only merge counter oriented paths
        if (hs2.leftToRight = hs1.leftToRight) then Continue;
      end;

      if hs1.leftToRight then
      begin
        right1 := hs1.rightOp;
        right2 := hs2.rightOp;
        left1 := right1.prev;
        left2 := right2.next;
      end else
      begin
        right1 := hs1.rightOp;
        right2 := hs2.rightOp;
        left1 := right1.next;
        left2 := right2.prev;
      end;

      if hs1.position = hpTop then
      begin
        // when joining make sure that
        // the lower idx is always the owner
        if or1.idx < or2.idx then
        begin
          FixOutRecPts(or2.pts, or1);
          or1.pts := or2.pts;
          or2.pts := nil;
          or2.owner := or1;
          or1.frontE := or2.frontE;
          if Assigned(or1.frontE) then
            or1.frontE.outrec := or1;
          or1.backE := or2.backE;
          if Assigned(or1.backE) then
            or1.backE.outrec := or1;
          hs2.outrec := or1;
        end else
        begin
          FixOutRecPts(or1.pts, or2);
          or1.pts := nil;
          or1.owner := or2;
        end;
      end
      else if hs2.position = hpTop then
      begin
        // when joining make sure that
        // the lower idx is always the owner
        if or2.idx < or1.idx then
        begin
          FixOutRecPts(or1.pts, or2);
          or2.pts := or1.pts;
          or1.pts := nil;
          or1.owner := or2;
          or2.frontE := or1.frontE;
          if Assigned(or2.frontE) then
            or2.frontE.outrec := or2;
          or2.backE := or1.backE;
          if Assigned(or2.backE) then
            or2.backE.outrec := or2;
        end else
        begin
          FixOutRecPts(or2.pts, or1);
          or2.pts := nil;
          or2.owner := or1;
          hs2.outrec := or1;
        end;
      end;

      if hs1.leftToRight then
      begin
        left1.next := left2;
        left2.prev := left1;
        right2.next := right1;
        right1.prev := right2;
      end else
      begin
        left1.prev := left2;
        left2.next := left1;
        right2.prev := right1;
        right1.next := right2;
      end;

      if (hs1.position <> hpTop) and (hs2.position <> hpTop) then
      begin
        // no merging, just breaking and rejoining
        right1 := or1.pts.next;
        right2 := or2.pts.next;
        or1.pts.next := right2;
        right2.prev := or1.pts;
        or2.pts.next := right1;
        right1.prev := or2.pts;

        e := or1.backE;
        or1.backE := or2.backE;
        or2.backE := e;
        or1.backE.outrec := or1;
        or2.backE.outrec := or2;

        FixOutRecPts(or1);
        FixOutRecPts(or2);
      end;
      hs2.outrec.horzDone := false;
      UpdateHorzSegment(hs2, currY);
      Break;
    end;
  end;

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
  ip: TPoint64;
  absDx1, absDx2: double;
  node: PIntersectNode;
begin
  if not GetIntersectPoint(e1.bot, e1.top, e2.bot, e2.top, ip) then
    ip := Point64(e1.currX, topY);
  // Rounding errors can occasionally place the calculated intersection
  // point either below or above the scanbeam, so check and correct ...
  if (ip.Y > FBotY) or (ip.Y < topY) then
  begin
    absDx1 := Abs(e1.dx);
    absDx2 := Abs(e2.dx);
    if (absDx1 > 100) and (absDx2 > 100) then
    begin
      if (absDx1 > absDx2) then
        ip := GetClosestPointOnSegment(ip, e1.bot, e1.top) else
        ip := GetClosestPointOnSegment(ip, e2.bot, e2.top);
    end
    else if (absDx1 > 100) then
      ip := GetClosestPointOnSegment(ip, e1.bot, e1.top)
    else if (absDx2 > 100) then
      ip := GetClosestPointOnSegment(ip, e2.bot, e2.top)
    else
    begin
      if (ip.Y < topY) then
        ip.Y := topY else
        ip.Y := fBotY;
      if (absDx1 < absDx2)  then
        ip.X := TopX(e1, topY) else
        ip.X := TopX(e2, topY);
    end;
  end;
  new(node);
  node.active1 := e1;
  node.active2 := e2;
  node.pt := ip;
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
  e, base,prevBase,left,right, lend, rend: PActive;
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
          e := right.prevInSEL;
          while true do
          begin
            AddNewIntersectNode(e, right, topY);
            if e = left then Break;
            e := e.prevInSEL;
          end;

          // now move the out of place edge on the right
          // to its new ordered place on the left.
          e := right;
          right := ExtractFromSEL(e); // ie returns the new right
          lend := right;
          Insert1Before2InSEL(e, left);
          if left = base then
          begin
            base := e;
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
  nodeI, nodeJ: PPIntersectNode;
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
      active1.currX := pt.X;
      active2.currX := pt.X;
      CheckJoinLeft(active2, pt);
      CheckJoinRight(active1, pt);
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

procedure TClipperBase.DoHorizontal(horzEdge: PActive);
var
  maxVertex: PVertex;
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
      while assigned(e) and (e.vertTop <> maxVertex) do
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
  currOr: POutRec;
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

  if horzIsOpen then
    maxVertex := GetCurrYMaximaVertexOpen(horzEdge) else
    maxVertex := GetCurrYMaximaVertex(horzEdge);

  if Assigned(maxVertex) and not horzIsOpen and
    (maxVertex <> horzEdge.vertTop) then
      TrimHorz(horzEdge, FPreserveCollinear);

  isLeftToRight := ResetHorzDirection;

  // nb: TrimHorz above hence not using Bot.X here
  if IsHotEdge(horzEdge) then
  begin
  {$IFDEF USINGZ}
    AddOutPt(horzEdge, Point64(horzEdge.currX, Y, horzEdge.bot.Z));
  {$ELSE}
    AddOutPt(horzEdge, Point64(horzEdge.currX, Y));
  {$ENDIF}
    FHorzSegList.Add(horzEdge.outrec);
  end;
  currOr := horzEdge.outrec;

  while true do // loop through consec. horizontal edges
  begin
    if isLeftToRight  then
      e := horzEdge.nextInAEL else
      e := horzEdge.prevInAEL;

    while assigned(e) do
    begin
      if (e.vertTop = maxVertex) then
      begin
        if IsHotEdge(horzEdge) and IsJoined(e) then
          Split(e, e.top);

        if IsHotEdge(horzEdge) then
        begin

          while (horzEdge.vertTop <> maxVertex) do
          begin
            AddOutPt(horzEdge, horzEdge.top);
            UpdateEdgeIntoAEL(horzEdge);
          end;

          if isLeftToRight then
            AddLocalMaxPoly(horzEdge, e, horzEdge.top) else
            AddLocalMaxPoly(e, horzEdge, horzEdge.top);
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
        IntersectEdges(horzEdge, e, pt);
        //nb: Op.outrec will differ from horzEdge.outrec when IsOpen(e)
        SwapPositionsInAEL(horzEdge, e);
        //??Join
        horzEdge.currX := e.currX;
        e := horzEdge.nextInAEL;
      end else
      begin
        IntersectEdges(e, horzEdge, pt);
        //nb: Op.outrec will differ from horzEdge.outrec when IsOpen(e)
        SwapPositionsInAEL(e, horzEdge);
        //??Join
        horzEdge.currX := e.currX;
        e := horzEdge.prevInAEL;
      end;
      if IsHotEdge(horzEdge) and (horzEdge.outrec <> currOr) then
      begin
        currOr := horzEdge.outrec;
        FHorzSegList.Add(currOr);
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
    end;

    if (NextVertex(horzEdge).pt.Y <> horzEdge.top.Y) then
      Break; // end of an intermediate horizontal

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
    AddOutPt(horzEdge, horzEdge.top);

  UpdateEdgeIntoAEL(horzEdge); // this is the end of an intermediate horiz.
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

  if IsJoined(e) then Split(e, e.top);
  if IsJoined(eMaxPair) then Split(eMaxPair, eMaxPair.top);

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
    // e.NextInAEL == eMaxPair
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

    i := 0;
    while i < FOutRecList.Count do
    begin
      outRec := FOutRecList.UnsafeGet(i);
      inc(i);
      if not assigned(outRec.pts) then Continue;

      if outRec.isOpen then
      begin
        if BuildPath(outRec.pts, FReverseSolution,
          true, openPaths[cntOpen]) then
            inc(cntOpen);
      end else
      begin
        // nb: CleanCollinear can add to FOutRecList
        CleanCollinear(outRec);
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

function DeepCheckOwner(outrec, owner: POutRec): Boolean;
var
  isInsideOwnerBounds: Boolean;
begin
  if (owner.bounds.IsEmpty) then
    owner.bounds := Clipper.Engine.GetBounds(owner.path);

  result := false;
  // only continue when not inside recursion
  if (owner <> outrec.owner) then Exit;

  isInsideOwnerBounds := owner.bounds.Contains(outrec.bounds);
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

    i := 0;
    while i < FOutRecList.Count do
    begin
      outRec := FOutRecList.UnsafeGet(i);
      inc(i);
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

      // nb: CleanCollinear can add to FOutRecList
      CleanCollinear(outRec);
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
        FOutRecList.Swap(i,j);
        POutrec(FOutRecList.UnsafeGet(j)).idx := j;
        outRec := POutrec(FOutRecList.UnsafeGet(i));
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
    if not ClearSolutionOnly then Result := false;
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
    if not ClearSolutionOnly then Result := false;
  end;
end;
//------------------------------------------------------------------------------

function TClipper64.Execute(clipType: TClipType; fillRule: TFillRule;
  var solutionTree: TPolyTree64; out openSolutions: TPaths64): Boolean;
begin
  if not assigned(solutionTree) then
    Raise EClipper2LibException(rsClipper_PolyTreeErr);
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
    if not ClearSolutionOnly then Result := false;
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

constructor TClipperD.Create(precision: integer);
begin
  inherited Create;
  CheckPrecisionRange(precision);
  FScale := Math.Power(10, precision);
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
    if not ClearSolutionOnly then Result := false;
  end;
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  var solutionsTree: TPolyTreeD; out openSolutions: TPathsD): Boolean;
var
  open_Paths: TPaths64;
begin
  if not assigned(solutionsTree) then
    Raise EClipper2LibException(rsClipper_PolyTreeErr);
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
    if not ClearSolutionOnly then Result := false;
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

