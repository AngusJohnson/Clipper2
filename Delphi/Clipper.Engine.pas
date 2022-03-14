unit Clipper.Engine;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  14 March 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This is the main polygon clipping module                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Classes, SysUtils, Math, Clipper.Core;

type
  TVertexFlag = (vfOpenStart, vfOpenEnd, vfLocMax, vfLocMin);
  TVertexFlags = set of TVertexFlag;

  //TVertex: a pre-clipping data structure. It is used to separate polygons
  //into ascending and descending 'bounds' (or sides) that start at local
  //minima and ascend (on the left with clockwise oriented paths) to a local
  //maxima, before descending to either the same or another local minima.
  PVertex = ^TVertex;
  TVertex = record
    Pt    : TPoint64;
    Next  : PVertex;
    Prev  : PVertex;
    Flags : TVertexFlags;
  end;

  //Every closed path (or polygon) is made up of a series of vertices forming
  //edges that alternate between going up (relative to the Y-axis) and going
  //down. Edges consecutively going up or consecutively going down are called
  //'bounds' (or sides if they're simple polygons). 'Local Minima' refer to
  //vertices where descending bounds become ascending ones.

  //TPathType:
  //  1. only subject paths may be open
  //  2. for closed paths, all boolean clipping operations except for
  //     Difference are commutative. (In other words, subjects and clips
  //     could be swapped and the same solution will be returned.)
  TPathType = (ptSubject, ptClip);

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    Vertex    : PVertex;
    PolyType  : TPathType;
    IsOpen    : Boolean;
  end;

  //OutPt: a post-clipping data structure representing a solution vertex.
  POutPt = ^TOutPt;
  TOutPt = record
    Pt       : TPoint64;
    Next     : POutPt;
    Prev     : POutPt;
  end;

  //POutRec: a post-clipping data structure representing a solution path.
  POutRec = ^TOutRec;

  //TActive and edge in the Active Edge Table (Vatti's AET)
  PActive = ^TActive;
  TActive = record
    Op       : POutPt;        //used in descendant classes
    Bot      : TPoint64;
    Top      : TPoint64;
    CurrX    : Int64;
    Dx       : Double;        //inverse of edge slope (zero = vertical)
    WindDx   : Integer;       //wind direction (ascending: +1; descending: -1)
    WindCnt  : Integer;       //current wind count
    WindCnt2 : Integer;       //current wind count of the opposite TPolyType
    OutRec   : POutRec;
    //AEL: 'active edge list' (Vatti's AET - active edge table)
    //     a linked list of all edges (from left to right) that are present
    //     (or 'active') within the current scanbeam (a horizontal 'beam' that
    //     sweeps from bottom to top over the paths in the clipping operation).
    PrevInAEL: PActive;
    NextInAEL: PActive;
    //SEL: 'sorted edge list' (Vatti's ST - sorted table)
    //     linked list used when sorting edges into their new positions at the
    //     top of scanbeams, but also (re)used to process horizontals.
    PrevInSEL: PActive;
    NextInSEL: PActive;
    Jump     : PActive;       //fast merge sorting (see BuildIntersectList())
    VertTop  : PVertex;
    LocMin   : PLocalMinima;  //the bottom of an edge 'bound' (also Vatti)
  end;

  //TIntersectNode: a structure representing 2 intersecting edges
  //Intersections must be sorted so they are processed in order (mostly
  //from the bottom up, but edges must also be adjacent).
  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
    Edge1  : PActive;
    Edge2  : PActive;
    Pt     : TPoint64;
  end;

  //an imaginary line representing the algorithm's process from
  //bottom to top of the polygon space
  PScanLine = ^TScanLine;
  TScanLine = record
    Y        : Int64;
    Next     : PScanLine;
  end;

  TOutRecState = (osUndefined, osOpen, osOuter, osInner);

  TPolyPathBase = class;
  TPolyTree     = class;
  TPolyTreeD    = class;

  //OutRec: contains a path in the clipping solution. Edges in the AEL will
  //have OutRec pointers assigned when they form part of the clipping solution.

  TOutRec = record
    Idx      : Integer;
    Owner    : POutRec;
    FrontE   : PActive;
    BackE    : PActive;
    Pts      : POutPt;
    PolyPath : TPolyPathBase;
    State    : TOutRecState;
  end;

  {$IFDEF USINGZ}
  TZCallback64 = procedure (const bot1, top1, bot2, top2: TPoint64;
    var intersectPt: TPoint64) of object;
  TZCallbackD = procedure (const bot1, top1, bot2, top2: TPointD;
    var intersectPt: TPointD) of object;
  {$ENDIF}

  //TClipperBase - abstract base class
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
    FActives            : PActive; //see AEL above
    FSel                : PActive; //see SEL above
  {$IFDEF USINGZ}
    FZFunc              : TZCallback64;
  {$ENDIF}
    procedure Reset;
    procedure InsertScanLine(const Y: Int64);
    function  PopScanLine(out Y: Int64): Boolean;
    function  PopLocalMinima(Y: Int64;
      out localMinima: PLocalMinima): Boolean;
    procedure DisposeScanLineList;
    procedure DisposeOutRec(index: Integer);
    procedure DisposeAllOutRecs;
    procedure DisposeVerticesAndLocalMinima;
    procedure AddPathToVertexList(const p: TPath64;
      polyType: TPathType; isOpen: Boolean);
    function IsContributingClosed(e: PActive): Boolean;
    function IsContributingOpen(e: PActive): Boolean;
    procedure SetWindCountForClosedPathEdge(e: PActive);
    procedure SetWindCountForOpenPathEdge(e: PActive);
    procedure InsertLocalMinimaIntoAEL(const botY: Int64);
    procedure InsertLeftEdge(e: PActive);
    procedure PushHorz(e: PActive); {$IFDEF INLINING} inline; {$ENDIF}
    function  PopHorz(out e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function  StartOpenPath(e: PActive; const pt: TPoint64): POutPt;
    procedure UpdateEdgeIntoAEL(var e: PActive);
    procedure IntersectEdges(e1, e2: PActive; pt: TPoint64);
    procedure DeleteFromAEL(e: PActive);
    procedure AdjustCurrXAndCopyToSEL(topY: Int64);
    procedure DoIntersections(const topY: Int64);
    procedure DisposeIntersectNodes;
    procedure AddNewIntersectNode(e1, e2: PActive; topY: Int64);
    function  BuildIntersectList(const topY: Int64): Boolean;
    procedure ProcessIntersectList;
    procedure SwapPositionsInAEL(e1, e2: PActive);
    procedure DoHorizontal(horzEdge: PActive);
    procedure DoTopOfScanbeam(Y: Int64);
    function  DoMaxima(e: PActive): PActive;
    function  AddOutPt(e: PActive; const pt: TPoint64): POutPt;
    function  AddLocalMinPoly(e1, e2: PActive;
      const pt: TPoint64; IsNew: Boolean = false): POutPt;
    function  AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64): POutPt;
    procedure FixSelfIntersects(var op: POutPt);
    procedure JoinOutrecPaths(e1, e2: PActive);
    function  GetIntersectNode(index: integer): PIntersectNode;
      {$IFDEF INLINING} inline; {$ENDIF}
  protected
    FLocMinListSorted   : Boolean;
    FHasOpenPaths       : Boolean;
    procedure AddPath(const path: TPath64;
      pathType: TPathType; isOpen: Boolean);
    procedure AddPaths(const paths: TPaths64;
      pathType: TPathType; isOpen: Boolean);
    procedure CleanUp; //unlike Clear, CleanUp preserves added paths
    procedure ExecuteInternal(clipType: TClipType; fillRule: TFillRule);
    function BuildPaths(out closedPaths, openPaths: TPaths64): Boolean;
    procedure BuildTree(polytree: TPolyPathBase; out openPaths: TPaths64);
  {$IFDEF USINGZ}
    procedure SetZ( e1, e2: PActive; var intersectPt: TPoint64);
  {$ENDIF}
    property IntersectNode[index: integer]: PIntersectNode
      read GetIntersectNode;
    property OutRecList : TList read FOutRecList;
  {$IFDEF USINGZ}
    property OnZFill : TZCallback64 read FZFunc write FZFunc;
  {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function GetBounds: TRect64;
    property PreserveCollinear: Boolean read
      FPreserveCollinear write FPreserveCollinear;
  end;

  TClipper = class(TClipperBase) //for integer coordinates
  public
    procedure AddSubject(const subject: TPath64); overload;
    procedure AddSubject(const subjects: TPaths64); overload;
    procedure AddOpenSubject(const subject: TPath64); overload;
    procedure AddOpenSubject(const subjects: TPaths64); overload;
    procedure AddClip(const clip: TPath64); overload;
    procedure AddClip(const clips: TPaths64); overload;
    //EXECUTE METHODS ...
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions: TPaths64): Boolean; overload; virtual;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions, openSolutions: TPaths64): Boolean; overload; virtual;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      var solutionTree: TPolyTree; out openSolutions: TPaths64): Boolean; overload; virtual;
  {$IFDEF USINGZ}
    property ZFillFunc;
  {$ENDIF}
  end;

  //TPolyPathBase: ancestor of TPolyPath and TPolyPathD
  TPolyPathBase = class
  {$IFDEF STRICT}strict{$ENDIF} private
    FParent     : TPolyPathBase;
    FChildList  : TList;
    function    GetChildCnt: Integer;
    function    GetChild(index: Integer): TPolyPathBase;
    function    GetIsHole: Boolean;
  protected
    property    ChildList: TList read FChildList;
    property    Parent: TPolyPathBase read FParent write FParent;
  public
    constructor Create;  virtual;
    destructor  Destroy; override;
    procedure   Clear; virtual;
    function    AddChild(const path: TPath64): TPolyPathBase; virtual; abstract;
    property    IsHole: Boolean read GetIsHole;
    property    ChildCount: Integer read GetChildCnt;
    property    Child[index: Integer]: TPolyPathBase read GetChild;
  end;

  TPolyPath = class(TPolyPathBase)
  {$IFDEF STRICT}strict{$ENDIF} private
    FPath : TPath64;
  public
    function AddChild(const path: TPath64): TPolyPathBase; override;
    property Polygon: TPath64 read FPath;
  end;

  //TPolyTree: is intended as a READ-ONLY data structure to receive closed path
  //solutions to clipping operations. While this structure is more complex than
  //the alternative TPaths structure, it does model path ownership (ie paths
  //that are contained by other paths). This will be useful to some users.
  TPolyTree = class(TPolyPath);

  //FLOATING POINT POLYGON COORDINATES (D suffix to indicate double precision)
  //To preserve numerical robustness, clipping must be done using integer
  //coordinates. Consequently, polygons that are defined with floating point
  //coordinates will need these converted into integer values together with
  //scaling to achieve the desired floating point precision.

  TClipperD = class(TClipperBase) //for floating point coordinates
  {$IFDEF STRICT}strict{$ENDIF} private
    FScale: double;
    FInvScale: double;
  {$IFDEF USINGZ}
    FZFuncD : TZCallbackD;
  {$ENDIF}
  public
    procedure AddSubject(const pathD: TPathD); overload;
    procedure AddSubject(const pathsD: TPathsD); overload;
    procedure AddOpenSubject(const pathD: TPathD); overload;
    procedure AddOpenSubject(const pathsD: TPathsD); overload;
    procedure AddClip(const pathD: TPathD); overload;
    procedure AddClip(const pathsD: TPathsD); overload;

    constructor Create(scale: double = 0); reintroduce; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions: TPathsD): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions, openSolutions: TPathsD): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      var solutionsTree: TPolyTreeD; out openSolutions: TPathsD): Boolean; overload;
{$IFDEF USINGZ}
    procedure ProxyZFillFunc(const bot1, top1, bot2, top2: TPoint64;
      var intersectPt: TPoint64);
    property ZFillFunc : TZCallbackD read FZFuncD write FZFuncD;
{$ENDIF}
  end;

  TPolyPathD = class(TPolyPathBase)
  {$IFDEF STRICT}strict{$ENDIF} private
    FPath   : TPathD;
  protected
    FScale  : double;
  public
    function  AddChild(const path: TPath64): TPolyPathBase; override;
    property  Polygon: TPathD read FPath;
  end;

  TPolyTreeD = class(TPolyPathD)
  protected
    procedure SetScale(value: double); //alternative to friend class
  public
    property  Scale: double read FScale;
  end;

implementation

//OVERFLOWCHECKS OFF is a necessary workaround for a compiler bug that very
//occasionally reports incorrect overflow errors in Delphi versions before 10.2.
//see https://forums.embarcadero.com/message.jspa?messageID=871444
{$OVERFLOWCHECKS OFF}

resourcestring
  rsClipper_OpenPathErr = 'Only subject paths can be open.';
  rsClipper_PolyTreeErr = 'The TPolyTree parameter must be assigned.';
  rsClipper_ClippingErr = 'Undefined clipping error';

const
  DefaultClipperDScale = 100;

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

procedure RaiseError(const msg: string); {$IFDEF INLINING} inline; {$ENDIF}
begin
  raise EClipperLibException.Create(msg);
end;
//------------------------------------------------------------------------------

function IsOpen(e: PActive): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.LocMin.IsOpen;
end;
//------------------------------------------------------------------------------

function IsOpenEnd(e: PActive): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.LocMin.IsOpen and
    (e.vertTop.Flags * [vfOpenStart, vfOpenEnd] <> []);
end;
//------------------------------------------------------------------------------

function IsOpen(outrec: POutRec): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State = osOpen;
end;
//------------------------------------------------------------------------------

function IsOuter(outrec: POutRec): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State = osOuter;
end;
//------------------------------------------------------------------------------

procedure SetAsOuter(outrec: POutRec); {$IFDEF INLINING} inline; {$ENDIF}
begin
  outrec.State := osOuter;
end;
//------------------------------------------------------------------------------

function IsInner(outrec: POutRec): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State = osInner;
end;
//------------------------------------------------------------------------------

procedure SetAsInner(outrec: POutRec); {$IFDEF INLINING} inline; {$ENDIF}
begin
  outrec.State := osInner;
end;
//------------------------------------------------------------------------------

function IsHotEdge(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := assigned(e.OutRec);
end;
//------------------------------------------------------------------------------

function GetPrevHotEdge(e: PActive): PActive; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.PrevInAEL;
  while assigned(Result) and (IsOpen(Result) or not IsHotEdge(Result)) do
    Result := Result.PrevInAEL;
end;
//------------------------------------------------------------------------------

function IsFront(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  //the front edge will be the LEFT edge when it's an OUTER polygon
  //so that outer polygons will be orientated clockwise

  if (e.OutRec.State = osOpen) then
    Result := e.WindDx > 0 else
    Result := (e = e.OutRec.FrontE);
end;
//------------------------------------------------------------------------------

function IsInvalidPath(op: POutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := not assigned(op) or (op.Next = op);
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
  if (currentY = e.Top.Y) or (e.Top.X = e.Bot.X) then Result := e.Top.X
  else Result := e.Bot.X + Round(e.Dx*(currentY - e.Bot.Y));
end;
//------------------------------------------------------------------------------

function IsHorizontal(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Top.Y = e.Bot.Y);
end;
//------------------------------------------------------------------------------

function IsHeadingRightHorz(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Dx = NegInfinity);
end;
//------------------------------------------------------------------------------

function IsHeadingLeftHorz(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Dx = Infinity);
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
  Result := e.LocMin.PolyType;
end;
//------------------------------------------------------------------------------

function IsSamePolyType(const e1, e2: PActive): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e1.LocMin.PolyType = e2.LocMin.PolyType;
end;
//------------------------------------------------------------------------------

function GetIntersectPoint(e1, e2: PActive): TPoint64;
var
  b1, b2, m: Double;
begin
  if (e1.Dx = e2.Dx) then
  begin
    Result := e1.Top;
    Exit;
  end
  else if e1.Dx = 0 then
  begin
    Result.X := e1.Bot.X;
    if IsHorizontal(e2) then
      Result.Y := e2.Bot.Y
    else
    begin
      with e2^ do b2 := Bot.Y - (Bot.X/Dx);
      Result.Y := round(Result.X/e2.Dx + b2);
    end;
  end
  else if e2.Dx = 0 then
  begin
    Result.X := e2.Bot.X;
    if IsHorizontal(e1) then
      Result.Y := e1.Bot.Y
    else
    begin
      with e1^ do b1 := Bot.Y - (Bot.X/Dx);
      Result.Y := round(Result.X/e1.Dx + b1);
    end;
  end else
  begin
    with e1^ do b1 := Bot.X - Bot.Y * Dx;
    with e2^ do b2 := Bot.X - Bot.Y * Dx;
    m := (b2-b1)/(e1.Dx - e2.Dx);
    Result.Y := round(m);
    if Abs(e1.Dx) < Abs(e2.Dx) then
      Result.X := round(e1.Dx * m + b1) else
      Result.X := round(e2.Dx * m + b2);
  end;
end;
//------------------------------------------------------------------------------

procedure SetDx(e: PActive);  {$IFDEF INLINING} inline; {$ENDIF}
begin
  e.Dx := GetDx(e.Bot, e.Top);
end;
//------------------------------------------------------------------------------

function IsLeftBound(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.WindDx > 0;
end;
//------------------------------------------------------------------------------

function NextVertex(e: PActive): PVertex;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if IsLeftBound(e) then
    Result := e.vertTop.Next else
    Result := e.vertTop.Prev;
end;
//------------------------------------------------------------------------------

function PrevVertex(e: PActive): PVertex;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if IsLeftBound(e) then
    Result := e.vertTop.Prev else
    Result := e.vertTop.Next;
end;
//------------------------------------------------------------------------------

function IsMaxima(e: PActive): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := vfLocMax in e.vertTop.Flags;
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PActive): PActive;
begin
  if IsHorizontal(e) then
  begin
    //we can't be sure whether the MaximaPair is on the left or right, so ...
    Result := e.PrevInAEL;
    while assigned(Result) and (Result.CurrX >= e.Top.X) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.PrevInAEL;
    end;
    Result := e.NextInAEL;
    while assigned(Result) and (TopX(Result, e.Top.Y) <= e.Top.X) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.NextInAEL;
    end;
  end else
  begin
    Result := e.NextInAEL;
    while assigned(Result) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.NextInAEL;
    end;
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
    p := p.Next;
  until p = pts;
end;
//------------------------------------------------------------------------------

function BuildPath(op: POutPt; isOpen: Boolean;
  out path: TPath64): Boolean;
var
  i,j, cnt: integer;
begin
  cnt := PointCount(op);
  setLength(path, cnt);
  path[0] := op.Pt;
  op := op.Next;
  j := 0;
  for i := 0 to cnt -2 do
  begin
    if not PointsEqual(path[j], op.Pt) then
    begin
      inc(j);
      path[j] := op.Pt;
    end;
    op := op.Next;
  end;

  setLength(path, j+1);
  if isOpen then
    Result := (j > 0) else
    Result := (j > 1);
end;
//------------------------------------------------------------------------------

function DisposeOutPt(op: POutPt): POutPt;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if op.Next = op then
    Result := nil else
    Result := op.Next;
  op.Prev.Next := op.Next;
  op.Next.Prev := op.Prev;
  Dispose(Op);
end;
//------------------------------------------------------------------------------

procedure DisposePolyPts(pp: POutPt);  {$IFDEF INLINING} inline; {$ENDIF}
var
  tmpPp: POutPt;
begin
  pp.Prev.Next := nil;
  while Assigned(pp) do
  begin
    tmpPp := pp;
    pp := pp.Next;
    Dispose(tmpPp);
  end;
end;
//------------------------------------------------------------------------------

function LocMinListSort(item1, item2: Pointer): Integer;
var
  dy: Int64;
begin
  dy := PLocalMinima(item2).Vertex.Pt.Y - PLocalMinima(item1).Vertex.Pt.Y;
  if dy < 0 then Result := -1
  else if dy > 0 then Result := 1
  else Result := 0;
end;
//------------------------------------------------------------------------------

procedure SetSides(outRec: POutRec; startEdge, endEdge: PActive);
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  outRec.FrontE := startEdge;
  outRec.BackE := endEdge;
end;
//------------------------------------------------------------------------------

procedure SwapOutRecs(e1, e2: PActive);
var
  or1, or2: POutRec;
  e: PActive;
begin
  or1 := e1.OutRec;
  or2 := e2.OutRec;
  if (or1 = or2) then
  begin
    e := or1.FrontE;
    or1.FrontE := or1.BackE;
    or1.BackE := e;
    Exit;
  end;
  if assigned(or1) then
  begin
    if e1 = or1.FrontE then
      or1.FrontE := e2 else
      or1.BackE := e2;
  end;
  if assigned(or2) then
  begin
    if e2 = or2.FrontE then
      or2.FrontE := e1 else
      or2.BackE := e1;
  end;
  e1.OutRec := or2;
  e2.OutRec := or1;
end;
//------------------------------------------------------------------------------

//Area result in a coordinate system where 0,0 is top/left
//(is opposite to cartesian coordinate system having 0,0 at bottom left).
function Area(op: POutPt): Double;
var
  op2: POutPt;
  d: Double;
begin
  Result := 0;
  if not Assigned(op) then Exit;
  op2 := op;
  repeat
    Result := Result +
      double(op2.Prev.Pt.Y + op2.Pt.Y) *
      (op2.Prev.Pt.X - op2.Pt.X);
    op2 := op2.Next;
  until op2 = op;
  Result := Result * 0.5;
end;
//------------------------------------------------------------------------------

procedure ReverseOutPts(op: POutPt);
var
  op1, op2: POutPt;
begin
  if not Assigned(op) then Exit;
  op1 := op;
  repeat
    op2:= op1.Next;
    op1.Next := op1.Prev;
    op1.Prev := op2;
    op1 := op2;
  until op1 = op;
end;
//------------------------------------------------------------------------------

procedure CheckFixInnerOuter(e: PActive);
var
  wasOuter, isOuter: Boolean;
  e2: PActive;
begin
  wasOuter := Clipper.Engine.IsOuter(e.OutRec);
  isOuter := true;
  e2 := e.PrevInAEL;
  while assigned(e2) do
  begin
   if IsHotEdge(e2) and not IsOpen(e2) then isOuter := not isOuter;
   e2 := e2.PrevInAEL;
  end;

  if isOuter <> wasOuter then
  begin
    if isOuter then SetAsOuter(e.outrec)
    else SetAsInner(e.outrec);
  end;

  e2 := GetPrevHotEdge(e);
  if isOuter then
  begin
    if assigned(e2) and IsInner(e2.OutRec) then e.OutRec.Owner := e2.OutRec
    else e.OutRec.Owner := nil;
  end else
  begin
    if not assigned(e2) then SetAsOuter(e.OutRec)
    else if IsInner(e2.OutRec) then e.OutRec.Owner := e2.OutRec.Owner
    else e.OutRec.Owner := e2.OutRec;
  end;

  if (Area(e.outrec.Pts) > 0) <> isOuter then
    ReverseOutPts(e.outrec.Pts);
end;
//------------------------------------------------------------------------------

procedure SwapSides(outRec: POutRec); {$IFDEF INLINING} inline; {$ENDIF}
var
  e2: PActive;
begin
  e2 := outRec.FrontE;
  outRec.FrontE := outRec.BackE;
  outRec.BackE := e2;
  outRec.Pts := outRec.Pts.Next;
end;
//------------------------------------------------------------------------------

function ValidatePath(var op: POutPt): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (op.Next <> op) and (op.Next <> op.Prev);
  if Result then Exit;
  if (op.Next <> op) then Dispose(op.Next);
  Dispose(op);
  op := nil;
end;
//------------------------------------------------------------------------------

function FixSides(e: PActive): Boolean;
begin
  if ValidatePath(e.OutRec.Pts) then
  begin
    CheckFixInnerOuter(e);
    Result := (IsOuter(e.OutRec) <> IsFront(e));
    if not Result then Exit; //ie swapping sides won't fix
  end;
  SwapSides(e.OutRec);
  Result := true;
end;
//------------------------------------------------------------------------------

procedure SetOwnerAndInnerOuterState(e: PActive);
var
  e2: PActive;
  outRec: POutRec;
begin
  outRec := e.OutRec;
  if IsOpen(e) then
  begin
    outRec.Owner := nil;
    outRec.State := osOpen;
    Exit;
  end;
  //set owner ...
  if IsHeadingLeftHorz(e) then
  begin
    e2 := e.NextInAEL; //ie assess state from opposite direction
    while assigned(e2) and (not IsHotEdge(e2) or IsOpen(e2)) do
      e2 := e2.NextInAEL;
    if not assigned(e2) then outRec.Owner := nil
    else if IsOuter(e2.OutRec) = (e2.OutRec.FrontE = e2) then
      outRec.Owner := e2.OutRec.Owner
    else
      outRec.Owner := e2.OutRec;
  end else
  begin
    e2 := GetPrevHotEdge(e);
    if not assigned(e2) then
      outRec.Owner := nil
    else if IsOuter(e2.OutRec) = (e2.OutRec.BackE = e2) then
      outRec.Owner := e2.OutRec.Owner
    else
      outRec.Owner := e2.OutRec;
  end;

  //set inner/outer ...
  if not assigned(outRec.Owner) or IsInner(outRec.Owner) then
    outRec.State := osOuter else
    outRec.State := osInner;

end;
//------------------------------------------------------------------------------

function EdgesAdjacentInAEL(node: PIntersectNode): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  with node^ do
    Result := (Edge1.NextInAEL = Edge2) or (Edge1.PrevInAEL = Edge2);
end;
//------------------------------------------------------------------------------

function IntersectListSort(node1, node2: Pointer): Integer;
var
  i1: PIntersectNode absolute node1;
  i2: PIntersectNode absolute node2;
begin
  result := i2.Pt.Y - i1.Pt.Y;
  if (result = 0) and (i1 <> i2) then
    result := i1.Pt.X - i2.Pt.X;
end;

//------------------------------------------------------------------------------
// TClipperBase methods ...
//------------------------------------------------------------------------------

constructor TClipperBase.Create;
begin
  FLocMinList       := TList.Create;
  FOutRecList       := TList.Create;
  FIntersectList    := TList.Create;
  FVertexArrayList  := TList.Create;
  FPreserveCollinear := true;
end;
//------------------------------------------------------------------------------

destructor TClipperBase.Destroy;
begin
  Clear;
  FLocMinList.Free;
  FOutRecList.Free;
  FIntersectList.Free;
  FVertexArrayList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.CleanUp;
var
  dummy: Int64;
begin
  try
    //in case of exceptions ...
    while assigned(FActives) do DeleteFromAEL(FActives);
    while assigned(FScanLine) do PopScanLine(dummy);
    DisposeIntersectNodes;

    DisposeScanLineList;
    DisposeAllOutRecs;
  except
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Clear;
begin
  CleanUp;
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
    InsertScanLine(PLocalMinima(FLocMinList[i]).Vertex.Pt.Y);
  FCurrentLocMinIdx := 0;
  FActives := nil;
  FSel := nil;
end;
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
function XYCoordsEqual(const pt1, pt2: TPoint64): Boolean;
begin
  Result := (pt1.X = pt2.X) and (pt1.Y = pt2.Y);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SetZ(e1, e2: PActive; var intersectPt: TPoint64);
var
  e3, e4: PActive;
begin
  if not Assigned(FZFunc) then Exit;

  //prioritize subject vertices over clip vertices
  //and pass the subject vertices before clip vertices in the callback
  if (GetPolyType(e1) = ptSubject) then
  begin
    e3 := e1; e4 := e2;
  end else
  begin
    e3 := e2; e4 := e1;
  end;

  if (XYCoordsEqual(intersectPt, e3.bot)) then intersectPt.Z := e3.bot.Z
  else if (XYCoordsEqual(intersectPt, e4.bot)) then intersectPt.Z := e4.bot.Z
  else if (XYCoordsEqual(intersectPt, e3.top)) then intersectPt.Z := e3.top.Z
  else if (XYCoordsEqual(intersectPt, e4.top)) then intersectPt.Z := e4.top.Z;
  FZFunc(e3.bot, e3.top, e4.bot, e4.top, intersectPt);
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure TClipperBase.InsertScanLine(const Y: Int64);
var
  newSl, sl: PScanLine;
begin
  //The scanline list is a single-linked list of all the Y coordinates of
  //subject and clip vertices in the clipping operation (sorted descending).
  //However, only scanline Y's at Local Minima are inserted before clipping
  //starts. While scanlines are removed sequentially during the sweep operation,
  //new scanlines are only inserted whenever edge bounds are updated. This keeps
  //the scanline list relatively short, optimising performance.
  if not Assigned(FScanLine) then
  begin
    new(newSl);
    newSl.Y := Y;
    FScanLine := newSl;
    newSl.Next := nil;
  end else if Y > FScanLine.Y then
  begin
    new(newSl);
    newSl.Y := Y;
    newSl.Next := FScanLine;
    FScanLine := newSl;
  end else
  begin
    sl := FScanLine;
    while Assigned(sl.Next) and (Y <= sl.Next.Y) do
      sl := sl.Next;
    if Y = sl.Y then Exit; //skip duplicates
    new(newSl);
    newSl.Y := Y;
    newSl.Next := sl.Next;
    sl.Next := newSl;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.PopScanLine(out Y: Int64): Boolean;
var
  sl: PScanLine;
begin
  Result := assigned(FScanLine);
  if not Result then Exit;
  Y := FScanLine.Y;
  sl := FScanLine;
  FScanLine := FScanLine.Next;
  dispose(sl);
end;
//------------------------------------------------------------------------------

function TClipperBase.PopLocalMinima(Y: Int64;
  out localMinima: PLocalMinima): Boolean;
begin
  Result := false;
  if FCurrentLocMinIdx = FLocMinList.Count then Exit;
  localMinima := PLocalMinima(FLocMinList[FCurrentLocMinIdx]);
  if (localMinima.Vertex.Pt.Y = Y) then
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
    sl := FScanLine.Next;
    Dispose(FScanLine);
    FScanLine := sl;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeOutRec(index: Integer);
var
  outRec: POutRec;
begin
  outRec := FOutRecList[index];
  if Assigned(outRec.Pts) then DisposePolyPts(outRec.Pts);
  Dispose(outRec);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeAllOutRecs;
var
  i: Integer;
begin
  for i := 0 to FOutRecList.Count -1 do DisposeOutRec(i);
  FOutRecList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeVerticesAndLocalMinima;
var
  i: Integer;
begin
  for i := 0 to FLocMinList.Count -1 do
    Dispose(PLocalMinima(FLocMinList[i]));
  FLocMinList.Clear;
  for i := 0 to FVertexArrayList.Count -1 do
    FreeMem(FVertexArrayList[i]);
  FVertexArrayList.Clear;
end;
//------------------------------------------------------------------------------

procedure SetVertex(v, prevV: PVertex; const point: TPoint64);
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  v.pt := point;
  if assigned(prevV) then
    prevV.Next := v;
  v.Next := nil;
  v.Prev := prevV;
  v.Flags := [];
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddPathToVertexList(const p: TPath64;
  polyType: TPathType; isOpen: Boolean);
var
  i, highI: integer;
  va0, vaCurr, vaPrev: PVertex;
  ascending, ascending0: Boolean;

  procedure AddLocMin(vert: PVertex);
  var
    lm: PLocalMinima;
  begin
    if vfLocMin in vert.Flags then Exit;  //ie already added
    Include(vert.Flags, vfLocMin);
    new(lm);
    lm.Vertex := vert;
    lm.PolyType := polyType;
    lm.IsOpen := isOpen;
    FLocMinList.Add(lm);                  //nb: sorted in Reset()
  end;
  //---------------------------------------------------------

begin
  highI := high(p);
  if not isOpen then
  begin
    while (highI > 0) and PointsEqual(p[highI], p[0]) do dec(highI);
    if (highI < 2) then Exit;
  end
  else if (highI < 1) then Exit;

  GetMem(va0, sizeof(TVertex) * (highI +1));
  FVertexArrayList.Add(va0);

  SetVertex(va0, nil, p[0]);

  if isOpen then
  begin
    i := 1;
    while (i < highI) and (p[i].Y = p[0].Y) do inc(i);
    ascending := p[i].Y <= p[0].Y;
    if ascending then
    begin
      va0.Flags := [vfOpenStart];
      AddLocMin(va0);
    end else
      va0.Flags := [vfOpenStart, vfLocMax];
  end
  else if p[0].Y = p[highI].Y then
  begin
    //since path[0] and path[highI] are horizontal
    //find the first prior non-horizontal pt
    i := (highI -1);
    while (i > 0) and (p[i].Y = p[highI].Y) do dec(i);
    if (i = 0) then Exit; //path is entirely horizontal
    //get the initial winding direction
    ascending := p[0].Y < p[i].Y;
  end else
    ascending := p[0].Y < p[highI].Y;

  ascending0 := ascending; //save the initial winding direction
  vaCurr := va0;
  for i := 1 to highI do
  begin
    if PointsEqual(p[i], vaCurr.pt) then Continue; //skip duplicate
    vaPrev := vaCurr;
    inc(vaCurr);
    SetVertex(vaCurr, vaPrev, p[i]);

    if ascending and (vaCurr.pt.Y > vaPrev.pt.Y) then
    begin
      Include(vaPrev.Flags, vfLocMax);
      ascending := false;
    end
    else if not ascending and (vaCurr.pt.Y < vaPrev.pt.Y) then
    begin
      AddLocMin(vaPrev);
      ascending := true;
    end;
  end;

  vaCurr.Next := va0;
  va0.Prev := vaCurr;

  if isOpen then
  begin
    Include(vaCurr.Flags, vfOpenEnd);
    if ascending then
      Include(vaCurr.Flags, vfLocMax) else
      AddLocMin(vaCurr);
  end
  else if ascending0 <> ascending then
  begin
    if ascending0 then
      AddLocMin(vaCurr) else
      Include(vaCurr.Flags, vfLocMax);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddPath(const path: TPath64;
  pathType: TPathType; isOpen: Boolean);
begin
  if isOpen then FHasOpenPaths := true;
  FLocMinListSorted := false;
  AddPathToVertexList(path, pathType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddPaths(const paths: TPaths64;
  pathType: TPathType; isOpen: Boolean);
var
  i: integer;
begin
  if isOpen then FHasOpenPaths := true;
  FLocMinListSorted := false;
  for i := 0 to High(paths) do
    AddPathToVertexList(paths[i], pathType, isOpen);
end;
//------------------------------------------------------------------------------

function TClipperBase.IsContributingClosed(e: PActive): Boolean;
begin
  Result := false;
  case FFillRule of
    frNonZero: if abs(e.WindCnt) <> 1 then Exit;
    frPositive: if (e.WindCnt <> 1) then Exit;
    frNegative: if (e.WindCnt <> -1) then Exit;
  end;

  case FClipType of
    ctIntersection:
      case FFillRule of
        frEvenOdd, frNonZero: Result := (e.WindCnt2 <> 0);
        frPositive: Result := (e.WindCnt2 > 0);
        frNegative: Result := (e.WindCnt2 < 0);
      end;
    ctUnion:
      case FFillRule of
        frEvenOdd, frNonZero: Result := (e.WindCnt2 = 0);
        frPositive: Result := (e.WindCnt2 <= 0);
        frNegative: Result := (e.WindCnt2 >= 0);
      end;
    ctDifference:
      if GetPolyType(e) = ptSubject then
        case FFillRule of
          frEvenOdd, frNonZero: Result := (e.WindCnt2 = 0);
          frPositive: Result := (e.WindCnt2 <= 0);
          frNegative: Result := (e.WindCnt2 >= 0);
        end
      else
        case FFillRule of
          frEvenOdd, frNonZero: Result := (e.WindCnt2 <> 0);
          frPositive: Result := (e.WindCnt2 > 0);
          frNegative: Result := (e.WindCnt2 < 0);
        end;
    ctXor:
        Result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.IsContributingOpen(e: PActive): Boolean;
begin
    case FClipType of
      ctIntersection:
        Result := (e.WindCnt2 <> 0);
      ctXor:
        Result := (e.WindCnt <> 0) <> (e.WindCnt2 <> 0);
      ctDifference:
        Result := (e.WindCnt2 = 0);
      else //ctUnion:
        Result := (e.WindCnt = 0) and (e.WindCnt2 = 0);
    end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SetWindCountForClosedPathEdge(e: PActive);
var
  e2: PActive;
begin
  //Wind counts refer to polygon regions not edges, so here an edge's WindCnt
  //indicates the higher of the wind counts for the two regions touching the
  //edge. (nb: Adjacent regions can only ever have their wind counts differ by
  //one. Also, open paths have no meaningful wind directions or counts.)

  e2 := e.PrevInAEL;
  //find the nearest closed path edge of the same PolyType in AEL (heading left)
  while Assigned(e2) and (not IsSamePolyType(e2, e) or IsOpen(e2)) do
    e2 := e2.PrevInAEL;

  if not Assigned(e2) then
  begin
    e.WindCnt := e.WindDx;
    e2 := FActives;
  end
  else if (FFillRule = frEvenOdd) then
  begin
    e.WindCnt := e.WindDx;
    e.WindCnt2 := e2.WindCnt2;
    e2 := e2.NextInAEL;
  end else
  begin
    //NonZero, positive, or negative filling here ...
    //if e's WindCnt is in the SAME direction as its WindDx, then polygon
    //filling will be on the right of 'e'.
    //nb: neither e2.WindCnt nor e2.WindDx should ever be 0.
    if (e2.WindCnt * e2.WindDx < 0) then
    begin
      //opposite directions so 'e' is outside 'e2' ...
      if (Abs(e2.WindCnt) > 1) then
      begin
        //outside prev poly but still inside another.
        if (e2.WindDx * e.WindDx < 0) then
          //reversing direction so use the same WC
          e.WindCnt := e2.WindCnt else
          //otherwise keep 'reducing' the WC by 1 (ie towards 0) ...
          e.WindCnt := e2.WindCnt + e.WindDx;
      end
      //now outside all polys of same polytype so set own WC ...
      else e.WindCnt := e.WindDx;
    end else
    begin
      //'e' must be inside 'e2'
      if (e2.WindDx * e.WindDx < 0) then
        //reversing direction so use the same WC
        e.WindCnt := e2.WindCnt
      else
        //otherwise keep 'increasing' the WC by 1 (ie away from 0) ...
        e.WindCnt := e2.WindCnt + e.WindDx;
    end;
    e.WindCnt2 := e2.WindCnt2;
    e2 := e2.NextInAEL;
  end;

  //update WindCnt2 ...
  if FFillRule = frEvenOdd then
    while (e2 <> e) do
    begin
      if IsSamePolyType(e2, e) or IsOpen(e2) then //do nothing
      else if e.WindCnt2 = 0 then e.WindCnt2 := 1
      else e.WindCnt2 := 0;
      e2 := e2.NextInAEL;
    end
  else
    while (e2 <> e) do
    begin
      if not IsSamePolyType(e2, e) and not IsOpen(e2) then
        Inc(e.WindCnt2, e2.WindDx);
      e2 := e2.NextInAEL;
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
      e2 := e2.NextInAEL;
    end;
    if Odd(cnt1) then e.WindCnt := 1 else e.WindCnt := 0;
    if Odd(cnt2) then e.WindCnt2 := 1 else e.WindCnt2 := 0;
  end else
  begin
    //if FClipType in [ctUnion, ctDifference] then e.WindCnt := e.WindDx;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(e.WindCnt2, e2.WindDx)
      else if not IsOpen(e2) then inc(e.WindCnt, e2.WindDx);
      e2 := e2.NextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function IsValidAelOrder(a1, a2: PActive): Boolean;
var
  pt: TPoint64;
  a1IsNewEdge, a2IsNewEdge: Boolean;
  d: double;
begin
  if a2.CurrX <> a1.CurrX then
  begin
    Result := a2.CurrX > a1.CurrX;
    Exit;
  end;

  //find the common base point then get the turning
  //direction (left or right) of the edge top points
  if (a1.bot.Y < a2.Bot.Y) then pt := a1.bot
  else pt := a2.bot;
  d := CrossProduct(a1.Top, pt, a2.top);
  if d <> 0 then
  begin
    Result := d < 0; //true when left turning
    Exit;
  end;

  //edges must be collinear to get here

  //for starting open paths, place them according to
  //the direction they're about to turn
  if IsOpen(a1) and not IsMaxima(a1) and
    (a1.bot.Y <= a2.bot.Y) and
    not IsSamePolyType(a1, a2) and
    (a1.top.Y > a2.top.Y) then
  begin
    Result := CrossProduct(a1.Bot, a1.Top,
      NextVertex(a1).Pt) <= 0;
    Exit;
  end
  else if IsOpen(a2) and not IsMaxima(a2) and
    (a2.bot.Y <= a1.bot.Y) and
    not IsSamePolyType(a1, a2) and
    (a2.top.Y > a1.top.Y) then
  begin
    Result := CrossProduct(a2.Bot, a2.Top,
      NextVertex(a2).Pt) >= 0;
    Exit;
  end;

  a1IsNewEdge := (a1.bot.Y <= a2.bot.Y) and
    (PrevVertex(a1) = a1.LocMin.Vertex);
  a2IsNewEdge := (a2.bot.Y <= a1.bot.Y) and
    (PrevVertex(a2) = a2.LocMin.Vertex);

  if IsHorizontal(a1) then
  begin
    if IsHorizontal(a2) then Result := a1.top.X < a2.top.X
    else Result := a1.Top.X < a1.bot.X;
  end
  else if IsHorizontal(a2) then
  begin
    Result := a2.Top.X > a2.bot.X;
  end
  //New *left edges* prefer placement to the right of collinear edges to
  //preserve proximity with their corresponding *right edges*. *Right edges*
  //likewise prefer left placement. With open edges and collinearity,
  //placement is undefined.
  else if (a2IsNewEdge) then Result := IsLeftBound(a2)
  else if (a1IsNewEdge) then Result := not IsLeftBound(a1)
  else Result := true; //open edge and undefined, let's stop here
end;
//------------------------------------------------------------------------------

procedure TClipperBase.InsertLeftEdge(e: PActive);
var
  e2: PActive;
begin
  if not Assigned(FActives) then
  begin
    e.PrevInAEL := nil;
    e.NextInAEL := nil;
    FActives := e;
  end
  else if IsValidAelOrder(e, FActives) then
  begin
    e.PrevInAEL := nil;
    e.NextInAEL := FActives;
    FActives.PrevInAEL := e;
    FActives := e;
  end else
  begin
    e2 := FActives;
    while Assigned(e2.NextInAEL) and IsValidAelOrder(e2.NextInAEL, e) do
      e2 := e2.NextInAEL;
    e.NextInAEL := e2.NextInAEL;
    if Assigned(e2.NextInAEL) then e2.NextInAEL.PrevInAEL := e;
    e.PrevInAEL := e2;
    e2.NextInAEL := e;
  end;
end;
//----------------------------------------------------------------------

procedure InsertRightEdge(e, e2: PActive);
begin
  e2.NextInAEL := e.NextInAEL;
  if Assigned(e.NextInAEL) then e.NextInAEL.PrevInAEL := e2;
  e2.PrevInAEL := e;
  e.NextInAEL := e2;
end;
//----------------------------------------------------------------------

procedure TClipperBase.InsertLocalMinimaIntoAEL(const botY: Int64);
var
  leftB, rightB: PActive;
  locMin: PLocalMinima;
  contributing: Boolean;
begin
  //Add local minima (if any) at BotY ...
  //nb: horizontal local minima edges should contain locMin.Vertex.prev

  while PopLocalMinima(botY, locMin) do
  begin
    if (vfOpenStart in locMin.Vertex.Flags) then
    begin
      leftB := nil;
    end else
    begin
      new(leftB);
      FillChar(leftB^, sizeof(TActive), 0);
      leftB.LocMin := locMin;
      leftB.OutRec := nil;
      leftB.Bot := locMin.Vertex.Pt;
      leftB.vertTop := locMin.Vertex.Prev; //ie descending
      leftB.Top := leftB.vertTop.Pt;
      leftB.CurrX := leftB.Bot.X;
      leftB.WindDx := -1;
      SetDx(leftB);
    end;

    if (vfOpenEnd in locMin.Vertex.Flags) then
    begin
      rightB := nil;
    end else
    begin
      new(rightB);
      FillChar(rightB^, sizeof(TActive), 0);
      rightB.LocMin := locMin;
      rightB.OutRec := nil;
      rightB.Bot := locMin.Vertex.Pt;
      rightB.vertTop := locMin.Vertex.Next; //ie ascending
      rightB.Top := rightB.vertTop.Pt;
      rightB.CurrX := rightB.Bot.X;
      rightB.WindDx := 1;
      SetDx(rightB);
    end;
    //Currently LeftB is just the descending bound and RightB is the ascending.
    //Now if the LeftB isn't on the left of RightB then we need swap them.
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
      else if (leftB.Dx < rightB.Dx) then SwapActives(leftB, rightB);
    end
    else if not assigned(leftB) then
    begin
      leftB := rightB;
      rightB := nil;
    end;

    InsertLeftEdge(leftB);                   ///////
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
      rightB.WindCnt := leftB.WindCnt;
      rightB.WindCnt2 := leftB.WindCnt2;
      InsertRightEdge(leftB, rightB);        ///////
      if contributing then
        AddLocalMinPoly(leftB, rightB, leftB.Bot, true);

      while Assigned(rightB.NextInAEL) and
        IsValidAelOrder(rightB.NextInAEL, rightB) do
      begin
        IntersectEdges(rightB, rightB.NextInAEL, rightB.Bot);
        SwapPositionsInAEL(rightB, rightB.NextInAEL);
      end;

      if IsHorizontal(rightB) then
        PushHorz(rightB) else
        InsertScanLine(rightB.Top.Y);
    end
    else if contributing then
      StartOpenPath(leftB, leftB.Bot);

    if IsHorizontal(leftB) then
      PushHorz(leftB) else
      InsertScanLine(leftB.Top.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.PushHorz(e: PActive);
begin
  if assigned(FSel) then
    e.NextInSEL := FSel else
    e.NextInSEL := nil;
  FSel := e;
end;
//------------------------------------------------------------------------------

function TClipperBase.PopHorz(out e: PActive): Boolean;
begin
  Result := assigned(FSel);
  if not Result then Exit;
  e := FSel;
  FSel := FSel.NextInSEL;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddLocalMinPoly(e1, e2: PActive;
  const pt: TPoint64; IsNew: Boolean = false): POutPt;
var
  outRec: POutRec;
begin
  new(outRec);
  outRec.Idx := FOutRecList.Add(outRec);
  outRec.Pts := nil;
  outRec.PolyPath := nil;

  e1.OutRec := outRec;
  SetOwnerAndInnerOuterState(e1);
  e2.OutRec := outRec;
  if not IsOpen(e1) then
  begin
    //Setting the owner and inner/outer states (above) is an essential
    //precursor to setting edge 'sides' (ie left and right sides of output
    //polygons) and hence the orientation of output paths ...
    if IsOuter(outRec) = IsNew then
      SetSides(outRec, e1, e2) else
      SetSides(outRec, e2, e1);
  end;

  new(Result);
  outRec.Pts := Result;
  Result.Pt := pt;
  Result.Prev := Result;
  Result.Next := Result;
end;
//------------------------------------------------------------------------------

procedure CleanCollinear(var op: POutPt; preserveCollinear: Boolean);
var
  op2, startOp: POutPt;
begin
  if not ValidatePath(op) then Exit;
  startOp := op;
  op2 := op;
  while true do
  begin
    if (CrossProduct(op2.Prev.Pt, op2.Pt, op2.Next.Pt) = 0) and
      (not preserveCollinear or
      (DotProduct(op2.Prev.Pt, op2.Pt, op2.Next.Pt) < 0)) then //ie 180deg. spike
    begin
      if op2 = op then op := op2.Prev;
      op2 := DisposeOutPt(op2);
      if not ValidatePath(op2) then
      begin
        op := nil;
        Exit;
      end;
      startOp := op2;
      Continue;
    end else
     op2 := op2.Next;
    if op2 = startOp then Break;
  end;
end;
//------------------------------------------------------------------------------

function AreaTriangle(const pt1, pt2, pt3: TPoint64): double;
begin
  Result := 0.5 * (pt1.X - pt3.X)*(pt2.Y - pt1.Y) -
    (pt1.X - pt2.X)*(pt3.Y - pt1.Y);
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
    prevOp := splitOp.Prev;
    nextNextOp := splitOp.Next.Next;
    Result := prevOp;
    ip := Point64(Clipper.Core.GetIntersectPoint(
      prevOp.Pt, splitOp.Pt, splitOp.Next.Pt, nextNextOp.Pt));
    {$IFDEF USINGZ}
      //???
    {$ENDIF}
    area1 := Area(op);
    area2 := AreaTriangle(ip, splitOp.Pt, splitOp.Next.Pt);

    if PointsEqual(ip, prevOp.Pt) or
      PointsEqual(ip, nextNextOp.Pt) then
    begin
      nextNextOp.Prev := prevOp;
      prevOp.Next := nextNextOp;
    end else
    begin
      new(newOp2);
      newOp2.Pt := ip;
      newOp2.Prev := prevOp;
      newOp2.Next := nextNextOp;
      nextNextOp.Prev := newOp2;
      prevOp.Next := newOp2;
    end;

    if ((Abs(area2) > (Abs(area1))) or
      ((area2 > 0) = (area1 > 0))) then
    begin
      new(newOutRec);
      newOutRec.Idx := FOutRecList.Add(newOutRec);
      newOutRec.PolyPath := nil;

      new(newOp);
      newOp.Pt := ip;
      newOp.Prev := splitOp.Next;
      newOp.Next := splitOp;
      newOutRec.Pts := newOp;
      splitOp.Prev := newOp;
      splitOp.Next.Next := newOp;
    end else
    begin
      Dispose(splitOp.Next);
      Dispose(splitOp);
    end;
  end;

var
  op2: POutPt;
begin
  //precondition:
  //the 'op' linked list's length must be at least 3
  op2 := op;
  while true do
  begin
    //3 edged polygons can't self-intersect
    if (op2.Prev = op2.Next.Next) then
      Break
    else if SegmentsIntersect(op2.Prev.Pt, op2.Pt,
      op2.Next.Pt, op2.Next.Next.Pt) then
    begin
      if (op2 = op) or (op2.Next = op) then
        op := op2.Prev;
      op2 := DoSplitOp(op2);
      op := op2;
      Continue;
    end else
      op2 := op2.Next;
    if (op2 = op) then Break;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64): POutPt;
var
  outRec: POutRec;
begin
  if not IsOpen(e1) and (IsFront(e1) = IsFront(e2)) then
    if not FixSides(e1) then FixSides(e2);
  Result := AddOutPt(e1, pt);

  if  (e1.OutRec = e2.OutRec) then
  begin
    outRec := e1.OutRec;
    e1.outRec.FrontE := nil;
    e1.outRec.BackE := nil;
    e1.OutRec := nil;
    e2.OutRec := nil;
    if IsOpen(e1) then Exit;

    CleanCollinear(outRec.Pts, FPreserveCollinear);
    if assigned(outRec.Pts) then
      FixSelfIntersects(outRec.Pts);
    Result := outRec.Pts;
    if assigned(Result) and
      (IsOuter(outRec) <> (Area(Result) > 0)) then
        ReverseOutPts(outRec.Pts);
  end
  //and to preserve the winding orientation of Outrec ...
  else if e1.OutRec.Idx < e2.OutRec.Idx then
    JoinOutrecPaths(e1, e2) else
    JoinOutrecPaths(e2, e1);

end;
//------------------------------------------------------------------------------

procedure TClipperBase.JoinOutrecPaths(e1, e2: PActive);
var
  p1_start, p1_end, p2_start, p2_end: POutPt;
begin
  if (IsFront(e1) = IsFront(e2)) then
  begin
    //one or other 'side' must be wrong ...
    if IsOpen(e1) then SwapSides(e2.OutRec)
    else if not FixSides(e1) and not FixSides(e2) then
      RaiseError(rsClipper_ClippingErr);
    if e1.OutRec.Owner = e2.OutRec then
      e1.OutRec.Owner := e2.OutRec.Owner;
  end;

  //join e2 outrec path onto e1 outrec path and then delete e2 outrec path
  //pointers. (see joining_outpt.svg)
  p1_start :=  e1.OutRec.Pts;
  p2_start :=  e2.OutRec.Pts;
  p1_end := p1_start.Next;
  p2_end := p2_start.Next;

  if IsFront(e1) then
  begin
    p2_end.Prev := p1_start;
    p1_start.Next := p2_end;
    p2_start.Next := p1_end;
    p1_end.Prev := p2_start;
    e1.OutRec.Pts := p2_start;
    if IsOpen(e1) then
    begin
      e1.OutRec.Pts := p2_start;
    end else
    begin
      e1.OutRec.FrontE := e2.OutRec.FrontE;
      e1.OutRec.FrontE.OutRec := e1.OutRec;
    end;
    //strip duplicates ...
    if (p2_end <> p2_start) and PointsEqual(p2_end.Pt, p2_end.Prev.Pt) then
      DisposeOutPt(p2_end);
  end else
  begin
    p1_end.Prev := p2_start;
    p2_start.Next := p1_end;
    p1_start.Next := p2_end;
    p2_end.Prev := p1_start;
    if IsOpen(e1) then
    begin
      e1.OutRec.Pts := p1_start;
    end else
    begin
      e1.OutRec.BackE := e2.OutRec.BackE;
      e1.OutRec.BackE.OutRec := e1.OutRec;
    end;
    //strip duplicates ...
    if (p1_end <> p1_start) and PointsEqual(p1_end.Pt, p1_end.Prev.Pt) then
      DisposeOutPt(p1_end);
  end;

  if PointsEqual(e1.OutRec.Pts.Pt, e1.OutRec.Pts.Prev.Pt) and
    not IsInvalidPath(e1.OutRec.Pts) then
      DisposeOutPt(e1.OutRec.Pts.Prev);

  //after joining, the e2.OutRec must contains no vertices ...
  e2.OutRec.FrontE := nil;
  e2.OutRec.BackE := nil;
  e2.OutRec.Pts := nil;
  e2.OutRec.Owner := e1.OutRec; //this may be redundant

  if IsOpenEnd(e1) then
  begin
    e2.OutRec.Pts := e1.OutRec.Pts;
    e1.OutRec.Pts := nil;
  end;

  //and e1 and e2 are maxima and are about to be dropped from the Actives list.
  e1.OutRec := nil;
  e2.OutRec := nil;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddOutPt(e: PActive; const pt: TPoint64): POutPt;
var
  opFront, opBack: POutPt;
  toFront: Boolean;
  outrec: POutRec;
begin
  //Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
  //opFront[.Prev]* ~~~> opBack & opBack == opFront.Next
  outrec := e.OutRec;
  toFront := IsFront(e);
  opFront := outrec.Pts;
  opBack := opFront.Next;
  if toFront and PointsEqual(pt, opFront.Pt) then
    result := opFront
  else if not toFront and PointsEqual(pt, opBack.Pt) then
    result := opBack
  else
  begin
    new(Result);
    Result.Pt := pt;
    opBack.Prev := Result;
    Result.Prev := opFront;
    Result.Next := opBack;
    opFront.Next := Result;
    if toFront then outrec.Pts := Result;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.StartOpenPath(e: PActive; const pt: TPoint64): POutPt;
var
  outRec: POutRec;
begin
  new(outRec);
  outRec.Idx := FOutRecList.Add(outRec);
  outRec.Owner := nil;
  outRec.State := osOpen;
  outRec.Pts := nil;
  outRec.PolyPath := nil;
  outRec.FrontE := nil;
  outRec.BackE := nil;
  e.OutRec := outRec;

  new(Result);
  outRec.Pts := Result;
  Result.Pt := pt;
  Result.Prev := Result;
  Result.Next := Result;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.UpdateEdgeIntoAEL(var e: PActive);
begin
  e.Bot := e.Top;
  e.vertTop := NextVertex(e);
  e.Top := e.vertTop.Pt;
  e.CurrX := e.Bot.X;
  SetDx(e);
  if not IsHorizontal(e) then InsertScanLine(e.Top.Y);
end;
//------------------------------------------------------------------------------

{$IFNDEF USINGZ}
{$HINTS OFF}
{$ENDIF}
procedure TClipperBase.IntersectEdges(e1, e2: PActive; pt: TPoint64);
var
  e1WindCnt, e2WindCnt, e1WindCnt2, e2WindCnt2: Integer;
  op, op2: POutPt;
begin

  //MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
  if FHasOpenPaths and (IsOpen(e1) or IsOpen(e2)) then
  begin
    if (IsOpen(e1) and IsOpen(e2) ) then Exit;
    //the following line avoids duplicating a whole lot of code ...
    if IsOpen(e2) then SwapActives(e1, e2);
    case FClipType of
      ctIntersection, ctDifference:
        if IsSamePolyType(e1, e2) or
          (abs(e2.WindCnt) <> 1) then Exit;
      ctUnion:
        if IsHotEdge(e1) <> ((abs(e2.WindCnt) <> 1) or
          (IsHotEdge(e1) <> (e2.WindCnt2 <> 0))) then Exit; //just works!
      ctXor:
        if (abs(e2.WindCnt) <> 1) then Exit;
    end;
    //toggle contribution ...
    if IsHotEdge(e1) then
    begin
      op := AddOutPt(e1, pt);
      {$IFDEF USINGZ}
      SetZ(e1, e2, op.pt);
      {$ENDIF}
      e1.OutRec := nil;
    end
    else StartOpenPath(e1, pt);
    Exit;
  end;

  //MANAGING CLOSED PATHS FROM HERE ON

  //FIRST, UPDATE WINDING COUNTS
  if IsSamePolyType(e1, e2) then
  begin
    if FFillRule = frEvenOdd then
    begin
      e1WindCnt := e1.WindCnt;
      e1.WindCnt := e2.WindCnt;
      e2.WindCnt := e1WindCnt;
    end else
    begin
      if e1.WindCnt + e2.WindDx = 0 then
        e1.WindCnt := -e1.WindCnt else
        Inc(e1.WindCnt, e2.WindDx);
      if e2.WindCnt - e1.WindDx = 0 then
        e2.WindCnt := -e2.WindCnt else
        Dec(e2.WindCnt, e1.WindDx);
    end;
  end else
  begin
    if FFillRule <> frEvenOdd then Inc(e1.WindCnt2, e2.WindDx)
    else if e1.WindCnt2 = 0 then e1.WindCnt2 := 1
    else e1.WindCnt2 := 0;

    if FFillRule <> frEvenOdd then Dec(e2.WindCnt2, e1.WindDx)
    else if e2.WindCnt2 = 0 then e2.WindCnt2 := 1
    else e2.WindCnt2 := 0;
  end;

  case FFillRule of
    frPositive:
      begin
        e1WindCnt := e1.WindCnt;
        e2WindCnt := e2.WindCnt;
      end;
    frNegative:
      begin
        e1WindCnt := -e1.WindCnt;
        e2WindCnt := -e2.WindCnt;
      end;
    else
      begin
        e1WindCnt := abs(e1.WindCnt);
        e2WindCnt := abs(e2.WindCnt);
      end;
  end;

  if (not IsHotEdge(e1) and not (e1WindCnt in [0,1])) or
    (not IsHotEdge(e2) and not (e2WindCnt in [0,1])) then Exit;

  //NOW PROCESS THE INTERSECTION

  //if both edges are 'hot' ...
  if IsHotEdge(e1) and IsHotEdge(e2) then
  begin
    if not (e1WindCnt in [0,1]) or not (e2WindCnt in [0,1]) or
      (not IsSamePolyType(e1, e2) and (fClipType <> ctXor)) then
    begin
      op := AddLocalMaxPoly(e1, e2, pt);
      {$IFDEF USINGZ}
      if Assigned(op) then SetZ(e1, e2, op.pt);
      {$ENDIF}
    end else if IsFront(e1) or (e1.OutRec = e2.OutRec) then
    begin
      op := AddLocalMaxPoly(e1, e2, pt);
      op2 := AddLocalMinPoly(e1, e2, pt);
      {$IFDEF USINGZ}
      if Assigned(op) then SetZ(e1, e2, op.pt);
      SetZ(e1, e2, op2.pt);
      {$ENDIF}
    end else
    begin
      //right & left bounds touching, NOT maxima & minima ...
      op := AddOutPt(e1, pt);
      op2 := AddOutPt(e2, pt);
      {$IFDEF USINGZ}
      SetZ(e1, e2, op.pt);
      SetZ(e1, e2, op2.pt);
      {$ENDIF}
      SwapOutRecs(e1, e2);
    end;
  end

  //if one or other edge is 'hot' ...
  else if IsHotEdge(e1) then
  begin
    op := AddOutPt(e1, pt);
    {$IFDEF USINGZ}
    SetZ(e1, e2, op.pt);
    {$ENDIF}
    SwapOutRecs(e1, e2);
  end
  else if IsHotEdge(e2) then
  begin
    op := AddOutPt(e2, pt);
    {$IFDEF USINGZ}
    SetZ(e1, e2, op.pt);
    {$ENDIF}
    SwapOutRecs(e1, e2);
  end

  else //neither edge is 'hot'
  begin
    case FFillRule of
      frPositive:
      begin
        e1WindCnt2 := e1.WindCnt2;
        e2WindCnt2 := e2.WindCnt2;
      end;
      frNegative:
      begin
        e1WindCnt2 := -e1.WindCnt2;
        e2WindCnt2 := -e2.WindCnt2;
      end
      else
      begin
        e1WindCnt2 := abs(e1.WindCnt2);
        e2WindCnt2 := abs(e2.WindCnt2);
      end;
    end;

    if not IsSamePolyType(e1, e2) then
    begin
      op := AddLocalMinPoly(e1, e2, pt, false);
      {$IFDEF USINGZ}
      SetZ(e1, e2, op.pt);
      {$ENDIF}
    end
    else if (e1WindCnt = 1) and (e2WindCnt = 1) then
    begin
      op := nil;
      case FClipType of
        ctIntersection:
          if (e1WindCnt2 <= 0) or (e2WindCnt2 <= 0) then Exit
          else op := AddLocalMinPoly(e1, e2, pt, false);
        ctUnion:
          if (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0) then
            op := AddLocalMinPoly(e1, e2, pt, false);
        ctDifference:
          if ((GetPolyType(e1) = ptClip) and
                (e1WindCnt2 > 0) and (e2WindCnt2 > 0)) or
              ((GetPolyType(e1) = ptSubject) and
                (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0)) then
            op := AddLocalMinPoly(e1, e2, pt, false);
        else //xOr
            op := AddLocalMinPoly(e1, e2, pt, false);
      end;
      {$IFDEF USINGZ}
      if assigned(op) then SetZ(e1, e2, op.pt);
      {$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------
{$IFNDEF USINGZ}
{$HINTS ON}
{$ENDIF}

procedure TClipperBase.DeleteFromAEL(e: PActive);
var
  aelPrev, aelNext: PActive;
begin
  aelPrev := e.PrevInAEL;
  aelNext := e.NextInAEL;
  if not Assigned(aelPrev) and not Assigned(aelNext) and
    (e <> FActives) then Exit; //already deleted
  if Assigned(aelPrev) then aelPrev.NextInAEL := aelNext
  else FActives := aelNext;
  if Assigned(aelNext) then aelNext.PrevInAEL := aelPrev;
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
    e.PrevInSEL := e.PrevInAEL;
    e.NextInSEL := e.NextInAEL;
    e.Jump := e.NextInSEL;
    e.CurrX := TopX(e, topY);
    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.ExecuteInternal(clipType: TClipType;
  fillRule: TFillRule);
var
  Y: Int64;
  e: PActive;
begin
  if clipType = ctNone then Exit;
  FFillRule := fillRule;
  FClipType := clipType;
  Reset;
  if not PopScanLine(Y) then Exit;
  while true do
  begin
    InsertLocalMinimaIntoAEL(Y);
    while PopHorz(e) do DoHorizontal(e);
    FBotY := Y;                       //FBotY == bottom of current scanbeam
    if not PopScanLine(Y) then Break; //Y     == top of current scanbeam
    DoIntersections(Y);
    DoTopOfScanbeam(Y);
    while PopHorz(e) do DoHorizontal(e);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.BuildTree(polytree: TPolyPathBase; out openPaths: TPaths64);
var
  i,j         : Integer;
  cntOpen     : Integer;
  outRec      : POutRec;
  path        : TPath64;
  isOpenPath  : Boolean;
  ownerPP     : TPolyPathBase;
begin
  try
    polytree.Clear;
    setLength(openPaths, OutRecList.Count);
    cntOpen := 0;
    for i := 0 to OutRecList.Count -1 do
      if Assigned(OutRecList[i]) then
      begin
        outRec := OutRecList[i];
        //make sure outer/owner paths preceed their inner paths ...
        if assigned(outRec.Owner) and (outRec.Owner.Idx > outRec.Idx) then
        begin
          j := outRec.Owner.Idx;
          outRec.idx := j;
          OutRecList[i] := OutRecList[j];
          OutRecList[j] := outRec;
          outRec := OutRecList[i];
          outRec.Idx := i;
        end;

        if not assigned(outRec.Pts) then Continue;
        isOpenPath := IsOpen(outRec);

        if not BuildPath(outRec.Pts.Next, isOpenPath, path) then
          Continue;

        if isOpenPath then
        begin
          openPaths[cntOpen] := path;
          inc(cntOpen);
          Continue;
        end;
        //update ownership ...
        while assigned(outRec.Owner) and not assigned(outRec.Owner.Pts) do
          outRec.Owner := outRec.Owner.Owner;
        if assigned(outRec.Owner) and (outRec.Owner.State = outRec.State) then
        begin
          if IsOuter(outRec) then outRec.Owner := nil
          else outRec.Owner := outRec.Owner.Owner;
        end;

        if assigned(outRec.Owner) and assigned(outRec.Owner.PolyPath) then
          ownerPP := outRec.Owner.PolyPath else
          ownerPP := polytree;

        outRec.PolyPath := ownerPP.AddChild(path);
      end;
    setLength(openPaths, cntOpen);
  except
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
    Dispose(IntersectNode[i]);
  FIntersectList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddNewIntersectNode(e1, e2: PActive; topY: Int64);
var
  pt: TPoint64;
  node: PIntersectNode;
begin
  pt := GetIntersectPoint(e1, e2);
  //Rounding errors can occasionally place the calculated intersection
  //point either below or above the scanbeam, so check and correct ...
  if (pt.Y > FBotY) then
  begin
    //E.Curr.Y is still at the bottom of scanbeam here
    pt.Y := FBotY;
    //use the more vertical of the 2 edges to derive pt.X ...
    if (abs(e1.Dx) < abs(e2.Dx)) then
      pt.X := TopX(e1, FBotY) else
      pt.X := TopX(e2, FBotY);
  end
  else if pt.Y < topY then
  begin
    //TopY = top of scanbeam
    pt.Y := topY;
    if e1.Top.Y = topY then
      pt.X := e1.Top.X
    else if e2.Top.Y = topY then
      pt.X := e2.Top.X
    else if (abs(e1.Dx) < abs(e2.Dx)) then
      pt.X := e1.CurrX
    else
      pt.X := e2.CurrX;
  end;

  new(node);
  node.Edge1 := e1;
  node.Edge2 := e2;
  node.Pt := pt;
  FIntersectList.Add(node);
end;
//------------------------------------------------------------------------------

function ExtractFromSEL(edge: PActive): PActive;
begin
  Result := edge.NextInSEL;
  if Assigned(Result) then
    Result.PrevInSEL := edge.PrevInSEL;
  if Assigned(edge.PrevInSEL) then
    edge.PrevInSEL.NextInSEL := Result;
end;
//------------------------------------------------------------------------------

procedure Insert1Before2InSEL(edge1, edge2: PActive);
begin
  edge1.PrevInSEL := edge2.PrevInSEL;
  if Assigned(edge1.PrevInSEL) then
    edge1.PrevInSEL.NextInSEL := edge1;
  edge1.NextInSEL := edge2;
  edge2.PrevInSEL := edge1;
end;
//------------------------------------------------------------------------------

function TClipperBase.BuildIntersectList(const topY: Int64): Boolean;
var
  q, base,prevBase,left,right, lend, rend: PActive;
begin
  result := false;
  if not Assigned(FActives) or not Assigned(FActives.NextInAEL) then Exit;

  //Calculate edge positions at the top of the current scanbeam, and from this
  //we will determine the intersections required to reach these new positions.
  AdjustCurrXAndCopyToSEL(topY);

  //Find all edge intersections in the current scanbeam using a stable merge
  //sort that ensures only adjacent edges are intersecting. Intersect info is
  //stored in FIntersectList ready to be processed in ProcessIntersectList.
  left := FSel;
  while Assigned(left.jump) do
  begin
    prevBase := nil;
    while Assigned(left) and Assigned(left.jump) do
    begin
      base := left;
      right := left.Jump;
      rend  := right.Jump;
      left.jump := rend;
      lend := right; rend := right.jump;
      while (left <> lend) and (right <> rend) do
      begin
        if right.CurrX < left.CurrX then
        begin

          //save edge intersections
          q := right.PrevInSEL;
          while true do
          begin
            AddNewIntersectNode(q, right, topY);
            if q = left then Break;
            q := q.PrevInSEL;
          end;

          //now move the out of place edge on the right
          //to its new ordered place on the left.
          q := right;
          right := ExtractFromSEL(q); //ie returns the new right
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
          left := left.NextInSEL;
      end;
      prevBase := base;
      left := rend;
    end;
    left := FSel;
  end;
  result := FIntersectList.Count > 0;
end;
//------------------------------------------------------------------------------

function TClipperBase.GetIntersectNode(index: integer): PIntersectNode;
begin
  result := PIntersectNode(FIntersectList[index]);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.ProcessIntersectList;
var
  i, j, highI: Integer;
  node: PIntersectNode;
begin
  //We now have a list of intersections required so that edges will be
  //correctly positioned at the top of the scanbeam. However, it's important
  //that edge intersections are processed from the bottom up, but it's also
  //crucial that intersections only occur between adjacent edges.

  //First we do a quicksort so intersections proceed in a bottom up order ...
  FIntersectList.Sort(IntersectListSort);

  //Now as we process these intersections, we must sometimes adjust the order
  //to ensure that intersecting edges are always adjacent ...
  highI := FIntersectList.Count - 1;
  for i := 0 to highI do
  begin
    if not EdgesAdjacentInAEL(FIntersectList[i]) then
    begin
      j := i + 1;
      while not EdgesAdjacentInAEL(FIntersectList[j]) do inc(j);
      //Swap IntersectNodes ...
      node := FIntersectList[i];
      FIntersectList[i] := FIntersectList[j];
      FIntersectList[j] := node;
    end;

    with IntersectNode[i]^ do
    begin
      IntersectEdges(Edge1, Edge2, Pt);
      SwapPositionsInAEL(Edge1, Edge2);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SwapPositionsInAEL(e1, e2: PActive);
var
  prev, next: PActive;
begin
  //preconditon: e1 must be immediately to the left of e2
  next := e2.NextInAEL;
  if Assigned(next) then next.PrevInAEL := e1;
  prev := e1.PrevInAEL;
  if Assigned(prev) then prev.NextInAEL := e2;
  e2.PrevInAEL := prev;
  e2.NextInAEL := e1;
  e1.PrevInAEL := e2;
  e1.NextInAEL := next;
  if not Assigned(e2.PrevInAEL) then FActives := e2;
end;
//------------------------------------------------------------------------------

function TrimHorz(horzEdge: PActive; preserveCollinear: Boolean): Boolean;
var
  pt: TPoint64;
begin
  Result := false;
  pt := NextVertex(horzEdge).Pt;
  //trim 180 deg. spikes in closed paths
  while ((pt.Y = horzEdge.top.Y) and
    (not preserveCollinear or
    ((pt.X < horzEdge.top.X) = (horzEdge.bot.X < horzEdge.top.X)))) do
  begin
    horzEdge.VertTop := NextVertex(horzEdge);
    horzEdge.top := pt;
    Result := true;
    if IsMaxima(horzEdge) then Break;
    pt := NextVertex(horzEdge).Pt;
  end;
  if (Result) then SetDx(horzEdge); // +/-infinity
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DoHorizontal(horzEdge: PActive);
var
  maxPair: PActive;
  horzLeft, horzRight: Int64;
  isLeftToRight: Boolean;

  function ResetHorzDirection: Boolean;
  var
    e: PActive;
  begin
    if (horzEdge.Bot.X = horzEdge.Top.X) then
    begin
      //the horizontal edge is going nowhere ...
      horzLeft := horzEdge.CurrX;
      horzRight := horzEdge.CurrX;
      e := horzEdge.NextInAEL;
      while assigned(e) and (e <> maxPair) do
        e := e.NextInAEL;
      Result := assigned(e);
      //nb: this block isn't yet redundant
    end
    else if horzEdge.CurrX < horzEdge.Top.X then
    begin
      horzLeft := horzEdge.CurrX;
      horzRight := horzEdge.Top.X;
      Result := true;
    end else
    begin
      horzLeft := horzEdge.Top.X;
      horzRight := horzEdge.CurrX;
      Result := false;
    end;
  end;
  //------------------------------------------------------------------------

var
  e: PActive;
  pt: TPoint64;
  isMax, horzIsOpen: Boolean;
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

  maxPair := nil;
  isMax := IsMaxima(horzEdge);

  //remove 180 deg.spikes and also with closed paths and not PreserveCollinear
  //simplify consecutive horizontals into a 'single' edge ...
  if not horzIsOpen and
    not isMax and TrimHorz(horzEdge, FPreserveCollinear) then
      isMax := IsMaxima(horzEdge);

  if isMax and not IsOpenEnd(horzEdge) then
    maxPair := GetMaximaPair(horzEdge);

  isLeftToRight := ResetHorzDirection;
  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, Point64(horzEdge.CurrX, horzEdge.Bot.Y));

  while true do //loop through consec. horizontal edges
  begin
    if isLeftToRight  then
      e := horzEdge.NextInAEL else
      e := horzEdge.PrevInAEL;

    while assigned(e) do
    begin
      if (e = maxPair) then
      begin
        if IsHotEdge(horzEdge)  then
        begin
          if isLeftToRight then
            AddLocalMaxPoly(horzEdge, e, horzEdge.Top) else
            AddLocalMaxPoly(e, horzEdge, horzEdge.Top);
        end;
        DeleteFromAEL(e);
        DeleteFromAEL(horzEdge);
        Exit;
      end;

      //if horzEdge is a maxima, keep going until we reach
      //its maxima pair, otherwise check for Break conditions
      if not isMax or IsOpenEnd(horzEdge) then
      begin
        //otherwise stop when 'e' is beyond the end of the horizontal line
        if (isLeftToRight and (e.CurrX > horzRight)) or
          (not isLeftToRight and (e.CurrX < horzLeft)) then Break;

        if (e.CurrX = horzEdge.Top.X) and not IsHorizontal(e) then
        begin
          //for edges at the end or horzEdge, keep going until horzEdge's
          //outslope is greater than e's slope when heading right or until
          //horzEdge's outslope is less than e's slope when heading left.
          pt := NextVertex(horzEdge).Pt;
          if (isLeftToRight and (TopX(E, pt.Y) >= pt.X)) or
            (not isLeftToRight and (TopX(E, pt.Y) <= pt.X)) then Break;
        end;
      end;

      pt := Point64(e.CurrX, horzEdge.Bot.Y);
      if (isLeftToRight) then
      begin
        IntersectEdges(horzEdge, e, pt);
        SwapPositionsInAEL(horzEdge, e);
        e := horzEdge.NextInAEL;
      end else
      begin
        IntersectEdges(e, horzEdge, pt);
        SwapPositionsInAEL(e, horzEdge);
        e := horzEdge.PrevInAEL;
      end;
    end;

    //check if we've finished looping through consecutive horizontals
    if isMax or (NextVertex(horzEdge).Pt.Y <> horzEdge.Top.Y) then
      Break;

    UpdateEdgeIntoAEL(horzEdge);
    isMax := IsMaxima(horzEdge);

    if not horzIsOpen and not isMax and
      TrimHorz(horzEdge, FPreserveCollinear) then
        isMax := IsMaxima(horzEdge); //ie update after TrimHorz

    isLeftToRight := ResetHorzDirection;
    if isMax then maxPair := GetMaximaPair(horzEdge);
    if IsHotEdge(horzEdge) then AddOutPt(horzEdge, horzEdge.Bot);

  end; //end while horizontal

  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, horzEdge.Top);

  if not IsOpen(horzEdge) then
    UpdateEdgeIntoAEL(horzEdge) //this is the end of an intermediate horiz.
  else if not isMax then
    UpdateEdgeIntoAEL(horzEdge)
  else if not assigned(maxPair) then //ie open at top
    DeleteFromAEL(horzEdge)
  else if IsHotEdge(horzEdge) then
      AddLocalMaxPoly(horzEdge, maxPair, horzEdge.Top)
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
  FSel := nil; //FSel is reused to flag horizontals (see PushHorz below)
  e := FActives;
  while Assigned(e) do
  begin
    //nb: 'e' will never be horizontal here
    if (e.Top.Y = Y) then
    begin
      e.CurrX := e.Top.X;
      if IsMaxima(e) then
      begin
        e := DoMaxima(e);  //TOP OF BOUND (MAXIMA)
        Continue;
      end else
      begin
        //INTERMEDIATE VERTEX ...
        UpdateEdgeIntoAEL(e);
        if IsHotEdge(e) then AddOutPt(e, e.Bot);
        if IsHorizontal(e) then
          PushHorz(e);     //horizontals are processed later
      end;
    end else
      e.CurrX := TopX(e, Y);
    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipperBase.DoMaxima(e: PActive): PActive;
var
  eNext, ePrev, eMaxPair: PActive;
begin
  ePrev := e.PrevInAEL;
  eNext := e.NextInAEL;
  Result := eNext;

  if IsOpenEnd(e) then
  begin
    if IsHotEdge(e) then AddOutPt(e, e.Top);
    if not IsHorizontal(e) then
    begin
      if IsHotEdge(e) then e.OutRec := nil;
      DeleteFromAEL(e);
    end;
    Exit;
  end else
  begin
    eMaxPair := GetMaximaPair(e);
    if not assigned(eMaxPair) then Exit; //EMaxPair is a horizontal ...
  end;

  //only non-horizontal maxima here.
  //process any edges between maxima pair ...
  while (eNext <> eMaxPair) do
  begin
    IntersectEdges(e, eNext, e.Top);
    SwapPositionsInAEL(e, eNext);
    eNext := e.NextInAEL;
  end;

  if IsOpen(e) then
  begin
    if IsHotEdge(e) then
    begin
      if assigned(eMaxPair) then
        AddLocalMaxPoly(e, eMaxPair, e.Top) else
        AddOutPt(e, e.Top);
    end;
    if assigned(eMaxPair) then
      DeleteFromAEL(eMaxPair);
    DeleteFromAEL(e);

    if assigned(ePrev) then
      Result := ePrev.NextInAEL else
      Result := FActives;
    Exit;
  end;

  //here E.NextInAEL == ENext == EMaxPair ...
  if IsHotEdge(e) then
    AddLocalMaxPoly(e, eMaxPair, e.Top);

  DeleteFromAEL(e);
  DeleteFromAEL(eMaxPair);
  if assigned(ePrev) then
    Result := ePrev.NextInAEL else
    Result := FActives;
end;
//------------------------------------------------------------------------------

function TClipperBase.BuildPaths(out closedPaths, openPaths: TPaths64): Boolean;
var
  i, cntClosed, cntOpen: Integer;
  outRec: POutRec;
  extras, extras2: TPaths64;
begin
  try
    extras := nil;
    cntClosed := 0; cntOpen := 0;
    SetLength(closedPaths, FOutRecList.Count);
    SetLength(openPaths, FOutRecList.Count);
    for i := 0 to FOutRecList.Count -1 do
    begin
      outRec := FOutRecList[i];
      if not assigned(outRec.Pts) then Continue;

      if IsOpen(outRec) then
      begin
        if BuildPath(outRec.Pts.Next,
          true, openPaths[cntOpen]) then
            inc(cntOpen);
      end else
      begin
        if BuildPath(outRec.Pts.Next,
          false, closedPaths[cntClosed]) then
            inc(cntClosed);
      end;
    end;
    SetLength(closedPaths, cntClosed);
    if Assigned(extras2) then
      AppendPaths(closedPaths, extras2);
    SetLength(openPaths, cntOpen);
    result := true;
  except
    result := false;
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
    vStart := FVertexArrayList[i];
    v := vStart;
    repeat
      if v.Pt.X < Result.Left then Result.Left := v.Pt.X
      else if v.Pt.X > Result.Right then Result.Right := v.Pt.X;
      if v.Pt.Y < Result.Top then Result.Top := v.Pt.Y
      else if v.Pt.Y > Result.Bottom then Result.Bottom := v.Pt.Y;
      v := v.Next;
    until v = vStart;
  end;
  if Result.Left > Result.Right then Result := NullRect64;
end;

//------------------------------------------------------------------------------
// TClipper methods
//------------------------------------------------------------------------------

procedure TClipper.AddSubject(const subject: TPath64);
begin
  AddPath(subject, ptSubject, false);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddSubject(const subjects: TPaths64);
begin
  AddPaths(subjects, ptSubject, false);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddOpenSubject(const subject: TPath64);
begin
  AddPath(subject, ptSubject, true);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddOpenSubject(const subjects: TPaths64);
begin
  AddPaths(subjects, ptSubject, true);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddClip(const clip: TPath64);
begin
  AddPath(clip, ptClip, false);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddClip(const clips: TPaths64);
begin
  AddPaths(clips, ptClip, false);
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType;
  fillRule: TFillRule; out closedSolutions: TPaths64): Boolean;
var
  dummy: TPaths64;
begin
  Result := true;
  closedSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildPaths(closedSolutions, dummy);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedSolutions, openSolutions: TPaths64): Boolean;
begin
  Result := true;
  closedSolutions := nil;
  openSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildPaths(closedSolutions, openSolutions);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule;
  var solutionTree: TPolyTree; out openSolutions: TPaths64): Boolean;
begin
  if not assigned(solutionTree) then RaiseError(rsClipper_PolyTreeErr);
  solutionTree.Clear;
  openSolutions := nil;
  Result := true;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildTree(solutionTree, openSolutions);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;

//------------------------------------------------------------------------------
//  TPolyPathBase methods
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

procedure TPolyPathBase.Clear;
var
  i: integer;
begin
  for i := 0 to FChildList.Count -1 do
    TPolyPathBase(FChildList[i]).Free;
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
  result := true;
  pp := FParent;
  while assigned(pp) do
  begin
    result := not result;
    pp := pp.FParent;
  end;
//  Result := not assigned(FParent) or not FParent.GetIsHole;
end;
//------------------------------------------------------------------------------

function  TPolyPathBase.GetChildCnt: Integer;
begin
  Result := FChildList.Count;
end;

//------------------------------------------------------------------------------
// TPolyPath method
//------------------------------------------------------------------------------

function TPolyPath.AddChild(const path: TPath64): TPolyPathBase;
begin
  Result := TPolyPath.Create;
  Result.Parent := self;
  TPolyPath(Result).FPath := path;;
  ChildList.Add(Result);
end;

//------------------------------------------------------------------------------
//  TClipperD methods
//------------------------------------------------------------------------------

constructor TClipperD.Create(scale: double);
begin
  inherited Create;
  if scale <= floatingPointTolerance then
    FScale := DefaultClipperDScale else
    FScale := scale;
  FInvScale := 1/FScale;
end;
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
procedure TClipperD.ProxyZFillFunc(const bot1, top1, bot2, top2: TPoint64;
  var intersectPt: TPoint64);
var
  tmp: TPointD;
begin
  //de-scale coordinates
  tmp := ScalePoint(intersectPt, FInvScale);
  FZFuncD(
    ScalePoint(bot1, FInvScale),
    ScalePoint(top1, FInvScale),
    ScalePoint(bot2, FInvScale),
    ScalePoint(top2, FInvScale), tmp);
  //re-scale
  intersectPt.Z := Round(tmp.Z * FScale);
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

{$IFDEF USINGZ}
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
  closedSolutions := nil;
  openSolutions := nil;
  try try
    if Assigned(ZFillFunc) then
      inherited ZFillFunc := ProxyZFillFunc else
      inherited ZFillFunc := nil;
    ExecuteInternal(clipType, fillRule);
    Result := BuildPaths(solClosed, solOpen);
    if not Result then Exit;
    closedSolutions := ScalePathsD(solClosed, FInvScale);
    openSolutions := ScalePathsD(solOpen, FInvScale);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  var solutionsTree: TPolyTreeD; out openSolutions: TPathsD): Boolean;
var
  open_Paths: TPaths64;
begin
  if not assigned(solutionsTree) then RaiseError(rsClipper_PolyTreeErr);
  solutionsTree.Clear;
  solutionsTree.SetScale(fScale);
  openSolutions := nil;
  try try
    if Assigned(ZFillFunc) then
      inherited ZFillFunc := ProxyZFillFunc else
      inherited ZFillFunc := nil;
    ExecuteInternal(clipType, fillRule);
    BuildTree(solutionsTree, open_Paths);
    openSolutions := ScalePathsD(open_Paths, FInvScale);
    Result := true;
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------
{$ELSE}

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedSolutions: TPathsD): Boolean;
var
  dummyP: TPathsD;
begin
  Result := Execute(clipType, fillRule, closedSolutions, dummyP);
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedSolutions, openSolutions: TPathsD): Boolean;
var
  closedP, openP: TPaths64;
begin
  closedSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    Result := BuildPaths(closedP, openP);
    if not Result then Exit;
    closedSolutions := ScalePathsD(closedP, FInvScale);
    openSolutions := ScalePathsD(openP, FInvScale);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  var solutionsTree: TPolyTreeD; out openSolutions: TPathsD): Boolean;
var
  open_Paths: TPaths64;
begin
  if not assigned(solutionsTree) then RaiseError(rsClipper_PolyTreeErr);
  solutionsTree.Clear;
  solutionsTree.SetScale(fScale);
  openSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildTree(solutionsTree, open_Paths);
    openSolutions := ScalePathsD(open_Paths, FInvScale);
    Result := true;
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
{$ENDIF}

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
// TPolyTreeD methods
//------------------------------------------------------------------------------

procedure TPolyTreeD.SetScale(value: double);
begin
  FScale := value;
end;

end.

