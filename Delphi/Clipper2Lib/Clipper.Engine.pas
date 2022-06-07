unit Clipper.Engine;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  7 June 2022                                                     *
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

  //Vertex: a pre-clipping data structure. It is used to separate polygons
  //into ascending and descending 'bounds' (or sides) that start at local
  //minima and ascend to a local maxima, before descending again.

  TVertexFlag = (vfOpenStart, vfOpenEnd, vfLocMax, vfLocMin);
  TVertexFlags = set of TVertexFlag;

  PVertex = ^TVertex;
  TVertex = record
    Pt    : TPoint64;
    Next  : PVertex;
    Prev  : PVertex;
    Flags : TVertexFlags;
  end;

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    Vertex    : PVertex;
    PolyType  : TPathType;
    IsOpen    : Boolean;
  end;

  //forward declarations
  POutRec = ^TOutRec;
  PJoiner = ^TJoiner;
  PActive = ^TActive;
  TPolyPathBase = class;
  TPolyTree     = class;
  TPolyTreeD    = class;

  //OutPt: vertex data structure for clipping solutions
  POutPt = ^TOutPt;
  TOutPt = record
    Pt       : TPoint64;
    Next     : POutPt;
    Prev     : POutPt;
    OutRec   : POutRec;
    Joiner   : PJoiner;
  end;

  TOutRecState = (osUndefined, osOpen, osOuter, osInner);
  TOutRecArray = array of POutRec;

  //OutRec: path data structure for clipping solutions
  TOutRec = record
    Idx      : Integer;
    Owner    : POutRec;
    Split    : TOutRecArray;
    FrontE   : PActive;
    BackE    : PActive;
    Pts      : POutPt;
    PolyPath : TPolyPathBase;
    State    : TOutRecState;
  end;

  //Joiner: structure used in merging "touching" solution polygons
  TJoiner = record
    idx   : integer;
    op1   : POutPt;
    op2   : POutPt;
    next1 : PJoiner;
    next2 : PJoiner;
    nextH : PJoiner;
  end;

  //Active: represents an edge in the Active Edge Table (Vatti's AET)
  TActive = record
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
    LeftBound : Boolean;
  end;

  //IntersectNode: a structure representing 2 intersecting edges.
  //Intersections must be sorted so they are processed from the largest
  //Y coordinates to the smallest while keeping edges adjacent.
  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
      Edge1  : PActive;
      Edge2  : PActive;
      Pt     : TPoint64;
  end;

  //Scanline: a virtual line representing current position
  //while processing edges using a "sweep line" algorithm.
  PScanLine = ^TScanLine;
  TScanLine = record
    Y     : Int64;
    Next  : PScanLine;
  end;

  {$IFDEF USINGZ}
  TZCallback64 = procedure (const bot1, top1, bot2, top2: TPoint64;
    var intersectPt: TPoint64) of object;
  TZCallbackD = procedure (const bot1, top1, bot2, top2: TPointD;
    var intersectPt: TPointD) of object;
  {$ENDIF}

  //ClipperBase: abstract base of Clipper class
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
    //FActives: see AEL above
    FActives            : PActive;
    //FSel: see SEL above.
    //      BUT also used to store horz. edges for later processing
    FSel                : PActive;
    FHorzTrials         : PJoiner;
    FHasOpenPaths       : Boolean;
    FLocMinListSorted   : Boolean;
  {$IFDEF USINGZ}
    FZFunc              : TZCallback64;
  {$ENDIF}
    procedure Reset;
    procedure InsertScanLine(const Y: Int64);
    function  PopScanLine(out Y: Int64): Boolean;
    function  PopLocalMinima(Y: Int64;
      out localMinima: PLocalMinima): Boolean;
    procedure DisposeScanLineList;
    procedure DisposeOutRecsAndJoiners;
    procedure DisposeVerticesAndLocalMinima;
    procedure AddPathsToVertexList(const paths: TPaths64;
      polyType: TPathType; isOpen: Boolean);
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
    function  FixSides(e1, e2: PActive): Boolean;
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
    procedure SafeDisposeOutPts(op: POutPt);
    procedure CleanCollinear(outRec: POutRec);
    procedure FixSelfIntersects(var op: POutPt);
  protected
    procedure AddPath(const path: TPath64;
      pathType: TPathType; isOpen: Boolean);
    procedure AddPaths(const paths: TPaths64;
      pathType: TPathType; isOpen: Boolean);
    procedure ClearSolution; //unlike Clear, CleanUp preserves added paths
    procedure ExecuteInternal(clipType: TClipType; fillRule: TFillRule);
    function  BuildPaths(out closedPaths, openPaths: TPaths64): Boolean;
    procedure BuildTree(polytree: TPolyPathBase; out openPaths: TPaths64);
  {$IFDEF USINGZ}
    procedure SetZ( e1, e2: PActive; var intersectPt: TPoint64);
    property  OnZFill : TZCallback64 read FZFunc write FZFunc;
  {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function GetBounds: TRect64;
    property PreserveCollinear: Boolean read
      FPreserveCollinear write FPreserveCollinear;
  end;

  TClipper64 = class(TClipperBase) //for integer coordinates
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
      var solutionTree: TPolyTree; out openSolutions: TPaths64): Boolean; overload; virtual;
  {$IFDEF USINGZ}
    property  ZFillFunc;
  {$ENDIF}
  end;

  //PolyPathBase: ancestor of TPolyPath and TPolyPathD
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

  //PolyTree: is intended as a READ-ONLY data structure to receive closed path
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
    constructor Create(roundingDecimalPrecision: integer = 2); reintroduce; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions: TPathsD): Boolean; overload;
    function  Execute(clipType: TClipType; fillRule: TFillRule;
      out closedSolutions, openSolutions: TPathsD): Boolean; overload;
    function  Execute(clipType: TClipType; fillRule: TFillRule;
      var solutionsTree: TPolyTreeD; out openSolutions: TPathsD): Boolean; overload;
{$IFDEF USINGZ}
    procedure ProxyZFillFunc(const bot1, top1, bot2, top2: TPoint64;
      var intersectPt: TPoint64);
    property  ZFillFunc : TZCallbackD read FZFuncD write FZFuncD;
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
  rsClipper_PolyTreeErr = 'The TPolyTree parameter must be assigned.';
  rsClipper_ClippingErr = 'Undefined clipping error';
  rsClipper_RoundingErr = 'The decimal rounding value is invalid';

const
  DefaultClipperDScale = 100;

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
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

function IsValidPath(op: POutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := assigned(op) and (op.Next <> op);
end;
//------------------------------------------------------------------------------

function IsValidClosedPath(op: POutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := assigned(op) and (op.Next <> op) and (op.Next <> op.Prev);
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
  else if (currentY = e.Bot.Y) then Result := e.Bot.X
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
  Result := e.LeftBound;
end;
//------------------------------------------------------------------------------

function NextVertex(e: PActive): PVertex;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
{$IFDEF REVERSE_ORIENTATION}
  if e.WindDx > 0 then
{$ELSE}
  if e.WindDx < 0 then
{$ENDIF}
    Result := e.vertTop.Next else
    Result := e.vertTop.Prev;
end;
//------------------------------------------------------------------------------

//PrevPrevVertex: useful to get the top of the alternate edge
//(ie left or right bound) during edge insertion.
function PrevPrevVertex(e: PActive): PVertex;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
{$IFDEF REVERSE_ORIENTATION}
  if e.WindDx > 0 then
{$ELSE}
  if e.WindDx < 0 then
{$ENDIF}
    Result := e.vertTop.Prev.Prev else
    Result := e.vertTop.Next.Next;
end;
//------------------------------------------------------------------------------

function IsMaxima(vertex: PVertex): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := vfLocMax in vertex.Flags;
end;
//------------------------------------------------------------------------------

function IsMaxima(e: PActive): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := vfLocMax in e.vertTop.Flags;
end;
//------------------------------------------------------------------------------

function GetCurrYMaximaVertex(e: PActive): PVertex;
begin
  //nb: function not safe with open paths
  Result := e.VertTop;
{$IFDEF REVERSE_ORIENTATION}
  if e.WindDx > 0 then
{$ELSE}
  if e.WindDx < 0 then
{$ENDIF}
    while Result.Next.Pt.Y = Result.Pt.Y do  Result := Result.Next
  else
    while Result.Prev.Pt.Y = Result.Pt.Y do  Result := Result.Prev;
  if not IsMaxima(Result) then Result := nil; //not a maxima
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PActive): PActive;
begin
  Result := e.NextInAEL;
  while assigned(Result) do
  begin
    if Result.vertTop = e.vertTop then Exit;  //Found!
    Result := Result.NextInAEL;
  end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function GetHorzMaximaPair(horz: PActive; maxVert: PVertex): PActive;
begin
  //we can't be sure whether the MaximaPair is on the left or right, so ...
  Result := horz.PrevInAEL;
  while assigned(Result) and (Result.CurrX >= maxVert.Pt.X) do
  begin
    if Result.vertTop = maxVert then Exit;  //Found!
    Result := Result.PrevInAEL;
  end;
  Result := horz.NextInAEL;
  while assigned(Result) and (TopX(Result, horz.Top.Y) <= maxVert.Pt.X) do
  begin
    if Result.vertTop = maxVert then Exit;  //Found!
    Result := Result.NextInAEL;
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

function GetRealOutRec(outRec: POutRec): POutRec;
 {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outRec;
  while Assigned(Result) and not Assigned(Result.Pts) do
    Result := Result.Owner;
end;
//------------------------------------------------------------------------------

procedure UncoupleOutRec(e: PActive);
var
  outRec: POutRec;
begin
  if not Assigned(e.OutRec) then Exit;
  outRec := e.OutRec;
  outRec.FrontE.OutRec := nil;
  outRec.BackE.OutRec := nil;
  outRec.FrontE := nil;
  outRec.BackE := nil;
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
    path[0] := op.Pt;
    op := op.Prev;
  end else
  begin
    op := op.Next;
    path[0] := op.Pt;
    op := op.Next;
  end;
  j := 0;
  for i := 0 to cnt -2 do
  begin
    if not PointsEqual(path[j], op.Pt) then
    begin
      inc(j);
      path[j] := op.Pt;
    end;
    if reverse then op := op.Prev else op := op.Next;
  end;

  setLength(path, j+1);
  if isOpen then
    Result := (j > 0) else
    Result := (j > 1);
end;
//------------------------------------------------------------------------------

function DisposeOutPt(op: POutPt): POutPt;
begin
  if op.Next = op then
    Result := nil else
    Result := op.Next;
  op.Prev.Next := op.Next;
  op.Next.Prev := op.Prev;
  Dispose(Op);
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SafeDisposeOutPts(op: POutPt);
var
  tmpOp: POutPt;
  outRec: POutRec;
begin
  outRec := GetRealOutRec(op.OutRec);
  if Assigned(outRec.FrontE) then
    outRec.FrontE.OutRec := nil;
  if Assigned(outRec.BackE) then
    outRec.BackE.OutRec := nil;
  outRec.Pts := nil;

  op.Prev.Next := nil;
  while Assigned(op) do
  begin
    SafeDeleteOutPtJoiners(op);
    tmpOp := op;
    op := op.Next;
    Dispose(tmpOp);
  end;
end;
//------------------------------------------------------------------------------

procedure DisposeOutPts(op: POutPt); {$IFDEF INLINING} inline; {$ENDIF}
var
  tmpPp: POutPt;
begin
  op.Prev.Next := nil;
  while Assigned(op) do
  begin
    tmpPp := op;
    op := op.Next;
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
  q := lm2.Vertex.Pt.Y - lm1.Vertex.Pt.Y;
  if q < 0 then
    Result := -1
  else if q > 0 then
    Result := 1
  else
  begin
    q := lm2.Vertex.Pt.X - lm1.Vertex.Pt.X;
    if q < 0 then Result := 1
    else if q > 0 then Result := -1
    else Result := 0;
  end;
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
    //nb: at least one edge is 'hot'
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

function Area(op: POutPt): Double;
var
  op2: POutPt;
  d: double;
begin
  //https://en.wikipedia.org/wiki/Shoelace_formula
  Result := 0;
  if not Assigned(op) then Exit;
  op2 := op;
  repeat
    d := (op2.Prev.Pt.Y + op2.Pt.Y);
    Result := Result + d * (op2.Prev.Pt.X - op2.Pt.X);
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

function CheckFixInnerOuter(e: PActive): Boolean;
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

  Result := isOuter <> wasOuter;
  if not Result then Exit;

  if isOuter then SetAsOuter(e.outrec)
  else SetAsInner(e.outrec);

  //now check and fix ownership
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

procedure SwapFrontBackSides(outRec: POutRec); {$IFDEF INLINING} inline; {$ENDIF}
var
  e2: PActive;
begin
  //this proc. is almost never needed
  e2 := outRec.FrontE;
  outRec.FrontE := outRec.BackE;
  outRec.BackE := e2;
  outRec.Pts := outRec.Pts.Next;
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
  i: Int64;
begin
  //note to self - can't return int64 values :)
  i := i2.Pt.Y - i1.Pt.Y;
  if (i = 0) then
  begin
    if (i1 = i2) then
    begin
      Result := 0;
      Exit;
    end;
    //Sort by X too. Not essential, but it significantly
    //speeds up the secondary sort in ProcessIntersectList .
    i := i1.Pt.X - i2.Pt.X;
  end;

  if i > 0 then Result := 1
  else if i < 0 then Result := -1
  else result := 0;
end;
//------------------------------------------------------------------------------

function TestJoinWithPrev1(e: PActive; currY: int64): Boolean;
begin
  //this is marginally quicker than TestJoinWithPrev2
  //but can only be used when e.PrevInAEL.currX is accurate
  Result := IsHotEdge(e) and not IsOpen(e) and
    Assigned(e.PrevInAEL) and (e.PrevInAEL.CurrX = e.CurrX) and
    IsHotEdge(e.PrevInAEL) and not IsOpen(e.PrevInAEL) and
    (currY - e.Top.Y > 1) and (currY - e.PrevInAEL.Top.Y > 1) and
    (CrossProduct(e.PrevInAEL.Top, e.Bot, e.Top) = 0);
end;
//------------------------------------------------------------------------------

function TestJoinWithPrev2(e: PActive; const currPt: TPoint64): Boolean;
begin
  Result := IsHotEdge(e) and not IsOpen(e) and
    Assigned(e.PrevInAEL) and not IsOpen(e.PrevInAEL) and
    IsHotEdge(e.PrevInAEL) and
    (Abs(TopX(e.PrevInAEL, currPt.Y) - currPt.X) < 2) and
    (e.PrevInAEL.Top.Y < currPt.Y) and
    (CrossProduct(e.PrevInAEL.Top, currPt, e.Top) = 0);
end;
//------------------------------------------------------------------------------

function TestJoinWithNext1(e: PActive; currY: Int64): Boolean;
begin
  //this is marginally quicker than TestJoinWithNext2
  //but can only be used when e.NextInAEL.currX is accurate
  Result := IsHotEdge(e) and Assigned(e.NextInAEL) and
    IsHotEdge(e.NextInAEL) and not IsOpen(e) and
    not IsOpen(e.NextInAEL) and
    (currY - e.Top.Y > 1) and (currY - e.NextInAEL.Top.Y > 1) and
    (e.NextInAEL.CurrX = e.currX) and
    (CrossProduct(e.NextInAEL.Top, e.Bot, e.Top) = 0);
end;
//------------------------------------------------------------------------------

function TestJoinWithNext2(e: PActive; const currPt: TPoint64): Boolean;
begin
  Result := IsHotEdge(e) and Assigned(e.NextInAEL) and
    IsHotEdge(e.NextInAEL) and not IsOpen(e) and
    not IsOpen(e.NextInAEL) and
    (Abs(TopX(e.NextInAEL, currPt.Y) - currPt.X) < 2) and                   //safer
    (e.NextInAEL.Top.Y < currPt.Y) and
    (CrossProduct(e.NextInAEL.Top, currPt, e.Top) = 0);
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
  Result.next1 := horz.Joiner;
  horz.Joiner := Result;
  Result.next2 := nil;
  Result.nextH := nextJoiner;
end;
//------------------------------------------------------------------------------

function OutPtInTrialHorzList(op: POutPt): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := Assigned(op.Joiner) and
    ((op.Joiner.idx < 0) or Assigned(GetHorzTrialParent(op)));
end;
//------------------------------------------------------------------------------

function InsertOp(const pt: TPoint64; insertAfter: POutPt): POutPt;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  new(Result);
  Result.Pt := pt;
  Result.Joiner := nil;
  Result.OutRec := insertAfter.OutRec;
  Result.Next := insertAfter.Next;
  insertAfter.Next.Prev := Result;
  insertAfter.Next := Result;
  Result.Prev := insertAfter;
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
  FPreserveCollinear := true;
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
    //in case of exceptions ...
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
begin
  if not Assigned(FZFunc) then Exit;

  //prioritize subject vertices over clip vertices
  //and pass the subject vertices before clip vertices in the callback
  if (GetPolyType(e1) = ptSubject) then
  begin
    if (XYCoordsEqual(intersectPt, e1.bot)) then intersectPt.Z := e1.bot.Z
    else if (XYCoordsEqual(intersectPt, e1.top)) then intersectPt.Z := e1.top.Z
    else if (XYCoordsEqual(intersectPt, e2.bot)) then intersectPt.Z := e2.bot.Z
    else if (XYCoordsEqual(intersectPt, e2.top)) then intersectPt.Z := e2.top.Z;
    FZFunc(e1.bot, e1.top, e2.bot, e2.top, intersectPt);
  end else
  begin
    if (XYCoordsEqual(intersectPt, e2.bot)) then intersectPt.Z := e2.bot.Z
    else if (XYCoordsEqual(intersectPt, e2.top)) then intersectPt.Z := e2.top.Z
    else if (XYCoordsEqual(intersectPt, e1.bot)) then intersectPt.Z := e1.bot.Z
    else if (XYCoordsEqual(intersectPt, e1.top)) then intersectPt.Z := e1.top.Z;
    FZFunc(e2.bot, e2.top, e1.bot, e1.top, intersectPt);
  end;
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

procedure TClipperBase.DisposeOutRecsAndJoiners;
var
  i: Integer;
begin
  //just in case joiners haven't already been disposed
  for i := 0 to FJoinerList.Count -1 do
    if Assigned(FJoinerList[i]) then
      Dispose(PJoiner(FJoinerList[i]));
  FJoinerList.Clear;
  FHorzTrials := nil;

  for i := 0 to FOutRecList.Count -1 do
    with POutRec(FOutRecList[i])^ do
    begin
      if Assigned(Pts) then DisposeOutPts(Pts);
      Dispose(POutRec(FOutRecList[i]));
    end;
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

procedure TClipperBase.AddPathsToVertexList(const paths: TPaths64;
  polyType: TPathType; isOpen: Boolean);
var
  i, j, len, totalVerts: integer;
  p: PPoint64;
  v, va0, vaCurr, vaPrev: PVertex;
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
  //count the total (maximum) number of vertices required
  totalVerts := 0;
  for i := 0 to High(paths) do
    totalVerts := totalVerts + Length(paths[i]);
  if (totalVerts = 0) then Exit;
  //allocate memory
  GetMem(v, sizeof(TVertex) * totalVerts);
  FVertexArrayList.Add(v);

  for i := 0 to High(paths) do
  begin
    len := Length(paths[i]);
    if len = 0 then Continue;
    p := @paths[i][0];
    va0 := v; vaCurr := v;
    vaCurr.Pt := p^;
    vaCurr.Prev := nil;
    inc(p);
    vaCurr.Flags := [];
    vaPrev := vaCurr;
    inc(vaCurr);
    for j := 1 to len -1 do
    begin
      if PointsEqual(vaPrev.Pt, p^) then
      begin
        inc(p);
        Continue; //skips duplicates
      end;
      vaPrev.Next := vaCurr;
      vaCurr.Prev := vaPrev;
      vaCurr.Pt := p^;
      vaCurr.Flags := [];
      vaPrev := vaCurr;
      inc(vaCurr);
      inc(p);
    end;
    if not Assigned(vaPrev.Prev) then Continue;
    if not isOpen and PointsEqual(vaPrev.Pt, va0.Pt) then
      vaPrev := vaPrev.Prev;

    vaPrev.Next := va0;
    va0.Prev := vaPrev;
    v := vaCurr; //ie get ready for next path
    if isOpen and (va0.Next = va0) then Continue;

    //now find and assign local minima
    if (isOpen) then
    begin
      vaCurr := va0.Next;
      while (vaCurr <> va0) and (vaCurr.Pt.Y = va0.Pt.Y) do
        vaCurr := vaCurr.Next;
      ascending := vaCurr.Pt.Y <= va0.Pt.Y;
      if (ascending) then
      begin
        va0.Flags := [vfOpenStart];
        AddLocMin(va0);
      end
      else
        va0.Flags := [vfOpenStart, vfLocMax];
    end else
    begin
      //closed path
      vaPrev := va0.Prev;
      while (vaPrev <> va0) and (vaPrev.Pt.Y = va0.Pt.Y) do
        vaPrev := vaPrev.Prev;
      if (vaPrev = va0) then
        Continue; //only open paths can be completely flat
      ascending := vaPrev.Pt.Y > va0.Pt.Y;
    end;

    ascending0 := ascending;
    vaPrev := va0;
    vaCurr := va0.Next;
    while (vaCurr <> va0) do
    begin
      if (vaCurr.Pt.Y > vaPrev.Pt.Y) and ascending then
      begin
        Include(vaPrev.flags, vfLocMax);
        ascending := false;
      end
      else if (vaCurr.Pt.Y < vaPrev.Pt.Y) and not ascending then
      begin
        ascending := true;
        AddLocMin(vaPrev);
      end;
      vaPrev := vaCurr;
      vaCurr := vaCurr.Next;
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
  AddPathsToVertexList(paths, pathType, isOpen);
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
  a2BotY: Int64;
  a2IsLeftBound: Boolean;
  d: double;
begin
  //nb: a2 is always the edge being inserted

  if a2.CurrX <> a1.CurrX then
  begin
    Result := a2.CurrX > a1.CurrX;
    Exit;
  end;

  //get the turning direction  a1.top, a2.bot, a2.top
  d := CrossProduct(a1.Top, a2.bot, a2.top);
  if d <> 0 then
  begin
    Result := d < 0;
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
    Result := CrossProduct(a1.Bot, a1.Top, NextVertex(a1).Pt) <= 0;
    Exit;
  end
  else if IsOpen(a2) and not IsMaxima(a2) and
    (a2.bot.Y <= a1.bot.Y) and not IsSamePolyType(a1, a2) and
    (a2.top.Y > a1.top.Y) then
  begin
    Result := CrossProduct(a2.Bot, a2.Top, NextVertex(a2).Pt) >= 0;
    Exit;
  end;

  a2BotY := a2.Bot.Y;
  a2IsLeftBound := IsLeftBound(a2);
  if  not IsOpen(a1) and
    (a1.Bot.Y = a2BotY) and (a1.LocMin.Vertex.Pt.Y = a2BotY) then
  begin
    //a1 must also be new
    if IsLeftBound(a1) <> a2IsLeftBound then
      Result := a2IsLeftBound
    else if (CrossProduct(PrevPrevVertex(a1).Pt, a1.Bot, a1.Top) = 0) then
      Result := true
    else
      //compare turning direction of the alternate bound
      Result := (CrossProduct(PrevPrevVertex(a1).Pt,
        a2.Bot, PrevPrevVertex(a2).Pt) > 0) = a2IsLeftBound;
  end
  else
    Result := a2IsLeftBound;
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
  else if not IsValidAelOrder(FActives, e) then
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
  op: POutPt;
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
{$IFDEF REVERSE_ORIENTATION}
      leftB.WindDx := -1;
{$ELSE}
      leftB.WindDx := 1;
{$ENDIF}
      leftB.vertTop := locMin.Vertex.Prev;
      leftB.Top := leftB.vertTop.Pt;
      leftB.CurrX := leftB.Bot.X;
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
{$IFDEF REVERSE_ORIENTATION}
      rightB.WindDx := 1;
{$ELSE}
      rightB.WindDx := -1;
{$ENDIF}
      rightB.vertTop := locMin.Vertex.Next;
      rightB.Top := rightB.vertTop.Pt;
      rightB.CurrX := rightB.Bot.X;
      SetDx(rightB);
    end;
    //Currently LeftB is just descending and RightB is ascending,
    //so now we swap them if LeftB isn't actually on the left.
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
    LeftB.LeftBound := true; //nb: we can't use winddx instead

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
      rightB.WindCnt := leftB.WindCnt;
      rightB.WindCnt2 := leftB.WindCnt2;
      InsertRightEdge(leftB, rightB);        ////////////////

      if contributing then
      begin
        AddLocalMinPoly(leftB, rightB, leftB.Bot, true);

        if not IsHorizontal(leftB) and
          TestJoinWithPrev1(leftB, botY) then
        begin
          op := AddOutPt(leftB.PrevInAEL, leftB.Bot);
          AddJoin(op, leftB.OutRec.Pts);
        end;
      end;

      while Assigned(rightB.NextInAEL) and
        IsValidAelOrder(rightB.NextInAEL, rightB) do
      begin
        IntersectEdges(rightB, rightB.NextInAEL, rightB.Bot);
        SwapPositionsInAEL(rightB, rightB.NextInAEL);
      end;

      if not IsHorizontal(rightB) and
        TestJoinWithNext1(rightB, botY) then
      begin
        op := AddOutPt(rightB.NextInAEL, rightB.Bot);
        AddJoin(rightB.OutRec.Pts, op);
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
  newOr: POutRec;
begin
  new(newOr);
  newOr.Idx := FOutRecList.Add(newOr);
  newOr.Pts := nil;
  newOr.Split := nil;
  newOr.PolyPath := nil;
  newOr.State := osUndefined;

  e1.OutRec := newOr;
  SetOwnerAndInnerOuterState(e1);
  e2.OutRec := newOr;
  if not IsOpen(e1) then
  begin
    //Setting the owner and inner/outer states (above) is an essential
    //precursor to setting edge 'sides' (ie left and right sides of output
    //polygons) and hence the orientation of output paths ...
    if IsOuter(newOr) = IsNew then
      SetSides(newOr, e1, e2) else
      SetSides(newOr, e2, e1);
  end;
  new(Result);
  newOr.Pts := Result;
  Result.Pt := pt;
  Result.Joiner := nil;
  Result.OutRec := newOr;
  Result.Prev := Result;
  Result.Next := Result;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.CleanCollinear(outRec: POutRec);
var
  op2, startOp: POutPt;
begin
  outRec := GetRealOutRec(outRec);
  if not Assigned(outRec) or Assigned(outRec.FrontE) or
    not ValidateClosedPathEx(outRec.Pts) then Exit;

  startOp := outRec.Pts;
  op2 := startOp;
  while true do
  begin
    if Assigned(op2.Joiner) then Exit;
    if (CrossProduct(op2.Prev.Pt, op2.Pt, op2.Next.Pt) = 0) and
      (PointsEqual(op2.Pt,op2.Prev.Pt) or
      PointsEqual(op2.Pt,op2.Next.Pt) or
      not preserveCollinear or
      (DotProduct(op2.Prev.Pt, op2.Pt, op2.Next.Pt) < 0)) then
    begin
      if op2 = outRec.Pts then outRec.Pts := op2.Prev;
      op2 := DisposeOutPt(op2);
      if not ValidateClosedPathEx(op2) then
      begin
        outRec.Pts := nil;
        Exit;
      end;
      startOp := op2;
      Continue;
    end;
    op2 := op2.Next;
    if op2 = startOp then Break;
  end;
  FixSelfIntersects(outRec.Pts);
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
    if Assigned(FZFunc) then
      FZFunc(prevOp.Pt, splitOp.Pt, splitOp.Next.Pt, nextNextOp.Pt, ip.Pt);
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
      newOp2.Joiner := nil;
      newOp2.OutRec := prevOp.OutRec;
      newOp2.Prev := prevOp;
      newOp2.Next := nextNextOp;
      nextNextOp.Prev := newOp2;
      prevOp.Next := newOp2;
    end;

    SafeDeleteOutPtJoiners(splitOp.Next);
    SafeDeleteOutPtJoiners(splitOp);

    if (Abs(area2) >= 1) and
      (((Abs(area2) > (Abs(area1))) or
      ((area2 > 0) = (area1 > 0)))) then
    begin
      new(newOutRec);
      FillChar(newOutRec^, SizeOf(TOutRec), 0);
      newOutRec.Idx := FOutRecList.Add(newOutRec);
      newOutRec.Owner := prevOp.OutRec.Owner;
      newOutRec.State := prevOp.OutRec.State;
      newOutRec.PolyPath := nil;
      newOutRec.Split := nil;
      splitOp.OutRec := newOutRec;
      splitOp.Next.OutRec := newOutRec;
      new(newOp);
      newOp.Pt := ip;
      newOp.Joiner := nil;
      newOp.OutRec := newOutRec;
      newOp.Prev := splitOp.Next;
      newOp.Next := splitOp;
      splitOp.Prev := newOp;
      splitOp.Next.Next := newOp;
      newOutRec.Pts := newOp;
    end else
    begin
      Dispose(splitOp.Next);
      Dispose(splitOp);
    end;
  end;

var
  op2: POutPt;
begin
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
  if (IsFront(e1) = IsFront(e2)) then
  begin
    //something is wrong so fix it!
    if not IsOpen(e1) then
    begin
      //we should practically never get here
      if not FixSides(e1, e2) then
      begin
        Result := nil;
        Exit;
      end;
    end
    else
      SwapFrontBackSides(e2.OutRec);
  end;

  Result := AddOutPt(e1, pt);
  if (e1.OutRec = e2.OutRec) then
  begin
    outRec := e1.outRec;
    outRec.Pts := Result;
    UncoupleOutRec(e1);
    if not IsOpen(e1) then CleanCollinear(outRec);
    Result := outRec.Pts;
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
    if not IsOpen(e1) then
    begin
      e1.OutRec.FrontE := e2.OutRec.FrontE;
      e1.OutRec.FrontE.OutRec := e1.OutRec;
    end;
  end else
  begin
    p1_end.Prev := p2_start;
    p2_start.Next := p1_end;
    p1_start.Next := p2_end;
    p2_end.Prev := p1_start;
    if not IsOpen(e1) then
    begin
      e1.OutRec.BackE := e2.OutRec.BackE;
      e1.OutRec.BackE.OutRec := e1.OutRec;
    end;
  end;

  //after joining, the e2.OutRec mustn't contains vertices
  e2.OutRec.FrontE := nil;
  e2.OutRec.BackE := nil;
  e2.OutRec.Pts := nil;
  e2.OutRec.Owner := e1.OutRec;

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
    Result.Joiner := nil;
    Result.OutRec := outrec;
    opBack.Prev := Result;
    Result.Prev := opFront;
    Result.Next := opBack;
    opFront.Next := Result;
    if toFront then outrec.Pts := Result;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.AddJoin(op1, op2: POutPt);
var
  joiner: PJoiner;
begin
  if (op1.OutRec = op2.OutRec) and ((op1 = op2) or
  //unless op1.next or op1.prev crosses the start-end divide
  //don't waste time trying to join adjacent vertices
  ((op1.Next = op2) and (op1 <> op1.OutRec.Pts)) or
  ((op2.Next = op1) and (op2 <> op1.OutRec.Pts))) then Exit;

  new(joiner);
  joiner.idx := FJoinerList.Add(joiner);
  joiner.op1 := op1;
  joiner.op2 := op2;
  joiner.nextH := nil;
  joiner.next1 := op1.Joiner;
  joiner.next2 := op2.Joiner;
  op1.Joiner := joiner;
  op2.Joiner := joiner;
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
  //This method deletes a single join, and it doesn't check for or
  //delete trial horz. joins. For that, use the following method.

  op1 := joiner.op1;
  op2 := joiner.op2;

  //both op1 and op2 can be associated with multiple joiners which
  //are chained together so we need to break and rejoin that chain

  if op1.Joiner <> joiner then
  begin
    parentJnr := FindJoinParent(joiner, op1);
    if parentJnr.op1 = op1 then
      parentJnr.next1 := joiner.next1 else
      parentJnr.next2 := joiner.next1;
  end else
    op1.Joiner := Joiner.next1;

  if op2.Joiner <> joiner then
  begin
    parentJnr := FindJoinParent(joiner, op2);
    if parentJnr.op1 = op2 then
      parentJnr.next1 := joiner.next2 else
      parentJnr.next2 := joiner.next2;
  end else
    op2.Joiner := joiner.next2;

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
    if Assigned(FJoinerList[i]) then
    begin
      joiner := FJoinerList[i];
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
  opCurr := outRec.Pts;
  repeat
    opCurr.OutRec := outRec;
    opCurr := opCurr.Next;
  until opCurr = outRec.Pts;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.CompleteSplit(op1, op2: POutPt; OutRec: POutRec);
var
  i: integer;
  area1, area2: double;
  newOr: POutRec;
begin
  area1 := Area(op1);
  area2 := Area(op2);
  if Abs(area1) < 1 then
  begin
    SafeDisposeOutPts(op1);
    op1 := nil;
  end
  else if Abs(area2) < 1 then
  begin
    SafeDisposeOutPts(op2);
    op2 := nil;
  end;
  if not Assigned(op1) then
    OutRec.Pts := op2
  else if not Assigned(op2) then
    OutRec.Pts := op1
  else
  begin
    new(newOr);
    FillChar(newOr^, SizeOf(TOutRec), 0);
    newOr.Idx := FOutRecList.Add(newOr);
    newOr.PolyPath := nil;
    newOr.Split := nil;

    i := Length(OutRec.Split);
    SetLength(OutRec.Split, i +1);
    OutRec.Split[i] := newOr;

    if Abs(area1) >= Abs(area2) then
    begin
      OutRec.Pts := op1;
      newOr.Pts := op2;
    end else
    begin
      OutRec.Pts := op2;
      newOr.Pts := op1;
    end;

    if (area1 > 0) = (area2 > 0) then
    begin
      newOr.Owner := OutRec.Owner;
      newOr.State := OutRec.State;
    end else
    begin
      newOr.Owner := OutRec;
      if OutRec.State = osOuter then
        newOr.State := osInner else
        newOr.State := osOuter;
    end;
    UpdateOutrecOwner(newOr);
    CleanCollinear(newOr);
  end;
end;
//------------------------------------------------------------------------------

function CollinearSegsOverlap(const  seg1a, seg1b,
  seg2a, seg2b: TPoint64): Boolean;
begin
  //precondition: seg1 and seg2 are collinear
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
  //nb: points may not be collinear
  Result := ValueEqualOrBetween(pt.X, corner1.X, corner2.X) and
    ValueEqualOrBetween(pt.Y, corner1.Y, corner2.Y);
end;
//------------------------------------------------------------------------------

function CheckDisposeAdjacent(var op: POutPt; guard: POutPt;
  outRec: POutRec): Boolean;
begin
  Result := false;
  while (op.Prev <> op) do
  begin
    if PointsEqual(op.Pt, op.Prev.Pt) and
      (op <> guard) and Assigned(op.Prev.Joiner) and
      not Assigned(op.Joiner) then
    begin
      if op = outRec.Pts then outRec.Pts := op.Prev;
      op := DisposeOutPt(op);
      op := op.Prev;
    end
    else if not Assigned(op.Prev.Joiner) and
    (op.Prev <> guard) and
    (DistanceSqr(op.Pt, op.Prev.Pt) < 2.1) then
    begin
      if op.Prev = outRec.Pts then outRec.Pts := op;
      DisposeOutPt(op.Prev);
      Result := true;
    end else
      break;
  end;
  while (op.Next <> op) do
  begin
    if PointsEqual(op.Pt, op.Next.Pt) and
      (op <> guard) and Assigned(op.Next.Joiner) and
      not Assigned(op.Joiner) then
    begin
      if op = outRec.Pts then outRec.Pts := op.Prev;
      op := DisposeOutPt(op);
      op := op.Prev;
    end
    else if not Assigned(op.Next.Joiner) and
      (op.Next <> guard) and
      (DistanceSqr(op.Pt, op.Next.Pt) < 2.1) then
    begin
      if op.Next = outRec.Pts then outRec.Pts := op;
      DisposeOutPt(op.Next);
      Result := true;
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

  or1 := GetRealOutRec(op1.OutRec);
  or2 := GetRealOutRec(op2.OutRec);
  op1.OutRec := or1;
  op2.OutRec := or2;
  DeleteJoin(joiner);

  Result := or1;
  if not Assigned(or2.Pts) then
    Exit
  else if not IsValidClosedPath(op2) then
  begin
    CleanCollinear(or2);
    Exit;
  end
  else if not Assigned(or1.Pts) or
    not IsValidClosedPath(op1) then
  begin
    CleanCollinear(or1);
    Result := or2; //ie tidy or2 in calling function;
    Exit;
  end
  else if (or1 = or2) and ((op1 = op2) or
    (op1.Next = op2) or (op1.Prev = op2)) then
  begin
    Exit;
  end;

  CheckDisposeAdjacent(op1, op2, or1);
  CheckDisposeAdjacent(op2, op1, or2);
  if (op1.Next = op2) or (op2.Next = op1) then Exit;

  while True do
  begin
    if not IsValidPath(op1) or not IsValidPath(op2) or
      ((or1 = or2) and ((op1.Prev = op2) or (op1.Next = op2))) then Exit;

    if PointsEqual(op1.Prev.Pt, op2.Next.Pt) or
    ((CrossProduct(op1.Prev.Pt, op1.Pt, op2.Next.Pt) = 0) and
      CollinearSegsOverlap(op1.Prev.Pt, op1.Pt, op2.Pt, op2.Next.Pt)) then
    begin
      if or1 = or2 then
      begin
        //SPLIT REQUIRED
        //make sure op1.prev and op2.next match positions
        //by inserting an extra vertex if needed
        if not PointsEqual(op1.Prev.Pt, op2.Next.Pt) then
        begin
          if PointBetween(op1.Prev.Pt, op2.Pt, op2.Next.Pt) then
            op2.Next := InsertOp(op1.Prev.Pt, op2) else
            op1.Prev := InsertOp(op2.Next.Pt, op1.Prev);
        end;
        //current              to     new
        //op1.p[opA] >>> op1   ...    opA \   / op1
        //op2.n[opB] <<< op2   ...    opB /   \ op2
        opA := op1.Prev;
        opB := op2.Next;
        opA.Next := opB;
        opB.Prev := opA;
        op1.Prev := op2;
        op2.Next := op1;
        CompleteSplit(op1, opA, or1);
      end else
      begin
        //JOIN, NOT SPLIT
        opA := op1.Prev;
        opB := op2.Next;
        opA.Next := opB;
        opB.Prev := opA;
        op1.Prev := op2;
        op2.Next := op1;
        //this isn't essential but it's
        //easier to track ownership when it
        //always defers to the lower index
        if or1.Idx < or2.Idx then
        begin
          or1.Pts := op1;
          or2.Pts := nil;
          or2.owner := or1
        end else
        begin
          Result := or2;
          or2.Pts := op1;
          or1.Pts := nil;
          or1.owner := or2;
        end;
      end;
      Break;
    end
    else if PointsEqual(op1.Next.Pt, op2.Prev.Pt) or
      ((CrossProduct(op1.Next.Pt, op2.Pt, op2.Prev.Pt) = 0) and
       CollinearSegsOverlap(op1.Next.Pt, op1.Pt, op2.Pt, op2.Prev.Pt)) then
    begin
      if or1 = or2 then
      begin
        //SPLIT REQUIRED
        //make sure op2.prev and op1.next match positions
        //by inserting an extra vertex if needed
        if not PointsEqual(op1.Next.Pt, op2.Prev.Pt) then
        begin
          if PointBetween(op2.Prev.Pt, op1.Pt, op1.Next.Pt) then
            op1.Next := InsertOp(op2.Prev.Pt, op1) else
            op2.Prev := InsertOp(op1.Next.Pt, op2.Prev);
        end;
        //current              to     new
        //op2.p[opA] >>> op2   ...    opA \   / op2
        //op1.n[opB] <<< op1   ...    opB /   \ op1
        opA := op2.Prev;
        opB := op1.Next;
        opA.Next := opB;
        opB.Prev := opA;
        op2.Prev := op1;
        op1.Next := op2;
        CompleteSplit(op1, opA, or1);
      end else
      begin
        //JOIN, NOT SPLIT
        opA := op1.Next;
        opB := op2.Prev;
        opA.Prev := opB;
        opB.Next := opA;
        op2.Prev := op1;
        op1.Next := op2;
        if or1.Idx < or2.Idx then
        begin
          or1.Pts := op1;
          or2.Pts := nil;
          or2.owner := or1;
        end else
        begin
          Result := or2;
          or2.Pts := op1;
          or1.Pts := nil;
          or1.owner := or2;
        end;
      end;
      Break;
    end
    else if PointBetween(op1.Next.Pt, op2.Pt, op2.Prev.Pt) and
      (DistanceFromLineSqrd(op1.Next.Pt, op2.Pt, op2.Prev.Pt) < 2.01) then
    begin
      InsertOp(op1.Next.Pt, op2.Prev);
      Continue;
    end
    else if PointBetween(op2.Next.Pt, op1.Pt, op1.Prev.Pt) and
      (DistanceFromLineSqrd(op2.Next.Pt, op1.Pt, op1.Prev.Pt) < 2.01) then
    begin
      InsertOp(op2.Next.Pt, op1.Prev);
      Continue;
    end
    else if PointBetween(op1.Prev.Pt, op2.Pt, op2.Next.Pt) and
      (DistanceFromLineSqrd(op1.Prev.Pt, op2.Pt, op2.Next.Pt) < 2.01) then
    begin
      InsertOp(op1.Prev.Pt, op2);
      Continue;
    end
    else if PointBetween(op2.Prev.Pt, op1.Pt, op1.Next.Pt) and
      (DistanceFromLineSqrd(op2.Prev.Pt, op1.Pt, op1.Next.Pt) < 2.01) then
    begin
      InsertOp(op2.Prev.Pt, op1);
      Continue;
    end;

    //something odd needs tidying up
    if CheckDisposeAdjacent(op1, op2, or1) then Continue
    else if CheckDisposeAdjacent(op2, op1, or1) then Continue
    else if not PointsEqual(op1.Prev.Pt, op2.Next.Pt) and
      (DistanceSqr(op1.Prev.Pt, op2.Next.Pt) < 2.01) then
    begin
      op1.Prev.Pt := op2.Next.Pt;
      Continue;
    end
    else if not PointsEqual(op1.Next.Pt, op2.Prev.Pt) and
      (DistanceSqr(op1.Next.Pt, op2.Prev.Pt) < 2.01) then
    begin
      op2.Prev.Pt := op1.Next.Pt;
      Continue;
    end else
    begin
      //OK, there doesn't seem to be a way to join afterall
      //so just tidy up the polygons
      or1.Pts := op1;
      if or2 <> or1 then
      begin
        or2.Pts := op2;
        CleanCollinear(or2);
      end;
      Break;
    end;
  end; //end while
end;
//------------------------------------------------------------------------------

function TClipperBase.StartOpenPath(e: PActive; const pt: TPoint64): POutPt;
var
  newOr: POutRec;
begin
  new(newOr);
  newOr.Idx := FOutRecList.Add(newOr);
  newOr.Owner := nil;
  newOr.State := osOpen;
  newOr.Pts := nil;
  newOr.Split := nil;
  newOr.PolyPath := nil;
  newOr.FrontE := nil;
  newOr.BackE := nil;
  e.OutRec := newOr;

  new(Result);
  newOr.Pts := Result;
  Result.Pt := pt;
  Result.Joiner := nil;
  Result.Prev := Result;
  Result.Next := Result;
  Result.OutRec := newOr;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.UpdateEdgeIntoAEL(var e: PActive);
var
  op1, op2: POutPt;
begin
  e.Bot := e.Top;
  e.vertTop := NextVertex(e);
  e.Top := e.vertTop.Pt;
  e.CurrX := e.Bot.X;
  SetDx(e);
  if IsHorizontal(e) then Exit;
  InsertScanLine(e.Top.Y);
  if TestJoinWithPrev1(e, e.Bot.Y) then
  begin
    op1 := AddOutPt(e.PrevInAEL, e.Bot);
    op2 := AddOutPt(e, e.Bot);
    AddJoin(op1, op2);
  end;
end;
//------------------------------------------------------------------------------

{$IFNDEF USINGZ}
{$HINTS OFF}
{$ENDIF}
function TClipperBase.IntersectEdges(e1, e2: PActive; pt: TPoint64): POutPt;
var
  e1WindCnt, e2WindCnt, e1WindCnt2, e2WindCnt2: Integer;
  op2: POutPt;
begin
  Result := nil;

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
      Result := AddOutPt(e1, pt);
      {$IFDEF USINGZ}
      SetZ(e1, e2, Result.pt);
      {$ENDIF}
      e1.OutRec := nil;
    end
    else
      Result := StartOpenPath(e1, pt);
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
      Result := AddLocalMaxPoly(e1, e2, pt);
      {$IFDEF USINGZ}
      if Assigned(Result) then SetZ(e1, e2, Result.pt);
      {$ENDIF}
    end else if IsFront(e1) or (e1.OutRec = e2.OutRec) then
    begin
      //this else condition isn't strictly needed but
      //it's easier to join polygons than break apart complex ones
      Result := AddLocalMaxPoly(e1, e2, pt);
      op2 := AddLocalMinPoly(e1, e2, pt);
      {$IFDEF USINGZ}
      if Assigned(Result) then SetZ(e1, e2, Result.pt);
      SetZ(e1, e2, op2.pt);
      {$ENDIF}
      if Assigned(Result) and PointsEqual(Result.Pt, op2.Pt) and
        not IsHorizontal(e1) and not IsHorizontal(e2) and
        (CrossProduct(e1.Bot, Result.Pt, e2.Bot) = 0) then
          AddJoin(Result, op2);
    end else
    begin
      //can't treat as maxima & minima
      Result := AddOutPt(e1, pt);
      op2 := AddOutPt(e2, pt);
      {$IFDEF USINGZ}
      SetZ(e1, e2, Result.pt);
      SetZ(e1, e2, op2.pt);
      {$ENDIF}
      SwapOutRecs(e1, e2);
    end;
  end

  //if one or other edge is 'hot' ...
  else if IsHotEdge(e1) then
  begin
    Result := AddOutPt(e1, pt);
    {$IFDEF USINGZ}
    SetZ(e1, e2, op.pt);
    {$ENDIF}
    SwapOutRecs(e1, e2);
  end
  else if IsHotEdge(e2) then
  begin
    Result := AddOutPt(e2, pt);
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
        else //xOr
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
  op := nil;
end;
//------------------------------------------------------------------------------

function TClipperBase.FixSides(e1, e2: PActive): Boolean;
begin
  Result := true;
  if ValidateClosedPathEx(e1.OutRec.Pts) and
    ValidateClosedPathEx(e2.OutRec.Pts) then
  begin
    if CheckFixInnerOuter(e1) and
      (IsOuter(e1.OutRec) <> IsFront(e1)) then
      SwapFrontBackSides(e1.OutRec)
    else if CheckFixInnerOuter(e2) and
      (IsOuter(e2.OutRec) <> IsFront(e2)) then
      SwapFrontBackSides(e2.OutRec)
    else
      Raise EClipperLibException(rsClipper_ClippingErr);
  end
  else if not Assigned(e1.OutRec.Pts) then
  begin
    if Assigned(e2.OutRec.Pts) and
      ValidateClosedPathEx(e2.OutRec.Pts) then
        Raise EClipperLibException(rsClipper_ClippingErr); //e2 can't join onto nothing!
    UncoupleOutRec(e1);
    UncoupleOutRec(e2);
    Result := false;
  end
  else
    Raise EClipperLibException(rsClipper_ClippingErr); //e1 can't join onto nothing!
end;
//------------------------------------------------------------------------------

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
    ConvertHorzTrialsToJoins;
    FBotY := Y;                       //FBotY == bottom of current scanbeam
    if not PopScanLine(Y) then Break; //Y     == top of current scanbeam
    DoIntersections(Y);
    DoTopOfScanbeam(Y);
    while PopHorz(e) do DoHorizontal(e);
  end;
  ProcessJoinList;
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
    Dispose(PIntersectNode(FIntersectList[i]));
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
  //nb: edge.PrevInSEL is always assigned
  Result := edge.NextInSEL;
  if Assigned(Result) then
    Result.PrevInSEL := edge.PrevInSEL;
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

procedure TClipperBase.ProcessIntersectList;
var
  i, j, highI: Integer;
  node: PIntersectNode;
  op1, op2: POutpt;
begin
  //The list of required intersections now needs to be processed in a specific
  //order such that intersection points with the largest Y coords are processed
  //before those with the smallest Y coords. However, it's critical that edges
  //are adjacent at the time of intersection.

  //First we do a quicksort so that intersections will be processed
  //generally from largest Y to smallest (as long as they're adjacent)
  FIntersectList.Sort(IntersectListSort);

  highI := FIntersectList.Count - 1;
  for i := 0 to highI do
  begin
    //make sure edges are adjacent, otherwise
    //change the intersection order before proceeding
    if not EdgesAdjacentInAEL(FIntersectList[i]) then
    begin
      j := i + 1;
      while not EdgesAdjacentInAEL(FIntersectList[j]) do inc(j);
      //now swap intersection order
      node := FIntersectList[i];
      FIntersectList[i] := FIntersectList[j];
      FIntersectList[j] := node;
    end;

    //now process the intersection
    node := FIntersectList[i];
    with node^ do
    begin
      IntersectEdges(Edge1, Edge2, Pt);
      SwapPositionsInAEL(Edge1, Edge2);

      if TestJoinWithPrev2(Edge2, pt) then
      begin
        op1 := AddOutPt(Edge2.PrevInAEL, pt);
        op2 := AddOutPt(Edge2, pt);
        if op1 <> op2 then
          AddJoin(op1, op2);
      end
      else if TestJoinWithNext2(Edge1, pt) then
      begin
        op1 := AddOutPt(Edge1, pt);
        op2 := AddOutPt(Edge1.NextInAEL, pt);
        if op1 <> op2 then
          AddJoin(op1, op2);
      end;
    end;
  end;
  //Edges should once again be correctly ordered (left to right) in the AEL.
end;
//------------------------------------------------------------------------------

procedure TClipperBase.SwapPositionsInAEL(e1, e2: PActive);
var
  prev, next: PActive;
begin
  //preconditon: e1 must be immediately prior to e2
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

function HorzIsSpike(horzEdge: PActive): Boolean;
var
  nextPt: TPoint64;
begin
  nextPt := NextVertex(horzEdge).Pt;
  Result := (horzEdge.Bot.X < horzEdge.Top.X) <> (horzEdge.Top.X < nextPt.X);
end;
//------------------------------------------------------------------------------

function TrimHorz(horzEdge: PActive; preserveCollinear: Boolean): Boolean;
var
  pt: TPoint64;
begin
  Result := false;
  pt := NextVertex(horzEdge).Pt;
  while (pt.Y = horzEdge.top.Y) do
  begin
    //always trim 180 deg. spikes (in closed paths)
    //but otherwise break if preserveCollinear = true
    if preserveCollinear and
    ((pt.X < horzEdge.top.X) <> (horzEdge.bot.X < horzEdge.top.X)) then
      break;

    horzEdge.VertTop := NextVertex(horzEdge);
    horzEdge.top := pt;
    Result := true;
    if IsMaxima(horzEdge) then Break;
    pt := NextVertex(horzEdge).Pt;
    end;
  if (Result) then SetDx(horzEdge); // +/-infinity
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
  //make sure 'op' isn't added more than once
  if not OutPtInTrialHorzList(op) then
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
  joiner := op.Joiner;
  parentOp := nil;
  while Assigned(joiner) do
  begin
    if (joiner.idx < 0) then
    begin
      //first remove joiner from FHorzTrials list
      if joiner = FHorzTrials then
        FHorzTrials := joiner.nextH
      else
      begin
        parentH := FHorzTrials;
        while parentH.nextH <> joiner do
          parentH := parentH.nextH;
        parentH.nextH := joiner.nextH;
      end;
      //now remove joiner from op's joiner list
      if not Assigned(parentOp) then
      begin
        //joiner must be first one in list
        op.Joiner := joiner.next1;
        Dispose(joiner);
        joiner := op.Joiner;
      end else
      begin
        //this trial joiner isn't op's first
        //nb: trial joiners only have a single 'op'
        if op = parentOp.op1 then
          parentOp.next1 := joiner.next1 else
          parentOp.next2 := joiner.next1; //never joiner.next2
        Dispose(joiner);
        joiner := parentOp;
      end;
      //loop in case there's more than one trial join
    end else
    begin
      //not a trial join but just to be sure there isn't one
      //a little deeper, look further along the linked list
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
  outRec := GetRealOutRec(op.OutRec);
  op2 := op;
  if Assigned(outRec.FrontE) then
  begin
    while (op.Prev <> outRec.Pts) and
      (op.Prev.Pt.Y = op.Pt.Y) do op := op.Prev;
    while (op2 <> outRec.Pts) and
      (op2.Next.Pt.Y = op2.Pt.Y) do op2 := op2.Next;
    Result := (op2 <> op);
  end else
  begin
    while (op.Prev <> op2) and
      (op.Prev.Pt.Y = op.Pt.Y) do op := op.Prev;
    while (op2.Next <> op) and
      (op2.Next.Pt.Y = op2.Pt.Y) do op2 := op2.Next;
    Result := (op2 <> op) and (op2.Next <> op);
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
    if op1a.Joiner = joiner then
    begin
      op1a.Joiner := joiner.next1;
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
      CleanCollinear(op1a.OutRec);
      Continue;
    end;

    joined := false;
    joiner := FHorzTrials;
    while Assigned(joiner) do
    begin
      op2a := joiner.op1;
      if GetHorzExtendedHorzSeg(op2a, op2b) and
        HorzEdgesOverlap(op1a.Pt.X, op1b.Pt.X, op2a.Pt.X, op2b.Pt.X) then
      begin
        joined := true;
        //overlap found so promote to a 'real' join
        if PointsEqual(op1a.Pt, op2b.Pt) then
          AddJoin(op1a, op2b)
        else if PointsEqual(op1a.Pt, op2a.Pt) then
          AddJoin(op1a, op2a)
        else if PointsEqual(op1b.Pt, op2a.Pt) then
          AddJoin(op1b, op2a)
        else if PointsEqual(op1b.Pt, op2b.Pt) then
          AddJoin(op1b, op2b)
        else if ValueBetween(op1a.Pt.X, op2a.Pt.X, op2b.Pt.X) then
          AddJoin(op1a, InsertOp(op1a.Pt, op2a))
        else if ValueBetween(op1b.Pt.X, op2a.Pt.X, op2b.Pt.X) then
          AddJoin(op1b, InsertOp(op1b.Pt, op2a))
        else if ValueBetween(op2a.Pt.X, op1a.Pt.X, op1b.Pt.X) then
          AddJoin(op2a, InsertOp(op2a.Pt, op1a))
        else if ValueBetween(op2b.Pt.X, op1a.Pt.X, op1b.Pt.X) then
          AddJoin(op2b, InsertOp(op2b.Pt, op1a));
        Break;
      end;
      joiner := joiner.nextH;
    end;
    if not joined then
      CleanCollinear(op1a.OutRec);
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
  Y := horzEdge.Bot.Y;
  maxVertex := nil;
  maxPair := nil;

  if not horzIsOpen then
  begin
    maxVertex := GetCurrYMaximaVertex(horzEdge);
    if Assigned(maxVertex) then
    begin
      maxPair := GetHorzMaximaPair(horzEdge, maxVertex);
      //remove 180 deg.spikes and also simplify
      //consecutive horizontals when PreserveCollinear = true
      if (maxVertex <> horzEdge.VertTop) then
          TrimHorz(horzEdge, FPreserveCollinear);
    end;
  end;

  isLeftToRight := ResetHorzDirection;

  //nb: TrimHorz above hence not using Bot.X here
  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, Point64(horzEdge.CurrX, Y));

  while true do //loop through consec. horizontal edges
  begin

    if horzIsOpen and
      IsMaxima(horzEdge) and not IsOpenEnd(horzEdge) then
    begin
      maxVertex := GetCurrYMaximaVertex(horzEdge);
      if Assigned(maxVertex) then
        maxPair := GetHorzMaximaPair(horzEdge, maxVertex);
    end;

    if isLeftToRight  then
      e := horzEdge.NextInAEL else
      e := horzEdge.PrevInAEL;

    while assigned(e) do
    begin
      if (e = maxPair) then
      begin
        if IsHotEdge(horzEdge) then
        begin
          while horzEdge.VertTop <> e.VertTop do
          begin
            AddOutPt(horzEdge, horzEdge.Top);
            UpdateEdgeIntoAEL(horzEdge);
          end;
          if isLeftToRight then
            op := AddLocalMaxPoly(horzEdge, e, horzEdge.Top) else
            op := AddLocalMaxPoly(e, horzEdge, horzEdge.Top);
          if Assigned(op) and not IsOpen(horzEdge) and
            PointsEqual(op.Pt, horzEdge.Top) then
              AddTrialHorzJoin(op);
        end;
        //remove horzEdge's maxPair from AEL
        DeleteFromAEL(e);
        DeleteFromAEL(horzEdge);
        Exit;
      end;

      //if horzEdge is a maxima, keep going until we reach
      //its maxima pair, otherwise check for Break conditions
      if (maxVertex <> horzEdge.VertTop) or IsOpenEnd(horzEdge) then
      begin
        //otherwise stop when 'e' is beyond the end of the horizontal line
        if (isLeftToRight and (e.CurrX > horzRight)) or
          (not isLeftToRight and (e.CurrX < horzLeft)) then Break;

        if (e.CurrX = horzEdge.Top.X) and not IsHorizontal(e) then
        begin
          //for edges at horzEdge's end, only stop when horzEdge's
          //outslope is greater than e's slope when heading right or when
          //horzEdge's outslope is less than e's slope when heading left.
          pt := NextVertex(horzEdge).Pt;
          if (isLeftToRight and (TopX(E, pt.Y) >= pt.X)) or
            (not isLeftToRight and (TopX(E, pt.Y) <= pt.X)) then Break;
        end;
      end;

      pt := Point64(e.CurrX, Y);

      if (isLeftToRight) then
      begin
        op := IntersectEdges(horzEdge, e, pt);
        SwapPositionsInAEL(horzEdge, e);

        if IsHotEdge(horzEdge) and Assigned(op) and
          not IsOpen(horzEdge) and PointsEqual(op.Pt, pt) then
            AddTrialHorzJoin(op);

        if not IsHorizontal(e) and
          TestJoinWithPrev1(e, Y) then
        begin
          op := AddOutPt(e.PrevInAEL, pt);
          op2 := AddOutPt(e, pt);
          AddJoin(op, op2);
        end;
        horzEdge.CurrX := e.CurrX;
        e := horzEdge.NextInAEL;
      end else
      begin
        op := IntersectEdges(e, horzEdge, pt);
        SwapPositionsInAEL(e, horzEdge);

        if IsHotEdge(horzEdge) and Assigned(op) and
          not IsOpen(horzEdge) and
          PointsEqual(op.Pt, pt) then
            AddTrialHorzJoin(op);

        if not IsHorizontal(e) and
          TestJoinWithNext1(e, Y) then
        begin
          op := AddOutPt(e, pt);
          op2 := AddOutPt(e.NextInAEL, pt);
          AddJoin(op, op2);
        end;
        horzEdge.CurrX := e.CurrX;
        e := horzEdge.PrevInAEL;
      end;
    end; //we've reached the end of this horizontal

    //check if we've finished looping through consecutive horizontals
    if horzIsOpen and IsOpenEnd(horzEdge) then
    begin
      if IsHotEdge(horzEdge) then
        AddOutPt(horzEdge, horzEdge.Top);
      DeleteFromAEL(horzEdge); //ie open at top
      Exit;
    end
    else if (NextVertex(horzEdge).Pt.Y <> horzEdge.Top.Y) then
      Break;

    //there must be a following (consecutive) horizontal

    if IsHotEdge(horzEdge) then
      AddOutPt(horzEdge, horzEdge.Top);
    UpdateEdgeIntoAEL(horzEdge);

    if PreserveCollinear and not horzIsOpen and
      HorzIsSpike(horzEdge) then
       TrimHorz(horzEdge, true);

    isLeftToRight := ResetHorzDirection;
  end; //end while horizontal

  if IsHotEdge(horzEdge) then
  begin
    op := AddOutPt(horzEdge, horzEdge.Top);
    if not IsOpen(horzEdge) then
      AddTrialHorzJoin(op);
  end else
    op := nil;

  if (horzIsOpen and not IsOpenEnd(horzEdge)) or
    (not horzIsOpen and (maxVertex <> horzEdge.VertTop)) then
  begin
    UpdateEdgeIntoAEL(horzEdge); //this is the end of an intermediate horiz.
    if IsOpen(horzEdge) then Exit;

    if isLeftToRight and TestJoinWithNext1(horzEdge, Y) then
    begin
      op2 := AddOutPt(horzEdge.NextInAEL, horzEdge.Bot);
      AddJoin(op, op2);
    end
    else if not isLeftToRight and TestJoinWithPrev1(horzEdge, Y) then
    begin
      op2 := AddOutPt(horzEdge.PrevInAEL, horzEdge.Bot);
      AddJoin(op2, op);
    end;

  end
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
  //FSel is reused to flag horizontals (see PushHorz below)
  FSel := nil;
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
        if IsHotEdge(e) then
          AddOutPt(e, e.Top);
        UpdateEdgeIntoAEL(e);
        if IsHorizontal(e) then
          PushHorz(e);
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
    //must be in the middle of an open path
    if IsHotEdge(e) then
      AddLocalMaxPoly(e, eMaxPair, e.Top);
    DeleteFromAEL(eMaxPair);
    DeleteFromAEL(e);

    if assigned(ePrev) then
      Result := ePrev.NextInAEL else
      Result := FActives;
  end else
  begin
    //here E.NextInAEL == ENext == EMaxPair ...
    if IsHotEdge(e) then
      AddLocalMaxPoly(e, eMaxPair, e.Top);

    DeleteFromAEL(e);
    DeleteFromAEL(eMaxPair);
    if assigned(ePrev) then
      Result := ePrev.NextInAEL else
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
      outRec := FOutRecList[i];
      if not assigned(outRec.Pts) then Continue;

      if IsOpen(outRec) then
      begin
        if BuildPath(outRec.Pts, FFillRule = frNegative,
          true, openPaths[cntOpen]) then
            inc(cntOpen);
      end else
      begin
        if BuildPath(outRec.Pts, FFillRule = frNegative,
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

function PointInPolygon(const pt: TPoint64; ops: POutPt): TPointInPolygonResult;
var
  val: Integer;
  d: Double; //used to avoid integer overflow
  ptCurr, ptPrev: POutPt;
begin
  if (ops.Next = ops) or (ops.Next = ops.Prev) then
  begin
    result := pipOutside;
    Exit;
  end;

  Result := pipOn;
  val := 0;
  ptPrev := ops.Prev;
  ptCurr := ops;
  repeat
    if (ptPrev.Pt.Y = ptCurr.Pt.Y) then //a horizontal edge
    begin
      if (pt.Y = ptCurr.Pt.Y) and
        ((pt.X = ptPrev.Pt.X) or (pt.X = ptCurr.Pt.X) or
          ((pt.X < ptPrev.Pt.X) <> (pt.X < ptCurr.Pt.X))) then Exit;
    end
    else if (ptPrev.Pt.Y < ptCurr.Pt.Y) then
    begin
      //nb: only allow one equality with Y to avoid
      //double counting when pt.Y == ptCurr.Pt.Y
      if (pt.Y > ptPrev.Pt.Y) and (pt.Y <= ptCurr.Pt.Y) and
        ((pt.X >= ptPrev.Pt.X) or (pt.X >= ptCurr.Pt.X)) then
      begin
        if((pt.X > ptPrev.Pt.X) and (pt.X > ptCurr.Pt.X)) then
          val := 1 - val //toggles val between 0 and 1
        else
        begin
          d := CrossProduct(ptPrev.Pt, ptCurr.Pt, pt);
          if d = 0 then Exit
          else if d > 0 then val := 1 - val;
        end;
      end;
    end else
    begin
      if (pt.Y > ptCurr.Pt.Y) and (pt.Y <= ptPrev.Pt.Y) and
        ((pt.X >= ptCurr.Pt.X) or (pt.X >= ptPrev.Pt.X)) then
      begin
        if((pt.X > ptPrev.Pt.X) and (pt.X > ptCurr.Pt.X)) then
          val := 1 - val //toggles val between 0 and 1
        else
        begin
          d := CrossProduct(ptCurr.Pt, ptPrev.Pt, pt);
          if d = 0 then Exit
          else if d > 0 then val := 1 - val;
        end;
      end;
    end;
    ptPrev := ptCurr;
    ptCurr := ptCurr.Next;
  until ptCurr = ops;
  if val = 0 then
     result := pipOutside else
     result := pipInside;
end;
//------------------------------------------------------------------------------

function Path1InsidePath2(const op1, op2: POutPt): Boolean;
var
  op: POutPt;
  pipResult: TPointInPolygonResult;
begin
  op := op1;
  repeat
    pipResult := PointInPolygon(op.Pt, op2);
    if pipResult <> pipOn then Break;
    op := op.Next;
  until op = op1;
  Result := pipResult = pipInside;
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
    if FHasOpenPaths then
      setLength(openPaths, FOutRecList.Count);
    cntOpen := 0;
    for i := 0 to FOutRecList.Count -1 do
    begin
      outRec := FOutRecList[i];
      if not assigned(outRec.Pts) then Continue;

      outRec.Owner := GetRealOutRec(outRec.Owner);
      if assigned(outRec.Owner) then
      begin

        if assigned(outRec.Owner.Split) then
        begin
          for j := 0 to High(outRec.Owner.Split) do
            if Assigned(outRec.Owner.Split[j].Pts) and
              Path1InsidePath2(OutRec.Pts,
                outRec.Owner.Split[j].Pts) then
            begin
              outRec.Owner := outRec.Owner.Split[j];
              break;
            end;
        end;

        //swap order if outer/owner paths are preceeded by their inner paths
        if (outRec.Owner.Idx > outRec.Idx) then
        begin
          j := outRec.Owner.Idx;
          outRec.idx := j;
          FOutRecList[i] := FOutRecList[j];
          FOutRecList[j] := outRec;
          outRec := FOutRecList[i];
          outRec.Idx := i;
        end;
      end;

      isOpenPath := IsOpen(outRec);
      if not BuildPath(outRec.Pts,
        FFillRule = frNegative, isOpenPath, path) then
          Continue;

      if isOpenPath then
      begin
        openPaths[cntOpen] := path;
        inc(cntOpen);
        Continue;
      end;

      if assigned(outRec.Owner) and (outRec.Owner.State = outRec.State) then
      begin
        //inner/outer state needs fixing
        while Assigned(outRec.Owner) and
          not Path1InsidePath2(outRec.Pts, outRec.Owner.Pts) do
            outRec.Owner := outRec.Owner.Owner;

        if not Assigned(outRec.Owner) or IsInner(outRec.Owner) then
          outRec.State := osOuter else
          outRec.State := osInner;
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
  Result := true;
  closedSolutions := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildPaths(closedSolutions, dummy);
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
    ClearSolution;
  end;
end;
//------------------------------------------------------------------------------

function TClipper64.Execute(clipType: TClipType; fillRule: TFillRule;
  var solutionTree: TPolyTree; out openSolutions: TPaths64): Boolean;
begin
  if not assigned(solutionTree) then
    Raise EClipperLibException(rsClipper_PolyTreeErr);
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
    ClearSolution;
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

constructor TClipperD.Create(roundingDecimalPrecision: integer = 2);
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

  solutionsTree.Clear;
  solutionsTree.SetScale(FScale);
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
    ClearSolution;
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

