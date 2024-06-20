unit Clipper.DLL.Data;
(* ******************************************************************************
 * Author    :  Jasper Schellingerhout                                         *
 * Date      :  12 June 2024                                                   *
 * License   :  http://www.boost.org/LICENSE_1_0.txt                           *
 **************************************************************************** *)

interface

{$I Clipper.DLL.inc}

type

  TClipperData<T: record> = Record // Typespace record
  const
    Dimensionality = {$IFDEF CLIPPER_VERTEX_HAS_Z} 3 {$ELSE} 2 {$ENDIF};

  type
    // Vertex
    PVertex = ^TVertex;

    TVertex = Record // Make sure size is Dimensionality * ElementSize
    public
      procedure SetValues(const X, Y: T);
    public
      X, Y: T;

    {$IFDEF CLIPPER_VERTEX_HAS_Z}   // Z can be utilized as data or Z coordinate (pointers are NativeUInt size)
      function Has(ThisType: TClass): Boolean;
      function DataOfType<Q: Class>: Q;

    case integer of
      0: (Data: TObject); // make sure Pointer Size <= SizeOf T
      1: (Z: T);
    {$ENDIF}

    end;

    // Rect
    PRect = ^TRect;

    TRect = record
      left,top,right, bottom: T;
      constructor Create(left,top,right, bottom: T);
    end;

    // PolyPath and Path and path have the same memory structure, except path has
    // _ChildCount always as 0
    PPath = ^TPath;

    TPath = Record
    private
      _VertexCount: T;
      _ChildCount: T; // number of nested Paths in case it is actually a polypath

      VertexData: Array [0 .. 0] of TVertex;
      // followed at unknown memory location by Array of Child Paths (we can get first element with FirstChild)

      function GetChildCount: int64;
      function GetVertexCount: int64;
      procedure SetVertexCount(const Value: int64);
      procedure SetChildCount(const Value: int64); // Pointer after the vertices

    public
      class function Create(NumberOfVertices: integer): PPath; static;
      procedure Free;

      function FirstChild: PPath;  // depth first tree data structure

      function GetVertex(index: integer): PVertex;

      property VertexCount: int64 read GetVertexCount write SetVertexCount;
      property ChildCount: int64 read GetChildCount write SetChildCount;
      property Vertex[index: integer]: PVertex read GetVertex; // get past range checks
    End;



    // Paths
    PPaths = ^TPaths;

    TPaths = Record
    private
      _ElementCount: T; // Sum of all TPath Element counts (each = VertexCount + HeaderElementCount) + 2 (for the TPaths header)
      _PathCount: T;
      PathData: Array [0 .. 0] of TPath;  // don't index beyond [0], TPath size is variable length structure

      function GetCount: int64;
      procedure SetCount(const Value: int64);

      function GetElementCount: int64;
      procedure SetElementCount(const Value: int64);

    public
      class function Create(const NumberOfVerticesPerPath: TArray<Integer> ): PPaths; overload; static;
      class function Create(const ElementCount: integer): PPaths; overload; static;
      class function CalculateElementCount(NumberOfPaths, NumberOfVertices: integer): integer; static;
      procedure Free;

      function FirstPath: PPath;
      function NextPath(Path: PPath): PPath;
      function GetPath(index: integer): PPath;
      property Count: int64 read GetCount write SetCount;
      property Path[index: integer]: PPath read GetPath;
      property ElementCount: int64 read GetElementCount write SetElementCount;
    End;



    // helper  for Path and PolyTree navigation
    PTreeNode = ^TTreeNode;

    TTreeNode = Record
      Path: PPath;
      Parent: PTreeNode;
      Children: TArray<PTreeNode>;
    End;

    TTreeRoot = Record
      NodeStore: TArray<TTreeNode>; // memory container
      Children: TArray<PTreeNode>;
    End;

    // TPolyTree has identical structure to TPaths, but the assumption is that
    // TPaths will not contain paths with children (i.e. no PolyPaths).
    // TPolyTree is exclusively used for output and the separation of
    // TPaths and TPolyTree also allows distinction when processing input and
    // output from the Clipper DLL so processing can be optimized for flat or
    // nested structures

    PPolyTree = ^TPolyTree;

    TPolyTree = Record
    private
      _Data : TPaths; // Delphi does not allow record inheritance, but we can compose

      function GetChildCount: int64; inline;
      function GetElementCount: int64; inline;

      class function LinkRecursively(Parent: PTreeNode; var ItPath: PPath; var NodeIdx: integer;
        const Nodes: TArray<TTreeNode>): PTreeNode; static;

    public
      function Root: TTreeRoot; // constructor of a navigable nested pointer structure

      function FirstPath: PPath; inline;
      function NextPath(Path: PPath): PPath; inline; // traverses in memory order (depth first)

      property ElementCount: int64 read GetElementCount;
      property ChildCount: int64 read GetChildCount;
    End;

    {$IFDEF CLIPPER_POINTER_WRAPPERS_ASSERTS}
    class procedure AssertSizesAreCorrect(); static;
    {$ENDIF}
  End;

  TClipperData64 = TClipperData<Int64>;
  TPolyTree64 = TClipperData64.TPolyTree;
  PVertex64 = TClipperData64.PVertex;
  TVertex64 =  TClipperData64.TVertex;
  TPaths64 = TClipperData64.TPaths;
  TPath64 = TClipperData64.TPath;
  TRect64 = TClipperData64.TRect;
  CPaths64 = TClipperData64.PPaths;
  CPath64 = TClipperData64.PPath;
  CPolyTree64 = TClipperData64.PPolyTree;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  TClipperDataD = TClipperData<Double>;
  TPolyTreeD = TClipperDataD.TPolyTree;
  PVertexD = TClipperDataD.PVertex;
  TVertexD =  TClipperDataD.TVertex;
  TPathsD = TClipperDataD.TPaths;
  TPathD = TClipperDataD.TPath;
  TRectD = TClipperDataD.TRect;
  CPathsD = TClipperDataD.PPaths;
  CPathD = TClipperDataD.PPath;
  CPolyTreeD = TClipperDataD.PPolyTree;
{$ENDIF}



  TPath64Helper = Record helper for TPath64
    function GetBounds: TRect64;
    procedure ExpandBounds(var Bounds: TRect64);
  End;


   TPaths64Helper = Record helper for TPaths64
    function GetBounds: TRect64;
    procedure ExpandBounds(var Bounds: TRect64); overload;
  End;

   TPolyTree64Helper = Record helper for TPolyTree64
    procedure ExpandBounds(var Bounds: TRect64);
    function GetBounds: TRect64;
   End;


{$IFDEF CLIPPER_DOUBLE_COORDINATES}


  TRectDHelper = Record helper for TRectD
    function IsEmpty: boolean;
  End;

  TPathDHelper = Record helper for TPathD
    function GetBounds: TRectD;
    procedure ExpandBounds(var Bounds: TRectD);
  End;

  TPathsDHelper = Record helper for TPathsD
    procedure ExpandBounds(var Bounds: TRectD);
    function GetBounds: TRectD;
  End;

   TPolyTreeDHelper = Record helper for TPolyTreeD
    procedure ExpandBounds(var Bounds: TRectD);
    function GetBounds: TRectD;
   End;

{$ENDIF}

implementation

//uses
//  System.Math;



{ TClipperData<T>.TPath }
procedure TClipperData<T>.TPath.Free;
begin
  if @self <> nil then
    FreeMem(@self);
end;



function TClipperData<T>.TPath.GetVertex(index: integer): PVertex;
begin
{$RANGECHECKS OFF}
  result := @VertexData[index];
{$RANGECHECKS ON}
end;


class function TClipperData<T>.TPath.Create(NumberOfVertices: integer): PPath;
const
  HeaderSize = 2 * SizeOf(T); // there are two header elements: _VertexCount: T,  _Reserved: T;
begin
  GetMem(result, HeaderSize + NumberOfVertices * SizeOf(TVertex));
  result.VertexCount := NumberOfVertices;
  result._ChildCount := Default (T); // should zero out memory
end;

procedure TClipperData<T>.TPath.SetChildCount(const Value: int64);
begin
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  if GetTypeKind(T) = tkFloat then    //today we only support Double and Int64
    PDouble(@_ChildCount)^ := value
  else
{$ENDIF}
    PInt64(@_ChildCount)^ := value;
end;

procedure TClipperData<T>.TPath.SetVertexCount(const Value: int64);
begin
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  if GetTypeKind(T) = tkFloat then    //today we only support Double and Int64
    PDouble(@_VertexCount)^ := value
  else
{$ENDIF}
    PInt64(@_VertexCount)^ := value;
end;

{ TClipperData<T>.TPaths }

function TClipperData<T>.TPaths.FirstPath: PPath;
begin
  if Count = 0 then
    exit(nil);

  result := @PathData;
end;

procedure TClipperData<T>.TPaths.Free;
begin
  if @self <> nil then
    FreeMem(@self);
end;

function TClipperData<T>.TPaths.GetCount: int64;
begin
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  if GetTypeKind(T) = tkFloat then    //today we only support Double and Int64
    result := trunc(PDouble(@_PathCount)^)
  else
{$ENDIF}
    result := PInt64(@_PathCount)^;
end;

function TClipperData<T>.TPaths.GetElementCount: int64;
begin
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  if GetTypeKind(T) = tkFloat then    //today we only support Double and Int64
    result := trunc(PDouble(@_ElementCount)^)
  else
{$ENDIF}
    result := PInt64(@_ElementCount)^;
 end;

function TClipperData<T>.TPaths.GetPath(index: integer): PPath;
var
  i: integer;
  It: PPath;
  ItVert: PVertex;
begin

  It := @PathData;

  for i := 1 to Index do
  begin
    ItVert := @It.VertexData[0];
    Inc(ItVert, It.VertexCount);
    It := PPath(ItVert);
  end;

  result := It;

end;

function TClipperData<T>.TPaths.NextPath(Path: PPath): PPath;
var
  ItVert: PVertex;
begin
  if Path = nil then
    exit(FirstPath);

  ItVert := @Path.VertexData[0];
  Inc(ItVert, Path.VertexCount);
  result := PPath(ItVert);


  if NativeUInt(result) >= (NativeUInt(@self) + NativeUInt(ElementCount * SizeOf(T))) then
    result := nil;
end;


class function TClipperData<T>.TPaths.Create(const ElementCount: integer): PPaths;
begin
  result := AllocMem(ElementCount * SizeOf(T));
  result.ElementCount := ElementCount;
end;


class function  TClipperData<T>.TPaths.CalculateElementCount(NumberOfPaths, NumberOfVertices: integer): integer;
Const
  PathHeaderElementCount = 2;  // _VertexCount: T, _Reserved: T;
  PathsHeaderElementCount = 2; // _ElementCount: T; _PathCount: T;
begin
   result := PathsHeaderElementCount +  // paths header
             PathHeaderElementCount * NumberOfPaths + // each path's header
             NumberOfVertices * Dimensionality;   // elements for each vertex
end;


class function TClipperData<T>.TPaths.Create(const NumberOfVerticesPerPath: TArray<Integer>): PPaths;
var
  i, VertexCount: integer;
  Size: integer;
  It: PPath;
  ItVert: PVertex;
  ElementCount: int64;
begin
  VertexCount := 0;
  for i := 0 to High(NumberOfVerticesPerPath) do
    Inc(VertexCount, NumberOfVerticesPerPath[i]);

  ElementCount := CalculateElementCount(Length(NumberOfVerticesPerPath), VertexCount);
 
  result := Create(ElementCount);
  result.Count := Length(NumberOfVerticesPerPath);

  It := @result.PathData;
  for i := 0 to High(NumberOfVerticesPerPath) do
  begin
    It.VertexCount := NumberOfVerticesPerPath[i];
    It._ChildCount := Default (T); // should zero out

    ItVert := @It.VertexData[0];
    Inc(ItVert, It.VertexCount);

    It := PPath(ItVert);
  end;
end;

procedure TClipperData<T>.TPaths.SetCount(const Value: int64);
begin
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  if GetTypeKind(T) = tkFloat then    //today we only support Double and Int64
    PDouble(@_PathCount)^ := Value
  else
{$ENDIF}
    PInt64(@_PathCount)^ := Value;
end;

procedure TClipperData<T>.TPaths.SetElementCount(const Value: int64);
begin
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  if GetTypeKind(T) = tkFloat then    //today we only support Double and Int64
    PDouble(@_ElementCount)^ := Value
  else
{$ENDIF}
    PInt64(@_ElementCount)^ := Value;

end;

{ TClipperData<T>.TPolyPath }

function TClipperData<T>.TPath.FirstChild: PPath;
var
  ItVert: PVertex;
begin
  if GetChildCount() = 0 then
    exit(nil);

  ItVert := @VertexData[0];
  Inc(ItVert, VertexCount);
  result := PPath(ItVert); //no longer a pointer to vertex
end;

function TClipperData<T>.TPath.GetChildCount: int64;
begin
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  if GetTypeKind(T) = tkFloat then    //today we only support Double and Int64
    result := trunc(PDouble(@_ChildCount)^)
  else
{$ENDIF}
    result := PInt64(@_ChildCount)^;
end;


function TClipperData<T>.TPath.GetVertexCount: int64;             //refactor
begin
{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  if GetTypeKind(T) = tkFloat then    //today we only support Double and Int64
    result := trunc(PDouble(@_VertexCount)^)
  else
{$ENDIF}
    result := PInt64(@_VertexCount)^;
end;


{ TClipperData<T>.TPolyTree }

function TClipperData<T>.TPolyTree.FirstPath: PPath;
begin
  result := _Data.FirstPath;
end;

function TClipperData<T>.TPolyTree.GetChildCount: int64;
begin
 result := _Data.Count;
end;

function TClipperData<T>.TPolyTree.GetElementCount: int64;
begin
  result := _Data.ElementCount;
end;

class function TClipperData<T>.TPolyTree.LinkRecursively(Parent: PTreeNode; var ItPath: PPath; var NodeIdx: integer;
  const Nodes: TArray<TTreeNode>): PTreeNode;
var
  i: integer;
  ItVert: PVertex;
begin
  result := @Nodes[NodeIdx];
  result.Path := ItPath;
  result.Parent := Parent;

  Inc(NodeIdx);

  ItVert := @result.Path.VertexData[0];
  Inc(ItVert, result.Path.VertexCount);
  itPath := PPath(ItVert);  //either the first child or the next vertex

  SetLength(result.Children, result.Path.GetChildCount);
  for i := 0 to High(result.Children) do
    result.Children[i] := LinkRecursively(result, ItPath, NodeIdx, Nodes);

end;

function TClipperData<T>.TPolyTree.NextPath(Path: PPath): PPath;
begin
  result := _Data.NextPath(Path);
end;

function TClipperData<T>.TPolyTree.Root: TTreeRoot;

var
  ItVert: PVertex;
  ItEnd: Pointer;
  ItPath: PPath;
  NodeCount: integer;
  i, NodeIdx: integer;
begin

  if ChildCount = 0 then
    exit;

  NodeCount := 0;
{$POINTERMATH ON}
  ItEnd := @PInt64(@Self)[ElementCount];
{$POINTERMATH OFF}
  ItPath := @_Data.PathData[0];
  repeat
    ItVert := @ItPath.VertexData[0];
    Inc(ItVert, ItPath.VertexCount);
    Inc(NodeCount);
    ItPath := PPath(ItVert);
  until ItPath = ItEnd;

  SetLength(result.NodeStore, NodeCount);

  NodeIdx := 0;
  ItPath := @_Data.PathData[0];
  SetLength(result.Children, ChildCount);

  for i := 0 to ChildCount - 1 do
    result.Children[i] := LinkRecursively(nil, ItPath, NodeIdx, result.NodeStore);
end;

{ TClipperData<T> }

{$IFDEF CLIPPER_POINTER_WRAPPERS_ASSERTS}
class procedure TClipperData<T>.AssertSizesAreCorrect;
begin
  Assert(SizeOf(TVertex) = Dimensionality * SizeOf(T), 'Vertex size should be equal to dimensionality * size of array element');
  Assert(SizeOf(Int64) <= SizeOf(T), 'Z Component on Clipper DLL side is Int64 and must fit in an array element');
end;
{$ENDIF}

{ TClipperData<T>.TVertex }

{$IFDEF CLIPPER_VERTEX_HAS_Z}
function TClipperData<T>.TVertex.DataOfType<Q>: Q;
begin
  if Data is Q then
    result := Q(Data)
  else
    result := nil;
end;

function TClipperData<T>.TVertex.Has(ThisType: TClass): Boolean;
begin
  result :=  Data is ThisType;
end;
{$ENDIF}

procedure TClipperData<T>.TVertex.SetValues(const X, Y: T);
begin
  self.X := X;
  Self.Y := Y;
end;

{ TClipperData<T>.TRect }

constructor TClipperData<T>.TRect.Create(left, top, right, bottom: T);
begin
  self.left := left;
  self.top := top;
  self.right := right;
  self.bottom := bottom;

end;

{ TPath64Helper }

procedure TPath64Helper.ExpandBounds(var Bounds: TRect64);
var
  i: integer;
  ItVert: TClipperData64.PVertex;
begin

  ItVert := @VertexData[0];

  for i := 0 to VertexCount-1 do
  begin
     if itVert.X < Bounds.Left then
        Bounds.Left := itVert.X;

     if itVert.X > Bounds.Right then
        Bounds.Right := itVert.X;

     // Y is down positive
     if itVert.Y < Bounds.Top then
        Bounds.Top := itVert.Y;

     if itVert.Y > Bounds.Bottom then
        Bounds.Bottom := itVert.Y;

     Inc(itVert);
  end;

end;

function TPath64Helper.GetBounds: TRect64;
var
  ItVert: TClipperData64.PVertex;
begin
  ItVert := @VertexData[0];
  result := TRect64.Create(ItVert.X, ItVert.Y, ItVert.X, itVert.Y);
  ExpandBounds(result);
end;


procedure TPaths64Helper.ExpandBounds(var Bounds: TRect64);
var
  Path: TClipperData64.PPath;
begin
  Path := FirstPath();
  while Path <> nil do
  begin
    Path.ExpandBounds(Bounds);
    Path := NextPath(Path);
  end;
end;



function TPaths64Helper.GetBounds: TRect64;
var
  ItVert: TClipperData64.PVertex;
begin
  ItVert := @Path[0].VertexData[0];
  result := TRect64.Create(ItVert.X, ItVert.Y, ItVert.X, itVert.Y);

  ExpandBounds(result);
end;

{ TPolyTree64Helper }

procedure TPolyTree64Helper.ExpandBounds(var Bounds: TRect64);
begin
  _Data.ExpandBounds(Bounds);
end;


function TPolyTree64Helper.GetBounds: TRect64;
begin
  result := _Data.GetBounds;
end;



{ TPathDHelper }
{$IFDEF CLIPPER_DOUBLE_COORDINATES}

procedure TPathDHelper.ExpandBounds(var Bounds: TRectD);
var
  i: integer;
  ItVert: TClipperDataD.PVertex;
begin

  ItVert := @VertexData[0];

  for i := 0 to VertexCount-1 do
  begin
     if itVert.X < Bounds.Left then
        Bounds.Left := itVert.X;

     if itVert.X > Bounds.Right then
        Bounds.Right := itVert.X;

     // Y is down positive
     if itVert.Y < Bounds.Top then
        Bounds.Top := itVert.Y;

     if itVert.Y > Bounds.Bottom then
        Bounds.Bottom := itVert.Y;

     Inc(itVert);
  end;

end;


function TPathDHelper.GetBounds: TRectD;
var
  ItVert: TClipperDataD.PVertex;
begin
  ItVert := @VertexData[0];
  result := TRectD.Create(ItVert.X, ItVert.Y, ItVert.X, itVert.Y);
  ExpandBounds(result);
end;

procedure TPathsDHelper.ExpandBounds(var Bounds: TRectD);
var
  i: integer;
  It: TClipperDataD.PPath;
  ItVert: TClipperDataD.PVertex;
begin

  It := @PathData;
  It.ExpandBounds(Bounds);

  for i := 1 to count-1 do
  begin
    ItVert := @It.VertexData[0];
    Inc(ItVert, It.VertexCount);
    It := TClipperDataD.PPath(ItVert);
    It.ExpandBounds(Bounds);
  end;
end;


function TPathsDHelper.GetBounds: TRectD;
var
  ItVert: TClipperDataD.PVertex;
begin
  ItVert := @Path[0].VertexData[0];
  result := TRectD.Create(ItVert.X, ItVert.Y, ItVert.X, itVert.Y);

  ExpandBounds(result);
end;


{ TRectDHelper }

function TRectDHelper.IsEmpty: boolean;
begin
  result := (bottom <= top) or (right <= left); // y axis down
end;


{ TPolyTreeDHelper }

procedure TPolyTreeDHelper.ExpandBounds(var Bounds: TRectD);
var
  Path: TClipperDataD.PPath;
begin
  Path := FirstPath();
  while Path <> nil do
  begin
    Path.ExpandBounds(Bounds);
    Path := NextPath(Path);
  end;
end;

function TPolyTreeDHelper.GetBounds: TRectD;
var
  ItVert: TClipperDataD.PVertex;
begin
  if ChildCount = 0 then
    exit;

  itVert := @Children[0].VertexData[0];
  result := TRectD.Create(ItVert.X, ItVert.Y, ItVert.X, itVert.Y);

  ExpandBounds(result);
end;

{$ENDIF}
end.
