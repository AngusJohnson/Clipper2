unit Clipper.DLL;
(* ******************************************************************************
 * Author    :  Jasper Schellingerhout                                         *
 * Date      :  12 June 2024                                                   *
 * License   :  http://www.boost.org/LICENSE_1_0.txt                           *
 **************************************************************************** *)

{$I Clipper.DLL.inc}   // CLIPPER_USINGZ and CLIPPER_USE_POINTER_WRAPPERS

interface
uses
{$IFDEF CLIPPER_USE_POINTER_WRAPPERS}
  Clipper.DLL.Data,
{$ENDIF}
  Clipper.Dll.Enums;

type

{$IFDEF CLIPPER_USE_POINTER_WRAPPERS}
// Enumerated types
  TFillRule = Clipper.Dll.Enums.TFillRule;
	TClipType = Clipper.Dll.Enums.TClipType;
  TJoinType = Clipper.Dll.Enums.TJoinType;
  TEndType = Clipper.Dll.Enums.TEndType;

// Structures
  PVertex64 = Clipper.Dll.Data.PVertex64;
  TVertex64 = Clipper.Dll.Data.TVertex64;
  TRect64 = Clipper.Dll.Data.TRect64;


// Pointer Wrappers
  CPath64 = Clipper.Dll.Data.CPath64;
  CPaths64 = Clipper.Dll.Data.CPaths64;
  CPolyTree64 = Clipper.Dll.Data.CPolyTree64;


// Pointer mapped structures
  TPaths64 = Clipper.Dll.Data.TPaths64;

// Navigation helper
  TTreeRoot64 = Clipper.DLL.Data.TClipperData64.TTreeRoot;
  TTreeNode64 = Clipper.DLL.Data.TClipperData64.TTreeNode;
  PTreeNode64 = Clipper.DLL.Data.TClipperData64.PTreeNode;

  {$IFDEF CLIPPER_DOUBLE_COORDINATES}
  PVertexD = Clipper.Dll.Data.PVertexD;
  TVertexD = Clipper.Dll.Data.TVertexD;
  TRectD = Clipper.Dll.Data.TRectD;

  CPathD = Clipper.Dll.Data.CPathD;
  CPathsD = Clipper.Dll.Data.CPathsD;
  CPolyTreeD = Clipper.Dll.Data.CPolyTreeD;

  TPathsD = Clipper.Dll.Data.TPathsD;

  TTreeRootD = Clipper.DLL.Data.TClipperDataD.TTreeRoot;
  TTreeNodeD = Clipper.DLL.Data.TClipperDataD.TTreeNode;
  PTreeNodeD = Clipper.DLL.Data.TClipperDataD.PTreeNode;

  {$ENDIF}

{$ELSE}
  TVertex64 =  Record
    public
      X, Y: Int64;
    {$IFDEF CLIPPER_VERTEX_HAS_Z}   // Z can be utilized as data or Z coordinate (pointers are NativeUInt size)
    case integer of
      0: (Data: Pointer); // Pointer Size <= SizeOf Int64
      1: (Z: Int64);
    {$ENDIF}
    end;


  CPath64 = PInt64;
  CPaths64 = PInt64;
  CPolyTree64 = PInt64;

  TRect64 = record
      left,top,right, bottom: Int64;
  end;

 {$IFDEF CLIPPER_DOUBLE_COORDINATES}
  CPathD = PDouble;
  CPathsD = PDouble;
  CPolyTreeD = PDouble;
  TRectD = record
      left,top,right, bottom: Double;
  end;

  TVertexD =  Record
    public
      X, Y: Double;
    {$IFDEF CLIPPER_VERTEX_HAS_Z}   // Z can be utilized as data or Z coordinate (pointers are NativeUInt size)
    case integer of
      0: (Data: Pointer); // Pointer Size <= SizeOf Double
      1: (Z: Double);
    {$ENDIF}
  end;
  {$ENDIF}

{$ENDIF}

{$IFDEF DEBUG}
  // During debug view a block of memory during debug behind a pointer such as
  // CPath64, CPaths64, CPolyPath64 or CPolyTree64:
  //
  //      PDebugInt64Array(Paths)^
  //
  //  To view data declared with dummy range [0..0] and avoid range check errors
  //
  //     PDebugInt64Array(@Path.VertexData[0])^

  PDebugInt64Array = ^TDebugInt64Array;
  TDebugInt64Array = Array[0..255] of Int64;

  PDebugDoubleArray = ^TDebugDoubleArray;
  TDebugDoubleArray = Array[0..255] of Double;
{$ENDIF}


const
  ClipperCpp_DLL = 'Clipper2_64.dll';


//EXTERN_DLL_EXPORT const char* Version();
 function Version: PAnsiChar; cdecl; external ClipperCpp_DLL;

//
//EXTERN_DLL_EXPORT void DisposeArray64(int64_t*& p)
 procedure DisposeArray64(var p: PInt64);  cdecl; external ClipperCpp_DLL;

// Call these for return values... not for ones we generate (type friendly aliases for DisposeArray64)
 procedure DisposeExternalCPaths64(var p: CPaths64); inline;
 procedure DisposeExternalCPolyTree64(var p: CPolyTree64); inline;


//EXTERN_DLL_EXPORT int BooleanOp64(uint8_t cliptype,
//  uint8_t fillrule, const CPaths64 subjects,
//  const CPaths64 subjects_open, const CPaths64 clips,
//  CPaths64& solution, CPaths64& solution_open,
//  bool preserve_collinear = true, bool reverse_solution = false);
function BooleanOp64(cliptype: TClipType;
   fillrule: TFillRule; const subjects: CPaths64;
   const subjects_open: CPaths64;  const clips: CPaths64;
   var solution: CPaths64; var solution_open: CPaths64;
   preserve_collinear: boolean = true; reverse_solution: boolean  = false): integer;   cdecl; external ClipperCpp_DLL;

//EXTERN_DLL_EXPORT int BooleanOp_PolyTree64(uint8_t cliptype,
//  uint8_t fillrule, const CPaths64 subjects,
//  const CPaths64 subjects_open, const CPaths64 clips,
//  CPolyTree64& sol_tree, CPaths64& solution_open,
//  bool preserve_collinear = true, bool reverse_solution = false);
function BooleanOp_PolyTree64(cliptype: TClipType;
  fillrule: TFillRule; const subjects: CPaths64;
  const subjects_open: CPaths64; const clips: CPaths64;
  var sol_tree: CPolyTree64; var solution_open: CPaths64;
  preserve_collinear: boolean = true; reverse_solution: boolean = false) : integer; cdecl; external ClipperCpp_DLL;


//EXTERN_DLL_EXPORT CPaths64 InflatePaths64(const CPaths64 paths,
//  double delta, uint8_t jointype, uint8_t endtype,
//  double miter_limit = 2.0, double arc_tolerance = 0.0,
//  bool reverse_solution = false);
function InflatePaths64(const paths: CPaths64;
  delta: double; jointype: TJoinType;  endtype: TEndType;
  miter_limit: double  = 2.0; arc_tolerance: double  = 0.0;
  reverse_solution: boolean = false): CPaths64; cdecl; external ClipperCpp_DLL;



//EXTERN_DLL_EXPORT CPaths64 InflatePath64(const CPath64 path,
//  double delta, uint8_t jointype, uint8_t endtype,
//  double miter_limit = 2.0, double arc_tolerance = 0.0,
//  bool reverse_solution = false);
function InflatePath64(const path: CPath64;
  delta: double; jointype: TJoinType;  endtype: TEndType;
  miter_limit: double  = 2.0; arc_tolerance: double  = 0.0;
  reverse_solution: boolean = false): CPaths64; cdecl; external ClipperCpp_DLL;


//// RectClip & RectClipLines:
//EXTERN_DLL_EXPORT CPaths64 RectClip64(const TRect64& rect,
//  const CPaths64 paths);
function RectClip64(const [ref] rect: TRect64;
  const paths: CPaths64): CPaths64;  cdecl; external ClipperCpp_DLL;


//EXTERN_DLL_EXPORT CPaths64 RectClipLines64(const TRect64& rect,
//  const CPaths64 paths);
function RectClipLines64(const [ref] rect: TRect64;
 const paths: CPaths64): CPaths64; cdecl; external ClipperCpp_DLL;



//EXTERN_DLL_EXPORT CPaths64 MinkowskiSum64(const CPath64& cpattern, const CPath64& cpath, bool is_closed)
function MinkowskiSum64(const [ref] cpattern: CPath64; const [ref] cpath: CPath64; is_closed: boolean ): CPaths64; cdecl; external ClipperCpp_DLL;

//EXTERN_DLL_EXPORT CPaths64 MinkowskiDiff64(const CPath64& cpattern, const CPath64& cpath, bool is_closed)
function MinkowskiDiff64(const [ref] cpattern: CPath64; const [ref] cpath: CPath64; is_closed: boolean): CPaths64; cdecl; external ClipperCpp_DLL;

{$IFDEF CLIPPER_VERTEX_HAS_Z}
type
  TZCallback64 = procedure(const [ref] e1bot, e1top, e2bot, e2top: TVertex64; var pt: TVertex64); cdecl;

procedure SetDefaultZCallback(callback: TZCallback64); cdecl; external ClipperCpp_DLL;
{$ENDIF}

 {$IFDEF CLIPPER_DOUBLE_COORDINATES}
//
//EXTERN_DLL_EXPORT void DisposeArrayD(double*& p)
 procedure DisposeArrayD(var p: pDouble);  cdecl; external ClipperCpp_DLL;

// Call these for return values... not for ones we generate  (type friendly aliases for DisposeArrayD)
 procedure DisposeExternalCPathsD(var p: CPathsD); inline;
 procedure DisposeExternalCPolyTreeD(var p: CPolyTreeD);  inline;

 //
//EXTERN_DLL_EXPORT int BooleanOpD(uint8_t cliptype,
//  uint8_t fillrule, const CPathsD subjects,
//  const CPathsD subjects_open, const CPathsD clips,
//  CPathsD& solution, CPathsD& solution_open, int precision = 2,
//  bool preserve_collinear = true, bool reverse_solution = false);

function BooleanOpD(cliptype: TClipType;
  fillrule: TFillRule; const subjects: CPathsD;
  const subjects_open: CPathsD; const clips: CPathsD;
  var solution: CPathsD; var solution_open: CPathsD; precision: integer = 2;
  preserve_collinear: boolean = true; reverse_solution: boolean = false) : integer; cdecl; external ClipperCpp_DLL;

//
//EXTERN_DLL_EXPORT int BooleanOp_PolyTreeD(uint8_t cliptype,
//  uint8_t fillrule, const CPathsD subjects,
//  const CPathsD subjects_open, const CPathsD clips,
//  CPolyTreeD& solution, CPathsD& solution_open, int precision = 2,
//  bool preserve_collinear = true, bool reverse_solution = false);
//

function BooleanOp_PolyTreeD(cliptype: TClipType;
  fillrule: TFillRule; const subjects: CPathsD;
  const subjects_open: CPathsD; const clips: CPathsD;
  var solution: CPolyTreeD; var solution_open: CPathsD; precision: integer = 2;
  preserve_collinear: boolean = true; reverse_solution: boolean = false) : integer; cdecl; external ClipperCpp_DLL;

//EXTERN_DLL_EXPORT CPathsD InflatePathsD(const CPathsD paths,
//  double delta, uint8_t jointype, uint8_t endtype,
//  int precision = 2, double miter_limit = 2.0,
//  double arc_tolerance = 0.0, bool reverse_solution = false);
//

function InflatePathsD(const paths: CPathsD;
  delta: double; jointype: TJoinType;  endtype: TEndType;
  precision: integer = 2; miter_limit: double  = 2.0;
  arc_tolerance: double  = 0.0; reverse_solution: boolean = false): CPathsD; cdecl; external ClipperCpp_DLL;

//EXTERN_DLL_EXPORT CPathsD InflatePathD(const CPathD path,
//  double delta, uint8_t jointype, uint8_t endtype,
//  int precision = 2, double miter_limit = 2.0,
//  double arc_tolerance = 0.0, bool reverse_solution = false);
//

function InflatePathD(const paths: CPathD;
  delta: double; jointype: TJoinType;  endtype: TEndType;
  precision: integer = 2; miter_limit: double  = 2.0;
  arc_tolerance: double  = 0.0; reverse_solution: boolean = false): CPathsD; cdecl; external ClipperCpp_DLL;

//EXTERN_DLL_EXPORT CPathsD RectClipD(const TRectD& rect,
//  const CPathsD paths, int precision = 2);

function RectClipD(const [ref] rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPathsD; cdecl; external ClipperCpp_DLL;

//EXTERN_DLL_EXPORT CPathsD RectClipLinesD(const TRectD& rect,
//  const CPathsD paths, int precision = 2);

function RectClipLinesD(const [ref] rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPaths64; cdecl; external ClipperCpp_DLL;

 {$ENDIF}
implementation


procedure DisposeExternalCPaths64(var p: CPaths64);
begin
  DisposeArray64(PInt64(p));
end;

procedure DisposeExternalCPolyTree64(var p: CPolyTree64);
begin
  DisposeArray64(PInt64(p));
end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure DisposeExternalCPathsD(var p: CPathsD);
begin
  DisposeArrayD(PDouble(p));
end;

procedure DisposeExternalCPolyTreeD(var p: CPolyTreeD);
begin
  DisposeArrayD(PDouble(p));
end;

{$ENDIF}


{$IFDEF CLIPPER_POINTER_WRAPPERS_ASSERTS}
initialization
  TClipperData64.AssertSizesAreCorrect;
 {$IFDEF CLIPPER_DOUBLE_COORDINATES}
  TClipperDataD.AssertSizesAreCorrect;
 {$ENDIF}
finalization
{$ENDIF}

end.
