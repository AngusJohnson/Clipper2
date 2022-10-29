program Test_DLL;

// Make sure that the Clipper2 DLLS are in either
// the OS Path or in the application's folder.

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellApi,
  SysUtils,
{$IFDEF USING_CLIPPER2_SRC}
  Clipper.Core in '..\..\Delphi\Clipper2Lib\Clipper.Core.pas',
  Clipper.Engine in '..\..\Delphi\Clipper2Lib\Clipper.Engine.pas',
  Clipper in '..\..\Delphi\Clipper2Lib\Clipper.pas',
{$ENDIF}
  SvgWriter in 'SvgWriter.pas',
  Timer in 'Timer.pas';
type

  ////////////////////////////////////////////////////////
  // Clipper2 DLL structures
  ////////////////////////////////////////////////////////

{$IFNDEF USING_CLIPPER2_SRC}
  TClipType = (ctNone, ctIntersection, ctUnion, ctDifference, ctXor);
  TJoinType = (jtSquare, jtRound, jtMiter);
  TEndType  = (etPolygon, etJoined, etButt, etSquare, etRound);
  //TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative); // see SvgWriter

//  TPoint64 = record X,Y: Int64; end;    // see SvgWriter
//  TPath64 = array of TPoint64;          // see SvgWriter
//  TPaths64 = array of TPath64;          // see SvgWriter
//  TRect64 = record l,t,r,b: Int64; end; // see SvgWriter
{$ENDIF}

  CInt64arr = array[0..$FFFF] of Int64;
  CPath64 = ^CInt64arr;
  CPath64arr = array[0..$FFFF] of CPath64;
  CPaths64 = ^CPath64arr;

//  TPointD = record X,Y: double; end;    // see SvgWriter
//  TPathD = array of TPointD;            // see SvgWriter
//  TPathsD = array of TPathD;            // see SvgWriter
//  TRectD = record l,t,r,b: double; end; // see SvgWriter

  CDblarr = array[0..$FFFF] of Double;
  CPathD = ^CDblarr;
  CPathDarr = array[0..$FFFF] of CPathD;
  CPathsD = ^CPathDarr;


  // nb: Pointer sizes could be 32bit or 64 bits
  // and this will depend on how the DLL was compiled
  // Obviously, DLLs compiled for 64bits won't work with
  // applications compiled for 32bits (and vice versa).

  PCPolyTree64 = ^CPolyTree64;
  CPolyTree64 = packed record
    polygon   : CPath64;        //pointer (32bit or 64bit)
    isHole    : LongBool;       //32 bits
    childCnt  : Int32;          //32 bits
    childs    : PCPolyTree64;   //pointer (32bit or 64bit)
  end;

  PCPolyTreeD = ^CPolyTreeD;
  CPolyTreeD = packed record
    polygon   : CPathD;         //pointer (32bit or 64bit)
    isHole    : LongBool;       //32 bits
    childCnt  : Int32;          //32 bits
    childs    : PCPolyTreeD;    //pointer (32bit or 64bit)
  end;

const
{$IFDEF WIN64}
  CLIPPER2_DLL = 'Clipper2_64.dll';
{$ELSE}
  CLIPPER2_DLL = 'Clipper2_32.dll';
{$ENDIF}

////////////////////////////////////////////////////////
// Clipper2 DLL functions
////////////////////////////////////////////////////////

function Version(): PAnsiChar; cdecl;
  external CLIPPER2_DLL name 'Version';

procedure DisposeExportedCPath64(cp: CPath64); cdecl;
  external CLIPPER2_DLL name 'DisposeExportedCPath64';
procedure DisposeExportedCPaths64(var cps: CPaths64); cdecl;
  external CLIPPER2_DLL name 'DisposeExportedCPaths64';
procedure DisposeExportedCPathD(cp: CPathD); cdecl;
  external CLIPPER2_DLL name 'DisposeExportedCPathD';
procedure DisposeExportedCPathsD(var cp: CPathsD); cdecl;
  external CLIPPER2_DLL name 'DisposeExportedCPathsD';
procedure DisposeExportedCPolyTree64(var cpt: PCPolyTree64); cdecl;
  external CLIPPER2_DLL name 'DisposeExportedCPolyTree64';
procedure DisposeExportedCPolyTreeD(var cpt: PCPolyTreeD); cdecl;
  external CLIPPER2_DLL name 'DisposeExportedCPolyTreeD';

function BooleanOp64(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPaths64; const subjects_open: CPaths64;
  const clips: CPaths64; out solution: CPaths64;
  out solution_open: CPaths64;
  preserve_collinear: boolean = true;
  reverse_solution: boolean = false): integer;  cdecl;
  external CLIPPER2_DLL name 'BooleanOp64';
function BooleanOpPt64(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPaths64; const subjects_open: CPaths64;
  const clips: CPaths64; out solution: PCPolyTree64;
  out solution_open: CPaths64;
  preserve_collinear: boolean = true;
  reverse_solution: boolean = false): integer; cdecl;
  external CLIPPER2_DLL name 'BooleanOpPt64';

function BooleanOpD(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPathsD; const subjects_open: CPathsD;
  const clips: CPathsD; out solution: CPathsD; out solution_open: CPathsD;
  precision: integer = 2;
  preserve_collinear: boolean = true;
  reverse_solution: boolean = false): integer; cdecl;
  external CLIPPER2_DLL name 'BooleanOpD';
function BooleanOpPtD(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPathsD; const subjects_open: CPathsD;
  const clips: CPathsD; out solution: PCPolyTreeD; out solution_open: CPathsD;
  precision: integer = 2;
  preserve_collinear: boolean = true;
  reverse_solution: boolean = false): integer; cdecl;
  external CLIPPER2_DLL name 'BooleanOpPtD';
function InflatePaths64(const paths: CPaths64;
  delta: double; jointype, endtype: UInt8; miter_limit: double = 2.0;
  arc_tolerance: double = 0.0;
  reverse_solution: Boolean = false): CPaths64; cdecl;
  external CLIPPER2_DLL name 'InflatePaths64';
function InflatePathsD(const paths: CPathsD;
  delta: double; jointype, endtype: UInt8; precision: integer = 2;
  miter_limit: double = 2.0; arc_tolerance: double = 0.0;
  reverse_solution: Boolean = false): CPathsD; cdecl;
  external CLIPPER2_DLL name 'InflatePathsD';

function RectClip64(const rect: TRect64;
  const paths: CPaths64): CPaths64; cdecl;
  external CLIPPER2_DLL name 'RectClip64';
function RectClipD(const rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPathsD; cdecl;
  external CLIPPER2_DLL name 'RectClipD';
function RectClipLines64(const rect: TRect64;
  const paths: CPaths64): CPaths64; cdecl;
  external CLIPPER2_DLL name 'RectClipLines64';
function RectClipLinesD(const rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPathsD; cdecl;
  external CLIPPER2_DLL name 'RectClipLinesD';

////////////////////////////////////////////////////////
// functions related to Clipper2 DLL structures
////////////////////////////////////////////////////////

procedure DisposeLocalCPath64(cp: CPath64);
begin
  FreeMem(cp);
end;

procedure DisposeLocalCPaths64(var cps: CPaths64);
var
  i, cnt: integer;
begin
  if cps = nil then Exit;
  cnt := cps[0][1];
  for i := 0 to cnt do //cnt +1
    FreeMem(cps[i]);
  FreeMem(cps);
  cps := nil;
end;

procedure DisposeLocalCPathD(cp: CPathD);
begin
  FreeMem(cp);
end;

procedure DisposeLocalCPathsD(var cps: CPathsD);
var
  i, cnt: integer;
begin
  if not Assigned(cps) then Exit;
  cnt := Round(cps[0][1]);
  for i := 0 to cnt do //cnt +1
    FreeMem(cps[i]);
  FreeMem(cps);
  cps := nil;
end;

////////////////////////////////////////////////////////
// conversion functions
////////////////////////////////////////////////////////

function TPath64ToCPath64(const path: TPath64): CPath64;
var
  i, len: integer;
begin
  len := Length(path);
  GetMem(Result, (2 + len * 2) * sizeof(Int64));
  Result[0] := len;
  Result[1] := 0;
  for i := 0 to len -1 do
  begin
    Result[2 + i*2] := path[i].X;
    Result[3 + i*2] := path[i].Y;
  end;
end;

function TPathDToCPathD(const path: TPathD): CPathD;
var
  i, len: integer;
begin
  len := Length(path);
  GetMem(Result, (2 + len * 2) * sizeof(Double));
  Result[0] := len;
  Result[1] := 0;
  for i := 0 to len -1 do
  begin
    Result[2 + i*2] := path[i].X;
    Result[3 + i*2] := path[i].Y;
  end;
end;

function CPath64Cntrs(cnt1, cnt2: Int64): CPath64;
begin
  GetMem(Result, 2 * sizeof(Int64));
  Result[0] := cnt1;
  Result[1] := cnt2;
end;

function CPathDCntrs(cnt1, cnt2: integer): CPathD;
begin
  GetMem(Result, 2 * sizeof(double));
  Result[0] := cnt1;
  Result[1] := cnt2;
end;

function TPaths64ToCPaths64(const pp: TPaths64): CPaths64;
var
  i,j, len, len2: integer;
begin
  len := Length(pp);
  len2 := len;
  for i := 0 to len -1 do
    if Length(pp[i]) = 0 then
      dec(len2);
  GetMem(Result, (1 + len2) * sizeof(Pointer));
  Result[0] := CPath64Cntrs(0, len2); // add the counter 'path'
  j := 1;
  for i := 0 to len -1 do
  begin
    if Length(pp[i]) = 0 then continue;
    Result[j] := TPath64ToCPath64(pp[i]);
    inc(j);
  end;
end;

function TPathsDToCPathsD(const pp: TPathsD): CPathsD;
var
  i,j, len, len2: integer;
begin
  len := Length(pp);
  len2 := len;
  for i := 0 to len -1 do
    if Length(pp[i]) = 0 then
      dec(len2);

  GetMem(Result, (1 + len2) * sizeof(Pointer));
  Result[0] := CPathDCntrs(0, len2); // add the counter 'path'
  j := 1;
  for i := 0 to len -1 do
  begin
    if Length(pp[i]) = 0 then continue;
    Result[j] := TPathDToCPathD(pp[i]);
    inc(j);
  end;
end;

function CPath64ToPath64(cp: CPath64): TPath64;
var
  i: integer;
  cnt: Int64;
begin
  if not Assigned(cp) then
    cnt := 0 else
    cnt := cp[0];
  SetLength(Result, cnt);
  for i := 0 to cnt -1 do
  begin
    Result[i].X := cp[2 + i*2];
    Result[i].Y := cp[3 + i*2];
  end;
end;

function CPathDToPathD(cp: CPathD): TPathD;
var
  i, cnt: integer;
begin
  if not Assigned(cp) then
    cnt := 0 else
    cnt := Round(cp[0]);
  SetLength(Result, cnt);
  for i := 0 to cnt -1 do
  begin
    Result[i].X := cp[2 + i*2];
    Result[i].Y := cp[3 + i*2];
  end;
end;

function CPaths64ToPaths64(cps: CPaths64): TPaths64;
var
  i: integer;
  cnt: Int64;
begin
  if not Assigned(cps) then
    cnt := 0 else
    cnt := cps[0][1];
  SetLength(Result, cnt);
  for i := 1 to cnt do
    Result[i-1] := CPath64ToPath64(cps[i]);
end;

function CPathsDToPathsD(cps: CPathsD): TPathsD;
var
  i, cnt: integer;
begin
  if not Assigned(cps) then
    cnt := 0 else
    cnt := Round(cps[0][1]);
  SetLength(Result, cnt);
  for i := 1 to cnt do
    Result[i-1] := CPathDToPathD(cps[i]);
end;

{$IFNDEF USING_CLIPPER2_SRC}

procedure AppendPath(var paths: TPaths64; const extra: TPath64); overload;
var
  len: Integer;
begin
  len := length(paths);
  SetLength(paths, len +1);
  paths[len] := extra;
end;

procedure AppendPath(var paths: TPathsD; const extra: TPathD); overload;
var
  len: Integer;
begin
  len := length(paths);
  SetLength(paths, len +1);
  paths[len] := extra;
end;
{$ENDIF}

procedure CPt64Internal(cpt: PCPolyTree64; var paths: TPaths64);
var
  i: integer;
  child: PCPolyTree64;
begin
  if Assigned(cpt.polygon) then
    AppendPath(paths, CPath64ToPath64(cpt.polygon));
  if cpt.childCnt = 0 then Exit;
  child := cpt.childs;
  for i := 0 to cpt.childCnt -1 do
  begin
    CPt64Internal(child, paths);
    inc(child);
  end;
end;

function CPolytree64ToPaths64(cpt: PCPolyTree64): TPaths64;
var
  i: integer;
  child: PCPolyTree64;
begin
  Result := nil;
  if not Assigned(cpt) or (cpt.childCnt = 0) then Exit;
  child := cpt.childs;
  for i := 0 to cpt.childCnt -1 do
  begin
    CPt64Internal(child, Result);
    inc(child);
  end;
end;

procedure CPtDInternal(cpt: PCPolyTreeD; var paths: TPathsD);
var
  i: integer;
  child: PCPolyTreeD;
begin
  AppendPath(paths, CPathDToPathD(cpt.polygon));
  if cpt.childCnt = 0 then Exit;
  child := cpt.childs;
  for i := 0 to cpt.childCnt -1 do
  begin
    CPtDInternal(child, paths);
    inc(child);
  end;
end;

function CPolytreeDToPathsD(cpt: PCPolyTreeD): TPathsD;
var
  i: integer;
  child: PCPolyTreeD;
begin
  Result := nil;
  if not Assigned(cpt) or (cpt.childCnt = 0) then Exit;
  child := cpt.childs;
  for i := 0 to cpt.childCnt -1 do
  begin
    CPtDInternal(child, Result);
    inc(child);
  end;
end;

////////////////////////////////////////////////////////
// miscellaneous functions
////////////////////////////////////////////////////////

function MakePath64(vals: array of Int64): TPath64;
var
  i, len: integer;
begin
  len := Length(vals) div 2;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := vals[i*2];
    Result[i].Y := vals[i*2 +1];
  end;
end;

function MakePathD(vals: array of double): TPathD;
var
  i, len: integer;
begin
  len := Length(vals) div 2;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := vals[i*2];
    Result[i].Y := vals[i*2 +1];
  end;
end;

function MakeRandomPath(maxWidth, maxHeight, count: Integer;
  margin: Integer = 10): TPath64;
var
  i: Integer;
begin
  setlength(Result, count);
  for i := 0 to count -1 do with Result[i] do
  begin
    X := Random(maxWidth - 2 * margin) + margin;
    Y := Random(maxHeight - 2 * margin) + margin;
  end;
end;

function MakeRandomPathD(maxWidth, maxHeight, count: Integer;
  margin: Integer = 10): TPathD;
var
  i: Integer;
begin
  setlength(Result, count);
  for i := 0 to count -1 do with Result[i] do
  begin
    X := Random(maxWidth - 2 * margin) + margin;
    Y := Random(maxHeight - 2 * margin) + margin;
  end;
end;

function RectToPath64(const rec: TRect64): TPath64;
begin
  with rec do
    Result := MakePath64([left, top, right, top,
      right, bottom, left, bottom]);
end;

procedure WriteCPath64(p: CPath64);
var
  i, len: integer;
  s: string;
begin
  len := p[0];
  if len = 0 then Exit;
  s := '';
  for i := 0 to len -1 do
    s := s +
      inttostr(p[2 + i*2]) + ',' +
      inttostr(p[3 + i*2]) + ', ';
  WriteLn(s);
end;

procedure WritePath64(p: TPath64);
var
  i,len: integer;
  s: string;
begin
  len := Length(p);
  if len = 0 then Exit;
  s := '';
  for i := 0 to len -1 do
    s := s +
      inttostr(p[i].X) + ',' +
      inttostr(p[i].Y) + ', ';
  WriteLn(s);
end;


procedure WritePaths64(pp: TPaths64);
var
  i: integer;
begin
  for i := 0 to High(pp) do
    WritePath64(pp[i]);
end;

procedure WritePathD(p: TPathD);
var
  i,len: integer;
  s: string;
begin
  len := Length(p);
  if len = 0 then Exit;
  s := '';
  for i := 0 to len -1 do
    s := Format('%s%1.2f,%1.2f, ',
      [s, p[i].X, p[i].Y]);
  WriteLn(s);
end;

procedure WritePathsD(pp: TPathsD);
var
  i: integer;
begin
  for i := 0 to High(pp) do
    WritePathD(pp[i]);
end;

procedure WriteCPolyTree64(pp: PCPolyTree64);
var
  i: integer;
  child: PCPolyTree64;
begin
  if Assigned(pp.polygon) then
    WriteCPath64(pp.polygon);
  if pp.childCnt = 0 then Exit;
  child := pp.childs;
  for i := 0 to pp.childCnt -1 do
  begin
    WriteCPolyTree64(child);
    inc(child);
  end;
end;

procedure WriteCPathD(p: CPathD);
var
  i, len: integer;
  s: string;
begin
  len := round(p[0]);
  if len = 0 then Exit;
  s := '';
  for i := 0 to len -1 do
    s := Format('%s%1.2f,%1.2f, ',
      [s, p[2 + i*2], p[3 + i*2]]);
  WriteLn(s);
end;

procedure WriteCPathsD(pp: CPathsD);
var
  i, len: integer;
begin
  len := Round(pp[0][1]);
  for i := 1 to len do
    WriteCPathD(pp[i]);
end;

procedure WriteCPolyTreeD(pp: PCPolyTreeD);
var
  i: integer;
  child: PCPolyTreeD;
begin
  if Assigned(pp.polygon) then
    WriteCPathD(pp.polygon);
  if pp.childCnt = 0 then Exit;
  child := pp.childs;
  for i := 0 to pp.childCnt -1 do
  begin
    WriteCPolyTreeD(child);
    inc(child);
  end;
end;

procedure ShowSvgImage(const svgFilename: string);
begin
  ShellExecute(0, 'open',PChar(svgFilename), nil, nil, SW_SHOW);
end;

////////////////////////////////////////////////////////
// test procedures
////////////////////////////////////////////////////////

const
  displayWidth = 800;
  displayHeight = 600;

procedure Test_MegaStress(maxCnt: integer);
var
  i: integer;
  sub, clp: TPathsD;
  csub_local, cclp_local: CPathsD;
  csol_extern, csolo_extern: CPathsD;
begin
  csol_extern   := nil;
  csolo_extern  := nil;
  SetLength(sub, 1);
  SetLength(clp, 1);

  for i := 1 to maxCnt do
  begin
    sub[0] := MakeRandomPathD(displayWidth, displayHeight, 50);
    clp[0] := MakeRandomPathD(displayWidth, displayHeight, 50);
    csub_local := TPathsDToCPathsD(sub);
    cclp_local := TPathsDToCPathsD(clp);

    BooleanOpD(Uint8(TClipType.ctIntersection),
      Uint8(TFillRule.frNonZero),
      csub_local, nil, cclp_local,
      csol_extern, csolo_extern);

    DisposeLocalCPathsD(csub_local);
    DisposeLocalCPathsD(cclp_local);
    DisposeExportedCPathsD(csol_extern);
    DisposeExportedCPathsD(csolo_extern);
    if i mod 100 = 0 then Write('.');
  end;
  WriteLn(#10'Passed!');
end;

procedure Test_Version();
begin
  Write(#10'Clipper2 DLL version: ');
  WriteLn(Version);
end;

procedure Test_BooleanOp64(edgeCnt: integer);
var
  sub, clp: TPaths64;
  csub_local, cclp_local: CPaths64;
  csol_extern, csolo_extern: CPaths64;
  svg: TSvgWriter;
begin
    // setup
    csolo_extern := nil;
    WriteLn(#10'Testing BooleanOp64');
    SetLength(sub, 1);
    sub[0] := MakeRandomPath(displayWidth, displayHeight, edgeCnt);
    SetLength(clp, 1);
    clp[0] := MakeRandomPath(displayWidth, displayHeight, edgeCnt);
    // convert paths into DLL structures (will require local clean up)
    csub_local := TPaths64ToCPaths64(sub);
    cclp_local := TPaths64ToCPaths64(clp);

    // do the DLL operation
    BooleanOp64(Uint8(TClipType.ctIntersection),
      Uint8(TFillRule.frNonZero),
      csub_local, nil, cclp_local,
      csol_extern, csolo_extern);

    // optionally display result on the console
    //WriteCPaths64(csol_extern);

    // finally, display and clean up
    svg := TSvgWriter.Create(frNonZero);
    try
      AddSubject(svg, sub);
      AddClip(svg, clp);
      AddSolution(svg, CPaths64ToPaths64(csol_extern));
      SaveSvg(svg, 'BooleanOp64.svg', displayWidth, displayHeight);
      ShowSvgImage('BooleanOp64.svg');
    finally
      svg.Free;
    end;

    DisposeLocalCPaths64(csub_local);
    DisposeLocalCPaths64(cclp_local);
    DisposeExportedCPaths64(csol_extern);
    DisposeExportedCPaths64(csolo_extern);
end;

procedure Test_BooleanOpD(edgeCnt: integer);
var
  sub, clp: TPathsD;
  csub_local, cclp_local: CPathsD;
  csol_extern, csolo_extern: CPathsD;
  svg: TSvgWriter;
begin
    // setup
    csolo_extern := nil;
    WriteLn(#10'Testing BooleanOpD');
    SetLength(sub, 1);
    sub[0] := MakeRandomPathD(displayWidth, displayHeight, edgeCnt);
    SetLength(clp, 1);
    clp[0] := MakeRandomPathD(displayWidth, displayHeight, edgeCnt);
    // convert paths into DLL structures (will require local clean up)
    csub_local := TPathsDToCPathsD(sub);
    cclp_local := TPathsDToCPathsD(clp);

    // do the DLL operation
    BooleanOpD(Uint8(TClipType.ctIntersection),
      Uint8(TFillRule.frNonZero),
      csub_local, nil, cclp_local,
      csol_extern, csolo_extern);

    // optionally display result on the console
    //WriteCPaths64(csol_extern);

    // finally, display and clean up
    svg := TSvgWriter.Create(frNonZero);
    try
      AddSubject(svg, sub);
      AddClip(svg, clp);
      AddSolution(svg, CPathsDToPathsD(csol_extern));
      SaveSvg(svg, 'BooleanOpD.svg', displayWidth, displayHeight);
      ShowSvgImage('BooleanOpD.svg');
    finally
      svg.Free;
    end;

    DisposeLocalCPathsD(csub_local);
    DisposeLocalCPathsD(cclp_local);
    DisposeExportedCPathsD(csol_extern);
    DisposeExportedCPathsD(csolo_extern);
end;

procedure Test_BooleanOpPtD(edgeCnt: integer);
var
  sub, clp, sol: TPathsD;
  csub_local, cclp_local: CPathsD;
  csol_extern: PCPolyTreeD;
  csolo_extern: CPathsD;
  svg: TSvgWriter;
begin
    // setup
    csolo_extern := nil;
    WriteLn(#10'Testing BooleanOpPtD');
    SetLength(sub, 1);
    sub[0] := MakeRandomPathD(displayWidth, displayHeight, edgeCnt);
    SetLength(clp, 1);
    clp[0] := MakeRandomPathD(displayWidth, displayHeight, edgeCnt);
    // convert paths into DLL structures (will require local clean up)
    csub_local := TPathsDToCPathsD(sub);
    cclp_local := TPathsDToCPathsD(clp);

    // do the DLL operation
    BooleanOpPtD(Uint8(TClipType.ctIntersection),
      Uint8(TFillRule.frNonZero),
      csub_local, nil, cclp_local,
      csol_extern, csolo_extern);

    // optionally display result on the console
    //WriteCPaths64(csol_extern);

    sol := CPolytreeDToPathsD(csol_extern);
    DisposeExportedCPolyTreeD(csol_extern);
    DisposeExportedCPathsD(csolo_extern);

    // finally, display and clean up
    svg := TSvgWriter.Create(frNonZero);
    try
      AddSubject(svg, sub);
      AddClip(svg, clp);
      AddSolution(svg, sol);
      SaveSvg(svg, 'BooleanOpPtD.svg', displayWidth, displayHeight);
      ShowSvgImage('BooleanOpPtD.svg');
    finally
      svg.Free;
    end;

    DisposeLocalCPathsD(csub_local);
    DisposeLocalCPathsD(cclp_local);
end;

procedure Test_InflatePaths64(delta: double);
var
  sub: TPaths64;
  csub_local: CPaths64;
  csol_extern: CPaths64;
  svg: TSvgWriter;
begin
    // setup
    WriteLn(#10'Testing InflatePaths64');
    SetLength(sub, 1);
    sub[0] := MakeRandomPath(displayWidth, displayHeight, 7);
    // convert path into DLL structure (will require local clean up)
    csub_local := TPaths64ToCPaths64(sub);

    // do the DLL operation
    csol_extern := InflatePaths64(csub_local, delta,
      UInt8(TJoinType.jtMiter), UInt8(TEndType.etPolygon), 2, 4);

    // optionally display result on the console
    //WriteCPaths64(csol_extern);

    // finally, display and clean up
    svg := TSvgWriter.Create(frNonZero);
    try
      AddSubject(svg, sub);
      AddSolution(svg, CPaths64ToPaths64(csol_extern));
      SaveSvg(svg, 'InflatePaths64.svg', displayWidth, displayHeight);
      ShowSvgImage('InflatePaths64.svg');
    finally
      svg.Free;
    end;

    DisposeLocalCPaths64(csub_local);
    DisposeExportedCPaths64(csol_extern);
end;

procedure Test_RectClip64(edgeCnt: integer);
var
  sub, clp: TPaths64;
  csub_local: CPaths64;
  csol_extern: CPaths64;
  rec: TRect64;
  svg: TSvgWriter;
begin
    // setup
    WriteLn(#10'Testing RectClip64:');
    SetLength(sub, 1);

    sub[0] := MakeRandomPath(displayWidth, displayHeight, edgeCnt);
    csub_local := TPaths64ToCPaths64(sub);

    rec.Left := 80;
    rec.Top := 80;
    rec.Right := displayWidth - 80;
    rec.Bottom := displayHeight -80;

    // do the DLL operation
    csol_extern := RectClip64(rec, csub_local);

    // optionally display result on the console
    //WriteCPaths64(csol_extern);

    // finally, display and clean up

    SetLength(clp, 1);
    clp[0] := RectToPath64(rec);

    svg := TSvgWriter.Create(frNonZero);
    try
      AddSubject(svg, sub);
      AddClip(svg, clp);
      AddSolution(svg, CPaths64ToPaths64(csol_extern));
      SaveSvg(svg, 'RectClip64.svg', displayWidth, displayHeight);
      ShowSvgImage('RectClip64.svg');
    finally
      svg.Free;
    end;

    DisposeLocalCPaths64(csub_local);
    DisposeExportedCPaths64(csol_extern);
end;

procedure Test_RectClipLines64(edgeCnt: integer);
var
  sub, clp: TPaths64;
  csub_local: CPaths64;
  csolo_extern: CPaths64;
  rec: TRect64;
  svg: TSvgWriter;
begin
    // setup
    WriteLn(#10'Testing RectClipLines64:');
    SetLength(sub, 1);

    sub[0] := MakeRandomPath(displayWidth, displayHeight, edgeCnt);
    csub_local := TPaths64ToCPaths64(sub);

    rec.Left := 80;
    rec.Top := 80;
    rec.Right := displayWidth - 80;
    rec.Bottom := displayHeight -80;

    // do the DLL operation
    csolo_extern := RectClipLines64(rec, csub_local);

    // optionally display result on the console
    //WriteCPaths64(csol_extern);

    // finally, display and clean up

    SetLength(clp, 1);
    clp[0] := RectToPath64(rec);

    svg := TSvgWriter.Create(frNonZero);
    try
      AddOpenSubject(svg, sub);
      AddClip(svg, clp); //rectangle
      AddOpenSolution(svg, CPaths64ToPaths64(csolo_extern));
      SaveSvg(svg, 'RectClipLines64.svg', displayWidth, displayHeight);
      ShowSvgImage('RectClipLines64.svg');
    finally
      svg.Free;
    end;

    DisposeLocalCPaths64(csub_local);
    DisposeExportedCPaths64(csolo_extern);
end;

procedure Test_Performance(lowThousand, hiThousand: integer);
var
  i: integer;
  elapsed: double;
  sub, clp: TPaths64;
  csub_local, cclp_local: CPaths64;
  csol_extern, csolo_extern: CPaths64;
  svg: TSvgWriter;
begin
  csolo_extern := nil;
  WriteLn(#10'Testing Performance');
  for i := lowThousand to hiThousand do
  begin
    Write(format(#10' C++ DLL     - %d edges: ', [i*1000]));
    // setup
    SetLength(sub, 1);
    sub[0] := MakeRandomPath(displayWidth, displayHeight, i*1000);
    SetLength(clp, 1);
    clp[0] := MakeRandomPath(displayWidth, displayHeight, i*1000);
    // convert paths into DLL structures (will require local clean up)
    csub_local := TPaths64ToCPaths64(sub);
    cclp_local := TPaths64ToCPaths64(clp);

    //using a code block for the timer
    begin
      InitTimer(elapsed);
      // do the DLL operation
      BooleanOp64(Uint8(TClipType.ctIntersection),
        Uint8(TFillRule.frNonZero),
        csub_local, nil, cclp_local,
        csol_extern, csolo_extern);
    end;
    WriteLn(format('%1.3n secs', [elapsed]));

{$IFDEF USING_CLIPPER2_SRC}
    Write(format(' Pure delphi - %d edges: ', [i*1000]));
    begin
      InitTimer(elapsed);
      Clipper.Intersect(sub, clp, Clipper.frNonZero);
    end;
    WriteLn(format('%1.3n secs', [elapsed]));
{$ENDIF}

    if i = hiThousand then
    begin
      svg := TSvgWriter.Create(frNonZero);
      try
        AddSubject(svg, sub);
        AddClip(svg, clp);
        AddSolution(svg, CPaths64ToPaths64(csol_extern));
        SaveSvg(svg, 'Performance.svg', displayWidth, displayHeight);
        ShowSvgImage('Performance.svg');
      finally
        svg.Free;
      end;
    end;

    DisposeLocalCPaths64(csub_local);
    DisposeLocalCPaths64(cclp_local);
    DisposeExportedCPaths64(csol_extern);
    DisposeExportedCPaths64(csolo_extern);

  end; //bottom of for loop
end;


////////////////////////////////////////////////////////
// main entry here
////////////////////////////////////////////////////////

var
  s: string;
begin
  Randomize;
  Test_Version();
  Test_BooleanOp64(50);
  Test_BooleanOpD(75);
  Test_BooleanOpPtD(20);
  Test_InflatePaths64(-10);
  Test_RectClip64(25);
  Test_RectClipLines64(25);
  //Test_Performance(1, 5); // 1000 t0 5000
  //Test_MegaStress(10000);

  WriteLn(#10'Press Enter to quit.');
  ReadLn(s);
end.
