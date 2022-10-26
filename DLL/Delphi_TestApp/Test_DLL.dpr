program Test_DLL;

// Make sure that Clipper2.dll is in either the OS Path
// or in the application's folder.

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  ShellApi,
  SysUtils,
  SvgWriter in 'SvgWriter.pas',
  Timer in 'Timer.pas';           //just for performance testing
type

  ////////////////////////////////////////////////////////
  // Clipper2 DLL structures
  ////////////////////////////////////////////////////////

  TClipType = (ctNone, ctIntersection, ctUnion, ctDifference, ctXor);
  //TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative); // see SvgWriter
  TJoinType = (jtSquare, jtRound, jtMiter);
  TEndType  = (etPolygon, etJoined, etButt, etSquare, etRound);

  TPoint64 = record X,Y: Int64; end;
  TPath64 = array of TPoint64;
  TPaths64 = array of TPath64;

  CInt64arr = array[0..$FFFFF] of Int64;
  CPath64 = ^CInt64arr;
  CPath64arr = array[0..$FFFFF] of CPath64;
  CPaths64 = ^CPath64arr;

//  TPointD = record X,Y: double; end;    // see SvgWriter
//  TPathD = array of TPointD;            // see SvgWriter
//  TPathsD = array of TPathD;            // see SvgWriter

  CDblarr = array[0..$FFFFF] of Double;
  CPathD = ^CDblarr;
  CPathDarr = array[0..$FFFFF] of CPathD;
  CPathsD = ^CPathDarr;

  TRect64 = record l,t,r,b: Int64; end;
//  TRectD = record l,t,r,b: double; end; // see SvgWriter

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
  CLIPPER2_DLL = 'Clipper2_64.dll';

////////////////////////////////////////////////////////
// Clipper2 DLL functions
////////////////////////////////////////////////////////

function Version(): PAnsiChar;
  external CLIPPER2_DLL name 'Version';

procedure DisposeExportedCPath64(cp: CPath64);
  external CLIPPER2_DLL name 'DisposeExportedCPath64';
procedure DisposeExportedCPaths64(var cps: CPaths64);
  external CLIPPER2_DLL name 'DisposeExportedCPaths64';
procedure DisposeExportedCPathD(cp: CPathD);
  external CLIPPER2_DLL name 'DisposeExportedCPathD';
procedure DisposeExportedCPathsD(var cp: CPathsD);
  external CLIPPER2_DLL name 'DisposeExportedCPathsD';
procedure DisposeExportedCPolyTree64(var cpt: PCPolyTree64);
  external CLIPPER2_DLL name 'DisposeExportedCPolyTree64';
procedure DisposeExportedCPolyTreeD(var cpt: PCPolyTreeD);
  external CLIPPER2_DLL name 'DisposeExportedCPolyTreeD';

function BooleanOp64(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPaths64; const subjects_open: CPaths64;
  const clips: CPaths64; out solution: CPaths64; out solution_open: CPaths64;
  preserve_collinear: boolean = true; reverse_solution: boolean = false): integer;
  external CLIPPER2_DLL name 'BooleanOp64';
function BooleanOpPt64(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPaths64; const subjects_open: CPaths64;
  const clips: CPaths64; out solution: PCPolyTree64; out solution_open: CPaths64;
  preserve_collinear: boolean = true; reverse_solution: boolean = false): integer;
  external CLIPPER2_DLL name 'BooleanOpPt64';

function BooleanOpD(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPathsD; const subjects_open: CPathsD;
  const clips: CPathsD; out solution: CPathsD; out solution_open: CPathsD;
  preserve_collinear: boolean = true; reverse_solution: boolean = false): integer;
  external CLIPPER2_DLL name 'BooleanOpD';
function BooleanOpPtD(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPathsD; const subjects_open: CPathsD;
  const clips: CPathsD; out solution: PCPolyTreeD; out solution_open: CPathsD;
  preserve_collinear: boolean = true; reverse_solution: boolean = false): integer;
  external CLIPPER2_DLL name 'BooleanOpPtD';
function InflatePaths64(const paths: CPaths64;
  delta: double; jt, et: UInt8; miter_limit: double = 2.0;
  arc_tolerance: double = 0.0; reverse_solution: Boolean = false): CPaths64;
  external CLIPPER2_DLL name 'InflatePaths64';
function InflatePathsD(const paths: CPathsD;
  delta: double; jt, et: UInt8; miter_limit: double = 2.0;
  arc_tolerance: double = 0.0; reverse_solution: Boolean = false): CPathsD;
  external CLIPPER2_DLL name 'InflatePathsD';

function RectClip64(const rect: TRect64; const paths: CPaths64): CPaths64;
 external CLIPPER2_DLL name 'RectClip64';
function RectClipD(const rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPathsD;
  external CLIPPER2_DLL name 'RectClipD';
function RectClipLines64(const rect: TRect64; const paths: CPaths64): CPaths64;
 external CLIPPER2_DLL name 'RectClipLines64';
function RectClipLinesD(const rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPathsD;
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
  begin
    Result := nil;
    Exit;
  end;
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
  begin
    Result := nil;
    Exit;
  end;
  cnt := Round(cps[0][1]);
  SetLength(Result, cnt);
  for i := 1 to cnt do
    Result[i-1] := CPathDToPathD(cps[i]);
end;

////////////////////////////////////////////////////////
// miscellaneous functions
////////////////////////////////////////////////////////

function MakePath(vals: array of Int64): TPath64;
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
// main entry here
////////////////////////////////////////////////////////

const
  perform_1000_low  = 1;
  perform_1000_high = 7;
var
  i: integer;
  elapsed: double;
  s: string;
  svg: TSvgWriter; // class  (heap storage)
  //sw: TStopwatch;  // record (stack storage)

  //dynamic arrays // heap storage but with automatic cleanup
  sub, clp, sol: TPathsD;
  //pointer arrays - locally allocated and requires manual cleanup
  //While these could also have been defined as dynamic arrays with
  //automatic cleanup, that might have confused those unfamiliar
  //with Delphi but still using this code as a template for code
  //in their preferred language.
  csub_local, cclp_local: CPathsD;
  //pointer arrays - external allocation and external cleanup
  csol_extern, csolo_extern: CPathsD;
  cptd_extern: PCPolyTreeD;
begin

  Randomize;
  csolo_extern := nil;
  svg := TSvgWriter.Create(frNonZero);
  try

    WriteLn(#10'Clipper2 DLL version:');
    WriteLn(Version);

    SetLength(sub, 1);
    sub[0] := MakePathD([10,10, 100,10, 100,100,10,100]);
    SetLength(clp, 1);
    clp[0] := MakePathD([20,20,110, 20, 110,110, 20,110]);
    csub_local := TPathsDToCPathsD(sub);
    cclp_local := TPathsDToCPathsD(clp);

    WriteLn(#10'Testing BooleanOpD:');
    BooleanOpD(Uint8(TClipType.ctIntersection),
      Uint8(TFillRule.frNonZero),
      csub_local, nil, cclp_local,
      csol_extern, csolo_extern);

    //WriteCPathsD(csol_extern); //write coords to console

    AddSubject(svg, sub);
    AddClip(svg, clp);
    AddSolution(svg, CPathsDToPathsD(csol_extern));
    SaveSvg(svg, 'BooleanOpD.svg', 400,400);
    ShowSvgImage('BooleanOpD.svg');
    svg.ClearAll;


    WriteLn(#10'Testing random path:');
    sub[0] := MakeRandomPathD(400,400,50);
    clp[0] := MakeRandomPathD(400,400,50);
    csub_local := TPathsDToCPathsD(sub);
    cclp_local := TPathsDToCPathsD(clp);
    BooleanOpD(Uint8(TClipType.ctIntersection),
      Uint8(TFillRule.frNonZero),
      csub_local, nil, cclp_local,
      csol_extern, csolo_extern);

    AddSubject(svg, sub);
    AddClip(svg, clp);
    AddSolution(svg, CPathsDToPathsD(csol_extern));
    //DisposeExportedCPathsD(csol_extern); //used below
    DisposeExportedCPathsD(csolo_extern);
    SaveSvg(svg, 'Random.svg', 400,400);
    ShowSvgImage('Random.svg');
    svg.ClearAll;

    WriteLn(#10'Testing InflatePathsD:');
    sol := CPathsDToPathsD(csol_extern);
    csol_extern := InflatePathsD( csol_extern, -10,
      UInt8(TJoinType.jtMiter), UInt8(TEndType.etPolygon), 2, 4);
    //WriteCPathsD(csol_extern); //write coords to console

    AddSubject(svg, sol);
    AddSolution(svg, CPathsDToPathsD(csol_extern));
    DisposeExportedCPathsD(csol_extern);
    SaveSvg(svg, 'InflatePathsD.svg', 400,400);
    ShowSvgImage('InflatePathsD.svg');
    svg.ClearAll;

    WriteLn(#10'Testing BooleanOpPtD:');
    BooleanOpPtD(Uint8(TClipType.ctIntersection),
      Uint8(TFillRule.frNonZero),
      csub_local, nil, cclp_local,
      cptd_extern, csolo_extern);
    //WriteCPolyTreeD(cptd_extern); //write coords to console

    WriteLn(#10'Performance testing ...');
    for i := perform_1000_low to perform_1000_high do
    begin
      sub[0] := MakeRandomPathD(800,600,i*1000);
      clp[0] := MakeRandomPathD(800,600,i*1000);
      Write(format(#10'  testing %d edges: ', [i*1000]));
      begin
        InitTimer(elapsed);
        csub_local := TPathsDToCPathsD(sub);
        cclp_local := TPathsDToCPathsD(clp);
        BooleanOpD(Uint8(TClipType.ctIntersection),
          Uint8(TFillRule.frNonZero),
          csub_local, nil, cclp_local,
          csol_extern, csolo_extern);
      end;
      WriteLn(format('%1.3n secs', [elapsed]));

      //clean up here unless the last iteration
      if i < perform_1000_high then
      begin
        DisposeLocalCPathsD(csub_local);
        DisposeLocalCPathsD(cclp_local);
        DisposeExportedCPathsD(csol_extern);
        DisposeExportedCPathsD(csolo_extern);
      end;
    end;

    AddSubject(svg, sub);
    AddClip(svg, clp);
    AddSolution(svg, CPathsDToPathsD(csol_extern));
    DisposeExportedCPathsD(csol_extern);
    DisposeExportedCPathsD(csolo_extern);
    SaveSvg(svg, 'Performance.svg', 800,600);
    ShowSvgImage('Performance.svg');
    svg.ClearAll;


    // clean up
    //dispose locally constructed CPaths
    DisposeLocalCPathsD(csub_local);
    DisposeLocalCPathsD(cclp_local);
    //dispose DLL constructed objects
    DisposeExportedCPolyTreeD(cptd_extern);

  finally
    svg.Free;
  end;

  WriteLn(#10'Press Enter to quit.');
  ReadLn(s);
end.
