program Test_DLL;

// Make sure that Clipper2.dll is in either the OS Path
// or in the application's folder.

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils;

type
  TClipType = (ctNone, ctIntersection, ctUnion, ctDifference, ctXor);
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);
  TJoinType = (jtSquare, jtRound, jtMiter);
  TEndType  = (etPolygon, etJoined, etButt, etSquare, etRound);

  TPoint64 = record X,Y: Int64; end;
  TPath64 = array of TPoint64;
  TPaths64 = array of TPath64;

  CInt64arr = array[0..$FFFFF] of Int64;
  CPath64 = ^CInt64arr;
  CPath64arr = array[0..$FFFFF] of CPath64;
  CPaths64 = ^CPath64arr;

  TPointD = record X,Y: double; end;
  TPathD = array of TPointD;
  TPathsD = array of TPathD;

  CDblarr = array[0..$FFFFF] of Double;
  CPathD = ^CDblarr;
  CPathDarr = array[0..$FFFFF] of CPathD;
  CPathsD = ^CPathDarr;

  TRect64 = record l,t,r,b: Int64; end;
  TRectD = record l,t,r,b: double; end;

  PCPolyTree64 = ^CPolyTree64;
  CPolyTree64 = record
    polygon   : CPath64;
    isHole    : Boolean;
    childCnt  : integer;
    childs    : PCPolyTree64;
  end;

  PCPolyTreeD = ^CPolyTreeD;
  CPolyTreeD = record
    polygon   : CPathD;
    isHole    : Boolean;
    childCnt  : integer;
    childs    : PCPolyTreeD;
  end;

function Version(): PAnsiChar;
  external 'Clipper2.dll' name 'Version';

procedure DisposeExportedCPath64(cp: CPath64);
  external 'Clipper2.dll' name 'DisposeExportedCPath64';
procedure DisposeExportedCPaths64(cps: CPaths64);
  external 'Clipper2.dll' name 'DisposeExportedCPaths64';
procedure DisposeExportedCPathD(cp: CPathD);
  external 'Clipper2.dll' name 'DisposeExportedCPathD';
procedure DisposeExportedCPathsD(cp: CPathsD);
  external 'Clipper2.dll' name 'DisposeExportedCPathsD';
procedure DisposeCPolyTree64(cpt: PCPolyTree64);
  external 'Clipper2.dll' name 'DisposeCPolyTree64';
procedure DisposeCPolyTreeD(var cpt: PCPolyTreeD);
  external 'Clipper2.dll' name 'DisposeCPolyTreeD';

function BooleanOp64(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPaths64; const subjects_open: CPaths64;
  const clips: CPaths64; out solution: CPaths64; out solution_open: CPaths64;
  preserve_collinear: boolean = true; reverse_solution: boolean = false): integer;
  external 'Clipper2.dll' name 'BooleanOp64';
function BooleanOpPt64(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPaths64; const subjects_open: CPaths64;
  const clips: CPaths64; out solution: PCPolyTree64; out solution_open: CPaths64;
  preserve_collinear: boolean = true; reverse_solution: boolean = false): integer;
  external 'Clipper2.dll' name 'BooleanOpPt64';

function BooleanOpD(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPathsD; const subjects_open: CPathsD;
  const clips: CPathsD; out solution: CPathsD; out solution_open: CPathsD;
  preserve_collinear: boolean = true; reverse_solution: boolean = false): integer;
  external 'Clipper2.dll' name 'BooleanOpD';
function BooleanOpPtD(cliptype: UInt8; fillrule: UInt8;
  const subjects: CPathsD; const subjects_open: CPathsD;
  const clips: CPathsD; out solution: PCPolyTreeD; out solution_open: CPathsD;
  preserve_collinear: boolean = true; reverse_solution: boolean = false): integer;
  external 'Clipper2.dll' name 'BooleanOpPtD';
function InflatePaths64(const paths: CPaths64;
  delta: double; jt, et: UInt8; miter_limit: double = 2.0;
  arc_tolerance: double = 0.0; reverse_solution: Boolean = false): CPaths64;
  external 'Clipper2.dll' name 'InflatePaths64';
function InflatePathsD(const paths: CPathsD;
  delta: double; jt, et: UInt8; miter_limit: double = 2.0;
  arc_tolerance: double = 0.0; reverse_solution: Boolean = false): CPathsD;
  external 'Clipper2.dll' name 'InflatePathsD';

function RectClip64(const rect: TRect64; const paths: CPaths64): CPaths64;
 external 'Clipper2.dll' name 'RectClip64';
function RectClipD(const rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPathsD;
  external 'Clipper2.dll' name 'RectClipD';
function RectClipLines64(const rect: TRect64; const paths: CPaths64): CPaths64;
 external 'Clipper2.dll' name 'RectClipLines64';
function RectClipLinesD(const rect: TRectD;
  const paths: CPathsD; precision: integer = 2): CPathsD;
  external 'Clipper2.dll' name 'RectClipLinesD';

procedure DisposeLocalCPath64(cp: CPath64);
begin
  FreeMem(cp);
end;

procedure DisposeLocalCPaths64(cps: CPaths64);
var
  i, cnt: integer;
  cp: CPath64;
begin
  cnt := cps[0][1];
  for i := 0 to cnt do //cnt +1
    FreeMem(cps[i]);
  FreeMem(cps);
end;

procedure DisposeLocalCPathD(cp: CPathD);
begin
  FreeMem(cp);
end;

procedure DisposeLocalCPathsD(cps: CPathsD);
var
  i, cnt: integer;
  cp: CPathD;
begin
  if not Assigned(cps) then Exit;
  cnt := Round(cps[0][1]);
  for i := 0 to cnt do //cnt +1
    FreeMem(cps[i]);
  FreeMem(cps);
end;

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
  p : CPath64;
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
  p : CPathD;
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
  s: string;
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

var
  i, res: integer;
  s: string;
  sub, clp, sol: TPathsD;
  csub, cclp, csol, csolo: CPathsD;
  cptd: PCPolyTreeD;
//  sub, clp, sol: TPaths64;
//  csub, cclp, csol, csolo: CPaths64;
  r: TRectD;
begin
  csolo := nil;

  WriteLn(#10'Clipper2 DLL version:');
  WriteLn(Version);

  SetLength(sub, 1);
  sub[0] := MakePathD([10,10, 100,10, 100,100,10,100]);
  SetLength(clp, 1);
  clp[0] := MakePathD([20,20,110, 20, 110,110, 20,110]);
  csub := TPathsDToCPathsD(sub);
  cclp := TPathsDToCPathsD(clp);

  WriteLn(#10'Testing BooleanOpD:');
  BooleanOpD(Uint8(TClipType.ctIntersection),
    Uint8(TFillRule.frNonZero), csub, nil, cclp, csol, csolo);
  WriteCPathsD(csol);

  WriteLn(#10'Testing BooleanOpPtD:');
  BooleanOpPtD(Uint8(TClipType.ctIntersection),
    Uint8(TFillRule.frNonZero), csub, nil, cclp, cptd, csolo);
  WriteCPolyTreeD(cptd);
  // clean up
  DisposeCPolyTreeD(cptd);

  WriteLn(#10'Testing InflatePathsD:');
  csol := InflatePathsD( csub, 20,
    UInt8(TJoinType.jtMiter), UInt8(TEndType.etPolygon), 2, 4);
  WriteCPathsD(csol);

  //dispose locally constructed CPaths
  DisposeLocalCPathsD(csub);
  DisposeLocalCPathsD(cclp);

  //dispose DLL constructed CPaths
  DisposeExportedCPathsD(csol);
  DisposeLocalCPathsD(csolo);


  WriteLn(#10'Press Enter to quit.');
  ReadLn(s);
end.
