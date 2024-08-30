program Test1_DLL;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  Math,
  SysUtils,
  Clipper2DllCore in 'Clipper2DllCore.pas',
  Clipper2DllSVG in 'Clipper2DllSVG.pas';

////////////////////////////////////////////////////////
// Z callback functions
////////////////////////////////////////////////////////

{$IFDEF USINGZ}
var
  counter64: Int64;
  counterD: double;

procedure MyZCallback64(const bot1, top1, bot2, top2: TPoint64;
  var intersectPt: TPoint64);
begin
  // when ztype = double, we would need to perform
  // a static cast from Int64 to double
  //if TypeHandle(ztype) = TypeHandle(double) then
  //  intersectPt.Z := PDouble(Pointer(@counter64))^ else
    intersectPt.Z := counter64;

  counter64 := counter64 +1;
end;

procedure MyZCallbackD(const bot1, top1, bot2, top2: TPointD;
  var intersectPt: TPointD);
begin
  // when ztype = Int64 then we need to perform
  // a static cast from double to Int64
  //if (TypeHandle(ztype) = TypeHandle(double)) then
  //  intersectPt.Z := counterD else
    intersectPt.Z := PInt64(Pointer(@counterD))^;

  counterD := counterD +1;
end;
{$ENDIF}

////////////////////////////////////////////////////////
// test procedures
////////////////////////////////////////////////////////

procedure Test_BooleanOp64;
var
  csub, cclp: CPaths64;
  csol_extern, csolo_extern: CPaths64;
begin
    // setup
    csolo_extern := nil;
    WriteLn(#10'Testing BooleanOp64');

{$IFDEF USINGZ}
    csub := MakeCPaths64([100,50,0, 10,79,0, 65,2,0, 65,98,0, 10,21,0]);
    cclp := MakeCPaths64([80,50,0, 59,79,0, 26,68,0, 26,32,0, 59,21,0]);
    SetZCallback64(MyZCallback64);
    counter64 := 10;
{$ELSE}
    csub := MakeCPaths64([100,50, 10,79, 65,2, 65,98, 10,21]);
    cclp := MakeCPaths64([80,50, 59,79, 26,68, 26,32, 59,21]);
{$ENDIF}

    // do the DLL operation
    BooleanOp64(Intersection, NonZero,
      csub, nil, cclp, csol_extern, csolo_extern);

    DisplaySVG(csub, nil, cclp, csol_extern, nil,
      'BooleanOp64.svg', 400,400, false);

    // clean up
    DisposeLocalArray64(csub);
    DisposeLocalArray64(cclp);
    DisposeExportedArray64(csol_extern);
    DisposeExportedArray64(csolo_extern);
end;

procedure Test_BooleanOpD;
var
  csub, cclp: CPathsD;
  csol_extern, csolo_extern: CPathsD;
begin
    // setup
    csolo_extern := nil;
    WriteLn(#10'Testing BooleanOpD');
{$IFDEF USINGZ}
    csub := MakeCPathsD([100,50,0, 10,79,0, 65,2,0, 65,98,0, 10,21,0]);
    cclp := MakeCPathsD([80,50,0, 59,79,0, 26,68,0, 26,32,0, 59,21,0]);
    SetZCallbackD(MyZCallbackD);
    counterD := 10;
{$ELSE}
    csub := MakeCPathsD([100,50, 10,79, 65,2, 65,98, 10,21]);
    cclp := MakeCPathsD([80,50, 59,79, 26,68, 26,32, 59,21]);
{$ENDIF}

    // do the DLL operation
    BooleanOpD(Uint8(TClipType.ctIntersection), Uint8(TFillRule.frNonZero),
      csub, nil, cclp, csol_extern, csolo_extern);

    // optionally display result on the console
    //WriteCPaths64(csol_extern);

    DisplaySVG(csub, nil, cclp, csol_extern, nil,
      'BooleanOpD.svg', 400,400, false);

    DisposeLocalArrayD(csub);
    DisposeLocalArrayD(cclp);
    DisposeExportedArrayD(csol_extern);
    DisposeExportedArrayD(csolo_extern);
end;

////////////////////////////////////////////////////////
// main entry here
////////////////////////////////////////////////////////

var
  s: string;
begin
  Randomize;

  Write(#10'Clipper2 DLL version: ');
  WriteLn(Version);

  Test_BooleanOp64;
  Test_BooleanOpD;

  WriteLn(#10'Press Enter to quit.');
  ReadLn(s);
end.
