program Test1_DLL;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  Math,
  SysUtils,
  Clipper2Dll.Core in '..\Clipper2Dll.Core.pas',
  Clipper2Dll.SVG in '..\Clipper2Dll.SVG.pas';

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

procedure TestTriangulate;
var
  csub: array of Int64;
  csol_extern: CPaths64;
begin
  csub := [
    0, 3,
    // outer path
    248, 0,
    5685, 2435, 5720, 2510, 5700, 2600, 5570, 2770, 5380, 2920, 5210, 2990, 5110, 2990, 5050, 2970, 4970,
    2930, 4850, 2895, 4770, 2920, 4700, 3040, 4570, 3430, 4440, 3910, 4370, 4140, 4270, 4330, 4100, 4500,
    3870, 4660, 3700, 4820, 3660, 4910, 3680, 5010, 3730, 5070, 3790, 5100, 3960, 5110, 4140, 5070, 4300,
    5000, 4520, 4840, 4730, 4600, 5110, 4010, 5390, 3550, 5470, 3485, 5530, 3495, 5560, 3530, 5580, 3710,
    5540, 3900, 5340, 4310, 5080, 4690, 4850, 4990, 4740, 5110, 4570, 5220, 4160, 5420, 3730, 5560, 3400,
    5620, 3060, 5610, 2710, 5570, 2380, 5500, 2070, 5370, 1770, 5150, 1520, 4880, 1060, 4290, 670, 3930,
    490, 3740, 370, 3520, 270, 3150, 270, 2980, 310, 2920, 370, 2880, 450, 2890, 510, 2940, 580, 3130,
    640, 3370, 730, 3560, 1040, 3930, 1210, 4050, 1300, 4060, 1370, 4020, 1460, 3880, 1440, 3730, 1340,
    3570, 1190, 3390, 830, 3030, 690, 2850, 590, 2660, 520, 2460, 510, 2370, 560, 2310, 630, 2310, 690,
    2360, 780, 2570, 880, 2760, 940, 2810, 1000, 2810, 1050, 2760, 1070, 2710, 1040, 2570, 820, 2240, 600,
    1900, 570, 1770, 590, 1710, 640, 1670, 730, 1650, 800, 1680, 930, 1840, 1140, 2290, 1220, 2480, 1260,
    2690, 1360, 3110, 1710, 4010, 1780, 4270, 1850, 4560, 1950, 4790, 2030, 4840, 2120, 4830, 2220, 4780,
    2290, 4720, 2340, 4540, 2300, 4310, 2200, 4050, 1930, 3480, 1800, 3210, 1720, 2950, 1630, 2720, 1500,
    2470, 1220, 1960, 1120, 1730, 1070, 1510, 1100, 1340, 1150, 1270, 1240, 1210, 1340, 1200, 1430, 1260,
    1500, 1390, 1530, 1580, 1560, 1910, 1620, 2280, 1680, 2430, 1750, 2560, 1850, 2630, 1980, 2640, 2090,
    2590, 2160, 2480, 2180, 2340, 2170, 2170, 2110, 1800, 2050, 1450, 2020, 1280, 1950, 1090, 1770, 700,
    1700, 530, 1680, 370, 1720, 260, 1820, 190, 1930, 200, 2000, 290, 2080, 660, 2150, 1050, 2230, 1180,
    2350, 1230, 2430, 1200, 2480, 1110, 2530, 860, 2580, 620, 2630, 560, 2710, 550, 2830, 630, 2870, 760,
    2860, 930, 2800, 1120, 2620, 1540, 2500, 1930, 2360, 2510, 2300, 2810, 2270, 3100, 2250, 3580, 2290,
    3790, 2340, 3850, 2420, 3890, 2500, 3880, 2560, 3840, 2650, 3670, 2760, 3250, 2830, 2950, 2870, 2640,
    2940, 2030, 3110, 1270, 3100, 1020, 3060, 740, 3060, 510, 3100, 430, 3190, 380, 3320, 380, 3390, 430,
    3430, 510, 3440, 610, 3440, 840, 3460, 930, 3520, 990, 3630, 1020, 3750, 980, 3830, 890, 3870, 770, 3920,
    460, 3950, 320, 4000, 210, 4110, 160, 4270, 170, 4340, 220, 4370, 300, 4330, 540, 4230, 810, 4170, 1070,
    4130, 1550, 4160, 1750, 4210, 1820, 4280, 1860, 4360, 1860, 4420, 1820, 4510, 1680, 4570, 1470, 4610,
    1230, 4650, 980, 4710, 780, 4790, 650, 4850, 630, 4930, 635, 5040, 700, 5090, 810, 5090, 950, 5060, 1110,
    4950, 1460, 4810, 1770, 4690, 1920, 4510, 2050, 4380, 2170, 4360, 2240, 4380, 2300, 4430, 2350, 4500,
    2370, 4670, 2320, 5010, 2110, 5150, 1950, 5270, 1730, 5390, 1570, 5450, 1550, 5520, 1560, 5570, 1610,
    5590, 1680, 5550, 1800, 5310, 2110, 5050, 2440, 5000, 2590, 5010, 2660, 5050, 2720, 5120, 2740, 5200,
    2710, 5380, 2570, 5560, 2420, 5630, 2410,
    // hole 1
    28, 0,
    3190, 2270, 3150, 2840, 3010, 3410, 2860, 3850, 2730, 4200, 2670, 4500, 2710, 4580, 2810, 4640, 2950,
    4650, 3060, 4630, 3200, 4520, 3300, 4380, 3360, 4200, 3400, 4010, 3450, 3600, 3530, 3210, 3770, 2730,
    3870, 2480, 3930, 2230, 3960, 2000, 3960, 1750, 3910, 1540, 3850, 1460, 3770, 1410, 3640, 1410, 3530,
    1490, 3340, 1740, 3230, 1990,
    // hole 2
    20, 0,
    4240, 2700, 4050, 2800, 3900, 2970, 3810, 3160, 3710, 3580, 3550, 4080, 3530, 4310, 3560, 4390, 3640,
    4430, 3870, 4390, 4040, 4290, 4150, 4130, 4230, 3940, 4320, 3490, 4380, 3260, 4460, 3050, 4540, 2880,
    4550, 2770, 4510, 2700, 4440, 2670
  ];
  csol_extern := Triangulate64(@csub[0], true);
  DisplaySVG_MultiColor(csol_extern, 'Triangulate.svg', 400,400);

  // nb: csub here is an dynamic array with
  // memory that is automatically freed.

  DisposeExportedArray64(csol_extern);
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
  TestTriangulate;

  WriteLn(#10'Press Enter to quit.');
  ReadLn(s);
end.
