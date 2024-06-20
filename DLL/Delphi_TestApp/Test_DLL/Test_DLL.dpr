program Test_DLL;

// Make sure that the Clipper2 DLLS are in either
// the OS Path or in the application's folder.
{$I Clipper.DLL.inc}

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  Math,
  ShellApi,
  SysUtils,
  Clipper.DLL in '..\..\Delphi_interface\Clipper.DLL.pas',
  Clipper.DLL.SVG in '..\..\Delphi_interface\SVG\Clipper.DLL.SVG.pas',
  Clipper.DLL.Utils in '..\..\Delphi_interface\Utils\Clipper.DLL.Utils.pas';

////////////////////////////////////////////////////////
// miscellaneous functions
////////////////////////////////////////////////////////




procedure ShowSvgImage(const svgFilename: string);
begin
  ShellExecute(0, 'open',PChar(svgFilename), nil, nil, SW_SHOW);
end;

const
  displayWidth = 600;
  displayHeight = 400;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure DisplaySVGD(const sub, subo, clp, sol, solo: CPathsD;
  const svgName: string; width: integer = displayWidth;
  height: integer = displayHeight); overload;
var
  svg: TSvgWriter;
begin
  svg := TSvgWriter.Create(TFillRule.NonZero);
  try
    AddSubject(svg, sub);
    AddOpenSubject(svg, subo);
    AddClip(svg, clp);
    AddSolution(svg, sol);
    AddOpenSolution(svg, solo);
    SaveSvg(svg, svgName, width, height);
    ShowSvgImage(svgName);
  finally
    svg.Free;
  end;
end;
{$ENDIF}

procedure DisplaySVG64(const sub, subo, clp, sol, solo: CPaths64;
  const svgName: string; width: integer = displayWidth;
  height: integer = displayHeight); overload;
var
  svg: TSvgWriter;
begin
  svg := TSvgWriter.Create(TFillRule.NonZero);
  try
    AddSubject(svg, sub);
    AddOpenSubject(svg, subo);
    AddClip(svg, clp);

    AddSolution(svg, sol);
    AddOpenSolution(svg, solo);
    SaveSvg(svg, svgName, width, height);
    ShowSvgImage(svgName);
  finally
    svg.Free;
  end;
end;

procedure DisplaySVG64T(const sub, subo, clp: CPaths64; const sol: CPolyTree64; const solo: CPaths64;
  const svgName: string; width: integer = displayWidth;
  height: integer = displayHeight);  overload;
var
  svg: TSvgWriter;
begin
  svg := TSvgWriter.Create(TFillRule.NonZero);
  try
    AddSubject(svg, sub);
    AddOpenSubject(svg, subo);
    AddClip(svg, clp);

    AddSolution(svg, sol);
    AddOpenSolution(svg, solo);
    SaveSvg(svg, svgName, width, height);
    ShowSvgImage(svgName);
  finally
    svg.Free;
  end;
end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure DisplaySVGDT(const sub, subo, clp: CPathsD; const sol: CPolyTreeD; const solo: CPathsD;
  const svgName: string; width: integer = displayWidth;
  height: integer = displayHeight);  overload;
var
  svg: TSvgWriter;
begin
  svg := TSvgWriter.Create(TFillRule.NonZero);
  try
    AddSubject(svg, sub);
    AddOpenSubject(svg, subo);
    AddClip(svg, clp);

    AddSolution(svg, sol);
    AddOpenSolution(svg, solo);
    SaveSvg(svg, svgName, width, height);
    ShowSvgImage(svgName);
  finally
    svg.Free;
  end;
end;
{$ENDIF}

////////////////////////////////////////////////////////
// test procedures
////////////////////////////////////////////////////////

procedure Test_Version();
begin
  Write(#10'Clipper2 DLL version: ');
  WriteLn(Version);
end;




procedure Test_BooleanOp64(edgeCnt: integer);
var
  csub_local, cclp_local: CPaths64;
  csol_extern, csolo_extern: CPaths64;
begin
    // setup
    csolo_extern := nil;
    WriteLn(#10'Testing BooleanOp64');

    csub_local := MakeRandomPaths(displayWidth, displayHeight, [edgeCnt]);
    cclp_local := MakeRandomPaths(displayWidth, displayHeight, [edgeCnt]);
    try

      // do the DLL operation
      BooleanOp64(TClipType.Intersection, TFillRule.NonZero,
        csub_local, nil, cclp_local, csol_extern, csolo_extern);

      DisplaySVG64(csub_local, nil, cclp_local,  csol_extern, nil, 'BooleanOp64.svg');

      // clean up
     DisposeExternalCPaths64(csol_extern);
     DisposeExternalCPaths64(csolo_extern);
    finally
       csub_local.Free;
       cclp_local.Free;
    end;
end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure Test_BooleanOpD(edgeCnt: integer);
var
  csub_local, cclp_local: CPathsD;
  csol_extern, csolo_extern: CPathsD;
begin
    // setup
    csolo_extern := nil;
    WriteLn(#10'Testing BooleanOpD');

    csub_local := MakeRandomPathsD(displayWidth, displayHeight, [edgeCnt]);
    cclp_local := MakeRandomPathsD(displayWidth, displayHeight, [edgeCnt]);
    try
      // do the DLL operation
      BooleanOpD(TClipType.Intersection,  TFillRule.NonZero,
        csub_local, nil, cclp_local, csol_extern, csolo_extern);

      DisplaySVGD(csub_local, nil, cclp_local, csol_extern, nil, 'BooleanOpD.svg');

      DisposeExternalCPathsD(csol_extern);
      DisposeExternalCPathsD(csolo_extern);

    finally
      csub_local.Free;
      cclp_local.Free;
    end;
end;
{$ENDIF}

procedure Test_BooleanOp_Polytree64(edgeCnt: integer);
var
  csub_local, cclp_local: CPaths64;
  csol_extern: CPolyTree64;
  csol_open_extern: CPaths64;
begin
    // setup
    WriteLn(#10'Testing BooleanOp_PolyTree64');
    csub_local := MakeRandomPaths(displayWidth, displayHeight, [edgeCnt]);
    cclp_local := MakeRandomPaths(displayWidth, displayHeight, [edgeCnt]);
    try
      // do the DLL operation
      BooleanOp_PolyTree64(TClipType.Intersection, TFillRule.NonZero,
        csub_local, nil, cclp_local, csol_extern, csol_open_extern);

      DisplaySVG64T(csub_local, nil, cclp_local, csol_extern, nil, 'BooleanOp_PolyTree64.svg');

      DisposeExternalCPolyTree64(csol_extern);
      DisposeExternalCPaths64(csol_open_extern);

    finally
      csub_local.Free;
      cclp_local.Free;
    end;


end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
procedure Test_BooleanOp_PolytreeD(edgeCnt: integer);
var
  csub_local, cclp_local: CPathsD;
  csol_extern: CPolyTreeD;
  csol_open_extern: CPathsD;
begin
    // setup
    WriteLn(#10'Testing BooleanOp_PolyTreeD');
    csub_local := MakeRandomPathsD(displayWidth, displayHeight, [edgeCnt]);
    cclp_local := MakeRandomPathsD(displayWidth, displayHeight, [edgeCnt]);
    try
      // do the DLL operation
      BooleanOp_PolyTreeD(TClipType.Intersection, TFillRule.NonZero,
        csub_local, nil, cclp_local, csol_extern, csol_open_extern);

      DisplaySVGDT(csub_local, nil, cclp_local, csol_extern, nil, 'BooleanOp_PolyTreeD.svg');

      DisposeExternalCPolyTreeD(csol_extern);
      DisposeExternalCPathsD(csol_open_extern);

    finally
      csub_local.Free;
      cclp_local.Free;
     end;

    // finally, display and clean up

end;


procedure Test_InflatePathsD(edgeCnt: integer; delta: double);
var
  csub_local: CPathsD;
  csol_extern: CPathsD;
  csolo_extern: CPathsD;
begin
    // setup
    WriteLn(#10'Testing InflatePaths64');
    csub_local := MakeRandomPathsD(displayWidth, displayHeight, [edgeCnt]);
    try
      // and because offsetting self-intersecting paths is unpredictable
      // we must remove self-intersection via a union operation
      BooleanOpD(TClipType.Union, TFillRule.NonZero, csub_local, nil, nil,
        csol_extern, csolo_extern);

      // now do the DLL operation
      csol_extern := InflatePathsD(csol_extern, delta,  TJoinType.Round, TEndType.Polygon, 2, 4);
      DisplaySVGD(csub_local, nil, nil, csol_extern, nil, 'InflatePathsD.svg');

       DisposeExternalCPathsD(csol_extern);
      DisposeExternalCPathsD(csolo_extern);
    finally
      csub_local.Free;
    end;

 end;

procedure Test_RectClipD(edgeCount: integer);
var
  rec_margin: Integer;
  csub_local: CPathsD;
  csol_extern: CPathsD;

  rec: TRectD;
  crec_local: CPathsD;
  crec_path: CPathD;
begin
  WriteLn(#10'Testing RectClipD:');

  rec_margin := Min(displayWidth,displayHeight) div 4;
  rec.Left := rec_margin;
  rec.Top := rec_margin;
  rec.Right := displayWidth - rec_margin;
  rec.Bottom := displayHeight -rec_margin;

  crec_local := TPathsD.Create([4]);
  crec_path := crec_local.FirstPath();

  crec_path.Vertex[0].SetValues(rec.Left, rec.Top);
  crec_path.Vertex[1].SetValues(rec.Right, rec.Top);
  crec_path.Vertex[2].SetValues(rec.Right, rec.Bottom);
  crec_path.Vertex[3].SetValues(rec.Left, rec.Bottom);

  csub_local := MakeRandomPathsD(displayWidth, displayHeight, [edgeCount]);
  csol_extern := RectClipD(rec, csub_local, 2);


  DisplaySVGD(csub_local, nil, crec_local, csol_extern,
    nil, 'RectClipD.svg', displayWidth,displayHeight);


  crec_local.Free;
  DisposeExternalCPathsD(csol_extern);
  csub_local.Free;

end;

{$ENDIF}

procedure Test_RectClipLines64(edgeCnt: integer);
var
  csub_local: CPaths64;
  csolo_extern: CPaths64;
  rec: TRect64;
  crec_local: CPaths64;
  crec_path: CPath64;
begin
    // setup
    WriteLn(#10'Testing RectClipLines64:');

    csub_local := MakeRandomPaths(displayWidth, displayHeight, [edgeCnt]);

    rec.Left := 80;
    rec.Top := 80;
    rec.Right := displayWidth - 80;
    rec.Bottom := displayHeight -80;

    crec_local := TPaths64.Create([4]);
    crec_path := crec_local.FirstPath();

    crec_path.Vertex[0].SetValues(rec.Left, rec.Top);
    crec_path.Vertex[1].SetValues(rec.Right, rec.Top);
    crec_path.Vertex[2].SetValues(rec.Right, rec.Bottom);
    crec_path.Vertex[3].SetValues(rec.Left, rec.Bottom);



    // do the DLL operation
    csolo_extern := RectClipLines64(rec, csub_local);

    DisplaySVG64(nil, csub_local, crec_local, nil, csolo_extern, 'RectClipLines64.svg');

    crec_local.Free;

    csub_local.Free;
    DisposeExternalCPaths64(csolo_extern);
end;

////////////////////////////////////////////////////////
// main entry here
////////////////////////////////////////////////////////

//var
//  s: string;
begin
  Randomize;
  Test_Version();
  Test_BooleanOp64(25);
  Test_BooleanOp_Polytree64(10);
  Test_RectClipLines64(25);

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
  Test_BooleanOpD(25);
  Test_BooleanOp_PolytreeD(25);
  Test_InflatePathsD(20, -10); // edgeCount, offsetDist
  Test_RectClipD(7);
{$ENDIF}

//  WriteLn(#10'Press Enter to quit.');
//  ReadLn(s);
end.
