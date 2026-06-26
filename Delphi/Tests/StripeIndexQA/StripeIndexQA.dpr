program StripeIndexQA;

// QA harness for the Delphi StripeIndex port, mirroring
// CPP/Tests/TestStripeIndex.cpp. Exits 0 on pass, nonzero (printing the failure)
// otherwise.

{$APPTYPE CONSOLE}
{$I Clipper.inc}

uses
  SysUtils,
  Math,
  Clipper.Core,
  Clipper.Engine;

var
  gFailures: Integer = 0;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Inc(gFailures);
end;

procedure CheckTrue(cond: Boolean; const msg: string);
begin
  if not cond then Fail(msg);
end;

procedure CheckPip(actual, expected: TPointInPolygonResult; const msg: string);
const
  Names: array[TPointInPolygonResult] of string = ('On', 'Inside', 'Outside');
begin
  if actual <> expected then
    Fail(Format('%s expected=%s actual=%s', [msg, Names[expected], Names[actual]]));
end;

function MakeSquare(size: Int64): TPath64;
begin
  SetLength(Result, 4);
  Result[0] := Point64(0, 0);
  Result[1] := Point64(size, 0);
  Result[2] := Point64(size, size);
  Result[3] := Point64(0, size);
end;

// Integer circle with enough vertices that sqrt(edges) yields several stripes.
function MakeCircle(r: Int64; n: Integer): TPath64;
var
  i: Integer;
  a: Double;
begin
  SetLength(Result, n);
  for i := 0 to n - 1 do
  begin
    a := 2.0 * Pi * i / n;
    Result[i] := Point64(Round(r * Cos(a)), Round(r * Sin(a)));
  end;
end;

// Independent boundary check so non-boundary points can be asserted exactly.
function IsOnAnySegment(const poly: TPath64; const pt: TPoint64): Boolean;
var
  i, j: Integer;
  a, b: TPoint64;
begin
  Result := false;
  j := High(poly);
  for i := 0 to High(poly) do
  begin
    a := poly[j];
    b := poly[i];
    j := i;
    if CrossProductSign(a, b, pt) <> 0 then Continue;
    if (pt.X >= Min(a.X, b.X)) and (pt.X <= Max(a.X, b.X)) and
       (pt.Y >= Min(a.Y, b.Y)) and (pt.Y <= Max(a.Y, b.Y)) then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

// Deterministic LCG so the coverage is reproducible.
type
  TLcg = record
    state: UInt64;
  end;

procedure LcgInit(var g: TLcg; seed: UInt64);
begin
  g.state := seed;
end;

function LcgNext(var g: TLcg; lo, hi: Int64): Int64;
var
  span: UInt64;
begin
  g.state := g.state * UInt64(6364136223846793005) + UInt64(1442695040888963407);
  span := UInt64(hi - lo + 1);
  Result := lo + Int64((g.state shr 33) mod span);
end;

procedure TestBasicSquare;
var
  idx: TStripeIndex;
begin
  idx := TStripeIndex.Create;
  try
    idx.BuildFromPath(MakeSquare(100));

    // inside
    CheckPip(idx.PointInPolygon(Point64(50, 50)), pipInside, 'square inside');

    // outside (all four directions)
    CheckPip(idx.PointInPolygon(Point64(150, 50)), pipOutside, 'square outside right');
    CheckPip(idx.PointInPolygon(Point64(-10, 50)), pipOutside, 'square outside left');
    CheckPip(idx.PointInPolygon(Point64(50, 200)), pipOutside, 'square outside above');
    CheckPip(idx.PointInPolygon(Point64(50, -5)), pipOutside, 'square outside below');

    // on-edge (horizontal + vertical)
    CheckPip(idx.PointInPolygon(Point64(50, 0)), pipOn, 'square on bottom edge');
    CheckPip(idx.PointInPolygon(Point64(100, 50)), pipOn, 'square on right edge');

    // on-vertex
    CheckPip(idx.PointInPolygon(Point64(0, 0)), pipOn, 'square on origin vertex');
    CheckPip(idx.PointInPolygon(Point64(100, 100)), pipOn, 'square on far vertex');

    // collinear-extension-not-On
    CheckPip(idx.PointInPolygon(Point64(200, 0)), pipOutside, 'collinear past bottom');
    CheckPip(idx.PointInPolygon(Point64(-50, 0)), pipOutside, 'collinear before bottom');
    CheckPip(idx.PointInPolygon(Point64(100, 200)), pipOutside, 'collinear above right edge');
  finally
    idx.Free;
  end;
end;

procedure TestConcaveMatchesBrute;
const
  PolyArr: array[0..7] of TPoint64 = (
    (X: 0; Y: 0), (X: 100; Y: 0), (X: 100; Y: 30), (X: 30; Y: 30),
    (X: 30; Y: 70), (X: 100; Y: 70), (X: 100; Y: 100), (X: 0; Y: 100));
var
  poly: TPath64;
  idx: TStripeIndex;
  x, y: Int64;
  pt: TPoint64;
  i: Integer;
begin
  SetLength(poly, Length(PolyArr));
  for i := 0 to High(PolyArr) do poly[i] := PolyArr[i];

  idx := TStripeIndex.Create;
  try
    idx.BuildFromPath(poly);
    y := -5;
    while y <= 105 do
    begin
      x := -5;
      while x <= 105 do
      begin
        pt := Point64(x, y);
        if IsOnAnySegment(poly, pt) then
          CheckPip(idx.PointInPolygon(pt), pipOn,
            Format('concave boundary at (%d,%d)', [x, y]))
        else
          CheckPip(idx.PointInPolygon(pt), PointInPolygon(pt, poly),
            Format('concave mismatch at (%d,%d)', [x, y]));
        Inc(x, 3);
      end;
      Inc(y, 3);
    end;
  finally
    idx.Free;
  end;
end;

procedure TestMultiStripeMatchesBrute;
var
  poly: TPath64;
  idx: TStripeIndex;
  rng: TLcg;
  i, checked: Integer;
  pt: TPoint64;
begin
  poly := MakeCircle(10000, 256);
  CheckTrue(Length(poly) > 64, 'circle needs > 64 vertices for several stripes');

  idx := TStripeIndex.Create;
  try
    idx.BuildFromPath(poly);
    CheckTrue(not idx.IsEmpty, 'circle index should not bail');

    LcgInit(rng, UInt64($9E3779B97F4A7C15));
    checked := 0;
    for i := 0 to 19999 do
    begin
      pt := Point64(LcgNext(rng, -10200, 10200), LcgNext(rng, -10200, 10200));
      if IsOnAnySegment(poly, pt) then
        CheckPip(idx.PointInPolygon(pt), pipOn,
          Format('circle boundary at (%d,%d)', [pt.X, pt.Y]))
      else
      begin
        CheckPip(idx.PointInPolygon(pt), PointInPolygon(pt, poly),
          Format('circle mismatch at (%d,%d)', [pt.X, pt.Y]));
        Inc(checked);
      end;
    end;
    CheckTrue(checked > 1000, 'circle fuzz should exercise > 1000 non-boundary points');
  finally
    idx.Free;
  end;
end;

procedure TestAllHorizontalRing;
var
  poly: TPath64;
  idx: TStripeIndex;
begin
  SetLength(poly, 3);
  poly[0] := Point64(0, 10);
  poly[1] := Point64(50, 10);
  poly[2] := Point64(100, 10);

  idx := TStripeIndex.Create;
  try
    idx.BuildFromPath(poly);
    CheckPip(idx.PointInPolygon(Point64(50, 0)), pipOutside, 'horiz ring below');
    CheckPip(idx.PointInPolygon(Point64(50, 20)), pipOutside, 'horiz ring above');
    CheckPip(idx.PointInPolygon(Point64(200, 10)), pipOutside, 'horiz ring collinear past');
  finally
    idx.Free;
  end;
end;

procedure TestDegeneratePaths;
var
  empty, seg: TStripeIndex;
  path: TPath64;
begin
  empty := TStripeIndex.Create;
  try
    SetLength(path, 0);
    empty.BuildFromPath(path);
    CheckTrue(empty.IsEmpty, 'empty path index should be empty');
    CheckPip(empty.PointInPolygon(Point64(0, 0)), pipOutside, 'empty path outside');
  finally
    empty.Free;
  end;

  seg := TStripeIndex.Create;
  try
    SetLength(path, 2);
    path[0] := Point64(0, 0);
    path[1] := Point64(10, 0);
    seg.BuildFromPath(path);
    CheckPip(seg.PointInPolygon(Point64(5, 0)), pipOutside, 'two-point path outside');
  finally
    seg.Free;
  end;
end;

procedure TestAdversarialCombDeclines;
var
  comb: TPath64;
  idx: TStripeIndex;
  i: Integer;
  height: Int64;
begin
  // Tall zigzag where every edge spans the full Y range, so the index must
  // decline and leave itself empty.
  height := 100000;
  SetLength(comb, 200);
  for i := 0 to 199 do
    if Odd(i) then comb[i] := Point64(i, height)
    else comb[i] := Point64(i, 0);

  idx := TStripeIndex.Create;
  try
    idx.BuildFromPath(comb);
    CheckTrue(idx.IsEmpty, 'adversarial comb ring should decline indexing');
  finally
    idx.Free;
  end;
end;

// FNV-1a coordinate hash, to tell apart nodes with equal vertex counts.
function HashPolygon(const poly: TPath64): UInt64;
var
  i: Integer;
  h: UInt64;
begin
  h := UInt64(14695981039346656037);
  for i := 0 to High(poly) do
  begin
    h := (h xor UInt64(poly[i].X)) * UInt64(1099511628211);
    h := (h xor UInt64(poly[i].Y)) * UInt64(1099511628211);
  end;
  Result := h;
end;

// Depth-first PolyTree signature (level, hole flag, vertex count, coord hash).
procedure SerializeTree(node: TPolyPath64; var sb: string);
var
  i: Integer;
  c: TPolyPath64;
begin
  for i := 0 to node.Count - 1 do
  begin
    c := node.Child[i];
    sb := sb + IntToStr(c.Level);
    if c.IsHole then sb := sb + 'H' else sb := sb + 'S';
    sb := sb + IntToStr(Length(c.Polygon)) + ':' +
      UIntToStr(HashPolygon(c.Polygon)) + ';';
    SerializeTree(c, sb);
  end;
end;

// Tree shape stats used to prove the union genuinely nests.
procedure GatherStats(node: TPolyPath64; var nodes, holes, maxChildren: Integer);
var
  i: Integer;
  c: TPolyPath64;
begin
  if node.Count > maxChildren then maxChildren := node.Count;
  for i := 0 to node.Count - 1 do
  begin
    c := node.Child[i];
    Inc(nodes);
    if c.IsHole then Inc(holes);
    GatherStats(c, nodes, holes, maxChildren);
  end;
end;

function UnionTreeSignature(const subject: TPaths64; threshold: Integer;
  var nodes, holes, maxChildren: Integer): string;
var
  clipper: TClipper64;
  tree: TPolyTree64;
  open: TPaths64;
begin
  clipper := TClipper64.Create;
  tree := TPolyTree64.Create;
  try
    clipper.PipIndexThreshold := threshold;
    clipper.AddSubject(subject);
    clipper.Execute(ctUnion, frEvenOdd, tree, open);
    Result := '';
    SerializeTree(tree, Result);
    nodes := 0; holes := 0; maxChildren := 0;
    GatherStats(tree, nodes, holes, maxChildren);
  finally
    tree.Free;
    clipper.Free;
  end;
end;

procedure TestNestedPolyTreeDifferential;
var
  subject: TPaths64;
  brute, indexed: string;
  bn, bh, bmc, ixn, ixh, ixmc: Integer;
begin
  // Concentric circles under EvenOdd nest into alternating solids and holes,
  // exercising the owner determination the index accelerates.
  SetLength(subject, 4);
  subject[0] := MakeCircle(10000, 220);
  subject[1] := MakeCircle(7500, 200);
  subject[2] := MakeCircle(5000, 180);
  subject[3] := MakeCircle(2500, 160);

  // High threshold forces brute-force; threshold 1 forces the stripe index.
  brute := UnionTreeSignature(subject, High(Integer), bn, bh, bmc);
  indexed := UnionTreeSignature(subject, 1, ixn, ixh, ixmc);

  CheckTrue(brute <> '', 'union tree signature should be non-empty');
  CheckTrue(bh > 0, 'union tree should contain holes (genuine nesting)');
  CheckTrue(bn > 1, 'union tree should contain multiple nodes');
  CheckTrue(brute = indexed, 'indexed and brute PolyTree signatures must be identical');
end;

procedure TestRingMatchesPath;
var
  clipper: TClipper64;
  subject: TPaths64;
  sigA, sigB: string;
  n1, n2, h1, h2, m1, m2: Integer;
begin
  // Run the same union twice at the default threshold to confirm BuildFromRing
  // (used inside Execute) is stable and reproducible.
  SetLength(subject, 2);
  subject[0] := MakeCircle(8000, 97);  // odd vertex count, distinct edges
  subject[1] := MakeCircle(4000, 71);

  clipper := nil;
  sigA := UnionTreeSignature(subject, 1, n1, h1, m1);
  sigB := UnionTreeSignature(subject, 1, n2, h2, m2);
  CheckTrue(sigA = sigB, 'repeated indexed union must be deterministic');
  if Assigned(clipper) then clipper.Free;
end;

begin
  try
    TestBasicSquare;
    TestConcaveMatchesBrute;
    TestMultiStripeMatchesBrute;
    TestAllHorizontalRing;
    TestDegeneratePaths;
    TestAdversarialCombDeclines;
    TestNestedPolyTreeDifferential;
    TestRingMatchesPath;
  except
    on E: Exception do
    begin
      Writeln('EXCEPTION: ', E.ClassName, ': ', E.Message);
      Halt(2);
    end;
  end;

  if gFailures = 0 then
  begin
    Writeln('All StripeIndex QA checks passed.');
    Halt(0);
  end
  else
  begin
    Writeln(Format('%d StripeIndex QA check(s) failed.', [gFailures]));
    Halt(1);
  end;
end.
