unit Clipper.DLL.Utils;

(* *****************************************************************************
 * Author    :  Jasper Schellingerhout                                         *
 *   based on code written by Angus Johnson for the Clipper2 library.          *
 *   released under the same license                                           *
 * Date      :  12 June 2024                                                   *
 * License   :  http://www.boost.org/LICENSE_1_0.txt                           *
 **************************************************************************** *)

{$I Clipper.DLL.inc}

interface
uses
  Clipper.DLL.Data;

function MakeRandomPaths(maxWidth, maxHeight: integer; counts: TArray<Integer>;
  margin: Integer = 10): CPaths64;

procedure FillWithEllipse(path: CPath64; const Bounds: TRect64);

procedure FillWithStar(path: CPath64; const Bounds: TRect64);

// PRE: Must have 4 elements at least
// POST: First four elements filled with clockwise from top left corner
procedure FillWithRectData(path: CPath64; const Bounds: TRect64);

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
function MakeRandomPathsD(maxWidth, maxHeight: integer; counts: TArray<Integer>;
  margin: Integer = 10): CPathsD;

procedure FillWithEllipseD(path: CPathD; const Bounds: TRectD);
procedure FillWithRectDataD(path: CPathD; const Bounds: TRectD);

{$ENDIF}

implementation
uses
  System.Math;


function MakeRandomPaths(maxWidth, maxHeight: integer; counts: TArray<Integer>;
  margin: Integer = 10): CPaths64;
var
  i, j: Integer;
  Path:  CPath64;
begin
  result := TPaths64.Create(counts);

  for i := 0 to High(counts) do
  begin
    Path := Result.GetPath(i);

    for j := 0 to Path.VertexCount -1 do
    begin
      Path.GetVertex(j).SetValues(Random(maxWidth - 2 * margin) + margin,
                                  Random(maxHeight - 2 * margin) + margin);
    end;

  end;
end;



procedure FillWithEllipse(path: CPath64; const Bounds: TRect64);
var
  i: Integer;
  sinA, cosA: double;

  center_X, center_Y,
  radius_X, radius_Y,
  pre_delta_X,
  delta_X, delta_Y: Double;
begin
  if (Bounds.bottom <= Bounds.top) or (Bounds.right <= Bounds.left) then
    Exit;

  if path.VertexCount < 3 then
    exit;

  radius_X := (Bounds.right - Bounds.left)  /2;
  radius_Y := (Bounds.bottom - Bounds.top)  /2;  //assumes y-down!

  center_X := Bounds.left + radius_X;
  center_Y := Bounds.top + radius_Y;


  SinCos(2 * Pi / path.VertexCount, sinA, cosA);

  delta_x := cosA;
  delta_y := sinA;

  path.Vertex[0].SetValues(round(center_X + radius_X), round(center_Y));

  for i := 1 to path.VertexCount -1 do
  begin
    path.Vertex[i].SetValues(round(center_X + radius_X * delta_x),
                             round(center_Y + radius_Y * delta_y));

    pre_delta_X := delta_X;  //next line modifies delta_x, but needs to be used for delta_y
    delta_X  := pre_delta_X * cosA - delta_Y * sinA;
    delta_Y  := delta_Y * cosA + pre_delta_X * sinA;
  end; // rotates clockwise
end;


procedure FillWithRectData(path: CPath64; const Bounds: TRect64);
begin
  path.Vertex[0].SetValues(Bounds.Left, Bounds.Top);
  path.Vertex[1].SetValues(Bounds.Right, Bounds.Top);
  path.Vertex[2].SetValues(Bounds.Right, Bounds.Bottom);
  path.Vertex[3].SetValues(Bounds.Left, Bounds.Bottom);
end;





procedure FillWithStar(path: CPath64; const Bounds: TRect64);
var
  jump,i,j,len : integer;

  TempPath: CPath64;
begin
  Assert(Odd(path.VertexCount));
  Assert(path.VertexCount	 >= 5);

  TempPath := TPath64.Create(Path.VertexCount);
  FillWithEllipse(TempPath, Bounds);

  len := path.VertexCount;
  jump := len div 2;

  j := 0;

  for i := 0 to len -1 do
  begin
    path.Vertex[i]^ := tempPath.Vertex[j mod len]^;
    inc(j, jump);
  end;

  TempPath.Free;

end;

{$IFDEF CLIPPER_DOUBLE_COORDINATES}
function MakeRandomPathsD(maxWidth, maxHeight: integer; counts: TArray<Integer>;
  margin: Integer = 10): CPathsD;
var
  i, j: Integer;
  Path: CPathD;
begin
  result := TPathsD.Create(counts);

  for i := 0 to High(counts) do
  begin
    Path := Result.GetPath(i);

    for j := 0 to Path.Vertexcount -1 do
    begin
      Path.GetVertex(j).SetValues(Random(maxWidth - 2 * margin) + margin,
                                  Random(maxHeight - 2 * margin) + margin);
    end;

  end;
end;


procedure FillWithEllipseD(path: CPathD; const Bounds: TRectD);
var
  i: Integer;
  sinA, cosA: double;

  center_X, center_Y,
  radius_X, radius_Y,
  pre_delta_X,
  delta_X, delta_Y: Double;
begin
  if (Bounds.bottom <= Bounds.top) or (Bounds.right <= Bounds.left) then
    Exit;

  if path.VertexCount < 3 then
    exit;

  radius_X := (Bounds.right - Bounds.left)  /2;
  radius_Y := (Bounds.bottom - Bounds.top)  /2;  //assumes y-down!

  center_X := Bounds.left + radius_X;
  center_Y := Bounds.top + radius_Y;


  SinCos(2 * Pi / path.VertexCount, sinA, cosA);

  delta_x := cosA;
  delta_y := sinA;

  path.Vertex[0].SetValues(center_X + radius_X, center_Y);

  for i := 1 to path.VertexCount -1 do
  begin
    path.Vertex[i].SetValues(center_X + radius_X * delta_x,
                        center_Y + radius_Y * delta_y);

    pre_delta_X := delta_X;  //next line modifies delta_x, but needs to be used for delta_y
    delta_X  := pre_delta_X * cosA - delta_Y * sinA;
    delta_Y  := delta_Y * cosA + pre_delta_X * sinA;
  end; // rotates clockwise
end;

procedure FillWithRectDataD(path: CPathD; const Bounds: TRectD);
begin
  path.Vertex[0].SetValues(Bounds.Left, Bounds.Top);
  path.Vertex[1].SetValues(Bounds.Right, Bounds.Top);
  path.Vertex[2].SetValues(Bounds.Right, Bounds.Bottom);
  path.Vertex[3].SetValues(Bounds.Left, Bounds.Bottom);
end;
{$ENDIF}



end.
