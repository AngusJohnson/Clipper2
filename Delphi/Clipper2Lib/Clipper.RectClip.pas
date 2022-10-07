unit Clipper.RectClip;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  Clipper2 - ver.1.0.5                                            *
* Date      :  2 October 2022                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Simple FAST rectangular clipping                                *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Math, SysUtils, Clipper.Core;

function RectClip(const rect: TRect64; const path: TPath64): TPath64; overload;
function RectClip(const rect: TRect64; const paths: TPaths64): TPaths64; overload;

implementation

type
  TLocation = (locLeft, locTop, locRight, locBottom, locInside);

  TRectClip = class
  private
    fResultCnt      : integer;
    fCapacity       : integer;
    fRect           : TRect64;
    fRectPath       : TPath64;
    fFirstCrossLoc  : TLocation;
    fResult         : TPath64;
    procedure Reset;
    procedure Add(const pt: TPoint64);
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure AddCorner(prevLoc, currLoc: TLocation);
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure CheckCorners(prevLoc, currLoc: TLocation; const pt: TPoint64);
  public
    constructor Create(const rect: TRect64);
    function Execute(const path: TPath64): TPath64; overload;
    function Execute(const paths: TPaths64): TPaths64; overload;
  end;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetLocation(const rec: TRect64;
  const pt: TPoint64; out loc: TLocation): Boolean; //inline;
begin
  Result := false; // only returns false when pt on rect
  if (pt.X = rec.Left) and
    (pt.Y >= rec.Top) and (pt.Y <= rec.Bottom) then
  begin
    loc := locLeft;
    Exit; //false
  end
  else if (pt.X = rec.Right) and
    (pt.Y >= rec.Top) and (pt.Y <= rec.Bottom) then
  begin
    loc := locRight;
    Exit; //false
  end
  else if (pt.Y = rec.Top) and
    (pt.X >= rec.Left) and (pt.X <= rec.Right) then
  begin
    loc := locTop;
    Exit; //false
  end
  else if (pt.Y = rec.Bottom) and
    (pt.X >= rec.Left) and (pt.X <= rec.Right) then
  begin
    loc := locBottom;
    Exit; //false
  end
  else if (pt.X < rec.Left) then loc := locLeft
  else if (pt.X > rec.Right) then loc := locRight
  else if (pt.Y < rec.Top) then loc := locTop
  else if (pt.Y > rec.Bottom) then loc := locBottom
  else loc := locInside;
  Result := true;
end;
//------------------------------------------------------------------------------

function GetIntersection(const rectPath: TPath64;
  p, p2: TPoint64; var loc: TLocation; out ip: TPoint64): Boolean;
begin
  // gets the intersection closest to 'p'
  // when Result = false, loc will remain unchanged
  Result := false;
  case loc of
    locLeft:
      if SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true) then
        ip := GetIntersectPoint64(p, p2, rectPath[0], rectPath[3])
      else if (p.Y < rectPath[0].Y) and
        SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[0], rectPath[1]);
        loc := locTop;
      end
      else if SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[2], rectPath[3]);
        loc := locBottom;
      end
      else Exit;
    locRight:
      if SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true) then
        ip := GetIntersectPoint64(p, p2, rectPath[1], rectPath[2])
      else if (p.Y < rectPath[0].Y) and
        SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[0], rectPath[1]);
        loc := locTop;
      end
      else if SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[2], rectPath[3]);
        loc := locBottom;
      end
      else Exit;
    locTop:
      if SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true) then
        ip := GetIntersectPoint64(p, p2, rectPath[0], rectPath[1])
      else if (p.X < rectPath[0].X) and
        SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[0], rectPath[3]);
        loc := locLeft;
      end
      else if (p.X > rectPath[1].X) and
        SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[1], rectPath[2]);
        loc := locRight;
      end
      else Exit;
    locBottom:
      if SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true) then
        ip := GetIntersectPoint64(p, p2, rectPath[2], rectPath[3])
      else if (p.X < rectPath[3].X) and
        SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[0], rectPath[3]);
        loc := locLeft;
      end
      else if (p.X > rectPath[2].X) and
        SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[1], rectPath[2]);
        loc := locRight;
      end
      else Exit;

    else // loc = rInside
    begin
      if SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[0], rectPath[3]);
        loc := locLeft;
      end else if SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[0], rectPath[1]);
        loc := locTop;
      end
      else if SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[1], rectPath[2]);
        loc := locRight;
      end
      else if SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true) then
      begin
        ip := GetIntersectPoint64(p, p2, rectPath[2], rectPath[3]);
        loc := locBottom;
      end
      else Exit;
    end;
  end;
  Result := true;
end;

//------------------------------------------------------------------------------
// TRectClip class
//------------------------------------------------------------------------------

constructor TRectClip.Create(const rect: TRect64);
begin
  fRect := rect;
  fRectPath := fRect.AsPath;
end;
//------------------------------------------------------------------------------

procedure TRectClip.Reset;
begin
  fResultCnt := 0;
  fCapacity := 0;
  fResult := nil;
end;
//------------------------------------------------------------------------------

procedure TRectClip.Add(const pt: TPoint64);
begin
  if fResultCnt = fCapacity then
  begin
    inc(fCapacity, 32);
    SetLength(fResult, fCapacity);
  end;
  fResult[fResultCnt] := pt;
  inc(fResultCnt);
end;
//------------------------------------------------------------------------------

procedure TRectClip.AddCorner(prevLoc, currLoc: TLocation);
var
  i: Byte absolute prevLoc;
  j: Byte absolute currLoc;
begin
  if (i > 3) or (j > 3) or (i = j) then Exit;
  if (i + 1) mod 4 = j then
    Add(fRectPath[i]) else
    Add(fRectPath[j]);
end;
//------------------------------------------------------------------------------

procedure TRectClip.CheckCorners(prevLoc, currLoc: TLocation; const pt: TPoint64);
var
  i: Byte absolute prevLoc;
  j: Byte absolute currLoc;
begin
  if (prevLoc  = locInside) then Exit;
  if fFirstCrossLoc = locInside then fFirstCrossLoc := prevLoc;

  if (j <> (i + 2) mod 4) then
  begin
    AddCorner(prevLoc, currLoc);
  end else
  begin
    case prevLoc of
      locLeft:
        if pt.Y < fRect.Top then
        begin
          Add(fRectPath[0]);
          Add(fRectPath[1]);
        end else
        begin
          Add(fRectPath[3]);
          Add(fRectPath[2]);
        end;
      locTop:
        if pt.X < fRect.Left then
        begin
          Add(fRectPath[0]);
          Add(fRectPath[3]);
        end else
        begin
          Add(fRectPath[1]);
          Add(fRectPath[2]);
        end;
      locRight:
        if pt.Y < fRect.Top then
        begin
          Add(fRectPath[1]);
          Add(fRectPath[0]);
        end else
        begin
          Add(fRectPath[2]);
          Add(fRectPath[3]);
        end;
      locBottom:
        if pt.X < fRect.Left then
        begin
          Add(fRectPath[3]);
          Add(fRectPath[0]);
        end else
        begin
          Add(fRectPath[2]);
          Add(fRectPath[1]);
        end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TRectClip.Execute(const path: TPath64): TPath64;
var
  i,k, highI    : integer;
  pathLen       : integer;
  prevPt,ip,ip2 : TPoint64;
  loc, prevLoc  : TLocation;
  tmpRect       : TRect64;
  crossingLoc   : TLocation;
  prevCrossLoc  : TLocation;
begin
  Result := nil;
  pathLen := Length(path);
  if (pathLen < 3) or fRect.IsEmpty then Exit;
  Reset;

  i := 0;
  highI := pathLen -1;
  crossingLoc     := locInside;
  fFirstCrossLoc   := locInside;

  GetLocation(fRect, path[highI], prevLoc);
  loc := prevLoc;
  while i < pathLen do
  begin
    prevLoc := loc;
    prevCrossLoc := crossingLoc;
    case loc of
      locLeft:
        begin
          while (i < pathLen) and (path[i].X <= fRect.Left) do inc(i);
          if (i = pathLen) then break
          else if path[i].Y <= fRect.Top then loc := locTop
          else if path[i].Y >= fRect.Bottom then loc := locBottom
          else if path[i].X < fRect.Right then loc := locInside
          else loc := locRight;
        end;
      locTop:
        begin
          while (i < pathLen) and (path[i].Y <= fRect.Top) do inc(i);
          if (i = pathLen) then break
          else if path[i].X <= fRect.Left then loc := locLeft
          else if path[i].X >= fRect.Right then loc := locRight
          else if path[i].Y < fRect.Bottom then loc := locInside
          else loc := locBottom;
        end;
      locRight:
        begin
          while (i < pathLen) and (path[i].X >= fRect.Right) do inc(i);
          if (i = pathLen) then break
          else if path[i].Y <= fRect.Top then loc := locTop
          else if path[i].Y >= fRect.Bottom then loc := locBottom
          else if path[i].X > fRect.Left then loc := locInside
          else loc := locLeft;
        end;
      locBottom:
        begin
          while (i < pathLen) and (path[i].Y >= fRect.Bottom) do inc(i);
          if (i = pathLen) then break
          else if path[i].X <= fRect.Left then loc := locLeft
          else if path[i].X >= fRect.Right then loc := locRight
          else if path[i].Y > fRect.Top then loc := locInside
          else loc := locTop;
        end;
      locInside:
        begin
          while (i < pathLen) do
          begin
            if path[i].X < fRect.Left then loc := locLeft
            else if path[i].X > fRect.Right then loc := locRight
            else if path[i].Y > fRect.Bottom then loc := locBottom
            else if path[i].Y < fRect.Top then loc := locTop
            else begin Add(path[i]); inc(i); continue; end;
            break; //inner loop
          end;
          if (i = pathLen) then break;
        end;
    end;

    if i = 0 then
      prevPt := path[highI] else
      prevPt := path[i-1];

    crossingLoc := loc;
    if not GetIntersection(fRectPath, path[i],
      prevPt, crossingLoc, ip) then
    begin
      // ie remaining outside
      CheckCorners(prevLoc, crossingLoc, prevPt);
      inc(i);
      Continue;
    end;

    // we must be crossing the rect boundary to get here

    if (loc = locInside) then
    begin
      // path must be entering rect
      if fFirstCrossLoc = locInside then
        fFirstCrossLoc := crossingLoc
      else
        AddCorner(prevLoc, crossingLoc);
    end
    else if (prevLoc <> locInside) then
    begin
      // passing right through but ip will be the second intersect pt
      // so get the first intersect pt

      loc := prevLoc;
      GetIntersection(fRectPath, prevPt, path[i], loc, ip2);
      AddCorner(prevCrossLoc, loc);

      if (fFirstCrossLoc = locInside) then
        fFirstCrossLoc := loc;
      loc := crossingLoc;
      Add(ip2);
      if PointsEqual(ip, ip2) then
      begin
        // it's very likely that path[i] is on rect
        GetLocation(fRect, path[i], loc);
        AddCorner(crossingLoc, loc);
        crossingLoc := loc;
        Continue;
      end;
    end else
    begin
      loc := crossingLoc;
      if (fFirstCrossLoc = locInside) then
        fFirstCrossLoc := crossingLoc;
    end;
    Add(ip);
  end; //while i < pathLen

  if (fFirstCrossLoc = locInside) then
  begin
    tmpRect := GetBounds(path);
    if tmpRect.Contains(fRect) then
      Result := fRectPath
    else if fRect.Contains(tmpRect) then
      Result := path
    else
      Result := nil;
    Exit;
  end;

  if (loc <> locInside) then
    CheckCorners(crossingLoc, fFirstCrossLoc, path[highI]);

  if fResultCnt < 3 then Exit;

  // tidy up duplicates and collinear segments
  SetLength(Result, fResultCnt);
  k := 0;
  prevPt := fResult[fResultCnt -1];
  Result[0] := fResult[0];
  for i := 1 to fResultCnt -1 do
    if CrossProduct(prevPt, Result[k], fResult[i]) = 0 then
    begin
      Result[k] := fResult[i];
    end else
    begin
      prevPt := Result[k];
      inc(k);
      Result[k] := fResult[i];
    end;

  if k < 2 then
    Result := nil
  // and a final check for collinearity
  else if CrossProduct(Result[0], Result[k-1], Result[k]) = 0 then
    SetLength(Result, k)
  else
    SetLength(Result, k +1);
end;
//------------------------------------------------------------------------------

function TRectClip.Execute(const paths: TPaths64): TPaths64;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Execute(paths[i]);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function RectClip(const rect: TRect64; const path: TPath64): TPath64;
begin
  with TRectClip.Create(rect) do
  try
    Result := Execute(path);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRect64; const paths: TPaths64): TPaths64;
var
  i,j, len: integer;
begin
  Result := nil;
  len := Length(paths);
  if rect.IsEmpty or (len = 0) then Exit;
  SetLength(Result, len);
  j := 0;
  with TRectClip.Create(rect) do
  try
    for i := 0 to len -1 do
      if  rect.Intersects(GetBounds(paths[i])) then
      begin
        Result[j] := Execute(paths[i]);
        inc(j);
      end;
  finally
    Free;
  end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

end.

