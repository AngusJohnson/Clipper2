unit Clipper.RectClip;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  9 November 2022                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  FAST rectangular clipping                                       *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Classes, Math, SysUtils, Clipper.Core;

type
  TLocation = (locLeft, locTop, locRight, locBottom, locInside);

  TRectClip = class
  protected
    fResultCnt      : integer;
    fCapacity       : integer;
    fRect           : TRect64;
    fRectPath       : TPath64;
    fRectMidPt      : TPoint64;
    fFirstCrossLoc  : TLocation;
    fResult         : TPath64;
    fStartLocs      : TList;
    procedure Reset;
    procedure Add(const pt: TPoint64);
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure AddCorner(prev, curr: TLocation); overload;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure AddCorner(var loc: TLocation; isClockwise: Boolean); overload;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure GetNextLocation(const path: TPath64;
      var loc: TLocation; var i: integer; highI: integer);
  public
    constructor Create(const rect: TRect64);
    destructor Destroy; override;
    function Execute(const path: TPath64): TPath64;
  end;

  TRectClipLines = class(TRectClip)
  private
    function GetCurrentPath: TPath64;
  public
    function Execute(const path: TPath64): TPaths64;
  end;

implementation

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetLocation(const rec: TRect64; const pt: TPoint64;
  out loc: TLocation): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
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
  const p, p2: TPoint64; var loc: TLocation; out ip: TPoint64): Boolean;
begin
  // gets the intersection closest to 'p'
  // when Result = false, loc will remain unchanged
  Result := false;
  case loc of
    locLeft:
      if SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true) then
        GetIntersectPoint(p, p2, rectPath[0], rectPath[3], ip)
      else if (p.Y < rectPath[0].Y) and
        SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[0], rectPath[1], ip);
        loc := locTop;
      end
      else if SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[2], rectPath[3], ip);
        loc := locBottom;
      end
      else Exit;
    locRight:
      if SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true) then
        GetIntersectPoint(p, p2, rectPath[1], rectPath[2], ip)
      else if (p.Y < rectPath[0].Y) and
        SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[0], rectPath[1], ip);
        loc := locTop;
      end
      else if SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[2], rectPath[3], ip);
        loc := locBottom;
      end
      else Exit;
    locTop:
      if SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true) then
        GetIntersectPoint(p, p2, rectPath[0], rectPath[1], ip)
      else if (p.X < rectPath[0].X) and
        SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[0], rectPath[3], ip);
        loc := locLeft;
      end
      else if (p.X > rectPath[1].X) and
        SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[1], rectPath[2], ip);
        loc := locRight;
      end
      else Exit;
    locBottom:
      if SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true) then
        GetIntersectPoint(p, p2, rectPath[2], rectPath[3], ip)
      else if (p.X < rectPath[3].X) and
        SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[0], rectPath[3], ip);
        loc := locLeft;
      end
      else if (p.X > rectPath[2].X) and
        SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[1], rectPath[2], ip);
        loc := locRight;
      end
      else Exit;

    else // loc = rInside
    begin
      if SegmentsIntersect(p, p2, rectPath[0], rectPath[3], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[0], rectPath[3], ip);
        loc := locLeft;
      end else if SegmentsIntersect(p, p2, rectPath[0], rectPath[1], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[0], rectPath[1], ip);
        loc := locTop;
      end
      else if SegmentsIntersect(p, p2, rectPath[1], rectPath[2], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[1], rectPath[2], ip);
        loc := locRight;
      end
      else if SegmentsIntersect(p, p2, rectPath[2], rectPath[3], true) then
      begin
        GetIntersectPoint(p, p2, rectPath[2], rectPath[3], ip);
        loc := locBottom;
      end
      else Exit;
    end;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------


function AreOpposites(prev, curr: TLocation): Boolean;
{$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := Abs(Ord(prev) - Ord(curr)) = 2;
end;
//------------------------------------------------------------------------------

function HeadingClockwise(prev, curr: TLocation): Boolean;
{$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (Ord(prev) + 1) mod 4 = Ord(curr);
end;
//------------------------------------------------------------------------------

function GetAdjacentLocation(loc: TLocation; isClockwise: Boolean): TLocation;
{$IFDEF INLINING} inline; {$ENDIF}
var
  delta: integer;
begin
  if isClockwise then delta := 1 else delta := 3;
  Result := TLocation((Ord(loc) + delta) mod 4);
end;
//------------------------------------------------------------------------------

function IsClockwise(prev, curr: TLocation;
  const prevPt, currPt, rectMidPt: TPoint64): Boolean;
{$IFDEF INLINING} inline; {$ENDIF}
begin
  if AreOpposites(prev, curr) then
    Result := CrossProduct(prevPt, rectMidPt, currPt) < 0 else
    Result := HeadingClockwise(prev, curr);
end;

//------------------------------------------------------------------------------
// TRectClip class
//------------------------------------------------------------------------------

constructor TRectClip.Create(const rect: TRect64);
begin
  fRect := rect;
  fRectPath := fRect.AsPath;
  fRectMidPt := rect.MidPoint;
  fStartLocs := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TRectClip.Destroy;
begin
  fStartLocs.Free;
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

procedure TRectClip.AddCorner(prev, curr: TLocation);
begin
  if (HeadingClockwise(prev, curr)) then
    Add(fRectPath[Ord(prev)]) else
    Add(fRectPath[Ord(curr)]);
end;
//------------------------------------------------------------------------------

procedure TRectClip.AddCorner(var loc: TLocation; isClockwise: Boolean);
begin
  if (isClockwise) then
  begin
    Add(fRectPath[Ord(loc)]);
    loc := GetAdjacentLocation(loc, true);
  end else
  begin
    loc := GetAdjacentLocation(loc, false);
    Add(fRectPath[Ord(loc)]);
  end;
end;
//------------------------------------------------------------------------------

procedure TRectClip.GetNextLocation(const path: TPath64;
  var loc: TLocation; var i: integer; highI: integer);
begin
  case loc of
    locLeft:
      begin
        while (i <= highI) and (path[i].X <= fRect.Left) do inc(i);
        if (i > highI) then Exit;
        if path[i].X >= fRect.Right then loc := locRight
        else if path[i].Y <= fRect.Top then loc := locTop
        else if path[i].Y >= fRect.Bottom then loc := locBottom
        else loc := locInside;
      end;
    locTop:
      begin
        while (i <= highI) and (path[i].Y <= fRect.Top) do inc(i);
        if (i > highI) then Exit;
        if path[i].Y >= fRect.Bottom then loc := locBottom
        else if path[i].X <= fRect.Left then loc := locLeft
        else if path[i].X >= fRect.Right then loc := locRight
        else loc := locInside;
      end;
    locRight:
      begin
        while (i <= highI) and (path[i].X >= fRect.Right) do inc(i);
        if (i > highI) then Exit;
        if path[i].X <= fRect.Left then loc := locLeft
        else if path[i].Y <= fRect.Top then loc := locTop
        else if path[i].Y >= fRect.Bottom then loc := locBottom
        else loc := locInside;
      end;
    locBottom:
      begin
        while (i <= highI) and (path[i].Y >= fRect.Bottom) do inc(i);
        if (i > highI) then Exit;
        if path[i].Y <= fRect.Top then loc := locTop
        else if path[i].X <= fRect.Left then loc := locLeft
        else if path[i].X >= fRect.Right then loc := locRight
        else loc := locInside;
      end;
    locInside:
      begin
        while (i <= highI) do
        begin
          if path[i].X < fRect.Left then loc := locLeft
          else if path[i].X > fRect.Right then loc := locRight
          else if path[i].Y > fRect.Bottom then loc := locBottom
          else if path[i].Y < fRect.Top then loc := locTop
          else begin Add(path[i]); inc(i); continue; end;
          break; //inner loop
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

function Path1ContainsPath2(const path1, path2: TPath64): TPointInPolygonResult;
var
  i: integer;
begin
  Result := pipOutside;
  for i := 0 to High(path2) do
  begin
    Result := PointInPolygon(path2[i], path1);
    if Result <> pipOn then break;
  end;
end;
//------------------------------------------------------------------------------

function TRectClip.Execute(const path: TPath64): TPath64;
var
  i,k, highI    : integer;
  prevPt,ip,ip2 : TPoint64;
  loc, prev     : TLocation;
  loc2          : TLocation;
  startingLoc   : TLocation;
  crossingLoc   : TLocation;
  prevCrossLoc  : TLocation;
  tmpRect       : TRect64;
  isClockw      : Boolean;
begin
  Result := nil;
  if (Length(path) < 3) or fRect.IsEmpty then Exit;
  Reset;

  i := 0;
  fStartLocs.Clear;
  highI := Length(path) -1;
  crossingLoc     := locInside;
  fFirstCrossLoc   := locInside;

  if not GetLocation(fRect, path[highI], loc) then
  begin
    i := highI - 1;
    while (i >= 0) and
      not GetLocation(fRect, path[i], prev) do
        dec(i);
    if (i < 0) then
    begin
      Result := path;
      Exit;
    end;
    if (prev = locInside) then
      loc := locInside;
    i := 0;
  end;
  startingLoc := loc;

  ///////////////////////////////////////////////////
  while i <= highI do
  begin
    prev := loc;
    prevCrossLoc := crossingLoc;
    GetNextLocation(path, loc, i, highI);
    if i > highI then Break;

    if i = 0 then
      prevPt := path[highI] else
      prevPt := path[i-1];
    crossingLoc := loc;
    if not GetIntersection(fRectPath, path[i], prevPt, crossingLoc, ip) then
    begin
      // ie remains outside (and crossingLoc still == loc)
      if (prevCrossLoc = locInside) then //ie rect still uncrossed
      begin
        isClockw := IsClockwise(prev, loc, prevPt, path[i], fRectMidPt);
        repeat
          fStartLocs.Add(Pointer(prev));
          prev := GetAdjacentLocation(prev, isClockw);
        until prev = loc;
        crossingLoc := prevCrossLoc; // because still not crossed
      end
      else if (prev <> locInside) and (prev <> loc) then
      begin
        isClockw := IsClockwise(prev, loc, prevPt, path[i], fRectMidPt);
        repeat
          AddCorner(prev, isClockw);
        until prev = loc;
      end;
      inc(i);
      Continue;
    end;

    ////////////////////////////////////////////////////
    // we must be crossing the rect boundary to get here
    ////////////////////////////////////////////////////

    if (loc = locInside) then // path must be entering rect
    begin
      if (fFirstCrossLoc = locInside) then
      begin
        fFirstCrossLoc := crossingLoc;
        fStartLocs.Add(Pointer(prev));
      end
      else if (prev <> crossingLoc) then
      begin
        isClockw := IsClockwise(prev, crossingLoc, prevPt, path[i], fRectMidPt);
        repeat
          AddCorner(prev, isClockw);
        until prev = crossingLoc;
      end;
    end
    else if (prev <> locInside) then
    begin
      // passing right through rect. 'ip' here will be the second
      // intersect pt but we'll also need the first intersect pt (ip2)
      loc := prev;
      GetIntersection(fRectPath, prevPt, path[i], loc, ip2);
      if (prevCrossLoc <> locInside) then
        AddCorner(prevCrossLoc, loc);

      if (fFirstCrossLoc = locInside) then
      begin
        fFirstCrossLoc := loc;
        fStartLocs.Add(Pointer(prev));
      end;

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
    end else // path must be exiting rect
    begin
      loc := crossingLoc;
      if (fFirstCrossLoc = locInside) then
        fFirstCrossLoc := crossingLoc;
    end;
    Add(ip);
  end; //while i <= highI
  ///////////////////////////////////////////////////

  if (fFirstCrossLoc = locInside) then
  begin
    // path never intersects with rect

    if startingLoc <> locInside then
    begin
      // path is outside rect but may or may not contain rect
      tmpRect := GetBounds(path);
      if tmpRect.Contains(fRect) and
        (Path1ContainsPath2(path, fRectPath) <> pipOutside) then
          Result := fRectPath
      else
        result := nil;
    end
    else Result := path;

    Exit;
  end;

  if (loc <> locInside) and
    ((loc <> fFirstCrossLoc) or (fStartLocs.Count > 2)) then
  begin
    if (fStartLocs.Count > 0) then
    begin
      prev := loc;
      for i := 0 to fStartLocs.Count -1 do
      begin
        loc2 := TLocation(fStartLocs[i]);
        if (prev = loc2) then Continue;
        AddCorner(prev, HeadingClockwise(prev, loc2));
        prev := loc2;
      end;
      loc := prev;
    end;
    if (loc <> fFirstCrossLoc) then
      AddCorner(loc, HeadingClockwise(loc, fFirstCrossLoc));
  end;

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
// TRectClipLines
//------------------------------------------------------------------------------

function TRectClipLines.GetCurrentPath: TPath64;
begin
  SetLength(fResult, fResultCnt);
  Result := fResult;
  Reset;
end;
//------------------------------------------------------------------------------

function TRectClipLines.Execute(const path: TPath64): TPaths64;
var
  i, highI      : integer;
  resCnt        : integer;
  prevPt,ip,ip2 : TPoint64;
  loc, prev     : TLocation;
  crossingLoc   : TLocation;
begin
  resCnt := 0;
  Result := nil;
  if (Length(path) < 2) or fRect.IsEmpty then Exit;

  Reset;
  i := 1;
  highI := Length(path) -1;

  if not GetLocation(fRect, path[0], loc) then
  begin
    while (i <= highI) and
      not GetLocation(fRect, path[i], prev) do
        inc(i);
    if (i > highI) then
    begin
      SetLength(Result, 1);
      Result[0] := path;
      Exit;
    end;
    if (prev = locInside) then
      loc := locInside;
    i := 1;
  end;

  if loc = locInside then Add(path[0]);
  ///////////////////////////////////////////////////
  while i <= highI do
  begin
    prev := loc;
    GetNextLocation(path, loc, i, highI);
    if i > highI then Break;
    prevPt := path[i-1];
    crossingLoc := loc;
    if not GetIntersection(fRectPath, path[i], prevPt, crossingLoc, ip) then
    begin
      // must be remaining outside
      inc(i);
      Continue;
    end;

    ////////////////////////////////////////////////////
    // we must be crossing the rect boundary to get here
    ////////////////////////////////////////////////////

    if (loc = locInside) then // path must be entering rect
    begin
      Add(ip);
    end
    else if (prev <> locInside) then
    begin
      // passing right through rect. 'ip' here will be the second
      // intersect pt but we'll also need the first intersect pt (ip2)
      crossingLoc := prev;
      GetIntersection(fRectPath, prevPt, path[i], crossingLoc, ip2);

      Add(ip2);
      Add(ip);
      inc(resCnt);
      SetLength(Result, resCnt);
      Result[resCnt -1] := GetCurrentPath;
    end else // path must be exiting rect
    begin
      Add(ip);
      inc(resCnt);
      SetLength(Result, resCnt);
      Result[resCnt -1] := GetCurrentPath;
    end;

  end; //while i <= highI
  ///////////////////////////////////////////////////

  if fResultCnt > 1 then
  begin
    inc(resCnt);
    SetLength(Result, resCnt);
    Result[resCnt -1] := GetCurrentPath;
  end;
  SetLength(Result, resCnt);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.

