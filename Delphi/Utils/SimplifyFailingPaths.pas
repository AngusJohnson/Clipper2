unit SimplifyFailingPaths;

interface

uses
  Windows, SysUtils, Classes, Clipper.Core;

function DoSimplifyFailingPaths(aClipOp: TClipType; aFillRule: TFillRule;
  var subj, clip: TPaths64): Boolean;

implementation

uses
  Clipper, Clipper.Engine;

type
  TSimplifyFailingPaths = class
  var
    simplified: Boolean;
    clipOp: TClipType;
    fillrule: TFillRule;
    sub: TPaths64;
    clp: TPaths64;
  private
    function CheckSubj(const subj: TPaths64): Boolean;
    function CheckClip(const clip: TPaths64): Boolean;
  public
    function Simplify(clipOp: TClipType; fillrule: TFillRule;
      var subj: TPaths64; var clip: TPaths64): Boolean;
  end;

  TSimplifyOp = function (const paths: TPaths64): Boolean of object;

function SplitPath(const path: TPath64; splitCnt: integer):  TPaths64;
var
  i, j, len, splitSize: integer;
begin
  Result := nil;
  len := Length(path);
  if splitCnt > len then Exit;
  splitSize := len div splitCnt;
  if splitSize * splitCnt < len then inc(splitSize);
  SetLength(result, splitCnt);
  j := 0;
  for i := 0 to splitCnt -1 do
  begin
    Result[i] := Copy(path, j, splitSize);
    inc(j, splitSize);
  end;
end;
//------------------------------------------------------------------------------

function SimplifyFailingClip(var paths: TPaths64;
  simplifyOp: TSimplifyOp): Boolean;
var
  i,j,k,q, len, splits: integer;
  tmp, tmp2: TPaths64;
  p: TPath64;
  opHasFailed: Boolean;
begin
  result := false;

  //make sure there's an error to find
  opHasFailed := false;
  try
    opHasFailed := not simplifyOp(paths);
  except
    opHasFailed := true;
  end;
  if not opHasFailed then
    Exit;

  tmp2 := Copy(paths, 0, Length(paths));
  for k := 0 to High(paths) do
  begin
    len := Length(paths[k]);
    q := Round(sqrt(len));
    splits := 1;
    while true do
    begin
      tmp := nil;
      if splits < q then
      begin
        inc(splits);
        tmp := SplitPath(tmp2[k], splits);
      end else
      begin
        dec(q);
        splits := (len div q) +1;
        tmp := SplitPath(tmp2[k], splits);
      end;
      if tmp = nil then Break; //////////////////////////

      for i := 0 to splits -1 do
      begin
        if tmp[i] = nil then Continue;
        opHasFailed := false;

        //extract trial points from tmp2[k]
        tmp2[k] := nil;
        for j := 0 to High(tmp) do
          if (j <> i) and (tmp[j] <> nil) then
            tmp2[k] := AppendPoints(tmp2[k], tmp[j]);

        //try bool op
        try
          opHasFailed := not simplifyOp(tmp2);
        except
          opHasFailed := true;
        end;

        if opHasFailed then
        begin
          tmp[i] := nil;
          Result := true;
        end;
      end;
      //recreate tmp2[k]
      tmp2[k] := nil;
      for j := 0 to High(tmp) do
        if (tmp[j] <> nil) then
          tmp2[k] := AppendPoints(tmp2[k], tmp[j]);
    end;
  end;
  if result then
    paths := tmp2;
end;
//------------------------------------------------------------------------------

function TSimplifyFailingPaths.CheckSubj(const subj: TPaths64): Boolean;
var
  tmp: TPaths64;
begin
  with TClipper.Create do
  try
    AddSubject(subj);
    AddClip(clp);
    Result := Execute(clipOp, fillRule, tmp);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function TSimplifyFailingPaths.CheckClip(const clip: TPaths64): Boolean;
var
  tmp: TPaths64;
begin
  with TClipper.Create do
  try
    AddSubject(sub);
    AddClip(clip);
    Result := Execute(clipOp, fillRule, tmp);
  finally
    Free;
  end;
end;

function TSimplifyFailingPaths.Simplify(clipOp: TClipType;
  fillrule: TFillRule; var subj: TPaths64; var clip: TPaths64): Boolean;
var
  res1, res2: boolean;
begin
  self.clipOp := clipOp;
  self.fillrule := fillrule;
  clp := clip;
  res1 := SimplifyFailingClip(subj, CheckSubj);
  sub := subj;
  res2 := SimplifyFailingClip(clip, CheckClip);
  Result := res1 or res2;
end;

function Compare(node1, node2: Pointer): Int64;
begin
  Result := Int64(node1) - Int64(node2);
end;
//------------------------------------------------------------------------------

function DoSimplifyFailingPaths(aClipOp: TClipType; aFillRule: TFillRule;
  var subj, clip: TPaths64): Boolean;
begin
  with TSimplifyFailingPaths.Create do
  try
    Result := Simplify(aClipOp, aFillRule, subj, clip);
  finally
    free;
  end;
end;

end.
