program DelphiBenchmark;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  Diagnostics,
  Clipper in '..\Clipper.pas',
  Clipper.SVG in '..\Clipper.SVG.pas';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function MakeRandomPath(maxWidth, maxHeight, count: Integer;
  RoundTo: Integer = 10; margin: Integer = 10): TPath64;
var
  i: Integer;
begin
  setlength(Result, count);
  for i := 0 to count -1 do with Result[i] do
  begin
    X := ((Random(maxWidth - 2 * margin) + margin) div RoundTo) * RoundTo;
    Y := ((Random(maxHeight - 2 * margin) + margin) div RoundTo) * RoundTo;
  end;
end;
//------------------------------------------------------------------------------

var
  s         : string;
  i,j       : integer;
  subj,clip : TPaths64;
  solution  : TPaths64;
  edgeCount : integer;
  msecs     : Int64;
  /////////////////////////////
  maxWidth  : integer   = 800;
  maxHeight : integer   = 600;
  minEdges  : integer   = 1000;
  maxEdges  : integer   = 5000;
  loopCount : integer   = 3;
  /////////////////////////////
begin
  Randomize;
  for j := (minEdges div 1000) to (maxEdges div 1000) do
  begin
    msecs := 0;
    edgeCount := j * 1000;
    for i := 1 to loopCount do
    begin
      if (i = 1) then
      begin
        WriteLn('Testing edge count: ' + Inttostr(edgeCount));
        Write('Loop: 1 ');
      end
        else Write(inttostr(i) + ' ');

      //make 2 random self-intersecting paths of 'edgeCount' length
      setLength(subj, 1);
      subj[0] := MakeRandomPath(maxWidth, maxHeight, edgeCount, 1, 0);
      setLength(clip, 1);
      clip[0] := MakeRandomPath(maxWidth, maxHeight, edgeCount, 1, 0);

      //time their intersection
      with TStopWatch.StartNew do
      begin
        solution := Intersect(subj, clip, frNonZero);
        if Length(solution) = 0 then
          raise Exception.Create('Error in loop count ' + inttostr(i));
        Inc(msecs, ElapsedMilliseconds);
      end;

    end; //bottom of loop;
    WriteLn(Format('Average time: %1.0f msecs', [msecs/loopCount]));
    WriteLn('');
  end; //bottom of edgecount loop

  //now display the very last solution ...
  with SimpleClipperSvgWriter.Create(frNonZero) do
  try
    AddText('sample', 0,0);
    AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
    AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
    AddPaths(solution, false, $2000FF00, $FF006600, 0.8);
    SaveToFile('test.svg');
  finally
    free;
  end;
  ShellExecute(0, nil, 'test.svg', Nil, Nil, SW_NORMAL);

  WriteLn('Finished. Press Enter to exit.');
  ReadLn(s);
end.
