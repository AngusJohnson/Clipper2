program DelphiBenchmark;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Clipper in '..\Clipper2Lib\Clipper.pas',
  Clipper.SVG in '..\Utils\Clipper.SVG.pas',
  ClipMisc in '..\Utils\ClipMisc.pas',
  Timer in '..\Utils\Timer.pas';

var
  s         : string;
  i,j       : integer;
  subj,clip : TPaths64;
  solution  : TPaths64;
  edgeCount : integer;
  timerResult   : double;
  timeTotal   : double;
  /////////////////////////////
  maxWidth  : integer   = 800;
  maxHeight : integer   = 600;
  minEdges  : integer   = 1000;
  maxEdges  : integer   = 5000;
  loopCount : integer   = 2;
  /////////////////////////////
begin
  Randomize;
  for j := (minEdges div 1000) to (maxEdges div 1000) do
  begin
    timeTotal := 0;
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
      subj[0] := MakeRandomPath(maxWidth, maxHeight, edgeCount);
      setLength(clip, 1);
      clip[0] := MakeRandomPath(maxWidth, maxHeight, edgeCount);

      begin
        DoTimer(timerResult);
        solution := Intersect(subj, clip, frNonZero);
      end;
      timeTotal := timeTotal + timerResult;

    end; //bottom of loop;
    WriteLn(Format('Average time: %1.0n msecs', [timeTotal*1000/loopCount]));
    WriteLn('');
  end; //bottom of edgecount loop

  //now display the very last solution ...
  with TSimpleClipperSvgWriter.Create(frNonZero) do
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
