program DelphiBenchmark;

{$APPTYPE CONSOLE}

uses
  Windows,
  ShellAPI,
  SysUtils,
  Classes,
  Clipper.DLL in '..\..\Delphi_interface\Clipper.DLL.pas',
  Clipper.DLL.Utils in '..\..\Delphi_interface\Utils\Clipper.DLL.Utils.pas',
  Clipper.DLL.SVG in '..\..\Delphi_interface\SVG\Clipper.DLL.SVG.pas';

type

  TTimeRec = record
    freq: TLargeInteger;
    startTime: TLargeInteger;
    endTime: TLargeInteger;
  end;

procedure StartTimer(out timeRec: TTimeRec);
begin
  QueryPerformanceFrequency(timeRec.freq);
  QueryPerformanceCounter(timeRec.startTime);
  timeRec.endTime := timeRec.startTime;
end;

function EndTimer(timeRec: TTimeRec): double;
begin
  QueryPerformanceCounter(timeRec.endTime);
  with timeRec do
    Result := (endTime - startTime) / freq;
end;


var
  s: string;
  i, j: integer;
  subj, clip: CPaths64;
  solution: CPaths64;
  solution_open: CPaths64;
  edgeCount: integer;
  timerRec: TTimeRec;
  timeTotal: double;

  /// //////////////////////////
const
  maxWidth: integer = 800;
  maxHeight: integer = 600;
  minEdgeCnt: integer = 1000;
  maxEdgeCnt: integer = 5000;
  loopCount: integer = 10;

  /// //////////////////////////
begin

  Randomize;
  subj := nil;
  clip := nil;
  solution := nil;
  for j := (minEdgeCnt div 1000) to (maxEdgeCnt div 1000) do
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
      else
        Write(Inttostr(i) + ' ');

      subj.Free; // last instance is kept for svg
      subj := MakeRandomPaths(maxWidth, maxHeight, [edgeCount]);

      clip.Free;
      clip := MakeRandomPaths(maxWidth, maxHeight, [edgeCount]);

      if solution <> nil then
        DisposeExternalCPaths64(solution);

      StartTimer(timerRec);

      BooleanOp64(TClipType.Intersection, TFillRule.NonZero, subj, nil, clip, solution, solution_open);
      timeTotal := timeTotal + EndTimer(timerRec);

    end; // bottom of loop;
    WriteLn(Format('Average time: %1.0n msecs', [timeTotal * 1000 / loopCount]));
    WriteLn('');
  end; // bottom of edgecount loop

  // now display the very last solution ...
  with TSvgWriter.Create(TFillRule.NonZero) do
    try
      AddText('sample', 0, 0);
      AddPaths(subj, false, $1000BBFF, $800099FF, 0.8);
      AddPaths(clip, false, $12F99F00, $80FF9900, 0.8);
      AddPaths(solution, false, $2000FF00, $FF006600, 0.8);
      SaveToFile('test.svg');
    finally
      Free;
    end;

  subj.Free;
  clip.Free;
  DisposeExternalCPaths64(solution);

  ShellExecute(0, nil, 'test.svg', Nil, Nil, SW_NORMAL);

  WriteLn('Finished. Press Enter to exit.');
  ReadLn(s);

end.
