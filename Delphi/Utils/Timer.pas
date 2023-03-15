unit Timer;

interface

uses
  Windows,
  SysUtils;

(*******************************************************************************
*  Example:                                                                    *
*                                                                              *
*    var                                                                       *
*      timeRec  : TTimeRec;                                                    *
*      elapsed  : double;                                                      *
*    begin                                                                     *
*      StartTimer(timeRec);                                                    *
*      DoSomeLengthyOp();                                                      *
*      elapsed := EndTimer(timeRec)                                            *
*      Caption := Format('DoSomeLengthyOp took %1.3n secs', [elapsed);         *
*    end;                                                                      *
*******************************************************************************)

{$I Clipper.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type

TTimeRec = record
  freq      : TLargeInteger;
  startTime : TLargeInteger;
  endTime   : TLargeInteger;
end;

procedure StartTimer(out timeRec: TTimeRec);
function EndTimer(timeRec: TTimeRec): double;

implementation

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
    Result := (endTime - startTime)/freq;
end;


end.
