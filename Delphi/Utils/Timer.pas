unit Timer;

interface

uses
  Windows,
  SysUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type

  ITimer = interface
  ['{A50173E8-6497-4F83-B4A8-9C0D5D709834}']
  end;

function DoTimer(out timeResult: double): ITimer;

implementation

type

  PTimerData = ^TTimerData;
  TTimerData = record
    FVTable     : Pointer;
    FRefCount   : Integer;
    FResult     : PDouble;
    FTimeFreq   : TLargeInteger;
    FStartTime  : TLargeInteger;
    FEndTime    : TLargeInteger;
  end;

{$IFDEF FPC}
  TTimer = object
{$ELSE}
  TTimer = record
{$ENDIF}
  public
    class function QueryInterface(Inst: PTimerData;
      const IID: TGUID; out Obj): HResult; stdcall; static;
    class function AddRef(Inst: PTimerData): Integer; stdcall; static;
    class function Release(Inst: PTimerData): Integer; stdcall; static;
  end;


const
  TimerVTable: array[0..2] of Pointer =
  (
    @TTimer.QueryInterface,
    @TTimer.AddRef,
    @TTimer.Release
  );

class function TTimer.QueryInterface(Inst: PTimerData;
      const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result:= E_NOINTERFACE;
end;

class function TTimer.AddRef(Inst: PTimerData): Integer; stdcall;
begin
  inc(inst.FRefCount);
  Result := inst.FRefCount;
  if inst.FRefCount <> 1 then Exit;
  QueryPerformanceCounter(inst.FStartTime);
end;

class function TTimer.Release(Inst: PTimerData): Integer; stdcall;
begin
  QueryPerformanceCounter(inst.FEndTime);
  with inst^ do
  begin
    dec(FRefCount);
    Result := FRefCount;
    if (Result <> 0) then Exit;
    if assigned(FResult) then
      FResult^ := (FEndTime - FStartTime)/FTimeFreq;
  end;
  Dispose(Inst);
end;

function DoTimer(out timeResult: double): ITimer;
var
  timer: PTimerData;
begin
  New(timer);
  timer.FVTable:= @TimerVTable;
  timer.FRefCount:= 0;
  timer.FResult := @timeResult;
  QueryPerformanceFrequency(timer.FTimeFreq);
  Result := ITimer(timer);
end;

end.
