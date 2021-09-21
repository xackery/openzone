unit Threads;

{ Copyright (c) 2007, Gurock Software GmbH. All rights reserved. For
  bug reports, patches and/or general feedback on this unit, please
  write to support@gurock.com.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer
      in the documentation and/or other materials provided with the
      distribution.
    * The name of Gurock Software may not be used to endorse or promote
      products derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE. }

interface

uses
  Windows, SysUtils;

{ The following condition variable implementation is mainly based on
  the work on condition variables for Win32 as found in the email
  conversation collected in the README.CV text file of the pthread
  distribution (http://sourceware.org/pthreads-win32/). The resulting
  algorithms and pseudo code are reused to implement a simple and
  hopefully correct TCondition implementation for Delphi. I would like
  to thank these people (and also Ross Johnson for taking the time to
  compile the email collection) which took part in the conversation
  for their efforts and especially for making their thoughts and ideas
  public.

  2007/08/04: Initial release. }

type
  TConditionRecord = record
    { Semaphore that guards access to waiters blocked count/block
      queue }
    BlockSemaphore: THandle;

    { Queue to queue up threads waiting for the condition to
      become signalled }
    BlockQueue: THandle;

    { Mutex/CriticalSection that guards access to waiters
      (to)unblock(ed) counts }
    UnblockLock: RTL_CRITICAL_SECTION;

    { Number of threads blocked }
    WaitersBlocked: Integer;

    { Number of threads unblocked }
    WaitersUnblocked: Integer;

    { Number of threads to unblock }
    WaitersToUnblock: Integer;
  end;

  TLock = class
  private
    FCriticalSection: RTL_CRITICAL_SECTION;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    function TryEnter: Boolean;
  end;

  TCondition = class
  private
    FConditionRecord: TConditionRecord;
  public
    constructor Create;
    destructor Destroy; override;
    function Wait(const ALock: TLock;
      const AMilliseconds: DWORD = INFINITE): Boolean;
    function Await(const ALock: TLock;
      const AMilliseconds: DWORD): DWORD;
    procedure Pulse;
    procedure PulseAll;
  end;

  TMonitor = class
  private
    FConditionRecord: TConditionRecord;
    FCriticalSection: RTL_CRITICAL_SECTION;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    function TryEnter: Boolean;
    procedure Pulse;
    procedure PulseAll;
    function Wait(const AMilliseconds: DWORD = INFINITE): Boolean;
    function Await(const AMilliseconds: DWORD): DWORD;
  end;

implementation

function WaitSemaphore(const ASemaphore: THandle): DWORD; overload;
begin
  Result := WaitForSingleObject(ASemaphore, INFINITE);
end;

function WaitSemaphore(const ASemaphore: THandle;
  const AMilliseconds: DWORD): DWORD; overload;
begin
  Result := WaitForSingleObject(ASemaphore, AMilliseconds);
end;

function SignalSemaphore(const ASemaphore: THandle): Boolean; overload;
begin
  Result := ReleaseSemaphore(ASemaphore, 1, nil);
end;

function SignalSemaphore(const ASemaphore: THandle;
  const AReleaseCount: DWORD): Boolean; overload;
begin
  Result := ReleaseSemaphore(ASemaphore, AReleaseCount, nil);
end;

function InitializeConditionVariable(var ACond: TConditionRecord): Boolean;
begin
  Result := False;
  ZeroMemory(@ACond, SizeOf(ACond)); { Initialize counters }

  ACond.BlockSemaphore := CreateSemaphore(nil, 1, MaxInt, nil);
  if ACond.BlockSemaphore <> 0 then
  begin
    ACond.BlockQueue := CreateSemaphore(nil, 0, MaxInt, nil);
    if ACond.BlockQueue <> 0 then
    begin
      InitializeCriticalSection(ACond.UnblockLock);
      Result := True;
    end else
      CloseHandle(ACond.BlockSemaphore);
  end;
end;

procedure DeleteConditionVariable(var ACond: TConditionRecord);
begin
  CloseHandle(ACond.BlockSemaphore);
  CloseHandle(ACond.BlockQueue);
  DeleteCriticalSection(ACond.UnblockLock);
end;

function WaitConditionVariable(var ACond: TConditionRecord;
  var ACriticalSection: RTL_CRITICAL_SECTION;
  const AMilliseconds: DWORD): Boolean;
var
  LResult: DWORD;
  LSignalsLeft: Integer;
begin
  WaitSemaphore(ACond.BlockSemaphore);
  Inc(ACond.WaitersBlocked);
  SignalSemaphore(ACond.BlockSemaphore);

  LeaveCriticalSection(ACriticalSection);

  LResult := WaitSemaphore(ACond.BlockQueue, AMilliseconds);
  EnterCriticalSection(ACond.UnblockLock);

  LSignalsLeft := ACond.WaitersToUnblock;
  if LSignalsLeft <> 0 then
  begin
    Dec(ACond.WaitersToUnblock)
  end else
  begin
    Inc(ACond.WaitersUnblocked);
    if ACond.WaitersUnblocked = MaxInt / 2 then
    begin
      WaitSemaphore(ACond.BlockSemaphore);
      Dec(ACond.WaitersBlocked, ACond.WaitersUnblocked);
      SignalSemaphore(ACond.BlockSemaphore);
      ACond.WaitersUnblocked := 0;
    end;
  end;

  LeaveCriticalSection(ACond.UnblockLock);

  if LSignalsLeft = 1 then
  begin
    SignalSemaphore(ACond.BlockSemaphore);
  end;

  EnterCriticalSection(ACriticalSection);
  Result := LResult = WAIT_OBJECT_0;
end;

function AwaitConditionVariable(var ACond: TConditionRecord;
  var ACriticalSection: RTL_CRITICAL_SECTION;
  const AMilliseconds: DWORD): DWORD;
var
  LCounterStart: Int64;
  LCounterEnd: Int64;
  LCounterFrequency: Int64;
  LHasCounter: Boolean;
  LTickCount: DWORD;
  LElapsed: DWORD;
begin
  LHasCounter := False;

  { Fall back to the low resolution GetTickCount timer if we do not
    have a high performance counter. This ensures that this method
    cannot fail due to a missing high performance counter. We call
    GetTickCount relatively early and late with respect to the
    actual WaitConditionVariable call but we first expect QPF/QPC
    to be available most of the time and secondly the resolution
    of GetTickCount is so low that this shouldn't matter at all. }
  LTickCount := GetTickCount;

  if QueryPerformanceFrequency(LCounterFrequency) then
  begin
    if QueryPerformanceCounter(LCounterStart) then
    begin
      LHasCounter := True;
    end;
  end;

  WaitConditionVariable(ACond, ACriticalSection, AMilliseconds);

  if LHasCounter then
  begin
    LHasCounter := QueryPerformanceCounter(LCounterEnd);
  end;

  if LHasCounter then
     LElapsed := Round((LCounterEnd - LCounterStart) /
       (LCounterFrequency / 1000)) { Convert to milliseconds }
  else
    LElapsed := GetTickCount - LTickCount;

  { Prevent a 'negative' result here. Windows handles timeouts as
    DWORD (unsigned) and a naive substraction could thus result in an
    integer overflow. Prevent this behavior by returning 0 in these
    cases. }
  if LElapsed < AMilliseconds then
    Result := AMilliseconds - LElapsed
  else
    Result := 0;
end;

procedure InternalPulseConditionVariable(var ACond: TConditionRecord;
  const APulseAll: Boolean);
var
  LSignalsToIssue: Integer;
begin
  EnterCriticalSection(ACond.UnblockLock);

  if ACond.WaitersToUnblock <> 0 then
  begin
    if ACond.WaitersBlocked = 0 then
    begin
      LeaveCriticalSection(ACond.UnblockLock);
      Exit;
    end;

    if APulseAll then
    begin
      LSignalsToIssue := ACond.WaitersBlocked;
      Inc(ACond.WaitersToUnblock, LSignalsToIssue);
      ACond.WaitersBlocked := 0;
    end else
    begin
      LSignalsToIssue := 1;
      Inc(ACond.WaitersToUnblock);
      Dec(ACond.WaitersBlocked);
    end;
  end else if ACond.WaitersBlocked > ACond.WaitersUnblocked then
  begin
    WaitSemaphore(ACond.BlockSemaphore);
    if ACond.WaitersUnblocked <> 0 then
    begin
      Dec(ACond.WaitersBlocked, ACond.WaitersUnblocked);
      ACond.WaitersUnblocked := 0;
    end;

    if APulseAll then
    begin
      ACond.WaitersToUnblock := ACond.WaitersBlocked;
      LSignalsToIssue := ACond.WaitersToUnblock;
      ACond.WaitersBlocked := 0;
    end else
    begin
      ACond.WaitersToUnblock := 1;
      LSignalsToIssue := 1;
      Dec(ACond.WaitersBlocked);
    end;
  end else
  begin
    LeaveCriticalSection(ACond.UnblockLock);
    Exit;
  end;

  LeaveCriticalSection(ACond.UnblockLock);
  SignalSemaphore(ACond.BlockQueue, LSignalsToIssue);
end;

procedure PulseConditionVariable(var ACond: TConditionRecord);
begin
  InternalPulseConditionVariable(ACond, False);
end;

procedure PulseAllConditionVariable(var ACond: TConditionRecord);
begin
  InternalPulseConditionVariable(ACond, True);
end;

{ TMonitor }

function TMonitor.Await(const AMilliseconds: DWORD): DWORD;
begin
  Result := AwaitConditionVariable(FConditionRecord, FCriticalSection,
    AMilliseconds);
end;

constructor TMonitor.Create;
begin
  InitializeCriticalSection(FCriticalSection);
  if not InitializeConditionVariable(FConditionRecord) then
  begin
    RaiseLastOSError;
  end;
end;

destructor TMonitor.Destroy;
begin
  DeleteConditionVariable(FConditionRecord);
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

procedure TMonitor.Enter;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TMonitor.Leave;
begin
  LeaveCriticalSection(FCriticalSection);
end;

procedure TMonitor.Pulse;
begin
  PulseConditionVariable(FConditionRecord);
end;

procedure TMonitor.PulseAll;
begin
  PulseAllConditionVariable(FConditionRecord);
end;

function TMonitor.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FCriticalSection);
end;

function TMonitor.Wait(const AMilliseconds: DWORD): Boolean;
begin
  Result := WaitConditionVariable(FConditionRecord, FCriticalSection,
    AMilliseconds)
end;

{ TCondition }

function TCondition.Await(const ALock: TLock;
  const AMilliseconds: DWORD): DWORD;
begin
  Result := AwaitConditionVariable(FConditionRecord, ALock.FCriticalSection,
    AMilliseconds);
end;

constructor TCondition.Create;
begin
  if not InitializeConditionVariable(FConditionRecord) then
  begin
    RaiseLastOSError;
  end;
end;

destructor TCondition.Destroy;
begin
  DeleteConditionVariable(FConditionRecord);
  inherited;
end;

procedure TCondition.Pulse;
begin
  PulseConditionVariable(FConditionRecord);
end;

procedure TCondition.PulseAll;
begin
  PulseAllConditionVariable(FConditionRecord);
end;

function TCondition.Wait(const ALock: TLock;
  const AMilliseconds: DWORD): Boolean;
begin
  Result := WaitConditionVariable(FConditionRecord, ALock.FCriticalSection,
    AMilliseconds)
end;

{ TLock }

constructor TLock.Create;
begin
  InitializeCriticalSection(FCriticalSection);
end;

destructor TLock.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

procedure TLock.Enter;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TLock.Leave;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TLock.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FCriticalSection);
end;

end.
