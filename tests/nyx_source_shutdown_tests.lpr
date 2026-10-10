{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

program nyx_source_shutdown_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Windows, nyx.text, nyx.bytes, nyx.data, nyx.model,
  nyx.codec, nyx.source, nyx.scheduler, nyx.studio.buildexecutor,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.projectionstorage,
  nyx.studio.sourceprojection, nyx.studio.sourcecompilation,
  nyx.studio.sourcecompilation.native, nyx.test.projection,
  nyx.test.source.compilation;

type
  { Counts live completion-port lifetimes, separate from retained projection
    values. Calls are read only after the operation's interlocked terminal
    publication. Caller cleanup retains no compiler or mutable editor. }
  TProbe = class(TInterfacedObject, INyxSourceCompilationPort)
  public
    Calls: Integer;
    Projection: INyxSourceProjection;
    Failure: TNyxText;
    ThrowOnComplete: Boolean;
    BlockOnComplete: Boolean;
    MarkerRoot: TNyxText;
    constructor Create;
    destructor Destroy; override;
    procedure Complete(const AProjection: INyxSourceProjection;
      const AFailure: TNyxText);
  end;

  { A cancelled queued port deliberately pauses its callback. This independently
    owned observer sees that the token stays nonterminal until callback release,
    then releases it. It never owns the compiler or dereferences an editor. }
  TReleaseCallback = class(TThread)
  private
    FOperation: INyxSourceCompilation;
    FRoot: TNyxText;
  protected
    procedure Execute; override;
  public
    ObservedNonterminal: Boolean;
    ConcurrentCancelNonterminal: Boolean;
    constructor Create(const AOperation: INyxSourceCompilation; const ARoot: TNyxText);
  end;

const
  CUnit: TNyxText = 'nyx.shutdown.fixture';

var
  GChecks: Integer;
  GProbes: LongInt;
  GDirectories: TNyxStudioDirectories;
  GProfile: TNyxText;
  GPolicy: TNyxProjectionStoragePolicy;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Native source shutdown: ' + AMessage);
  end;
  Inc(GChecks);
end;

procedure SaveText(const APath, AText: TNyxText);
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check((LFile.Size > 0) and (LFile.Size <= NyxProjectionMaximumResultBytes),
      'bounded qualification input');
    SetLength(LBytes, LFile.Size);
    LFile.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

constructor TProbe.Create;
begin
  inherited Create;
  InterlockedIncrement(GProbes);
end;

destructor TProbe.Destroy;
begin
  InterlockedDecrement(GProbes);
  inherited Destroy;
end;

procedure TProbe.Complete(const AProjection: INyxSourceProjection;
  const AFailure: TNyxText);
var
  LStarted: QWord;
begin
  Inc(Calls);
  Projection := AProjection;
  Failure := AFailure;

  if BlockOnComplete then
  begin
    SaveText(MarkerRoot + 'callback.entered', 'entered');
    LStarted := GetTickCount64;
    while not FileExists(MarkerRoot + 'callback.release') and
      (GetTickCount64 - LStarted < 10000) do
    begin
      Sleep(5);
    end;

    if not FileExists(MarkerRoot + 'callback.release') then
    begin
      raise Exception.Create('Callback observer did not release its marker');
    end;
  end;

  if ThrowOnComplete then
  begin
    raise Exception.Create('Deliberate completion-port failure');
  end;
end;

constructor TReleaseCallback.Create(const AOperation: INyxSourceCompilation;
  const ARoot: TNyxText);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOperation := AOperation;
  FRoot := ARoot;
end;

procedure TReleaseCallback.Execute;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  while not FileExists(FRoot + 'callback.entered') and
    (GetTickCount64 - LStarted < 10000) do
  begin
    Sleep(5);
  end;

  if FileExists(FRoot + 'callback.entered') then
  begin
    ObservedNonterminal := FOperation.State in [scsPending, scsRunning];
    { The winning Cancel is still inside its deliberately blocked port on the
      caller thread. Another thread must neither deliver twice nor report that
      ownership as terminal before the first callback returns. }
    FOperation.Cancel;
    ConcurrentCancelNonterminal := FOperation.State in [scsPending, scsRunning];
  end;
  SaveText(FRoot + 'callback.release', 'released');
end;

function Source(const ABody: TNyxText): TNyxText;
begin
  Result := 'unit ' + CUnit + ';' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.model; function BuildNyxDocument: TNyxDocument;' + #10 +
    'implementation uses Classes, SysUtils, Windows, nyx.controls, nyx.types;' + #10 +
    'function BuildNyxDocument: TNyxDocument;' + #10 + 'begin' + #10 +
    ABody + #10 + 'end;' + #10 + 'end.' + #10;
end;

function PlainSource: TNyxText;
begin
  Result := Source('  Result := TNyxDocument.Create;' + #10 +
    '  Result.Title := ''A completed source operation'';' + #10 +
    '  Result.AddPage(NewNyxPage(''home'', ncoDescriptor));');
end;

function GateSource: TNyxText;
begin
  Result := Source('  with TFileStream.Create(''running.marker'', fmCreate) do' + #10 +
    '  try' + #10 + '    WriteBuffer(GetCurrentProcessId, 4);' + #10 +
    '  finally' + #10 + '    Free;' + #10 + '  end;' + #10 +
    '  while not FileExists(''release.marker'') do' + #10 + '  begin' + #10 +
    '    Sleep(10);' + #10 + '  end;' + #10 +
    '  Result := TNyxDocument.Create;' + #10 +
    '  Result.Title := ''A released source operation'';' + #10 +
    '  Result.AddPage(NewNyxPage(''home'', ncoDescriptor));');
end;

function NewHost: INyxSourceCompiler;
begin
  Result := NewNyxNativeSourceCompiler(GDirectories, GProfile,
    TNyxCompilerLimits.Default, GPolicy,
    TNyxSchedulerOptions.Defaults.Workers(1).PendingCapacity(1));
end;

function NewProbe(out AProbe: TProbe): INyxSourceCompilationPort;
begin
  AProbe := TProbe.Create;
  Result := AProbe;
end;

procedure Jobs(ANames: TNyxStrings);
var
  LSearch: TSearchRec;
begin
  ANames.Clear;

  if FindFirst(GDirectories.Jobs + 'job-*', faDirectory, LSearch) = 0 then
  begin
    try
      repeat

        if ANames.Count >= 32 then
        begin
          raise Exception.Create('Isolated constructor inventory exceeds its budget');
        end;
        ANames.Add(TNyxText(LSearch.Name));
      until FindNext(LSearch) <> 0;
    finally
      SysUtils.FindClose(LSearch);
    end;
  end;
end;

function WaitGate(const AOperation: INyxSourceCompilation;
  ABefore: TNyxStrings): TNyxText;
var
  LAfter: TNyxStrings;
  LStarted: QWord;
  LIndex: Integer;
begin
  Result := '';
  LAfter := TNyxStrings.Create;
  try
    LStarted := GetTickCount64;
    repeat
      Jobs(LAfter);
      for LIndex := 0 to LAfter.Count - 1 do
      begin

        if (ABefore.IndexOf(LAfter[LIndex]) < 0) and
          FileExists(GDirectories.Jobs + LAfter[LIndex] + '/running.marker') then
        begin
          Result := GDirectories.Jobs + LAfter[LIndex] + PathDelim;
          Exit;
        end;
      end;
      CheckSynchronize(0);
      Sleep(5);
    until (GetTickCount64 - LStarted >= 90000) or
      not (AOperation.State in [scsPending, scsRunning]);
    AOperation.Cancel;
    raise Exception.Create('Actual gated constructor did not reach its marker');
  finally
    LAfter.Free;
  end;
end;

procedure WaitTerminal(const AOperation: INyxSourceCompilation);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  while AOperation.State in [scsPending, scsRunning] do
  begin

    if GetTickCount64 - LStarted >= 90000 then
    begin
      AOperation.Cancel;
      raise Exception.Create('Native source operation exceeded its retirement budget');
    end;
    CheckSynchronize(0);
    Sleep(5);
  end;
end;

function FailureOf(const AOperation: INyxSourceCompilation): TNyxText;
var
  LNative: INyxNativeSourceCompilation;
begin
  Check(Supports(AOperation, INyxNativeSourceCompilation, LNative),
    'detached native operation diagnostic facet');
  Result := LNative.Failure;
end;

procedure CheckCancelled(const AOperation: INyxSourceCompilation; AProbe: TProbe);
begin
  Check((AOperation.State = scsCancelled) and (AProbe.Calls = 1) and
    (AProbe.Projection <> nil) and (AProbe.Projection.State = spsCancelled) and
    (AProbe.Projection.Design = '') and (AProbe.Failure = ''),
    'one terminal typed cancellation retains no usable design');
  Check(FailureOf(AOperation) <> '', 'cancelled operation diagnostic remains queryable');
end;

procedure ShutdownChecks;
var
  LCompiler: INyxSourceCompiler;
  LActive: INyxSourceCompilation;
  LQueued: INyxSourceCompilation;
  LReplacement: INyxSourceCompilation;
  LRejectedOperation: INyxSourceCompilation;
  LActivePort: INyxSourceCompilationPort;
  LQueuedPort: INyxSourceCompilationPort;
  LReplacementPort: INyxSourceCompilationPort;
  LRejectedPort: INyxSourceCompilationPort;
  LActiveProbe: TProbe;
  LQueuedProbe: TProbe;
  LReplacementProbe: TProbe;
  LRejectedProbe: TProbe;
  LNamesBefore: TNyxStrings;
  LNamesAfter: TNyxStrings;
  LDirectory: TNyxText;
  LRejected: Boolean;
  LCallbackObserver: TReleaseCallback;
  LStarted: QWord;
  LMarker: TFileStream;
  LPID: Cardinal;
  LChild: THandle;
  LRetained: INyxSourceProjection;
begin
  LNamesBefore := TNyxStrings.Create;
  LNamesAfter := TNyxStrings.Create;
  try
    Jobs(LNamesBefore);
    LCompiler := NewHost;
    LActivePort := NewProbe(LActiveProbe);
    LActive := LCompiler.Start(GateSource, LActivePort);
    LDirectory := WaitGate(LActive, LNamesBefore);
    Check((LActive.State = scsRunning) and (LActiveProbe.Calls = 0),
      'actual constructor owns first worker until its marker releases');
    LMarker := TFileStream.Create(LDirectory + 'running.marker', fmOpenRead);
    try
      LMarker.ReadBuffer(LPID, SizeOf(LPID));
    finally
      LMarker.Free;
    end;
    LChild := OpenProcess(SYNCHRONIZE, False, LPID);
    Check((LChild <> 0) and (WaitForSingleObject(LChild, 0) = WAIT_TIMEOUT),
      'actual constructor process identity is retained live');
    try
      LQueuedPort := NewProbe(LQueuedProbe);
      LQueued := LCompiler.Start(PlainSource, LQueuedPort);
      Check((LQueued.State = scsPending) and (LQueuedProbe.Calls = 0),
        'real saturated scheduler leaves second operation queued');
      LRejectedPort := NewProbe(LRejectedProbe);
      LRejected := False;
      try
        LRejectedOperation := LCompiler.Start(PlainSource, LRejectedPort);
      except
        on ENyxScheduleCapacity do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LRejectedOperation = nil) and (LRejectedProbe.Calls = 0),
        'capacity refusal returns no token and delivers no unadmitted callback');
      LRejectedPort := nil;
      Check(GProbes = 2, 'refused source admission retains no completion port');
      LQueuedProbe.BlockOnComplete := True;
      LQueuedProbe.MarkerRoot := GDirectories.RuntimeRoot;
      LCallbackObserver := TReleaseCallback.Create(LQueued, GDirectories.RuntimeRoot);
      try
        LCallbackObserver.Start;
        LQueued.Cancel;
        LCallbackObserver.WaitFor;
        Check(LCallbackObserver.ObservedNonterminal,
          'pending cancellation remains nonterminal while its completion callback retires');
        Check(LCallbackObserver.ConcurrentCancelNonterminal,
          'concurrent cancellation cannot steal callback ownership or publish terminal early');
      finally
        LCallbackObserver.Free;
      end;
      CheckCancelled(LQueued, LQueuedProbe);
      LQueued.Cancel;
      Check(LQueuedProbe.Calls = 1, 'repeated queued cancellation delivers once');
      LQueuedPort := nil;
      Check(GProbes = 1, 'terminal queued operation does not retain its port');
      LReplacementPort := NewProbe(LReplacementProbe);
      LReplacement := LCompiler.Start(PlainSource, LReplacementPort);
      Check(LReplacement.State = scsPending, 'cancelled queue slot is reusable before worker release');
      Jobs(LNamesAfter);
      Check(LNamesAfter.Count = LNamesBefore.Count + 1,
        'queued/refused/cancelled jobs allocate no compiler directory');
      LStarted := GetTickCount64;
      LCompiler := nil;
      Check(GetTickCount64 - LStarted < 1000, 'strategy shutdown does not join active compiler on UI');
      CheckCancelled(LReplacement, LReplacementProbe);
      LReplacementPort := nil;
      WaitTerminal(LActive);
      CheckCancelled(LActive, LActiveProbe);
      Check(WaitForSingleObject(LChild, 0) = WAIT_OBJECT_0,
        'active cancellation is terminal only after exact constructor process exits');
      Check(not FileExists(LDirectory + 'nyx_projection.exe') and
        FileExists(LDirectory + 'compiler.log') and FileExists(LDirectory + 'execution.log'),
        'shutdown retires compiler derivatives and retains diagnostic evidence');
      Jobs(LNamesAfter);
      Check(LNamesAfter.Count = LNamesBefore.Count + 1,
        'host shutdown never dispatches queued replacement');
      LActivePort := nil;
      Check(GProbes = 0, 'retained terminal tokens own no completion ports');
    finally
      CloseHandle(LChild);
    end;

    Jobs(LNamesBefore);
    LCompiler := NewHost;
    LActivePort := NewProbe(LActiveProbe);
    LActive := LCompiler.Start(GateSource, LActivePort);
    LDirectory := WaitGate(LActive, LNamesBefore);
    LQueuedPort := NewProbe(LQueuedProbe);
    LQueuedProbe.ThrowOnComplete := True;
    LQueued := LCompiler.Start(PlainSource, LQueuedPort);
    LCompiler := nil;
    Check((LQueued.State = scsFailed) and (LQueuedProbe.Calls = 1) and
      (FailureOf(LQueued) = 'Deliberate completion-port failure'),
      'throwing pending port is observable and cannot abort host retirement');
    WaitTerminal(LActive);
    CheckCancelled(LActive, LActiveProbe);
    LQueuedPort := nil;
    LActivePort := nil;
    Check(GProbes = 0, 'throwing queued port and active shutdown release independent ownership');

    LCompiler := NewHost;
    LActivePort := NewProbe(LActiveProbe);
    LActive := LCompiler.Start(PlainSource, LActivePort);
    WaitTerminal(LActive);
    Check((LActive.State = scsCompleted) and (LActiveProbe.Calls = 1) and
      (LActiveProbe.Projection.State = spsExecuted) and (FailureOf(LActive) = ''),
      'ordinary completed constructor retains executed evidence');
    LRetained := LActiveProbe.Projection;
    LCompiler := nil;
    LActive.Cancel;
    Check((LActive.State = scsCompleted) and (LActiveProbe.Calls = 1),
      'completion wins before later shutdown/cancellation');
    LActivePort := nil;
    LActive := nil;
    Check((GProbes = 0) and (LRetained.State = spsExecuted) and
      (Pos('A completed source operation', LRetained.Design) > 0),
      'executed projection survives compiler, callback and token ownership');

    LCompiler := NewHost;
    LActivePort := NewProbe(LActiveProbe);
    LActiveProbe.ThrowOnComplete := True;
    LActive := LCompiler.Start(PlainSource, LActivePort);
    WaitTerminal(LActive);
    Check((LActive.State = scsFailed) and (LActiveProbe.Calls = 1) and
      (LActiveProbe.Projection.State = spsExecuted) and
      (FailureOf(LActive) = 'Deliberate completion-port failure'),
      'real executed constructor with throwing port reports failed delivery');
    LCompiler := nil;
    LActivePort := nil;
    Check(GProbes = 0, 'failed running completion releases its port');
  finally
    LCompiler := nil;

    if LActive <> nil then
    begin
      LActive.Cancel;
      WaitTerminal(LActive);
    end;
    LNamesAfter.Free;
    LNamesBefore.Free;
  end;
end;

procedure OrdinaryEditorChecks;
var
  LCompiler: INyxSourceCompiler;
  LJourney: INyxSourceCompilationJourney;
  LSource: TNyxText;
  LFailure: TNyxText;
  LStarted: QWord;
begin
  LSource := ReadText(GDirectories.SourceRoot + 'tests/fixtures/nyx.projection.fixture.pas');
  LFailure := 'unit nyx.projection.fixture;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.model; function BuildNyxDocument: TNyxDocument;' + #10 +
    'implementation uses SysUtils; function BuildNyxDocument: TNyxDocument;' + #10 +
    'begin' + #10 + '  raise Exception.Create(''Native shutdown editor qualification'');' + #10 +
    'end;' + #10 + 'end.' + #10;
  LCompiler := NewHost;
  LJourney := StartNyxSourceCompilationJourney(LCompiler, LSource, LFailure,
    ExpectedNyxProjectionDesign);
  LStarted := GetTickCount64;
  while not LJourney.Done and (GetTickCount64 - LStarted < 180000) do
  begin
    CheckSynchronize(1);
    LJourney.Pump;
    Sleep(1);
  end;
  Check(LJourney.Done, 'ordinary source-command journey completes');
  WriteLn('PASS ordinary editor source shutdown regression ', LJourney.Checks);
  LJourney := nil;
  LCompiler := nil;
end;

var
  LTools: TNyxDataValue;
  LConfiguration: TNyxOutputConfiguration;
begin

  if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, local toolchain JSON and a NEW isolated runtime');
  end;
  GDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
  GPolicy := TNyxProjectionStoragePolicy.Default.MinimumFreeBytes(0);
  ForceDirectories(GDirectories.RuntimeRoot);
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LConfiguration := TNyxOutputConfiguration.Create;
  try
    LConfiguration.SetField('fpc', LTools.Field('FPC').AsText);
    GProfile := LConfiguration.Encode;
  finally
    LConfiguration.Free;
  end;
  ShutdownChecks;
  WriteLn('PASS native source shutdown ', GChecks);
  OrdinaryEditorChecks;
  Check(GProbes = 0, 'all independent probe ports retired');
  WriteLn('PASS native source shutdown and consumer ', GChecks);
end.
