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

program nyx_file_open_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, Math, nyx.text, nyx.bytes, nyx.data, nyx.files,
  nyx.files.save, nyx.files.open, nyx.resources, nyx.scheduler,
  nyx.flow.types, nyx.logic, nyx.logic.files.open
  {$ifdef PAS2JS}, Web{$endif};

type
  { Deterministic shared host request. It exposes only protected lifecycle entry
    points, retaining no widgets or application and performing no platform I/O. }
  TRequest = class(TNyxFileOpenRequest)
  public
    procedure Keep;
    procedure Finish(AOutcome: TNyxFileOpenOutcome; const AFile: INyxFileSnapshot);
  end;
  { Borrowed receiver outlives its request tokens. Its retained snapshot tests
    independent ownership after both the request and source array have retired. }
  TReceiver = class
  public
    Calls: Integer;
    Last: INyxFileSnapshot;
    Outcome: TNyxFileOpenOutcome;
    procedure Received(AOutcome: TNyxFileOpenOutcome; const AFile: INyxFileSnapshot;
      const AError: TNyxText);
  end;

  { Adversarial foreign snapshot models a getter that retires the owning run
    and then raises. It owns only a pure scope, never the caller or its receiver. }
  TFaultSnapshot = class(TInterfacedObject, INyxFileSnapshot)
  public
    Scope: INyxCancellationScope;
    function Metadata: TNyxFileMetadata;
    function Bytes: TNyxBytes;
    function AsResource(AKind: TNyxResourceKind): INyxResourceDefinition;
  end;

  { Inline foreign host tests the public extension boundary without platform
    substitution in real-control journeys. No callback is retained after return. }
  TInlineHost = class(TInterfacedObject, INyxFileOpenHost)
  public
    FileValue: INyxFileSnapshot;
    Outcome: TNyxFileOpenOutcome;
    function BeginOpen(const AOptions: TNyxFileOpenOptions;
      const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
      AReply: TNyxFileOpenReply): INyxFileOpenRequest;
  end;

  { Pure terminal-frame observer. The local interface lease outlives every test
    invocation; fields distinguish silent retirement from an admitted failure. }
  TCompletion = class(TInterfacedObject, INyxLogicCompletion)
  public
    Successes: Integer;
    Failures: Integer;
    Outputs: TNyxFlowValues;
    procedure Succeed(const AOutputs: TNyxFlowValues);
    procedure Fail(const AReason: TNyxText);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure TRequest.Keep;
begin
  RetainPending;
end;

procedure TRequest.Finish(AOutcome: TNyxFileOpenOutcome; const AFile: INyxFileSnapshot);
begin
  Complete(AOutcome, AFile);
end;

procedure TReceiver.Received(AOutcome: TNyxFileOpenOutcome; const AFile: INyxFileSnapshot;
  const AError: TNyxText);
begin
  Inc(Calls);
  Last := AFile;
  Outcome := AOutcome;
end;

function TFaultSnapshot.Metadata: TNyxFileMetadata;
begin
  Result := Default(TNyxFileMetadata);

  if Scope <> nil then
  begin
    Scope.Cancel;
  end;
  raise Exception.Create('Foreign snapshot getter refused');
end;

function TFaultSnapshot.Bytes: TNyxBytes;
begin
  Result := nil;
end;

function TFaultSnapshot.AsResource(AKind: TNyxResourceKind): INyxResourceDefinition;
begin
  Result := nil;
end;

function TInlineHost.BeginOpen(const AOptions: TNyxFileOpenOptions;
  const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
  AReply: TNyxFileOpenReply): INyxFileOpenRequest;
begin
  Result := nil;
  AReply(Outcome, FileValue, '');
end;

procedure TCompletion.Succeed(const AOutputs: TNyxFlowValues);
begin
  Inc(Successes);
  Outputs := AOutputs;
end;

procedure TCompletion.Fail(const AReason: TNyxText);
begin
  Inc(Failures);
end;

procedure Run;
const
  CInvalid: array[0..3] of TNyxText = (
    '{"version":1,"title":"Open","maximumBytes":"1","extensions":[]}',
    '{"version":1,"title":"Open","maximumBytes":1,"extensions":[],"path":"x"}',
    '{"version":2,"title":"Open","maximumBytes":1,"extensions":[]}',
    '{"version":1,"title":"Open","maximumBytes":1,"extensions":["../json"]}');
var
  LBase: TNyxFileOpenOptions;
  LCopy: TNyxFileOpenOptions;
  LMetadata: TNyxFileMetadata;
  LFile: INyxFileSnapshot;
  LEmpty: INyxFileSnapshot;
  LAdmitted: INyxFileSnapshot;
  LBytes: TNyxBytes;
  LResult: TNyxBytes;
  LResource: INyxResourceDefinition;
  LReceiver: TReceiver;
  LRequest: TRequest;
  LToken: INyxFileOpenRequest;
  LScope: INyxCancellationScope;
  LScheduler: INyxScheduler;
  LRejected: Boolean;
  LIndex: Integer;
  LHostObject: TInlineHost;
  LHost: INyxFileOpenHost;
  LFault: TFaultSnapshot;
  LOperation: INyxOpenFile;
  LInvocation: INyxLogicInvocation;
  LCompletion: TCompletion;
  LCompletionLease: INyxLogicCompletion;
begin
  LReceiver := TReceiver.Create;
  LScheduler := NewNyxScheduler;
  try
    LBase := NyxFileOpenOptions.Title('Open a report').Allow('JSON').MaximumBytes(128);
    LCopy := LBase.Allow('txt').MaximumBytes(0);
    Check((LBase.ExtensionCount = 1) and (LCopy.ExtensionCount = 2),
      'options detach extension hints before partial edits');
    Check((LBase.ByteLimit = 128) and (LCopy.ByteLimit = 0),
      'zero budget explicitly admits empty files only');
    Check(LBase.Extension(0) = 'json', 'extension hints retain their shared normalized contract');
    Check(TNyxFileOpenOptions.FromData(LBase.ToData).ToData.ToJSON = LBase.ToData.ToJSON,
      'typed options round trip without machine paths or host authority');
    for LIndex := 0 to High(CInvalid) do
    begin
      LRejected := False;
      try
        LCopy := TNyxFileOpenOptions.FromData(TNyxDataValue.ParseJSON(CInvalid[LIndex]));
      except
        LRejected := True;
      end;
      Check(LRejected, 'malformed typed configuration refuses atomically');
    end;
    LRejected := False;
    try
      LCopy := Default(TNyxFileOpenOptions);
      LCopy.Validate;
    except
      LRejected := True;
    end;
    Check(LRejected, 'uninitialized options never invent a working configuration');
    LBytes := NyxEncodeUTF8('Hello 🌙' + #0 + 'world');
    LMetadata := NyxFileMetadata('report-🌙.txt', Length(LBytes))
      .MediaType(NyxMediaType(nfmtText)).ModifiedMilliseconds(1792036800123.0);
    LFile := NyxFileSnapshot(LMetadata, LBytes);
    LBytes[0] := 0;
    LResult := LFile.Bytes;
    Check(LResult[0] = Ord('H'), 'snapshot detaches caller bytes at construction');
    LResult[0] := 0;
    LResult := LFile.Bytes;
    Check(LResult[0] = Ord('H'), 'snapshot byte reads are independent on both targets');
    Check(NyxDecodeUTF8(LResult) = TNyxText('Hello 🌙' + #0 + 'world'),
      'supplementary Unicode and NUL survive exact raw file bytes');
    LMetadata := LFile.Metadata;
    Check((LMetadata.Name = TNyxText('report-🌙.txt')) and
      (LMetadata.ByteCount = Length(LResult)), 'copied metadata retains leaf and complete size');
    Check(LMetadata.HasContentType and LMetadata.HasModified,
      'known host observations remain distinguishable from unavailable metadata');
    Check(LMetadata.ToData.Field('lastModifiedMilliseconds').AsNumber = 1792036800123.0,
      'metadata preserves exact integral Unix milliseconds');
    LMetadata := NyxFileMetadata('empty.bin', 0);
    Check((LMetadata.ToData.Field('mediaType').Kind = ndNull) and
      (LMetadata.ToData.Field('lastModifiedMilliseconds').Kind = ndNull),
      'unavailable metadata stays explicit instead of fabricated values');
    LEmpty := NyxFileSnapshot(LMetadata, nil);
    LAdmitted := AdmitNyxFileOpenResult(nfooRead, LEmpty, '', 0);
    Check((LAdmitted.Metadata.ByteCount = 0) and (Length(LAdmitted.Bytes) = 0),
      'empty files remain valid completed snapshots');
    LResource := LFile.AsResource(nrkText);
    Check(NyxDecodeUTF8(LResource.Bytes) = TNyxText('Hello 🌙' + #0 + 'world'),
      'file snapshots explicitly enter the existing packed resource abstraction');
    LAdmitted := AdmitNyxFileOpenResult(nfooRead, LFile, '', 128);
    Check(LAdmitted.Metadata.ToData.ToJSON = LFile.Metadata.ToData.ToJSON,
      'foreign host receipt admission retains exact detached metadata');
    LRejected := False;
    try
      LAdmitted := AdmitNyxFileOpenResult(nfooRead, LFile, '', 0);
    except
      LRejected := True;
    end;
    Check(LRejected, 'host receipts cannot exceed the admitted read budget');
    LRejected := False;
    try
      LAdmitted := AdmitNyxFileOpenResult(nfooCancelled, LFile, '', 128);
    except
      LRejected := True;
    end;
    Check(LRejected, 'unread outcomes cannot smuggle payloads into output frames');
    LRejected := False;
    try
      LAdmitted := AdmitNyxFileOpenResult(nfooRead, nil, '', 128);
    except
      LRejected := True;
    end;
    Check(LRejected, 'a read outcome requires its complete payload');
    LRejected := False;
    try
      LMetadata := NyxFileMetadata('../report.txt', 1);
    except
      LRejected := True;
    end;
    Check(LRejected, 'portable metadata names never confer machine path authority');
    LRejected := False;
    try
      LMetadata := NyxFileMetadata('report.txt', 1).ModifiedMilliseconds(1.5);
    except
      LRejected := True;
    end;
    Check(LRejected, 'modification time refuses fractional millisecond claims');
    LRejected := False;
    try
      LMetadata := NyxFileMetadata('report.txt', 1).ModifiedMilliseconds(Infinity);
    except
      LRejected := True;
    end;
    Check(LRejected, 'nonfinite host timestamps refuse');
    LScope := NewNyxCancellationScope;
    LRequest := TRequest.Create(LBase, LScheduler, LScope, LReceiver.Received);
    LToken := LRequest;
    LRequest.Keep;
    LRequest.Finish(nfooRead, LFile);
    Check((LReceiver.Calls = 1) and (LReceiver.Last <> nil),
      'a completed read publishes one immutable result');
    LRequest.Finish(nfooRead, LFile);
    Check(LReceiver.Calls = 1, 'late duplicate platform results cannot revive a completed receiver');
    LToken := nil;
    LRequest := TRequest.Create(LBase, LScheduler, LScope, LReceiver.Received);
    LToken := LRequest;
    LRequest.Keep;
    LScope.Cancel;
    LRequest.Finish(nfooRead, LFile);
    Check(LReceiver.Calls = 1, 'scope cancellation silently revokes late platform results');
    LToken := nil;
    LScope := nil;
    LRequest := TRequest.Create(LBase, LScheduler, nil, LReceiver.Received);
    LToken := LRequest;
    LRequest.Keep;
    LRequest.Finish(nfooCancelled, nil);
    Check((LReceiver.Calls = 2) and (LReceiver.Outcome = nfooCancelled) and (LReceiver.Last = nil),
      'user cancellation remains a usable distinct host outcome');
    LToken := nil;

    LCompletion := TCompletion.Create;
    LCompletionLease := LCompletion;
    LHostObject := TInlineHost.Create;
    LHost := LHostObject;
    LFault := TFaultSnapshot.Create;
    LHostObject.FileValue := LFault;
    LScope := NewNyxCancellationScope;
    LFault.Scope := LScope;
    LOperation := NewNyxOpenFile(LBase, LHost);
    LInvocation := LOperation.BeginInvoke(NyxFlowValues.WithValue(
      NyxOpenFilePort(nofpTrigger), NyxFlowSignal), LScheduler, LScope, LCompletionLease);
    Check(LScope.Cancelled and (LCompletion.Successes = 0) and (LCompletion.Failures = 0),
      'a foreign getter that cancels then raises cannot revive its completion receiver');
    LInvocation := nil;
    LFault.Scope := nil;
    LInvocation := LOperation.BeginInvoke(NyxFlowValues.WithValue(
      NyxOpenFilePort(nofpTrigger), NyxFlowSignal), LScheduler, nil, LCompletionLease);
    Check((LCompletion.Successes = 0) and (LCompletion.Failures = 1),
      'an invalid active host outcome fails once before any partial frame');
    LInvocation := nil;
    LHostObject.FileValue := LFile;
    LInvocation := LOperation.BeginInvoke(NyxFlowValues.WithValue(
      NyxOpenFilePort(nofpTrigger), NyxFlowSignal), LScheduler, nil, LCompletionLease);
    Check((LCompletion.Successes = 1) and (LCompletion.Failures = 1) and
      LCompletion.Outputs.Value(NyxOpenFilePort(nofpRead)).AsBoolean,
      'a valid inline host completes one fully typed independent output frame');
    LInvocation := nil;
    LHostObject.FileValue := nil;
    LHostObject.Outcome := nfooCancelled;
    LInvocation := LOperation.BeginInvoke(NyxFlowValues.WithValue(
      NyxOpenFilePort(nofpTrigger), NyxFlowSignal), LScheduler, nil, LCompletionLease);
    Check((LCompletion.Successes = 2) and
      LCompletion.Outputs.Value(NyxOpenFilePort(nofpCancelled)).AsBoolean and
      not LCompletion.Outputs.Value(NyxOpenFilePort(nofpRead)).AsBoolean,
      'inline cancellation produces explicit non-read outputs instead of fake empty-file success');
    LInvocation := nil;
    LOperation := NewNyxOpenFile(LBase);
    LRejected := False;
    try
      LInvocation := LOperation.BeginInvoke(NyxFlowValues.WithValue(
        NyxOpenFilePort(nofpTrigger), NyxFlowSignal), LScheduler, nil, LCompletionLease);
    except
      LRejected := True;
    end;
    Check(LRejected and (LCompletion.Successes = 2) and (LCompletion.Failures = 1),
      'saved configuration cannot acquire runtime host authority implicitly');
  finally
    LInvocation := nil;
    LOperation := nil;
    LHost := nil;
    LCompletionLease := nil;
    LToken := nil;
    LScheduler.Shutdown;
    LReceiver.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-file-open-contract', 'passed');
    document.body.setAttribute('data-file-open-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable file-open snapshot/lifecycle checks; no platform I/O claim');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-file-open-contract', 'failed');
      document.body.setAttribute('data-file-open-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
