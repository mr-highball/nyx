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

program nyx_logic_file_save_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.files, nyx.files.save,
  nyx.logic, nyx.logic.files, nyx.flow.types, nyx.scheduler
  {$ifdef PAS2JS}, Web{$endif};

type
  { Test-owned request exposing the shared boundary without file authority.
    Independent interface leases keep it alive through each admission check. }
  TRequest = class(TNyxFileSaveRequest)
  public
    procedure Finish(AOutcome: TNyxFileSaveOutcome; ACount: Integer);
    function Payload: TNyxBytes;
  end;

  { Borrowed receiver outliving all base requests; retains portable results. }
  TReply = class
  public
    Calls: Integer;
    Last: TNyxFileSaveResult;
    procedure Receive(const AResult: TNyxFileSaveResult);
  end;

  { Independently retained token records host retirement without owning callers. }
  TToken = class(TInterfacedObject, INyxFileSaveRequest)
  public
    Cancelled: Boolean;
    procedure Cancel;
  end;

  { Substituted host, never disk evidence. Borrows the invocation callback and
    can deliberately send malformed/late receipts to test publication guards. }
  THost = class(TInterfacedObject, INyxFileSaveHost)
  public
    CompleteInline: Boolean;
    NoToken: Boolean;
    Calls: Integer;
    Reply: TNyxFileSaveReply;
    Receipt: TNyxFileSaveResult;
    Token: TToken;
    TokenLease: INyxFileSaveRequest;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

  { Managed completion retains complete graph values without UI back-references. }
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

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('File save: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TRequest.Finish(AOutcome: TNyxFileSaveOutcome; ACount: Integer);
begin
  Complete(AOutcome, FOptions.SuggestedName, ACount);
end;

function TRequest.Payload: TNyxBytes;
begin
  Result := FBytes;
end;

procedure TReply.Receive(const AResult: TNyxFileSaveResult);
begin
  Inc(Calls);
  Last := AResult;
end;

procedure TToken.Cancel;
begin
  Cancelled := True;
end;

function THost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  AReply: TNyxFileSaveReply): INyxFileSaveRequest;
begin
  AScheduler.RequireUI;
  AOptions.Admit(ABytes);
  Inc(Calls);
  Reply := AReply;
  Token := TToken.Create;
  TokenLease := Token;
  Result := TokenLease;

  if NoToken then
  begin
    Result := nil;
  end;

  if CompleteInline then
  begin
    Reply(Receipt);
  end;
end;

procedure TCompletion.Succeed(const AOutputs: TNyxFlowValues);
begin
  Inc(Successes);
  Outputs := AOutputs;
end;

procedure TCompletion.Fail(const AReason: TNyxText);
begin
  Check(AReason <> '', 'rejected receipt supplies a diagnostic');
  Inc(Failures);
end;

procedure Run;
const
  CExact: TNyxText = 'Exact Unicode / 🌙 / 漢字' + #0 + #10;
var
  LScheduler: INyxScheduler;
  LScope: INyxCancellationScope;
  LOptions: TNyxFileSaveOptions;
  LCopy: TNyxFileSaveOptions;
  LBytes: TNyxBytes;
  LRequest: TRequest;
  LRequestLease: INyxFileSaveRequest;
  LReceiver: TReply;
  LHost: THost;
  LHostLease: INyxFileSaveHost;
  LCompletion: TCompletion;
  LCompletionLease: INyxLogicCompletion;
  LOperation: INyxSaveFile;
  LInvocation: INyxLogicInvocation;
  LInputs: TNyxFlowValues;
  LRejected: Boolean;
  LIndex: Integer;
  LOutcome: TNyxFileSaveOutcome;
begin
  LScheduler := NewNyxScheduler;
  LReceiver := TReply.Create;
  try
    LBytes := NewNyxUTF8Encode.Encode(CExact);
    Check(NyxDecodeUTF8(LBytes) = CExact, 'encoding retains supplementary text and NUL');
    LOptions := NyxFileSaveOptions('notes-🌙.json').Allow('JSON')
      .MediaType(NyxMediaType(nfmtJSON)).MaximumBytes(2048);
    LCopy := LOptions.Allow('txt').Title('Save report').Mode(nfsmDownload)
      .DownloadActivation(nfdaPlatform).AllTypes(False);
    Check((LOptions.ExtensionCount = 1) and (LCopy.ExtensionCount = 2) and
      (LOptions.SaveMode = nfsmPreferPicker), 'fluent copies retain independent option arrays');
    Check(TNyxFileSaveOptions.FromData(LCopy.ToData).ToData.ToJSON = LCopy.ToData.ToJSON,
      'versioned options round trip exact typed choices');
    Check((LOptions.ContentType.Name = 'application/json') and
      (LOptions.Extension(0) = 'json'), 'MIME and extension choices normalize independently');
    for LIndex := 0 to 6 do
    begin
      LRejected := False;
      try
        case LIndex of
          0: LCopy := LOptions.Suggest('../notes.json');
          1: LCopy := LOptions.MaximumBytes(-1);
          2: LCopy := LOptions.MaximumBytes(4194305);
          3: LCopy := LOptions.Allow('bad|*.exe');
          4: LCopy := NyxFileSaveOptions('x').AllTypes(False);
          5: LCopy := LOptions.MediaType(NyxMediaType('invalid'));
          6: LOptions.MaximumBytes(0).Admit(LBytes);
        end;
      except
        on E: Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'invalid options refuse before effects ' + IntToStr(LIndex));
    end;
    LCopy := NyxFileSaveOptions('empty').MaximumBytes(0);
    LCopy.Admit(nil);
    Check(LCopy.ByteLimit = 0, 'zero budget admits an empty file');

    LScope := NewNyxCancellationScope;
    LRequest := TRequest.Create(LOptions, LBytes, LScheduler, LScope, LReceiver.Receive);
    LRequestLease := LRequest;
    LBytes[0] := 0;
    Check(NyxDecodeUTF8(LRequest.Payload) = CExact, 'request snapshots borrowed bytes');
    LRejected := False;
    try
      LRequest.Finish(nfsoWritten, 1);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LReceiver.Calls = 0), 'incomplete receipt refuses before callback');
    LRequest.Finish(nfsoWritten, Length(LRequest.Payload));
    LRequest.Finish(nfsoFailed, 0);
    Check((LReceiver.Calls = 1) and NyxFileSaveWritten(LReceiver.Last),
      'complete request delivers exactly once');
    LRequestLease := nil;
    LRequest := TRequest.Create(LOptions, nil, LScheduler, LScope, LReceiver.Receive);
    LRequestLease := LRequest;
    LScope.Cancel;
    LRequest.Finish(nfsoWritten, 0);
    Check(LReceiver.Calls = 1, 'scope retirement suppresses completion');
    LRequest.Cancel;
    LRequest.Cancel;
    LRequestLease := nil;

    LBytes := NewNyxUTF8Encode.Encode(CExact);
    LInputs := NyxFlowValues.WithValue(NyxFilePort(nfpTrigger), NyxFlowSignal)
      .WithValue(NyxFilePort(nfpBytes), NyxFlowBytes(LBytes));
    for LOutcome := Low(TNyxFileSaveOutcome) to High(TNyxFileSaveOutcome) do
    begin
      LHost := THost.Create;
      LHostLease := LHost;
      LHost.CompleteInline := True;
      LHost.Receipt := Default(TNyxFileSaveResult);
      LHost.Receipt.Name := 'result-🌙.json';
      LHost.Receipt.Outcome := LOutcome;

      if NyxFileSaveDelivered(LHost.Receipt) then
      begin
        LHost.Receipt.ByteCount := Length(LBytes);
      end;
      LCompletion := TCompletion.Create;
      LCompletionLease := LCompletion;
      LOperation := NewNyxSaveFile(LOptions, LHostLease);
      LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, nil, LCompletionLease);
      Check((LCompletion.Successes = 1) and (LCompletion.Failures = 0) and
        (LCompletion.Outputs.Value(NyxFilePort(nfpOutcome)).AsInteger = Ord(LOutcome)),
        'each host outcome is complete usable graph data');
      Check(LHost.Token.Cancelled and
        (LCompletion.Outputs.Value(NyxFilePort(nfpWritten)).AsBoolean = (LOutcome = nfsoWritten)) and
        (LCompletion.Outputs.Value(NyxFilePort(nfpDelivered)).AsBoolean = NyxFileSaveDelivered(LHost.Receipt)),
        'inline completion retires its token and preserves write/download distinction');
      LHost.Reply(LHost.Receipt);
      Check(LCompletion.Successes = 1, 'duplicate host reply cannot republish');
      LInvocation := nil;
      LOperation := nil;
      LCompletionLease := nil;
      LHostLease := nil;
    end;

    for LIndex := 0 to 5 do
    begin
      LHost := THost.Create;
      LHostLease := LHost;
      LHost.Receipt := Default(TNyxFileSaveResult);
      LHost.Receipt.Name := 'result.json';
      LHost.Receipt.Outcome := nfsoWritten;
      LHost.Receipt.ByteCount := Length(LBytes);
      LCompletion := TCompletion.Create;
      LCompletionLease := LCompletion;
      LScope := NewNyxCancellationScope;
      LOperation := NewNyxSaveFile(LOptions, LHostLease);
      LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
      Check(LCompletion.Successes = 0, 'deferred host waits for completion');
      case LIndex of
        0: LInvocation.Cancel;
        1: LScope.Cancel;
        2: LHost.Receipt.ByteCount := 1;
        3: LHost.Receipt.Name := '../private/path';
        4: LHost.Receipt.Error := TNyxText(StringOfChar('a', 8193));
        5: LHost.Receipt.Outcome := nfsoCancelled;
      end;
      LHost.Reply(LHost.Receipt);

      if LIndex < 2 then
      begin
        Check((LCompletion.Successes = 0) and (LCompletion.Failures = 0) and LHost.Token.Cancelled,
          'explicit/scope cancellation silently revokes late completion');
      end
      else
      begin
        Check((LCompletion.Successes = 0) and (LCompletion.Failures = 1) and LHost.Token.Cancelled,
          'malformed foreign receipt fails and retires host authority');
      end;
      LHost.Reply(LHost.Receipt);
      Check(LCompletion.Failures <= 1, 'retired receipt cannot fail twice');
      LInvocation := nil;
      LOperation := nil;
      LCompletionLease := nil;
      LHostLease := nil;
    end;

    for LIndex := 0 to 1 do
    begin
      LHost := THost.Create;
      LHostLease := LHost;
      LHost.NoToken := True;
      LHost.CompleteInline := LIndex = 1;
      LHost.Receipt := Default(TNyxFileSaveResult);
      LHost.Receipt.Name := 'result.json';
      LHost.Receipt.Outcome := nfsoWritten;
      LHost.Receipt.ByteCount := Length(LBytes);
      LCompletion := TCompletion.Create;
      LCompletionLease := LCompletion;
      LOperation := NewNyxSaveFile(LOptions, LHostLease);
      LRejected := False;
      try
        LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, nil, LCompletionLease);
      except
        on E: Exception do
        begin
          LRejected := True;
        end;
      end;
      Check((LRejected = (LIndex = 0)) and (LCompletion.Successes = LIndex),
        'a missing token is valid only after complete inline delivery');
      LInvocation := nil;
      LOperation := nil;
      LCompletionLease := nil;
      LHostLease := nil;
    end;

    LOperation := NewNyxSaveFile(LOptions);
    LCompletion := TCompletion.Create;
    LCompletionLease := LCompletion;
    LRejected := False;
    try
      LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, nil, LCompletionLease);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LCompletion.Successes = 0), 'data-only operation has no ambient authority');
    LOperation := nil;
    LCompletionLease := nil;
    LScope := nil;
  finally
    LRequestLease := nil;
    LReceiver.Free;
    LScheduler.Shutdown;
    LScheduler := nil;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-file-save-contract', 'passed');
    document.body.setAttribute('data-file-save-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' file-save contracts; substituted host, no disk claim');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-file-save-contract', 'failed');
      document.body.setAttribute('data-file-save-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
