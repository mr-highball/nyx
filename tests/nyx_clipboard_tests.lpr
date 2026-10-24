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
program nyx_clipboard_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.clipboard
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  { Per-suite receiver owned by the harness. It outlives every request, fake host
    and reply and observes actual native/pas2js managed object destruction. }
  TCounts = class
  public
    RequestsCreated: Integer;
    RequestsDestroyed: Integer;
    HostsDestroyed: Integer;
    ReceiversDestroyed: Integer;
    Ends: Integer;
    Replies: Integer;
  end;

  { A substituted host operation exposes the real shared lifetime boundary.
    EndHost has no OS effect; controlled cleanup reentry tests terminal flags. }
  TRequest = class(TNyxClipboardWriteRequest)
  private
    FCounts: TCounts;
  protected
    procedure EndHost; override;
  public
    CancelOnEnd: Boolean;
    ThrowOnEnd: Boolean;
    EndScope: INyxCancellationScope;
    constructor Create(const AText: TNyxText; const AScheduler: INyxScheduler;
      const AScope: INyxCancellationScope; AReply: TNyxClipboardWriteReply;
      ACounts: TCounts);
    destructor Destroy; override;
    procedure Hold;
    procedure Finish(AOutcome: TNyxClipboardWriteOutcome; ABytes: Integer;
      const AError: TNyxText = '');
    procedure FinishRepresentation(AOutcome: TNyxClipboardWriteOutcome;
      ABytes: Integer; ARepresentation: TNyxClipboardTextRepresentation);
    function Payload: TNyxText;
    function IsPending: Boolean;
  end;

  { Borrowed client receiver can drop its request/host leases during delivery.
    It never owns a UI/model, and final callback disposal is tested explicitly. }
  TReply = class
  public
    Counts: TCounts;
    Calls: Integer;
    Last: TNyxClipboardWriteResult;
    Token: INyxClipboardWriteRequest;
    Host: INyxClipboardWriteHost;
    Request: TRequest;
    DisposeOnReply: Boolean;
    ThrowOnReply: Boolean;
    destructor Destroy; override;
    procedure Receive(const AResult: TNyxClipboardWriteResult);
    procedure CancelOnRead(ASender: TObject);
    procedure FinishOnRead(ASender: TObject);
  end;

  { A real INyxClipboardWriteHost implementation for lifecycle fixtures only.
    Inline and retained operations share the production base; no clipboard
    permission, platform capability or actual external write is claimed. }
  THost = class(TInterfacedObject, INyxClipboardWriteHost)
  public
    Counts: TCounts;
    InlineCompletion: Boolean;
    Outcome: TNyxClipboardWriteOutcome;
    Last: TRequest;
    destructor Destroy; override;
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

  { Deliberately outward UI-only cancellation getter tests reentry boundaries.
    This fixture does not claim worker-safe behavior for custom scope code. }
  TReadScope = class(TInterfacedObject, INyxCancellationScope)
  private
    FCancelled: Boolean;
  public
    OnRead: TNotifyEvent;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxClipboard.Create('Clipboard contract: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TRequest.Create(const AText: TNyxText; const AScheduler: INyxScheduler;
  const AScope: INyxCancellationScope; AReply: TNyxClipboardWriteReply;
  ACounts: TCounts);
begin
  inherited Create(AText, AScheduler, AScope, AReply);
  FCounts := ACounts;
  Inc(FCounts.RequestsCreated);
end;

destructor TRequest.Destroy;
begin

  if FCounts <> nil then
  begin
    Inc(FCounts.RequestsDestroyed);
  end;
  inherited Destroy;
end;

procedure TRequest.EndHost;
begin
  Inc(FCounts.Ends);

  if CancelOnEnd then
  begin
    Cancel;
  end;

  if EndScope <> nil then
  begin
    EndScope.Cancel;
  end;

  if ThrowOnEnd then
  begin
    raise ENyxClipboard.Create('Intentional clipboard host cleanup failure');
  end;
end;

procedure TRequest.Hold;
begin
  RetainPending;
end;

procedure TRequest.Finish(AOutcome: TNyxClipboardWriteOutcome; ABytes: Integer;
  const AError: TNyxText);
var
  LRepresentation: TNyxClipboardTextRepresentation;
begin
  LRepresentation := nctrNone;

  if AOutcome = ncwoWritten then
  begin
    LRepresentation := nctrExactPlainText;
  end;
  Complete(AOutcome, ABytes, AError, LRepresentation);
end;

procedure TRequest.FinishRepresentation(AOutcome: TNyxClipboardWriteOutcome;
  ABytes: Integer; ARepresentation: TNyxClipboardTextRepresentation);
begin
  Complete(AOutcome, ABytes, '', ARepresentation);
end;

function TRequest.Payload: TNyxText;
begin
  Result := FText;
end;

function TRequest.IsPending: Boolean;
begin
  Result := Pending;
end;

destructor TReply.Destroy;
begin

  if Counts <> nil then
  begin
    Inc(Counts.ReceiversDestroyed);
  end;
  inherited Destroy;
end;

procedure TReply.Receive(const AResult: TNyxClipboardWriteResult);
begin
  Inc(Calls);
  Inc(Counts.Replies);
  Last := AResult;
  Token := nil;
  Host := nil;

  if DisposeOnReply then
  begin
    Free;
    Exit;
  end;

  if ThrowOnReply then
  begin
    raise ENyxClipboard.Create('Intentional clipboard receiver failure');
  end;
end;

procedure TReply.CancelOnRead(ASender: TObject);
begin
  Token.Cancel;
  Token := nil;
end;

procedure TReply.FinishOnRead(ASender: TObject);
begin
  Request.Finish(ncwoWritten, NyxClipboardTextByteCount(Request.Payload));
end;

destructor THost.Destroy;
begin
  Inc(Counts.HostsDestroyed);
  inherited Destroy;
end;

function THost.BeginWriteText(const AText: TNyxText;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LHost: INyxClipboardWriteHost;
  LRequest: TRequest;
  LLease: INyxClipboardWriteRequest;
  LBytes: Integer;
  LOutcome: TNyxClipboardWriteOutcome;
  LInline: Boolean;
begin
  LHost := Self;
  LInline := InlineCompletion;
  LOutcome := Outcome;
  LRequest := TRequest.Create(AText, AScheduler, ACancellation, AReply, Counts);
  LLease := LRequest;
  Result := LLease;
  Last := LRequest;

  if LInline then
  begin
    LBytes := 0;

    if LOutcome = ncwoWritten then
    begin
      LBytes := NyxClipboardTextByteCount(LRequest.Payload);
    end;
    LRequest.Finish(LOutcome, LBytes);
  end
  else
  begin
    LRequest.Hold;
  end;
end;

procedure TReadScope.Cancel;
begin
  FCancelled := True;
end;

function TReadScope.GetCancelled: Boolean;
var
  LOnRead: TNotifyEvent;
begin
  LOnRead := OnRead;
  OnRead := nil;

  if Assigned(LOnRead) then
  begin
    LOnRead(Self);
  end;
  Result := FCancelled;
end;

procedure Run;
const
  CExact: TNyxText = 'Full value / 🌙 / 漢字' + #13#10 + 'next' + #10 + #0 + 'tail';
var
  LCounts: TCounts;
  LScheduler: INyxScheduler;
  LScope: INyxCancellationScope;
  LReadScope: TReadScope;
  LReply: TReply;
  LRawReply: TReply;
  LHost: THost;
  LHostLease: INyxClipboardWriteHost;
  LRequest: TRequest;
  LToken: INyxClipboardWriteRequest;
  LResult: TNyxClipboardWriteResult;
  LText: TNyxText;
  LMalformed: TNyxText;
  LOutcome: TNyxClipboardWriteOutcome;
  LRepresentation: TNyxClipboardTextRepresentation;
  LBefore: Integer;
  LBytes: Integer;
  LRejected: Boolean;
  LCase: Integer;
begin
  LCounts := TCounts.Create;
  LScheduler := NewNyxScheduler;
  LReply := TReply.Create;
  LReply.Counts := LCounts;
  LRawReply := nil;
  try
    LBytes := NyxClipboardTextByteCount(CExact);
    Check((LBytes = NyxUTF8ByteCount(CExact)) and
      (NyxClipboardTextByteCount('') = 0),
      'exact scalar admission preserves mixed newlines, NUL, supplementary text and empty data');
    {$ifdef PAS2JS}
    LMalformed := 'A' + TJSString.fromCharCode($D800);
    {$else}
    LMalformed := 'A' + TNyxText(#$F0#$80#$80);
    {$endif}
    LRejected := False;
    try
      NyxClipboardTextByteCount(LMalformed);
    except
      on ENyxClipboard do LRejected := True;
    end;
    Check(LRejected, 'malformed Unicode refuses instead of copying replacement characters');
    LRejected := False;
    try
      NyxClipboardTextByteCount(StringOfChar('x', NyxClipboardMaximumBytes + 1));
    except
      on ENyxClipboard do LRejected := True;
    end;
    Check(LRejected, 'oversized complete text refuses without truncating an accepted prefix');

    for LCase := 0 to 2 do
    begin
      LRequest := nil;
      LRejected := False;
      try
        case LCase of
          0: LRequest := TRequest.Create(CExact, nil, nil,
            {$ifdef PAS2JS}@{$endif}LReply.Receive, LCounts);
          1: LRequest := TRequest.Create(CExact, LScheduler, nil, nil, LCounts);
          2: LRequest := TRequest.Create(LMalformed, LScheduler, nil,
            {$ifdef PAS2JS}@{$endif}LReply.Receive, LCounts);
        end;
      except
        on ENyxClipboard do
        begin
          LRejected := True;
        end;
      end;
      LRequest.Free;
      Check(LRejected, 'construction refuses absent authority/receiver or malformed complete text');
    end;

    LText := CExact;
    LBefore := LCounts.RequestsDestroyed;
    LRequest := TRequest.Create(LText, LScheduler, nil,
      {$ifdef PAS2JS}@{$endif}LReply.Receive, LCounts);
    LToken := LRequest;
    LRequest.Hold;
    LText := 'caller changed its input';
    Check((LRequest.Payload = CExact) and LRequest.IsPending and (LReply.Calls = 0),
      'retained operation owns the exact original text before any delayed delivery');
    LToken := nil;
    Check(LCounts.RequestsDestroyed = LBefore,
      'dropping a token does not cancel or destroy retained pending host work');
    LRequest.Finish(ncwoWritten, LBytes);
    LRequest := nil;
    Check((LReply.Calls = 1) and NyxClipboardWritten(LReply.Last) and
      (LReply.Last.ByteCount = LBytes) and (LCounts.RequestsDestroyed = LBefore + 1),
      'completed retained request delivers once then releases its final managed lease');

    for LOutcome := Low(TNyxClipboardWriteOutcome) to High(TNyxClipboardWriteOutcome) do
    begin
      LHost := THost.Create;
      LHost.Counts := LCounts;
      LHost.InlineCompletion := True;
      LHost.Outcome := LOutcome;
      LHostLease := LHost;
      LBefore := LReply.Calls;
      LToken := LHostLease.BeginWriteText(CExact, LScheduler, nil,
        {$ifdef PAS2JS}@{$endif}LReply.Receive);
      Check((LToken <> nil) and (LReply.Calls = LBefore + 1) and
        (LReply.Last.Outcome = LOutcome) and
        (NyxClipboardWritten(LReply.Last) = (LOutcome = ncwoWritten)) and
        (NyxClipboardWriteOutcomeName(LOutcome) <> ''),
        'inline fake host preserves each distinct typed delivery/capability/permission outcome');
      LBefore := LCounts.Ends;
      LToken.Cancel;
      LToken.Cancel;
      Check(LCounts.Ends = LBefore, 'repeated cancellation after inline delivery never repeats cleanup');
      LToken := nil;
      LHostLease := nil;
    end;

    { Invalid receipts terminate the request without publishing any client result.
      Each fresh operation is a real base request, not a standalone enum check. }
    for LCase := 0 to 5 do
    begin
      LRequest := TRequest.Create(CExact, LScheduler, nil,
        {$ifdef PAS2JS}@{$endif}LReply.Receive, LCounts);
      LToken := LRequest;
      LBefore := LReply.Calls;
      LRejected := False;
      try
        case LCase of
          0: LRequest.Finish(ncwoWritten, LBytes - 1);
          1: LRequest.Finish(ncwoWritten, LBytes, 'not an exact success');
          2: LRequest.Finish(ncwoUnavailable, 1);
          3: LRequest.Finish(ncwoFailed, 0,
            StringOfChar('e', NyxClipboardMaximumErrorBytes + 1));
          4: LRequest.FinishRepresentation(ncwoWritten, LBytes, nctrNone);
          5: LRequest.FinishRepresentation(ncwoDenied, 0, nctrPlatformText);
        end;
      except
        on ENyxClipboard do LRejected := True;
      end;
      Check(LRejected and not LRequest.IsPending and (LReply.Calls = LBefore) and
        (LRequest.Payload = ''), 'invalid host receipt revokes receiver and clears its complete payload');
      LRequest.Finish(ncwoWritten, LBytes);
      Check(LReply.Calls = LBefore, 'a malformed receipt cannot later be replaced with a fresh success');
      LToken := nil;
    end;

    LResult := Default(TNyxClipboardWriteResult);
    LResult.Outcome := ncwoWritten;
    LResult.Representation := nctrExactPlainText;
    ValidateNyxClipboardWriteResult(LResult, 0);
    Check(NyxClipboardWritten(LResult), 'a confirmed empty-string write is distinct from unavailable');
    for LRepresentation := nctrExactPlainText to High(TNyxClipboardTextRepresentation) do
    begin
      LRequest := TRequest.Create(CExact, LScheduler, nil,
        {$ifdef PAS2JS}@{$endif}LReply.Receive, LCounts);
      LToken := LRequest;
      LBefore := LReply.Calls;
      LRequest.FinishRepresentation(ncwoWritten, LBytes, LRepresentation);
      Check((LReply.Calls = LBefore + 1) and
        (LReply.Last.Representation = LRepresentation) and
        (LReply.Last.ByteCount = LBytes),
        'actual shared delivery retains exact versus platform-compatible representation without changing the admitted byte count');
      LToken := nil;
    end;
    LResult.Outcome := ncwoFailed;
    LResult.Representation := nctrNone;
    LResult.Error := LMalformed;
    LRejected := False;
    try
      ValidateNyxClipboardWriteResult(LResult, 0);
    except
      on ENyxClipboard do LRejected := True;
    end;
    Check(LRejected, 'malformed diagnostics cannot cross the portable result boundary');

    for LCase := 0 to 3 do
    begin
      LScope := NewNyxCancellationScope;
      LRequest := TRequest.Create(CExact, LScheduler, LScope,
        {$ifdef PAS2JS}@{$endif}LReply.Receive, LCounts);
      LToken := LRequest;
      LBefore := LReply.Calls;
      case LCase of
        0:
          begin
            LToken.Cancel;
            LToken.Cancel;
          end;
        1: LScope.Cancel;
        2: LRequest.CancelOnEnd := True;
        3: LRequest.EndScope := LScope;
      end;
      LRequest.Finish(ncwoWritten, LBytes);
      Check((LReply.Calls = LBefore) and not LRequest.IsPending,
        'explicit, scope and cleanup-started cancellation silently suppress exact old delivery');
      LToken := nil;
      LScope := nil;
    end;

    for LCase := 0 to 1 do
    begin
      LReadScope := TReadScope.Create;
      LScope := LReadScope;
      LRequest := TRequest.Create(CExact, LScheduler, LScope,
        {$ifdef PAS2JS}@{$endif}LReply.Receive, LCounts);
      LToken := LRequest;
      LReply.Token := LToken;
      LReply.Request := LRequest;
      LBefore := LReply.Calls;

      if LCase = 0 then
      begin
        LReadScope.OnRead := {$ifdef PAS2JS}@{$endif}LReply.CancelOnRead;
      end
      else
      begin
        LReadScope.OnRead := {$ifdef PAS2JS}@{$endif}LReply.FinishOnRead;
      end;
      LRequest.Finish(ncwoWritten, LBytes);
      Check((LReply.Calls = LBefore + LCase) and not LRequest.IsPending,
        'outward Cancelled getter cannot revive cancellation or recursively deliver twice');
      LReply.Token := nil;
      LReply.Request := nil;
      LToken := nil;
      LScope := nil;
    end;

    for LCase := 0 to 1 do
    begin
      LRequest := TRequest.Create(CExact, LScheduler, nil,
        {$ifdef PAS2JS}@{$endif}LReply.Receive, LCounts);
      LToken := LRequest;
      LRequest.ThrowOnEnd := LCase = 0;
      LReply.ThrowOnReply := LCase = 1;
      LBefore := LReply.Calls;
      LRejected := False;
      try
        LRequest.Finish(ncwoWritten, LBytes);
      except
        on LException: ENyxClipboard do
        begin
          LRejected := Pos('Intentional clipboard', LException.Message) > 0;
        end;
      end;
      Check(LRejected and not LRequest.IsPending and
        (LReply.Calls = LBefore + LCase),
        'host cleanup and receiver exceptions remain visible after terminal authority retires');
      LRequest.Finish(ncwoWritten, LBytes);
      LToken.Cancel;
      Check(LReply.Calls = LBefore + LCase, 'failed callbacks are never replayed by duplicate completion');
      LReply.ThrowOnReply := False;
      LToken := nil;
    end;

    LHost := THost.Create;
    LHost.Counts := LCounts;
    LHostLease := LHost;
    LToken := LHostLease.BeginWriteText(CExact, LScheduler, nil,
      {$ifdef PAS2JS}@{$endif}LReply.Receive);
    LRequest := LHost.Last;
    LReply.Token := LToken;
    LReply.Host := LHostLease;
    LBefore := LCounts.HostsDestroyed;
    LHostLease := nil;
    LToken := nil;
    LRequest.Finish(ncwoWritten, LBytes);
    LRequest := nil;
    Check((LReply.Token = nil) and (LReply.Host = nil) and
      (LCounts.HostsDestroyed = LBefore + 1),
      'receiver may release the exact host and last caller request lease during completion');

    LRawReply := LReply;
    LRawReply.DisposeOnReply := True;
    LRequest := TRequest.Create(CExact, LScheduler, nil,
      {$ifdef PAS2JS}@{$endif}LRawReply.Receive, LCounts);
    LToken := LRequest;
    LBefore := LCounts.ReceiversDestroyed;
    LReply := nil;
    LRequest.Finish(ncwoWritten, LBytes);
    Check(LCounts.ReceiversDestroyed = LBefore + 1,
      'completion makes no borrowed receiver access after a callback disposes itself');
    LRawReply := nil;
    LToken := nil;
    Check((LCounts.RequestsCreated = LCounts.RequestsDestroyed),
      'all real managed fake-host requests retire exactly once without pending self leases');
    Check(LCounts.Ends = LCounts.RequestsCreated,
      'each adopted operation cleans its host exactly once across cancellation and exceptions');
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LHostLease := nil;
    LScope := nil;
    LReply.Free;

    if (LRawReply <> nil) and (LCounts.ReceiversDestroyed = 0) then
    begin
      LRawReply.Free;
    end;
    LScheduler.Shutdown;
    LScheduler := nil;
    LCounts.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-clipboard-contract', 'passed');
    document.body.setAttribute('data-clipboard-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' clipboard contract checks; substituted hosts, no platform write claim');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-clipboard-contract', 'failed');
      document.body.setAttribute('data-clipboard-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
