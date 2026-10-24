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
program nyx_clipboard_lcl_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler,
  nyx.clipboard, nyx.clipboard.lcl;

type
  { Independent harness counters outlive the managed target/host. No test ever
    constructs or calls the default OS target; platform-byte admission is pure. }
  TCounts = class
  public
    TargetsDestroyed: Integer;
    ReceiversDestroyed: Integer;
  end;

  { Explicit no-effect publication authority substitutes ONLY the OS boundary.
    The real LCL host/request, exact packet builder and shared lifecycle execute.
    Copies prove the target cannot borrow request-owned arrays after return. }
  TTarget = class(TInterfacedObject, INyxLCLClipboardTextTarget)
  public
    Counts: TCounts;
    Writes: Integer;
    Exact: TNyxBytes;
    Platform: TNyxBytes;
    IncludePlain: Boolean;
    Outcome: TNyxClipboardWriteOutcome;
    Diagnostic: TNyxText;
    ThrowOnWrite: Boolean;
    CancelScope: INyxCancellationScope;
    destructor Destroy; override;
    function WriteText(const AExactPacket: TNyxBytes;
      const APlatformUTF8: TNyxBytes; AIncludePlain: Boolean;
      out AError: TNyxText): TNyxClipboardWriteOutcome;
  end;

  { Borrowed client owns no UI or store. It can release the host/token or itself
    inside the inline terminal callback, with no later borrowed-owner accesses. }
  TReply = class
  public
    Counts: TCounts;
    Calls: Integer;
    Last: TNyxClipboardWriteResult;
    Host: INyxClipboardWriteHost;
    Token: INyxClipboardWriteRequest;
    ThrowOnReply: Boolean;
    DisposeOnReply: Boolean;
    destructor Destroy; override;
    procedure Receive(const AResult: TNyxClipboardWriteResult);
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxClipboard.Create('Native clipboard seam: ' + AReason);
  end;
  Inc(GChecks);
end;

destructor TTarget.Destroy;
begin
  Inc(Counts.TargetsDestroyed);
  inherited Destroy;
end;

function TTarget.WriteText(const AExactPacket: TNyxBytes;
  const APlatformUTF8: TNyxBytes; AIncludePlain: Boolean;
  out AError: TNyxText): TNyxClipboardWriteOutcome;
var
  LScope: INyxCancellationScope;
begin
  Inc(Writes);
  Exact := Copy(AExactPacket);
  Platform := Copy(APlatformUTF8);
  IncludePlain := AIncludePlain;
  Result := Outcome;
  AError := Diagnostic;
  LScope := CancelScope;

  if LScope <> nil then
  begin
    LScope.Cancel;
  end;

  if ThrowOnWrite then
  begin
    raise ENyxClipboard.Create('native substituted publication failure');
  end;
end;

destructor TReply.Destroy;
begin
  Inc(Counts.ReceiversDestroyed);
  inherited Destroy;
end;

procedure TReply.Receive(const AResult: TNyxClipboardWriteResult);
begin
  Inc(Calls);
  Last := AResult;
  Token := nil;
  Host := nil;

  if ThrowOnReply then
  begin
    raise ENyxClipboard.Create('native inline reply failure');
  end;

  if DisposeOnReply then
  begin
    Free;
  end;
end;

procedure RefusePacket(const ABytes: TNyxBytes; const AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    DecodeNyxLCLClipboardPacket(ABytes);
  except
    on ENyxClipboard do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

procedure PacketCases;
const
  CExact: TNyxText = 'Fox 🌙 漢字' + #13#10 + 'a' + #0 + 'b' + #10 + 'c' + #13;
  CPlain: TNyxText = 'Fox 🌙 漢字' + #13#10 + 'a' + #10 + 'b' + #13 + 'c';
  CNormalized: TNyxText = 'Fox 🌙 漢字' + #13#10 + 'a' + #13#10 + 'b' + #13#10 + 'c' + #0;
var
  LPacket: TNyxBytes;
  LCopy: TNyxBytes;
  LPlain: TNyxBytes;
  LText: TNyxText;
  LRefused: Boolean;
begin
  LPacket := EncodeNyxLCLClipboardPacket(CExact);
  Check(Length(LPacket) = NyxLCLClipboardPacketHeaderBytes + NyxUTF8ByteCount(CExact),
    'exact framed length counts original UTF-8 bytes including NUL and Unicode');
  Check(DecodeNyxLCLClipboardPacket(LPacket) = CExact,
    'exact packet restores supplementary Unicode, mixed newlines and embedded NUL');
  LCopy := Copy(LPacket);
  SetLength(LCopy, Length(LCopy) + NyxLCLClipboardMaximumPaddingBytes);
  LCopy[High(LCopy)] := $FF;
  Check(DecodeNyxLCLClipboardPacket(LCopy) = CExact,
    'declared payload excludes bounded OS allocation padding without scanning NUL');
  SetLength(LCopy, Length(LCopy) + 1);
  RefusePacket(LCopy, 'one-byte excess allocation padding refuses');
  RefusePacket(Copy(LPacket, 0, Length(LPacket) - 1), 'truncated exact payload refuses');
  RefusePacket(nil, 'missing packet is not empty text');
  LCopy := Copy(LPacket);
  LCopy[0] := 0;
  RefusePacket(LCopy, 'unknown packet signature refuses');
  LCopy := Copy(LPacket);
  LCopy[6] := 2;
  RefusePacket(LCopy, 'future packet version refuses');
  LCopy := Copy(LPacket);
  LCopy[7] := 1;
  RefusePacket(LCopy, 'unknown packet flag refuses');
  LCopy := Copy(LPacket);
  LCopy[8] := $FF;
  LCopy[9] := $FF;
  LCopy[10] := $FF;
  LCopy[11] := $FF;
  RefusePacket(LCopy, 'unbounded declared payload refuses without allocation');
  LCopy := EncodeNyxLCLClipboardPacket('x');
  LCopy[NyxLCLClipboardPacketHeaderBytes] := $FF;
  RefusePacket(LCopy, 'malformed UTF-8 packet refuses');
  LCopy := EncodeNyxLCLClipboardPacket('');
  Check((Length(LCopy) = NyxLCLClipboardPacketHeaderBytes) and
    (DecodeNyxLCLClipboardPacket(LCopy) = ''),
    'empty exact text remains a real nonempty framed OS allocation');
  LPlain := NyxLCLClipboardPlatformText(CPlain);
  Check(NyxDecodeUTF8(LPlain) = CNormalized,
    'platform bytes preserve Unicode and normalize each newline exactly once with one terminator');
  Check(NyxDecodeUTF8(NyxLCLClipboardPlatformText('')) = #0,
    'empty interoperable text still has one OS terminator');
  LRefused := False;
  try
    NyxLCLClipboardPlatformText(CExact);
  except
    on ENyxClipboard do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'interoperable text refuses embedded NUL before a platform effect');
  SetLength(LText, NyxClipboardMaximumBytes + 1);
  LRefused := False;
  try
    EncodeNyxLCLClipboardPacket(LText);
  except
    on ENyxClipboard do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'exact builder preserves the shared byte budget');
end;

procedure HostCases;
const
  CExact: TNyxText = 'Copy 🌙' + #13#10 + 'x' + #0 + 'y' + #10;
  CPlain: TNyxText = 'Copy 🌙' + #13#10 + 'x' + #10;
var
  LCounts: TCounts;
  LTarget: TTarget;
  LTargetLease: INyxLCLClipboardTextTarget;
  LHost: INyxClipboardWriteHost;
  LToken: INyxClipboardWriteRequest;
  LScheduler: INyxScheduler;
  LScope: INyxCancellationScope;
  LReply: TReply;
  LRawReply: TReply;
  LOutcome: TNyxClipboardWriteOutcome;
  LBefore: Integer;
  LCalls: Integer;
  LRefused: Boolean;
begin
  LCounts := TCounts.Create;
  LReply := TReply.Create;
  LReply.Counts := LCounts;
  LRawReply := nil;
  LScheduler := NewNyxScheduler;
  LTarget := TTarget.Create;
  LTarget.Counts := LCounts;
  LTarget.Outcome := ncwoWritten;
  LTargetLease := LTarget;
  try
    LHost := NewNyxLCLClipboardWriteHost(LTargetLease);
    LToken := LHost.BeginWriteText(CExact, LScheduler, nil, LReply.Receive);
    Check((LReply.Calls = 1) and (LReply.Last.Outcome = ncwoWritten) and
      (LReply.Last.Representation = nctrExactUTF8) and
      (LReply.Last.ByteCount = NyxUTF8ByteCount(CExact)),
      'real inline native host reports complete exact-only NUL receipt');
    Check((LTarget.Writes = 1) and not LTarget.IncludePlain and
      (Length(LTarget.Platform) = 0) and
      (DecodeNyxLCLClipboardPacket(LTarget.Exact) = CExact),
      'NUL never advertises truncating interoperable text to native publication target');
    LToken.Cancel;
    LToken.Cancel;
    Check(LReply.Calls = 1, 'repeated terminal cancellation never replays inline reply');
    LToken := LHost.BeginWriteText(CPlain, LScheduler, nil, LReply.Receive);
    Check((LReply.Last.Representation = nctrExactUTF8WithPlatformText) and
      LTarget.IncludePlain and (DecodeNyxLCLClipboardPacket(LTarget.Exact) = CPlain) and
      (NyxDecodeUTF8(LTarget.Platform) = TNyxText('Copy 🌙') + #13#10 + 'x' + #13#10#0),
      'no-NUL multiline publishes original exact bytes alongside explicit platform text');
    LToken := LHost.BeginWriteText('', LScheduler, nil, LReply.Receive);
    Check((LReply.Last.Outcome = ncwoWritten) and (LReply.Last.ByteCount = 0) and
      (Length(LTarget.Exact) = NyxLCLClipboardPacketHeaderBytes) and
      (NyxDecodeUTF8(LTarget.Platform) = #0), 'empty text is an actual two-format write');
    LHost := NewNyxLCLClipboardWriteHost(LTargetLease, nctmPlatformText);
    LToken := LHost.BeginWriteText(CPlain, LScheduler, nil, LReply.Receive);
    Check((LReply.Last.Representation = nctrPlatformText) and LTarget.IncludePlain and
      (Length(LTarget.Exact) = 0) and (LReply.Last.ByteCount = NyxUTF8ByteCount(CPlain)),
      'explicit platform mode names weaker representation without original-byte miscount');
    LBefore := LTarget.Writes;
    LToken := LHost.BeginWriteText(CExact, LScheduler, nil, LReply.Receive);
    Check((LTarget.Writes = LBefore) and (LReply.Last.Outcome = ncwoUnavailable) and
      (LReply.Last.Representation = nctrNone) and (LReply.Last.ByteCount = 0),
      'platform-mode NUL refuses with no fake or actual clipboard write');
    LHost := NewNyxLCLClipboardWriteHost(LTargetLease);
    for LOutcome := Low(TNyxClipboardWriteOutcome) to High(TNyxClipboardWriteOutcome) do
    begin
      LTarget.Outcome := LOutcome;
      LToken := LHost.BeginWriteText(CPlain, LScheduler, nil, LReply.Receive);
      Check((LReply.Last.Outcome = LOutcome) and
        ((LOutcome = ncwoWritten) or ((LReply.Last.ByteCount = 0) and
          (LReply.Last.Representation = nctrNone))),
        'target outcome preserved without inventing bytes for a nonwrite');
    end;
    LTarget.Outcome := ncwoWritten;
    LTarget.ThrowOnWrite := True;
    LCalls := LReply.Calls;
    LToken := LHost.BeginWriteText(CPlain, LScheduler, nil, LReply.Receive);
    Check((LReply.Calls = LCalls + 1) and (LReply.Last.Outcome = ncwoFailed) and
      (LReply.Last.Error <> ''), 'actual target exception becomes one typed failure reply');
    LTarget.ThrowOnWrite := False;
    LScope := NewNyxCancellationScope;
    LScope.Cancel;
    LBefore := LTarget.Writes;
    LCalls := LReply.Calls;
    LToken := LHost.BeginWriteText(CPlain, LScheduler, LScope, LReply.Receive);
    Check((LTarget.Writes = LBefore) and (LReply.Calls = LCalls),
      'already cancelled scope prevents target entry and silently retires reply');
    LScope := NewNyxCancellationScope;
    LTarget.CancelScope := LScope;
    LToken := LHost.BeginWriteText(CPlain, LScheduler, LScope, LReply.Receive);
    Check((LTarget.Writes = LBefore + 1) and (LReply.Calls = LCalls),
      'scope cancellation inside actual target cannot undo effect or reach stale client');
    LTarget.CancelScope := nil;
    LScope := nil;
    LReply.ThrowOnReply := True;
    LRefused := False;
    try
      LHost.BeginWriteText(CPlain, LScheduler, nil, LReply.Receive);
    except
      on LError: ENyxClipboard do
      begin
        LRefused := LError.Message = 'native inline reply failure';
      end;
    end;
    Check(LRefused and (LReply.Calls = LCalls + 1),
      'inline receiver exception propagates unchanged once, outside platform catch');
    LReply.ThrowOnReply := False;
    LRefused := False;
    try
      NewNyxLCLClipboardWriteHost(nil);
    except
      on ENyxClipboard do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'missing explicit native publication authority refuses construction');
    LBefore := LCounts.TargetsDestroyed;
    LReply.Host := LHost;
    LHost := nil;
    LTargetLease := nil;
    LToken := LReply.Host.BeginWriteText(CPlain, LScheduler, nil, LReply.Receive);
    Check((LReply.Host = nil) and (LCounts.TargetsDestroyed = LBefore + 1),
      'client releases last host and target while entered request keeps exact authority alive');
    LTarget := TTarget.Create;
    LTarget.Counts := LCounts;
    LTarget.Outcome := ncwoWritten;
    LTargetLease := LTarget;
    LHost := NewNyxLCLClipboardWriteHost(LTargetLease);
    LRawReply := LReply;
    LReply := nil;
    LRawReply.DisposeOnReply := True;
    LBefore := LCounts.ReceiversDestroyed;
    LToken := LHost.BeginWriteText(CPlain, LScheduler, nil, LRawReply.Receive);
    LRawReply := nil;
    Check(LCounts.ReceiversDestroyed = LBefore + 1,
      'inline callback can dispose its borrowed receiver without later owner access');
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LReply.Free;
    LRawReply.Free;
    LHost := nil;
    LScope := nil;
    LTargetLease := nil;
    LScheduler.Shutdown;
    LScheduler := nil;
    LCounts.Free;
  end;
end;

begin
  try
    PacketCases;
    HostCases;
    WriteLn('PASS ', GChecks, ' native clipboard seam checks; no system clipboard reads or writes');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
