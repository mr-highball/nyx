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
unit nyx.clipboard;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.scheduler;

const
  { Direct text writes are bounded independently of a table's row count. Limits
    are UTF-8 bytes on both targets, never native character/code-unit counts. }
  NyxClipboardMaximumBytes = 4 * 1024 * 1024;
  NyxClipboardMaximumErrorBytes = 8192;

type
  { Invalid portable payloads/receipts refuse before they can claim an external
    effect. Operational capability/permission failures use the result below. }
  ENyxClipboard = class(Exception);

  { Written reports a completed host write for the ENTIRE supplied value in the
    named representation; it proves no later paste/continued clipboard ownership.
    ActivationRequired differs from denied permission/unavailable capability. Cancelled
    denotes an actual host/user cancellation; request.Cancel is silent. }
  TNyxClipboardWriteOutcome = (ncwoWritten, ncwoUnavailable,
    ncwoActivationRequired, ncwoDenied, ncwoCancelled, ncwoFailed);

  { Exact requires a lossless representation of the entire original value.
    PlatformText explicitly permits the interoperable text format's documented
    OS line-ending conversion; it never permits silent truncation. A host may
    expose this choice when constructed, without putting authority in the text. }
  TNyxClipboardTextMode = (nctmExact, nctmPlatformText);

  { A success names its actual representation, not merely the API requested.
    ExactUTF8WithPlatformText preserves the original UTF-8 bytes in a richer
    format AND supplies interoperable text that can use platform line endings.
    Consumers of only that interoperable format do not receive the exact-byte
    guarantee. ExactUTF8 has ONLY the lossless richer representation, useful
    when interoperable text would truncate NUL. None belongs to nonwrites. }
  TNyxClipboardTextRepresentation = (nctrNone, nctrExactPlainText,
    nctrPlatformText, nctrExactUTF8WithPlatformText, nctrExactUTF8);

  { Portable host receipt, with no widget/permission/native handle. Written has
    the complete ORIGINAL UTF-8 byte count, an explicit representation and no
    diagnostic; this is not a measurement of physical OS clipboard formats.
    Other outcomes have zero bytes/None. Error is bounded to 8192 UTF-8 bytes. }
  TNyxClipboardWriteResult = record
    Outcome: TNyxClipboardWriteOutcome;
    ByteCount: Integer;
    Representation: TNyxClipboardTextRepresentation;
    Error: TNyxText;
  end;

  { Borrowed UI receiver invoked at most once. Its owner must cancel before
    disposal; inline completion and releasing the request/host inside this
    callback are permitted. The copied result survives request retirement. }
  TNyxClipboardWriteReply = procedure(
    const AResult: TNyxClipboardWriteResult) of object;

  { One independently retained operation. Cancel is UI-only, idempotent and
    silently detaches its receiver. It cannot undo an already performed external
    clipboard write. Dropping the token alone does not cancel pending host work. }
  INyxClipboardWriteRequest = interface(IInterface)
    ['{34EAF88E-6429-4AC4-8C80-38FCA9B1677C}']
    procedure Cancel;
  end;

  { Explicit runtime authority, never recovered from saved text or model data.
    BeginWriteText copies/adopts the exact text BEFORE host effects. It may
    finish inline; pending work must return a request token (a terminal token is
    also valid after inline completion). Empty text is a real write, not absence.
    Exact mode never silently normalizes admitted NUL/newline/supplementary
    text: inability to preserve it reports Failed/Unavailable. PlatformText is
    an explicit host choice and its receipt names the weaker representation.
    This contract alone supplies no platform adapter, permission or registration. }
  INyxClipboardWriteHost = interface(IInterface)
    ['{B97CF627-BC0D-4DD8-BB53-812B02B9FEAF}']
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

  { Shared adapter lifetime with only copied text and scheduler/cancellation
    authority. No tree, mount, store or receiver owner is retained. Subclasses
    retain pending work before exposing delayed callbacks; they must eventually
    Complete or Cancel it. Every delayed platform callback checks Pending first.
    All request methods are UI-only; platform worker callbacks must marshal to
    the supplied scheduler before touching this object or its borrowed receiver.
    Terminal flags precede cleanup. EndHost is invoked at most once, must release
    its exact host resources and must never call the client receiver directly.
    Invalid receipts terminate silently and raise; callback failures propagate
    after terminal completion and can never cause a second delivery. }
  TNyxClipboardWriteRequest = class(TInterfacedObject, INyxClipboardWriteRequest)
  private
    FReply: TNyxClipboardWriteReply;
    FLease: INyxClipboardWriteRequest;
    FTerminal: Boolean;
    FCancelled: Boolean;
    FCompleting: Boolean;
    FHostEnded: Boolean;
    procedure EndHostOnce;
  protected
    FText: TNyxText;
    FByteCount: Integer;
    FScheduler: INyxScheduler;
    FScope: INyxCancellationScope;
    function Pending: Boolean;
    procedure RetainPending;
    procedure EndHost; virtual;
    procedure Complete(AOutcome: TNyxClipboardWriteOutcome;
      AByteCount: Integer; const AError: TNyxText = '';
      ARepresentation: TNyxClipboardTextRepresentation = nctrNone);
  public
    constructor Create(const AText: TNyxText;
      const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply);
    procedure Cancel; virtual;
  end;

{ Strict scalar/byte admission, without trimming, newline or NUL conversion.
  The returned count describes the complete copied payload on both targets. }
function NyxClipboardTextByteCount(const AText: TNyxText): Integer;
function NyxClipboardWriteOutcomeName(AOutcome: TNyxClipboardWriteOutcome): TNyxText;
function NyxClipboardWritten(const AResult: TNyxClipboardWriteResult): Boolean;
procedure ValidateNyxClipboardWriteResult(const AResult: TNyxClipboardWriteResult;
  AExpectedBytes: Integer);

implementation

uses nyx.bytes;

function NyxClipboardTextByteCount(const AText: TNyxText): Integer;
begin
  { UTF-8 bytes are never fewer than this target's string storage units. The
    cheap first bound also avoids scanning an arbitrarily oversized input. }

  if Length(AText) > NyxClipboardMaximumBytes then
  begin
    raise ENyxClipboard.Create('Clipboard text exceeds its UTF-8 byte budget');
  end;
  try
    Result := NyxUTF8ByteCount(AText);
  except
    on ENyxBytes do
    begin
      raise ENyxClipboard.Create('Clipboard text contains malformed Unicode');
    end;
  end;

  if Result > NyxClipboardMaximumBytes then
  begin
    raise ENyxClipboard.Create('Clipboard text exceeds its UTF-8 byte budget');
  end;
end;

function NyxClipboardWriteOutcomeName(AOutcome: TNyxClipboardWriteOutcome): TNyxText;
const
  CNames: array[TNyxClipboardWriteOutcome] of TNyxText =
    ('written', 'unavailable', 'activation-required', 'denied', 'cancelled', 'failed');
begin

  if (Ord(AOutcome) < Ord(Low(TNyxClipboardWriteOutcome))) or
    (Ord(AOutcome) > Ord(High(TNyxClipboardWriteOutcome))) then
  begin
    raise ENyxClipboard.Create('Unknown clipboard write outcome');
  end;
  Result := CNames[AOutcome];
end;

function NyxClipboardWritten(const AResult: TNyxClipboardWriteResult): Boolean;
begin
  Result := AResult.Outcome = ncwoWritten;
end;

procedure ValidateNyxClipboardWriteResult(const AResult: TNyxClipboardWriteResult;
  AExpectedBytes: Integer);
var
  LErrorBytes: Integer;
begin
  NyxClipboardWriteOutcomeName(AResult.Outcome);

  if (Ord(AResult.Representation) < Ord(Low(TNyxClipboardTextRepresentation))) or
    (Ord(AResult.Representation) > Ord(High(TNyxClipboardTextRepresentation))) then
  begin
    raise ENyxClipboard.Create('Unknown clipboard text representation');
  end;

  if (AExpectedBytes < 0) or (AExpectedBytes > NyxClipboardMaximumBytes) or
    (AResult.ByteCount < 0) then
  begin
    raise ENyxClipboard.Create('Invalid clipboard byte receipt');
  end;

  if Length(AResult.Error) > NyxClipboardMaximumErrorBytes then
  begin
    raise ENyxClipboard.Create('Clipboard diagnostic exceeds its UTF-8 byte budget');
  end;
  try
    LErrorBytes := NyxUTF8ByteCount(AResult.Error);
  except
    on ENyxBytes do
    begin
      raise ENyxClipboard.Create('Clipboard diagnostic contains malformed Unicode');
    end;
  end;

  if LErrorBytes > NyxClipboardMaximumErrorBytes then
  begin
    raise ENyxClipboard.Create('Clipboard diagnostic exceeds its UTF-8 byte budget');
  end;

  if NyxClipboardWritten(AResult) then
  begin

    if (AResult.ByteCount <> AExpectedBytes) or (AResult.Error <> '') or
      (AResult.Representation = nctrNone) then
    begin
      raise ENyxClipboard.Create('Clipboard success must account for the exact complete text');
    end;
  end
  else if (AResult.ByteCount <> 0) or (AResult.Representation <> nctrNone) then
  begin
    raise ENyxClipboard.Create('An unwritten clipboard outcome cannot claim written bytes');
  end;
end;

constructor TNyxClipboardWriteRequest.Create(const AText: TNyxText;
  const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply);
var
  LText: TNyxText;
  LScheduler: INyxScheduler;
  LScope: INyxCancellationScope;
  LReply: TNyxClipboardWriteReply;
begin
  inherited Create;
  { Const arguments may alias caller-owned fields. Retain each value before the
    first scheduler callout can retire or change that original receiver. }
  LText := AText;
  LScheduler := AScheduler;
  LScope := ACancellation;
  LReply := AReply;

  if (LScheduler = nil) or not Assigned(LReply) then
  begin
    raise ENyxClipboard.Create('Clipboard write requires a UI scheduler and reply');
  end;
  LScheduler.RequireUI;
  FByteCount := NyxClipboardTextByteCount(LText);
  FText := LText;
  FScheduler := LScheduler;
  FScope := LScope;
  FReply := LReply;
end;

function TNyxClipboardWriteRequest.Pending: Boolean;
var
  LLease: INyxClipboardWriteRequest;
  LScope: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LLease := Self;
  Result := False;

  if FTerminal or not Assigned(FReply) then
  begin
    Exit;
  end;
  LScope := FScope;
  LCancelled := (LScope <> nil) and LScope.Cancelled;
  { A foreign cancellation getter can synchronously Cancel or Complete this
    request. Its returned Boolean must never revive the old pending state. }
  Result := not LCancelled and not FTerminal and Assigned(FReply) and
    (FScope = LScope);
end;

procedure TNyxClipboardWriteRequest.RetainPending;
var
  LLease: INyxClipboardWriteRequest;
begin
  LLease := Self;
  FScheduler.RequireUI;

  if Pending then
  begin
    FLease := LLease;
  end;
end;

procedure TNyxClipboardWriteRequest.EndHost;
begin
  { Target subclasses release only their exact private platform operation. }
end;

procedure TNyxClipboardWriteRequest.EndHostOnce;
begin

  if FHostEnded then
  begin
    Exit;
  end;
  FHostEnded := True;
  EndHost;
end;

procedure TNyxClipboardWriteRequest.Cancel;
var
  LLease: INyxClipboardWriteRequest;
  LScope: INyxCancellationScope;
begin
  LLease := Self;
  FScheduler.RequireUI;
  FCancelled := True;

  if FTerminal then
  begin
    Exit;
  end;
  FTerminal := True;
  FReply := nil;
  LScope := FScope;
  FScope := nil;
  FText := '';
  FByteCount := 0;
  FLease := nil;
  EndHostOnce;
end;

procedure TNyxClipboardWriteRequest.Complete(AOutcome: TNyxClipboardWriteOutcome;
  AByteCount: Integer; const AError: TNyxText;
  ARepresentation: TNyxClipboardTextRepresentation);
var
  LLease: INyxClipboardWriteRequest;
  LScope: INyxCancellationScope;
  LReply: TNyxClipboardWriteReply;
  LResult: TNyxClipboardWriteResult;
  LExpectedBytes: Integer;
  LCancelled: Boolean;
begin
  LLease := Self;
  LScope := FScope;
  LExpectedBytes := FByteCount;
  LResult.Outcome := AOutcome;
  LResult.ByteCount := AByteCount;
  LResult.Representation := ARepresentation;
  LResult.Error := AError;
  FScheduler.RequireUI;

  if FTerminal or FCompleting then
  begin
    Exit;
  end;
  FCompleting := True;
  try

    if not Pending then
    begin
      Cancel;
      Exit;
    end;
    ValidateNyxClipboardWriteResult(LResult, LExpectedBytes);
    LReply := FReply;
    FTerminal := True;
    FReply := nil;
    FScope := nil;
    FText := '';
    FByteCount := 0;
    FLease := nil;
    EndHostOnce;
    { Cleanup can cancel the original scope or this exact request. Query that
      retained scope first, then inspect only our nonreviving local flags before
      invoking the detached receiver. No newer request is ever substituted. }
    LCancelled := (LScope <> nil) and LScope.Cancelled;

    if LCancelled or FCancelled then
    begin
      Exit;
    end;
    LReply(LResult);
  except
    { Preserve the original invalid-receipt, host-cleanup or receiver exception.
      Cancellation is flags-first and attempts remaining cleanup at most once. }
    try
      Cancel;
    except
      { A secondary adapter cleanup failure, including a raw browser throw,
        cannot replace the original invalid receipt or receiver failure. }
    end;
    raise;
  end;
end;

end.
