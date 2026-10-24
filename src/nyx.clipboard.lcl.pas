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
unit nyx.clipboard.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.bytes, nyx.clipboard;

const
  { The native format is framed because a movable OS allocation may contain
    padding. It is distinct from the browser's raw custom UTF-8 Blob format. }
  NyxLCLClipboardExactFormat = 'application/vnd.nyx.text.packet-v1';
  NyxLCLClipboardPacketHeaderBytes = 12;
  NyxLCLClipboardMaximumPaddingBytes = 65536;

type
  { Explicit synchronous UI publication authority. Both arrays are borrowed for
    the call and must be copied if retained. A nonempty exact packet contains
    the complete original UTF-8 text. IncludePlain requests the separately
    normalized, NUL-terminated interoperable UTF-8 bytes; False requires an
    empty platform array. Written promises verified publication of EVERY
    requested format, never merely a local clipboard cache assignment. Failure
    may follow a partial external effect and must not be retried implicitly.
    Implementations own no receiver, request, tree, state or scheduler. }
  INyxLCLClipboardTextTarget = interface(IInterface)
    ['{0A4A13A2-083A-4644-A606-75E352583272}']
    function WriteText(const AExactPacket: TNyxBytes;
      const APlatformUTF8: TNyxBytes; AIncludePlain: Boolean;
      out AError: TNyxText): TNyxClipboardWriteOutcome;
  end;

{ Strict pure packet helpers, without clipboard access. The fixed header is
  NYXTXT, version 1, reserved zero, then a little-endian uint32 UTF-8 length.
  Decode accepts only bounded OS allocation padding after that declared payload;
  it never scans for a NUL or invents a text length from GlobalSize. }
function EncodeNyxLCLClipboardPacket(const AText: TNyxText): TNyxBytes;
function DecodeNyxLCLClipboardPacket(const ABytes: TNyxBytes): TNyxText;

{ Win32 interoperable bytes, prepared without OS access. CRLF remains one pair;
  lone CR/LF becomes CRLF. Exactly one final NUL is added; embedded NUL refuses.
  The original input is subject to the portable four-MiB UTF-8 budget. }
function NyxLCLClipboardPlatformText(const AText: TNyxText): TNyxBytes;

{ Each operation copies/adopts its input before calling the managed target and
  completes synchronously on the supplied scheduler's UI thread. A reply may
  release the host/request or raise; terminal callback errors propagate once.
  Cancellation suppresses delivery and cannot undo a completed OS effect.

  The default Win32 target borrows the LCL application window for a checked OS
  clipboard transaction, without changing global TClipboard cache/batch state.
  Bounded OS-handle readback verifies the formats under that SAME lock. Other widgetsets report
  Unavailable until their publication/readback contracts are implemented; this
  remains unfinished native parity work. The explicit target overload supports
  other implementations and no-effect host tests. Neither factory writes until
  BeginWriteText. Exact NUL uses only the private channel; exact no-NUL text also
  supplies interoperable text. PlatformText explicitly permits CRLF conversion. }
function NewNyxLCLClipboardWriteHost(
  AMode: TNyxClipboardTextMode = nctmExact): INyxClipboardWriteHost; overload;
function NewNyxLCLClipboardWriteHost(const ATarget: INyxLCLClipboardTextTarget;
  AMode: TNyxClipboardTextMode = nctmExact): INyxClipboardWriteHost; overload;

implementation

uses SysUtils, Forms, LCLType, LCLIntf, InterfaceBase, LCLPlatformDef,
  nyx.scheduler
  {$ifdef MSWINDOWS}, Windows{$endif};

type
  { Built-in platform authority has no mutable request or callback fields. The
    LCL application window is borrowed only for one immediate Win32 operation;
    application shutdown must not destroy that host during its entered call.
    Global Clipbrd state, including unrelated staged data, remains untouched. }
  TLCLClipboardTarget = class(TInterfacedObject, INyxLCLClipboardTextTarget)
  public
    function WriteText(const AExactPacket: TNyxBytes;
      const APlatformUTF8: TNyxBytes; AIncludePlain: Boolean;
      out AError: TNyxText): TNyxClipboardWriteOutcome;
  end;

  { A single inline operation retains its exact target through every platform
    callback. EndHost withdraws only that lease, with no OS/focus side effects. }
  TLCLClipboardRequest = class(TNyxClipboardWriteRequest)
  private
    FTarget: INyxLCLClipboardTextTarget;
    FMode: TNyxClipboardTextMode;
  protected
    procedure EndHost; override;
  public
    constructor Create(const ATarget: INyxLCLClipboardTextTarget;
      AMode: TNyxClipboardTextMode; const AText: TNyxText;
      const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply);
    procedure Start;
  end;

  { Host policy is immutable; independent requests never retain their host or
    share completion/cancellation state. The target is explicit runtime authority. }
  TLCLClipboardHost = class(TInterfacedObject, INyxClipboardWriteHost)
  private
    FTarget: INyxLCLClipboardTextTarget;
    FMode: TNyxClipboardTextMode;
  public
    constructor Create(const ATarget: INyxLCLClipboardTextTarget;
      AMode: TNyxClipboardTextMode);
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

function SameBytes(const ALeft: TNyxBytes; const ARight: TNyxBytes): Boolean;
begin
  Result := Length(ALeft) = Length(ARight);

  if Result and (Length(ALeft) > 0) then
  begin
    Result := CompareMem(@ALeft[0], @ARight[0], Length(ALeft));
  end;
end;

function EncodeNyxLCLClipboardPacket(const AText: TNyxText): TNyxBytes;
const
  CMagic: array[0..5] of Byte = (78, 89, 88, 84, 88, 84);
var
  LPayload: TNyxBytes;
  LLength: Integer;
  LIndex: Integer;
begin
  LLength := NyxClipboardTextByteCount(AText);
  LPayload := NyxEncodeUTF8(AText);
  Result := nil;
  SetLength(Result, NyxLCLClipboardPacketHeaderBytes + LLength);
  for LIndex := 0 to High(CMagic) do
  begin
    Result[LIndex] := CMagic[LIndex];
  end;
  Result[6] := 1;
  Result[7] := 0;
  for LIndex := 0 to 3 do
  begin
    Result[8 + LIndex] := Byte((LongWord(LLength) shr (LIndex * 8)) and $FF);
  end;

  if LLength > 0 then
  begin
    Move(LPayload[0], Result[NyxLCLClipboardPacketHeaderBytes], LLength);
  end;
end;

function DecodeNyxLCLClipboardPacket(const ABytes: TNyxBytes): TNyxText;
const
  CMagic: array[0..5] of Byte = (78, 89, 88, 84, 88, 84);
var
  LLength: LongWord;
  LIndex: Integer;
  LPayload: TNyxBytes;
begin

  if (Length(ABytes) < NyxLCLClipboardPacketHeaderBytes) or
    (Length(ABytes) > NyxClipboardMaximumBytes + NyxLCLClipboardPacketHeaderBytes +
      NyxLCLClipboardMaximumPaddingBytes) then
  begin
    raise ENyxClipboard.Create('Clipboard packet is truncated or exceeds its byte budget');
  end;
  for LIndex := 0 to High(CMagic) do
  begin

    if ABytes[LIndex] <> CMagic[LIndex] then
    begin
      raise ENyxClipboard.Create('Clipboard packet has an unknown format');
    end;
  end;

  if (ABytes[6] <> 1) or (ABytes[7] <> 0) then
  begin
    raise ENyxClipboard.Create('Clipboard packet version or flags are unsupported');
  end;
  LLength := 0;
  for LIndex := 0 to 3 do
  begin
    LLength := LLength or (LongWord(ABytes[8 + LIndex]) shl (LIndex * 8));
  end;

  if (LLength > NyxClipboardMaximumBytes) or
    (LLength > LongWord(Length(ABytes) - NyxLCLClipboardPacketHeaderBytes)) or
    (LongWord(Length(ABytes) - NyxLCLClipboardPacketHeaderBytes) - LLength >
      NyxLCLClipboardMaximumPaddingBytes) then
  begin
    raise ENyxClipboard.Create('Clipboard packet payload length is invalid');
  end;
  LPayload := Copy(ABytes, NyxLCLClipboardPacketHeaderBytes, LLength);
  try
    Result := NyxDecodeUTF8(LPayload);
  except
    on ENyxBytes do
    begin
      raise ENyxClipboard.Create('Clipboard packet contains malformed UTF-8');
    end;
  end;
end;

function NyxLCLClipboardPlatformText(const AText: TNyxText): TNyxBytes;
var
  LInput: TNyxBytes;
  LIndex: Integer;
  LCount: Integer;
begin
  NyxClipboardTextByteCount(AText);

  if Pos(#0, AText) > 0 then
  begin
    raise ENyxClipboard.Create('Interoperable clipboard text cannot contain NUL');
  end;
  LInput := NyxEncodeUTF8(AText);
  Result := nil;
  SetLength(Result, Length(LInput) * 2 + 1);
  LIndex := 0;
  LCount := 0;
  while LIndex < Length(LInput) do
  begin

    if (LInput[LIndex] = 13) or (LInput[LIndex] = 10) then
    begin

      if (LInput[LIndex] = 13) and (LIndex + 1 < Length(LInput)) and
        (LInput[LIndex + 1] = 10) then
      begin
        Inc(LIndex);
      end;
      Result[LCount] := 13;
      Result[LCount + 1] := 10;
      Inc(LCount, 2);
    end
    else
    begin
      Result[LCount] := LInput[LIndex];
      Inc(LCount);
    end;
    Inc(LIndex);
  end;
  Result[LCount] := 0;
  SetLength(Result, LCount + 1);
end;

{$ifdef MSWINDOWS}
{ Bounded OS verification deliberately bypasses TClipboard.GetFormat/HasFormat:
  those may return the local staging cache even when SetClipboardData failed.
  The caller already owns the immediate OS transaction's lock. Both requested
  formats are checked under that ONE lock, so unrelated replacement
  cannot combine a prior exact packet with a later interoperable text value. }
function VerifyPublishedLocked(AFormat: TClipboardFormat; const AExact: TNyxBytes;
  const APlatform: TNyxBytes; AIncludePlain: Boolean): Boolean;
var
  LHandle: Windows.HGLOBAL;
  LSize: PtrUInt;
  LData: Pointer;
  LActual: TNyxBytes;
  LPlain: UnicodeString;
  LPlatformText: TNyxText;
  LExpectedBytes: PtrUInt;
begin
  Result := False;

  if Length(AExact) > 0 then
  begin
    LHandle := Windows.GetClipboardData(AFormat);

    if LHandle = 0 then
    begin
      Exit;
    end;
    LSize := Windows.GlobalSize(LHandle);

    if (LSize < PtrUInt(Length(AExact))) or
      (LSize > PtrUInt(Length(AExact) + NyxLCLClipboardMaximumPaddingBytes)) then
    begin
      Exit;
    end;
    LData := Windows.GlobalLock(LHandle);

    if LData = nil then
    begin
      Exit;
    end;
    try
      LActual := nil;
      SetLength(LActual, LSize);
      Move(LData^, LActual[0], LSize);
    finally
      Windows.GlobalUnlock(LHandle);
    end;
    { Validate the real framed allocation, including length/UTF-8, then compare
      the complete canonical header+payload. OS allocation padding is inert. }
    DecodeNyxLCLClipboardPacket(LActual);

    if not CompareMem(@LActual[0], @AExact[0], Length(AExact)) then
    begin
      Exit;
    end;
  end;

  if AIncludePlain then
  begin
    LPlatformText := NyxDecodeUTF8(APlatform);
    LPlain := UTF8Decode(LPlatformText);
    LExpectedBytes := Length(LPlain) * SizeOf(WideChar);
    LHandle := Windows.GetClipboardData(Windows.CF_UNICODETEXT);

    if LHandle = 0 then
    begin
      Exit;
    end;
    LSize := Windows.GlobalSize(LHandle);

    if (LSize < LExpectedBytes) or
      (LSize > LExpectedBytes + NyxLCLClipboardMaximumPaddingBytes) then
    begin
      Exit;
    end;
    LData := Windows.GlobalLock(LHandle);

    if LData = nil then
    begin
      Exit;
    end;
    try
      Result := CompareMem(LData, @LPlain[1], LExpectedBytes);
    finally
      Windows.GlobalUnlock(LHandle);
    end;
  end
  else
  begin
    { Embedded NUL is private-only; never advertise a text value which could
      truncate it. A stale text format would make this publication invalid. }
    Result := not Windows.IsClipboardFormatAvailable(Windows.CF_UNICODETEXT) and
      not Windows.IsClipboardFormatAvailable(Windows.CF_TEXT) and
      not Windows.IsClipboardFormatAvailable(Windows.CF_OEMTEXT);
  end;
end;

{ Win32 owns a movable allocation ONLY after successful SetClipboardData.
  Allocate/copy all bytes before EmptyClipboard; every failure frees only the
  handles still owned locally. Transferred handles must never be freed by Nyx. }
function AllocateClipboardBlock(AData: Pointer; ASize: PtrUInt): Windows.HGLOBAL;
var
  LData: Pointer;
begin
  Result := Windows.GlobalAlloc(Windows.GMEM_MOVEABLE or Windows.GMEM_ZEROINIT, ASize);

  if Result = 0 then
  begin
    Exit;
  end;
  LData := Windows.GlobalLock(Result);

  if LData = nil then
  begin
    Windows.GlobalFree(Result);
    Result := 0;
    Exit;
  end;
  try
    Move(AData^, LData^, ASize);
  finally
    Windows.GlobalUnlock(Result);
  end;
end;
{$endif}

function TLCLClipboardTarget.WriteText(const AExactPacket: TNyxBytes;
  const APlatformUTF8: TNyxBytes; AIncludePlain: Boolean;
  out AError: TNyxText): TNyxClipboardWriteOutcome;
{$ifdef MSWINDOWS}
var
  LFormat: TClipboardFormat;
  LExact: TNyxText;
  LPlain: UnicodeString;
  LWindow: Windows.HWND;
  LExactHandle: Windows.HGLOBAL;
  LPlainHandle: Windows.HGLOBAL;
{$endif}
begin
  Result := ncwoUnavailable;
  AError := 'This LCL widgetset has no verified clipboard text implementation';
  {$ifdef MSWINDOWS}

  if (WidgetSet = nil) or (WidgetSet.LCLPlatform <> lpWin32) then
  begin
    Exit;
  end;
  Result := ncwoFailed;
  AError := 'The native clipboard could not verify the complete write';

  if (Length(AExactPacket) = 0) and not AIncludePlain then
  begin
    Exit;
  end;

  if (Length(APlatformUTF8) > NyxClipboardMaximumBytes * 2 + 1) or
    (not AIncludePlain and (Length(APlatformUTF8) <> 0)) then
  begin
    Exit;
  end;

  if Length(AExactPacket) > 0 then
  begin
    LExact := DecodeNyxLCLClipboardPacket(AExactPacket);

    if not SameBytes(EncodeNyxLCLClipboardPacket(LExact), AExactPacket) then
    begin
      Exit;
    end;

    if AIncludePlain and not SameBytes(NyxLCLClipboardPlatformText(LExact),
      APlatformUTF8) then
    begin
      Exit;
    end;
  end;

  if AIncludePlain then
  begin

    if (Length(APlatformUTF8) = 0) or
      (APlatformUTF8[High(APlatformUTF8)] <> 0) then
    begin
      Exit;
    end;
    LExact := NyxDecodeUTF8(Copy(APlatformUTF8, 0, Length(APlatformUTF8) - 1));

    if Pos(#0, LExact) > 0 then
    begin
      Exit;
    end;
  end;
  LFormat := 0;

  if Length(AExactPacket) > 0 then
  begin
    LFormat := LCLIntf.ClipboardRegisterFormat(NyxLCLClipboardExactFormat);

    if LFormat = 0 then
    begin
      AError := 'The native clipboard exact format is unavailable';
      Result := ncwoUnavailable;
      Exit;
    end;
  end;
  { LCL marks Handle as platform-specific: this access is deliberately confined
    to the compile-time Windows AND runtime Win32 branch above. The application
    owns the hidden clipboard-owner window; no caller form is created/focused. }
  {$push}{$warn symbol_platform off}
  LWindow := Forms.Application.Handle;
  {$pop}

  if (LWindow = 0) or not Windows.IsWindow(LWindow) then
  begin
    AError := 'The native clipboard requires a live LCL application window';
    Result := ncwoUnavailable;
    Exit;
  end;
  LExactHandle := 0;
  LPlainHandle := 0;
  try

    if Length(AExactPacket) > 0 then
    begin
      LExactHandle := AllocateClipboardBlock(@AExactPacket[0], Length(AExactPacket));

      if LExactHandle = 0 then
      begin
        Exit;
      end;
    end;

    if AIncludePlain then
    begin
      LPlain := UTF8Decode(NyxDecodeUTF8(APlatformUTF8));
      LPlainHandle := AllocateClipboardBlock(@LPlain[1], Length(LPlain) * SizeOf(WideChar));

      if LPlainHandle = 0 then
      begin
        Exit;
      end;
    end;
    { Do not use TClipboard.Open/Clear: its private nesting counter can belong
      to unrelated caller staging, and its Win32 writer ignores individual
      SetClipboardData failures. All LCL objects are captured before this point;
      EmptyClipboard may notify the previous owner's actual window/callback. }

    if not Windows.OpenClipboard(LWindow) then
    begin
      AError := 'The native clipboard is currently unavailable for writing';
      Exit;
    end;
    try

      if not Windows.EmptyClipboard then
      begin
        Exit;
      end;

      if LExactHandle <> 0 then
      begin

        if Windows.SetClipboardData(LFormat, LExactHandle) = 0 then
        begin
          Exit;
        end;
        LExactHandle := 0;
      end;

      if LPlainHandle <> 0 then
      begin

        if Windows.SetClipboardData(Windows.CF_UNICODETEXT, LPlainHandle) = 0 then
        begin
          Exit;
        end;
        LPlainHandle := 0;
      end;

      if VerifyPublishedLocked(LFormat, AExactPacket, APlatformUTF8, AIncludePlain) then
      begin
        AError := '';
        Result := ncwoWritten;
      end;
    finally

      if not Windows.CloseClipboard then
      begin
        Result := ncwoFailed;
        AError := 'The native clipboard transaction could not close';
      end;
    end;
  finally

    if LExactHandle <> 0 then
    begin
      Windows.GlobalFree(LExactHandle);
    end;

    if LPlainHandle <> 0 then
    begin
      Windows.GlobalFree(LPlainHandle);
    end;
  end;
  {$endif}
end;

constructor TLCLClipboardRequest.Create(const ATarget: INyxLCLClipboardTextTarget;
  AMode: TNyxClipboardTextMode; const AText: TNyxText;
  const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply);
begin
  inherited Create(AText, AScheduler, AScope, AReply);
  FTarget := ATarget;
  FMode := AMode;
end;

procedure TLCLClipboardRequest.EndHost;
begin
  FTarget := nil;
end;

procedure TLCLClipboardRequest.Start;
var
  LLease: INyxClipboardWriteRequest;
  LTarget: INyxLCLClipboardTextTarget;
  LText: TNyxText;
  LError: TNyxText;
  LBytes: Integer;
  LExact: TNyxBytes;
  LPlatform: TNyxBytes;
  LIncludePlain: Boolean;
  LOutcome: TNyxClipboardWriteOutcome;
  LRepresentation: TNyxClipboardTextRepresentation;
begin
  LLease := Self;
  LTarget := FTarget;
  LText := FText;
  LBytes := FByteCount;
  LExact := nil;
  LPlatform := nil;
  LIncludePlain := Pos(#0, LText) = 0;
  LOutcome := ncwoUnavailable;
  LError := 'Interoperable clipboard text cannot contain NUL';
  LRepresentation := nctrNone;
  FScheduler.RequireUI;

  if not Pending then
  begin
    Cancel;
    Exit;
  end;

  if (FMode = nctmExact) or LIncludePlain then
  begin

    if FMode = nctmExact then
    begin
      LExact := EncodeNyxLCLClipboardPacket(LText);
      LRepresentation := nctrExactUTF8;

      if LIncludePlain then
      begin
        LRepresentation := nctrExactUTF8WithPlatformText;
      end;
    end
    else
    begin
      LRepresentation := nctrPlatformText;
    end;

    if LIncludePlain then
    begin
      LPlatform := NyxLCLClipboardPlatformText(LText);
    end;
    { This is the LAST cancellation getter before entering the external target.
      The local target/payload leases survive reentry and receiver disposal. }

    if not Pending then
    begin
      Cancel;
      Exit;
    end;
    try
      LOutcome := LTarget.WriteText(LExact, LPlatform, LIncludePlain, LError);
    except
      on Exception do
      begin
        LOutcome := ncwoFailed;
        LError := 'The native clipboard target failed during publication';
      end;
    end;
  end;

  if LOutcome <> ncwoWritten then
  begin
    LBytes := 0;
    LRepresentation := nctrNone;
  end;
  { A client exception belongs to this terminal delivery and must escape Begin;
    it is never caught/reclassified as a platform failure or delivered twice. }
  Complete(LOutcome, LBytes, LError, LRepresentation);
end;

constructor TLCLClipboardHost.Create(const ATarget: INyxLCLClipboardTextTarget;
  AMode: TNyxClipboardTextMode);
begin
  inherited Create;

  if (ATarget = nil) or (Ord(AMode) < Ord(Low(TNyxClipboardTextMode))) or
    (Ord(AMode) > Ord(High(TNyxClipboardTextMode))) then
  begin
    raise ENyxClipboard.Create('Native clipboard host requires a target and known text mode');
  end;
  FTarget := ATarget;
  FMode := AMode;
end;

function TLCLClipboardHost.BeginWriteText(const AText: TNyxText;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LHost: INyxClipboardWriteHost;
  LTarget: INyxLCLClipboardTextTarget;
  LMode: TNyxClipboardTextMode;
  LRequest: TLCLClipboardRequest;
begin
  LHost := Self;
  LTarget := FTarget;
  LMode := FMode;
  LRequest := TLCLClipboardRequest.Create(LTarget, LMode, AText, AScheduler,
    ACancellation, AReply);
  Result := LRequest;
  LRequest.Start;
end;

function NewNyxLCLClipboardWriteHost(AMode: TNyxClipboardTextMode): INyxClipboardWriteHost;
var
  LTarget: INyxLCLClipboardTextTarget;
begin
  LTarget := TLCLClipboardTarget.Create;
  Result := NewNyxLCLClipboardWriteHost(LTarget, AMode);
end;

function NewNyxLCLClipboardWriteHost(const ATarget: INyxLCLClipboardTextTarget;
  AMode: TNyxClipboardTextMode): INyxClipboardWriteHost;
begin
  Result := TLCLClipboardHost.Create(ATarget, AMode);
end;

end.
