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
unit nyx.pascal.witness;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.files, nyx.pascal.requirements,
  nyx.pascal.compilertrace;

type
  { Prepared, immutable compiler-only source witness. Files are separate exact
    byte copies with closed instrumentation; the original catalog is untouched.
    The host must compile this System root with -Mfpc -Us -s -Cn -vi -vn -vq,
    retain every source lease, and retire/join before publishing Read's result.
    No generated program, assembler or linker is executed. Read alone grants
    no process provenance. Earlier same-input receipts are remeasured in order;
    foreign/unsupported receipts or ambiguous physical occurrences refuse. }
  INyxPascalSourceWitness = interface(IInterface)
    ['{983AA21E-9BF2-483E-91CF-B20A1224E490}']
    function Files: INyxPascalFileCatalog;
    function Root: TNyxPascalPhysicalFile;
    function Read(const ATranscript: TNyxText; AExitCode: Integer;
      const AOptions: TNyxPascalCompilerTraceOptions;
      const ACancellation: INyxCancellationScope = nil): INyxFPCCompilerCapture;
  end;

{ Pure preflight/materialization. The exact compiler-root owner and supported
  UTF-8 physical site are required; unknown initial packing is never replaced
  by DEFAULT. Source directives, declarations and include order remain original.
  Its fresh notes are scoped by PUSH/POP; the final packet intentionally stops
  before the pending source condition. Every earlier receipt must agree again. }
function PlanNyxPascalSourceWitness(const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSourceWitness;
{ Revalidate a previously materialized compiler input, without executing a new
  compiler or accepting a supplied measurement. RetainedFiles must already be
  an owned captured byte catalog with its exact original path/search order.
  Its one canonical root packet identity is used to rebuild the closed witness
  for the CURRENT typed requirement, compiler owner and ordered earlier answers.
  Every file byte must agree before the existing Read becomes available. The
  retained transcript still needs its independent host/process provenance;
  restoration supplies neither a compiler receipt nor authority to reuse an old
  process-local source key. Callers rebuild requirements in their current world. }
function RestoreNyxPascalSourceWitness(const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture; const ARetainedFiles: INyxPascalFileCatalog;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSourceWitness;

implementation

uses SysUtils, nyx.bytes, nyx.pascal.context, nyx.pascal.encoding,
  nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.work;

const
  CKeyPrefix: TNyxText = 'nyx.storage.1|source-v1|';

type
  { Scalar coordinates, reference and prior measurement only. Interface leases
    live on the owning class, never inside records or static arrays. }
  TWitnessPoint = record
    FileIndex, Scalar, ByteOffset, Order: Integer;
    Name: TNyxPascalSizeReference;
    Expected: TNyxPascalStorageFact;
    Packet: TNyxBytes;
  end;
  { Owns a detached instrumented input and copied packet rows. Construction and
    transcript reads retain cancellation on the stack only; published metadata
    owns neither compiler, mutable producer, editor nor earlier analysis budget. }
  TSourceWitness = class(TInterfacedObject, INyxPascalSourceWitness)
  private
    FFiles: INyxPascalFileCatalog;
    FRoot: TNyxPascalPhysicalFile;
    FOwner: INyxFPCCompilerCapture;
    FKey: TNyxPascalStorageKey;
    FPrefix: TNyxText;
    FPoints: array of TWitnessPoint;
    function BaseName(AFile: Integer): TNyxText;
  public
    constructor Create(const ARequirement: INyxPascalCompilerRequirement;
      const AOwner: INyxFPCCompilerCapture; const ACancellation: INyxCancellationScope;
      const ARestoredPrefix: TNyxText = '');
    function Files: INyxPascalFileCatalog;
    function Root: TNyxPascalPhysicalFile;
    function Read(const ATranscript: TNyxText; AExitCode: Integer;
      const AOptions: TNyxPascalCompilerTraceOptions;
      const ACancellation: INyxCancellationScope): INyxFPCCompilerCapture;
  end;

procedure Check(const AScope: INyxCancellationScope);
begin

  if (AScope <> nil) and AScope.Cancelled then
    raise ENyxPascalAnalysisCancelled.Create('Source witness cancelled');
end;

{ Keys are bounded printable metadata, not executable expressions. Decode their
  length-framed UTF-8 hex, and refuse noncanonical framing/hex/Unicode. }
function KeyParts(const AKey: TNyxPascalStorageKey): TNyxStrings;
var
  LText, LCountText, LHex: TNyxText;
  LAt, LStart, LLength, LByte, LHigh, LLow: Integer;
  LBytes: TNyxBytes;
begin
  AKey.Validate;
  LText := AKey.Value;

  if Copy(LText, 1, Length(CKeyPrefix)) <> CKeyPrefix then
    raise ENyxPascalContext.Create('A source witness cannot revalidate another evidence family');
  Result := TNyxStrings.Create;
  try
    LAt := Length(CKeyPrefix) + 1;
    while LAt <= Length(LText) do
    begin
      LStart := LAt;
      while (LAt <= Length(LText)) and (LText[LAt] in ['0'..'9']) do Inc(LAt);
      LCountText := Copy(LText, LStart, LAt - LStart);

      if (LAt > Length(LText)) or (LText[LAt] <> ':') or
        not TryStrToInt(String(LCountText), LLength) or (LLength < 0) or
        (TNyxText(IntToStr(LLength)) <> LCountText) or (LLength mod 2 <> 0) or
        (LLength > Length(LText) - LAt - 1) then
        raise ENyxPascalContext.Create('A source witness key has malformed framing');
      Inc(LAt);
      LHex := Copy(LText, LAt, LLength);
      Inc(LAt, LLength);

      if (LAt > Length(LText)) or (LText[LAt] <> '|') then
        raise ENyxPascalContext.Create('A source witness key has no field delimiter');
      Inc(LAt);
      SetLength(LBytes, LLength div 2);
      for LByte := 0 to High(LBytes) do
      begin
        LHigh := Pos(LHex[LByte * 2 + 1], '0123456789ABCDEF') - 1;
        LLow := Pos(LHex[LByte * 2 + 2], '0123456789ABCDEF') - 1;

        if (LHigh < 0) or (LLow < 0) then
          raise ENyxPascalContext.Create('A source witness key has noncanonical hex');
        LBytes[LByte] := LHigh * 16 + LLow;
      end;
      Result.Add(NyxDecodeUTF8(LBytes));

      if Result.Count > 96 + 6 * 2048 then
        raise ENyxPascalContext.Create('A source witness key exceeds its captured symbol bound');
    end;

    if Result.Count < 8 then
      raise ENyxPascalContext.Create('A source witness key is incomplete');
  except
    Result.Free;
    raise;
  end;
end;

function TSourceWitness.BaseName(AFile: Integer): TNyxText;
var LAt: Integer;
begin
  Result := FFiles.FileAt(AFile).Name;
  LAt := Length(Result);
  while (LAt > 0) and (Result[LAt] <> '/') do Dec(LAt);
  Result := Copy(Result, LAt + 1, Length(Result));
end;

constructor TSourceWitness.Create(const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture; const ACancellation: INyxCancellationScope;
  const ARestoredPrefix: TNyxText);
var
  LRequest: INyxPascalCompilerRequirement;
  LSource: INyxPascalSourceSizeRequirement;
  LValues: INyxPascalContextStorage;
  LCurrent, LParts: TNyxStrings;
  LOriginal: INyxPascalFileCatalog;
  LByteFiles: INyxPascalByteFileCatalog;
  LView: INyxPascalSourceView;
  LBytes, LInserted: TNyxBytes;
  LContext: INyxPascalCompilerContext;
  LGuid: TGUID;
  LPoint: TWitnessPoint;
  LAt, LOther, LRow, LBit, LFile, LToken, LVisits, LOffset, LContextAt: Integer;
  LPacket, LGuards, LName, LExpected: TNyxText;
  LSymbol: TNyxPascalDefineRef;
  LValue: TNyxPascalDefineValue;
  LOption: Char;

  procedure AddPoint(AParts: TNyxStrings; const AExpected: TNyxPascalStorageFact;
    ACurrent: Boolean);
  var LAt, LCount, LOccurrence: Integer;
  begin
    Check(ACancellation);

    if AParts.Count <> LCurrent.Count then
      raise ENyxPascalContext.Create('A previous source receipt has different compiler inputs');
    for LAt := 0 to AParts.Count - 1 do
    begin

      if not (LAt in [2, 3, 4]) and (AParts[LAt] <> LCurrent[LAt]) then
        raise ENyxPascalContext.Create('A previous source receipt has foreign input ownership');
    end;
    LPoint := Default(TWitnessPoint);
    LPoint.FileIndex := LOriginal.Find(NyxPascalPhysicalFile(AParts[2]));

    if (LPoint.FileIndex < 0) or not TryStrToInt(String(AParts[3]), LPoint.Scalar) or
      (LPoint.Scalar < 0) or (TNyxText(IntToStr(LPoint.Scalar)) <> AParts[3]) then
      raise ENyxPascalContext.Create('A source receipt has no exact captured coordinate');
    LPoint.Name := NyxPascalSizeReference(AParts[4]);
    LPoint.Expected := AExpected;
    LView := NyxPascalSourceViewAt(LOriginal, LPoint.FileIndex);

    if (LView = nil) or (LView.Kind <> psekUTF8) or (LPoint.Scalar >= LView.Scalars) then
      raise ENyxPascalContext.Create('Native source witnesses require an exact UTF-8 physical site');
    LPoint.ByteOffset := LView.ByteOffset(LPoint.Scalar);
    LPoint.Order := -1;
    LCount := 0;
    for LAt := 0 to LRequest.Source.Source.OccurrenceCount - 1 do
    begin

      if LRequest.Source.Source.Occurrence(LAt).FileIndex = LPoint.FileIndex then Inc(LCount);
    end;

    if LCount <> 1 then
      raise ENyxPascalContext.Create('Repeated source-witness files require occurrence-specific materialization');

    if (AParts[2] = LCurrent[2]) and (AParts[3] = LCurrent[3]) then
      LPoint.Order := LRequest.Source.Source.Count
    else
    begin
      for LAt := 0 to LRequest.Source.Source.Count - 1 do
      begin
        Check(ACancellation);

        if LRequest.Source.Source.Token(LAt).Expanded then Continue;
        LOccurrence := LRequest.Source.Source.Token(LAt).Occurrence;

        if (LRequest.Source.Source.Occurrence(LOccurrence).FileIndex = LPoint.FileIndex) and
          (LRequest.Source.Source.PhysicalToken(LAt).Span.Start = LPoint.Scalar) and
          (LRequest.Source.Source.Token(LAt).Activity = ppkTrue) then
        begin
          LPoint.Order := LAt;
          Break;
        end;
      end;
    end;

    if LPoint.Order < 0 then
      raise ENyxPascalContext.Create('An earlier source receipt has no active physical prefix token');
    LAt := Length(FPoints);
    SetLength(FPoints, LAt + 1);
    FPoints[LAt] := LPoint;

    if ACurrent and (LPoint.Order <> LRequest.Source.Source.Count) then
      raise ENyxPascalContext.Create('The pending source request is not the exact prefix boundary');
  end;

  procedure Splice(var ABytes: TNyxBytes; AOffset: Integer; const AInsert: TNyxBytes);
  var LBefore: Integer;
    {$ifdef PAS2JS}LByte: Integer;{$endif}
  begin
    Check(ACancellation);
    LBefore := Length(ABytes);

    if (AOffset < 0) or (AOffset > LBefore) or
      (Length(AInsert) > 4 * 1024 * 1024 - LBefore) then
      raise ENyxPascalContext.Create('Source instrumentation exceeds its byte/coordinate limit');
    SetLength(ABytes, LBefore + Length(AInsert));

    {$ifdef PAS2JS}
    for LByte := LBefore - 1 downto AOffset do ABytes[LByte + Length(AInsert)] := ABytes[LByte];
    for LByte := 0 to High(AInsert) do ABytes[AOffset + LByte] := AInsert[LByte];
    {$else}

      if LBefore > AOffset then
        Move(ABytes[AOffset], ABytes[AOffset + Length(AInsert)], LBefore - AOffset);

      if Length(AInsert) > 0 then Move(AInsert[0], ABytes[AOffset], Length(AInsert));
    {$endif}
  end;

begin
  inherited Create;
  Check(ACancellation);
  LRequest := CaptureNyxPascalRequirement(ARequirement);
  FOwner := FreezeNyxFPCCompilerCapture(AOwner);

  if (LRequest.Kind <> pcrSourceSize) or
    not Supports(LRequest, INyxPascalSourceSizeRequirement, LSource) or
    not FOwner.Context.SameFacts(LRequest.Context) then
    raise ENyxPascalContext.Create('A source witness needs its exact source request and capture owner');
  FKey := LSource.StorageKey;

  if NyxPascalCapturedStorage(FOwner.Context, FKey).State <> psfsUnknown then
    raise ENyxPascalContext.Create('A source witness cannot replace an existing answer');
  LOriginal := LRequest.Source.Source.Files;
  FFiles := LOriginal;
  FRoot := LOriginal.FileAt(LRequest.Source.Source.Occurrence(0).FileIndex);
  LCurrent := KeyParts(FKey);
  try
    Supports(FOwner.Context, INyxPascalContextStorage, LValues);

    if LValues <> nil then
    begin

      if LValues.StorageCount >= 64 then
        raise ENyxPascalContext.Create('A source witness cannot exceed the existing structural receipt bound');
      for LAt := 0 to LValues.StorageCount - 1 do
      begin
        LParts := KeyParts(LValues.StorageKey(LAt));
        try
          AddPoint(LParts, LValues.StorageValueAt(LAt), False);

          if LValues.StorageValueAt(LAt).State <> psfsKnown then
            raise ENyxPascalContext.Create('Earlier source measurements must be complete known values');
        finally LParts.Free; end;
      end;
    end;
    AddPoint(LCurrent, NyxPascalStorageFact(psfsUnknown), True);
    { Stable source order; the new request remains last among multiple sizes
      in one conditional. Remeasurements never consume extra compiler jobs. }
    for LAt := 1 to High(FPoints) do
    begin
      LPoint := FPoints[LAt];
      LOther := LAt;
      while (LOther > 0) and (FPoints[LOther - 1].Order > LPoint.Order) do
      begin
        FPoints[LOther] := FPoints[LOther - 1];
        Dec(LOther);
      end;
      FPoints[LOther] := LPoint;
    end;

    FPrefix := ARestoredPrefix;

    if FPrefix = '' then
    begin

      if CreateGUID(LGuid) <> 0 then
        raise ENyxPascalContext.Create('A source witness needs an independent packet identity');
      FPrefix := 'NYX_SOURCE_';
      LName := TNyxText(GUIDToString(LGuid));
      for LAt := 1 to Length(LName) do
      begin

        if LName[LAt] in ['0'..'9', 'A'..'F', 'a'..'f'] then FPrefix := FPrefix + LName[LAt];
      end;
      FPrefix := FPrefix + '_';
    end;
    LGuards := '{$codepage utf8}'#10;
    for LContextAt := 0 to 1 do
    begin
    LContext := FOwner.Context;

    if LContextAt = 1 then LContext := LRequest.Source.Source.InitialContext;
    for LAt := 0 to LContext.SymbolCount - 1 do
    begin
      Check(ACancellation);
      LSymbol := LContext.SymbolName(LAt);
      LValue := LContext.Symbol(LSymbol);

      if LValue.Kind = ppdUnknown then Continue;
      LName := LSymbol.Name;

      if LValue.Kind = ppdUndefined then LGuards := LGuards + '{$ifdef ' + LName + '}'
      else LGuards := LGuards + '{$ifndef ' + LName + '}';
      LGuards := LGuards + '{$fatal ' + FPrefix + 'GUARD}{$endif}'#10;

      if LValue.Kind = ppdInteger then
        LGuards := LGuards + '{$if ' + LName + '<>' + TNyxText(IntToStr(LValue.IntegerValue)) +
          '}{$fatal ' + FPrefix + 'GUARD}{$endif}'#10;
    end;
    for LOption := 'A' to 'Z' do
    begin

      if LContext.Option(LOption) = ppkUnknown then Continue;
      LExpected := '+';

      if LContext.Option(LOption) = ppkTrue then LExpected := '-';
      LGuards := LGuards + '{$ifopt ' + TNyxText(LOption) + LExpected +
        '}{$fatal ' + FPrefix + 'GUARD}{$endif}'#10;
    end;
    end;
    LGuards := LGuards + '{$push}{$notes on}{$note ' + FPrefix + 'READY}{$pop}'#10;
    for LRow := 0 to High(FPoints) do
    begin
      Check(ACancellation);
      LName := FPoints[LRow].Name.Name;
      LPacket := #10'{$push}{$notes on}'#10'{$note ' + FPrefix + TNyxText(IntToStr(LRow)) + '_BEGIN}'#10;
      for LBit := 0 to 30 do
      begin
        LExpected := FPrefix + TNyxText(IntToStr(LRow)) + '_BIT_' + TNyxText(IntToStr(LBit)) + '_';
        LPacket := LPacket + '{$if ((sizeof(' + LName + ') div ' +
          TNyxText(IntToStr(1 shl LBit)) + ') mod 2)=0}{$note ' + LExpected +
          '0}{$else}{$note ' + LExpected + '1}{$endif}'#10;
      end;
      LPacket := LPacket + '{$if (sizeof(' + LName + ')<0) or (sizeof(' + LName +
        ')>2147483647)}{$fatal ' + FPrefix + 'RANGE}{$endif}'#10 +
        '{$note ' + FPrefix + TNyxText(IntToStr(LRow)) + '_END}'#10;

      if LRow = High(FPoints) then LPacket := LPacket + '{$fatal ' + FPrefix + 'STOP}'#10
      else LPacket := LPacket + '{$pop}'#10;
      FPoints[LRow].Packet := NyxEncodeUTF8(LPacket);
      { FPC may print only a basename. Refuse ambiguous packet owners rather
        than accepting a same-spelled diagnostic from another captured file. }
      LName := BaseName(FPoints[LRow].FileIndex);
      LVisits := 0;
      for LFile := 0 to LOriginal.Count - 1 do
      begin

        if NyxPascalASCIIKey(BaseName(LFile)) = NyxPascalASCIIKey(LName) then Inc(LVisits);
      end;

      if LVisits <> 1 then
        raise ENyxPascalContext.Create('Source witness diagnostic basenames must be unambiguous');
    end;
    for LFile := 0 to LOriginal.Count - 1 do
    begin
      Check(ACancellation);
      LInserted := nil;
      LBytes := nil;
      { Descending physical coordinates preserve every original byte. Same-site
        packets are inserted in reverse row order, retaining source reply order. }
      LOffset := High(Integer);
      repeat
        LToken := -1;
        for LRow := 0 to High(FPoints) do
        begin

          if (FPoints[LRow].FileIndex = LFile) and (FPoints[LRow].ByteOffset < LOffset) and
            ((LToken < 0) or (FPoints[LRow].ByteOffset > FPoints[LToken].ByteOffset)) then LToken := LRow;
        end;

        if LToken < 0 then Break;
        LOffset := FPoints[LToken].ByteOffset;

        if LBytes = nil then
          LBytes := NyxPascalSourceBytesAt(LOriginal, LFile).Slice(
            NyxPascalByteSpan(0, NyxPascalSourceBytesAt(LOriginal, LFile).Count));
        for LRow := High(FPoints) downto 0 do
        begin

          if (FPoints[LRow].FileIndex = LFile) and (FPoints[LRow].ByteOffset = LOffset) then
            Splice(LBytes, LOffset, FPoints[LRow].Packet);
        end;
      until False;

      if LOriginal.FileAt(LFile).Name = FRoot.Name then
      begin
        LView := NyxPascalSourceViewAt(LOriginal, LFile);

        if (LView = nil) or (LView.Kind <> psekUTF8) then
          raise ENyxPascalContext.Create('Native System witnesses need the exact UTF-8 root view');

        if LBytes = nil then LBytes := LView.Original.Slice(NyxPascalByteSpan(0, LView.Original.Count));
        LOffset := 0;

        if (Length(LBytes) >= 3) and (LBytes[0] = $EF) and
          (LBytes[1] = $BB) and (LBytes[2] = $BF) then LOffset := 3;
        LInserted := NyxEncodeUTF8(LGuards);
        Splice(LBytes, LOffset, LInserted);
      end;

      if LBytes <> nil then
      begin

        if not Supports(FFiles, INyxPascalByteFileCatalog, LByteFiles) then
          raise ENyxPascalContext.Create('Source-witness files lost their byte owner');
        FFiles := LByteFiles.WithByteSource(LOriginal.FileAt(LFile), NewNyxPascalSourceBytes(LBytes));
      end;
    end;
  finally
    LCurrent.Free;
  end;
end;

function TSourceWitness.Files: INyxPascalFileCatalog;
begin Result := FFiles; end;
function TSourceWitness.Root: TNyxPascalPhysicalFile;
begin Result := FRoot; end;

function TSourceWitness.Read(const ATranscript: TNyxText; AExitCode: Integer;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope): INyxFPCCompilerCapture;
var
  LAt, LStart, LLines, LRow, LBit, LBytes, LMarkerAt, LMessageAt, LFile: Integer;
  LLine, LBody, LExpected, LOwner: TNyxText;
  LReady, LStopped, LAborted, LEnded, LDriver: Boolean;

  procedure Fail;
  begin
    raise ENyxPascalContext.Create('The source-size witness transcript is incomplete, foreign or inconsistent');
  end;

  procedure RequireOwner(AFile: Integer; const AMessage: TNyxText);
  var LComma, LClose, LLineNo, LColumn: Integer;
  begin
    LOwner := BaseName(AFile);

    if (Copy(LLine, 1, Length(LOwner) + 1) <> LOwner + '(') or
      (LMessageAt <= Length(LOwner) + 1) or
      (Copy(LLine, LMessageAt, Length(AMessage)) <> AMessage) or
      (LMarkerAt <> LMessageAt + Length(AMessage)) then Fail;
    LComma := Pos(',', LLine);
    LClose := Pos(') ', LLine);

    if (LComma <= Length(LOwner) + 1) or (LClose <= LComma) or
      not TryStrToInt(String(Copy(LLine, Length(LOwner) + 2, LComma - Length(LOwner) - 2)), LLineNo) or
      not TryStrToInt(String(Copy(LLine, LComma + 1, LClose - LComma - 1)), LColumn) or
      (LLineNo < 1) or (LColumn < 1) or (LClose + 2 <> LMessageAt) then Fail;
  end;

begin
  AOptions.Validate;
  Check(ACancellation);

  if (AExitCode <> 1) or (NyxUTF8ByteCount(ATranscript) > AOptions.MaximumBytes) then Fail;
  LReady := False; LStopped := False; LAborted := False; LEnded := False; LDriver := False;
  LLines := 0; LRow := 0; LBit := -1; LBytes := 0;
  LAt := 1;
  while LAt <= Length(ATranscript) do
  begin
    Check(ACancellation);
    Inc(LLines);

    if LLines > AOptions.MaximumLines then Fail;
    LStart := LAt;
    while (LAt <= Length(ATranscript)) and not (ATranscript[LAt] in [#10, #13]) do Inc(LAt);
    LLine := Copy(ATranscript, LStart, LAt - LStart);

    if NyxUTF8ByteCount(LLine) > AOptions.MaximumLineLength then Fail;

    if (LAt <= Length(ATranscript)) and (ATranscript[LAt] = #13) then Inc(LAt);

    if (LAt <= Length(ATranscript)) and (ATranscript[LAt] = #10) then Inc(LAt);
    LMarkerAt := Pos(FPrefix, LLine);

    if LMarkerAt > 0 then
    begin
      LMessageAt := Pos('Note: (2025) User defined: ', LLine);
      LBody := Copy(LLine, LMarkerAt + Length(FPrefix), Length(LLine));

      if not LReady then
      begin
        RequireOwner(FFiles.Find(FRoot), 'Note: (2025) User defined: ');

        if LBody <> 'READY' then Fail;
        LReady := True;
        Continue;
      end;

      if LStopped or LAborted then Fail;

      if LBody = 'STOP' then
      begin
        LMessageAt := Pos('Fatal: (2022) User defined: ', LLine);
        RequireOwner(FPoints[High(FPoints)].FileIndex, 'Fatal: (2022) User defined: ');

        if not LEnded or (LRow <> Length(FPoints)) then Fail;
        LStopped := True;
        Continue;
      end;

      if LRow >= Length(FPoints) then Fail;
      LFile := FPoints[LRow].FileIndex;
      RequireOwner(LFile, 'Note: (2025) User defined: ');
      LExpected := TNyxText(IntToStr(LRow));

      if LBit = -1 then
      begin

        if LBody <> LExpected + '_BEGIN' then Fail;
        LBit := 0;
        LBytes := 0;
        Continue;
      end;

      if LBit < 31 then
      begin
        LExpected := LExpected + '_BIT_' + TNyxText(IntToStr(LBit)) + '_';

        if (LBody <> LExpected + '0') and (LBody <> LExpected + '1') then Fail;

        if LBody = LExpected + '1' then Inc(LBytes, 1 shl LBit);
        Inc(LBit);
        Continue;
      end;

      if LBody <> LExpected + '_END' then Fail;

      if (FPoints[LRow].Expected.State = psfsKnown) and
        (FPoints[LRow].Expected.Bytes <> LBytes) then Fail;
      Inc(LRow);
      LBit := -1;
      LEnded := LRow = Length(FPoints);
      Continue;
    end;

    if LLine = 'Fatal: (1018) Compilation aborted' then
    begin

      if not LStopped or LAborted then Fail;
      LAborted := True;
    end
    else if (Pos('Error:', LLine) > 0) or (Pos('Fatal:', LLine) > 0) then
    begin
      LExpected := ' returned an error exitcode';

      if not LAborted or LDriver or (Copy(LLine, 1, 7) <> 'Error: ') or
        (Copy(LLine, Length(LLine) - Length(LExpected) + 1, Length(LExpected)) <> LExpected) then Fail;
      LDriver := True;
    end;
  end;

  if not LReady or not LEnded or not LStopped or not LAborted then Fail;
  Check(ACancellation);
  Result := WithNyxFPCStorageReply(FOwner, FKey, NyxPascalStorageFact(psfsKnown, LBytes));
end;

function PlanNyxPascalSourceWitness(const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture;
  const ACancellation: INyxCancellationScope): INyxPascalSourceWitness;
begin
  Result := TSourceWitness.Create(ARequirement, AOwner, ACancellation);
end;

function RestoreNyxPascalSourceWitness(const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture; const ARetainedFiles: INyxPascalFileCatalog;
  const ACancellation: INyxCancellationScope): INyxPascalSourceWitness;
const
  CLead: TNyxText = '{$push}{$notes on}{$note NYX_SOURCE_';
  CTail: TNyxText = 'READY}{$pop}';
  CPrefix: TNyxText = 'NYX_SOURCE_';
var
  LScope: INyxCancellationScope;
  LRequest: INyxPascalCompilerRequirement;
  LOwner: INyxFPCCompilerCapture;
  LRetained, LExpected: INyxPascalFileCatalog;
  LRoot: TNyxPascalPhysicalFile;
  LBytes, LLeft, LRight: INyxPascalSourceBytes;
  LWitness: INyxPascalSourceWitness;
  LPrefix: TNyxText;
  LAt, LOther, LStart, LFound, LFile: Integer;

  function ReadByte(const ABytes: INyxPascalSourceBytes; AIndex: Integer): Byte;
  begin
    Check(LScope);
    NyxPascalAdmitAnalysis(LScope, papSourceProof);
    Check(LScope);
    Result := ABytes.ByteAt(AIndex);
  end;

  function Matches(AStart: Integer; const AText: TNyxText): Boolean;
  var
    LIndex: Integer;
  begin
    Result := False;

    if (AStart < 0) or (Length(AText) > LBytes.Count - AStart) then
      Exit;
    for LIndex := 1 to Length(AText) do
    begin

      if ReadByte(LBytes, AStart + LIndex - 1) <> Ord(AText[LIndex]) then
        Exit;
    end;
    Result := True;
  end;

  procedure Refuse;
  begin
    raise ENyxPascalContext.Create('Retained source witness does not match its current exact input');
  end;

begin
  Result := nil;
  { Lease all caller values before the first cancellation/admission callback.
    A caller may retire its separate slots without revoking these passed values. }
  LScope := ACancellation;
  LRetained := ARetainedFiles;
  LRequest := ARequirement;
  LOwner := AOwner;
  Check(LScope);

  if (LRetained = nil) or (LRequest = nil) or (LOwner = nil) then
    Refuse;
  { Mutable alternate suppliers must be captured by their caller first. This
    synchronous restoration cannot certify a producer's changing file universe. }
  if NyxPascalFileCatalogIdentity(LRetained) = '' then
    Refuse;
  LRequest := CaptureNyxPascalRequirement(LRequest);
  LOwner := FreezeNyxFPCCompilerCapture(LOwner);
  Check(LScope);

  if (LRequest.Kind <> pcrSourceSize) or (LRequest.Source = nil) or
    (LRequest.Source.Source.OccurrenceCount = 0) then
    Refuse;
  LRoot := LRequest.Source.Source.Files.FileAt(
    LRequest.Source.Source.Occurrence(0).FileIndex);
  LFile := LRetained.Find(LRoot);

  if LFile < 0 then
    Refuse;
  LBytes := NyxPascalSourceBytesAt(LRetained, LFile);
  LFound := 0;
  LPrefix := '';
  for LAt := 0 to LBytes.Count - 1 do
  begin

    if not Matches(LAt, CLead) then
      Continue;
    Inc(LFound);

    if LFound <> 1 then
      Refuse;
    LStart := LAt + Length(CLead);

    if (LStart > LBytes.Count - 33) or
      not Matches(LStart + 33, CTail) then
      Refuse;
    LPrefix := CPrefix;
    for LOther := 0 to 31 do
    begin
      LFile := ReadByte(LBytes, LStart + LOther);

      if not (LFile in [Ord('0')..Ord('9'), Ord('A')..Ord('F')]) then
        Refuse;
      LPrefix := LPrefix + TNyxText(Chr(LFile));
    end;

    if ReadByte(LBytes, LStart + 32) <> Ord('_') then
      Refuse;
    LPrefix := LPrefix + '_';
  end;

  if LFound <> 1 then
    Refuse;
  Check(LScope);
  { The retained marker carries no facts: the existing owner rebuilds all
    guards, current/prior measurement sites and closed note packets afresh. }
  LWitness := TSourceWitness.Create(LRequest, LOwner, LScope, LPrefix);
  LExpected := LWitness.Files;

  if (LRetained.CasePolicy <> LExpected.CasePolicy) or
    (LRetained.Count <> LExpected.Count) or
    (LRetained.DirectoryCount <> LExpected.DirectoryCount) then
    Refuse;
  for LAt := 0 to LExpected.DirectoryCount - 1 do
  begin
    Check(LScope);
    NyxPascalAdmitAnalysis(LScope, papSourceProof);

    if LRetained.DirectoryAt(LAt) <> LExpected.DirectoryAt(LAt) then
      Refuse;
  end;
  for LAt := 0 to LExpected.Count - 1 do
  begin
    Check(LScope);
    NyxPascalAdmitAnalysis(LScope, papSourceProof);

    if LRetained.FileAt(LAt).Name <> LExpected.FileAt(LAt).Name then
      Refuse;
    LLeft := NyxPascalSourceBytesAt(LExpected, LAt);
    LRight := NyxPascalSourceBytesAt(LRetained, LAt);

    if (LLeft.Count <> LRight.Count) or (LLeft.Origin <> LRight.Origin) then
      Refuse;
    for LOther := 0 to LLeft.Count - 1 do
    begin

      if ReadByte(LLeft, LOther) <> ReadByte(LRight, LOther) then
        Refuse;
    end;
  end;
  Check(LScope);
  { Publish only the reconstructed owner. Retained input decoding cannot donate
    source authority; compiler input is exact bytes, while its source views come
    from the current original requirement and the ordinary witness constructor. }
  Result := LWitness;
end;

end.
