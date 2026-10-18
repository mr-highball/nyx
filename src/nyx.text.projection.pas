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

unit nyx.text.projection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.editing;

type
  { Invalid Unicode, non-equivalent content and ambiguous/out-of-range scalar
    boundaries refuse without changing either text or any caller-owned state. }
  ENyxTextProjection = class(EArgumentException);
  { Presentation affects inserted line endings only; authored bytes stay exact.
    Preserve is the default. DOM textareas require LF; native raw restoration
    can preserve mixed physical endings without an implicit global conversion. }
  TNyxTextLineEndings = (ntePreserve, nteLF, nteCRLF, nteCR);
  { A boundary inside a CRLF pair has no exact image in a one-scalar newline.
    Exact refuses it; explicit Before/After choose the complete newline edge. }
  TNyxTextProjectionAffinity = (npaExact, npaBefore, npaAfter);
  { Copied insertion owns authored and displayed bytes independently. Fluent
    methods return detached values and admit only newline-equivalent content. }
  TNyxTextInsertion = record
  private
    FDefined: Boolean;
    FAuthored: TNyxText;
    FPresented: TNyxText;
  public
    function PresentedAs(const AText: TNyxText): TNyxTextInsertion;
    function UsingLineEndings(APolicy: TNyxTextLineEndings): TNyxTextInsertion;
    procedure Validate;
    property Authored: TNyxText read FAuthored;
    property Presented: TNyxText read FPresented;
  end;
  { Immutable exact authored/displayed text and sparse newline-width map. Unicode
    scalars, NUL and all non-newline content remain exact; no normalization of
    identifiers, case, combining marks or graphemes is performed. No editor,
    document, platform or mutable backing storage is borrowed. }
  INyxTextProjection = interface(IInterface)
    ['{353CB503-E1E5-42F3-B0EF-9507C6DE5313}']
    function Authored: TNyxText;
    function Presented: TNyxText;
    function AuthoredScalars: Integer;
    function PresentedScalars: Integer;
    { Map scalar boundaries. Default exact mapping refuses CRLF interiors that
      have no counterpart; affinities never silently change a caller's intent. }
    function ToAuthored(AOffset: Integer;
      AAffinity: TNyxTextProjectionAffinity = npaExact): Integer;
    function ToPresented(AOffset: Integer;
      AAffinity: TNyxTextProjectionAffinity = npaExact): Integer;
    function AuthoredSelection(const ASelection: TNyxTextSelection): TNyxTextSelection;
    function PresentedSelection(const ASelection: TNyxTextSelection): TNyxTextSelection;
    function AuthoredSlice(AStart, ACount: Integer): TNyxText;
    function PresentedSlice(AStart, ACount: Integer): TNyxText;
    { Exact physical expected range, explicit authored/displayed insertion.
      Untouched authored prefixes/suffixes retain their original bytes. }
    function ReplacePresented(AStart: Integer; const AExpected: TNyxText;
      const AInsertion: TNyxTextInsertion): INyxTextProjection;
    { Observe one contiguous physical delta, using longest common scalar prefix
      and suffix. Ambiguous repeated text is attributed after the common prefix.
      CRLF edges expand together. Explicit ranges are preferable when known;
      this is not a semantic diff or arbitrary multi-edit provenance. }
    function ObservePresented(const AText: TNyxText): INyxTextProjection;
  end;

{ Validate both complete texts and publish a detached immutable map. Only CR,
  LF and CRLF spelling may differ; all other content must match scalar-for-scalar. }
function NewNyxTextProjection(const AAuthored, APresented: TNyxText): INyxTextProjection;
function NyxTextInsertion(const AText: TNyxText): TNyxTextInsertion;
{ Pure presentation formatter; default Preserve retains every exact byte/unit. }
function NyxPresentText(const AText: TNyxText; APolicy: TNyxTextLineEndings): TNyxText;

implementation

type
  { One width discrepancy. Starts count scalars before the logical newline;
    widths are one or two. Arrays contain no managed/interface fields. }
  TProjectionBreak = record
    AuthoredStart: Integer;
    PresentedStart: Integer;
    AuthoredWidth: Integer;
    PresentedWidth: Integer;
  end;
  { Owns copied strings and an ordered sparse discrepancy vector. Binary search
    maps boundaries; identical-width runs require no per-character allocation. }
  TTextProjection = class(TInterfacedObject, INyxTextProjection)
  private
    FAuthored: TNyxText;
    FPresented: TNyxText;
    FAuthoredCount: Integer;
    FPresentedCount: Integer;
    FBreaks: array of TProjectionBreak;
    function Map(AOffset: Integer; AReverse: Boolean;
      AAffinity: TNyxTextProjectionAffinity): Integer;
  public
    constructor Create(const AAuthored, APresented: TNyxText);
    function Authored: TNyxText;
    function Presented: TNyxText;
    function AuthoredScalars: Integer;
    function PresentedScalars: Integer;
    function ToAuthored(AOffset: Integer; AAffinity: TNyxTextProjectionAffinity = npaExact): Integer;
    function ToPresented(AOffset: Integer; AAffinity: TNyxTextProjectionAffinity = npaExact): Integer;
    function AuthoredSelection(const ASelection: TNyxTextSelection): TNyxTextSelection;
    function PresentedSelection(const ASelection: TNyxTextSelection): TNyxTextSelection;
    function AuthoredSlice(AStart, ACount: Integer): TNyxText;
    function PresentedSlice(AStart, ACount: Integer): TNyxText;
    function ReplacePresented(AStart: Integer; const AExpected: TNyxText;
      const AInsertion: TNyxTextInsertion): INyxTextProjection;
    function ObservePresented(const AText: TNyxText): INyxTextProjection;
  end;

function StorageAt(const AText: TNyxText; AOffset: Integer): Integer;
var
  LAt: Integer;
  LScalar: Integer;
begin

  if AOffset < 0 then
  begin
    raise ENyxTextProjection.Create('Projection offset cannot be negative');
  end;
  Result := 1;
  for LAt := 1 to AOffset do
  begin

    if not NyxNextScalar(AText, Result, LScalar) then
    begin
      raise ENyxTextProjection.Create('Projection offset is outside its exact text');
    end;
  end;
end;

function Slice(const AText: TNyxText; AStart, ACount: Integer): TNyxText;
var
  LStart: Integer;
  LEnd: Integer;
  LCount: Integer;
begin
  LCount := NyxTextScalarCount(AText);

  if (AStart < 0) or (AStart > LCount) or (ACount < 0) or (ACount > LCount - AStart) then
  begin
    raise ENyxTextProjection.Create('Projection range is outside its exact text');
  end;
  LStart := StorageAt(AText, AStart);
  LEnd := StorageAt(AText, AStart + ACount);
  Result := Copy(AText, LStart, LEnd - LStart);
end;

function ReplaceRange(const AText: TNyxText; AStart, ACount: Integer;
  const AInsertion: TNyxText): TNyxText;
var
  LStart: Integer;
  LEnd: Integer;
begin
  LStart := StorageAt(AText, AStart);
  LEnd := StorageAt(AText, AStart + ACount);
  Result := Copy(AText, 1, LStart - 1) + AInsertion + Copy(AText, LEnd, MaxInt);
end;

procedure ReadToken(const AText: TNyxText; var AAt: Integer;
  out AScalar, AWidth: Integer; out ANewline: Boolean);
begin

  if not NyxNextScalar(AText, AAt, AScalar) then
  begin
    raise ENyxTextProjection.Create('Projection text contains invalid Unicode');
  end;
  AWidth := 1;
  ANewline := AScalar in [10, 13];

  if (AScalar = 13) and (AAt <= Length(AText)) and (Ord(AText[AAt]) = 10) then
  begin
    Inc(AAt);
    AWidth := 2;
  end;

  if ANewline then
  begin
    AScalar := 10;
  end;
end;

function Equivalent(const ALeft, ARight: TNyxText): Boolean;
var
  LLeft: Integer;
  LRight: Integer;
  LLeftScalar: Integer;
  LRightScalar: Integer;
  LWidth: Integer;
  LLeftNewline: Boolean;
  LRightNewline: Boolean;
begin
  LLeft := 1;
  LRight := 1;
  while (LLeft <= Length(ALeft)) and (LRight <= Length(ARight)) do
  begin
    ReadToken(ALeft, LLeft, LLeftScalar, LWidth, LLeftNewline);
    ReadToken(ARight, LRight, LRightScalar, LWidth, LRightNewline);

    if (LLeftScalar <> LRightScalar) or (LLeftNewline <> LRightNewline) then
    begin
      Exit(False);
    end;
  end;
  Result := (LLeft > Length(ALeft)) and (LRight > Length(ARight));
end;

constructor TTextProjection.Create(const AAuthored, APresented: TNyxText);
var
  LAAt: Integer;
  LPAt: Integer;
  LAScalar: Integer;
  LPScalar: Integer;
  LAWidth: Integer;
  LPWidth: Integer;
  LANewline: Boolean;
  LPNewline: Boolean;
  LAStart: Integer;
  LPStart: Integer;
  LCount: Integer;
begin
  inherited Create;
  FAuthoredCount := NyxTextScalarCount(AAuthored);
  FPresentedCount := NyxTextScalarCount(APresented);
  LAAt := 1;
  LPAt := 1;
  LAStart := 0;
  LPStart := 0;
  LCount := 0;
  while (LAAt <= Length(AAuthored)) and (LPAt <= Length(APresented)) do
  begin
    ReadToken(AAuthored, LAAt, LAScalar, LAWidth, LANewline);
    ReadToken(APresented, LPAt, LPScalar, LPWidth, LPNewline);

    if (LAScalar <> LPScalar) or (LANewline <> LPNewline) then
    begin
      raise ENyxTextProjection.Create('Authored and presented text differ beyond line endings');
    end;

    if LAWidth <> LPWidth then
    begin

      if LCount = Length(FBreaks) then
      begin
        SetLength(FBreaks, LCount + 16 + LCount div 2);
      end;
      FBreaks[LCount].AuthoredStart := LAStart;
      FBreaks[LCount].PresentedStart := LPStart;
      FBreaks[LCount].AuthoredWidth := LAWidth;
      FBreaks[LCount].PresentedWidth := LPWidth;
      Inc(LCount);
    end;
    Inc(LAStart, LAWidth);
    Inc(LPStart, LPWidth);
  end;

  if (LAAt <= Length(AAuthored)) or (LPAt <= Length(APresented)) then
  begin
    raise ENyxTextProjection.Create('Projection texts have different logical lengths');
  end;
  SetLength(FBreaks, LCount);
  FAuthored := AAuthored;
  FPresented := APresented;
end;

function TTextProjection.Authored: TNyxText;
begin
  Result := FAuthored;
end;

function TTextProjection.Presented: TNyxText;
begin
  Result := FPresented;
end;

function TTextProjection.AuthoredScalars: Integer;
begin
  Result := FAuthoredCount;
end;

function TTextProjection.PresentedScalars: Integer;
begin
  Result := FPresentedCount;
end;

function TTextProjection.Map(AOffset: Integer; AReverse: Boolean;
  AAffinity: TNyxTextProjectionAffinity): Integer;
var
  LLow: Integer;
  LHigh: Integer;
  LMid: Integer;
  LAt: Integer;
  LFrom: Integer;
  LTo: Integer;
  LFromWidth: Integer;
  LToWidth: Integer;
  LMaximum: Integer;
begin
  LMaximum := FPresentedCount;

  if AReverse then
  begin
    LMaximum := FAuthoredCount;
  end;

  if (AOffset < 0) or (AOffset > LMaximum) or
    (Ord(AAffinity) < Ord(Low(TNyxTextProjectionAffinity))) or
    (Ord(AAffinity) > Ord(High(TNyxTextProjectionAffinity))) then
  begin
    raise ENyxTextProjection.Create('Projection boundary or affinity is outside its domain');
  end;
  LLow := 0;
  LHigh := High(FBreaks);
  LAt := -1;
  while LLow <= LHigh do
  begin
    LMid := LLow + (LHigh - LLow) div 2;
    LFrom := FBreaks[LMid].PresentedStart;

    if AReverse then
    begin
      LFrom := FBreaks[LMid].AuthoredStart;
    end;

    if LFrom <= AOffset then
    begin
      LAt := LMid;
      LLow := LMid + 1;
    end
    else
    begin
      LHigh := LMid - 1;
    end;
  end;

  if LAt < 0 then
  begin
    Exit(AOffset);
  end;
  LFrom := FBreaks[LAt].PresentedStart;
  LTo := FBreaks[LAt].AuthoredStart;
  LFromWidth := FBreaks[LAt].PresentedWidth;
  LToWidth := FBreaks[LAt].AuthoredWidth;

  if AReverse then
  begin
    LFrom := FBreaks[LAt].AuthoredStart;
    LTo := FBreaks[LAt].PresentedStart;
    LFromWidth := FBreaks[LAt].AuthoredWidth;
    LToWidth := FBreaks[LAt].PresentedWidth;
  end;

  if AOffset = LFrom then
  begin
    Exit(LTo);
  end;

  if AOffset < LFrom + LFromWidth then
  begin
    case AAffinity of
      npaBefore: Exit(LTo);
      npaAfter: Exit(LTo + LToWidth);
    else
      raise ENyxTextProjection.Create('CRLF interior has no exact projected boundary');
    end;
  end;
  Result := AOffset - LFrom - LFromWidth + LTo + LToWidth;
end;

function TTextProjection.ToAuthored(AOffset: Integer;
  AAffinity: TNyxTextProjectionAffinity): Integer;
begin
  Result := Map(AOffset, False, AAffinity);
end;

function TTextProjection.ToPresented(AOffset: Integer;
  AAffinity: TNyxTextProjectionAffinity): Integer;
begin
  Result := Map(AOffset, True, AAffinity);
end;

function TTextProjection.AuthoredSelection(const ASelection: TNyxTextSelection): TNyxTextSelection;
begin
  Result := Default(TNyxTextSelection);

  if ASelection.Defined then
  begin
    Result := NyxTextSelection(FAuthored, ToAuthored(ASelection.Start),
      ToAuthored(ASelection.Finish), ASelection.Direction);
  end;
end;

function TTextProjection.PresentedSelection(const ASelection: TNyxTextSelection): TNyxTextSelection;
begin
  Result := Default(TNyxTextSelection);

  if ASelection.Defined then
  begin
    Result := NyxTextSelection(FPresented, ToPresented(ASelection.Start),
      ToPresented(ASelection.Finish), ASelection.Direction);
  end;
end;

function TTextProjection.AuthoredSlice(AStart, ACount: Integer): TNyxText;
begin
  Result := Slice(FAuthored, AStart, ACount);
end;

function TTextProjection.PresentedSlice(AStart, ACount: Integer): TNyxText;
begin
  Result := Slice(FPresented, AStart, ACount);
end;

function TTextProjection.ReplacePresented(AStart: Integer; const AExpected: TNyxText;
  const AInsertion: TNyxTextInsertion): INyxTextProjection;
var
  LCount: Integer;
  LAStart: Integer;
  LAEnd: Integer;
begin
  AInsertion.Validate;
  LCount := NyxTextScalarCount(AExpected);

  if PresentedSlice(AStart, LCount) <> AExpected then
  begin
    raise ENyxTextProjection.Create('Presented expected text no longer matches');
  end;
  LAStart := ToAuthored(AStart);
  LAEnd := ToAuthored(AStart + LCount);
  Result := NewNyxTextProjection(
    ReplaceRange(FAuthored, LAStart, LAEnd - LAStart, AInsertion.Authored),
    ReplaceRange(FPresented, AStart, LCount, AInsertion.Presented));
end;

{ These are target storage boundaries, not offsets. They are used only after
  complete Unicode validation; newline interior checks are independent of UTF. }
function SafeBoundary(const AText: TNyxText; AAt: Integer): Boolean;
var
  LValue: Integer;
begin
  Result := True;

  if (AAt <= 1) or (AAt > Length(AText)) then
  begin
    Exit;
  end;
  LValue := Ord(AText[AAt]);
  {$ifdef PAS2JS}
  Result := not ((LValue >= $DC00) and (LValue <= $DFFF));
  {$else}
  Result := (LValue < $80) or (LValue >= $C0);
  {$endif}
  Result := Result and not ((LValue = 10) and (Ord(AText[AAt - 1]) = 13));
end;

function TTextProjection.ObservePresented(const AText: TNyxText): INyxTextProjection;
var
  LOldAt: Integer;
  LNewAt: Integer;
  LOldNext: Integer;
  LNewNext: Integer;
  LOldScalar: Integer;
  LNewScalar: Integer;
  LPrefix: Integer;
  LSuffix: Integer;
  LExpected: TNyxText;
  LInsertion: TNyxText;
begin
  NyxTextScalarCount(AText);

  if AText = FPresented then
  begin
    Exit(Self);
  end;
  LOldAt := 1;
  LNewAt := 1;
  LPrefix := 0;
  while (LOldAt <= Length(FPresented)) and (LNewAt <= Length(AText)) do
  begin
    LOldNext := LOldAt;
    LNewNext := LNewAt;
    NyxNextScalar(FPresented, LOldNext, LOldScalar);
    NyxNextScalar(AText, LNewNext, LNewScalar);

    if LOldScalar <> LNewScalar then
    begin
      Break;
    end;
    LOldAt := LOldNext;
    LNewAt := LNewNext;
    Inc(LPrefix);
  end;

  if not SafeBoundary(FPresented, LOldAt) or not SafeBoundary(AText, LNewAt) then
  begin
    Dec(LOldAt);
    Dec(LNewAt);
    Dec(LPrefix);
  end;
  LSuffix := 0;
  while (LSuffix < Length(FPresented) - LOldAt + 1) and
    (LSuffix < Length(AText) - LNewAt + 1) and
    (FPresented[Length(FPresented) - LSuffix] = AText[Length(AText) - LSuffix]) do
  begin
    Inc(LSuffix);
  end;
  while (LSuffix > 0) and
    (not SafeBoundary(FPresented, Length(FPresented) - LSuffix + 1) or
    not SafeBoundary(AText, Length(AText) - LSuffix + 1)) do
  begin
    Dec(LSuffix);
  end;
  LExpected := Copy(FPresented, LOldAt, Length(FPresented) - LSuffix - LOldAt + 1);
  LInsertion := Copy(AText, LNewAt, Length(AText) - LSuffix - LNewAt + 1);

  if Equivalent(LExpected, LInsertion) then
  begin
    { A target-only newline respelling is presentation, not an authored edit.
      Explicit ReplacePresented remains available for deliberate source changes. }
    Exit(NewNyxTextProjection(FAuthored, AText));
  end;
  Result := ReplacePresented(LPrefix, LExpected, NyxTextInsertion(LInsertion));
end;

function NewNyxTextProjection(const AAuthored, APresented: TNyxText): INyxTextProjection;
begin
  Result := TTextProjection.Create(AAuthored, APresented);
end;

function NyxTextInsertion(const AText: TNyxText): TNyxTextInsertion;
begin
  NyxTextScalarCount(AText);
  Result.FDefined := True;
  Result.FAuthored := AText;
  Result.FPresented := AText;
end;

function TNyxTextInsertion.PresentedAs(const AText: TNyxText): TNyxTextInsertion;
var
  LProjection: INyxTextProjection;
begin
  Validate;
  LProjection := NewNyxTextProjection(FAuthored, AText);
  Result := Self;
  Result.FPresented := LProjection.Presented;
end;

function TNyxTextInsertion.UsingLineEndings(APolicy: TNyxTextLineEndings): TNyxTextInsertion;
begin
  Result := PresentedAs(NyxPresentText(FAuthored, APolicy));
end;

procedure TNyxTextInsertion.Validate;
var
  LProjection: INyxTextProjection;
begin

  if not FDefined then
  begin
    raise ENyxTextProjection.Create('Text insertion must be explicitly constructed');
  end;
  LProjection := NewNyxTextProjection(FAuthored, FPresented);
end;

function NyxPresentText(const AText: TNyxText; APolicy: TNyxTextLineEndings): TNyxText;
var
  LParts: TNyxStrings;
  LAt: Integer;
  LStart: Integer;
  LNext: Integer;
  LScalar: Integer;
  LEnding: TNyxText;
begin
  NyxTextScalarCount(AText);

  if (Ord(APolicy) < Ord(Low(TNyxTextLineEndings))) or
    (Ord(APolicy) > Ord(High(TNyxTextLineEndings))) then
  begin
    raise ENyxTextProjection.Create('Unknown text line-ending presentation policy');
  end;
  case APolicy of
    ntePreserve: Exit(AText);
    nteLF: LEnding := #10;
    nteCRLF: LEnding := #13#10;
    nteCR: LEnding := #13;
  end;
  LParts := TNyxStrings.Create;
  try
    LAt := 1;
    LStart := 1;
    while LAt <= Length(AText) do
    begin
      LNext := LAt;
      NyxNextScalar(AText, LNext, LScalar);

      if LScalar in [10, 13] then
      begin
        LParts.Add(Copy(AText, LStart, LAt - LStart) + LEnding);

        if (LScalar = 13) and (LNext <= Length(AText)) and (Ord(AText[LNext]) = 10) then
        begin
          Inc(LNext);
        end;
        LStart := LNext;
      end;
      LAt := LNext;
    end;
    LParts.Add(Copy(AText, LStart, MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

end.
