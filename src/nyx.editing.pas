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

unit nyx.editing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text;

type
  { Closed editing intentions from Input Events Level 2. Native slots that cannot
    identify an intention report Unknown; adapters never infer clipboard/IME
    meaning from a shortcut key. Unsupported future wire names remain diagnostic
    data in a snapshot, rather than changing behavior through raw strings. }
  TNyxEditIntent = (neiUnknown,
    neiInsertText,
    neiInsertReplacementText,
    neiInsertLineBreak,
    neiInsertParagraph,
    neiInsertOrderedList,
    neiInsertUnorderedList,
    neiInsertHorizontalRule,
    neiInsertFromYank,
    neiInsertFromDrop,
    neiInsertFromPaste,
    neiInsertFromPasteAsQuotation,
    neiInsertTranspose,
    neiInsertCompositionText,
    neiInsertLink,
    neiDeleteWordBackward,
    neiDeleteWordForward,
    neiDeleteSoftLineBackward,
    neiDeleteSoftLineForward,
    neiDeleteEntireSoftLine,
    neiDeleteHardLineBackward,
    neiDeleteHardLineForward,
    neiDeleteByDrag,
    neiDeleteByCut,
    neiDeleteContent,
    neiDeleteContentBackward,
    neiDeleteContentForward,
    neiHistoryUndo,
    neiHistoryRedo,
    neiFormatBold,
    neiFormatItalic,
    neiFormatUnderline,
    neiFormatStrikeThrough,
    neiFormatSuperscript,
    neiFormatSubscript,
    neiFormatJustifyFull,
    neiFormatJustifyCenter,
    neiFormatJustifyRight,
    neiFormatJustifyLeft,
    neiFormatIndent,
    neiFormatOutdent,
    neiFormatRemove,
    neiFormatSetBlockTextDirection,
    neiFormatSetInlineTextDirection,
    neiFormatBackColor,
    neiFormatFontColor,
    neiFormatFontName);
  TNyxTextDirection = (ntdNone, ntdForward, ntdBackward, ntdUnknown);
  TNyxEditingPhase = (nepObservation, nepBeforeEdit, nepInput,
    nepCompositionStart, nepCompositionUpdate, nepCompositionEnd,
    nepSelectionChange);

  { An immutable selection in zero-based Unicode scalar offsets, including its
    exclusive end. These are not grapheme clusters, UTF-8 bytes or UTF-16 units.
    Defined=False means the platform cannot describe the selection. An empty
    selection is still defined. Direction Unknown is distinct from a collapsed
    caret/None, because standard LCL slots do not expose the active endpoint. }
  TNyxTextSelection = record
  private
    FDefined: Boolean;
    FStart: Integer;
    FFinish: Integer;
    FDirection: TNyxTextDirection;
    function GetCollapsed: Boolean;
  public
    function SameRange(const AOther: TNyxTextSelection): Boolean;
    property Defined: Boolean read FDefined;
    property Start: Integer read FStart;
    property Finish: Integer read FFinish;
    property Direction: TNyxTextDirection read FDirection;
    property Collapsed: Boolean read GetCollapsed;
  end;

  { Owned physical editing context, independent of accepted model state.
    Text is the current physical value; Data is optional insertion/composition
    data and must be guarded by HasData. CanCancel describes a physical
    pre-edit window only, never the ability to roll back model admission.
    Composition lifecycle notifications and post-edit snapshots cannot cancel.
    Copies contain immutable text/selection values and no platform handles. }
  TNyxEditingSnapshot = record
  private
    { Managed marker makes an untouched compiler-initialized record undefined
      even when older client code initializes only its former event fields. }
    FMarker: TNyxText;
    FPhase: TNyxEditingPhase;
    FIntent: TNyxEditIntent;
    FWireIntent: TNyxText;
    FText: TNyxText;
    FData: TNyxText;
    FHasData: Boolean;
    FSelection: TNyxTextSelection;
    FComposing: Boolean;
    FCanCancel: Boolean;
    function GetDefined: Boolean;
  public
    property Defined: Boolean read GetDefined;
    property Phase: TNyxEditingPhase read FPhase;
    property Intent: TNyxEditIntent read FIntent;
    property WireIntent: TNyxText read FWireIntent;
    property Text: TNyxText read FText;
    property Data: TNyxText read FData;
    property HasData: Boolean read FHasData;
    property Selection: TNyxTextSelection read FSelection;
    property Composing: Boolean read FComposing;
    property CanCancel: Boolean read FCanCancel;
  end;

{ Checked wire translation belongs exclusively to the adapter boundary.
  Unknown/empty values return False and explicitly produce neiUnknown. }
function NyxEditIntentName(AIntent: TNyxEditIntent): TNyxText;
function TryNyxEditIntent(const AName: TNyxText; out AIntent: TNyxEditIntent): Boolean;

{ Constructors validate Unicode and bounds before returning an owned value.
  UTF16 conversion rejects offsets inside a surrogate pair instead of silently
  describing a different selection. Platform capture may represent that
  unsupported boundary as an undefined selection without discarding an edit. }
function NyxTextScalarCount(const AText: TNyxText): Integer;
function NyxTextUTF16Offset(const AText: TNyxText; AScalarOffset: Integer): Integer;
function NyxTextScalarOffset(const AText: TNyxText; AUTF16Offset: Integer): Integer;
function NyxTextSelection(const AText: TNyxText; AStart, AFinish: Integer;
  ADirection: TNyxTextDirection = ntdNone): TNyxTextSelection;
function NyxTextSelectionUTF16(const AText: TNyxText; AStart, AFinish: Integer;
  ADirection: TNyxTextDirection = ntdNone): TNyxTextSelection;
function NyxEditingSnapshot(APhase: TNyxEditingPhase; AIntent: TNyxEditIntent;
  const AText, AData: TNyxText; AHasData: Boolean;
  const ASelection: TNyxTextSelection; AComposing, ACanCancel: Boolean;
  const AWireIntent: TNyxText = ''): TNyxEditingSnapshot;

implementation

const
  CIntentNames: array[TNyxEditIntent] of TNyxText =
    ('',
      'insertText',
      'insertReplacementText',
      'insertLineBreak',
      'insertParagraph',
      'insertOrderedList',
      'insertUnorderedList',
      'insertHorizontalRule',
      'insertFromYank',
      'insertFromDrop',
      'insertFromPaste',
      'insertFromPasteAsQuotation',
      'insertTranspose',
      'insertCompositionText',
      'insertLink',
      'deleteWordBackward',
      'deleteWordForward',
      'deleteSoftLineBackward',
      'deleteSoftLineForward',
      'deleteEntireSoftLine',
      'deleteHardLineBackward',
      'deleteHardLineForward',
      'deleteByDrag',
      'deleteByCut',
      'deleteContent',
      'deleteContentBackward',
      'deleteContentForward',
      'historyUndo',
      'historyRedo',
      'formatBold',
      'formatItalic',
      'formatUnderline',
      'formatStrikeThrough',
      'formatSuperscript',
      'formatSubscript',
      'formatJustifyFull',
      'formatJustifyCenter',
      'formatJustifyRight',
      'formatJustifyLeft',
      'formatIndent',
      'formatOutdent',
      'formatRemove',
      'formatSetBlockTextDirection',
      'formatSetInlineTextDirection',
      'formatBackColor',
      'formatFontColor',
      'formatFontName');

procedure CheckIntent(AIntent: TNyxEditIntent);
begin

  if (Ord(AIntent) < Ord(Low(TNyxEditIntent))) or
    (Ord(AIntent) > Ord(High(TNyxEditIntent))) then
  begin
    raise EArgumentException.Create('Unknown editing intention ordinal');
  end;
end;

function NyxEditIntentName(AIntent: TNyxEditIntent): TNyxText;
begin
  CheckIntent(AIntent);
  Result := CIntentNames[AIntent];
end;

function TryNyxEditIntent(const AName: TNyxText; out AIntent: TNyxEditIntent): Boolean;
var
  LIntent: TNyxEditIntent;
begin
  AIntent := neiUnknown;
  for LIntent := Succ(Low(TNyxEditIntent)) to High(TNyxEditIntent) do
  begin

    if CIntentNames[LIntent] = AName then
    begin
      AIntent := LIntent;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxTextScalarCount(const AText: TNyxText): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  Result := 0;
  LIndex := 1;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise EArgumentException.Create('Editing text contains malformed Unicode');
    end;
    Inc(Result);
  end;
end;

function NyxTextUTF16Offset(const AText: TNyxText; AScalarOffset: Integer): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  { Reject malformed text even when the requested offset precedes its invalid
    suffix. Conversion is a public admission boundary, not a prefix decoder. }
  NyxTextScalarCount(AText);

  if AScalarOffset < 0 then
  begin
    raise EArgumentException.Create('Text offsets are zero-based');
  end;
  Result := 0;
  LCount := 0;
  LIndex := 1;
  while LCount < AScalarOffset do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise EArgumentException.Create('Text offset is outside admitted Unicode');
    end;
    Inc(LCount);
    Inc(Result);

    if LScalar > $ffff then
    begin
      Inc(Result);
    end;
  end;
end;

function NyxTextScalarOffset(const AText: TNyxText; AUTF16Offset: Integer): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
  LUnits: Integer;
begin
  NyxTextScalarCount(AText);

  if AUTF16Offset < 0 then
  begin
    raise EArgumentException.Create('Text offsets are zero-based');
  end;
  Result := 0;
  LUnits := 0;
  LIndex := 1;
  while LUnits < AUTF16Offset do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise EArgumentException.Create('UTF-16 offset is outside admitted Unicode');
    end;
    Inc(LUnits);

    if LScalar > $ffff then
    begin
      Inc(LUnits);
    end;
    Inc(Result);
  end;

  if LUnits <> AUTF16Offset then
  begin
    raise EArgumentException.Create('UTF-16 selection splits a Unicode scalar');
  end;
end;

function NyxTextSelection(const AText: TNyxText; AStart, AFinish: Integer;
  ADirection: TNyxTextDirection): TNyxTextSelection;
var
  LLength: Integer;
begin
  Result := Default(TNyxTextSelection);
  LLength := NyxTextScalarCount(AText);

  if (AStart < 0) or (AFinish < AStart) or (AFinish > LLength) or
    (Ord(ADirection) < Ord(Low(TNyxTextDirection))) or
    (Ord(ADirection) > Ord(High(TNyxTextDirection))) then
  begin
    raise EArgumentException.Create('Selection requires ordered Unicode scalar bounds');
  end;
  Result.FDefined := True;
  Result.FStart := AStart;
  Result.FFinish := AFinish;
  Result.FDirection := ADirection;
end;

function NyxTextSelectionUTF16(const AText: TNyxText; AStart, AFinish: Integer;
  ADirection: TNyxTextDirection): TNyxTextSelection;
begin
  Result := NyxTextSelection(AText, NyxTextScalarOffset(AText, AStart),
    NyxTextScalarOffset(AText, AFinish), ADirection);
end;

function TNyxTextSelection.GetCollapsed: Boolean;
begin
  Result := FDefined and (FStart = FFinish);
end;

function TNyxTextSelection.SameRange(const AOther: TNyxTextSelection): Boolean;
begin
  Result := (FDefined = AOther.FDefined) and
    (not FDefined or ((FStart = AOther.FStart) and (FFinish = AOther.FFinish) and
      (FDirection = AOther.FDirection)));
end;

function NyxEditingSnapshot(APhase: TNyxEditingPhase; AIntent: TNyxEditIntent;
  const AText, AData: TNyxText; AHasData: Boolean;
  const ASelection: TNyxTextSelection; AComposing, ACanCancel: Boolean;
  const AWireIntent: TNyxText): TNyxEditingSnapshot;
var
  LLength: Integer;
begin
  CheckIntent(AIntent);
  LLength := NyxTextScalarCount(AText);
  NyxTextScalarCount(AWireIntent);

  if AHasData then
  begin
    NyxTextScalarCount(AData);
  end;

  if ASelection.Defined and (ASelection.Finish > LLength) then
  begin
    raise EArgumentException.Create('Editing selection is outside its physical text');
  end;

  if (Ord(APhase) < Ord(Low(TNyxEditingPhase))) or
    (Ord(APhase) > Ord(High(TNyxEditingPhase))) or
    (ACanCancel and ((APhase <> nepBeforeEdit) or AComposing)) then
  begin
    raise EArgumentException.Create('Only a cancellable noncomposing pre-edit can cancel');
  end;

  if (AIntent <> neiUnknown) and (AWireIntent <> '') and
    (AWireIntent <> NyxEditIntentName(AIntent)) then
  begin
    raise EArgumentException.Create('Editing intention conflicts with its wire identity');
  end;
  Result := Default(TNyxEditingSnapshot);
  Result.FMarker := 'nyx.editing.v1';
  Result.FPhase := APhase;
  Result.FIntent := AIntent;
  Result.FWireIntent := AWireIntent;
  Result.FText := AText;
  Result.FHasData := AHasData;

  if AHasData then
  begin
    Result.FData := AData;
  end;
  Result.FSelection := ASelection;
  Result.FComposing := AComposing;
  Result.FCanCancel := ACanCancel;
end;

function TNyxEditingSnapshot.GetDefined: Boolean;
begin
  Result := FMarker = 'nyx.editing.v1';
end;

end.
