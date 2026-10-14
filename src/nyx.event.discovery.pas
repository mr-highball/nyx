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
unit nyx.event.discovery;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.schema, nyx.callbacks;

type
  { Discovery groups overlap: keyboard selection can appear in both families.
    All preserves the complete schema, including custom named component events.
    These are presentation choices, never event dispatch or compiler directives. }
  TNyxEventCategory = (necAll, necActions, necKeyboard, necText, necFocus,
    necPointer, necScroll, necDrag, necSelection, necImages);
  TNyxEventFilterPart = (nefpQuery, nefpCategory, nefpRegistered, nefpClear, nefpStatus);
  { Immutable copied query. ASCII letters compare case insensitively; other
    Unicode remains exact. Whitespace-separated terms must all occur somewhere
    in event help, identity or registered handler names. No tree is retained. }
  TNyxEventFilter = record
  private
    FQuery: TNyxText;
    FCategory: TNyxEventCategory;
    FRegistered: Boolean;
  public
    function Query(const AValue: TNyxText): TNyxEventFilter;
    function Category(AValue: TNyxEventCategory): TNyxEventFilter;
    function WithCallbacks(AValue: Boolean): TNyxEventFilter;
    procedure Validate;
    property SearchText: TNyxText read FQuery;
    property CategoryValue: TNyxEventCategory read FCategory;
    property RegisteredOnly: Boolean read FRegistered;
  end;
  { Matching counts query matches. Pinned counts extra review cards outside that
    query; Shown includes both, keeping a live removal warning visible. }
  TNyxEventFilterResult = record
    Total: Integer;
    Matching: Integer;
    Pinned: Integer;
    Shown: Integer;
  end;

{ Default complete list; fluent methods return independent value copies. }
function NyxEventFilter: TNyxEventFilter;
{ Closed English UI captions and stable part IDs; invalid enum casts refuse. }
function NyxEventCategoryCaption(AValue: TNyxEventCategory): TNyxText;
function NyxEventFilterPartID(const AID: TNyxText; APart: TNyxEventFilterPart): TNyxText;
{ Public Nyx Card/Input/Select/Checkbox/Button composition. The returned managed
  card owns every field; caller transfers it to a page/container normally. }
function NewNyxEventFilterEditor(const AID: TNyxText;
  const AFilter: TNyxEventFilter): INyxCard;
{ Read only a complete exact field group at the UI boundary. Absent returns
  False; wrong kinds/choices refuse. No input or accepted document is changed. }
function ReadNyxEventFilterEditor(const AID: TNyxText; ARoot: TNyxNode;
  out AFilter: TNyxEventFilter): Boolean;
{ Copy searchable metadata into this card's private extension boundary. There
  are no borrowed schema routes/callbacks and no reverse tree reference. Data
  survives realization/codec copies. Re-tag after registrations/help change. }
procedure TagNyxEventDiscovery(ACard: TNyxNode; const ASchema: TNyxEventSchema;
  const AEvent: TNyxAuthoredEventInfo; APinned: Boolean = False);
{ UI-thread filtering of tagged cards only. Validate all copied contexts before
  any visibility write. Untagged children, field values and callbacks remain
  owned/in place. Caller synchronizes its adapter afterward; no history is added. }
function ApplyNyxEventFilter(ARoot: TNyxNode;
  const AFilter: TNyxEventFilter): TNyxEventFilterResult;

implementation

const
  CContextKey = 'nyx.event-discovery';
  CCaptions: array[TNyxEventCategory] of TNyxText = ('All events', 'Actions',
    'Keyboard', 'Text and editing', 'Focus', 'Pointer', 'Wheel and scrolling',
    'Drag and drop', 'Selection', 'Images');
  CSuffixes: array[TNyxEventFilterPart] of TNyxText =
    ('-query', '-category', '-registered', '-clear', '-status');

function NyxEventFilter: TNyxEventFilter;
begin
  Result := Default(TNyxEventFilter);
end;

function TNyxEventFilter.Query(const AValue: TNyxText): TNyxEventFilter;
begin
  Result := Self;
  Result.FQuery := AValue;
end;

function TNyxEventFilter.Category(AValue: TNyxEventCategory): TNyxEventFilter;
begin
  Result := Self;
  Result.FCategory := AValue;
  Result.Validate;
end;

function TNyxEventFilter.WithCallbacks(AValue: Boolean): TNyxEventFilter;
begin
  Result := Self;
  Result.FRegistered := AValue;
end;

procedure TNyxEventFilter.Validate;
begin

  if (Ord(FCategory) < Ord(Low(TNyxEventCategory))) or
    (Ord(FCategory) > Ord(High(TNyxEventCategory))) then
  begin
    raise ENyxModel.Create('Unknown event discovery category');
  end;
end;

function NyxEventCategoryCaption(AValue: TNyxEventCategory): TNyxText;
begin
  NyxEventFilter.Category(AValue);
  Result := CCaptions[AValue];
end;

function NyxEventFilterPartID(const AID: TNyxText; APart: TNyxEventFilterPart): TNyxText;
begin

  if (Ord(APart) < Ord(Low(TNyxEventFilterPart))) or
    (Ord(APart) > Ord(High(TNyxEventFilterPart))) then
  begin
    raise ENyxModel.Create('Unknown event discovery part');
  end;
  Result := AID + CSuffixes[APart];
end;

function NewNyxEventFilterEditor(const AID: TNyxText;
  const AFilter: TNyxEventFilter): INyxCard;
var
  LCategory: TNyxEventCategory;
  LChoices: TNyxText;
begin
  AFilter.Validate;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(8).Surface(True).Done;
  Result.Add(NewNyxInput(NyxEventFilterPartID(AID, nefpQuery)).Configure
    .Text('Find an event').InputType(niSearch).Placeholder('KeyPress, focus, handler...')
    .Value(AFilter.SearchText).Done);
  LChoices := '';
  for LCategory := Low(TNyxEventCategory) to High(TNyxEventCategory) do
  begin

    if LChoices <> '' then
    begin
      LChoices := LChoices + #10;
    end;
    LChoices := LChoices + CCaptions[LCategory];
  end;
  Result.Add(NewNyxSelect(NyxEventFilterPartID(AID, nefpCategory)).Configure
    .Text('Event family').Items(LChoices).Value(CCaptions[AFilter.CategoryValue]).Done);
  Result.Add(NewNyxCheckbox(NyxEventFilterPartID(AID, nefpRegistered)).Configure
    .Text('With callbacks').Value(AFilter.RegisteredOnly).Done);
  Result.Add(NewNyxButton(NyxEventFilterPartID(AID, nefpClear)).Configure
    .Text('Clear filters').Variant(nvSecondary).Done);
  Result.Add(NewNyxLabel(NyxEventFilterPartID(AID, nefpStatus)).Configure
    .Text('All events').Done);
end;

function ReadNyxEventFilterEditor(const AID: TNyxText; ARoot: TNyxNode;
  out AFilter: TNyxEventFilter): Boolean;
var
  LQuery: TNyxNode;
  LCategory: TNyxNode;
  LRegistered: TNyxNode;
  LEditor: TNyxNode;
  LChoice: TNyxEventCategory;
  LBoolean: TNyxText;
begin
  Result := False;
  AFilter := NyxEventFilter;

  if (ARoot = nil) or (ARoot.Find(AID) = nil) then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(AID);
  LQuery := LEditor.Find(NyxEventFilterPartID(AID, nefpQuery));
  LCategory := LEditor.Find(NyxEventFilterPartID(AID, nefpCategory));
  LRegistered := LEditor.Find(NyxEventFilterPartID(AID, nefpRegistered));

  if (LQuery = nil) or (LCategory = nil) or (LRegistered = nil) or
    (LQuery.Kind <> NyxKindName(nkInput)) or (LCategory.Kind <> NyxKindName(nkSelect)) or
    (LRegistered.Kind <> NyxKindName(nkCheckbox)) then
  begin
    raise ENyxModel.Create('Event discovery requires its complete typed fields');
  end;
  for LChoice := Low(TNyxEventCategory) to High(TNyxEventCategory) do
  begin

    if LCategory.Prop(NyxAttributeName(atValue)) = CCaptions[LChoice] then
    begin
      LBoolean := LRegistered.Prop(NyxAttributeName(atValue));

      if (LBoolean <> 'true') and (LBoolean <> 'false') then
      begin
        raise ENyxModel.Create('Event discovery requires a Boolean registration choice');
      end;
      AFilter := NyxEventFilter.Query(LQuery.Prop(NyxAttributeName(atValue)))
        .Category(LChoice).WithCallbacks(LBoolean = 'true');
      Exit(True);
    end;
  end;
  raise ENyxModel.Create('Unknown event discovery family choice');
end;

procedure TagNyxEventDiscovery(ACard: TNyxNode; const ASchema: TNyxEventSchema;
  const AEvent: TNyxAuthoredEventInfo; APinned: Boolean);
var
  LCategories: array of TNyxDataValue;
  LText: TNyxText;
  LIndex: Integer;
  LContexts: TNyxEventContexts;

  procedure Include(ACondition: Boolean; ACategory: TNyxEventCategory);
  begin

    if ACondition then
    begin
      SetLength(LCategories, Length(LCategories) + 1);
      LCategories[High(LCategories)] := NyxData(Ord(ACategory));
      LText := LText + #10 + CCaptions[ACategory];
    end;
  end;

begin

  if ACard = nil then
  begin
    raise ENyxModel.Create('Event discovery requires an owned card');
  end;
  LContexts := ASchema.Contexts;
  LText := ASchema.Title + #10 + ASchema.Description + #10 + ASchema.Name.Name +
    #10 + NyxTriggerName(ASchema.Trigger);
  Include(ASchema.Trigger in [ntClick, ntDoubleClick, ntChange, ntNamed], necActions);
  Include(nctxKeyboard in LContexts, necKeyboard);
  Include((nctxTextEdit in LContexts) or (nctxEditing in LContexts), necText);
  Include(ASchema.Trigger in [ntAfterEnter, ntAfterExit], necFocus);
  Include(nctxPointer in LContexts, necPointer);
  Include((nctxWheel in LContexts) or (nctxViewport in LContexts), necScroll);
  Include(nctxDrag in LContexts, necDrag);
  Include((nctxCollectionSelection in LContexts) or (ASchema.Trigger = ntTextSelectionChange),
    necSelection);
  Include(nctxImage in LContexts, necImages);
  for LIndex := 0 to High(AEvent.Callbacks) do
  begin
    LText := LText + #10 + AEvent.Callbacks[LIndex].Handler.Name +
      #10 + AEvent.Callbacks[LIndex].ID.Name;
  end;
  ACard.Extensions.SetValue(NyxExtension(CContextKey), NyxObject([
    NyxField('version', NyxData(1)), NyxField('categories', NyxArray(LCategories)),
    NyxField('search', NyxData(LText)), NyxField('registered', NyxData(Length(AEvent.Callbacks) > 0)),
    NyxField('pinned', NyxData(APinned))]));
end;

function FoldASCII(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := AText;
  for LIndex := 1 to Length(Result) do
  begin

    if (Result[LIndex] >= 'A') and (Result[LIndex] <= 'Z') then
    begin
      Result[LIndex] := Chr(Ord(Result[LIndex]) + 32);
    end;
  end;
end;

function MatchesText(const AQuery, AText: TNyxText): Boolean;
var
  LQuery: TNyxText;
  LText: TNyxText;
  LStart: Integer;
  LIndex: Integer;
begin
  LQuery := FoldASCII(AQuery);
  LText := FoldASCII(AText);
  LIndex := 1;
  while LIndex <= Length(LQuery) do
  begin
    while (LIndex <= Length(LQuery)) and (Ord(LQuery[LIndex]) <= 32) do
    begin
      Inc(LIndex);
    end;
    LStart := LIndex;
    while (LIndex <= Length(LQuery)) and (Ord(LQuery[LIndex]) > 32) do
    begin
      Inc(LIndex);
    end;

    if (LIndex > LStart) and (Pos(Copy(LQuery, LStart, LIndex - LStart), LText) = 0) then
    begin
      Exit(False);
    end;
  end;
  Result := True;
end;

function ApplyNyxEventFilter(ARoot: TNyxNode;
  const AFilter: TNyxEventFilter): TNyxEventFilterResult;
var
  LCards: array of TNyxNode;
  LVisible: array of Boolean;
  LIndex: Integer;
  LResult: TNyxEventFilterResult;

  procedure Inspect(ANode: TNyxNode);
  var
    LData: TNyxDataValue;
    LCategories: TNyxDataValue;
    LCategory: Integer;
    LChild: Integer;
    LMatch: Boolean;
    LPinned: Boolean;
    LRegistered: Boolean;
    LSearch: TNyxText;
  begin

    if ANode.Extensions.Has(NyxExtension(CContextKey)) then
    begin
      LData := ANode.Extensions.Value(NyxExtension(CContextKey));

      if (LData.Kind <> ndObject) or (LData.Count <> 5) or
        (LData.Field('version').AsInteger <> 1) then
      begin
        raise ENyxModel.Create('Invalid event discovery context');
      end;
      LCategories := LData.Field('categories');
      LRegistered := LData.Field('registered').AsBoolean;
      LSearch := LData.Field('search').AsText;
      LPinned := LData.Field('pinned').AsBoolean;

      if (LCategories.Kind <> ndArray) or
        (LCategories.Count > Ord(High(TNyxEventCategory))) then
      begin
        raise ENyxModel.Create('Invalid event discovery categories');
      end;
      LMatch := AFilter.CategoryValue = necAll;
      for LChild := 0 to LCategories.Count - 1 do
      begin
        LCategory := LCategories.Item(LChild).AsInteger;

        if (LCategory <= Ord(necAll)) or (LCategory > Ord(High(TNyxEventCategory))) then
        begin
          raise ENyxModel.Create('Unknown event discovery category');
        end;
        LMatch := LMatch or (LCategory = Ord(AFilter.CategoryValue));
      end;
      LMatch := LMatch and MatchesText(AFilter.SearchText, LSearch);
      LMatch := LMatch and (not AFilter.RegisteredOnly or LRegistered);
      Inc(LResult.Total);

      if LMatch then
      begin
        Inc(LResult.Matching);
      end
      else if LPinned then
      begin
        Inc(LResult.Pinned);
      end;
      SetLength(LCards, Length(LCards) + 1);
      SetLength(LVisible, Length(LVisible) + 1);
      LCards[High(LCards)] := ANode;
      LVisible[High(LVisible)] := LMatch or LPinned;

      if LMatch or LPinned then
      begin
        Inc(LResult.Shown);
      end;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Inspect(ANode.Children[LChild]);
    end;
  end;

begin
  AFilter.Validate;

  if ARoot = nil then
  begin
    raise ENyxModel.Create('Event discovery requires a live tree');
  end;
  LResult := Default(TNyxEventFilterResult);
  Inspect(ARoot);
  for LIndex := 0 to High(LCards) do
  begin
    LCards[LIndex].Configure.Visible(LVisible[LIndex]).Done;
  end;
  Result := LResult;
end;

end.
