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
program nyx_resource_labels_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.source, nyx.collections, nyx.resources, nyx.resource.sources,
  nyx.resources.catalog, nyx.resources.editor, nyx.resources.labels.editor, nyx.collections.query,
  nyx.schema, nyx.studio.session, nyx.studio.commands, nyx.studio.projects,
  nyx.studio.presentation
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Resource labels: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Typed slicing avoids an ANSI RTL round-trip while adapting the exact UTF-8
  generated unit. Every replacement is literal; the remaining source is copied
  without normalization, locale conversion or regular-expression behavior. }
function ReplaceText(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LRemaining: TNyxText;
  LPosition: Integer;
begin
  Result := '';
  LRemaining := ASource;
  LPosition := Pos(ABefore, LRemaining);
  while LPosition > 0 do
  begin
    Result := Result + Copy(LRemaining, 1, LPosition - 1) + AAfter;
    LRemaining := Copy(LRemaining, LPosition + Length(ABefore), Length(LRemaining));
    LPosition := Pos(ABefore, LRemaining);
  end;
  Result := Result + LRemaining;
end;

{ Admission cases exercise the same copied wire boundary used by projects,
  cache envelopes and alternative resource implementations. Refusal must leave
  the already admitted definition and accepted project available to callers. }
procedure RejectLabels(const AData: TNyxDataValue; const AReason: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    TNyxResourceLabels.FromData(AData);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
end;

procedure RejectDefinition(const AJSON: TNyxText; const AReason: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    NyxResourceFromData(TNyxDataValue.ParseJSON(AJSON));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
end;

{ This is the ordinary shared paired admission path, not a direct registry
  replacement. The form owns a proposal, and the session owns independent
  accepted document/source checkpoints. It makes no physical input claim. }
procedure PairedHistory(ADocument: TNyxDocument; const ALabels: TNyxResourceLabels);
var
  LSession: TNyxStudioSession;
  LForm: INyxCard;
  LDraft: TNyxResourceEditorDraft;
  LChange: TNyxResourceEditorChange;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LRejected: Boolean;
begin
  LSession := TNyxStudioSession.Create;
  LSchemas := CaptureNyxSchemas;
  try
    LSession.Load(TNyxCodec.Encode(ADocument));
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LForm := NewNyxResourceEditor('history-labels', LSession.Document.Resources,
      NyxResourceSelection(NyxResourceRef('notes'), NyxDefaultLocale));
    SetNyxResourceEditorLabels(LForm.Node, ALabels);
    LDraft.Capture(LForm.ID, LForm.Node);
    Check(CaptureNyxResourceEditor(LForm.Node.Find(
      NyxResourceEditorActionID(LForm.ID, reaApply)), LForm.Node, LChange),
      'ordinary form captures the typed label proposal');
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaResource;
    LEdit.Selection := 'welcome';
    LEdit.View := 'labels-home';
    LEdit.Resource := LChange;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(ReadNyxStudioDesignRequest(LRequest.ToData), LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'isolated request admits labelled resources and generated source');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'label edit publishes through one ordinary paired operation');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(NyxResourceLabelsOf(LSession.Document.Resources.Definition(
      NyxResourceRef('notes'), NyxDefaultLocale)).ToData.ToJSON = ALabels.ToData.ToJSON,
      'paired publication retains the exact labels');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'one Undo restores the exact previous source/document pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
      'one Redo restores the exact labelled source/document pair');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LPrepared.Diagnostic.Defined and
      (LSession.CompleteDesignRequest(LRequest, LPrepared) = nscRejected) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LAfter),
      'old mounted label Apply refuses without altering paired history');
    LForm := NewNyxResourceEditor('history-labels', LSession.Document.Resources,
      NyxResourceSelection(NyxResourceRef('notes'), NyxDefaultLocale));
    Check(not LDraft.Restore(LForm.Node), 'changed accepted catalog refuses old label draft restoration');
    LSession.SetSourceDraft(LSession.Source + TNyxText(#10) + '// Unsubmitted application work.');
    LRejected := False;
    try
      LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LSession.ProjectSnapshot.Pending,
      'pending source refuses label edits while preserving the unfinished draft');
  finally
    LPrepared := nil;
    LForm := nil;
    LSession.Free;
  end;
end;

{ These checks cover the public compound's proposal and strict enclosing
  preferences, including the version-ten gap that previously refused labels.
  Mounted target callbacks are exercised by the ordinary authoring consumer. }
procedure LabelEditorChecks;
var
  LCase: Integer;
  LForm: INyxCard;
  LFresh: INyxCard;
  LEditor: TNyxNode;
  LTags: TNyxNode;
  LInput: TNyxNode;
  LButton: TNyxNode;
  LForeign: TNyxNode;
  LResources: INyxResources;
  LState: TNyxResourceLabelsEditorState;
  LInvalid: TNyxResourceLabelsEditorState;
  LDraft: TNyxResourceEditorDraft;
  LPreference: TNyxStudioPresentation;
  LLoaded: TNyxStudioPresentation;
  LPacket: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LResources := NewNyxResources;
  LForm := NewNyxResourceEditor('tag-proposal', LResources, NyxNewResourceSelection);
  LTags := LForm.Node.Find(NyxResourceEditorLabelsID(LForm.Node.ID));
  LInput := LTags.Find(NyxResourceLabelsEditorFieldID(LTags.ID, rlefInput));
  LButton := LTags.Find(NyxResourceLabelsEditorActionID(LTags.ID, rleaAdd));
  LInput.Configure.Value('Docs, "quick" | 🌙').Done;
  Check(NyxResourceEditorInput(LInput, LForm.Node, LEditor) and (LEditor = LForm.Node),
    'owned tag input routes through the resource proposal');
  Check(HandleNyxResourceEditorLabels(LButton, LForm.Node, LEditor),
    'owned tag action uses the common resource controller contract');
  LState := ReadNyxResourceLabelsEditor(LTags);
  Check((LState.Labels.Count = 1) and (LState.Input = '') and
    (LState.Selection.Name = TNyxText('Docs, "quick" | 🌙')),
    'Add retains a complete exact tag and clears only its completed input');
  Check(LTags.Find(NyxResourceLabelsEditorActionID(LTags.ID, rleaRemove)).Prop('enabled') = 'true',
    'the selected exact tag exposes removal');
  LInput.Configure.Value('Docs, "quick" | 🌙').Done;
  HandleNyxResourceEditorLabels(LButton, LForm.Node, LEditor);
  Check(NyxResourceEditorLabels(LForm.Node).Count = 1, 'duplicate UI Add is idempotent');
  LInput.Configure.Value('Unfinished' + TNyxText(#10) + 'tag').Done;
  LBefore := ReadNyxResourceLabelsEditor(LTags).ToData.ToJSON;
  LRejected := False;
  try
    HandleNyxResourceEditorLabels(LButton, LForm.Node, LEditor);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (ReadNyxResourceLabelsEditor(LTags).ToData.ToJSON = LBefore),
    'invalid complete tag refuses while preserving partial input and selection');
  LForeign := LButton.Clone;
  try
    Check(not HandleNyxResourceEditorLabels(LForeign, LForm.Node, LEditor),
      'a foreign node with the same action ID cannot edit tags');
  finally
    LForeign.Free;
  end;
  LDraft.Capture(LForm.Node.ID, LForm.Node);
  Check(LDraft.ToData.Field('version').AsInteger = 4,
    'incomplete tag text and selected tag use a strict new proposal version');
  LPreference := DefaultNyxStudioPresentation;
  LPreference.ResourceDraft := LDraft;
  LPacket := TNyxDataValue.ParseJSON(EncodeNyxStudioPresentation(LPreference));
  Check(LPacket.Field('version').AsInteger = 13,
    'outer preferences declare their broader draft contract explicitly');
  LLoaded := DecodeNyxStudioPresentation(LPacket.ToJSON);
  LFresh := NewNyxResourceEditor(LForm.Node.ID, LResources, NyxNewResourceSelection);
  Check(LLoaded.ResourceDraft.Restore(LFresh.Node) and
    (ReadNyxResourceLabelsEditor(LFresh.Node.Find(LTags.ID)).ToData.ToJSON = LBefore),
    'ordinary enclosing preferences retain exact tags, selection and incomplete text');
  SetLength(LFields, LPacket.Count - 2);
  LCase := 0;
  for LIndex := 0 to LPacket.Count - 1 do
  begin

    if (LPacket.Key(LIndex) = 'resourceBrowser') or
      (LPacket.Key(LIndex) = 'resourcesScroll') then
    begin
      Continue;
    end;
    LFields[LCase] := NyxField(LPacket.Key(LIndex), LPacket.Field(LPacket.Key(LIndex)));

    if LPacket.Key(LIndex) = 'version' then
    begin
      LFields[LCase].Value := NyxData(10);
    end;
    Inc(LCase);
  end;
  LRejected := False;
  try
    DecodeNyxStudioPresentation(NyxObject(LFields).ToJSON);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'historical version ten is not silently widened to new drafts');
  LInvalid := ReadNyxResourceLabelsEditor(LTags);
  LInvalid.Selection := NyxResourceLabel('Foreign tag');
  LRejected := False;
  try
    RestoreNyxResourceLabelsEditor(LTags, LInvalid);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (ReadNyxResourceLabelsEditor(LTags).ToData.ToJSON = LBefore),
    'invalid selection refuses before any mounted proposal changes');
  HandleNyxResourceEditorLabels(LTags.Find(NyxResourceLabelsEditorActionID(LTags.ID, rleaRemove)),
    LForm.Node, LEditor);
  LState := ReadNyxResourceLabelsEditor(LTags);
  Check((LState.Labels.Count = 0) and not LState.Selection.Defined and
    (LState.Input = TNyxText('Unfinished' + TNyxText(#10) + 'tag')),
    'Remove retains independent unfinished tag input');
  Check(TrySelectNyxResourceEditor(LForm.Node, LResources, NyxNewResourceSelection) and
    (LForm.Node.Find(LTags.ID) = LTags) and
    (LForm.Node.Find(LInput.ID) = LInput) and
    (ReadNyxResourceLabelsEditor(LTags).Input = ''),
    'New clears the proposal without replacing tag controls');
  SetNyxResourceEditorLabels(LForm.Node, NyxResourceLabels.Add(NyxResourceLabel('Help')));
  LPreference.ResourceDraft.Capture(LForm.Node.ID, LForm.Node);
  LLoaded := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPreference));
  Check((LLoaded.ResourceDraft.ToData.Field('version').AsInteger = 3) and
    LLoaded.ResourceDraft.Restore(LFresh.Node) and
    NyxResourceEditorLabels(LFresh.Node).Contains(NyxResourceLabel('Help')),
    'version eleven also admits the previously refused labelled version-three draft');
end;

{ Qualify the demonstrated nonempty proposal loss. A different selected owner
  is an explicit handoff, not permission to reuse stale catalog/binding context. }
procedure OwnerSelectionDraftChecks;
var
  LResources: INyxResources;
  LFirst: INyxLabel;
  LSecond: INyxLabel;
  LChanged: INyxInput;
  LForm: INyxCard;
  LFresh: INyxCard;
  LRefused: INyxCard;
  LDraft: TNyxResourceEditorDraft;
  LOriginal: TNyxResourceEditorDraft;
  LBefore: TNyxText;
  LInput: TNyxText;
begin
  LResources := NewNyxResources;
  LResources.Define(NyxResourceRef('notes'), NyxTextResource('Accepted notes'));
  LResources.Define(NyxResourceRef('copy'), NyxJSONResource('{"prompt":"Accepted prompt"}'));
  LFirst := NewNyxLabel('first-owner');
  LSecond := NewNyxLabel('second-owner');
  LForm := NewNyxResourceEditor('owner-proposal', LResources, NyxNewResourceSelection,
    LFirst.Node, LFirst.Node);
  LInput := TNyxText('{"unfinished": 🌙');
  LForm.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refKind)).Configure.Value('JSON').Done;
  LForm.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refContent)).Configure.Value(LInput).Done;
  LForm.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refBind)).Configure.Value(True).Done;
  SetNyxResourceEditorLabels(LForm.Node, NyxResourceLabels.Add(NyxResourceLabel('Unsubmitted')));
  LForm.Node.Find(NyxResourceLabelsEditorFieldID(NyxResourceEditorLabelsID(LForm.Node.ID),
    rlefInput)).Configure.Value('Incomplete tag...').Done;
  LDraft.Capture(LForm.Node.ID, LForm.Node);
  LOriginal := LDraft;
  LBefore := LOriginal.ToData.ToJSON;
  LFresh := NewNyxResourceEditor(LForm.Node.ID, LResources, NyxNewResourceSelection,
    LSecond.Node, LSecond.Node);
  Check(not LDraft.Restore(LFresh.Node), 'ordinary strict restore still refuses a changed owner');
  Check(LDraft.RestoreForOwnerSelection(LFresh.Node) and
    (LFresh.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refContent)).Prop('value') = LInput) and
    (NyxResourceEditorLabels(LFresh.Node).Count = 1) and
    (ReadNyxResourceLabelsEditor(LFresh.Node.Find(NyxResourceEditorLabelsID(LForm.Node.ID))).Input =
      'Incomplete tag...'), 'explicit handoff preserves partial JSON, tags and unfinished input');
  Check((LFresh.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refBind)).Prop('value') = 'false') and
    (LForm.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refBind)).Prop('value') = 'true') and
    (LOriginal.ToData.ToJSON = LBefore), 'handoff clears proposed binding without aliasing the former form/draft');
  LChanged := NewNyxInput(LSecond.Node.ID);
  LRefused := NewNyxResourceEditor(LForm.Node.ID, LResources, NyxNewResourceSelection,
    LChanged.Node, LChanged.Node);
  Check(not LDraft.RestoreForOwnerSelection(LRefused.Node) and
    (LRefused.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refContent)).Prop('value') = ''),
    'changed contract on the same owner refuses before writing fields');
  LRefused := NewNyxResourceEditor(LForm.Node.ID, LResources,
    NyxResourceSelection(NyxResourceRef('notes'), NyxDefaultLocale), LFirst.Node, LFirst.Node);
  Check(not LDraft.RestoreForOwnerSelection(LRefused.Node) and
    (LRefused.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refContent)).Prop('value') = 'Accepted notes'),
    'a different resource selection cannot acquire the previous proposal');
  Check(TrySelectNyxResourceEditor(LFresh.Node, LResources,
    NyxResourceSelection(NyxResourceRef('copy'), NyxDefaultLocale), LSecond.Node, LSecond.Node) and
    (ReadNyxResourceEditor(LFresh.Node).Kind = nrkJSON) and
    (Pos(TNyxText('Root["prompt"] / text'),
      LFresh.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refPath)).Prop('items')) > 0),
    'explicit Open replaces the handed-off proposal and discovers accepted JSON paths');
  LResources.Define(NyxResourceRef('notes'), NyxTextResource('Changed accepted notes'));
  LRefused := NewNyxResourceEditor(LForm.Node.ID, LResources, NyxNewResourceSelection,
    LFirst.Node, LFirst.Node);
  Check(not LDraft.RestoreForOwnerSelection(LRefused.Node) and
    (LRefused.Node.Find(NyxResourceEditorFieldID(LForm.Node.ID, refContent)).Prop('value') = ''),
    'changed accepted catalog refuses before writing the candidate form');
end;

procedure Run;
var
  LLabels: TNyxResourceLabels;
  LDerived: TNyxResourceLabels;
  LDefinition: INyxResourceDefinition;
  LHosted: INyxResourceDefinition;
  LDocument: TNyxDocument;
  LReplay: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LCatalog: INyxResourceCatalog;
  LForm: INyxCard;
  LFresh: INyxCard;
  LDraft: TNyxResourceEditorDraft;
  LCopy: TNyxResourceEditorDraft;
  LSource: TNyxText;
  LWire: TNyxText;
  LRejected: Boolean;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LQuery: TNyxResourceCatalogQuery;
  LLong: TNyxText;
  LChangedSource: TNyxText;
  {$ifndef PAS2JS}
  LOutput: TFileStream;
  LBytes: TNyxBytes;
  {$endif}
begin
  LabelEditorChecks;
  OwnerSelectionDraftChecks;
  LLabels := NyxResourceLabels.Add(NyxResourceLabel('Help'))
    .Add(NyxResourceLabel('Docs, "quick" | 🌙'));
  LDerived := LLabels.Remove(NyxResourceLabel('Help')).Add(NyxResourceLabel('Data'));
  Check((LLabels.Count = 2) and LLabels.Contains(NyxResourceLabel('Help')) and
    not LLabels.Contains(NyxResourceLabel('Data')), 'derived label sets leave their baseline independent');
  Check((LDerived.Count = 2) and not LDerived.Contains(NyxResourceLabel('Help')) and
    (LDerived.Item(0).Name = TNyxText('Docs, "quick" | 🌙')), 'removal retains exact punctuation and supplementary Unicode');
  Check(LLabels.Add(NyxResourceLabel('Help')).Count = 2, 'adding an exact duplicate is idempotent');
  RejectLabels(NyxData('Help'), 'a label list requires an array');
  RejectLabels(NyxArray([NyxData(1)]), 'numeric labels are never coerced into text');
  RejectLabels(NyxArray([NyxData('')]), 'empty labels refuse');
  RejectLabels(NyxArray([NyxData('Help' + TNyxText(#10))]), 'control scalars in label names refuse');
  RejectLabels(NyxArray([NyxData(TNyxText(StringOfChar('a', 129)))]),
    'label names retain the portable 128-scalar bound');
  LRejected := False;
  try
    TNyxResourceLabels.FromData(NyxArray([NyxData('Help'), NyxData('Help')]));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'wire duplicates refuse instead of silently changing a saved set');
  SetLength(LItems, NyxMaximumResourceLabels + 1);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := NyxData('Label ' + TNyxText(IntToStr(LIndex)));
  end;
  LRejected := False;
  try
    TNyxResourceLabels.FromData(NyxArray(LItems));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'excessive label count refuses before definition mutation');
  LLong := '';
  for LIndex := 1 to 125 do
  begin
    LLong := LLong + TNyxText('🌙');
  end;
  SetLength(LItems, 20);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := NyxData(TNyxText(IntToStr(LIndex)) + ' ' + LLong);
  end;
  RejectLabels(NyxArray(LItems), 'aggregate UTF-8 label data refuses before definition mutation');
  LDefinition := NyxTextResource('Exact notes 🌙').WithLabels(LLabels).Describe('Notes', 'Reader help');
  Check((LDefinition.Text = TNyxText('Exact notes 🌙')) and (LDefinition.ByteCount = 16) and
    (NyxResourceLabelsOf(LDefinition).ToData.ToJSON = LLabels.ToData.ToJSON),
    'creator help copies retain labels without changing payload byte count');
  Check((LDefinition.ToData.Field('version').AsInteger = 3) and
    (NyxResourceFromData(LDefinition.ToData).ToData.ToJSON = LDefinition.ToData.ToJSON),
    'labelled embedded wire round-trips exact metadata and payload');
  Check(NyxResourceDiscovery(LDefinition).WithLabels(NyxResourceLabels).ToData.Field('version').AsInteger = 1,
    'clearing labels restores the original unlabelled wire shape');
  LHosted := NyxHostedResource(nrkText, NyxResourceURL('https://example.test/notes'))
    .Tagged(NyxResourceLabel('Hosted')).Fallback(LDefinition)
    .Cache(NyxResourceCache.Persistent.FreshFor(30)).Describe('Remote notes', 'Shared help');
  Check((LHosted.ToData.Field('version').AsInteger = 4) and
    (NyxResourceLabelsOf(LHosted).Item(0).Name = 'Hosted') and
    (NyxResourceLabelsOf(LHosted.FallbackDefinition).Count = 2),
    'hosted/cache/fallback copies retain independently owned labels');
  Check(NyxResourceFromData(LHosted.ToData).ToData.ToJSON = LHosted.ToData.ToJSON,
    'hosted labelled wire retains cache policy and labelled fallback');
  RejectDefinition('{"version":3,"kind":"text","content":"Notes","title":"","description":"","labels":[]}',
    'labelled wire requires a nonempty set');
  RejectDefinition('{"version":1,"kind":"text","content":"Notes","title":"","description":"","labels":["Help"]}',
    'old wire versions refuse silently added discovery fields');
  RejectDefinition('{"version":3,"kind":"text","content":"Notes","title":"","description":"","labels":["Help"],"unknown":true}',
    'labelled wire refuses unknown fields');
  Check(NyxResourceLabelsOf(nil).Count = 0, 'absent optional discovery exposes no labels');
  LDocument := TNyxDocument.Create;
  LReplay := nil;
  LWorkspace := nil;
  try
    LDocument.AddPage(NewNyxColumn('labels-home').Add(NewNyxLabel('welcome').WithText('Resource notes')));
    LDocument.Resources.Define(NyxResourceRef('notes'), LDefinition)
      .Define(NyxResourceRef('remote'), LHosted)
      .Define(NyxResourceRef('collision'), NyxTextResource('Other notes')
        .Tagged(NyxResourceLabel('prefix"Help')).Tagged(NyxResourceLabel('Help, Data')));
    LDocument.Resources.Define(NyxResourceRef('numbers'),
      NyxJSONResource('{"value":9007199254740993,"caption":"Dataset"}')
        .Tagged(NyxResourceLabel('Data')));
    LDocument.Resources.Define(NyxResourceRef('packed'),
      NyxBinaryResource(NyxDecodeBase64('AAH/')).Tagged(NyxResourceLabel('Data')));
    LWire := TNyxCodec.Encode(LDocument);
    LReplay := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LReplay) = LWire, 'document persistence retains exact discovery metadata');
    LReplay.Free;
    LReplay := nil;
    LCatalog := NewNyxResourceCatalog(NyxCollection('labels-review'), LDocument.Resources);
    LCatalog.Filter(NyxResourceCatalogQuery.Tagged(NyxResourceLabel('Help')));
    Check(LCatalog.View.Snapshot.Count = 1, 'exact tag filtering rejects delimiter/substring collisions');
    LCatalog.Filter(NyxResourceCatalogQuery.Labels(
      NyxResourceLabels.Add(NyxResourceLabel('Help')).Add(NyxResourceLabel('Hosted')), rlmAny));
    Check(LCatalog.View.Snapshot.Count = 2, 'any-label matching composes alternatives');
    LCatalog.Filter(NyxResourceCatalogQuery.Labels(
      NyxResourceLabels.Add(NyxResourceLabel('Help')).Add(NyxResourceLabel('Hosted')), rlmAll));
    Check(LCatalog.View.Snapshot.Count = 0, 'all-label matching requires each exact label');
    LCatalog.Filter(NyxResourceCatalogQuery.Search('QUICK'));
    Check(LCatalog.View.Snapshot.Count = 1, 'creator labels participate in metadata search');
    LCatalog.Filter(NyxResourceCatalogQuery.Tagged(NyxResourceLabel('Data')).Kinds([nrkJSON]));
    Check(LCatalog.View.Snapshot.Count = 1, 'exact tags compose with typed built-in categories');
    LCatalog.Filter(NyxResourceCatalogQuery.Tagged(NyxResourceLabel('help')));
    Check(LCatalog.View.Snapshot.Count = 0, 'exact tags preserve case independently of text-search folding');
    LQuery := NyxResourceCatalogQuery.Tagged(NyxResourceLabel('Help'));
    LCatalog.Filter(LQuery.Tagged(NyxResourceLabel('Hosted')));
    Check(LCatalog.View.Snapshot.Count = 0, 'a derived query adds an independent required label');
    LCatalog.Filter(LQuery);
    Check(LCatalog.View.Snapshot.Count = 1, 'the original query retains its independent label selector');
    LCatalog.Filter(NyxResourceCatalogQuery);
    LForm := NewNyxResourceEditor('labels-editor', LDocument.Resources,
      NyxResourceSelection(NyxResourceRef('notes'), NyxDefaultLocale));
    Check(NyxResourceLabelsOf(ReadNyxResourceEditor(LForm.Node)).ToData.ToJSON = LLabels.ToData.ToJSON,
      'existing editor proposals preserve accepted labels');
    ProposeNyxResourceEditor(LForm.Node, NyxTextResource('Replacement bytes'));
    Check((ReadNyxResourceEditor(LForm.Node).Text = 'Replacement bytes') and
      (NyxResourceLabelsOf(ReadNyxResourceEditor(LForm.Node)).ToData.ToJSON = LLabels.ToData.ToJSON),
      'content imports retain the existing creator-label proposal');
    SetNyxResourceEditorLabels(LForm.Node, LDerived);
    LDraft.Capture('labels-editor', LForm.Node);
    Check(LDraft.ToData.Field('version').AsInteger = 3, 'labelled proposal drafts have a strict new wire version');
    LCopy := TNyxResourceEditorDraft.FromData(LDraft.ToData);
    LFresh := NewNyxResourceEditor('labels-editor', LDocument.Resources,
      NyxResourceSelection(NyxResourceRef('notes'), NyxDefaultLocale));
    Check(LCopy.Restore(LFresh.Node) and
      (NyxResourceLabelsOf(ReadNyxResourceEditor(LFresh.Node)).ToData.ToJSON = LDerived.ToData.ToJSON),
      'copied draft restoration retains unsubmitted exact labels');
    Check(TrySelectNyxResourceEditor(LFresh.Node, LDocument.Resources, NyxNewResourceSelection) and
      (NyxResourceEditorLabels(LFresh.Node).Count = 0), 'New navigation clears the previous label proposal');
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.labels');
    Check(Pos(TNyxText('.Tagged(NyxResourceLabel('), LSource) > 0,
      'crafted source uses typed fluent label references');
    LReplay := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LReplay) = LWire, 'managed replay preserves labels, payloads and hosted policies');
    LReplay.Free;
    LReplay := nil;
    LWorkspace.Free;
    LWorkspace := nil;
    { Exercise hand-authored fluent sets as well as Studio's readable per-label
      emission. A base-returning help call needs an explicit discovery adapter. }
    LChangedSource := ReplaceText(LSource,
      '.Tagged(NyxResourceLabel(''Help''))',
      '.WithLabels(NyxResourceLabels.Add(NyxResourceLabel(''Help'')))');
    LReplay := TNyxSourceWorkspace.PrepareDraft(LChangedSource, LWorkspace);
    Check(TNyxCodec.Encode(LReplay) = LWire, 'hand-authored typed label sets replay the same exact resources');
    LReplay.Free;
    LReplay := nil;
    LWorkspace.Free;
    LWorkspace := nil;
    LChangedSource := ReplaceText(LSource,
      'NyxTextResource(''Other notes'')',
      'NyxResourceDiscovery(NyxTextResource(''Other notes'').Describe('''', ''''))');
    LReplay := TNyxSourceWorkspace.PrepareDraft(LChangedSource, LWorkspace);
    Check(TNyxCodec.Encode(LReplay) = LWire, 'explicit discovery adaptation retains a base-interface definition');
    LReplay.Free;
    LReplay := nil;
    LWorkspace.Free;
    LWorkspace := nil;
    LRejected := False;
    try
      LChangedSource := ReplaceText(LSource,
        'NyxTextResource(''Other notes'')', 'NyxTextResource(''Other notes'').Describe('''', '''')');
      LReplay := TNyxSourceWorkspace.PrepareDraft(LChangedSource, LWorkspace);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'managed replay refuses a label call unavailable on the base return type');
    PairedHistory(LDocument, LDerived);
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      LBytes := NyxEncodeUTF8(LSource);
      LOutput := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LOutput.WriteBuffer(LBytes[0], Length(LBytes));
      finally
        LOutput.Free;
      end;
    end;
    {$endif}
  finally
    LFresh := nil;
    LForm := nil;
    LCatalog := nil;
    LWorkspace.Free;
    LReplay.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' portable resource label checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-test-result', 'passed');
    document.body.setAttribute('data-label-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'failed');
      document.body.setAttribute('data-label-error', LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
