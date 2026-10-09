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


unit nyx.resources.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.types, nyx.text, nyx.data, nyx.resources, nyx.resource.sources,
  nyx.state, nyx.binding.types, nyx.model, nyx.controls;

type
  { Every behavioral choice belongs to a closed Pascal family. Text fields hold
    open application names, creator help or file contents, never executable code. }
  TNyxResourceEditorField = (refName, refLocale, refTitle, refDescription, refKind,
    refSource, refURL, refCache, refFresh, refStale, refMaximum, refServer,
    refContent, refEncoding, refFallback, refBind, refTarget, refPath, refImageLocale);
  TNyxResourceEditorEncoding = (reeUTF8, reeJSONString, reeBase64);
  { Image binding intent is independent of the variant currently being edited.
    Selected pins that exact variant, including default; Runtime follows the
    application locale. Existing unversioned drafts migrate to Selected. }
  TNyxResourceEditorImageLocale = (reilSelected, reilRuntime);
  TNyxResourceEditorAction = (reaNew, reaOpen, reaImport, reaPreview, reaApply, reaRemove);
  TNyxResourceEditorOperation = (reoDefine, reoRemove, reoRows, reoDetachRows);

  { Synchronous target synchronization of a borrowed mounted form. A receiver
    must keep the form/shape alive for this call; it owns no lasting lease.
    Ordinary renderer Sync suppresses synthetic input and preserves siblings. }
  TNyxResourceEditorSynchronize = procedure of object;

  { Empty Reference means a new file. Locale is always explicit; empty selects
    the ordinary default. No catalog/document/widget survives in this value. }
  TNyxResourceEditorSelection = record
    Reference: TNyxResourceRef;
    Locale: TNyxLocaleRef;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceEditorSelection; static;
  end;

  { Copied complete proposal, including the exact catalog/control context seen
    by the user. DefinitionData contains immutable wire values, avoiding managed
    interfaces in records on pas2js. An optional scalar/image binding shares
    Apply and Undo with its file; removing a file never silently removes consumers. }
  TNyxResourceEditorChange = record
    Operation: TNyxResourceEditorOperation;
    Selection: TNyxResourceEditorSelection;
    CatalogBaseline: TNyxText;
    DefinitionData: TNyxDataValue;
    Bind: Boolean;
    Owner: TNyxText;
    OwnerBaseline: TNyxText;
    Binding: TNyxBindingSpec;
    { Row authoring carries its independent collection baseline and immutable
      recipe. These members are absent from ordinary file/scalar proposals. }
    CollectionBaseline: TNyxText;
    CollectionName: TNyxText;
    RowsData: TNyxDataValue;
    ReplaceStatic: Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceEditorChange; static;
  end;

  { Unsubmitted values, including incomplete JSON/Base64/numbers, are copied
    exactly. A parked form retains input; changed catalog/selection/control
    context refuses restoration before any field is written. No mutable tree,
    provider or asynchronous callback is retained. }
  TNyxResourceEditorDraft = record
  private
    FEditor: TNyxText;
    FContext: TNyxText;
    FPaths: TNyxDataValue;
    FLabels: TNyxResourceLabels;
    FLabelInput: TNyxText;
    FLabelSelection: TNyxResourceLabelRef;
    FValues: array[TNyxResourceEditorField] of TNyxText;
    function GetDefined: Boolean;
  public
    procedure Capture(const AID: TNyxText; AShellRoot: TNyxNode);
    function Restore(AShellRoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceEditorDraft; static;
    procedure Clear;
    property Defined: Boolean read GetDefined;
  end;

{ Stable ordinary descendant identities for closed roles and actions. }
function NyxResourceEditorFieldID(const AID: TNyxText;
  AField: TNyxResourceEditorField): TNyxText;
function NyxResourceEditorActionID(const AID: TNyxText;
  AAction: TNyxResourceEditorAction): TNyxText;
function NyxNewResourceSelection: TNyxResourceEditorSelection;
function NyxResourceSelection(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): TNyxResourceEditorSelection;
{ Context includes local and effective binding contracts as well as the exact
  authored kind. Callers borrow both nodes for this synchronous comparison. }
function NyxResourceEditorOwnerBaseline(AOwner, AProjection: TNyxNode): TNyxText;
{ Qualify the mounted proposal against current borrowed inputs, including before
  a queued chrome rebuild. Late picker replies must pass this check as well as
  the host's project-generation and exact raw-draft guards. }
function NyxResourceEditorContextMatches(AEditor: TNyxNode;
  const ACatalog: INyxResources; AOwner, AProjection: TNyxNode): Boolean;
{ One public Nyx card owns its file list and ordinary fields/buttons/preview.
  Catalog and optional exact editable owner/projection are borrowed only during
  construction. The accepted catalog and control bindings are never changed. }
function NewNyxResourceEditor(const AID: TNyxText; const ACatalog: INyxResources;
  const ASelection: TNyxResourceEditorSelection;
  AOwner: TNyxNode = nil; AProjection: TNyxNode = nil): INyxCard;
{ Navigate New/Open without replacing a compatible compound form. Exact catalog
  and selected-owner context are required; False changes nothing and asks the
  caller to use normal staged composition. Invalid selections raise before
  mutation. Existing nodes, bindings, callbacks, layout and creator additions
  remain owned by the mounted form; only this editor's proposal state changes.
  Preparation owns detached copies. Publication exchanges already allocated
  storage; a synchronization exception restores the previous model and invokes
  synchronization again before propagating. Nil synchronizes the model only.
  This changes no document resources, source or Undo history. }
function TrySelectNyxResourceEditor(AEditor: TNyxNode;
  const ACatalog: INyxResources; const ASelection: TNyxResourceEditorSelection;
  AOwner: TNyxNode = nil; AProjection: TNyxNode = nil;
  ASynchronize: TNyxResourceEditorSynchronize = nil): Boolean;
{ Recognize only a current mounted action, including exact listed entry data.
  Returns False for another command. Forged/incomplete forms raise. }
function NyxResourceEditorAction(AButton, AShellRoot: TNyxNode;
  out AEditor: TNyxNode; out AAction: TNyxResourceEditorAction;
  out ASelection: TNyxResourceEditorSelection): Boolean;
{ Recognize a current ordinary form field. Controllers retain its raw proposal
  and update disclosure without remounting or admitting partial file contents. }
function NyxResourceEditorInput(ANode, AShellRoot: TNyxNode;
  out AEditor: TNyxNode): Boolean;
{ Own tag actions use the same retained proposal and ordinary controller path.
  Invalid names raise without accepting any resource/source/history change. }
function HandleNyxResourceEditorLabels(AButton, AShellRoot: TNyxNode;
  out AEditor: TNyxNode): Boolean;
{ Stable public child identity for the reusable tag editor. }
function NyxResourceEditorLabelsID(const AID: TNyxText): TNyxText;
{ Typed UI input boundary. Embedded content is UTF-8 text/JSON or canonical
  Base64 for images/binary. Hosted content is optional explicit same-kind fallback.
  Refusal preserves fields and accepted work; metadata is applied after admission. }
function ReadNyxResourceEditor(AEditor: TNyxNode): INyxResourceDefinition;
{ Typed creator-label proposal. It changes neither accepted catalog nor payload.
  Sets are normalized before writing; read/capture/restore preserve exact labels.
  The owned reusable tag editor supplies visible proposal editing. }
function NyxResourceEditorLabels(AEditor: TNyxNode): TNyxResourceLabels;
procedure SetNyxResourceEditorLabels(AEditor: TNyxNode; const ALabels: TNyxResourceLabels);
function NyxResourceEditorKind(AEditor: TNyxNode): TNyxResourceKind;
{ Closed input boundary; unknown choices refuse without changing a proposal.
  These helpers borrow the mounted form and own no target/controller lifetime. }
function ReadNyxResourceEditorImageLocale(AEditor: TNyxNode): TNyxResourceEditorImageLocale;
procedure SetNyxResourceEditorImageLocale(AEditor: TNyxNode;
  AValue: TNyxResourceEditorImageLocale);
{ Imported immutable content changes a proposal only. Import into a hosted form
  supplies fallback bytes and retains its URL/cache choices. No path is persisted. }
procedure ProposeNyxResourceEditor(AEditor: TNyxNode;
  const ADefinition: INyxResourceDefinition);
{ Refresh disclosure from current source/kind choices. Preview additionally
  validates content and discovers bounded structural scalar binding choices.
  It never accepts design/source/history or starts a network request. }
procedure RefreshNyxResourceEditor(AEditor: TNyxNode; APreview: Boolean);
{ Capture Apply/Remove as one copied semantic command. Invalid complete input
  raises; Remove uses the exact opened entry and ignores incomplete new fields. }
function CaptureNyxResourceEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxResourceEditorChange): Boolean;

implementation

uses nyx.bytes, nyx.images, nyx.binding, nyx.layout.policy,
  nyx.resources.rows, nyx.collections, nyx.collections.codec, nyx.resources.labels.editor;

const
  CEditor = 'nyx.resource-editor';
  CCatalog = 'nyx.resource-editor.catalog';
  CSelection = 'nyx.resource-editor.selection';
  COwner = 'nyx.resource-editor.owner';
  COwnerBaseline = 'nyx.resource-editor.owner-baseline';
  CPaths = 'nyx.resource-editor.paths';
  CEntry = 'nyx.resource-editor.entry';
  CLabelProposal = 'nyx.resource-editor.labels';
  CFields: array[TNyxResourceEditorField] of TNyxText = ('name', 'locale', 'title',
    'description', 'kind', 'source', 'url', 'cache', 'fresh', 'stale', 'maximum',
    'server', 'content', 'encoding', 'fallback', 'bind', 'target', 'path', 'image-locale');
  CEncodings: array[TNyxResourceEditorEncoding] of TNyxText =
    ('UTF-8 text', 'Escaped JSON string', 'Base64 bytes');
  CKinds: array[TNyxResourceKind] of TNyxText = ('Image', 'JSON', 'Text', 'Binary');
  CSources: array[TNyxResourceSourceKind] of TNyxText = ('Embedded file', 'Hosted URL');
  CCaches: array[TNyxResourceCacheMode] of TNyxText = ('Bypass', 'Memory', 'Persistent');
  CServers: array[TNyxResourceServerPolicy] of TNyxText =
    ('Respect server directives', 'Override in private Nyx cache');
  CImageLocales: array[TNyxResourceEditorImageLocale] of TNyxText =
    ('Use this variant', 'Follow application locale');
  CActions: array[TNyxResourceEditorAction] of TNyxText =
    ('new', 'open', 'import', 'preview', 'apply', 'remove');

function NyxResourceEditorFieldID(const AID: TNyxText;
  AField: TNyxResourceEditorField): TNyxText;
begin
  Result := AID + TNyxText('-') + CFields[AField];
end;

function EditorLocale(const AName: TNyxText): TNyxLocaleRef;
begin
  Result := NyxDefaultLocale;

  if AName <> '' then
  begin
    Result := NyxLocale(AName);
  end;
end;

function NyxResourceEditorActionID(const AID: TNyxText;
  AAction: TNyxResourceEditorAction): TNyxText;
begin
  Result := AID + TNyxText('-') + CActions[AAction];
end;

function NyxNewResourceSelection: TNyxResourceEditorSelection;
begin
  Result := Default(TNyxResourceEditorSelection);
  Result.Locale := NyxDefaultLocale;
end;

function NyxResourceSelection(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): TNyxResourceEditorSelection;
begin
  Result.Reference := NyxResourceRef(AReference.Name);
  Result.Locale := EditorLocale(ALocale.Name);
end;

function TNyxResourceEditorSelection.ToData: TNyxDataValue;
begin
  EditorLocale(Locale.Name);
  Result := NyxObject([NyxField('reference', NyxData(Reference.Name)),
    NyxField('locale', NyxData(Locale.Name))]);
end;

class function TNyxResourceEditorSelection.FromData(
  const AData: TNyxDataValue): TNyxResourceEditorSelection;
var
  LReference: TNyxText;
  LResult: TNyxResourceEditorSelection;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 2) then
  begin
    raise ENyxResource.Create('Resource selection requires reference and locale');
  end;
  LResult := NyxNewResourceSelection;
  LReference := AData.Field('reference').AsText;

  if LReference <> '' then
  begin
    LResult.Reference := NyxResourceRef(LReference);
  end;
  LResult.Locale := EditorLocale(AData.Field('locale').AsText);
  Result := LResult;
end;

function BindingsData(ANode: TNyxNode): TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LSpec: TNyxBindingSpec;
  LResource: TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LValues, ANode.BindingCount);
  for LIndex := 0 to ANode.BindingCount - 1 do
  begin
    LSpec := ANode.Bindings[LIndex];
    LResource := NyxNull;

    if not LSpec.Cleared and (LSpec.Source = bsResource) then
    begin
      LResource := LSpec.ResourceValue.ToData;
    end;

    if not LSpec.Cleared and (LSpec.Source = bsResourceImage) then
    begin
      LResource := LSpec.ResourceImage.ToData;
    end;
    LValues[LIndex] := NyxObject([
      NyxField('target', NyxData(Ord(LSpec.Target))),
      NyxField('state', NyxData(LSpec.StateName)),
      NyxField('kind', NyxData(Ord(LSpec.ValueKind))),
      NyxField('direction', NyxData(Ord(LSpec.Direction))),
      NyxField('cleared', NyxData(LSpec.Cleared)),
      NyxField('source', NyxData(Ord(LSpec.Source))), NyxField('resource', LResource)]);
  end;
  Result := NyxArray(LValues);
end;

function NyxResourceEditorOwnerBaseline(AOwner, AProjection: TNyxNode): TNyxText;
begin
  Result := '';

  if (AOwner = nil) and (AProjection = nil) then
  begin
    Exit;
  end;

  if (AOwner = nil) or (AProjection = nil) then
  begin
    raise ENyxResource.Create('Resource binding requires its exact owner and projection');
  end;
  Result := NyxObject([NyxField('owner', NyxData(AOwner.ID)),
    NyxField('kind', NyxData(AOwner.Kind)),
    NyxField('projection', NyxData(AProjection.ProjectionKind)),
    NyxField('local', BindingsData(AOwner)),
    NyxField('effective', BindingsData(AProjection))]).ToJSON;
end;

function Complete(AEditor: TNyxNode): Boolean;
var
  LField: TNyxResourceEditorField;
  LNode: TNyxNode;
  LKind: TNyxKind;
begin
  Result := False;

  if (AEditor = nil) or (AEditor.Kind <> NyxKindName(nkCard)) or
    (AEditor.Prop(CEditor) <> AEditor.ID) or (AEditor.Prop(CCatalog) = '') or
    (AEditor.Prop(CSelection) = '') then
  begin
    Exit;
  end;
  for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
  begin
    LKind := nkInput;

    if LField in [refKind, refSource, refCache, refServer, refEncoding, refTarget,
      refPath, refImageLocale] then
    begin
      LKind := nkSelect;
    end
    else if LField in [refDescription, refContent] then
    begin
      LKind := nkMemo;
    end
    else if LField in [refFallback, refBind] then
    begin
      LKind := nkCheckbox;
    end;
    LNode := AEditor.Find(NyxResourceEditorFieldID(AEditor.ID, LField));

    if (LNode = nil) or (LNode.Kind <> NyxKindName(LKind)) then
    begin
      Exit;
    end;
  end;
  Result := (AEditor.Find(AEditor.ID + TNyxText('-summary')) <> nil) and
    (AEditor.Find(AEditor.ID + TNyxText('-image-preview')) <> nil);
end;

function Context(AEditor: TNyxNode): TNyxText;
begin
  Result := NyxObject([NyxField('catalog', NyxData(AEditor.Prop(CCatalog))),
    NyxField('selection', NyxData(AEditor.Prop(CSelection))),
    NyxField('owner', NyxData(AEditor.Prop(COwnerBaseline)))]).ToJSON;
end;

function NyxResourceEditorContextMatches(AEditor: TNyxNode;
  const ACatalog: INyxResources; AOwner, AProjection: TNyxNode): Boolean;
begin
  Result := False;

  if not Complete(AEditor) or (ACatalog = nil) then
  begin
    Exit;
  end;
  Result := (AEditor.Prop(CCatalog) = ACatalog.ToData.ToJSON) and
    (AEditor.Prop(COwnerBaseline) = NyxResourceEditorOwnerBaseline(AOwner, AProjection));
end;

function Field(AEditor: TNyxNode; AField: TNyxResourceEditorField): TNyxNode;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxResource.Create('Resource editing requires the complete mounted form');
  end;
  Result := AEditor.Find(NyxResourceEditorFieldID(AEditor.ID, AField));
end;

function Choice(AEditor: TNyxNode; AField: TNyxResourceEditorField;
  const ANames: array of TNyxText): Integer;
var
  LText: TNyxText;
  LIndex: Integer;
begin
  LText := Field(AEditor, AField).Prop('value');
  for LIndex := 0 to High(ANames) do
  begin

    if LText = ANames[LIndex] then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxResource.Create('Choose a supported resource option: ' + CFields[AField]);
end;

function Checked(AEditor: TNyxNode; AField: TNyxResourceEditorField): Boolean;
var
  LValue: TNyxText;
begin
  LValue := Field(AEditor, AField).Prop('value');

  if (LValue <> 'true') and (LValue <> 'false') then
  begin
    raise ENyxResource.Create('Resource checkbox requires a Boolean choice');
  end;
  Result := LValue = 'true';
end;

function NyxResourceEditorKind(AEditor: TNyxNode): TNyxResourceKind;
begin
  Result := TNyxResourceKind(Choice(AEditor, refKind, CKinds));
end;

function ReadNyxResourceEditorImageLocale(AEditor: TNyxNode): TNyxResourceEditorImageLocale;
begin
  Result := TNyxResourceEditorImageLocale(Choice(AEditor, refImageLocale, CImageLocales));
end;

procedure SetNyxResourceEditorImageLocale(AEditor: TNyxNode;
  AValue: TNyxResourceEditorImageLocale);
begin

  if (Ord(AValue) < Ord(Low(TNyxResourceEditorImageLocale))) or
    (Ord(AValue) > Ord(High(TNyxResourceEditorImageLocale))) then
  begin
    raise ENyxResource.Create('Unsupported image binding locale choice');
  end;
  Field(AEditor, refImageLocale).Configure.Value(CImageLocales[AValue]).Done;
end;

function NyxResourceEditorLabelsID(const AID: TNyxText): TNyxText;
begin
  Result := AID + TNyxText('-tags');
end;

function NyxResourceEditorLabels(AEditor: TNyxNode): TNyxResourceLabels;
var
  LTags: TNyxNode;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxResource.Create('Resource labels require a complete editor');
  end;
  LTags := AEditor.Find(NyxResourceEditorLabelsID(AEditor.ID));

  if LTags <> nil then
  begin
    Exit(ReadNyxResourceLabelsEditor(LTags).Labels);
  end;
  Result := NyxResourceLabels;

  if AEditor.Props.IndexOfName(CLabelProposal) >= 0 then
  begin
    Result := TNyxResourceLabels.FromData(TNyxDataValue.ParseJSON(AEditor.Prop(CLabelProposal)));
  end;
end;

procedure SetNyxResourceEditorLabels(AEditor: TNyxNode; const ALabels: TNyxResourceLabels);
var
  LLabels: TNyxResourceLabels;
  LTags: TNyxNode;
  LState: TNyxResourceLabelsEditorState;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxResource.Create('Resource labels require a complete editor');
  end;
  LLabels := ALabels.Copy;
  LTags := AEditor.Find(NyxResourceEditorLabelsID(AEditor.ID));

  if LTags <> nil then
  begin
    LState := ReadNyxResourceLabelsEditor(LTags);
    LState.Labels := LLabels;

    if LState.Selection.Defined and not LLabels.Contains(LState.Selection) then
    begin
      LState.Selection := Default(TNyxResourceLabelRef);
    end;
    RestoreNyxResourceLabelsEditor(LTags, LState);
  end;
  AEditor.SetProp(CLabelProposal, LLabels.ToData.ToJSON);
end;

function ReadNyxResourceEditor(AEditor: TNyxNode): INyxResourceDefinition;
var
  LKind: TNyxResourceKind;
  LSource: TNyxResourceSourceKind;
  LContent: TNyxText;
  LEmbedded: INyxResourceDefinition;
  LDefinition: INyxResourceDefinition;
  LPolicy: TNyxResourceCachePolicy;
  LMode: TNyxResourceCacheMode;
  LEncoding: TNyxResourceEditorEncoding;
  LBytes: TNyxBytes;
  LValue: Integer;
  LLabels: TNyxResourceLabels;
begin
  LKind := NyxResourceEditorKind(AEditor);
  LSource := TNyxResourceSourceKind(Choice(AEditor, refSource, CSources));
  LContent := Field(AEditor, refContent).Prop('value');
  LEmbedded := nil;

  if (LSource = rskEmbedded) or
    Checked(AEditor, refFallback) then
  begin
    LEncoding := TNyxResourceEditorEncoding(Choice(AEditor, refEncoding, CEncodings));

    if (LKind = nrkImage) and (LEncoding = reeBase64) and
      (Copy(LContent, 1, 5) = 'data:') then
    begin
      LEmbedded := NyxImageResource(TNyxImageSource.FromWire(LContent));
    end
    else
    begin
      case LEncoding of
        reeUTF8: LBytes := NyxEncodeUTF8(LContent);
        reeJSONString: LBytes := NyxEncodeUTF8(TNyxDataValue.ParseJSON(LContent).AsText);
        reeBase64: LBytes := NyxDecodeBase64(LContent);
      end;
      LEmbedded := NyxResourceFromBytes(LKind, LBytes);
    end;
  end;
  LDefinition := LEmbedded;

  if LSource = rskHosted then
  begin
    LMode := TNyxResourceCacheMode(Choice(AEditor, refCache, CCaches));
    LPolicy := NyxResourceCache;
    case LMode of
      rcmBypass: LPolicy := LPolicy.Bypass;
      rcmMemory: LPolicy := LPolicy.Memory;
      rcmPersistent: LPolicy := LPolicy.Persistent;
    end;

    if not TryNyxStateInteger(Field(AEditor, refFresh).Prop('value'), LValue) then
    begin
      raise ENyxResource.Create('Freshness requires whole seconds');
    end;
    LPolicy := LPolicy.FreshFor(LValue);

    if not TryNyxStateInteger(Field(AEditor, refStale).Prop('value'), LValue) then
    begin
      raise ENyxResource.Create('Stale fallback requires whole seconds');
    end;
    LPolicy := LPolicy.StaleFor(LValue);

    if not TryNyxStateInteger(Field(AEditor, refMaximum).Prop('value'), LValue) then
    begin
      raise ENyxResource.Create('Payload limit requires whole bytes');
    end;
    LPolicy := LPolicy.MaximumBytes(LValue)
      .ServerPolicy(TNyxResourceServerPolicy(Choice(AEditor, refServer, CServers)));
    LDefinition := NyxHostedResource(LKind, NyxResourceURL(Field(AEditor, refURL).Prop('value')))
      .Cache(LPolicy);

    if LEmbedded <> nil then
    begin
      LDefinition := LDefinition.Fallback(LEmbedded);
    end;
  end;
  Result := LDefinition.Describe(Field(AEditor, refTitle).Prop('value'),
    Field(AEditor, refDescription).Prop('value'));
  LLabels := NyxResourceEditorLabels(AEditor);

  if LLabels.Count > 0 then
  begin
    Result := NyxResourceDiscovery(Result).WithLabels(LLabels);
  end;
end;

function ContentText(const ADefinition: INyxResourceDefinition): TNyxText;
begin
  Result := '';
  case ADefinition.Kind of
    nrkText: Result := ADefinition.Text;
    nrkJSON: Result := NyxDecodeUTF8(ADefinition.Bytes);
    nrkImage, nrkBinary: Result := NyxEncodeBase64(ADefinition.Bytes);
  end;
end;

procedure ProposeNyxResourceEditor(AEditor: TNyxNode;
  const ADefinition: INyxResourceDefinition);
var
  LDefinition: INyxResourceDefinition;
  LEncoding: TNyxResourceEditorEncoding;
  LText: TNyxText;
begin

  if (ADefinition = nil) or (ADefinition.Source.Kind <> rskEmbedded) then
  begin
    raise ENyxResource.Create('Import requires an admitted embedded file');
  end;
  LDefinition := NyxResourceFromData(ADefinition.ToData);

  if LDefinition.Kind <> NyxResourceEditorKind(AEditor) then
  begin
    raise ENyxResource.Create('Imported file does not match the selected resource kind');
  end;
  LEncoding := reeBase64;
  LText := ContentText(LDefinition);

  if LDefinition.Kind in [nrkText, nrkJSON] then
  begin
    LEncoding := reeUTF8;

    if Pos(TNyxText(#0), LText) > 0 then
    begin
      LEncoding := reeJSONString;
      LText := NyxData(LText).ToJSON;
    end;
  end;
  Field(AEditor, refEncoding).Configure.Value(CEncodings[LEncoding]).Done;
  Field(AEditor, refContent).Configure.Value(LText).Done;

  if Choice(AEditor, refSource, CSources) = Ord(rskHosted) then
  begin
    Field(AEditor, refFallback).Configure.Value(True).Done;
  end;
  RefreshNyxResourceEditor(AEditor, True);
end;

function ScalarPaths(const ADefinition: INyxResourceDefinition): TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LCount: Integer;
  LVisited: Integer;

  procedure Visit(const AValue: TNyxDataValue; const APath: TNyxResourcePath;
    const ALabel: TNyxText; ADepth: Integer);
  var
    LIndex: Integer;
    LKind: TNyxStateKind;
    LInteger: Integer;
    LName: TNyxText;
    LTitle: TNyxText;
  begin

    if (LCount >= 256) or (LVisited >= 2048) or (ADepth > 16) then
    begin
      Exit;
    end;
    Inc(LVisited);
    case AValue.Kind of
      ndObject:
        begin
          for LIndex := 0 to AValue.Count - 1 do
          begin

            if (LCount >= 256) or (LVisited >= 2048) then
            begin
              Break;
            end;
            LName := AValue.Key(LIndex);
            Visit(AValue.Field(LName), APath.Field(LName),
              ALabel + TNyxText('[') + NyxData(LName).ToJSON + TNyxText(']'), ADepth + 1);
          end;
          Exit;
        end;
      ndArray:
        begin
          for LIndex := 0 to AValue.Count - 1 do
          begin

            if (LCount >= 256) or (LVisited >= 2048) then
            begin
              Break;
            end;
            Visit(AValue.Item(LIndex), APath.Item(LIndex),
              ALabel + TNyxText('[') + TNyxText(IntToStr(LIndex)) + TNyxText(']'), ADepth + 1);
          end;
          Exit;
        end;
      ndText: LKind := nskText;
      ndBoolean: LKind := nskBoolean;
      ndNumber:
        begin
          LKind := nskNumber;

          if TryNyxStateInteger(AValue.AsDecimal.Text, LInteger) then
          begin
            LKind := nskInteger;
          end;
        end;
      ndNull: Exit;
    end;
    LTitle := ALabel + TNyxText(' / ') + NyxStateKindName(LKind);
    { A newline inside a literal JSON key stays escaped by ToJSON above. }
    SetLength(LValues, LCount + 1);
    LValues[LCount] := NyxObject([NyxField('title', NyxData(LTitle)),
      NyxField('path', APath.ToData), NyxField('kind', NyxData(Ord(LKind)))]);
    Inc(LCount);
  end;

begin
  LCount := 0;
  { Bound work even for large arrays containing only nulls or empty objects. }
  LVisited := 0;
  LValues := nil;

  if ADefinition.Kind = nrkText then
  begin
    Visit(NyxData(ADefinition.Text), NyxResourcePath, 'File text', 0);
  end
  else if ADefinition.Kind = nrkJSON then
  begin
    Visit(ADefinition.Data, NyxResourcePath, 'Root', 0);
  end;
  Result := NyxArray(LValues);
end;

procedure RefreshNyxResourceEditor(AEditor: TNyxNode; APreview: Boolean);
var
  LHosted: Boolean;
  LField: TNyxResourceEditorField;
  LDefinition: INyxResourceDefinition;
  LContent: INyxResourceDefinition;
  LPaths: TNyxDataValue;
  LItems: TNyxText;
  LValue: TNyxText;
  LIndex: Integer;
  LSelected: Boolean;
  LTags: TNyxNode;
  LLabelState: TNyxResourceLabelsEditorState;
begin
  LTags := AEditor.Find(NyxResourceEditorLabelsID(AEditor.ID));

  if LTags <> nil then
  begin
    LLabelState := ReadNyxResourceLabelsEditor(LTags);
    { Changing selection updates the action's disclosure without replacing the
      input's text/caret or any compound descendant. }
    LTags.Find(NyxResourceLabelsEditorActionID(LTags.ID, rleaRemove)).Configure
      .Enabled(LLabelState.Selection.Defined).Done;
  end;
  LHosted := Choice(AEditor, refSource, CSources) = Ord(rskHosted);
  for LField := refURL to refServer do
  begin
    Field(AEditor, LField).Configure.Visible(LHosted).Done;
  end;
  Field(AEditor, refFallback).Configure.Visible(LHosted).Done;
  Field(AEditor, refContent).Configure.Visible(not LHosted or
    Checked(AEditor, refFallback)).Done;
  Field(AEditor, refEncoding).Configure.Visible(not LHosted or
    Checked(AEditor, refFallback)).Done;
  Field(AEditor, refTarget).Configure.Visible(Checked(AEditor, refBind)).Done;
  { Image payloads have one specialized target and no scalar path. Choose it
    automatically when the current editable projection exposes that capability;
    image captions from JSON/text keep their ordinary scalar target choices. }

  if (NyxResourceEditorKind(AEditor) = nrkImage) and
    (Pos(NyxBindingPropertyTitle(bpImage), Field(AEditor, refTarget).Prop('items')) > 0) then
  begin
    Field(AEditor, refTarget).Configure.Value(NyxBindingPropertyTitle(bpImage)).Done;
  end;
  Field(AEditor, refPath).Configure.Visible(Checked(AEditor, refBind) and
    (NyxResourceEditorKind(AEditor) <> nrkImage)).Done;
  Field(AEditor, refImageLocale).Configure.Visible(Checked(AEditor, refBind) and
    (NyxResourceEditorKind(AEditor) = nrkImage) and
    (Field(AEditor, refTarget).Prop('value') = NyxBindingPropertyTitle(bpImage))).Done;

  if not APreview then
  begin
    Exit;
  end;
  LDefinition := ReadNyxResourceEditor(AEditor);
  LContent := LDefinition;

  if LHosted then
  begin
    LContent := LDefinition.FallbackDefinition;
  end;
  LPaths := NyxArray([]);
  AEditor.Find(AEditor.ID + TNyxText('-image-preview')).Configure.Source(NyxNoImage)
    .Visible(False).Done;
  LItems := '';

  if LContent <> nil then
  begin
    LPaths := ScalarPaths(LContent);

    if LContent.Kind = nrkImage then
    begin
      AEditor.Find(AEditor.ID + TNyxText('-image-preview')).Configure
        .Source(LContent.Image).Visible(True).Done;
    end;
    LItems := 'Admitted ' + CKinds[LContent.Kind] + TNyxText(' / ') +
      TNyxText(IntToStr(LContent.ByteCount)) + TNyxText(' bytes');
  end;

  if LHosted then
  begin
    LItems := 'Hosted declaration / ' + LItems +
      TNyxText(' / fallback preview; network loading has not run');
  end;
  AEditor.Find(AEditor.ID + TNyxText('-summary')).Configure.Text(LItems).Done;
  AEditor.SetProp(CPaths, LPaths.ToJSON);
  LItems := '';
  LValue := Field(AEditor, refPath).Prop('value');
  LSelected := False;
  for LIndex := 0 to LPaths.Count - 1 do
  begin

    if LItems <> '' then
    begin
      LItems := LItems + TNyxText(#10);
    end;
    LItems := LItems + LPaths.Item(LIndex).Field('title').AsText;
    LSelected := LSelected or (LPaths.Item(LIndex).Field('title').AsText = LValue);
  end;
  Field(AEditor, refPath).Configure.Items(LItems).Done;

  if not LSelected then
  begin
    LValue := '';

    if LPaths.Count > 0 then
    begin
      LValue := LPaths.Item(0).Field('title').AsText;
    end;
    Field(AEditor, refPath).Configure.Value(LValue).Done;
  end;
end;

function NewNyxResourceEditor(const AID: TNyxText; const ACatalog: INyxResources;
  const ASelection: TNyxResourceEditorSelection;
  AOwner: TNyxNode; AProjection: TNyxNode): INyxCard;
const
  CLabels: array[TNyxResourceEditorField] of TNyxText = ('Resource name',
    'Locale (empty for default)', 'Title', 'Description and intent', 'File kind',
    'Source', 'Public HTTP(S) URL', 'Private cache', 'Fresh for (seconds)',
    'Stale fallback window (seconds)', 'Maximum payload (bytes)', 'Server directives',
    'File contents / hosted fallback', 'Content notation', 'Include embedded fallback',
    'Bind to selected control on Apply', 'Control property', 'Data value', 'Image binding locale');
var
  LField: TNyxResourceEditorField;
  LAction: TNyxResourceEditorAction;
  LInput: INyxControl;
  LButton: INyxButton;
  LDefinition: INyxResourceDefinition;
  LContent: INyxResourceDefinition;
  LPolicy: TNyxResourceCachePolicy;
  LTarget: TNyxBindingProperty;
  LItems: TNyxText;
  LIndex: Integer;
  LKind: TNyxResourceKind;
  LSelection: TNyxResourceEditorSelection;
  LCapturedSelection: TNyxResourceEditorSelection;
  LBinding: TNyxBindingSpec;
  LTags: INyxCard;

  procedure MarkTags(ANode: TNyxNode);
  var
    LChild: Integer;
  begin
    ANode.SetProp(CEditor, AID);
    for LChild := 0 to ANode.Count - 1 do
    begin
      MarkTags(ANode.Children[LChild]);
    end;
  end;
begin

  if ACatalog = nil then
  begin
    raise ENyxResource.Create('Resource authoring requires its catalog');
  end;
  LSelection := TNyxResourceEditorSelection.FromData(ASelection.ToData);

  if LSelection.Reference.Defined and
    not ACatalog.Contains(LSelection.Reference, LSelection.Locale) then
  begin
    LSelection := NyxNewResourceSelection;
  end;
  LCapturedSelection := TNyxResourceEditorSelection.FromData(LSelection.ToData);
  LDefinition := NyxTextResource('');

  if LSelection.Reference.Defined then
  begin
    LDefinition := ACatalog.Definition(LSelection.Reference, LSelection.Locale);
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(TNyxLayoutPolicy.Column).Gap(10).Done;
  Result.Node.SetProp(CEditor, AID).SetProp(CCatalog, ACatalog.ToData.ToJSON)
    .SetProp(CSelection, LSelection.ToData.ToJSON)
    .SetProp(CLabelProposal, NyxResourceLabelsOf(LDefinition).ToData.ToJSON)
    .SetProp(COwnerBaseline, NyxResourceEditorOwnerBaseline(AOwner, AProjection));

  if AOwner <> nil then
  begin
    Result.Node.SetProp(COwner, AOwner.ID);
  end;
  Result.Add(NewNyxHeading(AID + TNyxText('-heading')).WithText('Resources'));
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).WithText(
    'Keep images, JSON, text and data with your project, or declare a hosted file.'));
  for LIndex := 0 to ACatalog.Count - 1 do
  begin
    LSelection := NyxResourceSelection(ACatalog.Reference(LIndex), ACatalog.Locale(LIndex));
    LItems := LSelection.Reference.Name;

    if LSelection.Locale.Name <> '' then
    begin
      LItems := LItems + TNyxText(' / ') + LSelection.Locale.Name;
    end;
    LButton := NewNyxButton(AID + TNyxText('-entry-') + TNyxText(IntToStr(LIndex)))
      .WithText(LItems);
    LButton.Configure.Hint(ACatalog.Definition(LSelection.Reference,
      LSelection.Locale).Description).Done;
    LButton.Node.SetProp(CEditor, AID).SetProp(CEntry, LSelection.ToData.ToJSON);
    Result.Add(LButton);
  end;
  LButton := NewNyxButton(NyxResourceEditorActionID(AID, reaNew)).WithText('+ New resource');
  LButton.Node.SetProp(CEditor, AID);
  Result.Add(LButton);
  LContent := LDefinition;

  if LDefinition.Source.Kind = rskHosted then
  begin
    LContent := LDefinition.FallbackDefinition;
  end;
  LPolicy := LDefinition.Source.CachePolicy;

  if LDefinition.Source.Kind = rskEmbedded then
  begin
    LPolicy := NyxResourceCache;
  end;
  for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
  begin
    LItems := '';
    case LField of
      refKind:
        begin
          for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
          begin

            if LItems <> '' then
            begin
              LItems := LItems + TNyxText(#10);
            end;
            LItems := LItems + CKinds[LKind];
          end;
        end;
      refSource: LItems := CSources[rskEmbedded] + TNyxText(#10) + CSources[rskHosted];
      refCache: LItems := CCaches[rcmBypass] + TNyxText(#10) + CCaches[rcmMemory] +
        TNyxText(#10) + CCaches[rcmPersistent];
      refServer: LItems := CServers[rcspRespect] + TNyxText(#10) + CServers[rcspOverride];
      refEncoding: LItems := CEncodings[reeUTF8] + TNyxText(#10) + CEncodings[reeJSONString] +
        TNyxText(#10) + CEncodings[reeBase64];
      refImageLocale: LItems := CImageLocales[reilSelected] + TNyxText(#10) +
        CImageLocales[reilRuntime];
      refTarget:
        begin

          if AProjection <> nil then
          begin
            for LTarget := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
            begin

              if (NyxBindingKinds(AProjection, LTarget) <> []) or
                ((LTarget = bpImage) and NyxSupportsResourceImage(AProjection)) then
              begin

                if LItems <> '' then
                begin
                  LItems := LItems + TNyxText(#10);
                end;
                LItems := LItems + NyxBindingPropertyTitle(LTarget);
              end;
            end;
          end;
        end;
      else
      begin
        { Plain text, checkbox and path fields have no static choice list. }
      end;
    end;

    if LField in [refKind, refSource, refCache, refServer, refEncoding, refTarget,
      refPath, refImageLocale] then
    begin
      LInput := NewNyxSelect(NyxResourceEditorFieldID(AID, LField));
      LInput.Configure.Items(LItems).Done;

      if (LField = refTarget) and (LItems <> '') then
      begin
        LInput.Configure.Value(Copy(LItems, 1, Pos(#10, LItems + TNyxText(#10)) - 1)).Done;
      end;
    end
    else if LField in [refDescription, refContent] then
    begin
      LInput := NewNyxMemo(NyxResourceEditorFieldID(AID, LField));
      LInput.Configure.Height(112).Done;
    end
    else if LField in [refFallback, refBind] then
    begin
      LInput := NewNyxCheckbox(NyxResourceEditorFieldID(AID, LField));
      LInput.Configure.Value(False).Done;
    end
    else
    begin
      LInput := NewNyxInput(NyxResourceEditorFieldID(AID, LField));
    end;
    LInput.Configure.Text(CLabels[LField]).AccessibleName(CLabels[LField]).Done;
    LInput.Node.SetProp(CEditor, AID);
    Result.Add(LInput);
  end;
  LTags := NewNyxResourceLabelsEditor(NyxResourceEditorLabelsID(AID),
    NyxResourceLabelsOf(LDefinition));
  MarkTags(LTags.Node);
  Result.Add(LTags);
  Result.Add(NewNyxLabel(AID + TNyxText('-summary')));
  Result.Add(NewNyxImage(AID + TNyxText('-image-preview')).Configure
    .Height(120).ImageFit(nifContain).AlternativeText('Resource proposal preview').Done);
  Field(Result.Node, refName).Configure.Value(LCapturedSelection.Reference.Name).Done;
  Field(Result.Node, refLocale).Configure.Value(LCapturedSelection.Locale.Name).Done;
  Field(Result.Node, refName).Configure.ReadOnly(LCapturedSelection.Reference.Defined).Done;
  Field(Result.Node, refLocale).Configure.ReadOnly(LCapturedSelection.Reference.Defined).Done;
  Field(Result.Node, refTitle).Configure.Value(LDefinition.Title).Done;
  Field(Result.Node, refDescription).Configure.Value(LDefinition.Description).Done;
  Field(Result.Node, refKind).Configure.Value(CKinds[LDefinition.Kind]).Done;
  Field(Result.Node, refSource).Configure.Value(CSources[LDefinition.Source.Kind]).Done;
  Field(Result.Node, refURL).Configure.Value(LDefinition.Source.URL.Address).Done;
  Field(Result.Node, refCache).Configure.Value(CCaches[LPolicy.Mode]).Done;
  Field(Result.Node, refFresh).Configure.Value(IntToStr(LPolicy.FreshSeconds)).Done;
  Field(Result.Node, refStale).Configure.Value(IntToStr(LPolicy.StaleSeconds)).Done;
  Field(Result.Node, refMaximum).Configure.Value(IntToStr(LPolicy.ByteLimit)).Done;
  Field(Result.Node, refServer).Configure.Value(CServers[LPolicy.Server]).Done;
  Field(Result.Node, refBind).Configure.Enabled(AProjection <> nil).Done;
  Field(Result.Node, refEncoding).Configure.Value(CEncodings[reeUTF8]).Done;
  SetNyxResourceEditorImageLocale(Result.Node, reilSelected);
  { Opening an existing runtime-locale binding must not silently pin it on the
    next Apply. Other resources and older drafts keep the original pinned default. }

  if (AProjection <> nil) and AProjection.FindBinding(bpImage, LBinding) and
    (LBinding.Source = bsResourceImage) and
    (LBinding.ResourceImage.Reference.Name = LCapturedSelection.Reference.Name) and
    not LBinding.ResourceImage.Localized then
  begin
    SetNyxResourceEditorImageLocale(Result.Node, reilRuntime);
  end;
  Field(Result.Node, refImageLocale).Configure.Hint(
    'Use this variant pins the edited locale, including the default. ' +
    TNyxText('Follow application locale resolves each runtime independently.')).Done;

  if LContent <> nil then
  begin
    ProposeNyxResourceEditor(Result.Node, LContent);
  end;
  Field(Result.Node, refContent).Configure.Hint(
    'Text and JSON use UTF-8 contents. Images and binary use Base64. Preview discovers data values.').Done;
  Field(Result.Node, refServer).Configure.Hint(
    'Override deliberately permits private Nyx storage despite server no-store.').Done;
  for LAction := reaImport to reaRemove do
  begin
    LItems := 'Import file';
    case LAction of
      reaPreview: LItems := 'Preview and discover values';
      reaApply: LItems := 'Apply resource';
      reaRemove: LItems := 'Remove this variant';
      reaNew, reaOpen, reaImport:
        begin
          { Import keeps its default caption; New/Open are composed above. }
        end;
    end;
    LButton := NewNyxButton(NyxResourceEditorActionID(AID, LAction)).WithText(LItems);
    LButton.Node.SetProp(CEditor, AID);

    if LAction = reaRemove then
    begin
      LButton.Configure.Enabled(LCapturedSelection.Reference.Defined)
        .Hint('Removal is refused while a control still needs this file. Undo restores an accepted removal.').Done;
    end;
    Result.Add(LButton);
  end;
  RefreshNyxResourceEditor(Result.Node, True);
end;

function TrySelectNyxResourceEditor(AEditor: TNyxNode;
  const ACatalog: INyxResources; const ASelection: TNyxResourceEditorSelection;
  AOwner: TNyxNode; AProjection: TNyxNode;
  ASynchronize: TNyxResourceEditorSynchronize): Boolean;
const
  CStateAttributes: array[0..4] of TNyxAttribute =
    (atValue, atItems, atReadOnly, atEnabled, atVisible);
var
  LSelection: TNyxResourceEditorSelection;
  LFresh: INyxCard;
  LPrepared: TNyxNode;
  LField: TNyxResourceEditorField;
  LAttributeIndex: Integer;

  procedure CopyAttribute(AFrom, ATo: TNyxNode; AKey: TNyxAttribute);
  begin

    if AFrom.Props.IndexOfName(NyxAttributeName(AKey)) < 0 then
    begin
      ATo.Configure.Clear(AKey).Done;
    end
    else
    begin
      ATo.SetProp(NyxAttributeName(AKey), AFrom.StoredProp(NyxAttributeName(AKey)));
    end;
  end;

  procedure ExchangeProperties(AExisting, ACandidate: TNyxNode);
  var
    LChild: Integer;
  begin
    { Candidate is an independent clone of this exact live shape, including
      creator descendants. Swap owned strings, never nodes or callback owners;
      repeating the same exchange rolls back without allocation/conversion. }
    AExisting.Props.ExchangeStorage(ACandidate.Props);
    for LChild := 0 to AExisting.Count - 1 do
    begin
      ExchangeProperties(AExisting.Children[LChild], ACandidate.Children[LChild]);
    end;
  end;
begin
  Result := False;

  if not NyxResourceEditorContextMatches(AEditor, ACatalog, AOwner, AProjection) then
  begin
    Exit;
  end;

  if AEditor.Find(NyxResourceEditorActionID(AEditor.ID, reaRemove)) = nil then
  begin
    Exit;
  end;
  LSelection := TNyxResourceEditorSelection.FromData(ASelection.ToData);

  if LSelection.Reference.Defined and
    not ACatalog.Contains(LSelection.Reference, LSelection.Locale) then
  begin
    Exit;
  end;
  LFresh := NewNyxResourceEditor(AEditor.ID, ACatalog, LSelection, AOwner, AProjection);
  LPrepared := AEditor.Clone;
  try
    { Copy only owned proposal/disclosure fields. Fresh constructor defaults
      must not overwrite a caller's styling, labels, hints or added controls. }
    LPrepared.SetProp(CSelection, LFresh.Node.Prop(CSelection))
      .SetProp(CLabelProposal, LFresh.Node.Prop(CLabelProposal))
      .SetProp(CPaths, LFresh.Node.Prop(CPaths));

    if LPrepared.Find(NyxResourceEditorLabelsID(AEditor.ID)) <> nil then
    begin
      RestoreNyxResourceLabelsEditor(LPrepared.Find(NyxResourceEditorLabelsID(AEditor.ID)),
        ReadNyxResourceLabelsEditor(LFresh.Node.Find(NyxResourceEditorLabelsID(AEditor.ID))));
    end;
    for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
    begin
      for LAttributeIndex := Low(CStateAttributes) to High(CStateAttributes) do
      begin
        CopyAttribute(Field(LFresh.Node, LField), Field(LPrepared, LField),
          CStateAttributes[LAttributeIndex]);
      end;
    end;
    CopyAttribute(LFresh.Node.Find(AEditor.ID + TNyxText('-summary')),
      LPrepared.Find(AEditor.ID + TNyxText('-summary')), atText);
    CopyAttribute(LFresh.Node.Find(AEditor.ID + TNyxText('-image-preview')),
      LPrepared.Find(AEditor.ID + TNyxText('-image-preview')), atSource);
    CopyAttribute(LFresh.Node.Find(AEditor.ID + TNyxText('-image-preview')),
      LPrepared.Find(AEditor.ID + TNyxText('-image-preview')), atVisible);
    CopyAttribute(LFresh.Node.Find(NyxResourceEditorActionID(AEditor.ID, reaRemove)),
      LPrepared.Find(NyxResourceEditorActionID(AEditor.ID, reaRemove)), atEnabled);
    ExchangeProperties(AEditor, LPrepared);
    try

      if Assigned(ASynchronize) then
      begin
        ASynchronize;
      end;
    except
      ExchangeProperties(AEditor, LPrepared);

      if Assigned(ASynchronize) then
      begin
        ASynchronize;
      end;
      raise;
    end;
    Result := True;
  finally
    LPrepared.Free;
    LFresh := nil;
  end;
end;

function NyxResourceEditorAction(AButton, AShellRoot: TNyxNode;
  out AEditor: TNyxNode; out AAction: TNyxResourceEditorAction;
  out ASelection: TNyxResourceEditorSelection): Boolean;
var
  LAction: TNyxResourceEditorAction;
begin
  Result := False;
  AEditor := nil;
  AAction := reaNew;
  ASelection := NyxNewResourceSelection;

  if (AButton = nil) or (AShellRoot = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  AEditor := AShellRoot.Find(AButton.Prop(CEditor));

  if not Complete(AEditor) or (AButton.Kind <> NyxKindName(nkButton)) or
    (AEditor.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxResource.Create('Resource action requires its exact mounted form');
  end;

  if AButton.Prop(CEntry) <> '' then
  begin
    ASelection := TNyxResourceEditorSelection.FromData(
      TNyxDataValue.ParseJSON(AButton.Prop(CEntry)));
    AAction := reaOpen;
    Exit(True);
  end;
  for LAction := Low(TNyxResourceEditorAction) to High(TNyxResourceEditorAction) do
  begin

    if AButton.ID = NyxResourceEditorActionID(AEditor.ID, LAction) then
    begin
      AAction := LAction;
      { New starts an independent proposal even while an existing variant is
        open. Retaining that selection keeps its name locked and can turn an
        intended import into a replacement of the old file. Other commands
        continue to use the exact currently opened variant. }

      if LAction <> reaNew then
      begin
        ASelection := TNyxResourceEditorSelection.FromData(
          TNyxDataValue.ParseJSON(AEditor.Prop(CSelection)));
      end;
      Exit(True);
    end;
  end;
end;

function NyxResourceEditorInput(ANode, AShellRoot: TNyxNode;
  out AEditor: TNyxNode): Boolean;
var
  LField: TNyxResourceEditorField;
begin
  Result := False;
  AEditor := nil;

  if (ANode = nil) or (AShellRoot = nil) or (ANode.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  AEditor := AShellRoot.Find(ANode.Prop(CEditor));

  if not Complete(AEditor) or (AEditor.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
  begin

    if ANode.ID = NyxResourceEditorFieldID(AEditor.ID, LField) then
    begin
      Exit(True);
    end;
  end;
  Result := NyxResourceLabelsEditorInput(ANode,
    AEditor.Find(NyxResourceEditorLabelsID(AEditor.ID)));
end;

function HandleNyxResourceEditorLabels(AButton, AShellRoot: TNyxNode;
  out AEditor: TNyxNode): Boolean;
begin
  Result := False;
  AEditor := nil;

  if (AButton = nil) or (AShellRoot = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  AEditor := AShellRoot.Find(AButton.Prop(CEditor));

  if not Complete(AEditor) or (AEditor.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;
  Result := HandleNyxResourceLabelsEditorAction(AButton,
    AEditor.Find(NyxResourceEditorLabelsID(AEditor.ID)));

  if Result then
  begin
    AEditor.SetProp(CLabelProposal, NyxResourceEditorLabels(AEditor).ToData.ToJSON);
  end;
end;

function CaptureNyxResourceEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxResourceEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LAction: TNyxResourceEditorAction;
  LSelection: TNyxResourceEditorSelection;
  LDefinition: INyxResourceDefinition;
  LPaths: TNyxDataValue;
  LPath: TNyxResourcePath;
  LValue: TNyxResourceValueRef;
  LImage: TNyxResourceImageRef;
  LChoice: TNyxText;
  LTarget: TNyxBindingProperty;
  LIndex: Integer;
  LFound: Boolean;
begin
  AChange := Default(TNyxResourceEditorChange);
  Result := NyxResourceEditorAction(AButton, AShellRoot, LEditor, LAction, LSelection) and
    (LAction in [reaApply, reaRemove]);

  if not Result then
  begin
    Exit;
  end;
  AChange.CatalogBaseline := LEditor.Prop(CCatalog);
  AChange.Selection := LSelection;
  AChange.Operation := reoRemove;
  AChange.DefinitionData := NyxNull;

  if LAction = reaRemove then
  begin

    if not LSelection.Reference.Defined then
    begin
      raise ENyxResource.Create('Open a resource variant before removing it');
    end;
    Exit;
  end;
  AChange.Operation := reoDefine;
  AChange.Selection := NyxResourceSelection(
    NyxResourceRef(Field(LEditor, refName).Prop('value')),
    EditorLocale(Field(LEditor, refLocale).Prop('value')));

  if LSelection.Reference.Defined and
    ((LSelection.Reference.Name <> AChange.Selection.Reference.Name) or
    (LSelection.Locale.Name <> AChange.Selection.Locale.Name)) then
  begin
    raise ENyxResource.Create('Use New resource to create a different name or locale variant');
  end;
  LDefinition := ReadNyxResourceEditor(LEditor);
  AChange.DefinitionData := LDefinition.ToData;
  AChange.Bind := Checked(LEditor, refBind);

  if not AChange.Bind then
  begin
    Exit;
  end;
  AChange.Owner := LEditor.Prop(COwner);
  AChange.OwnerBaseline := LEditor.Prop(COwnerBaseline);
  LChoice := Field(LEditor, refTarget).Prop('value');
  LFound := False;
  for LTarget := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
  begin

    if LChoice = NyxBindingPropertyTitle(LTarget) then
    begin
      AChange.Binding := TNyxBindingSpec.Clear(LTarget);
      LFound := True;
      Break;
    end;
  end;

  if not LFound or (AChange.Owner = '') or (AChange.OwnerBaseline = '') then
  begin
    raise ENyxResource.Create('Choose a current selected control property');
  end;

  if AChange.Binding.Target = bpImage then
  begin

    if LDefinition.Kind <> nrkImage then
    begin
      raise ENyxResource.Create('The selected image property requires an image resource');
    end;
    LImage := NyxResourceImage(AChange.Selection.Reference);

    if ReadNyxResourceEditorImageLocale(LEditor) = reilSelected then
    begin
      LImage := LImage.Localize(AChange.Selection.Locale, NyxDefaultLocale);
    end;
    AChange.Binding := TNyxBindingSpec.Image(LImage);
    Exit;
  end;
  LPaths := ScalarPaths(LDefinition);
  LChoice := Field(LEditor, refPath).Prop('value');
  LFound := False;
  for LIndex := 0 to LPaths.Count - 1 do
  begin

    if LChoice = LPaths.Item(LIndex).Field('title').AsText then
    begin
      LPath := TNyxResourcePath.FromData(LPaths.Item(LIndex).Field('path'));
      LValue := NyxResourceValue(AChange.Selection.Reference);
      { Copy the structural path through its admitted descriptor, rather than
        interpreting the human label as a dotted path or executable expression. }
      LValue := TNyxResourceValueRef.FromData(NyxObject([
        NyxField('resource', NyxData(AChange.Selection.Reference.Name)),
        NyxField('path', LPath.ToData), NyxField('locale', NyxData(AChange.Selection.Locale.Name)),
        NyxField('fallback', NyxData('')), NyxField('type', NyxData(NyxStateKindName(
          TNyxStateKind(LPaths.Item(LIndex).Field('kind').AsInteger))))]));
      AChange.Binding := TNyxBindingSpec.Resource(AChange.Binding.Target, LValue);
      AChange.Binding.Validate;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxResource.Create('Preview the file and choose an existing scalar data value');
  end;
end;

function TNyxResourceEditorChange.ToData: TNyxDataValue;
var
  LBinding: TNyxDataValue;
begin

  if Operation in [reoRows, reoDetachRows] then
  begin
    Exit(NyxObject([NyxField('version', NyxData(2)),
      NyxField('operation', NyxData(Ord(Operation))), NyxField('catalog', NyxData(CatalogBaseline)),
      NyxField('collections', NyxData(CollectionBaseline)),
      NyxField('collection', NyxData(CollectionName)), NyxField('rows', RowsData),
      NyxField('replaceStatic', NyxData(ReplaceStatic))]));
  end;
  LBinding := NyxNull;

  if Bind then
  begin
    Binding.Validate;

    if Binding.Cleared or not (Binding.Source in [bsResource, bsResourceImage]) then
    begin
      raise ENyxResource.Create('Resource Apply requires a typed resource binding');
    end;

    if Binding.Source = bsResourceImage then
    begin
      LBinding := NyxObject([NyxField('target', NyxData(Ord(bpImage))),
        NyxField('value', Binding.ResourceImage.ToData)]);
    end
    else
    begin
      LBinding := NyxObject([NyxField('target', NyxData(Ord(Binding.Target))),
        NyxField('value', Binding.ResourceValue.ToData)]);
    end;
  end;
  Result := NyxObject([NyxField('operation', NyxData(Ord(Operation))),
    NyxField('selection', Selection.ToData), NyxField('catalog', NyxData(CatalogBaseline)),
    NyxField('definition', DefinitionData), NyxField('bind', NyxData(Bind)),
    NyxField('owner', NyxData(Owner)), NyxField('ownerBaseline', NyxData(OwnerBaseline)),
    NyxField('binding', LBinding)]);
end;

class function TNyxResourceEditorChange.FromData(
  const AData: TNyxDataValue): TNyxResourceEditorChange;
var
  LResult: TNyxResourceEditorChange;
  LOperation: Integer;
  LTarget: Integer;
  LBinding: TNyxDataValue;
  LIndex: Integer;
begin

  if (AData.Kind = ndObject) and (AData.Count = 7) then
  begin
    for LIndex := 0 to AData.Count - 1 do
    begin

      if Pos('|' + AData.Key(LIndex) + '|',
        '|version|operation|catalog|collections|collection|rows|replaceStatic|') = 0 then
      begin
        raise ENyxResource.Create('Unknown saved-row proposal member');
      end;
    end;
    LOperation := AData.Field('operation').AsInteger;

    if (AData.Field('version').AsInteger <> 2) or
      not (LOperation in [Ord(reoRows), Ord(reoDetachRows)]) then
    begin
      raise ENyxResource.Create('Unknown saved-row proposal operation/version');
    end;
    Result := Default(TNyxResourceEditorChange);
    Result.Operation := TNyxResourceEditorOperation(LOperation);
    Result.CatalogBaseline := AData.Field('catalog').AsText;
    Result.CollectionBaseline := AData.Field('collections').AsText;
    Result.CollectionName := NyxCollection(AData.Field('collection').AsText).Name;
    Result.RowsData := AData.Field('rows').Copy;
    Result.ReplaceStatic := AData.Field('replaceStatic').AsBoolean;
    NyxResourcesFromData(TNyxDataValue.ParseJSON(Result.CatalogBaseline));
    DecodeNyxCollectionDefaults(Result.CollectionBaseline, True);

    if Result.Operation = reoRows then
    begin
      TNyxResourceRows.FromData(Result.RowsData);
    end
    else if (Result.RowsData.Kind <> ndNull) or Result.ReplaceStatic then
    begin
      raise ENyxResource.Create('Detach cannot define a recipe or replace static rows');
    end;
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 8) then
  begin
    raise ENyxResource.Create('Resource proposal requires its exact copied fields');
  end;
  LResult := Default(TNyxResourceEditorChange);
  LOperation := AData.Field('operation').AsInteger;

  if not (LOperation in [Ord(reoDefine), Ord(reoRemove)]) then
  begin
    raise ENyxResource.Create('Unknown resource operation');
  end;
  LResult.Operation := TNyxResourceEditorOperation(LOperation);
  LResult.Selection := TNyxResourceEditorSelection.FromData(AData.Field('selection'));
  LResult.CatalogBaseline := AData.Field('catalog').AsText;
  LResult.DefinitionData := AData.Field('definition');
  LResult.Bind := AData.Field('bind').AsBoolean;
  LResult.Owner := AData.Field('owner').AsText;
  LResult.OwnerBaseline := AData.Field('ownerBaseline').AsText;

  if not LResult.Selection.Reference.Defined or (LResult.CatalogBaseline = '') then
  begin
    raise ENyxResource.Create('Resource Apply requires its reference and exact catalog');
  end;
  NyxResourcesFromData(TNyxDataValue.ParseJSON(LResult.CatalogBaseline));

  if LResult.Operation = reoDefine then
  begin
    NyxResourceFromData(LResult.DefinitionData);
  end
  else if (LResult.DefinitionData.Kind <> ndNull) or LResult.Bind then
  begin
    raise ENyxResource.Create('Removal cannot define content or add a binding');
  end;
  LBinding := AData.Field('binding');

  if LResult.Bind then
  begin

    if (LBinding.Kind <> ndObject) or (LBinding.Count <> 2) or
      (LResult.Owner = '') or (LResult.OwnerBaseline = '') then
    begin
      raise ENyxResource.Create('Resource binding requires its exact owner and descriptor');
    end;
    LTarget := LBinding.Field('target').AsInteger;

    if (LTarget < Ord(Low(TNyxBindingProperty))) or
      (LTarget > Ord(High(TNyxBindingProperty))) then
    begin
      raise ENyxResource.Create('Resource binding target is out of range');
    end;

    if LTarget = Ord(bpImage) then
    begin
      LResult.Binding := TNyxBindingSpec.Image(
        TNyxResourceImageRef.FromData(LBinding.Field('value')));

      if (LResult.Binding.ResourceImage.Reference.Name <> LResult.Selection.Reference.Name) or
        (LResult.Binding.ResourceImage.Localized and
        ((LResult.Binding.ResourceImage.Locale.Name <> LResult.Selection.Locale.Name) or
        LResult.Binding.ResourceImage.Fallback.Defined)) then
      begin
        raise ENyxResource.Create('Image binding belongs to a different file or locale');
      end;
    end
    else
    begin
      LResult.Binding := TNyxBindingSpec.Resource(TNyxBindingProperty(LTarget),
        TNyxResourceValueRef.FromData(LBinding.Field('value')));

      if (LResult.Binding.ResourceValue.Reference.Name <> LResult.Selection.Reference.Name) or
        (LResult.Binding.ResourceValue.Locale.Name <> LResult.Selection.Locale.Name) or
        LResult.Binding.ResourceValue.Fallback.Defined then
      begin
        raise ENyxResource.Create('Resource binding belongs to a different file or locale');
      end;
    end;
  end
  else if (LBinding.Kind <> ndNull) or (LResult.Owner <> '') or
    (LResult.OwnerBaseline <> '') then
  begin
    raise ENyxResource.Create('An unbound proposal cannot retain a control descriptor');
  end;
  Result := LResult;
end;

function TNyxResourceEditorDraft.GetDefined: Boolean;
begin
  Result := FEditor <> '';
end;

procedure TNyxResourceEditorDraft.Clear;
var
  LField: TNyxResourceEditorField;
begin
  FEditor := '';
  FContext := '';
  FPaths := NyxNull;
  FLabels := NyxResourceLabels;
  FLabelInput := '';
  FLabelSelection := Default(TNyxResourceLabelRef);
  { Retiring a proposal releases packed contents immediately, even while the
    enclosing per-project presentation record remains open. }
  for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
  begin
    FValues[LField] := '';
  end;
end;

procedure TNyxResourceEditorDraft.Capture(const AID: TNyxText; AShellRoot: TNyxNode);
var
  LEditor: TNyxNode;
  LField: TNyxResourceEditorField;
  LDraft: TNyxResourceEditorDraft;
  LTags: TNyxNode;
  LLabelState: TNyxResourceLabelsEditorState;
begin

  if AShellRoot = nil then
  begin
    Exit;
  end;
  LEditor := AShellRoot.Find(AID);

  if LEditor = nil then
  begin
    Exit;
  end;

  if not Complete(LEditor) then
  begin
    Clear;
    Exit;
  end;
  LDraft := Default(TNyxResourceEditorDraft);
  LDraft.FEditor := AID;
  LDraft.FContext := Context(LEditor);
  LDraft.FPaths := TNyxDataValue.ParseJSON(LEditor.Prop(CPaths));
  LDraft.FLabels := NyxResourceEditorLabels(LEditor);
  LTags := LEditor.Find(NyxResourceEditorLabelsID(LEditor.ID));

  if LTags <> nil then
  begin
    LLabelState := ReadNyxResourceLabelsEditor(LTags);
    LDraft.FLabelInput := LLabelState.Input;
    LDraft.FLabelSelection := LLabelState.Selection;
  end;
  for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
  begin
    LDraft.FValues[LField] := Field(LEditor, LField).Prop('value');
  end;
  Self := LDraft;
end;

function TNyxResourceEditorDraft.Restore(AShellRoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LField: TNyxResourceEditorField;
  LItems: TNyxText;
  LIndex: Integer;
  LTags: TNyxNode;
  LLabelState: TNyxResourceLabelsEditorState;
begin
  Result := False;

  if not Defined or (AShellRoot = nil) then
  begin
    Exit;
  end;
  LEditor := AShellRoot.Find(FEditor);

  if not Complete(LEditor) or (Context(LEditor) <> FContext) then
  begin
    Exit;
  end;
  LTags := LEditor.Find(NyxResourceEditorLabelsID(LEditor.ID));

  if (LTags = nil) and ((FLabelInput <> '') or FLabelSelection.Defined) then
  begin
    { A historical custom form has nowhere to restore this newer proposal.
      Refuse the whole draft instead of silently dropping incomplete tag input. }
    Exit;
  end;
  LLabelState := Default(TNyxResourceLabelsEditorState);
  LLabelState.Labels := FLabels;
  LLabelState.Input := FLabelInput;
  LLabelState.Selection := FLabelSelection;
  LItems := '';
  for LIndex := 0 to FPaths.Count - 1 do
  begin

    if LItems <> '' then
    begin
      LItems := LItems + TNyxText(#10);
    end;
    LItems := LItems + FPaths.Item(LIndex).Field('title').AsText;
  end;
  LEditor.SetProp(CPaths, FPaths.ToJSON);
  SetNyxResourceEditorLabels(LEditor, FLabels);

  if LTags <> nil then
  begin
    RestoreNyxResourceLabelsEditor(LTags, LLabelState);
  end;
  Field(LEditor, refPath).Configure.Items(LItems).Done;
  for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
  begin

    if LField in [refBind, refFallback] then
    begin
      Field(LEditor, LField).Configure.Value(FValues[LField] = 'true').Done;
    end
    else
    begin
      Field(LEditor, LField).Configure.Value(FValues[LField]).Done;
    end;
  end;
  { Invalid unsubmitted content stays editable. Discovery runs only by Preview
    or Apply, avoiding partial JSON becoming an unintended refresh failure. }
  RefreshNyxResourceEditor(LEditor, False);
  Result := True;
end;

function TNyxResourceEditorDraft.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LField: TNyxResourceEditorField;
  LLabelState: TNyxResourceLabelsEditorState;
begin
  Result := NyxNull;

  if not Defined then
  begin
    Exit;
  end;
  SetLength(LValues, Ord(High(TNyxResourceEditorField)) + 1);
  for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
  begin
    LValues[Ord(LField)] := NyxData(FValues[LField]);
  end;

  if (FLabelInput <> '') or FLabelSelection.Defined then
  begin
    LLabelState := Default(TNyxResourceLabelsEditorState);
    LLabelState.Labels := FLabels;
    LLabelState.Input := FLabelInput;
    LLabelState.Selection := FLabelSelection;
    Result := NyxObject([NyxField('version', NyxData(4)), NyxField('editor', NyxData(FEditor)),
      NyxField('context', NyxData(FContext)), NyxField('paths', FPaths),
      NyxField('values', NyxArray(LValues)), NyxField('labelEditor', LLabelState.ToData)]);
  end
  else if FLabels.Count > 0 then
  begin
    Result := NyxObject([NyxField('version', NyxData(3)), NyxField('editor', NyxData(FEditor)),
      NyxField('context', NyxData(FContext)), NyxField('paths', FPaths),
      NyxField('values', NyxArray(LValues)), NyxField('labels', FLabels.ToData)]);
  end
  else
  begin
    Result := NyxObject([NyxField('version', NyxData(2)), NyxField('editor', NyxData(FEditor)),
      NyxField('context', NyxData(FContext)), NyxField('paths', FPaths),
      NyxField('values', NyxArray(LValues))]);
  end;
end;

class function TNyxResourceEditorDraft.FromData(
  const AData: TNyxDataValue): TNyxResourceEditorDraft;
var
  LDraft: TNyxResourceEditorDraft;
  LValues: TNyxDataValue;
  LField: TNyxResourceEditorField;
  LIndex: Integer;
  LEntry: TNyxDataValue;
  LKind: Integer;
  LLegacy: Boolean;
  LCount: Integer;
  LVersion: Integer;
  LLabelState: TNyxResourceLabelsEditorState;

  procedure RequireChoice(const AValue: TNyxText; const ANames: array of TNyxText);
  var
    LChoice: Integer;
  begin
    for LChoice := 0 to High(ANames) do
    begin

      if AValue = ANames[LChoice] then
      begin
        Exit;
      end;
    end;
    raise ENyxResource.Create('Resource draft has an unsupported closed choice');
  end;
begin
  LDraft := Default(TNyxResourceEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit(LDraft);
  end;

  LLegacy := True;

  if AData.Kind = ndObject then
  begin
    for LIndex := 0 to AData.Count - 1 do
    begin

      if AData.Key(LIndex) = 'version' then
      begin
        LLegacy := False;
      end;
    end;
  end;
  LCount := Ord(High(TNyxResourceEditorField)) + 1;
  LVersion := 0;

  if not LLegacy then
  begin
    LVersion := AData.Field('version').AsInteger;
  end;

  if LLegacy then
  begin
    Dec(LCount);
  end;

  if (AData.Kind <> ndObject) or
    (LLegacy and (AData.Count <> 4)) or
    (not LLegacy and not (((AData.Count = 5) and (LVersion = 2)) or
      ((AData.Count = 6) and (LVersion in [3, 4])))) then
  begin
    raise ENyxResource.Create('Resource draft requires its exact context and fields');
  end;
  LDraft.FEditor := AData.Field('editor').AsText;
  LDraft.FContext := AData.Field('context').AsText;
  LDraft.FPaths := AData.Field('paths');

  if LVersion = 3 then
  begin
    LDraft.FLabels := TNyxResourceLabels.FromData(AData.Field('labels'));

    if LDraft.FLabels.Count = 0 then
    begin
      raise ENyxResource.Create('Labelled draft requires at least one label');
    end;
  end;

  if LVersion = 4 then
  begin
    LLabelState := TNyxResourceLabelsEditorState.FromData(AData.Field('labelEditor'));
    LDraft.FLabels := LLabelState.Labels;
    LDraft.FLabelInput := LLabelState.Input;
    LDraft.FLabelSelection := LLabelState.Selection;
  end;
  LValues := AData.Field('values');

  if (LDraft.FEditor = '') or (LDraft.FContext = '') or
    (LValues.Kind <> ndArray) or (LValues.Count <> LCount) or
    (LDraft.FPaths.Kind <> ndArray) or (LDraft.FPaths.Count > 256) then
  begin
    raise ENyxResource.Create('Resource draft has an invalid context or field count');
  end;
  for LField := Low(TNyxResourceEditorField) to High(TNyxResourceEditorField) do
  begin
    { The unversioned eighteen-field form always pinned the edited variant.
      Migration supplies only that historical meaning; new packets must carry
      the explicit choice and cannot accidentally acquire a different default. }

    if LLegacy and (LField = refImageLocale) then
    begin
      LDraft.FValues[LField] := CImageLocales[reilSelected];
      Continue;
    end;
    LDraft.FValues[LField] := LValues.Item(Ord(LField)).AsText;

    if (LField in [refBind, refFallback]) and
      (LDraft.FValues[LField] <> 'true') and (LDraft.FValues[LField] <> 'false') then
    begin
      raise ENyxResource.Create('Resource draft requires a Boolean choice');
    end;
  end;
  { Validate every copied choice before Restore writes its first field. Cached
    discovery remains presentation data; Apply rediscovers from current bytes. }
  RequireChoice(LDraft.FValues[refKind], CKinds);
  RequireChoice(LDraft.FValues[refSource], CSources);
  RequireChoice(LDraft.FValues[refCache], CCaches);
  RequireChoice(LDraft.FValues[refServer], CServers);
  RequireChoice(LDraft.FValues[refEncoding], CEncodings);
  RequireChoice(LDraft.FValues[refImageLocale], CImageLocales);
  for LIndex := 0 to LDraft.FPaths.Count - 1 do
  begin
    LEntry := LDraft.FPaths.Item(LIndex);

    if (LEntry.Kind <> ndObject) or (LEntry.Count <> 3) or
      (LEntry.Field('title').AsText = '') then
    begin
      raise ENyxResource.Create('Resource draft has an invalid scalar choice');
    end;
    TNyxResourcePath.FromData(LEntry.Field('path'));
    LKind := LEntry.Field('kind').AsInteger;

    if (LKind < Ord(Low(TNyxStateKind))) or (LKind > Ord(High(TNyxStateKind))) then
    begin
      raise ENyxResource.Create('Resource draft has an unknown scalar type');
    end;
  end;
  Result := LDraft;
end;

end.
