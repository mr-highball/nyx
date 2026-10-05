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
unit nyx.studio.collections;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.studio.session,
  nyx.studio.collectionintent;

type
  { Opaque, value-only chrome identity at the transport boundary. The six
    metadata values distinguish collection/row/column ownership when positional
    widget IDs are reused. This is presentation identity, not authoring intent;
    CaptureNyxStudioCollection decodes its behavioral choices into typed values. }
  TNyxStudioCollectionChromeIdentity = record
  private
    FDefined: Boolean;
    FCommand: TNyxText;
    FKey: TNyxText;
    FField: TNyxText;
    FRow: TNyxText;
    FOwner: TNyxText;
    FKind: TNyxText;
  public
    { Borrow only while copying; no accepted node or physical input is kept. }
    class function FromNode(ANode: TNyxNode): TNyxStudioCollectionChromeIdentity; static;
    { Undefined identities never match. A defined identity requires all copied
      values, so a reused positional widget ID cannot inherit another field's focus. }
    function Matches(ANode: TNyxNode): Boolean;
    property Defined: Boolean read FDefined;
  end;

{ Nyx-built collection authoring. Values cross normal control events, then the
  router decodes closed choices and invokes detached, undoable session commands.
  No target handles, callbacks or mutable stores are retained by this UI. }
procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean); overload;
procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean;
  const APending: TNyxStudioPendingDesign); overload;
procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode); overload;
procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode;
  const APending: TNyxStudioPendingDesign); overload;
{ Capture owns no accepted node/store and performs no source generation.
  Partial scalar notation stays text until the isolated session parses it.
  False means this is not the command's supported event. Invalid closed choices,
  stale mounted identities or pending structural locks raise before enqueueing. }
function CaptureNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;

const
  { Explicit shared chrome metadata; adapters use the complete logical tuple
    when restoring focus across positional collection/row/column IDs. }
  NyxStudioCollectionCommandKey = 'collection-command';
  NyxStudioCollectionKey = 'collection-key';
  NyxStudioCollectionFieldKey = 'collection-field';
  NyxStudioCollectionRowKey = 'collection-row';
  NyxStudioCollectionOwnerKey = 'collection-owner';
  NyxStudioCollectionKindKey = 'collection-kind';

{ Compatibility consumer for synchronous/embedded authoring. Studio's adapters
  consume Capture through their independent source queue before this router.
  A successful route uses the same typed replay and paired session boundary. }
function RouteNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger): Boolean;

implementation

uses
  SysUtils,
  nyx.state,
  nyx.contract,
  nyx.collections,
  nyx.collections.view.types,
  nyx.collections.selection,
  nyx.studio.authoring;

type
  TCollectionCommand = TNyxStudioCollectionAction;

const
  CCommandKey = 'collection-command';
  CKey = 'collection-key';
  CField = 'collection-field';
  CRow = 'collection-row';
  COwner = 'collection-owner';
  CKind = 'collection-kind';
  CCommands: array[TCollectionCommand] of TNyxText =
    ('create', 'remove', 'add-field', 'default', 'add-row', 'remove-row',
    'cell', 'bind', 'scope', 'title', 'mode', 'parent', 'remove-column',
    'add-column', 'clear', 'inherit', 'selection');

class function TNyxStudioCollectionChromeIdentity.FromNode(ANode: TNyxNode):
  TNyxStudioCollectionChromeIdentity;
begin
  Result := Default(TNyxStudioCollectionChromeIdentity);

  if (ANode = nil) or (ANode.Prop(CCommandKey) = '') then
  begin
    Exit;
  end;
  Result.FDefined := True;
  Result.FCommand := ANode.Prop(CCommandKey);
  Result.FKey := ANode.Prop(CKey);
  Result.FField := ANode.Prop(CField);
  Result.FRow := ANode.Prop(CRow);
  Result.FOwner := ANode.Prop(COwner);
  Result.FKind := ANode.Prop(CKind);
end;

function TNyxStudioCollectionChromeIdentity.Matches(ANode: TNyxNode): Boolean;
var
  LOther: TNyxStudioCollectionChromeIdentity;
begin
  LOther := FromNode(ANode);
  Result := FDefined and LOther.FDefined and (FCommand = LOther.FCommand) and
    (FKey = LOther.FKey) and (FField = LOther.FField) and (FRow = LOther.FRow) and
    (FOwner = LOther.FOwner) and (FKind = LOther.FKind);
end;

function Command(AKind: TNyxKind; const AID, ATitle, AKey: TNyxText;
  ACommand: TCollectionCommand): TNyxNode;
begin
  Result := TNyxNode.Create(AKind, AID).Configure.Text(ATitle)
    .Extension(CCommandKey, CCommands[ACommand]).Extension(CKey, AKey).Done;
end;

function Editor(const AID, ATitle, AKey, AField, ARow: TNyxText;
  const AValue: TNyxStateValue; ACommand: TCollectionCommand): TNyxNode;
var
  LInput: TNyxStudioStateInput;
  LKind: TNyxKind;
begin
  LInput := NyxStudioStateInputFor(AValue);
  LKind := nkInput;

  if LInput in [ssiText, ssiEscapedText] then
  begin
    LKind := nkMemo;
  end
  else if LInput = ssiBoolean then
  begin
    LKind := nkSelect;
  end;
  Result := Command(LKind, AID, ATitle, AKey, ACommand);
  Result.Configure.Value(NyxStudioStateEditorText(AValue))
    .Extension(CField, AField).Extension(CRow, ARow)
    .Extension(CKind, NyxStateKindName(AValue.Kind))
    .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput)).Done;

  if LInput = ssiBoolean then
  begin
    Result.Configure.Items('false' + #10 + 'true').Done;
  end;
end;

procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean);
begin
  AddNyxCollectionDefaultsPanel(AParent, ASession, AVisible,
    Default(TNyxStudioPendingDesign));
end;

procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean;
  const APending: TNyxStudioPendingDesign);
var
  LPanel, LCard, LRow: TNyxNode;
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LCollection, LIndex, LItem: Integer;
  LKind: TNyxStateKind;
  LKey, LPrefix: TNyxText;
  LPending: TNyxStudioCollectionIntent;
  LItemRef: TNyxItemRef;
  LLocked: Boolean;

  function PendingEditor(const AID, ATitle, AField: TNyxText;
    const AItem: TNyxItemRef; const AValue: TNyxStateValue;
    AAction: TNyxStudioCollectionAction): TNyxNode;
  var
    LRowID: TNyxText;
    LInput: TNyxStudioStateInput;
    LReference: TNyxStudioCollectionFieldRef;
  begin
    LRowID := '';

    if AItem.Defined then
    begin
      LRowID := AItem.ID;
    end;
    Result := Editor(AID, ATitle, LKey, AField, LRowID, AValue, AAction);
    Result.Configure.Enabled(not LLocked).Done;
    LReference := TNyxStudioCollectionFieldRef.FromMetadata(LField);

    if APending.CollectionValue(NyxCollection(LKey), AItem, LReference,
      AAction, '', LPending) then
    begin
      { A newer waiting escaped/plain notation owns this field presentation.
        Earlier publication or failure must not normalize/drop its exact text. }
      LInput := LPending.Input;
      Result.Configure.Value(LPending.Value)
        .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput)).Done;
    end;
  end;
begin

  if not AVisible then
  begin
    Exit;
  end;
  LPanel := TNyxNode.Create(nkColumn, 'studio-collections').Configure.Gap(12).Done;
  AParent.Add(LPanel);
  LPanel.Add(TNyxNode.Create(nkHeading, 'collections-title').Configure.Text('Collections').Done);
  LPanel.Add(TNyxNode.Create(nkLabel, 'collections-help').Configure.Text(
    'Define typed fields and saved rows. Bind a list, table or tree in the inspector.').Done);
  LPanel.Add(Command(nkButton, 'collection-create', 'Add collection', '', scaCreate)
    .Configure.Enabled(not APending.CollectionCreationPending).Done);
  for LCollection := 0 to ASession.Document.Collections.Count - 1 do
  begin
    LKey := ASession.Document.Collections.Key(LCollection).Name;
    LData := ASession.Document.Collections.Snapshot(NyxCollection(LKey));
    LSchema := LData.Schema;
    LLocked := APending.CollectionLocked(NyxCollection(LKey));
    LPrefix := 'collection-' + IntToStr(LCollection);
    LCard := TNyxNode.Create(nkPanel, LPrefix).Configure.Surface(True).Padding(12).Gap(8).Done;
    LPanel.Add(LCard);
    LCard.Add(TNyxNode.Create(nkHeading, LPrefix + '-title').Configure.Text(LKey).Done);
    LCard.Add(Command(nkButton, LPrefix + '-remove', 'Remove collection', LKey, scaRemove)
      .Configure.Enabled(not LLocked).Done);
    for LIndex := 0 to LSchema.Count - 1 do
    begin
      LField := LSchema.FieldAt(LIndex);
      LCard.Add(PendingEditor(LPrefix + '-field-' + IntToStr(LIndex),
        LField.Name + ' / ' + NyxStateKindName(LField.Kind) + ' default',
        LField.Name, Default(TNyxItemRef), LField.DefaultValue, scaDefault));
    end;
    LRow := TNyxNode.Create(nkGrid, LPrefix + '-add-fields').Configure.Columns(2).Gap(6).Done;
    LCard.Add(LRow);
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin
      LRow.Add(Command(nkButton, LPrefix + '-add-' + NyxStateKindName(LKind),
        '+ ' + NyxStateKindName(LKind) + ' field', LKey, scaAddField).Configure
        .Extension(CKind, NyxStateKindName(LKind)).Enabled(not LLocked).Done);
    end;
    for LItem := 0 to LData.Count - 1 do
    begin
      LItemRef := LData.ItemAt(LItem).Ref;
      LRow := TNyxNode.Create(nkPanel, LPrefix + '-row-' + IntToStr(LItem))
        .Configure.Surface(True).Padding(8).Gap(6).Done;
      LCard.Add(LRow);
      LRow.Add(TNyxNode.Create(nkLabel, LRow.ID + '-id').Configure.Text(LData.ItemAt(LItem).Ref.ID).Done);
      for LIndex := 0 to LSchema.Count - 1 do
      begin
        LField := LSchema.FieldAt(LIndex);
        LRow.Add(PendingEditor(LRow.ID + '-cell-' + IntToStr(LIndex), LField.Name,
          LField.Name, LItemRef,
          LData.ItemAt(LItem).FieldValue(LIndex), scaCell));
      end;
      LRow.Add(Command(nkButton, LRow.ID + '-remove', 'Remove row', LKey, scaRemoveRow)
        .Configure.Extension(CRow, LItemRef.ID).Enabled(not LLocked).Done);
    end;
    LCard.Add(Command(nkButton, LPrefix + '-add-row', 'Add row', LKey, scaAddRow)
      .Configure.Enabled(not LLocked).Done);
  end;
end;

procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode);
begin
  AddNyxCollectionBindingPanel(AParent, ASession, AProjection,
    Default(TNyxStudioPendingDesign));
end;

procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode;
  const APending: TNyxStudioPendingDesign);
var
  LPanel, LRow: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LInherited: TNyxCollectionViewSpec;
  LColumn: TNyxCollectionColumn;
  LSchema: TNyxCollectionSchema;
  LIndex, LFieldIndex: Integer;
  LKey, LID: TNyxText;
  LFound: Boolean;
  LLocked: Boolean;
  LPendingIndex: Integer;

  function Action(AKind: TNyxKind; const AID, ATitle: TNyxText;
    ACommand: TCollectionCommand): TNyxNode;
  begin
    Result := Command(AKind, AID, ATitle, LKey, ACommand).Configure
      .Extension(COwner, ASession.SelectedID).Enabled(not LLocked).Done;
  end;

begin

  if (AProjection = nil) or not
    ((AProjection.ProjectionKind = 'list') or (AProjection.ProjectionKind = 'table') or
    (AProjection.ProjectionKind = 'tree')) then
  begin
    Exit;
  end;
  LPanel := TNyxNode.Create(nkPanel, 'studio-collection-binding').Configure
    .Surface(True).Padding(12).Gap(8).Done;
  AParent.Add(LPanel);
  LPanel.Add(TNyxNode.Create(nkHeading, 'collection-binding-title').Configure.Text('Collection binding').Done);
  LKey := '';

  if AProjection.HasCollectionView then
  begin
    LSpec := AProjection.CollectionView;
  end
  else
  begin
    LSpec := Default(TNyxCollectionViewSpec);
  end;
  LLocked := APending.CollectionViewLocked(ASession.SelectedID);
  for LPendingIndex := 0 to High(APending.Collections) do
  begin

    if (APending.Collections[LPendingIndex].Owner = ASession.SelectedID) and
      NyxStudioCollectionViewAction(APending.Collections[LPendingIndex].Intent.Action) and
      (APending.Collections[LPendingIndex].Intent.Action <> scaInherit) then
    begin
      try
        LSchema := ASession.Document.Collections.Snapshot(
          APending.Collections[LPendingIndex].Intent.Key).Schema;
        LSpec := ApplyNyxStudioCollectionViewIntent(
          APending.Collections[LPendingIndex].Intent, LSpec, LSchema);
      except
        on ENyxCollection do
        begin
          { Pending presentation is never admission. A valid typed proposal can
            still reference a missing collection/field or a changed family.
            Preserve the last displayable specification while isolated replay
            owns its diagnostic; do not throw from the editor paint callback. }
        end;
      end;
    end;
  end;

  if LSpec.Defined then
  begin
    LKey := LSpec.Key.Name;
    LLocked := LLocked or APending.CollectionLocked(LSpec.Key);
    LPanel.Add(TNyxNode.Create(nkLabel, 'collection-binding-current').Configure.Text('Current: ' + LKey).Done);
    LPanel.Add(Action(nkSelect, 'collection-binding-scope', 'Data scope', scaScope)
      .Configure.Items('Application' + #10 + 'Reusable instance').Value('Application').Done);

    if LSpec.Scope = csInstance then
    begin
      LPanel.Children[LPanel.Count - 1].Configure.Value('Reusable instance').Done;
    end;
    LPanel.Add(Action(nkSelect, 'collection-binding-selection', 'Selection', scaSelection)
      .Configure.Items('Single item' + #10 + 'Multiple items').Value('Single item').Done);

    if LSpec.SelectionMode = nsmMultiple then
    begin
      LPanel.Children[LPanel.Count - 1].Configure.Value('Multiple items').Done;
    end;
    for LIndex := 0 to LSpec.Count - 1 do
    begin
      LColumn := LSpec.ColumnAt(LIndex);
      LID := 'collection-column-' + IntToStr(LIndex);
      LRow := TNyxNode.Create(nkColumn, LID).Configure.Gap(6).Done;
      LPanel.Add(LRow);
      LRow.Add(Action(nkInput, LID + '-title', LColumn.FieldName + ' title', scaTitle)
        .Configure.Value(LColumn.Title).Extension(CField, LColumn.FieldName)
        .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);
      LRow.Add(Action(nkSelect, LID + '-mode', 'Editing', scaMode)
        .Configure.Items('Read only' + #10 + 'Editable').Value('Read only')
        .Extension(CField, LColumn.FieldName)
        .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);

      if LColumn.Mode = cmEditable then
      begin
        LRow.Children[LRow.Count - 1].Configure.Value('Editable').Done;
      end;
      LRow.Add(Action(nkButton, LID + '-remove', 'Remove column', scaRemoveColumn)
        .Configure.Extension(CField, LColumn.FieldName)
        .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);
    end;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;
    for LFieldIndex := 0 to LSchema.Count - 1 do
    begin
      LFound := False;
      for LIndex := 0 to LSpec.Count - 1 do
      begin
        LFound := LFound or (LSpec.ColumnAt(LIndex).FieldName = LSchema.FieldAt(LFieldIndex).Name);
      end;

      if not LFound then
      begin
        LPanel.Add(Action(nkButton, 'collection-column-add-' + IntToStr(LFieldIndex),
          '+ ' + LSchema.FieldAt(LFieldIndex).Name + ' column', scaAddColumn)
          .Configure.Extension(CField, LSchema.FieldAt(LFieldIndex).Name)
          .Extension(CKind, NyxStateKindName(LSchema.FieldAt(LFieldIndex).Kind)).Done);
      end;

      if (AProjection.ProjectionKind = 'tree') and (LSchema.FieldAt(LFieldIndex).Kind = nskText) then
      begin
        LPanel.Add(Action(nkButton, 'collection-parent-' + IntToStr(LFieldIndex),
          'Parent field: ' + LSchema.FieldAt(LFieldIndex).Name, scaParent)
          .Configure.Extension(CField, LSchema.FieldAt(LFieldIndex).Name)
          .Extension(CKind, NyxStateKindName(nskText)).Done);
      end;
    end;

    if AProjection.ProjectionKind = 'tree' then
    begin
      LPanel.Add(Action(nkButton, 'collection-parent-none', 'No parent field', scaParent));
    end;
    LPanel.Add(Action(nkButton, 'collection-binding-clear', 'Unbind collection', scaClear));
    LPanel.Add(Action(nkButton, 'collection-binding-inherit', 'Restore inherited binding', scaInherit));
  end
  else if (ASession.Selected <> nil) and ASession.Selected.HasCollectionView and
    not ASession.Selected.CollectionView.Defined then
  begin
    { An explicit clear masks effective metadata; it must still offer a way
      back to this exact owner's inherited contract. Pending structural paint
      keeps the same locks, and capture retains that contract's exact key. }
    LInherited := ASession.ClearedCollectionInheritance(ASession.SelectedID);

    if LInherited.Defined then
    begin
      LKey := LInherited.Key.Name;
      LLocked := LLocked or APending.CollectionLocked(LInherited.Key);
      LPanel.Add(TNyxNode.Create(nkLabel, 'collection-binding-inherited').Configure
        .Text('Inherited collection: ' + LKey).Done);
      LPanel.Add(Action(nkButton, 'collection-binding-inherit', 'Restore inherited binding', scaInherit));
    end;
  end;
  for LIndex := 0 to ASession.Document.Collections.Count - 1 do
  begin
    LKey := ASession.Document.Collections.Key(LIndex).Name;
    LPanel.Add(Action(nkButton, 'collection-bind-' + IntToStr(LIndex), 'Bind ' + LKey, scaBind));
  end;

  if ASession.Document.Collections.Count = 0 then
  begin
    LPanel.Add(TNyxNode.Create(nkLabel, 'collection-binding-empty').Configure
      .Text('Add a collection in the project Data section first.').Done);
  end;
end;


function CaptureNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LAction: TNyxStudioCollectionAction;
  LFound: Boolean;
  LKey: TNyxText;
  LFieldName: TNyxText;
  LName: TNyxText;
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LIndex: Integer;
  LKind: TNyxStateKind;
begin
  Result := False;
  AEdit := Default(TNyxStudioDesignEdit);

  if (ANode = nil) or (ASession = nil) or (ANode.Prop(CCommandKey) = '') then
  begin
    Exit;
  end;
  LFound := False;
  for LAction := Low(TNyxStudioCollectionAction) to High(TNyxStudioCollectionAction) do
  begin

    if ANode.Prop(CCommandKey) = CCommands[LAction] then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Unknown collection command');
  end;

  if ((LAction in [scaDefault, scaCell, scaScope, scaTitle, scaMode, scaSelection]) and
    (AEvent <> ntChange)) or
    (not (LAction in [scaDefault, scaCell, scaScope, scaTitle, scaMode, scaSelection]) and
    (AEvent <> ntClick)) then
  begin
    Exit;
  end;
  AEdit.Action := sdaCollection;
  AEdit.Collection.Action := LAction;
  LKey := ANode.Prop(CKey);

  if LAction = scaCreate then
  begin

    if APending.CollectionCreationPending then
    begin
      raise ENyxCollection.Create('Wait for collection creation before submitting this form again');
    end;
    LIndex := 1;
    repeat
      LKey := 'collection' + IntToStr(LIndex);
      Inc(LIndex);
    until not ASession.Document.Collections.Has(NyxCollection(LKey));
    AEdit.Collection.Key := NyxCollection(LKey);
    AEdit.Collection.Validate;
    Exit(True);
  end;
  AEdit.Collection.Key := NyxCollection(LKey);

  if APending.CollectionLocked(AEdit.Collection.Key) then
  begin
    raise ENyxCollection.Create('Wait for this collection structure change before editing it');
  end;
  LData := ASession.Document.Collections.Snapshot(AEdit.Collection.Key);
  LSchema := LData.Schema;
  LFieldName := ANode.Prop(CField);

  if LFieldName <> '' then
  begin
    LFound := False;
    for LIndex := 0 to LSchema.Count - 1 do
    begin
      LField := LSchema.FieldAt(LIndex);

      if LField.Name = LFieldName then
      begin

        if (ANode.Prop(CKind) <> '') and
          (ANode.Prop(CKind) <> NyxStateKindName(LField.Kind)) then
        begin
          raise ENyxCollection.Create('Mounted collection field changed its scalar family');
        end;
        AEdit.Collection.Field := TNyxStudioCollectionFieldRef.FromMetadata(LField);
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxCollection.Create('Collection field no longer exists');
    end;
  end;

  if LAction = scaAddField then
  begin
    LFound := False;
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin

      if ANode.Prop(CKind) = NyxStateKindName(LKind) then
      begin
        AEdit.Collection.Kind := LKind;
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxCollection.Create('Unknown collection field family');
    end;
  end;

  if LAction = scaAddRow then
  begin
    LIndex := 1;
    repeat
      LName := 'row' + IntToStr(LIndex);
      Inc(LIndex);
    until LData.IndexOf(NyxItem(AEdit.Collection.Key, LName)) < 0;
    AEdit.Collection.Item := NyxItem(AEdit.Collection.Key, LName);
  end
  else if LAction in [scaCell, scaRemoveRow] then
  begin
    AEdit.Collection.Item := NyxItem(AEdit.Collection.Key, ANode.Prop(CRow));

    if LData.IndexOf(AEdit.Collection.Item) < 0 then
    begin
      raise ENyxCollection.Create('Collection row no longer exists');
    end;
  end;

  if LAction in [scaDefault, scaCell] then
  begin

    if not TryNyxStudioStateInput(ANode.Prop(NyxStudioStateInputKey), AEdit.Collection.Input) then
    begin
      raise ENyxCollection.Create('Unknown collection scalar notation');
    end;
    AEdit.Collection.Value := ANode.Prop('value');
  end
  else if LAction = scaTitle then
  begin
    AEdit.Collection.Value := ANode.Prop('value');
  end;

  if NyxStudioCollectionViewAction(LAction) then
  begin

    if ANode.Prop(COwner) <> ASession.SelectedID then
    begin
      raise ENyxCollection.Create('Collection selection changed; use its current inspector');
    end;

    if APending.CollectionViewLocked(ASession.SelectedID) then
    begin
      raise ENyxCollection.Create('Wait for this collection view structure change before editing it');
    end;
    AEdit.Selection := ASession.SelectedID;
    AEdit.View := ASession.ActiveViewID;
    LProjection := ASession.SelectedProjection;
    try

      if LProjection = nil then
      begin
        raise ENyxCollection.Create('Select a list, table or tree');
      end;

      if LProjection.ProjectionKind = NyxKindName(nkList) then
      begin
        AEdit.Collection.Projection := cpList;
      end
      else if LProjection.ProjectionKind = NyxKindName(nkTable) then
      begin
        AEdit.Collection.Projection := cpTable;
      end
      else if LProjection.ProjectionKind = NyxKindName(nkTree) then
      begin
        AEdit.Collection.Projection := cpTree;
      end
      else
      begin
        raise ENyxCollection.Create('Selected control no longer supports collection views');
      end;

      LSpec := LProjection.CollectionView;

      if (LAction = scaInherit) and not LSpec.Defined then
      begin
        LSpec := ASession.ClearedCollectionInheritance(ASession.SelectedID);
      end;

      if (LAction <> scaBind) and
        (not LSpec.Defined or (LSpec.Key.Name <> LKey)) then
      begin
        raise ENyxCollection.Create('Collection binding changed; use its current inspector');
      end;
    finally
      LProjection.Free;
    end;
    case LAction of
      scaScope:
        begin

          if ANode.Prop('value') = 'Application' then
          begin
            AEdit.Collection.Scope := csApplication;
          end
          else if ANode.Prop('value') = 'Reusable instance' then
          begin
            AEdit.Collection.Scope := csInstance;
          end
          else
          begin
            raise ENyxCollection.Create('Unknown collection scope');
          end;
        end;
      scaSelection:
        begin

          if ANode.Prop('value') = 'Single item' then
          begin
            AEdit.Collection.SelectionMode := nsmSingle;
          end
          else if ANode.Prop('value') = 'Multiple items' then
          begin
            AEdit.Collection.SelectionMode := nsmMultiple;
          end
          else
          begin
            raise ENyxCollection.Create('Unknown collection selection mode');
          end;
        end;
      scaMode:
        begin

          if ANode.Prop('value') = 'Read only' then
          begin
            AEdit.Collection.Mode := cmReadOnly;
          end
          else if ANode.Prop('value') = 'Editable' then
          begin
            AEdit.Collection.Mode := cmEditable;
          end
          else
          begin
            raise ENyxCollection.Create('Unknown collection column editability');
          end;
        end;
    else
      begin
        { Other operations carry no extra closed view choice. }
      end;
    end;
  end;
  AEdit.Collection.Validate;
  Result := True;
end;

function RouteNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger): Boolean;
var
  LEdit: TNyxStudioDesignEdit;
begin
  Result := CaptureNyxStudioCollection(ASession, ANode, AEvent,
    Default(TNyxStudioPendingDesign), LEdit);

  if Result then
  begin
    ASession.ApplyCollectionIntent(LEdit.Collection);
  end;
end;

end.
