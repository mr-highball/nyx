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
  nyx.studio.session;

{ Nyx-built collection authoring. Values cross normal control events, then the
  router decodes closed choices and invokes detached, undoable session commands.
  No target handles, callbacks or mutable stores are retained by this UI. }
procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean);
procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode);
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
  TCollectionCommand = (ccCreate, ccRemove, ccAddField, ccDefault, ccAddRow,
    ccRemoveRow, ccCell, ccBind, ccScope, ccTitle, ccMode, ccParent,
    ccRemoveColumn, ccAddColumn, ccClear, ccInherit, ccSelection);

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
    .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput)).Done;

  if LInput = ssiBoolean then
  begin
    Result.Configure.Items('false' + #10 + 'true').Done;
  end;
end;

procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean);
var
  LPanel, LCard, LRow: TNyxNode;
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LCollection, LIndex, LItem: Integer;
  LKind: TNyxStateKind;
  LKey, LPrefix: TNyxText;
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
  LPanel.Add(Command(nkButton, 'collection-create', 'Add collection', '', ccCreate));
  for LCollection := 0 to ASession.Document.Collections.Count - 1 do
  begin
    LKey := ASession.Document.Collections.Key(LCollection).Name;
    LData := ASession.Document.Collections.Snapshot(NyxCollection(LKey));
    LSchema := LData.Schema;
    LPrefix := 'collection-' + IntToStr(LCollection);
    LCard := TNyxNode.Create(nkPanel, LPrefix).Configure.Surface(True).Padding(12).Gap(8).Done;
    LPanel.Add(LCard);
    LCard.Add(TNyxNode.Create(nkHeading, LPrefix + '-title').Configure.Text(LKey).Done);
    LCard.Add(Command(nkButton, LPrefix + '-remove', 'Remove collection', LKey, ccRemove));
    for LIndex := 0 to LSchema.Count - 1 do
    begin
      LField := LSchema.FieldAt(LIndex);
      LCard.Add(Editor(LPrefix + '-field-' + IntToStr(LIndex),
        LField.Name + ' / ' + NyxStateKindName(LField.Kind) + ' default',
        LKey, LField.Name, '', LField.DefaultValue, ccDefault));
    end;
    LRow := TNyxNode.Create(nkGrid, LPrefix + '-add-fields').Configure.Columns(2).Gap(6).Done;
    LCard.Add(LRow);
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin
      LRow.Add(Command(nkButton, LPrefix + '-add-' + NyxStateKindName(LKind),
        '+ ' + NyxStateKindName(LKind) + ' field', LKey, ccAddField).Configure
        .Extension(CKind, NyxStateKindName(LKind)).Done);
    end;
    for LItem := 0 to LData.Count - 1 do
    begin
      LRow := TNyxNode.Create(nkPanel, LPrefix + '-row-' + IntToStr(LItem))
        .Configure.Surface(True).Padding(8).Gap(6).Done;
      LCard.Add(LRow);
      LRow.Add(TNyxNode.Create(nkLabel, LRow.ID + '-id').Configure.Text(LData.ItemAt(LItem).Ref.ID).Done);
      for LIndex := 0 to LSchema.Count - 1 do
      begin
        LField := LSchema.FieldAt(LIndex);
        LRow.Add(Editor(LRow.ID + '-cell-' + IntToStr(LIndex), LField.Name,
          LKey, LField.Name, LData.ItemAt(LItem).Ref.ID,
          LData.ItemAt(LItem).FieldValue(LIndex), ccCell));
      end;
      LRow.Add(Command(nkButton, LRow.ID + '-remove', 'Remove row', LKey, ccRemoveRow)
        .Configure.Extension(CRow, LData.ItemAt(LItem).Ref.ID).Done);
    end;
    LCard.Add(Command(nkButton, LPrefix + '-add-row', 'Add row', LKey, ccAddRow));
  end;
end;

procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode);
var
  LPanel, LRow: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LColumn: TNyxCollectionColumn;
  LSchema: TNyxCollectionSchema;
  LIndex, LFieldIndex: Integer;
  LKey, LID: TNyxText;
  LFound: Boolean;

  function Action(AKind: TNyxKind; const AID, ATitle: TNyxText;
    ACommand: TCollectionCommand): TNyxNode;
  begin
    Result := Command(AKind, AID, ATitle, LKey, ACommand).Configure
      .Extension(COwner, ASession.SelectedID).Done;
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

  if LSpec.Defined then
  begin
    LKey := LSpec.Key.Name;
    LPanel.Add(TNyxNode.Create(nkLabel, 'collection-binding-current').Configure.Text('Current: ' + LKey).Done);
    LPanel.Add(Action(nkSelect, 'collection-binding-scope', 'Data scope', ccScope)
      .Configure.Items('Application' + #10 + 'Reusable instance').Value('Application').Done);

    if LSpec.Scope = csInstance then
    begin
      LPanel.Children[LPanel.Count - 1].Configure.Value('Reusable instance').Done;
    end;
    LPanel.Add(Action(nkSelect, 'collection-binding-selection', 'Selection', ccSelection)
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
      LRow.Add(Action(nkInput, LID + '-title', LColumn.FieldName + ' title', ccTitle)
        .Configure.Value(LColumn.Title).Extension(CField, LColumn.FieldName).Done);
      LRow.Add(Action(nkSelect, LID + '-mode', 'Editing', ccMode)
        .Configure.Items('Read only' + #10 + 'Editable').Value('Read only')
        .Extension(CField, LColumn.FieldName).Done);

      if LColumn.Mode = cmEditable then
      begin
        LRow.Children[LRow.Count - 1].Configure.Value('Editable').Done;
      end;
      LRow.Add(Action(nkButton, LID + '-remove', 'Remove column', ccRemoveColumn)
        .Configure.Extension(CField, LColumn.FieldName).Done);
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
          '+ ' + LSchema.FieldAt(LFieldIndex).Name + ' column', ccAddColumn)
          .Configure.Extension(CField, LSchema.FieldAt(LFieldIndex).Name).Done);
      end;

      if (AProjection.ProjectionKind = 'tree') and (LSchema.FieldAt(LFieldIndex).Kind = nskText) then
      begin
        LPanel.Add(Action(nkButton, 'collection-parent-' + IntToStr(LFieldIndex),
          'Parent field: ' + LSchema.FieldAt(LFieldIndex).Name, ccParent)
          .Configure.Extension(CField, LSchema.FieldAt(LFieldIndex).Name).Done);
      end;
    end;

    if AProjection.ProjectionKind = 'tree' then
    begin
      LPanel.Add(Action(nkButton, 'collection-parent-none', 'No parent field', ccParent));
    end;
    LPanel.Add(Action(nkButton, 'collection-binding-clear', 'Unbind collection', ccClear));
    LPanel.Add(Action(nkButton, 'collection-binding-inherit', 'Restore inherited binding', ccInherit));
  end;
  for LIndex := 0 to ASession.Document.Collections.Count - 1 do
  begin
    LKey := ASession.Document.Collections.Key(LIndex).Name;
    LPanel.Add(Action(nkButton, 'collection-bind-' + IntToStr(LIndex), 'Bind ' + LKey, ccBind));
  end;

  if ASession.Document.Collections.Count = 0 then
  begin
    LPanel.Add(TNyxNode.Create(nkLabel, 'collection-binding-empty').Configure
      .Text('Add a collection in the project Data section first.').Done);
  end;
end;

function AddColumn(const ASpec: TNyxCollectionViewSpec; const AName, ATitle: TNyxText;
  AKind: TNyxStateKind; AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
begin
  case AKind of
    nskText: Result := ASpec.Column(NyxTextField(AName), ATitle, AMode);
    nskBoolean: Result := ASpec.Column(NyxBooleanField(AName), ATitle, AMode);
    nskInteger: Result := ASpec.Column(NyxIntegerField(AName), ATitle, AMode);
    nskNumber: Result := ASpec.Column(NyxNumberField(AName), ATitle, AMode);
  end;
end;

function PutValue(const AItem: TNyxCollectionItem; const AField: TNyxText;
  const AValue: TNyxStateValue): TNyxCollectionItem;
begin
  case AValue.Kind of
    nskText: Result := AItem.WithValue(NyxTextField(AField), AValue.TextValue);
    nskBoolean: Result := AItem.WithValue(NyxBooleanField(AField), AValue.BooleanValue);
    nskInteger: Result := AItem.WithValue(NyxIntegerField(AField), AValue.IntegerValue);
    nskNumber: Result := AItem.WithValue(NyxNumberField(AField), AValue.NumberValue);
  end;
end;

function RouteNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger): Boolean;
var
  LCommand: TCollectionCommand;
  LFound: Boolean;
  LKey, LField, LName: TNyxText;
  LData: INyxCollectionSnapshot;
  LSchema, LNextSchema: TNyxCollectionSchema;
  LItems: array of TNyxCollectionItem;
  LSpec, LNext: TNyxCollectionViewSpec;
  LProjection: TNyxNode;
  LColumn: TNyxCollectionColumn;
  LFieldInfo: TNyxCollectionField;
  LValue: TNyxStateValue;
  LKind: TNyxStateKind;
  LIndex, LCount, LRowIndex: Integer;
  LMode: TNyxCollectionCellMode;
  LInput: TNyxStudioStateInput;
  LTitle: TNyxText;
begin
  Result := False;

  if (ANode = nil) or (ASession = nil) or (ANode.Prop(CCommandKey) = '') then
  begin
    Exit;
  end;
  LFound := False;
  for LCommand := Low(TCollectionCommand) to High(TCollectionCommand) do
  begin

    if ANode.Prop(CCommandKey) = CCommands[LCommand] then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Unknown collection command');
  end;

  if ((LCommand in [ccDefault, ccCell, ccScope, ccTitle, ccMode, ccSelection]) and
    (AEvent <> ntChange)) or
    (not (LCommand in [ccDefault, ccCell, ccScope, ccTitle, ccMode, ccSelection]) and
    (AEvent <> ntClick)) then
  begin
    Exit;
  end;
  LKey := ANode.Prop(CKey);
  LField := ANode.Prop(CField);

  if LCommand = ccCreate then
  begin
    LIndex := 1;
    repeat
      LKey := 'collection' + IntToStr(LIndex);
      Inc(LIndex);
    until not ASession.Document.Collections.Has(NyxCollection(LKey));
    ASession.DefineCollection(NyxCollection(LKey), NyxCollectionSchema.Text(NyxTextField('caption'), ''), []);
    Exit(True);
  end;

  if LCommand = ccRemove then
  begin
    ASession.RemoveCollection(NyxCollection(LKey));
    Exit(True);
  end;

  if LCommand in [ccAddField, ccDefault, ccAddRow, ccRemoveRow, ccCell] then
  begin
    LData := ASession.Document.Collections.Snapshot(NyxCollection(LKey));
    LSchema := LData.Schema;
    SetLength(LItems, LData.Count);
    for LIndex := 0 to LData.Count - 1 do
    begin
      LItems[LIndex] := LData.ItemAt(LIndex);
    end;

    if LCommand = ccAddField then
    begin
      LFound := False;
      for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
      begin

        if ANode.Prop(CKind) = NyxStateKindName(LKind) then
        begin
          LFound := True;
          Break;
        end;
      end;

      if not LFound then
      begin
        raise ENyxCollection.Create('Unknown collection field type');
      end;
      LIndex := 1;
      repeat
        LName := NyxStateKindName(LKind) + IntToStr(LIndex);
        Inc(LIndex);
        LFound := False;
        for LCount := 0 to LSchema.Count - 1 do
        begin
          LFound := LFound or (LSchema.FieldAt(LCount).Name = LName);
        end;
      until not LFound;
      case LKind of
        nskText: LSchema := LSchema.Text(NyxTextField(LName), '');
        nskBoolean: LSchema := LSchema.Boolean(NyxBooleanField(LName), False);
        nskInteger: LSchema := LSchema.Integer(NyxIntegerField(LName), 0);
        nskNumber: LSchema := LSchema.Number(NyxNumberField(LName), 0);
      end;
    end
    else if LCommand = ccDefault then
    begin
      LNextSchema := NyxCollectionSchema;
      LFound := False;
      for LIndex := 0 to LSchema.Count - 1 do
      begin
        LFieldInfo := LSchema.FieldAt(LIndex);
        LValue := LFieldInfo.DefaultValue;

        if LFieldInfo.Name = LField then
        begin

          if not TryNyxStudioStateInput(ANode.Prop(NyxStudioStateInputKey), LInput) or
            (NyxStudioStateInputKind(LInput) <> LValue.Kind) then
          begin
            raise ENyxCollection.Create('Collection editor type no longer matches its field');
          end;
          LValue := ParseNyxStudioStateInput(LInput, ANode.Prop('value'));
          LFound := True;
        end;
        LNextSchema := LNextSchema.Field(LFieldInfo.Name, LValue, LFieldInfo.Domain);
      end;

      if not LFound then
      begin
        raise ENyxCollection.Create('Collection field no longer exists');
      end;
      LSchema := LNextSchema;
    end
    else if LCommand = ccAddRow then
    begin
      LIndex := 1;
      repeat
        LName := 'row' + IntToStr(LIndex);
        Inc(LIndex);
      until LData.IndexOf(NyxItem(NyxCollection(LKey), LName)) < 0;
      SetLength(LItems, LData.Count + 1);
      LItems[LData.Count] := NyxCollectionItem(NyxItem(NyxCollection(LKey), LName));
    end
    else
    begin
      LRowIndex := LData.IndexOf(NyxItem(NyxCollection(LKey), ANode.Prop(CRow)));

      if LRowIndex < 0 then
      begin
        raise ENyxCollection.Create('Collection row no longer exists');
      end;

      if LCommand = ccRemoveRow then
      begin
        for LIndex := LRowIndex to Length(LItems) - 2 do
        begin
          LItems[LIndex] := LItems[LIndex + 1];
        end;
        SetLength(LItems, Length(LItems) - 1);
      end
      else
      begin
        LFound := False;
        for LIndex := 0 to LSchema.Count - 1 do
        begin

          if LSchema.FieldAt(LIndex).Name = LField then
          begin

            if not TryNyxStudioStateInput(ANode.Prop(NyxStudioStateInputKey), LInput) or
              (NyxStudioStateInputKind(LInput) <> LSchema.FieldAt(LIndex).Kind) then
            begin
              raise ENyxCollection.Create('Collection editor type no longer matches its field');
            end;
            LValue := ParseNyxStudioStateInput(LInput, ANode.Prop('value'));
            LItems[LRowIndex] := PutValue(LItems[LRowIndex], LField, LValue);
            LFound := True;
            Break;
          end;
        end;

        if not LFound then
        begin
          raise ENyxCollection.Create('Collection field no longer exists');
        end;
      end;
    end;
    ASession.DefineCollection(NyxCollection(LKey), LSchema, LItems);
    Exit(True);
  end;

  if ANode.Prop(COwner) <> ASession.SelectedID then
  begin
    raise ENyxCollection.Create('Collection selection changed; use its current inspector');
  end;

  if LCommand = ccClear then
  begin
    ASession.SetCollectionView(Default(TNyxCollectionViewSpec));
    Exit(True);
  end;

  if LCommand = ccInherit then
  begin
    ASession.InheritCollectionView;
    Exit(True);
  end;
  LProjection := ASession.SelectedProjection;
  try

    if LProjection = nil then
    begin
      raise ENyxCollection.Create('Select a list, table or tree');
    end;
    LSpec := LProjection.CollectionView;

    if LCommand = ccBind then
    begin
      LSchema := ASession.Document.Collections.Snapshot(NyxCollection(LKey)).Schema;
      LSpec := NyxCollectionView(NyxCollection(LKey));
      for LIndex := 0 to LSchema.Count - 1 do
      begin
        LFieldInfo := LSchema.FieldAt(LIndex);
        LSpec := AddColumn(LSpec, LFieldInfo.Name, LFieldInfo.Name, LFieldInfo.Kind, cmReadOnly);
      end;
    end
    else
    begin

      if not LSpec.Defined or (LSpec.Key.Name <> LKey) then
      begin
        raise ENyxCollection.Create('Collection binding changed; use its current inspector');
      end;
      LNext := NyxCollectionView(LSpec.Key).Scoped(LSpec.Scope).Selection(LSpec.SelectionMode);

      if LSpec.ParentField <> '' then
      begin
        LNext := LNext.Parent(NyxTextField(LSpec.ParentField));
      end;
      for LIndex := 0 to LSpec.Count - 1 do
      begin
        LColumn := LSpec.ColumnAt(LIndex);

        if (LCommand = ccRemoveColumn) and (LColumn.FieldName = LField) then
        begin
          Continue;
        end;
        LTitle := LColumn.Title;
        LMode := LColumn.Mode;

        if LColumn.FieldName = LField then
        begin

          if LCommand = ccTitle then
          begin
            LTitle := ANode.Prop('value');
          end
          else if LCommand = ccMode then
          begin

            if ANode.Prop('value') = 'Editable' then
            begin
              LMode := cmEditable;
            end
            else if ANode.Prop('value') = 'Read only' then
            begin
              LMode := cmReadOnly;
            end
            else
            begin
              raise ENyxCollection.Create('Unknown column editability');
            end;
          end;
        end;
        LNext := AddColumn(LNext, LColumn.FieldName, LTitle, LColumn.Kind, LMode);
      end;

      if LCommand = ccSelection then
      begin

        if ANode.Prop('value') = 'Single item' then
        begin
          LNext := LNext.Selection(nsmSingle);
        end
        else if ANode.Prop('value') = 'Multiple items' then
        begin
          LNext := LNext.Selection(nsmMultiple);
        end
        else
        begin
          raise ENyxCollection.Create('Unknown selection mode');
        end;
      end
      else if LCommand = ccScope then
      begin

        if ANode.Prop('value') = 'Application' then
        begin
          LNext := LNext.Scoped(csApplication);
        end
        else if ANode.Prop('value') = 'Reusable instance' then
        begin
          LNext := LNext.Scoped(csInstance);
        end
        else
        begin
          raise ENyxCollection.Create('Unknown collection scope');
        end;
      end
      else if LCommand = ccParent then
      begin

        if LField = '' then
        begin
          { Rebuild without Parent; an uninitialized text reference is invalid. }
          LNext := NyxCollectionView(LSpec.Key).Scoped(LSpec.Scope).Selection(LSpec.SelectionMode);
          for LIndex := 0 to LSpec.Count - 1 do
          begin
            LColumn := LSpec.ColumnAt(LIndex);
            LNext := AddColumn(LNext, LColumn.FieldName, LColumn.Title, LColumn.Kind, LColumn.Mode);
          end;
        end
        else
        begin
          LNext := LNext.Parent(NyxTextField(LField));
        end;
      end
      else if LCommand = ccAddColumn then
      begin
        LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;
        LFound := False;
        for LIndex := 0 to LSchema.Count - 1 do
        begin

          if LSchema.FieldAt(LIndex).Name = LField then
          begin
            LFieldInfo := LSchema.FieldAt(LIndex);
            LNext := AddColumn(LNext, LField, LField, LFieldInfo.Kind, cmReadOnly);
            LFound := True;
            Break;
          end;
        end;

        if not LFound then
        begin
          raise ENyxCollection.Create('Collection field no longer exists');
        end;
      end;

      if LNext.Count = 0 then
      begin
        LNext := Default(TNyxCollectionViewSpec);
      end;
      LSpec := LNext;
    end;
    ASession.SetCollectionView(LSpec);
    Result := True;
  finally
    LProjection.Free;
  end;
end;

end.
