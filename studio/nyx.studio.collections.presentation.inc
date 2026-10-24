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

{ Included by nyx.studio.collections. These ordinary Nyx controls participate in
  the existing exact-baseline layout draft owner and paired source command queue.
  Text layout is independent of display formatting and accepted cell contents. }
const
  CPresentationAction = 'studio-presentation-policy-action';
  CPresentationColumn = 'studio-presentation-policy-column';
  CPresentationFields: array[TNyxStudioPresentationPolicyField] of TNyxText =
    ('density', 'overflow', 'maximum-lines');
  CPresentationDensities: array[TNyxTableRowDensity] of TNyxText =
    ('Compact', 'Normal', 'Comfortable');
  CPresentationOverflows: array[TNyxColumnTextOverflow] of TNyxText =
    ('Clip', 'Ellipsis', 'Wrap');

function NyxStudioPresentationPolicyFormID(AColumn: Integer): TNyxText;
begin

  if (AColumn < -1) or (AColumn >= NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Unknown presentation form column');
  end;
  Result := NyxStudioLayoutEditorID + '-presentation-' + TNyxText(IntToStr(AColumn));
end;

function NyxStudioPresentationPolicyFieldID(AColumn: Integer;
  AField: TNyxStudioPresentationPolicyField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxStudioPresentationPolicyField))) or
    (Ord(AField) > Ord(High(TNyxStudioPresentationPolicyField))) or
    ((AColumn = -1) <> (AField = sppfDensity)) then
  begin
    raise ENyxCollection.Create('Unknown presentation form role');
  end;
  Result := NyxStudioPresentationPolicyFormID(AColumn) + '-' + CPresentationFields[AField];
end;

function NyxStudioPresentationPolicyApplyID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioPresentationPolicyFormID(AColumn) + '-apply';
end;

function NyxStudioPresentationPolicyDefaultID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioPresentationPolicyFormID(AColumn) + '-default';
end;

procedure AddPresentationPolicyForms(AEditor: TNyxNode; const AOwner: TNyxText;
  const ASpec: TNyxCollectionViewSpec; ALocked: Boolean);
var
  LForm: INyxCard;
  LSection: INyxDisclosure;
  LColumn: TNyxCollectionColumn;
  LTablePolicy: TNyxTablePresentationPolicy;
  LTextPolicy: TNyxColumnTextPolicy;
  LIndex: Integer;

  procedure BeginForm(AColumn: Integer; const ATitle: TNyxText; ADeclared: Boolean);
  var
    LStatus: TNyxText;
  begin
    LSection := NewNyxDisclosure(NyxStudioPresentationPolicyFormID(AColumn) + '-section')
      .WithText(ATitle);
    LSection.Configure.Value(False).Surface(False).Done;
    LForm := NewNyxCard(NyxStudioPresentationPolicyFormID(AColumn));
    LForm.Configure.Padding(8).Gap(6).Surface(True).Done;
    LForm.Node.SetProp(COwner, AOwner).SetProp(CKey, ASpec.Key.Name)
      .SetProp(CPolicyBaseline, AEditor.Prop(NyxMenuFormBaselineKey))
      .SetProp(CPresentationColumn, TNyxText(IntToStr(AColumn)));
    LForm.Node.Add(NewNyxHeading(LForm.Node.ID + '-title').Configure.Text(ATitle).Done);
    LStatus := 'Using existing host defaults. Apply declares this presentation explicitly.';

    if ADeclared then
    begin
      LStatus := 'Explicit choices. Use Nyx defaults removes only this presentation policy.';
    end;
    LForm.Node.Add(NewNyxLabel(LForm.Node.ID + '-status').Configure.Text(LStatus).Done);
  end;

  procedure EndForm(AColumn: Integer);
  begin
    LForm.Node.Add(NewNyxButton(NyxStudioPresentationPolicyApplyID(AColumn)).Configure
      .Text('Apply presentation').Enabled(not ALocked)
      .Extension(CPresentationAction, CPolicyApply).Done);
    LForm.Node.Add(NewNyxButton(NyxStudioPresentationPolicyDefaultID(AColumn)).Configure
      .Text('Use Nyx defaults').Enabled(not ALocked)
      .Extension(CPresentationAction, CPolicyDefault).Done);
    LSection.BodyColumn.Node.Add(LForm);
    AEditor.Add(LSection);
  end;

begin
  LTablePolicy := NyxTablePresentation;

  if ASpec.HasTablePresentationPolicy then
  begin
    LTablePolicy := ASpec.TablePresentationPolicy;
  end;
  BeginForm(-1, 'Body row spacing', ASpec.HasTablePresentationPolicy);
  LForm.Node.Add(NewNyxSelect(NyxStudioPresentationPolicyFieldID(-1, sppfDensity)).Configure
    .Text('Row density').Items('Compact' + #10 + 'Normal' + #10 + 'Comfortable')
    .Value(CPresentationDensities[LTablePolicy.RowDensity]).Enabled(not ALocked).Done);
  EndForm(-1);
  for LIndex := 0 to ASpec.Count - 1 do
  begin
    LColumn := ASpec.ColumnAt(LIndex);
    LTextPolicy := NyxColumnText;

    if LColumn.HasTextPolicy then
    begin
      LTextPolicy := LColumn.TextPolicy;
    end;
    BeginForm(LIndex, LColumn.Title + ' text layout', LColumn.HasTextPolicy);
    LForm.Node.SetProp(CField, LColumn.FieldName).SetProp(CKind, NyxStateKindName(LColumn.Kind));
    LForm.Node.Add(NewNyxSelect(NyxStudioPresentationPolicyFieldID(LIndex, sppfTextOverflow))
      .Configure.Text('Text overflow').Items('Clip' + #10 + 'Ellipsis' + #10 + 'Wrap')
      .Value(CPresentationOverflows[LTextPolicy.Overflow]).Enabled(not ALocked).Done);
    { A text editor intentionally preserves unfinished numeric notation. Only
      Wrap Apply parses it; single-line choices and reset ignore the inactive
      draft. Runtime measurement limits are independently configurable by hosts. }
    LForm.Node.Add(NewNyxInput(NyxStudioPresentationPolicyFieldID(LIndex, sppfMaximumLines))
      .Configure.Text('Maximum lines when wrapping')
      .Value(TNyxText(IntToStr(LTextPolicy.MaximumLines))).Enabled(not ALocked).Done);
    EndForm(LIndex);
  end;
end;

function CaptureNyxStudioPresentation(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LEditor: TNyxNode;
  LForm: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LColumn: TNyxCollectionColumn;
  LColumnIndex: Integer;
  LFieldIndex: Integer;
  LLines: Integer;
  LOwner: TNyxText;
  LBaseline: TNyxText;
  LAction: TNyxText;
  LValue: TNyxText;
  LDensity: TNyxTableRowDensity;
  LFound: Boolean;

  function Role(const AID: TNyxText; AKind: TNyxKind): TNyxNode;
  begin
    Result := LForm.Find(AID);

    if (Result = nil) or (Result.Parent <> LForm) or
      (Result.Kind <> NyxKindName(AKind)) or (Result.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Use the complete current typed presentation form');
    end;
  end;

  function Value(AField: TNyxStudioPresentationPolicyField): TNyxText;
  var
    LKind: TNyxKind;
  begin
    LKind := nkSelect;

    if AField = sppfMaximumLines then
    begin
      LKind := nkInput;
    end;
    Result := Role(NyxStudioPresentationPolicyFieldID(LColumnIndex, AField), LKind)
      .Prop(NyxAttributeName(atValue));
  end;

begin
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and (AButton.Prop(CPresentationAction) <> '');

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or (AButton.Parent = nil) or
    (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxCollection.Create('Use the current mounted presentation action');
  end;
  LEditor := AShellRoot.Find(NyxStudioLayoutEditorID);
  LForm := AButton.Parent;

  if (LEditor = nil) or (LEditor.Find(LForm.ID) <> LForm) or
    (LEditor.Kind <> NyxKindName(nkCard)) or (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (LForm.Kind <> NyxKindName(nkCard)) or (LForm.ProjectionKind <> NyxKindName(nkCard)) or
    not TryNyxStateInteger(LForm.Prop(CPresentationColumn), LColumnIndex) or
    (LColumnIndex < -1) or (LColumnIndex >= NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Presentation action has no current table or column form');
  end;

  if (LForm.ID <> NyxStudioPresentationPolicyFormID(LColumnIndex)) or
    (LForm.Prop(CPresentationColumn) <> TNyxText(IntToStr(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Presentation form identity changed');
  end;
  LAction := AButton.Prop(CPresentationAction);

  if ((LAction <> CPolicyApply) and (LAction <> CPolicyDefault)) or
    ((LAction = CPolicyApply) and (AButton.ID <> NyxStudioPresentationPolicyApplyID(LColumnIndex))) or
    ((LAction = CPolicyDefault) and (AButton.ID <> NyxStudioPresentationPolicyDefaultID(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Unknown presentation action');
  end;
  Role(NyxStudioPresentationPolicyApplyID(LColumnIndex), nkButton);
  Role(NyxStudioPresentationPolicyDefaultID(LColumnIndex), nkButton);
  Role(LForm.ID + '-title', nkHeading);
  Role(LForm.ID + '-status', nkLabel);

  if LColumnIndex < 0 then
  begin
    Value(sppfDensity);

    if LForm.Count <> 5 then
    begin
      raise ENyxCollection.Create('Use the complete body spacing form');
    end;
  end
  else
  begin
    Value(sppfTextOverflow);
    Value(sppfMaximumLines);

    if LForm.Count <> 6 then
    begin
      raise ENyxCollection.Create('Use the complete column text form');
    end;
  end;
  LOwner := LForm.Prop(COwner);
  LBaseline := LForm.Prop(CPolicyBaseline);

  if (LOwner = '') or (ASession.SelectedID <> LOwner) or (LBaseline = '') or
    (LEditor.Prop(NyxMenuFormOwnerKey) <> LOwner) or
    (LEditor.Prop(NyxMenuFormBaselineKey) <> LBaseline) or
    APending.CollectionViewLocked(LOwner) or ASession.SourceDraftPending then
  begin
    raise ENyxCollection.Create('Select this table again after pending edits or Pascal drafts');
  end;
  LProjection := ASession.SelectedProjection;
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkTable)) or
      not LProjection.HasCollectionView or not LProjection.CollectionView.Defined then
    begin
      raise ENyxCollection.Create('Presentation defaults require the selected bound table');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if (LColumnIndex >= LSpec.Count) or (LForm.Prop(CKey) <> LSpec.Key.Name) or
      APending.CollectionLocked(LSpec.Key) or
      (NyxQueryEditorBaseline(LSchema, LSpec) <> LBaseline) then
    begin
      raise ENyxCollection.Create('Table binding or schema changed; reopen its presentation form');
    end;
    AEdit.Action := sdaCollection;
    AEdit.Selection := LOwner;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Projection := cpTable;
    AEdit.Collection.LayoutBaseline := LBaseline;

    if LColumnIndex < 0 then
    begin
      AEdit.Collection.Action := scaDefaultTablePresentation;

      if LAction = CPolicyApply then
      begin
        LValue := Value(sppfDensity);
        LFound := False;
        for LDensity := Low(TNyxTableRowDensity) to High(TNyxTableRowDensity) do
        begin

          if LValue = CPresentationDensities[LDensity] then
          begin
            LFound := True;
            AEdit.Collection.TablePresentation := NyxTablePresentation.Density(LDensity);
            Break;
          end;
        end;

        if not LFound then
        begin
          raise ENyxCollection.Create('Choose an exact body row spacing');
        end;
        AEdit.Collection.Action := scaTablePresentation;
      end;
    end
    else
    begin
      LColumn := LSpec.ColumnAt(LColumnIndex);

      if (LColumn.FieldName <> LForm.Prop(CField)) or
        (NyxStateKindName(LColumn.Kind) <> LForm.Prop(CKind)) then
      begin
        raise ENyxCollection.Create('Presentation column changed its full typed identity');
      end;
      for LFieldIndex := 0 to LSchema.Count - 1 do
      begin

        if (LSchema.FieldAt(LFieldIndex).Name = LColumn.FieldName) and
          (LSchema.FieldAt(LFieldIndex).Kind = LColumn.Kind) then
        begin
          AEdit.Collection.Field := TNyxStudioCollectionFieldRef.FromMetadata(LSchema.FieldAt(LFieldIndex));
          Break;
        end;
      end;

      if not AEdit.Collection.Field.Defined then
      begin
        raise ENyxCollection.Create('Presentation column no longer has its exact schema field');
      end;
      AEdit.Collection.Action := scaDefaultColumnText;

      if LAction = CPolicyApply then
      begin
        LValue := Value(sppfTextOverflow);
        AEdit.Collection.ColumnText := NyxColumnText;

        if LValue = CPresentationOverflows[nctoEllipsis] then
        begin
          AEdit.Collection.ColumnText := NyxColumnText.Ellipsis;
        end
        else if LValue = CPresentationOverflows[nctoWrap] then
        begin

          if not TryNyxStateInteger(Value(sppfMaximumLines), LLines) or (LLines < 1) then
          begin
            raise ENyxCollection.Create('Wrapping needs a positive whole maximum line count');
          end;
          AEdit.Collection.ColumnText := NyxColumnText.Wrap(LLines);
        end
        else if LValue <> CPresentationOverflows[nctoClip] then
        begin
          raise ENyxCollection.Create('Choose an exact text overflow mode');
        end;
        AEdit.Collection.Action := scaColumnText;
      end;
    end;
    ApplyNyxStudioCollectionViewIntent(AEdit.Collection, LSpec, LSchema);
  finally
    LProjection.Free;
  end;
end;
