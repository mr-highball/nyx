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

{ Included by nyx.studio.collections. Alignment uses ordinary Nyx controls and
  the existing retained layout draft/baseline owner, never a target widget API. }
const
  CAlignmentAction = 'studio-column-alignment-action';
  CAlignmentColumn = 'studio-column-alignment-column';
  CAlignmentInherited = 'Use host default';
  CAlignmentFields: array[TNyxStudioColumnAlignmentField] of TNyxText =
    ('horizontal', 'vertical');
  CAlignmentHorizontal: array[TNyxColumnHorizontalAlignment] of TNyxText =
    ('Start', 'Center', 'End');
  CAlignmentVertical: array[TNyxColumnVerticalAlignment] of TNyxText =
    ('Top', 'Middle', 'Bottom');

function NyxStudioColumnAlignmentFormID(AColumn: Integer): TNyxText;
begin

  if (AColumn < 0) or (AColumn >= NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Unknown alignment column');
  end;
  Result := NyxStudioLayoutEditorID + '-alignment-' + TNyxText(IntToStr(AColumn));
end;

function NyxStudioColumnAlignmentFieldID(AColumn: Integer;
  AField: TNyxStudioColumnAlignmentField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxStudioColumnAlignmentField))) or
    (Ord(AField) > Ord(High(TNyxStudioColumnAlignmentField))) then
  begin
    raise ENyxCollection.Create('Unknown alignment field role');
  end;
  Result := NyxStudioColumnAlignmentFormID(AColumn) + '-' + CAlignmentFields[AField];
end;

function NyxStudioColumnAlignmentApplyID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioColumnAlignmentFormID(AColumn) + '-apply';
end;

function NyxStudioColumnAlignmentDefaultID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioColumnAlignmentFormID(AColumn) + '-default';
end;

procedure AddColumnAlignmentForms(AEditor: TNyxNode; const AOwner: TNyxText;
  const ASpec: TNyxCollectionViewSpec; ALocked: Boolean);
var
  LSection: INyxDisclosure;
  LForm: INyxCard;
  LColumn: TNyxCollectionColumn;
  LPolicy: TNyxColumnAlignmentPolicy;
  LHorizontal: TNyxText;
  LVertical: TNyxText;
  LIndex: Integer;
begin
  for LIndex := 0 to ASpec.Count - 1 do
  begin
    LColumn := ASpec.ColumnAt(LIndex);
    LPolicy := LColumn.AlignmentPolicy;
    LHorizontal := CAlignmentInherited;
    LVertical := CAlignmentInherited;

    if LPolicy.HasHorizontal then
    begin
      LHorizontal := CAlignmentHorizontal[LPolicy.HorizontalAlignment];
    end;

    if LPolicy.HasVertical then
    begin
      LVertical := CAlignmentVertical[LPolicy.VerticalAlignment];
    end;
    LSection := NewNyxDisclosure(NyxStudioColumnAlignmentFormID(LIndex) + '-section')
      .WithText(LColumn.Title + ' alignment');
    LSection.Configure.Value(False).Surface(False).Done;
    LForm := NewNyxCard(NyxStudioColumnAlignmentFormID(LIndex));
    LForm.Configure.Padding(8).Gap(6).Surface(True).Done;
    LForm.Node.SetProp(COwner, AOwner).SetProp(CKey, ASpec.Key.Name)
      .SetProp(CPolicyBaseline, AEditor.Prop(NyxMenuFormBaselineKey))
      .SetProp(CAlignmentColumn, TNyxText(IntToStr(LIndex)))
      .SetProp(CField, LColumn.FieldName).SetProp(CKind, NyxStateKindName(LColumn.Kind));
    LForm.Node.Add(NewNyxHeading(LForm.Node.ID + '-title').WithText(LColumn.Title + ' alignment'));
    LForm.Node.Add(NewNyxLabel(LForm.Node.ID + '-status').WithText(
      'Start and End follow the reading direction. Unspecified axes keep host defaults.'));
    LForm.Node.Add(NewNyxSelect(NyxStudioColumnAlignmentFieldID(LIndex, scafHorizontal))
      .Configure.Text('Inline alignment')
      .Items(CAlignmentInherited + #10 + 'Start' + #10 + 'Center' + #10 + 'End')
      .Value(LHorizontal).Enabled(not ALocked).Done);
    LForm.Node.Add(NewNyxSelect(NyxStudioColumnAlignmentFieldID(LIndex, scafVertical))
      .Configure.Text('Vertical alignment')
      .Items(CAlignmentInherited + #10 + 'Top' + #10 + 'Middle' + #10 + 'Bottom')
      .Value(LVertical).Enabled(not ALocked).Done);
    LForm.Node.Add(NewNyxButton(NyxStudioColumnAlignmentApplyID(LIndex)).Configure
      .Text('Apply alignment').Enabled(not ALocked).Extension(CAlignmentAction, CPolicyApply).Done);
    LForm.Node.Add(NewNyxButton(NyxStudioColumnAlignmentDefaultID(LIndex)).Configure
      .Text('Use Nyx defaults').Enabled(not ALocked).Extension(CAlignmentAction, CPolicyDefault).Done);
    LSection.BodyColumn.Add(LForm);
    AEditor.Add(LSection);
  end;
end;

function CaptureNyxStudioColumnAlignment(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LEditor: TNyxNode;
  LForm: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LColumn: TNyxCollectionColumn;
  LPolicy: TNyxColumnAlignmentPolicy;
  LColumnIndex: Integer;
  LFieldIndex: Integer;
  LOwner: TNyxText;
  LBaseline: TNyxText;
  LAction: TNyxText;
  LHorizontal: TNyxText;
  LVertical: TNyxText;
  LHorizontalChoice: TNyxColumnHorizontalAlignment;
  LVerticalChoice: TNyxColumnVerticalAlignment;
  LFound: Boolean;

  function Role(const AID: TNyxText; AKind: TNyxKind): TNyxNode;
  begin
    Result := LForm.Find(AID);

    if (Result = nil) or (Result.Parent <> LForm) or
      (Result.Kind <> NyxKindName(AKind)) or (Result.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Use the complete current typed alignment form');
    end;
  end;

begin
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and (AButton.Prop(CAlignmentAction) <> '');

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or (AButton.Parent = nil) or
    (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxCollection.Create('Use the current mounted alignment action');
  end;
  LEditor := AShellRoot.Find(NyxStudioLayoutEditorID);
  LForm := AButton.Parent;

  if (LEditor = nil) or (LEditor.Find(LForm.ID) <> LForm) or
    (LEditor.Kind <> NyxKindName(nkCard)) or (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (LForm.Kind <> NyxKindName(nkCard)) or (LForm.ProjectionKind <> NyxKindName(nkCard)) or
    not TryNyxStateInteger(LForm.Prop(CAlignmentColumn), LColumnIndex) or
    (LColumnIndex < 0) or (LColumnIndex >= NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Alignment action has no current column form');
  end;

  if (LForm.ID <> NyxStudioColumnAlignmentFormID(LColumnIndex)) or
    (LForm.Prop(CAlignmentColumn) <> TNyxText(IntToStr(LColumnIndex))) or (LForm.Count <> 6) then
  begin
    raise ENyxCollection.Create('Alignment form identity or complete roles changed');
  end;
  LAction := AButton.Prop(CAlignmentAction);

  if ((LAction <> CPolicyApply) and (LAction <> CPolicyDefault)) or
    ((LAction = CPolicyApply) and (AButton.ID <> NyxStudioColumnAlignmentApplyID(LColumnIndex))) or
    ((LAction = CPolicyDefault) and (AButton.ID <> NyxStudioColumnAlignmentDefaultID(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Unknown alignment action');
  end;
  Role(NyxStudioColumnAlignmentApplyID(LColumnIndex), nkButton);
  Role(NyxStudioColumnAlignmentDefaultID(LColumnIndex), nkButton);
  Role(LForm.ID + '-title', nkHeading);
  Role(LForm.ID + '-status', nkLabel);
  LHorizontal := Role(NyxStudioColumnAlignmentFieldID(LColumnIndex, scafHorizontal), nkSelect)
    .Prop(NyxAttributeName(atValue));
  LVertical := Role(NyxStudioColumnAlignmentFieldID(LColumnIndex, scafVertical), nkSelect)
    .Prop(NyxAttributeName(atValue));
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
      raise ENyxCollection.Create('Column alignment requires the selected bound table');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if (LColumnIndex >= LSpec.Count) or (LForm.Prop(CKey) <> LSpec.Key.Name) or
      APending.CollectionLocked(LSpec.Key) or
      (NyxQueryEditorBaseline(LSchema, LSpec) <> LBaseline) then
    begin
      raise ENyxCollection.Create('Table binding or schema changed; reopen alignment');
    end;
    LColumn := LSpec.ColumnAt(LColumnIndex);

    if (LColumn.FieldName <> LForm.Prop(CField)) or
      (NyxStateKindName(LColumn.Kind) <> LForm.Prop(CKind)) then
    begin
      raise ENyxCollection.Create('Alignment column changed its full typed identity');
    end;
    AEdit.Action := sdaCollection;
    AEdit.Selection := LOwner;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Action := scaDefaultColumnAlignment;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Projection := cpTable;
    AEdit.Collection.LayoutBaseline := LBaseline;
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
      raise ENyxCollection.Create('Alignment column no longer has its exact schema field');
    end;

    if LAction = CPolicyApply then
    begin
      LPolicy := NyxColumnAlignment;
      LFound := LHorizontal = CAlignmentInherited;
      for LHorizontalChoice := Low(TNyxColumnHorizontalAlignment) to High(TNyxColumnHorizontalAlignment) do
      begin

        if LHorizontal = CAlignmentHorizontal[LHorizontalChoice] then
        begin
          LPolicy := LPolicy.Horizontal(LHorizontalChoice);
          LFound := True;
          Break;
        end;
      end;

      if not LFound then
      begin
        raise ENyxCollection.Create('Choose an exact inline alignment or host default');
      end;
      LFound := LVertical = CAlignmentInherited;
      for LVerticalChoice := Low(TNyxColumnVerticalAlignment) to High(TNyxColumnVerticalAlignment) do
      begin

        if LVertical = CAlignmentVertical[LVerticalChoice] then
        begin
          LPolicy := LPolicy.Vertical(LVerticalChoice);
          LFound := True;
          Break;
        end;
      end;

      if not LFound then
      begin
        raise ENyxCollection.Create('Choose an exact vertical alignment or host default');
      end;

      if LPolicy.Defined then
      begin
        AEdit.Collection.Action := scaColumnAlignment;
        AEdit.Collection.ColumnAlignment := LPolicy;
      end;
    end;
    ApplyNyxStudioCollectionViewIntent(AEdit.Collection, LSpec, LSchema);
  finally
    LProjection.Free;
  end;
end;
