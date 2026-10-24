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

{ Included by nyx.studio.collections. This is ordinary Nyx UI, sharing the
  existing retained layout draft owner. Open controls and their transport text
  are disposable; only the final typed intent enters source/history. }
const
  CWorkbenchAction = 'studio-workbench-policy-action';
  CWorkbenchFields: array[TNyxStudioWorkbenchPolicyField] of TNyxText =
    ('range-selection', 'copy-cell', 'copy-source', 'copy-page', 'copy-range',
     'copy-fields', 'headers', 'line-ending', 'focused-draft');
  CWorkbenchFieldChoices: array[TNyxWorkbenchCopyFields] of TNyxText =
    ('Visible columns', 'All authored columns, including hidden');
  CWorkbenchLineChoices: array[TNyxWorkbenchLineEnding] of TNyxText =
    ('LF', 'CRLF');

function NyxStudioWorkbenchPolicyFormID: TNyxText;
begin
  Result := NyxStudioLayoutEditorID + '-workbench';
end;

function NyxStudioWorkbenchPolicyFieldID(AField: TNyxStudioWorkbenchPolicyField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxStudioWorkbenchPolicyField))) or
    (Ord(AField) > Ord(High(TNyxStudioWorkbenchPolicyField))) then
  begin
    raise ENyxCollection.Create('Unknown workbench form field');
  end;
  Result := NyxStudioWorkbenchPolicyFormID + '-' + CWorkbenchFields[AField];
end;

function NyxStudioWorkbenchPolicyApplyID: TNyxText;
begin
  Result := NyxStudioWorkbenchPolicyFormID + '-apply';
end;

function NyxStudioWorkbenchPolicyDefaultID: TNyxText;
begin
  Result := NyxStudioWorkbenchPolicyFormID + '-default';
end;

procedure AddWorkbenchPolicyForm(AEditor: TNyxNode; const AOwner: TNyxText;
  const ASpec: TNyxCollectionViewSpec; ALocked: Boolean);
var
  LForm: INyxCard;
  LSection: INyxDisclosure;
  LPolicy: TNyxTableWorkbenchPolicy;
  LStatus: TNyxText;

  procedure Flag(AField: TNyxStudioWorkbenchPolicyField;
    const ACaption: TNyxText; AValue: Boolean);
  begin
    LForm.Node.Add(NewNyxCheckbox(NyxStudioWorkbenchPolicyFieldID(AField)).Configure
      .Text(ACaption).Value(AValue).Enabled(not ALocked).Done);
  end;

begin
  LPolicy := ASpec.TableWorkbenchPolicy;
  LSection := NewNyxDisclosure(NyxStudioWorkbenchPolicyFormID + '-section')
    .WithText('Inspection, selection and copy');
  LSection.Configure.Value(False).Surface(False).Done;
  LForm := NewNyxCard(NyxStudioWorkbenchPolicyFormID);
  LForm.Configure.Padding(8).Gap(6).Surface(True).Done;
  LForm.Node.SetProp(COwner, AOwner).SetProp(CKey, ASpec.Key.Name)
    .SetProp(CPolicyBaseline, AEditor.Prop(NyxMenuFormBaselineKey));
  LForm.Node.Add(NewNyxHeading(LForm.Node.ID + '-title').Configure
    .Text('Workbench actions').Done);
  LStatus := 'No workbench policy is declared. Apply enables these authored choices.';

  if ASpec.HasTableWorkbenchPolicy then
  begin
    LStatus := 'Explicit choices. Use Nyx defaults removes only this action policy.';
  end;
  LForm.Node.Add(NewNyxLabel(LForm.Node.ID + '-status').Configure.Text(LStatus).Done);
  LForm.Node.Add(NewNyxLabel(LForm.Node.ID + '-help').Configure.Text(
    'Inspection remains available when copying is disabled. Range selection and copying ' +
    'are independent. Clipboard access is supplied separately by the application.').Done);
  Flag(swpfRangeSelection, 'Enable cell-range selection', LPolicy.RangeSelectionEnabled);
  Flag(swpfCopyCell, 'Allow accepted, displayed and explicit draft cell copy',
    LPolicy.AllowsCopy(nwcaCell));
  Flag(swpfCopySelectedSource, 'Allow selected rows from the complete source',
    LPolicy.AllowsCopy(nwcaSelectedSource));
  Flag(swpfCopySelectedPage, 'Allow selected rows on the current page',
    LPolicy.AllowsCopy(nwcaSelectedPage));
  Flag(swpfCopyCellRange, 'Allow selected-cell range copy', LPolicy.AllowsCopy(nwcaCellRange));
  LForm.Node.Add(NewNyxSelect(NyxStudioWorkbenchPolicyFieldID(swpfCopyFields)).Configure
    .Text('Columns included in row copy')
    .Items(CWorkbenchFieldChoices[nwcfVisiblePresentation] + #10 +
      CWorkbenchFieldChoices[nwcfAllArrangement])
    .Value(CWorkbenchFieldChoices[LPolicy.FieldChoice]).Enabled(not ALocked).Done);
  Flag(swpfHeaders, 'Include column headers in row copy', LPolicy.IncludeHeaders);
  LForm.Node.Add(NewNyxSelect(NyxStudioWorkbenchPolicyFieldID(swpfLineEnding)).Configure
    .Text('TSV row separator').Items(CWorkbenchLineChoices[nwleLF] + #10 +
      CWorkbenchLineChoices[nwleCRLF]).Value(CWorkbenchLineChoices[LPolicy.Lines])
    .Enabled(not ALocked).Done);
  Flag(swpfFocusedDraft, 'Allow deliberate inspection of unfinished cell text',
    LPolicy.HasFocusedDraft);
  LForm.Node.Add(NewNyxButton(NyxStudioWorkbenchPolicyApplyID).Configure
    .Text('Apply actions').Enabled(not ALocked).Extension(CWorkbenchAction, CPolicyApply).Done);
  LForm.Node.Add(NewNyxButton(NyxStudioWorkbenchPolicyDefaultID).Configure
    .Text('Use Nyx defaults').Enabled(not ALocked).Extension(CWorkbenchAction, CPolicyDefault).Done);
  LSection.BodyColumn.Node.Add(LForm);
  AEditor.Add(LSection);
end;

function CaptureNyxStudioWorkbench(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LForm: TNyxNode;
  LEditor: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LRole: TNyxStudioWorkbenchPolicyField;
  LOwner: TNyxText;
  LBaseline: TNyxText;
  LAction: TNyxText;
  LValue: TNyxText;
  LCopy: TNyxWorkbenchCopyActions;
  LFields: TNyxWorkbenchCopyFields;
  LLines: TNyxWorkbenchLineEnding;

  function Role(AField: TNyxStudioWorkbenchPolicyField): TNyxNode;
  var
    LKind: TNyxKind;
  begin
    LKind := nkCheckbox;

    if AField in [swpfCopyFields, swpfLineEnding] then
    begin
      LKind := nkSelect;
    end;
    Result := LForm.Find(NyxStudioWorkbenchPolicyFieldID(AField));

    if (Result = nil) or (Result.Parent <> LForm) or
      (Result.Kind <> NyxKindName(LKind)) or (Result.ProjectionKind <> NyxKindName(LKind)) then
    begin
      raise ENyxCollection.Create('Use the complete current typed workbench form');
    end;
  end;

  procedure RequirePart(const AID: TNyxText; AKind: TNyxKind);
  var
    LNode: TNyxNode;
  begin
    LNode := LForm.Find(AID);

    if (LNode = nil) or (LNode.Parent <> LForm) or
      (LNode.Kind <> NyxKindName(AKind)) or (LNode.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Use the original workbench form and actions');
    end;
  end;

  function Flag(AField: TNyxStudioWorkbenchPolicyField): Boolean;
  var
    LText: TNyxText;
  begin
    LText := Role(AField).Prop(NyxAttributeName(atValue));

    if (LText <> 'true') and (LText <> 'false') then
    begin
      raise ENyxCollection.Create('Workbench Boolean choices require true or false');
    end;
    Result := LText = 'true';
  end;

begin
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and (AButton.Prop(CWorkbenchAction) <> '');

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or (AButton.Parent = nil) or
    (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxCollection.Create('Use the current mounted workbench action');
  end;
  LEditor := AShellRoot.Find(NyxStudioLayoutEditorID);
  LForm := AButton.Parent;

  if (LEditor = nil) or (LEditor.Kind <> NyxKindName(nkCard)) or
    (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (LForm.ID <> NyxStudioWorkbenchPolicyFormID) or
    (LEditor.Find(LForm.ID) <> LForm) or (LForm.Kind <> NyxKindName(nkCard)) or
    (LForm.ProjectionKind <> NyxKindName(nkCard)) or (LForm.Count <> 14) then
  begin
    raise ENyxCollection.Create('Use the complete current table workbench form');
  end;
  LAction := AButton.Prop(CWorkbenchAction);

  if ((LAction <> CPolicyApply) and (LAction <> CPolicyDefault)) or
    ((LAction = CPolicyApply) and (AButton.ID <> NyxStudioWorkbenchPolicyApplyID)) or
    ((LAction = CPolicyDefault) and (AButton.ID <> NyxStudioWorkbenchPolicyDefaultID)) then
  begin
    raise ENyxCollection.Create('Unknown workbench action');
  end;
  RequirePart(NyxStudioWorkbenchPolicyApplyID, nkButton);
  RequirePart(NyxStudioWorkbenchPolicyDefaultID, nkButton);
  RequirePart(LForm.ID + '-title', nkHeading);
  RequirePart(LForm.ID + '-status', nkLabel);
  RequirePart(LForm.ID + '-help', nkLabel);
  for LRole := Low(TNyxStudioWorkbenchPolicyField) to High(TNyxStudioWorkbenchPolicyField) do
  begin
    Role(LRole);
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
      raise ENyxCollection.Create('Workbench defaults require the selected bound table');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if (LForm.Prop(CKey) <> LSpec.Key.Name) or APending.CollectionLocked(LSpec.Key) or
      (NyxQueryEditorBaseline(LSchema, LSpec) <> LBaseline) then
    begin
      raise ENyxCollection.Create('Table binding or schema changed; reopen its workbench form');
    end;
    AEdit.Action := sdaCollection;
    AEdit.Selection := LOwner;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Projection := cpTable;
    AEdit.Collection.LayoutBaseline := LBaseline;
    AEdit.Collection.Action := scaDefaultTableWorkbench;

    if LAction = CPolicyApply then
    begin
      LCopy := [];

      if Flag(swpfCopyCell) then
      begin
        Include(LCopy, nwcaCell);
      end;

      if Flag(swpfCopySelectedSource) then
      begin
        Include(LCopy, nwcaSelectedSource);
      end;

      if Flag(swpfCopySelectedPage) then
      begin
        Include(LCopy, nwcaSelectedPage);
      end;

      if Flag(swpfCopyCellRange) then
      begin
        Include(LCopy, nwcaCellRange);
      end;
      LValue := Role(swpfCopyFields).Prop(NyxAttributeName(atValue));
      LFields := nwcfVisiblePresentation;

      if LValue = CWorkbenchFieldChoices[nwcfAllArrangement] then
      begin
        LFields := nwcfAllArrangement;
      end
      else if LValue <> CWorkbenchFieldChoices[nwcfVisiblePresentation] then
      begin
        raise ENyxCollection.Create('Choose an exact workbench column set');
      end;
      LValue := Role(swpfLineEnding).Prop(NyxAttributeName(atValue));
      LLines := nwleLF;

      if LValue = CWorkbenchLineChoices[nwleCRLF] then
      begin
        LLines := nwleCRLF;
      end
      else if LValue <> CWorkbenchLineChoices[nwleLF] then
      begin
        raise ENyxCollection.Create('Choose an exact workbench row separator');
      end;
      AEdit.Collection.Action := scaTableWorkbench;
      AEdit.Collection.TableWorkbench := NyxTableWorkbench
        .RangeSelection(Flag(swpfRangeSelection)).AllowCopy(LCopy).CopyFields(LFields)
        .Headers(Flag(swpfHeaders)).LineEnding(LLines).FocusedDraft(Flag(swpfFocusedDraft));
    end;
    ApplyNyxStudioCollectionViewIntent(AEdit.Collection, LSpec, LSchema);
  finally
    LProjection.Free;
  end;
end;
