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
{ Included in nyx.studio.collections. No runtime recovery authority is captured. }
const
  CRecoveryAction = 'studio-recovery-action';
  CRecoveryFields: array[TNyxStudioDraftRecoveryField] of TNyxText =
    ('entries', 'payload-bytes', 'warning-entries', 'warning-text-bytes');
  CRecoveryCaptions: array[TNyxStudioDraftRecoveryField] of TNyxText =
    ('Maximum retained entries (0 allows none)', 'Maximum retained payload (bytes)',
     'Entry warning threshold (0 disables)', 'Text warning threshold (bytes, 0 disables)');

function NyxStudioDraftRecoveryFieldID(AField: TNyxStudioDraftRecoveryField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxStudioDraftRecoveryField))) or
    (Ord(AField) > Ord(High(TNyxStudioDraftRecoveryField))) then
  begin
    raise ENyxCollection.Create('Unknown recovery field role');
  end;
  Result := NyxStudioDraftRecoveryEditorID + '-' + CRecoveryFields[AField];
end;

procedure AddDraftRecoveryForm(AParent: TNyxNode; const AOwner: TNyxText;
  const ASchema: TNyxCollectionSchema; const ASpec: TNyxCollectionViewSpec;
  AProjection: TNyxCollectionProjection; ALocked: Boolean);
var
  LSection: INyxDisclosure;
  LForm: INyxCard;
  LPolicy: TNyxCollectionDraftRecoveryPolicy;
  LStatus: TNyxText;
  LValues: array[TNyxStudioDraftRecoveryField] of Integer;
  LField: TNyxStudioDraftRecoveryField;
begin
  LPolicy := ASpec.DraftRecoveryPolicy;
  LStatus := 'Library defaults. Apply to save these limits for new runtime readers.';

  if LPolicy.Defined then
  begin
    LStatus := 'Saved defaults. Runtime readers can override limits independently.';
  end
  else
  begin
    LPolicy := NyxDraftRecoveryPolicy;
  end;
  LValues[srfEntries] := LPolicy.MaximumEntries;
  LValues[srfPayloadBytes] := LPolicy.MaximumPayloadBytes;
  LValues[srfWarningEntries] := LPolicy.WarningEntries;
  LValues[srfWarningTextBytes] := LPolicy.WarningTextBytes;
  LSection := NewNyxDisclosure(NyxStudioDraftRecoveryEditorID + '-section').WithText('Draft recovery');
  LSection.Configure.Value(False).Surface(False).Done;
  LForm := NewNyxCard(NyxStudioDraftRecoveryEditorID);
  LForm.Configure.Padding(8).Gap(6).Compound(True).Enabled(not ALocked).Done;
  LForm.Node.SetProp(NyxMenuFormOwnerKey, AOwner)
    .SetProp(NyxMenuFormBaselineKey, NyxQueryEditorBaseline(ASchema, ASpec))
    .SetProp(CKey, ASpec.Key.Name);
  LForm.Node.Add(NewNyxHeading(LForm.Node.ID + '-title').WithText('Unfinished edit recovery'));
  LForm.Node.Add(NewNyxLabel(LForm.Node.ID + '-status').WithText(LStatus +
    ' Full capacity holds original text; warnings never discard it.'));
  for LField := Low(LField) to High(LField) do
  begin
    LForm.Node.Add(NewNyxInput(NyxStudioDraftRecoveryFieldID(LField)).Configure
      .Text(CRecoveryCaptions[LField]).InputType(niText)
      .Value(TNyxText(IntToStr(LValues[LField]))).Enabled(not ALocked).Done);
  end;
  LForm.Node.Add(NewNyxButton(NyxStudioDraftRecoveryApplyID).Configure
    .Text('Apply recovery defaults').Enabled(not ALocked)
    .Extension(CRecoveryAction, CPolicyApply).Done);
  LForm.Node.Add(NewNyxButton(NyxStudioDraftRecoveryDefaultID).Configure
    .Text('Use library recovery defaults').Enabled(not ALocked)
    .Extension(CRecoveryAction, CPolicyDefault).Done);
  LSection.BodyColumn.Add(LForm);
  AParent.Add(LSection);
end;

function CaptureNyxStudioDraftRecovery(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LForm: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LOwner: TNyxText;
  LBaseline: TNyxText;
  LAction: TNyxText;
  LValues: array[TNyxStudioDraftRecoveryField] of Integer;
  LField: TNyxStudioDraftRecoveryField;

  function Role(const AID: TNyxText; AKind: TNyxKind): TNyxNode;
  begin
    Result := LForm.Find(AID);

    if (Result = nil) or (Result.Parent <> LForm) or
      (Result.Kind <> NyxKindName(AKind)) or (Result.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Use the complete current typed recovery form');
    end;
  end;

begin
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and (AButton.Prop(CRecoveryAction) <> '');

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or
    (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxCollection.Create('Use the current mounted recovery action');
  end;
  LForm := AShellRoot.Find(NyxStudioDraftRecoveryEditorID);

  if (LForm = nil) or (AButton.Parent <> LForm) or (LForm.Count <> 8) or
    (LForm.Kind <> NyxKindName(nkCard)) or (LForm.ProjectionKind <> NyxKindName(nkCard)) then
  begin
    raise ENyxCollection.Create('Recovery form identity or complete roles changed');
  end;
  Role(LForm.ID + '-title', nkHeading);
  Role(LForm.ID + '-status', nkLabel);
  Role(NyxStudioDraftRecoveryApplyID, nkButton);
  Role(NyxStudioDraftRecoveryDefaultID, nkButton);
  for LField := Low(LField) to High(LField) do
  begin
    Role(NyxStudioDraftRecoveryFieldID(LField), nkInput);
  end;
  LAction := AButton.Prop(CRecoveryAction);

  if ((LAction <> CPolicyApply) and (LAction <> CPolicyDefault)) or
    ((LAction = CPolicyApply) and (AButton.ID <> NyxStudioDraftRecoveryApplyID)) or
    ((LAction = CPolicyDefault) and (AButton.ID <> NyxStudioDraftRecoveryDefaultID)) then
  begin
    raise ENyxCollection.Create('Unknown recovery action');
  end;
  LOwner := LForm.Prop(NyxMenuFormOwnerKey);
  LBaseline := LForm.Prop(NyxMenuFormBaselineKey);

  if (LOwner = '') or (ASession.SelectedID <> LOwner) or (LBaseline = '') or
    APending.CollectionViewLocked(LOwner) or ASession.SourceDraftPending then
  begin
    raise ENyxCollection.Create('Reopen recovery settings after pending edits or Pascal drafts');
  end;
  LProjection := ASession.SelectedProjection;
  try

    if (LProjection = nil) or not LProjection.HasCollectionView or
      not LProjection.CollectionView.Defined then
    begin
      raise ENyxCollection.Create('Recovery defaults require the selected bound list, table or tree');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if (LForm.Prop(CKey) <> LSpec.Key.Name) or APending.CollectionLocked(LSpec.Key) or
      (LBaseline <> NyxQueryEditorBaseline(LSchema, LSpec)) then
    begin
      raise ENyxCollection.Create('Recovery binding or schema changed; reopen its form');
    end;
    AEdit.Action := sdaCollection;
    AEdit.Selection := LOwner;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Action := scaDefaultDraftRecovery;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Projection := NyxCollectionProjectionForNode(LProjection);
    AEdit.Collection.DraftRecoveryBaseline := LBaseline;

    if LAction = CPolicyApply then
    begin
      for LField := Low(LField) to High(LField) do
      begin

        if not TryNyxStateInteger(LForm.Find(NyxStudioDraftRecoveryFieldID(LField))
          .Prop(NyxAttributeName(atValue)), LValues[LField]) then
        begin
          raise ENyxCollection.Create('Recovery limits require exact nonnegative Integers');
        end;
      end;
      AEdit.Collection.Action := scaDraftRecovery;
      AEdit.Collection.DraftRecovery := NyxDraftRecoveryPolicy
        .Entries(LValues[srfEntries]).PayloadBytes(LValues[srfPayloadBytes])
        .WarnEntries(LValues[srfWarningEntries]).WarnTextBytes(LValues[srfWarningTextBytes]);
    end;
    AEdit.Collection.Validate;
  finally
    LProjection.Free;
  end;
end;
