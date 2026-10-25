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

{ Included in nyx.studio.collections. Ordinary Nyx controls own unfinished text;
  capture copies only validated scalar intent. No store/snapshot is retained. }
const
  CInspectionAction = 'studio-inspection-action';
  CInspectionFields: array[TNyxStudioInspectionField] of TNyxText =
    ('follow', 'paused-bytes', 'removed-identities');
  CInspectionKeep = 'Keep reading position';
  CInspectionLast = 'Follow last visible';

function NyxStudioInspectionFieldID(AField: TNyxStudioInspectionField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxStudioInspectionField))) or
    (Ord(AField) > Ord(High(TNyxStudioInspectionField))) then
  begin
    raise ENyxCollection.Create('Unknown inspection field role');
  end;
  Result := NyxStudioInspectionEditorID + '-' + CInspectionFields[AField];
end;

procedure AddInspectionForm(AParent: TNyxNode; const AOwner: TNyxText;
  const ASchema: TNyxCollectionSchema; const ASpec: TNyxCollectionViewSpec;
  AProjection: TNyxCollectionProjection; ALocked: Boolean);
var
  LSection: INyxDisclosure;
  LForm: INyxCard;
  LPolicy: TNyxCollectionInspectionPolicy;
  LChoices: TNyxText;
  LFollow: TNyxText;
  LStatus: TNyxText;
begin
  LPolicy := ASpec.InspectionPolicy;
  LStatus := 'Not configured. Apply enables these defaults; runtime pause does not edit the design.';

  if LPolicy.Defined then
  begin
    LStatus := 'Saved defaults. Readers can change follow mode or pause independently at runtime.';
  end
  else
  begin
    LPolicy := NyxInspectionPolicy;
  end;
  LChoices := CInspectionKeep;

  if AProjection <> cpTree then
  begin
    LChoices := LChoices + #10 + CInspectionLast;
  end;
  LFollow := CInspectionKeep;

  if LPolicy.DefaultFollow = ncfLastVisible then
  begin
    LFollow := CInspectionLast;
  end;
  LSection := NewNyxDisclosure(NyxStudioInspectionEditorID + '-section').WithText('Live inspection');
  LSection.Configure.Value(False).Surface(False).Done;
  LForm := NewNyxCard(NyxStudioInspectionEditorID);
  LForm.Configure.Padding(8).Gap(6).Compound(True).Enabled(not ALocked).Done;
  LForm.Node.SetProp(NyxMenuFormOwnerKey, AOwner)
    .SetProp(NyxMenuFormBaselineKey, NyxQueryEditorBaseline(ASchema, ASpec))
    .SetProp(CKey, ASpec.Key.Name);
  LForm.Node.Add(NewNyxHeading(LForm.Node.ID + '-title').WithText('Inspection defaults'));
  LForm.Node.Add(NewNyxLabel(LForm.Node.ID + '-status').WithText(LStatus));
  LForm.Node.Add(NewNyxSelect(NyxStudioInspectionFieldID(sifFollow)).Configure
    .Text('Initial follow mode').Items(LChoices).Value(LFollow).Enabled(not ALocked).Done);
  { These inputs own unfinished notation, including empty/partial values. A
    numeric-domain control would reject that draft before the complete form's
    exact Integer admission. The saved policy itself always remains typed. }
  LForm.Node.Add(NewNyxInput(NyxStudioInspectionFieldID(sifPausedBytes)).Configure
    .Text('Maximum paused payload (bytes)').InputType(niText)
    .Value(TNyxText(IntToStr(LPolicy.MaximumPausedBytes))).Enabled(not ALocked).Done);
  LForm.Node.Add(NewNyxInput(NyxStudioInspectionFieldID(sifRemovedIdentities)).Configure
    .Text('Removed identities retained (0 allows none)').InputType(niText)
    .Value(TNyxText(IntToStr(LPolicy.IdentityLimit))).Enabled(not ALocked).Done);
  LForm.Node.Add(NewNyxButton(NyxStudioInspectionApplyID).Configure
    .Text('Apply inspection defaults').Enabled(not ALocked)
    .Extension(CInspectionAction, CPolicyApply).Done);
  LForm.Node.Add(NewNyxButton(NyxStudioInspectionDefaultID).Configure
    .Text('Use unconfigured defaults').Enabled(not ALocked)
    .Extension(CInspectionAction, CPolicyDefault).Done);
  LSection.BodyColumn.Add(LForm);
  AParent.Add(LSection);
end;

function CaptureNyxStudioInspection(ASession: TNyxStudioSession;
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
  LText: TNyxText;
  LBytes: Integer;
  LIdentities: Integer;

  function Role(const AID: TNyxText; AKind: TNyxKind): TNyxNode;
  begin
    Result := LForm.Find(AID);

    if (Result = nil) or (Result.Parent <> LForm) or
      (Result.Kind <> NyxKindName(AKind)) or (Result.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Use the complete current typed inspection form');
    end;
  end;

begin
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and (AButton.Prop(CInspectionAction) <> '');

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or
    (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxCollection.Create('Use the current mounted inspection action');
  end;
  LForm := AShellRoot.Find(NyxStudioInspectionEditorID);

  if (LForm = nil) or (AButton.Parent <> LForm) or (LForm.Count <> 7) or
    (LForm.Kind <> NyxKindName(nkCard)) or (LForm.ProjectionKind <> NyxKindName(nkCard)) then
  begin
    raise ENyxCollection.Create('Inspection form identity or complete roles changed');
  end;
  Role(LForm.ID + '-title', nkHeading);
  Role(LForm.ID + '-status', nkLabel);
  Role(NyxStudioInspectionApplyID, nkButton);
  Role(NyxStudioInspectionDefaultID, nkButton);
  Role(NyxStudioInspectionFieldID(sifFollow), nkSelect);
  Role(NyxStudioInspectionFieldID(sifPausedBytes), nkInput);
  Role(NyxStudioInspectionFieldID(sifRemovedIdentities), nkInput);
  LAction := AButton.Prop(CInspectionAction);

  if ((LAction <> CPolicyApply) and (LAction <> CPolicyDefault)) or
    ((LAction = CPolicyApply) and (AButton.ID <> NyxStudioInspectionApplyID)) or
    ((LAction = CPolicyDefault) and (AButton.ID <> NyxStudioInspectionDefaultID)) then
  begin
    raise ENyxCollection.Create('Unknown inspection action');
  end;
  LOwner := LForm.Prop(NyxMenuFormOwnerKey);
  LBaseline := LForm.Prop(NyxMenuFormBaselineKey);

  if (LOwner = '') or (ASession.SelectedID <> LOwner) or (LBaseline = '') or
    APending.CollectionViewLocked(LOwner) or ASession.SourceDraftPending then
  begin
    raise ENyxCollection.Create('Reopen inspection after pending edits or Pascal drafts');
  end;
  LProjection := ASession.SelectedProjection;
  try

    if (LProjection = nil) or not LProjection.HasCollectionView or
      not LProjection.CollectionView.Defined then
    begin
      raise ENyxCollection.Create('Inspection requires the selected bound list, table or tree');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if (LForm.Prop(CKey) <> LSpec.Key.Name) or APending.CollectionLocked(LSpec.Key) or
      (LBaseline <> NyxQueryEditorBaseline(LSchema, LSpec)) then
    begin
      raise ENyxCollection.Create('Inspection binding or schema changed; reopen its form');
    end;
    AEdit.Action := sdaCollection;
    AEdit.Selection := LOwner;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Action := scaDefaultInspection;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Projection := NyxCollectionProjectionForNode(LProjection);
    AEdit.Collection.InspectionBaseline := LBaseline;

    if LAction = CPolicyApply then
    begin
      LText := LForm.Find(NyxStudioInspectionFieldID(sifFollow)).Prop(NyxAttributeName(atValue));

      if (LText <> CInspectionKeep) and (LText <> CInspectionLast) then
      begin
        raise ENyxCollection.Create('Choose a supported initial follow mode');
      end;

      if not TryNyxStateInteger(LForm.Find(NyxStudioInspectionFieldID(sifPausedBytes))
        .Prop(NyxAttributeName(atValue)), LBytes) or
        not TryNyxStateInteger(LForm.Find(NyxStudioInspectionFieldID(sifRemovedIdentities))
        .Prop(NyxAttributeName(atValue)), LIdentities) then
      begin
        raise ENyxCollection.Create('Inspection limits require exact nonnegative Integers');
      end;
      AEdit.Collection.Action := scaInspection;
      AEdit.Collection.Inspection := NyxInspectionPolicy.PausedBytes(LBytes)
        .RemovedIdentities(LIdentities);

      if LText = CInspectionLast then
      begin
        AEdit.Collection.Inspection := AEdit.Collection.Inspection.Follow(ncfLastVisible);
      end;
    end;
    AEdit.Collection.Validate;
  finally
    LProjection.Free;
  end;
end;
