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

{ Included by nyx.data.toolbar.editor. Form state owns only scalar values and
  copied identity maps. No form child retains its document, renderer or host. }

function NyxDataToolbarEditorFieldID(const AID: TNyxText;
  AField: TNyxDataToolbarEditorField): TNyxText;
begin
  Result := NyxControl(AID).ID + '-' + CNamesField[AField];
end;

function NyxDataToolbarEditorSortID(const AID: TNyxText; AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= NyxMaximumToolbarSorts - 1) then
  begin
    raise ENyxDataToolbar.Create('Toolbar choice index is out of range');
  end;
  Result := AID + '-sort-' + TNyxText(IntToStr(AIndex));
end;

function NyxDataToolbarEditorKeyID(const AID: TNyxText; AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= NyxMaximumQuerySorts) then
  begin
    raise ENyxDataToolbar.Create('Toolbar ordering index is out of range');
  end;
  Result := AID + '-key-' + TNyxText(IntToStr(AIndex));
end;

procedure AddIdentity(var AValues: TNyxDataValue; const AValue: TNyxText);
var
  LRows: array of TNyxDataValue;
  LIndex: Integer;
begin
  for LIndex := 0 to AValues.Count - 1 do
  begin

    if AValues.Item(LIndex).AsText = AValue then
    begin
      Exit;
    end;
  end;
  SetLength(LRows, AValues.Count + 1);
  for LIndex := 0 to AValues.Count - 1 do
  begin
    LRows[LIndex] := AValues.Item(LIndex);
  end;
  LRows[High(LRows)] := NyxData(AValue);
  AValues := NyxArray(LRows);
end;

procedure Choices(const AParent: INyxControl; const AID: TNyxText;
  AField: TNyxDataToolbarEditorField; const ACaption: TNyxText;
  const AValues: TNyxDataValue; const AValue: TNyxText);
var
  LItems: TNyxText;
  LSelected: TNyxText;
  LLabel: TNyxText;
  LIndex: Integer;
  LSelect: INyxSelect;
begin
  LItems := '';
  LSelected := '';
  for LIndex := 0 to AValues.Count - 1 do
  begin
    LLabel := NyxMenuEditorChoiceCaption(LIndex, AValues.Item(LIndex).AsText);

    if LIndex > 0 then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + LLabel;

    if (LIndex = 0) or (AValues.Item(LIndex).AsText = AValue) then
    begin
      LSelected := LLabel;
    end;
  end;
  LSelect := NewNyxSelect(NyxDataToolbarEditorFieldID(AID, AField));
  LSelect.Configure.Text(ACaption).AccessibleName(ACaption).Items(LItems).
    Value(LSelected).PartName(NyxPart(CNamesField[AField])).Done;
  LSelect.Node.SetProp(CField, CNamesField[AField]).SetProp(CChoices, AValues.ToJSON);
  AParent.Add(LSelect);
end;

procedure TextField(const AParent: INyxControl; const AID: TNyxText;
  AField: TNyxDataToolbarEditorField; const ACaption, AValue: TNyxText);
var
  LInput: INyxInput;
begin
  LInput := NewNyxInput(NyxDataToolbarEditorFieldID(AID, AField));
  LInput.Configure.Text(ACaption).AccessibleName(ACaption).Value(AValue).
    PartName(NyxPart(CNamesField[AField])).Done;
  LInput.Node.SetProp(CField, CNamesField[AField]);
  AParent.Add(LInput);
end;

procedure GatherSources(ADocument: TNyxDocument; AContext, AProjection: TNyxNode;
  out ATargets, ATexts, AFields: TNyxDataValue);

  procedure Visit(ANode: TNyxNode);
  var
    LIndex: Integer;
    LSchema: TNyxCollectionSchema;
    LTarget: TNyxNode;
  begin

    if ANode.HasCollectionView and ANode.CollectionView.Defined and
      ADocument.Collections.Has(ANode.CollectionView.Key) and
      (NyxCollectionProjectionForNode(ANode) in [cpList, cpTable]) then
    begin
      LTarget := nil;
      try
        LTarget := TargetNode(AContext, AProjection, NyxControl(ANode.SourceID));
      except
        on ENyxDataToolbar do
        begin
          { A nested or foreign instance is outside this exact expansion. }
        end;
      end;

      if LTarget = ANode then
      begin
        AddIdentity(ATargets, ANode.SourceID);
        LSchema := ADocument.Collections.Snapshot(ANode.CollectionView.Key).Schema;
        for LIndex := 0 to LSchema.Count - 1 do
        begin
          AddIdentity(AFields, LSchema.FieldAt(LIndex).Name);

          if LSchema.FieldAt(LIndex).Kind = nskText then
          begin
            AddIdentity(ATexts, LSchema.FieldAt(LIndex).Name);
          end;
        end;
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex]);
    end;
  end;

begin
  ATargets := NyxArray([]);
  ATexts := NyxArray([]);
  AFields := NyxArray([NyxData('')]);
  Visit(AContext);
end;

function NewNyxDataToolbarEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
var
  LRaw: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LTargets: TNyxDataValue;
  LTexts: TNyxDataValue;
  LFields: TNyxDataValue;
  LIntent: TNyxDataToolbarIntent;
  LOptions: TNyxDataToolbarOptions;
  LTarget: TNyxText;
  LSearch: TNyxText;
  LBaseline: TNyxText;
  LMatch: TNyxToolbarSearchMode;
  LComparison: TNyxQueryTextComparison;
  LCount: Integer;
  LKeys: Integer;
  LIndex: Integer;
  LKey: Integer;
  LQuery: TNyxCollectionQuery;
  LSort: TNyxCollectionSort;
  LRow: INyxCard;
  LKeyRow: INyxCard;
  LCheck: INyxCheckbox;
  LButton: INyxButton;
  LAction: TNyxDataToolbarEditorAction;
  LCanConfigure: Boolean;
begin
  LContext := nil;
  try
    ResolveOwner(ADocument, AOwner, LRaw, LContext, LProjection);
    GatherSources(ADocument, LContext, LProjection, LTargets, LTexts, LFields);
    LCanConfigure := (LTargets.Count > 0) and (LTexts.Count > 0);
    LTarget := '';
    LSearch := '';
    LBaseline := 'Original order';
    LMatch := ntsmContains;
    LComparison := nqtAsciiInsensitive;
    LIntent := GetNyxDataToolbarIntent(LProjection);
    LOptions := Default(TNyxDataToolbarOptions);

    if LCanConfigure then
    begin
      LTarget := LTargets.Item(0).AsText;
      LSearch := LTexts.Item(0).AsText;
    end;

    if LIntent.Mode = ntiConfigured then
    begin
      LOptions := LIntent.Options;
      LTarget := LOptions.Target.ID;
      LSearch := LOptions.SearchField.Name;
      LBaseline := LOptions.BaselineChoice.Name;
      LMatch := LOptions.MatchMode;
      LComparison := LOptions.Comparison;
    end;
    AddIdentity(LTargets, LTarget);
    AddIdentity(LTexts, LSearch);
    Result := NewNyxCard(AID);
    Result.Configure.Gap(10).Padding(16).Done;
    Result.Node.SetProp(CVersion, CFormVersion).SetProp(COwner, LRaw.ID).
      SetProp(CBaseline, NyxDataToolbarEditorBaseline(ADocument, AOwner));
    Result.Add(NewNyxLabel(AID + '-title').WithText('Search and sort'));
    Result.Add(NewNyxLabel(AID + '-help').WithText(
      'Choose a bound List or Table in this workspace. Fields are checked against its schema. ' +
      'Clear preserves other filters and restores your original ordering.'));
    Choices(Result, AID, ntefTarget, 'Target List or Table', LTargets, LTarget);
    Choices(Result, AID, ntefSearch, 'Search text field', LTexts, LSearch);
    Choices(Result, AID, ntefMatch, 'Text matching', NyxArray([
      NyxData(CMatchLabels[ntsmContains]), NyxData(CMatchLabels[ntsmStartsWith]),
      NyxData(CMatchLabels[ntsmEqual])]), CMatchLabels[LMatch]);
    Choices(Result, AID, ntefComparison, 'Text comparison', NyxArray([
      NyxData(CComparisonLabels[nqtExact]), NyxData(CComparisonLabels[nqtAsciiInsensitive])]),
      CComparisonLabels[LComparison]);
    TextField(Result, AID, ntefBaseline, 'Original order caption', LBaseline);
    LCount := LOptions.Count + 1;

    if LCount >= NyxMaximumToolbarSorts then
    begin
      LCount := NyxMaximumToolbarSorts - 1;
    end;
    Result.Node.SetProp(CRows, TNyxText(IntToStr(LCount)));
    Result.Add(NewNyxLabel(AID + '-sort-help').WithText(
      'Include a choice to add it. Uncheck it to remove it on Apply. ' +
      'Each choice can order by multiple fields; an empty field skips that key.'));
    for LIndex := 0 to LCount - 1 do
    begin
      LRow := NewNyxCard(NyxDataToolbarEditorSortID(AID, LIndex));
      LRow.Configure.Gap(8).Done;
      LCheck := NewNyxCheckbox(NyxDataToolbarEditorFieldID(LRow.ID, ntefIncluded));
      LCheck.Configure.Text('Include sort choice').AccessibleName('Include sort choice').
        PartName(NyxPart(CNamesField[ntefIncluded])).Value(LIndex < LOptions.Count).Done;
      LCheck.Node.SetProp(CField, CNamesField[ntefIncluded]);
      LRow.Add(LCheck);
      LQuery := NyxCollectionQuery;
      LTarget := '';

      if LIndex < LOptions.Count then
      begin
        LTarget := LOptions.ChoiceAt(LIndex).Reference.Name;
        LQuery := LOptions.ChoiceAt(LIndex).Query;
      end;
      TextField(LRow, LRow.ID, ntefCaption, 'Sort choice caption', LTarget);
      LKeys := LQuery.SortCount + 1;

      if LKeys > NyxMaximumQuerySorts then
      begin
        LKeys := NyxMaximumQuerySorts;
      end;
      LRow.Node.SetProp(CKeys, TNyxText(IntToStr(LKeys)));
      for LKey := 0 to LKeys - 1 do
      begin
        LKeyRow := NewNyxCard(NyxDataToolbarEditorKeyID(LRow.ID, LKey));
        LKeyRow.Configure.Gap(6).Done;
        LTarget := '';
        LSort := Default(TNyxCollectionSort);

        if LKey < LQuery.SortCount then
        begin
          LSort := LQuery.SortAt(LKey);
          LTarget := LSort.FieldName;
        end;
        AddIdentity(LFields, LTarget);
        Choices(LKeyRow, LKeyRow.ID, ntefSortField, 'Order by field', LFields, LTarget);
        Choices(LKeyRow, LKeyRow.ID, ntefDirection, 'Direction', NyxArray([
          NyxData(CDirectionLabels[nsdAscending]), NyxData(CDirectionLabels[nsdDescending])]),
          CDirectionLabels[LSort.Direction]);
        Choices(LKeyRow, LKeyRow.ID, ntefSortComparison, 'Ordering text comparison', NyxArray([
          NyxData(CComparisonLabels[nqtExact]), NyxData(CComparisonLabels[nqtAsciiInsensitive])]),
          CComparisonLabels[LSort.TextComparison]);
        LRow.Add(LKeyRow);
      end;
      Result.Add(LRow);
    end;
    for LAction := Low(TNyxDataToolbarEditorAction) to High(TNyxDataToolbarEditorAction) do
    begin
      LButton := NewNyxButton(NyxDataToolbarEditorActionID(AID, LAction));
      LButton.Configure.Text(CLabels[LAction]).AccessibleName(CLabels[LAction]).
        PartName(NyxPart(CNames[LAction])).Done;
      LButton.Node.SetProp(CEditor, AID).SetProp(CAction, CNames[LAction]);

      if LAction = nteaConfigure then
      begin
        LButton.Configure.Enabled(LCanConfigure).Done;
      end
      else if LAction = nteaInherit then
      begin
        LButton.Configure.Enabled(HasNyxDataToolbarIntent(LRaw)).Done;
      end;
      Result.Add(LButton);
    end;
  finally
    LContext.Free;
  end;
end;

function FormField(AParent: TNyxNode; const AID: TNyxText;
  AField: TNyxDataToolbarEditorField; AKind: TNyxKind): TNyxNode;
begin
  Result := AParent.Find(NyxDataToolbarEditorFieldID(AID, AField));

  if (Result = nil) or (Result.Parent <> AParent) or
    (Result.Kind <> NyxKindName(AKind)) or (Result.ProjectionKind <> NyxKindName(AKind)) or
    (Result.StoredProp(CField) <> CNamesField[AField]) or
    (Result.StoredProp(NyxAttributeName(atPart)) <> CNamesField[AField]) then
  begin
    raise ENyxDataToolbar.Create('Toolbar form member is missing or incompatible');
  end;
end;

function ReadChoice(AParent: TNyxNode; const AID: TNyxText;
  AField: TNyxDataToolbarEditorField): TNyxText;
var
  LNode: TNyxNode;
  LValues: TNyxDataValue;
  LValue: TNyxText;
  LIndex: Integer;
begin
  LNode := FormField(AParent, AID, AField, nkSelect);
  LValues := TNyxDataValue.ParseJSON(LNode.StoredProp(CChoices));

  if (LValues.Kind <> ndArray) or (LValues.Count > NyxMaximumNodes) then
  begin
    raise ENyxDataToolbar.Create('Toolbar choice map is malformed');
  end;
  LValue := LNode.Prop(NyxAttributeName(atValue));
  for LIndex := 0 to LValues.Count - 1 do
  begin

    if LValue = NyxMenuEditorChoiceCaption(LIndex, LValues.Item(LIndex).AsText) then
    begin
      Exit(LValues.Item(LIndex).AsText);
    end;
  end;
  raise ENyxDataToolbar.Create('Choose one of the available toolbar values');
end;

function ReadComparison(const AValue: TNyxText): TNyxQueryTextComparison;
begin
  for Result := Low(TNyxQueryTextComparison) to High(TNyxQueryTextComparison) do
  begin

    if AValue = CComparisonLabels[Result] then
    begin
      Exit;
    end;
  end;
  raise ENyxDataToolbar.Create('Unknown toolbar text comparison');
end;

function ReadOptions(AEditor: TNyxNode; const AID: TNyxText;
  ADocument: TNyxDocument; const AOwner: TNyxControlRef): TNyxDataToolbarOptions;
var
  LRaw: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LTarget: TNyxNode;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LMatch: TNyxToolbarSearchMode;
  LValue: TNyxText;
  LName: TNyxText;
  LCount: Integer;
  LKeys: Integer;
  LIndex: Integer;
  LKey: Integer;
  LFieldIndex: Integer;
  LFound: Boolean;
  LRow: TNyxNode;
  LKeyRow: TNyxNode;
  LDirection: TNyxSortDirection;
  LComparison: TNyxQueryTextComparison;
  LQuery: TNyxCollectionQuery;
  LSort: TNyxCollectionSort;
begin
  Result := NyxDataToolbarOptions(NyxControl(ReadChoice(AEditor, AID, ntefTarget)),
    NyxTextField(ReadChoice(AEditor, AID, ntefSearch)));
  LValue := ReadChoice(AEditor, AID, ntefMatch);
  LFound := False;
  for LMatch := Low(TNyxToolbarSearchMode) to High(TNyxToolbarSearchMode) do
  begin

    if LValue = CMatchLabels[LMatch] then
    begin
      Result := Result.SearchMode(LMatch,
        ReadComparison(ReadChoice(AEditor, AID, ntefComparison)));
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxDataToolbar.Create('Unknown toolbar search mode');
  end;
  Result := Result.Baseline(NyxToolbarSort(FormField(AEditor, AID, ntefBaseline,
    nkInput).Prop(NyxAttributeName(atValue))));

  if not TryStrToInt(AEditor.StoredProp(CRows), LCount) or
    (LCount < 1) or (LCount >= NyxMaximumToolbarSorts) then
  begin
    raise ENyxDataToolbar.Create('Toolbar choice rows are malformed');
  end;
  LContext := nil;
  try
    ResolveOwner(ADocument, AOwner, LRaw, LContext, LProjection);
    LTarget := TargetNode(LContext, LProjection, Result.Target);
    LSchema := ADocument.Collections.Snapshot(LTarget.CollectionView.Key).Schema;
    for LIndex := 0 to LCount - 1 do
    begin
      LName := NyxDataToolbarEditorSortID(AID, LIndex);
      LRow := AEditor.Find(LName);

      if (LRow = nil) or (LRow.Parent <> AEditor) then
      begin
        raise ENyxDataToolbar.Create('Toolbar choice row is missing');
      end;
      LValue := FormField(LRow, LName, ntefIncluded, nkCheckbox).Prop(NyxAttributeName(atValue));

      if (LValue <> 'true') and (LValue <> 'false') then
      begin
        raise ENyxDataToolbar.Create('Toolbar choice inclusion requires a Boolean');
      end;

      if LValue = 'false' then
      begin
        Continue;
      end;
      LValue := FormField(LRow, LName, ntefCaption, nkInput).Prop(NyxAttributeName(atValue));

      if not TryStrToInt(LRow.StoredProp(CKeys), LKeys) or
        (LKeys < 1) or (LKeys > NyxMaximumQuerySorts) then
      begin
        raise ENyxDataToolbar.Create('Toolbar ordering keys are malformed');
      end;
      LQuery := NyxCollectionQuery;
      for LKey := 0 to LKeys - 1 do
      begin
        LName := NyxDataToolbarEditorKeyID(LRow.SourceID, LKey);
        LKeyRow := LRow.Find(LName);

        if (LKeyRow = nil) or (LKeyRow.Parent <> LRow) then
        begin
          raise ENyxDataToolbar.Create('Toolbar ordering row is missing');
        end;
        LName := ReadChoice(LKeyRow, LKeyRow.SourceID, ntefSortField);

        if LName = '' then
        begin
          Continue;
        end;
        LFound := False;
        for LFieldIndex := 0 to LSchema.Count - 1 do
        begin

          if LSchema.FieldAt(LFieldIndex).Name = LName then
          begin
            LField := LSchema.FieldAt(LFieldIndex);
            LFound := True;
            Break;
          end;
        end;

        if not LFound then
        begin
          raise ENyxDataToolbar.Create('The ordering field is absent from the selected target');
        end;
        LName := ReadChoice(LKeyRow, LKeyRow.SourceID, ntefDirection);
        LDirection := nsdAscending;

        if LName = CDirectionLabels[nsdDescending] then
        begin
          LDirection := nsdDescending;
        end
        else if LName <> CDirectionLabels[nsdAscending] then
        begin
          raise ENyxDataToolbar.Create('Unknown toolbar ordering direction');
        end;
        LComparison := ReadComparison(ReadChoice(LKeyRow, LKeyRow.SourceID, ntefSortComparison));

        if (LField.Kind <> nskText) and (LComparison <> nqtExact) then
        begin
          raise ENyxDataToolbar.Create('Text ordering comparison applies only to text fields');
        end;
        case LField.Kind of
          nskText:
            begin
              LSort := NyxSort(NyxTextField(LField.Name), LDirection, LComparison);
            end;
          nskBoolean:
            begin
              LSort := NyxSort(NyxBooleanField(LField.Name), LDirection);
            end;
          nskInteger:
            begin
              LSort := NyxSort(NyxIntegerField(LField.Name), LDirection);
            end;
          nskNumber:
            begin
              LSort := NyxSort(NyxNumberField(LField.Name), LDirection);
            end;
        end;

        if LQuery.SortCount = 0 then
        begin
          LQuery := LQuery.OrderBy(LSort);
        end
        else
        begin
          LQuery := LQuery.ThenBy(LSort);
        end;
      end;
      Result := Result.Sort(NyxToolbarSort(LValue), LQuery);
    end;
  finally
    LContext.Free;
  end;
end;

function CaptureNyxDataToolbarEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxDataToolbarEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LForm: TNyxText;
  LOwner: TNyxControlRef;
  LAction: TNyxDataToolbarEditorAction;
  LSelected: TNyxDataToolbarEditorAction;
  LFound: Boolean;
begin
  AChange := Default(TNyxDataToolbarEditorChange);
  Result := False;

  if (AButton = nil) or (AButton.Kind <> NyxKindName(nkButton)) or
    (AButton.StoredProp(CEditor) = '') then
  begin
    Exit;
  end;

  if (AShellRoot = nil) or (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxDataToolbar.Create('This toolbar form belongs to a retired view');
  end;
  LForm := AButton.StoredProp(CEditor);
  LEditor := AButton.Parent;

  if (LEditor = nil) or (LEditor.Kind <> NyxKindName(nkCard)) or
    (LEditor.SourceID <> LForm) or (LEditor.StoredProp(CVersion) <> CFormVersion) then
  begin
    raise ENyxDataToolbar.Create('Toolbar form is missing or malformed');
  end;
  LFound := False;
  LSelected := nteaConfigure;
  for LAction := Low(TNyxDataToolbarEditorAction) to High(TNyxDataToolbarEditorAction) do
  begin

    if (AButton.SourceID = NyxDataToolbarEditorActionID(LForm, LAction)) and
      (AButton.StoredProp(CAction) = CNames[LAction]) then
    begin
      LSelected := LAction;
      LFound := True;
    end;
  end;

  if not LFound or (AButton.Prop(NyxAttributeName(atEnabled), 'true') <> 'true') then
  begin
    raise ENyxDataToolbar.Create('This toolbar action is unavailable');
  end;
  LOwner := NyxControl(LEditor.StoredProp(COwner));

  if LEditor.StoredProp(CBaseline) <> NyxDataToolbarEditorBaseline(ADocument, LOwner) then
  begin
    raise ENyxDataToolbar.Create('Toolbar or its source changed; reopen this form');
  end;

  if LSelected = nteaConfigure then
  begin
    AChange := NyxDataToolbarEditorChange(ADocument, LOwner,
      ReadOptions(LEditor, LForm, ADocument, LOwner));
  end
  else
  begin
    AChange := NyxDataToolbarEditorChange(ADocument, LOwner, LSelected);
  end;
  Result := True;
end;
