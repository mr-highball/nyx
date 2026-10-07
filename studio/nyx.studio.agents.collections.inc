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



{ Included by nyx.studio.agents. Queries borrow accepted document defaults for one
  bounded call. They never expose runtime stores or retain projected controls. }

type
  TNyxAgentCollectionMode = (acmList, acmSchema, acmRows, acmValue, acmDomain,
    acmBindings, acmColumn, acmApply, acmQuery, acmQueryValue);
  TNyxAgentCollectionSource = (acsLocal, acsEffective, acsRestorable);

const
  CCollectionModes: array[TNyxAgentCollectionMode] of TNyxText =
    ('list', 'schema', 'rows', 'value', 'domain', 'bindings', 'column', 'apply',
     'query', 'query-value');
  CCollectionSources: array[TNyxAgentCollectionSource] of TNyxText =
    ('local', 'effective', 'restorable');

function CollectionField(const ASchema: TNyxCollectionSchema;
  const AName: TNyxText): TNyxCollectionField;
var
  LIndex: Integer;
begin
  for LIndex := 0 to ASchema.Count - 1 do
  begin

    if ASchema.FieldAt(LIndex).Name = AName then
    begin
      Exit(ASchema.FieldAt(LIndex));
    end;
  end;
  raise ENyxCollection.Create('Exact schema field does not exist');
end;

function CollectionCell(const AItem: TNyxCollectionItem;
  const AField: TNyxCollectionField): TNyxStateValue;
var
  LIndex: Integer;
begin
  for LIndex := 0 to AItem.Count - 1 do
  begin

    if AItem.FieldName(LIndex) = AField.Name then
    begin
      Result := AItem.FieldValue(LIndex);

      if Result.Kind <> AField.Kind then
      begin
        raise ENyxCollection.Create('Collection cell has a different scalar family');
      end;
      Exit;
    end;
  end;
  raise ENyxCollection.Create('Accepted row is missing its materialized field');
end;

function CollectionScalar(AKind: TNyxStateKind; const AData: TNyxDataValue):
  TNyxStateValue;
begin
  case Ord(AKind) of
    Ord(nskText):
      begin
        Result := TNyxStateValue.FromText(AData.AsText);
      end;
    Ord(nskBoolean):
      begin
        Result := TNyxStateValue.FromBoolean(AData.AsBoolean);
      end;
    Ord(nskInteger):
      begin
        Result := TNyxStateValue.FromInteger(AData.AsInteger);
      end;
    Ord(nskNumber):
      begin
        Result := TNyxStateValue.FromNumber(AData.AsNumber);
      end;
  else
    begin
      raise ENyxCollection.Create('Unknown collection scalar family');
    end;
  end;
end;

function CollectionScalarPreview(const AValue: TNyxStateValue): TNyxDataValue;
var
  LText: TNyxText;
  LCount: Integer;
begin

  if AValue.Kind = nskText then
  begin
    LText := TextSpan(AValue.TextValue, 0, 80, LCount);
    Result := NyxObject([NyxField('kind', NyxData('text')),
      NyxField('preview', NyxData(LText)), NyxField('characters', NyxData(LCount)),
      NyxField('truncated', NyxData(LCount > 80))]);
  end
  else
  begin
    Result := NyxObject([NyxField('kind', NyxData(NyxStateKindName(AValue.Kind))),
      NyxField('value', NyxStateValueData(AValue))]);
  end;
end;

function CollectionDomainSummary(const ADomain: TNyxValueDomain): TNyxDataValue;
var
  LData: TNyxDataValue;
  LMinimum: TNyxDataValue;
  LMaximum: TNyxDataValue;
  LChoices: Integer;
begin

  if not ADomain.Defined then
  begin
    Exit(NyxNull);
  end;
  LData := ADomain.ToData;
  LMinimum := NyxNull;
  LMaximum := NyxNull;
  LChoices := 0;

  if NyxAgentHas(LData, 'min') then
  begin
    LMinimum := LData.Field('min');
    LMaximum := LData.Field('max');
  end;

  if NyxAgentHas(LData, 'choices') then
  begin
    LChoices := LData.Field('choices').Count;
  end;
  Result := NyxObject([NyxField('kind', NyxData(NyxStateKindName(ADomain.Kind))),
    NyxField('min', LMinimum), NyxField('max', LMaximum),
    NyxField('choiceCount', NyxData(LChoices))]);
end;

function CollectionScalarWindow(const AValue: TNyxStateValue;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LOffset: Integer;
  LCount: Integer;
  LTotal: Integer;
  LText: TNyxText;
begin

  if AValue.Kind <> nskText then
  begin

    if NyxAgentHas(AArguments, 'offset') or NyxAgentHas(AArguments, 'count') then
    begin
      raise ENyxCollection.Create('Only text values have Unicode scalar windows');
    end;
    Exit(NyxObject([NyxField('kind', NyxData(NyxStateKindName(AValue.Kind))),
      NyxField('value', NyxStateValueData(AValue))]));
  end;
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumCollectionBytes);
  LCount := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
  LText := TextSpan(AValue.TextValue, LOffset, LCount, LTotal);

  if LOffset > LTotal then
  begin
    raise ENyxCollection.Create('Text offset is beyond the accepted collection value');
  end;
  Result := NyxObject([NyxField('kind', NyxData('text')),
    NyxField('offset', NyxData(LOffset)),
    NyxField('nextOffset', NyxData(Min(LOffset + LCount, LTotal))),
    NyxField('characters', NyxData(LTotal)), NyxField('text', NyxData(LText))]);
end;

function CollectionViewContext(const ASpec: TNyxCollectionViewSpec;
  AOffset, ALimit: Integer): TNyxDataValue;
var
  LColumns: array of TNyxDataValue;
  LColumn: TNyxCollectionColumn;
  LIndex: Integer;
  LScope: TNyxText;
  LSelection: TNyxText;
begin

  if not ASpec.Defined then
  begin
    Exit(NyxObject([NyxField('cleared', NyxData(True))]));
  end;
  LScope := 'application';
  LSelection := 'single';

  if ASpec.Scope = csInstance then
  begin
    LScope := 'instance';
  end;

  if ASpec.SelectionMode = nsmMultiple then
  begin
    LSelection := 'multiple';
  end;
  SetLength(LColumns, Max(0, Min(ALimit, ASpec.Count - AOffset)));
  for LIndex := 0 to High(LColumns) do
  begin
    LColumn := ASpec.ColumnAt(AOffset + LIndex);
    LColumns[LIndex] := NyxObject([NyxField('field', NyxData(LColumn.FieldName)),
      NyxField('kind', NyxData(NyxStateKindName(LColumn.Kind))),
      NyxField('editable', NyxData(LColumn.Mode = cmEditable)),
      NyxField('title', CollectionScalarPreview(TNyxStateValue.FromText(LColumn.Title)))]);
  end;
  Result := NyxObject([NyxField('cleared', NyxData(False)),
    NyxField('key', NyxData(ASpec.Key.Name)), NyxField('scope', NyxData(LScope)),
    NyxField('selection', NyxData(LSelection)), NyxField('parent', NyxData(ASpec.ParentField)),
    NyxField('offset', NyxData(AOffset)),
    NyxField('nextOffset', NyxData(Min(AOffset + Length(LColumns), ASpec.Count))),
    NyxField('total', NyxData(ASpec.Count)), NyxField('columns', NyxArray(LColumns))]);
end;

{$include nyx.studio.agents.collectionqueries.inc}

function TNyxAgentSession.Collections(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LMode: TNyxAgentCollectionMode;
  LFound: Boolean;
  LPatch: INyxCollectionPatch;
  LPair: TNyxProjectPair;
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LItem: TNyxCollectionItem;
  LValue: TNyxStateValue;
  LValues: array of TNyxDataValue;
  LCells: array of TNyxDataValue;
  LFields: array of TNyxCollectionField;
  LFieldData: TNyxDataValue;
  LDomain: TNyxDataValue;
  LChoices: TNyxDataValue;
  LItemContext: TNyxDataValue;
  LChoiceContext: TNyxDataValue;
  LKey: TNyxCollectionRef;
  LName: TNyxText;
  LFilter: TNyxText;
  LSource: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LOther: Integer;
  LCount: Integer;
  LChoice: Integer;
  LNode: TNyxNode;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LLocal: TNyxDataValue;
  LEffective: TNyxDataValue;
  LRestorable: TNyxDataValue;
  LColumn: TNyxCollectionColumn;
  LProjectionName: TNyxText;
  LColumnSource: TNyxAgentCollectionSource;
begin
  LFound := False;
  for LMode := Low(TNyxAgentCollectionMode) to High(TNyxAgentCollectionMode) do
  begin

    if CCollectionModes[LMode] = TextArgument(AArguments, 'mode') then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Unknown collection query/apply mode');
  end;

  if LMode = acmApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);
    LPatch := ReadNyxCollectionPatch(AArguments.Field('changes'));
    LPair := LPatch.Candidate(FSession.ProjectSnapshot);
    Result := NyxObject([NyxField('changes', NyxData(LPatch.Count))]);
    { Preflight the complete response before the only active project publication.
      Actor authority, permissions and exact retry receipts belong to Call. }
    BoundContext(WithResults(Summary, Result, 'collections', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;

  if LMode = acmList then
  begin
    NyxAgentFields(AArguments, '|mode|offset|limit|filter|');
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, High(Integer));
    LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
    LFilter := TextArgument(AArguments, 'filter');
    LValues := nil;
    LTotal := 0;
    for LIndex := 0 to FSession.Document.Collections.Count - 1 do
    begin
      LKey := FSession.Document.Collections.Key(LIndex);

      if (LFilter <> '') and (Pos(LFilter, LKey.Name) = 0) then
      begin
        Continue;
      end;

      if (LTotal >= LOffset) and (Length(LValues) < LLimit) then
      begin
        LData := FSession.Document.Collections.Snapshot(LKey);
        SetLength(LValues, Length(LValues) + 1);
        LValues[High(LValues)] := NyxObject([NyxField('key', NyxData(LKey.Name)),
          NyxField('fields', NyxData(LData.Schema.Count)), NyxField('rows', NyxData(LData.Count))]);
      end;
      Inc(LTotal);
    end;
    Result := NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('offset', NyxData(LOffset)),
      NyxField('nextOffset', NyxData(Min(Min(LOffset, LTotal) + Length(LValues), LTotal))),
      NyxField('total', NyxData(LTotal)), NyxField('collections', NyxArray(LValues))]);
    Exit;
  end;

  if LMode in [acmBindings, acmColumn, acmQuery, acmQueryValue] then
  begin

    if LMode = acmBindings then
    begin
      NyxAgentFields(AArguments, '|mode|owner|offset|limit|');
      LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumCollectionFields);
      LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
    end
    else
    begin
      case LMode of
        acmColumn:
          begin
            NyxAgentFields(AArguments, '|mode|owner|field|source|offset|count|');
          end;
        acmQuery:
          begin
            NyxAgentFields(AArguments, '|mode|owner|source|offset|limit|');
          end;
        acmQueryValue:
          begin
            NyxAgentFields(AArguments, '|mode|owner|source|path|offset|count|');
          end;
      else
        begin
          raise ENyxCollection.Create('Unexpected collection owner query');
        end;
      end;
      LSource := AArguments.Field('source').AsText;
      LFound := False;
      for LColumnSource := Low(TNyxAgentCollectionSource) to High(TNyxAgentCollectionSource) do
      begin

        if CCollectionSources[LColumnSource] = LSource then
        begin
          LFound := True;
          Break;
        end;
      end;

      if not LFound then
      begin
        raise ENyxCollection.Create('Collection source must be local, effective or restorable');
      end;
      LOffset := 0;
      LLimit := 1;
    end;
    LName := AArguments.Field('owner').AsText;
    LNode := FSession.Document.Find(LName);

    if LNode = nil then
    begin
      raise ENyxCollection.Create('The exact authored collection owner is missing');
    end;
    LRoot := RealizeNyxContext(FSession.Document, LNode, LProjection);
    try
      LLocal := NyxNull;
      LEffective := NyxNull;
      LRestorable := NyxNull;
      LProjectionName := '';

      if LNode.HasCollectionView then
      begin
        LLocal := CollectionViewContext(LNode.CollectionView, LOffset, LLimit);

        if (LMode = acmBindings) and not LNode.CollectionView.Defined then
        begin
          LSpec := FSession.ClearedCollectionInheritance(LNode.ID);

          if LSpec.Defined then
          begin
            LRestorable := CollectionViewContext(LSpec, LOffset, LLimit);
          end;
        end;
      end;

      if LProjection <> nil then
      begin
        LProjectionName := LProjection.ProjectionKind;

        if LProjection.HasCollectionView then
        begin
          LEffective := CollectionViewContext(LProjection.CollectionView, LOffset, LLimit);
        end;
      end;

      if LMode = acmBindings then
      begin
        Result := NyxObject([NyxField('revision', NyxData(FRevision)),
          NyxField('owner', NyxData(LName)), NyxField('projection', NyxData(LProjectionName)),
          NyxField('supported', NyxData((LProjectionName = 'list') or
            (LProjectionName = 'table') or (LProjectionName = 'tree'))),
          NyxField('inherited', NyxData(not LNode.HasCollectionView and
            (LEffective.Kind <> ndNull))), NyxField('local', LLocal),
          NyxField('effective', LEffective)]);
        Result := WithResults(Result, LRestorable, 'restorable');
        Exit;
      end;
      LSpec := Default(TNyxCollectionViewSpec);

      if (LColumnSource = acsLocal) and LNode.HasCollectionView then
      begin
        LSpec := LNode.CollectionView;
      end;

      if (LColumnSource = acsEffective) and (LProjection <> nil) and LProjection.HasCollectionView then
      begin
        LSpec := LProjection.CollectionView;
      end;

      if LColumnSource = acsRestorable then
      begin
        LSpec := FSession.ClearedCollectionInheritance(LNode.ID);
      end;

      if LMode in [acmQuery, acmQueryValue] then
      begin
        Result := NyxObject([NyxField('revision', NyxData(FRevision)),
          NyxField('owner', NyxData(LName)), NyxField('source', NyxData(LSource)),
          NyxField('projection', NyxData(LProjectionName))]);

        if LMode = acmQuery then
        begin
          Exit(WithResults(Result, CollectionQueryPage(LSpec, AArguments), 'query'));
        end;
        Exit(WithResults(Result, CollectionQueryValue(LSpec, AArguments), 'value'));
      end;

      if LSpec.Defined then
      begin
        for LIndex := 0 to LSpec.Count - 1 do
        begin
          LColumn := LSpec.ColumnAt(LIndex);

          if LColumn.FieldName = AArguments.Field('field').AsText then
          begin
            Result := NyxObject([NyxField('revision', NyxData(FRevision)),
              NyxField('owner', NyxData(LName)), NyxField('source', NyxData(LSource)),
              NyxField('field', NyxData(LColumn.FieldName)),
              NyxField('kind', NyxData(NyxStateKindName(LColumn.Kind))),
              NyxField('editable', NyxData(LColumn.Mode = cmEditable)),
              NyxField('title', CollectionScalarWindow(
                TNyxStateValue.FromText(LColumn.Title), AArguments))]);
            Exit;
          end;
        end;
      end;
      raise ENyxCollection.Create('Exact authored/effective column is missing or cleared');
    finally
      LRoot.Free;
    end;
  end;

  case LMode of
    acmSchema:
      NyxAgentFields(AArguments, '|mode|key|offset|limit|');
    acmRows:
      NyxAgentFields(AArguments, '|mode|key|offset|limit|fields|');
    acmValue:
      NyxAgentFields(AArguments, '|mode|key|field|item|choice|offset|count|');
    acmDomain:
      NyxAgentFields(AArguments, '|mode|key|field|offset|limit|');
  else
    begin
      raise ENyxCollection.Create('Unexpected collection data query');
    end;
  end;
  LKey := NyxCollection(AArguments.Field('key').AsText);
  LData := FSession.Document.Collections.Snapshot(LKey);
  LSchema := LData.Schema;

  if LMode = acmValue then
  begin
    LField := CollectionField(LSchema, AArguments.Field('field').AsText);
    LValue := LField.DefaultValue;
    LSource := 'default';
    LItemContext := NyxNull;
    LChoiceContext := NyxNull;

    if NyxAgentHas(AArguments, 'item') and NyxAgentHas(AArguments, 'choice') then
    begin
      raise ENyxCollection.Create('Value query cannot combine a row with a domain choice');
    end;

    if NyxAgentHas(AArguments, 'item') then
    begin
      LItem := LData.Item(NyxItem(LKey, AArguments.Field('item').AsText));
      LValue := CollectionCell(LItem, LField);
      LSource := 'row';
      LItemContext := NyxData(LItem.Ref.ID);
    end;

    if NyxAgentHas(AArguments, 'choice') then
    begin
      LChoice := IntegerArgument(AArguments, 'choice', 0, 0, NyxMaximumDomainChoices - 1);
      LDomain := LField.Domain.ToData;

      if not LField.Domain.Defined or not NyxAgentHas(LDomain, 'choices') then
      begin
        raise ENyxCollection.Create('Field has no domain choices');
      end;
      LValue := CollectionScalar(LField.Kind, LDomain.Field('choices').Item(LChoice));
      LSource := 'choice';
      LChoiceContext := NyxData(LChoice);
    end;
    Result := NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('key', NyxData(LKey.Name)), NyxField('field', NyxData(LField.Name)),
      NyxField('source', NyxData(LSource)),
      NyxField('item', LItemContext), NyxField('choice', LChoiceContext),
      NyxField('value', CollectionScalarWindow(LValue, AArguments))]);
    Exit;
  end;

  if LMode = acmDomain then
  begin
    LField := CollectionField(LSchema, AArguments.Field('field').AsText);
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumDomainChoices);
    LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
    LChoices := NyxArray([]);
    LDomain := LField.Domain.ToData;

    if LField.Domain.Defined and NyxAgentHas(LDomain, 'choices') then
    begin
      LChoices := LDomain.Field('choices');
    end;
    LTotal := LChoices.Count;
    SetLength(LValues, Max(0, Min(LLimit, LTotal - LOffset)));
    for LIndex := 0 to High(LValues) do
    begin
      LValues[LIndex] := NyxObject([NyxField('index', NyxData(LOffset + LIndex)),
        NyxField('value', CollectionScalarPreview(CollectionScalar(
          LField.Kind, LChoices.Item(LOffset + LIndex))))]);
    end;
    Result := NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('key', NyxData(LKey.Name)), NyxField('field', NyxData(LField.Name)),
      NyxField('domain', CollectionDomainSummary(LField.Domain)),
      NyxField('offset', NyxData(LOffset)),
      NyxField('nextOffset', NyxData(Min(LOffset + Length(LValues), LTotal))),
      NyxField('total', NyxData(LTotal)), NyxField('choices', NyxArray(LValues))]);
    Exit;
  end;

  if LMode = acmSchema then
  begin
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumCollectionFields);
    LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
    LTotal := LSchema.Count;
    SetLength(LValues, Max(0, Min(LLimit, LTotal - LOffset)));
    for LIndex := 0 to High(LValues) do
    begin
      LField := LSchema.FieldAt(LOffset + LIndex);
      LValues[LIndex] := NyxObject([NyxField('name', NyxData(LField.Name)),
        NyxField('kind', NyxData(NyxStateKindName(LField.Kind))),
        NyxField('default', CollectionScalarPreview(LField.DefaultValue)),
        NyxField('domain', CollectionDomainSummary(LField.Domain))]);
    end;
  end
  else
  begin
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumCollectionItems);
    LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 20);
    LFields := nil;

    if NyxAgentHas(AArguments, 'fields') then
    begin
      LFieldData := AArguments.Field('fields');

      if (LFieldData.Kind <> ndArray) or (LFieldData.Count < 1) or (LFieldData.Count > 16) then
      begin
        raise ENyxCollection.Create('Row query selects 1..16 exact fields');
      end;
      SetLength(LFields, LFieldData.Count);
      for LIndex := 0 to High(LFields) do
      begin
        LFields[LIndex] := CollectionField(LSchema, LFieldData.Item(LIndex).AsText);
        for LOther := 0 to LIndex - 1 do
        begin

          if LFields[LOther].Name = LFields[LIndex].Name then
          begin
            raise ENyxCollection.Create('Duplicate queried row field');
          end;
        end;
      end;
    end;
    LTotal := LData.Count;
    SetLength(LValues, Max(0, Min(LLimit, LTotal - LOffset)));
    for LIndex := 0 to High(LValues) do
    begin
      LItem := LData.ItemAt(LOffset + LIndex);
      SetLength(LCells, Length(LFields));
      for LOther := 0 to High(LFields) do
      begin
        LCells[LOther] := NyxObject([NyxField('field', NyxData(LFields[LOther].Name)),
          NyxField('value', CollectionScalarPreview(CollectionCell(LItem, LFields[LOther])))]);
      end;
      LValues[LIndex] := NyxObject([NyxField('item', NyxData(LItem.Ref.ID)),
        NyxField('values', NyxArray(LCells))]);
    end;
  end;
  LName := 'fields';

  if LMode = acmRows then
  begin
    LName := 'rows';
  end;
  LCount := Length(LValues);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('key', NyxData(LKey.Name)), NyxField('offset', NyxData(LOffset)),
    NyxField('nextOffset', NyxData(Min(LOffset + LCount, LTotal))),
    NyxField('total', NyxData(LTotal)), NyxField(LName, NyxArray(LValues))]);
end;
