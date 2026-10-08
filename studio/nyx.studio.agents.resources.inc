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


{ Included by nyx.studio.agents. Bounded authored-resource discovery never fetches
  a URL or reports an independent runtime cache as document state. }

type
  TNyxAgentResourceMode = (armList, armDetails, armContent, armJSON, armBindings, armApply,
    armSources, armRows);

const
  CResourceModes: array[TNyxAgentResourceMode] of TNyxText =
    ('list', 'details', 'content', 'json', 'bindings', 'apply', 'sources', 'rows');
  CResourceDataKinds: array[TNyxDataKind] of TNyxText =
    ('null', 'text', 'boolean', 'number', 'object', 'array');

function ResourceTextWindow(const AText: TNyxText; AOffset, ACount: Integer): TNyxDataValue;
var
  LTotal: Integer;
  LText: TNyxText;
begin
  LText := TextSpan(AText, AOffset, ACount, LTotal);

  if AOffset > LTotal then
  begin
    raise ENyxResource.Create('Resource scalar offset is beyond the authored text');
  end;
  Result := NyxObject([NyxField('unit', NyxData('unicode-scalar')),
    NyxField('offset', NyxData(AOffset)), NyxField('characters', NyxData(LTotal)),
    NyxField('nextOffset', NyxData(Min(AOffset + ACount, LTotal))),
    NyxField('text', NyxData(LText))]);
end;

function ResourceTextCount(const AText: TNyxText): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  LIndex := 1;
  Result := 0;
  while NyxNextScalar(AText, LIndex, LScalar) do
  begin
    Inc(Result);
  end;
end;

function ResourceValueSummary(const AValue: TNyxDataValue): TNyxDataValue;
begin
  case AValue.Kind of
    ndText:
      Result := NyxObject([NyxField('kind', NyxData('text')),
        NyxField('preview', ResourceTextWindow(AValue.AsText, 0, 80))]);
    ndObject, ndArray:
      Result := NyxObject([NyxField('kind', NyxData(CResourceDataKinds[AValue.Kind])),
        NyxField('children', NyxData(AValue.Count))]);
  else
    Result := NyxObject([NyxField('kind', NyxData(CResourceDataKinds[AValue.Kind])),
      NyxField('value', AValue)]);
  end;
end;

function TNyxAgentSession.Resources(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LMode: TNyxAgentResourceMode;
  LFound: Boolean;
  LPatch: INyxResourcePatch;
  LResources: INyxResources;
  LDefinition: INyxResourceDefinition;
  LPayload: INyxResourceDefinition;
  LSource: TNyxResourceSource;
  LReference: TNyxResourceRef;
  LLocale: TNyxLocaleRef;
  LName: TNyxText;
  LFilter: TNyxText;
  LOrigin: TNyxText;
  LWindow: TNyxDataValue;
  LData: TNyxDataValue;
  LChild: TNyxDataValue;
  LPath: TNyxResourcePath;
  LChildPath: TNyxResourcePath;
  LValues: array of TNyxDataValue;
  LBytes: TNyxBytes;
  LSlice: TNyxBytes;
  LOffset: Integer;
  LLimit: Integer;
  LTextOffset: Integer;
  LTextCount: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LCount: Integer;
  LCollection: TNyxCollectionRef;
  LRows: TNyxResourceRows;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
begin
  LFound := False;
  for LMode := Low(TNyxAgentResourceMode) to High(TNyxAgentResourceMode) do
  begin

    if CResourceModes[LMode] = TextArgument(AArguments, 'mode') then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxResource.Create('Unknown resource query/apply mode');
  end;

  if LMode = armApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);
    LPatch := ReadNyxResourcePatch(AArguments.Field('changes'));
    Result := NyxObject([NyxField('changes', NyxData(LPatch.Count))]);
    BoundContext(WithResults(Summary, Result, 'resources', True));
    { ApplyPatch owns source reconciliation, draft refusal and paired history.
      Call owns permission, revision, actor-scoped receipts and visible activity. }
    FSession.ApplyPatch(LPatch);
    Exit;
  end;

  if LMode = armBindings then
  begin
    { Same local/effective property discovery as the ordinary binding inspector;
      BindingData includes the typed resource selector rather than a state name. }
    Result := StateBindings(AArguments);
    Exit;
  end;
  LResources := FSession.Document.Resources;

  if LMode = armSources then
  begin
    NyxAgentFields(AArguments, '|mode|offset|limit|filter|');
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumCollections);
    LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 16);
    LFilter := TextArgument(AArguments, 'filter');
    LValues := nil;
    LTotal := 0;
    for LIndex := 0 to FSession.Document.Collections.Count - 1 do
    begin
      LCollection := FSession.Document.Collections.Key(LIndex);

      if not NyxCollectionResourceSource(FSession.Document.Collections, LCollection, LRows) or
        ((LFilter <> '') and (Pos(LFilter, LCollection.Name) = 0) and
          (Pos(LFilter, LRows.Reference.Name) = 0)) then
      begin
        Continue;
      end;

      if (LTotal >= LOffset) and (Length(LValues) < LLimit) then
      begin
        LCount := Length(LValues);
        SetLength(LValues, LCount + 1);
        LValues[LCount] := NyxObject([NyxField('collection', NyxData(LCollection.Name)),
          NyxField('resource', NyxData(LRows.Reference.Name)),
          NyxField('fields', NyxData(LRows.Schema.Count))]);
      end;
      Inc(LTotal);
    end;
    Exit(NyxObject([NyxField('origin', NyxData('saved-resource-relationships')),
      NyxField('total', NyxData(LTotal)), NyxField('offset', NyxData(LOffset)),
      NyxField('nextOffset', NyxData(Min(LOffset + Length(LValues), LTotal))),
      NyxField('sources', NyxArray(LValues))]));
  end;

  if LMode = armRows then
  begin
    NyxAgentFields(AArguments, '|mode|collection|offset|limit|');
    LCollection := NyxCollection(TextArgument(AArguments, 'collection'));

    if not NyxCollectionResourceSource(FSession.Document.Collections, LCollection, LRows) then
    begin
      raise ENyxResource.Create('The collection has no saved resource relationship');
    end;
    LSchema := LRows.Schema;
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, LSchema.Count);
    LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 16);
    SetLength(LValues, Min(LLimit, LSchema.Count - LOffset));
    for LIndex := 0 to High(LValues) do
    begin
      LField := LSchema.FieldAt(LOffset + LIndex);
      LValues[LIndex] := NyxObject([NyxField('name', NyxData(LField.Name)),
        NyxField('type', NyxData(NyxStateKindName(LField.Kind))),
        NyxField('path', LRows.FieldPath(LOffset + LIndex).ToData)]);
    end;
    { Recipes/schema seeds describe authored intent, not loaded runtime rows.
      Source data is queried separately through the existing bounded JSON mode. }
    Exit(NyxObject([NyxField('origin', NyxData('saved-resource-relationship')),
      NyxField('collection', NyxData(LCollection.Name)),
      NyxField('resource', NyxData(LRows.Reference.Name)), NyxField('path', LRows.Path.ToData),
      NyxField('identity', LRows.IdentityPath.ToData), NyxField('fieldCount', NyxData(LSchema.Count)),
      NyxField('offset', NyxData(LOffset)),
      NyxField('nextOffset', NyxData(LOffset + Length(LValues))),
      NyxField('fields', NyxArray(LValues)), NyxField('runtimeRowsIncluded', NyxData(False))]));
  end;

  if LMode = armList then
  begin
    NyxAgentFields(AArguments, '|mode|offset|limit|filter|');
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumResources);
    LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 16);
    LFilter := TextArgument(AArguments, 'filter');
    LValues := nil;
    LTotal := 0;
    for LIndex := 0 to LResources.Count - 1 do
    begin
      LReference := LResources.Reference(LIndex);
      LLocale := LResources.Locale(LIndex);
      LDefinition := LResources.Definition(LReference, LLocale);

      if (LFilter <> '') and (Pos(LFilter, LReference.Name) = 0) and
        (Pos(LFilter, LDefinition.Title) = 0) and
        (Pos(LFilter, LDefinition.Description) = 0) then
      begin
        Continue;
      end;

      if (LTotal >= LOffset) and (Length(LValues) < LLimit) then
      begin
        LOrigin := 'embedded';

        if LDefinition.Source.Kind = rskHosted then
        begin
          LOrigin := 'hosted';
        end;
        SetLength(LValues, Length(LValues) + 1);
        LValues[High(LValues)] := NyxObject([
          NyxField('name', NyxData(LReference.Name)), NyxField('locale', NyxData(LLocale.Name)),
          NyxField('kind', NyxData(NyxResourceKindName(LDefinition.Kind))),
          NyxField('source', NyxData(LOrigin)), NyxField('bytes', NyxData(LDefinition.ByteCount)),
          NyxField('fallback', NyxData(LDefinition.FallbackDefinition <> nil)),
          NyxField('title', ResourceTextWindow(LDefinition.Title, 0, 80))]);
      end;
      Inc(LTotal);
    end;
    Result := NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
      NyxField('nextOffset', NyxData(Min(Min(LOffset, LTotal) + Length(LValues), LTotal))),
      NyxField('resources', NyxArray(LValues))]);
    Exit;
  end;

  if LMode = armJSON then
  begin
    NyxAgentFields(AArguments, '|mode|name|locale|path|offset|limit|textOffset|textCount|');
  end
  else
  begin
    NyxAgentFields(AArguments, '|mode|name|locale|offset|count|');
  end;
  LReference := NyxResourceRef(AArguments.Field('name').AsText);
  LName := AArguments.Field('locale').AsText;
  LLocale := NyxDefaultLocale;

  if LName <> '' then
  begin
    LLocale := NyxLocale(LName);
  end;
  { Exact variant lookup. An agent never accidentally edits a locale selected
    through fallback. Payload queries explicitly identify authored fallback. }
  LDefinition := LResources.Definition(LReference, LLocale);
  LSource := LDefinition.Source;
  LOrigin := 'embedded';

  if LSource.Kind = rskHosted then
  begin
    LOrigin := 'authored-fallback';
  end;
  LPayload := LDefinition;

  if LSource.Kind = rskHosted then
  begin
    LPayload := LDefinition.FallbackDefinition;
  end;

  if LMode = armDetails then
  begin
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumPackedBytes);
    LLimit := IntegerArgument(AArguments, 'count', 256, 1, 1024);
    LData := NyxNull;

    if LSource.Kind = rskHosted then
    begin
      LData := LSource.ToData;
    end;
    { Metadata offsets may exceed a shorter title when paging a description.
      Clamp each separately and report each exact total/next cursor. }
    Result := NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('name', NyxData(LReference.Name)), NyxField('locale', NyxData(LLocale.Name)),
      NyxField('kind', NyxData(NyxResourceKindName(LDefinition.Kind))),
      NyxField('hosted', NyxData(LSource.Kind = rskHosted)),
      NyxField('source', LData), NyxField('bytes', NyxData(LDefinition.ByteCount)),
      NyxField('fallback', NyxData(LDefinition.FallbackDefinition <> nil)),
      NyxField('title', ResourceTextWindow(LDefinition.Title,
        Min(LOffset, ResourceTextCount(LDefinition.Title)), LLimit)),
      NyxField('description', ResourceTextWindow(LDefinition.Description,
        Min(LOffset, ResourceTextCount(LDefinition.Description)), LLimit))]);
    Exit;
  end;

  if LPayload = nil then
  begin
    raise ENyxResource.Create('The hosted resource has no authored fallback; this tool never fetches');
  end;

  if LMode = armContent then
  begin
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumPackedBytes);
    LLimit := IntegerArgument(AArguments, 'count', 2048, 1, 4096);

    if LPayload.Kind in [nrkText, nrkJSON] then
    begin
      { Raw UTF-8 decoding preserves JSON whitespace and decimal spelling too. }
      LWindow := ResourceTextWindow(NyxDecodeUTF8(LPayload.Bytes), LOffset, LLimit);
    end
    else
    begin
      LBytes := LPayload.Bytes;
      LTotal := Length(LBytes);

      if LOffset > LTotal then
      begin
        raise ENyxResource.Create('Resource byte offset is beyond the authored payload');
      end;
      LCount := Min(LLimit, LTotal - LOffset);
      SetLength(LSlice, LCount);
      for LIndex := 0 to LCount - 1 do
      begin
        LSlice[LIndex] := LBytes[LOffset + LIndex];
      end;
      { Each Base64 window decodes independently. Concatenate decoded bytes,
        never padded Base64 strings from consecutive windows. }
      LWindow := NyxObject([NyxField('unit', NyxData('byte')),
        NyxField('offset', NyxData(LOffset)), NyxField('bytes', NyxData(LTotal)),
        NyxField('nextOffset', NyxData(LOffset + LCount)),
        NyxField('base64', NyxData(NyxEncodeBase64(LSlice)))]);
    end;
    Result := NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('name', NyxData(LReference.Name)), NyxField('locale', NyxData(LLocale.Name)),
      NyxField('kind', NyxData(NyxResourceKindName(LPayload.Kind))),
      NyxField('origin', NyxData(LOrigin)), NyxField('content', LWindow)]);
    Exit;
  end;

  if LPayload.Kind <> nrkJSON then
  begin
    raise ENyxResource.Create('Structural paths require a JSON resource');
  end;
  LPath := TNyxResourcePath.FromData(AArguments.Field('path'));
  LData := LPath.Select(LPayload.Data);
  LValues := nil;

  if LData.Kind in [ndObject, ndArray] then
  begin

    if NyxAgentHas(AArguments, 'textOffset') or NyxAgentHas(AArguments, 'textCount') then
    begin
      raise ENyxResource.Create('JSON containers use child pages, not scalar text windows');
    end;
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumPackedBytes);
    LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 16);
    LTotal := LData.Count;

    if LOffset > LTotal then
    begin
      raise ENyxResource.Create('JSON child offset is beyond this container');
    end;
    for LIndex := LOffset to Min(LOffset + LLimit, LTotal) - 1 do
    begin

      if LData.Kind = ndObject then
      begin
        LName := LData.Key(LIndex);
        LChildPath := LPath.Field(LName);
        LChild := LData.Field(LName);
      end
      else
      begin
        LChildPath := LPath.Item(LIndex);
        LChild := LData.Item(LIndex);
      end;
      SetLength(LValues, Length(LValues) + 1);
      LValues[High(LValues)] := NyxObject([NyxField('path', LChildPath.ToData),
        NyxField('summary', ResourceValueSummary(LChild))]);
    end;
    LWindow := NyxObject([NyxField('kind', NyxData(CResourceDataKinds[LData.Kind])),
      NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
      NyxField('nextOffset', NyxData(Min(LOffset + Length(LValues), LTotal))),
      NyxField('children', NyxArray(LValues))]);
  end
  else
  begin

    if NyxAgentHas(AArguments, 'offset') or NyxAgentHas(AArguments, 'limit') then
    begin
      raise ENyxResource.Create('JSON scalar values do not have child pages');
    end;

    if LData.Kind = ndText then
    begin
      LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, NyxMaximumPackedBytes);
      LTextCount := IntegerArgument(AArguments, 'textCount', 2048, 1, 4096);
      LWindow := NyxObject([NyxField('kind', NyxData('text')),
        NyxField('content', ResourceTextWindow(LData.AsText, LTextOffset, LTextCount))]);
    end
    else
    begin

      if NyxAgentHas(AArguments, 'textOffset') or NyxAgentHas(AArguments, 'textCount') then
      begin
        raise ENyxResource.Create('Only JSON text has Unicode scalar windows');
      end;
      LWindow := ResourceValueSummary(LData);
    end;
  end;
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('name', NyxData(LReference.Name)), NyxField('locale', NyxData(LLocale.Name)),
    NyxField('origin', NyxData(LOrigin)), NyxField('path', LPath.ToData),
    NyxField('value', LWindow)]);
end;
