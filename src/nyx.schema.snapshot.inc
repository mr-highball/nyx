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


{ Immutable creator environments. Native publication/read leases are short;
  they never hold a lock across parsing, user work, or a target callback. }
procedure LockSchemas;
begin
  {$ifndef PAS2JS}
  EnterCriticalSection(GSchemaLock);
  {$endif}
end;

procedure UnlockSchemas;
begin
  {$ifndef PAS2JS}
  LeaveCriticalSection(GSchemaLock);
  {$endif}
end;

function CurrentSchemas: TNyxPublishedSchemas;
begin

  if GSchemaEnvironment <> nil then
  begin
    Exit(GSchemaEnvironment.Schemas);
  end;
  LockSchemas;
  try
    Result := GPublishedSchemas;
  finally
    UnlockSchemas;
  end;
end;

function NyxSchemaRevision: Integer;
begin
  LockSchemas;
  try
    Result := GSchemaRevision;
  finally
    UnlockSchemas;
  end;
end;

function CommitNyxSchemaRevision(ARevision: Integer;
  const AAction: INyxSchemaAction): Boolean;
begin

  if AAction = nil then
  begin
    raise ENyxModel.Create('Creator-guarded publication requires a synchronous action');
  end;
  LockSchemas;
  try
    Result := GSchemaRevision = ARevision;

    if Result then
    begin
      AAction.Execute;
    end;
  finally
    UnlockSchemas;
  end;
end;

function TNyxSchemaSnapshot.GetRevision: Integer;
begin
  Result := CapturedRevision;
end;

procedure TNyxSchemaSnapshot.Execute(const AAction: INyxSchemaAction);
var
  LPrevious: TNyxSchemaSnapshot;
  LLease: INyxSchemaSnapshot;
begin

  if AAction = nil then
  begin
    raise ENyxModel.Create('A schema environment requires synchronous work');
  end;
  LLease := Self;
  LPrevious := GSchemaEnvironment;
  GSchemaEnvironment := Self;
  try
    AAction.Execute;
  finally
    GSchemaEnvironment := LPrevious;
    LLease := nil;
  end;
end;

function CaptureNyxSchemas: INyxSchemaSnapshot;
var
  LOwner: TNyxSchemaSnapshot;
  LCurrent: TNyxPublishedSchemas;
  LIndex: Integer;
  LMember: Integer;
begin
  LOwner := TNyxSchemaSnapshot.Create;
  Result := LOwner;

  if GSchemaEnvironment <> nil then
  begin
    LCurrent := GSchemaEnvironment.Schemas;
    LOwner.CapturedRevision := GSchemaEnvironment.CapturedRevision;
  end
  else
  begin
    LockSchemas;
    try
      LCurrent := GPublishedSchemas;
      LOwner.CapturedRevision := GSchemaRevision;
    finally
      UnlockSchemas;
    end;
  end;
  SetLength(LOwner.Schemas, Length(LCurrent));
  for LIndex := 0 to High(LCurrent) do
  begin
    LOwner.Schemas[LIndex].Kind := LCurrent[LIndex].Kind;
    SetLength(LOwner.Schemas[LIndex].Properties, Length(LCurrent[LIndex].Properties));
    for LMember := 0 to High(LCurrent[LIndex].Properties) do
    begin
      LOwner.Schemas[LIndex].Properties[LMember] := LCurrent[LIndex].Properties[LMember];
    end;
    SetLength(LOwner.Schemas[LIndex].Events, Length(LCurrent[LIndex].Events));
    for LMember := 0 to High(LCurrent[LIndex].Events) do
    begin
      LOwner.Schemas[LIndex].Events[LMember] := LCurrent[LIndex].Events[LMember].Copy;
    end;
  end;
end;

function SchemaOrdinal(const AValue: TNyxDataValue; AMaximum: Integer): Integer;
begin
  Result := AValue.AsInteger;

  if (Result < 0) or (Result > AMaximum) then
  begin
    raise ENyxModel.Create('Schema transport contains an unknown enum value');
  end;
end;

procedure SchemaFields(const AValue: TNyxDataValue; const AKeys: TNyxText;
  ACount: Integer);
var
  LIndex: Integer;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> ACount) then
  begin
    raise ENyxModel.Create('Schema transport requires its complete versioned fields');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if Pos('|' + AValue.Key(LIndex) + '|', AKeys) = 0 then
    begin
      raise ENyxModel.Create('Unknown schema transport field: ' + AValue.Key(LIndex));
    end;
  end;
end;

function SchemaContextMask(AContexts: TNyxEventContexts): Integer;
var
  LKind: TNyxEventContextKind;
begin
  Result := 0;
  for LKind := Low(TNyxEventContextKind) to High(TNyxEventContextKind) do
  begin

    if LKind in AContexts then
    begin
      Result := Result or (1 shl Ord(LKind));
    end;
  end;
end;

function SchemaEventData(const AEvent: TNyxEventSchema): TNyxDataValue;
var
  LPayload: TNyxDataValue;
  LRoutes: array of TNyxDataValue;
  LIndex: Integer;
begin
  LPayload := NyxNull;

  if AEvent.Payload.Defined then
  begin
    LPayload := AEvent.Payload.ToData;
  end;
  SetLength(LRoutes, Length(AEvent.Routes));
  for LIndex := 0 to High(LRoutes) do
  begin
    LRoutes[LIndex] := AEvent.Routes[LIndex].ToData;
  end;
  Result := NyxObject([
    NyxField('trigger', NyxData(Ord(AEvent.Trigger))),
    NyxField('name', NyxData(AEvent.Name.Name)), NyxField('payload', LPayload),
    NyxField('title', NyxData(AEvent.Title)), NyxField('description', NyxData(AEvent.Description)),
    NyxField('browser', NyxData(Ord(AEvent.Browser))),
    NyxField('native', NyxData(Ord(AEvent.Native))),
    NyxField('optional', NyxData(AEvent.PayloadOptional)),
    NyxField('producer', NyxData(AEvent.DeclaredProducer)),
    NyxField('contexts', NyxData(SchemaContextMask(AEvent.Contexts))),
    NyxField('routes', NyxArray(LRoutes))]);
end;

function TNyxSchemaSnapshot.ToData: TNyxDataValue;
var
  LRows: array of TNyxDataValue;
  LProperties: array of TNyxDataValue;
  LEvents: array of TNyxDataValue;
  LProperty: TNyxPropertyInfo;
  LIndex: Integer;
  LMember: Integer;
begin
  SetLength(LRows, Length(Schemas));
  for LIndex := 0 to High(Schemas) do
  begin
    SetLength(LProperties, Length(Schemas[LIndex].Properties));
    for LMember := 0 to High(LProperties) do
    begin
      LProperty := Schemas[LIndex].Properties[LMember];
      LProperties[LMember] := NyxObject([
        NyxField('key', NyxData(LProperty.Key)), NyxField('title', NyxData(LProperty.Title)),
        NyxField('type', NyxData(Ord(LProperty.ValueType))),
        NyxField('default', NyxData(LProperty.DefaultValue)),
        NyxField('choices', NyxData(LProperty.Choices)),
        NyxField('minimum', NyxData(LProperty.Minimum)),
        NyxField('maximum', NyxData(LProperty.Maximum)),
        NyxField('advanced', NyxData(LProperty.Advanced)),
        NyxField('support', NyxObject([
          NyxField('defined', NyxData(LProperty.Support.Defined)),
          NyxField('meaning', NyxData(Ord(LProperty.Support.Meaning))),
          NyxField('browser', NyxData(Ord(LProperty.Support.Browser))),
          NyxField('native', NyxData(Ord(LProperty.Support.Native))),
          NyxField('description', NyxData(LProperty.Support.Description))]))]);
    end;
    SetLength(LEvents, Length(Schemas[LIndex].Events));
    for LMember := 0 to High(LEvents) do
    begin
      LEvents[LMember] := SchemaEventData(Schemas[LIndex].Events[LMember]);
    end;
    LRows[LIndex] := NyxObject([NyxField('kind', NyxData(Schemas[LIndex].Kind)),
      NyxField('properties', NyxArray(LProperties)), NyxField('events', NyxArray(LEvents))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('revision', NyxData(CapturedRevision)), NyxField('schemas', NyxArray(LRows))]);
end;

function ReadSchemaEvent(const AData: TNyxDataValue): TNyxEventSchema;
var
  LMask: Integer;
  LContexts: TNyxEventContexts;
  LKind: TNyxEventContextKind;
  LRoutes: TNyxDataValue;
begin
  SchemaFields(AData, '|trigger|name|payload|title|description|browser|native|' +
    'optional|producer|contexts|routes|', 11);
  Result := Default(TNyxEventSchema);
  Result.Trigger := TNyxTrigger(SchemaOrdinal(AData.Field('trigger'), Ord(High(TNyxTrigger))));
  Result.Name := NyxEvent(AData.Field('name').AsText);
  Result.Title := AData.Field('title').AsText;
  Result.Description := AData.Field('description').AsText;
  Result.Browser := TNyxCapability(SchemaOrdinal(AData.Field('browser'), Ord(High(TNyxCapability))));
  Result.Native := TNyxCapability(SchemaOrdinal(AData.Field('native'), Ord(High(TNyxCapability))));
  Result.PayloadOptional := AData.Field('optional').AsBoolean;
  Result.DeclaredProducer := AData.Field('producer').AsBoolean;

  if AData.Field('payload').Kind <> ndNull then
  begin
    Result.Payload := TNyxEventPayloadSpec.FromData(AData.Field('payload'));
  end;
  LMask := SchemaOrdinal(AData.Field('contexts'),
    (1 shl (Ord(High(TNyxEventContextKind)) + 1)) - 1);
  LContexts := [];
  for LKind := Low(TNyxEventContextKind) to High(TNyxEventContextKind) do
  begin

    if (LMask and (1 shl Ord(LKind))) <> 0 then
    begin
      Include(LContexts, LKind);
    end;
  end;
  Result.Contexts := LContexts;
  LRoutes := AData.Field('routes');

  if (LRoutes.Kind <> ndArray) or (LRoutes.Count <> 0) then
  begin
    raise ENyxModel.Create('Global schema transport cannot contain resolved view routes');
  end;

  if not NyxIsRuntimeTrigger(Result.Trigger) and (Result.Trigger <> ntNamed) then
  begin
    raise ENyxModel.Create('Published schema events require runtime triggers');
  end;

  if (Result.Trigger = ntNamed) and
    ((Trim(Result.Name.Name) = '') or not Result.Payload.Defined) then
  begin
    raise ENyxModel.Create('Named schema transport requires its name and payload');
  end;

  if (Result.Trigger <> ntNamed) and (Result.Name.Name <> '') then
  begin
    raise ENyxModel.Create('Physical schema transport cannot name a stream');
  end;

  if Result.DeclaredProducer <> (Result.Trigger = ntNamed) then
  begin
    raise ENyxModel.Create('Schema producer authority must match its declaration');
  end;

  if Result.Trigger = ntNamed then
  begin
    NyxNamedEvent(Result.Name.Name);
  end;
end;

function ReadNyxSchemaSnapshot(const AData: TNyxDataValue): INyxSchemaSnapshot;
var
  LOwner: TNyxSchemaSnapshot;
  LRows: TNyxDataValue;
  LRow: TNyxDataValue;
  LProperties: TNyxDataValue;
  LEvents: TNyxDataValue;
  LData: TNyxDataValue;
  LSupport: TNyxDataValue;
  LProperty: TNyxPropertyInfo;
  LIndex: Integer;
  LMember: Integer;
  LOther: Integer;
begin
  SchemaFields(AData, '|version|revision|schemas|', 3);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxModel.Create('Unsupported schema snapshot version');
  end;
  LOwner := TNyxSchemaSnapshot.Create;
  Result := LOwner;
  LOwner.CapturedRevision := SchemaOrdinal(AData.Field('revision'), High(Integer));
  LRows := AData.Field('schemas');

  if LRows.Kind <> ndArray then
  begin
    raise ENyxModel.Create('Schema snapshot requires an array');
  end;
  SetLength(LOwner.Schemas, LRows.Count);
  for LIndex := 0 to LRows.Count - 1 do
  begin
    LRow := LRows.Item(LIndex);
    SchemaFields(LRow, '|kind|properties|events|', 3);
    LOwner.Schemas[LIndex].Kind := LRow.Field('kind').AsText;

    if Trim(LOwner.Schemas[LIndex].Kind) = '' then
    begin
      raise ENyxModel.Create('Schema snapshot requires its component kind');
    end;
    for LOther := 0 to LIndex - 1 do
    begin

      if LOwner.Schemas[LOther].Kind = LOwner.Schemas[LIndex].Kind then
      begin
        raise ENyxModel.Create('Duplicate kind in schema snapshot');
      end;
    end;
    LProperties := LRow.Field('properties');
    LEvents := LRow.Field('events');

    if (LProperties.Kind <> ndArray) or (LEvents.Kind <> ndArray) then
    begin
      raise ENyxModel.Create('Schema properties/events require arrays');
    end;
    SetLength(LOwner.Schemas[LIndex].Properties, LProperties.Count);
    for LMember := 0 to LProperties.Count - 1 do
    begin
      LData := LProperties.Item(LMember);
      SchemaFields(LData, '|key|title|type|default|choices|minimum|maximum|advanced|support|', 9);
      LProperty := Default(TNyxPropertyInfo);
      LProperty.Key := LData.Field('key').AsText;
      LProperty.Title := LData.Field('title').AsText;
      LProperty.ValueType := TNyxPropertyType(SchemaOrdinal(LData.Field('type'),
        Ord(High(TNyxPropertyType))));
      LProperty.DefaultValue := LData.Field('default').AsText;
      LProperty.Choices := LData.Field('choices').AsText;
      LProperty.Minimum := LData.Field('minimum').AsInteger;
      LProperty.Maximum := LData.Field('maximum').AsInteger;
      LProperty.Advanced := LData.Field('advanced').AsBoolean;
      LSupport := LData.Field('support');
      SchemaFields(LSupport, '|defined|meaning|browser|native|description|', 5);
      LProperty.Support.FDefined := LSupport.Field('defined').AsBoolean;
      LProperty.Support.FMeaning := TNyxPropertyMeaning(SchemaOrdinal(LSupport.Field('meaning'),
        Ord(High(TNyxPropertyMeaning))));
      LProperty.Support.FBrowser := TNyxCapability(SchemaOrdinal(LSupport.Field('browser'),
        Ord(High(TNyxCapability))));
      LProperty.Support.FNative := TNyxCapability(SchemaOrdinal(LSupport.Field('native'),
        Ord(High(TNyxCapability))));
      LProperty.Support.FDescription := LSupport.Field('description').AsText;

      if (LProperty.Key = '') or (LProperty.Minimum > LProperty.Maximum) then
      begin
        raise ENyxModel.Create('Invalid property in schema snapshot');
      end;
      for LOther := 0 to LMember - 1 do
      begin

        if LOwner.Schemas[LIndex].Properties[LOther].Key = LProperty.Key then
        begin
          raise ENyxModel.Create('Duplicate property in schema snapshot');
        end;
      end;
      LOwner.Schemas[LIndex].Properties[LMember] := LProperty;
    end;
    SetLength(LOwner.Schemas[LIndex].Events, LEvents.Count);
    for LMember := 0 to LEvents.Count - 1 do
    begin
      LOwner.Schemas[LIndex].Events[LMember] := ReadSchemaEvent(LEvents.Item(LMember));
      for LOther := 0 to LMember - 1 do
      begin

        if (LOwner.Schemas[LIndex].Events[LOther].Trigger =
          LOwner.Schemas[LIndex].Events[LMember].Trigger) and
          (LOwner.Schemas[LIndex].Events[LOther].Name.Name =
          LOwner.Schemas[LIndex].Events[LMember].Name.Name) then
        begin
          raise ENyxModel.Create('Duplicate event in schema snapshot');
        end;
      end;
    end;
  end;
end;
