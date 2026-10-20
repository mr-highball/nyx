{ Nyx documentation uses the existing metadata authorities. MIT license, see LICENSE. }
unit nyx.documentation.schema;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.scheduler, nyx.schema, nyx.catalog, nyx.documentation;

type
  { Detached binding of a declared public API to one explicit creator kind.
    Registered property/event facts retain their original owners and are never
    inferred from a class name, recipe tree, documentation prose or reflection.
    HasSchema distinguishes missing facts from a genuinely empty declaration. }
  INyxDocumentationSchema = interface(IInterface)
    ['{A57772DC-C625-4EC8-B49E-979880182F4D}']
    function Declaration: INyxDocumentationType;
    function CreatorKind: TNyxKindRef;
    function SchemaRevision: Integer;
    function HasSchema: Boolean;
    function FactsEvidence: TNyxDocumentationEvidence;
    function PropertyCount: Integer;
    function PropertyAt(AIndex: Integer): TNyxPropertyInfo;
    function EventCount: Integer;
    function EventAt(AIndex: Integer): TNyxEventSchema;
    function HasCatalog: Boolean;
    function CatalogInfo: TNyxComponentInfo;
    function HasPrimitive: Boolean;
    function PrimitiveInfo: TNyxPrimitiveInfo;
    { Existing creator description takes precedence when present; decoration
      supplies a fallback, never an override of schema/default/support facts. }
    function Summary: TNyxText;
    function SummaryEvidence: TNyxDocumentationEvidence;
  end;

{ Copy bounded typed facts from the supplied immutable schema environment and
  explicit catalog. No Execute/global scope, NewNode, recipe, callback, example
  or control constructor runs. Alternate snapshots lacking typed inspection
  refuse explicitly. Mutable catalog callers serialize this synchronous read. }
function CaptureNyxDocumentationSchema(const ADeclaration: INyxDocumentationType;
  const AKind: TNyxKindRef; const ASchemas: INyxSchemaSnapshot;
  ACatalog: TNyxCatalog = nil;
  const ACancellation: INyxCancellationScope = nil): INyxDocumentationSchema;

implementation

uses SysUtils, nyx.event.payload;

type
  { Only detached descriptor/value rows survive. The catalog, schema producer,
    cancellation scope and registration environment are never retained. }
  TDocumentationSchema = class(TInterfacedObject, INyxDocumentationSchema)
  public
    API: INyxDocumentationType;
    KindRef: TNyxKindRef;
    Revision: Integer;
    SchemaPresent, CatalogPresent, PrimitivePresent: Boolean;
    Properties: array of TNyxPropertyInfo;
    Events: array of TNyxEventSchema;
    Catalog: TNyxComponentInfo;
    Primitive: TNyxPrimitiveInfo;
    function Declaration: INyxDocumentationType;
    function CreatorKind: TNyxKindRef;
    function SchemaRevision: Integer;
    function HasSchema: Boolean;
    function FactsEvidence: TNyxDocumentationEvidence;
    function PropertyCount: Integer;
    function PropertyAt(AIndex: Integer): TNyxPropertyInfo;
    function EventCount: Integer;
    function EventAt(AIndex: Integer): TNyxEventSchema;
    function HasCatalog: Boolean;
    function CatalogInfo: TNyxComponentInfo;
    function HasPrimitive: Boolean;
    function PrimitiveInfo: TNyxPrimitiveInfo;
    function Summary: TNyxText;
    function SummaryEvidence: TNyxDocumentationEvidence;
  end;

function TDocumentationSchema.Declaration: INyxDocumentationType;
begin
  Result := API;
end;

function TDocumentationSchema.CreatorKind: TNyxKindRef;
begin
  Result := KindRef;
end;

function TDocumentationSchema.SchemaRevision: Integer;
begin
  Result := Revision;
end;

function TDocumentationSchema.HasSchema: Boolean;
begin
  Result := SchemaPresent;
end;

function TDocumentationSchema.FactsEvidence: TNyxDocumentationEvidence;
begin

  if not SchemaPresent then
    raise ENyxDocumentation.Create('Absent schema metadata supplies no declaration evidence');
  Result := ndeSchemaDeclaration;
end;

function TDocumentationSchema.PropertyCount: Integer;
begin
  Result := Length(Properties);
end;

function TDocumentationSchema.PropertyAt(AIndex: Integer): TNyxPropertyInfo;
begin

  if (AIndex < 0) or (AIndex >= PropertyCount) then
    raise ENyxDocumentation.Create('Documentation property ordinal is out of range');
  Result := Properties[AIndex];
end;

function TDocumentationSchema.EventCount: Integer;
begin
  Result := Length(Events);
end;

function TDocumentationSchema.EventAt(AIndex: Integer): TNyxEventSchema;
begin

  if (AIndex < 0) or (AIndex >= EventCount) then
    raise ENyxDocumentation.Create('Documentation event ordinal is out of range');
  Result := Events[AIndex].Copy;
end;

function TDocumentationSchema.HasCatalog: Boolean;
begin
  Result := CatalogPresent;
end;

function TDocumentationSchema.CatalogInfo: TNyxComponentInfo;
begin

  if not CatalogPresent then
    raise ENyxDocumentation.Create('This documentation has no captured catalog declaration');
  Result := Catalog;
end;

function TDocumentationSchema.HasPrimitive: Boolean;
begin
  Result := PrimitivePresent;
end;

function TDocumentationSchema.PrimitiveInfo: TNyxPrimitiveInfo;
begin

  if not PrimitivePresent then
    raise ENyxDocumentation.Create('This creator has no declared primitive capability metadata');
  Result := Primitive;
end;

function TDocumentationSchema.Summary: TNyxText;
begin

  if CatalogPresent and (Catalog.Discovery.Description <> '') then
    Result := Catalog.Discovery.Description
  else
    Result := API.Summary;
end;

function TDocumentationSchema.SummaryEvidence: TNyxDocumentationEvidence;
begin

  if CatalogPresent and (Catalog.Discovery.Description <> '') then
    Result := ndeCatalogDeclaration
  else
    Result := ndeAuthoredDeclaration;
end;

procedure CheckCancellation(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
    raise ENyxDocumentationCancelled.Create('Documentation schema inspection cancelled');
end;

procedure ValidateCreatorKind(const AKind: TNyxKindRef);
var
  LAt, LScalar, LCount: Integer;
begin

  if Length(AKind.Name) > 1024 then
    raise ENyxDocumentation.Create('Documentation creator name allowance exceeded');

  if Trim(AKind.Name) = '' then
    raise ENyxDocumentation.Create('Documentation requires an explicit creator name');
  LAt := 1;
  LCount := 0;
  while LAt <= Length(AKind.Name) do
  begin

    if not NyxNextScalar(AKind.Name, LAt, LScalar) then
      raise ENyxDocumentation.Create('Documentation creator name has malformed Unicode');
    Inc(LCount);

    if LCount > 256 then
      raise ENyxDocumentation.Create('Documentation creator name allowance exceeded');
  end;
end;

procedure AddTextBytes(const AText: TNyxText; var ABytes: Integer;
  const ACancellation: INyxCancellationScope);
var
  LAt, LScalar, LBytes: Integer;
begin
  { Code units are a lower bound on valid UTF-8 bytes on both supported text
    owners. Refuse oversized input before scanning/copying its whole contents. }

  if Length(AText) > NyxDocumentationMaximumBytes - ABytes then
    raise ENyxDocumentation.Create('Captured documentation text allowance exceeded');
  LAt := 1;
  while LAt <= Length(AText) do
  begin
    CheckCancellation(ACancellation);

    if not NyxNextScalar(AText, LAt, LScalar) then
      raise ENyxDocumentation.Create('Captured documentation text has malformed Unicode');

    if LScalar < $80 then
      LBytes := 1
    else if LScalar < $800 then
      LBytes := 2
    else if LScalar < $10000 then
      LBytes := 3
    else
      LBytes := 4;

    if LBytes > NyxDocumentationMaximumBytes - ABytes then
      raise ENyxDocumentation.Create('Captured documentation text allowance exceeded');
    Inc(ABytes, LBytes);
  end;
end;

procedure AddPayloadBytes(const APayload: TNyxEventPayloadSpec; var ABytes: Integer;
  const ACancellation: INyxCancellationScope);
begin

  if not APayload.Defined then
    Exit;
  { Scalar domains already own admitted immutable data. Read its exact text
    before any enclosing descriptor serialization could allocate a large copy. }

  if APayload.Kind = nepScalar then
    AddTextBytes(APayload.Domain.ToData.ToJSON, ABytes, ACancellation)
  else
    AddTextBytes(APayload.ToData.ToJSON, ABytes, ACancellation);
end;

function CaptureNyxDocumentationSchema(const ADeclaration: INyxDocumentationType;
  const AKind: TNyxKindRef; const ASchemas: INyxSchemaSnapshot;
  ACatalog: TNyxCatalog;
  const ACancellation: INyxCancellationScope): INyxDocumentationSchema;
var
  LValue: TDocumentationSchema;
  LHold: INyxDocumentationSchema;
  LInspection: INyxSchemaInspection;
  LAt, LCount, LKind, LIndex, LBytes: Integer;
  LProperty: TNyxPropertyInfo;
  LEvent: TNyxEventSchema;
begin
  Result := nil;
  CheckCancellation(ACancellation);
  ValidateCreatorKind(AKind);

  if (ASchemas = nil) or not Supports(ASchemas, INyxSchemaInspection, LInspection) then
    raise ENyxDocumentation.Create('Documentation requires an explicit creator and typed captured schema');
  LValue := TDocumentationSchema.Create;
  LHold := LValue;
  LValue.API := CaptureNyxDocumentationType(ADeclaration, ACancellation);
  LValue.KindRef := AKind;
  LValue.Revision := ASchemas.Revision;

  if LValue.Revision < 0 then
    raise ENyxDocumentation.Create('Captured schema revision cannot be negative');
  LCount := LInspection.KindCount;

  if (LCount < 0) or (LCount > 4096) then
    raise ENyxDocumentation.Create('Documentation schema kind allowance exceeded');
  LKind := -1;
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);

    if LInspection.KindAt(LAt).Name = AKind.Name then
    begin

      if LKind >= 0 then
        raise ENyxDocumentation.Create('Captured schema repeats the selected creator');
      LKind := LAt;
    end;
  end;
  LBytes := 0;
  LValue.SchemaPresent := LKind >= 0;

  if LValue.SchemaPresent then
  begin
    LCount := LInspection.PropertyCount(LKind);

    if (LCount < 0) or (LCount > NyxDocumentationMaximumMembers) then
      raise ENyxDocumentation.Create('Documentation property allowance exceeded');
    SetLength(LValue.Properties, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      CheckCancellation(ACancellation);
      LProperty := LInspection.PropertyAt(LKind, LAt);
      AddTextBytes(LProperty.Key, LBytes, ACancellation);
      AddTextBytes(LProperty.Title, LBytes, ACancellation);
      AddTextBytes(LProperty.DefaultValue, LBytes, ACancellation);
      AddTextBytes(LProperty.Choices, LBytes, ACancellation);
      AddTextBytes(LProperty.Support.Description, LBytes, ACancellation);

      if (LProperty.Key = '') or (LProperty.Minimum > LProperty.Maximum) or
        (Ord(LProperty.ValueType) < 0) or (Ord(LProperty.ValueType) > Ord(High(TNyxPropertyType))) or
        (LBytes > NyxDocumentationMaximumBytes) then
        raise ENyxDocumentation.Create('Captured documentation property is invalid or exceeds its allowance');

      if LProperty.Support.Defined and
        ((Ord(LProperty.Support.Meaning) < 0) or
        (Ord(LProperty.Support.Meaning) > Ord(High(TNyxPropertyMeaning))) or
        (Ord(LProperty.Support.Browser) < 0) or
        (Ord(LProperty.Support.Browser) > Ord(High(TNyxCapability))) or
        (Ord(LProperty.Support.Native) < 0) or
        (Ord(LProperty.Support.Native) > Ord(High(TNyxCapability)))) then
        raise ENyxDocumentation.Create('Captured documentation property has unknown support metadata');
      for LIndex := 0 to LAt - 1 do
      begin

        if LValue.Properties[LIndex].Key = LProperty.Key then
          raise ENyxDocumentation.Create('Captured documentation repeats a property');
      end;
      LValue.Properties[LAt] := LProperty;
    end;
    LCount := LInspection.EventCount(LKind);

    if (LCount < 0) or (LCount > NyxDocumentationMaximumMembers) then
      raise ENyxDocumentation.Create('Documentation event allowance exceeded');
    SetLength(LValue.Events, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      CheckCancellation(ACancellation);
      LEvent := LInspection.EventAt(LKind, LAt);

      if ((LEvent.Trigger = ntNamed) and
        ((LEvent.Name.Name = '') or not LEvent.Payload.Defined)) or
        ((LEvent.Trigger <> ntNamed) and (LEvent.Name.Name <> '')) then
        raise ENyxDocumentation.Create('Captured documentation event has no matching declaration');

      if Length(LEvent.Routes) > NyxDocumentationMaximumMembers then
        raise ENyxDocumentation.Create('Captured documentation event route allowance exceeded');
      AddTextBytes(LEvent.Name.Name, LBytes, ACancellation);
      AddTextBytes(LEvent.Title, LBytes, ACancellation);
      AddTextBytes(LEvent.Description, LBytes, ACancellation);
      AddPayloadBytes(LEvent.Payload, LBytes, ACancellation);
      for LIndex := 0 to High(LEvent.Routes) do
      begin
        CheckCancellation(ACancellation);

        if (Ord(LEvent.Routes[LIndex].Trigger) < 0) or
          (Ord(LEvent.Routes[LIndex].Trigger) > Ord(High(TNyxTrigger))) or
          (Ord(LEvent.Routes[LIndex].Browser) < 0) or
          (Ord(LEvent.Routes[LIndex].Browser) > Ord(High(TNyxCapability))) or
          (Ord(LEvent.Routes[LIndex].Native) < 0) or
          (Ord(LEvent.Routes[LIndex].Native) > Ord(High(TNyxCapability))) then
          raise ENyxDocumentation.Create('Captured documentation route has unknown declaration metadata');
        AddTextBytes(LEvent.Routes[LIndex].OriginID, LBytes, ACancellation);
        AddTextBytes(LEvent.Routes[LIndex].SourceID, LBytes, ACancellation);
        AddTextBytes(LEvent.Routes[LIndex].ValueID, LBytes, ACancellation);
        AddPayloadBytes(LEvent.Routes[LIndex].Payload, LBytes, ACancellation);
      end;

      if (LBytes > NyxDocumentationMaximumBytes) or
        (Ord(LEvent.Browser) < 0) or (Ord(LEvent.Browser) > Ord(High(TNyxCapability))) or
        (Ord(LEvent.Native) < 0) or (Ord(LEvent.Native) > Ord(High(TNyxCapability))) or
        (Ord(LEvent.Trigger) < 0) or (Ord(LEvent.Trigger) > Ord(High(TNyxTrigger))) or
        (not NyxIsRuntimeTrigger(LEvent.Trigger) and (LEvent.Trigger <> ntNamed)) then
        raise ENyxDocumentation.Create('Captured documentation event allowance exceeded');
      for LIndex := 0 to LAt - 1 do
      begin

        if (LValue.Events[LIndex].Trigger = LEvent.Trigger) and
          (LValue.Events[LIndex].Name.Name = LEvent.Name.Name) then
          raise ENyxDocumentation.Create('Captured documentation repeats an event');
      end;
      LValue.Events[LAt] := LEvent.Copy;
    end;
  end;

  if ACatalog <> nil then
  begin
    LIndex := ACatalog.IndexOf(AKind.Name);
    LValue.CatalogPresent := LIndex >= 0;

    if LValue.CatalogPresent then
    begin
      LValue.Catalog := ACatalog[LIndex];
      AddTextBytes(LValue.Catalog.Title, LBytes, ACancellation);
      AddTextBytes(LValue.Catalog.Category, LBytes, ACancellation);
      AddTextBytes(LValue.Catalog.Discovery.Aliases, LBytes, ACancellation);
      AddTextBytes(LValue.Catalog.Discovery.Description, LBytes, ACancellation);

      if LBytes > NyxDocumentationMaximumBytes then
        raise ENyxDocumentation.Create('Captured documentation catalog allowance exceeded');
    end;
  end;
  LValue.PrimitivePresent := FindNyxPrimitive(AKind.Name, LValue.Primitive);
  CheckCancellation(ACancellation);
  Result := LHold;
end;

end.
