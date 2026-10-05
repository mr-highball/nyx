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

unit nyx.schema;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.data,
  nyx.types,
  nyx.contract,
  nyx.event.payload,
  nyx.state,
  nyx.model;

type
  { Capability describes the current projection, not production acceptance.
    Basic means a usable projection with incomplete family behavior; Text means
    a text fallback for a specialized widget. Custom factories advertise Custom,
    and must document their own behavior. Missing must produce a diagnostic. }
  TNyxCapability = (ncMissing, ncAvailable, ncBasic, ncText, ncCustom);

  { Shared primitive definitions drive the palette and adapter admission. Kind
    stays a string so consumers can add recipes/factories without editing enums.
    Container means a layout host, not merely an LCL TWinControl descendant. }
  TNyxPrimitiveInfo = record
    Kind: TNyxText;
    Title: TNyxText;
    Category: TNyxText;
    Container: Boolean;
    Browser: TNyxCapability;
    Native: TNyxCapability;
  end;

  TNyxPropertyType = (npText, npLines, npBoolean, npInteger, npNumber, npChoice, npReference);
  TNyxPropertyMeaning = (npmContract, npmPresentation, npmInteraction, npmCustom);
  { Immutable property support, distinct from value-type admission. A property
    may be valid portable data while a particular standard projection cannot
    present it. Contract describes shared semantic metadata; Presentation a
    widget/layout effect; Interaction a scope policy. Custom requires a supplied
    implementation. Grades describe current support, not production acceptance.
    Undefined lets older creator schemas inherit standard metadata. }
  TNyxPropertySupport = record
  private
    FDefined: Boolean;
    FMeaning: TNyxPropertyMeaning;
    FBrowser: TNyxCapability;
    FNative: TNyxCapability;
    FDescription: TNyxText;
  public
    function ForPlatform(APlatform: TNyxPlatform): TNyxPropertySupport;
    property Defined: Boolean read FDefined;
    property Meaning: TNyxPropertyMeaning read FMeaning;
    property Browser: TNyxCapability read FBrowser;
    property Native: TNyxCapability read FNative;
    property Description: TNyxText read FDescription;
  end;
  { String persistence remains extensible. Metadata gives known values a type,
    default, choices and integer bounds for authoring/admission. Empty values
    clear optional fields. Unknown extension keys are preserved without coercion. }
  TNyxPropertyInfo = record
    Key: TNyxText;
    Title: TNyxText;
    ValueType: TNyxPropertyType;
    DefaultValue: TNyxText;
    Choices: TNyxText;
    Minimum: Integer;
    Maximum: Integer;
    { Complete typed configuration remains available in an expandable group;
      the primary inspector starts with properties meaningful to this projection. }
    Advanced: Boolean;
    Support: TNyxPropertySupport;
  end;
  TNyxPropertyInfos = array of TNyxPropertyInfo;
  { One owned physical route into a semantic stream. IDs refer to the resolved
    view context; no node/widget is retained. Scalar payloads may be absent before
    an optional value is supplied, so Optional is explicit. Per-route contracts
    preserve meaningful differences when several controls emit the same name. }
  TNyxEventRoute = record
    Trigger: TNyxTrigger;
    OriginID: TNyxText;
    SourceID: TNyxText;
    ValueID: TNyxText;
    Payload: TNyxEventPayloadSpec;
    PayloadOptional: Boolean;
    Browser: TNyxCapability;
    Native: TNyxCapability;
    function Copy: TNyxEventRoute;
    function ToData: TNyxDataValue;
  end;
  TNyxEventRoutes = array of TNyxEventRoute;
  { Snapshot families are independent of a declared scalar payload. A text
    event may carry both its admitted Before/After and physical editing context;
    agents can inspect this bounded typed set without inferring help strings. }
  TNyxEventContextKind = (nctxKeyboard, nctxTextEdit, nctxPointer, nctxWheel,
    nctxViewport, nctxCollectionSelection, nctxEditing, nctxDrag);
  TNyxEventContexts = set of TNyxEventContextKind;
  TNyxEventSchema = record
    Trigger: TNyxTrigger;
    { Named declarations use ntNamed and an exact typed reference. Physical
      declarations leave Name empty. Payload is immutable and required only by
      a custom producer; legacy physical declarations remain compatible. }
    Name: TNyxEventRef;
    Payload: TNyxEventPayloadSpec;
    Title: TNyxText;
    Description: TNyxText;
    Browser: TNyxCapability;
    Native: TNyxCapability;
    PayloadOptional: Boolean;
    Routes: TNyxEventRoutes;
    { A creator declaration admits custom Emit calls. Inferred physical aliases
      only describe existing routes, and never grant a second producer API. }
    DeclaredProducer: Boolean;
  private
    { Old creator code may fill only the former fields of a local record.
      Compiler-initialized managed text makes an untouched context declaration
      empty, without interpreting uninitialized set bits as capabilities. }
    FContextMarker: TNyxText;
    FContexts: TNyxEventContexts;
    function GetContexts: TNyxEventContexts;
    procedure SetContexts(AValue: TNyxEventContexts);
  public
    property Contexts: TNyxEventContexts read GetContexts write SetContexts;
    { Arrays and payload descriptors are detached before publication/return. }
    function Copy: TNyxEventSchema;
  end;
  TNyxEventSchemas = array of TNyxEventSchema;

{ Closed snapshot-family names at the query/serialization boundary. At most
  seven entries are returned; a caller receives a new immutable array value. }
function NyxEventContextsData(AContexts: TNyxEventContexts): TNyxDataValue;

const
  NyxPrimitiveCount = 41;

{ Returned records/arrays are values owned by the caller, never mutable handles
  into the default registry. A failed lookup returns False with an empty record. }
function NyxPrimitiveInfo(AIndex: Integer): TNyxPrimitiveInfo;
function FindNyxPrimitive(const AKind: TNyxText; out AInfo: TNyxPrimitiveInfo): Boolean;
function NyxCapabilityText(ACapability: TNyxCapability): TNyxText;
function NyxPropertyMeaningText(AMeaning: TNyxPropertyMeaning): TNyxText;
{ A creator supplies explicit immutable support with typed grades. Descriptions
  are ordinary help text, never behavior keywords. A standard lookup inspects
  the selected projection/domain; it retains no node or document. }
function NyxPropertySupport(AMeaning: TNyxPropertyMeaning;
  ABrowser, ANative: TNyxCapability;
  const ADescription: TNyxText): TNyxPropertySupport; overload;
function NyxPropertySupport(ANode: TNyxNode; AAttribute: TNyxAttribute;
  ADocument: TNyxDocument = nil): TNyxPropertySupport; overload;
{ Resolve only the primitive metadata source of a reusable reference. Result is
  borrowed from the supplied node/document and must not be freed or mutated to
  edit an instance. Bounded traversal also diagnoses cycles in unadmitted input. }
function NyxProjectionSource(ANode: TNyxNode; ADocument: TNyxDocument): TNyxNode;
{ Explicit layout overrides the base primitive. Absent/cleared layout uses its
  natural row/grid/column default, consistently for native layout and metadata. }
function NyxLayout(ANode: TNyxNode): TNyxText;
{ Spacer's shared implicit weight is one. An explicit zero opts out; a missing
  or cleared weight restores the primitive default. Other controls default zero. }
function NyxFlexWeight(ANode: TNyxNode): Integer;

{ Returns relevant editable properties for the node's base projection. Known
  extension properties already on a node can be presented separately by Studio. }
function NyxProperties(ANode: TNyxNode; ADocument: TNyxDocument = nil): TNyxPropertyInfos;
{ Supported adapter callbacks, including declared/compound events. Returned
  arrays are independent snapshots. Extensions publish schema against their open
  typed kind; records do not expose mutable registry storage or target widgets. }
function NyxEventsMetadata(ANode: TNyxNode;
  ADocument: TNyxDocument = nil): TNyxEventSchemas;
{ Default scrollable faces share this metadata/adapter admission boundary.
  Framed editors observe their actual input; containers observe their own face. }
function NyxSupportsViewport(ANode: TNyxNode): Boolean;
{ Closed physical classification shared by metadata and adapters. Compound
  schemas aggregate their descendants separately; this does not turn a layout
  host into a focusable widget. Creator factories retain their own focus bridge. }
function NyxSupportsKeyboard(ANode: TNyxNode): Boolean;
{ Publish a creator's named event alongside its physical schema. The declaration
  describes support and payload admission; the supplied adapter provides the
  actual producer. A no-payload overload declares an explicit signal. }
function NyxNamedEventSchema(const AName: TNyxEventRef;
  const ATitle, ADescription: TNyxText; ABrowser, ANative: TNyxCapability;
  const APayload: TNyxEventPayloadSpec): TNyxEventSchema; overload;
function NyxNamedEventSchema(const AName: TNyxEventRef;
  const ATitle, ADescription: TNyxText;
  ABrowser, ANative: TNyxCapability): TNyxEventSchema; overload;
function FindNyxNamedEvent(ANode: TNyxNode; const AName: TNyxEventRef;
  out ASchema: TNyxEventSchema): Boolean;
{ A text family remains a text family only when its declared value domain is
  text. Numeric input drafts retain their editing-complete admission boundary. }
function NyxSupportsTextInput(ANode: TNyxNode): Boolean;
procedure RegisterNyxSchema(const AKind: TNyxKindRef;
  const AProperties: array of TNyxPropertyInfo; const AEvents: array of TNyxEventSchema);

{ Exact scalar contract: local declaration, nearest declared named field, then
  intrinsic primitive meaning. A generic compound/container has no value domain.
  Returned specifications own data. Value fields refer to named parts explicitly. }
function NyxNodeValueDomain(ANode: TNyxNode): TNyxValueDomain;

{ Pure event-value resolution shared by live capture and semantic discovery.
  Origin declarations override the nearest compound source declaration, then
  the target's scalar meaning supplies the default. Return value says whether
  an explicit event declaration supplied the contract. Nodes are borrowed from
  the caller-owned synchronous view/command frame; domains are owned copies. }
function ResolveNyxEventValueContract(AOrigin, ASource, ATarget: TNyxNode;
  ATrigger: TNyxTrigger; out AValueNode: TNyxNode;
  out ADomain: TNyxValueDomain): Boolean;

{ Whole signed 32-bit decimal admission shared by property and control-binding
  boundaries. Optional leading sign is accepted; whitespace, fractions, suffixes
  and overflow are refused. It does not change locale or coerce empty input. }
function TryNyxInteger(const AValue: TNyxText; out AValueNumber: Integer): Boolean;

{ Validate recognized portable property types before accepting a command or
  mounting a target. Unknown property keys survive. Child admission is separate:
  target factories may intentionally supply a host for a custom kind. }
procedure ValidateNyxProperties(ANode: TNyxNode; ADocument: TNyxDocument = nil);
procedure ValidateNyxPropertyTree(ARoot: TNyxNode; ADocument: TNyxDocument = nil);
{ Validate all portable properties, including inherited reusable overrides.
  The document is borrowed. Structural references/ownership are admitted first. }
procedure ValidateNyxDocumentProperties(ADocument: TNyxDocument);

implementation

uses
  nyx.binding,
  nyx.callbacks,
  nyx.collections.view,
  nyx.collections.bindings,
  nyx.design.tokens,
  nyx.composition;

type
  TNyxPublishedSchema = record
    Kind: TNyxText;
    Properties: TNyxPropertyInfos;
    Events: TNyxEventSchemas;
  end;

var
  GPublishedSchemas: array of TNyxPublishedSchema;

procedure RegisterNyxSchema(const AKind: TNyxKindRef;
  const AProperties: array of TNyxPropertyInfo; const AEvents: array of TNyxEventSchema);
var
  LIndex: Integer;
  LOther: Integer;
  LSchema: TNyxPublishedSchema;
begin

  if Trim(AKind.Name) = '' then
  begin
    raise ENyxModel.Create('A published schema requires its component kind');
  end;
  for LIndex := 0 to High(GPublishedSchemas) do
  begin

    if GPublishedSchemas[LIndex].Kind = AKind.Name then
    begin
      raise ENyxModel.Create('A component schema has already been published');
    end;
  end;
  LSchema.Kind := AKind.Name;
  SetLength(LSchema.Properties, Length(AProperties));
  for LIndex := 0 to High(AProperties) do
  begin

    if (AProperties[LIndex].Key = '') or
      (AProperties[LIndex].Minimum > AProperties[LIndex].Maximum) then
    begin
      raise ENyxModel.Create('Invalid published property metadata');
    end;
    for LOther := 0 to LIndex - 1 do
    begin

      if AProperties[LOther].Key = AProperties[LIndex].Key then
      begin
        raise ENyxModel.Create('Duplicate published property');
      end;
    end;
    LSchema.Properties[LIndex] := AProperties[LIndex];
  end;
  SetLength(LSchema.Events, Length(AEvents));
  for LIndex := 0 to High(AEvents) do
  begin

    if not NyxIsRuntimeTrigger(AEvents[LIndex].Trigger) and
      (AEvents[LIndex].Trigger <> ntNamed) then
    begin
      raise ENyxModel.Create('Published events require runtime triggers');
    end;

    if AEvents[LIndex].Trigger = ntNamed then
    begin

      if (Trim(AEvents[LIndex].Name.Name) = '') or
        not AEvents[LIndex].Payload.Defined then
      begin
        raise ENyxModel.Create('A named event requires its reference and payload specification');
      end;
      { Re-admit the owned specification before publishing any registry entry. }
      AEvents[LIndex].Payload.Copy;
      NyxNamedEvent(AEvents[LIndex].Name.Name);
    end
    else if AEvents[LIndex].Name.Name <> '' then
    begin
      raise ENyxModel.Create('A physical event cannot declare a named stream identity');
    end;
    for LOther := 0 to LIndex - 1 do
    begin

      if (AEvents[LOther].Trigger = AEvents[LIndex].Trigger) and
        (AEvents[LOther].Name.Name = AEvents[LIndex].Name.Name) then
      begin
        raise ENyxModel.Create('Duplicate published callback event');
      end;
    end;
    if Length(AEvents[LIndex].Routes) <> 0 then
    begin
      raise ENyxModel.Create('Physical semantic routes are resolved from a view, never global registry data');
    end;
    LSchema.Events[LIndex] := AEvents[LIndex].Copy;
    LSchema.Events[LIndex].DeclaredProducer := AEvents[LIndex].Trigger = ntNamed;
  end;
  LIndex := Length(GPublishedSchemas);
  SetLength(GPublishedSchemas, LIndex + 1);
  GPublishedSchemas[LIndex] := LSchema;
end;

function TNyxEventRoute.Copy: TNyxEventRoute;
begin
  Result := Default(TNyxEventRoute);
  Result.Trigger := Trigger;
  Result.OriginID := OriginID;
  Result.SourceID := SourceID;
  Result.ValueID := ValueID;
  Result.PayloadOptional := PayloadOptional;
  Result.Browser := Browser;
  Result.Native := Native;

  if Payload.Defined then
  begin
    Result.Payload := Payload.Copy;
  end;
end;

function TNyxEventRoute.ToData: TNyxDataValue;
var
  LPayload: TNyxDataValue;
begin
  LPayload := NyxNull;

  if Payload.Defined then
  begin
    LPayload := Payload.ToData;
  end;
  Result := NyxObject([
    NyxField('trigger', NyxData(NyxTriggerName(Trigger))),
    NyxField('origin', NyxData(OriginID)), NyxField('source', NyxData(SourceID)),
    NyxField('value', NyxData(ValueID)), NyxField('payload', LPayload),
    NyxField('payloadOptional', NyxData(PayloadOptional)),
    NyxField('browser', NyxData(NyxCapabilityText(Browser))),
    NyxField('native', NyxData(NyxCapabilityText(Native)))]);
end;

function TNyxEventSchema.GetContexts: TNyxEventContexts;
begin
  Result := [];

  if FContextMarker = 'nyx.event-contexts.v1' then
  begin
    Result := FContexts;
  end;
end;

procedure TNyxEventSchema.SetContexts(AValue: TNyxEventContexts);
begin
  FContexts := AValue;
  FContextMarker := 'nyx.event-contexts.v1';
end;

function TNyxEventSchema.Copy: TNyxEventSchema;
var
  LIndex: Integer;
begin
  Result := Default(TNyxEventSchema);
  Result.Trigger := Trigger;
  Result.Name := Name;
  Result.Title := Title;
  Result.Description := Description;
  Result.Browser := Browser;
  Result.Native := Native;
  Result.PayloadOptional := PayloadOptional;
  Result.DeclaredProducer := DeclaredProducer;
  Result.Contexts := Contexts;

  if Payload.Defined then
  begin
    Result.Payload := Payload.Copy;
  end;
  SetLength(Result.Routes, Length(Routes));
  for LIndex := 0 to High(Routes) do
  begin
    Result.Routes[LIndex] := Routes[LIndex].Copy;
  end;
end;

function ResolveNyxEventValueContract(AOrigin, ASource, ATarget: TNyxNode;
  ATrigger: TNyxTrigger; out AValueNode: TNyxNode;
  out ADomain: TNyxValueDomain): Boolean;
var
  LDeclared: TNyxEventContract;
begin

  if (AOrigin = nil) or (ASource = nil) then
  begin
    raise ENyxModel.Create('Event value resolution requires its origin and source');
  end;
  AValueNode := ATarget;
  ADomain := NyxNoDomain;
  Result := AOrigin.Contract.FindEvent(ATrigger, LDeclared);

  if not Result and (ASource <> AOrigin) then
  begin
    Result := ASource.Contract.FindEvent(ATrigger, LDeclared);
  end;

  if Result then
  begin
    ADomain := LDeclared.Domain.Copy;
    case LDeclared.ValueSource.Source of
      nvsNone:
        begin
          AValueNode := nil;
        end;
      nvsTarget:
        begin
          AValueNode := ATarget;
        end;
      nvsOrigin:
        begin
          AValueNode := AOrigin;
        end;
      nvsSource:
        begin
          AValueNode := ASource;
        end;
      nvsPart:
        begin
          AValueNode := ASource.Part(NyxPart(LDeclared.ValueSource.Part));
        end;
    end;
  end
  else if AValueNode <> nil then
  begin
    ADomain := NyxNodeValueDomain(AValueNode);
  end;
end;

function NyxNamedEventSchema(const AName: TNyxEventRef;
  const ATitle, ADescription: TNyxText; ABrowser, ANative: TNyxCapability;
  const APayload: TNyxEventPayloadSpec): TNyxEventSchema;
begin

  if (Trim(AName.Name) = '') or not APayload.Defined then
  begin
    raise ENyxModel.Create('A named schema requires its reference and payload specification');
  end;
  Result := Default(TNyxEventSchema);
  Result.Trigger := ntNamed;
  Result.Name := NyxNamedEvent(AName.Name);
  Result.Title := ATitle;
  Result.Description := ADescription;
  Result.Browser := ABrowser;
  Result.Native := ANative;
  Result.Payload := APayload.Copy;
  Result.DeclaredProducer := True;
end;

function NyxNamedEventSchema(const AName: TNyxEventRef;
  const ATitle, ADescription: TNyxText;
  ABrowser, ANative: TNyxCapability): TNyxEventSchema;
begin
  Result := NyxNamedEventSchema(AName, ATitle, ADescription, ABrowser,
    ANative, NyxSignalPayload);
end;

const
  CPrimitives: array[0..NyxPrimitiveCount - 1] of TNyxPrimitiveInfo = (
    (Kind:'page'; Title:'Page'; Category:'Layout'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'column'; Title:'Column'; Category:'Layout'; Container:True;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'row'; Title:'Row'; Category:'Layout'; Container:True;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'grid'; Title:'Grid'; Category:'Layout'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'panel'; Title:'Panel'; Category:'Layout'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'card'; Title:'Card'; Category:'Layout'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'group'; Title:'Group box'; Category:'Layout'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'toolbar'; Title:'Toolbar'; Category:'Navigation'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'scroll'; Title:'Scroll area'; Category:'Layout'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'tabs'; Title:'Tabs'; Category:'Navigation'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'tab'; Title:'Tab page'; Category:'Navigation'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'heading'; Title:'Heading'; Category:'Display'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'label'; Title:'Text'; Category:'Display'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'button'; Title:'Button'; Category:'Actions'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'link'; Title:'Link'; Category:'Actions'; Container:False;
      Browser:ncAvailable; Native:ncText),
    (Kind:'input'; Title:'Text field'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'memo'; Title:'Text area'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'checkbox'; Title:'Checkbox'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'switch'; Title:'Switch'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncBasic),
    (Kind:'radio'; Title:'Radio button'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'select'; Title:'Select'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'spin'; Title:'Number field'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'slider'; Title:'Slider'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'date'; Title:'Date field'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncText),
    (Kind:'time'; Title:'Time field'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncText),
    (Kind:'color'; Title:'Color field'; Category:'Inputs'; Container:False;
      Browser:ncAvailable; Native:ncText),
    (Kind:'list'; Title:'List'; Category:'Data'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'table'; Title:'Table'; Category:'Data'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'tree'; Title:'Tree'; Category:'Data'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'image'; Title:'Image'; Category:'Media'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'avatar'; Title:'Avatar'; Category:'Media'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'progress'; Title:'Progress'; Category:'Feedback'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'badge'; Title:'Badge'; Category:'Feedback'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'alert'; Title:'Alert'; Category:'Feedback'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'separator'; Title:'Divider'; Category:'Display'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'spacer'; Title:'Spacer'; Category:'Layout'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'code'; Title:'Code block'; Category:'Display'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'code-editor'; Title:'Code editor'; Category:'Authoring'; Container:False;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'design-surface'; Title:'Design surface'; Category:'Authoring'; Container:True;
      Browser:ncBasic; Native:ncBasic),
    (Kind:'component'; Title:'Reusable component'; Category:'Composition'; Container:False;
      Browser:ncAvailable; Native:ncAvailable),
    (Kind:'split-view'; Title:'Split view'; Category:'Layout'; Container:True;
      Browser:ncAvailable; Native:ncAvailable)
  );

function NyxPrimitiveInfo(AIndex: Integer): TNyxPrimitiveInfo;
begin

  if (AIndex < 0) or (AIndex >= NyxPrimitiveCount) then
  begin
    raise ERangeError.Create('Primitive index out of range');
  end;
  { Explicit field copies keep the public record independent of typed constants
    on pas2js too. Strings are values; this record owns no mutable nested arrays. }
  Result.Kind := CPrimitives[AIndex].Kind;
  Result.Title := CPrimitives[AIndex].Title;
  Result.Category := CPrimitives[AIndex].Category;
  Result.Container := CPrimitives[AIndex].Container;
  Result.Browser := CPrimitives[AIndex].Browser;
  Result.Native := CPrimitives[AIndex].Native;
end;

function FindNyxPrimitive(const AKind: TNyxText; out AInfo: TNyxPrimitiveInfo): Boolean;
var
  LIndex: Integer;
begin
  AInfo.Kind := '';
  AInfo.Title := '';
  AInfo.Category := '';
  AInfo.Container := False;
  AInfo.Browser := ncMissing;
  AInfo.Native := ncMissing;
  for LIndex := 0 to NyxPrimitiveCount - 1 do
  begin

    if CPrimitives[LIndex].Kind = AKind then
    begin
      AInfo := NyxPrimitiveInfo(LIndex);
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxCapabilityText(ACapability: TNyxCapability): TNyxText;
begin
  case ACapability of
    ncAvailable:
      begin
        Result := 'Available';
      end;
    ncBasic:
      begin
        Result := 'Basic support';
      end;
    ncText:
      begin
        Result := 'Text fallback';
      end;
    ncCustom:
      begin
        Result := 'Custom component';
      end;
  else
    Result := 'Unavailable';
  end;
end;

function NyxLayout(ANode: TNyxNode): TNyxText;
begin
  Result := ANode.Prop('layout');

  if Result <> '' then
  begin
    Exit;
  end;
  Result := 'column';

  if (ANode.ProjectionKind = 'row') or (ANode.ProjectionKind = 'toolbar') then
  begin
    Result := 'row';
  end
  else if ANode.ProjectionKind = 'grid' then
  begin
    Result := 'grid';
  end;
end;

function KindIn(const AKind, AKinds: TNyxText): Boolean;
begin
  Result := Pos('|' + AKind + '|', '|' + AKinds + '|') > 0;
end;

function NyxFlexWeight(ANode: TNyxNode): Integer;
var
  LDefault: Integer;
begin
  LDefault := 0;

  if ANode.ProjectionKind = 'spacer' then
  begin
    LDefault := 1;
  end;
  Result := LDefault;

  if ANode.Prop('flex') <> '' then
  begin
    Result := StrToIntDef(ANode.Prop('flex'), LDefault);
  end;
end;

function NyxProjectionSource(ANode: TNyxNode; ADocument: TNyxDocument): TNyxNode;
var
  LDefinition: TNyxNode;
  LDepth: Integer;
begin
  Result := ANode;
  LDepth := 0;

  if (Result = nil) or (ADocument = nil) then
  begin
    Exit;
  end;
  while (Result.Kind = 'component') or (Result.ProjectionKind = 'component') do
  begin
    Inc(LDepth);

    if LDepth > 128 then
    begin
      raise ENyxModel.Create('Reusable metadata exceeds reference depth');
    end;
    LDefinition := ADocument.FindComponent(Result.Prop('component'));

    if LDefinition = nil then
    begin
      Exit;
    end;
    Result := LDefinition;
  end;
end;

function NyxPropertyMeaningText(AMeaning: TNyxPropertyMeaning): TNyxText;
const
  CNames: array[TNyxPropertyMeaning] of TNyxText =
    ('Shared contract', 'Presentation', 'Interaction policy', 'Custom implementation');
begin
  Result := CNames[AMeaning];
end;

function NyxPropertySupport(AMeaning: TNyxPropertyMeaning;
  ABrowser, ANative: TNyxCapability;
  const ADescription: TNyxText): TNyxPropertySupport;
begin
  Result.FDefined := True;
  Result.FMeaning := AMeaning;
  Result.FBrowser := ABrowser;
  Result.FNative := ANative;
  Result.FDescription := ADescription;
end;

function TNyxPropertySupport.ForPlatform(APlatform: TNyxPlatform): TNyxPropertySupport;
begin
  Result := Self;

  if APlatform = npfBrowser then
  begin
    Result.FNative := ncMissing;
    Result.FDescription := 'Browser-only override. ' + FDescription;
  end
  else if APlatform = npfNativeLCL then
  begin
    Result.FBrowser := ncMissing;
    Result.FDescription := 'Native-only override. ' + FDescription;
  end;
end;

function NyxPropertySupport(ANode: TNyxNode; AAttribute: TNyxAttribute;
  ADocument: TNyxDocument): TNyxPropertySupport;
var
  LBase: TNyxNode;
  LInfo: TNyxPrimitiveInfo;
  LKind: TNyxText;
  LMeaning: TNyxPropertyMeaning;
  LBrowser: TNyxCapability;
  LNative: TNyxCapability;
  LDescription: TNyxText;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Property support requires a component');
  end;
  LBase := NyxProjectionSource(ANode, ADocument);
  LKind := LBase.ProjectionKind;
  LMeaning := npmPresentation;
  LBrowser := ncAvailable;
  LNative := ncAvailable;
  LDescription := 'Applies to the selected standard projection.';

  if not FindNyxPrimitive(LKind, LInfo) then
  begin
    Exit(NyxPropertySupport(npmCustom, ncCustom, ncCustom,
      'The supplied component adapters define this property effect.'));
  end;
  case AAttribute of
    atCompound, atAction, atProjection, atOverrideMode, atPart, atTarget,
    atComponent, atEmit, atEmitChange, atOption, atPath, atDesignID:
      begin
        LMeaning := npmContract;
        LDescription := 'Shared composition/routing metadata; meaning follows its declared context.';
      end;
    atEnabled, atVisible:
      begin
        LMeaning := npmInteraction;
        LDescription := 'An ancestor scope also governs descendant interaction.';
      end;
    atReadOnly:
      begin
        LMeaning := npmInteraction;
        LDescription := 'Refuses user value changes; focus, notifications and ' +
          'programmatic state updates remain available.';

        if KindIn(LKind, 'select|checkbox|switch|radio|slider') then
        begin
          LBrowser := ncBasic;
          LNative := ncBasic;
          LDescription := LDescription + ' This selector restores refused physical drafts; ' +
            'it has no standard read-only widget mode.';
        end;
      end;
    atText:
      begin

        if not KindIn(LKind, 'heading|label|button|link|input|memo|select|spin|date|time|color|' +
          'checkbox|switch|radio|avatar|badge|alert|code|code-editor|group') then
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'This standard projection has no text caption; ' +
            'compose a label or heading inside it.';
        end
        else if LKind = 'code-editor' then
        begin
          LMeaning := npmInteraction;
          LDescription := 'The source editor uses text as its accessible name; ' +
            'compose a separate label for a visible caption.';
        end;
      end;
    atValue:
      begin

        if not NyxNodeValueDomain(LBase).Defined then
        begin
          LBrowser := ncCustom;
          LNative := ncCustom;
          LMeaning := npmCustom;
          LDescription := 'No intrinsic scalar value; declare a domain and supply its application meaning.';
        end
        else if not KindIn(LKind, 'input|memo|select|spin|slider|progress|date|time|' +
          'color|checkbox|switch|radio|code-editor') then
        begin
          LMeaning := npmContract;
          LDescription := 'Declared semantic value; the composed parts or application present it.';
        end;
      end;
    atPlaceholder:
      begin

        if not KindIn(LKind, 'input|memo|code-editor') then
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'Placeholder belongs to a text-entry projection.';
        end
        else if KindIn(LKind, 'memo|code-editor') then
        begin
          LNative := ncBasic;
          LDescription := 'Native multiline placeholder display depends on the widgetset; ' +
            'browser placeholder is available.';
        end;
      end;
    atItems:
      begin

        if KindIn(LKind, 'select|list|table|tree') then
        begin
          LBrowser := ncBasic;
          LNative := ncBasic;
          LDescription := 'Static initial rows; use typed collection views for live structured datasets.';
        end
        else
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'Static items belong to a choice/list/table/tree projection.';
        end;
      end;
    atHref:
      begin
        LNative := ncMissing;
        LDescription := 'Browser link destination. Standard LCL link is a focusable ' +
          'command face; a handler supplies native navigation.';

        if LKind <> 'link' then
        begin
          LBrowser := ncMissing;
          LDescription := 'A link destination requires a link projection.';
        end;
      end;
    atSource, atAlt:
      begin

        if LKind = 'image' then
        begin

          if AAttribute = atSource then
          begin
            LNative := ncBasic;
            LDescription := 'Browser image URL; standard LCL pictures resolve local ' +
              'files. Native network/portable asset resolution requires a supplied adapter.';
          end
          else
          begin
            LDescription := 'Browser alternative text and native accessible ' +
              'description. Empty text deliberately describes a decorative image.';
          end;
        end
        else
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'Image source/alternative text requires an image projection.';
        end;
      end;
    atLayout, atGap, atColumns, atFlowWrap, atCrossAlignment, atJustification:
      begin

        if not LInfo.Container or (LKind = 'split-view') then
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'Compose children in a layout host; ' +
            'split panes use their dedicated arrangement.';
        end
        else if AAttribute = atColumns then
        begin
          LDescription := 'Column count applies when grid layout is selected.';
        end
        else if AAttribute = atFlowWrap then
        begin
          LDescription := 'Rows wrap by available content width. Automatic wraps ' +
            'at narrow host widths; nowrap preserves one line. Other layouts retain the choice.';
        end
        else if AAttribute in [atCrossAlignment, atJustification] then
        begin
          LDescription := 'Logical cross/main-axis alignment applies to row/column ' +
            'flow. Start/end preserve authored order; grid/absolute retain the choice.';
        end;
      end;
    atPadding:
      begin

        if not LInfo.Container or (LKind = 'split-view') then
        begin
          LNative := ncMissing;
          LDescription := 'Browser CSS padding is available; standard LCL leaf/split ' +
            'padding requires a custom face or child host.';
        end;
      end;
    atMinimum, atMaximum:
      begin

        if LKind = 'input' then
        begin
          LBrowser := ncBasic;
          LNative := ncMissing;
          LDescription := 'Browser numeric inputs use min/max hints. Declare a bounded ' +
            'numeric domain for exact model admission on both targets.';
        end
        else if not KindIn(LKind, 'spin|slider|progress') then
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'Standard min/max applies to range widgets. ' +
            'Declare a bounded numeric domain for another semantic value.';
        end;
      end;
    atPressed:
      begin

        if KindIn(LKind, 'button|link') then
        begin
          LNative := ncBasic;
          LDescription := 'Semantic pressed/accessibility state; ' +
            'native styling follows the selected variant.';
        end
        else
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'Pressed presentation belongs to a button or link command face.';
        end;
      end;
    atSurface, atVariant:
      begin
        LBrowser := ncBasic;
        LNative := ncBasic;
        LDescription := 'Theme effect follows the supported surface/variant of this ' +
          'projection; arbitrary styles require a supplied face.';
      end;
    atInputType:
      begin

        if LKind = 'input' then
        begin
          LNative := ncBasic;
          LDescription := 'Browser input semantics; native edit supports password masking ' +
            'and typed value admission, with remaining format/picker behavior ' +
            'supplied by the application.';
        end
        else
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'Input type belongs to the single-line input projection.';
        end;
      end;
    atSplitOrientation, atSplitPosition, atSplitMinimum, atSplitMaximum, atSplitResizable:
      begin

        if LKind <> 'split-view' then
        begin
          LBrowser := ncMissing;
          LNative := ncMissing;
          LDescription := 'Pane sizing belongs to the split-view projection.';
        end;
      end;
    atDragSource, atDropTarget:
      begin
        LBrowser := ncBasic;
        LNative := ncBasic;
        LDescription := 'Opt in to typed drag offers/drop negotiation; callbacks supply data and acceptance. Browser host drag support and native internal LCL mouse drags apply; no automatic source deletion or external native file transfer.';
      end;
    atTouchBehavior:
      begin
        LNative := ncMissing;
        LDescription := 'Browser touch-action negotiation for direct manipulation; declared before a gesture starts. Native mouse hooks do not implement touch-action.';
      end;
    atWidthSizing, atHeightSizing:
      begin
        LDescription := 'Automatic uses the retained pixel metric or primitive default; ' +
          'Content uses intrinsic size; Fill uses the containing content extent. ' +
          'A parent weight owns its main-axis allocation. Fill height in an ' +
          'indefinite parent falls back to natural content.';
      end;
    atHint, atAccessibleName, atWidth, atHeight, atLeft, atTop, atFlex:
      begin
        { These common semantics are applied by both adapters. }
      end;
  end;
  Result := NyxPropertySupport(LMeaning, LBrowser, LNative, LDescription);
end;

function NyxProperties(ANode: TNyxNode; ADocument: TNyxDocument): TNyxPropertyInfos;
var
  LKind: TNyxText;
  LInfo: TNyxPrimitiveInfo;
  LBase: TNyxNode;
  LIndex: Integer;
  LRuntime: TNyxNode;
  LDomain: TNyxValueDomain;
  LHasValue: Boolean;
  LDeclared: TNyxDataValue;
  LMinimum: Integer;
  LMaximum: Integer;
  LAttribute: TNyxAttribute;
  LPublishedIndex: Integer;
  LPropertyIndex: Integer;
  LFoundIndex: Integer;
  LAttributeType: TNyxPropertyType;
  LTitle: TNyxText;
  LChoiceNames: TNyxText;
  LLayoutChoice: TNyxLayoutMode;
  LActionChoice: TNyxAction;
  LOverrideChoice: TNyxOverrideMode;
  LInputChoice: TNyxInputType;
  LPlatform: TNyxPlatform;
  LPlatformCount: Integer;
  LScopedKey: TNyxText;

  procedure Add(const AKey, ATitle: TNyxText; AType: TNyxPropertyType;
    const ADefault: TNyxText = ''; const AChoices: TNyxText = '';
    AMinimum: Integer = 0; AMaximum: Integer = 100000);
  var
    LIndex: Integer;
  begin
    LIndex := Length(Result);
    SetLength(Result, LIndex + 1);
    Result[LIndex].Key := AKey;
    Result[LIndex].Title := ATitle;
    Result[LIndex].ValueType := AType;
    Result[LIndex].DefaultValue := ADefault;
    Result[LIndex].Choices := AChoices;
    Result[LIndex].Minimum := AMinimum;
    Result[LIndex].Maximum := AMaximum;
    Result[LIndex].Advanced := False;
  end;

  function InheritedValue(const AKey, ADefault: TNyxText): TNyxText;
  var
    LNode: TNyxNode;
    LLevel: Integer;
  begin
    Result := ADefault;
    LNode := ANode;
    for LLevel := 0 to 127 do
    begin

      if (LNode.Kind <> 'component') and (LNode.ProjectionKind <> 'component') then
      begin
        Exit;
      end;
      LNode := ADocument.FindComponent(LNode.Prop('component'));

      if LNode = nil then
      begin
        Exit;
      end;

      if LNode.Props.IndexOfName(AKey) >= 0 then
      begin
        Exit(LNode.Prop(AKey));
      end;
    end;
  end;
begin
  Result := nil;

  if ANode = nil then
  begin
    raise ENyxModel.Create('Property metadata requires a node');
  end;

  if ANode.Kind = 'slot-override' then
  begin
    { Inspector metadata is a value snapshot of the actual customized part,
      including nested reusable parts and replacement payloads. The temporary
      realization is never returned as a borrowed document pointer. }

    if (ADocument <> nil) and (ANode.Prop('mode') <> 'remove') then
    begin
      LRuntime := RealizeNyxView(ADocument, ANode.Parent);
      try
        LBase := LRuntime.Part(ANode.Prop('path'));
        Result := NyxProperties(LBase);
        for LIndex := 0 to Length(Result) - 1 do
        begin
          Result[LIndex].DefaultValue := LBase.Prop(Result[LIndex].Key,
            Result[LIndex].DefaultValue);
        end;
      finally
        LRuntime.Free;
      end;
    end;
    Add('path', 'Part path', npText);
    Result[High(Result)].Support := NyxPropertySupport(npmContract,
      ncAvailable, ncAvailable, 'Named reusable part addressed by this override descriptor.');
    Add('mode', 'Override mode', npChoice, 'properties',
      'properties' + #10 + 'append' + #10 + 'prepend' + #10 + 'replace' + #10 + 'remove');
    Result[High(Result)].Support := NyxPropertySupport(npmContract,
      ncAvailable, ncAvailable, 'Shared reusable override operation; realized parts supply presentation.');
    Exit;
  end;
  LBase := NyxProjectionSource(ANode, ADocument);
  LKind := LBase.ProjectionKind;
  FindNyxPrimitive(LKind, LInfo);

  if KindIn(LKind, 'heading|label|button|link|input|memo|select|spin|date|time|color|' +
    'checkbox|switch|radio|avatar|badge|alert|code|code-editor|group') then
  begin
    Add('text', 'Text', npText);
  end;

  if KindIn(LKind, 'input|memo|select|spin|slider|progress|date|time|color|code-editor') then
  begin

    if KindIn(LKind, 'memo|code-editor') then
    begin
      Add('value', 'Value', npLines);
    end
    else if KindIn(LKind, 'spin|slider|progress') then
    begin
      Add('value', 'Value', npInteger, '0', '', -1000000, 1000000);
      Add('min', 'Minimum', npInteger, '0', '', -1000000, 1000000);
      Add('max', 'Maximum', npInteger, '100', '', -1000000, 1000000);
    end
    else
    begin
      Add('value', 'Value', npText);
    end;
  end;

  if KindIn(LKind, 'checkbox|switch|radio') then
  begin
    Add('value', 'Checked', npBoolean, 'false');
  end;
  { Logical compound values and numeric inputs share the same admitted domain
    used by bindings/events. Inspector metadata must not infer text from a
    container projection or lose a declared Number to an integer editor. }
  LDomain := NyxNodeValueDomain(LBase);
  LHasValue := False;
  for LIndex := 0 to Length(Result) - 1 do
  begin

    if Result[LIndex].Key = 'value' then
    begin
      LHasValue := True;

      if LDomain.Defined and (LDomain.Kind = nskNumber) then
      begin
        Result[LIndex].ValueType := npNumber;
      end;
    end;
  end;

  if LDomain.Defined and not LHasValue then
  begin
    case LDomain.Kind of
      nskText: Add('value', 'Value', npText);
      nskBoolean: Add('value', 'Value', npBoolean);
      nskInteger:
        begin
          LMinimum := Low(Integer);
          LMaximum := High(Integer);
          LDeclared := LDomain.ToData;
          for LIndex := 0 to LDeclared.Count - 1 do
          begin

            if LDeclared.Key(LIndex) = 'min' then
            begin
              LMinimum := LDeclared.Field('min').AsInteger;
              LMaximum := LDeclared.Field('max').AsInteger;
            end;
          end;
          Add('value', 'Value', npInteger, '', '', LMinimum, LMaximum);
        end;
      nskNumber: Add('value', 'Value', npNumber);
    end;
  end;

  if KindIn(LKind, 'input|memo') then
  begin
    Add('placeholder', 'Placeholder', npText);
  end;

  if KindIn(LKind, 'input|memo|code-editor|list|table|tree|column|row|grid|panel|card|group|toolbar') then
  begin
    Add('readonly', 'Read only', npBoolean, 'false');
  end;

  if KindIn(LKind, 'select|list|table|tree') then
  begin
    Add('items', 'Items (one row per line)', npLines);
  end;

  if (ANode.Kind = 'component') or (ANode.ProjectionKind = 'component') then
  begin
    Add('component', 'Definition', npReference);
  end;

  if LKind = 'link' then
  begin
    Add('href', 'Link URL', npText);
  end;

  if LKind = 'image' then
  begin
    Add('src', 'Image source', npText);
    Add('alt', 'Alternative text', npText);
  end;

  if LKind = 'split-view' then
  begin
    Add('split-orientation', 'Pane arrangement', npChoice, 'stacked',
      'stacked' + #10 + 'side-by-side');
    Add('split-position', 'First pane (%)', npInteger, '65', '', 0, 100);
    Add('split-minimum', 'Minimum first pane (%)', npInteger, '15', '', 0, 100);
    Add('split-maximum', 'Maximum first pane (%)', npInteger, '85', '', 0, 100);
    Add('split-resizable', 'Allow resizing', npBoolean, 'true');
  end;

  if LInfo.Container then
  begin
    Add('layout', 'Layout', npChoice, NyxLayout(LBase),
      'column' + #10 + 'row' + #10 + 'grid' + #10 + 'absolute');
    Add('padding', 'Padding (px)', npInteger);
    Add('gap', 'Gap (px)', npInteger, '12');
    Add('columns', 'Grid columns', npInteger, '2', '', 1, 64);
  end;

  if LKind = 'button' then
  begin
    Add('variant', 'Style', npText);
    Add('emit', 'Click event', npText);
  end;
  Add('width', 'Width (px)', npInteger);
  Add('height', 'Height (px)', npInteger);
  Add('width-sizing', 'Width sizing', npChoice, 'auto', 'auto' + #10 + 'content' + #10 + 'fill');
  Add('height-sizing', 'Height sizing', npChoice, 'auto', 'auto' + #10 + 'content' + #10 + 'fill');
  Add('flow-wrap', 'Row wrapping', npChoice, 'auto', 'auto' + #10 + 'nowrap' + #10 + 'wrap');
  Add('cross-alignment', 'Cross-axis alignment', npChoice, 'auto',
    'auto' + #10 + 'start' + #10 + 'center' + #10 + 'end' + #10 + 'stretch');
  Add('justification', 'Main-axis alignment', npChoice, 'start',
    'start' + #10 + 'center' + #10 + 'end' + #10 + 'space-between' + #10 +
    'space-around' + #10 + 'space-evenly');
  Add('left', 'Left (px)', npInteger);
  Add('top', 'Top (px)', npInteger);
  Add('enabled', 'Enabled', npBoolean, 'true');
  Add('drag-source', 'Drag source', npBoolean, 'false');
  Add('drop-target', 'Drop target', npBoolean, 'false');
  Add('touch-behavior', 'Touch behavior', npChoice, 'auto',
    'auto' + #10 + 'none' + #10 + 'pan-x' + #10 + 'pan-y' + #10 + 'manipulation');
  Add('visible', 'Visible', npBoolean, 'true');
  Add('hint', 'Hint', npText);
  Add('aria-label', 'Accessible name', npText);

  { The base fluent configuration is shared by every specialized control.
    Preserve the concise projection-specific fields above, while making every
    remaining typed attribute reachable through the expanded inspector group.
    Open custom variants/projections remain references/text, not closed choices. }
  for LAttribute := Low(TNyxAttribute) to High(TNyxAttribute) do
  begin
    LFoundIndex := -1;
    for LPropertyIndex := 0 to High(Result) do
    begin

      if Result[LPropertyIndex].Key = NyxAttributeName(LAttribute) then
      begin
        LFoundIndex := LPropertyIndex;
        Break;
      end;
    end;

    if LFoundIndex >= 0 then
    begin
      Continue;
    end;
    LAttributeType := npText;
    LChoiceNames := '';
    LTitle := NyxAttributeName(LAttribute);
    case LAttribute of
      atText: LTitle := 'Text';
      atValue: LTitle := 'Value';
      atItems: LAttributeType := npLines;
      atPadding, atGap, atColumns, atWidth, atHeight, atLeft, atTop,
      atFlex, atMinimum, atMaximum: LAttributeType := npInteger;
      atSplitPosition, atSplitMinimum, atSplitMaximum: LAttributeType := npInteger;
      atEnabled, atVisible, atReadOnly, atSurface, atCompound, atPressed, atSplitResizable,
        atDragSource, atDropTarget:
        LAttributeType := npBoolean;
      atPart, atTarget, atComponent, atEmit, atEmitChange, atPath, atProjection:
        LAttributeType := npReference;
      atLayout:
        begin
          LAttributeType := npChoice;
          for LLayoutChoice := Low(TNyxLayoutMode) to High(TNyxLayoutMode) do
          begin
            LChoiceNames := LChoiceNames + NyxLayoutName(LLayoutChoice) + #10;
          end;
        end;
      atAction:
        begin
          LAttributeType := npChoice;
          for LActionChoice := Low(TNyxAction) to High(TNyxAction) do
          begin
            LChoiceNames := LChoiceNames + NyxActionName(LActionChoice) + #10;
          end;
        end;
      atOverrideMode:
        begin
          LAttributeType := npChoice;
          for LOverrideChoice := Low(TNyxOverrideMode) to High(TNyxOverrideMode) do
          begin
            LChoiceNames := LChoiceNames + NyxOverrideName(LOverrideChoice) + #10;
          end;
        end;
      atInputType:
        begin
          LAttributeType := npChoice;
          for LInputChoice := Low(TNyxInputType) to High(TNyxInputType) do
          begin
            LChoiceNames := LChoiceNames + NyxInputTypeName(LInputChoice) + #10;
          end;
        end;
      atSplitOrientation:
        begin
          LAttributeType := npChoice;
          LChoiceNames := 'stacked' + #10 + 'side-by-side';
        end;
      atTouchBehavior:
        begin
          LAttributeType := npChoice;
          LChoiceNames := 'auto' + #10 + 'none' + #10 + 'pan-x' + #10 + 'pan-y' + #10 + 'manipulation';
        end;
    end;
    { The expanded group must retain the same closed argument families as the
      concise inspector. Enum choices derive from their public wire helpers. }
    LChoiceNames := Trim(LChoiceNames);
    Add(NyxAttributeName(LAttribute), LTitle, LAttributeType, '', LChoiceNames);
    case LAttribute of
      atFlex:
        begin
          Result[High(Result)].DefaultValue := '0';

          if LBase.ProjectionKind = 'spacer' then
          begin
            Result[High(Result)].DefaultValue := '1';
          end;
        end;
      atSplitPosition, atSplitMinimum, atSplitMaximum:
        begin
          Result[High(Result)].Maximum := 100;
        end;
      atColumns:
        begin
          Result[High(Result)].Minimum := 1;
          Result[High(Result)].Maximum := 64;
        end;
      atMinimum, atMaximum:
        begin
          Result[High(Result)].Minimum := -1000000;
          Result[High(Result)].Maximum := 1000000;
        end;
    end;
    Result[High(Result)].Advanced := LAttribute <> atText;
  end;


  for LPublishedIndex := 0 to High(GPublishedSchemas) do
  begin

    if (GPublishedSchemas[LPublishedIndex].Kind <> ANode.Kind) and
      (GPublishedSchemas[LPublishedIndex].Kind <> LBase.Kind) then
    begin
      Continue;
    end;
    for LPropertyIndex := 0 to High(GPublishedSchemas[LPublishedIndex].Properties) do
    begin
      LFoundIndex := -1;
      for LIndex := 0 to High(Result) do
      begin

        if Result[LIndex].Key = GPublishedSchemas[LPublishedIndex].Properties[LPropertyIndex].Key then
        begin
          LFoundIndex := LIndex;
          Break;
        end;
      end;

      if LFoundIndex < 0 then
      begin
        LFoundIndex := Length(Result);
        SetLength(Result, LFoundIndex + 1);
      end;
      Result[LFoundIndex] := GPublishedSchemas[LPublishedIndex].Properties[LPropertyIndex];
    end;
  end;

  { Explicit creator support wins. Older schemas inherit ordinary attribute
    metadata; open custom keys remain custom instead of advertising a bridge
    that the standard adapters never implemented. }
  for LPropertyIndex := 0 to High(Result) do
  begin

    if Result[LPropertyIndex].Support.Defined then
    begin
      Continue;
    end;

    if TryNyxAttribute(Result[LPropertyIndex].Key, LAttribute) then
    begin
      Result[LPropertyIndex].Support := NyxPropertySupport(LBase, LAttribute);
    end
    else
    begin
      Result[LPropertyIndex].Support := NyxPropertySupport(npmCustom, ncCustom,
        ncCustom, 'Creator-defined property; its supplied adapters define the effect.');
    end;
  end;

  { Present platform overrides are discoverable typed inspector/MCP properties.
    Defaults are supplied by the ordinary property; no absent override is
    manufactured. The reserved wire prefix never enters authored Pascal. }
  LPlatformCount := Length(Result);
  for LPlatform := npfBrowser to npfNativeLCL do
  begin
    for LPropertyIndex := 0 to LPlatformCount - 1 do
    begin

      if TryNyxAttribute(Result[LPropertyIndex].Key, LAttribute) and
        NyxPlatformAttribute(LAttribute) then
      begin
        LScopedKey := NyxPlatformKey(LPlatform, LAttribute);

        if ANode.Props.IndexOfName(LScopedKey) >= 0 then
        begin
          LFoundIndex := Length(Result);
          SetLength(Result, LFoundIndex + 1);
          Result[LFoundIndex] := Result[LPropertyIndex];
          Result[LFoundIndex].Key := LScopedKey;
          Result[LFoundIndex].Title := NyxPlatformName(LPlatform) + ' / ' + Result[LPropertyIndex].Title;
          Result[LFoundIndex].DefaultValue := '';
          Result[LFoundIndex].Advanced := True;
          Result[LFoundIndex].Support := Result[LPropertyIndex].Support.ForPlatform(LPlatform);
        end;
      end;
    end;
  end;

  if LBase <> ANode then
  begin
    { Instances inherit actual definition values as authoring defaults;
      explicit instance overrides remain separate persisted properties. }
    for LIndex := 0 to Length(Result) - 1 do
    begin

      if Result[LIndex].Key <> 'component' then
      begin
        Result[LIndex].DefaultValue := InheritedValue(Result[LIndex].Key,
          Result[LIndex].DefaultValue);
      end;
    end;
  end;
end;

function NyxEventContextsData(AContexts: TNyxEventContexts): TNyxDataValue;
const
  CNames: array[TNyxEventContextKind] of TNyxText = ('keyboard', 'text-edit',
    'pointer', 'wheel', 'viewport', 'collection-selection', 'editing', 'drag');
var
  LKind: TNyxEventContextKind;
  LItems: array of TNyxDataValue;
  LCount: Integer;
begin
  LItems := nil;
  LCount := 0;
  for LKind := Low(TNyxEventContextKind) to High(TNyxEventContextKind) do
  begin

    if LKind in AContexts then
    begin
      SetLength(LItems, LCount + 1);
      LItems[LCount] := NyxData(CNames[LKind]);
      Inc(LCount);
    end;
  end;
  Result := NyxArray(LItems);
end;

function NyxSupportsTextInput(ANode: TNyxNode): Boolean;
var
  LDomain: TNyxValueDomain;
begin
  Result := False;

  if (ANode = nil) or not KindIn(ANode.ProjectionKind, 'input|memo|code-editor') then
  begin
    Exit;
  end;
  LDomain := NyxNodeValueDomain(ANode);
  Result := not LDomain.Defined or (LDomain.Kind = nskText);
end;

function NyxSupportsViewport(ANode: TNyxNode): Boolean;
begin
  Result := (ANode <> nil) and KindIn(ANode.ProjectionKind,
    'scroll|memo|code|code-editor|list|table|tree');
end;

function NyxSupportsKeyboard(ANode: TNyxNode): Boolean;
var
  LKind: TNyxKind;
begin
  Result := False;

  if (ANode = nil) or not TryNyxKind(ANode.ProjectionKind, LKind) then
  begin
    Exit;
  end;
  Result := LKind in [nkButton, nkLink, nkInput, nkMemo, nkCheckbox, nkSwitch,
    nkRadio, nkSelect, nkSpin, nkSlider, nkDate, nkTime, nkColor, nkCode,
    nkCodeEditor, nkList, nkTable, nkTree, nkSplitView];
end;

function NyxEventsMetadata(ANode: TNyxNode; ADocument: TNyxDocument): TNyxEventSchemas;
var
  LRoot: TNyxNode;
  LContext: TNyxNode;
  LOwned: Boolean;
  LTriggers: set of TNyxTrigger;
  LBridgedTriggers: set of TNyxTrigger;
  LTrigger: TNyxTrigger;
  LIndex: Integer;
  LPublishedIndex: Integer;
  LEventIndex: Integer;
  LFound: Integer;
  LAuthored: TNyxAuthoredEventInfos;
  LPublished: TNyxEventSchema;

  procedure AddRoute(AControl: TNyxNode; ATrigger: TNyxTrigger;
    AAttribute: TNyxAttribute);
  var
    LName: TNyxEventRef;
    LSource: TNyxNode;
    LAncestor: TNyxNode;
    LTarget: TNyxNode;
    LValueNode: TNyxNode;
    LDomain: TNyxValueDomain;
    LAction: TNyxAction;
    LRoute: TNyxEventRoute;
    LSemantic: TNyxSemanticEvent;
    LMatch: Integer;
    LRouteIndex: Integer;
    LExisting: Integer;
    LPayloadsMatch: Boolean;
  begin
    { Only deliberately named producers are semantic declarations. Absent
      physical names retain the normal OnClick/OnChange entries; an explicit
      empty name suppresses a route rather than publishing a blank stream. }

    if AControl.Prop(NyxAttributeName(AAttribute)) = '' then
    begin
      Exit;
    end;
    LName := NyxNamedEvent(AControl.Prop(NyxAttributeName(AAttribute)));
    LSource := AControl;
    LAncestor := AControl;
    while LAncestor <> nil do
    begin

      if LAncestor.Prop(NyxAttributeName(atCompound)) = 'true' then
      begin
        LSource := LAncestor;
        Break;
      end;
      LAncestor := LAncestor.Parent;
    end;
    LTarget := AControl;

    if ATrigger = ntClick then
    begin

      if not TryNyxAction(AControl.Prop(NyxAttributeName(atAction)), LAction) then
      begin
        raise ENyxModel.Create('Semantic route has an unknown portable action');
      end;

      if LAction <> naNone then
      begin
        LTarget := LSource;

        if (LAction <> naSelect) and (AControl.Prop(NyxAttributeName(atTarget)) <> '') then
        begin
          LTarget := LSource.Part(AControl.Prop(NyxAttributeName(atTarget)));
        end;
      end;
    end;
    ResolveNyxEventValueContract(AControl, LSource, LTarget, ATrigger,
      LValueNode, LDomain);
    LRoute := Default(TNyxEventRoute);
    LRoute.Trigger := ATrigger;
    LRoute.OriginID := AControl.ID;
    LRoute.SourceID := LSource.ID;
    LRoute.Payload := NyxSignalPayload;
    LRoute.Browser := ncAvailable;
    LRoute.Native := ncAvailable;

    if LValueNode <> nil then
    begin
      LRoute.ValueID := LValueNode.ID;
    end;

    if LDomain.Defined then
    begin
      LRoute.Payload := NyxScalarPayload(LDomain);
      LRoute.PayloadOptional := True;
    end;

    if (ATrigger = ntChange) and not KindIn(AControl.ProjectionKind,
      'input|memo|checkbox|switch|radio|select|spin|slider|date|time|color|code-editor|split-view') then
    begin
      LRoute.Browser := ncCustom;
      LRoute.Native := ncCustom;
    end;
    LMatch := -1;
    for LExisting := 0 to High(Result) do
    begin

      if (Result[LExisting].Trigger = ntNamed) and
        (Result[LExisting].Name.Name = LName.Name) then
      begin
        LMatch := LExisting;
        Break;
      end;
    end;

    if LMatch < 0 then
    begin
      LMatch := Length(Result);
      SetLength(Result, LMatch + 1);
      Result[LMatch] := NyxNamedEventSchema(LName, LName.Name,
        'Runs when ' + AControl.Prop('text', LName.Name) + ' is activated.',
        LRoute.Browser, LRoute.Native, LRoute.Payload);
      Result[LMatch].DeclaredProducer := False;

      if ATrigger = ntChange then
      begin
        Result[LMatch].Description := 'Reports accepted changes from ' +
          AControl.Prop('text', LName.Name) + '.';
      end;

      if TryNyxSemantic(LName.Name, LSemantic) then
      begin
        Result[LMatch].Title := NyxSemanticTitle(LSemantic);
      end;
    end;
    for LRouteIndex := 0 to High(Result[LMatch].Routes) do
    begin

      if (Result[LMatch].Routes[LRouteIndex].Trigger = ATrigger) and
        (Result[LMatch].Routes[LRouteIndex].OriginID = LRoute.OriginID) then
      begin
        Exit;
      end;
    end;
    LPayloadsMatch := Result[LMatch].Payload.Defined and
      (Result[LMatch].Payload.ToData.ToJSON = LRoute.Payload.ToData.ToJSON);

    if not LPayloadsMatch then
    begin
      Result[LMatch].Payload := Default(TNyxEventPayloadSpec);
      Result[LMatch].Description :=
        'Source controls provide different payloads; inspect each route.';
    end;
    Result[LMatch].PayloadOptional := Result[LMatch].PayloadOptional or LRoute.PayloadOptional;

    if (ATrigger = ntChange) and NyxSupportsTextInput(AControl) then
    begin
      Result[LMatch].Contexts := Result[LMatch].Contexts + [nctxTextEdit, nctxEditing];
    end;

    if Result[LMatch].Browser <> LRoute.Browser then
    begin
      Result[LMatch].Browser := ncBasic;
    end;

    if Result[LMatch].Native <> LRoute.Native then
    begin
      Result[LMatch].Native := ncBasic;
    end;
    LRouteIndex := Length(Result[LMatch].Routes);
    SetLength(Result[LMatch].Routes, LRouteIndex + 1);
    Result[LMatch].Routes[LRouteIndex] := LRoute.Copy;
  end;

  procedure CollectRoutes(AControl: TNyxNode; ACompound: Boolean);
  var
    LChildIndex: Integer;
  begin
    AddRoute(AControl, ntClick, atEmit);
    AddRoute(AControl, ntChange, atEmitChange);

    if ACompound then
    begin
      for LChildIndex := 0 to AControl.Count - 1 do
      begin

        if AControl.Children[LChildIndex].Prop('compound') <> 'true' then
        begin
          CollectRoutes(AControl.Children[LChildIndex], True);
        end;
      end;
    end;
  end;

  procedure Collect(AControl: TNyxNode; ACompound: Boolean);
  var
    LKind: TNyxText;
    LChildIndex: Integer;
    LEventIndex: Integer;
    LKeyboardTrigger: TNyxTrigger;
  begin
    LKind := AControl.ProjectionKind;

    { Both adapters wire the common physical click hook on every projected
      control. Focus/change still require an actual focusable/editable leaf. }
    Include(LTriggers, ntClick);
    Include(LBridgedTriggers, ntClick);
    { Both target adapters bridge these on the outer face and framed inputs.
      Containers retain pointer enter/exit meaning; ordinary bubbling never
      duplicates down/up/move on an ancestor control. }
    LTriggers := LTriggers + [ntDoubleClick, ntPointerDown, ntPointerUp,
      ntPointerMove, ntPointerEnter, ntPointerExit, ntContextMenu];
    LBridgedTriggers := LBridgedTriggers + [ntDoubleClick, ntPointerDown,
      ntPointerUp, ntPointerMove, ntPointerEnter, ntPointerExit, ntContextMenu];
    LTriggers := LTriggers + [ntPointerCancel, ntPointerCapture, ntPointerCaptureLost,
      ntDragStart, ntDrag, ntDragEnter, ntDragOver, ntDragExit, ntDrop, ntDragEnd];
    LBridgedTriggers := LBridgedTriggers + [ntPointerCancel, ntPointerCapture,
      ntPointerCaptureLost, ntDragStart, ntDrag, ntDragEnter, ntDragOver,
      ntDragExit, ntDrop, ntDragEnd];
    LTriggers := LTriggers + [ntBeforeWheel, ntWheel, ntAfterWheel];
    LBridgedTriggers := LBridgedTriggers + [ntBeforeWheel, ntWheel, ntAfterWheel];

    if NyxSupportsViewport(AControl) then
    begin
      LTriggers := LTriggers + [ntScroll, ntScrollEnd];
      LBridgedTriggers := LBridgedTriggers + [ntScroll, ntScrollEnd];
    end;

    if AControl.HasCollectionView and AControl.CollectionView.Defined then
    begin
      Include(LTriggers, ntSelectionChange);
      Include(LBridgedTriggers, ntSelectionChange);
    end;

    if NyxSupportsTextInput(AControl) then
    begin
      LTriggers := LTriggers + [ntBeforeTextInput, ntTextInput, ntAfterTextInput,
        ntBeforeEdit, ntCompositionStart, ntCompositionUpdate, ntCompositionEnd,
        ntTextSelectionChange];
      LBridgedTriggers := LBridgedTriggers + [ntBeforeTextInput, ntTextInput, ntAfterTextInput,
        ntBeforeEdit, ntCompositionStart, ntCompositionUpdate, ntCompositionEnd,
        ntTextSelectionChange];
    end;

    if KindIn(LKind, 'input|memo|checkbox|switch|radio|select|spin|slider|date|time|color|code-editor|split-view') then
    begin
      Include(LTriggers, ntChange);
      Include(LBridgedTriggers, ntChange);
    end;

    if NyxSupportsKeyboard(AControl) then
    begin
      Include(LTriggers, ntAfterEnter);
      Include(LTriggers, ntAfterExit);
      Include(LBridgedTriggers, ntAfterEnter);
      Include(LBridgedTriggers, ntAfterExit);
      for LKeyboardTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
      begin

        if NyxIsKeyboardTrigger(LKeyboardTrigger) then
        begin
          Include(LTriggers, LKeyboardTrigger);
          Include(LBridgedTriggers, LKeyboardTrigger);
        end;
      end;
    end;
    for LEventIndex := 0 to AControl.Contract.EventCount - 1 do
    begin

      if NyxIsRuntimeTrigger(AControl.Contract.EventAt(LEventIndex).Trigger) then
      begin
        Include(LTriggers, AControl.Contract.EventAt(LEventIndex).Trigger);
      end;
    end;

    if ACompound then
    begin
      for LChildIndex := 0 to AControl.Count - 1 do
      begin

        if AControl.Children[LChildIndex].Prop('compound') <> 'true' then
        begin
          Collect(AControl.Children[LChildIndex], True);
        end;
      end;
    end;
  end;

begin
  Result := nil;

  if ANode = nil then
  begin
    raise ENyxModel.Create('Event metadata requires a component');
  end;
  LRoot := ANode;
  LContext := nil;
  LOwned := (ADocument <> nil) and not ANode.IsRealized;

  if LOwned then
  begin
    LContext := RealizeNyxContext(ADocument, ANode, LRoot);

    if LRoot = nil then
    begin
      Exit;
    end;
  end;
  try
    LTriggers := [];
    LBridgedTriggers := [];
    Collect(LRoot, LRoot.Prop('compound') = 'true');
    for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
    begin

      if LTrigger in LTriggers then
      begin
        LIndex := Length(Result);
        SetLength(Result, LIndex + 1);
        Result[LIndex].Trigger := LTrigger;
        Result[LIndex].Title := NyxTriggerTitle(LTrigger);
        Result[LIndex].Description := 'Multiple ordered callbacks';

        if (LTrigger = ntChange) and NyxSupportsTextInput(LRoot) then
        begin
          Result[LIndex].Contexts := [nctxTextEdit, nctxEditing];
        end;

        if (LRoot.ProjectionKind = 'split-view') and (LTrigger = ntChange) then
        begin
          Result[LIndex].Description := 'Completed resize; integer first-pane percentage; multiple ordered callbacks';
        end;

        if NyxIsKeyboardTrigger(LTrigger) then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxKeyboard];
          Result[LIndex].Description := 'Typed keys/modifiers; emitted before platform default; sequential hooks may consume';

          if not NyxIsInputTrigger(LTrigger) then
          begin
            Result[LIndex].Description := 'After Nyx key dispatch; observes synchronous cancellation; cannot consume';
          end;
        end;

        if LTrigger in [ntBeforeTextInput, ntTextInput, ntAfterTextInput] then
        begin
          Result[LIndex].Contexts := [nctxTextEdit, nctxEditing];
          Result[LIndex].Description := 'Owned old/proposed text; includes paste, deletion and composition';

          if LTrigger = ntBeforeTextInput then
          begin
            Result[LIndex].Description := 'Before model admission; sequential callbacks may reject and restore text';
          end
          else if LTrigger = ntAfterTextInput then
          begin
            Result[LIndex].Description := 'After text admission attempt; observes accepted or cancelled proposal';
          end;
        end;

        if LTrigger in [ntPointerDown, ntPointerUp, ntPointerMove,
          ntPointerEnter, ntPointerExit, ntDoubleClick, ntContextMenu] then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxPointer];
          Result[LIndex].Description := 'Owned control-relative pointer data; browser touch/pen and LCL mouse';

          if LTrigger = ntContextMenu then
          begin
            Result[LIndex].Description := 'Pointer or keyboard menu request; sequential callbacks may consume default';
          end;
        end;
        Result[LIndex].Browser := ncAvailable;
        Result[LIndex].Native := ncAvailable;

        if LTrigger in [ntPointerCancel, ntPointerCapture, ntPointerCaptureLost] then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxPointer];
          Result[LIndex].Native := ncBasic;
          Result[LIndex].Description := 'Physical pointer capture acquisition/loss or cancellation; browser pointer identity and native mouse capture. Notifications cannot request capture; active sequential down/move callbacks may request it.';
        end;

        if LTrigger in [ntDragStart, ntDrag, ntDragEnter, ntDragOver, ntDragExit,
          ntDrop, ntDragEnd] then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxDrag, nctxPointer];
          Result[LIndex].Browser := ncBasic;
          Result[LIndex].Native := ncBasic;
          Result[LIndex].Description := 'Owned transfer formats; payloads readable only at start/drop. Opt in with DragSource/DropTarget; sequential start offers data, enter/over/drop negotiate allowed operation. Native LCL internal mouse drags; browser host drag support. No automatic model mutation.';

          if LTrigger = ntDrag then
          begin
            Result[LIndex].Native := ncMissing;
            Result[LIndex].Description := 'Browser source drag progress; protected transfer formats. LCL has no corresponding source progress slot; use target OnDragOver for native movement.';
          end;
        end;

        if LTrigger = ntBeforeEdit then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxEditing];
          Result[LIndex].Description := 'Physical beforeinput; typed edit intent, optional data and scalar selection; cancellation follows host cancelability outside composition; unavailable in LCL';
          Result[LIndex].Native := ncMissing;
        end;

        if LTrigger in [ntCompositionStart, ntCompositionUpdate, ntCompositionEnd] then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxEditing];
          Result[LIndex].Description := 'Owned physical IME context; accepted model admission waits for composition end; Win32 LCL observes genuine messages and drains final characters at UI idle; other widgetsets require a bridge';
          Result[LIndex].Native := ncBasic;
        end;

        if LTrigger = ntTextSelectionChange then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxEditing];
          Result[LIndex].Description := 'Owned Unicode scalar selection against physical text; browser observes select/selectionchange; Win32 LCL coalesces at idle and cannot report direction';
          Result[LIndex].Browser := ncBasic;
          Result[LIndex].Native := ncBasic;
        end;

        if LTrigger in [ntBeforeWheel, ntWheel, ntAfterWheel] then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxWheel];
          Result[LIndex].Description := 'Owned wheel request; explicit pixels/lines/pages or native detents; cancellation depends on platform';

          if LTrigger = ntAfterWheel then
          begin
            Result[LIndex].Description := 'After Nyx wheel dispatch, before platform default; observes cancellation, not actual movement';
          end;
        end;

        if LTrigger = ntScroll then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxViewport];
          Result[LIndex].Description := 'Actual viewport movement from any origin; typed offsets/units; LCL coalesces at UI idle; cannot consume';
          Result[LIndex].Native := ncBasic;
        end;

        if LTrigger = ntScrollEnd then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxViewport];
          Result[LIndex].Description := 'Actual browser scrollend when supported by the host; no timeout approximation; unavailable in LCL';
          Result[LIndex].Browser := ncBasic;
          Result[LIndex].Native := ncMissing;
        end;

        if LTrigger = ntSelectionChange then
        begin
          Result[LIndex].Contexts := Result[LIndex].Contexts + [nctxCollectionSelection];
          Result[LIndex].Description := 'Owned collection selection before/after; ' +
            'typed item membership, independent focus and range anchor; requires ' +
            'an admitted collection binding on this control or compound part';
        end;

        if not (LTrigger in LBridgedTriggers) then
        begin
          { A semantic declaration describes a callback but does not manufacture
            a physical adapter hook. Custom factories must publish that hook. }
          Result[LIndex].Description := 'Declared callback; requires a custom adapter event bridge';
          Result[LIndex].Browser := ncCustom;
          Result[LIndex].Native := ncCustom;
        end;
      end;
    end;
    CollectRoutes(LRoot, LRoot.Prop('compound') = 'true');
    for LPublishedIndex := 0 to High(GPublishedSchemas) do
    begin

      if (GPublishedSchemas[LPublishedIndex].Kind <> ANode.Kind) and
        (GPublishedSchemas[LPublishedIndex].Kind <> LRoot.Kind) then
      begin
        Continue;
      end;
      for LEventIndex := 0 to High(GPublishedSchemas[LPublishedIndex].Events) do
      begin
        LFound := -1;
        for LIndex := 0 to High(Result) do
        begin

          if (Result[LIndex].Trigger = GPublishedSchemas[LPublishedIndex].Events[LEventIndex].Trigger) and
            (Result[LIndex].Name.Name = GPublishedSchemas[LPublishedIndex].Events[LEventIndex].Name.Name) then
          begin
            LFound := LIndex;
            Break;
          end;
        end;

        LPublished := GPublishedSchemas[LPublishedIndex].Events[LEventIndex].Copy;

        if LFound < 0 then
        begin
          LFound := Length(Result);
          SetLength(Result, LFound + 1);
        end
        else
        begin
          { The creator's payload admits its custom producer. Existing physical
            routes keep their own independent contracts under the same name. }
          LPublished.Routes := Result[LFound].Copy.Routes;
        end;
        Result[LFound] := LPublished;
      end;
    end;
    { Keep exact authored names discoverable even when their creator is absent.
      This is an honest custom requirement, not a manufactured producer schema. }
    LAuthored := NyxAuthoredEvents(LRoot);
    for LEventIndex := 0 to High(LAuthored) do
    begin

      if LAuthored[LEventIndex].Trigger <> ntNamed then
      begin
        Continue;
      end;
      LFound := -1;
      for LIndex := 0 to High(Result) do
      begin

        if (Result[LIndex].Trigger = ntNamed) and
          (Result[LIndex].Name.Name = LAuthored[LEventIndex].Name.Name) then
        begin
          LFound := LIndex;
          Break;
        end;
      end;

      if LFound < 0 then
      begin
        LFound := Length(Result);
        SetLength(Result, LFound + 1);
        Result[LFound] := Default(TNyxEventSchema);
        Result[LFound].Trigger := ntNamed;
        Result[LFound].Name := LAuthored[LEventIndex].Name;
        Result[LFound].Title := LAuthored[LEventIndex].Name.Name;
        Result[LFound].Description := 'Named callback; requires its creator payload declaration and adapter producer';
        Result[LFound].Browser := ncCustom;
        Result[LFound].Native := ncCustom;
      end;
    end;
  finally

    if LOwned then
    begin
      ReleaseNyxNode(LContext);
    end;
  end;
end;

function FindNyxNamedEvent(ANode: TNyxNode; const AName: TNyxEventRef;
  out ASchema: TNyxEventSchema): Boolean;
var
  LPublishedIndex: Integer;
  LEventIndex: Integer;
begin
  ASchema := Default(TNyxEventSchema);

  if ANode = nil then
  begin
    raise ENyxModel.Create('Named producer lookup requires a component');
  end;
  { Discovery aliases do not authorize a custom producer. Only an explicitly
    registered creator declaration can admit Emit on this component kind. }
  for LPublishedIndex := 0 to High(GPublishedSchemas) do
  begin

    if GPublishedSchemas[LPublishedIndex].Kind <> ANode.Kind then
    begin
      Continue;
    end;
    for LEventIndex := 0 to High(GPublishedSchemas[LPublishedIndex].Events) do
    begin

      if (GPublishedSchemas[LPublishedIndex].Events[LEventIndex].Trigger = ntNamed) and
        (GPublishedSchemas[LPublishedIndex].Events[LEventIndex].Name.Name = AName.Name) then
      begin
        ASchema := GPublishedSchemas[LPublishedIndex].Events[LEventIndex].Copy;
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

function IntrinsicValueDomain(ANode: TNyxNode): TNyxValueDomain;
var
  LKind: TNyxText;
begin
  Result := NyxNoDomain;
  LKind := ANode.ProjectionKind;

  if KindIn(LKind, 'checkbox|switch|radio') then
  begin
    Exit(NyxBooleanDomain.Definition);
  end;

  if KindIn(LKind, 'spin|slider|progress') then
  begin
    Exit(NyxIntegerDomain.Definition);
  end;

  if (LKind = 'input') and (ANode.Prop('input-type') = 'number') then
  begin
    Exit(NyxNumberDomain.Definition);
  end;

  if KindIn(LKind, 'input|memo|select|date|time|color|code-editor') then
  begin
    Exit(NyxTextDomain.Definition);
  end;
end;

function NyxNodeValueDomain(ANode: TNyxNode): TNyxValueDomain;
var
  LOwner: TNyxNode;
  LField: TNyxFieldContract;
  LTarget: TNyxNode;
  LIndex: Integer;
begin
  Result := NyxNoDomain;

  if ANode = nil then
  begin
    raise ENyxModel.Create('Value domain requires a node');
  end;

  if ANode.Contract.FindValue(Result) then
  begin
    Exit;
  end;
  LOwner := ANode.Parent;
  while LOwner <> nil do
  begin
    for LIndex := 0 to LOwner.Contract.FieldCount - 1 do
    begin
      LField := LOwner.Contract.FieldAt(LIndex);
      LTarget := nil;
      try
        LTarget := LOwner.Part(LField.Part);
      except
        on LException: ENyxModel do
        begin
          { Raw reusable references are checked after independent realization;
            a missing final part is diagnosed by contract tree admission. }
          LTarget := nil;
        end;
      end;

      if LTarget = ANode then
      begin
        Exit(LField.Domain.Copy);
      end;
    end;
    LOwner := LOwner.Parent;
  end;
  Result := IntrinsicValueDomain(ANode);

  if Result.Defined then
  begin
    Exit;
  end;
  { Legacy catalog selections have an exact established meaning even before
    explicit declarations were serialized. New catalog nodes declare it. }

  if ANode.Kind = 'rating' then
  begin
    Exit(NyxIntegerDomain.Range(1, 5).Definition);
  end;

  if ANode.Kind = 'segmented-control' then
  begin
    Exit(NyxTextDomain.Definition);
  end;

  if ANode.Kind = 'pagination' then
  begin
    Exit(NyxIntegerDomain.Range(1, 100).Definition);
  end;
end;

function TryNyxInteger(const AValue: TNyxText; out AValueNumber: Integer): Boolean;
begin
  Result := TryNyxStateInteger(AValue, AValueNumber);
end;

procedure ValidateNyxProperties(ANode: TNyxNode; ADocument: TNyxDocument);
var
  LProperties: TNyxPropertyInfos;
  LIndex: Integer;
  LValue: TNyxText;
  LNumber: Integer;
  LMinimum: Integer;
  LMaximum: Integer;
  LValid: Boolean;
  LPlatform: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LSplitPosition: Integer;
  LSplitMinimum: Integer;
  LSplitMaximum: Integer;

  LMinimumText: TNyxText;
  LMaximumText: TNyxText;
  LDomain: TNyxValueDomain;
  LField: TNyxFieldContract;
  LTarget: TNyxNode;
  LHasReference: Boolean;
  LPhysical: TNyxValueDomain;
  LEvent: TNyxEventContract;
  LEventOwner: TNyxNode;
  LScalar: Double;

  function SplitMetric(APlatform: TNyxPlatform; AKey: TNyxAttribute;
    ADefault: Integer): Integer;
  var
    LText: TNyxText;
  begin
    LText := ANode.Prop(NyxPlatformKey(APlatform, AKey),
      ANode.Prop(NyxAttributeName(AKey), IntToStr(ADefault)));
    Result := StrToIntDef(LText, ADefault);
  end;

  procedure CheckFamily(const ADeclared, APhysical: TNyxValueDomain);
  begin

    if APhysical.Defined and (not ADeclared.Defined or
      ((ADeclared.Kind <> APhysical.Kind) and not
      ((ADeclared.Kind = nskInteger) and (APhysical.Kind = nskNumber)))) then
    begin
      raise ENyxContract.Create('Declared domain conflicts with control value family: ' + ANode.ID);
    end;
  end;

  function ContainsReference(ANode: TNyxNode): Boolean;
  var
    LChildIndex: Integer;
  begin
    Result := (ANode.Kind = 'component') or (ANode.ProjectionKind = 'component');
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      Result := Result or ContainsReference(ANode.Children[LChildIndex]);
    end;
  end;
begin
  ANode.Contract.Validate;
  ValidateNyxCallbacks(ANode);
  LDomain := NyxNodeValueDomain(ANode);
  CheckFamily(LDomain, IntrinsicValueDomain(ANode));

  if LDomain.Defined and (ANode.Props.IndexOfName('value') >= 0) and
    ((ANode.Prop('value') <> '') or (LDomain.Kind = nskText)) then
  begin
    LDomain.ReadWire(ANode.Prop('value'));
  end;
  { Raw reference trees defer field paths to their independently realized view.
    A final runtime/concrete tree must retain every declared named field. }
  LHasReference := (ADocument <> nil) and ContainsReference(ANode);

  if not LHasReference then
  begin
    for LIndex := 0 to ANode.Contract.FieldCount - 1 do
    begin
      LField := ANode.Contract.FieldAt(LIndex);
      LTarget := ANode.Part(LField.Part);
      LPhysical := IntrinsicValueDomain(LTarget);
      CheckFamily(LField.Domain, LPhysical);
      LPhysical := NyxNodeValueDomain(LTarget);
      CheckFamily(LField.Domain, LPhysical);

      if LTarget.Props.IndexOfName('value') >= 0 then
      begin

        if (LTarget.Prop('value') <> '') or (LField.Domain.Kind = nskText) then
        begin
          LField.Domain.ReadWire(LTarget.Prop('value'));
        end;
      end;
    end;
    for LIndex := 0 to ANode.Contract.EventCount - 1 do
    begin
      LEvent := ANode.Contract.EventAt(LIndex);

      if LEvent.ValueSource.Source = nvsPart then
      begin
        LEventOwner := ANode;
        while (LEventOwner.Parent <> nil) and (LEventOwner.Prop('compound') <> 'true') do
        begin
          LEventOwner := LEventOwner.Parent;
        end;
        LTarget := LEventOwner.Part(NyxPart(LEvent.ValueSource.Part));
        LPhysical := NyxNodeValueDomain(LTarget);

        if LPhysical.Defined and (LEvent.Domain.Kind <> LPhysical.Kind) and not
          ((LEvent.Domain.Kind = nskNumber) and (LPhysical.Kind = nskInteger)) then
        begin
          raise ENyxContract.Create('Event payload conflicts with its named field: ' + ANode.ID);
        end;
      end;
    end;
  end;
  LProperties := NyxProperties(ANode, ADocument);
  for LIndex := 0 to ANode.Props.Count - 1 do
  begin
    LValue := ANode.Props.Names[LIndex];

    if (Copy(LValue, 1, 5) = '@nyx.') and
      not TryNyxPlatformKey(LValue, LPlatform, LAttribute) then
    begin
      raise ENyxModel.Create('Unknown or nonportable platform property on ' + ANode.ID);
    end;
  end;

  if ANode.ProjectionKind = 'split-view' then
  begin

    if ANode.Count > 2 then
    begin
      raise ENyxModel.Create('Split view accepts at most two child panes: ' + ANode.ID);
    end;
    for LPlatform := npfAny to npfNativeLCL do
    begin
      LSplitPosition := SplitMetric(LPlatform, atSplitPosition, 65);
      LSplitMinimum := SplitMetric(LPlatform, atSplitMinimum, 15);
      LSplitMaximum := SplitMetric(LPlatform, atSplitMaximum, 85);

      if (LSplitMinimum > LSplitMaximum) or (LSplitPosition < LSplitMinimum) or
        (LSplitPosition > LSplitMaximum) then
      begin
        raise ENyxModel.Create('Split position must fit its bounds on ' + ANode.ID);
      end;
    end;
  end;
  LMinimumText := ANode.Prop('min');
  LMaximumText := ANode.Prop('max');
  for LIndex := 0 to Length(LProperties) - 1 do
  begin
    LValue := ANode.Prop(LProperties[LIndex].Key, LProperties[LIndex].DefaultValue);

    if LProperties[LIndex].Key = 'min' then
    begin
      LMinimumText := LValue;
    end;

    if LProperties[LIndex].Key = 'max' then
    begin
      LMaximumText := LValue;
    end;
    { Empty explicitly clears an optional property, retaining its absence/default
      behavior. Reference existence and graph cycles are checked by the model. }

    if LValue = '' then
    begin
      Continue;
    end;
    LValid := True;
    case LProperties[LIndex].ValueType of
      npText, npLines:
        begin
          { Portable widget text must survive native terminated strings. State
            and opaque extension data still retain exact NUL/Unicode values;
            this check belongs only to recognized control properties. }
          LValid := Pos(#0, LValue) = 0;

          if (LProperties[LIndex].Key = 'value') and
            KindIn(ANode.ProjectionKind, 'input|select|date|time|color') then
          begin
            LValid := LValid and (Pos(#10, LValue) = 0) and (Pos(#13, LValue) = 0);
          end;
        end;
      npReference:
        begin
          LValid := True;
        end;
      npBoolean:
        begin
          LValid := (LValue = 'true') or (LValue = 'false');
        end;
      npInteger:
        begin
          LValid := TryNyxInteger(LValue, LNumber);

          if LValid then
          begin
            LValid := (LNumber >= LProperties[LIndex].Minimum) and
              (LNumber <= LProperties[LIndex].Maximum);
          end;
        end;
      npNumber:
        begin
          LValid := TryNyxStateNumber(LValue, LScalar);
        end;
      npChoice:
        begin
          LValid := Pos(#10 + LValue + #10,
            #10 + LProperties[LIndex].Choices + #10) > 0;
        end;
    end;

    if not LValid then
    begin
      raise ENyxModel.Create('Invalid ' + LProperties[LIndex].Key + ' on ' + ANode.ID + ': ' + LValue);
    end;
  end;

  if (LMinimumText <> '') and (LMaximumText <> '') and
    TryNyxInteger(LMinimumText, LMinimum) and TryNyxInteger(LMaximumText, LMaximum) and
    (LMinimum > LMaximum) then
  begin
    raise ENyxModel.Create('Minimum exceeds maximum on ' + ANode.ID);
  end;
end;

procedure ValidateNyxPropertyTree(ARoot: TNyxNode; ADocument: TNyxDocument);
var
  LCount: Integer;

  procedure Visit(ANode: TNyxNode; ADepth: Integer);
  var
    LIndex: Integer;
  begin
    Inc(LCount);

    if (ADepth > 128) or (LCount > 10000) then
    begin
      raise ENyxModel.Create('Property tree exceeds depth or node budget');
    end;
    { Override descriptors are admitted structurally by the document. Their
      target-dependent values are checked on the independent realized tree,
      rather than assigning the descriptor an unrelated primitive schema. }

    if ANode.Kind <> 'slot-override' then
    begin
      ValidateNyxProperties(ANode, ADocument);
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex], ADepth + 1);
    end;
  end;
begin
  LCount := 0;
  Visit(ARoot, 0);
end;

procedure ValidateNyxDocumentProperties(ADocument: TNyxDocument);
var
  LIndex: Integer;
  LRequiresRealization: Boolean;

  function RequiresRealization(ANode: TNyxNode): Boolean;
  var
    LChildIndex: Integer;
  begin
    Result := (ANode.Kind = 'slot-override') or (ANode.BindingCount > 0) or
      ANode.HasCollectionView;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin

      if RequiresRealization(ANode.Children[LChildIndex]) then
      begin
        Exit(True);
      end;
    end;
  end;

  procedure ValidateRealized(ARoot: TNyxNode);
  var
    LRuntime: TNyxNode;

    procedure CheckHosts(ANode: TNyxNode);
    var
      LInfo: TNyxPrimitiveInfo;
      LChildIndex: Integer;
    begin

      if FindNyxPrimitive(ANode.ProjectionKind, LInfo) and not LInfo.Container and
        (ANode.Count > 0) then
      begin
        raise ENyxModel.Create('Part override cannot add children to a leaf: ' + ANode.ID);
      end;

      if ANode.HasCollectionView and ANode.CollectionView.Defined then
      begin
        ValidateNyxCollectionViewSnapshot(ADocument.Collections.Snapshot(ANode.CollectionView.Key),
          ANode.CollectionView, NyxCollectionProjectionForNode(ANode));
      end;
      for LChildIndex := 0 to ANode.Count - 1 do
      begin
        CheckHosts(ANode.Children[LChildIndex]);
      end;
    end;
  begin
    LRuntime := RealizeNyxView(ADocument, ARoot);
    try
      CheckHosts(LRuntime);
      ApplyNyxBindings(LRuntime, ADocument.State);
    finally
      LRuntime.Free;
    end;
  end;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Property validation requires a document');
  end;
  ADocument.Validate;
  { Semantic palette data is admitted with the whole document, before either
    renderer or source workspace can publish a candidate. }
  ValidateNyxDesignTokens(ADocument);
  LRequiresRealization := False;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin
    ValidateNyxPropertyTree(ADocument.Components[LIndex], ADocument);
    LRequiresRealization := LRequiresRealization or RequiresRealization(ADocument.Components[LIndex]);
  end;
  for LIndex := 0 to ADocument.Count - 1 do
  begin
    ValidateNyxPropertyTree(ADocument.Pages[LIndex], ADocument);
    LRequiresRealization := LRequiresRealization or RequiresRealization(ADocument.Pages[LIndex]);
  end;

  if LRequiresRealization then
  begin
    { Check paths, host admission and effective property values before Studio
      accepts history or a compiler job starts. No renderer or machine tools are
      needed. Expansion budgets still apply to every independently built view. }
    for LIndex := 0 to ADocument.ComponentCount - 1 do
    begin
      ValidateRealized(ADocument.Components[LIndex]);
    end;
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      ValidateRealized(ADocument.Pages[LIndex]);
    end;
  end;
end;

end.
