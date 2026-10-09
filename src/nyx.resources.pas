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

unit nyx.resources;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.images, nyx.state, nyx.resource.sources;

const
  NyxResourcesWireField = 'resources';
  NyxMaximumResources = 128;
  NyxMaximumResourceWireBytes = 3 * 1048576;
  { Admission limits apply per definition's ordered label set. The byte limit
    covers its complete encoded UTF-8 JSON, independently of payload budgets. }
  NyxMaximumResourceLabels = 32;
  NyxMaximumResourceLabelBytes = 8192;

type
  { Closed payload families. JSON is preserved as exact UTF-8 source and parsed
    data; arbitrary bytes use Base64 only at the explicit persistence boundary. }
  TNyxResourceKind = (nrkImage, nrkJSON, nrkText, nrkBinary);
  { Preserve UTF-8 diagnostic text through the native exception boundary. }
  ENyxResource = class(ENyxState);

  { Distinct application resource and locale identities. Their names are open
    Unicode data, never paths, target handles or behavioral string switches. }
  TNyxResourceRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;
  TNyxLocaleRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;

  { An exact creator-owned discovery label. Its printable Unicode name is open
    application data, not a payload kind, path or behavioral string switch. }
  TNyxResourceLabelRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;

  { Independent immutable ordered set of exact labels. Add is idempotent and
    retains first insertion order; Remove returns another set. Labels may contain
    spaces, punctuation or supplementary Unicode without delimiter parsing.
    Empty/default means no labels. Wire admission rejects duplicate labels,
    malformed names, excessive count and aggregate UTF-8 data. No definition,
    payload, catalog, project or target is retained. }
  TNyxResourceLabels = record
  private
    FData: TNyxDataValue;
    function GetCount: Integer;
  public
    function Add(const ALabel: TNyxResourceLabelRef): TNyxResourceLabels;
    function Remove(const ALabel: TNyxResourceLabelRef): TNyxResourceLabels;
    function Contains(const ALabel: TNyxResourceLabelRef): Boolean;
    function Item(AIndex: Integer): TNyxResourceLabelRef;
    function Copy: TNyxResourceLabels;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceLabels; static;
    property Count: Integer read GetCount;
  end;

  { Immutable structural JSON selector. Field/Item append independent copies;
    no dotted path parsing or implicit scalar conversion occurs. Wrong shapes,
    missing fields and out-of-range rows refuse at Select. Empty means the root. }
  TNyxResourcePath = record
  private
    FSteps: TNyxDataValue;
  public
    function Field(const AName: TNyxText): TNyxResourcePath;
    function Item(AIndex: Integer): TNyxResourcePath;
    function Select(const AData: TNyxDataValue): TNyxDataValue;
    function Copy: TNyxResourcePath;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourcePath; static;
  end;

  { Managed immutable file content and creator help. Describe returns another
    definition rather than mutating subscribers or the original object. Bytes
    returns an independent array. Image/Text/Data require their exact kind;
    binary bytes are never silently treated as UTF-8/JSON or coerced as images.
    No document, widget, stream or filename is retained. }
  INyxResourceDefinition = interface
    ['{970743DD-6D6E-436C-9963-94DA16B7B571}']
    function GetKind: TNyxResourceKind;
    function GetTitle: TNyxText;
    function GetDescription: TNyxText;
    function GetByteCount: Integer;
    function GetSource: TNyxResourceSource;
    function GetFallback: INyxResourceDefinition;
    function Bytes: TNyxBytes;
    function Text: TNyxText;
    function Data: TNyxDataValue;
    function Image: TNyxImageSource;
    function Describe(const ATitle, ADescription: TNyxText): INyxResourceDefinition;
    { Hosted defaults may carry an explicitly authored embedded fallback for
      immediate/offline rendering. It must have the same file kind. Missing
      fallback refuses synchronous data access until a resolver has loaded it. }
    function Fallback(const ADefinition: INyxResourceDefinition): INyxResourceDefinition;
    function Cache(const APolicy: TNyxResourceCachePolicy): INyxResourceDefinition;
    function ToData: TNyxDataValue;
    property Kind: TNyxResourceKind read GetKind;
    property Title: TNyxText read GetTitle;
    property Description: TNyxText read GetDescription;
    property ByteCount: Integer read GetByteCount;
    property Source: TNyxResourceSource read GetSource;
    property FallbackDefinition: INyxResourceDefinition read GetFallback;
  end;

  { Optional specialized discovery capability. The original definition
    interface/GUID stays unchanged for alternative implementations. Built-in
    factories expose this interface; a base definition can be normalized through
    NyxResourceDiscovery. Fluent changes return an independently owned immutable
    definition. Help/cache/fallback copies retain these annotations but preserve
    their original base return type: configure tags before those calls, or adapt
    their result explicitly before configuring more labels. }
  INyxResourceDiscovery = interface(INyxResourceDefinition)
    ['{783229DE-E519-44FB-9627-7516672948E8}']
    function GetLabels: TNyxResourceLabels;
    function WithLabels(const ALabels: TNyxResourceLabels): INyxResourceDiscovery;
    function Tagged(const ALabel: TNyxResourceLabelRef): INyxResourceDiscovery;
    property Labels: TNyxResourceLabels read GetLabels;
  end;

  { Document-owned registry of immutable definitions, including optional locale
    variants. Foreign definitions are normalized at Define. Replacements retain
    order and admit byte/catalog budgets before mutation. Clone owns independent
    membership; retained definitions/registries have no cycle back to a document.
    Exact locale, explicit fallback, then the unlocalized default is the lookup
    order. Missing resources refuse; they are never substituted with empty data. }
  INyxResources = interface
    ['{38DC73C7-3172-4315-8C39-951DB89EDEBD}']
    function Define(const AReference: TNyxResourceRef;
      const ADefinition: INyxResourceDefinition): INyxResources; overload;
    function Define(const AReference: TNyxResourceRef; const ALocale: TNyxLocaleRef;
      const ADefinition: INyxResourceDefinition): INyxResources; overload;
    function Remove(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): INyxResources;
    function Contains(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): Boolean;
    function Definition(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): INyxResourceDefinition;
    function Resolve(const AReference: TNyxResourceRef;
      const ALocale, AFallback: TNyxLocaleRef): INyxResourceDefinition;
    function Reference(AIndex: Integer): TNyxResourceRef;
    function Locale(AIndex: Integer): TNyxLocaleRef;
    function GetCount: Integer;
    function Clone: INyxResources;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

  { A copied, strongly typed scalar selector. Localize fixes an explicit locale,
    including NyxDefaultLocale; Localized distinguishes that pin from inheritance.
    Otherwise the runtime view's locale/fallback selects a variant. Field and
    Item preserve JSON structure, including literal dots in field names. Reading
    an absent/null/wrong-kind value raises; no default caption hides bad data.
    Number deliberately requests a Double, while the resource retains its exact
    JSON numeric token. Selectors own no document, store or target handle. }
  TNyxResourceValueRef = record
  private
    FReference: TNyxResourceRef;
    FPath: TNyxResourcePath;
    FLocale: TNyxLocaleRef;
    FFallback: TNyxLocaleRef;
    FKind: TNyxStateKind;
    FLocalized: Boolean;
  public
    function Field(const AName: TNyxText): TNyxResourceValueRef;
    function Item(AIndex: Integer): TNyxResourceValueRef;
    function Localize(const ALocale, AFallback: TNyxLocaleRef): TNyxResourceValueRef;
    function AsText: TNyxResourceValueRef;
    function AsBoolean: TNyxResourceValueRef;
    function AsInteger: TNyxResourceValueRef;
    function AsNumber: TNyxResourceValueRef;
    function Copy: TNyxResourceValueRef;
    function Read(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef): TNyxStateValue;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceValueRef; static;
    property Reference: TNyxResourceRef read FReference;
    property Path: TNyxResourcePath read FPath;
    property Locale: TNyxLocaleRef read FLocale;
    property Fallback: TNyxLocaleRef read FFallback;
    property Kind: TNyxStateKind read FKind;
    property Localized: Boolean read FLocalized;
  end;

type
  { A distinct read-only image selector. It resolves one named image variant,
    never a scalar JSON path or an arbitrary URL. Localize fixes the selector's
    locale/fallback; otherwise the mounted view supplies both. Copies retain
    only immutable names and can outlive their document and runtime catalog. }
  TNyxResourceImageRef = record
  private
    FReference: TNyxResourceRef;
    FLocale: TNyxLocaleRef;
    FFallback: TNyxLocaleRef;
    FLocalized: Boolean;
  public
    function Localize(const ALocale, AFallback: TNyxLocaleRef): TNyxResourceImageRef;
    function Read(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef): TNyxImageSource;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceImageRef; static;
    property Reference: TNyxResourceRef read FReference;
    property Locale: TNyxLocaleRef read FLocale;
    property Fallback: TNyxLocaleRef read FFallback;
    property Localized: Boolean read FLocalized;
  end;

function NyxResourceRef(const AName: TNyxText): TNyxResourceRef;
function NyxLocale(const AName: TNyxText): TNyxLocaleRef;
function NyxResourceLabel(const AName: TNyxText): TNyxResourceLabelRef;
function NyxResourceLabels: TNyxResourceLabels;
{ Retained immutable capability. A foreign base-only definition is normalized
  through its strict wire before exposing discovery; nil/invalid definitions refuse. }
function NyxResourceDiscovery(const ADefinition: INyxResourceDefinition): INyxResourceDiscovery;
{ Base-only implementations have no labels. Returned sets are independently owned. }
function NyxResourceLabelsOf(const ADefinition: INyxResourceDefinition): TNyxResourceLabels;
{ Empty locale selects an ordinary default; it is distinct from a missing
  resource name. Explicit fallback is supplied by the caller, never host locale. }
function NyxDefaultLocale: TNyxLocaleRef;
function NyxResourcePath: TNyxResourcePath;
{ Text is the default scalar projection. Other choices remain explicit fluent
  operations so behavior never depends on an untyped property string. }
function NyxResourceValue(const AReference: TNyxResourceRef): TNyxResourceValueRef;
{ The image family is deliberately separate from scalar selectors. Wrong file
  kinds and hosted files without a loaded value/authored fallback refuse Read. }
function NyxResourceImage(const AReference: TNyxResourceRef): TNyxResourceImageRef;
function NyxTextResource(const AText: TNyxText): INyxResourceDiscovery;
function NyxJSONResource(const AText: TNyxText): INyxResourceDiscovery; overload;
function NyxJSONResource(const AData: TNyxDataValue): INyxResourceDiscovery; overload;
function NyxImageResource(const AImage: TNyxImageSource): INyxResourceDiscovery;
function NyxBinaryResource(const ABytes: TNyxBytes): INyxResourceDiscovery;
function NyxHostedResource(AKind: TNyxResourceKind;
  const AURL: TNyxResourceURL): INyxResourceDiscovery;
{ Closed file kind is selected explicitly at import. Image headers determine the
  PNG/JPEG format; JSON/text require strict UTF-8 and preserve original bytes. }
function NyxResourceFromBytes(AKind: TNyxResourceKind;
  const ABytes: TNyxBytes): INyxResourceDefinition;
function NewNyxResources: INyxResources;
{ Versioned strict persistence boundaries; unknown members/scalar kinds refuse. }
function NyxResourceFromData(const AData: TNyxDataValue): INyxResourceDefinition;
function NyxResourcesFromData(const AData: TNyxDataValue): INyxResources;
function NyxResourceKindName(AKind: TNyxResourceKind): TNyxText;

implementation

type
  TResource = class(TInterfacedObject, INyxResourceDefinition, INyxResourceDiscovery)
  private
    FKind: TNyxResourceKind;
    FTitle: TNyxText;
    FDescription: TNyxText;
    FContent: TNyxText;
    FByteCount: Integer;
    FData: TNyxDataValue;
    FImage: TNyxImageSource;
    FHosted: Boolean;
    FSource: TNyxResourceSource;
    FFallback: INyxResourceDefinition;
    FLabels: TNyxResourceLabels;
    procedure RequireKind(AKind: TNyxResourceKind);
  public
    function GetKind: TNyxResourceKind;
    function GetTitle: TNyxText;
    function GetDescription: TNyxText;
    function GetByteCount: Integer;
    function GetSource: TNyxResourceSource;
    function GetFallback: INyxResourceDefinition;
    function Bytes: TNyxBytes;
    function Text: TNyxText;
    function Data: TNyxDataValue;
    function Image: TNyxImageSource;
    function Describe(const ATitle, ADescription: TNyxText): INyxResourceDefinition;
    function Fallback(const ADefinition: INyxResourceDefinition): INyxResourceDefinition;
    function Cache(const APolicy: TNyxResourceCachePolicy): INyxResourceDefinition;
    function ToData: TNyxDataValue;
    function GetLabels: TNyxResourceLabels;
    function WithLabels(const ALabels: TNyxResourceLabels): INyxResourceDiscovery;
    function Tagged(const ALabel: TNyxResourceLabelRef): INyxResourceDiscovery;
  end;
  { Owned entries use classes because pas2js cannot put managed interfaces in a
    record. Cloning allocates fresh entries and shares only immutable content. }
  TResourceEntry = class
    Reference: TNyxResourceRef;
    Locale: TNyxLocaleRef;
    Definition: INyxResourceDefinition;
  end;
  TResources = class(TInterfacedObject, INyxResources)
  private
    FEntries: array of TResourceEntry;
    function IndexOf(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): Integer;
    procedure CheckIndex(AIndex: Integer);
  public
    destructor Destroy; override;
    function Define(const AReference: TNyxResourceRef;
      const ADefinition: INyxResourceDefinition): INyxResources; overload;
    function Define(const AReference: TNyxResourceRef; const ALocale: TNyxLocaleRef;
      const ADefinition: INyxResourceDefinition): INyxResources; overload;
    function Remove(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): INyxResources;
    function Contains(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): Boolean;
    function Definition(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): INyxResourceDefinition;
    function Resolve(const AReference: TNyxResourceRef;
      const ALocale, AFallback: TNyxLocaleRef): INyxResourceDefinition;
    function Reference(AIndex: Integer): TNyxResourceRef;
    function Locale(AIndex: Integer): TNyxLocaleRef;
    function GetCount: Integer;
    function Clone: INyxResources;
    function ToData: TNyxDataValue;
  end;

procedure Name(const AName: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LIndex := 1;
  LCount := 0;

  if AName = '' then
  begin
    raise ENyxResource.Create('A resource, locale or label name is required');
  end;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or (LScalar < 32) or (LScalar = 127) then
    begin
      raise ENyxResource.Create('Resource names require printable Unicode scalars');
    end;
    Inc(LCount);

    if LCount > 128 then
    begin
      raise ENyxResource.Create('Resource name exceeds 128 Unicode scalars');
    end;
  end;
end;

function NyxResourceRef(const AName: TNyxText): TNyxResourceRef;
begin
  Name(AName);
  Result.FName := AName;
end;

function NyxLocale(const AName: TNyxText): TNyxLocaleRef;
begin
  Name(AName);
  Result.FName := AName;
end;

function NyxDefaultLocale: TNyxLocaleRef;
begin
  Result := Default(TNyxLocaleRef);
end;

function NyxResourceLabel(const AName: TNyxText): TNyxResourceLabelRef;
begin
  Name(AName);
  Result.FName := AName;
end;

function TNyxResourceLabelRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NyxResourceLabels: TNyxResourceLabels;
begin
  Result := Default(TNyxResourceLabels);
end;

function TNyxResourceLabels.GetCount: Integer;
begin
  Result := 0;

  if FData.Defined then
  begin
    Result := FData.Count;
  end;
end;

function TNyxResourceLabels.Item(AIndex: Integer): TNyxResourceLabelRef;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxResource.Create('Resource label index is out of range');
  end;
  Result := NyxResourceLabel(FData.Item(AIndex).AsText);
end;

function TNyxResourceLabels.Contains(const ALabel: TNyxResourceLabelRef): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin

    if Item(LIndex).Name = ALabel.Name then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxResourceLabels.ToData: TNyxDataValue;
begin
  Result := NyxArray([]);

  if FData.Defined then
  begin
    Result := FData.Copy;
  end;
end;

class function TNyxResourceLabels.FromData(const AData: TNyxDataValue): TNyxResourceLabels;
var
  LIndex: Integer;
  LPrevious: Integer;
  LLabel: TNyxResourceLabelRef;
begin
  Result := Default(TNyxResourceLabels);

  if (AData.Kind <> ndArray) or (AData.Count > NyxMaximumResourceLabels) or
    (NyxUTF8ByteCount(AData.ToJSON) > NyxMaximumResourceLabelBytes) then
  begin
    raise ENyxResource.Create('Resource labels exceed their count/data budget or shape');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LLabel := NyxResourceLabel(AData.Item(LIndex).AsText);
    for LPrevious := 0 to LIndex - 1 do
    begin

      if AData.Item(LPrevious).AsText = LLabel.Name then
      begin
        raise ENyxResource.Create('Duplicate resource label');
      end;
    end;
  end;
  Result.FData := AData.Copy;
end;

function TNyxResourceLabels.Copy: TNyxResourceLabels;
begin
  Result := FromData(ToData);
end;

function TNyxResourceLabels.Add(const ALabel: TNyxResourceLabelRef): TNyxResourceLabels;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  NyxResourceLabel(ALabel.Name);

  if Contains(ALabel) then
  begin
    Exit(Copy);
  end;
  SetLength(LItems, Count + 1);
  for LIndex := 0 to Count - 1 do
  begin
    LItems[LIndex] := NyxData(Item(LIndex).Name);
  end;
  LItems[Count] := NyxData(ALabel.Name);
  Result := FromData(NyxArray(LItems));
end;

function TNyxResourceLabels.Remove(const ALabel: TNyxResourceLabelRef): TNyxResourceLabels;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
begin
  SetLength(LItems, Count);
  LCount := 0;
  for LIndex := 0 to Count - 1 do
  begin

    if Item(LIndex).Name <> ALabel.Name then
    begin
      LItems[LCount] := NyxData(Item(LIndex).Name);
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  Result := FromData(NyxArray(LItems));
end;

function NyxResourceDiscovery(const ADefinition: INyxResourceDefinition): INyxResourceDiscovery;
var
  LNormalized: INyxResourceDefinition;
begin

  if ADefinition = nil then
  begin
    raise ENyxResource.Create('Resource discovery requires a definition');
  end;

  if Supports(ADefinition, INyxResourceDiscovery, Result) then
  begin
    Exit;
  end;
  LNormalized := NyxResourceFromData(ADefinition.ToData);

  if not Supports(LNormalized, INyxResourceDiscovery, Result) then
  begin
    raise ENyxResource.Create('Normalized resource has no discovery capability');
  end;
end;

function NyxResourceLabelsOf(const ADefinition: INyxResourceDefinition): TNyxResourceLabels;
var
  LDiscovery: INyxResourceDiscovery;
begin
  Result := NyxResourceLabels;

  if (ADefinition <> nil) and Supports(ADefinition, INyxResourceDiscovery, LDiscovery) then
  begin
    Result := LDiscovery.Labels.Copy;
  end;
end;

function TNyxResourceRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function TNyxLocaleRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NyxResourceKindName(AKind: TNyxResourceKind): TNyxText;
const
  CNames: array[TNyxResourceKind] of TNyxText = ('image', 'json', 'text', 'binary');
begin

  if (Ord(AKind) < Ord(Low(TNyxResourceKind))) or
    (Ord(AKind) > Ord(High(TNyxResourceKind))) then
  begin
    raise ENyxResource.Create('Unknown resource kind');
  end;
  Result := CNames[AKind];
end;

function NewResource(AKind: TNyxResourceKind;
  const AContent: TNyxText): INyxResourceDiscovery;
var
  LResource: TResource;
  LBytes: TNyxBytes;
  LGuard: INyxResourceDiscovery;
begin
  NyxResourceKindName(AKind);
  LResource := TResource.Create;
  LGuard := LResource;
  LResource.FKind := AKind;
  LResource.FContent := AContent;
  case AKind of
    nrkText, nrkJSON:
      begin
        LResource.FByteCount := NyxUTF8ByteCount(AContent);

        if LResource.FByteCount > NyxMaximumPackedBytes then
        begin
          raise ENyxResource.Create('Resource text exceeds 1 MiB');
        end;

        if AKind = nrkJSON then
        begin
          LResource.FData := TNyxDataValue.ParseJSON(AContent);
        end;
      end;
    nrkImage:
      begin
        LResource.FImage := TNyxImageSource.FromWire(AContent);

        if LResource.FImage.Kind <> nisEmbedded then
        begin
          raise ENyxResource.Create('Packed image resources require embedded PNG or JPEG');
        end;
        LResource.FByteCount := Length(LResource.FImage.Bytes);
      end;
    nrkBinary:
      begin
        LBytes := NyxDecodeBase64(AContent);
        LResource.FByteCount := Length(LBytes);
      end;
  end;
  Result := LGuard;
end;

function NyxTextResource(const AText: TNyxText): INyxResourceDiscovery;
begin
  Result := NewResource(nrkText, AText);
end;

function NyxJSONResource(const AText: TNyxText): INyxResourceDiscovery;
begin
  Result := NewResource(nrkJSON, AText);
end;

function NyxJSONResource(const AData: TNyxDataValue): INyxResourceDiscovery;
begin
  Result := NyxJSONResource(AData.ToJSON);
end;

function NyxImageResource(const AImage: TNyxImageSource): INyxResourceDiscovery;
begin
  Result := NewResource(nrkImage, AImage.ToWire);
end;

function NyxBinaryResource(const ABytes: TNyxBytes): INyxResourceDiscovery;
begin
  Result := NewResource(nrkBinary, NyxEncodeBase64(ABytes));
end;

function NyxResourceFromBytes(AKind: TNyxResourceKind;
  const ABytes: TNyxBytes): INyxResourceDefinition;
var
  LFormat: TNyxImageFormat;
begin
  NyxResourceKindName(AKind);

  if Length(ABytes) > NyxMaximumPackedBytes then
  begin
    raise ENyxResource.Create('Resource file exceeds 1 MiB');
  end;
  case AKind of
    nrkText:
      begin
        Result := NyxTextResource(NyxDecodeUTF8(ABytes));
      end;
    nrkJSON:
      begin
        Result := NyxJSONResource(NyxDecodeUTF8(ABytes));
      end;
    nrkImage:
      begin
        LFormat := nimPNG;

        if (Length(ABytes) >= 2) and (ABytes[0] = $FF) and (ABytes[1] = $D8) then
        begin
          LFormat := nimJPEG;
        end;
        Result := NyxImageResource(NyxEmbeddedImageBytes(LFormat, ABytes));
      end;
    nrkBinary:
      begin
        Result := NyxBinaryResource(ABytes);
      end;
  end;
end;

procedure TResource.RequireKind(AKind: TNyxResourceKind);
begin

  if FKind <> AKind then
  begin
    { Name both closed kinds at the failure boundary. This also keeps Studio's
      isolated source diagnostics useful without exposing content or paths. }
    raise ENyxResource.Create('Resource payload is ' + NyxResourceKindName(FKind) +
      TNyxText('; this accessor requires ') + NyxResourceKindName(AKind));
  end;
end;

function NyxHostedResource(AKind: TNyxResourceKind;
  const AURL: TNyxResourceURL): INyxResourceDiscovery;
var
  LResource: TResource;
  LSource: TNyxResourceSource;
begin
  NyxResourceKindName(AKind);
  LSource := NyxHostedResourceSource(AURL);
  LResource := TResource.Create;
  LResource.FKind := AKind;
  LResource.FHosted := True;
  LResource.FSource := LSource;
  Result := LResource;
end;

function TResource.GetSource: TNyxResourceSource;
begin

  if FHosted then
  begin
    Exit(FSource);
  end;
  Result := NyxEmbeddedResourceSource(Bytes);
end;

function TResource.GetFallback: INyxResourceDefinition;
begin
  Result := FFallback;
end;

function TResource.Fallback(const ADefinition: INyxResourceDefinition): INyxResourceDefinition;
var
  LDefinition: INyxResourceDefinition;
  LCopy: INyxResourceDefinition;
  LResource: TResource;
begin

  if not FHosted or (ADefinition = nil) then
  begin
    raise ENyxResource.Create('Fallback requires a hosted resource and embedded definition');
  end;
  LDefinition := NyxResourceFromData(ADefinition.ToData);

  if (LDefinition.Kind <> FKind) or (LDefinition.Source.Kind <> rskEmbedded) then
  begin
    raise ENyxResource.Create('Hosted fallback must be embedded with the same file kind');
  end;
  LCopy := Describe(FTitle, FDescription);
  LResource := LCopy as TResource;
  LResource.FFallback := LDefinition;
  LResource.FByteCount := LDefinition.ByteCount;
  Result := LCopy;
end;

function TResource.Cache(const APolicy: TNyxResourceCachePolicy): INyxResourceDefinition;
var
  LCopy: INyxResourceDefinition;
  LSource: TNyxResourceSource;
  LResource: TResource;
begin

  if not FHosted then
  begin
    raise ENyxResource.Create('Cache policy applies to hosted resources');
  end;
  LSource := FSource.Cache(APolicy);
  LCopy := Describe(FTitle, FDescription);
  LResource := LCopy as TResource;
  LResource.FSource := LSource;
  Result := LCopy;
end;

function TResource.GetKind: TNyxResourceKind;
begin
  Result := FKind;
end;

function TResource.GetTitle: TNyxText;
begin
  Result := FTitle;
end;

function TResource.GetDescription: TNyxText;
begin
  Result := FDescription;
end;

function TResource.GetByteCount: Integer;
begin
  Result := FByteCount;
end;

function TResource.Text: TNyxText;
begin
  RequireKind(nrkText);

  if FHosted then
  begin

    if FFallback = nil then
    begin
      raise ENyxResource.Create('Hosted text must be loaded or have an explicit fallback');
    end;
    Exit(FFallback.Text);
  end;
  Result := FContent;
end;

function TResource.Data: TNyxDataValue;
begin
  RequireKind(nrkJSON);

  if FHosted then
  begin

    if FFallback = nil then
    begin
      raise ENyxResource.Create('Hosted JSON must be loaded or have an explicit fallback');
    end;
    Exit(FFallback.Data);
  end;
  Result := FData.Copy;
end;

function TResource.Image: TNyxImageSource;
begin
  RequireKind(nrkImage);

  if FHosted then
  begin

    if FFallback = nil then
    begin
      raise ENyxResource.Create('Hosted image must be loaded or have an explicit fallback');
    end;
    Exit(FFallback.Image);
  end;
  Result := FImage;
end;

function TResource.Bytes: TNyxBytes;
var
  LImageBytes: TNyxImageBytes;
  LIndex: Integer;
  LBytes: TNyxBytes;
begin

  if FHosted then
  begin

    if FFallback = nil then
    begin
      raise ENyxResource.Create('Hosted bytes must be loaded or have an explicit fallback');
    end;
    Exit(FFallback.Bytes);
  end;
  LBytes := nil;
  case FKind of
    nrkImage:
      begin
        LImageBytes := FImage.Bytes;
        SetLength(LBytes, Length(LImageBytes));
        for LIndex := 0 to High(LImageBytes) do
        begin
          LBytes[LIndex] := LImageBytes[LIndex];
        end;
      end;
    nrkJSON, nrkText:
      begin
        LBytes := NyxEncodeUTF8(FContent);
      end;
    nrkBinary:
      begin
        LBytes := NyxDecodeBase64(FContent);
      end;
  end;
  Result := LBytes;
end;

function TResource.Describe(const ATitle, ADescription: TNyxText): INyxResourceDefinition;
var
  LResource: TResource;
  LGuard: INyxResourceDiscovery;
begin

  if (NyxUTF8ByteCount(ATitle) > 512) or (NyxUTF8ByteCount(ADescription) > 4096) then
  begin
    raise ENyxResource.Create('Resource help exceeds its metadata budget');
  end;
  LResource := TResource.Create;
  LGuard := LResource;
  LResource.FKind := FKind;
  LResource.FContent := FContent;
  LResource.FByteCount := FByteCount;

  if FData.Defined then
  begin
    LResource.FData := FData.Copy;
  end;
  LResource.FImage := FImage;
  LResource.FHosted := FHosted;
  LResource.FSource := FSource;
  LResource.FFallback := FFallback;
  LResource.FLabels := FLabels.Copy;
  LResource.FTitle := ATitle;
  LResource.FDescription := ADescription;
  Result := LGuard;
end;

function TResource.GetLabels: TNyxResourceLabels;
begin
  Result := FLabels.Copy;
end;

function TResource.WithLabels(const ALabels: TNyxResourceLabels): INyxResourceDiscovery;
var
  LCopy: INyxResourceDefinition;
  LResource: TResource;
  LLabels: TNyxResourceLabels;
begin
  { Admit/copy all names before allocating a replacement definition. A refused
    set cannot mutate a subscribed definition or its payload/fallback policy. }
  LLabels := ALabels.Copy;
  LCopy := Describe(FTitle, FDescription);
  LResource := LCopy as TResource;
  LResource.FLabels := LLabels;
  Result := LResource;
end;

function TResource.Tagged(const ALabel: TNyxResourceLabelRef): INyxResourceDiscovery;
begin
  Result := WithLabels(FLabels.Add(ALabel));
end;

{ Unlabelled definitions preserve their exact original version/field order.
  Labels add a strict new version, rather than silently extending old schemas.
  Immutable field copies have no alias back into an admitted caller container. }
function LabelledResourceData(const AData: TNyxDataValue;
  const ALabels: TNyxResourceLabels): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LName: TNyxText;
begin

  if ALabels.Count = 0 then
  begin
    Exit(AData);
  end;
  { Use an explicit integer for the final slot. pas2js does not consistently
    emit a parameterless Count call when it is itself the array subscript. }
  LCount := AData.Count;
  SetLength(LFields, LCount + 1);
  for LIndex := 0 to LCount - 1 do
  begin
    LName := AData.Key(LIndex);

    if LName = 'version' then
    begin
      LFields[LIndex] := NyxField(LName, NyxData(AData.Field(LName).AsInteger + 2));
    end
    else
    begin
      LFields[LIndex] := NyxField(LName, AData.Field(LName).Copy);
    end;
  end;
  LFields[LCount] := NyxField('labels', ALabels.ToData);
  Result := NyxObject(LFields);
end;

function TResource.ToData: TNyxDataValue;
var
  LFallback: TNyxDataValue;
begin

  if FHosted then
  begin
    LFallback := NyxNull;

    if FFallback <> nil then
    begin
      LFallback := FFallback.ToData;
    end;
    Exit(LabelledResourceData(NyxObject([NyxField('version', NyxData(2)),
      NyxField('kind', NyxData(NyxResourceKindName(FKind))),
      NyxField('source', FSource.ToData), NyxField('fallback', LFallback),
      NyxField('title', NyxData(FTitle)), NyxField('description', NyxData(FDescription))]), FLabels));
  end;
  Result := LabelledResourceData(NyxObject([NyxField('version', NyxData(1)),
    NyxField('kind', NyxData(NyxResourceKindName(FKind))),
    NyxField('content', NyxData(FContent)), NyxField('title', NyxData(FTitle)),
    NyxField('description', NyxData(FDescription))]), FLabels);
end;

function NyxResourceFromData(const AData: TNyxDataValue): INyxResourceDefinition;
var
  LKind: TNyxResourceKind;
  LVersion: Integer;
  LSource: TNyxResourceSource;
  LDefinition: INyxResourceDefinition;
  LLabels: TNyxResourceLabels;
begin

  if AData.Kind <> ndObject then
  begin
    raise ENyxResource.Create('Unsupported resource definition');
  end;
  LVersion := AData.Field('version').AsInteger;

  if not (((LVersion = 1) and (AData.Count = 5)) or
    ((LVersion = 2) and (AData.Count = 6)) or
    ((LVersion = 3) and (AData.Count = 6)) or
    ((LVersion = 4) and (AData.Count = 7))) then
  begin
    raise ENyxResource.Create('Unsupported resource definition version or fields');
  end;
  LLabels := NyxResourceLabels;

  if LVersion in [3, 4] then
  begin
    LLabels := TNyxResourceLabels.FromData(AData.Field('labels'));

    if LLabels.Count = 0 then
    begin
      raise ENyxResource.Create('Labelled resource wire requires at least one label');
    end;
  end;
  for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
  begin

    if AData.Field('kind').AsText = NyxResourceKindName(LKind) then
    begin

      if LVersion in [1, 3] then
      begin
        LDefinition := NewResource(LKind, AData.Field('content').AsText);
      end
      else
      begin
        LSource := TNyxResourceSource.FromData(AData.Field('source'));

        if LSource.Kind <> rskHosted then
        begin
          raise ENyxResource.Create('Version-2 resources require a hosted source');
        end;
        LDefinition := NyxHostedResource(LKind, LSource.URL).Cache(LSource.CachePolicy);

        if AData.Field('fallback').Kind <> ndNull then
        begin

          if not (AData.Field('fallback').Field('version').AsInteger in [1, 3]) then
          begin
            raise ENyxResource.Create('Hosted fallback requires an embedded definition');
          end;
          LDefinition := LDefinition.Fallback(NyxResourceFromData(AData.Field('fallback')));
        end;
      end;
      LDefinition := LDefinition.Describe(AData.Field('title').AsText, AData.Field('description').AsText);

      if LLabels.Count > 0 then
      begin
        LDefinition := NyxResourceDiscovery(LDefinition).WithLabels(LLabels);
      end;
      Result := LDefinition;
      Exit;
    end;
  end;
  raise ENyxResource.Create('Unknown resource payload kind');
end;

function TResources.IndexOf(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): Integer;
var
  LIndex: Integer;
begin
  Name(AReference.Name);
  for LIndex := 0 to High(FEntries) do
  begin

    if (FEntries[LIndex].Reference.Name = AReference.Name) and
      (FEntries[LIndex].Locale.Name = ALocale.Name) then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TResources.Define(const AReference: TNyxResourceRef;
  const ADefinition: INyxResourceDefinition): INyxResources;
begin
  Result := Define(AReference, NyxDefaultLocale, ADefinition);
end;

function TResources.Define(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef; const ADefinition: INyxResourceDefinition): INyxResources;
var
  LDefinition: INyxResourceDefinition;
  LIndex: Integer;
  LTarget: Integer;
  LBytes: Integer;
begin

  if ADefinition = nil then
  begin
    raise ENyxResource.Create('Resource definition is required');
  end;
  LTarget := IndexOf(AReference, ALocale);
  LDefinition := NyxResourceFromData(ADefinition.ToData);

  if (LTarget < 0) and (Length(FEntries) >= NyxMaximumResources) then
  begin
    raise ENyxResource.Create('Resource catalog exceeds 128 entries');
  end;
  LBytes := NyxUTF8ByteCount(LDefinition.ToData.ToJSON) +
    NyxUTF8ByteCount(AReference.Name) + NyxUTF8ByteCount(ALocale.Name);
  for LIndex := 0 to High(FEntries) do
  begin

    if LIndex <> LTarget then
    begin
      Inc(LBytes, NyxUTF8ByteCount(FEntries[LIndex].Definition.ToData.ToJSON) +
        NyxUTF8ByteCount(FEntries[LIndex].Reference.Name) +
        NyxUTF8ByteCount(FEntries[LIndex].Locale.Name));
    end;
  end;

  if LBytes > NyxMaximumResourceWireBytes then
  begin
    raise ENyxResource.Create('Resource catalog exceeds its packed wire budget');
  end;

  if LTarget < 0 then
  begin
    LTarget := Length(FEntries);
    SetLength(FEntries, LTarget + 1);
    FEntries[LTarget] := TResourceEntry.Create;
  end;
  FEntries[LTarget].Reference := AReference;
  FEntries[LTarget].Locale := ALocale;
  FEntries[LTarget].Definition := LDefinition;
  Result := Self;
end;

function TResources.Remove(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): INyxResources;
var
  LIndex: Integer;
  LTarget: Integer;
begin
  LTarget := IndexOf(AReference, ALocale);

  if LTarget < 0 then
  begin
    raise ENyxResource.Create('Unknown resource variant');
  end;
  FEntries[LTarget].Free;
  for LIndex := LTarget to High(FEntries) - 1 do
  begin
    FEntries[LIndex] := FEntries[LIndex + 1];
  end;
  SetLength(FEntries, Length(FEntries) - 1);
  Result := Self;
end;

function TResources.Contains(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): Boolean;
begin
  Result := IndexOf(AReference, ALocale) >= 0;
end;

function TResources.Definition(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): INyxResourceDefinition;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference, ALocale);

  if LIndex < 0 then
  begin
    raise ENyxResource.Create('Unknown resource variant');
  end;
  Result := FEntries[LIndex].Definition;
end;

function TResources.Resolve(const AReference: TNyxResourceRef;
  const ALocale, AFallback: TNyxLocaleRef): INyxResourceDefinition;
begin

  if Contains(AReference, ALocale) then
  begin
    Exit(Definition(AReference, ALocale));
  end;

  if AFallback.Defined and Contains(AReference, AFallback) then
  begin
    Exit(Definition(AReference, AFallback));
  end;
  Result := Definition(AReference, NyxDefaultLocale);
end;

procedure TResources.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Length(FEntries)) then
  begin
    raise ENyxResource.Create('Resource index is out of range');
  end;
end;

function TResources.Reference(AIndex: Integer): TNyxResourceRef;
begin
  CheckIndex(AIndex);
  Result := FEntries[AIndex].Reference;
end;

function TResources.Locale(AIndex: Integer): TNyxLocaleRef;
begin
  CheckIndex(AIndex);
  Result := FEntries[AIndex].Locale;
end;

function TResources.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function NewNyxResources: INyxResources;
begin
  Result := TResources.Create;
end;

destructor TResources.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin
    FEntries[LIndex].Free;
  end;
  inherited Destroy;
end;

function TResources.Clone: INyxResources;
var
  LCopy: TResources;
  LIndex: Integer;
begin
  LCopy := TResources.Create;
  Result := LCopy;
  SetLength(LCopy.FEntries, Length(FEntries));
  for LIndex := 0 to High(FEntries) do
  begin
    LCopy.FEntries[LIndex] := TResourceEntry.Create;
    LCopy.FEntries[LIndex].Reference := FEntries[LIndex].Reference;
    LCopy.FEntries[LIndex].Locale := FEntries[LIndex].Locale;
    LCopy.FEntries[LIndex].Definition := FEntries[LIndex].Definition;
  end;
end;

function TResources.ToData: TNyxDataValue;
var
  LEntries: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LEntries, Length(FEntries));
  for LIndex := 0 to High(FEntries) do
  begin
    LEntries[LIndex] := NyxObject([
      NyxField('name', NyxData(FEntries[LIndex].Reference.Name)),
      NyxField('locale', NyxData(FEntries[LIndex].Locale.Name)),
      NyxField('definition', FEntries[LIndex].Definition.ToData)]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('entries', NyxArray(LEntries))]);
end;

function NyxResourcesFromData(const AData: TNyxDataValue): INyxResources;
var
  LResources: INyxResources;
  LEntries: TNyxDataValue;
  LEntry: TNyxDataValue;
  LReference: TNyxResourceRef;
  LLocale: TNyxLocaleRef;
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 2) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxResource.Create('Unsupported resource catalog');
  end;
  LEntries := AData.Field('entries');

  if (LEntries.Kind <> ndArray) or (LEntries.Count > NyxMaximumResources) then
  begin
    raise ENyxResource.Create('Resource catalog requires a bounded entry array');
  end;
  LResources := NewNyxResources;
  for LIndex := 0 to LEntries.Count - 1 do
  begin
    LEntry := LEntries.Item(LIndex);

    if (LEntry.Kind <> ndObject) or (LEntry.Count <> 3) then
    begin
      raise ENyxResource.Create('Resource entry requires exact name/locale/definition');
    end;
    LReference := NyxResourceRef(LEntry.Field('name').AsText);
    LLocale := NyxDefaultLocale;

    if LEntry.Field('locale').AsText <> '' then
    begin
      LLocale := NyxLocale(LEntry.Field('locale').AsText);
    end;

    if LResources.Contains(LReference, LLocale) then
    begin
      raise ENyxResource.Create('Duplicate resource variant');
    end;
    LResources.Define(LReference, LLocale, NyxResourceFromData(LEntry.Field('definition')));
  end;
  Result := LResources;
end;

function NyxResourcePath: TNyxResourcePath;
begin
  Result := Default(TNyxResourcePath);
end;

function TNyxResourcePath.ToData: TNyxDataValue;
begin
  Result := FSteps;

  if FSteps.Kind = ndNull then
  begin
    Result := NyxArray([]);
  end;
end;

class function TNyxResourcePath.FromData(const AData: TNyxDataValue): TNyxResourcePath;
var
  LIndex: Integer;
  LStep: TNyxDataValue;
  LPath: TNyxResourcePath;
begin
  LPath := NyxResourcePath;

  if (AData.Kind <> ndArray) or (AData.Count > 32) then
  begin
    raise ENyxResource.Create('Resource selector requires at most 32 typed steps');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LStep := AData.Item(LIndex);

    if LStep.Kind = ndText then
    begin
      LPath := LPath.Field(LStep.AsText);
    end
    else if LStep.Kind = ndNumber then
    begin
      LPath := LPath.Item(LStep.AsInteger);
    end
    else
    begin
      raise ENyxResource.Create('Resource selector requires field names or integer items');
    end;
  end;
  Result := LPath;
end;

function Append(const APath: TNyxResourcePath;
  const AStep: TNyxDataValue): TNyxResourcePath;
var
  LSteps: array of TNyxDataValue;
  LIndex: Integer;
  LData: TNyxDataValue;
  LCount: Integer;
begin
  LData := APath.ToData;
  LCount := LData.Count;

  if LCount >= 32 then
  begin
    raise ENyxResource.Create('Resource selector exceeds 32 steps');
  end;
  { Materialize the length before indexing. The supported pas2js compiler can
    emit a parameterless record method as a function reference in this index,
    even though it calls that method correctly in an arithmetic expression. }
  SetLength(LSteps, LCount + 1);
  for LIndex := 0 to LCount - 1 do
  begin
    LSteps[LIndex] := LData.Item(LIndex);
  end;
  LSteps[LCount] := AStep;
  Result.FSteps := NyxArray(LSteps);
end;

function TNyxResourcePath.Field(const AName: TNyxText): TNyxResourcePath;
begin
  NyxUTF8ByteCount(AName);
  Result := Append(Self, NyxData(AName));
end;

function TNyxResourcePath.Item(AIndex: Integer): TNyxResourcePath;
begin

  if AIndex < 0 then
  begin
    raise ENyxResource.Create('Resource item indices are nonnegative');
  end;
  Result := Append(Self, NyxData(AIndex));
end;

function TNyxResourcePath.Copy: TNyxResourcePath;
begin
  Result := Default(TNyxResourcePath);

  if FSteps.Defined then
  begin
    Result.FSteps := FSteps.Copy;
  end;
end;

function TNyxResourcePath.Select(const AData: TNyxDataValue): TNyxDataValue;
var
  LIndex: Integer;
  LData: TNyxDataValue;
  LSteps: TNyxDataValue;
  LStep: TNyxDataValue;
begin
  LData := AData.Copy;
  LSteps := ToData;
  for LIndex := 0 to LSteps.Count - 1 do
  begin
    LStep := LSteps.Item(LIndex);

    if LStep.Kind = ndText then
    begin
      LData := LData.Field(LStep.AsText);
    end
    else
    begin
      LData := LData.Item(LStep.AsInteger);
    end;
  end;
  Result := LData;
end;

function NyxResourceValue(const AReference: TNyxResourceRef): TNyxResourceValueRef;
begin
  Result := Default(TNyxResourceValueRef);
  Result.FReference := NyxResourceRef(AReference.Name);
  Result.FKind := nskText;
end;

function TNyxResourceValueRef.Copy: TNyxResourceValueRef;
begin
  Result.FReference := FReference;
  Result.FPath := FPath.Copy;
  Result.FLocale := FLocale;
  Result.FFallback := FFallback;
  Result.FKind := FKind;
  Result.FLocalized := FLocalized;
end;

function TNyxResourceValueRef.Field(const AName: TNyxText): TNyxResourceValueRef;
begin
  Result := Copy;
  Result.FPath := FPath.Field(AName);
end;

function TNyxResourceValueRef.Item(AIndex: Integer): TNyxResourceValueRef;
begin
  Result := Copy;
  Result.FPath := FPath.Item(AIndex);
end;

function TNyxResourceValueRef.Localize(const ALocale, AFallback: TNyxLocaleRef): TNyxResourceValueRef;
begin
  Result := Copy;
  Result.FLocale := ALocale;
  Result.FFallback := AFallback;
  Result.FLocalized := True;
end;

function TNyxResourceValueRef.AsText: TNyxResourceValueRef;
begin
  Result := Copy;
  Result.FKind := nskText;
end;

function TNyxResourceValueRef.AsBoolean: TNyxResourceValueRef;
begin
  Result := Copy;
  Result.FKind := nskBoolean;
end;

function TNyxResourceValueRef.AsInteger: TNyxResourceValueRef;
begin
  Result := Copy;
  Result.FKind := nskInteger;
end;

function TNyxResourceValueRef.AsNumber: TNyxResourceValueRef;
begin
  Result := Copy;
  Result.FKind := nskNumber;
end;

function TNyxResourceValueRef.Read(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef): TNyxStateValue;
var
  LDefinition: INyxResourceDefinition;
  LLocale: TNyxLocaleRef;
  LFallback: TNyxLocaleRef;
  LValue: TNyxDataValue;
begin

  if AResources = nil then
  begin
    raise ENyxResource.Create('Resource binding requires an admitted catalog');
  end;
  LLocale := ALocale;
  LFallback := AFallback;

  if FLocalized then
  begin
    LLocale := FLocale;
    LFallback := FFallback;
  end;
  LDefinition := AResources.Resolve(FReference, LLocale, LFallback);

  if LDefinition.Kind = nrkText then
  begin

    if (FPath.ToData.Count <> 0) or (FKind <> nskText) then
    begin
      raise ENyxResource.Create('A text file requires a root text selector');
    end;
    Exit(TNyxStateValue.FromText(LDefinition.Text));
  end;
  LValue := FPath.Select(LDefinition.Data);
  case FKind of
    nskText:
      begin
        Result := TNyxStateValue.FromText(LValue.AsText);
      end;
    nskBoolean:
      begin
        Result := TNyxStateValue.FromBoolean(LValue.AsBoolean);
      end;
    nskInteger:
      begin
        Result := TNyxStateValue.FromInteger(LValue.AsInteger);
      end;
    nskNumber:
      begin
        Result := TNyxStateValue.FromNumber(LValue.AsNumber);
      end;
  end;
end;

function TNyxResourceValueRef.ToData: TNyxDataValue;
begin
  NyxResourceRef(FReference.Name);
  { Keep historical five-field packets byte-stable where they already express
    the same meaning. Only an explicit default pin needs the sixth discriminator.
    An older unlocalized fallback is retained as inert historical metadata. }

  if FLocalized and not FLocale.Defined then
  begin
    Exit(NyxObject([
      NyxField('resource', NyxData(FReference.Name)),
      NyxField('path', FPath.ToData),
      NyxField('locale', NyxData(FLocale.Name)),
      NyxField('fallback', NyxData(FFallback.Name)),
      NyxField('type', NyxData(NyxStateKindName(FKind))),
      NyxField('localized', NyxData(True))]));
  end;
  Result := NyxObject([
    NyxField('resource', NyxData(FReference.Name)),
    NyxField('path', FPath.ToData),
    NyxField('locale', NyxData(FLocale.Name)),
    NyxField('fallback', NyxData(FFallback.Name)),
    NyxField('type', NyxData(NyxStateKindName(FKind)))]);
end;

class function TNyxResourceValueRef.FromData(const AData: TNyxDataValue): TNyxResourceValueRef;
var
  LValue: TNyxResourceValueRef;
  LKind: TNyxStateKind;
  LFound: Boolean;
  LName: TNyxText;
begin

  if (AData.Kind <> ndObject) or not (AData.Count in [5, 6]) then
  begin
    raise ENyxResource.Create('Resource selector requires its exact locale contract');
  end;
  LValue := NyxResourceValue(NyxResourceRef(AData.Field('resource').AsText));
  LValue.FPath := TNyxResourcePath.FromData(AData.Field('path'));
  LName := AData.Field('locale').AsText;

  if LName <> '' then
  begin
    LValue.FLocale := NyxLocale(LName);
  end;
  LValue.FLocalized := LValue.FLocale.Defined;

  if AData.Count = 6 then
  begin
    { Five fields historically inherit when locale is empty. The new field is
      exclusively an explicit default pin, never a disguised nondefault packet. }

    if not AData.Field('localized').AsBoolean or LValue.FLocale.Defined then
    begin
      raise ENyxResource.Create('Explicit scalar default pin requires localized=True and empty locale');
    end;
    LValue.FLocalized := True;
  end;
  LName := AData.Field('fallback').AsText;

  if LName <> '' then
  begin
    LValue.FFallback := NyxLocale(LName);
  end;
  LFound := False;
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if NyxStateKindName(LKind) = AData.Field('type').AsText then
    begin
      LFound := True;
      LValue.FKind := LKind;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxResource.Create('Unknown resource scalar type');
  end;
  Result := LValue;
end;

function NyxResourceImage(const AReference: TNyxResourceRef): TNyxResourceImageRef;
begin
  Result := Default(TNyxResourceImageRef);
  Result.FReference := NyxResourceRef(AReference.Name);
end;

function TNyxResourceImageRef.Localize(const ALocale,
  AFallback: TNyxLocaleRef): TNyxResourceImageRef;
begin
  Result := Self;
  Result.FLocale := ALocale;
  Result.FFallback := AFallback;
  Result.FLocalized := True;
end;

function TNyxResourceImageRef.Read(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef): TNyxImageSource;
var
  LLocale: TNyxLocaleRef;
  LFallback: TNyxLocaleRef;
begin

  if AResources = nil then
  begin
    raise ENyxResource.Create('Image binding requires an admitted resource catalog');
  end;
  LLocale := ALocale;
  LFallback := AFallback;

  if FLocalized then
  begin
    LLocale := FLocale;
    LFallback := FFallback;
  end;
  { Definition.Image enforces image kind and synchronous fallback availability.
    The returned immutable source owns its wire/validation policy independently. }
  Result := AResources.Resolve(FReference, LLocale, LFallback).Image;
end;

function TNyxResourceImageRef.ToData: TNyxDataValue;
begin
  NyxResourceRef(FReference.Name);
  Result := NyxObject([NyxField('resource', NyxData(FReference.Name)),
    NyxField('locale', NyxData(FLocale.Name)),
    NyxField('fallback', NyxData(FFallback.Name)),
    NyxField('localized', NyxData(FLocalized))]);
end;

class function TNyxResourceImageRef.FromData(
  const AData: TNyxDataValue): TNyxResourceImageRef;
var
  LResult: TNyxResourceImageRef;
  LName: TNyxText;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 4) then
  begin
    raise ENyxResource.Create('Image resource selector requires four exact fields');
  end;
  LResult := NyxResourceImage(NyxResourceRef(AData.Field('resource').AsText));
  LResult.FLocalized := AData.Field('localized').AsBoolean;
  LName := AData.Field('locale').AsText;

  if LName <> '' then
  begin
    LResult.FLocale := NyxLocale(LName);
  end;
  LName := AData.Field('fallback').AsText;

  if LName <> '' then
  begin
    LResult.FFallback := NyxLocale(LName);
  end;

  if not LResult.FLocalized and (LResult.FLocale.Defined or LResult.FFallback.Defined) then
  begin
    raise ENyxResource.Create('Inherited image locale cannot carry fixed locale names');
  end;
  Result := LResult;
end;

end.
