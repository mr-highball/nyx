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
program nyx_resource_foundation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.images, nyx.resources,
  nyx.resource.sources, nyx.resource.context, nyx.model, nyx.controls, nyx.codec
  {$ifdef PAS2JS}, Web{$endif};

const
  { Existing project-owned 100-by-50 PNG, used as exact bytes rather than an
    assertion about pixel decoding. Raster codec qualification has target owners. }
  CPNG: TNyxText = 'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAyEAIAAAB1xzWqAAAACXBIWXMAAAAAAAAAAACdYiYyAAABMElEQVR4nO3OsQ0AIAzAsP7/dOEEtsgSGTxnduf2fbE/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDxwO6T+sr8laFkAAAAABJRU5ErkJggg==';
  CJSON: TNyxText = '{ "name":"Exact 🌙", "large":9007199254740993,' +
    ' "tiny":1.000e-2, "negativeZero":-0, "escaped":"\uD83C\uDF19" }' + #13#10;

type
  { One deliberately mutable extension producer. Its base accessors describe
    the original payload, while ToData supplies the current wire candidate.
    Admission must normalize that candidate rather than retain this producer;
    replacing Wire afterwards cannot change accepted registry membership/data. }
  TSuppliedResource = class(TInterfacedObject, INyxResourceDefinition)
  private
    FDefinition: INyxResourceDefinition;
    FWire: TNyxDataValue;
  public
    constructor Create(const ADefinition: INyxResourceDefinition);
    procedure Supply(const AWire: TNyxDataValue);
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
  end;

var
  GChecks: Integer;
  GPhase: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Resource foundation: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TSuppliedResource.Create(const ADefinition: INyxResourceDefinition);
begin
  inherited Create;
  FDefinition := ADefinition;
  FWire := ADefinition.ToData;
end;

procedure TSuppliedResource.Supply(const AWire: TNyxDataValue);
begin
  FWire := AWire.Copy;
end;

function TSuppliedResource.GetKind: TNyxResourceKind;
begin
  Result := FDefinition.Kind;
end;

function TSuppliedResource.GetTitle: TNyxText;
begin
  Result := FDefinition.Title;
end;

function TSuppliedResource.GetDescription: TNyxText;
begin
  Result := FDefinition.Description;
end;

function TSuppliedResource.GetByteCount: Integer;
begin
  Result := FDefinition.ByteCount;
end;

function TSuppliedResource.GetSource: TNyxResourceSource;
begin
  Result := FDefinition.Source;
end;

function TSuppliedResource.GetFallback: INyxResourceDefinition;
begin
  Result := FDefinition.FallbackDefinition;
end;

function TSuppliedResource.Bytes: TNyxBytes;
begin
  Result := FDefinition.Bytes;
end;

function TSuppliedResource.Text: TNyxText;
begin
  Result := FDefinition.Text;
end;

function TSuppliedResource.Data: TNyxDataValue;
begin
  Result := FDefinition.Data;
end;

function TSuppliedResource.Image: TNyxImageSource;
begin
  Result := FDefinition.Image;
end;

function TSuppliedResource.Describe(const ATitle, ADescription: TNyxText): INyxResourceDefinition;
begin
  Result := FDefinition.Describe(ATitle, ADescription);
end;

function TSuppliedResource.Fallback(const ADefinition: INyxResourceDefinition): INyxResourceDefinition;
begin
  Result := FDefinition.Fallback(ADefinition);
end;

function TSuppliedResource.Cache(const APolicy: TNyxResourceCachePolicy): INyxResourceDefinition;
begin
  Result := FDefinition.Cache(APolicy);
end;

function TSuppliedResource.ToData: TNyxDataValue;
begin
  Result := FWire.Copy;
end;

{ Literal test-wire substitution, deliberately confined to the persistence
  boundary. It preserves every other member and its insertion order. }
function ReplaceField(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AData.Count);
  for LIndex := 0 to High(LFields) do
  begin

    if AData.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end
    else
    begin
      LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));
    end;
  end;
  Result := NyxObject(LFields);
end;

function RepeatText(const AText: TNyxText; ACount: Integer): TNyxText;
var
  LPart: TNyxText;
begin
  Result := '';
  LPart := AText;
  while ACount > 0 do
  begin

    if ACount mod 2 = 1 then
    begin
      Result := Result + LPart;
    end;
    ACount := ACount div 2;

    if ACount > 0 then
    begin
      LPart := LPart + LPart;
    end;
  end;
end;

{ Failed replacement reads exact catalog bytes before and after; merely seeing
  an exception would not establish absence of partial publication. }
procedure RejectDefinition(const AResources: INyxResources;
  const AData: TNyxDataValue; const AReason: TNyxText);
var
  LBefore: TNyxText;
  LRejected: Boolean;
  LProducer: TSuppliedResource;
  LGuard: INyxResourceDefinition;
begin
  LBefore := AResources.ToData.ToJSON;
  LProducer := TSuppliedResource.Create(NyxTextResource('Producer baseline'));
  LGuard := LProducer;
  LProducer.Supply(AData);
  LRejected := False;
  try
    AResources.Define(NyxResourceRef('copy'), LGuard);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason + TNyxText(' refuses'));
  Check(AResources.ToData.ToJSON = LBefore, AReason + TNyxText(' preserves every accepted entry'));
end;

procedure RejectImport(const AResources: INyxResources; AKind: TNyxResourceKind;
  const ABytes: TNyxBytes; const AReason: TNyxText);
var
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LBefore := AResources.ToData.ToJSON;
  LRejected := False;
  try
    AResources.Define(NyxResourceRef('copy'), NyxResourceFromBytes(AKind, ABytes));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason + TNyxText(' refuses'));
  Check(AResources.ToData.ToJSON = LBefore, AReason + TNyxText(' preserves every accepted entry'));
end;

procedure ExactOwnership;
var
  LDocument: TNyxDocument;
  LClone: TNyxDocument;
  LResources: INyxResources;
  LSnapshot: INyxResources;
  LContext: INyxResourceContext;
  LRetained: INyxResourceDefinition;
  LOriginal: INyxResourceDefinition;
  LDerived: INyxResourceDefinition;
  LProducer: TSuppliedResource;
  LProducerGuard: INyxResourceDefinition;
  LBytes: TNyxBytes;
  LPayload: TNyxBytes;
  LData: TNyxDataValue;
  LWire: TNyxText;
  LName: TNyxResourceRef;
  LImage: TNyxImageSource;
begin
  LDocument := TNyxDocument.Create;
  LClone := nil;
  try
    LDocument.AddPage(NewNyxColumn('foundation-home'));
    LName := NyxResourceRef('copy 🌙');
    LOriginal := NyxJSONResource(CJSON).Tagged(NyxResourceLabel('Copy, exact'))
      .Describe('Project 🌙', 'Exact Unicode help' + TNyxText(#0) + ' / 🌙');
    LDerived := LOriginal.Describe('Different help', 'Independent metadata');
    Check(LOriginal.Title = TNyxText('Project 🌙'), 'Describe leaves original metadata unchanged');
    Check(NyxDecodeUTF8(LDerived.Bytes) = CJSON, 'Describe preserves original JSON file bytes');
    LDocument.Resources.Define(LName, LOriginal);
    LPayload := NyxDecodeBase64('AAH/');
    LDocument.Resources.Define(NyxResourceRef('bytes'), NyxBinaryResource(LPayload));
    LPayload[0] := 44;
    Check(NyxEncodeBase64(LDocument.Resources.Definition(NyxResourceRef('bytes'),
      NyxDefaultLocale).Bytes) = 'AAH/', 'later caller byte edits cannot alter admitted content');
    LDocument.Resources.Define(NyxResourceRef('notes'),
      NyxTextResource('Line one' + TNyxText(#13#10#0) + 'Line two 🌙'));
    LDocument.Resources.Define(NyxResourceRef('image'), NyxResourceFromBytes(nrkImage,
      NyxDecodeBase64(CPNG)));
    LDocument.Resources.Define(NyxResourceRef('remote'),
      NyxHostedResource(nrkJSON, NyxResourceURL('https://example.test/copy.json?lang=en'))
        .Tagged(NyxResourceLabel('Remote')).Fallback(LOriginal)
        .Cache(NyxResourceCache.Persistent.FreshFor(60).ServerPolicy(rcspOverride)));
    LRetained := LDocument.Resources.Definition(LName, NyxDefaultLocale);
    LData := LRetained.Data;
    LResources := LDocument.Resources.Clone;
    LContext := NewNyxResourceContext(LDocument.Resources, NyxLocale('en-US'), NyxDefaultLocale);
    LSnapshot := LContext.Snapshot;
    LClone := LDocument.Clone;
    LWire := LResources.ToData.ToJSON;
    LDocument.Free;
    LDocument := nil;
    Check(NyxDecodeUTF8(LRetained.Bytes) = CJSON, 'retained definition outlives its document exactly');
    Check(LData.Field('large').AsDecimal.Text = '9007199254740993',
      'retained data preserves a decimal beyond Double integer precision');
    Check(LData.Field('tiny').AsDecimal.Text = '1.000e-2', 'exponent and trailing zeros remain exact');
    Check(LData.Field('negativeZero').AsDecimal.Text = '-0', 'negative zero spelling remains exact');
    Check(LData.Field('escaped').AsText = TNyxText('🌙'), 'escaped supplementary Unicode decodes exactly');
    Check(NyxResourceLabelsOf(LRetained).Item(0).Name = 'Copy, exact',
      'retained discovery metadata does not split punctuation');
    Check((LResources.ToData.ToJSON = LWire) and (LSnapshot.ToData.ToJSON = LWire),
      'catalog clones and context snapshots outlive the document');
    Check((LContext.Locale.Name = 'en-US') and not LContext.Fallback.Defined,
      'retained context keeps explicit independent locale intent');
    LResources.Remove(LName, NyxDefaultLocale);
    LSnapshot.Define(LName, NyxJSONResource('{"name":"Changed snapshot"}'));
    Check(LContext.Snapshot.ToData.ToJSON = LWire, 'snapshot edits cannot mutate the immutable frame');
    Check(LClone.Resources.ToData.ToJSON = LWire, 'catalog and snapshot edits cannot mutate a document clone');
    LClone.Free;
    LClone := nil;
    Check(LRetained.Title = TNyxText('Project 🌙'), 'metadata remains safe after all documents retire');
    Check(LRetained.Description = TNyxText('Exact Unicode help') + TNyxText(#0) + TNyxText(' / 🌙'),
      'creator description retains embedded NUL and supplementary Unicode');
    Check(LContext.Snapshot.Definition(NyxResourceRef('notes'), NyxDefaultLocale).Text =
      TNyxText('Line one') + TNyxText(#13#10#0) + TNyxText('Line two 🌙'),
      'text files retain exact line endings and NUL');
    LImage := LContext.Snapshot.Definition(NyxResourceRef('image'), NyxDefaultLocale).Image;
    Check((LImage.Width = 100) and (LImage.Height = 50) and
      (NyxEncodeBase64(LImage.Bytes) = CPNG), 'retained raster source keeps exact packed bytes and dimensions');
    LDerived := LContext.Snapshot.Definition(NyxResourceRef('remote'), NyxDefaultLocale);
    Check((LDerived.Source.URL.Address = 'https://example.test/copy.json?lang=en') and
      (LDerived.Source.CachePolicy.Server = rcspOverride) and
      (NyxDecodeUTF8(LDerived.Bytes) = CJSON), 'retained hosted declaration keeps policy and independent fallback');
    LBytes := LRetained.Bytes;
    LBytes[0] := 0;
    Check(NyxDecodeUTF8(LRetained.Bytes) = CJSON, 'retained Bytes does not expose mutable resource memory');
    LProducer := TSuppliedResource.Create(NyxTextResource('Original extension'));
    LProducerGuard := LProducer;
    LProducer.Supply(NyxTextResource('Admitted extension').ToData);
    LResources.Define(NyxResourceRef('extension'), LProducerGuard);
    LProducer.Supply(NyxTextResource('Later producer change').ToData);
    LProducerGuard := nil;
    Check(LResources.Definition(NyxResourceRef('extension'), NyxDefaultLocale).Text =
      'Admitted extension', 'foreign implementation is normalized once without retaining its producer');
  finally
    LClone.Free;
    LDocument.Free;
  end;
end;

procedure AtomicAdmission;
var
  LResources: INyxResources;
  LDefinition: TNyxDataValue;
  LWire: TNyxDataValue;
  LEntries: array of TNyxDataValue;
  LBytes: TNyxBytes;
  LBefore: TNyxText;
  LRejected: Boolean;
  LKind: TNyxResourceKind;
  LIndex: Integer;
begin
  LResources := NewNyxResources;
  LResources.Define(NyxResourceRef('copy'), NyxTextResource('Accepted baseline'))
    .Define(NyxResourceRef('other'), NyxJSONResource('{"count":3}'));
  LDefinition := LResources.Definition(NyxResourceRef('copy'), NyxDefaultLocale).ToData;
  RejectDefinition(LResources, ReplaceField(LDefinition, 'version', NyxData(99)), 'unknown version');
  RejectDefinition(LResources, ReplaceField(LDefinition, 'kind', NyxData('script')), 'unknown payload family');
  RejectDefinition(LResources, ReplaceField(LDefinition, 'title', NyxData(12)), 'wrong metadata type');
  RejectDefinition(LResources, ReplaceField(LDefinition, 'title', NyxData(RepeatText('🌙', 129))),
    'UTF-8 title byte budget');
  RejectDefinition(LResources, ReplaceField(LDefinition, 'description', NyxData(RepeatText('🌙', 1025))),
    'UTF-8 help byte budget');
  RejectDefinition(LResources, ReplaceField(LDefinition, 'content', NyxData(RepeatText('x',
    NyxMaximumPackedBytes + 1))), 'definition payload budget');
  RejectDefinition(LResources, NyxObject([
    NyxField('version', NyxData(1)), NyxField('kind', NyxData('text')),
    NyxField('content', NyxData('Replacement')), NyxField('title', NyxData('Help')),
    NyxField('unknown', NyxData('Missing description'))]), 'unknown field substituting a required member');
  RejectDefinition(LResources, ReplaceField(NyxBinaryResource(NyxDecodeBase64('AAH/')).ToData,
    'content', NyxData('AB==')), 'noncanonical Base64 pad bits');
  RejectDefinition(LResources, ReplaceField(NyxHostedResource(nrkJSON,
    NyxResourceURL('https://example.test/data')).ToData, 'fallback', NyxTextResource('Wrong kind').ToData),
    'conflicting fallback family');
  RejectImport(LResources, nrkText, NyxDecodeBase64('7aCA'), 'UTF-8 encoded surrogate');
  RejectImport(LResources, nrkText, NyxDecodeBase64('wK8='), 'overlong UTF-8');
  RejectImport(LResources, nrkText, NyxDecodeBase64('8J8='), 'truncated UTF-8');
  RejectImport(LResources, nrkJSON, NyxEncodeUTF8('{"broken":'), 'incomplete JSON');
  RejectImport(LResources, nrkJSON, NyxEncodeUTF8('{"name":1,"\u006eame":2}'),
    'conflicting decoded JSON member');
  RejectImport(LResources, nrkImage, NyxDecodeBase64('R0lGODlh'), 'unsupported raster format');
  LBytes := NyxDecodeBase64(CPNG);
  LBytes[29] := LBytes[29] xor 1;
  RejectImport(LResources, nrkImage, LBytes, 'damaged PNG checksum');
  SetLength(LBytes, NyxMaximumPackedBytes + 1);
  for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
  begin
    RejectImport(LResources, LKind, LBytes, 'oversized ' + NyxResourceKindName(LKind) + TNyxText(' file'));
  end;
  LBefore := LResources.ToData.ToJSON;
  LRejected := False;
  try
    LResources.Define(NyxResourceRef('copy'), nil);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResources.ToData.ToJSON = LBefore), 'nil definition refuses complete replacement');
  LWire := LResources.ToData;
  SetLength(LEntries, 3);
  for LIndex := 0 to 1 do
  begin
    LEntries[LIndex] := LWire.Field('entries').Item(LIndex);
  end;
  LEntries[2] := LEntries[0].Copy;
  LRejected := False;
  try
    LResources := NyxResourcesFromData(ReplaceField(LWire, 'entries', NyxArray(LEntries)));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResources.ToData.ToJSON = LBefore),
    'late duplicate variant refuses before returning a candidate catalog');
  LEntries[0] := ReplaceField(LEntries[0], 'name', NyxData('New first entry'));
  LEntries[2] := ReplaceField(ReplaceField(LEntries[1], 'name', NyxData('Invalid last entry')), 'definition',
    ReplaceField(LEntries[1].Field('definition'), 'content', NyxData('{"bad":')));
  LRejected := False;
  try
    LResources := NyxResourcesFromData(ReplaceField(LWire, 'entries', NyxArray(LEntries)));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResources.ToData.ToJSON = LBefore) and
    not LResources.Contains(NyxResourceRef('New first entry'), NyxDefaultLocale),
    'late malformed entry cannot publish an earlier valid prefix');
end;

{ File and registry capacities are distinct. A permitted replacement at a full
  registry must retain order; a failed larger replacement must retain every byte. }
procedure CapacityBoundaries;
var
  LResources: INyxResources;
  LDefinition: INyxResourceDefinition;
  LBefore: TNyxText;
  LBytes: TNyxBytes;
  LIndex: Integer;
  LRejected: Boolean;
begin
  LDefinition := NyxTextResource(RepeatText('🌙', NyxMaximumPackedBytes div 4));
  Check(LDefinition.ByteCount = NyxMaximumPackedBytes, 'text admits exactly one MiB of UTF-8');
  LDefinition := NyxJSONResource('"' + RepeatText('x', NyxMaximumPackedBytes - 2) + '"');
  Check(LDefinition.ByteCount = NyxMaximumPackedBytes, 'JSON admits the exact file-byte boundary');
  SetLength(LBytes, NyxMaximumPackedBytes);
  for LIndex := 0 to High(LBytes) do
  begin
    LBytes[LIndex] := LIndex mod 256;
  end;
  LDefinition := NyxBinaryResource(LBytes);
  Check((LDefinition.ByteCount = NyxMaximumPackedBytes) and
    (NyxEncodeBase64(LDefinition.Bytes) = NyxEncodeBase64(LBytes)),
    'binary admits the exact boundary without losing zero or high bytes');
  LDefinition := NyxTextResource('Metadata').Describe(RepeatText('🌙', 128), RepeatText('🌙', 1024));
  Check((NyxUTF8ByteCount(LDefinition.Title) = 512) and
    (NyxUTF8ByteCount(LDefinition.Description) = 4096), 'creator metadata admits its exact UTF-8 limits');
  LResources := NewNyxResources;
  for LIndex := 1 to NyxMaximumResources do
  begin
    LResources.Define(NyxResourceRef('File ' + TNyxText(IntToStr(LIndex))), NyxTextResource('Small'));
  end;
  LBefore := LResources.ToData.ToJSON;
  LRejected := False;
  try
    LResources.Define(NyxResourceRef('Excess'), NyxTextResource('One too many'));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResources.ToData.ToJSON = LBefore), 'capacity refusal retains the complete registry');
  LResources.Define(NyxResourceRef('File 1'), NyxTextResource('Replacement'));
  Check((LResources.Count = NyxMaximumResources) and (LResources.Reference(0).Name = 'File 1') and
    (LResources.Definition(NyxResourceRef('File 1'), NyxDefaultLocale).Text = 'Replacement'),
    'replacement remains legal at capacity and preserves order');
  LResources := NewNyxResources;
  LDefinition := NyxBinaryResource(LBytes);
  LResources.Define(NyxResourceRef('Large first'), LDefinition)
    .Define(NyxResourceRef('Large second'), LDefinition)
    .Define(NyxResourceRef('Small third'), NyxTextResource('Accepted small file'));
  LBefore := LResources.ToData.ToJSON;
  LRejected := False;
  try
    LResources.Define(NyxResourceRef('Small third'), LDefinition);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResources.ToData.ToJSON = LBefore),
    'aggregate wire budget refuses larger replacement without partially publishing');
end;

{ The HTTP fixture yields between three complete contract groups. Startup must
  finish before the heavy capacity group runs; blocking the page's load callback
  would test the navigation driver's deadline instead of resource admission.
  No byte/count limit or assertion is reduced, and browser clocks stay real. }
procedure Run;
{$ifdef PAS2JS}
const
  CCheckpoints: array[0..2] of String =
    ('foundation-ownership', 'foundation-admission', 'foundation-capacity');
{$endif}
begin
  try
    {$ifdef PAS2JS}
    document.body.setAttribute('data-foundation-phase', IntToStr(GPhase));
    document.body.setAttribute('data-test-checks', IntToStr(GChecks));
    document.body.setAttribute('data-capture-checkpoint', CCheckpoints[GPhase]);
    { The existing Pascal driver acknowledges a bounded checkpoint before the
      next group runs. A blocked group therefore leaves its exact input phase
      recorded without reducing limits or increasing debugger deadlines. }

    if document.body.getAttribute('data-capture-observed') <> CCheckpoints[GPhase] then
    begin
      window.setTimeout(@Run, 20);
      Exit;
    end;
    {$endif}
    case GPhase of
      0:
        begin
          ExactOwnership;
        end;
      1:
        begin
          AtomicAdmission;
        end;
      2:
        begin
          CapacityBoundaries;
        end;
    end;
    Inc(GPhase);
    {$ifdef PAS2JS}

    if GPhase < 3 then
    begin
      window.setTimeout(@Run, 20);
      Exit;
    end;
    document.body.textContent := 'PASS / immutable resource foundation / ' + IntToStr(GChecks) + ' checks';
    document.body.setAttribute('data-test-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}

    if GPhase = 3 then
    begin
      WriteLn('PASS / immutable resource foundation / ', GChecks, ' checks');
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL / ' + LException.Message;
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifdef PAS2JS}
  window.setTimeout(@Run, 20);
  {$else}
  while (GPhase < 3) and (ExitCode = 0) do
  begin
    Run;
  end;
  {$endif}
end.
