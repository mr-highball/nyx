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

program nyx_resource_import_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.bytes, nyx.resources,
  nyx.resource.sources, nyx.resource.context, nyx.studio.glyphs
  {$ifdef PAS2JS}, Web{$endif};

const
  CNotesName: TNyxText = 'notes 🌙';
  CAcceptedText: TNyxText = 'Accepted' + #0 + '🌙';
  CForeignText: TNyxText = 'Admitted 🌙';

type
  { Borrowed synchronous fixture callback. Its owner outlives the foreign source
    getter invocation and clears the callback before retirement. }
  TCatalogRead = procedure of object;

  { Models an application whose last destination leases live in fields. The
    foreign source deliberately releases both fields during its outward getter. }
  TImportOwner = class
  public
    Target: INyxResources;
    Importer: INyxResourceCatalogImport;
    ReleaseCalls: Integer;
    procedure ReleaseTarget;
  end;

  { Deliberately foreign mutable catalog. Clone returns its own facade rather
    than an independent snapshot; atomic import must use its strict copied wire,
    never trust that foreign claim or retain its later mutable membership. }
  TSuppliedCatalog = class(TInterfacedObject, INyxResources)
  private
    FBacking: INyxResources;
    FWire: TNyxDataValue;
  public
    WireReads: Integer;
    OnRead: TCatalogRead;
    constructor Create(const ABacking: INyxResources);
    procedure Supply(const AWire: TNyxDataValue);
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

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Resource import: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TImportOwner.ReleaseTarget;
begin
  Inc(ReleaseCalls);
  Importer := nil;
  Target := nil;
end;

constructor TSuppliedCatalog.Create(const ABacking: INyxResources);
begin
  inherited Create;
  FBacking := ABacking;
  FWire := ABacking.ToData;
end;

procedure TSuppliedCatalog.Supply(const AWire: TNyxDataValue);
begin
  FWire := AWire.Copy;
end;

function TSuppliedCatalog.Define(const AReference: TNyxResourceRef;
  const ADefinition: INyxResourceDefinition): INyxResources;
begin
  FBacking.Define(AReference, ADefinition);
  Result := Self;
end;

function TSuppliedCatalog.Define(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef; const ADefinition: INyxResourceDefinition): INyxResources;
begin
  FBacking.Define(AReference, ALocale, ADefinition);
  Result := Self;
end;

function TSuppliedCatalog.Remove(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): INyxResources;
begin
  FBacking.Remove(AReference, ALocale);
  Result := Self;
end;

function TSuppliedCatalog.Contains(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): Boolean;
begin
  Result := FBacking.Contains(AReference, ALocale);
end;

function TSuppliedCatalog.Definition(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): INyxResourceDefinition;
begin
  Result := FBacking.Definition(AReference, ALocale);
end;

function TSuppliedCatalog.Resolve(const AReference: TNyxResourceRef;
  const ALocale, AFallback: TNyxLocaleRef): INyxResourceDefinition;
begin
  Result := FBacking.Resolve(AReference, ALocale, AFallback);
end;

function TSuppliedCatalog.Reference(AIndex: Integer): TNyxResourceRef;
begin
  Result := FBacking.Reference(AIndex);
end;

function TSuppliedCatalog.Locale(AIndex: Integer): TNyxLocaleRef;
begin
  Result := FBacking.Locale(AIndex);
end;

function TSuppliedCatalog.GetCount: Integer;
begin
  Result := FBacking.Count;
end;

function TSuppliedCatalog.Clone: INyxResources;
begin
  Result := Self;
end;

function TSuppliedCatalog.ToData: TNyxDataValue;
begin
  Inc(WireReads);

  if Assigned(OnRead) then
  begin
    OnRead;
  end;
  Result := FWire.Copy;
end;

procedure CatalogComposition;
var
  LTarget: INyxResources;
  LSource: INyxResources;
  LRetained: INyxResources;
  LReturn: INyxResources;
  LCopy: INyxResources;
  LImporter: INyxResourceCatalogImport;
  LSourceWire: TNyxText;
  LBefore: TNyxText;
  LRejected: Boolean;
  LBytes: TNyxBytes;
  LBinary: TNyxBytes;
  LDefinition: INyxResourceDefinition;
begin
  LTarget := NewNyxResources;
  LTarget.Define(NyxResourceRef('unchanged'), NyxTextResource('Before'));
  LTarget.Define(NyxResourceRef('notes 🌙'), NyxTextResource('Original'));
  LRetained := LTarget;
  LSource := NewNyxResources;
  LSource.Define(NyxResourceRef('notes 🌙'), NyxTextResource('Accepted' + #0 + '🌙')
    .Tagged(NyxResourceLabel('User help')).Describe('Notes', 'Intent survives import'));
  LSource.Define(NyxResourceRef('notes 🌙'), NyxLocale('en-US'),
    NyxTextResource('English'));
  LSource.Define(NyxResourceRef('data'),
    NyxJSONResource('{"large":9007199254740993,"tiny":1.000e-2,"zero":-0}'));
  SetLength(LBytes, 4);
  LBytes[0] := 0;
  LBytes[1] := 255;
  LBytes[2] := 128;
  LBytes[3] := 1;
  LSource.Define(NyxResourceRef('bytes'), NyxBinaryResource(LBytes));
  LSource.Define(NyxResourceRef('hosted'),
    NyxHostedResource(nrkText, NyxResourceURL('https://example.test/notes'))
      .Fallback(NyxTextResource('Offline')));
  LSourceWire := LSource.ToData.ToJSON;
  LImporter := NyxResourceCatalogImport(LTarget);
  LReturn := LImporter.Import(LSource);
  Check((LReturn = LTarget) and (LRetained.Count = 6),
    'publication preserves destination identity and previously retained facades');
  Check((LTarget.Reference(0).Name = 'unchanged') and
    (LTarget.Reference(1).Name = CNotesName) and
    (LTarget.Reference(2).Name = CNotesName) and (LTarget.Locale(2).Name = 'en-US') and
    (LTarget.Reference(5).Name = 'hosted'),
    'exact replacements keep position and new localized pairs follow source order');
  Check(LTarget.Definition(NyxResourceRef('notes 🌙'), NyxDefaultLocale).Text =
    CAcceptedText, 'embedded Unicode and control text remain exact');
  LDefinition := LTarget.Definition(NyxResourceRef('notes 🌙'), NyxDefaultLocale);
  Check((LDefinition.Title = 'Notes') and
    (LDefinition.Description = 'Intent survives import') and
    NyxResourceLabelsOf(LDefinition).Contains(NyxResourceLabel('User help')),
    'creator help and discovery labels survive admission reuse');
  Check(LTarget.Definition(NyxResourceRef('notes 🌙'), NyxLocale('en-US')).Text =
    'English', 'localized variants remain distinct from default membership');
  Check(NyxDecodeUTF8(LTarget.Definition(NyxResourceRef('data'), NyxDefaultLocale).Bytes) =
    '{"large":9007199254740993,"tiny":1.000e-2,"zero":-0}',
    'resource JSON retains exact numeric source rather than converting through Double');
  LBinary := LTarget.Definition(NyxResourceRef('bytes'), NyxDefaultLocale).Bytes;
  Check((Length(LBinary) = 4) and (LBinary[0] = 0) and (LBinary[1] = 255) and
    (LBinary[2] = 128) and (LBinary[3] = 1), 'binary resources retain exact bytes');
  LBinary[0] := 77;
  LBinary := LTarget.Definition(NyxResourceRef('bytes'), NyxDefaultLocale).Bytes;
  Check(LBinary[0] = 0, 'exported byte arrays cannot mutate imported resource content');
  LDefinition := LTarget.Definition(NyxResourceRef('hosted'), NyxDefaultLocale);
  Check((LDefinition.Source.Kind = rskHosted) and
    (LDefinition.FallbackDefinition.Text = 'Offline'),
    'hosted declarations retain their embedded fallback without fetching');
  Check(LSource.ToData.ToJSON = LSourceWire, 'source catalog is unchanged by import');
  LBefore := LTarget.ToData.ToJSON;
  LReturn := LImporter.Import(LTarget);
  Check(LTarget.ToData.ToJSON = LBefore, 'self import preserves exact canonical catalog wire');
  LSource.Define(NyxResourceRef('notes 🌙'), NyxTextResource('Later source'));
  Check(LTarget.Definition(NyxResourceRef('notes 🌙'), NyxDefaultLocale).Text =
    CAcceptedText, 'later source replacement cannot mutate destination');
  LTarget.Remove(NyxResourceRef('notes 🌙'), NyxLocale('en-US'));
  Check(LSource.Contains(NyxResourceRef('notes 🌙'), NyxLocale('en-US')),
    'destination removal cannot change source membership');
  LCopy := LTarget.Clone;
  LImporter := nil;
  LRetained := nil;
  LReturn := nil;
  LTarget := nil;
  Check(LCopy.Definition(NyxResourceRef('notes 🌙'), NyxDefaultLocale).Text =
    CAcceptedText, 'independent clone outlives all original destination facades');
  LBefore := LCopy.ToData.ToJSON;
  LRejected := False;
  try
    NyxResourceCatalogImport(LCopy).Import(nil);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCopy.ToData.ToJSON = LBefore),
    'nil source refuses atomically');
end;

procedure ForeignAdmission;
var
  LTarget: INyxResources;
  LBacking: INyxResources;
  LForeign: TSuppliedCatalog;
  LLease: INyxResources;
  LCapability: INyxResourceCatalogImport;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LTarget := NewNyxResources;
  LTarget.Define(NyxResourceRef('existing'), NyxTextResource('Keep'));
  LBacking := NewNyxResources;
  LBacking.Define(NyxResourceRef('foreign'), NyxTextResource('Admitted 🌙'));
  LForeign := TSuppliedCatalog.Create(LBacking);
  LLease := LForeign;
  NyxResourceCatalogImport(LTarget).Import(LLease);
  Check(LForeign.WireReads = 1, 'foreign source is admitted through its wire exactly once');
  LForeign.Supply(NewNyxResources.Define(NyxResourceRef('foreign'),
    NyxTextResource('Changed later')).ToData);
  LBacking.Define(NyxResourceRef('foreign'), NyxTextResource('Backing changed'));
  Check(LTarget.Definition(NyxResourceRef('foreign'), NyxDefaultLocale).Text =
    CForeignText, 'lying foreign Clone and later wire changes cannot mutate imported data');
  LBefore := LTarget.ToData.ToJSON;
  LForeign.Supply(NyxObject([NyxField('version', NyxData(99)),
    NyxField('entries', NyxArray([]))]));
  LRejected := False;
  try
    NyxResourceCatalogImport(LTarget).Import(LLease);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LTarget.ToData.ToJSON = LBefore),
    'invalid foreign wire leaves complete destination membership unchanged');
  LRejected := False;
  try
    LCapability := NyxResourceCatalogImport(LLease);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCapability = nil),
    'legacy destination without optional atomic import fails explicitly');
  LRejected := False;
  try
    LCapability := NyxResourceCatalogImport(nil);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil destination has no fabricated import capability');
end;

procedure ReentrantReceiverLease;
  { Bound interface-returning preparation temporaries to this completed call.
    pas2js can retain a fluent factory result until its enclosing routine ends;
    that hidden lease would otherwise mask the receiver-withdrawal regression. }
  procedure PrepareOwner(AOwner: TImportOwner);
  begin
    AOwner.Target := NewNyxResources;
    AOwner.Target.Define(NyxResourceRef('existing'), NyxTextResource('Original destination'));
    AOwner.Importer := NyxResourceCatalogImport(AOwner.Target);
  end;

var
  LOwner: TImportOwner;
  LForeign: TSuppliedCatalog;
  LSource: INyxResources;
  LReturned: INyxResources;
begin
  LOwner := TImportOwner.Create;
  LForeign := TSuppliedCatalog.Create(NewNyxResources.Define(
    NyxResourceRef('incoming'), NyxTextResource('From the foreign source')));
  LSource := LForeign;
  try
    PrepareOwner(LOwner);
    LForeign.OnRead := LOwner.ReleaseTarget;
    LReturned := LOwner.Importer.Import(LSource);
    Check((LOwner.ReleaseCalls = 1) and (LOwner.Target = nil) and
      (LOwner.Importer = nil), 'foreign getter actually withdraws every caller-held target lease');
    Check((LReturned.Count = 2) and
      (LReturned.Definition(NyxResourceRef('existing'), NyxDefaultLocale).Text =
        'Original destination') and
      (LReturned.Definition(NyxResourceRef('incoming'), NyxDefaultLocale).Text =
        'From the foreign source'),
      'entered receiver survives foreign reentry and hands off a complete returned lease');
    LReturned.Remove(NyxResourceRef('incoming'), NyxDefaultLocale);
    Check(LSource.Count = 1, 'surviving returned target still owns independent membership');
  finally
    LForeign.OnRead := nil;
    LSource := nil;
    LReturned := nil;
    LOwner.Free;
  end;
end;

procedure CapacityAndRollback;
var
  LTarget: INyxResources;
  LSource: INyxResources;
  LBefore: TNyxText;
  LText: TNyxText;
  LIndex: Integer;
  LRejected: Boolean;
begin
  LTarget := NewNyxResources;
  for LIndex := 1 to NyxMaximumResources do
  begin
    LTarget.Define(NyxResourceRef('item-' + IntToStr(LIndex)), NyxTextResource('Original'));
  end;
  LSource := NewNyxResources;
  LSource.Define(NyxResourceRef('item-1'), NyxTextResource('Would replace'));
  LSource.Define(NyxResourceRef('overflow'), NyxTextResource('Would append'));
  LBefore := LTarget.ToData.ToJSON;
  LRejected := False;
  try
    NyxResourceCatalogImport(LTarget).Import(LSource);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LTarget.ToData.ToJSON = LBefore),
    'count overflow cannot publish an earlier replacement from the same import');
  LSource.Remove(NyxResourceRef('overflow'), NyxDefaultLocale);
  NyxResourceCatalogImport(LTarget).Import(LSource);
  Check((LTarget.Count = NyxMaximumResources) and
    (LTarget.Definition(NyxResourceRef('item-1'), NyxDefaultLocale).Text = 'Would replace'),
    'replacement at exact catalog capacity remains permitted');
  LTarget := NewNyxResources;
  LText := StringOfChar('x', 900000);
  LTarget.Define(NyxResourceRef('large-1'), NyxTextResource(LText));
  LTarget.Define(NyxResourceRef('large-2'), NyxTextResource(LText));
  LTarget.Define(NyxResourceRef('large-3'), NyxTextResource(LText));
  LSource := NewNyxResources;
  LSource.Define(NyxResourceRef('new-large'), NyxTextResource(LText));
  LSource.Define(NyxResourceRef('large-1'), NyxTextResource('Small replacement'));
  NyxResourceCatalogImport(LTarget).Import(LSource);
  Check((LTarget.Count = 4) and
    (LTarget.Definition(NyxResourceRef('large-1'), NyxDefaultLocale).Text = 'Small replacement'),
    'whole-candidate byte admission permits a later shrinking replacement');
  Check((LTarget.Reference(0).Name = 'large-1') and
    (LTarget.Reference(3).Name = 'new-large'),
    'whole-candidate admission preserves destination order and appends once');
  LBefore := LTarget.ToData.ToJSON;
  LSource := NewNyxResources;
  LSource.Define(NyxResourceRef('newest-large'), NyxTextResource(LText));
  LSource.Define(NyxResourceRef('large-1'), NyxTextResource('Would change first'));
  LRejected := False;
  try
    NyxResourceCatalogImport(LTarget).Import(LSource);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LTarget.ToData.ToJSON = LBefore),
    'aggregate wire overflow preserves exact original catalog and byte charges');
end;

procedure StudioGlyphComposition;
var
  LFirst: INyxResources;
  LSecond: INyxResources;
  LTarget: INyxResources;
  LWire: TNyxText;
  LImporter: INyxResourceCatalogImport;
  LIndex: Integer;
begin
  LFirst := NyxStudioGlyphResources;
  LSecond := NyxStudioGlyphResources;
  LWire := LFirst.ToData.ToJSON;
  Check((LFirst.Count = 4) and (LSecond.ToData.ToJSON = LWire),
    'independent Studio glyph snapshots preserve the original asset wire');
  Check(LFirst.Contains(NyxStudioGlyphResource, NyxDefaultLocale) and
    LFirst.Contains(NyxResourceRef('nyx.studio.edit-glyph-atlas'), NyxDefaultLocale) and
    LFirst.Contains(NyxResourceRef('nyx.studio.move-down-glyph'), NyxDefaultLocale) and
    LFirst.Contains(NyxResourceRef('nyx.studio.delete-glyph'), NyxDefaultLocale),
    'Studio publishes both atlases and the two isolated editing glyphs');
  LTarget := NewNyxResources;
  LTarget.Define(NyxResourceRef('application-help'), NyxTextResource('Project-owned'));
  LImporter := NyxResourceCatalogImport(LTarget);
  for LIndex := 1 to 20 do
  begin
    LImporter.Import(LFirst);
  end;
  Check((LTarget.Count = 5) and
    (LTarget.Definition(NyxResourceRef('application-help'), NyxDefaultLocale).Text =
      'Project-owned'), 'repeated asset import preserves unrelated resource membership');
  Check(LTarget.Definition(NyxStudioGlyphResource, NyxDefaultLocale).Image.ToWire =
    NyxStudioGlyphAtlas.ToWire, 'imported atlas retains exact original image bytes');
  LFirst.Remove(NyxStudioGlyphResource, NyxDefaultLocale);
  Check((LFirst.Count = 3) and (LSecond.Count = 4) and
    (NyxStudioGlyphResources.Count = 4) and
    LTarget.Contains(NyxStudioGlyphResource, NyxDefaultLocale),
    'mutating a caller snapshot cannot change another shell or startup glyph catalog');
end;

{ Runtime contexts may share privately admitted immutable definitions, while
  every exported catalog keeps independent mutable membership. A foreign Clone
  cannot certify admission; its exact wire still controls the captured result. }
procedure RuntimeContextAdmission;
var
  LSource: INyxResources;
  LSnapshot: INyxResources;
  LAnother: INyxResources;
  LContext: INyxResourceContext;
  LPeer: INyxResourceContext;
  LDefinition: INyxResourceDefinition;
  LForeign: TSuppliedCatalog;
  LForeignLease: INyxResources;
  LBacking: INyxResources;
  LRejectedContext: INyxResourceContext;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LSource := NyxStudioGlyphResources;
  LSource.Define(NyxResourceRef(CNotesName), NyxTextResource(CAcceptedText));
  LSource.Define(NyxResourceRef(CNotesName), NyxLocale('en-US'),
    NyxTextResource('English 🌙'));
  LBefore := LSource.ToData.ToJSON;
  LDefinition := LSource.Definition(NyxStudioGlyphResource, NyxDefaultLocale);
  LContext := NewNyxResourceContext(LSource, NyxLocale('en-US'), NyxLocale('en'));
  LSnapshot := LContext.Snapshot;
  Check(LSnapshot.Definition(NyxStudioGlyphResource, NyxDefaultLocale) = LDefinition,
    'runtime capture shares the already admitted immutable atlas definition');
  Check((LSnapshot.ToData.ToJSON = LBefore) and
    (LContext.Locale.Name = 'en-US') and (LContext.Fallback.Name = 'en'),
    'runtime capture retains exact catalog wire and independent locale choices');
  LPeer := NewNyxResourceContext(LSource, NyxDefaultLocale, NyxDefaultLocale);
  LSource.Remove(NyxStudioGlyphResource, NyxDefaultLocale);
  LSource.Define(NyxResourceRef(CNotesName), NyxTextResource('Changed source'));
  LSnapshot.Remove(NyxResourceRef(CNotesName), NyxDefaultLocale);
  LSnapshot.Define(NyxResourceRef('snapshot-only'), NyxTextResource('Independent'));
  LAnother := LContext.Snapshot;
  Check((LAnother.ToData.ToJSON = LBefore) and
    (LPeer.Snapshot.ToData.ToJSON = LBefore),
    'source/export membership edits cannot change either captured runtime');
  Check(LAnother.Definition(NyxResourceRef(CNotesName), NyxDefaultLocale).Text =
    CAcceptedText, 'captured text retains supplementary Unicode and embedded NUL');
  Check(LAnother.Definition(NyxStudioGlyphResource, NyxDefaultLocale).Image.ToWire =
    NyxStudioGlyphAtlas.ToWire, 'captured immutable image retains its exact bytes');
  LBacking := NewNyxResources.Define(NyxResourceRef('foreign'),
    NyxTextResource(CForeignText));
  LForeign := TSuppliedCatalog.Create(LBacking);
  LForeignLease := LForeign;
  LBefore := LBacking.ToData.ToJSON;
  LContext := NewNyxResourceContext(LForeignLease, NyxDefaultLocale, NyxDefaultLocale);
  Check((LForeign.WireReads = 1) and (LContext.Snapshot.ToData.ToJSON = LBefore),
    'foreign runtime source crosses the strict wire boundary exactly once');
  LForeign.Supply(NewNyxResources.Define(NyxResourceRef('foreign'),
    NyxTextResource('Changed wire')).ToData);
  LBacking.Define(NyxResourceRef('foreign'), NyxTextResource('Changed backing'));
  Check(LContext.Snapshot.ToData.ToJSON = LBefore,
    'lying foreign Clone and later membership/wire edits cannot change a runtime');
  LForeign.Supply(NyxObject([NyxField('version', NyxData(99)),
    NyxField('entries', NyxArray([]))]));
  LRejected := False;
  try
    LRejectedContext := NewNyxResourceContext(LForeignLease,
      NyxDefaultLocale, NyxDefaultLocale);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LRejectedContext = nil) and
    (LContext.Snapshot.ToData.ToJSON = LBefore),
    'invalid foreign runtime wire refuses without a context or prior mutation');
  LRejected := False;
  try
    LRejectedContext := NewNyxResourceContext(nil, NyxDefaultLocale, NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LRejectedContext = nil), 'nil runtime resources still refuse');
  LContext := NewNyxResourceContext(NewNyxResources, NyxDefaultLocale, NyxDefaultLocale);
  Check(LContext.Snapshot.Count = 0, 'an independently owned empty runtime catalog stays valid');
end;

{$ifndef PAS2JS}
{ Optional diagnostic of the exact former shell operation versus catalog import.
  It times public asset admission only; no ratio is an editor latency claim and
  no wall-clock threshold substitutes for functional assertions or native UI. }
procedure GlyphAdmissionCosts;
const
  CIterations = 20;
var
  LTarget: INyxResources;
  LSource: INyxResources;
  LOriginalWire: TNyxText;
  LIndex: Integer;
  LStarted: QWord;
  LDefineMilliseconds: QWord;
  LImportMilliseconds: QWord;
begin
  LSource := NyxStudioGlyphResources;
  LStarted := GetTickCount64;
  for LIndex := 1 to CIterations do
  begin
    { The current family contains four images. Re-admit that complete wire,
      rather than comparing one former atlas with an imported four-asset pack. }
    LTarget := NyxResourcesFromData(LSource.ToData);
  end;
  LDefineMilliseconds := GetTickCount64 - LStarted;
  LOriginalWire := LTarget.ToData.ToJSON;
  LTarget := nil;
  LStarted := GetTickCount64;
  for LIndex := 1 to CIterations do
  begin
    LTarget := NewNyxResources;
    NyxResourceCatalogImport(LTarget).Import(LSource);
  end;
  LImportMilliseconds := GetTickCount64 - LStarted;
  Check(LTarget.ToData.ToJSON = LOriginalWire,
    'measured import and full fresh admission produce identical catalog wire');
  WriteLn('GLYPH_ADMISSION iterations=', CIterations,
    ' define_ms=', LDefineMilliseconds, ' import_ms=', LImportMilliseconds);
end;
{$endif}

begin
  try
    CatalogComposition;
    ForeignAdmission;
    ReentrantReceiverLease;
    CapacityAndRollback;
    StudioGlyphComposition;
    RuntimeContextAdmission;
    {$ifndef PAS2JS}

    if (ParamCount = 1) and (ParamStr(1) = '--glyph-costs') then
    begin
      GlyphAdmissionCosts;
    end;
    {$endif}
    WriteLn('PASS ', GChecks, ' atomic resource import checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-test-result', 'passed');
    document.body.setAttribute('data-test-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'failed');
      document.body.setAttribute('data-test-error', LException.Message);
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
