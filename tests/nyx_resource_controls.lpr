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
program nyx_resource_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.resources, nyx.resources.rows,
  nyx.resource.sources,
  nyx.resource.cache,
  nyx.data, nyx.state, nyx.binding.types, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.source, nyx.composition, nyx.binding,
  nyx.collections, nyx.collections.view.types, nyx.collections.view, nyx.collections.mount,
  nyx.studio.session, nyx.studio.projects
  {$ifdef PAS2JS}, Web, nyx.render.browser, nyx.collections.browser, nyx.resource.cache.browser
  {$else}, Interfaces, Forms, StdCtrls, Grids, Graphics, IntfGraphics,
    FPWritePNG, nyx.render.lcl, nyx.collections.lcl, nyx.resource.cache.lcl{$endif};

const
  CCopy = '{"headline":"Your resource workshop","prompt":"Choose a project name","count":3,"ready":true}';
  CPeople = '{"people":[{"id":"ada","name":"Ada","score":9},{"id":"sam","name":"Sam","score":7}]}';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Resources: ' + AReason);
  end;
  Inc(GChecks);
end;

function Rows: TNyxResourceRows;
begin
  Result := NyxResourceRows(NyxResourceRef('people')).Field('people')
    .Identity(NyxResourcePath.Field('id'))
    .Text(NyxTextField('name'))
    .Integer(NyxIntegerField('score'));
end;

function Workshop: TNyxDocument;
var
  LDocument: TNyxDocument;
  LPage: INyxColumn;
  LHeadline: INyxLabel;
  LName: INyxInput;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Resource workshop';
    LDocument.Resources.Define(NyxResourceRef('copy'), NyxJSONResource(CCopy)
      .Describe('Workshop copy', 'Captions and prompts shared by workshop controls.'));
    LDocument.Resources.Define(NyxResourceRef('copy'), NyxLocale('en-GB'),
      NyxJSONResource('{"headline":"Your resource workbench","prompt":"Choose a programme name","count":4,"ready":false}'));
    LDocument.Resources.Define(NyxResourceRef('people'), NyxJSONResource(CPeople)
      .Describe('People', 'Example records for the editable team table.'));
    LDocument.Resources.Define(NyxResourceRef('notes'), NyxTextResource('Notes / 🌙' + TNyxText(#0)));
    LDocument.Resources.Define(NyxResourceRef('guide'), NyxTextResource('Resources keep your data with your project.'));
    LDocument.Resources.Define(NyxResourceRef('binary'), NyxBinaryResource(NyxDecodeBase64('AAH/')));
    LDocument.Resources.Define(NyxResourceRef('hosted-copy'),
      NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/workshop.json'))
        .Cache(NyxResourceCache.Persistent.FreshFor(600).StaleFor(60)
          .MaximumBytes(65536).ServerPolicy(rcspOverride))
        .Fallback(NyxJSONResource('{"caption":"Ready while hosted data loads"}'))
        .Describe('Hosted copy', 'Remote workshop copy with an authored initial fallback.'));
    LDocument.Collections.Define(Rows.Read(LDocument.Resources, NyxCollection('people'),
      NyxDefaultLocale, NyxDefaultLocale));
    LPage := NewNyxColumn('resource-home');
    LPage.Configure.Padding(24).Gap(14).Done;
    LDocument.AddPage(LPage);
    LHeadline := NewNyxLabel('workshop-headline');
    LHeadline.Binds.Text(NyxResourceValue(NyxResourceRef('copy')).Field('headline')).Done;
    LName := NewNyxInput('project-name');
    LName.Binds.Placeholder(NyxResourceValue(NyxResourceRef('copy')).Field('prompt')).Done;
    LPage.Add(LHeadline).Add(LName).Add(NewNyxTable('people-table'));
    LPage.Add(NewNyxLabel('notes-label').Binds.Text(NyxResourceValue(NyxResourceRef('guide'))).Done);
    LPage.Add(NewNyxLabel('hosted-label').Binds.Text(
      NyxResourceValue(NyxResourceRef('hosted-copy')).Field('caption')).Done);
    LDocument.Validate;
    Result := LDocument;
  except
    LDocument.Free;
    raise;
  end;
end;

type
  TCacheProbe = class
  public
    Found: Boolean;
    Success: Boolean;
    Error: TNyxText;
    Entry: TNyxResourceCacheEntry;
    procedure Read(AFound: Boolean; const AEntry: TNyxResourceCacheEntry;
      const AError: TNyxText);
    procedure Write(ASuccess: Boolean; const AError: TNyxText);
  end;

procedure TCacheProbe.Read(AFound: Boolean; const AEntry: TNyxResourceCacheEntry;
  const AError: TNyxText);
begin
  Found := AFound;
  Entry := AEntry;
  Error := AError;
end;

procedure TCacheProbe.Write(ASuccess: Boolean; const AError: TNyxText);
begin
  Success := ASuccess;
  Error := AError;
end;

procedure CacheStorage;
var
  LEntry: TNyxResourceCacheEntry;
  LOther: TNyxResourceCacheEntry;
  LPolicy: TNyxResourceCachePolicy;
  LDerived: TNyxResourceCachePolicy;
  LCache: INyxResourceCacheStorage;
  LJob: INyxResourceCacheJob;
  LProbe: TCacheProbe;
  LURL: TNyxResourceURL;
  LRefused: Boolean;
begin
  LURL := NyxResourceURL('https://example.com/data.json');
  LPolicy := NyxResourceCache.Memory.FreshFor(100).StaleFor(20);
  LDerived := LPolicy.FreshFor(10).ServerPolicy(rcspOverride);
  Check((LPolicy.FreshSeconds = 100) and (LPolicy.Server = rcspRespect),
    'cache policy branches preserve their baseline');
  LEntry := NyxResourceCacheEntry(LURL, NyxJSONResource('{"name":"cached"}'), 1000,
    NyxResourceCacheHeaders('max-age=50', '10'));
  Check(LEntry.StateAt(LPolicy, 1039) = rcsFresh, 'server freshness/Age qualifies caller TTL');
  Check(LEntry.StateAt(LPolicy, 1040) = rcsStale, 'fresh boundary enters explicit stale window');
  Check(LEntry.StateAt(LPolicy, 1060) = rcsExpired, 'stale boundary expires');
  Check(LEntry.StateAt(LPolicy, 999) = rcsMiss, 'backwards UTC clock refuses cache reuse');
  Check(LEntry.StateAt(LPolicy.Bypass, 1000) = rcsMiss, 'bypass never yields a cache hit');
  Check(LEntry.StateAt(LPolicy.MaximumBytes(1), 1000) = rcsMiss,
    'requesting payload budget also qualifies stored data');
  LEntry := NyxResourceCacheEntry(LURL, NyxJSONResource('{"name":"cached"}'), 1000,
    NyxResourceCacheHeaders('no-store', ''));
  Check(not LEntry.CanStore(LPolicy) and (LEntry.StateAt(LPolicy, 1000) = rcsMiss),
    'default respects no-store for storage/reuse');
  Check(LEntry.CanStore(LDerived) and (LEntry.StateAt(LDerived, 1000) = rcsFresh),
    'explicit caller override admits no-store into the application cache');
  LEntry := NyxResourceCacheEntry(LURL, NyxJSONResource('{"name":"cached"}'), 1000,
    NyxResourceCacheHeaders('max-age=40, must-revalidate', ''));
  Check(LEntry.StateAt(LPolicy, 1040) = rcsExpired, 'respect forbids stale must-revalidate data');
  Check(LEntry.StateAt(LPolicy.ServerPolicy(rcspOverride), 1090) = rcsFresh,
    'override uses caller freshness');
  LEntry := NyxResourceCacheEntry(LURL, NyxJSONResource('{"name":"cached"}'), 1000,
    NyxResourceCacheHeaders('no-cache', ''));
  Check(LEntry.StateAt(LPolicy, 1000) = rcsExpired, 'no-cache requires validation');
  LEntry := NyxResourceCacheEntry(LURL, NyxJSONResource('{"name":"cached"}'), 1000,
    NyxResourceCacheHeaders('max-age=20,max-age=30', ''));
  Check(LEntry.StateAt(LPolicy, 1000) = rcsExpired, 'conflicting server freshness requires validation');
  Check(TNyxResourceCacheEntry.FromData(LEntry.ToData).ToData.ToJSON = LEntry.ToData.ToJSON,
    'cache envelope round-trip is exact');
  LRefused := False;
  try
    LDerived := LPolicy.FreshFor(-1);
  except
    on ENyxBytes do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LDerived.FreshSeconds = 10), 'invalid derived policy preserves accepted value');
  LProbe := TCacheProbe.Create;
  LCache := NewNyxMemoryResourceCache(1);
  try
    LJob := LCache.Write(LEntry, LProbe.Write);
    Check(LProbe.Success and (LProbe.Error = ''), 'memory provider publishes admitted envelope');
    LJob := LCache.Read(LURL, nrkJSON, LProbe.Read);
    Check(LProbe.Found and (LProbe.Entry.Definition.Data.Field('name').AsText = 'cached'),
      'memory cache returns typed exact payload');
    LJob := LCache.Read(LURL, nrkText, LProbe.Read);
    Check(not LProbe.Found and (LProbe.Error = ''), 'same URL with another file kind is a miss');
    LOther := NyxResourceCacheEntry(NyxResourceURL('https://example.com/other.txt'),
      NyxTextResource('other'), 1000, NyxResourceCacheHeaders('', ''));
    LJob := LCache.Write(LOther, LProbe.Write);
    Check(not LProbe.Success and (LProbe.Error <> ''), 'cache entry budget refuses new content');
    LJob := LCache.Read(LURL, nrkJSON, LProbe.Read);
    Check(LProbe.Found, 'failed storage admission retains prior envelope');
    LJob.Cancel;
    {$ifndef PAS2JS}
    { This private test root remains ignored. The default user-temp provider is
      constructed separately, without writing a user/application URL into it. }
    LCache := NewNyxFileResourceCache;
    Check(LCache <> nil, 'native provider supports default user temporary storage');
    LCache := NewNyxFileResourceCache('build/resources/cache-🌙');
    LJob := LCache.Write(LEntry, LProbe.Write);
    Check(LProbe.Success, 'native provider writes one atomic UTF-8 envelope');
    LCache := nil;
    LCache := NewNyxFileResourceCache('build/resources/cache-🌙');
    LJob := LCache.Read(LURL, nrkJSON, LProbe.Read);
    Check(LProbe.Found and (LProbe.Entry.Definition.Data.Field('name').AsText = 'cached'),
      'native cache survives provider restart and Unicode folder');
    LCache := NewNyxFileResourceCache('build/resources/cache-🌙', 128, 1);
    LJob := LCache.Write(LEntry, LProbe.Write);
    Check(not LProbe.Success, 'native byte quota refuses oversized replacement');
    LJob := LCache.Read(LURL, nrkJSON, LProbe.Read);
    Check(LProbe.Found, 'native refused replacement preserves the existing file');
    {$endif}
  finally
    LJob.Cancel;
    LJob := nil;
    LCache := nil;
    LProbe.Free;
  end;
end;

procedure Shared;
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LCompanion: TNyxSourceWorkspace;
  LDefinition: INyxResourceDefinition;
  LCopy: INyxResources;
  LView: TNyxNode;
  LBefore: TNyxText;
  LSource: TNyxText;
  LSelector: TNyxResourceValueRef;
  LBranch: TNyxResourceValueRef;
  LBytes: TNyxBytes;
  LRefused: Boolean;
  LStore: INyxCollection;
  LPrevious: INyxCollectionSnapshot;
  LRows: TNyxResourceRows;
  LBranchRows: TNyxResourceRows;
  LSession: TNyxStudioSession;
  LInitialSource: TNyxText;
begin
  LDocument := Workshop;
  LDecoded := nil;
  LCandidate := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCompanion := nil;
  LView := nil;
  LSession := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    Check(TNyxDataValue.ParseJSON(LBefore).Field('version').AsInteger = 8,
      'resources select explicit document version eight');
    LDecoded := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LDecoded) = LBefore, 'exact resource/selector persistence');
    LDefinition := LDecoded.Resources.Definition(NyxResourceRef('hosted-copy'), NyxDefaultLocale);
    Check((LDefinition.Source.Kind = rskHosted) and
      (LDefinition.Source.URL.Address = 'https://example.com/workshop.json') and
      (LDefinition.Source.CachePolicy.Server = rcspOverride) and
      (LDefinition.Source.CachePolicy.FreshSeconds = 600), 'hosted transport and explicit cache policy survive');
    Check(LDefinition.Data.Field('caption').AsText = 'Ready while hosted data loads',
      'embedded fallback feeds the same typed hosted binding');
    LRefused := False;
    try
      LDefinition := LDefinition.Fallback(NyxTextResource('wrong file kind'));
    except
      on ENyxResource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDefinition.Kind = nrkJSON), 'invalid hosted fallback retains accepted definition');
    LRefused := False;
    try
      NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/unresolved.json')).Data;
    except
      on ENyxResource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'unresolved hosted payload never masquerades as empty data');
    LCopy := LDocument.Resources.Clone;
    LCopy.Define(NyxResourceRef('copy'), NyxJSONResource('{"headline":"Independent","prompt":"Independent"}'));
    Check(LDocument.Resources.Definition(NyxResourceRef('copy'), NyxDefaultLocale).Data
      .Field('headline').AsText = 'Your resource workshop', 'catalog clone membership is independent');
    LBytes := LDocument.Resources.Definition(NyxResourceRef('binary'), NyxDefaultLocale).Bytes;
    Check((Length(LBytes) = 3) and (LBytes[2] = 255), 'arbitrary bytes retained');
    LBytes[0] := 33;
    LBytes := LDocument.Resources.Definition(NyxResourceRef('binary'), NyxDefaultLocale).Bytes;
    Check(LBytes[0] = 0, 'byte access returns an independent copy');
    Check(NyxDecodeUTF8(NyxEncodeUTF8(TNyxText('🌙') + TNyxText(#0))) = TNyxText('🌙') + TNyxText(#0),
      'strict UTF-8 retains supplementary text and NUL');
    Check(NyxUTF8ByteCount('🌙') = 4, 'payload budgets count actual UTF-8 bytes');
    LRefused := False;
    LBytes := NyxDecodeBase64('7aCA');
    try
      NyxDecodeUTF8(LBytes);
    except
      on ENyxBytes do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'UTF-8 encoded surrogate refuses');
    LDefinition := NyxTextResource('accepted');
    LRefused := False;
    try
      LDefinition := NyxJSONResource('{"broken":');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDefinition.Text = 'accepted'), 'failed managed definition assignment is atomic');
    LDefinition := NyxJSONResource('{ "decimal" : 9007199254740993, "small":1.000e-2 }');
    Check(NyxDecodeUTF8(LDefinition.Bytes) = '{ "decimal" : 9007199254740993, "small":1.000e-2 }',
      'JSON original bytes and numeric spelling survive');
    Check(LDefinition.Data.Field('decimal').AsDecimal.Text = '9007199254740993',
      'JSON data retains integer precision beyond Double');
    LSelector := NyxResourceValue(NyxResourceRef('copy'));
    LBranch := LSelector.Field('count').AsInteger;
    Check((LSelector.Path.ToData.Count = 0) and (LSelector.Kind = nskText), 'selector branches are independent');
    Check(LBranch.Read(LDocument.Resources, NyxDefaultLocale, NyxDefaultLocale).IntegerValue = 3,
      'explicit integer field admission');
    Check(LSelector.Field('ready').AsBoolean.Read(LDocument.Resources,
      NyxDefaultLocale, NyxDefaultLocale).BooleanValue, 'explicit Boolean field admission');
    Check(LSelector.Field('headline').Read(LDocument.Resources, NyxLocale('missing'),
      NyxLocale('en-GB')).TextValue = 'Your resource workbench', 'explicit locale fallback');
    LRefused := False;
    try
      LSelector.Field('count').Read(LDocument.Resources, NyxDefaultLocale, NyxDefaultLocale);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'numeric field cannot silently become text');
    LRefused := False;
    try
      LSelector.Field('absent').Read(LDocument.Resources, NyxDefaultLocale, NyxDefaultLocale);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'missing field is actionable');
    LSelector := LSelector.Field('headline').Localize(NyxLocale('en-GB'), NyxDefaultLocale);
    Check(TNyxResourceValueRef.FromData(LSelector.ToData).ToData.ToJSON = LSelector.ToData.ToJSON,
      'localized selector wire retains all choices');
    LDecoded.Find('workshop-headline').Binds.Text(LSelector).Done;
    LView := RealizeNyxView(LDecoded, LDecoded.Pages[0]);
    ApplyNyxBindings(LView, LDecoded.State);
    Check(LView.Find('workshop-headline').Prop('text') = 'Your resource workbench',
      'realized explicit-localized caption');
    LRows := Rows;
    LBranchRows := LRows.Text(NyxTextField('another'), NyxResourcePath.Field('name'));
    LStore := NewNyxCollection(LRows.Read(LDocument.Resources, NyxCollection('people'),
      NyxDefaultLocale, NyxDefaultLocale));
    Check((LStore.Snapshot.Count = 2) and (LStore.Snapshot.Schema.Count = 2),
      'row recipe maps typed fields without aliasing a branch');
    Check(LBranchRows.Read(LDocument.Resources, NyxCollection('branch'),
      NyxDefaultLocale, NyxDefaultLocale).Schema.Count = 3, 'independent recipe adds a field');
    LPrevious := LStore.Snapshot;
    LCopy.Define(NyxResourceRef('people'), NyxJSONResource(
      '{"people":[{"id":"ada","name":"Ada","score":"wrong"}]}'));
    LRefused := False;
    try
      LRows.Reload(LStore, LCopy, NyxDefaultLocale, NyxDefaultLocale);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Snapshot.Revision = LPrevious.Revision) and
      (LStore.Snapshot.Count = 2), 'wrong row type preserves accepted dataset');
    LCopy.Define(NyxResourceRef('people'), NyxJSONResource(
      '{"people":[{"id":"ada","name":"Ada","score":1},{"id":"ada","name":"Duplicate","score":2}]}'));
    LRefused := False;
    try
      LRows.Reload(LStore, LCopy, NyxDefaultLocale, NyxDefaultLocale);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Snapshot.Revision = 0), 'duplicate stable identity refuses before mutation');
    LSource := TNyxCodegen.Generate(LDecoded);
    LCandidate := TNyxSourceWorkspace.PrepareDraft(LSource, LCompanion);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDecoded),
      'crafted generated resource source reconstructs exact document');
    Check(Pos('.Binds' + #10, LSource) > 0, 'resource authoring uses ordinary fluent bindings');
    Check(Pos('.Localize(NyxLocale(', LSource) > 0, 'source uses typed locale construction');
    LInitialSource := TNyxCodegen.Generate(LDocument);
    LSession := TNyxStudioSession.Create(NyxProjectPair(LBefore, LInitialSource));
    LSession.AdoptProject(NyxProjectPair(TNyxCodec.Encode(LDecoded), LSource));
    Check((LSession.Save = TNyxCodec.Encode(LDecoded)) and (LSession.Source = LSource),
      'ordinary session admits resources and exact source as one owned pair');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LInitialSource),
      'one paired Undo restores the complete resource/source baseline');
    LSession.Redo;
    Check((LSession.Save = TNyxCodec.Encode(LDecoded)) and (LSession.Source = LSource),
      'paired Redo restores hosted policies/selectors and exact source');
    LWorkspace.Accept(LDocument, TNyxCodegen.Generate(LDocument));
    Check(Pos('nyx.resources', LWorkspace.Render(LDecoded)) > 0,
      'managed resource changes keep required imports');
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      with TFileStream.Create(ParamStr(1), fmCreate) do
      try
        WriteBuffer(LSource[1], Length(LSource));
      finally
        Free;
      end;
    end;
    {$endif}
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'qualification retains the accepted authored baseline');
  finally
    LSession.Free;
    LView.Free;
    LCompanion.Free;
    LWorkspace.Free;
    LCandidate.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

procedure Controls;
var
  LDocument: TNyxDocument;
  LCopy: INyxResources;
  LStore: INyxCollection;
  LSibling: INyxCollection;
  LTable: INyxCollectionView;
  LMount: INyxCollectionMount;
  LRefused: Boolean;
  LBefore: TNyxText;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LRenderer: TNyxBrowserRenderer;
  LSiblingRenderer: TNyxBrowserRenderer;
  LSiblingHost: TJSHTMLElement;
  {$else}
  LHost: TForm;
  LRenderer: TNyxLCLRenderer;
  LSiblingRenderer: TNyxLCLRenderer;
  LSiblingHost: TForm;
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  {$endif}

  procedure Caption(const AExpected: TNyxText);
  begin
    {$ifdef PAS2JS}
    Check(LRenderer.ElementFor('workshop-headline').textContent = AExpected, 'actual browser resource caption');
    {$else}
    Check(TNyxText(TLabel(LRenderer.ControlFor('workshop-headline')).Caption) = AExpected,
      'actual native resource caption');
    {$endif}
  end;

  procedure Prompt(const AExpected: TNyxText);
  begin
    {$ifdef PAS2JS}
    Check(TJSHTMLInputElement(LRenderer.InputFor('project-name')).placeholder = AExpected,
      'actual browser resource placeholder');
    {$else}
    Check(TNyxText(TEdit(LRenderer.InputFor('project-name')).TextHint) = AExpected,
      'actual native resource placeholder');
    {$endif}
  end;

  procedure TableCell(const AExpected: TNyxText);
  begin
    {$ifdef PAS2JS}
    Check(Pos(AExpected, LRenderer.ElementFor('people-table').textContent) > 0,
      'actual browser resource table cell');
    {$else}
    Check(TNyxText(TStringGrid(LRenderer.ControlFor('people-table')).Cells[0, 1]) = AExpected,
      'actual native resource table cell');
    {$endif}
  end;

begin
  LDocument := Workshop;
  LBefore := TNyxCodec.Encode(LDocument);
  LCopy := LDocument.Resources.Clone;
  LStore := NewNyxCollection(LDocument.Collections.Snapshot(NyxCollection('people')));
  LSibling := LStore.Clone;
  LTable := NewNyxCollectionView(LStore, NyxCollectionView(NyxCollection('people'))
    .Column(NyxTextField('name'), 'Name')
    .Column(NyxIntegerField('score'), 'Score', cmEditable), cpTable);
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LSiblingHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  document.body.appendChild(LSiblingHost);
  LRenderer := TNyxBrowserRenderer.Create;
  LSiblingRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 840, 620);
  LSiblingHost := TForm.CreateNew(nil);
  LSiblingHost.SetBounds(0, 0, 840, 620);
  LRenderer := TNyxLCLRenderer.Create;
  LSiblingRenderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LSiblingRenderer.Render(LDocument, LDocument.Pages[0], LSiblingHost);
    LMount := LRenderer.BindCollection('people-table', LTable);
    Caption('Your resource workshop');
    Prompt('Choose a project name');
    TableCell('Ada');
    LRenderer.ReloadResources(LCopy, NyxLocale('en-GB'), NyxDefaultLocale);
    Caption('Your resource workbench');
    Prompt('Choose a programme name');
    Check(LSiblingRenderer.Root.Find('workshop-headline').Prop('text') = 'Your resource workshop',
      'runtime locale reload leaves sibling view independent');
    LCopy.Define(NyxResourceRef('copy'), NyxJSONResource(
      '{"headline":"Updated workshop","prompt":"Name your new project"}'));
    LRenderer.ReloadResources(LCopy, NyxDefaultLocale, NyxDefaultLocale);
    Caption('Updated workshop');
    Prompt('Name your new project');
    LCopy.Define(NyxResourceRef('copy'), NyxJSONResource(
      '{"headline":"Must not appear","prompt":42}'));
    LRefused := False;
    try
      LRenderer.ReloadResources(LCopy, NyxDefaultLocale, NyxDefaultLocale);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid second scalar refuses the complete reload');
    Caption('Updated workshop');
    Prompt('Name your new project');
    LTable.Select(NyxItem(NyxCollection('people'), 'ada'));
    LCopy.Define(NyxResourceRef('people'), NyxJSONResource(
      '{"people":[{"id":"ada","name":"Ada updated","score":10},{"id":"sam","name":"Sam","score":8}]}'));
    Rows.Reload(LStore, LCopy, NyxDefaultLocale, NyxDefaultLocale, 0);
    TableCell('Ada updated');
    Check(LTable.Selected.ID = 'ada', 'resource reload preserves stable selected identity');
    Check(LSibling.Snapshot.Item(NyxItem(NyxCollection('people'), 'ada'))
      .GetValue(NyxTextField('name')) = 'Ada', 'table reload leaves sibling store independent');
    Check(LDocument.Collections.Snapshot(NyxCollection('people')).Revision = 0,
      'resource reload does not write saved collection defaults');
    LRefused := False;
    try
      Rows.Reload(LStore, LCopy, NyxDefaultLocale, NyxDefaultLocale, 0);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'stale resource row reload refuses');
    {$ifndef PAS2JS}
    LHost.Show;
    Application.ProcessMessages;

    if ParamCount > 1 then
    begin
      LBitmap := TBitmap.Create;
      LImage := nil;
      try
        LBitmap.SetSize(LHost.ClientWidth, LHost.ClientHeight);
        LHost.PaintTo(LBitmap.Canvas, 0, 0);
        LImage := LBitmap.CreateIntfImage;
        LImage.SaveToFile(ParamStr(2));
      finally
        LImage.Free;
        LBitmap.Free;
      end;
    end;
    {$endif}
    LRenderer.Unmount;
    LStore.Update(NyxCollectionItem(NyxItem(NyxCollection('people'), 'ada'))
      .WithValue(NyxTextField('name'), 'After unmount'));
    Check(LStore.Snapshot.Revision = 2, 'retained store remains safe after mount retirement');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'actual controls retain authored bytes/source/defaults');
  finally
    LMount := nil;
    LRenderer.Free;
    LSiblingRenderer.Free;
    LTable := nil;
    LStore := nil;
    LSibling := nil;
    {$ifdef PAS2JS}
    LHost.remove;
    LSiblingHost.remove;
    {$else}
    LHost.Free;
    LSiblingHost.Free;
    {$endif}
    LDocument.Free;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    Shared;
    CacheStorage;
    Controls;
    WriteLn('PASS / resource bindings / ', GChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');{$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
