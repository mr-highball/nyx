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

program nyx_resource_mapping_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.state, nyx.resources, nyx.resources.rows,
  nyx.collections, nyx.collections.registry, nyx.collections.codec,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.bindings,
  nyx.model, nyx.codec, nyx.codegen, nyx.source, nyx.composition,
  nyx.resource.mapping.fixture, nyx.application.resources,
  nyx.resource.context,
  nyx.studio.session, nyx.studio.projects, nyx.studio.resourceedits,
  nyx.studio.agents
  {$ifdef PAS2JS}, Web, nyx.render.browser, nyx.application.browser
  {$else}, Classes, Interfaces, Forms, Grids, StdCtrls,
    nyx.render.lcl, nyx.application.lcl, nyx.studio.mcp,
    nyx.studio.directories, nyx.studio.outputs{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  TTestApplication = TNyxBrowserApplication;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TTestApplication = TNyxLCLApplication;
  {$endif}

var
  GChecks: Integer;
  GPhase: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Saved resource mapping: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure RefuseDocument(const ASource: TNyxText; const AReason: TNyxText);
var
  LDocument: TNyxDocument;
  LRefused: Boolean;
begin
  LDocument := nil;
  LRefused := False;
  try
    try
      LDocument := TNyxCodec.Decode(ASource);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, AReason);
  finally
    LDocument.Free;
  end;
end;

{ Exact structural corruption is independent of target JSON whitespace. Every
  step must already exist; the immutable original document remains unchanged. }
function Changed(const AData: TNyxDataValue; const APath: TNyxResourcePath;
  const AValue: TNyxDataValue; AStep: Integer = 0): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LStep: TNyxDataValue;
begin

  if AStep = APath.ToData.Count then
  begin
    Exit(AValue.Copy);
  end;
  LStep := APath.ToData.Item(AStep);
  { Select the remaining existing member now, rather than accepting a typo. }
  case AData.Kind of
    ndObject:
      begin
        SetLength(LFields, AData.Count);
        for LIndex := 0 to High(LFields) do
        begin
          LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

          if AData.Key(LIndex) = LStep.AsText then
          begin
            LFields[LIndex] := NyxField(AData.Key(LIndex),
              Changed(AData.Field(AData.Key(LIndex)), APath, AValue, AStep + 1));
          end;
        end;
        AData.Field(LStep.AsText);
        Result := NyxObject(LFields);
      end;
    ndArray:
      begin
        SetLength(LItems, AData.Count);
        for LIndex := 0 to High(LItems) do
        begin
          LItems[LIndex] := AData.Item(LIndex);

          if LIndex = LStep.AsInteger then
          begin
            LItems[LIndex] := Changed(AData.Item(LIndex), APath, AValue, AStep + 1);
          end;
        end;
        AData.Item(LStep.AsInteger);
        Result := NyxArray(LItems);
      end;
  else
    begin
      raise Exception.Create('Qualification path crosses a scalar');
    end;
  end;
end;

procedure Contract;
var
  LDocument: TNyxDocument;
  LClone: TNyxDocument;
  LDecoded: TNyxDocument;
  LPage: TNyxDocument;
  LReusable: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LCompanion: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LDefaults: INyxCollectionDefaults;
  LMaterialized: INyxCollectionDefaults;
  LContext: INyxCollectionContext;
  LRecipe: TNyxResourceRows;
  LSource: TNyxText;
  LBefore: TNyxText;
  LRecipePath: TNyxResourcePath;
  LDescriptor: TNyxText;
  LRoot: TNyxDataValue;
  LRefused: Boolean;
  LBytes: Integer;
  LIndex: Integer;
  {$ifndef PAS2JS}
  LOutput: TFileStream;
  LOutputRoot: String;
  LTarget: TNyxDocument;
  {$endif}
begin
  LDocument := NyxMappingWorkshop;
  LClone := nil;
  LDecoded := nil;
  LPage := nil;
  LReusable := nil;
  LParsed := nil;
  LSession := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCompanion := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LRoot := TNyxDataValue.ParseJSON(LBefore);
    LRecipePath := NyxResourcePath.Field('collections').Field('definitions').Item(0).Field('source');
    Check(LRoot.Field('version').AsInteger = 9, 'saved mappings opt into document version nine');
    Check(LRoot.Field('collections').Field('version').AsInteger = 2, 'source descriptor has its own version');
    Check(LDocument.Collections.Snapshot(NyxCollection('people')).Count = 0,
      'authored schema seed contains no copied runtime rows');
    Check(LDocument.Collections.Snapshot(NyxCollection('people')).Schema.Count = 4,
      'typed schema retains all four scalar families');
    LRecipe := LDocument.ResourceCollections.Source(NyxCollection('people'));
    Check(LRecipe.ToData.ToJSON = NyxMappingRecipe.ToData.ToJSON, 'recipe reads preserve structural paths');
    LRecipe := LRecipe.Text(NyxTextField('extra'), NyxResourcePath.Field('literal.name'));
    Check(LDocument.ResourceCollections.Source(NyxCollection('people')).Schema.Count = 4,
      'a derived recipe cannot mutate saved metadata');
    LBytes := LDocument.Collections.Snapshot(NyxCollection('people')).DataBytes +
      NyxUTF8ByteCount(NyxMappingRecipe.ToData.ToJSON) +
      LDocument.Collections.Snapshot(NyxCollection('choices')).DataBytes;
    Check(LDocument.Collections.DataBytes = LBytes, 'aggregate admission charges source metadata');
    LDescriptor := EncodeNyxCollectionDefaults(LDocument.Collections);
    LDefaults := DecodeNyxCollectionDefaults(LDescriptor, True);
    Check(NyxResourceCollections(LDefaults).Count = 1, 'explicit version-two decode retains source capability');
    LRefused := False;
    try
      LDefaults := DecodeNyxCollectionDefaults(LDescriptor);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'older collection descriptor boundary cannot silently promote sources');
    RefuseDocument(Changed(LRoot, NyxResourcePath.Field('version'), NyxData(8)).ToJSON,
      'version-eight documents reject source descriptors');

    LDecoded := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LDecoded) = LBefore, 'exact persistence is deterministic');
    Check(LDecoded.Resources.Definition(NyxResourceRef('team'), NyxDefaultLocale).Data
      .Field('batches').Item(0).Field('people').Item(0).Field('ratio').ToJSON = '1.2500',
      'saving a recipe does not normalize original JSON numeric spelling');
    LClone := LDocument.Clone;
    LClone.Collections.Define(NyxCollection('people'), NyxMappingRecipe.Schema, []);
    Check(not LClone.ResourceCollections.HasSource(NyxCollection('people')) and
      LDocument.ResourceCollections.HasSource(NyxCollection('people')),
      'explicit static replacement clears only the copied registry source');
    LClone.ResourceCollections.Define(NyxCollection('people'), NyxMappingRecipe);
    LClone.Collections.Remove(NyxCollection('people'));
    Check((LClone.ResourceCollections.Count = 0) and
      (LClone.Collections.DataBytes = LClone.Collections.Snapshot(NyxCollection('choices')).DataBytes),
      'source removal retires metadata and its payload charge together');
    LClone.Free;
    LClone := nil;

    LRefused := False;
    try
      LRecipe := TNyxResourceRows.FromData(Changed(NyxMappingRecipe.ToData,
        NyxResourcePath.Field('fields').Item(1).Field('type'), NyxData('untyped')));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore), 'unknown field families refuse atomically');
    RefuseDocument(Changed(LRoot, LRecipePath.Field('identity'), NyxArray([NyxData('missing')])).ToJSON,
      'missing identity paths refuse detached document admission');
    RefuseDocument(Changed(LRoot, LRecipePath.Field('fields').Item(0).Field('path'),
      NyxArray([NyxData('score')])).ToJSON,
      'wrong scalar family refuses complete document admission');
    RefuseDocument(Changed(LRoot, LRecipePath.Field('version'), NyxData(2)).ToJSON,
      'unknown source recipe versions refuse');
    RefuseDocument(Changed(LRoot, LRecipePath.Field('fields').Item(0).Field('type'),
      NyxData('boolean')).ToJSON,
      'schema/source family mismatch refuses');

    LMaterialized := MaterializeNyxCollectionDefaults(LDocument.Collections, LDocument.Resources,
      NyxDefaultLocale, NyxDefaultLocale);
    Check((LMaterialized.Snapshot(NyxCollection('people')).Count = 2) and
      not NyxHasResourceCollections(LMaterialized), 'explicit materialization creates independent static runtime seeds');
    LContext := NewNyxCollectionContext(LDocument.Collections, LDocument.Resources,
      NyxLocale('en-GB'), NyxDefaultLocale);
    Check(LContext.Collections.Collection(NyxCollection('people')).Snapshot.ItemAt(0)
      .GetValue(NyxTextField('name')) = TNyxText('Ada 🌙'), 'explicit initialization locale selects exact source rows');

    LSource := TNyxCodegen.Generate(LDocument);
    GPhase := 'source replay';
    Check((Pos('Result.ResourceCollections.Define(', LSource) > 0) and
      (Pos('.Identity(NyxResourcePath.Field(', LSource) > 0) and
      (Pos('.Number(NyxNumberField(', LSource) > 0),
      'crafted source describes typed behavior through fluent objects');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LCompanion);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'managed source replay reconstructs exact saved mappings');
    LParsed.Free;
    LParsed := nil;
    LWorkspace.Accept(LDocument, LSource);
    Check(LWorkspace.Render(LDocument) = LSource, 'no-op source reconciliation is stable');
    LPage := CloneNyxViewDocument(LDocument, LDocument.Pages[0]);
    LReusable := CloneNyxViewDocument(LDocument, LDocument.Components[0]);
    Check(LPage.ResourceCollections.HasSource(NyxCollection('people')) and
      LReusable.ResourceCollections.HasSource(NyxCollection('people')),
      'page and reusable view builds retain source recipes independently');

    LSession := TNyxStudioSession.Create(NyxProjectPair(LBefore, LSource));
    GPhase := 'paired history';
    LClone := LDocument.Clone;
    LClone.Resources.Define(NyxResourceRef('team'), NyxJSONResource(NyxMappingUpdated));
    LSession.AdoptProject(NyxProjectPair(TNyxCodec.Encode(LClone), LWorkspace.Render(LClone)));
    Check(LSession.Document.ResourceCollections.HasSource(NyxCollection('people')),
      'paired source admission retains mappings');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LSource),
      'one paired Undo restores exact mapping/source baseline');
    LSession.Redo;
    Check(LSession.Document.Resources.Definition(NyxResourceRef('team'), NyxDefaultLocale)
      .Data.ToJSON = TNyxDataValue.ParseJSON(NyxMappingUpdated).ToJSON, 'paired Redo retains exact updated source data');
    LSource := LSession.Save;
    LRefused := False;
    try
      LSession.DefineCollection(NyxCollection('people'), NyxMappingRecipe.Schema, []);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Save = LSource), 'ordinary static collection editor cannot silently erase a recipe');

    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      for LIndex := 0 to 2 do
      begin
        case LIndex of
          0:
            begin
              LTarget := LDocument;
              LOutputRoot := ParamStr(1) + '/full';
            end;
          1:
            begin
              LTarget := LPage;
              LOutputRoot := ParamStr(1) + '/page';
            end;
          2:
            begin
              LTarget := LReusable;
              LOutputRoot := ParamStr(1) + '/reusable';
            end;
        end;
        ForceDirectories(LOutputRoot);
        LSource := TNyxCodegen.Generate(LTarget);
        LOutput := TFileStream.Create(LOutputRoot + '/nyx.generated.view.pas', fmCreate);
        try
          LOutput.WriteBuffer(LSource[1], Length(LSource));
        finally
          LOutput.Free;
        end;
      end;
    end;
    {$endif}
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'source/history/build qualification retains authored baseline');
  finally
    LSession.Free;
    LParsed.Free;
    LReusable.Free;
    LPage.Free;
    LDecoded.Free;
    LClone.Free;
    LWorkspace.Free;
    LCompanion.Free;
    LDocument.Free;
  end;
end;

procedure Cell(ARenderer: TRenderer; const AID, AExpected: TNyxText);
begin
  {$ifdef PAS2JS}
  Check(ARenderer.ElementFor(AID).querySelectorAll('[role="gridcell"]')[0].textContent =
    AExpected, 'browser table paints source value: ' + AID);
  {$else}
  Check(TNyxText(RawByteString(TStringGrid(ARenderer.ControlFor(AID)).Cells[0, 1])) =
    AExpected, 'actual native table paints source value: ' + AID);
  {$endif}
end;

procedure Controls;
var
  LDocument: TNyxDocument;
  LRenderer: TRenderer;
  LHost: THost;
  LResources: INyxResources;
  LBefore: TNyxText;
  LRefused: Boolean;
  LView: INyxCollectionView;
  LFirst: INyxCollectionView;
  LSecond: INyxCollectionView;
  LRetained: INyxCollection;
  LApp: TTestApplication;
  LOther: TTestApplication;
  LLocalized: TTestApplication;
begin
  LDocument := NyxMappingWorkshop;
  LRenderer := TRenderer.Create;
  LApp := nil;
  LOther := nil;
  LLocalized := nil;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 900, 700);
  {$endif}
  try
    LBefore := TNyxCodec.Encode(LDocument);
    GPhase := 'standalone mount';
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LView := LRenderer.CollectionView('people-table');
    LFirst := LRenderer.CollectionView('first-card/card-table');
    LSecond := LRenderer.CollectionView('second-card/card-table');
    Cell(LRenderer, 'people-table', 'Ada');
    Cell(LRenderer, 'first-card/card-table', 'Ada');
    Cell(LRenderer, 'second-card/card-table', 'Ada');
    Check((LView.Store <> LFirst.Store) and (LFirst.Store <> LSecond.Store),
      'saved recipes seed independent application and reusable scopes');
    LFirst.Edit(NyxItem(NyxCollection('people'), 'ada'), 0, TNyxStateValue.FromText('Local reusable edit'));
    Cell(LRenderer, 'first-card/card-table', 'Local reusable edit');
    Cell(LRenderer, 'second-card/card-table', 'Ada');
    Cell(LRenderer, 'people-table', 'Ada');
    LRetained := LFirst.Store;
    LResources := LDocument.Resources.Clone;
    LResources.Define(NyxResourceRef('team'), NyxJSONResource(
      '{"batches":[{"people":[{"key":["ada"],"literal.name":"Rejected","score":10,"ready":"false","ratio":1.5}]}]}'));
    LResources.Define(NyxResourceRef('copy'), NyxTextResource('Must not publish'));
    LRefused := False;
    try
      LRenderer.ReloadResources(LResources, NyxDefaultLocale, NyxDefaultLocale);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'wrong typed row family refuses the whole prepared frame');
    Cell(LRenderer, 'people-table', 'Ada');
    {$ifdef PAS2JS}
    Check(LRenderer.ElementFor('headline').textContent = 'Build for tomorrow',
      'refused reload preserves caption');
    {$else}
    Check(TLabel(LRenderer.ControlFor('headline')).Caption = 'Build for tomorrow',
      'refused reload preserves caption');
    {$endif}
    LResources := LDocument.Resources.Clone;
    LResources.Define(NyxResourceRef('copy'), NyxTextResource('Ready for tomorrow 🌙'));
    GPhase := 'unchanged source reload';
    LRenderer.ReloadResources(LResources, NyxDefaultLocale, NyxDefaultLocale);
    Cell(LRenderer, 'first-card/card-table', 'Local reusable edit');
    {$ifdef PAS2JS}
    Check(LRenderer.ElementFor('headline').textContent = 'Ready for tomorrow 🌙',
      'unrelated resource replacement remains usable');
    {$else}
    Check(TNyxText(RawByteString(TLabel(LRenderer.ControlFor('headline')).Caption)) =
      TNyxText('Ready for tomorrow 🌙'), 'unrelated resource replacement remains usable');
    {$endif}
    LResources.Define(NyxResourceRef('team'), NyxJSONResource(NyxMappingUpdated));
    LRenderer.ReloadResources(LResources, NyxDefaultLocale, NyxDefaultLocale);
    Cell(LRenderer, 'people-table', 'Ada 🌙');
    Cell(LRenderer, 'first-card/card-table', 'Ada 🌙');
    Cell(LRenderer, 'second-card/card-table', 'Ada 🌙');
    Check((LView.Store = LRenderer.CollectionView('people-table').Store) and
      (LFirst.Store = LRenderer.CollectionView('first-card/card-table').Store),
      'changed source publication retains existing stores and control bindings');
    LRenderer.Unmount;
    Check(LRetained.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = TNyxText('Ada 🌙'),
      'runtime store survives target/token retirement independently');
    LRenderer.ResourceContext := NewNyxResourceContext(LDocument.Resources,
      NyxLocale('en-GB'), NyxDefaultLocale);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Cell(LRenderer, 'people-table', 'Ada 🌙');
    Check(LRenderer.CollectionView('people-table').Store.Snapshot.ItemAt(0)
      .GetValue(NyxNumberField('ratio')) = 1.5,
      'standalone source initialization uses the accepted resource frame');
    LRenderer.Unmount;
    LRenderer.ResourceContext := nil;

    LApp := TTestApplication.Create;
    GPhase := 'application mount';
    LOther := TTestApplication.Create;
    LApp.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand));
    LOther.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand));
    {$ifdef PAS2JS}
    LApp.Run(LDocument, LHost);
    LOther.Run(LDocument, TJSHTMLElement(document.createElement('div')));
    {$else}
    LApp.Mount(LDocument);
    LOther.Mount(LDocument);
    LApp.Window.Show;
    Application.ProcessMessages;
    {$endif}
    Cell(LApp.View, 'people-table', 'Ada');
    LApp.View.CollectionView('people-table').Edit(NyxItem(NyxCollection('people'), 'ada'),
      0, TNyxStateValue.FromText('Ada 🌙'));
    Cell(LOther.View, 'people-table', 'Ada');
    LApp.ShowPage('details');
    Cell(LApp.View, 'detail-table', 'Ada 🌙');
    LApp.ShowPage('home');
    Cell(LApp.View, 'people-table', 'Ada 🌙');
    Cell(LApp.View, 'first-card/card-table', 'Ada');
    GPhase := 'coordinated application locale';
    LApp.Resources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
    Check(LApp.Resources.Context.Locale.Name = 'en-GB',
      'locale and saved source rows share the accepted application frame');
    Cell(LApp.View, 'people-table', 'Ada 🌙');
    Cell(LApp.View, 'first-card/card-table', 'Ada 🌙');
    Cell(LApp.View, 'second-card/card-table', 'Ada 🌙');
    Check(LApp.Collections.Collection(NyxCollection('people')).Snapshot.ItemAt(0)
      .GetValue(NyxIntegerField('score')) = 10, 'mapped numeric row values reload with the locale');
    Cell(LOther.View, 'people-table', 'Ada');
    LApp.ShowPage('details');
    Cell(LApp.View, 'detail-table', 'Ada 🌙');
    LLocalized := TTestApplication.Create;
    LLocalized.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand)
      .Localize(NyxLocale('en-GB'), NyxDefaultLocale));
    {$ifdef PAS2JS}
    LLocalized.Run(LDocument, TJSHTMLElement(document.createElement('div')));
    {$else}
    LLocalized.Mount(LDocument);
    {$endif}
    Cell(LLocalized.View, 'people-table', 'Ada 🌙');
    Cell(LLocalized.View, 'first-card/card-table', 'Ada 🌙');
    LLocalized.ShowPage('details');
    Cell(LLocalized.View, 'detail-table', 'Ada 🌙');
    Check(LLocalized.Resources.Context.Locale.Name = 'en-GB',
      'initial application locale seeds ordinary and instance datasets before validation');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'mounted runtime edits/navigation leave exact saved schema/resources unchanged');
  finally
    LLocalized.Free;
    LOther.Free;
    LApp.Free;
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LDocument.Free;
  end;
end;

procedure Semantic;
var
  LDocument: TNyxDocument;
  LSeed: TNyxProjectPair;
  LResult: TNyxDataValue;
  LRevision: Integer;
  LOriginal: TNyxText;
  LResultRevision: Integer;
  LImportedRows: TNyxResourceRows;
  LDescriptor: TNyxDataValue;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LAgent: TNyxAgentSession;
  {$else}
  LEngine: TNyxStudioMCP;
  LProfile: TNyxOutputConfiguration;
  LRuntime: TNyxText;
  LToken: TNyxText;
  {$endif}

  function Call(const ATool: TNyxText; const AArgs: TNyxDataValue): TNyxDataValue;
  begin
    {$ifdef PAS2JS}
    Result := LAgent.Call(ATool, 'Scooty', AArgs, 'mapping-owner');
    {$else}
    Result := LEngine.InvokeTool(ATool, 'mapping-owner', 'Scooty', AArgs);
    {$endif}
  end;

  function Pair: TNyxProjectPair;
  var
    LObserved: TNyxDataValue;
  begin
    {$ifdef PAS2JS}
    LObserved := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))]));
    {$else}
    LObserved := LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))]));
    {$endif}
    Result := DecodeNyxProject(LObserved.Field('project').AsText);
  end;

begin
  LDocument := NyxMappingWorkshop;
  try
    LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
  {$ifdef PAS2JS}
  LAgent := TNyxAgentSession.Create(LSeed);
  {$else}

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Supply emitted source root and a new private semantic runtime');
  end;
  LRuntime := ExpandFileName(ParamStr(2));

  if DirectoryExists(LRuntime) then
  begin
    raise Exception.Create('Semantic runtime must be new and independently owned');
  end;
  LProfile := TNyxOutputConfiguration.Create;
  try
    { Actual public MCP dispatch in a fresh suspended engine. Never Start:
      no listener/browser/replacement or authenticated rollout is inferred. }
    LEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(LRuntime),
      8640, 8641, LProfile.Encode);
  finally
    LProfile.Free;
  end;
  {$endif}
  try
    {$ifndef PAS2JS}
    LResult := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LSeed))),
      NyxField('selection', NyxData('people-table')), NyxField('view', NyxData('home'))]));
    LToken := LResult.Field('token').AsText;
    {$endif}
    LRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
    LOriginal := EncodeNyxProject(Pair);
    LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('json')),
      NyxField('name', NyxData('team')), NyxField('locale', NyxData('')),
      NyxField('path', NyxResourcePath.Field('batches').Item(0).Field('people').Item(0)
        .Field('literal.name').ToData)]));
    Check(Pos('Ada', LResult.ToJSON) > 0, 'bounded semantic query reads source data without a screenshot');
    LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('replace-team')),
      NyxField('changes', NyxResourcePatch([
        NyxDefineResource(NyxResourceRef('team'), NyxDefaultLocale, NyxJSONResource(NyxMappingUpdated)),
        NyxDefineResource(NyxResourceRef('copy'), NyxDefaultLocale, NyxTextResource('Ready to build'))]).ToData)]));
    LRevision := LResult.Field('revision').AsInteger;
    LDocument := TNyxCodec.Decode(Pair.Design);
    try
      Check(LDocument.ResourceCollections.HasSource(NyxCollection('people')) and
        (MaterializeNyxCollectionDefaults(LDocument.Collections, LDocument.Resources,
          NyxDefaultLocale, NyxDefaultLocale).Snapshot(NyxCollection('people')).ItemAt(0)
          .GetValue(NyxTextField('name')) = TNyxText('Ada 🌙')),
        'one semantic resource group retains saved recipe and its new admitted dataset');
    finally
      LDocument.Free;
    end;
    LResult := Call('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('undo-team'))]));
    Check(EncodeNyxProject(Pair) = LOriginal, 'semantic paired Undo restores exact source and design');
    LResultRevision := LResult.Field('revision').AsInteger;
    LDescriptor := NyxMappingRecipe.ToData;
    LImportedRows := TNyxResourceRows.FromData(NyxObject([NyxField('version', NyxData(1)),
      NyxField('resource', NyxData('imported-team')), NyxField('path', LDescriptor.Field('path')),
      NyxField('identity', LDescriptor.Field('identity')), NyxField('fields', LDescriptor.Field('fields'))]));

    LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('sources')),
      NyxField('filter', NyxData('people')), NyxField('limit', NyxData(1))]));
    Check((LResult.Field('total').AsInteger = 1) and (LResult.Field('sources').Count = 1),
      'semantic relationship list is filtered and bounded');
    LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('rows')),
      NyxField('collection', NyxData('people')), NyxField('offset', NyxData(1)),
      NyxField('limit', NyxData(1))]));
    Check((LResult.Field('fields').Count = 1) and (LResult.Field('nextOffset').AsInteger = 2) and
      (LResult.Field('fields').Item(0).Field('type').AsText = 'integer') and
      not LResult.Field('runtimeRowsIncluded').AsBoolean,
      'semantic schema page keeps exact family/path and distinguishes runtime data');

    LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LResultRevision)), NyxField('operationId', NyxData('admit-linked-source')),
      NyxField('changes', NyxResourcePatch([
        NyxDefineResourceRows(NyxCollection('people'), LImportedRows),
        NyxDefineResource(NyxResourceRef('imported-team'), NyxDefaultLocale, NyxJSONResource(NyxMappingUpdated))
      ]).ToData)]));
    LResultRevision := LResult.Field('revision').AsInteger;
    LDocument := TNyxCodec.Decode(Pair.Design);
    try
      Check((LDocument.ResourceCollections.Source(NyxCollection('people')).Reference.Name = 'imported-team') and
        (LDocument.Collections.Snapshot(NyxCollection('people')).Count = 0),
        'one semantic group admits a recipe before its related resource without copying runtime rows');
      Check(LDocument.Find('people-table').HasCollectionView,
        'semantic source replacement retains the original table contract');
    finally
      LDocument.Free;
    end;
    LResult := Call('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(LResultRevision)), NyxField('operationId', NyxData('undo-linked-source'))]));
    LResultRevision := LResult.Field('revision').AsInteger;
    Check(EncodeNyxProject(Pair) = LOriginal, 'linked source group has one exact paired Undo');

    LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LResultRevision)), NyxField('operationId', NyxData('detach-people')),
      NyxField('changes', NyxResourcePatch([NyxDetachResourceRows(NyxCollection('people'))]).ToData)]));
    LResultRevision := LResult.Field('revision').AsInteger;
    LDocument := TNyxCodec.Decode(Pair.Design);
    try
      Check(not LDocument.ResourceCollections.HasSource(NyxCollection('people')) and
        (LDocument.Collections.Snapshot(NyxCollection('people')).Count = 2) and
        (LDocument.Collections.Snapshot(NyxCollection('people')).ItemAt(0).Ref.ID = 'ada'),
        'semantic detach preserves authored default rows and identities');
      Check(LDocument.Find('people-table').HasCollectionView and LDocument.Find('card-table').HasCollectionView,
        'semantic detach keeps application and reusable control bindings');
    finally
      LDocument.Free;
    end;
    LResult := Call('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(LResultRevision)), NyxField('operationId', NyxData('undo-detach-people'))]));
    LResultRevision := LResult.Field('revision').AsInteger;
    Check(EncodeNyxProject(Pair) = LOriginal, 'one paired Undo restores the exact saved relationship');

    LRefused := False;
    try
      Call('nyx_resources', NyxObject([NyxField('mode', NyxData('apply')),
        NyxField('expectedRevision', NyxData(LResultRevision)), NyxField('operationId', NyxData('static-refusal')),
        NyxField('changes', NyxResourcePatch([NyxDefineResourceRows(NyxCollection('choices'),
          NyxMappingRecipe)]).ToData)]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(Pair) = LOriginal),
      'static conversion refuses without explicit consent and preserves source/design');
    LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LResultRevision)), NyxField('operationId', NyxData('static-conversion')),
      NyxField('changes', NyxResourcePatch([NyxDefineResourceRows(NyxCollection('choices'),
        NyxMappingRecipe, True)]).ToData)]));
    LResultRevision := LResult.Field('revision').AsInteger;
    LDocument := TNyxCodec.Decode(Pair.Design);
    try
      Check(LDocument.ResourceCollections.HasSource(NyxCollection('choices')),
        'explicit Boolean consent converts an existing static collection');
    finally
      LDocument.Free;
    end;
    LResult := Call('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(LResultRevision)), NyxField('operationId', NyxData('undo-static-conversion'))]));
    LResultRevision := LResult.Field('revision').AsInteger;
    Check(EncodeNyxProject(Pair) = LOriginal, 'static conversion preserves paired rollback');

    LRefused := False;
    try
      Call('nyx_resources', NyxObject([NyxField('mode', NyxData('apply')),
        NyxField('expectedRevision', NyxData(LResultRevision - 1)), NyxField('operationId', NyxData('stale-source')),
        NyxField('changes', NyxResourcePatch([NyxDetachResourceRows(NyxCollection('people'))]).ToData)]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(Pair) = LOriginal), 'stale source mutation refuses atomically');
  finally
    {$ifdef PAS2JS}LAgent.Free;{$else}LEngine.Free;{$endif}
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    GPhase := 'contract';
    Contract;
    GPhase := 'controls';
    Controls;
    GPhase := 'semantic';
    Semantic;
    WriteLn('PASS / saved resource mappings / ', GChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', GPhase, ' / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');{$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
