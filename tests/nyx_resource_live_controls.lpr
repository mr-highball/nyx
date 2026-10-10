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

program nyx_resource_live_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.controls, nyx.codec, nyx.state, nyx.binding.types, nyx.publication, nyx.resources,
  nyx.resource.sources, nyx.resource.cache, nyx.resources.loader,
  nyx.resource.context, nyx.application.resources, nyx.application.state, nyx.collections,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.query,
  nyx.collections.registry, nyx.resources.rows, nyx.resource.mapping.fixture
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, StdCtrls, nyx.application.lcl{$endif};

type
  TTestApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  TReplyRequest = class(TNyxResourceRequest)
  public
    procedure Reply(const AValue: TNyxText);
  end;
  { Deterministic byte transport; the real resolver and application UI courier
    still perform typed HTTP-result/cache/catalog admission. No listener starts. }
  TTransport = class(TInterfacedObject, INyxResourceTransport)
  private
    FTokens: array of INyxResourceRequest;
    FRequests: array of TReplyRequest;
  public
    function Request(const AURL: TNyxResourceURL; const AOptions: TNyxResourceLoadOptions;
      AMaximumBytes: Integer; AReply: TNyxResourceHTTPReply): INyxResourceRequest;
    procedure Send(AIndex: Integer; const AValue: TNyxText);
    function Count: Integer;
  end;

  { All callbacks are borrowed and explicitly disconnected before destruction.
    Managed snapshots, views and resource frames contain no application back-link. }
  TJourney = class
  private
    FDocument: TNyxDocument;
    FBefore: TNyxText;
    FApplication: TTestApplication;
    FOther: TTestApplication;
    FTransport: TTransport;
    FTransportLease: INyxResourceTransport;
    FResources: INyxApplicationResources;
    FView: INyxCollectionView;
    FFirst: INyxCollectionView;
    FSecond: INyxCollectionView;
    FResourceToken: INyxResourceSubscription;
    FFailureToken: INyxResourceSubscription;
    FRetireToken: INyxResourceSubscription;
    FRowToken: INyxCollectionSubscription;
    FInstanceToken: INyxCollectionSubscription;
    FRejectToken: INyxCollectionSubscription;
    FExpectedName: TNyxText;
    FExpectedHeadline: TNyxText;
    FObserve: Boolean;
    FBusy: Boolean;
    FRejectRows: Boolean;
    FResourceCalls: Integer;
    FRowCalls: Integer;
    FChecks: Integer;
    FStage: Integer;
    FFinished: Boolean;
    FStartedAt: {$ifdef PAS2JS}Double{$else}QWord{$endif};
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Mount(AApplication: TTestApplication);
    function Admit(const AContext: INyxResourceContext): Boolean;
    procedure Changed(const AContext: INyxResourceContext);
    procedure RowsChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
    procedure RejectRows(const AData: INyxCollectionSnapshot; const AChanges: INyxCollectionChanges);
    procedure FailedReceiver(const AContext: INyxResourceContext);
    procedure RetireHost(const AContext: INyxResourceContext);
    procedure Coherent;
    procedure Cell(const AID: TNyxText; const AExpected: TNyxText);
    function Caption: TNyxText;
    { Reads the actual label and input in both reusable instances. No model or
      store read substitutes for target publication; the application is borrowed
      only for this call and no control survives navigation or host retirement. }
    procedure ReusedText(AApplication: TTestApplication; const AHeadline: TNyxText);
    { Refuses an invalid row identity frame and inspects all still-mounted table,
      caption and prompt consumers. Snapshot/notification counts stay unchanged. }
    procedure RefuseIdentity(const ALocale: TNyxText);
    { Exercises exact runtime locale/fallback selection on mounted controls.
      Changing only the requested locale must retain an instance's local edits
      when it still resolves the same admitted source variant. }
    procedure LocaleFallbacks;
    procedure LocaleJourneys;
    procedure LateScopes;
    procedure InitialAttachment;
    function DeclinePreparation(const AContext: INyxResourceContext;
      out APrepared: INyxPreparedPublication): Boolean;
    procedure Advance;
    procedure Finish;
  public
    destructor Destroy; override;
    procedure Start;
    procedure Next;
    property Finished: Boolean read FFinished;
  end;

function Payload(const AHeadline: TNyxText; AMaximum: Integer;
  const ARows: TNyxText): TNyxText;
begin
  Result := NyxObject([NyxField('headline', NyxData(AHeadline)),
    NyxField('prompt', NyxData(TNyxText('Project name / ') + AHeadline)),
    NyxField('maximum', NyxData(AMaximum)),
    NyxField('batches', TNyxDataValue.ParseJSON(ARows).Field('batches'))]).ToJSON;
end;

function NyxAtomicBusy(const AStore: INyxCollection): Boolean;
var
  LAtomic: INyxAtomicCollection;
begin

  if not Supports(AStore, INyxAtomicCollection, LAtomic) then
  begin
    raise ENyxCollection.Create('Expected a prepared collection store');
  end;
  Result := LAtomic.Busy;
end;

function Workshop: TNyxDocument;
var
  LLimit: INyxSlider;
begin
  Result := NyxMappingWorkshop;
  try
    Result.State.SetValue(NyxIntegerState('count'), 2);
    Result.Resources.Define(NyxResourceRef('team'),
      NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/team.json'))
        .Cache(NyxResourceCache.Bypass)
        .Fallback(NyxJSONResource(Payload('Team workbench', 10, NyxMappingInitial))));
    Result.Resources.Define(NyxResourceRef('team'), NyxLocale('en-GB'),
      NyxJSONResource(Payload('A shared workbench 🌙', 20, NyxMappingUpdated)));
    Result.Resources.Define(NyxResourceRef('team'), NyxLocale('short'),
      NyxJSONResource(Payload('Rejected hidden constraint', 1, NyxMappingUpdated)));
    Result.Resources.Define(NyxResourceRef('team'), NyxLocale('malformed'),
      NyxJSONResource(Payload('Rejected row type', 20,
        '{"batches":[{"people":[{"key":["ada"],"literal.name":"Rejected","score":10,"ready":"false","ratio":1.5}]}]}')));
    { Rows, headline and numeric bounds are valid in these frames. Only the
      reusable prompt consumer rejects them, proving its typed selector takes
      part in whole-application admission rather than failing after publication. }
    Result.Resources.Define(NyxResourceRef('team'), NyxLocale('prompt-type'),
      NyxJSONResource(NyxObject([
        NyxField('headline', NyxData('Rejected prompt type')),
        NyxField('prompt', NyxData(True)),
        NyxField('maximum', NyxData(20)),
        NyxField('batches', TNyxDataValue.ParseJSON(NyxMappingUpdated).Field('batches'))]).ToJSON));
    Result.Resources.Define(NyxResourceRef('team'), NyxLocale('prompt-path'),
      NyxJSONResource(NyxObject([
        NyxField('headline', NyxData('Rejected missing prompt')),
        NyxField('Prompt', NyxData('Wrong-case field must not appear')),
        NyxField('maximum', NyxData(20)),
        NyxField('batches', TNyxDataValue.ParseJSON(NyxMappingUpdated).Field('batches'))]).ToJSON));
    { Invalid row identity is rejected before scalar or any reusable control
      paints. These payloads otherwise satisfy every consumer's field family. }
    Result.Resources.Define(NyxResourceRef('team'), NyxLocale('duplicate-row'),
      NyxJSONResource(Payload('Rejected duplicate identity', 20,
        '{"batches":[{"people":[' +
        '{"key":["ada"],"literal.name":"Ada","score":10,"ready":true,"ratio":1.5},' +
        '{"key":["ada"],"literal.name":"Another Ada","score":8,"ready":false,"ratio":2.0}]}]}')));
    Result.Resources.Define(NyxResourceRef('team'), NyxLocale('identity-path'),
      NyxJSONResource(Payload('Rejected missing identity', 20,
        '{"batches":[{"people":[' +
        '{"literal.name":"Ada","score":10,"ready":true,"ratio":1.5}]}]}')));
    Result.Pages[0].Find('headline').Binds.Text(
      NyxResourceValue(NyxResourceRef('team')).Field('headline')).Done;
    Result.Find('card-guide').Binds.Text(
      NyxResourceValue(NyxResourceRef('team')).Field('headline')).Done;
    Result.Find('card-prompt').Binds.Placeholder(
      NyxResourceValue(NyxResourceRef('team')).Field('prompt')).Done;
    LLimit := NewNyxSlider('count');
    LLimit.Configure.Minimum(0).Maximum(10).Done;
    LLimit.Binds.Value(NyxIntegerState('count'))
      .Maximum(NyxResourceValue(NyxResourceRef('team')).Field('maximum').AsInteger).Done;
    Result.Pages[1].Add(LLimit.Node);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure TReplyRequest.Reply(const AValue: TNyxText);
var
  LResponse: TNyxResourceHTTPResult;
begin
  LResponse := Default(TNyxResourceHTTPResult);
  LResponse.Status := 200;
  LResponse.Bytes := NyxEncodeUTF8(AValue);
  LResponse.Hints := NyxResourceCacheHeaders('', '');
  Complete(LResponse);
end;

function TTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LIndex: Integer;
  LRequest: TReplyRequest;
begin
  LRequest := TReplyRequest.Create(AReply);
  Result := LRequest;
  LIndex := Length(FTokens);
  SetLength(FTokens, LIndex + 1);
  SetLength(FRequests, LIndex + 1);
  FTokens[LIndex] := Result;
  FRequests[LIndex] := LRequest;
end;

procedure TTransport.Send(AIndex: Integer; const AValue: TNyxText);
begin
  FRequests[AIndex].Reply(AValue);
end;

function TTransport.Count: Integer;
begin
  Result := Length(FTokens);
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Live resource controls: ' + AReason);
  end;
  Inc(FChecks);
end;

procedure TJourney.Mount(AApplication: TTestApplication);
{$ifdef PAS2JS}
var
  LHost: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  AApplication.Run(FDocument, LHost);
  {$else}
  AApplication.Mount(FDocument);
  AApplication.Window.Show;
  {$endif}
end;

procedure TJourney.Cell(const AID: TNyxText; const AExpected: TNyxText);
{$ifdef PAS2JS}
var
  LCell: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LText: TNyxText;
{$endif}
begin
  {$ifdef PAS2JS}
  LCell := TJSHTMLElement(FApplication.View.ElementFor(AID).querySelector('[role="gridcell"]'));
  LInput := TJSHTMLInputElement(LCell.querySelector('input'));
  { The live journey includes editable columns. Their visible value belongs to
    the actual input, while read-only cells expose text. Reading the store here
    would conceal a missed target publication, so inspect the mounted element. }

  if LInput <> nil then
  begin
    LText := LInput.value;
  end
  else
  begin
    LText := LCell.textContent;
  end;
  Check(LText = AExpected, 'browser cell: ' + AID);
  {$else}
  Check(TNyxText(RawByteString(TStringGrid(FApplication.View.ControlFor(AID))
    .Cells[0, 1])) = AExpected, 'actual native cell: ' + AID);
  {$endif}
end;

function TJourney.Caption: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := FApplication.View.ElementFor('headline').textContent;
  {$else}
  Result := RawByteString(TLabel(FApplication.View.ControlFor('headline')).Caption);
  {$endif}
end;

procedure TJourney.ReusedText(AApplication: TTestApplication; const AHeadline: TNyxText);
var
  LInstance: TNyxText;
  LIndex: Integer;
  LCaption: TNyxText;
  LPrompt: TNyxText;
begin
  for LIndex := 0 to 1 do
  begin

    if LIndex = 0 then
    begin
      LInstance := 'first-card/';
    end
    else
    begin
      LInstance := 'second-card/';
    end;
    {$ifdef PAS2JS}
    LCaption := AApplication.View.ElementFor(LInstance + 'card-guide').textContent;
    LPrompt := TJSHTMLInputElement(AApplication.View.InputFor(LInstance + 'card-prompt')).placeholder;
    {$else}
    LCaption := RawByteString(TLabel(AApplication.View.ControlFor(LInstance + 'card-guide')).Caption);
    LPrompt := RawByteString(TEdit(AApplication.View.InputFor(LInstance + 'card-prompt')).TextHint);
    {$endif}
    Check(LCaption = AHeadline, 'actual reusable caption: ' + LInstance);
    Check(LPrompt = TNyxText('Project name / ') + AHeadline,
      'actual reusable prompt: ' + LInstance);
  end;
end;

function TJourney.Admit(const AContext: INyxResourceContext): Boolean;
begin
  Result := not FBusy;
end;

function TJourney.DeclinePreparation(const AContext: INyxResourceContext;
  out APrepared: INyxPreparedPublication): Boolean;
begin
  APrepared := nil;
  Result := False;
end;

procedure TJourney.Coherent;
begin

  if not FObserve then
  begin
    Exit;
  end;
  Check(FResources.Context.Snapshot.Resolve(NyxResourceRef('team'),
    FResources.Context.Locale, FResources.Context.Fallback).Data
    .Field('headline').AsText = FExpectedHeadline, 'observer sees accepted catalog');

  if FApplication <> nil then
  begin
    Check(FApplication.View.ResourceContext.Locale.Name = FResources.Context.Locale.Name,
      'renderer exposes accepted locale before notifications');
    Check(FApplication.View.Root.Find('headline').Prop('text') = FExpectedHeadline,
      'observer sees accepted scalar model before target paint');
    Check(FApplication.View.Root.Find('first-card/card-guide').Prop(
      NyxBindingPropertyName(bpText)) = FExpectedHeadline,
      'observer sees accepted first reusable caption before target paint');
    Check(FApplication.View.Root.Find('second-card/card-guide').Prop(
      NyxBindingPropertyName(bpText)) = FExpectedHeadline,
      'observer sees accepted last reusable caption before target paint');
    Check(FApplication.View.Root.Find('first-card/card-prompt').Prop(NyxBindingPropertyName(bpPlaceholder)) =
      TNyxText('Project name / ') + FExpectedHeadline,
      'observer sees accepted first reusable prompt before target paint');
    Check(FApplication.View.Root.Find('second-card/card-prompt').Prop(NyxBindingPropertyName(bpPlaceholder)) =
      TNyxText('Project name / ') + FExpectedHeadline,
      'observer sees accepted last reusable prompt before target paint');
  end;
  Check(FView.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = FExpectedName,
    'observer sees installed application projection');
  Check(FFirst.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = FExpectedName,
    'observer sees installed first instance projection');
  Check(FSecond.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = FExpectedName,
    'observer sees installed last instance projection');
end;

procedure TJourney.Changed(const AContext: INyxResourceContext);
var
  LRefused: Boolean;
begin
  Inc(FResourceCalls);
  Coherent;

  if not FObserve then
  begin
    Exit;
  end;
  LRefused := False;
  try
    FApplication.State.SetValue(NyxIntegerState('count'), 3);
  except
    on ENyxState do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'snapshot hold rejects reentrant state writes');
  LRefused := False;
  try
    FApplication.ShowPage('details');
  except
    on ENyxState do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'prepared application rejects reentrant navigation');
  LRefused := False;
  try
    {$ifdef PAS2JS}
    FApplication.View.Render(FDocument, FDocument.Pages[0], TJSHTMLElement(document.body));
    {$else}
    FApplication.View.Render(FDocument, FDocument.Pages[0], FApplication.Window);
    {$endif}
  except
    on ENyxState do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'direct view replacement rejects prepared resource reentry');
  LRefused := False;
  try
    FView.ClearSelection;
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'mapped view selection refuses until the complete group retires');
  LRefused := False;
  try
    FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'catalog owner rejects reentrant localization');
end;

procedure TJourney.RowsChanged(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(FRowCalls);
  Coherent;
end;

procedure TJourney.RejectRows(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin

  if FRejectRows then
  begin
    Check(FView.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) =
      TNyxText('Ada 🌙'), 'later validator sees accepted earlier application rows');
    raise ENyxCollection.Create('Last reusable receiver refuses the candidate');
  end;
end;

procedure TJourney.FailedReceiver(const AContext: INyxResourceContext);
begin
  { Empty exception text must still report committed observer failure. }
  raise ENyxResource.Create('');
end;

procedure TJourney.RetireHost(const AContext: INyxResourceContext);
begin
  { Frame callbacks run before row/paint notifications. Their stages must safely
    retire borrowed receivers when this host destroys models/state/controls. }
  FreeAndNil(FApplication);
end;

procedure TJourney.RefuseIdentity(const ALocale: TNyxText);
var
  LRefused: Boolean;
  LRevision: Integer;
  LResourceCalls: Integer;
  LRowCalls: Integer;
begin
  LRefused := False;
  LRevision := FView.Store.Snapshot.Revision;
  LResourceCalls := FResourceCalls;
  LRowCalls := FRowCalls;
  try
    FResources.Localize(NyxLocale(ALocale), NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'invalid row identity refuses the complete frame: ' + ALocale);
  Check((FResources.Context.Locale.Name = 'en-GB') and
    (FView.Store.Snapshot.Revision = LRevision), 'identity refusal retains accepted frame/revision');
  Check((FResourceCalls = LResourceCalls) and (FRowCalls = LRowCalls),
    'identity refusal publishes no resource or row notification');
  Cell('people-table', 'Ada 🌙');
  Cell('first-card/card-table', 'Ada 🌙');
  Cell('second-card/card-table', 'Ada 🌙');
  Check(Caption = FExpectedHeadline, 'identity refusal retains actual application caption');
  ReusedText(FApplication, FExpectedHeadline);
  ReusedText(FOther, 'Team workbench');
end;

procedure TJourney.LocaleFallbacks;
var
  LFirst: INyxCollectionView;
  LRefused: Boolean;
  LBefore: INyxResourceContext;

  procedure Frame(const ALocale, AFallback: TNyxLocaleRef;
    const AHeadline, AName, AFirstName: TNyxText);
  begin
    Check((FResources.Context.Locale.Name = ALocale.Name) and
      (FResources.Context.Fallback.Name = AFallback.Name),
      'runtime retains exact selected/fallback locale names');
    Check(Caption = AHeadline, 'actual application caption follows the resolved locale');
    Cell('people-table', AName);
    Cell('first-card/card-table', AFirstName);
    Cell('second-card/card-table', AName);
    ReusedText(FApplication, AHeadline);
    ReusedText(FOther, 'Team workbench');
    Check(FApplication.State.GetValue(NyxIntegerState('count')) = 2,
      'locale selection preserves independent accepted application state');
    Check(TNyxCodec.Encode(FDocument) = FBefore,
      'locale selection never materializes fallback variants into authored defaults');
  end;

begin
  FResources.Localize(NyxLocale('en-US'), NyxLocale('en-GB'));
  Frame(NyxLocale('en-US'), NyxLocale('en-GB'), 'A shared workbench 🌙', 'Ada 🌙', 'Ada 🌙');
  Check(not FResources.Context.Snapshot.Contains(NyxResourceRef('team'), NyxLocale('en-US')),
    'missing selected locale is resolved without adding a translated copy');
  LFirst := FApplication.View.CollectionView('first-card/card-table');
  LFirst.Edit(NyxItem(NyxCollection('people'), 'ada'), 0,
    TNyxStateValue.FromText('A local fallback edit 🌙'));
  Cell('first-card/card-table', 'A local fallback edit 🌙');
  FResources.Localize(NyxLocale('en-AU'), NyxLocale('en-GB'));
  Frame(NyxLocale('en-AU'), NyxLocale('en-GB'), 'A shared workbench 🌙',
    'Ada 🌙', 'A local fallback edit 🌙');
  FResources.Localize(NyxLocale('en-GB'), NyxLocale('en-US'));
  Frame(NyxLocale('en-GB'), NyxLocale('en-US'), 'A shared workbench 🌙',
    'Ada 🌙', 'A local fallback edit 🌙');
  { The selected variant exists but its exact lower-case prompt field does not.
    A complete valid fallback must not silently supply that field or reset a
    local row edit after a failed whole-application admission. }
  LBefore := FResources.Context;
  LRefused := False;
  try
    FResources.Localize(NyxLocale('prompt-path'), NyxLocale('en-GB'));
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (FResources.Context = LBefore),
    'wrong-case selected field refuses despite a valid explicit fallback');
  Frame(NyxLocale('en-GB'), NyxLocale('en-US'), 'A shared workbench 🌙',
    'Ada 🌙', 'A local fallback edit 🌙');
  FResources.Localize(NyxLocale('en-gb'), NyxLocale('en-US'));
  Frame(NyxLocale('en-gb'), NyxLocale('en-US'), 'Team workbench', 'Ada', 'Ada');
  FResources.Localize(NyxLocale('en-US'), NyxLocale('en-CA'));
  Frame(NyxLocale('en-US'), NyxLocale('en-CA'), 'Team workbench', 'Ada', 'Ada');
  FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
  Frame(NyxDefaultLocale, NyxDefaultLocale, 'Team workbench', 'Ada', 'Ada');
end;

procedure TJourney.LocaleJourneys;
var
  LRefused: Boolean;
  LCalls: Integer;
  LRowCalls: Integer;
  LRevision: Integer;
  LMaximum: Double;
  LWaitToken: INyxResourceSubscription;
  LControl: {$ifdef PAS2JS}TJSHTMLElement{$else}TObject{$endif};
begin
  FView := FApplication.View.CollectionView('people-table');
  FFirst := FApplication.View.CollectionView('first-card/card-table');
  FSecond := FApplication.View.CollectionView('second-card/card-table');
  FView.Select(NyxItem(NyxCollection('people'), 'sam'));
  FFirst.ConfigureQuery(NyxCollectionQuery.Where(
    NyxWhere(NyxIntegerField('score')).GreaterThan(8)));
  FFirst.Select(NyxItem(NyxCollection('people'), 'ada'));
  FRowToken := FView.Store.Subscribe(RowsChanged);
  FInstanceToken := FFirst.Store.Subscribe(RowsChanged);
  FRejectToken := FSecond.Store.Subscribe(nil, RejectRows);
  FResourceToken := FResources.Subscribe(Admit, Changed);
  {$ifdef PAS2JS}
  LControl := FApplication.View.ElementFor('people-table');
  {$else}
  LControl := FApplication.View.ControlFor('people-table');
  {$endif}
  FObserve := True;
  FExpectedName := 'Ada 🌙';
  FExpectedHeadline := 'A shared workbench 🌙';
  FResources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
  Cell('people-table', 'Ada 🌙');
  Cell('first-card/card-table', 'Ada 🌙');
  Cell('second-card/card-table', 'Ada 🌙');
  Check(Caption = FExpectedHeadline, 'locale paints scalar caption after grouped adoption');
  ReusedText(FApplication, FExpectedHeadline);
  ReusedText(FOther, 'Team workbench');
  Check((FView.Selected.ID = 'sam') and (FFirst.Selected.ID = 'ada'),
    'stable identities preserve independent selections');
  Check((FFirst.Snapshot.Count = 1) and (FFirst.Store.Snapshot.Count = 2),
    'per-instance query projects updated rows without editing the source');
  {$ifdef PAS2JS}
  Check(LControl = FApplication.View.ElementFor('people-table'), 'locale retains DOM identity');
  {$else}
  Check(LControl = FApplication.View.ControlFor('people-table'), 'locale retains native grid identity');
  {$endif}
  Check(FOther.View.CollectionView('people-table').Snapshot.ItemAt(0)
    .GetValue(NyxTextField('name')) = 'Ada', 'sibling application retains its private runtime');
  LCalls := FResourceCalls;
  LRevision := FView.Store.Snapshot.Revision;
  FRejectRows := True;
  LRefused := False;
  try
    FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  FRejectRows := False;
  Check(LRefused and (FResourceCalls = LCalls), 'later reusable rejection publishes no frame notification');
  Check((FResources.Context.Locale.Name = 'en-GB') and
    (FView.Store.Snapshot.Revision = LRevision), 'later rejection preserves locale and revision');
  Cell('people-table', 'Ada 🌙');
  Check(Caption = FExpectedHeadline, 'later rejection preserves painted caption');
  ReusedText(FApplication, FExpectedHeadline);
  LRefused := False;
  try
    FResources.Localize(NyxLocale('short'), NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (FResources.Context.Locale.Name = 'en-GB'),
    'unmounted slider rejects a resource maximum below current state');
  LRefused := False;
  try
    FResources.Localize(NyxLocale('malformed'), NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (FResources.Context.Locale.Name = 'en-GB'),
    'malformed row family preserves accepted application frame');
  LRefused := False;
  try
    FResources.Localize(NyxLocale('prompt-type'), NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (FResources.Context.Locale.Name = 'en-GB'),
    'reusable prompt type refuses the complete application frame');
  ReusedText(FApplication, FExpectedHeadline);
  LRefused := False;
  try
    FResources.Localize(NyxLocale('prompt-path'), NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (FResources.Context.Locale.Name = 'en-GB'),
    'missing reusable prompt path refuses the complete application frame');
  ReusedText(FApplication, FExpectedHeadline);
  RefuseIdentity('duplicate-row');
  RefuseIdentity('identity-path');
  LWaitToken := NyxPreparedApplicationResources(FResources).SubscribePrepared(nil, DeclinePreparation);
  LRefused := False;
  try
    try
      FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
    except
      on ENyxResource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (FResources.Context.Locale.Name = 'en-GB'),
      'late preparation wait retires earlier stages without publishing');
  finally
    LWaitToken.Disconnect;
  end;
  Check(not FApplication.State.Busy and FApplication.View.ResourceReady,
    'failed admission releases scalar and row reservations');
  FObserve := False;
  FView.Edit(NyxItem(NyxCollection('people'), 'ada'), 0,
    TNyxStateValue.FromText('A local edit'));
  LRowCalls := FRowCalls;
  FResources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
  Cell('people-table', 'A local edit');
  Check(FRowCalls = LRowCalls, 'unchanged source frame does not overwrite or notify runtime rows');
  FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
  Cell('people-table', 'Ada');
  ReusedText(FApplication, 'Team workbench');
  ReusedText(FOther, 'Team workbench');
  Check(not FApplication.State.Busy, 'successful retirement releases shared snapshot hold');
  FApplication.ShowPage('details');
  Check(TryNyxStateNumber(FApplication.View.Root.Find('count').Prop(
    NyxBindingPropertyName(bpMaximum)), LMaximum)
    and (LMaximum = 10),
    'hidden page navigation uses current accepted scalar resources');
  FApplication.ShowPage('home');
  FView := FApplication.View.CollectionView('people-table');
  FFirst := FApplication.View.CollectionView('first-card/card-table');
  FSecond := FApplication.View.CollectionView('second-card/card-table');
  Check(FView.Selected.ID = 'sam', 'navigation retains application selection');
  ReusedText(FApplication, 'Team workbench');
end;

procedure TJourney.LateScopes;
var
  LContext: INyxCollectionContext;
  LResources: INyxResources;
  LPrepared: INyxPreparedPublication;
  LFirst: INyxCollection;
  LLate: INyxCollection;
  LSpec: TNyxCollectionViewSpec;
  LRefused: Boolean;
begin
  { Scope seed qualification is separate from the mounted-target journey above.
    A late resolver must use installed source defaults, never an edited app store. }
  LContext := NewNyxCollectionContext(FDocument.Collections, FDocument.Resources,
    NyxDefaultLocale, NyxDefaultLocale);
  LSpec := NyxCollectionView(NyxCollection('people')).Scoped(csInstance)
    .Column(NyxTextField('name'), 'Name');
  LFirst := LContext.Resolve(LSpec, 'early-card');
  LResources := FDocument.Resources.Clone;
  LResources.Define(NyxResourceRef('team'), NyxJSONResource(
    Payload('New scope seed', 20, NyxMappingUpdated)));
  LPrepared := PrepareNyxCollectionContextResources(LContext, LResources,
    NyxDefaultLocale, NyxDefaultLocale);
  LRefused := False;
  try
    LContext.Resolve(LSpec, 'late-card');
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'new reusable scope refuses during held source admission');
  PublishNyxGroup([LPrepared]);
  LLate := LContext.Resolve(LSpec, 'late-card');
  Check((LLate <> LFirst) and (LLate.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) =
    TNyxText('Ada 🌙')), 'late scope receives the installed seed in an independent store');
  Check(LFirst.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = TNyxText('Ada 🌙'),
    'existing early scope receives the same accepted resource revision');
end;

procedure TJourney.InitialAttachment;
var
  LRuntime: TNyxApplicationState;
  LResources: INyxApplicationResources;
begin
  { Public runtime attachment must admit a frame whose initial locale differs
    from the runtime constructor, even without an ordinary target host. }
  LRuntime := TNyxApplicationState.Create(FDocument);
  try
    LResources := NewNyxApplicationResources(FDocument.Resources,
      FApplication.View.Events.Scheduler, NyxApplicationResourceOptions
        .Loading(nrlOnDemand).Localize(NyxLocale('en-GB'), NyxDefaultLocale));
    LRuntime.AttachResources(LResources);
    Check(LRuntime.Collections.Collection(NyxCollection('people')).Snapshot.ItemAt(0)
      .GetValue(NyxTextField('name')) = TNyxText('Ada 🌙'),
      'initial attachment publishes the configured source locale');
    Check(LRuntime.PageCollections('home').ViewFor('first-card/card-table')
      .Snapshot.ItemAt(0).GetValue(NyxIntegerField('score')) = 10,
      'initial attachment prepares existing reusable scopes too');
    Check(not LRuntime.State.Busy, 'initial attachment releases its state snapshot hold');
  finally

    if LResources <> nil then
    begin
      LResources.Stop;
    end;
    LRuntime.Free;
  end;
end;

procedure TJourney.Start;
var
  LResolver: INyxResourceResolver;
begin
  FDocument := Workshop;
  FBefore := TNyxCodec.Encode(FDocument);
  FTransport := TTransport.Create;
  FTransportLease := FTransport;
  LResolver := NewNyxResourceResolver(FTransportLease);
  FApplication := TTestApplication.Create;
  FOther := TTestApplication.Create;
  FApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand), LResolver);
  FOther.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand), LResolver);
  Mount(FApplication);
  Mount(FOther);
  ReusedText(FApplication, 'Team workbench');
  ReusedText(FOther, 'Team workbench');
  FResources := FApplication.Resources;
  Check(FTransport.Count = 0, 'on-demand construction does not start a transport');
  LocaleFallbacks;
  LocaleJourneys;
  LateScopes;
  InitialAttachment;
  FExpectedName := 'Ada 🌙';
  FExpectedHeadline := 'Loaded team 🌙';
  FObserve := True;
  FBusy := True;
  FResources.Reload(NyxResourceRef('team'), NyxDefaultLocale);
  FStage := 0;
  {$ifdef PAS2JS}
  FStartedAt := TJSDate.now;
  window.setTimeout(@Next, 10);
  {$else}
  FStartedAt := GetTickCount64;
  {$endif}
end;

procedure TJourney.Advance;
var
  LStatus: TNyxApplicationResourceStatus;
  LMaximum: Double;
begin
  {$ifdef PAS2JS}

  if TJSDate.now - FStartedAt > 20000 then
  {$else}

  if GetTickCount64 - FStartedAt > 20000 then
  {$endif}
  begin
    raise ENyxResource.Create('Live resource journey timed out');
  end;
  LStatus := FResources.Status(NyxResourceRef('team'), NyxDefaultLocale);
  case FStage of
    0:
      begin

        if FTransport.Count < 1 then
        begin
          Exit;
        end;
        FTransport.Send(0, Payload(FExpectedHeadline, 20, NyxMappingUpdated));
        FStage := 1;
      end;
    1:
      begin

        if LStatus.Phase <> nrpWaiting then
        begin
          Exit;
        end;
        Check(Caption = 'Team workbench', 'busy receiver keeps accepted caption while waiting');
        ReusedText(FApplication, 'Team workbench');
        Cell('people-table', 'Ada');
        FBusy := False;
        FFailureToken := FResources.Subscribe(nil, FailedReceiver);
        FResources.Wake;
        FStage := 2;
      end;
    2:
      begin

        if LStatus.Phase <> nrpReady then
        begin
          Exit;
        end;
        Check((LStatus.Origin = rloNetwork) and (LStatus.NotificationError <> ''),
          'empty observer exception reports committed network publication');
        Check(Caption = FExpectedHeadline, 'independent scalar receiver paints despite observer failure');
        ReusedText(FApplication, FExpectedHeadline);
        ReusedText(FOther, 'Team workbench');
        Cell('people-table', 'Ada 🌙');
        Cell('first-card/card-table', 'Ada 🌙');
        Cell('second-card/card-table', 'Ada 🌙');
        FFailureToken.Disconnect;
        FFailureToken := nil;
        FApplication.ShowPage('details');
        Cell('detail-table', 'Ada 🌙');
        Check(TryNyxStateNumber(FApplication.View.Root.Find('count').Prop(
          NyxBindingPropertyName(bpMaximum)), LMaximum)
          and (LMaximum = 20),
          'loaded frame survives navigation and hidden bound limits');
        FApplication.ShowPage('home');
        FView := FApplication.View.CollectionView('people-table');
        FFirst := FApplication.View.CollectionView('first-card/card-table');
        FSecond := FApplication.View.CollectionView('second-card/card-table');
        ReusedText(FApplication, FExpectedHeadline);
        FResources.Reload(NyxResourceRef('team'), NyxDefaultLocale);
        FStage := 3;
      end;
    3:
      begin

        if FTransport.Count < 2 then
        begin
          Exit;
        end;
        FTransport.Send(1, Payload('Rejected network row', 20,
          '{"batches":[{"people":[{"key":["ada"],"literal.name":"Rejected","score":10,"ready":"false","ratio":1.5}]}]}'));
        FStage := 4;
      end;
    4:
      begin

        if LStatus.Phase <> nrpRejected then
        begin
          Exit;
        end;
        Check(LStatus.Error <> '', 'invalid network source reports typed admission rejection');
        Check(Caption = FExpectedHeadline, 'rejected network frame preserves painted caption');
        ReusedText(FApplication, FExpectedHeadline);
        ReusedText(FOther, 'Team workbench');
        Cell('people-table', 'Ada 🌙');
        FExpectedName := 'Ada';
        FExpectedHeadline := 'Final workbench 🌙';
        FRetireToken := FResources.Subscribe(nil, RetireHost);
        FResources.Reload(NyxResourceRef('team'), NyxDefaultLocale);
        FStage := 5;
      end;
    5:
      begin

        if FTransport.Count < 3 then
        begin
          Exit;
        end;
        FTransport.Send(2, Payload(FExpectedHeadline, 30, NyxMappingInitial));
        FStage := 6;
      end;
    6:
      begin

        if FApplication <> nil then
        begin
          Exit;
        end;
        Check(FView.Store.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Ada',
          'retained store survives disposal before row/paint notifications');
        Check(FResources.Context.Snapshot.Resolve(NyxResourceRef('team'), NyxDefaultLocale,
          NyxDefaultLocale).Data.Field('headline').AsText = FExpectedHeadline,
          'retained catalog survives application disposal');
        Check(not NyxAtomicBusy(FView.Store), 'disposed application preparation retires retained stores');
        Check(FResources.Status(NyxResourceRef('team'), NyxDefaultLocale).NotificationError = '',
          'host disposal revokes scalar receiver without a stale callback failure');
        Check(TNyxCodec.Encode(FDocument) = FBefore, 'loading/localization never changes saved design/source defaults');
        Finish;
      end;
  end;
end;

procedure TJourney.Next;
begin

  if FFinished then
  begin
    Exit;
  end;
  try
    Advance;
  except
    on LException: Exception do
    begin
      FFinished := True;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-result', 'failed');
      document.body.setAttribute('data-nyx-error', LException.Message);
      {$else}
      raise;
      {$endif}
    end;
  end;
  {$ifdef PAS2JS}

  if not FFinished then
  begin
    window.setTimeout(@Next, 10);
  end;
  {$endif}
end;

procedure TJourney.Finish;
begin
  FFinished := True;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-nyx-result', 'passed');
  document.body.setAttribute('data-nyx-checks', IntToStr(FChecks));
  {$else}
  WriteLn('PASS / live resource controls / ', FChecks, ' checks');
  {$endif}
end;

destructor TJourney.Destroy;
begin

  if FResourceToken <> nil then
  begin
    FResourceToken.Disconnect;
  end;

  if FFailureToken <> nil then
  begin
    FFailureToken.Disconnect;
  end;

  if FRetireToken <> nil then
  begin
    FRetireToken.Disconnect;
  end;

  if FRowToken <> nil then
  begin
    FRowToken.Disconnect;
  end;

  if FInstanceToken <> nil then
  begin
    FInstanceToken.Disconnect;
  end;

  if FRejectToken <> nil then
  begin
    FRejectToken.Disconnect;
  end;
  FApplication.Free;
  FOther.Free;
  FResources := nil;
  FView := nil;
  FFirst := nil;
  FSecond := nil;
  FDocument.Free;
  inherited Destroy;
end;

var
  GJourney: TJourney;

begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  GJourney := TJourney.Create;
  {$ifdef PAS2JS}
  GJourney.Start;
  {$else}
  try
    try
      GJourney.Start;
      while not GJourney.Finished do
      begin
        CheckSynchronize(0);
        Application.ProcessMessages;
        GJourney.Next;
        Sleep(1);
      end;
    finally
      GJourney.Free;
      CheckSynchronize(0);
      Application.ProcessMessages;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / live resource controls / ', LException.Message);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
