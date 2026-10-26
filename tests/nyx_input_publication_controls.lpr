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
program nyx_input_publication_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Forms, Controls, StdCtrls,
  {$ifdef WINDOWS}Windows,{$endif}nyx.render.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.state, nyx.behavior,
  nyx.binding.inputs, nyx.publication, nyx.collections,
  nyx.collections.query, nyx.collections.query.session,
  nyx.collections.view, nyx.collections.view.types;

type
  {$ifdef PAS2JS}
  { Standard DOM CompositionEvent constructor for synthetic adapter input.
    The borrowed mounted element owns no fixture; this requires a browser
    implementing composition events and does not claim a physical IME session. }
  TInputComposition = class external name 'CompositionEvent'(TJSEvent)
  public
    constructor new(const AName: String; AOptions: TJSObject); reintroduce;
  end;
  {$endif}
  {$ifndef PAS2JS}
  { Capture native message-boundary exceptions as test failures. This receiver
    outlives every fixture form and prevents a modal dialog from hiding the
    real diagnostic behind the unchanged process watchdog. }
  TInputErrors = class
  public
    Count: Integer;
    Message: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { One independent real renderer, authored document and public query owner.
    The fixture destroys its renderer before controls/document. Retained ports
    and prepared captures must survive that actual owner retirement safely. }
  TJourney = class
  public
    Document: TNyxDocument;
    {$ifdef PAS2JS}
    Renderer: TNyxBrowserRenderer;
    Host: TJSHTMLElement;
    {$else}
    Renderer: TNyxLCLRenderer;
    Host: TForm;
    {$endif}
    Store: INyxCollection;
    View: INyxCollectionView;
    Session: INyxCollectionQuerySession;
    Actor: INyxCollectionQueryOwner;
    Changes: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Draft(const AID, AText: TNyxText);
    procedure TypeSearch(const AText: TNyxText);
    function Text(const AID: TNyxText): TNyxText;
    procedure Composition(AStarting: Boolean);
    function ClearQuery: INyxPreparedPublication;
  end;
  { A later validator simulates newer physical input before the final fence.
    Its fixture is borrowed through this synchronous group only. No other
    participant's phase methods are called by this receiver. }
  TDraftGate = class(TInterfacedObject, INyxPreparedPublication)
  public
    Journey: TJourney;
    Composing: Boolean;
    Installs: Integer;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

var
  GChecks: Integer;
  {$ifndef PAS2JS}
  GErrors: TInputErrors;
  {$endif}

{$ifndef PAS2JS}
procedure TInputErrors.Failed(ASender: TObject; AException: Exception);
begin
  Inc(Count);

  if Count = 1 then
  begin
    Message := TNyxText(AException.Message);
  end;
  WriteLn(StdErr, 'Unhandled native input exception / ', AException.Message);
  Flush(StdErr);
end;
{$endif}

{ Optional flushed receipts locate a real failed native journey without
  changing assertions, dispatch or deadlines. Browser result attributes remain
  the normal observation surface; native tracing writes only when requested. }
procedure Trace(const AMessage: TNyxText);
begin
  {$ifndef PAS2JS}

  if GetEnvironmentVariable('NYX_INPUT_TRACE') = '1' then
  begin
    WriteLn(StdErr, AMessage);
    Flush(StdErr);
  end;
  {$endif}
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('Physical input publication: ' + AReason);
  end;
  Inc(GChecks);
  Trace('checked / ' + AReason);
end;

constructor TJourney.Create;
var
  LPage: TNyxNode;
  LKey: TNyxCollectionRef;
  {$ifndef PAS2JS}
  LID: TNyxText;
  LIndex: Integer;
  LHandle: THandle;
  {$endif}
begin
  inherited Create;
  Document := TNyxDocument.Create;
  Document.State.SetValue(NyxTextState('search'), 'Ada');
  LPage := TNyxNode.Create(nkColumn, 'home').Configure.Padding(12).Gap(8).Done;
  Document.AddPage(LPage);
  LPage.Add(TNyxNode.Create(nkInput, 'search').Configure.Text('Search people').Done);
  LPage.Find('search').Binds.Value(NyxTextState('search'));
  LPage.Add(TNyxNode.Create(nkSelect, 'sort').Configure
    .Text('Sort people').Items('Name' + #10 + 'Score').Value('Name').Done);
  LPage.Add(TNyxNode.Create(nkInput, 'amount').Configure
    .Text('Reset this draft').InputType(niNumber).Value(1.0).Done);
  LPage.Add(TNyxNode.Create(nkInput, 'other').Configure
    .Text('Keep this draft').InputType(niNumber).Value(2.0).Done);
  LPage.Add(TNyxNode.Create(nkMemo, 'mirror'));
  LPage.Find('mirror').Binds.Value(NyxTextState('search'));
  {$ifdef PAS2JS}
  Host := TJSHTMLElement(Web.document.createElement('div'));
  Web.document.body.appendChild(Host);
  Renderer := TNyxBrowserRenderer.Create;
  Renderer.OnEvent := @Event;
  Renderer.Render(Document, LPage, Host);
  {$else}
  Host := TForm.CreateNew(nil);
  Host.ClientWidth := 1280;

  if ParamCount = 1 then
  begin
    Host.ClientWidth := StrToInt(ParamStr(1));
  end;
  Host.ClientHeight := 720;
  Host.Show;
  Trace('fixture / native renderer');
  Renderer := TNyxLCLRenderer.Create;
  Renderer.OnEvent := Event;
  Renderer.Render(Document, LPage, Host);
  Trace('fixture / native idle');
  Application.ProcessMessages;
  { Handle creation belongs to setup, never final publication readiness. }
  for LIndex := 0 to LPage.Count - 1 do
  begin
    LID := LPage.Children[LIndex].ID;
    LHandle := TWinControl(Renderer.InputFor(LID)).Handle;
    Check(LHandle <> 0, 'fixture acquired its real native input handle');
  end;
  {$endif}
  LKey := NyxCollection('people');
  Store := NewNyxCollection(LKey,
    NyxCollectionSchema.Text(NyxTextField('name'), ''), [
      NyxCollectionItem(NyxItem(LKey, 'ada')).WithValue(NyxTextField('name'), 'Ada'),
      NyxCollectionItem(NyxItem(LKey, 'bea')).WithValue(NyxTextField('name'), 'Bea')]);
  View := NewNyxCollectionView(Store,
    NyxCollectionView(LKey).Column(NyxTextField('name'), 'Name', cmReadOnly), cpTable);
  Session := NyxCollectionQuerySession(View);
  Actor := Session.AcquireOwner(NyxQueryOrigin('toolbar'), 'People search');
  PublishNyxGroup([Session.PrepareSession(Session.Capture, Actor,
    NyxQuerySessionEdits.Filter(NyxQueryChannel('search'),
      NyxWhere(NyxTextField('name')).Contains('Ada')))]);
end;

destructor TJourney.Destroy;
begin
  Renderer.Free;
  {$ifdef PAS2JS}

  if Host <> nil then
  begin
    Host.remove;
  end;
  {$else}
  Host.Free;
  {$endif}
  Document.Free;
  inherited Destroy;
end;

procedure TJourney.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.Trigger = ntChange then
  begin
    Inc(Changes);
  end;
end;

procedure TJourney.Draft(const AID, AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(Renderer.InputFor(AID)).value := AText;
  Renderer.InputFor(AID).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(Renderer.InputFor(AID)).Text := AText;
  {$endif}
end;

procedure TJourney.TypeSearch(const AText: TNyxText);
begin
  Draft('search', AText);
end;

function TJourney.Text(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}

  if Renderer.InputFor(AID) is TJSHTMLTextAreaElement then
  begin
    Exit(TJSHTMLTextAreaElement(Renderer.InputFor(AID)).value);
  end;

  if Renderer.InputFor(AID) is TJSHTMLSelectElement then
  begin
    Exit(TJSHTMLSelectElement(Renderer.InputFor(AID)).value);
  end;
  Result := TJSHTMLInputElement(Renderer.InputFor(AID)).value;
  {$else}

  if Renderer.InputFor(AID) is TComboBox then
  begin
    Exit(TNyxText(TComboBox(Renderer.InputFor(AID)).Text));
  end;
  Result := StringReplace(TNyxText(TCustomEdit(Renderer.InputFor(AID)).Text),
    #13#10, #10, [rfReplaceAll]);
  {$endif}
end;

procedure TJourney.Composition(AStarting: Boolean);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
  LName: String;
{$endif}
begin
  {$ifdef PAS2JS}
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['data'] := '';
  LName := 'compositionend';

  if AStarting then
  begin
    LName := 'compositionstart';
  end;
  Renderer.InputFor('search').dispatchEvent(TInputComposition.new(LName, LOptions));
  {$else}
  {$ifdef WINDOWS}

  if AStarting then
  begin
    Windows.SendMessage(TWinControl(Renderer.InputFor('search')).Handle,
      WM_IME_STARTCOMPOSITION, 0, 0);
  end
  else
  begin
    Windows.SendMessage(TWinControl(Renderer.InputFor('search')).Handle,
      WM_IME_ENDCOMPOSITION, 0, 0);
  end;

  if not AStarting then
  begin
    { End drains outside prepared admission. A starting message is synchronous;
      pumping queued layout while a validator holds the state owner would wait
      for the very publication that cannot resume until this call returns. }
    Application.ProcessMessages;
    Application.Idle(False);
  end;
  {$else}
  raise ENyxState.Create('This synthetic composition fixture requires Win32');
  {$endif}
  {$endif}
end;

function TJourney.ClearQuery: INyxPreparedPublication;
begin
  Result := Session.PrepareSession(Session.Capture, Actor, NyxQuerySessionEdits.ClearOwner);
end;

procedure TDraftGate.Validate;
begin

  if Composing then
  begin
    Journey.Composition(True);
  end
  else
  begin
    Journey.Draft('amount', '2.5');
  end;
end;

procedure TDraftGate.Install;
begin
  Inc(Installs);
end;

procedure TDraftGate.Notify;
begin
end;

procedure TDraftGate.Retire;
begin
end;

procedure Run;
var
  LJourney: TJourney;
  LPeer: TJourney;
  LCapture: INyxInputCapture;
  LInputs: INyxInputCommands;
  LReset: INyxPreparedPublication;
  LGate: TDraftGate;
  LGuard: INyxPreparedPublication;
  LRefused: Boolean;
  LCase: Integer;
  LLastCase: Integer;
  LChanges: Integer;
begin
  LJourney := TJourney.Create;
  LPeer := nil;
  try
    LPeer := TJourney.Create;
    {$ifdef PAS2JS}
    LJourney.Renderer.InputFor('search').focus;
    {$else}
    TWinControl(LJourney.Renderer.InputFor('search')).SetFocus;
    Application.ProcessMessages;
    {$endif}
    LInputs := LJourney.Renderer.Inputs;
    Check((LInputs <> nil) and LInputs.Connected, 'renderer exposes its active physical command port');
    LJourney.Draft('amount', '');
    LJourney.Draft('other', '');
    Check((LJourney.Text('amount') = '') and (LJourney.Renderer.Root.Find('amount').Prop('value') = '1'),
      'real numeric field retains an unfinished draft outside accepted projection');
    LCapture := LInputs.Capture([NyxControl('amount')]);
    LChanges := LJourney.Changes;
    PublishNyxGroup([LInputs.Prepare(LCapture, [NyxInputValue(NyxControl('amount'), 1)])]);
    Check((LJourney.Text('amount') = '1') and (LJourney.Text('other') = ''),
      'same-value reset restores only its requested draft');
    Check(LJourney.Changes = LChanges, 'reset emits no synthetic Change callback');
    Check((LPeer.Text('amount') = '1') and (LPeer.Text('other') = '2'),
      'peer physical runtime and defaults remain independent');

    LJourney.TypeSearch('Bea');
    Check(LJourney.Renderer.State.GetValue(NyxTextState('search')) = 'Bea',
      'ordinary real input still writes the existing binding owner');
    LChanges := LJourney.Changes;
    LCapture := LInputs.Capture([NyxControl('search'), NyxControl('sort')]);
    PublishNyxGroup([LInputs.Prepare(LCapture, [NyxInputValue(NyxControl('search'), ''),
      NyxInputValue(NyxControl('sort'), 'Score')]), LJourney.ClearQuery]);
    Check((LJourney.Text('search') = '') and (LJourney.Text('mirror') = '') and
      (LJourney.Text('sort') = 'Score') and (LJourney.View.Snapshot.Count = 2),
      'bound search, mirror, select and query synchronize after one grouped reset');
    Check((LJourney.Text('other') = '') and (LJourney.Changes = LChanges),
      'joined query reset preserves unrelated draft and event counts');
    Check(LJourney.Document.State.GetValue(NyxTextState('search')) = 'Ada',
      'physical runtime reset preserves authored state defaults');
    LCapture := LInputs.Capture([NyxControl('search')]);
    PublishNyxGroup([LInputs.Prepare(LCapture,
      [NyxInputValue(NyxControl('search'), TNyxText('Café / 🌙'))])]);
    Check((LJourney.Text('search') = TNyxText('Café / 🌙')) and
      (LJourney.Text('mirror') = TNyxText('Café / 🌙')),
      'typed reset preserves supplementary Unicode through real input and memo faces');

    LCapture := LInputs.Capture([NyxControl('amount')]);
    LJourney.Draft('amount', '3.5');
    LRefused := False;
    try
      LReset := LInputs.Prepare(LCapture, [NyxInputValue(NyxControl('amount'), 1)]);
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    LReset := nil;
    Check(LRefused and (LJourney.Text('amount') = '3.5'),
      'newer physical input stales a prior accepted-value capture');

    LLastCase := 0;
    {$if defined(PAS2JS) or defined(WINDOWS)}
    LLastCase := 1;
    {$endif}
    for LCase := 0 to LLastCase do
    begin
      LCapture := LInputs.Capture([NyxControl('search'), NyxControl('amount')]);
      LReset := LInputs.Prepare(LCapture, [NyxInputValue(NyxControl('amount'), 1)]);
      LGate := TDraftGate.Create;
      LGuard := LGate;
      LGate.Journey := LJourney;
      LGate.Composing := LCase = 1;
      LRefused := False;
      try
        PublishNyxGroup([LReset, LGuard]);
      except
        on ENyxState do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LGate.Installs = 0) and
        (LJourney.Renderer.Root.Find('amount').Prop('value') = '1'),
        'later draft/composition input fences every grouped install');
      LGuard := nil;
      LReset := nil;

      if LCase = 1 then
      begin
        LRefused := False;
        try
          LCapture := LInputs.Capture([NyxControl('search')]);
        except
          on ENyxState do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'active composition refuses a fresh physical capture');
        LJourney.Composition(False);
        LCapture := LInputs.Capture([NyxControl('search')]);
        Check(LCapture.HasValueAt(0), 'genuine composition end restores physical capture readiness');
      end;
    end;

    LCapture := LInputs.Capture([NyxControl('amount')]);
    LReset := LInputs.Prepare(LCapture, [NyxInputValue(NyxControl('amount'), 1)]);
    FreeAndNil(LJourney);
    Check(not LInputs.Connected and LCapture.HasValueAt(0),
      'retained physical capture is readable after actual renderer destruction');
    LRefused := False;
    try
      PublishNyxGroup([LReset]);
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'old physical preparation cannot revive a destroyed renderer');
    LPeer.TypeSearch('Still independent');
    Check(LPeer.Renderer.State.GetValue(NyxTextState('search')) = 'Still independent',
      'peer input remains usable after original renderer retirement');
  finally
    LReset := nil;
    LGuard := nil;
    LCapture := nil;
    LInputs := nil;
    LPeer.Free;
    LJourney.Free;
  end;
end;

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GErrors := TInputErrors.Create;
    try
      Application.OnException := GErrors.Failed;
      Run;
      Check(GErrors.Count = 0, 'native message dispatch has no unhandled failure: ' + GErrors.Message);
    finally
      Application.OnException := nil;
      GErrors.Free;
    end;
    {$else}
    Run;
    {$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-input-controls', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' physical input publication checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-input-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
