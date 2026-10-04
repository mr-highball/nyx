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
program nyx_named_events_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  {$ifdef PAS2JS}
  Web, JS, nyx.render.browser,
  {$else}
  Classes, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl,
  {$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.data, nyx.contract,
  nyx.codec, nyx.schema, nyx.behavior, nyx.events, nyx.callbacks, nyx.state,
  nyx.scheduler, nyx.event.payload, nyx.event.emitter, nyx.source,
  nyx.studio.session, nyx.studio.inspector, nyx.studio.projects,
  nyx.studio.agents, nyx.test.named
  {$ifdef NYX_COMPILED_NAMED}
  , nyx.named.fixture
  {$endif}
  ;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  {$else}
  TRenderer = TNyxLCLRenderer;
  { This real control owns only its managed event port. Click is installed by
    the creator and preserved by the Nyx native adapter's ordinary hook chain. }
  TNamedButton = class(TButton)
  public
    Emitter: INyxEventEmitter;
    procedure Publish(ASender: TObject);
  end;
  TButtonAccess = class(TButton);
  { No UI object is borrowed by the worker. Only its owned port crosses the
    boundary, and access refusal is read after WaitFor provides synchronization. }
  TProducerWorker = class(TThread)
  public
    Port: INyxEventEmitter;
    Refused: Boolean;
    procedure Execute; override;
  end;
  {$endif}
  TProbe = class(TNyxEventCallback)
  public
    Calls: Integer;
    Total: Integer;
    Last: TNyxEventInfo;
    Renderer: TRenderer;
    Unmount: Boolean;
    CancelOther: INyxEventSubscription;
    Fail: Boolean;
    Reenter: Boolean;
    Port: INyxEventEmitter;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  GRenderer: TRenderer;
  GEmitter: INyxEventEmitter;
  GCandidatePort: INyxEventEmitter;
  GDormant: Boolean;
  GFailFactory: Boolean;
  GQueuedProbe: TProbe;
  GQueuedOwner: INyxEventCallback;
  GQueuedSubscription: INyxEventSubscription;
  GDocument: TNyxDocument;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  {$else}
  GHost: TForm;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Named events: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);
  Last := AEvent.Copy;

  if AEvent.HasValue then
  begin
    Inc(Total, AEvent.Value.AsInteger);
  end;

  if CancelOther <> nil then
  begin
    CancelOther.Cancel;
  end;

  if Fail then
  begin
    raise ENyxModel.Create('Deliberate named callback failure');
  end;

  if Reenter then
  begin
    Reenter := False;
    Port.Emit(NyxEvent(NamedOpened), NyxData(2));
  end;

  if Unmount then
  begin
    Renderer.Unmount;
  end;
end;

{$ifndef PAS2JS}
procedure TNamedButton.Publish(ASender: TObject);
begin
  { The call may navigate and free Self. Read no control fields afterward. }
  Emitter.Emit(NyxEvent(NamedOpened), NyxData(3));
end;

procedure TProducerWorker.Execute;
begin
  try
    Port.Emit(NyxEvent(NamedOpened), NyxData(1));
  except
    on ENyxSchedule do
    begin
      Refused := True;
    end;
  end;
end;
{$endif}

{$ifdef PAS2JS}
function Factory(ANode: TNyxNode; const AEmitter: INyxEventEmitter): TJSHTMLElement;
var
  LListener: TJSEventHandler;
{$else}
function Factory(ANode: TNyxNode; AOwner: TComponent;
  const AEmitter: INyxEventEmitter): TControl;
{$endif}
begin
  GCandidatePort := AEmitter;
  GDormant := not AEmitter.Connected;
  try
    AEmitter.Emit(NyxEvent(NamedClosed));
    GDormant := False;
  except
    on ENyxSchedule do
    begin
      { Early target notifications cannot enter the unaccepted candidate. }
    end;
  end;

  if GFailFactory then
  begin
    raise ENyxModel.Create('Deliberate factory admission failure');
  end;
  GEmitter := AEmitter;
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('button'));
  Result.textContent := ANode.Prop('text');
  LListener := function(AEvent: TEventListenerEvent): Boolean
    begin
      AEmitter.Emit(NyxEvent(NamedOpened), NyxData(3));
      Result := True;
    end;
  Result.addEventListener('click', LListener);
  {$else}
  Result := TNamedButton.Create(AOwner);
  TNamedButton(Result).Emitter := AEmitter;
  TNamedButton(Result).Caption := ANode.Prop('text');
  TNamedButton(Result).OnClick := TNamedButton(Result).Publish;
  {$endif}
end;

procedure Click;
begin
  {$ifdef PAS2JS}
  GRenderer.ElementFor('item-button').click;
  {$else}
  TButtonAccess(GRenderer.ControlFor('item-button')).Click;
  {$endif}
end;

procedure RejectEmit(const AName: TNyxText; const AValue: TNyxDataValue;
  AHasValue: Boolean);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try

    if AHasValue then
    begin
      GEmitter.Emit(NyxEvent(AName), AValue);
    end
    else
    begin
      GEmitter.Emit(NyxEvent(AName));
    end;
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'producer refuses ' + AName + ' / ' + AValue.ToJSON);
end;

procedure RunAdmission;
var
  LRouter: INyxEvents;
  LAuthored: INyxAuthoredEvents;
  LNode: TNyxNode;
  LRejected: Boolean;
  LSpec: TNyxEventPayloadSpec;
  LEvents: array[0..1] of TNyxEventSchema;
  LName: TNyxText;
  LIndex: Integer;
begin
  LRouter := NewNyxEvents;
  LNode := TNyxNode.Create(nkButton, 'admission-button');
  try
    for LIndex := 0 to 3 do
    begin
      case LIndex of
        0: LName := '';
        1: LName := '   ';
        2: LName := 'Event' + #10 + 'Injected';
        3: LName := StringOfChar('x', 129);
      end;
      LRejected := False;
      try
        LRouter.OnNamed(NyxControlEvents('admission-button'), NyxEvent(LName));
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'named router refuses invalid identity ' + IntToStr(LIndex));
    end;
    LRejected := False;
    try
      LRouter.On(NyxControlEvents('admission-button'), ntNamed);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'anonymous closed trigger cannot subscribe to named transport');
    LRejected := False;
    try
      LAuthored := NyxCallbacks(LNode);
      LAuthored.On(ntNamed);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'authored named transport requires its exact reference');
    LEvents[0] := NyxNamedEventSchema(NyxEvent(NamedOpened), 'Opened',
      'Duplicate fixture', ncCustom, ncCustom);
    LEvents[1] := LEvents[0];
    LRejected := False;
    try
      RegisterNyxSchema(NyxCustomKind('refused-duplicate-event-fixture'), [], LEvents);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'creator duplicate semantic names fail atomically');
    LSpec := Default(TNyxEventPayloadSpec);
    LRejected := False;
    try
      LSpec.Admit(NyxNull, False);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'undefined payload schema never admits a value');
    LRejected := False;
    try
      LSpec := TNyxEventPayloadSpec.FromData(NyxObject([
        NyxField('kind', NyxData('signal')), NyxField('extra', NyxData(True))
      ]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'payload descriptor rejects unknown fields');
    Check(NyxNamedEvent(NamedDetails).Name = NamedDetails,
      'exact supplementary Unicode semantic identity is admitted');
  finally
    LAuthored := nil;
    LNode.Free;
    LRouter := nil;
  end;
end;

procedure RunAuthoring;
var
  LSession: TNyxStudioSession;
  LBase: TNyxDocument;
  LDecoded: TNyxDocument;
  LShell: TNyxNode;
  LProjection: TNyxNode;
  LInfos: TNyxAuthoredEventInfos;
  LFirst, LSecond: TNyxHandlerRef;
  LLine, LIndex, LCards: Integer;
  LEffect: TNyxInspectorEffect;
  LPending, LRemoval: TNyxCallbackRemoval;
  LBefore, LSource: TNyxText;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LReply: TNyxDataValue;
  LRejected: Boolean;
begin
  LSession := TNyxStudioSession.Create;
  LBase := NamedDesign;
  LShell := nil;
  LAgent := TNyxAgentSession.Create;
  try
    LSession.Load(TNyxCodec.Encode(LBase));
    LSession.Select('item-button');
    LFirst := LSession.AddCallback(NyxEvent(NamedOpened), LLine);
    Check((LLine > 1) and (LSession.CallbackLine(LFirst) > 1), 'named stub and source navigation');
    Check(Pos('ItemOpened', LFirst.Name) > 0, 'purposeful named callback class name');
    LSecond := LSession.AddCallback(NyxEvent(NamedOpened), LLine);
    LSession.AddCallback(NyxEvent(NamedClosed), LLine);
    LSession.AddCallback(NyxEvent(NamedDetails), LLine);
    LSession.SetCallbackPolicy(NyxEvent(NamedOpened), neUIQueue);
    LInfos := NyxAuthoredEvents(LSession.Selected);
    Check((Length(LInfos) = 3) and (Length(LInfos[0].Callbacks) = 2) and
      (LInfos[0].Policy = neUIQueue) and (LInfos[1].Policy = neSequential),
      'independent names, ordered callbacks and policies');
    Check(Pos('.OnNamed(NyxEvent(''ItemOpened''))', LSession.Source) > 0,
      'crafted source uses typed named authoring');
    Check(Pos(NamedDetails, LSession.Source) > 0, 'exact Unicode semantic name in crafted source');
    LBefore := LSession.Save;
    LSource := LSession.Source;
    LDecoded := TNyxCodec.Decode(LBefore);
    try
      Check(TNyxCodec.Encode(LDecoded) = LBefore, 'named persistence byte round trip');
    finally
      LDecoded.Free;
    end;
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    Check(LSession.Save = LBefore, 'generated named source reconstruction');
    LSession.RemoveCallback(NyxEvent(NamedOpened), NyxCallbackID(LFirst.Name));
    Check(Length(NyxAuthoredEvents(LSession.Selected)[0].Callbacks) = 1, 'remove exact named registration');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LSource), 'paired undo restores exact source and names');
    LProjection := LSession.SelectedProjection;
    LShell := TNyxNode.Create(nkColumn, 'named-inspector');
    LPending := Default(TNyxCallbackRemoval);
    try
      AddNyxEventsInspector(LShell, LSession, LProjection, LPending);
    finally
      LProjection.Free;
    end;
    LCards := 0;
    for LIndex := 0 to LShell.Count - 1 do
    begin

      if Copy(LShell.Children[LIndex].ID, 1, 12) = 'event-named-' then
      begin
        Inc(LCards);

        if LShell.Children[LIndex].Find(LShell.Children[LIndex].ID + '-add')
          .Prop(NyxStudioEventNameKey) = NamedOpened then
        begin
          RouteNyxStudioEvents(LSession,
            LShell.Children[LIndex].Find(LShell.Children[LIndex].ID + '-callback-0-remove'),
            ntClick, LPending, LEffect, LLine, LRemoval);
          Check(LEffect = nieRequestRemoval, 'named removal requests a warning');
          Check(LRemoval.Name.Name = NamedOpened, 'warning retains exact semantic identity');
        end;
      end;
    end;
    Check(LCards = 4, 'each creator event has its own inspector card');
    LShell.Free;
    LShell := TNyxNode.Create(nkColumn, 'named-warning');
    LProjection := LSession.SelectedProjection;
    try
      AddNyxEventsInspector(LShell, LSession, LProjection, LRemoval);
    finally
      LProjection.Free;
    end;
    LPending := LRemoval;
    RouteNyxStudioEvents(LSession, LShell.Find('event-removal-confirm'), ntClick,
      LPending, LEffect, LLine, LRemoval);
    Check(LEffect = nieRemoved, 'confirmation removes exact named registration');
    Check(Pos('procedure ' + LFirst.Name + '.Invoke', LSession.Source) > 0,
      'named removal retains handwritten implementation');
    LSession.Undo;
    Check(LSession.Save = LBefore, 'named confirmation is undoable');
    LSession.SetSourceDraft(StringReplace(LSession.Source,
      'OnNamed(NyxEvent(''ItemOpened''))', 'OnNamed(''ItemOpened'')', []));
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore), 'raw named authoring fails atomically');
    LSession.DiscardSourceDraft;
    LPair := NyxProjectPair(LSession.Save, LSession.Source);
    LReply := LAgent.Exchange(NyxObject([
      NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('item-button')),
      NyxField('view', NyxData('home'))
    ]));
    LReply := LAgent.Call('nyx_node', 'named fixture', NyxObject([
      NyxField('id', NyxData('item-button')), NyxField('events', NyxData(True)),
      NyxField('eventOffset', NyxData(7)), NyxField('eventLimit', NyxData(1))
    ]));
    Check(LReply.Field('events').Count = 1, 'MCP event context is paged');
    Check(LReply.Field('eventOffset').AsInteger = 7, 'MCP retains requested offset');
    Check(LReply.Field('totalEvents').AsInteger > 7, 'MCP reports event total without whole state');
    { Query each published declaration by bounded page, including exact payload. }
    LCards := 0;
    for LIndex := 0 to LReply.Field('totalEvents').AsInteger - 1 do
    begin
      LReply := LAgent.Call('nyx_node', 'named fixture', NyxObject([
        NyxField('id', NyxData('item-button')), NyxField('events', NyxData(True)),
        NyxField('eventOffset', NyxData(LIndex)), NyxField('eventLimit', NyxData(1))
      ]));

      if LReply.Field('events').Item(0).Field('name').AsText = NamedOpened then
      begin
        Check(LReply.Field('events').Item(0).Field('payload').Field('kind').AsText = 'scalar',
          'MCP exposes declared payload family');
        Check((LReply.Field('registrations').Field('events').Count = 1) and
          (LReply.Field('totalRegistrations').AsInteger = 2) and
          LReply.Field('registrationsPartial').AsBoolean,
          'MCP callback context belongs only to the queried exact event');
        LReply := LAgent.Call('nyx_node', 'named fixture', NyxObject([
          NyxField('id', NyxData('item-button')), NyxField('events', NyxData(True)),
          NyxField('eventOffset', NyxData(LIndex)), NyxField('eventLimit', NyxData(1)),
          NyxField('registrationOffset', NyxData(1)), NyxField('registrationLimit', NyxData(1))
        ]));
        Check(LReply.Field('registrationsPartial').AsBoolean and
          (LReply.Field('registrations').Field('events').Item(0).Field('callbacks').Count = 1) and
          (LReply.Field('registrations').Field('events').Item(0).Field('callbacks').Item(0)
            .Field('handler').AsText = LSecond.Name),
          'MCP pages callback order and marks the partial descriptor');
        Inc(LCards);
      end;
    end;
    Check(LCards = 1, 'MCP exposes each exact named event once');
  finally
    LAgent.Free;
    LShell.Free;
    LBase.Free;
    LSession.Free;
  end;
end;

procedure RunControls;
var
  LOpened, LOther, LDetails, LFailure: TProbe;
  LOwners: array of INyxEventCallback;
  LOpenedSub, LOtherSub, LDetailsSub, LFailSub: INyxEventSubscription;
  LOldPort: INyxEventEmitter;
  LData: TNyxDataValue;
  LSpec: TNyxEventPayloadSpec;
  LSource: TNyxText;
  LExpected: TNyxDocument;
  LBefore: TNyxText;
  LRejected: Boolean;
  LCalls: Integer;
  LNode: TNyxNode;
  {$ifndef PAS2JS}
  LWorker: TProducerWorker;
  {$endif}
begin
  {$ifdef NYX_COMPILED_NAMED}
  GDocument := BuildNyxDocument;
  LExpected := NamedCompanion(LSource);
  try
    Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(LExpected),
      'actual compiled companion reconstructs exact named document');
  finally
    LExpected.Free;
  end;
  {$else}
  GDocument := NamedDesign;
  {$endif}
  LBefore := TNyxCodec.Encode(GDocument);
  GRenderer := TRenderer.Create;
  GRenderer.RegisterEventFactory(NyxCustomKind(NamedFixtureKind), @Factory);
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(GHost);
  {$else}
  GHost := TForm.Create(nil);
  GHost.SetBounds(0, 0, 640, 480);
  {$endif}
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  Check(GDormant and GEmitter.Connected, 'candidate port connects only after full admission');
  {$ifndef PAS2JS}
  LWorker := TProducerWorker.Create(True);
  try
    LWorker.Port := GEmitter;
    LWorker.Start;
    LWorker.WaitFor;
    Check(LWorker.Refused, 'native worker cannot enter a UI-owned producer');
  finally
    LWorker.Free;
  end;
  {$endif}
  {$ifdef NYX_COMPILED_NAMED}
  BindNyxCallbacks(GDocument, GRenderer.Events);
  Click;
  Check(NamedCalls = 3, 'actual control invokes handwritten generated companion');
  {$endif}
  LOpened := TProbe.Create;
  LOther := TProbe.Create;
  LDetails := TProbe.Create;
  LFailure := TProbe.Create;
  SetLength(LOwners, 4);
  LOwners[0] := LOpened;
  LOwners[1] := LOther;
  LOwners[2] := LDetails;
  LOwners[3] := LFailure;
  LOpenedSub := GRenderer.Events.OnNamed(NyxControlEvents('item-button'), NyxEvent(NamedOpened))
    .Subscribe(LOwners[0]);
  LOtherSub := GRenderer.Events.OnNamed(NyxControlEvents('item-button'), NyxEvent(NamedClosed))
    .Subscribe(LOwners[1]);
  LDetailsSub := GRenderer.Events.OnNamed(NyxControlEvents('item-button'), NyxEvent(NamedDetails))
    .Subscribe(LOwners[2]);
  Click;
  Check((LOpened.Calls = 1) and (LOpened.Total = 3) and (LOther.Calls = 0),
    'actual widget event reaches only its exact named stream');
  Check((LOpened.Last.Trigger = ntNamed) and LOpened.Last.HasValue and
    (LOpened.Last.ValueKind = nskInteger), 'typed scalar snapshot from actual producer');
  Check((LOpened.Last.OriginID = 'item-button') and
    (LOpened.Last.SourceID = 'item-button'), 'mounted identity retained in snapshot');
  LFailSub := GRenderer.Events.OnNamed(NyxControlEvents('item-button'), NyxEvent(NamedOpened))
    .Subscribe(LOwners[3]);
  LOpened.CancelOther := LFailSub;
  Click;
  Check(LFailure.Calls = 0, 'dispatch mutation cancels a later named snapshot registration');
  LOpened.CancelOther := nil;
  LOpened.Port := GEmitter;
  LOpened.Reenter := True;
  LCalls := LOpened.Calls;
  Click;
  Check(LOpened.Calls = LCalls + 2, 'named reentrancy retains independent dispatch snapshots');
  LOpened.Port := nil;
  GEmitter.Emit(NyxEvent(NamedClosed));
  Check((LOther.Calls = 1) and not LOther.Last.HasValue and not LOther.Last.HasDetails,
    'signal has no invented payload');
  LData := NyxObject([
    NyxField('caption', NyxData('Owned / 🌙 漢字')),
    NyxField('fraction', NyxData(NyxDecimal('0.1250')))
  ]);
  GEmitter.Emit(NyxEvent(NamedDetails), LData);
  Check(LDetails.Last.HasDetails and not LDetails.Last.HasValue,
    'structured payload is distinct from scalar values');
  Check(LDetails.Last.Details.ToJSON = LData.ToJSON, 'exact owned Unicode and decimal details');
  LSpec := NyxScalarPayload(NyxIntegerDomain.Range(0, 10));
  Check(TNyxEventPayloadSpec.FromData(LSpec.ToData).ToData.ToJSON = LSpec.ToData.ToJSON,
    'scalar payload schema round trip');
  LSpec := NyxDataPayload(ndObject);
  Check(TNyxEventPayloadSpec.FromData(LSpec.ToData).ToData.ToJSON = LSpec.ToData.ToJSON,
    'structured payload schema round trip');
  LCalls := LOpened.Calls;
  RejectEmit(NamedOpened, NyxData('3'), True);
  RejectEmit(NamedOpened, NyxData(11), True);
  RejectEmit(NamedOpened, NyxNull, False);
  RejectEmit(NamedClosed, NyxNull, True);
  RejectEmit(NamedDetails, NyxArray([]), True);
  RejectEmit('Undeclared', NyxNull, False);
  Check(LOpened.Calls = LCalls, 'refused payloads never invoke callbacks');
  {$ifndef PAS2JS}
  RejectEmit('BrowserOnly', NyxNull, False);
  {$endif}
  LNode := GRenderer.Root.Find('item-button');
  LNode.Configure.Enabled(False).Done;
  Check(not GEmitter.Emit(NyxEvent(NamedOpened), NyxData(2)),
    'disabled producer suppresses notifications');
  LNode.Configure.Enabled(True).Visible(False).Done;
  Check(not GEmitter.Emit(NyxEvent(NamedOpened), NyxData(2)),
    'hidden producer suppresses notifications');
  LNode.Configure.Visible(True).ReadOnly(True).Done;
  Check(GEmitter.Emit(NyxEvent(NamedOpened), NyxData(2)),
    'read-only producer may report notifications');
  LNode.Configure.ReadOnly(False).Done;
  LOtherSub.Cancel;
  GEmitter.Emit(NyxEvent(NamedClosed));
  Check(LOther.Calls = 1, 'named cancellation removes its registration');
  LFailure.Fail := True;
  LFailSub := GRenderer.Events.OnNamed(NyxControlEvents('item-button'), NyxEvent(NamedOpened))
    .Subscribe(LOwners[3]);
  LRejected := False;
  try
    GEmitter.Emit(NyxEvent(NamedOpened), NyxData(1));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(not LRejected and (LFailure.Calls = 1) and
    (LFailSub.LastExecution.Status = nesFailed) and
    (LFailSub.LastExecution.Failure = 'Deliberate named callback failure'),
    'callback failure retains its owned execution diagnostic');
  LFailSub.Cancel;
  LOldPort := GEmitter;
  GFailFactory := True;
  LRejected := False;
  try
    GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  GFailFactory := False;
  Check(LRejected and LOldPort.Connected and not GCandidatePort.Connected,
    'failed candidate revokes its port while preserving accepted producer');
  Check(LOldPort.Emit(NyxEvent(NamedOpened), NyxData(1)),
    'accepted producer still dispatches after failed candidate');
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  Check(not LOldPort.Connected and GEmitter.Connected, 'remount replaces producer lifetime');
  GEmitter := LOldPort;
  RejectEmit(NamedOpened, NyxData(1), True);
  GEmitter := GCandidatePort;
  Check(LDetails.Last.Details.ToJSON = LData.ToJSON, 'owned payload survives old widget disposal');
  LOpened.Renderer := GRenderer;
  LOpened.Unmount := True;
  Click;
  Check(not GEmitter.Connected, 'navigation in real widget callback safely disconnects its producer');
  LOpened.Unmount := False;
  LOpened.Renderer := nil;
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  GQueuedProbe := TProbe.Create;
  GQueuedOwner := GQueuedProbe;
  GQueuedSubscription := GRenderer.Events.OnNamed(NyxControlEvents('item-button'),
    NyxEvent(NamedOpened)).Policy(neUIQueue).Subscribe(GQueuedOwner);
  GEmitter.Emit(NyxEvent(NamedOpened), NyxData(4));
  Check(GQueuedProbe.Calls = 0, 'UI queue is deferred for named events');
  GRenderer.Unmount;
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  GEmitter.Emit(NyxEvent(NamedOpened), NyxData(5));
  Check(TNyxCodec.Encode(GDocument) = LBefore, 'runtime events never mutate authored design');
end;

procedure Cleanup;
begin
  GRenderer.Free;
  GRenderer := nil;
  {$ifdef PAS2JS}
  GHost.remove;
  {$else}
  GHost.Free;
  {$endif}
  GHost := nil;
  GEmitter := nil;
  GCandidatePort := nil;
  GQueuedSubscription := nil;
  GQueuedOwner := nil;
  GDocument.Free;
  GDocument := nil;
end;

procedure Finish;
begin
  try
    Check((GQueuedProbe.Calls = 1) and (GQueuedProbe.Total = 5),
      'real UI queue cancels old generation and delivers the new owned payload');
    GRenderer.Free;
    GRenderer := nil;
    Check(not GEmitter.Connected, 'retained producer disconnects on renderer destruction');
    RejectEmit(NamedOpened, NyxData(1), True);
    Cleanup;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' named event controls/authoring checks';
    document.body.setAttribute('data-named-events', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' named event controls/authoring checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-named-events', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifndef PAS2JS}
var
  LPump: Integer;
  LFile: TFileStream;
  LSource: TNyxText;
  LCompanion: TNyxDocument;
{$endif}
begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    {$endif}
    RegisterNamedFixture;
    RunAdmission;
    RunAuthoring;
    RunControls;
    {$ifdef PAS2JS}
    window.setTimeout(@Finish, 100);
    {$else}
    for LPump := 1 to 10 do
    begin
      Application.ProcessMessages;
      CheckSynchronize(10);
    end;
    Finish;

    if (ExitCode = 0) and (ParamCount > 0) then
    begin
      LCompanion := NamedCompanion(LSource);
      try
        LFile := TFileStream.Create(ParamStr(1), fmCreate);
        try
          LFile.WriteBuffer(LSource[1], Length(LSource));
        finally
          LFile.Free;
        end;
      finally
        LCompanion.Free;
      end;
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-named-events', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
