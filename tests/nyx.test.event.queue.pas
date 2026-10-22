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

unit nyx.test.event.queue;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.studio.projects;

{ English event workshop shared by private-admission and actual control journeys.
  Every caller receives independent owned design/source data, never a live tree. }
function CreateNyxEventQueueSeed: TNyxProjectPair;
{ Exercise exact callback tickets, receipt admission, pair/history and retirement.
  The exported pair includes real newly authored callback implementations. }
function RunNyxEventQueueJourney(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.callbacks, nyx.scheduler, nyx.schema, nyx.source.preparation,
  nyx.studio.session, nyx.studio.inspector, nyx.event.discovery,
  nyx.event.payload, nyx.source, nyx.studio.callbackedits;

function CreateNyxEventQueueSeed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxColumn;
  LDefinition: INyxSearchField;
  LInstance: INyxComponent;
  LSource: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Callback workshop';
    LPage := NewNyxColumn('home');
    LPage.Configure.Padding(20).Gap(12).Done;
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxMemo('reply-memo').Configure.Text('Write a reply').Done);
    LPage.Add(NewNyxButton('send-button').Configure.Text('Send reply').Done);
    LDefinition := NewNyxSearchField('search-template');
    LDefinition.Configure.Text('Search the workshop').Done;
    LDocument.AddComponent(LDefinition);
    LInstance := NewNyxComponent('first-search');
    LInstance.Configure.Component(NyxComponent('search-template')).Done;
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-search');
    LInstance.Configure.Component(NyxComponent('search-template')).Done;
    LPage.Add(LInstance);
    LDocument.AddPage(NewNyxColumn('other'));
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := TNyxText(StringReplace(String(LSource), 'implementation' + #10,
      'implementation' + #10 + #10 +
      '{ Application helper remains handwritten and independently owned. }' + #10 +
      'function EventWorkshopNote: TNyxText;' + #10 +
      'begin' + #10 + '  Result := ''Keep crafting.'';' + #10 + 'end;' + #10, []));
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
end;

function ReplaceField(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AObject.Count);
  for LIndex := 0 to AObject.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

    if LFields[LIndex].Name = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end;
  end;
  Result := NyxObject(LFields);
end;

type
  { This synchronous public action borrows only the already accepted legacy
    session during Execute. The snapshot owns metadata, not a global producer;
    the caller keeps both the action lease and borrowed session alive. }
  TQueuePolicyRouteCheck = class(TInterfacedObject, INyxSchemaAction)
  private
    FChecks: Integer;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
  public
    Legacy: TNyxStudioSession;
    procedure Execute;
    property Checks: Integer read FChecks;
  end;

{ Resolve the Inspector's actual declaration metadata rather than relying on a
  named-event card ordinal, which changes when creator discovery gains rows. }
function FindQueueEventCommand(ARoot: TNyxNode; const ACommand: TNyxText;
  ATrigger: TNyxTrigger; const AName: TNyxEventRef): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if (ARoot.Prop(NyxStudioEventCommandKey) = ACommand) and
    (ARoot.Prop(NyxStudioEventTriggerKey) = NyxTriggerName(ATrigger)) and
    (ARoot.Prop(NyxStudioEventNameKey) = AName.Name) then
  begin
    Exit(ARoot);
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin
    Result := FindQueueEventCommand(ARoot.Children[LIndex], ACommand, ATrigger, AName);

    if Result <> nil then
    begin
      Exit;
    end;
  end;
end;

{ Closed worker metadata is deliberately scoped to this existing workshop.
  Physical KeyDown permits only UI queue; the named creator permits two choices
  without sequential. No registration survives this snapshot's Execute scope. }
function QueuePolicySnapshot: INyxSchemaSnapshot;
var
  LPhysical, LNamed: TNyxDataValue;
begin
  LPhysical := NyxObject([
    NyxField('trigger', NyxData(Ord(ntKeyDown))), NyxField('name', NyxData('')),
    NyxField('payload', NyxNull), NyxField('title', NyxData('Workshop keys')),
    NyxField('description', NyxData('Only the explicitly allowed deferred policy.')),
    NyxField('browser', NyxData(Ord(ncAvailable))),
    NyxField('native', NyxData(Ord(ncAvailable))),
    NyxField('optional', NyxData(False)), NyxField('producer', NyxData(False)),
    NyxField('contexts', NyxData(0)), NyxField('routes', NyxArray([])),
    NyxField('policies', NyxData(1 shl Ord(neUIQueue)))]);
  LNamed := NyxObject([
    NyxField('trigger', NyxData(Ord(ntNamed))),
    NyxField('name', NyxData(NyxSemantic(nseOpen).Name)),
    NyxField('payload', NyxSignalPayload.ToData),
    NyxField('title', NyxData('Workshop open')),
    NyxField('description', NyxData('An explicit non-sequential initial choice.')),
    NyxField('browser', NyxData(Ord(ncCustom))),
    NyxField('native', NyxData(Ord(ncCustom))),
    NyxField('optional', NyxData(False)), NyxField('producer', NyxData(True)),
    NyxField('contexts', NyxData(0)), NyxField('routes', NyxArray([])),
    NyxField('policies', NyxData((1 shl Ord(neAsynchronous)) or
      (1 shl Ord(neUIQueue))))]);
  Result := ReadNyxSchemaSnapshot(NyxObject([
    NyxField('version', NyxData(2)), NyxField('revision', NyxData(NyxSchemaRevision)),
    NyxField('schemas', NyxArray([
      NyxObject([NyxField('kind', NyxData(NyxKindName(nkMemo))),
        NyxField('properties', NyxArray([])), NyxField('events', NyxArray([LPhysical]))]),
      NyxObject([NyxField('kind', NyxData(NyxKindName(nkButton))),
        NyxField('properties', NyxArray([])), NyxField('events', NyxArray([LNamed]))])]))]));
end;

procedure TQueuePolicyRouteCheck.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Event queue policy route: ' + AReason);
  end;
  Inc(FChecks);
end;

procedure TQueuePolicyRouteCheck.Execute;
var
  LSession: TNyxStudioSession;
  LRoot, LProjection, LAdd, LPolicy: TNyxNode;
  LBefore: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LEffect: TNyxInspectorEffect;
  LRemoval: TNyxCallbackRemoval;
  LName: TNyxEventRef;
  LTrigger: TNyxTrigger;
  LExpected: TNyxExecutionPolicy;
  LEvents: TNyxAuthoredEventInfos;
  LIndex, LLine: Integer;
  LFailed: Boolean;
begin
  for LIndex := 0 to 1 do
  begin
    LSession := TNyxStudioSession.Create(CreateNyxEventQueueSeed);
    LRoot := nil;
    LProjection := nil;
    try
      LTrigger := ntKeyDown;
      LName := Default(TNyxEventRef);
      LExpected := neUIQueue;
      LSession.Select('reply-memo');

      if LIndex = 1 then
      begin
        LSession.Select('send-button');
        LTrigger := ntNamed;
        LName := NyxSemantic(nseOpen);
        LExpected := neAsynchronous;
      end;
      LBefore := LSession.ProjectSnapshot;
      LProjection := LSession.SelectedProjection;
      LRoot := TNyxNode.Create(nkColumn, 'policy-route-inspector');
      AddNyxEventsInspector(LRoot, LSession, LProjection,
        Default(TNyxCallbackRemoval));
      LAdd := FindQueueEventCommand(LRoot, 'add', LTrigger, LName);
      LPolicy := FindQueueEventCommand(LRoot, 'policy', LTrigger, LName);
      Check((LAdd <> nil) and (LPolicy <> nil),
        'The actual Inspector exposes the exact physical or named creator stream');
      Check((LPolicy.Prop(NyxAttributeName(atValue)) = NyxPolicyName(LExpected)) and
        (Pos(NyxPolicyName(neSequential), LPolicy.Prop(NyxAttributeName(atItems))) = 0),
        'Absent streams display a permitted default without sequential');

      if LIndex = 0 then
      begin
        Check(Trim(LPolicy.Prop(NyxAttributeName(atItems))) = NyxPolicyName(neUIQueue),
          'A single-policy creator exposes exactly its one allowed choice');
      end
      else
      begin
        Check((Pos(NyxPolicyName(neAsynchronous), LPolicy.Prop(NyxAttributeName(atItems))) > 0) and
          (Pos(NyxPolicyName(neUIQueue), LPolicy.Prop(NyxAttributeName(atItems))) > 0) and
          (Pos(NyxPolicyName(neThreaded), LPolicy.Prop(NyxAttributeName(atItems))) = 0),
          'A multi-policy creator exposes only its two declared choices');
      end;
      Check(CaptureNyxStudioEvents(LSession, LAdd, ntClick,
        Default(TNyxCallbackRemoval), Default(TNyxStudioPendingDesign), LEdit,
        LEffect, LLine, LRemoval) and (LEdit.Action = sdaCallbacks) and
        (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
        'Capture chooses a grouped edit without publishing a transient stream');
      Check(RouteNyxStudioEvents(LSession, LAdd, ntClick,
        Default(TNyxCallbackRemoval), LEffect, LLine, LRemoval) and
        (LEffect = nieSource) and (LLine > 0) and (Pos('TODO', LSession.Source) > 0),
        'Synchronous routing resolves the new Pascal TODO before one publication');
      LEvents := NyxAuthoredEvents(LSession.Selected);
      Check((Length(LEvents) = 1) and (LEvents[0].Trigger = LTrigger) and
        (LEvents[0].Name.Name = LName.Name) and (LEvents[0].Policy = LExpected) and
        (Length(LEvents[0].Callbacks) = 1),
        'The committed first callback already owns the allowed initial policy');
      LSession.Undo;
      Check((EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
        not LSession.CanUndo, 'One synchronous Undo restores the absent stream and whole pair');
      LSession.Redo;
      Check(Pos('TODO', LSession.Source) > 0, 'Redo restores the same useful callback source');
    finally
      LRoot.Free;
      LProjection.Free;
      LSession.Free;
    end;
  end;

  LSession := TNyxStudioSession.Create(CreateNyxEventQueueSeed);
  LRoot := nil;
  LProjection := nil;
  try
    LSession.Select('reply-memo');
    LSession.SetCallbackPolicy(ntKeyDown, neUIQueue);
    LBefore := LSession.ProjectSnapshot;
    LEvents := NyxAuthoredEvents(LSession.Selected);
    Check((Length(LEvents) = 1) and (Length(LEvents[0].Callbacks) = 0) and
      (LEvents[0].Policy = neUIQueue), 'A real empty stream retains its authored policy');
    LProjection := LSession.SelectedProjection;
    LRoot := TNyxNode.Create(nkColumn, 'existing-empty-policy-inspector');
    AddNyxEventsInspector(LRoot, LSession, LProjection, Default(TNyxCallbackRemoval));
    LAdd := FindQueueEventCommand(LRoot, 'add', ntKeyDown, Default(TNyxEventRef));
    LPolicy := FindQueueEventCommand(LRoot, 'policy', ntKeyDown, Default(TNyxEventRef));
    Check((LAdd <> nil) and (LPolicy <> nil) and
      (LPolicy.Prop(NyxAttributeName(atValue)) = NyxPolicyName(neUIQueue)),
      'An existing zero-callback stream is not mistaken for absence');
    Check(CaptureNyxStudioEvents(LSession, LAdd, ntClick,
      Default(TNyxCallbackRemoval), Default(TNyxStudioPendingDesign), LEdit,
      LEffect, LLine, LRemoval) and (LEdit.Action = sdaEvent) and
      (LEdit.Event.Action = seaAdd), 'Existing-stream Add retains its ordinary single-event intent');
    Check(RouteNyxStudioEvents(LSession, LAdd, ntClick,
      Default(TNyxCallbackRemoval), LEffect, LLine, LRemoval) and (LLine > 0),
      'The existing empty stream also routes to a real TODO');
    LEvents := NyxAuthoredEvents(LSession.Selected);
    Check((LEvents[0].Policy = neUIQueue) and (Length(LEvents[0].Callbacks) = 1),
      'Adding to an empty stream preserves its already accepted policy');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One Undo restores the exact explicit empty stream');
  finally
    LRoot.Free;
    LProjection.Free;
    LSession.Free;
  end;

  { This session was accepted BEFORE the scoped creator restriction. The
    Inspector must describe that stale policy honestly, without mutating it. }
  LRoot := nil;
  LProjection := nil;
  try
    LBefore := Legacy.ProjectSnapshot;
    LProjection := Legacy.SelectedProjection;
    LRoot := TNyxNode.Create(nkColumn, 'legacy-policy-inspector');
    AddNyxEventsInspector(LRoot, Legacy, LProjection, Default(TNyxCallbackRemoval));
    LAdd := FindQueueEventCommand(LRoot, 'add', ntKeyDown, Default(TNyxEventRef));
    LPolicy := FindQueueEventCommand(LRoot, 'policy', ntKeyDown, Default(TNyxEventRef));
    Check((LAdd <> nil) and (LPolicy <> nil) and
      (LPolicy.Prop(NyxAttributeName(atValue)) = '') and
      (LAdd.Prop(NyxAttributeName(atEnabled)) = 'false') and
      (LRoot.Find('event-key-down-invalid-policy') <> nil),
      'An invalid existing policy is visible and never shown as an accepted allowed choice');
    LFailed := False;
    try
      CaptureNyxStudioEvents(Legacy, LAdd, ntClick, Default(TNyxCallbackRemoval),
        Default(TNyxStudioPendingDesign), LEdit, LEffect, LLine, LRemoval);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and
      (EncodeNyxProject(Legacy.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'An old Add command refuses the invalid policy instead of repairing it implicitly');
    LPolicy.SetProp(NyxAttributeName(atValue), NyxPolicyName(neThreaded));
    LFailed := False;
    try
      CaptureNyxStudioEvents(Legacy, LPolicy, ntChange, Default(TNyxCallbackRemoval),
        Default(TNyxStudioPendingDesign), LEdit, LEffect, LLine, LRemoval);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and
      (EncodeNyxProject(Legacy.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'Forged disallowed Select input refuses before changing the accepted pair');
  finally
    LRoot.Free;
    LProjection.Free;
  end;
end;

{ These independent cases reuse the seed and ordinary worker APIs. They do not
  replace the original journey, mutate its exported pair or add a new program. }
function RunPolicyEventQueueCases: Integer;
var
  LSession, LLegacy: TNyxStudioSession;
  LSchemas, LRestricted: INyxSchemaSnapshot;
  LEdit, LAlternative: TNyxStudioDesignEdit;
  LRequest, LAlternativeRequest, LRead: TNyxStudioDesignRequest;
  LPrepared, LReceived, LAlternativePrepared: INyxPreparedDesign;
  LGroup: INyxPreparedCallbackGroup;
  LReply, LAlternativeReply, LRows: TNyxDataValue;
  LBefore: TNyxProjectPair;
  LHandler: TNyxHandlerRef;
  LEvent, LOtherEvent: TNyxCallbackEvent;
  LInfos: TNyxAuthoredEventInfos;
  LPending: TNyxStudioPendingDesign;
  LPolicy: TNyxExecutionPolicy;
  LIndex, LChecks: Integer;
  LAction: TQueuePolicyRouteCheck;
  LLease: INyxSchemaAction;
  LGlobal: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Event queue group: ' + AReason);
    end;
    Inc(LChecks);
  end;

  procedure RefuseReply(const AReply: TNyxDataValue);
  var
    LBad: INyxPreparedDesign;
    LRefused: Boolean;
  begin
    LRefused := False;
    LBad := nil;
    try
      LBad := ReceiveNyxPreparedDesign(AReply, LRequest, LSchemas);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    LBad := nil;
    Check(LRefused and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'A forged singular group receipt refuses without publication or navigation');
  end;

  procedure Prepare(const AEdit: TNyxStudioDesignEdit);
  begin
    LBefore := LSession.ProjectSnapshot;
    LRequest := LSession.PrepareDesignRequest(AEdit, LSchemas.Revision);
    LRead := ReadNyxStudioDesignRequest(LRequest.ToData);
    Check(LRead.SameRequest(LRequest) and (LRequest.ToData.Field('version').AsInteger = 17),
      'The grouped edit retains the existing exact version-seventeen ticket');
    LPrepared := PrepareNyxStudioDesign(LRead, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'The ordinary grouped worker proposal is admitted');
    LGroup := nil;
    Check(Supports(LPrepared, INyxPreparedCallbackGroup, LGroup),
      'Grouped preparation exposes its optional bounded callback results');
    LReply := LPrepared.ToData;
    Check((LReply.Field('version').AsInteger = 2) and (LReply.Count = 7),
      'The singular helper uses the unchanged version-two reply shape');
    LReceived := ReceiveNyxPreparedDesign(LReply, LRequest, LSchemas);
  end;

begin
  LChecks := 0;
  LSchemas := CaptureNyxSchemas;
  for LIndex := 0 to 1 do
  begin
    LSession := TNyxStudioSession.Create(CreateNyxEventQueueSeed);
    try
      LSession.Select('reply-memo');
      LEvent := NyxCallbackEvent(ntBeforeKeyPress);

      if LIndex = 1 then
      begin
        LSession.Select('first-search');
        LEvent := NyxCallbackEvent(NyxSemantic(nseSearch));
      end;
      LEdit := Default(TNyxStudioDesignEdit);
      LEdit.Action := sdaCallbacks;
      LEdit.Selection := LSession.SelectedID;
      LEdit.View := LSession.ActiveViewID;
      LEdit.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
        NyxSetCallbackPolicy(LEdit.Selection, LEvent, neUIQueue),
        NyxAddCallback(LEdit.Selection, LEvent)]));
      Prepare(LEdit);
      LRows := LGroup.CallbackResults;
      LHandler := LReceived.AddedHandler;
      Check((LHandler.Name <> '') and (LPrepared.AddedHandler.Name = LHandler.Name) and
        (LRows.Count = 2) and (LRows.Item(1).Field('handler').AsText = LHandler.Name),
        'Physical and named Policy/Add groups expose exactly their sole fresh handler');
      Check((EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
        (Pos('TODO', LPrepared.Source) > 0),
        'Preparation owns useful TODO source without changing the accepted pair');
      RefuseReply(ReplaceField(LReply, 'addedHandler', NyxData('EventWorkshopNote')));
      RefuseReply(ReplaceField(LReply, 'addedHandler', NyxData('')));

      LAlternative := LEdit;

      if LIndex = 0 then
      begin
        LAlternative.Selection := 'send-button';
      end
      else
      begin
        LAlternative.Selection := 'second-search';
      end;
      LAlternative.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
        NyxSetCallbackPolicy(LAlternative.Selection, LEvent, neUIQueue),
        NyxAddCallback(LAlternative.Selection, LEvent)]));
      LAlternativeRequest := LSession.PrepareDesignRequest(LAlternative, LSchemas.Revision);
      LAlternativePrepared := PrepareNyxStudioDesign(LAlternativeRequest, LSchemas);
      Check(not LAlternativePrepared.Diagnostic.Defined,
        'The wrong-owner receipt fixture is itself an admitted independent proposal');
      LAlternativeReply := ReplaceField(LAlternativePrepared.ToData, 'request', LRequest.ToData);
      RefuseReply(LAlternativeReply);
      LAlternativePrepared := nil;

      LOtherEvent := NyxCallbackEvent(ntAfterKeyPress);

      if LIndex = 1 then
      begin
        LOtherEvent := NyxCallbackEvent(NyxSemantic(nseClear));
      end;
      LAlternative := LEdit;
      LAlternative.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
        NyxSetCallbackPolicy(LEdit.Selection, LOtherEvent, neUIQueue),
        NyxAddCallback(LEdit.Selection, LOtherEvent)]));
      LAlternativeRequest := LSession.PrepareDesignRequest(LAlternative, LSchemas.Revision);
      LAlternativePrepared := PrepareNyxStudioDesign(LAlternativeRequest, LSchemas);
      Check(not LAlternativePrepared.Diagnostic.Defined,
        'The wrong-event receipt fixture is itself an admitted independent proposal');
      RefuseReply(ReplaceField(LAlternativePrepared.ToData, 'request', LRequest.ToData));
      LAlternativePrepared := nil;

      LAlternative := LEdit;
      LAlternative.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
        NyxSetCallbackPolicy(LEdit.Selection, LEvent, neAsynchronous),
        NyxAddCallback(LEdit.Selection, LEvent)]));
      LAlternativeRequest := LSession.PrepareDesignRequest(LAlternative, LSchemas.Revision);
      LAlternativePrepared := PrepareNyxStudioDesign(LAlternativeRequest, LSchemas);
      Check(not LAlternativePrepared.Diagnostic.Defined,
        'The wrong-policy receipt fixture is itself an admitted independent proposal');
      RefuseReply(ReplaceField(LAlternativePrepared.ToData, 'request', LRequest.ToData));
      LAlternativePrepared := nil;

      Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
        'An exact grouped worker receipt publishes once');
      LInfos := NyxAuthoredEvents(LSession.Selected);
      Check((Length(LInfos) = 1) and (LInfos[0].Trigger = LEvent.Trigger) and
        (LInfos[0].Name.Name = LEvent.Name.Name) and (LInfos[0].Policy = neUIQueue) and
        (Length(LInfos[0].Callbacks) = 1) and (LSession.CallbackLine(LHandler) > 0),
        'The published sole callback, requested policy and TODO location agree');
      LSession.Undo;
      Check((EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
        not LSession.CanUndo, 'One grouped Undo restores the whole original pair');
      LSession.Redo;
      Check((LSession.CallbackLine(LHandler) > 0) and (Pos('TODO', LSession.Source) > 0),
        'Redo restores the exact group and retained helper source');
    finally
      LAlternativePrepared := nil;
      LReceived := nil;
      LGroup := nil;
      LPrepared := nil;
      LSession.Free;
    end;
  end;

  for LIndex := 0 to 1 do
  begin
    LSession := TNyxStudioSession.Create(CreateNyxEventQueueSeed);
    try
      LSession.Select('reply-memo');
      LEvent := NyxCallbackEvent(ntKeyDown);
      LEdit := Default(TNyxStudioDesignEdit);
      LEdit.Action := sdaCallbacks;
      LEdit.Selection := LSession.SelectedID;
      LEdit.View := LSession.ActiveViewID;

      if LIndex = 0 then
      begin
        LEdit.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
          NyxAddCallback(LEdit.Selection, LEvent),
          NyxSetCallbackPolicy(LEdit.Selection, LEvent, neUIQueue)]));
      end
      else
      begin
        LEdit.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
          NyxAddCallback(LEdit.Selection, LEvent),
          NyxAddCallback('send-button', NyxCallbackEvent(ntClick))]));
      end;
      Prepare(LEdit);
      LRows := LGroup.CallbackResults;
      Check((LRows.Count = 2) and (LPrepared.AddedHandler.Name = '') and
        (LReceived.AddedHandler.Name = ''),
        'Reversed and generic multi-add batches retain plural results without a scalar helper');
      RefuseReply(ReplaceField(LReply, 'addedHandler', LRows.Item(0).Field('handler')));
      Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
        'Ordinary non-pair callback grouping still publishes its complete candidate');
      LSession.Undo;
      Check((EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
        not LSession.CanUndo, 'Generic grouped callbacks still have exactly one paired Undo');
    finally
      LReceived := nil;
      LGroup := nil;
      LPrepared := nil;
      LSession.Free;
    end;
  end;

  LPending := Default(TNyxStudioPendingDesign);
  SetLength(LPending.EventLocks, 1);
  LPending.EventLocks[0].Owner := 'reply-memo';
  LPending.EventLocks[0].Trigger := ntKeyDown;
  Check(LPending.EventLocked('reply-memo', ntKeyDown, Default(TNyxEventRef)) and
    not LPending.EventLocked('send-button', ntKeyDown, Default(TNyxEventRef)) and
    not LPending.EventLocked('reply-memo', ntAfterKeyDown, Default(TNyxEventRef)),
    'Grouped locks use exact owner and physical event identity');
  LPending.EventLocks[0].Trigger := ntNamed;
  LPending.EventLocks[0].Name := NyxEvent('Workshop.Open');
  Check(LPending.EventLocked('reply-memo', ntNamed, NyxEvent('Workshop.Open')) and
    not LPending.EventLocked('reply-memo', ntNamed, NyxEvent('workshop.open')) and
    not LPending.EventLocked('reply-memo', ntNamed, NyxEvent('Workshop.Close')),
    'Named group locks preserve exact case and leave unrelated streams available');
  SetLength(LPending.Events, 3);
  LPending.Events[0].Owner := 'reply-memo';
  LPending.Events[0].Intent.Action := seaPolicy;
  LPending.Events[0].Intent.Trigger := ntNamed;
  LPending.Events[0].Intent.Name := NyxEvent('Workshop.Open');
  LPending.Events[0].Intent.Policy := neAsynchronous;
  LPending.Events[1] := LPending.Events[0];
  LPending.Events[1].Intent.Policy := neUIQueue;
  LPending.Events[2].Owner := 'send-button';
  LPending.Events[2].Intent.Action := seaRemove;
  LPending.Events[2].Intent.Trigger := ntClick;
  Check(LPending.EventPolicy('reply-memo', ntNamed, NyxEvent('Workshop.Open'), LPolicy) and
    (LPolicy = neUIQueue), 'Ordered policy overlays expose the latest exact waiting choice');
  Check(not LPending.EventPolicy('reply-memo', ntNamed, NyxEvent('workshop.open'), LPolicy) and
    (LPolicy = neSequential), 'An unrelated policy lookup returns a cleared default');
  Check(LPending.EventLocked('send-button', ntClick, Default(TNyxEventRef)),
    'Existing exact confirmed-removal locks remain effective beside grouped locks');

  LLegacy := TNyxStudioSession.Create(CreateNyxEventQueueSeed);
  try
    LLegacy.Select('reply-memo');
    LLegacy.SetCallbackPolicy(ntKeyDown, neSequential);
    LGlobal := CaptureNyxSchemas.ToData.ToJSON;
    LRestricted := QueuePolicySnapshot;
    LAction := TQueuePolicyRouteCheck.Create;
    LLease := LAction;
    LAction.Legacy := LLegacy;
    LRestricted.Execute(LLease);
    Inc(LChecks, LAction.Checks);
    LLease := nil;
    LAction := nil;
    Check(CaptureNyxSchemas.ToData.ToJSON = LGlobal,
      'Synchronous creator-policy checks leave the global schema environment unchanged');
  finally
    LLease := nil;
    LRestricted := nil;
    LLegacy.Free;
  end;
  Result := LChecks;
end;

function RunNyxEventQueueJourney(out APair: TNyxProjectPair): Integer;
var
  LSession: TNyxStudioSession;
  LOther: TNyxStudioSession;
  LSchemas: INyxSchemaSnapshot;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LRead: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LReceived: INyxPreparedDesign;
  LBefore: TNyxProjectPair;
  LWire: TNyxDataValue;
  LReply: TNyxDataValue;
  LHandler: TNyxHandlerRef;
  LSecond: TNyxHandlerRef;
  LLine: Integer;
  LEvents: TNyxAuthoredEventInfos;
  LProjection: TNyxNode;
  LCommand: TNyxNode;
  LInspector: TNyxNode;
  LReview: TNyxCallbackRemoval;
  LRemoval: TNyxCallbackRemoval;
  LEffect: TNyxInspectorEffect;
  LFailed: Boolean;
  LChecks: Integer;
  LIndex: Integer;
  LName: TNyxEventRef;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Event queue: ' + AReason);
    end;
    Inc(LChecks);
  end;

  procedure Intent(AAction: TNyxStudioEventAction; ATrigger: TNyxTrigger;
    const AName: TNyxEventRef);
  begin
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaEvent;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Event.Action := AAction;
    LEdit.Event.Trigger := ATrigger;
    LEdit.Event.Name := AName;
  end;

  procedure Discovery;
  var
    LRoot: TNyxNode;
    LClone: TNyxNode;
    LSchema: TNyxEventSchema;
    LEvent: TNyxAuthoredEventInfo;
    LResult: TNyxEventFilterResult;
    LFilter: TNyxEventFilter;
    LOtherFilter: TNyxEventFilter;
    LRefused: Boolean;
    LBeforeVisible: TNyxText;
  begin
    LRoot := TNyxNode.Create(nkColumn, 'discovery');
    LClone := nil;
    try
      LRoot.Add(TNyxNode.Create(nkCard, 'keyboard-card'));
      LRoot.Add(TNyxNode.Create(nkCard, 'focus-card'));
      LSchema := Default(TNyxEventSchema);
      LSchema.Trigger := ntBeforeKeyPress;
      LSchema.Title := 'OnBeforeKeyPress';
      LSchema.Description := TNyxText('Keyboard notes 🎵');
      LSchema.Contexts := [nctxKeyboard];
      LEvent := Default(TNyxAuthoredEventInfo);
      SetLength(LEvent.Callbacks, 1);
      LEvent.Callbacks[0].Handler := NyxHandler('TReplyBeforeKeyPress');
      LEvent.Callbacks[0].ID := NyxCallbackID('reply-key');
      TagNyxEventDiscovery(LRoot.Children[0], LSchema, LEvent);
      LEvent.Callbacks := nil;
      LSchema := Default(TNyxEventSchema);
      LSchema.Trigger := ntAfterEnter;
      LSchema.Title := 'OnAfterEnter';
      TagNyxEventDiscovery(LRoot.Children[1], LSchema, LEvent);
      LRoot.Add(TNyxNode.Create(nkLabel, 'unrelated'));
      LResult := ApplyNyxEventFilter(LRoot, NyxEventFilter);
      Check((LResult.Total = 2) and (LResult.Matching = 2) and (LResult.Shown = 2),
        'Discovery counts only tagged cards');
      LResult := ApplyNyxEventFilter(LRoot, NyxEventFilter.Query('KEY press'));
      Check((LResult.Matching = 1) and (LRoot.Children[0].Prop('visible') = 'true') and
        (LRoot.Children[1].Prop('visible') = 'false'),
        'Discovery requires every ASCII-insensitive search term');
      LResult := ApplyNyxEventFilter(LRoot, NyxEventFilter.Query('TReplyBeforeKeyPress'));
      Check(LResult.Matching = 1, 'Copied callback identities survive source-array retirement');
      LResult := ApplyNyxEventFilter(LRoot, NyxEventFilter.Query(TNyxText('🎵')));
      Check(LResult.Matching = 1, 'Supplementary Unicode remains searchable exactly');
      LFilter := NyxEventFilter.Category(necFocus);
      LOtherFilter := LFilter.Category(necKeyboard);
      Check((LFilter.CategoryValue = necFocus) and (LOtherFilter.CategoryValue = necKeyboard),
        'Fluent discovery choices are independent copies');
      LResult := ApplyNyxEventFilter(LRoot, NyxEventFilter.Query('focus'));
      Check(LResult.Matching = 1, 'Intent captions make focus events searchable by their family');
      LResult := ApplyNyxEventFilter(LRoot, LFilter.WithCallbacks(True));
      Check(LResult.Matching = 0, 'Family and registration filters combine');
      TagNyxEventDiscovery(LRoot.Children[1], LSchema, LEvent, True);
      LResult := ApplyNyxEventFilter(LRoot, NyxEventFilter.Query('missing callback'));
      Check((LResult.Matching = 0) and (LResult.Pinned = 1) and (LResult.Shown = 1),
        'A pending review remains visible outside the query');
      LClone := LRoot.Clone;
      ReleaseNyxNode(LRoot);
      LRoot := nil;
      LResult := ApplyNyxEventFilter(LClone, NyxEventFilter.WithCallbacks(True));
      Check((LResult.Matching = 1) and (LResult.Shown = 2),
        'Independent realization context outlives the authored discovery tree');
      LBeforeVisible := LClone.Children[0].Prop('visible');
      LClone.Children[1].Extensions.SetValue(NyxExtension('nyx.event-discovery'),
        NyxObject([NyxField('version', NyxData(1)),
          NyxField('categories', NyxArray([])), NyxField('search', NyxData('bad')),
          NyxField('registered', NyxData('true')), NyxField('pinned', NyxData(False))]));
      LRefused := False;
      try
        ApplyNyxEventFilter(LClone, NyxEventFilter.Query('unmatched'));
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LClone.Children[0].Prop('visible') = LBeforeVisible),
        'Malformed later context refuses before changing an earlier card');
      LClone.Add(NewNyxEventFilterEditor('event-form',
        NyxEventFilter.Query('keys').Category(necKeyboard).WithCallbacks(True)));
      Check(ReadNyxEventFilterEditor('event-form', LClone, LFilter) and
        (LFilter.SearchText = 'keys') and (LFilter.CategoryValue = necKeyboard) and
        LFilter.RegisteredOnly, 'Typed filter form reads exact query/family/Boolean');
      LClone.Find(NyxEventFilterPartID('event-form', nefpRegistered))
        .SetProp(NyxAttributeName(atValue), 'yes');
      LRefused := False;
      try
        ReadNyxEventFilterEditor('event-form', LClone, LFilter);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Malformed Boolean input refuses rather than silently choosing False');
    finally
      ReleaseNyxNode(LRoot);
      ReleaseNyxNode(LClone);
    end;
  end;

  procedure Prepare;
  begin
    LBefore := LSession.ProjectSnapshot;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LWire := LRequest.ToData;
    LRead := ReadNyxStudioDesignRequest(LWire);
    Check(LRequest.SameRequest(LRead), 'Version-four ticket retains exact typed event intent');
    LPrepared := PrepareNyxStudioDesign(LRead, LSchemas);
    LReply := LPrepared.ToData;
    LReceived := ReceiveNyxPreparedDesign(LReply, LRequest, LSchemas);
  end;

  procedure Admit;
  begin
    Prepare;
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
      'Independent event command publishes one design/source pair');
    Check(Pos('function EventWorkshopNote', LSession.Source) > 0,
      'Application helper survives source regeneration');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One Undo restores the exact pair and draft');
    LSession.Redo;
    LReceived := nil;
    LPrepared := nil;
  end;

  procedure RefuseWire(const AData: TNyxDataValue);
  begin
    LFailed := False;
    try
      LRead := ReadNyxStudioDesignRequest(AData);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Malformed or unrelated event descriptor refuses before preparation');
  end;

  procedure RefuseReply(const AData: TNyxDataValue);
  var
    LBad: INyxPreparedDesign;
  begin
    LFailed := False;
    try
      LBad := ReceiveNyxPreparedDesign(AData, LRequest, LSchemas);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    LBad := nil;
    Check(LFailed and (LBad = nil) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'False receipt cannot navigate or publish a pair');
  end;

begin
  LChecks := 0;
  Discovery;
  LSession := TNyxStudioSession.Create(CreateNyxEventQueueSeed);
  LOther := TNyxStudioSession.Create(CreateNyxEventQueueSeed);
  try
    LSchemas := CaptureNyxSchemas;
    LSession.Select('reply-memo');
    Intent(seaAdd, ntBeforeKeyPress, Default(TNyxEventRef));
    Prepare;
    LHandler := LReceived.AddedHandler;
    Check((LHandler.Name <> '') and (LSession.Source = LBefore.Source),
      'Preparation returns its handler without publishing accepted source');
    RefuseReply(ReplaceField(LReply, 'addedHandler', NyxData('EventWorkshopNote')));
    RefuseReply(ReplaceField(LReply, 'addedHandler', NyxData('')));
    LSession.Select('send-button');
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
      'Captured owner admission permits independent later navigation');
    Check((LSession.SelectedID = 'send-button') and
      (LSession.CallbackLine(LHandler) > 0) and (Pos('TODO', LSession.Source) > 0),
      'Publication keeps later selection and creates a real Pascal TODO stub');
    LReceived := nil;
    LPrepared := nil;

    LSession.Select('reply-memo');
    Intent(seaAdd, ntBeforeKeyPress, Default(TNyxEventRef));
    Prepare;
    LSecond := LReceived.AddedHandler;
    Check(LSecond.Name <> LHandler.Name, 'Multiple additions receive distinct crafted handler names');
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
      'Second registration publishes independently');
    LReceived := nil;
    LPrepared := nil;
    Intent(seaPolicy, ntBeforeKeyPress, Default(TNyxEventRef));
    LEdit.Event.Policy := neUIQueue;
    Admit;
    Check(Pos(LHandler.Name, LSession.Source) > 0, 'Policy edit retains all implementations');

    Intent(seaRemove, ntBeforeKeyPress, Default(TNyxEventRef));
    LEdit.Event.ID := NyxCallbackID(LHandler.Name);
    LEdit.Event.Handler := LSecond;
    Prepare;
    Check((LSession.CompleteDesignRequest(LRequest, LReceived) = nscRejected) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'Removal refuses a reviewed ID whose handler has changed');
    RefuseReply(ReplaceField(LReply, 'addedHandler', NyxData(LHandler.Name)));
    LReceived := nil;
    LPrepared := nil;
    LEdit.Event.Handler := LHandler;
    Admit;
    Check(LSession.CallbackLine(LHandler) > 0, 'Removing a registration retains handwritten implementation');

    LSession.SetSourceDraft(LSession.Source + #10 + '{ Independent application draft }');
    Intent(seaAdd, ntAfterKeyPress, Default(TNyxEventRef));
    Prepare;
    Check((LSession.CompleteDesignRequest(LRequest, LReceived) = nscRejected) and
      (LReceived.AddedHandler.Name = '') and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'Addition refuses pending Pascal rather than erasing handwritten work');
    LReceived := nil;
    LPrepared := nil;
    Intent(seaPolicy, ntBeforeKeyPress, Default(TNyxEventRef));
    LEdit.Event.Policy := neSequential;
    Prepare;
    Check((LSession.CompleteDesignRequest(LRequest, LReceived) = nscRejected) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'Policy changes refuse pending Pascal and retain the complete pair');
    Check(LSession.DraftSource = LBefore.Draft, 'Ordinary policy edit retains independent draft/base');
    LReceived := nil;
    LPrepared := nil;
    LSession.DiscardSourceDraft;

    LSession.Select('first-search');
    Intent(seaAdd, ntNamed, NyxSemantic(nseSearch));
    Admit;
    LEvents := NyxAuthoredEvents(LSession.Selected);
    Check((Length(LEvents) = 1) and (Length(LEvents[0].Callbacks) = 1),
      'Named compound event addition owns a local registration');
    Check(not LSession.Document.Find('second-search').Extensions.Has(NyxExtension(NyxCallbacksKey)),
      'Another reusable instance owns no added callback');

    Intent(seaPolicy, ntNamed, NyxSemantic(nseSearch));
    LEdit.Event.Policy := neAsynchronous;
    Prepare;
    LName := NyxSemantic(nseSearch);
    for LIndex := 0 to 5 do
    begin
      case LIndex of
        0: RefuseWire(ReplaceField(LWire, 'version', NyxData(3)));
        1: RefuseWire(ReplaceField(LWire, 'edit',
          ReplaceField(LWire.Field('edit'), 'event', NyxNull)));
        2: RefuseWire(ReplaceField(LWire, 'edit', ReplaceField(LWire.Field('edit'), 'event',
          ReplaceField(LWire.Field('edit').Field('event'), 'policy', NyxData(999)))));
        3: RefuseWire(ReplaceField(LWire, 'edit', ReplaceField(LWire.Field('edit'), 'event',
          ReplaceField(LWire.Field('edit').Field('event'), 'name', NyxData('')))));
        4: RefuseWire(ReplaceField(LWire, 'edit', ReplaceField(LWire.Field('edit'), 'event',
          ReplaceField(LWire.Field('edit').Field('event'), 'id', NyxData('unreviewed')))));
        5: RefuseWire(ReplaceField(LWire, 'edit',
          ReplaceField(LWire.Field('edit'), 'action', NyxData(Ord(sdaTitle)))));
      end;
    end;
    Check(LOther.CompleteDesignRequest(LRequest, LReceived) = nscStale,
      'Another session refuses the same value ticket');
    LSession.SetSourceDraft(LSession.Source + #10 + '{ Newer local typing }');
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscStale,
      'Later handwritten draft refuses an older event result');
    LSession.DiscardSourceDraft;
    LReceived := nil;
    LPrepared := nil;

    { A confirmation warning is tied to the session/load, not just matching IDs. }
    LProjection := LSession.SelectedProjection;
    LCommand := TNyxNode.Create(nkButton, 'review-registration');
    try
      LEvents := NyxAuthoredEvents(LProjection);
      LCommand.Configure.Extension(NyxStudioEventCommandKey, 'request-removal')
        .Extension(NyxStudioEventOwnerKey, LSession.SelectedID)
        .Extension(NyxStudioEventTriggerKey, NyxTriggerName(ntNamed))
        .Extension(NyxStudioEventNameKey, LName.Name)
        .Extension(NyxStudioEventIDKey, LEvents[0].Callbacks[0].ID.Name).Done;
      Check(CaptureNyxStudioEvents(LSession, LCommand, ntClick,
        Default(TNyxCallbackRemoval), Default(TNyxStudioPendingDesign), LEdit,
        LEffect, LLine, LReview) and (LEffect = nieRequestRemoval),
        'Request exposes a warning without publication');
      APair := LSession.ProjectSnapshot;
      LSession.LoadProject(APair);
      LSession.Select('first-search');
      LInspector := TNyxNode.Create(nkColumn, 'retired-removal-inspector');
      try
        AddNyxEventsInspector(LInspector, LSession, LProjection, LReview);
        Check(LInspector.Find('event-removal-warning') = nil,
          'Reload retires the visible warning even when its owner and IDs still match');
      finally
        LInspector.Free;
      end;
      LCommand.Configure.Extension(NyxStudioEventCommandKey, 'confirm-removal').Done;
      LFailed := False;
      try
        CaptureNyxStudioEvents(LSession, LCommand, ntClick, LReview,
          Default(TNyxStudioPendingDesign), LEdit, LEffect, LLine, LRemoval);
      except
        on Exception do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed and not LSession.CanUndo,
        'Identical reloaded IDs cannot reuse a stale removal warning');
    finally
      LCommand.Free;
      LProjection.Free;
    end;

    Intent(seaPolicy, ntNamed, NyxSemantic(nseSearch));
    LEdit.Event.Policy := neUIQueue;
    Prepare;
    LSession.LoadProject(LSession.ProjectSnapshot);
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscStale,
      'Reload retires an already prepared event result');
  finally
    LReceived := nil;
    LPrepared := nil;
    LOther.Free;
    LSession.Free;
  end;
  Inc(LChecks, RunPolicyEventQueueCases);
  Result := LChecks;
end;

end.
