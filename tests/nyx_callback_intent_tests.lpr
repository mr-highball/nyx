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
program nyx_callback_intent_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.types, nyx.schema, nyx.source,
  nyx.callbacks, nyx.scheduler, nyx.studio.projects, nyx.studio.session,
  nyx.studio.callbackintent, nyx.studio.callbackedits,
  nyx.studio.handlerintent, nyx.studio.handleredits
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Inert inspection uses the real queued snapshot boundary. These cases neither
  create a session nor infer permission/stream existence from a recognized shape. }
procedure InspectionCases;
var
  LPair, LNamedPair, LBounded: TNyxCallbackBatchSnapshot;
  LChanges, LAgain, LMaximum: TNyxCallbackChanges;
  LWireItems: array of TNyxDataValue;
  LOwner: TNyxText;
  LEvent: TNyxCallbackEvent;
  LPolicy: TNyxExecutionPolicy;
  LIndex: Integer;
  LRejected: Boolean;

  procedure NotPair(const AChanges: array of TNyxCallbackChange;
    const AReason: TNyxText);
  begin
    LOwner := 'previous-owner';
    LEvent := NyxCallbackEvent(NyxEvent('previous-event'));
    LPolicy := neThreaded;
    Check(not TryNyxCallbackPolicyAddShape(
      CaptureNyxCallbackBatch(NyxCallbackPatch(AChanges)), LOwner, LEvent, LPolicy),
      AReason);
    Check((LOwner = '') and (LEvent.Trigger = ntClick) and
      (LEvent.Name.Name = '') and (LPolicy = neSequential),
      AReason + ' clears every output');
  end;

  procedure RefuseWire(const AWire: TNyxDataValue; const AReason: TNyxText);
  var
    LRefused: Boolean;
  begin
    LRefused := False;
    try
      ReadNyxCallbackChanges(ReadNyxCallbackBatchSnapshot(AWire));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, AReason);
  end;

begin
  LPair := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxSetCallbackPolicy('project-name', NyxCallbackEvent(ntKeyDown), neUIQueue),
    NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown))]));
  Check(TryNyxCallbackPolicyAddShape(LPair, LOwner, LEvent, LPolicy) and
    (LOwner = 'project-name') and (LEvent.Trigger = ntKeyDown) and
    (LEvent.Name.Name = '') and (LPolicy = neUIQueue),
    'physical-event Policy then Add retains exact typed identity and policy');
  LNamedPair := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxSetCallbackPolicy('project-name', NyxCallbackEvent(NyxEvent('query-before')), neSequential),
    NyxAddCallback('project-name', NyxCallbackEvent(NyxEvent('query-before')))]));
  Check(TryNyxCallbackPolicyAddShape(LNamedPair, LOwner, LEvent, LPolicy) and
    (LEvent.Trigger = ntNamed) and (LEvent.Name.Name = 'query-before') and
    (LPolicy = neSequential), 'named-event shape retains its exact open name');
  LChanges := ReadNyxCallbackChanges(LPair);
  Check((Length(LChanges) = 2) and (LChanges[0].Kind = nccPolicy) and
    (LChanges[1].Kind = nccAdd), 'inspection retains ordered operation meanings');
  LAgain := ReadNyxCallbackChanges(LPair);
  LChanges[0] := NyxAddCallback('other-owner', NyxCallbackEvent(ntClick));
  Check((LAgain[0].Kind = nccPolicy) and (LAgain[0].Owner = 'project-name'),
    'replacing an inspected member cannot change a peer read');
  LChanges := ReadNyxCallbackChanges(LPair);
  Check(LChanges[0].Policy = neUIQueue,
    'replacing an inspected member cannot change the queued snapshot');
  LChanges := ReadNyxCallbackChanges(CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxMoveCallback('project-name', NyxCallbackEvent(ntKeyDown), NyxCallbackID('registration'), 2),
    NyxRemoveCallback('project-name', NyxCallbackEvent(ntKeyDown), NyxCallbackID('registration'))])));
  Check((LChanges[0].Kind = nccMove) and (LChanges[0].Position = 2) and
    (LChanges[0].Registration.Name = 'registration') and
    (LChanges[1].Kind = nccRemove) and (LChanges[1].Registration.Name = 'registration'),
    'inspection preserves typed registration and position meanings for generic changes');

  NotPair([NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown)),
    NyxSetCallbackPolicy('project-name', NyxCallbackEvent(ntKeyDown), neUIQueue)],
    'reversed Add then Policy remains a generic batch');
  NotPair([NyxSetCallbackPolicy('project-name', NyxCallbackEvent(ntKeyDown), neUIQueue),
    NyxAddCallback('Project-name', NyxCallbackEvent(ntKeyDown))],
    'case-distinct owners are not one exact policy-addition target');
  NotPair([NyxSetCallbackPolicy('project-name', NyxCallbackEvent(ntKeyDown), neUIQueue),
    NyxAddCallback('project-name', NyxCallbackEvent(ntChange))],
    'different physical triggers are not one policy-addition target');
  NotPair([NyxSetCallbackPolicy('project-name', NyxCallbackEvent(NyxEvent('query-before')), neUIQueue),
    NyxAddCallback('project-name', NyxCallbackEvent(NyxEvent('Query-before')))],
    'case-distinct named events are not one policy-addition target');
  NotPair([NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown))],
    'an ordinary single Add does not acquire a policy-pair meaning');
  NotPair([NyxSetCallbackPolicy('project-name', NyxCallbackEvent(ntKeyDown), neUIQueue),
    NyxMoveCallback('project-name', NyxCallbackEvent(ntKeyDown), NyxCallbackID('registration'), 0)],
    'a policy followed by a registration move is not a callback addition');
  NotPair([NyxSetCallbackPolicy('project-name', NyxCallbackEvent(ntKeyDown), neUIQueue),
    NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown)),
    NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown))],
    'generic multiple additions do not choose one scalar navigation handler');

  SetLength(LMaximum, NyxMaximumCallbackChanges);
  for LIndex := 0 to High(LMaximum) do
  begin
    LMaximum[LIndex] := NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown));
  end;
  LBounded := CaptureNyxCallbackBatch(NyxCallbackPatch(LMaximum));
  Check(Length(ReadNyxCallbackChanges(LBounded)) = NyxMaximumCallbackChanges,
    'inspection preserves the existing complete 32-member batch budget');
  SetLength(LWireItems, NyxMaximumCallbackChanges + 1);
  for LIndex := 0 to High(LWireItems) do
  begin
    LWireItems[LIndex] := LPair.ToData.Item(1);
  end;
  RefuseWire(NyxArray(LWireItems), 'inspection cannot admit an over-budget wire batch');
  RefuseWire(NyxArray([]), 'inspection cannot admit an empty wire batch');
  RefuseWire(NyxArray([NyxObject([
    NyxField('op', NyxData('add')), NyxField('id', NyxData('')),
    NyxField('event', NyxObject([NyxField('trigger', NyxData('key-down'))]))])]),
    'inspection retains exact owner-reference admission');
  RefuseWire(NyxArray([NyxObject([
    NyxField('op', NyxData('add')), NyxField('id', NyxData('project-name')),
    NyxField('event', NyxObject([NyxField('trigger', NyxData('key-down'))])),
    NyxField('registration', NyxData('not-an-add-field'))])]),
    'inspection retains action-specific closed fields');
  RefuseWire(NyxArray([NyxObject([
    NyxField('op', NyxData('policy')), NyxField('id', NyxData('project-name')),
    NyxField('event', NyxObject([NyxField('trigger', NyxData('key-down'))])),
    NyxField('policy', NyxData(True))])]),
    'inspection rejects a Boolean in the closed policy family');
  RefuseWire(NyxArray([NyxObject([
    NyxField('op', NyxData('policy')), NyxField('id', NyxData('project-name')),
    NyxField('event', NyxObject([NyxField('trigger', NyxData('key-down'))])),
    NyxField('policy', NyxData('later'))])]),
    'inspection rejects unknown execution-policy names');
  RefuseWire(NyxArray([NyxObject([
    NyxField('op', NyxData('add')), NyxField('id', NyxData('project-name')),
    NyxField('event', NyxObject([NyxField('trigger', NyxData('key-down')),
      NyxField('name', NyxData('query-before'))]))])]),
    'inspection rejects an ambiguous trigger and name');
  RefuseWire(NyxArray([NyxObject([
    NyxField('op', NyxData('add')), NyxField('id', NyxData('project-name')),
    NyxField('event', NyxObject([NyxField('trigger', NyxData('not-a-trigger'))]))])]),
    'inspection rejects an unknown event trigger');
  RefuseWire(NyxArray([NyxObject([
    NyxField('op', NyxData('move')), NyxField('id', NyxData('project-name')),
    NyxField('event', NyxObject([NyxField('trigger', NyxData('key-down'))])),
    NyxField('registration', NyxData('registration')), NyxField('index', NyxData('zero'))])]),
    'inspection retains the Integer position family');
  LRejected := False;
  try
    ReadNyxCallbackChanges(Default(TNyxCallbackBatchSnapshot));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'an undefined snapshot cannot enter typed inspection');
  LOwner := 'previous-owner';
  LEvent := NyxCallbackEvent(NyxEvent('previous-event'));
  LPolicy := neThreaded;
  LRejected := False;
  try
    TryNyxCallbackPolicyAddShape(Default(TNyxCallbackBatchSnapshot),
      LOwner, LEvent, LPolicy);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LOwner = '') and (LEvent.Name.Name = '') and
    (LPolicy = neSequential), 'undefined shape inspection refuses with cleared outputs');
end;

{ A real independent planner/worker/history case proves that inspection does not
  replace normal admission. This deliberately uses an absent unrestricted stream
  with an explicit nondefault policy, then retains one exact Add/TODO result. }
procedure PolicyAddCase;
var
  LSession: TNyxStudioSession;
  LBefore, LAfter: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LSchemas: INyxSchemaSnapshot;
  LPrepared, LReceived: INyxPreparedDesign;
  LGroup: INyxPreparedCallbackGroup;
  LRows: TNyxDataValue;
  LInfos: TNyxAuthoredEventInfos;
begin
  LSession := TNyxStudioSession.Create;
  try
    LSession.Select('project-name');
    LBefore := LSession.ProjectSnapshot;
    Check(Length(NyxAuthoredEvents(LSession.Document.Find('project-name'))) = 0,
      'the policy-addition journey starts with no authored stream');
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaCallbacks;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
      NyxSetCallbackPolicy('project-name', NyxCallbackEvent(ntKeyDown), neUIQueue),
      NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown))]));
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    Check((LRequest.IntentData.Field('version').AsInteger = 17) and
      ReadNyxStudioDesignRequest(LRequest.ToData).SameRequest(LRequest),
      'the exact policy-addition pair uses the existing complete request shape');
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined and
      Supports(LPrepared, INyxPreparedCallbackGroup, LGroup),
      'Policy then Add prepares as one ordinary grouped candidate');
    LRows := LGroup.CallbackResults;
    Check((LRows.Count = 2) and (LRows.Item(0).Field('op').AsText = 'policy') and
      (LRows.Item(1).Field('op').AsText = 'add') and
      (LRows.Item(1).Field('policy').AsText = NyxPolicyName(neUIQueue)),
      'the Add result observes the earlier explicit policy in the same candidate');
    Check((LPrepared.AddedHandler.Name <> '') and
      (LPrepared.AddedHandler.Name = LRows.Item(1).Field('handler').AsText),
      'only the exact pair returns its sole added handler for navigation');
    LReceived := ReceiveNyxPreparedDesign(LPrepared.ToData, LRequest, LSchemas);
    Check((LReceived.AddedHandler.Name = LPrepared.AddedHandler.Name) and
      (Pos('TODO', ReadNyxHandlerSource(LReceived.Source, LReceived.AddedHandler).Code) > 0),
      'the strict worker receipt retains the exact fresh handler and editable TODO');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'policy-addition preparation and receipt leave the accepted pair unchanged');
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
      'one completion admits policy and callback together');
    LAfter := LSession.ProjectSnapshot;
    LInfos := NyxAuthoredEvents(LSession.Document.Find('project-name'));
    Check((Length(LInfos) = 1) and (LInfos[0].Policy = neUIQueue) and
      (Length(LInfos[0].Callbacks) = 1) and
      (LInfos[0].Callbacks[0].Handler.Name = LReceived.AddedHandler.Name),
      'the admitted stream has its requested policy and sole exact registration');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one Undo removes the complete policy-addition pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores the complete policy-addition pair');
  finally
    LSession.Free;
  end;
end;

{ Shared record/worker boundary qualification runs unchanged in checked native
  and the actual browser. It uses independent literal sessions; executed
  handwritten publication belongs to the production semantic/compiler consumer. }
procedure Execute;
var
  LSession: TNyxStudioSession;
  LOther: TNyxStudioSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LDecoded: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LReceived: INyxPreparedDesign;
  LGroup: INyxPreparedCallbackGroup;
  LSchemas: INyxSchemaSnapshot;
  LRows: TNyxDataValue;
  LWire: TNyxDataValue;
  LHandler: TNyxHandlerRef;
  LSnapshot: TNyxCallbackBatchSnapshot;
  LInfos: TNyxAuthoredEventInfos;
  LRejected: Boolean;
  LBody: TNyxHandlerSource;
  LHandlerGroup: INyxPreparedHandlerGroup;
begin
  LSession := TNyxStudioSession.Create;
  LOther := nil;
  try
    LSession.Select('project-name');
    LBefore := LSession.ProjectSnapshot;
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaCallbacks;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
      NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown)),
      NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown)),
      NyxSetCallbackPolicy('project-name', NyxCallbackEvent(ntKeyDown), neUIQueue)]));
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LDecoded := ReadNyxStudioDesignIntent(LRequest.IntentData);
    Check((LRequest.IntentData.Field('version').AsInteger = 17) and
      (LDecoded.Callbacks.ToData.ToJSON = LEdit.Callbacks.ToData.ToJSON),
      'grouped intent retains exact normalized choices without a source authority');
    LWire := LRequest.ToData;
    Check(ReadNyxStudioDesignRequest(LWire).SameRequest(LRequest),
      'literal worker ticket retains the complete grouped request');
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined and not LPrepared.RequiresCompilation,
      'literal group prepares independently: ' + LPrepared.Diagnostic.Message);
    Check(Supports(LPrepared, INyxPreparedCallbackGroup, LGroup),
      'optional proposal context preserves the existing prepared interface');
    LRows := LGroup.CallbackResults;
    Check((LRows.Count = 3) and
      (LRows.Item(0).Field('handler').AsText <> LRows.Item(1).Field('handler').AsText),
      'group addition reserves every handler independently');
    LHandler := NyxHandler(LRows.Item(0).Field('handler').AsText);
    Check(Pos('TODO', ReadNyxHandlerSource(LPrepared.Source, LHandler).Code) > 0,
      'portable planner produces a real editable callback stub');
    LReceived := ReceiveNyxPreparedDesign(LPrepared.ToData, LRequest, LSchemas);
    Check((LReceived.Source = LPrepared.Source) and (LReceived.Design = LPrepared.Design),
      'ordinary worker result preserves the exact candidate pair');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'preparation and wire receipt do not mutate the supplier');
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
      'one ordinary completion admits the entire literal group');
    LAfter := LSession.ProjectSnapshot;
    LInfos := NyxAuthoredEvents(LSession.Document.Find('project-name'));
    Check((Length(LInfos) = 1) and (Length(LInfos[0].Callbacks) = 2) and
      (LInfos[0].Policy = neUIQueue), 'published typed events retain policy and ordered membership');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one Undo restores the exact source/design baseline');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores the exact complete callback group');
    LOther := TNyxStudioSession.Create(LAfter);
    LOther.Select('project-name');
    LEdit.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
      NyxMoveCallback('project-name', NyxCallbackEvent(ntKeyDown), LInfos[0].Callbacks[1].ID, 0),
      NyxRemoveCallback('project-name', NyxCallbackEvent(ntKeyDown), LInfos[0].Callbacks[0].ID)]));
    LRequest := LOther.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'ordered move/removal group prepares');
    Check(LOther.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'ordinary move/removal group publishes once');
    Check(Pos('TODO', ReadNyxHandlerSource(LOther.Source, LHandler).Code) > 0,
      'unregistering retains the original Pascal implementation');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'independent planning and mutation preserve the original owner');
    { The body snapshot also crosses both compilers without retaining a patch
      interface in its queued record. It can edit a retained unregistered class. }
    LBefore := LOther.ProjectSnapshot;
    LBody := ReadNyxHandlerSource(LOther.Source, LHandler);
    LEdit.Action := sdaHandlers;
    LEdit.Handlers := CaptureNyxHandlerBatch(NewNyxHandlerPatch([
      NyxHandlerEdit(LHandler, LBody.Code,
        'begin' + #10 + '  { Crafted Unicode 🚀 𐐷. }' + #10 + 'end;')]));
    LRequest := LOther.PrepareDesignRequest(LEdit, LSchemas.Revision);
    Check((LRequest.IntentData.Field('version').AsInteger = 18) and
      (ReadNyxStudioDesignIntent(LRequest.IntentData).Handlers.ToData.ToJSON =
      LEdit.Handlers.ToData.ToJSON), 'body intent retains exact typed expected/replacement data');
    Check(ReadNyxStudioDesignRequest(LRequest.ToData).SameRequest(LRequest),
      'body worker ticket retains its full immutable intent');
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined and
      Supports(LPrepared, INyxPreparedHandlerGroup, LHandlerGroup),
      'literal body group prepares with optional bounded navigation');
    Check(LHandlerGroup.HandlerResults.Count = 1, 'body proposal returns only its exact source location');
    LReceived := ReceiveNyxPreparedDesign(LPrepared.ToData, LRequest, LSchemas);
    Check(LOther.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
      'ordinary worker-value body result admits through paired history');
    Check((LOther.Save = LBefore.Design) and
      (Pos('🚀 𐐷', ReadNyxHandlerSource(LOther.Source, LHandler).Code) > 0),
      'body admission retains design and exact supplementary Unicode');
    LOther.Undo;
    Check(EncodeNyxProject(LOther.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'body group has one exact paired Undo');
    LOther.Redo;
    Check(Pos('🚀 𐐷', ReadNyxHandlerSource(LOther.Source, LHandler).Code) > 0,
      'body group has one exact paired Redo');
    LBefore := LOther.ProjectSnapshot;
    LEdit.Action := sdaCallbacks;
    LEdit.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
      NyxAddCallback('project-name', NyxCallbackEvent(ntKeyDown)),
      NyxAddCallback('missing-control', NyxCallbackEvent(ntKeyDown))]));
    LPrepared := PrepareNyxStudioDesign(LOther.PrepareDesignRequest(LEdit, LSchemas.Revision), LSchemas);
    Check(LPrepared.Diagnostic.Defined and (LPrepared.Source = ''),
      'invalid final member refuses the whole candidate without leaking partial source');
    Check(EncodeNyxProject(LOther.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'late refusal preserves the complete accepted pair');
    LSnapshot := Default(TNyxCallbackBatchSnapshot);
    LRejected := False;
    try
      LSnapshot.ToData;
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'undefined snapshots cannot become queued intent');
    LRejected := False;
    try
      ReadNyxCallbackBatchSnapshot(NyxArray([NyxObject([
        NyxField('op', NyxData('policy')), NyxField('id', NyxData('project-name')),
        NyxField('event', NyxObject([NyxField('trigger', NyxData('key-down'))])),
        NyxField('policy', NyxData(True))])]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'wire scalar types cannot substitute for closed policy choices');
  finally
    LOther.Free;
    LSession.Free;
  end;
end;

begin
  try
    InspectionCases;
    PolicyAddCase;
    Execute;
    WriteLn('PASS ', GChecks, ' callback intent checks');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' callback intent checks';
    document.body.setAttribute('data-nyx-callback-intents', 'passed');
    document.body.setAttribute('data-nyx-callback-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-callback-intents', 'failed');
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
