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
  nyx.studio.callbackintent, nyx.studio.callbackedits
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
    LBefore := LOther.ProjectSnapshot;
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
