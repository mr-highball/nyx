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
  nyx.studio.session, nyx.studio.inspector;

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
    Admit;
    Check(LSession.DraftSource = LBefore.Draft, 'Ordinary policy edit retains independent draft/base');
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
  Result := LChecks;
end;

end.
