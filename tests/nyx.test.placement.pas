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


unit nyx.test.placement;
{$mode delphi}{$H+}{$codepage utf8}
interface
uses nyx.studio.projects;
{ Independent English fixture. Returned text owns both portable files. }
function NyxPlacementFixture: TNyxProjectPair;
{ Exercise the actual semantic engine and isolated paired processor, without
  listeners, browser automation or changes to the observing user's project. }
function RunNyxPlacementJourney(out APair: TNyxProjectPair): Integer;
implementation
uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.schema, nyx.studio.edits, nyx.studio.session, nyx.studio.agents;

{ Reconstruct the exact preceding private worker shape. A newer placement
  action cannot be smuggled through a ticket that predates that vocabulary. }
function VersionFive(const AData: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LEdit: TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
begin
  LEdit := AData.Field('edit');
  SetLength(LFields, LEdit.Count - 1);
  LCount := 0;
  for LIndex := 0 to LEdit.Count - 1 do
  begin

    if LEdit.Key(LIndex) <> 'placement' then
    begin
      LFields[LCount] := NyxField(LEdit.Key(LIndex), LEdit.Field(LEdit.Key(LIndex)));
      Inc(LCount);
    end;
  end;
  LEdit := NyxObject(LFields);
  SetLength(LFields, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

    if AData.Key(LIndex) = 'version' then
    begin
      LFields[LIndex] := NyxField('version', NyxData(5));
    end
    else if AData.Key(LIndex) = 'edit' then
    begin
      LFields[LIndex] := NyxField('edit', LEdit);
    end;
  end;
  Result := NyxObject(LFields);
end;

function NyxPlacementFixture: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LPosition: Integer;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Arrange your workspace';
    LDocument.AddPage(NewNyxPage('home')
      .Add(NewNyxColumn('left-layout')
        .Add(NewNyxLabel('first-caption').WithText('First'))
        .Add(NewNyxMemo('notes-editor').Configure.Text('Notes')
          .Value('Keep these notes.').Done)
        .Add(NewNyxLabel('last-caption').WithText('Last')))
      .Add(NewNyxColumn('right-layout')
        .Add(NewNyxButton('send-button').WithText('Send'))));
    LDocument.AddPage(NewNyxPage('archive')
      .Add(NewNyxColumn('archive-layout')));
    LDocument.AddComponent(NewNyxColumn('reusable-layout')
      .Add(NewNyxRow('definition-actions').Configure.PartName(NyxPart('actions')).Done)
      .Add(NewNyxLabel('definition-caption').Configure.PartName(NyxPart('caption'))
        .Text('Reusable caption').Done));
    LDocument.Find('home').Add(NewNyxComponent('reusable-instance')
      .Configure.Component(NyxComponent('reusable-layout')).Done);
    LSource := TNyxCodegen.Generate(LDocument);
    LPosition := Pos('implementation', LSource);
    LSource := Copy(LSource, 1, LPosition - 1) +
      '{ Keep this handwritten English source comment. }' + #10 +
      Copy(LSource, LPosition, MaxInt);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
end;

function RunNyxPlacementJourney(out APair: TNyxProjectPair): Integer;
var
  LAgent: TNyxAgentSession;
  LSession: TNyxStudioSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LDocument: TNyxDocument;
  LRevision: Integer;
  LChecks: Integer;
  LArguments: TNyxDataValue;
  LQuery: TNyxDataValue;
  LRequest: TNyxStudioDesignRequest;
  LWire: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LEdit: TNyxStudioDesignEdit;
  LRefused: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Placement: ' + AReason);
    end;
    Inc(LChecks);
  end;

  function Arguments(const AID: TNyxText;
    const AOperations: array of TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)),
      NyxField('operations', NyxArray(AOperations))]);
  end;

  procedure Apply(const AID: TNyxText; const AOperations: array of TNyxDataValue);
  begin
    LAgent.Call('nyx_transaction', 'Scooty', Arguments(AID, AOperations));
    LRevision := LAgent.Revision;
  end;

  procedure Refuses(const AID: TNyxText; const AOperations: array of TNyxDataValue);
  var
    LPair: TNyxProjectPair;
    LRejected: Boolean;
  begin
    LPair := LAgent.PreviewPair(LRevision, 'home');
    LRejected := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty', Arguments(AID, AOperations));
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LAgent.Revision = LRevision) and
      (EncodeNyxProject(LPair) =
       EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'))),
      'refusal retains exact pair/revision: ' + AID);
  end;

  procedure History(const ADirection: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('history-' + IntToStr(LRevision))),
      NyxField('direction', NyxData(ADirection))]));
    LRevision := LAgent.Revision;
  end;

begin
  LChecks := 0;
  LAgent := TNyxAgentSession.Create(NyxPlacementFixture);
  try
    LRevision := LAgent.Revision;
    LBefore := LAgent.PreviewPair(LRevision, 'home');
    LQuery := LAgent.Call('nyx_node', 'Scooty', NyxObject([
      NyxField('id', NyxData('notes-editor')), NyxField('limit', NyxData(1))]));
    Check(LQuery.Field('node').Field('id').AsText = 'notes-editor', 'bounded exact source inspection');

    Apply('before-first', [NyxPlaceControl(NyxControl('notes-editor'),
      NyxControl('first-caption'), nplBefore).ToData]);
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    try
      Check(LDocument.Find('left-layout').Children[0].ID = 'notes-editor',
        'before resolves the current target index');
    finally
      LDocument.Free;
    end;
    Apply('after-last', [NyxPlaceControl(NyxControl('notes-editor'),
      NyxControl('last-caption'), nplAfter).ToData]);
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    try
      Check(LDocument.Find('left-layout').Children[2].ID = 'notes-editor',
        'same-parent after resolves after detaching the source');
    finally
      LDocument.Free;
    end;
    History('undo');
    History('undo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LBefore),
      'relative moves retain exact paired history');

    LArguments := Arguments('grouped-placement', [
      NyxPlaceControl(NyxControl('notes-editor'), NyxControl('right-layout'), nplInside).ToData,
      NyxPlaceNewControl(nkLabeledButton, NyxControl('reply-action'),
        NyxControl('send-button'), nplBefore).ToData,
      TNyxDataValue.ParseJSON('{"op":"update","id":"send-button","properties":{"text":"Post reply"}}')]);
    LAgent.Call('nyx_transaction', 'Scooty', LArguments);
    LRevision := LAgent.Revision;
    LAfter := LAgent.PreviewPair(LRevision, 'home');
    LDocument := TNyxCodec.Decode(LAfter.Design);
    try
      Check(LDocument.Find('notes-editor').Parent.ID = 'right-layout', 'cross-container ownership');
      Check(LDocument.Find('right-layout').Children[0].ID = 'reply-action', 'new compound before target');
      Check(LDocument.Find('reply-action').Count > 0, 'catalog recipe retains independently owned parts');
      Check(LDocument.Find('notes-editor').Prop('value') = 'Keep these notes.', 'moved content remains exact');
    finally
      LDocument.Free;
    end;
    Check(Pos('Keep this handwritten English source comment.', LAfter.Source) > 0,
      'grouped placement preserves handwritten source');
    LAgent.Call('nyx_transaction', 'Scooty', LArguments);
    Check(LAgent.Revision = LRevision, 'operation receipt deduplicates grouped placement');
    History('undo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LBefore),
      'one Undo restores all related operations and their Pascal');
    History('redo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LAfter),
      'one Redo restores the exact pair');

    Refuses('root', [NyxPlaceControl(NyxControl('home'), NyxControl('archive-layout'), nplInside).ToData]);
    Refuses('cycle', [NyxPlaceControl(NyxControl('right-layout'), NyxControl('notes-editor'), nplAfter).ToData]);
    Refuses('self', [NyxPlaceControl(NyxControl('notes-editor'), NyxControl('notes-editor'), nplBefore).ToData]);
    Refuses('leaf', [NyxPlaceControl(NyxControl('notes-editor'), NyxControl('first-caption'), nplInside).ToData]);
    Refuses('foreign', [NyxPlaceControl(NyxControl('notes-editor'), NyxControl('missing'), nplInside).ToData]);
    Refuses('occupied', [NyxPlaceNewControl(nkButton, NyxControl('notes-editor'),
      NyxControl('left-layout'), nplInside).ToData]);
    Refuses('new-root', [NyxPlaceNewControl(nkPage, NyxControl('new-root'),
      NyxControl('left-layout'), nplInside).ToData]);
    Refuses('instance-child', [NyxPlaceControl(NyxControl('notes-editor'),
      NyxControl('reusable-instance'), nplInside).ToData]);
    Refuses('atomic-group', [
      NyxPlaceControl(NyxControl('notes-editor'), NyxControl('left-layout'), nplInside).ToData,
      NyxPlaceControl(NyxControl('send-button'), NyxControl('first-caption'), nplInside).ToData]);
    Refuses('closed-placement', [TNyxDataValue.ParseJSON(
      '{"op":"place","id":"notes-editor","target":"left-layout","placement":"around"}')]);
    Refuses('unknown-field', [TNyxDataValue.ParseJSON(
      '{"op":"place","id":"notes-editor","target":"left-layout","placement":"inside","index":0}')]);
    Refuses('root-order', [NyxPlaceControl(NyxControl('notes-editor'), NyxControl('archive'), nplBefore).ToData]);

    Apply('customized-layout', [TNyxDataValue.ParseJSON(
      '{"op":"override","instance":"reusable-instance","id":"local-actions","path":"actions","mode":"properties"}'),
      NyxPlaceControl(NyxControl('notes-editor'), NyxControl('local-actions'), nplInside).ToData]);
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    try
      Check(LDocument.Find('local-actions').Prop('mode') = 'append',
        'customized layout promotes properties to owned append content');
      Check(LDocument.Find('notes-editor').Parent.ID = 'local-actions',
        'instance payload remains independently owned');
    finally
      LDocument.Free;
    end;
    Refuses('part-descriptor', [NyxPlaceControl(NyxControl('local-actions'),
      NyxControl('left-layout'), nplInside).ToData]);
    Refuses('empty-append', [NyxPlaceControl(NyxControl('notes-editor'),
      NyxControl('left-layout'), nplInside).ToData]);
    Apply('cross-page', [NyxPlaceControl(NyxControl('send-button'),
      NyxControl('archive-layout'), nplInside).ToData]);
    APair := LAgent.PreviewPair(LRevision, 'home');
  finally
    LAgent.Free;
  end;

  LSession := TNyxStudioSession.Create(APair);
  LSchemas := CaptureNyxSchemas;
  try
    LSession.Select('first-caption');
    LSession.BeginPlacement;
    LSession.Select('right-layout');
    Check(LSession.PlacementSource.ID = 'first-caption', 'destination selection retains the source');
    LEdit := LSession.CapturePlacement(NyxPlaceControl(LSession.PlacementSource,
      NyxControl(LSession.SelectedID), nplInside), LSession.CommandContext);
    LBefore := LSession.ProjectSnapshot;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LWire := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON));
    Check(LRequest.SameRequest(LWire), 'worker wire retains typed exact placement');
    LRefused := False;
    try
      ReadNyxStudioDesignRequest(VersionFive(LRequest.ToData));
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'previous worker version refuses newer placement vocabulary');
    LPrepared := PrepareNyxStudioDesign(LWire, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'isolated processor admits placement');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'preparation does not mutate accepted owners');
    LPrepared := ReceiveNyxPreparedDesign(TNyxDataValue.ParseJSON(LPrepared.ToData.ToJSON),
      LRequest, LSchemas);
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'paired admission publishes one placement');
    Check((LSession.SelectedID = 'first-caption') and
      (LSession.Document.Find('first-caption').Parent.ID = 'right-layout'),
      'ordinary session selects the moved control');
    Check(LSession.PlacementSource.ID = '', 'changed pair invalidates the armed move');
    LAfter := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(LSession.PlacementSource.ID = '', 'Undo cannot resurrect the retired move source');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'isolated placement has one paired Undo');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'isolated placement has exact Redo');

    LEdit := LSession.CapturePlacement(NyxPlaceControl(NyxControl('send-button'),
      NyxControl('left-layout'), nplInside), LSession.CommandContext);
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LSession.SetTitle('Changed while preparing');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale,
      'changed accepted pair refuses publication');
    LSession.LoadProject(LAfter);
    LRefused := False;
    try
      LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'old placement cannot follow matching IDs through project reload');
    LSession.Select('first-caption');
    LSession.BeginPlacement;
    LSession.SetSourceDraft(LSession.Source + #10 + '{ A pending draft. }');
    Check(LSession.PlacementSource.ID = '', 'pending Pascal draft invalidates the armed source');
    LSession.DiscardSourceDraft;
    LSession.BeginPlacement;
    LSession.CancelPlacement;
    Check(LSession.PlacementSource.ID = '', 'cancel retains the pair and clears only presentation');
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaProperty;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Name := NyxAttributeName(atText);
    LEdit.Value := 'An older property intent';
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LWire := ReadNyxStudioDesignRequest(VersionFive(LRequest.ToData));
    Check(LRequest.SameRequest(LWire), 'exact preceding worker property shape remains compatible');
    APair := LSession.ProjectSnapshot;
  finally
    LPrepared := nil;
    LSchemas := nil;
    LSession.Free;
  end;
  Result := LChecks;
end;
end.
