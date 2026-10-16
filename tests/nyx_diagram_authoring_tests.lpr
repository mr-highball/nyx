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
program nyx_diagram_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen, nyx.source,
  nyx.diagram, nyx.diagram.declarations, nyx.diagram.editing, nyx.diagram.adoption,
  nyx.canvas.geometry, nyx.studio.agents, nyx.studio.projects,
  nyx.studio.session, nyx.studio.sourcejobs, nyx.diagram.editor,
  nyx.test.diagram.document
  {$ifdef PAS2JS}, Web{$else}, nyx.studio.mcp{$endif};

type
  { Each external getter may replace the active project with an identical pair.
    Load authority must rotate even when complete accepted bytes stay equal. }
  TSaveReentry = (srPending, srSnapshot);

  { Scoped foreign request borrows its fixture session only until release.
    It reloads once from the chosen getter and owns only copied snapshot values;
    no editor, worker, persistence path or socket is involved. }
  TReentrantSave = class(TInterfacedObject, INyxDiagramSaveRequest)
  public
    Session: TNyxStudioSession;
    SnapshotValue: TNyxDiagramSaveSnapshot;
    Reentry: TSaveReentry;
    Fired: Boolean;
    procedure Reload;
    function GetSnapshot: TNyxDiagramSaveSnapshot;
    function GetPending: Boolean;
    function Accepted(ADocument: TNyxDocument): Boolean;
    function Refused(const AMessage: TNyxText): Boolean;
  end;

var
  GChecks: Integer;
  GAgent: TNyxAgentSession;

procedure TReentrantSave.Reload;
begin

  if not Fired then
  begin
    Fired := True;
    Session.LoadProject(Session.ProjectSnapshot);
  end;
end;

function TReentrantSave.GetSnapshot: TNyxDiagramSaveSnapshot;
begin

  if Reentry = srSnapshot then
  begin
    Reload;
  end;
  Result := SnapshotValue;
end;

function TReentrantSave.GetPending: Boolean;
begin

  if Reentry = srPending then
  begin
    Reload;
  end;
  Result := True;
end;

function TReentrantSave.Accepted(ADocument: TNyxDocument): Boolean;
begin
  Result := False;
end;

function TReentrantSave.Refused(const AMessage: TNyxText): Boolean;
begin
  Result := False;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Diagram authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure CheckReentrantSave;
var
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LHost: INyxDiagramSaveHost;
  LRequest: TReentrantSave;
  LLease: INyxDiagramSaveRequest;
  LContext: TNyxStudioCommandContext;
  LBefore: TNyxText;
  LReentry: TSaveReentry;
  LRejected: Boolean;
begin
  for LReentry := Low(TSaveReentry) to High(TSaveReentry) do
  begin
    LSession := TNyxStudioSession.Create;
    LCommands := nil;
    try
      LCommands := TNyxSourceCommands.Create(LSession, nil);
      LContext := LSession.CommandContext;
      LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
      LHost := LCommands.DiagramSaveHost(LContext);
      LRequest := TReentrantSave.Create;
      LLease := LRequest;
      LRequest.Session := LSession;
      LRequest.Reentry := LReentry;
      LRequest.SnapshotValue := CaptureNyxDiagramSave(
        NyxDiagramSaveTarget(LSession.Document, NyxDiagramRef('Proposal')),
        NewNyxStarterDiagram, 0);
      LRejected := False;
      try
        LHost.Submit(LLease);
      except
        on E: ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRequest.Fired and not LSession.MatchesCommandContext(LContext),
        'foreign getter really rotates project-load authority despite identical bytes');
      Check(LRejected and not LCommands.Busy and
        (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
        'save refuses the retired load after a foreign getter without queuing or changing its replacement');
    finally
      LLease := nil;
      LHost := nil;
      LCommands.Free;
      LSession.Free;
    end;
  end;
end;

function Pair: TNyxProjectPair;
begin
  Result := GAgent.PreviewPair(GAgent.Revision, 'home');
end;

function Request(const AID: TNyxText; const AOperations: TNyxDataValue;
  ARevision: Integer): TNyxDataValue;
begin
  Result := NyxObject([NyxField('operationId', NyxData(AID)),
    NyxField('expectedRevision', NyxData(ARevision)), NyxField('operations', AOperations)]);
end;

procedure Refuse(const ATool: TNyxText; const AArguments: TNyxDataValue);
var
  LBefore: TNyxText;
  LRevision: Integer;
  LFailed: Boolean;
begin
  LBefore := EncodeNyxProject(Pair);
  LRevision := GAgent.Revision;
  LFailed := False;
  try
    GAgent.Call(ATool, 'Scooty diagram qualification', AArguments);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GAgent.Revision = LRevision) and (EncodeNyxProject(Pair) = LBefore),
    'refused semantic operation retains exact accepted pair/draft/history revision');
end;

function Query(const AScope, AName, AMember: TNyxText;
  AOffset, ALimit, ARevision: Integer): TNyxDataValue;
begin
  Result := GAgent.Call('nyx_outline', 'Scooty diagram qualification', NyxObject([
    NyxField('scope', NyxData(AScope)), NyxField('name', NyxData(AName)),
    NyxField('member', NyxData(AMember)), NyxField('offset', NyxData(AOffset)),
    NyxField('limit', NyxData(ALimit)), NyxField('expectedRevision', NyxData(ARevision))]));
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPlan: INyxDiagram;
  LAfterPlan: INyxDiagram;
  LReference: TNyxDiagramRef;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LPending: TNyxProjectPair;
  LRequest: TNyxDataValue;
  LReply: TNyxDataValue;
  LWindow: TNyxDataValue;
  LOldWindow: TNyxDataValue;
  LChanges: TNyxDiagramChanges;
  LChange: TNyxDiagramChange;
  LSnapshot: TNyxDiagramSaveSnapshot;
  LTarget: TNyxDiagramSaveTarget;
  LIndex: Integer;
  LRevision: Integer;
  LFailed: Boolean;
  {$ifndef PAS2JS}
  LTools: TNyxDataValue;
  {$endif}
begin
  LDocument := DiagramDocumentFixture;
  LRead := nil;
  LWorkspace := nil;
  GAgent := nil;
  try
    LReference := NyxDiagramRef('Publishing workflow');
    LPlan := LDocument.Diagrams.Definition(LReference);
    Check(TNyxDiagramVertex.FromData(LPlan.Vertex(0).ToData).ToData.ToJSON =
      LPlan.ToData.Field('vertices').Item(0).ToJSON, 'standalone vertex boundary matches complete graph bytes');
    Check(TNyxDiagramEdge.FromData(LPlan.Edge(0).ToData).ToData.ToJSON =
      LPlan.ToData.Field('edges').Item(0).ToJSON, 'standalone edge boundary matches complete graph bytes');
    SetLength(LChanges, 7);
    LChanges[0] := NyxDefineDiagram(NyxDiagramRef('New board'), NewNyxDiagram).ExpectAbsent;
    LChanges[1] := NyxRemoveDiagram(LReference).Expect(LPlan);
    LChanges[2] := NyxSetDiagramVertex(LReference, LPlan.Vertex(0));
    LChanges[3] := NyxSetDiagramBounds(LReference, LPlan.Vertex(0).Reference,
      NyxCanvasBounds(-150.5, 77.25, 123.125, 82.5));
    LChanges[4] := NyxRemoveDiagramVertex(LReference, LPlan.Vertex(0).Reference, ndrmIncidentEdges);
    LChanges[5] := NyxSetDiagramEdge(LReference, LPlan.Edge(0));
    LChanges[6] := NyxRemoveDiagramEdge(LReference, LPlan.Edge(0).Reference);
    for LIndex := 0 to High(LChanges) do
    begin
      LChange := TNyxDiagramChange.FromData(LChanges[LIndex].ToData);
      Check(LChange.ToData.ToJSON = LChanges[LIndex].ToData.ToJSON,
        'every typed change retains its exact strict wire family');
    end;
    LTarget := NyxDiagramSaveTarget(LDocument, LReference);
    LAfterPlan := LPlan.WithVertex(LPlan.Vertex(0).Captioned('Captured proposal'));
    LSnapshot := CaptureNyxDiagramSave(LTarget, LAfterPlan, 12);
    Check((LSnapshot.Revision = 12) and not LSnapshot.Matches(LDocument),
      'save capture does not impersonate durable publication');
    LSnapshot.Change.Apply(LDocument);
    Check(LSnapshot.Matches(LDocument), 'applied snapshot matches only exact saved intent');
    Check(AcceptedNyxDiagramSaveTarget(LSnapshot, LDocument).Before.ToJSON =
      LAfterPlan.ToData.ToJSON, 'acknowledgement advances the exact definition baseline');
    LFailed := False;
    try
      LSnapshot.Change.Apply(LDocument);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'retained editor expectation cannot overwrite a changed accepted definition');
    LDocument.Free;
    LDocument := DiagramDocumentFixture;
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    GAgent := TNyxAgentSession.Create(LBefore);
    LRevision := GAgent.Revision;
    LWindow := Query('diagrams', '', '', 0, 1, LRevision);
    Check((LWindow.Field('total').AsInteger = 2) and (LWindow.Field('items').Count = 1) and
      (LWindow.Field('items').Item(0).Count = 3), 'diagram outline returns a bounded count summary, not whole plans');
    LWindow := Query('diagram-vertices', LReference.Name, '', 0, 1, LRevision);
    LOldWindow := LWindow;
    Check((LWindow.Field('total').AsInteger = 3) and (LWindow.Field('items').Count = 1) and
      (LWindow.Field('items').Item(0).Field('caption').AsText =
        TNyxText('Start / 🌙 / 漢字 / O''Brien')), 'bounded vertex inspection retains exact text and fractional data');
    LWindow := Query('diagram-edges', LReference.Name, 'retry', 0, 1, LRevision);
    Check((LWindow.Field('total').AsInteger = 1) and
      (LWindow.Field('items').Item(0).Field('routing').AsInteger = Ord(ndrOrthogonal)),
      'exact edge filter retains self-loop intent without unrelated members');
    LWindow := Query('diagram-vertices', LReference.Name, 'missing', 0, 1, LRevision);
    Check(LWindow.Field('total').AsInteger = 0, 'missing member is an empty exact filter, not a fallback');
    LWindow := Query('diagram-edges', LReference.Name, '', 100000, 1, LRevision);
    Check((LWindow.Field('total').AsInteger = 4) and (LWindow.Field('items').Count = 0),
      'distant bounded page retains total without inventing members');
    Refuse('nyx_outline', NyxObject([NyxField('scope', NyxData('diagram-vertices')),
      NyxField('name', NyxData(LReference.Name)), NyxField('limit', NyxData(33))]));
    Refuse('nyx_outline', NyxObject([NyxField('scope', NyxData('diagram-vertices'))]));
    Refuse('nyx_outline', NyxObject([NyxField('scope', NyxData('diagrams')),
      NyxField('parent', NyxData('home'))]));

    LRequest := Request('arrange-visual-diagram', NyxArray([
      LChanges[0].ToData,
      NyxSetDiagramBounds(LReference, NyxDiagramVertexRef('start'),
        NyxCanvasBounds(-150.5, 77.25, 123.125, 82.5)).ToData,
      NyxSetDiagramVertex(LReference, NyxDiagramVertex(NyxDiagramVertexRef('annotation'),
        ndsRectangle, NyxCanvasBounds(160, 220, 140, 60)).Captioned('A useful note')).ToData,
      NyxSetDiagramEdge(LReference, NyxDiagramEdge(NyxDiagramEdgeRef('annotates'),
        NyxDiagramAttached(NyxDiagramVertexRef('annotation'), ndaRight),
        NyxDiagramAttached(NyxDiagramVertexRef('finish'), ndaBottom)).Routed(ndrOrthogonal)).ToData]), LRevision);
    LReply := GAgent.Call('nyx_transaction', 'Scooty diagram qualification', LRequest);
    LAfter := Pair;
    Check(GAgent.Revision = LRevision + 1, 'related graph changes publish once');
    Check(GAgent.Call('nyx_transaction', 'Scooty diagram qualification', LRequest).ToJSON = LReply.ToJSON,
      'the exact revision-pinned retry returns its original receipt without another publication');
    LRead := TNyxCodec.Decode(LAfter.Design);
    Check((LRead.Diagrams.Count = 3) and
      (LRead.Diagrams.Definition(LReference).VertexCount = 4) and
      (LRead.Diagrams.Definition(LReference).EdgeCount = 5), 'semantic creation/movement/connectivity share complete intent');
    Check(LRead.Diagrams.Definition(NyxDiagramRef('Empty board')).ToData.ToJSON =
      LDocument.Diagrams.Definition(NyxDiagramRef('Empty board')).ToData.ToJSON,
      'another named diagram remains exact');
    Check(LRead.Diagrams.Definition(LReference).Vertex(0).Caption = LPlan.Vertex(0).Caption,
      'bounds-only movement preserves specialized shape caption and style');
    FreeAndNil(LRead);
    LRead := TNyxSourceWorkspace.PrepareDraft(LAfter.Source, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LAfter.Design, 'accepted crafted Pascal reconstructs the complete semantic candidate');
    Check(LOldWindow.Field('items').Item(0).Field('bounds').Item(0).AsNumber = -120.125,
      'retained bounded context remains independent after mutation');
    Refuse('nyx_outline', NyxObject([NyxField('scope', NyxData('diagrams')),
      NyxField('expectedRevision', NyxData(LRevision))]));
    Refuse('nyx_transaction', Request('old', NyxArray([LChanges[0].ToData]), LRevision));
    Refuse('nyx_transaction', Request('collision', NyxArray([
      NyxDefineDiagram(LReference, NewNyxDiagram).ExpectAbsent.ToData]), GAgent.Revision));
    Refuse('nyx_transaction', Request('atomic', NyxArray([
      NyxSetDiagramBounds(LReference, NyxDiagramVertexRef('start'), NyxCanvasBounds(0, 0, 120, 80)).ToData,
      NyxSetDiagramEdge(LReference, NyxDiagramEdge(NyxDiagramEdgeRef('dangling'),
        NyxDiagramAttached(NyxDiagramVertexRef('missing')),
        NyxDiagramAttached(NyxDiagramVertexRef('finish')))).ToData]), GAgent.Revision));
    Refuse('nyx_transaction', Request('connected-removal', NyxArray([
      NyxRemoveDiagramVertex(LReference, NyxDiagramVertexRef('start')).ToData]), GAgent.Revision));
    GAgent.Call('nyx_history', 'Scooty diagram qualification', NyxObject([
      NyxField('expectedRevision', NyxData(GAgent.Revision)), NyxField('operationId', NyxData('undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(LBefore), 'one semantic Undo restores both exact original files');
    GAgent.Call('nyx_history', 'Scooty diagram qualification', NyxObject([
      NyxField('expectedRevision', NyxData(GAgent.Revision)), NyxField('operationId', NyxData('redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(LAfter), 'one semantic Redo restores both exact changed files');
    GAgent.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    Check(Query('diagrams', '', '', 0, 1, GAgent.Revision).Field('total').AsInteger = 3,
      'read-only agent policy retains bounded inspection');
    Refuse('nyx_transaction', Request('read-only', NyxArray([
      NyxRemoveDiagram(NyxDiagramRef('New board')).ToData]), GAgent.Revision));
    GAgent.Free;
    GAgent := nil;
    LPending := LAfter;
    LPending.Pending := True;
    LPending.DraftBase := LAfter.Source;
    LPending.Draft := LAfter.Source + #10 + '// unfinished Pascal';
    GAgent := TNyxAgentSession.Create(LPending);
    Refuse('nyx_transaction', Request('pending-draft', NyxArray([
      NyxRemoveDiagram(NyxDiagramRef('New board')).ToData]), GAgent.Revision));
    {$ifndef PAS2JS}
    LTools := NyxStudioMCPTools;
    Check((LTools.Field('tools').Count = 24) and
      (Pos('diagram-vertex-bounds', LTools.ToJSON) > 0) and
      (Pos('diagram-vertices', LTools.ToJSON) > 0),
      'existing focused tools advertise typed diagram scope/operations without another tool');
    {$endif}
  finally
    GAgent.Free;
    GAgent := nil;
    LWorkspace.Free;
    LRead.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    CheckReentrantSave;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram-authoring', 'passed');
    document.body.setAttribute('data-diagram-authoring-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' semantic diagram authoring checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-authoring', 'failed');
      document.body.setAttribute('data-diagram-authoring-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
