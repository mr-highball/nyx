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
program nyx_canvas_adoption_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.controls, nyx.events, nyx.view.behavior, nyx.canvas.scene, nyx.canvas.geometry,
  nyx.canvas.types, nyx.canvas.workspace, nyx.canvas.routes, nyx.canvas.route.types,
  nyx.canvas.adoption, nyx.flow.routes, nyx.tool.canvas, nyx.test.programs.logic,
  nyx.flow.types, nyx.flow.declarations, nyx.flow.editing, nyx.logic.programs,
  nyx.state, nyx.flow.route.model,
  nyx.canvas.editing, nyx.studio.edits, nyx.studio.transactions, nyx.studio.session,
  nyx.studio.projects, nyx.schema
  {$ifdef PAS2JS}, Web{$endif};

type
  { Deliberate capture interruption, not a physical adapter. Each alternative
    changes a real revision or revokes the borrowed view during the callout. }
  TCaptureInterruption = (ciNone, ciRevision, ciDisconnect);

  { Portable qualification view owns an independently materialized saved scene
    and inert route values. It substitutes rendering/events only: no widget,
    application execution, source publication or hardware claim follows. }
  TCaptureView = class(TInterfacedObject, INyxCanvasWorkspaceView,
    INyxCanvasRuntimeView, INyxCanvasRouteEditing)
  private
    FWorkspace: INyxCanvasWorkspace;
  public
    Active: Boolean;
    Revision: Integer;
    Captured: TNyxCanvasRouteChanges;
    Interrupt: TCaptureInterruption;
    constructor Create(ADocument: TNyxDocument);
    function GetConnected: Boolean;
    function GetCamera: TNyxCanvasCamera;
    function GetMountedCount: Integer;
    procedure SetCamera(const ACamera: TNyxCanvasCamera);
    procedure PanBy(AX, AY: Double);
    procedure ZoomAt(AZoom: Double; const AAnchor: TNyxCanvasPoint);
    procedure Execute(ACommand: TNyxCanvasCommand);
    procedure Refresh;
    procedure Constrain(AEnabled, AReadOnly: Boolean);
    function EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
    procedure Disconnect;
    function BehaviorFor(const AItem: TNyxCanvasItemRef): INyxViewBehavior;
    function GetScene: INyxCanvasScene;
    function RouteContext(const AItem: TNyxCanvasItemRef; out AOwnedRoot: TNyxNode): TNyxDocument;
    function CaptureRoutes: TNyxCanvasRouteChanges;
    function GetRouteRevision: Integer;
    procedure ApplyRouteChanges(const AChanges: array of TNyxCanvasRouteChange;
      AExpectedScene, AExpectedRoutes: Integer);
  end;

  { Portable optional graph provider. It owns an independent inert registry;
    direct fixture edits are not runtime publication or application execution.
    One deliberately separate revision can advance during a read. }
  TGraphCaptureView = class(TCaptureView, INyxCanvasFlowEditing)
  public
    Graphs: INyxFlowDeclarations;
    GraphRevision: Integer;
    AdvanceGraphRead: Boolean;
    function RuntimeFlows: INyxFlowDeclarations;
    function GetFlowRevision: Integer;
    procedure ApplyFlowChanges(const AChanges: array of TNyxFlowChange;
      AExpectedScene, AExpectedFlows: Integer);
  end;

  { Explicit substituted persistence host retains its request for deliberate
    acknowledgement. It never writes a file or represents real publication. }
  TCaptureHost = class(TInterfacedObject, INyxCanvasRouteAdoptionHost)
  public
    Request: INyxCanvasRouteAdoptionRequest;
    Count: Integer;
    procedure Submit(const ARequest: INyxCanvasRouteAdoptionRequest);
  end;

  { Borrowed presentation callback outlives its inspector. It deliberately fails
    only after ticket completion, checking the admission/notification boundary. }
  TFeedbackFailure = class
  public
    Persistence: INyxToolCanvasRoutePersistence;
    procedure Changed;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas adoption: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TCaptureView.Create(ADocument: TNyxDocument);
begin
  inherited Create;
  FWorkspace := NewNyxCanvasWorkspace('capture-workspace', ADocument, NyxCanvasRef('Greeting canvas'));
  Active := True;
end;

function TCaptureView.GetConnected: Boolean;
begin
  Result := Active;
end;

function TCaptureView.GetCamera: TNyxCanvasCamera;
begin
  Result := FWorkspace.Camera;
end;

function TCaptureView.GetMountedCount: Integer;
begin
  Result := 0;
end;

procedure TCaptureView.SetCamera(const ACamera: TNyxCanvasCamera);
begin
  raise Exception.Create('Capture fixture does not implement camera interaction');
end;

procedure TCaptureView.PanBy(AX, AY: Double);
begin
  raise Exception.Create('Capture fixture does not implement panning');
end;

procedure TCaptureView.ZoomAt(AZoom: Double; const AAnchor: TNyxCanvasPoint);
begin
  raise Exception.Create('Capture fixture does not implement zooming');
end;

procedure TCaptureView.Execute(ACommand: TNyxCanvasCommand);
begin
  raise Exception.Create('Capture fixture does not implement physical commands');
end;

procedure TCaptureView.Refresh;
begin
end;

procedure TCaptureView.Constrain(AEnabled, AReadOnly: Boolean);
begin
end;

function TCaptureView.EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
begin
  Result := nil;
end;

procedure TCaptureView.Disconnect;
begin
  Active := False;
end;

function TCaptureView.BehaviorFor(const AItem: TNyxCanvasItemRef): INyxViewBehavior;
begin
  Result := nil;
end;

function TCaptureView.GetScene: INyxCanvasScene;
begin
  Result := FWorkspace.Scene;
end;

function TCaptureView.RouteContext(const AItem: TNyxCanvasItemRef;
  out AOwnedRoot: TNyxNode): TNyxDocument;
begin
  Result := nil;
  AOwnedRoot := nil;
  raise Exception.Create('Capture fixture does not implement route authoring');
end;

function TCaptureView.CaptureRoutes: TNyxCanvasRouteChanges;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Captured));
  for LIndex := 0 to High(Captured) do
  begin
    Result[LIndex] := TNyxCanvasRouteChange.FromData(Captured[LIndex].ToData);
  end;
  case Interrupt of
    ciNone:
      begin
        { Deliberately uninterrupted creator capture. }
      end;
    ciRevision:
      begin
        Inc(Revision);
      end;
    ciDisconnect:
      begin
        Active := False;
      end;
  end;
end;

function TCaptureView.GetRouteRevision: Integer;
begin
  Result := Revision;
end;

procedure TCaptureView.ApplyRouteChanges(const AChanges: array of TNyxCanvasRouteChange;
  AExpectedScene, AExpectedRoutes: Integer);
begin
  raise Exception.Create('Capture fixture does not implement runtime publication');
end;

function TGraphCaptureView.RuntimeFlows: INyxFlowDeclarations;
begin
  Result := Graphs.Clone;

  if AdvanceGraphRead then
  begin
    Inc(GraphRevision);
  end;
end;

function TGraphCaptureView.GetFlowRevision: Integer;
begin
  Result := GraphRevision;
end;

procedure TGraphCaptureView.ApplyFlowChanges(const AChanges: array of TNyxFlowChange;
  AExpectedScene, AExpectedFlows: Integer);
begin
  raise Exception.Create('Graph capture fixture does not implement runtime publication');
end;

procedure TCaptureHost.Submit(const ARequest: INyxCanvasRouteAdoptionRequest);
begin
  Request := ARequest;
  Inc(Count);
end;

procedure TFeedbackFailure.Changed;
begin

  if not Persistence.AdoptionPending then
  begin
    raise Exception.Create('Deliberate completed-feedback failure');
  end;
end;

procedure CaptureContracts;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LView: TCaptureView;
  LViewLease: INyxCanvasWorkspaceView;
  LHost: TCaptureHost;
  LHostLease: INyxCanvasRouteAdoptionHost;
  LInspector: INyxToolCanvasInspector;
  LPersistence: INyxToolCanvasRoutePersistence;
  LLocalPersistence: INyxToolCanvasRoutePersistence;
  LFeedback: TFeedbackFailure;
  LSnapshot: TNyxCanvasRouteAdoptionSnapshot;
  LChanges: TNyxCanvasRouteChanges;
  LBefore: TNyxText;
  LOwner: TNyxControlRef;
  LFailed: Boolean;
  LMode: TCaptureInterruption;
begin
  LDocument := ProgramToolCanvasDocument;
  LCandidate := nil;
  LFeedback := TFeedbackFailure.Create;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LOwner := NyxControl(LDocument.Find('program-tools').Part('workspace').ID);
    LView := TCaptureView.Create(LDocument);
    LViewLease := LView;
    SetLength(LView.Captured, 2);
    LView.Captured[0] := NyxCanvasRoutes(NyxCanvasItem('Greeting'),
      NyxControl('name-input'), NewNyxFlowRoutes);
    LView.Captured[1] := NyxCanvasRoutes(NyxCanvasItem('Greeting'),
      NyxControl('greet-button'), NewNyxFlowRoutes);
    LSnapshot := Default(TNyxCanvasRouteAdoptionSnapshot);
    LFailed := False;
    try
      LChanges := LSnapshot.Changes;
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'undefined snapshots expose no adoptable changes');
    LSnapshot := CaptureNyxCanvasRouteAdoption(LDocument, LOwner, LViewLease);
    Check(LSnapshot.Defined and (LSnapshot.Count = 2) and
      (LSnapshot.BaseDesign = LBefore), 'capture retains exact source and explicit empty masks');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'capture never changes authored roots or source defaults');
    LChanges := LSnapshot.Changes;
    LChanges[0] := NyxCanvasRoutes(NyxCanvasItem('Other'), NyxControl('other'), NewNyxFlowRoutes);
    Check(LSnapshot.Change(0).Item.Name = 'Greeting', 'returned vectors cannot rewrite the retained snapshot');
    LCandidate := TNyxCodec.Decode(LSnapshot.CandidateDesign);
    Check(TNyxCodec.NodeSnapshot(LCandidate.Find('name-input')).ToJSON =
      TNyxCodec.NodeSnapshot(LDocument.Find('name-input')).ToJSON,
      'candidate masks do not rewrite shared reusable component controls');
    LSnapshot := CaptureNyxCanvasRouteAdoption(LCandidate, LOwner, LViewLease);
    Check((LSnapshot.Count = 0) and (LSnapshot.CandidateDesign = LSnapshot.BaseDesign),
      'saved overlays compare correctly against retained raw item content');

    LView.GetScene.Control(NyxCanvasItem('Greeting')).Node.Find('caption-label').SetProp('text', 'Different content');
    LFailed := False;
    try
      LSnapshot := CaptureNyxCanvasRouteAdoption(LDocument, LOwner, LViewLease);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'unrelated live content differences refuse instead of silently saving routes over a different tree');
    { Recreate the independently owned fixture content. Writing an empty text
      property cannot restore an originally absent property byte-for-byte. }
    LChanges := LView.CaptureRoutes;
    LView := TCaptureView.Create(LDocument);
    LViewLease := LView;
    LView.Captured := LChanges;
    for LMode := ciRevision to ciDisconnect do
    begin
      LView.Interrupt := LMode;
      LFailed := False;
      try
        LSnapshot := CaptureNyxCanvasRouteAdoption(LDocument, LOwner, LViewLease);
      except
        on Exception do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed, 'revision change or retirement during creator capture refuses atomically');
      LView.Active := True;
    end;
    LView.Interrupt := ciNone;

    LHost := TCaptureHost.Create;
    LHostLease := LHost;
    LInspector := NewNyxToolCanvasInspector('adoption-contract', LDocument, LViewLease);
    Check(Supports(LInspector, INyxToolCanvasRoutePersistence, LLocalPersistence),
      'ordinary public inspector exposes optional explicit persistence');
    LPersistence := LLocalPersistence;
    LFeedback.Persistence := LPersistence;
    LPersistence.ConfigureAdoption(LOwner, LHostLease, {$ifdef PAS2JS}@{$endif}LFeedback.Changed);
    LPersistence.SaveConnections;
    Check(LPersistence.AdoptionPending and (LHost.Count = 1),
      'returning from a substituted host does not imply source publication');
    LFailed := False;
    try
      LHost.Request.Accepted(LDocument);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LHost.Request.Pending, 'wrong accepted document refuses before retiring the pending ticket');
    Check(LHost.Request.Accepted(LCandidate) and not LPersistence.AdoptionPending and
      (Pos('Deliberate completed-feedback failure', LPersistence.AdoptionFeedback) > 0),
      'post-admission feedback failure retains successful acknowledgement and its diagnostic');
    LPersistence.ConfigureAdoption(LOwner, LHostLease, nil);
    LPersistence.SaveConnections;
    Check((LHost.Count = 1) and not LPersistence.AdoptionPending,
      'completed baseline prevents unchanged snapshots from creating another command');
    SetLength(LView.Captured, 3);
    LView.Captured[2] := NyxCanvasRoutes(NyxCanvasItem('Greeting'),
      NyxControl('caption-label'), NewNyxFlowRoutes);
    LPersistence.SaveConnections;
    Check(LHost.Request.Pending and (LHost.Request.Snapshot.Count = 1),
      'a later save captures only the next unsaved exact control mask');
    LInspector.Disconnect;
    Check(not LHost.Request.Pending and not LHost.Request.Accepted(nil) and
      not LHost.Request.Refused('Late'), 'disconnect retires request authority before late host results');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'substituted host acknowledgement alone performs no project mutation');
  finally

    if LInspector <> nil then
    begin
      LInspector.Disconnect;
    end;
    LFeedback.Persistence := nil;
    LPersistence := nil;
    LLocalPersistence := nil;
    LInspector := nil;
    LHostLease := nil;
    LViewLease := nil;
    LFeedback.Free;
    LCandidate.Free;
    LDocument.Free;
  end;
end;

{ Rebuild exact immutable objects to corrupt one protocol field deliberately.
  This is test input construction, not a second product decoder or mutator. }
function Replaced(const AObject: TNyxDataValue; const AKey: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AObject.Count);
  for LIndex := 0 to High(LFields) do
  begin

    if AObject.Key(LIndex) = AKey then
    begin
      LFields[LIndex] := NyxField(AKey, AValue);
    end
    else
    begin
      LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));
    end;
  end;
  Result := NyxObject(LFields);
end;

procedure ProtocolContracts;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LSchemas: INyxSchemaSnapshot;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LRead: TNyxStudioDesignRequest;
  LOther: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LWire: TNyxDataValue;
  LBad: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LFailed: Boolean;
  LIndex: Integer;
begin
  LDocument := ProgramToolCanvasDocument;
  LSession := nil;
  try
    LSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument)));
    LSchemas := CaptureNyxSchemas;
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTransaction;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Transaction := CaptureNyxProjectTransaction(NyxProjectTransaction([
      NyxDesignStep(NyxCanvasPatch([NyxSetCanvasConnections(NyxCanvasRef('Greeting canvas'),
        [NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('greet-button'), NewNyxFlowRoutes)])]))]));
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LWire := LRequest.ToData;
    Check(LWire.Field('version').AsInteger = 16, 'ordinary unguarded transaction retains its version-16 wire');
    LRead := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LWire.ToJSON));
    Check(LRequest.SameRequest(LRead), 'legacy transaction round trip remains exact');
    LEdit.CanvasSceneBaseline := 'Explicit form baseline';
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LWire := LRequest.ToData;
    LRead := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LWire.ToJSON));
    Check((LWire.Field('version').AsInteger = 19) and LRequest.SameRequest(LRead),
      'legacy canvas-form guard retains its distinct version-19 wire and equality');
    LEdit.TransactionDesignBaseline := TNyxCodec.Encode(LSession.Document);
    LFailed := False;
    try
      LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
      LWire := LRequest.ToData;
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'two different transaction baseline contracts cannot share one ticket');
    LEdit.CanvasSceneBaseline := '';
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LWire := LRequest.ToData;
    Check((LWire.Field('version').AsInteger = 20) and
      (LWire.Field('edit').Field('transactionDesignBaseline').AsText = LSession.Save),
      'guarded transaction carries exact design in its closed private worker ticket');
    LRead := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LWire.ToJSON));
    Check(LRequest.SameRequest(LRead), 'guarded worker round trip retains full meaning and session identity');
    LOther := ReadNyxStudioDesignRequest(Replaced(LWire, 'edit',
      Replaced(LWire.Field('edit'), 'transactionDesignBaseline', NyxData('Different captured source'))));
    Check(not LRequest.SameRequest(LOther), 'request equality includes captured full design');
    for LIndex := 0 to 2 do
    begin
      case LIndex of
        0:
          begin
            LBad := Replaced(LWire, 'edit', Replaced(LWire.Field('edit'),
              'transactionDesignBaseline', NyxData('')));
          end;
        1:
          begin
            LBad := Replaced(LWire, 'version', NyxData(19));
          end;
        2:
          begin
            LBad := Replaced(LWire, 'edit', Replaced(LWire.Field('edit'),
              'transactionDesignBaseline', NyxData(False)));
          end;
      end;
      LFailed := False;
      try
        LRead := ReadNyxStudioDesignRequest(LBad);
      except
        on Exception do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed, 'empty, wrong-version and wrong-kind guarded baselines refuse strict worker admission');
    end;
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'independent preparation leaves accepted pair and history untouched');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'exact guarded candidate publishes through the existing paired owner');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Undo;
    Check((EncodeNyxProject(LSession.ProjectSnapshot) = LBefore) and not LSession.CanUndo,
      'one Undo restores the exact pair with no intermediate save command');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'one Redo restores exact saved source and design');
    LSession.Undo;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'current full baseline prepares before an intervening editor change');
    LSession.SetTitle('Intervening title');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check((LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LAfter),
      'post-preparation authoring refuses stale acknowledgement without changing the new pair');
    LOther := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LOther, LSchemas);
    Check(LPrepared.Diagnostic.Defined and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LAfter),
      'dispatch after an unrelated authored change refuses the stale live capture before publication');
  finally
    LSession.Free;
    LDocument.Free;
  end;
end;

{ The real capture/model gate must admit a new graph and its referencing mask
  together. The substituted view cannot establish widget or compiler execution. }
procedure GraphContracts;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LView: TGraphCaptureView;
  LLease: INyxCanvasWorkspaceView;
  LSnapshot: TNyxCanvasRouteAdoptionSnapshot;
  LFlows: TNyxFlowChanges;
  LPair: TNyxProjectPair;
  LOwner: TNyxControlRef;
  LReference: TNyxFlowRef;
  LBefore: TNyxText;
  LFailed: Boolean;
begin
  LDocument := ProgramToolCanvasDocument;
  LCandidate := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LOwner := NyxControl(LDocument.Find('program-tools').Part('workspace').ID);
    LReference := NyxFlowRef('Live greeting');
    LView := TGraphCaptureView.Create(LDocument);
    LLease := LView;
    LView.Graphs := LDocument.Flows.Clone;
    LView.GraphRevision := 7;
    LView.Graphs.Define(LReference, LDocument.Flows.Definition(NyxFlowRef('Greeting flow')));
    LSnapshot := CaptureNyxCanvasRouteAdoption(LDocument, LOwner, LLease);
    Check(LSnapshot.HasChanges and (LSnapshot.Count = 0) and (LSnapshot.FlowCount = 1) and
      (LSnapshot.FlowRevision = 7), 'graph-only changes are not mistaken for a no-op route snapshot');
    LCandidate := TNyxCodec.Decode(LSnapshot.CandidateDesign);
    Check(LCandidate.Flows.ToData.ToJSON = LView.Graphs.ToData.ToJSON,
      'graph-only candidate contains exact applied registry membership and meaning');
    LFlows := LSnapshot.FlowChanges;
    LFlows[0] := NyxRemoveFlow(NyxFlowRef('Greeting flow'));
    Check((LSnapshot.FlowChange(0).Kind = nfcDefine) and
      (LSnapshot.FlowChange(0).Reference.Name = LReference.Name),
      'returned typed graph changes cannot rewrite the captured snapshot');
    LSnapshot := CaptureNyxCanvasRouteAdoption(LCandidate, LOwner, LLease);
    Check(not LSnapshot.HasChanges and (LSnapshot.CandidateDesign = LSnapshot.BaseDesign),
      'recaptured exact applied registry creates no additional save');
    FreeAndNil(LCandidate);
    SetLength(LView.Captured, 1);
    LView.Captured[0] := NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('name-input'),
      NewNyxFlowRoutes.WithRoute(NyxFlowRoute(NyxFlowRouteID('New graph'), LReference,
        ntChange, nfcmAfterDispatch)
        .InputEventValue(ProgramLogicEndpoint('Name'), nftText)
        .OutputState(ProgramLogicEndpoint('Caption'), NyxTextState('caption'))));
    LSnapshot := CaptureNyxCanvasRouteAdoption(LDocument, LOwner, LLease);
    LCandidate := TNyxCodec.Decode(LSnapshot.CandidateDesign);
    Check((LSnapshot.Count = 1) and (LSnapshot.FlowCount = 1) and
      LCandidate.Flows.Contains(LReference),
      'new graph and its referring route mask admit as one complete source candidate');
    LPair := NyxProjectTransaction([
      NyxDesignStep(NyxFlowPatch(LSnapshot.FlowChanges)),
      NyxDesignStep(NyxCanvasPatch([NyxSetCanvasConnections(LSnapshot.Scene, LSnapshot.Changes)]))])
      .Candidate(NyxProjectPair(LBefore, TNyxCodegen.Generate(LDocument)));
    Check(LPair.Design = LSnapshot.CandidateDesign,
      'ordinary paired transaction coalesces the forward graph reference before source/model admission');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'graph/mask capture never changes source defaults or roots');
    LView.AdvanceGraphRead := True;
    LFailed := False;
    try
      LSnapshot := CaptureNyxCanvasRouteAdoption(LDocument, LOwner, LLease);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'independent graph revision movement during capture refuses the whole snapshot');
    LView.AdvanceGraphRead := False;
    LView.Captured := nil;
    LView.Graphs.Remove(NyxFlowRef('Greeting flow'));
    LFailed := False;
    try
      LSnapshot := CaptureNyxCanvasRouteAdoption(LDocument, LOwner, LLease);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'graph removal with an authored application reference refuses before submission');
  finally
    LLease := nil;
    LCandidate.Free;
    LDocument.Free;
  end;
end;

begin
  try
    CaptureContracts;
    GraphContracts;
    ProtocolContracts;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-adoption-contract', 'passed');
    document.body.setAttribute('data-canvas-adoption-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable canvas adoption/worker checks; substituted view and host');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-adoption-contract', 'failed');
      document.body.setAttribute('data-canvas-adoption-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
