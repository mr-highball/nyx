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
program nyx_source_observer_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model,
  nyx.source, nyx.types, nyx.studio.projects, nyx.studio.session,
  nyx.studio.mcp, nyx.studio.directories, nyx.studio.outputs,
  nyx.studio.buildexecutor, nyx.studio.builds, nyx.studio.workspaces,
  nyx.studio.sourceprojection, nyx.studio.sourceobservations,
  nyx.studio.exchange, nyx.studio.agentbridge, nyx.studio.agents, nyx.test.projection;

type
  { Faults alter copied replies after the actual private backend admission.
    Deterministic delivery qualifies the real portable controller, not sockets,
    elapsed timers, physical widgets or browser execution. No listener starts. }
  TReplyFault = (rfNone, rfIssuer, rfWorkspace, rfRevision, rfMissing,
    rfStale, rfBoundary, rfLegacy);
  TEngineExchange = class(TNyxStudioEditorExchange)
  private
    FEngine: TNyxStudioMCP;
    FReply: TNyxEditorReply;
    FTick: TNyxEditorTick;
    FBody: TNyxDataValue;
    FConnect: Boolean;
    FToken: TNyxText;
    FStatus: Integer;
    FResponse: TNyxText;
  public
    Fault: TReplyFault;
    Receipt: TNyxDataValue;
    constructor Create(AEngine: TNyxStudioMCP);
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
    procedure Prepare(AFull: Boolean = False);
    procedure Deliver;
    procedure FireTick;
    function Pending: Boolean;
  end;

const
  CUnit: TNyxText = 'nyx.projection.fixture';
  CPrefix: TNyxText = '{ observer prefix 🚀 }' + #10;
  CDraft: TNyxText = #10 + '{ unfinished observer notes 🌙 }';

var
  GChecks: Integer;
  GEngine: TNyxStudioMCP;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Copy every field before changing one explicit protocol member; never mutate
  the backend's reply or accidentally replace a context through an alias. }
function FieldCopy(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue; ARemove: Boolean = False): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
begin
  SetLength(LFields, AData.Count);
  LCount := 0;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin

      if ARemove then
      begin
        Continue;
      end;
      LFields[LCount] := NyxField(AName, AValue.Copy);
    end
    else
    begin
      LFields[LCount] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)).Copy);
    end;
    Inc(LCount);
  end;
  SetLength(LFields, LCount);
  Result := NyxObject(LFields);
end;

constructor TEngineExchange.Create(AEngine: TNyxStudioMCP);
begin
  inherited Create;
  FEngine := AEngine;
end;

destructor TEngineExchange.Destroy;
begin
  CancelRequest;
  CancelTick;
  FEngine := nil;
  inherited Destroy;
end;

procedure TEngineExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin

  if Assigned(FReply) then
  begin
    raise Exception.Create('One deterministic editor request is already owned');
  end;
  FConnect := AConnect;
  FToken := AToken;
  FBody := TNyxDataValue.ParseJSON(ABody);
  FReply := AReply;
end;

procedure TEngineExchange.CancelRequest;
begin
  FReply := nil;
  FResponse := '';
end;

procedure TEngineExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  FTick := ATick;
end;

procedure TEngineExchange.CancelTick;
begin
  FTick := nil;
end;

procedure TEngineExchange.Prepare(AFull: Boolean);
var
  LState: TNyxDataValue;
  LFrame: TNyxDataValue;
  LSummary: TNyxDataValue;
begin

  if not Assigned(FReply) then
  begin
    raise Exception.Create('Prepare requires an owned editor request');
  end;
  FStatus := 200;
  try

    if AFull and not FConnect then
    begin
      FBody := FieldCopy(FBody, 'after', NyxData(0));
    end;

    if FConnect then
    begin
      Receipt := FEngine.ConnectEditor(FBody);
      LState := Receipt.Field('state');
    end
    else
    begin
      Receipt := FEngine.EditorExchange(FToken, FBody);
      LState := Receipt;
    end;
    case Fault of
      rfNone:
        begin
          { Preserve the exact real owning-engine response. }
        end;
      rfIssuer:
        LState := FieldCopy(LState, 'sourceObservationIssuer', NyxData('another-server'));
      rfMissing:
        LState := FieldCopy(LState, 'sourceObservation', NyxNull, True);
      rfLegacy:
        begin
          LState := FieldCopy(LState, 'sourceObservationIssuer', NyxNull, True);
          LState := FieldCopy(LState, 'sourceObservation', NyxNull, True);
        end;
      rfWorkspace, rfRevision, rfBoundary:
        begin
          LFrame := LState.Field('sourceObservation');
          case Fault of
            rfWorkspace:
              LFrame := FieldCopy(LFrame, 'workspace', NyxData('another-project'));
            rfRevision:
              LFrame := FieldCopy(LFrame, 'revision',
                NyxData(LFrame.Field('revision').AsInteger + 1));
            rfBoundary:
              LFrame := FieldCopy(LFrame, 'prefixBytes', NyxData(-1));
          else
            begin
              raise Exception.Create('Only source-frame faults reach this branch');
            end;
          end;
          LState := FieldCopy(LState, 'sourceObservation', LFrame);
        end;
      rfStale:
        begin
          LSummary := LState.Field('session');
          LSummary := FieldCopy(LSummary, 'revision',
            NyxData(LSummary.Field('revision').AsInteger - 1));
          LState := FieldCopy(LState, 'session', LSummary);
        end;
    end;

    if FConnect then
    begin
      Receipt := FieldCopy(Receipt, 'state', LState);
    end
    else
    begin
      Receipt := LState;
    end;
    FResponse := Receipt.ToJSON;
  except
    on LException: Exception do
    begin
      FStatus := 409;
      FResponse := NyxObject([NyxField('error', NyxData(TNyxText(LException.Message)))]).ToJSON;
    end;
  end;
end;

procedure TEngineExchange.Deliver;
var
  LReply: TNyxEditorReply;
  LResponse: TNyxText;
begin
  LReply := FReply;
  LResponse := FResponse;
  FReply := nil;
  FResponse := '';
  LReply(FStatus, LResponse);
end;

procedure TEngineExchange.FireTick;
var
  LTick: TNyxEditorTick;
begin
  LTick := FTick;
  FTick := nil;

  if not Assigned(LTick) then
  begin
    raise Exception.Create('The portable bridge must own a scheduled tick');
  end;
  LTick;
end;

function TEngineExchange.Pending: Boolean;
begin
  Result := Assigned(FReply);
end;

procedure RoundTrip(AExchange: TEngineExchange; AFull: Boolean = False);
begin

  if not AExchange.Pending then
  begin
    AExchange.FireTick;
  end;
  AExchange.Prepare(AFull);
  AExchange.Deliver;
end;

procedure CompleteHistory(ABridge: TNyxStudioAgentBridge;
  AExchange: TEngineExchange; ADirection: TNyxEditorHistory);
var
  LPass: Integer;
begin
  { RecordLocal can already own an observation when the history intent queues.
    Complete the real bounded acknowledgement sequence, not just its first reply. }
  ABridge.History(ADirection);
  for LPass := 1 to 4 do
  begin

    if not ABridge.SourceSynchronized or AExchange.Pending then
    begin
      RoundTrip(AExchange);
    end;
  end;
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 1) or (LStream.Size > 4 * 1024 * 1024) then
    begin
      raise Exception.Create('Qualification requires a bounded owned input');
    end;
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure RefuseReply(AFault: TReplyFault);
var
  LSession: TNyxStudioSession;
  LBridge: TNyxStudioAgentBridge;
  LExchange: TEngineExchange;
  LBefore: TNyxText;
  LRevision: Integer;
begin
  LSession := TNyxStudioSession.Create;
  LBridge := nil;
  try
    LExchange := TEngineExchange.Create(GEngine);
    LBridge := TNyxStudioAgentBridge.Create(LSession, nil, NyxPrimaryWorkspace, LExchange);
    LBridge.Connect;
    RoundTrip(LExchange);
    Check(not LBridge.State.Conflict, 'Fresh owning claim admits executed shared content');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRevision := LBridge.State.Revision;
    LExchange.Fault := AFault;
    RoundTrip(LExchange, True);
    Check(LBridge.State.Conflict and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore) and
      (LBridge.State.Revision = LRevision) and not LSession.CanUndo and not LSession.CanRedo,
      'Faulted observation retains exact pair, revision and history');
  finally
    LBridge.Free;
    LSession.Free;
  end;
end;

var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LExecutor: TNyxBuildExecutor;
  LSession: TNyxStudioSession;
  LSecond: TNyxStudioSession;
  LBridge: TNyxStudioAgentBridge;
  LSecondBridge: TNyxStudioAgentBridge;
  LExchange: TEngineExchange;
  LSecondExchange: TEngineExchange;
  LRequest: TNyxStudioSourcePublication;
  LBuild: INyxSourceProjectionBuild;
  LTools: TNyxDataValue;
  LState: TNyxDataValue;
  LFrame: TNyxDataValue;
  LCheckpoint: TNyxSourceCheckpoint;
  LPair: TNyxProjectPair;
  LBaseline: TNyxProjectPair;
  LSource: TNyxText;
  LToken: TNyxText;
  LIssuer: TNyxText;
  LRevision: Integer;
  LRejected: Boolean;
  LFault: TReplyFault;
  LOtherWorkspace: TNyxWorkspaceRef;
begin
  GEngine := nil;
  LProfile := nil;
  LExecutor := nil;
  LSession := nil;
  LSecond := nil;
  LBridge := nil;
  LSecondBridge := nil;
  try

    if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
    end;
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LSource := ReadText(LDirectories.SourceRoot + 'tests/fixtures/' + CUnit + '.pas');
    LSession := TNyxStudioSession.Create;
    LPair := LSession.ProjectSnapshot;
    LPair.Source := CPrefix + LPair.Source;
    LSession.LoadProject(LPair);
    LSession.SetSourceDraft(LSource);
    LBaseline := LSession.ProjectSnapshot;
    GEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763, LProfile.Encode);
    LExchange := TEngineExchange.Create(GEngine);
    LBridge := TNyxStudioAgentBridge.Create(LSession, nil, NyxPrimaryWorkspace, LExchange);
    LBridge.Connect;
    RoundTrip(LExchange);
    Check(LBridge.SourceSynchronized, 'Private claim authenticates an exact pending pair');
    LState := LExchange.Receipt.Field('state');
    LToken := LExchange.Receipt.Field('token').AsText;
    LIssuer := LState.Field('sourceObservationIssuer').AsText;
    LRevision := LBridge.State.Revision;
    LFrame := LState.Field('sourceObservation');
    Check((Length(LFrame.ToJSON) < 512) and (LFrame.Count = 8),
      'Changed-pair observation adds a bounded frame without duplicate source/design');
    LCheckpoint := ReceiveNyxSourceObservation(LFrame, LIssuer, NyxPrimaryWorkspace,
      LRevision, LBaseline);
    Check((LCheckpoint.Source = LBaseline.Source) and
      (LCheckpoint.Design = LBaseline.Design) and (LCheckpoint.Origin = nsoDeclarative),
      'UTF-8 source frame reconstructs exact admitted literal files');
    LRejected := False;
    try
      ReceiveNyxSourceObservation(FieldCopy(LFrame, 'prefixBytes',
        NyxData(NyxUTF8ByteCount('{ observer prefix ') + 1)), LIssuer,
        NyxPrimaryWorkspace, LRevision, LBaseline);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Frame boundaries may not split a supplementary UTF-8 scalar');
    LPair := LBaseline;
    LPair.Source := LPair.Source + #10;
    LRejected := False;
    try
      EncodeNyxSourceObservation(LIssuer, NyxPrimaryWorkspace, LRevision, LPair, LCheckpoint);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'The server may encode only its exact captured accepted pair');
    LSecond := TNyxStudioSession.Create;
    LSecondExchange := TEngineExchange.Create(GEngine);
    LSecondExchange.Fault := rfLegacy;
    LSecondBridge := TNyxStudioAgentBridge.Create(LSecond, nil, NyxPrimaryWorkspace, LSecondExchange);
    LSecondBridge.Connect;
    RoundTrip(LSecondExchange);
    Check(LSecondBridge.SourceSynchronized and (LSecond.Source = LBaseline.Source),
      'An older peer remains compatible with ordinary literal project admission');
    RoundTrip(LExchange);
    Check(not LBridge.State.Conflict and (LExchange.Receipt.Count > 0) and
      not NyxAgentHas(LExchange.Receipt, 'project') and
      not NyxAgentHas(LExchange.Receipt, 'sourceObservation'),
      'Unchanged observer replies omit both complete pair and source frame');
    LRequest := GEngine.EditorCaptureSourcePublication(LToken,
      NyxPrimaryWorkspace, LRevision, LSource);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LBuild := LExecutor.ProjectSource(LRequest.Source, NyxPascalUnit(CUnit), btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = ExpectedNyxProjectionDesign) and
      (Pos('0 unfreed memory blocks', LBuild.RuntimeLog) > 0),
      'Actual compiler helper/loop execution produces the independently expected design');
    FreeAndNil(LExecutor);
    GEngine.EditorCommitSourceProjection(LToken, LRequest,
      LBuild.Projection, NyxControl('heading-1'), NyxControl('notebook-1'));
    RoundTrip(LExchange);
    Check(LBridge.SourceSynchronized and
      (LSession.Source = LSource) and
      (LSession.AcceptedSourceCheckpoint.Origin = nsoExecuted) and
      (LSession.ProjectSnapshot.Design = ExpectedNyxProjectionDesign),
      'Ordinary observing bridge adopts real executed Pascal without literal reinterpretation');
    RoundTrip(LSecondExchange);
    Check(LSecondBridge.State.Conflict and (LSecond.Source = LBaseline.Source),
      'An unnegotiated peer cannot acquire executed origin from generic project strings');
    FreeAndNil(LSecondBridge);
    FreeAndNil(LSecond);
    Check((LSession.Selected.ID = 'heading-1') and
      (LSession.ActiveView.ID = 'notebook-1') and LSession.CanUndo,
      'Observation selects the admitted root/control and retains one local paired Undo');
    LSession.Undo;
    Check((LSession.Source = LBaseline.Source) and
      (LSession.AcceptedSourceCheckpoint.Origin = nsoDeclarative) and LSession.CanRedo,
      'Observer Undo restores the literal accepted companion and origin');
    LSession.Redo;
    Check((LSession.Source = LSource) and
      (LSession.AcceptedSourceCheckpoint.Origin = nsoExecuted), 'Observer Redo restores executed origin');
    { Direct local history is a model qualification here. Restore the acknowledged
      presentation frame before allowing the bridge to observe another edit. }
    LSession.Activate('notebook-1');
    LSession.Select('heading-1');
    LSecond := TNyxStudioSession.Create;
    LSecondExchange := TEngineExchange.Create(GEngine);
    LSecondBridge := TNyxStudioAgentBridge.Create(LSecond, nil, NyxPrimaryWorkspace, LSecondExchange);
    LSecondBridge.Connect;
    RoundTrip(LSecondExchange);
    Check(LSecondBridge.SourceSynchronized and (LSecond.Source = LSource) and
      not LSecond.CanUndo and (LSecond.Document <> LSession.Document),
      'A new owning claim loads independent executed owners with fresh project history');
    LOtherWorkspace := NyxWorkspace(GEngine.InvokeTool('nyx_workspaces',
      'owned-observer-qualification', 'Scooty', NyxObject([
        NyxField('mode', NyxData('create')),
        NyxField('expectedRevision', NyxData(LBridge.State.Revision)),
        NyxField('operationId', NyxData('independent-observer-project')),
        NyxField('label', NyxData('Independent observer project')),
        NyxField('base', NyxData('empty'))])).Field('workspace').AsText);
    LState := GEngine.EditorExchange(LToken, NyxWithWorkspace(NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]), LOtherWorkspace));
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    LCheckpoint := ReceiveNyxSourceObservation(LState.Field('sourceObservation'),
      LIssuer, LOtherWorkspace, LState.Field('session').Field('revision').AsInteger, LPair);
    Check((LState.Field('workspace').AsText = LOtherWorkspace.ID) and
      (LCheckpoint.Source = LPair.Source) and (LCheckpoint.Origin = nsoDeclarative),
      'Private observation names and admits the independently selected workspace');
    LRejected := False;
    try
      ReceiveNyxSourceObservation(LState.Field('sourceObservation'), LIssuer,
        NyxPrimaryWorkspace, LState.Field('session').Field('revision').AsInteger, LPair);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'A real other-project frame cannot be retargeted to the primary context');
    LPair := LSession.ProjectSnapshot;
    LPair.Pending := True;
    LPair.DraftBase := LSource;
    LPair.Draft := LSource + CDraft;
    LRevision := LBridge.State.Revision;
    GEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('heading-1')), NyxField('view', NyxData('notebook-1'))]));
    RoundTrip(LExchange);
    Check(LBridge.SourceSynchronized and (LSession.DraftSource = LPair.Draft) and
      LSession.CanUndo, 'Remote draft-only updates preserve exact supplementary text and paired history: ' +
      LBridge.State.Status);
    LSecondExchange.FireTick;
    LSecondExchange.Prepare;
    LSecond.SetSourceDraft('Independent local observer draft');
    LSecondBridge.RecordDraft;
    LSecondExchange.Deliver;
    Check(LSecondBridge.State.Conflict and
      (LSecond.DraftSource = 'Independent local observer draft') and
      (LSecond.Source = LSource), 'A prepared remote update cannot replace a newly typed local draft');
    FreeAndNil(LSecondBridge);
    FreeAndNil(LSecond);
    for LFault := rfIssuer to rfBoundary do
    begin
      RefuseReply(LFault);
    end;
    LRejected := False;
    try
      LSession.LoadProject(LPair);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Source = LSource),
      'Ordinary project import does not inherit owning-channel execution authority');
    CompleteHistory(LBridge, LExchange, nehUndo);
    Check(LBridge.SourceSynchronized and (LSession.Source = LBaseline.Source) and
      (LSession.AcceptedSourceCheckpoint.Origin = nsoDeclarative) and
      not LSession.ProjectSnapshot.Pending,
      'Ordinary shared Undo observes the earlier complete literal pair: ' +
      LBridge.State.Status + ' / source=' + BoolToStr(LSession.Source = LBaseline.Source, True) +
      ' / pending=' + BoolToStr(LSession.ProjectSnapshot.Pending, True));
    CompleteHistory(LBridge, LExchange, nehRedo);
    Check(LBridge.SourceSynchronized and (LSession.Source = LSource) and
      (LSession.AcceptedSourceCheckpoint.Origin = nsoExecuted) and
      (LSession.DraftSource = LPair.Draft),
      'Ordinary shared Redo observes executed origin and the same unfinished draft');
    FreeAndNil(LBridge);
    FreeAndNil(LSession);
    FreeAndNil(GEngine);
    FreeAndNil(LProfile);
    WriteLn('PASS ', GChecks, ' private source observer bridge checks');
  except
    on LException: Exception do
    begin
      LExecutor.Free;
      LSecondBridge.Free;
      LSecond.Free;
      LBridge.Free;
      LSession.Free;
      GEngine.Free;
      LProfile.Free;
      WriteLn('FAIL ', LException.Message);
      Halt(1);
    end;
  end;
end.
