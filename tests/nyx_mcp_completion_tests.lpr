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

program nyx_mcp_completion_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, SyncObjs, nyx.text, nyx.bytes, nyx.data, nyx.editing,
  nyx.pascal.completion, nyx.pascal.sources, nyx.pascal.drafts,
  nyx.studio.completion.native, nyx.studio.mcp, nyx.studio.session,
  nyx.studio.projects, nyx.studio.directories, nyx.studio.agents, nyx.studio.outputs,
  nyx.studio.reviews, nyx.studio.workspaces;

type
  { Controlled provider outcomes qualify the host boundary, not Pascal language
    resolution. The real optional SDK provider is exercised separately below. }
  TReplyBehavior = (rbReady, rbForeignQuery, rbException, rbLargePage);

  { Owns two native events. Tests retain its interface through every joined query;
    it borrows no engine or editor. A gate permits real concurrent operator work
    while Complete is outside the model guard, with a bounded refusal fallback. }
  TGateProvider = class(TInterfacedObject, INyxPascalCompletionProvider)
  public
    Entered: TEvent;
    ContinueQuery: TEvent;
    Calls: Integer;
    Behavior: TReplyBehavior;
    constructor Create(ABlock: Boolean);
    destructor Destroy; override;
    function Complete(const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
  end;

  { One actual native semantic request. The test owns/joins this thread before
    freeing its borrowed engine or inspecting its detached reply/error fields. }
  TQueryThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Engine: TNyxStudioMCP;
    Arguments: TNyxDataValue;
    Reply: TNyxDataValue;
    Failure: TNyxText;
  end;

const
  COwner: TNyxText = 'completion-owned-transport';
  CActor: TNyxText = 'Scooty';
  CDeclarationTail: TNyxText = ';' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.controls; procedure Run; implementation' + #10 +
    'procedure Run; var LBadge: INyxBadge; begin' + #10 +
    '// Dedicated Unicode qualification 🚀 𐐷' + #10 + '  LBadge.';
  CSuffix: TNyxText = 'WithTexx(''Ready'');' + #10 + 'end; end.';

var
  GEngine: TNyxStudioMCP;
  GToken: TNyxText;
  GRevision: Integer;
  GChecks: Integer;
  GStage: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

constructor TGateProvider.Create(ABlock: Boolean);
begin
  inherited Create;
  Entered := TEvent.Create(nil, True, False, '');
  ContinueQuery := TEvent.Create(nil, True, not ABlock, '');
end;

destructor TGateProvider.Destroy;
begin
  ContinueQuery.Free;
  Entered.Free;
  inherited Destroy;
end;

function TGateProvider.Complete(
  const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
var
  LQuery: INyxPascalCompletionRequest;
  LItems: array of TNyxPascalCompletionItem;
  LAt: Integer;
begin
  Inc(Calls);
  Entered.SetEvent;

  if ContinueQuery.WaitFor(5000) <> wrSignaled then
  begin
    raise Exception.Create('Owned completion gate expired');
  end;

  if Behavior = rbException then
  begin
    raise Exception.Create('PRIVATE-PROVIDER-PATH-MUST-NOT-ESCAPE');
  end;
  LQuery := ARequest;

  if Behavior = rbForeignQuery then
  begin
    LQuery := ARequest.At(ARequest.FileReference, ARequest.Caret - 1);
  end;
  SetLength(LItems, 1);
  LItems[0] := NyxPascalCompletion('WithText', 'WithText', 'A typed method', pckMethod);

  if Behavior = rbLargePage then
  begin
    SetLength(LItems, 50);
    for LAt := 0 to High(LItems) do
    begin
      LItems[LAt] := NyxPascalCompletion('Member' + IntToStr(LAt),
        'Member' + IntToStr(LAt), StringOfChar('d', 1800), pckMethod);
    end;
  end;
  Result := NewNyxPascalCompletionResult(LQuery, pcsReady, '',
    LQuery.Caret, '', LItems, Length(LItems));
end;

procedure TQueryThread.Execute;
begin
  try
    Reply := Engine.InvokeTool('nyx_pascal', COwner, CActor, Arguments);
  except
    on E: Exception do
    begin
      Failure := E.Message;
    end;
  end;
end;

function Observe: TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
end;

function Args(ARevision, ACaret: Integer; ALimit: Integer = 5): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('complete')),
    NyxField('expectedRevision', NyxData(ARevision)), NyxField('caret', NyxData(ACaret)),
    NyxField('limit', NyxData(ALimit))]);
end;

procedure Permission(const AValue: TNyxText);
begin
  GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('configure')), NyxField('permission', NyxData(AValue))]));
end;

procedure Refuse(const AArguments: TNyxDataValue; const AReason: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    GEngine.InvokeTool('nyx_pascal', COwner, CActor, AArguments);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
end;

procedure Retained(const ABefore: TNyxDataValue; const AReason: TNyxText);
var
  LAfter: TNyxDataValue;
  LKey: TNyxText;
begin
  LAfter := Observe;
  Check(LAfter.Field('project').AsText = ABefore.Field('project').AsText,
    AReason + ': complete accepted/draft/base/helper pair remains exact');
  for LKey in ['revision', 'selection', 'view', 'canUndo', 'canRedo'] do
  begin
    Check(LAfter.Field('session').Field(LKey).ToJSON =
      ABefore.Field('session').Field(LKey).ToJSON, AReason + ': ' + LKey);
  end;
end;

function StartQuery(const AArguments: TNyxDataValue; out AGate: TGateProvider;
  out AProvider: INyxPascalCompletionProvider): TQueryThread;
begin
  AGate := TGateProvider.Create(True);
  AProvider := AGate;
  GEngine.ConfigureCompletionProvider(AProvider);
  Result := TQueryThread.Create(True);
  Result.FreeOnTerminate := False;
  Result.Engine := GEngine;
  Result.Arguments := AArguments;
  Result.Start;

  if AGate.Entered.WaitFor(5000) <> wrSignaled then
  begin
    AGate.ContinueQuery.SetEvent;
    Result.WaitFor;
    Result.Free;
    Result := nil;
    raise Exception.Create('Semantic request did not enter its owned provider');
  end;
end;

procedure JoinQuery(var AThread: TQueryThread; AGate: TGateProvider;
  ARefused: Boolean; const AReason: TNyxText);
begin
  AGate.ContinueQuery.SetEvent;
  AThread.WaitFor;
  try
    Check((AThread.Failure <> '') = ARefused, AReason);
  finally
    FreeAndNil(AThread);
  end;
end;

function ReadText(const APath: String): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LFile.Size);

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure Run;
var
  LLocal: TNyxStudioSession;
  LProfile: TNyxOutputConfiguration;
  LDirectories: TNyxStudioDirectories;
  LTools: TNyxDataValue;
  LClaim: TNyxDataValue;
  LBefore: TNyxDataValue;
  LFrame: TNyxDataValue;
  LReply: TNyxDataValue;
  LArguments: TNyxDataValue;
  LPair: TNyxProjectPair;
  LPrefix: TNyxText;
  LCaret: Integer;
  LNative: INyxPascalCompletionProvider;
  LProvider: INyxPascalCompletionProvider;
  LGate: TGateProvider;
  LThread: TQueryThread;
  LStarted: QWord;
  LReview: TNyxReviewRef;
  LReviewRevision: Integer;
  LWorkspace: TNyxWorkspaceRef;
begin
  LLocal := nil;
  LProfile := nil;
  LThread := nil;
  LGate := nil;
  try

    if (ParamCount <> 5) or DirectoryExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain JSON, fresh owned runtime, optional helper and FPC source root');
    end;
    GStage := 'prepare owned inputs';
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
    LLocal := TNyxStudioSession.Create;
    LPrefix := TNyxText('unit ') + LLocal.AcceptedPascalProject.MainUnit.Name + CDeclarationTail;
    LCaret := NyxTextScalarCount(LPrefix) + 6;
    LLocal.SetSourceDraft(LPrefix + CSuffix);
    LDirectories := TNyxStudioDirectories.ForRepository(TNyxText(ParamStr(1)))
      .RunningIn(TNyxText(ParamStr(3))).EnrollingProject(TNyxText(ParamStr(3)));
    { Construct the production engine suspended; no Start or HTTP request occurs.
      Only this explicit fresh runtime receives private enrollment/checkpoints. }
    GStage := 'construct never-listening engine';
    LProfile := TNyxOutputConfiguration.Create;
    GEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763, LProfile.Encode);
    GStage := 'claim owned editor';
    LClaim := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LLocal.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LClaim.Field('token').AsText;
    GRevision := LClaim.Field('state').Field('session').Field('revision').AsInteger;
    LBefore := Observe;
    GStage := 'unconfigured completion';
    LReply := GEngine.InvokeTool('nyx_pascal', COwner, CActor, Args(GRevision, LCaret));
    Check((LReply.Field('status').AsText = 'unavailable') and
      (LReply.Field('snapshot').AsText = 'editing') and
      (LReply.Field('items').Count = 0), 'Absent optional provider leaves a useful current-buffer capability result');
    Retained(LBefore, 'Unavailable completion');
    LReply := GEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
      NyxField('mode', NyxData('complete')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('snapshot', NyxData('accepted')), NyxField('caret', NyxData(0))]));
    Check((LReply.Field('snapshot').AsText = 'accepted') and
      (LReply.Field('status').AsText = 'unavailable'),
      'Explicit accepted completion remains revision-pinned independently of the pending buffer');
    Refuse(NyxObject([NyxField('mode', NyxData('complete')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('caret', NyxData(0)),
      NyxField('snapshot', NyxData('future'))]), 'Unknown source origins refuse before analysis');

    Refuse(Args(GRevision - 1, LCaret), 'Stale revisions refuse before dispatch');
    Refuse(NyxObject([NyxField('mode', NyxData('complete')), NyxField('caret', NyxData(LCaret))]),
      'Missing revision refuses even when completion is read-only');
    Refuse(Args(GRevision, NyxTextScalarCount(LPrefix + CSuffix) + 1),
      'Caret outside its exact buffer refuses before dispatch');
    Refuse(NyxObject([NyxField('mode', NyxData('complete')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('caret', NyxData(LCaret)),
      NyxField('executable', NyxData('untrusted'))]), 'Wire callers cannot configure host executables');

    GStage := 'real SDK provider';
    LNative := NewNyxNativePascalCompletionProvider(TNyxPascalCompletionHost.Using(
      ParamStr(4), ParamStr(1), String(LTools.Field('FPC').AsText),
      ParamStr(5), String(LTools.Field('LAZARUS').AsText),
      IncludeTrailingPathDelimiter(ParamStr(3)) + 'completion-queries'));
    GEngine.ConfigureCompletionProvider(LNative);
    Permission('readOnly');
    LBefore := Observe;
    LReply := GEngine.InvokeTool('nyx_pascal', COwner, CActor, Args(GRevision, LCaret));
    Check((LReply.Field('status').AsText = 'ready') and
      (LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('name').AsText = 'WithText') and
      (LReply.Field('items').Item(0).Field('kind').AsText = 'method'),
      'Read-only semantic completion resolves the real specialized SDK interface');
    Check((LReply.Field('replacement').Field('expected').AsText = 'WithTexx') and
      (LReply.Field('replacement').Field('offset').AsInteger = NyxTextScalarCount(LPrefix)) and
      LReply.Field('inspectionOnly').AsBoolean and LReply.Field('pendingDrafts').AsBoolean,
      'Semantic rows retain exact scalar replacement metadata and the unfinished-draft boundary');
    Check(not NyxAgentHas(LReply, 'source') and not NyxAgentHas(LReply, 'project') and
      (NyxUTF8ByteCount(LReply.ToJSON) < 4096), 'A bounded completion query exposes no complete source document');
    Retained(LBefore, 'Actual optional-engine completion');

    Permission('edit');
    GStage := 'concurrent operator observation';
    LThread := StartQuery(Args(GRevision, LCaret), LGate, LProvider);
    LStarted := GetTickCount64;
    LFrame := Observe;
    Check(GetTickCount64 - LStarted < 1000, 'Ordinary editor observation proceeds while provider analysis is gated');
    Check(LFrame.Field('activity').Item(LFrame.Field('activity').Count - 1)
      .Field('outcome').AsText = 'resolving completion', 'Observers see scoped in-flight completion activity');
    LBefore := LFrame;
    JoinQuery(LThread, LGate, False, 'Unchanged query returns after independent analysis');
    Retained(LBefore, 'Detached successful completion');

    GStage := 'independent workspace activity';
    LThread := StartQuery(Args(GRevision, LCaret), LGate, LProvider);
    LReply := GEngine.InvokeTool('nyx_workspaces', COwner, CActor, NyxObject([
      NyxField('mode', NyxData('create')), NyxField('base', NyxData('empty')),
      NyxField('label', NyxData('Independent completion project')),
      NyxField('operationId', NyxData('completion-parallel-project')),
      NyxField('expectedRevision', NyxData(GRevision))]));
    LWorkspace := NyxWorkspace(LReply.Field('workspace').AsText);
    JoinQuery(LThread, LGate, False,
      'An independent project can be created without invalidating the unchanged primary query');
    LGate := TGateProvider.Create(False);
    LProvider := LGate;
    GEngine.ConfigureCompletionProvider(LProvider);
    LReply := GEngine.InvokeTool('nyx_pascal', COwner, CActor,
      NyxWithWorkspace(Args(1, 0), LWorkspace));
    Check((LReply.Field('workspace').AsText = LWorkspace.ID) and
      (LReply.Field('revision').AsInteger = 1) and
      (LReply.Field('caret').AsInteger = 0),
      'Workspace completion returns only its own revision-pinned scope');
    Retained(LBefore, 'Independent workspace completion');

    GStage := 'provider and permission retirement';
    LThread := StartQuery(Args(GRevision, LCaret), LGate, LProvider);
    GEngine.ConfigureCompletionProvider(nil);
    GEngine.ConfigureCompletionProvider(LProvider);
    JoinQuery(LThread, LGate, True, 'Replacing then restoring the same provider retires the old query');
    LThread := StartQuery(Args(GRevision, LCaret), LGate, LProvider);
    Permission('disabled');
    Permission('edit');
    JoinQuery(LThread, LGate, True, 'Disabling then reenabling agents cannot revive the prior query');

    GStage := 'new operator text';
    LThread := StartQuery(Args(GRevision, LCaret), LGate, LProvider);
    LFrame := Observe;
    LPair := DecodeNyxProject(LFrame.Field('project').AsText);
    LPair.Draft := LPair.Draft + #10 + '// Newer operator text';
    LFrame := GEngine.EditorExchange(GToken, NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GRevision := LFrame.Field('session').Field('revision').AsInteger;
    JoinQuery(LThread, LGate, True, 'Ordinary typing wins over an older completion page');
    Retained(LFrame, 'Refused stale completion');

    GStage := 'provider response guards';
    LGate := TGateProvider.Create(False);
    LProvider := LGate;
    GEngine.ConfigureCompletionProvider(LProvider);
    LGate.Behavior := rbForeignQuery;
    Refuse(Args(GRevision, LCaret), 'Provider cannot substitute a nearby caret with the same source');
    LGate.Behavior := rbException;
    Refuse(Args(GRevision, LCaret), 'Provider exceptions refuse without publishing a page');
    LFrame := Observe;
    Check(Pos('PRIVATE-PROVIDER-PATH-MUST-NOT-ESCAPE', LFrame.ToJSON) = 0,
      'External exception diagnostics never leak through operator activity');
    LGate.Behavior := rbLargePage;
    Refuse(Args(GRevision, LCaret, 50), 'Large valid provider vectors retain the semantic 48 KiB response limit');
    Permission('disabled');
    Refuse(Args(GRevision, LCaret), 'Disabled agents cannot dispatch completion');
    Permission('edit');

    GStage := 'review retirement';
    LReply := GEngine.InvokeTool('nyx_reviews', COwner, CActor, NyxObject([
      NyxField('mode', NyxData('create')), NyxField('base', NyxData('empty')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('owned-completion-review')),
      NyxField('label', NyxData('Completion review'))]));
    LReview := NyxReview(LReply.Field('review').AsText);
    LReviewRevision := GEngine.InvokeTool('nyx_session', COwner, CActor,
      NyxWithReview(NyxObject([]), LReview)).Field('revision').AsInteger;
    LArguments := NyxWithReview(Args(LReviewRevision, 0), LReview);
    LThread := StartQuery(LArguments, LGate, LProvider);
    GEngine.InvokeTool('nyx_reviews', COwner, CActor, NyxObject([
      NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LReview.ID)),
      NyxField('expectedRevision', NyxData(LReviewRevision)),
      NyxField('operationId', NyxData('retire-completion-review'))]));
    JoinQuery(LThread, LGate, True, 'Retired review is resolved afresh instead of dereferencing its former session');
    Retained(LFrame, 'Independent review retirement');
  finally

    if LThread <> nil then
    begin
      LGate.ContinueQuery.SetEvent;
      LThread.WaitFor;
      LThread.Free;
    end;
    GEngine.Free;
    GEngine := nil;
    LProvider := nil;
    LNative := nil;
    LLocal.Free;
    LProfile.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' production semantic completion checks');
  except
    on E: Exception do
    begin
      WriteLn(StdErr, GStage, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
