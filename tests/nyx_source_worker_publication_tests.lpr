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
program nyx_source_worker_publication_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.schema, nyx.studio.projects, nyx.studio.session, nyx.studio.mcp,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.builds,
  nyx.studio.workspaces, nyx.studio.sourceprojection, nyx.studio.sourcebuilds,
  nyx.studio.sourcepublications, nyx.test.projection;

type
  { Fixture-only immutable wire substitution. Never share or mutate an admitted
    object; preserve every unrelated member when exercising malformed replies. }
  TWireFields = record helper for TNyxDataValue
    function WithField(const AName: TNyxText; const AValue: TNyxDataValue): TNyxDataValue;
  end;

var
  GChecks: Integer;
  GEngine: TNyxStudioMCP;
  GToken: TNyxText;
  GIssuer: TNyxText;
  GSource: TNyxText;
  GWorkspace: TNyxWorkspaceRef;

function TWireFields.WithField(const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
begin
  LFound := False;
  SetLength(LFields, Count);
  for LIndex := 0 to Count - 1 do
  begin
    LFields[LIndex] := NyxField(Key(LIndex), Field(Key(LIndex)));

    if Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField(AName, AValue);
  end;
  Result := NyxObject(LFields);
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Delegated source publication: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Read exact bounded bytes, without ANSI collections or a global codepage. }
function ReadBytes(const APath: TNyxText): TNyxBytes;
var
  LFile: TFileStream;
begin
  Result := nil;
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 16 * 1024 * 1024) then
    begin
      raise Exception.Create('Qualification input requires bounded nonempty bytes');
    end;
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[0], Length(Result));
  finally
    LFile.Free;
  end;
end;

function SameBytes(const ALeft, ARight: TNyxBytes): Boolean;
var
  LIndex: Integer;
begin
  Result := Length(ALeft) = Length(ARight);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(ALeft) do
  begin

    if ALeft[LIndex] <> ARight[LIndex] then
    begin
      Exit(False);
    end;
  end;
end;

function Observe: TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxWithWorkspace(NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]), GWorkspace));
end;

function Exchange(const AArguments: TNyxDataValue;
  const AToken: TNyxText = ''): TNyxDataValue;
var
  LToken: TNyxText;
begin
  LToken := AToken;

  if LToken = '' then
  begin
    LToken := GToken;
  end;
  Result := GEngine.EditorSourceExchange(LToken,
    NyxWithWorkspace(NyxObject([NyxField('compile', AArguments)]), GWorkspace));
end;

function Request(const AOperation: TNyxText; APublish: Boolean;
  ARevision: Integer): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('request')),
    NyxField('operationId', NyxData(AOperation)),
    NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('source', NyxData(GSource))]);

  if APublish then
  begin
    Result := Result.WithField('publish', NyxData(True)).WithField('issuer', NyxData(GIssuer));
  end;
end;

function Completion(const ABuild: INyxSourceProjectionBuild;
  const AProducer: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('complete')),
    NyxField('reference', NyxData(ABuild.Reference.Name)),
    NyxField('projection', NyxData(AProducer.ToJSON))]);
end;

{ Activity deliberately advances when a job is admitted. It is observational
  metadata, so exclude only that field from exact revision/selection/history. }
function SessionMeaning(const AState: TNyxDataValue): TNyxText;
begin
  Result := AState.Field('session').WithField('activitySequence', NyxData(0)).ToJSON;
end;

{ Refusal assertions compare the whole paired project and session history. Job
  staging/activity is permitted, but cannot change accepted editor meaning. }
procedure Refuse(const AArguments: TNyxDataValue; const AReason: TNyxText;
  const AToken: TNyxText = '');
var
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LRejected: Boolean;
begin
  LBefore := Observe;
  LRejected := False;
  try
    Exchange(AArguments, AToken);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  LAfter := Observe;
  Check(LRejected and (LAfter.Field('project').AsText = LBefore.Field('project').AsText) and
    (SessionMeaning(LAfter) = SessionMeaning(LBefore)), AReason);
end;

function Compile(const AOperation: TNyxText; APublish: Boolean): INyxSourceProjectionBuild;
var
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin
  LReceipt := Exchange(Request(AOperation, APublish,
    Observe.Field('session').Field('revision').AsInteger));
  LStarted := GetTickCount64;
  repeat
    LStatus := Exchange(NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', LReceipt.Field('job'))]));

    if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) then
    begin
      Break;
    end;
    Sleep(5);
  until GetTickCount64 - LStarted > 180000;
  Check(LStatus.Field('state').AsText = 'succeeded', 'actual pas2js job joins successfully');
  Result := DecodeNyxBrowserSourceBuild(GSource, LStatus.Field('receipt'));
  Check((Result.Projection.State = spsCompiled) and (Result.Projection.Design = ''),
    'compiler alone conveys no executed design');
end;

{ This is intentionally a simulated owning-browser producer envelope, using
  independently authored expected meaning. Actual pas2js compilation above and
  native admission below are real. This fixture does NOT execute a browser
  worker, HTTP exchange or UI; those require the separate browser consumer. }
function Producer(const ABuild: INyxSourceProjectionBuild): TNyxDataValue;
var
  LDocument: TNyxDocument;
begin
  LDocument := TNyxCodec.Decode(ExpectedNyxProjectionDesign);
  try
    Result := CaptureNyxSourceProjection(LDocument, ABuild.Reference, btBrowser);
  finally
    LDocument.Free;
  end;
end;

procedure ReceiptRefusal(const AData: TNyxDataValue; const AReference: TNyxSourceProjectionRef;
  const AReason: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    DecodeNyxSourcePublicationReceipt(AData, GIssuer, GWorkspace, AReference);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
end;

{ The native service executes the complete helper/loop unit with real FPC. These
  cases exercise the production private exchange/job/publication path in process;
  they do not start an HTTP listener or substitute a simulated native producer. }
procedure NativeServiceChecks(const ADirectories: TNyxStudioDirectories);
var
  LBefore: TNyxDataValue;
  LState: TNyxDataValue;
  LArguments: TNyxDataValue;
  LAdmission: TNyxDataValue;
  LWire: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LPair: TNyxProjectPair;
  LBuild: INyxSourceProjectionBuild;
  LStale: INyxSourceProjectionBuild;
  LCompileOnly: INyxSourceProjectionBuild;
  LDecoded: TNyxSourcePublicationReceipt;
  LFirst: TNyxDocument;
  LSecond: TNyxDocument;
  LOriginalSource: TNyxText;
  LLock: TFileStream;
  LBeforeBytes: TNyxBytes;

  function Complete(const ABuild: INyxSourceProjectionBuild): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('complete')),
      NyxField('reference', NyxData(ABuild.Reference.Name))]);
  end;

  function WaitNative(const AArguments: TNyxDataValue): INyxSourceProjectionBuild;
  var
    LReply: TNyxDataValue;
    LStatus: TNyxDataValue;
    LStarted: QWord;
  begin
    LReply := Exchange(AArguments);
    LStarted := GetTickCount64;
    repeat
      LStatus := Exchange(NyxObject([NyxField('mode', NyxData('status')),
        NyxField('job', LReply.Field('job'))]));

      if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) then
      begin
        Break;
      end;
      Sleep(5);
    until GetTickCount64 - LStarted > 180000;
    Result := DecodeNyxNativeSourceBuild(AArguments.Field('source').AsText,
      LStatus.Field('receipt'));
  end;

  procedure RefuseWire(const AData: TNyxDataValue; const AReason: TNyxText);
  var
    LRefused: Boolean;
  begin
    LRefused := False;
    try
      DecodeNyxNativeSourceBuild(GSource, AData);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, AReason);
  end;

begin
  LOriginalSource := GSource;
  LFirst := nil;
  LSecond := nil;
  LLock := nil;
  try
    LBefore := Observe;
    LArguments := Request('native-compile-only', False,
      LBefore.Field('session').Field('revision').AsInteger)
      .WithField('target', NyxData(NyxBuildTargetName(btNativeLCL)));
    Refuse(LArguments.WithField('target', NyxData('unknown-target')),
      'unknown constructor target refuses without changing the pair');
    Refuse(LArguments.WithField('target', NyxData(1)),
      'numeric constructor target refuses without changing the pair');
    LCompileOnly := WaitNative(LArguments);
    Check((LCompileOnly.Projection.State = spsExecuted) and
      (LCompileOnly.Projection.Target = btNativeLCL) and (LCompileOnly.Artifact = '') and
      (LCompileOnly.Projection.Design = ExpectedNyxProjectionDesign),
      'actual FPC helper/loop execution returns exact native meaning without an executable URL');
    Refuse(Complete(LCompileOnly), 'compile-only native execution has no publication authority');
    Check((Observe.Field('project').AsText = LBefore.Field('project').AsText) and
      (SessionMeaning(Observe) = SessionMeaning(LBefore)),
      'native execution alone leaves accepted files, pending buffer and history exact');

    LFirst := LCompileOnly.Projection.CopyDocument;
    LSecond := LCompileOnly.Projection.CopyDocument;
    LFirst.Title := 'Independent native receipt copy';
    Check((LSecond.Title = 'Handwritten notebook') and
      (LCompileOnly.Projection.Design = ExpectedNyxProjectionDesign),
      'native receipt copies own independent trees');
    FreeAndNil(LFirst);
    FreeAndNil(LSecond);
    LWire := EncodeNyxNativeSourceBuild(LCompileOnly);
    Check((LWire.Count = 7) and (LWire.Field('projection').Kind = ndObject) and
      (LWire.Field('diagnostics').Kind = ndArray) and
      (DecodeNyxNativeSourceBuild(GSource, LWire).Projection.Report.Source = GSource),
      'native receipt carries bounded construction/diagnostics without echoing source');
    RefuseWire(LWire.WithField('target', NyxData('browser')), 'native receipt rejects another target');
    RefuseWire(LWire.WithField('reference', NyxData('different-native-producer')),
      'native receipt rejects a substituted producer identity');
    RefuseWire(LWire.WithField('projection', NyxNull), 'native execution cannot omit its producer packet');
    RefuseWire(LWire.WithField('projection', LWire.Field('projection')
      .WithField('target', NyxData('browser'))), 'native receipt checks the inner producer target');
    RefuseWire(LWire.WithField('state', NyxData('compiled')), 'native receipt cannot claim browser compilation');
    RefuseWire(LWire.WithField('state', NyxData('compilation-failed')),
      'failed native receipt cannot retain an executed construction');
    RefuseWire(LWire.WithField('version', NyxData(1.5)), 'native receipt rejects fractional versions');
    RefuseWire(LWire.WithField('extra', NyxData(True)), 'native receipt rejects added fields');
    RefuseWire(NyxObject([
      NyxField('version', LWire.Field('version')),
      NyxField('reference', LWire.Field('reference')),
      NyxField('target', LWire.Field('target')),
      NyxField('state', LWire.Field('state')),
      NyxField('message', LWire.Field('message')),
      NyxField('projection', LWire.Field('projection')),
      NyxField('diagnostics|projection', NyxNull)]),
      'native receipt refuses delimiter-bearing replacement fields at the exact member count');

    LBuild := WaitNative(LArguments.WithField('operationId', NyxData('native-type-failure'))
      .WithField('source', NyxData(StringReplace(GSource, 'BuildNyxDocument',
        'MissingDocumentBuilder', [rfReplaceAll]))));
    Check((LBuild.Projection.State = spsCompilationFailed) and
      (LBuild.Projection.Design = '') and (LBuild.Projection.Report.Count > 0),
      'real FPC type failure returns native diagnostics without construction');
    Refuse(Complete(LBuild), 'failed native job cannot publish');

    { A distinct exact draft makes paired Apply/history observable even though
      the handwritten constructor intentionally reproduces the same design. }
    GSource := GSource + #10;
    LPair := DecodeNyxProject(LBefore.Field('project').AsText);
    LPair.Pending := True;
    LPair.Draft := GSource;
    LPair.DraftBase := LPair.Source;
    GEngine.EditorExchange(GToken, NyxObject([
      NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', LBefore.Field('session').Field('revision')),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', LBefore.Field('session').Field('selection')),
      NyxField('view', LBefore.Field('session').Field('view'))]));
    LBefore := Observe;
    LArguments := Request('native-shared-result', True,
      LBefore.Field('session').Field('revision').AsInteger)
      .WithField('target', NyxData(NyxBuildTargetName(btNativeLCL)));
    LAdmission := Exchange(LArguments);
    LBuild := WaitNative(LArguments);
    Check(Exchange(LArguments).ToJSON = LAdmission.ToJSON,
      'native compilation retry keeps its original job admission');
    Refuse(LArguments.WithField('target', NyxData('browser')),
      'retry identity cannot change constructor targets');
    LStale := WaitNative(LArguments.WithField('operationId', NyxData('native-stale-result')));
    Refuse(Complete(LBuild), 'wrong native completion capability preserves pair/history', 'not-an-editor');
    Refuse(Completion(LBuild, EncodeNyxNativeSourceBuild(LBuild).Field('projection')),
      'native completion refuses client construction even when its meaning matches');
    Refuse(Complete(LBuild).WithField('projection', NyxData('')),
      'native completion rejects explicit empty producer claims');
    LBeforeBytes := ReadBytes(ADirectories.SessionCheckpoint);
    LLock := TFileStream.Create(ADirectories.SessionCheckpoint, fmOpenRead or fmShareExclusive);
    try
      Refuse(Complete(LBuild), 'native durable failure preserves the entire precompile pair/history');
    finally
      FreeAndNil(LLock);
    end;
    Check(SameBytes(LBeforeBytes, ReadBytes(ADirectories.SessionCheckpoint)),
      'native durable refusal leaves exact checkpoint bytes');
    LReceipt := Exchange(Complete(LBuild));
    LDecoded := DecodeNyxSourcePublicationReceipt(LReceipt, GIssuer, GWorkspace, LBuild.Reference);
    LState := Observe;
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    Check((LReceipt.Count = 7) and (Length(LReceipt.ToJSON) < 1024) and
      (LDecoded.Revision = LBefore.Field('session').Field('revision').AsInteger + 1) and
      (LDecoded.Revision = LState.Field('session').Field('revision').AsInteger) and
      (LPair.Source = GSource) and (LPair.Design = ExpectedNyxProjectionDesign) and not LPair.Pending,
      'native completion publishes exact full source/design as one revision with a small acknowledgement');
    Refuse(Complete(LStale), 'another native precompile context becomes stale after publication');
    Check(Exchange(Complete(LBuild)).ToJSON = LReceipt.ToJSON,
      'native completion retry returns its original exact receipt');
    LState := GEngine.EditorExchange(GToken, NyxObject([
      NyxField('op', NyxData('history')), NyxField('expectedRevision', NyxData(LDecoded.Revision)),
      NyxField('direction', NyxData('undo'))]));
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    Check((LPair.Source = LOriginalSource) and (LPair.Design = ExpectedNyxProjectionDesign) and
      LState.Field('session').Field('canRedo').AsBoolean,
      'one paired Undo restores the previous source and full construction');
    Check(Exchange(Complete(LBuild)).ToJSON = LReceipt.ToJSON,
      'native completion replay cannot reapply after Undo');
    LState := GEngine.EditorExchange(GToken, NyxObject([
      NyxField('op', NyxData('history')),
      NyxField('expectedRevision', LState.Field('session').Field('revision')),
      NyxField('direction', NyxData('redo'))]));
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    Check((LPair.Source = GSource) and (LPair.Design = ExpectedNyxProjectionDesign),
      'paired Redo retains actual native construction and exact source');
  finally
    LLock.Free;
    LFirst.Free;
    LSecond.Free;
    GSource := LOriginalSource;
  end;
end;

var
  LDirectories: TNyxStudioDirectories;
  LTools: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LLocal: TNyxStudioSession;
  LClaim: TNyxDataValue;
  LBefore: TNyxDataValue;
  LState: TNyxDataValue;
  LCompileOnly: INyxSourceProjectionBuild;
  LOldCreators: INyxSourceProjectionBuild;
  LBuild: INyxSourceProjectionBuild;
  LStale: INyxSourceProjectionBuild;
  LPacket: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LDecoded: TNyxSourcePublicationReceipt;
  LPair: TNyxProjectPair;
  LBeforeBytes: TNyxBytes;
  LLock: TFileStream;
  LPrimary: TNyxDataValue;
begin
  GEngine := nil;
  LProfile := nil;
  LLocal := nil;
  LLock := nil;
  GWorkspace := NyxPrimaryWorkspace;
  try

    if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
    end;
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
    LTools := TNyxDataValue.ParseJSON(NyxDecodeUTF8(ReadBytes(ParamStr(2))));
    GSource := NyxDecodeUTF8(ReadBytes(LDirectories.SourceRoot +
      'tests/fixtures/nyx.projection.fixture.pas'));
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LLocal := TNyxStudioSession.Create;
    LLocal.SetSourceDraft(GSource);
    { Construction only: never Start a listener or enroll the production host. }
    GEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763, LProfile.Encode);
    LClaim := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LLocal.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LClaim.Field('token').AsText;
    GIssuer := LClaim.Field('state').Field('sourceObservationIssuer').AsText;
    Check(LClaim.Field('state').Field('sharedSourcePublication').AsBoolean,
      'private opt-in shared publication capability is advertised');
    LBefore := Observe;
    Refuse(Request('wrong-issuer', True, LBefore.Field('session').Field('revision').AsInteger)
      .WithField('issuer', NyxData('different-server')), 'wrong issuer cannot capture shared work');
    LCompileOnly := Compile('compile-only', False);
    Refuse(Completion(LCompileOnly, Producer(LCompileOnly)),
      'ordinary compiled-only jobs cannot gain publication authority');
    LOldCreators := Compile('old-creators', True);
    RegisterNyxSchema(NyxCustomKind('worker-publication-qualification'), [], []);
    Refuse(Completion(LOldCreators, Producer(LOldCreators)),
      'creator changes revoke the precompile snapshot');
    LBuild := Compile('shared-result', True);
    LStale := Compile('later-stale-result', True);
    LState := Observe;
    Check((LState.Field('project').AsText = LBefore.Field('project').AsText) and
      (SessionMeaning(LState) = SessionMeaning(LBefore)),
      'all compiler jobs leave accepted files, pending buffer and history unchanged');
    LPacket := Producer(LBuild);
    Refuse(Completion(LBuild, LPacket), 'wrong private capability preserves pair/history',
      'not-the-owning-editor');
    Refuse(Completion(LBuild, LPacket.WithField('reference', NyxData(LOldCreators.Reference.Name))),
      'substituted producer refuses');
    Refuse(Completion(LBuild, LPacket.WithField('target', NyxData('native-lcl'))),
      'substituted target refuses');
    Refuse(Completion(LBuild, LPacket.WithField('state', NyxData('compiled'))),
      'compiled envelope cannot publish construction');
    Refuse(Completion(LBuild, LPacket.WithField('design', NyxData('{}'))),
      'invalid design refuses before paired admission');
    LBeforeBytes := ReadBytes(LDirectories.SessionCheckpoint);
    LLock := TFileStream.Create(LDirectories.SessionCheckpoint, fmOpenRead or fmShareExclusive);
    try
      Refuse(Completion(LBuild, LPacket), 'durable replacement failure restores full session state');
    finally
      FreeAndNil(LLock);
    end;
    Check(SameBytes(LBeforeBytes, ReadBytes(LDirectories.SessionCheckpoint)),
      'durable failure preserves exact checkpoint bytes');
    LReceipt := Exchange(Completion(LBuild, LPacket));
    Check((LReceipt.Count = 7) and (Length(LReceipt.ToJSON) < 1024),
      'successful reply remains a small source-free acknowledgement');
    LDecoded := DecodeNyxSourcePublicationReceipt(LReceipt, GIssuer, GWorkspace, LBuild.Reference);
    LState := Observe;
    Check((LDecoded.Revision = LState.Field('session').Field('revision').AsInteger) and
      (LDecoded.Revision = LBefore.Field('session').Field('revision').AsInteger + 1),
      'successful completion advances exactly one captured revision');
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    Check((LPair.Source = GSource) and (LPair.Design = ExpectedNyxProjectionDesign) and not LPair.Pending,
      'delegated protocol publishes exact full Pascal and independently expected Unicode meaning');
    Check((LState.Field('session').Field('view').AsText = 'notebook-1') and
      (LState.Field('session').Field('selection').AsText = 'notebook-1'),
      'ordinary completion repairs replacement root and scoped selection');
    Check(Exchange(Completion(LBuild, LPacket)).ToJSON = LReceipt.ToJSON,
      'lost acknowledgement retry returns the unchanged original receipt');
    Refuse(Completion(LBuild, LPacket.WithField('message', NyxData('altered replay'))),
      'successful retry cannot substitute producer bytes');
    Refuse(Completion(LStale, Producer(LStale)), 'other precompile revision is stale after admission');
    ReceiptRefusal(LReceipt.WithField('issuer', NyxData('different-server')), LBuild.Reference,
      'acknowledgement validates server lifetime identity');
    ReceiptRefusal(LReceipt.WithField('workspace', NyxData('different-project')), LBuild.Reference,
      'acknowledgement validates immutable workspace');
    ReceiptRefusal(LReceipt.WithField('reference', NyxData(LStale.Reference.Name)), LBuild.Reference,
      'acknowledgement validates bound producer');
    ReceiptRefusal(LReceipt.WithField('extra', NyxData(True)), LBuild.Reference,
      'acknowledgement refuses extra fields');
    LState := GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', NyxData(LDecoded.Revision)),
      NyxField('direction', NyxData('undo'))]));
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    Check((LPair.Source = LLocal.Source) and not LPair.Pending and
      LState.Field('session').Field('canRedo').AsBoolean,
      'one ordinary paired Undo restores the earlier accepted files');
    Check(Exchange(Completion(LBuild, LPacket)).ToJSON = LReceipt.ToJSON,
      'exact completion replay does not reapply after a later Undo');
    LState := GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', LState.Field('session').Field('revision')),
      NyxField('direction', NyxData('redo'))]));
    Check(DecodeNyxProject(LState.Field('project').AsText).Design = ExpectedNyxProjectionDesign,
      'paired Redo retains delegated construction');
    LPrimary := Observe;
    GWorkspace := NyxWorkspace(GEngine.InvokeTool('nyx_workspaces', 'owned-publication-review', 'Scooty',
      NyxObject([NyxField('mode', NyxData('create')),
        NyxField('expectedRevision', LPrimary.Field('session').Field('revision')),
        NyxField('operationId', NyxData('independent-publication-project')),
        NyxField('label', NyxData('Independent publication project')),
        NyxField('base', NyxData('empty'))])).Field('workspace').AsText);
    Refuse(Completion(LBuild, LPacket), 'second project cannot consume primary producer');
    LState := Observe;
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    LPair.Pending := True;
    LPair.Draft := GSource;
    LPair.DraftBase := LPair.Source;
    GEngine.EditorExchange(GToken, NyxWithWorkspace(NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', LState.Field('session').Field('revision')),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', LState.Field('session').Field('selection')),
      NyxField('view', LState.Field('session').Field('view'))]), GWorkspace));
    LBuild := Compile('second-project-result', True);
    LReceipt := Exchange(Completion(LBuild, Producer(LBuild)));
    LDecoded := DecodeNyxSourcePublicationReceipt(LReceipt, GIssuer, GWorkspace, LBuild.Reference);
    Check((LDecoded.Workspace.ID = GWorkspace.ID) and
      (LReceipt.Count = 7) and (DecodeNyxProject(Observe.Field('project').AsText).Design = ExpectedNyxProjectionDesign),
      'second project publication has exactly one explicit workspace in its receipt');
    GWorkspace := NyxPrimaryWorkspace;
    Check(Observe.Field('project').AsText = LPrimary.Field('project').AsText,
      'second project leaves primary accepted files unchanged');
    NativeServiceChecks(LDirectories);
    FreeAndNil(GEngine);
    FreeAndNil(LLocal);
    FreeAndNil(LProfile);
    WriteLn('PASS ', GChecks, ' authenticated native/delegated publication/receipt/replay checks');
    WriteLn('Native construction is real FPC. Browser producer is simulated; HTTP/UI remain separate.');
  except
    on LException: Exception do
    begin
      LLock.Free;
      GEngine.Free;
      LLocal.Free;
      LProfile.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
