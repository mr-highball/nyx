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
program nyx_shared_source_publication_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.controls, nyx.codec, nyx.codegen,
  nyx.source, nyx.schema, nyx.studio.projects, nyx.studio.session, nyx.studio.mcp,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.buildexecutor,
  nyx.studio.builds, nyx.studio.workspaces, nyx.studio.sourceprojection,
  nyx.test.projection;

const
  CUnit: TNyxText = 'nyx.projection.fixture';
  CDraftSuffix: TNyxText = #10 + '{ unfinished work 🚀 }';

var
  GChecks: Integer;
  GEngine: TNyxStudioMCP;
  GToken: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Shared source publication: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Files are copied as bounded bytes. Source decoding never uses an ANSI RTL
  collection; the private recovery checkpoint remains an opaque byte sequence. }
function ReadBytes(const APath: TNyxText): TNyxBytes;
var
  LFile: TFileStream;
begin
  Result := nil;
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 16 * 1024 * 1024) then
    begin
      raise Exception.Create('Qualification file requires bounded nonempty bytes');
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
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
end;

{ Every refusal checks the exact authoritative pair, revision and history flags,
  rather than inferring safety merely from an exception being raised. }
procedure Refuse(const AToken: TNyxText; const ARequest: TNyxStudioSourcePublication;
  const AProjection: INyxSourceProjection; const ASelection, AView: TNyxControlRef;
  const AReason: TNyxText);
var
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LRefused: Boolean;
begin
  LBefore := Observe;
  LRefused := False;
  try
    GEngine.EditorCommitSourceProjection(AToken, ARequest, AProjection, ASelection, AView);
  except
    on LException: Exception do
    begin
      LRefused := True;
    end;
  end;
  LAfter := Observe;
  Check(LRefused and (LBefore.Field('project').AsText = LAfter.Field('project').AsText) and
    (LBefore.Field('session').ToJSON = LAfter.Field('session').ToJSON), AReason);
end;

procedure RefuseCapture(const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ASource, AReason: TNyxText);
var
  LBefore: TNyxDataValue;
  LRefused: Boolean;
begin
  LBefore := Observe;
  LRefused := False;
  try
    GEngine.EditorCaptureSourcePublication(GToken, AWorkspace, ARevision, ASource);
  except
    on LException: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (Observe.Field('project').AsText = LBefore.Field('project').AsText) and
    (Observe.Field('session').ToJSON = LBefore.Field('session').ToJSON), AReason);
end;

function CommitDraft(const APair: TNyxProjectPair; ARevision: Integer): TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('project', NyxData(EncodeNyxProject(APair))),
    NyxField('selection', NyxData('heading-1')), NyxField('view', NyxData('notebook-1'))]));
end;

function History(const ADirection: TNyxText; ARevision: Integer): TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('history')), NyxField('direction', NyxData(ADirection)),
    NyxField('expectedRevision', NyxData(ARevision))]));
end;

var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LExecutor: TNyxBuildExecutor;
  LLocal: TNyxStudioSession;
  LObserver: TNyxStudioSession;
  LMoved: TNyxStudioSession;
  LOldDocument: TNyxDocument;
  LOldRoot: INyxPage;
  LForeign: TNyxStudioMCP;
  LRequest: TNyxStudioSourcePublication;
  LForeignRequest: TNyxStudioSourcePublication;
  LBuild: INyxSourceProjectionBuild;
  LBrowser: INyxSourceProjectionBuild;
  LTools: TNyxDataValue;
  LState: TNyxDataValue;
  LClaim: TNyxDataValue;
  LSource: TNyxText;
  LExpected: TNyxText;
  LBaseline: TNyxProjectPair;
  LChanged: TNyxProjectPair;
  LPair: TNyxProjectPair;
  LFrame: TNyxSourceCheckpoint;
  LRevision: Integer;
  LRejected: Boolean;
  LBeforeBytes: TNyxBytes;
  LLock: TFileStream;
begin
  LProfile := nil;
  LExecutor := nil;
  LLocal := nil;
  LObserver := nil;
  LMoved := nil;
  LOldDocument := nil;
  LForeign := nil;
  LLock := nil;
  try

    if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
    end;
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
    LTools := TNyxDataValue.ParseJSON(NyxDecodeUTF8(ReadBytes(ParamStr(2))));
    LSource := NyxDecodeUTF8(ReadBytes(LDirectories.SourceRoot + 'tests/fixtures/' + CUnit + '.pas'));
    LExpected := ExpectedNyxProjectionDesign;
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LLocal := TNyxStudioSession.Create;
    LLocal.SetSourceDraft(LSource);
    LBaseline := LLocal.ProjectSnapshot;
    { Construct only; no Start, HTTP listener or production enrollment is used. }
    GEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763, LProfile.Encode);
    LClaim := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LBaseline))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LClaim.Field('token').AsText;
    LRevision := LClaim.Field('state').Field('session').Field('revision').AsInteger;
    LRequest := GEngine.EditorCaptureSourcePublication(GToken,
      NyxPrimaryWorkspace, LRevision, LSource);
    Check((LRequest.Source = LSource) and (LRequest.Workspace.ID = NyxPrimaryWorkspace.ID),
      'opaque publication context is captured before actual compilation');
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LBuild := LExecutor.ProjectSource(LRequest.Source, NyxPascalUnit(CUnit), btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = LExpected), 'real FPC helpers/loop reproduce independent meaning');
    Check(Pos('0 unfreed memory blocks', LBuild.RuntimeLog) > 0,
      'actual native constructor child retires with no reported heap leaks');
    LBrowser := LExecutor.ProjectSource(LRequest.Source, NyxPascalUnit(CUnit), btBrowser);
    Check(LBrowser.Projection.State = spsCompiled, 'actual browser receipt is compiled only');
    FreeAndNil(LExecutor);
    Refuse('wrong-private-authority', LRequest,
      LBuild.Projection, NyxControl('heading-1'), NyxControl('notebook-1'), 'wrong authority retains full pending pair');
    Refuse(GToken, Default(TNyxStudioSourcePublication), LBuild.Projection,
      NyxControl('heading-1'), NyxControl('notebook-1'), 'unissued publication context cannot be forged');
    RefuseCapture(NyxWorkspace('missing-owned-project'), LRevision, LSource, 'missing workspace never falls back');
    RefuseCapture(NyxPrimaryWorkspace, LRevision + 1, LSource, 'stale capture retains pair/history');
    RefuseCapture(NyxPrimaryWorkspace, LRevision, LSource + CDraftSuffix,
      'capture cannot substitute a different unfinished unit');
    LForeign := TNyxStudioMCP.Create(LDirectories
      .RunningIn(LDirectories.RuntimeRoot + 'foreign')
      .EnrollingProject(LDirectories.RuntimeRoot + 'foreign'), 8762, 8763, LProfile.Encode);
    LClaim := LForeign.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LBaseline))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LForeignRequest := LForeign.EditorCaptureSourcePublication(LClaim.Field('token').AsText,
      NyxPrimaryWorkspace, LClaim.Field('state').Field('session').Field('revision').AsInteger, LSource);
    Refuse(GToken, LForeignRequest, LBuild.Projection, NyxControl('heading-1'),
      NyxControl('notebook-1'), 'identical project/revision/source cannot reuse another issuer context');
    FreeAndNil(LForeign);
    Refuse(GToken, LRequest,
      nil, NyxControl('heading-1'), NyxControl('notebook-1'), 'nil completion cannot publish');
    Refuse(GToken, LRequest,
      LBrowser.Projection, NyxControl('heading-1'), NyxControl('notebook-1'), 'actual compiled browser receipt cannot publish');
    Refuse(GToken, LRequest,
      LBuild.Projection, NyxControl('missing-heading'), NyxControl('notebook-1'), 'invalid selection refuses before publication');
    Refuse(GToken, LRequest,
      LBuild.Projection, NyxControl('heading-1'), NyxControl('heading-1'), 'descendant cannot become active view');
    LState := GEngine.EditorCommitSourceProjection(GToken, LRequest,
      LBuild.Projection, NyxControl('heading-1'), NyxControl('notebook-1'));
    LRevision := LState.Field('session').Field('revision').AsInteger;
    Refuse(GToken, LRequest, LBuild.Projection, NyxControl('heading-1'),
      NyxControl('notebook-1'), 'successful publication makes its captured context stale');
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    Check((LPair.Design = LExpected) and (LPair.Source = LSource) and not LPair.Pending,
      'trusted backend publishes exact executed files and consumes their pending buffer');
    Check(LState.Field('session').Field('canUndo').AsBoolean,
      'backend publication records ordinary paired history');
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision, LPair, LFrame);
    Check((LFrame.Origin = nsoExecuted) and (LFrame.Source = LSource),
      'authorized observer receives opaque live execution origin/exact source');
    { The incoming real constructor turns the former root identity into a
      descendant and moves its selection to another page. Both existed before
      and after, so an existence-only fallback would leave an unusable view. }
    LOldDocument := TNyxDocument.Create;
    LOldRoot := NewNyxPage('heading-1', ncoDescriptor);
    LOldRoot.Add(NewNyxLabel('notebook-2', ncoDescriptor));
    LOldDocument.AddPage(LOldRoot);
    LOldRoot := nil;
    LMoved := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LOldDocument),
      TNyxCodegen.Generate(LOldDocument)));
    FreeAndNil(LOldDocument);
    LMoved.Select('notebook-2');
    LMoved.AdoptCapturedProject(LPair, LFrame);
    Check((LMoved.ActiveViewID = 'notebook-1') and (LMoved.SelectedID = 'notebook-1'),
      'observer repairs a moved root and selection against the actual incoming view');
    FreeAndNil(LMoved);
    LObserver := TNyxStudioSession.Create(LBaseline);
    LRejected := False;
    try
      LObserver.AdoptProject(LPair);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LObserver.Save = LBaseline.Design) and
      (LObserver.Source = LBaseline.Source) and (LObserver.DraftSource = LSource),
      'ordinary project strings cannot substitute trusted execution admission');
    LObserver.AdoptCapturedProject(LPair, LFrame);
    Check((LObserver.Source = LSource) and (LObserver.Save = LExpected),
      'independent observer adopts helpers/loop without literal replay');
    LObserver.Undo;
    Check((LObserver.Save = LBaseline.Design) and (LObserver.Source = LBaseline.Source) and
      not LObserver.SourceDraftPending, 'observer Undo restores previous pair after consuming exact draft');
    LObserver.Redo;
    Check((LObserver.Source = LSource) and (LObserver.Save = LExpected) and
      (LObserver.AcceptedSourceCheckpoint.Origin = nsoExecuted), 'observer Redo retains execution origin/exact full unit');
    LChanged := LPair;
    LChanged.Pending := True;
    LChanged.Draft := LSource + CDraftSuffix;
    LChanged.DraftBase := LSource;
    LState := CommitDraft(LChanged, LRevision);
    LRevision := LState.Field('session').Field('revision').AsInteger;
    Check(LState.Field('project').AsText = EncodeNyxProject(LChanged),
      'ordinary shared draft commit reuses only its exact live accepted files');
    LRequest := GEngine.EditorCaptureSourcePublication(GToken,
      NyxPrimaryWorkspace, LRevision, LChanged.Draft);
    Refuse(GToken, LRequest, LBuild.Projection, NyxControl('heading-1'),
      NyxControl('notebook-1'), 'completion cannot substitute the newly captured unfinished text');
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision, LPair, LFrame);
    LObserver.AdoptCapturedProject(LPair, LFrame);
    Check((LObserver.DraftSource = LChanged.Draft) and
      (LObserver.ProjectSnapshot.DraftBase = LSource), 'observer retains exact supplementary Unicode draft/base');
    LObserver.SetSourceDraft('Independent observer notes');
    Check(Observe.Field('project').AsText = EncodeNyxProject(LChanged),
      'observer buffers own no reference back to the shared session');
    LChanged.Source := LChanged.Source + #10;
    LRejected := False;
    try
      LObserver.AdoptCapturedProject(LChanged, LFrame);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LObserver.DraftSource = 'Independent observer notes'),
      'substituted source refuses without replacing observer buffers');
    LRejected := False;
    try
      LObserver.AdoptProjectedProject(LChanged, LBuild.Projection);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LObserver.DraftSource = 'Independent observer notes'),
      'project files cannot substitute the exact executed source proof');
    LChanged := NyxProjectPair(LBaseline.Design, LSource);
    LRejected := False;
    try
      LObserver.AdoptProjectedProject(LChanged, LBuild.Projection);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LObserver.DraftSource = 'Independent observer notes'),
      'project files cannot substitute executed design meaning');
    LState := History('undo', LRevision);
    LRevision := LState.Field('session').Field('revision').AsInteger;
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    Check((LPair.Source = LBaseline.Source) and (LPair.Design = LBaseline.Design) and
      not LPair.Pending, 'one backend Undo restores original accepted files');
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision, LPair, LFrame);
    Check(LFrame.Origin = nsoDeclarative, 'backend Undo restores declarative source origin');
    LState := History('redo', LRevision);
    LRevision := LState.Field('session').Field('revision').AsInteger;
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision, LPair, LFrame);
    Check((LFrame.Origin = nsoExecuted) and LPair.Pending and (LPair.Source = LSource),
      'backend Redo restores executed files with the latest exact unfinished buffer');
    LPair.Pending := False;
    LPair.Draft := '';
    LPair.DraftBase := '';
    LState := CommitDraft(LPair, LRevision);
    LRevision := LState.Field('session').Field('revision').AsInteger;
    LRequest := GEngine.EditorCaptureSourcePublication(GToken,
      NyxPrimaryWorkspace, LRevision, LSource);
    RegisterNyxSchema(NyxCustomKind('source-publication-qualification'), [], []);
    Refuse(GToken, LRequest, LBuild.Projection, NyxControl('heading-2'),
      NyxControl('notebook-2'), 'creator registration revokes a previously captured publication');
    LRequest := GEngine.EditorCaptureSourcePublication(GToken,
      NyxPrimaryWorkspace, LRevision, LSource);
    LBeforeBytes := ReadBytes(LDirectories.SessionCheckpoint);
    { Hold the exact owned committed file against replacement. The failed save
      must roll back already staged owners, selection/view, revision and history. }
    LLock := TFileStream.Create(LDirectories.SessionCheckpoint, fmOpenRead or fmShareExclusive);
    try
      Refuse(GToken, LRequest, LBuild.Projection,
        NyxControl('heading-2'), NyxControl('notebook-2'), 'failed durable replacement rolls back full backend state');
    finally
      FreeAndNil(LLock);
    end;
    Check(SameBytes(LBeforeBytes, ReadBytes(LDirectories.SessionCheckpoint)),
      'failed publication retains exact previous checkpoint bytes');
    LRejected := False;
    try
      GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision - 1, LPair, LFrame);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'observer cannot capture a stale revision');
    FreeAndNil(GEngine);
    FreeAndNil(LObserver);
    FreeAndNil(LLocal);
    FreeAndNil(LProfile);
    WriteLn('PASS ', GChecks, ' trusted shared source publication/observation/history checks');
  except
    on LException: Exception do
    begin
      LLock.Free;
      LForeign.Free;
      LMoved.Free;
      LOldDocument.Free;
      LExecutor.Free;
      GEngine.Free;
      LObserver.Free;
      LLocal.Free;
      LProfile.Free;
      WriteLn('FAIL ', LException.Message);
      Halt(1);
    end;
  end;
end.
