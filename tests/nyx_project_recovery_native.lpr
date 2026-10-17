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
program nyx_project_recovery_native;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec, nyx.editing,
  nyx.codegen, nyx.source, nyx.pascal.project, nyx.studio.projects,
  nyx.studio.history, nyx.studio.session, nyx.studio.agents, nyx.studio.workspaces,
  nyx.studio.recovery, nyx.studio.recovery.native, nyx.studio.directories,
  nyx.studio.outputs, nyx.studio.buildexecutor, nyx.studio.builds,
  nyx.studio.sourceprojection, nyx.studio.projectinputs, nyx.studio.mcp,
  nyx.studio.sourcebuilds, nyx.test.compiled.runtime, nyx.test.pascal.project,
  nyx.pascal.drafts, nyx.test.pascal.drafts, nyx.studio.sourcejobs,
  nyx.studio.sourcecompilation.native;

type
  { Delegates every result to the actual native producer. Counters distinguish
    complete project inputs from main-only verification; cancellation after one
    real construction tests atomic whole-registry publication. }
  TCountedVerifier = class(TInterfacedObject, INyxRuntimeSourceVerifier,
    INyxRuntimeProjectVerifier)
  public
    Inner: INyxRuntimeSourceVerifier;
    Cancellation: INyxBuildCancellation;
    Calls: Integer;
    CancelAfter: Integer;
    procedure RequireActive;
    function Verify(const ASource: TNyxText): INyxSourceProjection;
    function VerifyProject(const AProject: INyxPascalProject): INyxSourceProjection;
  end;

  { Source-only compatibility probe deliberately provides no project capability.
    Its counter must stay zero when recovery refuses supporting-file input. }
  TLegacyVerifier = class(TInterfacedObject, INyxRuntimeSourceVerifier)
  public
    Calls: Integer;
    procedure RequireActive;
    function Verify(const ASource: TNyxText): INyxSourceProjection;
  end;

const
  CFirst: TNyxText = 'An independent first plan';
  CSecond: TNyxText = 'A second plan 🚀 𐐷';
  CDraft: TNyxText = '{ An unfinished main idea 🚀';
  CBase: TNyxText = '{ Its earlier independent base 𐐷 }';
  COwner: TNyxText = 'owned-whole-project-qualification';
  CActor: TNyxText = 'Whole project qualification';

var
  GChecks: Integer;
  GFileDrafts: Boolean;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

function FileBytes(const APath: TNyxText): TNyxBytes;
var
  LStream: TFileStream;
begin
  Result := nil;
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[0], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

function SameBytes(const ALeft, ARight: TNyxBytes): Boolean;
begin
  Result := Length(ALeft) = Length(ARight);

  if Result and (Length(ALeft) > 0) then
  begin
    Result := CompareByte(ALeft[0], ARight[0], Length(ALeft)) = 0;
  end;
end;

procedure TCountedVerifier.RequireActive;
begin
  Inner.RequireActive;
end;

function TCountedVerifier.Verify(const ASource: TNyxText): INyxSourceProjection;
begin
  Inc(Calls);
  Result := Inner.Verify(ASource);
end;

function TCountedVerifier.VerifyProject(
  const AProject: INyxPascalProject): INyxSourceProjection;
var
  LVerifier: INyxRuntimeProjectVerifier;
begin
  Check(Supports(Inner, INyxRuntimeProjectVerifier, LVerifier),
    'actual native verifier exposes its optional project capability');
  Inc(Calls);
  Result := LVerifier.VerifyProject(AProject);

  if (CancelAfter > 0) and (Calls = CancelAfter) then
  begin
    Cancellation.Cancel;
  end;
end;

procedure TLegacyVerifier.RequireActive;
begin
end;

function TLegacyVerifier.Verify(const ASource: TNyxText): INyxSourceProjection;
begin
  Inc(Calls);
  Result := nil;
end;

function Pair(const ACheckpoint: TNyxSourceCheckpoint): TNyxProjectPair;
var
  LInputs: INyxPascalProject;
  LFile: TNyxPascalSourceRef;
begin
  Result := NyxProjectPair(ACheckpoint.Design, ACheckpoint.Source);
  Result.Files := ACheckpoint.Files;
  Result.Pending := True;
  Result.Draft := CDraft;
  Result.DraftBase := CBase;

  if GFileDrafts then
  begin
    LInputs := ACheckpoint.Files.Open(ACheckpoint.Source);
    LFile := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('project.values'));
    Result.FileDrafts := Result.FileDrafts.Editing(LFile, LFile.Text(LInputs),
      '{ An unfinished unit 🚀' + #0);
    LFile := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('plan-values.inc'));
    Result.FileDrafts := Result.FileDrafts.Editing(LFile, LFile.Text(LInputs), '');
  end;
end;

function Checkpoint(const AProjection: INyxSourceProjection): TNyxSourceCheckpoint;
var
  LProject: INyxPascalProjectProjection;
  LInput: TNyxProjectPair;
  LResolved: TNyxProjectPair;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
begin
  Check(Supports(AProjection, INyxPascalProjectProjection, LProject),
    'actual producer retains its exact project inventory');
  LInput := NyxProjectPair(AProjection.Design, AProjection.Source);
  LInput.Files := LProject.Files;
  AdmitNyxProjectedProject(LInput, AProjection, LDocument, LWorkspace, LResolved);
  try
    Result := LWorkspace.Capture;
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

procedure SemanticImports(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const AVerifier: INyxRuntimeSourceVerifier;
  const AFirst, ASecond: TNyxSourceCheckpoint);
var
  LEngine: TNyxStudioMCP;
  LLocal: TNyxStudioSession;
  LToken: TNyxText;
  LSerial: Integer;
  LInput: TNyxProjectPair;
  LBefore: TNyxText;
  LFirstResolved: TNyxText;
  LImport: TNyxText;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LIndex: Integer;

  function Observe: TNyxDataValue;
  begin
    Result := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
  end;

  function Arguments(const AMode: TNyxText;
    const AExtra: array of TNyxDataField): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LField: Integer;
  begin
    Inc(LSerial);
    SetLength(LFields, Length(AExtra) + 3);
    LFields[0] := NyxField('mode', NyxData(AMode));
    LFields[1] := NyxField('expectedRevision',
      Observe.Field('session').Field('revision'));
    LFields[2] := NyxField('operationId', NyxData('owned-project-' + IntToStr(LSerial)));
    for LField := 0 to High(AExtra) do
    begin
      LFields[LField + 3] := AExtra[LField];
    end;
    Result := NyxObject(LFields);
  end;

  function Call(const AArguments: TNyxDataValue): TNyxDataValue;
  begin
    Result := LEngine.InvokeTool('nyx_project', COwner, CActor, AArguments);
  end;

  function Upload(const APair: TNyxProjectPair): TNyxText;
  var
    LPacket: TNyxText;
    LReply: TNyxDataValue;
    LOffset: Integer;
    LPosition: Integer;
    LStart: Integer;
    LCount: Integer;
    LScalar: Integer;
  begin
    LPacket := EncodeNyxProject(APair);
    LReply := Call(Arguments('begin-import', [
      NyxField('bytes', NyxData(NyxUTF8ByteCount(LPacket)))]));
    Result := LReply.Field('projectImport').Field('import').AsText;
    LOffset := 0;
    LPosition := 1;
    repeat
      LStart := LPosition;
      LCount := 0;
      while (LPosition <= Length(LPacket)) and (LCount < 4096) do
      begin

        if not NyxNextScalar(LPacket, LPosition, LScalar) then
        begin
          raise Exception.Create('Upload must retain exact scalar text');
        end;
        Inc(LCount);
      end;
      LReply := Call(Arguments('append-import', [
        NyxField('import', NyxData(Result)), NyxField('offset', NyxData(LOffset)),
        NyxField('text', NyxData(Copy(LPacket, LStart, LPosition - LStart)))]));
      LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
    until LOffset = NyxTextScalarCount(LPacket);
    Check(LReply.Field('projectImport').Field('complete').AsBoolean,
      'semantic upload owns the complete exact project packet');
  end;

  function WaitReview(const AJob: TNyxText): TNyxDataValue;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Result := LEngine.InvokeBuild(COwner, CActor, NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', NyxData(AJob))]));

      if Result.Field('publication').Field('state').AsText <> 'pending' then
      begin
        Exit;
      end;
      Sleep(5);
    until GetTickCount64 - LStarted > 180000;
    raise Exception.Create('Owned project review exceeded its compiler wait budget');
  end;

  function History(const ADirection: TNyxText): TNyxDataValue;
  begin
    Result := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('history')), NyxField('direction', NyxData(ADirection)),
      NyxField('expectedRevision', Observe.Field('session').Field('revision'))]));
  end;

  procedure ResolveDraft;
  var
    LPair: TNyxProjectPair;
  begin
    LPair := DecodeNyxProject(Observe.Field('project').AsText);
    Check(LPair.Pending, 'first semantic import retains its unfinished draft until explicit resolution');
    LPair.Pending := False;
    LPair.Draft := '';
    LPair.DraftBase := '';
    LPair.FileDrafts := Default(TNyxPascalFileDrafts);
    LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', Observe.Field('session').Field('revision')),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('compose')), NyxField('view', NyxData('compose'))]));
    LFirstResolved := Observe.Field('project').AsText;
    Check(not DecodeNyxProject(LFirstResolved).Pending and
      (DecodeNyxProject(LFirstResolved).Files.Encoded = AFirst.Files.Encoded),
      'ordinary draft-only synchronization preserves every accepted helper byte');
  end;

begin
  LSerial := 0;
  LEngine := TNyxStudioMCP.Create(ADirectories, 8676, 8677, AProfile);
  LLocal := TNyxStudioSession.Create;
  try
    LReceipt := LEngine.ConnectEditor(NyxObject([
      NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LLocal.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LToken := LReceipt.Field('token').AsText;
    for LIndex := 0 to 1 do
    begin
      LInput := Pair(AFirst);

      if LIndex = 1 then
      begin
        ResolveDraft;
        LInput := Pair(ASecond);
      end;
      LBefore := Observe.Field('project').AsText;
      LImport := Upload(LInput);
      LRequest := Arguments('review-import', [
        NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match')),
        NyxField('admission', NyxData('compiler'))]);
      LReceipt := Call(LRequest);
      Check(Call(LRequest).ToJSON = LReceipt.ToJSON, 'exact review retry retains one compiler job');
      LStatus := WaitReview(LReceipt.Field('job').AsText);
      Check((LStatus.Field('state').AsText = 'succeeded') and
        (LStatus.Field('publication').Field('state').AsText = 'reviewed'),
        'actual semantic import compiles the incoming helpers independently of destination');
      Check(Observe.Field('project').AsText = LBefore, 'review has no adoption authority');
      LRequest := Arguments('apply', [
        NyxField('import', NyxData(LImport)),
        NyxField('reviewID', LStatus.Field('projectImport').Field('reviewID'))]);
      LReceipt := Call(LRequest);
      Check(Call(LRequest).ToJSON = LReceipt.ToJSON, 'exact adoption retry adds no history command');
      Check(Observe.Field('project').AsText = EncodeNyxProject(LInput),
        'semantic adoption retains the complete input and its independent draft');
      Check(History('undo').Field('project').AsText = LBefore,
        'one semantic Undo restores the complete previous helper context');
      Check(History('redo').Field('project').AsText = EncodeNyxProject(LInput),
        'one semantic Redo restores the incoming source/helpers/draft together');
    end;
    FreeAndNil(LEngine);
    LEngine := TNyxStudioMCP.Create(ADirectories, 8676, 8677, AProfile,
      AVerifier, rrmImmediate);
    LReceipt := LEngine.ConnectEditor(NyxObject([
      NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(Pair(ASecond)))),
      NyxField('selection', NyxData('compose')), NyxField('view', NyxData('compose'))]));
    LToken := LReceipt.Field('token').AsText;
    Check(Observe.Field('project').AsText = EncodeNyxProject(Pair(ASecond)),
      'actual cold shared recovery retains semantic-imported helper meaning');
    Check(History('undo').Field('project').AsText = LFirstResolved,
      'cold recovered semantic history preserves equal-main/different-helper inputs');
  finally
    LLocal.Free;
    LEngine.Free;
  end;
end;

{ Production deferred protocol compiles an exact saved input without a listener
  or synthetic browser execution. Its malformed producer must never admit data. }
procedure BrowserRecovery(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const AFirst: TNyxSourceCheckpoint);
var
  LEngine: TNyxStudioMCP;
  LConnection: TNyxDataValue;
  LUnit: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LToken: TNyxText;
  LJob: TNyxText;
  LFiles: TNyxPascalProjectFiles;
  LBuild: INyxSourceProjectionBuild;
  LProject: INyxPascalProjectBuild;
  LStarted: QWord;
  LRefused: Boolean;
  LBefore: TNyxBytes;
begin
  LBefore := FileBytes(ADirectories.SessionCheckpoint);
  LEngine := TNyxStudioMCP.Create(ADirectories, 8674, 8675, AProfile,
    nil, rrmBrowserWorker);
  try
    LConnection := LEngine.ConnectRecovery;
    LToken := LConnection.Field('token').AsText;
    Check(LConnection.Field('recovery').Field('units').AsInteger = 2,
      'deferred backend retains equal-main/different-helper input identities');
    LUnit := LEngine.RecoveryExchange(LToken, NyxObject([
      NyxField('mode', NyxData('unit')), NyxField('unit', NyxData(0))]));
    LFiles := ReadNyxSourceProjectFiles(LUnit);
    Check((LUnit.Field('source').AsText = AFirst.Source) and
      (LFiles.Encoded = AFirst.Files.Encoded),
      'private unit response returns only its captured main/helper context');
    LReceipt := LEngine.RecoveryExchange(LToken, NyxObject([
      NyxField('mode', NyxData('request')), NyxField('unit', NyxData(0)),
      NyxField('operationId', NyxData('whole-project-browser-compile'))]));
    LJob := LReceipt.Field('job').AsText;
    LStarted := GetTickCount64;
    repeat
      LStatus := LEngine.RecoveryExchange(LToken, NyxObject([
        NyxField('mode', NyxData('job')), NyxField('job', NyxData(LJob))]));

      if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) then
      begin
        Break;
      end;
      Sleep(5);
    until GetTickCount64 - LStarted > 180000;
    Check(LStatus.Field('state').AsText = 'succeeded',
      'actual deferred pas2js job compiles the retained helper inventory');
    LBuild := DecodeNyxBrowserSourceBuild(AFirst.Source, LStatus.Field('receipt'), LFiles);
    Check((LBuild.Projection.State = spsCompiled) and
      Supports(LBuild, INyxPascalProjectBuild, LProject) and
      (TNyxPascalProjectFiles.Capture(LProject.Inputs).Encoded = LFiles.Encoded),
      'browser receipt retains input data without claiming execution');
    LRefused := False;
    try
      LEngine.RecoveryExchange(LToken, NyxObject([
        NyxField('mode', NyxData('complete')), NyxField('unit', NyxData(0)),
        NyxField('job', NyxData(LJob)), NyxField('producer', NyxData('{}'))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    LStatus := LEngine.RecoveryExchange(LToken, NyxObject([
      NyxField('mode', NyxData('status'))]));
    Check(LRefused and (LStatus.Field('accepted').AsInteger = 0),
      'compiled helpers and malformed worker data cannot advance recovery');
    LEngine.RecoveryExchange(LToken, NyxObject([NyxField('mode', NyxData('cancel'))]));
    Check(SameBytes(LBefore, FileBytes(ADirectories.SessionCheckpoint)),
      'cancelled deferred project recovery preserves the entire original registry');
  finally
    LEngine.Free;
  end;
end;

{ Ordinary session/command ownership, using an actual previously constructed
  project. Pending helper text is deliberately invalid Pascal: main compilation
  must consume accepted inputs and retain these separate buffers verbatim. }
procedure SessionDraftChecks(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const AProjection: INyxSourceProjection);
const
  CNewerHelper: TNyxText = '{ A newer helper idea 𐐷';
var
  LSession: TNyxStudioSession;
  LCopy: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LPair: TNyxProjectPair;
  LUnit: TNyxPascalSourceRef;
  LInclude: TNyxPascalSourceRef;
  LAcceptedUnit: TNyxText;
  LOriginalSource: TNyxText;
  LStarted: QWord;
begin
  LSession := TNyxStudioSession.Create;
  LCopy := nil;
  LCommands := nil;
  try
    LPair := Pair(Checkpoint(AProjection));
    LPair.Pending := False;
    LPair.Draft := '';
    LPair.DraftBase := '';
    LPair.FileDrafts := Default(TNyxPascalFileDrafts);
    LSession.AdoptProjectedProject(LPair, AProjection);
    LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('project.values'));
    LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('plan-values.inc'));
    LAcceptedUnit := LSession.SourceFileText(LUnit);
    LOriginalSource := LSession.Source;
    LSession.SetFileDraft(TNyxPascalSourceRef.Main, CDraft);
    LSession.SetFileDraft(LUnit, '{ An unfinished unit 🚀' + #0);
    LSession.SetFileDraft(LInclude, '');
    Check(LSession.SourceDraftsPending and (LSession.FileDrafts.Count = 2),
      'ordinary session holds main and two helper buffers independently');
    Check((LSession.DraftFileText(TNyxPascalSourceRef.Main) = CDraft) and
      (LSession.DraftFileText(LInclude) = '') and
      (LSession.SourceFileText(LUnit) = LAcceptedUnit),
      'editable helper/main text never replaces accepted compiler input');
    LCopy := LSession.Clone;
    LCopy.SetFileDraft(LUnit, '{ A clone-only edit }');
    Check(LSession.DraftFileText(LUnit) <> LCopy.DraftFileText(LUnit),
      'rollback/session clone retains independent immutable helper buffers');
    FreeAndNil(LCopy);
    LSession.DiscardFileDraft(TNyxPascalSourceRef.Main);
    Check(not LSession.SourceDraftPending and (LSession.FileDrafts.Count = 2),
      'discarding main text does not discard other file buffers');
    LSession.SetSourceDraft(LSession.Source + #10 + '{ A compiled main note. }' + #10);
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCommands.UseCompiler(NewNyxNativeSourceCompiler(ADirectories, AProfile,
      TNyxCompilerLimits.Default));
    LCommands.Apply;
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize(1);

      if not LCommands.Busy then
      begin
        Break;
      end;
    until GetTickCount64 - LStarted > 180000;
    Check(LCommands.State = nssApplied, 'actual main compiler ignores unfinished helper buffers');
    Check((LSession.Source <> LOriginalSource) and (LSession.FileDrafts.Count = 2) and
      (LSession.SourceFileText(LUnit) = LAcceptedUnit),
      'main publication retains accepted helpers and every independent helper buffer');
    LSession.Undo;
    Check((LSession.Source = LOriginalSource) and LSession.CanRedo and
      (LSession.FileDrafts.Count = 2), 'one Undo restores accepted main and independent file buffers');
    LSession.SetFileDraft(LUnit, CNewerHelper);
    Check(LSession.CanRedo, 'ordinary helper typing preserves existing Redo');
    LSession.Redo;
    LSession.Undo;
    Check(LSession.DraftFileText(LUnit) = CNewerHelper,
      'opposite history captures newer helper text before traversal');
    LSession.DiscardFileDraft(LInclude);
    Check((LSession.FileDrafts.Count = 1) and LSession.FileDrafts.Contains(LUnit),
      'explicit helper discard affects only that selected file');
  finally
    FreeAndNil(LCommands);
    LCopy.Free;
    LSession.Free;
    { Retire any detached queued delivery on a real following UI turn. }
    CheckSynchronize(1);
  end;
end;

procedure Run;
var
  LDirectories: TNyxStudioDirectories;
  LTools: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LVerifier: INyxRuntimeSourceVerifier;
  LProjectVerifier: INyxRuntimeProjectVerifier;
  LCounted: INyxRuntimeSourceVerifier;
  LCounter: TCountedVerifier;
  LLegacy: TLegacyVerifier;
  LCancellation: INyxBuildCancellation;
  LProjectFirst: INyxPascalProject;
  LProjectSecond: INyxPascalProject;
  LProjectionFirst: INyxSourceProjection;
  LProjectionSecond: INyxSourceProjection;
  LFirst: TNyxSourceCheckpoint;
  LSecond: TNyxSourceCheckpoint;
  LSource: TNyxText;
  LUnused: TNyxText;
  LFirstDesign: TNyxText;
  LSecondDesign: TNyxText;
  LDocument: TNyxDocument;
  LFrame: TNyxAgentRecoveryFrame;
  LRegistry: TNyxWorkspaceRecoveryFrame;
  LPrimary: TNyxAgentSession;
  LWorkspaces: TNyxStudioWorkspaces;
  LRecovered: TNyxAgentSession;
  LRecoveredWorkspaces: TNyxStudioWorkspaces;
  LStore: TNyxStudioRuntimeStore;
  LBefore: TNyxBytes;
  LRefused: Boolean;
  LSemanticDirectories: TNyxStudioDirectories;
begin

  GFileDrafts := (ParamCount = 4) and (ParamStr(4) = '--file-drafts');

  if ((ParamCount <> 3) and not GFileDrafts) or
    DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LProfile := TNyxOutputConfiguration.Create;
  LPrimary := nil;
  LWorkspaces := nil;
  LRecovered := nil;
  LRecoveredWorkspaces := nil;
  LStore := nil;
  try
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LVerifier := NewNyxNativeRuntimeSourceVerifier(LDirectories, LProfile.Encode,
      TNyxCompilerLimits.Default, nil, spcChecked);
    Check(Supports(LVerifier, INyxRuntimeProjectVerifier, LProjectVerifier),
      'native recovery offers complete captured-project verification');
    LDocument := CompiledRuntimeDocument(CFirst, LSource);
    try
      LFirstDesign := TNyxCodec.Encode(LDocument);
    finally
      LDocument.Free;
    end;
    LDocument := CompiledRuntimeDocument(CSecond, LUnused);
    try
      LSecondDesign := TNyxCodec.Encode(LDocument);
    finally
      LDocument.Free;
    end;
    LProjectFirst := CompiledRuntimeProject(LSource, CFirst);
    LProjectSecond := LProjectFirst.WithInclude(NyxPascalInclude('plan-values.inc'),
      'function ProjectPlan: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := ' + TNyxCodegen.TextLiteral(CSecond) + ';' + #10 + 'end;' + #10);
    Check(LProjectFirst.Source = LProjectSecond.Source,
      'qualification has exact equal main text and different supporting meaning');
    LProjectionFirst := LProjectVerifier.VerifyProject(LProjectFirst);
    LProjectionSecond := LProjectVerifier.VerifyProject(LProjectSecond);
    Check((LProjectionFirst.State = spsExecuted) and
      (LProjectionFirst.Design = LFirstDesign), 'first actual construction matches independent meaning');
    Check((LProjectionSecond.State = spsExecuted) and
      (LProjectionSecond.Design = LSecondDesign) and (LFirstDesign <> LSecondDesign),
      'changed helper construction matches independently generated second meaning');
    LFirst := Checkpoint(LProjectionFirst);
    LSecond := Checkpoint(LProjectionSecond);

    if GFileDrafts then
    begin
      SessionDraftChecks(LDirectories, LProfile.Encode, LProjectionFirst);
    end;
    LFrame := Default(TNyxAgentRecoveryFrame);
    LFrame.Revision := 17;
    LFrame.Permission := apEdit;
    LFrame.Claimed := True;
    LFrame.Session.Pair := Pair(LFirst);
    LFrame.Session.AcceptedCheckpoint := LFirst;
    LFrame.Session.Selection := 'compose';
    LFrame.Session.View := 'compose';
    LFrame.Session.NextID := 61;
    SetLength(LFrame.Session.Undo, 1);
    SetLength(LFrame.Session.Redo, 1);
    LFrame.Session.Undo[0] := NyxStudioCheckpoint(LSecond, CDraft, CBase, True);
    LFrame.Session.Redo[0] := NyxStudioCheckpoint(LFirst, CDraft, CBase, True);

    if GFileDrafts then
    begin
      Inc(GChecks, RunNyxPascalDraftChecks);
      LFrame.Session.Undo[0] := NyxStudioCheckpoint(LSecond, CDraft, CBase, True,
        Pair(LSecond).FileDrafts);
      LFrame.Session.Redo[0] := NyxStudioCheckpoint(LFirst, CDraft, CBase, True,
        Pair(LFirst).FileDrafts);
    end;
    LPrimary := TNyxAgentSession.CreateRecovered(LFrame);
    LRegistry := Default(TNyxWorkspaceRecoveryFrame);
    LRegistry.Identity := 'whole-project';
    LRegistry.Serial := 1;
    SetLength(LRegistry.Entries, 1);
    LRegistry.Entries[0].Reference := NyxWorkspace('whole-project.project-1');
    LRegistry.Entries[0].LabelText := 'A different supporting-file context';
    LRegistry.Entries[0].Session := LFrame;
    LRegistry.Entries[0].Session.Session.Pair := Pair(LSecond);
    LRegistry.Entries[0].Session.Session.AcceptedCheckpoint := LSecond;
    LWorkspaces := TNyxStudioWorkspaces.CreateRecovered(LPrimary, LRegistry);
    LStore := TNyxStudioRuntimeStore.Create(LDirectories);
    LStore.Save(LPrimary, LWorkspaces);
    LBefore := FileBytes(LDirectories.SessionCheckpoint);
    Check(LStore.Stage(btNativeLCL), 'whole project checkpoint stages without executing');
    Check((LStore.Staged.SourceCount = 2) and
      (LStore.Staged.Source(0) = LStore.Staged.Source(1)) and
      (LStore.Staged.Files(0).Encoded <> LStore.Staged.Files(1).Encoded),
      'staging identity includes helpers across all current and history frames');
    Check(not LStore.TryPublishLiteral(LRecovered, LRecoveredWorkspaces) and
      (LRecovered = nil) and (LRecoveredWorkspaces = nil),
      'compiler-independent startup retains complete project input for explicit recovery');
    LRefused := False;
    try
      LStore.Staged.Accept(0, LProjectionSecond);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Staged.AcceptedCount = 0),
      'a real result with matching main but different helpers cannot advance recovery');
    LStore.Staged.Accept(0, LProjectionFirst);
    LStore.Staged.Accept(1, LProjectionSecond);
    LStore.Staged.Complete(LRecovered, LRecoveredWorkspaces);
    Check(EncodeNyxProject(LRecovered.RecoveryFrame.Session.Pair) = EncodeNyxProject(Pair(LFirst)),
      'staged complete-input verification restores the exact current pair');
    FreeAndNil(LRecoveredWorkspaces);
    FreeAndNil(LRecovered);
    FreeAndNil(LStore);
    LStore := TNyxStudioRuntimeStore.Create(LDirectories);
    LLegacy := TLegacyVerifier.Create;
    LCounted := LLegacy;
    LRefused := False;
    try
      LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LLegacy.Calls = 0) and (LRecovered = nil),
      'legacy verifier refuses helpers before any main-only execution');
    LCounter := TCountedVerifier.Create;
    LCounted := LCounter;
    LCounter.Inner := LVerifier;
    Check(LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted),
      'fresh actual constructors admit the complete registry');
    Check(LCounter.Calls = 2, 'one load verifies each exact main/helper context once');
    LStore.Save(LRecovered, LRecoveredWorkspaces);
    Check(SameBytes(LBefore, FileBytes(LDirectories.SessionCheckpoint)),
      'complete current/history/draft registry re-saves byte exactly');
    LRecovered.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(LRecovered.Revision))]));
    Check(EncodeNyxProject(LRecovered.RecoveryFrame.Session.Pair) = EncodeNyxProject(Pair(LSecond)),
      'recovered Undo retains different helpers plus exact unfinished draft/base');
    LRecovered.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('direction', NyxData('redo')),
      NyxField('expectedRevision', NyxData(LRecovered.Revision))]));
    Check(EncodeNyxProject(LRecovered.RecoveryFrame.Session.Pair) = EncodeNyxProject(Pair(LFirst)),
      'recovered Redo restores the exact complete previous context');
    FreeAndNil(LRecoveredWorkspaces);
    FreeAndNil(LRecovered);
    LCancellation := NewNyxBuildCancellation;
    LCounter := TCountedVerifier.Create;
    LCounted := LCounter;
    LCounter.Inner := NewNyxNativeRuntimeSourceVerifier(LDirectories, LProfile.Encode,
      TNyxCompilerLimits.Default, LCancellation, spcChecked);
    LCounter.Cancellation := LCancellation;
    LCounter.CancelAfter := 1;
    LRefused := False;
    try
      LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCounter.Calls = 1) and
      (LRecovered = nil) and (LRecoveredWorkspaces = nil),
      'cancellation after actual project execution publishes no partial registry');
    Check(SameBytes(LBefore, FileBytes(LDirectories.SessionCheckpoint)),
      'all refusal/cancellation paths preserve complete original checkpoint bytes');
    FreeAndNil(LStore);
    BrowserRecovery(LDirectories, LProfile.Encode, LFirst);
    LSemanticDirectories := LDirectories.RunningIn(LDirectories.RuntimeRoot + 'semantic-import/')
      .EnrollingProject(LDirectories.RuntimeRoot + 'semantic-import/');
    SemanticImports(LSemanticDirectories, LProfile.Encode, LVerifier, LFirst, LSecond);
    WriteLn('PASS ', GChecks, ' actual complete-project recovery/import checks');
  finally
    LStore.Free;
    LRecoveredWorkspaces.Free;
    LRecovered.Free;
    LWorkspaces.Free;
    LPrimary.Free;
    LProfile.Free;
  end;
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
