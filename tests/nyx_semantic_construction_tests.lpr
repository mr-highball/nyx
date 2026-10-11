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
program nyx_semantic_construction_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.controls, nyx.codec, nyx.schema, nyx.source, nyx.resources, nyx.design.tokens, nyx.colors,
  nyx.binding.types,
  nyx.collections, nyx.collections.registry, nyx.collections.view.types,
  nyx.contract, nyx.studio.collectionedits, nyx.studio.stateedits,
  nyx.studio.edits,
  nyx.studio.resourceedits, nyx.studio.projects,
  nyx.studio.callbackedits, nyx.studio.callbackintent,
  nyx.studio.handleredits, nyx.studio.handlerintent,
  nyx.callbacks, nyx.scheduler,
  nyx.studio.session, nyx.studio.transactions, nyx.studio.mcp,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.buildexecutor,
  nyx.studio.builds, nyx.studio.sourceprojection, nyx.studio.workspaces,
  nyx.studio.buildjobs, nyx.studio.reviews, nyx.studio.editorbuild,
  nyx.test.projection;

const
  CTitle: TNyxText = 'A crafted notebook';
  CText: TNyxText = 'An exact rocket 🚀 and letter 𐐷.';
  COwner: TNyxText = 'qualification-connection';
  CActor: TNyxText = 'Semantic qualification';
  CUnfinished: TNyxText = #10 + '{ unfinished 🚀 }';
  CNewDraft: TNyxText = #10 + '{ newly typed 𐐷 }';

type
  { An independent value owner answers only the presentation current-pair query.
    It neither publishes an edit nor confers editor/source authority. }
  TRetentionObserver = class
  public
    Pair: TNyxProjectPair;
    function IsCurrent(const APair: TNyxProjectPair): Boolean;
  end;

var
  GEngine: TNyxStudioMCP;
  GToken: TNyxText;
  GGate: TNyxText;
  GChecks: Integer;

function TRetentionObserver.IsCurrent(const APair: TNyxProjectPair): Boolean;
begin
  Result := EncodeNyxProject(APair) = EncodeNyxProject(Pair);
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Semantic construction: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Fixture files are bounded bytes. In particular, checkpoint equality does not
  reinterpret the server's opaque recovery bytes as text. }
function ReadBytes(const APath: TNyxText): TNyxBytes;
var
  LFile: TFileStream;
begin
  Result := nil;
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 16 * 1024 * 1024) then
    begin
      raise Exception.Create('Expected bounded nonempty fixture bytes');
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

procedure Marker(const APath: TNyxText; AExists: Boolean);
var
  LFile: TFileStream;
begin

  if AExists then
  begin
    LFile := TFileStream.Create(APath, fmCreate);
    LFile.Free;
  end
  else if FileExists(APath) then
  begin

    if not DeleteFile(APath) then
    begin
      raise Exception.Create('Could not release owned qualification marker');
    end;
  end;
end;

function Observe: TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
end;

function Revision: Integer;
begin
  Result := Observe.Field('session').Field('revision').AsInteger;
end;

function Group(ARevision: Integer; const AID: TNyxText;
  const AOperations: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('operationId', NyxData(AID)), NyxField('operations', AOperations)]);
end;

function Title(const AText: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('op', NyxData('title')),
    NyxField('value', NyxData(AText))]);
end;

function Submit(const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := GEngine.InvokeTool('nyx_transaction', COwner, CActor, AArguments);
end;

function SameSession(const ALeft, ARight: TNyxDataValue): Boolean;
const
  CFields: array[0..9] of TNyxText = ('revision', 'permission', 'title',
    'selection', 'view', 'pages', 'components', 'pendingDraft', 'canUndo', 'canRedo');
var
  LIndex: Integer;
begin
  { Activity intentionally records enqueue/refusal even when no document command
    commits. Compare every document/history field, not that transient sequence. }
  for LIndex := Low(CFields) to High(CFields) do
  begin

    if ALeft.Field(CFields[LIndex]).ToJSON <> ARight.Field(CFields[LIndex]).ToJSON then
    begin
      Exit(False);
    end;
  end;
  Result := True;
end;

{ Rejections must retain source, design, draft, revision and paired history.
  The gate makes that assertion independent of compiler/host scheduling speed. }
procedure Refuse(const ATool: TNyxText; const AArguments: TNyxDataValue;
  const AReason: TNyxText);
var
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LRejected: Boolean;
begin
  LBefore := Observe;
  LRejected := False;
  try
    GEngine.InvokeTool(ATool, COwner, CActor, AArguments);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  LAfter := Observe;
  Check(LRejected and (LBefore.Field('project').AsText = LAfter.Field('project').AsText) and
    SameSession(LBefore.Field('session'), LAfter.Field('session')), AReason);
end;

function Await(const AJob: TNyxText): TNyxDataValue;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Result := GEngine.InvokeBuild(COwner, CActor, NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(AJob)),
      NyxField('limit', NyxData(3))]));

    if Result.Field('publication').Field('state').AsText <> 'pending' then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Owned semantic job exceeded the qualification deadline');
    end;
    Sleep(5);
  until False;
end;

function History(const ADirection: TNyxText): TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('history')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('direction', NyxData(ADirection))]));
end;

procedure Permission(const AValue: TNyxText);
begin
  GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('configure')),
    NyxField('after', NyxData(0)), NyxField('permission', NyxData(AValue))]));
end;

function Draft(const AText: TNyxText): TNyxDataValue;
var
  LPair: TNyxProjectPair;
begin
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LPair.Pending := True;
  LPair.Draft := AText;
  LPair.DraftBase := LPair.Source;
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('heading-1')), NyxField('view', NyxData('notebook-1'))]));
end;

procedure ClearDraft;
var
  LPair: TNyxProjectPair;
begin
  { Draft-only synchronization deliberately does not create an Undo command.
    Clear that buffer explicitly; Undo would instead undo the last design group. }
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LPair.Pending := False;
  LPair.Draft := '';
  LPair.DraftBase := '';
  GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('heading-1')), NyxField('view', NyxData('notebook-1'))]));
end;

{ Exercise captured handwritten application/page/reusable builds through the
  real authenticated hosting seam. This invokes actual compilers, starts no
  listener and changes no existing project. Execution remains a separate gate. }
{ Exercise the production semantic compiler barrier with typed collection
  transactions. This fixture owns its engine/runtime; no live project is replaced.
  Every successful group has one exact paired command, then the original pair
  is restored before the existing failure/lifetime journeys continue. }
procedure SemanticCollections;
var
  LInitial: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LDocument: TNyxDocument;
  LKey: TNyxCollectionRef;
  LRows: INyxCollectionSnapshot;

  procedure Apply(const AID: TNyxText; const ATransaction: INyxProjectTransaction);
  begin
    LBefore := Observe;
    LReceipt := Submit(Group(Revision, AID, ATransaction.ToData));
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed'),
      AID + ' passes actual semantic compiler construction');
    LAfter := Observe;
    Check(Pos('class function TNotebookCards.Caption',
      DecodeNyxProject(LAfter.Field('project').AsText).Source) > 0,
      AID + ' preserves handwritten helpers');
    Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
      AID + ' has one exact paired Undo');
    Check(History('redo').Field('project').AsText = LAfter.Field('project').AsText,
      AID + ' has one exact paired Redo');
  end;

begin
  LInitial := Observe;
  LKey := NyxCollection('semantic-notes');
  Apply('semantic-collection-create', NyxProjectTransaction([
    NyxDesignStep(NyxPlacementPatch([NyxPlaceNewControl(nkTable,
      NyxControl('semantic-table'), NyxControl('notebook-1'), nplInside)])),
    NyxCollectionStep(NyxCollectionPatch([
      NyxDefineCollection(LKey, NyxCollectionSchema.Text(NyxTextField('label'), 'Note')
        .Integer(NyxIntegerField('rank'), 3, NyxIntegerDomain.Range(0, 9)),
        [NyxCollectionItem(NyxItem(LKey, 'first'))]),
      NyxBindCollection(NyxBindingOwner('semantic-table'), cpTable,
        NyxCollectionView(LKey).Column(NyxTextField('label'), 'Note')
          .Column(NyxIntegerField('rank'), 'Rank', cmEditable))]))]));
  Apply('semantic-collection-edit', NyxProjectTransaction([
    NyxCollectionStep(NyxCollectionPatch([
      NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(LKey, 'first'))
        .WithValue(NyxIntegerField('rank'), 7)),
      NyxSetCollectionField(LKey, NyxCollectionSchema.Text(NyxTextField('label'),
        'Updated default'))]))]));
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    LRows := LDocument.Collections.Snapshot(LKey);
    Check((LRows.Count = 1) and
      (LRows.ItemAt(0).GetValue(NyxIntegerField('rank')) = 7) and
      (LRows.ItemAt(0).GetValue(NyxTextField('label')) = 'Note') and
      (LRows.Schema.Definition(NyxTextField('label')).DefaultValue.TextValue = 'Updated default'),
      'semantic cell/default edits retain exact untouched row data');
    Check(LDocument.Find('semantic-table').CollectionView.Count = 2,
      'semantic compiler retains specialized table binding');
  finally
    LDocument.Free;
  end;
  History('undo');
  Check(History('undo').Field('project').AsText = LInitial.Field('project').AsText,
    'collection consumer restores its exact original owned pair');
end;

{$include nyx.test.semantic.callbacks.inc}
{$include nyx.test.semantic.handlers.inc}

procedure ApplicationJobs(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText);
const
  CScopes: array[0..2] of TNyxText = ('application', 'view', 'reusable');
  CViews: array[0..2] of TNyxText = ('', 'notebook-2', 'reusable-note');
  CTargets: array[0..1] of TNyxText = ('lcl', 'browser');
var
  LBefore: TNyxDataValue;
  LPair: TNyxProjectPair;
  LFields: array of TNyxDataField;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LOutput: TNyxText;
  LSourcePath: TNyxText;
  LStarted: QWord;
  LScope: Integer;
  LTarget: Integer;
  LPortableSource: TNyxText;
  LLocal: TNyxStudioSession;
  LExecutor: TNyxBuildExecutor;
  LBuild: INyxSourceProjectionBuild;
  LPublication: TNyxStudioSourcePublication;
begin
  { The original concurrency fixture deliberately uses native filesystem gates.
    Keep those unchanged. This separately owned in-process engine admits the
    original portable fixture through an actual native producer before either
    application compiler consumes it. No directive-refusal guard is weakened. }
  LPortableSource := NyxDecodeUTF8(ReadBytes(ADirectories.SourceRoot +
    'tests/fixtures/nyx.projection.fixture.pas'));
  ForceDirectories(ADirectories.RuntimeRoot);
  LLocal := TNyxStudioSession.Create;
  LExecutor := nil;
  try
    LLocal.SetSourceDraft(LPortableSource);
    GEngine := TNyxStudioMCP.Create(ADirectories, 8762, 8763, AProfile);
    LReceipt := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LLocal.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LReceipt.Field('token').AsText;
    LPublication := GEngine.EditorCaptureSourcePublication(GToken, NyxPrimaryWorkspace,
      Revision, LPortableSource);
    LExecutor := TNyxBuildExecutor.Create(ADirectories, AProfile);
    LBuild := LExecutor.ProjectSource(LPortableSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecuted, 'portable application fixture is actually executed');
    GEngine.EditorCommitSourceProjection(GToken, LPublication, LBuild.Projection,
      NyxControl('heading-1'), NyxControl('notebook-1'));
  finally
    LExecutor.Free;
    LLocal.Free;
  end;
  LBefore := Observe;
  LPair := DecodeNyxProject(LBefore.Field('project').AsText);
  LOutput := GEngine.InvokeBuild(COwner, CActor, NyxObject([
    NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
  for LTarget := 0 to 1 do
  begin
    for LScope := 0 to 2 do
    begin
      SetLength(LFields, 6);
      LFields[0] := NyxField('mode', NyxData('request'));
      LFields[1] := NyxField('expectedRevision', LBefore.Field('session').Field('revision'));
      LFields[2] := NyxField('operationId', NyxData('handwritten-' +
        CTargets[LTarget] + '-' + CScopes[LScope]));
      LFields[3] := NyxField('outputID', NyxData(LOutput));
      LFields[4] := NyxField('target', NyxData(CTargets[LTarget]));
      LFields[5] := NyxField('scope', NyxData(CScopes[LScope]));

      if LScope <> 0 then
      begin
        SetLength(LFields, 7);
        LFields[6] := NyxField('view', NyxData(CViews[LScope]));
      end;
      LRequest := NyxObject(LFields);
      LReceipt := GEngine.InvokeBuild(COwner, CActor, LRequest);
      LStarted := GetTickCount64;
      repeat
        LStatus := GEngine.InvokeBuild(COwner, CActor, NyxObject([
          NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
          NyxField('limit', NyxData(10))]));

        if (LStatus.Field('state').AsText = 'succeeded') or
          (LStatus.Field('state').AsText = 'failed') or
          (LStatus.Field('state').AsText = 'cancelled') then
        begin
          Break;
        end;
        Sleep(5);
      until GetTickCount64 - LStarted >= 90000;

      if LStatus.Field('state').AsText <> 'succeeded' then
      begin
        WriteLn('Application failure ', LStatus.ToJSON);
      end;
      Check((LStatus.Field('state').AsText = 'succeeded') and
        LStatus.Field('currentSource').AsBoolean and LStatus.Field('currentOutput').AsBoolean,
        'actual captured handwritten ' + CTargets[LTarget] + '/' + CScopes[LScope] + ' builds');
      LSourcePath := ADirectories.Jobs + Copy(LStatus.Field('compiledSource').AsText, 8, MaxInt);
      Check(NyxDecodeUTF8(ReadBytes(LSourcePath)) = LPair.Source,
        'application/page/reusable build retains every exact handwritten companion byte');
      Check((Observe.Field('project').AsText = LBefore.Field('project').AsText) and
        SameSession(Observe.Field('session'), LBefore.Field('session')),
        'compiler jobs leave the accepted pair and complete history state unchanged');
    end;
  end;
end;


var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LExecutor: TNyxBuildExecutor;
  LLocal: TNyxStudioSession;
  LObserver: TNyxStudioSession;
  LDocument: TNyxDocument;
  LLock: TFileStream;
  LPool: TNyxBuildJobs;
  LUnavailable: TNyxOutputConfiguration;
  LRetention: TRetentionObserver;
  LProposal: INyxPreparedDesign;
  LCompletion: TNyxConstructionCompletion;
  LTools: TNyxDataValue;
  LState: TNyxDataValue;
  LBefore: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LOperations: TNyxDataValue;
  LJobs: TNyxDataValue;
  LRequest: TNyxStudioSourcePublication;
  LBuild: INyxSourceProjectionBuild;
  LFrame: TNyxSourceCheckpoint;
  LPair: TNyxProjectPair;
  LBaseline: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LIntent: TNyxStudioDesignRequest;
  LSource: TNyxText;
  LExpected: TNyxText;
  LFailure: TNyxText;
  LMismatch: TNyxText;
  LCheckpoint: TNyxText;
  LJob: TNyxText;
  LBeforeBytes: TNyxBytes;
  LRejected: Boolean;
  LRevision: Integer;
  LIndex: Integer;
  LStarted: QWord;
  LCurrentOutput: Boolean;
begin
  LProfile := nil;
  LExecutor := nil;
  LLocal := nil;
  LObserver := nil;
  LDocument := nil;
  LLock := nil;
  LPool := nil;
  LUnavailable := nil;
  LRetention := nil;
  try

    if ((ParamCount <> 3) and ((ParamCount <> 4) or
      ((ParamStr(4) <> '--callbacks-only') and (ParamStr(4) <> '--handlers-only')))) or
      DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
    end;
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
    ForceDirectories(LDirectories.RuntimeRoot);
    GGate := LDirectories.RuntimeRoot + 'constructor.wait';
    LFailure := LDirectories.RuntimeRoot + 'constructor.fail';
    LMismatch := LDirectories.RuntimeRoot + 'constructor.mismatch';
    LCheckpoint := LDirectories.RuntimeRoot + '.local/studio-session.nyx';
    LTools := TNyxDataValue.ParseJSON(NyxDecodeUTF8(ReadBytes(ParamStr(2))));
    LSource := NyxDecodeUTF8(ReadBytes(LDirectories.SourceRoot +
      'tests/fixtures/nyx.projection.fixture.pas'));
    { An unrelated handwritten class reserves the first natural callback name.
      The disposable literal planner must account for the complete retained unit. }
    LSource := StringReplace(LSource, '  TNotebookCards = class',
      '  THeading1Click = class' + #10 + '  end;' + #10 + #10 +
      '  TNotebookCards = class', []);
    { Native-only external-input fixture gates actual constructor execution.
      These private paths exist only in generated ignored qualification source.
      Browser qualification compiles the portable companion separately. }
    LSource := StringReplace(LSource, 'function PageName(AIndex: Integer): TNyxText;',
      'procedure WaitForQualification;' + #10 + 'begin' + #10 +
      '  while FileExists(' + TNyxText(QuotedStr(GGate)) + ') do' + #10 +
      '  begin' + #10 + '    Sleep(5);' + #10 + '  end;' + #10 + #10 +
      '  if FileExists(' + TNyxText(QuotedStr(LFailure)) + ') then' + #10 +
      '  begin' + #10 + '    raise Exception.Create(''Deliberate constructor refusal'');' + #10 +
      '  end;' + #10 + 'end;' + #10 + #10 +
      'function PageName(AIndex: Integer): TNyxText;', []);
    LSource := StringReplace(LSource, '  LDocument := TNyxDocument.Create;',
      '  WaitForQualification;' + #10 + '  LDocument := TNyxDocument.Create;', []);
    LSource := StringReplace(LSource, '    LDocument.Validate;',
      #10 + '    if FileExists(' + TNyxText(QuotedStr(LMismatch)) + ') then' + #10 +
      '    begin' + #10 + '      LDocument.State.SetValue(NyxIntegerState(''notes''), 4);' + #10 +
      '    end;' + #10 + '    LDocument.Validate;', []);
    { ANSI RTL StringReplace returns byte-identical text with its native codepage
      label. Re-admit these known UTF-8 fixture bytes before any text comparison;
      do not change the process codepage or relax the backend's exact guard. }
    LSource := NyxDecodeUTF8(NyxEncodeUTF8(LSource));
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LProfile.SetField('lazarus', LTools.Field('LAZARUS').AsText);
    LProfile.SetField('platform', 'i386-win32');
    LProfile.SetField('widgetset', 'win32');
    LLocal := TNyxStudioSession.Create;
    LLocal.SetSourceDraft(LSource);
    LPair := LLocal.ProjectSnapshot;
    Check(LPair.Pending and (LPair.Draft = LSource), 'fixture owns its exact pending source');
    { In-process authenticated hosting seam: no Start/listener, existing project,
      installed enrollment or browser UI automation is involved. }
    GEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763, LProfile.Encode);
    LState := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LState.Field('token').AsText;
    Check(LState.Field('state').Field('project').AsText = EncodeNyxProject(LPair),
      'initial editor claim preserves its exact pending pair');
    LBefore := Observe;
    Check(LBefore.Field('project').AsText = EncodeNyxProject(LPair),
      'independent observation retains the pending source');
    LRevision := LBefore.Field('session').Field('revision').AsInteger;
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision, LBaseline, LFrame);
    Check(LBaseline.Pending and (LBaseline.Draft = LSource),
      'native captured registry retains the same pending buffer');
    LRequest := GEngine.EditorCaptureSourcePublication(GToken, NyxPrimaryWorkspace,
      LRevision, LSource);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);

    if LBuild.Projection.State <> spsExecuted then
    begin
      WriteLn('Native constructor state=', Ord(LBuild.Projection.State),
        ' message=', LBuild.Projection.Message);
    end;
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = ExpectedNyxProjectionDesign), 'actual baseline construction');
    Check(Pos('0 unfreed memory blocks', LBuild.RuntimeLog) > 0, 'constructor child clean heap');
    GEngine.EditorCommitSourceProjection(GToken, LRequest, LBuild.Projection,
      NyxControl('heading-1'), NyxControl('notebook-1'));
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, Revision, LBaseline, LFrame);
    LLocal.AdoptCapturedProject(LBaseline, LFrame);

    if ParamStr(4) = '--handlers-only' then
    begin
      SemanticHandlers(LFailure, LMismatch, LCheckpoint);
      WriteLn('PASS ', GChecks, ' focused semantic handler construction checks');
      Exit;
    end;

    if ParamStr(4) = '--callbacks-only' then
    begin
      SemanticCallbacks(LFailure, LCheckpoint);
      WriteLn('PASS ', GChecks, ' focused semantic callback construction checks');
      Exit;
    end;
    LOperations := NyxArray([Title(CTitle), NyxObject([
      NyxField('op', NyxData('update')), NyxField('id', NyxData('heading-1')),
      NyxField('properties', NyxObject([NyxField('text', NyxData(CText))]))]),
      NyxObject([NyxField('op', NyxData('update')), NyxField('id', NyxData('notebook-1')),
      NyxField('properties', NyxObject([NyxField('padding', NyxData(19))]))])]);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTransaction;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Transaction := CaptureNyxProjectTransaction(ReadNyxProjectTransaction(LOperations));
    LIntent := LLocal.PrepareDesignRequest(LEdit, CaptureNyxSchemas.Revision);
    Check((LIntent.IntentData.Field('version').AsInteger = 16) and
      (ReadNyxStudioDesignIntent(LIntent.IntentData).Transaction.ToData.ToJSON =
      LEdit.Transaction.ToData.ToJSON), 'closed grouped intent round trips independently');
    LDocument := TNyxCodec.Decode(ExpectedNyxProjectionDesign);
    LDocument.Title := CTitle;
    RequireNyxControl(LDocument, NyxControl('heading-1'), nkHeading).Configure.Text(CText);
    RequireNyxControl(LDocument, NyxControl('notebook-1'), nkPage).Configure.Padding(19);
    LExpected := TNyxCodec.Encode(LDocument);
    FreeAndNil(LDocument);
    Marker(GGate, True);
    LRevision := Revision;
    LArguments := Group(LRevision, 'first-semantic-group', LOperations);
    LReceipt := Submit(LArguments);
    LJob := LReceipt.Field('job').AsText;
    Check((LReceipt.Field('purpose').AsText = 'transaction') and
      (LReceipt.Field('scope').AsText = 'construction') and
      (LReceipt.Field('publication').Field('state').AsText = 'pending'),
      'bounded receipt distinguishes compiler job from document publication');
    Check((LReceipt.Field('artifact').AsText = '') and
      (LReceipt.Field('compiledSource').AsText = '') and (Length(LReceipt.ToJSON) < 8192),
      'receipt contains no application artifact or source');
    Check(Observe.Field('project').AsText = EncodeNyxProject(LBaseline),
      'enqueue preserves exact active source/design');
    Check(Submit(LArguments).ToJSON = LReceipt.ToJSON, 'exact retry does not start another worker');
    Refuse('nyx_transaction', Group(LRevision, 'first-semantic-group', NyxArray([Title('Other')])),
      'changed retry arguments refuse atomically');
    Refuse('nyx_transaction', Group(LRevision + 1, 'stale', NyxArray([Title('Other')])),
      'stale revision refuses before compiler admission');
    Refuse('nyx_transaction', Group(LRevision, 'invalid-tail', NyxArray([
      Title('Partial'), NyxObject([NyxField('op', NyxData('update')),
      NyxField('id', NyxData('missing')), NyxField('properties',
      NyxObject([NyxField('text', NyxData('No'))]))])])), 'late invalid operation retains whole pair');
    Refuse('nyx_transaction', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('foreign-source')), NyxField('operations', LOperations),
      NyxField('source', NyxData(LSource))]), 'caller cannot supply source or execution authority');
    LJobs := GEngine.InvokeBuild(COwner, CActor, NyxObject([
      NyxField('mode', NyxData('jobs')), NyxField('limit', NyxData(2))]));
    Check((LJobs.Field('items').Count = 1) and
      (LJobs.Field('items').Item(0).Field('purpose').AsText = 'transaction') and
      LJobs.Field('items').Item(0).Field('canCancel').AsBoolean and
      not LJobs.Field('items').Item(0).Field('currentSource').AsBoolean,
      'observer metadata distinguishes owned unpublished semantic job');
    LJobs := GEngine.InvokeBuild('foreign-connection', CActor, NyxObject([
      NyxField('mode', NyxData('jobs'))]));
    Check(not LJobs.Field('items').Item(0).Field('canCancel').AsBoolean,
      'display actor cannot grant another transport cancellation');
    LRejected := False;
    try
      GEngine.InvokeBuild('foreign-connection', CActor, NyxObject([
        NyxField('mode', NyxData('cancel')), NyxField('job', NyxData(LJob)),
        NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('steal'))]));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'foreign transport cannot cancel compiler verification');
    Marker(GGate, False);
    LStatus := Await(LJob);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed') and
      (LStatus.Field('publication').Field('revision').AsInteger = LRevision + 1),
      'real compiler group publishes exactly the next revision');
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, Revision, LPair, LFrame);
    Check((LPair.Design = LExpected) and not LPair.Pending and
      (LFrame.Origin = nsoExecuted), 'whole exact Unicode group and execution origin admitted');
    Check((Pos('for LIndex := 1 to 2 do', LPair.Source) > 0) and
      (Pos('Padding(3 * 4)', LPair.Source) > 0) and
      (Pos('class function TNotebookCards.Caption', LPair.Source) > 0) and
      (Pos('INyxHeading', LPair.Source) > 0), 'helpers/expressions and specialized interfaces retained');
    Check(LStatus.Field('currentSource').AsBoolean and LStatus.Field('currentOutput').AsBoolean,
      'job currentness refers to published pair rather than its previous baseline');
    Check(Submit(LArguments).ToJSON = LReceipt.ToJSON, 'completed exact retry keeps original receipt');
    LObserver := TNyxStudioSession.Create;
    LObserver.AdoptCapturedProject(LPair, LFrame);
    Check((LObserver.Save = LExpected) and (LObserver.Source = LPair.Source),
      'independent observing host receives exact compiled pair');
    Check(History('undo').Field('project').AsText = EncodeNyxProject(LBaseline),
      'one Undo restores the entire previous source/design group');
    Check(History('redo').Field('project').AsText = EncodeNyxProject(LPair),
      'one Redo restores the entire verified group');
    LRejected := False;
    try
      GEngine.InvokeBuild(COwner, CActor, NyxObject([NyxField('mode', NyxData('launch')),
        NyxField('job', NyxData(LJob)), NyxField('expectedRevision', NyxData(Revision)),
        NyxField('operationId', NyxData('not-an-app'))]));
    except
      on LException: Exception do
      begin
        LRejected := Pos('not a launchable', LException.Message) > 0;
      end;
    end;
    Check(LRejected, 'constructor proof cannot be mounted as an application');
    { Preserve the whole-meaning refusal with an admitted family that the
      continuation does not express yet. Resource variants now have their own
      typed continuation, while theme reconciliation remains an explicit gap. }
    Refuse('nyx_transaction', Group(Revision, 'unsupported-theme', NyxArray([
      Title('Partial'), NyxDesignChanges(NyxThemePatch(NyxThemeTokens.Accent(NyxRGB(118, 84, 200))))
        .ToData.Item(0)])), 'unsupported theme meaning refuses the complete group');
    { The production semantic compiler barrier, not a caller supplied projection,
      now accepts one resource/binding/title group in the same handwritten pair. }
    LBefore := Observe;
    LReceipt := Submit(Group(Revision, 'resource-continuation', NyxArray([
      Title('A resource-backed notebook'), NyxObject([NyxField('op', NyxData('resources')),
        NyxField('changes', NyxResourcePatch([
          NyxDefineResource(NyxResourceRef('shared-copy'), NyxDefaultLocale,
            NyxJSONResource('{"title":"Shared copy 🚀 𐐷","exact":9007199254740993}')
              .Tagged(NyxResourceLabel('Copy')).Describe('Shared copy', 'Semantic resource group')),
          NyxBindResource(NyxControl('heading-1'), bpText,
            NyxResourceValue(NyxResourceRef('shared-copy')).Field('title'))
        ]).ToData)])])));
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed'),
      'actual semantic resource construction publishes the whole group');
    LState := Observe;
    LDocument := TNyxCodec.Decode(DecodeNyxProject(LState.Field('project').AsText).Design);
    try
      Check((LDocument.Title = 'A resource-backed notebook') and
        (LDocument.Resources.Count = 3) and (LDocument.Find('heading-1').BindingCount = 1),
        'semantic resource and binding meaning is complete');
      Check(LDocument.Resources.Definition(NyxResourceRef('shared-copy'), NyxDefaultLocale)
        .Data.Field('exact').AsDecimal.Text = '9007199254740993',
        'semantic compilation preserves exact JSON numeric tokens');
    finally
      FreeAndNil(LDocument);
    end;
    Check(Pos('NyxTextResource(TNotebookCards.Caption)',
      DecodeNyxProject(LState.Field('project').AsText).Source) > 0,
      'semantic group preserves unrelated computed resource code');
    Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
      'semantic resource group has one exact paired Undo');
    Check(History('redo').Field('project').AsText = LState.Field('project').AsText,
      'semantic resource group has one exact paired Redo');
    SemanticCollections;
    SemanticCallbacks(LFailure, LCheckpoint);
    SemanticHandlers(LFailure, LMismatch, LCheckpoint);
    Check(History('undo').Field('project').AsText = EncodeNyxProject(LPair),
      'fixture restores its prior owned pair before remaining refusal journeys');
    Draft(LPair.Source + CUnfinished);
    Refuse('nyx_transaction', Group(Revision, 'pending', NyxArray([Title('No')])),
      'pending draft refuses compiler transaction admission');
    ClearDraft;
    Marker(GGate, True);
    LReceipt := Submit(Group(Revision, 'later-draft', NyxArray([Title('No')])));
    LBefore := Draft(LPair.Source + CNewDraft);
    Marker(GGate, False);
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'later exact draft makes completion stale without replacing it');
    Check(not LStatus.Field('currentSource').AsBoolean,
      'unpublished diagnostics never navigate the active source');
    ClearDraft;
    Marker(GGate, True);
    LReceipt := Submit(Group(Revision, 'permission-change', NyxArray([Title('No')])));
    Permission('readOnly');
    LBefore := Observe;
    Marker(GGate, False);
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'revoked edit permission prevents completed publication');
    Permission('edit');
    Marker(GGate, True);
    LBefore := Observe;
    LReceipt := Submit(Group(Revision, 'cancel-own', NyxArray([Title('No')])));
    GEngine.InvokeBuild(COwner, CActor, NyxObject([NyxField('mode', NyxData('cancel')),
      NyxField('job', LReceipt.Field('job')), NyxField('expectedRevision', NyxData(Revision)),
      NyxField('operationId', NyxData('cancel-owned-proof'))]));
    Marker(GGate, False);
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('state').AsText = 'cancelled') and
      (LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'owned cancellation joins and retains exact accepted pair');
    Marker(LFailure, True);
    LBefore := Observe;
    LReceipt := Submit(Group(Revision, 'runtime-failure', NyxArray([Title('No')])));
    LStatus := Await(LReceipt.Field('job').AsText);
    Marker(LFailure, False);
    Check((LStatus.Field('state').AsText = 'failed') and
      (LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'actual constructor exception never publishes partial design');
    Marker(LMismatch, True);
    LBefore := Observe;
    LReceipt := Submit(Group(Revision, 'whole-meaning-mismatch', NyxArray([Title('No')])));
    LStatus := Await(LReceipt.Field('job').AsText);
    Marker(LMismatch, False);
    Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'same-source real native external-input mismatch refuses complete meaning');
    Marker(GGate, True);
    LBefore := Observe;
    LBeforeBytes := ReadBytes(LCheckpoint);
    LLock := TFileStream.Create(LCheckpoint, fmOpenRead or fmShareExclusive);
    LReceipt := Submit(Group(Revision, 'durable-refusal', NyxArray([Title('No')])));
    Marker(GGate, False);
    LStatus := Await(LReceipt.Field('job').AsText);
    FreeAndNil(LLock);
    Check(LStatus.Field('publication').Field('state').AsText = 'refused',
      'durable replacement failure refuses publication');
    Check(Observe.Field('project').AsText = LBefore.Field('project').AsText,
      'durable replacement failure restores exact source/design/draft');
    Check(SameSession(Observe.Field('session'), LBefore.Field('session')),
      'durable replacement failure restores revision/selection/history');
    Check(SameBytes(ReadBytes(LCheckpoint), LBeforeBytes),
      'durable replacement failure retains exact checkpoint bytes');
    { Direct owning-pool qualification isolates the narrow retirement race: a
      worker can join after the backend's first poll, before another admission.
      Unavailable-verifier jobs require no child or application output target;
      each joined completion deliberately remains unsealed in this test owner. }
    LBefore := Observe;
    LRevision := Revision;
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision, LPair, LFrame);
    LLocal.AdoptCapturedProject(LPair, LFrame);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTitle;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Value := 'Retention qualification';
    LIntent := LLocal.PrepareDesignRequest(LEdit, CaptureNyxSchemas.Revision);
    LProposal := PrepareNyxStudioDesign(LIntent, CaptureNyxSchemas);
    LRequest := GEngine.EditorCaptureVisualPublication(GToken, NyxPrimaryWorkspace,
      LRevision, LIntent.IntentData, LProposal.Source);
    LUnavailable := TNyxOutputConfiguration.Create;
    LUnavailable.SetField('fpc', LDirectories.RuntimeRoot + 'missing-qualification-verifier.exe');
    LPool := TNyxBuildJobs.Create(LDirectories.RunningIn(LDirectories.RuntimeRoot +
      'retention'), LUnavailable.Encode);
    LPool.ConfigureSourceLease(1);
    LJob := '';
    for LIndex := 1 to 16 do
    begin
      LReceipt := LPool.SubmitTransaction(CActor,
        Group(LRevision, 'retention-' + TNyxText(IntToStr(LIndex)),
        NyxArray([Title(LEdit.Value)])), Default(TNyxReviewRef), COwner,
        NyxPrimaryWorkspace, LRequest);

      if LIndex = 1 then
      begin
        LJob := LReceipt.Field('job').AsText;
      end;
      LStarted := GetTickCount64;
      while not LPool.TakeConstructionCompletion(LCompletion) do
      begin

        if GetTickCount64 - LStarted > 10000 then
        begin
          raise Exception.Create('Retained owning pool did not join its unavailable verifier');
        end;
        Sleep(1);
      end;
    end;
    LStatus := LPool.Status(NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob))]), LPair, LCurrentOutput);
    Check(LStatus.Field('publication').Field('state').AsText = 'pending',
      'sixteen joined completions retain their first unsealed semantic edit');
    LRetention := TRetentionObserver.Create;
    LRetention.Pair := LPair;
    LJobs := LPool.List(NyxObject([NyxField('filter', NyxData('active')),
      NyxField('limit', NyxData(16))]), Default(TNyxReviewRef), NyxPrimaryWorkspace,
      COwner, True, True, LRetention.IsCurrent);
    Check((LJobs.Field('total').AsInteger = 16) and
      (LJobs.Field('items').Count = 16),
      'all joined unsealed edits remain in the bounded active operator list');
    Check((LJobs.Field('running').AsInteger = 0) and
      (LJobs.Field('queued').AsInteger = 0) and
      (LJobs.Field('cancelling').AsInteger = 0),
      'publication work is visible without claiming an occupied compiler slot');
    for LIndex := 0 to 15 do
    begin
      LStatus := LJobs.Field('items').Item(LIndex);
      Check((LStatus.Field('publication').Field('state').AsText = 'pending') and
        not LStatus.Field('currentSource').AsBoolean and
        not LStatus.Field('canCancel').AsBoolean,
        'joined unsealed row grants neither active-source navigation nor worker cancellation');
    end;
    LJobs := LPool.List(NyxObject([]), Default(TNyxReviewRef),
      NyxWorkspace('another-owned-workspace'), COwner, True, True, LRetention.IsCurrent);
    Check(LJobs.Field('total').AsInteger = 0,
      'unsealed activity stays confined to its owning workspace');
    LJobs := LPool.List(NyxObject([]), NyxReview('another-owned-review'),
      NyxPrimaryWorkspace, COwner, True, True, LRetention.IsCurrent);
    Check(LJobs.Field('total').AsInteger = 0,
      'unsealed activity stays confined to its owning review');
    LRejected := False;
    try
      LPool.SubmitTransaction(CActor, Group(LRevision, 'retention-seventeen',
        NyxArray([Title(LEdit.Value)])), Default(TNyxReviewRef), COwner,
        NyxPrimaryWorkspace, LRequest);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'full pending publication retention refuses another job without eviction');
    LPool.FinishConstruction(NyxBuildJob(LJob), False, 0,
      Default(TNyxProjectPair), 'Qualification owner deliberately refused publication');
    LJobs := LPool.List(NyxObject([NyxField('filter', NyxData('active')),
      NyxField('limit', NyxData(16))]), Default(TNyxReviewRef), NyxPrimaryWorkspace,
      COwner, True, True, LRetention.IsCurrent);
    Check((LJobs.Field('total').AsInteger = 15) and
      (LJobs.Field('items').Count = 15),
      'sealed refusal leaves active publication work without hiding other pending edits');
    LPool.SubmitTransaction(CActor, Group(LRevision, 'retention-seventeen',
      NyxArray([Title(LEdit.Value)])), Default(TNyxReviewRef), COwner,
      NyxPrimaryWorkspace, LRequest);
    LRejected := False;
    try
      LPool.Status(NyxObject([NyxField('mode', NyxData('status')),
        NyxField('job', NyxData(LJob))]), LPair, LCurrentOutput);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'only a sealed terminal completion becomes eligible for bounded retirement');
    FreeAndNil(LPool);
    Check((Observe.Field('project').AsText = LBefore.Field('project').AsText) and
      SameSession(Observe.Field('session'), LBefore.Field('session')),
      'owning worker pool cannot publish or borrow the active editor');
    FreeAndNil(GEngine);
    ApplicationJobs(LDirectories.RunningIn(LDirectories.RuntimeRoot + 'portable-applications/')
      .EnrollingProject(LDirectories.RuntimeRoot + 'portable-applications/'), LProfile.Encode);
    WriteLn('PASS ', GChecks, ' semantic construction/application checks');
  finally
    LLock.Free;
    LPool.Free;
    LRetention.Free;
    LUnavailable.Free;
    Marker(GGate, False);
    LDocument.Free;
    LObserver.Free;
    LLocal.Free;
    LExecutor.Free;
    GEngine.Free;
    LProfile.Free;
  end;
end.
