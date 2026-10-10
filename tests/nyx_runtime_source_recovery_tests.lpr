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
program nyx_runtime_source_recovery_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, md5, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.types,
  nyx.source, nyx.schema, nyx.studio.projects, nyx.studio.history,
  nyx.studio.session, nyx.studio.agents, nyx.studio.workspaces,
  nyx.studio.recovery, nyx.studio.recovery.native, nyx.studio.mcp,
  nyx.studio.sourceprojection, nyx.studio.buildexecutor, nyx.studio.directories,
  nyx.studio.outputs, nyx.studio.sourcebuilds, nyx.studio.builds,
  nyx.test.projection;

type
  { Counts actual delegated executions, never fabricates a projection. Faults
    cancel the shared token or change creator metadata after real native work. }
  TCountingVerifier = class(TInterfacedObject, INyxRuntimeSourceVerifier)
  public
    Inner: INyxRuntimeSourceVerifier;
    Cancellation: INyxBuildCancellation;
    Calls: Integer;
    CancelAfter: Integer;
    ChangeSchemaAfter: Integer;
    procedure RequireActive;
    function Verify(const ASource: TNyxText): INyxSourceProjection;
  end;

const
  CCurrentDraft: TNyxText = 'Unfinished current notes 🚀';
  CUndoDraft: TNyxText = 'Unfinished earlier notes 𐐷';
  CRedoDraft: TNyxText = 'Unfinished later notes é';
  CUndoBase: TNyxText = '{ An older Undo base 🚀 }';
  CRedoBase: TNyxText = '{ An older Redo base 𐐷 }';
  CTitle: TNyxText = 'LDocument.Title := ''Handwritten notebook'';';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Runtime source recovery: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TCountingVerifier.RequireActive;
begin
  Inner.RequireActive;
end;

function TCountingVerifier.Verify(const ASource: TNyxText): INyxSourceProjection;
begin
  Inc(Calls);
  Result := Inner.Verify(ASource);

  if Calls = CancelAfter then
  begin
    Cancellation.Cancel;
  end;

  if Calls = ChangeSchemaAfter then
  begin
    RegisterNyxSchema(NyxCustomKind('recovery-generation-check'), [], []);
  end;
end;

function Counted(const AInner: INyxRuntimeSourceVerifier;
  out ACounter: TCountingVerifier): INyxRuntimeSourceVerifier;
begin
  ACounter := TCountingVerifier.Create;
  Result := ACounter;
  ACounter.Inner := AInner;
end;

function ReadBytes(const APath: TNyxText): TNyxBytes;
var
  LStream: TFileStream;
begin
  Result := nil;
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 1) or (LStream.Size > 16 * 1024 * 1024) then
    begin
      raise Exception.Create('Owned qualification file exceeds its input budget');
    end;
    SetLength(Result, LStream.Size);
    LStream.ReadBuffer(Result[0], Length(Result));
  finally
    LStream.Free;
  end;
end;

procedure WriteBytes(const APath: TNyxText; const ABytes: TNyxBytes);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate or fmShareExclusive);
  try

    if Length(ABytes) > 0 then
    begin
      LStream.WriteBuffer(ABytes[0], Length(ABytes));
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
    Result := CompareMem(@ALeft[0], @ARight[0], Length(ALeft));
  end;
end;

{ Mutate an existing last field without changing its framing length, then
  recompute the checksum. This is explicit owned-file corruption qualification,
  not a second implementation of the checkpoint codec or execution producer. }
function ReplaceLastByte(const ABytes: TNyxBytes; const APattern: TNyxText;
  AReplacement: Byte): TNyxBytes;
var
  LPattern: TNyxBytes;
  LChecksum: TNyxBytes;
  LIndex: Integer;
begin
  Result := Copy(ABytes);
  LPattern := NyxEncodeUTF8(APattern);
  LIndex := Length(Result) - 32 - Length(LPattern);
  while LIndex >= 0 do
  begin

    if CompareMem(@Result[LIndex], @LPattern[0], Length(LPattern)) then
    begin
      Result[LIndex + Length(LPattern) - 1] := AReplacement;
      LChecksum := NyxEncodeUTF8(TNyxText(MD5Print(MD5Buffer(Result[0],
        Length(Result) - 32))));
      Move(LChecksum[0], Result[Length(Result) - 32], 32);
      Exit;
    end;
    Dec(LIndex);
  end;
  raise Exception.Create('The owned corruption field was not found');
end;

function PascalLiteral(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '''';
  for LIndex := 1 to Length(AText) do
  begin
    Result := Result + Copy(AText, LIndex, 1);

    if AText[LIndex] = '''' then
    begin
      Result := Result + '''';
    end;
  end;
  Result := Result + '''';
end;

function ExecutedCheckpoint(const AVerifier: INyxRuntimeSourceVerifier;
  const ASource: TNyxText): TNyxSourceCheckpoint;
var
  LProjection: INyxSourceProjection;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  LProjection := AVerifier.Verify(ASource);
  Check((LProjection.State = spsExecuted) and
    (LProjection.Design = ExpectedNyxProjectionDesign),
    'real FPC construction independently reproduces the complete notebook');
  AdmitNyxProjectedProject(NyxProjectPair(ExpectedNyxProjectionDesign, ASource),
    LProjection, LDocument, LWorkspace, LResolved);
  try
    Result := LWorkspace.Capture;
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

function Pair(const ACheckpoint: TNyxSourceCheckpoint;
  const ADraft: TNyxText): TNyxProjectPair;
begin
  Result := NyxProjectPair(ACheckpoint.Design, ACheckpoint.Source);
  Result.Pending := True;
  Result.Draft := ADraft;
  Result.DraftBase := '{ An older independent base 🌙 }';
end;

procedure Traverse(ASession: TNyxAgentSession; const ADirection: TNyxText);
begin
  ASession.Exchange(NyxObject([NyxField('op', NyxData('history')),
    NyxField('expectedRevision', NyxData(ASession.Revision)),
    NyxField('direction', NyxData(ADirection))]));
end;

var
  LDirectories: TNyxStudioDirectories;
  LTools: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LVerifier: INyxRuntimeSourceVerifier;
  LCounted: INyxRuntimeSourceVerifier;
  LCounter: TCountingVerifier;
  LCancellation: INyxBuildCancellation;
  LFirst: TNyxSourceCheckpoint;
  LSecond: TNyxSourceCheckpoint;
  LSource: TNyxText;
  LChangedSource: TNyxText;
  LFaultPath: TNyxText;
  LDesignPattern: TNyxText;
  LPosition: Integer;
  LFrame: TNyxAgentRecoveryFrame;
  LRegistry: TNyxWorkspaceRecoveryFrame;
  LPrimary: TNyxAgentSession;
  LWorkspaces: TNyxStudioWorkspaces;
  LRecovered: TNyxAgentSession;
  LRecoveredWorkspaces: TNyxStudioWorkspaces;
  LSession: TNyxAgentSession;
  LStore: TNyxStudioRuntimeStore;
  LEngine: TNyxStudioMCP;
  LObserved: TNyxDataValue;
  LProgress: TNyxDataValue;
  LRequest: TNyxDataValue;
  LStatus: TNyxDataValue;
  LBuild: INyxSourceProjectionBuild;
  LProjection: INyxSourceProjection;
  LStage: TNyxStudioRuntimeRecovery;
  LToken: TNyxText;
  LPreviousToken: TNyxText;
  LJob: TNyxText;
  LStarted: QWord;
  LBefore: TNyxBytes;
  LCorrupt: TNyxBytes;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LProfile := nil;
  LPrimary := nil;
  LWorkspaces := nil;
  LRecovered := nil;
  LRecoveredWorkspaces := nil;
  LStore := nil;
  LEngine := nil;
  try

    if (ParamCount <> 3) or FileExists(ParamStr(3)) or DirectoryExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
    end;
    ForceDirectories(ParamStr(3));
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
    LTools := TNyxDataValue.ParseJSON(NyxDecodeUTF8(ReadBytes(ParamStr(2))));
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LVerifier := NewNyxNativeRuntimeSourceVerifier(LDirectories, LProfile.Encode,
      TNyxCompilerLimits.Default, nil, spcChecked);
    LSource := NyxDecodeUTF8(ReadBytes(LDirectories.SourceRoot +
      'tests/fixtures/nyx.projection.fixture.pas'));
    LFirst := ExecutedCheckpoint(LVerifier, LSource);
    LFaultPath := LDirectories.RuntimeRoot + 'fail-later-construction.flag';
    LPosition := Pos(CTitle, LSource);
    Check(LPosition > 0, 'qualification inserts its owned runtime dependency exactly');
    LChangedSource := Copy(LSource, 1, LPosition - 1) +
      'if FileExists(' + PascalLiteral(LFaultPath) + ') then' + #10 +
      '    begin' + #10 +
      '      raise Exception.Create(''Late recovery construction failed'');' + #10 +
      '    end;' + #10 + #10 + '    ' + Copy(LSource, LPosition, MaxInt);
    LSecond := ExecutedCheckpoint(LVerifier, LChangedSource);

    LFrame := Default(TNyxAgentRecoveryFrame);
    LFrame.Revision := 17;
    LFrame.Permission := apEdit;
    LFrame.Claimed := True;
    LFrame.Session.Pair := Pair(LFirst, CCurrentDraft);
    LFrame.Session.AcceptedCheckpoint := LFirst;
    LFrame.Session.Selection := 'heading-2';
    LFrame.Session.View := 'notebook-2';
    LFrame.Session.NextID := 73;
    SetLength(LFrame.Session.Undo, 1);
    SetLength(LFrame.Session.Redo, 1);
    LFrame.Session.Undo[0] := NyxStudioCheckpoint(LFirst, CUndoDraft,
      CUndoBase, True);
    LFrame.Session.Redo[0] := NyxStudioCheckpoint(LFirst, CRedoDraft,
      CRedoBase, True);
    LPrimary := TNyxAgentSession.CreateRecovered(LFrame);
    LRegistry := Default(TNyxWorkspaceRecoveryFrame);
    LRegistry.Identity := 'recover';
    LRegistry.Serial := 8;
    SetLength(LRegistry.Entries, 8);
    for LIndex := 0 to 7 do
    begin
      LRegistry.Entries[LIndex].Reference := NyxWorkspace('recover.project-' +
        TNyxText(IntToStr(LIndex + 1)));
      LRegistry.Entries[LIndex].LabelText := 'Workspace ' + TNyxText(IntToStr(LIndex + 1));
      LRegistry.Entries[LIndex].Session := LFrame;
      LRegistry.Entries[LIndex].Session.Revision := 18 + LIndex;
    end;
    { Copy the last history array before replacing it: record assignment shares
      dynamic arrays on FPC. All preceding projects must remain independently
      valid before the late history execution failure is introduced. }
    LRegistry.Entries[7].Session.Session.Redo := Copy(LFrame.Session.Redo);
    LRegistry.Entries[7].Session.Session.Redo[0] := NyxStudioCheckpoint(LSecond,
      CRedoDraft, CRedoBase, True);
    LWorkspaces := TNyxStudioWorkspaces.CreateRecovered(LPrimary, LRegistry);
    LStore := TNyxStudioRuntimeStore.Create(LDirectories);
    LStore.Save(LPrimary, LWorkspaces);
    LBefore := ReadBytes(LDirectories.SessionCheckpoint);
    LRefused := False;
    try
      LStore.Load(LRecovered, LRecoveredWorkspaces);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRecovered = nil) and (LRecoveredWorkspaces = nil),
      'strict compiler-independent recovery refuses executed history without partial owners');

    LCounted := Counted(LVerifier, LCounter);
    Check(LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted),
      'actual native compilation admits all nine projects and every paired history entry');
    Check(LCounter.Calls = 2, 'exact source repeats compile once per load across all sessions/history');
    Check((LRecoveredWorkspaces.RecoveryFrame.Identity = LRegistry.Identity) and
      (LRecoveredWorkspaces.RecoveryFrame.Serial = 8), 'registry identity/serial survive fresh admission');
    LStore.Save(LRecovered, LRecoveredWorkspaces);
    Check(SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'complete current/history/draft/navigation/revisions re-save byte exactly');
    for LIndex := -1 to 7 do
    begin
      LSession := LRecovered;

      if LIndex >= 0 then
      begin
        LSession := LRecoveredWorkspaces.Resolve(LRegistry.Entries[LIndex].Reference);
      end;
      Check(EncodeNyxProject(LSession.RecoveryFrame.Session.Pair) =
        EncodeNyxProject(LFrame.Session.Pair), 'every current pair retains exact unfinished Unicode text');
      Traverse(LSession, 'undo');
      Check((LSession.RecoveryFrame.Session.Pair.Draft = CUndoDraft) and
        (LSession.RecoveryFrame.Session.Pair.DraftBase = CUndoBase),
        'every restored Undo executes its exact accepted/draft/base checkpoint');
      Traverse(LSession, 'redo');
      Check(EncodeNyxProject(LSession.RecoveryFrame.Session.Pair) =
        EncodeNyxProject(LFrame.Session.Pair), 'paired Redo restores the complete current buffer');
      Traverse(LSession, 'redo');
      Check((LSession.RecoveryFrame.Session.Pair.Draft = CRedoDraft) and
        (((LIndex = 7) and (LSession.RecoveryFrame.Session.Pair.Source = LSecond.Source)) or
          ((LIndex < 7) and (LSession.RecoveryFrame.Session.Pair.Source = LFirst.Source))),
        'older Redo retains its independently verified accepted Pascal and unfinished buffer');
    end;
    FreeAndNil(LRecoveredWorkspaces);
    FreeAndNil(LRecovered);

    { Deferred admission uses actual independent native constructor results to
      qualify staging/ownership. It does not claim browser worker execution. }
    Check(LStore.Stage(btBrowser), 'browser recovery stages without executing any application source');
    LStage := LStore.Staged;
    Check((LStage.SessionCount = 9) and (LStage.SourceCount = 2) and
      (LStage.AcceptedCount = 0), 'bounded stage deduplicates units across all current and history pairs');
    Check(LStage.Source(0) = LSource, 'stage exposes only the exact requested accepted unit');
    LProjection := LVerifier.Verify(LSource);
    LRefused := False;
    try
      LStage.Accept(0, LProjection);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStage.AcceptedCount = 0), 'native execution cannot qualify staged browser recovery');
    LRefused := False;
    try
      LStage.Complete(LRecovered, LRecoveredWorkspaces);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRecovered = nil) and (LRecoveredWorkspaces = nil),
      'incomplete staged execution cannot publish partial owners');
    LRefused := False;
    try
      LStore.Save(LPrimary, LWorkspaces);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'staged recovery refuses replacing retained bytes with another live registry');
    LStage.Cancel;
    Check(LStage.State = rrsCancelled, 'cancel retires staged execution without admitting projects');
    LCorrupt := Copy(LBefore);
    LCorrupt[High(LCorrupt)] := LCorrupt[High(LCorrupt)] xor 1;
    WriteBytes(LDirectories.SessionCheckpoint, LCorrupt);
    LRefused := False;
    try
      LStore.Stage(btNativeLCL);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Staged.State = rrsCancelled),
      'failed retry retains its cancelled guard instead of enabling checkpoint overwrite');
    LRefused := False;
    try
      LStore.Save(LPrimary, LWorkspaces);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and SameBytes(LCorrupt, ReadBytes(LDirectories.SessionCheckpoint)),
      'cancelled recovery and corrupt retry still preserve retained input');
    WriteBytes(LDirectories.SessionCheckpoint, LBefore);
    Check(LStore.Stage(btNativeLCL), 'explicit retry reads and validates the entire checkpoint again');
    LStage := LStore.Staged;
    LStage.Accept(0, LProjection);
    LRefused := False;
    try
      LStage.Accept(0, LProjection);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStage.AcceptedCount = 1), 'a unit cannot be accepted twice or out of order');
    LStage.Accept(1, LVerifier.Verify(LChangedSource));
    LStage.Complete(LRecovered, LRecoveredWorkspaces);
    Check((LStage.State = rrsPublished) and
      (EncodeNyxProject(LRecovered.RecoveryFrame.Session.Pair) = EncodeNyxProject(LFrame.Session.Pair)) and
      (LRecoveredWorkspaces.RecoveryFrame.Identity = LRegistry.Identity),
      'deferred actual results publish the entire exact registry together');
    LStore.Save(LRecovered, LRecoveredWorkspaces);
    Check(SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'deferred history and unfinished buffers re-save byte exactly');
    FreeAndNil(LRecoveredWorkspaces);
    FreeAndNil(LRecovered);
    LRefused := False;
    try
      LStage.Complete(LRecovered, LRecoveredWorkspaces);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRecovered = nil) and (LRecoveredWorkspaces = nil),
      'published staged owners cannot be transferred twice');
    FreeAndNil(LStore);
    LStore := TNyxStudioRuntimeStore.Create(LDirectories);

    { Late actual constructor failure must discard all preceding candidates. }
    WriteBytes(LFaultPath, NyxEncodeUTF8('fail'));
    LCounted := Counted(LVerifier, LCounter);
    LRefused := False;
    try
      LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCounter.Calls = 2) and
      (LRecovered = nil) and (LRecoveredWorkspaces = nil),
      'actual failure in the last project Redo returns no partially recovered registry');
    Check(SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'late execution refusal retains committed bytes and existing live owners');
    DeleteFile(LFaultPath);

    LCorrupt := Copy(LBefore);
    LCorrupt[High(LCorrupt)] := LCorrupt[High(LCorrupt)] xor 1;
    WriteBytes(LDirectories.SessionCheckpoint, LCorrupt);
    LCounted := Counted(LVerifier, LCounter);
    LRefused := False;
    try
      LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCounter.Calls = 0),
      'complete digest corruption refuses before any application execution');
    LCorrupt := ReplaceLastByte(LBefore, 'recover.project-8', Ord('1'));
    WriteBytes(LDirectories.SessionCheckpoint, LCorrupt);
    LRefused := False;
    try
      LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCounter.Calls = 0),
      'checksum-valid duplicated late handle refuses before any execution');
    LDesignPattern := Copy(ExpectedNyxProjectionDesign, 1,
      Pos('Handwritten notebook', ExpectedNyxProjectionDesign) +
        Length('Handwritten notebook') - 1);
    LCorrupt := ReplaceLastByte(LBefore, LDesignPattern, Ord('X'));
    WriteBytes(LDirectories.SessionCheckpoint, LCorrupt);
    LRefused := False;
    try
      LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCounter.Calls = 2) and
      (LRecovered = nil) and (LRecoveredWorkspaces = nil),
      'canonical late history design divergence refuses the entire executed recovery');
    Check(SameBytes(LCorrupt, ReadBytes(LDirectories.SessionCheckpoint)),
      'compiled meaning mismatch retains exact divergent input for explicit recovery');
    WriteBytes(LDirectories.SessionCheckpoint, LBefore);

    LCancellation := NewNyxBuildCancellation;
    LCounted := Counted(NewNyxNativeRuntimeSourceVerifier(LDirectories,
      LProfile.Encode, TNyxCompilerLimits.Default, LCancellation, spcChecked), LCounter);
    LCounter.CancelAfter := 1;
    LCounter.Cancellation := LCancellation;
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
      'cancellation after actual construction refuses cached pairs and whole publication');
    Check(SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'cancelled restoration preserves the complete committed checkpoint');
    FreeAndNil(LStore);

    { The real owning backend consumes the same recovery strategy before its
      suspended listener/enrollment setup. No HTTP server is started here. }
    LCounted := Counted(LVerifier, LCounter);
    LEngine := TNyxStudioMCP.Create(LDirectories, 8674, 8675, LProfile.Encode, LCounted);
    LObserved := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('after', NyxData(0))]));
    Check(LCounter.Calls = 2, 'ordinary backend construction freshly verifies all saved source');
    Check(LObserved.Field('state').Field('project').AsText =
      EncodeNyxProject(LFrame.Session.Pair), 'ordinary backend observer sees the recovered exact pair');
    Check(SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'backend startup/connection retains complete recovery bytes');
    FreeAndNil(LEngine);

    { Real backend deferred startup is separate from native constructor success.
      Actual pas2js compilation yields a compiled-only receipt here; no worker
      is simulated and no HTTP listener is started by this suspended owner. }
    LEngine := TNyxStudioMCP.Create(LDirectories, 8674, 8675, LProfile.Encode,
      nil, rrmBrowserWorker);
    LObserved := LEngine.ConnectRecovery;
    LToken := LObserved.Field('token').AsText;
    LProgress := LObserved.Field('recovery');
    Check(LProgress.Field('pending').AsBoolean and
      (LProgress.Field('sessions').AsInteger = 9) and (LProgress.Field('units').AsInteger = 2),
      'backend starts deferred recovery without requiring an application compiler');
    Check(LObserved.Count = 2, 'startup metadata exposes no replacement design or pending drafts');
    LRefused := False;
    try
      LEngine.InvokeTool('nyx_session', 'owned-agent', 'Owned agent', NyxObject([]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'agents cannot inspect or modify a substitute primary while recovery is pending');
    LRefused := False;
    try
      LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
        NyxField('after', NyxData(0))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'ordinary editor claim cannot overwrite a pending runtime recovery');
    LRefused := False;
    try
      LEngine.RecoveryExchange('wrong-capability',
        NyxObject([NyxField('mode', NyxData('unit')), NyxField('unit', NyxData(0))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'editor or arbitrary capabilities cannot read retained recovery source');
    LObserved := LEngine.RecoveryExchange(LToken,
      NyxObject([NyxField('mode', NyxData('unit')), NyxField('unit', NyxData(0))]));
    Check((LObserved.Field('source').AsText = LSource) and (LObserved.Count = 2),
      'recovery source query reads one exact unique unit');
    LProfile.SetField('pas2js', LDirectories.RuntimeRoot + 'missing-pas2js.exe');
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LEngine.ConfigureOutputs(LProfile.Encode);
    LObserved := LEngine.RecoveryExchange(LToken, NyxObject([
      NyxField('mode', NyxData('request')), NyxField('unit', NyxData(0)),
      NyxField('operationId', NyxData('missing-recovery-compiler'))]));
    LJob := LObserved.Field('job').AsText;
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
    LBuild := DecodeNyxBrowserSourceBuild(LSource, LStatus.Field('receipt'));
    Check((LStatus.Field('state').AsText = 'failed') and
      (LBuild.Projection.State = spsUnavailable), 'missing compiler has a typed unavailable recovery result');
    LProgress := LEngine.RecoveryExchange(LToken, NyxObject([NyxField('mode', NyxData('status'))]));
    Check(LProgress.Field('pending').AsBoolean and (LProgress.Field('accepted').AsInteger = 0) and
      SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'missing tools keep the complete registry retained and unadmitted');
    LEngine.RecoveryExchange(LToken, NyxObject([NyxField('mode', NyxData('cancel'))]));
    LObserved := LEngine.RecoveryExchange(LToken, NyxObject([NyxField('mode', NyxData('retry'))]));
    LToken := LObserved.Field('token').AsText;
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LEngine.ConfigureOutputs(LProfile.Encode);
    LRequest := NyxObject([NyxField('mode', NyxData('request')),
      NyxField('unit', NyxData(0)), NyxField('operationId', NyxData('owned-recovery-unit-0'))]);
    LObserved := LEngine.RecoveryExchange(LToken, LRequest);
    LJob := LObserved.Field('job').AsText;
    LObserved := LEngine.RecoveryExchange(LToken, LRequest);
    Check(LObserved.Field('job').AsText = LJob, 'exact recovery compile retry reuses its owning job');
    LRefused := False;
    try
      LEngine.RecoveryExchange(LToken, NyxObject([
        NyxField('mode', NyxData('request')), NyxField('unit', NyxData(0)),
        NyxField('operationId', NyxData('substituted-recovery-unit')),
        NyxField('source', NyxData(LChangedSource))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'client source cannot substitute the exact staged compiler unit');
    LRefused := False;
    try
      LEngine.RecoveryExchange(LToken, NyxObject([
        NyxField('mode', NyxData('job')), NyxField('job', NyxData('another-job'))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'another compiler job cannot supply recovery execution authority');
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
    Check(LStatus.Field('state').AsText = 'succeeded', 'actual queued pas2js recovery compiler joins successfully');
    LBuild := DecodeNyxBrowserSourceBuild(LSource, LStatus.Field('receipt'));
    Check(LBuild.Projection.State = spsCompiled, 'server compiler cannot advertise browser execution');
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
    LProgress := LEngine.RecoveryExchange(LToken, NyxObject([NyxField('mode', NyxData('status'))]));
    Check(LRefused and (LProgress.Field('accepted').AsInteger = 0),
      'compiled receipt and malformed worker result cannot advance recovery');
    LProgress := LEngine.RecoveryExchange(LToken, NyxObject([NyxField('mode', NyxData('cancel'))]));
    Check(LProgress.Field('state').AsText = 'cancelled', 'operator cancellation retires the complete startup attempt');
    LPreviousToken := LToken;
    LObserved := LEngine.RecoveryExchange(LToken, NyxObject([NyxField('mode', NyxData('retry'))]));
    LToken := LObserved.Field('token').AsText;
    Check((LToken <> LPreviousToken) and
      (LObserved.Field('recovery').Field('accepted').AsInteger = 0),
      'joined retry rotates its capability and reloads retained inputs');
    LRefused := False;
    try
      LEngine.RecoveryExchange(LPreviousToken, NyxObject([NyxField('mode', NyxData('status'))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'retired browser authority stays revoked and original registry bytes stay exact');
    FreeAndNil(LEngine);

    LStore := TNyxStudioRuntimeStore.Create(LDirectories);
    LCounted := Counted(LVerifier, LCounter);
    LCounter.ChangeSchemaAfter := 2;
    LRefused := False;
    try
      LStore.Load(LRecovered, LRecoveredWorkspaces, LCounted);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRecovered = nil) and (LRecoveredWorkspaces = nil),
      'creator generation changed during real compilation refuses final whole-registry publication');
    Check(SameBytes(LBefore, ReadBytes(LDirectories.SessionCheckpoint)),
      'schema retirement leaves complete original bytes intact');
    WriteLn('PASS ', GChecks, ' actual compiled nine-session/history recovery checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  LEngine.Free;
  LStore.Free;
  LRecoveredWorkspaces.Free;
  LRecovered.Free;
  LWorkspaces.Free;
  LPrimary.Free;
  LProfile.Free;
  LCounted := nil;
  LVerifier := nil;
  LCancellation := nil;
end.
