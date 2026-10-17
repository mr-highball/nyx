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
unit nyx.test.projectqueues;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.directories, nyx.studio.projectinputs;

{ Existing actual-runtime fixture supplies an already executed captured project.
  Exercise normal Open/Apply/visual commands on independent native owners. This
  creates no listener, enrollment, browser, user project or alternative fixture. }
function RunNyxProjectQueueChecks(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const AProject: INyxPascalProjectBuild): Integer;

{ Exercise the production compiler pool directly without an HTTP host or shared
  registry adoption. Only the supplied actual producer seeds this private pair;
  service decoding retains captured inputs but grants no publication authority. }
function RunNyxProjectServiceChecks(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const AProject: INyxPascalProjectBuild): Integer;

implementation

uses Classes, SysUtils, nyx.model, nyx.pascal.project, nyx.studio.projects,
  nyx.studio.session, nyx.studio.sourcejobs, nyx.studio.sourcecompilation,
  nyx.studio.sourcecompilation.native, nyx.studio.buildexecutor, nyx.data,
  nyx.source, nyx.studio.buildjobs, nyx.studio.builds, nyx.studio.sourcebuilds,
  nyx.studio.sourceprojection, nyx.studio.workspaces, nyx.studio.reviews;

type
  { Capability probe owns only a call counter. Returning nil records routing,
    never successful compilation; project dispatch must refuse before calling
    this legacy-only strategy or retaining any completion port. }
  TLegacyCompilerProbe = class(TInterfacedObject, INyxSourceCompiler)
  public
    Calls: Integer;
    function Start(const ASource: TNyxText;
      const APort: INyxSourceCompilationPort): INyxSourceCompilation;
  end;

function TLegacyCompilerProbe.Start(const ASource: TNyxText;
  const APort: INyxSourceCompilationPort): INyxSourceCompilation;
begin
  Inc(Calls);
  Result := nil;
end;

function RunNyxProjectQueueChecks(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const AProject: INyxPascalProjectBuild): Integer;
const
  CUnfinished: TNyxText = '{ A separate unfinished idea 🚀';
  CNewer: TNyxText = '{ A newer idea while compilation runs 𐐷';
  CTitle: TNyxText = 'A composed project workshop';
var
  LChecks: Integer;
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LPair: TNyxProjectPair;
  LBefore: TNyxProjectPair;
  LSource: TNyxText;
  LSourceEdit: TNyxText;
  LBroken: TNyxText;
  LEdit: TNyxStudioDesignEdit;
  LFiles: TNyxPascalProjectFiles;
  LLegacy: TLegacyCompilerProbe;
  LLegacyOwner: INyxSourceCompiler;
  LRefused: Boolean;
  LAt: Integer;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin

      if LCommands <> nil then
      begin
        raise Exception.Create(AReason + ': ' + LCommands.Message);
      end;
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

  procedure Pump;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize(1);

      if not LCommands.Busy then
      begin
        Exit;
      end;
    until GetTickCount64 - LStarted > 180000;
    raise Exception.Create('Owned project queue exceeded its compilation wait budget');
  end;

begin
  LChecks := 0;
  LCommands := nil;
  LPair := NyxProjectPair(AProject.Projection.Design, AProject.Projection.Source);
  LPair.Files := TNyxPascalProjectFiles.Capture(AProject.Inputs);
  LPair.Pending := True;
  LPair.Draft := CUnfinished;
  LPair.DraftBase := LPair.Source;
  LFiles := LPair.Files;
  LLegacy := TLegacyCompilerProbe.Create;
  LLegacyOwner := LLegacy;
  LRefused := False;
  try
    StartNyxSourceCompilation(LLegacyOwner, LPair.Source, LFiles, nil);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LLegacy.Calls = 0), 'legacy provider refuses helpers before dispatch');
  StartNyxSourceCompilation(LLegacyOwner, LPair.Source, Default(TNyxPascalProjectFiles), nil);
  Check(LLegacy.Calls = 1, 'absent files retain the original single-unit dispatch');
  LLegacyOwner := nil;

  LSession := TNyxStudioSession.Create;
  LCommands := nil;
  try
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCommands.UseCompiler(NewNyxNativeSourceCompiler(ADirectories, AProfile,
      TNyxCompilerLimits.Default));
    LCommands.OpenProject(LPair);
    Check(LCommands.Busy, 'ordinary project Open owns an actual compilation');
    Pump;
    Check(LCommands.State = nssApplied, 'normal project compiler accepts main/helper includes');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'Open retains exact helpers and an independent unfinished main buffer');

    LSource := LSession.Source;
    LSourceEdit := LSource + #10 + '{ A retained handwritten note. }' + #10;
    LSession.SetSourceDraft(LSourceEdit);
    LCommands.Apply;
    Pump;
    Check(LCommands.State = nssApplied, 'normal Pascal Apply compiles with the same helpers');
    Check((LSession.Source = LSourceEdit) and not LSession.SourceDraftPending,
      'Apply publishes the exact main source and consumes only its acknowledged draft');
    Check(LSession.ProjectSnapshot.Files.Encoded = LFiles.Encoded,
      'Apply preserves every supporting-file byte and its declared limits');
    LSession.Undo;
    Check((LSession.Source = LSource) and
      (LSession.ProjectSnapshot.Files.Encoded = LFiles.Encoded),
      'one ordinary Undo restores main and helper ownership together');
    LSession.Redo;
    Check(LSession.Source = LSourceEdit, 'ordinary Redo restores the compiled main edit');

    LSession.SetSourceDraft(CUnfinished);
    LBefore := LSession.ProjectSnapshot;
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTitle;
    LEdit.Value := CTitle;
    LCommands.Edit(LEdit);
    Pump;
    Check(LCommands.State = nssApplied, 'normal visual writer/queue executes an include-aware proposal');
    Check(LSession.Document.Title = CTitle, 'actual construction agrees with the proposed title');
    Check((LSession.ProjectSnapshot.Files.Encoded = LFiles.Encoded) and
      (LSession.Source <> LBefore.Source), 'visual continuation changes only main source');
    Check((LSession.ProjectSnapshot.Draft = CUnfinished) and
      (LSession.ProjectSnapshot.DraftBase = LBefore.DraftBase),
      'visual publication retains the independent unfinished buffer and exact older base');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one paired Undo restores the complete pre-visual project and draft');
    LSession.Redo;
    Check((LSession.Document.Title = CTitle) and
      (LSession.ProjectSnapshot.Files.Encoded = LFiles.Encoded),
      'visual Redo retains the compiled helper context');

    LSession.DiscardSourceDraft;
    LBefore := LSession.ProjectSnapshot;
    LAt := Pos('ReadProjectPlan', LBefore.Source);
    Check(LAt > 0, 'existing exact fixture supplies its include-defined plan reader');
    LBroken := Copy(LBefore.Source, 1, LAt - 1) + 'MissingProjectPlan' +
      Copy(LBefore.Source, LAt + Length('ReadProjectPlan'), MaxInt);
    LSession.SetSourceDraft(LBroken);
    LCommands.Apply;
    Pump;
    Check(LCommands.State = nssRejected, 'real FPC failure remains an ordinary rejected Apply');
    Check((LSession.Source = LBefore.Source) and
      (LSession.ProjectSnapshot.Files.Encoded = LFiles.Encoded) and
      (LSession.DraftSource = LBroken), 'compiler failure retains accepted files and broken draft');

    LSession.DiscardSourceDraft;
    LSession.SetSourceDraft(LBefore.Source + #10 + '{ A checked proposal. }' + #10);
    LCommands.Apply;
    LSession.SetSourceDraft(CNewer);
    Pump;
    Check(LCommands.State = nssStale, 'typing after dispatch makes actual compiler completion stale');
    Check((LSession.Source = LBefore.Source) and (LSession.DraftSource = CNewer) and
      (LSession.ProjectSnapshot.Files.Encoded = LFiles.Encoded),
      'stale completion preserves newer text and all accepted helper data');

    LSession.DiscardSourceDraft;
    LSession.SetSourceDraft(LBefore.Source + #10 + '{ A cancelled proposal. }' + #10);
    LCommands.Apply;
    LCommands.Cancel;
    Check(LCommands.State = nssCancelled, 'ordinary cancellation revokes queued project publication');
    Check((LSession.Source = LBefore.Source) and
      (LSession.ProjectSnapshot.Files.Encoded = LFiles.Encoded),
      'cancellation cannot replace the accepted main or supporting inventory');
  finally
    { Controller retirement joins producers and detaches delivery before the
      session is released. Its already posted UI courier retires on the next
      real RTL turn, just as in a running editor. Qualify that detached turn
      after releasing both owners; no global compiler/process is stopped. }
    FreeAndNil(LCommands);
    FreeAndNil(LSession);
    CheckSynchronize(1);
  end;
  Result := LChecks;
end;

function RunNyxProjectServiceChecks(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const AProject: INyxPascalProjectBuild): Integer;
const
  COwner: TNyxText = 'owned-project-compiler';
var
  LJobs: TNyxBuildJobs;
  LFiles: TNyxPascalProjectFiles;
  LChangedFiles: TNyxPascalProjectFiles;
  LPair: TNyxProjectPair;
  LResultPair: TNyxProjectPair;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LRetry: TNyxDataValue;
  LBuild: INyxSourceProjectionBuild;
  LProjectBuild: INyxPascalProjectBuild;
  LProjectProjection: INyxPascalProjectProjection;
  LTarget: TNyxBuildTarget;
  LInput: INyxPascalInput;
  LChanged: INyxPascalProject;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LFrame: TNyxSourceCheckpoint;
  LCurrent: Boolean;
  LRefused: Boolean;
  LChecks: Integer;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

  function Arguments(const AOperation: TNyxText;
    ATarget: TNyxBuildTarget): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('request')),
      NyxField('operationId', NyxData(AOperation)),
      NyxField('expectedRevision', NyxData(1)),
      NyxField('source', NyxData(AProject.Inputs.Source)),
      NyxField('target', NyxData(NyxBuildTargetName(ATarget)))]);
  end;

  function WaitSource(const AJob: TNyxText): TNyxDataValue;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Result := LJobs.SourceStatus(AJob, NyxPrimaryWorkspace, COwner);

      if NyxBuildJobTerminal(ParseNyxBuildJobState(Result.Field('state').AsText)) then
      begin
        Exit;
      end;
      Sleep(5);
    until GetTickCount64 - LStarted > 180000;
    raise Exception.Create('Owned project service exceeded its compilation wait budget');
  end;

  function WaitApplication(const AJob: TNyxText): TNyxDataValue;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Result := LJobs.Status(NyxObject([NyxField('mode', NyxData('status')),
        NyxField('job', NyxData(AJob))]), LResultPair, LCurrent);

      if NyxBuildJobTerminal(ParseNyxBuildJobState(Result.Field('state').AsText)) then
      begin
        Exit;
      end;
      Sleep(5);
    until GetTickCount64 - LStarted > 180000;
    raise Exception.Create('Owned project application exceeded its compilation wait budget');
  end;

begin
  LChecks := 0;
  LFiles := TNyxPascalProjectFiles.Capture(AProject.Inputs);
  LPair := NyxProjectPair(AProject.Projection.Design, AProject.Inputs.Source);
  LPair.Files := LFiles;
  LInput := AProject.Inputs.Input(0);
  LChanged := AProject.Inputs;

  if LInput.Kind = npiUnit then
  begin
    LChanged := LChanged.WithUnit(LInput.UnitReference,
      LInput.Text + #10 + '{ A different captured helper. }' + #10);
  end
  else
  begin
    LChanged := LChanged.WithInclude(NyxPascalInclude(LInput.Name),
      LInput.Text + #10 + '{ A different captured helper. }' + #10);
  end;
  LChangedFiles := TNyxPascalProjectFiles.Capture(LChanged);
  LJobs := TNyxBuildJobs.Create(ADirectories, AProfile);
  LWorkspace := TNyxSourceWorkspace.Create;
  LDocument := nil;
  try
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      LArguments := WithNyxSourceProjectFiles(
        Arguments('project-' + NyxBuildTargetName(LTarget), LTarget), LFiles);
      Check(ReadNyxSourceProjectFiles(LArguments).Encoded = LFiles.Encoded,
        'private request retains exact typed supporting-file inventory');
      LReceipt := LJobs.RequestSource(LArguments, LPair, NyxPrimaryWorkspace, COwner);
      LRetry := LJobs.RequestSource(LArguments, LPair, NyxPrimaryWorkspace, COwner);
      Check(LRetry.ToJSON = LReceipt.ToJSON, 'exact active retry retains one compiler job');
      LStatus := WaitSource(LReceipt.Field('job').AsText);
      Check(LStatus.Field('state').AsText = 'succeeded',
        'production compiler pool accepts captured helpers: ' + LStatus.Field('error').AsText);

      if LTarget = btNativeLCL then
      begin
        LBuild := DecodeNyxNativeSourceBuild(LPair.Source, LStatus.Field('receipt'), LFiles);
        Check((LBuild.Projection.State = spsExecuted) and
          (LBuild.Projection.Design = LPair.Design),
          'private native decode retains actual construction and exact design');
      end
      else
      begin
        LBuild := DecodeNyxBrowserSourceBuild(LPair.Source, LStatus.Field('receipt'), LFiles);
        Check((LBuild.Projection.State = spsCompiled) and
          (LBuild.Projection.Design = ''), 'browser service result remains compiled only');
      end;
      Check(Supports(LBuild, INyxPascalProjectBuild, LProjectBuild) and
        (TNyxPascalProjectFiles.Capture(LProjectBuild.Inputs).Encoded = LFiles.Encoded),
        'detached service result retains its exact locally captured inputs');
      Check(Supports(LBuild.Projection, INyxPascalProjectProjection, LProjectProjection) and
        (LProjectProjection.Files.Encoded = LFiles.Encoded),
        'projection retains helper context independently of its service wrapper');
      LRefused := False;
      try
        LJobs.RequestSource(WithNyxSourceProjectFiles(
          Arguments('project-' + NyxBuildTargetName(LTarget), LTarget), LChangedFiles),
          LPair, NyxPrimaryWorkspace, COwner);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'reusing an operation identity with different helpers refuses');
      LRefused := False;
      try
        LJobs.SourceStatus(LReceipt.Field('job').AsText, NyxPrimaryWorkspace, 'another-owner');
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'another owner cannot poll captured project compilation');
    end;

    LArguments := WithNyxSourceProjectFiles(Arguments('cancel-project', btNativeLCL), LFiles);
    LReceipt := LJobs.RequestSource(LArguments, LPair, NyxPrimaryWorkspace, COwner);
    LJobs.SourceStatus(LReceipt.Field('job').AsText, NyxPrimaryWorkspace, COwner, True);
    LStatus := WaitSource(LReceipt.Field('job').AsText);
    Check((LStatus.Field('state').AsText = 'cancelled') and
      (LStatus.Field('receipt').Kind = ndNull), 'cancelled service work exposes no producer result');

    LDocument := AProject.Projection.CopyDocument;
    LWorkspace.AcceptExecuted(LDocument, LPair.Source, LFiles);
    LFrame := LWorkspace.Capture(LDocument);
    LArguments := NyxObject([NyxField('mode', NyxData('request')),
      NyxField('operationId', NyxData('accepted-project-application')),
      NyxField('expectedRevision', NyxData(1)),
      NyxField('outputID', NyxData(NyxBuildFingerprint(AProfile))),
      NyxField('target', NyxData(NyxBuildTargetName(btNativeLCL))),
      NyxField('scope', NyxData('application'))]);
    LReceipt := LJobs.Submit('Studio', LArguments, LPair, NyxActiveWorkspace,
      COwner, NyxPrimaryWorkspace, LFrame);
    LStatus := WaitApplication(LReceipt.Field('job').AsText);
    Check(LStatus.Field('state').AsText = 'succeeded',
      'ordinary application queue builds accepted source plus supporting files');
    Check(EncodeNyxProject(LResultPair) = EncodeNyxProject(LPair),
      'application completion retains the exact complete captured project');
    Check(LCurrent, 'application completion retains its captured output identity');
  finally
    LJobs.Free;
    LDocument.Free;
    LWorkspace.Free;
  end;
  Result := LChecks;
end;

end.
