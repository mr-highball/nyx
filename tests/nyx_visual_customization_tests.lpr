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

program nyx_visual_customization_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.codec, nyx.codegen, nyx.source, nyx.source.preparation, nyx.schema, nyx.responsive,
  nyx.controls, nyx.presentations, nyx.state, nyx.root.types, nyx.studio.edits,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.buildexecutor,
  nyx.studio.builds, nyx.studio.sourceprojection, nyx.studio.projectionediting,
  nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.native;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Visual customization: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LFile.Size);
    LFile.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure SaveText(const APath, AText: TNyxText);
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

function PropertyEdit(const AName, AValue: TNyxText): TNyxStudioDesignEdit;
begin
  Result := Default(TNyxStudioDesignEdit);
  Result.Action := sdaProperty;
  Result.Selection := 'heading-1';
  Result.View := 'notebook-1';
  Result.Name := AName;
  Result.Value := AValue;
end;

procedure Drain(ACommands: TNyxSourceCommands);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  while ACommands.Busy do
  begin
    CheckSynchronize(5);

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Actual visual compiler queue did not retire before deadline');
    end;
  end;
end;

{ Actual constructor evidence is required before the new clearing source can
  enter a session. This independent editor owns no active user or original
  qualification session; late drafts and one paired Undo/Redo remain observable. }
procedure ResetHistory(AExecutor: TNyxBuildExecutor; ABefore, AAfter: TNyxDocument;
  const ABeforeSource, AAfterSource: TNyxText; const AAfterProjection: INyxSourceProjection);
var
  LBuild: INyxSourceProjectionBuild;
  LSession: TNyxStudioSession;
  LRequest: TNyxStudioSourceRequest;
  LPrepared: INyxPreparedSource;
  LSchemas: INyxSchemaSnapshot;
  LBeforePair: TNyxProjectPair;
  LDraft: TNyxText;
  LBeforeUndo: Boolean;
  LBeforeRedo: Boolean;
begin
  LBuild := AExecutor.ProjectSource(ABeforeSource, NyxPascalUnit('nyx.clearing.fixture'),
    btNativeLCL, spcChecked);
  Check((LBuild.Projection.State = spsExecuted) and
    (LBuild.Projection.Design = TNyxCodec.Encode(ABefore)),
    'clearing history has independently executed original meaning');
  LSession := TNyxStudioSession.Create;
  try
    LBeforePair := NyxProjectPair(TNyxCodec.Encode(ABefore), ABeforeSource);
    LSession.AdoptProjectedProject(LBeforePair, LBuild.Projection);
    LBeforeUndo := LSession.CanUndo;
    LBeforeRedo := LSession.CanRedo;
    LSession.SetSourceDraft(AAfterSource);
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LPrepared := PrepareNyxProjectedSource(AAfterProjection, LSchemas);
    LDraft := AAfterSource + TNyxText(#10 + '{ An unfinished clearing idea 🚀 }');
    LSession.SetSourceDraft(LDraft);
    Check((LSession.CompleteSourceRequest(LRequest, LPrepared) = nscStale) and
      (LSession.Source = ABeforeSource) and (LSession.DraftSource = LDraft) and
      (LSession.CanUndo = LBeforeUndo) and (LSession.CanRedo = LBeforeRedo),
      'late clearing completion preserves the independent draft, pair and existing history');
    LSession.SetSourceDraft(AAfterSource);
    Check((LSession.CompleteSourceRequest(LRequest, LPrepared) = nscApplied) and
      (LSession.Source = AAfterSource) and (LSession.Save = TNyxCodec.Encode(AAfter)),
      'executed clearing publishes the exact source/design together');
    LSession.Undo;
    Check((LSession.Source = ABeforeSource) and (LSession.Save = LBeforePair.Design) and
      (LSession.CanUndo = LBeforeUndo),
      'one clearing Undo restores the complete original accepted pair and prior Undo availability');
    LSession.Redo;
    Check((LSession.Source = AAfterSource) and (LSession.Save = TNyxCodec.Encode(AAfter)),
      'one clearing Redo restores the exact executed pair');
  finally
    LSession.Free;
  end;
end;

{ Clear all supported scope families through the same source writer. Named
  Unicode presentations are qualification input, not English starter content.
  Empty text must remain explicitly authored after a subsequent edit. }
procedure ScopedResets(AExecutor: TNyxBuildExecutor);
const
  CPresentation: TNyxText = 'Focused 🚀';
var
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LEmpty: TNyxDocument;
  LPage: INyxPage;
  LHeading: INyxHeading;
  LSource: TNyxText;
  LCleared: TNyxText;
  LEmptySource: TNyxText;
  LCanonical: TNyxText;
  LBuild: INyxSourceProjectionBuild;
  LRejected: Boolean;
begin
  LBefore := TNyxDocument.Create;
  LAfter := nil;
  LEmpty := nil;
  try
    LBefore.Presentations.Define(NyxPresentation(CPresentation), TNyxPresentationCondition.Manual);
    LPage := NewNyxPage('home');
    LBefore.AddPage(LPage);
    LHeading := NewNyxHeading('review-title');
    LHeading.Configure.Clear(atText).Done;
    Check((LHeading.Node.Props.IndexOfName('text') >= 0) and
      (LHeading.Node.Prop('text') = ''), 'existing Clear retains an explicit empty property');
    LHeading.Configure.Reset(atText).Done;
    Check(LHeading.Node.Props.IndexOfName('text') < 0,
      'managed typed Reset removes that authored property');
    LHeading.Configure.Text('Review your next idea').Padding(19).Done;
    LHeading.Configure.ForPlatform(npfNativeLCL).Padding(31).Done;
    LHeading.Configure.WhenViewport(TNyxViewportWidth.Below(600))
      .ForPlatform(npfBrowser).Visible(False).Done;
    LHeading.Configure.WhenPresentation(NyxPresentation(CPresentation))
      .ForPlatform(npfBrowser).Gap(7).Done;
    LPage.Add(LHeading);
    LSource := TNyxCodegen.Generate(LBefore, 'nyx.clearing.fixture');
    LAfter := LBefore.Clone;
    LAfter.Find('review-title').Configure.Reset(atText).Reset(atPadding).Done;
    LAfter.Find('review-title').Configure.ForPlatform(npfNativeLCL).Reset(atPadding).Done;
    LAfter.Find('review-title').Configure.WhenViewport(TNyxViewportWidth.Below(600))
      .ForPlatform(npfBrowser).Reset(atVisible).Done;
    LAfter.Find('review-title').Configure.WhenPresentation(NyxPresentation(CPresentation))
      .ForPlatform(npfBrowser).Reset(atGap).Done;
    LCleared := CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    Check((Pos('.Reset(atText)', LCleared) > 0) and
      (Pos('.Reset(atPadding)', LCleared) > 0) and
      (Pos('.Reset(atVisible)', LCleared) > 0) and
      (Pos('.Reset(atGap)', LCleared) > 0),
      'all missing properties emit typed Reset rather than empty setters');
    Check((Pos('.ForPlatform(npfNativeLCL)', LCleared) > 0) and
      (Pos('.WhenViewport(', LCleared) > 0) and
      (Pos('.WhenPresentation(NyxPresentation(', LCleared) > 0),
      'clearing retains native, viewport and named presentation scopes');
    LBuild := AExecutor.ProjectSource(LCleared, NyxPascalUnit('nyx.clearing.fixture'),
      btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = TNyxCodec.Encode(LAfter)),
      'actual native scoped clearing reproduces complete proposed meaning');
    ResetHistory(AExecutor, LBefore, LAfter, LSource, LCleared, LBuild.Projection);
    LBuild := AExecutor.ProjectSource(LCleared, NyxPascalUnit('nyx.clearing.fixture'), btBrowser);
    Check(LBuild.Projection.State = spsCompiled,
      'same complete scoped clearing unit compiles for pas2js');
    LEmpty := LAfter.Clone;
    LEmpty.Find('review-title').Configure.Text('').Done;
    LEmptySource := CustomizeNyxExecutedSource(LCleared, LAfter, LEmpty);
    Check(Pos('.Text('''')', LEmptySource) > 0,
      'subsequent explicit empty text replaces the prior Reset with a typed setter');
    LBuild := AExecutor.ProjectSource(LEmptySource, NyxPascalUnit('nyx.clearing.fixture'),
      btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = TNyxCodec.Encode(LEmpty)) and
      (LEmpty.Find('review-title').Props.IndexOfName('text') >= 0),
      'actual merged empty value remains distinct from absent text');
    Check(CustomizeNyxExecutedSource(LEmptySource, LEmpty, LEmpty) = LEmptySource,
      'no-op after merged clearing retains exact source bytes');
    LEmpty.Find('review-title').Configure.Extension('creator-marker', 'Retain creator data').Done;
    FreeAndNil(LAfter);
    LAfter := LEmpty.Clone;
    LAfter.Find('review-title').Configure.Reset(atText).Done;
    LAfter.Find('review-title').Props.Delete(
      LAfter.Find('review-title').Props.IndexOfName('creator-marker'));
    LCanonical := TNyxCodec.Encode(LEmpty);
    LRejected := False;
    try
      CustomizeNyxExecutedSource(TNyxCodegen.Generate(LEmpty, 'nyx.clearing.fixture'),
        LEmpty, LAfter);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LEmpty) = LCanonical),
      'unknown extension removal refuses the entire mixed clearing proposal');
  finally
    LEmpty.Free;
    LAfter.Free;
    LBefore.Free;
  end;
end;

{$I nyx.test.visual.tree.inc}

procedure Run;
const
  CCaption: TNyxText = 'A crafted heading 🚀 𐐷 é';
var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LTools: TNyxDataValue;
  LExecutor: TNyxBuildExecutor;
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LCompiler: INyxSourceCompiler;
  LBuild: INyxSourceProjectionBuild;
  LOriginal: INyxSourceProjection;
  LSchemas: INyxSchemaSnapshot;
  LRequest: TNyxStudioDesignRequest;
  LProposal: INyxPreparedDesign;
  LCompleted: INyxPreparedDesign;
  LSource: TNyxText;
  LExpected: TNyxText;
  LAfterSource: TNyxText;
  LQueueSource: TNyxText;
  LNativeSource: TNyxText;
  LMarker: TNyxText;
  LInsertion: Integer;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LRejected: Boolean;
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
begin

  if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, local toolchain JSON and a NEW owned runtime home');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
  ForceDirectories(LDirectories.RuntimeRoot + 'web');
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LProfile := TNyxOutputConfiguration.Create;
  LExecutor := nil;
  LSession := nil;
  LCommands := nil;
  LDocument := nil;
  LWorkspace := nil;
  try
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LSource := ReadText(LDirectories.SourceRoot + 'tests/fixtures/nyx.projection.fixture.pas');
    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecuted, 'actual original Pascal executes');
    LOriginal := LBuild.Projection;
    LExpected := LOriginal.Design;
    Check(LOriginal.Source = LSource, 'executed receipt retains submitted source exactly');
    SaveText(LDirectories.RuntimeRoot + 'submitted.pas', LSource);
    SaveText(LDirectories.RuntimeRoot + 'project-roundtrip.pas',
      DecodeNyxProject(EncodeNyxProject(NyxProjectPair(LExpected, LSource))).Source);
    Check(DecodeNyxProject(EncodeNyxProject(NyxProjectPair(LExpected, LSource))).Source = LSource,
      'qualification source retains exact project wire bytes');
    LSchemas := CaptureNyxSchemas;
    LSession := TNyxStudioSession.Create;
    LSession.AdoptProjectedProject(NyxProjectPair(LExpected, LSource), LOriginal);
    LSession.Activate('notebook-1');
    LSession.Select('heading-1');
    LRequest := LSession.PrepareDesignRequest(PropertyEdit('text', CCaption), LSchemas.Revision);
    Check(LRequest.RequiresExecution, 'visual request retains opaque executed admission');
    LProposal := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LProposal.Diagnostic.Defined, 'detached semantic edit produces a proposal: ' +
      LProposal.Diagnostic.Message);
    Check(LProposal.RequiresCompilation, 'proposal requires actual execution');
    Check(Pos('LHeading1Heading: INyxHeading;', LProposal.Source) > 0,
      'customization uses a purpose/control specialized interface');
    Check(Pos('Padding(8 + LIndex)', LProposal.Source) > 0,
      'unedited arithmetic expression remains exact');
    Check(Pos('Text(PageName(LIndex))', LProposal.Source) > 0,
      'unedited helper expression remains exact');
    Check(Pos('class function TNotebookCards.Definition', LProposal.Source) > 0,
      'handwritten helper implementation remains present');
    Check((LSession.Source = LSource) and (LSession.Save = LExpected),
      'proposal cannot change the live pair');
    LRejected := False;
    try
      LProposal.Take(LDocument, LWorkspace);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LDocument = nil) and (LWorkspace = nil),
      'proposal refuses transfer before actual execution');
    LRejected := False;
    try
      LRequest.ToData;
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'opaque execution authority cannot use literal worker transport');
    SaveText(LDirectories.RuntimeRoot + 'web/customized.pas', LProposal.Source);
    LBuild := LExecutor.ProjectSource(LProposal.Source, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    SaveText(LDirectories.RuntimeRoot + 'customization-compiler.json', LBuild.Projection.Report.Encode);
    Check(LBuild.Projection.State = spsExecuted, 'actual customization compiles and executes');
    LCompleted := PrepareNyxCompiledDesign(LRequest, LProposal, LOriginal, LSchemas);
    Check(LCompleted.Diagnostic.Defined, 'different actual producer source refuses');
    LCompleted := PrepareNyxCompiledDesign(LRequest, LProposal, LBuild.Projection, LSchemas);
    Check(not LCompleted.Diagnostic.Defined and not LCompleted.RequiresCompilation,
      'exact actual design supplies independent publishable owners');
    LSession.SetSourceDraft(LSource + #10 + '{ Unfinished handwritten work }');
    Check(LSession.CompleteDesignRequest(LRequest, LCompleted) = nscStale,
      'newer draft makes the whole completion stale');
    Check(LSession.Source = LSource, 'stale compilation retains accepted Pascal');
    LSession.DiscardSourceDraft;
    Check(LSession.CompleteDesignRequest(LRequest, LCompleted) = nscApplied,
      'fresh exact completion publishes as one paired command');
    LAfterSource := LSession.Source;
    Check(LSession.Document.Find('heading-1').Prop('text') = CCaption,
      'supplementary Unicode reaches the authored control');
    Check(LSession.Document.Find('notebook-2').Prop('text') = 'Notebook 2',
      'unrelated computed helper still supplies its control');
    LSession.Undo;
    Check((LSession.Source = LSource) and (LSession.Save = LExpected),
      'one Undo restores exact handwritten source and design');
    LSession.Redo;
    Check(LSession.Source = LAfterSource, 'Redo restores exact executed customization');
    LSession.SetSourceDraft(LAfterSource + #10 + '{ Retained unfinished Pascal }');
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCompiler := NewNyxNativeSourceCompiler(LDirectories, LProfile.Encode, TNyxCompilerLimits.Default);
    LCommands.UseCompiler(LCompiler);
    LCommands.Edit(PropertyEdit('text', 'A second crafted heading'));
    LCommands.Edit(PropertyEdit('padding', '16'));
    Check(LCommands.Busy, 'ordinary FIFO retains compiler-dependent design edits');
    Drain(LCommands);
    Check(LCommands.State = nssApplied, 'ordinary real compiler queue publishes: ' + LCommands.Message);
    Check((LSession.Document.Find('heading-1').Prop('text') = 'A second crafted heading') and
      (LSession.Document.Find('heading-1').Prop('padding') = '16'),
      'both queued edits use freshly compiled baselines');
    LQueueSource := LSession.Source;
    Check(LSession.DraftSource = LAfterSource + #10 + '{ Retained unfinished Pascal }',
      'queued visual edits preserve the existing unfinished buffer exactly');
    Check(LSession.SourceDraftBase = LAfterSource,
      'queued visual edits retain the draft original baseline');
    Check(Pos('function BuildNyxOriginalDocument', LQueueSource) =
      Pos('function BuildNyxOriginalDocument', LAfterSource),
      'original builder remains at its retained location');
    Check(Pos(CCaption, LQueueSource) = 0, 'repeated property edit replaces its prior override');
    LSession.Undo;
    Check(LSession.Document.Find('heading-1').Prop('padding') = '',
      'one Undo removes only the second queued property');
    LSession.Undo;
    Check(LSession.Source = LAfterSource, 'second Undo restores the earlier exact customization');
    Check(LSession.SourceDraftPending, 'paired Undo retains the unfinished Pascal buffer');
    LSession.Redo;
    LSession.Redo;
    Check(LSession.Source = LQueueSource, 'queued edits retain exact paired Redo');
    LBefore := LSession.Document.Clone;
    LAfter := LBefore.Clone;
    try
      LAfter.Find('heading-1').Add(TNyxNode.Create(nkLabel, 'new-child'));
      LAfter.State.SetValue(NyxTextState('unsupported-default'), 'Independent meaning');
      LRejected := False;
      try
        CustomizeNyxExecutedSource(LQueueSource, LBefore, LAfter);
      except
        on LException: ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'tree construction mixed with unhandled state refuses the whole proposal');
    finally
      LAfter.Free;
      LBefore.Free;
    end;
    LBefore := LSession.Document.Clone;
    LAfter := LBefore.Clone;
    try
      LAfter.Find('heading-1').Configure.Enabled(False).Layout(nlRow);
      LAfter.Find('heading-1').Configure.ForPlatform(npfBrowser)
        .WhenViewport(TNyxViewportWidth.Below(600)).Visible(False);
      LAfter.Title := 'A customized notebook';
      LAfterSource := CustomizeNyxExecutedSource(LQueueSource, LBefore, LAfter);
      Check(Pos('.Layout(nlRow)', LAfterSource) > 0, 'closed layout emits an enum');
      Check(Pos('.Enabled(False)', LAfterSource) > 0, 'Boolean emits a Boolean argument');
      Check(Pos('.WhenViewport(', LAfterSource) > 0, 'viewport emits a typed fluent condition');
      LBuild := LExecutor.ProjectSource(LAfterSource, NyxPascalUnit('nyx.projection.fixture'),
        btNativeLCL, spcChecked);
      Check((LBuild.Projection.State = spsExecuted) and
        (LBuild.Projection.Design = TNyxCodec.Encode(LAfter)),
        'actual native scoped/title/Boolean/enum result is exact');
      LAfter.Find('heading-1').Configure.Reset(atText).Done;
      LAfterSource := CustomizeNyxExecutedSource(LQueueSource, LBefore, LAfter);
      SaveText(LDirectories.RuntimeRoot + 'web/cleared-handwritten.pas', LAfterSource);
      Check((Pos('.Reset(atText)', LAfterSource) > 0) and
        (Pos('Text(PageName(LIndex))', LAfterSource) > 0),
        'typed clearing preserves unrelated handwritten helper expressions');
      LBuild := LExecutor.ProjectSource(LAfterSource, NyxPascalUnit('nyx.projection.fixture'),
        btNativeLCL, spcChecked);
      Check((LBuild.Projection.State = spsExecuted) and
        (LBuild.Projection.Design = TNyxCodec.Encode(LAfter)) and
        (LAfter.Find('heading-1').Props.IndexOfName('text') < 0),
        'actual handwritten clearing removes text instead of authoring an empty value');
    finally
      LAfter.Free;
      LBefore.Free;
    end;
    ScopedResets(LExecutor);
    TreeJourney(LExecutor, LCompiler, LOriginal);
    LBuild := LExecutor.ProjectSource(LQueueSource, NyxPascalUnit('nyx.projection.fixture'), btBrowser);
    Check(LBuild.Projection.State = spsCompiled, 'same customized unit compiles for pas2js');
    Check(LBuild.Projection.Design = '', 'browser compilation does not claim runtime parity');
    { A separate native-only external-input qualification follows. FileExists is
      not a browser API and is never put in the both-target companion above.
      Only this new owned runtime marker is read/written; no executed result is
      synthesized. Every source concatenation run remains explicitly UTF-8. }
    LMarker := LDirectories.RuntimeRoot + 'visual-mismatch.marker';
    LInsertion := Pos('    LDocument.Validate;', LQueueSource);
    Check(LInsertion > 0, 'qualification builder has its exact validation point');
    LNativeSource := Copy(LQueueSource, 1, LInsertion - 1) + TNyxText(#10 + '    if FileExists(''') +
      TNyxText(StringReplace(LMarker, '''', '''''', [rfReplaceAll])) + TNyxText(''') then' + #10 +
      '    begin' + #10 + '      LDocument.Title := ''Changed external input'';' + #10 +
      '    end;' + #10) + Copy(LQueueSource, LInsertion, MaxInt);
    LBuild := LExecutor.ProjectSource(LNativeSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecuted, 'real native external-input builder establishes a baseline');
    FreeAndNil(LCommands);
    LSession.AdoptProjectedProject(NyxProjectPair(LBuild.Projection.Design, LNativeSource), LBuild.Projection);
    LRequest := LSession.PrepareDesignRequest(PropertyEdit('text', 'An exact proposal'), LSchemas.Revision);
    LProposal := PrepareNyxStudioDesign(LRequest, LSchemas);
    SaveText(LMarker, 'Changed external input');
    try
      LBuild := LExecutor.ProjectSource(LProposal.Source, NyxPascalUnit('nyx.projection.fixture'),
        btNativeLCL, spcChecked);
      Check(LBuild.Projection.State = spsExecuted, 'different real input still executes successfully');
      LCompleted := PrepareNyxCompiledDesign(LRequest, LProposal, LBuild.Projection, LSchemas);
      Check(LCompleted.Diagnostic.Defined and
        (Pos('complete proposed design', LCompleted.Diagnostic.Message) > 0),
        'same source with different whole executed meaning refuses');
      Check((LSession.Source = LNativeSource) and
        (LSession.Document.Title <> 'Changed external input'),
        'different executed result retains the accepted pair');
    finally

      if not DeleteFile(LMarker) then
      begin
        raise Exception.Create('Owned qualification marker could not retire');
      end;
    end;
  finally
    LCommands.Free;
    LSession.Free;
    LWorkspace.Free;
    LDocument.Free;
    LExecutor.Free;
    LProfile.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS visual customization ', GChecks);
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, LException.ClassName, ': ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
