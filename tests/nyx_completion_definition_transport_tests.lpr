{ nyx - MIT License, Copyright (c) 2020 mr-highball }
program nyx_completion_definition_transport_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.data, nyx.scheduler, nyx.pascal.drafts,
  nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.definition, nyx.studio.agents,
  nyx.studio.completion, nyx.studio.workspaces
  {$ifdef PAS2JS}, Web
  {$else}, nyx.studio.mcp, nyx.studio.directories, nyx.studio.outputs,
    nyx.studio.session, nyx.studio.projects{$endif};

{$ifndef PAS2JS}
type
  { Controlled provider isolates real editor/MCP transport from language binding.
    It names the actual captured main declaration and owns no compiler or SDK. }
  TDefinitionProvider = class(TInterfacedObject, INyxPascalCompletionProvider,
    INyxPascalDefinitionProvider)
  public
    Calls: Integer;
    DefinitionCalls: Integer;
    WithLocations: Boolean;
    function Complete(const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
    function ResolveDefinition(const ARequest: INyxPascalDefinitionRequest;
      const ACancellation: INyxCancellationScope = nil): INyxPascalDefinitionResult;
  end;
{$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Args(ARevision: Integer): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('complete')),
    NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('caret', NyxData(0)), NyxField('limit', NyxData(1))]);
end;

procedure Run;
var
  LOwner: TNyxAgentSession;
  LQuery, LPublicQuery, LForeignQuery: INyxPascalCompletionRequest;
  LContext, LPublicContext: TNyxAgentPascalCompletionContext;
  LBase, LResult, LFrozen, LReceived, LForeign: INyxPascalCompletionResult;
  LDefinitions: INyxPascalCompletionDefinitions;
  LLocation: TNyxPascalCompletionLocation;
  LPage, LBaseline, LPublic, LEnvelope: TNyxDataValue;
  LName, LText, LBefore: TNyxText;
  LStart, LAt: Integer;
  LRejected: Boolean;
begin
  LOwner := TNyxAgentSession.Create;
  try
    LQuery := LOwner.CaptureEditorCompletion(Args(1), LContext);
    LName := LQuery.Sources.MainUnit.Name;
    LText := LQuery.Sources.Text(TNyxPascalSourceRef.Main);
    LAt := Pos(LName, LText);
    Check(LAt > 0, 'Ordinary captured Studio source retains its exact main declaration');
    LStart := NyxTextScalarCount(Copy(LText, 1, LAt - 1));
    LBase := NewNyxPascalCompletionResult(LQuery, pcsReady, '', 0, '',
      [NyxPascalCompletion(LName, LName, 'Main declaration', pckUnit)], 1);
    LResult := NewNyxPascalCompletionDefinitions(LBase,
      [NyxPascalCompletionLocation(TNyxPascalSourceRef.Main,
        LStart, NyxTextScalarCount(LName), LName)]);
    LBefore := LOwner.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))])).ToJSON;
    LBaseline := LOwner.FinishEditorCompletion(Args(1), LContext, LQuery, LBase);
    LPage := LOwner.FinishEditorCompletion(Args(1), LContext, LQuery, LResult, LFrozen);
    Check((LFrozen <> nil) and Supports(LFrozen, INyxPascalCompletionDefinitions, LDefinitions),
      'Successful private admission publishes a detached typed result with optional definitions');
    LLocation := LDefinitions.DefinitionLocation(0);
    Check(LLocation.Available and (LLocation.Start = LStart) and (LLocation.Name = LName),
      'Typed handoff preserves the exact project definition after ordinary context admission');
    Check(LPage.ToJSON = LBaseline.ToJSON,
      'Optional navigation changes no existing semantic completion page bytes');
    Check(LBefore = LOwner.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))])).ToJSON,
      'Completion admission preserves project, source drafts, selection and paired history');
    LEnvelope := NyxStudioCompletionEnvelope(LPage, 'typed-editor', NyxPrimaryWorkspace, LFrozen);
    Check(LEnvelope.Field('page').Field('version').AsInteger = 2,
      'Private editor envelope uses the shared versioned definition codec');
    LReceived := ReadNyxStudioCompletionEnvelope(LEnvelope, 'typed-editor',
      NyxPrimaryWorkspace, 1, LQuery);
    Check(Supports(LReceived, INyxPascalCompletionDefinitions, LDefinitions) and
      LDefinitions.DefinitionLocation(0).Available and
      (LDefinitions.DefinitionLocation(0).Start = LStart),
      'The ordinary correlated client envelope receives project navigation metadata');
    LEnvelope := NyxStudioCompletionEnvelope(LBaseline, 'legacy-editor', NyxPrimaryWorkspace, LBase);
    Check(LEnvelope.ToJSON = NyxStudioCompletionEnvelope(LBaseline,
      'legacy-editor', NyxPrimaryWorkspace).ToJSON,
      'Metadata-free private handoff preserves the original whole envelope bytes');

    LPublicQuery := LOwner.CapturePascalCompletion(Args(1), LPublicContext);
    LBase := NewNyxPascalCompletionResult(LPublicQuery, pcsReady, '', 0, '',
      [NyxPascalCompletion(LName, LName, 'Main declaration', pckUnit)], 1);
    LResult := NewNyxPascalCompletionDefinitions(LBase,
      [NyxPascalCompletionLocation(TNyxPascalSourceRef.Main,
        LStart, NyxTextScalarCount(LName), LName)]);
    LBaseline := LOwner.FinishPascalCompletion(Args(1), LPublicContext, LPublicQuery, LBase);
    LPublic := LOwner.FinishPascalCompletion(Args(1), LPublicContext, LPublicQuery, LResult);
    Check(LPublic.ToJSON = LBaseline.ToJSON,
      'Public MCP semantic response remains byte-identical with project navigation present');

    LRejected := False;
    try
      LOwner.FinishEditorCompletion(Args(1), LPublicContext, LPublicQuery, LResult, LFrozen);
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected and (LFrozen = nil),
      'Agent admission cannot publish a private typed operator result');
    LRejected := False;
    try
      LOwner.FinishEditorCompletion(Args(2), LContext, LQuery, LResult, LFrozen);
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected and (LFrozen = nil),
      'Stale revision refuses before publishing any typed handoff');
    LForeignQuery := LQuery.At(TNyxPascalSourceRef.Main, 1);
    LForeign := NewNyxPascalCompletionResult(LForeignQuery, pcsReady, '', 1, '',
      [NyxPascalCompletion(LName, LName, 'Main declaration', pckUnit)], 1);
    LRejected := False;
    try
      LOwner.FinishEditorCompletion(Args(1), LContext, LQuery, LForeign, LFrozen);
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected and (LFrozen = nil),
      'A provider result for another caret cannot escape as a typed operator result');
    LRejected := False;
    try
      NyxStudioCompletionEnvelope(LPage, 'missing', NyxPrimaryWorkspace, nil);
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected, 'A missing typed result cannot fall back to a metadata-stripping private page');
  finally
    LOwner.Free;
  end;
end;

{$ifndef PAS2JS}
function TDefinitionProvider.ResolveDefinition(const ARequest: INyxPascalDefinitionRequest;
  const ACancellation: INyxCancellationScope): INyxPascalDefinitionResult;
var
  LText, LName: TNyxText;
  LAt: Integer;
begin
  Inc(DefinitionCalls);
  LText := ARequest.Sources.Text(TNyxPascalSourceRef.Main);
  LName := ARequest.Sources.MainUnit.Name;
  LAt := Pos(LName, LText);
  Result := NewNyxPascalDefinitionResult(ARequest, pdsReady, '',
    [NyxPascalDefinitionTarget(LName, 'Explicit definition operation', pckUnit,
      NyxPascalCompletionLocation(TNyxPascalSourceRef.Main,
        NyxTextScalarCount(Copy(LText, 1, LAt - 1)), NyxTextScalarCount(LName), LName))], 1);
end;

function TDefinitionProvider.Complete(
  const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
var
  LText, LName: TNyxText;
  LAt: Integer;
  LBase: INyxPascalCompletionResult;
begin
  Inc(Calls);
  LText := ARequest.Sources.Text(TNyxPascalSourceRef.Main);
  LName := ARequest.Sources.MainUnit.Name;
  LAt := Pos(LName, LText);

  if LAt = 0 then
  begin
    raise Exception.Create('Controlled provider requires its actual captured main declaration');
  end;
  LBase := NewNyxPascalCompletionResult(ARequest, pcsReady, '', ARequest.Caret, '',
    [NyxPascalCompletion(LName, LName, 'Main declaration', pckUnit)], 1);
  Result := LBase;

  if WithLocations then
  begin
    Result := NewNyxPascalCompletionDefinitions(LBase,
      [NyxPascalCompletionLocation(TNyxPascalSourceRef.Main,
        NyxTextScalarCount(Copy(LText, 1, LAt - 1)), NyxTextScalarCount(LName), LName)]);
  end;
end;

procedure NativeEditorExchange;
var
  LRuntime: TNyxText;
  LDirectories: TNyxStudioDirectories;
  LEngine: TNyxStudioMCP;
  LLocal: TNyxStudioSession;
  LProfile: TNyxOutputConfiguration;
  LProvider: TDefinitionProvider;
  LRetained: INyxPascalCompletionProvider;
  LQuery: INyxPascalCompletionRequest;
  LReceived: INyxPascalCompletionResult;
  LDefinitions: INyxPascalCompletionDefinitions;
  LClaim, LBefore, LAfter, LReply, LPacket, LPublic, LBaseline: TNyxDataValue;
  LToken, LKey, LName: TNyxText;
  LRevision, LCalls: Integer;
  LNewCaret: Integer;
  LPreview: INyxPascalSourceSnapshot;
  LRejected: Boolean;
begin

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Supply repository and a brand-new owned runtime directory');
  end;
  LRuntime := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));

  if DirectoryExists(LRuntime) or FileExists(ExcludeTrailingPathDelimiter(LRuntime)) then
  begin
    raise Exception.Create('The native definition fixture requires a new runtime directory');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(LRuntime).EnrollingProject(LRuntime);

  if not ForceDirectories(LRuntime) then
  begin
    raise Exception.Create('Cannot create the explicitly owned definition fixture runtime');
  end;
  Check(not FileExists(LRuntime + '.local' + PathDelim + 'codex-mcp-registration.json'),
    'New fixture enrollment has no registration capable of refreshing user configuration');
  LEngine := nil;
  LLocal := TNyxStudioSession.Create;
  LProfile := TNyxOutputConfiguration.Create;
  try
    { The normal suspended constructor writes only this new fixture enrollment.
      Start is never called; no HTTP listener, compiler or SDK capture executes. }
    LEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763, LProfile.Encode);
    LProvider := TDefinitionProvider.Create;
    LRetained := LProvider;
    LProvider.WithLocations := True;
    LEngine.ConfigureCompletionProvider(LRetained);
    LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LLocal.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LToken := LClaim.Field('token').AsText;
    LRevision := LClaim.Field('state').Field('session').Field('revision').AsInteger;
    LQuery := NewNyxPascalCompletionRequest(CaptureNyxPascalSources(LLocal.AcceptedPascalProject))
      .At(TNyxPascalSourceRef.Main, 0).Take(1);
    LName := LQuery.Sources.MainUnit.Name;
    LBefore := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    LPacket := NyxStudioCompletionRequest(LQuery, LRevision, 'native-definition', NyxPrimaryWorkspace);
    LReply := LEngine.EditorExchange(LToken, LPacket);
    Check((LProvider.Calls = 1) and (LReply.Field('page').Field('version').AsInteger = 2),
      'Actual private EditorExchange carries admitted project definitions through CompleteSource');
    LReceived := ReadNyxStudioCompletionEnvelope(LReply, 'native-definition',
      NyxPrimaryWorkspace, LRevision, LQuery);
    Check(Supports(LReceived, INyxPascalCompletionDefinitions, LDefinitions) and
      LDefinitions.DefinitionLocation(0).Available and
      LDefinitions.DefinitionLocation(0).FileReference.SameFile(TNyxPascalSourceRef.Main) and
      (LDefinitions.DefinitionLocation(0).Name = LName),
      'The real private handoff reaches the correlated client with the exact main source target');
    LCalls := LProvider.Calls;
    LReply := LEngine.EditorExchange(LToken, NyxStudioCompletionRequest(LQuery, LRevision,
      'native-caret-definition', NyxPrimaryWorkspace, spqDefinition));
    LReceived := ReadNyxStudioCompletionEnvelope(LReply, 'native-caret-definition',
      NyxPrimaryWorkspace, LRevision, LQuery);
    Check((LProvider.DefinitionCalls = 1) and (LProvider.Calls = LCalls) and
      (LReceived.Count = 1) and Supports(LReceived, INyxPascalCompletionDefinitions, LDefinitions) and
      LDefinitions.DefinitionLocation(0).Available,
      'Private definition dispatch uses the specialized provider without completing a prefix');
    LPreview := LReceived.Preview(0, LQuery.Sources, LNewCaret);
    Check((LPreview.Count = LQuery.Sources.Count) and (LNewCaret = LQuery.Caret),
      'Shared transport definition rows retain source membership and caret through Preview');
    { Ordinary Preview can mark a returned value as editing-origin even for an
      empty insertion. Compare every accepted/current byte, not that distinct
      inspection origin; the definition menu refuses Preview/Accept altogether. }
    for LCalls := 0 to LQuery.Sources.Count - 1 do
    begin
      Check(LPreview.FileReference(LCalls).SameFile(LQuery.Sources.FileReference(LCalls)) and
        (LPreview.Text(LPreview.FileReference(LCalls)) =
          LQuery.Sources.Text(LQuery.Sources.FileReference(LCalls))) and
        (LPreview.AcceptedText(LPreview.FileReference(LCalls)) =
          LQuery.Sources.AcceptedText(LQuery.Sources.FileReference(LCalls))),
        'Shared definition page contains no accepted/current source replacement');
    end;
    LCalls := LProvider.Calls;
    LRejected := False;
    try
      LEngine.EditorExchange(LToken, NyxStudioCompletionRequest(LQuery, LRevision + 1,
        'stale-definition', NyxPrimaryWorkspace));
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected and (LProvider.Calls = LCalls),
      'Private stale revision refuses before invoking the controlled provider');
    LRejected := False;
    try
      LEngine.EditorExchange(LToken, NyxStudioCompletionRequest(LQuery, LRevision,
        'foreign-definition', NyxWorkspace('missing-definition-workspace')));
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected and (LProvider.Calls = LCalls),
      'Missing workspace refuses before private provider dispatch');
    LRejected := False;
    try
      ReadNyxStudioCompletionEnvelope(LReply, 'other-definition', NyxPrimaryWorkspace,
        LRevision, LQuery);
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected, 'Actual private response cannot bind to another request identity');
    LPublic := LEngine.InvokeTool('nyx_pascal', 'definition-fixture', 'Lil'' Scooty', Args(LRevision));
    LProvider.WithLocations := False;
    LBaseline := LEngine.InvokeTool('nyx_pascal', 'definition-fixture', 'Lil'' Scooty', Args(LRevision));
    Check(LPublic.ToJSON = LBaseline.ToJSON,
      'Actual public semantic response bytes remain exact when the provider adds navigation');
    LReply := LEngine.EditorExchange(LToken, NyxStudioCompletionRequest(LQuery, LRevision,
      'native-without-definition', NyxPrimaryWorkspace));
    Check(LReply.Field('page').Field('version').AsInteger = 1,
      'Actual private metadata-free provider still returns the original reply protocol');
    LAfter := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    Check(LBefore.Field('project').AsText = LAfter.Field('project').AsText,
      'Actual private completion leaves the complete accepted/draft/base/helper pair exact');
    for LKey in ['revision', 'selection', 'view', 'canUndo', 'canRedo'] do
    begin
      Check(LBefore.Field('session').Field(LKey).ToJSON =
        LAfter.Field('session').Field(LKey).ToJSON,
        'Actual completion preserves ordinary Studio ' + LKey);
    end;
    Check(FileExists(LRuntime + '.codex' + PathDelim + 'config.toml') and
      not FileExists(LRuntime + '.local' + PathDelim + 'codex-mcp-registration.json'),
      'The retained fixture has only its own local enrollment and no global registration');
  finally
    LEngine.Free;
    LRetained := nil;
    LProfile.Free;
    LLocal.Free;
  end;
end;
{$endif}

begin
  try
    Run;
    {$ifndef PAS2JS}
    NativeEditorExchange;
    {$endif}
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' definition transport checks';
    {$else}
    WriteLn('PASS ', GChecks, ' definition transport checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
