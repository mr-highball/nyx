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

program nyx_file_save_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.controls, nyx.logic, nyx.logic.files, nyx.logic.catalog, nyx.files.save,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.flow.editing,
  nyx.codegen, nyx.source, nyx.studio.projects, nyx.studio.session,
  nyx.studio.agents
  , nyx.files.save.editor, nyx.logic.editor
  {$ifdef NYX_FILE_SAVE_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function ExampleGraph(const AOptions: TNyxFileSaveOptions): INyxFlowGraph;
begin
  Result := NewNyxFlowGraph
    .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat)
    .WithStep(NyxFlowStep('encode'), NewNyxUTF8Encode)
    .WithStep(NyxFlowStep('save'), NewNyxSaveFile(AOptions))
    .Connect(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)),
      NyxFlowEndpoint(NyxFlowStep('pretty'), NyxLogicPort(nlpJSON)))
    .Connect(NyxFlowEndpoint(NyxFlowStep('pretty'), NyxLogicPort(nlpText)),
      NyxFlowEndpoint(NyxFlowStep('encode'), NyxLogicPort(nlpText)))
    .Connect(NyxFlowEndpoint(NyxFlowStep('encode'), NyxFilePort(nfpBytes)),
      NyxFlowEndpoint(NyxFlowStep('save'), NyxFilePort(nfpBytes)))
    .Build;
end;

procedure Run;
var
  LDoc: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LOptions: TNyxFileSaveOptions;
  LCatalog: INyxLogicCatalog;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
  LArguments: TNyxDataValue;
  LRevision: Integer;
  LSession: TNyxStudioSession;
  LRejected: Boolean;
  LMode: TNyxFileSaveMode;
  LForms: TNyxDocument;
  LForm: INyxCard;
  LSaveForm: INyxCard;
  LDraft: TNyxFileSaveEditorDraft;
  LLogicDraft: TNyxLogicEditorDraft;
  LRestored: TNyxLogicEditorDraft;
  LCapture: TNyxLogicEditorCapture;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  LDoc := TNyxDocument.Create;
  LParsed := nil;
  LWorkspace := nil;
  LAgent := nil;
  LSession := nil;
  LForms := nil;
  try
    LDoc.Title := 'Keep a report';
    LDoc.AddPage(NewNyxPage('home').Add(NewNyxHeading('title').WithText('Save your work')));
    LOptions := NyxFileSaveOptions('report-🌙.json').Title('Save a report')
      .Mode(nfsmDownload).MediaType(NyxMediaType(nfmtJSON)).Allow('json')
      .Allow('txt').AllTypes(False).MaximumBytes(4096).DownloadActivation(nfdaPlatform);
    LDoc.Flows.Define(NyxFlowRef('Save report'), ExampleGraph(LOptions));
    LSource := TNyxCodegen.Generate(LDoc);
    Check((Pos('NewNyxSaveFile(', LSource) > 0) and
      (Pos('NewNyxUTF8Encode', LSource) > 0) and
      (Pos('.Mode(nfsmDownload)', LSource) > 0) and
      (Pos('.MediaType(NyxMediaType(nfmtJSON))', LSource) > 0) and
      (Pos('NyxFilePort(nfpBytes)', LSource) > 0),
      'crafted source uses specialized factories, options, media and port enums');
    Check(Pos('NewNyxLogicDeclaration(', LSource) = 0, 'built-ins do not emit opaque descriptor JSON');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc), 'literal source admits the exact complete document');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op source retains exact bytes');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LRejected := False;
    try
      LParsed := TNyxSourceWorkspace.PrepareDraft(StringReplace(LSource,
        '.Mode(nfsmDownload)', '.Mode(''download'')', []), LWorkspace);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'raw behavior strings cannot replace closed mode enums');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    for LMode := Low(TNyxFileSaveMode) to High(TNyxFileSaveMode) do
    begin
      LDoc.Flows.Define(NyxFlowRef('Save report'), ExampleGraph(LOptions.Mode(LMode)
        .MediaType(NyxMediaType('application/x-nyx-report'))));
      LAfter := TNyxCodegen.Generate(LDoc);
      LParsed := TNyxSourceWorkspace.PrepareDraft(LAfter, LWorkspace);
      Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc),
        'each mode and an open typed MIME reference survive source admission');
      FreeAndNil(LParsed);
      FreeAndNil(LWorkspace);
    end;
    LDoc.Flows.Define(NyxFlowRef('Save report'), ExampleGraph(LOptions));
    Check(TNyxCodegen.Generate(LDoc) = LSource, 'restoring exact configuration restores source bytes');
    LCatalog := NewNyxLogicCatalog;
    Check((LCatalog.Count = 20) and LCatalog.Matches(10, 'utf encoding') and
      LCatalog.Matches(11, 'files save') and
      (LCatalog.Descriptor(11).Group = nlgFiles),
      'default discovery provides useful intent/help in the Files collection');
    Check(NyxLogicFromData(NyxLogicKind('file-save'), LOptions.ToData).Configuration.ToJSON =
      LOptions.ToData.ToJSON, 'data-only import reconstructs intent without a runtime host');

    LForms := TNyxDocument.Create;
    LSaveForm := NewNyxFileSaveEditor('save-form', LOptions);
    LForms.AddComponent(LSaveForm);
    Check(CaptureNyxFileSaveEditor('save-form', LSaveForm.Node).ToData.ToJSON = LOptions.ToData.ToJSON,
      'public Nyx form captures exact options including a false checkbox');
    LSaveForm.Node.Find('save-form-maximum').Configure.Value('unfinished!').Done;
    LDraft.Capture('save-form', LSaveForm.Node);
    LDraft := TNyxFileSaveEditorDraft.FromData(LDraft.ToData);
    LSaveForm.Node.Find('save-form-maximum').Configure.Value('2').Done;
    Check(LDraft.Restore('save-form', LSaveForm.Node) and
      (LSaveForm.Node.Find('save-form-maximum').Prop('value') = 'unfinished!'),
      'unfinished numeric input survives owned draft round trip');
    LRejected := False;
    try
      CaptureNyxFileSaveEditor('save-form', LSaveForm.Node);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSaveForm.Node.Find('save-form-maximum').Prop('value') = 'unfinished!'),
      'invalid options refuse without discarding unfinished input');
    LForm := NewNyxLogicEditor('editor', LDoc, NyxFlowRef('Save report'));
    LForms.AddComponent(LForm);
    LLogicDraft.Capture('editor', LForm.Node);
    Check(LLogicDraft.ToData.Field('version').AsInteger = 3,
      'unused save form preserves the existing Logic draft version');
    LForm.Node.Find('editor-step').Configure.Value('save').Done;
    HandleNyxLogicEditor(LForm.Node.Find('editor-load'), LForm.Node);
    Check(LForm.Node.Find('editor-save').Prop('visible') = 'true',
      'Logic configures built-in save intent through its specialized form');
    LForm.Node.Find('editor-save-maximum').Configure.Value('still typing').Done;
    LLogicDraft.Capture('editor', LForm.Node);
    Check(LLogicDraft.ToData.Field('version').AsInteger = 5,
      'used save controls receive the optional version-5 draft child');
    LRestored := TNyxLogicEditorDraft.FromData(LLogicDraft.ToData);
    LForm.Node.Find('editor-save-maximum').Configure.Value('1').Done;
    Check(LRestored.Restore('editor', LForm.Node) and
      (LForm.Node.Find('editor-save-maximum').Prop('value') = 'still typing'),
      'complete Logic draft restores unfinished specialized save inputs');
    LForm.Node.Find('editor-save-maximum').Configure.Value('8192').Done;
    Check(CaptureNyxLogicEditor(LForm.Node.Find('editor-set'), LForm.Node, LCapture) and
      (LCapture.Change.ToData.Field('configuration').Field('maximumBytes').AsInteger = 8192),
      'Logic capture produces a typed guarded save change');

    LSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDoc), LSource));
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Select('title');
    LSession.SetProperty('text', 'A report worth keeping');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(LAfter <> LBefore, 'ordinary visual edit retains the authored save graph');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'ordinary paired Undo is exact');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'ordinary paired Redo is exact');

    LAgent := TNyxAgentSession.Create(NyxProjectPair(TNyxCodec.Encode(LDoc), LSource));
    LRevision := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'File save qualification', NyxObject([
      NyxField('scope', NyxData('catalog')), NyxField('query', NyxData('files save')),
      NyxField('limit', NyxData(1)), NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('kind').AsText = 'file-save'),
      'bounded local semantic discovery exposes only requested save context');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('save-options')),
      NyxField('operations', NyxArray([NyxReplaceFlowStep(NyxFlowRef('Save report'),
        NyxFlowStep('save'), NewNyxSaveFile(LOptions.Title('Keep a copy'))).ToData]))]);
    LAgent.Call('nyx_transaction', 'File save qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'semantic save edit is one guarded revision');
    LAfter := EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home'));
    LAgent.Call('nyx_transaction', 'File save qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'semantic exact retry adds no second history entry');
    LAgent.Call('nyx_history', 'File save qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('undo-save')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) =
      EncodeNyxProject(NyxProjectPair(TNyxCodec.Encode(LDoc), LSource)),
      'semantic Undo restores exact source and design');
    LAgent.Call('nyx_history', 'File save qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('redo-save')), NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LAfter,
      'semantic Redo restores exact edited pair');

    {$ifdef NYX_FILE_SAVE_COMPANION}
    LParsed := BuildNyxDocument;
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc),
      'exact generated Pascal constructs the complete expected saved document');
    FreeAndNil(LParsed);
    {$endif}

    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LAgent.Free;
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDoc.Free;
    LSaveForm := nil;
    LForm := nil;
    LForms.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-file-save-contract', 'passed');
    document.body.setAttribute('data-file-save-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' file-save source/history/local semantic checks; no host effects');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-file-save-contract', 'failed');
      document.body.setAttribute('data-file-save-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
