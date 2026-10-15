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

program nyx_file_open_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types,
  nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.source,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.logic, nyx.logic.http, nyx.http.types,
  nyx.files.open, nyx.logic.files.open, nyx.test.files.openfixture,
  nyx.files.open.editor, nyx.logic.editor, nyx.logic.catalog, nyx.flow.editing,
  nyx.studio.projects, nyx.studio.session, nyx.studio.agents, nyx.studio.edits
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LDoc: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LBefore: TNyxText;
  LOptions: TNyxFileOpenOptions;
  LGraph: INyxFlowGraph;
  LRejected: Boolean;
  LCatalog: INyxLogicCatalog;
  LForms: TNyxDocument;
  LForm: INyxCard;
  LOpenForm: INyxCard;
  LDraft: TNyxFileOpenEditorDraft;
  LLogicDraft: TNyxLogicEditorDraft;
  LRestored: TNyxLogicEditorDraft;
  LCapture: TNyxLogicEditorCapture;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LArguments: TNyxDataValue;
  LReply: TNyxDataValue;
  LRevision: Integer;
  LAfter: TNyxText;
  {$ifndef PAS2JS}LFile: TFileStream; LBytes: TNyxBytes;{$endif}
begin
  LDoc := FileOpenDocument(False);
  LParsed := nil;
  LWorkspace := nil;
  LForms := nil;
  LSession := nil;
  LAgent := nil;
  try
    LBefore := TNyxCodegen.Generate(LDoc);
    Check(Pos('nyx.files.open', LBefore) = 0, 'unused file-open vocabulary never changes old companion imports');
    LOptions := NyxFileOpenOptions.Title('Choose a report').Allow('json').Allow('txt')
      .MaximumBytes(4096);
    LGraph := NewNyxFlowGraph
      .WithStep(NyxFlowStep('choose'), NewNyxOpenFile(LOptions))
      .WithStep(NyxFlowStep('decode'), NewNyxUTF8Decode)
      .Connect(NyxFlowEndpoint(NyxFlowStep('choose'), NyxOpenFilePort(nofpBytes)),
        NyxFlowEndpoint(NyxFlowStep('decode'), NyxHTTPPort(nhpBody)))
      .Build;
    LDoc.Flows.Define(NyxFlowRef('Read report'), LGraph);
    LSource := TNyxCodegen.Generate(LDoc);
    Check((Pos('NewNyxOpenFile(', LSource) > 0) and
      (Pos('NyxFileOpenOptions.Title(', LSource) > 0) and
      (Pos('.MaximumBytes(4096)', LSource) > 0) and
      (Pos('NyxOpenFilePort(nofpBytes)', LSource) > 0),
      'default generated source uses specialized factories, fluent values and closed ports');
    Check(Pos('NewNyxLogicDeclaration(', LSource) = 0, 'built-in opening never falls back to opaque configuration JSON');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc), 'typed literal source retains the exact inert document');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op source preserves the exact crafted bytes');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LRejected := False;
    try
      LParsed := TNyxSourceWorkspace.PrepareDraft(StringReplace(LSource,
        '.MaximumBytes(4096)', '.MaximumBytes(''4096'')', []), LWorkspace);
    except
      LRejected := True;
    end;
    Check(LRejected, 'raw strings cannot replace the numeric read budget');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LDoc.Flows.Remove(NyxFlowRef('Read report'));
    Check(TNyxCodegen.Generate(LDoc) = LBefore, 'removing the unused family restores exact original source');
    LDoc.Flows.Define(NyxFlowRef('Read report'), LGraph);
    LCatalog := NewNyxLogicCatalog;
    Check((LCatalog.Count = 24) and LCatalog.Matches(12, 'files open metadata') and
      (LCatalog.Descriptor(12).Group = nlgFiles),
      'default discovery exposes bounded file opening with intent/help in Files');
    Check(NyxLogicFromData(NyxLogicKind('file-open'), LOptions.ToData).Configuration.ToJSON =
      LOptions.ToData.ToJSON, 'persistence reconstructs intent without a runtime host');
    LForms := TNyxDocument.Create;
    LOpenForm := NewNyxFileOpenEditor('open-form', LOptions);
    LForms.AddComponent(LOpenForm);
    Check(CaptureNyxFileOpenEditor('open-form', LOpenForm.Node).ToData.ToJSON = LOptions.ToData.ToJSON,
      'public typed form captures the exact bounded open intent');
    LDraft.Capture('open-form', LOpenForm.Node, False);
    Check(not LDraft.Defined, 'unused pristine open form requires no new draft version');
    LOpenForm.Node.Find('open-form-maximum').Configure.Value('still typing!').Done;
    LDraft.Capture('open-form', LOpenForm.Node);
    LDraft := TNyxFileOpenEditorDraft.FromData(LDraft.ToData);
    LOpenForm.Node.Find('open-form-maximum').Configure.Value('1').Done;
    Check(LDraft.Restore('open-form', LOpenForm.Node) and
      (LOpenForm.Node.Find('open-form-maximum').Prop('value') = 'still typing!'),
      'unfinished numeric input survives independent draft round trip');
    LRejected := False;
    try
      CaptureNyxFileOpenEditor('open-form', LOpenForm.Node);
    except
      LRejected := True;
    end;
    Check(LRejected and (LOpenForm.Node.Find('open-form-maximum').Prop('value') = 'still typing!'),
      'refused admission preserves exact unfinished controls');
    LForm := NewNyxLogicEditor('editor', LDoc, NyxFlowRef('Read report'));
    LForms.AddComponent(LForm);
    LLogicDraft.Capture('editor', LForm.Node);
    Check(LLogicDraft.ToData.Field('version').AsInteger = 3,
      'unused open form retains existing Logic draft format');
    LForm.Node.Find('editor-step').Configure.Value('choose').Done;
    HandleNyxLogicEditor(LForm.Node.Find('editor-load'), LForm.Node);
    Check((LForm.Node.Find('editor-file-open').Prop('visible') = 'true') and
      (LForm.Node.Find('editor-creator-config').Prop('visible') = 'false'),
      'built-in opening uses specialized controls instead of opaque configuration JSON');
    LForm.Node.Find('editor-file-open-maximum').Configure.Value('not finished').Done;
    LLogicDraft.Capture('editor', LForm.Node);
    Check(LLogicDraft.ToData.Field('version').AsInteger = 6,
      'used opening fields receive the optional version-6 draft child');
    LRestored := TNyxLogicEditorDraft.FromData(LLogicDraft.ToData);
    LForm.Node.Find('editor-file-open-maximum').Configure.Value('2').Done;
    Check(LRestored.Restore('editor', LForm.Node) and
      (LForm.Node.Find('editor-file-open-maximum').Prop('value') = 'not finished'),
      'full Logic restoration preserves unfinished specialized open fields');
    LForm.Node.Find('editor-file-open-maximum').Configure.Value('8192').Done;
    Check(CaptureNyxLogicEditor(LForm.Node.Find('editor-set'), LForm.Node, LCapture) and
      (LCapture.Change.ToData.Field('configuration').Field('maximumBytes').AsInteger = 8192),
      'ordinary Logic capture produces one guarded typed configuration change');

    LPair := NyxProjectPair(TNyxCodec.Encode(LDoc), LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.ApplyPatch(NyxFlowPatch([LCapture.Change]));
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(LAfter <> LBefore, 'ordinary paired authoring publishes the captured opening intent');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'ordinary open edit Undo restores exact source/design');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'ordinary open edit Redo restores exact changed pair');

    LAgent := TNyxAgentSession.Create(LPair);
    LRevision := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'File opening qualification', NyxObject([
      NyxField('scope', NyxData('catalog')), NyxField('query', NyxData('files open metadata')),
      NyxField('limit', NyxData(1)), NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('kind').AsText = 'file-open'),
      'bounded local semantic discovery returns only requested opening context');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('open-options')),
      NyxField('operations', NyxArray([NyxReplaceFlowStep(NyxFlowRef('Read report'),
        NyxFlowStep('choose'), NewNyxOpenFile(LOptions.Title('Read a different report'))).ToData]))]);
    LAgent.Call('nyx_transaction', 'File opening qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'semantic opening configuration publishes one guarded revision');
    LAfter := EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home'));
    LAgent.Call('nyx_transaction', 'File opening qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'exact semantic retry creates no duplicate history');
    LAgent.Call('nyx_history', 'File opening qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('undo-open')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LPair),
      'semantic Undo restores the exact complete accepted pair');
    LAgent.Call('nyx_history', 'File opening qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('redo-open')), NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LAfter,
      'semantic Redo restores the exact complete changed pair');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LBytes := NyxEncodeUTF8(LSource);
      LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
        'nyx.generated.view.pas', fmCreate);
      try
        LFile.WriteBuffer(LBytes[0], Length(LBytes));
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
    LOpenForm := nil;
    LForm := nil;
    LForms.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-file-open-authoring', 'passed');
    document.body.setAttribute('data-file-open-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed file-open source checks; no host I/O claim');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-file-open-authoring', 'failed');
      document.body.setAttribute('data-file-open-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
