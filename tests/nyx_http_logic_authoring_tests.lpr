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

program nyx_http_logic_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec, nyx.controls,
  nyx.types, nyx.http.types, nyx.http.editor, nyx.logic.http, nyx.logic.catalog,
  nyx.logic.editor, nyx.logic, nyx.flow, nyx.flow.types, nyx.flow.declarations,
  nyx.flow.editing, nyx.codegen, nyx.source, nyx.studio.projects,
  nyx.studio.session, nyx.studio.agents, nyx.test.http.authoring
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure SetValue(AForm: TNyxNode; const AName, AValue: TNyxText);
begin
  AForm.Find(AForm.ID + '-' + AName).Configure.Value(AValue).Done;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LForms: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LForm: INyxCard;
  LPair: TNyxProjectPair;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LSource: TNyxText;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LRequest: TNyxHTTPRequest;
  LCapture: TNyxLogicEditorCapture;
  LDraft: TNyxLogicEditorDraft;
  LRestored: TNyxLogicEditorDraft;
  LDraftData: TNyxDataValue;
  LLegacy: TNyxDataValue;
  LReply: TNyxDataValue;
  LArguments: TNyxDataValue;
  LRevision: Integer;
  LRejected: Boolean;
  LCatalog: INyxLogicCatalog;
  LChanged: INyxFlowDeclarations;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  LDocument := HTTPAuthoringDocument;
  LParsed := nil;
  LWorkspace := nil;
  LForms := nil;
  LSession := nil;
  LAgent := nil;
  try
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NewNyxHTTPCall(', LSource) > 0) and
      (Pos('.Method(nhmPost)', LSource) > 0) and
      (Pos('NewNyxUTF8Decode', LSource) > 0) and
      (Pos('NyxHTTPPort(nhpTrigger)', LSource) > 0) and
      (Pos('NyxHTTPPort(nhpBody)', LSource) > 0), 'crafted specialized HTTP/decoder/route source');
    Check(Pos('.WithBody(NyxEncodeUTF8(', LSource) > 0, 'text request bodies remain readable Pascal');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument),
      'literal generated admission retains complete request, duplicates and route');
    Check(LWorkspace.Render(LParsed) = LSource, 'HTTP no-op source remains exact');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);
    LBefore := StringReplace(LSource,
      'NyxHTTPRequest(NyxResourceURL(''https://example.invalid/api/profile''))',
      'NyxHTTPRequest(NyxResourceURL(''https://example.invalid/initial''))' +
        '.WithURL(NyxResourceURL(''https://example.invalid/api/profile''))', []);
    Check(LBefore <> LSource, 'derived-target source changes the actual emitted HTTP constructor');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LBefore, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument),
      'fluent WithURL source retains the final configured request and all graph meaning');
    Check(LWorkspace.Render(LParsed) = LBefore,
      'the crafted WithURL chain remains exact through source admission');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);
    LRejected := False;
    try
      LParsed := TNyxSourceWorkspace.PrepareDraft(StringReplace(LSource,
        '.Method(nhmPost)', '.Method(''POST'')', []), LWorkspace);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'raw method text refuses instead of erasing the Pascal type boundary');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);
    LCatalog := NewNyxLogicCatalog;
    Check((LCatalog.Count = 24) and LCatalog.Matches(9, 'http request') and
      LCatalog.Matches(8, 'utf unicode'), 'new kinds have searchable intent/help');
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('title');
    LSession.SetProperty('text', 'A crafted HTTP tool');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair), 'complete paired HTTP Undo');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'complete paired HTTP Redo');

    LForms := TNyxDocument.Create;
    LForm := NewNyxLogicEditor('editor', LDocument, NyxFlowRef('Download'));
    LForms.AddComponent(LForm);
    SetValue(LForm.Node, 'step', 'request');
    Check(HandleNyxLogicEditor(LForm.Node.Find('editor-load'), LForm.Node), 'saved HTTP loads into typed fields');
    Check(LForm.Node.Find('editor-http').Prop('visible') <> 'false', 'HTTP form is visible for HTTP');
    Check(LForm.Node.Find('editor-creator-config').Prop('visible') = 'false', 'built-in HTTP uses typed fields');
    LRequest := CaptureNyxHTTPRequestEditor('editor-http', LForm.Node);
    Check(LRequest.ToData.ToJSON = HTTPAuthoringRequest.ToData.ToJSON, 'typed form retains duplicate header rows exactly');
    SetValue(LForm.Node.Find('editor-http'), 'body-kind', 'Base64 bytes');
    Check(HandleNyxLogicEditor(LForm.Node.Find('editor-http-body-kind'), LForm.Node),
      'body presentation changes through ordinary public form handling');
    Check(CaptureNyxHTTPRequestEditor('editor-http', LForm.Node).ToData.ToJSON = LRequest.ToData.ToJSON,
      'changing body presentation preserves exact bytes');
    SetValue(LForm.Node.Find('editor-http'), 'header-name', 'x-extra');
    SetValue(LForm.Node.Find('editor-http'), 'header-value', 'owned');
    HandleNyxLogicEditor(LForm.Node.Find('editor-http-header-set'), LForm.Node);
    Check(CaptureNyxHTTPRequestEditor('editor-http', LForm.Node).Headers.Count = 4,
      'individual typed header admission appends without erasing existing rows');
    LBefore := CaptureNyxHTTPRequestEditor('editor-http', LForm.Node).ToData.ToJSON;
    SetValue(LForm.Node.Find('editor-http'), 'header-value', 'bad' + #13#10 + 'Injected: yes');
    LRejected := False;
    try
      HandleNyxLogicEditor(LForm.Node.Find('editor-http-header-set'), LForm.Node);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (CaptureNyxHTTPRequestEditor('editor-http', LForm.Node).ToData.ToJSON = LBefore),
      'invalid header input preserves admitted request rows');

    SetValue(LForm.Node.Find('editor-http'), 'deadline', 'unfinished!');
    LDraft.Capture('editor', LForm.Node);
    LDraftData := LDraft.ToData;
    Check(LDraftData.Field('version').AsInteger = 3, 'Logic draft versions its nested request controls');
    LRestored := TNyxLogicEditorDraft.FromData(LDraftData);
    Check(LRestored.ToData.ToJSON = LDraftData.ToJSON, 'request/editor draft round trip is exact');
    LLegacy := NyxObject([NyxField('version', NyxData(3)),
      NyxField('context', LDraftData.Field('context')), NyxField('values', LDraftData.Field('values')),
      NyxField('path', LDraftData.Field('path')), NyxField('descriptor', LDraftData.Field('descriptor')),
      NyxField('http', NyxNull)]);
    LRejected := False;
    try
      LRestored := TNyxLogicEditorDraft.FromData(LLegacy);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'missing version-3 HTTP fields refuse instead of silently downgrading the draft');
    LRestored := TNyxLogicEditorDraft.FromData(LDraftData);
    SetValue(LForm.Node.Find('editor-http'), 'deadline', '15000');
    Check(LRestored.Restore('editor', LForm.Node) and
      (LForm.Node.Find('editor-http-deadline').Prop('value') = 'unfinished!'),
      'malformed request input survives editor remount/restoration');
    LRejected := False;
    try
      CaptureNyxLogicEditor(LForm.Node.Find('editor-set'), LForm.Node, LCapture);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LPair.Design),
      'malformed request refuses without changing the accepted document');
    SetValue(LForm.Node.Find('editor-http'), 'deadline', '7000');
    Check(CaptureNyxLogicEditor(LForm.Node.Find('editor-set'), LForm.Node, LCapture),
      'typed request captures an ordinary graph replacement');
    LChanged := EditNyxFlows(LDocument.Flows, [LCapture.Change]);
    Check(LChanged.Definition(NyxFlowRef('Download')).Configuration(0).Field('deadline').AsInteger = 7000,
      'whole graph retains typed HTTP configuration');

    LLegacy := NyxObject([NyxField('version', NyxData(2)),
      NyxField('context', LDraftData.Field('context')), NyxField('values', LDraftData.Field('values')),
      NyxField('path', LDraftData.Field('path')), NyxField('descriptor', LDraftData.Field('descriptor'))]);
    LRestored := TNyxLogicEditorDraft.FromData(LLegacy);
    Check(LRestored.ToData.ToJSON = LLegacy.ToJSON, 'legacy version-2 draft bytes are unchanged');
    Check(LRestored.Restore('editor', LForm.Node), 'legacy draft restores without manufacturing HTTP input');

    LRequest := HTTPAuthoringRequest.Method(NyxHTTPMethod('REPORT')).WithBody(NyxDecodeBase64('/wAB'));
    LDocument.Flows.Define(NyxFlowRef('Binary'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('request'), NewNyxHTTPCall(LRequest)).Build);
    LAfter := TNyxCodegen.Generate(LDocument);
    Check((Pos('NyxHTTPMethod(''REPORT'')', LAfter) > 0) and
      (Pos('NyxDecodeBase64(''/wAB'')', LAfter) > 0), 'extension methods and binary bodies retain explicit typed constructors');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LAfter, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument), 'binary/custom-method source rebuild is exact');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);

    LAgent := TNyxAgentSession.Create(LPair);
    LAgent.InheritPermission(apEdit);
    LRevision := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'HTTP authoring qualification',
      NyxObject([NyxField('scope', NyxData('catalog')), NyxField('query', NyxData('http request')),
        NyxField('limit', NyxData(1)), NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('kind').AsText = 'http-call'),
      'bounded semantic discovery exposes the typed HTTP default');
    LReply := LAgent.Call('nyx_logic', 'HTTP authoring qualification', NyxObject([
      NyxField('scope', NyxData('ports')), NyxField('flow', NyxData('Download')),
      NyxField('step', NyxData('request')), NyxField('offset', NyxData(1)),
      NyxField('limit', NyxData(1)), NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('port').AsText = 'url') and
      (LReply.Field('items').Item(0).Field('direction').AsText = 'input') and
      (LReply.Field('items').Item(0).Field('type').AsText = NyxFlowTypeName(nftText)) and
      LReply.Field('items').Item(0).Field('optional').AsBoolean,
      'bounded semantic port inspection exposes URL type and optionality on an existing saved graph');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('http-request-group')),
      NyxField('operations', NyxArray([
        NyxReplaceFlowStep(NyxFlowRef('Download'), NyxFlowStep('request'),
          NewNyxHTTPCall(HTTPAuthoringRequest.WholeRequest(8000))).ToData]))]);
    LAgent.Call('nyx_transaction', 'HTTP authoring qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'semantic HTTP replacement is one guarded revision');
    LAgent.Call('nyx_transaction', 'HTTP authoring qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'exact semantic retry retains one history entry');
    LReply := LAgent.Call('nyx_logic', 'HTTP authoring qualification',
      NyxObject([NyxField('scope', NyxData('configuration')), NyxField('flow', NyxData('Download')),
        NyxField('step', NyxData('request')), NyxField('textLimit', NyxData(32)),
        NyxField('expectedRevision', NyxData(LAgent.Revision))]));
    Check(LReply.Field('truncated').AsBoolean and
      (Length(LReply.Field('text').AsText) <= 32), 'request configuration remains a bounded semantic window');
    LAgent.Call('nyx_history', 'HTTP authoring qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('http-request-undo')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LPair),
      'semantic HTTP Undo restores the exact source/design pair');

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
    LForms.Free;
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-http-authoring', 'passed');
    document.body.setAttribute('data-http-authoring-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' HTTP source/editor/semantic checks / no host I/O');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-http-authoring', 'failed');
      document.body.setAttribute('data-http-authoring-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
