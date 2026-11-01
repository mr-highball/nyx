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
program nyx_resource_logic_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.source, nyx.logic.editor, nyx.logic.resources,
  nyx.resource.workshop, nyx.resources, nyx.flow.types, nyx.flow.declarations,
  nyx.flow.editing, nyx.studio.agents, nyx.studio.projects
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LBefore: TNyxText;
  LForm: INyxCard;
  LDraft: TNyxLogicEditorDraft;
  LRestored: TNyxLogicEditorDraft;
  LRejected: Boolean;
  LCapture: TNyxLogicEditorCapture;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LReply: TNyxDataValue;
  LArguments: TNyxDataValue;
  LRevision: Integer;
  {$ifndef PAS2JS}
  LFile: TFileStream;
  LBytes: TNyxBytes;
  {$endif}
begin
  LDocument := BuildResourceWorkshopDocument;
  LParsed := nil;
  LWorkspace := nil;
  LAgent := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NewNyxJSONResource(', LSource) > 0) and
      (Pos('NyxJSONResourceOptions(NyxResourceRef(', LSource) > 0) and
      (Pos('NyxJSONResourcePort(njrpJSON)', LSource) > 0),
      'resource graph emits crafted typed options and specialized ports');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'source admission retains the complete canvas/data/route document');
    Check(LWorkspace.Render(LParsed) = LSource, 'unchanged admitted source remains exact');
    LForm := NewNyxLogicEditor('resource-editor', LDocument, ResourceWorkshopFlow);
    LForm.Node.Find('resource-editor-operation').Configure.Value(NyxJSONResourceLogicKind).Done;
    HandleNyxLogicEditor(LForm.Node.Find('resource-editor-defaults'), LForm.Node);
    Check(LForm.Node.Find('resource-editor-resource-name').Prop('value') = 'data',
      'ordinary resource configuration loads typed defaults');
    LForm.Node.Find('resource-editor-resource-name').Configure.Value('hosted-team').Done;
    LForm.Node.Find('resource-editor-resource-deadline').Configure.Value('unfinished').Done;
    LDraft.Capture('resource-editor', LForm.Node);
    LRestored := TNyxLogicEditorDraft.FromData(LDraft.ToData);
    Check(LRestored.ToData.ToJSON = LDraft.ToData.ToJSON, 'unfinished resource draft survives its versioned wire');
    LForm.Node.Find('resource-editor-resource-name').Configure.Value('sample-team').Done;
    LForm.Node.Find('resource-editor-resource-deadline').Configure.Value('15000').Done;
    Check(LRestored.Restore('resource-editor', LForm.Node), 'exact baseline accepts draft restoration');
    Check(LForm.Node.Find('resource-editor-resource-deadline').Prop('value') = 'unfinished',
      'malformed deadline remains editable without silently accepting a default');
    LRejected := False;
    try
      CaptureNyxLogicEditor(LForm.Node.Find('resource-editor-add'), LForm.Node, LCapture);
    except
      on E: Exception do LRejected := True;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBefore),
      'invalid configuration preserves the authored document');
    LForm.Node.Find('resource-editor-resource-deadline').Configure.Value('2500').Done;
    Check(CaptureNyxLogicEditor(LForm.Node.Find('resource-editor-add'), LForm.Node, LCapture),
      'ordinary typed controls capture a valid guarded operation proposal');
    LPair := NyxProjectPair(LBefore, LSource);
    LAgent := TNyxAgentSession.Create(LPair);
    LAgent.InheritPermission(apEdit);
    LRevision := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'Resource workflow qualification', NyxObject([
      NyxField('scope', NyxData('catalog')), NyxField('query', NyxData('hosted resource')),
      NyxField('limit', NyxData(1)), NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('kind').AsText = NyxJSONResourceLogicKind),
      'bounded semantic discovery exposes the typed resource operation');
    LReply := LAgent.Call('nyx_logic', 'Resource workflow qualification', NyxObject([
      NyxField('scope', NyxData('ports')), NyxField('flow', NyxData(ResourceWorkshopFlow.Name)),
      NyxField('step', NyxData('resource')), NyxField('offset', NyxData(1)),
      NyxField('limit', NyxData(1)), NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('port').AsText = 'reference') and
      LReply.Field('items').Item(0).Field('optional').AsBoolean,
      'bounded semantic inspection retains optional reference input meaning');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('resource-request-group')),
      NyxField('operations', NyxArray([
        NyxReplaceFlowStep(ResourceWorkshopFlow, NyxFlowStep('resource'),
          NewNyxJSONResource(NyxJSONResourceOptions(NyxResourceRef('sample-team'))
            .WholeRequest(2500))).ToData]))]);
    LAgent.Call('nyx_transaction', 'Resource workflow qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'semantic replacement is one guarded resource/source revision');
    LAgent.Call('nyx_transaction', 'Resource workflow qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'identical semantic retry creates no duplicate history');
    LAgent.Call('nyx_history', 'Resource workflow qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('resource-request-undo')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LPair),
      'semantic Undo restores the exact resource source/design pair');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LBytes := NyxEncodeUTF8(LSource);
      LFile := TFileStream.Create(ParamStr(1), fmCreate);
      try

        if Length(LBytes) > 0 then
        begin
          LFile.WriteBuffer(LBytes[0], Length(LBytes));
        end;
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LForm := nil;
    LAgent.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-resource-authoring', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' resource authoring checks';
    {$else}
    WriteLn('PASS ', GChecks, ' resource authoring checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-resource-authoring', 'failed');
      document.body.textContent := E.Message;
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
