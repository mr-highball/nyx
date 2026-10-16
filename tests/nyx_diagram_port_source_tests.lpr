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
program nyx_diagram_port_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.codegen, nyx.source, nyx.diagram, nyx.diagram.declarations,
  nyx.studio.projects, nyx.studio.session, nyx.studio.agents, nyx.diagram.editing, nyx.test.diagram.ports
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
{ Emit exact complete UTF-8 pairs into an explicitly owned fixture directory.
  Independent consumers compile these bytes; no installed project is touched. }
procedure ExportPair(const AName: String; const APair: TNyxProjectPair);
var
  LDirectory: String;

  procedure Save(const AName: String; const AText: TNyxText);
  var
    LStream: TFileStream;
    LBytes: TNyxBytes;
  begin
    LBytes := NyxEncodeUTF8(AText);
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(LBytes) > 0 then
      begin
        LStream.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LStream.Free;
    end;
  end;
begin

  if ParamStr(1) = '' then
  begin
    Exit;
  end;
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AName + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', APair.Source);
  Save('expected.nyx', APair.Design);
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LSource: TNyxText;
begin
  LDocument := DiagramPortsDocument;
  LRead := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LRead := TNyxCodec.Decode(LBefore.Design);
    Check(TNyxCodec.Encode(LRead) = LBefore.Design, 'complete diagram document keeps exact named-port data');
    FreeAndNil(LRead);
    LRead := TNyxSourceWorkspace.PrepareDraft(LBefore.Source, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LBefore.Design,
      'crafted typed port/endpoint source reconstructs the whole document exactly');
    Check(PrepareNyxCompanion(LRead, LRead, LBefore.Source, False) = LBefore.Source,
      'named ports pass strict complete companion/import admission');
    Check((Pos('.WithPort(NyxDiagramPort(', LBefore.Source) > 0) and
      (Pos('NyxDiagramPortEnd(', LBefore.Source) > 0),
      'default authoring uses typed port objects and references');
    LSession := TNyxStudioSession.Create;
    LSession.LoadProject(LBefore);
    LSource := LBefore.Source;
    LSource := Copy(LSource, 1, Pos('Message output', LSource) - 1) +
      'Changed output' + Copy(LSource, Pos('Message output', LSource) + Length('Message output'), MaxInt);
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    LAfter := LSession.ProjectSnapshot;
    Check(LSession.Document.Diagrams.Definition(NyxDiagramRef('Workspace'))
      .FindVertex(NyxDiagramVertexRef('Form')).FindPort(NyxDiagramPortRef('Message')).Caption = 'Changed output',
      'ordinary source application edits exact named-port intent');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one Undo restores the complete accepted design/source pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores the exact port/source change');
    {$ifndef PAS2JS}
    ExportPair('original', LBefore);
    ExportPair('changed', LAfter);
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LRead.Free;
    LDocument.Free;
  end;
end;

{ Exercise the production semantic dispatcher against an explicitly owned pair.
  This is transport-independent qualification, not authenticated HTTP/MCP use. }
procedure SemanticPorts;
var
  LDocument: TNyxDocument;
  LAgent: TNyxAgentSession;
  LPlan: INyxDiagram;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LQuery: TNyxDataValue;
  LRequest: TNyxDataValue;
  LRevision: Integer;
  LFailed: Boolean;
begin
  LDocument := DiagramPortsDocument;
  LAgent := nil;
  try
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LAgent := TNyxAgentSession.Create(LBefore);
    LRevision := LAgent.Revision;
    LPlan := LDocument.Diagrams.Definition(NyxDiagramRef('Workspace'));
    LQuery := LAgent.Call('nyx_outline', 'Scooty owned port fixture', NyxObject([
      NyxField('scope', NyxData('diagram-vertices')), NyxField('name', NyxData('Workspace')),
      NyxField('member', NyxData('Form')), NyxField('limit', NyxData(1)),
      NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LQuery.Field('total').AsInteger = 1) and
      (LQuery.Field('items').Item(0).Field('ports').Count = 1),
      'bounded exact-vertex context includes its named physical connections');
    LRequest := NyxObject([NyxField('operationId', NyxData('move-port')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operations', NyxArray([
        NyxSetDiagramVertex(NyxDiagramRef('Workspace'), LPlan.Vertex(0).WithPort(
          LPlan.Vertex(0).Port(0).Captioned('Semantic output'))).Expect(LPlan).ToData]))]);
    LAgent.Call('nyx_transaction', 'Scooty owned port fixture', LRequest);
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    Check((LAgent.Revision = LRevision + 1) and (LAfter.Design <> LBefore.Design) and
      (Pos('Semantic output', LAfter.Source) > 0), 'one semantic operation publishes typed port design/source together');
    Check(LQuery.Field('items').Item(0).Field('ports').Item(0).Field('caption').AsText = 'Message output',
      'previous bounded context is independent from semantic port mutation');
    LFailed := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty owned port fixture', NyxObject([
        NyxField('operationId', NyxData('stale-port')), NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operations', LRequest.Field('operations'))]));
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) =
      EncodeNyxProject(LAfter)), 'stale semantic port proposals refuse without pair changes');
    LAgent.Call('nyx_history', 'Scooty owned port fixture', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('undo-port')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LBefore),
      'one semantic Undo restores both exact original files');
    LAgent.Call('nyx_history', 'Scooty owned port fixture', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('redo-port')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter),
      'one semantic Redo restores both exact port files');
  finally
    LAgent.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    SemanticPorts;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram-port-source', 'passed');
    document.body.setAttribute('data-diagram-port-source-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' named diagram port source checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-port-source', 'failed');
      document.body.setAttribute('data-diagram-port-source-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
