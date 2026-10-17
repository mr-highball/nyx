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
program nyx_diagram_content_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.codegen, nyx.source,
  nyx.data, nyx.bytes, nyx.images, nyx.resources, nyx.diagram, nyx.diagram.content,
  nyx.diagram.declarations, nyx.diagram.materialization, nyx.canvas.geometry,
  nyx.studio.projects, nyx.studio.session, nyx.studio.agents, nyx.diagram.editing,
  nyx.test.diagram.content, nyx.test.diagram.creator
  {$ifdef PAS2JS}, Web{$else}, Classes, nyx.studio.mcp{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Diagram content: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure ExportPair(const AName: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;

  procedure Save(const AName, AText: TNyxText);
  var
    LFile: TFileStream;
  begin
    LFile := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if AText <> '' then
      begin
        LFile.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LFile.Free;
    end;
  end;

begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AName + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', APair.Source);
  Save('expected.nyx', APair.Design);
end;
{$endif}

procedure CheckSemantic(const APair: TNyxProjectPair);
var
  LAgent: TNyxAgentSession;
  LDocument: TNyxDocument;
  LPlan: INyxDiagram;
  LAfter: TNyxProjectPair;
  LReply: TNyxDataValue;
  LRequest: TNyxDataValue;
  LRevision: Integer;
  LFailed: Boolean;

  function Pair: TNyxProjectPair;
  begin
    Result := LAgent.PreviewPair(LAgent.Revision, 'home');
  end;

  function Request(const AID: TNyxText; const AOperations: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('operationId', NyxData(AID)),
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operations', AOperations)]);
  end;

begin
  { Production semantic dispatch, explicitly isolated from HTTP/observing
    authority. The fixture never connects to or replaces a user's project. }
  LAgent := TNyxAgentSession.Create(APair);
  LDocument := nil;
  try
    LDocument := TNyxCodec.Decode(APair.Design);
    LPlan := LDocument.Diagrams.Definition(NyxDiagramRef('Content board'));
    LRevision := LAgent.Revision;
    LReply := LAgent.Call('nyx_outline', 'Scooty content qualification', NyxObject([
      NyxField('scope', NyxData('diagram-vertices')), NyxField('name', NyxData('Content board')),
      NyxField('member', NyxData('Form')), NyxField('limit', NyxData(1)),
      NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('content').Field('component').AsText = 'Details'),
      'bounded production outline includes only the requested vertex and its typed content');
    LRequest := Request('content-group', NyxArray([
      NyxSetDiagramVertex(NyxDiagramRef('Content board'), LPlan.Vertex(0)
        .WithContent(LPlan.Vertex(0).Content.Fitted(nifCover))).ToData,
      NyxSetDiagramVertex(NyxDiagramRef('Content board'), LPlan.Vertex(1)
        .WithContent(NyxDiagramComponent(NyxComponent('Summary')).Padded(20))).ToData]));
    LReply := LAgent.Call('nyx_transaction', 'Scooty content qualification', LRequest);
    LAfter := Pair;
    Check((LAgent.Revision = LRevision + 1) and
      (LAgent.Call('nyx_transaction', 'Scooty content qualification', LRequest).ToJSON = LReply.ToJSON),
      'related image/reusable mutations publish once and exact retries reuse the receipt');
    FreeAndNil(LDocument);
    LDocument := TNyxCodec.Decode(LAfter.Design);
    Check((LDocument.Diagrams.Definition(NyxDiagramRef('Content board')).Vertex(0).Content.ImageFit = nifCover) and
      (LDocument.Diagrams.Definition(NyxDiagramRef('Content board')).Vertex(1).Content.Component.Name = 'Summary'),
      'semantic content changes retain specialized typed intent through paired source admission');
    LRequest := Request('content-refusal', NyxArray([
      NyxSetDiagramBounds(NyxDiagramRef('Content board'), LPlan.Vertex(0).Reference,
        NyxCanvasBounds(0, 0, 100, 100)).ToData,
      NyxSetDiagramVertex(NyxDiagramRef('Content board'), LPlan.Vertex(1)
        .WithContent(NyxDiagramComponent(NyxComponent('Missing reusable')))).ToData]));
    LRevision := LAgent.Revision;
    LFailed := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty content qualification', LRequest);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LAgent.Revision = LRevision) and (EncodeNyxProject(Pair) = EncodeNyxProject(LAfter)),
      'missing content dependency refuses the whole semantic group without changing either file');
    LAgent.Call('nyx_history', 'Scooty content qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('content-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(APair), 'one semantic Undo restores the complete original pair');
    LAgent.Call('nyx_history', 'Scooty content qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('content-redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(LAfter), 'one semantic Redo restores the complete authored pair');
    {$ifndef PAS2JS}
    Check((Pos('captionSpace', NyxStudioMCPTools.ToJSON) > 0) and
      (Pos('localized', NyxStudioMCPTools.ToJSON) > 0), 'current schema advertises the same typed content boundary');
    ExportPair('semantic', LAfter);
    {$endif}
  finally
    LDocument.Free;
    LAgent.Free;
  end;
end;

procedure CheckUnicode(AEnglishDocument: TNyxDocument);
var
  LDocument: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPlan: INyxDiagram;
  LReference: TNyxResourceRef;
  LPair: TNyxProjectPair;
begin
  { Dedicated qualification data exercises supplementary text and NUL without
    changing the English starter document or narrowing its portable contract. }
  LDocument := AEnglishDocument.Clone;
  LRead := nil;
  LWorkspace := nil;
  try
    LReference := NyxResourceRef('Tile 🌙');
    LDocument.Resources.Define(LReference, NyxLocale('en-US'),
      AEnglishDocument.Resources.Definition(NyxResourceRef('Tile image'), NyxDefaultLocale));
    LDocument.Resources.Define(NyxResourceRef('Supplementary text'),
      NyxTextResource(TNyxText('Packed 🌙 / O''Brien') + #0 + ' retained'));
    LPlan := LDocument.Diagrams.Definition(NyxDiagramRef('Content board'));
    LDocument.Diagrams.Define(NyxDiagramRef('Content board'), LPlan.WithVertex(LPlan.Vertex(0)
      .Captioned(TNyxText('Image 🌙 / O''Brien'))
      .WithContent(NyxDiagramImage(NyxResourceImage(LReference).Localize(NyxLocale('en-US'), NyxLocale('en'))))));
    LDocument.Validate;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LRead := TNyxSourceWorkspace.PrepareDraft(LPair.Source, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LPair.Design,
      'dedicated supplementary Unicode, NUL and localized resource selectors reconstruct exact document bytes');
    {$ifndef PAS2JS}ExportPair('unicode', LPair);{$endif}
  finally
    LWorkspace.Free;
    LRead.Free;
    LDocument.Free;
  end;
end;

{ Document dependency admission consumes one complete graph snapshot. It must
  not trust foreign/aggregated getters, lose whole-byte refusal for built-ins,
  or cache the outcome against a different resource/component registry. All
  documents and creator leases are owned here; no host or compiler is involved. }
procedure CheckContentAdmission;
var
  LDocument: TNyxDocument;
  LProbe: TNyxDocument;
  LPlan: INyxDiagram;
  LCreator: INyxDiagram;
  LFacade: TNyxTestDiagramPlan;
  LDefinition: INyxResourceDefinition;
  LBad: INyxDiagram;
  LLarge: INyxDiagram;
  LVertex: TNyxDiagramVertex;
  LCaption: TNyxText;
  LBefore: TNyxText;
  LWire: TNyxText;
  LFailed: Boolean;
  LIndex: Integer;
  LPort: Integer;
begin
  LDocument := DiagramContentDocument;
  LProbe := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LPlan := CaptureNyxDiagram(LDocument.Diagrams.Definition(NyxDiagramRef('Content board')));
    LWire := LPlan.ToData.ToJSON;
    LDocument.ValidateDiagramContent(LPlan);
    LDocument.ValidateDiagramContent(LPlan);
    Check((TNyxCodec.Encode(LDocument) = LBefore) and (LPlan.ToData.ToJSON = LWire),
      'repeated private graph admission preserves the exact document and graph');
    LFacade := TNyxTestDiagramPlan.Create(LPlan);
    LCreator := LFacade;
    LDocument.ValidateDiagramContent(LCreator);
    Check((LFacade.WireReads = 1) and (LFacade.GetterReads = 0),
      'document admission captures a foreign wire once without live getter traversal');
    LFacade.ForwardObject := True;
    LFacade.SetWire(NyxNull);
    LFailed := False;
    try
      LDocument.ValidateDiagramContent(LCreator);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LFacade.WireReads = 2) and (LFacade.GetterReads = 0),
      'malformed foreign wire refuses even with valid live getters and forwarded object identity');
    {$ifndef PAS2JS}
    Check(LFacade.ObjectQueries > 0, 'native aggregation actually exercises the object-instance bridge');
    {$endif}
    LCreator := nil;
    LFacade := nil;
    LFailed := False;
    try
      LDocument.ValidateDiagramContent(nil);
    except
      on E: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'nil graph remains a typed document refusal');
    LProbe := LDocument.Clone;
    LDefinition := LProbe.Resources.Definition(NyxResourceRef('Tile image'), NyxDefaultLocale);
    LProbe.Resources.Remove(NyxResourceRef('Tile image'), NyxDefaultLocale);
    LFailed := False;
    try
      LProbe.ValidateDiagramContent(LPlan);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LDocument.Resources.Contains(NyxResourceRef('Tile image'), NyxDefaultLocale),
      'the same admitted graph still checks independent current resource membership');
    LProbe.Resources.Define(NyxResourceRef('Tile image'), LDefinition);
    LProbe.ValidateDiagramContent(LPlan);
    Check(True, 'restoring the resource makes the same immutable graph admissible again');
    LProbe.Resources.Define(NyxResourceRef('Tile image'), NyxTextResource('Not an image'));
    LFailed := False;
    try
      LProbe.ValidateDiagramContent(LPlan);
    except
      on E: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'current resource kind remains authoritative after private capture');
    LProbe.Resources.Define(NyxResourceRef('Tile image'), LDefinition);
    LProbe.ValidateDiagramContent(LPlan);
    Check(True, 'restored image metadata is revalidated without a stale dependency result');
    LBad := CaptureNyxDiagram(LPlan.WithVertex(LPlan.FindVertex(NyxDiagramVertexRef('Form'))
      .WithContent(NyxDiagramComponent(NyxComponent('Missing reusable')))));
    LFailed := False;
    try
      LDocument.ValidateDiagramContent(LBad);
    except
      on E: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'private byte admission never substitutes for named reusable validation');
    LCaption := '';
    for LIndex := 1 to 256 do
    begin
      LCaption := LCaption + TNyxText('𝄞');
    end;
    Check(18 * MaximumNyxDiagramPortsPerVertex * NyxUTF8ByteCount(LCaption) > MaximumNyxDiagramBytes,
      'the integration fixture exceeds whole bytes within closed member/scalar limits');
    LLarge := NewNyxDiagram;
    for LIndex := 0 to 17 do
    begin
      LVertex := NyxDiagramVertex(NyxDiagramVertexRef('Large ' + TNyxText(IntToStr(LIndex))),
        ndsRectangle, NyxCanvasBounds(0, 0, 96, 64));
      for LPort := 0 to MaximumNyxDiagramPortsPerVertex - 1 do
      begin
        LVertex := LVertex.WithPort(NyxDiagramPort(NyxDiagramPortRef('Port ' +
          TNyxText(IntToStr(LPort))), 0.5, 0.5).Captioned(LCaption));
      end;
      LLarge := LLarge.WithVertex(LVertex);
    end;
    LFailed := False;
    try
      LDocument.ValidateDiagramContent(LLarge);
    except
      on E: ENyxDiagram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'document admission still enforces the whole-byte cap of a fresh private graph');
    Check((TNyxCodec.Encode(LDocument) = LBefore) and (LPlan.ToData.ToJSON = LWire),
      'all direct admission failures preserve the caller document and original immutable plan');
  finally
    LCreator := nil;
    LProbe.Free;
    LDocument.Free;
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LRead: TNyxDocument;
  LPlan: INyxDiagram;
  LPlain: INyxDiagram;
  LVertex: TNyxDiagramVertex;
  LContent: TNyxDiagramContent;
  LBeforeContent: TNyxText;
  LWire: TNyxText;
  LSource: TNyxText;
  LChanged: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LFailed: Boolean;
  LBounds: TNyxCanvasBounds;
begin
  LDocument := DiagramContentDocument;
  LCopy := nil;
  LRead := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LPlan := LDocument.Diagrams.Definition(NyxDiagramRef('Content board'));
    Check(LDocument.HasDiagramContent and (LPlan.ToData.Field('version').AsInteger = 2),
      'rich descriptors select explicit version two');
    LVertex := LPlan.FindVertex(NyxDiagramVertexRef('Image'));
    LContent := LVertex.Content;
    LBeforeContent := LContent.ToData.ToJSON;
    Check((LContent.Kind = ndckImage) and (LContent.Image.Reference.Name = 'Tile image') and
      (LContent.HeaderHeight = 32), 'typed content retains selector, fit and caption-space policy');
    LBounds := NyxDiagramContentBounds(LVertex);
    Check((LBounds.Left = -248) and (LBounds.Top = -56) and
      (LBounds.Width = 216) and (LBounds.Height = 164), 'content geometry retains exact signed world coordinates');
    LVertex := LVertex.AtBounds(NyxCanvasBounds(-33.5, 12.25, 320, 200)).Captioned('New caption');
    Check(LVertex.Content.ToData.ToJSON = LBeforeContent, 'geometry and caption edits retain independent content intent');
    Check(LContent.Padded(2).Padding = 2, 'fluent derivation returns a new content value');
    Check(LContent.ToData.ToJSON = LBeforeContent, 'fluent derivation preserves the retained original');
    LBounds := NyxDiagramContentBounds(LVertex.AtBounds(NyxCanvasBounds(0, 0, 8, 8)));
    Check((LBounds.Width = 0) and (LBounds.Height = 0), 'small shapes clip content to no area without inventing extent');
    LPlain := NewNyxDiagram.WithVertex(LVertex.WithoutContent);
    Check((LPlain.ToData.Field('version').AsInteger = 1) and
      (LPlain.ToData.Field('vertices').Item(0).Count = 5), 'no-content graphs retain version-one field shape');
    LFailed := False;
    try
      NyxDiagramFromData(NyxObject([NyxField('version', NyxData(1)),
        NyxField('vertices', LPlan.ToData.Field('vertices')),
        NyxField('edges', LPlan.ToData.Field('edges'))]));
    except
      on ENyxDiagram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'rich fields cannot enter through a forged version-one descriptor');
    LWire := TNyxCodec.Encode(LDocument);
    LCopy := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LCopy) = LWire, 'entire document/resources/reusables round-trip exactly');
    LCopy.Diagrams.Define(NyxDiagramRef('Content board'),
      LPlan.WithVertex(LPlan.FindVertex(NyxDiagramVertexRef('Form')).WithContent(
        NyxDiagramComponent(NyxComponent('Missing reusable')))));
    LFailed := False;
    try
      LCopy.Validate;
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LWire),
      'missing reusable dependencies refuse without changing independent accepted owners');
    FreeAndNil(LCopy);
    LCopy := LDocument.Clone;
    LCopy.Diagrams.Define(NyxDiagramRef('Content board'),
      LPlan.WithVertex(LPlan.FindVertex(NyxDiagramVertexRef('Image')).WithContent(
        NyxDiagramImage(NyxResourceImage(NyxResourceRef('Hosted tile'))))));
    LCopy.Validate;
    Check(LCopy.Diagrams.Definition(NyxDiagramRef('Content board')).Vertex(0)
      .Content.Image.Reference.Name = 'Hosted tile', 'hosted resources remain typed abstraction references');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('.WithContent(NyxDiagramImage(', LSource) > 0) and
      (Pos('.WithContent(NyxDiagramComponent(', LSource) > 0) and
      (Pos('.CaptionSpace(32)', LSource) > 0), 'crafted Pascal uses fluent typed content without serialized blobs');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LWire, 'literal source reconstructs the complete rich document');
    FreeAndNil(LRead);
    LBefore := NyxProjectPair(LWire, LSource);
    CheckSemantic(LBefore);
    CheckUnicode(LDocument);
    LSession := TNyxStudioSession.Create(LBefore);
    LChanged := StringReplace(LSource, '.Fitted(nifContain)', '.Fitted(nifCover)', []);
    LSession.SetSourceDraft(LChanged);
    LSession.ApplySourceDraft;
    LAfter := LSession.ProjectSnapshot;
    Check((LAfter.Source = LChanged) and
      (LSession.Document.Diagrams.Definition(NyxDiagramRef('Content board')).Vertex(0)
        .Content.ImageFit = nifCover), 'source Apply publishes typed content changes with exact Pascal');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one Undo restores the exact rich design/source pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores the exact edited rich pair');
    {$ifndef PAS2JS}
    ExportPair('original', LBefore);
    ExportPair('edited', LAfter);
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LRead.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

begin
  try
    CheckContentAdmission;
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram-content', 'passed');
    document.body.setAttribute('data-diagram-content-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' diagram content/source checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-content', 'failed');
      document.body.setAttribute('data-diagram-content-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
