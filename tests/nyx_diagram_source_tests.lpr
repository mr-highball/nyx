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

program nyx_diagram_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.model, nyx.controls,
  nyx.data, nyx.codec, nyx.codegen, nyx.source, nyx.composition,
  nyx.diagram, nyx.diagram.content, nyx.diagram.materialization,
  nyx.diagram.declarations, nyx.diagram.adoption,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.declarations,
  nyx.root.types {$ifdef PAS2JS}, Web{$else}, Classes{$endif};
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
{ Write exact owned fixture bytes for independent complete constructor checks.
  The caller supplies a fresh evidence directory; no live workspace is addressed. }
procedure ExportDocument(const AStage: String; ADocument: TNyxDocument);
var
  LDirectory: String;

  procedure Save(const AName: String; const AText: TNyxText);
  var
    LBytes: TNyxBytes;
    LStream: TFileStream;
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
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AStage + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', TNyxCodegen.Generate(ADocument));
  Save('expected.nyx', TNyxCodec.Encode(ADocument));
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LChanged: TNyxDocument;
  LCopy: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LWrapper: INyxColumn;
  LReference: INyxComponent;
  LDiagram: INyxDiagram;
  LSnapshot: TNyxDiagramSourceSnapshot;
  LFailed: Boolean;
  LSource: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  LChanged := nil;
  LCopy := nil;
  LRead := nil;
  LWorkspace := nil;
  try
    LDocument.Title := 'Source continuity';
    LDocument.AddPage(NewNyxPage('home'));
    LDocument.AddComponent(NewNyxLabel('Leaf 🌙').WithText('Original leaf'));
    LWrapper := NewNyxColumn('Wrapper');
    LReference := NewNyxComponent('wrapped-leaf');
    LReference.Configure.Component(NyxComponent('Leaf 🌙')).Done;
    LWrapper.Add(LReference);
    LDocument.AddComponent(LWrapper);
    LDocument.AddComponent(NewNyxLabel('Unrelated').WithText('Unrelated label'));
    LDocument.AddComponent(NewNyxColumn('Canvas owner').Add(
      NewNyxCanvas('nested-canvas').WithScene(NyxCanvasRef('Nested board'))));
    LDocument.Canvases.Define(NyxCanvasRef('Nested board'), NewNyxCanvasDefinition
      .WithLayer(NyxCanvasLayer('Content')).WithItem(NyxCanvasItem('Nested leaf'),
        NyxReusableRoot('Leaf 🌙'), NyxCanvasPlacement(NyxCanvasLayer('Content'),
          NyxCanvasBounds(0, 0, 180, 60))));
    LDiagram := NewNyxDiagram.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Body'),
      ndsRectangle, NyxCanvasBounds(0, 0, 240, 160))
      .WithContent(NyxDiagramComponent(NyxComponent('Wrapper'))));
    LDocument.Diagrams.Define(NyxDiagramRef('Board'), LDiagram);
    LDocument.Validate;
    LSnapshot := NyxDiagramSourceSnapshot(LDocument, NyxDiagramRef('Board'), 7);
    Check(LSnapshot.Defined and (LSnapshot.Revision = 7) and LSnapshot.Matches(LDocument),
      'source snapshot retains exact complete accepted design and host revision');
    LChanged := LDocument.Clone;
    Check(SameNyxDiagramSource(LDocument, LChanged, NyxDiagramComponent(NyxComponent('Wrapper'))),
      'independent identical contexts retain reusable meaning');
    LChanged.FindComponent('Unrelated').Configure.Text('A different unrelated label').Done;
    LChanged.Title := 'A different project caption';
    LChanged.Diagrams.Define(NyxDiagramRef('Board'), LDiagram.WithVertex(
      LDiagram.FindVertex(NyxDiagramVertexRef('Body')).AtBounds(NyxCanvasBounds(20, 30, 240, 160))));
    Check(SameNyxDiagramSource(LDocument, LChanged, NyxDiagramComponent(NyxComponent('Wrapper'))),
      'unrelated roots/title and graph geometry do not replace an unchanged component');
    Check(not LSnapshot.Matches(LChanged), 'a source host fence still detects the entire changed accepted document');
    LChanged.FindComponent('Leaf 🌙').Configure.Text('Changed nested leaf 🌙').Done;
    LChanged.Validate;
    Check(not SameNyxDiagramSource(LDocument, LChanged, NyxDiagramComponent(NyxComponent('Wrapper'))),
      'transitive reusable changes participate in exact source meaning');
    Check(not SameNyxDiagramSource(LDocument, LChanged, NyxDiagramComponent(NyxComponent('Canvas owner'))),
      'nested saved canvas content follows its transitive source roots');
    LCopy := LSnapshot.OpenDocument;
    Check((TNyxCodec.Encode(LCopy) = TNyxCodec.Encode(LDocument)) and
      (LCopy.FindComponent('Leaf 🌙').Prop(NyxAttributeName(atText)) = 'Original leaf'),
      'snapshot reopening preserves complete independent UTF-8 source');
    LCopy.FindComponent('Leaf 🌙').Configure.Text('Private reopened change').Done;
    Check(LSnapshot.Matches(LDocument), 'editing a reopened source tree never mutates captured bytes');
    LFailed := False;
    try
      NyxDiagramSourceSnapshot(LDocument, NyxDiagramRef('Board'), -2);
    except
      on ENyxDiagram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LSnapshot.Matches(LDocument), 'invalid source fence refuses without changing an older snapshot');
    LSource := TNyxCodegen.Generate(LChanged);
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = TNyxCodec.Encode(LChanged),
      'complete changed source with nested reusable/canvas intent round trips exactly');
    {$ifndef PAS2JS}
    ExportDocument('original', LDocument);
    ExportDocument('changed', LChanged);
    {$endif}
  finally
    LWorkspace.Free;
    LRead.Free;
    LCopy.Free;
    LChanged.Free;
    LReference := nil;
    LWrapper := nil;
    LDocument.Free;
  end;
end;
begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram-source', 'passed');
    document.body.setAttribute('data-diagram-source-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' diagram source snapshot/dependency checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-source', 'failed');
      document.body.setAttribute('data-diagram-source-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
