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
program nyx_diagram_document_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.diagram, nyx.diagram.declarations,
  nyx.diagram.editor, nyx.generated.view, nyx.test.diagram.document
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LClone: TNyxDocument;
  LPlan: INyxDiagram;
  LEditor: INyxDiagramEditor;
  LChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Compiled diagram document: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  LDocument := nil;
  LExpected := nil;
  LClone := nil;
  try
    try
      LExpected := DiagramDocumentFixture;
      {$ifdef NYX_DIAGRAM_CHANGED}
      LPlan := LExpected.Diagrams.Definition(NyxDiagramRef('Publishing workflow'));
      LExpected.Diagrams.Define(NyxDiagramRef('Publishing workflow'),
        LPlan.WithVertex(LPlan.Vertex(1).Captioned('Visual proposal')));
      {$endif}
      {$ifdef NYX_DIAGRAM_REMOVED}
      LExpected.Diagrams.Remove(NyxDiagramRef('Publishing workflow'));
      {$endif}
      {$ifdef NYX_DIAGRAM_RECREATED}
      LPlan := LExpected.Diagrams.Definition(NyxDiagramRef('Publishing workflow'));
      LExpected.Diagrams.Remove(NyxDiagramRef('Publishing workflow'))
        .Define(NyxDiagramRef('Publishing workflow'), LPlan);
      {$endif}
      LDocument := BuildNyxDocument;
      Check(TNyxCodec.Encode(LDocument) = TNyxCodec.Encode(LExpected),
        'executed public builder reproduces the entire independent expected candidate');
      LClone := LDocument.Clone;
      Check(TNyxCodec.Encode(LClone) = TNyxCodec.Encode(LDocument),
        'executed declarations survive independent document cloning');

      if LDocument.Diagrams.Contains(NyxDiagramRef('Publishing workflow')) then
      begin
        LPlan := LDocument.Diagrams.Definition(NyxDiagramRef('Publishing workflow'));
        LEditor := NewNyxDiagramEditor('reopened', LDocument, NyxDiagramRef('Publishing workflow'));
        LDocument.Free;
        LDocument := nil;
        Check(LEditor.InitialDiagram.ToData.ToJSON = LPlan.ToData.ToJSON,
          'compiled saved intent reopens after source document retirement');
      end;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-generated', 'passed');
      document.body.setAttribute('data-diagram-generated-checks', IntToStr(LChecks));
      {$else}
      WriteLn('PASS ', LChecks, ' compiled exact diagram document checks');
      {$endif}
    except
      on E: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-diagram-generated', 'failed');
        document.body.setAttribute('data-diagram-generated-error', E.Message);
        {$else}
        WriteLn('FAIL ', E.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    LClone.Free;
    LExpected.Free;
    LDocument.Free;
  end;
end.
