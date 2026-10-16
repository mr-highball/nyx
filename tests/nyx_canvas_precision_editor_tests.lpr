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
}program nyx_canvas_precision_editor_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.canvas.geometry, nyx.canvas.precision, nyx.canvas.declarations, nyx.canvas.editor,
  nyx.canvas.editing, nyx.canvas.types, nyx.canvas.precision.interaction,
  nyx.studio.presentation {$ifdef PAS2JS}, Web{$endif};
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

procedure Run;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LForm: INyxCard;
  LDefinition: INyxCanvasDefinition;
  LOptions: TNyxCanvasPrecisionOptions;
  LDraft: TNyxCanvasPrecisionDraft;
  LCopy: TNyxCanvasPrecisionDraft;
  LPresentation: TNyxStudioPresentation;
  LBefore: TNyxText;
  LChange: TNyxCanvasBoundaryEditorChange;
  LFailed: Boolean;
  LInteraction: INyxCanvasGuideInteraction;
  LCamera: TNyxCanvasCamera;

  procedure Field(APart: TNyxCanvasPrecisionPart; const AValue: TNyxText);
  begin
    LForm.Node.Find(NyxCanvasPrecisionPart('precision', APart).ID).Configure.Value(AValue).Done;
  end;

  function Button(APart: TNyxCanvasPrecisionPart): TNyxNode;
  begin
    Result := LForm.Node.Find(NyxCanvasPrecisionPart('precision', APart).ID);
  end;

  procedure Rebuild;
  begin

    if LForm <> nil then
    begin
      LPage.Node.Remove(LForm.Node);
      LForm := nil;
    end;
    LForm := NewNyxCanvasPrecisionEditor('precision', NyxCanvasRef('Board'), LDocument);
    LPage.Add(LForm);
  end;
begin
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LDefinition := NewNyxCanvasDefinition;
    LDocument.Canvases.Define(NyxCanvasRef('Board'), LDefinition);
    LDraft := Default(TNyxCanvasPrecisionDraft);
    Rebuild;
    LDraft.Capture('precision', LPage.Node);
    Check(not LDraft.Defined, 'untouched form creates no retained draft');
    LPresentation := DefaultNyxStudioPresentation;
    LBefore := EncodeNyxStudioPresentation(LPresentation);
    Check(TNyxDataValue.ParseJSON(LBefore).Field('version').AsInteger = 16,
      'unused precision retains legacy presentation version');
    Field(ncppOriginX, '-unfinished');
    LDraft.Capture('precision', LPage.Node);
    Check(LDraft.Defined, 'invalid unfinished numeric text is valid retained input');
    LPresentation.CanvasPrecisionDraft := LDraft;
    LBefore := EncodeNyxStudioPresentation(LPresentation);
    Check(TNyxDataValue.ParseJSON(LBefore).Field('version').AsInteger = 21,
      'only an edited precision form opts into presentation version twenty-one');
    LPresentation := DecodeNyxStudioPresentation(LBefore);
    Check(EncodeNyxStudioPresentation(LPresentation) = LBefore,
      'complete precision presentation round trips exactly');
    LCopy := TNyxCanvasPrecisionDraft.FromData(LDraft.ToData);
    Rebuild;
    Check(LCopy.Restore(LPage.Node) and
      (Button(ncppOriginX).Prop(NyxAttributeName(atValue)) = '-unfinished'),
      'copied draft restores exact unfinished text into matching rebuilt form');
    LFailed := False;
    try
      CaptureNyxCanvasPrecisionEditor(Button(ncppApply), LPage.Node, LDocument, LChange);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LDocument.Canvases.Definition(NyxCanvasRef('Board')).ToData.ToJSON =
      LDefinition.ToData.ToJSON), 'invalid Apply leaves accepted definition exact');
    Field(ncppOriginX, '15.25');
    Field(ncppAxes, 'Top and left');
    Field(ncppGuideAction, 'Add or update guide');
    Field(ncppGuideName, 'Left margin');
    Field(ncppGuidePosition, '-25.5');
    LDraft.Capture('precision', LPage.Node);
    Check(CaptureNyxCanvasPrecisionEditor(Button(ncppApply), LPage.Node, LDocument, LChange),
      'public Apply captures one typed complete saved-scene proposal');
    LOptions := NyxCanvasPrecisionOptions.WithRulers(
      NyxCanvasRulers.FromOrigin(NyxCanvasPoint(15.25, 0)))
      .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Left margin'), ncaxX, -25.5));
    LDocument.Canvases.Define(NyxCanvasRef('Board'), NyxCanvasPrecision(LDefinition).Options(LOptions).Done);
    Rebuild;
    Check(not LDraft.Restore(LPage.Node) and not LDraft.Defined,
      'exact successful application consumes its completed retained proposal');
    Field(ncppGuideName, 'Different name');
    Field(ncppOriginX, '99');
    Field(ncppGuideChoice, 'Guide / Left margin');
    Check(RouteNyxCanvasPrecisionGuide(Button(ncppLoad), LPage.Node),
      'local Load uses the same public form contract');
    Check((Button(ncppGuideName).Prop(NyxAttributeName(atValue)) = 'Left margin') and
      (Button(ncppOriginX).Prop(NyxAttributeName(atValue)) = '99'),
      'Load replaces only guide fields and preserves unfinished ruler text');
    LDraft.Capture('precision', LPage.Node);
    LDocument.Canvases.Define(NyxCanvasRef('Board'),
      LDocument.Canvases.Definition(NyxCanvasRef('Board')).WithCamera(
        NyxCanvasCamera(500, 400, NyxCanvasPoint(10, 20))));
    Rebuild;
    Check(not LDraft.Restore(LPage.Node) and LDraft.Defined,
      'unrelated scene changes park the old exact-context proposal');
    LFailed := False;
    LDocument.Canvases.Define(NyxCanvasRef('Board'), LDefinition);
    try
      CaptureNyxCanvasPrecisionEditor(Button(ncppApply), LPage.Node, LDocument, LChange);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'stale form refuses before producing a source command');

    LOptions := LOptions
      .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Snap target'), ncaxX, 13).Locked(True))
      .Snapping(ncgspVisible, 8);
    LInteraction := NewNyxCanvasGuideInteraction(LOptions);
    LBefore := LOptions.ToData.ToJSON;
    LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0));
    LInteraction.Select(NyxCanvasGuideRef('Left margin'));
    LInteraction.BeginMove(NyxCanvasGuideRef('Left margin'));
    LInteraction.Move(NyxCanvasPoint(14, 0), LCamera,
      NyxCanvasGrid(10, ncgsLines, ncsmGrid));
    Check((LInteraction.Accepted.ToData.ToJSON = LBefore) and
      (LInteraction.Display.Named(NyxCanvasGuideRef('Left margin')).Position = 13),
      'nearby guide wins over grid without changing accepted intent');
    LInteraction.Cancel;
    Check((LInteraction.Accepted.ToData.ToJSON = LBefore) and
      (LInteraction.Selection.Name = 'Left margin') and not LInteraction.Active,
      'cancel restores exact accepted options and prior selection');
    LInteraction.BeginCreate(ncaxY, NyxCanvasPoint(0, 30));
    LInteraction.Move(NyxCanvasPoint(0, 40), LCamera, Default(TNyxCanvasGrid));
    Check(LInteraction.Commit and not LInteraction.Commit and
      (LInteraction.Accepted.Count = LOptions.Count + 1),
      'release commits once; repeated release creates no duplicate guide');
    LFailed := False;
    LBefore := LInteraction.Accepted.ToData.ToJSON;
    try
      LInteraction.BeginMove(NyxCanvasGuideRef('Snap target'));
    except
      on ENyxCanvasPrecision do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LInteraction.Active and
      (LInteraction.Accepted.ToData.ToJSON = LBefore), 'locked movement refuses atomically');
  finally
    LForm := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;
begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-precision-editor', 'passed');
    document.body.setAttribute('data-canvas-precision-editor-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' precision editor/draft/gesture checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-precision-editor', 'failed');
      document.body.setAttribute('data-canvas-precision-editor-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
