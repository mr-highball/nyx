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
program nyx_content_editor_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}Web,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.content, nyx.responsive,
  nyx.presentations, nyx.composition, nyx.schema, nyx.generated.view;

var
  LDocument: TNyxDocument;
  LView: TNyxNode;
  LChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

begin
  LDocument := nil;
  LView := nil;
  try
    { Execute the unchanged source exported after actual recipe editing. This
      checks generated meaning, not only syntax or source-parser admission. }
    LDocument := BuildNyxDocument;
    LDocument.Validate;
    ValidateNyxDocumentProperties(LDocument);
    {$ifdef NYX_RECIPE_REVISIONS}
    Check(LDocument.Find('workspace').Content.Count = 2, 'Revising a choice retains its accepted rule count');
    Check(LDocument.Find('workspace').Content.Rule(0).Viewport.Same(
      TNyxViewportCondition.Any.WidthBelow(720)),
      'Generated revised condition replaces the original bound');
    {$else}
    Check(LDocument.Find('workspace').Content.Count = 3, 'Generated recipe registry retains its accepted rule count');
    Check(LDocument.Find('workspace').Content.Rule(0).Viewport.Same(
      TNyxViewportCondition.Any.WidthBelow(700).HeightBelow(500).Orientation(nvoLandscape)),
      'Generated width/height/orientation retains its accepted condition');
    {$endif}
    Check((LDocument.Find('workspace').Content.Rule(1).Presentation.Name = 'focused') and
      (LDocument.Find('workspace').Content.Rule(1).Component.Name = 'reading-card'),
      'Generated named presentation retains its distinct reference');
    LView := RealizeNyxView(LDocument, LDocument.Pages[0], TNyxViewFrame.At(650, 400, npfBrowser));
    Check(LView.Find(NyxQualifiedID('workspace', 'compact-name')) <> nil,
      'Executed generated size condition chooses the authored compact structure');
    FreeAndNil(LView);
    LView := RealizeNyxView(LDocument, LDocument.Pages[0],
      TNyxViewFrame.At(900, 600, npfNativeLCL).Selecting(NyxPresentation('focused')));
    Check(LView.Find(NyxQualifiedID('workspace', 'reading-notes')) <> nil,
      'Executed generated manual presentation chooses the authored alternate family');
    WriteLn('PASS ', LChecks, ' executed editor-generated recipe checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-projection-refresh', 'passed');
    document.body.setAttribute('data-projection-refresh-checks', IntToStr(LChecks));
    {$endif}
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'failed');
      document.body.setAttribute('data-projection-refresh-error', LError.Message);
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
  LView.Free;
  LDocument.Free;
end.
