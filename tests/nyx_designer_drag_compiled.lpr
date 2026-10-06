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
program nyx_designer_drag_compiled;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  {$ifdef PAS2JS}Web, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl;{$endif}

var
  LDocument: TNyxDocument;
  LChecks: Integer;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LMemo: TJSHTMLTextAreaElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LMemo: TMemo;
  LStream: TFileStream;
  LExpected: TNyxText;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Compiled designer drag: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    { Compile and consume the exact adjacent Pascal exported by ordinary physical
      Studio input. No second tree or rewritten generated unit substitutes it. }
    LDocument := BuildNyxDocument;
    {$ifdef PAS2JS}
    LRenderer := TNyxBrowserRenderer.Create;
    LHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(LHost);
    {$else}
    LRenderer := TNyxLCLRenderer.Create;
    LHost := TForm.CreateNew(nil);
    LHost.SetBounds(0, 0, 960, 640);
    LHost.HandleNeeded;
    {$endif}
    try
      Check(LDocument.Find('notes-editor').Parent.ID = 'right-layout', 'moved memo owns its new layout');
      Check(LDocument.Find('right-layout').Children[0].ID = 'notes-editor', 'relative order survives compilation');
      Check(LDocument.Find('labeled-button-1').Kind = 'labeled-button', 'specialized compound survives compilation');
      {$ifndef PAS2JS}
      LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
      try
        SetLength(LExpected, LStream.Size);

        if LExpected <> '' then
        begin
          LStream.ReadBuffer(LExpected[1], Length(LExpected));
        end;
      finally
        LStream.Free;
      end;
      Check(TNyxCodec.Encode(LDocument) = LExpected, 'unchanged source reconstructs the exact admitted design');
      {$endif}
      LRenderer.Render(LDocument, LDocument.Find('home'), LHost, False);
      {$ifdef PAS2JS}
      LMemo := TJSHTMLTextAreaElement(LRenderer.ElementFor('notes-editor').querySelector('textarea'));
      Check((LMemo <> nil) and (LMemo.value = 'Keep these notes.'), 'memo content reaches the actual browser input');
      Check(LRenderer.ElementFor('labeled-button-1-part-2').textContent = 'Continue',
        'compound action reaches its browser button');
      LMemo.value := 'Notes after compilation.';
      LMemo.dispatchEvent(TJSEvent.new('input'));
      Check(LMemo.value = 'Notes after compilation.', 'actual browser memo remains editable');
      {$else}
      LMemo := TMemo(LRenderer.InputFor('notes-editor'));
      Check((LMemo <> nil) and (TNyxText(LMemo.Text) = 'Keep these notes.'), 'memo content reaches the actual native input');
      Check(TButton(LRenderer.ControlFor('labeled-button-1-part-2')).Caption = 'Continue',
        'compound action reaches its native button');
      LMemo.Text := 'Notes after compilation.';
      LMemo.OnChange(LMemo);
      Check(TNyxText(LMemo.Text) = 'Notes after compilation.', 'actual native memo remains editable');
      {$endif}
      WriteLn('Compiled designer drag checks passed: ', LChecks);
      {$ifdef PAS2JS}document.body.setAttribute('data-result', 'passed');{$endif}
    finally
      LRenderer.Free;
      LDocument.Free;
      {$ifndef PAS2JS}LHost.Free;{$endif}
    end;
  except
    on LError: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.textContent := LError.Message;
      {$else}
      WriteLn(LError.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
