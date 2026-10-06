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

program nyx_resize_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view,
  nyx.designer.resize,
  {$ifdef PAS2JS}Web, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl;{$endif}

var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LChecks: Integer;
  LBefore: TNyxResizeSize;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LInput: TJSHTMLTextAreaElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LInput: TMemo;
  LStream: TFileStream;
  LExpected: TNyxText;
  LChange: TNotifyEvent;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Compiled resize controls: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    LDocument := BuildNyxDocument;
    LCandidate := nil;
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.getElementById('fixture'));
    LRenderer := TNyxBrowserRenderer.Create;
    {$else}
    LHost := TForm.CreateNew(nil);
    LHost.SetBounds(20, 20, 800, 600);
    LHost.Show;
    LRenderer := TNyxLCLRenderer.Create;
    {$endif}
    try
      {$ifndef PAS2JS}
      LStream := TFileStream.Create(ParamStr(1), fmOpenRead);
      try
        SetLength(LExpected, LStream.Size);

        if LExpected <> '' then
        begin
          LStream.ReadBuffer(LExpected[1], Length(LExpected));
        end;
      finally
        LStream.Free;
      end;
      Check(TNyxCodec.Encode(LDocument) = LExpected, 'unchanged compiled source matches exact design export');
      {$endif}
      LRenderer.Render(LDocument, LDocument.Find('home'), LHost);
      Check(LRenderer.SizeFor('notes-editor').SameSize(NyxResizeSize(272, 168)),
        'unchanged admitted source allocates the expected memo dimensions');
      Check(LRenderer.SizeFor('other-editor').SameSize(NyxResizeSize(160, 120)),
        'companion control remains independent');
      {$ifdef PAS2JS}
      LInput := TJSHTMLTextAreaElement(LRenderer.InputFor('notes-editor'));
      LInput.value := 'Retain these English notes.';
      LInput.selectionStart := 2;
      LInput.selectionEnd := 7;
      LInput.focus;
      {$else}
      LInput := TMemo(LRenderer.InputFor('notes-editor'));
      LChange := LInput.OnChange;
      LInput.OnChange := nil;
      try
        LInput.Text := 'Retain these English notes.';
      finally
        LInput.OnChange := LChange;
      end;
      LInput.SelStart := 2;
      LInput.SelLength := 5;
      LInput.SetFocus;
      {$endif}
      LDocument.Find('notes-editor').Configure.Width(288).Height(176).Done;
      Check(LRenderer.TryRefresh(LDocument, LDocument.Find('home'), False),
        'size-only admission retains the existing control projection');
      Check(LRenderer.SizeFor('notes-editor').SameSize(NyxResizeSize(288, 176)),
        'retained refresh updates actual outer-face allocation');
      {$ifdef PAS2JS}
      Check((LRenderer.InputFor('notes-editor') = LInput) and
        (LInput.value = 'Retain these English notes.') and
        (LInput.selectionStart = 2) and (LInput.selectionEnd = 7) and
        (document.activeElement = LInput), 'browser input identity/text/selection/focus survive resizing');
      {$else}
      Check((LRenderer.InputFor('notes-editor') = LInput) and
        (LInput.Text = 'Retain these English notes.') and
        (LInput.SelStart = 2) and (LInput.SelLength = 5) and
        (LHost.ActiveControl = LInput), 'native input identity/text/selection/focus survive resizing');
      {$endif}
      LBefore := LRenderer.SizeFor('notes-editor');
      LCandidate := LDocument.Clone;
      LCandidate.Find('notes-editor').SetProp('height', 'invalid');
      LRefused := False;
      try
        LRenderer.TryRefresh(LCandidate, LCandidate.Find('home'), False);
      except
        on LException: Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and LRenderer.SizeFor('notes-editor').SameSize(LBefore),
        'invalid new dimensions refuse before changing accepted controls');
    finally
      LRenderer.Free;
      {$ifndef PAS2JS}LHost.Free;{$endif}
      LCandidate.Free;
      LDocument.Free;
    end;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-result', 'passed');
    document.getElementById('result').textContent := 'PASS ' + IntToStr(LChecks) + ' compiled resize controls';
    {$else}WriteLn('PASS ', LChecks, ' unchanged compiled resize controls');{$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.getElementById('result').textContent := LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
