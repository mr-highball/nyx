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

program nyx_agent_reusable_controls;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.state,
  nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, StdCtrls, Classes, nyx.application.lcl{$endif};

var
  LDocument: TNyxDocument;
  LBefore, LExpected: TNyxText;
  LApplication: {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  LChecks: Integer;
  {$ifdef PAS2JS}
  LMemo, LSibling: TJSHTMLTextAreaElement;
  LButton: TJSHTMLElement;
  LEvent: TJSEvent;
  {$else}
  LMemo, LSibling: TMemo;
  LButton: TButton;
  LStream: TFileStream;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled reusable controls: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  LDocument := nil;
  LApplication := nil;
  try
    try
      {$ifndef PAS2JS}
      Application.Initialize;

      if ParamCount <> 1 then
      begin
        raise Exception.Create('Supply the exact semantic design artifact');
      end;
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
      {$endif}
      { This is the unchanged semantic companion, compiled against the public
        Nyx interfaces. No reconstruction or regeneration substitutes its bytes. }
      LDocument := BuildNyxDocument;
      LBefore := TNyxCodec.Encode(LDocument);
      {$ifndef PAS2JS}
      Check(LBefore = LExpected, 'compiled companion reproduces exact admitted design');
      {$endif}
      Check(LDocument.State.Value('qualification').TextValue =
        TNyxText('A🌙') + #0 + TNyxText('éZ'),
        'compiled supplementary/NUL default retains exact Unicode');
      Check(LDocument.ComponentCount = 1, 'reusable definition survives compilation');
      Check(ReusableWorkshopInvocations = 0, 'retained callback helper begins independently');
      {$ifdef PAS2JS}
      LApplication := TNyxBrowserApplication.Create;
      LApplication.Run(LDocument, TJSHTMLElement(document.body));
      LMemo := TJSHTMLTextAreaElement(LApplication.View.ElementFor('first-card/reply-editor').querySelector('textarea'));
      LSibling := TJSHTMLTextAreaElement(LApplication.View.ElementFor('second-card/reply-editor').querySelector('textarea'));
      Check((LMemo <> nil) and (LSibling <> nil), 'both reusable memo controls mount');
      Check(LApplication.View.ElementFor('first-card/featured-heading').textContent = 'A featured reply',
        'replacement paints its English caption');
      Check(LApplication.View.ElementFor('second-card/reply-heading').textContent = 'Write something wonderful',
        'sibling retains inherited English caption');
      Check(LApplication.View.ElementFor('first-card/first-save').textContent = 'Save draft',
        'appended button paints its English caption');
      Check(LApplication.View.ElementFor('second-card/second-cancel').textContent = 'Cancel',
        'prepended button paints its English caption');
      Check((LMemo.value = 'Ready to compose.') and (LSibling.value = 'Ready to compose.'),
        'owned defaults initialize both bound instances');
      LMemo.value := 'A reply from the first card';
      LEvent := TJSEvent.new('input');
      LMemo.dispatchEvent(LEvent);
      Check((LSibling.value = 'A reply from the first card') and
        (LApplication.State.GetValue(NyxTextState('reply')) = 'A reply from the first card'),
        'physical input updates the deliberately shared named application binding');
      LButton := LApplication.View.ElementFor('first-card/reply-submit');
      LButton.click();
      LButton := LApplication.View.ElementFor('second-card/reply-submit');
      LButton.click();
      LButton := LApplication.View.ElementFor('original-submit');
      LButton.click();
      {$else}
      LApplication := TNyxLCLApplication.Create;
      LApplication.Mount(LDocument);
      LApplication.Window.Show;
      Application.ProcessMessages;
      LMemo := TMemo(LApplication.View.InputFor('first-card/reply-editor'));
      LSibling := TMemo(LApplication.View.InputFor('second-card/reply-editor'));
      Check((LMemo <> nil) and (LSibling <> nil), 'both reusable memo controls mount');
      Check(TLabel(LApplication.View.ControlFor('first-card/featured-heading')).Caption = 'A featured reply',
        'replacement paints its English caption');
      Check(TLabel(LApplication.View.ControlFor('second-card/reply-heading')).Caption = 'Write something wonderful',
        'sibling retains inherited English caption');
      Check(TButton(LApplication.View.ControlFor('first-card/first-save')).Caption = 'Save draft',
        'appended button paints its English caption');
      Check(TButton(LApplication.View.ControlFor('second-card/second-cancel')).Caption = 'Cancel',
        'prepended button paints its English caption');
      Check((LMemo.Text = 'Ready to compose.') and (LSibling.Text = 'Ready to compose.'),
        'owned defaults initialize both bound instances');
      LMemo.Text := 'A reply from the first card';
      Application.ProcessMessages;
      Check((LSibling.Text = 'A reply from the first card') and
        (LApplication.State.GetValue(NyxTextState('reply')) = 'A reply from the first card'),
        'native memo change updates the deliberately shared named application binding');
      LButton := TButton(LApplication.View.ControlFor('first-card/reply-submit'));
      LButton.Click;
      LButton := TButton(LApplication.View.ControlFor('second-card/reply-submit'));
      LButton.Click;
      LButton := TButton(LApplication.View.ControlFor('original-submit'));
      LButton.Click;
      {$endif}
      Check(ReusableWorkshopInvocations = 3,
        'original and copied reusable routes invoke the retained compiled callback');
      Check(LApplication.View.LastBindingError = '',
        'physical input has no binding rejection');
      Check(TNyxCodec.Encode(LDocument) = LBefore,
        'target interaction preserves exact authored defaults and ownership');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-reusable-controls', 'passed');
      document.body.setAttribute('data-nyx-reusable-checks', IntToStr(LChecks));
      {$else}
      WriteLn('PASS ', LChecks, ' exact compiled reusable/native control checks');
      {$endif}
    except
      on LException: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-nyx-reusable-controls', 'failed');
        document.body.setAttribute('data-nyx-reusable-error', LException.Message);
        {$else}
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    LApplication.Free;
    LDocument.Free;
  end;
end.
