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


program nyx_declaration_controls;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.events, nyx.scheduler, nyx.callbacks,
  nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, Interfaces, Forms, StdCtrls, Classes, nyx.render.lcl{$endif};

var
  LDocument: TNyxDocument;
  LStream: INyxEventStream;
  LChecks: Integer;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LInput: TJSHTMLTextAreaElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LInput: TCustomEdit;
  LFile: TFileStream;
  LExpected: TNyxText;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled helpers: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure Edit(const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  LInput.value := AText;
  LInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  LInput.Text := String(AText);
  {$endif}
end;

function InputText: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := LInput.value;
  {$else}
  Result := TNyxText(LInput.Text);
  {$endif}
end;

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    LDocument := BuildNyxDocument;
    try
      Check(TNotePolicy.Limit = 5, 'Actual compiler resolves changed private parameter/result types through the retained policy method');
      Check(EnglishCaption('Note') = 'Note: up to 5 characters',
        'Actual compiler resolves the changed public parameter and both signature counterparts');
      {$ifndef PAS2JS}
      LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
      try
        SetLength(LExpected, LFile.Size);

        if LExpected <> '' then
        begin
          LFile.ReadBuffer(LExpected[1], Length(LExpected));
        end;
      finally
        LFile.Free;
      end;
      Check(TNyxCodec.Encode(LDocument) = LExpected, 'Exact emitted companion reconstructs saved design bytes');
      {$endif}
      {$ifdef PAS2JS}
      LRenderer := TNyxBrowserRenderer.Create;
      LHost := TJSHTMLElement(document.createElement('main'));
      document.body.appendChild(LHost);
      {$else}
      LRenderer := TNyxLCLRenderer.Create;
      LHost := TForm.CreateNew(nil);
      LHost.SetBounds(0, 0, 640, 480);
      {$endif}
      try
        BindNyxCallbacks(LDocument, LRenderer.Events);
        LRenderer.Render(LDocument, LDocument.Find('helper-workshop'), LHost);
        {$ifndef PAS2JS}
        { Actual memo notifications need a mounted native widget handle. }
        LHost.Show;
        Application.ProcessMessages;
        {$endif}
        {$ifdef PAS2JS}
        LInput := TJSHTMLTextAreaElement(LRenderer.ElementFor('short-note').querySelector('textarea'));
        {$else}
        LInput := TCustomEdit(LRenderer.InputFor('short-note'));
        {$endif}
        LStream := LRenderer.Events.OnBeforeTextInput(NyxControlEvents(
          LRenderer.Root.Find('short-note').ID, niRuntime));
        Check(LStream.Count = 1, 'Actual generated handler registration binds to the memo');
        Edit('abcde');
        Check((InputText = 'abcde') and (LRenderer.Root.Find('short-note').Prop('value') = 'abcde'),
          'Real physical text input admits the edited helper limit');
        Check(LStream.Registrations[0].LastExecution.Status = nesSucceeded,
          'Compiled callback really executes');
        Edit('abcdef');
        Check(InputText = 'abcde', 'Edited helper rejects excessive text physically');
        Edit(TNyxText('a🌙bcd'));
        Check(InputText = TNyxText('a🌙bcd'), 'Supplementary Unicode counts one scalar');
        Edit(TNyxText('a🌙bcde'));
        Check((InputText = TNyxText('a🌙bcd')) and
          (LRenderer.Root.Find('short-note').Prop('value') = TNyxText('a🌙bcd')),
          'Rejected Unicode input preserves exact control and model');
        LStream := nil;
      finally
        LStream := nil;
        LRenderer.Free;
        {$ifdef PAS2JS}
        LHost.remove;
        {$else}
        LHost.Free;
        {$endif}
      end;
    finally
      LDocument.Free;
    end;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-declaration-controls', 'passed');
    {$else}
    WriteLn('PASS ', LChecks, ' exact compiled declaration/native input checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-declaration-controls', 'failed');
      document.body.setAttribute('data-nyx-declaration-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
