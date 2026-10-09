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

program nyx_unit_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, Interfaces, Forms, StdCtrls, nyx.render.lcl{$endif};

{$I expected-design.inc}

const
  CHeading: TNyxText = 'A thoughtful workspace 🌟';

var
  LDocument: TNyxDocument;
  LChecks: Integer;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled semantic source: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    LDocument := BuildNyxDocument;
    try
      Check(TNyxCodec.Encode(LDocument) = ExpectedDesign,
        'Exact authored Pascal reconstructs accepted design');
      Check(TWorkshopTheme.Caption = CHeading, 'Authored class declaration and implementation execute');
      Check(WorkshopNote = CHeading, 'Changed handwritten helper calls the authored class');
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
        LRenderer.Render(LDocument, LDocument.Find('home'), LHost);
        {$ifdef PAS2JS}
        Check(LRenderer.ElementFor('workshop-heading').textContent = CHeading,
          'Browser heading receives admitted supplementary text');
        {$else}
        LHost.Show;
        Application.ProcessMessages;
        Check(TNyxText(TCustomLabel(LRenderer.ControlFor('workshop-heading')).Caption) = CHeading,
          'Win32 label receives admitted supplementary text');
        {$endif}
        Check(LRenderer.Root.Find('home').Prop('gap') = '16', 'Untouched typed layout is retained');
      finally
        LRenderer.Free;
        {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
      end;
    finally
      LDocument.Free;
    end;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' exact compiled source checks';
    document.body.setAttribute('data-nyx-unit-controls', 'passed');
    {$else}
    WriteLn('PASS ', LChecks, ' exact compiled source checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-nyx-unit-controls', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
