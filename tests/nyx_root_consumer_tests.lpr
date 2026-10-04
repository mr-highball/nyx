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
program nyx_root_consumer_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.sample, nyx.events,
  nyx.callbacks,
  nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, nyx.render.lcl;{$endif}

var
  GDocument: TNyxDocument;
  GOriginal: TNyxDocument;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Release;
begin
  FreeAndNil(GRenderer);
  {$ifdef PAS2JS}

  if GHost <> nil then
  begin
    GHost.remove;
    GHost := nil;
  end;
  {$else}FreeAndNil(GHost);{$endif}
  FreeAndNil(GDocument);
  FreeAndNil(GOriginal);
end;

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    GDocument := BuildNyxDocument;
    GOriginal := CreateNyxSample;
    Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(GOriginal),
      'Unchanged compiled MCP source restores every unrelated design byte');
    Check((GDocument.Find('review-workshop') = nil) and
      (GDocument.Find('review-definition') = nil), 'Both owned review roots remain removed');
    GDocument.Validate;
    {$ifdef PAS2JS}
    GRenderer := TNyxBrowserRenderer.Create;
    GHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(GHost);
    {$else}
    GRenderer := TNyxLCLRenderer.Create;
    GHost := TForm.CreateNew(nil);
    GHost.SetBounds(0, 0, 640, 480);
    {$endif}
    { The runtime binder must ignore retained helper classes when the admitted
      document no longer owns any registration for their retired controls. }
    BindNyxCallbacks(GDocument, GRenderer.Events);
    Check(GRenderer.Events.OnAfterTextInput(NyxControlEvents('review-note')).Count = 0,
      'Retained Pascal class does not register a retired callback');
    GRenderer.Render(GDocument, GDocument.Find('home'), GHost);
    Check(GRenderer.Root.Find('project-name') <> nil, 'Surviving home input renders through its actual adapter');
    GRenderer.Render(GDocument, GDocument.FindComponent('welcome-card'), GHost);
    Check(GRenderer.Root <> nil, 'Surviving reusable component renders independently');
    Release;
    WriteLn('PASS ', GChecks, ' compiled MCP root cleanup control checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-root-consumers', 'passed');
    document.body.setAttribute('data-nyx-root-consumer-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      Release;
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-root-consumers', 'failed');
      document.body.setAttribute('data-nyx-root-consumer-error', LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
