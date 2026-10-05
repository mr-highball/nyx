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
program nyx_review_consumer_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.events, nyx.scheduler, nyx.callbacks,
  nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, StdCtrls, nyx.render.lcl;{$endif}

var
  GDocument: TNyxDocument;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Drive the actual adapter control. This programmatic input qualifies complete
  proposed-value handling and rollback; trusted browser host typing is exercised
  separately by the real protocol journey against its compiled application. }
procedure Edit(const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GRenderer.InputFor('review-quantity')).value := AText;
  GRenderer.InputFor('review-quantity').dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(GRenderer.InputFor('review-quantity')).Text := AText;
  {$endif}
end;

function Value: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(GRenderer.InputFor('review-quantity')).value;
  {$else}
  Result := TNyxText(TCustomEdit(GRenderer.InputFor('review-quantity')).Text);
  {$endif}
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
  {$else}
  FreeAndNil(GHost);
  {$endif}
  FreeAndNil(GDocument);
end;

procedure Run;
var
  LStream: INyxEventStream;
const
  CTitle: TNyxText = 'Moonlit workshop 🌙漢字';
  CInvalidQuantity: TNyxText = '12🌙';
begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  { The build imports the exact bounded MCP export. The fixture neither rewrites
    its generated class nor substitutes a callback implementation at runtime. }
  GDocument := BuildNyxDocument;
  Check((GDocument.Count = 1) and (GDocument.Find('review-workshop') <> nil),
    'Compiled review owns its exact authored root');
  Check(GDocument.Find('review-title').Prop('text') = CTitle,
    'Compiled source retains exact supplementary Unicode');
  {$ifdef PAS2JS}
  GRenderer := TNyxBrowserRenderer.Create;
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  {$else}
  GRenderer := TNyxLCLRenderer.Create;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(0, 0, 640, 480);
  {$endif}
  BindNyxCallbacks(GDocument, GRenderer.Events);
  GRenderer.Render(GDocument, GDocument.Find('review-workshop'), GHost);
  LStream := GRenderer.Events.OnBeforeTextInput(NyxControlEvents(
    GRenderer.Root.Find('review-quantity').ID, niRuntime));
  Check(LStream.Count = 1, 'Actual generated class resolves through the public startup registry');
  Edit('12');
  Check((Value = '12') and (GRenderer.Root.Find('review-quantity').Prop('value') = '12'),
    'Complete numeric input is admitted by the compiled callback');
  Check(LStream.Registrations[0].LastExecution.Status = nesSucceeded,
    'Actual callback execution succeeds');
  Edit('12x');
  Check((Value = '12') and (GRenderer.Root.Find('review-quantity').Prop('value') = '12'),
    'Invalid ASCII input atomically retains control and model');
  Edit(CInvalidQuantity);
  Check(Value = '12', 'Invalid supplementary Unicode input retains the complete accepted value');
  Edit('');
  Check(Value = '', 'Clearing is admitted independently of the review lifetime');
  LStream := nil;
  Release;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' compiled review control checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-review-consumers', 'passed');
    document.body.setAttribute('data-nyx-review-consumer-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      Release;
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-review-consumers', 'failed');
      document.body.setAttribute('data-nyx-review-consumer-error', LException.Message);
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
