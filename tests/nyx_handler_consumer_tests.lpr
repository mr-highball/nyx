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
program nyx_handler_consumer_tests;

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

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Text(const AID, AText: TNyxText);
{$ifdef PAS2JS}
var
  LElement: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LElement := GRenderer.ElementFor(AID);

  if not (LElement is TJSHTMLInputElement) and not (LElement is TJSHTMLTextAreaElement) then
  begin
    LElement := TJSHTMLElement(LElement.querySelector('input,textarea'));
  end;

  if LElement is TJSHTMLInputElement then
  begin
    TJSHTMLInputElement(LElement).value := AText;
  end
  else
  begin
    TJSHTMLTextAreaElement(LElement).value := AText;
  end;
  LElement.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(GRenderer.InputFor(AID)).Text := AText;
  {$endif}
end;

function TextValue(const AID: TNyxText): TNyxText;
{$ifdef PAS2JS}
var
  LElement: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LElement := GRenderer.ElementFor(AID);

  if not (LElement is TJSHTMLInputElement) and not (LElement is TJSHTMLTextAreaElement) then
  begin
    LElement := TJSHTMLElement(LElement.querySelector('input,textarea'));
  end;

  if LElement is TJSHTMLInputElement then
  begin
    Result := TJSHTMLInputElement(LElement).value;
  end
  else
  begin
    Result := TJSHTMLTextAreaElement(LElement).value;
  end;
  {$else}
  Result := TNyxText(TCustomEdit(GRenderer.InputFor(AID)).Text);
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
  {$else}FreeAndNil(GHost);{$endif}
  FreeAndNil(GDocument);
end;

var
  LMemo: TNyxText;
  LStream: INyxEventStream;
begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    GDocument := BuildNyxDocument;
    Check(GDocument.Find('handler-workshop') <> nil, 'Compile the exact MCP-authored review');
    {$ifdef PAS2JS}
    GRenderer := TNyxBrowserRenderer.Create;
    GHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(GHost);
    {$else}
    GRenderer := TNyxLCLRenderer.Create;
    GHost := TForm.CreateNew(nil);
    GHost.SetBounds(0, 0, 640, 480);
    {$endif}
    { Resolve the exact generated classes through the public application
      startup contract, just as the delegated application bootstrap does. }
    BindNyxCallbacks(GDocument, GRenderer.Events);
    GRenderer.Render(GDocument, GDocument.Find('handler-workshop'), GHost);
    LStream := GRenderer.Events.OnBeforeTextInput(NyxControlEvents(
      GRenderer.Root.Find('number-input').ID, niRuntime));
    Check(LStream.Count = 1, 'The compiled authored registration resolves its actual callback');
    Text('number-input', '12');
    Check((TextValue('number-input') = '12') and
      (GRenderer.Root.Find('number-input').Prop('value') = '12'), 'ASCII digits are accepted physically and in the model');
    Check(LStream.Registrations[0].LastExecution.Status = nesSucceeded, 'Authored local-function callback actually executes');
    Text('number-input', '12x');
    Check((TextValue('number-input') = '12') and
      (GRenderer.Root.Find('number-input').Prop('value') = '12'), 'Invalid character is rejected atomically by the compiled callback');
    Text('number-input', '12🌙');
    Check(TextValue('number-input') = '12', 'Non-ASCII input is rejected without partial state');
    Text('number-input', '');
    Check(TextValue('number-input') = '', 'Clearing remains a valid editing operation');
    LMemo := 'A thoughtful reply 🌙';
    Text('short-note', LMemo);
    Check((TextValue('short-note') = LMemo) and
      (GRenderer.Root.Find('short-note').Prop('value') = LMemo), 'Short supplementary Unicode note is admitted exactly');
    Text('short-note', StringOfChar('x', 41));
    Check((TextValue('short-note') = LMemo) and
      (GRenderer.Root.Find('short-note').Prop('value') = LMemo), 'A second independently authored callback protects its text limit');
    LStream := nil;
    Release;
    WriteLn('PASS ', GChecks, ' compiled MCP-authored handler control checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-handler-consumers', 'passed');
    document.body.setAttribute('data-nyx-handler-consumer-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      LStream := nil;
      Release;
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-handler-consumers', 'failed');
      document.body.setAttribute('data-nyx-handler-consumer-error', LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
