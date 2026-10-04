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
program nyx_mcp_callback_consumer_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.callbacks, nyx.events,
  nyx.scheduler, nyx.composition, nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl;{$endif}

type
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer;
  {$else}TRenderer = TNyxLCLRenderer; TButtonAccess = class(TCustomButton);{$endif}

var
  GDocument: TNyxDocument;
  GRenderer: TRenderer;
  GStream: INyxEventStream;
  GChecks: Integer;
  GPolls: Integer;
  GExpected: Integer;
  {$ifdef PAS2JS}GHost: TJSHTMLElement;
  {$else}GHost: TForm;{$endif}

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
  GStream := nil;
  {$ifdef PAS2JS}

  if GHost <> nil then
  begin
    GHost.remove;
    GHost := nil;
  end;
  {$else}FreeAndNil(GHost);{$endif}
  FreeAndNil(GDocument);
end;

function Complete: Boolean;
var
  LIndex: Integer;
begin
  Result := True;
  for LIndex := 0 to GStream.Count - 1 do
  begin

    if (GStream.Registrations[LIndex].LastExecution = nil) or
      (GStream.Registrations[LIndex].LastExecution.Status in [nesPending, nesRunning]) then
    begin
      Result := False;
    end
    else
    begin
      Check(GStream.Registrations[LIndex].LastExecution.Status = nesSucceeded,
        'Actual compiled TODO callback succeeds after the control click');
    end;
  end;
end;

{$ifdef PAS2JS}
procedure Poll;
begin
  try
    Inc(GPolls);
    Check(GPolls < 100, 'Queued compiled callback completes within bounded polls');

    if not Complete then
    begin
      window.setTimeout(@Poll, 20);
      Exit;
    end;
    Release;
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' compiled MCP callback checks';
    document.body.setAttribute('data-nyx-compiled-callbacks', 'passed');
  except
    on LException: Exception do
    begin
      Release;
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-compiled-callbacks', 'failed');
    end;
  end;
end;
{$endif}

function RuntimeID(const AAuthored: TNyxText): TNyxText;
var
  LRoot: TNyxNode;
  LProjection: TNyxNode;
begin
  LRoot := RealizeNyxContext(GDocument, GDocument.Find(AAuthored), LProjection);
  try
    Check(LProjection <> nil, 'Compiled callback owner has a realized runtime identity');
    Result := LProjection.ID;
  finally
    LRoot.Free;
  end;
end;

procedure Prepare;
var
  LEvents: TNyxAuthoredEventInfos;
  {$ifdef PAS2JS}LButton: TJSHTMLElement;{$endif}
begin
  { This exact unit is exported through bounded real MCP source windows. No
    fixture rewrites its source or substitutes callback implementations. The
    startup class registry must resolve the generated TODO classes themselves. }
  GDocument := BuildNyxDocument;
  GRenderer := TRenderer.Create;
  BindNyxCallbacks(GDocument, GRenderer.Events);
  GStream := GRenderer.Events.On(NyxControlEvents(
    RuntimeID('callback-apply'), niRuntime), ntClick);
  Check((GStream.Count = GExpected) and (GStream.ExecutionPolicy = neUIQueue),
    'Compiled registration count and typed execution policy match the MCP pair');
  LEvents := NyxAuthoredEvents(GDocument.Find('callback-apply'));
  Check(Length(LEvents[0].Callbacks) = GExpected, 'Compiled authoring preserves ordered descriptor count');
  Check(GRenderer.Events.On(NyxControlEvents(
    RuntimeID('callback-notes'), niRuntime), ntAfterKeyPress).Count = 1,
    'Compiled input phase registration resolves its generated class');
  Check(GRenderer.Events.OnNamed(NyxCompoundEvents(
    RuntimeID('callback-search'), niRuntime), NyxSemantic(nseSearch)).Count = 1,
    'Compiled semantic registration resolves its generated class');
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GRenderer.Render(GDocument, GDocument.Find('callback-review'), GHost);
  LButton := TJSHTMLElement(GHost.querySelector('[data-node="callback-apply"]'));

  if not (LButton is TJSHTMLButtonElement) then
  begin
    LButton := TJSHTMLElement(LButton.querySelector('button'));
  end;
  TJSHTMLButtonElement(LButton).click;
  {$else}
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(0, 0, 640, 480);
  GRenderer.Render(GDocument, GDocument.Find('callback-review'), GHost);
  GHost.Show;
  Application.ProcessMessages;
  TButtonAccess(GRenderer.ControlFor('callback-apply')).Click;
  {$endif}
end;

begin
  try
    {$ifdef PAS2JS}
    GExpected := 1;

    if Pos('ordered', window.location.search) > 0 then
    begin
      GExpected := 2;
    end;
    {$else}
    Application.Initialize;
    GExpected := StrToInt(ParamStr(1));
    {$endif}
    Prepare;
    {$ifdef PAS2JS}window.setTimeout(@Poll, 20);
    {$else}
    repeat
      Application.ProcessMessages;
      CheckSynchronize(10);
      Inc(GPolls);
      Check(GPolls < 100, 'Native queued callback completes within bounded polls');
    until Complete;
    Release;
    WriteLn('PASS ', GChecks, ' compiled MCP callback checks');
    {$endif}
  except
    on LException: Exception do
    begin
      Release;
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-compiled-callbacks', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
