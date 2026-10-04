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
program nyx_collection_view_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  nyx.test.collections.view
  {$ifdef PAS2JS}
  , Web, nyx.collections.browser, nyx.test.collections.controls
  {$endif};

procedure Run;
var
  LChecks: Integer;
  {$ifdef PAS2JS}
  LControlChecks: Integer;
  {$endif}
begin
  try
    LChecks := RunNyxCollectionViewTests;
    {$ifdef PAS2JS}
    LControlChecks := RunNyxCollectionControlJourney;
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' shared / ' +
      IntToStr(LControlChecks) + ' collection control checks';
    document.body.setAttribute('data-collection-controls', IntToStr(LControlChecks));
    document.body.setAttribute('data-collection-views', 'passed');
    {$else}
    WriteLn('PASS ', LChecks, ' collection view checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-collection-views', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
var
  GFrame: TJSHTMLIFrameElement;
  GPolls: Integer;

procedure PollFrame;
var
  LBody: TJSHTMLElement;
  LStatus: String;
begin
  Inc(GPolls);
  { The child document can briefly have no body while its scripts are loading.
    Keep polling that transition instead of throwing and losing the result. }

  if (GFrame.contentDocument = nil) or (GFrame.contentDocument.body = nil) then
  begin

    if GPolls >= 400 then
    begin
      document.body.setAttribute('data-collection-views-host', 'failed');
      document.body.textContent := 'FAIL collection view iframe did not initialize';
    end
    else
    begin
      window.setTimeout(@PollFrame, 25);
    end;
    Exit;
  end;
  LBody := TJSHTMLElement(GFrame.contentDocument.body);
  LStatus := LBody.getAttribute('data-collection-views');

  if LStatus = 'passed' then
  begin
    document.body.setAttribute('data-collection-views-host', 'passed');
    document.body.setAttribute('data-collection-views-width',
      IntToStr(GFrame.contentWindow.innerWidth));
    document.body.textContent := LBody.textContent;
  end
  else if (LStatus = 'failed') or (GPolls >= 400) then
  begin
    document.body.setAttribute('data-collection-views-host', 'failed');
    document.body.textContent := LBody.textContent;
  end
  else
  begin
    window.setTimeout(@PollFrame, 25);
  end;
end;

procedure Host;
begin
  GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
  GFrame.style.setProperty('width', '390px');
  GFrame.style.setProperty('height', '844px');
  GFrame.style.setProperty('border', '0');
  GFrame.src := 'collection-views.html?frame=1';
  document.body.appendChild(GFrame);
  window.setTimeout(@PollFrame, 25);
end;
{$endif}

begin
  {$ifdef PAS2JS}

  if window.location.search = '?host=1' then
  begin
    Host;
  end
  else
  begin
    Run;
  end;
  {$else}
  Run;
  {$endif}
end.
