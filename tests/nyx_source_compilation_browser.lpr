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

program nyx_source_compilation_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, Web, nyx.text, nyx.data, nyx.studio.browser,
  nyx.studio.sourcejobs, nyx.test.source.compilation.browser
  {$ifdef NYX_SOURCE_SERVICE}, nyx.studio.sourcecompilation,
  nyx.studio.sourcecompilation.browser, nyx.studio.sourcecompilation.service.browser,
  nyx.studio.session, nyx.studio.projects, nyx.studio.workspaces,
  nyx.test.source.compilation{$endif}
  {$ifdef NYX_SOURCE_SHARED}, nyx.studio.sourcecompilation.shared.browser{$endif};

var
  GRequest: TJSXMLHttpRequest;
  GStudio: TNyxStudio;
  GInput: TJSHTMLTextAreaElement;
  GManifest: TNyxDataValue;
  GSource: TNyxText;
  GFailureSource: TNyxText;
  GBefore: TNyxText;
  GLoad: Integer;
  GPhase: Integer;
  GChecks: Integer;
  {$ifdef NYX_SOURCE_SERVICE}
  GCompiler: INyxSourceCompiler;
  GJourney: INyxSourceCompilationJourney;
  GExpected: TNyxText;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Browser compiler source controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Failed(const AReason: TNyxText);
begin
  GStudio.Free;
  GStudio := nil;
  document.body.setAttribute('data-source-controls', 'failed');
  document.body.setAttribute('data-event-error', AReason);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing ordinary Studio control: ' + AID);
  end;
end;

procedure Click(const AID: TNyxText);
begin
  Find(AID).click;
end;

procedure Pump;
begin
  try

    if not GStudio.SourceBusy and not GStudio.PresentationPending
      {$ifdef NYX_SOURCE_SHARED}and GStudio.SharedSourceSynchronized{$endif} then
    begin
      case GPhase of
        0:
          begin
            Click('action-code');
            GPhase := 1;
          end;
        1:
          begin
            GInput := TJSHTMLTextAreaElement(Find('studio-code'));
            GBefore := GInput.value;
            GInput.value := GSource;
            GInput.dispatchEvent(TJSEvent.new('change'));
            Click('action-apply-source');
            Check(GStudio.SourceBusy, 'physical Apply dispatches a live constructor worker');
            GPhase := 2;
          end;
        2:
          begin
            Check(GStudio.SourceCommands.State = nssApplied, 'compiled constructor publishes through real Apply');
            Check(GInput.value = GSource, 'exact complete helper/loop Pascal remains visible');
            Check(Find('heading-1').textContent = 'Notes for page 1',
              'actually executed constructor reaches the mounted design');
            Check(Find('studio-code') = GInput, 'publication retains the physical source editor');
            Click('action-undo');
            GPhase := 3;
          end;
        3:
          begin
            Check(GInput.value = GBefore, 'real Undo restores the initial accepted source');
            Check(document.querySelector('[data-node="heading-1"]') = nil,
              'real Undo restores the initial design');
            Click('action-redo');
            GPhase := 4;
          end;
        4:
          begin
            Check(GInput.value = GSource, 'real Redo restores the exact full source');
            Check(Find('heading-1').textContent = 'Notes for page 1',
              'real Redo restores the compiled design');
            Check(Find('studio-code') = GInput, 'history retains the same real source control');
            {$if defined(NYX_SOURCE_SERVICE) and not defined(NYX_SOURCE_SHARED)}
            { Every exact source is freshly compiled by the backend. This
              independent journey qualifies stale/failure/cancel/detach without
              publishing an executed source pair into the shared server project. }
            GJourney := StartNyxSourceCompilationJourney(GCompiler, GSource,
              GFailureSource, GExpected);
            GPhase := 5;
            {$else}
            document.body.setAttribute('data-source-controls-checks', IntToStr(GChecks));
            document.body.setAttribute('data-source-controls', 'passed');
            Exit;
            {$endif}
          end;
        {$ifdef NYX_SOURCE_SERVICE}
        5:
          begin
            GJourney.Pump;

            if GJourney.Done then
            begin
              document.body.setAttribute('data-compiled-service-checks', IntToStr(GJourney.Checks));
              document.body.setAttribute('data-source-controls-checks', IntToStr(GChecks));
              document.body.setAttribute('data-source-controls', 'passed');
              Exit;
            end;
          end;
        {$endif}
      end;
    end;
    window.setTimeout(@Pump, 20);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure LoadNext; forward;

{$ifdef NYX_SOURCE_SERVICE}
function Connected(AEvent: TJSProgressEvent): Boolean;
var
  LClaim: TNyxDataValue;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'owned editor connects with same-origin authority');
    LClaim := TNyxDataValue.ParseJSON(GRequest.responseText);
    Check(LClaim.Field('state').Field('sourceCompilation').AsBoolean,
      'actual server advertises its private source compiler capability');
    {$ifdef NYX_SOURCE_SHARED}
    GStudio := TNyxStudio.Create(nil, NewNyxSharedBrowserSourceCompilerFactory);
    {$else}
    GCompiler := NewNyxBrowserSourceCompiler(NewNyxBrowserSourceService(
      LClaim.Field('token').AsText, NyxPrimaryWorkspace,
      LClaim.Field('state').Field('session').Field('revision').AsInteger));
    GStudio := TNyxStudio.Create(GCompiler);
    {$endif}
    GStudio.Run(False);
    document.body.setAttribute('data-source-controls', 'pending');
    window.setTimeout(@Pump, 20);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;
{$endif}

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'owned current compiler input loaded over HTTP');
    case GLoad of
      0:
        begin
          GManifest := TNyxDataValue.ParseJSON(GRequest.responseText);
        end;
      1:
        begin
          GSource := GRequest.responseText;
        end;
      2:
        begin
          GFailureSource := GRequest.responseText;
        end;
      {$ifdef NYX_SOURCE_SERVICE}
      3:
        begin
          GExpected := GRequest.responseText;
        end;
      {$endif}
    end;
    Inc(GLoad);
    LoadNext;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function LoadError(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  Failed('Owned compiler control input did not arrive');
end;

procedure LoadNext;
const
  {$ifdef NYX_SOURCE_SERVICE}
  CFiles: array[0..3] of String = ('projection.json', 'source.pas', 'throw.pas', 'expected.nyx');
  {$else}
  CFiles: array[0..2] of String = ('projection.json', 'source.pas', 'throw.pas');
  {$endif}
  {$ifdef NYX_SOURCE_SERVICE}
var
  LSeed: TNyxStudioSession;
  LClaim: TNyxDataValue;
  {$endif}
begin

  if GLoad = Length(CFiles) then
  begin
    {$ifdef NYX_SOURCE_SERVICE}
    { This unit differs from all staged receipts. Backend compilation must happen
      now; the test cannot replay the previously generated worker artifact. }
    GSource := GSource + #10 + '{ Fresh authenticated HTTP source compilation. }' + #10;
    LSeed := TNyxStudioSession.Create;
    try
      LClaim := NyxObject([NyxField('op', NyxData('claim')),
        NyxField('project', NyxData(EncodeNyxProject(LSeed.ProjectSnapshot))),
        NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]);
    finally
      LSeed.Free;
    end;
    GRequest := TJSXMLHttpRequest.new;
    GRequest.open('POST', 'api/agents/connect', True);
    GRequest.timeout := 10000;
    GRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    GRequest.onload := @Connected;
    GRequest.send(LClaim.ToJSON);
    {$else}
    GStudio := TNyxStudio.Create(NyxFixtureBrowserCompiler(GSource, GFailureSource, GManifest));
    GStudio.Run(False);
    document.body.setAttribute('data-source-controls', 'pending');
    window.setTimeout(@Pump, 20);
    {$endif}
    Exit;
  end;
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', CFiles[GLoad], True);
  GRequest.timeout := 10000;
  GRequest.onload := @Loaded;
  GRequest.onerror := @LoadError;
  GRequest.ontimeout := @LoadError;
  GRequest.send;
end;

begin
  document.body.setAttribute('data-source-controls', 'pending');
  LoadNext;
end.
