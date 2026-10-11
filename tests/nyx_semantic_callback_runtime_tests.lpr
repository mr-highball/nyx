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
program nyx_semantic_callback_runtime_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.model, nyx.codec, nyx.callbacks,
  nyx.events, nyx.scheduler, nyx.composition, nyx.projection.fixture,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser, nyx.application.browser;
  {$else}Classes, Interfaces, Forms, Controls, nyx.render.lcl, nyx.application.lcl;{$endif}

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TApplicationHost = TNyxBrowserApplication;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TApplicationHost = TNyxLCLApplication;
  TControlAccess = class(TControl);
  {$endif}

const
  CFailure: TNyxText = 'An authored failure 🚀 𐐷.';

var
  GDocument: TNyxDocument;
  GApplication: TApplicationHost;
  { Borrowed from the normal application host, never freed separately. }
  GRenderer: TRenderer;
  GStream: INyxEventStream;
  GRetired: TNyxExecutions;
  GDesign: TNyxText;
  GQueued: Boolean;
  GChecks: Integer;
  GPhase: Integer;
  GDeadline: Double;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GRequest: TJSXMLHttpRequest;
  {$else}
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

function NowMilliseconds: Double;
begin
  {$ifdef PAS2JS}
  Result := TJSDate.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

procedure Release;
begin
  { Executions retain diagnostics independently of their retired controls.
    Neither this harness nor generated callbacks retain a renderer/tree cycle. }
  GRenderer := nil;
  FreeAndNil(GApplication);
  GStream := nil;
  {$ifdef PAS2JS}

  if GHost <> nil then
  begin
    GHost.remove;
    GHost := nil;
  end;
  {$else}
  GHost := nil;
  {$endif}
  FreeAndNil(GDocument);
end;

procedure Render;
begin
  { Exercise ordinary application navigation, including the temporary absence
    of the callback's control. The host owns the renderer, automatic callback
    binding and independent runtime stores throughout both page changes. }
  GApplication.ShowPage(GDocument.Pages[1].ID);
  GApplication.ShowPage(GDocument.Pages[0].ID);
  Check(GApplication.State <> GDocument.State,
    'Ordinary page navigation retains an independent application state store');
end;

procedure Click;
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(GHost.querySelector('[data-node="heading-1"]')).click;
  {$else}
  TControlAccess(GRenderer.ControlFor('heading-1')).Click;
  {$endif}
end;

procedure SnapshotExecutions;
var
  LIndex: Integer;
begin
  SetLength(GRetired, GStream.Count);
  for LIndex := 0 to GStream.Count - 1 do
  begin
    GRetired[LIndex] := GStream.Registrations[LIndex].LastExecution;
    Check(GRetired[LIndex] <> nil, 'Actual control click creates each execution');
  end;
end;

function Complete: Boolean;
var
  LIndex: Integer;
  LExecution: INyxExecution;
begin
  Result := True;
  for LIndex := 0 to GStream.Count - 1 do
  begin
    LExecution := GStream.Registrations[LIndex].LastExecution;

    if (LExecution = nil) or (LExecution.Status in [nesPending, nesRunning]) then
    begin
      Result := False;
    end;
  end;
end;

procedure VerifyAuthoredExecutions;
var
  LCancelled: Integer;
  LFailed: Integer;
begin
  LCancelled := 0;
  LFailed := 1;

  if not GQueued then
  begin
    LCancelled := 1;
    LFailed := 0;
  end;
  Check(GStream.Registrations[LCancelled].LastExecution.Status = nesCancelled,
    'Exact MCP-authored cancellation body executes in its accepted registration');
  Check(GStream.Registrations[LFailed].LastExecution.Status = nesFailed,
    'Exact MCP-authored exception body executes in its accepted registration');
  Check(GStream.Registrations[LFailed].LastExecution.Failure = CFailure,
    'Application execution preserves the exact supplementary Unicode failure');
  Check(GStream.Registrations[LCancelled].LastExecution.Failure = '',
    'A sibling failure does not contaminate cancelled execution diagnostics');
  Check(TNyxCodec.Encode(GDocument) = GDesign,
    'Physical dispatch preserves the entire accepted constructed descriptor');
end;

procedure VerifyRetiredExecutions;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(GRetired) do
  begin

    if not GQueued and (LIndex = 0) then
    begin
      Check(GRetired[LIndex].Status = nesFailed,
        'Completed sequential failure survives view and renderer retirement');
      Check(GRetired[LIndex].Failure = CFailure,
        'Retained completed failure keeps its exact Unicode diagnostic');
    end
    else
    begin
      Check(GRetired[LIndex].Status = nesCancelled,
        'Retired cancelled execution keeps its exact terminal status');
      Check(GRetired[LIndex].Failure = '',
        'Retired cancellation has no exception-body diagnostic');
    end;
  end;
end;

function Advance: Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if NowMilliseconds >= GDeadline then
  begin
    raise Exception.Create('Actual application callbacks did not finish before deadline');
  end;

  if not Complete then
  begin
    Exit;
  end;
  VerifyAuthoredExecutions;

  if GPhase = 0 then
  begin
    { Re-render before servicing the deferred queue. These executions belong to
      the old mounted view and must never enter their handwritten bodies. The
      sequential variant has already executed; its retained diagnostics stay
      terminal instead of being rewritten by navigation. }
    Click;
    SnapshotExecutions;
    for LIndex := 0 to High(GRetired) do
    begin

      if GQueued then
      begin
        Check(GRetired[LIndex].Status = nesPending,
          'UI-queue callback has not entered its body synchronously');
      end;
    end;
    Render;
    VerifyRetiredExecutions;
    GPhase := 1;
    Click;
    Exit;
  end;
  for LIndex := 0 to High(GRetired) do
  begin
    Check(GStream.Registrations[LIndex].LastExecution <> GRetired[LIndex],
      'Fresh mounted input receives independent execution identities');

    if GQueued then
    begin
      Check(GRetired[LIndex].Status = nesCancelled,
        'Later mounted work cannot revive cancelled prior-view executions');
    end;
  end;
  GRenderer := nil;
  FreeAndNil(GApplication);
  VerifyRetiredExecutions;
  Result := True;
end;

procedure Prepare;
var
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LRuntimeID: TNyxText;
  LInfos: TNyxAuthoredEventInfos;
begin
  { This constructor is the complete accepted source exported by the production
    semantic/body/compiler/native-observer journey. No generated TODO substitute,
    source normalization or alternate callback factory participates here. }
  GDocument := BuildNyxDocument;
  Check(TNyxCodec.Encode(GDocument) = GDesign,
    'Exact accepted MCP source reconstructs the complete accepted design');
  LRoot := RealizeNyxContext(GDocument, GDocument.Find('heading-1'), LProjection);
  try
    Check(LProjection <> nil, 'Authored heading has a realized runtime target');
    LRuntimeID := LProjection.ID;
  finally
    LRoot.Free;
  end;
  GApplication := TApplicationHost.Create;
  GRenderer := GApplication.View;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GApplication.Run(GDocument, GHost);
  {$else}
  GApplication.Mount(GDocument);
  GHost := GApplication.Window;
  GHost.Show;
  Application.ProcessMessages;
  {$endif}
  Check(GApplication.State <> GDocument.State,
    'The normal application host owns runtime state independently of defaults');
  GStream := GRenderer.Events.On(NyxControlEvents(LRuntimeID, niRuntime), ntClick);
  LInfos := NyxAuthoredEvents(GDocument.Find('heading-1'));
  Check((GStream.Count = 2) and (Length(LInfos) = 1) and
    (Length(LInfos[0].Callbacks) = 2), 'Both accepted callbacks resolve their actual classes');

  if GQueued then
  begin
    Check(GStream.ExecutionPolicy = neUIQueue, 'Accepted UI-queue policy is compiled');
  end
  else
  begin
    Check(GStream.ExecutionPolicy = neSequential, 'Accepted sequential policy is compiled');
  end;
  GDeadline := NowMilliseconds + 10000;
  Click;
end;

{$ifdef PAS2JS}
procedure Failed(const AMessage: TNyxText);
begin
  Release;
  GRetired := nil;
  document.body.setAttribute('data-nyx-semantic-runtime', 'failed');
  document.body.setAttribute('data-nyx-semantic-runtime-error', AMessage);
end;

procedure Poll;
begin
  try

    if not Advance then
    begin
      window.setTimeout(@Poll, 10);
      Exit;
    end;
    Release;
    GRetired := nil;
    document.body.textContent := 'PASS ' + IntToStr(GChecks) +
      ' actual MCP-authored application callback checks';
    document.body.setAttribute('data-nyx-semantic-runtime', 'passed');
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'Accepted whole-design oracle loads over HTTP');
    GDesign := GRequest.responseText;
    GRequest := nil;
    Prepare;
    window.setTimeout(@Poll, 10);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function LoadFailed(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  Failed('Accepted design oracle could not load');
end;
{$else}
function ReadDesign(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;
{$endif}

begin
  try
    {$ifdef PAS2JS}
    GQueued := Pos('queued', window.location.search) > 0;
    GRequest := TJSXMLHttpRequest.new;
    GRequest.onload := @Loaded;
    GRequest.onerror := @LoadFailed;
    GRequest.open('GET', 'design.json', True);
    GRequest.send;
    {$else}
    Application.Initialize;
    Check((ParamCount = 2) and (ParamStr(1) <> ''),
      'Supply queued/sequential policy and the exact producer design oracle');
    Check((ParamStr(1) = 'queued') or (ParamStr(1) = 'sequential'),
      'Runtime qualification selects one explicit accepted application');
    GQueued := ParamStr(1) = 'queued';
    GDesign := ReadDesign(ParamStr(2));
    Prepare;
    repeat
      Application.ProcessMessages;
      CheckSynchronize(1);
    until Advance;
    Release;
    GRetired := nil;
    WriteLn('PASS ', GChecks, ' actual MCP-authored application callback checks');
    {$endif}
  except
    on LException: Exception do
    begin
      Release;
      GRetired := nil;
      {$ifdef PAS2JS}
      Failed(LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
