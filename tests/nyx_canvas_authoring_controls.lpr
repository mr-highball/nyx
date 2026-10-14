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
program nyx_canvas_authoring_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.state, nyx.codec, nyx.behavior,
  nyx.events, nyx.scheduler, nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.workspace,
  nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.canvas.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, nyx.widgets.lcl,
  nyx.render.lcl, nyx.canvas.lcl{$endif};

type
  { The same application handler receives actual embedded-control events.
    It owns no renderer, document or scene reference. }
  TApplicationObserver = class
  public
    Clicks: Integer;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    {$ifndef PAS2JS}
    { LCL must not turn an unhandled host callback error into a modal dialog
      that lets this qualification continue as though the callback succeeded. }
    procedure HostException(ASender: TObject; AException: Exception);
    {$endif}
  end;
  { A parent registration may retire the entire application view while a child
    dispatch is active. It retains no renderer and exercises the ordinary guard. }
  TCloseParentCallback = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  GDocument: TNyxDocument;
  GState: TNyxState;
  GObserver: TApplicationObserver;
  GBefore: TNyxText;
  GItem: TNyxCanvasItemRef;
  GSubscription: INyxEventSubscription;
  GHostError: TNyxText;
  GStage: TNyxText;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GRenderer: TNyxBrowserRenderer;
  GView: INyxBrowserCanvasWorkspaceView;
  GInput: TJSHTMLTextAreaElement;
  {$else}
  GHost: TForm;
  GRenderer: TNyxLCLRenderer;
  GView: INyxLCLCanvasWorkspaceView;
  GInput: TMemo;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Ordinary canvas application: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TApplicationObserver.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.Trigger = ntClick then
  begin
    Inc(Clicks);
  end;
end;

{$ifndef PAS2JS}
procedure TApplicationObserver.HostException(ASender: TObject; AException: Exception);
begin
  GHostError := GStage + ': ' + TNyxText(AException.Message);
  WriteLn('HOST ERROR ', GHostError);
  DumpExceptionBackTrace(Output);
  Flush(Output);
end;
{$endif}

procedure TCloseParentCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GRenderer.Free;
  GRenderer := nil;
end;

{$ifdef PAS2JS}
function RefusingFactory(ANode: TNyxNode): TJSHTMLElement;
{$else}
function RefusingFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
{$endif}
begin
  Result := nil;
  raise Exception.Create('Owned canvas item factory deliberately refuses');
end;

procedure Run;
var
  LCamera: TNyxCanvasCamera;
  LBeforeInput: {$ifdef PAS2JS}TJSHTMLTextAreaElement{$else}TMemo{$endif};
  LRefused: Boolean;
begin
  GDocument := BuildNyxDocument;
  GBefore := TNyxCodec.Encode(GDocument);
  GState := GDocument.State.Clone;
  GObserver := TApplicationObserver.Create;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('width', '100%');
  GHost.style.setProperty('height', '650px');
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.OnEvent := @GObserver.Event;
  {$else}
  Application.Initialize;
  Application.OnException := GObserver.HostException;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(40, 40, 1000, 750);
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  GRenderer.OnEvent := GObserver.Event;
  {$endif}
  { This is ordinary page rendering. There is no manual scene materialization
    or explicit target canvas mount in the application. }
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False, GState);
  {$ifndef PAS2JS}Application.ProcessMessages;{$endif}
  {$ifdef PAS2JS}
  GView := GRenderer.CanvasView('board') as INyxBrowserCanvasWorkspaceView;
  {$else}
  GView := GRenderer.CanvasView('board') as INyxLCLCanvasWorkspaceView;
  {$endif}
  Check(GView.Connected and (GView.MountedCount = 1),
    'ordinary page mounts its declared live scene');
  GItem := NyxCanvasItem('Reply 🐾');
  {$ifdef PAS2JS}
  GInput := TJSHTMLTextAreaElement(GView.InputFor(GItem, NyxControl('reply-memo')));
  Check(GInput.value = 'Start here', 'ordinary browser scene reads application defaults');
  GInput.value := 'Ordinary canvas stays live';
  GInput.dispatchEvent(TJSEvent.new('input'));
  Check(GView.RendererFor(GItem).ElementFor('reply-status').textContent =
    'Reply: Ordinary canvas stays live', 'ordinary browser scene updates its computed caption');
  TJSHTMLButtonElement(GView.RendererFor(GItem).ElementFor('send-button')).click;
  {$else}
  GInput := TMemo(GView.InputFor(GItem, NyxControl('reply-memo')));
  Check(TNyxText(GInput.Text) = 'Start here', 'ordinary native scene reads application defaults');
  GInput.Text := 'Ordinary canvas stays live';
  Application.ProcessMessages;
  Check(TNyxText(TLabel(GView.RendererFor(GItem).ControlFor('reply-status')).Caption) =
    'Reply: Ordinary canvas stays live', 'ordinary native scene updates its computed caption');
  TNyxLCLButton(GView.RendererFor(GItem).ControlFor('send-button')).Click;
  {$endif}
  Check(GState.GetValue(NyxTextState('reply')) = 'Ordinary canvas stays live',
    'ordinary application store receives actual scene input');
  Check(GObserver.Clicks = 1, 'ordinary application handler receives an embedded control click');
  GRenderer.Root.Find('board').Configure.ReadOnly(True).Done;
  GRenderer.Sync;
  {$ifdef PAS2JS}
  Check(GInput.readOnly and not GInput.disabled,
    'canvas read-only gate retains a selectable enabled browser editor');
  {$else}
  Check(GInput.ReadOnly and GInput.Enabled,
    'canvas read-only gate retains a selectable enabled native editor');
  {$endif}
  GRenderer.Root.Find('board').Configure.ReadOnly(False).Enabled(False).Done;
  GRenderer.Sync;
  {$ifdef PAS2JS}
  Check(GInput.disabled, 'canvas disabled gate reaches the browser child');
  {$else}
  Check(not GInput.Enabled, 'canvas disabled gate reaches the native child');
  {$endif}
  GRenderer.Root.Find('board').Configure.Enabled(True).Done;
  GRenderer.Sync;
  {$ifdef PAS2JS}
  Check(not GInput.disabled and not GInput.readOnly,
    'browser gate removal restores authored interaction without replacing the input');
  {$else}
  Check(GInput.Enabled and not GInput.ReadOnly,
    'native gate removal restores authored interaction without replacing the input');
  {$endif}
  LCamera := GView.Camera;
  GView.PanBy(30, 12);
  GView.ZoomAt(1.5, NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2));
  Check(GView.InputFor(GItem, NyxControl('reply-memo')) = GInput,
    'ordinary scene camera retains the exact physical editor');
  Check(TNyxCodec.Encode(GDocument) = GBefore,
    'ordinary rendering and input preserve the exact authored document');
  GView.Execute(nccFit);
  { Disconnect follows the ordinary renderer's lifetime, even when an
    application retains the public view interface after navigation. }
  GRenderer.Free;
  GRenderer := nil;
  Check(not GView.Connected, 'ordinary renderer retirement disconnects retained canvas interfaces');
  GView := nil;
  {$ifdef PAS2JS}
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.OnEvent := @GObserver.Event;
  {$else}
  GRenderer := TNyxLCLRenderer.Create;
  GRenderer.OnEvent := GObserver.Event;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False, GState);
  {$ifndef PAS2JS}Application.ProcessMessages;{$endif}
  {$ifdef PAS2JS}
  GView := GRenderer.CanvasView('board') as INyxBrowserCanvasWorkspaceView;
  GInput := TJSHTMLTextAreaElement(GView.InputFor(GItem, NyxControl('reply-memo')));
  Check(GInput.value = 'Ordinary canvas stays live', 'ordinary remount retains the caller application store');
  {$else}
  GView := GRenderer.CanvasView('board') as INyxLCLCanvasWorkspaceView;
  GInput := TMemo(GView.InputFor(GItem, NyxControl('reply-memo')));
  Check(TNyxText(GInput.Text) = 'Ordinary canvas stays live',
    'ordinary remount retains the caller application store');
  {$endif}
  GView.Execute(nccFit);
  { A late nested factory refusal must preserve the accepted application face.
    The override remains local to this renderer, retired by the callback below. }
  GStage := 'nested candidate refusal';
  LBeforeInput := GInput;
  GRenderer.RegisterFactory('button', @RefusingFactory);
  LRefused := False;
  try
    GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False, GState);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and GView.Connected and
    (GView.InputFor(GItem, NyxControl('reply-memo')) = LBeforeInput),
    'nested candidate failure preserves the accepted physical editor and scene');
  Check(GHostError = '', 'candidate refusal does not leak into an LCL host callback');
  GStage := 'parent callback retirement';
  GSubscription := GRenderer.Events.On(NyxControlEvents('send-button'), ntClick)
    .Subscribe(TCloseParentCallback.Create);
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GView.RendererFor(GItem).ElementFor('send-button')).click;
  {$else}
  TNyxLCLButton(GView.RendererFor(GItem).ControlFor('send-button')).Click;
  {$endif}
  Check((GRenderer = nil) and not GView.Connected,
    'parent event registration safely retires a nested ordinary dispatch');
  Check(GHostError = '', 'parent retirement does not leak into an LCL host callback');
  GSubscription := nil;
  GView := nil;
  {$ifdef PAS2JS}
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False, GState);
  {$ifndef PAS2JS}Application.ProcessMessages;{$endif}
  GRenderer.CanvasView('board').Execute(nccFit);
  {$ifdef PAS2JS}
  document.body.setAttribute('data-canvas-authoring', 'passed');
  document.body.setAttribute('data-canvas-checks', IntToStr(GChecks));
  {$else}
  WriteLn('PASS ', GChecks, ' ordinary canvas application checks');
  {$endif}
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-authoring', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  GRenderer.Free;
  GView := nil;
  GObserver.Free;
  GState.Free;
  GDocument.Free;
  GHost.Free;
  {$endif}
end.
