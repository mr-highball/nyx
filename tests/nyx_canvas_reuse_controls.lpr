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
program nyx_canvas_reuse_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.state, nyx.codec, nyx.behavior,
  nyx.events, nyx.scheduler, nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.workspace,
  nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.canvas.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, nyx.widgets.lcl,
  nyx.render.lcl, nyx.canvas.lcl{$endif};

type
  { Capture native host failures explicitly; this observer owns no renderer,
    document or scene reference. }
  TApplicationObserver = class
  public
    {$ifndef PAS2JS}
    { LCL must not turn an unhandled host callback error into a modal dialog
      that lets this qualification continue as though the callback succeeded. }
    procedure HostException(ASender: TObject; AException: Exception);
    {$endif}
  end;

var
  GChecks: Integer;
  GDocument: TNyxDocument;
  GState: TNyxState;
  GObserver: TApplicationObserver;
  GBefore: TNyxText;
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
    raise Exception.Create('Reusable canvas application: ' + AReason);
  end;
  Inc(GChecks);
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

procedure Run;
var
  LSecond: INyxCanvasWorkspaceView;
  LEmpty: INyxCanvasWorkspaceView;
  LNested: INyxCanvasWorkspaceView;
  LOther: {$ifdef PAS2JS}TJSHTMLTextAreaElement{$else}TMemo{$endif};
  LOtherCamera: Double;
begin
  GDocument := BuildNyxDocument;
  GBefore := TNyxCodec.Encode(GDocument);
  GState := GDocument.State.Clone;
  GObserver := TApplicationObserver.Create;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  Application.Initialize;
  Application.OnException := GObserver.HostException;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, 1000, 1000);
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False, GState);
  {$ifndef PAS2JS}Application.ProcessMessages;{$endif}
  {$ifdef PAS2JS}
  GView := GRenderer.CanvasView(NyxQualifiedID('inherited', 'canvas-template'),
    niRuntime) as INyxBrowserCanvasWorkspaceView;
  {$else}
  GView := GRenderer.CanvasView(NyxQualifiedID('inherited', 'canvas-template'),
    niRuntime) as INyxLCLCanvasWorkspaceView;
  {$endif}
  LSecond := GRenderer.CanvasView(NyxQualifiedID('alternate', 'canvas-template'), niRuntime);
  LEmpty := GRenderer.CanvasView(NyxQualifiedID('empty', 'canvas-template'), niRuntime);
  LNested := GRenderer.CanvasView(NyxQualifiedID('wrapped', 'wrapped-board'), niRuntime);
  Check(GView.Connected and LSecond.Connected and LEmpty.Connected and LNested.Connected,
    'four ordinary reusable instances mount independent views');
  GView.Execute(nccFit);
  LSecond.Execute(nccFit);
  LNested.Execute(nccFit);
  Check((GView.MountedCount = 1) and (LSecond.MountedCount = 1) and
    (LNested.MountedCount = 1) and (LEmpty.MountedCount = 0),
    'inherited/alternate/named-part scenes mount while empty masks content');
  {$ifdef PAS2JS}
  GInput := TJSHTMLTextAreaElement(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('first-content')));
  LOther := TJSHTMLTextAreaElement((LSecond as INyxBrowserCanvasWorkspaceView)
    .InputFor(NyxCanvasItem('Editor'), NyxControl('second-content')));
  Check((GInput.value = 'First scene') and (LOther.value = 'Second scene'),
    'exact scene content reaches the actual browser editors');
  GInput.value := 'Independent first edit';
  GInput.dispatchEvent(TJSEvent.new('input'));
  Check(LOther.value = 'Second scene', 'editing one instance preserves sibling text');
  Check(TJSHTMLTextAreaElement((LNested as INyxBrowserCanvasWorkspaceView)
    .InputFor(NyxCanvasItem('Editor'), NyxControl('second-content'))).value = 'Second scene',
    'named-part override reaches actual nested content');
  {$else}
  GInput := TMemo(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('first-content')));
  LOther := TMemo((LSecond as INyxLCLCanvasWorkspaceView)
    .InputFor(NyxCanvasItem('Editor'), NyxControl('second-content')));
  Check((TNyxText(GInput.Text) = 'First scene') and (TNyxText(LOther.Text) = 'Second scene'),
    'exact scene content reaches the actual native editors');
  GInput.Text := 'Independent first edit';
  Application.ProcessMessages;
  Check(TNyxText(LOther.Text) = 'Second scene', 'editing one instance preserves sibling text');
  Check(TNyxText(TMemo((LNested as INyxLCLCanvasWorkspaceView)
    .InputFor(NyxCanvasItem('Editor'), NyxControl('second-content'))).Text) = 'Second scene',
    'named-part override reaches actual nested content');
  {$endif}
  LOtherCamera := LSecond.Camera.WorldCenter.X;
  GView.PanBy(33, 12);
  Check(LSecond.Camera.WorldCenter.X = LOtherCamera, 'instance cameras remain independent');
  Check(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('first-content')) = GInput,
    'camera retains exact physical editor');
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'editing/navigation preserve authored definitions');
  Check(GHostError = '', 'actual input has no swallowed native host error');
  GRenderer.Free;
  GRenderer := nil;
  Check(not GView.Connected and not LSecond.Connected and not LEmpty.Connected and
    not LNested.Connected, 'parent retirement revokes all retained instance views');
  {$ifdef PAS2JS}
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False, GState);
  {$ifndef PAS2JS}Application.ProcessMessages;{$endif}
  GRenderer.CanvasView(NyxQualifiedID('inherited', 'canvas-template'), niRuntime).Execute(nccFit);
  GRenderer.CanvasView(NyxQualifiedID('alternate', 'canvas-template'), niRuntime).Execute(nccFit);
  GRenderer.CanvasView(NyxQualifiedID('wrapped', 'wrapped-board'), niRuntime).Execute(nccFit);
  {$ifdef PAS2JS}
  document.body.setAttribute('data-canvas-reuse-controls', 'passed');
  document.body.setAttribute('data-canvas-checks', IntToStr(GChecks));
  {$else}
  WriteLn('PASS ', GChecks, ' actual reusable canvas checks');
  {$endif}
end;

procedure ExecuteQualification;
begin
  try
    Run;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-reuse-controls', 'failed');
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
end;

begin
  {$ifdef PAS2JS}
  window.setTimeout(@ExecuteQualification, 0);
  {$else}
  ExecuteQualification;
  {$endif}
end.
