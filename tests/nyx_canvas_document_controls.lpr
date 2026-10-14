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
program nyx_canvas_document_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state, nyx.codec,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.workspace, nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.canvas.browser
  {$else}, Interfaces, Classes, Forms, Controls, Types, StdCtrls, nyx.canvas.lcl{$endif};

var
  GChecks: Integer;
  GDocument: TNyxDocument;
  GWorkspace: INyxCanvasWorkspace;
  GState: TNyxState;
  GBefore: TNyxText;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GView: INyxBrowserCanvasWorkspaceView;
  GInput: TJSHTMLTextAreaElement;
  GCompanion: TJSXMLHttpRequest;
  {$else}
  GHost: TForm;
  GView: INyxLCLCanvasWorkspaceView;
  GInput: TMemo;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled saved canvas: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifdef PAS2JS}
function CompanionLoaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;
  try
    Check(GCompanion.status = 200, 'exact HTTP companion is available');
    Check(GCompanion.responseText = GBefore,
      'actual browser compiler reconstruction equals the complete native companion');
    document.body.setAttribute('data-canvas-compiled', 'passed');
    document.body.setAttribute('data-canvas-checks', IntToStr(GChecks));
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-canvas-compiled', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
    end;
  end;
end;

function CompanionFailed(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;
  document.body.setAttribute('data-canvas-compiled', 'failed');
  document.body.setAttribute('data-canvas-error', 'Exact companion HTTP request failed');
end;
{$endif}

{$ifndef PAS2JS}
function ReadFixture(const APath: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: RawByteString;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check(LStream.Size <= 4 * 1024 * 1024, 'bounded UTF-8 companion input');
    SetLength(LBytes, LStream.Size);

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(LBytes[1], Length(LBytes));
    end;
    Result := TNyxText(LBytes);
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure Run;
var
  LItem: TNyxCanvasItemRef;
  LCamera: TNyxCanvasCamera;
  {$ifdef PAS2JS}
  LOriginal: TJSHTMLElement;
  LInputRect: TJSDOMRect;
  LSurfaceRect: TJSDOMRect;
  {$else}
  LOriginal: TControl;
  LInputOrigin: TPoint;
  LSurfaceOrigin: TPoint;
  {$endif}
begin
  GDocument := BuildNyxDocument;
  GBefore := TNyxCodec.Encode(GDocument);
  GState := GDocument.State.Clone;
  GWorkspace := NewNyxCanvasWorkspace('saved', GDocument, NyxCanvasRef('workshop 🐾'));
  LItem := NyxCanvasItem('Reply 🐾');
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('width', '100%');
  GHost.style.setProperty('height', '650px');
  document.body.appendChild(GHost);
  GView := MountNyxBrowserCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  GInput := TJSHTMLTextAreaElement(GView.InputFor(LItem, NyxControl('reply-memo')));
  Check(GInput.value = 'Start here', 'compiled browser binding reads saved defaults');
  GInput.value := 'Saved canvas remains live';
  GInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  Application.Initialize;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(40, 40, 1000, 750);
  GHost.Show;
  GView := MountNyxLCLCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  Application.ProcessMessages;
  GInput := TMemo(GView.InputFor(LItem, NyxControl('reply-memo')));
  Check(TNyxText(GInput.Text) = 'Start here', 'compiled native binding reads saved defaults');
  GInput.Text := 'Saved canvas remains live';
  Application.ProcessMessages;
  {$endif}
  Check(GState.GetValue(NyxTextState('reply')) = 'Saved canvas remains live',
    'actual compiled input writes through the ordinary binding contract');
  {$ifdef PAS2JS}
  Check(GView.RendererFor(LItem).ElementFor('reply-status').textContent =
    'Reply: Saved canvas remains live', 'saved expression updates an actual browser caption');
  {$else}
  Check(TNyxText(TLabel(GView.RendererFor(LItem).ControlFor('reply-status')).Caption) =
    'Reply: Saved canvas remains live', 'saved expression updates an actual native caption');
  {$endif}
  LOriginal := GView.InputFor(LItem, NyxControl('reply-memo'));
  LCamera := GView.Camera;
  GView.PanBy(20, 15);
  GView.ZoomAt(2, NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2));
  Check(GView.InputFor(LItem, NyxControl('reply-memo')) = LOriginal,
    'compiled live input survives camera pan and saved logical-size policy');
  Check((GWorkspace.Scene.Control(LItem).Node <> GDocument.Components[0]) and
    (GWorkspace.Scene.Control(LItem).Node.Parent <> nil), 'compiled runtime owns an independent cloned template');
  Check(GDocument.Canvases.Definition(NyxCanvasRef('workshop 🐾')).Camera.WorldCenter.X = -90.25,
    'runtime camera does not rewrite saved viewport settings');
  Check(TNyxCodec.Encode(GDocument) = GBefore,
    'compiled application preserves exact complete design and saved settings');
  { Assertions deliberately navigate away from the card. Return the review
    workshop to a useful framed view, without editing its authored camera. }
  GView.Execute(nccFit);
  Check(GView.InputFor(LItem, NyxControl('reply-memo')) = LOriginal,
    'compiled fit preserves the real edited control');
  {$ifdef PAS2JS}
  LInputRect := GInput.getBoundingClientRect;
  LSurfaceRect := GView.Surface.getBoundingClientRect;
  Check((LInputRect.left >= LSurfaceRect.left) and
    (LInputRect.right <= LSurfaceRect.right + 1),
    'actual fitted browser editor is fully visible under logical-size allocation');
  {$else}
  Application.ProcessMessages;
  LInputOrigin := GInput.ClientToScreen(Point(0, 0));
  LSurfaceOrigin := GView.Surface.ClientToScreen(Point(0, 0));
  Check((LInputOrigin.X >= LSurfaceOrigin.X) and
    (LInputOrigin.X + GInput.Width <= LSurfaceOrigin.X + GView.Surface.ClientWidth + 1),
    'actual fitted native editor is fully visible under logical-size allocation');
  {$endif}
  {$ifndef PAS2JS}

  if ParamCount = 1 then
  begin
    Check(GBefore = ReadFixture(ParamStr(1)),
      'compiler reconstruction equals exact companion design');
  end;
  {$endif}
  {$ifdef PAS2JS}
  document.body.setAttribute('data-canvas-compiled', 'running');
  GCompanion := TJSXMLHttpRequest.new;
  GCompanion.open('GET', 'design.nyx', True);
  GCompanion.onload := @CompanionLoaded;
  GCompanion.onerror := @CompanionFailed;
  GCompanion.send;
  {$else}
  WriteLn('PASS ', GChecks, ' compiled live canvas checks');
  {$endif}
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-compiled', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}

  if GView <> nil then
  begin
    GView.Disconnect;
  end;
  GView := nil;
  GWorkspace := nil;
  GHost.Free;
  GState.Free;
  GDocument.Free;
  {$endif}
end.
