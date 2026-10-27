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
program nyx_canvas_uniform_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.canvas.geometry, nyx.canvas.types,
  nyx.canvas.scene, nyx.canvas.workspace
  {$ifdef PAS2JS}, JS, Web, nyx.canvas.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Types, nyx.canvas.lcl{$endif};

type
  { The router retains this callback, which uses only harness-owned state.
    It retains no view, renderer, scene or UI object through an event cycle. }
  TActionCallback = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

const
  CNote: TNyxText = 'Notes 🌙 stay yours across every zoom.';
  CZooms: array[0..4] of Double = (1, 0.5, 1.5, 0.5, 1);

var
  GDocument: TNyxDocument;
  GState: TNyxState;
  GWorkspace: INyxCanvasWorkspace;
  GScene: INyxCanvasScene;
  GSubscription: INyxEventSubscription;
  GItem: TNyxCanvasItemRef;
  GChecks: Integer;
  GStage: Integer;
  GDone: Boolean;
  GActionCount: Integer;
  GLegacy: Boolean;
  GBaseWidth: Double;
  GBaseHeight: Double;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GView: INyxBrowserCanvasWorkspaceView;
  GInput: TJSHTMLTextAreaElement;
  {$else}
  GHost: TForm;
  GView: INyxLCLCanvasWorkspaceView;
  GInput: TMemo;
  {$endif}

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure TActionCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GActionCount);
  GState.SetValue(NyxTextState('result'), 'Action received ' + TNyxText(IntToStr(GActionCount)));
end;

procedure Build;
var
  LCard: INyxCard;
  LHeading: INyxHeading;
  LHelp: INyxLabel;
  LMemo: INyxMemo;
  LResult: INyxLabel;
  LLayer: TNyxCanvasLayerRef;
begin
  GDocument := TNyxDocument.Create;
  GDocument.AddPage(NewNyxPage('home'));
  GDocument.State.SetValue(NyxTextState('note'), CNote);
  GDocument.State.SetValue(NyxTextState('result'), 'Result stays inside the fitted card.');
  GState := GDocument.State.Clone;
  LCard := NewNyxCard('notebook');
  LCard.Configure.Padding(20).Gap(12).Done;
  LHeading := NewNyxHeading('notebook-title');
  LHeading.Text := 'Your workspace';
  LCard.Add(LHeading);
  LHelp := NewNyxLabel('notebook-help');
  LHelp.Text := 'Write a note, keep a result, and try the action. Zooming should retain '
    + 'the same layout, live controls and exact text. The result at the bottom must '
    + 'remain reachable after fitting this tall card into a smaller workspace.';
  LCard.Add(LHelp);
  LMemo := NewNyxMemo('note');
  LMemo.Text := 'Your note';
  LMemo.Configure.Height(170).Done;
  LMemo.Binds.Value(NyxTextState('note'));
  LCard.Add(LMemo);
  LMemo := NewNyxMemo('prepared-note');
  LMemo.Text := 'Prepared text';
  LMemo.Configure.Height(170).Done;
  LMemo.Binds.Value(NyxTextState('note'));
  LCard.Add(LMemo);
  LCard.Add(NewNyxButton('try-action').Configure.Text('Try the action').Done);
  LHelp := NewNyxLabel('result-help');
  LHelp.Text := 'This last section belongs to the same live compound. Its text '
    + 'should scale with the rest of the card instead of reflowing outside a '
    + 'container that has become shorter. Camera changes never replace your editor.';
  LCard.Add(LHelp);
  LResult := NewNyxLabel('result');
  LResult.Binds.Text(NyxTextState('result'));
  LCard.Add(LResult);
  GScene := NewNyxCanvasScene;
  LLayer := NyxCanvasLayer('Content');
  GScene.AddLayer(LLayer);
  GItem := NyxCanvasItem('Notebook');
  GScene.AddControl(GItem,
    NyxCanvasPlacement(LLayer, NyxCanvasBounds(0, 0, 450, 960)), LCard);
  GWorkspace := NewNyxCanvasWorkspace('workspace', GScene)
    .WithScaling(nccsUniform)
    .WithCamera(NyxCanvasCamera(1280, 900, NyxCanvasPoint(225, 480)));

  if GLegacy then
  begin
    GWorkspace.WithScaling(nccsReflow);
  end;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('height', 'calc(100vh - 32px)');
  GHost.style.setProperty('width', '100%');
  document.body.appendChild(GHost);
  GView := MountNyxBrowserCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  GInput := TJSHTMLTextAreaElement(GView.InputFor(GItem, NyxControl('note')));
  {$else}
  Application.Initialize;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, 1280, 800);
  GHost.Show;
  GView := MountNyxLCLCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  Application.ProcessMessages;
  GInput := TMemo(GView.InputFor(GItem, NyxControl('note')));
  {$endif}
  GSubscription := GView.EventsFor(GItem).On(NyxControlEvents('try-action'), ntClick)
    .Subscribe(TActionCallback.Create);
end;

procedure Step;
var
  LZoom: Double;
  {$ifdef PAS2JS}
  LInputBox: TJSDOMRect;
  LResultBox: TJSDOMRect;
  LClip: TJSDOMRect;
  LElement: TJSHTMLElement;
  {$else}
  LResult: TControl;
  LPoint: TPoint;
  LSurface: TPoint;
  {$endif}
begin

  if GStage = 0 then
  begin
    {$ifdef PAS2JS}
    GInput.focus;
    GInput.selectionStart := 2;
    GInput.selectionEnd := 5;
    LInputBox := GInput.getBoundingClientRect;
    GBaseWidth := LInputBox.width;
    GBaseHeight := LInputBox.height;
    {$else}
    GInput.SetFocus;
    GInput.SelStart := 2;
    GInput.SelLength := 3;
    GBaseWidth := GInput.Width;
    GBaseHeight := GInput.Height;
    {$endif}

    if GLegacy then
    begin
      { The differential path isolates Fit clipping under the old policy;
        it does not impose uniform geometry on deliberate legacy reflow. }
      GStage := Length(CZooms);
    end;
  end
  else if GStage <= Length(CZooms) then
  begin
    LZoom := CZooms[GStage - 1];
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(225, 480), LZoom));
    Check(GView.InputFor(GItem, NyxControl('note')) = GInput, 'zoom retains the same editor');
    {$ifdef PAS2JS}
    Check((GInput.value = CNote) and (document.activeElement = GInput),
      'zoom preserves exact text and actual editor focus');
    Check((GInput.selectionStart = 2) and (GInput.selectionEnd = 5),
      'zoom retains the actual native browser selection');
    LInputBox := GInput.getBoundingClientRect;
    Check((Abs(LInputBox.width - GBaseWidth * LZoom) < 2) and
      (Abs(LInputBox.height - GBaseHeight * LZoom) < 2),
      'live editor geometry scales from its stable logical baseline');
    {$else}
    Check((TNyxText(GInput.Text) = CNote) and GInput.Focused,
      'zoom preserves exact text and actual native editor focus');
    Check((GInput.SelStart = 2) and (GInput.SelLength = 3), 'zoom retains native selection');
    Check((Abs(GInput.Width - GBaseWidth * LZoom) < 2) and
      (Abs(GInput.Height - GBaseHeight * LZoom) < 2), 'native editor uses absolute logical geometry');
    {$endif}
  end
  else
  begin
    GView.Execute(nccFit);
    {$ifdef PAS2JS}
    LElement := GView.RendererFor(GItem).ElementFor('result');
    LResultBox := LElement.getBoundingClientRect;
    LClip := GView.Surface.getBoundingClientRect;
    Check((LResultBox.top >= LClip.top - 1) and (LResultBox.bottom <= LClip.bottom + 1),
      'Fit leaves the actual final result inside the canvas viewport');
    while (LElement <> GView.Surface) and (LElement.parentElement <> nil) do
    begin
      LElement := TJSHTMLElement(LElement.parentElement);

      if window.getComputedStyle(LElement).getPropertyValue('overflow') = 'hidden' then
      begin
        LClip := LElement.getBoundingClientRect;
        Check((LResultBox.top >= LClip.top - 1) and (LResultBox.bottom <= LClip.bottom + 1),
          'the fitted result is not hidden by an ancestor clip');
      end;
    end;
    document.body.setAttribute('data-uniform-canvas', 'passed');
    document.body.setAttribute('data-uniform-checks', IntToStr(GChecks));
    {$else}
    LResult := GView.RendererFor(GItem).ControlFor('result');
    LPoint := LResult.ClientToScreen(Point(0, 0));
    LSurface := GView.Surface.ClientToScreen(Point(0, 0));
    Check((LPoint.Y >= LSurface.Y) and
      (LPoint.Y + LResult.Height <= LSurface.Y + GView.Surface.ClientHeight + 1),
      'native Fit retains the final live result in the viewport');
    {$endif}
    GDone := True;
  end;
  Inc(GStage);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if not GDone then
    begin
      window.setTimeout(@Drive, 30);
    end;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-uniform-canvas', 'failed');
      document.body.setAttribute('data-uniform-error', LException.Message);
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GLegacy := Pos('reflow', window.location.search) > 0;
  Build;
  window.setTimeout(@Drive, 30);
  {$else}
  try
    GLegacy := (ParamCount > 0) and (ParamStr(1) = '--reflow');
    Build;
    while not GDone do
    begin
      Application.ProcessMessages;
      Step;
    end;
    WriteLn('PASS ', GChecks, ' live uniform canvas checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL at stage ', GStage, ' / ', LException.Message);
      ExitCode := 1;
    end;
  end;

  if GView <> nil then
  begin
    GView.Disconnect;
  end;
  GSubscription := nil;
  GView := nil;
  GWorkspace := nil;
  GScene := nil;
  GHost.Free;
  GState.Free;
  GDocument.Free;
  {$endif}
end.
