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
program nyx_split_controls_tests;

{$mode delphi}{$H+}
{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.behavior, nyx.split,
  nyx.test.split,
  {$ifdef PAS2JS}
  JS, Web, nyx.render.browser, nyx.test.keyboard.browser;
  {$else}
  Interfaces, Forms, Controls, StdCtrls, Types, Classes, nyx.render.lcl, nyx.split.lcl;
  {$endif}

type
  TSink = class
    Count: Integer;
    Last: TNyxEventInfo;
    Navigate: Boolean;
    {$ifdef PAS2JS}Renderer: TNyxBrowserRenderer;{$else}Renderer: TNyxLCLRenderer;{$endif}
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;
  {$ifdef PAS2JS}
  TPointer = class external name 'PointerEvent' (TJSPointerEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TGripAccess = class(TNyxLCLSplitGrip);
  {$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Split controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TSink.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.Trigger = ntChange then
  begin
    Inc(Count);
    Last := AEvent.Copy;

    if Navigate then
    begin
      Renderer.Unmount;
    end;
  end;
end;

{$ifdef PAS2JS}
procedure Pointer(AHost: TJSHTMLElement; const AType: String;
  AX, AY: Double; AID: Integer = 1);
var
  LOptions: TJSObject;
begin
  LOptions := TJSObject.new;
  LOptions['clientX'] := AX;
  LOptions['clientY'] := AY;
  LOptions['pointerId'] := AID;
  LOptions['pointerType'] := 'touch';
  LOptions['isPrimary'] := True;
  LOptions['button'] := 0;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  AHost.dispatchEvent(TPointer.new(AType, LOptions));
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LSink: TSink;
  LWire: TNyxText;
  LBefore: Integer;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LSplit: TJSHTMLElement;
  LGrip: TJSHTMLElement;
  LInput: TJSHTMLTextAreaElement;
  LX, LY: Double;
  LHeight: Double;
  {$else}
  LHost: TForm;
  LSplit: TNyxLCLSplitView;
  LGrip: TGripAccess;
  LInput: TMemo;
  LPoint: TPoint;
  LStart: TPoint;
  LKey: Word;
  LWidth: Integer;
  {$endif}
begin
  LDocument := CreateNyxSplitFixture;
  LSink := TSink.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.cssText := 'width:700px;max-width:100%;height:700px;';
  document.body.appendChild(LHost);
  LSink.Renderer := TNyxBrowserRenderer.Create;
  {$else}
  Application.Initialize;
  LHost := TForm.Create(nil);
  LHost.SetBounds(20, 20, 700, 700);
  LHost.Show;
  LSink.Renderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LSink.Renderer.OnEvent := LSink.Event;
    LSink.Renderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$ifdef PAS2JS}
    LSplit := LSink.Renderer.ElementFor('work-split');
    LGrip := TJSHTMLElement(LSplit.querySelector('.nyx-split-divider'));
    LInput := TJSHTMLTextAreaElement(LSink.Renderer.ElementFor('second-pane').querySelector('textarea'));
    Check((LGrip.getAttribute('aria-orientation') = 'horizontal') and
      (LGrip.getAttribute('aria-disabled') = 'false'), 'actual browser rule and separator');
    Check(LGrip.getBoundingClientRect.height = 44, 'touch hit area');
    {$else}
    Application.ProcessMessages;
    LSplit := TNyxLCLSplitView(LSink.Renderer.ControlFor('work-split'));
    Check((LSplit.State.Orientation = nsoSideBySide) and not LSplit.Grip.Enabled,
      'actual native rule disables resizing');
    LDocument.Find('work-split').Configure.ForPlatform(npfNativeLCL).SplitResizable(True);
    LSink.Renderer.Render(LDocument, LDocument.Pages[0], LHost);
    Application.ProcessMessages;
    LSplit := TNyxLCLSplitView(LSink.Renderer.ControlFor('work-split'));
    LGrip := TGripAccess(LSplit.Grip);
    LInput := TMemo(LSink.Renderer.InputFor('second-pane'));
    Check(LGrip.Enabled and LGrip.TabStop, 'native grip is focusable');
    Check(LGrip.Width = 44, 'native divider extent');
    {$endif}
    LWire := TNyxCodec.Encode(LDocument);
    {$ifdef PAS2JS}
    LInput.value := 'Keep exact edit / 🌙漢字';
    LInput.focus;
    LInput.selectionStart := 5;
    LInput.selectionEnd := 10;
    LHeight := LSink.Renderer.ElementFor('second-pane').getBoundingClientRect.height;
    LX := LGrip.getBoundingClientRect.left + 12;
    LY := LGrip.getBoundingClientRect.top + 12;
    Pointer(LGrip, 'pointerdown', LX, LY);
    Check(LGrip.getAttribute('aria-valuenow') = '65', 'touch begins without snapping');
    Pointer(LGrip, 'pointermove', LX, LY - 96, 2);
    Check(LGrip.getAttribute('aria-valuenow') = '65', 'unrelated pointer refused');
    Pointer(LGrip, 'pointermove', LX, LY - 96);
    Pointer(LGrip, 'pointerup', LX, LY - 96);
    Check((LSink.Renderer.ElementFor('second-pane').querySelector('textarea') = LInput) and
      (LInput.value = 'Keep exact edit / 🌙漢字'), 'touch retains actual input and exact draft');
    Check((LInput.selectionStart = 5) and (LInput.selectionEnd = 10),
      'touch retains caret selection');
    Check(LSink.Renderer.ElementFor('second-pane').getBoundingClientRect.height > LHeight,
      'second pane gains visible height');
    LBefore := StrToInt(LGrip.getAttribute('aria-valuenow'));
    Pointer(LGrip, 'pointerdown', LX, LY);
    Pointer(LGrip, 'pointermove', LX, LY + 100);
    Pointer(LGrip, 'pointercancel', LX, LY + 100);
    Check(StrToInt(LGrip.getAttribute('aria-valuenow')) = LBefore, 'pointer cancellation restores proportion');
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    Check(LGrip.getAttribute('aria-valuenow') = '10', 'keyboard minimum');
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'ArrowDown', [nmShift]));
    Check(LGrip.getAttribute('aria-valuenow') = '20', 'keyboard coarse adjustment');
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'End'));
    Check(LGrip.getAttribute('aria-valuenow') = '90', 'keyboard maximum');
    LSplit.style.setProperty('height', '600px');
    Check(LGrip.getAttribute('aria-valuenow') = '90', 'host resize retains proportion');
    LSplit.style.setProperty('height', '20px');
    Check(LGrip.getBoundingClientRect.height <= 20, 'tiny browser host bounds its divider');
    LSplit.style.setProperty('height', '600px');
    {$else}
    LInput.Text := 'Keep exact edit / 🌙漢字';
    LInput.SetFocus;
    LInput.SelStart := 5;
    LInput.SelLength := 5;
    LSink.Count := 0;
    LWidth := LSplit.Panes[1].Width;
    LStart := LGrip.ClientToScreen(Point(12, 12));
    LGrip.MouseDown(mbLeft, [], 12, 12);
    Check(LSplit.State.Position = 65, 'native drag begins without snapping');
    LGrip.MouseMove([ssLeft], -84, 12);
    LPoint := LGrip.ScreenToClient(Point(LStart.X - 96, LStart.Y));
    LGrip.MouseUp(mbLeft, [], LPoint.X, LPoint.Y);
    Check((LSink.Renderer.InputFor('second-pane') = LInput) and
      (LInput.Text = 'Keep exact edit / 🌙漢字'), 'native drag retains actual memo and Unicode draft');
    Check((LInput.SelStart = 5) and (LInput.SelLength = 5), 'native drag retains caret');
    Check(LSplit.Panes[1].Width > LWidth, 'native second pane gains space');
    LBefore := LSplit.State.Position;
    LGrip.MouseDown(mbLeft, [], 12, 12);
    LGrip.MouseMove([ssLeft], 120, 12);
    LGrip.MouseCapture := False;
    Check(LSplit.State.Position = LBefore, 'native capture loss cancels');
    LKey := $24;
    LGrip.KeyDown(LKey, []);
    Check((LKey = 0) and (LSplit.State.Position = 10), 'native keyboard minimum consumes key');
    LKey := $27;
    LGrip.KeyDown(LKey, [ssShift]);
    Check(LSplit.State.Position = 20, 'native keyboard coarse adjustment');
    LKey := $23;
    LGrip.KeyDown(LKey, []);
    Check(LSplit.State.Position = 90, 'native keyboard maximum');
    LSplit.Width := 620;
    Check(LSplit.State.Position = 90, 'native host resize retains proportion');
    LSplit.Width := 20;
    Check(LGrip.Width <= 20, 'tiny native host bounds its divider');
    LSplit.Width := 620;
    {$endif}
    Check(LSink.Count = 4, 'only completed changes dispatch');
    Check(LSink.Last.HasValue and (LSink.Last.Value.AsInteger = 90),
      'actual change contains typed percent snapshot');
    Check(TNyxCodec.Encode(LDocument) = LWire, 'widget interaction leaves authored rules unchanged');
    LSink.Renderer.Root.Configure.ReadOnly(True);
    LSink.Renderer.Sync;
    {$ifdef PAS2JS}
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    Check(LGrip.getAttribute('aria-valuenow') = '90',
      'inherited browser read-only refuses keyboard resizing');
    {$else}
    LKey := $24;
    LGrip.KeyDown(LKey, []);
    Check((LSplit.State.Position = 90) and not LGrip.Enabled,
      'inherited native read-only refuses keyboard resizing');
    {$endif}
    LSink.Renderer.Root.Configure.ReadOnly(False);
    LSink.Renderer.Sync;
    {$ifdef PAS2JS}
    Pointer(LGrip, 'pointerdown', LX, LY);
    Pointer(LGrip, 'pointermove', LX, LY - 96);
    Check(LGrip.getAttribute('aria-valuenow') <> '90', 'browser permission test begins a real drag');
    {$else}
    LGrip.MouseDown(mbLeft, [ssLeft], 8, 8);
    LGrip.MouseMove([ssLeft], -80, 8);
    Check(LSplit.State.Position <> 90, 'native permission test begins a real drag');
    {$endif}
    LSink.Renderer.Root.Configure.ReadOnly(True);
    LSink.Renderer.Sync;
    {$ifdef PAS2JS}
    Check(LGrip.getAttribute('aria-valuenow') = '90', 'browser permission loss cancels captured drag');
    {$else}
    Check((LSplit.State.Position = 90) and not LSplit.State.Dragging,
      'native permission loss cancels captured drag');
    {$endif}
    Check(LSink.Count = 4, 'permission cancellation does not dispatch a completed change');
    LSink.Renderer.Root.Configure.ReadOnly(False);
    LSink.Renderer.Sync;
    LSink.Navigate := True;
    {$ifdef PAS2JS}
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    {$else}
    LKey := $24;
    LGrip.KeyDown(LKey, []);
    {$endif}
    Check(LSink.Renderer.Root = nil, 'resize callback may dispose mounted view');
    Check(LSink.Last.Value.AsInteger = 10, 'callback snapshot survives disposal');
  finally
    LSink.Renderer.Free;
    LSink.Free;
    LDocument.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$else}
    LHost.Free;
    {$endif}
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' browser split controls';
    document.body.setAttribute('data-split-controls', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' native split controls');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-split-controls', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
