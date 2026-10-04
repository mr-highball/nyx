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
program nyx_interaction_controls_tests;

{$mode delphi}{$H+}
{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.behavior,
  nyx.events, nyx.callbacks, nyx.scheduler, nyx.test.interactions,
  {$ifdef NYX_COMPILED_INTERACTIONS}nyx.interaction.generated,{$endif}
  {$ifdef PAS2JS}
  JS, Web, nyx.render.browser, nyx.test.keyboard.browser;
  {$else}
  Interfaces, Forms, Controls, StdCtrls, Classes, Types, nyx.render.lcl;
  {$endif}

type
  { The factory returns one retained probe to all independent registrations.
    The probe borrows its renderer only while this fixture is mounted. }
  TProbe = class(TNyxEventCallback, INyxCallbackFactory)
    Counts: array[TNyxTrigger] of Integer;
    Last: array[TNyxTrigger] of TNyxEventInfo;
    CanConsume: array[TNyxTrigger] of Boolean;
    Order: TNyxText;
    Consume: TNyxTrigger;
    Navigate: TNyxTrigger;
    {$ifdef PAS2JS}Renderer: TNyxBrowserRenderer;{$else}Renderer: TNyxLCLRenderer;{$endif}
    function Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
    procedure Reset;
  end;
  {$ifdef PAS2JS}
  TPointer = class external name 'PointerEvent' (TJSPointerEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  TMouse = class external name 'MouseEvent' (TJSMouseEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TAccess = class(TWinControl);
  TNativeFailure = class
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}

var
  GChecks: Integer;
  {$ifndef PAS2JS}
  GNativeFailure: TNativeFailure;
  {$endif}

{$ifndef PAS2JS}
procedure TNativeFailure.Failed(ASender: TObject; AException: Exception);
begin
  WriteLn(StdErr, 'FAIL native callback: ', AException.ClassName, ': ', AException.Message);
  DumpExceptionBackTrace(StdErr);
  Flush(StdErr);
  Halt(1);
end;
{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Interaction controls: ' + AReason);
  end;
  Inc(GChecks);
  {$ifndef PAS2JS}
  {$ifdef NYX_TRACE_EDITING}
  WriteLn('TRACE check ', GChecks, ': ', AReason);
  Flush(Output);
  {$endif}
  {$endif}
end;

function TProbe.Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
begin
  Result := Self;
end;

procedure TProbe.Reset;
var
  LTrigger: TNyxTrigger;
begin
  for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
  begin
    Counts[LTrigger] := 0;
  end;
  Order := '';
  Consume := ntDesignSelect;
  Navigate := ntDesignSelect;
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(Counts[AEvent.Trigger]);
  Last[AEvent.Trigger] := AEvent.Copy;
  Order := Order + NyxTriggerName(AEvent.Trigger) + '|';
  CanConsume[AEvent.Trigger] := NyxEventResponse(AExecution).CanConsume;

  if AEvent.Trigger = Consume then
  begin
    NyxEventResponse(AExecution).Consume;
  end;

  if AEvent.Trigger = Navigate then
  begin
    {$ifdef NYX_TRACE_EDITING}
    WriteLn('TRACE callback before unmount ', NyxTriggerName(AEvent.Trigger));
    Flush(Output);
    {$endif}
    Renderer.Unmount;
    {$ifdef NYX_TRACE_EDITING}
    WriteLn('TRACE callback after unmount');
    Flush(Output);
    {$endif}
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LProbe: TProbe;
  LFactory: INyxCallbackFactory;
  LWire: TNyxText;
  LValue: TNyxText;
  LRetained: TNyxEventInfo;
  LTrigger: TNyxTrigger;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LInput: TJSHTMLTextAreaElement;
  LCode: TJSHTMLElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LInput: TMemo;
  LCode: TWinControl;
  {$endif}

  function Key(ARelease: Boolean; ARepeat: Boolean = False; ACode: Boolean = False): Boolean;
  {$ifdef PAS2JS}
  var
    LEvent: TJSKeyboardEvent;
    LTarget: TJSHTMLElement;
  begin
    LTrigger := ntKeyDown;

    if ARelease then
    begin
      LTrigger := ntKeyUp;
    end;
    LEvent := NyxTestKeyboard(LTrigger, 'Enter', [nmControl], ARepeat);
    LTarget := LInput;

    if ACode then
    begin
      LTarget := LCode;
    end;
    LTarget.dispatchEvent(LEvent);
    Result := LEvent.defaultPrevented;
  end;
  {$else}
  var
    LKey: Word;
    LTarget: TWinControl;
  begin
    LKey := 13;
    LTarget := LInput;

    if ACode then
    begin
      LTarget := LCode;
    end;

    if ARelease then
    begin
      TAccess(LTarget).OnKeyUp(LTarget, LKey, [ssCtrl]);
    end
    else
    begin
      TAccess(LTarget).OnKeyDown(LTarget, LKey, [ssCtrl]);
    end;
    Result := LKey = 0;
  end;
  {$endif}

  procedure Text(const AValue: TNyxText);
  begin
    {$ifdef PAS2JS}
    LInput.value := AValue;
    LInput.dispatchEvent(TJSEvent.new('input'));
    {$else}
    LInput.Text := AValue;
    {$endif}
  end;

  function Pointer(ATrigger: TNyxTrigger): Boolean;
  {$ifdef PAS2JS}
  var
    LOptions: TJSObject;
    LBounds: TJSDOMRect;
    LName: String;
    LEvent: TJSMouseEvent;
  begin
    LBounds := LInput.getBoundingClientRect;
    LOptions := TJSObject.new;
    LOptions['clientX'] := LBounds.left + 12;
    LOptions['clientY'] := LBounds.top + 8;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := True;
    LOptions['buttons'] := 1;
    LOptions['button'] := 0;
    LOptions['pointerType'] := 'touch';
    LOptions['ctrlKey'] := True;
    case ATrigger of
      ntDoubleClick:
        begin
          LName := 'dblclick';
        end;
      ntPointerDown:
        begin
          LName := 'pointerdown';
        end;
      ntPointerUp:
        begin
          LName := 'pointerup';
        end;
      ntPointerMove:
        begin
          LName := 'pointermove';
        end;
      ntPointerEnter:
        begin
          LName := 'pointerenter';
        end;
      ntPointerExit:
        begin
          LName := 'pointerleave';
        end;
      ntContextMenu:
        begin
          LName := 'contextmenu';
        end;
    end;

    if ATrigger in [ntDoubleClick, ntContextMenu] then
    begin
      LEvent := TMouse.new(LName, LOptions);
    end
    else
    begin
      LEvent := TPointer.new(LName, LOptions);
    end;

    if ATrigger in [ntPointerEnter, ntPointerExit] then
    begin
      LRenderer.ElementFor('reply').dispatchEvent(LEvent);
    end
    else
    begin
      LInput.dispatchEvent(LEvent);
    end;
    Result := LEvent.defaultPrevented;
  end;
  {$else}
  var
    LHandled: Boolean;
  begin
    LHandled := False;
    case ATrigger of
      ntDoubleClick:
        begin
          TAccess(LInput).OnDblClick(LInput);
        end;
      ntPointerDown:
        begin
          TAccess(LInput).OnMouseDown(LInput, mbLeft, [ssLeft, ssCtrl], 12, 8);
        end;
      ntPointerUp:
        begin
          TAccess(LInput).OnMouseUp(LInput, mbLeft, [ssCtrl], 12, 8);
        end;
      ntPointerMove:
        begin
          TAccess(LInput).OnMouseMove(LInput, [ssLeft, ssCtrl], 12, 8);
        end;
      ntPointerEnter:
        begin
          TAccess(LInput).OnMouseEnter(LInput);
        end;
      ntPointerExit:
        begin
          TAccess(LInput).OnMouseLeave(LInput);
        end;
      ntContextMenu:
        begin
          TAccess(LInput).OnContextPopup(LInput, Point(12, 8), LHandled);
        end;
    end;
    Result := LHandled;
  end;
  {$endif}

begin
  LExpected := CreateNyxInteractionFixture;
  {$ifdef NYX_COMPILED_INTERACTIONS}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := CreateNyxInteractionFixture;
  {$endif}
  LProbe := TProbe.Create;
  LFactory := LProbe;
  LRenderer := {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif}.Create;
  LHost := nil;
  try
    LWire := TNyxCodec.Encode(LDocument);
    Check(LWire = TNyxCodec.Encode(LExpected), 'compiled/default complete event fixture agrees');
    LProbe.Renderer := LRenderer;
    LProbe.Reset;
    BindNyxCallbacks(LDocument, LRenderer.Events, LFactory);
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(LHost);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LInput := TJSHTMLTextAreaElement(LRenderer.ElementFor('reply').querySelector('textarea'));
    LCode := LRenderer.ElementFor('source-block');
    {$else}
    LHost := TForm.Create(nil);
    LHost.SetBounds(0, 0, 620, 680);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LInput := TMemo(LRenderer.InputFor('reply'));
    LCode := TWinControl(LRenderer.ControlFor('source-block'));
    { Allocate actual widgetset handles while keeping the fixture window hidden.
      A handle-less memo stores Text but cannot deliver physical change messages. }
    LInput.HandleNeeded;
    LCode.HandleNeeded;
    {$endif}
    LProbe.Reset;
    LRenderer.Events.OnKeyPress(NyxControlEvents('source-block')).Subscribe(LProbe);
    LRenderer.Events.OnPointerDown(NyxControlEvents('interactions')).Subscribe(LProbe);
    Check(not Key(False), 'unhandled keyboard retains its platform default');
    Check(LProbe.Order = 'before-key-down|key-down|after-key-down|' +
      'before-key-press|key-press|after-key-press|', 'ordered modern key actuation cycles');
    Check(LProbe.Last[ntKeyPress].Keyboard.Matches(nkEnterKey, [nmControl]),
      'key press owns typed shortcut data');
    Check(LProbe.CanConsume[ntBeforeKeyDown] and
      not LProbe.CanConsume[ntAfterKeyDown], 'after key hooks cannot consume platform defaults');
    Check(not LProbe.Last[ntBeforeKeyDown].HasValue and
      LProbe.Last[ntBeforeKeyDown].HasKeyboard and
      LProbe.Last[ntKeyDown].HasValue and not LProbe.Last[ntAfterKeyDown].HasValue,
      'down phases capture their own declared scalar and shortcut contracts');
    Check(not LProbe.Last[ntBeforeKeyPress].HasValue and
      LProbe.Last[ntKeyPress].HasValue and
      (LProbe.Last[ntKeyPress].Value.AsText = 'Original / 🌙') and
      not LProbe.Last[ntAfterKeyPress].HasValue,
      'press phases preserve distinct payload declarations');
    Key(True);
    Check(Pos('before-key-up|key-up|after-key-up|', LProbe.Order) > 0,
      'key release has independent before/main/after hooks');
    Check(not LProbe.Last[ntBeforeKeyUp].HasValue and LProbe.Last[ntKeyUp].HasValue and
      not LProbe.Last[ntAfterKeyUp].HasValue,
      'release phases preserve distinct payload declarations');
    LProbe.Reset;
    LProbe.Consume := ntBeforeKeyPress;
    Check(Key(False), 'before key press consumes the platform default');
    Check((LProbe.Counts[ntKeyPress] = 0) and
      LProbe.Last[ntAfterKeyPress].DefaultPrevented, 'cancelled press skips main and reports completion');
    LProbe.Reset;
    Key(True);
    Key(False);
    Key(False, True);
    Check(LProbe.Last[ntKeyPress].Keyboard.Repeating, 'repeat survives key press dispatch');
    LProbe.Reset;
    Key(False, False, True);
    Check(LProbe.Counts[ntKeyPress] = 1, 'read-only code block has real keyboard access');
    {$ifdef PAS2JS}
    LCode := LRenderer.ElementFor('reference');
    {$else}
    LCode := TWinControl(LRenderer.ControlFor('reference'));
    {$endif}
    LRenderer.Events.OnKeyPress(NyxControlEvents('reference')).Subscribe(LProbe);
    LProbe.Reset;
    Key(False, False, True);
    Check((LProbe.Counts[ntKeyPress] = 1) and
      (LProbe.Last[ntKeyPress].OriginID = 'reference'),
      'link projection is keyboard reachable on the actual target');

    LProbe.Reset;
    LValue := 'Typed, pasted or composed / 🌙 漢字';
    Text(LValue);
    Check((LProbe.Counts[ntBeforeTextInput] = 1) and
      (LProbe.Counts[ntTextInput] = 1) and (LProbe.Counts[ntAfterTextInput] = 1),
      'one complete text admission cycle: ' +
      IntToStr(LProbe.Counts[ntBeforeTextInput]) + '/' +
      IntToStr(LProbe.Counts[ntTextInput]) + '/' +
      IntToStr(LProbe.Counts[ntAfterTextInput]) + ' ' + LRenderer.LastBindingError);
    Check((LProbe.Last[ntTextInput].TextEdit.Before = 'Original / 🌙') and
      (LProbe.Last[ntTextInput].TextEdit.After = LValue) and
      (LRenderer.Root.Find('reply').Prop('value') = LValue),
      'exact accepted Unicode text replacement');
    Check(not LProbe.Last[ntBeforeTextInput].HasValue and
      LProbe.Last[ntBeforeTextInput].HasTextEdit and
      LProbe.Last[ntTextInput].HasValue and
      (LProbe.Last[ntTextInput].Value.AsText = LValue) and
      not LProbe.Last[ntAfterTextInput].HasValue and
      LProbe.Last[ntAfterTextInput].HasTextEdit,
      'text phase scalar contracts remain independent of exact edit snapshots');
    LRetained := LProbe.Last[ntTextInput].Copy;
    LProbe.Reset;
    LProbe.Consume := ntBeforeTextInput;
    Text('Rejected');
    Check((LRenderer.Root.Find('reply').Prop('value') = LValue) and
      (LProbe.Counts[ntChange] = 0) and (LProbe.Counts[ntTextInput] = 0),
      'consumed text proposal has no partial accepted command');
    Check(LProbe.Last[ntAfterTextInput].DefaultPrevented and
      (LProbe.Last[ntAfterTextInput].TextEdit.After = 'Rejected'),
      'cancelled text preserves an owned proposal');
    Check(not LProbe.Last[ntAfterTextInput].HasValue and
      (LProbe.Last[ntAfterTextInput].TextEdit.Before = LValue),
      'cancelled text completion retains its own signal-only declaration');
    {$ifdef PAS2JS}
    Check(LInput.value = LValue, 'rejected browser proposal restores physical text');
    {$else}
    Check(TNyxText(LInput.Text) = LValue, 'rejected native proposal restores physical text');
    {$endif}
    LProbe.Reset;

    for LTrigger := ntDoubleClick to ntContextMenu do
    begin
      Pointer(LTrigger);
      Check((LProbe.Counts[LTrigger] = 1) and LProbe.Last[LTrigger].HasPointer,
        'actual physical bridge: ' + NyxTriggerTitle(LTrigger));
    end;
    Check((LProbe.Last[ntPointerDown].Pointer.Button = npbPrimary) and
      (nmControl in LProbe.Last[ntPointerDown].Pointer.Modifiers),
      'typed pointer button and modifiers');
    Check(LProbe.Last[ntPointerDown].Pointer.Kind =
      {$ifdef PAS2JS}npiTouch{$else}npiMouse{$endif}, 'actual pointer kind is explicit');
    Check(LProbe.Last[ntPointerDown].Pointer.HasPosition and
      (LProbe.Last[ntPointerDown].Pointer.X >= 12), 'control-relative owned pointer position');
    LProbe.Consume := ntContextMenu;
    Check(Pointer(ntContextMenu), 'context menu default can be consumed sequentially');
    LProbe.Reset;
    LRenderer.Root.Configure.ReadOnly(True);
    LRenderer.Root.Find('reply').Configure.ReadOnly(False);
    LRenderer.Sync;
    Check(LInput.{$ifdef PAS2JS}readOnly{$else}ReadOnly{$endif} and
      {$ifdef PAS2JS}not LInput.disabled{$else}LInput.Enabled{$endif},
      'inherited read-only uses the actual text flag and retains enabled interaction');
    Text('Forbidden inherited edit / 🌙');
    Check((LRenderer.Root.Find('reply').Prop('value') = LValue) and
      (LProbe.Counts[ntChange] = 0) and (LProbe.Counts[ntBeforeTextInput] = 0),
      'read-only ancestor refuses physical drafts before proposal callbacks');
    Check({$ifdef PAS2JS}LInput.value{$else}TNyxText(LInput.Text){$endif} = LValue,
      'read-only refusal restores exact physical accepted text');
    Key(False);
    Check(LProbe.Counts[ntKeyPress] = 1,
      'read-only memo retains keyboard callbacks against accepted text');
    LProbe.Reset;
    LRenderer.Root.Configure.ReadOnly(False).Enabled(False);
    LRenderer.Sync;
    Check({$ifdef PAS2JS}LInput.disabled{$else}not LInput.Enabled{$endif},
      'disabled ancestor reaches the actual input');
    Text('Forbidden disabled edit');
    Key(False);
    Check((LProbe.Counts[ntBeforeTextInput] = 0) and (LProbe.Counts[ntKeyPress] = 0) and
      (LRenderer.Root.Find('reply').Prop('value') = LValue),
      'disabled scope has no physical edit or keyboard invocation');
    LRenderer.Root.Configure.Enabled(True).Visible(False);
    LRenderer.Sync;
    Text('Forbidden hidden edit');
    Key(False);
    Check((LProbe.Counts[ntBeforeTextInput] = 0) and (LProbe.Counts[ntKeyPress] = 0) and
      (LRenderer.Root.Find('reply').Prop('value') = LValue),
      'hidden scope has no physical edit or keyboard invocation');
    LRenderer.Root.Configure.Visible(True);
    LRenderer.Sync;
    LProbe.Reset;
    Text(LValue + ' / resumed');
    Check(LProbe.Counts[ntTextInput] = 1, 'restoring policy reactivates the mounted editor');
    Check(TNyxCodec.Encode(LDocument) = LWire, 'runtime interactions never alter authored document');
    LProbe.Reset;
    LProbe.Navigate := ntBeforeKeyDown;
    Check(Key(False) and (LProbe.Counts[ntKeyDown] = 0),
      'navigation in before hook safely ends remaining keyboard cycle');
    Check(LRetained.TextEdit.After = LValue, 'text snapshot survives unmount');
    {$ifdef NYX_TRACE_EDITING}
    WriteLn('TRACE before remount');
    Flush(Output);
    {$endif}
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$ifdef NYX_TRACE_EDITING}
    WriteLn('TRACE after remount');
    Flush(Output);
    {$endif}
    {$ifdef PAS2JS}
    LInput := TJSHTMLTextAreaElement(LRenderer.ElementFor('reply').querySelector('textarea'));
    {$else}
    LInput := TMemo(LRenderer.InputFor('reply'));
    LInput.HandleNeeded;
    {$endif}
    LProbe.Reset;
    LProbe.Navigate := ntTextInput;
    {$ifdef NYX_TRACE_EDITING}
    WriteLn('TRACE before navigation text');
    Flush(Output);
    {$endif}
    Text('Navigate from accepted text / 🌙');
    {$ifdef NYX_TRACE_EDITING}
    WriteLn('TRACE after navigation text');
    Flush(Output);
    {$endif}
    Check((LProbe.Counts[ntTextInput] = 1) and (LProbe.Counts[ntAfterTextInput] = 0),
      'text callback navigation safely suppresses later phases');
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$ifdef PAS2JS}
    LInput := TJSHTMLTextAreaElement(LRenderer.ElementFor('reply').querySelector('textarea'));
    {$else}
    LInput := TMemo(LRenderer.InputFor('reply'));
    LInput.HandleNeeded;
    {$endif}
    LProbe.Reset;
    LProbe.Navigate := ntPointerDown;
    Pointer(ntPointerDown);
    Check((LProbe.Counts[ntPointerDown] = 1) and
      LProbe.Last[ntPointerDown].Pointer.HasPosition,
      'pointer navigation neither duplicates an ancestor nor uses stale widgets');
    LProbe.Renderer := nil;
  finally
    LRenderer.Free;
    {$ifdef PAS2JS}

    if LHost <> nil then
    begin
      LHost.remove;
    end;
    {$else}
    LHost.Free;
    {$endif}
    LFactory := nil;
    LDocument.Free;
    LExpected.Free;
  end;
end;

begin
  try
    {$ifndef PAS2JS}
    {$ifdef NYX_TRACE_EDITING}
    WriteLn('TRACE before Application.Initialize');
    Flush(Output);
    {$endif}
    Application.Initialize;
    { A real LCL callback failure must fail the harness, never wait for a modal
      exception dialog when running without an interactive desktop. }
    Application.CaptureExceptions := False;
    GNativeFailure := TNativeFailure.Create;
    Application.OnException := GNativeFailure.Failed;
    {$ifdef NYX_TRACE_EDITING}
    WriteLn('TRACE after Application.Initialize');
    Flush(Output);
    {$endif}
    {$endif}
    Run;
    {$ifndef PAS2JS}
    Application.OnException := nil;
    FreeAndNil(GNativeFailure);
    {$endif}
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' interaction controls';
    document.body.setAttribute('data-interaction-controls', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' interaction controls');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-interaction-controls', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
