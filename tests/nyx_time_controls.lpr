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

program nyx_time_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,
  {$else}Interfaces, Classes, Forms, Controls, StdCtrls, ComCtrls, LCLIntf, LCLType, Graphics,
    IntfGraphics, FPWritePNG, nyx.times.lcl, nyx.render.lcl,{$endif}
  SysUtils, nyx.text, nyx.times, nyx.types, nyx.model, nyx.contract,
  nyx.controls, nyx.state, nyx.behavior, nyx.events, nyx.scheduler, nyx.codec,
  nyx.generated.time;

type
  { Each actual target delivers two ordered callbacks. The final accepted edit
    requests retirement to qualify copied event data and deferred physical disposal. }
  TClockObserver = class(TNyxEventCallback)
  private
    FMarker: Integer;
  public
    constructor Create(AMarker: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}
  TClockInputEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TClockEditAccess = class(TCustomEdit)
  public
    procedure Complete;
    procedure SendKey(var AKey: Word);
  end;
  TClockArrowAccess = class(TCustomUpDown)
  public
    procedure Activate(AButton: TUDBtnType);
  end;
  {$endif}

var
  GDocument: TNyxDocument;
  GChecks: Integer;
  GOrder: TNyxText;
  GSnapshot: TNyxEventInfo;
  GRetire: Boolean;
  GOriginal: TNyxText;
  {$ifdef PAS2JS}
  GView: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  {$else}
  GView: TNyxLCLRenderer;
  GHost: TForm;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Clock controls: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TClockObserver.Create(AMarker: Integer);
begin
  inherited Create;
  FMarker := AMarker;
end;

procedure TClockObserver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GOrder := GOrder + TNyxText(IntToStr(FMarker));
  GSnapshot := AEvent.Copy;

  if GRetire and (FMarker = 2) then
  begin
    GRetire := False;
    GView.Unmount;
  end;
end;

procedure Pump;
begin
  {$ifndef PAS2JS}
  Application.ProcessMessages;
  {$endif}
end;

function InputValue(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(GView.InputFor(AID, niRuntime)).value;
  {$else}
  Result := TNyxText(TCustomEdit(GView.InputFor(AID, niRuntime)).Text);
  {$endif}
end;

procedure Edit(const AID, AText: TNyxText);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
  LInput: TJSHTMLInputElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(GView.InputFor(AID, niRuntime));
  LInput.value := AText;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LInput.dispatchEvent(TClockInputEvent.new('input', LOptions));
  {$else}
  TCustomEdit(GView.InputFor(AID, niRuntime)).Text := AText;
  {$endif}
  Pump;
end;

{$ifndef PAS2JS}
procedure TClockEditAccess.Complete;
begin
  EditingDone;
end;

procedure TClockEditAccess.SendKey(var AKey: Word);
begin
  KeyDown(AKey, []);
end;

procedure TClockArrowAccess.Activate(AButton: TUDBtnType);
begin
  Click(AButton);
end;

procedure Commit(const AID: TNyxText);
begin
  { Exercise the actual inner editor's retained grouped forwarding. }
  TClockEditAccess(GView.InputFor(AID, niRuntime)).Complete;
  Pump;
end;

function Field(const AID: TNyxText): TNyxLCLTimeField;
begin
  Check(GView.InputFor(AID, niRuntime).Parent is TNyxLCLTimeField,
    'the standard time projection owns a specialized native field');
  Result := TNyxLCLTimeField(GView.InputFor(AID, niRuntime).Parent);
end;

procedure OpenPicker(AField: TNyxLCLTimeField);
var
  LClient: TRect;
begin
  AField.Button.Click;
  Pump;
  Check((AField.Popup <> nil) and AField.Popup.Visible, 'the exact owned native picker opens');
  Check(LCLIntf.GetClientRect(AField.Popup.Handle, LClient) and
    (LClient.Right >= AField.AcceptButton.Left + AField.AcceptButton.Width) and
    (LClient.Bottom >= AField.AcceptButton.Top + AField.AcceptButton.Height),
    'the actual native client window contains the complete acceptance controls');
end;

procedure Pick(AField: TNyxLCLTimeField; AHour, AMinute, ASecond, AMillisecond: Integer);
begin
  AField.HourControl.Value := AHour;
  AField.MinuteControl.Value := AMinute;
  AField.SecondControl.Value := ASecond;
  AField.MillisecondControl.Value := AMillisecond;
  AField.AcceptButton.Click;
  Pump;
end;

procedure PickerKey(AField: TNyxLCLTimeField; AKey: Word);
begin
  AField.HourControl.OnKeyDown(AField.HourControl, AKey, []);
  Check(AKey = 0, 'the picker consumes its own command key');
  Pump;
end;

procedure Capture(AForm: TForm; const AName: String);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LBounds: TRect;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  { Native PaintTo is explicitly a diagnostic print, not foreground/displayed
    pixel evidence. Actual control/focus/window assertions qualify behavior. }
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := TFPWriterPNG.Create;
  try
    Check(LCLIntf.GetWindowRect(AForm.Handle, LBounds) <> 0,
      'the diagnostic print measures the actual outer window');
    LBitmap.SetSize(LBounds.Right - LBounds.Left, LBounds.Bottom - LBounds.Top);
    AForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LImage.Free;
    LWriter.Free;
    LBitmap.Free;
  end;
end;
{$endif}

procedure Journey;
var
  LRoot: TNyxNode;
  LBefore: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLInputElement;
  {$else}
  LStart: TNyxLCLTimeField;
  LEarliest: TNyxLCLTimeField;
  LLatest: TNyxLCLTimeField;
  LChoice: TNyxLCLTimeField;
  LKey: Word;
  {$endif}
begin
  { This is the exact previously compiled public-Pascal companion, not an active
    MCP design or a claim that the frozen MCP schema admits new clock policies. }
  GDocument := nyx.generated.time.BuildNyxDocument;
  GOriginal := TNyxCodec.Encode(GDocument);
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GView := TNyxBrowserRenderer.Create;
  {$else}
  Application.Initialize;
  GHost := TForm.CreateNew(nil);
  GHost.Caption := 'Meeting planner';
  GHost.SetBounds(20, 20, 640, 850);
  GHost.Show;
  GView := TNyxLCLRenderer.Create;
  {$endif}
  GView.Render(GDocument, GDocument.Pages[0], GHost);
  Pump;
  GView.Events.On(NyxControlEvents('start-time', niRuntime), ntChange)
    .Subscribe(TClockObserver.Create(1));
  GView.Events.On(NyxControlEvents('start-time', niRuntime), ntChange)
    .Subscribe(TClockObserver.Create(2));
  Check(InputValue('start-time') = '00:30:00.000', 'bound exact precision reaches the actual input');
  {$ifndef PAS2JS}
  TCustomEdit(GView.InputFor('start-time', niRuntime)).SetFocus;
  Edit('start-time', '01:00:00.1');
  Check((InputValue('start-time') = '01:00:00.1') and
    (GView.State.GetValue(NyxTextState('reminder')) = '00:30:00.000'),
    'an unfinished off-step fraction is a retained physical draft, not an accepted clock');
  GView.Sync;
  Check(InputValue('start-time') = '01:00:00.1', 'unrelated sync retains the exact clock draft');
  LStart := Field('start-time');
  Check(LStart.Editor = GView.InputFor('start-time', niRuntime), 'focus/input hooks use the real inner editor');
  Commit('start-time');
  Check((InputValue('start-time') = '00:30:00.000') and (GView.LastBindingError <> '') and
    (GOrder = ''), 'editing completion refuses off-step text without a successful callback');
  Edit('start-time', '1 pm');
  Commit('start-time');
  Check(InputValue('start-time') = '00:30:00.000', 'native locale shorthand is never coerced into a clock');
  {$else}
  LInput := TJSHTMLInputElement(GView.InputFor('start-time', niRuntime));
  Check((LInput.getAttribute('type') = 'time') and (LInput.getAttribute('min') = '22:00') and
    (LInput.getAttribute('max') = '02:00') and (LInput.getAttribute('step') = '1.5'),
    'browser projects the standard clock input and exact overnight step/bounds');
  LInput := TJSHTMLInputElement(GView.InputFor('earliest-time', niRuntime));
  Check((LInput.getAttribute('min') = '08:30') and not LInput.hasAttribute('max') and
    (LInput.getAttribute('step') = '0.125'), 'one-sided bounds and fractional steps project exactly');
  Check(TJSHTMLInputElement(GView.InputFor('latest-time', niRuntime)).getAttribute('step') = 'any',
    'the browser never substitutes its default minute step');
  {$endif}
  GOrder := '';
  Edit('start-time', '01:00:00.000');
  {$ifndef PAS2JS}Commit('start-time');{$endif}
  Check((GView.State.GetValue(NyxTextState('reminder')) = '01:00:00.000') and
    (GOrder = '12'), 'an accepted physical clock commits exact state and two ordered callbacks');
  Check(GSnapshot.HasValue and (GSnapshot.Value.AsText = '01:00:00.000'),
    'the callback carries an independently retained exact clock value');
  GView.State.SetValue(NyxTextState('reminder'), '23:00:00.000');
  Check(InputValue('start-time') = '23:00:00.000', 'programmatic state refresh updates the same actual field');
  {$ifndef PAS2JS}
  OpenPicker(LStart);
  Check((LStart.HourControl.Value = 23) and (LStart.MillisecondControl.Value = 0),
    'the native picker starts from the accepted exact reading');
  GView.Sync;
  Check(LStart.Popup.Visible, 'unchanged domain sync preserves the picker context');
  Capture(LStart.Popup, 'picker');
  LStart.HourControl.Value := 12;
  LStart.AcceptButton.Click;
  Check(LStart.Popup.Visible and (LStart.StatusLabel.Caption <> '') and
    (InputValue('start-time') = '23:00:00.000'), 'out-of-range picking remains open with an exact diagnostic');
  Pick(LStart, 23, 0, 0, 1);
  Check(LStart.Popup.Visible and (InputValue('start-time') = '23:00:00.000'),
    'off-step picking cannot round or publish a candidate');
  Pick(LStart, 23, 0, 1, 500);
  Check(not LStart.Popup.Visible and (InputValue('start-time') = '23:00:01.500'),
    'the popup admits exact millisecond steps through ordinary shared state');
  LEarliest := Field('earliest-time');
  OpenPicker(LEarliest);
  Check(LEarliest.Popup <> LStart.Popup, 'each field owns its own picker instance');
  Pick(LEarliest, 8, 30, 0, 375);
  Check(InputValue('earliest-time') = '08:30:00.375', 'picking raises precision only when needed to retain milliseconds');
  OpenPicker(LEarliest);
  LEarliest.MillisecondControl.SetFocus;
  LEarliest.MillisecondControl.Text := '';
  LEarliest.HourControl.SetFocus;
  Pump;
  Check(LEarliest.MillisecondControl.Text = '',
    'actual picker focus loss retains an unfinished numeric draft for admission');
  LEarliest.AcceptButton.Click;
  Check(LEarliest.Popup.Visible and (LEarliest.StatusLabel.Caption <> '') and
    (InputValue('earliest-time') = '08:30:00.375'), 'unfinished numeric picker parts never become zero');
  LEarliest.MillisecondControl.Text := '1.5';
  LEarliest.MillisecondControl.SetFocus;
  LEarliest.HourControl.SetFocus;
  Pump;
  LEarliest.AcceptButton.Click;
  Check(LEarliest.Popup.Visible and (LEarliest.MillisecondControl.Text = '1.5') and
    (InputValue('earliest-time') = '08:30:00.375'),
    'fractional part paste and focus loss cannot silently round into an accepted integer');
  TClockArrowAccess(TCustomUpDown(LEarliest.MillisecondControl.Stepper)).Activate(btNext);
  Check(LEarliest.MillisecondControl.Text = '1.5',
    'native arrow activation preserves an invalid draft instead of supplying a substitute');
  LEarliest.MillisecondControl.Value := 998;
  TClockArrowAccess(TCustomUpDown(LEarliest.MillisecondControl.Stepper)).Activate(btNext);
  Check(LEarliest.MillisecondControl.Value = 999, 'native arrow activation advances an exact integer part');
  LKey := VK_UP;
  TClockEditAccess(TCustomEdit(LEarliest.MillisecondControl)).SendKey(LKey);
  Check((LKey = 0) and (LEarliest.MillisecondControl.Value = 999),
    'native part keyboard stepping consumes the key and clamps at its bound');
  LKey := VK_DOWN;
  TClockEditAccess(TCustomEdit(LEarliest.MillisecondControl)).SendKey(LKey);
  Check((LKey = 0) and (LEarliest.MillisecondControl.Value = 998),
    'native part keyboard stepping decrements exactly without a floating parser');
  PickerKey(LEarliest, VK_ESCAPE);
  Check(not LEarliest.Popup.Visible and (Screen.ActiveControl = LEarliest.Editor),
    'Escape cancels and returns focus to the exact editor');
  LLatest := Field('latest-time');
  OpenPicker(LLatest);
  LLatest.ClearButton.Click;
  Check(InputValue('latest-time') = '', 'Clear preserves an optional empty value');
  OpenPicker(LLatest);
  Check(InputValue('latest-time') = '', 'opening an empty field never substitutes the current time');
  PickerKey(LLatest, VK_RETURN);
  Check(InputValue('latest-time') = '00:00', 'explicit acceptance distinguishes defined midnight from empty');
  LChoice := Field('choice-time');
  OpenPicker(LChoice);
  Pick(LChoice, 9, 0, 0, 200);
  Check(LChoice.Popup.Visible and (InputValue('choice-time') = '09:00:00.1'),
    'choice membership rejects another reading without losing original precision');
  LChoice.CancelButton.Click;
  OpenPicker(LChoice);
  LChoice.AcceptButton.Click;
  Check(InputValue('choice-time') = '09:00:00.1', 'an unchanged choice retains its one-digit fraction');
  OpenPicker(LChoice);
  GView.Root.Find('choice-time').Contract.Value(NyxTimeDomain.Choices([
    NyxTime(9, 0, 0, 100).WithPrecision(ntpTenth)]));
  GView.Sync;
  Check(not LChoice.Popup.Visible, 'changed domain revokes an old picker context');
  LChoice.ClearButton.Click;
  Check(InputValue('choice-time') = '09:00:00.1', 'a hidden stale picker action cannot publish');
  OpenPicker(LChoice);
  LChoice.ClearButton.Click;
  Check(LChoice.Popup.Visible and (LChoice.StatusLabel.Caption <> ''),
    'a restricted choice list may exclude Clear without coercing a replacement');
  LChoice.CancelButton.Click;
  LBefore := InputValue('start-time');
  LRoot := GView.Root;
  OpenPicker(LStart);
  LRoot.Configure.ReadOnly(True).Done;
  GView.Sync;
  Check(not LStart.Popup.Visible and not LStart.Button.Enabled and LStart.Editor.ReadOnly,
    'inherited read-only closes the popup and reaches its actual controls');
  LStart.Button.Click;
  Check(not LStart.Popup.Visible, 'read-only prevents another picker opening');
  LRoot.Configure.ReadOnly(False).Enabled(False).Done;
  GView.Sync;
  Edit('start-time', '00:00');
  Commit('start-time');
  Check(InputValue('start-time') = LBefore, 'inherited disabled prevents clock publication');
  LRoot.Configure.Enabled(True).Done;
  GView.Sync;
  OpenPicker(LStart);
  LRoot.Configure.Visible(False).Done;
  GView.Sync;
  Check(not LStart.Popup.Visible, 'hidden ancestry closes its owned picker');
  LRoot.Configure.Visible(True).Done;
  GView.Sync;
  Capture(GHost, 'desktop');
  GHost.ClientWidth := 390;
  Pump;
  GView.Sync;
  Check((InputValue('start-time') = LBefore) and (LStart.Editor.Width > 0),
    'narrow resizing retains the same usable exact clock editor');
  Capture(GHost, 'narrow');
  {$else}
  LRoot := GView.Root;
  LBefore := InputValue('start-time');
  LRoot.Configure.ReadOnly(True).Done;
  GView.Sync;
  Check(TJSHTMLInputElement(GView.InputFor('start-time', niRuntime)).readOnly,
    'inherited read-only reaches the actual browser clock input');
  LRoot.Configure.ReadOnly(False).Enabled(False).Done;
  GView.Sync;
  Edit('start-time', '00:00');
  Check(InputValue('start-time') = LBefore, 'disabled browser proposals preserve the accepted reading');
  LRoot.Configure.Enabled(True).Done;
  GView.Sync;
  {$endif}
  Check(TNyxCodec.Encode(GDocument) = GOriginal, 'physical changes never mutate authored defaults or reusable recipes');
  GRetire := True;
  {$ifndef PAS2JS}
  OpenPicker(LStart);
  Pick(LStart, 23, 0, 3, 0);
  {$else}
  Edit('start-time', '23:00:03.000');
  {$endif}
  Check(GView.Root = nil, 'an accepted callback retires its own native/browser view safely');
  Check(GSnapshot.Value.AsText = '23:00:03.000', 'the last callback value outlives its controls');
  Pump;
end;

procedure Retire;
begin
  GView.Free;
  GView := nil;
  GDocument.Free;
  GDocument := nil;
  {$ifndef PAS2JS}
  GHost.Free;
  GHost := nil;
  {$endif}
end;

begin
  try
    Journey;
    Retire;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-time-controls', 'passed');
    document.body.setAttribute('data-time-control-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' actual native clock checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-time-controls', 'failed');
      document.body.setAttribute('data-time-error', LException.Message);
      {$else}
      WriteLn('FAIL after ', GChecks, ' checks / ', LException.Message);
      DumpExceptionBackTrace(Output);
      Retire;
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
