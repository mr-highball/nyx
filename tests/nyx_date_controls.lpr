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

program nyx_date_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$IFDEF PAS2JS}{$modeswitch externalclass}{$ENDIF}

uses
  {$IFDEF PAS2JS}JS, Web, nyx.render.browser,
  {$ELSE}Interfaces, Classes, Forms, Controls, StdCtrls, Calendar, LCLType,
    Graphics, IntfGraphics, FPWritePNG, nyx.dates.lcl, nyx.render.lcl,{$ENDIF}
  SysUtils, nyx.text, nyx.dates, nyx.types, nyx.data, nyx.contract, nyx.model,
  nyx.controls, nyx.composition, nyx.state, nyx.binding.types, nyx.behavior, nyx.events,
  nyx.scheduler, nyx.codec, nyx.codegen, nyx.test.dates, nyx.generated.view;

type
  { The renderer owns each callback. It borrows no physical control and retains
    only an accepted immutable event snapshot. Retirement is requested from the
    final change callback to exercise the physical adapter's lifetime boundary. }
  TChangeObserver = class(TNyxEventCallback)
  public
    Marker: Integer;
    constructor Create(AMarker: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$IFDEF PAS2JS}
  TInputEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$ENDIF}

var
  GDocument: TNyxDocument;
  GChecks: Integer;
  GOrder: TNyxText;
  GSnapshot: TNyxEventInfo;
  GRetainedSnapshot: TNyxEventInfo;
  GArrivalID: TNyxText;
  GOtherID: TNyxText;
  GOriginal: TNyxText;
  GRetire: Boolean;
  {$IFNDEF PAS2JS}
  GStackIndex: Integer;
  {$ENDIF}
  {$IFDEF PAS2JS}
  GView: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  {$ELSE}
  GView: TNyxLCLRenderer;
  GHost: TForm;
  {$ENDIF}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Date controls: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TChangeObserver.Create(AMarker: Integer);
begin
  inherited Create;
  Marker := AMarker;
end;

procedure TChangeObserver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GOrder := GOrder + TNyxText(IntToStr(Marker));
  GSnapshot := AEvent.Copy;

  if GRetire and (Marker = 2) then
  begin
    GRetire := False;
    GView.Unmount;
  end;
end;

procedure Pump;
begin
  {$IFNDEF PAS2JS}
  Application.ProcessMessages;
  {$ENDIF}
end;

function InputValue(const AID: TNyxText): TNyxText;
begin
  {$IFDEF PAS2JS}
  Result := TJSHTMLInputElement(GView.InputFor(AID, niRuntime)).value;
  {$ELSE}
  Result := TNyxText(TCustomEdit(GView.InputFor(AID, niRuntime)).Text);
  {$ENDIF}
end;

procedure Edit(const AID, AValue: TNyxText);
{$IFDEF PAS2JS}
var
  LOptions: TJSObject;
  LInput: TJSHTMLInputElement;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  LInput := TJSHTMLInputElement(GView.InputFor(AID, niRuntime));
  LInput.value := AValue;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LInput.dispatchEvent(TInputEvent.new('input', LOptions));
  {$ELSE}
  TCustomEdit(GView.InputFor(AID, niRuntime)).Text := AValue;
  {$ENDIF}
  Pump;
end;

{$IFNDEF PAS2JS}
function Field(const AID: TNyxText): TNyxLCLDateField;
begin
  Result := TNyxLCLDateField(GView.InputFor(AID, niRuntime).Parent);
end;

procedure CalendarKey(AField: TNyxLCLDateField; AKey: Word);
begin
  AField.CalendarControl.OnKeyDown(AField.CalendarControl, AKey, []);
  Check(AKey = 0, 'calendar command is consumed by its exact field');
  Pump;
end;

procedure CaptureNative(AForm: TForm; const ASuffix: String);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    AForm.Repaint;
    Pump;
    LBitmap.SetSize(AForm.Width, AForm.Height);
    AForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(ParamStr(1) + ASuffix + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$ENDIF}

procedure Journey;
var
  LRuntime: TNyxNode;
  LRefused: Boolean;
  {$IFNDEF PAS2JS}
  LField: TNyxLCLDateField;
  LOther: TNyxLCLDateField;
  {$ELSE}
  LInput: TJSHTMLInputElement;
  {$ENDIF}
begin
  GChecks := RunNyxDateTests;
  GDocument := BuildNyxDocument;
  { This is a typed-library enrichment of the exact MCP-authored companion.
    The live server cannot yet author date bounds. Keep that gap explicit. }
  ConfigureNyxDateReview(GDocument);
  GOriginal := TNyxCodec.Encode(GDocument);
  Check(Pos('.Value(NyxDate(2026, 10, 6))', TNyxCodegen.Generate(GDocument)) > 0,
    'the actual reusable arrival override generates a typed date');
  {$IFDEF PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GView := TNyxBrowserRenderer.Create;
  document.body.setAttribute('data-date-width', IntToStr(window.innerWidth));
  {$ELSE}
  Application.Initialize;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, 1100, 800);
  GHost.Show;
  GView := TNyxLCLRenderer.Create;
  {$ENDIF}
  GView.Render(GDocument, GDocument.Pages[0], GHost);
  Pump;
  GArrivalID := GView.Root.Find(NyxQualifiedID('first-trip', 'trip-dates')).Part('start').ID;
  GOtherID := GView.Root.Find(NyxQualifiedID('second-trip', 'trip-dates')).Part('start').ID;
  GView.Events.On(NyxControlEvents(GArrivalID, niRuntime), ntChange).Subscribe(TChangeObserver.Create(1));
  GView.Events.On(NyxControlEvents(GArrivalID, niRuntime), ntChange).Subscribe(TChangeObserver.Create(2));
  Check(InputValue(GArrivalID) = '2026-10-06', 'bound arrival is canonical');
  Check(InputValue(GOtherID) = '2026-11-03', 'second reusable instance has its own value');
  {$IFDEF PAS2JS}
  LInput := TJSHTMLInputElement(GView.InputFor(GArrivalID, niRuntime));
  Check(LInput.getAttribute('type') = 'date', 'browser uses the standard date control');
  Check((LInput.getAttribute('min') = '2026-01-01') and
    (LInput.getAttribute('max') = '2026-12-31'), 'browser projects typed inclusive bounds');
  {$ELSE}
  LField := Field(GArrivalID);
  LOther := Field(GOtherID);
  Check(LField <> LOther, 'reusable instances own different calendar fields');
  Check(LField.Editor = GView.InputFor(GArrivalID, niRuntime), 'input hooks use the actual grouped inner editor');
  {$ENDIF}
  GOrder := '';
  Edit(GArrivalID, '2026-10-08');
  Check(GView.State.GetValue(NyxTextState('arrival')) = '2026-10-08', 'actual edit updates runtime state');
  Check(GOrder = '12', 'accepted changes run in registration order once / ' + GOrder);
  Check(GSnapshot.HasValue and (GSnapshot.Value.AsText = '2026-10-08') and
    GSnapshot.HasTextEdit, 'callback retains canonical value and editing snapshot');
  GRetainedSnapshot := GSnapshot.Copy;
  Check(InputValue(GOtherID) = '2026-11-03', 'editing one instance leaves the other independent');
  GOrder := '';
  Edit(GArrivalID, '2027-01-01');
  Check((InputValue(GArrivalID) = '2026-10-08') and
    (GView.State.GetValue(NyxTextState('arrival')) = '2026-10-08'),
    'physical out-of-range proposal restores exact accepted state/control');
  Check((GView.LastBindingError <> '') and (GOrder = ''), 'refusal is diagnostic without successful callback');
  LRefused := False;
  try
    GView.State.SetValue(NyxTextState('arrival'), 'tomorrow');
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (GView.State.GetValue(NyxTextState('arrival')) = '2026-10-08'),
    'bound programmatic state admission is atomic too');
  Edit(GArrivalID, '');
  Check(GView.State.GetValue(NyxTextState('arrival')) = '', 'empty date remains empty');
  GView.State.SetValue(NyxTextState('arrival'), '2026-10-09');
  Check(InputValue(GArrivalID) = '2026-10-09', 'state change updates the same actual input');
  {$IFNDEF PAS2JS}
  LField.Editor.SetFocus;
  Edit(GArrivalID, '2026-0');
  Check((InputValue(GArrivalID) = '2026-0') and
    (GView.State.GetValue(NyxTextState('arrival')) = '2026-10-09'),
    'native partial date is retained without state admission');
  GView.Sync;
  Check(InputValue(GArrivalID) = '2026-0', 'unrelated synchronization preserves the draft');
  LField.OnEditingDone(LField);
  Check((InputValue(GArrivalID) = '2026-10-09') and (GView.LastBindingError <> ''),
    'commit rejects the malformed date without locale coercion');
  LField.Button.Click;
  Pump;
  Check((LField.Popup <> nil) and LField.Popup.Visible, 'actual native picker opens');
  Check((LField.CalendarControl.MinDate = EncodeDate(2026, 1, 1)) and
    (LField.CalendarControl.MaxDate = EncodeDate(2026, 12, 31)), 'native calendar bounds match domain');
  GView.Sync;
  Check(LField.Popup.Visible, 'unchanged domain sync retains the open picker');
  CaptureNative(GHost, '-fields');
  CaptureNative(LField.Popup, '-calendar');
  LField.CalendarControl.DateTime := EncodeDate(2026, 10, 10);
  CalendarKey(LField, VK_RETURN);
  Check(not LField.Popup.Visible and (InputValue(GArrivalID) = '2026-10-10'),
    'calendar acceptance edits through the existing value command');
  Check(Screen.ActiveControl = LField.Editor, 'calendar acceptance returns focus to the exact editor');
  LField.Button.Click;
  Pump;
  LField.CalendarControl.DateTime := EncodeDate(2026, 10, 11);
  CalendarKey(LField, VK_ESCAPE);
  Check((InputValue(GArrivalID) = '2026-10-10') and not LField.Popup.Visible,
    'Escape cancels without admitting highlighted date');
  Check(Screen.ActiveControl = LField.Editor, 'Escape returns focus to the exact editor');
  {$ENDIF}

  LRuntime := GView.Root.Find('trip-card');
  {$IFNDEF PAS2JS}
  LField.Button.Click;
  Pump;
  Check(LField.Popup.Visible, 'calendar starts a new independent choice context');
  {$ENDIF}
  LRuntime.Configure.ReadOnly(True).Done;
  GView.Sync;
  {$IFNDEF PAS2JS}
  Check(not LField.Popup.Visible, 'ancestor read-only ends an already open calendar');
  {$ENDIF}
  GOrder := '';
  Edit(GArrivalID, '2026-10-12');
  Check((GOrder = '') and (GView.State.GetValue(NyxTextState('arrival')) <>
    '2026-10-12'), 'inherited read-only refuses physical proposals');
  {$IFNDEF PAS2JS}
  LField.Button.Click;
  Check(not LField.Popup.Visible and not LField.Button.Enabled, 'read-only prevents calendar opening');
  {$ELSE}
  Check(TJSHTMLInputElement(GView.InputFor(GArrivalID, niRuntime)).readOnly,
    'browser read-only reaches its real input');
  {$ENDIF}
  LRuntime.Configure.ReadOnly(False).Enabled(False).Done;
  GView.Sync;
  Edit(GArrivalID, '2026-10-12');
  Check(GView.State.GetValue(NyxTextState('arrival')) <> '2026-10-12',
    'inherited disabled refuses proposals');
  LRuntime.Configure.Enabled(True).Done;
  GView.Sync;
  {$IFNDEF PAS2JS}
  LField.Button.Click;
  Pump;
  GView.Root.Find(GArrivalID).Contract.Value(
    NyxDateDomain.Range(NyxDate(2026, 1, 1), NyxDate(2026, 11, 30)));
  GView.Sync;
  Check(not LField.Popup.Visible, 'changed date bounds revoke the old choice context');
  LField.Button.Click;
  Pump;
  Check(LField.CalendarControl.MaxDate = EncodeDate(2026, 11, 30),
    'the next opening uses the changed admitted domain');
  LRuntime.Configure.Visible(False).Done;
  GView.Sync;
  Check(not LField.Popup.Visible, 'hidden ancestry retires the visible calendar');
  LRuntime.Configure.Visible(True).Done;
  GView.Sync;
  {$ENDIF}
  Check(TNyxCodec.Encode(GDocument) = GOriginal, 'physical editing never changes authored defaults/recipes');
  {$IFNDEF PAS2JS}
  LField.Button.Click;
  Pump;
  GRetire := True;
  LField.CalendarControl.DateTime := EncodeDate(2026, 10, 13);
  CalendarKey(LField, VK_RETURN);
  Check(GView.Root = nil, 'calendar callback safely retires its renderer and owned popup');
  Check(GSnapshot.Value.AsText = '2026-10-13', 'retained callback data outlives the native field');
  {$ELSE}
  Check(GRetainedSnapshot.Value.AsText = '2026-10-08', 'earlier event snapshot remains owned');
  {$ENDIF}
end;

procedure Retire;
begin
  GView.Free;
  GView := nil;
  GDocument.Free;
  GDocument := nil;
  {$IFNDEF PAS2JS}
  GHost.Free;
  GHost := nil;
  {$ENDIF}
end;

{$IFDEF PAS2JS}
procedure Finish;
begin

  if (window.location.search = '?capture=1') and
    (document.body.getAttribute('data-capture-observed') <> 'date-fields') then
  begin
    window.setTimeout(@Finish, 25);
    Exit;
  end;
  Retire;
  Check(GRetainedSnapshot.Value.AsText = '2026-10-08', 'owned snapshot survives browser retirement');
  Check(document.querySelector('[data-node=home]') = nil, 'browser view unmounts owned date inputs');
  document.body.setAttribute('data-date-checks', IntToStr(GChecks));
  document.body.setAttribute('data-date-controls', 'passed');
end;
{$ENDIF}

begin
  try
    Journey;
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-capture-checkpoint', 'date-fields');
    window.setTimeout(@Finish, 25);
    {$ELSE}
    Retire;
    WriteLn('PASS ', GChecks, ' actual native date checks');
    {$ENDIF}
  except
    on E: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-date-controls', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
      {$ELSE}
      WriteLn('FAIL after ', GChecks, ' checks / ', E.Message);
      WriteLn(BackTraceStrFunc(ExceptAddr));
      for GStackIndex := 0 to ExceptFrameCount - 1 do
      begin
        WriteLn(BackTraceStrFunc(ExceptFrames[GStackIndex]));
      end;
      Retire;
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
end.
