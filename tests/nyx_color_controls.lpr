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
program nyx_color_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,
  {$else}Interfaces, Classes, Forms, Controls, StdCtrls, Graphics,
    nyx.colors.lcl, nyx.render.lcl,{$endif}
  SysUtils, nyx.text, nyx.colors, nyx.types, nyx.model, nyx.contract,
  nyx.state, nyx.behavior, nyx.events, nyx.scheduler, nyx.codec, nyx.codegen,
  nyx.test.colors, nyx.generated.colors;

type
  { Ordered callbacks copy their event data; the second can retire its own view
    to exercise producer teardown without borrowed popup access after return. }
  TColorObserver = class(TNyxEventCallback)
  private
    FMarker: Integer;
  public
    constructor Create(AMarker: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}
  TColorInputEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TColorEditAccess = class(TCustomEdit)
  public
    procedure Complete;
  end;
  {$endif}

var
  GDoc: TNyxDocument;
  GChecks: Integer;
  GOrder: TNyxText;
  GSnapshot: TNyxEventInfo;
  GRetire: Boolean;
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
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

constructor TColorObserver.Create(AMarker: Integer);
begin
  inherited Create;
  FMarker := AMarker;
end;

procedure TColorObserver.Invoke(const AEvent: TNyxEventInfo;
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

procedure Draft(const AID, AText: TNyxText);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
{$endif}
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GView.InputFor(AID, niRuntime)).value := AText;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  GView.InputFor(AID, niRuntime).dispatchEvent(TColorInputEvent.new('input', LOptions));
  {$else}
  TCustomEdit(GView.InputFor(AID, niRuntime)).Text := AText;
  Pump;
  {$endif}
end;

procedure Complete(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
{$endif}
begin
  {$ifdef PAS2JS}
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  GView.InputFor(AID, niRuntime).dispatchEvent(TColorInputEvent.new('change', LOptions));
  {$else}
  TColorEditAccess(GView.InputFor(AID, niRuntime)).Complete;
  Pump;
  {$endif}
end;

{$ifndef PAS2JS}
procedure TColorEditAccess.Complete;
begin
  EditingDone;
end;

function Field(const AID: TNyxText): TNyxLCLColorField;
begin
  Result := TNyxLCLColorField(GView.InputFor(AID, niRuntime).Parent);
end;

procedure OpenPicker(AField: TNyxLCLColorField);
begin
  AField.Button.Click;
  Pump;
  Check((AField.Popup <> nil) and AField.Popup.Visible, 'Owned native color picker opens');
  Check((AField.AcceptButton.Top + AField.AcceptButton.Height <= AField.Popup.ClientHeight) and
    (AField.AcceptButton.Left + AField.AcceptButton.Width <= AField.Popup.ClientWidth),
    'Realized native client includes acceptance actions');
end;

procedure Pick(AField: TNyxLCLColorField; ARed, AGreen, ABlue: Integer);
begin
  AField.RedSlider.Position := ARed;
  AField.GreenSlider.Position := AGreen;
  AField.BlueSlider.Position := ABlue;
  AField.AcceptButton.Click;
  Pump;
end;
{$endif}

procedure Run;
var
  LExpected: TNyxDocument;
  LOriginal: TNyxText;
  {$ifndef PAS2JS}
  LAccent: TNyxLCLColorField;
  LOptional: TNyxLCLColorField;
  {$else}
  LPicker: TJSHTMLInputElement;
  LOptions: TJSObject;
  {$endif}
begin
  LExpected := CreateNyxColorFixture;
  try
    GDoc := nyx.generated.colors.BuildNyxDocument;
    Check(TNyxCodec.Encode(GDoc) = TNyxCodec.Encode(LExpected),
      'Exact compiled RGB source reconstructs its accepted document');
    Check(TNyxCodegen.Generate(GDoc) = TNyxCodegen.Generate(LExpected),
      'Compiled RGB source is deterministic');
  finally
    LExpected.Free;
  end;
  LOriginal := TNyxCodec.Encode(GDoc);
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GView := TNyxBrowserRenderer.Create;
  {$else}
  Application.Initialize;
  GHost := TForm.CreateNew(nil);
  GHost.Caption := 'Color workshop';
  GHost.SetBounds(20, 20, 540, 400);
  GHost.Show;
  GView := TNyxLCLRenderer.Create;
  {$endif}
  GView.Render(GDoc, GDoc.Pages[0], GHost);
  Pump;
  GView.Events.On(NyxControlEvents('accent-color', niRuntime), ntChange)
    .Subscribe(TColorObserver.Create(1));
  GView.Events.On(NyxControlEvents('accent-color', niRuntime), ntChange)
    .Subscribe(TColorObserver.Create(2));
  Check(InputValue('accent-color') = '#7357e8', 'Exact bound RGB reaches real target editor');
  Check(InputValue('optional-color') = '', 'Empty target field stays distinct from black');
  Check(InputValue('imported-color') = '#AbCdEf', 'Imported spelling survives actual projection');
  Draft('accent-color', '#');
  Check((InputValue('accent-color') = '#') and
    (GView.State.GetValue(NyxTextState('accent')) = '#7357e8') and (GOrder = ''),
    'Incomplete hex remains a physical draft without callbacks/store writes');
  Draft('accent-color', '#123456');
  Complete('accent-color');
  Check((GView.State.GetValue(NyxTextState('accent')) = '#123456') and (GOrder = '12'),
    'Actual completion publishes one value and ordered registrations');
  Check(GSnapshot.HasValue and (GSnapshot.Value.AsText = '#123456'),
    'Event owns exact admitted color text');
  GOrder := '';
  Draft('accent-color', 'blue');
  Complete('accent-color');
  Check((InputValue('accent-color') = '#123456') and
    (GView.State.GetValue(NyxTextState('accent')) = '#123456') and (GOrder = '') and
    (GView.LastBindingError <> ''), 'Invalid completed color restores accepted state without callbacks');

  {$ifndef PAS2JS}
  LAccent := Field('accent-color');
  LOptional := Field('optional-color');
  Check((LAccent.Editor = GView.InputFor('accent-color', niRuntime)) and
    (LAccent.Width > LAccent.Button.Width) and (LAccent.Editor.Width > 0),
    'Grouped native field retains inner input and usable picker geometry');
  OpenPicker(LOptional);
  Check(InputValue('optional-color') = '', 'Opening optional picker never writes suggested black');
  LOptional.RedSlider.Position := 40;
  LOptional.CancelButton.Click;
  Pump;
  Check(not LOptional.Popup.Visible and (InputValue('optional-color') = ''),
    'Cancel keeps exact absent value and closes proposal');
  OpenPicker(LOptional);
  Pick(LOptional, 0, 0, 0);
  Check(InputValue('optional-color') = '#000000', 'Explicitly selected black is defined');
  OpenPicker(LOptional);
  LOptional.ClearButton.Click;
  Pump;
  Check(InputValue('optional-color') = '', 'Clear publishes explicit absence');
  OpenPicker(LAccent);
  Pick(LAccent, 1, 2, 3);
  Check(LAccent.Popup.Visible and (LAccent.Status.Caption <> '') and (GOrder = '') and
    (InputValue('accent-color') = '#123456'), 'Constrained picker refusal retains proposal and accepted field');
  Pick(LAccent, 115, 87, 232);
  Check(not LAccent.Popup.Visible and (InputValue('accent-color') = '#7357e8') and
    (GOrder = '12'), 'Accepted picker value follows ordinary bound callbacks');
  GOrder := '';
  OpenPicker(LAccent);
  GView.Root.Find('accent-color').Configure.ReadOnly(True).Done;
  GView.Sync;
  Check(not LAccent.Popup.Visible and not LAccent.Button.Enabled,
    'Read-only policy closes native picker and disables activation');
  GView.Root.Find('accent-color').Configure.ReadOnly(False).Done;
  GView.Sync;
  OpenPicker(LAccent);
  GView.Root.Find('accent-color').Contract.Value(NyxRGBDomain.Choices([
    NyxRGB(115, 87, 232), NyxRGB(18, 52, 86)]));
  GView.Sync;
  Check(not LAccent.Popup.Visible and (InputValue('accent-color') = '#7357e8'),
    'Changed domain retires old native proposal without changing accepted input');
  OpenPicker(LAccent);
  GRetire := True;
  Pick(LAccent, 18, 52, 86);
  Check((GView.Root = nil) and (GOrder = '12') and
    (GSnapshot.Value.AsText = '#123456'), 'Picker callbacks safely retire renderer with owned event snapshot');
  {$else}
  LPicker := TJSHTMLInputElement(GView.ElementFor('accent-color').querySelector('.nyx-color-picker'));
  Check((LPicker <> nil) and (LPicker._type = 'color'),
    'Browser retains a real native color chooser beside the exact editor');
  LPicker.click;
  LPicker.value := '#7357e8';
  GView.Sync;
  Check((LPicker.value = '#7357e8') and (InputValue('accent-color') = '#123456'),
    'Unchanged sync preserves an uncommitted chooser proposal and accepted editor');
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LPicker.dispatchEvent(TColorInputEvent.new('change', LOptions));
  Check((InputValue('accent-color') = '#7357e8') and (GOrder = '12'),
    'Chooser completion enters the ordinary bound dispatch');
  GOrder := '';
  LPicker.click;
  GView.Root.Find('accent-color').Configure.ReadOnly(True).Done;
  GView.Sync;
  LPicker.value := '#123456';
  LPicker.dispatchEvent(TColorInputEvent.new('change', LOptions));
  Check(LPicker.disabled and (InputValue('accent-color') = '#7357e8') and (GOrder = ''),
    'Revoked browser chooser cannot publish a stale proposal');
  GView.Root.Find('accent-color').Configure.ReadOnly(False).Done;
  GView.Sync;
  LPicker.click;
  GRetire := True;
  LPicker.value := '#123456';
  LPicker.dispatchEvent(TColorInputEvent.new('change', LOptions));
  Check((GView.Root = nil) and (GOrder = '12') and
    (GSnapshot.Value.AsText = '#123456'), 'Chooser callback retirement retains copied event');
  LPicker.dispatchEvent(TColorInputEvent.new('change', LOptions));
  Check(GOrder = '12', 'Retired DOM handle has no live producer');
  {$endif}
  Check(TNyxCodec.Encode(GDoc) = LOriginal, 'Runtime editing does not mutate authored design/defaults');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-color-controls', 'passed');
    document.body.setAttribute('data-color-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' exact compiled RGB and actual Win32 control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-color-controls', 'failed');
      document.body.setAttribute('data-color-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  GView.Free;
  GDoc.Free;
  {$ifndef PAS2JS}
  GHost.Free;
  {$endif}
end.
