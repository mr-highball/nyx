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
program nyx_gesture_cdp_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, base64, nyx.text, nyx.data, nyx.test.browser.host;

type
  TMousePhase = (mpMove, mpDown, mpUp);
  { Gesture assertions remain in the Pascal fixture. This driver supplies real
    host input through the shared isolated browser owner. }
  TGestureDriver = class(TNyxBrowserHost)
  private
    FDragData: TNyxDataValue;
    procedure Center(const AID: TNyxText; out AX, AY: Double);
    procedure Mouse(APhase: TMousePhase; AX, AY: Double; APressed: Boolean);
    procedure WaitAttribute(const AName, AValue: TNyxText);
  protected
    procedure Notification(const AMethod: TNyxText; const AParams: TNyxDataValue); override;
  public
    procedure Run;
  end;

procedure TGestureDriver.Notification(const AMethod: TNyxText;
  const AParams: TNyxDataValue);
begin

  if AMethod = 'Input.dragIntercepted' then
  begin
    FDragData := AParams.Field('data');
  end;
end;

procedure TGestureDriver.Center(const AID: TNyxText; out AX, AY: Double);
var
  LBox: TNyxDataValue;
begin
  LBox := Command('DOM.getBoxModel', NyxObject([NyxField('nodeId',
    NyxData(Node('[data-runtime-id="' + AID + '"]')))])).Field('model').Field('content');
  AX := (LBox.Item(0).AsNumber + LBox.Item(4).AsNumber) / 2;
  AY := (LBox.Item(1).AsNumber + LBox.Item(5).AsNumber) / 2;
end;

procedure TGestureDriver.Mouse(APhase: TMousePhase; AX, AY: Double; APressed: Boolean);
const
  CNames: array[TMousePhase] of TNyxText = ('mouseMoved', 'mousePressed', 'mouseReleased');
var
  LButton: TNyxText;
  LButtons: Integer;
begin
  LButton := 'none';

  if (APhase <> mpMove) or APressed then
  begin
    LButton := 'left';
  end;
  LButtons := 0;

  if APressed then
  begin
    LButtons := 1;
  end;
  Command('Input.dispatchMouseEvent', NyxObject([
    NyxField('type', NyxData(CNames[APhase])), NyxField('x', NyxData(AX)),
    NyxField('y', NyxData(AY)), NyxField('button', NyxData(LButton)),
    NyxField('buttons', NyxData(LButtons)), NyxField('clickCount', NyxData(1))]));
  { Give the compositor an input frame between distinct physical actions. The
    result is still decided by callback state, never by elapsed time alone. }
  Sleep(50);
end;

procedure TGestureDriver.WaitAttribute(const AName, AValue: TNyxText);
var
  LStarted: QWord;
  LActual: TNyxText;
  LSnapshot: TNyxDataValue;
begin
  LStarted := GetTickCount64;
  repeat
    LActual := Attribute(AName);

    if Attribute('data-physical-gestures') = 'failed' then
    begin
      raise Exception.Create('Pascal physical fixture failed: ' +
        String(Attribute('data-physical-error')));
    end;

    if LActual = AValue then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      Save('failure.json', NyxObject([
        NyxField('capture', NyxData(Attribute('data-capture-count'))),
        NyxField('loss', NyxData(Attribute('data-loss-count'))),
        NyxField('outsideMoves', NyxData(Attribute('data-outside-count'))),
        NyxField('dragStart', NyxData(Attribute('data-drag-start-count'))),
        NyxField('hostPointer', NyxData(Attribute('data-host-pointer'))),
        NyxField('hostSubscribed', NyxData(Attribute('data-host-subscribed'))),
        NyxField('hostDefault', NyxData(Attribute('data-host-default'))),
        NyxField('hostGestureError', NyxData(Attribute('data-host-gesture-error'))),
        NyxField('pendingCapture', NyxData(Attribute('data-host-pending-capture'))),
        NyxField('callbackFailure', NyxData(Attribute('data-host-callback-failure'))),
        NyxField('callbackStatus', NyxData(Attribute('data-host-callback-status'))),
        NyxField('probeTrigger', NyxData(Attribute('data-probe-trigger')))]).ToJSON);
      LSnapshot := Command('Page.captureScreenshot', NyxObject([]));
      Save('failure.png', DecodeStringBase64(LSnapshot.Field('data').AsText));
      raise Exception.Create('Physical fixture did not reach ' + String(AName) +
        '=' + String(AValue) + '; observed ' + String(LActual));
    end;
    Sleep(10);
  until False;
end;

procedure TGestureDriver.Run;
var
  LX: Double;
  LY: Double;
  LTargetX: Double;
  LTargetY: Double;
  LStarted: QWord;
  LPhase: Integer;
  LDragKind: TNyxText;
  LSnapshot: TNyxDataValue;
  LHit: TNyxDataValue;
begin
  WaitAttribute('data-physical-gestures', 'ready');
  Center('capture-button', LX, LY);
  LHit := Command('DOM.getNodeForLocation', NyxObject([
    NyxField('x', NyxData(Round(LX))), NyxField('y', NyxData(Round(LY)))]));
  Save('input-target.json', Command('DOM.describeNode', NyxObject([
    NyxField('backendNodeId', LHit.Field('backendNodeId'))])).ToJSON);
  Mouse(mpMove, LX, LY, False);
  Mouse(mpDown, LX, LY, True);
  Mouse(mpMove, 900, LY, True);
  Mouse(mpUp, 900, LY, False);
  WaitAttribute('data-loss-count', '1');
  WriteLn('PASS physical capture, outside movement and release');
  { Real touch messages exercise implicit capture termination and pointercancel;
    no synthetic PointerEvent can establish these host interaction semantics. }
  Command('Input.dispatchTouchEvent', NyxObject([
    NyxField('type', NyxData('touchStart')), NyxField('touchPoints', NyxArray([
      NyxObject([NyxField('x', NyxData(LX)), NyxField('y', NyxData(LY)),
        NyxField('id', NyxData(501)), NyxField('force', NyxData(0.5))])]))]));
  Command('Input.dispatchTouchEvent', NyxObject([
    NyxField('type', NyxData('touchMove')), NyxField('touchPoints', NyxArray([
      NyxObject([NyxField('x', NyxData(400)), NyxField('y', NyxData(LY)),
        NyxField('id', NyxData(501)), NyxField('force', NyxData(0.5))])]))]));
  Command('Input.dispatchTouchEvent', NyxObject([
    NyxField('type', NyxData('touchCancel')), NyxField('touchPoints', NyxArray([]))]));
  WaitAttribute('data-touch-gestures', 'passed');
  WriteLn('PASS physical touch capture, outside movement, cancellation and implicit loss');
  Command('Input.setInterceptDrags', NyxObject([NyxField('enabled', NyxData(True))]));
  Center('transfer-button', LX, LY);
  Center('drop-card', LTargetX, LTargetY);
  Mouse(mpMove, LX, LY, False);
  Mouse(mpDown, LX, LY, True);
  Mouse(mpMove, LX + 20, LY + 4, True);
  Mouse(mpMove, LX + 40, LY + 8, True);
  LStarted := GetTickCount64;
  while not FDragData.Defined do
  begin
    Pump;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Real browser drag did not produce intercepted owned data');
    end;
  end;
  for LPhase := 0 to 2 do
  begin
    case LPhase of
      0: LDragKind := 'dragEnter';
      1: LDragKind := 'dragOver';
    else
      LDragKind := 'drop';
    end;
    Command('Input.dispatchDragEvent', NyxObject([
      NyxField('type', NyxData(LDragKind)), NyxField('x', NyxData(LTargetX)),
      NyxField('y', NyxData(LTargetY)), NyxField('data', FDragData)]));
  end;
  Mouse(mpUp, LTargetX, LTargetY, False);
  WaitAttribute('data-physical-gestures', 'passed');
  Save('result.json', NyxObject([NyxField('capture', NyxData(Attribute('data-capture-count'))),
    NyxField('loss', NyxData(Attribute('data-loss-count'))),
    NyxField('outsideMoves', NyxData(Attribute('data-outside-count'))),
    NyxField('touchMoves', NyxData(Attribute('data-touch-move-count'))),
    NyxField('canceled', NyxData(Attribute('data-cancel-count'))),
    NyxField('dragStart', NyxData(Attribute('data-drag-start-count'))),
    NyxField('hover', NyxData(Attribute('data-hover-count'))),
    NyxField('drop', NyxData(Attribute('data-drop-count'))),
    NyxField('dragEnd', NyxData(Attribute('data-drag-end-count')))]).ToJSON);
  LSnapshot := Command('Page.captureScreenshot', NyxObject([NyxField('format', NyxData('png'))]));
  Save('capture.png', DecodeStringBase64(LSnapshot.Field('data').AsText));
  WriteLn('PASS physical typed drag offer, protected hover, owned drop and final Copy');
end;

var
  LDriver: TGestureDriver;
begin
  LDriver := nil;
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Supply loopback fixture URL and artifact directory');
    end;
    LDriver := TGestureDriver.Create(ParamStr(1), ParamStr(2));
    try
      LDriver.Run;
    finally
      FreeAndNil(LDriver);
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
