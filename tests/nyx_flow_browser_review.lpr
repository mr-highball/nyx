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
program nyx_flow_browser_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, base64, nyx.text, nyx.data, nyx.test.browser.host;

type
  TMousePhase = (mpMove, mpDown, mpUp);
  { One owned Chromium host supplies actual drag input. The intercepted payload
    comes from Studio's real source callback; this driver never fabricates leases
    or authors a design. Fixture assertions remain in Pascal. }
  TFlowReview = class(TNyxBrowserHost)
  private
    FDragData: TNyxDataValue;
    FX, FY: Double;
    procedure Mouse(APhase: TMousePhase; AX, AY: Double; APressed: Boolean);
    function Box(const AID: TNyxText): TNyxDataValue;
    procedure BeginDrag(ARow: Boolean);
    procedure Drag(const AKind: TNyxText);
    procedure Capture(const AName: TNyxText);
  protected
    procedure Notification(const AMethod: TNyxText; const AParams: TNyxDataValue); override;
  public
    procedure Run(ACompact: Boolean);
  end;

procedure TFlowReview.Notification(const AMethod: TNyxText; const AParams: TNyxDataValue);
begin

  if AMethod = 'Input.dragIntercepted' then
  begin
    FDragData := AParams.Field('data');
  end;
end;

procedure TFlowReview.Mouse(APhase: TMousePhase; AX, AY: Double; APressed: Boolean);
const
  CNames: array[TMousePhase] of TNyxText = ('mouseMoved', 'mousePressed', 'mouseReleased');
var
  LButton: TNyxText;
  LButtons: Integer;
begin
  LButton := 'none';
  LButtons := 0;

  if (APhase <> mpMove) or APressed then
  begin
    LButton := 'left';
  end;

  if APressed then
  begin
    LButtons := 1;
  end;
  Command('Input.dispatchMouseEvent', NyxObject([
    NyxField('type', NyxData(CNames[APhase])), NyxField('x', NyxData(AX)),
    NyxField('y', NyxData(AY)), NyxField('button', NyxData(LButton)),
    NyxField('buttons', NyxData(LButtons)), NyxField('clickCount', NyxData(1))]));
  Sleep(50);
end;

function TFlowReview.Box(const AID: TNyxText): TNyxDataValue;
var
  LAttempt: Integer;
begin
  { Observing Studio can retire a chrome node between these two protocol reads
    while admitting paired Undo or selection. Reacquire bounded geometry before
    physical input; never retry a drag offer, drop or accepted mutation. }
  for LAttempt := 0 to 5 do
  begin
    try
      Result := Command('DOM.getBoxModel', NyxObject([NyxField('nodeId',
        NyxData(Node('[data-node="' + AID + '"]')))])).Field('model').Field('border');
      Exit;
    except
      on LException: Exception do
      begin

        if (LAttempt = 5) or
          ((Pos('Could not find node with given id', LException.Message) = 0) and
          (Pos('Physical fixture lacks semantic DOM identity', LException.Message) = 0)) then
        begin
          raise;
        end;
        Sleep(50);
      end;
    end;
  end;
end;

procedure TFlowReview.BeginDrag(ARow: Boolean);
var
  LBox: TNyxDataValue;
  LX, LY: Double;
  LStarted: QWord;
begin
  FDragData := Default(TNyxDataValue);
  if ARow then
  begin
    { Bring the exact insertion edge into the clipped canvas. Scrolling a child
      label alone can leave its parent's padding outside a narrow viewport. }
    Command('DOM.scrollIntoViewIfNeeded', NyxObject([NyxField('nodeId',
      NyxData(Node('[data-node="left-layout"]'))), NyxField('rect', NyxObject([
        NyxField('x', NyxData(0)), NyxField('y', NyxData(12)),
        NyxField('width', NyxData(8)), NyxField('height', NyxData(28))]))]));
    LBox := Box('left-layout');
    FX := LBox.Item(0).AsNumber + 4;
    FY := LBox.Item(1).AsNumber + 26;
  end
  else
  begin
    Command('DOM.scrollIntoViewIfNeeded', NyxObject([NyxField('nodeId',
      NyxData(Node('[data-node="reference-button"]')))]));
    LBox := Box('reference-button');
    FX := (LBox.Item(0).AsNumber + LBox.Item(4).AsNumber) / 2;
    FY := LBox.Item(1).AsNumber + 2;
  end;
  LBox := Box('studio-drag-move');
  LX := (LBox.Item(0).AsNumber + LBox.Item(4).AsNumber) / 2;
  LY := (LBox.Item(1).AsNumber + LBox.Item(5).AsNumber) / 2;
  Save('input-coordinates.json', NyxObject([
    NyxField('sourceX', NyxData(LX)), NyxField('sourceY', NyxData(LY)),
    NyxField('targetX', NyxData(FX)), NyxField('targetY', NyxData(FY))]).ToJSON);
  Mouse(mpMove, LX, LY, False);
  Mouse(mpDown, LX, LY, True);
  Mouse(mpMove, LX + 20, LY + 4, True);
  Mouse(mpMove, LX + 40, LY + 8, True);
  LStarted := GetTickCount64;
  while not FDragData.Defined do
  begin
    Pump;

    if GetTickCount64 - LStarted > 10000 then
    begin
      Capture('source-failure.png');
      raise Exception.Create('Actual Studio drag source did not offer owned data');
    end;
  end;
  Drag('dragEnter');
  Drag('dragOver');
end;

procedure TFlowReview.Drag(const AKind: TNyxText);
begin
  Command('Input.dispatchDragEvent', NyxObject([
    NyxField('type', NyxData(AKind)), NyxField('x', NyxData(FX)),
    NyxField('y', NyxData(FY)), NyxField('data', FDragData)]));
end;

procedure TFlowReview.Capture(const AName: TNyxText);
begin
  Save(AName, DecodeStringBase64(Command('Page.captureScreenshot',
    NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));
end;

procedure TFlowReview.Run(ACompact: Boolean);
var
  LStarted: QWord;
  LPhase: Integer;
  LInput, LMarker: TNyxText;
begin

  if ACompact then
  begin
    Command('Emulation.setDeviceMetricsOverride', NyxObject([
      NyxField('width', NyxData(390)), NyxField('height', NyxData(900)),
      NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(False))]));
  end;
  Command('Input.setInterceptDrags', NyxObject([NyxField('enabled', NyxData(True))]));
  LPhase := 0;
  LStarted := GetTickCount64;
  repeat
    Pump;
    LMarker := Attribute('data-nyx-flow');
    LInput := Attribute('data-nyx-flow-input');

    if LMarker = 'failed' then
    begin
      Capture('failure.png');
      raise Exception.Create(Attribute('data-nyx-flow-error'));
    end;
    case LPhase of
      0:

        if LInput = 'ready' then
        begin
          BeginDrag(False);
          LPhase := 1;
        end;
      1:

        if LInput = 'release' then
        begin
          Capture('insertion.png');
          Drag('drop');
          Mouse(mpUp, FX, FY, False);
          LPhase := 2;
        end;
      2:

        if LInput = 'cancel-ready' then
        begin
          BeginDrag(False);
          LPhase := 3;
        end;
      3:

        if LInput = 'cancel' then
        begin
          Drag('dragCancel');
          Mouse(mpUp, FX, FY, False);
          Command('DOM.setAttributeValue', NyxObject([
            NyxField('nodeId', NyxData(Node('body'))),
            NyxField('name', NyxData('data-nyx-flow-input')),
            NyxField('value', NyxData('cancel-ended'))]));
          LPhase := 4;
        end;
      4:

        if LInput = 'row-ready' then
        begin
          BeginDrag(True);
          LPhase := 5;
        end;
      5:

        if LInput = 'row-release' then
        begin
          Capture('row-insertion.png');
          Drag('drop');
          Mouse(mpUp, FX, FY, False);
          LPhase := 6;
        end;
    end;

    if LMarker = 'passed' then
    begin

      if LPhase <> 6 then
      begin
        raise Exception.Create('Flow fixture passed before completing actual host input');
      end;
      Save('result.json', NyxObject([
        NyxField('checks', NyxData(StrToInt(Attribute('data-nyx-flow-checks')))),
        NyxField('actualDragJourneys', NyxData(3)),
        NyxField('compact', NyxData(ACompact))]).ToJSON);
      Capture('capture.png');
      WriteLn('PASS ', Attribute('data-nyx-flow-checks'), ' actual browser flow Studio checks');
      Exit;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      Capture('timeout.png');
      raise Exception.Create('Actual flow journey timed out / phase ' + IntToStr(LPhase) +
        ' / input ' + LInput);
    end;
    Sleep(10);
  until False;
end;

var
  LReview: TFlowReview;
begin
  LReview := nil;
  try

    if (ParamCount < 2) or (ParamCount > 3) then
    begin
      raise Exception.Create('Supply explicit loopback semantic workspace URL and capture directory');
    end;

    if (ParamCount = 3) and (ParamStr(3) <> 'compact') then
    begin
      raise Exception.Create('Unknown flow review presentation');
    end;
    LReview := TFlowReview.Create(ParamStr(1), ParamStr(2));
    LReview.Run(ParamCount = 3);
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      Flush(Output);

      if LReview <> nil then
      begin
        try
          LReview.Capture('unexpected-failure.png');
        except
          { A broken host must not replace the original input failure. }
        end;
      end;
      ExitCode := 1;
    end;
  end;
  LReview.Free;
end.
