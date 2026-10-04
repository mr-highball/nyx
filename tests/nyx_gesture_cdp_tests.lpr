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
  Classes, SysUtils, Process, fphttpclient, fpwebsocket, fpwebsocketclient,
  base64, nyx.text, nyx.data;

type
  TMousePhase = (mpMove, mpDown, mpUp);
  { One bounded local protocol connection. It never executes JavaScript or
    evaluates application source: DOM queries and physical Input commands only. }
  TGestureDriver = class
  private
    FBrowser: TProcess;
    FSocket: TWebsocketClient;
    FDirectory: String;
    FBrowserLog: RawByteString;
    FSequence: Integer;
    FExpected: Integer;
    FReply: TNyxDataValue;
    FDragData: TNyxDataValue;
    FLoaded: Boolean;
    procedure Received(ASender: TObject; const AMessage: TWSMessage);
    procedure DrainBrowser;
    function Command(const AMethod: TNyxText; const AParams: TNyxDataValue): TNyxDataValue;
    function Node(const ASelector: TNyxText): Integer;
    function Attribute(const AName: TNyxText): TNyxText;
    procedure Center(const AID: TNyxText; out AX, AY: Double);
    procedure Mouse(APhase: TMousePhase; AX, AY: Double; APressed: Boolean);
    procedure WaitAttribute(const AName, AValue: TNyxText);
    procedure Save(const AName: String; const ABytes: RawByteString);
  public
    constructor Create(const AURL, ADirectory: String);
    destructor Destroy; override;
    procedure Run;
  end;

function HasField(const AValue: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if not AValue.Defined or (AValue.Kind <> ndObject) then
  begin
    Exit;
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if AValue.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

procedure TGestureDriver.Save(const AName: String; const ABytes: RawByteString);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(FDirectory + AName, fmCreate);
  try

    if ABytes <> '' then
    begin
      LFile.WriteBuffer(ABytes[1], Length(ABytes));
    end;
  finally
    LFile.Free;
  end;
end;

procedure TGestureDriver.DrainBrowser;
var
  LBuffer: array[0..8191] of Byte;
  LChunk: RawByteString;
  LRead: Integer;
begin
  while FBrowser.Output.NumBytesAvailable > 0 do
  begin
    LRead := FBrowser.Output.Read(LBuffer, SizeOf(LBuffer));
    SetLength(LChunk, LRead);

    if LRead > 0 then
    begin
      Move(LBuffer[0], LChunk[1], LRead);
    end;
    FBrowserLog := FBrowserLog + LChunk;

    if Length(FBrowserLog) > 1024 * 1024 then
    begin
      raise Exception.Create('Browser diagnostics exceed the bounded test budget');
    end;
  end;
end;

procedure TGestureDriver.Received(ASender: TObject; const AMessage: TWSMessage);
var
  LPacket: TNyxDataValue;
  LText: TNyxText;
begin
  LText := AMessage.AsUTF8String;

  if Length(LText) > 4 * 1024 * 1024 then
  begin
    raise Exception.Create('Protocol response exceeds the bounded test budget');
  end;
  LPacket := TNyxDataValue.ParseJSON(LText);

  if HasField(LPacket, 'id') then
  begin

    if LPacket.Field('id').AsInteger = FExpected then
    begin
      FReply := LPacket;
    end;
  end
  else if HasField(LPacket, 'method') and
    (LPacket.Field('method').AsText = 'Input.dragIntercepted') then
  begin
    FDragData := LPacket.Field('params').Field('data');
  end
  else if HasField(LPacket, 'method') and
    (LPacket.Field('method').AsText = 'Page.loadEventFired') then
  begin
    FLoaded := True;
  end
  else if HasField(LPacket, 'method') and
    (LPacket.Field('method').AsText = 'Runtime.exceptionThrown') then
  begin
    Save('runtime-error.json', LPacket.ToJSON);
  end;
end;

function TGestureDriver.Command(const AMethod: TNyxText;
  const AParams: TNyxDataValue): TNyxDataValue;
var
  LStarted: QWord;
begin
  Inc(FSequence);
  FExpected := FSequence;
  FReply := Default(TNyxDataValue);
  FSocket.SendMessage(NyxObject([NyxField('id', NyxData(FExpected)),
    NyxField('method', NyxData(AMethod)), NyxField('params', AParams)]).ToJSON);
  LStarted := GetTickCount64;
  repeat
    FSocket.CheckIncoming;
    DrainBrowser;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Local protocol command timed out: ' + String(AMethod));
    end;
  until FReply.Defined;

  if HasField(FReply, 'error') then
  begin
    raise Exception.Create('Local protocol refused ' + String(AMethod) + ': ' +
      String(FReply.Field('error').ToJSON));
  end;
  Result := FReply.Field('result');
end;

constructor TGestureDriver.Create(const AURL, ADirectory: String);
var
  LProfile: String;
  LActiveFile: String;
  LPort: Integer;
  LLines: TStringList;
  LHTTP: TFPHTTPClient;
  LTargets: TNyxDataValue;
  LTarget: TNyxDataValue;
  LWebSocket: TNyxText;
  LPrefix: TNyxText;
  LStarted: QWord;
  LIndex: Integer;
  LGuid: TGuid;
  LActiveStream: TFileStream;
  LReadActive: Boolean;
begin
  inherited Create;

  if Pos('http://127.0.0.1:', AURL) <> 1 then
  begin
    raise Exception.Create('Physical fixture must use an explicit loopback HTTP URL');
  end;
  FDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory));
  ForceDirectories(FDirectory);
  CreateGUID(LGuid);
  LProfile := FDirectory + 'profile-' + GUIDToString(LGuid);
  ForceDirectories(LProfile);
  LActiveFile := IncludeTrailingPathDelimiter(LProfile) + 'DevToolsActivePort';
  FBrowser := TProcess.Create(nil);
  FBrowser.Executable := GetEnvironmentVariable('ProgramFiles(x86)') +
    '\Microsoft\Edge\Application\msedge.exe';
  FBrowser.Options := [poUsePipes, poStderrToOutput, poNoConsole];
  FBrowser.Parameters.Add('--headless=new');
  FBrowser.Parameters.Add('--disable-gpu');
  FBrowser.Parameters.Add('--no-first-run');
  FBrowser.Parameters.Add('--no-default-browser-check');
  FBrowser.Parameters.Add('--disable-extensions');
  FBrowser.Parameters.Add('--remote-debugging-address=127.0.0.1');
  FBrowser.Parameters.Add('--remote-debugging-port=0');
  FBrowser.Parameters.Add('--remote-allow-origins=127.0.0.1');
  FBrowser.Parameters.Add('--user-data-dir=' + LProfile);
  FBrowser.Parameters.Add('--window-size=1100,1000');
  FBrowser.Parameters.Add('about:blank');
  FBrowser.Execute;
  LStarted := GetTickCount64;
  while not FileExists(LActiveFile) do
  begin
    DrainBrowser;

    if not FBrowser.Running or (GetTickCount64 - LStarted > 20000) then
    begin
      raise Exception.Create('Owned headless browser did not publish a local endpoint');
    end;
    Sleep(10);
  end;
  LLines := TStringList.Create;
  try
    LReadActive := False;
    repeat
      try
        LActiveStream := TFileStream.Create(LActiveFile, fmOpenRead or fmShareDenyNone);
        try
          LLines.LoadFromStream(LActiveStream);
          LReadActive := (LLines.Count > 0) and (LLines[0] <> '');
        finally
          LActiveStream.Free;
        end;
      except
        on EFOpenError do
        begin
          { The owned browser publishes this file while startup is still in
            progress. Retry a transient sharing conflict within the same bound. }
        end;
      end;

      if not LReadActive then
      begin

        if GetTickCount64 - LStarted > 20000 then
        begin
          raise Exception.Create('Owned debugger endpoint file remained unavailable');
        end;
        Sleep(10);
      end;
    until LReadActive;
    LPort := StrToInt(LLines[0]);
  finally
    LLines.Free;
  end;
  LHTTP := TFPHTTPClient.Create(nil);
  try
    LHTTP.ConnectTimeout := 3000;
    LHTTP.IOTimeout := 3000;
    LTargets := TNyxDataValue.ParseJSON(LHTTP.Get('http://127.0.0.1:' +
      IntToStr(LPort) + '/json/list'));
  finally
    LHTTP.Free;
  end;
  LWebSocket := '';
  for LIndex := 0 to LTargets.Count - 1 do
  begin
    LTarget := LTargets.Item(LIndex);

    if LTarget.Field('type').AsText = 'page' then
    begin
      LWebSocket := LTarget.Field('webSocketDebuggerUrl').AsText;
      Break;
    end;
  end;
  LPrefix := 'ws://127.0.0.1:' + TNyxText(IntToStr(LPort));

  if Pos(LPrefix + '/', LWebSocket) <> 1 then
  begin
    raise Exception.Create('Browser supplied an unexpected debugger authority');
  end;
  FSocket := TWebsocketClient.Create(nil);
  FSocket.HostName := '127.0.0.1';
  FSocket.Port := LPort;
  FSocket.Resource := Copy(LWebSocket, Length(LPrefix) + 1, Length(LWebSocket));
  FSocket.ConnectTimeout := 3000;
  FSocket.CheckTimeOut := 50;
  FSocket.OnMessageReceived := Received;
  FSocket.Active := True;

  if not FSocket.Active or (FSocket.Connection = nil) then
  begin
    raise Exception.Create('Owned loopback debugger refused its WebSocket handshake');
  end;
  { Navigate only after connecting and enabling load notifications. Initial
    browser navigation otherwise races DOM IDs and silently targets an old page. }
  Command('Page.enable', NyxObject([]));
  Command('DOM.enable', NyxObject([]));
  Command('Runtime.enable', NyxObject([]));
  FLoaded := False;
  Command('Page.navigate', NyxObject([NyxField('url', NyxData(TNyxText(AURL)))]));
  LStarted := GetTickCount64;
  while not FLoaded do
  begin
    FSocket.CheckIncoming;
    DrainBrowser;

    if GetTickCount64 - LStarted > 20000 then
    begin
      raise Exception.Create('Owned physical fixture did not complete navigation');
    end;
  end;
  Command('Page.bringToFront', NyxObject([]));
end;

destructor TGestureDriver.Destroy;
var
  LStarted: QWord;
begin
  { Close this harness's unique headless profile through its owned debugger.
    Keep a bounded process fallback for failed startup or a broken connection. }

  if (FSocket <> nil) and FSocket.Active then
  begin
    try
      FSocket.SendMessage(NyxObject([NyxField('id', NyxData(FSequence + 1)),
        NyxField('method', NyxData('Browser.close'))]).ToJSON);
    except
      { Cleanup must preserve the original semantic test failure. }
    end;
  end;

  if FBrowser <> nil then
  begin

    if FBrowser.Running then
    begin
      LStarted := GetTickCount64;
      repeat
        DrainBrowser;

        if FBrowser.Running then
        begin
          Sleep(10);
        end;
      until not FBrowser.Running or (GetTickCount64 - LStarted > 3000);

      if FBrowser.Running then
      begin
        FBrowser.Terminate(0);
        FBrowser.WaitOnExit;
      end;
    end;

    if FBrowser.Output <> nil then
    begin
      DrainBrowser;
    end;
    Save('browser.log', FBrowserLog);
  end;
  FBrowser.Free;
  FSocket.Free;
  inherited Destroy;
end;

function TGestureDriver.Node(const ASelector: TNyxText): Integer;
var
  LRoot: TNyxDataValue;
begin
  LRoot := Command('DOM.getDocument', NyxObject([])).Field('root');
  Result := Command('DOM.querySelector', NyxObject([
    NyxField('nodeId', LRoot.Field('nodeId')),
    NyxField('selector', NyxData(ASelector))])).Field('nodeId').AsInteger;

  if Result = 0 then
  begin
    raise Exception.Create('Physical fixture lacks semantic DOM identity: ' + String(ASelector));
  end;
end;

function TGestureDriver.Attribute(const AName: TNyxText): TNyxText;
var
  LAttributes: TNyxDataValue;
  LIndex: Integer;
begin
  LAttributes := Command('DOM.getAttributes', NyxObject([
    NyxField('nodeId', NyxData(Node('body')))])).Field('attributes');
  Result := '';
  LIndex := 0;
  while LIndex < LAttributes.Count do
  begin

    if LAttributes.Item(LIndex).AsText = AName then
    begin
      Exit(LAttributes.Item(LIndex + 1).AsText);
    end;
    Inc(LIndex, 2);
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
    FSocket.CheckIncoming;
    DrainBrowser;

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
