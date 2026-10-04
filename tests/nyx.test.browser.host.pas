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
unit nyx.test.browser.host;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Process, fphttpclient, fpwebsocket, fpwebsocketclient,
  nyx.text, nyx.data;

type
  { Bounded native owner of one isolated Chromium profile and loopback CDP
    transport. Consumers issue DOM and host Input commands, never inject scripts.
    Constructor failures and normal shutdown close only this owned browser.
    Responses and diagnostics are bounded; semantic fixture state decides passes. }
  TNyxBrowserHost = class
  private
    FBrowser: TProcess;
    FSocket: TWebsocketClient;
    FDirectory: String;
    FBrowserLog: RawByteString;
    FSequence: Integer;
    FExpected: Integer;
    FReply: TNyxDataValue;
    FLoaded: Boolean;
    procedure Received(ASender: TObject; const AMessage: TWSMessage);
    procedure DrainBrowser;
  protected
    { Notification payloads are immutable owned values. Overrides retain any
      payload needed after returning. Pump services the owned connection. }
    procedure Notification(const AMethod: TNyxText; const AParams: TNyxDataValue); virtual;
    procedure Pump;
    function Command(const AMethod: TNyxText; const AParams: TNyxDataValue): TNyxDataValue;
    function Node(const ASelector: TNyxText): Integer;
    function Attribute(const AName: TNyxText): TNyxText;
    procedure Save(const AName: String; const ABytes: RawByteString);
  public
    constructor Create(const AURL, ADirectory: String);
    destructor Destroy; override;
  end;

implementation

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

procedure TNyxBrowserHost.Save(const AName: String; const ABytes: RawByteString);
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

procedure TNyxBrowserHost.DrainBrowser;
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

procedure TNyxBrowserHost.Received(ASender: TObject; const AMessage: TWSMessage);
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
    (LPacket.Field('method').AsText = 'Page.loadEventFired') then
  begin
    FLoaded := True;
  end
  else if HasField(LPacket, 'method') and
    (LPacket.Field('method').AsText = 'Runtime.exceptionThrown') then
  begin
    Save('runtime-error.json', LPacket.ToJSON);
  end;

  if HasField(LPacket, 'method') then
  begin
    Notification(LPacket.Field('method').AsText, LPacket.Field('params'));
  end;
end;

function TNyxBrowserHost.Command(const AMethod: TNyxText;
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

constructor TNyxBrowserHost.Create(const AURL, ADirectory: String);
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

destructor TNyxBrowserHost.Destroy;
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

function TNyxBrowserHost.Node(const ASelector: TNyxText): Integer;
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

function TNyxBrowserHost.Attribute(const AName: TNyxText): TNyxText;
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

procedure TNyxBrowserHost.Notification(const AMethod: TNyxText;
  const AParams: TNyxDataValue);
begin
  { Derived input journeys observe only the notifications they require. }
end;

procedure TNyxBrowserHost.Pump;
begin
  FSocket.CheckIncoming;
  DrainBrowser;
end;

end.