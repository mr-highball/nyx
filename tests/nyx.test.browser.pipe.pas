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

unit nyx.test.browser.pipe;

{$mode delphi}{$H+}{$codepage utf8}
{$IFNDEF MSWINDOWS}{$fatal This host uses Windows anonymous Chromium pipes}{$ENDIF}

interface

uses
  Classes, SysUtils, Windows, Process, nyx.text, nyx.data;

type
  { Owns one unique headless Chromium profile/process and anonymous debugger
    pipes. No TCP listener or existing editor tab is opened. Requests are serial,
    JSON/UTF-8 packets are NUL-delimited and bounded, and no scripts are injected.
    The browser and its Pascal workers use their ordinary real clocks. }
  TNyxBrowserPipe = class
  private
    FBrowser: TProcess;
    FInput: THandle;
    FOutput: THandle;
    FPending: RawByteString;
    FDiagnostics: RawByteString;
    FDirectory: String;
    FSession: TNyxText;
    FRuntimeError: TNyxText;
    FSequence: Integer;
    FWaiting: Boolean;
    FLoaded: Boolean;
    FBody: Integer;
    procedure DrainDiagnostics;
    function ReadPacket(out APacket: TNyxDataValue): Boolean;
    procedure Send(const ABytes: RawByteString);
    function Request(const AMethod: TNyxText; const AParams: TNyxDataValue;
      const ASession: TNyxText): TNyxDataValue;
    procedure Save(const AName: String; const ABytes: RawByteString);
    function Body: Integer;
  public
    { URL must name a loopback HTTP fixture. Directory receives a fresh profile,
      bounded diagnostics and captures. Width accepts 320..4096 CSS pixels.
      Failed construction retires only the process/handles owned by this host. }
    constructor Create(const AURL, ADirectory: String; AWidth: Integer = 1100);
    destructor Destroy; override;
    { Bounded current body observation; absent attributes return empty text.
      SetAttribute acknowledges fixture-only capture checkpoints, not editor
      operations. Product design composition remains semantic MCP work. }
    function Attribute(const AName: TNyxText): TNyxText;
    procedure SetAttribute(const AName, AValue: TNyxText);
    { Save exact current outer HTML and PNG, using a simple artifact name. This
      does not infer readiness; the caller first observes a terminal fixture mark. }
    procedure Capture(const AName: String);
    { Last actual Runtime exception packet, independent of readiness markers. }
    property RuntimeError: TNyxText read FRuntimeError;
  end;

implementation

uses
  base64;

const
  CPacketLimit = 4 * 1024 * 1024;
  CDiagnosticLimit = 1024 * 1024;
  CCommandMilliseconds = 15000;

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

procedure ClosePipe(var AHandle: THandle);
begin

  if AHandle <> 0 then
  begin
    CloseHandle(AHandle);
    AHandle := 0;
  end;
end;

constructor TNyxBrowserPipe.Create(const AURL, ADirectory: String; AWidth: Integer);
var
  LSecurity: TSecurityAttributes;
  LChildInput: THandle;
  LChildOutput: THandle;
  LProfile: String;
  LGuid: TGuid;
  LTarget: TNyxText;
  LPacket: TNyxDataValue;
  LStarted: QWord;
begin
  inherited Create;

  if (Pos('http://127.0.0.1:', AURL) <> 1) or
    (AWidth < 320) or (AWidth > 4096) then
  begin
    raise Exception.Create('Supply a loopback HTTP fixture and width 320..4096');
  end;
  FDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory));
  ForceDirectories(FDirectory);
  CreateGUID(LGuid);
  LProfile := FDirectory + 'profile-' + GUIDToString(LGuid);
  ForceDirectories(LProfile);
  LChildInput := 0;
  LChildOutput := 0;
  LSecurity := Default(TSecurityAttributes);
  LSecurity.nLength := SizeOf(LSecurity);
  LSecurity.bInheritHandle := True;
  try

    if not CreatePipe(LChildInput, FInput, @LSecurity, 0) or
      not CreatePipe(FOutput, LChildOutput, @LSecurity, 0) or
      not SetHandleInformation(FInput, HANDLE_FLAG_INHERIT, 0) or
      not SetHandleInformation(FOutput, HANDLE_FLAG_INHERIT, 0) then
    begin
      RaiseLastOSError;
    end;
    FBrowser := TProcess.Create(nil);
    FBrowser.Executable := SysUtils.GetEnvironmentVariable('ProgramFiles(x86)') +
      '\Microsoft\Edge\Application\msedge.exe';
    FBrowser.Options := [poUsePipes, poStderrToOutput, poNoConsole];
    { This standalone owner has only its two child pipe endpoints and ordinary
      redirected standard handles inheritable. Parent endpoints are explicitly
      non-inheritable and close after the exact owned child retires. }
    FBrowser.InheritHandles := True;
    FBrowser.Parameters.Add('--headless=new');
    FBrowser.Parameters.Add('--disable-gpu');
    FBrowser.Parameters.Add('--no-first-run');
    FBrowser.Parameters.Add('--no-default-browser-check');
    FBrowser.Parameters.Add('--disable-extensions');
    FBrowser.Parameters.Add('--remote-debugging-pipe');
    FBrowser.Parameters.Add('--remote-debugging-io-pipes=' +
      UIntToStr(PtrUInt(LChildInput)) + ',' + UIntToStr(PtrUInt(LChildOutput)));
    FBrowser.Parameters.Add('--user-data-dir=' + LProfile);
    FBrowser.Parameters.Add('--window-size=' + IntToStr(AWidth) + ',1000');
    FBrowser.Parameters.Add('about:blank');
    FBrowser.Execute;
  finally
    ClosePipe(LChildInput);
    ClosePipe(LChildOutput);
  end;
  Request('Browser.getVersion', NyxObject([]), '');
  LTarget := Request('Target.createTarget', NyxObject([
    NyxField('url', NyxData('about:blank'))]), '').Field('targetId').AsText;
  FSession := Request('Target.attachToTarget', NyxObject([
    NyxField('targetId', NyxData(LTarget)),
    NyxField('flatten', NyxData(True))]), '').Field('sessionId').AsText;
  Request('Runtime.enable', NyxObject([]), FSession);
  Request('Page.enable', NyxObject([]), FSession);
  Request('DOM.enable', NyxObject([]), FSession);
  Request('Emulation.setDeviceMetricsOverride', NyxObject([
    NyxField('width', NyxData(AWidth)), NyxField('height', NyxData(900)),
    NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(False))]), FSession);
  Request('Page.navigate', NyxObject([NyxField('url', NyxData(TNyxText(AURL)))]), FSession);
  LStarted := GetTickCount64;
  while not FLoaded do
  begin
    ReadPacket(LPacket);

    if GetTickCount64 - LStarted > CCommandMilliseconds then
    begin
      raise Exception.Create('Real-clock fixture navigation did not complete');
    end;
    Sleep(5);
  end;
end;

procedure TNyxBrowserPipe.Send(const ABytes: RawByteString);
var
  LBytes: RawByteString;
  LWritten: DWORD;
begin

  if (Length(ABytes) = 0) or (Length(ABytes) > CPacketLimit) then
  begin
    raise Exception.Create('Debugger request exceeds the packet budget');
  end;
  LBytes := ABytes + #0;

  if not WriteFile(FInput, LBytes[1], Length(LBytes), LWritten, nil) or
    (LWritten <> DWORD(Length(LBytes))) then
  begin
    RaiseLastOSError;
  end;
end;

procedure TNyxBrowserPipe.DrainDiagnostics;
var
  LBuffer: array[0..8191] of Byte;
  LBytes: RawByteString;
  LRead: Integer;
begin

  if (FBrowser = nil) or (FBrowser.Output = nil) then
  begin
    Exit;
  end;
  while FBrowser.Output.NumBytesAvailable > 0 do
  begin
    LRead := FBrowser.Output.Read(LBuffer, SizeOf(LBuffer));
    SetLength(LBytes, LRead);

    if LRead > 0 then
    begin
      Move(LBuffer[0], LBytes[1], LRead);
    end;
    FDiagnostics := FDiagnostics + LBytes;

    if Length(FDiagnostics) > CDiagnosticLimit then
    begin
      raise Exception.Create('Browser diagnostics exceed the capture budget');
    end;
  end;
end;

function TNyxBrowserPipe.ReadPacket(out APacket: TNyxDataValue): Boolean;
var
  LAvailable: DWORD;
  LRead: DWORD;
  LBuffer: array[0..16383] of Byte;
  LBytes: RawByteString;
  LEnd: Integer;
begin
  Result := False;
  APacket := Default(TNyxDataValue);
  DrainDiagnostics;
  LEnd := Pos(#0, FPending);

  if LEnd = 0 then
  begin

    if not PeekNamedPipe(FOutput, nil, 0, nil, @LAvailable, nil) then
    begin
      RaiseLastOSError;
    end;

    if LAvailable > 0 then
    begin

      if not ReadFile(FOutput, LBuffer, SizeOf(LBuffer), LRead, nil) then
      begin
        RaiseLastOSError;
      end;
      SetLength(LBytes, LRead);

      if LRead > 0 then
      begin
        Move(LBuffer[0], LBytes[1], LRead);
      end;
      FPending := FPending + LBytes;

      if Length(FPending) > CPacketLimit then
      begin
        raise Exception.Create('Debugger reply exceeds the packet budget');
      end;
      LEnd := Pos(#0, FPending);
    end;
  end;

  if LEnd > 0 then
  begin
    APacket := TNyxDataValue.ParseJSON(TNyxText(Copy(FPending, 1, LEnd - 1)));
    Delete(FPending, 1, LEnd);

    if HasField(APacket, 'method') then
    begin

      if APacket.Field('method').AsText = 'Page.loadEventFired' then
      begin
        FLoaded := True;
      end
      else if APacket.Field('method').AsText = 'DOM.documentUpdated' then
      begin
        FBody := 0;
      end
      else if APacket.Field('method').AsText = 'Runtime.exceptionThrown' then
      begin
        FRuntimeError := APacket.ToJSON;
        Save('runtime-error.json', FRuntimeError);
      end;
    end;
    Result := True;
  end;
end;

function TNyxBrowserPipe.Request(const AMethod: TNyxText;
  const AParams: TNyxDataValue; const ASession: TNyxText): TNyxDataValue;
var
  LRequest: TNyxDataValue;
  LPacket: TNyxDataValue;
  LStarted: QWord;
begin

  if FWaiting then
  begin
    raise Exception.Create('Debugger requests cannot overlap');
  end;
  Inc(FSequence);
  LRequest := NyxObject([NyxField('id', NyxData(FSequence)),
    NyxField('method', NyxData(AMethod)), NyxField('params', AParams)]);

  if ASession <> '' then
  begin
    LRequest := NyxObject([NyxField('id', NyxData(FSequence)),
      NyxField('method', NyxData(AMethod)), NyxField('params', AParams),
      NyxField('sessionId', NyxData(ASession))]);
  end;
  FWaiting := True;
  try
    Send(LRequest.ToJSON);
    LStarted := GetTickCount64;
    repeat

      if ReadPacket(LPacket) then
      begin

        if HasField(LPacket, 'id') and
          (LPacket.Field('id').AsInteger = FSequence) then
        begin

          if HasField(LPacket, 'error') then
          begin
            raise Exception.Create('Debugger refused ' + AMethod + ': ' +
              LPacket.Field('error').ToJSON);
          end;
          Exit(LPacket.Field('result'));
        end;
      end
      else
      begin

        if not FBrowser.Running then
        begin
          raise Exception.Create('Owned browser retired before its command reply');
        end;
        Sleep(5);
      end;

      if GetTickCount64 - LStarted > CCommandMilliseconds then
      begin
        raise Exception.Create('Real-clock debugger command timed out: ' + AMethod);
      end;
    until False;
  finally
    FWaiting := False;
  end;
end;

procedure TNyxBrowserPipe.Save(const AName: String; const ABytes: RawByteString);
var
  LFile: TFileStream;
begin

  if (AName = '') or (ExtractFileName(AName) <> AName) then
  begin
    raise Exception.Create('Capture requires a simple artifact filename');
  end;
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

function TNyxBrowserPipe.Body: Integer;
var
  LRoot: TNyxDataValue;
begin
  { Body identity survives ordinary Studio child replacement. Invalidate it on
    actual document publication; repeated full document queries are unnecessary. }

  if FBody <> 0 then
  begin
    Exit(FBody);
  end;
  LRoot := Request('DOM.getDocument', NyxObject([]), FSession).Field('root');
  Result := Request('DOM.querySelector', NyxObject([
    NyxField('nodeId', LRoot.Field('nodeId')), NyxField('selector', NyxData('body'))]),
    FSession).Field('nodeId').AsInteger;

  if Result = 0 then
  begin
    raise Exception.Create('Fixture body is not ready');
  end;
  FBody := Result;
end;

function TNyxBrowserPipe.Attribute(const AName: TNyxText): TNyxText;
var
  LAttributes: TNyxDataValue;
  LIndex: Integer;
begin
  LAttributes := Request('DOM.getAttributes', NyxObject([
    NyxField('nodeId', NyxData(Body))]), FSession).Field('attributes');
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

procedure TNyxBrowserPipe.SetAttribute(const AName, AValue: TNyxText);
begin
  Request('DOM.setAttributeValue', NyxObject([NyxField('nodeId', NyxData(Body)),
    NyxField('name', NyxData(AName)), NyxField('value', NyxData(AValue))]), FSession);
end;

procedure TNyxBrowserPipe.Capture(const AName: String);
var
  LRoot: TNyxDataValue;
  LImage: TNyxDataValue;
begin
  LRoot := Request('DOM.getDocument', NyxObject([]), FSession).Field('root');
  FBody := 0;
  Save(AName + '.dom.html', Request('DOM.getOuterHTML', NyxObject([
    NyxField('nodeId', LRoot.Field('nodeId'))]), FSession).Field('outerHTML').AsText);
  LImage := Request('Page.captureScreenshot', NyxObject([
    NyxField('format', NyxData('png')), NyxField('captureBeyondViewport', NyxData(False))]),
    FSession);
  Save(AName + '.png', DecodeStringBase64(LImage.Field('data').AsText));
end;

destructor TNyxBrowserPipe.Destroy;
var
  LStarted: QWord;
begin

  if FBrowser <> nil then
  begin

    if FBrowser.Running and (FInput <> 0) then
    begin
      try
        Send(NyxObject([NyxField('id', NyxData(FSequence + 1)),
          NyxField('method', NyxData('Browser.close'))]).ToJSON);
      except
        { Preserve the original failure; the owned process handle is the fallback. }
      end;
      LStarted := GetTickCount64;
      repeat
        try
          DrainDiagnostics;
        except
          { Diagnostics must not prevent exact owned-child retirement. }
        end;

        if FBrowser.Running then
        begin
          Sleep(10);
        end;
      until not FBrowser.Running or (GetTickCount64 - LStarted > 3000);

      if FBrowser.Running then
      begin
        FBrowser.Terminate(1);
        FBrowser.WaitOnExit;
      end;
    end;
    try
      DrainDiagnostics;
      Save('browser.log', FDiagnostics);
    except
      { Teardown still releases handles if an artifact cannot be written. }
    end;
  end;
  FBrowser.Free;
  ClosePipe(FInput);
  ClosePipe(FOutput);
  inherited Destroy;
end;

end.
