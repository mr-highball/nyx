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
  { Ordinary fixtures keep their exact loopback origin. The opt-in variant maps
    only one reserved .test name to the same loopback listener, leaving actual
    browser secure-context and Cache Storage availability rules in force.
    It never changes OS DNS, certificate trust, host APIs or another process. }
  TNyxBrowserOrigin = (nboLoopback, nboUntrustworthyFixture);
  { A bounded optional exact-URL wire observation, separate from document state.
    It never intercepts a request or changes browser certificate/CORS decisions. }
  TNyxBrowserNetworkFailure = (nnfNone, nnfCORS, nnfCertificate, nnfOther);
  TNyxBrowserNetworkResult = record
    Requests: Integer;
    Status: Integer;
    Secure: Boolean;
    Failure: TNyxBrowserNetworkFailure;
    Error: TNyxText;
  end;
  { Closed host keys used by maintained input journeys. These are Chromium
    protocol input, not portable product events or application shortcuts. }
  TNyxBrowserKey = (nbkHome, nbkEnd, nbkUp, nbkDown, nbkEnter, nbkEscape,
    nbkSelectAll, nbkLeft, nbkRight, nbkF2);
  { Physical axis-aligned viewport border bounds in CSS pixels, observed through
    Chromium's DOM protocol. This is target evidence, not document layout state. }
  TNyxBrowserBox = record
    Left: Double;
    Top: Double;
    Width: Double;
    Height: Double;
  end;
  { Owns a freshly allocated qualification profile, never an existing user
    profile. A pipe borrows this owner, which must outlive every pipe using it.
    Only one process may lease the profile at a time. Reuse after retirement
    qualifies real browser persistence; files remain as private evidence. }
  TNyxBrowserProfile = class
  private
    FDirectory: String;
    FInUse: Boolean;
  public
    constructor Create(const ADirectory: String);
    property Directory: String read FDirectory;
  end;
  { Owns one unique headless Chromium process and anonymous debugger
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
    FProfile: TNyxBrowserProfile;
    FSession: TNyxText;
    FRuntimeError: TNyxText;
    FSequence: Integer;
    FWaiting: Boolean;
    FLoaded: Boolean;
    FBody: Integer;
    FObservedURL: TNyxText;
    FObservedRequest: TNyxText;
    FNetwork: TNyxBrowserNetworkResult;
    {$ifdef NYX_BROWSER_CONSOLE_TRACE}
    { Opt-in qualification receipts retain at most 64 complete console packets.
      They stay private in the owned capture directory; ordinary hosts do not
      collect application text. No script evaluation or document edit occurs. }
    FConsolePackets: Integer;
    {$endif}
    procedure DrainDiagnostics;
    procedure ObserveNetworkPacket(const APacket: TNyxDataValue);
    function ReadPacket(out APacket: TNyxDataValue): Boolean;
    procedure Send(const ABytes: RawByteString);
    function Request(const AMethod: TNyxText; const AParams: TNyxDataValue;
      const ASession: TNyxText): TNyxDataValue;
    procedure Save(const AName: String; const ABytes: RawByteString);
    function Body: Integer;
    function QueryRoot(const AFrame: TNyxText): Integer;
  public
    { URL must name a loopback HTTP fixture, or the fixed nyx-cache.test origin
      with its explicit opt-in origin mode. Directory receives a fresh profile,
      bounded diagnostics and captures. Width accepts 320..4096 CSS pixels.
      An optional borrowed fresh profile permits sequential process restarts;
      its owner must outlive this pipe. Concurrent use refuses before launching.
      Failed construction retires only the process/handles owned by this host. }
    constructor Create(const AURL, ADirectory: String; AWidth: Integer = 1100;
      AHeight: Integer = 900; AProfile: TNyxBrowserProfile = nil;
      AOrigin: TNyxBrowserOrigin = nboLoopback);
    destructor Destroy; override;
    { Bounded current body observation; absent attributes return empty text.
      SetAttribute acknowledges fixture-only capture checkpoints, not editor
      operations. Product design composition remains semantic MCP work. }
    function Attribute(const AName: TNyxText): TNyxText;
    procedure SetAttribute(const AName, AValue: TNyxText);
    { Read a bounded selector's rendered subtree or live value without evaluating
      scripts. Missing elements return empty text. This is physical observation,
      separate from document inspection through semantic MCP. }
    function ElementHTML(const ASelector: TNyxText): TNyxText;
    { Single bounded existence query, avoiding full-shell serialization and
      the two-request lifetime race when only presentation presence matters. }
    function Exists(const ASelector: TNyxText): Boolean;
    { Qualify actual fetch deadlines/cancellation through host network latency,
      without substituting a transport, evaluating scripts or changing a design.
      Apply after fixture readiness and before its capture acknowledgement.
      Zero restores normal networking; accepted delay is 0..30000 milliseconds.
      This is Chromium emulation, not a physical-network timing qualification. }
    procedure NetworkLatency(AMilliseconds: Integer);
    { Enable observation before the fixture acknowledges its next actual load.
      Exactly one URL is retained, with at most eight request starts. No headers,
      bodies, cookies or credentials are collected. Reset replaces prior evidence.
      NetworkResult borrows no DOM/node or document and never starts a request. }
    procedure ObserveResource(const AURL: TNyxText);
    function NetworkResult: TNyxBrowserNetworkResult;
    { Observe the mounted face without scrolling, evaluating scripts or changing
      the design. Missing faces fail explicitly; transformed quads are enclosed
      in their viewport-aligned border box. Call after ordinary readiness. }
    function Bounds(const ASelector: TNyxText): TNyxBrowserBox;
    { Reveal an existing face for selective visual qualification without clicking
      it, running application scripts or changing document state. Missing faces
      refuse. The scroll affects only this host's independently owned profile. }
    procedure Reveal(const ASelector: TNyxText);
    { False means absent or retired during ordinary asynchronous DOM replacement,
      distinct from a present empty field. Unknown protocol errors still raise. }
    function TryFieldValue(const ASelector: TNyxText; out AValue: TNyxText;
      const AFrame: TNyxText = ''): Boolean;
    { Actual child-document backend identity, obtained through DOM protocol.
      Zero means the selected frame/document is absent or not ready. This proves
      document retention independently of matching iframe attributes or reports. }
    function FrameDocumentIdentity(const AFrame: TNyxText): Integer;
    { Exercise one visible editor host control through ordinary pointer input.
      Callers must use semantic MCP for composition and accepted design edits. }
    procedure Click(const ASelector: TNyxText; const AFrame: TNyxText = '');
    { Physical Chromium Tab down/up, including the browser's focus traversal.
      This qualifies host defaults that synthetic DOM events cannot establish;
      it does not claim hardware, IME or assistive-technology input. }
    procedure Tab(AReverse: Boolean = False);
    { Exercise the focused host control with trusted keyboard input. SelectAll
      uses Control+A on this Windows host. Optional modifiers are explicit
      protocol input; no hardware key state or DOM value is assigned. }
    procedure Key(AKey: TNyxBrowserKey; AControl: Boolean = False;
      AShift: Boolean = False);
    { Insert text at the actual focused host caret. Does not assign a DOM value
      or focus a replacement element; this also qualifies source navigation. }
    procedure TypeText(const AText: TNyxText);
    { Replace a visible text input through focus, host selection and text input;
      ordinary Tab commits its change. Use only for control qualification, never
      as a substitute for semantic demo/document composition. }
    procedure ReplaceText(const ASelector, AText: TNyxText);
    { Change real host allocation, including landscape/short-window transitions.
      Bounds are 320..4096 wide and 240..4096 high; no application script runs. }
    procedure Resize(AWidth, AHeight: Integer);
    { Trusted host touch sequence on a visible control. Intended for public
      splitter/gesture qualification; never use it to author a design. }
    procedure DragTouch(const ASelector: TNyxText; ADeltaX, ADeltaY: Double);
    { Save exact current outer HTML and PNG, using a simple artifact name. This
      does not infer readiness; the caller first observes a terminal fixture mark. }
    procedure Capture(const AName: String);
    { Preserve exact Pascal exception properties and its native JS error stack
      in private receipts. Reads own debugger properties without evaluating
      source; launch context remains in the ignored qualification directory. }
    procedure CaptureRuntimeError;
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

constructor TNyxBrowserProfile.Create(const ADirectory: String);
var
  LGuid: TGuid;
begin
  inherited Create;
  CreateGUID(LGuid);
  FDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory)) +
    'profile-' + GUIDToString(LGuid);
  ForceDirectories(FDirectory);
end;

constructor TNyxBrowserPipe.Create(const AURL, ADirectory: String;
  AWidth, AHeight: Integer; AProfile: TNyxBrowserProfile; AOrigin: TNyxBrowserOrigin);
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

  if (((AOrigin = nboLoopback) and (Pos('http://127.0.0.1:', AURL) <> 1)) or
    ((AOrigin = nboUntrustworthyFixture) and (Pos('http://nyx-cache.test:', AURL) <> 1))) or
    (AWidth < 320) or (AWidth > 4096) or (AHeight < 240) or (AHeight > 4096) then
  begin
    raise Exception.Create('Supply the admitted fixture origin and width 320..4096');
  end;
  FDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory));
  ForceDirectories(FDirectory);

  if AProfile <> nil then
  begin

    if AProfile.FInUse then
    begin
      raise Exception.Create('Qualification profile already has a live browser');
    end;
    FProfile := AProfile;
    FProfile.FInUse := True;
    LProfile := FProfile.Directory;
  end
  else
  begin
    CreateGUID(LGuid);
    LProfile := FDirectory + 'profile-' + GUIDToString(LGuid);
    ForceDirectories(LProfile);
  end;
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

    if AOrigin = nboUntrustworthyFixture then
    begin
      FBrowser.Parameters.Add('--host-resolver-rules=MAP nyx-cache.test 127.0.0.1');
      FBrowser.Parameters.Add('--proxy-bypass-list=nyx-cache.test');
    end;
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
  Resize(AWidth, AHeight);
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
      ObserveNetworkPacket(APacket);

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
      {$ifdef NYX_BROWSER_CONSOLE_TRACE}

      if (APacket.Field('method').AsText = 'Runtime.consoleAPICalled') and
        (FConsolePackets < 64) then
      begin
        Inc(FConsolePackets);
        Save('console-' + IntToStr(FConsolePackets) + '.json', APacket.ToJSON);
      end;
      {$endif}
    end;
    Result := True;
  end;
end;

procedure TNyxBrowserPipe.ObserveNetworkPacket(const APacket: TNyxDataValue);
var
  LMethod: TNyxText;
  LParameters: TNyxDataValue;
  LResponse: TNyxDataValue;
begin

  if FObservedURL = '' then
  begin
    Exit;
  end;
  LMethod := APacket.Field('method').AsText;

  if Copy(LMethod, 1, Length('Network.')) <> 'Network.' then
  begin
    Exit;
  end;
  LParameters := APacket.Field('params');

  if LMethod = 'Network.requestWillBeSent' then
  begin

    if LParameters.Field('request').Field('url').AsText = FObservedURL then
    begin
      Inc(FNetwork.Requests);

      if FNetwork.Requests > 8 then
      begin
        raise Exception.Create('Observed fixture URL exceeds eight bounded request starts');
      end;
      FObservedRequest := LParameters.Field('requestId').AsText;
    end;
    Exit;
  end;

  if (FObservedRequest = '') or not HasField(LParameters, 'requestId') or
    (LParameters.Field('requestId').AsText <> FObservedRequest) then
  begin
    Exit;
  end;

  if LMethod = 'Network.responseReceived' then
  begin
    LResponse := LParameters.Field('response');
    FNetwork.Status := LResponse.Field('status').AsInteger;
    FNetwork.Secure := HasField(LResponse, 'securityState') and
      (LResponse.Field('securityState').AsText = 'secure');
  end
  else if LMethod = 'Network.loadingFailed' then
  begin
    FNetwork.Error := LParameters.Field('errorText').AsText;

    if Length(FNetwork.Error) > 256 then
    begin
      raise Exception.Create('Observed protocol error exceeds its bounded text budget');
    end;
    FNetwork.Failure := nnfOther;

    if HasField(LParameters, 'corsErrorStatus') then
    begin
      FNetwork.Failure := nnfCORS;
    end
    else if Pos('net::ERR_CERT_', FNetwork.Error) = 1 then
    begin
      FNetwork.Failure := nnfCertificate;
    end;
  end;
end;

procedure TNyxBrowserPipe.ObserveResource(const AURL: TNyxText);
begin

  if (AURL = '') or (Length(AURL) > 4096) then
  begin
    raise Exception.Create('Observed fixture URL must be nonempty and at most 4096 code units');
  end;
  FObservedURL := AURL;
  FObservedRequest := '';
  FNetwork := Default(TNyxBrowserNetworkResult);
  Request('Network.enable', NyxObject([]), FSession);
end;

function TNyxBrowserPipe.NetworkResult: TNyxBrowserNetworkResult;
begin
  Result := FNetwork;
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

procedure TNyxBrowserPipe.CaptureRuntimeError;
var
  LError: TNyxDataValue;
  LProperties: TNyxDataValue;
  LProperty: TNyxDataValue;
  LIndex: Integer;
begin

  if FRuntimeError = '' then
  begin
    Exit;
  end;
  LError := TNyxDataValue.ParseJSON(FRuntimeError).Field('params')
    .Field('exceptionDetails').Field('exception');
  LProperties := Request('Runtime.getProperties', NyxObject([
    NyxField('objectId', LError.Field('objectId')),
    NyxField('ownProperties', NyxData(True))]), FSession).Field('result');
  Save('runtime-properties.json', LProperties.ToJSON);
  for LIndex := 0 to LProperties.Count - 1 do
  begin
    LProperty := LProperties.Item(LIndex);

    if (LProperty.Field('name').AsText = 'FJSError') and
      HasField(LProperty.Field('value'), 'objectId') then
    begin
      LProperties := Request('Runtime.getProperties', NyxObject([
        NyxField('objectId', LProperty.Field('value').Field('objectId')),
        NyxField('ownProperties', NyxData(True))]), FSession).Field('result');
      Save('runtime-native-error.json', LProperties.ToJSON);
      Break;
    end;
  end;
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

function TNyxBrowserPipe.QueryRoot(const AFrame: TNyxText): Integer;
var
  LNode: Integer;
  LFace: TNyxDataValue;
begin

  if AFrame = '' then
  begin
    Exit(Body);
  end;
  Result := 0;
  LNode := Request('DOM.querySelector', NyxObject([
    NyxField('nodeId', NyxData(Body)), NyxField('selector', NyxData(AFrame))]),
    FSession).Field('nodeId').AsInteger;

  if LNode = 0 then
  begin
    Exit;
  end;
  try
    LFace := Request('DOM.describeNode', NyxObject([
      NyxField('nodeId', NyxData(LNode)), NyxField('depth', NyxData(1)),
      NyxField('pierce', NyxData(True))]), FSession).Field('node');
  except
    on LException: Exception do
    begin
      { A deliberate runtime replacement can retire the iframe between these
        two bounded observations. Report absence for that exact debugger race;
        callers still need a positive identity and actual mounted controls.
        Other debugger/transport errors retain their ordinary failure. }

      if Pos('Could not find node with given id', LException.Message) > 0 then
      begin
        Exit(0);
      end;
      raise;
    end;
  end;

  if not HasField(LFace, 'contentDocument') then
  begin
    Exit;
  end;
  { Backend IDs survive a debugger tree refresh; push only this one document
    into the frontend tree instead of returning the entire application DOM. }
  Result := Request('DOM.pushNodesByBackendIdsToFrontend', NyxObject([
    NyxField('backendNodeIds', NyxArray([
      LFace.Field('contentDocument').Field('backendNodeId')]))]),
    FSession).Field('nodeIds').Item(0).AsInteger;
end;

function TNyxBrowserPipe.FrameDocumentIdentity(const AFrame: TNyxText): Integer;
var
  LNode: Integer;
begin
  Result := 0;
  LNode := QueryRoot(AFrame);

  if LNode <> 0 then
  begin
    try
      Result := Request('DOM.describeNode', NyxObject([
        NyxField('nodeId', NyxData(LNode))]), FSession).Field('node')
        .Field('backendNodeId').AsInteger;
    except
      on LException: Exception do
      begin

        if Pos('Could not find node with given id', LException.Message) = 0 then
        begin
          raise;
        end;
        Result := 0;
      end;
    end;
  end;
end;

procedure TNyxBrowserPipe.SetAttribute(const AName, AValue: TNyxText);
begin
  Request('DOM.setAttributeValue', NyxObject([NyxField('nodeId', NyxData(Body)),
    NyxField('name', NyxData(AName)), NyxField('value', NyxData(AValue))]), FSession);
end;

procedure TNyxBrowserPipe.NetworkLatency(AMilliseconds: Integer);
begin

  if (AMilliseconds < 0) or (AMilliseconds > 30000) then
  begin
    raise Exception.Create('Qualification network latency requires 0..30000 milliseconds');
  end;
  Request('Network.enable', NyxObject([]), FSession);
  Request('Network.emulateNetworkConditions', NyxObject([
    NyxField('offline', NyxData(False)),
    NyxField('latency', NyxData(AMilliseconds)),
    NyxField('downloadThroughput', NyxData(-1)),
    NyxField('uploadThroughput', NyxData(-1))]), FSession);
end;

function TNyxBrowserPipe.Exists(const ASelector: TNyxText): Boolean;
begin
  Result := Request('DOM.querySelector', NyxObject([
    NyxField('nodeId', NyxData(Body)), NyxField('selector', NyxData(ASelector))]),
    FSession).Field('nodeId').AsInteger <> 0;
end;

function TNyxBrowserPipe.ElementHTML(const ASelector: TNyxText): TNyxText;
var
  LNode: Integer;
begin
  LNode := Request('DOM.querySelector', NyxObject([
    NyxField('nodeId', NyxData(Body)), NyxField('selector', NyxData(ASelector))]),
    FSession).Field('nodeId').AsInteger;
  Result := '';

  if LNode <> 0 then
  begin
    try
      Result := Request('DOM.getOuterHTML', NyxObject([
        NyxField('nodeId', NyxData(LNode))]), FSession).Field('outerHTML').AsText;
    except
      on LException: Exception do
      begin
        { An asynchronous editor paint can retire the queried node before this
          second debugger request. Report transient absence so the caller's
          bounded readiness loop can query again; other failures remain errors. }

        if Pos('Could not find node with given id', LException.Message) > 0 then
        begin
          Exit('');
        end;
        raise;
      end;
    end;
  end;
end;

function TNyxBrowserPipe.TryFieldValue(const ASelector: TNyxText;
  out AValue: TNyxText; const AFrame: TNyxText): Boolean;
var
  LNode: Integer;
  LBackend: Integer;
  LSnapshot: TNyxDataValue;
  LNodes: TNyxDataValue;
  LValues: TNyxDataValue;
  LStrings: TNyxDataValue;
  LBackends: TNyxDataValue;
  LNames: TNyxDataValue;
  LIndexes: TNyxDataValue;
  LStringIndexes: TNyxDataValue;
  LIndex: Integer;
  LValueIndex: Integer;
  LFace: TNyxDataValue;
  LRoot: Integer;
  LDocument: Integer;
  LFound: Boolean;
begin
  Result := False;
  AValue := '';
  LRoot := QueryRoot(AFrame);

  if LRoot = 0 then
  begin
    Exit;
  end;
  LNode := Request('DOM.querySelector', NyxObject([
    NyxField('nodeId', NyxData(LRoot)), NyxField('selector', NyxData(ASelector))]),
    FSession).Field('nodeId').AsInteger;

  if LNode = 0 then
  begin
    Exit;
  end;
  try
    LBackend := Request('DOM.describeNode', NyxObject([
      NyxField('nodeId', NyxData(LNode))]), FSession).Field('node').Field('backendNodeId').AsInteger;
  except
    on LException: Exception do
    begin

      if Pos('Could not find node with given id', LException.Message) > 0 then
      begin
        Exit(False);
      end;
      raise;
    end;
  end;
  LFace := Request('DOM.describeNode', NyxObject([
    NyxField('backendNodeId', NyxData(LBackend))]), FSession).Field('node');

  if LFace.Field('nodeName').AsText = 'SELECT' then
  begin
    { DOMSnapshot inputValue excludes SELECT. Its native accessible value is
      the current option caption, including a keyboard change that never edits
      the option's selected attribute. Nyx's text-only choices use that exact
      caption as their value. Keep this observation bounded to one host face. }
    LSnapshot := Request('Accessibility.getPartialAXTree', NyxObject([
      NyxField('backendNodeId', NyxData(LBackend)),
      NyxField('fetchRelatives', NyxData(False))]), FSession).Field('nodes');

    if LSnapshot.Count = 1 then
    begin
      AValue := LSnapshot.Item(0).Field('value').Field('value').AsText;
      Exit(True);
    end;
    Exit(False);
  end;
  { The browser snapshot exposes live input/textarea values without evaluating
    getters or injecting source. Backend identity selects exactly this field;
    no snapshot is returned as agent document context. Packet bounds still apply. }
  LSnapshot := Request('DOMSnapshot.captureSnapshot', NyxObject([
    NyxField('computedStyles', NyxArray([]))]), FSession);
  LStrings := LSnapshot.Field('strings');
  LFound := False;
  for LDocument := 0 to LSnapshot.Field('documents').Count - 1 do
  begin
    LNodes := LSnapshot.Field('documents').Item(LDocument).Field('nodes');
    LBackends := LNodes.Field('backendNodeId');
    for LIndex := 0 to LBackends.Count - 1 do
    begin

      if LBackends.Item(LIndex).AsInteger = LBackend then
      begin
        LFound := True;
        Break;
      end;
    end;

    if LFound then
    begin
      Break;
    end;
  end;

  if not LFound then
  begin
    Exit;
  end;
  { Field returns a detached owned value. Cache each table once; copying a full
    node table inside every indexed iteration makes observation quadratic. }
  LBackends := LNodes.Field('backendNodeId');
  LNames := LNodes.Field('nodeName');
  LValues := LNodes.Field('inputValue');
  for LIndex := 0 to LBackends.Count - 1 do
  begin

    if (LBackends.Item(LIndex).AsInteger = LBackend) and
      (LStrings.Item(LNames.Item(LIndex).AsInteger).AsText = 'TEXTAREA') then
    begin
      { CDP exposes textarea values separately from INPUT values. Keep source
        editor observation exact without evaluating a getter or script. }
      LValues := LNodes.Field('textValue');
      Break;
    end;
  end;
  LIndexes := LValues.Field('index');
  LStringIndexes := LValues.Field('value');
  for LIndex := 0 to LIndexes.Count - 1 do
  begin
    LValueIndex := LIndexes.Item(LIndex).AsInteger;

    if LBackends.Item(LValueIndex).AsInteger = LBackend then
    begin
      LValueIndex := LStringIndexes.Item(LIndex).AsInteger;

      if LValueIndex < 0 then
      begin
        Exit(True);
      end;
      AValue := LStrings.Item(LValueIndex).AsText;
      Exit(True);
    end;
  end;
end;

procedure TNyxBrowserPipe.Reveal(const ASelector: TNyxText);
var
  LNode: Integer;
begin
  LNode := Request('DOM.querySelector', NyxObject([
    NyxField('nodeId', NyxData(Body)), NyxField('selector', NyxData(ASelector))]),
    FSession).Field('nodeId').AsInteger;

  if LNode = 0 then
  begin
    raise Exception.Create('Requested visual host face is absent');
  end;
  Request('DOM.scrollIntoViewIfNeeded', NyxObject([
    NyxField('nodeId', NyxData(LNode))]), FSession);
end;

function TNyxBrowserPipe.Bounds(const ASelector: TNyxText): TNyxBrowserBox;
var
  LNode: Integer;
  LQuad: TNyxDataValue;
  LIndex: Integer;
  LRight: Double;
  LBottom: Double;
  LX: Double;
  LY: Double;
begin
  Result := Default(TNyxBrowserBox);
  LNode := Request('DOM.querySelector', NyxObject([
    NyxField('nodeId', NyxData(Body)), NyxField('selector', NyxData(ASelector))]),
    FSession).Field('nodeId').AsInteger;

  if LNode = 0 then
  begin
    raise Exception.Create('Requested observed host face is absent');
  end;
  LQuad := Request('DOM.getBoxModel', NyxObject([
    NyxField('nodeId', NyxData(LNode))]), FSession).Field('model').Field('border');
  Result.Left := LQuad.Item(0).AsNumber;
  Result.Top := LQuad.Item(1).AsNumber;
  LRight := Result.Left;
  LBottom := Result.Top;
  for LIndex := 1 to 3 do
  begin
    LX := LQuad.Item(LIndex * 2).AsNumber;
    LY := LQuad.Item(LIndex * 2 + 1).AsNumber;

    if LX < Result.Left then
    begin
      Result.Left := LX;
    end;

    if LY < Result.Top then
    begin
      Result.Top := LY;
    end;

    if LX > LRight then
    begin
      LRight := LX;
    end;

    if LY > LBottom then
    begin
      LBottom := LY;
    end;
  end;
  Result.Width := LRight - Result.Left;
  Result.Height := LBottom - Result.Top;
end;

procedure TNyxBrowserPipe.Click(const ASelector: TNyxText; const AFrame: TNyxText);
var
  LNode: Integer;
  LQuad: TNyxDataValue;
  LX: Double;
  LY: Double;
  LPrepared: Boolean;
  LStarted: QWord;
  LRoot: Integer;
begin
  LStarted := GetTickCount64;
  repeat
    LPrepared := False;
    try
      LRoot := QueryRoot(AFrame);
      LNode := Request('DOM.querySelector', NyxObject([
        NyxField('nodeId', NyxData(LRoot)), NyxField('selector', NyxData(ASelector))]),
        FSession).Field('nodeId').AsInteger;

      if LNode <> 0 then
      begin
        Request('DOM.scrollIntoViewIfNeeded', NyxObject([
          NyxField('nodeId', NyxData(LNode))]), FSession);
        LQuad := Request('DOM.getBoxModel', NyxObject([
          NyxField('nodeId', NyxData(LNode))]), FSession).Field('model').Field('border');
        LPrepared := True;
      end;
    except
      on LError: Exception do
      begin
        { Observation can race an ordinary asynchronous chrome replacement.
          Retry only retired-node preparation, before any physical input has
          been sent. Never replay a mouse press or an editor command. }

        if Pos('Could not find node with given id', LError.Message) = 0 then
        begin
          raise;
        end;
      end;
    end;

    if not LPrepared then
    begin

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('Requested host control is absent or keeps retiring / ' + ASelector);
      end;
      Sleep(50);
    end;
  until LPrepared;
  LX := (LQuad.Item(0).AsNumber + LQuad.Item(4).AsNumber) / 2;
  LY := (LQuad.Item(1).AsNumber + LQuad.Item(5).AsNumber) / 2;
  Request('Input.dispatchMouseEvent', NyxObject([
    NyxField('type', NyxData('mousePressed')), NyxField('x', NyxData(LX)),
    NyxField('y', NyxData(LY)), NyxField('button', NyxData('left')),
    NyxField('clickCount', NyxData(1))]), FSession);
  Request('Input.dispatchMouseEvent', NyxObject([
    NyxField('type', NyxData('mouseReleased')), NyxField('x', NyxData(LX)),
    NyxField('y', NyxData(LY)), NyxField('button', NyxData('left')),
    NyxField('clickCount', NyxData(1))]), FSession);
end;

procedure TNyxBrowserPipe.Tab(AReverse: Boolean);
var
  LModifiers: Integer;
begin
  LModifiers := 0;

  if AReverse then
  begin
    LModifiers := 8;
  end;
  Request('Input.dispatchKeyEvent', NyxObject([
    NyxField('type', NyxData('keyDown')), NyxField('key', NyxData('Tab')),
    NyxField('code', NyxData('Tab')), NyxField('windowsVirtualKeyCode', NyxData(9)),
    NyxField('nativeVirtualKeyCode', NyxData(9)),
    NyxField('modifiers', NyxData(LModifiers))]), FSession);
  Request('Input.dispatchKeyEvent', NyxObject([
    NyxField('type', NyxData('keyUp')), NyxField('key', NyxData('Tab')),
    NyxField('code', NyxData('Tab')), NyxField('windowsVirtualKeyCode', NyxData(9)),
    NyxField('nativeVirtualKeyCode', NyxData(9)),
    NyxField('modifiers', NyxData(LModifiers))]), FSession);
end;

procedure TNyxBrowserPipe.Key(AKey: TNyxBrowserKey; AControl, AShift: Boolean);
const
  CNames: array[TNyxBrowserKey] of TNyxText =
    ('Home', 'End', 'ArrowUp', 'ArrowDown', 'Enter', 'Escape', 'a',
      'ArrowLeft', 'ArrowRight', 'F2');
  CCodes: array[TNyxBrowserKey] of TNyxText =
    ('Home', 'End', 'ArrowUp', 'ArrowDown', 'Enter', 'Escape', 'KeyA',
      'ArrowLeft', 'ArrowRight', 'F2');
  CVirtual: array[TNyxBrowserKey] of Integer = (36, 35, 38, 40, 13, 27, 65, 37, 39, 113);
var
  LModifiers: Integer;
begin
  LModifiers := 0;

  if (AKey = nbkSelectAll) or AControl then
  begin
    LModifiers := 2;
  end;

  if AShift then
  begin
    LModifiers := LModifiers or 8;
  end;
  Request('Input.dispatchKeyEvent', NyxObject([
    NyxField('type', NyxData('keyDown')), NyxField('key', NyxData(CNames[AKey])),
    NyxField('code', NyxData(CCodes[AKey])),
    NyxField('windowsVirtualKeyCode', NyxData(CVirtual[AKey])),
    NyxField('nativeVirtualKeyCode', NyxData(CVirtual[AKey])),
    NyxField('modifiers', NyxData(LModifiers))]), FSession);
  Request('Input.dispatchKeyEvent', NyxObject([
    NyxField('type', NyxData('keyUp')), NyxField('key', NyxData(CNames[AKey])),
    NyxField('code', NyxData(CCodes[AKey])),
    NyxField('windowsVirtualKeyCode', NyxData(CVirtual[AKey])),
    NyxField('nativeVirtualKeyCode', NyxData(CVirtual[AKey])),
    NyxField('modifiers', NyxData(LModifiers))]), FSession);
end;

procedure TNyxBrowserPipe.TypeText(const AText: TNyxText);
begin
  Request('Input.insertText', NyxObject([NyxField('text', NyxData(AText))]), FSession);
end;

procedure TNyxBrowserPipe.ReplaceText(const ASelector, AText: TNyxText);
begin
  Click(ASelector);
  Key(nbkSelectAll);
  TypeText(AText);
  Tab;
end;

procedure TNyxBrowserPipe.Resize(AWidth, AHeight: Integer);
begin

  if (AWidth < 320) or (AWidth > 4096) or (AHeight < 240) or (AHeight > 4096) then
  begin
    raise Exception.Create('Host allocation is outside the admitted bounds');
  end;
  Request('Emulation.setDeviceMetricsOverride', NyxObject([
    NyxField('width', NyxData(AWidth)), NyxField('height', NyxData(AHeight)),
    NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(False))]), FSession);
end;

procedure TNyxBrowserPipe.DragTouch(const ASelector: TNyxText;
  ADeltaX, ADeltaY: Double);
var
  LBox: TNyxBrowserBox;
  LIndex: Integer;
  LX: Double;
  LY: Double;

  procedure Touch(const AType: TNyxText; APosition: Double; AEnd: Boolean);
  var
    LPoints: TNyxDataValue;
  begin
    LPoints := NyxArray([]);

    if not AEnd then
    begin
      LPoints := NyxArray([NyxObject([
        NyxField('x', NyxData(LX + ADeltaX * APosition)),
        NyxField('y', NyxData(LY + ADeltaY * APosition)),
        NyxField('id', NyxData(1))])]);
    end;
    Request('Input.dispatchTouchEvent', NyxObject([
      NyxField('type', NyxData(AType)), NyxField('touchPoints', LPoints)]), FSession);
  end;
begin
  LBox := Bounds(ASelector);

  if (LBox.Width <= 0) or (LBox.Height <= 0) then
  begin
    raise Exception.Create('Touch target has no visible allocation');
  end;
  LX := LBox.Left + LBox.Width / 2;
  LY := LBox.Top + LBox.Height / 2;
  Touch('touchStart', 0, False);
  try
    for LIndex := 1 to 5 do
    begin
      Touch('touchMove', LIndex / 5, False);
    end;
    Touch('touchEnd', 1, True);
  except
    Touch('touchCancel', 0, True);
    raise;
  end;
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
  { Browser.close/owned-process retirement precedes reuse. The profile is
    borrowed and keeps its private storage for the next explicitly owned host. }

  if FProfile <> nil then
  begin
    FProfile.FInUse := False;
    FProfile := nil;
  end;
  ClosePipe(FInput);
  ClosePipe(FOutput);
  inherited Destroy;
end;

end.
