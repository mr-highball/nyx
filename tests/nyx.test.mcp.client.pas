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

unit nyx.test.mcp.client;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, fphttpclient, nyx.text, nyx.data;

type
  { Native Pascal protocol consumer. Reads credentials privately from the local
    generated Codex entry; no token is emitted into logs or test page URLs. }
  TNyxMCPTestClient = class
  private
    FEndpoint: TNyxText;
    FAuthorization: TNyxText;
    FSession: TNyxText;
    FSerial: Integer;
  public
    constructor Create(const AConfiguration: TNyxText;
      const AClientName: TNyxText = 'Scooty protocol fixture');
    function Exchange(const AMethod: TNyxText; const APacket: TNyxDataValue;
      AExpectedStatus: Integer = 200; const AOrigin: TNyxText = ''): TNyxDataValue;
    function RPC(const AMethod: TNyxText; const AParams: TNyxDataValue): TNyxDataValue;
    function Tool(const AName: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
    procedure Close;
  end;

{ Internal editor protocol fixture. Origin and its independent capability are
  explicit. Tests never expose this operator API as an agent/MCP tool. }
function NyxTestEditorExchange(const ABase, APath, AToken: TNyxText;
  const ARequest: TNyxDataValue; AStatus: Integer = 200): TNyxDataValue;

implementation

function Request(const AURL, AMethod, AAuthorization, ASession, AOrigin,
  AEditorToken: TNyxText; const APacket: TNyxDataValue; AExpectedStatus: Integer;
  out AReturnedSession: TNyxText): TNyxDataValue;
var
  LClient: TFPHTTPClient;
  LBody: TMemoryStream;
  LReply: TMemoryStream;
  LText: TNyxText;
begin
  LClient := TFPHTTPClient.Create(nil);
  LBody := TMemoryStream.Create;
  LReply := TMemoryStream.Create;
  try
    LClient.IOTimeout := 30000;
    LClient.AllowRedirect := False;
    LClient.AddHeader('Content-Type', 'application/json');
    LClient.AddHeader('Accept', 'application/json, text/event-stream');

    if AAuthorization <> '' then
    begin
      LClient.AddHeader('Authorization', AAuthorization);
    end;

    if ASession <> '' then
    begin
      LClient.AddHeader('Mcp-Session-Id', ASession);
      LClient.AddHeader('MCP-Protocol-Version', '2025-11-25');
    end;

    if AOrigin <> '' then
    begin
      LClient.AddHeader('Origin', AOrigin);
    end;

    if AEditorToken <> '' then
    begin
      LClient.AddHeader('X-Nyx-Editor', AEditorToken);
    end;
    LText := APacket.ToJSON;

    if AMethod = 'POST' then
    begin
      LBody.WriteBuffer(LText[1], Length(LText));
      LBody.Position := 0;
      LClient.RequestBody := LBody;
    end;
    LClient.HTTPMethod(AMethod, AURL, LReply, [200, 202, 400, 401, 403, 404, 405, 413, 415]);

    if LClient.ResponseStatusCode <> AExpectedStatus then
    begin
      raise Exception.Create('Unexpected protocol status: ' + IntToStr(LClient.ResponseStatusCode));
    end;
    AReturnedSession := TNyxText(LClient.GetHeader(LClient.ResponseHeaders, 'Mcp-Session-Id'));

    if LReply.Size = 0 then
    begin
      Exit(NyxNull);
    end;
    SetLength(LText, LReply.Size);
    SetCodePage(RawByteString(LText), CP_UTF8, False);
    LReply.Position := 0;
    LReply.ReadBuffer(LText[1], LReply.Size);
    Result := TNyxDataValue.ParseJSON(LText);
  finally
    LClient.Free;
    LReply.Free;
    LBody.Free;
  end;
end;

constructor TNyxMCPTestClient.Create(const AConfiguration: TNyxText;
  const AClientName: TNyxText);
var
  LFile: TFileStream;
  LText: TNyxText;
  LLines: TNyxStrings;
  LIndex: Integer;
  LLine: TNyxText;
  LStart: Integer;
  LResponse: TNyxDataValue;
  LPortEnd: Integer;
  LPort: Integer;
begin
  inherited Create;
  LFile := TFileStream.Create(AConfiguration, fmOpenRead or fmShareDenyNone);
  LLines := TNyxStrings.Create;
  try
    SetLength(LText, LFile.Size);

    if LFile.Size > 0 then
    begin
      LFile.ReadBuffer(LText[1], LFile.Size);
    end;
    LLines.Text := LText;
    LStart := 0;
    for LIndex := 0 to LLines.Count - 1 do
    begin
      LLine := Trim(LLines[LIndex]);

      if LLine = '[mcp_servers.nyx_studio]' then
      begin
        LStart := 1;
      end
      else if (LStart = 1) and (Pos('url = "', LLine) = 1) then
      begin
        FEndpoint := Copy(LLine, 8, Length(LLine) - 8);
      end
      else if (LStart = 1) and (Pos('http_headers = { Authorization = "', LLine) = 1) then
      begin
        FAuthorization := Copy(LLine, 35, Length(LLine) - 37);
      end
      else if (LStart = 1) and (Pos('[', LLine) = 1) then
      begin
        Break;
      end;
    end;
  finally
    LLines.Free;
    LFile.Free;
  end;

  if (FEndpoint = '') or (FAuthorization = '') then
  begin
    raise Exception.Create('Local Nyx Codex entry is missing');
  end;
  { This consumer is deliberately local. Never forward a private generated
    credential to a URL changed to a remote host or a redirect by configuration. }
  LPortEnd := Pos('/mcp/', FEndpoint);

  if (Pos('http://127.0.0.1:', FEndpoint) <> 1) or (LPortEnd < 19) or
    not TryStrToInt(Copy(FEndpoint, 18, LPortEnd - 18), LPort) or
    (LPort < 1024) or (LPort > 65535) or
    (Pos('Bearer ', FAuthorization) <> 1) or
    (Pos(#13, FAuthorization) > 0) or (Pos(#10, FAuthorization) > 0) then
  begin
    raise Exception.Create('Nyx MCP requires its generated authenticated loopback endpoint');
  end;
  LResponse := RPC('initialize', NyxObject([
    NyxField('protocolVersion', NyxData('2025-11-25')),
    NyxField('capabilities', NyxObject([])),
    NyxField('clientInfo', NyxObject([NyxField('name', NyxData(AClientName)),
      NyxField('version', NyxData('1'))]))]));

  if LResponse.Field('result').Field('protocolVersion').AsText <> '2025-11-25' then
  begin
    raise Exception.Create('MCP protocol negotiation failed');
  end;
  Exchange('POST', NyxObject([NyxField('jsonrpc', NyxData('2.0')),
    NyxField('method', NyxData('notifications/initialized'))]), 202);
end;

function TNyxMCPTestClient.Exchange(const AMethod: TNyxText;
  const APacket: TNyxDataValue; AExpectedStatus: Integer;
  const AOrigin: TNyxText): TNyxDataValue;
var
  LReturned: TNyxText;
begin
  Result := Request(FEndpoint, AMethod, FAuthorization, FSession,
    AOrigin, '', APacket, AExpectedStatus, LReturned);

  if LReturned <> '' then
  begin
    FSession := LReturned;
  end;
end;

function TNyxMCPTestClient.RPC(const AMethod: TNyxText;
  const AParams: TNyxDataValue): TNyxDataValue;
begin
  Inc(FSerial);
  Result := Exchange('POST', NyxObject([NyxField('jsonrpc', NyxData('2.0')),
    NyxField('id', NyxData(FSerial)), NyxField('method', NyxData(AMethod)),
    NyxField('params', AParams)]));
end;

function TNyxMCPTestClient.Tool(const AName: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := RPC('tools/call', NyxObject([NyxField('name', NyxData(AName)),
    NyxField('arguments', AArguments)])).Field('result');
end;

procedure TNyxMCPTestClient.Close;
begin
  Exchange('DELETE', NyxObject([]));
  FSession := '';
end;

function NyxTestEditorExchange(const ABase, APath, AToken: TNyxText;
  const ARequest: TNyxDataValue; AStatus: Integer): TNyxDataValue;
var
  LReturned: TNyxText;
begin
  Result := Request(ABase + APath, 'POST', '', '', ABase, AToken,
    ARequest, AStatus, LReturned);
end;

end.
