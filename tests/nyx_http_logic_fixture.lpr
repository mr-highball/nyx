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

program nyx_http_logic_fixture;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, fphttpserver, httpdefs, nyx.text;

type
  { Owned loopback-only qualification producer. Fixed service routes and four
    whitelisted staged files expose no Studio, compiler, credentials or project.
    An idle-turn stop lets the final HTTP response finish before retirement. }
  TFixture = class
  private
    FServer: TFPHTTPServer;
    FRoot: String;
    FStop: LongInt;
    procedure Request(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure Idle(Sender: TObject);
  public
    procedure Run;
  end;

procedure TFixture.Idle(Sender: TObject);
begin

  if InterlockedCompareExchange(FStop, 0, 0) <> 0 then
  begin
    FServer.Active := False;
  end;
end;

procedure TFixture.Request(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  LName: String;
begin
  AResponse.ContentType := 'application/octet-stream';
  AResponse.CustomHeaders.Values['X-Nyx-Reply'] := 'owned-http-fixture';
  AResponse.CustomHeaders.Values['Cache-Control'] := 'no-store';

  if ARequest.URI = '/finish' then
  begin
    AResponse.Content := 'finished';
    InterlockedExchange(FStop, 1);
    Exit;
  end;

  if ARequest.URI = '/slow' then
  begin
    Sleep(500);
    AResponse.Content := 'late response';
    Exit;
  end;

  if ARequest.URI = '/status' then
  begin
    AResponse.Code := 422;
    AResponse.Content := 'Please check the input.';
    Exit;
  end;

  if ARequest.URI = '/json' then
  begin
    AResponse.ContentType := 'application/json';
    AResponse.Content := '{"items":[{"id":"a","active":true,"rank":9007199254740993},' +
      '{"id":"b","active":false,"rank":1}],"caption":"Hello from HTTP"}';
    Exit;
  end;

  if ARequest.URI = '/head' then
  begin
    AResponse.Content := 'representation has length';
    Exit;
  end;

  if ARequest.URI = '/echo' then
  begin
    AResponse.Content := ARequest.Method + #10 +
      ARequest.CustomHeaders.Values['X-Nyx-Input'] + #10 + ARequest.Content;
    Exit;
  end;
  LName := Copy(ARequest.URI, 2, MaxInt);

  if (LName = 'rtl.js') or (LName = 'nyx_http_logic_tests.js') or
    (LName = 'nyx_async_flow_tests.js') or (LName = 'index.html') then
  begin

    if LName = 'index.html' then
    begin
      AResponse.ContentType := 'text/html; charset=utf-8';
      LName := 'logic-http-network.html';
    end
    else
    begin
      AResponse.ContentType := 'text/javascript; charset=utf-8';
    end;
    AResponse.ContentStream := TFileStream.Create(FRoot + LName, fmOpenRead or fmShareDenyNone);
    AResponse.FreeContentStream := True;
    Exit;
  end;
  AResponse.Code := 404;
  AResponse.Content := 'Unknown qualification route';
end;

procedure TFixture.Run;
var
  LPort: Integer;
begin

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Supply an unused loopback port and owned staged directory');
  end;
  LPort := StrToInt(ParamStr(1));

  if (LPort < 1024) or (LPort > 65534) then
  begin
    raise Exception.Create('Qualification port is out of range');
  end;
  FRoot := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));

  if not FileExists(FRoot + 'logic-http-network.html') then
  begin
    raise Exception.Create('Qualification requires its staged entry page');
  end;
  FServer := TFPHTTPServer.Create(nil);
  try
    FServer.Address := '127.0.0.1';
    FServer.Port := LPort;
    FServer.ThreadMode := tmThread;
    FServer.AcceptIdleTimeout := 50;
    FServer.RequestReadTimeout := 5000;
    FServer.OnRequest := Request;
    FServer.OnAcceptIdle := Idle;
    FServer.Active := True;
  finally
    FServer.Free;
  end;
end;

var
  LFixture: TFixture;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Run;
  finally
    LFixture.Free;
  end;
end.
