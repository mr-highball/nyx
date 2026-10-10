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
program nyx_preview_host;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, fphttpserver, httpdefs, nyx.text;

type
  { An optional reserved test hostname permits actual untrustworthy-origin cache
    qualification while the listener remains bound to loopback. Default hosting
    admits only its exact loopback Host header. No arbitrary aliases are added. }
  TPreviewOrigin = (poLoopback, poUntrustworthyFixture);
  { A read-only loopback host for already compiled preview/test artifacts.
    It owns the HTTP server; response streams are transferred to the response.
    Only flat HTML/JavaScript/CSS/JSON/PNG/JPEG files beneath the supplied directory are
    served. It has no compiler, editor, MCP endpoint, enrollment or upload API. }
  TPreviewHost = class
  private
    FServer: TFPHTTPServer;
    FDirectory: TNyxText;
    FHost: TNyxText;
    FFixtureHost: TNyxText;
    procedure Request(ASender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(const ADirectory: TNyxText; APort: Integer;
      AOrigin: TPreviewOrigin);
    destructor Destroy; override;
    { Runs until the owning process is stopped. Callers must verify its process
      identity before retirement; this program does not stop another service. }
    procedure Run;
  end;

constructor TPreviewHost.Create(const ADirectory: TNyxText; APort: Integer;
  AOrigin: TPreviewOrigin);
begin
  inherited Create;

  if (APort < 1024) or (APort > 65535) then
  begin
    raise Exception.Create('Preview port must be in 1024..65535');
  end;
  FDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory));

  if not DirectoryExists(FDirectory) then
  begin
    raise Exception.Create('Preview directory must already exist');
  end;
  FHost := '127.0.0.1:' + IntToStr(APort);

  if AOrigin = poUntrustworthyFixture then
  begin
    FFixtureHost := 'nyx-cache.test:' + IntToStr(APort);
  end;
  FServer := TFPHTTPServer.Create(nil);
  FServer.Address := '127.0.0.1';
  FServer.Port := APort;
  FServer.Threaded := False;
  FServer.OnRequest := Request;
end;

destructor TPreviewHost.Destroy;
begin
  FServer.Free;
  inherited Destroy;
end;

procedure TPreviewHost.Request(ASender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  LName: TNyxText;
  LExtension: TNyxText;
  LIndex: Integer;
begin
  AResponse.SetCustomHeader('Cache-Control', 'no-store');
  AResponse.SetCustomHeader('X-Content-Type-Options', 'nosniff');

  if (ARequest.Host <> FHost) and
    ((FFixtureHost = '') or (ARequest.Host <> FFixtureHost)) then
  begin
    AResponse.Code := 403;
    Exit;
  end;

  if (ARequest.Method <> 'GET') and (ARequest.Method <> 'HEAD') then
  begin
    AResponse.Code := 405;
    AResponse.SetCustomHeader('Allow', 'GET, HEAD');
    Exit;
  end;
  LName := Copy(ARequest.PathInfo, 2, MaxInt);

  if (LName = '') or (Pos('..', LName) > 0) then
  begin
    AResponse.Code := 404;
    Exit;
  end;
  { Strict ASCII basenames avoid separators, percent escapes, alternate streams,
    query/path reinterpretation and native ANSI filesystem ambiguity. }
  for LIndex := 1 to Length(LName) do
  begin

    if not (LName[LIndex] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.']) then
    begin
      AResponse.Code := 404;
      Exit;
    end;
  end;
  LExtension := ExtractFileExt(LName);

  if LExtension = '.html' then
  begin
    AResponse.ContentType := 'text/html; charset=utf-8';
  end
  else if LExtension = '.js' then
  begin
    AResponse.ContentType := 'application/javascript; charset=utf-8';
  end
  else if LExtension = '.css' then
  begin
    AResponse.ContentType := 'text/css; charset=utf-8';
  end
  else if LExtension = '.json' then
  begin
    AResponse.ContentType := 'application/json; charset=utf-8';
  end
  else if LExtension = '.png' then
  begin
    AResponse.ContentType := 'image/png';
  end
  else if (LExtension = '.jpg') or (LExtension = '.jpeg') then
  begin
    AResponse.ContentType := 'image/jpeg';
  end
  else
  begin
    AResponse.Code := 404;
    Exit;
  end;

  if not FileExists(FDirectory + LName) then
  begin
    AResponse.Code := 404;
    Exit;
  end;
  AResponse.ContentStream := TFileStream.Create(FDirectory + LName,
    fmOpenRead or fmShareDenyNone);
  AResponse.FreeContentStream := True;
end;

procedure TPreviewHost.Run;
begin
  FServer.Active := True;
end;

var
  LHost: TPreviewHost;
  LPort: Integer;
  LOrigin: TPreviewOrigin;

begin
  LHost := nil;
  try

    if not (ParamCount in [2, 3]) or not TryStrToInt(ParamStr(2), LPort) then
    begin
      raise Exception.Create('Usage: nyx_preview_host <staged-directory> <loopback-port> [--untrustworthy-origin]');
    end;
    LOrigin := poLoopback;

    if ParamCount = 3 then
    begin

      if ParamStr(3) <> '--untrustworthy-origin' then
      begin
        raise Exception.Create('Unknown preview host option');
      end;
      LOrigin := poUntrustworthyFixture;
    end;
    LHost := TPreviewHost.Create(ParamStr(1), LPort, LOrigin);
    LHost.Run;
  except
    on LException: Exception do
    begin
      WriteLn('Preview host: ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LHost.Free;
end.
