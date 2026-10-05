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

unit nyx.studio.exchange.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  JS, Web, nyx.text, nyx.studio.exchange;

type
  { Browser adapter for the shared editor protocol. Same-origin XHR supplies its
    Origin; this adapter owns requests/timers and borrows notification receivers. }
  TNyxBrowserEditorExchange = class(TNyxStudioEditorExchange)
  private
    FRequest: TJSXMLHttpRequest;
    FReply: TNyxEditorReply;
    FTick: TNyxEditorTick;
    FTimer: NativeInt;
    procedure Ready;
    procedure Tick;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
  end;

implementation

uses
  SysUtils;

constructor TNyxBrowserEditorExchange.Create;
begin
  inherited Create;
  FTimer := -1;
end;

destructor TNyxBrowserEditorExchange.Destroy;
begin
  CancelTick;
  CancelRequest;
  inherited Destroy;
end;

procedure TNyxBrowserEditorExchange.CancelRequest;
begin
  FReply := nil;

  if FRequest <> nil then
  begin
    FRequest.onreadystatechange := nil;
    FRequest.abort;
    FRequest := nil;
  end;
end;

procedure TNyxBrowserEditorExchange.Post(AConnect: Boolean;
  const AToken, ABody: TNyxText; AReply: TNyxEditorReply);
var
  LURL: TNyxText;
begin

  if FRequest <> nil then
  begin
    raise Exception.Create('An editor request is already pending');
  end;
  FReply := AReply;
  FRequest := TJSXMLHttpRequest.new;
  FRequest.onreadystatechange := @Ready;
  LURL := 'api/agents';

  if AConnect then
  begin
    LURL := 'api/agents/connect';
  end;
  FRequest.open('POST', LURL, True);
  FRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');

  if not AConnect then
  begin
    FRequest.setRequestHeader('X-Nyx-Editor', AToken);
  end;
  FRequest.send(ABody);
end;

procedure TNyxBrowserEditorExchange.Ready;
var
  LStatus: Integer;
  LText: TNyxText;
  LReply: TNyxEditorReply;
begin

  if (FRequest = nil) or (FRequest.readyState <> 4) then
  begin
    Exit;
  end;
  LStatus := FRequest.status;
  LText := FRequest.responseText;
  FRequest.onreadystatechange := nil;
  FRequest := nil;
  LReply := FReply;
  FReply := nil;

  if Assigned(LReply) then
  begin
    LReply(LStatus, LText);
  end;
end;

procedure TNyxBrowserEditorExchange.CancelTick;
begin

  if FTimer >= 0 then
  begin
    window.clearTimeout(FTimer);
  end;
  FTimer := -1;
  FTick := nil;
end;

procedure TNyxBrowserEditorExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  CancelTick;
  FTick := ATick;
  FTimer := window.setTimeout(@Tick, ADelayMS);
end;

procedure TNyxBrowserEditorExchange.Tick;
var
  LTick: TNyxEditorTick;
begin
  FTimer := -1;
  LTick := FTick;
  FTick := nil;

  if Assigned(LTick) then
  begin
    LTick;
  end;
end;

end.
