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
program nyx_source_worker_replay;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, WebWorker, WebOrWorker, nyx.text, nyx.data,
  nyx.schema, nyx.source.preparation, nyx.studio.worker.transport;

type
  { Worker-owned timer packet contains copied result text only. A terminated
    realm retires its timer; no Studio, document or UI receiver is borrowed. }
  TDelayedReply = class
  public
    Packet: TNyxSourceWorkerPacket;
    procedure Send;
  end;

var
  GPrevious: TNyxSourceWorkerPacket;
  GReceived: Integer;
  { Separate test-only observation acknowledges a fully prepared result whose
    timer has been armed. It carries no source/schema or publication authority. }
  GReadyChannel: TJSBroadcastChannel;
  GReadyName: JSValue;

procedure TDelayedReply.Send;
begin
  WebWorker.Self_.postMessage(Packet.ToValue);
  Free;
end;

function Receive(AEvent: TJSEvent): Boolean;
var
  LPacket: TNyxSourceWorkerPacket;
  LRequest: TNyxDataValue;
  LPrepared: INyxPreparedSource;
  LSchemas: INyxSchemaSnapshot;
  LDelayed: TDelayedReply;
begin
  Result := True;
  LPacket := TNyxSourceWorkerPacket.Read(TJSMessageEvent(AEvent).data);
  LRequest := TNyxDataValue.ParseJSON(LPacket.Text);

  if (LRequest.Count <> 3) or (LRequest.Field('version').AsInteger <> 1) then
  begin
    raise Exception.Create('Replay helper requires the unchanged source/schema request');
  end;
  LSchemas := ReadNyxSchemaSnapshot(LRequest.Field('schemas'));
  LPrepared := PrepareNyxSource(LRequest.Field('source').AsText, LSchemas);
  LPacket := LPacket.WithText(LPrepared.ToData.ToJSON);

  if GReceived > 0 then
  begin
    { Intentionally malformed stale text must be rejected by identity BEFORE
      logical JSON admission. It cannot cancel this newer live request. }
    WebWorker.Self_.postMessage(GPrevious.WithText('{malformed stale result').ToValue);
  end;
  Inc(GReceived);
  GPrevious := LPacket;

  if Pos('{ Delay worker reply }', LRequest.Field('source').AsText) > 0 then
  begin
    LDelayed := TDelayedReply.Create;
    LDelayed.Packet := LPacket;
    WebWorker.Self_.setTimeout(@LDelayed.Send, 1000);

    if GReadyChannel <> nil then
    begin
      GReadyChannel.postMessage(LPacket.WithText('delayed-ready').ToValue);
    end;
    Exit;
  end;
  WebWorker.Self_.postMessage(LPacket.ToValue);
  { A matching duplicate must never produce a second paired publication. }
  WebWorker.Self_.postMessage(LPacket.ToValue);
end;

begin
  GReadyName := TJSURLSearchParams.new(String(WebWorker.location.search)).get('nyx-review');

  if isString(GReadyName) and (String(GReadyName) <> '') then
  begin
    GReadyChannel := TJSBroadcastChannel.new(String(GReadyName));
  end;
  WebWorker.Self_.addEventListener('message', @Receive);
end.
