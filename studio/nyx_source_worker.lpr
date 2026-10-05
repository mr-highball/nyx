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


program nyx_source_worker;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, WebWorker, WebOrWorker,
  nyx.text, nyx.data, nyx.schema, nyx.source.preparation;

function Receive(AEvent: TJSEvent): Boolean;
var
  LRequest: TNyxDataValue;
  LReply: INyxPreparedSource;
  LSchemas: INyxSchemaSnapshot;
begin
  Result := False;
  try

    if not isString(TJSMessageEvent(AEvent).data) then
    begin
      raise EArgumentException.Create('Source worker input must be a JSON text packet');
    end;
    LRequest := TNyxDataValue.ParseJSON(String(TJSMessageEvent(AEvent).data));

    if (LRequest.Kind <> ndObject) or (LRequest.Count <> 3) or
      (LRequest.Field('version').AsInteger <> 1) then
    begin
      raise EArgumentException.Create('Source worker requires its versioned source/schema packet');
    end;
    LSchemas := ReadNyxSchemaSnapshot(LRequest.Field('schemas'));
    LReply := PrepareNyxSource(LRequest.Field('source').AsText, LSchemas);
    WebWorker.Self_.postMessage(LReply.ToData.ToJSON);
  except
    on LException: Exception do
    begin
      { Protocol/startup failures are distinct from positioned user diagnostics.
        The host must reject this reply and retain its own accepted pair/draft. }
      WebWorker.Self_.postMessage(NyxObject([
        NyxField('version', NyxData(1)),
        NyxField('protocolError', NyxData(TNyxText(LException.Message)))]).ToJSON);
    end;
  end;
end;

begin
  { Built with the matched RTL embedded and -Tmodule for its generated rtl.run
    entry. This ordinary Pascal program creates no DOM/Studio/server and accepts
    no filesystem/URL/compiler instruction. Each request reconstructs fresh owners. }
  WebWorker.Self_.addEventListener('message', @Receive);
end.
