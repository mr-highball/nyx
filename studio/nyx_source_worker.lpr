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
  {$ifdef NYX_SOURCE_PROFILE}nyx.source,{$endif}
  nyx.text, nyx.data, nyx.schema, nyx.source.preparation, nyx.studio.session;

{$ifdef NYX_SOURCE_PROFILE}
var
  GProfileSeen: TNyxSourceProfileSamples;
  GProfileLines: Integer;
  GProfileChannel: TJSBroadcastChannel;
  GProfileName: JSValue;

{ A diagnostic-only BroadcastChannel is separate from result postMessage. Its
  name comes from this worker's URL, never project/source content or authority. }
procedure ProfileLine(const AText: TNyxText);
begin
  WriteLn(AText);

  if GProfileChannel <> nil then
  begin
    GProfileChannel.postMessage(AText);
  end;
end;

{ Isolated diagnostic worker only. Completed inclusive counters are written to
  the console, never to the strict result channel. A bounded log contains no
  source/schema bytes. Worker performance.now is monotonic within this owner. }
procedure ReportProfile;
var
  LSamples: TNyxSourceProfileSamples;
  LStage: TNyxSourceProfileStage;
begin
  LSamples := ReadNyxSourceProfile;
  for LStage := Low(TNyxSourceProfileStage) to High(TNyxSourceProfileStage) do
  begin

    if (LSamples[LStage].Calls <> GProfileSeen[LStage].Calls) and
      ((LSamples[LStage].Milliseconds >= 50) or (LStage >= spWorkerPacket)) and
      (GProfileLines < 256) then
    begin
      ProfileLine('stage=' + IntToStr(Ord(LStage)) + ' calls=' +
        IntToStr(LSamples[LStage].Calls) + ' milliseconds=' +
        FloatToStr(LSamples[LStage].Milliseconds) + ' at=' + FloatToStr(performance.now));
      Inc(GProfileLines);
    end;
  end;
  GProfileSeen := LSamples;
end;

function ProfileClock: Double;
begin
  Result := performance.now;
  ReportProfile;
end;
{$endif}

function Receive(AEvent: TJSEvent): Boolean;
var
  LRequest: TNyxDataValue;
  LReply: INyxPreparedSource;
  LDesign: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LDesignRequest: TNyxStudioDesignRequest;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  LReplyText: TNyxText;
  {$endif}
begin
  Result := False;
  {$ifdef NYX_SOURCE_PROFILE}
  ResetNyxSourceProfile(@ProfileClock);
  GProfileSeen := ReadNyxSourceProfile;
  GProfileLines := 0;
  LStarted := SourceProfileStart;
  ProfileLine('received at=' + FloatToStr(performance.now));
  {$endif}
  try

    if not isString(TJSMessageEvent(AEvent).data) then
    begin
      raise EArgumentException.Create('Source worker input must be a JSON text packet');
    end;
    LRequest := TNyxDataValue.ParseJSON(String(TJSMessageEvent(AEvent).data));

    if (LRequest.Kind <> ndObject) or (LRequest.Count <> 3) then
    begin
      raise EArgumentException.Create('Source worker requires its versioned source/schema packet');
    end;
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spWorkerPacket, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    LSchemas := ReadNyxSchemaSnapshot(LRequest.Field('schemas'));
    {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spWorkerSchemas, LStarted);{$endif}
    case LRequest.Field('version').AsInteger of
      1:
        begin
          LReply := PrepareNyxSource(LRequest.Field('source').AsText, LSchemas);
          WebWorker.Self_.postMessage(LReply.ToData.ToJSON);
        end;
      2:
        begin
          {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
          LDesignRequest := ReadNyxStudioDesignRequest(LRequest.Field('request'));
          {$ifdef NYX_SOURCE_PROFILE}
          SourceProfileFinish(spWorkerRequest, LStarted);
          LStarted := SourceProfileStart;
          {$endif}
          LDesign := PrepareNyxStudioDesign(LDesignRequest, LSchemas);
          {$ifdef NYX_SOURCE_PROFILE}
          SourceProfileFinish(spWorkerPreparation, LStarted);
          LStarted := SourceProfileStart;
          LReplyText := LDesign.ToData.ToJSON;
          SourceProfileFinish(spWorkerReply, LStarted);
          ReportProfile;
          WebWorker.Self_.postMessage(LReplyText);
          {$else}
          WebWorker.Self_.postMessage(LDesign.ToData.ToJSON);
          {$endif}
        end;
    else
      begin
        raise EArgumentException.Create('Editor worker request version is unsupported');
      end;
    end;
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
  {$ifdef NYX_SOURCE_PROFILE}
  GProfileName := TJSURLSearchParams.new(String(WebWorker.location.search)).get('nyx-profile');

  if isString(GProfileName) and (String(GProfileName) <> '') then
  begin
    GProfileChannel := TJSBroadcastChannel.new(String(GProfileName));
  end;
  ProfileLine('boot at=' + FloatToStr(performance.now) +
    ' origin=' + FloatToStr(performance.timeOrigin));
  {$endif}
  WebWorker.Self_.addEventListener('message', @Receive);
end.
