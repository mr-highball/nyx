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


program nyx_source_projection_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, Web, nyx.text, nyx.data, nyx.studio.builds,
  nyx.studio.sourceprojection, nyx.test.projection;

var
  GRequest: TJSXMLHttpRequest;
  GManifest: TNyxDataValue;
  GSource: array[0..2] of TNyxText;
  GExpected: TNyxText;
  GWorker: TJSWorker;
  GTimeout: NativeInt;
  GLoadStep: Integer;
  GWorkerIndex: Integer;
  GChecks: Integer;

procedure RetireWorker;
begin

  if GTimeout <> 0 then
  begin
    window.clearTimeout(GTimeout);
    GTimeout := 0;
  end;

  if GWorker <> nil then
  begin
    GWorker.terminate;
    GWorker := nil;
  end;
end;

procedure Failed(const AReason: TNyxText);
begin
  RetireWorker;
  document.body.setAttribute('data-source-projection', 'failed');
  document.body.setAttribute('data-event-error', AReason);
  document.body.textContent := 'FAIL source projection: ' + AReason;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure StartWorker; forward;

function Receive(AEvent: TJSEvent): Boolean;
var
  LReference: TNyxSourceProjectionRef;
  LWire: TNyxText;
  LProjection: INyxSourceProjection;
  LState: TNyxSourceProjectionState;
begin
  Result := False;
  try
    Check(isString(TJSMessageEvent(AEvent).data), 'worker returned exact JSON text');
    LWire := TNyxText(TJSMessageEvent(AEvent).data);
    LReference := NyxSourceProjectionRef(
      GManifest.Field('workers').Item(GWorkerIndex).Field('ticket').AsText);

    if GWorkerIndex = 0 then
    begin
      GChecks := GChecks + RunNyxProjectionPacketChecks(GSource[0], LReference,
        btBrowser, LWire, GExpected);
    end
    else
    begin
      LState := spsExecutionFailed;

      if GWorkerIndex = 2 then
      begin
        LState := spsInvalidDesign;
      end;
      LProjection := ReceiveNyxSourceProjection(GSource[GWorkerIndex], LReference,
        btBrowser, LWire);
      Check((LProjection.State = LState) and (LProjection.Design = '') and
        (LProjection.Source = GSource[GWorkerIndex]),
        'actual browser throwing/nil constructor is unusable');
    end;
    RetireWorker;
    Inc(GWorkerIndex);
    StartWorker;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function WorkerError(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  Failed('The actual projection worker failed to load or execute');
end;

procedure WorkerTimeout;
begin
  Failed('The actual projection worker exceeded its execution deadline');
end;

procedure StartWorker;
begin

  if GWorkerIndex = 3 then
  begin
    document.body.textContent := 'PASS source projection workers ' + IntToStr(GChecks);
    document.body.setAttribute('data-source-projection', 'passed');
    Exit;
  end;
  document.body.setAttribute('data-source-projection-step', IntToStr(GWorkerIndex));
  { The owned artifact is a complete Pascal worker with an embedded matching RTL.
    Each worker is physically terminated before the next invocation starts. }
  GWorker := TJSWorker.new(
    GManifest.Field('workers').Item(GWorkerIndex).Field('artifact').AsText);
  GWorker.addEventListener('message', @Receive);
  GWorker.addEventListener('error', @WorkerError);
  GTimeout := window.setTimeout(@WorkerTimeout, 10000);
end;

procedure LoadNext; forward;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'exact current-source fixture loaded over HTTP');
    case GLoadStep of
      0:
        begin
          GManifest := TNyxDataValue.ParseJSON(GRequest.responseText);
          Check((GManifest.Field('version').AsInteger = 1) and
            (GManifest.Field('workers').Count = 3), 'complete owned worker manifest');
        end;
      1:
        begin
          GSource[0] := GRequest.responseText;
        end;
      2:
        begin
          GExpected := GRequest.responseText;
          Check(GExpected = ExpectedNyxProjectionDesign,
            'browser independent literal expected meaning matches native');
        end;
      3:
        begin
          GSource[1] := GRequest.responseText;
        end;
      4:
        begin
          GSource[2] := GRequest.responseText;
        end;
    end;
    Inc(GLoadStep);
    LoadNext;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function LoadError(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  Failed('Owned projection HTTP input did not arrive');
end;

procedure LoadNext;
const
  CFiles: array[0..4] of String = ('projection.json', 'source.pas', 'expected.nyx',
    'throw.pas', 'nil.pas');
begin

  if GLoadStep = 5 then
  begin
    StartWorker;
    Exit;
  end;
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', CFiles[GLoadStep], True);
  GRequest.timeout := 10000;
  GRequest.onload := @Loaded;
  GRequest.onerror := @LoadError;
  GRequest.ontimeout := @LoadError;
  GRequest.send;
end;

begin
  document.body.setAttribute('data-source-projection', 'pending');
  LoadNext;
end.
