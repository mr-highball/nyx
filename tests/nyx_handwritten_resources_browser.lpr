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

program nyx_handwritten_resources_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.source,
  nyx.resources, nyx.composition, nyx.binding, nyx.studio.builds,
  nyx.studio.sourceprojection;

const
  CFiles: array[0..4] of TNyxText = ('resource-base-source.pas',
    'resource-base-design.json', 'resource-source.pas', 'resource-design.json',
    'resource-worker.json');
  CText: TNyxText = 'A crafted resource 🚀 𐐷 é.';

var
  GRequest: TJSXMLHttpRequest;
  GWorker: TJSWorker;
  GTimeout: NativeInt;
  GStep: Integer;
  GChecks: Integer;
  GInputs: array[0..4] of TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Retire;
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
  Retire;
  document.body.textContent := 'FAIL handwritten resources: ' + AReason;
  document.body.setAttribute('data-test-result', 'failed');
  document.body.setAttribute('data-event-error', AReason);
end;

function Received(AEvent: TJSEvent): Boolean;
var
  LManifest: TNyxDataValue;
  LProjection: INyxSourceProjection;
  LDocument: TNyxDocument;
  LView: TNyxNode;
begin
  Result := False;
  LDocument := nil;
  LView := nil;
  try
    try
      Check(isString(TJSMessageEvent(AEvent).data), 'worker returns exact JSON text');
      LManifest := TNyxDataValue.ParseJSON(GInputs[4]);
      LProjection := ReceiveNyxSourceProjection(GInputs[2],
        NyxSourceProjectionRef(LManifest.Field('reference').AsText), btBrowser,
        TNyxText(TJSMessageEvent(AEvent).data));
      Check((LProjection.State = spsExecuted) and (LProjection.Design = GInputs[3]) and
        (LProjection.Source = GInputs[2]), 'actual compiled browser constructor reproduces the entire pair');
      LDocument := LProjection.CopyDocument;
      Check(LDocument.Resources.Definition(NyxResourceRef('structured'), NyxDefaultLocale)
        .Data.Field('literal.path').Item(0).Field('exact').AsDecimal.Text = '9007199254740993',
        'executed JSON retains exact numeric spelling beyond safe integer precision');
      Check(LDocument.Resources.Definition(NyxResourceRef('copy|name: 🚀'), NyxDefaultLocale)
        .Text = CText + #0 + 'Exact bytes', 'packed Unicode/NUL and open identity survive execution');
      Check(NyxResourceLabelsOf(LDocument.Resources.Definition(NyxResourceRef('copy'),
        NyxLocale('en-US'))).Item(0).Name = 'Reviewed', 'locale labels survive complete execution');
      LView := RealizeNyxView(LDocument, LDocument.Find('notebook-1'));
      ApplyNyxBindings(LView, LDocument.State);
      Check(LView.Find('heading-1').Prop('text') = CText,
        'literal JSON path projects the executed resource into the realized heading');
      Check(LView.Find('resource-image').Prop('src') <> '',
        'packed image resource projects through its distinct typed binding');
      FreeAndNil(LView);
      LView := RealizeNyxView(LDocument, LDocument.Find('notebook-2'));
      ApplyNyxBindings(LView, LDocument.State);
      Check(LView.Find('heading-2').Prop('text') = CText,
        'explicit English locale projects through the same resource contract');
      Retire;
      document.body.textContent := 'PASS handwritten resources / ' + IntToStr(GChecks) + ' checks';
      document.body.setAttribute('data-test-result', 'passed');
    except
      on LException: Exception do
      begin
        Failed(LException.Message);
      end;
    end;
  finally
    LView.Free;
    LDocument.Free;
  end;
end;

function WorkerFailed(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  Failed('Actual compiled resource constructor could not execute');
end;

procedure TimedOut;
begin
  Failed('Actual browser constructor exceeded ten seconds');
end;

procedure Execute;
var
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LManifest: TNyxDataValue;
begin
  LBefore := nil;
  LAfter := nil;
  try
    LBefore := TNyxCodec.Decode(GInputs[1]);
    LAfter := TNyxCodec.Decode(GInputs[3]);
    Check(CustomizeNyxExecutedSource(GInputs[0], LBefore, LAfter) = GInputs[2],
      'browser source writer emits the exact same targeted resource continuation as native');
  finally
    LAfter.Free;
    LBefore.Free;
  end;
  LManifest := TNyxDataValue.ParseJSON(GInputs[4]);
  { The retained artifact is built by the ordinary Pascal constructor compiler.
    This fixture receives its detached result; it grants no editor publication
    or network-provider authority. Worker retirement owns all completion paths. }
  GWorker := TJSWorker.new(LManifest.Field('artifact').AsText);
  GWorker.addEventListener('message', @Received);
  GWorker.addEventListener('error', @WorkerFailed);
  GTimeout := window.setTimeout(@TimedOut, 10000);
end;

procedure LoadNext; forward;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'bounded qualification input loads over HTTP');
    GInputs[GStep] := GRequest.responseText;
    Inc(GStep);
    LoadNext;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function LoadFailed(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  Failed('HTTP qualification input could not load');
end;

procedure LoadNext;
begin

  if GStep = Length(CFiles) then
  begin
    Execute;
    Exit;
  end;
  GRequest := TJSXMLHttpRequest.new;
  GRequest.onload := @Loaded;
  GRequest.onerror := @LoadFailed;
  GRequest.open('GET', CFiles[GStep], True);
  GRequest.send;
end;

begin
  document.body.setAttribute('data-test-result', 'running');
  document.body.textContent := 'Checking handwritten resource continuation...';
  LoadNext;
end.
