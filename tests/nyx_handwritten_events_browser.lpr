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

program nyx_handwritten_events_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.types, nyx.model, nyx.codec, nyx.source,
  nyx.callbacks, nyx.events, nyx.scheduler, nyx.studio.builds,
  nyx.studio.sourceprojection;

const
  CFiles: array[0..6] of TNyxText = ('event-steps.json',
    'event-source.pas', 'event-design.json', 'event-worker.json',
    'event-changed-source.pas', 'event-changed-design.json',
    'event-changed-worker.json');

var
  GRequest: TJSXMLHttpRequest;
  GWorker: TJSWorker;
  GTimeout: NativeInt;
  GReplayTimer: NativeInt;
  GStep: Integer;
  GPhase: Integer;
  GChecks: Integer;
  GReplay: Integer;
  GSteps: TNyxDataValue;
  GInputs: array[0..6] of TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ This fixture owns both detached worker executions. No successful reply grants
  authority to publish an observing editor or mutate a live project. }
procedure Retire;
begin

  if GReplayTimer <> 0 then
  begin
    window.clearTimeout(GReplayTimer);
    GReplayTimer := 0;
  end;

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
  document.body.textContent := 'FAIL handwritten callbacks: ' + AReason;
  document.body.setAttribute('data-test-result', 'failed');
  document.body.setAttribute('data-event-error', AReason);
end;

procedure StartWorker; forward;

{ Compiled construction is separate from physical input/scheduler execution.
  The second worker changes the original helper, without changing overrides. }
procedure CheckCallbacks(ADocument: TNyxDocument);
var
  LInfos: TNyxAuthoredEventInfos;
  LPolicy: TNyxExecutionPolicy;
begin
  LPolicy := neUIQueue;

  if GPhase = 1 then
  begin
    LPolicy := neSequential;
  end;
  LInfos := NyxAuthoredEvents(ADocument.Find('heading-1'));
  Check((Length(LInfos) = 1) and (LInfos[0].Policy = neAsynchronous),
    'deliberate policy survives future helper recomputation');
  Check((Length(LInfos[0].Callbacks) = 3) and
    (LInfos[0].Callbacks[0].ID.Name = 'THeading1Click2') and
    (LInfos[0].Callbacks[1].ID.Name = 'primary 🚀') and
    (LInfos[0].Callbacks[2].ID.Name = 'secondary 𐐷'),
    'ordered registration identities and supplementary Unicode are exact');
  LInfos := NyxAuthoredEvents(ADocument.Find('heading-2'));
  Check(LInfos[0].Policy = LPolicy, 'unmodified event policy retains its original helper');
  LInfos := NyxAuthoredEvents(ADocument.Find('reply'));
  Check((Length(LInfos) = 3) and (LInfos[0].Trigger = ntBeforeKeyPress) and
    (LInfos[1].Trigger = ntAfterKeyPress) and (LInfos[2].Trigger = ntNamed) and
    (LInfos[2].Name.Name = 'app.reply'), 'physical phases and named streams remain distinct');
  Check((LInfos[0].Policy = LPolicy) and (LInfos[1].Policy = LPolicy) and
    (LInfos[2].Policy = neSequential), 'untouched policy in an edited stream recomputes');
  Check((Length(LInfos[0].Callbacks) = 2) and
    (LInfos[0].Callbacks[0].ID.Name = 'before') and
    (LInfos[0].Callbacks[0].Handler.Name = 'TNotebookOther') and
    (LInfos[0].Callbacks[1].ID.Name = 'second-before'),
    'registration replacement preserves final order and independent siblings');
end;

function Received(AEvent: TJSEvent): Boolean;
var
  LManifest: TNyxDataValue;
  LProjection: INyxSourceProjection;
  LDocument: TNyxDocument;
  LIndex: Integer;
  LPosition: Integer;
  LSource: TNyxText;
  LRejected: Boolean;
begin
  Result := False;
  LDocument := nil;
  try
    try
      Check(isString(TJSMessageEvent(AEvent).data), 'worker returns exact JSON text');
      LIndex := 1 + GPhase * 3;
      LManifest := TNyxDataValue.ParseJSON(GInputs[LIndex + 2]);
      LProjection := ReceiveNyxSourceProjection(GInputs[LIndex],
        NyxSourceProjectionRef(LManifest.Field('reference').AsText), btBrowser,
        TNyxText(TJSMessageEvent(AEvent).data));
      Check((LProjection.State = spsExecuted) and
        (LProjection.Design = GInputs[LIndex + 1]) and
        (LProjection.Source = GInputs[LIndex]),
        'actual compiled browser constructor reproduces the entire pair');
      LDocument := LProjection.CopyDocument;
      CheckCallbacks(LDocument);
      Check(Pos('A crafted reply callback 🚀 𐐷 é.', LProjection.Source) > 0,
        'actual complete source retains the exact owned callback implementation');
      LSource := LProjection.Source;
      LPosition := Pos('{ nyx:customizations-end }', LSource);
      Check(LPosition > 0, 'enclosing property scope retains its closing boundary');
      Insert('procedure TManagedTailCallback.Invoke(const AEvent: TNyxEventInfo;' + #10 +
        '  const AExecution: INyxExecution);' + #10 + 'begin' + #10 + 'end;' + #10,
        LSource, LPosition);
      LRejected := False;
      try
        ReadNyxHandlerSource(LSource, NyxHandler('TManagedTailCallback'));
      except
        on LException: ENyxSource do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'nested managed ownership extends through the latest enclosing end');
      Retire;

      if GPhase = 0 then
      begin
        GPhase := 1;
        StartWorker;
      end
      else
      begin
        document.body.textContent := 'PASS handwritten callbacks / ' +
          IntToStr(GChecks) + ' checks';
        document.body.setAttribute('data-test-result', 'passed');
      end;
    except
      on LException: Exception do
      begin
        Failed(LException.Message);
      end;
    end;
  finally
    LDocument.Free;
  end;
end;

function WorkerFailed(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  Failed('Actual compiled callback constructor could not execute');
end;

procedure TimedOut;
begin
  Failed('Actual callback constructor exceeded ten seconds');
end;

procedure StartWorker;
var
  LManifest: TNyxDataValue;
begin
  LManifest := TNyxDataValue.ParseJSON(GInputs[3 + GPhase * 3]);
  GWorker := TJSWorker.new(LManifest.Field('artifact').AsText);
  GWorker.addEventListener('message', @Received);
  GWorker.addEventListener('error', @WorkerFailed);
  GTimeout := window.setTimeout(@TimedOut, 10000);
end;

{ The carrier replays a history of separate editor operations. Yield between
  operations just as the ordinary asynchronous command queue does; treating the
  entire history as one synchronous browser task can starve the debugger/input.
  Each individual writer operation still has its own finite execution budget. }
procedure ReplayNext;
var
  LStep: TNyxDataValue;
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LStarted: Double;
  LSource: TNyxText;
  LLine: Integer;
begin
  GReplayTimer := 0;

  if GReplay = GSteps.Count then
  begin
    StartWorker;
    Exit;
  end;
  LBefore := nil;
  LAfter := nil;
  try
    try
      document.body.setAttribute('data-event-stage', 'source-' + IntToStr(GReplay));
      LStarted := TJSDate.now;
      LStep := GSteps.Item(GReplay);
      LBefore := TNyxCodec.Decode(LStep.Field('beforeDesign').AsText);
      LAfter := TNyxCodec.Decode(LStep.Field('afterDesign').AsText);
      LSource := CustomizeNyxExecutedSource(LStep.Field('beforeSource').AsText, LBefore, LAfter);

      if LStep.Field('added').AsText <> '' then
      begin
        LSource := AddNyxHandlerStub(LSource, NyxHandler(LStep.Field('added').AsText), ntClick, LLine);
        Check(LLine > 0, 'owned callback stub exposes a source-navigation line');
      end;

      if LStep.Field('bodyHandler').AsText <> '' then
      begin
        LSource := ReplaceNyxHandlerImplementation(LSource,
          NyxHandler(LStep.Field('bodyHandler').AsText), LStep.Field('expectedBody').AsText,
          LStep.Field('replacementBody').AsText);
      end;
      Check(LSource = LStep.Field('afterSource').AsText,
        'browser emits the exact native callback continuation/stub/body at step ' +
        IntToStr(GReplay));
      Check(TJSDate.now - LStarted < 10000, 'one source operation completes within ten seconds');
      Inc(GReplay);
      GReplayTimer := window.setTimeout(@ReplayNext, 0);
    except
      on LException: Exception do
      begin
        Failed(LException.Message);
      end;
    end;
  finally
    LAfter.Free;
    LBefore.Free;
  end;
end;

procedure Execute;
begin
  GSteps := TNyxDataValue.ParseJSON(GInputs[0]);
  Check((GSteps.Kind = ndArray) and (GSteps.Count = 9),
    'bounded carrier retains every native event/source operation');
  GReplayTimer := window.setTimeout(@ReplayNext, 0);
end;

procedure LoadNext; forward;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'qualification input loads over actual HTTP');
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
  Failed('HTTP callback input could not load');
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
  document.body.textContent := 'Checking handwritten callback continuation...';
  LoadNext;
end.

