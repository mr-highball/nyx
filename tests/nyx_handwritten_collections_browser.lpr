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

program nyx_handwritten_collections_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.source,
  nyx.collections, nyx.collections.registry, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.selection, nyx.typeahead,
  nyx.resources, nyx.resources.rows, nyx.studio.builds,
  nyx.studio.sourceprojection;

const
  CFiles: array[0..6] of TNyxText = ('collection-steps.json',
    'collection-source.pas', 'collection-design.json', 'collection-worker.json',
    'collection-changed-source.pas', 'collection-changed-design.json',
    'collection-changed-worker.json');
  CChangedCaption: TNyxText = 'A recomputed collection 🚀 𐐷 é.';

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
  document.body.textContent := 'FAIL handwritten collections: ' + AReason;
  document.body.setAttribute('data-test-result', 'failed');
  document.body.setAttribute('data-event-error', AReason);
end;

procedure StartWorker; forward;

procedure CheckCollection(ADocument: TNyxDocument);
var
  LKey: TNyxCollectionRef;
  LRows: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LCopy: TNyxCollectionSchema;
  LRecipe: TNyxResourceRows;
  LSpec: TNyxCollectionViewSpec;
  LRejected: Boolean;
begin
  LKey := NyxCollection('roster');
  LRows := ADocument.Collections.Snapshot(LKey);
  LSchema := LRows.Schema;
  Check((LRows.Count = 3) and (LRows.ItemAt(0).Ref.ID = 'third'),
    'executed membership and stable row movement are exact');
  Check(LRows.Item(NyxItem(LKey, 'first')).GetValue(NyxIntegerField('rank')) = 7,
    'deliberate edited cell survives all later schema stages');
  Check(LRows.Item(NyxItem(LKey, 'first')).GetValue(NyxNumberField('weight')) = 0.125,
    'retained computed numeric cell is exact');
  Check(LSchema.Definition(NyxNumberField('weight')).DefaultValue.NumberValue = 0.25,
    'explicit changed numeric default is exact');
  Check(LSchema.Definition(NyxIntegerField('rank')).DefaultValue.IntegerValue = 3,
    'retained computed integer default is exact');
  Check(LSchema.Definition(NyxBooleanField('active')).DefaultValue.BooleanValue and
    not LSchema.Definition(NyxBooleanField('active')).Domain.Defined,
    'typed Boolean metadata retains explicit domain absence');
  LCopy := NyxCollectionSchema.Field(LSchema.Definition(NyxTextField('label')));
  LCopy := LCopy.Boolean(NyxBooleanField('independent'), False);
  Check((LCopy.Count = 2) and (LSchema.Count = 4),
    'field composition owns an independent schema');
  LRejected := False;
  try
    LSchema.Definition(NyxBooleanField('rank'));
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'typed metadata rejects a mismatched family');
  LRejected := False;
  try
    LSchema.Definition(NyxTextField('missing'));
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'typed metadata rejects an absent identity');
  LRejected := False;
  try
    LCopy.Field(LSchema.Definition(NyxTextField('label')));
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCopy.Count = 2), 'duplicate copied fields refuse without mutation');
  Check(ADocument.ResourceCollections.HasSource(NyxCollection('catalog')) and
    (ADocument.Collections.Snapshot(NyxCollection('catalog')).Count = 0),
    'resource recipe remains independent of its authored empty seed');
  LRecipe := ADocument.ResourceCollections.Source(NyxCollection('catalog'));
  LRows := LRecipe.Read(ADocument.Resources, NyxCollection('catalog'),
    NyxDefaultLocale, NyxDefaultLocale);
  Check((LRows.Count = 1) and
    (LRows.ItemAt(0).GetValue(NyxTextField('label')) = 'A resource-backed row 🚀') and
    (LRows.ItemAt(0).GetValue(NyxIntegerField('rank')) = 2),
    'executed resource path projects exact typed data');
  LSpec := ADocument.Find('collection-table').CollectionView;
  Check(LSpec.Defined and (LSpec.Key.Name = 'catalog') and (LSpec.Count = 2),
    'retargeted table binds its typed resource fields');
  LSpec := ADocument.Find('collection-list').CollectionView;
  Check((LSpec.SelectionMode = nsmMultiple) and LSpec.HasTypeAhead and
    (LSpec.TypeAheadPolicy.WindowMS = 1600) and (LSpec.TypeAheadPolicy.MatchMode = ntmExact),
    'list selection and search policies execute through fluent types');

  if GPhase = 1 then
  begin
    LRows := ADocument.Collections.Snapshot(LKey);
    Check(LRows.Item(NyxItem(LKey, 'first')).GetValue(NyxTextField('label')) = CChangedCaption,
      'changing the original helper recomputes the edited collection');
    Check(LSchema.Definition(NyxTextField('label')).DefaultValue.TextValue = CChangedCaption,
      'changing the helper recomputes untouched schema metadata');
    Check(ADocument.Collections.Snapshot(NyxCollection('pins')).ItemAt(0)
      .GetValue(NyxTextField('label')) = CChangedCaption,
      'the independent unmodified collection also recomputes');
  end;
end;

function Received(AEvent: TJSEvent): Boolean;
var
  LManifest: TNyxDataValue;
  LProjection: INyxSourceProjection;
  LDocument: TNyxDocument;
  LIndex: Integer;
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
      CheckCollection(LDocument);
      Retire;

      if GPhase = 0 then
      begin
        GPhase := 1;
        StartWorker;
      end
      else
      begin
        document.body.textContent := 'PASS handwritten collections / ' +
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
  Failed('Actual compiled collection constructor could not execute');
end;

procedure TimedOut;
begin
  Failed('Actual collection constructor exceeded ten seconds');
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
      document.body.setAttribute('data-collection-stage', 'source-' + IntToStr(GReplay));
      LStarted := TJSDate.now;
      LStep := GSteps.Item(GReplay);
      LBefore := TNyxCodec.Decode(LStep.Field('beforeDesign').AsText);
      LAfter := TNyxCodec.Decode(LStep.Field('afterDesign').AsText);
      Check(CustomizeNyxExecutedSource(LStep.Field('beforeSource').AsText,
        LBefore, LAfter) = LStep.Field('afterSource').AsText,
        'browser emits the exact native row/schema/view/recipe continuation at step ' +
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
  Check((GSteps.Kind = ndArray) and (GSteps.Count >= 12),
    'bounded carrier retains every ordinary native transaction');
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
  Failed('HTTP collection input could not load');
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
  document.body.textContent := 'Checking handwritten collection continuation...';
  LoadNext;
end.

