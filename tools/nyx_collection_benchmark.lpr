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

program nyx_collection_benchmark;

{$mode delphi}{$H+}
{$codepage utf8}

{ Measures the public runtime collection boundary, without a renderer or hard
  machine-dependent pass threshold. Run identical fixtures on FPC and pas2js.
  CSV reports total setup, 20 atomic single-field updates and 20000 indexed
  identity lookups. Correct final values/counts must pass before publishing data.
  Virtualized control rendering and document wire budgets have separate owners. }

uses
  SysUtils,
  {$IFDEF PAS2JS}
  Web,
  {$ENDIF}
  nyx.text,
  nyx.state,
  nyx.collections;

function Milliseconds: Double;
begin
  {$IFDEF PAS2JS}
  Result := window.performance.now;
  {$ELSE}
  Result := GetTickCount64;
  {$ENDIF}
end;

function Measure(AItems: Integer): TNyxText;
var
  LKey: TNyxCollectionRef;
  LScore: TNyxIntegerFieldRef;
  LSchema: TNyxCollectionSchema;
  LStore: INyxCollection;
  LBefore: INyxCollectionSnapshot;
  LEdits: array of TNyxCollectionEdit;
  LBatch: Integer;
  LIndex: Integer;
  LTarget: TNyxItemRef;
  LStart: Double;
  LBuild: Double;
  LUpdates: Double;
  LLookups: Double;
  LChecksum: Integer;
begin
  LKey := NyxCollection('benchmark/🌙');
  LScore := NyxIntegerField('score');
  LSchema := NyxCollectionSchema
    .Text(NyxTextField('caption'), 'A durable row / 🌙')
    .Integer(LScore, 0);
  LStore := NewNyxCollection(LKey, LSchema);
  SetLength(LEdits, NyxMaximumCollectionEdits);
  LStart := Milliseconds;
  for LBatch := 0 to (AItems div Length(LEdits)) - 1 do
  begin
    for LIndex := 0 to High(LEdits) do
    begin
      LEdits[LIndex] := NyxInsert(LStore.Snapshot.Count + LIndex,
        NyxCollectionItem(NyxItem(LKey,
          'row-' + TNyxText(IntToStr(LBatch * Length(LEdits) + LIndex)))));
    end;
    LStore.Apply(LEdits);
  end;
  LBuild := Milliseconds - LStart;
  LBefore := LStore.Snapshot;
  LTarget := NyxItem(LKey, 'row-' + TNyxText(IntToStr(AItems - 1)));
  LStart := Milliseconds;
  for LIndex := 1 to 20 do
  begin
    LStore.Apply([NyxUpdate(NyxCollectionItem(LTarget).WithValue(LScore, LIndex))]);
  end;
  LUpdates := Milliseconds - LStart;
  LChecksum := 0;
  LStart := Milliseconds;
  for LIndex := 1 to 20000 do
  begin
    Inc(LChecksum, LStore.Snapshot.IndexOf(LTarget));
  end;
  LLookups := Milliseconds - LStart;

  if (LStore.Snapshot.Count <> AItems) or
    (LStore.Snapshot.Item(LTarget).GetValue(LScore) <> 20) or
    (LBefore.Item(LTarget).GetValue(LScore) <> 0) or
    (LChecksum <> (AItems - 1) * 20000) then
  begin
    raise ENyxCollection.Create('Benchmark refused incorrect values/snapshot/lookup results');
  end;
  Result := TNyxText(IntToStr(AItems)) + ',' + NyxStateNumberText(LBuild) + ',' +
    NyxStateNumberText(LUpdates) + ',' + NyxStateNumberText(LLookups) + ',' +
    TNyxText(IntToStr(LStore.Snapshot.DataBytes));
end;

var
  LReport: TNyxStrings;
  LText: TNyxText;

begin
  LReport := TNyxStrings.Create;
  try
    try
      LReport.Add('items,setup_ms,20_updates_ms,20000_lookups_ms,payload_bytes');
      LReport.Add(Measure(512));
      LReport.Add(Measure(4096));
      LReport.Add(Measure(16384));
      LText := LReport.Join(#10);
      {$IFDEF PAS2JS}
      document.body.textContent := LText;
      document.body.setAttribute('data-collection-benchmark', 'passed');
      {$ELSE}
      WriteLn(LText);
      {$ENDIF}
    except
      on LException: Exception do
      begin
        {$IFDEF PAS2JS}
        document.body.textContent := 'FAIL ' + LException.Message;
        document.body.setAttribute('data-collection-benchmark', 'failed');
        {$ELSE}
        WriteLn(LException.ClassName + ': ', LException.Message);
        Halt(1);
        {$ENDIF}
      end;
    end;
  finally
    LReport.Free;
  end;
end.

