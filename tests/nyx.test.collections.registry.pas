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

unit nyx.test.collections.registry;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.model;

{ Public-consumer fixture for registry/wire/history/generated tests. The caller
  owns the document; no test-only metadata or alternate UI toolkit is required. }
function CreateNyxCollectionFixture: TNyxDocument;
function RunNyxCollectionRegistryTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.state,
  nyx.data,
  nyx.contract,
  nyx.collections,
  nyx.collections.registry,
  nyx.collections.codec,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.composition,
  nyx.studio.session,
  nyx.test.source.managed,
  nyx.application.state;

type
  { An implementation-neutral consumer must not trust an alternative snapshot's
    count or mutable backing pointer. Defaults re-admit and retain owned rows. }
  TForeignSnapshot = class(TInterfacedObject, INyxCollectionSnapshot)
  public
    Backing: INyxCollectionSnapshot;
    ReportedCount: Integer;
    constructor Create(const ABacking: INyxCollectionSnapshot);
    function GetKey: TNyxCollectionRef;
    function GetSchema: TNyxCollectionSchema;
    function GetCount: Integer;
    function GetRevision: Integer;
    function GetDataBytes: Integer;
    function ItemAt(AIndex: Integer): TNyxCollectionItem;
    function Item(const ARef: TNyxItemRef): TNyxCollectionItem;
    function IndexOf(const ARef: TNyxItemRef): Integer;
    function Has(const ARef: TNyxItemRef): Boolean;
  end;

  TRegistryObserver = class
  public
    Calls: Integer;
    procedure Observe(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
  end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Registry test failed: ' + AMessage);
  end;
  Inc(ACount);
end;

function TaskKey: TNyxCollectionRef;
begin
  Result := NyxCollection('tasks/🌙');
end;

function CaptionField: TNyxTextFieldRef;
begin
  Result := NyxTextField('caption/🌙');
end;

function DesignTask: TNyxItemRef;
begin
  Result := NyxItem(TaskKey, 'design/🌙');
end;

function CreateNyxCollectionFixture: TNyxDocument;
var
  LSchema: TNyxCollectionSchema;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'A collection application / 🌙 漢字';
    LSchema := NyxCollectionSchema
      .Text(CaptionField, 'Untitled / 🌙')
      .Boolean(NyxBooleanField('complete'), False)
      .Integer(NyxIntegerField('priority'), 1, NyxIntegerDomain.Range(1, 5))
      .Number(NyxNumberField('score'), 0.1, NyxNumberDomain.Range(0, 1));
    Result.Collections.Define(TaskKey, LSchema, [
      NyxCollectionItem(DesignTask)
        .WithValue(CaptionField, 'Craft this / 🌙 漢字' + #0 + 'tail')
        .WithValue(NyxIntegerField('priority'), 2)
        .WithValue(NyxNumberField('score'), 0.3),
      NyxCollectionItem(NyxItem(TaskKey, 'review'))
    ]);
    Result.Collections.Define(NyxCollection('notes'),
      NyxCollectionSchema.Text(NyxTextField('summary'), 'No notes')
        .Field('remark', TNyxStateValue.FromText('No additional constraint'), NyxNoDomain), []);
    Result.AddPage(TNyxNode.Create(nkColumn, 'home')
      .Add(TNyxNode.Create(nkMemo, 'reply').Configure.Text('Write a reply').Done));
    Result.AddPage(TNyxNode.Create(nkColumn, 'review-page'));
    Result.AddComponent(TNyxNode.Create(nkColumn, 'task-card')
      .Add(TNyxNode.Create(nkLabel, 'task-card-caption').Configure.Text('Task / 🌙').Done));
    Result.Find('home').Add(TNyxNode.Create(nkComponent, 'task-card-instance')
      .Configure.Component(NyxComponent('task-card')).Done);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

constructor TForeignSnapshot.Create(const ABacking: INyxCollectionSnapshot);
begin
  inherited Create;
  Backing := ABacking;
  ReportedCount := -999;
end;

function TForeignSnapshot.GetKey: TNyxCollectionRef;
begin
  Result := Backing.Key;
end;

function TForeignSnapshot.GetSchema: TNyxCollectionSchema;
begin
  Result := Backing.Schema;
end;

function TForeignSnapshot.GetCount: Integer;
begin
  Result := Backing.Count;

  if ReportedCount <> -999 then
  begin
    Result := ReportedCount;
  end;
end;

function TForeignSnapshot.GetRevision: Integer;
begin
  Result := 900;
end;

function TForeignSnapshot.GetDataBytes: Integer;
begin
  Result := 0;
end;

function TForeignSnapshot.ItemAt(AIndex: Integer): TNyxCollectionItem;
begin
  Result := Backing.ItemAt(AIndex);
end;

function TForeignSnapshot.Item(const ARef: TNyxItemRef): TNyxCollectionItem;
begin
  Result := Backing.Item(ARef);
end;

function TForeignSnapshot.IndexOf(const ARef: TNyxItemRef): Integer;
begin
  Result := Backing.IndexOf(ARef);
end;

function TForeignSnapshot.Has(const ARef: TNyxItemRef): Boolean;
begin
  Result := Backing.Has(ARef);
end;

procedure TRegistryObserver.Observe(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);
end;

procedure RejectWire(const ASource: TNyxText; var ACount: Integer);
var
  LDecoded: INyxCollectionDefaults;
  LRejected: Boolean;
begin
  LRejected := False;
  try
    LDecoded := DecodeNyxCollectionDefaults(ASource);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'malformed collection wire must reject: ' + ASource, ACount);
end;

function RunNyxCollectionRegistryTests: Integer;
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LCloned: TNyxDocument;
  LPlain: TNyxDocument;
  LAppOne: TNyxApplicationState;
  LAppTwo: TNyxApplicationState;
  LRuntime: INyxCollections;
  LRuntimeClone: INyxCollections;
  LStore: INyxCollection;
  LForeignStore: INyxCollection;
  LDefaults: INyxCollectionDefaults;
  LDefaultClone: INyxCollectionDefaults;
  LForeign: TForeignSnapshot;
  LForeignRef: INyxCollectionSnapshot;
  LRetained: INyxCollectionSnapshot;
  LSubscription: INyxCollectionSubscription;
  LObserver: TRegistryObserver;
  LWire: TNyxText;
  LOld: TNyxText;
  LPacket: TNyxText;
  LRejected: Boolean;
  LIndex: Integer;
  LRows: array of TNyxCollectionItem;
  LBigSchema: TNyxCollectionSchema;
  LKey: TNyxCollectionRef;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LCandidate: TNyxDocument;
  LIsolated: TNyxDocument;
  LSource: TNyxText;
  LDraft: TNyxText;
  LBeforeSource: TNyxText;
  LAfterWire: TNyxText;
begin
  Result := 0;
  LDocument := CreateNyxCollectionFixture;
  LDecoded := nil;
  LCloned := nil;
  LPlain := nil;
  LAppOne := nil;
  LAppTwo := nil;
  LWorkspace := nil;
  LSession := nil;
  LCandidate := nil;
  LIsolated := nil;
  LObserver := TRegistryObserver.Create;
  try
    Check((LDocument.Collections.Count = 2) and
      (LDocument.Collections.Key(0).Name = TaskKey.Name) and
      (LDocument.Collections.Key(1).Name = 'notes'), 'ordered defaults', Result);
    LRetained := LDocument.Collections.Snapshot(TaskKey);
    Check((LRetained.Revision = 0) and (LRetained.Count = 2),
      'whole defaults seed revision zero', Result);
    Check(LRetained.ItemAt(0).GetValue(CaptionField) = 'Craft this / 🌙 漢字' + #0 + 'tail',
      'owned Unicode/NUL defaults', Result);
    LCloned := LDocument.Clone;
    LCloned.Collections.Remove(NyxCollection('notes'));
    Check((LCloned.Collections.Count = 1) and (LDocument.Collections.Count = 2),
      'document clones have independent registries', Result);
    LCloned.Collections.Define(TaskKey, LRetained.Schema,
      [NyxCollectionItem(DesignTask).WithValue(CaptionField, 'Clone only')]);
    Check(LDocument.Collections.Snapshot(TaskKey).Count = 2,
      'clone replacement preserves source rows', Result);

    LAppOne := TNyxApplicationState.Create(LDocument);
    LAppTwo := TNyxApplicationState.Create(LDocument);
    LRuntime := LAppOne.Collections;
    LStore := LRuntime.Collection(TaskKey);
    Check((LRuntime.Count = 2) and (LStore.Snapshot.Revision = 0),
      'runtime registries seed without synthetic revisions', Result);
    LSubscription := LStore.Subscribe(LObserver.Observe);
    LStore.Update(NyxCollectionItem(DesignTask).WithValue(CaptionField, 'Runtime one'));
    Check(LObserver.Calls = 1, 'runtime edit notification', Result);
    Check(LAppTwo.Collections.Collection(TaskKey).Snapshot.Item(DesignTask).GetValue(CaptionField) =
      LRetained.Item(DesignTask).GetValue(CaptionField), 'sibling runtime isolation', Result);
    Check(LDocument.Collections.Snapshot(TaskKey).Item(DesignTask).GetValue(CaptionField) =
      LRetained.Item(DesignTask).GetValue(CaptionField), 'authored defaults isolation', Result);
    LRuntimeClone := LRuntime.Clone;
    Check(LRuntimeClone.Collection(TaskKey).Snapshot.Revision = LStore.Snapshot.Revision,
      'runtime clone preserves revision', Result);
    LRuntimeClone.Collection(TaskKey).Remove(DesignTask);
    Check((LStore.Snapshot.Count = 2) and (LObserver.Calls = 1),
      'runtime clone owns stores and omits observers', Result);
    FreeAndNil(LAppOne);
    Check(LStore.Snapshot.Item(DesignTask).GetValue(CaptionField) = 'Runtime one',
      'retained runtime outlives application owner', Result);
    LSubscription.Disconnect;

    LWire := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.collections.view');
    Check((Pos('Result.Collections.Define(', LSource) > 0) and
      (Pos('.Integer(NyxIntegerField(', LSource) > 0) and
      (Pos('TNyxStateValue.FromText(', LSource) > 0),
      'generation uses typed schema/item factories and explicit descriptor absence', Result);
    LWorkspace := TNyxSourceWorkspace.Create;
    LWorkspace.Accept(LDocument, LSource);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LWire, 'generated builder reconstructs collection meaning', Result);
    FreeAndNil(LCandidate);
    LSession := TNyxStudioSession.Create;
    LSession.Load(LWire);
    LSource := LSession.Source;
    LDraft := EditNyxManagedFixture(LSource, 'Result.Collections.Define(',
      '// A thoughtful collection / 🌙' + #10 + '    Result.Collections.Define(');
    LSession.SetSourceDraft(LDraft);
    LSession.ApplySourceDraft;
    LBeforeSource := LSession.Source;
    Check((LBeforeSource = LDraft) and (LSession.Save = LWire),
      'Studio accepts crafted collection comments without changing meaning', Result);
    LSession.Select('reply');
    LSession.SetProperty('text', 'A visual note');
    Check((Pos('A thoughtful collection / 🌙', LSession.Source) > 0) and
      (LSession.Document.Collections.Snapshot(TaskKey).Count = 2),
      'visual regeneration retains crafted collection metadata and values', Result);
    LSession.Undo;
    Check((LSession.Save = LWire) and (LSession.Source = LBeforeSource),
      'paired history restores exact collection design/source', Result);
    LSession.Redo;
    LAfterWire := LSession.Save;
    LSession.Undo;
    LSession.Redo;
    Check(LSession.Save = LAfterWire, 'redo restores collection meaning with visual edits', Result);
    LSource := LSession.Source;
    LDraft := EditNyxManagedFixture(LSource, '.WithValue(NyxIntegerField(''priority''), 2)',
      '.WithValue(NyxIntegerField(''priority''), 3)');
    Check(LDraft <> LSource, 'source edit fixture changes a typed row value', Result);
    LSession.SetSourceDraft(LDraft);
    LSession.ApplySourceDraft;
    Check(LSession.Document.Collections.Snapshot(TaskKey).Item(DesignTask)
      .GetValue(NyxIntegerField('priority')) = 3,
      'source row edits publish through the shared Studio pair boundary', Result);
    LSession.Undo;
    Check((LSession.Source = LSource) and (LSession.Save = LAfterWire),
      'undo restores original authored collection value/source', Result);
    LDraft := EditNyxManagedFixture(LSource, '.WithValue(NyxIntegerField(''priority''), 2)',
      '.WithValue(NyxIntegerField(''priority''), ''wrong family'')');
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LAfterWire) and (LSession.Source = LSource) and
      (LSession.DraftSource = LDraft), 'wrong typed row draft retains accepted pair and rejected text', Result);
    LSession.DiscardSourceDraft;
    LSession.Redo;
    Check(LSession.Document.Collections.Snapshot(TaskKey).Item(DesignTask)
      .GetValue(NyxIntegerField('priority')) = 3, 'rejected draft preserves collection redo', Result);
    LIsolated := CloneNyxViewDocument(LDocument, LDocument.Find('reply'));
    Check((LIsolated.Collections.Count = LDocument.Collections.Count) and
      (LIsolated.Collections.Snapshot(TaskKey).Item(DesignTask).SameItem(LRetained.Item(DesignTask))),
      'isolated view retains independently owned collection defaults', Result);
    LDraft := PrepareNyxCompanion(LDocument, LIsolated, TNyxCodegen.Generate(LDocument), True);
    LCandidate := LWorkspace.Candidate(LIsolated, LDraft);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LIsolated),
      'isolated collection companion reconstructs its whole design', Result);
    FreeAndNil(LCandidate);
    Check(TNyxDataValue.ParseJSON(LWire).Field('version').AsInteger = 2,
      'collection document selects wire version 2', Result);
    LDecoded := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LDecoded) = LWire, 'complete document round trip', Result);
    Check(LDecoded.Collections.Snapshot(TaskKey).Schema.SameSchema(LRetained.Schema),
      'schema/default/domain exact reconstruction', Result);
    Check(LDecoded.Collections.Snapshot(TaskKey).Item(DesignTask).SameItem(LRetained.Item(DesignTask)),
      'ordered scalar values reconstruct exactly', Result);
    Check(LDecoded.Collections.Snapshot(TaskKey).Item(DesignTask).FieldValue(3)
      .SameValue(TNyxStateValue.FromNumber(0.3)), 'finite Double reconstruction', Result);
    LPacket := EncodeNyxCollectionDefaults(LDocument.Collections);
    LDefaults := DecodeNyxCollectionDefaults(LPacket);
    Check(EncodeNyxCollectionDefaults(LDefaults) = LPacket, 'descriptor canonical round trip', Result);
    Check((LDefaults.Snapshot(NyxCollection('notes')).Count = 0) and
      (LDefaults.Snapshot(NyxCollection('notes')).Schema.FieldAt(0).Domain.Kind = nskText),
      'empty datasets and unconstrained typed domains survive', Result);
    LDefaults := NewNyxCollectionDefaults;
    LDefaults.Define(NyxCollection('descriptor'), NyxCollectionSchema.Field('summary',
      TNyxStateValue.FromText('No notes'), NyxNoDomain), []);
    LDefaults := DecodeNyxCollectionDefaults(EncodeNyxCollectionDefaults(LDefaults));
    Check(not LDefaults.Snapshot(NyxCollection('descriptor')).Schema.FieldAt(0).Domain.Defined,
      'explicit descriptor absence remains distinct from an unconstrained typed domain', Result);

    LOld := '{"version":1,"title":"Legacy","pages":[],"components":[],"collections":{"custom":"🌙"}}';
    LPlain := TNyxCodec.Decode(LOld);
    Check((LPlain.Collections.Count = 0) and
      (LPlain.Extensions.Value(NyxExtension('collections')).Field('custom').AsText = '🌙'),
      'version-1 collection-named extension remains opaque', Result);
    LOld := TNyxCodec.Encode(LPlain);
    LPlain.Collections.Define(TaskKey, LRetained.Schema, []);
    LRejected := False;
    try
      TNyxCodec.Encode(LPlain);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LPlain.Extensions.Has(NyxExtension('collections')),
      'typed/opaque collision rejects without discarding the extension', Result);
    LPlain.Collections.Remove(TaskKey);
    Check(TNyxCodec.Encode(LPlain) = LOld, 'legacy version 1 remains byte-stable', Result);

    LForeignStore := NewNyxCollection(LRetained);
    LForeign := TForeignSnapshot.Create(LForeignStore.Snapshot);
    LForeignRef := LForeign;
    LDefaults := NewNyxCollectionDefaults;
    LDefaults.Define(LForeignRef);
    Check((LDefaults.Snapshot(TaskKey).DataBytes > 0) and
      (LDefaults.Snapshot(TaskKey).Revision = 0),
      'foreign byte/revision claims are re-admitted as defaults', Result);
    LForeignStore.Update(NyxCollectionItem(DesignTask).WithValue(CaptionField, 'Foreign changed'));
    LForeign.Backing := LForeignStore.Snapshot;
    Check(LDefaults.Snapshot(TaskKey).Item(DesignTask).GetValue(CaptionField) =
      LRetained.Item(DesignTask).GetValue(CaptionField),
      'foreign mutable backing cannot change admitted defaults', Result);
    LOld := EncodeNyxCollectionDefaults(LDefaults);
    LForeign.ReportedCount := -1;
    LRejected := False;
    try
      LDefaults.Define(LForeignRef);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (EncodeNyxCollectionDefaults(LDefaults) = LOld),
      'negative foreign count rejects atomically', Result);
    LForeign.ReportedCount := NyxMaximumCollectionItems + 1;
    LRejected := False;
    try
      NewNyxCollection(LForeignRef);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'oversized foreign count rejects before allocation', Result);

    LDefaultClone := LDefaults.Clone;
    LDefaultClone.Remove(TaskKey);
    Check((LDefaultClone.Count = 0) and (LDefaults.Count = 1),
      'retained defaults clone/removal isolation', Result);
    LDefaults := NewNyxCollectionDefaults;
    for LIndex := 1 to NyxMaximumCollections do
    begin
      LDefaults.Define(NyxCollection('scope-' + TNyxText(IntToStr(LIndex))),
        NyxCollectionSchema, []);
    end;
    LRejected := False;
    try
      LDefaults.Define(NyxCollection('scope-extra'), NyxCollectionSchema, []);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LDefaults.Count = NyxMaximumCollections),
      'aggregate definition count rejection', Result);
    LDefaults.Define(LDefaults.Key(0),
      NyxCollectionSchema.Text(NyxTextField('caption'), 'Replacement'), []);
    Check((LDefaults.Count = NyxMaximumCollections) and
      (LDefaults.Key(0).Name = 'scope-1'), 'replacement retains order at the count limit', Result);

    { Whole seed construction can exceed the edit-batch limit, without a
      quadratic sequence of synthetic Append publications or revision changes. }
    LKey := NyxCollection('seed');
    SetLength(LRows, 513);
    for LIndex := 0 to High(LRows) do
    begin
      LRows[LIndex] := NyxCollectionItem(NyxItem(LKey, 'row-' + TNyxText(IntToStr(LIndex))));
    end;
    LStore := NewNyxCollection(LKey, NyxCollectionSchema, LRows);
    Check((LStore.Snapshot.Count = 513) and (LStore.Snapshot.Revision = 0),
      'whole seed exceeds batch limit without synthetic revisions', Result);
    LRows[512] := LRows[0];
    LRejected := False;
    try
      NewNyxCollection(LKey, NyxCollectionSchema, LRows);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'duplicate whole-seed identities reject', Result);

    { Aggregate logical defaults remain bounded even when individual snapshots
      are valid. Export additionally retains the existing 4-MiB JSON limit. }
    LDefaults := NewNyxCollectionDefaults;
    LBigSchema := NyxCollectionSchema.Text(NyxTextField('large'),
      TNyxText(StringOfChar('x', 1024 * 1024)));
    for LIndex := 1 to 7 do
    begin
      LDefaults.Define(NyxCollection('large-' + TNyxText(IntToStr(LIndex))), LBigSchema, []);
    end;
    LRejected := False;
    try
      LDefaults.Define(NyxCollection('large-8'), LBigSchema, []);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LDefaults.Count = 7),
      'aggregate payload includes empty-schema defaults', Result);
    LRejected := False;
    try
      EncodeNyxCollectionDefaults(LDefaults);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LDefaults.Count = 7),
      'wire budget rejects an oversized complete packet without truncation', Result);

    RejectWire('{"version":2,"definitions":[]}', Result);
    RejectWire('{"version":1,"definitions":[],"extra":true}', Result);
    RejectWire('{"version":1,"definitions":[{"key":"a","schema":[],"items":[]},{"key":"a","schema":[],"items":[]}]}', Result);
    RejectWire('{"version":1,"definitions":[{"key":"a","schema":[],"items":[{"id":"x","values":[1]}]}]}', Result);
    RejectWire('{"version":1,"definitions":[{"key":"a","schema":[{"name":"value","default":{"type":"integer","value":0.5},"domain":null}],"items":[]}]}', Result);
    RejectWire('{"version":1,"definitions":[{"key":"a","schema":[{"name":"value","default":{"type":"number","value":"NaN"},"domain":null}],"items":[]}]}', Result);
    RejectWire('{"version":1,"definitions":[{"key":"a","schema":[{"name":"value","default":{"type":"text","value":false},"domain":null}],"items":[]}]}', Result);
    RejectWire('{"version":1,"definitions":[{"key":"a","schema":[],"items":[{"id":"x","values":[]},{"id":"x","values":[]}]}]}', Result);
    RejectWire('{"version":1,"definitions":[{"key":"a","schema":[{"name":"value","default":{"type":"boolean","value":true},"domain":{"kind":"text"}}],"items":[]}]}', Result);
    LRetained := LDocument.Collections.Snapshot(TaskKey);
    FreeAndNil(LDocument);
    Check(LRetained.Item(DesignTask).GetValue(CaptionField) = 'Craft this / 🌙 漢字' + #0 + 'tail',
      'retained authored snapshot outlives document', Result);
  finally

    if LSubscription <> nil then
    begin
      LSubscription.Disconnect;
    end;
    LObserver.Free;
    LWorkspace.Free;
    LSession.Free;
    LCandidate.Free;
    LIsolated.Free;
    LAppOne.Free;
    LAppTwo.Free;
    LPlain.Free;
    LCloned.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

end.
