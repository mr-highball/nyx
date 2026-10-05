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


unit nyx.test.collection.queue;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.studio.projects;

{ English qualification seed, independent of live/observing projects.
  Dedicated scalar proposals below exercise supplementary Unicode and NUL.
  Caller owns the returned paired value; no UI or mutable store is retained. }
function CreateNyxCollectionQueueSeed: TNyxProjectPair;
function RunNyxCollectionQueueJourney(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.types, nyx.state, nyx.data, nyx.model, nyx.controls, nyx.codegen,
  nyx.codec, nyx.schema, nyx.collections, nyx.collections.view.types,
  nyx.collections.selection, nyx.studio.authoring, nyx.studio.collectionintent,
  nyx.studio.session;

function CreateNyxCollectionQueueSeed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxColumn;
  LSpec: TNyxCollectionViewSpec;
begin
  Result := Default(TNyxProjectPair);
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Collection workshop';
    LDocument.Collections.Define(NyxCollection('tasks'),
      NyxCollectionSchema.Text(NyxTextField('caption'), 'Untitled task')
        .Boolean(NyxBooleanField('done'), False)
        .Integer(NyxIntegerField('priority'), 1)
        .Number(NyxNumberField('effort'), 0.125)
        .Text(NyxTextField('parent'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'alpha'))
         .WithValue(NyxTextField('caption'), 'Plan the release'),
       NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'beta'))
         .WithValue(NyxTextField('caption'), 'Review the details')
         .WithValue(NyxTextField('parent'), 'alpha')]);
    { Deliberately differ from schema order: column identity is the exact field,
      never the index of the corresponding schema field. }
    LSpec := NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxIntegerField('priority'), 'Priority')
      .Column(NyxTextField('caption'), 'Task')
      .Column(NyxBooleanField('done'), 'Done')
      .Column(NyxNumberField('effort'), 'Effort');
    LDocument.AddComponent(NewNyxList('task-list').Binds.Collection(
      LSpec.Scoped(csInstance)).Done);
    LPage := NewNyxColumn('home');
    LPage.Add(NewNyxTable('tasks-table').Binds.Collection(LSpec).Done);
    LPage.Add(NewNyxTable('unbound-table'));
    LPage.Add(NewNyxComponent('left-list').Configure.Component(NyxComponent('task-list')).Done);
    LPage.Add(NewNyxComponent('right-list').Configure.Component(NyxComponent('task-list')).Done);
    LDocument.AddPage(LPage);
    LPage := NewNyxColumn('other');
    LPage.Add(NewNyxTree('tasks-tree').Binds.Collection(
      LSpec.Parent(NyxTextField('parent'))).Done);
    LDocument.AddPage(LPage);
    LDocument.Validate;
    Result.Design := TNyxCodec.Encode(LDocument);
    Result.Source := TNyxCodegen.Generate(LDocument);
    { The generated managed view shares its unit with application-owned code.
      Collection admission must retain this helper instead of regenerating the
      entire source file from the document. Its text is deliberately English. }
    Result.Source := TNyxText(StringReplace(String(Result.Source), 'implementation' + #10,
      'implementation' + #10 + #10 +
      'function CollectionWorkshopNote: TNyxText;' + #10 +
      'begin' + #10 + '  Result := ''Keep crafting.'';' + #10 + 'end;' + #10, []));
  finally
    LDocument.Free;
  end;
end;

function RunNyxCollectionQueueJourney(out APair: TNyxProjectPair): Integer;
var
  LSession: TNyxStudioSession;
  LScope: INyxSchemaSnapshot;
  LIntent: TNyxStudioCollectionIntent;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSeed: TNyxProjectPair;
  LPairBefore: TNyxText;
  LPairAfter: TNyxText;
  LDraft: TNyxText;
  LBase: TNyxText;
  LSpec: TNyxCollectionViewSpec;
  LSnapshot: INyxCollectionSnapshot;
  LWire: TNyxDataValue;
  LRejected: Boolean;
  LImportCount: Integer;
  LImportCursor: Integer;
  LImportPosition: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxCollection.Create('Collection queue: ' + AReason);
    end;
    Inc(Result);
  end;

  function Proposal(AAction: TNyxStudioCollectionAction;
    const AKey: TNyxText): TNyxStudioCollectionIntent;
  begin
    Result := Default(TNyxStudioCollectionIntent);
    Result.Action := AAction;
    Result.Key := NyxCollection(AKey);
  end;

  function ViewSpec(const AOwner: TNyxText): TNyxCollectionViewSpec;
  var
    LProjection: TNyxNode;
  begin
    LSession.Select(AOwner);
    LProjection := LSession.SelectedProjection;
    try
      Result := LProjection.CollectionView;
    finally
      LProjection.Free;
    end;
  end;

  function Capture(const AProposal: TNyxStudioCollectionIntent;
    const AOwner: TNyxText = ''): TNyxStudioDesignRequest;
  var
    LLocal: TNyxStudioDesignEdit;
  begin
    LLocal := Default(TNyxStudioDesignEdit);
    LLocal.Action := sdaCollection;
    LLocal.Collection := AProposal;

    if NyxStudioCollectionViewAction(AProposal.Action) then
    begin

      if AOwner = 'tasks-tree' then
      begin
        LSession.Activate('other');
      end
      else
      begin
        LSession.Activate('home');
      end;
      LSession.Select(AOwner);
      LLocal.Selection := AOwner;
      LLocal.View := LSession.ActiveViewID;
    end;
    Result := LSession.PrepareDesignRequest(LLocal, LScope.Revision);
  end;

  procedure Apply(const AProposal: TNyxStudioCollectionIntent; const AOwner: TNyxText = '');
  var
    LTicket: TNyxStudioDesignRequest;
    LWorkerTicket: TNyxStudioDesignRequest;
    LCandidate: INyxPreparedDesign;
    LReceived: INyxPreparedDesign;
    LBefore: TNyxText;
  begin
    LTicket := Capture(AProposal, AOwner);
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LWorkerTicket := ReadNyxStudioDesignRequest(
      TNyxDataValue.ParseJSON(LTicket.ToData.ToJSON));
    LCandidate := PrepareNyxStudioDesign(LWorkerTicket, LScope);
    Check(not LCandidate.Diagnostic.Defined,
      'Independent ' + IntToStr(Ord(AProposal.Action)) + ' preparation / ' +
      LCandidate.Diagnostic.Message);
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'Preparation never mutates the accepted pair');
    LReceived := ReceiveNyxPreparedDesign(
      TNyxDataValue.ParseJSON(LCandidate.ToData.ToJSON), LTicket, LScope);
    Check(LSession.CompleteDesignRequest(LTicket, LReceived) = nscApplied,
      'Typed operation publishes a changed paired candidate');
  end;

  procedure Refuse(const AProposal: TNyxStudioCollectionIntent; const AOwner: TNyxText = '');
  var
    LTicket: TNyxStudioDesignRequest;
    LCandidate: INyxPreparedDesign;
    LBefore: TNyxText;
  begin
    LTicket := Capture(AProposal, AOwner);
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LCandidate := PrepareNyxStudioDesign(LTicket, LScope);
    Check(LCandidate.Diagnostic.Defined, 'Invalid ordinary intent has an owned diagnostic');
    Check((LSession.CompleteDesignRequest(LTicket, LCandidate) = nscRejected) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'Rejected candidate cannot publish partial data/source/history');
  end;

  function ReplaceField(const AObject: TNyxDataValue; const AName: TNyxText;
    const AValue: TNyxDataValue): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LIndex: Integer;
  begin
    SetLength(LFields, AObject.Count);
    for LIndex := 0 to AObject.Count - 1 do
    begin
      LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

      if LFields[LIndex].Name = AName then
      begin
        LFields[LIndex] := NyxField(AName, AValue);
      end;
    end;
    Result := NyxObject(LFields);
  end;

  procedure RefuseDescriptor(const AData: TNyxDataValue);
  var
    LRead: TNyxStudioCollectionIntent;
    LFailed: Boolean;
  begin
    LFailed := False;
    try
      LRead := TNyxStudioCollectionIntent.FromData(AData);
      LRead.Validate;
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Malformed/private collection descriptor refuses before replay');
  end;

begin
  Result := 0;
  APair := Default(TNyxProjectPair);
  LSeed := CreateNyxCollectionQueueSeed;
  LSession := TNyxStudioSession.Create(LSeed);
  LScope := CaptureNyxSchemas;
  try
    LIntent := Proposal(scaCreate, 'scratch');
    Apply(LIntent);
    Check(LSession.Document.Collections.Has(LIntent.Key), 'Collection has its exact allocated name');
    LIntent := Proposal(scaAddField, 'scratch');
    LIntent.Kind := nskBoolean;
    Apply(LIntent);
    LIntent.Kind := nskInteger;
    Apply(LIntent);
    LIntent.Kind := nskNumber;
    Apply(LIntent);
    LIntent.Kind := nskText;
    Apply(LIntent);

    LIntent := Proposal(scaDefault, 'scratch');
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
    LIntent.Input := ssiEscapedText;
    LIntent.Value := NyxData(TNyxText('Café / 🌙') + #0 + ' / exact').ToJSON;
    Apply(LIntent);
    LIntent := Proposal(scaAddRow, 'scratch');
    LIntent.Item := NyxItem(LIntent.Key, 'row1');
    Apply(LIntent);
    LSnapshot := LSession.Document.Collections.Snapshot(LIntent.Key);
    Check(LSnapshot.ItemAt(0).GetValue(NyxTextField('caption')) =
      TNyxText('Café / 🌙') + #0 + ' / exact', 'New rows retain exact typed/Unicode/NUL defaults');

    LIntent := Proposal(scaCell, 'scratch');
    LIntent.Item := NyxItem(LIntent.Key, 'row1');
    LIntent.Field := TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField('boolean1'));
    LIntent.Input := ssiBoolean;
    LIntent.Value := 'true';
    Apply(LIntent);
    LSnapshot := LSession.Document.Collections.Snapshot(LIntent.Key);
    Check(LSnapshot.ItemAt(0).GetValue(NyxBooleanField('boolean1')), 'Cell value stays Boolean');

    LIntent.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('integer1'));
    LIntent.Input := ssiInteger;
    LIntent.Value := '-2147483648';
    Apply(LIntent);
    LSnapshot := LSession.Document.Collections.Snapshot(LIntent.Key);
    Check(LSnapshot.ItemAt(0).GetValue(NyxIntegerField('integer1')) = Low(Integer),
      'Signed Integer boundary survives private admission and publication');

    LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('number1'));
    LIntent.Input := ssiNumber;
    LIntent.Value := '0.125';
    Apply(LIntent);
    LSnapshot := LSession.Document.Collections.Snapshot(LIntent.Key);
    Check(LSnapshot.ItemAt(0).GetValue(NyxNumberField('number1')) = 0.125,
      'Number value survives without display coercion');
    LIntent.Value := '1e9999';
    Refuse(LIntent);
    LIntent := Proposal(scaRemoveRow, 'scratch');
    LIntent.Item := NyxItem(LIntent.Key, 'row1');
    Apply(LIntent);
    Check(LSession.Document.Collections.Snapshot(LIntent.Key).Count = 0, 'Exact row removal retains schema');
    LIntent := Proposal(scaRemove, 'scratch');
    Apply(LIntent);
    Check(not LSession.Document.Collections.Has(LIntent.Key), 'Unreferenced collection removal is complete');

    LIntent := Proposal(scaBind, 'tasks');
    LIntent.Projection := cpTable;
    Apply(LIntent, 'unbound-table');
    Check(ViewSpec('unbound-table').Count = 5, 'Binding uses all current typed schema fields');

    LIntent := Proposal(scaScope, 'tasks');
    LIntent.Projection := cpList;
    LIntent.Scope := csApplication;
    Apply(LIntent, 'left-list');
    Check((ViewSpec('left-list').Scope = csApplication) and
      (ViewSpec('right-list').Scope = csInstance), 'Reusable instance view overrides remain independent');

    LIntent := Proposal(scaTitle, 'tasks');
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
    LIntent.Value := 'Task details';
    Apply(LIntent, 'tasks-table');
    LSpec := ViewSpec('tasks-table');
    Check((LSpec.ColumnAt(0).FieldName = 'priority') and
      (LSpec.ColumnAt(1).Title = 'Task details') and (LSpec.ColumnAt(1).Kind = nskText),
      'Column title resolves field identity across differing schema/view order');
    LIntent := Proposal(scaMode, 'tasks');
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
    LIntent.Mode := cmEditable;
    Apply(LIntent, 'tasks-table');
    Check(ViewSpec('tasks-table').ColumnAt(1).Mode = cmEditable, 'Column editing policy remains typed');

    LIntent := Proposal(scaParent, 'tasks');
    LIntent.Projection := cpTree;
    Apply(LIntent, 'tasks-tree');
    Check(ViewSpec('tasks-tree').ParentField = '', 'Undefined optional parent removes its mapping');
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('parent'));
    Apply(LIntent, 'tasks-tree');
    Check(ViewSpec('tasks-tree').ParentField = 'parent', 'Typed text parent mapping restores exactly');

    LIntent := Proposal(scaRemoveColumn, 'tasks');
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField('done'));
    Apply(LIntent, 'tasks-table');
    LIntent := Proposal(scaTitle, 'tasks');
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField('done'));
    LIntent.Value := 'Removed';
    Refuse(LIntent, 'tasks-table');
    LIntent := Proposal(scaAddColumn, 'tasks');
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField('done'));
    Apply(LIntent, 'tasks-table');
    Check(ViewSpec('tasks-table').ColumnAt(3).FieldName = 'done', 'Restored column has its exact typed family');

    LIntent := Proposal(scaClear, 'tasks');
    LIntent.Projection := cpTable;
    Apply(LIntent, 'unbound-table');
    Check(not ViewSpec('unbound-table').Defined, 'Clear records an explicit absent collection binding');
    LIntent := Proposal(scaInherit, 'tasks');
    LIntent.Projection := cpList;
    Apply(LIntent, 'left-list');
    Check(ViewSpec('left-list').Scope = csInstance, 'Inheritance restores the reusable definition');
    LIntent := Proposal(scaClear, 'tasks');
    Apply(LIntent, 'left-list');
    Check(not ViewSpec('left-list').Defined, 'Reusable clear masks its effective contract');
    LIntent := Proposal(scaInherit, 'tasks');
    Apply(LIntent, 'left-list');
    Check(ViewSpec('left-list').Defined and (ViewSpec('left-list').Scope = csInstance),
      'Isolated inheritance restores an exact previously cleared reusable contract');

    LIntent := Proposal(scaSelection, 'tasks');
    LIntent.Projection := cpTable;
    LIntent.SelectionMode := nsmMultiple;
    LRequest := Capture(LIntent, 'tasks-table');
    LPairBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LPrepared := PrepareNyxStudioDesign(LRequest, LScope);
    Check(not LPrepared.Diagnostic.Defined, 'Selection policy prepares independently');
    LSession.Select('unbound-table');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'Selection policy publishes at its captured authored owner');
    LPairAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check((LSession.SelectedID = 'unbound-table') and
      (LSession.Document.Find('tasks-table').CollectionView.SelectionMode = nsmMultiple),
      'Later navigation stays independent of the accepted view patch');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LPairBefore, 'One Undo restores exact source and design');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LPairAfter, 'One Redo restores the accepted pair');

    LBase := LSession.Source;
    LDraft := LBase + #10 + '{ Independent handwritten application notes }';
    LSession.SetSourceDraft(LDraft);
    LIntent := Proposal(scaCell, 'tasks');
    LIntent.Item := NyxItem(LIntent.Key, 'alpha');
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
    LIntent.Input := ssiEscapedText;
    LIntent.Value := NyxData(TNyxText('Ready for release 🌙') + #10 + 'Café').ToJSON;
    Apply(LIntent);
    Check((LSession.DraftSource = LDraft) and (LSession.SourceDraftBase = LBase),
      'Ordinary collection data preserves the exact independent source draft and base');
    LSession.DiscardSourceDraft;
    Check(Pos('function CollectionWorkshopNote: TNyxText;', LSession.Source) > 0,
      'All ordinary collection operations retain handwritten application helpers');
    LImportCount := 0;
    LImportCursor := 1;
    repeat
      LImportPosition := Pos('nyx.collections.view.types',
        Copy(LSession.Source, LImportCursor, MaxInt));

      if LImportPosition = 0 then
      begin
        Break;
      end;
      Inc(LImportCount);
      Inc(LImportCursor, LImportPosition + Length('nyx.collections.view.types') - 1);
    until False;
    Check(LImportCount = 1, 'Repeated ordinary edits retain one complete qualified import');
    APair := LSession.ProjectSnapshot;

    LIntent := Proposal(scaCell, 'tasks');
    LIntent.Item := NyxItem(LIntent.Key, 'alpha');
    LIntent.Field := TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField('caption'));
    LIntent.Input := ssiBoolean;
    LIntent.Value := 'false';
    Refuse(LIntent);
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('absent'));
    LIntent.Input := ssiText;
    Refuse(LIntent);
    LIntent := Proposal(scaScope, 'tasks');
    LIntent.Projection := cpTree;
    LIntent.Scope := csInstance;
    Refuse(LIntent, 'tasks-table');

    LIntent := Proposal(scaCell, 'tasks');
    LIntent.Item := NyxItem(LIntent.Key, 'alpha');
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
    LIntent.Value := 'Stale load';
    LRequest := Capture(LIntent);
    LPrepared := PrepareNyxStudioDesign(LRequest, LScope);
    LSession.LoadProject(LSeed);
    LPairBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    Check((LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LPairBefore),
      'Same IDs in a new load cannot receive an old prepared collection edit');

    LIntent.Item := NyxItem(NyxCollection('other'), 'alpha');
    LRejected := False;
    try
      LIntent.Validate;
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Item scope cannot be confused with another collection');

    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaCollection;
    LEdit.Collection := Proposal(scaCreate, 'ticket');
    LRequest := LSession.PrepareDesignRequest(LEdit, LScope.Revision);
    LWire := LRequest.ToData;
    Check(LWire.Field('version').AsInteger = 5, 'Private collection ticket uses version five');
    LIntent := Proposal(scaCell, 'tasks');
    LIntent.Item := NyxItem(LIntent.Key, 'alpha');
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
    LWire := LIntent.ToData;
    RefuseDescriptor(NyxNull);
    RefuseDescriptor(ReplaceField(LWire, 'action', NyxData(17)));
    RefuseDescriptor(ReplaceField(LWire, 'action', NyxData('cell')));
    RefuseDescriptor(ReplaceField(LWire, 'projection', NyxData(-1)));
    RefuseDescriptor(ReplaceField(LWire, 'input', NyxData(Ord(ssiInteger))));
    RefuseDescriptor(ReplaceField(LWire, 'item', NyxObject([
      NyxField('collection', NyxData('other')), NyxField('id', NyxData('alpha'))])));
    RefuseDescriptor(ReplaceField(LWire, 'field', NyxObject([
      NyxField('name', NyxData('caption')), NyxField('kind', NyxData(4))])));
    RefuseDescriptor(ReplaceField(LWire, 'field', NyxObject([
      NyxField('name', NyxData('caption')), NyxField('kind', NyxData(0)),
      NyxField('extra', NyxNull)])));
    RefuseDescriptor(ReplaceField(LWire, 'scope', NyxData(Ord(csInstance))));
  finally
    LSession.Free;
  end;
end;

end.
