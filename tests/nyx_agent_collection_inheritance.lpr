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



program nyx_agent_collection_inheritance;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.collections, nyx.collections.view.types,
  nyx.collections.selection, nyx.studio.projects, nyx.studio.agents,
  nyx.studio.collectionedits, nyx.studio.collectionintent, nyx.studio.stateedits,
  nyx.test.agent.collections {$ifdef PAS2JS}, Web{$endif};

var
  LAgent: TNyxAgentSession;
  LIntent: TNyxStudioCollectionIntent;
  LRevision: Integer;
  LReply: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LSeed: TNyxProjectPair;
  LChecks: Integer;
  LRejected: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Masked inheritance: ' + AReason);
  end;
  Inc(LChecks);
end;

function PairText: TNyxText;
begin
  Result := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
end;

function Apply(const AID: TNyxText; const AChanges: array of TNyxCollectionChange):
  TNyxDataValue;
begin
  Result := LAgent.Call('nyx_collections', 'Scooty', NyxObject([
    NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData(AID)), NyxField('changes', NyxCollectionPatch(AChanges).ToData)]));
  LRevision := LAgent.Revision;
end;

function Binding(const AOwner: TNyxText): TNyxDataValue;
begin
  Result := LAgent.Call('nyx_collections', 'Scooty', NyxObject([
    NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData(AOwner)),
    NyxField('limit', NyxData(1))]));
end;

procedure History(const ADirection, AID: TNyxText);
begin
  LAgent.Call('nyx_history', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
    NyxField('direction', NyxData(ADirection))]));
  LRevision := LAgent.Revision;
end;

begin
  LAgent := nil;
  try
    try
      LSeed := CreateNyxAgentCollectionSeed;
      LAgent := TNyxAgentSession.Create(LSeed);
      LRevision := LAgent.Revision;
      Apply('seed', [
        NyxDefineCollection(NyxCollection('tasks'),
          NyxCollectionSchema.Text(NyxTextField('caption'), 'Task'), []),
        NyxDefineCollection(NyxCollection('Tasks'),
          NyxCollectionSchema.Text(NyxTextField('caption'), 'Other task'), []),
        NyxBindCollection(NyxBindingOwner('definition-list'), cpList,
          NyxCollectionView(NyxCollection('tasks')).Scoped(csInstance)
            .Selection(nsmMultiple).Column(NyxTextField('caption'), 'Task 🌙')),
        NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable,
          NyxCollectionView(NyxCollection('tasks')).Column(NyxTextField('caption'), 'Task'))]);
      LReply := Binding('first-items');
      Check(LReply.Field('inherited').AsBoolean and
        (LReply.Field('restorable').Kind = ndNull), 'ordinary inherited view has no local mask');
      LBefore := PairText;
      LIntent := Default(TNyxStudioCollectionIntent);
      LIntent.Action := scaClear;
      LIntent.Key := NyxCollection('tasks');
      Apply('clear', [NyxCollectionIntentChange(NyxBindingOwner('first-items'), LIntent)]);
      LAfter := PairText;
      LReply := Binding('first-items');
      Check(LReply.Field('local').Field('cleared').AsBoolean and
        LReply.Field('effective').Field('cleared').AsBoolean and
        not LReply.Field('inherited').AsBoolean, 'local clear remains effective');
      Check((LReply.Field('restorable').Field('key').AsText = 'tasks') and
        (LReply.Field('restorable').Field('scope').AsText = 'instance') and
        (LReply.Field('restorable').Field('selection').AsText = 'multiple'),
        'restorable descriptor retains exact inherited family/scope/selection');
      Check((LReply.Field('restorable').Field('columns').Count = 1) and
        (LReply.Field('restorable').Field('columns').Item(0).Field('kind').AsText = 'text'),
        'restorable columns obey the same bounded typed context');
      Check((PairText = LAfter) and
        (LAgent.Call('nyx_session', 'Scooty', NyxObject([])).Field('selection').AsText = 'home'),
        'exact-owner query preserves independent selection and paired history');
      LReply := LAgent.Call('nyx_collections', 'Scooty', NyxObject([
        NyxField('mode', NyxData('column')), NyxField('owner', NyxData('first-items')),
        NyxField('field', NyxData('caption')), NyxField('source', NyxData('restorable')),
        NyxField('offset', NyxData(5)), NyxField('count', NyxData(1))]));
      Check((LReply.Field('title').Field('text').AsText = TNyxText('🌙')) and
        (PairText = LAfter), 'masked column title has an exact read-only Unicode window');
      LReply := Binding('second-card');
      Check((LReply.Field('restorable').Kind = ndNull) and
        (LReply.Field('local').Kind = ndNull), 'other authored owner never borrows this mask');
      LIntent.Action := scaInherit;
      LIntent.Key := NyxCollection('Tasks');
      LRejected := False;
      try
        Apply('foreign-key', [NyxCollectionIntentChange(NyxBindingOwner('first-items'), LIntent)]);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (PairText = LAfter), 'case-distinct key refuses restoration atomically');
      LIntent.Key := NyxCollection('tasks');
      Apply('restore', [NyxCollectionIntentChange(NyxBindingOwner('first-items'), LIntent)]);
      Check(PairText = LBefore, 'restoration returns exact accepted pair');
      LReply := Binding('first-items');
      Check(LReply.Field('inherited').AsBoolean and
        (LReply.Field('restorable').Kind = ndNull), 'removed mask restores effective inheritance');
      History('undo', 'undo-restore');
      Check(PairText = LAfter, 'one Undo restores exact mask/source');
      History('redo', 'redo-restore');
      Check(PairText = LBefore, 'one Redo restores exact inherited source');
      LIntent.Action := scaClear;
      LIntent.Projection := cpTable;
      Apply('clear-standalone', [NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent)]);
      LReply := Binding('tasks-table');
      Check(LReply.Field('local').Field('cleared').AsBoolean and
        (LReply.Field('restorable').Kind = ndNull), 'standalone absent inheritance stays explicit');
      LReply := Binding('home');
      Check(not LReply.Field('supported').AsBoolean and (LReply.Field('restorable').Kind = ndNull),
        'unrelated noncollection control exposes no inherited collection guess');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-collection-inheritance', 'passed');
      {$else}
      WriteLn('PASS ', LChecks, ' exact masked-inheritance context/paired checks');
      {$endif}
    except
      on LException: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-nyx-collection-inheritance', 'failed');
        document.body.textContent := LException.Message;
        {$else}
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    LAgent.Free;
  end;
end.
