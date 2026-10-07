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

unit nyx.test.datepolicy;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.studio.projects;

{ Exact semantic command/source/history qualification on an independently owned
  accepted seed. Runs unchanged natively and in pas2js; never owns a listener or
  user's project. Return count includes strict refusal/preservation checks. }
function RunNyxDatePolicyTests(const ASeed: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.text, nyx.data, nyx.json, nyx.types, nyx.dates, nyx.contract, nyx.state,
  nyx.codec, nyx.codegen, nyx.model, nyx.studio.agents,
  nyx.studio.edits;

function RunNyxDatePolicyTests(const ASeed: TNyxProjectPair): Integer;
var
  LAgent: TNyxAgentSession;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LDomain: TNyxDataValue;
  LFields: TNyxDataValue;
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  LRevision: Integer;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create(TNyxText('Date policy: ') + AReason);
    end;
    Inc(Result);
  end;

  function PairText: TNyxText;
  begin
    Result := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
  end;

  function Query(const AID, AScope: TNyxText; AOffset: Integer = 0): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_node', 'Policy workshop', NyxObject([
      NyxField('id', NyxData(AID)), NyxField('limit', NyxData(1)),
      NyxField('valueDomain', NyxData(True)), NyxField('domainScope', NyxData(AScope)),
      NyxField('domainOffset', NyxData(AOffset)), NyxField('domainLimit', NyxData(1))]));
    Check(not NyxAgentHas(Result, 'code'), 'bounded policy query is admitted');
    Result := Result.Field('valueDomain');
  end;

  function Transaction(const AID: TNyxText;
    const AOperations: array of TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)),
      NyxField('operations', NyxArray(AOperations))]);
  end;

  procedure Refuse(const ARequest: TNyxDataValue; const AReason: TNyxText);
  var
    LSaved: TNyxText;
    LResult: TNyxDataValue;
    LRefused: Boolean;
  begin
    LSaved := PairText;
    LRefused := False;
    try
      LResult := LAgent.Call('nyx_transaction', 'Policy workshop', ARequest);
      LRefused := NyxAgentHas(LResult, 'code');
    except
      on LException: Exception do
      begin
        { The direct portable boundary raises admission errors. Its owning HTTP
          transport converts them to structured refusal; this fixture is local. }
        if not ((LException is ENyxModel) or (LException is ENyxState) or
          (LException is ENyxJSON)) then
        begin
          raise;
        end;
        LRefused := True;
      end;
    end;
    Check(LRefused, AReason + TNyxText(' refuses'));
    Check(PairText = LSaved, AReason + TNyxText(' retains the exact accepted/draft pair'));
    Check(LAgent.Revision = LRevision, AReason + TNyxText(' retains revision/history'));
  end;

begin
  Result := 0;
  LAgent := TNyxAgentSession.Create(ASeed);
  try
    LRevision := LAgent.Revision;
    LBefore := PairText;
    LDomain := Query('first-arrival', 'effective');
    Check(LDomain.Field('format').AsText = 'date', 'named inherited date part has calendar semantics');
    Check(not LDomain.Field('localDeclared').AsBoolean, 'no fabricated local value declaration');
    Check(LDomain.Field('minimum').Kind = ndNull, 'unbounded dates report absent bounds explicitly');
    LDomain := Query('first-arrival', 'local');
    Check(not LDomain.Field('defined').AsBoolean, 'local absence is distinct from inherited domain');
    { Release the independently decoded document; the snapshot itself owns values. }
    LDocument := TNyxCodec.Decode(ASeed.Design);
    try
      LFields := LDocument.Components[0].Contract.Snapshot;
    finally
      LDocument.Free;
    end;
    LArgs := Transaction('date-policies', [
      NyxSetValueDomain(NyxControl('first-arrival'), NyxDateDomain
        .Range(NyxDate(2026, 10, 1), NyxDate(2026, 10, 31))
        .Choices([NyxDate(2026, 10, 6), NyxDate(2026, 10, 9), NyxNoDate])).ToData,
      NyxSetValueDomain(NyxControl('second-arrival'), NyxDateDomain
        .Range(NyxDate(2026, 11, 1), NyxDate(2026, 11, 30))).ToData]);
    LReply := LAgent.Call('nyx_transaction', 'Policy workshop', LArgs);
    Check(not NyxAgentHas(LReply, 'code'), 'related typed policies publish together');
    Inc(LRevision);
    Check(LAgent.Revision = LRevision, 'one group advances exactly one revision');
    LAfter := PairText;
    LPair := DecodeNyxProject(LAfter);
    Check(Pos('.Value(NyxDateDomain.Range(NyxDate(2026, 10, 1)', LPair.Source) > 0,
      'adjacent Pascal uses typed fluent dates and constraints');
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      { Paired editing retains unchanged legacy source spelling from the running
        semantic seed. Fresh current generation uses the typed date constructor. }
      Check(Pos('.Value(NyxDate(2026, 10, 6))', TNyxCodegen.Generate(LDocument)) > 0,
        'fresh reusable value overrides use specialized typed date values');
      Check(LDocument.Components[0].Contract.Snapshot.ToJSON = LFields.ToJSON,
        'definition fields and explicit NoValue declaration stay exact');
      Check(LDocument.Find('first-arrival').Prop('value') = '2026-10-06',
        'constraint changes retain authored defaults');
    finally
      LDocument.Free;
    end;
    LDomain := Query('first-arrival', 'effective', 1);
    Check((LDomain.Field('totalChoices').AsInteger = 3) and
      (LDomain.Field('choices').Count = 1) and
      (LDomain.Field('choices').Item(0).Field('index').AsInteger = 1) and
      (LDomain.Field('choices').Item(0).Field('value').AsText = '2026-10-09'),
      'choices are paged exact canonical dates');
    LDomain := Query('second-arrival', 'effective');
    Check(LDomain.Field('minimum').AsText = '2026-11-01',
      'second reusable instance retains its independent domain');
    LReply := LAgent.Call('nyx_transaction', 'Policy workshop', LArgs);
    Check(not NyxAgentHas(LReply, 'code') and (PairText = LAfter) and
      (LAgent.Revision = LRevision), 'exact grouped retry spends no history');
    Refuse(Transaction('invalid-calendar', [
      NyxObject([NyxField('op', NyxData('update')), NyxField('id', NyxData('trip-title')),
        NyxField('properties', NyxObject([NyxField('text', NyxData('Must never publish'))]))]),
      NyxObject([NyxField('op', NyxData('value-domain-set')),
        NyxField('id', NyxData('first-arrival')), NyxField('domain', NyxObject([
          NyxField('type', NyxData('text')), NyxField('format', NyxData('date')),
          NyxField('min', NyxData('2026-02-30')), NyxField('max', NyxData('2026-12-31'))]))])]),
      'impossible date in a multi-operation group');
    Refuse(Transaction('default-outside', [
      NyxSetValueDomain(NyxControl('first-arrival'), NyxDateDomain
        .Range(NyxDate(2026, 10, 8), NyxDate(2026, 10, 31))).ToData]),
      'existing default outside replacement bounds');
    Refuse(Transaction('wrong-family', [
      NyxSetValueDomain(NyxControl('first-arrival'), NyxBooleanDomain).ToData]),
      'mismatched scalar family');
    Refuse(Transaction('numeric-date-bound', [
      NyxObject([NyxField('op', NyxData('value-domain-set')),
        NyxField('id', NyxData('first-arrival')), NyxField('domain', NyxObject([
          NyxField('type', NyxData('text')), NyxField('format', NyxData('date')),
          NyxField('min', NyxData(20261001)), NyxField('max', NyxData(20261031))]))])]),
      'numeric date bounds at JSON boundary');
    LAgent.InheritPermission(apReadOnly);
    Refuse(Transaction('readonly-policy', [
      NyxInheritValueDomain(NyxControl('first-arrival')).ToData]), 'operator read-only policy');
    LAgent.InheritPermission(apEdit);
    LReply := LAgent.Call('nyx_history', 'Policy workshop', NyxObject([
      NyxField('direction', NyxData('undo')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('undo-date-policy'))]));
    Check(not NyxAgentHas(LReply, 'code') and (PairText = LBefore),
      'one Undo restores the exact paired policy group');
    Inc(LRevision);
    LReply := LAgent.Call('nyx_history', 'Policy workshop', NyxObject([
      NyxField('direction', NyxData('redo')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('redo-date-policy'))]));
    Check(not NyxAgentHas(LReply, 'code') and (PairText = LAfter),
      'one Redo restores the exact accepted Pascal/design');
    Inc(LRevision);
    LReply := LAgent.Call('nyx_transaction', 'Policy workshop',
      Transaction('restore-first-policy', [NyxInheritValueDomain(NyxControl('first-arrival')).ToData]));
    Check(not NyxAgentHas(LReply, 'code'), 'typed restoration removes only the local declaration');
    Inc(LRevision);
    LDomain := Query('first-arrival', 'effective');
    Check(not LDomain.Field('localDeclared').AsBoolean and
      (LDomain.Field('minimum').Kind = ndNull), 'restoration reveals inherited date constraints');
    LDomain := Query('second-arrival', 'effective');
    Check(LDomain.Field('minimum').AsText = '2026-11-01',
      'restoring one instance retains the other instance policy');
    Refuse(Transaction('missing-local', [
      NyxInheritValueDomain(NyxControl('first-arrival')).ToData]), 'absent local restoration');
  finally
    LAgent.Free;
  end;
end;

end.
