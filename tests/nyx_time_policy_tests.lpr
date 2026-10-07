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

program nyx_time_policy_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.codec, nyx.model,
  nyx.generated.time, nyx.studio.projects, nyx.studio.agents,
  nyx.types, nyx.times, nyx.contract, nyx.controls, nyx.times.editor, nyx.studio.edits,
  nyx.schema, nyx.state, nyx.json;

var
  GChecks: Integer;
  GAgent: TNyxAgentSession;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Clock policy: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Exercise the real semantic session against an independently owned exact
  public-Pascal companion. This is local admission, not HTTP authentication or
  an edit to the active MCP project. Transport/observing gates remain separate. }
function Query(const AID: TNyxText): TNyxDataValue;
begin
  Result := GAgent.Call('nyx_node', 'Clock workshop', NyxObject([
    NyxField('id', NyxData(AID)), NyxField('limit', NyxData(1)),
    NyxField('valueDomain', NyxData(True)), NyxField('domainLimit', NyxData(1))]));
  Check(not NyxAgentHas(Result, 'code'), 'bounded domain query admits');
  Result := Result.Field('valueDomain');
end;

function PairText: TNyxText;
begin
  Result := GAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
    .Field('project').AsText;
end;

function Transaction(const AID: TNyxText;
  const AOperations: array of TNyxDataValue; ARevision: Integer = -1): TNyxDataValue;
begin

  if ARevision < 0 then
  begin
    ARevision := GAgent.Revision;
  end;
  Result := NyxObject([NyxField('operationId', NyxData(AID)),
    NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('operations', NyxArray(AOperations))]);
end;

procedure Refuse(const AArguments: TNyxDataValue; const AReason: TNyxText);
var
  LBefore: TNyxText;
  LRevision: Integer;
  LRefused: Boolean;
  LReply: TNyxDataValue;
begin
  LBefore := PairText;
  LRevision := GAgent.Revision;
  LRefused := False;
  try
    LReply := GAgent.Call('nyx_transaction', 'Clock workshop', AArguments);
    LRefused := NyxAgentHas(LReply, 'code');
  except
    on LException: Exception do
    begin

      if not ((LException is ENyxModel) or (LException is ENyxState) or
        (LException is ENyxJSON)) then
      begin
        raise;
      end;
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason + ' refuses');
  Check((PairText = LBefore) and (GAgent.Revision = LRevision),
    AReason + ' retains exact pair/history/revision');
end;

var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LStream: TFileStream;
  LReply: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LEditor: INyxCard;
  LChange: TNyxTimeDomainEditorChange;
  LDomain: TNyxValueDomain;
  LOwner: TNyxNode;
  LIndex: Integer;
const
  CFields: array[0..4] of TNyxText =
    ('start-time', 'earliest-time', 'latest-time', 'choice-time', 'optional-time');
begin
  LDocument := nil;
  GAgent := nil;
  try
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LSource, LStream.Size);

      if LSource <> '' then
      begin
        LStream.ReadBuffer(LSource[1], Length(LSource));
      end;
    finally
      LStream.Free;
    end;
    LDocument := BuildNyxDocument;
    GAgent := TNyxAgentSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument), LSource));
    LReply := Query('start-time');
    Check(LReply.Field('format').AsText = 'time', 'clock format is discoverable');
    Check(LReply.Field('crossesMidnight').AsBoolean and
      (LReply.Field('stepMilliseconds').AsInteger = 1500), 'overnight step context is exact');
    LReply := Query('earliest-time');
    Check((LReply.Field('minimum').AsText = '08:30') and
      (LReply.Field('maximum').Kind = ndNull), 'minimum-only context never fabricates a maximum');
    Check(LReply.Field('stepBase').AsText = '08:30', 'minimum is the exact step base');
    LReply := Query('latest-time');
    Check((LReply.Field('minimum').Kind = ndNull) and
      (LReply.Field('maximum').AsText = '10:00') and
      (LReply.Field('step').AsText = 'any'), 'maximum-only and explicit Any remain independent');
    LReply := Query('choice-time');
    Check((LReply.Field('choices').Count = 1) and (LReply.Field('totalChoices').AsInteger = 2),
      'exact choices remain paged instead of dumping their list');
    LReply := Query('optional-time');
    Check(not LReply.Field('stepDeclared').AsBoolean and
      (LReply.Field('step').Kind = ndNull), 'absence is distinct from explicit Any');

    for LIndex := Low(CFields) to High(CFields) do
    begin
      LOwner := LDocument.Find(CFields[LIndex]);
      LDomain := NyxNodeValueDomain(LOwner);
      LEditor := NewNyxTimeDomainEditor('clock-policy', NyxControl(LOwner.ID),
        LOwner.Contract, LDomain);
      Check(CaptureNyxTimeDomainEditor(LEditor.Node.Find(
        NyxTimeDomainEditorFieldID('clock-policy', ntfApply)), LEditor.Node, LChange),
        'public composed policy captures its mounted typed intent');
      Check(LChange.Domain.ToData.ToJSON = LDomain.ToData.ToJSON,
        'unchanged editor retains exact bounds/steps/choice precision');
      LEditor := nil;
    end;
    LBefore := PairText;
    LReply := GAgent.Call('nyx_transaction', 'Clock workshop', Transaction('clock-policy-group', [
      NyxSetValueDomain(NyxControl('start-time'),
        NyxTimeDomain.Range(NyxTime(22, 0), NyxTime(2, 0)).StepMilliseconds(500)).ToData,
      NyxSetValueDomain(NyxControl('earliest-time'), NyxTimeDomain.Minimum(NyxTime(7, 0))).ToData]));
    Check(not NyxAgentHas(LReply, 'code'), 'related typed clock policies publish as one group');
    LAfter := PairText;
    Check(LAfter <> LBefore, 'paired source/design change together');
    LReply := GAgent.Call('nyx_history', 'Clock workshop', NyxObject([
      NyxField('direction', NyxData('undo')), NyxField('expectedRevision', NyxData(GAgent.Revision)),
      NyxField('operationId', NyxData('undo-clock-policy'))]));
    Check(not NyxAgentHas(LReply, 'code') and (PairText = LBefore),
      'one Undo restores the entire exact paired group');
    LReply := GAgent.Call('nyx_history', 'Clock workshop', NyxObject([
      NyxField('direction', NyxData('redo')), NyxField('expectedRevision', NyxData(GAgent.Revision)),
      NyxField('operationId', NyxData('redo-clock-policy'))]));
    Check(not NyxAgentHas(LReply, 'code') and (PairText = LAfter),
      'one Redo restores the exact source and policies');
    Refuse(Transaction('stale-clock-policy', [NyxSetValueDomain(NyxControl('start-time'),
      NyxTimeDomain.AnyStep).ToData], 0), 'stale clock revision');
    Refuse(Transaction('invalid-clock-step', [NyxObject([
      NyxField('op', NyxData('value-domain-set')), NyxField('id', NyxData('start-time')),
      NyxField('domain', NyxObject([NyxField('type', NyxData('text')),
        NyxField('format', NyxData('time')), NyxField('step', NyxData(1.5))]))])]),
      'fractional milliseconds');
    Refuse(Transaction('invalid-clock-group', [
      NyxSetValueDomain(NyxControl('earliest-time'), NyxTimeDomain.AnyStep).ToData,
      NyxSetValueDomain(NyxControl('start-time'), NyxTimeDomain.Maximum(NyxTime(0, 0))).ToData]),
      'dependent default outside a grouped clock policy');
    WriteLn('PASS ', GChecks, ' local semantic clock-policy checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL after ', GChecks, ' / ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  GAgent.Free;
  LEditor := nil;
  LDocument.Free;
end.
