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
program nyx_handler_edit_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.callbacks, nyx.model, nyx.source,
  nyx.studio.session, nyx.studio.projects, nyx.studio.agents, nyx.studio.handleredits
  {$ifdef PAS2JS}, Web{$endif};

var
  GAgent: TNyxAgentSession;
  GStudio: TNyxStudioSession;
  GCount: Integer;
  GRevision: Integer;
  GFirst: TNyxHandlerRef;
  GSecond: TNyxHandlerRef;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

function Pair: TNyxProjectPair;
begin
  Result := DecodeNyxProject(GAgent.Exchange(NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))])).Field('project').AsText);
end;

function Change(const AHandler: TNyxHandlerRef; const AExpected, AImplementation: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('handler', NyxData(AHandler.Name)),
    NyxField('expected', NyxData(AExpected)), NyxField('implementation', NyxData(AImplementation))]);
end;

function Args(const AID: TNyxText; const AChanges: TNyxDataValue;
  ARevision: Integer = 0): TNyxDataValue;
begin

  if ARevision = 0 then
  begin
    ARevision := GRevision;
  end;
  Result := NyxObject([NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(ARevision)), NyxField('operationId', NyxData(AID)),
    NyxField('changes', AChanges)]);
end;

procedure Refuse(const AArgs: TNyxDataValue; const AReason: TNyxText);
var
  LBefore: TNyxText;
  LSummary: TNyxText;
  LRefused: Boolean;
begin
  LBefore := EncodeNyxProject(Pair);
  LSummary := GAgent.Call('nyx_session', 'test', NyxObject([])).ToJSON;
  LRefused := False;
  try
    GAgent.Call('nyx_pascal', 'test', AArgs);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
  Check((LBefore = EncodeNyxProject(Pair)) and
    (GAgent.Revision = GRevision), 'Refusal preserves exact pair and revision');
  { Activity is deliberately observable; content, selection and history remain. }
  Check(GAgent.Call('nyx_session', 'test', NyxObject([])).Field('selection').AsText =
    TNyxDataValue.ParseJSON(LSummary).Field('selection').AsText, 'Refusal preserves selection');
end;

procedure RefuseRead(const ASource: TNyxText; const AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    ReadNyxHandlerSource(ASource, GFirst);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

function ReplaceFirst(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
  LParts: TNyxStrings;
begin
  LPosition := Pos(ABefore, ASource);
  Check(LPosition > 0, 'Fixture replacement has an exact source anchor');
  LParts := TNyxStrings.Create;
  try
    { Avoid the Windows ANSI SysUtils.StringReplace boundary. Fixture setup
      must retain the same exact Unicode bytes that the product must preserve. }
    LParts.Add(Copy(ASource, 1, LPosition - 1));
    LParts.Add(AAfter);
    LParts.Add(Copy(ASource, LPosition + Length(ABefore), MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

{ BEGIN pending-main-draft facts helper }
{ Own a copy of the existing callback seed. The extra ordinary unit helper gives
  declaration/routine readers a real indexed target without treating generated
  infrastructure or a callback class member as an editable unit declaration. }
procedure EqualMainDraftFacts(const ASeed: TNyxProjectPair;
  const AHandler: TNyxHandlerRef);
var
  LAgent: TNyxAgentSession;
  LSeed: TNyxProjectPair;
  LFrame: TNyxProjectPair;
  LCatalog: TNyxRoutineCatalog;
  LRoutine: TNyxRoutineSource;
  LDeclaration: TNyxRoutineDeclarationSource;
  LHandler: TNyxHandlerSource;
  LIndex: Integer;
  LFound: Boolean;
  LRevision: Integer;
  LBefore: TNyxText;
  LStamp: TNyxText;
  LState: TNyxDataValue;

  function State: TNyxDataValue;
  begin
    Result := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))])).Field('session');
  end;

  procedure Facts(APending: Boolean);
  var
    LValue: TNyxDataValue;
  begin
    Check(LAgent.PendingDraft = APending, 'public getter retains explicit main draft state');
    Check(LAgent.Call('nyx_session', 'Pending facts', NyxObject([]))
      .Field('pendingDraft').AsBoolean = APending, 'session summary agrees with the main draft flag');
    LValue := LAgent.Call('nyx_pascal', 'Pending facts', NyxObject([
      NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(AHandler.Name)),
      NyxField('count', NyxData(8))]));
    Check((LValue.Field('pendingDraft').AsBoolean = APending) and
      (LValue.Field('text').AsText = Copy(LHandler.Code, 1, 8)),
      'handler pending fact accompanies exact bounded accepted text');
    Check(LAgent.Call('nyx_pascal', 'Pending facts', NyxObject([
      NyxField('mode', NyxData('imports')), NyxField('section', NyxData('interface')),
      NyxField('limit', NyxData(1))])).Field('pendingDraft').AsBoolean = APending,
      'accepted import inspection retains the explicit pending main flag');
    LValue := LAgent.Call('nyx_pascal', 'Pending facts', NyxObject([
      NyxField('mode', NyxData('routines')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('limit', NyxData(1))]));
    Check((LValue.Field('pendingDraft').AsBoolean = APending) and
      (LValue.Field('total').AsInteger = LCatalog.Count),
      'legacy main-unit routine catalog agrees with its actual source index');
    LValue := LAgent.Call('nyx_pascal', 'Pending facts', NyxObject([
      NyxField('mode', NyxData('routine')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('routine', NyxData(LRoutine.Routine.Name)), NyxField('count', NyxData(8))]));
    Check((LValue.Field('pendingDraft').AsBoolean = APending) and
      (LValue.Field('text').AsText = Copy(LRoutine.Code, 1, 8)),
      'routine body pending fact accompanies the indexed accepted implementation');
    LValue := LAgent.Call('nyx_pascal', 'Pending facts', NyxObject([
      NyxField('mode', NyxData('declaration')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('routine', NyxData(LRoutine.Routine.Name)), NyxField('count', NyxData(8))]));
    Check((LValue.Field('pendingDraft').AsBoolean = APending) and
      (LValue.Field('text').AsText = Copy(LDeclaration.Text(dspInterface), 1, 8)),
      'declaration pending fact accompanies the real accepted counterpart');
    Check(LAgent.Call('nyx_pascal', 'Pending facts', NyxObject([
      NyxField('mode', NyxData('views')), NyxField('count', NyxData(8))]))
      .Field('pendingDraft').AsBoolean = APending, 'managed views retain their authoritative main flag');
    LValue := LAgent.Call('nyx_pascal', 'Pending facts', NyxObject([
      NyxField('mode', NyxData('files')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('limit', NyxData(1))]));
    Check((LValue.Field('pendingDrafts').AsBoolean = APending) and
      (LValue.Field('files').Item(0).Field('pendingDraft').AsBoolean = APending),
      'main-only fixture agrees with existing file and aggregate pending facts');
    Check(LAgent.Call('nyx_pascal', 'Pending facts', NyxObject([
      NyxField('mode', NyxData('unit')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('snapshot', NyxData('accepted')), NyxField('count', NyxData(8))]))
      .Field('pendingDraft').AsBoolean = APending, 'accepted unit inspection retains its main flag');
  end;

  procedure Unchanged;
  var
    LAfter: TNyxDataValue;
  begin
    LAfter := State;
    Check((EncodeNyxProject(LAgent.EditorSourcePair(LRevision)) = LBefore) and
      (LAgent.Revision = LRevision) and (LAgent.RecoveryStamp = LStamp) and
      (LAfter.Field('selection').AsText = LState.Field('selection').AsText) and
      (LAfter.Field('view').AsText = LState.Field('view').AsText) and
      (LAfter.Field('canUndo').AsBoolean = LState.Field('canUndo').AsBoolean) and
      (LAfter.Field('canRedo').AsBoolean = LState.Field('canRedo').AsBoolean),
      'pending facts preserve complete pair, revision, navigation and history; activity may log');
  end;
begin
  LSeed := ASeed;
  LSeed.Source := AddNyxRoutineDeclaration(LSeed.Source,
    NyxRoutineDeclaration(nrFunction, NyxRoutine('PendingMainFact'), rvInterface,
      'function PendingMainFact: Integer;', #10 + 'begin' + #10 +
      '  Result := 7;' + #10 + 'end;'));
  Check(LSeed.Files.IsEmpty and not LSeed.Pending,
    'owned callback seed exercises main-only legacy inspection branches');
  LCatalog := ReadNyxRoutines(LSeed.Source);
  LRoutine := Default(TNyxRoutineSource);
  LFound := False;
  for LIndex := 0 to LCatalog.Count - 1 do
  begin

    if SameText(LCatalog.Item(LIndex).Routine.Name, 'PendingMainFact') then
    begin
      LRoutine := LCatalog.Item(LIndex);
      LFound := True;
    end;
  end;
  Check(LFound, 'pending fact queries use a routine reference returned by the real source index');
  LDeclaration := ReadNyxRoutineDeclaration(LSeed.Source, LRoutine.Routine);
  LHandler := ReadNyxHandlerSource(LSeed.Source, AHandler);
  LAgent := TNyxAgentSession.Create(LSeed);
  try
    LRevision := LAgent.Revision;
    LAgent.Call('nyx_transaction', 'Pending facts', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('facts-history')),
      NyxField('operations', NyxArray([NyxObject([NyxField('op', NyxData('title')),
        NyxField('value', NyxData('Pending fact history'))])]))]));
    LAgent.Call('nyx_history', 'Pending facts', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('facts-history-undo')), NyxField('direction', NyxData('undo'))]));
    LRevision := LAgent.Revision;
    Check(EncodeNyxProject(LAgent.EditorSourcePair(LRevision)) = EncodeNyxProject(LSeed),
      'ordinary history restores the exact accepted seed before pending setup');
    LFrame := LSeed;
    LFrame.Pending := True;
    LFrame.Draft := LFrame.Source;
    LFrame.DraftBase := LFrame.Source;
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LFrame))),
      NyxField('selection', NyxData('project-name')), NyxField('view', NyxData('home'))]));
    LRevision := LAgent.Revision;
    LBefore := EncodeNyxProject(LAgent.EditorSourcePair(LRevision));
    LState := State;
    LStamp := LAgent.RecoveryStamp;
    Check((LBefore = EncodeNyxProject(LFrame)) and LState.Field('canRedo').AsBoolean,
      'equal-byte PendingTrue frame retains real Redo through ordinary synchronization');
    Facts(True);
    Unchanged;
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LSeed))),
      NyxField('selection', NyxData('project-name')), NyxField('view', NyxData('home'))]));
    LRevision := LAgent.Revision;
    LBefore := EncodeNyxProject(LAgent.EditorSourcePair(LRevision));
    LState := State;
    LStamp := LAgent.RecoveryStamp;
    Check(LBefore = EncodeNyxProject(LSeed), 'deliberate resolution changes no accepted source or design');
    Facts(False);
    Unchanged;
  finally
    LAgent.Free;
  end;
end;
{ END pending-main-draft facts helper }

var
  LFirst: TNyxHandlerSource;
  LSecond: TNyxHandlerSource;
  LOriginal: TNyxProjectPair;
  LAccepted: TNyxProjectPair;
  LDraft: TNyxProjectPair;
  LBody: TNyxText;
  LSecondBody: TNyxText;
  LSource: TNyxText;
  LText: TNyxText;
  LValue: TNyxDataValue;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LLine: Integer;
  LOffset: Integer;
  LHeader: TNyxText;
  LGuard: TNyxText;
begin
  GAgent := nil;
  GStudio := nil;
  try
    GStudio := TNyxStudioSession.Create;
    GStudio.Select('project-name');
    GFirst := GStudio.AddCallback(ntKeyDown, LLine);
    GSecond := GStudio.AddCallback(ntBeforeEdit, LLine);
    LOriginal := GStudio.ProjectSnapshot;
    LFirst := ReadNyxHandlerSource(LOriginal.Source, GFirst);
    LSecond := ReadNyxHandlerSource(LOriginal.Source, GSecond);
    Check(Pos('TODO', LFirst.Code) > 0, 'Read the exact ordinary generated callback body');
    Check(Pos('const AEvent: TNyxEventInfo', LFirst.Signature) > 0, 'Read immutable strongly typed signature');
    Check(LFirst.Line = NyxHandlerSourceLine(LOriginal.Source, GFirst), 'Navigation uses the real qualified method');
    LHeader := LFirst.Signature;
    LGuard := TNyxText('// Preserved sibling note 🌙 / procedure ') + GFirst.Name +
      '.Invoke; begin end;';
    LSource := ReplaceFirst(LOriginal.Source, 'implementation',
      'implementation' + #10 + LGuard);
    RefuseRead(LSource + #10 + LFirst.Signature + LFirst.Code,
      'Duplicate implementations refuse instead of choosing the first');
    RefuseRead(ReplaceFirst(LSource, LFirst.Code,
      #10 + 'begin' + #10 + '{$IFDEF FPC}' + #10 + 'end;'),
      'Conditional callback ownership refuses');
    RefuseRead(ReplaceFirst(LSource, LFirst.Signature + LFirst.Code,
      '{$IFDEF FPC}' + #10 + LFirst.Signature + LFirst.Code + #10 + '{$ENDIF}'),
      'A conditional outside the method cannot conceal target ownership');
    RefuseRead(ReplaceFirst(LSource, LFirst.Code,
      #10 + 'begin' + #10 + '  type TInline = record Value: Integer; end;' + #10 + 'end;'),
      'Inline type end cannot be mistaken for the method boundary');
    RefuseRead(ReplaceFirst(LSource, NyxViewsBegin,
      NyxViewsBegin + #10 + LFirst.Signature + LFirst.Code),
      'Managed-view method injection refuses');
    LBody := #10 + 'type' + #10 +
      '  TLocalNote = record' + #10 +
      '    Value: Integer;' + #10 + '  end;' + #10 +
      'const' + #10 + '  CDecoy = ''begin end; try case repeat until 🌙'';' + #10 +
      '  CParenthesis = ''('';' + #10 +
      'var' + #10 + '  LNote: TLocalNote;' + #10 +
      'function Score(AValue: Integer): Integer;' + #10 +
      'begin' + #10 + '  Result := AValue + 1;' + #10 + 'end;' + #10 +
      'begin' + #10 + #10 + '  if AExecution.Cancelled then' + #10 +
      '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 +
      '  LNote.Value := 0;' + #10 +
      '  try' + #10 + '    repeat' + #10 +
      '      LNote.Value := Score(LNote.Value);' + #10 +
      '    until LNote.Value > 1;' + #10 +
      '    case LNote.Value of' + #10 +
      '      2: LNote.Value := 3;' + #10 + '    end;' + #10 +
      '  finally' + #10 + '    LNote.Value := 0;' + #10 + '  end;' + #10 +
      '  // Crafted note 🌙 and lookalike end; tokens remain plain comments.' + #10 + 'end;';
    LSecondBody := #13#10 + 'begin' + #13#10 + '  // A separate authored method.' + #13#10 + 'end;';
    LSource := ReplaceNyxHandlerImplementation(LSource, GFirst, LFirst.Code, LBody);
    Check(ReadNyxHandlerSource(LSource, GFirst).Code = LBody,
      'Nested local type/function, repeat/case/try and lookalike literals retain exact boundaries');
    Check(ReadNyxHandlerSource(LSource, GFirst).Signature = LHeader, 'Replacement retains exact signature bytes');
    Check(Pos(LGuard, LSource) > 0, 'Replacement retains sibling Unicode notes');
    Check(ReadNyxHandlerSource(LSource, GSecond).Code = LSecond.Code,
      'Replacement never edits a neighboring implementation');
    LOriginal.Source := ReplaceFirst(LOriginal.Source, 'implementation',
      'implementation' + #10 + LGuard);
    GAgent := TNyxAgentSession.Create;
    GAgent.Exchange(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LOriginal))),
      NyxField('selection', NyxData('project-name')), NyxField('view', NyxData('home'))]));
    GRevision := GAgent.Revision;
    LValue := GAgent.Call('nyx_pascal', 'test', NyxObject([
      NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(GFirst.Name))]));
    Check(LValue.Field('text').AsText = LFirst.Code, 'Semantic read returns exact accepted implementation only');
    Check(not LValue.Field('pendingDraft').AsBoolean and
      (LValue.Field('revision').AsInteger = GRevision), 'Semantic read binds the accepted revision');
    LRequest := Args('authored-methods', NyxArray([
      Change(GFirst, LFirst.Code, LBody), Change(GSecond, LSecond.Code, LSecondBody)]));
    LReceipt := GAgent.Call('nyx_pascal', 'test', LRequest);
    GRevision := LReceipt.Field('revision').AsInteger;
    LAccepted := Pair;
    Check((LAccepted.Design = LOriginal.Design) and (LReceipt.Field('handlers').Count = 2),
      'Grouped source edits retain design and expose small final navigation results');
    Check(ReadNyxHandlerSource(LAccepted.Source, GSecond).Code = LSecondBody,
      'Mixed authored CRLF and LF are retained exactly');
    Check(GAgent.Call('nyx_pascal', 'test', LRequest).ToJSON = LReceipt.ToJSON,
      'Exact actor/argument retry returns its original receipt');
    LOffset := 0;
    LText := '';
    repeat
      LValue := GAgent.Call('nyx_pascal', 'test', NyxObject([
        NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(GFirst.Name)),
        NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(7))]));
      LText := LText + LValue.Field('text').AsText;
      LOffset := LValue.Field('nextOffset').AsInteger;
    until LOffset = LValue.Field('total').AsInteger;
    Check(LText = LBody, 'Small Unicode scalar windows reconstruct exact implementation, including supplementary text');
    GAgent.Call('nyx_history', 'test', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('undo-body')),
      NyxField('direction', NyxData('undo'))]));
    GRevision := GAgent.Revision;
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(LOriginal), 'One ordinary Undo restores both complete bodies');
    LValue := GAgent.Call('nyx_pascal', 'test', Args('noop-body', NyxArray([
      Change(GFirst, LFirst.Code, LFirst.Code)])));
    Check((LValue.Field('revision').AsInteger = GRevision) and LValue.Field('canRedo').AsBoolean,
      'Exact no-op retains revision and Redo history');
    GAgent.Call('nyx_history', 'test', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('redo-body')),
      NyxField('direction', NyxData('redo'))]));
    GRevision := GAgent.Revision;
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(LAccepted), 'One Redo restores exact authored methods');
    Refuse(Args('partial-refused', NyxArray([
      Change(GFirst, LBody, LFirst.Code), Change(GSecond, 'wrong old text', LSecondBody)])),
      'A failed second change rolls back the complete group');
    Refuse(Args('wrong-revision', NyxArray([Change(GFirst, LBody, LBody)]), GRevision - 1),
      'Stale revision refuses');
    Refuse(Args('duplicate-owner', NyxArray([Change(GFirst, LBody, LBody), Change(GFirst, LBody, LBody)])),
      'Duplicate handler changes refuse before staging');
    Refuse(Args('sibling-injection', NyxArray([Change(GFirst, LBody,
      LBody + #10 + 'procedure Intruder; begin end;')])), 'Sibling method injection refuses');
    Refuse(Args('trailing-comment', NyxArray([Change(GFirst, LBody,
      LBody + '// swallow the next same-line helper')])), 'Trailing comments cannot change sibling ownership');
    Refuse(Args('unbalanced', NyxArray([Change(GFirst, LBody, #10 + 'begin try end;')])),
      'Unbalanced executable blocks refuse');
    Refuse(Args('missing-field', TNyxDataValue.ParseJSON('[{"handler":"TBad","expected":true,"implementation":""}]')),
      'Wrong field types refuse');
    Refuse(Args('unknown-field', TNyxDataValue.ParseJSON('[{"handler":"TBad","expected":"","implementation":"","command":"bad"}]')),
      'Unknown transport fields refuse');
    Refuse(Args('joined-field', NyxArray([NyxObject([
      NyxField('handler', NyxData(GFirst.Name)), NyxField('expected', NyxData(LBody)),
      NyxField('implementation', NyxData(LBody)), NyxField('handler|expected', NyxData('unexpected'))])])),
      'Joined transport names cannot bypass the closed change shape');
    Refuse(NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('joined-argument')),
      NyxField('changes', NyxArray([Change(GFirst, LBody, LBody)])),
      NyxField('mode|expectedRevision', NyxData('unexpected'))]),
      'Joined argument names cannot bypass the closed tool shape');
    Refuse(Args('empty-group', NyxArray([])), 'Empty groups refuse');
    Refuse(Args('too-long', NyxArray([Change(GFirst, LBody, StringOfChar('x', 32769))])),
      'Callback text budgets refuse');
    GAgent.Exchange(NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('readOnly'))]));
    Check(GAgent.Call('nyx_pascal', 'test', NyxObject([
      NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(GFirst.Name))])).Field('text').AsText = LBody,
      'Read-only permits bounded accepted callback inspection');
    Refuse(Args('read-only', NyxArray([Change(GFirst, LBody, LBody)])), 'Read-only refuses source publication');
    GAgent.Exchange(NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('edit'))]));
    LDraft := Pair;
    LDraft.Draft := LDraft.Source + #10 + TNyxText('// Protected unaccepted draft 🌙');
    LDraft.DraftBase := LDraft.Source;
    LDraft.Pending := True;
    GAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('project', NyxData(EncodeNyxProject(LDraft))),
      NyxField('selection', NyxData('project-name')), NyxField('view', NyxData('home'))]));
    GRevision := GAgent.Revision;
    Check(GAgent.Call('nyx_pascal', 'test', NyxObject([
      NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(GFirst.Name))])).Field('pendingDraft').AsBoolean,
      'Inspection reports the protected draft while reading accepted code');
    Refuse(Args('pending-draft', NyxArray([Change(GFirst, LBody, LBody)])), 'Pending drafts refuse publication');
    Check(Pair.Draft = LDraft.Draft, 'Rejected edit retains every draft byte');
    { BEGIN pending-main-draft facts call }
    EqualMainDraftFacts(LOriginal, GFirst);
    { END pending-main-draft facts call }
    FreeAndNil(GAgent);
    FreeAndNil(GStudio);
    WriteLn('PASS ', GCount, ' portable handler source/transaction checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-handler-edits', 'passed');
    document.body.setAttribute('data-nyx-handler-checks', IntToStr(GCount));
    {$endif}
  except
    on LException: Exception do
    begin
      GAgent.Free;
      GStudio.Free;
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-handler-edits', 'failed');
      document.body.setAttribute('data-nyx-handler-error', LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
