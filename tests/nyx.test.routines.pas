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

unit nyx.test.routines;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.studio.projects;

{ Independent English project and exact returned companion. Semantic operations
  exercise the real agent/session boundary without a listener or user project. }
function RunNyxRoutineJourney(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.types, nyx.codegen,
  nyx.codec, nyx.source, nyx.callbacks, nyx.studio.session, nyx.studio.agents,
  nyx.studio.routineedits;

function RunNyxRoutineJourney(out APair: TNyxProjectPair): Integer;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LStudio: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LInitial: TNyxProjectPair;
  LAccepted: TNyxProjectPair;
  LHandler: TNyxHandlerRef;
  LCallback: TNyxHandlerSource;
  LSource: TNyxText;
  LParts: TNyxStrings;
  LBody: TNyxText;
  LBudget: TNyxRoutineSource;
  LCaption: TNyxRoutineSource;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LReply: TNyxDataValue;
  LPatch: INyxRoutinePatch;
  LEdits: array of TNyxRoutineEdit;
  LResults: TNyxRoutineEditResults;
  LPosition: Integer;
  LLine: Integer;
  LRevision: Integer;
  LChecks: Integer;
  LOffset: Integer;
  LCollected: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Semantic routines: ' + AReason);
    end;
    Inc(LChecks);
  end;

  function Current: TNyxProjectPair;
  begin
    Result := DecodeNyxProject(LAgent.Exchange(NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))])).Field('project').AsText);
  end;

  function Change(const AName, AExpected, ACode: TNyxText): TNyxDataValue;
  begin
    Result := NyxObject([
      NyxField('routine', NyxData(AName)), NyxField('expected', NyxData(AExpected)),
      NyxField('implementation', NyxData(ACode))]);
  end;

  function Args(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([
      NyxField('mode', NyxData('edit-routines')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges)]);
  end;

  function Query(const AName: TNyxText; AOffset, ACount: Integer): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
      NyxField('mode', NyxData('routine')), NyxField('routine', NyxData(AName)),
      NyxField('offset', NyxData(AOffset)), NyxField('count', NyxData(ACount))]));
  end;

  procedure Refuse(const ARequest: TNyxDataValue; const AReason: TNyxText;
    const AOwner: TNyxText = 'routine-client');
  var
    LBefore: TNyxText;
    LBeforeState: TNyxDataValue;
    LAfterState: TNyxDataValue;
    LRejected: Boolean;
  begin
    LBefore := EncodeNyxProject(Current);
    LBeforeState := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    LRejected := False;
    try
      LAgent.Call('nyx_pascal', 'Scooty', ARequest, AOwner);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    LAfterState := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    Check(LRejected and (LAgent.Revision = LRevision) and
      (EncodeNyxProject(Current) = LBefore), AReason);
    Check((LBeforeState.Field('selection').AsText = LAfterState.Field('selection').AsText) and
      (LBeforeState.Field('canUndo').AsBoolean = LAfterState.Field('canUndo').AsBoolean) and
      (LBeforeState.Field('canRedo').AsBoolean = LAfterState.Field('canRedo').AsBoolean),
      'refusal preserves navigation and paired history');
  end;

  procedure History(const ADirection, AID: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('direction', NyxData(ADirection)), NyxField('operationId', NyxData(AID)),
      NyxField('expectedRevision', NyxData(LRevision))]));
    LRevision := LAgent.Revision;
  end;

  procedure Publish(const AValue: TNyxProjectPair);
  begin
    LAgent.Exchange(NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(AValue))),
      NyxField('selection', NyxData('short-note')), NyxField('view', NyxData('helper-workshop'))]));
    LRevision := LAgent.Revision;
  end;

begin
  LChecks := 0;
  LStudio := nil;
  LAgent := nil;
  LPatch := nil;
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Helper workshop';
    LPage := NewNyxPage('helper-workshop');
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxMemo('short-note').Configure.Text('A short note').Done);
    LStudio := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument)));
    LStudio.Select('short-note');
    LHandler := LStudio.AddCallback(ntBeforeTextInput, LLine);
    LPair := LStudio.ProjectSnapshot;
    LCallback := ReadNyxHandlerSource(LPair.Source, LHandler);
    LBody := #10 + 'begin' + #10 + #10 +
      '  if AExecution.Cancelled or not AEvent.HasTextEdit then' + #10 +
      '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 + #10 +
      '  if NyxTextScalarCount(AEvent.TextEdit.After) > TNotePolicy.Limit then' + #10 +
      '  begin' + #10 + '    NyxEventResponse(AExecution).Consume;' + #10 +
      '  end;' + #10 + 'end;';
    LSource := ReplaceNyxHandlerImplementation(LPair.Source, LHandler, LCallback.Code, LBody);
    LPosition := Pos('implementation', LSource);
    LParts := TNyxStrings.Create;
    try
      LParts.Add(Copy(LSource, 1, LPosition - 1));
      LParts.Add('type');
      LParts.Add('  { Handwritten policy remains independent of managed view declarations. }');
      LParts.Add('  TNotePolicy = class');
      LParts.Add('  public');
      LParts.Add('    class function Limit: Integer; static;');
      LParts.Add('    constructor Create;');
      LParts.Add('    destructor Destroy; override;');
      LParts.Add('  end;');
      LParts.Add('');
      LParts.Add('function HelperCaption: TNyxText;');
      LParts.Add('');
      LParts.Add(Copy(LSource, LPosition, MaxInt - LPosition));
      LSource := LParts.Text;
    finally
      LParts.Free;
    end;
    LPosition := Pos(NyxViewsBegin, LSource);
    LParts := TNyxStrings.Create;
    try
      LParts.Add(Copy(LSource, 1, LPosition - 1));
      LParts.Add('// Kept outside the helper: decoy function Wrong: Integer; begin end;');
      LParts.Add('class function TNotePolicy.Limit: Integer;');
      LParts.Add('begin');
      LParts.Add('  Result := 2;');
      LParts.Add('end;');
      LParts.Add('');
      LParts.Add('function HelperCaption: TNyxText;');
      LParts.Add('begin');
      LParts.Add('  Result := ''Up to two characters'';');
      LParts.Add('end;');
      LParts.Add('');
      LParts.Add('constructor TNotePolicy.Create;');
      LParts.Add('begin');
      LParts.Add('  inherited Create;');
      LParts.Add('end;');
      LParts.Add('');
      LParts.Add('destructor TNotePolicy.Destroy;');
      LParts.Add('begin');
      LParts.Add('  inherited Destroy;');
      LParts.Add('end;');
      LParts.Add('');
      LParts.Add(Copy(LSource, LPosition, MaxInt - LPosition));
      LSource := LParts.Text;
    finally
      LParts.Free;
    end;
    LSource := EditNyxImport(LSource, nisImplementation, niaAdd, NyxPascalUnit('SysUtils'));
    LStudio.SetSourceDraft(LSource);
    LStudio.ApplySourceDraft;
    Check(LStudio.Source = LSource, 'ordinary admission accepts handwritten class/global helpers');
    LInitial := LStudio.ProjectSnapshot;
    LAgent := TNyxAgentSession.Create(LInitial);
    LRevision := LAgent.Revision;
    LBudget := ReadNyxRoutineSource(LInitial.Source, NyxRoutine('TNotePolicy.Limit'));
    LCaption := ReadNyxRoutineSource(LInitial.Source, NyxRoutine('HelperCaption'));
    Check(LBudget.Editable and (LBudget.Kind = nrFunction) and
      (Pos('class function', LBudget.Signature) = 1), 'qualified class function retains full signature');
    LReply := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
      NyxField('mode', NyxData('routines')), NyxField('offset', NyxData(0)),
      NyxField('limit', NyxData(2))]));
    Check((LReply.Field('routines').Count = 2) and
      (LReply.Field('nextOffset').AsInteger = 2) and
      (LReply.Field('routines').Item(0).Field('routine').AsText = 'TNotePolicy.Limit'),
      'bounded discovery supplies authored names instead of whole source');
    LCollected := '';
    LOffset := 0;
    repeat
      LReply := Query('helpercaption', LOffset, 7);
      LCollected := LCollected + LReply.Field('text').AsText;
      LOffset := LReply.Field('nextOffset').AsInteger;
    until LOffset = LReply.Field('total').AsInteger;
    Check(LCollected = LCaption.Code, 'case-insensitive windows concatenate exact accepted text');
    Check((LReply.Field('revision').AsInteger = LRevision) and
      not LReply.Field('pendingDraft').AsBoolean, 'queries retain one accepted revision');
    LBody := #10 + 'begin' + #10 +
      TNyxText('  { Qualification text 🌙 remains exact; demo caption stays English. }') + #10 +
      '  Result := 4;' + #10 + 'end;';
    SetLength(LEdits, 2);
    LEdits[0] := NyxRoutineEdit(NyxRoutine('TNotePolicy.Limit'), LBudget.Code, LBody);
    LEdits[1] := NyxRoutineEdit(NyxRoutine('HelperCaption'), LCaption.Code,
      #10 + 'begin' + #10 + '  Result := ''Up to '' + IntToStr(TNotePolicy.Limit) + '' characters'';' + #10 + 'end;');
    LPatch := NewNyxRoutinePatch(LEdits);
    LEdits[0] := NyxRoutineEdit(NyxRoutine('HelperCaption'), '', '');
    LPair := LPatch.Candidate(LStudio, LResults);
    Check((Length(LResults) = 2) and (LPair.Design = LInitial.Design) and
      (ReadNyxRoutineSource(LPair.Source, NyxRoutine('TNotePolicy.Limit')).Code = LBody),
      'managed patch owns copied proposals and returns an independent candidate');
    Check((EncodeNyxProject(Current) = EncodeNyxProject(LInitial)) and
      (EncodeNyxProject(LStudio.ProjectSnapshot) = EncodeNyxProject(LInitial)),
      'candidate never publishes or spends active history');
    LRequest := Args('crafted-helpers', NyxArray([
      Change('TNotePolicy.Limit', LBudget.Code, LBody),
      Change('HelperCaption', LCaption.Code,
        ReadNyxRoutineSource(LPair.Source, NyxRoutine('HelperCaption')).Code)]));
    LReceipt := LAgent.Call('nyx_pascal', 'Scooty', LRequest, 'routine-client');
    LRevision := LAgent.Revision;
    LAccepted := Current;
    Check((LReceipt.Field('routines').Count = 2) and
      (LReceipt.Field('routines').Item(0).Field('line').AsInteger = LBudget.Line),
      'one grouped mutation returns final source navigation sites');
    Check((LAccepted.Design = LInitial.Design) and
      (ReadNyxRoutineSource(LAccepted.Source, NyxRoutine('TNotePolicy.Limit')).Signature = LBudget.Signature) and
      (ReadNyxHandlerSource(LAccepted.Source, LHandler).Code =
      ReadNyxHandlerSource(LInitial.Source, LHandler).Code) and
      (Pos('// Kept outside the helper:', LAccepted.Source) > 0),
      'source publication retains exact design, signatures, callback and surrounding comments');
    Check(LAgent.Call('nyx_pascal', 'Scooty', LRequest, 'routine-client').ToJSON = LReceipt.ToJSON,
      'exact retry returns the original receipt');
    Refuse(LRequest, 'foreign authority cannot claim stale receipt', 'foreign-client');
    History('undo', 'undo-helpers');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LInitial), 'one Undo restores both implementations');
    History('redo', 'redo-helpers');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LAccepted), 'one Redo restores exact source/design');
    LBudget := ReadNyxRoutineSource(LAccepted.Source, NyxRoutine('TNotePolicy.Limit'));
    Refuse(Args('late', NyxArray([
      Change('TNotePolicy.Limit', LBudget.Code, #10 + 'begin Result := 9; end;'),
      Change('HelperCaption', 'stale', #10 + 'begin Result := ''''; end;')])),
      'late stale expected text refuses complete group');
    Refuse(Args('duplicate', NyxArray([
      Change('TNotePolicy.Limit', LBudget.Code, LBudget.Code),
      Change('tnotepolicy.limit', LBudget.Code, LBudget.Code)])), 'duplicate qualified targets refuse');
    Refuse(Args('inject', NyxArray([Change('TNotePolicy.Limit', LBudget.Code,
      #10 + 'begin Result := 8; end;' + #10 + 'procedure Extra; begin end;')])),
      'an implementation cannot add sibling code');
    Refuse(Args('noop', NyxArray([Change('TNotePolicy.Limit', LBudget.Code, LBudget.Code)])),
      'no-op patch spends no history');
    Refuse(Args('managed', NyxArray([Change('BuildNyxDocument', '', '')])),
      'managed builder cannot be edited through the helper API');
    LAgent.InheritPermission(apReadOnly);
    Refuse(Args('readonly', NyxArray([Change('TNotePolicy.Limit', LBudget.Code, LBudget.Code)])),
      'operator read-only policy protects helpers');
    Check(Query('TNotePolicy.Limit', 0, 3).Field('text').AsText = #10 + 'be',
      'bounded source remains inspectable under read-only policy');
    LAgent.InheritPermission(apEdit);
    LPair := Current;
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + #10 + TNyxText('// independent draft 🌙');
    Publish(LPair);
    Refuse(Args('draft', NyxArray([Change('TNotePolicy.Limit', LBudget.Code, LBudget.Code)])),
      'pending draft blocks helper mutation without losing exact Unicode');
    Check(Query('TNotePolicy.Limit', 0, 3).Field('pendingDraft').AsBoolean,
      'accepted helper context reports independent pending draft');
    LPair.Pending := False;
    LPair.Draft := '';
    LPair.DraftBase := '';
    Publish(LPair);
    APair := Current;
    Check((APair.Design = LInitial.Design) and not APair.Pending,
      'final unchanged companion remains suitable for real compiler/control qualification');
    Result := LChecks;
  finally
    LPatch := nil;
    LAgent.Free;
    LStudio.Free;
    LPage := nil;
    LDocument.Free;
  end;
end;

end.
