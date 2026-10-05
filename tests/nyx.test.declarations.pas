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

unit nyx.test.declarations;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.studio.projects;

{ Independent English project and exact returned companion. Semantic operations
  exercise the real agent/session boundary without a listener or user project. }
function RunNyxDeclarationJourney(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.types, nyx.codegen,
  nyx.codec, nyx.source, nyx.callbacks, nyx.studio.session, nyx.studio.agents,
  nyx.studio.declarationedits;

function RunNyxDeclarationJourney(out APair: TNyxProjectPair): Integer;
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
  LPolicy: TNyxRoutineSource;
  LReplacementCaption: TNyxRoutineDeclaration;
  LSignaturePair: TNyxProjectPair;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LReply: TNyxDataValue;
  LPatch: INyxDeclarationPatch;
  LEdits: array of TNyxDeclarationEdit;
  LDeclaration: TNyxRoutineDeclaration;
  LSite: TNyxRoutineDeclarationSource;
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
      raise ENyxModel.Create('Semantic declarations: ' + AReason);
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
      NyxField('op', NyxData('edit')), NyxField('routine', NyxData(AName)),
      NyxField('expectedImplementation', NyxData(AExpected)),
      NyxField('implementation', NyxData(ACode))]);
  end;

  function Args(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([
      NyxField('mode', NyxData('edit-declarations')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges)]);
  end;

  function SignatureChange(const APrevious: TNyxRoutineSource;
    const AReplacement: TNyxRoutineDeclaration; const APrototype: TNyxText): TNyxDataValue;
  var
    LVisibility: TNyxText;
  begin
    LVisibility := 'implementation';

    if AReplacement.Visibility = rvInterface then
    begin
      LVisibility := 'interface';
    end;
    Result := NyxObject([
      NyxField('op', NyxData('signature')), NyxField('routine', NyxData(AReplacement.Routine.Name)),
      NyxField('kind', NyxData('function')), NyxField('visibility', NyxData(LVisibility)),
      NyxField('signature', NyxData(AReplacement.Signature)),
      NyxField('implementation', NyxData(AReplacement.Code)),
      NyxField('expectedSignature', NyxData(APrevious.Signature)),
      NyxField('expectedImplementation', NyxData(APrevious.Code)),
      NyxField('expectedDeclaration', NyxData(APrototype))]);
  end;

  function Query(const AName: TNyxText; AOffset, ACount: Integer): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
      NyxField('mode', NyxData('routine')), NyxField('routine', NyxData(AName)),
      NyxField('offset', NyxData(AOffset)), NyxField('count', NyxData(ACount))]));
  end;

  procedure Refuse(const ARequest: TNyxDataValue; const AReason: TNyxText;
    const AOwner: TNyxText = 'declaration-client');
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

    LDeclaration := NyxRoutineDeclaration(nrFunction, NyxRoutine('TextBudget'), rvImplementation,
      'function TextBudget: Integer;', #10 + 'begin' + #10 +
      TNyxText('  { Exact qualification 🌙; captions stay English. }') + #10 +
      '  Result := 6;' + #10 + 'end;');
    LBody := #10 + 'begin' + #10 + '  Result := TextBudget;' + #10 + 'end;';
    SetLength(LEdits, 4);
    LEdits[0] := NyxCreateDeclaration(LDeclaration);
    LEdits[1] := NyxCreateDeclaration(NyxRoutineDeclaration(nrFunction, NyxRoutine('EnglishCaption'),
      rvInterface, 'function EnglishCaption: TNyxText;',
      #10 + 'begin' + #10 +
      '  Result := ''Up to '' + IntToStr(TextBudget) + '' characters'';' + #10 + 'end;'));
    LEdits[2] := NyxEditDeclaration(LBudget.Routine, LBudget.Code, LBody);
    LSite := ReadNyxRoutineDeclaration(LInitial.Source, NyxRoutine('HelperCaption'));
    LEdits[3] := NyxRemoveDeclaration(LCaption.Routine, LCaption.Signature,
      LCaption.Code, LSite.Declaration);
    LPatch := NyxDeclarationPatch(LEdits);
    LEdits[0] := NyxEditDeclaration(LCaption.Routine, '', '');
    LPair := LPatch.Candidate(LInitial);
    Check((LPair.Design = LInitial.Design) and
      (ReadNyxRoutineSource(LPair.Source, NyxRoutine('TextBudget')).Code = LDeclaration.Code),
      'immutable copied group admits created private/public helpers, caller edit and paired removal');
    Check((EncodeNyxProject(Current) = EncodeNyxProject(LInitial)) and
      (EncodeNyxProject(LStudio.ProjectSnapshot) = EncodeNyxProject(LInitial)),
      'candidate borrows no active model/history');
    LRequest := Args('helper-declarations', NyxArray([
      NyxObject([NyxField('op', NyxData('create')), NyxField('routine', NyxData('TextBudget')),
        NyxField('kind', NyxData('function')), NyxField('visibility', NyxData('implementation')),
        NyxField('signature', NyxData(LDeclaration.Signature)),
        NyxField('implementation', NyxData(LDeclaration.Code))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('routine', NyxData('EnglishCaption')),
        NyxField('kind', NyxData('function')), NyxField('visibility', NyxData('interface')),
        NyxField('signature', NyxData('function EnglishCaption: TNyxText;')),
        NyxField('implementation', NyxData(
          ReadNyxRoutineSource(LPair.Source, NyxRoutine('EnglishCaption')).Code))]),
      Change('TNotePolicy.Limit', LBudget.Code, LBody),
      NyxObject([NyxField('op', NyxData('remove')), NyxField('routine', NyxData('HelperCaption')),
        NyxField('expectedSignature', NyxData(LCaption.Signature)),
        NyxField('expectedImplementation', NyxData(LCaption.Code)),
        NyxField('expectedDeclaration', NyxData(LSite.Declaration))])]));
    LReceipt := LAgent.Call('nyx_pascal', 'Scooty', LRequest, 'declaration-client');
    LRevision := LAgent.Revision;
    LAccepted := Current;
    Check(LReceipt.Field('declarations').Field('changes').AsInteger = 4,
      'one four-operation group publishes one paired source candidate');
    Check((LAccepted.Design = LInitial.Design) and
      (ReadNyxRoutineSource(LAccepted.Source, NyxRoutine('TNotePolicy.Limit')).Signature = LBudget.Signature) and
      (ReadNyxHandlerSource(LAccepted.Source, LHandler).Code =
      ReadNyxHandlerSource(LInitial.Source, LHandler).Code) and
      (Pos('// Kept outside the helper:', LAccepted.Source) > 0),
      'class signatures, callback implementation, comments and exact design are retained');
    Check(Pos('HelperCaption', LAccepted.Source) = 0,
      'obsolete public helper retires both exact counterparts');
    LReply := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
      NyxField('mode', NyxData('declaration')), NyxField('routine', NyxData('TextBudget'))]));
    Check((LReply.Field('visibility').AsText = 'implementation') and
      (LReply.Field('text').AsText = '') and (LReply.Field('line').AsInteger = 0),
      'private helper query reports an explicit empty counterpart');
    LReply := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
      NyxField('mode', NyxData('declaration')), NyxField('routine', NyxData('TextBudget')),
      NyxField('part', NyxData('implementation')), NyxField('count', NyxData(5))]));
    Check((LReply.Field('text').AsText = 'funct') and
      (LReply.Field('total').AsInteger = Length('function TextBudget: Integer;')),
      'private implementation signatures are independently queryable in bounded windows');
    LCollected := '';
    LOffset := 0;
    repeat
      LReply := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
        NyxField('mode', NyxData('declaration')), NyxField('routine', NyxData('englishcaption')),
        NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(5))]));
      LCollected := LCollected + LReply.Field('text').AsText;
      LOffset := LReply.Field('nextOffset').AsInteger;
    until LOffset = LReply.Field('total').AsInteger;
    Check(LCollected = 'function EnglishCaption: TNyxText;', 'bounded counterpart windows concatenate exact text');
    Check(LAgent.Call('nyx_pascal', 'Scooty', LRequest, 'declaration-client').ToJSON = LReceipt.ToJSON,
      'exact retry returns the original declaration receipt');
    Refuse(LRequest, 'foreign authority cannot claim stale receipt', 'foreign-client');
    History('undo', 'undo-declarations');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LInitial), 'one Undo restores all four source operations');
    History('redo', 'redo-declarations');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LAccepted), 'one Redo restores exact declared source/design');
    LBudget := ReadNyxRoutineSource(LAccepted.Source, NyxRoutine('TextBudget'));
    Refuse(Args('referenced', NyxArray([NyxObject([
      NyxField('op', NyxData('remove')), NyxField('routine', NyxData('TextBudget')),
      NyxField('expectedSignature', NyxData(LBudget.Signature)),
      NyxField('expectedImplementation', NyxData(LBudget.Code)),
      NyxField('expectedDeclaration', NyxData(''))])])),
      'retained qualified and global callers prevent private helper removal');
    Refuse(Args('late', NyxArray([
      Change('TNotePolicy.Limit', LBody, #10 + 'begin Result := 9; end;'),
      Change('EnglishCaption', 'stale', #10 + 'begin Result := ''''; end;')])),
      'late stale expected text refuses entire group');
    Refuse(Args('inject', NyxArray([Change('TextBudget', LBudget.Code,
      #10 + 'begin Result := 8; end;' + #10 + 'procedure Extra; begin end;')])),
      'grouped implementation edit cannot inject a sibling');
    Refuse(Args('noop', NyxArray([Change('TextBudget', LBudget.Code, LBudget.Code)])),
      'no-op group spends no history');
    Refuse(Args('unknown', NyxArray([NyxObject([
      NyxField('op', NyxData('remove')), NyxField('routine', NyxData('EnglishCaption')),
      NyxField('expectedSignature', NyxData('function EnglishCaption: TNyxText;')),
      NyxField('expectedImplementation', NyxData('')), NyxField('expectedDeclaration', NyxData('')),
      NyxField('workspace', NyxData('foreign'))])])), 'per-change context injection refuses');
    LAgent.InheritPermission(apReadOnly);
    Refuse(Args('readonly', NyxArray([Change('TextBudget', LBudget.Code,
      #10 + 'begin Result := 99; end;')])),
      'operator read-only policy protects helpers');
    Check(Query('TextBudget', 0, 3).Field('text').AsText = #10 + 'be',
      'bounded source remains inspectable under read-only policy');
    LAgent.InheritPermission(apEdit);
    LPair := Current;
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + #10 + TNyxText('// independent draft 🌙');
    Publish(LPair);
    Refuse(Args('draft', NyxArray([Change('TextBudget', LBudget.Code,
      #10 + 'begin Result := 99; end;')])),
      'pending draft blocks helper mutation without losing exact Unicode');
    Check(Query('TNotePolicy.Limit', 0, 3).Field('pendingDraft').AsBoolean,
      'accepted helper context reports independent pending draft');
    LReply := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
      NyxField('mode', NyxData('declaration')), NyxField('routine', NyxData('EnglishCaption'))]));
    Check(LReply.Field('pendingDraft').AsBoolean and
      (LReply.Field('text').AsText = 'function EnglishCaption: TNyxText;'),
      'counterpart context reads accepted source and visibly retains pending draft');
    LPair.Pending := False;
    LPair.Draft := '';
    LPair.DraftBase := '';
    Publish(LPair);

    { Change both private parameter/result typing and a public parameter contract.
      The retained class caller is supplied explicitly in the same semantic group;
      no intermediate incompatible source is published or compiled. }
    LAccepted := Current;
    LBudget := ReadNyxRoutineSource(LAccepted.Source, NyxRoutine('TextBudget'));
    LCaption := ReadNyxRoutineSource(LAccepted.Source, NyxRoutine('EnglishCaption'));
    LPolicy := ReadNyxRoutineSource(LAccepted.Source, NyxRoutine('TNotePolicy.Limit'));
    LSite := ReadNyxRoutineDeclaration(LAccepted.Source, LCaption.Routine);
    LDeclaration := NyxRoutineDeclaration(nrFunction, LBudget.Routine, rvImplementation,
      'function TextBudget(const AMaximum: Integer): TNyxText;',
      #10 + 'begin' + #10 + TNyxText('  { Exact qualification 🌙; a typed result survives. }') + #10 +
      '  Result := IntToStr(AMaximum);' + #10 + 'end;');
    LReplacementCaption := NyxRoutineDeclaration(nrFunction, LCaption.Routine, rvInterface,
      'function EnglishCaption(const APurpose: TNyxText): TNyxText;',
      #10 + 'begin' + #10 +
      '  Result := APurpose + TNyxText('': up to '') + TextBudget(5) + TNyxText('' characters'');' +
      #10 + 'end;');
    LBody := #10 + 'begin' + #10 + '  Result := StrToInt(TextBudget(5));' + #10 + 'end;';
    LPatch := NyxDeclarationPatch([
      NyxChangeDeclarationSignature(LDeclaration, LBudget.Signature, LBudget.Code, ''),
      NyxChangeDeclarationSignature(LReplacementCaption, LCaption.Signature, LCaption.Code,
        LSite.Declaration),
      NyxEditDeclaration(LPolicy.Routine, LPolicy.Code, LBody)]);
    LSignaturePair := LPatch.Candidate(LAccepted);
    Check((LSignaturePair.Design = LInitial.Design) and not LSignaturePair.Pending and
      (EncodeNyxProject(Current) = EncodeNyxProject(LAccepted)),
      'typed signature candidate owns an independent exact pair without publication');
    Check((ReadNyxRoutineSource(LSignaturePair.Source, LBudget.Routine).Signature =
      LDeclaration.Signature) and
      (ReadNyxRoutineDeclaration(LSignaturePair.Source, LBudget.Routine).Declaration = ''),
      'private parameter/result replacement retains implementation-only visibility');
    Check((ReadNyxRoutineSource(LSignaturePair.Source, LCaption.Routine).Signature =
      LReplacementCaption.Signature) and
      (ReadNyxRoutineDeclaration(LSignaturePair.Source, LCaption.Routine).Declaration =
      LReplacementCaption.Signature), 'public signature replacements own both exact counterparts');
    LRequest := Args('helper-signatures', NyxArray([
      SignatureChange(LBudget, LDeclaration, ''),
      SignatureChange(LCaption, LReplacementCaption, LSite.Declaration),
      Change('TNotePolicy.Limit', LPolicy.Code, LBody)]));
    LReceipt := LAgent.Call('nyx_pascal', 'Scooty', LRequest, 'signature-client');
    LRevision := LAgent.Revision;
    Check((EncodeNyxProject(Current) = EncodeNyxProject(LSignaturePair)) and
      (LReceipt.Field('declarations').Field('changes').AsInteger = 3),
      'one semantic signature/caller group publishes the exact admitted pair');
    Check((ReadNyxRoutineSource(Current.Source, LPolicy.Routine).Signature = LPolicy.Signature) and
      (ReadNyxHandlerSource(Current.Source, LHandler).Code =
      ReadNyxHandlerSource(LInitial.Source, LHandler).Code) and
      (Pos('// Kept outside the helper:', Current.Source) > 0),
      'class signatures, callback implementation and surrounding comments remain owned');
    Check(LAgent.Call('nyx_pascal', 'Scooty', LRequest, 'signature-client').ToJSON = LReceipt.ToJSON,
      'signature retry returns its exact actor-bound receipt');
    Refuse(LRequest, 'foreign actor cannot claim a signature receipt', 'foreign-signature-client');
    History('undo', 'undo-signatures');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LAccepted),
      'one Undo restores private/public signatures, bodies and related caller together');
    History('redo', 'redo-signatures');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LSignaturePair),
      'one Redo restores exact signature and caller composition');
    LReply := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
      NyxField('mode', NyxData('declaration')), NyxField('routine', NyxData('EnglishCaption'))]));
    Check(LReply.Field('text').AsText = LReplacementCaption.Signature,
      'bounded public inspection immediately observes the changed accepted signature');
    Refuse(Args('stale-signature', NyxArray([
      SignatureChange(LCaption, LReplacementCaption, LSite.Declaration)])),
      'old exact counterparts refuse at a fresh outer revision');
    LBudget := ReadNyxRoutineSource(Current.Source, LBudget.Routine);
    LCaption := ReadNyxRoutineSource(Current.Source, LCaption.Routine);
    LSite := ReadNyxRoutineDeclaration(Current.Source, LCaption.Routine);
    LDeclaration := NyxRoutineDeclaration(nrFunction, LBudget.Routine, rvImplementation,
      'function TextBudget(const AMaximum, AFloor: Integer): TNyxText;',
      #10 + 'begin' + #10 + '  Result := IntToStr(AMaximum + AFloor);' + #10 + 'end;');
    Refuse(Args('late-signature', NyxArray([
      SignatureChange(LBudget, LDeclaration, ''),
      Change('TNotePolicy.Limit', 'stale caller body', LBody)])),
      'a late caller refusal publishes none of the preceding signature replacement');
    Refuse(Args('signature-visibility', NyxArray([
      SignatureChange(LBudget, NyxRoutineDeclaration(nrFunction, LBudget.Routine, rvInterface,
        LDeclaration.Signature, LDeclaration.Code), '')])),
      'signature operations do not silently change public visibility');
    Refuse(Args('signature-noop', NyxArray([
      SignatureChange(LCaption, LReplacementCaption, LSite.Declaration)])),
      'no-op signature replacement spends no history');
    LAgent.InheritPermission(apReadOnly);
    Refuse(Args('signature-readonly', NyxArray([
      SignatureChange(LBudget, LDeclaration, '')])),
      'read-only permission refuses an otherwise valid changed signature');
    LAgent.InheritPermission(apEdit);
    LPair := Current;
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + #10 + TNyxText('// signature draft 🌙');
    Publish(LPair);
    Refuse(Args('signature-draft', NyxArray([
      SignatureChange(LBudget, LDeclaration, '')])),
      'pending draft retains exact text and refuses an otherwise valid signature');
    LPair.Pending := False;
    LPair.DraftBase := '';
    LPair.Draft := '';
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
