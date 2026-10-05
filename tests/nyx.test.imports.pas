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


unit nyx.test.imports;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.studio.projects;

{ Independent English design; exact admitted companion is returned for compiler/
  actual-control qualification. No listener or operator project is accessed. }
function RunNyxImportJourney(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.types, nyx.codegen, nyx.codec,
  nyx.source, nyx.callbacks, nyx.studio.session, nyx.studio.agents,
  nyx.studio.importedits;

function RunNyxImportJourney(out APair: TNyxProjectPair): Integer;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LStudio: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LInitial: TNyxProjectPair;
  LAccepted: TNyxProjectPair;
  LHandler: TNyxHandlerRef;
  LHandlerCode: TNyxHandlerSource;
  LSource: TNyxText;
  LSourceParts: TNyxStrings;
  LBody: TNyxText;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LReply: TNyxDataValue;
  LPatch: INyxImportPatch;
  LChanges: array of TNyxImportEdit;
  LPosition: Integer;
  LLine: Integer;
  LRevision: Integer;
  LChecks: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Semantic imports: ' + AReason);
    end;
    Inc(LChecks);
  end;

  function Change(const AOp, ASection, AUnit: TNyxText): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('op', NyxData(AOp)), NyxField('section', NyxData(ASection)),
      NyxField('unit', NyxData(AUnit))]);
  end;

  function Args(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('edit-imports')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
      NyxField('changes', AChanges)]);
  end;

  function Query(const ASection: TNyxText; AOffset, ALimit: Integer): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_pascal', 'Scooty', NyxObject([
      NyxField('mode', NyxData('imports')), NyxField('section', NyxData(ASection)),
      NyxField('offset', NyxData(AOffset)), NyxField('limit', NyxData(ALimit))]));
  end;

  function Current: TNyxProjectPair;
  begin
    Result := DecodeNyxProject(LAgent.Exchange(NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))])).Field('project').AsText);
  end;

  procedure Refuse(const ARequest: TNyxDataValue; const AReason: TNyxText;
    const AOwner: TNyxText = 'import-client');
  var
    LBefore: TNyxText;
    LState: TNyxDataValue;
    LAfter: TNyxDataValue;
    LRejected: Boolean;
  begin
    LBefore := EncodeNyxProject(Current);
    LState := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    LRejected := False;
    try
      LAgent.Call('nyx_pascal', 'Scooty', ARequest, AOwner);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    LAfter := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    Check(LRejected and (EncodeNyxProject(Current) = LBefore) and
      (LAgent.Revision = LRevision), AReason);
    Check((LState.Field('selection').AsText = LAfter.Field('selection').AsText) and
      (LState.Field('canUndo').AsBoolean = LAfter.Field('canUndo').AsBoolean) and
      (LState.Field('canRedo').AsBoolean = LAfter.Field('canRedo').AsBoolean),
      'refusal preserves independent navigation and paired history');
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
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(AValue))),
      NyxField('selection', NyxData('short-note')), NyxField('view', NyxData('import-workshop'))]));
    LRevision := LAgent.Revision;
  end;

begin
  LChecks := 0;
  LAgent := nil;
  LStudio := nil;
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Import workshop';
    LPage := NewNyxPage('import-workshop');
    LPage.Configure.Layout(nlColumn).Padding(20).Gap(12).Done;
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxMemo('short-note').Configure.Text('A short note').Done);
    LStudio := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument)));
    LStudio.Select('short-note');
    LHandler := LStudio.AddCallback(ntBeforeTextInput, LLine);
    LPair := LStudio.ProjectSnapshot;
    LHandlerCode := ReadNyxHandlerSource(LPair.Source, LHandler);
    LBody := #10 + 'begin' + #10 + #10 +
      '  if AExecution.Cancelled or not AEvent.HasTextEdit then' + #10 +
      '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 + #10 +
      '  if NyxTextScalarCount(AEvent.TextEdit.After) > ImportedLimit then' + #10 +
      '  begin' + #10 + '    NyxEventResponse(AExecution).Consume;' + #10 +
      '  end;' + #10 + 'end;';
    LSource := ReplaceNyxHandlerImplementation(LPair.Source, LHandler, LHandlerCode.Code, LBody);
    LPosition := Pos('implementation', LSource);
    Check(LPosition > 0, 'seed has exact ordinary implementation header');
    LSourceParts := TNyxStrings.Create;
    try
      LSourceParts.Add(Copy(LSource, 1, LPosition - 1));
      LSourceParts.Add('function ImportedLimit: Integer;' + #10 +
        'function ImportedCaption: TNyxText;' + #10 + #10 + 'implementation' + #10 +
        '// Retained handwritten helper 🌙; decoy uses Wrong;' + #10 +
        'function ImportedLimit: Integer;' + #10 + 'begin' + #10 +
        '  Result := Ceil(2.25);' + #10 + 'end;' + #10 + #10 +
        'function ImportedCaption: TNyxText;' + #10 + 'begin' + #10 +
        '  Result := TNyxText(''Limit: '') + TNyxText(IntToStr(ImportedLimit));' + #10 +
        'end;' + #10);
      LSourceParts.Add(Copy(LSource, LPosition + Length('implementation'), MaxInt));
      LPair.Source := LSourceParts.Join;
    finally
      LSourceParts.Free;
    end;
    LAgent := TNyxAgentSession.Create(LPair);
    LRevision := LAgent.Revision;
    LInitial := Current;
    LReply := Query('implementation', 0, 1);
    Check((LReply.Field('total').AsInteger = 0) and
      (LReply.Field('units').Count = 0), 'empty implementation has bounded context');
    LReply := Query('interface', 0, 1);
    Check((LReply.Field('units').Count = 1) and (LReply.Field('total').AsInteger > 1) and
      (LReply.Field('revision').AsInteger = LRevision), 'query returns only requested revision/window');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LInitial), 'queries preserve exact pair');
    SetLength(LChanges, 1);
    LChanges[0] := NyxImportEdit(nisImplementation, niaAdd, NyxPascalUnit('Math'));
    LPatch := NyxImportPatch(LChanges);
    LChanges[0] := NyxImportEdit(nisImplementation, niaAdd, NyxPascalUnit('Types'));
    LPair := LPatch.Candidate(LInitial);
    Check(ReadNyxImports(LPair.Source, nisImplementation).UnitAt(0).Name = 'Math',
      'managed patch copies typed proposals independently');

    LRequest := Args('import-group', NyxArray([
      Change('add', 'interface', 'Types'), Change('add', 'implementation', 'Math'),
      Change('add', 'implementation', 'SysUtils')]));
    LReceipt := LAgent.Call('nyx_pascal', 'Scooty', LRequest, 'import-client');
    LRevision := LAgent.Revision;
    LAccepted := Current;
    Check((LReceipt.Field('imports').Field('changes').AsInteger = 3) and
      (LAccepted.Design = LInitial.Design), 'one grouped source publication retains exact design');
    Check((Pos('// Retained handwritten helper 🌙; decoy uses Wrong;', LAccepted.Source) > 0) and
      (ReadNyxHandlerSource(LAccepted.Source, LHandler).Code = LBody) and
      (ReadNyxHandlerSource(LAccepted.Source, LHandler).Signature = LHandlerCode.Signature),
      'comments, callback implementation/signature and helpers stay crafted');
    Check(LAgent.Call('nyx_pascal', 'Scooty', LRequest, 'import-client').ToJSON = LReceipt.ToJSON,
      'exact retry returns original receipt without another history entry');
    Refuse(LRequest, 'foreign authority cannot claim original stale receipt', 'foreign-client');
    LReply := Query('implementation', 1, 1);
    Check((LReply.Field('units').Count = 1) and
      (LReply.Field('units').Item(0).Field('unit').AsText = 'SysUtils'),
      'authored import order supports bounded next-page inspection');
    History('undo', 'undo-import-group');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LInitial), 'one Undo restores entire original pair');
    History('redo', 'redo-import-group');
    Check(EncodeNyxProject(Current) = EncodeNyxProject(LAccepted), 'one Redo restores entire crafted pair');
    Refuse(Args('late-refusal', NyxArray([
      Change('remove', 'interface', 'Types'), Change('add', 'implementation', 'mAtH')])),
      'late duplicate refuses complete group');
    Refuse(Args('missing', NyxArray([Change('remove', 'implementation', 'Unknown')])),
      'missing import refuses');
    Refuse(Args('path', NyxArray([Change('add', 'implementation', '../Math')])),
      'paths cannot substitute for typed namespaces');
    Refuse(Args('unknown', NyxArray([NyxObject([
      NyxField('op', NyxData('add')), NyxField('section', NyxData('implementation')),
      NyxField('unit', NyxData('Math')), NyxField('source', NyxData('injected'))])])),
      'extra code/path payload refuses');
    Refuse(Args('empty', NyxArray([])), 'empty group refuses');
    LAgent.InheritPermission(apReadOnly);
    Refuse(Args('readonly', NyxArray([Change('remove', 'interface', 'Types')])),
      'operator read-only permission protects imports');
    Check(Query('implementation', 0, 1).Field('units').Count = 1, 'read-only context remains available');
    LAgent.InheritPermission(apEdit);
    LAgent.Call('nyx_pascal', 'Scooty', Args('remove-unused', NyxArray([
      Change('remove', 'interface', 'tYpEs')])), 'import-client');
    LRevision := LAgent.Revision;
    LPair := Current;
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + #10 + TNyxText('// independent draft 🌙');
    Publish(LPair);
    Refuse(Args('draft', NyxArray([Change('add', 'interface', 'Types')])),
      'pending draft is retained and blocks external import mutation');
    Check(Query('implementation', 0, 1).Field('pendingDraft').AsBoolean,
      'accepted import context visibly reports pending draft');
    LPair.Pending := False;
    LPair.Draft := '';
    LPair.DraftBase := '';
    Publish(LPair);
    APair := Current;
    Check((APair.Design = LInitial.Design) and
      (ReadNyxImports(APair.Source, nisImplementation).Count = 2),
      'exact final companion retains data and required helper imports');
    Check(LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))])).Field('activity').Count > 0,
      'observing activity includes typed import successes/refusals');
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
