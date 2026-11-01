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
program nyx_choice_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.controls, nyx.choices, nyx.choices.editor, nyx.select.configuration,
  nyx.codec, nyx.codegen, nyx.source, nyx.studio.projects, nyx.studio.session,
  nyx.studio.agents, nyx.studio.edits, nyx.studio.inspector
  {$ifdef PAS2JS}, Web{$endif};

const
  CCaption: TNyxText = 'A friendly caption 🌙';
  CKey: TNyxText = 'second/🌙';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Choices: INyxChoiceItems;
begin
  Result := NewNyxChoices
    .WithChoice(NyxChoice(''), 'None')
    .WithChoice(NyxChoice('first'), CCaption)
    .WithChoice(NyxChoice(CKey), CCaption)
    .WithChoice(NyxChoice(TNyxText('line') + #13#10 + #0 + TNyxText('🌙')), 'Exact special key');
end;

function Seed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LSelect: INyxSelect;
  LInstance: INyxComponent;
begin
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LSelect := NewNyxSelect('operation');
    LSelect.Configure.Text('Operation').Items('Legacy first' + #10 + 'Legacy second')
      .Value(CKey).Done;
    LPage.Add(LSelect);
    LSelect := NewNyxSelect('template');
    NyxSelectConfigure(LSelect).Choices(Choices);
    LDocument.AddComponent(LSelect);
    LInstance := NewNyxComponent('instance');
    LInstance.Reference := NyxComponent('template');
    LPage.Add(LInstance);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LInstance := nil;
    LSelect := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

procedure Semantic;
var
  LAgent: TNyxAgentSession;
  LSeed: TNyxProjectPair;
  LBefore: TNyxProjectPair;
  LAccepted: TNyxProjectPair;
  LReply: TNyxDataValue;
  LArguments: TNyxDataValue;
  LState: TNyxDataValue;
  LItems: TNyxDataValue;
  LRejected: Boolean;
  LRevision: Integer;
  LSerial: Integer;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  {$ifndef PAS2JS}
  LFile: TFileStream;
  LBytes: TNyxBytes;
  {$endif}

  function Mutation(const AOperations: TNyxDataValue): TNyxDataValue;
  begin
    Inc(LSerial);
    Result := NyxObject([NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData(TNyxText('choice-') + TNyxText(IntToStr(LSerial)))),
      NyxField('operations', AOperations)]);
  end;

  procedure Apply(const APatch: INyxDesignPatch);
  begin
    LAgent.Call('nyx_transaction', 'Choice authoring qualification',
      Mutation(NyxDesignChanges(APatch).ToData));
  end;

  function ReadChoices(const AOwner, AScope: TNyxText;
    AOffset: Integer = 0; ATextOffset: Integer = 0; ATextLimit: Integer = 80): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_node', 'Choice authoring qualification', NyxObject([
      NyxField('id', NyxData(AOwner)), NyxField('choices', NyxData(True)),
      NyxField('choiceScope', NyxData(AScope)), NyxField('choiceOffset', NyxData(AOffset)),
      NyxField('choiceLimit', NyxData(1)), NyxField('textOffset', NyxData(ATextOffset)),
      NyxField('textLimit', NyxData(ATextLimit)),
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('limit', NyxData(1))]))
      .Field('choices');
  end;

  procedure Refuse(const AOperations: TNyxDataValue; const AReason: TNyxText);
  var
    LAfter: TNyxDataValue;
  begin
    LBefore := LAgent.PreviewPair(LAgent.Revision, 'home');
    LRevision := LAgent.Revision;
    LState := LAgent.Call('nyx_session', 'Choice authoring qualification', NyxObject([]));
    LRejected := False;
    try
      LAgent.Call('nyx_transaction', 'Choice authoring qualification', Mutation(AOperations));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    LAfter := LAgent.Call('nyx_session', 'Choice authoring qualification', NyxObject([]));
    Check(LRejected and (LAgent.Revision = LRevision) and
      (EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LBefore)) and
      (LState.Field('canUndo').AsBoolean = LAfter.Field('canUndo').AsBoolean) and
      (LState.Field('canRedo').AsBoolean = LAfter.Field('canRedo').AsBoolean), AReason);
  end;

begin
  LSeed := Seed;
  LAgent := TNyxAgentSession.Create(LSeed);
  LSerial := 0;
  LRead := nil;
  LWorkspace := nil;
  try
    LAgent.InheritPermission(apEdit);
    LReply := ReadChoices('operation', 'local');
    Check(not LReply.Field('defined').AsBoolean and
      not LReply.Field('localDeclared').AsBoolean and (LReply.Field('total').AsInteger = 0),
      'legacy Items are distinguished from an explicit empty choice set');
    LReply := LAgent.Call('nyx_node', 'Choice authoring qualification', NyxObject([
      NyxField('id', NyxData('operation')), NyxField('limit', NyxData(1))]));
    Check(not NyxAgentHas(LReply, 'choices'), 'choice rows require explicit opt-in');
    LArguments := Mutation(NyxArray([
      NyxDesignChanges(NyxSetChoices(NyxControl('operation'), Choices)).ToData.Item(0),
      NyxObject([NyxField('op', NyxData('update')), NyxField('id', NyxData('operation')),
        NyxField('properties', NyxObject([NyxField('text', NyxData('Friendly operations'))]))])]));
    LRevision := LAgent.Revision;
    LAgent.Call('nyx_transaction', 'Choice authoring qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'choice and caption edits publish one paired revision');
    LAccepted := LAgent.PreviewPair(LAgent.Revision, 'home');
    Check(Pos('NewNyxChoices', LAccepted.Source) > 0,
      'ordinary semantic changes emit crafted typed choice source');
    LAgent.Call('nyx_transaction', 'Choice authoring qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'identical mutation retry adds no history');
    LReply := ReadChoices('operation', 'effective', 2, 7, 1);
    LItems := LReply.Field('items');
    Check((LReply.Field('total').AsInteger = 4) and (LItems.Count = 1) and
      (LItems.Item(0).Field('key').Field('value').AsText = TNyxText('🌙')) and
      (LItems.Item(0).Field('key').Field('totalScalars').AsInteger = 8) and
      LItems.Item(0).Field('key').Field('truncated').AsBoolean,
      'row and scalar windows preserve supplementary Unicode without pretending to be full identities');
    LReply := ReadChoices('operation', 'effective', 4);
    Check((LReply.Field('total').AsInteger = 4) and (LReply.Field('items').Count = 0),
      'an exhausted page returns no invented choice');
    LRead := TNyxSourceWorkspace.PrepareDraft(LAccepted.Source, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LAccepted.Design,
      'the semantic publication admits the exact generated source/design pair');
    LRead.Free;
    LRead := nil;
    LWorkspace.Free;
    LWorkspace := nil;
    LAgent.Call('nyx_history', 'Choice authoring qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('choice-undo')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LSeed),
      'one Undo restores the exact complete pair before the grouped change');
    LAgent.Call('nyx_history', 'Choice authoring qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('choice-redo')), NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAccepted),
      'Redo restores the exact complete grouped result');
    LReply := ReadChoices('instance', 'local');
    Check(not LReply.Field('defined').AsBoolean, 'local inspection does not smuggle in inherited choices');
    LReply := ReadChoices('instance', 'effective');
    Check(LReply.Field('defined').AsBoolean and not LReply.Field('localDeclared').AsBoolean and
      (LReply.Field('total').AsInteger = 4), 'effective inspection resolves inherited choices');
    Apply(NyxSetChoices(NyxControl('instance'), NewNyxChoices));
    LReply := ReadChoices('instance', 'effective');
    Check(LReply.Field('defined').AsBoolean and LReply.Field('localDeclared').AsBoolean and
      (LReply.Field('total').AsInteger = 0), 'an explicit empty instance masks the definition');
    Apply(NyxInheritChoices(NyxControl('instance')));
    Check(ReadChoices('instance', 'effective').Field('total').AsInteger = 4,
      'inherit removes only the local mask and reveals original definition choices');
    Refuse(NyxDesignChanges(NyxInheritChoices(NyxControl('instance'))).ToData,
      'missing local inheritance refuses atomically');
    Refuse(NyxArray([NyxObject([NyxField('op', NyxData('update')),
      NyxField('id', NyxData('operation')), NyxField('properties', NyxObject([
        NyxField('text', NyxData('Must roll back'))]))]),
      NyxDesignChanges(NyxSetChoices(NyxControl('home'), Choices)).ToData.Item(0)]),
      'a late incompatible choice owner rolls back earlier changes and history');
    Refuse(TNyxDataValue.ParseJSON('[{"op":"choices-set","id":"operation","choices":' +
      '{"version":1,"items":[{"value":"same","caption":"One"},{"value":"same","caption":"Two"}]}}]'),
      'duplicate keys refuse before any publication');
    Refuse(TNyxDataValue.ParseJSON('[{"op":"choices-set","id":"operation","choices":null}]'),
      'null is not an empty set or inheritance');
    Refuse(TNyxDataValue.ParseJSON('[{"op":"choices-inherit","id":"operation","extra":true}]'),
      'unknown operation fields refuse');
    LAgent.InheritPermission(apReadOnly);
    Refuse(NyxDesignChanges(NyxSetChoices(NyxControl('operation'), NewNyxChoices)).ToData,
      'read-only permission preserves choice history and pair');
    LAgent.InheritPermission(apEdit);
    LRejected := False;
    try
      LAgent.Call('nyx_node', 'Choice authoring qualification', NyxObject([
        NyxField('id', NyxData('operation')), NyxField('choices', NyxData(True)),
        NyxField('choiceLimit', NyxData(17))]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'query bounds are enforced by the semantic endpoint');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LBytes := NyxEncodeUTF8(LAccepted.Source);
      LFile := TFileStream.Create(ParamStr(1), fmCreate);
      try

        if Length(LBytes) > 0 then
        begin
          LFile.WriteBuffer(LBytes[0], Length(LBytes));
        end;
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LRead.Free;
    LWorkspace.Free;
    LAgent.Free;
  end;
end;

procedure Bounds;
var
  LDocument: TNyxDocument;
  LSelect: INyxSelect;
  LPage: INyxPage;
  LForm: INyxCard;
  LRows: array of TNyxChoiceItem;
  LCaption: TNyxText;
  LIndex: Integer;
begin
  LCaption := '';
  for LIndex := 1 to 61 do
  begin
    LCaption := LCaption + TNyxText('🌙');
  end;
  SetLength(LRows, NyxMaximumChoiceItems);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := NyxChoiceItem(NyxChoice(TNyxText(IntToStr(LIndex))), LCaption);
  end;
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LSelect := NewNyxSelect('maximum-choices');
    LPage.Add(LSelect);
    NyxSelectConfigure(LSelect).Choices(NewNyxChoices(LRows));
    LForm := NewNyxChoiceEditor('maximum-form', LDocument, NyxControl('maximum-choices'));
    Check((LForm.Node.Find(NyxChoiceEditorFieldID('maximum-form', ncefSelected))
      .ChoiceItems.Count = NyxMaximumChoiceItems) and
      (LForm.Node.Find(NyxChoiceEditorActionID('maximum-form', nceaAdd)).Prop('enabled') = 'false'),
      'a maximum-size set produces bounded valid navigation without inventing a 1025th row');
  finally
    LForm := nil;
    LSelect := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

procedure Forms;
var
  LSession: TNyxStudioSession;
  LForm: INyxCard;
  LNext: INyxCard;
  LDraft: TNyxChoiceEditorDraft;
  LChange: TNyxChoiceEditorChange;
  LEdit: TNyxStudioDesignEdit;
  LBefore: TNyxText;
  LRejected: Boolean;
  LSelected: TNyxNode;
begin
  LSession := TNyxStudioSession.Create(Seed);
  try
    LSession.Select('template');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LForm := NewNyxChoiceEditor('choice-form', LSession.Document, NyxControl('template'));
    LSelected := LForm.Node.Find(NyxChoiceEditorFieldID('choice-form', ncefSelected));
    LSelected.Configure.Value('3').Done;
    Check(RouteNyxChoiceEditorSelection(LSelected, LForm.Node), 'ordinary row navigation loads the choice fields');
    Check(LForm.Node.Find(NyxChoiceEditorFieldID('choice-form', ncefEscaped)).Prop('value') = 'true',
      'special exact keys use an explicit lossless escaped form');
    Check(CaptureNyxChoiceEditor(LForm.Node.Find(NyxChoiceEditorActionID('choice-form', nceaSave)),
      LForm.Node, LSession.Document, LChange) and NyxSameChoiceItems(LChange.Items, Choices),
      'saving an unchanged special key preserves CR/LF/NUL and Unicode exactly');
    LForm.Node.Find(NyxChoiceEditorFieldID('choice-form', ncefKey)).Configure.Value('"unfinished').Done;
    LDraft.Capture('choice-form', LForm.Node);
    LNext := NewNyxChoiceEditor('choice-form', LSession.Document, NyxControl('template'));
    Check(LDraft.Restore(LNext.Node) and
      (LNext.Node.Find(NyxChoiceEditorFieldID('choice-form', ncefKey)).Prop('value') = '"unfinished') and
      (LNext.Node.Find(NyxChoiceEditorActionID('choice-form', nceaSave)).Prop('enabled') = 'true'),
      'unfinished row fields and action availability survive form reconstruction');
    LDraft.Capture('another-closed-form', nil);
    Check(LDraft.Restore(LNext.Node), 'an absent form preserves the parked draft identity and input');
    LRejected := False;
    try
      CaptureNyxChoiceEditor(LNext.Node.Find(NyxChoiceEditorActionID('choice-form', nceaSave)),
        LNext.Node, LSession.Document, LChange);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'invalid unfinished input preserves both accepted files and editable draft');
    LSelected.Configure.Value('2').Done;
    RouteNyxChoiceEditorSelection(LSelected, LForm.Node);
    LForm.Node.Find(NyxChoiceEditorFieldID('choice-form', ncefCaption))
      .Configure.Value('Renamed 🌙').Done;
    Check(CaptureNyxChoicesInspector(LSession,
      LForm.Node.Find(NyxChoiceEditorActionID('choice-form', nceaSave)), LForm.Node, LEdit) and
      (LEdit.Action = sdaTransaction) and (LEdit.Selection = 'template') and
      (LEdit.TransactionDesignBaseline = TNyxCodec.Encode(LSession.Document)),
      'ordinary Inspector captures a typed paired command with its exact queued baseline');
    Check(CaptureNyxChoiceEditor(LForm.Node.Find(NyxChoiceEditorActionID('choice-form', nceaDown)),
      LForm.Node, LSession.Document, LChange) and
      (LChange.Items.Item(3).Key.Value = CKey), 'reordering preserves exact identity');
    Check(CaptureNyxChoiceEditor(LForm.Node.Find(NyxChoiceEditorActionID('choice-form', nceaRemove)),
      LForm.Node, LSession.Document, LChange) and (LChange.Items.Count = 3) and
      (LChange.Items.IndexOf(NyxChoice(CKey)) = -1), 'removal affects the chosen exact row');
    LSession.SetSourceDraft(LSession.Source + TNyxText(#10 + '{ Unfinished Pascal }'));
    LRejected := False;
    try
      CaptureNyxChoicesInspector(LSession,
        LForm.Node.Find(NyxChoiceEditorActionID('choice-form', nceaSave)), LForm.Node, LEdit);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LSession.SourceDraftPending,
      'pending Pascal draft refuses Inspector choice publication without discarding input');
    LSession.DiscardSourceDraft;
    LSession.ApplyPatch(NyxSetChoices(NyxControl('template'), NewNyxChoices));
    LNext := NewNyxChoiceEditor('choice-form', LSession.Document, NyxControl('template'));
    Check(not LDraft.Restore(LNext.Node), 'changed accepted choices retire an old form draft');
    LRejected := False;
    try
      CaptureNyxChoiceEditor(LForm.Node.Find(NyxChoiceEditorActionID('choice-form', nceaSave)),
        LForm.Node, LSession.Document, LChange);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a stale form cannot overwrite a later accepted choice set');
  finally
    LNext := nil;
    LForm := nil;
    LSession.Free;
  end;
end;

begin
  try
    Semantic;
    Forms;
    Bounds;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-choice-authoring', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' choice authoring checks';
    {$else}
    WriteLn('PASS ', GChecks, ' choice authoring checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-choice-authoring', 'failed');
      document.body.textContent := 'FAIL after ' + IntToStr(GChecks) + ': ' + LException.Message;
      {$else}
      WriteLn('FAIL after ', GChecks, ': ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
