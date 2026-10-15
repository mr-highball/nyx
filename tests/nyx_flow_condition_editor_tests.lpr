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

program nyx_flow_condition_editor_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.flow.editing, nyx.logic,
  nyx.flow.conditions.editor, nyx.logic.editor, nyx.test.flow.conditionfixture
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LDoc: TNyxDocument;
  LForms: TNyxDocument;
  LDefinition: INyxFlowDefinition;
  LForm: INyxCard;
  LSame: INyxCard;
  LOther: INyxCard;
  LLogic: INyxCard;
  LEmpty: INyxCard;
  LChange: TNyxFlowChange;
  LDraft: TNyxFlowConditionEditorDraft;
  LLogicDraft: TNyxLogicEditorDraft;
  LRestored: TNyxLogicEditorDraft;
  LChanged: INyxFlowDeclarations;
  LGraph: INyxFlowGraph;
  LBefore: TNyxText;
  LSourceLabel: TNyxText;
  LRejected: Boolean;
  LCapture: TNyxLogicEditorCapture;
  LCaption: TNyxText;
begin
  LDoc := ConditionalFileDocument;
  LForms := TNyxDocument.Create;
  try
    LDefinition := LDoc.Flows.Definition(NyxFlowRef('Read report'));
    LBefore := TNyxCodec.Encode(LDoc);
    LForm := NewNyxFlowConditionEditor('condition', LDefinition);
    LForms.AddComponent(LForm);
    LDraft.Capture('condition', LForm.Node, False);
    Check(not LDraft.Defined, 'unused pristine conditions require no new persisted draft');
    Check(LForm.Node.Find('condition-clear').Prop('enabled') = 'false',
      'clearing remains disabled before a saved condition is selected');
    LoadNyxFlowConditionEditor('condition', LForm.Node, NyxFlowStep('decode'));
    LSourceLabel := LForm.Node.Find('condition-source').Prop('value');
    Check((Pos('choose', LSourceLabel) > 0) and (Pos('read', LSourceLabel) > 0),
      'loading resolves the exact declared Boolean producer');
    Check((LForm.Node.Find('condition-expected').Prop('value') = 'true') and
      (Pos('is True', LForm.Node.Find('condition-summary').Prop('text')) > 0),
      'saved expectation and human condition caption agree');
    Check(LForm.Node.Find('condition-clear').Prop('enabled') = 'true',
      'a saved condition visibly enables its explicit clear action');
    LForm.Node.Find('condition-expected').Configure.Value(False).Done;
    Check(CaptureNyxFlowConditionEditor(LForm.Node.Find('condition-set'), LForm.Node,
      NyxFlowRef('Read report'), LChange) and
      not LChange.ToData.Field('equals').AsBoolean and
      (LChange.ToData.Field('expected').Field('definition').ToJSON = LDefinition.ToData.ToJSON),
      'typed capture separates Boolean intent from exact accepted baseline authority');
    LChanged := EditNyxFlows(LDoc.Flows, [LChange]);
    Check((LChanged.Definition(NyxFlowRef('Read report')).ToData.ToJSON <> LDefinition.ToData.ToJSON) and
      (TNyxCodec.Encode(LDoc) = LBefore), 'capturing and staging preserve the original document');
    LRejected := False;
    try
      CaptureNyxFlowConditionEditor(LForm.Node.Find('condition-clear'), LForm.Node,
        NyxFlowRef('Read report'), LChange);
    except
      LRejected := True;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDoc) = LBefore),
      'unconfirmed clearing refuses before any accepted data changes');
    LForm.Node.Find('condition-confirm-clear').Configure.Value(True).Done;
    Check(CaptureNyxFlowConditionEditor(LForm.Node.Find('condition-clear'), LForm.Node,
      NyxFlowRef('Read report'), LChange) and (LChange.Kind = nfcClearCondition),
      'explicit Boolean confirmation creates a typed clear proposal');
    LForm.Node.Find('condition-source').Configure.Value('unfinished 🌙').Done;
    LRejected := False;
    try
      CaptureNyxFlowConditionEditor(LForm.Node.Find('condition-set'), LForm.Node,
        NyxFlowRef('Read report'), LChange);
    except
      LRejected := True;
    end;
    Check(LRejected and (LForm.Node.Find('condition-source').Prop('value') = TNyxText('unfinished 🌙')),
      'unavailable choices refuse without losing unfinished Unicode input');
    LDraft.Capture('condition', LForm.Node);
    LDraft := TNyxFlowConditionEditorDraft.FromData(LDraft.ToData);
    LSame := NewNyxFlowConditionEditor('same', LDefinition);
    LForms.AddComponent(LSame);
    Check(LDraft.Restore('same', LSame.Node) and
      (LSame.Node.Find('same-source').Prop('value') = TNyxText('unfinished 🌙')) and
      (LSame.Node.Find('same-expected').Prop('value') = 'false') and
      (LSame.Node.Find('same-confirm-clear').Prop('value') = 'true'),
      'owned draft restores all unfinished choices and Boolean intent');
    Check(LSame.Node.Find('same-clear').Prop('enabled') = 'true',
      'draft restoration also refreshes the saved-condition action state');
    LOther := NewNyxFlowConditionEditor('changed', LChanged.Definition(NyxFlowRef('Read report')));
    LForms.AddComponent(LOther);
    LCaption := TNyxCodec.Encode(LForms);
    Check(not LDraft.Restore('changed', LOther.Node) and (TNyxCodec.Encode(LForms) = LCaption),
      'changed graph restoration refuses before mutating any form field');
    LForm.Node.Find('condition-source').Configure.Value(LSourceLabel).Done;
    LRejected := False;
    try
      LForm.Node.Find('condition-expected').Configure.Value('not Boolean').Done;
      CaptureNyxFlowConditionEditor(LForm.Node.Find('condition-set'), LForm.Node,
        NyxFlowRef('Read report'), LChange);
    except
      LRejected := True;
    end;
    Check(LRejected, 'raw text cannot substitute for a Boolean condition choice');
    LForm.Node.Find('condition-expected').Configure.Value(True).Done;
    LoadNyxFlowConditionEditor('condition', LForm.Node, NyxFlowStep('choose'));
    LForm.Node.Find('condition-source').Configure.Value(LSourceLabel).Done;
    LRejected := False;
    try
      CaptureNyxFlowConditionEditor(LForm.Node.Find('condition-set'), LForm.Node,
        NyxFlowRef('Read report'), LChange);
    except
      LRejected := True;
    end;
    Check(LRejected, 'self-dependent UI proposals refuse complete cycle admission');
    LLogic := NewNyxLogicEditor('logic', LDoc, NyxFlowRef('Read report'));
    LForms.AddComponent(LLogic);
    Check(LLogic.Node.Find('logic-conditions').Find('logic-condition-editor') <>
      nil, 'ordinary Logic consumes the public condition form inside its own section');
    Check(LLogic.Node.Find('logic-configuration').Find('logic-file-open') <> nil,
      'opening configuration stays in the ordinary operation section');
    Check(Pos('is True', LLogic.Node.Find('logic-step-card-1-condition').Prop('text')) > 0,
      'saved step cards describe their current condition without requiring a load');
    LLogicDraft.Capture('logic', LLogic.Node);
    Check(LLogicDraft.ToData.Field('version').AsInteger = 3,
      'an unused condition form preserves the legacy Logic draft encoding');
    LoadNyxFlowConditionEditor('logic-condition-editor', LLogic.Node, NyxFlowStep('decode'));
    LLogic.Node.Find('logic-condition-editor-expected').Configure.Value(False).Done;
    LLogic.Node.Find('logic-condition-editor-source').Configure.Value('unfinished 🌙').Done;
    LLogicDraft.Capture('logic', LLogic.Node);
    Check(LLogicDraft.ToData.Field('version').AsInteger = 7,
      'used condition controls own the optional version-7 child');
    LRestored := TNyxLogicEditorDraft.FromData(LLogicDraft.ToData);
    LLogic.Node.Find('logic-condition-editor-source').Configure.Value('').Done;
    Check(LRestored.Restore('logic', LLogic.Node) and
      (LLogic.Node.Find('logic-condition-editor-source').Prop('value') = TNyxText('unfinished 🌙')) and
      (LLogic.Node.Find('logic-condition-editor-expected').Prop('value') = 'false'),
      'whole Logic restoration retains unfinished specialized condition fields');
    LLogic.Node.Find('logic-condition-editor-source').Configure.Value(LSourceLabel).Done;
    Check(CaptureNyxLogicEditor(LLogic.Node.Find('logic-condition-editor-set'), LLogic.Node, LCapture) and
      (LCapture.Change.Kind = nfcSetCondition),
      'the ordinary Logic capture routes nested public condition actions');
    LEmpty := NewNyxFlowConditionEditor('empty', nil);
    LForms.AddComponent(LEmpty);
    Check((LEmpty.Node.Find('empty-set').Prop('enabled') = 'false') and
      (LEmpty.Node.Find('empty-step').Prop('enabled') = 'false'),
      'compiler/output-free graphless authoring remains usable with disabled conditions');

    LGraph := NewNyxFlowGraph.WithStep(NyxFlowStep('Gate 🌙 / ready'),
      NewNyxLogicDeclaration(NyxLogicDescriptor(NyxLogicKind('creator-predicate'),
        'Unicode predicate', 'Exact creator references', nlgFlow)
        .Output(NyxFlowPort('Ready 🌙 / now'), nftBoolean, 'Boolean result'), NyxNull))
      .WithStep(NyxFlowStep('Body 🌙 / report'), NewNyxJSONParse).Build;
    LGraph := NyxFlowConditions(LGraph).When(NyxFlowStep('Body 🌙 / report'),
      NyxFlowEndpoint(NyxFlowStep('Gate 🌙 / ready'), NyxFlowPort('Ready 🌙 / now')), False).Build;
    LDefinition := NyxFlowDefinitionFromData(LGraph.ToData);
    LOther := NewNyxFlowConditionEditor('unicode', LDefinition);
    LForms.AddComponent(LOther);
    LoadNyxFlowConditionEditor('unicode', LOther.Node, NyxFlowStep('Body 🌙 / report'));
    Check(CaptureNyxFlowConditionEditor(LOther.Node.Find('unicode-set'), LOther.Node,
      NyxFlowRef('Unicode paths'), LChange) and
      (LChange.ToData.Field('source').Field('step').AsText = TNyxText('Gate 🌙 / ready')) and
      (LChange.ToData.Field('source').Field('port').AsText = TNyxText('Ready 🌙 / now')),
      'display labels never replace exact supplementary Unicode/slash references');
    Check(Pos(TNyxText('Gate 🌙 / ready'), LOther.Node.Find('unicode-summary').Prop('text')) > 0,
      'native/browser condition captions preserve supplementary text');
  finally
    LForm := nil;
    LSame := nil;
    LOther := nil;
    LLogic := nil;
    LEmpty := nil;
    LForms.Free;
    LDoc.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-condition-editor-contract', 'passed');
    document.body.setAttribute('data-condition-editor-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' public condition editor/draft checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-condition-editor-contract', 'failed');
      document.body.setAttribute('data-condition-editor-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
