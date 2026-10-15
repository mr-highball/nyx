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
program nyx_logic_json_editing_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.json, nyx.logic, nyx.logic.json.editing,
  nyx.logic.catalog, nyx.logic.editor, nyx.flow, nyx.flow.types, nyx.flow.editing,
  nyx.flow.declarations,
  nyx.scheduler, nyx.model, nyx.codec, nyx.codegen, nyx.source, nyx.controls,
  nyx.studio.session, nyx.studio.projects, nyx.studio.agents, nyx.studio.edits,
  nyx.test.logic.json.editing
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { One invocation's cancellation oracle. A finite read budget proves that
    cancellation is observed inside a transformation, after frame admission. }
  TCountingScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Reads: Integer;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;

procedure TCountingScope.Cancel;
begin
  Reads := 10000;
end;

function TCountingScope.GetCancelled: Boolean;
begin
  Inc(Reads);
  Result := Reads > 8;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Equal(const AValue: TNyxDataValue; const AExpected: TNyxText);
begin
  Check(AValue.ToJSON = TNyxDataValue.ParseJSON(AExpected).ToJSON,
    'Unexpected JSON: ' + AValue.ToJSON);
end;

procedure Refuse(const AOperation: INyxLogicOperation; const AInputs: TNyxFlowValues);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    AOperation.Invoke(AInputs, nil);
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Complete invocation must refuse');
end;

function SetInputs(const ASource, AValue: TNyxDataValue): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpJSON), NyxFlowJSON(ASource))
    .WithValue(NyxLogicPort(nlpRight), NyxFlowJSON(AValue));
end;

function MergeInputs(const ALeft, ARight: TNyxDataValue): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpLeft), NyxFlowJSON(ALeft))
    .WithValue(NyxLogicPort(nlpRight), NyxFlowJSON(ARight));
end;

procedure Core;
const
  CName: TNyxText = 'profile 🐾' + #0;
var
  LSource: TNyxDataValue;
  LRight: TNyxDataValue;
  LResult: TNyxDataValue;
  LOriginal: TNyxText;
  LPath: TNyxDataPath;
  LSet: INyxJSONSet;
  LScope: TCountingScope;
  LScopePort: INyxCancellationScope;
  LFailed: Boolean;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LOperation: INyxLogicOperation;
begin
  LSource := TNyxDataValue.ParseJSON('{"a":{"b":[1,2,3]},"tail":9007199254740993}');
  LOriginal := LSource.ToJSON;
  Equal(NewNyxJSONSet(NyxDataPath.Field('a').Field('b').Item(1), njsReplace)
    .SetValue(LSource, NyxData(NyxDecimal('-0.0e+0'))),
    '{"a":{"b":[1,-0.0e+0,3]},"tail":9007199254740993}');
  Equal(NewNyxJSONSet(NyxDataPath.Field('a').Field('b').Item(0), njsInsert)
    .SetValue(LSource, NyxData(True)), '{"a":{"b":[true,1,2,3]},"tail":9007199254740993}');
  Equal(NewNyxJSONSet(NyxDataPath.Item(3)).SetValue(
    TNyxDataValue.ParseJSON('[1,2,3]'), NyxNull), '[1,2,3,null]');
  Equal(NewNyxJSONSet(NyxDataPath.Item(0), njsInsert).SetValue(NyxArray([]), NyxData(1)), '[1]');
  Equal(NewNyxJSONSet(NyxDataPath.Item(1), njsInsert).SetValue(
    TNyxDataValue.ParseJSON('[1,2]'), NyxData(3)), '[1,3,2]');
  Equal(NewNyxJSONSet(NyxDataPath.Field('new'), njsInsert).SetValue(NyxObject([]), NyxNull), '{"new":null}');
  Equal(NewNyxJSONSet(NyxDataPath.Field('tail')).SetValue(LSource, NyxNull),
    '{"a":{"b":[1,2,3]},"tail":null}');
  Equal(NewNyxJSONSet(NyxDataPath).SetValue(LSource, NyxData(5)), '5');
  Equal(NewNyxJSONSet(NyxDataPath, njsReplace).SetValue(NyxNull, NyxObject([])), '{}');
  Refuse(NewNyxJSONSet(NyxDataPath, njsInsert), SetInputs(LSource, NyxNull));
  Refuse(NewNyxJSONSet(NyxDataPath.Field('missing'), njsReplace), SetInputs(LSource, NyxNull));
  Refuse(NewNyxJSONSet(NyxDataPath.Field('tail'), njsInsert), SetInputs(LSource, NyxNull));
  Refuse(NewNyxJSONSet(NyxDataPath.Field('absent').Field('x')), SetInputs(LSource, NyxNull));
  Refuse(NewNyxJSONSet(NyxDataPath.Item(4)), SetInputs(NyxArray([]), NyxNull));
  Refuse(NewNyxJSONSet(NyxDataPath.Item(0), njsReplace), SetInputs(NyxArray([]), NyxNull));
  Refuse(NewNyxJSONSet(NyxDataPath.Item(0)), SetInputs(LSource, NyxNull));
  Equal(NewNyxJSONRemove(NyxDataPath.Field('a').Field('b').Item(1)).Remove(LSource),
    '{"a":{"b":[1,3]},"tail":9007199254740993}');
  Equal(NewNyxJSONRemove(NyxDataPath.Item(0)).Remove(TNyxDataValue.ParseJSON('[1]')), '[]');
  Equal(NewNyxJSONRemove(NyxDataPath.Field('a')).Remove(LSource), '{"tail":9007199254740993}');
  Equal(NewNyxJSONRemove(NyxDataPath.Field('absent').Field('x'), njmIgnore).Remove(LSource), LOriginal);
  Equal(NewNyxJSONRemove(NyxDataPath.Item(99), njmIgnore).Remove(NyxArray([])), '[]');
  Refuse(NewNyxJSONRemove(NyxDataPath, njmIgnore),
    NyxFlowValues.WithValue(NyxLogicPort(nlpJSON), NyxFlowJSON(LSource)));
  Refuse(NewNyxJSONRemove(NyxDataPath.Field('absent')),
    NyxFlowValues.WithValue(NyxLogicPort(nlpJSON), NyxFlowJSON(LSource)));
  Refuse(NewNyxJSONRemove(NyxDataPath.Item(0), njmIgnore),
    NyxFlowValues.WithValue(NyxLogicPort(nlpJSON), NyxFlowJSON(LSource)));
  Check(LSource.ToJSON = LOriginal, 'All successful/refused path operations preserve source');

  LPath := NyxDataPath.Field(CName).Item(0);
  LSet := NewNyxJSONSet(LPath, njsReplace);
  LPath := LPath.Field('detached');
  LSource := NyxObject([NyxField(CName, NyxArray([NyxData(NyxDecimal('1.234567890123456789e-7'))]))]);
  LResult := LSet.SetValue(LSource, NyxData(CName));
  Check((LResult.Field(CName).Item(0).AsText = CName) and
    (LSet.Path.ToData.Count = 2), 'Unicode/NUL path/text and detached configuration');
  Check(LSource.Field(CName).Item(0).ToJSON = '1.234567890123456789e-7',
    'No approximate number projection or caller alias');

  LSource := TNyxDataValue.ParseJSON('{"a":{"keep":1,"remove":2},"list":[1,2],"null":null}');
  LRight := TNyxDataValue.ParseJSON('{"a":{"remove":null,"new":3},"list":[4],"added":null}');
  Equal(NewNyxJSONMerge.Merge(LSource, LRight),
    '{"a":{"keep":1,"remove":null,"new":3},"list":[4],"null":null,"added":null}');
  Equal(NewNyxJSONMerge(njmpMergePatch).Merge(LSource, LRight),
    '{"a":{"keep":1,"new":3},"list":[4],"null":null}');
  Equal(NewNyxJSONMerge(njmpMergePatch).Merge(NyxData(3),
    TNyxDataValue.ParseJSON('{"nested":{"absent":null}}')), '{"nested":{}}');
  Equal(NewNyxJSONMerge(njmpMergePatch).Merge(LSource, NyxNull), 'null');
  Equal(NewNyxJSONMerge.Merge(LSource, NyxArray([])), '[]');
  Equal(NewNyxJSONMerge(njmpMergePatch).Merge(NyxArray([]), NyxObject([])), '{}');
  Equal(NewNyxJSONMerge.Merge(NyxNull, NyxObject([NyxField(CName, NyxData(CName))])),
    NyxObject([NyxField(CName, NyxData(CName))]).ToJSON);
  Check(LRight.Field('a').Field('remove').Kind = ndNull, 'Merge never edits the supplied patch');
  for LIndex := 0 to 2 do
  begin
    case LIndex of
      0: LOperation := LSet;
      1: LOperation := NewNyxJSONRemove(NyxDataPath.Field(CName), njmIgnore);
      2: LOperation := NewNyxJSONMerge(njmpMergePatch);
    end;
    Check(NyxLogicFromData(LOperation.Descriptor.Kind, LOperation.Configuration)
      .Descriptor.ToData.ToJSON = LOperation.Descriptor.ToData.ToJSON,
      'Closed wire reconstructs the exact specialized schema');
  end;

  SetLength(LFields, NyxMaximumJSONMembers);
  for LIndex := 0 to High(LFields) do
  begin
    LFields[LIndex] := NyxField('field-' + TNyxText(IntToStr(LIndex)), NyxData(LIndex));
  end;
  LSource := NyxObject(LFields);
  Refuse(NewNyxJSONMerge, MergeInputs(LSource, NyxObject([NyxField('extra', NyxData(1))])));
  Refuse(NewNyxJSONSet(NyxDataPath.Field('extra')), SetInputs(LSource, NyxNull));
  Check(LSource.Count = NyxMaximumJSONMembers, 'Member-budget refusal retains input');

  LRight := NyxNull;
  for LIndex := 1 to NyxMaximumJSONDepth - 5 do
  begin
    LRight := NyxArray([LRight]);
  end;
  LPath := NyxDataPath;
  LResult := NyxNull;
  for LIndex := 1 to 10 do
  begin
    LPath := LPath.Item(0);
    LResult := NyxArray([LResult]);
  end;
  Refuse(NewNyxJSONSet(LPath), SetInputs(LResult, LRight));
  Check(LResult.ToJSON = '[[[[[[[[[[null]]]]]]]]]]', 'Combined depth failure retains source');

  LScope := TCountingScope.Create;
  LScopePort := LScope;
  LFailed := False;
  try
    NewNyxJSONMerge.Invoke(MergeInputs(LSource, NyxObject([])), LScopePort);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LScope.Reads > 8), 'Cancellation cooperates inside recursive merge');
  LScope.Reads := 0;
  LFailed := False;
  try
    NewNyxJSONSet(NyxDataPath.Field('field-0')).Invoke(SetInputs(LSource, NyxNull), LScopePort);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LScope.Reads > 8), 'Cancellation cooperates while rebuilding path ancestors');
end;

procedure Authoring;
var
  LDoc: TNyxDocument;
  LParsed: TNyxDocument;
  LFormDoc: TNyxDocument;
  LForm: INyxCard;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LBeforeDraft: TNyxProjectPair;
  LSource: TNyxText;
  LDraft: TNyxLogicEditorDraft;
  LCopy: TNyxLogicEditorDraft;
  LCapture: TNyxLogicEditorCapture;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LVersion: Integer;
  LFailed: Boolean;
  LIndex: Integer;
  LResult: INyxFlowResult;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}
begin
  LDoc := JSONEditingDocument;
  LParsed := nil;
  LFormDoc := nil;
  LWorkspace := nil;
  LSession := nil;
  LAgent := nil;
  try
    LSource := TNyxCodegen.Generate(LDoc);
    Check((Pos('NewNyxJSONSet(NyxDataPath.Field(', LSource) > 0) and
      (Pos('njsReplace)', LSource) > 0) and (Pos('njmpMergePatch)', LSource) > 0) and
      (Pos('njmRequire)', LSource) > 0), 'Crafted source retains specialized constructors/enums');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc), 'Exact whole source admission');
    Check(LWorkspace.Render(LParsed) = LSource, 'No-op generation retains authored text');
    LResult := JSONEditingGraph.Evaluate([
      NyxFlowInput(JSONEditingEndpoint('source', nlpText), NyxFlowText(JSONEditingInput)),
      NyxFlowInput(JSONEditingEndpoint('patch', nlpText), NyxFlowText(JSONEditingPatch)),
      NyxFlowInput(JSONEditingEndpoint('name', nlpText), NyxFlowText('"Grace"'))]);
    Equal(TNyxDataValue.ParseJSON(LResult.Value(JSONEditingEndpoint('format', nlpText)).AsText), JSONEditingExpected);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDoc), LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LFormDoc := TNyxDocument.Create;
    LFormDoc.AddPage(NewNyxPage('form-home'));
    LForm := NewNyxLogicEditor('editor', LSession.Document, NyxFlowRef('Request'));
    LFormDoc.Pages[0].Add(LForm);
    for LIndex := 0 to 2 do
    begin
      case LIndex of
        0: LForm.Node.Find('editor-operation').Configure.Value('json-set').Done;
        1: LForm.Node.Find('editor-operation').Configure.Value('json-remove').Done;
        2: LForm.Node.Find('editor-operation').Configure.Value('json-merge').Done;
      end;
      HandleNyxLogicEditor(LForm.Node.Find('editor-operation'), LFormDoc.Pages[0]);
      Check(LForm.Node.Find('editor-creator-config').Prop('visible') = 'false',
        'Built-ins expose typed controls rather than extension JSON');
      LForm.Node.Find('editor-step-name').Configure.Value('extra-' + TNyxText(IntToStr(LIndex))).Done;
      Check(CaptureNyxLogicEditor(LForm.Node.Find('editor-add'), LFormDoc.Pages[0], LCapture),
        'Public form captures an operation');
      Check(EditNyxFlows(LDoc.Flows, [LCapture.Change]).Definition(NyxFlowRef('Request')).StepCount = 8,
        'Typed form operation admits through the common final-graph boundary');
    end;
    LForm.Node.Find('editor-merge-policy').Configure.Value('not finished').Done;
    LDraft.Capture('editor', LFormDoc.Pages[0]);
    Check(LDraft.ToData.Field('version').AsInteger = 8, 'New unfinished choices have a versioned draft');
    LCopy := TNyxLogicEditorDraft.FromData(LDraft.ToData);
    Check(LCopy.ToData.ToJSON = LDraft.ToData.ToJSON, 'New draft retains exact bytes');
    LForm.Node.Find('editor-merge-policy').Configure.Value('Overlay (preserve null)').Done;
    Check(LCopy.Restore('editor', LFormDoc.Pages[0]) and
      (LForm.Node.Find('editor-merge-policy').Prop('value') = 'not finished'), 'Restore unfinished choice exactly');
    LFailed := False;
    try
      CaptureNyxLogicEditor(LForm.Node.Find('editor-add'), LFormDoc.Pages[0], LCapture);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair)),
      'Invalid unfinished choice preserves accepted pair');

    LAgent := TNyxAgentSession.Create(LPair);
    LVersion := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'JSON editing qualification', NyxObject([
      NyxField('scope', NyxData('catalog')), NyxField('query', NyxData('overlay')),
      NyxField('limit', NyxData(1)), NyxField('expectedRevision', NyxData(LVersion))]));
    Check((LReply.Field('items').Count = 1) and
      (LReply.Field('items').Item(0).Field('kind').AsText = 'json-merge'), 'Bounded semantic intent discovery');
    LArgs := NyxObject([NyxField('expectedRevision', NyxData(LVersion)),
      NyxField('operationId', NyxData('json-editing-group')), NyxField('operations', NyxArray([
        TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Request","step":"rename","kind":"json-set","configuration":{"path":["profile","display"],"mode":0}}'),
        TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Request","step":"remove","kind":"json-remove","configuration":{"path":["debug"],"missing":1}}'),
        TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Request","step":"merge","kind":"json-merge","configuration":{"policy":0}}')]))]);
    LAgent.Call('nyx_transaction', 'JSON editing qualification', LArgs);
    Check(LAgent.Revision = LVersion + 1, 'Grouped semantic changes publish once');
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    LAgent.Call('nyx_transaction', 'JSON editing qualification', LArgs);
    Check(LAgent.Revision = LVersion + 1, 'Exact retry has no second history command');
    LAgent.Call('nyx_history', 'JSON editing qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('json-edit-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LPair), 'Semantic paired Undo is exact');
    LAgent.Call('nyx_history', 'JSON editing qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('json-edit-redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter), 'Semantic paired Redo is exact');
    LVersion := LAgent.Revision;
    LFailed := False;
    try
      LAgent.Call('nyx_transaction', 'JSON editing qualification', NyxObject([
        NyxField('expectedRevision', NyxData(LVersion)), NyxField('operationId', NyxData('json-invalid-group')),
        NyxField('operations', NyxArray([
          TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Request","step":"merge","kind":"json-merge","configuration":{"policy":1}}'),
          TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Request","step":"rename","kind":"json-set","configuration":{"path":[],"mode":99}}')]))]));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LAgent.Revision = LVersion) and
      (EncodeNyxProject(LAgent.PreviewPair(LVersion, 'home')) = EncodeNyxProject(LAfter)),
      'Malformed closed policy rejects the complete semantic group without history');
    LSession.SetSourceDraft(LSession.Source + #10 + '// unfinished JSON workflow');
    LBeforeDraft := LSession.ProjectSnapshot;
    LFailed := False;
    try
      LSession.ApplyPatch(NyxFlowPatch([NyxReplaceFlowStep(NyxFlowRef('Request'),
        NyxFlowStep('merge'), NewNyxJSONMerge)]));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBeforeDraft)) and
      (LSession.DraftSource = LSession.Source + #10 + '// unfinished JSON workflow'),
      'Pending handwritten draft and accepted pair survive typed mutation refusal');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LSource[1], Length(LSource));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LAgent.Free;
    LSession.Free;
    LWorkspace.Free;
    LFormDoc.Free;
    LParsed.Free;
    LDoc.Free;
  end;
end;

begin
  try
    Core;
    Authoring;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-json-editing', 'passed');
    document.body.setAttribute('data-json-editing-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' JSON editing/source/form/semantic checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-json-editing', 'failed');
      document.body.setAttribute('data-json-editing-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
