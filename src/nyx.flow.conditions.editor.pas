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

unit nyx.flow.conditions.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.flow.types,
  nyx.flow.declarations, nyx.flow.editing;

type
  { Owned unfinished choices and exact inert graph data. No definition object,
    control, callback or runtime capability is retained. Restore refuses a changed
    graph before writing any field; malformed checkbox values are not admitted. }
  TNyxFlowConditionEditorDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Clear;
    function Defined: Boolean;
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode;
      AIncludePristine: Boolean = True);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFlowConditionEditorDraft; static;
  end;

{ Reusable public Nyx condition form owns an inert definition snapshot. Nil
  definitions render disabled authoring without requiring a compiler or output.
  Display labels use unique ordinal prefixes; captured references retain exact
  original Unicode names and are never parsed from labels. }
function NewNyxFlowConditionEditor(const AID: TNyxText;
  const ADefinition: INyxFlowDefinition): INyxCard;
{ A UI caption only; absent conditions and transitive dependency skipping remain
  distinct. Unknown step references refuse rather than inventing an unconditional step. }
function NyxFlowConditionCaption(const ADefinition: INyxFlowDefinition;
  const AStep: TNyxFlowStepRef): TNyxText;
{ Borrows the current form/root only for this call. Loads one exact saved step,
  replaces its unfinished condition fields and resets removal confirmation.
  An unavailable step refuses before any field changes. No edit is published. }
procedure LoadNyxFlowConditionEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AStep: TNyxFlowStepRef);
{ Handles local step-caption updates and loading. True asks the owning host to
  synchronize its renderer; it never publishes a document edit. Nodes/root are
  borrowed for this call and no callback or renderer is retained. }
function HandleNyxFlowConditionEditor(ANode, ARoot: TNyxNode): Boolean;
{ Captures a complete typed proposal and its exact graph baseline. It neither
  changes the document nor retains the borrowed controls; the owning editor must
  admit the returned change through its normal revision-aware paired command. }
function CaptureNyxFlowConditionEditor(ANode, ARoot: TNyxNode;
  const AReference: TNyxFlowRef; out AChange: TNyxFlowChange): Boolean;

implementation

uses nyx.flow, nyx.bytes;

const
  CEditor = 'nyx.flow-condition-editor';
  CFields: array[0..3] of TNyxText = ('step', 'source', 'expected', 'confirm-clear');

function DisplayName(const AValue: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 1 to Length(AValue) do
  begin
    case AValue[LIndex] of
      #13:
        begin
          Result := Result + TNyxText('\r');
        end;
      #10:
        begin
          Result := Result + TNyxText('\n');
        end;
      else
        begin
          Result := Result + Copy(AValue, LIndex, 1);
        end;
    end;
  end;
end;

function FindCondition(const ADefinition: TNyxDataValue;
  const AStep: TNyxFlowStepRef): TNyxDataValue;
var
  LItems: TNyxDataValue;
  LIndex: Integer;
begin
  Result := NyxNull;

  if ADefinition.Field('version').AsInteger in [2, 3] then
  begin
    LItems := ADefinition.Field('conditions');
    for LIndex := 0 to LItems.Count - 1 do
    begin

      if LItems.Item(LIndex).Field('step').AsText = AStep.Name then
      begin
        Exit(LItems.Item(LIndex));
      end;
    end;
  end;
end;

function NyxFlowConditionCaption(const ADefinition: INyxFlowDefinition;
  const AStep: TNyxFlowStepRef): TNyxText;
var
  LDefinition: INyxFlowDefinition;
  LIndex: Integer;
  LFound: Boolean;
  LCondition: TNyxDataValue;
  LExpected: TNyxText;
begin

  if ADefinition = nil then
  begin
    raise ENyxFlow.Create('Choose an admitted graph to inspect its condition');
  end;
  LDefinition := ADefinition;
  LFound := False;
  for LIndex := 0 to LDefinition.StepCount - 1 do
  begin
    LFound := LFound or (LDefinition.Step(LIndex).Name = AStep.Name);
  end;

  if not LFound then
  begin
    raise ENyxFlow.Create('Choose an exact saved step to inspect its condition');
  end;
  LCondition := FindCondition(LDefinition.ToData, AStep);

  if LCondition.Kind = ndNull then
  begin
    Exit('No explicit condition; the step runs when its inputs are available.');
  end;
  LExpected := 'False';

  if LCondition.Field('expected').AsBoolean then
  begin
    LExpected := 'True';
  end;
  Result := 'Runs when ' + DisplayName(LCondition.Field('source').Field('step').AsText) +
    ' / ' + DisplayName(LCondition.Field('source').Field('port').AsText) + ' is ' + LExpected + '.';
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxFlow.Create('Condition editor field is unavailable');
  end;
end;

function Form(const AID: TNyxText; ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if ARoot <> nil then
  begin
    Result := ARoot.Find(AID);
  end;

  if (Result = nil) or not Result.Extensions.Has(NyxExtension(CEditor)) then
  begin
    raise ENyxFlow.Create('Condition editor is unavailable');
  end;
end;

function FormFor(ANode, ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if (ANode = nil) or (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  Result := ANode;
  while (Result <> nil) and not Result.Extensions.Has(NyxExtension(CEditor)) do
  begin
    Result := Result.Parent;
  end;
end;

function Definition(AForm: TNyxNode): INyxFlowDefinition;
begin
  Result := NyxFlowDefinitionFromData(
    AForm.Extensions.Value(NyxExtension(CEditor)).Field('definition'));
end;

function Target(AForm: TNyxNode): TNyxFlowStepRef;
var
  LItems: TNyxDataValue;
  LIndex: Integer;
begin
  LItems := AForm.Extensions.Value(NyxExtension(CEditor)).Field('steps');
  for LIndex := 0 to LItems.Count - 1 do
  begin

    if LItems.Item(LIndex).Field('label').AsText = Field(AForm, 'step').Prop('value') then
    begin
      Exit(NyxFlowStep(LItems.Item(LIndex).Field('step').AsText));
    end;
  end;
  raise ENyxFlow.Create('Choose an exact step from this graph');
end;

function Source(AForm: TNyxNode): TNyxFlowEndpoint;
var
  LItems: TNyxDataValue;
  LIndex: Integer;
begin
  LItems := AForm.Extensions.Value(NyxExtension(CEditor)).Field('ports');
  for LIndex := 0 to LItems.Count - 1 do
  begin

    if LItems.Item(LIndex).Field('label').AsText = Field(AForm, 'source').Prop('value') then
    begin
      Exit(NyxFlowEndpoint(NyxFlowStep(LItems.Item(LIndex).Field('step').AsText),
        NyxFlowPort(LItems.Item(LIndex).Field('port').AsText)));
    end;
  end;
  raise ENyxFlow.Create('Choose a Boolean output from this graph');
end;

procedure RefreshSummary(AForm: TNyxNode);
var
  LTarget: TNyxFlowStepRef;
  LDefinition: INyxFlowDefinition;
begin
  Field(AForm, 'clear').Configure.Enabled(False).Done;
  Field(AForm, 'summary').Configure.Text('Choose a step and load its saved condition.').Done;

  if (Field(AForm, 'step').Prop('value') = '') or
    (AForm.Extensions.Value(NyxExtension(CEditor)).Field('definition').Kind = ndNull) then
  begin
    Exit;
  end;
  try
    LTarget := Target(AForm);
  except
    on ENyxFlow do
    begin
      { An unfinished or obsolete choice remains intact, without being admitted. }
      Exit;
    end;
  end;
  LDefinition := Definition(AForm);
  Field(AForm, 'summary').Configure.Text('Saved condition: ' +
    NyxFlowConditionCaption(LDefinition, LTarget)).Done;
  Field(AForm, 'clear').Configure.Enabled(
    FindCondition(LDefinition.ToData, LTarget).Kind <> ndNull).Done;
end;

function Checkbox(AForm: TNyxNode; const AName: TNyxText): Boolean;
var
  LValue: TNyxText;
begin
  LValue := Field(AForm, AName).Prop('value');

  if (LValue <> 'true') and (LValue <> 'false') then
  begin
    raise ENyxFlow.Create('Condition checkbox values must be Boolean');
  end;
  Result := LValue = 'true';
end;

function RawValues(AForm: TNyxNode): TNyxDataValue;
var
  LItems: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LItems, Length(CFields));
  for LIndex := 0 to High(CFields) do
  begin
    LItems[LIndex] := NyxField(CFields[LIndex],
      NyxData(Field(AForm, CFields[LIndex]).Prop('value')));
  end;
  Result := NyxObject(LItems);
end;

function NewNyxFlowConditionEditor(const AID: TNyxText;
  const ADefinition: INyxFlowDefinition): INyxCard;
var
  LDefinition: INyxFlowDefinition;
  LData: TNyxDataValue;
  LSteps: array of TNyxDataValue;
  LPorts: array of TNyxDataValue;
  LStepItems: TNyxText;
  LPortItems: TNyxText;
  LLabel: TNyxText;
  LIndex: Integer;
  LPart: Integer;
  LCount: Integer;
  LPort: TNyxFlowPort;
  LDescriptor: TNyxLogicDescriptor;
begin
  LData := NyxNull;
  LStepItems := '';
  LPortItems := '';
  LSteps := nil;
  LPorts := nil;
  { Retain a caller-supplied implementation before its getter can reenter and
    release the caller's reference. Only its inert data enters the form. }
  LDefinition := ADefinition;

  if LDefinition <> nil then
  begin
    LData := LDefinition.ToData;
    LDefinition := NyxFlowDefinitionFromData(LData);
    LData := LDefinition.ToData;
    SetLength(LSteps, LDefinition.StepCount);
    for LIndex := 0 to LDefinition.StepCount - 1 do
    begin
      LLabel := TNyxText(IntToStr(LIndex + 1)) + ' / ' + DisplayName(LDefinition.Step(LIndex).Name);
      LSteps[LIndex] := NyxObject([NyxField('label', NyxData(LLabel)),
        NyxField('step', NyxData(LDefinition.Step(LIndex).Name))]);
      LStepItems := LStepItems + LLabel + #10;
      LDescriptor := LDefinition.Descriptor(LIndex);
      for LPart := 0 to LDescriptor.PortCount - 1 do
      begin
        LPort := LDescriptor.Port(LPart);

        if (LPort.Direction <> nfdOutput) or (LPort.ValueType <> nftBoolean) then
        begin
          Continue;
        end;
        LCount := Length(LPorts);
        LLabel := TNyxText(IntToStr(LCount + 1)) + ' / ' +
          DisplayName(LDefinition.Step(LIndex).Name) + ' / ' + DisplayName(LPort.Reference.Name);
        SetLength(LPorts, LCount + 1);
        LPorts[LCount] := NyxObject([NyxField('label', NyxData(LLabel)),
          NyxField('step', NyxData(LDefinition.Step(LIndex).Name)),
          NyxField('port', NyxData(LPort.Reference.Name))]);
        LPortItems := LPortItems + LLabel + #10;
      end;
    end;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Add(NewNyxHeading(AID + '-title').WithText('Run condition'));
  Result.Add(NewNyxSelect(AID + '-step').Configure.Text('Step to control').Items(LStepItems)
    .Value('').Enabled(LDefinition <> nil).Done);
  Result.Add(NewNyxButton(AID + '-load').Configure.Text('Load saved condition')
    .Enabled(LDefinition <> nil).Done);
  Result.Add(NewNyxLabel(AID + '-summary').WithText('Choose a step and load its saved condition.'));
  Result.Add(NewNyxSelect(AID + '-source').Configure.Text('Boolean output')
    .Items(LPortItems).Value('').Enabled(Length(LPorts) > 0).Done);
  Result.Add(NewNyxCheckbox(AID + '-expected').Configure.Text('Run when this output is True')
    .Value(True).Enabled(Length(LPorts) > 0).Done);
  Result.Add(NewNyxButton(AID + '-set').Configure.Text('Apply condition')
    .Enabled(Length(LPorts) > 0).Done);
  Result.Add(NewNyxCheckbox(AID + '-confirm-clear').Configure
    .Text('I understand that clearing this condition may allow additional work to run.')
    .Value(False).Done);
  Result.Add(NewNyxButton(AID + '-clear').Configure.Text('Clear condition').Enabled(False).Done);
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Only Boolean outputs can control a step. Dependent steps skip when an input producer skips. ' +
    'Each applied condition is one undoable change; runtime host permissions remain separate.'));
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxObject([
    NyxField('definition', LData), NyxField('steps', NyxArray(LSteps)),
    NyxField('ports', NyxArray(LPorts)), NyxField('values', RawValues(Result.Node))]));
end;

procedure LoadNyxFlowConditionEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AStep: TNyxFlowStepRef);
var
  LForm: TNyxNode;
  LDefinition: INyxFlowDefinition;
  LContext: TNyxDataValue;
  LItems: TNyxDataValue;
  LCondition: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
begin
  LForm := Form(AID, ARoot);
  LDefinition := Definition(LForm);
  LContext := LForm.Extensions.Value(NyxExtension(CEditor));
  LItems := LContext.Field('steps');
  LFound := False;
  for LIndex := 0 to LItems.Count - 1 do
  begin

    if LItems.Item(LIndex).Field('step').AsText = AStep.Name then
    begin
      Field(LForm, 'step').Configure.Value(LItems.Item(LIndex).Field('label').AsText).Done;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxFlow.Create('Choose an exact saved step to load its condition');
  end;
  LCondition := FindCondition(LDefinition.ToData, AStep);
  Field(LForm, 'source').Configure.Value('').Done;
  Field(LForm, 'expected').Configure.Value(True).Done;
  Field(LForm, 'confirm-clear').Configure.Value(False).Done;
  Field(LForm, 'clear').Configure.Enabled(LCondition.Kind <> ndNull).Done;

  if LCondition.Kind <> ndNull then
  begin
    LItems := LContext.Field('ports');
    for LIndex := 0 to LItems.Count - 1 do
    begin

      if (LItems.Item(LIndex).Field('step').AsText = LCondition.Field('source').Field('step').AsText) and
        (LItems.Item(LIndex).Field('port').AsText = LCondition.Field('source').Field('port').AsText) then
      begin
        Field(LForm, 'source').Configure.Value(LItems.Item(LIndex).Field('label').AsText).Done;
        Break;
      end;
    end;
    Field(LForm, 'expected').Configure.Value(LCondition.Field('expected').AsBoolean).Done;
  end;
  Field(LForm, 'summary').Configure.Text('Saved condition: ' +
    NyxFlowConditionCaption(LDefinition, AStep)).Done;
end;

function HandleNyxFlowConditionEditor(ANode, ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
begin
  Result := False;
  LForm := FormFor(ANode, ARoot);

  if (LForm <> nil) and (ANode.ID = LForm.ID + '-step') then
  begin
    RefreshSummary(LForm);
    Exit(True);
  end;

  if (LForm <> nil) and (ANode.ID = LForm.ID + '-load') then
  begin
    LoadNyxFlowConditionEditor(LForm.ID, ARoot, Target(LForm));
    Result := True;
  end;
end;

function CaptureNyxFlowConditionEditor(ANode, ARoot: TNyxNode;
  const AReference: TNyxFlowRef; out AChange: TNyxFlowChange): Boolean;
var
  LForm: TNyxNode;
  LDefinition: INyxFlowDefinition;
  LTarget: TNyxFlowStepRef;
  LSource: TNyxFlowEndpoint;
  LExpected: Boolean;
  LCandidate: INyxFlowGraph;
begin
  Result := False;
  AChange := Default(TNyxFlowChange);
  LForm := FormFor(ANode, ARoot);

  if (LForm = nil) or ((ANode.ID <> LForm.ID + '-set') and (ANode.ID <> LForm.ID + '-clear')) then
  begin
    Exit;
  end;
  LDefinition := Definition(LForm);
  LTarget := Target(LForm);

  if ANode.ID = LForm.ID + '-set' then
  begin
    LSource := Source(LForm);
    LExpected := Checkbox(LForm, 'expected');
    LCandidate := NyxFlowConditions(NyxFlowSignatureGraph(LDefinition))
      .When(LTarget, LSource, LExpected).Build;
    AChange := NyxSetFlowCondition(AReference, LTarget, LSource, LExpected);
  end
  else
  begin

    if not Checkbox(LForm, 'confirm-clear') then
    begin
      raise ENyxFlow.Create('Confirm that clearing this condition may allow additional work to run');
    end;
    LCandidate := NyxFlowConditions(NyxFlowSignatureGraph(LDefinition)).Always(LTarget).Build;
    AChange := NyxClearFlowCondition(AReference, LTarget);
  end;
  AChange := AChange.Expect(NyxObject([NyxField('exists', NyxData(True)),
    NyxField('definition', LDefinition.ToData)]));
  Result := True;
end;

procedure TNyxFlowConditionEditorDraft.Clear;
begin
  FData := NyxNull;
end;

function TNyxFlowConditionEditorDraft.Defined: Boolean;
begin
  Result := FData.Kind = ndObject;
end;

procedure TNyxFlowConditionEditorDraft.Capture(const AID: TNyxText;
  ARoot: TNyxNode; AIncludePristine: Boolean);
var
  LForm: TNyxNode;
  LValues: TNyxDataValue;
begin
  Clear;

  if (ARoot = nil) or (ARoot.Find(AID) = nil) then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);
  LValues := RawValues(LForm);

  if not AIncludePristine and
    (LValues.ToJSON = LForm.Extensions.Value(NyxExtension(CEditor)).Field('values').ToJSON) then
  begin
    Exit;
  end;
  FData := NyxObject([NyxField('version', NyxData(1)),
    NyxField('definition', LForm.Extensions.Value(NyxExtension(CEditor)).Field('definition')),
    NyxField('values', LValues)]);
end;

function TNyxFlowConditionEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LIndex: Integer;
  LValue: TNyxText;
begin
  Result := False;

  if not Defined or (ARoot = nil) or (ARoot.Find(AID) = nil) then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);

  if FData.Field('definition').ToJSON <>
    LForm.Extensions.Value(NyxExtension(CEditor)).Field('definition').ToJSON then
  begin
    Exit;
  end;
  for LIndex := 0 to High(CFields) do
  begin
    LValue := FData.Field('values').Field(CFields[LIndex]).AsText;

    if (CFields[LIndex] = 'expected') or (CFields[LIndex] = 'confirm-clear') then
    begin
      Field(LForm, CFields[LIndex]).Configure.Value(LValue = 'true').Done;
    end
    else
    begin
      Field(LForm, CFields[LIndex]).Configure.Value(LValue).Done;
    end;
  end;
  RefreshSummary(LForm);
  Result := True;
end;

function TNyxFlowConditionEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := FData.Copy;
  end;
end;

class function TNyxFlowConditionEditorDraft.FromData(
  const AData: TNyxDataValue): TNyxFlowConditionEditorDraft;
var
  LIndex: Integer;
  LValue: TNyxText;
begin
  Result.Clear;

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ValidateNyxFlowFields(AData, ['version', 'definition', 'values']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxFlow.Create('Unknown condition editor draft version');
  end;

  if AData.Field('definition').Kind <> ndNull then
  begin
    NyxFlowDefinitionFromData(AData.Field('definition'));
  end;
  ValidateNyxFlowFields(AData.Field('values'), CFields);
  for LIndex := 0 to High(CFields) do
  begin
    LValue := AData.Field('values').Field(CFields[LIndex]).AsText;

    if NyxUTF8ByteCount(LValue) > 8192 then
    begin
      raise ENyxFlow.Create('Condition draft field exceeds its text budget');
    end;

    if ((CFields[LIndex] = 'expected') or (CFields[LIndex] = 'confirm-clear')) and
      (LValue <> 'true') and (LValue <> 'false') then
    begin
      raise ENyxFlow.Create('Condition draft checkboxes must be Boolean');
    end;
  end;
  Result.FData := AData.Copy;
end;

end.
