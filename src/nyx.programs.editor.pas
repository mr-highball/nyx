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

unit nyx.programs.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.programs.types,
  nyx.programs.contract, nyx.programs.declarations, nyx.programs.editing;

type
  { Opening selects presentation only; publication/removal remain explicit
    typed commands. None represents a local-only check/schema/reset action. }
  TNyxProgramEditorAction = (npeaNone, npeaOpen, npeaDefine, npeaRemove);

  { Copied exact authoring proposal. No control/document/runtime interface is
    stored in this portable record. Owners publish Change through paired history. }
  TNyxProgramEditorCapture = record
    Action: TNyxProgramEditorAction;
    Reference: TNyxProgramRef;
    Change: TNyxProgramChange;
  end;

  { Unfinished source, numbers and typed contract edits. Capture retains invalid
    source verbatim; Restore requires the exact accepted definition context and
    never compiles or publishes a program. This value owns no renderer/document. }
  TNyxProgramEditorDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Clear;
    function Defined: Boolean;
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxProgramEditorDraft; static;
    { Compare exact context, source, contract and unfinished parameter/budget
      fields, ignoring only the program chooser's navigation value. No parsing
      occurs, so malformed source remains a guarded unfinished proposal. }
    function SameProposal(const AOther: TNyxProgramEditorDraft): Boolean;
  end;

{ Public Nyx composition for named Pascal source, typed input/output schemas,
  existing/pure/canvas vocabulary and explicit budgets. The document is borrowed
  only during construction; the returned controls own copied metadata. }
function NewNyxProgramEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxProgramRef): INyxCard;
{ Local check/port/reset actions update this same tree; callers only Sync it. }
function HandleNyxProgramEditor(ANode, ARoot: TNyxNode): Boolean;
{ Presentation-only open/new choice. This does not compile a definition or
  capture a publication request; dirty proposals refuse before navigation. }
function OpenNyxProgramEditorChoice(ANode, ARoot: TNyxNode;
  out AReference: TNyxProgramRef): Boolean;
{ Callers capture their draft BEFORE entering this boundary. Invalid source,
  schema, budgets or unconfirmed removal raise without changing any form field.
  Dirty source must be applied/restored before opening a different definition.
  Supply the active document to validate exact accepted context and Save-as
  collisions. Without it, the caller owns equivalent revision/context admission;
  ordinary Studio always supplies its current admitted document. }
function CaptureNyxProgramEditor(ANode, ARoot: TNyxNode;
  out ACapture: TNyxProgramEditorCapture; ADocument: TNyxDocument = nil): Boolean;

implementation

uses nyx.programs, nyx.programs.canvas, nyx.flow.types;

const
  CEditor = 'nyx.program-editor';
  CAction = 'nyx.program-action';
  CContext = 'nyx.program-context';
  CContract = 'nyx.program-contract';
  CLimits = 'nyx.program-limits';
  CBaseline = 'nyx.program-baseline';
  CFields: array[0..12] of TNyxText = ('name', 'source', 'host', 'steps', 'depth',
    'allocation', 'port-name', 'port-direction', 'port-kind', 'handle-type', 'port', 'confirm', 'choice');
  CHosts: array[0..2] of TNyxText = ('Keep current contract', 'Pure Pascal', 'Canvas layout');
  CDirections: array[0..1] of TNyxText = ('Input', 'Output');
  CTypes: array[0..6] of TNyxText =
    ('Text', 'Boolean', 'Integer', 'Number', 'JSON', 'Bytes', 'Opaque handle');
  CKinds: array[0..6] of TNyxProgramKind =
    (npkText, npkBoolean, npkInteger, npkNumber, npkJSON, npkBytes, npkHandle);

type
  { Closed private control intents. Serialized ordinals belong only to this
    component's own event metadata, never to project behavior or script authority. }
  TAction = (paOpen, paNew, paCheck, paAddPort, paRemovePort, paRestore, paApply, paDelete);

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxModel.Create('Program editor is missing a required field');
  end;
end;

function Form(const AID: TNyxText; ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if ARoot <> nil then
  begin
    Result := ARoot.Find(AID);
  end;

  if (Result <> nil) and not Result.Extensions.Has(NyxExtension(CContext)) then
  begin
    Result := nil;
  end;
end;

function EventForm(ANode, ARoot: TNyxNode; out AAction: TAction): TNyxNode;
var
  LValue: Integer;
begin
  Result := nil;

  if (ANode = nil) or not ANode.Extensions.Has(NyxExtension(CEditor)) then
  begin
    Exit;
  end;
  LValue := ANode.Extensions.Value(NyxExtension(CAction)).AsInteger;

  if (LValue < Ord(Low(TAction))) or (LValue > Ord(High(TAction))) then
  begin
    raise ENyxModel.Create('Unknown program editor action');
  end;
  AAction := TAction(LValue);
  Result := Form(ANode.Extensions.Value(NyxExtension(CEditor)).AsText, ARoot);
end;

function Values(AForm: TNyxNode): TNyxDataValue;
var
  LValues: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LValues, Length(CFields));
  for LIndex := 0 to High(CFields) do
  begin
    LValues[LIndex] := NyxField(CFields[LIndex], NyxData(Field(AForm, CFields[LIndex]).Prop('value')));
  end;
  Result := NyxObject(LValues);
end;

procedure RestoreValues(AForm: TNyxNode; const AValues: TNyxDataValue);
var
  LIndex: Integer;
  LText: TNyxText;
begin
  for LIndex := 0 to High(CFields) do
  begin
    LText := AValues.Field(CFields[LIndex]).AsText;

    if CFields[LIndex] = 'confirm' then
    begin
      Field(AForm, CFields[LIndex]).Configure.Value(LText = 'true').Done;
    end
    else
    begin
      Field(AForm, CFields[LIndex]).Configure.Value(LText).Done;
    end;
  end;
end;

{ Select owns a newline-delimited caption list. Keep the closed Pascal choices
  central while adapting that public control's existing configuration contract. }
function Choices(const AValues: array of TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 0 to High(AValues) do
  begin

    if LIndex > 0 then
    begin
      Result := Result + #10;
    end;
    Result := Result + AValues[LIndex];
  end;
end;

function Choice(AForm: TNyxNode; const AName: TNyxText;
  const AValues: array of TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AValues) do
  begin

    if Field(AForm, AName).Prop('value') = AValues[LIndex] then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxModel.Create('Choose a declared program setting');
end;

function Number(AForm: TNyxNode; const AName: TNyxText): Integer;
begin

  if not TryStrToInt(Field(AForm, AName).Prop('value'), Result) then
  begin
    raise ENyxModel.Create('Program budgets require integer values');
  end;
end;

function Limits(AForm: TNyxNode): TNyxProgramLimits;
begin
  Result := TNyxProgramLimits.FromData(AForm.Extensions.Value(NyxExtension(CLimits)))
    .Steps(Number(AForm, 'steps')).CallDepth(Number(AForm, 'depth'))
    .AllocationBytes(Number(AForm, 'allocation'));
end;

function Contract(AForm: TNyxNode): TNyxProgramContract;
var
  LOriginal: TNyxProgramContract;
  LPort: TNyxProgramPort;
  LHost: Integer;
  LIndex: Integer;
begin
  LOriginal := TNyxProgramContract.FromData(AForm.Extensions.Value(NyxExtension(CContract)), Limits(AForm));
  LHost := Choice(AForm, 'host', CHosts);

  if LHost = 0 then
  begin
    Exit(LOriginal);
  end;

  if LHost = 1 then
  begin
    Result := NyxProgramContract;
  end
  else
  begin
    Result := NyxCanvasProgramContract;
  end;
  for LIndex := 0 to LOriginal.PortCount - 1 do
  begin
    LPort := LOriginal.Port(LIndex);

    if LPort.Direction = npvInput then
    begin
      Result := Result.Input(LPort.Name, LPort.ValueType);
    end
    else
    begin
      Result := Result.Output(LPort.Name, LPort.ValueType);
    end;
  end;
end;

procedure ShowPorts(AForm: TNyxNode);
var
  LContract: TNyxProgramContract;
  LPort: TNyxProgramPort;
  LNames: TNyxStrings;
  LText: TNyxText;
  LIndex: Integer;
begin
  LContract := TNyxProgramContract.FromData(AForm.Extensions.Value(NyxExtension(CContract)),
    TNyxProgramLimits.FromData(AForm.Extensions.Value(NyxExtension(CLimits))));
  LNames := TNyxStrings.Create;
  try
    LText := '';
    for LIndex := 0 to LContract.PortCount - 1 do
    begin
      LPort := LContract.Port(LIndex);
      LNames.Add(LPort.Name.Name);

      if LIndex > 0 then
      begin
        LText := LText + #10;
      end;

      if LPort.Direction = npvInput then
      begin
        LText := LText + 'Input ';
      end
      else
      begin
        LText := LText + 'Output ';
      end;
      LText := LText + LPort.Name.Name + ': ' + LPort.ValueType.DisplayName;
    end;
    Field(AForm, 'ports').Configure.Text(LText).Done;
    Field(AForm, 'port').Configure.Items(LNames.Join(#10)).Done;
  finally
    LNames.Free;
  end;
end;

function Dirty(AForm: TNyxNode): Boolean;
const
  CMeaning: array[0..5] of TNyxText = ('name', 'source', 'host', 'steps', 'depth', 'allocation');
var
  LBefore: TNyxDataValue;
  LIndex: Integer;
begin
  LBefore := AForm.Extensions.Value(NyxExtension(CBaseline));
  Result := LBefore.Field('contract').ToJSON <>
    AForm.Extensions.Value(NyxExtension(CContract)).ToJSON;
  for LIndex := 0 to High(CMeaning) do
  begin
    Result := Result or (LBefore.Field('values').Field(CMeaning[LIndex]).AsText <>
      Field(AForm, CMeaning[LIndex]).Prop('value'));
  end;
end;

function NewNyxProgramEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxProgramRef): INyxCard;
var
  LDefinition: INyxProgramDefinition;
  LSource: TNyxText;
  LName: TNyxText;
  LContract: TNyxProgramContract;
  LLimits: TNyxProgramLimits;
  LRoot: TNyxNode;
  LIndex: Integer;
  LNames: TNyxStrings;
  LSourceEditor: INyxMemo;
  LContext: TNyxDataValue;

  procedure Button(const ASuffix, ACaption: TNyxText; AAction: TAction);
  var
    LControl: INyxButton;
  begin
    LControl := NewNyxButton(AID + '-' + ASuffix).WithText(ACaption);
    LControl.Node.Extensions.SetValue(NyxExtension(CEditor), NyxData(AID));
    LControl.Node.Extensions.SetValue(NyxExtension(CAction), NyxData(Ord(AAction)));
    LRoot.Add(LControl);
  end;

begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Program editor construction requires a document snapshot');
  end;
  LContract := NyxProgramContract;
  LLimits := NyxProgramLimits;
  LSource := 'begin' + #10 + 'end.' + #10;
  LName := 'Program';
  LIndex := 2;
  while ADocument.Programs.Contains(NyxProgramRef(LName)) do
  begin
    LName := 'Program ' + TNyxText(IntToStr(LIndex));
    Inc(LIndex);
  end;
  LContext := NyxObject([NyxField('name', NyxData('')), NyxField('definition', NyxNull)]);

  if AReference.Defined then
  begin
    LDefinition := ADocument.Programs.Definition(AReference);
    LName := AReference.Name;
    LSource := LDefinition.Source;
    LContract := LDefinition.Contract;
    LLimits := LDefinition.Limits;
    LContext := NyxObject([NyxField('name', NyxData(LName)),
      NyxField('definition', LDefinition.ToData)]);
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(16).Gap(12).Done;
  LRoot := Result.Node;
  LRoot.Extensions.SetValue(NyxExtension(CContext), LContext);
  LRoot.Extensions.SetValue(NyxExtension(CContract), LContract.ToData);
  LRoot.Extensions.SetValue(NyxExtension(CLimits), LLimits.ToData);
  Result.Add(NewNyxHeading(AID + '-heading').WithText('Pascal programs'));
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Write reusable typed behavior. Check it here; runtime hosts and grants are supplied by your application.'));
  LNames := TNyxStrings.Create;
  try
    for LIndex := 0 to ADocument.Programs.Count - 1 do
    begin
      LNames.Add(ADocument.Programs.Reference(LIndex).Name);
    end;
    Result.Add(NewNyxSelect(AID + '-choice').Configure.Text('Saved programs')
      .Items(LNames.Join(#10)).Value(AReference.Name).Done);
  finally
    LNames.Free;
  end;
  Button('open', 'Open selected program', paOpen);
  Button('new', 'New program', paNew);
  Result.Add(NewNyxInput(AID + '-name').Configure.Text('Program name').Value(LName).Done);
  Result.Add(NewNyxSelect(AID + '-host').Configure.Text('Host vocabulary')
    .Items(Choices(CHosts)).Value(CHosts[0]).Done);
  LSourceEditor := NewNyxMemo(AID + '-source');
  LSourceEditor.Configure.Text('Interpreted Pascal source').Value(LSource).Height(320).Done;
  Result.Add(LSourceEditor);
  Button('check', 'Check program', paCheck);
  Result.Add(NewNyxLabel(AID + '-feedback').WithText('No program is executed while checking or saving.'));
  Result.Add(NewNyxHeading(AID + '-schema').WithText('Inputs and outputs'));
  Result.Add(NewNyxLabel(AID + '-ports'));
  Result.Add(NewNyxInput(AID + '-port-name').Configure.Text('Parameter name').Value('').Done);
  Result.Add(NewNyxSelect(AID + '-port-direction').Configure.Text('Direction')
    .Items(Choices(CDirections)).Value(CDirections[0]).Done);
  Result.Add(NewNyxSelect(AID + '-port-kind').Configure.Text('Value type')
    .Items(Choices(CTypes)).Value(CTypes[0]).Done);
  Result.Add(NewNyxInput(AID + '-handle-type').Configure.Text('Opaque handle type, when selected').Value('').Done);
  Button('add-port', 'Add typed parameter', paAddPort);
  Result.Add(NewNyxSelect(AID + '-port').Configure.Text('Existing parameter').Value('').Done);
  Button('remove-port', 'Remove parameter from proposal', paRemovePort);
  Result.Add(NewNyxInput(AID + '-steps').Configure.Text('Instruction/work budget')
    .Value(TNyxText(IntToStr(LLimits.MaximumSteps))).Done);
  Result.Add(NewNyxInput(AID + '-depth').Configure.Text('Call depth')
    .Value(TNyxText(IntToStr(LLimits.MaximumCallDepth))).Done);
  Result.Add(NewNyxInput(AID + '-allocation').Configure.Text('Allocation budget, bytes')
    .Value(TNyxText(IntToStr(LLimits.MaximumAllocationBytes))).Done);
  Button('apply', 'Save program', paApply);
  Button('restore', 'Restore accepted definition', paRestore);
  Result.Add(NewNyxCheckbox(AID + '-confirm').Configure.Text('Confirm program removal').Value(False).Done);
  Button('remove', 'Remove program', paDelete);
  ShowPorts(LRoot);
  LRoot.Extensions.SetValue(NyxExtension(CBaseline), NyxObject([
    NyxField('values', Values(LRoot)), NyxField('contract', LContract.ToData)]));
end;

function HandleNyxProgramEditor(ANode, ARoot: TNyxNode): Boolean;
var
  LAction: TAction;
  LForm: TNyxNode;
  LContract: TNyxProgramContract;
  LNext: TNyxProgramContract;
  LType: TNyxProgramType;
  LPort: TNyxProgramPort;
  LIndex: Integer;
  LSelected: TNyxText;
  LFound: Boolean;
  LReceipt: INyxProgramCompilation;
  LDiagnostic: TNyxProgramDiagnostic;
  LText: TNyxText;
  LBaseline: TNyxDataValue;
begin
  Result := False;
  LForm := EventForm(ANode, ARoot, LAction);

  if LForm = nil then
  begin
    Exit;
  end;

  if not (LAction in [paCheck, paAddPort, paRemovePort, paRestore]) then
  begin
    Exit;
  end;
  Result := True;
  try
    case LAction of
      paCheck:
      begin
        LReceipt := InspectNyxProgram(Field(LForm, 'source').Prop('value'), Contract(LForm), Limits(LForm));

        if LReceipt.Succeeded then
        begin
          LText := 'Program checked. ' + TNyxText(IntToStr(LReceipt.CompiledProgram.CapabilityCount)) +
            ' runtime capabilities required. No code was executed.';
        end
        else
        begin
          LDiagnostic := LReceipt.Diagnostic(0);
          LText := 'Line ' + TNyxText(IntToStr(LDiagnostic.Span.Line)) + ', column ' +
            TNyxText(IntToStr(LDiagnostic.Span.Column)) + ': ' + LDiagnostic.Message;
        end;
        Field(LForm, 'feedback').Configure.Text(LText).Done;
      end;
      paAddPort:
      begin
        LContract := Contract(LForm);
        LIndex := Choice(LForm, 'port-kind', CTypes);

        if CKinds[LIndex] = npkHandle then
        begin
          LType := NyxProgramHandleType(NyxProgramSymbol(Field(LForm, 'handle-type').Prop('value')));
        end
        else
        begin
          LType := NyxProgramType(CKinds[LIndex]);
        end;

        if Choice(LForm, 'port-direction', CDirections) = 0 then
        begin
          LContract := LContract.Input(NyxProgramSymbol(Field(LForm, 'port-name').Prop('value')), LType);
        end
        else
        begin
          LContract := LContract.Output(NyxProgramSymbol(Field(LForm, 'port-name').Prop('value')), LType);
        end;
        LContract.ValidateLimits(Limits(LForm));
        LForm.Extensions.SetValue(NyxExtension(CContract), LContract.ToData);
        Field(LForm, 'host').Configure.Value(CHosts[0]).Done;
        ShowPorts(LForm);
      end;
      paRemovePort:
      begin
        LContract := Contract(LForm);
        LNext := NyxProgramContract;
        LSelected := Field(LForm, 'port').Prop('value');
        LFound := False;

        if LSelected = '' then
        begin
          raise ENyxModel.Create('Choose a parameter to remove from the proposal');
        end;
        for LIndex := 0 to LContract.PortCount - 1 do
        begin
          LPort := LContract.Port(LIndex);

          if LPort.Name.Name = LSelected then
          begin
            LFound := True;
            Continue;
          end;

          if LPort.Direction = npvInput then
          begin
            LNext := LNext.Input(LPort.Name, LPort.ValueType);
          end
          else
          begin
            LNext := LNext.Output(LPort.Name, LPort.ValueType);
          end;
        end;
        for LIndex := 0 to LContract.RoutineCount - 1 do
        begin
          LNext := LNext.WithRoutine(LContract.Routine(LIndex));
        end;

        if not LFound then
        begin
          raise ENyxModel.Create('Choose an existing parameter to remove');
        end;
        LForm.Extensions.SetValue(NyxExtension(CContract), LNext.ToData);
        Field(LForm, 'host').Configure.Value(CHosts[0]).Done;
        ShowPorts(LForm);
      end;
      paRestore:
      begin
        LBaseline := LForm.Extensions.Value(NyxExtension(CBaseline));
        RestoreValues(LForm, LBaseline.Field('values'));
        LForm.Extensions.SetValue(NyxExtension(CContract), LBaseline.Field('contract'));
        Field(LForm, 'feedback').Configure.Text('Accepted definition restored; nothing was published.').Done;
        ShowPorts(LForm);
      end;
      else ;
    end;
  except
    on E: ENyxProgram do
    begin
      Field(LForm, 'feedback').Configure.Text(E.Diagnostic.Message).Done;
    end;
    on E: ENyxModel do
    begin
      Field(LForm, 'feedback').Configure.Text(TNyxText(E.Message)).Done;
    end;
  end;
end;

function OpenNyxProgramEditorChoice(ANode, ARoot: TNyxNode;
  out AReference: TNyxProgramRef): Boolean;
var
  LAction: TAction;
  LForm: TNyxNode;
begin
  AReference := Default(TNyxProgramRef);
  LForm := EventForm(ANode, ARoot, LAction);
  Result := (LForm <> nil) and (LAction in [paOpen, paNew]);

  if not Result then
  begin
    Exit;
  end;

  if Dirty(LForm) then
  begin
    raise ENyxModel.Create('Save or restore your current program proposal before opening another');
  end;

  if LAction = paOpen then
  begin
    AReference := NyxProgramRef(Field(LForm, 'choice').Prop('value'));
  end;
end;

function CaptureNyxProgramEditor(ANode, ARoot: TNyxNode;
  out ACapture: TNyxProgramEditorCapture; ADocument: TNyxDocument): Boolean;
var
  LAction: TAction;
  LForm: TNyxNode;
  LReference: TNyxProgramRef;
  LDefinition: INyxProgramDefinition;
  LContext: TNyxDataValue;
  LName: TNyxText;
begin
  ACapture := Default(TNyxProgramEditorCapture);
  LForm := EventForm(ANode, ARoot, LAction);
  Result := LForm <> nil;

  if not Result then
  begin
    Exit;
  end;

  if (ADocument <> nil) and (LAction in [paApply, paDelete]) then
  begin
    { Match the accepted definition, not a caption or a newer form timestamp.
      Stale mounted proposals and Save-as collisions cannot overwrite another
      agent/operator's accepted definition without explicitly opening it first. }
    LContext := LForm.Extensions.Value(NyxExtension(CContext));
    LName := LContext.Field('name').AsText;

    if LName <> '' then
    begin
      LReference := NyxProgramRef(LName);

      if not ADocument.Programs.Contains(LReference) or
        (ADocument.Programs.Definition(LReference).ToData.ToJSON <>
          LContext.Field('definition').ToJSON) then
      begin
        raise ENyxModel.Create('This program definition changed; reopen its current accepted version');
      end;
    end;

    if LAction = paApply then
    begin
      LReference := NyxProgramRef(Field(LForm, 'name').Prop('value'));

      if (LReference.Name <> LName) and ADocument.Programs.Contains(LReference) then
      begin
        raise ENyxModel.Create('That program name already exists; open it before replacing its definition');
      end;
    end;
  end;
  case LAction of
    paOpen, paNew:
    begin
      ACapture.Action := npeaOpen;
      OpenNyxProgramEditorChoice(ANode, ARoot, ACapture.Reference);
    end;
    paApply:
    begin
      LReference := NyxProgramRef(Field(LForm, 'name').Prop('value'));
      LDefinition := NewNyxProgramDefinition(Field(LForm, 'source').Prop('value'),
        Contract(LForm), Limits(LForm));
      ACapture.Action := npeaDefine;
      ACapture.Reference := LReference;
      ACapture.Change := NyxDefineProgram(LReference, LDefinition);
    end;
    paDelete:
    begin

      if Field(LForm, 'confirm').Prop('value') <> 'true' then
      begin
        raise ENyxModel.Create('Confirm program removal before publishing it');
      end;
      LReference := NyxProgramRef(LForm.Extensions.Value(NyxExtension(CContext)).Field('name').AsText);
      ACapture.Action := npeaRemove;
      ACapture.Reference := LReference;
      ACapture.Change := NyxRemoveProgram(LReference);
    end;
    else Result := False;
  end;
end;

function TNyxProgramEditorDraft.SameProposal(const AOther: TNyxProgramEditorDraft): Boolean;
var
  LIndex: Integer;
begin
  Result := Defined = AOther.Defined;

  if not Result or not Defined then
  begin
    Exit;
  end;
  Result := (FData.Field('context').ToJSON = AOther.FData.Field('context').ToJSON) and
    (FData.Field('contract').ToJSON = AOther.FData.Field('contract').ToJSON);
  for LIndex := 0 to High(CFields) do
  begin

    if CFields[LIndex] <> 'choice' then
    begin
      Result := Result and
        (FData.Field('values').Field(CFields[LIndex]).AsText =
          AOther.FData.Field('values').Field(CFields[LIndex]).AsText);
    end;
  end;
end;

procedure TNyxProgramEditorDraft.Clear;
begin
  FData := NyxNull;
end;

function TNyxProgramEditorDraft.Defined: Boolean;
begin
  Result := FData.Kind = ndObject;
end;

procedure TNyxProgramEditorDraft.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
begin
  LForm := Form(AID, ARoot);

  if LForm = nil then
  begin
    Exit;
  end;
  FData := NyxObject([NyxField('version', NyxData(1)),
    NyxField('context', LForm.Extensions.Value(NyxExtension(CContext))),
    NyxField('values', Values(LForm)),
    NyxField('contract', LForm.Extensions.Value(NyxExtension(CContract)))]);
end;

function TNyxProgramEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
begin
  LForm := Form(AID, ARoot);
  Result := Defined and (LForm <> nil);

  if Result then
  begin
    Result := LForm.Extensions.Value(NyxExtension(CContext)).ToJSON = FData.Field('context').ToJSON;
  end;

  if Result then
  begin
    RestoreValues(LForm, FData.Field('values'));
    LForm.Extensions.SetValue(NyxExtension(CContract), FData.Field('contract'));
    ShowPorts(LForm);
  end;
end;

function TNyxProgramEditorDraft.ToData: TNyxDataValue;
begin

  if not Defined then
  begin
    Exit(NyxNull);
  end;
  Result := FData.Copy;
end;

class function TNyxProgramEditorDraft.FromData(const AData: TNyxDataValue): TNyxProgramEditorDraft;
var
  LIndex: Integer;
begin
  Result := Default(TNyxProgramEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'contract']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxModel.Create('Unsupported program editor draft version');
  end;
  ValidateNyxFlowFields(AData.Field('context'), ['name', 'definition']);
  AData.Field('context').Field('name').AsText;
  for LIndex := 0 to High(CFields) do
  begin
    AData.Field('values').Field(CFields[LIndex]).AsText;
  end;
  TNyxProgramContract.FromData(AData.Field('contract'),
    NyxProgramLimits.Parameters(65536).Symbols(65536).Routines(65536));
  Result.FData := AData.Copy;
end;

end.
