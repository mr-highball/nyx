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

unit nyx.logic.programs.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.model, nyx.controls, nyx.logic.programs,
  nyx.programs.declarations;

type
  { Exact unfinished choices, independently owned and tied to copied program
    names/signatures. Invalid presentation values remain drafts, not executable
    configuration. No registry, form, document or runtime capability is retained. }
  TNyxProgramLogicEditorDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Clear;
    function Defined: Boolean;
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode;
      AIncludePristine: Boolean = False);
    { Read-only compatibility check allows an owning compound draft to refuse
      before replacing any of its other unfinished controls. }
    function CanRestore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxProgramLogicEditorDraft; static;
  end;

{ Public Nyx form for named program selection and typed number policy. It copies
  schemas only, never source/code or runtime grants. Unsupported opaque ports
  remain explained in the preview and refuse ordinary graph capture. }
function NewNyxProgramLogicEditor(const AID: TNyxText;
  const APrograms: INyxProgramDeclarations): INyxCard;
{ Load only after the exact reference/signature/policy validates against this
  form's copied context. A refused load leaves every current field unchanged. }
procedure LoadNyxProgramLogicEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AOperation: INyxProgramLogic);
{ Validate local choices and return inert typed intent. Invalid/missing names or
  choices raise ENyxFlow; no accepted document, source or history changes here. }
function CaptureNyxProgramLogicEditor(const AID: TNyxText;
  ARoot: TNyxNode): INyxProgramLogic;
{ Selection input updates only local help/visibility; publication belongs to the
  owning Logic command and its complete candidate validation. }
function HandleNyxProgramLogicEditor(ANode, ARoot: TNyxNode): Boolean;

implementation

uses SysUtils, nyx.bytes, nyx.types, nyx.flow.types, nyx.programs.types,
  nyx.programs.contract;

const
  CEditor = 'nyx.program-logic-editor';
  CInitial = 'nyx.program-logic-initial';
  CNumbers: array[TNyxProgramNumberConversion] of TNyxText =
    ('Preserve decimal value', 'Allow approximate Double');

function Form(const AID: TNyxText; ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if ARoot <> nil then
  begin
    Result := ARoot.Find(AID);
  end;

  if (Result = nil) or not Result.Extensions.Has(NyxExtension(CEditor)) then
  begin
    raise ENyxFlow.Create('Program Logic editor is unavailable');
  end;
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxFlow.Create('Program Logic field is unavailable');
  end;
end;

function Values(AForm: TNyxNode): TNyxDataValue;
begin
  Result := NyxObject([
    NyxField('program', NyxData(Field(AForm, 'program').Prop('value'))),
    NyxField('numbers', NyxData(Field(AForm, 'numbers').Prop('value')))]);
end;

function FindContract(AForm: TNyxNode; const AName: TNyxText): TNyxProgramContract;
var
  LItems: TNyxDataValue;
  LIndex: Integer;
begin
  LItems := AForm.Extensions.Value(NyxExtension(CEditor));
  for LIndex := 0 to LItems.Count - 1 do
  begin

    if LItems.Item(LIndex).Field('program').AsText = AName then
    begin
      Exit(TNyxProgramContract.FromData(LItems.Item(LIndex).Field('contract')));
    end;
  end;
  raise ENyxFlow.Create('Choose an available saved Pascal program');
end;

function SelectedContract(AForm: TNyxNode): TNyxProgramContract;
begin
  Result := FindContract(AForm, Field(AForm, 'program').Prop('value'));
end;

procedure ShowSchema(AForm: TNyxNode);
var
  LContract: TNyxProgramContract;
  LPort: TNyxProgramPort;
  LText: TNyxText;
  LIndex: Integer;
  LNumbers: Boolean;
  LChoice: TNyxText;
begin
  try
    LContract := SelectedContract(AForm);
  except
    on E: ENyxFlow do
    begin
      Field(AForm, 'schema').Configure.Text('Choose an available saved Pascal program.').Done;
      Exit;
    end;
  end;
  LText := '';
  LNumbers := False;
  for LIndex := 0 to LContract.PortCount - 1 do
  begin
    LPort := LContract.Port(LIndex);
    LNumbers := LNumbers or (LPort.ValueType.Kind = npkNumber);

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

  if not NyxProgramLogicContractSupported(LContract) then
  begin
    LText := LText + #10 + 'Opaque scene ports need their owning canvas adapter.';
  end;

  if LContract.RoutineCount > 0 then
  begin
    LText := LText + #10 + 'Host routines need explicit runtime services; this graph grants none.';
  end;
  LChoice := Field(AForm, 'numbers').Prop('value');
  Field(AForm, 'numbers').Configure.Visible(LNumbers or
    ((LChoice <> CNumbers[npncPreserveDecimal]) and
      (LChoice <> CNumbers[npncApproximate]))).Done;
  Field(AForm, 'schema').Configure.Text(LText).Done;
end;

function NewNyxProgramLogicEditor(const AID: TNyxText;
  const APrograms: INyxProgramDeclarations): INyxCard;
var
  LPrograms: INyxProgramDeclarations;
  LDefinition: INyxProgramDefinition;
  LSchemas: array of TNyxDataValue;
  LReference: TNyxProgramRef;
  LNames: TNyxText;
  LDefault: TNyxText;
  LIndex: Integer;
begin
  LPrograms := APrograms;

  if LPrograms = nil then
  begin
    raise ENyxFlow.Create('Program Logic editor requires explicit declarations');
  end;
  LPrograms := LPrograms.Clone;

  if LPrograms = nil then
  begin
    raise ENyxFlow.Create('Program form requires independent declaration membership');
  end;
  SetLength(LSchemas, LPrograms.Count);
  LNames := '';
  LDefault := '';
  for LIndex := 0 to High(LSchemas) do
  begin
    LReference := LPrograms.Reference(LIndex);
    LDefinition := LPrograms.Definition(LReference);
    LSchemas[LIndex] := NyxObject([NyxField('program', NyxData(LReference.Name)),
      NyxField('contract', LDefinition.Contract.ToData)]);

    if LIndex > 0 then
    begin
      LNames := LNames + #10;
    end;
    LNames := LNames + LReference.Name;

    if (LDefault = '') and NyxProgramLogicContractSupported(LDefinition.Contract) then
    begin
      LDefault := LReference.Name;
    end;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(0).Gap(10).Surface(False).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxArray(LSchemas));
  Result.Add(NewNyxSelect(AID + '-program').Configure.Text('Saved Pascal program')
    .Items(LNames).Value(LDefault).Enabled(Length(LSchemas) > 0).Done);
  Result.Add(NewNyxSelect(AID + '-numbers').Configure.Text('Decimal to Double conversion')
    .Items(CNumbers[npncPreserveDecimal] + #10 + CNumbers[npncApproximate])
    .Value(CNumbers[npncPreserveDecimal]).Done);
  Result.Add(NewNyxCode(AID + '-schema').Configure.ReadOnly(True).Height(120).Done);
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Edit source and typed parameters in Pascal programs. A graph maps these ports; ' +
    'runtime events and publication keep the ordinary scheduler and cancellation rules.'));
  Result.Node.Extensions.SetValue(NyxExtension(CInitial), Values(Result.Node));
  ShowSchema(Result.Node);
end;

procedure LoadNyxProgramLogicEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AOperation: INyxProgramLogic);
var
  LForm: TNyxNode;
  LOperation: INyxProgramLogic;
  LIntent: INyxProgramLogic;
begin
  LOperation := AOperation;

  if LOperation = nil then
  begin
    raise ENyxFlow.Create('Program Logic loading requires typed intent');
  end;
  LIntent := NewNyxProgramLogic(LOperation.ProgramReference, LOperation.Contract)
    .Numbers(LOperation.NumberConversion);
  LForm := Form(AID, ARoot);

  if FindContract(LForm, LIntent.ProgramReference.Name).ToData.ToJSON <>
    LIntent.Contract.ToData.ToJSON then
  begin
    raise ENyxFlow.Create('Program Logic form signature differs from the loaded intent');
  end;
  Field(LForm, 'program').Configure.Value(LIntent.ProgramReference.Name).Done;
  Field(LForm, 'numbers').Configure.Value(CNumbers[LIntent.NumberConversion]).Done;
  LForm.Extensions.SetValue(NyxExtension(CInitial), Values(LForm));
  ShowSchema(LForm);
end;

function CaptureNyxProgramLogicEditor(const AID: TNyxText;
  ARoot: TNyxNode): INyxProgramLogic;
var
  LForm: TNyxNode;
  LMode: TNyxProgramNumberConversion;
begin
  LForm := Form(AID, ARoot);
  for LMode := Low(TNyxProgramNumberConversion) to High(TNyxProgramNumberConversion) do
  begin

    if Field(LForm, 'numbers').Prop('value') = CNumbers[LMode] then
    begin
      Exit(NewNyxProgramLogic(NyxProgramRef(Field(LForm, 'program').Prop('value')),
        SelectedContract(LForm)).Numbers(LMode));
    end;
  end;
  raise ENyxFlow.Create('Choose a decimal-to-Double conversion policy');
end;

function HandleNyxProgramLogicEditor(ANode, ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
begin
  Result := False;

  if (ANode = nil) or (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  LForm := ANode;
  while (LForm <> nil) and not LForm.Extensions.Has(NyxExtension(CEditor)) do
  begin
    LForm := LForm.Parent;
  end;

  if (LForm <> nil) and ((ANode.ID = LForm.ID + '-program') or
    (ANode.ID = LForm.ID + '-numbers')) then
  begin
    ShowSchema(LForm);
    Result := True;
  end;
end;

procedure TNyxProgramLogicEditorDraft.Clear;
begin
  Self := Default(TNyxProgramLogicEditorDraft);
end;

function TNyxProgramLogicEditorDraft.Defined: Boolean;
begin
  Result := FData.Defined and (FData.Kind <> ndNull);
end;

procedure TNyxProgramLogicEditorDraft.Capture(const AID: TNyxText; ARoot: TNyxNode;
  AIncludePristine: Boolean);
var
  LForm: TNyxNode;
  LValues: TNyxDataValue;
begin
  Clear;
  LForm := Form(AID, ARoot);
  LValues := Values(LForm);

  if not AIncludePristine and
    (LValues.ToJSON = LForm.Extensions.Value(NyxExtension(CInitial)).ToJSON) then
  begin
    Exit;
  end;
  FData := NyxObject([NyxField('version', NyxData(1)),
    NyxField('context', LForm.Extensions.Value(NyxExtension(CEditor))),
    NyxField('values', LValues)]);
end;

function TNyxProgramLogicEditorDraft.CanRestore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
begin
  Result := False;

  if not Defined or (ARoot = nil) then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CEditor)) then
  begin
    Exit;
  end;
  Result := LForm.Extensions.Value(NyxExtension(CEditor)).ToJSON = FData.Field('context').ToJSON;
end;

function TNyxProgramLogicEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
begin
  Result := CanRestore(AID, ARoot);

  if not Result then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);
  Field(LForm, 'program').Configure.Value(FData.Field('values').Field('program').AsText).Done;
  Field(LForm, 'numbers').Configure.Value(FData.Field('values').Field('numbers').AsText).Done;
  ShowSchema(LForm);
  Result := True;
end;

function TNyxProgramLogicEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := FData.Copy;
  end;
end;

class function TNyxProgramLogicEditorDraft.FromData(
  const AData: TNyxDataValue): TNyxProgramLogicEditorDraft;
var
  LContext: TNyxDataValue;
  LIndex: Integer;
  LContract: TNyxProgramContract;
begin
  Result := Default(TNyxProgramLogicEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ValidateNyxFlowFields(AData, ['version', 'context', 'values']);

  if (AData.Field('version').AsInteger <> 1) or
    (NyxUTF8ByteCount(AData.ToJSON) > NyxMaximumProgramDefinitionBytes) then
  begin
    raise ENyxFlow.Create('Program Logic draft exceeds its version/byte boundary');
  end;
  LContext := AData.Field('context');

  if (LContext.Kind <> ndArray) or (LContext.Count > NyxMaximumProgramDefinitions) then
  begin
    raise ENyxFlow.Create('Program Logic draft requires bounded program signatures');
  end;
  for LIndex := 0 to LContext.Count - 1 do
  begin
    ValidateNyxFlowFields(LContext.Item(LIndex), ['program', 'contract']);
    NyxProgramRef(LContext.Item(LIndex).Field('program').AsText);
    LContract := TNyxProgramContract.FromData(LContext.Item(LIndex).Field('contract'));
  end;
  ValidateNyxFlowFields(AData.Field('values'), ['program', 'numbers']);
  AData.Field('values').Field('program').AsText;
  AData.Field('values').Field('numbers').AsText;
  Result.FData := AData.Copy;
end;

end.
