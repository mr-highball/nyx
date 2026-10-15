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

unit nyx.flow.rows.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.types,
  nyx.flow.routes;

type
  { Value-only proposal; callers publish RoutesData through the ordinary paired
    route patch. Revision and exact selected owner guard stale form admission.
    No COM interface in this record, keeping native/pas2js ownership identical. }
  TNyxFlowRowsEditorChange = record
    Owner: TNyxControlRef;
    Revision: Integer;
    RoutesData: TNyxDataValue;
  end;
  { Unfinished field/path text is independent of accepted design. Restore only
    into the same owner/revision. Malformed path input remains editable. }
  TNyxFlowRowsEditorDraft = record
  private
    FData: TNyxDataValue;
    function GetDefined: Boolean;
  public
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    procedure Clear;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFlowRowsEditorDraft; static;
    property Defined: Boolean read GetDefined;
  end;

{ Existing saved routes are the explicit boundary. This component edits their
  JSON-to-collection outputs; generic event/input route creation stays separate.
  All controls are Nyx components. The model is borrowed during each call only. }
function NewNyxFlowRowsEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; ARevision: Integer): INyxCard;
function HandleNyxFlowRowsEditor(ANode, ARoot: TNyxNode;
  ADocument: TNyxDocument; ARevision: Integer): Boolean;
function CaptureNyxFlowRowsEditor(ANode, ARoot: TNyxNode;
  ADocument: TNyxDocument; ARevision: Integer; out AChange: TNyxFlowRowsEditorChange): Boolean;

implementation

uses
  nyx.state, nyx.resources, nyx.data.rows, nyx.collections,
  nyx.flow.types, nyx.flow.declarations, nyx.flow.route.model;

const
  CContext = 'nyx.flow-rows.editor';
  CRecipe = 'nyx.flow-rows.recipe';
  CFields: array[0..10] of TNyxText = ('route', 'collection', 'step', 'port',
    'field', 'scope', 'segment', 'part', 'projection', 'numbers', 'confirm');

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxFlow.Create('The row route form is incomplete');
  end;
end;

function Value(AForm: TNyxNode; const AName: TNyxText): TNyxText;
begin
  Result := Field(AForm, AName).Prop('value');
end;

procedure SetValue(AForm: TNyxNode; const AName, AValue: TNyxText);
begin
  if AName = 'confirm' then
  begin

    if (AValue <> 'true') and (AValue <> 'false') then
    begin
      raise ENyxFlow.Create('Removal confirmation requires a Boolean');
    end;
    Field(AForm, AName).Configure.Value(AValue = 'true').Done;
  end
  else
  begin
    Field(AForm, AName).Configure.Value(AValue).Done;
  end;
end;

function Editor(ANode, ARoot: TNyxNode): TNyxNode;
begin
  Result := ANode;
  while Result <> nil do
  begin

    if Result.Extensions.Has(NyxExtension(CContext)) then
    begin

      if (ARoot = nil) or (ARoot.Find(Result.ID) <> Result) then
      begin
        Exit(nil);
      end;
      Exit;
    end;
    Result := Result.Parent;
  end;
end;

{ Scope context excludes dataset rows and scalar defaults. Exact local routes
  and collection field identities/families guard the editing meaning; the host
  command separately seals its complete current pair/draft before publication. }
function Baseline(ADocument: TNyxDocument; const AOwner: TNyxControlRef): TNyxDataValue;
var
  LCollections: array of TNyxDataValue;
  LFields: array of TNyxDataValue;
  LSchema: TNyxCollectionSchema;
  LKey: TNyxCollectionRef;
  LIndex: Integer;
  LPart: Integer;
begin
  SetLength(LCollections, ADocument.Collections.Count);
  for LIndex := 0 to High(LCollections) do
  begin
    LKey := ADocument.Collections.Key(LIndex);
    LSchema := ADocument.Collections.Snapshot(LKey).Schema;
    SetLength(LFields, LSchema.Count);
    for LPart := 0 to High(LFields) do
    begin
      LFields[LPart] := NyxObject([
        NyxField('name', NyxData(LSchema.FieldAt(LPart).Name)),
        NyxField('kind', NyxData(Ord(LSchema.FieldAt(LPart).Kind)))]);
    end;
    LCollections[LIndex] := NyxObject([NyxField('name', NyxData(LKey.Name)),
      NyxField('fields', NyxArray(LFields))]);
  end;
  Result := NyxObject([NyxField('routes', NyxControlFlowRoutes(ADocument.Find(AOwner.ID)).ToData),
    NyxField('collections', NyxArray(LCollections))]);
end;

procedure CheckContext(AForm: TNyxNode; ADocument: TNyxDocument; ARevision: Integer);
var
  LContext: TNyxDataValue;
begin
  LContext := AForm.Extensions.Value(NyxExtension(CContext));

  if (ADocument = nil) or (LContext.Field('revision').AsInteger <> ARevision) or
    (ADocument.Find(LContext.Field('owner').AsText) = nil) or
    (Baseline(ADocument, NyxControl(LContext.Field('owner').AsText)).ToJSON <>
      LContext.Field('baseline').ToJSON) then
  begin
    raise ENyxFlow.Create('Reopen this control before changing its row route');
  end;
end;

function Route(AForm: TNyxNode; ADocument: TNyxDocument): TNyxFlowRoute;
var
  LRoutes: INyxFlowRoutes;
  LIndex: Integer;
begin
  LRoutes := NyxControlFlowRoutes(ADocument.Find(
    AForm.Extensions.Value(NyxExtension(CContext)).Field('owner').AsText));
  for LIndex := 0 to LRoutes.Count - 1 do
  begin

    if LRoutes.Route(LIndex).ID.Name = Value(AForm, 'route') then
    begin
      Exit(LRoutes.Route(LIndex));
    end;
  end;
  raise ENyxFlow.Create('Choose an exact saved Logic route');
end;

{ Only this form-data boundary rewrites descriptor fields. Public callers use
  TNyxDataRows and typed paths; no application behavior is selected by raw keys. }
function Put(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AData.Count);
  for LIndex := 0 to High(LFields) do
  begin
    LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

    if LFields[LIndex].Name = AName then
    begin
      LFields[LIndex].Value := AValue;
    end;
  end;
  Result := NyxObject(LFields);
end;

function Recipe(AForm: TNyxNode): TNyxDataRows;
begin
  Result := TNyxDataRows.FromData(AForm.Extensions.Value(NyxExtension(CRecipe)));
end;

function FieldIndex(AForm: TNyxNode; const ARows: TNyxDataRows): Integer;
begin
  for Result := 0 to ARows.Schema.Count - 1 do
  begin

    if ARows.Schema.FieldAt(Result).Name = Value(AForm, 'field') then
    begin
      Exit;
    end;
  end;
  raise ENyxFlow.Create('Choose an exact mapped field name');
end;

function SelectedPath(AForm: TNyxNode): TNyxResourcePath;
var
  LRows: TNyxDataRows;
begin
  LRows := Recipe(AForm);

  if Value(AForm, 'scope') = 'Rows' then
  begin
    Exit(LRows.Path);
  end;

  if Value(AForm, 'scope') = 'Identity' then
  begin
    Exit(LRows.IdentityPath);
  end;

  if Value(AForm, 'scope') <> 'Field' then
  begin
    raise ENyxFlow.Create('Choose Rows, Identity or Field path');
  end;
  Result := LRows.FieldPath(FieldIndex(AForm, LRows));
end;

procedure ChangeField(AForm: TNyxNode; const APath: TNyxDataValue;
  ASetProjection: Boolean; AProjection: TNyxRowProjection);
var
  LRows: TNyxDataRows;
  LData: TNyxDataValue;
  LFields: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LSelected: Integer;
begin
  LRows := Recipe(AForm);
  LData := LRows.ToData;
  LFields := LData.Field('fields');
  LSelected := FieldIndex(AForm, LRows);
  SetLength(LItems, LFields.Count);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := LFields.Item(LIndex);

    if LIndex = LSelected then
    begin

      if APath.Defined then
      begin
        LItems[LIndex] := Put(LItems[LIndex], 'path', APath);
      end;

      if ASetProjection then
      begin
        LItems[LIndex] := Put(LItems[LIndex], 'projection', NyxData(Ord(AProjection)));
      end;
    end;
  end;
  LRows := TNyxDataRows.FromData(Put(LData, 'fields', NyxArray(LItems)));
  AForm.Extensions.SetValue(NyxExtension(CRecipe), LRows.ToData);
end;

procedure ChangePath(AForm: TNyxNode; const APath: TNyxResourcePath);
var
  LData: TNyxDataValue;
  LRows: TNyxDataRows;
begin

  if Value(AForm, 'scope') = 'Field' then
  begin
    ChangeField(AForm, APath.ToData, False, nrpDirect);
    Exit;
  end;
  LData := Recipe(AForm).ToData;

  if Value(AForm, 'scope') = 'Rows' then
  begin
    LData := Put(LData, 'path', APath.ToData);
  end
  else if Value(AForm, 'scope') = 'Identity' then
  begin
    LData := Put(LData, 'identity', APath.ToData);
  end
  else
  begin
    raise ENyxFlow.Create('Choose Rows, Identity or Field path');
  end;
  LRows := TNyxDataRows.FromData(LData);
  AForm.Extensions.SetValue(NyxExtension(CRecipe), LRows.ToData);
end;

procedure Preview(AForm: TNyxNode);
begin
  try
    Field(AForm, 'path-preview').Configure.Text(SelectedPath(AForm).ToData.ToJSON).Done;
  except
    on E: Exception do
    begin
      Field(AForm, 'path-preview').Configure.Text('Choose an existing mapped field to inspect its path.').Done;
    end;
  end;
end;

{ Field selection loads that field's admitted projection. Ordinary preview and
  draft restoration deliberately leave an unfinished projection choice alone. }
procedure PrefillProjection(AForm: TNyxNode);
var
  LRows: TNyxDataRows;
  LIndex: Integer;
begin
  LRows := Recipe(AForm);
  for LIndex := 0 to LRows.Schema.Count - 1 do
  begin

    if LRows.Schema.FieldAt(LIndex).Name = Value(AForm, 'field') then
    begin
      SetValue(AForm, 'projection', 'Direct');

      if LRows.Projection(LIndex) = nrpDecimalText then
      begin
        SetValue(AForm, 'projection', 'Decimal text');
      end;
      Exit;
    end;
  end;
end;

procedure UseCollection(AForm: TNyxNode; ADocument: TNyxDocument);
var
  LSchema: TNyxCollectionSchema;
  LRows: TNyxDataRows;
  LField: TNyxCollectionField;
  LIndex: Integer;
begin
  LSchema := ADocument.Collections.Snapshot(NyxCollection(Value(AForm, 'collection'))).Schema;
  LRows := NyxDataRows.Identity(NyxResourcePath.Field('id'));
  for LIndex := 0 to LSchema.Count - 1 do
  begin
    LField := LSchema.FieldAt(LIndex);
    case LField.Kind of
      nskText: LRows := LRows.Text(NyxTextField(LField.Name));
      nskBoolean: LRows := LRows.Boolean(NyxBooleanField(LField.Name));
      nskInteger: LRows := LRows.Integer(NyxIntegerField(LField.Name));
      nskNumber: LRows := LRows.Number(NyxNumberField(LField.Name));
    end;
  end;
  LRows.MatchSchema(LSchema);
  AForm.Extensions.SetValue(NyxExtension(CRecipe), LRows.ToData);
  SetValue(AForm, 'field', LSchema.FieldAt(0).Name);
  SetValue(AForm, 'numbers', 'Canonical');
  PrefillProjection(AForm);
end;

procedure Load(AForm: TNyxNode; ADocument: TNyxDocument);
var
  LRoute: TNyxFlowRoute;
  LRows: TNyxFlowRouteRowsOutput;
  LDefinition: INyxFlowDefinition;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxFlowPort;
  LStep: Integer;
  LPart: Integer;
begin
  LRoute := Route(AForm, ADocument);

  if LRoute.RowOutputCount > 0 then
  begin
    LRows := LRoute.RowOutput(0);
    SetValue(AForm, 'collection', LRows.Collection.Name);
    SetValue(AForm, 'step', LRows.Source.Step.Name);
    SetValue(AForm, 'port', LRows.Source.Port.Name);
    SetValue(AForm, 'field', LRows.Rows.Schema.FieldAt(0).Name);
    SetValue(AForm, 'numbers', 'Canonical');

    if LRows.Rows.NumberPolicy = nrnpApproximate then
    begin
      SetValue(AForm, 'numbers', 'Approximate');
    end;
    AForm.Extensions.SetValue(NyxExtension(CRecipe), LRows.Rows.ToData);
    PrefillProjection(AForm);
    Preview(AForm);
    Exit;
  end;
  UseCollection(AForm, ADocument);
  LDefinition := ADocument.Flows.Definition(LRoute.Flow);
  for LStep := LDefinition.StepCount - 1 downto 0 do
  begin
    LDescriptor := LDefinition.Descriptor(LStep);
    for LPart := 0 to LDescriptor.PortCount - 1 do
    begin
      LPort := LDescriptor.Port(LPart);

      if (LPort.Direction = nfdOutput) and (LPort.ValueType = nftJSON) then
      begin
        SetValue(AForm, 'step', LDefinition.Step(LStep).Name);
        SetValue(AForm, 'port', LPort.Reference.Name);
        Preview(AForm);
        Exit;
      end;
    end;
  end;
  SetValue(AForm, 'step', '');
  SetValue(AForm, 'port', '');
  Preview(AForm);
end;

function NewNyxFlowRowsEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; ARevision: Integer): INyxCard;
var
  LRoutes: INyxFlowRoutes;
  LIndex: Integer;
  LHelp: TNyxText;

  procedure Input(const AName, ACaption, AValue: TNyxText);
  begin
    Result.Add(NewNyxInput(AID + '-' + AName).Configure.Text(ACaption).Value(AValue).Done);
  end;

  procedure Choice(const AName, ACaption, AItems, AValue: TNyxText);
  begin
    Result.Add(NewNyxSelect(AID + '-' + AName).Configure.Text(ACaption).Items(AItems).Value(AValue).Done);
  end;

  procedure Button(const AName, ACaption: TNyxText);
  begin
    Result.Add(NewNyxButton(AID + '-' + AName).WithText(ACaption));
  end;

begin
  Result := NewNyxCard(AID);
  Result.Add(NewNyxHeading(AID + '-title').WithText('Table outputs'));

  if (ADocument = nil) or (ADocument.Find(AOwner.ID) = nil) then
  begin
    raise ENyxFlow.Create('Table route authoring requires an exact selected control');
  end;
  LRoutes := NyxControlFlowRoutes(ADocument.Find(AOwner.ID));

  if (LRoutes.Count = 0) or (ADocument.Collections.Count = 0) then
  begin
    Result.Add(NewNyxLabel(AID + '-help').WithText(
      'Choose a control with a saved Logic route and an application collection.'));
    Exit;
  end;
  Result.Node.Extensions.SetValue(NyxExtension(CContext), NyxObject([
    NyxField('owner', NyxData(AOwner.ID)), NyxField('revision', NyxData(ARevision)),
    NyxField('baseline', Baseline(ADocument, AOwner))]));
  LHelp := 'Saved routes: ';
  for LIndex := 0 to LRoutes.Count - 1 do
  begin
    LHelp := LHelp + LRoutes.Route(LIndex).ID.Name + '; ';
  end;
  Result.Add(NewNyxLabel(AID + '-help').WithText(LHelp));
  Input('route', 'Saved route name', LRoutes.Route(0).ID.Name);
  Button('load', 'Open saved mapping');
  Input('collection', 'Application collection', ADocument.Collections.Key(0).Name);
  Button('use-collection', 'Use this collection schema');
  Input('step', 'JSON output step', '');
  Input('port', 'JSON output port', '');
  Choice('scope', 'Path to edit', 'Rows' + #10 + 'Identity' + #10 + 'Field', 'Identity');
  Input('field', 'Mapped field name', '');
  Choice('segment', 'Path segment', 'Field' + #10 + 'Item', 'Field');
  Input('part', 'Field name or array index', '');
  Button('append', 'Add path segment');
  Button('clear-path', 'Use root path');
  Result.Add(NewNyxLabel(AID + '-path-preview'));
  Choice('projection', 'Text field projection', 'Direct' + #10 + 'Decimal text', 'Direct');
  Button('set-projection', 'Set field projection');
  Choice('numbers', 'Number conversion', 'Canonical' + #10 + 'Approximate', 'Canonical');
  Button('apply', 'Apply Table output');
  Result.Add(NewNyxCheckbox(AID + '-confirm').WithText('Confirm removing this collection output')
    .Configure.Value(False).Done);
  Button('remove', 'Remove Table output');
  Load(Result.Node, ADocument);
end;

function HandleNyxFlowRowsEditor(ANode, ARoot: TNyxNode;
  ADocument: TNyxDocument; ARevision: Integer): Boolean;
var
  LForm: TNyxNode;
  LPath: TNyxResourcePath;
  LIndex: Integer;
  LProjection: TNyxRowProjection;
begin
  Result := False;
  LForm := Editor(ANode, ARoot);

  if LForm = nil then
  begin
    Exit;
  end;
  CheckContext(LForm, ADocument, ARevision);

  if ANode.ID = LForm.ID + '-load' then
  begin
    Load(LForm, ADocument);
  end
  else if ANode.ID = LForm.ID + '-use-collection' then
  begin
    UseCollection(LForm, ADocument);
  end
  else if ANode.ID = LForm.ID + '-append' then
  begin
    LPath := SelectedPath(LForm);

    if Value(LForm, 'segment') = 'Field' then
    begin
      LPath := LPath.Field(Value(LForm, 'part'));
    end
    else if (Value(LForm, 'segment') = 'Item') and
      TryNyxStateInteger(Value(LForm, 'part'), LIndex) then
    begin
      LPath := LPath.Item(LIndex);
    end
    else
    begin
      raise ENyxFlow.Create('An Item segment requires a nonnegative Integer index');
    end;
    ChangePath(LForm, LPath);
  end
  else if ANode.ID = LForm.ID + '-clear-path' then
  begin
    ChangePath(LForm, NyxResourcePath);
  end
  else if ANode.ID = LForm.ID + '-set-projection' then
  begin
    LProjection := nrpDirect;

    if Value(LForm, 'projection') = 'Decimal text' then
    begin
      LProjection := nrpDecimalText;
    end
    else if Value(LForm, 'projection') <> 'Direct' then
    begin
      raise ENyxFlow.Create('Unknown row projection');
    end;
    ChangeField(LForm, Default(TNyxDataValue), True, LProjection);
  end
  else if ANode.ID = LForm.ID + '-field' then
  begin
    PrefillProjection(LForm);
  end
  else if ANode.ID <> LForm.ID + '-scope' then
  begin
    Exit;
  end;
  Preview(LForm);
  Result := True;
end;

function CaptureNyxFlowRowsEditor(ANode, ARoot: TNyxNode;
  ADocument: TNyxDocument; ARevision: Integer; out AChange: TNyxFlowRowsEditorChange): Boolean;
var
  LForm: TNyxNode;
  LRoute: TNyxFlowRoute;
  LRoutes: INyxFlowRoutes;
  LRows: TNyxDataRows;
  LKey: TNyxCollectionRef;
begin
  AChange := Default(TNyxFlowRowsEditorChange);
  Result := False;
  LForm := Editor(ANode, ARoot);

  if (LForm = nil) or ((ANode.ID <> LForm.ID + '-apply') and
    (ANode.ID <> LForm.ID + '-remove')) then
  begin
    Exit;
  end;
  CheckContext(LForm, ADocument, ARevision);
  AChange.Owner := NyxControl(LForm.Extensions.Value(NyxExtension(CContext)).Field('owner').AsText);
  AChange.Revision := ARevision;
  LRoutes := NyxControlFlowRoutes(ADocument.Find(AChange.Owner.ID));
  LRoute := Route(LForm, ADocument);
  LKey := NyxCollection(Value(LForm, 'collection'));

  if ANode.ID = LForm.ID + '-remove' then
  begin

    if Value(LForm, 'confirm') <> 'true' then
    begin
      raise ENyxFlow.Create('Confirm removal before deleting this collection output');
    end;
    LRoute := LRoute.WithoutRows(LKey);
  end
  else
  begin
    LRows := Recipe(LForm);

    if Value(LForm, 'numbers') = 'Approximate' then
    begin
      LRows := LRows.Numbers(nrnpApproximate);
    end
    else if Value(LForm, 'numbers') = 'Canonical' then
    begin
      LRows := LRows.Numbers(nrnpCanonical);
    end
    else
    begin
      raise ENyxFlow.Create('Unknown row number policy');
    end;
    LRows.MatchSchema(ADocument.Collections.Snapshot(LKey).Schema);
    LRoute := LRoute.WithRows(NyxFlowEndpoint(NyxFlowStep(Value(LForm, 'step')),
      NyxFlowPort(Value(LForm, 'port'))), LKey, LRows);
  end;
  ValidateNyxFlowRoute(LRoute, ADocument.Flows.Definition(LRoute.Flow),
    ADocument.State, ADocument.Collections);
  AChange.RoutesData := LRoutes.WithRoute(LRoute).ToData;
  Result := True;
end;

function TNyxFlowRowsEditorDraft.GetDefined: Boolean;
begin
  Result := FData.Defined and (FData.Kind <> ndNull);
end;

procedure TNyxFlowRowsEditorDraft.Clear;
begin
  FData := NyxNull;
end;

function TNyxFlowRowsEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := FData.Copy;
  end;
end;

class function TNyxFlowRowsEditorDraft.FromData(const AData: TNyxDataValue): TNyxFlowRowsEditorDraft;
var
  LValues: TNyxDataValue;
  LContext: TNyxDataValue;
  LRows: TNyxDataRows;
  LIndex: Integer;
begin
  Result := Default(TNyxFlowRowsEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'recipe']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxFlow.Create('Unknown Table route draft version');
  end;
  LContext := AData.Field('context');
  ValidateNyxFlowFields(LContext, ['owner', 'revision', 'baseline']);
  NyxControl(LContext.Field('owner').AsText);
  LContext.Field('revision').AsInteger;
  LValues := AData.Field('values');
  ValidateNyxFlowFields(LValues, CFields);
  for LIndex := 0 to High(CFields) do
  begin
    LValues.Field(CFields[LIndex]).AsText;
  end;
  LRows := TNyxDataRows.FromData(AData.Field('recipe'));
  Result.FData := AData.Copy;
end;

procedure TNyxFlowRowsEditorDraft.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CContext)) then
  begin
    Exit;
  end;
  SetLength(LFields, Length(CFields));
  for LIndex := 0 to High(CFields) do
  begin
    LFields[LIndex] := NyxField(CFields[LIndex], NyxData(Value(LForm, CFields[LIndex])));
  end;
  FData := NyxObject([NyxField('version', NyxData(1)),
    NyxField('context', LForm.Extensions.Value(NyxExtension(CContext))),
    NyxField('values', NyxObject(LFields)),
    NyxField('recipe', Recipe(LForm).ToData)]);
end;

function TNyxFlowRowsEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LIndex: Integer;
begin
  Result := False;

  if not Defined or (ARoot = nil) then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CContext)) or
    (LForm.Extensions.Value(NyxExtension(CContext)).ToJSON <> FData.Field('context').ToJSON) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(CFields) do
  begin
    SetValue(LForm, CFields[LIndex], FData.Field('values').Field(CFields[LIndex]).AsText);
  end;
  LForm.Extensions.SetValue(NyxExtension(CRecipe), FData.Field('recipe'));
  Preview(LForm);
  Result := True;
end;

end.
