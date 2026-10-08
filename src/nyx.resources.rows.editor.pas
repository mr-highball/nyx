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



unit nyx.resources.rows.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.resources,
  nyx.resources.rows, nyx.resources.editor, nyx.collections.registry;

type
  { Closed form roles. Names are open application data; source paths are chosen
    from copied structural descriptors, never interpreted as dot expressions. }
  TNyxResourceRowsField = (rrExisting, rrName, rrResource, rrDataset, rrIdentity,
    rrFieldName, rrFieldType, rrFieldPath, rrMapped, rrReplaceStatic);
  TNyxResourceRowsAction = (raNew, raLoad, raDiscover, raInspect, raSetField,
    raEditField, raRemoveField, raApply, raDetach);
  { Presentation draft owns immutable values only. Exact catalog/collection
    context guards restoration; partial names and unsubmitted mappings survive
    chrome changes without becoming document state or retaining controls. }
  TNyxResourceRowsDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceRowsDraft; static;
    procedure Clear;
  end;

function NyxResourceRowsFieldID(const AID: TNyxText;
  AField: TNyxResourceRowsField): TNyxText;
function NyxResourceRowsActionID(const AID: TNyxText;
  AAction: TNyxResourceRowsAction): TNyxText;
{ Ordinary Nyx controls, shared by both Studio adapters. Discovery is bounded to
  256 choices/2048 visited values/32 path steps and reads authored default data
  or hosted fallback only. Empty arrays retain saved paths when opened.
  Full Apply validates every row and all retained consumers independently. }
function NewNyxResourceRowsEditor(const AID: TNyxText; const AResources: INyxResources;
  const ACollections: INyxCollectionDefaults): INyxCard;
{ Recognizes only exact descendants of a complete current form. Presentation
  actions update copied choices/mappings; Apply/Detach remain paired commands. }
function NyxResourceRowsInput(ANode, ARoot: TNyxNode; out AEditor: TNyxNode): Boolean;
function HandleNyxResourceRowsEditor(ANode, ARoot: TNyxNode): Boolean;
function CaptureNyxResourceRowsEditor(ANode, ARoot: TNyxNode;
  out AChange: TNyxResourceEditorChange): Boolean;

implementation

uses nyx.types, nyx.state, nyx.collections, nyx.collections.codec, nyx.layout.policy,
  nyx.resource.sources;

const
  CEditor = 'nyx.rows-editor';
  CCatalog = 'nyx.rows-editor.catalog';
  CCollections = 'nyx.rows-editor.collections';
  CArrays = 'nyx.rows-editor.arrays';
  CScalars = 'nyx.rows-editor.scalars';
  CFields = 'nyx.rows-editor.fields';
  CFieldNames: array[TNyxResourceRowsField] of TNyxText = ('existing', 'name', 'resource',
    'dataset', 'identity', 'field-name', 'field-type', 'field-path', 'mapped', 'replace-static');
  CLabels: array[TNyxResourceRowsField] of TNyxText = ('Saved relationship', 'Collection name',
    'JSON resource', 'Rows array', 'Text identity in each row', 'Field name',
    'Pascal field type', 'Value in each row', 'Mapped fields',
    'Replace existing collection defaults');
  CActionNames: array[TNyxResourceRowsAction] of TNyxText = ('new', 'load', 'discover',
    'inspect', 'set-field', 'edit-field', 'remove-field', 'apply', 'detach');
  CActionLabels: array[TNyxResourceRowsAction] of TNyxText = ('+ New relationship',
    'Open relationship', 'Discover arrays', 'Inspect row values', 'Add / update field',
    'Edit mapped field', 'Remove mapped field', 'Apply relationship', 'Keep rows and detach');
  CKinds: array[TNyxStateKind] of TNyxText = ('Text', 'Boolean', 'Integer', 'Number');

function NyxResourceRowsFieldID(const AID: TNyxText;
  AField: TNyxResourceRowsField): TNyxText;
begin
  Result := AID + TNyxText('-') + CFieldNames[AField];
end;

function NyxResourceRowsActionID(const AID: TNyxText;
  AAction: TNyxResourceRowsAction): TNyxText;
begin
  Result := AID + TNyxText('-action-') + CActionNames[AAction];
end;

function Complete(AEditor: TNyxNode): Boolean;
var
  LField: TNyxResourceRowsField;
begin
  Result := False;

  if (AEditor = nil) or (AEditor.Prop(CEditor) <> AEditor.ID) or
    (AEditor.Prop(CCatalog) = '') or (AEditor.Prop(CCollections) = '') then
  begin
    Exit;
  end;
  for LField := Low(TNyxResourceRowsField) to High(TNyxResourceRowsField) do
  begin

    if AEditor.Find(NyxResourceRowsFieldID(AEditor.ID, LField)) = nil then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function Field(AEditor: TNyxNode; AField: TNyxResourceRowsField): TNyxNode;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxResource.Create('Resource rows require the complete current form');
  end;
  Result := AEditor.Find(NyxResourceRowsFieldID(AEditor.ID, AField));
end;

function Context(AEditor: TNyxNode): TNyxText;
begin
  Result := NyxObject([NyxField('catalog', NyxData(AEditor.Prop(CCatalog))),
    NyxField('collections', NyxData(AEditor.Prop(CCollections)))]).ToJSON;
end;

function EditorFor(ANode, ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if (ANode = nil) or (ARoot = nil) or (ANode.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  Result := ARoot.Find(ANode.Prop(CEditor));

  if not Complete(Result) or (Result.Find(ANode.ID) <> ANode) then
  begin
    Result := nil;
  end;
end;

function NyxResourceRowsInput(ANode, ARoot: TNyxNode; out AEditor: TNyxNode): Boolean;
var
  LField: TNyxResourceRowsField;
begin
  AEditor := EditorFor(ANode, ARoot);
  Result := False;

  if AEditor = nil then
  begin
    Exit;
  end;
  for LField := Low(TNyxResourceRowsField) to High(TNyxResourceRowsField) do
  begin

    if ANode.ID = NyxResourceRowsFieldID(AEditor.ID, LField) then
    begin
      Exit(True);
    end;
  end;
end;

function Action(ANode, ARoot: TNyxNode; out AEditor: TNyxNode;
  out AAction: TNyxResourceRowsAction): Boolean;
var
  LAction: TNyxResourceRowsAction;
begin
  AEditor := EditorFor(ANode, ARoot);
  AAction := raNew;
  Result := False;

  if AEditor = nil then
  begin
    Exit;
  end;
  for LAction := Low(TNyxResourceRowsAction) to High(TNyxResourceRowsAction) do
  begin

    if ANode.ID = NyxResourceRowsActionID(AEditor.ID, LAction) then
    begin
      AAction := LAction;
      Exit(True);
    end;
  end;
end;

function Data(AEditor: TNyxNode; const AKey: TNyxText): TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(AEditor.Prop(AKey));
end;

function SelectedPath(AEditor: TNyxNode; AField: TNyxResourceRowsField;
  const AChoices: TNyxText): TNyxDataValue;
var
  LChoices: TNyxDataValue;
  LIndex: Integer;
begin
  LChoices := Data(AEditor, AChoices);
  for LIndex := 0 to LChoices.Count - 1 do
  begin

    if LChoices.Item(LIndex).ToJSON = Field(AEditor, AField).Prop('value') then
    begin
      Exit(LChoices.Item(LIndex));
    end;
  end;
  raise ENyxResource.Create('Choose a discovered or saved structural path');
end;

function Payload(AEditor: TNyxNode): TNyxDataValue;
var
  LResources: INyxResources;
  LDefinition: INyxResourceDefinition;
  LReference: TNyxResourceRef;
begin
  LReference := NyxResourceRef(TNyxDataValue.ParseJSON(Field(AEditor, rrResource).Prop('value')).AsText);
  LResources := NyxResourcesFromData(Data(AEditor, CCatalog));
  LDefinition := LResources.Resolve(LReference, NyxDefaultLocale, NyxDefaultLocale);

  if LDefinition.Source.Kind = rskHosted then
  begin
    LDefinition := LDefinition.FallbackDefinition;
  end;

  if (LDefinition = nil) or (LDefinition.Kind <> nrkJSON) then
  begin
    raise ENyxResource.Create('Choose JSON with authored default data or an embedded hosted fallback');
  end;
  Result := LDefinition.Data;
end;

procedure AddPath(var AChoices: TNyxDataValue; const APath: TNyxDataValue);
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
begin
  TNyxResourcePath.FromData(APath);
  SetLength(LValues, AChoices.Count + 1);
  for LIndex := 0 to AChoices.Count - 1 do
  begin

    if AChoices.Item(LIndex).ToJSON = APath.ToJSON then
    begin
      Exit;
    end;
    LValues[LIndex] := AChoices.Item(LIndex);
  end;
  LValues[High(LValues)] := APath;
  AChoices := NyxArray(LValues);
end;

function Discover(const AData: TNyxDataValue; AArrays: Boolean): TNyxDataValue;
var
  LVisited: Integer;

  procedure Visit(const AValue: TNyxDataValue; const APath: TNyxResourcePath;
    ADepth: Integer);
  var
    LIndex: Integer;
  begin

    if (LVisited >= 2048) or (Result.Count >= 256) or (ADepth > 32) then
    begin
      Exit;
    end;
    Inc(LVisited);

    if (AArrays and (AValue.Kind = ndArray)) or
      (not AArrays and (AValue.Kind in [ndText, ndBoolean, ndNumber])) then
    begin
      AddPath(Result, APath.ToData);
    end;

    if ADepth = 32 then
    begin
      Exit;
    end;
    case AValue.Kind of
      ndObject:
        for LIndex := 0 to AValue.Count - 1 do
        begin

          if (LVisited >= 2048) or (Result.Count >= 256) then
          begin
            Break;
          end;
          Visit(AValue.Field(AValue.Key(LIndex)), APath.Field(AValue.Key(LIndex)), ADepth + 1);
        end;
      ndArray:
        for LIndex := 0 to AValue.Count - 1 do
        begin

          if (LVisited >= 2048) or (Result.Count >= 256) then
          begin
            Break;
          end;
          Visit(AValue.Item(LIndex), APath.Item(LIndex), ADepth + 1);
        end;
    else
      begin
        { Scalar leaves and null have no descendants to discover. }
      end;
    end;
  end;

begin
  Result := NyxArray([]);
  LVisited := 0;
  Visit(AData, NyxResourcePath, 0);
end;

procedure Choices(AEditor: TNyxNode; AField: TNyxResourceRowsField;
  const APaths: TNyxDataValue);
var
  LItems: TNyxText;
  LIndex: Integer;
  LValue: TNyxText;
  LFound: Boolean;
begin
  LItems := '';
  LValue := Field(AEditor, AField).Prop('value');
  LFound := False;
  for LIndex := 0 to APaths.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LItems := LItems + TNyxText(#10);
    end;
    LItems := LItems + APaths.Item(LIndex).ToJSON;
    LFound := LFound or (LValue = APaths.Item(LIndex).ToJSON);
  end;
  Field(AEditor, AField).Configure.Items(LItems).Done;

  if not LFound then
  begin
    LValue := '';

    if APaths.Count > 0 then
    begin
      LValue := APaths.Item(0).ToJSON;
    end;
    Field(AEditor, AField).Configure.Value(LValue).Done;
  end;
end;

procedure Refresh(AEditor: TNyxNode);
var
  LFields: TNyxDataValue;
  LNames: array of TNyxDataValue;
  LIndex: Integer;
begin
  Choices(AEditor, rrDataset, Data(AEditor, CArrays));
  Choices(AEditor, rrIdentity, Data(AEditor, CScalars));
  Choices(AEditor, rrFieldPath, Data(AEditor, CScalars));
  LFields := Data(AEditor, CFields);
  SetLength(LNames, LFields.Count);
  for LIndex := 0 to High(LNames) do
  begin
    LNames[LIndex] := LFields.Item(LIndex).Field('name');
  end;
  Choices(AEditor, rrMapped, NyxArray(LNames));
  AEditor.Find(AEditor.ID + TNyxText('-summary')).Configure.Text(
    TNyxText(IntToStr(LFields.Count)) + TNyxText(' typed fields. Discovery is bounded; Apply checks every row.')).Done;
end;

function Recipe(AEditor: TNyxNode): TNyxResourceRows;
begin
  Result := TNyxResourceRows.FromData(NyxObject([NyxField('version', NyxData(1)),
    NyxField('resource', NyxData(NyxResourceRef(TNyxDataValue.ParseJSON(
      Field(AEditor, rrResource).Prop('value')).AsText).Name)),
    NyxField('path', SelectedPath(AEditor, rrDataset, CArrays)),
    NyxField('identity', SelectedPath(AEditor, rrIdentity, CScalars)),
    NyxField('fields', Data(AEditor, CFields))]));
end;

procedure Load(AEditor: TNyxNode);
var
  LDefaults: INyxCollectionDefaults;
  LKey: TNyxCollectionRef;
  LRows: TNyxResourceRows;
  LDescriptor: TNyxDataValue;
  LPaths: TNyxDataValue;
  LIndex: Integer;
begin
  LDefaults := DecodeNyxCollectionDefaults(AEditor.Prop(CCollections), True);
  LKey := NyxCollection(TNyxDataValue.ParseJSON(Field(AEditor, rrExisting).Prop('value')).AsText);

  if not NyxCollectionResourceSource(LDefaults, LKey, LRows) then
  begin
    raise ENyxResource.Create('Open an existing saved resource relationship');
  end;
  LDescriptor := LRows.ToData;
  Field(AEditor, rrName).Configure.Value(LKey.Name).Done;
  Field(AEditor, rrResource).Configure.Value(NyxData(LRows.Reference.Name).ToJSON).Done;
  AEditor.SetProp(CArrays, NyxArray([LRows.Path.ToData]).ToJSON);
  LPaths := NyxArray([LRows.IdentityPath.ToData]);
  for LIndex := 0 to LRows.Schema.Count - 1 do
  begin
    AddPath(LPaths, LRows.FieldPath(LIndex).ToData);
  end;
  AEditor.SetProp(CScalars, LPaths.ToJSON).SetProp(CFields, LDescriptor.Field('fields').ToJSON);
  Field(AEditor, rrReplaceStatic).Configure.Value(False).Done;
  Field(AEditor, rrDataset).Configure.Value(LRows.Path.ToData.ToJSON).Done;
  Field(AEditor, rrIdentity).Configure.Value(LRows.IdentityPath.ToData.ToJSON).Done;
  Refresh(AEditor);
end;

procedure EditFields(AEditor: TNyxNode; AAction: TNyxResourceRowsAction);
var
  LFields: TNyxDataValue;
  LValues: array of TNyxDataValue;
  LField: TNyxDataValue;
  LName: TNyxText;
  LKind: TNyxStateKind;
  LFound: Boolean;
  LIndex: Integer;
  LAt: Integer;
begin
  LFields := Data(AEditor, CFields);
  LName := Field(AEditor, rrFieldName).Prop('value');

  if AAction in [raEditField, raRemoveField] then
  begin
    LName := TNyxDataValue.ParseJSON(Field(AEditor, rrMapped).Prop('value')).AsText;
  end;
  LAt := -1;
  for LIndex := 0 to LFields.Count - 1 do
  begin

    if LFields.Item(LIndex).Field('name').AsText = LName then
    begin
      LAt := LIndex;
    end;
  end;

  if AAction = raEditField then
  begin

    if LAt < 0 then
    begin
      raise ENyxResource.Create('Choose a mapped field to edit');
    end;
    LField := LFields.Item(LAt);
    Field(AEditor, rrFieldName).Configure.Value(LName).Done;
    Field(AEditor, rrFieldPath).Configure.Value(LField.Field('path').ToJSON).Done;
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin

      if NyxStateKindName(LKind) = LField.Field('type').AsText then
      begin
        Field(AEditor, rrFieldType).Configure.Value(CKinds[LKind]).Done;
      end;
    end;
    Exit;
  end;

  if AAction = raRemoveField then
  begin

    if LAt < 0 then
    begin
      raise ENyxResource.Create('Choose a mapped field to remove');
    end;
    SetLength(LValues, LFields.Count - 1);
    for LIndex := 0 to LFields.Count - 1 do
    begin

      if LIndex < LAt then
      begin
        LValues[LIndex] := LFields.Item(LIndex);
      end
      else if LIndex > LAt then
      begin
        LValues[LIndex - 1] := LFields.Item(LIndex);
      end;
    end;
  end
  else
  begin
    NyxTextField(LName);
    LFound := False;
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin

      if CKinds[LKind] = Field(AEditor, rrFieldType).Prop('value') then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxResource.Create('Choose one of the four Pascal scalar families');
    end;
    LField := NyxObject([NyxField('name', NyxData(LName)),
      NyxField('type', NyxData(NyxStateKindName(LKind))),
      NyxField('path', SelectedPath(AEditor, rrFieldPath, CScalars))]);

    if (LAt < 0) and (LFields.Count >= NyxMaximumCollectionFields) then
    begin
      raise ENyxResource.Create('A resource relationship supports at most 64 fields');
    end;
    SetLength(LValues, LFields.Count + Ord(LAt < 0));
    for LIndex := 0 to LFields.Count - 1 do
    begin
      LValues[LIndex] := LFields.Item(LIndex);
    end;

    if LAt < 0 then
    begin
      LAt := LFields.Count;
    end;
    LValues[LAt] := LField;
  end;
  AEditor.SetProp(CFields, NyxArray(LValues).ToJSON);
  Refresh(AEditor);
end;

function HandleNyxResourceRowsEditor(ANode, ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LAction: TNyxResourceRowsAction;
  LData: TNyxDataValue;
begin
  Result := Action(ANode, ARoot, LEditor, LAction) and
    not (LAction in [raApply, raDetach]);

  if not Result then
  begin
    Exit;
  end;
  case LAction of
    raNew:
      begin
        Field(LEditor, rrName).Configure.Value('').Done;
        Field(LEditor, rrReplaceStatic).Configure.Value(False).Done;
        LEditor.SetProp(CFields, NyxArray([]).ToJSON);
        Refresh(LEditor);
      end;
    raLoad: Load(LEditor);
    raDiscover:
      begin
        { Discovery changes choices only. Existing field descriptors remain
          explicit until the user edits/removes them or Apply rejects them. }
        LData := Discover(Payload(LEditor), True);
        LEditor.SetProp(CArrays, LData.ToJSON).SetProp(CScalars, NyxArray([]).ToJSON);
        Refresh(LEditor);
      end;
    raInspect:
      begin
        LData := TNyxResourcePath.FromData(SelectedPath(LEditor, rrDataset, CArrays))
          .Select(Payload(LEditor));

        if LData.Kind <> ndArray then
        begin
          raise ENyxResource.Create('The selected dataset must be an array');
        end;

        if LData.Count = 0 then
        begin
          raise ENyxResource.Create('An empty array has no sample; open a saved recipe to keep its typed paths');
        end;
        LEditor.SetProp(CScalars, Discover(LData.Item(0), False).ToJSON);
        Refresh(LEditor);
      end;
    raSetField, raEditField, raRemoveField: EditFields(LEditor, LAction);
    raApply, raDetach:
      begin
        { Paired authoring owns these actions. }
      end;
  end;
end;

function CaptureNyxResourceRowsEditor(ANode, ARoot: TNyxNode;
  out AChange: TNyxResourceEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LAction: TNyxResourceRowsAction;
  LChoice: TNyxText;
begin
  AChange := Default(TNyxResourceEditorChange);
  Result := Action(ANode, ARoot, LEditor, LAction) and (LAction in [raApply, raDetach]);

  if not Result then
  begin
    Exit;
  end;
  AChange.CatalogBaseline := LEditor.Prop(CCatalog);
  AChange.CollectionBaseline := LEditor.Prop(CCollections);
  AChange.CollectionName := NyxCollection(Field(LEditor, rrName).Prop('value')).Name;
  AChange.RowsData := NyxNull;
  AChange.Operation := reoDetachRows;

  if LAction = raApply then
  begin
    AChange.Operation := reoRows;
    AChange.RowsData := Recipe(LEditor).ToData;
    LChoice := Field(LEditor, rrReplaceStatic).Prop('value');

    if (LChoice <> 'true') and (LChoice <> 'false') then
    begin
      raise ENyxResource.Create('Static conversion requires a Boolean choice');
    end;
    AChange.ReplaceStatic := LChoice = 'true';
  end;
  AChange := TNyxResourceEditorChange.FromData(AChange.ToData);
end;

function NewNyxResourceRowsEditor(const AID: TNyxText; const AResources: INyxResources;
  const ACollections: INyxCollectionDefaults): INyxCard;
var
  LField: TNyxResourceRowsField;
  LAction: TNyxResourceRowsAction;
  LInput: INyxControl;
  LButton: INyxButton;
  LNames: array of TNyxDataValue;
  LIndex: Integer;
begin

  if (AResources = nil) or (ACollections = nil) then
  begin
    raise ENyxResource.Create('Resource row authoring requires copied catalog and collection context');
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(TNyxLayoutPolicy.Column).Gap(10).Done;
  Result.Node.SetProp(CEditor, AID).SetProp(CCatalog, AResources.ToData.ToJSON)
    .SetProp(CCollections, EncodeNyxCollectionDefaults(ACollections))
    .SetProp(CArrays, NyxArray([]).ToJSON).SetProp(CScalars, NyxArray([]).ToJSON)
    .SetProp(CFields, NyxArray([]).ToJSON);
  Result.Add(NewNyxHeading(AID + TNyxText('-heading')).WithText('Resource data collections'));
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).WithText(
    'Map JSON rows to Pascal fields, then choose the named collection in a control binding. '
    + 'Default data or hosted fallback is previewed here; runtime data stays independent.'));
  for LField := Low(TNyxResourceRowsField) to High(TNyxResourceRowsField) do
  begin

    if LField = rrReplaceStatic then
    begin
      LInput := NewNyxCheckbox(NyxResourceRowsFieldID(AID, LField));
      { Existing empty schemas are still authored defaults. Consent applies to
        replacing that definition, not only to deleting nonempty static rows. }
      LInput.Configure.Value(False)
        .Hint('Replace the existing schema and defaults with this resource relationship. Runtime application data stays independent.')
        .Done;
    end
    else if LField in [rrName, rrFieldName] then
    begin
      LInput := NewNyxInput(NyxResourceRowsFieldID(AID, LField));
    end
    else
    begin
      LInput := NewNyxSelect(NyxResourceRowsFieldID(AID, LField));
    end;
    LInput.Configure.Text(CLabels[LField]).AccessibleName(CLabels[LField]).Done;
    LInput.Node.SetProp(CEditor, AID);
    Result.Add(LInput);
  end;
  LNames := nil;
  for LIndex := 0 to ACollections.Count - 1 do
  begin

    if NyxResourceCollections(ACollections).HasSource(ACollections.Key(LIndex)) then
    begin
      SetLength(LNames, Length(LNames) + 1);
      LNames[High(LNames)] := NyxData(ACollections.Key(LIndex).Name);
    end;
  end;
  Choices(Result.Node, rrExisting, NyxArray(LNames));
  LNames := nil;
  for LIndex := 0 to AResources.Count - 1 do
  begin

    if (AResources.Locale(LIndex).Name = '') and
      (AResources.Definition(AResources.Reference(LIndex), NyxDefaultLocale).Kind = nrkJSON) then
    begin
      SetLength(LNames, Length(LNames) + 1);
      LNames[High(LNames)] := NyxData(AResources.Reference(LIndex).Name);
    end;
  end;
  Choices(Result.Node, rrResource, NyxArray(LNames));
  { Family captions are closed choices, unlike JSON-escaped open references. }
  Field(Result.Node, rrFieldType).Configure.Items(CKinds[nskText] + TNyxText(#10) +
    CKinds[nskBoolean] + TNyxText(#10) + CKinds[nskInteger] + TNyxText(#10) + CKinds[nskNumber])
    .Value(CKinds[nskText]).Done;
  Result.Add(NewNyxLabel(AID + TNyxText('-summary')));
  for LAction := Low(TNyxResourceRowsAction) to High(TNyxResourceRowsAction) do
  begin
    LButton := NewNyxButton(NyxResourceRowsActionID(AID, LAction)).WithText(CActionLabels[LAction]);
    LButton.Node.SetProp(CEditor, AID);
    Result.Add(LButton);
  end;
  Refresh(Result.Node);

  if Field(Result.Node, rrExisting).Prop('value') <> '' then
  begin
    Load(Result.Node);
  end;
end;

procedure TNyxResourceRowsDraft.Clear;
begin
  FData := NyxNull;
end;

procedure TNyxResourceRowsDraft.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LValues: array of TNyxDataValue;
  LField: TNyxResourceRowsField;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(AID);

  if LEditor = nil then
  begin
    Exit;
  end;

  if not Complete(LEditor) then
  begin
    Clear;
    Exit;
  end;
  SetLength(LValues, Ord(High(TNyxResourceRowsField)) + 1);
  for LField := Low(TNyxResourceRowsField) to High(TNyxResourceRowsField) do
  begin
    LValues[Ord(LField)] := NyxData(Field(LEditor, LField).Prop('value'));
  end;
  FData := NyxObject([NyxField('editor', NyxData(AID)), NyxField('context', NyxData(Context(LEditor))),
    NyxField('values', NyxArray(LValues)), NyxField('arrays', Data(LEditor, CArrays)),
    NyxField('scalars', Data(LEditor, CScalars)), NyxField('fields', Data(LEditor, CFields))]);
end;

function TNyxResourceRowsDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LField: TNyxResourceRowsField;
  LValues: TNyxDataValue;
begin
  Result := False;

  if not FData.Defined or (FData.Kind = ndNull) or (ARoot = nil) then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(FData.Field('editor').AsText);

  if not Complete(LEditor) or (Context(LEditor) <> FData.Field('context').AsText) then
  begin
    Exit;
  end;
  LEditor.SetProp(CArrays, FData.Field('arrays').ToJSON).SetProp(CScalars, FData.Field('scalars').ToJSON)
    .SetProp(CFields, FData.Field('fields').ToJSON);
  LValues := FData.Field('values');
  for LField := Low(TNyxResourceRowsField) to High(TNyxResourceRowsField) do
  begin

    if LField = rrReplaceStatic then
    begin
      Field(LEditor, LField).Configure.Value(LValues.Item(Ord(LField)).AsText = 'true').Done;
    end
    else
    begin
      Field(LEditor, LField).Configure.Value(LValues.Item(Ord(LField)).AsText).Done;
    end;
  end;
  Refresh(LEditor);
  Result := True;
end;

function TNyxResourceRowsDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if not FData.Defined then
  begin
    Exit;
  end;
  Result := FData.Copy;
end;

class function TNyxResourceRowsDraft.FromData(const AData: TNyxDataValue): TNyxResourceRowsDraft;
var
  LChoices: TNyxDataValue;
  LField: TNyxResourceRowsField;
  LIndex: Integer;
  LPart: Integer;
  LNames: array[0..1] of TNyxText;
begin
  Result := Default(TNyxResourceRowsDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 6) or
    (AData.Field('values').Kind <> ndArray) or
    (AData.Field('values').Count <> Ord(High(TNyxResourceRowsField)) + 1) or
    (AData.Field('fields').Kind <> ndArray) or (AData.Field('fields').Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxResource.Create('Unsupported resource row editor draft');
  end;
  AData.Field('editor').AsText;
  AData.Field('context').AsText;
  { Copied field descriptors are complete even while the surrounding proposal
    is partial. Reuse the typed recipe codec for shape, family/name and path
    admission without resolving any file or requiring a selected identity. }

  if AData.Field('fields').Count > 0 then
  begin
    TNyxResourceRows.FromData(NyxObject([NyxField('version', NyxData(1)),
      NyxField('resource', NyxData('draft')), NyxField('path', NyxArray([])),
      NyxField('identity', NyxArray([])), NyxField('fields', AData.Field('fields'))]));
  end;
  for LField := Low(TNyxResourceRowsField) to High(TNyxResourceRowsField) do
  begin
    AData.Field('values').Item(Ord(LField)).AsText;
  end;
  LNames[0] := 'arrays';
  LNames[1] := 'scalars';
  for LPart := 0 to High(LNames) do
  begin
    LChoices := AData.Field(LNames[LPart]);

    if (LChoices.Kind <> ndArray) or (LChoices.Count > 256) then
    begin
      raise ENyxResource.Create('Resource row discovery exceeds the copied draft budget');
    end;
    for LIndex := 0 to LChoices.Count - 1 do
    begin
      TNyxResourcePath.FromData(LChoices.Item(LIndex));
    end;
  end;
  { Complete recipes are admitted at Apply. The draft may intentionally have
    zero fields, no selected identity, or a partially entered application name. }
  Result.FData := AData.Copy;
end;

end.
