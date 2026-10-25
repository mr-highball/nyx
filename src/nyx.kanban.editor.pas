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

unit nyx.kanban.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.kanban,
  nyx.kanban.intent, nyx.collections.view.types;

type
  { Stable public form parts, independent of captions and application field names. }
  TNyxKanbanEditorField = (nkfCollection, nkfTitleField, nkfLaneField, nkfScope,
    nkfLane0, nkfCaption0, nkfLane1, nkfCaption1, nkfLane2, nkfCaption2,
    nkfApply, nkfDisable, nkfInherit);

  { Exact authored target. Instance is absent for a concrete board; otherwise
    Path addresses the board within that reference. Owner remains the selected
    authored node/descriptor, never a reverse-parsed realized identity. }
  TNyxKanbanEditorTarget = record
  private
    FOwner: TNyxControlRef;
    FInstance: TNyxControlRef;
    FPath: TNyxPartRef;
  public
    function ToData: TNyxDataValue;
    property Owner: TNyxControlRef read FOwner;
    property Instance: TNyxControlRef read FInstance;
    property Path: TNyxPartRef read FPath;
  end;

  { Copied whole-board intent. Undefined Intent explicitly means Inherit and
    serializes as null; it is never passed to the declared-intent serializer.
    The host validates this value before one atomic paired design/source patch. }
  TNyxKanbanEditorChange = record
  private
    FTarget: TNyxKanbanEditorTarget;
    FIntent: TNyxKanbanIntent;
    FBaseline: TNyxText;
  public
    function Copy: TNyxKanbanEditorChange;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxKanbanEditorChange; static;
    property Target: TNyxKanbanEditorTarget read FTarget;
    property Intent: TNyxKanbanIntent read FIntent;
    property Baseline: TNyxText read FBaseline;
  end;

  { Immutable copied form inputs. Capture creates a fresh vector; Restore never
    mutates that vector, so record copies remain independent on both targets.
    Missing panels park drafts; changed owner/schema/parts retire them. }
  TNyxKanbanEditorDraft = record
  private
    FEditor: TNyxText;
    FOwner: TNyxText;
    FBaseline: TNyxText;
    FValues: array of TNyxText;
  public
    procedure Clear;
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
  end;

function NyxKanbanEditorTarget(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxKanbanEditorTarget;
{ Conservative authored-tree/schema baseline includes effective roles, local
  descriptors and occupied IDs. Collection rows/revisions are deliberately absent. }
function NyxKanbanEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
function NyxKanbanEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  const AIntent: TNyxKanbanIntent): TNyxKanbanEditorChange;
procedure ValidateNyxKanbanEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxKanbanEditorChange);
{ Pure required-role rewrite. A prior single text column retains its caption,
  sizing, explicit typeahead and paging. Behavioral key/field/scope/query change
  together; incompatible column shapes refuse rather than discard metadata. }
function NyxKanbanEditorLaneSpec(const AOptions: TNyxKanbanOptions;
  AIndex: Integer; const APrevious: TNyxCollectionViewSpec): TNyxCollectionViewSpec;
function NyxKanbanEditorFieldID(const AID: TNyxText;
  AField: TNyxKanbanEditorField): TNyxText;
{ Borrows doc/owner only while constructing an ordinary Card/Select/Input/Button
  form. Empty collections produce helpful inert Apply controls. Display choices
  carry copied exact identities separately; no document or controller survives. }
function NewNyxKanbanEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
{ Unrelated controls return False. Current mounted membership, typed fields,
  exact canonical target and baseline precede returning a copied change. }
function CaptureNyxKanbanEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxKanbanEditorChange): Boolean;

implementation

uses
  SysUtils, nyx.codec, nyx.composition, nyx.collections, nyx.collections.registry,
  nyx.collections.codec, nyx.collections.query, nyx.collections.selection,
  nyx.state, nyx.menu.editor;

const
  CEditor = 'nyx.kanban-editor';
  CVersion = 'nyx.kanban-editor.version';
  COwner = 'nyx.kanban-editor.owner';
  CBaseline = 'nyx.kanban-editor.baseline';
  CCollections = 'nyx.kanban-editor.collections';
  CFields = 'nyx.kanban-editor.fields';
  CFormVersion = '1';
  CChoose = 'Choose a value';
  CInstance = 'Independent per reusable instance';
  CApplication = 'Shared application collection';
  CNames: array[TNyxKanbanEditorField] of TNyxText =
    ('collection', 'title-field', 'lane-field', 'scope',
     'lane-0', 'caption-0', 'lane-1', 'caption-1', 'lane-2', 'caption-2',
     'apply', 'disable', 'inherit');
  CLabels: array[TNyxKanbanEditorField] of TNyxText =
    ('Collection', 'Card title field', 'Lane field', 'Runtime data',
     'Lane 1 value', 'Lane 1 label', 'Lane 2 value', 'Lane 2 label',
     'Lane 3 value', 'Lane 3 label', 'Apply board configuration',
     'Disable automatic movement', 'Use inherited configuration');

procedure ExactObject(const AData: TNyxDataValue; const AKeys: TNyxText;
  ACount: Integer);
var
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> ACount) then
  begin
    raise ENyxKanban.Create('Kanban editor value requires its exact fields');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if Pos('|' + AData.Key(LIndex) + '|', AKeys) = 0 then
    begin
      raise ENyxKanban.Create('Unknown Kanban editor value field');
    end;
  end;
end;

function IntentData(const AIntent: TNyxKanbanIntent): TNyxDataValue;
begin
  Result := NyxNull;

  if AIntent.Mode <> nkiUndefined then
  begin
    Result := AIntent.ToData;
  end;
end;

function TargetFromData(const AData: TNyxDataValue): TNyxKanbanEditorTarget;
begin
  ExactObject(AData, '|owner|instance|path|', 3);
  Result := Default(TNyxKanbanEditorTarget);
  Result.FOwner := NyxControl(AData.Field('owner').AsText);

  if AData.Field('instance').Kind <> ndNull then
  begin
    Result.FInstance := NyxControl(AData.Field('instance').AsText);
  end;
  Result.FPath := NyxPart(AData.Field('path').AsText);

  if (Result.FInstance.ID = '') and (Result.FPath.Name <> '.') then
  begin
    raise ENyxKanban.Create('A concrete board has no reusable part path');
  end;
  Result.ToData;
end;

function TNyxKanbanEditorTarget.ToData: TNyxDataValue;
var
  LInstance: TNyxDataValue;
begin

  if (FOwner.ID = '') or (FPath.Name = '') or
    (FPath.Name[1] = '/') or (FPath.Name[Length(FPath.Name)] = '/') or
    (Pos('//', FPath.Name) > 0) or
    ((FInstance.ID = '') and (FPath.Name <> '.')) then
  begin
    raise ENyxKanban.Create('Kanban editor target requires its authored identity and path');
  end;
  LInstance := NyxNull;

  if FInstance.ID <> '' then
  begin
    LInstance := NyxData(FInstance.ID);
  end;
  Result := NyxObject([NyxField('owner', NyxData(FOwner.ID)),
    NyxField('instance', LInstance), NyxField('path', NyxData(FPath.Name))]);
end;

function TNyxKanbanEditorChange.Copy: TNyxKanbanEditorChange;
begin
  Result := Self;
  Result.FIntent := FIntent.Copy;
end;

function TNyxKanbanEditorChange.ToData: TNyxDataValue;
begin

  if FBaseline = '' then
  begin
    raise ENyxKanban.Create('Kanban editor change requires its exact baseline');
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('target', FTarget.ToData), NyxField('baseline', NyxData(FBaseline)),
    NyxField('intent', IntentData(FIntent))]);
end;

class function TNyxKanbanEditorChange.FromData(
  const AData: TNyxDataValue): TNyxKanbanEditorChange;
begin
  ExactObject(AData, '|version|target|baseline|intent|', 4);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxKanban.Create('Unknown Kanban editor change version');
  end;
  Result := Default(TNyxKanbanEditorChange);
  Result.FTarget := TargetFromData(AData.Field('target'));
  Result.FBaseline := AData.Field('baseline').AsText;

  if AData.Field('intent').Kind <> ndNull then
  begin
    Result.FIntent := TNyxKanbanIntent.FromData(AData.Field('intent'));
  end;
  Result.ToData;
end;

function ResolveTarget(ADocument: TNyxDocument; const AOwner: TNyxControlRef;
  out AContext, AProjection: TNyxNode): TNyxKanbanEditorTarget;
var
  LOwner: TNyxNode;
  LIndex: Integer;
  LPath: TNyxPartRef;
begin
  AContext := nil;
  AProjection := nil;
  Result := Default(TNyxKanbanEditorTarget);

  if ADocument = nil then
  begin
    raise ENyxKanban.Create('Kanban editor requires its document');
  end;
  LOwner := ADocument.FindControl(AOwner);

  if LOwner = nil then
  begin
    raise ENyxKanban.Create('Kanban editor owner is missing');
  end;
  Result.FOwner := NyxControl(LOwner.ID);
  Result.FPath := NyxPart('.');

  if LOwner.Kind = NyxKindName(nkSlotOverride) then
  begin

    if (LOwner.Parent = nil) or
      (LOwner.Parent.ProjectionKind <> NyxKindName(nkComponent)) or
      (LOwner.StoredProp(NyxAttributeName(atOverrideMode)) =
        NyxOverrideName(noRemove)) then
    begin
      raise ENyxKanban.Create('Kanban editor requires an available named-part descriptor');
    end;
    LPath := NyxPart(LOwner.StoredProp('path'));
    for LIndex := 0 to LOwner.Parent.Count - 1 do
    begin

      if (LOwner.Parent.Children[LIndex] <> LOwner) and
        (LOwner.Parent.Children[LIndex].Kind = NyxKindName(nkSlotOverride)) and
        (LOwner.Parent.Children[LIndex].StoredProp('path') = LPath.Name) then
      begin
        raise ENyxKanban.Create('Kanban editor named-part descriptor is ambiguous');
      end;
    end;
    Result.FInstance := NyxControl(LOwner.Parent.ID);
    Result.FPath := LPath;
  end
  else if LOwner.ProjectionKind = NyxKindName(nkComponent) then
  begin
    Result.FInstance := NyxControl(LOwner.ID);
  end;
  AContext := RealizeNyxContext(ADocument, LOwner, AProjection);
  try

    if (AProjection = nil) or
      (AProjection.Kind <> NyxKindName(nkKanbanBoard)) then
    begin
      raise ENyxKanban.Create('This authored selection is not a Kanban board');
    end;
  except
    FreeAndNil(AContext);
    AProjection := nil;
    raise;
  end;
end;

function NyxKanbanEditorTarget(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxKanbanEditorTarget;
var
  LContext: TNyxNode;
  LProjection: TNyxNode;
begin
  LContext := nil;
  try
    Result := ResolveTarget(ADocument, AOwner, LContext, LProjection);
  finally
    LContext.Free;
  end;
end;

function SchemaDefaults(ADocument: TNyxDocument): TNyxText;
var
  LDefaults: INyxCollectionDefaults;
  LSources: INyxResourceCollectionDefaults;
  LKey: TNyxCollectionRef;
  LIndex: Integer;
begin
  LDefaults := NewNyxCollectionDefaults;
  for LIndex := 0 to ADocument.Collections.Count - 1 do
  begin
    LKey := ADocument.Collections.Key(LIndex);
    LDefaults.Define(LKey, ADocument.Collections.Snapshot(LKey).Schema, []);
  end;
  { Resource source declarations are metadata, not materialized row contents. }
  LSources := NyxResourceCollections(LDefaults);
  for LIndex := 0 to ADocument.Collections.Count - 1 do
  begin
    LKey := ADocument.Collections.Key(LIndex);

    if NyxResourceCollections(ADocument.Collections).HasSource(LKey) then
    begin
      LSources.Define(LKey, NyxResourceCollections(ADocument.Collections).Source(LKey));
    end;
  end;
  Result := EncodeNyxCollectionDefaults(LDefaults);
end;

function NyxKanbanEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LTarget: TNyxKanbanEditorTarget;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LRoots: array of TNyxDataValue;
  LIndex: Integer;
begin
  LContext := nil;
  try
    LTarget := ResolveTarget(ADocument, AOwner, LContext, LProjection);
    SetLength(LRoots, ADocument.Count + ADocument.ComponentCount);
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      LRoots[LIndex] := TNyxCodec.NodeSnapshot(ADocument.Pages[LIndex]);
    end;
    for LIndex := 0 to ADocument.ComponentCount - 1 do
    begin
      LRoots[ADocument.Count + LIndex] :=
        TNyxCodec.NodeSnapshot(ADocument.Components[LIndex]);
    end;
    Result := NyxObject([NyxField('target', LTarget.ToData),
      NyxField('roots', NyxArray(LRoots)),
      NyxField('effective', TNyxCodec.NodeSnapshot(LProjection)),
      NyxField('schemas', NyxData(SchemaDefaults(ADocument)))]).ToJSON;
  finally
    LContext.Free;
  end;
end;

function NyxKanbanEditorLaneSpec(const AOptions: TNyxKanbanOptions;
  AIndex: Integer; const APrevious: TNyxCollectionViewSpec): TNyxCollectionViewSpec;
var
  LOptions: TNyxKanbanOptions;
  LPrevious: TNyxCollectionViewSpec;
  LColumn: TNyxCollectionColumn;
  LCaption: TNyxText;
begin
  LOptions := AOptions.Copy;
  LOptions.Validate;
  LPrevious := APrevious.Copy;
  LCaption := 'Card';

  if (AIndex < 0) or (AIndex >= LOptions.Count) then
  begin
    raise ENyxKanban.Create('Kanban lane index is outside the current recipe');
  end;

  if LPrevious.Defined then
  begin
    LPrevious.Validate;

    if (LPrevious.Count <> 1) or (LPrevious.ColumnAt(0).Kind <> nskText) then
    begin
      raise ENyxKanban.Create('Reconfigure a single text-column lane without losing other columns');
    end;
    LColumn := LPrevious.ColumnAt(0);
    LCaption := LColumn.Title;
  end;
  Result := NyxCollectionView(LOptions.Collection).Column(LOptions.TitleField,
    LCaption, cmReadOnly).Scoped(LOptions.Scope).Selection(nsmSingle).
    Query(NyxCollectionQuery.Where(NyxWhere(LOptions.LaneField).
      EqualTo(LOptions.LaneAt(AIndex).Name, nqtExact)));

  if LPrevious.Defined then
  begin

    if LColumn.Sizing.Defined then
    begin
      Result := Result.ColumnSizing(LColumn.Sizing);
    end;

    if LColumn.HasFormatPolicy then
    begin
      Result := Result.ColumnFormat(LColumn.FormatPolicy);
    end;

    { Preserve declarations, including explicit opt-outs. Resolved defaults
      must remain absent so older bindings do not acquire version-7 metadata
      merely because the author reconfigures a lane's collection or title. }

    if LColumn.HasQueryPolicy then
    begin
      Result := Result.ColumnQuery(LColumn.QueryPolicy);
    end;

    if LPrevious.HasTableQueryPolicy then
    begin
      Result := Result.TableQuery(LPrevious.TableQueryPolicy);
    end;

    if LPrevious.HasTypeAhead then
    begin
      Result := Result.TypeAhead(LPrevious.TypeAheadPolicy);
    end;

    if LPrevious.PagingPolicy.Defined then
    begin
      Result := Result.Paged(LPrevious.PagingPolicy);
    end;
  end;
end;

function NyxKanbanEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  const AIntent: TNyxKanbanIntent): TNyxKanbanEditorChange;
var
  LCandidate: TNyxKanbanEditorChange;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LOptions: TNyxKanbanOptions;
  LIndex: Integer;
  LItems: TNyxNode;
begin
  LCandidate := Default(TNyxKanbanEditorChange);
  LCandidate.FIntent := AIntent.Copy;
  LContext := nil;
  try
    LCandidate.FTarget := ResolveTarget(ADocument, AOwner, LContext, LProjection);

    if AIntent.Mode = nkiUndefined then
    begin

      if not HasNyxKanbanIntent(ADocument.FindControl(AOwner)) then
      begin
        raise ENyxKanban.Create('There is no local Kanban declaration to inherit');
      end;
    end
    else
    begin
      LCandidate.FIntent.Validate;

      if LCandidate.FIntent.Mode = nkiConfigured then
      begin
        LOptions := LCandidate.FIntent.Options;

        if not ADocument.Collections.Has(LOptions.Collection) then
        begin
          raise ENyxKanban.Create('Choose an existing authored collection');
        end;
        ADocument.Collections.Snapshot(LOptions.Collection).Schema.
          Definition(LOptions.TitleField);
        ADocument.Collections.Snapshot(LOptions.Collection).Schema.
          Definition(LOptions.LaneField);
        for LIndex := 0 to LOptions.Count - 1 do
        begin
          LItems := LProjection.Part(NyxPart('lane-' + IntToStr(LIndex) + '/items'));

          if LItems.ProjectionKind <> NyxKindName(nkList) then
          begin
            raise ENyxKanban.Create('A required Kanban list part is unavailable');
          end;
          NyxKanbanEditorLaneSpec(LOptions, LIndex, LItems.CollectionView);
        end;
      end;
    end;
    LCandidate.FBaseline := NyxKanbanEditorBaseline(ADocument, AOwner);
    LCandidate.ToData;
    Result := LCandidate;
  finally
    LContext.Free;
  end;
end;

procedure ValidateNyxKanbanEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxKanbanEditorChange);
var
  LExpected: TNyxKanbanEditorChange;
begin
  LExpected := NyxKanbanEditorChange(ADocument, AChange.Target.Owner, AChange.Intent);

  if (LExpected.Target.ToData.ToJSON <> AChange.Target.ToData.ToJSON) or
    (LExpected.Baseline <> AChange.Baseline) then
  begin
    raise ENyxKanban.Create('The Kanban owner, schema or parts changed; reopen this form');
  end;
end;

function NyxKanbanEditorFieldID(const AID: TNyxText;
  AField: TNyxKanbanEditorField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxKanbanEditorField))) or
    (Ord(AField) > Ord(High(TNyxKanbanEditorField))) then
  begin
    raise ENyxKanban.Create('Unknown Kanban editor field');
  end;
  Result := AID + '-' + CNames[AField];
end;

function FieldNode(AEditor: TNyxNode; AField: TNyxKanbanEditorField): TNyxNode;
var
  LKind: TNyxKind;
begin
  Result := AEditor.Part(NyxPart(CNames[AField]));
  LKind := nkInput;

  if AField <= nkfScope then
  begin
    LKind := nkSelect;
  end
  else if AField >= nkfApply then
  begin
    LKind := nkButton;
  end;

  if (Result.ProjectionKind <> NyxKindName(LKind)) or
    (Result.SourceID <> NyxKanbanEditorFieldID(AEditor.SourceID, AField)) then
  begin
    raise ENyxKanban.Create('The mounted Kanban form has a missing or changed field');
  end;
end;

function ChoiceItems(const AValues: TNyxDataValue): TNyxText;
var
  LIndex: Integer;
begin
  Result := CChoose;
  for LIndex := 0 to AValues.Count - 1 do
  begin
    Result := Result + #10 + AValues.Item(LIndex).Field('caption').AsText;
  end;
end;

function FindChoice(const AValues: TNyxDataValue;
  const AValue: TNyxText; const ACollection: TNyxText = ''): TNyxText;
var
  LIndex: Integer;
begin
  Result := CChoose;
  for LIndex := 0 to AValues.Count - 1 do
  begin

    if (AValues.Item(LIndex).Field('name').AsText = AValue) and
      ((ACollection = '') or
      (AValues.Item(LIndex).Field('collection').AsText = ACollection)) then
    begin
      Exit(AValues.Item(LIndex).Field('caption').AsText);
    end;
  end;
end;

function ReadChoice(const AValues: TNyxDataValue;
  const ACaption: TNyxText; const ACollection: TNyxText = ''): TNyxText;
var
  LIndex: Integer;
begin
  for LIndex := 0 to AValues.Count - 1 do
  begin

    if AValues.Item(LIndex).Field('caption').AsText = ACaption then
    begin

      if (ACollection <> '') and
        (AValues.Item(LIndex).Field('collection').AsText <> ACollection) then
      begin
        raise ENyxKanban.Create('Choose fields belonging to this collection');
      end;
      Exit(AValues.Item(LIndex).Field('name').AsText);
    end;
  end;
  raise ENyxKanban.Create('Choose an exact available collection or text field');
end;

function NewNyxKanbanEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
var
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LTarget: TNyxKanbanEditorTarget;
  LBaseline: TNyxText;
  LIntent: TNyxKanbanIntent;
  LOptions: TNyxKanbanOptions;
  LCollections: array of TNyxDataValue;
  LFields: array of TNyxDataValue;
  LCollectionData: TNyxDataValue;
  LFieldData: TNyxDataValue;
  LKey: TNyxCollectionRef;
  LSchema: TNyxCollectionSchema;
  LDefinition: TNyxCollectionField;
  LValues: array[nkfCollection..nkfCaption2] of TNyxText;
  LControl: INyxControl;
  LIndex: Integer;
  LFieldIndex: Integer;
  LCount: Integer;
  LTextCount: Integer;
  LEligible: Boolean;
  LCanApply: Boolean;
  LField: TNyxKanbanEditorField;
begin
  LContext := nil;
  try
    LTarget := ResolveTarget(ADocument, AOwner, LContext, LProjection);
    LBaseline := NyxKanbanEditorBaseline(ADocument, AOwner);
    LIntent := GetNyxKanbanIntent(LProjection);
    SetLength(LCollections, ADocument.Collections.Count);
    LFields := nil;
    LCanApply := False;
    for LIndex := 0 to ADocument.Collections.Count - 1 do
    begin
      LKey := ADocument.Collections.Key(LIndex);
      LSchema := ADocument.Collections.Snapshot(LKey).Schema;
      LTextCount := 0;
      for LFieldIndex := 0 to LSchema.Count - 1 do
      begin
        LDefinition := LSchema.FieldAt(LFieldIndex);

        if LDefinition.Kind = nskText then
        begin
          Inc(LTextCount);
          LCount := Length(LFields);
          SetLength(LFields, LCount + 1);
          LFields[LCount] := NyxObject([
            NyxField('name', NyxData(LDefinition.Name)),
            NyxField('collection', NyxData(LKey.Name)),
            NyxField('caption', NyxData(NyxMenuEditorChoiceCaption(LCount,
              LKey.Name + ' / ' + LDefinition.Name)))]);
        end;
      end;
      LEligible := LTextCount >= 2;
      LCanApply := LCanApply or LEligible;
      LCollections[LIndex] := NyxObject([NyxField('name', NyxData(LKey.Name)),
        NyxField('caption', NyxData(NyxMenuEditorChoiceCaption(LIndex, LKey.Name)))]);
    end;
    LCollectionData := NyxArray(LCollections);
    LFieldData := NyxArray(LFields);
    for LField := nkfCollection to nkfCaption2 do
    begin
      LValues[LField] := '';
    end;
    LValues[nkfCollection] := CChoose;
    LValues[nkfTitleField] := CChoose;
    LValues[nkfLaneField] := CChoose;
    LValues[nkfScope] := CInstance;

    if LIntent.Mode = nkiConfigured then
    begin
      LOptions := LIntent.Options;
      LValues[nkfCollection] := FindChoice(LCollectionData, LOptions.Collection.Name);
      LValues[nkfTitleField] := FindChoice(LFieldData, LOptions.TitleField.Name,
        LOptions.Collection.Name);
      LValues[nkfLaneField] := FindChoice(LFieldData, LOptions.LaneField.Name,
        LOptions.Collection.Name);

      if LOptions.Scope = csApplication then
      begin
        LValues[nkfScope] := CApplication;
      end;
      for LIndex := 0 to 2 do
      begin
        LValues[TNyxKanbanEditorField(Ord(nkfLane0) + LIndex * 2)] :=
          LOptions.LaneAt(LIndex).Name;
        LValues[TNyxKanbanEditorField(Ord(nkfCaption0) + LIndex * 2)] :=
          LOptions.CaptionAt(LIndex);
      end;
    end;
    Result := NewNyxCard(AID);
    Result.Configure.Gap(10).Padding(16).Done;
    Result.Node.SetProp(CVersion, CFormVersion).SetProp(COwner, LTarget.Owner.ID).
      SetProp(CBaseline, LBaseline).SetProp(CCollections, LCollectionData.ToJSON).
      SetProp(CFields, LFieldData.ToJSON);
    Result.Add(NewNyxLabel(AID + '-title').Configure.Text('Kanban movement').Done);
    Result.Add(NewNyxLabel(AID + '-help').Configure.Text(
      'Apply all three lane bindings together. Existing headings and action customizations stay. ' +
      'Disable keeps the board static; Inherit restores the parent configuration.').Done);

    if not LCanApply then
    begin
      Result.Add(NewNyxLabel(AID + '-empty').Configure.Text(
        'Create a collection with distinct text fields for card titles ' +
        'and lane values first.').Done);
    end;
    for LField := nkfCollection to nkfInherit do
    begin

      if LField <= nkfScope then
      begin
        LControl := NewNyxSelect(NyxKanbanEditorFieldID(AID, LField));

        if LField = nkfCollection then
        begin
          LControl.Configure.Items(ChoiceItems(LCollectionData)).Done;
        end
        else if LField = nkfScope then
        begin
          LControl.Configure.Items(CInstance + #10 + CApplication).Done;
        end
        else
        begin
          LControl.Configure.Items(ChoiceItems(LFieldData)).Done;
        end;
      end
      else if LField >= nkfApply then
      begin
        LControl := NewNyxButton(NyxKanbanEditorFieldID(AID, LField));
        LControl.Node.SetProp(CEditor, AID);

        if LField = nkfApply then
        begin
          LControl.Configure.Enabled(LCanApply).Done;
        end
        else if LField = nkfInherit then
        begin
          LControl.Configure.Enabled(HasNyxKanbanIntent(
            ADocument.FindControl(AOwner))).Done;
        end;
      end
      else
      begin
        LControl := NewNyxInput(NyxKanbanEditorFieldID(AID, LField));
      end;
      LControl.Configure.PartName(NyxPart(CNames[LField])).Text(CLabels[LField]).
        AccessibleName(CLabels[LField]).Done;

      if LField < nkfApply then
      begin
        LControl.Configure.Value(LValues[LField]).Done;
      end;
      Result.Add(LControl);
    end;
  finally
    LContext.Free;
  end;
end;

function CaptureNyxKanbanEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxKanbanEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LForm: TNyxText;
  LOwner: TNyxControlRef;
  LCollections: TNyxDataValue;
  LFields: TNyxDataValue;
  LKey: TNyxCollectionRef;
  LOptions: TNyxKanbanOptions;
  LIntent: TNyxKanbanIntent;
  LCollection: TNyxText;
  LScope: TNyxText;
  LIndex: Integer;
  LAction: TNyxKanbanEditorField;
begin
  Result := False;
  AChange := Default(TNyxKanbanEditorChange);

  if (AButton = nil) or (AButton.StoredProp(CEditor) = '') then
  begin
    Exit;
  end;

  if (AShellRoot = nil) or (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxKanban.Create('This Kanban form belongs to a retired view');
  end;
  LForm := AButton.StoredProp(CEditor);
  LEditor := AButton.Parent;
  while (LEditor <> nil) and (LEditor.SourceID <> LForm) do
  begin
    LEditor := LEditor.Parent;
  end;

  if (LEditor = nil) or (LEditor.StoredProp(CVersion) <> CFormVersion) or
    (LEditor.ProjectionKind <> NyxKindName(nkCard)) then
  begin
    raise ENyxKanban.Create('The Kanban form is missing or malformed');
  end;
  LOwner := NyxControl(LEditor.StoredProp(COwner));

  if LEditor.StoredProp(CBaseline) <> NyxKanbanEditorBaseline(ADocument, LOwner) then
  begin
    raise ENyxKanban.Create('The Kanban configuration changed; reopen the form');
  end;
  LAction := nkfApply;

  if AButton = FieldNode(LEditor, nkfDisable) then
  begin
    LAction := nkfDisable;
  end
  else if AButton = FieldNode(LEditor, nkfInherit) then
  begin
    LAction := nkfInherit;
  end
  else if AButton <> FieldNode(LEditor, nkfApply) then
  begin
    raise ENyxKanban.Create('Unknown Kanban form action');
  end;

  if AButton.Prop(NyxAttributeName(atEnabled), 'true') <> 'true' then
  begin
    raise ENyxKanban.Create('This Kanban form action is unavailable');
  end;
  LIntent := Default(TNyxKanbanIntent);

  if LAction = nkfDisable then
  begin
    LIntent := TNyxKanbanIntent.Disabled;
  end
  else if LAction = nkfApply then
  begin
    LCollections := TNyxDataValue.ParseJSON(LEditor.StoredProp(CCollections));
    LFields := TNyxDataValue.ParseJSON(LEditor.StoredProp(CFields));
    LCollection := ReadChoice(LCollections,
      FieldNode(LEditor, nkfCollection).Prop(NyxAttributeName(atValue)));
    LKey := NyxCollection(LCollection);
    LOptions := NyxKanbanOptions(LKey,
      NyxTextField(ReadChoice(LFields,
        FieldNode(LEditor, nkfTitleField).Prop(NyxAttributeName(atValue)), LCollection)),
      NyxTextField(ReadChoice(LFields,
        FieldNode(LEditor, nkfLaneField).Prop(NyxAttributeName(atValue)), LCollection)));
    LScope := FieldNode(LEditor, nkfScope).Prop(NyxAttributeName(atValue));

    if LScope = CApplication then
    begin
      LOptions := LOptions.Scoped(csApplication);
    end
    else if LScope <> CInstance then
    begin
      raise ENyxKanban.Create('Choose an exact runtime collection scope');
    end;
    for LIndex := 0 to 2 do
    begin
      LOptions := LOptions.Lane(NyxKanbanLane(FieldNode(LEditor,
        TNyxKanbanEditorField(Ord(nkfLane0) + LIndex * 2)).
        Prop(NyxAttributeName(atValue))), FieldNode(LEditor,
        TNyxKanbanEditorField(Ord(nkfCaption0) + LIndex * 2)).
        Prop(NyxAttributeName(atValue)));
    end;
    LIntent := TNyxKanbanIntent.Configured(LOptions);
  end;
  AChange := NyxKanbanEditorChange(ADocument, LOwner, LIntent);
  Result := True;
end;

procedure TNyxKanbanEditorDraft.Clear;
begin
  FEditor := '';
  FOwner := '';
  FBaseline := '';
  FValues := nil;
end;

procedure TNyxKanbanEditorDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LNext: TNyxKanbanEditorDraft;
  LField: TNyxKanbanEditorField;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(AEditor);

  if LEditor = nil then
  begin
    Exit;
  end;

  if LEditor.StoredProp(CVersion) <> CFormVersion then
  begin
    raise ENyxKanban.Create('Cannot park a malformed Kanban form');
  end;
  LNext := Default(TNyxKanbanEditorDraft);
  LNext.FEditor := AEditor;
  LNext.FOwner := LEditor.StoredProp(COwner);
  LNext.FBaseline := LEditor.StoredProp(CBaseline);
  SetLength(LNext.FValues, Ord(nkfApply));
  for LField := nkfCollection to nkfCaption2 do
  begin
    LNext.FValues[Ord(LField)] := FieldNode(LEditor, LField).
      Prop(NyxAttributeName(atValue));
  end;
  Self := LNext;
end;

function TNyxKanbanEditorDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LField: TNyxKanbanEditorField;
begin
  Result := False;

  if (ARoot = nil) or (FEditor = '') then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(FEditor);

  if LEditor = nil then
  begin
    Exit;
  end;

  if (LEditor.StoredProp(CVersion) <> CFormVersion) or
    (LEditor.StoredProp(COwner) <> FOwner) or
    (LEditor.StoredProp(CBaseline) <> FBaseline) then
  begin
    Clear;
    Exit;
  end;

  if Length(FValues) <> Ord(nkfApply) then
  begin
    raise ENyxKanban.Create('A parked Kanban draft has an incomplete value vector');
  end;
  for LField := nkfCollection to nkfCaption2 do
  begin
    FieldNode(LEditor, LField);
  end;
  for LField := nkfCollection to nkfCaption2 do
  begin
    FieldNode(LEditor, LField).SetProp(NyxAttributeName(atValue),
      FValues[Ord(LField)]);
  end;
  Result := True;
end;

end.
