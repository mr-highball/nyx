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
unit nyx.kanban.intent;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.model, nyx.controls, nyx.kanban;

const
  { A known optional node extension. Absence leaves legacy static boards and
    explicit manual attachments unchanged; no model version or slot is added. }
  NyxKanbanIntentKey = 'nyx.kanban';

type
  { Undefined is local absence/inheritance, not an implicit enabled default.
    Disabled is an authored mask which survives reusable whole-key overlays. }
  TNyxKanbanIntentMode = (nkiUndefined, nkiDisabled, nkiConfigured);

  { Copied portable automatic-movement intent, with no store, event router,
    controls or subscription. Configured replaces the complete options value.
    Cosmetic part captions, styles and payloads remain independently authored. }
  TNyxKanbanIntent = record
  private
    FMode: TNyxKanbanIntentMode;
    FOptions: TNyxKanbanOptions;
    function GetOptions: TNyxKanbanOptions;
  public
    class function Disabled: TNyxKanbanIntent; static;
    class function Configured(const AOptions: TNyxKanbanOptions): TNyxKanbanIntent; static;
    function Copy: TNyxKanbanIntent;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxKanbanIntent; static;
    property Mode: TNyxKanbanIntentMode read FMode;
    property Options: TNyxKanbanOptions read GetOptions;
  end;

function HasNyxKanbanIntent(ANode: TNyxNode): Boolean;
function GetNyxKanbanIntent(ANode: TNyxNode): TNyxKanbanIntent;
{ Set declares intent only. It never reconstructs children or duplicates the
  Move buttons already owned by ConfigureNyxKanbanBoard or a customized recipe. }
procedure SetNyxKanbanIntent(const AControl: INyxControl;
  const AIntent: TNyxKanbanIntent);
{ Removing a local value reveals inherited intent; disabling writes a mask. }
procedure InheritNyxKanbanIntent(const AControl: INyxControl);
{ A reusable reference validates its local metadata now; complete document
  admission checks its expanded board and all behavioral part roles later. }
procedure ValidateNyxKanbanIntent(ANode: TNyxNode);

implementation

uses
  SysUtils, nyx.types, nyx.collections, nyx.collections.view.types, nyx.collections.query,
  nyx.collections.selection, nyx.state;

const
  CConfigured = 'configured';
  CDisabled = 'disabled';
  CApplication = 'application';
  CInstance = 'instance';

class function TNyxKanbanIntent.Disabled: TNyxKanbanIntent;
begin
  Result := Default(TNyxKanbanIntent);
  Result.FMode := nkiDisabled;
end;

class function TNyxKanbanIntent.Configured(
  const AOptions: TNyxKanbanOptions): TNyxKanbanIntent;
begin
  AOptions.Validate;
  Result := Default(TNyxKanbanIntent);
  Result.FMode := nkiConfigured;
  Result.FOptions := AOptions.Copy;
end;

function TNyxKanbanIntent.Copy: TNyxKanbanIntent;
begin
  Result := Default(TNyxKanbanIntent);
  Result.FMode := FMode;
  Result.FOptions := FOptions.Copy;
end;

function TNyxKanbanIntent.GetOptions: TNyxKanbanOptions;
begin

  if FMode <> nkiConfigured then
  begin
    raise ENyxKanban.Create('Only configured Kanban intent has movement options');
  end;
  Result := FOptions.Copy;
end;

procedure TNyxKanbanIntent.Validate;
begin
  case FMode of
    nkiDisabled: ;
    nkiConfigured: FOptions.Validate;
  else
    raise ENyxKanban.Create('Use InheritNyxKanbanIntent to remove local intent');
  end;
end;

function TNyxKanbanIntent.ToData: TNyxDataValue;
var
  LLanes: array of TNyxDataValue;
  LScope: TNyxText;
  LIndex: Integer;
begin
  Validate;

  if FMode = nkiDisabled then
  begin
    Exit(NyxObject([NyxField('version', NyxData(1)),
      NyxField('mode', NyxData(CDisabled))]));
  end;
  LScope := CInstance;

  if FOptions.Scope = csApplication then
  begin
    LScope := CApplication;
  end;
  SetLength(LLanes, FOptions.Count);
  for LIndex := 0 to High(LLanes) do
  begin
    LLanes[LIndex] := NyxObject([
      NyxField('name', NyxData(FOptions.LaneAt(LIndex).Name)),
      NyxField('caption', NyxData(FOptions.CaptionAt(LIndex)))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('mode', NyxData(CConfigured)),
    NyxField('collection', NyxData(FOptions.Collection.Name)),
    NyxField('title', NyxData(FOptions.TitleField.Name)),
    NyxField('lane', NyxData(FOptions.LaneField.Name)),
    NyxField('scope', NyxData(LScope)), NyxField('lanes', NyxArray(LLanes))]);
end;

class function TNyxKanbanIntent.FromData(const AValue: TNyxDataValue): TNyxKanbanIntent;
var
  LMode: TNyxText;
  LScope: TNyxText;
  LLanes: TNyxDataValue;
  LLane: TNyxDataValue;
  LOptions: TNyxKanbanOptions;
  LIndex: Integer;
begin

  if (AValue.Kind <> ndObject) or (AValue.Field('version').AsInteger <> 1) then
  begin
    raise ENyxKanban.Create('Kanban intent requires a version-one object');
  end;
  LMode := AValue.Field('mode').AsText;

  if LMode = CDisabled then
  begin

    if AValue.Count <> 2 then
    begin
      raise ENyxKanban.Create('Disabled Kanban intent has no configuration fields');
    end;
    Exit(Disabled);
  end;

  if (LMode <> CConfigured) or (AValue.Count <> 7) then
  begin
    raise ENyxKanban.Create('Kanban intent requires its exact configured fields');
  end;
  LOptions := NyxKanbanOptions(NyxCollection(AValue.Field('collection').AsText),
    NyxTextField(AValue.Field('title').AsText), NyxTextField(AValue.Field('lane').AsText));
  LScope := AValue.Field('scope').AsText;

  if LScope = CApplication then
  begin
    LOptions := LOptions.Scoped(csApplication);
  end
  else if LScope <> CInstance then
  begin
    raise ENyxKanban.Create('Unknown Kanban collection scope');
  end;
  LLanes := AValue.Field('lanes');

  if (LLanes.Kind <> ndArray) or (LLanes.Count <> 3) then
  begin
    raise ENyxKanban.Create('Kanban intent requires exactly three lanes');
  end;
  for LIndex := 0 to LLanes.Count - 1 do
  begin
    LLane := LLanes.Item(LIndex);

    if (LLane.Kind <> ndObject) or (LLane.Count <> 2) then
    begin
      raise ENyxKanban.Create('Kanban lane requires its exact name and caption');
    end;
    LOptions := LOptions.Lane(NyxKanbanLane(LLane.Field('name').AsText),
      LLane.Field('caption').AsText);
  end;
  Result := Configured(LOptions);
end;

function HasNyxKanbanIntent(ANode: TNyxNode): Boolean;
begin
  Result := (ANode <> nil) and
    ANode.Extensions.Has(NyxExtension(NyxKanbanIntentKey));
end;

function GetNyxKanbanIntent(ANode: TNyxNode): TNyxKanbanIntent;
begin
  Result := Default(TNyxKanbanIntent);

  if HasNyxKanbanIntent(ANode) then
  begin
    Result := TNyxKanbanIntent.FromData(
      ANode.Extensions.Value(NyxExtension(NyxKanbanIntentKey)));
  end;
end;

procedure SetNyxKanbanIntent(const AControl: INyxControl;
  const AIntent: TNyxKanbanIntent);
var
  LValue: TNyxDataValue;
begin

  if AControl = nil then
  begin
    raise ENyxKanban.Create('Kanban intent requires its authored control');
  end;
  LValue := AIntent.ToData;
  AControl.Extensions.SetValue(NyxExtension(NyxKanbanIntentKey), LValue);
end;

procedure InheritNyxKanbanIntent(const AControl: INyxControl);
begin

  if AControl = nil then
  begin
    raise ENyxKanban.Create('Kanban inheritance requires its authored control');
  end;
  AControl.Extensions.Remove(NyxExtension(NyxKanbanIntentKey));
end;

procedure ValidateNyxKanbanIntent(ANode: TNyxNode);
var
  LIntent: TNyxKanbanIntent;
  LOptions: TNyxKanbanOptions;
  LItems: TNyxNode;
  LMove: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LQuery: TNyxCollectionQuery;
  LColumn: TNyxCollectionColumn;
  LPath: TNyxText;
  LIndex: Integer;
begin

  if not HasNyxKanbanIntent(ANode) then
  begin
    Exit;
  end;
  LIntent := GetNyxKanbanIntent(ANode);

  if ANode.ProjectionKind = 'component' then
  begin
    Exit;
  end;

  if ANode.Kind <> 'kanban-board' then
  begin
    raise ENyxKanban.Create('Automatic Kanban intent requires its specialized board');
  end;

  if LIntent.Mode = nkiDisabled then
  begin
    Exit;
  end;
  LOptions := LIntent.Options;
  for LIndex := 0 to LOptions.Count - 1 do
  begin
    LPath := 'lane-' + IntToStr(LIndex);
    LItems := ANode.Part(NyxPart(LPath + '/items'));
    LMove := ANode.Part(NyxPart(LPath + '/move'));

    if (LItems.ProjectionKind <> 'list') or (LMove.ProjectionKind <> 'button') or
      not LItems.HasCollectionView then
    begin
      raise ENyxKanban.Create('Automatic Kanban requires bound lists and Move buttons');
    end;
    LSpec := LItems.CollectionView;
    LQuery := NyxCollectionQuery.Where(NyxWhere(LOptions.LaneField).EqualTo(
      LOptions.LaneAt(LIndex).Name, nqtExact));

    if not LSpec.Defined or (LSpec.Key.Name <> LOptions.Collection.Name) or
      (LSpec.Scope <> LOptions.Scope) or (LSpec.SelectionMode <> nsmSingle) or
      (LSpec.ParentField <> '') or (LSpec.Count <> 1) or
      (LSpec.QueryPolicy.ToData.ToJSON <> LQuery.ToData.ToJSON) then
    begin
      raise ENyxKanban.Create('Automatic Kanban list binding disagrees with its typed intent');
    end;
    LColumn := LSpec.ColumnAt(0);

    if (LColumn.FieldName <> LOptions.TitleField.Name) or
      (LColumn.Kind <> nskText) or (LColumn.Mode <> cmReadOnly) then
    begin
      raise ENyxKanban.Create('Automatic Kanban requires its typed card-title field');
    end;
  end;
end;

end.
