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

unit nyx.data.toolbar.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.data.toolbar, nyx.menu.editor;

type
  { Closed commands for one copied whole-policy proposal. }
  TNyxDataToolbarEditorAction = (nteaConfigure, nteaDisable, nteaInherit);

  { The exact authored owner and source-context baseline authorize no mutation.
    Inherit serializes null, rather than undefined intent. A controller must
    revalidate before its single design/source transaction. }
  TNyxDataToolbarEditorChange = record
  private
    FOwner: TNyxControlRef;
    FAction: TNyxDataToolbarEditorAction;
    FIntent: TNyxDataToolbarIntent;
    FBaseline: TNyxText;
    function GetInherit: Boolean;
    function GetIntent: TNyxDataToolbarIntent;
    procedure Validate;
  public
    function Copy: TNyxDataToolbarEditorChange;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDataToolbarEditorChange; static;
    property Owner: TNyxControlRef read FOwner;
    property Action: TNyxDataToolbarEditorAction read FAction;
    property Inherit: Boolean read GetInherit;
    property Intent: TNyxDataToolbarIntent read GetIntent;
    property Baseline: TNyxText read FBaseline;
  end;

  { Closed field identities keep host navigation independent of property names. }
  TNyxDataToolbarEditorField = (ntefTarget, ntefSearch, ntefMatch,
    ntefComparison, ntefBaseline, ntefIncluded, ntefCaption, ntefSortField,
    ntefDirection, ntefSortComparison);
  { Shared copied scalar draft storage borrows no controls or document. Exact
    owner/source baseline and complete member shape fence unrelated repaints. }
  TNyxDataToolbarEditorDraft = TNyxMenuEditorDraft;

function NyxDataToolbarEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
function NyxDataToolbarEditorActionID(const AID: TNyxText;
  AAction: TNyxDataToolbarEditorAction): TNyxText;
function NyxDataToolbarEditorFieldID(const AID: TNyxText;
  AField: TNyxDataToolbarEditorField): TNyxText;
function NyxDataToolbarEditorSortID(const AID: TNyxText; AIndex: Integer): TNyxText;
function NyxDataToolbarEditorKeyID(const AID: TNyxText; AIndex: Integer): TNyxText;
{ Configure requires explicit typed target/field/query options. The action overload accepts
  only Disable/Inherit, so an absent options record never becomes a default. }
function NyxDataToolbarEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  AAction: TNyxDataToolbarEditorAction): TNyxDataToolbarEditorChange; overload;
function NyxDataToolbarEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  const AOptions: TNyxDataToolbarOptions): TNyxDataToolbarEditorChange; overload;
procedure ValidateNyxDataToolbarEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxDataToolbarEditorChange);
{ Ordinary public content borrows the authored owner only during construction.
  Selectors map bounded display captions to exact identities, including Unicode
  and embedded newlines. Sort rows preserve all eight possible ordering keys;
  the empty next row/key supports incremental additions through the same Apply. }
function NewNyxDataToolbarEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
{ Unrelated input returns False. Exact mounted membership, complete form shape,
  current source baseline and typed form values precede a copied proposal. }
function CaptureNyxDataToolbarEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxDataToolbarEditorChange): Boolean;
{ Mutates ONLY a caller-owned detached candidate. Required roles, selector domain
  and intent are installed together, preserving named reusable siblings and
  definitions. The host performs final source admission and owns Undo/Redo. }
procedure ApplyNyxDataToolbarEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxDataToolbarEditorChange);

implementation

uses
  SysUtils, nyx.codec, nyx.composition, nyx.collections, nyx.collections.registry,
  nyx.collections.codec, nyx.collections.view, nyx.collections.view.types, nyx.state,
  nyx.contract, nyx.collections.query, nyx.collections.bindings, nyx.schema;

const
  CVersion = 'nyx.data-toolbar-editor.version';
  CFormVersion = '1';
  COwner = NyxMenuFormOwnerKey;
  CBaseline = NyxMenuFormBaselineKey;
  CEditor = 'nyx.data-toolbar-editor.editor';
  CAction = 'nyx.data-toolbar-editor.action';
  CField = 'nyx.data-toolbar-editor.field';
  CChoices = 'nyx.data-toolbar-editor.choices';
  CRows = 'nyx.data-toolbar-editor.rows';
  CKeys = 'nyx.data-toolbar-editor.keys';
  CNamesField: array[TNyxDataToolbarEditorField] of TNyxText =
    ('target', 'search', 'match', 'comparison', 'baseline', 'included', 'caption',
      'sort-field', 'direction', 'sort-comparison');
  CMatchLabels: array[TNyxToolbarSearchMode] of TNyxText =
    ('Contains', 'Starts with', 'Equals');
  CComparisonLabels: array[TNyxQueryTextComparison] of TNyxText =
    ('Exact Unicode', 'ASCII case insensitive');
  CDirectionLabels: array[TNyxSortDirection] of TNyxText =
    ('Ascending', 'Descending');
  CPath = 'path';
  CNames: array[TNyxDataToolbarEditorAction] of TNyxText =
    ('configure', 'disable', 'inherit');
  CLabels: array[TNyxDataToolbarEditorAction] of TNyxText =
    ('Apply search and sort', 'Disable toolbar behavior', 'Use inherited behavior');

procedure CheckAction(AAction: TNyxDataToolbarEditorAction);
begin

  if (Ord(AAction) < Ord(Low(TNyxDataToolbarEditorAction))) or
    (Ord(AAction) > Ord(High(TNyxDataToolbarEditorAction))) then
  begin
    raise ENyxDataToolbar.Create('Unknown DataToolbar editor action');
  end;
end;

procedure ResolveOwner(ADocument: TNyxDocument; const AOwner: TNyxControlRef;
  out ARaw, AContext, AProjection: TNyxNode);
var
  LIndex: Integer;
  LPath: TNyxText;
begin
  ARaw := nil;
  AContext := nil;
  AProjection := nil;

  if ADocument = nil then
  begin
    raise ENyxDataToolbar.Create('DataToolbar editor requires its document');
  end;
  ARaw := ADocument.FindControl(AOwner);

  if ARaw = nil then
  begin
    raise ENyxDataToolbar.Create('DataToolbar editor owner is missing');
  end;

  if ARaw.Kind = NyxKindName(nkSlotOverride) then
  begin

    if (ARaw.Parent = nil) or (ARaw.Parent.Kind <> NyxKindName(nkComponent)) or
      (ARaw.StoredProp(NyxAttributeName(atOverrideMode)) = NyxOverrideName(noRemove)) then
    begin
      raise ENyxDataToolbar.Create('DataToolbar editor needs an available named descriptor');
    end;
    LPath := ARaw.StoredProp(CPath);
    for LIndex := 0 to ARaw.Parent.Count - 1 do
    begin

      if (ARaw.Parent.Children[LIndex] <> ARaw) and
        (ARaw.Parent.Children[LIndex].Kind = NyxKindName(nkSlotOverride)) and
        (ARaw.Parent.Children[LIndex].StoredProp(CPath) = LPath) then
      begin
        raise ENyxDataToolbar.Create('DataToolbar editor descriptor is ambiguous');
      end;
    end;
  end;
  AContext := RealizeNyxContext(ADocument, ARaw, AProjection);
  try

    if (AProjection = nil) or (AProjection.Kind <> NyxKindName(nkDataToolbar)) then
    begin
      raise ENyxDataToolbar.Create('This authored selection is not a DataToolbar');
    end;
    ValidateNyxDataToolbarIntent(AProjection);
  except
    FreeAndNil(AContext);
    AProjection := nil;
    raise;
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

function NyxDataToolbarEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LRaw: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LRoots: array of TNyxDataValue;
  LIndex: Integer;
begin
  LContext := nil;
  try
    ResolveOwner(ADocument, AOwner, LRaw, LContext, LProjection);
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
    Result := NyxObject([NyxField('owner', NyxData(LRaw.ID)),
      NyxField('local', TNyxCodec.NodeSnapshot(LRaw)), NyxField('roots', NyxArray(LRoots)),
      NyxField('effective', TNyxCodec.NodeSnapshot(LProjection)),
      NyxField('schemas', NyxData(SchemaDefaults(ADocument)))]).ToJSON;
  finally
    LContext.Free;
  end;
end;

{ Resolve only the toolbar's exact reusable expansion. A globally matching
  control elsewhere is never an alternative to a missing local target. }
function TargetNode(AContext, AProjection: TNyxNode;
  const ATarget: TNyxControlRef): TNyxNode;
var
  LPrefix: TNyxText;
  LIndex: Integer;
begin
  LPrefix := '';
  for LIndex := Length(AProjection.ID) downto 1 do
  begin

    if AProjection.ID[LIndex] = '/' then
    begin
      LPrefix := Copy(AProjection.ID, 1, LIndex - 1);
      Break;
    end;
  end;
  Result := AContext.Find(NyxQualifiedID(LPrefix, ATarget.ID));

  if (Result = nil) or not Result.HasCollectionView or
    not Result.CollectionView.Defined or
    not (NyxCollectionProjectionForNode(Result) in [cpList, cpTable]) then
  begin
    raise ENyxDataToolbar.Create('Choose a bound List or Table in this toolbar workspace');
  end;
end;

procedure AdmitOptions(ADocument: TNyxDocument; AContext, AProjection: TNyxNode;
  const AOptions: TNyxDataToolbarOptions);
var
  LTarget: TNyxNode;
  LSnapshot: INyxCollectionSnapshot;
begin
  AOptions.Validate;
  LTarget := TargetNode(AContext, AProjection, AOptions.Target);

  if not ADocument.Collections.Has(LTarget.CollectionView.Key) then
  begin
    raise ENyxDataToolbar.Create('The target collection must have an authored schema');
  end;
  LSnapshot := ADocument.Collections.Snapshot(LTarget.CollectionView.Key);
  ValidateNyxCollectionViewSnapshot(LSnapshot, LTarget.CollectionView,
    NyxCollectionProjectionForNode(LTarget));
  AOptions.ValidateSchema(LSnapshot.Schema);
end;
procedure TNyxDataToolbarEditorChange.Validate;
begin
  CheckAction(FAction);

  if (FOwner.ID = '') or (FBaseline = '') then
  begin
    raise ENyxDataToolbar.Create('DataToolbar change needs its owner and exact baseline');
  end;
  NyxControl(FOwner.ID);

  if FAction = nteaInherit then
  begin

    if FIntent.Mode <> ntiUndefined then
    begin
      raise ENyxDataToolbar.Create('Inherit cannot carry local DataToolbar intent');
    end;
    Exit;
  end;
  FIntent.ToData;

  if ((FAction = nteaConfigure) and (FIntent.Mode <> ntiConfigured)) or
    ((FAction = nteaDisable) and (FIntent.Mode <> ntiDisabled)) then
  begin
    raise ENyxDataToolbar.Create('DataToolbar action and intent disagree');
  end;
end;

function TNyxDataToolbarEditorChange.GetInherit: Boolean;
begin
  Validate;
  Result := FAction = nteaInherit;
end;

function TNyxDataToolbarEditorChange.GetIntent: TNyxDataToolbarIntent;
begin
  Validate;

  if FAction = nteaInherit then
  begin
    raise ENyxDataToolbar.Create('Inherited toolbar behavior has no local intent');
  end;
  Result := FIntent.Copy;
end;

function TNyxDataToolbarEditorChange.Copy: TNyxDataToolbarEditorChange;
begin
  Validate;
  Result.FOwner := NyxControl(FOwner.ID);
  Result.FAction := FAction;
  Result.FIntent := FIntent.Copy;
  Result.FBaseline := FBaseline;
end;

function TNyxDataToolbarEditorChange.ToData: TNyxDataValue;
var
  LIntent: TNyxDataValue;
begin
  Validate;
  LIntent := NyxNull;

  if FAction <> nteaInherit then
  begin
    LIntent := FIntent.ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('owner', NyxData(FOwner.ID)),
    NyxField('action', NyxData(CNames[FAction])), NyxField('baseline', NyxData(FBaseline)),
    NyxField('intent', LIntent)]);
end;

class function TNyxDataToolbarEditorChange.FromData(
  const AData: TNyxDataValue): TNyxDataToolbarEditorChange;
var
  LAction: TNyxDataToolbarEditorAction;
  LName: TNyxText;
  LIntent: TNyxDataValue;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxDataToolbar.Create('DataToolbar change needs its exact version-one object');
  end;
  Result := Default(TNyxDataToolbarEditorChange);
  Result.FOwner := NyxControl(AData.Field('owner').AsText);
  Result.FBaseline := AData.Field('baseline').AsText;
  LName := AData.Field('action').AsText;
  LIntent := AData.Field('intent');
  for LAction := Low(TNyxDataToolbarEditorAction) to High(TNyxDataToolbarEditorAction) do
  begin

    if LName = CNames[LAction] then
    begin
      Result.FAction := LAction;

      if LAction = nteaInherit then
      begin

        if LIntent.Kind <> ndNull then
        begin
          raise ENyxDataToolbar.Create('DataToolbar inheritance requires explicit null');
        end;
      end
      else
      begin
        Result.FIntent := TNyxDataToolbarIntent.FromData(LIntent);
      end;
      Result.Validate;
      Exit;
    end;
  end;
  raise ENyxDataToolbar.Create('Unknown DataToolbar editor action');
end;

function NyxDataToolbarEditorActionID(const AID: TNyxText;
  AAction: TNyxDataToolbarEditorAction): TNyxText;
begin
  CheckAction(AAction);
  Result := NyxControl(AID).ID + '-' + CNames[AAction];
end;

function NyxDataToolbarEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  AAction: TNyxDataToolbarEditorAction): TNyxDataToolbarEditorChange;
begin
  CheckAction(AAction);

  if AAction = nteaConfigure then
  begin
    raise ENyxDataToolbar.Create('Configure requires explicit target, search and sort options');
  end;
  Result := Default(TNyxDataToolbarEditorChange);
  Result.FOwner := NyxControl(AOwner.ID);
  Result.FAction := AAction;
  Result.FBaseline := NyxDataToolbarEditorBaseline(ADocument, AOwner);

  if AAction = nteaDisable then
  begin
    Result.FIntent := TNyxDataToolbarIntent.Disabled;
  end
  else if not HasNyxDataToolbarIntent(ADocument.FindControl(AOwner)) then
  begin
    raise ENyxDataToolbar.Create('There is no local DataToolbar intent to inherit');
  end;
  Result.Validate;
end;

function NyxDataToolbarEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  const AOptions: TNyxDataToolbarOptions): TNyxDataToolbarEditorChange;
var
  LRaw: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LOptions: TNyxDataToolbarOptions;
begin
  LOptions := AOptions.Copy;
  Result := Default(TNyxDataToolbarEditorChange);
  LContext := nil;
  try
    ResolveOwner(ADocument, AOwner, LRaw, LContext, LProjection);
    AdmitOptions(ADocument, LContext, LProjection, LOptions);
    Result.FOwner := NyxControl(LRaw.ID);
    Result.FAction := nteaConfigure;
    Result.FIntent := TNyxDataToolbarIntent.Configured(LOptions);
    Result.FBaseline := NyxDataToolbarEditorBaseline(ADocument, AOwner);
    Result.Validate;
  finally
    LContext.Free;
  end;
end;

procedure ValidateNyxDataToolbarEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxDataToolbarEditorChange);
var
  LChange: TNyxDataToolbarEditorChange;
  LCurrent: TNyxDataToolbarEditorChange;
begin
  LChange := AChange.Copy;

  if LChange.Action = nteaConfigure then
  begin
    LCurrent := NyxDataToolbarEditorChange(ADocument, LChange.Owner, LChange.Intent.Options);
  end
  else
  begin
    LCurrent := NyxDataToolbarEditorChange(ADocument, LChange.Owner, LChange.Action);
  end;

  if LCurrent.Baseline <> LChange.Baseline then
  begin
    raise ENyxDataToolbar.Create('DataToolbar or its source changed; reopen this form');
  end;
end;


{ Shared form and detached application deliberately retain no host services. }
{$I nyx.data.toolbar.editor.form.inc}
{$I nyx.data.toolbar.editor.apply.inc}

end.
