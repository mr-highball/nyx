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

unit nyx.master.detail.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.master.detail;

type
  { Closed commands for one copied whole-policy proposal. }
  TNyxMasterDetailEditorAction = (ndmaConfigure, ndmaDisable, ndmaInherit);

  { The exact authored owner and source-context baseline authorize no mutation.
    Inherit serializes null, rather than undefined intent. A controller must
    revalidate before its single design/source transaction. }
  TNyxMasterDetailEditorChange = record
  private
    FOwner: TNyxControlRef;
    FAction: TNyxMasterDetailEditorAction;
    FIntent: TNyxMasterDetailIntent;
    FBaseline: TNyxText;
    function GetInherit: Boolean;
    function GetIntent: TNyxMasterDetailIntent;
    procedure Validate;
  public
    function Copy: TNyxMasterDetailEditorChange;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxMasterDetailEditorChange; static;
    property Owner: TNyxControlRef read FOwner;
    property Action: TNyxMasterDetailEditorAction read FAction;
    property Inherit: Boolean read GetInherit;
    property Intent: TNyxMasterDetailIntent read GetIntent;
    property Baseline: TNyxText read FBaseline;
  end;

  { Disposable copied inputs use scalar storage on both compiler targets.
    Missing panels park the values; changed owner/source baselines retire them.
    No document, controller, node or shared mutable vector is retained. }
  TNyxMasterDetailEditorDraft = record
  private
    FEditor: TNyxText;
    FOwner: TNyxText;
    FBaseline: TNyxText;
    FTitle: TNyxText;
    FDescription: TNyxText;
  public
    procedure Clear;
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
  end;

function NyxMasterDetailEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
function NyxMasterDetailEditorActionID(const AID: TNyxText;
  AAction: TNyxMasterDetailEditorAction): TNyxText;
function NyxMasterDetailEditorFieldID(const AID: TNyxText;
  ADescription: Boolean): TNyxText;
{ Configure requires the explicit two mapped fields. The action overload accepts
  only Disable/Inherit, so an absent options record never becomes a default. }
function NyxMasterDetailEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  AAction: TNyxMasterDetailEditorAction): TNyxMasterDetailEditorChange; overload;
function NyxMasterDetailEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  const AOptions: TNyxMasterDetailOptions): TNyxMasterDetailEditorChange; overload;
procedure ValidateNyxMasterDetailEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxMasterDetailEditorChange);
{ Ordinary public content borrows the authored owner only during construction.
  Static/unbound/incompatible lists display a helpful inert Configure action. }
function NewNyxMasterDetailEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
{ Unrelated input returns False. Exact mounted membership, complete form shape,
  current source baseline and both typed drafts precede a copied proposal. }
function CaptureNyxMasterDetailEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxMasterDetailEditorChange): Boolean;

implementation

uses
  SysUtils, nyx.codec, nyx.composition, nyx.collections, nyx.collections.registry,
  nyx.collections.codec, nyx.collections.view, nyx.collections.view.types, nyx.state;

const
  CVersion = 'nyx.master-detail-editor.version';
  CFormVersion = '1';
  COwner = 'nyx.master-detail-editor.owner';
  CBaseline = 'nyx.master-detail-editor.baseline';
  CEditor = 'nyx.master-detail-editor.editor';
  CAction = 'nyx.master-detail-editor.action';
  CField = 'nyx.master-detail-editor.field';
  CPath = 'path';
  CNames: array[TNyxMasterDetailEditorAction] of TNyxText =
    ('configure', 'disable', 'inherit');
  CLabels: array[TNyxMasterDetailEditorAction] of TNyxText =
    ('Apply detail fields', 'Disable live details', 'Use inherited details');

procedure CheckAction(AAction: TNyxMasterDetailEditorAction);
begin

  if (Ord(AAction) < Ord(Low(TNyxMasterDetailEditorAction))) or
    (Ord(AAction) > Ord(High(TNyxMasterDetailEditorAction))) then
  begin
    raise ENyxMasterDetail.Create('Unknown MasterDetail editor action');
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
    raise ENyxMasterDetail.Create('MasterDetail editor requires its document');
  end;
  ARaw := ADocument.FindControl(AOwner);

  if ARaw = nil then
  begin
    raise ENyxMasterDetail.Create('MasterDetail editor owner is missing');
  end;

  if ARaw.Kind = NyxKindName(nkSlotOverride) then
  begin

    if (ARaw.Parent = nil) or (ARaw.Parent.Kind <> NyxKindName(nkComponent)) or
      (ARaw.StoredProp(NyxAttributeName(atOverrideMode)) = NyxOverrideName(noRemove)) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail editor needs an available named descriptor');
    end;
    LPath := ARaw.StoredProp(CPath);
    for LIndex := 0 to ARaw.Parent.Count - 1 do
    begin

      if (ARaw.Parent.Children[LIndex] <> ARaw) and
        (ARaw.Parent.Children[LIndex].Kind = NyxKindName(nkSlotOverride)) and
        (ARaw.Parent.Children[LIndex].StoredProp(CPath) = LPath) then
      begin
        raise ENyxMasterDetail.Create('MasterDetail editor descriptor is ambiguous');
      end;
    end;
  end;
  AContext := RealizeNyxContext(ADocument, ARaw, AProjection);
  try

    if (AProjection = nil) or (AProjection.Kind <> NyxKindName(nkMasterDetail)) then
    begin
      raise ENyxMasterDetail.Create('This authored selection is not a MasterDetail');
    end;
    ValidateNyxMasterDetailIntent(AProjection);
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

function NyxMasterDetailEditorBaseline(ADocument: TNyxDocument;
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

procedure AdmitOptions(ADocument: TNyxDocument; AProjection: TNyxNode;
  const AOptions: TNyxMasterDetailOptions);
var
  LMaster: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSnapshot: INyxCollectionSnapshot;
begin
  ValidateNyxMasterDetailOptions(AProjection, AOptions);
  LMaster := AProjection.Part(NyxPart('master'));
  LSpec := LMaster.CollectionView;

  if not ADocument.Collections.Has(LSpec.Key) then
  begin
    raise ENyxMasterDetail.Create('The master list needs an existing authored collection');
  end;
  LSnapshot := ADocument.Collections.Snapshot(LSpec.Key);
  ValidateNyxCollectionViewSnapshot(LSnapshot, LSpec, cpList);
  ValidateNyxMasterDetailFields(LSnapshot.Schema, AOptions);
end;

procedure TNyxMasterDetailEditorChange.Validate;
begin
  CheckAction(FAction);

  if (FOwner.ID = '') or (FBaseline = '') then
  begin
    raise ENyxMasterDetail.Create('MasterDetail change needs its owner and exact baseline');
  end;
  NyxControl(FOwner.ID);

  if FAction = ndmaInherit then
  begin

    if FIntent.Mode <> nmiUndefined then
    begin
      raise ENyxMasterDetail.Create('Inherit cannot carry local MasterDetail intent');
    end;
    Exit;
  end;
  FIntent.Validate;

  if ((FAction = ndmaConfigure) and (FIntent.Mode <> nmiConfigured)) or
    ((FAction = ndmaDisable) and (FIntent.Mode <> nmiDisabled)) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail action and intent disagree');
  end;
end;

function TNyxMasterDetailEditorChange.GetInherit: Boolean;
begin
  Validate;
  Result := FAction = ndmaInherit;
end;

function TNyxMasterDetailEditorChange.GetIntent: TNyxMasterDetailIntent;
begin
  Validate;

  if FAction = ndmaInherit then
  begin
    raise ENyxMasterDetail.Create('Inherited details have no local intent');
  end;
  Result := FIntent.Copy;
end;

function TNyxMasterDetailEditorChange.Copy: TNyxMasterDetailEditorChange;
begin
  Validate;
  Result.FOwner := NyxControl(FOwner.ID);
  Result.FAction := FAction;
  Result.FIntent := FIntent.Copy;
  Result.FBaseline := FBaseline;
end;

function TNyxMasterDetailEditorChange.ToData: TNyxDataValue;
var
  LIntent: TNyxDataValue;
begin
  Validate;
  LIntent := NyxNull;

  if FAction <> ndmaInherit then
  begin
    LIntent := FIntent.ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('owner', NyxData(FOwner.ID)),
    NyxField('action', NyxData(CNames[FAction])), NyxField('baseline', NyxData(FBaseline)),
    NyxField('intent', LIntent)]);
end;

class function TNyxMasterDetailEditorChange.FromData(
  const AData: TNyxDataValue): TNyxMasterDetailEditorChange;
var
  LAction: TNyxMasterDetailEditorAction;
  LName: TNyxText;
  LIntent: TNyxDataValue;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail change needs its exact version-one object');
  end;
  Result := Default(TNyxMasterDetailEditorChange);
  Result.FOwner := NyxControl(AData.Field('owner').AsText);
  Result.FBaseline := AData.Field('baseline').AsText;
  LName := AData.Field('action').AsText;
  LIntent := AData.Field('intent');
  for LAction := Low(TNyxMasterDetailEditorAction) to High(TNyxMasterDetailEditorAction) do
  begin

    if LName = CNames[LAction] then
    begin
      Result.FAction := LAction;

      if LAction = ndmaInherit then
      begin

        if LIntent.Kind <> ndNull then
        begin
          raise ENyxMasterDetail.Create('MasterDetail inheritance requires explicit null');
        end;
      end
      else
      begin
        Result.FIntent := TNyxMasterDetailIntent.FromData(LIntent);
      end;
      Result.Validate;
      Exit;
    end;
  end;
  raise ENyxMasterDetail.Create('Unknown MasterDetail editor action');
end;

function NyxMasterDetailEditorActionID(const AID: TNyxText;
  AAction: TNyxMasterDetailEditorAction): TNyxText;
begin
  CheckAction(AAction);
  Result := NyxControl(AID).ID + '-' + CNames[AAction];
end;

function NyxMasterDetailEditorFieldID(const AID: TNyxText;
  ADescription: Boolean): TNyxText;
begin
  Result := NyxControl(AID).ID + '-title-field';

  if ADescription then
  begin
    Result := NyxControl(AID).ID + '-description-field';
  end;
end;

function NyxMasterDetailEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  AAction: TNyxMasterDetailEditorAction): TNyxMasterDetailEditorChange;
begin
  CheckAction(AAction);

  if AAction = ndmaConfigure then
  begin
    raise ENyxMasterDetail.Create('Configure requires two explicit mapped text fields');
  end;
  Result := Default(TNyxMasterDetailEditorChange);
  Result.FOwner := NyxControl(AOwner.ID);
  Result.FAction := AAction;
  Result.FBaseline := NyxMasterDetailEditorBaseline(ADocument, AOwner);

  if AAction = ndmaDisable then
  begin
    Result.FIntent := TNyxMasterDetailIntent.Disabled;
  end
  else if not HasNyxMasterDetailIntent(ADocument.FindControl(AOwner)) then
  begin
    raise ENyxMasterDetail.Create('There is no local MasterDetail intent to inherit');
  end;
  Result.Validate;
end;

function NyxMasterDetailEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  const AOptions: TNyxMasterDetailOptions): TNyxMasterDetailEditorChange;
var
  LRaw: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LOptions: TNyxMasterDetailOptions;
begin
  LOptions := AOptions.Copy;
  Result := Default(TNyxMasterDetailEditorChange);
  LContext := nil;
  try
    ResolveOwner(ADocument, AOwner, LRaw, LContext, LProjection);
    AdmitOptions(ADocument, LProjection, LOptions);
    Result.FOwner := NyxControl(LRaw.ID);
    Result.FAction := ndmaConfigure;
    Result.FIntent := TNyxMasterDetailIntent.Configured(LOptions);
    Result.FBaseline := NyxMasterDetailEditorBaseline(ADocument, AOwner);
    Result.Validate;
  finally
    LContext.Free;
  end;
end;

procedure ValidateNyxMasterDetailEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxMasterDetailEditorChange);
var
  LChange: TNyxMasterDetailEditorChange;
  LCurrent: TNyxMasterDetailEditorChange;
begin
  LChange := AChange.Copy;

  if LChange.Action = ndmaConfigure then
  begin
    LCurrent := NyxMasterDetailEditorChange(ADocument, LChange.Owner, LChange.Intent.Options);
  end
  else
  begin
    LCurrent := NyxMasterDetailEditorChange(ADocument, LChange.Owner, LChange.Action);
  end;

  if LCurrent.Baseline <> LChange.Baseline then
  begin
    raise ENyxMasterDetail.Create('MasterDetail or its source changed; reopen this form');
  end;
end;

function IntentLabel(const AIntent: TNyxMasterDetailIntent): TNyxText;
begin
  Result := 'static/inherited';

  if AIntent.Mode = nmiDisabled then
  begin
    Result := 'disabled';
  end
  else if AIntent.Mode = nmiConfigured then
  begin
    Result := AIntent.Options.TitleField.Name + ' / ' + AIntent.Options.DescriptionField.Name;
  end;
end;

function InitialFields(ADocument: TNyxDocument; AProjection: TNyxNode;
  out ATitle, ADescription, AStatus: TNyxText): Boolean;
var
  LMaster: TNyxNode;
  LSchema: TNyxCollectionSchema;
  LIntent: TNyxMasterDetailIntent;
  LOptions: TNyxMasterDetailOptions;
  LDefault: TNyxText;
  LIndex: Integer;
begin
  ATitle := '';
  ADescription := '';
  AStatus := '';
  Result := False;
  LIntent := GetNyxMasterDetailIntent(AProjection);

  if LIntent.Mode = nmiConfigured then
  begin
    ATitle := LIntent.Options.TitleField.Name;
    ADescription := LIntent.Options.DescriptionField.Name;
  end;
  try
    LMaster := AProjection.Part(NyxPart('master'));

    if (LMaster = nil) or not LMaster.HasCollectionView or
      not LMaster.CollectionView.Defined then
    begin
      raise ENyxMasterDetail.Create('Bind the master list to a collection first.');
    end;

    if not ADocument.Collections.Has(LMaster.CollectionView.Key) then
    begin
      raise ENyxMasterDetail.Create('The master list collection is not declared.');
    end;
    LSchema := ADocument.Collections.Snapshot(LMaster.CollectionView.Key).Schema;
    LDefault := '';
    for LIndex := 0 to LSchema.Count - 1 do
    begin

      if LSchema.FieldAt(LIndex).Kind = nskText then
      begin
        LDefault := LSchema.FieldAt(LIndex).Name;
        Break;
      end;
    end;

    if ATitle = '' then
    begin
      ATitle := LDefault;
    end;

    if ADescription = '' then
    begin
      ADescription := LDefault;
    end;
    { Availability proves the source can accept text mappings. Current draft
      values are admitted separately on Configure, so a stale mapped name can
      be corrected rather than permanently disabling the editor. }
    LOptions := NyxMasterDetailOptions(NyxTextField(LDefault), NyxTextField(LDefault));
    AdmitOptions(ADocument, AProjection, LOptions);
    AStatus := 'Source: ' + LMaster.CollectionView.Key.Name +
      '. Edit only the two detail text fields; source, query and paging stay on the master list.';
    Result := True;
  except
    on E: Exception do
    begin
      AStatus := TNyxText(E.Message) + ' Use the master list binding to choose a compatible ' +
        'single-selection source with text fields.';
    end;
  end;
end;

function NewNyxMasterDetailEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
var
  LRaw: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LTitle: TNyxText;
  LDescription: TNyxText;
  LStatus: TNyxText;
  LLocal: TNyxText;
  LCanConfigure: Boolean;
  LDescriptionField: Boolean;
  LInput: INyxInput;
  LButton: INyxButton;
  LAction: TNyxMasterDetailEditorAction;
begin
  LContext := nil;
  try
    ResolveOwner(ADocument, AOwner, LRaw, LContext, LProjection);
    LCanConfigure := InitialFields(ADocument, LProjection, LTitle, LDescription, LStatus);
    LLocal := 'inherited/default';

    if HasNyxMasterDetailIntent(LRaw) then
    begin
      LLocal := IntentLabel(GetNyxMasterDetailIntent(LRaw));
    end;
    Result := NewNyxCard(AID);
    Result.Configure.Gap(10).Padding(16).Done;
    Result.Node.SetProp(CVersion, CFormVersion).SetProp(COwner, LRaw.ID).
      SetProp(CBaseline, NyxMasterDetailEditorBaseline(ADocument, AOwner));
    Result.Add(NewNyxLabel(AID + '-title').WithText('Master and detail'));
    Result.Add(NewNyxLabel(AID + '-local').WithText('Local policy: ' + LLocal));
    Result.Add(NewNyxLabel(AID + '-effective').WithText(
      'Effective policy: ' + IntentLabel(GetNyxMasterDetailIntent(LProjection))));
    Result.Add(NewNyxLabel(AID + '-status').WithText(LStatus));
    for LDescriptionField := False to True do
    begin
      LInput := NewNyxInput(NyxMasterDetailEditorFieldID(AID, LDescriptionField));

      if LDescriptionField then
      begin
        LInput.Configure.Text('Description text field').Value(LDescription).
          AccessibleName('Detail description text field').
          PartName(NyxPart('description-field')).Done;
        LInput.Node.SetProp(CField, 'description');
      end
      else
      begin
        LInput.Configure.Text('Title text field').Value(LTitle).
          AccessibleName('Detail title text field').PartName(NyxPart('title-field')).Done;
        LInput.Node.SetProp(CField, 'title');
      end;
      LInput.Node.SetProp(CEditor, AID);
      Result.Add(LInput);
    end;
    for LAction := Low(TNyxMasterDetailEditorAction) to High(TNyxMasterDetailEditorAction) do
    begin
      LButton := NewNyxButton(NyxMasterDetailEditorActionID(AID, LAction));
      LButton.Configure.Text(CLabels[LAction]).AccessibleName(CLabels[LAction]).
        PartName(NyxPart(CNames[LAction])).Done;
      LButton.Node.SetProp(CEditor, AID).SetProp(CAction, CNames[LAction]);

      if LAction = ndmaConfigure then
      begin
        LButton.Configure.Enabled(LCanConfigure).Done;
      end
      else if LAction = ndmaInherit then
      begin
        LButton.Configure.Enabled(HasNyxMasterDetailIntent(LRaw)).Done;
      end;
      Result.Add(LButton);
    end;
  finally
    LContext.Free;
  end;
end;

function Member(AEditor: TNyxNode; const AID: TNyxText): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to AEditor.Count - 1 do
  begin

    if AEditor.Children[LIndex].SourceID = AID then
    begin

      if Result <> nil then
      begin
        raise ENyxMasterDetail.Create('MasterDetail form member is ambiguous');
      end;
      Result := AEditor.Children[LIndex];
    end;
  end;

  if Result = nil then
  begin
    raise ENyxMasterDetail.Create('MasterDetail form member is missing');
  end;
end;

function FieldNode(AEditor: TNyxNode; const AForm: TNyxText;
  ADescription: Boolean): TNyxNode;
var
  LName: TNyxText;
begin
  LName := 'title';

  if ADescription then
  begin
    LName := 'description';
  end;
  Result := Member(AEditor, NyxMasterDetailEditorFieldID(AForm, ADescription));

  if (Result.Kind <> NyxKindName(nkInput)) or
    (Result.ProjectionKind <> NyxKindName(nkInput)) or
    (Result.StoredProp(CEditor) <> AForm) or (Result.StoredProp(CField) <> LName) or
    (Result.StoredProp(NyxAttributeName(atPart)) <> LName + '-field') then
  begin
    raise ENyxMasterDetail.Create('MasterDetail form input is malformed');
  end;
end;

function ActionNode(AEditor: TNyxNode; const AForm: TNyxText;
  AAction: TNyxMasterDetailEditorAction): TNyxNode;
begin
  Result := Member(AEditor, NyxMasterDetailEditorActionID(AForm, AAction));

  if (Result.Kind <> NyxKindName(nkButton)) or
    (Result.ProjectionKind <> NyxKindName(nkButton)) or
    (Result.StoredProp(CEditor) <> AForm) or
    (Result.StoredProp(CAction) <> CNames[AAction]) or
    (Result.StoredProp(NyxAttributeName(atPart)) <> CNames[AAction]) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail form action is malformed');
  end;
end;

procedure CheckForm(AEditor: TNyxNode; const AForm: TNyxText);
var
  LAction: TNyxMasterDetailEditorAction;
begin

  if (AEditor = nil) or (AEditor.Kind <> NyxKindName(nkCard)) or
    (AEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (AEditor.StoredProp(CVersion) <> CFormVersion) or
    ((AEditor.SourceID <> AForm) and (AEditor.ID <> AForm)) or
    (AEditor.StoredProp(COwner) = '') or (AEditor.StoredProp(CBaseline) = '') then
  begin
    raise ENyxMasterDetail.Create('MasterDetail form is missing or malformed');
  end;
  FieldNode(AEditor, AForm, False);
  FieldNode(AEditor, AForm, True);
  for LAction := Low(TNyxMasterDetailEditorAction) to High(TNyxMasterDetailEditorAction) do
  begin
    ActionNode(AEditor, AForm, LAction);
  end;
end;

function CaptureNyxMasterDetailEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxMasterDetailEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LForm: TNyxText;
  LOwner: TNyxControlRef;
  LAction: TNyxMasterDetailEditorAction;
  LSelected: TNyxMasterDetailEditorAction;
  LFound: Boolean;
  LOptions: TNyxMasterDetailOptions;
begin
  AChange := Default(TNyxMasterDetailEditorChange);
  Result := False;

  if (AButton = nil) or (AButton.Kind <> NyxKindName(nkButton)) or
    (AButton.StoredProp(CEditor) = '') then
  begin
    Exit;
  end;

  if (AShellRoot = nil) or (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxMasterDetail.Create('This MasterDetail form belongs to a retired view');
  end;
  LForm := AButton.StoredProp(CEditor);
  LEditor := AButton.Parent;
  CheckForm(LEditor, LForm);
  LFound := False;
  LSelected := ndmaConfigure;
  for LAction := Low(TNyxMasterDetailEditorAction) to High(TNyxMasterDetailEditorAction) do
  begin

    if AButton = ActionNode(LEditor, LForm, LAction) then
    begin
      LSelected := LAction;
      LFound := True;
    end;
  end;

  if not LFound or (AButton.Prop(NyxAttributeName(atEnabled), 'true') <> 'true') then
  begin
    raise ENyxMasterDetail.Create('This MasterDetail action is unavailable');
  end;
  LOwner := NyxControl(LEditor.StoredProp(COwner));

  if LEditor.StoredProp(CBaseline) <> NyxMasterDetailEditorBaseline(ADocument, LOwner) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail or its source changed; reopen this form');
  end;

  if LSelected = ndmaConfigure then
  begin
    LOptions := NyxMasterDetailOptions(
      NyxTextField(FieldNode(LEditor, LForm, False).Prop(NyxAttributeName(atValue))),
      NyxTextField(FieldNode(LEditor, LForm, True).Prop(NyxAttributeName(atValue))));
    AChange := NyxMasterDetailEditorChange(ADocument, LOwner, LOptions);
  end
  else
  begin
    AChange := NyxMasterDetailEditorChange(ADocument, LOwner, LSelected);
  end;
  Result := True;
end;

procedure TNyxMasterDetailEditorDraft.Clear;
begin
  FEditor := '';
  FOwner := '';
  FBaseline := '';
  FTitle := '';
  FDescription := '';
end;

procedure TNyxMasterDetailEditorDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LNext: TNyxMasterDetailEditorDraft;
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
  CheckForm(LEditor, AEditor);
  LNext.FEditor := AEditor;
  LNext.FOwner := LEditor.StoredProp(COwner);
  LNext.FBaseline := LEditor.StoredProp(CBaseline);
  LNext.FTitle := FieldNode(LEditor, AEditor, False).Prop(NyxAttributeName(atValue));
  LNext.FDescription := FieldNode(LEditor, AEditor, True).Prop(NyxAttributeName(atValue));
  Self := LNext;
end;

function TNyxMasterDetailEditorDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
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
  CheckForm(LEditor, FEditor);
  FieldNode(LEditor, FEditor, False).SetProp(NyxAttributeName(atValue), FTitle);
  FieldNode(LEditor, FEditor, True).SetProp(NyxAttributeName(atValue), FDescription);
  Result := True;
end;

end.
