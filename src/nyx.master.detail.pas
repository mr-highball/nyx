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

unit nyx.master.detail;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Runtime commands and borrowed refresh receivers are UI-thread confined.
  Copied options, intent and record snapshots retain no document or renderer. }
uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.collections,
  nyx.collections.bindings, nyx.events, nyx.scheduler, nyx.behavior,
  nyx.publication.types, nyx.coordinators;

const
  NyxMasterDetailIntentKey = 'nyx.master-detail';

type
  { Admission failures belong to the existing portable collection family. }
  ENyxMasterDetail = class(ENyxCollection);
  { Revoked generations never authorize another refresh or Edit capture. }
  ENyxMasterDetailCancelled = class(ENyxMasterDetail);

  { The master list binding alone owns source, scope, query and paging. These
    two copied text fields select the detail presentation of that same source. }
  TNyxMasterDetailOptions = record
  private
    FTitle: TNyxTextFieldRef;
    FDescription: TNyxTextFieldRef;
    FDefined: Boolean;
  public
    function Copy: TNyxMasterDetailOptions;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxMasterDetailOptions; static;
    property TitleField: TNyxTextFieldRef read FTitle;
    property DescriptionField: TNyxTextFieldRef read FDescription;
  end;

  { Absence inherits or leaves a static recipe. Disabled is a persistent local
    availability mask; Configured supplies the complete copied field policy. }
  TNyxMasterDetailIntentMode = (nmiUndefined, nmiDisabled, nmiConfigured);
  { Portable authored intent contains no runtime selection or live handles. }
  TNyxMasterDetailIntent = record
  private
    FMode: TNyxMasterDetailIntentMode;
    FOptions: TNyxMasterDetailOptions;
    function GetOptions: TNyxMasterDetailOptions;
  public
    class function Disabled: TNyxMasterDetailIntent; static;
    class function Configured(const AOptions: TNyxMasterDetailOptions):
      TNyxMasterDetailIntent; static;
    function Copy: TNyxMasterDetailIntent;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxMasterDetailIntent; static;
    property Mode: TNyxMasterDetailIntentMode read FMode;
    property Options: TNyxMasterDetailOptions read GetOptions;
  end;

  { Detached descriptive data, never mutation authority. Item copies retain
    exact scalar families and remain readable after selection/view retirement. }
  TNyxMasterDetailRecord = record
  private
    FDefined: Boolean;
    FRef: TNyxItemRef;
    FRevision: Integer;
    FItem: TNyxCollectionItem;
    function GetRef: TNyxItemRef;
    function GetRevision: Integer;
    function GetItem: TNyxCollectionItem;
  public
    function Copy: TNyxMasterDetailRecord;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxMasterDetailRecord; static;
    property Defined: Boolean read FDefined;
    property Ref: TNyxItemRef read GetRef;
    property DataRevision: Integer read GetRevision;
    property Item: TNyxCollectionItem read GetItem;
  end;

  { Last borrowed presentation call; it may disconnect or dispose its receiver. }
  TNyxMasterDetailRefresh = procedure of object;
  { One accepted runtime owner. Current is descriptive cached data. CaptureEdit
    rechecks active generation and the exact visible selection before copying.
    EnrichEdit preserves unrelated events and refuses conflicting Details. }
  INyxMasterDetailBinding = interface(IInterface)
    ['{77DAAE64-EEC8-445B-BE1C-7A29DF932FE6}']
    function GetConnected: Boolean;
    function GetCurrent: TNyxMasterDetailRecord;
    function CaptureEdit: TNyxMasterDetailRecord;
    function EnrichEdit(const AEvent: TNyxEventInfo): TNyxEventInfo;
    procedure Refresh;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
    property Current: TNyxMasterDetailRecord read GetCurrent;
  end;
  { Preparation renders cached text/availability on independent candidate nodes.
    Install activates flags only. Retire releases preparation-only event leases;
    installed lifetime remains usable until explicit Disconnect/destruction. }
  INyxPreparedMasterDetailBinding = interface(INyxPreparedPublication)
    ['{B4843B41-1188-4CFB-AEF8-08101B38156E}']
    function GetBinding: INyxMasterDetailBinding;
    procedure Revoke;
    procedure Disconnect;
    property Binding: INyxMasterDetailBinding read GetBinding;
  end;

function NyxMasterDetailOptions(const ATitleField,
  ADescriptionField: TNyxTextFieldRef): TNyxMasterDetailOptions;
function HasNyxMasterDetailIntent(ANode: TNyxNode): Boolean;
function GetNyxMasterDetailIntent(ANode: TNyxNode): TNyxMasterDetailIntent;
procedure SetNyxMasterDetailIntent(const AControl: INyxControl;
  const AIntent: TNyxMasterDetailIntent);
procedure InheritNyxMasterDetailIntent(const AControl: INyxControl);
{ Metadata and known target only; raw reusable references/descriptors defer. }
procedure ValidateNyxMasterDetailIntent(ANode: TNyxNode);
{ Complete expanded behavioral roles and one source-owned SINGLE list spec.
  Titles, styles and arbitrary payload remain independently authored. }
procedure ValidateNyxMasterDetailOptions(ANode: TNyxNode;
  const AOptions: TNyxMasterDetailOptions);
{ Source/schema admission uses the same exact two text-field requirements. }
procedure ValidateNyxMasterDetailFields(const ASchema: TNyxCollectionSchema;
  const AOptions: TNyxMasterDetailOptions);
{ False for unrelated Details, strict refusal for a malformed matching payload. }
function TryNyxMasterDetailEdit(const AEvent: TNyxEventInfo;
  out ARecord: TNyxMasterDetailRecord): Boolean;
function PrepareNyxMasterDetailBinding(const ADetail: INyxMasterDetail;
  const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  ARefresh: TNyxMasterDetailRefresh): INyxPreparedMasterDetailBinding;
{ Disabled preserves authored content and constrains only a recognized Edit face. }
function PrepareNyxMasterDetailDisabled(const ADetail: INyxMasterDetail;
  const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler): INyxPreparedMasterDetailBinding;
{ The same selected-record policy participates in ordinary coordinator mounting.
  Saved intent does not load this factory; the application supplies registration. }
function NewNyxMasterDetailCoordinatorFactory: INyxCoordinatorFactory;

implementation

uses
  nyx.types, nyx.state, nyx.binding.types, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.selection, nyx.interaction.policy;

const
  CRecordKind = 'nyx.master-detail.edit';
  CScalarNames: array[TNyxStateKind] of TNyxText = ('text', 'boolean', 'integer', 'number');

type
  { Forward owner declaration used only by its revocable weak port. }
  TDetailBinding = class;
  { Managed callbacks retain this port, never the runtime owner or renderer. }
  IDetailPort = interface(IInterface)
    ['{250DA1FB-F273-4526-81C2-845747902CD5}']
    function Acquire: INyxMasterDetailBinding;
    function Owner: TDetailBinding;
    procedure Revoke;
  end;
  { View subscription receiver and weak availability lease share one revocation. }
  TDetailPort = class(TInterfacedObject, IDetailPort)
  private
    FOwner: TDetailBinding;
  public
    constructor Create(AOwner: TDetailBinding);
    function Acquire: INyxMasterDetailBinding;
    function Owner: TDetailBinding;
    procedure Revoke;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;
  { Private marker prevents two accepted owners of the same Edit availability. }
  IDetailEnabled = interface(INyxRuntimeEnabledPolicy)
    ['{27FF1BD9-6063-4F93-8EDD-9E0984815EDE}']
    function Reserved: Boolean;
  end;
  { Additional runtime restriction preserves the authored Enabled property. }
  TDetailEnabled = class(TInterfacedObject, INyxRuntimeEnabledPolicy, IDetailEnabled)
  private
    FPort: IDetailPort;
  public
    constructor Create(const APort: IDetailPort);
    function GetAllowed: Boolean;
    function Reserved: Boolean;
  end;
  { One renderer-free prepared/live controller. Nodes belong to the retained
    runtime root; dirty callbacks remain dormant until accepted installation. }
  TDetailBinding = class(TInterfacedObject, INyxMasterDetailBinding,
    INyxCoordinatorBinding, INyxCoordinatorEventEnrichment)
  private
    FDetail: INyxMasterDetail;
    FOptions: TNyxMasterDetailOptions;
    FGeneration: INyxCancellationScope;
    FScheduler: INyxScheduler;
    FView: INyxCollectionView;
    FReadiness: INyxCollectionViewPublication;
    FToken: INyxCollectionViewSubscription;
    FCallback: TDetailPort;
    FPort: IDetailPort;
    FPolicy: INyxRuntimeEnabledPolicy;
    FTitleReservation: INyxRuntimeTextReservation;
    FDescriptionReservation: INyxRuntimeTextReservation;
    FProjection: INyxCoordinatorProjection;
    FProjectionLifetime: INyxCoordinatorProjectionLifetime;
    FRoleFacts: INyxCoordinatorRoleFacts;
    FScopedRefresh: INyxCoordinatorRefresh;
    FRootRole: TNyxControlRef;
    FMasterRole: TNyxControlRef;
    FTitleRole: TNyxControlRef;
    FDescriptionRole: TNyxControlRef;
    FEditRole: TNyxControlRef;
    FSource: TNyxCollectionRef;
    FScoped: Boolean;
    FRoot: TNyxNode;
    FParent: TNyxNode;
    FMaster: TNyxNode;
    FTitle: TNyxNode;
    FDescription: TNyxNode;
    FEdit: TNyxNode;
    FRootID: TNyxText;
    FEditID: TNyxText;
    FIntent: TNyxText;
    FSpec: TNyxText;
    FEditEnabled: TNyxText;
    FTitleDefault: TNyxText;
    FDescriptionDefault: TNyxText;
    FTitleProjection: TNyxText;
    FDescriptionProjection: TNyxText;
    FCurrent: TNyxMasterDetailRecord;
    FRefresh: TNyxMasterDetailRefresh;
    FConnected: Boolean;
    FDisabled: Boolean;
    FDirty: Boolean;
    FReading: Boolean;
    FRevoked: Boolean;
    FCleaned: Boolean;
    procedure CheckActive(APrepared: Boolean);
    procedure CheckNodes;
    procedure CheckProjection;
    procedure AdmitView;
    function ReadCurrent(APrepared: Boolean): TNyxMasterDetailRecord;
    procedure Project(const ARecord: TNyxMasterDetailRecord);
    procedure ViewChanged;
    procedure Revoke;
    procedure Cleanup;
  public
    constructor Create(const ADetail: INyxMasterDetail;
      const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
      const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      ARefresh: TNyxMasterDetailRefresh; ADisabled: Boolean);
    constructor CreateScoped(const AProjection: INyxCoordinatorProjection;
      const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
      const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh; ADisabled: Boolean);
    destructor Destroy; override;
    function GetConnected: Boolean;
    function GetCurrent: TNyxMasterDetailRecord;
    function CaptureEdit: TNyxMasterDetailRecord;
    function EnrichEdit(const AEvent: TNyxEventInfo): TNyxEventInfo;
    function Enrich(const AEvent: TNyxEventInfo): TNyxEventInfo;
    procedure Refresh;
    procedure Disconnect;
  end;
  { Single-use preparation keeps the lifetime controller and prospective router
    alive; installed retirement never revokes the accepted binding. }
  TPreparedDetail = class(TInterfacedObject, INyxPreparedMasterDetailBinding,
    INyxPreparedPublication, INyxPublicationReadiness)
  private
    FOwner: TDetailBinding;
    FBinding: INyxMasterDetailBinding;
    FEventView: INyxPreparedEventView;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
    procedure Cleanup;
  public
    constructor Create(const ADetail: INyxMasterDetail;
      const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
      const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      ARefresh: TNyxMasterDetailRefresh; ADisabled: Boolean);
    constructor CreateScoped(const AProjection: INyxCoordinatorProjection;
      const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
      const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh; ADisabled: Boolean);
    destructor Destroy; override;
    function GetBinding: INyxMasterDetailBinding;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Thin common-publication adapter preserves the original manual preparation
    interface. A disabled owner exposes only a base lifetime, not Edit commands. }
  TCoordinatorDetailPlan = class(TInterfacedObject, INyxPreparedCoordinator,
    INyxPreparedPublication, INyxPublicationReadiness, INyxCoordinatorBinding)
  private
    FOwner: TPreparedDetail;
    FPrepared: INyxPreparedMasterDetailBinding;
    FReadiness: INyxPublicationReadiness;
    FBinding: INyxCoordinatorBinding;
  public
    constructor Create(AOwner: TPreparedDetail;
      const APrepared: INyxPreparedMasterDetailBinding);
    function GetBinding: INyxCoordinatorBinding;
    function GetConnected: Boolean;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Explicit Pascal registration reuses the strict authored intent codec and
    scoped role authority; no renderer-specific behavior branch is required. }
  TDetailCoordinatorFactory = class(TInterfacedObject, INyxCoordinatorFactory)
  public
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;

function NyxMasterDetailOptions(const ATitleField,
  ADescriptionField: TNyxTextFieldRef): TNyxMasterDetailOptions;
begin
  Result := Default(TNyxMasterDetailOptions);
  Result.FTitle := NyxTextField(ATitleField.Name);
  Result.FDescription := NyxTextField(ADescriptionField.Name);
  Result.FDefined := True;
end;

procedure TNyxMasterDetailOptions.Validate;
begin

  if not FDefined then
  begin
    raise ENyxMasterDetail.Create('MasterDetail options require two typed text fields');
  end;
  NyxTextField(FTitle.Name);
  NyxTextField(FDescription.Name);
end;

function TNyxMasterDetailOptions.Copy: TNyxMasterDetailOptions;
begin
  Validate;
  Result := NyxMasterDetailOptions(FTitle, FDescription);
end;

function TNyxMasterDetailOptions.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('title', NyxData(FTitle.Name)),
    NyxField('description', NyxData(FDescription.Name))]);
end;

class function TNyxMasterDetailOptions.FromData(
  const AData: TNyxDataValue): TNyxMasterDetailOptions;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 3) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail options require an exact version-one object');
  end;
  Result := NyxMasterDetailOptions(NyxTextField(AData.Field('title').AsText),
    NyxTextField(AData.Field('description').AsText));
end;

class function TNyxMasterDetailIntent.Disabled: TNyxMasterDetailIntent;
begin
  Result := Default(TNyxMasterDetailIntent);
  Result.FMode := nmiDisabled;
end;

class function TNyxMasterDetailIntent.Configured(
  const AOptions: TNyxMasterDetailOptions): TNyxMasterDetailIntent;
begin
  Result := Default(TNyxMasterDetailIntent);
  Result.FOptions := AOptions.Copy;
  Result.FMode := nmiConfigured;
end;

function TNyxMasterDetailIntent.GetOptions: TNyxMasterDetailOptions;
begin

  if FMode <> nmiConfigured then
  begin
    raise ENyxMasterDetail.Create('Only configured MasterDetail intent has field options');
  end;
  Result := FOptions.Copy;
end;

procedure TNyxMasterDetailIntent.Validate;
begin
  case Ord(FMode) of
    Ord(nmiDisabled):
      begin
        Exit;
      end;
    Ord(nmiConfigured):
      begin
        FOptions.Validate;
      end;
  else
    begin
      raise ENyxMasterDetail.Create('MasterDetail inheritance is absence, not serialized intent');
    end;
  end;
end;

function TNyxMasterDetailIntent.Copy: TNyxMasterDetailIntent;
begin
  Result := Default(TNyxMasterDetailIntent);
  Result.FMode := FMode;

  if FMode = nmiConfigured then
  begin
    Result.FOptions := FOptions.Copy;
  end;
end;

function TNyxMasterDetailIntent.ToData: TNyxDataValue;
begin
  Validate;

  if FMode = nmiDisabled then
  begin
    Exit(NyxObject([NyxField('version', NyxData(1)),
      NyxField('mode', NyxData('disabled'))]));
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('mode', NyxData('configured')), NyxField('options', FOptions.ToData)]);
end;

class function TNyxMasterDetailIntent.FromData(
  const AData: TNyxDataValue): TNyxMasterDetailIntent;
var
  LMode: TNyxText;
begin

  if (AData.Kind <> ndObject) or (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail intent requires a version-one object');
  end;
  LMode := AData.Field('mode').AsText;

  if (LMode = 'disabled') and (AData.Count = 2) then
  begin
    Exit(Disabled);
  end;

  if (LMode = 'configured') and (AData.Count = 3) then
  begin
    Exit(Configured(TNyxMasterDetailOptions.FromData(AData.Field('options'))));
  end;
  raise ENyxMasterDetail.Create('Unknown or malformed MasterDetail intent');
end;

function HasNyxMasterDetailIntent(ANode: TNyxNode): Boolean;
begin
  Result := (ANode <> nil) and
    ANode.Extensions.Has(NyxExtension(NyxMasterDetailIntentKey));
end;

function GetNyxMasterDetailIntent(ANode: TNyxNode): TNyxMasterDetailIntent;
begin
  Result := Default(TNyxMasterDetailIntent);

  if HasNyxMasterDetailIntent(ANode) then
  begin
    Result := TNyxMasterDetailIntent.FromData(
      ANode.Extensions.Value(NyxExtension(NyxMasterDetailIntentKey)));
  end;
end;

procedure SetNyxMasterDetailIntent(const AControl: INyxControl;
  const AIntent: TNyxMasterDetailIntent);
var
  LControl: INyxControl;
  LIntent: TNyxMasterDetailIntent;
  LData: TNyxDataValue;
begin
  LControl := AControl;
  LIntent := AIntent.Copy;
  LData := LIntent.ToData;

  if LControl = nil then
  begin
    raise ENyxMasterDetail.Create('MasterDetail intent requires its authored control');
  end;
  LControl.Extensions.SetValue(NyxExtension(NyxMasterDetailIntentKey), LData);
end;

procedure InheritNyxMasterDetailIntent(const AControl: INyxControl);
var
  LControl: INyxControl;
begin
  LControl := AControl;

  if LControl = nil then
  begin
    raise ENyxMasterDetail.Create('MasterDetail inheritance requires its authored control');
  end;
  LControl.Extensions.Remove(NyxExtension(NyxMasterDetailIntentKey));
end;

procedure ValidateNyxMasterDetailIntent(ANode: TNyxNode);
var
  LIntent: TNyxMasterDetailIntent;
begin

  if not HasNyxMasterDetailIntent(ANode) then
  begin
    Exit;
  end;
  LIntent := GetNyxMasterDetailIntent(ANode);
  LIntent.Validate;

  if not ANode.IsRealized and ((ANode.Kind = NyxKindName(nkComponent)) or
    (ANode.Kind = NyxKindName(nkSlotOverride))) then
  begin
    Exit;
  end;

  if ANode.Kind <> NyxKindName(nkMasterDetail) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail intent requires its specialized target');
  end;
end;

function Role(ARoot: TNyxNode; const APath: TNyxText; AKind: TNyxKind): TNyxNode;
begin
  Result := ARoot.Part(NyxPart(APath));

  if (Result = nil) or (Result.Kind <> NyxKindName(AKind)) or
    (Result.ProjectionKind <> NyxKindName(AKind)) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail requires its exact role: ' + APath);
  end;
end;

procedure RequireUnboundText(ANode: TNyxNode);
var
  LSpec: TNyxBindingSpec;
begin

  if ANode.FindBinding(bpText, LSpec) and not LSpec.Cleared then
  begin
    raise ENyxMasterDetail.Create('MasterDetail text cannot have two projection authorities');
  end;
end;

procedure ValidateNyxMasterDetailOptions(ANode: TNyxNode;
  const AOptions: TNyxMasterDetailOptions);
var
  LMaster: TNyxNode;
  LTitle: TNyxNode;
  LDescription: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
begin
  AOptions.Validate;

  if (ANode = nil) or (ANode.Kind <> NyxKindName(nkMasterDetail)) or
    (ANode.Prop(NyxAttributeName(atCompound), 'false') <> 'true') then
  begin
    raise ENyxMasterDetail.Create('MasterDetail requires its specialized compound routing');
  end;
  LMaster := Role(ANode, 'master', nkList);
  LTitle := Role(ANode, 'detail/title', nkHeading);
  LDescription := Role(ANode, 'detail/description', nkLabel);
  Role(ANode, 'detail/edit', nkButton);
  RequireUnboundText(LTitle);
  RequireUnboundText(LDescription);

  if not LMaster.HasCollectionView then
  begin
    raise ENyxMasterDetail.Create('MasterDetail requires a bound master list');
  end;
  LSpec := LMaster.CollectionView.Copy;
  LSpec.Validate;

  if not LSpec.Defined or (LSpec.SelectionMode <> nsmSingle) or
    (LSpec.Count < 1) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail requires a single-selection list source');
  end;

end;

procedure ValidateNyxMasterDetailFields(const ASchema: TNyxCollectionSchema;
  const AOptions: TNyxMasterDetailOptions);
begin
  AOptions.Validate;
  ASchema.Validate;
  ASchema.Definition(AOptions.TitleField);
  ASchema.Definition(AOptions.DescriptionField);
end;

function TNyxMasterDetailRecord.GetRef: TNyxItemRef;
begin

  if not FDefined then
  begin
    raise ENyxMasterDetail.Create('MasterDetail has no visible selected record');
  end;
  Result := NyxItem(FRef.Collection, FRef.ID);
end;

function TNyxMasterDetailRecord.GetRevision: Integer;
begin
  GetRef;
  Result := FRevision;
end;

function TNyxMasterDetailRecord.GetItem: TNyxCollectionItem;
begin
  GetRef;
  Result := FItem.Copy;
end;

function TNyxMasterDetailRecord.Copy: TNyxMasterDetailRecord;
begin
  Result := Default(TNyxMasterDetailRecord);

  if FDefined then
  begin
    Result.FRef := GetRef;
    Result.FRevision := FRevision;
    Result.FItem := FItem.Copy;
    Result.FDefined := True;
  end;
end;

function ScalarData(const AValue: TNyxStateValue): TNyxDataValue;
begin
  AValue.Validate;
  case AValue.Kind of
    nskText:
      begin
        Result := NyxData(AValue.TextValue);
      end;
    nskBoolean:
      begin
        Result := NyxData(AValue.BooleanValue);
      end;
    nskInteger:
      begin
        Result := NyxData(AValue.IntegerValue);
      end;
    nskNumber:
      begin
        Result := NyxData(AValue.NumberValue);
      end;
  end;
end;

function TNyxMasterDetailRecord.ToData: TNyxDataValue;
var
  LFields: array of TNyxDataValue;
  LIndex: Integer;
  LValue: TNyxStateValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  SetLength(LFields, FItem.Count);
  for LIndex := 0 to FItem.Count - 1 do
  begin
    LValue := FItem.FieldValue(LIndex);
    LFields[LIndex] := NyxObject([NyxField('name', NyxData(FItem.FieldName(LIndex))),
      NyxField('type', NyxData(CScalarNames[LValue.Kind])),
      NyxField('value', ScalarData(LValue))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('kind', NyxData(CRecordKind)), NyxField('collection', NyxData(FRef.Collection.Name)),
    NyxField('id', NyxData(FRef.ID)), NyxField('revision', NyxData(FRevision)),
    NyxField('fields', NyxArray(LFields))]);
end;

class function TNyxMasterDetailRecord.FromData(
  const AData: TNyxDataValue): TNyxMasterDetailRecord;
var
  LFields: TNyxDataValue;
  LField: TNyxDataValue;
  LName: TNyxText;
  LType: TNyxText;
  LIndex: Integer;
  LPrior: Integer;
begin
  Result := Default(TNyxMasterDetailRecord);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 6) or
    (AData.Field('version').AsInteger <> 1) or
    (AData.Field('kind').AsText <> CRecordKind) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail record requires its exact version-one payload');
  end;
  Result.FRef := NyxItem(NyxCollection(AData.Field('collection').AsText),
    AData.Field('id').AsText);
  Result.FRevision := AData.Field('revision').AsInteger;

  if Result.FRevision < 0 then
  begin
    raise ENyxMasterDetail.Create('MasterDetail record revision cannot be negative');
  end;
  Result.FItem := NyxCollectionItem(Result.FRef);
  LFields := AData.Field('fields');

  if (LFields.Kind <> ndArray) or (LFields.Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail record fields exceed their declared boundary');
  end;
  for LIndex := 0 to LFields.Count - 1 do
  begin
    LField := LFields.Item(LIndex);

    if (LField.Kind <> ndObject) or (LField.Count <> 3) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail record requires exact typed fields');
    end;
    LName := LField.Field('name').AsText;
    LType := LField.Field('type').AsText;
    for LPrior := 0 to Result.FItem.Count - 1 do
    begin

      if Result.FItem.FieldName(LPrior) = LName then
      begin
        raise ENyxMasterDetail.Create('MasterDetail record contains a duplicate field');
      end;
    end;

    if LType = CScalarNames[nskText] then
    begin
      Result.FItem := Result.FItem.WithValue(NyxTextField(LName), LField.Field('value').AsText);
    end
    else if LType = CScalarNames[nskBoolean] then
    begin
      Result.FItem := Result.FItem.WithValue(NyxBooleanField(LName),
        LField.Field('value').AsBoolean);
    end
    else if LType = CScalarNames[nskInteger] then
    begin
      Result.FItem := Result.FItem.WithValue(NyxIntegerField(LName),
        LField.Field('value').AsInteger);
    end
    else if LType = CScalarNames[nskNumber] then
    begin
      Result.FItem := Result.FItem.WithValue(NyxNumberField(LName),
        LField.Field('value').AsNumber);
    end
    else
    begin
      raise ENyxMasterDetail.Create('MasterDetail record field has an unknown scalar family');
    end;
  end;
  Result.FDefined := True;
end;

function TryNyxMasterDetailEdit(const AEvent: TNyxEventInfo;
  out ARecord: TNyxMasterDetailRecord): Boolean;
var
  LData: TNyxDataValue;
  LMarker: TNyxDataValue;
  LIndex: Integer;
  LKind: Boolean;
begin
  ARecord := Default(TNyxMasterDetailRecord);
  Result := False;

  if (AEvent.Trigger <> ntClick) or not AEvent.HasDetails or
    (AEvent.Details.Kind <> ndObject) then
  begin
    Exit;
  end;
  LData := AEvent.Details;
  LKind := False;
  for LIndex := 0 to LData.Count - 1 do
  begin

    if LData.Key(LIndex) = 'kind' then
    begin
      LMarker := LData.Field('kind');
      LKind := (LMarker.Kind = ndText) and (LMarker.AsText = CRecordKind);
    end;
  end;

  if not LKind then
  begin
    Exit;
  end;
  ARecord := TNyxMasterDetailRecord.FromData(LData);
  Result := ARecord.Defined;
end;

constructor TDetailPort.Create(AOwner: TDetailBinding);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TDetailPort.Acquire: INyxMasterDetailBinding;
begin
  Result := nil;

  if FOwner <> nil then
  begin
    Result := FOwner;
  end;
end;

function TDetailPort.Owner: TDetailBinding;
begin
  Result := FOwner;
end;

procedure TDetailPort.Revoke;
begin
  FOwner := nil;
end;

procedure TDetailPort.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LLease: INyxMasterDetailBinding;
  LOwner: TDetailBinding;
begin
  LLease := Acquire;
  LOwner := FOwner;

  if (LLease <> nil) and (LOwner <> nil) then
  begin
    LOwner.ViewChanged;
  end;
end;

constructor TDetailEnabled.Create(const APort: IDetailPort);
begin
  inherited Create;
  FPort := APort;
end;

function TDetailEnabled.GetAllowed: Boolean;
var
  LLease: INyxMasterDetailBinding;
  LOwner: TDetailBinding;
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  Result := False;
  LLease := FPort.Acquire;
  LOwner := FPort.Owner;

  if (LLease = nil) or (LOwner = nil) or LOwner.FRevoked then
  begin
    Exit;
  end;
  LGeneration := LOwner.FGeneration;

  if LGeneration = nil then
  begin
    Exit;
  end;
  LCancelled := LGeneration.Cancelled;
  Result := not LCancelled and not LOwner.FRevoked and not LOwner.FDisabled and
    LOwner.FCurrent.Defined;
end;

function TDetailEnabled.Reserved: Boolean;
var
  LLease: INyxMasterDetailBinding;
  LOwner: TDetailBinding;
begin
  LLease := FPort.Acquire;
  LOwner := FPort.Owner;
  Result := (LLease <> nil) and (LOwner <> nil) and not LOwner.FRevoked;
end;

function OptionalEdit(ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;
  try
    Result := ARoot.Part(NyxPart('detail/edit'));
  except
    on E: ENyxModel do
    begin
      Result := nil;
    end;
  end;

  if (Result <> nil) and ((Result.Kind <> NyxKindName(nkButton)) or
    (Result.ProjectionKind <> NyxKindName(nkButton))) then
  begin
    Result := nil;
  end;
end;

function IntentText(ANode: TNyxNode): TNyxText;
begin
  Result := '';

  if HasNyxMasterDetailIntent(ANode) then
  begin
    Result := ANode.Extensions.Value(NyxExtension(NyxMasterDetailIntentKey)).ToJSON;
  end;
end;

constructor TDetailBinding.Create(const ADetail: INyxMasterDetail;
  const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  ARefresh: TNyxMasterDetailRefresh; ADisabled: Boolean);
var
  LViews: INyxCollectionBindings;
  LEventView: INyxPreparedEventView;
  LScheduler: INyxScheduler;
  LExisting: IDetailEnabled;
  LReadiness: INyxCollectionViewPublication;
  LIndex: Integer;
  LRecord: TNyxMasterDetailRecord;
begin
  inherited Create;
  FDetail := ADetail;
  LViews := AViews;
  LEventView := AEventView;
  FScheduler := AScheduler;
  LScheduler := FScheduler;
  FRefresh := ARefresh;
  FDisabled := ADisabled;

  if not FDisabled then
  begin
    FOptions := AOptions.Copy;
  end;

  if (FDetail = nil) or (LEventView = nil) or (LScheduler = nil) then
  begin
    raise ENyxMasterDetail.Create('Prepared MasterDetail needs target, event plan and scheduler');
  end;
  FGeneration := LEventView.Generation;
  FRoot := FDetail.Node;
  LScheduler.RequireUI;
  CheckActive(True);

  if (FRoot = nil) or not FRoot.IsRealized or
    (FRoot.Kind <> NyxKindName(nkMasterDetail)) then
  begin
    raise ENyxMasterDetail.Create('Prepared MasterDetail requires its realized specialized root');
  end;
  FParent := FRoot.Parent;
  FRootID := FRoot.ID;
  FIntent := IntentText(FRoot);
  FCallback := TDetailPort.Create(Self);
  FPort := FCallback;
  FEdit := OptionalEdit(FRoot);

  if FEdit <> nil then
  begin
    FEditID := FEdit.ID;
    FEditEnabled := FEdit.StoredProp(NyxAttributeName(atEnabled), 'true');
    for LIndex := 0 to FEdit.EnabledPolicyCount - 1 do
    begin

      if Supports(FEdit.EnabledPolicyAt(LIndex), IDetailEnabled, LExisting) and
        LExisting.Reserved then
      begin
        raise ENyxMasterDetail.Create('MasterDetail already reserves this Edit face');
      end;
      CheckActive(True);
    end;
    FPolicy := TDetailEnabled.Create(FPort);
    FEdit.AttachEnabledPolicy(FPolicy);
  end;

  if not FDisabled then
  begin
    ValidateNyxMasterDetailOptions(FRoot, FOptions);
    FMaster := Role(FRoot, 'master', nkList);
    FTitle := Role(FRoot, 'detail/title', nkHeading);
    FDescription := Role(FRoot, 'detail/description', nkLabel);
    FTitleDefault := FTitle.Prop(NyxAttributeName(atText));
    FDescriptionDefault := FDescription.Prop(NyxAttributeName(atText));
    FTitleProjection := FTitle.StoredProp(NyxAttributeName(atText));
    FDescriptionProjection := FDescription.StoredProp(NyxAttributeName(atText));
    FSpec := FMaster.CollectionView.ToData.ToJSON;
    FSource := FMaster.CollectionView.Key;
    { Exclusive runtime ownership is independent of equal initial text. Reserve
      both roles before the first projection or outward view admission. }
    try
      FTitleReservation := FTitle.ReserveRuntimeText;
      FDescriptionReservation := FDescription.ReserveRuntimeText;
    except
      on E: ENyxModel do
      begin
        raise ENyxMasterDetail.Create('MasterDetail text role is already reserved: ' +
          TNyxText(E.Message));
      end;
    end;

    if LViews = nil then
    begin
      raise ENyxMasterDetail.Create('Prepared MasterDetail requires its actual collection views');
    end;
    FView := LViews.ViewFor(FMaster.ID);
    CheckActive(True);

    if (FView = nil) or not Supports(FView, INyxCollectionViewPublication, LReadiness) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail view requires explicit publication readiness');
    end;
    { The field retains its own lease after the query local is finalized. }
    FReadiness := LReadiness;
    AdmitView;
    FToken := FView.Subscribe(FCallback.Changed);
    CheckActive(True);
    LRecord := ReadCurrent(True);
    Project(LRecord);
  end;
  CheckNodes;
end;

constructor TDetailBinding.CreateScoped(const AProjection: INyxCoordinatorProjection;
  const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh; ADisabled: Boolean);
var
  LViews: INyxCollectionBindings;
  LEventView: INyxPreparedEventView;
  LScheduler: INyxScheduler;
  LRoleFacts: INyxCoordinatorRoleFacts;
  LLifetime: INyxCoordinatorProjectionLifetime;
  LReadiness: INyxCollectionViewPublication;
  LInfo: TNyxCoordinatorRoleInfo;
  LSpec: TNyxCollectionViewSpec;
  LRecord: TNyxMasterDetailRecord;

  function RequiredRole(const APath: TNyxText; AKind: TNyxKind): TNyxControlRef;
  var
    LRoleInfo: TNyxCoordinatorRoleInfo;
  begin
    Result := FProjection.Part(NyxPart(APath));
    LRoleInfo := FRoleFacts.Describe(Result);
    CheckActive(True);

    if (LRoleInfo.Kind.Name <> NyxKindName(AKind)) or
      (LRoleInfo.ProjectionKind.Name <> NyxKindName(AKind)) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail requires its exact role: ' + APath);
    end;
  end;

begin
  inherited Create;
  FScoped := True;
  FProjection := AProjection;
  FScopedRefresh := ARefresh;
  LViews := AViews;
  LEventView := AEventView;
  FScheduler := AScheduler;
  LScheduler := FScheduler;
  FDisabled := ADisabled;

  if not FDisabled then
  begin
    FOptions := AOptions.Copy;
  end;

  if (FProjection = nil) or (LEventView = nil) or (LScheduler = nil) or
    not Supports(FProjection, INyxCoordinatorRoleFacts, LRoleFacts) or
    not Supports(FProjection, INyxCoordinatorProjectionLifetime, LLifetime) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail needs checked coordinator roles and retirement');
  end;
  { Query locals and retained fields each need an independent owning lease.
    Publish neither scoped capability until the complete original guard admits. }
  FRoleFacts := LRoleFacts;
  FProjectionLifetime := LLifetime;
  FGeneration := LEventView.Generation;
  LScheduler.RequireUI;
  CheckActive(True);
  FRootRole := FProjection.Root;
  FRootID := FRootRole.ID;
  LInfo := FRoleFacts.Describe(FRootRole);
  CheckActive(True);

  if (LInfo.Kind.Name <> NyxKindName(nkMasterDetail)) or
    (not FDisabled and not LInfo.Compound) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail requires its specialized compound routing');
  end;
  FCallback := TDetailPort.Create(Self);
  FPort := FCallback;
  try
    FEditRole := FProjection.Part(NyxPart('detail/edit'));
  except
    on ENyxModel do
    begin
      FEditRole := Default(TNyxControlRef);
    end;
  end;

  if FEditRole.ID <> '' then
  begin
    LInfo := FRoleFacts.Describe(FEditRole);
    CheckActive(True);

    if (LInfo.Kind.Name <> NyxKindName(nkButton)) or
      (LInfo.ProjectionKind.Name <> NyxKindName(nkButton)) then
    begin
      FEditRole := Default(TNyxControlRef);
    end;
  end;
  FEditID := FEditRole.ID;

  if not FDisabled then
  begin
    FOptions.Validate;
    FMasterRole := RequiredRole('master', nkList);
    FTitleRole := RequiredRole('detail/title', nkHeading);
    FDescriptionRole := RequiredRole('detail/description', nkLabel);
    RequiredRole('detail/edit', nkButton);
    LInfo := FRoleFacts.Describe(FTitleRole);
    FTitleDefault := LInfo.EffectiveText;
    LInfo := FRoleFacts.Describe(FDescriptionRole);
    FDescriptionDefault := LInfo.EffectiveText;
    CheckActive(True);

    if not FProjection.TryCollectionSpec(FMasterRole, LSpec) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail requires a bound master list');
    end;
    LSpec.Validate;

    if (LSpec.SelectionMode <> nsmSingle) or (LSpec.Count < 1) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail requires a single-selection list source');
    end;
    FSpec := LSpec.ToData.ToJSON;
    FSource := LSpec.Key;

    if LViews = nil then
    begin
      raise ENyxMasterDetail.Create('MasterDetail requires its actual collection views');
    end;
    FView := LViews.ViewFor(FMasterRole.ID);
    CheckActive(True);

    if (FView = nil) or not Supports(FView, INyxCollectionViewPublication, LReadiness) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail view requires explicit publication readiness');
    end;
    FReadiness := LReadiness;
    AdmitView;
    FToken := FView.Subscribe(FCallback.Changed);
    CheckActive(True);
    LRecord := ReadCurrent(True);
    Project(LRecord);
  end
  else if FEditRole.ID <> '' then
  begin
    FProjection.Enabled(FEditRole, False);
  end;
  CheckNodes;
end;

destructor TDetailBinding.Destroy;
begin
  try
    Cleanup;
  except
    { Explicit cleanup reports failures; destruction still revokes every port. }
  end;
  inherited Destroy;
end;

procedure TDetailBinding.CheckActive(APrepared: Boolean);
var
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LGeneration := FGeneration;

  if FRevoked or FCleaned or (LGeneration = nil) or
    (APrepared and FDirty) or (not APrepared and not FConnected) then
  begin
    raise ENyxMasterDetailCancelled.Create('MasterDetail is changed or retired');
  end;
  LCancelled := LGeneration.Cancelled;

  if LCancelled or FRevoked or FCleaned or (FGeneration <> LGeneration) or
    (APrepared and FDirty) or (not APrepared and not FConnected) then
  begin
    raise ENyxMasterDetailCancelled.Create('MasterDetail generation is no longer current');
  end;
end;

procedure TDetailBinding.CheckNodes;
var
  LIndex: Integer;
  LFound: Boolean;
begin
  CheckActive(True);

  if FScoped then
  begin
    { The enclosing common participant fences canonical roles after every group
      validator. Its child final fence must make no outward projection call. }
    Exit;
  end;

  if not FRoot.IsRealized or (FRoot.ID <> FRootID) or (FRoot.Parent <> FParent) or
    (FRoot.Kind <> NyxKindName(nkMasterDetail)) or
    (IntentText(FRoot) <> FIntent) or (OptionalEdit(FRoot) <> FEdit) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail target or intent changed during preparation');
  end;

  if FEdit <> nil then
  begin

    if (FEdit.ID <> FEditID) or
      (FEdit.StoredProp(NyxAttributeName(atEnabled), 'true') <> FEditEnabled) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail Edit authority changed');
    end;
    LFound := False;
    for LIndex := 0 to FEdit.EnabledPolicyCount - 1 do
    begin
      LFound := LFound or (FEdit.EnabledPolicyAt(LIndex) = FPolicy);
    end;

    if not LFound then
    begin
      raise ENyxMasterDetail.Create('MasterDetail availability policy was detached');
    end;
  end;

  if not FDisabled then
  begin
    ValidateNyxMasterDetailOptions(FRoot, FOptions);

    if (Role(FRoot, 'master', nkList) <> FMaster) or
      (Role(FRoot, 'detail/title', nkHeading) <> FTitle) or
      (Role(FRoot, 'detail/description', nkLabel) <> FDescription) or
      (FMaster.CollectionView.ToData.ToJSON <> FSpec) or
      (FTitle.StoredProp(NyxAttributeName(atText)) <> FTitleProjection) or
      (FDescription.StoredProp(NyxAttributeName(atText)) <> FDescriptionProjection) or
      (FTitleReservation = nil) or not FTitleReservation.Active or
      (FDescriptionReservation = nil) or not FDescriptionReservation.Active then
    begin
      raise ENyxMasterDetail.Create('MasterDetail behavioral roles changed');
    end;
  end;
  CheckActive(True);
end;

procedure TDetailBinding.CheckProjection;
var
  LInfo: TNyxCoordinatorRoleInfo;
begin
  CheckActive(not FConnected);

  if FScoped then
  begin
    { Describe is an outward, revocable capability. Use it during ordinary
      admission/input, never the child's final local readiness fence. }
    LInfo := FRoleFacts.Describe(FRootRole);
    CheckActive(not FConnected);

    if (LInfo.Kind.Name <> NyxKindName(nkMasterDetail)) or
      (not FDisabled and not LInfo.Compound) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail scoped role authority changed');
    end;
  end;
end;

procedure TDetailBinding.AdmitView;
var
  LView: INyxCollectionView;
  LSnapshot: INyxCollectionSnapshot;
  LKey: TNyxText;
begin
  LView := FView;
  CheckActive(not FConnected);
  CheckProjection;

  if not FScoped then
  begin
    ValidateNyxMasterDetailOptions(FRoot, FOptions);
  end;
  LKey := FSource.Name;

  if LView.Spec.ToData.ToJSON <> FSpec then
  begin
    raise ENyxMasterDetail.Create('MasterDetail view and authored master spec disagree');
  end;
  CheckActive(not FConnected);

  if LView.Projection <> cpList then
  begin
    raise ENyxMasterDetail.Create('MasterDetail requires the actual list projection');
  end;
  CheckActive(not FConnected);
  LSnapshot := LView.Snapshot;
  CheckActive(not FConnected);

  if (LSnapshot = nil) or (LSnapshot.Key.Name <> LKey) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail snapshot belongs to another collection');
  end;
  CheckActive(not FConnected);
  ValidateNyxMasterDetailFields(LSnapshot.Schema, FOptions);
  CheckActive(not FConnected);
end;

function TDetailBinding.ReadCurrent(APrepared: Boolean): TNyxMasterDetailRecord;
var
  LView: INyxCollectionView;
  LSnapshot: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LItem: TNyxItemRef;
begin
  Result := Default(TNyxMasterDetailRecord);
  LView := FView;
  CheckActive(APrepared);

  if FReading then
  begin
    raise ENyxMasterDetail.Create('MasterDetail does not admit reentrant snapshot capture');
  end;
  FReading := True;
  try
    LSnapshot := LView.Snapshot;
    CheckActive(APrepared);
    LSelection := LView.Selection;
    CheckActive(APrepared);

    if (LSnapshot = nil) or (LSelection = nil) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail requires defined snapshot and selection');
    end;

    if (LSelection.Key.Name <> LSnapshot.Key.Name) or
      (LSelection.DataRevision <> LSnapshot.Revision) or (LSelection.Count > 1) then
    begin
      raise ENyxMasterDetail.Create('MasterDetail selection source/revision is not exact SINGLE');
    end;
    CheckActive(APrepared);

    if LSelection.Count = 0 then
    begin
      Exit;
    end;
    LItem := LSelection.ItemAt(0);
    CheckActive(APrepared);

    if LItem.Collection.Name <> FSource.Name then
    begin
      raise ENyxMasterDetail.Create('MasterDetail selected identity belongs to another source');
    end;

    if not LSnapshot.Has(LItem) then
    begin
      { Query/page-hidden membership is owned by the view, not rewritten here. }
      CheckActive(APrepared);
      Exit;
    end;
    CheckActive(APrepared);
    Result.FItem := LSnapshot.Item(LItem).Copy;
    CheckActive(APrepared);
    Result.FRef := NyxItem(LItem.Collection, LItem.ID);
    Result.FRevision := LSnapshot.Revision;
    CheckActive(APrepared);
    Result.FDefined := True;
  finally
    FReading := False;
  end;
end;

procedure TDetailBinding.Project(const ARecord: TNyxMasterDetailRecord);
var
  LRecord: TNyxMasterDetailRecord;
  LTitle: TNyxText;
  LDescription: TNyxText;
begin
  LRecord := ARecord.Copy;
  LTitle := FTitleDefault;
  LDescription := FDescriptionDefault;

  if LRecord.Defined then
  begin
    LTitle := LRecord.FItem.GetValue(FOptions.TitleField);
    LDescription := LRecord.FItem.GetValue(FOptions.DescriptionField);
  end;
  { Only this independent runtime projection changes. Authored nodes, rows,
    selection and scalar state are never written by a detail presentation. }

  if FScoped then
  begin
    FProjection.Text(FTitleRole, LTitle);
    CheckActive(not FConnected);
    FProjection.Text(FDescriptionRole, LDescription);
    CheckActive(not FConnected);
    FProjection.Enabled(FEditRole, LRecord.Defined);
    CheckActive(not FConnected);
  end
  else
  begin
    FTitle.SetProp(NyxAttributeName(atText), LTitle);
    FDescription.SetProp(NyxAttributeName(atText), LDescription);
  end;
  FTitleProjection := LTitle;
  FDescriptionProjection := LDescription;
  FCurrent := LRecord;
end;

procedure TDetailBinding.ViewChanged;
var
  LKeepAlive: INyxMasterDetailBinding;
begin
  LKeepAlive := Self;

  if FRevoked or FCleaned then
  begin
    Exit;
  end;

  if not FConnected then
  begin
    FDirty := True;
    Exit;
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  Refresh;
end;

function TDetailBinding.GetConnected: Boolean;
var
  LKeepAlive: INyxMasterDetailBinding;
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LKeepAlive := Self;
  LGeneration := FGeneration;
  Result := FConnected and not FRevoked and (LGeneration <> nil);

  if Result then
  begin
    LCancelled := LGeneration.Cancelled;
    Result := not LCancelled and FConnected and not FRevoked and
      (FGeneration = LGeneration);
  end;
end;

function TDetailBinding.GetCurrent: TNyxMasterDetailRecord;
var
  LKeepAlive: INyxMasterDetailBinding;
begin
  LKeepAlive := Self;
  Result := FCurrent.Copy;
end;

function TDetailBinding.CaptureEdit: TNyxMasterDetailRecord;
var
  LKeepAlive: INyxMasterDetailBinding;
  LScheduler: INyxScheduler;
  LCurrent: TNyxMasterDetailRecord;
begin
  LKeepAlive := Self;
  LScheduler := FScheduler;
  CheckActive(False);
  LScheduler.RequireUI;
  CheckActive(False);

  if FDisabled then
  begin
    raise ENyxMasterDetail.Create('Disabled MasterDetail has no Edit record');
  end;
  CheckNodes;
  CheckProjection;
  LCurrent := ReadCurrent(False);
  CheckNodes;
  CheckProjection;

  if not LCurrent.Defined or not FCurrent.Defined or
    (LCurrent.FRevision <> FCurrent.FRevision) or
    not LCurrent.FItem.SameItem(FCurrent.FItem) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail visible record changed before Edit capture');
  end;
  Result := LCurrent.Copy;
end;

function TDetailBinding.EnrichEdit(const AEvent: TNyxEventInfo): TNyxEventInfo;
var
  LKeepAlive: INyxMasterDetailBinding;
  LRecord: TNyxMasterDetailRecord;
begin
  LKeepAlive := Self;
  Result := AEvent.Copy;

  if (Result.Trigger <> ntClick) or (Result.OriginID <> FEditID) or
    (Result.SourceID <> FRootID) then
  begin
    Exit;
  end;

  if Result.HasDetails then
  begin
    raise ENyxMasterDetail.Create('MasterDetail Edit cannot overwrite structured Details');
  end;
  LRecord := CaptureEdit;
  Result.Details := LRecord.ToData;
  Result.HasDetails := True;
end;

function TDetailBinding.Enrich(const AEvent: TNyxEventInfo): TNyxEventInfo;
begin
  Result := EnrichEdit(AEvent);
end;

procedure TDetailBinding.Refresh;
var
  LKeepAlive: INyxMasterDetailBinding;
  LScheduler: INyxScheduler;
  LRefresh: TNyxMasterDetailRefresh;
  LScopedRefresh: INyxCoordinatorRefresh;
  LRecord: TNyxMasterDetailRecord;
begin
  LKeepAlive := Self;
  LScheduler := FScheduler;
  CheckActive(False);
  LScheduler.RequireUI;
  CheckActive(False);

  if FDisabled then
  begin
    Exit;
  end;
  CheckNodes;
  AdmitView;
  LRecord := ReadCurrent(False);
  CheckNodes;
  CheckActive(False);
  Project(LRecord);
  LRefresh := FRefresh;
  LScopedRefresh := FScopedRefresh;

  if LScopedRefresh <> nil then
  begin
    { The guarded final-renderer port may retire this complete binding. }
    LScopedRefresh.Request;
    Exit;
  end;

  if Assigned(LRefresh) then
  begin
    { Last borrowed call: the receiver may retire this entire view. }
    LRefresh;
  end;
end;

procedure TDetailBinding.Revoke;
var
  LLifetime: INyxCoordinatorProjectionLifetime;
begin
  LLifetime := FProjectionLifetime;
  FRevoked := True;
  FConnected := False;
  FRefresh := nil;

  if FTitleReservation <> nil then
  begin
    FTitleReservation.Revoke;
  end;

  if FDescriptionReservation <> nil then
  begin
    FDescriptionReservation.Revoke;
  end;

  if FPort <> nil then
  begin
    FPort.Revoke;
  end;

  if LLifetime <> nil then
  begin
    { Explicit binding retirement revokes its scope too. This only changes
      owned flags; it never recurses through the enclosing mount. }
    LLifetime.Revoke;
  end;
end;

procedure TDetailBinding.Cleanup;
var
  LLifetime: INyxCoordinatorProjectionLifetime;
  LProjection: INyxCoordinatorProjection;
  LFailure: TNyxText;
begin
  LLifetime := FProjectionLifetime;
  LProjection := FProjection;
  Revoke;

  if FCleaned then
  begin
    Exit;
  end;
  FCleaned := True;
  LFailure := '';

  if FToken <> nil then
  begin
    try
      FToken.Disconnect;
    except
      on E: Exception do
      begin
        LFailure := TNyxText(E.Message);
      end;
    end;
  end;

  if (FEdit <> nil) and (FPolicy <> nil) then
  begin
    try
      FEdit.DetachEnabledPolicy(FPolicy);
    except
      on E: Exception do
      begin

        if LFailure = '' then
        begin
          LFailure := TNyxText(E.Message);
        end;
      end;
    end;
  end;

  if (FTitle <> nil) and (FTitleReservation <> nil) then
  begin
    try
      FTitle.ReleaseRuntimeText(FTitleReservation);
    except
      on E: Exception do
      begin

        if LFailure = '' then
        begin
          LFailure := TNyxText(E.Message);
        end;
      end;
    end;
  end;

  if (FDescription <> nil) and (FDescriptionReservation <> nil) then
  begin
    try
      FDescription.ReleaseRuntimeText(FDescriptionReservation);
    except
      on E: Exception do
      begin

        if LFailure = '' then
        begin
          LFailure := TNyxText(E.Message);
        end;
      end;
    end;
  end;

  if LLifetime <> nil then
  begin
    try
      { The shared scope owns the scoped text claims and AND-enabled policy.
        Releasing this binding must detach them now, not at view retirement. }
      LLifetime.Disconnect;
    except
      on E: Exception do
      begin

        if LFailure = '' then
        begin
          LFailure := TNyxText(E.Message);
        end;
      end;
    end;
  end;
  FToken := nil;
  FReadiness := nil;
  FView := nil;
  FPolicy := nil;
  FTitleReservation := nil;
  FDescriptionReservation := nil;
  FScopedRefresh := nil;
  FRoleFacts := nil;
  FProjectionLifetime := nil;
  FProjection := nil;
  FEdit := nil;
  FMaster := nil;
  FTitle := nil;
  FDescription := nil;
  FRoot := nil;
  FParent := nil;
  FDetail := nil;
  FGeneration := nil;
  FScheduler := nil;
  FCallback := nil;
  FPort := nil;
  LProjection := nil;

  if LFailure <> '' then
  begin
    raise ENyxMasterDetail.Create('MasterDetail disconnected; cleanup failed: ' + LFailure);
  end;
end;

procedure TDetailBinding.Disconnect;
var
  LKeepAlive: INyxMasterDetailBinding;
begin
  LKeepAlive := Self;
  Cleanup;
end;

constructor TPreparedDetail.Create(const ADetail: INyxMasterDetail;
  const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  ARefresh: TNyxMasterDetailRefresh; ADisabled: Boolean);
begin
  inherited Create;
  FEventView := AEventView;
  FOwner := TDetailBinding.Create(ADetail, AOptions, AViews, FEventView,
    AScheduler, ARefresh, ADisabled);
  FBinding := FOwner;
end;

constructor TPreparedDetail.CreateScoped(const AProjection: INyxCoordinatorProjection;
  const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh; ADisabled: Boolean);
begin
  inherited Create;
  FEventView := AEventView;
  FOwner := TDetailBinding.CreateScoped(AProjection, AOptions, AViews, FEventView,
    AScheduler, ARefresh, ADisabled);
  FBinding := FOwner;
end;

procedure TPreparedDetail.Cleanup;
begin

  if FOwner <> nil then
  begin
    FOwner.Cleanup;
  end;
end;

destructor TPreparedDetail.Destroy;
begin
  try
    Cleanup;
  except
    { Installed/abandoned cleanup is defensive; explicit Disconnect reports. }
  end;
  inherited Destroy;
end;

function TPreparedDetail.GetBinding: INyxMasterDetailBinding;
begin
  Result := nil;

  if (FOwner <> nil) and not FOwner.FDisabled then
  begin
    Result := FBinding;
  end;
end;

procedure TPreparedDetail.Revoke;
begin

  if FOwner <> nil then
  begin
    FOwner.Revoke;
  end;
end;

procedure TPreparedDetail.Disconnect;
var
  LKeepAlive: INyxPreparedMasterDetailBinding;
begin
  LKeepAlive := Self;
  Cleanup;
end;

procedure TPreparedDetail.Validate;
var
  LKeepAlive: INyxPreparedMasterDetailBinding;
  LScheduler: INyxScheduler;
  LReadiness: INyxCollectionViewPublication;
begin
  LKeepAlive := Self;

  if FValidated or FInstalled or FRetired then
  begin
    raise ENyxMasterDetail.Create('MasterDetail preparation is single use');
  end;
  FOwner.CheckActive(True);
  LScheduler := FOwner.FScheduler;
  LReadiness := FOwner.FReadiness;
  LScheduler.RequireUI;
  FOwner.CheckActive(True);

  if not FOwner.FDisabled then
  begin
    FOwner.AdmitView;

    if not LReadiness.PublicationReady then
    begin
      raise ENyxMasterDetail.Create('MasterDetail view is busy before publication');
    end;
    FOwner.CheckActive(True);
  end;
  FValidated := True;
  CheckReady;
end;

procedure TPreparedDetail.CheckReady;
var
  LKeepAlive: INyxPreparedMasterDetailBinding;
begin
  LKeepAlive := Self;

  if not FValidated or FInstalled or FRetired then
  begin
    raise ENyxMasterDetail.Create('MasterDetail readiness requires an admitted preparation');
  end;
  FOwner.CheckNodes;
end;

procedure TPreparedDetail.Install;
begin

  if FValidated and not FInstalled and not FRetired and not FOwner.FRevoked then
  begin
    FOwner.FConnected := True;
    FInstalled := True;
  end;
end;

procedure TPreparedDetail.Notify;
begin
  { First presentation was rendered while dormant; activation invokes no host. }
end;

procedure TPreparedDetail.Retire;
var
  LKeepAlive: INyxPreparedMasterDetailBinding;
begin
  LKeepAlive := Self;

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if not FInstalled then
  begin
    try
      Cleanup;
    except
      { Every lease was already attempted; Retire does not throw. }
    end;
  end;
  FEventView := nil;
end;

function PrepareNyxMasterDetailBinding(const ADetail: INyxMasterDetail;
  const AOptions: TNyxMasterDetailOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  ARefresh: TNyxMasterDetailRefresh): INyxPreparedMasterDetailBinding;
begin
  Result := TPreparedDetail.Create(ADetail, AOptions, AViews,
    AEventView, AScheduler, ARefresh, False);
end;

function PrepareNyxMasterDetailDisabled(const ADetail: INyxMasterDetail;
  const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler): INyxPreparedMasterDetailBinding;
begin
  Result := TPreparedDetail.Create(ADetail, Default(TNyxMasterDetailOptions),
    nil, AEventView, AScheduler, nil, True);
end;

constructor TCoordinatorDetailPlan.Create(AOwner: TPreparedDetail;
  const APrepared: INyxPreparedMasterDetailBinding);
var
  LReadiness: INyxPublicationReadiness;
  LBinding: INyxCoordinatorBinding;
begin
  inherited Create;
  FOwner := AOwner;
  FPrepared := APrepared;

  if (FOwner = nil) or (FPrepared = nil) or
    not Supports(FPrepared, INyxPublicationReadiness, LReadiness) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail coordinator needs prepared readiness');
  end;
  { Keep the retained fields separate from each temporary query output. }
  FReadiness := LReadiness;

  if not FOwner.FOwner.FDisabled and
    not Supports(FOwner.FBinding, INyxCoordinatorBinding, LBinding) then
  begin
    raise ENyxMasterDetail.Create('MasterDetail must preserve its exact typed binding');
  end;
  FBinding := LBinding;
end;

function TCoordinatorDetailPlan.GetBinding: INyxCoordinatorBinding;
begin
  Result := FBinding;

  if Result = nil then
  begin
    { A disabled preparation has a lifetime owner but no selected-record API. }
    Result := Self;
  end;
end;

function TCoordinatorDetailPlan.GetConnected: Boolean;
var
  LLease: INyxPreparedCoordinator;
begin
  LLease := Self;
  Result := FOwner.FOwner.GetConnected;
end;

procedure TCoordinatorDetailPlan.Revoke;
begin
  FPrepared.Revoke;
end;

procedure TCoordinatorDetailPlan.Disconnect;
var
  LLease: INyxPreparedCoordinator;
  LPrepared: INyxPreparedMasterDetailBinding;
begin
  LLease := Self;
  LPrepared := FPrepared;
  LPrepared.Disconnect;
end;

procedure TCoordinatorDetailPlan.Validate;
var
  LLease: INyxPreparedCoordinator;
  LPrepared: INyxPreparedMasterDetailBinding;
begin
  LLease := Self;
  LPrepared := FPrepared;
  LPrepared.Validate;
end;

procedure TCoordinatorDetailPlan.CheckReady;
var
  LLease: INyxPreparedCoordinator;
  LReadiness: INyxPublicationReadiness;
begin
  LLease := Self;
  LReadiness := FReadiness;
  LReadiness.CheckReady;
end;

procedure TCoordinatorDetailPlan.Install;
begin
  FPrepared.Install;
end;

procedure TCoordinatorDetailPlan.Notify;
begin
  FPrepared.Notify;
end;

procedure TCoordinatorDetailPlan.Retire;
begin
  FPrepared.Retire;
end;

function TDetailCoordinatorFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
var
  LProjection: INyxCoordinatorProjection;
  LViews: INyxCollectionBindings;
  LView: INyxPreparedEventView;
  LScheduler: INyxScheduler;
  LRefresh: INyxCoordinatorRefresh;
  LIntent: TNyxMasterDetailIntent;
  LOptions: TNyxMasterDetailOptions;
  LOwner: TPreparedDetail;
  LPrepared: INyxPreparedMasterDetailBinding;
begin
  LProjection := AProjection;
  LViews := AViews;
  LView := AView;
  LScheduler := AScheduler;
  LRefresh := ARefresh;
  LIntent := TNyxMasterDetailIntent.FromData(AIntent);
  LOptions := Default(TNyxMasterDetailOptions);

  if LIntent.Mode = nmiConfigured then
  begin
    LOptions := LIntent.Options;
  end;
  LOwner := TPreparedDetail.CreateScoped(LProjection, LOptions, LViews,
    LView, LScheduler, LRefresh, LIntent.Mode = nmiDisabled);
  LPrepared := LOwner;
  Result := TCoordinatorDetailPlan.Create(LOwner, LPrepared);
end;

function NewNyxMasterDetailCoordinatorFactory: INyxCoordinatorFactory;
begin
  Result := TDetailCoordinatorFactory.Create;
end;

end.
