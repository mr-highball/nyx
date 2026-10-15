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

unit nyx.programs.canvas;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.resources, nyx.canvas.geometry, nyx.canvas.scene,
  nyx.canvas.workspace, nyx.programs.types, nyx.programs.contract, nyx.programs,
  nyx.scheduler;

type
  { Closed built-in scene/camera/resource vocabulary. Names are emitted only at
    the explicit Pascal-language boundary; application code selects these enums. }
  TNyxCanvasProgramRoutine = (ncprCount, ncprItem, ncprX, ncprY, ncprWidth,
    ncprHeight, ncprVisible, ncprMove, ncprSize, ncprShow, ncprCameraX,
    ncprCameraY, ncprCameraZoom, ncprSetCamera, ncprResourceCount, ncprResource,
    ncprResourceName, ncprResourceLocale, ncprResourceText, ncprResourceJSON,
    ncprResourceBytes, ncprResourceNumber);

  { Prepared UI-only complete scene/camera result. It owns a revision-pinned
    plan, never a renderer/application. A layout view admits target allocations
    before committing that plan and camera together. Dropping it changes nothing. }
  INyxCanvasProgramLayout = interface(IInterface)
    ['{E291C5CB-6B87-4E85-A8E6-000000000204}']
    function GetPlan: INyxCanvasLayoutPlan;
    function GetCamera: TNyxCanvasCamera;
    property Plan: INyxCanvasLayoutPlan read GetPlan;
    property Camera: TNyxCanvasCamera read GetCamera;
  end;

  { Immutable worker-safe vocabulary and copied world/resource input. Opaque item
    and resource handles are stable within this capture, foreign to every other
    capture. No live scene, tree, view, resource registry or application is kept.
    Prepare belongs to UI/authoring, using the exact scene token/revision. It
    validates the entire journal and combines repeated edits chronologically.
    Scalar outputs refuse here: use CaptureNyxCanvasProgramDelivery from the
    publication unit to map every output into a coordinated state/scene frame. }
  INyxCanvasProgramSnapshot = interface(INyxProgramQueries)
    ['{E291C5CB-6B87-4E85-A8E6-000000000205}']
    function GetContract: TNyxProgramContract;
    function GetCamera: TNyxCanvasCamera;
    function GetSceneRevision: Integer;
    function Prepare(const AResult: INyxProgramResult; const AScene: INyxCanvasScene;
      AMaximumProposals: Integer = 1024): INyxCanvasProgramLayout;
    property Contract: TNyxProgramContract read GetContract;
    property Camera: TNyxCanvasCamera read GetCamera;
    property SceneRevision: Integer read GetSceneRevision;
  end;

  { Optional UI/authoring-only geometry preparation for a multi-owner result. This does
    not publish or consume scalar outputs: the owning delivery must map and
    validate every output before admitting its state/scene/view group. Keeping
    it separate preserves Prepare's original refusal to drop scalar results. }
  INyxCanvasProgramFrameSnapshot = interface(IInterface)
    ['{E291C5CB-6B87-4E85-A8E6-000000000208}']
    function PrepareFrameGeometry(const AResult: INyxProgramResult;
      const AScene: INyxCanvasScene; AMaximumProposals: Integer): INyxCanvasProgramLayout;
  end;

{ Fresh default declarations; they confer no execution grants or live authority. }
function NyxCanvasProgramContract: TNyxProgramContract;
function NyxCanvasProgramSymbol(ARoutine: TNyxCanvasProgramRoutine): TNyxProgramSymbolRef;
{ Capture on the scene's authoring thread. Resource membership is explicit and
  copied as materialized typed values, under the caller's cumulative byte limit.
  Hosted definitions refuse: resolve them with the existing loader/cache first
  and pass those materialized definitions, never perform I/O inside a query. }
function CaptureNyxCanvasProgram(const AScene: INyxCanvasScene;
  const ACamera: TNyxCanvasCamera; const AResources: INyxResources = nil;
  AMaximumResourceBytes: Integer = 16777216): INyxCanvasProgramSnapshot;
{ UI-only delivery of one complete result. Exact view presentation revision,
  scene identity/revision and camera are checked before target staging. Scope
  reentry/cancellation is checked by the view immediately before publication.
  Scheduling/capture of application event state remains owned by the caller. }
function ApplyNyxCanvasProgram(const AView: INyxCanvasWorkspaceView;
  const ASnapshot: INyxCanvasProgramSnapshot; const AResult: INyxProgramResult;
  AExpectedPresentation: Integer; const ACancellation: INyxCancellationScope = nil;
  AMaximumProposals: Integer = 1024): Boolean;

implementation

uses nyx.data, nyx.bytes, nyx.resource.sources, nyx.text.index;

const
  RoutineNames: array[TNyxCanvasProgramRoutine] of TNyxText = (
    'SceneCount', 'SceneItem', 'ItemX', 'ItemY', 'ItemWidth', 'ItemHeight',
    'ItemVisible', 'MoveItem', 'SizeItem', 'ShowItem', 'CameraX', 'CameraY',
    'CameraZoom', 'SetCamera', 'ResourceCount', 'ResourceAt', 'ResourceName',
    'ResourceLocale', 'ResourceText', 'ResourceJSON', 'ResourceBytes', 'ResourceNumber');

type
  { Private typed handle inspection. The token is a capture-only identity object
    with no backreference to its snapshot, scene, registry or application. }
  ICapturedHandle = interface(IInterface)
    ['{E291C5CB-6B87-4E85-A8E6-000000000206}']
    function GetToken: IInterface;
    function GetIndex: Integer;
  end;

  { Stable immutable item/resource capability. Source cannot construct this
    interface from an index, name or another capture's handle. }
  TCapturedHandle = class(TInterfacedObject, INyxProgramHandle, ICapturedHandle)
  public
    Token: IInterface;
    Index: Integer;
    Resource: Boolean;
    function GetValueType: TNyxProgramType;
    function GetToken: IInterface;
    function GetIndex: Integer;
  end;

  { Owned materialized resource value, including exact locale metadata. A class
    keeps managed interfaces out of pas2js record fields on both targets. }
  TCapturedResource = class
  public
    Name: TNyxText;
    Locale: TNyxText;
    Value: INyxProgramValue;
  end;

  { Private immutable capture. All vectors, query signatures and resource values
    are sealed in construction; query calls allocate only invocation-local data.
    The opaque scene identity is independent of scene/control lifetime. }
  TCanvasProgramSnapshot = class(TInterfacedObject, INyxCanvasProgramSnapshot,
    INyxCanvasProgramFrameSnapshot)
  private
    FSceneIdentity: IInterface;
    FToken: IInterface;
    FItems: INyxCanvasSnapshot;
    FCamera: TNyxCanvasCamera;
    FContract: TNyxProgramContract;
    FNames: TNyxTextIndex;
    FResources: array of TCapturedResource;
    FItemHandles: array of INyxProgramHandle;
    FResourceHandles: array of INyxProgramHandle;
    function MakeHandle(AIndex: Integer; AResource: Boolean): INyxProgramHandle;
    function HandleIndex(const AValue: INyxProgramValue; AResource: Boolean): Integer;
    function RoutineIndex(const AName: TNyxProgramSymbolRef): TNyxCanvasProgramRoutine;
    procedure CheckArguments(const ARoutine: TNyxProgramRoutine;
      const AArguments: INyxProgramArguments);
    function PrepareGeometry(const AResult: INyxProgramResult; const AScene: INyxCanvasScene;
      AMaximumProposals: Integer; AFrame: Boolean): INyxCanvasProgramLayout;
  public
    constructor Create(const AScene: INyxCanvasScene; const ACamera: TNyxCanvasCamera;
      const AResources: INyxResources; AMaximumResourceBytes: Integer);
    destructor Destroy; override;
    function Query(const ARoutine: TNyxProgramSymbolRef;
      const AArguments: INyxProgramArguments): INyxProgramValue;
    function GetContract: TNyxProgramContract;
    function GetCamera: TNyxCanvasCamera;
    function GetSceneRevision: Integer;
    function Prepare(const AResult: INyxProgramResult; const AScene: INyxCanvasScene;
      AMaximumProposals: Integer = 1024): INyxCanvasProgramLayout;
    function PrepareFrameGeometry(const AResult: INyxProgramResult;
      const AScene: INyxCanvasScene; AMaximumProposals: Integer): INyxCanvasProgramLayout;
  end;

  { Complete admitted geometry/camera recipe. Its plan alone retains the scene;
    ordinary worker execution never owns this UI-only result object. }
  TCanvasProgramLayout = class(TInterfacedObject, INyxCanvasProgramLayout)
  public
    LayoutPlan: INyxCanvasLayoutPlan;
    CameraValue: TNyxCanvasCamera;
    function GetPlan: INyxCanvasLayoutPlan;
    function GetCamera: TNyxCanvasCamera;
  end;

procedure Require(ACondition: Boolean; AKind: TNyxProgramDiagnosticKind;
  const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxProgram.Create(AKind, Default(TNyxProgramSpan), AMessage);
  end;
end;

function ItemType(AResource: Boolean): TNyxProgramType;
begin

  if AResource then
  begin
    Result := NyxProgramHandleType(NyxProgramSymbol('TNyxCanvasResource'));
  end
  else
  begin
    Result := NyxProgramHandleType(NyxProgramSymbol('TNyxCanvasItem'));
  end;
end;

function NyxCanvasProgramSymbol(ARoutine: TNyxCanvasProgramRoutine): TNyxProgramSymbolRef;
begin
  Result := NyxProgramSymbol(RoutineNames[ARoutine]);
end;

function NyxCanvasProgramContract: TNyxProgramContract;
var
  LKind: TNyxCanvasProgramRoutine;
  LRoutine: TNyxProgramRoutine;
  LType: TNyxProgramType;
  LCapability: TNyxProgramCapability;
begin
  Result := NyxProgramContract;
  for LKind := Low(TNyxCanvasProgramRoutine) to High(TNyxCanvasProgramRoutine) do
  begin
    LCapability := npcSceneRead;
    LType := NyxProgramType(npkNumber);
    case LKind of
      ncprCount: LType := NyxProgramType(npkInteger);
      ncprItem: LType := ItemType(False);
      ncprVisible: LType := NyxProgramType(npkBoolean);
      ncprCameraX, ncprCameraY, ncprCameraZoom: LCapability := npcCameraRead;
      ncprResourceCount, ncprResource, ncprResourceName, ncprResourceLocale,
      ncprResourceText, ncprResourceJSON, ncprResourceBytes, ncprResourceNumber:
      begin
        LCapability := npcResourceRead;
        case LKind of
          ncprResourceCount: LType := NyxProgramType(npkInteger);
          ncprResource: LType := ItemType(True);
          ncprResourceName, ncprResourceLocale, ncprResourceText: LType := NyxProgramType(npkText);
          ncprResourceJSON: LType := NyxProgramType(npkJSON);
          ncprResourceBytes: LType := NyxProgramType(npkBytes);
          else LType := NyxProgramType(npkNumber);
        end;
      end;
      else
      begin
        { Geometry queries retain the default Double/SceneRead signature. }
      end;
    end;

    if LKind in [ncprMove, ncprSize, ncprShow, ncprSetCamera] then
    begin
      LCapability := npcSceneEdit;

      if LKind = ncprSetCamera then
      begin
        LCapability := npcCameraEdit;
      end;
      LRoutine := NyxProgramPropose(NyxCanvasProgramSymbol(LKind),
        NyxProgramCapability(LCapability));
    end
    else
    begin
      LRoutine := NyxProgramQuery(NyxCanvasProgramSymbol(LKind), LType,
        NyxProgramCapability(LCapability));
    end;

    if LKind in [ncprItem, ncprResource] then
    begin
      LRoutine := LRoutine.Argument(NyxProgramSymbol('Index'), NyxProgramType(npkInteger));
    end
    else if LKind in [ncprX, ncprY, ncprWidth, ncprHeight, ncprVisible,
      ncprMove, ncprSize, ncprShow] then
    begin
      LRoutine := LRoutine.Argument(NyxProgramSymbol('Item'), ItemType(False));
    end
    else if LKind in [ncprResourceName, ncprResourceLocale, ncprResourceText,
      ncprResourceJSON, ncprResourceBytes, ncprResourceNumber] then
    begin
      LRoutine := LRoutine.Argument(NyxProgramSymbol('Resource'), ItemType(True));
    end;
    case LKind of
      ncprMove: LRoutine := LRoutine.Argument(NyxProgramSymbol('X'), NyxProgramType(npkNumber))
        .Argument(NyxProgramSymbol('Y'), NyxProgramType(npkNumber));
      ncprSize: LRoutine := LRoutine.Argument(NyxProgramSymbol('Width'), NyxProgramType(npkNumber))
        .Argument(NyxProgramSymbol('Height'), NyxProgramType(npkNumber));
      ncprShow: LRoutine := LRoutine.Argument(NyxProgramSymbol('Visible'), NyxProgramType(npkBoolean));
      ncprSetCamera: LRoutine := LRoutine.Argument(NyxProgramSymbol('X'), NyxProgramType(npkNumber))
        .Argument(NyxProgramSymbol('Y'), NyxProgramType(npkNumber))
        .Argument(NyxProgramSymbol('Zoom'), NyxProgramType(npkNumber));
      else
      begin
        { Other routines have no additional positional arguments. }
      end;
    end;
    Result := Result.WithRoutine(LRoutine);
  end;
end;

function TCapturedHandle.GetValueType: TNyxProgramType;
begin
  Result := ItemType(Resource);
end;

function TCapturedHandle.GetToken: IInterface;
begin
  Result := Token;
end;

function TCapturedHandle.GetIndex: Integer;
begin
  Result := Index;
end;

function TCanvasProgramSnapshot.MakeHandle(AIndex: Integer;
  AResource: Boolean): INyxProgramHandle;
var
  LHandle: TCapturedHandle;
begin
  LHandle := TCapturedHandle.Create;
  Result := LHandle;
  LHandle.Token := FToken;
  LHandle.Index := AIndex;
  LHandle.Resource := AResource;
end;

constructor TCanvasProgramSnapshot.Create(const AScene: INyxCanvasScene;
  const ACamera: TNyxCanvasCamera; const AResources: INyxResources;
  AMaximumResourceBytes: Integer);
var
  LLayouts: INyxCanvasSceneLayouts;
  LKind: TNyxCanvasProgramRoutine;
  LResource: INyxResourceDefinition;
  LCatalog: INyxResources;
  LIndex: Integer;
  LBytes: TNyxTextBytes;
begin
  inherited Create;
  Require((AScene <> nil) and ACamera.Defined and (AMaximumResourceBytes > 0),
    npdType, 'Canvas program capture requires a scene, camera and positive resource budget');
  Require(Supports(AScene, INyxCanvasSceneLayouts, LLayouts), npdCapability,
    'The scene does not expose prepared layout admission');
  FSceneIdentity := LLayouts.GetLayoutIdentity;
  Require(FSceneIdentity <> nil, npdCapability, 'The scene requires an opaque layout identity');
  FItems := AScene.Snapshot;
  Require((FItems <> nil) and (FItems.Count >= 0) and
    (FItems.Count <= MaximumNyxCanvasItems), npdLimit, 'Canvas capture requires a bounded scene snapshot');
  FCamera := ACamera;
  FToken := TInterfacedObject.Create;
  FContract := NyxCanvasProgramContract;
  FNames := TNyxTextIndex.Create;
  for LKind := Low(TNyxCanvasProgramRoutine) to High(TNyxCanvasProgramRoutine) do
  begin
    FNames.AddFirst(NyxProgramIdentifierKey(RoutineNames[LKind]), Ord(LKind));
  end;
  SetLength(FItemHandles, FItems.Count);
  for LIndex := 0 to FItems.Count - 1 do
  begin
    FItemHandles[LIndex] := MakeHandle(LIndex, False);
  end;
  LBytes := 0;

  if AResources <> nil then
  begin
    LCatalog := AResources.Clone;
    Require((LCatalog <> nil) and (LCatalog.Count >= 0) and
      (LCatalog.Count <= NyxMaximumResources), npdLimit, 'Program resource membership exceeds its bound');
    SetLength(FResources, LCatalog.Count);
    SetLength(FResourceHandles, LCatalog.Count);
    for LIndex := 0 to High(FResources) do
    begin
      LResource := LCatalog.Definition(LCatalog.Reference(LIndex), LCatalog.Locale(LIndex));
      Require(LResource.Source.Kind = rskEmbedded, npdCapability,
        'Materialize hosted resources through the loader before program capture');
      Require(LResource.ByteCount <= AMaximumResourceBytes - LBytes, npdLimit,
        'Program resource capture exceeds its byte budget');
      FResources[LIndex] := TCapturedResource.Create;
      FResources[LIndex].Name := LCatalog.Reference(LIndex).Name;
      FResources[LIndex].Locale := LCatalog.Locale(LIndex).Name;
      case LResource.Kind of
        nrkText: FResources[LIndex].Value := NyxProgramValue(LResource.Text);
        nrkJSON: FResources[LIndex].Value := NyxProgramValue(LResource.Data);
        nrkBinary, nrkImage: FResources[LIndex].Value := NyxProgramValue(LResource.Bytes);
      end;
      LBytes := LBytes + FResources[LIndex].Value.ByteSize +
        NyxUTF8ByteCount(FResources[LIndex].Name) + NyxUTF8ByteCount(FResources[LIndex].Locale);
      Require(LBytes <= AMaximumResourceBytes, npdLimit, 'Program resource capture exceeds its byte budget');
      FResourceHandles[LIndex] := MakeHandle(LIndex, True);
    end;
  end;
  Require(AScene.Revision = FItems.Revision, npdStale, 'Scene changed during program capture');
end;

destructor TCanvasProgramSnapshot.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FResources) do
  begin
    FResources[LIndex].Free;
  end;
  FNames.Free;
  inherited Destroy;
end;

function TCanvasProgramSnapshot.GetContract: TNyxProgramContract;
begin
  Result := FContract.Copy;
end;

function TCanvasProgramSnapshot.GetCamera: TNyxCanvasCamera;
begin
  Result := FCamera;
end;

function TCanvasProgramSnapshot.GetSceneRevision: Integer;
begin
  Result := FItems.Revision;
end;

function TCanvasProgramSnapshot.RoutineIndex(
  const AName: TNyxProgramSymbolRef): TNyxCanvasProgramRoutine;
var
  LIndex: Integer;
begin
  LIndex := FNames.IndexOf(NyxProgramIdentifierKey(AName.Name));
  Require(LIndex >= 0, npdName, 'Unknown canvas program host routine');
  Result := TNyxCanvasProgramRoutine(LIndex);
end;

procedure TCanvasProgramSnapshot.CheckArguments(const ARoutine: TNyxProgramRoutine;
  const AArguments: INyxProgramArguments);
var
  LIndex: Integer;
  LValue: INyxProgramValue;
begin
  Require((AArguments <> nil) and (AArguments.Count = ARoutine.ParameterCount),
    npdType, 'Canvas host calls require their complete declared arguments');
  for LIndex := 0 to AArguments.Count - 1 do
  begin
    LValue := AArguments.Item(LIndex);
    Require((LValue <> nil) and LValue.ValueType.Same(ARoutine.Parameter(LIndex).ValueType),
      npdType, 'Canvas host argument has a different declared family');
  end;
end;

function TCanvasProgramSnapshot.HandleIndex(const AValue: INyxProgramValue;
  AResource: Boolean): Integer;
var
  LHandle: ICapturedHandle;
begin
  Require((AValue <> nil) and AValue.ValueType.Same(ItemType(AResource)) and
    Supports(AValue.AsHandle, ICapturedHandle, LHandle), npdCapability,
    'Canvas host requires an opaque captured capability');
  Require(LHandle.GetToken = FToken, npdCapability, 'A handle belongs to another canvas program capture');
  Result := LHandle.GetIndex;

  if AResource then
  begin
    Require((Result >= 0) and (Result < Length(FResources)), npdCapability, 'Resource handle is retired');
  end
  else
  begin
    Require((Result >= 0) and (Result < FItems.Count), npdCapability, 'Item handle is retired');
  end;
end;

function TCanvasProgramSnapshot.Query(const ARoutine: TNyxProgramSymbolRef;
  const AArguments: INyxProgramArguments): INyxProgramValue;
var
  LKind: TNyxCanvasProgramRoutine;
  LIndex: Integer;
  LBounds: TNyxCanvasBounds;
  LSignature: TNyxProgramRoutine;
begin
  LKind := RoutineIndex(ARoutine);
  LSignature := FContract.Routine(Ord(LKind));
  Require(LSignature.Kind = nprQuery, npdType, 'A proposed canvas edit is not a live query');
  CheckArguments(LSignature, AArguments);
  LIndex := -1;

  if LKind in [ncprX, ncprY, ncprWidth, ncprHeight, ncprVisible] then
  begin
    LIndex := HandleIndex(AArguments.Item(0), False);
    LBounds := FItems.Items[LIndex].Placement.Bounds;
  end
  else if LKind in [ncprResourceName, ncprResourceLocale, ncprResourceText,
    ncprResourceJSON, ncprResourceBytes, ncprResourceNumber] then
  begin
    LIndex := HandleIndex(AArguments.Item(0), True);
  end;
  case LKind of
    ncprCount: Result := NyxProgramValue(FItems.Count);
    ncprResourceCount: Result := NyxProgramValue(Length(FResources));
    ncprItem, ncprResource:
    begin
      LIndex := AArguments.Item(0).AsInteger;

      if LKind = ncprItem then
      begin
        Require((LIndex >= 0) and (LIndex < Length(FItemHandles)), npdName, 'Scene item index is unavailable');
        Result := NyxProgramValue(FItemHandles[LIndex]);
      end
      else
      begin
        Require((LIndex >= 0) and (LIndex < Length(FResourceHandles)), npdName, 'Resource index is unavailable');
        Result := NyxProgramValue(FResourceHandles[LIndex]);
      end;
    end;
    ncprX: Result := NyxProgramValue(LBounds.Left);
    ncprY: Result := NyxProgramValue(LBounds.Top);
    ncprWidth: Result := NyxProgramValue(LBounds.Width);
    ncprHeight: Result := NyxProgramValue(LBounds.Height);
    ncprVisible: Result := NyxProgramValue(FItems.Items[LIndex].Placement.IsVisible);
    ncprCameraX: Result := NyxProgramValue(FCamera.WorldCenter.X);
    ncprCameraY: Result := NyxProgramValue(FCamera.WorldCenter.Y);
    ncprCameraZoom: Result := NyxProgramValue(FCamera.Zoom);
    ncprResourceName: Result := NyxProgramValue(FResources[LIndex].Name);
    ncprResourceLocale: Result := NyxProgramValue(FResources[LIndex].Locale);
    ncprResourceText: Result := NyxProgramValue(FResources[LIndex].Value.AsText);
    ncprResourceJSON: Result := NyxProgramValue(FResources[LIndex].Value.AsJSON);
    ncprResourceBytes: Result := NyxProgramValue(FResources[LIndex].Value.AsBytes);
    ncprResourceNumber: Result := NyxProgramValue(FResources[LIndex].Value.AsJSON.AsNumber);
    else
    begin
      Require(False, npdType, 'Unsupported canvas query');
      Result := nil;
    end;
  end;
end;

function TCanvasProgramSnapshot.Prepare(const AResult: INyxProgramResult;
  const AScene: INyxCanvasScene; AMaximumProposals: Integer): INyxCanvasProgramLayout;
begin
  Result := PrepareGeometry(AResult, AScene, AMaximumProposals, False);
end;

function TCanvasProgramSnapshot.PrepareFrameGeometry(const AResult: INyxProgramResult;
  const AScene: INyxCanvasScene; AMaximumProposals: Integer): INyxCanvasProgramLayout;
begin
  Result := PrepareGeometry(AResult, AScene, AMaximumProposals, True);
end;

function TCanvasProgramSnapshot.PrepareGeometry(const AResult: INyxProgramResult;
  const AScene: INyxCanvasScene; AMaximumProposals: Integer;
  AFrame: Boolean): INyxCanvasProgramLayout;
var
  LLayouts: INyxCanvasSceneLayouts;
  LResult: TCanvasProgramLayout;
  LPlacements: array of TNyxCanvasPlacement;
  LChanged: array of Boolean;
  LChanges: array of TNyxCanvasPlacementChange;
  LProposal: INyxProgramProposal;
  LOutputs: INyxProgramValues;
  LArguments: INyxProgramArguments;
  LRoutine: TNyxProgramRoutine;
  LDeclared: TNyxProgramRoutine;
  LKind: TNyxCanvasProgramRoutine;
  LCamera: TNyxCanvasCamera;
  LBounds: TNyxCanvasBounds;
  LCount: Integer;
  LIndex: Integer;
  LItem: Integer;
  LPart: Integer;
  LProposals: Integer;
begin
  Require((AResult <> nil) and (AScene <> nil) and (AMaximumProposals > 0),
    npdType, 'Canvas result preparation requires initialized owners and limits');
  Require(Supports(AScene, INyxCanvasSceneLayouts, LLayouts) and
    (LLayouts.GetLayoutIdentity = FSceneIdentity) and (AScene.Revision = FItems.Revision),
    npdStale, 'Canvas result belongs to another or changed scene');
  LOutputs := AResult.Outputs;
  Require((LOutputs <> nil) and (AFrame or (LOutputs.Count = 0)), npdType,
    'Canvas publication cannot silently drop scalar outputs');
  LProposals := AResult.ProposalCount;
  Require((LProposals >= 0) and (LProposals <= AMaximumProposals), npdLimit,
    'Canvas result proposal count exceeds its admission budget');
  SetLength(LPlacements, FItems.Count);
  SetLength(LChanged, FItems.Count);
  LCamera := FCamera;
  for LIndex := 0 to LProposals - 1 do
  begin
    LProposal := AResult.Proposal(LIndex);
    Require(LProposal <> nil, npdType, 'Canvas proposal is missing');
    try
      LRoutine := LProposal.Routine;
      LKind := RoutineIndex(LRoutine.Name);
      LDeclared := FContract.Routine(Ord(LKind));
      Require((LDeclared.Kind = nprProposal) and (LRoutine.Kind = nprProposal) and
        LRoutine.ResultType.Same(LDeclared.ResultType) and
        LRoutine.Capability.Same(LDeclared.Capability) and
        (LRoutine.ParameterCount = LDeclared.ParameterCount), npdType,
        'Canvas proposal does not match a declared edit signature');
      for LPart := 0 to LRoutine.ParameterCount - 1 do
      begin
        Require(LRoutine.Parameter(LPart).Name.Same(LDeclared.Parameter(LPart).Name) and
          LRoutine.Parameter(LPart).ValueType.Same(LDeclared.Parameter(LPart).ValueType),
          npdType, 'Canvas proposal parameter metadata changed');
      end;
      LArguments := LProposal.Arguments;
      CheckArguments(LDeclared, LArguments);

      if LKind = ncprSetCamera then
      begin
        LCamera := LCamera.ZoomAt(LArguments.Item(2).AsNumber,
          NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2))
          .AtCenter(NyxCanvasPoint(LArguments.Item(0).AsNumber, LArguments.Item(1).AsNumber));
        Continue;
      end;
      LItem := HandleIndex(LArguments.Item(0), False);

      if not LChanged[LItem] then
      begin
        LPlacements[LItem] := LLayouts.PlacementFor(FItems.Items[LItem].Item);
        LChanged[LItem] := True;
      end;
      LBounds := LPlacements[LItem].Bounds;
      case LKind of
        ncprMove: LPlacements[LItem] := LPlacements[LItem].AtBounds(NyxCanvasBounds(
          LArguments.Item(1).AsNumber, LArguments.Item(2).AsNumber, LBounds.Width, LBounds.Height));
        ncprSize: LPlacements[LItem] := LPlacements[LItem].AtBounds(NyxCanvasBounds(
          LBounds.Left, LBounds.Top, LArguments.Item(1).AsNumber, LArguments.Item(2).AsNumber));
        ncprShow: LPlacements[LItem] := LPlacements[LItem].Visible(LArguments.Item(1).AsBoolean);
        else Require(False, npdType, 'Unknown canvas edit');
      end;
    except
      on E: ENyxCanvasGeometry do
      begin
        raise ENyxProgram.Create(npdType, LProposal.Span,
          'Canvas proposal contains invalid world geometry');
      end;
    end;
  end;
  LCount := 0;
  for LIndex := 0 to High(LChanged) do
  begin

    if LChanged[LIndex] then
    begin
      Inc(LCount);
    end;
  end;
  SetLength(LChanges, LCount);
  LCount := 0;
  for LIndex := 0 to High(LChanged) do
  begin

    if LChanged[LIndex] then
    begin
      LChanges[LCount] := NyxCanvasPlace(FItems.Items[LIndex].Item, LPlacements[LIndex]);
      Inc(LCount);
    end;
  end;
  { Alternative result/argument getters may reenter. The exact owner must still
    match after the entire journal has been decoded, before staging a plan. }
  Require((LLayouts.GetLayoutIdentity = FSceneIdentity) and (AScene.Revision = FItems.Revision),
    npdStale, 'Scene changed during canvas result preparation');
  LResult := TCanvasProgramLayout.Create;
  Result := LResult;
  LResult.LayoutPlan := LLayouts.PrepareLayout(LChanges, FItems.Revision);
  LResult.CameraValue := LCamera;
end;

function TCanvasProgramLayout.GetPlan: INyxCanvasLayoutPlan;
begin
  Result := LayoutPlan;
end;

function TCanvasProgramLayout.GetCamera: TNyxCanvasCamera;
begin
  Result := CameraValue;
end;

function CaptureNyxCanvasProgram(const AScene: INyxCanvasScene;
  const ACamera: TNyxCanvasCamera; const AResources: INyxResources;
  AMaximumResourceBytes: Integer): INyxCanvasProgramSnapshot;
begin
  Result := TCanvasProgramSnapshot.Create(AScene, ACamera, AResources, AMaximumResourceBytes);
end;

function SameCamera(const ALeft, ARight: TNyxCanvasCamera): Boolean;
begin
  Result := ALeft.Defined and ARight.Defined and
    (ALeft.WorldCenter.X = ARight.WorldCenter.X) and (ALeft.WorldCenter.Y = ARight.WorldCenter.Y) and
    (ALeft.Zoom = ARight.Zoom) and (ALeft.ViewportWidth = ARight.ViewportWidth) and
    (ALeft.ViewportHeight = ARight.ViewportHeight) and
    (ALeft.Limits.Minimum = ARight.Limits.Minimum) and (ALeft.Limits.Maximum = ARight.Limits.Maximum);
end;

function ApplyNyxCanvasProgram(const AView: INyxCanvasWorkspaceView;
  const ASnapshot: INyxCanvasProgramSnapshot; const AResult: INyxProgramResult;
  AExpectedPresentation: Integer; const ACancellation: INyxCancellationScope;
  AMaximumProposals: Integer): Boolean;
var
  LRuntime: INyxCanvasRuntimeView;
  LView: INyxCanvasLayoutView;
  LLayout: INyxCanvasProgramLayout;
begin
  Require((AView <> nil) and (ASnapshot <> nil), npdType, 'Canvas delivery requires a view and capture');
  Require(Supports(AView, INyxCanvasRuntimeView, LRuntime) and
    Supports(AView, INyxCanvasLayoutView, LView), npdCapability,
    'The workspace does not expose atomic runtime layout admission');
  Result := AView.Connected;

  if not Result then
  begin
    Exit;
  end;
  Require((LView.PresentationRevision = AExpectedPresentation) and
    SameCamera(AView.Camera, ASnapshot.Camera), npdStale, 'Canvas presentation changed since capture');
  LLayout := ASnapshot.Prepare(AResult, LRuntime.Scene, AMaximumProposals);
  { The target performs another expected-presentation/cancellation check after
    staging all physical mounts. Scene identity is sealed into this plan. }
  Result := LView.ApplyLayout(LLayout.Plan, LLayout.Camera, AExpectedPresentation, ACancellation);
end;

end.
