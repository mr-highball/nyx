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
unit nyx.selection.summary;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.data, nyx.controls, nyx.coordinators,
  nyx.collections.view.types;

type
  { Creator-owned compound keeps ordinary Card layout and specialized access to
    its typed parts. Its portable kind remains Card; behavior is an explicit
    caller registration keyed by the separately saved intent. }
  INyxSelectionSummary = interface(INyxCard)
    ['{2BA6BA45-E00E-4CF4-95DA-B0FF0170E03D}']
    function GetItemsList: INyxList;
    function GetCountLabel: INyxLabel;
    function GetClearButton: INyxButton;
    property ItemsList: INyxList read GetItemsList;
    property CountLabel: INyxLabel read GetCountLabel;
    property ClearButton: INyxButton read GetClearButton;
  end;
  { A creator can expose its own managed command interface. The generic driver
    preserves this identity; commands still check this mount's own generation.
    SelectedCount describes the last published presentation, even after Close. }
  INyxSelectionSummaryBinding = interface(INyxCoordinatorBinding)
    ['{CCD17766-8157-4FB1-A8C8-84196012C44E}']
    function GetSelectedCount: Integer;
    procedure Clear;
    property SelectedCount: Integer read GetSelectedCount;
  end;

function SelectionSummaryIntent: TNyxExtensionRef;
function SelectionSummaryItems: TNyxPartRef;
function SelectionSummaryCount: TNyxPartRef;
function SelectionSummaryClear: TNyxPartRef;
function SelectionSummaryCaption(ACount: Integer): TNyxText;
{ These ordinary typed controls are the whole visual component. The registered
  behavior supplies runtime presentation; it never replaces the List's owner. }
function NewSelectionSummary(const AID: TNyxText;
  const ASpec: TNyxCollectionViewSpec): INyxSelectionSummary;
function NewSelectionSummaryFactory: INyxCoordinatorFactory;
procedure RegisterSelectionSummary(const ARegistry: INyxCoordinatorRegistry);

implementation

uses
  SysUtils, nyx.model, nyx.collections, nyx.collections.bindings,
  nyx.collections.view, nyx.collections.selection, nyx.events,
  nyx.behavior, nyx.scheduler, nyx.publication;

type
  { Public Nyx control classes support this ordinary Pascal subclass. Named
    parts remain extensible; a missing/replaced incompatible part refuses. }
  TSelectionSummaryCard = class(TNyxCard, INyxSelectionSummary)
  public
    function GetItemsList: INyxList;
    function GetCountLabel: INyxLabel;
    function GetClearButton: INyxButton;
  end;
  { Forward declaration for the weak callback receiver. }
  TSummaryBinding = class;
  { Event observers are retained by the router; collection observers borrow
    this receiver. Neither path retains the binding/controller back from it. }
  TSummaryPort = class(TNyxEventObserver)
  public
    Owner: TSummaryBinding;
    function Acquire: INyxSelectionSummaryBinding;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;
  { The same managed object owns preparation and the accepted custom commands.
    It retains only scoped capabilities, views and tokens, never a node/widget.
    Local revocation precedes disconnecting subscriptions. Dirty dormant views
    refuse publication instead of projecting after hidden controls were built.
    Construction is inert. The factory acquires a managed lease before Initialize
    can call outward or enter a method that temporarily retains Self; this also
    protects initialization under pas2js's initially zero interface count. }
  TSummaryBinding = class(TInterfacedObject, INyxSelectionSummaryBinding,
    INyxCoordinatorBinding, INyxPreparedCoordinator, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FProjection: INyxCoordinatorProjection;
    FRefresh: INyxCoordinatorRefresh;
    FScheduler: INyxScheduler;
    FEventView: INyxPreparedEventView;
    FGeneration: INyxCancellationScope;
    FView: INyxCollectionView;
    FReadiness: INyxCollectionViewPublication;
    FViewToken: INyxCollectionViewSubscription;
    FClickToken: INyxEventSubscription;
    FPort: TSummaryPort;
    FPortLease: INyxEventObserver;
    FCountRole: TNyxControlRef;
    FClearRole: TNyxControlRef;
    FCount: Integer;
    FDirty: Boolean;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
    FConnected: Boolean;
    FRevoked: Boolean;
    FCleaned: Boolean;
    procedure CheckAuthority(APreparing: Boolean);
    procedure Project(APreparing: Boolean);
    procedure Changed;
    procedure Cleanup;
    { Called once by the factory while its prepared-owner lease is retained.
      Failure releases that lease through the existing destructor/cleanup path. }
    procedure Initialize(const AProjection: INyxCoordinatorProjection;
      const AViews: INyxCollectionBindings; const AView: INyxPreparedEventView;
      const AScheduler: INyxScheduler; const ARefresh: INyxCoordinatorRefresh);
  public
    destructor Destroy; override;
    function GetConnected: Boolean;
    function GetSelectedCount: Integer;
    function GetBinding: INyxCoordinatorBinding;
    procedure Clear;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Only explicit caller registration supplies executable behavior. Saved data
    contains its checked version, not a class name, URL or authority to load code. }
  TSummaryFactory = class(TInterfacedObject, INyxCoordinatorFactory)
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;

function SelectionSummaryIntent: TNyxExtensionRef;
begin
  Result := NyxExtension('example.selection-summary');
end;

function SelectionSummaryItems: TNyxPartRef;
begin
  Result := NyxPart('items');
end;

function SelectionSummaryCount: TNyxPartRef;
begin
  Result := NyxPart('selection-count');
end;

function SelectionSummaryClear: TNyxPartRef;
begin
  Result := NyxPart('clear-selection');
end;

function SelectionSummaryCaption(ACount: Integer): TNyxText;
begin
  Result := IntToStr(ACount) + ' selected (including hidden items)';
end;

function NewSelectionSummary(const AID: TNyxText;
  const ASpec: TNyxCollectionViewSpec): INyxSelectionSummary;
var
  LItems: INyxList;
  LCount: INyxLabel;
  LClear: INyxButton;
begin
  ASpec.Validate;

  if not ASpec.Defined or (ASpec.SelectionMode <> nsmMultiple) then
  begin
    raise ENyxCoordinator.Create('Selection Summary requires a multiple-selection binding');
  end;
  Result := TSelectionSummaryCard.Create(AID);
  Result.Configure.Layout(nlColumn).Gap(12).Padding(20).Done;
  Result.Extensions.SetValue(SelectionSummaryIntent,
    NyxObject([NyxField('version', NyxData(1))]));
  LItems := NewNyxList(AID + '-items');
  LItems.Configure.PartName(SelectionSummaryItems).Height(160).Done;
  LItems.Binds.Collection(ASpec).Done;
  LCount := NewNyxLabel(AID + '-count').WithText(SelectionSummaryCaption(0));
  LCount.Configure.PartName(SelectionSummaryCount).Done;
  LClear := NewNyxButton(AID + '-clear').WithText('Clear selection');
  LClear.Configure.PartName(SelectionSummaryClear).Done;
  Result.Add(LItems).Add(LCount).Add(LClear);
end;

function TSelectionSummaryCard.GetItemsList: INyxList;
begin

  if not Supports(Part(SelectionSummaryItems), INyxList, Result) then
  begin
    raise ENyxCoordinator.Create('Selection Summary items part is not a List');
  end;
end;

function TSelectionSummaryCard.GetCountLabel: INyxLabel;
begin

  if not Supports(Part(SelectionSummaryCount), INyxLabel, Result) then
  begin
    raise ENyxCoordinator.Create('Selection Summary count part is not a Label');
  end;
end;

function TSelectionSummaryCard.GetClearButton: INyxButton;
begin

  if not Supports(Part(SelectionSummaryClear), INyxButton, Result) then
  begin
    raise ENyxCoordinator.Create('Selection Summary clear part is not a Button');
  end;
end;

function TSummaryPort.Acquire: INyxSelectionSummaryBinding;
begin
  Result := nil;

  if (Owner <> nil) and not Owner.FRevoked and not Owner.FCleaned then
  begin
    Result := Owner;
  end;
end;

procedure TSummaryPort.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LLease: INyxSelectionSummaryBinding;
  LOwner: TSummaryBinding;
begin
  LLease := Acquire;
  LOwner := Owner;

  if (LLease <> nil) and (LOwner <> nil) then
  begin
    LOwner.Changed;
  end;
end;

procedure TSummaryPort.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
var
  LLease: INyxSelectionSummaryBinding;
begin
  LLease := Acquire;

  if (LLease <> nil) and (AEvent.Trigger = ntClick) then
  begin
    LLease.Clear;
  end;
end;

procedure TSummaryBinding.Initialize(const AProjection: INyxCoordinatorProjection;
  const AViews: INyxCollectionBindings; const AView: INyxPreparedEventView;
  const AScheduler: INyxScheduler; const ARefresh: INyxCoordinatorRefresh);
var
  LViews: INyxCollectionBindings;
  LItems: TNyxControlRef;
  LSpec: TNyxCollectionViewSpec;
  LReadiness: INyxCollectionViewPublication;
begin
  FProjection := AProjection;
  LViews := AViews;
  FEventView := AView;
  FScheduler := AScheduler;
  FRefresh := ARefresh;

  if (FProjection = nil) or (LViews = nil) or (FEventView = nil) or
    (FScheduler = nil) or (FRefresh = nil) then
  begin
    raise ENyxCoordinator.Create('Selection Summary needs its candidate capabilities');
  end;
  FGeneration := FEventView.Generation;
  FScheduler.RequireUI;
  CheckAuthority(True);
  LItems := FProjection.Part(SelectionSummaryItems);
  FCountRole := FProjection.Part(SelectionSummaryCount);
  FClearRole := FProjection.Part(SelectionSummaryClear);

  if (FProjection.Kind(LItems).Name <> NyxKindName(nkList)) or
    (FProjection.Kind(FCountRole).Name <> NyxKindName(nkLabel)) or
    (FProjection.Kind(FClearRole).Name <> NyxKindName(nkButton)) or
    not FProjection.TryCollectionSpec(LItems, LSpec) or
    (LSpec.SelectionMode <> nsmMultiple) then
  begin
    raise ENyxCoordinator.Create('Selection Summary roles require List, Label and Button');
  end;
  FView := LViews.ViewFor(LItems.ID);
  CheckAuthority(True);

  if (FView = nil) or not Supports(FView, INyxCollectionViewPublication, LReadiness) then
  begin
    raise ENyxCoordinator.Create('Selection Summary needs a publication-aware collection view');
  end;
  { The local query result and the retained field each own their interface lease.
    Finalizing the Supports output must release only the local reference. }
  FReadiness := LReadiness;
  FPort := TSummaryPort.Create;
  FPortLease := FPort;
  FPort.Owner := Self;
  FViewToken := FView.Subscribe({$ifdef PAS2JS}@{$endif}FPort.Changed);
  CheckAuthority(True);
  FClickToken := FEventView.PrepareObservation(
    NyxControlEvents(FClearRole.ID, niRuntime), ntClick, FPortLease,
    nopBeforeCallbacks, NyxEvent(''));
  CheckAuthority(True);
  Project(True);
end;

destructor TSummaryBinding.Destroy;
begin
  try
    Cleanup;
  except
    { Explicit Disconnect reports failure after attempting both tokens. }
  end;
  inherited Destroy;
end;

procedure TSummaryBinding.CheckAuthority(APreparing: Boolean);
var
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LGeneration := FGeneration;

  if FRevoked or FCleaned or (LGeneration = nil) or
    (APreparing and (FDirty or FRetired)) or (not APreparing and not FConnected) then
  begin
    raise ENyxCoordinatorCancelled.Create('Selection Summary is changed or retired');
  end;
  LCancelled := LGeneration.Cancelled;

  if LCancelled or FRevoked or FCleaned or (LGeneration <> FGeneration) or
    (APreparing and FDirty) or (not APreparing and not FConnected) then
  begin
    raise ENyxCoordinatorCancelled.Create('Selection Summary generation is retired');
  end;
end;

procedure TSummaryBinding.Project(APreparing: Boolean);
var
  LLease: INyxSelectionSummaryBinding;
  LProjection: INyxCoordinatorProjection;
  LRefresh: INyxCoordinatorRefresh;
  LSelection: INyxCollectionSelection;
  LCount: Integer;
begin
  LLease := Self;
  CheckAuthority(APreparing);
  LProjection := FProjection;
  LRefresh := FRefresh;
  LSelection := FView.Selection;
  CheckAuthority(APreparing);
  LCount := LSelection.Count;
  CheckAuthority(APreparing);
  LProjection.Text(FCountRole, SelectionSummaryCaption(LCount));
  CheckAuthority(APreparing);
  LProjection.Enabled(FClearRole, LCount > 0);
  CheckAuthority(APreparing);
  FCount := LCount;

  if not APreparing then
  begin
    { Last outward call: target synchronization may dispose this complete view. }
    LRefresh.Request;
  end;
end;

procedure TSummaryBinding.Changed;
var
  LLease: INyxSelectionSummaryBinding;
begin
  LLease := Self;

  if not FConnected then
  begin
    FDirty := True;
    Exit;
  end;
  Project(False);
end;

function TSummaryBinding.GetConnected: Boolean;
var
  LLease: INyxSelectionSummaryBinding;
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LLease := Self;
  LGeneration := FGeneration;
  Result := FConnected and not FRevoked and not FCleaned and
    (LGeneration <> nil);

  if Result then
  begin
    LCancelled := LGeneration.Cancelled;
    { An alternate cancellation owner may retire this binding while queried. }
    Result := not LCancelled and FConnected and not FRevoked and not FCleaned and
      (FGeneration = LGeneration);
  end;
end;

function TSummaryBinding.GetSelectedCount: Integer;
begin
  Result := FCount;
end;

function TSummaryBinding.GetBinding: INyxCoordinatorBinding;
begin
  Result := Self;
end;

procedure TSummaryBinding.Clear;
var
  LLease: INyxSelectionSummaryBinding;
  LView: INyxCollectionView;
begin
  LLease := Self;
  CheckAuthority(False);
  LView := FView;
  { The existing view owns membership, query and the single notification. Its
    callbacks can replace the view; no borrowed receiver is used afterward. }
  LView.ClearSelection;
end;

procedure TSummaryBinding.Revoke;
begin
  FRevoked := True;
  FConnected := False;

  if FPort <> nil then
  begin
    FPort.Owner := nil;
  end;
end;

procedure TSummaryBinding.Cleanup;
var
  LFailure: TNyxText;
begin
  Revoke;

  if FCleaned then
  begin
    Exit;
  end;
  FCleaned := True;
  LFailure := '';
  try

    if FViewToken <> nil then
    begin
      FViewToken.Disconnect;
    end;
  except
    on E: Exception do LFailure := TNyxText(E.Message);
  end;
  try

    if FClickToken <> nil then
    begin
      FClickToken.Cancel;
    end;
  except
    on E: Exception do
    begin

      if LFailure = '' then
      begin
        LFailure := TNyxText(E.Message);
      end;
    end;
  end;
  FViewToken := nil;
  FClickToken := nil;
  FEventView := nil;
  FView := nil;
  FReadiness := nil;
  FRefresh := nil;
  FProjection := nil;

  if LFailure <> '' then
  begin
    raise ENyxCoordinator.Create(LFailure);
  end;
end;

procedure TSummaryBinding.Disconnect;
var
  LLease: INyxSelectionSummaryBinding;
begin
  LLease := Self;
  Cleanup;
end;

procedure TSummaryBinding.Validate;
var
  LLease: INyxPreparedCoordinator;
  LReadiness: INyxCollectionViewPublication;
begin
  LLease := Self;

  if FValidated or FInstalled or FRetired then
  begin
    raise ENyxCoordinator.Create('Selection Summary preparation validates once');
  end;
  CheckAuthority(True);
  FScheduler.RequireUI;
  CheckAuthority(True);
  LReadiness := FReadiness;

  if not LReadiness.PublicationReady then
  begin
    raise ENyxCoordinator.Create('Selection Summary collection is busy');
  end;
  CheckAuthority(True);
  FValidated := True;
end;

procedure TSummaryBinding.CheckReady;
var
  LLease: INyxPreparedCoordinator;
begin
  LLease := Self;

  if not FValidated or FInstalled or FRetired then
  begin
    raise ENyxCoordinator.Create('Selection Summary requires a validated preparation');
  end;
  { No scheduler, collection getter or host callback belongs in this fence. }
  CheckAuthority(True);
end;

procedure TSummaryBinding.Install;
begin

  if FValidated and not FInstalled and not FRetired and not FRevoked then
  begin
    FConnected := True;
    FInstalled := True;
  end;
end;

procedure TSummaryBinding.Notify;
begin
  { Initial presentation was projected before Build, while observers dormant. }
end;

procedure TSummaryBinding.Retire;
var
  LLease: INyxPreparedCoordinator;
begin
  LLease := Self;

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  FEventView := nil;

  if not FInstalled then
  begin
    Cleanup;
  end;
end;

function TSummaryFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
var
  LOwner: TSummaryBinding;
  LLease: INyxPreparedCoordinator;
begin

  if (AIntent.Kind <> ndObject) or (AIntent.Count <> 1) or
    (AIntent.Field('version').AsInteger <> 1) then
  begin
    raise ENyxCoordinator.Create('Selection Summary requires version 1 intent');
  end;
  LOwner := TSummaryBinding.Create;
  { Pin the owner before any initialization callout or temporary Self lease.
    Initialization still prepares dormant observers and initial presentation
    before the common driver can validate or build physical controls. }
  LLease := LOwner;
  LOwner.Initialize(AProjection, AViews, AView, AScheduler, ARefresh);
  Result := LLease;
end;

function NewSelectionSummaryFactory: INyxCoordinatorFactory;
begin
  Result := TSummaryFactory.Create;
end;

procedure RegisterSelectionSummary(const ARegistry: INyxCoordinatorRegistry);
begin

  if ARegistry = nil then
  begin
    raise ENyxCoordinator.Create('Selection Summary needs an explicit caller registry');
  end;
  ARegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
    SelectionSummaryIntent, NewSelectionSummaryFactory);
end;

end.
