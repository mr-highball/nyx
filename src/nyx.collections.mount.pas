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
unit nyx.collections.mount;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.collections,
  nyx.collections.view,
  nyx.collections.selection,
  nyx.collections.refresh,
  nyx.typeahead,
  nyx.events,
  nyx.collections.query.controls,
  nyx.binding.types;

type
  { A renderer borrows the method receiver only until Disconnect. Snapshots own
    identities; callers may retain them after navigation or view destruction. }
  TNyxCollectionSelectionObserver = procedure(
    const ABefore, AAfter: INyxCollectionSelection) of object;
  { Managed renderer attachment. The renderer owns and disconnects it before
    its controls are freed; retained interfaces then report Connected=False.
    View is available while connected. Errors distinguish rejected commands from
    already-published observer failures. RefreshCount counts complete adapter
    synchronizations, not painted frames or a virtualization guarantee. }
  INyxCollectionMount = interface
    ['{C6197F04-2D12-414B-A404-067E2C626386}']
    procedure Disconnect;
    procedure Refresh;
    { Controls enforce effective ancestor policy as well as column editability.
      Read-only allows selection. Disabled controls reject selection and edits. }
    procedure SetInteraction(AEnabled, AReadOnly: Boolean);
    { Transient list/tree keyboard policy. Replaces the control's independent
      search after validation; no document, store or history is changed. Tables
      retain their cell-editor defaults. Disconnected controls refuse. }
    procedure ConfigureTypeAhead(const AOptions: TNyxTypeAheadOptions);
    procedure ObserveSelection(AObserver: TNyxCollectionSelectionObserver);
    function Select(const AItem: TNyxItemRef): Boolean; overload;
    function Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction): Boolean; overload;
    function SetSelection(const AItems: array of TNyxItemRef;
      const AFocus, AAnchor: TNyxItemRef): Boolean;
    function SelectAll: Boolean;
    function SelectRange(const AItem: TNyxItemRef;
      const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean): Boolean;
    function EditCell(const AItem: TNyxItemRef; AColumn: Integer;
      const AWireValue: TNyxText): Boolean;
    function GetConnected: Boolean;
    function GetView: INyxCollectionView;
    function GetError: TNyxText;
    function GetFailure: TNyxBindingFailure;
    function GetRefreshCount: Integer;
    property Connected: Boolean read GetConnected;
    property View: INyxCollectionView read GetView;
    property ErrorText: TNyxText read GetError;
    property Failure: TNyxBindingFailure read GetFailure;
    property RefreshCount: Integer read GetRefreshCount;
  end;

  { Optional table affordances leave the original mount GUID/vtable intact.
    Hosts supply their exact accepted event router and a weak target presenter.
    Literal Items controls have no bound mount and never acquire this surface. }
  INyxCollectionQueryMount = interface(IInterface)
    ['{96760DF7-7CB0-46B7-9259-32C886E6CC03}']
    procedure ConfigureQueries(const APresenters: INyxQueryFormPresenterFactory;
      const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
      const ADesignID: TNyxText = '');
    function GetQueryWorkbench: INyxCollectionQueryWorkbench;
    property QueryWorkbench: INyxCollectionQueryWorkbench read GetQueryWorkbench;
  end;

  { Adapter base only. Derived classes borrow target controls, implement in-place
    synchronization and detach their target callbacks. No target types enter the
    portable interfaces. Activate runs after concrete handles are initialized
    and the caller retains an interface owner; target factories do this before
    activating. Active commands retain their own call frames through callbacks.
    UI-thread confinement is inherited from the view/store contract. }
  TNyxCollectionMountBase = class(TInterfacedObject, INyxCollectionMount, INyxCollectionQueryMount)
  private
    FToken: INyxCollectionViewSubscription;
    FConnected: Boolean;
    FDisconnecting: Boolean;
    FQueryFactory: INyxQueryFormPresenterFactory;
    FError: TNyxText;
    FFailure: TNyxBindingFailure;
    FRefreshCount: Integer;
    FSelection: INyxCollectionSelection;
    FSelectionObserver: TNyxCollectionSelectionObserver;
    FTypeAhead: INyxTypeAhead;
    FTypeAheadSnapshot: INyxCollectionSnapshot;
    FSearchOrder: TNyxItemRefs;
    FLastSnapshot: INyxCollectionSnapshot;
    FChanges: INyxCollectionChanges;
    function SearchLabel(AIndex: Integer): TNyxText;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  protected
    FView: INyxCollectionView;
    FUpdating: Boolean;
    FEnabled: Boolean;
    FReadOnly: Boolean;
    FQueries: INyxCollectionQueryWorkbench;
    { Cached during synchronization, never query a source/controller from the
      native paint reader. Header zero retains its existing data-row ABI. }
    FQueryHeaders: array of TNyxText;
    FQuerySummary: TNyxText;
    { Valid only during RenderDataset. Dataset notifications carry their log;
      explicit policy/selection refreshes still synchronize target state. }
    FRefreshPlan: TNyxCollectionRefreshPlan;
    { A completed target edit restores only its own item/column. Selection and
      unrelated publications preserve other live drafts. No-op/rejected edits
      still normalize their admitted display without a data revision. These
      borrowed command values are cleared before leaving the retained frame. }
    FNormalizeValues: Boolean;
    FNormalizeItem: TNyxItemRef;
    FNormalizeColumn: Integer;
    procedure UpdateQueryHeaders;
    function QueryHeader(AColumn: Integer): TNyxText;
    { Target wire decoders report rejection through the same diagnostic phase
      contract as commands, before restoring their accepted widget display. }
    procedure Failed(AException: Exception);
    { Borrow visible identities only during the pure search; the reader cannot
      publish notifications. Dataset changes reset prefix, selection alone does
      not. Target handlers retain their mount before selection publication. }
    function FindTypeAhead(const ACharacter: TNyxText; ATimeMS: Double;
      const AOrder: TNyxItemRefs; AFocus: Integer): Integer;
    procedure ResetTypeAhead;
    procedure RenderDataset; virtual; abstract;
    procedure DetachTarget; virtual; abstract;
  public
    constructor Create(const AView: INyxCollectionView);
    destructor Destroy; override;
    procedure Activate;
    procedure Disconnect;
    procedure Refresh;
    procedure SetInteraction(AEnabled, AReadOnly: Boolean);
    procedure ConfigureTypeAhead(const AOptions: TNyxTypeAheadOptions);
    procedure ConfigureQueries(const APresenters: INyxQueryFormPresenterFactory;
      const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
      const ADesignID: TNyxText = '');
    function GetQueryWorkbench: INyxCollectionQueryWorkbench;
    procedure ObserveSelection(AObserver: TNyxCollectionSelectionObserver);
    function Select(const AItem: TNyxItemRef): Boolean; overload;
    function Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction): Boolean; overload;
    function SetSelection(const AItems: array of TNyxItemRef;
      const AFocus, AAnchor: TNyxItemRef): Boolean;
    function SelectAll: Boolean;
    function SelectRange(const AItem: TNyxItemRef;
      const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean): Boolean;
    function EditCell(const AItem: TNyxItemRef; AColumn: Integer;
      const AWireValue: TNyxText): Boolean;
    function GetConnected: Boolean;
    function GetView: INyxCollectionView;
    function GetError: TNyxText;
    function GetFailure: TNyxBindingFailure;
    function GetRefreshCount: Integer;
  end;

implementation

uses
  nyx.collections.view.types, nyx.collections.query,
  nyx.collections.query.session,
  nyx.collections.query.workbench;

function TNyxCollectionMountBase.GetQueryWorkbench: INyxCollectionQueryWorkbench;
begin
  Result := FQueries;
end;

procedure TNyxCollectionMountBase.ConfigureQueries(
  const APresenters: INyxQueryFormPresenterFactory; const AEvents: INyxEvents;
  const ATarget: TNyxEventTarget; const ADesignID: TNyxText);
var
  LKeepAlive: INyxCollectionMount;
  LQueries: INyxCollectionQueryWorkbench;
  LSession: INyxCollectionQuerySession;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if not FConnected or (FView = nil) or (FView.Projection <> cpTable) or
    (APresenters = nil) or (AEvents = nil) or (FQueries <> nil) then
  begin
    raise ENyxCollection.Create('Query controls require one connected table and exact target presenter');
  end;

  if not FView.Spec.TableQueryPolicy.IsEnabled then
  begin
    APresenters.Revoke;
    Exit;
  end;

  if not Supports(FView, INyxCollectionQuerySession, LSession) then
  begin
    APresenters.Revoke;

    if FView.Spec.HasQueryControls then
    begin
      raise ENyxCollection.Create('This view implementation does not expose query sessions for saved table controls');
    end;
    { Preserve ordinary custom view implementations of the original interface.
      An absent optional capability never invents a second mutable query owner. }
    Exit;
  end;
  LQueries := NewNyxCollectionQueryWorkbench(FView, AEvents, ATarget, APresenters, ADesignID);

  if not FConnected then
  begin
    LQueries.Disconnect;
    raise ENyxCollection.Create('Table retired while creating query controls');
  end;
  FQueryFactory := APresenters;
  FQueries := LQueries;
  FQueries.SetEnabled(FEnabled);
  Refresh;
end;

procedure TNyxCollectionMountBase.UpdateQueryHeaders;
var
  LObservation: TNyxQueryWorkbenchObservation;
  LColumn: Integer;
begin
  FQueryHeaders := nil;
  FQuerySummary := '';

  if (FQueries = nil) or not FQueries.Connected then
  begin
    Exit;
  end;
  LObservation := FQueries.Observation;

  if not FConnected or (FView = nil) then
  begin
    Exit;
  end;
  FQuerySummary := IntToStr(LObservation.MatchedRows) + TNyxText(' matching / ') +
    IntToStr(LObservation.SourceRows) + TNyxText(' source rows');
  SetLength(FQueryHeaders, FView.Spec.Count);
  for LColumn := 0 to High(FQueryHeaders) do
  begin
    FQueryHeaders[LColumn] := NyxQueryWorkbenchHeader(LObservation, FView.Spec, LColumn);
  end;
end;

function TNyxCollectionMountBase.QueryHeader(AColumn: Integer): TNyxText;
begin

  if (AColumn >= 0) and (AColumn < Length(FQueryHeaders)) then
  begin
    Exit(FQueryHeaders[AColumn]);
  end;
  Result := FView.Spec.ColumnAt(AColumn).Title;
end;

constructor TNyxCollectionMountBase.Create(const AView: INyxCollectionView);
begin
  inherited Create;

  if AView = nil then
  begin
    raise ENyxCollection.Create('Collection mount requires a live view');
  end;
  FView := AView;
  FEnabled := True;
  { The view owns only immutable authored options. Each browser/native mount
    starts a separate engine before activation; prefix/time/focus and later
    ConfigureTypeAhead overrides never write back into the saved binding. }
  FTypeAhead := NewNyxTypeAhead(AView.Spec.TypeAheadPolicy);
  FTypeAheadSnapshot := nil;

  FLastSnapshot := nil;
  FChanges := nil;
  FRefreshPlan := Default(TNyxCollectionRefreshPlan);
end;

destructor TNyxCollectionMountBase.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxCollectionMountBase.Activate;
begin

  if FConnected then
  begin
    raise ENyxCollection.Create('Collection mount is already active');
  end;

  if FView = nil then
  begin
    raise ENyxCollection.Create('Disconnected collection mount cannot reactivate');
  end;
  FToken := FView.Subscribe(Changed);
  FSelection := FView.Selection;
  FConnected := True;
  try
    Refresh;
  except
    Disconnect;
    raise;
  end;
end;

procedure TNyxCollectionMountBase.Disconnect;
var
  LQueries: INyxCollectionQueryWorkbench;
begin

  if FDisconnecting then
  begin
    Exit;
  end;
  FDisconnecting := True;
  FConnected := False;
  LQueries := FQueries;
  FQueries := nil;

  if LQueries <> nil then
  begin
    LQueries.Revoke;
  end;

  if FQueryFactory <> nil then
  begin
    FQueryFactory.Revoke;
    FQueryFactory := nil;
  end;
  try
    try

      if LQueries <> nil then
      begin
        LQueries.Disconnect;
      end;
    finally
      ResetTypeAhead;
      FSearchOrder := nil;
      FSelectionObserver := nil;
      FSelection := nil;
      FTypeAheadSnapshot := nil;
      FLastSnapshot := nil;
      FChanges := nil;
      FRefreshPlan := Default(TNyxCollectionRefreshPlan);
      FQueryHeaders := nil;
      FQuerySummary := '';
      try

        if FToken <> nil then
        begin
          FToken.Disconnect;
        end;
      finally
        FToken := nil;
        try

          if FView <> nil then
          begin
            DetachTarget;
          end;
        finally
          FView := nil;
        end;
      end;
    end;
  finally
    FDisconnecting := False;
  end;
end;

procedure TNyxCollectionMountBase.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LKeepAlive: INyxCollectionMount;
  LBefore: INyxCollectionSelection;
  LAfter: INyxCollectionSelection;
  LObserver: TNyxCollectionSelectionObserver;
begin
  LKeepAlive := Self as INyxCollectionMount;
  LBefore := FSelection;
  LAfter := AView.Selection;
  FSelection := LAfter;
  LObserver := FSelectionObserver;
  FChanges := AChanges;
  try
    Refresh;
  finally
    { Do not retain before/after datasets beyond the notification frame. }
    FChanges := nil;
  end;

  if FConnected and FEnabled and Assigned(LObserver) and
    not LAfter.SameState(LBefore) then
  begin
    { Last borrowed call. It may dispose the renderer and disconnect this mount. }
    LObserver(LBefore, LAfter);
  end;
end;

procedure TNyxCollectionMountBase.ObserveSelection(AObserver: TNyxCollectionSelectionObserver);
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Cannot observe a disconnected collection control');
  end;
  FSelectionObserver := AObserver;
  FSelection := FView.Selection;
end;

procedure TNyxCollectionMountBase.Refresh;
var
  LKeepAlive: INyxCollectionMount;
  LSnapshot: INyxCollectionSnapshot;
begin

  if not FConnected or FUpdating then
  begin
    Exit;
  end;
  { Target callbacks may release the renderer's last attachment reference.
    Retain the current call frame until its cleanup has finished. }
  LKeepAlive := Self as INyxCollectionMount;
  FUpdating := True;
  try
    UpdateQueryHeaders;

    if not FConnected then
    begin
      Exit;
    end;
    LSnapshot := FView.Snapshot;
    FRefreshPlan := NyxCollectionRefreshPlan(FLastSnapshot, LSnapshot, FView.Spec, FChanges);
    RenderDataset;

    if FConnected then
    begin
      FLastSnapshot := LSnapshot;
    end;
    Inc(FRefreshCount);
  finally
    FRefreshPlan := Default(TNyxCollectionRefreshPlan);
    FUpdating := False;
  end;
end;

procedure TNyxCollectionMountBase.Failed(AException: Exception);
begin
  FError := TNyxText(AException.Message);
  FFailure := nbfRejected;

  if AException is ENyxCollectionNotification then
  begin
    FFailure := nbfNotificationFailed;
  end;
end;

procedure TNyxCollectionMountBase.SetInteraction(AEnabled, AReadOnly: Boolean);
begin

  if (FEnabled = AEnabled) and (FReadOnly = AReadOnly) then
  begin
    Exit;
  end;
  FEnabled := AEnabled;
  FReadOnly := AReadOnly;

  if FQueries <> nil then
  begin
    FQueries.SetEnabled(AEnabled);
  end;
  ResetTypeAhead;
  Refresh;
end;

procedure TNyxCollectionMountBase.ConfigureTypeAhead(const AOptions: TNyxTypeAheadOptions);
var
  LCandidate: INyxTypeAhead;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  LCandidate := NewNyxTypeAhead(AOptions);
  FTypeAhead := LCandidate;
  FTypeAheadSnapshot := nil;
end;

procedure TNyxCollectionMountBase.ResetTypeAhead;
begin

  if FTypeAhead <> nil then
  begin
    FTypeAhead.Reset;
  end;
end;

function TNyxCollectionMountBase.SearchLabel(AIndex: Integer): TNyxText;
begin
  Result := FView.CellText(FSearchOrder[AIndex], 0);
end;

function TNyxCollectionMountBase.FindTypeAhead(const ACharacter: TNyxText;
  ATimeMS: Double; const AOrder: TNyxItemRefs; AFocus: Integer): Integer;
begin

  if FTypeAheadSnapshot <> FView.Snapshot then
  begin
    ResetTypeAhead;
    FTypeAheadSnapshot := FView.Snapshot;
  end;
  FSearchOrder := AOrder;
  try
    Result := FTypeAhead.Find(ACharacter, ATimeMS, Length(AOrder), AFocus, SearchLabel);
  finally
    FSearchOrder := nil;
  end;
end;

function TNyxCollectionMountBase.Select(const AItem: TNyxItemRef): Boolean;
begin
  Result := Select(AItem, nsaReplace);
end;

function TNyxCollectionMountBase.Select(const AItem: TNyxItemRef;
  AAction: TNyxSelectionAction): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  try

    if not FEnabled then
    begin
      raise ENyxCollection.Create('Collection control is disabled');
    end;
    LView.Select(AItem, AAction);
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
      Refresh;
    end;
  end;
end;

function TNyxCollectionMountBase.SetSelection(const AItems: array of TNyxItemRef;
  const AFocus, AAnchor: TNyxItemRef): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  try

    if not FEnabled then
    begin
      raise ENyxCollection.Create('Collection control is disabled');
    end;
    LView.SetSelection(AItems, AFocus, AAnchor);
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
      Refresh;
    end;
  end;
end;

function TNyxCollectionMountBase.SelectAll: Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  try

    if not FEnabled then
    begin
      raise ENyxCollection.Create('Collection control is disabled');
    end;
    LView.SelectAll;
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
      Refresh;
    end;
  end;
end;

function TNyxCollectionMountBase.SelectRange(const AItem: TNyxItemRef;
  const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  try

    if not FEnabled then
    begin
      raise ENyxCollection.Create('Collection control is disabled');
    end;
    LView.SelectRange(AItem, AVisibleOrder, AAdd);
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
      Refresh;
    end;
  end;
end;

function TNyxCollectionMountBase.EditCell(const AItem: TNyxItemRef;
  AColumn: Integer; const AWireValue: TNyxText): Boolean;
var
  LBefore: Integer;
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  LBefore := FRefreshCount;
  try

    if not FEnabled or FReadOnly then
    begin
      raise ENyxCollection.Create('Collection control does not allow editing');
    end;
    LView.EditWire(AItem, AColumn, AWireValue);
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
    end;
  end;
  { A no-op value or rejected draft still needs normalization/restoration.
    Accepted notifications already synchronized the controls exactly once. }

  if FRefreshCount = LBefore then
  begin
    FNormalizeItem := AItem;
    FNormalizeColumn := AColumn;
    FNormalizeValues := False;

    if AItem.Defined then
    begin
      FNormalizeValues := AItem.Collection.Name = LView.Snapshot.Key.Name;
    end;
    try
      Refresh;
    finally
      FNormalizeValues := False;
      FNormalizeItem := Default(TNyxItemRef);
      FNormalizeColumn := -1;
    end;
  end;
end;

function TNyxCollectionMountBase.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TNyxCollectionMountBase.GetView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  Result := FView;
end;

function TNyxCollectionMountBase.GetError: TNyxText;
begin
  Result := FError;
end;

function TNyxCollectionMountBase.GetFailure: TNyxBindingFailure;
begin
  Result := FFailure;
end;

function TNyxCollectionMountBase.GetRefreshCount: Integer;
begin
  Result := FRefreshCount;
end;

end.
