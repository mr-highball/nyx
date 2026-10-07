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

  { Adapter base only. Derived classes borrow target controls, implement in-place
    synchronization and detach their target callbacks. No target types enter the
    portable interfaces. Activate runs after concrete handles are initialized
    and the caller retains an interface owner; target factories do this before
    activating. Active commands retain their own call frames through callbacks.
    UI-thread confinement is inherited from the view/store contract. }
  TNyxCollectionMountBase = class(TInterfacedObject, INyxCollectionMount)
  private
    FToken: INyxCollectionViewSubscription;
    FConnected: Boolean;
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
begin
  ResetTypeAhead;
  FSearchOrder := nil;
  FSelectionObserver := nil;
  FSelection := nil;
  FTypeAheadSnapshot := nil;
  FLastSnapshot := nil;
  FChanges := nil;
  FRefreshPlan := Default(TNyxCollectionRefreshPlan);

  if FToken <> nil then
  begin
    FToken.Disconnect;
    FToken := nil;
  end;

  if FView <> nil then
  begin
    FConnected := False;
    DetachTarget;
    FView := nil;
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
