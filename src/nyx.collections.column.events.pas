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
unit nyx.collections.column.events;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.objects, nyx.collections, nyx.collections.arrangement,
  nyx.collections.view.types, nyx.text;

type
  { Arrangement axes describe override ownership as well as effective values.
    An explicit width equal to its authored default is still a new user choice. }
  TNyxColumnChangeProperty = (nccWidth, nccOrder, nccVisibility, nccPin);
  { One field can change several axes in a single admitted publication. }
  TNyxColumnChangeProperties = set of TNyxColumnChangeProperty;
  { These are reported operation paths, never permission or user-gesture proof.
    Older preparation callers remain Unspecified; no origin is guessed from data.
    Reset is reported only by an explicit manager reset/default-width command. }
  TNyxColumnChangeOrigin = (nccoUnspecified, nccoProgrammatic, nccoManager,
    nccoResize, nccoAutoSize, nccoTransfer, nccoReset);
  { Opaque copied view identity owns only an inert token. It distinguishes two
    instances of the same collection without retaining either view or granting
    authority over it. It deliberately has no wire identity or command methods.
    Native copies count the marker; pas2js records retain an inert GC object
    because this compiler cannot embed a COM interface. Neither owns a resource. }
  TNyxColumnViewRef = record
  private
    {$ifdef PAS2JS}
    FIdentity: TObject;
    {$else}
    FIdentity: IInterface;
    {$endif}
    function GetDefined: Boolean;
  public
    function Same(const AOther: TNyxColumnViewRef): Boolean;
    property Defined: Boolean read GetDefined;
  end;
  { One copied canonical field's before/after overrides and resolved positions.
    Empty overrides mean authored defaults. Positions are zero-based in the
    COMPLETE arrangement, including hidden fields, rather than painted cells.
    Order reports movement or changed membership/position in the explicit
    order prefix; unrelated fields are not labeled changed merely by a swap. }
  TNyxColumnChange = record
  private
    FField: TNyxColumnIdentity;
    FProperties: TNyxColumnChangeProperties;
    FBefore: TNyxColumnPreference;
    FAfter: TNyxColumnPreference;
    FBeforePosition: Integer;
    FPosition: Integer;
  public
    property Field: TNyxColumnIdentity read FField;
    property Properties: TNyxColumnChangeProperties read FProperties;
    property Before: TNyxColumnPreference read FBefore;
    property After: TNyxColumnPreference read FAfter;
    property BeforePosition: Integer read FBeforePosition;
    property Position: Integer read FPosition;
  end;
  { Portable immutable event value. Private arrays/descriptors are frozen before
    publication; getters copy writable preferences/specifications. Record copies
    share only immutable storage and an inert identity, including on pas2js where
    COM interface fields are unsupported. No live owner or authority is retained.
    Default/older event records remain safely absent through the managed marker. }
  TNyxColumnChangesSnapshot = record
  private
    FMarker: TNyxText;
    FView: TNyxColumnViewRef;
    FSpec: TNyxCollectionViewSpec;
    FBefore: TNyxColumnPreferences;
    FAfter: TNyxColumnPreferences;
    FBeforeSerial: Integer;
    FSerial: Integer;
    FOrigin: TNyxColumnChangeOrigin;
    FChanges: array of TNyxColumnChange;
    function GetDefined: Boolean;
    function GetSpec: TNyxCollectionViewSpec;
    function GetBefore: TNyxColumnPreferences;
    function GetAfter: TNyxColumnPreferences;
    function GetCount: Integer;
  public
    function ChangeAt(AIndex: Integer): TNyxColumnChange;
    property Defined: Boolean read GetDefined;
    property View: TNyxColumnViewRef read FView;
    property Spec: TNyxCollectionViewSpec read GetSpec;
    property Before: TNyxColumnPreferences read GetBefore;
    property After: TNyxColumnPreferences read GetAfter;
    property BeforeSerial: Integer read FBeforeSerial;
    property Serial: Integer read FSerial;
    property Origin: TNyxColumnChangeOrigin read FOrigin;
    property Count: Integer read GetCount;
  end;
  { Immutable committed arrangement data, safe to retain after view retirement.
    Getters return detached values. Spec retains authored defaults, not a model;
    the payload owns no callback, view, store, renderer or publication witness. }
  INyxColumnChanges = interface(IInterface)
    ['{CF5ED3C4-D861-4706-9D22-799C79F00001}']
    function GetView: TNyxColumnViewRef;
    function GetSpec: TNyxCollectionViewSpec;
    function GetBefore: TNyxColumnPreferences;
    function GetAfter: TNyxColumnPreferences;
    function GetBeforeSerial: Integer;
    function GetSerial: Integer;
    function GetOrigin: TNyxColumnChangeOrigin;
    function GetCount: Integer;
    function GetSnapshot: TNyxColumnChangesSnapshot;
    function ChangeAt(AIndex: Integer): TNyxColumnChange;
    property View: TNyxColumnViewRef read GetView;
    property Spec: TNyxCollectionViewSpec read GetSpec;
    property Before: TNyxColumnPreferences read GetBefore;
    property After: TNyxColumnPreferences read GetAfter;
    property BeforeSerial: Integer read GetBeforeSerial;
    property Serial: Integer read GetSerial;
    property Origin: TNyxColumnChangeOrigin read GetOrigin;
    property Count: Integer read GetCount;
    property Snapshot: TNyxColumnChangesSnapshot read GetSnapshot;
  end;
  { A managed UI-thread observer receives committed data, not an admission hook.
    Throwing cannot undo installation or suppress independent view observers.
    Keep callbacks acyclic; dropping/disconnecting the owning token releases it.
    Observer destructors release without raising, as other managed UI receivers. }
  INyxColumnChangeObserver = interface(IInterface)
    ['{CF5ED3C4-D861-4706-9D22-799C79F00002}']
    procedure ColumnsChanged(const AChanges: INyxColumnChanges);
  end;
  { Portable managed base preserves constructor Self-lease safety. Ordinary
    consumers may instead implement the observer interface on another base. }
  TNyxColumnChangeObserver = class(TNyxInterfacedObject, INyxColumnChangeObserver)
  public
    procedure ColumnsChanged(const AChanges: INyxColumnChanges); virtual; abstract;
  end;
  { Convenience adapter borrows its method receiver. The caller disconnects its
    token before disposing that receiver; an entered callback must hold its own
    generation/frame lease if it permits navigation or disposal. }
  TNyxColumnChangeHandler = procedure(const AChanges: INyxColumnChanges) of object;

function NewNyxColumnViewRef: TNyxColumnViewRef;
{ Build during fallible preparation, never during pure Install. Both preference
  sets must belong to the same copied specification. Identical sets return nil;
  every changed ownership/value produces a bounded canonical field delta. }
function NewNyxColumnChanges(const AView: TNyxColumnViewRef;
  const ASpec: TNyxCollectionViewSpec;
  const ABefore, AAfter: TNyxColumnPreferences; ABeforeSerial, ASerial: Integer;
  AOrigin: TNyxColumnChangeOrigin): INyxColumnChanges;
function NewNyxColumnChangeObserver(AHandler: TNyxColumnChangeHandler): INyxColumnChangeObserver;

implementation

uses SysUtils;

type
  { Frozen payload privately owns every descriptor/array. Accessors neither call
    application code nor consult a live view when used by scheduled callbacks. }
  TColumnChanges = class(TInterfacedObject, INyxColumnChanges)
  private
    FData: TNyxColumnChangesSnapshot;
  public
    constructor Create(const AView: TNyxColumnViewRef;
      const ASpec: TNyxCollectionViewSpec;
      const ABefore, AAfter: TNyxColumnPreferences; ABeforeSerial, ASerial: Integer;
      AOrigin: TNyxColumnChangeOrigin);
    function GetView: TNyxColumnViewRef;
    function GetSpec: TNyxCollectionViewSpec;
    function GetBefore: TNyxColumnPreferences;
    function GetAfter: TNyxColumnPreferences;
    function GetBeforeSerial: Integer;
    function GetSerial: Integer;
    function GetOrigin: TNyxColumnChangeOrigin;
    function GetCount: Integer;
    function ChangeAt(AIndex: Integer): TNyxColumnChange;
    function GetSnapshot: TNyxColumnChangesSnapshot;
  end;
  { Method bridge retains no receiver owner. Its token/frame contract is the
    same explicit borrowed-receiver discipline as existing view subscriptions. }
  TMethodObserver = class(TNyxColumnChangeObserver)
  public
    Handler: TNyxColumnChangeHandler;
    procedure ColumnsChanged(const AChanges: INyxColumnChanges); override;
  end;

function TNyxColumnViewRef.GetDefined: Boolean;
begin
  Result := FIdentity <> nil;
end;

function TNyxColumnViewRef.Same(const AOther: TNyxColumnViewRef): Boolean;
begin
  Result := Defined and AOther.Defined and (FIdentity = AOther.FIdentity);
end;

function NewNyxColumnViewRef: TNyxColumnViewRef;
begin
  Result := Default(TNyxColumnViewRef);
  {$ifdef PAS2JS}
  Result.FIdentity := TObject.Create;
  {$else}
  Result.FIdentity := TInterfacedObject.Create;
  {$endif}
end;

constructor TColumnChanges.Create(const AView: TNyxColumnViewRef;
  const ASpec: TNyxCollectionViewSpec;
  const ABefore, AAfter: TNyxColumnPreferences; ABeforeSerial, ASerial: Integer;
  AOrigin: TNyxColumnChangeOrigin);
var
  LBeforeOrder: TNyxCollectionColumnOrder;
  LAfterOrder: TNyxCollectionColumnOrder;
  LBeforePositions: array of Integer;
  LAfterPositions: array of Integer;
  LBeforeExplicit: array of Integer;
  LAfterExplicit: array of Integer;
  LIndex: Integer;
  LFound: Integer;
  LCount: Integer;
  LChange: TNyxColumnChange;
begin
  inherited Create;

  if not AView.Defined or not ASpec.Defined or
    (ABeforeSerial < 0) or (ASerial <= ABeforeSerial) or
    (Ord(AOrigin) < Ord(Low(TNyxColumnChangeOrigin))) or
    (Ord(AOrigin) > Ord(High(TNyxColumnChangeOrigin))) then
  begin
    raise EArgumentException.Create('Column changes require a view, increasing serial and typed origin');
  end;
  FData.FView := AView;
  FData.FSpec := ASpec.Copy;
  FData.FBefore := ABefore.Copy;
  FData.FAfter := AAfter.Copy;
  FData.FSpec.Validate;
  FData.FBefore.Validate;
  FData.FAfter.Validate;
  FData.FBeforeSerial := ABeforeSerial;
  FData.FSerial := ASerial;
  FData.FOrigin := AOrigin;
  FData.FMarker := 'nyx-column-changes';
  { Resolve also validates the complete typed order. Preference-only fields
    require an explicit check, since order resolution does not inspect them. }
  LBeforeOrder := FData.FSpec.ResolveColumnOrder(FData.FBefore);
  LAfterOrder := FData.FSpec.ResolveColumnOrder(FData.FAfter);
  for LIndex := 0 to FData.FBefore.Count - 1 do
  begin

    if FData.FSpec.ColumnIndex(FData.FBefore.ColumnAt(LIndex).Field) < 0 then
    begin
      raise EArgumentException.Create('Previous column change field is absent or has a different family');
    end;
  end;
  for LIndex := 0 to FData.FAfter.Count - 1 do
  begin

    if FData.FSpec.ColumnIndex(FData.FAfter.ColumnAt(LIndex).Field) < 0 then
    begin
      raise EArgumentException.Create('Next column change field is absent or has a different family');
    end;
  end;
  SetLength(LBeforePositions, FData.FSpec.Count);
  SetLength(LAfterPositions, FData.FSpec.Count);
  SetLength(LBeforeExplicit, FData.FSpec.Count);
  SetLength(LAfterExplicit, FData.FSpec.Count);
  for LIndex := 0 to FData.FSpec.Count - 1 do
  begin
    LBeforePositions[LBeforeOrder[LIndex]] := LIndex;
    LAfterPositions[LAfterOrder[LIndex]] := LIndex;
    LBeforeExplicit[LIndex] := -1;
    LAfterExplicit[LIndex] := -1;
  end;
  for LIndex := 0 to FData.FBefore.OrderCount - 1 do
  begin
    LBeforeExplicit[FData.FSpec.ColumnIndex(FData.FBefore.OrderAt(LIndex))] := LIndex;
  end;
  for LIndex := 0 to FData.FAfter.OrderCount - 1 do
  begin
    LAfterExplicit[FData.FSpec.ColumnIndex(FData.FAfter.OrderAt(LIndex))] := LIndex;
  end;
  for LIndex := 0 to FData.FSpec.Count - 1 do
  begin
    LChange := Default(TNyxColumnChange);
    LChange.FField := FData.FSpec.ColumnAt(LIndex).Identity;
    LChange.FBefore := NyxColumnPreference(LChange.Field);
    LChange.FAfter := NyxColumnPreference(LChange.Field);
    LFound := FData.FBefore.IndexOf(LChange.Field);

    if LFound >= 0 then
    begin
      LChange.FBefore := FData.FBefore.ColumnAt(LFound);
    end;
    LFound := FData.FAfter.IndexOf(LChange.Field);

    if LFound >= 0 then
    begin
      LChange.FAfter := FData.FAfter.ColumnAt(LFound);
    end;
    LChange.FBeforePosition := LBeforePositions[LIndex];
    LChange.FPosition := LAfterPositions[LIndex];

    if (LChange.BeforePosition <> LChange.Position) or
      (LBeforeExplicit[LIndex] <> LAfterExplicit[LIndex]) then
    begin
      Include(LChange.FProperties, nccOrder);
    end;

    if (LChange.Before.HasWidth <> LChange.After.HasWidth) or
      (LChange.Before.HasWidth and LChange.After.HasWidth and
        (LChange.Before.WidthValue <> LChange.After.WidthValue)) then
    begin
      Include(LChange.FProperties, nccWidth);
    end;

    if (LChange.Before.HasVisibility <> LChange.After.HasVisibility) or
      (LChange.Before.HasVisibility and LChange.After.HasVisibility and
        (LChange.Before.IsVisible <> LChange.After.IsVisible)) then
    begin
      Include(LChange.FProperties, nccVisibility);
    end;

    if (LChange.Before.HasPin <> LChange.After.HasPin) or
      (LChange.Before.HasPin and LChange.After.HasPin and
        (LChange.Before.Pin <> LChange.After.Pin)) then
    begin
      Include(LChange.FProperties, nccPin);
    end;

    if LChange.Properties <> [] then
    begin
      LCount := Length(FData.FChanges);
      SetLength(FData.FChanges, LCount + 1);
      FData.FChanges[LCount] := LChange;
    end;
  end;
end;

function TColumnChanges.GetView: TNyxColumnViewRef;
begin
  Result := FData.FView;
end;

function TColumnChanges.GetSpec: TNyxCollectionViewSpec;
begin
  Result := FData.FSpec.Copy;
end;

function TColumnChanges.GetBefore: TNyxColumnPreferences;
begin
  Result := FData.FBefore.Copy;
end;

function TColumnChanges.GetAfter: TNyxColumnPreferences;
begin
  Result := FData.FAfter.Copy;
end;

function TColumnChanges.GetBeforeSerial: Integer;
begin
  Result := FData.FBeforeSerial;
end;

function TColumnChanges.GetSerial: Integer;
begin
  Result := FData.FSerial;
end;

function TColumnChanges.GetOrigin: TNyxColumnChangeOrigin;
begin
  Result := FData.FOrigin;
end;

function TColumnChanges.GetCount: Integer;
begin
  Result := Length(FData.FChanges);
end;

function TColumnChanges.ChangeAt(AIndex: Integer): TNyxColumnChange;
begin
  Result := FData.ChangeAt(AIndex);
end;

function TColumnChanges.GetSnapshot: TNyxColumnChangesSnapshot;
begin
  Result := FData;
end;

function TNyxColumnChangesSnapshot.GetDefined: Boolean;
begin
  Result := FMarker = 'nyx-column-changes';
end;

function TNyxColumnChangesSnapshot.GetSpec: TNyxCollectionViewSpec;
begin
  Result := FSpec.Copy;
end;

function TNyxColumnChangesSnapshot.GetBefore: TNyxColumnPreferences;
begin
  Result := FBefore.Copy;
end;

function TNyxColumnChangesSnapshot.GetAfter: TNyxColumnPreferences;
begin
  Result := FAfter.Copy;
end;

function TNyxColumnChangesSnapshot.GetCount: Integer;
begin
  Result := Length(FChanges);
end;

function TNyxColumnChangesSnapshot.ChangeAt(AIndex: Integer): TNyxColumnChange;
begin

  if not Defined or (AIndex < 0) or (AIndex >= Length(FChanges)) then
  begin
    raise EArgumentOutOfRangeException.Create('Column change index is outside its snapshot');
  end;
  Result := FChanges[AIndex];
end;

function NewNyxColumnChanges(const AView: TNyxColumnViewRef;
  const ASpec: TNyxCollectionViewSpec;
  const ABefore, AAfter: TNyxColumnPreferences; ABeforeSerial, ASerial: Integer;
  AOrigin: TNyxColumnChangeOrigin): INyxColumnChanges;
var
  LChanges: INyxColumnChanges;
begin
  LChanges := TColumnChanges.Create(AView, ASpec, ABefore, AAfter,
    ABeforeSerial, ASerial, AOrigin);
  Result := nil;

  if LChanges.Count > 0 then
  begin
    Result := LChanges;
  end;
end;

procedure TMethodObserver.ColumnsChanged(const AChanges: INyxColumnChanges);
begin
  Handler(AChanges);
end;

function NewNyxColumnChangeObserver(AHandler: TNyxColumnChangeHandler): INyxColumnChangeObserver;
var
  LObserver: TMethodObserver;
begin

  if not Assigned(AHandler) then
  begin
    raise EArgumentException.Create('A column change observer requires a receiver');
  end;
  LObserver := TMethodObserver.Create;
  Result := LObserver;
  LObserver.Handler := AHandler;
end;

end.
