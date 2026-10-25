{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.collections.position;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.selection;

type
  { Immutable evidence for ONE displayed source transition. Removed identities
    cannot inherit their old viewport position after reinsertion. Reset denotes
    bounded-history overflow; no source, view, callback or widget is retained. }
  INyxCollectionDisplayContinuity = interface(IInterface)
    ['{9F54E122-BA9C-4BEA-95C1-C77E52FDD2DE}']
    function GetCollection: TNyxCollectionRef;
    function GetFromRevision: Integer;
    function GetToRevision: Integer;
    function GetReset: Boolean;
    function Retains(const AItem: TNyxItemRef): Boolean;
    property Collection: TNyxCollectionRef read GetCollection;
    property FromRevision: Integer read GetFromRevision;
    property ToRevision: Integer read GetToRevision;
    property Reset: Boolean read GetReset;
  end;

  { Recovery is a viewport decision, never a row selection. Following/preceding
    choose the nearest surviving OLD neighbor, not a newly inserted record.
    Reset retains an approximate ordinal when identity cannot be proved; a known
    projection with no surviving old rows starts at its new first row. Latest is
    an explicit follow proposal for the final projected row, not a selection. }
  TNyxCollectionPositionKind = (ncpUnavailable, ncpRetained, ncpFollowing,
    ncpPreceding, ncpEmpty, ncpReset, ncpLatest);

  { Detached recovery proposal. Index is zero based and -1 for unavailable/empty.
    Targets may clamp a requested row near the physical viewport's end. No pixel,
    focus or mutation authority is implied by this value. ObservationSerial is
    assigned by a mount; pure model resolution leaves it zero. }
  TNyxCollectionPositionRecovery = record
    Kind: TNyxCollectionPositionKind;
    ObservationSerial: Integer;
    PreviousRevision: Integer;
    CurrentRevision: Integer;
    Previous: TNyxItemRef;
    Current: TNyxItemRef;
    Index: Integer;
  end;

  { Data-only observing port. The mount publishes copied recovery facts and
    clears Active before teardown; retained ports own no mount, callback, view,
    router or widget. Reads are UI-owner only, like view observation. Capture a
    Recovery value on UI before passing detached facts to a worker. This port
    is safe to retain beside application callbacks without creating a cycle. }
  INyxCollectionPositionState = interface(IInterface)
    ['{F535490E-BCF1-42D6-B727-FC2D2ED66984}']
    function GetActive: Boolean;
    function GetRecovery: TNyxCollectionPositionRecovery;
    property Active: Boolean read GetActive;
    property Recovery: TNyxCollectionPositionRecovery read GetRecovery;
  end;

  { Optional readonly mount observation exposes the last proposed recovery,
    independently of selection. It does not command scrolling or keep a former
    physical target alive after ordinary mount retirement. }
  INyxCollectionPositionObservation = interface(IInterface)
    ['{B439164E-827B-41AE-A17B-EB32FD233891}']
    function GetPositionRecovery: TNyxCollectionPositionRecovery;
    function CapturePositionState: INyxCollectionPositionState;
  end;

{ Removed references are copied into an exact immutable index. The caller's
  policy bounds their count; AReset explicitly refuses all old identities. }
function NewNyxCollectionDisplayContinuity(const ACollection: TNyxCollectionRef;
  AFromRevision, AToRevision: Integer; const ARemoved: TNyxItemRefs;
  AReset: Boolean): INyxCollectionDisplayContinuity;

{ Capture only the data port, releasing the observing owner before return.
  Unsupported owners return nil. No retained mount/renderer cycle is introduced. }
function NyxCollectionPositionState(const AOwner: IInterface): INyxCollectionPositionState;

{ Same-revision views share source identity. Across source revisions, an exact
  matching continuity receipt is required. Missing/overflowed/skipped evidence
  returns explicit Reset rather than inventing continuity from a reused ID. }
function NyxCollectionPositionRecovery(const ABefore, AAfter: INyxCollectionSnapshot;
  AIndex: Integer; const AContinuity: INyxCollectionDisplayContinuity):
  TNyxCollectionPositionRecovery;

{ Explicit follow requests target the current last projected row. Thereafter,
  only source publications with a changed final row identity request movement;
  value-only updates and local query/page changes do not repeatedly pull a reader.
  An exact continuity receipt also identifies retirement/reuse of the same ID.
  The caller checks the follow mode. This pure proposal never changes selection. }
function NyxCollectionLatestPosition(const ABefore, AAfter: INyxCollectionSnapshot;
  AIndex: Integer; AExplicitRequest: Boolean;
  const AContinuity: INyxCollectionDisplayContinuity = nil): TNyxCollectionPositionRecovery;

implementation

uses Math, SysUtils, nyx.text.index;

type
  { Owns only copied identity/revision facts and a private immutable text index.
    External callers cannot obtain or modify the underlying index. }
  TContinuity = class(TInterfacedObject, INyxCollectionDisplayContinuity)
  private
    FCollection: TNyxCollectionRef;
    FFrom: Integer;
    FTo: Integer;
    FReset: Boolean;
    FRemoved: TNyxTextIndex;
  public
    constructor Create(const ACollection: TNyxCollectionRef; AFrom, ATo: Integer;
      const ARemoved: TNyxItemRefs; AReset: Boolean);
    destructor Destroy; override;
    function GetCollection: TNyxCollectionRef;
    function GetFromRevision: Integer;
    function GetToRevision: Integer;
    function GetReset: Boolean;
    function Retains(const AItem: TNyxItemRef): Boolean;
  end;

constructor TContinuity.Create(const ACollection: TNyxCollectionRef;
  AFrom, ATo: Integer; const ARemoved: TNyxItemRefs; AReset: Boolean);
var
  LIndex: Integer;
begin
  inherited Create;

  if (ACollection.Name = '') or (AFrom < 0) or (ATo < AFrom) then
  begin
    raise ENyxCollection.Create('Display continuity requires an ordered source transition');
  end;
  FCollection := ACollection;
  FFrom := AFrom;
  FTo := ATo;
  FReset := AReset;
  FRemoved := TNyxTextIndex.Create;
  for LIndex := 0 to High(ARemoved) do
  begin

    if not ARemoved[LIndex].Defined or
      (ARemoved[LIndex].Collection.Name <> ACollection.Name) then
    begin
      raise ENyxCollection.Create('Removed display identity belongs to another source');
    end;
    FRemoved.AddFirst(ARemoved[LIndex].ID, LIndex);
  end;
end;

destructor TContinuity.Destroy;
begin
  FRemoved.Free;
  inherited Destroy;
end;

function TContinuity.GetCollection: TNyxCollectionRef;
begin
  Result := FCollection;
end;

function TContinuity.GetFromRevision: Integer;
begin
  Result := FFrom;
end;

function TContinuity.GetToRevision: Integer;
begin
  Result := FTo;
end;

function TContinuity.GetReset: Boolean;
begin
  Result := FReset;
end;

function TContinuity.Retains(const AItem: TNyxItemRef): Boolean;
begin
  Result := not FReset and AItem.Defined and
    (AItem.Collection.Name = FCollection.Name) and (FRemoved.IndexOf(AItem.ID) < 0);
end;

function NewNyxCollectionDisplayContinuity(const ACollection: TNyxCollectionRef;
  AFromRevision, AToRevision: Integer; const ARemoved: TNyxItemRefs;
  AReset: Boolean): INyxCollectionDisplayContinuity;
begin
  Result := TContinuity.Create(ACollection, AFromRevision, AToRevision, ARemoved, AReset);
end;

function NyxCollectionPositionRecovery(const ABefore, AAfter: INyxCollectionSnapshot;
  AIndex: Integer; const AContinuity: INyxCollectionDisplayContinuity):
  TNyxCollectionPositionRecovery;
var
  LKnown: Boolean;
  LIndex: Integer;
  LNext: Integer;
  LItem: TNyxItemRef;

  function Locate(AOldIndex: Integer): Integer;
  begin
    LItem := ABefore.ItemAt(AOldIndex).Ref;
    Result := -1;

    if (ABefore.Revision = AAfter.Revision) or AContinuity.Retains(LItem) then
    begin
      Result := AAfter.IndexOf(LItem);
    end;
  end;

begin
  Result := Default(TNyxCollectionPositionRecovery);
  Result.Index := -1;

  if (ABefore = nil) or (AAfter = nil) or (AIndex < 0) or (AIndex >= ABefore.Count) then
  begin
    Exit;
  end;

  if ABefore.Key.Name <> AAfter.Key.Name then
  begin
    raise ENyxCollection.Create('Viewport recovery requires the same collection');
  end;
  Result.Previous := ABefore.ItemAt(AIndex).Ref;
  Result.PreviousRevision := ABefore.Revision;
  Result.CurrentRevision := AAfter.Revision;

  if AAfter.Count = 0 then
  begin
    Result.Kind := ncpEmpty;
    Exit;
  end;
  LKnown := ABefore.Revision = AAfter.Revision;

  if not LKnown and (AContinuity <> nil) then
  begin
    LKnown := (AContinuity.Collection.Name = ABefore.Key.Name) and
      (AContinuity.FromRevision = ABefore.Revision) and
      (AContinuity.ToRevision = AAfter.Revision) and not AContinuity.Reset;
  end;
  LNext := -1;

  if LKnown then
  begin
    LNext := Locate(AIndex);
    Result.Kind := ncpRetained;

    if LNext < 0 then
    begin
      Result.Kind := ncpFollowing;
      LIndex := AIndex + 1;
      while (LIndex < ABefore.Count) and (LNext < 0) do
      begin
        LNext := Locate(LIndex);
        Inc(LIndex);
      end;
    end;

    if LNext < 0 then
    begin
      Result.Kind := ncpPreceding;
      LIndex := AIndex - 1;
      while (LIndex >= 0) and (LNext < 0) do
      begin
        LNext := Locate(LIndex);
        Dec(LIndex);
      end;
    end;
  end;

  if LNext < 0 then
  begin
    Result.Kind := ncpReset;

    if LKnown then
    begin
      LNext := 0;
    end
    else
    begin
      LNext := Min(AIndex, AAfter.Count - 1);
    end;
  end;
  Result.Index := LNext;
  Result.Current := AAfter.ItemAt(LNext).Ref;
end;

function NyxCollectionLatestPosition(const ABefore, AAfter: INyxCollectionSnapshot;
  AIndex: Integer; AExplicitRequest: Boolean;
  const AContinuity: INyxCollectionDisplayContinuity): TNyxCollectionPositionRecovery;
begin
  Result := Default(TNyxCollectionPositionRecovery);
  Result.Index := -1;
  Result.PreviousRevision := -1;

  if AAfter = nil then
  begin
    Exit;
  end;

  if ABefore <> nil then
  begin

    if ABefore.Key.Name <> AAfter.Key.Name then
    begin
      Exit;
    end;
    Result.PreviousRevision := ABefore.Revision;

    if (AIndex >= 0) and (AIndex < ABefore.Count) then
    begin
      Result.Previous := ABefore.ItemAt(AIndex).Ref;
    end;
  end;

  if not AExplicitRequest then
  begin

    if (ABefore = nil) or (ABefore.Revision = AAfter.Revision) then
    begin
      Exit;
    end;

    if (ABefore.Count > 0) and (AAfter.Count > 0) and
      (ABefore.ItemAt(ABefore.Count - 1).Ref.ID = AAfter.ItemAt(AAfter.Count - 1).Ref.ID) then
    begin

      if (AContinuity = nil) or AContinuity.Reset or
        (AContinuity.Collection.Name <> AAfter.Key.Name) or
        (AContinuity.FromRevision <> ABefore.Revision) or
        (AContinuity.ToRevision <> AAfter.Revision) then
      begin
        Exit;
      end;

      if AContinuity.Retains(ABefore.ItemAt(ABefore.Count - 1).Ref) then
      begin
        Exit;
      end;
    end;
  end;
  Result.CurrentRevision := AAfter.Revision;

  if AAfter.Count = 0 then
  begin
    Result.Kind := ncpEmpty;
    Exit;
  end;
  Result.Kind := ncpLatest;
  Result.Index := AAfter.Count - 1;
  Result.Current := AAfter.ItemAt(Result.Index).Ref;
end;

function NyxCollectionPositionState(const AOwner: IInterface): INyxCollectionPositionState;
var
  LObservation: INyxCollectionPositionObservation;
begin
  Result := nil;

  if (AOwner <> nil) and Supports(AOwner, INyxCollectionPositionObservation, LObservation) then
  begin
    Result := LObservation.CapturePositionState;
  end;
end;

end.
