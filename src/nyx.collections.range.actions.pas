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
unit nyx.collections.range.actions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.arrangement, nyx.collections.clipboard,
  nyx.collections.range, nyx.scheduler;

type
  { A newer selection/clear/retirement during an outward getter defeats the
    entered reservation. Mounted input may claim this refused default without
    reporting a malformed-data diagnostic or replacing the newer selection. }
  ENyxCollectionRangeStale = class(ENyxCollectionRange);

  { One accepted range generation owns immutable source/page snapshots and the
    exact bounded RAW rectangle, never the live store, view, mount or editor.
    Current/Readiness describe only selection lifetime. A command must ALSO
    compare the original source/page objects, column observation and input
    epoch against its mounted owner; this token cannot grant fresh UI access.
    Retaining the immutable data after retirement is safe. Readiness performs
    synchronized flags-only reads and is suitable for a detached host scope. }
  INyxCollectionCellRangeIntent = interface(IInterface)
    ['{D08A0885-447F-4B69-9F48-38D745EB5889}']
    function GetCapture: INyxCollectionCellRangeCapture;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetReadiness: INyxCancellationReadiness;
    function GetCurrent: Boolean;
    property Capture: INyxCollectionCellRangeCapture read GetCapture;
    property Source: INyxCollectionSnapshot read GetSource;
    property Page: INyxCollectionSnapshot read GetPage;
    property Readiness: INyxCancellationReadiness read GetReadiness;
    property Current: Boolean read GetCurrent;
  end;

  { Explicit cell selection is independent of ordinary row selection. All
    methods are UI-owner operations; the mounted caller admits its scheduler
    and exact input BEFORE entry. This owner schedules nothing and invokes no
    observer. Select reserves intent before snapshot getters, admits a complete
    rectangle, then installs without callouts. A nested Select/Clear/Revoke
    prevents that older reservation from replacing the newer choice.

    Failed admission preserves the previous accepted range. Successful select
    and Clear retire the previous generation before releasing any old snapshot;
    Revoke is terminal. These operations cancel receiver authority only and
    cannot undo a clipboard write already entered on the platform. }
  INyxCollectionCellRangeSelection = interface(IInterface)
    ['{C7416955-174E-4378-9FE0-CCDD9B670903}']
    function Select(const ASource, APage: INyxCollectionSnapshot;
      const AVisibleFields: array of TNyxColumnIdentity;
      const AAnchor, AFocus: TNyxCollectionCellIdentity; AColumnSerial: Integer;
      AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
      const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangeIntent;
    function GetSelection: INyxCollectionCellRangeIntent;
    procedure Clear;
    procedure Revoke;
    property Selection: INyxCollectionCellRangeIntent read GetSelection;
  end;

{ Create an inactive, independent selection owner. No source getters, scheduler
  work, input binding or platform clipboard operation occurs at construction. }
function NewNyxCollectionCellRangeSelection: INyxCollectionCellRangeSelection;

implementation

uses SysUtils;

type
  { The concrete intent is private so the selection owner can retire its known
    canonical cancellation flag without querying an arbitrary public provider. }
  TRangeIntent = class(TInterfacedObject, INyxCollectionCellRangeIntent)
  private
    FCapture: INyxCollectionCellRangeCapture;
    FSource: INyxCollectionSnapshot;
    FPage: INyxCollectionSnapshot;
    FScope: INyxCancellationScope;
    FReady: INyxCancellationReadiness;
  public
    constructor Create(const ACapture: INyxCollectionCellRangeCapture;
      const ASource, APage: INyxCollectionSnapshot);
    procedure Retire;
    function GetCapture: INyxCollectionCellRangeCapture;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetReadiness: INyxCancellationReadiness;
    function GetCurrent: Boolean;
  end;

  { The UI owner retains exactly one accepted intent. The borrowed concrete
    pointer is valid only while FSelection owns it. Intent serial is separate
    from accepted lifetime: failed work cannot revoke the previous selection,
    but a nested newer reservation makes the older operation refuse. }
  TRangeSelection = class(TInterfacedObject, INyxCollectionCellRangeSelection)
  private
    FSelection: INyxCollectionCellRangeIntent;
    FSelected: TRangeIntent;
    FIntent: Integer;
    FRevoked: Boolean;
    procedure ClearInternal(ARevoke: Boolean);
  public
    destructor Destroy; override;
    function Select(const ASource, APage: INyxCollectionSnapshot;
      const AVisibleFields: array of TNyxColumnIdentity;
      const AAnchor, AFocus: TNyxCollectionCellIdentity; AColumnSerial: Integer;
      AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
      const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangeIntent;
    function GetSelection: INyxCollectionCellRangeIntent;
    procedure Clear;
    procedure Revoke;
  end;

constructor TRangeIntent.Create(const ACapture: INyxCollectionCellRangeCapture;
  const ASource, APage: INyxCollectionSnapshot);
var
  LReady: INyxCancellationReadiness;
begin
  inherited Create;
  FCapture := ACapture;
  FSource := ASource;
  FPage := APage;
  FScope := NewNyxCancellationScope;

  if not Supports(FScope, INyxCancellationReadiness, LReady) then
  begin
    raise ENyxCollectionRange.Create('Cell range requires canonical cancellation readiness');
  end;
  FReady := LReady;
end;

procedure TRangeIntent.Retire;
begin
  FScope.Cancel;
end;

function TRangeIntent.GetCapture: INyxCollectionCellRangeCapture;
begin
  Result := FCapture;
end;

function TRangeIntent.GetSource: INyxCollectionSnapshot;
begin
  Result := FSource;
end;

function TRangeIntent.GetPage: INyxCollectionSnapshot;
begin
  Result := FPage;
end;

function TRangeIntent.GetReadiness: INyxCancellationReadiness;
begin
  Result := FReady;
end;

function TRangeIntent.GetCurrent: Boolean;
begin
  Result := not FScope.Cancelled;
end;

destructor TRangeSelection.Destroy;
begin
  { Final release must not create a new Self interface. The canonical flag is
    retired and all fields detached before any old foreign snapshot finalizes. }
  ClearInternal(True);
  inherited Destroy;
end;

procedure TRangeSelection.ClearInternal(ARevoke: Boolean);
var
  LPrevious: INyxCollectionCellRangeIntent;
  LSelected: TRangeIntent;
begin
  FRevoked := FRevoked or ARevoke;

  if FIntent < High(Integer) then
  begin
    Inc(FIntent);
  end;
  LPrevious := FSelection;
  LSelected := FSelected;
  FSelected := nil;
  FSelection := nil;

  if LSelected <> nil then
  begin
    LSelected.Retire;
  end;
  { LPrevious releases only after the last Self access. Its payload is inert
    before caller-owned snapshot destructors can reenter this selection. }
end;

procedure TRangeSelection.Clear;
var
  LLease: INyxCollectionCellRangeSelection;
begin
  LLease := Self;
  ClearInternal(False);
end;

procedure TRangeSelection.Revoke;
var
  LLease: INyxCollectionCellRangeSelection;
begin
  LLease := Self;
  ClearInternal(True);
end;

function TRangeSelection.GetSelection: INyxCollectionCellRangeIntent;
begin
  Result := FSelection;
end;

function TRangeSelection.Select(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AAnchor, AFocus: TNyxCollectionCellIdentity; AColumnSerial: Integer;
  AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangeIntent;
var
  LLease: INyxCollectionCellRangeSelection;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LCapture: INyxCollectionCellRangeCapture;
  LCandidate: TRangeIntent;
  LCandidateLease: INyxCollectionCellRangeIntent;
  LPrevious: INyxCollectionCellRangeIntent;
  LPreviousObject: TRangeIntent;
  LIntent: Integer;
begin
  LLease := Self;
  LSource := ASource;
  LPage := APage;

  if FRevoked or (FIntent >= High(Integer) - 1) then
  begin
    raise ENyxCollectionRange.Create('Cell range selection is retired or saturated');
  end;
  Inc(FIntent);
  LIntent := FIntent;
  { The pure producer copies all caller fields/records before its first getter.
    Its complete output is installed only if this SAME reservation survives. }
  LCapture := NewNyxCollectionCellRange(LSource, LPage, AVisibleFields,
    AAnchor, AFocus, AColumnSerial, AHeaders, ALineEnding, ALimits);
  LCandidate := TRangeIntent.Create(LCapture, LSource, LPage);
  LCandidateLease := LCandidate;

  if FRevoked or (FIntent <> LIntent) then
  begin
    LCandidate.Retire;
    raise ENyxCollectionRangeStale.Create('Cell range reservation was replaced during capture');
  end;
  LPrevious := FSelection;
  LPreviousObject := FSelected;

  if LPreviousObject <> nil then
  begin
    LPreviousObject.Retire;
  end;
  FSelection := LCandidateLease;
  FSelected := LCandidate;
  { Final-only result assignment also preserves a caller's earlier interface
    when admission fails. No caller-owned interface is released mid-install. }
  Result := LCandidateLease;
end;

function NewNyxCollectionCellRangeSelection: INyxCollectionCellRangeSelection;
begin
  Result := TRangeSelection.Create;
end;

end.
