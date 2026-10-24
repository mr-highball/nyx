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
unit nyx.collections.range.stroke;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.collections, nyx.collections.arrangement,
  nyx.collections.clipboard, nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.range.pointer, nyx.scheduler;

type
  { Explicit device identity is supplied by a target's actual original event.
    It is not inferred from mouse replay, coordinates or a selected cell. }
  TNyxCollectionRangeStrokeDevice = (ncrsdMouse, ncrsdPen, ncrsdTouch);

  { One host-local physical pointer. Native mouse may use zero; concurrent
    devices/IDs never borrow another stroke's release or cancellation. }
  TNyxCollectionRangeStrokePointer = record
  private
    FDefined: Boolean;
    FDevice: TNyxCollectionRangeStrokeDevice;
    FID: Integer;
  public
    procedure Validate;
    function Same(const AOther: TNyxCollectionRangeStrokePointer): Boolean;
    property Device: TNyxCollectionRangeStrokeDevice read FDevice;
    property ID: Integer read FID;
  end;

  { Copied caller-observed facts, NOT physical authority. A target must reserve
    its original mounted input before collecting them, finish canonical events
    synchronously and repeat all checks after focus. Ready, epoch, mutation and
    column serial describe that SAME incarnation; Focus is its exact cell.
    No mounted Entry, callback, widget, view or runtime budget is stored here. }
  TNyxCollectionRangeStrokeInput = record
  private
    FDefined: Boolean;
    FReady: Boolean;
    FEpoch: Integer;
    FMutation: Integer;
    FColumns: Integer;
    FFocus: TNyxCollectionCellIdentity;
  public
    procedure Validate;
    function SameGeneration(const AOther: TNyxCollectionRangeStrokeInput): Boolean;
    property Ready: Boolean read FReady;
    property Focus: TNyxCollectionCellIdentity read FFocus;
    property ColumnSerial: Integer read FColumns;
  end;

  { Detached preview of the SAME fully admitted private pointer candidate.
    It grants no Install or live owner access. Retained captures own copied
    values/immutable snapshots only; old pointer interfaces keep their ABI. }
  INyxCollectionCellRangePointerPreview = interface(IInterface)
    ['{50707BC0-1F7E-4351-949C-D5B91285042B}']
    function GetPreview: INyxCollectionCellRangeCapture;
    property Preview: INyxCollectionCellRangeCapture read GetPreview;
  end;

  { Terminal status distinguishes explicit abandonment, stale original input
    and expected bounded producer refusal. No status promises a physical drag
    or changes accepted RAW values, ordinary rows, focus or clipboard state. }
  TNyxCollectionRangeStrokeStatus = (ncrssPending, ncrssCommitted,
    ncrssCancelled, ncrssStale, ncrssRefused);

  { One synchronous matching release. The caller dispatches its ORIGINAL event
    and focuses the staged endpoint, then supplies final source/page/input facts.
    Complete claims its first call before any check; consumed/stale retries are
    inert. Retire abandons an uncompleted release. It retains only the pure
    stroke owner, never a mounted Entry, renderer, router or target callback. }
  INyxCollectionRangeStrokeRelease = interface(IInterface)
    ['{3DC5AF2A-2023-4494-B331-5A66183403C5}']
    function Complete(AAllowDefault: Boolean;
      const ASource, APage: INyxCollectionSnapshot;
      const AFinalInput: TNyxCollectionRangeStrokeInput): Boolean;
    procedure Retire;
  end;

  { UI-owner pending lifecycle reuses the SAME accepted range selection owner.
    Stage admits full RAW candidates but publishes none. Each move reserves
    before source getters; nested newer moves/cancellation defeat older returns.
    Matching release commits once, after canonical consumption and physical
    focus fences supplied by the adapter. Other pointers are ignored.

    Readiness MUST be the captured owner-free flags-only cancellation facet,
    never a mount-retaining synchronous fence. Source/page are immutable exact
    launch objects. No asynchronous mounted Entry, view/store, widget, callback,
    scheduler or event router is retained. Physical capture/drag/touch adapters
    remain separate; this public model contract alone grants no input authority. }
  INyxCollectionRangeStroke = interface(IInterface)
    ['{87476F60-FD31-45BB-B358-64F1D11C86A4}']
    function Stage(const APointer: TNyxCollectionRangeStrokePointer;
      const ASource, APage: INyxCollectionSnapshot;
      const AInput: TNyxCollectionRangeStrokeInput;
      const AEndpoint: TNyxCollectionCellIdentity): Boolean;
    function PrepareRelease(const APointer: TNyxCollectionRangeStrokePointer;
      const ASource, APage: INyxCollectionSnapshot;
      const AInput: TNyxCollectionRangeStrokeInput): INyxCollectionRangeStrokeRelease;
    procedure LostCapture(const APointer: TNyxCollectionRangeStrokePointer);
    procedure Cancel;
    procedure Retire;
    function GetCurrent: Boolean;
    function GetPreview: INyxCollectionCellRangeCapture;
    function GetStatus: TNyxCollectionRangeStrokeStatus;
    function GetError: TNyxText;
    property Current: Boolean read GetCurrent;
    property Preview: INyxCollectionCellRangeCapture read GetPreview;
    property Status: TNyxCollectionRangeStrokeStatus read GetStatus;
    property ErrorText: TNyxText read GetError;
  end;

function NyxCollectionRangeStrokePointer(ADevice: TNyxCollectionRangeStrokeDevice;
  AID: Integer): TNyxCollectionRangeStrokePointer;
function NyxCollectionRangeStrokeInput(AReady: Boolean; AEpoch, AMutation,
  AColumns: Integer; const AFocus: TNyxCollectionCellIdentity): TNyxCollectionRangeStrokeInput;

{ Copies caller interfaces/records/fields before outward getters and assigns
  Result only after complete construction. Expected initial producer refusal is
  explicit Refused status/error; malformed facts/options still raise. A later
  source/query change normally retires accepted ranges through their existing
  owner, so cancellation never resurrects an incompatible old generation. }
function NewNyxCollectionRangeStroke(
  const ASelection: INyxCollectionCellRangeSelection;
  const AReadiness: INyxCancellationReadiness;
  const APointer: TNyxCollectionRangeStrokePointer;
  const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AOrigin, AEndpoint: TNyxCollectionCellIdentity;
  AAction: TNyxCollectionRangePointerAction;
  const AInput: TNyxCollectionRangeStrokeInput;
  AHeaders: Boolean; ALines: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionRangeStroke;

implementation

uses SysUtils;

type
  { Pure owner forward is borrowed only while an entered interface lease holds
    it. No release envelope is retained by this owner. }
  TStroke = class;

  { Matching release keeps exact request/candidate authority, not a recapture.
    The owner marks itself terminal before committing or releasing old duties. }
  TStrokeRelease = class(TInterfacedObject, INyxCollectionRangeStrokeRelease)
  private
    FOwner: TStroke;
    FLease: INyxCollectionRangeStroke;
    FRequest: Integer;
    FCompleted: Boolean;
  public
    constructor Create(AOwner: TStroke; ARequest: Integer);
    destructor Destroy; override;
    function Complete(AAllowDefault: Boolean;
      const ASource, APage: INyxCollectionSnapshot;
      const AFinalInput: TNyxCollectionRangeStrokeInput): Boolean;
    procedure Retire;
  end;

  { Only immutable launch values, a flags-only readiness and pure preparation
    survive between host messages. Request changes precede every source getter;
    detached local plans shield arbitrary snapshot-finalizer reentry. }
  TStroke = class(TInterfacedObject, INyxCollectionRangeStroke)
  private
    FSelection: INyxCollectionCellRangeSelection;
    FPreparation: INyxCollectionCellRangePointerPreparation;
    FReadiness: INyxCancellationReadiness;
    FPointer: TNyxCollectionRangeStrokePointer;
    FSource: INyxCollectionSnapshot;
    FPage: INyxCollectionSnapshot;
    FFields: TNyxCollectionClipboardFields;
    FPrevious: INyxCollectionCellRangeIntent;
    FOrigin: TNyxCollectionCellIdentity;
    FInput: TNyxCollectionRangeStrokeInput;
    FAction: TNyxCollectionRangePointerAction;
    FHeaders: Boolean;
    FLines: TNyxCollectionCopyLineEnding;
    FLimits: TNyxCollectionClipboardLimits;
    FPlan: INyxCollectionCellRangePointerCommand;
    FPreview: INyxCollectionCellRangeCapture;
    FRequest: Integer;
    FReleasing: Boolean;
    FTerminal: Boolean;
    FStatus: TNyxCollectionRangeStrokeStatus;
    FError: TNyxText;
    function Ready: Boolean;
    procedure ReleaseSelection;
    function Matches(const ASource, APage: INyxCollectionSnapshot;
      const AInput: TNyxCollectionRangeStrokeInput;
      const AFocus: TNyxCollectionCellIdentity): Boolean;
    procedure Stop(AStatus: TNyxCollectionRangeStrokeStatus; const AError: TNyxText = '');
    function Finish(ARequest: Integer; AAllowDefault: Boolean;
      const ASource, APage: INyxCollectionSnapshot;
      const AInput: TNyxCollectionRangeStrokeInput): Boolean;
    procedure Abandon(ARequest: Integer);
  public
    destructor Destroy; override;
    function Stage(const APointer: TNyxCollectionRangeStrokePointer;
      const ASource, APage: INyxCollectionSnapshot;
      const AInput: TNyxCollectionRangeStrokeInput;
      const AEndpoint: TNyxCollectionCellIdentity): Boolean;
    function PrepareRelease(const APointer: TNyxCollectionRangeStrokePointer;
      const ASource, APage: INyxCollectionSnapshot;
      const AInput: TNyxCollectionRangeStrokeInput): INyxCollectionRangeStrokeRelease;
    procedure LostCapture(const APointer: TNyxCollectionRangeStrokePointer);
    procedure Cancel;
    procedure Retire;
    function GetCurrent: Boolean;
    function GetPreview: INyxCollectionCellRangeCapture;
    function GetStatus: TNyxCollectionRangeStrokeStatus;
    function GetError: TNyxText;
  end;

procedure TNyxCollectionRangeStrokePointer.Validate;
begin

  if not FDefined or (Ord(FDevice) < Ord(Low(TNyxCollectionRangeStrokeDevice))) or
    (Ord(FDevice) > Ord(High(TNyxCollectionRangeStrokeDevice))) or
    (FID < 0) or (FID >= High(Integer)) then
  begin
    raise ENyxCollectionRange.Create('Pointer stroke identity is undefined or invalid');
  end;
end;

function TNyxCollectionRangeStrokePointer.Same(
  const AOther: TNyxCollectionRangeStrokePointer): Boolean;
begin
  Result := FDefined and AOther.FDefined and (FDevice = AOther.FDevice) and (FID = AOther.FID);
end;

function NyxCollectionRangeStrokePointer(ADevice: TNyxCollectionRangeStrokeDevice;
  AID: Integer): TNyxCollectionRangeStrokePointer;
var
  LResult: TNyxCollectionRangeStrokePointer;
begin
  LResult := Default(TNyxCollectionRangeStrokePointer);
  LResult.FDefined := True;
  LResult.FDevice := ADevice;
  LResult.FID := AID;
  LResult.Validate;
  Result := LResult;
end;

procedure TNyxCollectionRangeStrokeInput.Validate;
begin

  if not FDefined or (FEpoch <= 0) or (FEpoch >= High(Integer)) or
    (FMutation < 0) or (FMutation >= High(Integer)) or
    (FColumns < 0) or (FColumns >= High(Integer)) or not FFocus.Defined then
  begin
    raise ENyxCollectionRange.Create('Pointer stroke input facts are undefined or saturated');
  end;
end;

function TNyxCollectionRangeStrokeInput.SameGeneration(
  const AOther: TNyxCollectionRangeStrokeInput): Boolean;
begin
  Result := FDefined and AOther.FDefined and FReady and AOther.FReady and
    (FEpoch = AOther.FEpoch) and (FMutation = AOther.FMutation) and (FColumns = AOther.FColumns);
end;

function NyxCollectionRangeStrokeInput(AReady: Boolean; AEpoch, AMutation,
  AColumns: Integer; const AFocus: TNyxCollectionCellIdentity): TNyxCollectionRangeStrokeInput;
var
  LResult: TNyxCollectionRangeStrokeInput;
begin
  LResult := Default(TNyxCollectionRangeStrokeInput);
  LResult.FDefined := True;
  LResult.FReady := AReady;
  LResult.FEpoch := AEpoch;
  LResult.FMutation := AMutation;
  LResult.FColumns := AColumns;
  LResult.FFocus := AFocus;
  LResult.Validate;
  Result := LResult;
end;

function TStroke.Ready: Boolean;
begin
  Result := False;

  if FTerminal or (FReadiness = nil) then
  begin
    Exit;
  end;
  try
    FReadiness.CheckReady;
    Result := True;
  except
    on E: ENyxSchedule do
    begin
      { Only the scheduler's typed flags-only cancellation is expected here. }
    end;
  end;
end;

function TStroke.Matches(const ASource, APage: INyxCollectionSnapshot;
  const AInput: TNyxCollectionRangeStrokeInput;
  const AFocus: TNyxCollectionCellIdentity): Boolean;
begin
  Result := (ASource = FSource) and (APage = FPage) and
    AInput.SameGeneration(FInput) and AInput.Focus.Same(AFocus) and Ready;
end;

function TStroke.GetCurrent: Boolean;
begin
  Result := Ready and (FPlan <> nil) and FPlan.Current;
end;

function TStroke.GetPreview: INyxCollectionCellRangeCapture;
begin
  Result := FPreview;
end;

function TStroke.GetStatus: TNyxCollectionRangeStrokeStatus;
begin
  Result := FStatus;
end;

function TStroke.GetError: TNyxText;
begin
  Result := FError;
end;

procedure TStroke.Stop(AStatus: TNyxCollectionRangeStrokeStatus; const AError: TNyxText);
var
  LPlan: INyxCollectionCellRangePointerCommand;
begin

  if FTerminal then
  begin
    Exit;
  end;
  FTerminal := True;
  FReleasing := False;
  FStatus := AStatus;
  FError := AError;
  LPlan := FPlan;
  FPlan := nil;
  ReleaseSelection;

  if LPlan <> nil then
  begin
    LPlan.Retire;
  end;
end;

procedure TStroke.ReleaseSelection;
var
  LSelection: INyxCollectionCellRangeSelection;
  LPreparation: INyxCollectionCellRangePointerPreparation;
begin
  LSelection := FSelection;
  LPreparation := FPreparation;
  FSelection := nil;
  FPreparation := nil;
  FReadiness := nil;
  { Detach both exact ports before their final release can call an application.
    A local plan/entered preparation still protects its own canonical owner. }
  LPreparation := nil;
  LSelection := nil;
end;

procedure TStroke.Cancel;
var
  LLease: INyxCollectionRangeStroke;
begin
  LLease := Self;
  Stop(ncrssCancelled);
end;

procedure TStroke.Retire;
begin
  Cancel;
end;

procedure TStroke.LostCapture(const APointer: TNyxCollectionRangeStrokePointer);
begin

  if APointer.Same(FPointer) then
  begin
    Cancel;
  end;
end;

destructor TStroke.Destroy;
begin
  try
    Stop(ncrssCancelled);
  except
    { Explicit cancellation reports cleanup failure; final release cannot. }
  end;
  inherited Destroy;
end;

function TStroke.Stage(const APointer: TNyxCollectionRangeStrokePointer;
  const ASource, APage: INyxCollectionSnapshot;
  const AInput: TNyxCollectionRangeStrokeInput;
  const AEndpoint: TNyxCollectionCellIdentity): Boolean;
var
  LLease: INyxCollectionRangeStroke;
  LPlan: INyxCollectionCellRangePointerCommand;
  LOld: INyxCollectionCellRangePointerCommand;
  LPreview: INyxCollectionCellRangePointerPreview;
  LCapture: INyxCollectionCellRangeCapture;
  LPreparation: INyxCollectionCellRangePointerPreparation;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LInput: TNyxCollectionRangeStrokeInput;
  LPointer: TNyxCollectionRangeStrokePointer;
  LEndpoint: TNyxCollectionCellIdentity;
  LRequest: Integer;
begin
  LLease := Self;
  LSource := ASource;
  LPage := APage;
  LInput := AInput;
  LPointer := APointer;
  LEndpoint := AEndpoint;
  Result := False;

  if FTerminal or FReleasing or not LPointer.Same(FPointer) then
  begin
    Exit;
  end;

  if not Matches(LSource, LPage, LInput, FOrigin) or
    ((FPlan <> nil) and not FPlan.Current) or (FRequest >= High(Integer) - 1) then
  begin
    Stop(ncrssStale);
    Exit;
  end;
  Inc(FRequest);
  LRequest := FRequest;
  LOld := FPlan;
  FPlan := nil;
  LPreparation := FPreparation;
  try
    try
      LPlan := LPreparation.PreparePointer(FSource, FPage, FFields, FOrigin,
        LEndpoint, FPrevious, FAction, FInput.ColumnSerial, FHeaders, FLines, FLimits);
    except
      on E: ENyxCollectionClipboard do
      begin

        if not FTerminal and (FRequest = LRequest) then
        begin

          if E is ENyxCollectionRangeStale then
          begin
            Stop(ncrssStale);
          end
          else
          begin
            Stop(ncrssRefused, TNyxText(E.Message));
          end;
        end;
        Exit;
      end;
    end;

    if FTerminal or FReleasing or (FRequest <> LRequest) or
      not Matches(LSource, LPage, LInput, FOrigin) or (LPlan = nil) or not LPlan.Current then
    begin

      if not FTerminal and (FRequest = LRequest) then
      begin
        Stop(ncrssStale);
      end;
      Exit;
    end;

    if not Supports(LPlan, INyxCollectionCellRangePointerPreview, LPreview) then
    begin
      Stop(ncrssRefused, 'Pointer preparation does not expose an admitted preview');
      Exit;
    end;
    LCapture := LPreview.Preview;

    if FTerminal or (FRequest <> LRequest) or (LCapture = nil) or not LPlan.Current or
      not Matches(LSource, LPage, LInput, FOrigin) then
    begin

      if not FTerminal and (FRequest = LRequest) then
      begin
        Stop(ncrssStale);
      end;
      Exit;
    end;
    FPlan := LPlan;
    LPlan := nil;
    FPreview := LCapture;
    Result := not FTerminal and (FRequest = LRequest) and GetCurrent;
  finally
    try

      if LPlan <> nil then
      begin
        LPlan.Retire;
      end;
    finally

      if LOld <> nil then
      begin
        LOld.Retire;
      end;
    end;
  end;
end;

function TStroke.PrepareRelease(const APointer: TNyxCollectionRangeStrokePointer;
  const ASource, APage: INyxCollectionSnapshot;
  const AInput: TNyxCollectionRangeStrokeInput): INyxCollectionRangeStrokeRelease;
var
  LLease: INyxCollectionRangeStroke;
  LResult: INyxCollectionRangeStrokeRelease;
begin
  LLease := Self;

  if not FTerminal and not FReleasing and APointer.Same(FPointer) then
  begin

    if Matches(ASource, APage, AInput, FOrigin) and GetCurrent then
    begin
      FReleasing := True;
      LResult := TStrokeRelease.Create(Self, FRequest);
    end
    else
    begin
      Stop(ncrssStale);
    end;
  end;
  Result := LResult;
end;

function TStroke.Finish(ARequest: Integer; AAllowDefault: Boolean;
  const ASource, APage: INyxCollectionSnapshot;
  const AInput: TNyxCollectionRangeStrokeInput): Boolean;
var
  LPlan: INyxCollectionCellRangePointerCommand;
  LFocus: TNyxCollectionCellIdentity;
  LAllowed: Boolean;
begin
  Result := False;

  if FTerminal or not FReleasing or (FRequest <> ARequest) then
  begin
    Exit;
  end;
  LPlan := FPlan;
  LFocus := FPreview.Focus;
  LAllowed := AAllowDefault and (LPlan <> nil) and LPlan.Current and
    Matches(ASource, APage, AInput, LFocus);
  { Terminal BEFORE install/retirement. Snapshot finalizers may reenter, but
    cannot turn success into rollback or lend this release another candidate. }
  FTerminal := True;
  FReleasing := False;
  FPlan := nil;
  FStatus := ncrssCancelled;
  try

    if LAllowed then
    begin
      Result := LPlan.Complete(True);

      if Result then
      begin
        FStatus := ncrssCommitted;
      end
      else
      begin
        FStatus := ncrssStale;
      end;
    end
    else if AAllowDefault then
    begin
      FStatus := ncrssStale;
    end;
  finally
    try

      if LPlan <> nil then
      begin
        LPlan.Retire;
      end;
    finally
      ReleaseSelection;
    end;
  end;
end;

procedure TStroke.Abandon(ARequest: Integer);
begin

  if FReleasing and (FRequest = ARequest) then
  begin
    Stop(ncrssCancelled);
  end;
end;

constructor TStrokeRelease.Create(AOwner: TStroke; ARequest: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FLease := AOwner as INyxCollectionRangeStroke;
  FRequest := ARequest;
end;

function TStrokeRelease.Complete(AAllowDefault: Boolean;
  const ASource, APage: INyxCollectionSnapshot;
  const AFinalInput: TNyxCollectionRangeStrokeInput): Boolean;
var
  LLease: INyxCollectionRangeStroke;
begin
  Result := False;

  if FCompleted then
  begin
    Exit;
  end;
  FCompleted := True;
  LLease := FLease;
  Result := FOwner.Finish(FRequest, AAllowDefault, ASource, APage, AFinalInput);
end;

procedure TStrokeRelease.Retire;
var
  LOwner: TStroke;
  LLease: INyxCollectionRangeStroke;
begin
  LOwner := FOwner;
  LLease := FLease;
  FOwner := nil;
  FLease := nil;

  if not FCompleted then
  begin
    FCompleted := True;

    if LOwner <> nil then
    begin
      LOwner.Abandon(FRequest);
    end;
  end;
end;

destructor TStrokeRelease.Destroy;
begin
  try
    Retire;
  except
    { Explicit abandonment reports cleanup failure; final release cannot. }
  end;
  inherited Destroy;
end;

function NewNyxCollectionRangeStroke(
  const ASelection: INyxCollectionCellRangeSelection;
  const AReadiness: INyxCancellationReadiness;
  const APointer: TNyxCollectionRangeStrokePointer;
  const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AOrigin, AEndpoint: TNyxCollectionCellIdentity;
  AAction: TNyxCollectionRangePointerAction;
  const AInput: TNyxCollectionRangeStrokeInput;
  AHeaders: Boolean; ALines: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionRangeStroke;
var
  LSelection: INyxCollectionCellRangeSelection;
  LReadiness: INyxCancellationReadiness;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LPreparation: INyxCollectionCellRangePointerPreparation;
  LLease: INyxCollectionRangeStroke;
  LObject: TStroke;
  LFields: TNyxCollectionClipboardFields;
  LOrigin: TNyxCollectionCellIdentity;
  LEndpoint: TNyxCollectionCellIdentity;
  LPointer: TNyxCollectionRangeStrokePointer;
  LInput: TNyxCollectionRangeStrokeInput;
  LLimits: TNyxCollectionClipboardLimits;
  LIndex: Integer;
begin
  LSelection := ASelection;
  LReadiness := AReadiness;
  LSource := ASource;
  LPage := APage;
  LOrigin := AOrigin;
  LEndpoint := AEndpoint;
  LPointer := APointer;
  LInput := AInput;
  LLimits := ALimits;
  SetLength(LFields, Length(AVisibleFields));
  for LIndex := 0 to High(AVisibleFields) do
  begin
    LFields[LIndex] := AVisibleFields[LIndex];
  end;
  LPointer.Validate;
  LInput.Validate;
  LLimits.Validate;

  if (LSelection = nil) or (LReadiness = nil) or (LSource = nil) or (LPage = nil) or
    not LOrigin.Defined or not LInput.Focus.Same(LOrigin) or
    (Ord(AAction) < Ord(Low(TNyxCollectionRangePointerAction))) or
    (Ord(AAction) > Ord(High(TNyxCollectionRangePointerAction))) or
    (Ord(ALines) < Ord(Low(TNyxCollectionCopyLineEnding))) or
    (Ord(ALines) > Ord(High(TNyxCollectionCopyLineEnding))) or
    not Supports(LSelection, INyxCollectionCellRangePointerPreparation, LPreparation) then
  begin
    raise ENyxCollectionRange.Create('Pointer stroke requires original selection, snapshots, input and focus');
  end;
  LObject := TStroke.Create;
  LLease := LObject;
  LObject.FSelection := LSelection;
  LObject.FPreparation := LPreparation;
  LObject.FReadiness := LReadiness;
  LObject.FPointer := LPointer;
  LObject.FSource := LSource;
  LObject.FPage := LPage;
  LObject.FOrigin := LOrigin;
  LObject.FInput := LInput;
  LObject.FAction := AAction;
  LObject.FHeaders := AHeaders;
  LObject.FLines := ALines;
  LObject.FLimits := LLimits;
  LObject.FFields := LFields;
  LObject.FPrevious := LSelection.Selection;
  LObject.Stage(LPointer, LSource, LPage, LInput, LEndpoint);
  Result := LLease;
end;

end.
