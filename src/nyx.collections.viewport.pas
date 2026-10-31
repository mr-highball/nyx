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
unit nyx.collections.viewport;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.arrangement;

type
  { Logical alignment inside this mounted control's available data viewport.
    Nearest keeps an already visible item still, including an oversized item
    which already spans the viewport. Native row/column boundaries may clamp. }
  TNyxCollectionRevealAlignment = (ncvaNearest, ncvaStart, ncvaCenter, ncvaEnd);
  { Preserve never blurs an editor. Take deliberately moves the keyboard cursor
    and physical focus without replacing row-selection membership or editing. }
  TNyxCollectionRevealFocus = (ncvfPreserve, ncvfTake);
  { An independent request is pending until measured target geometry settles.
    Completion is a software viewport observation, not physical display timing. }
  TNyxCollectionRevealStatus = (ncvsPending, ncvsCompleted, ncvsRefused,
    ncvsCancelled, ncvsFailed);
  { Closed refusal/cancellation facts, suitable for small semantic responses.
    NotDisplayed includes filtered, paged-out and missing rows; revealing never
    changes a query/page or shows a hidden field to manufacture a match. }
  TNyxCollectionRevealReason = (ncvrNone, ncvrUnsupported, ncvrDisconnected,
    ncvrInputUnavailable, ncvrInvalidItem, ncvrNotDisplayed, ncvrUnknownField,
    ncvrHiddenField, ncvrNoViewport, ncvrChanged, ncvrSuperseded, ncvrViewportMoved,
    ncvrCancelled, ncvrExhausted, ncvrTargetFailure);
  { Copied authoring intent owns only typed identities and closed choices.
    No model, callback, renderer or target is retained. Use NyxRevealItem as the
    fluent starting point; omitting a field requests vertical row reveal only. }
  TNyxCollectionRevealOptions = record
  private
    FItem: TNyxItemRef;
    FField: TNyxColumnIdentity;
    FVertical: TNyxCollectionRevealAlignment;
    FHorizontal: TNyxCollectionRevealAlignment;
    FFocus: TNyxCollectionRevealFocus;
  public
    function InField(const AField: TNyxColumnIdentity): TNyxCollectionRevealOptions; overload;
    function InField(const AField: TNyxTextFieldRef): TNyxCollectionRevealOptions; overload;
    function InField(const AField: TNyxBooleanFieldRef): TNyxCollectionRevealOptions; overload;
    function InField(const AField: TNyxIntegerFieldRef): TNyxCollectionRevealOptions; overload;
    function InField(const AField: TNyxNumberFieldRef): TNyxCollectionRevealOptions; overload;
    function Vertical(AAlignment: TNyxCollectionRevealAlignment): TNyxCollectionRevealOptions;
    function Horizontal(AAlignment: TNyxCollectionRevealAlignment): TNyxCollectionRevealOptions;
    function Focus(AFocus: TNyxCollectionRevealFocus): TNyxCollectionRevealOptions;
    function Valid: Boolean;
    property Item: TNyxItemRef read FItem;
    property Field: TNyxColumnIdentity read FField;
    property VerticalAlignment: TNyxCollectionRevealAlignment read FVertical;
    property HorizontalAlignment: TNyxCollectionRevealAlignment read FHorizontal;
    property FocusPolicy: TNyxCollectionRevealFocus read FFocus;
  end;
  { Detached UI-owner observation. A worker may receive this copied value, not
    interrogate a live UI port. FullyVisible can be false for oversized content
    or physical clamping even when useful visible placement has completed. }
  TNyxCollectionRevealObservation = record
    Serial: Integer;
    Options: TNyxCollectionRevealOptions;
    Status: TNyxCollectionRevealStatus;
    Reason: TNyxCollectionRevealReason;
    FullyVisible: Boolean;
  end;
  { Retainable receipt has no mount/view/widget backlink. Capture and Cancel
    are UI-owner calls; terminal results never change and late Cancel is inert.
    Pending requests can be superseded, invalidated or retired before a frame. }
  INyxCollectionReveal = interface(IInterface)
    ['{FB4FAAE5-46E5-4BE1-A358-338335EA8F00}']
    function Capture: TNyxCollectionRevealObservation;
    function GetPending: Boolean;
    procedure Cancel;
    property Pending: Boolean read GetPending;
  end;
  { Independent revocable command port. Its mount is borrowed only during UI
    entry. Retaining this port beside application callbacks cannot keep a mount
    or renderer alive. Reveal scrolls this control, not containing pages/tabs. }
  INyxCollectionViewport = interface(IInterface)
    ['{6378C6EC-75EA-4DA4-9E51-3CDFE05532E9}']
    function GetActive: Boolean;
    function Reveal(const AOptions: TNyxCollectionRevealOptions): INyxCollectionReveal;
    property Active: Boolean read GetActive;
  end;
  { Optional acquisition preserves the original mount interface and alternative
    implementations. Capture the independent port, then release this owner. }
  INyxCollectionViewportAccess = interface(IInterface)
    ['{62A438D7-96CC-4AD9-961D-882F7B93C4AA}']
    function CaptureViewport: INyxCollectionViewport;
  end;
  { Adapter-owned state exposes only the receipt interface to applications.
    Finish is flags/data-only and accepts one terminal transition. No source,
    target or receiver is retained; a receipt survives ordinary mount release. }
  TNyxCollectionRevealState = class(TInterfacedObject, INyxCollectionReveal)
  private
    FObservation: TNyxCollectionRevealObservation;
  public
    constructor Create(const AOptions: TNyxCollectionRevealOptions; ASerial: Integer = 0);
    function Capture: TNyxCollectionRevealObservation;
    function GetPending: Boolean;
    procedure Cancel;
    function Finish(AStatus: TNyxCollectionRevealStatus;
      AReason: TNyxCollectionRevealReason; AFullyVisible: Boolean = False): Boolean;
  end;
  { Borrowed synchronous adapter entry. The owner retires the port before target
    disposal. A copied method pointer never acquires an interface owner lease. }
  TNyxCollectionRevealHandler = function(const AOptions: TNyxCollectionRevealOptions):
    INyxCollectionReveal of object;
  { Adapter-owned port leases itself during entry, then calls the borrowed owner
    with no intervening callouts. Retirement clears that delegate before cleanup;
    an entered owner must retain its own call frame through target callbacks. }
  TNyxCollectionViewportPort = class(TInterfacedObject, INyxCollectionViewport)
  private
    FHandler: TNyxCollectionRevealHandler;
  public
    constructor Create(AHandler: TNyxCollectionRevealHandler);
    function GetActive: Boolean;
    function Reveal(const AOptions: TNyxCollectionRevealOptions): INyxCollectionReveal;
    procedure Retire;
  end;

function NyxRevealItem(const AItem: TNyxItemRef): TNyxCollectionRevealOptions;
{ Unsupported owners return nil. The acquired port remains safe after owner
  release, and reports an explicit disconnected receipt after retirement. }
function NyxCollectionViewport(const AOwner: IInterface): INyxCollectionViewport;
{ Shared coordinate calculation uses one plane and excludes target mutation.
  Nonfinite/negative geometry refuses. The target applies its physical clamp. }
function NyxCollectionRevealDelta(AStart, AExtent, AClipStart, AClipExtent: Double;
  AAlignment: TNyxCollectionRevealAlignment): Double;

implementation

uses SysUtils, Math;

function NyxRevealItem(const AItem: TNyxItemRef): TNyxCollectionRevealOptions;
begin
  Result := Default(TNyxCollectionRevealOptions);
  Result.FItem := AItem;
end;

function TNyxCollectionRevealOptions.InField(const AField: TNyxColumnIdentity):
  TNyxCollectionRevealOptions;
begin
  Result := Self;
  Result.FField := AField;
end;

function TNyxCollectionRevealOptions.InField(const AField: TNyxTextFieldRef): TNyxCollectionRevealOptions;
begin
  Result := InField(NyxColumnIdentity(AField));
end;

function TNyxCollectionRevealOptions.InField(const AField: TNyxBooleanFieldRef): TNyxCollectionRevealOptions;
begin
  Result := InField(NyxColumnIdentity(AField));
end;

function TNyxCollectionRevealOptions.InField(const AField: TNyxIntegerFieldRef): TNyxCollectionRevealOptions;
begin
  Result := InField(NyxColumnIdentity(AField));
end;

function TNyxCollectionRevealOptions.InField(const AField: TNyxNumberFieldRef): TNyxCollectionRevealOptions;
begin
  Result := InField(NyxColumnIdentity(AField));
end;

function TNyxCollectionRevealOptions.Vertical(AAlignment: TNyxCollectionRevealAlignment): TNyxCollectionRevealOptions;
begin
  Result := Self;
  Result.FVertical := AAlignment;
end;

function TNyxCollectionRevealOptions.Horizontal(AAlignment: TNyxCollectionRevealAlignment): TNyxCollectionRevealOptions;
begin
  Result := Self;
  Result.FHorizontal := AAlignment;
end;

function TNyxCollectionRevealOptions.Focus(AFocus: TNyxCollectionRevealFocus): TNyxCollectionRevealOptions;
begin
  Result := Self;
  Result.FFocus := AFocus;
end;

function TNyxCollectionRevealOptions.Valid: Boolean;
begin
  Result := FItem.Defined and
    (Ord(FVertical) >= Ord(Low(TNyxCollectionRevealAlignment))) and
    (Ord(FVertical) <= Ord(High(TNyxCollectionRevealAlignment))) and
    (Ord(FHorizontal) >= Ord(Low(TNyxCollectionRevealAlignment))) and
    (Ord(FHorizontal) <= Ord(High(TNyxCollectionRevealAlignment))) and
    (Ord(FFocus) >= Ord(Low(TNyxCollectionRevealFocus))) and
    (Ord(FFocus) <= Ord(High(TNyxCollectionRevealFocus)));
end;

constructor TNyxCollectionRevealState.Create(const AOptions: TNyxCollectionRevealOptions;
  ASerial: Integer);
begin
  inherited Create;
  FObservation := Default(TNyxCollectionRevealObservation);
  FObservation.Options := AOptions;
  FObservation.Serial := ASerial;
end;

function TNyxCollectionRevealState.Capture: TNyxCollectionRevealObservation;
begin
  Result := FObservation;
end;

function TNyxCollectionRevealState.GetPending: Boolean;
begin
  Result := FObservation.Status = ncvsPending;
end;

function TNyxCollectionRevealState.Finish(AStatus: TNyxCollectionRevealStatus;
  AReason: TNyxCollectionRevealReason; AFullyVisible: Boolean): Boolean;
begin
  Result := GetPending and (AStatus <> ncvsPending);

  if Result then
  begin
    FObservation.Status := AStatus;
    FObservation.Reason := AReason;
    FObservation.FullyVisible := (AStatus = ncvsCompleted) and AFullyVisible;
  end;
end;

procedure TNyxCollectionRevealState.Cancel;
begin
  Finish(ncvsCancelled, ncvrCancelled);
end;

constructor TNyxCollectionViewportPort.Create(AHandler: TNyxCollectionRevealHandler);
begin
  inherited Create;
  FHandler := AHandler;
end;

function TNyxCollectionViewportPort.GetActive: Boolean;
begin
  Result := Assigned(FHandler);
end;

function TNyxCollectionViewportPort.Reveal(const AOptions: TNyxCollectionRevealOptions):
  INyxCollectionReveal;
var
  LLease: INyxCollectionViewport;
  LHandler: TNyxCollectionRevealHandler;
  LState: TNyxCollectionRevealState;
begin
  LLease := Self;
  LHandler := FHandler;

  if Assigned(LHandler) then
  begin
    Exit(LHandler(AOptions));
  end;
  LState := TNyxCollectionRevealState.Create(AOptions);
  Result := LState;
  LState.Finish(ncvsRefused, ncvrDisconnected);
end;

procedure TNyxCollectionViewportPort.Retire;
begin
  FHandler := nil;
end;

function NyxCollectionViewport(const AOwner: IInterface): INyxCollectionViewport;
var
  LAccess: INyxCollectionViewportAccess;
begin
  Result := nil;

  if Supports(AOwner, INyxCollectionViewportAccess, LAccess) then
  begin
    Result := LAccess.CaptureViewport;
  end;
end;

function NyxCollectionRevealDelta(AStart, AExtent, AClipStart, AClipExtent: Double;
  AAlignment: TNyxCollectionRevealAlignment): Double;
begin

  if IsNan(AStart) or IsInfinite(AStart) or IsNan(AExtent) or IsInfinite(AExtent) or
    IsNan(AClipStart) or IsInfinite(AClipStart) or IsNan(AClipExtent) or
    IsInfinite(AClipExtent) or (AExtent < 0) or (AClipExtent <= 0) then
  begin
    raise ENyxCollection.Create('Collection reveal requires finite available geometry');
  end;

  if (Ord(AAlignment) < Ord(Low(TNyxCollectionRevealAlignment))) or
    (Ord(AAlignment) > Ord(High(TNyxCollectionRevealAlignment))) then
  begin
    raise ENyxCollection.Create('Unknown collection reveal alignment');
  end;
  case AAlignment of
    ncvaStart: Result := AStart - AClipStart;
    ncvaCenter: Result := AStart + AExtent / 2 - AClipStart - AClipExtent / 2;
    ncvaEnd: Result := AStart + AExtent - AClipStart - AClipExtent;
    ncvaNearest:
    begin
      Result := 0;

      if AExtent > AClipExtent then
      begin

        if AStart > AClipStart then
        begin
          Result := AStart - AClipStart;
        end
        else if AStart + AExtent < AClipStart + AClipExtent then
        begin
          Result := AStart + AExtent - AClipStart - AClipExtent;
        end;
      end
      else if AStart < AClipStart then
      begin
        Result := AStart - AClipStart;
      end
      else if AStart + AExtent > AClipStart + AClipExtent then
      begin
        Result := AStart + AExtent - AClipStart - AClipExtent;
      end;
    end;
  end;

  if IsNan(Result) or IsInfinite(Result) then
  begin
    raise ENyxCollection.Create('Collection reveal delta exceeds finite geometry');
  end;
end;

end.
