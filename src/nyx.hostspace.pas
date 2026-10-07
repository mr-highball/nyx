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
unit nyx.hostspace;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Layout retains the allocated client rectangle. AvailableHeight fits vertical
    visual occlusion while retaining layout width and typography. It does not
    identify a keyboard or move content to follow visual-viewport panning.
    Native clients without a separate visual viewport supply the same rectangle
    for both. These choices describe host sizing, never build targets. }
  TNyxHostFit = (nhfLayout, nhfAvailableHeight);

  { Immutable fluent options. The initialized default record means Layout.
    Invalid closed choices refuse before a host installs any observation. }
  TNyxHostSizingOptions = record
  private
    FFit: TNyxHostFit;
  public
    function Fit(AValue: TNyxHostFit): TNyxHostSizingOptions;
    property FitMode: TNyxHostFit read FFit;
  end;

  { Copied integer logical-pixel allocation. No controls/documents are retained.
    Construction rounds half pixels upward and refuses nonfinite, negative or
    out-of-range dimensions. Width is always the allocated layout width. }
  TNyxHostExtent = record
  private
    FWidth: Integer;
    FHeight: Integer;
  public
    function Same(const AOther: TNyxHostExtent): Boolean;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  { A value observation keeps layout dimensions separate from visual height
    and visual magnification. AvailableHeight removes magnification from the
    measured height before fitting; ordinary pinch zoom therefore does not
    create narrower breakpoints or resize fonts. Rounding can change allocation
    by one logical pixel. Browser page zoom already changes layout dimensions.
    Zero-sized inactive hosts are valid; a default record is not an observation. }
  TNyxHostSpaceSnapshot = record
  private
    FDefined: Boolean;
    FLayoutWidth: Double;
    FLayoutHeight: Double;
    FVisualHeight: Double;
    FVisualScale: Double;
  public
    function Resolve(const AOptions: TNyxHostSizingOptions): TNyxHostExtent;
    property Defined: Boolean read FDefined;
    property LayoutWidth: Double read FLayoutWidth;
    property LayoutHeight: Double read FLayoutHeight;
    property VisualHeight: Double read FVisualHeight;
    property VisualScale: Double read FVisualScale;
  end;

  { Borrowed method receiver. Clear it or Disconnect before retiring the
    receiver. A callback may disconnect or release its observation; adapters
    retain themselves until dispatch returns. Equal resolved extents are silent.
    Observations run on the host UI thread, without owning a renderer/tree. }
  TNyxHostSpaceChanged = procedure(const AExtent: TNyxHostExtent) of object;
  INyxHostSpace = interface(IInterface)
    ['{271BE5DA-8A62-4F45-A8BC-001008005001}']
    function GetConnected: Boolean;
    function GetSnapshot: TNyxHostSpaceSnapshot;
    function GetExtent: TNyxHostExtent;
    function GetOnChange: TNyxHostSpaceChanged;
    procedure SetOnChange(AValue: TNyxHostSpaceChanged);
    { Capture current metrics. Failure retains the previous accepted snapshot;
      disconnected observations are inert. No callback fires during creation. }
    procedure Refresh;
    { Idempotent cancellation detaches target listeners and clears the receiver.
      The last accepted value remains readable after cancellation/host death. }
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
    property Snapshot: TNyxHostSpaceSnapshot read GetSnapshot;
    property Extent: TNyxHostExtent read GetExtent;
    property OnChange: TNyxHostSpaceChanged read GetOnChange write SetOnChange;
  end;

  { Shared target-extension base. Initialize once, before attaching listeners;
    platform adapters implement Capture and detach their listeners before the
    inherited Disconnect. No host types enter this public portable contract. }
  TNyxHostSpaceObserver = class(TInterfacedObject, INyxHostSpace)
  private
    FConnected: Boolean;
    FOptions: TNyxHostSizingOptions;
    FSnapshot: TNyxHostSpaceSnapshot;
    FExtent: TNyxHostExtent;
    FOnChange: TNyxHostSpaceChanged;
  protected
    procedure Initialize(const AOptions: TNyxHostSizingOptions;
      const ASnapshot: TNyxHostSpaceSnapshot);
    function Capture: TNyxHostSpaceSnapshot; virtual; abstract;
  public
    destructor Destroy; override;
    function GetConnected: Boolean;
    function GetSnapshot: TNyxHostSpaceSnapshot;
    function GetExtent: TNyxHostExtent;
    function GetOnChange: TNyxHostSpaceChanged;
    procedure SetOnChange(AValue: TNyxHostSpaceChanged);
    procedure Refresh;
    procedure Disconnect; virtual;
  end;

function NyxHostSizing: TNyxHostSizingOptions;
{ AVisualScale must be finite and positive. Layout dimensions fit signed
  32-bit logical pixels. Visual height is finite/nonnegative; its scaled product
  is bounded before multiplication, avoiding overflow on extreme observations. }
function NyxHostSpace(ALayoutWidth, ALayoutHeight, AVisualHeight,
  AVisualScale: Double): TNyxHostSpaceSnapshot;

implementation

uses Math, SysUtils;

function NyxHostSizing: TNyxHostSizingOptions;
begin
  Result := Default(TNyxHostSizingOptions);
end;

function TNyxHostSizingOptions.Fit(AValue: TNyxHostFit): TNyxHostSizingOptions;
begin

  if (Ord(AValue) < Ord(Low(TNyxHostFit))) or
    (Ord(AValue) > Ord(High(TNyxHostFit))) then
  begin
    raise EArgumentException.Create('Unknown host fitting policy');
  end;
  Result := Self;
  Result.FFit := AValue;
end;

function TNyxHostExtent.Same(const AOther: TNyxHostExtent): Boolean;
begin
  Result := (Width = AOther.Width) and (Height = AOther.Height);
end;

procedure AdmitDimension(AValue: Double; ALayout: Boolean);
begin

  if IsNan(AValue) or IsInfinite(AValue) or (AValue < 0) then
  begin
    raise EArgumentException.Create('Host dimensions must be finite and nonnegative');
  end;

  if ALayout and (AValue > 2147483647.0) then
  begin
    raise EArgumentException.Create('Host allocation exceeds logical pixel space');
  end;
end;

function NyxHostSpace(ALayoutWidth, ALayoutHeight, AVisualHeight,
  AVisualScale: Double): TNyxHostSpaceSnapshot;
begin
  AdmitDimension(ALayoutWidth, True);
  AdmitDimension(ALayoutHeight, True);
  AdmitDimension(AVisualHeight, False);

  if IsNan(AVisualScale) or IsInfinite(AVisualScale) or (AVisualScale <= 0) then
  begin
    raise EArgumentException.Create('Visual magnification must be finite and positive');
  end;
  Result := Default(TNyxHostSpaceSnapshot);
  Result.FDefined := True;
  Result.FLayoutWidth := ALayoutWidth;
  Result.FLayoutHeight := ALayoutHeight;
  Result.FVisualHeight := AVisualHeight;
  Result.FVisualScale := AVisualScale;
end;

function TNyxHostSpaceSnapshot.Resolve(
  const AOptions: TNyxHostSizingOptions): TNyxHostExtent;
var
  LHeight: Double;
begin

  if not Defined then
  begin
    raise EArgumentException.Create('An observed host space is required');
  end;
  AOptions.Fit(AOptions.FitMode);
  LHeight := LayoutHeight;

  if (AOptions.FitMode = nhfAvailableHeight) and
    (VisualHeight < LayoutHeight / VisualScale) then
  begin
    LHeight := VisualHeight * VisualScale;
  end;
  Result.FWidth := Floor(LayoutWidth + 0.5);
  Result.FHeight := Floor(LHeight + 0.5);
end;

procedure TNyxHostSpaceObserver.Initialize(const AOptions: TNyxHostSizingOptions;
  const ASnapshot: TNyxHostSpaceSnapshot);
begin
  FExtent := ASnapshot.Resolve(AOptions);
  FOptions := AOptions;
  FSnapshot := ASnapshot;
  FConnected := True;
end;

destructor TNyxHostSpaceObserver.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TNyxHostSpaceObserver.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TNyxHostSpaceObserver.GetSnapshot: TNyxHostSpaceSnapshot;
begin
  Result := FSnapshot;
end;

function TNyxHostSpaceObserver.GetExtent: TNyxHostExtent;
begin
  Result := FExtent;
end;

function TNyxHostSpaceObserver.GetOnChange: TNyxHostSpaceChanged;
begin
  Result := FOnChange;
end;

procedure TNyxHostSpaceObserver.SetOnChange(AValue: TNyxHostSpaceChanged);
begin

  if FConnected then
  begin
    FOnChange := AValue;
  end;
end;

procedure TNyxHostSpaceObserver.Refresh;
var
  LKeepAlive: INyxHostSpace;
  LSnapshot: TNyxHostSpaceSnapshot;
  LExtent: TNyxHostExtent;
  LObserver: TNyxHostSpaceChanged;
  LChanged: Boolean;
begin

  if not FConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self;
  LSnapshot := Capture;

  if not FConnected then
  begin
    Exit;
  end;
  LExtent := LSnapshot.Resolve(FOptions);
  LChanged := not LExtent.Same(FExtent);
  FSnapshot := LSnapshot;
  FExtent := LExtent;
  LObserver := FOnChange;

  if LChanged and Assigned(LObserver) then
  begin
    LObserver(LExtent);
  end;
  LKeepAlive.GetConnected;
end;

procedure TNyxHostSpaceObserver.Disconnect;
begin
  FConnected := False;
  FOnChange := nil;
end;

end.
