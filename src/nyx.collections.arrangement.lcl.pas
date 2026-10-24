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

{ nyx — native column-fit metrics, independent of active editor ownership. }
unit nyx.collections.arrangement.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
Graphics, Controls, nyx.theme, nyx.collections.mount,
  nyx.collections.arrangement.presenter, nyx.popover.lcl, nyx.text,
  nyx.collections.view, nyx.collections.arrangement.controls;

type
  { Managed detached native metric and its copied font/text-style witness.
    Fonts belong to this value owner, never a control; the measurement sample
    retains immutable data only. Matches is an explicit UI-thread validation
    operation on borrowed current fonts, NOT a final callback-free publication
    fence. The caller retains/rechecks its exact mount before invoking it. }
  INyxLCLColumnFitMeasurement = interface(IInterface)
    ['{06486EC8-E590-4575-BD98-B0EEFE45667B}']
    function GetMeasurement: INyxColumnFitMeasurement;
    function Matches(AHeaderFont, ABodyFont: TFont;
      const AHeaderText: TNyxText;
      const AHeaderStyle, ABodyStyle: TTextStyle;
      ATargetPPI, AHeaderInset, ABodyInset: Integer): Boolean;
    property Measurement: INyxColumnFitMeasurement read GetMeasurement;
  end;

{ Same accepted display conversion as native collection cells. Embedded NUL
  becomes quoted JSON so Win32 does not silently draw only a string prefix. }
function NyxLCLColumnDisplayText(const AValue: TNyxText): TNyxText;

{ Measure on a private bitmap canvas. Supply actual header/body fonts (the
  standard grid header uses TitleFont), actual drawing styles and already-scaled
  TOTAL horizontal chrome insets in physical pixels. AHeaderText includes the
  current query/header adornments. ATargetPPI is the mounted grid's resolved
  physical-to-logical scale; it must be positive.

  All borrowed fonts/text/styles are copied BEFORE sample getters can call out.
  No grid, editor, selection, focus, layout or source is changed. Natural width
  removes soft wrapping while retaining the actual single-line/prefix/tab/bidi
  drawing rules. SystemFont callbacks, arbitrary custom painting and rotated
  fonts need their own resolved metric; this adapter refuses those style flags.
  The returned witness grants no input, event, form-open or model authority. }
function MeasureNyxLCLColumnFit(const ASample: INyxCollectionColumnSample;
  AHeaderFont, ABodyFont: TFont; const AHeaderText: TNyxText;
  const AHeaderStyle, ABodyStyle: TTextStyle;
  ATargetPPI, AHeaderInset, ABodyInset: Integer): INyxLCLColumnFitMeasurement;

type
  { Target observation retains the exact ordinary Nyx popup. Its renderer and
    controls are borrowed from that popup and must never be freed externally. }
  INyxLCLColumnArrangementPresenter = interface(INyxColumnArrangementPresenter)
    ['{9014934F-B1F8-49FB-B145-C94FA5A3D4FD}']
    function GetPresentation: INyxLCLPopover;
    property Presentation: INyxLCLPopover read GetPresentation;
  end;

{ Borrow the exact table anchor and its theme until Revoke. The input lease is
  owner-free; it fences every actual popup generation without retaining a mount.
  Revoke the factory before the anchor or theme can be destroyed. }
function NewNyxLCLColumnArrangementFactory(AAnchor: TWinControl;
  ATheme: TNyxTheme; const AInput: INyxCollectionInputLifetime):
  INyxColumnArrangementPresenterFactory;

implementation

uses
Types, SysUtils, Math, LCLType, LCLIntf, nyx.data, nyx.collections,
  nyx.collections.arrangement, nyx.layout.constraints, nyx.model, nyx.root.types,
  nyx.events, nyx.state, nyx.popover, nyx.popover.types,
  nyx.collections.arrangement.workbench, nyx.types;

type
  { One weak table target, retained only as inert retirement facts by its forms.
    No form, mount or renderer is owned here, so the popup cannot form a cycle. }
  TLCLArrangementFactory = class(TInterfacedObject, INyxColumnArrangementPresenterFactory)
  private
    FActive: Boolean;
    FAnchor: TWinControl;
    FTheme: TNyxTheme;
    FInput: INyxCollectionInputLifetime;
  public
    constructor Create(AAnchor: TWinControl; ATheme: TNyxTheme;
      const AInput: INyxCollectionInputLifetime);
    destructor Destroy; override;
    function CreatePresenter(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxColumnArrangementPresenter;
    procedure Revoke;
  end;

  { The actual popover owns the cloned form document, state and live renderer.
    The forwarding event surface retains the real router and original input,
    while captured generations retain no presenter or physical widget. }
  TLCLArrangementForm = class(TInterfacedObject, INyxColumnArrangementPresenter,
    INyxLCLColumnArrangementPresenter)
  private
    FTarget: TLCLArrangementFactory;
    FLease: INyxColumnArrangementPresenterFactory;
    FHost: INyxLCLPopover;
    FEvents: INyxEvents;
    FConnected: Boolean;
  public
    constructor Create(ATarget: TLCLArrangementFactory; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef);
    destructor Destroy; override;
    function GetEvents: INyxEvents;
    function GetConnected: Boolean;
    function GetOpen: Boolean;
    function GetPresentation: INyxLCLPopover;
    function CaptureRoot: TNyxNode;
    procedure SetStatus(const AText: TNyxText);
    procedure Show(const AOptions: TNyxPopoverOptions);
    procedure Close;
    procedure Disconnect;
  end;

constructor TLCLArrangementFactory.Create(AAnchor: TWinControl;
  ATheme: TNyxTheme; const AInput: INyxCollectionInputLifetime);
begin
  inherited Create;

  if (AAnchor = nil) or (AInput = nil) then
  begin
    raise ENyxCollection.Create('Column management requires its exact table and input lifetime');
  end;
  FAnchor := AAnchor;
  FTheme := ATheme;
  FInput := AInput;
  FActive := True;
end;

destructor TLCLArrangementFactory.Destroy;
begin
  Revoke;
  inherited Destroy;
end;

procedure TLCLArrangementFactory.Revoke;
begin
  FActive := False;
  FAnchor := nil;
  FTheme := nil;
  FInput := nil;
end;

function TLCLArrangementFactory.CreatePresenter(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxColumnArrangementPresenter;
var
  LKeepAlive: INyxColumnArrangementPresenterFactory;
begin
  LKeepAlive := Self;

  if not FActive or (FAnchor = nil) or (FInput = nil) then
  begin
    raise ENyxCollection.Create('The column-management target has retired');
  end;
  Result := TLCLArrangementForm.Create(Self, ADocument, ARoot);
end;

constructor TLCLArrangementForm.Create(ATarget: TLCLArrangementFactory;
  ADocument: TNyxDocument; const ARoot: TNyxRootRef);
var
  LAnchor: TWinControl;
  LTableAnchor: TWinControl;
  LButton: TControl;
  LInput: INyxCollectionInputLifetime;
  LHost: INyxLCLPopover;
  LEvents: INyxEvents;
  LAdmission: INyxPopoverOpenAdmission;
begin
  inherited Create;
  FTarget := ATarget;
  FLease := ATarget;
  LTableAnchor := ATarget.FAnchor;
  LInput := ATarget.FInput;
  LAnchor := LTableAnchor;
  LButton := LTableAnchor.FindChildControl('NyxColumnManagerButton');

  if LButton is TWinControl then
  begin
    LAnchor := TWinControl(LButton);
  end;
  { Mounted tables return focus to their persistent real button. A direct host
    can still supply an ordinary anchor without the adapter-owned toolbar. }
  if not ATarget.FActive or (ATarget.FAnchor <> LTableAnchor) or
    (ATarget.FInput <> LInput) then
  begin
    raise ENyxCollection.Create('The column launcher retired during anchor lookup');
  end;
  LHost := NewNyxLCLPopover(LAnchor, ADocument, ARoot, ATarget.FTheme);

  if not Supports(LHost, INyxPopoverOpenAdmission, LAdmission) then
  begin
    raise ENyxCollection.Create('Column management requires actual popup open admission');
  end;
  LEvents := NewNyxArrangementQueryEvents(LHost.Events, LInput, LAdmission);

  if not ATarget.FActive or (ATarget.FAnchor <> LTableAnchor) or
    (ATarget.FInput <> LInput) then
  begin
    raise ENyxCollection.Create('The table retired while creating column management');
  end;
  FHost := LHost;
  FEvents := LEvents;
  FConnected := True;
end;

destructor TLCLArrangementForm.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TLCLArrangementForm.GetConnected: Boolean;
begin
  Result := FConnected and (FTarget <> nil) and FTarget.FActive and
    (FHost <> nil) and (FEvents <> nil);
end;

function TLCLArrangementForm.GetOpen: Boolean;
begin
  Result := GetConnected and FHost.IsOpen;
end;

function TLCLArrangementForm.GetEvents: INyxEvents;
begin
  Result := nil;

  if GetConnected then
  begin
    Result := FEvents;
  end;
end;

function TLCLArrangementForm.GetPresentation: INyxLCLPopover;
begin
  Result := FHost;
end;

function TLCLArrangementForm.CaptureRoot: TNyxNode;
begin
  Result := nil;

  if GetConnected then
  begin
    Result := FHost.Renderer.Root;
  end;
end;

procedure TLCLArrangementForm.SetStatus(const AText: TNyxText);
var
  LKeepAlive: INyxColumnArrangementPresenter;
  LHost: INyxLCLPopover;
begin
  LKeepAlive := Self;

  if not GetConnected then
  begin
    Exit;
  end;
  LHost := FHost;
  LHost.State.SetValue(NyxTextState(NyxColumnArrangementStatusState), AText);
end;

procedure TLCLArrangementForm.Show(const AOptions: TNyxPopoverOptions);
var
  LKeepAlive: INyxColumnArrangementPresenter;
  LHost: INyxLCLPopover;
  LRoot: TNyxNode;
  LFocusID: TNyxText;
begin
  LKeepAlive := Self;

  if not GetConnected then
  begin
    raise ENyxCollection.Create('The column-management form has retired');
  end;
  LHost := FHost;
  { Accepted replacement may have restored the former invoker's focus. Reuse
    this actual mounted draft, honoring explicit focus without reopening or
    recapturing another form generation. No borrowed control is read afterward. }

  if LHost.IsOpen then
  begin
    AOptions.Validate;

    if AOptions.InitialFocus.Name <> '' then
    begin
      LRoot := LHost.Renderer.Root;

      if LRoot = nil then
      begin
        raise ENyxCollection.Create('Column management has no live focus root');
      end;
      LFocusID := LRoot.Part(AOptions.InitialFocus).ID;
      LHost.Renderer.FocusWithoutScroll(LFocusID, niRuntime);
    end;
    Exit;
  end;
  LHost.Open(AOptions);
end;

procedure TLCLArrangementForm.Close;
var
  LKeepAlive: INyxColumnArrangementPresenter;
  LHost: INyxLCLPopover;
begin
  LKeepAlive := Self;
  LHost := FHost;

  if LHost <> nil then
  begin
    LHost.Close;
  end;
end;

procedure TLCLArrangementForm.Disconnect;
var
  LHost: INyxLCLPopover;
  LEvents: INyxEvents;
  LFactory: INyxColumnArrangementPresenterFactory;
begin
  FConnected := False;
  LHost := FHost;
  LEvents := FEvents;
  LFactory := FLease;
  FHost := nil;
  FEvents := nil;
  FTarget := nil;
  FLease := nil;
  { Logical detachment precedes all focus/Close callbacks. These local owners
    retain the exact former router, factory and host until cleanup unwinds. }

  if LHost <> nil then
  begin
    LHost.Close;
  end;
end;

function NewNyxLCLColumnArrangementFactory(AAnchor: TWinControl;
  ATheme: TNyxTheme; const AInput: INyxCollectionInputLifetime):
  INyxColumnArrangementPresenterFactory;
begin
  Result := TLCLArrangementFactory.Create(AAnchor, ATheme, AInput);
end;

type
  { A synchronous measurement frame owns copied fonts across every outward
    sample getter. No raw widget or mount is retained, even after the original
    attachment disappears. The private canvas is released before returning. }
  TLCLColumnFitMeasurement = class(TInterfacedObject, INyxLCLColumnFitMeasurement)
  private
    FHeaderFont, FBodyFont: TFont;
    FHeaderStyle, FBodyStyle: TTextStyle;
    FHeaderText: TNyxText;
    FTargetPPI, FHeaderInset, FBodyInset: Integer;
    FMeasurement: INyxColumnFitMeasurement;
  public
    destructor Destroy; override;
    procedure Initialize(const ASample: INyxCollectionColumnSample;
      AHeaderFont, ABodyFont: TFont; const AHeaderText: TNyxText;
      const AHeaderStyle, ABodyStyle: TTextStyle;
      ATargetPPI, AHeaderInset, ABodyInset: Integer);
    function GetMeasurement: INyxColumnFitMeasurement;
    function Matches(AHeaderFont, ABodyFont: TFont;
      const AHeaderText: TNyxText;
      const AHeaderStyle, ABodyStyle: TTextStyle;
      ATargetPPI, AHeaderInset, ABodyInset: Integer): Boolean;
  end;

function NyxLCLColumnDisplayText(const AValue: TNyxText): TNyxText;
begin
  Result := AValue;

  if Pos(#0, AValue) > 0 then
  begin
    Result := NyxData(AValue).ToJSON;
  end;
end;

function SameFitTextStyle(const ALeft, ARight: TTextStyle): Boolean;
begin
  Result := (ALeft.SingleLine = ARight.SingleLine) and
    (ALeft.WordBreak = ARight.WordBreak) and
    (ALeft.ShowPrefix = ARight.ShowPrefix) and
    (ALeft.ExpandTabs = ARight.ExpandTabs) and
    (ALeft.RightToLeft = ARight.RightToLeft) and
    (ALeft.SystemFont = ARight.SystemFont);
end;

function FitDrawFlags(const AStyle: TTextStyle): Cardinal;
begin
  Result := DT_CALCRECT;

  if AStyle.SingleLine then
  begin
    Result := Result or DT_SINGLELINE;
  end;

  if not AStyle.ShowPrefix then
  begin
    Result := Result or DT_NOPREFIX;
  end;

  if AStyle.ExpandTabs then
  begin
    Result := Result or DT_EXPANDTABS;
  end;

  if AStyle.RightToLeft then
  begin
    Result := Result or DT_RTLREADING;
  end;
  { No DT_WORDBREAK/ellipsis: measure natural width before authored clamping,
    rather than the old column width's already wrapped/truncated face. }
end;

function NativeFitTextWidth(ACanvas: TCanvas; const AText: TNyxText;
  const AStyle: TTextStyle): Double;
var
  LBounds: TRect;
  LText: TNyxText;
  LHeight: Integer;
begin
  LText := NyxLCLColumnDisplayText(AText);

  if LText = '' then
  begin
    Exit(0);
  end;
  LBounds := Rect(0, 0, 0, 0);
  LHeight := LCLIntf.DrawText(ACanvas.Handle, PChar(LText), Length(LText),
    LBounds, FitDrawFlags(AStyle));
  Result := Double(LBounds.Right) - Double(LBounds.Left);

  if (LHeight <= 0) or (Result < 0) or IsNan(Result) or IsInfinite(Result) then
  begin
    raise ENyxCollection.Create('Native column text metrics are unavailable or exceed their range');
  end;
end;

destructor TLCLColumnFitMeasurement.Destroy;
begin
  FHeaderFont.Free;
  FBodyFont.Free;
  inherited Destroy;
end;

procedure TLCLColumnFitMeasurement.Initialize(
  const ASample: INyxCollectionColumnSample; AHeaderFont, ABodyFont: TFont;
  const AHeaderText: TNyxText; const AHeaderStyle, ABodyStyle: TTextStyle;
  ATargetPPI, AHeaderInset, ABodyInset: Integer);
var
  LSample: INyxCollectionColumnSample;
  LBitmap: TBitmap;
  LNatural: Double;
  LCount, LIndex: Integer;
begin
  LSample := ASample;
  FHeaderText := AHeaderText;
  FHeaderStyle := AHeaderStyle;
  FBodyStyle := ABodyStyle;
  FTargetPPI := ATargetPPI;
  FHeaderInset := AHeaderInset;
  FBodyInset := ABodyInset;

  if (LSample = nil) or (AHeaderFont = nil) or (ABodyFont = nil) or
    (ATargetPPI <= 0) or (AHeaderInset < 0) or (ABodyInset < 0) or
    (AHeaderInset > MaximumNyxLayoutBound) or
    (ABodyInset > MaximumNyxLayoutBound) then
  begin
    raise ENyxCollection.Create('Native column fit requires exact fonts, positive DPI and bounded insets');
  end;

  if (AHeaderFont.Orientation <> 0) or (ABodyFont.Orientation <> 0) then
  begin
    raise ENyxCollection.Create('Native column fit requires a custom metric for rotated fonts');
  end;

  if AHeaderStyle.SystemFont or ABodyStyle.SystemFont then
  begin
    { TextRect may obtain a different font through OnGetSystemFont. A copied
      ordinary font cannot prove that callback's output or later lifetime. }
    raise ENyxCollection.Create('Native column fit requires a resolved metric for SystemFont callbacks');
  end;
  FHeaderFont := TFont.Create;
  FHeaderFont.PixelsPerInch := AHeaderFont.PixelsPerInch;
  FHeaderFont.Assign(AHeaderFont);
  FBodyFont := TFont.Create;
  FBodyFont.PixelsPerInch := ABodyFont.PixelsPerInch;
  FBodyFont.Assign(ABodyFont);
  { No borrowed font is touched after this point. A sample getter may retire
    the source attachment; the host later rejects that through its own fence. }
  LCount := LSample.Count;

  if (LCount < 0) or (LCount > NyxMaximumColumnAutoSizeRows) then
  begin
    raise ENyxCollection.Create('Native column sample exceeds the declared row bound');
  end;
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(1, 1);
    LBitmap.Canvas.Font.PixelsPerInch := FHeaderFont.PixelsPerInch;
    LBitmap.Canvas.Font.Assign(FHeaderFont);
    LNatural := NativeFitTextWidth(LBitmap.Canvas, FHeaderText, FHeaderStyle) + FHeaderInset;
    LBitmap.Canvas.Font.PixelsPerInch := FBodyFont.PixelsPerInch;
    LBitmap.Canvas.Font.Assign(FBodyFont);
    for LIndex := 0 to LCount - 1 do
    begin
      LNatural := Max(LNatural, NativeFitTextWidth(LBitmap.Canvas,
        LSample.TextAt(LIndex), FBodyStyle) + FBodyInset);
    end;
    FMeasurement := NyxColumnFitMeasurement(LSample, LNatural * 96.0 / FTargetPPI);
  finally
    LBitmap.Free;
  end;
end;

function TLCLColumnFitMeasurement.GetMeasurement: INyxColumnFitMeasurement;
begin
  Result := FMeasurement.Copy;
end;

function TLCLColumnFitMeasurement.Matches(AHeaderFont, ABodyFont: TFont;
  const AHeaderText: TNyxText; const AHeaderStyle, ABodyStyle: TTextStyle;
  ATargetPPI, AHeaderInset, ABodyInset: Integer): Boolean;
begin
  Result := (FMeasurement <> nil) and (AHeaderFont <> nil) and
    (ABodyFont <> nil) and (FHeaderText = AHeaderText) and
    (FTargetPPI = ATargetPPI) and (FHeaderInset = AHeaderInset) and
    (FBodyInset = ABodyInset) and SameFitTextStyle(FHeaderStyle, AHeaderStyle) and
    SameFitTextStyle(FBodyStyle, ABodyStyle) and
    (FHeaderFont.PixelsPerInch = AHeaderFont.PixelsPerInch) and
    (FBodyFont.PixelsPerInch = ABodyFont.PixelsPerInch) and
    FHeaderFont.IsEqual(AHeaderFont) and FBodyFont.IsEqual(ABodyFont);
end;

function MeasureNyxLCLColumnFit(const ASample: INyxCollectionColumnSample;
  AHeaderFont, ABodyFont: TFont; const AHeaderText: TNyxText;
  const AHeaderStyle, ABodyStyle: TTextStyle;
  ATargetPPI, AHeaderInset, ABodyInset: Integer): INyxLCLColumnFitMeasurement;
var
  LMeasurement: TLCLColumnFitMeasurement;
begin
  { Acquire managed ownership before an alternative sample can call out. }
  LMeasurement := TLCLColumnFitMeasurement.Create;
  Result := LMeasurement;
  LMeasurement.Initialize(ASample, AHeaderFont, ABodyFont, AHeaderText,
    AHeaderStyle, ABodyStyle, ATargetPPI, AHeaderInset, ABodyInset);
end;

end.
