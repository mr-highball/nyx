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

unit nyx.widgets.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  Types,
  Controls,
  Graphics,
  LCLType,
  ExtCtrls,
  Forms,
  CustomDrawnControls,
  nyx.text,
  nyx.button.presentation,
  nyx.layout.viewport,
  nyx.theme,
  nyx.accessibility.button.lcl,
  nyx.paint.buffer.lcl,
  nyx.event.emitter,
  nyx.gestures.lcl,
  nyx.types;

type
  { Target presentation only: menu commands stay physically focusable while their
    logical enablement controls appearance and the managed menu owns admission. }
  TNyxLCLButtonPresentation = (nbpControl, nbpMenuItem);
  { Closed target-only expansion cue. It never replaces the caption or owns a
    model pointer; the adapter supplies an accepted presentation snapshot. }
  TNyxLCLDisclosureCue = (ndcNone, ndcCollapsed, ndcExpanded);
  { Themed Lazarus button. TCDButton supplies window/control ownership, tab
    navigation, pointer capture, Space/Enter activation and action integration.
    Nyx reuses its state/activation operations, with keyboard callbacks ordered
    before activation so consumed keys cannot cause a default click.
    Theme fields are copied during ApplyTheme: controls retain no theme pointer.
    The native paint scope buffers only the dirty visible rectangle. Ordinary
    supplied-DC painting, caller-disabled buffering and unsupported widgetsets
    retain LCL's path. Both HWND scopes retire before local paint resources;
    nested window creation retires an already installed field-owned scope.
    Custom factories may still return standard TButton or other LCL controls. }
  TNyxLCLButton = class(TCDButton)
  private
    FBorderColor: TColor;
    FAccentColor: TColor;
    FTextColor: TColor;
    FTransparentFace: Boolean;
    FBorderVisible: Boolean;
    FRadius: Integer;
    FKeyboardActivation: Word;
    FPresentation: TNyxLCLButtonPresentation;
    FMenuEnabled: Boolean;
    FDisclosureCue: TNyxLCLDisclosureCue;
    { Copied semantic face and a small independent decoded glyph. The caption
      remains intact for accessibility even when glyph-only painting hides it. }
    FButtonFace: TNyxButtonPresentation;
    FGlyph: TBitmap;
    { The HWND lease borrows this control only until DestroyWnd. Managed input
      context has no owning renderer/model reference and is revoked at unmount. }
    FAccessibility: INyxLCLAccessibleButton;
    FAccessibilityManaged: Boolean;
    FAccessibilitySource: INyxEventEmitter;
    FAccessibilityFrame: INyxNativeGestureFrame;
    { This interface owns a per-HWND attachment, which only borrows the button.
      Retire it before freeing glyphs or allowing native handle destruction. }
    FPaintBuffer: INyxLCLPaintBuffer;
    function AccessibleButtonState: TNyxLCLAccessibleButtonState;
    function DisclosureSpace: Integer;
    function GetGlyphAvailable: Boolean;
  protected
    { Install after ordinary HWND initialization, and retire before LCL destroys
      it. Recreated windows receive independent native proxy/receiver leases. }
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    { Intrinsic size uses the same font/caption and horizontal frame as Paint.
      TCDButton's generic default size cannot distinguish short/long captions;
      GetPreferredSize must remain useful to every native layout consumer. }
    procedure CalculatePreferredSize(var APreferredWidth, APreferredHeight: Integer;
      AWithThemeSpace: Boolean); override;
    { TCDButtonControl activates on key-up before invoking OnKeyUp, and activates
      even when the preceding key-down was consumed. Track admitted activation
      keys and order the existing LCL callback before the reused Click operation.
      Callback navigation may free Self; a consumed key returns without reads. }
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure KeyUp(var AKey: Word; AShift: TShiftState); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { APicture is borrowed; this control copies it and never owns a renderer,
      resource catalog or atlas cache. Nil restores the usable caption fallback. }
    procedure ApplyPresentation(const AValue: TNyxButtonPresentation; APicture: TBitmap);
    { A managed view supplies its revocable producer scope and existing physical
      frame before parenting. No action runs before admission or after unmount;
      frame retirement follows the same deferred-control path as pointer input. }
    procedure ConnectAccessibility(const ASource: INyxEventEmitter;
      const AFrame: INyxNativeGestureFrame);
    { Revokes managed activation without changing visible/focusable presentation.
      A retained control can later connect to its newly admitted view context. }
    procedure DisconnectAccessibility;
    { LCL/Windows accessibility enters the existing Click operation on the UI
      thread. Standalone controls need no application context; managed controls
      require their current producer scope, and retain no fields after Click. }
    procedure ExecuteDefaultAction; override;
    { Snapshot a validated palette. Variant primary uses Accent/AccentText;
      other variants use Surface/Text. Call again after an intentional style edit. }
    procedure ApplyTheme(ATheme: TNyxTheme; const AVariant: TNyxText);
    { Closed adapter policy. Menu items align captions left and visually dim
      disabled choices without disabling their physical keyboard focus. Reset
      to nbpControl when a managed menu releases an externally retained host. }
    procedure Presentation(AValue: TNyxLCLButtonPresentation; AEnabled: Boolean = True);
    { Update a copied cue. Intrinsic sizing and painting use the same font-scale
      reservation. Repeating the accepted state does not invalidate the widget. }
    procedure Disclosure(ACue: TNyxLCLDisclosureCue);
    procedure Paint; override;
    { Copy native diagnostics without retaining a window/DC. A missing handle
      or unsupported widgetset reports the same zero value as other hosts. }
    function PaintBufferStats: TNyxLCLPaintBufferStats;
    { Expose the inherited activation entry point for applications/harnesses.
      Disabled controls (including disabled ancestors) must never activate. }
    procedure Click; override;
    property BorderColor: TColor read FBorderColor;
    property CornerRadius: Integer read FRadius;
    property DisclosureCue: TNyxLCLDisclosureCue read FDisclosureCue;
    { Host observation only: decode succeeded for this owned face. This grants
      no authority to mutate its atlas, resource, button route or renderer. }
    property GlyphAvailable: Boolean read GetGlyphAvailable;
  end;

  { Rounded themed surface retains TPanel's child ownership/layout behavior.
    Painting fills the parent's backdrop outside the rounded face, so a dark
    surface never acquires white corners. Ordinary padded child controls remain
    LCL controls; clipping children to a curved region is a separate capability. }
  TNyxLCLSurface = class(TPanel)
  private
    FViewportFace: TNyxViewportBox;
    FBorderColor: TColor;
    FAccentColor: TColor;
    FControlSurface: Boolean;
    FRadius: Integer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    { ControlSurface selects compact input-frame metrics instead of card metrics. }
    procedure ApplyTheme(ATheme: TNyxTheme; AControlSurface: Boolean = False);
    { Value-only original face coordinates inside a projected viewport. Native
      window bounds remain safe while GDI paints the logical border/rounded face
      under its real clip. Default bounds restore ordinary ClientRect painting. }
    procedure ProjectFace(const ABounds: TNyxViewportBox);
    property BorderColor: TColor read FBorderColor;
    property CornerRadius: Integer read FRadius;
  end;

{ Translate portable RGB to the LCL color representation after shared validation.
  Keeping conversion here avoids platform color types in the theme contract. }
function NyxLCLColor(const AValue: TNyxText): TColor;

implementation

uses
  CustomDrawnDrawers,
  { TCDButton requires a registered LCL drawer for initialization/geometry even
    when its descendant paints its own face. This is Lazarus's built-in drawer. }
  CustomDrawn_Common,
  nyx.model, Math, SysUtils;

type
  { Color is protected on some LCL ancestors. This access class borrows only the
    published logical color; it does not replace or cast ownership of the parent. }
  TNyxColorAccess = class(TControl);

function NyxLCLColor(const AValue: TNyxText): TColor;
var
  LRGB: Integer;
begin
  LRGB := NyxThemeRGB(AValue);
  Result := RGBToColor((LRGB shr 16) and $ff, (LRGB shr 8) and $ff, LRGB and $ff);
end;

function Blend(AFirst, ASecond: TColor; ASecondPercent: Integer): TColor;
var
  LFirst: LongInt;
  LSecond: LongInt;
  LRed: Integer;
  LGreen: Integer;
  LBlue: Integer;
begin
  LFirst := ColorToRGB(AFirst);
  LSecond := ColorToRGB(ASecond);
  LRed := ((LFirst and $ff) * (100 - ASecondPercent) +
    (LSecond and $ff) * ASecondPercent) div 100;
  LGreen := (((LFirst shr 8) and $ff) * (100 - ASecondPercent) +
    ((LSecond shr 8) and $ff) * ASecondPercent) div 100;
  LBlue := (((LFirst shr 16) and $ff) * (100 - ASecondPercent) +
    ((LSecond shr 16) and $ff) * ASecondPercent) div 100;
  Result := RGBToColor(LRed, LGreen, LBlue);
end;

function Backdrop(AControl: TControl): TColor;
begin
  Result := TNyxColorAccess(AControl).Color;

  if AControl.Parent <> nil then
  begin
    Result := TNyxColorAccess(TControl(AControl.Parent)).Color;
  end;
end;

procedure Face(ACanvas: TCanvas; const ABounds: TRect; ARadius: Integer);
var
  LRadius: Integer;
begin
  LRadius := ARadius;

  if LRadius > (ABounds.Right - ABounds.Left) div 2 then
  begin
    LRadius := (ABounds.Right - ABounds.Left) div 2;
  end;

  if LRadius > (ABounds.Bottom - ABounds.Top) div 2 then
  begin
    LRadius := (ABounds.Bottom - ABounds.Top) div 2;
  end;

  if LRadius > 0 then
  begin
    ACanvas.RoundRect(ABounds.Left, ABounds.Top, ABounds.Right, ABounds.Bottom,
      LRadius * 2, LRadius * 2);
  end
  else
  begin
    ACanvas.Rectangle(ABounds);
  end;
end;

constructor TNyxLCLButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  ParentFont := True;
  DoubleBuffered := True;
  AccessibleRole := larButton;
  FRadius := 12;
  FMenuEnabled := True;
  FBorderVisible := True;
  FGlyph := TBitmap.Create;
  FButtonFace := NyxButtonPresentation;
end;

destructor TNyxLCLButton.Destroy;
begin

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;

  if FAccessibility <> nil then
  begin
    FAccessibility.Retire;
    FAccessibility := nil;
  end;
  FGlyph.Free;
  inherited Destroy;
end;

function TNyxLCLButton.GetGlyphAvailable: Boolean;
begin
  Result := (FGlyph <> nil) and not FGlyph.Empty;
end;

procedure TNyxLCLButton.ApplyPresentation(const AValue: TNyxButtonPresentation; APicture: TBitmap);
begin
  AValue.Validate;
  FButtonFace := AValue.Copy;
  FGlyph.Assign(APicture);
  case AValue.Weight of
    nbwInherit: Font.Style := [fsBold];
    nbwRegular: Font.Style := [];
    nbwSemibold: Font.Style := [fsBold];
  end;
  InvalidatePreferredSize;
  Invalidate;
end;

function TNyxLCLButton.AccessibleButtonState: TNyxLCLAccessibleButtonState;
begin
  Result.Pressed := csfSunken in FState;
  Result.Focused := Focused;
end;

procedure TNyxLCLButton.CreateWnd;
begin
  inherited CreateWnd;
  { Native creation may reenter before the outer call returns. Retire the
    exact scopes already owned by these fields, including an inner creation's
    attachments, before installing their replacement on the current HWND. }

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;

  if FAccessibility <> nil then
  begin
    FAccessibility.Retire;
    FAccessibility := nil;
  end;
  FPaintBuffer := AttachNyxLCLPaintBuffer(Self);
  FAccessibility := AttachNyxLCLAccessibleButton(Self, AccessibleButtonState);
end;

procedure TNyxLCLButton.DestroyWnd;
begin

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;

  if FAccessibility <> nil then
  begin
    FAccessibility.Retire;
    FAccessibility := nil;
  end;
  inherited DestroyWnd;
end;

function TNyxLCLButton.PaintBufferStats: TNyxLCLPaintBufferStats;
begin
  Result := Default(TNyxLCLPaintBufferStats);

  if FPaintBuffer <> nil then
  begin
    Result := FPaintBuffer.Stats;
  end;
end;

procedure TNyxLCLButton.ConnectAccessibility(const ASource: INyxEventEmitter;
  const AFrame: INyxNativeGestureFrame);
begin

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise EInvalidOperation.Create('Native button activation context requires the UI thread');
  end;

  if (ASource = nil) or (AFrame = nil) then
  begin
    raise EArgumentNilException.Create('Managed button activation requires its source and native frame');
  end;
  FAccessibilityManaged := True;
  FAccessibilitySource := ASource;
  FAccessibilityFrame := AFrame;
end;

procedure TNyxLCLButton.DisconnectAccessibility;
begin

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise EInvalidOperation.Create('Native button activation retirement requires the UI thread');
  end;
  FAccessibilityManaged := True;
  FAccessibilitySource := nil;
  FAccessibilityFrame := nil;
end;

procedure TNyxLCLButton.ExecuteDefaultAction;
var
  LSource: INyxEventEmitter;
  LFrame: INyxNativeGestureFrame;
begin

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise EInvalidOperation.Create('Native button activation requires the UI thread');
  end;
  LSource := FAccessibilitySource;
  LFrame := FAccessibilityFrame;

  if FAccessibilityManaged and ((LSource = nil) or not LSource.Connected) then
  begin
    Exit;
  end;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;
  try
    Click;
    { Navigation may retire this control. Only the copied frame is used below. }
  finally

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

procedure TNyxLCLButton.Presentation(AValue: TNyxLCLButtonPresentation; AEnabled: Boolean);
begin

  if (Ord(AValue) < Ord(Low(TNyxLCLButtonPresentation))) or
    (Ord(AValue) > Ord(High(TNyxLCLButtonPresentation))) then
  begin
    raise ENyxModel.Create('Unknown native button presentation');
  end;

  if FPresentation <> AValue then
  begin
    FPresentation := AValue;
    InvalidatePreferredSize;
  end;
  FMenuEnabled := AEnabled;
  Invalidate;
end;

function TNyxLCLButton.DisclosureSpace: Integer;
begin
  Result := 0;

  if (FDisclosureCue <> ndcNone) and (FPresentation = nbpControl) then
  begin
    Result := 2 * Max(4, Abs(Font.Height) div 3) + Max(8, Abs(Font.Height) div 2);
  end;
end;

procedure TNyxLCLButton.Disclosure(ACue: TNyxLCLDisclosureCue);
begin

  if (Ord(ACue) < Ord(Low(TNyxLCLDisclosureCue))) or
    (Ord(ACue) > Ord(High(TNyxLCLDisclosureCue))) then
  begin
    raise ENyxModel.Create('Unknown native disclosure cue');
  end;

  if FDisclosureCue = ACue then
  begin
    Exit;
  end;
  FDisclosureCue := ACue;
  InvalidatePreferredSize;
  Invalidate;
end;

procedure TNyxLCLButton.ApplyTheme(ATheme: TNyxTheme; const AVariant: TNyxText);
var
  LVariant: TNyxVariant;
  LFound: Boolean;
  LAppearance: TNyxButtonAppearance;
begin

  if ATheme = nil then
  begin
    raise ENyxModel.Create('Button theme is required');
  end;
  ATheme.Validate;
  FAccentColor := NyxLCLColor(ATheme.Accent);
  FRadius := ATheme.ControlRadius;
  ParentColor := False;
  LFound := False;
  for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
  begin

    if NyxVariantName(LVariant) = AVariant then
    begin
      LFound := True;
      Break;
    end;
  end;
  { Custom browser style references remain open. The standard LCL face keeps
    its documented default fallback until a custom adapter supplies that style. }

  if not LFound then
  begin
    LVariant := nvDefault;
  end;
  LAppearance := ATheme.ButtonAppearance(LVariant);
  Color := NyxLCLColor(LAppearance.Background);
  FTextColor := NyxLCLColor(LAppearance.Foreground);
  FBorderColor := NyxLCLColor(LAppearance.Border);
  FTransparentFace := LAppearance.Transparent;
  FBorderVisible := LAppearance.BorderVisible;
  Font.Height := -ATheme.FontSize;
  Font.Color := FTextColor;

  { Assign only the final native weight. Switching a regular face through bold
    fires two Font.OnChange/preferred-size notifications on every unchanged
    renderer Sync. Native inherit/semibold retain their existing bold mapping;
    ordinary TFont setters still repair externally changed styles. }

  if FButtonFace.Weight = nbwRegular then
  begin
    Font.Style := [];
  end
  else
  begin
    Font.Style := [fsBold];
  end;
  ParentFont := False;
  Invalidate;
end;

procedure TNyxLCLButton.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited KeyDown(AKey, AShift);

  if AKey = 0 then
  begin
    Exit;
  end;

  if AKey in [VK_SPACE, VK_RETURN] then
  begin
    FKeyboardActivation := AKey;
  end;
end;

procedure TNyxLCLButton.KeyUp(var AKey: Word; AShift: TShiftState);
var
  LOriginal: Word;
  LActivate: Boolean;
  LHandler: TKeyEvent;
begin
  LOriginal := AKey;
  LActivate := (FKeyboardActivation = AKey) and (AKey in [VK_SPACE, VK_RETURN]);
  LHandler := OnKeyUp;

  if FKeyboardActivation = AKey then
  begin
    FKeyboardActivation := 0;
  end;

  if AKey in [VK_SPACE, VK_RETURN] then
  begin
    DoButtonUp;
  end;
  { The ancestor's final TWinControl.KeyUp operation is its OnKeyUp slot. Invoke
    that slot exactly once before Click, retaining the existing button-state
    operations without the ancestor's unconditional premature activation. }

  if Assigned(LHandler) then
  begin
    LHandler(Self, AKey, AShift);
  end;

  if AKey = 0 then
  begin
    Exit;
  end;

  if LActivate and (AKey = LOriginal) then
  begin
    Click;
  end;
end;

procedure TNyxLCLButton.DoExit;
begin
  FKeyboardActivation := 0;
  DoButtonUp;
  inherited DoExit;
end;

procedure TNyxLCLButton.Click;
begin

  if IsEnabled then
  begin
    inherited Click;
  end;
end;

procedure TNyxLCLButton.Paint;
var
  LBounds: TRect;
  LTextStyle: TTextStyle;
  LFace: TColor;
  LText: TColor;
  LCueHalf: Integer;
  LCueSpace: Integer;
  LCueLeft: Integer;
  LCueCenter: Integer;
  LGlyphSpace, LGroupWidth, LGroupLeft: Integer;
  LCaption: String;
begin

  if (Width < 1) or (Height < 1) then
  begin
    Exit;
  end;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Backdrop(Self);
  Canvas.FillRect(Rect(0, 0, Width, Height));
  LFace := Color;
  LText := FTextColor;

  if FTransparentFace then
  begin
    { A standard solid parent supplies the idle ghost backdrop. Hover/press
      feedback still uses the existing native interaction state and admission. }
    LFace := Backdrop(Self);
  end;

  if not IsEnabled or ((FPresentation = nbpMenuItem) and not FMenuEnabled) then
  begin
    LFace := Blend(LFace, Backdrop(Self), 50);
    LText := Blend(LText, LFace, 50);
  end
  else if csfSunken in FState then
  begin
    LFace := Blend(LFace, FTextColor, 12);
  end
  else if MouseInClient then
  begin
    LFace := Blend(LFace, FAccentColor, 10);
  end;
  LBounds := Rect(1, 1, Width - 1, Height - 1);
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FBorderColor;

  if not FBorderVisible then
  begin
    Canvas.Pen.Style := psClear;
  end;
  Canvas.Brush.Color := LFace;
  Face(Canvas, LBounds, FRadius);

  if Focused and IsEnabled then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := FTextColor;
    Canvas.Pen.Width := 2;
    Face(Canvas, Rect(4, 4, Width - 4, Height - 4), FRadius - 2);
  end;
  Canvas.Font.Assign(Font);
  Canvas.Font.Color := LText;
  Canvas.Brush.Style := bsClear;
  LTextStyle := Canvas.TextStyle;
  LTextStyle.Alignment := taCenter;
  LCaption := Caption;

  if LCaption = '' then
  begin
    LCaption := AccessibleName;
  end;
  LGlyphSpace := 0;

  if not FGlyph.Empty and (FButtonFace.ContentMode <> nbcCaption) then
  begin
    LGlyphSpace := FButtonFace.GlyphBoxSize;

    if FButtonFace.ContentMode = nbcGlyphOnly then
    begin
      LCaption := '';
    end
    else if LCaption <> '' then
    begin
      Inc(LGlyphSpace, FButtonFace.GlyphSpacing);
    end;
  end;
  LGroupWidth := LGlyphSpace + Canvas.TextWidth(LCaption);
  LGroupLeft := Max(12, (Width - LGroupWidth) div 2);

  if FButtonFace.ContentAlignment = nbaStart then
  begin
    LGroupLeft := 12;
  end;

  if LGlyphSpace > 0 then
  begin
    Canvas.Draw(LGroupLeft + (FButtonFace.GlyphBoxSize - FGlyph.Width) div 2,
      (Height - FGlyph.Height) div 2, FGlyph);
    LTextStyle.Alignment := taLeftJustify;
    LBounds.Left := LGroupLeft + LGlyphSpace;
    LBounds.Right := Width - 12;
  end
  else if FButtonFace.ContentAlignment = nbaStart then
  begin
    LTextStyle.Alignment := taLeftJustify;
    LBounds.Left := 12;
    LBounds.Right := Width - 12;
  end;
  LCueSpace := DisclosureSpace;

  if LCueSpace > 0 then
  begin
    { Center the complete cue/caption group, preserving a distinct caption for
      accessibility and custom text. Drawn lines avoid font-dependent symbols. }
    LCueHalf := Max(4, Abs(Font.Height) div 3);
    LCueLeft := Max(16, (Width - Canvas.TextWidth(Caption) - LCueSpace) div 2);
    LCueCenter := Height div 2;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := LText;
    Canvas.Pen.Width := Max(2, Abs(Font.Height) div 10);

    if FDisclosureCue = ndcExpanded then
    begin
      Canvas.MoveTo(LCueLeft, LCueCenter - LCueHalf div 2);
      Canvas.LineTo(LCueLeft + LCueHalf, LCueCenter + LCueHalf div 2);
      Canvas.LineTo(LCueLeft + 2 * LCueHalf, LCueCenter - LCueHalf div 2);
    end
    else
    begin
      Canvas.MoveTo(LCueLeft + LCueHalf div 2, LCueCenter - LCueHalf);
      Canvas.LineTo(LCueLeft + 3 * LCueHalf div 2, LCueCenter);
      Canvas.LineTo(LCueLeft + LCueHalf div 2, LCueCenter + LCueHalf);
    end;
    LTextStyle.Alignment := taLeftJustify;
    LBounds.Left := LCueLeft + LCueSpace;
    Dec(LBounds.Right, 16);
  end;

  if FPresentation = nbpMenuItem then
  begin
    LTextStyle.Alignment := taLeftJustify;
    Inc(LBounds.Left, 16);
    Dec(LBounds.Right, 16);
  end;
  LTextStyle.Layout := tlCenter;
  LTextStyle.SingleLine := True;
  LTextStyle.Wordbreak := False;
  LTextStyle.ShowPrefix := False;
  { Left-justified TextRect uses its explicit text origin, unlike centered text.
    Keep that origin inside the padded menu bounds so no leading glyph clips. }
  Canvas.TextRect(LBounds, LBounds.Left, 0, LCaption, LTextStyle);
end;

procedure TNyxLCLButton.CalculatePreferredSize(var APreferredWidth,
  APreferredHeight: Integer; AWithThemeSpace: Boolean);
var
  LCaption: String;
begin
  Canvas.Font.Assign(Font);
  { Measure the same fallback Paint displays after a decorative image fails. }
  LCaption := Caption;

  if LCaption = '' then
  begin
    LCaption := AccessibleName;
  end;
  APreferredWidth := Canvas.TextWidth(LCaption) + 38;

  if not FGlyph.Empty and (FButtonFace.ContentMode <> nbcCaption) then
  begin

    if FButtonFace.ContentMode = nbcGlyphOnly then
    begin
      APreferredWidth := 24 + FButtonFace.GlyphBoxSize;
    end
    else
    begin
      Inc(APreferredWidth, FButtonFace.GlyphBoxSize + FButtonFace.GlyphSpacing);
    end;
  end;
  Inc(APreferredWidth, DisclosureSpace);
  APreferredHeight := (Abs(Font.Height) * 3 div 2) + 22;
  case FButtonFace.ControlDensity of
    nbdNormal: ; { Preserve the ordinary button's preferred height. }
    nbdCompact: APreferredHeight := Max(32, Abs(Font.Height) + 16);
    nbdTouch: APreferredHeight := Max(44, Abs(Font.Height) + 24);
  end;

  if not FGlyph.Empty and (FButtonFace.ContentMode <> nbcCaption) then
  begin
    APreferredHeight := Max(APreferredHeight, FButtonFace.GlyphBoxSize + 8);
  end;
end;

constructor TNyxLCLSurface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Caption := '';
  DoubleBuffered := True;
end;

procedure TNyxLCLSurface.ApplyTheme(ATheme: TNyxTheme; AControlSurface: Boolean);
begin

  if ATheme = nil then
  begin
    raise ENyxModel.Create('Surface theme is required');
  end;
  ATheme.Validate;
  Color := NyxLCLColor(ATheme.Surface);
  FBorderColor := NyxLCLColor(ATheme.Border);
  FRadius := ATheme.Radius;
  FAccentColor := NyxLCLColor(ATheme.Accent);
  FControlSurface := AControlSurface;

  if AControlSurface then
  begin
    FRadius := ATheme.ControlRadius;
  end;
  ParentColor := False;
  Invalidate;
end;

procedure TNyxLCLSurface.Paint;
var
  LForm: TCustomForm;
  LBounds: TRect;
begin

  if (Width < 1) or (Height < 1) then
  begin
    Exit;
  end;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Backdrop(Self);
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Canvas.Brush.Color := Color;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FBorderColor;
  LForm := GetParentForm(Self);

  if FControlSurface and IsEnabled and (LForm <> nil) and
    (LForm.ActiveControl <> nil) and (LForm.ActiveControl.Parent = Self) then
  begin
    Canvas.Pen.Color := FAccentColor;
  end;
  LBounds := Rect(0, 0, Width, Height);

  if FViewportFace.Defined then
  begin
    LBounds := Rect(FViewportFace.X, FViewportFace.Y,
      FViewportFace.Right, FViewportFace.Bottom);
  end;
  Face(Canvas, LBounds, FRadius);
end;

procedure TNyxLCLSurface.ProjectFace(const ABounds: TNyxViewportBox);
begin

  if (FViewportFace.Defined = ABounds.Defined) and
    (FViewportFace.X = ABounds.X) and (FViewportFace.Y = ABounds.Y) and
    (FViewportFace.Width = ABounds.Width) and (FViewportFace.Height = ABounds.Height) then
  begin
    Exit;
  end;
  FViewportFace := ABounds;
  Invalidate;
end;

end.
