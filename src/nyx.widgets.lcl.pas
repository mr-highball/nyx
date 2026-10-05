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
  nyx.layout.viewport,
  nyx.theme;

type
  { Themed Lazarus button. TCDButton supplies window/control ownership, tab
    navigation, pointer capture, Space/Enter activation and action integration.
    Nyx reuses its state/activation operations, with keyboard callbacks ordered
    before activation so consumed keys cannot cause a default click.
    Theme fields are copied during ApplyTheme: controls retain no theme pointer.
    Custom factories may still return standard TButton or other LCL controls. }
  TNyxLCLButton = class(TCDButton)
  private
    FBorderColor: TColor;
    FAccentColor: TColor;
    FTextColor: TColor;
    FRadius: Integer;
    FKeyboardActivation: Word;
  protected
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
    { Snapshot a validated palette. Variant primary uses Accent/AccentText;
      other variants use Surface/Text. Call again after an intentional style edit. }
    procedure ApplyTheme(ATheme: TNyxTheme; const AVariant: TNyxText);
    procedure Paint; override;
    { Expose the inherited activation entry point for applications/harnesses.
      Disabled controls (including disabled ancestors) must never activate. }
    procedure Click; override;
    property BorderColor: TColor read FBorderColor;
    property CornerRadius: Integer read FRadius;
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
  nyx.model;

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
end;

procedure TNyxLCLButton.ApplyTheme(ATheme: TNyxTheme; const AVariant: TNyxText);
begin

  if ATheme = nil then
  begin
    raise ENyxModel.Create('Button theme is required');
  end;
  ATheme.Validate;
  FAccentColor := NyxLCLColor(ATheme.Accent);
  FBorderColor := NyxLCLColor(ATheme.Border);
  FRadius := ATheme.ControlRadius;
  ParentColor := False;

  if AVariant = 'primary' then
  begin
    Color := FAccentColor;
    FTextColor := NyxLCLColor(ATheme.AccentText);
    FBorderColor := FAccentColor;
  end
  else
  begin
    Color := NyxLCLColor(ATheme.Surface);
    FTextColor := NyxLCLColor(ATheme.Text);
  end;
  Font.Height := -ATheme.FontSize;
  Font.Color := FTextColor;
  Font.Style := [fsBold];
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

  if not IsEnabled then
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
  Canvas.Brush.Color := LFace;
  Face(Canvas, LBounds, FRadius);

  if Focused and IsEnabled then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := FTextColor;
    Canvas.Pen.Width := 2;
    Face(Canvas, Rect(4, 4, Width - 4, Height - 4), FRadius - 2);
  end;
  Canvas.Font.Assign(Font);
  Canvas.Font.Color := LText;
  Canvas.Brush.Style := bsClear;
  LTextStyle := Canvas.TextStyle;
  LTextStyle.Alignment := taCenter;
  LTextStyle.Layout := tlCenter;
  LTextStyle.SingleLine := True;
  LTextStyle.Wordbreak := False;
  LTextStyle.ShowPrefix := False;
  Canvas.TextRect(LBounds, 0, 0, Caption, LTextStyle);
end;

procedure TNyxLCLButton.CalculatePreferredSize(var APreferredWidth,
  APreferredHeight: Integer; AWithThemeSpace: Boolean);
begin
  Canvas.Font.Assign(Font);
  APreferredWidth := Canvas.TextWidth(Caption) + 38;
  APreferredHeight := (Abs(Font.Height) * 3 div 2) + 22;
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
