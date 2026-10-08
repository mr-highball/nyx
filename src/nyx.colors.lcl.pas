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
unit nyx.colors.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Math, Types, Forms, Controls, StdCtrls, ComCtrls, EditBtn,
  ColorBox, ExtCtrls, Graphics, LCLType,
  nyx.text, nyx.colors, nyx.contract, nyx.focus.lcl;

type
  { Native grouped hex editor and owned nonmodal RGB/palette picker. Popup
    channels are proposals until Use color; opening/cancel never writes state.
    This field owns its popup/widgets. Renderer callbacks and Editor are
    borrowed, revoked before retirement; no document or renderer is retained.
    Standard LCL palette/trackbars provide native keyboard behavior. }
  TNyxLCLColorField = class(TEditButton)
  private
    FPopup: TForm;
    FPalette: TColorBox;
    FRed: TTrackBar;
    FGreen: TTrackBar;
    FBlue: TTrackBar;
    FSwatch: TShape;
    FStatus: TLabel;
    FAccept: TButton;
    FCancel: TButton;
    FClear: TButton;
    FDomain: TNyxValueDomain;
    FAccepted: TNyxRGBColor;
    FUpdating: Boolean;
    FClosing: Boolean;
    FDisconnected: Boolean;
    function GetEditor: TCustomEdit;
    procedure CreatePopup;
    procedure ClosePopup(AReturnFocus: Boolean);
    procedure ChannelChanged(ASender: TObject);
    procedure PaletteChanged(ASender: TObject);
    procedure PopupKeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure PopupDeactivate(ASender: TObject);
    procedure PopupClose(ASender: TObject; var AAction: TCloseAction);
    procedure AcceptClick(ASender: TObject);
    procedure CancelClick(ASender: TObject);
    procedure ClearClick(ASender: TObject);
    procedure Publish(const AValue: TNyxRGBColor);
    function PickerValue: TNyxRGBColor;
  protected
    procedure ButtonClick; override;
    procedure EditEditingDone; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Policy copies preserve choices. A changed policy retires the popup context;
      unchanged policy leaves pending proposals and editor drafts untouched. }
    procedure SetDomain(const AValue: TNyxValueDomain);
    { Copy an accepted value without rewriting a physical text draft. A change,
      including wire spelling, revokes any older open proposal. }
    procedure SetAcceptedValue(const AValue: TNyxRGBColor);
    { Ancestor policy revokes popup publication immediately when synchronized. }
    procedure SetInteraction(AEnabled, AReadOnly, AVisible: Boolean);
    { Revoke borrowed callbacks and conceal the owned popup before retirement. }
    procedure Disconnect;
    property Editor: TCustomEdit read GetEditor;
    { Borrowed native widgets for hosts/qualification; nil before first opening.
      They are never transferable and must not outlive this field. }
    property Popup: TForm read FPopup;
    property Palette: TColorBox read FPalette;
    property RedSlider: TTrackBar read FRed;
    property GreenSlider: TTrackBar read FGreen;
    property BlueSlider: TTrackBar read FBlue;
    property AcceptButton: TButton read FAccept;
    property CancelButton: TButton read FCancel;
    property ClearButton: TButton read FClear;
    property Status: TLabel read FStatus;
  end;

implementation

constructor TNyxLCLColorField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomain := NyxRGBDomain.Definition;
  FAccepted := NyxNoColor;
  ButtonCaption := '...';
  ButtonHint := 'Choose a color';
  Button.ShowHint := True;
  Button.AccessibleName := 'Choose a color';
  FocusOnButtonClick := False;
end;

function TNyxLCLColorField.GetEditor: TCustomEdit;
begin
  Result := BaseEditor;
end;

procedure TNyxLCLColorField.EditEditingDone;
begin

  if not FDisconnected and Assigned(OnEditingDone) then
  begin
    OnEditingDone(Self);
  end;
end;

procedure TNyxLCLColorField.SetDomain(const AValue: TNyxValueDomain);
var
  LCandidate: TNyxValueDomain;
begin
  LCandidate := NyxRGBDomain(AValue).Definition;

  if LCandidate.ToData.ToJSON = FDomain.ToData.ToJSON then
  begin
    Exit;
  end;
  FDomain := LCandidate;
  ClosePopup(False);
end;

procedure TNyxLCLColorField.SetAcceptedValue(const AValue: TNyxRGBColor);
begin

  if not FAccepted.SameColor(AValue) or (FAccepted.ToText <> AValue.ToText) then
  begin
    FAccepted := AValue;
    ClosePopup(False);
  end;
end;

procedure TNyxLCLColorField.SetInteraction(AEnabled, AReadOnly, AVisible: Boolean);
begin
  Enabled := AEnabled;
  ReadOnly := AReadOnly;
  Button.Enabled := AEnabled and not AReadOnly and not FDisconnected;

  if not AEnabled or AReadOnly or not AVisible then
  begin
    ClosePopup(False);
  end;
end;

procedure TNyxLCLColorField.CreatePopup;

  function Channel(const ACaption: TNyxText; ATop, AOrder: Integer): TTrackBar;
  var
    LLabel: TLabel;
  begin
    LLabel := TLabel.Create(FPopup);
    LLabel.Parent := FPopup;
    LLabel.Caption := ACaption;
    LLabel.SetBounds(12, ATop + 6, 48, 20);
    Result := TTrackBar.Create(FPopup);
    Result.Parent := FPopup;
    Result.SetBounds(60, ATop, 220, 34);
    Result.Min := 0;
    Result.Max := 255;
    Result.TickStyle := tsNone;
    Result.TabOrder := AOrder;
    Result.AccessibleName := ACaption;
    Result.OnChange := ChannelChanged;
    Result.OnKeyDown := PopupKeyDown;
    LLabel.FocusControl := Result;
  end;

  function Action(const ACaption: TNyxText; ALeft, AWidth, AOrder: Integer;
    AClick: TNotifyEvent): TButton;
  begin
    Result := TButton.Create(FPopup);
    Result.Parent := FPopup;
    Result.Caption := ACaption;
    Result.SetBounds(ALeft, 200, AWidth, 32);
    Result.TabOrder := AOrder;
    Result.AccessibleName := ACaption;
    Result.OnClick := AClick;
    Result.OnKeyDown := PopupKeyDown;
  end;

begin
  FPopup := TForm.CreateNew(Self);
  FPopup.Caption := 'Choose a color';
  FPopup.BorderStyle := bsToolWindow;
  FPopup.ShowInTaskBar := stNever;
  FPopup.Position := poDesigned;
  FPopup.Font.Assign(Font);
  FPopup.ClientWidth := 300;
  FPopup.ClientHeight := 244;
  FPopup.OnDeactivate := PopupDeactivate;
  FPopup.OnClose := PopupClose;
  FPalette := TColorBox.Create(FPopup);
  FPalette.Parent := FPopup;
  { cbCustomColor opens an unrelated modal dialog. This owned nonmodal picker
    already supplies all 24-bit values through its RGB sliders. }
  FPalette.Style := [cbStandardColors, cbExtendedColors];
  FPalette.SetBounds(12, 12, 214, 30);
  FPalette.AccessibleName := 'Color palette';
  FPalette.OnChange := PaletteChanged;
  FPalette.OnKeyDown := PopupKeyDown;
  FSwatch := TShape.Create(FPopup);
  FSwatch.Parent := FPopup;
  FSwatch.SetBounds(238, 12, 48, 30);
  FRed := Channel('Red', 52, 1);
  FGreen := Channel('Green', 92, 2);
  FBlue := Channel('Blue', 132, 3);
  FStatus := TLabel.Create(FPopup);
  FStatus.Parent := FPopup;
  FStatus.AutoSize := False;
  FStatus.SetBounds(12, 170, 276, 24);
  FStatus.AccessibleName := 'Color validation';
  FClear := Action('Clear', 12, 60, 4, ClearClick);
  FCancel := Action('Cancel', 124, 72, 5, CancelClick);
  FCancel.Cancel := True;
  FAccept := Action('Use color', 204, 84, 6, AcceptClick);
  FAccept.Default := True;
end;

function TNyxLCLColorField.PickerValue: TNyxRGBColor;
begin
  Result := NyxRGB(FRed.Position, FGreen.Position, FBlue.Position);
end;

procedure TNyxLCLColorField.ChannelChanged(ASender: TObject);
var
  LValue: TNyxRGBColor;
begin

  if FUpdating or FDisconnected then
  begin
    Exit;
  end;
  LValue := PickerValue;
  FSwatch.Brush.Color := RGBToColor(LValue.Red, LValue.Green, LValue.Blue);
  FStatus.Caption := LValue.ToText;
end;

procedure TNyxLCLColorField.PaletteChanged(ASender: TObject);
var
  LColor: TColor;
begin

  if FUpdating or FDisconnected then
  begin
    Exit;
  end;
  LColor := ColorToRGB(FPalette.Selected);
  FUpdating := True;
  try
    FRed.Position := LColor and 255;
    FGreen.Position := (LColor shr 8) and 255;
    FBlue.Position := (LColor shr 16) and 255;
  finally
    FUpdating := False;
  end;
  ChannelChanged(Self);
end;

procedure TNyxLCLColorField.ButtonClick;
var
  LValue: TNyxRGBColor;
  LPosition: TPoint;
  LWork: TRect;
  LLease: TNyxLCLFocusReturn;
begin

  if FDisconnected or not IsEnabled or not IsVisible or ReadOnly then
  begin
    Exit;
  end;

  if FPopup = nil then
  begin
    CreatePopup;
  end;

  if not TryNyxRGB(TNyxText(Text), LValue) then
  begin
    LValue := FAccepted;
  end;

  if not LValue.Defined then
  begin
    { Suggested black is only a proposal, never an implicit empty conversion. }
    LValue := NyxRGB(0, 0, 0);
  end;
  FUpdating := True;
  try
    FRed.Position := LValue.Red;
    FGreen.Position := LValue.Green;
    FBlue.Position := LValue.Blue;
    FPalette.Selected := RGBToColor(LValue.Red, LValue.Green, LValue.Blue);
  finally
    FUpdating := False;
  end;
  ChannelChanged(Self);
  LPosition := ClientToScreen(Point(0, Height));
  LWork := Screen.MonitorFromPoint(LPosition).WorkareaRect;
  FPopup.Left := LPosition.X;
  FPopup.Top := LPosition.Y;
  LLease := TNyxLCLFocusReturn.CreateFor(Self, Editor);
  try
    FPopup.Show;

    if not LLease.ContextAlive or FDisconnected then
    begin
      Exit;
    end;
    FPopup.ClientWidth := 300;
    FPopup.ClientHeight := 244;
    FPopup.Left := Max(LWork.Left, Min(FPopup.Left, LWork.Right - FPopup.Width));
    FPopup.Top := Max(LWork.Top, Min(FPopup.Top, LWork.Bottom - FPopup.Height));
    FPalette.SetFocus;
  finally
    LLease.Free;
  end;
end;

procedure TNyxLCLColorField.Publish(const AValue: TNyxRGBColor);
var
  LReturn: TNyxLCLFocusReturn;
begin

  if FDisconnected or not IsEnabled or not IsVisible or ReadOnly then
  begin
    ClosePopup(False);
    Exit;
  end;
  try
    FDomain.ReadWire(AValue.ToText);
  except
    on LException: ENyxContract do
    begin
      FStatus.Caption := LException.Message;
      Exit;
    end;
  end;
  LReturn := TNyxLCLFocusReturn.CreateFor(Self, Editor);
  try
    LReturn.Capture;

    if not LReturn.ContextAlive or FDisconnected then
    begin
      Exit;
    end;
    try
      Text := AValue.ToText;

      if LReturn.ContextAlive and not FDisconnected and Assigned(OnEditingDone) then
      begin
        OnEditingDone(Self);
      end;
    finally

      if LReturn.ContextAlive and not FDisconnected then
      begin
        LReturn.BeforeConceal;
        ClosePopup(False);
      end;
    end;

    if LReturn.ContextAlive and not FDisconnected then
    begin
      LReturn.Restore;
    end;
  finally
    LReturn.Free;
  end;
end;

procedure TNyxLCLColorField.AcceptClick(ASender: TObject);
begin

  if not FDisconnected and (FPopup <> nil) and FPopup.Visible then
  begin
    Publish(PickerValue);
  end;
end;

procedure TNyxLCLColorField.ClearClick(ASender: TObject);
begin

  if not FDisconnected and (FPopup <> nil) and FPopup.Visible then
  begin
    Publish(NyxNoColor);
  end;
end;

procedure TNyxLCLColorField.CancelClick(ASender: TObject);
begin
  ClosePopup(True);
end;

procedure TNyxLCLColorField.PopupKeyDown(ASender: TObject; var AKey: Word;
  AShift: TShiftState);
begin

  if AShift <> [] then
  begin
    Exit;
  end;

  if AKey = VK_ESCAPE then
  begin
    AKey := 0;
    ClosePopup(True);
  end
  else if AKey = VK_RETURN then
  begin
    AKey := 0;
    AcceptClick(ASender);
  end;
end;

procedure TNyxLCLColorField.ClosePopup(AReturnFocus: Boolean);
var
  LReturn: TNyxLCLFocusReturn;
begin

  if FClosing or (FPopup = nil) or not FPopup.Visible then
  begin
    Exit;
  end;
  LReturn := TNyxLCLFocusReturn.CreateFor(Self, Editor);
  try
    FClosing := True;
    LReturn.Capture;
    LReturn.BeforeConceal;
    try
      FPopup.Hide;
    finally

      if LReturn.ContextAlive then
      begin
        FClosing := False;
      end;
    end;

    if LReturn.ContextAlive and AReturnFocus and not FDisconnected then
    begin
      LReturn.Restore;
    end;
  finally
    LReturn.Free;
  end;
end;

procedure TNyxLCLColorField.PopupDeactivate(ASender: TObject);
begin
  ClosePopup(False);
end;

procedure TNyxLCLColorField.PopupClose(ASender: TObject; var AAction: TCloseAction);
begin
  AAction := caNone;
  ClosePopup(True);
end;

procedure TNyxLCLColorField.Disconnect;
begin
  FDisconnected := True;
  OnChange := nil;
  OnEditingDone := nil;
  ClosePopup(False);
end;

destructor TNyxLCLColorField.Destroy;
begin
  Disconnect;

  if FPopup <> nil then
  begin
    FPopup.OnDeactivate := nil;
    FPopup.OnClose := nil;
  end;
  FreeAndNil(FPopup);
  inherited Destroy;
end;

end.
