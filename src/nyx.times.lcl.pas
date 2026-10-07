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

unit nyx.times.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, StdCtrls, ComCtrls, EditBtn, LCLType,
  nyx.text, nyx.times, nyx.data, nyx.contract, nyx.focus.lcl;

type
  { Exact integer editor for one clock part. The edit owns its native arrow
    control; the popup parents both widgets. Deliberately do not associate the
    arrows with the edit: widgetset buddy/spin parsing can replace invalid text
    on focus loss before Nyx's admission sees it. Text remains an independent
    draft. Value refuses incomplete, non-ASCII, fractional and out-of-range
    drafts; explicit integer assignment refuses an invalid range. Arrow/keyboard
    stepping changes only a complete admitted part and clamps at its bounds. }
  TNyxLCLClockPart = class(TEdit)
  private
    FStepper: TUpDown;
    FMaximum: Integer;
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
    procedure SetMaximum(AValue: Integer);
    procedure Step(ADirection: Integer);
    procedure StepClick(ASender: TObject; AButton: TUDBtnType);
  protected
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Failure leaves Text unchanged and does not supply a substitute reading. }
    function TryValue(out AValue: Integer): Boolean;
    property Value: Integer read GetValue write SetValue;
    property Maximum: Integer read FMaximum write SetMaximum;
    { Borrowed native arrow control; it must not outlive this editor. }
    property Stepper: TUpDown read FStepper;
  end;

  { Native clock field retains LCL's grouped editor and clock button. Nyx owns
    exact text/precision and its own popup; TTimeEdit's locale parser, floating
    Time property and globally created hour/minute popup are never used.
    The renderer borrows Editor and callbacks. This field owns its popup and
    children, revoking callbacks before physical retirement. }
  TNyxLCLTimeField = class(TTimeEdit)
  private
    FPopup: TForm;
    FHour: TNyxLCLClockPart;
    FMinute: TNyxLCLClockPart;
    FSecond: TNyxLCLClockPart;
    FMillisecond: TNyxLCLClockPart;
    FAccept: TButton;
    FCancel: TButton;
    FClear: TButton;
    FStatus: TLabel;
    FDomain: TNyxValueDomain;
    FAcceptedTime: TNyxClockTime;
    FPopupPrecision: TNyxTimePrecision;
    FClosing: Boolean;
    FDisconnected: Boolean;
    function GetEditor: TCustomEdit;
    procedure CreatePopup;
    procedure ClosePopup(AReturnFocus: Boolean);
    procedure PopupKeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure PopupDeactivate(ASender: TObject);
    procedure PopupClose(ASender: TObject; var AAction: TCloseAction);
    procedure AcceptClick(ASender: TObject);
    procedure CancelClick(ASender: TObject);
    procedure ClearClick(ASender: TObject);
    procedure Publish(const AValue: TNyxClockTime);
    function PickerValue: TNyxClockTime;
  protected
    procedure ButtonClick; override;
    { The inherited double-click would open Lazarus's unrelated popup. }
    procedure EditDblClick; override;
    { Complete and incomplete native text are drafts until editing completion.
      Preserve grouped forwarding instead of installing another inner handler. }
    procedure EditEditingDone; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Copy admitted clock policy. A changed policy cancels the old popup context;
      it never rounds a value, moves a bound or mutates an unfinished draft. }
    procedure SetDomain(const AValue: TNyxValueDomain);
    { The accepted runtime reading is independent of physical editor text. }
    procedure SetAcceptedValue(const AValue: TNyxClockTime);
    { Ancestor disabled/read-only/hidden policy also closes the owned popup. }
    procedure SetInteraction(AEnabled, AReadOnly, AVisible: Boolean);
    procedure Disconnect;
    property Editor: TCustomEdit read GetEditor;
    { Borrowed exact native parts for hosts and physical qualification. Nil
      means the popup has not been created; none may outlive this field. }
    property Popup: TForm read FPopup;
    property HourControl: TNyxLCLClockPart read FHour;
    property MinuteControl: TNyxLCLClockPart read FMinute;
    property SecondControl: TNyxLCLClockPart read FSecond;
    property MillisecondControl: TNyxLCLClockPart read FMillisecond;
    property AcceptButton: TButton read FAccept;
    property CancelButton: TButton read FCancel;
    property ClearButton: TButton read FClear;
    property StatusLabel: TLabel read FStatus;
  end;

implementation

constructor TNyxLCLClockPart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaximum := 999;
  FStepper := TUpDown.Create(Self);
  FStepper.Min := -1;
  FStepper.Max := 1;
  FStepper.Position := 0;
  FStepper.ArrowKeys := False;
  FStepper.TabStop := False;
  FStepper.OnClick := StepClick;
  Text := '0';
end;

destructor TNyxLCLClockPart.Destroy;
begin

  if FStepper <> nil then
  begin
    FStepper.OnClick := nil;
  end;
  inherited Destroy;
end;

procedure TNyxLCLClockPart.SetMaximum(AValue: Integer);
begin

  if (AValue < 0) or (AValue > 999) then
  begin
    raise ENyxTimeValue.Create('A clock part bound must be between zero and 999.');
  end;
  FMaximum := AValue;
end;

function TNyxLCLClockPart.TryValue(out AValue: Integer): Boolean;
var
  LText: TNyxText;
  LIndex: Integer;
begin
  Result := False;
  AValue := 0;
  LText := Text;

  if LText = '' then
  begin
    Exit;
  end;
  for LIndex := 1 to Length(LText) do
  begin

    if (LText[LIndex] < '0') or (LText[LIndex] > '9') then
    begin
      Exit;
    end;
  end;
  Result := TryStrToInt(LText, AValue) and (AValue >= 0) and (AValue <= FMaximum);
end;

function TNyxLCLClockPart.GetValue: Integer;
begin

  if not TryValue(Result) then
  begin
    raise ENyxTimeValue.Create('Complete each clock part with whole digits within its range.');
  end;
end;

procedure TNyxLCLClockPart.SetValue(AValue: Integer);
begin

  if (AValue < 0) or (AValue > FMaximum) then
  begin
    raise ENyxTimeValue.Create('A clock part value is outside its range.');
  end;
  Text := IntToStr(AValue);
end;

procedure TNyxLCLClockPart.Step(ADirection: Integer);
var
  LValue: Integer;
begin

  if not IsEnabled or ReadOnly or not TryValue(LValue) then
  begin
    Exit;
  end;
  LValue := LValue + ADirection;

  if LValue < 0 then
  begin
    LValue := 0;
  end;

  if LValue > FMaximum then
  begin
    LValue := FMaximum;
  end;
  Value := LValue;
end;

procedure TNyxLCLClockPart.StepClick(ASender: TObject; AButton: TUDBtnType);
begin
  { Keep the arrow position relative. Its native numeric buddy is absent, so
    neither focus loss nor an invalid pasted draft invokes a floating parser. }
  FStepper.Position := 0;

  if AButton = btNext then
  begin
    Step(1);
  end
  else
  begin
    Step(-1);
  end;
end;

procedure TNyxLCLClockPart.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited KeyDown(AKey, AShift);
  { A consumed Enter/Escape may publish and retire the entire field. Only the
    caller-owned key/shift arguments are read before returning from that path. }

  if (AKey = 0) or (AShift <> []) then
  begin
    Exit;
  end;

  if AKey = VK_UP then
  begin
    AKey := 0;
    Step(1);
  end
  else if AKey = VK_DOWN then
  begin
    AKey := 0;
    Step(-1);
  end;
end;

constructor TNyxLCLTimeField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomain := NyxTimeDomain.Definition;
  FAcceptedTime := NyxNoTime;
  DefaultNow := False;
  ButtonHint := 'Choose a time';
  Button.ShowHint := True;
  Button.AccessibleName := 'Choose a time';
  FocusOnButtonClick := False;
end;

function TNyxLCLTimeField.GetEditor: TCustomEdit;
begin
  Result := BaseEditor;
end;

procedure TNyxLCLTimeField.EditEditingDone;
begin
  { Do not call TTimeEdit.ParseInput: it accepts locale shorthand, normalizes
    spelling and can discard explicit seconds/fractions. Shared admission owns
    validation/restoration at the renderer's ordinary commit boundary. }

  if not FDisconnected and Assigned(OnEditingDone) then
  begin
    OnEditingDone(Self);
  end;
end;

procedure TNyxLCLTimeField.EditDblClick;
begin
  { The renderer dispatches Nyx's pointer phases on the real editor. Retain the
    grouped native callback as well, after opening only the owned picker; that
    borrowed callback may retire this field, so no access follows it. }
  ButtonClick;

  if not FDisconnected and Assigned(OnDblClick) then
  begin
    OnDblClick(Self);
  end;
end;

procedure TNyxLCLTimeField.SetDomain(const AValue: TNyxValueDomain);
var
  LCandidate: TNyxValueDomain;
begin
  AValue.Validate;

  if not AValue.ClockTime then
  begin
    raise ENyxContract.Create('A native clock picker requires a time domain');
  end;
  LCandidate := AValue.Copy;

  if LCandidate.ToData.ToJSON = FDomain.ToData.ToJSON then
  begin
    Exit;
  end;
  FDomain := LCandidate;
  ClosePopup(False);
end;

procedure TNyxLCLTimeField.SetAcceptedValue(const AValue: TNyxClockTime);
begin
  FAcceptedTime := AValue;
end;

procedure TNyxLCLTimeField.SetInteraction(AEnabled, AReadOnly, AVisible: Boolean);
begin
  Enabled := AEnabled;
  ReadOnly := AReadOnly;
  Button.Enabled := AEnabled and not AReadOnly and not FDisconnected;

  if not AEnabled or AReadOnly or not AVisible then
  begin
    ClosePopup(False);
  end;
end;

procedure TNyxLCLTimeField.CreatePopup;

  function Part(const ACaption: TNyxText; ALeft, AWidth, AMaximum,
    AOrder: Integer): TNyxLCLClockPart;
  var
    LLabel: TLabel;
  begin
    LLabel := TLabel.Create(FPopup);
    LLabel.Parent := FPopup;
    LLabel.Caption := ACaption;
    LLabel.SetBounds(ALeft, 12, AWidth, 20);
    Result := TNyxLCLClockPart.Create(FPopup);
    Result.Parent := FPopup;
    Result.AutoSize := False;
    Result.SetBounds(ALeft, 34, AWidth - 18, 30);
    Result.Maximum := AMaximum;
    Result.Stepper.Parent := FPopup;
    Result.Stepper.SetBounds(ALeft + AWidth - 18, 34, 18, 30);
    Result.Stepper.AccessibleName := 'Adjust ' + ACaption;
    Result.TabOrder := AOrder;
    Result.AccessibleName := ACaption;
    Result.OnKeyDown := PopupKeyDown;
    LLabel.FocusControl := Result;
  end;

  function Action(const ACaption: TNyxText; ALeft, AWidth,
    AOrder: Integer; AClick: TNotifyEvent): TButton;
  begin
    Result := TButton.Create(FPopup);
    Result.Parent := FPopup;
    Result.Caption := ACaption;
    Result.SetBounds(ALeft, 122, AWidth, 30);
    Result.TabOrder := AOrder;
    Result.AccessibleName := ACaption;
    Result.OnClick := AClick;
    Result.OnKeyDown := PopupKeyDown;
  end;

begin
  FPopup := TForm.CreateNew(Self);
  FPopup.Caption := 'Choose a time';
  FPopup.BorderStyle := bsToolWindow;
  FPopup.ShowInTaskBar := stNever;
  FPopup.Position := poDesigned;
  FPopup.Font.Assign(Font);
  FPopup.ClientWidth := 344;
  FPopup.ClientHeight := 164;
  FPopup.OnDeactivate := PopupDeactivate;
  FPopup.OnClose := PopupClose;
  FHour := Part('Hour', 12, 58, 23, 0);
  FMinute := Part('Minute', 78, 64, 59, 1);
  FSecond := Part('Second', 150, 66, 59, 2);
  FMillisecond := Part('Millisecond', 224, 108, 999, 3);
  FStatus := TLabel.Create(FPopup);
  FStatus.Parent := FPopup;
  FStatus.AutoSize := False;
  FStatus.WordWrap := True;
  FStatus.SetBounds(12, 72, 320, 44);
  FStatus.AccessibleName := 'Time validation';
  FClear := Action('Clear', 12, 72, 4, ClearClick);
  FCancel := Action('Cancel', 168, 76, 5, CancelClick);
  FCancel.Cancel := True;
  FAccept := Action('Use time', 252, 80, 6, AcceptClick);
  FAccept.Default := True;
end;

procedure TNyxLCLTimeField.ButtonClick;
var
  LTime: TNyxClockTime;
  LData: TNyxDataValue;
  LChoices: TNyxDataValue;
  LIndex: Integer;
  LPosition: TPoint;
  LWork: TRect;
begin

  if FDisconnected or not IsEnabled or not IsVisible or ReadOnly then
  begin
    Exit;
  end;

  if FPopup = nil then
  begin
    CreatePopup;
  end;

  if not TryNyxTime(TNyxText(Text), LTime) then
  begin
    LTime := FAcceptedTime;
  end;

  if not LTime.Defined then
  begin
    { Opening never changes empty into a value. A deterministic suggested reading
      may use the minimum/first nonempty choice; only explicit acceptance publishes. }
    LTime := NyxTime(0, 0);
    LData := FDomain.ToData;
    LChoices := NyxArray([]);
    for LIndex := 0 to LData.Count - 1 do
    begin

      if LData.Key(LIndex) = 'min' then
      begin
        LTime := TNyxClockTime.FromText(LData.Field('min').AsText);
      end;

      if LData.Key(LIndex) = 'choices' then
      begin
        LChoices := LData.Field('choices');
      end;
    end;
    for LIndex := 0 to LChoices.Count - 1 do
    begin

      if LChoices.Item(LIndex).AsText <> '' then
      begin
        LTime := TNyxClockTime.FromText(LChoices.Item(LIndex).AsText);
        Break;
      end;
    end;
  end;
  FPopupPrecision := LTime.Precision;
  FHour.Value := LTime.Hour;
  FMinute.Value := LTime.Minute;
  FSecond.Value := LTime.Second;
  FMillisecond.Value := LTime.Millisecond;
  FStatus.Caption := '';
  LPosition := ClientToScreen(Point(0, Height));
  LWork := Screen.MonitorFromPoint(LPosition).WorkareaRect;
  FPopup.Left := LPosition.X;
  FPopup.Top := LPosition.Y;
  FPopup.Show;

  if FDisconnected then
  begin
    Exit;
  end;
  { Set usable client size with a realized decorated window. On Win32, cached
    pre-show form bounds can describe the outer tool window and crop buttons.
    This uses LCL's client-size contract, not a guessed caption/frame height. }
  FPopup.ClientWidth := 344;
  FPopup.ClientHeight := 164;

  if FPopup.Left + FPopup.Width > LWork.Right then
  begin
    FPopup.Left := LWork.Right - FPopup.Width;
  end;

  if FPopup.Top + FPopup.Height > LWork.Bottom then
  begin
    FPopup.Top := ClientToScreen(Point(0, 0)).Y - FPopup.Height;
  end;

  if FPopup.Left < LWork.Left then
  begin
    FPopup.Left := LWork.Left;
  end;

  if FPopup.Top < LWork.Top then
  begin
    FPopup.Top := LWork.Top;
  end;
  FHour.SetFocus;
end;

function TNyxLCLTimeField.PickerValue: TNyxClockTime;
var
  LPrecision: TNyxTimePrecision;

  function PartValue(AControl: TNyxLCLClockPart): Integer;
  begin
    Result := AControl.Value;
  end;

begin
  { Each native part reads its actual draft strictly before construction; no
    locale, floating-point rounding or focus-loss substitution enters a clock. }
  Result := NyxTime(PartValue(FHour), PartValue(FMinute),
    PartValue(FSecond), PartValue(FMillisecond));
  LPrecision := FPopupPrecision;
  { Preserve explicit zero seconds/fraction and shorter fractional spelling
    when lossless. A newly chosen nonzero part increases precision as needed. }

  if (Ord(LPrecision) < Ord(ntpSecond)) and (Result.Second <> 0) then
  begin
    LPrecision := ntpSecond;
  end;

  if (Ord(LPrecision) < Ord(ntpMillisecond)) and
    (((LPrecision = ntpMinute) or (LPrecision = ntpSecond)) and (Result.Millisecond <> 0) or
    ((LPrecision = ntpTenth) and (Result.Millisecond mod 100 <> 0)) or
    ((LPrecision = ntpHundredth) and (Result.Millisecond mod 10 <> 0))) then
  begin
    LPrecision := ntpMillisecond;
  end;
  Result := Result.WithPrecision(LPrecision);
end;

procedure TNyxLCLTimeField.Publish(const AValue: TNyxClockTime);
var
  LValue: TNyxText;
  LReturn: TNyxLCLFocusReturn;
begin

  if FDisconnected or not IsEnabled or not IsVisible or ReadOnly then
  begin
    ClosePopup(False);
    Exit;
  end;
  LValue := AValue.ToText;
  try
    FDomain.ReadWire(LValue);
  except
    on LException: ENyxContract do
    begin
      FStatus.Caption := LException.Message;
      Exit;
    end;
  end;
  { Native Hide can itself re-enter the editor. Commit first so focus callbacks
    observe the admitted store; the independent weak lease guards later access
    if a text/change/commit callback retires the field. Disconnect closes that
    retired field's popup. Deliberate application focus redirection wins. }
  LReturn := TNyxLCLFocusReturn.CreateFor(Self, Editor);
  try
    LReturn.Capture;

    if not LReturn.ContextAlive or FDisconnected then
    begin
      Exit;
    end;
    try
      Text := LValue;

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

procedure TNyxLCLTimeField.AcceptClick(ASender: TObject);
var
  LValue: TNyxClockTime;
begin

  if FDisconnected or (FPopup = nil) or not FPopup.Visible then
  begin
    Exit;
  end;
  try
    LValue := PickerValue;
  except
    on LException: ENyxTimeValue do
    begin
      FStatus.Caption := LException.Message;
      Exit;
    end;
  end;
  { Keep application callbacks outside the picker-parse exception handler;
    retirement or a callback failure must not touch this field afterward. }
  Publish(LValue);
end;

procedure TNyxLCLTimeField.ClearClick(ASender: TObject);
begin

  if FDisconnected or (FPopup = nil) or not FPopup.Visible then
  begin
    Exit;
  end;
  Publish(NyxNoTime);
end;

procedure TNyxLCLTimeField.CancelClick(ASender: TObject);
begin
  ClosePopup(True);
end;

procedure TNyxLCLTimeField.PopupKeyDown(ASender: TObject; var AKey: Word;
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

procedure TNyxLCLTimeField.ClosePopup(AReturnFocus: Boolean);
begin

  if FClosing or (FPopup = nil) or not FPopup.Visible then
  begin
    Exit;
  end;
  FClosing := True;
  try
    FPopup.Hide;
  finally
    FClosing := False;
  end;

  if AReturnFocus and not FDisconnected and Editor.CanSetFocus then
  begin
    Editor.SetFocus;
  end;
end;

procedure TNyxLCLTimeField.PopupDeactivate(ASender: TObject);
begin
  ClosePopup(False);
end;

procedure TNyxLCLTimeField.PopupClose(ASender: TObject; var AAction: TCloseAction);
begin
  AAction := caNone;
  ClosePopup(True);
end;

procedure TNyxLCLTimeField.Disconnect;
begin
  FDisconnected := True;
  OnChange := nil;
  OnEditingDone := nil;
  ClosePopup(False);
end;

destructor TNyxLCLTimeField.Destroy;
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
