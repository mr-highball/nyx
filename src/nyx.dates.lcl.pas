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

unit nyx.dates.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, StdCtrls, EditBtn, Calendar,
  LCLType, nyx.text, nyx.dates, nyx.data, nyx.contract;

type
  { LCL calendar field. The real editor remains the renderer's input/focus surface.
    This field owns its calendar window; popup events borrow only this field.
    Disconnect precedes renderer retirement, and destruction frees that exact
    popup without touching another date field or an application window. }
  TNyxLCLDateField = class(TDateEdit)
  private
    FPopup: TForm;
    FCalendar: TCalendar;
    FDomain: TNyxValueDomain;
    FAcceptedDate: TNyxCalendarDate;
    FClosing: Boolean;
    function GetEditor: TCustomEdit;
    procedure CalendarKeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure CalendarDoubleClick(ASender: TObject);
    procedure CalendarDeactivate(ASender: TObject);
    procedure CalendarClose(ASender: TObject; var AAction: TCloseAction);
    procedure AcceptCalendar;
    procedure CloseCalendar(AReturnFocus: Boolean);
  protected
    procedure ButtonClick; override;
    { A partial date is a retained editing draft, not an accepted scalar. }
    procedure EditChange; override;
    { Bypass TDateEdit's permissive locale/default normalization. The ordinary
      Nyx commit callback validates/restores the exact physical draft. }
    procedure EditEditingDone; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Copy an admitted date domain and project its inclusive bounds. Passing a
      non-calendar explicit domain uses the intrinsic calendar picker bounds;
      shared value admission still belongs to the renderer's declared domain. }
    procedure SetDomain(const AValue: TNyxValueDomain);
    { Copy the admitted runtime value separately from an unfinished editor draft. }
    procedure SetAcceptedValue(const AValue: TNyxCalendarDate);
    { An ancestor read-only/disabled/hidden policy also closes an open popup. }
    procedure SetInteraction(AEnabled, AReadOnly, AVisible: Boolean);
    { Revoke borrowed renderer callbacks before native controls retire. }
    procedure Disconnect;
    property Editor: TCustomEdit read GetEditor;
    { Borrowed exact owned popup/calendar for target hosts and input qualification.
      Neither handle may outlive this field. Nil means not created yet. }
    property Popup: TForm read FPopup;
    property CalendarControl: TCalendar read FCalendar;
  end;

implementation

constructor TNyxLCLDateField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomain := NyxDateDomain.Definition;
  DefaultToday := False;
  DateOrder := doNone;
  DateFormat := 'yyyy-mm-dd';
  ButtonHint := 'Choose a date';
  Button.ShowHint := True;
  FocusOnButtonClick := False;
end;

function TNyxLCLDateField.GetEditor: TCustomEdit;
begin
  Result := BaseEditor;
end;

procedure TNyxLCLDateField.EditChange;
var
  LDate: TNyxCalendarDate;
begin

  if not TryNyxDate(TNyxText(Text), LDate) then
  begin
    Exit;
  end;
  inherited EditChange;
end;

procedure TNyxLCLDateField.EditEditingDone;
begin

  if Assigned(OnEditingDone) then
  begin
    OnEditingDone(Self);
  end;
end;

procedure TNyxLCLDateField.SetDomain(const AValue: TNyxValueDomain);
var
  LCandidate: TNyxValueDomain;
begin
  AValue.Validate;

  if not AValue.CalendarDate then
  begin
    LCandidate := NyxDateDomain.Definition;
  end
  else
  begin
    LCandidate := AValue.Copy;
  end;

  if FDomain.ToData.ToJSON = LCandidate.ToData.ToJSON then
  begin
    Exit;
  end;
  FDomain := LCandidate;
  { Bounds are applied on each opening. Changing the declared domain while the
    picker is open ends that old choice context without mutating a value. }

  if (FPopup <> nil) and FPopup.Visible then
  begin
    CloseCalendar(False);
  end;
end;

procedure TNyxLCLDateField.SetAcceptedValue(const AValue: TNyxCalendarDate);
begin
  FAcceptedDate := AValue;
end;

procedure TNyxLCLDateField.SetInteraction(AEnabled, AReadOnly, AVisible: Boolean);
begin
  Enabled := AEnabled;
  ReadOnly := AReadOnly;
  Button.Enabled := AEnabled and not AReadOnly;

  if not AEnabled or AReadOnly or not AVisible then
  begin
    CloseCalendar(False);
  end;
end;

procedure TNyxLCLDateField.ButtonClick;
var
  LDate: TNyxCalendarDate;
  LMinimum: TNyxCalendarDate;
  LMaximum: TNyxCalendarDate;
  LData: TNyxDataValue;
  LField: Integer;
  LPosition: TPoint;
  LWork: TRect;
  LCurrent: TDateTime;
begin

  if not IsEnabled or not IsVisible or ReadOnly then
  begin
    Exit;
  end;

  if FPopup = nil then
  begin
    FPopup := TForm.CreateNew(Self);
    FPopup.Caption := 'Choose a date';
    FPopup.BorderStyle := bsToolWindow;
    FPopup.ShowInTaskBar := stNever;
    FPopup.Position := poDesigned;
    { Let the installed calendar report its actual widgetset/DPI size. Fixed
      dimensions leave unused columns beside a standard Win32 month calendar. }
    FPopup.AutoSize := True;
    FPopup.OnDeactivate := CalendarDeactivate;
    FPopup.OnClose := CalendarClose;
    FCalendar := TCalendar.Create(FPopup);
    FCalendar.Parent := FPopup;
    FCalendar.AutoSize := True;
    FCalendar.OnKeyDown := CalendarKeyDown;
    FCalendar.OnDblClick := CalendarDoubleClick;
    FCalendar.AccessibleName := 'Choose a date';
  end;
  LMinimum := NyxDate(1, 1, 1);
  LMaximum := NyxDate(9999, 12, 31);
  LData := FDomain.ToData;
  for LField := 0 to LData.Count - 1 do
  begin

    if LData.Key(LField) = 'min' then
    begin
      LMinimum := TNyxCalendarDate.FromText(LData.Field('min').AsText);
      LMaximum := TNyxCalendarDate.FromText(LData.Field('max').AsText);
    end;
  end;
  { Reset the old host interval first, then apply both admitted bounds. }

  FCalendar.MinDate := 0;
  FCalendar.MaxDate := 0;
  FCalendar.MinDate := EncodeDate(LMinimum.Year, LMinimum.Month, LMinimum.Day);
  FCalendar.MaxDate := EncodeDate(LMaximum.Year, LMaximum.Month, LMaximum.Day);

  if not TryNyxDate(TNyxText(Text), LDate) then
  begin
    LDate := FAcceptedDate;
  end;
  LCurrent := SysUtils.Date;

  if LDate.Defined then
  begin
    LCurrent := EncodeDate(LDate.Year, LDate.Month, LDate.Day);
  end;

  if LCurrent < FCalendar.MinDate then
  begin
    LCurrent := FCalendar.MinDate;
  end
  else if LCurrent > FCalendar.MaxDate then
  begin
    LCurrent := FCalendar.MaxDate;
  end;
  FCalendar.DateTime := LCurrent;
  LPosition := ClientToScreen(Point(0, Height));
  LWork := Screen.MonitorFromPoint(LPosition).WorkareaRect;
  FPopup.Left := LPosition.X;
  FPopup.Top := LPosition.Y;
  FPopup.Show;

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
  FCalendar.SetFocus;
end;

procedure TNyxLCLDateField.CalendarKeyDown(ASender: TObject; var AKey: Word;
  AShift: TShiftState);
begin

  if AShift <> [] then
  begin
    Exit;
  end;

  if AKey = VK_ESCAPE then
  begin
    AKey := 0;
    CloseCalendar(True);
  end
  else if (AKey in [VK_RETURN, VK_SPACE]) and
    (FCalendar.GetCalendarView = cvMonth) then
  begin
    AKey := 0;
    AcceptCalendar;
  end;
end;

procedure TNyxLCLDateField.CalendarDoubleClick(ASender: TObject);
begin

  if (FCalendar.GetCalendarView = cvMonth) and
    (FCalendar.HitTest(FCalendar.ScreenToClient(Mouse.CursorPos)) = cpDate) then
  begin
    AcceptCalendar;
  end;
end;

procedure TNyxLCLDateField.AcceptCalendar;
var
  LYear: Word;
  LMonth: Word;
  LDay: Word;
  LValue: TNyxText;
begin

  if not IsEnabled or not IsVisible or ReadOnly then
  begin
    CloseCalendar(False);
    Exit;
  end;
  DecodeDate(FCalendar.DateTime, LYear, LMonth, LDay);
  LValue := NyxDate(LYear, LMonth, LDay).ToText;
  { Shared admission reports a refused range/choice through ordinary diagnostics.
    Hide before notifying application code; it may retire this entire field. }
  CloseCalendar(False);
  Text := LValue;
end;

procedure TNyxLCLDateField.CloseCalendar(AReturnFocus: Boolean);
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

  if AReturnFocus and not (csDestroying in ComponentState) and Editor.CanFocus then
  begin
    { Focus callbacks may retire the field. Nothing touches Self afterward. }
    Editor.SetFocus;
  end;
end;

procedure TNyxLCLDateField.CalendarDeactivate(ASender: TObject);
begin
  CloseCalendar(False);
end;

procedure TNyxLCLDateField.CalendarClose(ASender: TObject; var AAction: TCloseAction);
begin
  AAction := caNone;
  CloseCalendar(True);
end;

procedure TNyxLCLDateField.Disconnect;
begin
  OnChange := nil;
  OnEditingDone := nil;
  CloseCalendar(False);
end;

destructor TNyxLCLDateField.Destroy;
begin
  Disconnect;

  if FPopup <> nil then
  begin
    FPopup.OnDeactivate := nil;
    FPopup.OnClose := nil;
  end;
  FreeAndNil(FPopup);
  FCalendar := nil;
  inherited Destroy;
end;

end.
