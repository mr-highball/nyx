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
unit nyx.viewport.surface.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, Forms, Controls, StdCtrls, Types, nyx.viewport;

type
  TNyxLogicalScrollBox = class;

  { Owned axis port, borrowing its containing control. Ordinary mode delegates
    to automatic LCL bars; logical mode uses a standard TScrollBar with a full
    Integer range. The port never outlives its containing control. }
  TNyxViewportBar = class
  private
    FOwner: TNyxLogicalScrollBox;
    FKind: TScrollBarKind;
    FControl: TScrollBar;
    FRange: Integer;
    function NativeBar: TControlScrollBar;
    function GetPosition: Integer;
    function GetRange: Integer;
    function GetPage: Integer;
    procedure SetPosition(AValue: Integer);
    procedure Changed(ASender: TObject);
    procedure Arrange;
  public
    constructor Create(AOwner: TNyxLogicalScrollBox; AKind: TScrollBarKind);
    destructor Destroy; override;
    property Position: Integer read GetPosition write SetPosition;
    property Range: Integer read GetRange;
    property Page: Integer read GetPage;
  end;

  { Keep every descendant while projecting geometry into a physical client
    frame. In logical mode inherited bars remain at zero: some LCL widgetsets
    directly subtract those fields during native window positioning/recreation,
    independently of virtual ScrollBy/GetClientScrollOffset overrides.
    Separate standard LCL scrollbar controls avoid this second translation.
    Ordinary views retain their TScrollBox behavior and native child parenting.
    The projection receiver is borrowed; detach before renderer disposal. }
  TNyxLogicalScrollBox = class(TScrollBox)
  private
    FLogical: Boolean;
    FConfiguring: Boolean;
    FOnLogicalScroll: TNotifyEvent;
    FHorizontal: TNyxViewportBar;
    FVertical: TNyxViewportBar;
    FWheelY: Integer;
    FWheelX: Integer;
    function GetViewportWidth: Integer;
    function GetViewportHeight: Integer;
    procedure NotifyProjection;
  protected
    function GetClientScrollOffset: TPoint; override;
    function GetLogicalClientRect: TRect; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollBy(ADeltaX, ADeltaY: Integer); override;
    { Complete logical extent, never a truncated document/child. Both axis
      visibility/page sizes settle before subsequent renderer projection. }
    procedure SetLogicalExtent(AWidth, AHeight: Integer; AReceiver: TNotifyEvent);
    procedure UseNativeScrolling;
    procedure Detach;
    function Snapshot: TNyxViewportSnapshot;
    property Logical: Boolean read FLogical;
    property ViewportWidth: Integer read GetViewportWidth;
    property ViewportHeight: Integer read GetViewportHeight;
    { Ports deliberately hide inherited implementation bars. Public snapshots
      and these ports report logical offsets; base bars are physical plumbing. }
    property HorzScrollBar: TNyxViewportBar read FHorizontal;
    property VertScrollBar: TNyxViewportBar read FVertical;
  end;

implementation

uses
  Math, SysUtils, LCLIntf, LCLType;

constructor TNyxViewportBar.Create(AOwner: TNyxLogicalScrollBox; AKind: TScrollBarKind);
begin
  inherited Create;
  FOwner := AOwner;
  FKind := AKind;
  FControl := TScrollBar.Create(AOwner);
  FControl.Parent := AOwner;
  FControl.Kind := AKind;
  FControl.Visible := False;
  FControl.TabStop := False;
  FControl.SmallChange := 8;
  FControl.OnChange := Changed;
end;

destructor TNyxViewportBar.Destroy;
begin

  if FControl <> nil then
  begin
    FControl.OnChange := nil;
  end;
  { The LCL owner releases actual controls after these borrowed ports retire. }
  inherited Destroy;
end;

function TNyxViewportBar.NativeBar: TControlScrollBar;
begin

  if FKind = sbHorizontal then
  begin
    Result := TScrollBox(FOwner).HorzScrollBar;
  end
  else
  begin
    Result := TScrollBox(FOwner).VertScrollBar;
  end;
end;

function TNyxViewportBar.GetPosition: Integer;
begin

  if FOwner.Logical then
  begin
    Result := FControl.Position;
  end
  else
  begin
    Result := NativeBar.Position;
  end;
end;

function TNyxViewportBar.GetRange: Integer;
begin

  if FOwner.Logical then
  begin
    Result := FRange;
  end
  else
  begin
    Result := NativeBar.Range;
  end;
end;

function TNyxViewportBar.GetPage: Integer;
begin

  if not FOwner.Logical then
  begin
    Result := NativeBar.Page;
  end
  else if FKind = sbHorizontal then
  begin
    Result := FOwner.ViewportWidth;
  end
  else
  begin
    Result := FOwner.ViewportHeight;
  end;
end;

procedure TNyxViewportBar.SetPosition(AValue: Integer);
begin

  if FOwner.Logical then
  begin
    FControl.Position := Max(0, Min(AValue, Max(0, FRange - Page)));
  end
  else
  begin
    NativeBar.Position := AValue;
  end;
end;

procedure TNyxViewportBar.Changed(ASender: TObject);
begin

  if FOwner.FConfiguring then
  begin
    Exit;
  end;
  { Bound a keyboard/OS proposal to the exact logical range/page before
    publishing. LCL's native scrollbar Max is inclusive. }
  FOwner.FConfiguring := True;
  try
    Position := FControl.Position;
  finally
    FOwner.FConfiguring := False;
  end;
  FOwner.NotifyProjection;
end;

procedure TNyxViewportBar.Arrange;
begin
  FControl.SetParams(Max(0, Min(FControl.Position, Max(0, FRange - Page))),
    0, Max(0, FRange - 1), Min(FRange, Page));
  FControl.LargeChange := Max(1, Min(Page, High(TScrollBarInc)));

  if FKind = sbHorizontal then
  begin
    FControl.SetBounds(0, FOwner.ViewportHeight, FOwner.ViewportWidth,
      Max(1, GetSystemMetrics(SM_CYHSCROLL)));
  end
  else
  begin
    FControl.SetBounds(FOwner.ViewportWidth, 0,
      Max(1, GetSystemMetrics(SM_CXVSCROLL)), FOwner.ViewportHeight);
  end;
  FControl.BringToFront;
end;

constructor TNyxLogicalScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorizontal := TNyxViewportBar.Create(Self, sbHorizontal);
  FVertical := TNyxViewportBar.Create(Self, sbVertical);
end;

destructor TNyxLogicalScrollBox.Destroy;
begin
  Detach;
  FreeAndNil(FHorizontal);
  FreeAndNil(FVertical);
  inherited Destroy;
end;

function TNyxLogicalScrollBox.GetViewportWidth: Integer;
begin
  Result := Max(0, ClientWidth);

  if FLogical and FVertical.FControl.Visible then
  begin
    Result := Max(0, Result - Max(1, GetSystemMetrics(SM_CXVSCROLL)));
  end;
end;

function TNyxLogicalScrollBox.GetViewportHeight: Integer;
begin
  Result := Max(0, ClientHeight);

  if FLogical and FHorizontal.FControl.Visible then
  begin
    Result := Max(0, Result - Max(1, GetSystemMetrics(SM_CYHSCROLL)));
  end;
end;

function TNyxLogicalScrollBox.GetClientScrollOffset: TPoint;
begin

  if FLogical then
  begin
    Result := Point(0, 0);
  end
  else
  begin
    Result := inherited GetClientScrollOffset;
  end;
end;

function TNyxLogicalScrollBox.GetLogicalClientRect: TRect;
begin

  if FLogical then
  begin
    Result := Rect(0, 0, ViewportWidth, ViewportHeight);
  end
  else
  begin
    Result := inherited GetLogicalClientRect;
  end;
end;

procedure TNyxLogicalScrollBox.NotifyProjection;
begin

  if not FConfiguring and Assigned(FOnLogicalScroll) then
  begin
    FOnLogicalScroll(Self);
  end;
end;

procedure TNyxLogicalScrollBox.ScrollBy(ADeltaX, ADeltaY: Integer);
begin

  if not FLogical then
  begin
    inherited ScrollBy(ADeltaX, ADeltaY);
  end
  else
  begin
    NotifyProjection;
  end;
end;

procedure TNyxLogicalScrollBox.SetLogicalExtent(AWidth, AHeight: Integer;
  AReceiver: TNotifyEvent);
var
  LPass: Integer;
begin

  if (AWidth < 0) or (AHeight < 0) or not Assigned(AReceiver) then
  begin
    raise EArgumentException.Create('Logical scroll extent requires dimensions and its renderer');
  end;
  FConfiguring := True;
  try
    FLogical := True;
    FOnLogicalScroll := AReceiver;
    AutoScroll := False;
    TScrollBox(Self).HorzScrollBar.Position := 0;
    TScrollBox(Self).VertScrollBar.Position := 0;
    TScrollBox(Self).HorzScrollBar.Visible := False;
    TScrollBox(Self).VertScrollBar.Visible := False;
    TScrollBox(Self).HorzScrollBar.Range := 0;
    TScrollBox(Self).VertScrollBar.Range := 0;
    FHorizontal.FRange := AWidth;
    FVertical.FRange := AHeight;
    FHorizontal.FControl.Visible := False;
    FVertical.FControl.Visible := False;
    for LPass := 0 to 1 do
    begin
      FHorizontal.FControl.Visible := AWidth > ViewportWidth;
      FVertical.FControl.Visible := AHeight > ViewportHeight;
    end;
    FHorizontal.Arrange;
    FVertical.Arrange;
  finally
    FConfiguring := False;
  end;
end;

procedure TNyxLogicalScrollBox.UseNativeScrolling;
begin
  FOnLogicalScroll := nil;
  FHorizontal.FControl.Visible := False;
  FVertical.FControl.Visible := False;
  FLogical := False;
  TScrollBox(Self).HorzScrollBar.Visible := True;
  TScrollBox(Self).VertScrollBar.Visible := True;
  AutoScroll := True;
end;

function TNyxLogicalScrollBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  LBefore: Integer;
  LTotal: Double;
  LSteps: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);

  if not Result and FLogical then
  begin
    LBefore := FVertical.Position;
    { Widen before combining signed ticks/offsets. One detent moves 24 logical
      pixels; partial detents accumulate without wrapping a distant offset. }
    LTotal := WheelDelta;
    LTotal := LTotal + FWheelY;
    LSteps := Trunc(LTotal / 120);
    FWheelY := Trunc(LTotal - LSteps * 120.0);
    FVertical.Position := Trunc(Max(0.0, Min(Double(Max(0,
      FVertical.Range - FVertical.Page)), Double(LBefore) - LSteps * 24.0)));
    Result := (FVertical.Position <> LBefore) or (FWheelY <> 0);
  end;
end;

function TNyxLogicalScrollBox.DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  LBefore: Integer;
  LTotal: Double;
  LSteps: Integer;
begin
  Result := inherited DoMouseWheelHorz(Shift, WheelDelta, MousePos);

  if not Result and FLogical then
  begin
    LBefore := FHorizontal.Position;
    LTotal := WheelDelta;
    LTotal := LTotal + FWheelX;
    LSteps := Trunc(LTotal / 120);
    FWheelX := Trunc(LTotal - LSteps * 120.0);
    FHorizontal.Position := Trunc(Max(0.0, Min(Double(Max(0,
      FHorizontal.Range - FHorizontal.Page)), Double(LBefore) + LSteps * 24.0)));
    Result := (FHorizontal.Position <> LBefore) or (FWheelX <> 0);
  end;
end;

function TNyxLogicalScrollBox.Snapshot: TNyxViewportSnapshot;
begin
  Result := NyxViewport(
    NyxViewportAxis(FHorizontal.Position, FHorizontal.Range, ViewportWidth, nvuLogicalPixels),
    NyxViewportAxis(FVertical.Position, FVertical.Range, ViewportHeight, nvuLogicalPixels),
    ViewportWidth, ViewportHeight);
end;

procedure TNyxLogicalScrollBox.Detach;
begin
  FOnLogicalScroll := nil;
end;

end.
