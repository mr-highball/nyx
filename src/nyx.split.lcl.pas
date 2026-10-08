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
unit nyx.split.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Math, Types, Controls, ExtCtrls, Graphics, Forms,
  nyx.text, nyx.types, nyx.model, nyx.split;

type
  TNyxLCLSplitView = class;

  { LCL provides capture, focus, accessibility and widget ownership. The small
    custom grip adds keyboard resizing to the separator behavior missing from
    its nonfocusable TSplitter. All sizing/cancellation uses the shared state. }
  TNyxLCLSplitGrip = class(TCustomControl)
  private
    FSplit: TNyxLCLSplitView;
    function Coordinate(AX, AY: Integer): Double;
  protected
    procedure Paint; override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;
    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;
    procedure CaptureChanged; override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { A native split host reuses owned LCL panels/scroll boxes. Child descriptors
    remain renderer-owned; the widget borrows its descriptor and never frees it.
    OnLayout arranges existing child controls. OnChanged is only a completed
    pointer/keyboard command and may navigate/dispose this entire widget. }
  TNyxLCLSplitView = class(TPanel)
  private
    FNode: TNyxNode;
    FState: TNyxSplitState;
    FGrip: TNyxLCLSplitGrip;
    FPanes: array[0..1] of TScrollBox;
    FOnLayout: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FReady: Boolean;
    FArranging: Boolean;
    { Focus remains available for inspection when resizing is read-only. This
      separate gate covers inherited policy and cancels an in-flight gesture. }
    FResizeAllowed: Boolean;
    procedure Publish;
    procedure Notify;
    function GetPane(AIndex: Integer): TScrollBox;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(ANode: TNyxNode);
    { Apply inherited interaction gates without recreating panes. Losing resize
      permission cancels an active gesture and restores its starting position. }
    procedure SetInteraction(AEnabled, AReadOnly: Boolean);
    procedure Arrange;
    procedure Ready;
    property Panes[AIndex: Integer]: TScrollBox read GetPane;
    property State: TNyxSplitState read FState;
    property Grip: TNyxLCLSplitGrip read FGrip;
    property OnLayout: TNotifyEvent read FOnLayout write FOnLayout;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

constructor TNyxLCLSplitGrip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSplit := TNyxLCLSplitView(AOwner);
  TabStop := True;
  AccessibleName := 'Resize panes';
  AccessibleDescription := 'Arrow keys resize; Shift adjusts by ten percent; Home and End use the limits.';
end;

function TNyxLCLSplitGrip.Coordinate(AX, AY: Integer): Double;
var
  LPoint: TPoint;
begin
  LPoint := ClientToScreen(Point(AX, AY));
  Result := LPoint.Y;

  if FSplit.State.Orientation = nsoSideBySide then
  begin
    Result := LPoint.X;
  end;
end;

procedure TNyxLCLSplitGrip.Paint;
var
  LIndex: Integer;
  LX: Integer;
  LY: Integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Color := clGray;

  if Focused then
  begin
    Canvas.Pen.Color := clHighlight;
    Canvas.Rectangle(1, 1, Width - 1, Height - 1);
  end;
  LX := Width div 2;
  LY := Height div 2;
  for LIndex := -1 to 1 do
  begin

    if FSplit.State.Orientation = nsoStacked then
    begin
      Canvas.MoveTo(LX - 16, LY + LIndex * 4);
      Canvas.LineTo(LX + 16, LY + LIndex * 4);
    end
    else
    begin
      Canvas.MoveTo(LX + LIndex * 4, LY - 16);
      Canvas.LineTo(LX + LIndex * 4, LY + 16);
    end;
  end;
end;

procedure TNyxLCLSplitGrip.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LExtent: Integer;
begin

  if (AButton <> mbLeft) or not IsEnabled or not FSplit.FResizeAllowed then
  begin
    Exit;
  end;
  LExtent := FSplit.ClientHeight;

  if FSplit.State.Orientation = nsoSideBySide then
  begin
    LExtent := FSplit.ClientWidth;
  end;
  FSplit.State.BeginDrag(Coordinate(AX, AY), LExtent - 44);
  MouseCapture := FSplit.State.Dragging;
end;

procedure TNyxLCLSplitGrip.MouseMove(AShift: TShiftState; AX, AY: Integer);
begin

  if FSplit.State.Drag(Coordinate(AX, AY)) then
  begin
    FSplit.Publish;
  end;
end;

procedure TNyxLCLSplitGrip.MouseUp(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin

  if (AButton = mbLeft) and FSplit.State.Dragging then
  begin
    FSplit.State.Drag(Coordinate(AX, AY));
    FSplit.State.EndDrag(False);
    MouseCapture := False;
    FSplit.Publish;
    FSplit.Notify;
  end;
end;

procedure TNyxLCLSplitGrip.CaptureChanged;
begin
  inherited CaptureChanged;

  if not MouseCapture and FSplit.State.Dragging then
  begin
    FSplit.State.EndDrag(True);
    FSplit.Publish;
  end;
end;

procedure TNyxLCLSplitGrip.KeyDown(var AKey: Word; AShift: TShiftState);
var
  LKey: TNyxKey;
begin
  { Notify the installed Nyx/creator key slot before the separator's default.
    Consumption or navigation seals AKey to zero; after that the borrowed split
    may have been disposed, so return without reading it. }
  inherited KeyDown(AKey, AShift);

  if AKey = 0 then
  begin
    Exit;
  end;

  if not IsEnabled or not FSplit.FResizeAllowed then
  begin
    Exit;
  end;
  LKey := NyxKeyFromVirtualCode(AKey);

  if (LKey = nkEscapeKey) and FSplit.State.Dragging then
  begin
    FSplit.State.EndDrag(True);
    MouseCapture := False;
    FSplit.Publish;
    AKey := 0;
    Exit;
  end;

  if not (LKey in [nkHomeKey, nkEndKey]) and
    not ((FSplit.State.Orientation = nsoStacked) and (LKey in [nkUpKey, nkDownKey])) and
    not ((FSplit.State.Orientation = nsoSideBySide) and (LKey in [nkLeftKey, nkRightKey])) then
  begin
    Exit;
  end;
  AKey := 0;

  if FSplit.State.Key(LKey, ssShift in AShift) then
  begin
    FSplit.Publish;
    FSplit.Notify;
  end;
end;

constructor TNyxLCLSplitView.Create(AOwner: TComponent);
var
  LIndex: Integer;
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  for LIndex := 0 to 1 do
  begin
    FPanes[LIndex] := TScrollBox.Create(Self);
    FPanes[LIndex].Parent := Self;
    FPanes[LIndex].BorderStyle := bsNone;
  end;
  FGrip := TNyxLCLSplitGrip.Create(Self);
  FGrip.Parent := Self;
end;

destructor TNyxLCLSplitView.Destroy;
begin
  FReady := False;
  FOnLayout := nil;
  FOnChanged := nil;
  { Controls release capture while their sizing state is still alive. }
  FGrip.Free;
  FGrip := nil;
  FState.Free;
  inherited Destroy;
end;

procedure TNyxLCLSplitView.SetInteraction(AEnabled, AReadOnly: Boolean);
var
  LAllowed: Boolean;
begin
  LAllowed := AEnabled and not AReadOnly and FState.Resizable;
  FResizeAllowed := LAllowed;

  if not LAllowed and FState.Dragging then
  begin
    FState.EndDrag(True);
    FGrip.MouseCapture := False;
    Publish;
  end;
  { Read-only and fixed separators still expose their values and key hooks.
    Disabled ancestors remove the entry without permitting a resize command. }
  FGrip.Enabled := AEnabled;
  FGrip.TabStop := AEnabled;
end;

procedure TNyxLCLSplitView.Initialize(ANode: TNyxNode);
begin
  FNode := ANode;
  FState := TNyxSplitState.Create(ANode);
  SetInteraction(ANode.Prop('enabled', 'true') <> 'false',
    ANode.Prop('readonly') = 'true');

  if FState.Orientation = nsoStacked then
  begin
    FGrip.Cursor := crVSplit;
  end
  else
  begin
    FGrip.Cursor := crHSplit;
  end;
end;

function TNyxLCLSplitView.GetPane(AIndex: Integer): TScrollBox;
begin

  if (AIndex < 0) or (AIndex > 1) then
  begin
    raise ERangeError.Create('Split pane index must be zero or one');
  end;
  Result := FPanes[AIndex];
end;

procedure TNyxLCLSplitView.Arrange;
var
  LGeometry: TNyxSplitGeometry;
begin

  if not FReady or FArranging then
  begin
    Exit;
  end;
  FArranging := True;
  try
    FState.ConfigurePanes(FNode);

    if FState.Orientation = nsoStacked then
    begin
      LGeometry := FState.Geometry(ClientHeight, 44);
      FPanes[0].SetBounds(0, 0, ClientWidth, LGeometry.FirstExtent);
      FGrip.SetBounds(0, LGeometry.FirstExtent, ClientWidth, LGeometry.DividerExtent);
      FPanes[1].SetBounds(0, LGeometry.FirstExtent + LGeometry.DividerExtent,
        ClientWidth, LGeometry.SecondExtent);
    end
    else
    begin
      LGeometry := FState.Geometry(ClientWidth, 44);
      FPanes[0].SetBounds(0, 0, LGeometry.FirstExtent, ClientHeight);
      FGrip.SetBounds(LGeometry.FirstExtent, 0, LGeometry.DividerExtent, ClientHeight);
      FPanes[1].SetBounds(LGeometry.FirstExtent + LGeometry.DividerExtent, 0,
        LGeometry.SecondExtent, ClientHeight);
    end;
    { Passive host resizing can constrain the visible divider without changing
      its requested preference. Native accessibility reports the physical value,
      just as the browser separator does. }
    FGrip.AccessibleValue := IntToStr(FState.EffectivePosition);

    if Assigned(FOnLayout) then
    begin
      FOnLayout(Self);
    end;
  finally
    FArranging := False;
  end;
end;

procedure TNyxLCLSplitView.Ready;
begin
  FReady := True;
  Arrange;
end;

procedure TNyxLCLSplitView.Resize;
begin
  inherited Resize;
  Arrange;
end;

procedure TNyxLCLSplitView.Publish;
begin
  FNode.Configure.SplitPosition(FState.Position);
  Arrange;
end;

procedure TNyxLCLSplitView.Notify;
begin

  if Assigned(FOnChanged) then
  begin
    { Never read a borrowed widget/node after this callback. }
    FOnChanged(Self);
  end;
end;

end.
