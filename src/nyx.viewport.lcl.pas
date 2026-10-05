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
unit nyx.viewport.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses Classes, Controls, nyx.text, nyx.events, nyx.viewport;

type
  TNyxViewportHandler = procedure(const AOriginID: TNyxText;
    const AViewport: TNyxViewportSnapshot) of object;
  { One idle observer per admitted view. It borrows controls only while connected
    and owns only its event-router lease and value baselines. A coalesced native
    observation is not a per-pixel OS notification or gesture-end approximation.
    No timer, background polling or widget-specific WindowProc is installed. }
  INyxViewportObserver = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000024}']
    procedure Add(const AOriginID: TNyxText; AControl: TWinControl);
    procedure Activate(const AEvents: INyxEvents; AHandler: TNyxViewportHandler);
    procedure Disconnect;
  end;

function CaptureNyxViewport(AControl: TWinControl): TNyxViewportSnapshot;
function NewNyxViewportObserver: INyxViewportObserver;

implementation

uses SysUtils, Forms, StdCtrls, Grids, LCLType, LCLIntf, nyx.types,
  nyx.viewport.surface.lcl;

type
  TNyxObservedViewport = record
    ID: TNyxText;
    Control: TWinControl;
    Baseline: TNyxViewportSnapshot;
  end;
  TNyxViewportObserver = class(TInterfacedObject, INyxViewportObserver)
  private
    FControls: array of TNyxObservedViewport;
    FEvents: INyxEvents;
    FHandler: TNyxViewportHandler;
    FRevision: Integer;
    FConnected: Boolean;
    procedure Idle(ASender: TObject; var ADone: Boolean);
  public
    destructor Destroy; override;
    procedure Add(const AOriginID: TNyxText; AControl: TWinControl);
    procedure Activate(const AEvents: INyxEvents; AHandler: TNyxViewportHandler);
    procedure Disconnect;
  end;

function ScrollAxis(AControl: TWinControl; ABar: Integer;
  AUnit: TNyxViewportUnit): TNyxViewportAxis;
var
  LInfo: TScrollInfo;
begin
  Result := Default(TNyxViewportAxis);
  LInfo := Default(TScrollInfo);
  LInfo.cbSize := SizeOf(LInfo);
  LInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;

  if AControl.HandleAllocated and GetScrollInfo(AControl.Handle, ABar, LInfo) then
  begin
    Result := NyxViewportAxis(LInfo.nPos,
      Double(LInfo.nMax) - Double(LInfo.nMin) + 1, LInfo.nPage, AUnit);
  end;
end;

function CaptureNyxViewport(AControl: TWinControl): TNyxViewportSnapshot;
var
  LX, LY: TNyxViewportAxis;
  LGrid: TStringGrid;
  LScroll: TScrollingWinControl;
begin

  if AControl = nil then
  begin
    raise EArgumentException.Create('Viewport capture requires a mounted control');
  end;

  if AControl is TNyxLogicalScrollBox then
  begin
    Exit(TNyxLogicalScrollBox(AControl).Snapshot);
  end;
  LX := Default(TNyxViewportAxis);
  LY := Default(TNyxViewportAxis);

  if AControl is TStringGrid then
  begin
    LGrid := TStringGrid(AControl);
    LX := NyxViewportAxis(LGrid.LeftCol, LGrid.ColCount, LGrid.VisibleColCount, nvuColumns);
    LY := NyxViewportAxis(LGrid.TopRow, LGrid.RowCount, LGrid.VisibleRowCount, nvuRows);
  end
  else if AControl is TScrollingWinControl then
  begin
    LScroll := TScrollingWinControl(AControl);
    LX := NyxViewportAxis(LScroll.HorzScrollBar.Position, LScroll.HorzScrollBar.Range,
      LScroll.ClientWidth, nvuLogicalPixels);
    LY := NyxViewportAxis(LScroll.VertScrollBar.Position, LScroll.VertScrollBar.Range,
      LScroll.ClientHeight, nvuLogicalPixels);
  end
  else
  begin
    LX := ScrollAxis(AControl, SB_HORZ, nvuNativeUnits);
    LY := ScrollAxis(AControl, SB_VERT, nvuNativeUnits);

    if AControl is TListBox then
    begin
      { LCL owns the exact item offset independently of scrollbar visibility. }
      LY := NyxViewportAxis(TListBox(AControl).TopIndex, TListBox(AControl).Items.Count,
        LY.PageSize, nvuItems);
    end;
  end;
  Result := NyxViewport(LX, LY, AControl.ClientWidth, AControl.ClientHeight);
end;

function NewNyxViewportObserver: INyxViewportObserver;
begin
  Result := TNyxViewportObserver.Create;
end;

destructor TNyxViewportObserver.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxViewportObserver.Add(const AOriginID: TNyxText; AControl: TWinControl);
var
  LIndex: Integer;
begin

  if FConnected then
  begin
    raise EArgumentException.Create('Register viewport controls before admission');
  end;
  LIndex := Length(FControls);
  SetLength(FControls, LIndex + 1);
  FControls[LIndex].ID := AOriginID;
  FControls[LIndex].Control := AControl;
end;

procedure TNyxViewportObserver.Activate(const AEvents: INyxEvents;
  AHandler: TNyxViewportHandler);
var
  LIndex: Integer;
begin

  if FConnected or (AEvents = nil) or not Assigned(AHandler) then
  begin
    raise EArgumentException.Create('Viewport observation requires its admitted UI owner');
  end;
  AEvents.Scheduler.RequireUI;
  for LIndex := 0 to High(FControls) do
  begin
    FControls[LIndex].Baseline := CaptureNyxViewport(FControls[LIndex].Control);
  end;
  FEvents := AEvents;
  FRevision := AEvents.ViewRevision;
  FHandler := AHandler;
  FConnected := True;
  Application.AddOnIdleHandler(Idle);
end;

procedure TNyxViewportObserver.Disconnect;
begin

  if FEvents <> nil then
  begin
    FEvents.Scheduler.RequireUI;
  end;

  if FConnected then
  begin
    Application.RemoveOnIdleHandler(Idle);
  end;
  FConnected := False;
  FHandler := nil;
  FEvents := nil;
  FControls := nil;
end;

procedure TNyxViewportObserver.Idle(ASender: TObject; var ADone: Boolean);
var
  LKeepAlive: INyxViewportObserver;
  LEvents: INyxEvents;
  LIndex: Integer;
  LViewport: TNyxViewportSnapshot;
  LChanged: Boolean;
begin
  LKeepAlive := Self;
  LEvents := FEvents;

  if not FConnected or (LEvents = nil) or (LEvents.ViewRevision <> FRevision) then
  begin
    Exit;
  end;
  { Preserve the application's idle decision. Do not request an idle spin when
    no input occurred, and do not scan controls without a scroll subscriber. }

  if not LEvents.HasSubscribers(ntScroll) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FControls) do
  begin
    LViewport := CaptureNyxViewport(FControls[LIndex].Control);
    LChanged := not LViewport.SamePosition(FControls[LIndex].Baseline);
    FControls[LIndex].Baseline := LViewport;

    if LChanged then
    begin
      FHandler(FControls[LIndex].ID, LViewport);
      { Navigation may have disconnected this scope and freed all borrowed
        controls. The managed keepalive protects only the observer, not them. }

      if not FConnected or (LEvents.ViewRevision <> FRevision) then
      begin
        Exit;
      end;
    end;
  end;
end;

end.
