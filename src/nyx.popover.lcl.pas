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

unit nyx.popover.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, Controls, Forms, nyx.text, nyx.types, nyx.root.types, nyx.model,
  nyx.theme, nyx.behavior, nyx.events, nyx.popover, nyx.render.lcl;

type
  TControlAccess = class(TControl);
  { Native window/renderer observations are borrowed. The presenter owns a
    standard nonmodal TForm, ordinary LCL controls and weak anchor notifications.
    Theme is borrowed. Anchor retirement is detected without a dangling pointer. }
  INyxLCLPopover = interface(INyxPopover)
    ['{B2672EF9-496D-4EC9-8006-061026000003}']
    function GetWindow: TForm;
    function GetRenderer: TNyxLCLRenderer;
    { Adapter-only family registration. Windows are borrowed; descendants remove
      themselves before destruction and retain their ancestor presenter. }
    procedure IncludeWindow(AWindow: TForm);
    procedure ExcludeWindow(AWindow: TForm);
    property Window: TForm read GetWindow;
    property Renderer: TNyxLCLRenderer read GetRenderer;
  end;

function NewNyxLCLPopover(AAnchor: TWinControl; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; ATheme: TNyxTheme = nil;
  const AParent: INyxLCLPopover = nil): INyxLCLPopover;

implementation

uses
  Math, Types, LCLType, LCLIntf, LMessages, ExtCtrls, nyx.interaction;

type
  { Weak target controls. Notifications only clear observations; the UI timer
    performs dismissal after component destruction has completed. }
  TPopoverTargets = class(TComponent)
  public
    Anchor: TWinControl;
    Invoker: TWinControl;
    constructor Create(AAnchor: TWinControl); reintroduce;
    destructor Destroy; override;
    procedure Capture;
    procedure ReturnFocus;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  end;

  TLCLPopover = class(TNyxPopoverPresenter, INyxPopover, INyxLCLPopover)
  private
    FParent: INyxLCLPopover;
    FFamilyWindows: array of TForm;
    FTargets: TPopoverTargets;
    FWindow: TForm;
    FRenderer: TNyxLCLRenderer;
    FTimer: TTimer;
    FOptions: TNyxPopoverOptions;
    procedure Reposition;
    function AnchorAvailable: Boolean;
    procedure Tick(ASender: TObject);
    function FamilyContains(const APoint: TPoint): Boolean;
    function FamilyContainsControl(AControl: TControl): Boolean;
    procedure UserInput(ASender: TObject; var AMessage: TLMessage);
    procedure KeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure WindowClose(ASender: TObject; var AAction: TCloseAction);
    procedure Deactivate(ASender: TObject);
    procedure Semantic(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  protected
    procedure Present(const AOptions: TNyxPopoverOptions;
      const AFocusID: TNyxText); override;
    procedure Conceal(ARestoreFocus: Boolean); override;
  public
    constructor Create(AAnchor: TWinControl; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef; ATheme: TNyxTheme; const AParent: INyxLCLPopover);
    destructor Destroy; override;
    function GetWindow: TForm;
    function GetRenderer: TNyxLCLRenderer;
    function GetEvents: INyxEvents; override;
    procedure IncludeWindow(AWindow: TForm);
    procedure ExcludeWindow(AWindow: TForm);
  end;

constructor TPopoverTargets.Create(AAnchor: TWinControl);
begin
  inherited Create(nil);
  Anchor := AAnchor;
  Anchor.FreeNotification(Self);
end;

destructor TPopoverTargets.Destroy;
begin

  if Anchor <> nil then
  begin
    Anchor.RemoveFreeNotification(Self);
  end;

  if (Invoker <> nil) and (Invoker <> Anchor) then
  begin
    Invoker.RemoveFreeNotification(Self);
  end;
  inherited Destroy;
end;

procedure TPopoverTargets.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);

  if AOperation = opRemove then
  begin

    if AComponent = Anchor then
    begin
      Anchor := nil;
    end;

    if AComponent = Invoker then
    begin
      Invoker := nil;
    end;
  end;
end;

procedure TPopoverTargets.Capture;
begin

  if (Invoker <> nil) and (Invoker <> Anchor) then
  begin
    Invoker.RemoveFreeNotification(Self);
  end;
  Invoker := Screen.ActiveControl;

  if Invoker <> nil then
  begin
    Invoker.FreeNotification(Self);
  end;
end;

procedure TPopoverTargets.ReturnFocus;
var
  LInvoker: TWinControl;
begin
  LInvoker := Invoker;
  Invoker := nil;

  if LInvoker <> nil then
  begin
    { Anchor can also be the invoker; keep its independent notification. }

    if LInvoker <> Anchor then
    begin
      LInvoker.RemoveFreeNotification(Self);
    end;

    if LInvoker.CanSetFocus then
    begin
      LInvoker.SetFocus;
    end;
  end;
end;

constructor TLCLPopover.Create(AAnchor: TWinControl; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; ATheme: TNyxTheme; const AParent: INyxLCLPopover);
begin
  inherited Create(ADocument, ARoot);

  if AAnchor = nil then
  begin
    raise ENyxModel.Create('Popover requires an anchor');
  end;
  FTargets := TPopoverTargets.Create(AAnchor);
  FWindow := TForm.CreateNew(nil);
  FParent := AParent;

  if FParent <> nil then
  begin
    FParent.IncludeWindow(FWindow);
  end;
  FWindow.BorderStyle := bsNone;
  FWindow.Position := poDesigned;
  FWindow.ShowInTaskBar := stNever;
  FWindow.OnClose := WindowClose;
  FWindow.OnDeactivate := Deactivate;
  FRenderer := TNyxLCLRenderer.Create(ATheme);
  FRenderer.OnEvent := Semantic;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := Tick;
end;

destructor TLCLPopover.Destroy;
begin
  Conceal(False);

  if FParent <> nil then
  begin
    FParent.ExcludeWindow(FWindow);
    FParent := nil;
  end;
  FTimer.Free;

  if FRenderer <> nil then
  begin
    FRenderer.OnEvent := nil;
    FRenderer.Free;
  end;
  FWindow.Free;
  FTargets.Free;
  inherited Destroy;
end;

procedure TLCLPopover.Semantic(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.SourceID = GetContent.ID) and AEvent.IsNamed(NyxSemantic(nseDismiss)) then
  begin
    Dismiss(nprAction);
  end;
end;

function TLCLPopover.AnchorAvailable: Boolean;
begin
  Result := (FTargets <> nil) and (FTargets.Anchor <> nil);

  if Result then
  begin
    Result := FTargets.Anchor.IsVisible and FTargets.Anchor.IsEnabled and
      (FTargets.Anchor.Width > 0) and (FTargets.Anchor.Height > 0);
  end;
end;

procedure TLCLPopover.Reposition;
var
  LOrigin: TPoint;
  LWork: TRect;
  LPlaced: TNyxPopoverRect;
  LPPI: Integer;
  LOptions: TNyxPopoverOptions;
begin
  LOrigin := FTargets.Anchor.ClientToScreen(Point(0, 0));
  LWork := Screen.MonitorFromPoint(LOrigin).WorkareaRect;
  LPPI := Max(1, FTargets.Anchor.Font.PixelsPerInch);
  LOptions := FOptions;

  if FWindow.Visible and (FOptions.SizeMode = npzContent) then
  begin
    LOptions := FOptions.Size(FOptions.Width, Min(FOptions.Height,
      Max(16, MulDiv(FRenderer.ControlFor(GetContent.ID).Height, 96, LPPI))));
  end;
  { Absolute screen coordinates and the monitor work area are converted together;
    negative monitor origins remain signed. The shared resolver sees logical px. }
  LPlaced := PlaceNyxPopover(NyxPopoverRect(MulDiv(LOrigin.X, 96, LPPI),
    MulDiv(LOrigin.Y, 96, LPPI), MulDiv(FTargets.Anchor.Width, 96, LPPI),
    MulDiv(FTargets.Anchor.Height, 96, LPPI)),
    NyxPopoverRect(MulDiv(LWork.Left, 96, LPPI), MulDiv(LWork.Top, 96, LPPI),
      MulDiv(LWork.Right - LWork.Left, 96, LPPI),
      MulDiv(LWork.Bottom - LWork.Top, 96, LPPI)), LOptions);
  FWindow.SetBounds(MulDiv(LPlaced.Left, LPPI, 96), MulDiv(LPlaced.Top, LPPI, 96),
    MulDiv(LPlaced.Width, LPPI, 96), MulDiv(LPlaced.Height, LPPI, 96));
end;

procedure TLCLPopover.Present(const AOptions: TNyxPopoverOptions;
  const AFocusID: TNyxText);
var
  LFocus: TWinControl;
begin

  if not AnchorAvailable then
  begin
    raise ENyxModel.Create('Popover anchor is unavailable');
  end;
  FOptions := AOptions;
  FTargets.Capture;
  FWindow.Caption := AOptions.Title;
  Reposition;

  if not Mounted then
  begin
    FRenderer.Render(Document, GetContent.Node, FWindow, State, Collections);
  end;
  FWindow.Color := TControlAccess(FRenderer.ControlFor(GetContent.ID)).Color;
  LFocus := nil;

  if AFocusID <> '' then
  begin
    LFocus := FRenderer.FocusFor(AFocusID, niDesign);

    if (LFocus = nil) or not NyxInteractionPolicy(
      FRenderer.Root.Find(AFocusID)).CanIssueCommand then
    begin
      raise ENyxModel.Create('Popover initial part has no available focus face');
    end;
  end;
  try
    FWindow.Show;
    FRenderer.Sync;
    Reposition;
    FRenderer.Sync;
    Application.AddOnUserInputHandler(UserInput);
    { LCL's application after-key handler observes child consumption first.
      A zero key is never interpreted as Escape. No KeyPreview interception. }
    Application.AddOnKeyDownHandler(KeyDown, False);
    FTimer.Enabled := True;

    if LFocus <> nil then
    begin

      if not LFocus.CanSetFocus then
      begin
        raise ENyxModel.Create('Popover initial part cannot receive focus');
      end;
      LFocus.SetFocus;

      if Screen.ActiveControl <> LFocus then
      begin
        raise ENyxModel.Create('Popover initial focus was refused');
      end;
    end;
    { Showing a standard form may activate it. Explicit no-focus presentation
      returns the previous native active control while keeping the weak capture. }

    if (LFocus = nil) and (FTargets.Invoker <> nil) and
      FTargets.Invoker.CanSetFocus then
    begin
      FTargets.Invoker.SetFocus;
    end;
  except
    Conceal(True);
    raise;
  end;
end;

procedure TLCLPopover.Conceal(ARestoreFocus: Boolean);
begin

  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
  end;
  Application.RemoveOnUserInputHandler(UserInput);
  Application.RemoveOnKeyDownHandler(KeyDown);

  if FWindow <> nil then
  begin
    FWindow.Hide;
  end;

  if ARestoreFocus and (FTargets <> nil) then
  begin
    FTargets.ReturnFocus;
  end;
end;

procedure TLCLPopover.Tick(ASender: TObject);
var
  LKeepAlive: INyxPopover;
begin
  LKeepAlive := Self;

  if IsOpen then
  begin

    if AnchorAvailable then
    begin
      Reposition;
    end
    else
    begin
      Dismiss(nprAnchorUnavailable);
    end;
  end;
  LKeepAlive.GetOpen;
end;

procedure TLCLPopover.UserInput(ASender: TObject; var AMessage: TLMessage);
var
  LKeepAlive: INyxPopover;
  LPoint: TPoint;
  LOrigin: TPoint;
  LAnchor: TRect;
  LControl: TControl;
begin
  LKeepAlive := Self;

  if IsOpen and (npdOutsidePress in FOptions.Dismissals) and
    ((AMessage.Msg = LM_LBUTTONDOWN) or (AMessage.Msg = LM_RBUTTONDOWN) or
      (AMessage.Msg = LM_MBUTTONDOWN)) then
  begin

    if not GetCursorPos(LPoint) then
    begin
      { A locked/noninteractive desktop may refuse cursor coordinates. Never
        compare uninitialized geometry. A live LCL target still establishes its
        exact window/ancestor ownership; an unknown sender cannot dismiss. }

      if ASender is TControl then
      begin
        LControl := TControl(ASender);

        if not FamilyContainsControl(LControl) then
        begin
          Dismiss(nprOutsidePress);
        end;
      end;
      Exit;
    end;

    if AnchorAvailable then
    begin
      LOrigin := FTargets.Anchor.ClientToScreen(Point(0, 0));
      LAnchor := Rect(LOrigin.X, LOrigin.Y, LOrigin.X + FTargets.Anchor.Width,
        LOrigin.Y + FTargets.Anchor.Height);

      if not FamilyContains(LPoint) and not PtInRect(LAnchor, LPoint) then
      begin
        Dismiss(nprOutsidePress);
      end;
    end;
  end;
  LKeepAlive.GetOpen;
end;

procedure TLCLPopover.KeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
var
  LKeepAlive: INyxPopover;
  LActive: TWinControl;
begin
  LKeepAlive := Self;
  LActive := Screen.ActiveControl;

  if IsOpen and (npdEscape in FOptions.Dismissals) and (AKey = VK_ESCAPE) and
    ((GetParentForm(LActive) = FWindow) or (LActive = FTargets.Anchor)) then
  begin
    AKey := 0;
    Dismiss(nprEscape);
  end;
  LKeepAlive.GetOpen;
end;

procedure TLCLPopover.WindowClose(ASender: TObject; var AAction: TCloseAction);
begin
  AAction := caNone;
  Dismiss(nprAction);
end;

procedure TLCLPopover.Deactivate(ASender: TObject);
var
  LMessage: TLMessage;
begin

  { Window activation alone is not an outside press. Let ordinary keyboard
    focus leave the nonmodal view. Mouse input elsewhere in this application
    uses UserInput; this covers a press in a different application. }

  if IsOpen and (npdOutsidePress in FOptions.Dismissals) and
    ((GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0) or
      (GetKeyState(VK_MBUTTON) < 0)) then
  begin
    { Apply the same exact hit test: clicking the invoker is not outside.
      An application may choose a toggle command on that ordinary Nyx button. }
    LMessage := Default(TLMessage);
    LMessage.Msg := LM_LBUTTONDOWN;
    UserInput(ASender, LMessage);
  end;
end;

function TLCLPopover.GetWindow: TForm;
begin
  Result := FWindow;
end;

function TLCLPopover.GetRenderer: TNyxLCLRenderer;
begin
  Result := FRenderer;
end;

function TLCLPopover.GetEvents: INyxEvents;
begin
  Result := FRenderer.Events;
end;

function NewNyxLCLPopover(AAnchor: TWinControl; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; ATheme: TNyxTheme;
  const AParent: INyxLCLPopover): INyxLCLPopover;
begin
  Result := TLCLPopover.Create(AAnchor, ADocument, ARoot, ATheme, AParent);
end;

procedure TLCLPopover.IncludeWindow(AWindow: TForm);
begin
  SetLength(FFamilyWindows, Length(FFamilyWindows) + 1);
  FFamilyWindows[High(FFamilyWindows)] := AWindow;

  if FParent <> nil then
  begin
    FParent.IncludeWindow(AWindow);
  end;
end;

procedure TLCLPopover.ExcludeWindow(AWindow: TForm);
var
  LIndex: Integer;
  LMove: Integer;
begin
  for LIndex := 0 to High(FFamilyWindows) do
  begin

    if FFamilyWindows[LIndex] = AWindow then
    begin
      for LMove := LIndex + 1 to High(FFamilyWindows) do
      begin
        FFamilyWindows[LMove - 1] := FFamilyWindows[LMove];
      end;
      SetLength(FFamilyWindows, Length(FFamilyWindows) - 1);
      Break;
    end;
  end;

  if FParent <> nil then
  begin
    FParent.ExcludeWindow(AWindow);
  end;
end;

function TLCLPopover.FamilyContains(const APoint: TPoint): Boolean;
var
  LIndex: Integer;
begin
  Result := PtInRect(FWindow.BoundsRect, APoint);
  for LIndex := 0 to High(FFamilyWindows) do
  begin

    if FFamilyWindows[LIndex].Visible and
      PtInRect(FFamilyWindows[LIndex].BoundsRect, APoint) then
    begin
      Exit(True);
    end;
  end;
end;

function TLCLPopover.FamilyContainsControl(AControl: TControl): Boolean;
var
  LWindow: TCustomForm;
  LAncestor: TControl;
  LIndex: Integer;
begin
  LAncestor := AControl;
  while LAncestor <> nil do
  begin

    if LAncestor = FTargets.Anchor then
    begin
      Exit(True);
    end;
    LAncestor := LAncestor.Parent;
  end;
  LWindow := GetParentForm(AControl);
  Result := (LWindow <> nil) and (LWindow = FWindow);
  for LIndex := 0 to High(FFamilyWindows) do
  begin

    if (LWindow = FFamilyWindows[LIndex]) and FFamilyWindows[LIndex].Visible then
    begin
      Exit(True);
    end;
  end;
end;

end.
