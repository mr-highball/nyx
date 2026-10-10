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
{ Viewport projection keeps all logical boxes/control instances. No accepted
  document, text buffer, runtime store or ownership edge changes while scrolling. }

procedure TNyxLCLRenderer.ArrangeInput(ABinding: TNyxLCLBinding; AWidth, AHeight: Integer);
var
  LFrameHeight: Integer;
  LGrouped: TWinControl;
begin

  if (ABinding.FInput = nil) or (ABinding.FInput = ABinding.FControl) then
  begin
    Exit;
  end;

  if ABinding.FCaption <> nil then
  begin
    { Caption and input remain mounted and focus-linked while their outer frame
      is parked. No preferred label extent may escape that frame's allocation. }
    ABinding.FCaption.SetBounds(0, 0, AWidth, Min(20, AHeight));
  end;

  LGrouped := ABinding.FDateField;

  if ABinding.FColorField <> nil then
  begin
    LGrouped := ABinding.FColorField;
  end;

  if LGrouped <> nil then
  begin
    { The grouped picker host owns its editor's geometry. Keep the existing
      themed frame and let Lazarus arrange its real edit and picker button. }
    LFrameHeight := Max(0, AHeight - 24);
    LGrouped.Parent.SetBounds(0, 24, AWidth, LFrameHeight);
    LGrouped.SetBounds(12, 5, Max(0, AWidth - 24), Max(0, AHeight - 34));
  end
  else if ABinding.FInput.Parent is TNyxLCLSurface then
  begin
    LFrameHeight := Max(0, AHeight - 24);
    ABinding.FInput.Parent.SetBounds(0, 24, AWidth, LFrameHeight);
    ABinding.FInput.SetBounds(12, 10, Max(0, AWidth - 24), Max(0, AHeight - 44));
  end
  else
  begin
    ABinding.FInput.SetBounds(0, 24, AWidth, Max(0, AHeight - 24));
  end;
end;

procedure TNyxLCLRenderer.ProjectViewport(ASender: TObject);
var
  LIndex: Integer;
  LChild: Integer;
  LBinding: TNyxLCLBinding;
  LParent: TNyxLCLBinding;
  LParentClip: TNyxViewportBox;
  LOriginX: Integer;
  LOriginY: Integer;
  LParentX: Integer;
  LParentY: Integer;
  LHost: TWinControl;
  LScroll: TNyxLogicalScrollBox;
  LExtentX: Integer;
  LExtentY: Integer;
  LUpdating: Boolean;
  LFrame: TRect;
begin

  if not FVirtualLayout or FProjecting or (FPanel = nil) then
  begin
    Exit;
  end;
  FProjecting := True;
  LUpdating := FUpdating;
  FUpdating := True;
  FPanel.DisableAutoSizing;
  try
    for LIndex := 0 to High(FBindings) do
    begin
      LBinding := FBindings[LIndex];

      if not LBinding.FLogicalBox.Defined then
      begin
        Continue;
      end;
      LParent := LBinding.FLogicalParent;
      LParentX := 0;
      LParentY := 0;
      LOriginX := -FPanel.HorzScrollBar.Position;
      LOriginY := -FPanel.VertScrollBar.Position;
      LParentClip := NyxViewportBox(0, 0, FPanel.ViewportWidth, FPanel.ViewportHeight);

      if LParent <> nil then
      begin
        LParentClip := LParent.FPlacement.Clip;
        LParentX := LParent.FPlacement.ScreenX;
        LParentY := LParent.FPlacement.ScreenY;
        LOriginX := LParent.FScreenBox.X;
        LOriginY := LParent.FScreenBox.Y;
        { Child logical coordinates begin at the native parent's client origin,
          rather than under its group caption/frame. Clip to usable content as
          well as the ancestor viewport, without changing the outer face box. }

        if LParent.FControl is TCustomGroupBox then
        begin
          LFrame := ContentInsets(LParent.FNode);
          Inc(LOriginX, LFrame.Left);
          Inc(LOriginY, LFrame.Top);
          Inc(LParentX, LFrame.Left);
          Inc(LParentY, LFrame.Top);
          LParentClip := NyxPlaceViewport(NyxViewportBox(LParentX, LParentY,
            Max(0, LParent.FPlacement.Width - LFrame.Left - LFrame.Right),
            Max(0, LParent.FPlacement.Height - LFrame.Top - LFrame.Bottom)),
            LParentClip, LParentX, LParentY, High(SmallInt), High(Word)).Clip;
        end;
        { Split panes are ordinary intermediate LCL hosts, not authored nodes.
          Their offsets/clips remain part of physical parenting. }
        LHost := LBinding.FControl.Parent;
        while (LHost <> nil) and (LHost <> LParent.FControl) do
        begin
          Inc(LOriginX, LHost.Left);
          Inc(LOriginY, LHost.Top);
          Inc(LParentX, LHost.Left);
          Inc(LParentY, LHost.Top);
          LParentClip := NyxPlaceViewport(
            NyxViewportBox(LParentX, LParentY, LHost.ClientWidth, LHost.ClientHeight),
            LParentClip, LParentX, LParentY, High(SmallInt), High(Word)).Clip;
          LHost := LHost.Parent;
        end;

        if LParent.FControl is TNyxLogicalScrollBox then
        begin
          LScroll := TNyxLogicalScrollBox(LParent.FControl);
          Dec(LOriginX, LScroll.HorzScrollBar.Position);
          Dec(LOriginY, LScroll.VertScrollBar.Position);
        end;
      end;
      LBinding.FScreenBox := LBinding.FLogicalBox.Translated(LOriginX, LOriginY);
      LBinding.FPlacement := NyxPlaceViewport(LBinding.FScreenBox, LParentClip,
        LParentX, LParentY, High(SmallInt), High(Word));
      { An oversized native input/custom/group face needs its own logical paint
        adapter. Refuse explicitly instead of changing its alignment/viewport
        meaning by pretending a cropped window is the original enormous face. }

      if (((LBinding.FLogicalBox.Width > High(Word)) or
        (LBinding.FLogicalBox.Height > High(Word))) or
        (LBinding.FPlacement.HasArea and
        ((LBinding.FPlacement.ContentOffsetX <> 0) or
        (LBinding.FPlacement.ContentOffsetY <> 0)))) and
        ((LBinding.FInput <> nil) or LBinding.FCustom or
        (LBinding.FControl is TNyxLCLSplitView) or
        not ((LBinding.FControl is TPanel) or
        (LBinding.FControl is TNyxLogicalScrollBox))) then
      begin
        raise ENyxModel.Create('Oversized native face needs a logical paint adapter: ' +
          LBinding.FNode.ID);
      end;

      if LBinding.FPlacement.HasArea then
      begin
        LBinding.FControl.SetBounds(LBinding.FPlacement.X, LBinding.FPlacement.Y,
          LBinding.FPlacement.Width, LBinding.FPlacement.Height);
      end
      else
      begin
        { Lazarus initializes its realized-bounds cache to the all-zero rectangle.
          A window first created at (0,0,0,0) can therefore keep its widgetset's
          default size: the later realization sees an unchanged cached rectangle.
          Use a distinct zero-area physical origin so normal LCL realization sends
          the parked size. Logical boxes/clips are unchanged; visibility, parenting
          and focus eligibility remain intact for focus-triggered Reveal. }
        LBinding.FControl.SetBounds(-1, -1, 0, 0);
      end;
      ArrangeInput(LBinding, LBinding.FPlacement.Width, LBinding.FPlacement.Height);

      if LBinding.FControl is TNyxLCLSurface then
      begin
        TNyxLCLSurface(LBinding.FControl).ProjectFace(NyxViewportBox(
          -LBinding.FPlacement.ContentOffsetX, -LBinding.FPlacement.ContentOffsetY,
          LBinding.FLogicalBox.Width, LBinding.FLogicalBox.Height));
      end;

      if LBinding.FControl is TNyxLogicalScrollBox then
      begin
        LScroll := TNyxLogicalScrollBox(LBinding.FControl);
        LExtentX := LBinding.FLogicalBox.Width;
        LExtentY := LBinding.FLogicalBox.Height;
        for LChild := LIndex + 1 to High(FBindings) do
        begin

          if (FBindings[LChild].FLogicalParent = LBinding) and
            FBindings[LChild].FLogicalBox.Defined then
          begin
            LExtentX := Max(LExtentX, FBindings[LChild].FLogicalBox.Right +
              Metric(LBinding.FNode, 'padding', 0));
            LExtentY := Max(LExtentY, FBindings[LChild].FLogicalBox.Bottom +
              Metric(LBinding.FNode, 'padding', 0));
          end;
        end;
        LScroll.SetLogicalExtent(LExtentX, LExtentY, ProjectViewport);
      end;
    end;
    UpdateSelection;
  finally
    FPanel.EnableAutoSizing;
    FUpdating := LUpdating;
    FProjecting := False;
  end;
end;

function TNyxLCLRenderer.ViewViewport: TNyxViewportSnapshot;
begin
  FEvents.Scheduler.RequireUI;

  if FPanel = nil then
  begin
    raise ENyxModel.Create('A containing viewport requires a mounted view');
  end;
  Result := FPanel.Snapshot;
end;

procedure TNyxLCLRenderer.ScrollView(AX, AY: Integer);
begin
  FEvents.Scheduler.RequireUI;

  if FPanel = nil then
  begin
    raise ENyxModel.Create('Scrolling a containing viewport requires a mounted view');
  end;
  FPanel.HorzScrollBar.Position := AX;
  FPanel.VertScrollBar.Position := AY;
end;

procedure TNyxLCLRenderer.Reveal(const AID: TNyxText; AIdentity: TNyxIdentityKind);
var
  LBinding: TNyxLCLBinding;
  LParent: TNyxLCLBinding;
  LScroll: TNyxLogicalScrollBox;
  LHost: TWinControl;
  LOffsetX: Integer;
  LOffsetY: Integer;

  function Movement(AStart, ASize, APage: Integer): Integer;
  begin
    Result := 0;

    if (AStart < 0) or (ASize > APage) then
    begin
      Result := AStart;
    end
    else if Double(AStart) + ASize > APage then
    begin
      Result := AStart + ASize - APage;
    end;
  end;

  procedure MoveScroll(AScroll: TNyxLogicalScrollBox; AX, AY: Integer);
  begin
    { Addition is evaluated before narrowing: a valid distant identity may be
      requested while already scrolled. Native range/page rules bound the final
      offset; integer overflow must not precede that ordinary clamp. }
    AScroll.HorzScrollBar.Position := Trunc(Max(0.0,
      Min(Double(AScroll.HorzScrollBar.Range), Double(AScroll.HorzScrollBar.Position) + AX)));
    AScroll.VertScrollBar.Position := Trunc(Max(0.0,
      Min(Double(AScroll.VertScrollBar.Range), Double(AScroll.VertScrollBar.Position) + AY)));
  end;

begin
  FEvents.Scheduler.RequireUI;
  LBinding := IdentityBinding(AID, AIdentity);

  if not FVirtualLayout then
  begin
    { Ordinary nested containers clip independently. Reveal the same borrowed
      face from the innermost native scroll scope outward; ScrollInView computes
      descendant coordinates after each prior movement. Stop at our containing
      panel rather than changing presentation in a borrowed application host.
      Parked/detached controls retain the previous harmless no-op behavior. }

    if (FPanel = nil) or not FPanel.IsParentOf(LBinding.FControl) then
    begin
      Exit;
    end;
    LHost := LBinding.FControl.Parent;
    while LHost <> nil do
    begin

      if LHost is TScrollingWinControl then
      begin
        TScrollingWinControl(LHost).ScrollInView(LBinding.FControl);
      end;

      if LHost = FPanel then
      begin
        Break;
      end;
      LHost := LHost.Parent;
    end;
    Exit;
  end;

  if not LBinding.FLogicalBox.Defined then
  begin
    Exit;
  end;
  { Reveal inner scroll scopes first, then the containing view. Each native
    scrollbar change reprojects value-owned boxes without replacing controls. }
  LParent := LBinding.FLogicalParent;
  while LParent <> nil do
  begin

    if LParent.FControl is TNyxLogicalScrollBox then
    begin
      LScroll := TNyxLogicalScrollBox(LParent.FControl);
      LOffsetX := Movement(LBinding.FScreenBox.X - LParent.FScreenBox.X,
        LBinding.FLogicalBox.Width, LScroll.ViewportWidth);
      LOffsetY := Movement(LBinding.FScreenBox.Y - LParent.FScreenBox.Y,
        LBinding.FLogicalBox.Height, LScroll.ViewportHeight);
      MoveScroll(LScroll, LOffsetX, LOffsetY);
    end;
    LParent := LParent.FLogicalParent;
  end;
  LOffsetX := Movement(LBinding.FScreenBox.X, LBinding.FLogicalBox.Width,
    FPanel.ViewportWidth);
  LOffsetY := Movement(LBinding.FScreenBox.Y, LBinding.FLogicalBox.Height,
    FPanel.ViewportHeight);
  MoveScroll(FPanel, LOffsetX, LOffsetY);
end;
