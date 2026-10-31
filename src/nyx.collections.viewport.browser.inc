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

function TBrowserMount.SupportsViewportReveal: Boolean;
begin
  Result := (FHost <> nil) and ((FHost.tagName = 'TABLE') or (FHost.tagName = 'UL'));
end;

procedure TBrowserMount.QueueViewportReveal;
begin

  if ViewportRevealPending then
  begin
    QueueWindow;
  end;
end;

function TBrowserMount.RevealViewportTarget(const AOptions: TNyxCollectionRevealOptions;
  ARow, AColumn: Integer; ATakeFocus: Boolean; out AFullyVisible: Boolean;
  out AReason: TNyxCollectionRevealReason): TNyxCollectionRevealStatus;
var
  LRequest: INyxCollectionReveal;
  LHost: TJSHTMLElement;
  LRow: TBrowserRow;
  LFace: TJSHTMLElement;
  LBounds: TJSDOMRect;
  LClipLeft: Double;
  LClipRight: Double;
  LClipTop: Double;
  LClipBottom: Double;
  LScaleX: Double;
  LScaleY: Double;
  LDeltaX: Double;
  LDeltaY: Double;
  LOldTop: Double;
  LOldLeft: Double;
  LMoved: Boolean;
  LUpdating: Boolean;
  LFocus: TFocusOptions;
  LHorizontal: TNyxCollectionRevealAlignment;

  function Current: Boolean;
  begin
    Result := ViewportRequestCurrent(LRequest) and (FHost = LHost) and
      (FRendered <> nil) and (ARow >= 0) and (ARow < Length(FRows)) and
      (FRows[ARow] <> nil);
  end;

  procedure ReadClip;
  var
    LRect: TJSDOMRect;
    LAncestor: TJSHTMLElement;
    LStyle: TJSCSSStyleDeclaration;
    LOverflow: String;
    LSX: Double;
    LSY: Double;
    LColumn: Integer;
    LPin: TNyxColumnPin;
  begin
    { Detached or deliberately hidden controls retain useful layout boxes in
      some browsers. Geometry alone cannot establish an available viewport. }
    LStyle := window.getComputedStyle(LFace);

    if not document.documentElement.contains(LHost) or
      (LStyle.getPropertyValue('visibility') = 'hidden') or
      (LStyle.getPropertyValue('visibility') = 'collapse') then
    begin
      LClipLeft := 0;
      LClipRight := 0;
      LClipTop := 0;
      LClipBottom := 0;
      Exit;
    end;
    LRect := LHost.getBoundingClientRect;
    LScaleX := 1;
    LScaleY := 1;

    if LHost.offsetWidth > 0 then
    begin
      LScaleX := LRect.width / LHost.offsetWidth;
    end;

    if LHost.offsetHeight > 0 then
    begin
      LScaleY := LRect.height / LHost.offsetHeight;
    end;

    if IsNan(LScaleX) or IsInfinite(LScaleX) or (LScaleX <= 0) or
      IsNan(LScaleY) or IsInfinite(LScaleY) or (LScaleY <= 0) then
    begin
      LClipLeft := 0;
      LClipRight := 0;
      LClipTop := 0;
      LClipBottom := 0;
      Exit;
    end;
    LClipLeft := Max(0, LRect.left + LHost.clientLeft * LScaleX);
    LClipRight := Min(window.innerWidth,
      LRect.left + (LHost.clientLeft + LHost.clientWidth) * LScaleX);
    LClipTop := Max(0, LRect.top + LHost.clientTop * LScaleY);
    LClipBottom := Min(window.innerHeight,
      LRect.top + (LHost.clientTop + LHost.clientHeight) * LScaleY);
    LStyle := window.getComputedStyle(LHost);

    if LStyle.getPropertyValue('content-visibility') = 'hidden' then
    begin
      LClipBottom := LClipTop;
    end;
    LAncestor := TJSHTMLElement(LHost.parentElement);
    while LAncestor <> nil do
    begin
      LStyle := window.getComputedStyle(LAncestor);

      if LStyle.getPropertyValue('content-visibility') = 'hidden' then
      begin
        LClipBottom := LClipTop;
        Exit;
      end;
      LRect := LAncestor.getBoundingClientRect;
      LSX := 1;
      LSY := 1;

      if LAncestor.offsetWidth > 0 then
      begin
        LSX := LRect.width / LAncestor.offsetWidth;
      end;

      if LAncestor.offsetHeight > 0 then
      begin
        LSY := LRect.height / LAncestor.offsetHeight;
      end;
      LOverflow := LStyle.getPropertyValue('overflow-x');

      if (LOverflow = 'auto') or (LOverflow = 'scroll') or
        (LOverflow = 'hidden') or (LOverflow = 'clip') then
      begin
        LClipLeft := Max(LClipLeft, LRect.left + LAncestor.clientLeft * LSX);
        LClipRight := Min(LClipRight,
          LRect.left + (LAncestor.clientLeft + LAncestor.clientWidth) * LSX);
      end;
      LOverflow := LStyle.getPropertyValue('overflow-y');

      if (LOverflow = 'auto') or (LOverflow = 'scroll') or
        (LOverflow = 'hidden') or (LOverflow = 'clip') then
      begin
        LClipTop := Max(LClipTop, LRect.top + LAncestor.clientTop * LSY);
        LClipBottom := Min(LClipBottom,
          LRect.top + (LAncestor.clientTop + LAncestor.clientHeight) * LSY);
      end;
      LAncestor := TJSHTMLElement(LAncestor.parentElement);
    end;
    for LColumn := 0 to High(FHeaderCells) do
    begin
      LRect := FHeaderCells[LColumn].getBoundingClientRect;

      if (LRect.width > 0) and (LRect.right > LClipLeft) and
        (LRect.left < LClipRight) and
        (window.getComputedStyle(FHeaderCells[LColumn]).getPropertyValue('position') = 'sticky') then
      begin
        LClipTop := Max(LClipTop, Min(LClipBottom, LRect.bottom));
      end;
    end;

    if FHasColumnLayout and AOptions.Field.Defined and
      (FColumnPresentation.EffectivePin(AColumn) = ncpNone) then
    begin
      { A middle cell cannot count opaque start/end panes as visible space.
        EffectivePin already applies the shared narrow-overflow policy. }
      for LColumn := 0 to High(LRow.FCells) do
      begin
        LPin := FColumnPresentation.EffectivePin(LColumn);

        if ColumnVisible(LColumn) and (LPin <> ncpNone) then
        begin
          LRect := LRow.FCells[LColumn].getBoundingClientRect;

          if (LRect.width > 0) and (LRect.left <= LClipLeft + 0.75) then
          begin
            LClipLeft := Max(LClipLeft, LRect.right);
          end
          else if (LRect.width > 0) and (LRect.right >= LClipRight - 0.75) then
          begin
            LClipRight := Min(LClipRight, LRect.left);
          end;
        end;
      end;
    end;
  end;

begin
  Result := ncvsPending;
  AReason := ncvrNone;
  AFullyVisible := False;
  LRequest := CurrentViewportRequest;
  LHost := FHost;

  if (FComposingInput <> nil) or FCompositionEnding then
  begin
    AReason := ncvrInputUnavailable;
    Exit(ncvsRefused);
  end;
  EnsureRow(ARow);

  if not Current then
  begin
    Exit;
  end;
  LRow := FRows[ARow];

  if (LRow.FRef.Collection.Name <> AOptions.Item.Collection.Name) or
    (LRow.FRef.ID <> AOptions.Item.ID) or not LHost.contains(LRow.FElement) then
  begin
    AReason := ncvrChanged;
    Exit(ncvsCancelled);
  end;

  if (AColumn < 0) or (AColumn >= Length(LRow.FCells)) then
  begin
    AReason := ncvrUnknownField;
    Exit(ncvsRefused);
  end;
  LFace := LRow.FElement;

  if AOptions.Field.Defined then
  begin
    LFace := LRow.FCells[AColumn];
  end;
  ReadClip;

  if (LClipBottom <= LClipTop) or (LClipRight <= LClipLeft) then
  begin
    AReason := ncvrNoViewport;
    Exit(ncvsRefused);
  end;
  LBounds := LFace.getBoundingClientRect;
  LDeltaY := NyxCollectionRevealDelta(LBounds.top, LBounds.height, LClipTop,
    LClipBottom - LClipTop, AOptions.VerticalAlignment);
  LDeltaX := 0;

  if AOptions.Field.Defined and
    (not FHasColumnLayout or (FColumnPresentation.EffectivePin(AColumn) = ncpNone)) then
  begin
    LHorizontal := AOptions.HorizontalAlignment;

    if window.getComputedStyle(LHost).getPropertyValue('direction') = 'rtl' then
    begin

      if LHorizontal = ncvaStart then
      begin
        LHorizontal := ncvaEnd;
      end
      else if LHorizontal = ncvaEnd then
      begin
        LHorizontal := ncvaStart;
      end;
    end;
    LDeltaX := NyxCollectionRevealDelta(LBounds.left, LBounds.width, LClipLeft,
      LClipRight - LClipLeft, LHorizontal);
  end;

  if not Current then
  begin
    Exit;
  end;
  LOldTop := TScrollableElement(LHost).scrollTopPixels;
  LOldLeft := TScrollableElement(LHost).scrollLeftPixels;
  TScrollableElement(LHost).scrollTopPixels := LOldTop + LDeltaY / LScaleY;
  TScrollableElement(LHost).scrollLeftPixels := LOldLeft + LDeltaX / LScaleX;
  FViewportScrollRequest := LRequest;
  FViewportExpectedTop := TScrollableElement(LHost).scrollTopPixels;
  FViewportExpectedLeft := TScrollableElement(LHost).scrollLeftPixels;
  LMoved := (Abs(FViewportExpectedTop - LOldTop) > 0.25) or
    (Abs(FViewportExpectedLeft - LOldLeft) > 0.25);

  if FViewportMeasured or LMoved then
  begin
    QueueWindow;
    Exit;
  end;
  ReadClip;
  LBounds := LFace.getBoundingClientRect;

  if (LBounds.bottom <= LClipTop) or (LBounds.top >= LClipBottom) or
    (AOptions.Field.Defined and
      ((LBounds.right <= LClipLeft) or (LBounds.left >= LClipRight))) then
  begin
    AReason := ncvrNoViewport;
    Exit(ncvsRefused);
  end;
  AFullyVisible := (LBounds.top >= LClipTop - 0.75) and
    (LBounds.bottom <= LClipBottom + 0.75) and
    (not AOptions.Field.Defined or
      ((LBounds.left >= LClipLeft - 0.75) and (LBounds.right <= LClipRight + 0.75)));

  if ATakeFocus and Current then
  begin
    LUpdating := FUpdating;
    FUpdating := True;
    try
      FFocusedColumn := AColumn;
      LFocus := TFocusOptions.new;
      LFocus.preventScroll := True;
      { This deliberate focus does not enter edit or commit the old input.
        Its exact draft remains resident/dormant under the existing guards. }
      TFocusable(LRow.FCells[AColumn]).focus(LFocus);
    finally
      FUpdating := LUpdating;
    end;
  end;
  Result := ncvsCompleted;
end;
