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

type
  { A local managed lease keeps copied projection metrics alive if a native
    setter retires the calling renderer. Every callout checks the same weak
    generation; neither state nor component notices retain that renderer. }
  INyxNativeContentScale = interface(IInterface)
    ['{504E4679-C8F1-46E7-B5DC-20720215A844}']
    procedure Restore(AHost: TWinControl; ARoot: TControl;
      const AGeneration: INyxLCLBindingGeneration);
    procedure Validate(AHost: TWinControl; ARoot: TControl; AScale: Double;
      const AGeneration: INyxLCLBindingGeneration);
    procedure Project(AHost: TWinControl; ARoot: TControl; AScale: Double;
      const AGeneration: INyxLCLBindingGeneration);
  end;

  { One absolute logical baseline. Control is borrowed and revoked by component
    notification before destruction. No text, caret, event handler or model is
    copied or restored by scaling. Native integer geometry rounds once per frame. }
  TNativeScaleRow = record
    Control: TControl;
    Bounds: TRect;
    FontHeight: Integer;
    ProjectedFontHeight: Integer;
    ParentFont: Boolean;
    MinimumWidth: Integer;
    MinimumHeight: Integer;
    MaximumWidth: Integer;
    MaximumHeight: Integer;
    Seen: Boolean;
  end;

  { Non-owning address lookup avoids repeated whole-tree scans per native face.
    Dead controls are removed immediately; the row vector compacts between passes. }
  TNativeScaleIndex = TDictionary<TControl, Integer>;

  { Forward implementation identity for a weak component-removal receiver. }
  TNativeContentScale = class;

  { LCL owns the notification relation, not this object. Its borrowed scale
    owner disconnects before freeing the notice, removing all remaining links. }
  TNativeScaleNotice = class(TComponent)
  private
    FScale: TNativeContentScale;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  { UI-owned absolute projection. The renderer retains a world-sized layout
    frame independently of physical clipping. Its actual child HWNDs/fonts
    change, while the normal renderer keeps logical boxes for layout and queries.
    All metric writes occur under its physical
    frame/layout guard, with a managed lease and generation checks at callouts. }
  TNativeContentScale = class(TInterfacedObject, INyxNativeContentScale)
  private
    FRows: array of TNativeScaleRow;
    FIndex: TNativeScaleIndex;
    FNotice: TNativeScaleNotice;
    FDead: Integer;
    FProjected: Boolean;
    function Track(AControl: TControl): Integer;
    procedure Removed(AControl: TControl);
    procedure Compact;
    procedure Capture(AControl: TControl; AChildren: Boolean;
      const AGeneration: INyxLCLBindingGeneration);
    procedure RestoreTree(AControl: TControl; AChildren: Boolean;
      const AGeneration: INyxLCLBindingGeneration);
    procedure CheckRows(AHost: TWinControl; AScale: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Restore(AHost: TWinControl; ARoot: TControl;
      const AGeneration: INyxLCLBindingGeneration);
    procedure Validate(AHost: TWinControl; ARoot: TControl; AScale: Double;
      const AGeneration: INyxLCLBindingGeneration);
    procedure Project(AHost: TWinControl; ARoot: TControl; AScale: Double;
      const AGeneration: INyxLCLBindingGeneration);
  end;

function NativeScalePixel(AValue: Integer; AScale: Double): Integer;
var
  LValue: Double;
begin
  LValue := AValue * AScale;

  if IsNan(LValue) or IsInfinite(LValue) or (Abs(LValue) > High(Integer) - 1.0) then
  begin
    raise ENyxModel.Create('Native uniform coordinate exceeds integer projection');
  end;
  Result := Round(LValue);
end;

function NativeScaleFont(AValue: Integer; AScale: Double): Integer;
begin
  Result := NativeScalePixel(AValue, AScale);

  if (Result = 0) and (AValue <> 0) then
  begin
    Result := 1;

    if AValue < 0 then
    begin
      Result := -1;
    end;
  end;

  if Abs(Result) > MaximumNyxCanvasControlExtent then
  begin
    raise ENyxModel.Create('Native uniform font exceeds the live extent budget');
  end;
end;

procedure TNativeScaleNotice.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FScale <> nil) and (AComponent is TControl) then
  begin
    FScale.Removed(TControl(AComponent));
  end;
end;

constructor TNativeContentScale.Create;
begin
  inherited Create;
  FIndex := TNativeScaleIndex.Create;
  FNotice := TNativeScaleNotice.Create(nil);
  FNotice.FScale := Self;
end;

destructor TNativeContentScale.Destroy;
begin
  FNotice.FScale := nil;
  FNotice.Free;
  FIndex.Free;
  inherited Destroy;
end;

function TNativeContentScale.Track(AControl: TControl): Integer;
begin

  if FIndex.TryGetValue(AControl, Result) then
  begin
    Exit;
  end;
  Result := Length(FRows);
  SetLength(FRows, Result + 1);
  FRows[Result].Control := AControl;
  FRows[Result].Bounds := AControl.BoundsRect;
  FRows[Result].FontHeight := TNyxControlAccess(AControl).Font.Height;
  FRows[Result].ProjectedFontHeight := FRows[Result].FontHeight;
  FRows[Result].ParentFont := TNyxControlAccess(AControl).ParentFont;
  FRows[Result].MinimumWidth := AControl.Constraints.MinWidth;
  FRows[Result].MinimumHeight := AControl.Constraints.MinHeight;
  FRows[Result].MaximumWidth := AControl.Constraints.MaxWidth;
  FRows[Result].MaximumHeight := AControl.Constraints.MaxHeight;
  FIndex.Add(AControl, Result);
  AControl.FreeNotification(FNotice);
end;

procedure TNativeContentScale.Removed(AControl: TControl);
var
  LIndex: Integer;
begin

  if FIndex.TryGetValue(AControl, LIndex) then
  begin
    FRows[LIndex].Control := nil;
    FIndex.Remove(AControl);
    Inc(FDead);
  end;
end;

procedure TNativeContentScale.Compact;
var
  LIndex: Integer;
  LCount: Integer;
begin

  if FDead <= Length(FRows) div 2 then
  begin
    Exit;
  end;
  FIndex.Clear;
  LCount := 0;
  for LIndex := 0 to High(FRows) do
  begin

    if FRows[LIndex].Control <> nil then
    begin
      FRows[LCount] := FRows[LIndex];
      FIndex.Add(FRows[LCount].Control, LCount);
      Inc(LCount);
    end;
  end;
  SetLength(FRows, LCount);
  FDead := 0;
end;

procedure TNativeContentScale.RestoreTree(AControl: TControl; AChildren: Boolean;
  const AGeneration: INyxLCLBindingGeneration);
var
  LIndex: Integer;
  LChild: Integer;
  LRow: TNativeScaleRow;
begin
  RequireNativeGeneration(AGeneration);
  LIndex := Track(AControl);
  LRow := FRows[LIndex];
  { Restore paint metrics too, before intrinsic measurement observes a themed
    button. The copied theme/glyph data itself always remains logical. }

  if AControl is TNyxLCLButton then
  begin
    TNyxLCLButton(AControl).ContentScale := 1;
    RequireNativeGeneration(AGeneration);
  end
  else if AControl is TNyxLCLSurface then
  begin
    TNyxLCLSurface(AControl).ContentScale := 1;
    RequireNativeGeneration(AGeneration);
  end;
  { Ordinary Sync may deliberately replace an explicit role font before Resize.
    Preserve that logical value instead of reviving the previous projection. }

  if not TNyxControlAccess(AControl).ParentFont and
    (TNyxControlAccess(AControl).Font.Height <> LRow.ProjectedFontHeight) then
  begin
    LRow.FontHeight := TNyxControlAccess(AControl).Font.Height;
    FRows[LIndex].FontHeight := LRow.FontHeight;
  end;
  AControl.Constraints.MinWidth := 0;
  RequireNativeGeneration(AGeneration);
  AControl.Constraints.MinHeight := 0;
  RequireNativeGeneration(AGeneration);
  AControl.Constraints.MaxWidth := 0;
  RequireNativeGeneration(AGeneration);
  AControl.Constraints.MaxHeight := 0;
  RequireNativeGeneration(AGeneration);
  TNyxControlAccess(AControl).ParentFont := LRow.ParentFont;
  RequireNativeGeneration(AGeneration);

  if not LRow.ParentFont then
  begin
    TNyxControlAccess(AControl).Font.Height := LRow.FontHeight;
    RequireNativeGeneration(AGeneration);
  end;

  if AChildren then
  begin
    AControl.BoundsRect := LRow.Bounds;
    RequireNativeGeneration(AGeneration);
  end;
  AControl.Constraints.MinWidth := LRow.MinimumWidth;
  RequireNativeGeneration(AGeneration);
  AControl.Constraints.MinHeight := LRow.MinimumHeight;
  RequireNativeGeneration(AGeneration);
  AControl.Constraints.MaxWidth := LRow.MaximumWidth;
  RequireNativeGeneration(AGeneration);
  AControl.Constraints.MaxHeight := LRow.MaximumHeight;
  RequireNativeGeneration(AGeneration);

  if AChildren and (AControl is TWinControl) then
  begin
    LChild := 0;
    while LChild < TWinControl(AControl).ControlCount do
    begin
      RestoreTree(TWinControl(AControl).Controls[LChild], True, AGeneration);
      RequireNativeGeneration(AGeneration);
      Inc(LChild);
    end;
  end;
end;

procedure TNativeContentScale.Restore(AHost: TWinControl; ARoot: TControl;
  const AGeneration: INyxLCLBindingGeneration);
begin
  RequireNativeGeneration(AGeneration);
  Compact;

  if FProjected then
  begin
    RestoreTree(AHost, False, AGeneration);
    RestoreTree(ARoot, True, AGeneration);
    RequireNativeGeneration(AGeneration);
    FProjected := False;
  end;
end;

procedure TNativeContentScale.Capture(AControl: TControl; AChildren: Boolean;
  const AGeneration: INyxLCLBindingGeneration);
var
  LIndex: Integer;
  LChild: Integer;
begin
  RequireNativeGeneration(AGeneration);
  LIndex := Track(AControl);
  FRows[LIndex].Bounds := AControl.BoundsRect;
  FRows[LIndex].FontHeight := TNyxControlAccess(AControl).Font.Height;
  FRows[LIndex].ParentFont := TNyxControlAccess(AControl).ParentFont;
  FRows[LIndex].MinimumWidth := AControl.Constraints.MinWidth;
  FRows[LIndex].MinimumHeight := AControl.Constraints.MinHeight;
  FRows[LIndex].MaximumWidth := AControl.Constraints.MaxWidth;
  FRows[LIndex].MaximumHeight := AControl.Constraints.MaxHeight;
  FRows[LIndex].Seen := True;

  if AChildren and (AControl is TWinControl) then
  begin
    for LChild := 0 to TWinControl(AControl).ControlCount - 1 do
    begin
      Capture(TWinControl(AControl).Controls[LChild], True, AGeneration);
      RequireNativeGeneration(AGeneration);
    end;
  end;
end;

procedure TNativeContentScale.CheckRows(AHost: TWinControl; AScale: Double);
var
  LIndex: Integer;
  LRow: TNativeScaleRow;
  LWidth: Integer;
  LHeight: Integer;
begin
  for LIndex := 0 to High(FRows) do
  begin
    LRow := FRows[LIndex];

    if (LRow.Control = nil) or not LRow.Seen then
    begin
      Continue;
    end;
    NativeScaleFont(LRow.FontHeight, AScale);
    { Constraint setters run after admission too. Validate all four copied
      values before the first projected face/font write, including loose maxima
      that may be much larger than the current measured control. }
    NativeScalePixel(LRow.MinimumWidth, AScale);
    NativeScalePixel(LRow.MinimumHeight, AScale);
    NativeScalePixel(LRow.MaximumWidth, AScale);
    NativeScalePixel(LRow.MaximumHeight, AScale);

    if LRow.Control is TNyxLCLButton then
    begin
      TNyxLCLButton(LRow.Control).ValidateContentScale(AScale);
    end
    else if LRow.Control is TNyxLCLSurface then
    begin
      TNyxLCLSurface(LRow.Control).ValidateContentScale(AScale);
    end;

    if LRow.Control = AHost then
    begin
      Continue;
    end;
    LWidth := NativeScalePixel(LRow.Bounds.Right, AScale) -
      NativeScalePixel(LRow.Bounds.Left, AScale);
    LHeight := NativeScalePixel(LRow.Bounds.Bottom, AScale) -
      NativeScalePixel(LRow.Bounds.Top, AScale);

    if (LWidth > MaximumNyxCanvasControlExtent) or
      (LHeight > MaximumNyxCanvasControlExtent) then
    begin
      raise ENyxModel.Create('Native uniform face exceeds the live extent budget');
    end;
  end;
end;

procedure TNativeContentScale.Validate(AHost: TWinControl; ARoot: TControl; AScale: Double;
  const AGeneration: INyxLCLBindingGeneration);
begin
  RequireNativeGeneration(AGeneration);

  if not FProjected then
  begin
    Capture(AHost, False, AGeneration);
    Capture(ARoot, True, AGeneration);
  end;
  CheckRows(AHost, AScale);
end;

procedure TNativeContentScale.Project(AHost: TWinControl; ARoot: TControl; AScale: Double;
  const AGeneration: INyxLCLBindingGeneration);
var
  LIndex: Integer;
  LRow: TNativeScaleRow;
  LControl: TControl;
  LBounds: TRect;
begin
  RequireNativeGeneration(AGeneration);
  for LIndex := 0 to High(FRows) do
  begin
    FRows[LIndex].Seen := False;
  end;
  Capture(AHost, False, AGeneration);
  Capture(ARoot, True, AGeneration);
  CheckRows(AHost, AScale);
  { Capture/validation completes before the first physical metric changes. The
    host itself keeps physical dimensions; only its font and live children
    project from world metrics. Its private scrollbar faces are not content. }
  for LIndex := 0 to High(FRows) do
  begin
    LRow := FRows[LIndex];
    LControl := LRow.Control;

    if (LControl = nil) or not LRow.Seen then
    begin
      Continue;
    end;

    if LControl is TNyxLCLButton then
    begin
      TNyxLCLButton(LControl).ContentScale := AScale;
      RequireNativeGeneration(AGeneration);
    end
    else if LControl is TNyxLCLSurface then
    begin
      TNyxLCLSurface(LControl).ContentScale := AScale;
      RequireNativeGeneration(AGeneration);
    end;
    FRows[LIndex].ProjectedFontHeight := NativeScaleFont(LRow.FontHeight, AScale);

    if not LRow.ParentFont then
    begin
      TNyxControlAccess(LControl).Font.Height := FRows[LIndex].ProjectedFontHeight;
      RequireNativeGeneration(AGeneration);
    end;

    if LControl <> AHost then
    begin
      LControl.Constraints.MinWidth := 0;
      RequireNativeGeneration(AGeneration);
      LControl.Constraints.MinHeight := 0;
      RequireNativeGeneration(AGeneration);
      LControl.Constraints.MaxWidth := 0;
      RequireNativeGeneration(AGeneration);
      LControl.Constraints.MaxHeight := 0;
      RequireNativeGeneration(AGeneration);
      LBounds := Rect(NativeScalePixel(LRow.Bounds.Left, AScale),
        NativeScalePixel(LRow.Bounds.Top, AScale),
        NativeScalePixel(LRow.Bounds.Right, AScale),
        NativeScalePixel(LRow.Bounds.Bottom, AScale));
      LControl.BoundsRect := LBounds;
      RequireNativeGeneration(AGeneration);
      LControl.Constraints.MinWidth := NativeScalePixel(LRow.MinimumWidth, AScale);
      RequireNativeGeneration(AGeneration);
      LControl.Constraints.MinHeight := NativeScalePixel(LRow.MinimumHeight, AScale);
      RequireNativeGeneration(AGeneration);
      LControl.Constraints.MaxWidth := NativeScalePixel(LRow.MaximumWidth, AScale);
      RequireNativeGeneration(AGeneration);
      LControl.Constraints.MaxHeight := NativeScalePixel(LRow.MaximumHeight, AScale);
      RequireNativeGeneration(AGeneration);
    end;
  end;
  FProjected := True;
end;

procedure TNyxLCLRenderer.ValidateContentScale(AScale: Double);
var
  LScale: INyxNativeContentScale;
  LGeneration: INyxLCLBindingGeneration;
begin
  FEvents.Scheduler.RequireUI;

  if IsNan(AScale) or IsInfinite(AScale) or (AScale <= 0) then
  begin
    raise ENyxModel.Create('Native content scale requires a finite positive factor');
  end;
  LGeneration := FGeneration;

  if FContentScaleState = nil then
  begin
    FContentScaleState := TNativeContentScale.Create;
  end;
  LScale := FContentScaleState as INyxNativeContentScale;

  if (FPanel <> nil) and (FRoot <> nil) then
  begin
    LScale.Validate(FPanel, Binding(FRoot).FControl, AScale, LGeneration);
    RequireNativeGeneration(LGeneration);
  end;
end;

function TNyxLCLRenderer.ContentLayoutWidth(ADefault: Integer): Integer;
begin
  Result := ADefault;

  if FContentLayoutWidth > 0 then
  begin
    Result := FContentLayoutWidth;
  end;
end;

function TNyxLCLRenderer.ContentLayoutHeight(ADefault: Integer): Integer;
begin
  Result := ADefault;

  if FContentLayoutHeight > 0 then
  begin
    Result := FContentLayoutHeight;
  end;
end;

procedure TNyxLCLRenderer.SetContentScale(ALayoutWidth, ALayoutHeight: Integer; AScale: Double);
begin

  if (ALayoutWidth < 1) or (ALayoutHeight < 1) or
    (ALayoutWidth > MaximumNyxCanvasControlExtent) or
    (ALayoutHeight > MaximumNyxCanvasControlExtent) then
  begin
    raise ENyxModel.Create('Native uniform layout requires bounded positive world dimensions');
  end;
  ValidateContentScale(AScale);

  if (FContentScaleFactor = AScale) and (FContentLayoutWidth = ALayoutWidth) and
    (FContentLayoutHeight = ALayoutHeight) then
  begin
    Exit;
  end;
  FContentScaleFactor := AScale;
  FContentLayoutWidth := ALayoutWidth;
  FContentLayoutHeight := ALayoutHeight;

  if FPanel <> nil then
  begin
    { LAST: Resize may retire this renderer through a native callback. }
    Resize(FPanel);
  end;
end;
