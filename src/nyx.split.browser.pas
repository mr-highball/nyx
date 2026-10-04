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
unit nyx.split.browser;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.model, nyx.split;

type
  TNyxBrowserSplit = class;
  TNyxBrowserSplitChanged = procedure(ASplit: TNyxBrowserSplit) of object;

  { Renderer-owned behavior around actual child DOM roots. Reparenting them
    into pane frames retains input identity, selection and scroll. Pointer
    capture keeps a touch gesture attached outside the handle. The adapter
    detaches every listener before its borrowed realized tree is released. }
  TNyxBrowserSplit = class
  private
    FNode: TNyxNode;
    FHost: TJSHTMLElement;
    FDivider: TJSHTMLElement;
    FChildren: array[0..1] of TJSHTMLElement;
    FState: TNyxSplitState;
    FPointer: NativeInt;
    FCaptured: Boolean;
    FClosed: Boolean;
    FDesignMode: Boolean;
    FOnChanged: TNyxBrowserSplitChanged;
    function CanResize: Boolean;
    function Coordinate(AEvent: TJSPointerEvent): Double;
    function Down(AEvent: TJSPointerEvent): Boolean;
    function Move(AEvent: TJSPointerEvent): Boolean;
    function Up(AEvent: TJSPointerEvent): Boolean;
    function Cancel(AEvent: TJSPointerEvent): Boolean;
    function Keyboard(AEvent: TJSKeyboardEvent): Boolean;
    function Click(AEvent: TJSMouseEvent): Boolean;
    procedure ReleaseCapture;
    procedure Publish;
    procedure Notify;
  public
    constructor Create(ANode: TNyxNode; AHost, AFirst, ASecond: TJSHTMLElement;
      ADesignMode: Boolean);
    destructor Destroy; override;
    procedure Update;
    property Node: TNyxNode read FNode;
    property State: TNyxSplitState read FState;
    property Divider: TJSHTMLElement read FDivider;
    property OnChanged: TNyxBrowserSplitChanged read FOnChanged write FOnChanged;
  end;

implementation

constructor TNyxBrowserSplit.Create(ANode: TNyxNode;
  AHost, AFirst, ASecond: TJSHTMLElement; ADesignMode: Boolean);
var
  LPane: TJSHTMLElement;
  LIndex: Integer;
  LChild: TJSHTMLElement;
  LID: TNyxText;
begin
  inherited Create;
  FNode := ANode;
  FHost := AHost;
  FDesignMode := ADesignMode;
  FPointer := -1;
  FState := TNyxSplitState.Create(ANode);
  LID := AHost.getAttribute('data-runtime-id');
  FDivider := TJSHTMLElement(document.createElement('div'));
  FDivider.className := 'nyx-split-divider';
  FDivider.setAttribute('role', 'separator');
  FDivider.setAttribute('aria-label', ANode.Prop('aria-label', 'Resize panes'));
  FDivider.setAttribute('aria-controls', LID + '-first ' + LID + '-second');
  FDivider.title := 'Drag to resize. Arrow keys adjust; Shift adjusts by 10%; Home and End use the limits.';
  FDivider.style.cssText := 'display:flex;align-items:center;justify-content:center;' +
    'touch-action:none;user-select:none;background:var(--nyx-surface);' +
    'color:var(--nyx-muted);border:1px solid var(--nyx-border);box-sizing:border-box;';
  FDivider.textContent := '•••';
  FDivider.onpointerdown := @Down;
  FDivider.onpointermove := @Move;
  FDivider.onpointerup := @Up;
  FDivider.onpointercancel := @Cancel;
  FDivider.onlostpointercapture := @Cancel;
  FDivider.onkeydown := @Keyboard;
  FDivider.onclick := @Click;
  for LIndex := 0 to 1 do
  begin
    LPane := TJSHTMLElement(document.createElement('div'));
    LPane.className := 'nyx-split-pane';
    LPane.style.cssText := 'display:flex;flex-direction:column;min-height:0;min-width:0;overflow:hidden;';

    if LIndex = 0 then
    begin
      LPane.id := LID + '-first';
      LChild := AFirst;
    end
    else
    begin
      FHost.appendChild(FDivider);
      LPane.id := LID + '-second';
      LChild := ASecond;
    end;

    if LChild <> nil then
    begin
      FChildren[LIndex] := LChild;
      LChild.style.setProperty('flex', '1');
      LChild.style.setProperty('min-height', '0');
      LChild.style.setProperty('min-width', '0');
      LPane.appendChild(LChild);
    end;
    FHost.appendChild(LPane);
  end;
  Update;
end;

destructor TNyxBrowserSplit.Destroy;
begin
  FClosed := True;
  FOnChanged := nil;

  if FDivider <> nil then
  begin
    FDivider.onpointerdown := nil;
    FDivider.onpointermove := nil;
    FDivider.onpointerup := nil;
    FDivider.onpointercancel := nil;
    FDivider.onlostpointercapture := nil;
    FDivider.onkeydown := nil;
    FDivider.onclick := nil;
    ReleaseCapture;
  end;
  FState.Free;
  inherited Destroy;
end;

function TNyxBrowserSplit.CanResize: Boolean;
var
  LNode: TNyxNode;
begin
  Result := not FClosed and not FDesignMode and FState.Resizable;
  LNode := FNode;
  while Result and (LNode <> nil) do
  begin
    Result := (LNode.Prop('enabled', 'true') <> 'false') and
      (LNode.Prop('readonly') <> 'true');
    LNode := LNode.Parent;
  end;
end;

procedure TNyxBrowserSplit.Update;
var
  LTracks: TNyxText;
  LIndex: Integer;
begin
  { Runtime state may change enablement during a captured gesture. Cancel that
    gesture immediately during in-place synchronization, preserving its baseline. }

  if FState.Dragging and not CanResize then
  begin
    FState.EndDrag(True);
    ReleaseCapture;
    FNode.Configure.SplitPosition(FState.Position);
  end;
  FHost.style.setProperty('display', 'grid');

  if FNode.Prop('visible', 'true') = 'false' then
  begin
    FHost.style.setProperty('display', 'none');
  end;
  { General binding refreshes restore authored root metrics first. A split
    owns its two root allocations; retain these overrides after that pass. }
  for LIndex := 0 to 1 do
  begin

    if FChildren[LIndex] <> nil then
    begin
      FChildren[LIndex].style.setProperty('flex', '1');
      FChildren[LIndex].style.setProperty('min-height', '0');
      FChildren[LIndex].style.setProperty('min-width', '0');
    end;
  end;
  FHost.style.setProperty('gap', '0');
  FHost.style.setProperty('padding', '0');
  FHost.style.setProperty('min-height', '0');
  FHost.style.setProperty('min-width', '0');
  FHost.style.setProperty('overflow', 'hidden');

  if (FNode.Prop('height') = '') and (FNode.Prop('flex') = '') then
  begin
    FHost.style.setProperty('height', '320px');
  end;
  LTracks := 'minmax(0,' + IntToStr(FState.Position) + 'fr) minmax(0,44px) minmax(0,' +
    IntToStr(100 - FState.Position) + 'fr)';

  if FState.Orientation = nsoStacked then
  begin
    FHost.style.setProperty('grid-template-rows', LTracks);
    FHost.style.setProperty('grid-template-columns', 'minmax(0,1fr)');
    FDivider.setAttribute('aria-orientation', 'horizontal');
    FDivider.style.setProperty('cursor', 'row-resize');
  end
  else
  begin
    FHost.style.setProperty('grid-template-columns', LTracks);
    FHost.style.setProperty('grid-template-rows', 'minmax(0,1fr)');
    FDivider.setAttribute('aria-orientation', 'vertical');
    FDivider.style.setProperty('cursor', 'col-resize');
  end;
  FDivider.setAttribute('aria-valuemin', IntToStr(FState.Minimum));
  FDivider.setAttribute('aria-valuemax', IntToStr(FState.Maximum));
  FDivider.setAttribute('aria-valuenow', IntToStr(FState.Position));

  if CanResize then
  begin
    FDivider.tabIndex := 0;
    FDivider.setAttribute('aria-disabled', 'false');
  end
  else
  begin
    FDivider.tabIndex := -1;
    FDivider.setAttribute('aria-disabled', 'true');
    FDivider.style.setProperty('cursor', 'default');
  end;
end;

function TNyxBrowserSplit.Coordinate(AEvent: TJSPointerEvent): Double;
begin
  Result := AEvent.clientY;

  if FState.Orientation = nsoSideBySide then
  begin
    Result := AEvent.clientX;
  end;
end;

function TNyxBrowserSplit.Down(AEvent: TJSPointerEvent): Boolean;
var
  LExtent: Integer;
begin
  Result := True;

  if not CanResize or FState.Dragging or not AEvent.isPrimary or (AEvent.button <> 0) then
  begin
    Exit;
  end;
  LExtent := FHost.clientHeight;

  if FState.Orientation = nsoSideBySide then
  begin
    LExtent := FHost.clientWidth;
  end;
  FState.BeginDrag(Coordinate(AEvent), LExtent - 44);

  if not FState.Dragging then
  begin
    Exit;
  end;
  FPointer := AEvent.pointerId;
  AEvent.preventDefault;
  AEvent.stopPropagation;
  { Synthetic events in Pascal fixtures cannot own OS pointer capture. Trusted
    physical input always uses the browser's capture facility. }

  if AEvent.isTrusted then
  begin
    FDivider.setPointerCapture(FPointer);
    FCaptured := True;
  end;
  Result := False;
end;

procedure TNyxBrowserSplit.Publish;
begin
  FNode.Configure.SplitPosition(FState.Position);
  Update;
end;

function TNyxBrowserSplit.Move(AEvent: TJSPointerEvent): Boolean;
begin
  Result := True;

  if FState.Dragging and (AEvent.pointerId = FPointer) then
  begin
    AEvent.preventDefault;
    AEvent.stopPropagation;

    if FState.Drag(Coordinate(AEvent)) then
    begin
      Publish;
    end;
    Result := False;
  end;
end;

procedure TNyxBrowserSplit.ReleaseCapture;
begin

  if FCaptured then
  begin
    FCaptured := False;
    FDivider.releasePointerCapture(FPointer);
  end;
  FPointer := -1;
end;

procedure TNyxBrowserSplit.Notify;
begin

  if Assigned(FOnChanged) then
  begin
    { No access after this callback: application navigation can dispose Self. }
    FOnChanged(Self);
  end;
end;

function TNyxBrowserSplit.Up(AEvent: TJSPointerEvent): Boolean;
begin
  Result := True;

  if FState.Dragging and (AEvent.pointerId = FPointer) then
  begin
    AEvent.preventDefault;
    AEvent.stopPropagation;
    FState.Drag(Coordinate(AEvent));
    FState.EndDrag(False);
    ReleaseCapture;
    Publish;
    Result := False;
    Notify;
  end;
end;

function TNyxBrowserSplit.Cancel(AEvent: TJSPointerEvent): Boolean;
begin
  Result := True;

  if not FClosed and FState.Dragging and (AEvent.pointerId = FPointer) then
  begin
    FState.EndDrag(True);
    ReleaseCapture;
    Publish;
  end;
end;

function TNyxBrowserSplit.Keyboard(AEvent: TJSKeyboardEvent): Boolean;
var
  LKey: TNyxKey;
begin
  Result := True;

  if not CanResize or AEvent.defaultPrevented then
  begin
    Exit;
  end;
  LKey := NyxKeyFromBrowser(AEvent.key);

  if (LKey = nkEscapeKey) and FState.Dragging then
  begin
    FState.EndDrag(True);
    ReleaseCapture;
    Publish;
    AEvent.preventDefault;
    Exit(False);
  end;

  if not (LKey in [nkHomeKey, nkEndKey]) and
    not ((FState.Orientation = nsoStacked) and (LKey in [nkUpKey, nkDownKey])) and
    not ((FState.Orientation = nsoSideBySide) and (LKey in [nkLeftKey, nkRightKey])) then
  begin
    Exit;
  end;
  AEvent.preventDefault;
  AEvent.stopPropagation;
  Result := False;

  if FState.Key(LKey, AEvent.shiftKey) then
  begin
    Publish;
    Notify;
  end;
end;

function TNyxBrowserSplit.Click(AEvent: TJSMouseEvent): Boolean;
begin
  AEvent.stopPropagation;
  Result := False;
end;

end.
