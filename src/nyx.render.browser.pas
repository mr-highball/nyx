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

unit nyx.render.browser;

{$mode delphi}{$H+}
{$codepage utf8}
{$modeswitch externalclass}

interface

uses
  nyx.types,
  nyx.text,
  Classes,
  SysUtils,
  Math,
  JS,
  Web,
  nyx.model,
  nyx.interaction,
  nyx.editing,
  nyx.editing.browser,
  nyx.gestures,
  nyx.gestures.browser,
  nyx.platform,
  nyx.split,
  nyx.split.browser,
  nyx.schema,
  nyx.contract,
  nyx.theme,
  nyx.design.tokens,
  nyx.behavior,
  nyx.events,
  nyx.data,
  nyx.viewport,
  nyx.event.emitter,
  nyx.state,
  nyx.binding,
  nyx.binding.types,
  nyx.collections.view,
  nyx.collections.selection,
  nyx.collections.bindings,
  nyx.collections.view.types,
  nyx.collections.mount,
  nyx.collections.browser,
  nyx.literal.items,
  nyx.composition;

type
  TNyxBrowserRenderer = class;
  TNyxBrowserEvent = TNyxEventHandler;
  TNyxBrowserBindingError = procedure(ANode: TNyxNode; const AReason: TNyxText;
    AFailure: TNyxBindingFailure) of object;
  TNyxBrowserFactory = function(ANode: TNyxNode): TJSHTMLElement;
  { The emitter is initially dormant. It becomes connected only when the whole
    candidate view is accepted, and disconnects before its controls are freed.
    Creators may retain it without retaining the renderer or document. }
  TNyxBrowserEventFactory = function(ANode: TNyxNode;
    const AEmitter: INyxEventEmitter): TJSHTMLElement;
  { Refresh custom markup in place from already admitted runtime properties.
    The callback borrows both arguments, retains children/identity and does not
    write state. Bound custom factories require it; no textContent replacement
    may silently destroy extension-owned markup. }
  TNyxBrowserUpdater = procedure(ANode: TNyxNode; AElement: TJSHTMLElement);

  { A binding borrows a realized node and DOM handles. The renderer owns both
    the binding and realized tree, clearing bindings before freeing that tree.
    All target events cross one portable node/event-name boundary. }
  TNyxBrowserBinding = class
  private
    FRenderer: TNyxBrowserRenderer;
    FNode: TNyxNode;
    FElement: TJSHTMLElement;
    FInput: TJSHTMLElement;
    FCaption: TJSHTMLElement;
    FLastValue: TNyxText;
    FHasValueBaseline: Boolean;
    { Literal rows have their own baseline. An unrelated state publication must
      not destroy option/row handles, selection or scroll. Bound collections and
      creator factories own their content independently of this fallback. }
    FLastItems: TNyxText;
    FHasItemsBaseline: Boolean;
    FCustom: Boolean;
    FUpdater: TNyxBrowserUpdater;
    FViewRevision: Integer;
    FComposing: Boolean;
    FEditingSelection: TNyxTextSelection;
    { Capture IDs are host observations/requests for this exact mounted face.
      Teardown removes listeners before releasing any outstanding capture. }
    FCapturedPointers: array of Integer;
    procedure SyncLiteralItems;
    function BeforeEdit(AEvent: TEventListenerEvent): Boolean;
    function CompositionStart(AEvent: TEventListenerEvent): Boolean;
    function CompositionUpdate(AEvent: TEventListenerEvent): Boolean;
    function CompositionEnd(AEvent: TEventListenerEvent): Boolean;
    function TextSelectionChanged(AEvent: TEventListenerEvent): Boolean;
    function EditingEvent(AEvent: TEventListenerEvent; ATrigger: TNyxTrigger;
      APhase: TNyxEditingPhase): Boolean;
    FCollectionMount: INyxCollectionMount;
    FSplit: TNyxBrowserSplit;
    procedure SplitChanged(ASplit: TNyxBrowserSplit);
    function Click(AEvent: TJSMouseEvent): Boolean;
    function Change(AEvent: TEventListenerEvent): Boolean;
    function Enter(AEvent: TEventListenerEvent): Boolean;
    function Leave(AEvent: TEventListenerEvent): Boolean;
    { The scalar input, separator grip or ordinary leaf; borrowed from this
      mount. Collection descendants are admitted separately by their owner. }
    function FocusElement: TJSHTMLElement;
    function IsFocusBoundary(AEvent: TEventListenerEvent): Boolean;
    { A checked but disabled/hidden HTML radio can suppress every peer's native
      Tab entry. A temporary label entry forwards focus to the same real input
      without scroll, value changes, synthetic activation or extra callbacks. }
    function ForwardRadioFocus(AEvent: TEventListenerEvent): Boolean;
    function KeyDown(AEvent: TJSKeyboardEvent): Boolean;
    function KeyUp(AEvent: TJSKeyboardEvent): Boolean;
    function Keyboard(AEvent: TJSKeyboardEvent; ATrigger: TNyxTrigger): Boolean;
    function DoubleClick(AEvent: TJSMouseEvent): Boolean;
    function PointerDown(AEvent: TJSPointerEvent): Boolean;
    function PointerUp(AEvent: TJSPointerEvent): Boolean;
    function PointerMove(AEvent: TJSPointerEvent): Boolean;
    function PointerEnter(AEvent: TJSPointerEvent): Boolean;
    function PointerExit(AEvent: TJSPointerEvent): Boolean;
    function PointerCancel(AEvent: TJSPointerEvent): Boolean;
    function PointerCapture(AEvent: TJSPointerEvent): Boolean;
    function PointerCaptureLost(AEvent: TJSPointerEvent): Boolean;
    procedure ObserveCapture(APointerID: Integer; ACaptured: Boolean);
    function DragStart(AEvent: TJSDragEvent): Boolean;
    function Drag(AEvent: TJSDragEvent): Boolean;
    function DragEnter(AEvent: TJSDragEvent): Boolean;
    function DragOver(AEvent: TJSDragEvent): Boolean;
    function DragExit(AEvent: TJSDragEvent): Boolean;
    function Drop(AEvent: TJSDragEvent): Boolean;
    function DragEnd(AEvent: TJSDragEvent): Boolean;
    function DragEvent(AEvent: TJSDragEvent; ATrigger: TNyxTrigger;
      APhase: TNyxDragPhase): Boolean;
    function ContextMenu(AEvent: TJSMouseEvent): Boolean;
    function PointerEvent(AEvent: TJSMouseEvent; ATrigger: TNyxTrigger): Boolean;
    function Wheel(AEvent: TJSWheelEvent): Boolean;
    function Scroll(AEvent: TEventListenerEvent): Boolean;
    function ScrollEnd(AEvent: TEventListenerEvent): Boolean;
    function ViewportEvent(AEvent: TEventListenerEvent; ATrigger: TNyxTrigger): Boolean;
    procedure CollectionSelectionChanged(const ABefore, AAfter: INyxCollectionSelection);
  public
    destructor Destroy; override;
  end;

  { Projects the shared contract into real semantic browser controls.
    RegisterFactory admits an owned custom recipe without editing the default
    catalog or renderer. Full Render is the initial correctness path; target
    identity and event ownership remain explicit for later incremental updates.
    Renderer owns its default theme; a supplied theme is borrowed. }
  TNyxBrowserRenderer = class
  private
    FTheme: TNyxTheme;
    FOwnTheme: Boolean;
    { Independent startup palette. Document overrides never accumulate across
      renders or mutate a caller's supplied theme. }
    FBaseTheme: TNyxTheme;
    { A caller-supplied palette remains borrowed and may change between renders.
      Effective document overlays never mutate it or accumulate into its base. }
    FBorrowedTheme: TNyxTheme;
    { Renderer-local CSS identity is not document identity. An embedded preview
      must not change another renderer's palette, font or corner-radius tokens. }
    FThemeScope: TNyxText;
    FRoot: TNyxNode;
    FHost: TJSHTMLElement;
    FDesignMode: Boolean;
    FBindings: array of TNyxBrowserBinding;
    FFactoryKinds: array of TNyxText;
    FFactories: array of TNyxBrowserFactory;
    FEventFactories: array of TNyxBrowserEventFactory;
    FEmitterScope: INyxEventEmitterScope;
    FUpdaters: array of TNyxBrowserUpdater;
    FOnEvent: TNyxBrowserEvent;
    FEvents: INyxEvents;
    FState: TNyxState;
    FOwnState: Boolean;
    FLiveBindings: TNyxLiveBindings;
    FCollectionBindings: INyxCollectionBindings;
    FUpdating: Boolean;
    FForceValues: Boolean;
    FOnBindingError: TNyxBrowserBindingError;
    FLastBindingError: TNyxText;
    FLastBindingFailure: TNyxBindingFailure;
    FLastGestureError: TNyxText;
    FOnGestureFailure: TNyxGestureFailure;
    procedure GestureFailed(const AOriginID, AReason: TNyxText);
    procedure Emit(AOrigin: TNyxNode; const ADispatch: TNyxDispatch);
    function EmitNamed(const AOriginID: TNyxText; const AName: TNyxEventRef;
      const APayload: TNyxDataValue; AHasPayload: Boolean): Boolean;
    procedure BindingFailed(ANode: TNyxNode; const AReason: TNyxText;
      AFailure: TNyxBindingFailure = nbfRejected);
    procedure Clear;
    function FactoryIndex(ANode: TNyxNode): Integer;
    function CreateElement(ANode: TNyxNode; out AInput: TJSHTMLElement): TJSHTMLElement;
    function Build(ANode: TNyxNode): TJSHTMLElement;
    { Retain one eligible entry when a checked radio becomes disabled/hidden.
      Grouping follows the physical HTML name/form boundary, not display rows. }
    procedure SyncRadioFocus;
  public
    constructor Create(ATheme: TNyxTheme = nil);
    destructor Destroy; override;
    procedure RegisterFactory(const AKind: TNyxText; AFactory: TNyxBrowserFactory;
      AUpdater: TNyxBrowserUpdater = nil);
    { Publish a custom producer using declared, typed named event schemas.
      Registration replaces an existing factory for the same exact kind. }
    procedure RegisterEventFactory(const AKind: TNyxKindRef;
      AFactory: TNyxBrowserEventFactory; AUpdater: TNyxBrowserUpdater = nil);
    { Reports this adapter's actual registration/base projection. A custom
      factory takes precedence over a derived primitive's default projection. }
    function Capability(ANode: TNyxNode): TNyxCapability;
    procedure Render(ADocument: TNyxDocument; ARoot: TNyxNode;
      AHost: TJSHTMLElement; ADesignMode: Boolean = False; AState: TNyxState = nil;
      const ACollections: INyxCollectionBindings = nil);
    { A supplied runtime store is borrowed and must outlive this mounted view.
      Otherwise the renderer owns a fresh copy of document defaults. Design mode
      projects that copy but does not subscribe or write application state.
      Failed control edits restore accepted values and report a diagnostic;
      LastBindingError is cleared by the next accepted control command. }
    { Borrow a projected host/control by runtime ID or original design ID. This
      lets applications compose nested Nyx views without global DOM queries or
      private component factories. Missing identities raise a model diagnostic. }
    function ElementFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TJSHTMLElement;
    { Borrow the actual value input, including a labeled/framed editor's inner
      face. Mirrors the LCL adapter: missing identity raises; a component with
      no scalar input returns nil. Valid only until the mounted view ends. }
    function InputFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TJSHTMLElement;
    { Borrow the declared focus face, including the split separator's divider.
      No declared face returns nil; missing identity raises. Bound collections
      return their logical host and manage their own descendant Tab entry.
      Never retain the element beyond this mounted view's lifetime. }
    function FocusFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TJSHTMLElement;
    { Owned runtime text range, independent of document/design selection.
      SetTextSelection changes only the mounted input; no focus/scroll is forced. }
    function TextSelectionFor(const AID: TNyxText): TNyxTextSelection;
    function EditingFor(const AID: TNyxText): TNyxEditingSnapshot;
    procedure SetTextSelection(const AID: TNyxText; const ASelection: TNyxTextSelection);
    { Read current local CSS-pixel offsets without changing state/selection.
      Signed offsets preserve the browser's RTL and overscroll conventions. }
    function ViewportFor(const AID: TNyxText): TNyxViewportSnapshot;
    { Navigate a mounted public code editor to a one-based source line. Focus
      stays inside the editor and never scrolls its containing designer canvas. }
    { One-based Unicode-scalar column; caret storage is translated for UTF-16.
      Focus is retained inside the editor without scrolling the whole document. }
    procedure NavigateCodeLine(const AID: TNyxText; ALine: Integer; AColumn: Integer = 1);
    { Mount a compiler-service browser artifact in an isolated view frame.
      URL is a relative service artifact. Renderer owns the mounted frame;
      a later normal Render replaces it through the same ownership boundary. }
    procedure RenderCompiled(const AArtifact: TNyxText; AHost: TJSHTMLElement);
    { Release the mounted view before disposing/replacing a containing Nyx host.
      The host remains caller-owned; no stale event bindings are retained. }
    procedure Unmount;
    { Attach a typed collection view to an already projected list/table/tree.
      Renderer retains and disconnects the managed attachment on unmount/remount;
      callers may retain its interface safely. A control accepts one live mount.
      This runtime bridge is also usable by custom application/component code. }
    function BindCollection(const AID: TNyxText;
      const AView: INyxCollectionView): INyxCollectionMount;
    { Retain the automatically mounted typed view by exact runtime identity. }
    function CollectionView(const AID: TNyxText): INyxCollectionView;
    { Move a mounted view without recreating controls or bindings. Caller owns
      both hosts; the new host must be empty and outside the old view. Focus and
      scroll restoration belong to the containing layout. Compiled frames may
      move through the same boundary. }
    procedure MoveHost(AHost: TJSHTMLElement);
    procedure Select(const ADesignID: TNyxText);
    { Synchronize runtime properties while retaining DOM identity and focus.
      Compound actions use this path rather than rebuilding the entire view. }
    procedure Sync;
    property OnEvent: TNyxBrowserEvent read FOnEvent write FOnEvent;
    { Managed multiple registrations are application/view scoped. Clear cancels
      queued invocations; renderer destruction closes every registration. }
    property Events: INyxEvents read FEvents;
    property Root: TNyxNode read FRoot;
    property State: TNyxState read FState;
    property OnBindingError: TNyxBrowserBindingError read FOnBindingError write FOnBindingError;
    property LastBindingError: TNyxText read FLastBindingError;
    property LastBindingFailure: TNyxBindingFailure read FLastBindingFailure;
    { A host refusal is observable without changing the accepted model. The sink
      receives owned text and may dispose the view. A valid request clears it. }
    property LastGestureError: TNyxText read FLastGestureError;
    property OnGestureFailure: TNyxGestureFailure read FOnGestureFailure write FOnGestureFailure;
  end;

{ Restore focus without implicit scroll-to-control behavior. This typed bridge
  supplies the standard focus options absent from older pas2js Web declarations.
  The caller retains ownership of the borrowed control. }
procedure NyxFocusWithoutScroll(AControl: TJSHTMLElement);

implementation

type
  TNyxFocusOptions = class external name 'Object' (TJSObject)
    preventScroll: Boolean;
  end;
  TNyxFocusableElement = class external name 'HTMLElement' (TJSHTMLElement)
    procedure focus(AOptions: TNyxFocusOptions); reintroduce;
  end;
  TNyxWheelElement = class external name 'HTMLElement' (TJSHTMLElement)
    procedure Listen(const AName: String; AHandler: TJSMouseWheelEventHandler;
      AOptions: TJSObject); external name 'addEventListener';
  end;
  { Older Web declarations omit removeEventListener's capture argument. The
    standard DOM requires the same capture bit to revoke a key producer. }
  TNyxKeyboardElement = class external name 'HTMLElement' (TJSHTMLElement)
    procedure Unlisten(const AName: String; AHandler: TJSKeyEventHandler;
      ACapture: Boolean); external name 'removeEventListener';
  end;

function CaptureBrowserViewport(AElement: TJSHTMLElement): TNyxViewportSnapshot;
begin
  Result := NyxViewport(
    NyxViewportAxis(AElement.scrollLeft, AElement.scrollWidth,
      AElement.clientWidth, nvuLogicalPixels),
    NyxViewportAxis(AElement.scrollTop, AElement.scrollHeight,
      AElement.clientHeight, nvuLogicalPixels),
    AElement.clientWidth, AElement.clientHeight);
end;

function TNyxBrowserRenderer.ViewportFor(const AID: TNyxText): TNyxViewportSnapshot;
var
  LElement: TJSHTMLElement;
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  LElement := ElementFor(AID);
  for LIndex := 0 to High(FBindings) do
  begin

    if (FBindings[LIndex].FElement = LElement) and (FBindings[LIndex].FInput <> nil) then
    begin
      LElement := FBindings[LIndex].FInput;
      Break;
    end;
  end;
  Result := CaptureBrowserViewport(LElement);
end;

procedure NyxFocusWithoutScroll(AControl: TJSHTMLElement);
var
  LOptions: TNyxFocusOptions;
begin

  if AControl <> nil then
  begin
    LOptions := TNyxFocusOptions.new;
    LOptions.preventScroll := True;
    TNyxFocusableElement(AControl).focus(LOptions);
  end;
end;

var
  GThemeScope: Integer = 0;

procedure TNyxBrowserRenderer.NavigateCodeLine(const AID: TNyxText; ALine: Integer; AColumn: Integer);
var
  LEditor: TJSHTMLTextAreaElement;
  LElement: TJSHTMLElement;
  LIndex: Integer;
  LLineHeight: Double;
begin
  LElement := ElementFor(AID);

  if not (LElement is TJSHTMLTextAreaElement) then
  begin
    LElement := TJSHTMLElement(LElement.querySelector('textarea'));
  end;

  if (LElement = nil) or (ALine < 1) or (AColumn < 1) then
  begin
    raise ENyxModel.Create('Source navigation requires a code editor and source line');
  end;
  LEditor := TJSHTMLTextAreaElement(LElement);
  LIndex := NyxTextPosition(LEditor.value, ALine, AColumn) - 1;
  NyxFocusWithoutScroll(LEditor);
  LEditor.selectionStart := LIndex;
  LEditor.selectionEnd := LIndex;
  LLineHeight := parseFloat(window.getComputedStyle(LEditor).getPropertyValue('line-height'));

  if not (LLineHeight > 0) then
  begin
    LLineHeight := 20;
  end;
  LEditor.scrollTop := Round((ALine - 1) * LLineHeight);
end;

function Element(const ATag, AClass: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.createElement(ATag));
  Result.className := AClass;
end;

function SafeURL(const AValue: TNyxText; AImage: Boolean): Boolean;
var
  LValue: TNyxText;
begin
  LValue := LowerCase(Trim(AValue));
  Result := (LValue = '') or (Pos('https://', LValue) = 1) or
    (Pos('http://', LValue) = 1) or (Pos('./', LValue) = 1) or
    (Pos('../', LValue) = 1) or (Pos('/', LValue) = 1) or (Pos('#', LValue) = 1);

  if AImage then
  begin
    Result := Result or (Pos('data:image/png;base64,', LValue) = 1) or
      (Pos('data:image/jpeg;base64,', LValue) = 1);
  end
  else
  begin
    Result := Result or (Pos('mailto:', LValue) = 1);
  end;
end;

constructor TNyxBrowserRenderer.Create(ATheme: TNyxTheme);
begin
  inherited Create;
  Inc(GThemeScope);
  FThemeScope := 'nyx-theme-' + IntToStr(GThemeScope);
  FTheme := ATheme;
  FBorrowedTheme := ATheme;
  FOwnTheme := ATheme = nil;

  if FOwnTheme then
  begin
    FTheme := TNyxTheme.Create;
  end;
  FTheme.Validate;
  FBaseTheme := NewNyxDocumentTheme(nil, FTheme);
  FEvents := NewNyxEvents;
end;

destructor TNyxBrowserRenderer.Destroy;
begin

  if FEvents <> nil then
  begin
    FEvents.Close;
  end;
  Clear;
  FBaseTheme.Free;

  if FOwnTheme then
  begin
    FTheme.Free;
  end;
  inherited Destroy;
end;

procedure TNyxBrowserRenderer.Clear;
var
  LIndex: Integer;
begin
  { Disconnect before clearing any controls: their destructors and retained
    external producers must never enter a half-disposed view. }

  if FEmitterScope <> nil then
  begin
    FEmitterScope.Disconnect;
    FEmitterScope := nil;
  end;
  FUpdating := True;

  if FEvents <> nil then
  begin
    FEvents.CancelPending;
  end;
  { Disconnect before releasing DOM/model/state borrowed by the coordinator. }
  FLiveBindings.Free;
  FLiveBindings := nil;
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FCollectionMount <> nil then
    begin
      FBindings[LIndex].FCollectionMount.Disconnect;
      FBindings[LIndex].FCollectionMount := nil;
    end;
  end;
  { Release the DOM first: handlers cannot run against nodes during teardown. }

  if FHost <> nil then
  begin
    FHost.textContent := '';
  end;
  for LIndex := 0 to Length(FBindings) - 1 do
  begin
    { Detached nodes may still be held by caller code. Clear callbacks before
      releasing their Pascal bindings so those references cannot dispatch into
      a disposed tree. }
    FBindings[LIndex].FElement.onclick := nil;

    if FBindings[LIndex].FInput <> nil then
    begin
      FBindings[LIndex].FInput.onchange := nil;
      FBindings[LIndex].FInput.removeEventListener('focus', @FBindings[LIndex].Enter);
      FBindings[LIndex].FInput.removeEventListener('blur', @FBindings[LIndex].Leave);
    end;
    FBindings[LIndex].FElement.removeEventListener('focus', @FBindings[LIndex].Enter);
    FBindings[LIndex].FElement.removeEventListener('blur', @FBindings[LIndex].Leave);
    FBindings[LIndex].Free;
  end;
  SetLength(FBindings, 0);
  FCollectionBindings := nil;
  ReleaseNyxNode(FRoot);

  if FOwnState then
  begin
    FState.Free;
  end;
  FState := nil;
  FOwnState := False;
  FUpdating := False;
end;

procedure TNyxBrowserRenderer.RegisterFactory(const AKind: TNyxText;
  AFactory: TNyxBrowserFactory; AUpdater: TNyxBrowserUpdater);
var
  LIndex: Integer;
begin

  if (AKind = '') or not Assigned(AFactory) then
  begin
    raise ENyxModel.Create('Custom browser kind and factory are required');
  end;
  for LIndex := 0 to Length(FFactoryKinds) - 1 do
  begin

    if FFactoryKinds[LIndex] = AKind then
    begin
      FFactories[LIndex] := AFactory;
      FEventFactories[LIndex] := nil;
      FUpdaters[LIndex] := AUpdater;
      Exit;
    end;
  end;
  LIndex := Length(FFactoryKinds);
  SetLength(FFactoryKinds, LIndex + 1);
  SetLength(FFactories, LIndex + 1);
  SetLength(FEventFactories, LIndex + 1);
  SetLength(FUpdaters, LIndex + 1);
  FFactoryKinds[LIndex] := AKind;
  FFactories[LIndex] := AFactory;
  FUpdaters[LIndex] := AUpdater;
end;

procedure TNyxBrowserRenderer.RegisterEventFactory(const AKind: TNyxKindRef;
  AFactory: TNyxBrowserEventFactory; AUpdater: TNyxBrowserUpdater);
var
  LIndex: Integer;
begin

  if (AKind.Name = '') or not Assigned(AFactory) then
  begin
    raise ENyxModel.Create('Custom browser kind and event factory are required');
  end;
  LIndex := 0;
  while (LIndex < Length(FFactoryKinds)) and (FFactoryKinds[LIndex] <> AKind.Name) do
  begin
    Inc(LIndex);
  end;

  if LIndex = Length(FFactoryKinds) then
  begin
    SetLength(FFactoryKinds, LIndex + 1);
    SetLength(FFactories, LIndex + 1);
    SetLength(FEventFactories, LIndex + 1);
    SetLength(FUpdaters, LIndex + 1);
  end;
  FFactoryKinds[LIndex] := AKind.Name;
  FFactories[LIndex] := nil;
  FEventFactories[LIndex] := AFactory;
  FUpdaters[LIndex] := AUpdater;
end;

function TNyxBrowserRenderer.FactoryIndex(ANode: TNyxNode): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  { Exact semantic overrides win. A base factory also customizes derivatives,
    without requiring one registration for every derived recipe name. }
  for LIndex := 0 to Length(FFactoryKinds) - 1 do
  begin

    if FFactoryKinds[LIndex] = ANode.Kind then
    begin
      Exit(LIndex);
    end;
  end;
  for LIndex := 0 to Length(FFactoryKinds) - 1 do
  begin

    if FFactoryKinds[LIndex] = ANode.ProjectionKind then
    begin
      Exit(LIndex);
    end;
  end;
end;

function TNyxBrowserRenderer.Capability(ANode: TNyxNode): TNyxCapability;
var
  LInfo: TNyxPrimitiveInfo;
begin
  Result := ncMissing;

  if ANode = nil then
  begin
    Exit;
  end;

  if FactoryIndex(ANode) >= 0 then
  begin
    Exit(ncCustom);
  end;

  if FindNyxPrimitive(ANode.ProjectionKind, LInfo) then
  begin
    Result := LInfo.Browser;
  end;
end;

function TNyxBrowserRenderer.CreateElement(ANode: TNyxNode;
  out AInput: TJSHTMLElement): TJSHTMLElement;
var
  LKind: TNyxText;
  LInput: TJSHTMLInputElement;
  LCaption: TJSHTMLElement;
  LIndex: Integer;
  LURL: TNyxText;
  LInfo: TNyxPrimitiveInfo;
  LFactoryIndex: Integer;
begin
  AInput := nil;
  LKind := ANode.ProjectionKind;
  ValidateNyxProperties(ANode);
  LFactoryIndex := FactoryIndex(ANode);

  if LFactoryIndex >= 0 then
  begin
    for LIndex := 0 to ANode.BindingCount - 1 do
    begin

      if not ANode.Bindings[LIndex].Cleared and not Assigned(FUpdaters[LFactoryIndex]) then
      begin
        raise ENyxModel.Create('Bound custom browser factory requires an updater: ' + ANode.Kind);
      end;
    end;

    if Assigned(FEventFactories[LFactoryIndex]) then
    begin
      Result := FEventFactories[LFactoryIndex](ANode, FEmitterScope.ForControl(ANode.ID));
    end
    else
    begin
      Result := FFactories[LFactoryIndex](ANode);
    end;

    if (Result = nil) or (Result.parentNode <> nil) then
    begin
      raise ENyxModel.Create('Browser factory must return a new detached element: ' + ANode.Kind);
    end;

    if (Result is TJSHTMLInputElement) or (Result is TJSHTMLTextAreaElement) or
      (Result is TJSHTMLSelectElement) then
    begin
      AInput := Result;
    end;
    Exit;
  end;

  if not FindNyxPrimitive(LKind, LInfo) then
  begin
    raise ENyxModel.Create('No browser projection for ' + ANode.Kind + ' (base ' + LKind +
      '). Derive a primitive recipe or register a browser factory.');
  end;

  if not LInfo.Container and (ANode.Count > 0) then
  begin
    raise ENyxModel.Create('Compose children inside a layout host, not ' + ANode.ID);
  end;

  if LKind = 'code-editor' then
  begin
    Result := Element('textarea', 'nyx-code-editor');
    AInput := Result;
    TJSHTMLTextAreaElement(Result).value := ANode.Prop('value');
    TJSHTMLTextAreaElement(Result).spellcheck := False;
    Result.setAttribute('aria-label', ANode.Prop('text', 'Pascal source'));
  end
  else if (LKind = 'input') or (LKind = 'memo') or (LKind = 'select') or
    (LKind = 'spin') or (LKind = 'date') or (LKind = 'time') or (LKind = 'color') then
  begin
    Result := Element('label', 'nyx-field');
    LCaption := Element('span', 'nyx-field-caption');
    LCaption.textContent := ANode.Prop('text');
    Result.appendChild(LCaption);

    if LKind = 'memo' then
    begin
      AInput := Element('textarea', 'nyx-memo-control');
      TJSHTMLTextAreaElement(AInput).value := ANode.Prop('value');
    end
    else if LKind = 'select' then
    begin
      AInput := Element('select', 'nyx-select-control');
      { SyncLiteralItems supplies the admitted initial rows once. }
    end
    else
    begin
      AInput := Element('input', 'nyx-input-control');
      LInput := TJSHTMLInputElement(AInput);
      LInput._type := 'text';

      if ANode.Prop('input-type') = 'password' then
      begin
        LInput._type := 'password';
      end;

      if ANode.Prop('input-type') = 'number' then
      begin
        LInput._type := 'number';
        LInput.setAttribute('step', 'any');
      end;

      if LKind = 'spin' then
      begin
        LInput._type := 'number';
      end;

      if (LKind = 'date') or (LKind = 'time') or (LKind = 'color') then
      begin
        LInput._type := LKind;
      end;
      LInput.value := ANode.Prop('value');
    end;
    AInput.setAttribute('placeholder', ANode.Prop('placeholder'));
    AInput.setAttribute('aria-label', ANode.Prop('text', ANode.ID));
    Result.appendChild(AInput);
  end
  else if (LKind = 'checkbox') or (LKind = 'switch') or (LKind = 'radio') then
  begin
    Result := Element('label', 'nyx-check');
    AInput := Element('input', '');
    LInput := TJSHTMLInputElement(AInput);
    LInput._type := 'checkbox';

    if LKind = 'radio' then
    begin
      LInput._type := 'radio';
      LInput.name := ANode.Prop('group', 'nyx-radio');
    end;

    if LKind = 'switch' then
    begin
      LInput.setAttribute('role', 'switch');
    end;
    LInput.checked := ANode.Prop('value') = 'true';
    Result.appendChild(AInput);
    LCaption := Element('span', '');
    LCaption.textContent := ANode.Prop('text');
    Result.appendChild(LCaption);
  end
  else if LKind = 'slider' then
  begin
    Result := Element('label', 'nyx-slider');
    AInput := Element('input', '');
    LInput := TJSHTMLInputElement(AInput);
    LInput._type := 'range';
    LInput.value := ANode.Prop('value', '0');
    LInput.setAttribute('aria-label', ANode.Prop('text', ANode.ID));
    Result.appendChild(AInput);
  end
  else if LKind = 'progress' then
  begin
    Result := Element('progress', '');
    TJSHTMLProgressElement(Result).max := StrToFloatDef(ANode.Prop('max'), 100);
    TJSHTMLProgressElement(Result).value := StrToFloatDef(ANode.Prop('value'), 0);
    Result.setAttribute('aria-label', ANode.Prop('text', 'Progress'));
  end
  else if LKind = 'image' then
  begin
    Result := Element('img', '');
    LURL := ANode.Prop('src');

    if not SafeURL(LURL, True) then
    begin
      raise ENyxModel.Create('Unsupported image URL');
    end;
    TJSHTMLImageElement(Result).src := LURL;
    TJSHTMLImageElement(Result).alt := ANode.Prop('alt', ANode.Prop('text'));
  end
  else if LKind = 'list' then
  begin
    Result := Element('ul', '');
  end
  else if LKind = 'table' then
  begin
    Result := Element('table', '');
  end
  else if LKind = 'tree' then
  begin
    { Literal Items are flat leaves; bound views supply actual hierarchy. }
    Result := Element('div', '');
  end
  else
  begin

    if LKind = 'button' then
    begin
      Result := Element('button', '');
      Result.setAttribute('type', 'button');
    end
    else if LKind = 'link' then
    begin
      Result := Element('a', '');
      LURL := ANode.Prop('href', '#');

      if not SafeURL(LURL, False) then
      begin
        raise ENyxModel.Create('Unsupported link URL');
      end;
      Result.setAttribute('href', LURL);
    end
    else if LKind = 'heading' then
    begin
      Result := Element('h2', '');
    end
    else if LKind = 'separator' then
    begin
      Result := Element('hr', '');
    end
    else if LKind = 'code' then
    begin
      Result := Element('pre', '');
    end
    else if LKind = 'group' then
    begin
      Result := Element('fieldset', '');
      LCaption := Element('legend', '');
      LCaption.textContent := ANode.Prop('text');
      Result.appendChild(LCaption);
    end
    else
    begin
      Result := Element('div', '');
    end;

    if (LKind = 'button') or (LKind = 'link') or (LKind = 'heading') or
      (LKind = 'label') or (LKind = 'badge') or (LKind = 'alert') or
      (LKind = 'avatar') or (LKind = 'code') then
    begin
      Result.textContent := ANode.Prop('text');
    end;
  end;
end;

function TNyxBrowserRenderer.Build(ANode: TNyxNode): TJSHTMLElement;
var
  LInput: TJSHTMLElement;
  LBinding: TNyxBrowserBinding;
  LIndex: Integer;
  LValue: TNyxText;
  LFirst: TJSHTMLElement;
  LSecond: TJSHTMLElement;
  LWheelOptions: TJSObject;
  LViewportElement: TJSHTMLElement;
begin
  Result := CreateElement(ANode, LInput);
  Result.className := Result.className + ' nyx-node nyx-' + ANode.Kind;

  if ANode.ProjectionKind <> ANode.Kind then
  begin
    Result.classList.add('nyx-' + ANode.ProjectionKind);
  end;

  if ANode.Prop('surface') = 'true' then
  begin
    Result.classList.add('nyx-card');
  end;
  Result.setAttribute('data-node', ANode.Prop('design-id', ANode.ID));
  Result.setAttribute('data-runtime-id', ANode.ID);
  Result.setAttribute('data-variant', ANode.Prop('variant'));

  if ANode.Prop('aria-label') <> '' then
  begin
    Result.setAttribute('aria-label', ANode.Prop('aria-label'));
  end;
  Result.title := ANode.Prop('hint');
  LValue := ANode.Prop('layout');

  if (LValue = 'row') or (LValue = 'column') then
  begin
    Result.style.setProperty('display', 'flex');
    Result.style.setProperty('flex-direction', LValue);
  end
  else if LValue = 'grid' then
  begin
    Result.style.setProperty('display', 'grid');
  end;

  if ANode.Prop('columns') <> '' then
  begin
    Result.style.setProperty('grid-template-columns', 'repeat(' +
      IntToStr(StrToIntDef(ANode.Prop('columns'), 2)) + ',minmax(0,1fr))');
  end;
  for LIndex := 0 to ANode.Props.Count - 1 do
  begin
    LValue := ANode.Props.Names[LIndex];

    if (LValue = 'width') or (LValue = 'height') or (LValue = 'gap') or
      (LValue = 'padding') or (LValue = 'left') or (LValue = 'top') then
    begin

      if ANode.Prop(LValue) = '' then
      begin
        Continue;
      end;
      Result.style.setProperty(LValue, IntToStr(StrToIntDef(ANode.Prop(LValue), 0)) + 'px');
    end;
  end;

  if ANode.Prop('layout') = 'absolute' then
  begin
    Result.style.setProperty('display', 'block');
  end;

  if (ANode.Parent <> nil) and (ANode.Parent.Prop('layout') = 'absolute') then
  begin
    Result.style.setProperty('position', 'absolute');
  end;

  if ANode.Prop('flex') <> '' then
  begin

    if StrToIntDef(ANode.Prop('flex'), 0) > 0 then
    begin
      Result.style.setProperty('flex', ANode.Prop('flex'));
      Result.classList.add('nyx-flex');
    end
    else
    begin
      { CSS's bare zero also sets a zero basis. The Pascal zero-weight contract
        restores the authored/natural size instead of collapsing that item. }
      Result.style.setProperty('flex', '0 0 auto');
    end;
  end;

  if ANode.Prop('visible', 'true') = 'false' then
  begin
    Result.style.setProperty('display', 'none');
  end;

  if LInput <> nil then
  begin
    LInput.setAttribute('min', ANode.Prop('min', '0'));
    LInput.setAttribute('max', ANode.Prop('max', '100'));

    if ANode.Prop('enabled', 'true') = 'false' then
    begin
      LInput.setAttribute('disabled', '');
    end;

    if ANode.Prop('readonly') = 'true' then
    begin
      LInput.setAttribute('readonly', '');
    end;
  end
  else if (ANode.ProjectionKind = 'button') and (ANode.Prop('enabled', 'true') = 'false') then
  begin
    Result.setAttribute('disabled', '');
  end;
  LBinding := TNyxBrowserBinding.Create;
  LBinding.FRenderer := Self;
  LBinding.FNode := ANode;
  LBinding.FElement := Result;
  LBinding.FInput := LInput;
  LBinding.FCustom := FactoryIndex(ANode) >= 0;

  if LBinding.FCustom then
  begin
    LBinding.FUpdater := FUpdaters[FactoryIndex(ANode)];
  end;

  if not LBinding.FCustom then
  begin
    LBinding.FCaption := TJSHTMLElement(Result.querySelector('.nyx-field-caption'));

    if (ANode.ProjectionKind = 'checkbox') or (ANode.ProjectionKind = 'switch') or
      (ANode.ProjectionKind = 'radio') then
    begin
      LBinding.FCaption := TJSHTMLElement(Result.querySelector('span'));
    end;

    if ANode.ProjectionKind = 'group' then
    begin
      LBinding.FCaption := TJSHTMLElement(Result.querySelector('legend'));
    end;
  end;
  if not LBinding.FCustom and (ANode.ProjectionKind = 'radio') then
  begin
    Result.addEventListener('focus', @LBinding.ForwardRadioFocus);
  end;
  Result.onclick := @LBinding.Click;
  Result.addEventListener('dblclick', @LBinding.DoubleClick);
  Result.addEventListener('pointerdown', @LBinding.PointerDown);
  Result.addEventListener('pointerup', @LBinding.PointerUp);
  Result.addEventListener('pointermove', @LBinding.PointerMove);
  Result.addEventListener('pointerenter', @LBinding.PointerEnter);
  Result.addEventListener('pointerleave', @LBinding.PointerExit);
  Result.addEventListener('pointercancel', @LBinding.PointerCancel);
  Result.addEventListener('gotpointercapture', @LBinding.PointerCapture);
  Result.addEventListener('lostpointercapture', @LBinding.PointerCaptureLost);
  Result.addEventListener('dragstart', @LBinding.DragStart);
  Result.addEventListener('drag', @LBinding.Drag);
  Result.addEventListener('dragenter', @LBinding.DragEnter);
  Result.addEventListener('dragover', @LBinding.DragOver);
  Result.addEventListener('dragleave', @LBinding.DragExit);
  Result.addEventListener('drop', @LBinding.Drop);
  Result.addEventListener('dragend', @LBinding.DragEnd);
  Result.draggable := not FDesignMode and (ANode.Prop('drag-source') = 'true');
  Result.style.setProperty('touch-action', ANode.Prop('touch-behavior', 'auto'));
  Result.addEventListener('contextmenu', @LBinding.ContextMenu);
  { Wheel input must be non-passive when a sequential hook may cancel it.
    Only the outer face listens: input bubbling cannot duplicate a Nyx route. }
  LWheelOptions := TJSObject.new;
  LWheelOptions['passive'] := False;
  TNyxWheelElement(Result).Listen('wheel', @LBinding.Wheel, LWheelOptions);

  if NyxSupportsViewport(ANode) then
  begin
    LViewportElement := Result;

    if LInput <> nil then
    begin
      LViewportElement := LInput;
    end;
    LViewportElement.addEventListener('scroll', @LBinding.Scroll);
    LViewportElement.addEventListener('scrollend', @LBinding.ScrollEnd);

    if (LInput = nil) and (ANode.Prop('height') <> '') then
    begin
      Result.style.setProperty('overflow', 'auto');

      if ANode.ProjectionKind = 'table' then
      begin
        Result.style.setProperty('display', 'block');
      end;
    end;
  end;
  { Standard non-input focus policy is applied in Sync, including disabled
    transitions. Bound collection mounts retain their own roving Tab entry. }
  { Delegated focus observes collection rows/editors without adding a second
    Tab stop. Ordinary leaves still admit only their exact physical face.
    Capture keys before a collection's default row navigation so sequential
    Nyx hooks can consume it. Creator factories keep their existing ordering. }

  if (LInput = nil) and (LBinding.FCustom or
    (ANode.ProjectionKind <> 'split-view')) then
  begin
    Result.addEventListener('focusin', @LBinding.Enter);
    Result.addEventListener('focusout', @LBinding.Leave);
    Result.addEventListener('keydown', @LBinding.KeyDown, not LBinding.FCustom);
    Result.addEventListener('keyup', @LBinding.KeyUp, not LBinding.FCustom);
  end;

  if LInput <> nil then
  begin
    LInput.onchange := @LBinding.Change;
    { Text admission follows physical input on both adapters. This also covers
      paste, deletion, composition and virtual keyboards without keydown. Scalar
      number/choice controls retain their explicit editing-complete boundary. }

    if NyxSupportsTextInput(ANode) or (ANode.ProjectionKind = 'input') then
    begin
      { The input's scalar domain/format can change while it stays mounted.
        Install once; each producer checks the current typed domain before
        admission. Numeric drafts still wait for the physical change boundary. }
      LInput.addEventListener('input', @LBinding.Change);
      LInput.addEventListener('beforeinput', @LBinding.BeforeEdit);
      LInput.addEventListener('compositionstart', @LBinding.CompositionStart);
      LInput.addEventListener('compositionupdate', @LBinding.CompositionUpdate);
      LInput.addEventListener('compositionend', @LBinding.CompositionEnd);
      LInput.addEventListener('selectionchange', @LBinding.TextSelectionChanged);
      LInput.addEventListener('select', @LBinding.TextSelectionChanged);
      LBinding.FEditingSelection := CaptureNyxBrowserSelection(LInput);
    end;
    LInput.addEventListener('focus', @LBinding.Enter);
    LInput.addEventListener('blur', @LBinding.Leave);
    LInput.addEventListener('keydown', @LBinding.KeyDown);
    LInput.addEventListener('keyup', @LBinding.KeyUp);
  end;
  SetLength(FBindings, Length(FBindings) + 1);
  FBindings[Length(FBindings) - 1] := LBinding;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Result.appendChild(Build(ANode.Children[LIndex]));
  end;

  if not LBinding.FCustom and (ANode.ProjectionKind = 'split-view') then
  begin
    LFirst := nil;
    LSecond := nil;

    if ANode.Count > 0 then
    begin
      LFirst := TJSHTMLElement(Result.children[0]);
    end;

    if ANode.Count > 1 then
    begin
      LSecond := TJSHTMLElement(Result.children[1]);
    end;
    LBinding.FSplit := TNyxBrowserSplit.Create(ANode, Result, LFirst, LSecond, FDesignMode);
    LBinding.FSplit.OnChanged := @LBinding.SplitChanged;
    LBinding.FSplit.Divider.addEventListener('focus', @LBinding.Enter);
    LBinding.FSplit.Divider.addEventListener('blur', @LBinding.Leave);
    LBinding.FSplit.Divider.addEventListener('keydown', @LBinding.KeyDown, True);
    LBinding.FSplit.Divider.addEventListener('keyup', @LBinding.KeyUp, True);
  end;
end;

procedure TNyxBrowserRenderer.Render(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TJSHTMLElement; ADesignMode: Boolean; AState: TNyxState;
  const ACollections: INyxCollectionBindings);
var
  LCandidate: TNyxBrowserRenderer;
  LStyle: TJSHTMLElement;
  LIndex: Integer;
  LTransferState: Boolean;
begin
  { Build the entire candidate offscreen. Custom factories and child mounting
    may fail after realization, so accepting only the model is insufficient.
    Transfer target/model ownership only after every factory has succeeded. }

  if AHost = nil then
  begin
    raise ENyxModel.Create('Browser host is required');
  end;
  LCandidate := TNyxBrowserRenderer.Create(FTheme);
  LTransferState := (AState <> nil) and FOwnState and (AState = FState);
  try

    if FBorrowedTheme <> nil then
    begin
      LCandidate.FTheme := NewNyxDocumentTheme(ADocument, FBorrowedTheme);
    end
    else
    begin
      LCandidate.FTheme := NewNyxDocumentTheme(ADocument, FBaseTheme);
    end;
    LCandidate.FOwnTheme := True;
    LCandidate.FFactoryKinds := Copy(FFactoryKinds, 0, Length(FFactoryKinds));
    LCandidate.FFactories := Copy(FFactories, 0, Length(FFactories));
    LCandidate.FEventFactories := Copy(FEventFactories, 0, Length(FEventFactories));
    { Use the final owner's scheduler, never the candidate's temporary router. }
    LCandidate.FEmitterScope := NewNyxEventEmitterScope(FEvents.Scheduler);
    LCandidate.FUpdaters := Copy(FUpdaters, 0, Length(FUpdaters));
    LCandidate.FRoot := RealizeNyxView(ADocument, ARoot);
    ApplyNyxPlatform(LCandidate.FRoot, npfBrowser);
    LCandidate.FCollectionBindings := ACollections;

    if ACollections <> nil then
    begin
      ACollections.ValidateRoot(LCandidate.FRoot);
    end
    else
    begin
      LCandidate.FCollectionBindings := NewNyxCollectionBindings(LCandidate.FRoot,
        NewNyxCollectionContext(ADocument.Collections));
    end;
    LCandidate.FState := AState;
    LCandidate.FOwnState := AState = nil;

    if LCandidate.FOwnState then
    begin
      LCandidate.FState := ADocument.State.Clone;
    end;
    LCandidate.FLiveBindings := TNyxLiveBindings.Create(LCandidate.FRoot, LCandidate.FState);
    LCandidate.FLiveBindings.OnSync := @LCandidate.Sync;
    LCandidate.FHost := Element('div', '');
    LCandidate.FDesignMode := ADesignMode;
    LStyle := Element('style', '');
    LStyle.textContent := LCandidate.FTheme.CSS('.nyx-root[data-nyx-theme="' + FThemeScope + '"]');
    LCandidate.FHost.appendChild(LStyle);
    LCandidate.FHost.appendChild(LCandidate.Build(LCandidate.FRoot));
    for LIndex := 0 to LCandidate.FCollectionBindings.Count - 1 do
    begin
      LCandidate.BindCollection(LCandidate.FCollectionBindings.ID(LIndex),
        LCandidate.FCollectionBindings.View(LIndex));
    end;
    LCandidate.Sync;

    if not ADesignMode then
    begin
      LCandidate.FLiveBindings.Activate;
    end;
    { Passing this renderer's own State preserves that ownership across a full
      remount. Until admission the candidate only borrows it, so failure cannot
      release the still-mounted store. Clear must not free it during transfer. }

    if LTransferState then
    begin
      FOwnState := False;
    end;
    Clear;
    { Transfer the independently admitted effective palette with its controls.
      Failure before this point preserves the mounted palette and tree. }

    if FOwnTheme then
    begin
      FTheme.Free;
    end;
    FTheme := LCandidate.FTheme;
    FOwnTheme := True;
    LCandidate.FTheme := nil;
    LCandidate.FOwnTheme := False;
    FState := LCandidate.FState;
    FOwnState := LCandidate.FOwnState or LTransferState;
    LCandidate.FState := nil;
    LCandidate.FOwnState := False;
    FLiveBindings := LCandidate.FLiveBindings;
    LCandidate.FLiveBindings := nil;
    FLiveBindings.OnSync := @Sync;
    FCollectionBindings := LCandidate.FCollectionBindings;
    LCandidate.FCollectionBindings := nil;
    FLastBindingError := '';
    FLastBindingFailure := nbfNone;
    FRoot := LCandidate.FRoot;
    LCandidate.FRoot := nil;
    FBindings := LCandidate.FBindings;
    FEmitterScope := LCandidate.FEmitterScope;
    LCandidate.FEmitterScope := nil;
    { pas2js arrays share JavaScript identity after assignment. Detach the
      candidate reference; resizing it would also erase the accepted bindings. }
    LCandidate.FBindings := nil;
    FHost := AHost;
    FDesignMode := ADesignMode;
    FHost.classList.add('nyx-root');
    FHost.setAttribute('data-nyx-theme', FThemeScope);

    if ADesignMode then
    begin
      FHost.classList.add('nyx-design');
    end
    else
    begin
      FHost.classList.remove('nyx-design');
    end;
    while LCandidate.FHost.firstChild <> nil do
    begin
      FHost.appendChild(LCandidate.FHost.firstChild);
    end;
    for LIndex := 0 to Length(FBindings) - 1 do
    begin
      FBindings[LIndex].FRenderer := Self;
      FBindings[LIndex].FViewRevision := FEvents.ViewRevision;
    end;
    FEmitterScope.Activate(@EmitNamed);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxBrowserRenderer.Unmount;
begin
  Clear;
  FHost := nil;
end;

function TNyxBrowserRenderer.CollectionView(const AID: TNyxText): INyxCollectionView;
begin

  if FCollectionBindings = nil then
  begin
    raise ENyxModel.Create('No authored collection views are mounted');
  end;
  Result := FCollectionBindings.ViewFor(AID);
end;

function TNyxBrowserRenderer.BindCollection(const AID: TNyxText;
  const AView: INyxCollectionView): INyxCollectionMount;
var
  LElement: TJSHTMLElement;
  LIndex: Integer;
  LKind: TNyxText;
begin

  if AView = nil then
  begin
    raise ENyxModel.Create('Collection binding requires a live view');
  end;
  LKind := 'list';

  if AView.Projection = cpTable then
  begin
    LKind := 'table';
  end
  else if AView.Projection = cpTree then
  begin
    LKind := 'tree';
  end;
  LElement := ElementFor(AID);
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FElement = LElement then
    begin

      if FBindings[LIndex].FNode.ProjectionKind <> LKind then
      begin
        raise ENyxModel.Create('Collection view differs from the mounted control kind');
      end;

      if (FBindings[LIndex].FCollectionMount <> nil) and
        FBindings[LIndex].FCollectionMount.Connected then
      begin
        raise ENyxModel.Create('Control already has a live collection binding');
      end;
      Result := MountNyxBrowserCollection(LElement, AView);
      FBindings[LIndex].FCollectionMount := Result;
      Result.ObserveSelection(FBindings[LIndex].CollectionSelectionChanged);
      Exit;
    end;
  end;
  raise ENyxModel.Create('Collection control is not mounted: ' + AID);
end;

procedure TNyxBrowserRenderer.MoveHost(AHost: TJSHTMLElement);
begin

  if (AHost = nil) or (FHost = nil) then
  begin
    raise ENyxModel.Create('Mounted view and replacement browser host are required');
  end;

  if AHost = FHost then
  begin
    Exit;
  end;

  if FHost.contains(AHost) or (AHost.firstChild <> nil) then
  begin
    raise ENyxModel.Create('Replacement browser host must be empty and outside the view');
  end;
  AHost.classList.add('nyx-root');
  AHost.setAttribute('data-nyx-theme', FThemeScope);

  if FDesignMode then
  begin
    AHost.classList.add('nyx-design');
  end;
  while FHost.firstChild <> nil do
  begin
    AHost.appendChild(FHost.firstChild);
  end;
  FHost.classList.remove('nyx-root');
  FHost.classList.remove('nyx-design');
  FHost.removeAttribute('data-nyx-theme');
  FHost := AHost;
end;

function TNyxBrowserRenderer.InputFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TJSHTMLElement;
var
  LElement: TJSHTMLElement;
  LIndex: Integer;
begin
  LElement := ElementFor(AID, AIdentity);
  Result := nil;
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FElement = LElement then
    begin
      Exit(FBindings[LIndex].FInput);
    end;
  end;
end;

function TNyxBrowserRenderer.FocusFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TJSHTMLElement;
var
  LElement: TJSHTMLElement;
  LBinding: TNyxBrowserBinding;
  LIndex: Integer;
begin
  LElement := ElementFor(AID, AIdentity);
  Result := nil;
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];

    if (LBinding.FElement = LElement) and ((LBinding.FInput <> nil) or
      NyxSupportsKeyboard(LBinding.FNode) or
      (LBinding.FCustom and (LElement.tabIndex >= 0))) then
    begin
      Exit(LBinding.FocusElement);
    end;
  end;
end;

function TNyxBrowserRenderer.ElementFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TJSHTMLElement;
var
  LIndex: Integer;
  LPass: Integer;
begin
  { Automatic convenience has global precedence: a later exact runtime key must
    win over an earlier editable-owner match. Explicit modes avoid ambiguity when
    an authored slash name happens to equal another node's qualified runtime key. }
  for LPass := 0 to 1 do
  begin
    for LIndex := 0 to Length(FBindings) - 1 do
    begin

      if (LPass = 0) and (AIdentity <> niDesign) and
        (FBindings[LIndex].FNode.ID = AID) then
      begin
        Exit(FBindings[LIndex].FElement);
      end;

      if (LPass = 1) and (AIdentity <> niRuntime) and
        (FBindings[LIndex].FNode.DesignID = AID) then
      begin
        Exit(FBindings[LIndex].FElement);
      end;
    end;
  end;
  raise ENyxModel.Create('Browser component is not mounted: ' + AID);
end;

procedure TNyxBrowserRenderer.RenderCompiled(const AArtifact: TNyxText;
  AHost: TJSHTMLElement);
var
  LFrame: TJSHTMLElement;
begin

  if (AHost = nil) or (Pos('builds/job-', AArtifact) <> 1) or
    (Pos('..', AArtifact) > 0) or (Pos(':', AArtifact) > 0) or
    (Pos('\', AArtifact) > 0) then
  begin
    raise ENyxModel.Create('Compiled preview needs a local build artifact and host');
  end;
  LFrame := Element('iframe', 'nyx-compiled-preview');
  LFrame.setAttribute('src', AArtifact);
  LFrame.setAttribute('title', 'Compiled Nyx view');
  LFrame.style.setProperty('width', '100%');
  LFrame.style.setProperty('height', '620px');
  LFrame.style.setProperty('border', '0');
  Clear;
  FHost := AHost;
  FHost.textContent := '';
  FHost.appendChild(LFrame);
end;

procedure TNyxBrowserRenderer.Select(const ADesignID: TNyxText);
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FNode.DesignID = ADesignID then
    begin
      FBindings[LIndex].FElement.classList.add('nyx-selected');
    end
    else
    begin
      FBindings[LIndex].FElement.classList.remove('nyx-selected');
    end;
  end;
end;

procedure TNyxBrowserBinding.SplitChanged(ASplit: TNyxBrowserSplit);
begin
  FRenderer.Emit(FNode, NyxSplitChange(FNode, ASplit.State.Position));
end;

function TNyxBrowserBinding.Click(AEvent: TJSMouseEvent): Boolean;
var
  LDispatch: TNyxDispatch;
begin
  Result := True;

  if FRenderer.FUpdating then
  begin
    Exit;
  end;

  if FRenderer.FDesignMode then
  begin
    AEvent.stopPropagation;
    { Selecting an editable control preserves native focus and interaction.
      Commands and links still select without executing application behavior. }

    if FInput = nil then
    begin
      AEvent.preventDefault;
    end;

    if Assigned(FRenderer.OnEvent) then
    begin
      FRenderer.OnEvent(FNode, NyxDesignEvent(FNode, ntDesignSelect));
    end;
    Exit(FInput <> nil);
  end;

  { Runtime clicks route once from their nearest projected control, matching
    native non-bubbling slots. Wrapped inputs retain their native default action. }
  AEvent.stopPropagation;

  if NyxHasLegacyClick(FNode) or FRenderer.FEvents.HasSubscribers(ntClick) or
    (FNode.Prop(NyxAttributeName(atAction)) <> '') then
  begin
    try
      LDispatch := FRenderer.FLiveBindings.Dispatch(FNode, ntClick);
      FRenderer.FLastBindingError := '';
      FRenderer.FLastBindingFailure := nbfNone;
    except
      on LException: ENyxStateNotification do
      begin
        FRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
        Exit(False);
      end;
      on LException: Exception do
      begin
        FRenderer.BindingFailed(FNode, LException.Message);
        Exit(False);
      end;
    end;

    FRenderer.Emit(FNode, LDispatch);
  end;
end;

function TNyxBrowserRenderer.TextSelectionFor(const AID: TNyxText): TNyxTextSelection;
begin
  FEvents.Scheduler.RequireUI;
  Result := CaptureNyxBrowserSelection(InputFor(AID));
end;

function TNyxBrowserRenderer.EditingFor(const AID: TNyxText): TNyxEditingSnapshot;
var
  LInput: TJSHTMLElement;
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  LInput := InputFor(AID);
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FInput = LInput then
    begin
      Exit(CaptureNyxBrowserEditing(LInput, nepObservation, nil,
        FBindings[LIndex].FComposing));
    end;
  end;
  raise ENyxModel.Create('The mounted component has no text editing context');
end;

procedure TNyxBrowserRenderer.SetTextSelection(const AID: TNyxText;
  const ASelection: TNyxTextSelection);
begin
  FEvents.Scheduler.RequireUI;
  SelectNyxBrowserText(InputFor(AID), ASelection);
end;

function TNyxBrowserBinding.EditingEvent(AEvent: TEventListenerEvent;
  ATrigger: TNyxTrigger; APhase: TNyxEditingPhase): Boolean;
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
begin
  Result := True;
  LEvents := FRenderer.FEvents;
  LRevision := FViewRevision;

  if (LEvents.ViewRevision <> LRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode or not NyxSupportsTextInput(FNode) or
    not NyxInteractionPolicy(FNode).CanIssueCommand then
  begin
    Exit;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ATrigger);
  LDispatch.Info.HasEditing := True;
  LDispatch.Info.Editing := CaptureNyxBrowserEditing(FInput, APhase, AEvent, FComposing);
  { The router lease is all that is read after Emit: a callback may unmount,
    navigate or dispose both renderer and this borrowed producer. }
  FRenderer.Emit(FNode, LDispatch);
  Result := LEvents.ViewRevision = LRevision;
end;

function TNyxBrowserBinding.BeforeEdit(AEvent: TEventListenerEvent): Boolean;
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
  LConsumed: Boolean;
begin
  Result := True;
  LEvents := FRenderer.FEvents;
  LRevision := FViewRevision;

  if (LEvents.ViewRevision <> LRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode or not NyxSupportsTextInput(FNode) or
    not NyxInteractionPolicy(FNode).CanEditValue or
    AEvent.defaultPrevented or not LEvents.HasSubscribers(ntBeforeEdit) then
  begin
    Exit;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ntBeforeEdit);
  LDispatch.Info.HasEditing := True;
  LDispatch.Info.Editing := CaptureNyxBrowserEditing(FInput, nepBeforeEdit, AEvent, FComposing);
  LConsumed := DispatchNyxInput(LEvents, FNode, LDispatch);

  if LConsumed and LDispatch.Info.Editing.CanCancel then
  begin
    AEvent.preventDefault;
    Result := False;
  end;
  { A navigation invalidates this producer even when no physical cancellation
    window exists. Never inspect the freed binding after synchronous dispatch. }
  Result := Result and (LEvents.ViewRevision = LRevision);
end;

function TNyxBrowserBinding.CompositionStart(AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;

  if FRenderer.FUpdating or FRenderer.FDesignMode or
    not NyxSupportsTextInput(FNode) or not NyxInteractionPolicy(FNode).CanEditValue then
  begin
    Exit;
  end;
  FComposing := True;
  Result := EditingEvent(AEvent, ntCompositionStart, nepCompositionStart);
end;

function TNyxBrowserBinding.CompositionUpdate(AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;

  if not FComposing then
  begin
    Exit;
  end;
  Result := EditingEvent(AEvent, ntCompositionUpdate, nepCompositionUpdate);
end;

function TNyxBrowserBinding.CompositionEnd(AEvent: TEventListenerEvent): Boolean;
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LEditing: TNyxEditingSnapshot;
  LDispatch: TNyxDispatch;
begin
  Result := True;

  if not FComposing or FRenderer.FUpdating or FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := FViewRevision;
  LEditing := CaptureNyxBrowserEditing(FInput, nepCompositionEnd, AEvent, False);
  FComposing := False;
  { Capture the physical result first. Final model admission may deliberately
    reject it or a callback may update accepted state; neither changes the
    original owned IME result. Admission emits at most one accepted command. }
  Change(AEvent);

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit(False);
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ntCompositionEnd);
  LDispatch.Info.HasEditing := True;
  LDispatch.Info.Editing := LEditing;
  FRenderer.Emit(FNode, LDispatch);
  Result := LEvents.ViewRevision = LRevision;
end;

function TNyxBrowserBinding.TextSelectionChanged(AEvent: TEventListenerEvent): Boolean;
var
  LSelection: TNyxTextSelection;
begin
  Result := True;

  if FRenderer.FUpdating or FRenderer.FDesignMode or not NyxSupportsTextInput(FNode) then
  begin
    Exit;
  end;
  LSelection := CaptureNyxBrowserSelection(FInput);

  if not LSelection.Defined or FEditingSelection.SameRange(LSelection) then
  begin
    Exit;
  end;
  FEditingSelection := LSelection;
  Result := EditingEvent(AEvent, ntTextSelectionChange, nepSelectionChange);
end;

function TNyxBrowserBinding.Change(AEvent: TEventListenerEvent): Boolean;
var
  LValue: TNyxText;
  LDispatch: TNyxDispatch;
  LProposal: TNyxDispatch;
  LEvents: INyxEvents;
  LRevision: Integer;
  LConsumed: Boolean;
  LEditing: TNyxEditingSnapshot;
begin
  Result := True;

  if FRenderer.FUpdating then
  begin
    Exit;
  end;

  if (AEvent <> nil) and (AEvent._type = 'input') and
    not NyxSupportsTextInput(FNode) then
  begin
    { A format/domain transition must not turn an existing input listener into
      per-keystroke numeric admission. Physical change retains commit semantics. }
    Exit;
  end;

  LEditing := Default(TNyxEditingSnapshot);

  if NyxSupportsTextInput(FNode) then
  begin
    LEditing := CaptureNyxBrowserEditing(FInput, nepInput, AEvent, FComposing);

    if LEditing.Composing then
    begin
      { IME owns its live draft. Model admission and normalization wait until
        compositionend; neither an input listener nor Sync rewrites that draft. }
      FComposing := True;
      Exit;
    end;
  end;

  if FInput is TJSHTMLTextAreaElement then
  begin
    LValue := TJSHTMLTextAreaElement(FInput).value;
  end
  else if FInput is TJSHTMLSelectElement then
  begin
    LValue := TJSHTMLSelectElement(FInput).value;
  end
  else if (FNode.ProjectionKind = 'checkbox') or (FNode.ProjectionKind = 'switch') or
    (FNode.ProjectionKind = 'radio') then
  begin
    LValue := 'false';

    if TJSHTMLInputElement(FInput).checked then
    begin
      LValue := 'true';
    end;
  end
  else
  begin
    LValue := TJSHTMLInputElement(FInput).value;
  end;

  if not NyxInteractionPolicy(FNode).CanEditValue then
  begin
    { A platform without a read-only selector can still emit a physical draft.
      Restore it before proposal callbacks or model commands. Keep True so
      keyboard preflight can still deliver shortcuts against accepted text. }

    if LValue <> FNode.Prop('value') then
    begin
      FRenderer.FForceValues := True;
      try
        FRenderer.Sync;
      finally
        FRenderer.FForceValues := False;
      end;
    end;
    Exit;
  end;

  if FRenderer.FDesignMode then
  begin
    FNode.SetProp('value', LValue);
    { Send the actual field, rather than a compound's semantic event root. The
      host persists an authored value or independent reusable-part override. }

    if Assigned(FRenderer.OnEvent) then
    begin
      FRenderer.OnEvent(FNode, NyxDesignEvent(FNode, ntDesignValue));
    end;
    Exit;
  end;
  { A platform change event with the already accepted value carries no edit. }

  if LValue = FNode.Prop('value') then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;

  if NyxSupportsTextInput(FNode) and LEvents.HasSubscribers(ntBeforeTextInput) then
  begin
    LProposal := FRenderer.FLiveBindings.ProposeText(FNode, LValue);
    LProposal.Info.HasEditing := LEditing.Defined;
    LProposal.Info.Editing := LEditing;
    LConsumed := DispatchNyxInput(LEvents, FNode, LProposal);

    if LEvents.ViewRevision <> LRevision then
    begin
      Exit(False);
    end;

    if LConsumed then
    begin
      { The physical edit is a proposal. Restore the latest accepted value in
        place, without a new history/state command or a spurious error dialog. }
      FRenderer.FForceValues := True;
      try
        FRenderer.Sync;
      finally
        FRenderer.FForceValues := False;
      end;
      LProposal.Info.DefaultPrevented := True;
      DispatchNyxTextResult(LEvents, FNode, LProposal,
        FRenderer.FLiveBindings.SignalSnapshot);
      Exit(False);
    end;
  end;
  try
    LDispatch := FRenderer.FLiveBindings.Edit(FNode, LValue);
    LDispatch.Info.HasEditing := LEditing.Defined;
    LDispatch.Info.Editing := LEditing;
    FRenderer.FLastBindingError := '';
    FRenderer.FLastBindingFailure := nbfNone;
  except
    on LException: ENyxStateNotification do
    begin
      FRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
      Exit(False);
    end;
    on LException: Exception do
    begin
      FRenderer.BindingFailed(FNode, LException.Message);
      Exit(False);
    end;
  end;

  FRenderer.Emit(FNode, LDispatch);
end;

function TNyxBrowserRenderer.EmitNamed(const AOriginID: TNyxText;
  const AName: TNyxEventRef; const APayload: TNyxDataValue;
  AHasPayload: Boolean): Boolean;
var
  LIndex: Integer;
  LDispatch: TNyxDispatch;
begin
  Result := False;

  if FDesignMode then
  begin
    Exit;
  end;
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FNode.ID = AOriginID then
    begin
      LDispatch := DispatchNyxNamedEvent(FBindings[LIndex].FNode, AName,
        APayload, AHasPayload, npfBrowser);
      Result := LDispatch.EventName <> '';

      if Result then
      begin
        Emit(FBindings[LIndex].FNode, LDispatch);
      end;
      { A callback may dispose this renderer. Do not read fields after Emit. }
      Exit;
    end;
  end;
  raise ENyxModel.Create('Named event origin is not mounted: ' + AOriginID);
end;

procedure TNyxBrowserBinding.CollectionSelectionChanged(
  const ABefore, AAfter: INyxCollectionSelection);
var
  LDispatch: TNyxDispatch;
  LEvents: INyxEvents;
begin

  if FRenderer.FUpdating or FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;

  if LEvents.ViewRevision <> FViewRevision then
  begin
    Exit;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ntSelectionChange);
  LDispatch.Info.HasCollectionSelection := True;
  LDispatch.Info.SelectionBefore := ABefore.Snapshot;
  LDispatch.Info.Selection := AAfter.Snapshot;
  { Last borrowed call. Managed snapshots survive navigation/destruction. }
  FRenderer.Emit(FNode, LDispatch);
end;

procedure TNyxBrowserRenderer.Emit(AOrigin: TNyxNode; const ADispatch: TNyxDispatch);
var
  LEvents: INyxEvents;
  LLegacy: TNyxBrowserEvent;
  LRevision: Integer;
  LLegacyClick: Boolean;
begin

  if ADispatch.EventName = '' then
  begin
    Exit;
  end;
  LEvents := FEvents;
  LLegacy := FOnEvent;
  LLegacyClick := NyxHasLegacyClick(AOrigin);
  LRevision := LEvents.ViewRevision;
  AOrigin.AcquireReference;
  ADispatch.Source.AcquireReference;
  try
    { Capture dependencies before callbacks: navigation may dispose this view.
      Managed callbacks own only snapshots. The legacy method still borrows its
      receiver, which applications keep alive for the synchronous callback. }
    LEvents.Dispatch(ADispatch.Info, AOrigin.DesignID, ADispatch.Source.DesignID);

    if ADispatch.Info.HasTextEdit and (LEvents.ViewRevision = LRevision) then
    begin
      DispatchNyxTextResult(LEvents, AOrigin, ADispatch, FLiveBindings.SignalSnapshot);
    end;

    if Assigned(LLegacy) and (ADispatch.Info.Trigger in [ntClick, ntChange, ntNamed]) and
      ((ADispatch.Info.Trigger <> ntClick) or LLegacyClick) and
      (LEvents.ViewRevision = LRevision) then
    begin
      LLegacy(ADispatch.Source, ADispatch.Info);
    end;
  finally
    ADispatch.Source.ReleaseReference;
    AOrigin.ReleaseReference;
  end;
end;

function TNyxBrowserBinding.DoubleClick(AEvent: TJSMouseEvent): Boolean;
begin
  Result := PointerEvent(AEvent, ntDoubleClick);
end;

function TNyxBrowserBinding.PointerDown(AEvent: TJSPointerEvent): Boolean;
begin
  Result := PointerEvent(AEvent, ntPointerDown);
end;

function TNyxBrowserBinding.PointerUp(AEvent: TJSPointerEvent): Boolean;
begin
  Result := PointerEvent(AEvent, ntPointerUp);
end;

function TNyxBrowserBinding.PointerMove(AEvent: TJSPointerEvent): Boolean;
begin
  Result := PointerEvent(AEvent, ntPointerMove);
end;

function TNyxBrowserBinding.PointerEnter(AEvent: TJSPointerEvent): Boolean;
begin
  Result := PointerEvent(AEvent, ntPointerEnter);
end;

function TNyxBrowserBinding.PointerExit(AEvent: TJSPointerEvent): Boolean;
begin
  Result := PointerEvent(AEvent, ntPointerExit);
end;

function TNyxBrowserBinding.ContextMenu(AEvent: TJSMouseEvent): Boolean;
begin
  Result := PointerEvent(AEvent, ntContextMenu);
end;

procedure TNyxBrowserBinding.ObserveCapture(APointerID: Integer; ACaptured: Boolean);
var
  LIndex: Integer;
  LOther: Integer;
begin
  for LIndex := 0 to High(FCapturedPointers) do
  begin

    if FCapturedPointers[LIndex] = APointerID then
    begin

      if not ACaptured then
      begin
        for LOther := LIndex to High(FCapturedPointers) - 1 do
        begin
          FCapturedPointers[LOther] := FCapturedPointers[LOther + 1];
        end;
        SetLength(FCapturedPointers, Length(FCapturedPointers) - 1);
      end;
      Exit;
    end;
  end;

  if ACaptured then
  begin
    SetLength(FCapturedPointers, Length(FCapturedPointers) + 1);
    FCapturedPointers[High(FCapturedPointers)] := APointerID;
  end;
end;

function TNyxBrowserBinding.PointerCancel(AEvent: TJSPointerEvent): Boolean;
begin
  Result := PointerEvent(AEvent, ntPointerCancel);
end;

function TNyxBrowserBinding.PointerCapture(AEvent: TJSPointerEvent): Boolean;
begin
  ObserveCapture(AEvent.pointerId, True);
  Result := PointerEvent(AEvent, ntPointerCapture);
end;

function TNyxBrowserBinding.PointerCaptureLost(AEvent: TJSPointerEvent): Boolean;
begin
  ObserveCapture(AEvent.pointerId, False);
  Result := PointerEvent(AEvent, ntPointerCaptureLost);
end;

function TNyxBrowserBinding.PointerEvent(AEvent: TJSMouseEvent;
  ATrigger: TNyxTrigger): Boolean;
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
  LBounds: TJSDOMRect;
  LPointer: TNyxPointerSnapshot;
  LConsumed: Boolean;
  LTarget: TJSHTMLElement;
  LGesture: INyxGestureDecision;
  LResponse: TNyxGestureResult;
  LCapabilities: TNyxGestureCapabilities;
  LElement: TNyxGestureElement;
  LInput: TNyxGestureElement;
  LRenderer: TNyxBrowserRenderer;
  LOriginID: TNyxText;
begin
  Result := True;
  LEvents := FRenderer.FEvents;
  LRevision := FViewRevision;

  if (LEvents.ViewRevision <> LRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode or AEvent.defaultPrevented or
    not LEvents.HasSubscribers(ATrigger) or
    not NyxInteractionPolicy(FNode).CanIssueCommand then
  begin
    Exit;
  end;
  { A framed input belongs to its outer Nyx face. A separately projected child
    belongs to that child's binding; bubbling must not duplicate its event. }
  LTarget := TJSHTMLElement(AEvent.target);

  if not (ATrigger in [ntPointerEnter, ntPointerExit]) and
    (LTarget.closest('[data-runtime-id]') <> FElement) then
  begin
    Exit;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ATrigger);
  LPointer := LDispatch.Info.Pointer;
  LPointer.Kind := npiMouse;

  if AEvent is TJSPointerEvent then
  begin
    LPointer.ID := TJSPointerEvent(AEvent).pointerId;
    LPointer.Primary := TJSPointerEvent(AEvent).isPrimary;
    LPointer.Pressure := TJSPointerEvent(AEvent).pressure;

    if TJSPointerEvent(AEvent).pointerType = 'touch' then
    begin
      LPointer.Kind := npiTouch;
    end
    else if TJSPointerEvent(AEvent).pointerType = 'pen' then
    begin
      LPointer.Kind := npiPen;
    end;
  end;
  LPointer.HasPosition := not ((ATrigger = ntContextMenu) and
    (AEvent.detail = 0) and (AEvent.clientX = 0) and (AEvent.clientY = 0));
  LBounds := FElement.getBoundingClientRect;
  LPointer.X := AEvent.clientX - LBounds.left;
  LPointer.Y := AEvent.clientY - LBounds.top;

  if not LPointer.HasPosition then
  begin
    LPointer.Kind := npiUnknown;
    LPointer.X := 0;
    LPointer.Y := 0;
  end;
  LPointer.Button := npbNone;

  if ATrigger in [ntPointerDown, ntPointerUp] then
  begin
    case AEvent.button of
      0:
        begin
          LPointer.Button := npbPrimary;
        end;
      1:
        begin
          LPointer.Button := npbAuxiliary;
        end;
      2:
        begin
          LPointer.Button := npbSecondary;
        end;
    else
      LPointer.Button := npbOther;
    end;
  end;
  LPointer.Buttons := [];

  if (AEvent.buttons and 1) <> 0 then
  begin
    Include(LPointer.Buttons, npbPrimary);
  end;

  if (AEvent.buttons and 2) <> 0 then
  begin
    Include(LPointer.Buttons, npbSecondary);
  end;

  if (AEvent.buttons and 4) <> 0 then
  begin
    Include(LPointer.Buttons, npbAuxiliary);
  end;

  if (AEvent.buttons and not 7) <> 0 then
  begin
    Include(LPointer.Buttons, npbOther);
  end;
  LPointer.Modifiers := [];

  if AEvent.shiftKey then
  begin
    Include(LPointer.Modifiers, nmShift);
  end;

  if AEvent.ctrlKey then
  begin
    Include(LPointer.Modifiers, nmControl);
  end;

  if AEvent.altKey then
  begin
    Include(LPointer.Modifiers, nmAlt);
  end;

  if AEvent.metaKey then
  begin
    Include(LPointer.Modifiers, nmMeta);
  end;
  LDispatch.Info.HasPointer := True;
  LDispatch.Info.Pointer := LPointer;
  LConsumed := False;
  LElement := TNyxGestureElement(FElement);
  LInput := TNyxGestureElement(FInput);
  LRenderer := FRenderer;
  LOriginID := FNode.ID;

  if ATrigger = ntContextMenu then
  begin
    LConsumed := DispatchNyxInput(LEvents, FNode, LDispatch);
  end
  else if ATrigger in [ntPointerDown, ntPointerMove, ntPointerUp] then
  begin
    LCapabilities := [];

    if (AEvent is TJSPointerEvent) and (AEvent.buttons <> 0) and
      (ATrigger in [ntPointerDown, ntPointerMove]) then
    begin
      Include(LCapabilities, ngcCapturePointer);
    end;

    if (AEvent is TJSPointerEvent) and
      (LElement.hasPointerCapture(LPointer.ID) or
        ((LInput <> nil) and LInput.hasPointerCapture(LPointer.ID))) then
    begin
      Include(LCapabilities, ngcReleasePointer);
    end;
    LGesture := NewNyxGestureDecision(LCapabilities);
    DispatchNyxGesture(LEvents, FNode, LDispatch, LGesture);
    LResponse := LGesture.Seal;

    if (LEvents.ViewRevision = LRevision) and (LResponse.PointerRequest <> nprUnchanged) then
    begin
      try

        if LResponse.PointerRequest = nprCapture then
        begin
          LElement.setPointerCapture(LPointer.ID);
          ObserveCapture(LPointer.ID, True);
        end
        else
        begin

          if LElement.hasPointerCapture(LPointer.ID) then
          begin
            LElement.releasePointerCapture(LPointer.ID);
          end;

          if (LInput <> nil) and LInput.hasPointerCapture(LPointer.ID) then
          begin
            LInput.releasePointerCapture(LPointer.ID);
          end;
        end;
        LRenderer.FLastGestureError := '';
      except
        LRenderer.GestureFailed(LOriginID,
          'Browser refused capture/release for the current active pointer');
      end;
    end;
  end
  else
  begin
    FRenderer.Emit(FNode, LDispatch);
  end;
  { A callback can unmount the binding. Only the owned router and physical
    event remain safe to inspect after dispatch. }

  if LConsumed or (LEvents.ViewRevision <> LRevision) then
  begin
    AEvent.preventDefault;
    Result := False;
  end;
  AEvent.stopPropagation;
end;

procedure TNyxBrowserRenderer.GestureFailed(const AOriginID, AReason: TNyxText);
var
  LSink: TNyxGestureFailure;
begin
  FLastGestureError := AReason;
  LSink := FOnGestureFailure;

  if Assigned(LSink) then
  begin
    LSink(AOriginID, AReason);
  end;
end;

function TNyxBrowserBinding.DragStart(AEvent: TJSDragEvent): Boolean;
begin
  Result := DragEvent(AEvent, ntDragStart, ndpStart);
end;

function TNyxBrowserBinding.Drag(AEvent: TJSDragEvent): Boolean;
begin
  Result := DragEvent(AEvent, ntDrag, ndpDrag);
end;

function TNyxBrowserBinding.DragEnter(AEvent: TJSDragEvent): Boolean;
begin
  Result := DragEvent(AEvent, ntDragEnter, ndpEnter);
end;

function TNyxBrowserBinding.DragOver(AEvent: TJSDragEvent): Boolean;
begin
  Result := DragEvent(AEvent, ntDragOver, ndpOver);
end;

function TNyxBrowserBinding.DragExit(AEvent: TJSDragEvent): Boolean;
begin
  Result := DragEvent(AEvent, ntDragExit, ndpExit);
end;

function TNyxBrowserBinding.Drop(AEvent: TJSDragEvent): Boolean;
begin
  Result := DragEvent(AEvent, ntDrop, ndpDrop);
end;

function TNyxBrowserBinding.DragEnd(AEvent: TJSDragEvent): Boolean;
begin
  Result := DragEvent(AEvent, ntDragEnd, ndpEnd);
end;

function TNyxBrowserBinding.DragEvent(AEvent: TJSDragEvent; ATrigger: TNyxTrigger;
  APhase: TNyxDragPhase): Boolean;
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LRenderer: TNyxBrowserRenderer;
  LOriginID: TNyxText;
  LPolicy: TNyxInteractionPolicy;
  LDispatch: TNyxDispatch;
  LTransfer: TNyxTransferSnapshot;
  LAllowed: TNyxDropOperations;
  LOperation: TNyxDropOperation;
  LCapabilities: TNyxGestureCapabilities;
  LDecision: INyxGestureDecision;
  LResponse: TNyxGestureResult;
  LBounds: TJSDOMRect;
  LTarget: TJSHTMLElement;
  LSource: Boolean;
begin
  Result := True;
  LEvents := FRenderer.FEvents;
  LRevision := FViewRevision;
  LSource := APhase in [ndpStart, ndpDrag, ndpEnd];

  if (LEvents.ViewRevision <> LRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode or AEvent.defaultPrevented then
  begin
    Exit;
  end;
  LTarget := TJSHTMLElement(AEvent.target);

  if (LTarget.closest('[data-runtime-id]') <> FElement) or
    (LSource and (FNode.Prop('drag-source') <> 'true')) or
    (not LSource and (FNode.Prop('drop-target') <> 'true')) then
  begin
    Exit;
  end;
  LRenderer := FRenderer;
  LOriginID := FNode.ID;
  LPolicy := NyxInteractionPolicy(FNode);
  { Opted-in sources/targets negotiate explicitly. Default browser source
    behavior is canceled without an offer; a drop never inserts text, opens a
    file or follows a transferred URI through a browser default. }

  if APhase = ndpDrop then
  begin
    AEvent.preventDefault;
  end;
  AEvent.stopPropagation;

  if not LPolicy.CanIssueCommand then
  begin

    if APhase in [ndpStart, ndpDrop] then
    begin
      AEvent.preventDefault;
      Result := False;
    end;
    Exit;
  end;
  try
    LTransfer := CaptureNyxBrowserTransfer(AEvent.dataTransfer,
      APhase in [ndpStart, ndpDrop]);
    TryNyxDropOperations(AEvent.dataTransfer.effectAllowed, LAllowed);
    TryNyxDropOperation(AEvent.dataTransfer.dropEffect, LOperation);
  except
    on LException: Exception do
    begin
      AEvent.preventDefault;
      LRenderer.GestureFailed(LOriginID, LException.Message);
      Exit(False);
    end;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ATrigger);
  LCapabilities := [];

  if AEvent.cancelable then
  begin

    if APhase = ndpStart then
    begin
      Include(LCapabilities, ngcOfferDrag);
    end
    else if (APhase in [ndpEnter, ndpOver, ndpDrop]) and LPolicy.CanEditValue then
    begin
      Include(LCapabilities, ngcAcceptDrop);
    end;
  end;
  LDispatch.Info.HasDrag := True;
  LDispatch.Info.Drag := NyxDragSnapshot(APhase, LTransfer, LAllowed,
    LOperation, '', LCapabilities <> []);
  LDispatch.Info.HasPointer := True;
  LDispatch.Info.Pointer.Kind := npiMouse;
  LBounds := FElement.getBoundingClientRect;
  LDispatch.Info.Pointer.X := AEvent.clientX - LBounds.left;
  LDispatch.Info.Pointer.Y := AEvent.clientY - LBounds.top;
  LDispatch.Info.Pointer.HasPosition := True;

  if AEvent.shiftKey then
  begin
    Include(LDispatch.Info.Pointer.Modifiers, nmShift);
  end;

  if AEvent.ctrlKey then
  begin
    Include(LDispatch.Info.Pointer.Modifiers, nmControl);
  end;

  if AEvent.altKey then
  begin
    Include(LDispatch.Info.Pointer.Modifiers, nmAlt);
  end;

  if AEvent.metaKey then
  begin
    Include(LDispatch.Info.Pointer.Modifiers, nmMeta);
  end;
  LDecision := NewNyxGestureDecision(LCapabilities, LAllowed);
  DispatchNyxGesture(LEvents, FNode, LDispatch, LDecision);
  LResponse := LDecision.Seal;

  if LEvents.ViewRevision <> LRevision then
  begin
    AEvent.preventDefault;
    Exit(False);
  end;

  if APhase = ndpStart then
  begin

    if not LResponse.Offered or (LPolicy.ReadOnly and (ndoMove in LResponse.Allowed)) then
    begin
      AEvent.preventDefault;
      LRenderer.GestureFailed(LOriginID,
        'Drag requires a synchronous offer; a read-only source cannot offer Move');
      Exit(False);
    end;
    try
      OfferNyxBrowserTransfer(AEvent.dataTransfer, LResponse);
    except
      { Host DOM failures are not necessarily Pascal Exception instances. Keep
        the offer canceled and report an owned refusal through the public sink. }
      AEvent.preventDefault;
      LRenderer.GestureFailed(LOriginID, 'The browser refused the typed drag transfer');
      Exit(False);
    end;
    LRenderer.FLastGestureError := '';
  end
  else if APhase in [ndpEnter, ndpOver, ndpDrop] then
  begin
    LOperation := ndoNone;

    if LResponse.Accepted then
    begin
      LOperation := LResponse.Operation;
    end;
    try
      AEvent.dataTransfer.dropEffect := NyxDropOperationName(LOperation);
    except
      AEvent.preventDefault;
      LRenderer.GestureFailed(LOriginID, 'The browser refused the typed drop operation');
      Exit(False);
    end;

    if LOperation <> ndoNone then
    begin
      AEvent.preventDefault;
      LRenderer.FLastGestureError := '';
    end;
  end;
end;

destructor TNyxBrowserBinding.Destroy;
var
  LViewportElement: TJSHTMLElement;
  LFocus: TJSHTMLElement;
  LIndex: Integer;
  LPointerID: Integer;
begin
  { Detach the exact focus surface before disposing a split behavior. A caller
    may still hold its old DOM element after navigation; it must have no live
    Pascal event sink. Remove both capture modes used by default/custom faces. }
  LFocus := FocusElement;

  if LFocus <> nil then
  begin
    LFocus.removeEventListener('focus', @Enter);
    LFocus.removeEventListener('blur', @Leave);
    LFocus.removeEventListener('focusin', @Enter);
    LFocus.removeEventListener('focusout', @Leave);
    LFocus.removeEventListener('keydown', @KeyDown);
    LFocus.removeEventListener('keyup', @KeyUp);
    TNyxKeyboardElement(LFocus).Unlisten('keydown', @KeyDown, True);
    TNyxKeyboardElement(LFocus).Unlisten('keyup', @KeyUp, True);
  end;
  FSplit.Free;
  FSplit := nil;
  { Queued DOM scroll notifications can outlive removal from the document.
    Revoke these producers before releasing their borrowed renderer/node. }

  if FElement <> nil then
  begin
    FElement.removeEventListener('focus', @ForwardRadioFocus);
    FElement.removeEventListener('pointerdown', @PointerDown);
    FElement.removeEventListener('pointerup', @PointerUp);
    FElement.removeEventListener('pointermove', @PointerMove);
    FElement.removeEventListener('pointerenter', @PointerEnter);
    FElement.removeEventListener('pointerleave', @PointerExit);
    FElement.removeEventListener('pointercancel', @PointerCancel);
    FElement.removeEventListener('gotpointercapture', @PointerCapture);
    FElement.removeEventListener('lostpointercapture', @PointerCaptureLost);
    FElement.removeEventListener('dragstart', @DragStart);
    FElement.removeEventListener('drag', @Drag);
    FElement.removeEventListener('dragenter', @DragEnter);
    FElement.removeEventListener('dragover', @DragOver);
    FElement.removeEventListener('dragleave', @DragExit);
    FElement.removeEventListener('drop', @Drop);
    FElement.removeEventListener('dragend', @DragEnd);
    { Revoke sinks before asking the host to release capture; no loss callback
      can reenter a disposed binding, even if the old DOM face is retained. }
    for LIndex := 0 to High(FCapturedPointers) do
    begin
      LPointerID := FCapturedPointers[LIndex];
      try

        if TNyxGestureElement(FElement).hasPointerCapture(LPointerID) then
        begin
          FElement.releasePointerCapture(LPointerID);
        end;

        if (FInput <> nil) and TNyxGestureElement(FInput).hasPointerCapture(LPointerID) then
        begin
          FInput.releasePointerCapture(LPointerID);
        end;
      except
        { Host implicit release may already have ended the pointer. }
      end;
    end;
    FElement.removeEventListener('wheel', @Wheel);
    LViewportElement := FElement;

    if FInput <> nil then
    begin
      LViewportElement := FInput;
    end;
    LViewportElement.removeEventListener('scroll', @Scroll);
    LViewportElement.removeEventListener('scrollend', @ScrollEnd);
    LViewportElement.removeEventListener('beforeinput', @BeforeEdit);
    LViewportElement.removeEventListener('compositionstart', @CompositionStart);
    LViewportElement.removeEventListener('compositionupdate', @CompositionUpdate);
    LViewportElement.removeEventListener('compositionend', @CompositionEnd);
    LViewportElement.removeEventListener('selectionchange', @TextSelectionChanged);
    LViewportElement.removeEventListener('select', @TextSelectionChanged);
  end;
  inherited Destroy;
end;

function TNyxBrowserBinding.Wheel(AEvent: TJSWheelEvent): Boolean;
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
  LUnits: TNyxWheelUnit;
  LModifiers: TNyxKeyModifiers;
  LConsumed: Boolean;
begin
  Result := True;
  LEvents := FRenderer.FEvents;
  LRevision := FViewRevision;

  if (LEvents.ViewRevision <> LRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode or AEvent.defaultPrevented or
    (TJSHTMLElement(AEvent.target).closest('[data-runtime-id]') <> FElement) then
  begin
    Exit;
  end;

  if not LEvents.HasSubscribers(ntBeforeWheel) and not LEvents.HasSubscribers(ntWheel) and
    not LEvents.HasSubscribers(ntAfterWheel) then
  begin
    Exit;
  end;
  case AEvent.deltaMode of
    0:
      begin
        LUnits := nwuPixels;
      end;
    1:
      begin
        LUnits := nwuLines;
      end;
    2:
      begin
        LUnits := nwuPages;
      end;
  else
    { Unknown device units cannot safely be guessed. Leave the native action. }
    Exit;
  end;
  LModifiers := [];

  if AEvent.shiftKey then
  begin
    Include(LModifiers, nmShift);
  end;

  if AEvent.ctrlKey then
  begin
    Include(LModifiers, nmControl);
  end;

  if AEvent.altKey then
  begin
    Include(LModifiers, nmAlt);
  end;

  if AEvent.metaKey then
  begin
    Include(LModifiers, nmMeta);
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ntWheel);
  LDispatch.Info.HasWheel := True;
  LDispatch.Info.Wheel := NyxWheel(AEvent.deltaX, AEvent.deltaY, AEvent.deltaZ,
    LUnits, LModifiers, AEvent.cancelable);
  LConsumed := DispatchNyxWheel(LEvents, FNode, LDispatch,
    FRenderer.FLiveBindings.SignalSnapshot);
  { A callback may destroy this binding; use only retained locals below. }

  if (LConsumed or (LEvents.ViewRevision <> LRevision)) and AEvent.cancelable then
  begin
    AEvent.preventDefault;
    Result := False;
  end;
  AEvent.stopPropagation;
end;

function TNyxBrowserBinding.Scroll(AEvent: TEventListenerEvent): Boolean;
begin
  Result := ViewportEvent(AEvent, ntScroll);
end;

function TNyxBrowserBinding.ScrollEnd(AEvent: TEventListenerEvent): Boolean;
begin
  Result := ViewportEvent(AEvent, ntScrollEnd);
end;

function TNyxBrowserBinding.ViewportEvent(AEvent: TEventListenerEvent;
  ATrigger: TNyxTrigger): Boolean;
var
  LElement: TJSHTMLElement;
  LDispatch: TNyxDispatch;
begin
  Result := True;
  LElement := FElement;

  if FInput <> nil then
  begin
    LElement := FInput;
  end;

  if (FRenderer.FEvents.ViewRevision <> FViewRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode or (AEvent.target <> LElement) or
    not FRenderer.FEvents.HasSubscribers(ATrigger) then
  begin
    Exit;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ATrigger);
  LDispatch.Info.HasViewport := True;
  LDispatch.Info.Viewport := CaptureBrowserViewport(LElement);
  { Scroll is a post-movement observation; it never edits the document value,
    selection or undo history, and it cannot consume a platform default. }
  FRenderer.Emit(FNode, LDispatch);
end;

function TNyxBrowserBinding.FocusElement: TJSHTMLElement;
begin
  Result := FInput;

  if Result = nil then
  begin
    Result := FElement;

    if FSplit <> nil then
    begin
      Result := FSplit.Divider;
    end;
  end;
end;

function TNyxBrowserBinding.ForwardRadioFocus(AEvent: TEventListenerEvent): Boolean;
var
  LPolicy: TNyxInteractionPolicy;
begin
  Result := True;

  if (FRenderer.FEvents.ViewRevision <> FViewRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode or (AEvent.target <> FElement) then
  begin
    Exit;
  end;
  LPolicy := NyxInteractionPolicy(FNode);

  if LPolicy.CanIssueCommand and (FInput <> nil) then
  begin
    { The input's existing focus listener owns the one Nyx notification. }
    NyxFocusWithoutScroll(FInput);
  end;
end;

function TNyxBrowserBinding.IsFocusBoundary(AEvent: TEventListenerEvent): Boolean;
var
  LRelated: TJSNode;
begin
  Result := False;

  if (FRenderer.FEvents.ViewRevision <> FViewRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode then
  begin
    Exit;
  end;

  if (FCollectionMount <> nil) and FCollectionMount.Connected then
  begin
    { Moving between a row and its editor stays in the same logical control.
      Only crossing the owning collection boundary emits enter/exit. }
    LRelated := TJSNode(TJSFocusEvent(AEvent).relatedTarget);
    Result := FElement.contains(TJSNode(AEvent.target)) and
      ((LRelated = nil) or not FElement.contains(LRelated));
  end
  else
  begin
    Result := AEvent.target = FocusElement;
  end;
end;

function TNyxBrowserBinding.Enter(AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;

  if IsFocusBoundary(AEvent) and
    FRenderer.FEvents.HasSubscribers(ntAfterEnter) then
  begin
    FRenderer.Emit(FNode, FRenderer.FLiveBindings.Focus(FNode, ntAfterEnter));
  end;
end;

function TNyxBrowserBinding.Leave(AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;

  if IsFocusBoundary(AEvent) and
    FRenderer.FEvents.HasSubscribers(ntAfterExit) then
  begin
    FRenderer.Emit(FNode, FRenderer.FLiveBindings.Focus(FNode, ntAfterExit));
  end;
end;

function TNyxBrowserBinding.KeyDown(AEvent: TJSKeyboardEvent): Boolean;
begin
  Result := Keyboard(AEvent, ntKeyDown);
end;

function TNyxBrowserBinding.KeyUp(AEvent: TJSKeyboardEvent): Boolean;
begin
  Result := Keyboard(AEvent, ntKeyUp);
end;

function TNyxBrowserBinding.Keyboard(AEvent: TJSKeyboardEvent;
  ATrigger: TNyxTrigger): Boolean;
var
  LModifiers: TNyxKeyModifiers;
  LInput: TJSHTMLElement;
  LDispatch: TNyxDispatch;
  LConsumed: Boolean;
  LEvents: INyxEvents;
  LRevision: Integer;
  LDomain: TNyxValueDomain;
begin
  Result := True;
  LEvents := FRenderer.FEvents;
  LRevision := FViewRevision;
  { A custom listener may have navigated before this listener was entered.
    Check the saved mount generation before dereferencing its borrowed node. }

  if LEvents.ViewRevision <> LRevision then
  begin
    AEvent.preventDefault;
    Exit(False);
  end;

  if FRenderer.FUpdating or FRenderer.FDesignMode or
    FComposing or AEvent.isComposing or AEvent.defaultPrevented then
  begin
    Exit;
  end;
  LInput := FocusElement;
  { Keyboard events bubble. Ordinary controls route their actual input/leaf.
    A collection attachment owns its row/edit descendants, so their keys route
    through the owning data control before its default selection action.
    Compounds receive semantic source registrations through the router. }

  if ((AEvent.target <> LInput) and
    ((FCollectionMount = nil) or not FCollectionMount.Connected or
    not FElement.contains(TJSNode(AEvent.target)))) or
    not NyxHasKeyboardSubscribers(LEvents, ATrigger) then
  begin
    Exit;
  end;
  { Native text edits publish OnChange while typing; browsers normally publish
    change on blur. Admit a pending text edit before capturing a keyboard payload,
    so Ctrl+Enter sees the same accepted memo text without requiring a blur.
    Numeric/domain drafts retain their explicit editing-complete boundary. }

  if (FInput <> nil) and ((FNode.ProjectionKind = 'memo') or
    (FNode.ProjectionKind = 'input') or (FNode.ProjectionKind = 'code-editor')) then
  begin
    LDomain := NyxNodeValueDomain(FNode);

    if not LDomain.Defined or (LDomain.Kind = nskText) then
    begin

      if not Change(AEvent) then
      begin
        Exit;
      end;

      if LEvents.ViewRevision <> LRevision then
      begin
        AEvent.preventDefault;
        Exit(False);
      end;
    end;
  end;
  LModifiers := [];

  if AEvent.shiftKey then
  begin
    Include(LModifiers, nmShift);
  end;

  if AEvent.ctrlKey then
  begin
    Include(LModifiers, nmControl);
  end;

  if AEvent.altKey then
  begin
    Include(LModifiers, nmAlt);
  end;

  if AEvent.metaKey then
  begin
    Include(LModifiers, nmMeta);
  end;

  if AEvent.getModifierState('AltGraph') then
  begin
    Include(LModifiers, nmAltGraph);
  end;
  LDispatch := FRenderer.FLiveBindings.Keyboard(FNode, ATrigger,
    NyxKeyStroke(NyxKeyFromBrowser(AEvent.key), LModifiers,
    (ATrigger = ntKeyDown) and AEvent._repeat));
  LConsumed := DispatchNyxKeyboard(LEvents, FNode, LDispatch,
    FRenderer.FLiveBindings.SignalSnapshot);
  { No borrowed binding/node/widget reads after dispatch: callbacks may navigate.
    The DOM event itself remains owned by the browser for this listener call. }

  if LConsumed or (LEvents.ViewRevision <> LRevision) then
  begin
    AEvent.preventDefault;
    AEvent.stopPropagation;
    Result := False;
  end;
end;

procedure TNyxBrowserRenderer.BindingFailed(ANode: TNyxNode; const AReason: TNyxText;
  AFailure: TNyxBindingFailure);
begin
  FLastBindingError := AReason;
  FLastBindingFailure := AFailure;
  { The accepted model was untouched by rejection. Restore the physical input
    through the same in-place path, including any Boolean or compound mirrors. }

  if AFailure = nbfRejected then
  begin
    FForceValues := True;
    try
      Sync;
    finally
      FForceValues := False;
    end;
  end;

  if Assigned(FOnBindingError) then
  begin
    FOnBindingError(ANode, AReason, AFailure);
  end;
end;

procedure TNyxBrowserBinding.SyncLiteralItems;
var
  LKind: TNyxKind;
  LText: TNyxText;
  LSelected: TNyxText;
  LRows: TNyxStrings;
  LCells: TNyxStrings;
  LHost: TJSHTMLElement;
  LRow: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LIndex: Integer;
  LColumn: Integer;
  LScrollTop: NativeInt;
  LScrollLeft: NativeInt;
begin

  if FCustom or (FCollectionMount <> nil) or
    not TryNyxKind(FNode.ProjectionKind, LKind) or
    not (LKind in [nkSelect, nkList, nkTable, nkTree]) then
  begin
    Exit;
  end;
  LText := FNode.Prop('items');

  if FHasItemsBaseline and (FLastItems = LText) then
  begin
    Exit;
  end;
  LHost := FElement;
  LSelected := '';

  if LKind = nkSelect then
  begin
    LHost := FInput;
    LSelected := FNode.Prop('value');

    if FHasValueBaseline and (FLastValue = LSelected) then
    begin
      { A changed option set preserves the physical draft when its accepted
        value has not changed. Assigning a missing choice yields no selection;
        it never writes an invented first item back into the portable model. }
      LSelected := TJSHTMLSelectElement(LHost).value;
    end;
  end;
  LScrollTop := LHost.scrollTop;
  LScrollLeft := LHost.scrollLeft;
  LRows := TNyxStrings.Create;
  try
    LRows.Text := LText;
    { Only these fallback-owned children are replaced. The logical host, its
      focus, callbacks and viewport remain mounted; identical Items is a no-op. }
    LHost.textContent := '';
    for LIndex := 0 to LRows.Count - 1 do
    begin

      if LKind = nkTable then
      begin
        LRow := Element('tr', '');
        LCells := NyxLiteralCells(LRows[LIndex]);
        try
          for LColumn := 0 to LCells.Count - 1 do
          begin

            if LIndex = 0 then
            begin
              LCell := Element('th', '');
            end
            else
            begin
              LCell := Element('td', '');
            end;
            LCell.textContent := LCells[LColumn];
            LRow.appendChild(LCell);
          end;
        finally
          LCells.Free;
        end;
      end
      else
      begin
        case LKind of
          nkSelect: LRow := Element('option', '');
          nkList: LRow := Element('li', '');
        else
          LRow := Element('div', '');
        end;
        LRow.textContent := LRows[LIndex];

        if LKind = nkSelect then
        begin
          LRow.setAttribute('value', LRows[LIndex]);
        end;
      end;
      LHost.appendChild(LRow);
    end;

    if LKind = nkSelect then
    begin
      TJSHTMLSelectElement(LHost).value := LSelected;
    end;
    LHost.scrollTop := LScrollTop;
    LHost.scrollLeft := LScrollLeft;
    FLastItems := LText;
    FHasItemsBaseline := True;
  finally
    LRows.Free;
  end;
end;

procedure TNyxBrowserRenderer.SyncRadioFocus;
type
  TRadioScope = record
    Name: TNyxText;
    Form: TJSHTMLFormElement;
    Entry: TNyxBrowserBinding;
    Checked: Boolean;
    BlockedChecked: Boolean;
  end;
var
  LScopes: array of TRadioScope;
  LIndex: Integer;
  LScope: Integer;
  LRadio: TJSHTMLInputElement;
  LPolicy: TNyxInteractionPolicy;
begin
  SetLength(LScopes, 0);
  for LIndex := 0 to High(FBindings) do
  begin

    if not FBindings[LIndex].FCustom and
      (FBindings[LIndex].FNode.ProjectionKind = 'radio') then
    begin
      LRadio := TJSHTMLInputElement(FBindings[LIndex].FInput);
      LRadio.tabIndex := -1;
      FBindings[LIndex].FElement.removeAttribute('tabindex');
      LPolicy := NyxInteractionPolicy(FBindings[LIndex].FNode);
      LScope := 0;
      while (LScope < Length(LScopes)) and
        ((LRadio.name = '') or (LScopes[LScope].Name <> LRadio.name) or
        (LScopes[LScope].Form <> LRadio.form)) do
      begin
        Inc(LScope);
      end;

      if LScope = Length(LScopes) then
      begin
        SetLength(LScopes, LScope + 1);
        LScopes[LScope].Name := LRadio.name;
        LScopes[LScope].Form := LRadio.form;
        LScopes[LScope].Entry := nil;
        LScopes[LScope].Checked := False;
        LScopes[LScope].BlockedChecked := False;
      end;

      if not LPolicy.CanIssueCommand then
      begin
        LScopes[LScope].BlockedChecked := LScopes[LScope].BlockedChecked or LRadio.checked;
        Continue;
      end;

      if LScopes[LScope].Entry = nil then
      begin
        LScopes[LScope].Entry := FBindings[LIndex];
      end;

      if LRadio.checked and not LScopes[LScope].Checked then
      begin
        LScopes[LScope].Entry := FBindings[LIndex];
        LScopes[LScope].Checked := True;
      end;
    end;
  end;
  for LScope := 0 to High(LScopes) do
  begin

    if LScopes[LScope].Entry = nil then
    begin
      Continue;
    end;

    if LScopes[LScope].BlockedChecked then
    begin
      { Explicit input tabindex cannot override Chromium's checked-peer group
        exclusion. Keep that value intact and enter through its existing label;
        ForwardRadioFocus immediately delegates to the same enabled input. }
      LScopes[LScope].Entry.FElement.tabIndex := 0;
    end
    else
    begin
      LScopes[LScope].Entry.FInput.tabIndex := 0;
    end;
  end;
end;

procedure TNyxBrowserRenderer.Sync;
const
  CMetricKeys: array[0..5] of TNyxText = ('width', 'height', 'gap', 'padding', 'left', 'top');
var
  LIndex: Integer;
  LMetricIndex: Integer;
  LBinding: TNyxBrowserBinding;
  LNode: TNyxNode;
  LPolicy: TNyxInteractionPolicy;
  LControl: TJSHTMLElement;
  LEnabled: Boolean;
  LReadOnly: Boolean;
  LKeyboardKind: TNyxKind;
  LValue: TNyxText;
  LInfo: TNyxPrimitiveInfo;
  LLayout: TNyxText;
  LMinimum: Integer;
  LMaximum: Integer;

  procedure AttributeFlag(AElement: TJSHTMLElement; const AName: TNyxText; ASet: Boolean);
  begin

    if ASet then
    begin
      AElement.setAttribute(AName, '');
    end
    else
    begin
      AElement.removeAttribute(AName);
    end;
  end;

begin

  if FUpdating then
  begin
    Exit;
  end;
  FUpdating := True;
  try
    for LIndex := 0 to Length(FBindings) - 1 do
    begin
      LBinding := FBindings[LIndex];
      LNode := LBinding.FNode;
      LControl := LBinding.FElement;
      LControl.setAttribute('data-variant', LNode.Prop('variant'));
      LControl.title := LNode.Prop('hint');
      LControl.draggable := not FDesignMode and (LNode.Prop('drag-source') = 'true');
      LControl.style.setProperty('touch-action', LNode.Prop('touch-behavior', 'auto'));

      if LNode.Prop('pressed') <> '' then
      begin
        LControl.setAttribute('aria-pressed', LowerCase(LNode.Prop('pressed')));
      end
      else
      begin
        LControl.removeAttribute('aria-pressed');
      end;
      LControl.style.removeProperty('display');
      LControl.style.removeProperty('flex-direction');
      LControl.style.removeProperty('position');
      LControl.style.removeProperty('flex-wrap');
      LControl.style.removeProperty('align-items');
      LControl.style.removeProperty('justify-content');
      LControl.style.removeProperty('align-content');
      LControl.classList.remove('nyx-flow-row');
      LControl.classList.remove('nyx-flow-column');
      LControl.classList.remove('nyx-aligned');
      LLayout := LNode.Prop('layout');

      if (LLayout = '') and not LBinding.FCustom and
        FindNyxPrimitive(LNode.ProjectionKind, LInfo) and LInfo.Container and
        (LNode.ProjectionKind <> 'split-view') then
      begin
        LLayout := NyxLayout(LNode);
      end;

      if LLayout = 'grid' then
      begin
        LControl.style.setProperty('display', 'grid');
      end
      else if (LLayout = 'row') or (LLayout = 'column') then
      begin
        LControl.style.setProperty('display', 'flex');
        LControl.style.setProperty('flex-direction', LLayout);
        LControl.classList.add('nyx-flow-' + LLayout);
        { Wrapped rows pack natural lines at the leading cross edge. A definite
          height does not stretch every line's height behind the author's back. }
        LControl.style.setProperty('align-content', 'flex-start');
        LValue := LNode.Prop('flow-wrap', 'auto');

        if (LLayout = 'row') and (LValue <> '') and (LValue <> 'auto') then
        begin
          LControl.style.setProperty('flex-wrap', LValue);
        end;
        LValue := LNode.Prop('cross-alignment', 'auto');

        if (LValue <> '') and (LValue <> 'auto') then
        begin
          LControl.classList.add('nyx-aligned');

          if (LValue = 'start') or (LValue = 'end') then
          begin
            LValue := 'flex-' + LValue;
          end;

          if (LValue = 'center') or (LValue = 'flex-end') then
          begin
            LValue := 'safe ' + LValue;
          end;
          LControl.style.setProperty('align-items', LValue);
        end;
        LValue := LNode.Prop('justification', 'start');

        if LValue <> '' then
        begin

          if (LValue = 'start') or (LValue = 'end') then
          begin
            LValue := 'flex-' + LValue;
          end;

          if (LValue = 'center') or (LValue = 'flex-end') then
          begin
            LValue := 'safe ' + LValue;
          end;
          LControl.style.setProperty('justify-content', LValue);
        end;
      end
      else if LLayout = 'absolute' then
      begin
        LControl.style.setProperty('display', 'block');
        { Coordinates belong to this host's content box, not the page body. }
        LControl.style.setProperty('position', 'relative');
      end;

      if (LNode.Parent <> nil) and (NyxLayout(LNode.Parent) = 'absolute') then
      begin
        LControl.style.setProperty('position', 'absolute');
      end;

      if not LBinding.FCustom and (LNode.ProjectionKind <> 'card') then
      begin

        if LNode.Prop('surface') = 'true' then
        begin
          LControl.classList.add('nyx-card');
        end
        else
        begin
          LControl.classList.remove('nyx-card');
        end;
      end;

      if NyxSupportsViewport(LNode) and (LBinding.FInput = nil) then
      begin
        { Sync must retain the same scroll face as initial projection. A table
          otherwise reverts to intrinsic height and silently loses its viewport. }
        LControl.style.removeProperty('overflow');

        if (LNode.Prop('height') <> '') or (LNode.ProjectionKind = 'scroll') then
        begin
          LControl.style.setProperty('overflow', 'auto');
        end;

        if (LNode.ProjectionKind = 'table') and (LNode.Prop('height') <> '') then
        begin
          LControl.style.setProperty('display', 'block');
        end;
      end;

      if LNode.Prop('visible', 'true') = 'false' then
      begin
        LControl.style.setProperty('display', 'none');
      end;
      for LMetricIndex := 0 to High(CMetricKeys) do
      begin
        LValue := LNode.Prop(CMetricKeys[LMetricIndex]);

        if LValue = '' then
        begin
          LControl.style.removeProperty(CMetricKeys[LMetricIndex]);
        end
        else
        begin
          LControl.style.setProperty(CMetricKeys[LMetricIndex], LValue + 'px');
        end;
      end;
      { Sizing policies are translated only here. Retained pixel metrics become
        effective again after Automatic/Clear; Sync never changes descriptor data. }
      LValue := LNode.Prop('width-sizing');

      if LValue = 'content' then
      begin
        LControl.style.setProperty('width', 'fit-content');
      end
      else if LValue = 'fill' then
      begin
        LControl.style.setProperty('width', '100%');
      end;
      LValue := LNode.Prop('height-sizing');
      LControl.style.removeProperty('min-height');

      if LValue = 'content' then
      begin
        LControl.style.setProperty('height', 'max-content');
        LControl.style.setProperty('min-height', '0');
      end
      else if LValue = 'fill' then
      begin
        LControl.style.setProperty('height', '100%');
        LControl.style.setProperty('min-height', '0');
      end;
      LControl.style.removeProperty('flex');
      LControl.classList.remove('nyx-flex');
      LControl.style.removeProperty('grid-template-columns');

      if (LNode.Prop('flex') <> '') or (NyxFlexWeight(LNode) > 0) then
      begin

        if NyxFlexWeight(LNode) > 0 then
        begin
          LControl.style.setProperty('flex', IntToStr(NyxFlexWeight(LNode)));
          LControl.classList.add('nyx-flex');
        end
        else
        begin
          LControl.style.setProperty('flex', '0 0 auto');
        end;
      end;

      if LNode.Prop('columns') <> '' then
      begin
        LControl.style.setProperty('grid-template-columns',
          'repeat(' + LNode.Prop('columns') + ',minmax(0,1fr))');
      end;
      LPolicy := NyxInteractionPolicy(LNode);
      LEnabled := LPolicy.Enabled;
      LReadOnly := LPolicy.ReadOnly;
      AttributeFlag(LControl, 'disabled', not LEnabled);
      LControl.setAttribute('aria-disabled', LowerCase(BoolToStr(not LEnabled, True)));

      if not LBinding.FCustom and (LBinding.FInput = nil) and
        NyxSupportsKeyboard(LNode) and TryNyxKind(LNode.ProjectionKind, LKeyboardKind) then
      begin
        case LKeyboardKind of
          nkCode, nkList, nkTable, nkTree:
            begin
              { These standard faces have no intrinsic HTML Tab entry. A
                disabled static face loses tabindex altogether; a negative
                tabindex would still allow programmatic/click focus. Read-only
                retains inspection. The managed collection owns its own entry. }

              if LBinding.FCollectionMount = nil then
              begin

                if LEnabled then
                begin
                  LControl.setAttribute('tabindex', '0');
                end
                else
                begin
                  LControl.removeAttribute('tabindex');
                end;
              end;
            end;
          nkLink:
            begin
              { An anchor ignores HTML disabled. Retain its accessible link
                identity while withdrawing navigation/focus, then restore the
                authored URL on re-enable. Do not hide disabled content with
                inert or manufacture a positive focus order. }
              LControl.setAttribute('role', 'link');

              if LEnabled then
              begin
                LValue := LNode.Prop('href', '#');

                if not SafeURL(LValue, False) then
                begin
                  raise ENyxModel.Create('Unsupported link URL');
                end;
                LControl.setAttribute('href', LValue);
              end
              else
              begin
                LControl.removeAttribute('href');
              end;
            end;
          else
            begin
              { Other standard faces already supply their intrinsic focus and
                accessibility behavior; this branch makes no additional change. }
            end;
        end;
      end;

      if LBinding.FCollectionMount <> nil then
      begin
        LBinding.FCollectionMount.SetInteraction(LEnabled and not FDesignMode,
          LReadOnly or FDesignMode);
      end;
      LBinding.SyncLiteralItems;

      if LBinding.FCaption <> nil then
      begin
        LBinding.FCaption.textContent := LNode.Prop('text');
      end
      else if not LBinding.FCustom and (LNode.Count = 0) and
        ((LNode.ProjectionKind = 'label') or (LNode.ProjectionKind = 'heading') or
        (LNode.ProjectionKind = 'button') or (LNode.ProjectionKind = 'link') or
        (LNode.ProjectionKind = 'badge') or (LNode.ProjectionKind = 'alert') or
        (LNode.ProjectionKind = 'avatar') or (LNode.ProjectionKind = 'code')) then
      begin
        LControl.textContent := LNode.Prop('text');
      end;

      if LControl is TJSHTMLProgressElement then
      begin
        { HTML progress has an intrinsic minimum of zero. Project the declared
          interval while retaining its actual scalar bounds in accessibility. }
        LMinimum := StrToIntDef(LNode.Prop('min'), 0);
        LMaximum := StrToIntDef(LNode.Prop('max'), 100);
        TJSHTMLProgressElement(LControl).max := Max(1, LMaximum - LMinimum);
        TJSHTMLProgressElement(LControl).value :=
          StrToIntDef(LNode.Prop('value'), 0) - LMinimum;

        if LMaximum = LMinimum then
        begin
          TJSHTMLProgressElement(LControl).value := 1;
        end;
        LControl.setAttribute('aria-valuemin', IntToStr(LMinimum));
        LControl.setAttribute('aria-valuemax', IntToStr(LMaximum));
        LControl.setAttribute('aria-valuenow', LNode.Prop('value', '0'));
      end;

      if not LBinding.FCustom and (LControl is TJSHTMLImageElement) then
      begin
        LValue := LNode.Prop('src');

        if not SafeURL(LValue, True) then
        begin
          raise ENyxModel.Create('Unsupported image URL');
        end;

        if LValue = '' then
        begin
          { src="" requests the current page on some hosts. Clearing a source
            withdraws the attribute rather than issuing a meaningless request. }
          LControl.removeAttribute('src');
        end
        else if LControl.getAttribute('src') <> LValue then
        begin
          LControl.setAttribute('src', LValue);
        end;
        TJSHTMLImageElement(LControl).alt := LNode.Prop('alt', LNode.Prop('text'));
      end;

      if LNode.Prop('aria-label') <> '' then
      begin
        LControl.setAttribute('aria-label', LNode.Prop('aria-label'));
      end
      else if not LBinding.FCustom and (LBinding.FInput = nil) and
        (LBinding.FCollectionMount = nil) and
        TryNyxKind(LNode.ProjectionKind, LKeyboardKind) and
        (LKeyboardKind in [nkCode, nkList, nkTable, nkTree]) then
      begin
        { The same authored caption names native static inspection faces. }
        LControl.setAttribute('aria-label', LNode.Prop('text', LNode.ID));
      end
      else
      begin
        LControl.removeAttribute('aria-label');
      end;

      if Assigned(LBinding.FUpdater) then
      begin
        LBinding.FUpdater(LNode, LBinding.FElement);
      end;

      if LBinding.FInput <> nil then
      begin
        LControl := LBinding.FInput;
        AttributeFlag(LControl, 'disabled', not LEnabled);
        AttributeFlag(LControl, 'readonly', LReadOnly);
        LControl.setAttribute('aria-readonly', LowerCase(BoolToStr(LReadOnly, True)));
        LControl.title := LNode.Prop('hint');
        LControl.setAttribute('placeholder', LNode.Prop('placeholder'));
        LControl.setAttribute('aria-label', LNode.Prop('aria-label', LNode.Prop('text', LNode.ID)));

        if not LBinding.FCustom and not LBinding.FComposing and
          (LNode.ProjectionKind = 'input') then
        begin
          LValue := LNode.Prop('input-type', 'text');

          if LValue = '' then
          begin
            LValue := 'text';
          end;

          if TJSHTMLInputElement(LControl)._type <> LValue then
          begin
            TJSHTMLInputElement(LControl)._type := LValue;
          end;

          if LValue = 'number' then
          begin
            LControl.setAttribute('step', 'any');
          end
          else
          begin
            LControl.removeAttribute('step');
          end;
        end;

        if (LNode.ProjectionKind = 'spin') or (LNode.ProjectionKind = 'slider') then
        begin
          LControl.setAttribute('min', LNode.Prop('min', '0'));
          LControl.setAttribute('max', LNode.Prop('max', '100'));
        end
        else
        begin
          LControl.removeAttribute('min');
          LControl.removeAttribute('max');

          if LNode.Prop('min') <> '' then
          begin
            LControl.setAttribute('min', LNode.Prop('min'));
          end;

          if LNode.Prop('max') <> '' then
          begin
            LControl.setAttribute('max', LNode.Prop('max'));
          end;
        end;
        LValue := LNode.Prop('value');
        { Layout/state publications must not replace text owned by an IME. The
          accepted baseline remains pending until the real composition end. }

        if LBinding.FComposing then
        begin
          Continue;
        end;
        { A different key may update while this field holds an unfinished draft.
          Only an accepted change to its own value replaces that physical draft;
          failed commands explicitly force restoration above. }

        if not FForceValues and LBinding.FHasValueBaseline and
          (LBinding.FLastValue = LValue) then
        begin
          Continue;
        end;
        LBinding.FHasValueBaseline := True;
        LBinding.FLastValue := LValue;
        { Setting the same value can reset selection/caret on mobile browsers.
          Keep unchanged physical values, focus and unfinished native drafts. }

        if LControl is TJSHTMLTextAreaElement then
        begin

          if TJSHTMLTextAreaElement(LControl).value <> LValue then
          begin
            TJSHTMLTextAreaElement(LControl).value := LValue;
          end;
        end
        else if LControl is TJSHTMLSelectElement then
        begin

          if TJSHTMLSelectElement(LControl).value <> LValue then
          begin
            TJSHTMLSelectElement(LControl).value := LValue;
          end;
        end
        else if (LNode.ProjectionKind = 'checkbox') or (LNode.ProjectionKind = 'switch') or
          (LNode.ProjectionKind = 'radio') then
        begin
          TJSHTMLInputElement(LControl).checked := LValue = 'true';
        end
        else
        begin

          if TJSHTMLInputElement(LControl).value <> LValue then
          begin
            TJSHTMLInputElement(LControl).value := LValue;
          end;
        end;
      end;
    end;
    SyncRadioFocus;
    { Generic metrics never override an adapter-owned split layout. Do this
      after all child bindings so their ordinary root styles cannot undo it. }
    for LIndex := 0 to Length(FBindings) - 1 do
    begin

      if FBindings[LIndex].FSplit <> nil then
      begin
        FBindings[LIndex].FSplit.Update;
      end;
    end;
  finally
    FUpdating := False;
  end;
end;

end.
