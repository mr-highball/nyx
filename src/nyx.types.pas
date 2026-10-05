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

unit nyx.types;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text;

type
  { Built-in kinds have compile-time identities. Custom recipe names remain an
    explicit extension boundary; adding one never requires editing this enum. }
  TNyxKind = (nkPage, nkColumn, nkRow, nkGrid, nkPanel, nkCard, nkGroup,
    nkToolbar, nkScroll, nkTabs, nkTab, nkHeading, nkLabel, nkButton, nkLink,
    nkInput, nkMemo, nkCheckbox, nkSwitch, nkRadio, nkSelect, nkSpin, nkSlider,
    nkDate, nkTime, nkColor, nkList, nkTable, nkTree, nkImage, nkAvatar,
    nkProgress, nkBadge, nkAlert, nkSeparator, nkSpacer, nkCode, nkCodeEditor,
    nkDesignSurface, nkComponent, nkSplitView, nkLabeledButton, nkSplitButton, nkSearchField,
    nkFormField, nkLoginForm, nkSettingsPanel, nkDateRange, nkNumberStepper,
    nkSegmentedControl, nkRating, nkDataToolbar, nkFilterBar, nkPagination,
    nkBreadcrumbs, nkSidebarNav, nkMasterDetail, nkListCard, nkDataCard,
    nkKanbanBoard, nkTimeline, nkStatCard, nkMetricGrid, nkEmptyState,
    nkConfirmationDialog, nkNotificationCard, nkToast, nkWizardStep, nkStepper,
    nkProfileCard, nkMediaCard, nkAvatarGroup, nkCommentThread, nkPropertyGrid,
    nkCommandBar, nkFloatingActionPanel, nkSlotOverride);

  TNyxLayoutMode = (nlColumn, nlRow, nlGrid, nlAbsolute);
  { Flow policies use logical axes: main follows row/column direction; cross
    is perpendicular. Automatic preserves the primitive's documented defaults.
    Wrap applies to rows; it never changes authored order or keyboard order. }
  TNyxFlowWrap = (nfwAutomatic, nfwNoWrap, nfwWrap);
  TNyxCrossAlignment = (ncaAutomatic, ncaStart, ncaCenter, ncaEnd, ncaStretch);
  TNyxJustification = (njStart, njCenter, njEnd, njSpaceBetween, njSpaceAround,
    njSpaceEvenly);
  { Automatic honors an authored pixel metric or the primitive's natural policy.
    Content requests intrinsic size; Fill requests its containing content extent.
    A positive Flex weight still owns the parent's main-axis allocation. }
  TNyxSizing = (nsAutomatic, nsContent, nsFill);
  { Platform selection is a runtime projection choice, independent of the
    compiler that built the authoring tool. Any supplies portable defaults. }
  TNyxPlatform = (npfAny, npfBrowser, npfNativeLCL);
  { Stacked panes share height; side-by-side panes share width. Position and
    bounds are integer percentages of the space remaining after the divider. }
  TNyxSplitOrientation = (nsoStacked, nsoSideBySide);
  { Browser direct-manipulation negotiation. Native mouse capture does not
    implement touch-action; that difference is exposed by property metadata. }
  TNyxTouchBehavior = (ntbAutomatic, ntbNone, ntbPanX, ntbPanY, ntbManipulation);
  TNyxVariant = (nvDefault, nvPrimary, nvSecondary, nvDanger, nvSuccess,
    nvWarning, nvGhost);
  TNyxAction = (naNone, naClear, naDismiss, naToggle, naSelect, naIncrement,
    naDecrement);
  { Closed portable interaction families. Existing ordinals are retained; wire
    descriptors use names. Before/after keyboard hooks bracket Nyx dispatch,
    before the platform default; text hooks bracket accepted model admission.
    Design-only triggers never invoke application actions. ntNamed is an open
    transport selected by OnNamed with an exact name and declared payload; it
    is never accepted as an anonymous closed physical registration. }
  TNyxTrigger = (ntClick, ntChange, ntDesignSelect, ntDesignValue,
    ntAfterEnter, ntAfterExit, ntKeyDown, ntKeyUp,
    ntBeforeKeyDown, ntAfterKeyDown, ntKeyPress, ntBeforeKeyPress,
    ntAfterKeyPress, ntBeforeKeyUp, ntAfterKeyUp,
    ntBeforeTextInput, ntTextInput, ntAfterTextInput, ntDoubleClick,
    ntPointerDown, ntPointerUp, ntPointerMove, ntPointerEnter, ntPointerExit,
    ntContextMenu, ntNamed, ntBeforeWheel, ntWheel, ntAfterWheel, ntScroll,
    ntScrollEnd, ntSelectionChange, ntBeforeEdit, ntCompositionStart,
    ntCompositionUpdate, ntCompositionEnd, ntTextSelectionChange,
    ntPointerCancel, ntPointerCapture, ntPointerCaptureLost,
    ntDragStart, ntDrag, ntDragEnter, ntDragOver, ntDragExit, ntDrop, ntDragEnd);
  TNyxOverrideMode = (noProperties, noAppend, noPrepend, noReplace, noRemove);
  TNyxInputType = (niText, niPassword, niEmail, niNumber, niSearch, niTel, niUrl);

  { Logical shortcut keys, independent of DOM/LCL classes and keyboard layout.
    Text entry remains the control's change/value contract, including IME input.
    Unrecognized layout-specific keys are explicitly nkUnknownKey; adapters must
    never guess a letter from a physical browser code. }
  TNyxKey = (nkUnknownKey, nkBackspaceKey, nkTabKey, nkEnterKey, nkEscapeKey,
    nkSpaceKey, nkPageUpKey, nkPageDownKey, nkEndKey, nkHomeKey, nkLeftKey,
    nkUpKey, nkRightKey, nkDownKey, nkInsertKey, nkDeleteKey, nkShiftKey,
    nkControlKey, nkAltKey, nkMetaKey,
    nk0Key, nk1Key, nk2Key, nk3Key, nk4Key, nk5Key, nk6Key, nk7Key, nk8Key, nk9Key,
    nkAKey, nkBKey, nkCKey, nkDKey, nkEKey, nkFKey, nkGKey, nkHKey, nkIKey,
    nkJKey, nkKKey, nkLKey, nkMKey, nkNKey, nkOKey, nkPKey, nkQKey, nkRKey,
    nkSKey, nkTKey, nkUKey, nkVKey, nkWKey, nkXKey, nkYKey, nkZKey,
    nkF1Key, nkF2Key, nkF3Key, nkF4Key, nkF5Key, nkF6Key, nkF7Key, nkF8Key,
    nkF9Key, nkF10Key, nkF11Key, nkF12Key, nkF13Key, nkF14Key, nkF15Key,
    nkF16Key, nkF17Key, nkF18Key, nkF19Key, nkF20Key, nkF21Key, nkF22Key,
    nkF23Key, nkF24Key);
  TNyxKeyModifier = (nmShift, nmControl, nmAlt, nmMeta, nmAltGraph);
  TNyxKeyModifiers = set of TNyxKeyModifier;

  { Target-neutral pointer data. Coordinates are logical pixels relative to the
    projected control's outer face. Native LCL mouse hooks report npiMouse;
    browsers retain touch/pen identity without exposing platform handles.
    A context menu invoked from the keyboard has no pointer position. }
  TNyxPointerKind = (npiUnknown, npiMouse, npiTouch, npiPen);
  TNyxPointerButton = (npbNone, npbPrimary, npbAuxiliary, npbSecondary, npbOther);
  TNyxPointerButtons = set of TNyxPointerButton;
  TNyxPointerSnapshot = record
    Kind: TNyxPointerKind;
    Button: TNyxPointerButton;
    Buttons: TNyxPointerButtons;
    Modifiers: TNyxKeyModifiers;
    X: Double;
    Y: Double;
    HasPosition: Boolean;
    { Browser pointer identity is scoped to its active interaction; native
      mouse slots use zero. Pressure is normalized 0..1; unknown native mouse
      pressure is represented as zero rather than invented hardware data. }
    ID: Integer;
    Primary: Boolean;
    Pressure: Double;
  end;

  { Complete proposed text replacement, with exact Unicode and line endings.
    This intentionally covers paste, deletion, composition and virtual keyboards
    without inferring characters from shortcut keys. Copies own both strings. }
  TNyxTextEdit = record
    Before: TNyxText;
    After: TNyxText;
  end;

  { Owned immutable scalar snapshot. Matches uses exact modifiers and rejects
    repeats by default, making one-shot shortcuts deliberate. No widget/event
    handle is retained, so queued callbacks may safely inspect this value. }
  TNyxKeyStroke = record
  private
    FKey: TNyxKey;
    FModifiers: TNyxKeyModifiers;
    FRepeating: Boolean;
  public
    function Matches(AKey: TNyxKey; AModifiers: TNyxKeyModifiers = [];
      AAllowRepeat: Boolean = False): Boolean;
    property Key: TNyxKey read FKey;
    property Modifiers: TNyxKeyModifiers read FModifiers;
    property Repeating: Boolean read FRepeating;
  end;

  { Keys belong to persistence/adapters. Fluent callers use named typed methods;
    Clear takes a key enum so clearing an optional property remains typed too. }
  TNyxAttribute = (atText, atValue, atPlaceholder, atItems, atHint,
    atAccessibleName, atHref, atSource, atAlt, atLayout, atPadding, atGap,
    atColumns, atWidth, atHeight, atLeft, atTop, atFlex, atMinimum, atMaximum,
    atEnabled, atVisible, atReadOnly, atSurface, atCompound, atPressed,
    atVariant, atAction, atProjection, atOverrideMode, atInputType, atPart,
    atTarget, atComponent, atEmit, atEmitChange, atOption, atPath, atDesignID,
    atSplitOrientation, atSplitPosition, atSplitMinimum, atSplitMaximum,
    atSplitResizable, atDragSource, atDropTarget, atTouchBehavior,
    atFlowWrap, atCrossAlignment, atJustification, atWidthSizing, atHeightSizing);

  { Open application names are distinct value types, never behavioral keywords.
    These records own immutable text values, without mutable arrays/UI handles.
    A part path, event name and component reference cannot be interchanged. }
  TNyxPartRef = record
    Name: TNyxText;
  end;
  TNyxEventRef = record
    Name: TNyxText;
  end;
  { The built-in recipes share a closed, typed vocabulary of meaningful actions.
    These are named streams, independent of the physical click/change producer.
    Extensions retain distinct open TNyxEventRef names through NyxNamedEvent. }
  TNyxSemanticEvent = (nseActivate, nsePrimary, nseMenu, nseSearch, nseClear,
    nseSubmit, nseSave, nseDecrement, nseIncrement, nseSelect, nseRate,
    nseCreate, nseRefresh, nseApply, nseReset, nsePrevious, nseNext, nseHome,
    nseSection, nseOverview, nseProjects, nseSettings, nseEdit, nseViewAll,
    nseExport, nseAdd, nseCancel, nseConfirm, nseDismiss, nseUndo, nseBack,
    nseStep, nseMessage, nseFollow, nseOpen, nseFavorite, nseReply, nseNew);
  TNyxComponentRef = record
    Name: TNyxText;
  end;
  { Exact authored control identity, distinct from a reusable definition name
    and a named-part path. Existence and document uniqueness are admitted by
    the model; no control, document or platform handle is retained. }
  TNyxControlRef = record
    ID: TNyxText;
  end;
  { An explicit copied identity assignment used when deriving an owned subtree.
    Open names are data; derivation never guesses extension/source references. }
  TNyxIdentityAssignment = record
    Source, Destination: TNyxControlRef;
  end;
  TNyxStyleRef = record
    Name: TNyxText;
  end;
  TNyxKindRef = record
    Name: TNyxText;
  end;

function NyxKindName(AKind: TNyxKind): TNyxText;
function TryNyxKind(const AName: TNyxText; out AKind: TNyxKind): Boolean;
function NyxAttributeName(AAttribute: TNyxAttribute): TNyxText;
function TryNyxAttribute(const AName: TNyxText; out AAttribute: TNyxAttribute): Boolean;
function NyxLayoutName(AValue: TNyxLayoutMode): TNyxText;
function NyxFlowWrapName(AValue: TNyxFlowWrap): TNyxText;
function NyxCrossAlignmentName(AValue: TNyxCrossAlignment): TNyxText;
function NyxJustificationName(AValue: TNyxJustification): TNyxText;
function NyxSizingName(AValue: TNyxSizing): TNyxText;
function NyxPlatformName(AValue: TNyxPlatform): TNyxText;
function NyxPlatformSymbol(AValue: TNyxPlatform): TNyxText;
function NyxSplitOrientationName(AValue: TNyxSplitOrientation): TNyxText;
function NyxTouchBehaviorName(AValue: TNyxTouchBehavior): TNyxText;
{ Reserved persistence keys are interpreted only at this explicit boundary.
  Applications use Configure.ForPlatform and typed configuration methods. }
function NyxPlatformKey(APlatform: TNyxPlatform; AAttribute: TNyxAttribute): TNyxText;
function TryNyxPlatformKey(const AKey: TNyxText; out APlatform: TNyxPlatform;
  out AAttribute: TNyxAttribute): Boolean;
function NyxPlatformAttribute(AAttribute: TNyxAttribute): Boolean;
function NyxVariantName(AValue: TNyxVariant): TNyxText;
function NyxActionName(AValue: TNyxAction): TNyxText;
function TryNyxAction(const AName: TNyxText; out AAction: TNyxAction): Boolean;
function NyxTriggerName(ATrigger: TNyxTrigger): TNyxText;
{ One canonical registry serves schema, persistence and crafted source. }
function NyxTriggerTitle(ATrigger: TNyxTrigger): TNyxText;
function NyxTriggerSymbol(ATrigger: TNyxTrigger): TNyxText;
function TryNyxTrigger(const AName: TNyxText; out ATrigger: TNyxTrigger): Boolean;
function NyxIsKeyboardTrigger(ATrigger: TNyxTrigger): Boolean;
function NyxIsInputTrigger(ATrigger: TNyxTrigger): Boolean;
function NyxIsRuntimeTrigger(ATrigger: TNyxTrigger): Boolean;
function NyxKeyStroke(AKey: TNyxKey; AModifiers: TNyxKeyModifiers = [];
  ARepeating: Boolean = False): TNyxKeyStroke;
{ Explicit adapter boundaries: modern browser KeyboardEvent.key and LCL virtual
  codes. Unknown keys stay unknown; user callbacks use the enums above. }
function NyxKeyFromBrowser(const AKey: TNyxText): TNyxKey;
function NyxKeyFromVirtualCode(ACode: Word): TNyxKey;
function NyxOverrideName(AValue: TNyxOverrideMode): TNyxText;
function NyxInputTypeName(AValue: TNyxInputType): TNyxText;
function NyxPart(const AName: TNyxText): TNyxPartRef;
function NyxEvent(const AName: TNyxText): TNyxEventRef;
{ Return a typed named reference for a built-in action. Closed values are checked
  before indexing; wire names remain exactly compatible with existing designs.
  Name/symbol/title helpers serve codec, source admission and shared discovery. }
function NyxSemantic(AEvent: TNyxSemanticEvent): TNyxEventRef;
function NyxSemanticName(AEvent: TNyxSemanticEvent): TNyxText;
function NyxSemanticSymbol(AEvent: TNyxSemanticEvent): TNyxText;
function NyxSemanticTitle(AEvent: TNyxSemanticEvent): TNyxText;
function TryNyxSemantic(const AName: TNyxText; out AEvent: TNyxSemanticEvent): Boolean;
{ Named stream identities retain exact case/Unicode. Refuse blank names, control
  characters, malformed encoding and more than 128 Unicode scalars. Unlike a
  suppressed physical event name, an open named stream cannot be empty. }
function NyxNamedEvent(const AName: TNyxText): TNyxEventRef;
function NyxComponent(const AName: TNyxText): TNyxComponentRef;
{ Capture an exact open control name; existence/encoding/uniqueness are admitted
  with its document operation. This value never retains a descriptor lifetime. }
function NyxControl(const AID: TNyxText): TNyxControlRef;
{ Copy two distinct identity references. Complete one-to-one mapping, occupancy
  and subtree membership are checked atomically by reusable derivation. }
function NyxIdentity(const AFrom, ATo: TNyxControlRef): TNyxIdentityAssignment;
function NyxStyle(const AName: TNyxText): TNyxStyleRef;
function NyxCustomKind(const AName: TNyxText): TNyxKindRef;

implementation

uses
  SysUtils;

const
  CSemanticNames: array[TNyxSemanticEvent] of TNyxText = (
    'activate', 'primary', 'menu', 'search', 'clear', 'submit', 'save',
    'decrement', 'increment', 'select', 'rate', 'create', 'refresh', 'apply',
    'reset', 'previous', 'next', 'home', 'section', 'overview', 'projects',
    'settings', 'edit', 'view-all', 'export', 'add', 'cancel', 'confirm',
    'dismiss', 'undo', 'back', 'step', 'message', 'follow', 'open', 'favorite',
    'reply', 'new');
  CSemanticSymbols: array[TNyxSemanticEvent] of TNyxText = (
    'nseActivate', 'nsePrimary', 'nseMenu', 'nseSearch', 'nseClear', 'nseSubmit',
    'nseSave', 'nseDecrement', 'nseIncrement', 'nseSelect', 'nseRate', 'nseCreate',
    'nseRefresh', 'nseApply', 'nseReset', 'nsePrevious', 'nseNext', 'nseHome',
    'nseSection', 'nseOverview', 'nseProjects', 'nseSettings', 'nseEdit',
    'nseViewAll', 'nseExport', 'nseAdd', 'nseCancel', 'nseConfirm', 'nseDismiss',
    'nseUndo', 'nseBack', 'nseStep', 'nseMessage', 'nseFollow', 'nseOpen',
    'nseFavorite', 'nseReply', 'nseNew');

const
  CKindNames: array[TNyxKind] of TNyxText = (
    'page', 'column', 'row', 'grid', 'panel', 'card', 'group',
    'toolbar', 'scroll', 'tabs', 'tab', 'heading', 'label', 'button', 'link',
    'input', 'memo', 'checkbox', 'switch', 'radio', 'select', 'spin', 'slider',
    'date', 'time', 'color', 'list', 'table', 'tree', 'image', 'avatar',
    'progress', 'badge', 'alert', 'separator', 'spacer', 'code', 'code-editor',
    'design-surface', 'component', 'split-view', 'labeled-button', 'split-button', 'search-field',
    'form-field', 'login-form', 'settings-panel', 'date-range', 'number-stepper',
    'segmented-control', 'rating', 'data-toolbar', 'filter-bar', 'pagination',
    'breadcrumbs', 'sidebar-nav', 'master-detail', 'list-card', 'data-card',
    'kanban-board', 'timeline', 'stat-card', 'metric-grid', 'empty-state',
    'confirmation-dialog', 'notification-card', 'toast', 'wizard-step', 'stepper',
    'profile-card', 'media-card', 'avatar-group', 'comment-thread', 'property-grid',
    'command-bar', 'floating-action-panel', 'slot-override');
  CAttributeNames: array[TNyxAttribute] of TNyxText = (
    'text', 'value', 'placeholder', 'items', 'hint', 'aria-label', 'href', 'src',
    'alt', 'layout', 'padding', 'gap', 'columns', 'width', 'height', 'left', 'top',
    'flex', 'min', 'max', 'enabled', 'visible', 'readonly', 'surface', 'compound',
    'pressed', 'variant', 'action', 'projection-kind', 'mode', 'input-type', 'part',
    'target', 'component', 'emit', 'emit.change', 'option', 'path', 'design-id',
    'split-orientation', 'split-position', 'split-minimum', 'split-maximum',
    'split-resizable', 'drag-source', 'drop-target', 'touch-behavior',
    'flow-wrap', 'cross-alignment', 'justification', 'width-sizing', 'height-sizing');
  CLayoutNames: array[TNyxLayoutMode] of TNyxText = ('column', 'row', 'grid', 'absolute');
  CVariantNames: array[TNyxVariant] of TNyxText =
    ('', 'primary', 'secondary', 'danger', 'success', 'warning', 'ghost');
  CActionNames: array[TNyxAction] of TNyxText =
    ('', 'clear', 'dismiss', 'toggle', 'select', 'increment', 'decrement');
  CTriggerNames: array[TNyxTrigger] of TNyxText =
    ('click', 'change', 'select', 'edit-value', 'after-enter', 'after-exit',
    'key-down', 'key-up', 'before-key-down', 'after-key-down', 'key-press',
    'before-key-press', 'after-key-press', 'before-key-up', 'after-key-up',
    'before-text-input', 'text-input', 'after-text-input', 'double-click',
    'pointer-down', 'pointer-up', 'pointer-move', 'pointer-enter', 'pointer-exit',
    'context-menu', 'named', 'before-wheel', 'wheel', 'after-wheel', 'scroll',
    'scroll-end', 'selection-change', 'before-edit', 'composition-start',
    'composition-update', 'composition-end', 'text-selection-change',
    'pointer-cancel', 'pointer-capture', 'pointer-capture-lost',
    'drag-start', 'drag', 'drag-enter', 'drag-over', 'drag-exit', 'drop', 'drag-end');
  CTriggerTitles: array[TNyxTrigger] of TNyxText =
    ('OnClick', 'OnChange', '', '', 'OnAfterEnter', 'OnAfterExit',
    'OnKeyDown', 'OnKeyUp', 'OnBeforeKeyDown', 'OnAfterKeyDown', 'OnKeyPress',
    'OnBeforeKeyPress', 'OnAfterKeyPress', 'OnBeforeKeyUp', 'OnAfterKeyUp',
    'OnBeforeTextInput', 'OnTextInput', 'OnAfterTextInput', 'OnDoubleClick',
    'OnPointerDown', 'OnPointerUp', 'OnPointerMove', 'OnPointerEnter',
    'OnPointerExit', 'OnContextMenu', 'OnNamed', 'OnBeforeWheel', 'OnWheel',
    'OnAfterWheel', 'OnScroll', 'OnScrollEnd', 'OnSelectionChange', 'OnBeforeEdit',
    'OnCompositionStart', 'OnCompositionUpdate', 'OnCompositionEnd', 'OnTextSelectionChange',
    'OnPointerCancel', 'OnPointerCapture', 'OnPointerCaptureLost',
    'OnDragStart', 'OnDrag', 'OnDragEnter', 'OnDragOver', 'OnDragExit', 'OnDrop', 'OnDragEnd');
  COverrideNames: array[TNyxOverrideMode] of TNyxText =
    ('properties', 'append', 'prepend', 'replace', 'remove');
  CInputTypeNames: array[TNyxInputType] of TNyxText =
    ('text', 'password', 'email', 'number', 'search', 'tel', 'url');

function NyxKindName(AKind: TNyxKind): TNyxText;
begin
  Result := CKindNames[AKind];
end;

function NyxPlatformName(AValue: TNyxPlatform): TNyxText;
begin
  case AValue of
    npfAny: Result := 'any';
    npfBrowser: Result := 'browser';
    npfNativeLCL: Result := 'native-lcl';
  end;
end;

function NyxPlatformSymbol(AValue: TNyxPlatform): TNyxText;
begin
  case AValue of
    npfAny: Result := 'npfAny';
    npfBrowser: Result := 'npfBrowser';
    npfNativeLCL: Result := 'npfNativeLCL';
  end;
end;

function NyxSplitOrientationName(AValue: TNyxSplitOrientation): TNyxText;
begin
  case AValue of
    nsoStacked: Result := 'stacked';
    nsoSideBySide: Result := 'side-by-side';
  end;
end;

function NyxTouchBehaviorName(AValue: TNyxTouchBehavior): TNyxText;
const
  CNames: array[TNyxTouchBehavior] of TNyxText =
    ('auto', 'none', 'pan-x', 'pan-y', 'manipulation');
begin
  Result := CNames[AValue];
end;

function NyxPlatformAttribute(AAttribute: TNyxAttribute): Boolean;
begin
  { A presentation override cannot change tree identity, ownership, state
    domains, reusable/event routing or a scalar default on just one target. }
  Result := AAttribute in [atText, atPlaceholder, atItems, atHint,
    atAccessibleName, atHref, atSource, atAlt, atLayout, atPadding, atGap,
    atColumns, atWidth, atHeight, atLeft, atTop, atFlex, atEnabled, atVisible,
    atReadOnly, atSurface, atPressed, atVariant, atInputType,
    atSplitOrientation, atSplitPosition, atSplitMinimum, atSplitMaximum,
    atSplitResizable, atDragSource, atDropTarget, atTouchBehavior,
    atFlowWrap, atCrossAlignment, atJustification, atWidthSizing, atHeightSizing];
end;

function NyxPlatformKey(APlatform: TNyxPlatform; AAttribute: TNyxAttribute): TNyxText;
begin
  Result := NyxAttributeName(AAttribute);

  if APlatform <> npfAny then
  begin
    Result := '@nyx.' + NyxPlatformName(APlatform) + ':' + Result;
  end;
end;

function TryNyxPlatformKey(const AKey: TNyxText; out APlatform: TNyxPlatform;
  out AAttribute: TNyxAttribute): Boolean;
var
  LPlatform: TNyxPlatform;
  LPrefix: TNyxText;
begin
  APlatform := npfAny;
  AAttribute := atText;
  for LPlatform := npfBrowser to npfNativeLCL do
  begin
    LPrefix := '@nyx.' + NyxPlatformName(LPlatform) + ':';

    if Copy(AKey, 1, Length(LPrefix)) = LPrefix then
    begin
      APlatform := LPlatform;
      Exit(TryNyxAttribute(Copy(AKey, Length(LPrefix) + 1, MaxInt), AAttribute)
        and NyxPlatformAttribute(AAttribute));
    end;
  end;
  Result := False;
end;

function TryNyxKind(const AName: TNyxText; out AKind: TNyxKind): Boolean;
var
  LKind: TNyxKind;
begin
  AKind := nkPage;
  for LKind := Low(TNyxKind) to High(TNyxKind) do
  begin

    if CKindNames[LKind] = AName then
    begin
      AKind := LKind;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxAttributeName(AAttribute: TNyxAttribute): TNyxText;
begin
  Result := CAttributeNames[AAttribute];
end;

function TryNyxAttribute(const AName: TNyxText; out AAttribute: TNyxAttribute): Boolean;
var
  LAttribute: TNyxAttribute;
begin
  AAttribute := atText;
  for LAttribute := Low(TNyxAttribute) to High(TNyxAttribute) do
  begin

    if CAttributeNames[LAttribute] = AName then
    begin
      AAttribute := LAttribute;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxLayoutName(AValue: TNyxLayoutMode): TNyxText;
begin
  Result := CLayoutNames[AValue];
end;

function NyxFlowWrapName(AValue: TNyxFlowWrap): TNyxText;
const
  CNames: array[TNyxFlowWrap] of TNyxText = ('auto', 'nowrap', 'wrap');
begin
  Result := CNames[AValue];
end;

function NyxCrossAlignmentName(AValue: TNyxCrossAlignment): TNyxText;
const
  CNames: array[TNyxCrossAlignment] of TNyxText =
    ('auto', 'start', 'center', 'end', 'stretch');
begin
  Result := CNames[AValue];
end;

function NyxJustificationName(AValue: TNyxJustification): TNyxText;
const
  CNames: array[TNyxJustification] of TNyxText =
    ('start', 'center', 'end', 'space-between', 'space-around', 'space-evenly');
begin
  Result := CNames[AValue];
end;

function NyxSizingName(AValue: TNyxSizing): TNyxText;
const
  CNames: array[TNyxSizing] of TNyxText = ('auto', 'content', 'fill');
begin
  Result := CNames[AValue];
end;

function NyxVariantName(AValue: TNyxVariant): TNyxText;
begin
  Result := CVariantNames[AValue];
end;

function NyxActionName(AValue: TNyxAction): TNyxText;
begin
  Result := CActionNames[AValue];
end;

function TryNyxAction(const AName: TNyxText; out AAction: TNyxAction): Boolean;
var
  LAction: TNyxAction;
begin
  AAction := naNone;
  for LAction := Low(TNyxAction) to High(TNyxAction) do
  begin

    if CActionNames[LAction] = AName then
    begin
      AAction := LAction;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxTriggerName(ATrigger: TNyxTrigger): TNyxText;
begin
  Result := CTriggerNames[ATrigger];
end;

function NyxTriggerTitle(ATrigger: TNyxTrigger): TNyxText;
begin
  Result := CTriggerTitles[ATrigger];
end;

function NyxTriggerSymbol(ATrigger: TNyxTrigger): TNyxText;
begin
  case ATrigger of
    ntDesignSelect:
      begin
        Result := 'ntDesignSelect';
      end;
    ntDesignValue:
      begin
        Result := 'ntDesignValue';
      end;
  else
    Result := 'nt' + Copy(NyxTriggerTitle(ATrigger), 3, MaxInt);
  end;
end;

function TryNyxTrigger(const AName: TNyxText; out ATrigger: TNyxTrigger): Boolean;
var
  LTrigger: TNyxTrigger;
begin
  for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
  begin

    if NyxTriggerName(LTrigger) = AName then
    begin
      ATrigger := LTrigger;
      Exit(True);
    end;
  end;
  ATrigger := ntClick;
  Result := False;
end;

function NyxIsKeyboardTrigger(ATrigger: TNyxTrigger): Boolean;
begin
  Result := ATrigger in [ntKeyDown, ntKeyUp, ntBeforeKeyDown, ntAfterKeyDown,
    ntKeyPress, ntBeforeKeyPress, ntAfterKeyPress, ntBeforeKeyUp, ntAfterKeyUp];
end;

function NyxIsInputTrigger(ATrigger: TNyxTrigger): Boolean;
begin
  Result := ATrigger in [ntKeyDown, ntKeyUp, ntBeforeKeyDown, ntKeyPress,
    ntBeforeKeyPress, ntBeforeKeyUp, ntBeforeTextInput, ntBeforeEdit, ntContextMenu,
    ntBeforeWheel, ntWheel];
end;

function NyxIsRuntimeTrigger(ATrigger: TNyxTrigger): Boolean;
begin
  { Named transport requires an explicit reference and producer declaration;
    it is not a closed physical family usable with On(Trigger) or Contract.On. }
  Result := not (ATrigger in [ntDesignSelect, ntDesignValue, ntNamed]);
end;

function NyxKeyStroke(AKey: TNyxKey; AModifiers: TNyxKeyModifiers;
  ARepeating: Boolean): TNyxKeyStroke;
begin
  Result.FKey := AKey;
  Result.FModifiers := AModifiers;
  Result.FRepeating := ARepeating;
end;

function TNyxKeyStroke.Matches(AKey: TNyxKey; AModifiers: TNyxKeyModifiers;
  AAllowRepeat: Boolean): Boolean;
begin
  Result := (FKey = AKey) and (FModifiers = AModifiers) and
    (AAllowRepeat or not FRepeating);
end;

function NyxKeyFromVirtualCode(ACode: Word): TNyxKey;
begin
  Result := nkUnknownKey;
  case ACode of
    $08: Result := nkBackspaceKey;
    $09: Result := nkTabKey;
    $0D: Result := nkEnterKey;
    $10, $A0, $A1: Result := nkShiftKey;
    $11, $A2, $A3: Result := nkControlKey;
    $12, $A4, $A5: Result := nkAltKey;
    $1B: Result := nkEscapeKey;
    $20: Result := nkSpaceKey;
    $21: Result := nkPageUpKey;
    $22: Result := nkPageDownKey;
    $23: Result := nkEndKey;
    $24: Result := nkHomeKey;
    $25: Result := nkLeftKey;
    $26: Result := nkUpKey;
    $27: Result := nkRightKey;
    $28: Result := nkDownKey;
    $2D: Result := nkInsertKey;
    $2E: Result := nkDeleteKey;
    $5B, $5C: Result := nkMetaKey;
    $30..$39: Result := TNyxKey(Ord(nk0Key) + ACode - $30);
    $41..$5A: Result := TNyxKey(Ord(nkAKey) + ACode - $41);
    $60..$69: Result := TNyxKey(Ord(nk0Key) + ACode - $60);
    $70..$87: Result := TNyxKey(Ord(nkF1Key) + ACode - $70);
  end;
end;

function NyxKeyFromBrowser(const AKey: TNyxText): TNyxKey;
const
  CNames: array[0..18] of TNyxText = ('Backspace', 'Tab', 'Enter', 'Escape',
    'PageUp', 'PageDown', 'End', 'Home', 'ArrowLeft', 'ArrowUp', 'ArrowRight',
    'ArrowDown', 'Insert', 'Delete', 'Shift', 'Control', 'Alt', 'AltGraph', 'Meta');
  CKeys: array[0..18] of TNyxKey = (nkBackspaceKey, nkTabKey, nkEnterKey,
    nkEscapeKey, nkPageUpKey, nkPageDownKey, nkEndKey, nkHomeKey, nkLeftKey,
    nkUpKey, nkRightKey, nkDownKey, nkInsertKey, nkDeleteKey, nkShiftKey,
    nkControlKey, nkAltKey, nkAltKey, nkMetaKey);
var
  LCode: Integer;
  LFunction: Integer;
  LIndex: Integer;
begin
  Result := nkUnknownKey;

  if Length(AKey) = 1 then
  begin
    LCode := Ord(AKey[1]);

    if (LCode >= Ord('a')) and (LCode <= Ord('z')) then
    begin
      LCode := LCode - Ord('a') + Ord('A');
    end;

    if ((LCode >= Ord('A')) and (LCode <= Ord('Z'))) or
      ((LCode >= Ord('0')) and (LCode <= Ord('9'))) or (LCode = Ord(' ')) then
    begin
      Result := NyxKeyFromVirtualCode(Word(LCode));
    end;
    Exit;
  end;
  for LIndex := Low(CNames) to High(CNames) do
  begin

    if AKey = CNames[LIndex] then
    begin
      Exit(CKeys[LIndex]);
    end;
  end;

  if (Length(AKey) >= 2) and (AKey[1] = 'F') then
  begin
    LFunction := StrToIntDef(Copy(AKey, 2, Length(AKey)), 0);

    if (LFunction >= 1) and (LFunction <= 24) and
      (AKey = 'F' + IntToStr(LFunction)) then
    begin
      Result := TNyxKey(Ord(nkF1Key) + LFunction - 1);
    end;
  end;
end;

function NyxOverrideName(AValue: TNyxOverrideMode): TNyxText;
begin
  Result := COverrideNames[AValue];
end;

function NyxInputTypeName(AValue: TNyxInputType): TNyxText;
begin
  Result := CInputTypeNames[AValue];
end;

function NyxPart(const AName: TNyxText): TNyxPartRef;
begin
  Result.Name := AName;
end;

function NyxEvent(const AName: TNyxText): TNyxEventRef;
begin
  Result.Name := AName;
end;

function NyxSemanticName(AEvent: TNyxSemanticEvent): TNyxText;
begin

  if (Ord(AEvent) < Ord(Low(TNyxSemanticEvent))) or
    (Ord(AEvent) > Ord(High(TNyxSemanticEvent))) then
  begin
    raise EArgumentException.Create('Unknown built-in semantic event');
  end;
  Result := CSemanticNames[AEvent];
end;

function NyxSemantic(AEvent: TNyxSemanticEvent): TNyxEventRef;
begin
  Result := NyxNamedEvent(NyxSemanticName(AEvent));
end;

function NyxSemanticSymbol(AEvent: TNyxSemanticEvent): TNyxText;
begin
  { Validate through the same checked lookup before indexing the symbol table. }
  NyxSemanticName(AEvent);
  Result := CSemanticSymbols[AEvent];
end;

function NyxSemanticTitle(AEvent: TNyxSemanticEvent): TNyxText;
begin
  Result := 'On' + Copy(NyxSemanticSymbol(AEvent), 4, MaxInt);
end;

function TryNyxSemantic(const AName: TNyxText; out AEvent: TNyxSemanticEvent): Boolean;
var
  LEvent: TNyxSemanticEvent;
begin
  AEvent := Low(TNyxSemanticEvent);
  for LEvent := Low(TNyxSemanticEvent) to High(TNyxSemanticEvent) do
  begin

    if AName = CSemanticNames[LEvent] then
    begin
      AEvent := LEvent;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxNamedEvent(const AName: TNyxText): TNyxEventRef;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
  LContent: Boolean;
begin
  LIndex := 1;
  LCount := 0;
  LContent := False;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) then
    begin
      raise EArgumentException.Create('Named event contains malformed Unicode');
    end;

    if (LScalar < 32) or ((LScalar >= $7f) and (LScalar <= $9f)) or
      (LScalar = $2028) or (LScalar = $2029) then
    begin
      raise EArgumentException.Create('Named event contains a control or line separator');
    end;
    Inc(LCount);

    if LCount > 128 then
    begin
      raise EArgumentException.Create('Named event exceeds 128 Unicode scalars');
    end;
    LContent := LContent or (LScalar > 32);
  end;

  if not LContent then
  begin
    raise EArgumentException.Create('Named event requires content');
  end;
  Result := NyxEvent(AName);
end;

function NyxComponent(const AName: TNyxText): TNyxComponentRef;
begin
  Result.Name := AName;
end;

function NyxControl(const AID: TNyxText): TNyxControlRef;
begin
  Result.ID := AID;
end;

function NyxIdentity(const AFrom, ATo: TNyxControlRef): TNyxIdentityAssignment;
begin
  Result.Source := AFrom;
  Result.Destination := ATo;
end;

function NyxStyle(const AName: TNyxText): TNyxStyleRef;
begin
  Result.Name := AName;
end;

function NyxCustomKind(const AName: TNyxText): TNyxKindRef;
begin
  Result.Name := AName;
end;

end.
