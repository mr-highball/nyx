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

{ nyx — browser library-text column-fit metrics and copied style witnesses. }
unit nyx.collections.arrangement.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses
  Web, nyx.theme, nyx.collections.mount, nyx.collections.arrangement.presenter,
  nyx.popover.browser, nyx.collections.view, nyx.collections.arrangement.controls;

type
  { Borrowed synchronous input only. Supply the exact canonical column parts,
    never an editor or a neighboring field's representative. Body parts may be
    absent only for an empty sample. A hidden navigation label is valid: its
    typography, not its draft/editor value, describes accepted table text. Supply
    BodyEditor when that cell also owns a dormant/active standard input; only its
    identity is inspected and its value is never read.
    The caller owns target identity/lifetime and uniform library row styling. }
  TNyxBrowserColumnFitSurface = record
    HeaderCell: TJSHTMLElement;
    HeaderLabel: TJSHTMLElement;
    SortButton: TJSHTMLElement;
    FilterButton: TJSHTMLElement;
    BodyCell: TJSHTMLElement;
    BodyLabel: TJSHTMLElement;
    BodyEditor: TJSHTMLElement;
  end;

  { A managed metric owns copied strings/styles and the inert model sample;
    it retains no DOM node, document, FontFaceSet, mount or callback. Matches
    recaptures styles/readiness and remeasures copied text on the UI thread,
    catching a changed face under an unchanged CSS font string. It is NOT a
    callback-free final publication fence or an input/form authority. }
  INyxBrowserColumnFitMeasurement = interface(IInterface)
    ['{635CA841-B18A-4A76-A8FB-0A0E62B1E499}']
    function GetMeasurement: INyxColumnFitMeasurement;
    function Matches(const ASurface: TNyxBrowserColumnFitSurface): Boolean;
    property Measurement: INyxColumnFitMeasurement read GetMeasurement;
  end;

{ Current-document, horizontal, uniform default-library text only. Distinct
  header/button/body computed styles and actual chrome are measured with an
  offscreen ordinary composition; no IDs, custom elements, events or live content are
  cloned. Font readiness is synchronous and text-aware. Missing representatives,
  generated/custom paint or unsupported geometry refuse instead of guessing.
  CSS pixels already are logical pixels; no DPR/ancestor scale is multiplied.
  Mount/input/IME/generation/form and prepared-publication fences remain required. }
function MeasureNyxBrowserColumnFit(const ASample: INyxCollectionColumnSample;
  const ASurface: TNyxBrowserColumnFitSurface): INyxBrowserColumnFitMeasurement;

type
  { Target observation retains the exact ordinary Nyx popup. Its renderer and
    controls are borrowed from that popup and must never be freed externally. }
  INyxBrowserColumnArrangementPresenter = interface(INyxColumnArrangementPresenter)
    ['{71BA65ED-39D7-4752-B05B-A1D42ED8B978}']
    function GetPresentation: INyxBrowserPopover;
    property Presentation: INyxBrowserPopover read GetPresentation;
  end;

{ Borrow the exact table anchor and its theme until Revoke. The input lease is
  owner-free; it fences every actual popup generation without retaining a mount.
  Revoke the factory before the anchor or theme can be destroyed. }
function NewNyxBrowserColumnArrangementFactory(AAnchor: TJSHTMLElement;
  ATheme: TNyxTheme; const AInput: INyxCollectionInputLifetime):
  INyxColumnArrangementPresenterFactory;

implementation

uses
SysUtils, Math, JS, nyx.text, nyx.collections, nyx.model, nyx.root.types,
  nyx.events, nyx.state, nyx.popover, nyx.popover.types,
  nyx.collections.arrangement.workbench, nyx.types, nyx.render.browser;

type
  { One weak table target, retained only as inert retirement facts by its forms.
    No form, mount or renderer is owned here, so the popup cannot form a cycle. }
  TBrowserArrangementFactory = class(TInterfacedObject, INyxColumnArrangementPresenterFactory)
  private
    FActive: Boolean;
    FAnchor: TJSHTMLElement;
    FTheme: TNyxTheme;
    FInput: INyxCollectionInputLifetime;
  public
    constructor Create(AAnchor: TJSHTMLElement; ATheme: TNyxTheme;
      const AInput: INyxCollectionInputLifetime);
    destructor Destroy; override;
    function CreatePresenter(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxColumnArrangementPresenter;
    procedure Revoke;
  end;

  { The actual popover owns the cloned form document, state and live renderer.
    The forwarding event surface retains the real router and original input,
    while captured generations retain no presenter or physical widget. }
  TBrowserArrangementForm = class(TInterfacedObject, INyxColumnArrangementPresenter,
    INyxBrowserColumnArrangementPresenter)
  private
    FTarget: TBrowserArrangementFactory;
    FLease: INyxColumnArrangementPresenterFactory;
    FHost: INyxBrowserPopover;
    FEvents: INyxEvents;
    FConnected: Boolean;
  public
    constructor Create(ATarget: TBrowserArrangementFactory; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef);
    destructor Destroy; override;
    function GetEvents: INyxEvents;
    function GetConnected: Boolean;
    function GetOpen: Boolean;
    function GetPresentation: INyxBrowserPopover;
    function CaptureRoot: TNyxNode;
    procedure SetStatus(const AText: TNyxText);
    procedure Show(const AOptions: TNyxPopoverOptions);
    procedure Close;
    procedure Disconnect;
  end;

constructor TBrowserArrangementFactory.Create(AAnchor: TJSHTMLElement;
  ATheme: TNyxTheme; const AInput: INyxCollectionInputLifetime);
begin
  inherited Create;

  if (AAnchor = nil) or (AInput = nil) then
  begin
    raise ENyxCollection.Create('Column management requires its exact table and input lifetime');
  end;
  FAnchor := AAnchor;
  FTheme := ATheme;
  FInput := AInput;
  FActive := True;
end;

destructor TBrowserArrangementFactory.Destroy;
begin
  Revoke;
  inherited Destroy;
end;

procedure TBrowserArrangementFactory.Revoke;
begin
  FActive := False;
  FAnchor := nil;
  FTheme := nil;
  FInput := nil;
end;

function TBrowserArrangementFactory.CreatePresenter(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxColumnArrangementPresenter;
var
  LKeepAlive: INyxColumnArrangementPresenterFactory;
begin
  LKeepAlive := Self;

  if not FActive or (FAnchor = nil) or (FInput = nil) then
  begin
    raise ENyxCollection.Create('The column-management target has retired');
  end;
  Result := TBrowserArrangementForm.Create(Self, ADocument, ARoot);
end;

constructor TBrowserArrangementForm.Create(ATarget: TBrowserArrangementFactory;
  ADocument: TNyxDocument; const ARoot: TNyxRootRef);
var
  LAnchor: TJSHTMLElement;
  LTableAnchor: TJSHTMLElement;
  LInput: INyxCollectionInputLifetime;
  LHost: INyxBrowserPopover;
  LEvents: INyxEvents;
  LAdmission: INyxPopoverOpenAdmission;
begin
  inherited Create;
  FTarget := ATarget;
  FLease := ATarget;
  LTableAnchor := ATarget.FAnchor;
  LInput := ATarget.FInput;
  LAnchor := TJSHTMLElement(LTableAnchor.querySelector('[data-nyx-columns-button]'));

  if LAnchor = nil then
  begin
    LAnchor := LTableAnchor;
  end;
  { A direct host may supply its own anchor. Mounted tables use their persistent
    launcher, so popup positioning and return focus remain useful when all
    columns are hidden. The factory still belongs to the original whole table. }
  if not ATarget.FActive or (ATarget.FAnchor <> LTableAnchor) or
    (ATarget.FInput <> LInput) then
  begin
    raise ENyxCollection.Create('The column launcher retired during anchor lookup');
  end;
  LHost := NewNyxBrowserPopover(LAnchor, ADocument, ARoot, ATarget.FTheme);

  if not Supports(LHost, INyxPopoverOpenAdmission, LAdmission) then
  begin
    raise ENyxCollection.Create('Column management requires actual popup open admission');
  end;
  LEvents := NewNyxArrangementQueryEvents(LHost.Events, LInput, LAdmission);

  if not ATarget.FActive or (ATarget.FAnchor <> LTableAnchor) or
    (ATarget.FInput <> LInput) then
  begin
    raise ENyxCollection.Create('The table retired while creating column management');
  end;
  FHost := LHost;
  FEvents := LEvents;
  FConnected := True;
end;

destructor TBrowserArrangementForm.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TBrowserArrangementForm.GetConnected: Boolean;
begin
  Result := FConnected and (FTarget <> nil) and FTarget.FActive and
    (FHost <> nil) and (FEvents <> nil);
end;

function TBrowserArrangementForm.GetOpen: Boolean;
begin
  Result := GetConnected and FHost.IsOpen;
end;

function TBrowserArrangementForm.GetEvents: INyxEvents;
begin
  Result := nil;

  if GetConnected then
  begin
    Result := FEvents;
  end;
end;

function TBrowserArrangementForm.GetPresentation: INyxBrowserPopover;
begin
  Result := FHost;
end;

function TBrowserArrangementForm.CaptureRoot: TNyxNode;
begin
  Result := nil;

  if GetConnected then
  begin
    Result := FHost.Renderer.Root;
  end;
end;

procedure TBrowserArrangementForm.SetStatus(const AText: TNyxText);
var
  LKeepAlive: INyxColumnArrangementPresenter;
  LHost: INyxBrowserPopover;
begin
  LKeepAlive := Self;

  if not GetConnected then
  begin
    Exit;
  end;
  LHost := FHost;
  LHost.State.SetValue(NyxTextState(NyxColumnArrangementStatusState), AText);
end;

procedure TBrowserArrangementForm.Show(const AOptions: TNyxPopoverOptions);
var
  LKeepAlive: INyxColumnArrangementPresenter;
  LHost: INyxBrowserPopover;
  LRoot: TNyxNode;
  LFocus: TJSHTMLElement;
begin
  LKeepAlive := Self;

  if not GetConnected then
  begin
    raise ENyxCollection.Create('The column-management form has retired');
  end;
  LHost := FHost;
  { Accepted replacement may have restored the former invoker's focus. Reuse
    this actual mounted draft, honoring explicit focus without reopening or
    recapturing another form generation. No borrowed control is read afterward. }

  if LHost.IsOpen then
  begin
    AOptions.Validate;

    if AOptions.InitialFocus.Name <> '' then
    begin
      LRoot := LHost.Renderer.Root;

      if LRoot = nil then
      begin
        raise ENyxCollection.Create('Column management has no live focus root');
      end;
      LFocus := LHost.Renderer.FocusFor(
        LRoot.Part(AOptions.InitialFocus).ID, niRuntime);

      if LFocus = nil then
      begin
        raise ENyxCollection.Create('Column-manager focus has no live control');
      end;
      NyxFocusWithoutScroll(LFocus);
    end;
    Exit;
  end;
  LHost.Open(AOptions);
end;

procedure TBrowserArrangementForm.Close;
var
  LKeepAlive: INyxColumnArrangementPresenter;
  LHost: INyxBrowserPopover;
begin
  LKeepAlive := Self;
  LHost := FHost;

  if LHost <> nil then
  begin
    LHost.Close;
  end;
end;

procedure TBrowserArrangementForm.Disconnect;
var
  LHost: INyxBrowserPopover;
  LEvents: INyxEvents;
  LFactory: INyxColumnArrangementPresenterFactory;
begin
  FConnected := False;
  LHost := FHost;
  LEvents := FEvents;
  LFactory := FLease;
  FHost := nil;
  FEvents := nil;
  FTarget := nil;
  FLease := nil;
  { Logical detachment precedes all focus/Close callbacks. These local owners
    retain the exact former router, factory and host until cleanup unwinds. }

  if LHost <> nil then
  begin
    LHost.Close;
  end;
end;

function NewNyxBrowserColumnArrangementFactory(AAnchor: TJSHTMLElement;
  ATheme: TNyxTheme; const AInput: INyxCollectionInputLifetime):
  INyxColumnArrangementPresenterFactory;
begin
  Result := TBrowserArrangementFactory.Create(AAnchor, ATheme, AInput);
end;

type
  { Borrowed browser FontFaceSet bridge for document.fonts, never an owned font
    collection. Availability is checked before measuring; check(font,text) must
    witness the actual Unicode-range glyphs, not merely a family's presence. }
  TFitFontSet = class external name 'FontFaceSet'(TJSObject)
  private
    FStatus: String; external name 'status';
  public
    function Check(const AFont, AText: String): Boolean; external name 'check';
    property Status: String read FStatus;
  end;
  { Borrowed global Window bridge for getComputedStyle's pseudo-element argument.
    Style reads stay on the UI thread; no host window is constructed or retained. }
  TPseudoWindow = class external name 'Window'(TJSWindow)
    function ReadStyle(AElement: TJSElement; const APseudo: String): TJSCSSStyleDeclaration;
      external name 'getComputedStyle';
  end;
  { Complete closed set of text-layout facts witnessed by one measurement.
    A change to any participating property invalidates that detached result. }
  TFitTextProperty = (ftpFont, ftpFamily, ftpSize, ftpStyle, ftpWeight,
    ftpStretch, ftpVariant, ftpKerning, ftpFeatures, ftpVariations, ftpOptical,
    ftpSizeAdjust, ftpSynthesis, ftpRendering, ftpLineHeight, ftpWhiteSpace,
    ftpCollapse, ftpWrapMode, ftpTabSize, ftpLetterSpacing, ftpWordSpacing,
    ftpTransform, ftpDirection, ftpBidi, ftpWordBreak, ftpOverflowWrap,
    ftpHyphens, ftpIndent, ftpWritingMode, ftpOrientation);
  { Owned copied CSS values indexed by the typed measurement-property set. }
  TFitTextValues = array[TFitTextProperty] of String;
  { Existing header faces whose actual painted order contributes to width. }
  TFitHeaderPart = (fhpLabel, fhpSort, fhpFilter);
  { Bounded order of the three known faces, including hidden participants. }
  TFitHeaderOrder = array[0..2] of TFitHeaderPart;

  { All fields are detached values. Element display controls header composition;
    body label display may be hidden while its separate editor is active. Insets
    and minima are CSS logical pixels; old allocated column widths are excluded. }
  TFitProfile = record
    Values: TFitTextValues;
    Lang: String;
    Display: String;
    BoxSizing: String;
    Position: String;
    PaddingLeft: Double;
    PaddingRight: Double;
    BorderLeft: Double;
    BorderRight: Double;
    MarginLeft: Double;
    MarginRight: Double;
    Insets: Double;
    Margins: Double;
    Minimum: Double;
  end;
  { Copied typography, chrome and accepted text for one exact mounted sample.
    No DOM element or live model is retained across measurement validation. }
  TFitScene = record
    HeaderCell: TFitProfile;
    HeaderLabel: TFitProfile;
    SortButton: TFitProfile;
    FilterButton: TFitProfile;
    BodyCell: TFitProfile;
    BodyLabel: TFitProfile;
    HeaderText: TNyxText;
    SortText: TNyxText;
    FilterText: TNyxText;
    Order: TFitHeaderOrder;
    HasBody: Boolean;
  end;
  { Copied accepted sample strings are acquired before observing UI styles.
    There are no model/interface getters after that observation or in Matches.
    The ephemeral probe is confined to MeasureScene and always removed. }
  TBrowserColumnFitMeasurement = class(TInterfacedObject, INyxBrowserColumnFitMeasurement)
  private
    FScene: TFitScene;
    FTexts: array of TNyxText;
    FMeasurement: INyxColumnFitMeasurement;
    function MeasureScene(const AScene: TFitScene): Double;
  public
    procedure Initialize(const ASample: INyxCollectionColumnSample;
      const ASurface: TNyxBrowserColumnFitSurface);
    function GetMeasurement: INyxColumnFitMeasurement;
    function Matches(const ASurface: TNyxBrowserColumnFitSurface): Boolean;
  end;

const
  FitTextProperties: array[TFitTextProperty] of String = (
    'font', 'font-family', 'font-size', 'font-style', 'font-weight',
    'font-stretch', 'font-variant', 'font-kerning', 'font-feature-settings',
    'font-variation-settings', 'font-optical-sizing', 'font-size-adjust',
    'font-synthesis', 'text-rendering', 'line-height', 'white-space',
    'white-space-collapse', 'text-wrap-mode', 'tab-size', 'letter-spacing',
    'word-spacing', 'text-transform', 'direction', 'unicode-bidi',
    'word-break', 'overflow-wrap', 'hyphens', 'text-indent', 'writing-mode',
    'text-orientation');
  CResizeAttribute = 'data-nyx-column-resize';
  CLoaded = 'loaded';

function Pixels(const AValue: String): Double;
var
  LValue: String;
begin
  LValue := Trim(AValue);

  if LValue = '0' then
  begin
    Exit(0);
  end;

  if (Length(LValue) < 3) or (Copy(LValue, Length(LValue) - 1, 2) <> 'px') then
  begin
    raise ENyxCollection.Create('Browser column fit requires resolved pixel geometry');
  end;
  Result := parseFloat(Copy(LValue, 1, Length(LValue) - 2));

  if IsNan(Result) or IsInfinite(Result) then
  begin
    raise ENyxCollection.Create('Browser column fit geometry is not finite');
  end;
end;

function EffectiveLang(AElement: TJSHTMLElement): String;
var
  LOwner: TJSHTMLElement;
begin
  LOwner := TJSHTMLElement(AElement.closest('[lang]'));
  Result := '';

  if LOwner <> nil then
  begin
    Result := LOwner.getAttribute('lang');
  end;
end;

procedure RequireNoGeneratedContent(AElement: TJSHTMLElement);
var
  LContent: String;
  LPseudo: Integer;
begin
  for LPseudo := 0 to 1 do
  begin

    if LPseudo = 0 then
    begin
      LContent := TPseudoWindow(window).ReadStyle(AElement, '::before').getPropertyValue('content');
    end
    else
    begin
      LContent := TPseudoWindow(window).ReadStyle(AElement, '::after').getPropertyValue('content');
    end;

    if (LContent <> 'none') and (LContent <> 'normal') then
    begin
      raise ENyxCollection.Create('Generated column content needs its own fit metric');
    end;
  end;
end;

function ReadProfile(AElement: TJSHTMLElement; ATextPart: Boolean): TFitProfile;
var
  LStyle: TJSCSSStyleDeclaration;
  LProperty: TFitTextProperty;
  LMinimum: String;

  function ZeroOffset(const AName: String): Boolean;
  var
    LOffset: String;
  begin
    LOffset := LStyle.getPropertyValue(AName);

    if LOffset = 'auto' then
    begin
      Exit(True);
    end;
    Result := Pixels(LOffset) = 0;
  end;

begin
  Result := Default(TFitProfile);
  LStyle := window.getComputedStyle(AElement);
  for LProperty := Low(TFitTextProperty) to High(TFitTextProperty) do
  begin
    Result.Values[LProperty] := LStyle.getPropertyValue(FitTextProperties[LProperty]);
  end;
  Result.Lang := EffectiveLang(AElement);
  Result.Display := LStyle.getPropertyValue('display');
  Result.BoxSizing := LStyle.getPropertyValue('box-sizing');
  Result.Position := LStyle.getPropertyValue('position');
  Result.PaddingLeft := Pixels(LStyle.getPropertyValue('padding-left'));
  Result.PaddingRight := Pixels(LStyle.getPropertyValue('padding-right'));
  Result.BorderLeft := Pixels(LStyle.getPropertyValue('border-left-width'));
  Result.BorderRight := Pixels(LStyle.getPropertyValue('border-right-width'));
  Result.MarginLeft := Pixels(LStyle.getPropertyValue('margin-left'));
  Result.MarginRight := Pixels(LStyle.getPropertyValue('margin-right'));
  Result.Insets := Result.PaddingLeft + Result.PaddingRight +
    Result.BorderLeft + Result.BorderRight;
  Result.Margins := Result.MarginLeft + Result.MarginRight;
  LMinimum := LStyle.getPropertyValue('min-width');

  if (LMinimum <> '') and (LMinimum <> 'auto') then
  begin
    Result.Minimum := Pixels(LMinimum);
  end;
  RequireNoGeneratedContent(AElement);

  if (Result.Values[ftpWritingMode] <> 'horizontal-tb') or
    (LStyle.getPropertyValue('transform') <> 'none') or
    (LStyle.getPropertyValue('text-shadow') <> 'none') or
    (LStyle.getPropertyValue('float') <> 'none') or
    ((LStyle.getPropertyValue('zoom') <> '') and
      (LStyle.getPropertyValue('zoom') <> '1') and
      (LStyle.getPropertyValue('zoom') <> 'normal')) or
    (Pixels(Result.Values[ftpIndent]) <> 0) then
  begin
    raise ENyxCollection.Create('This column text style needs its own fit metric');
  end;

  if ATextPart and (AElement.children.length <> 0) then
  begin
    raise ENyxCollection.Create('Custom column text children need their own fit metric');
  end;

  if ATextPart and (Result.Position <> 'static') and (Result.Position <> 'relative') then
  begin
    raise ENyxCollection.Create('Positioned column text needs its own fit metric');
  end;

  if ATextPart and (Result.Position = 'relative') and
    (not ZeroOffset('left') or not ZeroOffset('right') or
      not ZeroOffset('top') or not ZeroOffset('bottom')) then
  begin
    raise ENyxCollection.Create('Offset column text needs its own fit metric');
  end;

  if (Result.Insets < 0) or (Result.Minimum < 0) or
    (Result.MarginLeft < 0) or (Result.MarginRight < 0) then
  begin
    raise ENyxCollection.Create('Browser column fit needs nonnegative box geometry');
  end;
end;

procedure RequirePart(AElement, AParent: TJSHTMLElement; const ATag: String);
begin

  if (AElement = nil) or (AElement.tagName <> ATag) or
    (AElement.ownerDocument <> document) or not document.body.contains(AElement) or
    ((AParent <> nil) and (AElement.parentElement <> AParent)) then
  begin
    raise ENyxCollection.Create('Browser column fit requires its exact mounted parts');
  end;
end;

function ReadScene(const ASurface: TNyxBrowserColumnFitSurface;
  AHasBody: Boolean): TFitScene;
var
  LChild: TJSNode;
  LIndex: Integer;
  LResize: TJSHTMLElement;
  LResizeStyle: TJSCSSStyleDeclaration;
  LResizeWidth, LEndPadding: Double;
  LTable: TJSElement;
  LPart: Integer;
begin
  Result := Default(TFitScene);
  RequirePart(ASurface.HeaderCell, nil, 'TH');
  RequirePart(ASurface.HeaderLabel, ASurface.HeaderCell, 'SPAN');
  RequirePart(ASurface.SortButton, ASurface.HeaderCell, 'BUTTON');
  RequirePart(ASurface.FilterButton, ASurface.HeaderCell, 'BUTTON');

  if ASurface.SortButton = ASurface.FilterButton then
  begin
    raise ENyxCollection.Create('Browser header parts must have distinct identities');
  end;
  LTable := ASurface.HeaderCell.closest('table');

  if LTable = nil then
  begin
    raise ENyxCollection.Create('Browser column fit requires its actual mounted table');
  end;
  LResize := nil;
  LPart := 0;
  for LIndex := 0 to ASurface.HeaderCell.childNodes.length - 1 do
  begin
    LChild := ASurface.HeaderCell.childNodes[LIndex];

    if (LChild = ASurface.HeaderLabel) or (LChild = ASurface.SortButton) or
      (LChild = ASurface.FilterButton) then
    begin

      if LChild = ASurface.HeaderLabel then
      begin
        Result.Order[LPart] := fhpLabel;
      end
      else if LChild = ASurface.SortButton then
      begin
        Result.Order[LPart] := fhpSort;
      end
      else
      begin
        Result.Order[LPart] := fhpFilter;
      end;
      Inc(LPart);
      Continue;
    end;

    if (LChild is TJSHTMLElement) and (TJSHTMLElement(LChild).tagName = 'DIV') and
      TJSHTMLElement(LChild).hasAttribute(CResizeAttribute) then
    begin

      if LResize <> nil then
      begin
        raise ENyxCollection.Create('Column resize chrome is ambiguous');
      end;
      LResize := TJSHTMLElement(LChild);
      Continue;
    end;
    raise ENyxCollection.Create('Custom header composition needs its own fit metric');
  end;

  if LPart <> Length(Result.Order) then
  begin
    raise ENyxCollection.Create('Browser column fit requires its complete header composition');
  end;
  Result.HeaderCell := ReadProfile(ASurface.HeaderCell, False);

  if (Result.HeaderCell.Display <> 'table-cell') and
    (Result.HeaderCell.Display <> 'none') then
  begin
    raise ENyxCollection.Create('Custom header layout needs its own fit metric');
  end;
  { The grip is absolute. Existing inline-end padding normally reserves it;
    account for a wider themed grip without adding its ordinary width twice. }

  if LResize <> nil then
  begin
    LResizeStyle := window.getComputedStyle(LResize);

    if LResizeStyle.getPropertyValue('display') <> 'none' then
    begin

      if (LResize.children.length <> 0) or
        (LResizeStyle.getPropertyValue('position') <> 'absolute') or
        (Pixels(LResizeStyle.getPropertyValue('inset-inline-end')) <> 0) then
      begin
        raise ENyxCollection.Create('Custom resize chrome needs its own fit metric');
      end;
      RequireNoGeneratedContent(LResize);
      LResizeWidth := Pixels(LResizeStyle.getPropertyValue('width'));

      if LResizeStyle.getPropertyValue('box-sizing') <> 'border-box' then
      begin
        LResizeWidth := LResizeWidth + Pixels(LResizeStyle.getPropertyValue('padding-left')) +
          Pixels(LResizeStyle.getPropertyValue('padding-right')) +
          Pixels(LResizeStyle.getPropertyValue('border-left-width')) +
          Pixels(LResizeStyle.getPropertyValue('border-right-width'));
      end;
      LResizeWidth := LResizeWidth + Pixels(LResizeStyle.getPropertyValue('margin-left')) +
        Pixels(LResizeStyle.getPropertyValue('margin-right'));
      LEndPadding := Pixels(window.getComputedStyle(ASurface.HeaderCell)
        .getPropertyValue('padding-inline-end'));
      Result.HeaderCell.Insets := Result.HeaderCell.Insets + Max(0, LResizeWidth - LEndPadding);
    end;
  end;
  Result.HeaderLabel := ReadProfile(ASurface.HeaderLabel, True);
  Result.SortButton := ReadProfile(ASurface.SortButton, True);
  Result.FilterButton := ReadProfile(ASurface.FilterButton, True);
  Result.HeaderText := ASurface.HeaderLabel.textContent;
  Result.SortText := ASurface.SortButton.textContent;
  Result.FilterText := ASurface.FilterButton.textContent;
  Result.HasBody := AHasBody;

  if AHasBody then
  begin
    RequirePart(ASurface.BodyCell, nil, 'TD');
    RequirePart(ASurface.BodyLabel, ASurface.BodyCell, 'SPAN');

    if ASurface.BodyCell.closest('table') <> LTable then
    begin
      raise ENyxCollection.Create('Browser body metric belongs to another table');
    end;

    if ASurface.BodyEditor <> nil then
    begin
      RequirePart(ASurface.BodyEditor, ASurface.BodyCell, 'INPUT');

      if not ASurface.BodyEditor.classList.contains('nyx-cell-editor') then
      begin
        raise ENyxCollection.Create('Custom body editor needs its own fit metric');
      end;
    end;
    for LIndex := 0 to ASurface.BodyCell.childNodes.length - 1 do
    begin
      LChild := ASurface.BodyCell.childNodes[LIndex];

      if (LChild <> ASurface.BodyLabel) and (LChild <> ASurface.BodyEditor) then
      begin
        raise ENyxCollection.Create('Custom body composition needs its own fit metric');
      end;
    end;
    Result.BodyCell := ReadProfile(ASurface.BodyCell, False);
    Result.BodyLabel := ReadProfile(ASurface.BodyLabel, True);

    if ((Result.BodyCell.Display <> 'table-cell') and (Result.BodyCell.Display <> 'none')) or
      ((Result.BodyLabel.Display <> 'block') and (Result.BodyLabel.Display <> 'none')) then
    begin
      raise ENyxCollection.Create('Custom body text layout needs its own fit metric');
    end;
  end;

  if (Result.HeaderLabel.Display = 'none') = (Result.SortButton.Display = 'none') then
  begin
    raise ENyxCollection.Create('Column fit requires one displayed header label or sort button');
  end;
end;

function SameProfile(const ALeft, ARight: TFitProfile): Boolean;
var
  LProperty: TFitTextProperty;
begin
  Result := (ALeft.Lang = ARight.Lang) and (ALeft.Display = ARight.Display) and
    (ALeft.BoxSizing = ARight.BoxSizing) and (ALeft.Position = ARight.Position) and
    (ALeft.PaddingLeft = ARight.PaddingLeft) and (ALeft.PaddingRight = ARight.PaddingRight) and
    (ALeft.BorderLeft = ARight.BorderLeft) and (ALeft.BorderRight = ARight.BorderRight) and
    (ALeft.MarginLeft = ARight.MarginLeft) and (ALeft.MarginRight = ARight.MarginRight) and
    (ALeft.Insets = ARight.Insets) and
    (ALeft.Margins = ARight.Margins) and (ALeft.Minimum = ARight.Minimum);
  for LProperty := Low(TFitTextProperty) to High(TFitTextProperty) do
  begin
    Result := Result and (ALeft.Values[LProperty] = ARight.Values[LProperty]);
  end;
end;

function SameScene(const ALeft, ARight: TFitScene): Boolean;
var
  LIndex: Integer;
begin
  Result := SameProfile(ALeft.HeaderCell, ARight.HeaderCell) and
    SameProfile(ALeft.HeaderLabel, ARight.HeaderLabel) and
    SameProfile(ALeft.SortButton, ARight.SortButton) and
    SameProfile(ALeft.FilterButton, ARight.FilterButton) and
    (ALeft.HeaderText = ARight.HeaderText) and (ALeft.SortText = ARight.SortText) and
    (ALeft.FilterText = ARight.FilterText) and (ALeft.HasBody = ARight.HasBody);
  for LIndex := Low(TFitHeaderOrder) to High(TFitHeaderOrder) do
  begin
    Result := Result and (ALeft.Order[LIndex] = ARight.Order[LIndex]);
  end;

  if Result and ALeft.HasBody then
  begin
    Result := SameProfile(ALeft.BodyCell, ARight.BodyCell) and
      SameProfile(ALeft.BodyLabel, ARight.BodyLabel);
  end;
end;

function ReadyFontSet: TFitFontSet;
var
  LValue: JSValue;
begin
  LValue := TJSObject(document)['fonts'];

  if not isObject(LValue) or not isFunction(TJSObject(LValue)['check']) then
  begin
    raise ENyxCollection.Create('Browser text-aware font readiness is unavailable');
  end;
  Result := TFitFontSet(LValue);

  if Result.Status <> CLoaded then
  begin
    raise ENyxCollection.Create('Choose Fit again when browser fonts have loaded');
  end;
end;

procedure ApplyTextProfile(AProbe: TJSHTMLElement; const AProfile: TFitProfile);
var
  LProperty: TFitTextProperty;
begin
  for LProperty := Low(TFitTextProperty) to High(TFitTextProperty) do
  begin

    if AProfile.Values[LProperty] <> '' then
    begin
      AProbe.style.setProperty(FitTextProperties[LProperty], AProfile.Values[LProperty], 'important');
    end
    else
    begin
      AProbe.style.removeProperty(FitTextProperties[LProperty]);
    end;
  end;
  AProbe.setAttribute('lang', AProfile.Lang);
end;

procedure RequireReadyText(AProbe: TJSHTMLElement; const AProfile: TFitProfile);
var
  LRendered: JSValue;
  LFonts: TFitFontSet;
begin
  { innerText supplies the rendered whitespace/casing for Unicode-range font
    checks. Opacity (not visibility:hidden) keeps this ordinary text rendered;
    the probe is offscreen, noninteractive and excluded from accessibility. }
  LRendered := TJSObject(AProbe)['innerText'];
  LFonts := ReadyFontSet;

  if not isString(LRendered) or (AProfile.Values[ftpFont] = '') or
    not LFonts.Check(AProfile.Values[ftpFont], String(LRendered)) then
  begin
    raise ENyxCollection.Create('The sampled browser text font is not ready');
  end;
end;

function ProfileWidth(AProbe: TJSHTMLElement; const AProfile: TFitProfile;
  const AText: TNyxText): Double;
var
  LWidth: Double;
begin
  ApplyTextProfile(AProbe, AProfile);
  AProbe.textContent := AText;
  RequireReadyText(AProbe, AProfile);
  LWidth := Pixels(window.getComputedStyle(AProbe).getPropertyValue('width'));
  RequireReadyText(AProbe, AProfile);

  if LWidth < 0 then
  begin
    raise ENyxCollection.Create('The browser text metric changed during measurement');
  end;

  if AProfile.BoxSizing = 'border-box' then
  begin
    Result := Max(LWidth + AProfile.Insets, AProfile.Minimum);
  end
  else
  begin
    Result := Max(LWidth, AProfile.Minimum) + AProfile.Insets;
  end;
  Result := Result + AProfile.Margins;

  if Result < 0 then
  begin
    raise ENyxCollection.Create('Browser text margins require a custom fit metric');
  end;
end;

function TBrowserColumnFitMeasurement.MeasureScene(const AScene: TFitScene): Double;
var
  LProbe: TJSHTMLElement;
  LParts: array[TFitHeaderPart] of TJSHTMLElement;
  LPart: TFitHeaderPart;
  LHeader, LBody: Double;
  LIndex: Integer;

  procedure RequireInline(const AProfile: TFitProfile);
  begin

    if (AProfile.Display <> 'none') and (AProfile.Display <> 'inline') and
      (AProfile.Display <> 'inline-block') and (AProfile.Display <> 'inline-flex') then
    begin
      raise ENyxCollection.Create('Header layout needs its own fit metric');
    end;
  end;

  function CSSPixels(AValue: Double): String;
  var
    LFormat: TFormatSettings;
  begin
    LFormat := FormatSettings;
    LFormat.DecimalSeparator := '.';
    Result := FloatToStr(AValue, LFormat) + 'px';
  end;

  function PartProfile(APart: TFitHeaderPart): TFitProfile;
  begin
    case APart of
      fhpLabel: Result := AScene.HeaderLabel;
      fhpSort: Result := AScene.SortButton;
      fhpFilter: Result := AScene.FilterButton;
    end;
  end;

  procedure AppendPart(APart: TFitHeaderPart);
  var
    LProfile: TFitProfile;
    LElement: TJSHTMLElement;
    LText: TNyxText;
  begin
    LProfile := PartProfile(APart);
    RequireInline(LProfile);

    if LProfile.Display = 'none' then
    begin
      Exit;
    end;
    case APart of
      fhpLabel: LText := AScene.HeaderText;
      fhpSort: LText := AScene.SortText;
      fhpFilter: LText := AScene.FilterText;
    end;

    if APart = fhpLabel then
    begin
      LElement := TJSHTMLElement(document.createElement('span'));
    end
    else
    begin
      LElement := TJSHTMLElement(document.createElement('button'));
      LElement.setAttribute('type', 'button');
    end;
    LElement.setAttribute('tabindex', '-1');
    { Explicit per-side geometry replaces UA/global button styles. Inline spans
      remain inline, so real whitespace boundaries, hard breaks and CSS rules
      for inline minima are resolved by ONE shared ordinary line-box layout. }
    LElement.style.cssText := 'all:initial!important;position:static!important;' +
      'pointer-events:none!important;width:max-content!important;max-width:none!important;' +
      'height:auto!important;padding:0!important;margin:0!important;' +
      'border:0 solid transparent!important;';
    ApplyTextProfile(LElement, LProfile);
    LElement.style.setProperty('display', LProfile.Display, 'important');
    LElement.style.setProperty('box-sizing', LProfile.BoxSizing, 'important');
    LElement.style.setProperty('min-width', CSSPixels(LProfile.Minimum), 'important');
    LElement.style.setProperty('padding-left', CSSPixels(LProfile.PaddingLeft), 'important');
    LElement.style.setProperty('padding-right', CSSPixels(LProfile.PaddingRight), 'important');
    LElement.style.setProperty('border-left-width', CSSPixels(LProfile.BorderLeft), 'important');
    LElement.style.setProperty('border-right-width', CSSPixels(LProfile.BorderRight), 'important');
    LElement.style.setProperty('margin-left', CSSPixels(LProfile.MarginLeft), 'important');
    LElement.style.setProperty('margin-right', CSSPixels(LProfile.MarginRight), 'important');
    LElement.textContent := LText;
    LProbe.appendChild(LElement);
    LParts[APart] := LElement;
  end;

  procedure RequireHeaderFonts;
  var
    LKind: TFitHeaderPart;
  begin
    for LKind := Low(TFitHeaderPart) to High(TFitHeaderPart) do
    begin

      if LParts[LKind] <> nil then
      begin
        RequireReadyText(LParts[LKind], PartProfile(LKind));
      end;
    end;
  end;

begin
  for LPart := Low(TFitHeaderPart) to High(TFitHeaderPart) do
  begin
    LParts[LPart] := nil;
  end;
  LProbe := TJSHTMLElement(document.createElement('span'));
  LProbe.setAttribute('aria-hidden', 'true');
  LProbe.setAttribute('inert', '');
  LProbe.style.cssText := 'all:initial!important;position:fixed!important;' +
    'left:-100000px!important;top:0!important;opacity:0!important;pointer-events:none!important;' +
    'display:inline-block!important;width:max-content!important;min-width:0!important;' +
    'max-width:none!important;height:auto!important;margin:0!important;padding:0!important;' +
    'border:0!important;box-sizing:content-box!important;';
  try
    document.body.appendChild(LProbe);
    ApplyTextProfile(LProbe, AScene.HeaderCell);
    for LIndex := Low(TFitHeaderOrder) to High(TFitHeaderOrder) do
    begin
      AppendPart(AScene.Order[LIndex]);
    end;
    RequireHeaderFonts;
    LHeader := Pixels(window.getComputedStyle(LProbe).getPropertyValue('width'));
    RequireHeaderFonts;
    Result := LHeader + AScene.HeaderCell.Insets;
    { Retire the complete header composition before reusing only its ordinary
      parent span for unwrapped accepted body strings. No editor is cloned. }
    LProbe.textContent := '';
    for LIndex := 0 to High(FTexts) do
    begin
      LBody := ProfileWidth(LProbe, AScene.BodyLabel, FTexts[LIndex]) + AScene.BodyCell.Insets;
      Result := Max(Result, LBody);
    end;
  finally
    LProbe.remove;
  end;
end;

procedure TBrowserColumnFitMeasurement.Initialize(
  const ASample: INyxCollectionColumnSample;
  const ASurface: TNyxBrowserColumnFitSurface);
var
  LSample: INyxCollectionColumnSample;
  LSurface: TNyxBrowserColumnFitSurface;
  LFacts: INyxColumnFitMeasurement;
  LIndex: Integer;
begin
  LSample := ASample;
  LSurface := ASurface;
  LFacts := NyxColumnFitMeasurement(LSample, 0);
  SetLength(FTexts, LFacts.SampleRows);
  for LIndex := 0 to High(FTexts) do
  begin
    FTexts[LIndex] := LSample.TextAt(LIndex);
  end;
  { All arbitrary sample getters have returned. Observe current live styles
    now, measure copied strings, then update detached facts without getters. }
  FScene := ReadScene(LSurface, LFacts.SampleRows > 0);
  FMeasurement := LFacts.WithNaturalWidth(MeasureScene(FScene));
end;

function TBrowserColumnFitMeasurement.GetMeasurement: INyxColumnFitMeasurement;
begin
  Result := FMeasurement.Copy;
end;

function TBrowserColumnFitMeasurement.Matches(
  const ASurface: TNyxBrowserColumnFitSurface): Boolean;
var
  LScene: TFitScene;
begin
  Result := False;

  if (FMeasurement = nil) then
  begin
    Exit;
  end;
  try
    LScene := ReadScene(ASurface, FMeasurement.SampleRows > 0);

    if not SameScene(FScene, LScene) then
    begin
      Exit;
    end;
    Result := Abs(MeasureScene(LScene) - FMeasurement.NaturalWidth) < 0.001;
  except
    { A missing/changed/unsupported surface or pending font is simply no longer
      this metric's witness. Probe cleanup still ran in its own finally. }
    Result := False;
  end;
end;

function MeasureNyxBrowserColumnFit(const ASample: INyxCollectionColumnSample;
  const ASurface: TNyxBrowserColumnFitSurface): INyxBrowserColumnFitMeasurement;
var
  LMetric: TBrowserColumnFitMeasurement;
begin
  LMetric := TBrowserColumnFitMeasurement.Create;
  Result := LMetric;
  LMetric.Initialize(ASample, ASurface);
end;

end.
