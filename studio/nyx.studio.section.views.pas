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
unit nyx.studio.section.views;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.theme, nyx.state, nyx.behavior,
  nyx.events, nyx.gestures, nyx.designer.resize, nyx.collections.view,
  nyx.collections.mount, nyx.view.sections,
  nyx.studio.sections, nyx.studio.drag, nyx.studio.session, nyx.content.mount
  {$ifdef PAS2JS}, Web, nyx.render.browser, nyx.view.sections.browser
  {$else}, Controls, ExtCtrls, Forms, nyx.render.lcl, nyx.view.sections.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Borrowed browser renderer selected by an already mounted Studio role. }
  TNyxStudioSectionRenderer = TNyxBrowserRenderer;
  { DOM allocation slot borrowed for the lifetime of the mounted frame. }
  TNyxStudioSectionHost = TJSHTMLElement;
  { Browser element returned for lookup; its owning renderer controls retirement. }
  TNyxStudioSectionControl = TJSHTMLElement;
  { Focused DOM element borrowed only during interaction capture/restoration. }
  TNyxStudioSectionFocus = TJSHTMLElement;
  { Candidate factory callback whose borrowed receiver outlives pending frames. }
  TNyxStudioSectionConfigure = TNyxBrowserSectionConfigure;
  {$else}
  { Borrowed LCL renderer selected by an already mounted Studio role. }
  TNyxStudioSectionRenderer = TNyxLCLRenderer;
  { Native allocation slot borrowed for the lifetime of the mounted frame. }
  TNyxStudioSectionHost = TWinControl;
  { Native control lookup may return a non-windowed face; never transfer its owner. }
  TNyxStudioSectionControl = TControl;
  { Native keyboard focus requires a windowed control and expires with its view. }
  TNyxStudioSectionFocus = TWinControl;
  { Candidate factory callback whose borrowed receiver outlives pending frames. }
  TNyxStudioSectionConfigure = TNyxLCLSectionConfigure;
  {$endif}

  { Studio target routing over ordinary Nyx views. This owns view lifetimes and
    dedicated hosts, not a second set of UI controls. Root is explicitly a
    lookup forest; RootFor returns a real independently owned model root for
    compound commands. Borrowed results expire when their section is replaced.
    Host/theme/event receivers must outlive this owner. Render/refresh run on
    the owning UI thread and refuse reentry from its event callback. Keep this
    owner and its hosts alive through callback return; queue disposal to a later
    UI turn, just as section preparation/publication is queued. }
  TNyxStudioSectionViews = class
  private
    FFrame: TObject;
    FTheme: TNyxTheme;
    FContext: TNyxStudioSectionContext;
    FHost: TNyxStudioSectionHost;
    FConfigure: TNyxStudioSectionConfigure;
    FOnEvent: TNyxEventHandler;
    FInEvent: Integer;
    function GetRoot: TNyxStudioSectionRoots;
    function GetEvents: INyxEvents;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  public
    constructor Create(ATheme: TNyxTheme = nil;
      AContext: TNyxStudioSectionContext = nscComplete;
      AConfigure: TNyxStudioSectionConfigure = nil);
    destructor Destroy; override;
    { Admit a complete shell frame without retaining the caller's document.
      Host must be attached/live and remain borrowed through retirement. This
      editor-only owner refuses design mode, an external store and dispatch-time
      replacement. Candidate failure leaves the prior frame owned and mounted;
      physical extension failures propagate through ordinary target admission.
      Continuity is opt-in per role after the caller qualifies its semantic
      owner; IDs alone are insufficient. Supported copied text/ranges/scroll
      and focus transfer through public target adapters, never old widget
      pointers. Changed value/domain/binding contracts qualify each field again.
      Failure restores all prior roles regardless of the forward policy and
      reports recovery failures rather than silently claiming a rollback. }
    procedure Render(ADocument: TNyxDocument; ARoot: TNyxNode;
      AHost: TNyxStudioSectionHost; ADesignMode: Boolean = False;
      AState: TNyxState = nil; AContinuity: TNyxStudioSectionSet = []);
    { True admits compatible retained changes and grouped independent replacements,
      including dynamically growing workspace details. Unmentioned/compatible
      Resources inputs retain their physical ownership and event scope.
      False requests complete-frame admission for changed membership/chrome or
      refuses busy views. Already applied retained changes are recovered before
      refusal; extension/recovery failures raise and must be reported by the host. }
    function TryRefresh(ADocument: TNyxDocument; ARoot: TNyxNode;
      ADesignMode: Boolean = False): Boolean;
    { Borrowed callback receiver. Neither receiver nor this view owner may be
      destroyed before the synchronous call returns. }
    property OnEvent: TNyxEventHandler read FOnEvent write FOnEvent;
    { Borrowed lookup forest, rebuilt after each successful admission even when
      all real roots are retained. Resolve it again after refresh; actual model
      roots/controls instead follow their owning section's lifetime. }
    property Root: TNyxStudioSectionRoots read GetRoot;
    { Chrome router only, available after mounting. Use ViewFor for section
      registrations. An unmounted lookup raises rather than losing callbacks. }
    property Events: INyxEvents read GetEvents;
    { Input callbacks and collection/store notifications share this lifetime
      boundary. Missing compact views are idle, not a reason to wait forever. }
    function Dispatching: Boolean;
    { Resolve the actual owner. ViewFor raises for an unmounted ID; RootFor
      returns nil. Ambiguous mounted IDs always raise instead of choosing a view. }
    function ViewFor(const AID: TNyxText): TNyxStudioSectionRenderer;
    function RootFor(const AID: TNyxText): TNyxNode;
    { Borrow the currently mounted well-formed role; inactive roles return nil. }
    function SectionRoot(ASection: TNyxStudioSection): TNyxNode;
    function SectionView(ASection: TNyxStudioSection): TNyxStudioSectionRenderer;
    { Missing compact sections return nil. Receivers must disconnect their old
      registrations rather than routing them through a different section. }
    function SectionEvents(ASection: TNyxStudioSection): INyxEvents;
    { ABroker is nonnil/borrowed. Supply every mounted scope in one batch so a
      later source connection cannot silently replace an earlier section's routes. }
    procedure ConnectSources(ABroker: TNyxStudioDrag;
      const AMount: TNyxStudioCommandContext);
    { Ordinary target lookup semantics, routed through ViewFor. Identity selects
      runtime/design/automatic resolution; returned controls are borrowed and
      may be absent when that node has no matching target face. }
    function ControlFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TNyxStudioSectionControl;
    {$ifdef PAS2JS}
    function ElementFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TJSHTMLElement;
    {$endif}
    function InputFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TNyxStudioSectionControl;
    function FocusFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TNyxStudioSectionFocus;
    { Explicit navigation reveals through this aggregate's owned scroll scopes
      before requesting focus. Borrowed application hosts are never scrolled.
      The owner must outlive synchronous callbacks; composition stays guarded
      until they return. True reports a focus request, not OS activation proof. }
    function FocusControl(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): Boolean;
    function CollectionView(const AID: TNyxText): INyxCollectionView;
    { Mount an independently owned runtime collection through the ordinary
      section adapter. The actual renderer owns/disconnects the returned mount;
      retaining its interface cannot keep the target control alive. No document
      default or model-node ownership changes. Already connected IDs refuse. }
    function BindCollection(const AID: TNyxText;
      const AView: INyxCollectionView): INyxCollectionMount;
    function CollectionMount(const AID: TNyxText): INyxCollectionMount;
    { Convert a copied pointer through its real owning target view. No host
      input object or model root is retained by the returned value. }
    function ScreenPointFor(const AID: TNyxText;
      const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    { Synchronize ordinary controls in each mounted scope on the owning UI
      thread. This does not admit a document edit or replace a section. }
    procedure Sync;
  end;

implementation

uses
  nyx.text.index, nyx.data, nyx.publication, nyx.projection.refresh,
  nyx.studio.inspector, nyx.studio.inspector.groups, nyx.reusable.editor,
  nyx.parts.editor, nyx.button.editor,
  nyx.studio.resize, nyx.studio.move, nyx.studio.collections;

type
  {$ifdef PAS2JS}
  { Managed browser section lease, independent of its borrowed renderer lookup. }
  ISection = INyxBrowserViewSection;
  {$else}
  { Managed native section lease, independent of its borrowed renderer lookup. }
  ISection = INyxLCLViewSection;
  {$endif}

  { Managed forwarding callback with a borrowed facade pointer. The frame revokes
    that pointer before retiring sections, avoiding a cycle back into its owner. }
  TSectionObserver = class(TInterfacedObject, INyxViewSectionObserver)
  public
    Owner: TNyxStudioSectionViews; { borrowed, revoked before destruction }
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

  { Canonical UI-only frame liveness. Prepared bookkeeping keeps this flag alive
    without owning the frame or its controls; disposal revokes it before cleanup. }
  TFrameLifetime = class(TInterfacedObject)
  public
    Current: Boolean;
  end;

  { One owned complete shell: core renderer, dedicated role hosts, section leases
    and copied model documents. Children retire before their borrowed core-owned
    parents; Roots is a lookup forest rather than another model-tree owner. }
  TSectionFrame = class
  public
    Lifetime: TFrameLifetime;
    LifetimeLease: IInterface;
    Core: TNyxStudioSectionRenderer;
    Host: TNyxStudioSectionHost;
    Hosts: array[TNyxStudioSection] of TNyxStudioSectionHost;
    { Dynamic interface storage is supported by both compilers. Ordinals are
      the closed section roles; host/model arrays contain ordinary references. }
    Sections: array of ISection;
    Documents: TNyxStudioSectionDocuments;
    Roots: TNyxStudioSectionRoots;
    Observer: TSectionObserver;
    ObserverLease: INyxViewSectionObserver;
    constructor Create(AOwner: TNyxStudioSectionViews; ADocument: TNyxDocument);
    destructor Destroy; override;
    function View(ASection: TNyxStudioSection): TNyxStudioSectionRenderer;
    procedure UpdateRoots;
  end;

  { Grouped frame-source bookkeeping. Install moves prepared document ownership
    and updates the fixed lookup slots after section ownership changes, before
    any notification or old-view retirement. Old documents survive that boundary.
    A retired frame is detected through an independent flags-only lease. }
  TPreparedStudioFrame = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FFrame: TSectionFrame;
    FLifetime: TFrameLifetime;
    FLifetimeLease: IInterface;
    FBefore: TNyxStudioSectionDocuments;
    FNext: TNyxStudioSectionDocuments;
    FInstalled: Boolean;
    FRetired: Boolean;
  public
    constructor Create(AFrame: TSectionFrame;
      var ADocuments: TNyxStudioSectionDocuments);
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

constructor TPreparedStudioFrame.Create(AFrame: TSectionFrame;
  var ADocuments: TNyxStudioSectionDocuments);
begin
  inherited Create;
  FFrame := AFrame;
  FLifetime := AFrame.Lifetime;
  FLifetimeLease := AFrame.LifetimeLease;
  FBefore := AFrame.Documents;
  FNext := ADocuments;
  ADocuments := nil;
end;

destructor TPreparedStudioFrame.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TPreparedStudioFrame.CheckReady;
begin

  if FRetired or FInstalled or not FLifetime.Current then
  begin
    raise ENyxModel.Create('Studio frame retired before context publication');
  end;

  if FFrame.Documents <> FBefore then
  begin
    raise ENyxModel.Create('Studio frame source changed before context publication');
  end;
end;

procedure TPreparedStudioFrame.Validate;
begin
  CheckReady;
end;

procedure TPreparedStudioFrame.Install;
begin
  FInstalled := True;
  FFrame.Documents := FNext;
  FNext := nil;
  { The frame owns only canonical sections. Their getters and the fixed role
    slots perform no allocation, managed release or application callback. }
  FFrame.UpdateRoots;
end;

procedure TPreparedStudioFrame.Notify;
begin
  { All application-visible ownership has already changed during Install. }
end;

procedure TPreparedStudioFrame.Retire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  FFrame := nil;

  if FInstalled then
  begin
    FreeAndNil(FBefore);
  end
  else
  begin
    FreeAndNil(FNext);
    FBefore := nil; { still owned by the unchanged frame }
  end;
end;

function NewFrameHost(AHost: TNyxStudioSectionHost): TNyxStudioSectionHost;
{$ifndef PAS2JS}
var
  LPanel: TPanel;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('div'));
  try
    Result.style.cssText := 'position:fixed;left:-100000px;top:0;visibility:hidden;pointer-events:none;';
    Result.style.setProperty('width', IntToStr(AHost.clientWidth) + 'px');
    Result.style.setProperty('height', IntToStr(AHost.clientHeight) + 'px');
    Result.setAttribute('inert', '');
    Result.setAttribute('data-nyx-shell-frame', '');
    AHost.parentNode.appendChild(Result);
  except
    { Ownership has not yet reached the frame. A host/extension refusal must
      detach the local proposal even when insertion partially succeeded. }
    Result.remove;
    raise;
  end;
  {$else}
  LPanel := TPanel.Create(nil);
  try
    LPanel.BevelOuter := bvNone;
    LPanel.Visible := False;
    LPanel.Parent := AHost;
    LPanel.SetBounds(0, 0, AHost.ClientWidth, AHost.ClientHeight);
    Result := LPanel;
  except
    { The failed function has not assigned TSectionFrame.Host. Parent is
      borrowed, not the component owner, so retire this local control here. }
    LPanel.Free;
    raise;
  end;
  {$endif}
end;

function NewSectionHost(AParent: TNyxStudioSectionHost): TNyxStudioSectionHost;
{$ifndef PAS2JS}
var
  LPanel: TPanel;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('div'));
  try
    Result.style.cssText := 'width:100%;height:100%;min-width:0;min-height:0;';
    AParent.appendChild(Result);
  except
    Result.remove;
    raise;
  end;
  {$else}
  LPanel := TPanel.Create(nil);
  try
    LPanel.BevelOuter := bvNone;
    LPanel.Parent := AParent;
    LPanel.Align := alClient;
    Result := LPanel;
  except
    { A child host also remains locally owned until this function returns. }
    LPanel.Free;
    raise;
  end;
  {$endif}
end;

procedure FreeHost(var AHost: TNyxStudioSectionHost);
begin
  {$ifdef PAS2JS}

  if AHost <> nil then
  begin
    AHost.remove;
    AHost := nil;
  end;
  {$else}
  FreeAndNil(AHost);
  {$endif}
end;

procedure RevealFrame(AFrame: TSectionFrame; AParent: TNyxStudioSectionHost);
begin
  {$ifdef PAS2JS}
  AParent.appendChild(AFrame.Host);
  AFrame.Host.removeAttribute('inert');
  AFrame.Host.style.cssText := 'width:100%;height:100%;min-width:0;min-height:0;';
  {$else}
  AFrame.Host.Align := alClient;
  AFrame.Host.Visible := True;
  {$endif}
end;

procedure TSectionObserver.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LOwner: TNyxStudioSectionViews;
begin
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LOwner.Changed(ANode, AEvent);
  end;
end;

constructor TSectionFrame.Create(AOwner: TNyxStudioSectionViews; ADocument: TNyxDocument);
var
  LSection: TNyxStudioSection;
  LParent: TNyxStudioSectionHost;
  LChanges: array of INyxViewSectionChange;
begin
  inherited Create;
  Lifetime := TFrameLifetime.Create;
  LifetimeLease := Lifetime;
  Lifetime.Current := True;
  LChanges := nil;
  SetLength(Sections, Ord(High(TNyxStudioSection)) + 1);
  Documents := TNyxStudioSectionDocuments.Create(ADocument, AOwner.FContext);
  Roots := TNyxStudioSectionRoots.Create;
  Observer := TSectionObserver.Create;
  ObserverLease := Observer;
  Observer.Owner := AOwner;
  Host := NewFrameHost(AOwner.FHost);
  Core := TNyxStudioSectionRenderer.Create(AOwner.FTheme);

  if Assigned(AOwner.FConfigure) then
  begin
    AOwner.FConfigure(Core);
  end;
  Core.OnEvent := {$ifdef PAS2JS}@{$endif}Observer.Changed;
  Core.Render(Documents[nssChrome], Documents[nssChrome].Pages[0], Host);
  for LSection := nssProject to High(TNyxStudioSection) do
  begin

    if Documents[LSection] = nil then
    begin
      Continue;
    end;
    {$ifdef PAS2JS}
    LParent := Core.ElementFor(NyxStudioSectionMountID(LSection));
    {$else}
    LParent := TWinControl(Core.ControlFor(NyxStudioSectionMountID(LSection)));
    {$endif}
    Hosts[LSection] := NewSectionHost(LParent);
    {$ifdef PAS2JS}
    Sections[Ord(LSection)] := NewNyxBrowserViewSection(
    {$else}
    Sections[Ord(LSection)] := NewNyxLCLViewSection(
    {$endif}
      TNyxViewSectionRef.Named(NyxStudioSectionRootID(LSection)), Hosts[LSection],
      AOwner.FConfigure, ObserverLease, AOwner.FTheme, Core.ViewportContext);
    SetLength(LChanges, Length(LChanges) + 1);
    LChanges[High(LChanges)] := Sections[Ord(LSection)].Prepare(Documents[LSection],
      Documents[LSection].Pages[0]);
  end;

  if not PublishNyxViewSections(LChanges) then
  begin
    raise ENyxModel.Create('Prepared Studio frame changed before publication');
  end;
  UpdateRoots;
end;

destructor TSectionFrame.Destroy;
var
  LSection: TNyxStudioSection;
begin

  if Lifetime <> nil then
  begin
    Lifetime.Current := False;
  end;

  if Observer <> nil then
  begin
    Observer.Owner := nil;
  end;
  Roots.Free;
  { Children borrow core-owned mount parents. Retire their views and dedicated
    hosts before destroying the surrounding chrome and its physical children. }
  for LSection := nssProject to High(TNyxStudioSection) do
  begin

    if Ord(LSection) < Length(Sections) then
    begin
      Sections[Ord(LSection)] := nil;
    end;
    FreeHost(Hosts[LSection]);
  end;
  Core.Free;
  FreeHost(Host);
  ObserverLease := nil;
  Documents.Free;
  inherited Destroy;
end;

function TSectionFrame.View(ASection: TNyxStudioSection): TNyxStudioSectionRenderer;
begin
  Result := nil;

  if ASection = nssChrome then
  begin
    Exit(Core);
  end;

  if Sections[Ord(ASection)] <> nil then
  begin
    Result := Sections[Ord(ASection)].Renderer;
  end;
end;

procedure TSectionFrame.UpdateRoots;
var
  LSection: TNyxStudioSection;
  LView: TNyxStudioSectionRenderer;
begin
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin
    LView := View(LSection);

    if LView = nil then
    begin
      Roots.SetRoot(LSection, nil);
    end
    else
    begin
      Roots.SetRoot(LSection, LView.Root);
    end;
  end;
end;

constructor TNyxStudioSectionViews.Create(ATheme: TNyxTheme;
  AContext: TNyxStudioSectionContext; AConfigure: TNyxStudioSectionConfigure);
begin
  inherited Create;
  FTheme := ATheme;
  FContext := AContext;
  { Configure every fresh renderer before admission, including later section
    replacements. The callback/theme are borrowed until this owner retires. }
  FConfigure := AConfigure;
end;

destructor TNyxStudioSectionViews.Destroy;
begin
  FOnEvent := nil;
  FreeAndNil(FFrame);
  FConfigure := nil;
  inherited Destroy;
end;

procedure TNyxStudioSectionViews.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LHandler: TNyxEventHandler;
begin

  if (FFrame = nil) or (Root.Find(ANode.ID) <> ANode) or not Assigned(FOnEvent) then
  begin
    Exit;
  end;
  LHandler := FOnEvent;
  Inc(FInEvent);
  try
    LHandler(ANode, AEvent);
  finally
    Dec(FInEvent);
  end;
end;

function TNyxStudioSectionViews.Dispatching: Boolean;
var
  LSection: TNyxStudioSection;
  LView: TNyxStudioSectionRenderer;
begin
  Result := FInEvent <> 0;

  if Result then
  begin
    Exit;
  end;
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin
    LView := SectionView(LSection);

    if (LView <> nil) and not LView.SectionPublicationReady then
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxStudioSectionViews.Render(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TNyxStudioSectionHost; ADesignMode: Boolean; AState: TNyxState;
  AContinuity: TNyxStudioSectionSet);
var
  LNext: TSectionFrame;
  LPrevious: TSectionFrame;
  LPreviousHost: TNyxStudioSectionHost;
  LInputs: array[TNyxStudioSection] of TNyxContentFaceStates;
  LSection: TNyxStudioSection;
  LView: TNyxStudioSectionRenderer;
  LAdmitted: Boolean;

  procedure RestoreCandidate(AFocus: Boolean);
  var
    LRole: TNyxStudioSection;
    LOwner: TNyxStudioSectionRenderer;
  begin
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin

      if not (LRole in AContinuity) then
      begin
        Continue;
      end;
      LOwner := LNext.View(LRole);

      if (LOwner <> nil) and not LOwner.RestoreInteraction(LInputs[LRole], AFocus) then
      begin
        raise ENyxModel.Create('Studio candidate input boundary became busy');
      end;
    end;
  end;

  procedure RestorePrevious(const AFailure: TNyxText);
  var
    LRole: TNyxStudioSection;
    LOwner: TNyxStudioSectionRenderer;
    LFailure: TNyxText;
  begin

    if LPrevious = nil then
    begin
      Exit;
    end;
    LFailure := '';
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      LOwner := LPrevious.View(LRole);

      if LOwner = nil then
      begin
        Continue;
      end;
      try

        if not LOwner.RestoreInteraction(LInputs[LRole]) then
        begin
          raise ENyxModel.Create('Prior frame input boundary became busy');
        end;
      except
        on LException: Exception do
        begin
          LFailure := LFailure + ' / ' + NyxStudioSectionRootID(LRole) + ': ' +
            LException.Message;
        end;
      end;
    end;

    if LFailure <> '' then
    begin
      raise ENyxModel.Create(AFailure + ' / Studio frame recovery failed' + LFailure);
    end;
  end;
begin

  if Dispatching or ADesignMode or (AState <> nil) or (ADocument = nil) or
    (ADocument.Count <> 1) or (ARoot <> ADocument.Pages[0]) or (AHost = nil) then
  begin
    raise ENyxModel.Create('Studio shell frames require an idle owning host and shared shell document');
  end;
  LPrevious := TSectionFrame(FFrame);
  LPreviousHost := FHost;
  LNext := nil;
  LAdmitted := False;
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin
    LInputs[LSection] := nil;

    if LPrevious = nil then
    begin
      Continue;
    end;
    LView := LPrevious.View(LSection);

    if (LView <> nil) and not LView.CaptureInteraction(LInputs[LSection]) then
    begin
      raise ENyxModel.Create('Studio prior frame input boundary became busy');
    end;
  end;
  FHost := AHost;
  try
    try
      LNext := TSectionFrame.Create(Self, ADocument);
      { Restore copies while detached, then focus only once the complete
        candidate is visible. Target adapters suppress ordinary input callbacks
        during replay. The old frame remains owned until every step succeeds. }
      RestoreCandidate(False);
      RevealFrame(LNext, FHost);
      RestoreCandidate(True);
    except
      on LException: Exception do
      begin
        FreeAndNil(LNext);
        FHost := LPreviousHost;
        { Candidate revelation/focus can change the old physical selection even
          though no old model was edited. Retire the candidate before recovery. }
        RestorePrevious(LException.Message);
        raise;
      end;
    end;
    FFrame := LNext;
    LNext := nil;
    LAdmitted := True;
    LPrevious.Free;
  finally
    LNext.Free;

    if not LAdmitted then
    begin
      FHost := LPreviousHost;
    end;
  end;
end;

{ Negative evidence only, never source admission or compatibility. Catalog
  recipes already own their materialized children. With no component/content/
  override path anywhere in this source, composition's generic Expand copies
  each concrete node once, with an empty prefix and canonical escaped runtime
  ID. Presentation/resource snapshots and scalar binding/platform projection
  preserve that membership. The current realized root supplies the other set;
  its former source may itself have used dynamic composition.

  Both retained refresh and arrangement require identical GLOBAL runtime IDs.
  Different sets therefore need independent Prepare anyway; avoid preparing and
  discarding a doomed retained candidate first. Equal sets or an unknown shape
  still take the complete ordinary path, including reparenting/reordering whose
  per-parent child counts change. Full fresh Prepare remains the admission owner.
  Indices retain copied keys only and expire with this invocation. AKnown means
  both complete concrete trees passed these bounded membership checks. It is a
  routing observation only: properties, contracts and target admission remain
  unchecked. Unknown/composed shapes never authorize an early form hint. }
function StaticMembershipChanged(AExisting, ASource: TNyxNode;
  out AKnown: Boolean): Boolean;
var
  LExistingIDs: TNyxTextIndex;
  LSourceIDs: TNyxTextIndex;
  LExistingCount: Integer;
  LSourceCount: Integer;
  LDifferent: Boolean;

  function Collect(ANode, AParent: TNyxNode; ARealized: Boolean;
    ADepth: Integer; AIDs: TNyxTextIndex; var ACount: Integer): Boolean;
  var
    LKey: TNyxText;
    LChild: Integer;
  begin
    Result := False;

    if (ANode = nil) or (ANode.Parent <> AParent) or
      (ANode.IsRealized <> ARealized) or (ADepth > NyxMaximumTreeDepth) or
      (ACount >= NyxMaximumNodes) then
    begin
      Exit;
    end;

    if not ARealized and
      ((ANode.Kind = NyxKindName(nkComponent)) or
        (ANode.ProjectionKind = NyxKindName(nkComponent)) or
        (ANode.Kind = NyxKindName(nkSlotOverride)) or
        (ANode.ProjectionKind = NyxKindName(nkSlotOverride)) or ANode.HasContent) then
    begin
      Exit;
    end;
    LKey := ANode.ID;

    if not ARealized then
    begin
      { Empty prefix still escapes slash/tilde. Raw design IDs are not runtime
        IDs, even when this source contains no reusable component reference. }
      LKey := NyxQualifiedID('', LKey);
    end;

    if AIDs.IndexOf(LKey) >= 0 then
    begin
      Exit;
    end;
    AIDs.AddFirst(LKey, ACount);
    Inc(ACount);

    if not ARealized and (LExistingIDs.IndexOf(LKey) < 0) then
    begin
      LDifferent := True;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin

      if not Collect(ANode.Children[LChild], ANode, ARealized,
        ADepth + 1, AIDs, ACount) then
      begin
        Exit;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;
  AKnown := False;
  LExistingIDs := TNyxTextIndex.Create;
  LSourceIDs := nil;
  try
    LSourceIDs := TNyxTextIndex.Create;
    LExistingCount := 0;
    LSourceCount := 0;
    LDifferent := False;

    if Collect(AExisting, nil, True, 0, LExistingIDs, LExistingCount) and
      Collect(ASource, nil, False, 0, LSourceIDs, LSourceCount) then
    begin
      AKnown := True;
      Result := LDifferent or (LExistingCount <> LSourceCount);
    end;
  finally
    LSourceIDs.Free;
    LExistingIDs.Free;
  end;
end;

function TNyxStudioSectionViews.TryRefresh(ADocument: TNyxDocument; ARoot: TNyxNode;
  ADesignMode: Boolean): Boolean;
var
  LFrame: TSectionFrame;
  LDocuments: TNyxStudioSectionDocuments;
  LSection: TNyxStudioSection;
  LView: TNyxStudioSectionRenderer;
  LChanges: array of INyxViewSectionChange;
  LBefore: array[TNyxStudioSection] of TNyxNode;
  LInputs: array[TNyxStudioSection] of TNyxContentFaceStates;
  LRetained: array[TNyxStudioSection] of Boolean;
  LPublished: Boolean;
  LFormEffect: INyxPreparedPublication;
  LFrameEffect: INyxPreparedPublication;
  LEffects: INyxPreparedPublication;
  LForms: TNyxProjectionFormContexts;
  LFailed: Boolean;
  LStaticKnown: Boolean;
  LMembershipChanged: Boolean;
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  LPhaseStarted: QWord;

  procedure RecordPhase(const AStage: TNyxText; ARole: TNyxStudioSection);
  var
    LNow: QWord;
  begin
    { Static role/stage evidence only. Native diagnostic builds keep these
      boundaries separate from renderer clocks; production/browser omit them. }
    LNow := GetTickCount64;
    WriteLn('studio-section-refresh,', AStage, ',', Ord(ARole), ',',
      LNow - LPhaseStarted);
    LPhaseStarted := LNow;
  end;
  {$endif}

  procedure PrepareInspectorForms;
  var
    LCollectionForms: TNyxProjectionFormContexts;
    LIndex: Integer;
    LCount: Integer;

    procedure Add(const AID: TNyxText; const AKeys: array of TNyxExtensionRef);
    var
      LCount: Integer;
    begin

      if (LView.Root.Find(AID) = nil) or
        (LDocuments[nssInspector].Pages[0].Find(AID) = nil) then
      begin
        Exit;
      end;
      LCount := Length(LForms);
      SetLength(LForms, LCount + 1);
      LForms[LCount] := TNyxProjectionFormContext.ForForm(AID, AID, AKeys);
    end;

  begin
    LForms := nil;
    { Separate scopes are essential: a new reusable proposal baseline resets
      only that compound's input, while independent unfinished property and
      responsive-form drafts remain unchanged for the same selected owner. }
    Add(NyxInspectorGroupID(nigProperties),
      [NyxExtension(NyxStudioInspectorContextKey), NyxExtension(NyxStudioPropertyOwnerKey)]);
    Add(NyxStudioReusableEditorID,
      [NyxExtension(NyxReusableEditorSourceKey), NyxExtension(NyxReusableEditorBaselineKey)]);
    { The Parts baseline includes the complete authored document, so even an
      unrelated scalar edit advances it. Its independent scope must participate
      in the SAME event-fenced publication as the other forms; strict instance,
      path/action and membership checks still force replacement when needed. }
    Add(NyxStudioPartsEditorID, [NyxExtension(NyxPartsEditorBaselineKey)]);
    { Button style captures both the authored/effective baseline and accepted
      presentation, including localized image references. Advance them together
      in their own scope; owner, marker and undeclared metadata remain strict.
      Apply/history reset this form's defaults and fence old event work while
      unrelated Inspector fields retain their independent unfinished drafts. }
    Add('inspector-button', [NyxExtension(NyxButtonEditorBaselineKey),
      NyxExtension(NyxButtonEditorValueKey)]);
    Add('inspector-viewport-rule', [NyxExtension(NyxStudioPropertyOwnerKey)]);
    { Geometry commands carry the selected owner even when they contain no
      editable value. Their event authority must advance in the same group as
      property forms, rather than leaving grips attached to the old selection. }
    Add('studio-resize-tools', [NyxExtension(NyxStudioResizeOwnerKey)]);
    Add('studio-position-tools', [NyxExtension(NyxStudioMoveOwnerKey)]);
    { Collection form owners declare their exact command-baseline keys. Their
      query/layout descendants are independent of the primary property form;
      no arbitrary inspector metadata becomes refreshable here. }
    LCollectionForms := NyxStudioCollectionFormContexts(LView.Root);
    for LIndex := 0 to High(LCollectionForms) do
    begin

      if LDocuments[nssInspector].Pages[0].Find(
        LCollectionForms[LIndex].RuntimeID) = nil then
      begin
        Continue;
      end;
      LCount := Length(LForms);
      SetLength(LForms, LCount + 1);
      LForms[LCount] := LCollectionForms[LIndex];
    end;
  end;

  function TryChangedInspectorForm: Boolean;
  var
    LRestoreHint: TNyxProjectionValueRestores;
    LChangedHint: Boolean;
    LHintValid: Boolean;
  begin
    Result := False;
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
    LPhaseStarted := GetTickCount64;
    {$endif}
    PrepareInspectorForms;
    { Only complete bounded static trees reach this hint. Their source/runtime
      spelling can still differ (for example escaped IDs), and form structure or
      context presence may be unknown. Any uncertainty keeps the ordinary path.
      The copied restore vector is discarded before the target call; it grants
      no reset rights and never survives as an admission or publication plan. }
    LHintValid := PrepareNyxProjectionFormRestores(LView.Root,
      LDocuments[nssInspector].Pages[0], LForms, LRestoreHint, LChangedHint);
    LRestoreHint := nil;
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
    RecordPhase('form-hint', nssInspector);
    {$endif}

    if not LHintValid or not LChangedHint then
    begin
      Exit;
    end;
    { A changed context cannot pass ordinary scalar admission. Prefer the SAME
      complete renderer preparation used after that refusal: fresh document,
      realization, baseline, draft, scheduler and event-generation checks still
      own the decision. Nil falls through to the complete former routing below. }
    LFormEffect := LView.TryPrepareFormRefresh(LDocuments[nssInspector],
      LDocuments[nssInspector].Pages[0], LForms);
    Result := LFormEffect <> nil;
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}

    if Result then
    begin
      RecordPhase('form-preferred-accepted', nssInspector);
    end
    else
    begin
      RecordPhase('form-preferred-refused', nssInspector);
    end;
    {$endif}
  end;

  procedure RestoreRetained;
  var
    LFailed: Boolean;

    procedure RestoreRole(ARole: Integer);
    var
      LRole: TNyxStudioSection;
      LOwner: TNyxStudioSectionRenderer;
      LRoleFailed: Boolean;
    begin

      if ARole < Ord(Low(TNyxStudioSection)) then
      begin
        Exit;
      end;
      LRole := TNyxStudioSection(ARole);
      LRoleFailed := False;
      try
        try

          if LRetained[LRole] then
          begin
            { Attempt each retained role once, even if an earlier form or role
              failed recovery. This fixed-role unwind preserves the first exact
              Pascal exception or raw platform value without boxing either. }
            LRetained[LRole] := False;
            LOwner := LFrame.View(LRole);

            if not LOwner.TryRefresh(LFrame.Documents[LRole],
              LFrame.Documents[LRole].Pages[0], False) then
            begin
              raise ENyxModel.Create('Retained rollback context changed / ' +
                NyxStudioSectionRootID(LRole));
            end;
            { Replay restores the private source/arrangement baseline. The
              exact realized clone restores runtime properties in that same
              admitted tree; trusted exchange performs no callbacks/allocation. }
            LOwner.Root.ExchangeResourceProjection(LBefore[LRole]);
            LOwner.Sync;

            if not LOwner.RestoreInteraction(LInputs[LRole]) then
            begin
              raise ENyxModel.Create('Retained rollback input boundary became busy / ' +
                NyxStudioSectionRootID(LRole));
            end;
          end;
        except
          LRoleFailed := True;
          raise;
        end;
      finally
        try
          RestoreRole(ARole - 1);
        except

          if not LRoleFailed then
          begin
            raise;
          end;
        end;
      end;
    end;

  begin
    LFailed := False;
    try
      try

        if LFormEffect <> nil then
        begin
          { Its target restores old input while its own event preview is fenced.
            Its failure must not skip independent previously refreshed roles. }
          LFormEffect.Retire;
        end;
      except
        LFailed := True;
        raise;
      end;
    finally
      try
        RestoreRole(Ord(High(TNyxStudioSection)));
      except

        if not LFailed then
        begin
          raise;
        end;
      end;
    end;
  end;

begin
  Result := False;

  if Dispatching or (FFrame = nil) or ADesignMode or (ADocument = nil) or
    (ADocument.Count <> 1) or (ARoot <> ADocument.Pages[0]) then
  begin
    Exit;
  end;
  LFrame := TSectionFrame(FFrame);
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  LPhaseStarted := GetTickCount64;
  {$endif}
  LDocuments := TNyxStudioSectionDocuments.Create(ADocument, FContext);
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  RecordPhase('documents', nssChrome);
  {$endif}
  LChanges := nil;
  LPublished := False;
  LFailed := False;
  LForms := nil;
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin
    LBefore[LSection] := nil;
    LInputs[LSection] := nil;
    LRetained[LSection] := False;
  end;
  try
    for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin

      if (LDocuments[LSection] = nil) <> (LFrame.Documents[LSection] = nil) then
      begin
        Exit; { compact/full-frame membership changes use detached admission }
      end;
      LView := LFrame.View(LSection);

      if LView = nil then
      begin
        Continue;
      end;

      if not LView.SectionPublicationReady then
      begin
        Exit;
      end;
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      LPhaseStarted := GetTickCount64;
      {$endif}
      LBefore[LSection] := LView.Root.Clone;
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      RecordPhase('before-clone', LSection);
      {$endif}

      if not LView.CaptureInteraction(LInputs[LSection]) then
      begin
        {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
        RecordPhase('input-refused', LSection);
        {$endif}
        Exit;
      end;
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      RecordPhase('input-capture', LSection);
      {$endif}
    end;
    try
      for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
      begin
        LView := LFrame.View(LSection);

        if LView = nil then
        begin
          Continue;
        end;

        { Chrome retains its original full-frame fallback. For another role,
          a complete static membership mismatch can only reach replacement.
          Readiness, model/draft capture above and fresh Prepare below remain
          required; this decision grants no admission or continuity rights. }
        LStaticKnown := False;
        LMembershipChanged := False;

        if LSection <> nssChrome then
        begin
          LMembershipChanged := StaticMembershipChanged(LView.Root,
            LDocuments[LSection].Pages[0], LStaticKnown);
        end;

        if not LMembershipChanged then
        begin

          if (LSection = nssInspector) and LStaticKnown and TryChangedInspectorForm then
          begin
            Continue;
          end;
          { Even a target Sync exception can have touched the physical draft.
            Ordinary TryRefresh recovers its model; the outer owner must also
            restore continuity for this attempted retained role. }
          LRetained[LSection] := True;
          {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
          LPhaseStarted := GetTickCount64;
          {$endif}

          if LView.TryRefresh(LDocuments[LSection], LDocuments[LSection].Pages[0], False) then
          begin
            {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
            RecordPhase('retained-accepted', LSection);
            {$endif}
            Continue;
          end;
          {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
          RecordPhase('retained-refused', LSection);
          {$endif}
          LRetained[LSection] := False; { False promises no admitted changes. }

          if LSection = nssInspector then
          begin
            { The stock form explicitly owns only these context keys. Unknown
              editor metadata, changed contracts or membership still require
              ordinary detached replacement; no schema check is bypassed. }
            PrepareInspectorForms;
            LFormEffect := LView.TryPrepareFormRefresh(LDocuments[LSection],
              LDocuments[LSection].Pages[0], LForms);

            if LFormEffect <> nil then
            begin
              Continue;
            end;
          end;
        end;
        {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
        { Static role identity only; authored fields never enter timing output. }
        WriteLn('studio-ui,section-replace,', Ord(LSection));
        {$endif}

        if LSection = nssChrome then
        begin
          RestoreRetained;
          Exit;
        end
        else
        begin
          {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
          LPhaseStarted := GetTickCount64;
          {$endif}
          SetLength(LChanges, Length(LChanges) + 1);
          LChanges[High(LChanges)] := LFrame.Sections[Ord(LSection)].Prepare(
            LDocuments[LSection], LDocuments[LSection].Pages[0]);
          {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
          RecordPhase('replacement-prepare', LSection);
          {$endif}
        end;
      end;
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      LPhaseStarted := GetTickCount64;
      {$endif}

      LFrameEffect := TPreparedStudioFrame.Create(LFrame, LDocuments);

      if LFormEffect <> nil then
      begin
        LEffects := PrepareNyxGroup([LFormEffect, LFrameEffect]);
      end
      else
      begin
        LEffects := PrepareNyxGroup([LFrameEffect]);
      end;

      if not PublishNyxViewSections(LChanges, LEffects, LPublished) then
      begin
        {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
        RecordPhase('publication-refused', nssChrome);
        {$endif}
        RestoreRetained;
        Exit;
      end;
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      RecordPhase('publication-accepted', nssChrome);
      {$endif}
      Result := True;
    except
      LFailed := True;

      if not LPublished then
      begin
        try
          RestoreRetained;
        except
          { Always attempt recovery, retaining the exact primary Pascal/raw
            failure when an independent target's recovery also refuses. }
        end;
      end;
      raise;
    end;
  finally
    try

      if LFormEffect <> nil then
      begin
        try
          LFormEffect.Retire;
        except

          if not LFailed then
          begin
            raise;
          end;
        end;
      end;
    finally
      LEffects := nil;
      LFormEffect := nil;
      LFrameEffect := nil;
      LChanges := nil;
      LDocuments.Free;
      for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
      begin
        LBefore[LSection].Free;
        LInputs[LSection] := nil;
      end;
    end;
  end;
end;

function TNyxStudioSectionViews.GetRoot: TNyxStudioSectionRoots;
begin
  Result := nil;

  if FFrame <> nil then
  begin
    Result := TSectionFrame(FFrame).Roots;
  end;
end;

function TNyxStudioSectionViews.GetEvents: INyxEvents;
begin
  Result := nil;

  if FFrame <> nil then
  begin
    Result := TSectionFrame(FFrame).Core.Events;
  end;

  if Result = nil then
  begin
    raise ENyxModel.Create('Mount the Studio shell before registering Chrome events');
  end;
end;

function TNyxStudioSectionViews.SectionView(ASection: TNyxStudioSection): TNyxStudioSectionRenderer;
begin
  Result := nil;

  if FFrame <> nil then
  begin
    Result := TSectionFrame(FFrame).View(ASection);
  end;
end;

function TNyxStudioSectionViews.SectionRoot(ASection: TNyxStudioSection): TNyxNode;
begin
  Result := nil;

  if Root <> nil then
  begin
    Result := Root.Roots[ASection];
  end;
end;

function TNyxStudioSectionViews.SectionEvents(ASection: TNyxStudioSection): INyxEvents;
var
  LView: TNyxStudioSectionRenderer;
begin
  Result := nil;
  LView := SectionView(ASection);

  if LView <> nil then
  begin
    Result := LView.Events;
  end;
end;

procedure TNyxStudioSectionViews.ConnectSources(ABroker: TNyxStudioDrag;
  const AMount: TNyxStudioCommandContext);
var
  LRouters: TNyxStudioDragRouters;
  LRoots: TNyxStudioDragRoots;
  LSection: TNyxStudioSection;
begin
  LRouters := nil;
  LRoots := nil;
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin

    if SectionRoot(LSection) <> nil then
    begin
      SetLength(LRoots, Length(LRoots) + 1);
      SetLength(LRouters, Length(LRouters) + 1);
      LRoots[High(LRoots)] := SectionRoot(LSection);
      LRouters[High(LRouters)] := SectionEvents(LSection);
    end;
  end;
  ABroker.ConnectSources(LRouters, LRoots, AMount);
end;

function TNyxStudioSectionViews.RootFor(const AID: TNyxText): TNyxNode;
begin
  Result := nil;

  if Root <> nil then
  begin
    Result := Root.RootFor(AID);
  end;
end;

function TNyxStudioSectionViews.ViewFor(const AID: TNyxText): TNyxStudioSectionRenderer;
var
  LSection: TNyxStudioSection;
begin
  Result := nil;

  if (Root <> nil) and Root.SectionFor(AID, LSection) then
  begin
    Result := SectionView(LSection);
  end;

  if Result = nil then
  begin
    raise ENyxModel.Create('Studio control is not mounted: ' + AID);
  end;
end;

function TNyxStudioSectionViews.ControlFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxStudioSectionControl;
begin
  {$ifdef PAS2JS}
  Result := ViewFor(AID).ElementFor(AID, AIdentity);
  {$else}
  Result := ViewFor(AID).ControlFor(AID, AIdentity);
  {$endif}
end;

{$ifdef PAS2JS}
function TNyxStudioSectionViews.ElementFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TJSHTMLElement;
begin
  Result := ControlFor(AID, AIdentity);
end;
{$endif}

function TNyxStudioSectionViews.InputFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxStudioSectionControl;
begin
  Result := ViewFor(AID).InputFor(AID, AIdentity);
end;

function TNyxStudioSectionViews.FocusFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxStudioSectionFocus;
begin
  Result := ViewFor(AID).FocusFor(AID, AIdentity);
end;

function TNyxStudioSectionViews.FocusControl(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): Boolean;
var
  LFrame: TSectionFrame;
  LLifetime: TFrameLifetime;
  LLease: IInterface;
  LFocus: TNyxStudioSectionFocus;
  {$ifndef PAS2JS}
  LParent: TWinControl;
  {$endif}
begin
  Result := False;

  if (FFrame = nil) or (RootFor(AID) = nil) then
  begin
    Exit;
  end;
  LFrame := TSectionFrame(FFrame);
  LLifetime := LFrame.Lifetime;
  LLease := LFrame.LifetimeLease;
  { Explicit reveal may emit scroll/focus callbacks. Keep composition guarded
    across those callbacks and retain only the independent frame-liveness lease. }
  Inc(FInEvent);
  try
    ViewFor(AID).Reveal(AID, AIdentity);

    if (LLease = nil) or not LLifetime.Current then
    begin
      Exit;
    end;
    LFocus := FocusFor(AID, AIdentity);

    if LFocus = nil then
    begin
      Exit;
    end;
    {$ifndef PAS2JS}

    if not LFocus.CanFocus or not LFrame.Host.IsParentOf(LFocus) then
    begin
      Exit;
    end;
    { Independent section renderers stop at their own panel. Complete reveal
      through this aggregate's enclosing scroll controls, stopping before the
      borrowed application host. No renderer is granted another view's scope. }
    LParent := LFocus.Parent;
    while (LParent <> nil) and (LParent <> LFrame.Host) do
    begin

      if LParent is TScrollingWinControl then
      begin
        TScrollingWinControl(LParent).ScrollInView(LFocus);

        if not LLifetime.Current then
        begin
          Exit;
        end;
      end;
      LParent := LParent.Parent;
    end;
    {$endif}
    {$ifdef PAS2JS}
    NyxFocusWithoutScroll(FocusFor(AID, AIdentity));
    {$else}
    ViewFor(AID).FocusWithoutScroll(AID, AIdentity);
    {$endif}
    Result := LLifetime.Current;
  finally
    Dec(FInEvent);
  end;
end;

function TNyxStudioSectionViews.CollectionView(const AID: TNyxText): INyxCollectionView;
begin
  Result := ViewFor(AID).CollectionView(AID);
end;

function TNyxStudioSectionViews.BindCollection(const AID: TNyxText;
  const AView: INyxCollectionView): INyxCollectionMount;
begin
  Result := ViewFor(AID).BindCollection(AID, AView);
end;

function TNyxStudioSectionViews.CollectionMount(const AID: TNyxText): INyxCollectionMount;
begin
  Result := ViewFor(AID).CollectionMount(AID);
end;

function TNyxStudioSectionViews.ScreenPointFor(const AID: TNyxText;
  const APointer: TNyxPointerSnapshot): TNyxResizePoint;
begin
  Result := ViewFor(AID).ScreenPointFor(AID, APointer);
end;

procedure TNyxStudioSectionViews.Sync;
var
  LSection: TNyxStudioSection;
  LView: TNyxStudioSectionRenderer;
begin
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin
    LView := SectionView(LSection);

    if LView <> nil then
    begin
      LView.Sync;
    end;
  end;
end;

end.
