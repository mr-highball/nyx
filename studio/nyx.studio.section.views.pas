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
  {$else}, Controls, ExtCtrls, nyx.render.lcl, nyx.view.sections.lcl{$endif};

type
  {$ifdef PAS2JS}
  TNyxStudioSectionRenderer = TNyxBrowserRenderer;
  TNyxStudioSectionHost = TJSHTMLElement;
  TNyxStudioSectionControl = TJSHTMLElement;
  TNyxStudioSectionFocus = TJSHTMLElement;
  TNyxStudioSectionConfigure = TNyxBrowserSectionConfigure;
  {$else}
  TNyxStudioSectionRenderer = TNyxLCLRenderer;
  TNyxStudioSectionHost = TWinControl;
  TNyxStudioSectionControl = TControl;
  TNyxStudioSectionFocus = TWinControl;
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
      physical extension failures propagate through ordinary target admission. }
    procedure Render(ADocument: TNyxDocument; ARoot: TNyxNode;
      AHost: TNyxStudioSectionHost; ADesignMode: Boolean = False;
      AState: TNyxState = nil);
    { True admits compatible retained changes and grouped side replacements.
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

type
  {$ifdef PAS2JS}
  ISection = INyxBrowserViewSection;
  {$else}
  ISection = INyxLCLViewSection;
  {$endif}

  TSectionObserver = class(TInterfacedObject, INyxViewSectionObserver)
  public
    Owner: TNyxStudioSectionViews; { borrowed, revoked before destruction }
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

  TSectionFrame = class
  public
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

function NewFrameHost(AHost: TNyxStudioSectionHost): TNyxStudioSectionHost;
{$ifndef PAS2JS}
var
  LPanel: TPanel;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('div'));
  Result.style.cssText := 'position:fixed;left:-100000px;top:0;visibility:hidden;pointer-events:none;';
  Result.style.setProperty('width', IntToStr(AHost.clientWidth) + 'px');
  Result.style.setProperty('height', IntToStr(AHost.clientHeight) + 'px');
  Result.setAttribute('inert', '');
  Result.setAttribute('data-nyx-shell-frame', '');
  AHost.parentNode.appendChild(Result);
  {$else}
  LPanel := TPanel.Create(nil);
  LPanel.BevelOuter := bvNone;
  LPanel.Visible := False;
  LPanel.Parent := AHost;
  LPanel.SetBounds(0, 0, AHost.ClientWidth, AHost.ClientHeight);
  Result := LPanel;
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
  Result.style.cssText := 'width:100%;height:100%;min-width:0;min-height:0;';
  AParent.appendChild(Result);
  {$else}
  LPanel := TPanel.Create(nil);
  LPanel.BevelOuter := bvNone;
  LPanel.Parent := AParent;
  LPanel.Align := alClient;
  Result := LPanel;
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
      AOwner.FConfigure, ObserverLease, AOwner.FTheme);
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
  AHost: TNyxStudioSectionHost; ADesignMode: Boolean; AState: TNyxState);
var
  LNext: TSectionFrame;
  LPrevious: TObject;
begin

  if Dispatching or ADesignMode or (AState <> nil) or (ADocument = nil) or
    (ADocument.Count <> 1) or (ARoot <> ADocument.Pages[0]) or (AHost = nil) then
  begin
    raise ENyxModel.Create('Studio shell frames require an idle owning host and shared shell document');
  end;
  FHost := AHost;
  LNext := TSectionFrame.Create(Self, ADocument);
  try
    { Candidate controls/compounds have admitted before any accepted host leaves
      the display. The surrounding renderer owns only public Nyx chrome. }
    RevealFrame(LNext, FHost);
    LPrevious := FFrame;
    FFrame := LNext;
    LNext := nil;
    LPrevious.Free;
  finally
    LNext.Free;
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

  procedure RestoreRetained;
  var
    LRole: TNyxStudioSection;
    LOwner: TNyxStudioSectionRenderer;
    LFailure: TNyxText;
  begin
    LFailure := '';
    for LRole := High(TNyxStudioSection) downto Low(TNyxStudioSection) do
    begin

      if not LRetained[LRole] then
      begin
        Continue;
      end;
      { Recovery attempts each role at most once; a failed recovery must not
        exchange an already restored runtime tree twice. }
      LRetained[LRole] := False;
      LOwner := LFrame.View(LRole);

      try

        if not LOwner.TryRefresh(LFrame.Documents[LRole],
          LFrame.Documents[LRole].Pages[0], False) then
        begin
          raise ENyxModel.Create('Retained rollback context changed');
        end;
        { Replay restores the private source/arrangement baseline. The exact
          realized clone restores runtime properties in that same admitted
          tree; trusted exchange performs no callbacks/allocation. }
        LOwner.Root.ExchangeResourceProjection(LBefore[LRole]);
        LOwner.Sync;
        { Physical drafts can differ from that restored accepted value. Apply
          the copied adapter continuity only after source/model replay, while
          the exact retained controls and their event scopes still belong here. }

        if not LOwner.RestoreInteraction(LInputs[LRole]) then
        begin
          raise ENyxModel.Create('Retained rollback input boundary became busy');
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
      raise ENyxModel.Create('Studio section recovery failed' + LFailure);
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
  LDocuments := TNyxStudioSectionDocuments.Create(ADocument, FContext);
  LChanges := nil;
  LPublished := False;
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
      LBefore[LSection] := LView.Root.Clone;

      if not LView.CaptureInteraction(LInputs[LSection]) then
      begin
        Exit;
      end;
    end;
    try
      for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
      begin
        LView := LFrame.View(LSection);

        if LView = nil then
        begin
          Continue;
        end;

        { Even a target Sync exception can have touched the physical draft.
          Ordinary TryRefresh recovers its model; the outer owner must also
          restore continuity for this attempted retained role. }
        LRetained[LSection] := True;

        if LView.TryRefresh(LDocuments[LSection], LDocuments[LSection].Pages[0], False) then
        begin
          Continue;
        end
        else
        begin
          LRetained[LSection] := False; { False promises no admitted changes. }
        end;

        if LSection = nssChrome then
        begin
          RestoreRetained;
          Exit;
        end
        else
        begin
          SetLength(LChanges, Length(LChanges) + 1);
          LChanges[High(LChanges)] := LFrame.Sections[Ord(LSection)].Prepare(
            LDocuments[LSection], LDocuments[LSection].Pages[0]);
        end;
      end;

      if not PublishNyxViewSections(LChanges) then
      begin
        RestoreRetained;
        Exit;
      end;
      LPublished := True;
      LFrame.UpdateRoots;
      LFrame.Documents.Free;
      LFrame.Documents := LDocuments;
      LDocuments := nil;
      Result := True;
    except

      if not LPublished then
      begin
        RestoreRetained;
      end;
      raise;
    end;
  finally
    LChanges := nil;
    LDocuments.Free;
    for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      LBefore[LSection].Free;
      LInputs[LSection] := nil;
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
