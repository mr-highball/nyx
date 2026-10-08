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

unit nyx.application.state;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.resources,
  nyx.types,
  nyx.state,
  nyx.publication,
  nyx.collections.registry,
  nyx.collections.view,
  nyx.collections.bindings,
  nyx.resource.context,
  nyx.application.resources,
  nyx.model;

type
  { Owns an application's runtime scalar store and admitted page prototypes.
    The authored document is borrowed and must remain unchanged while mounted;
    its defaults and nodes are never mutated by application interaction.

    All pages, including unmounted pages, validate each proposed store snapshot.
    This prevents admitting an invalid range/key/kind that fails only on later
    navigation. Prototypes retain no target handles. Subscription disconnects
    before prototypes/store are freed. Renderers borrow State and must unmount
    before this owner is destroyed. }
  TNyxApplicationState = class
  private
    FState: TNyxState;
    FCollections: INyxCollections;
    FCollectionContext: INyxCollectionContext;
    FPageCollections: array of INyxCollectionBindings;
    FPages: array of TNyxNode;
    FSubscription: TNyxStateSubscription;
    FResourceSubscription: INyxResourceSubscription;
    FResourceContext: INyxResourceContext;
    FResourcePort: IInterface; { revocable weak receiver retained by preparations }
    FResourcePreparation: TObject;
    function GetResourcePublicationBusy: Boolean;
    procedure Initialize(ADocument: TNyxDocument; APlatform: TNyxPlatform;
      const ALocale, AFallback: TNyxLocaleRef);
    procedure ValidateCandidate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    function ValidateResources(const AContext: INyxResourceContext): Boolean;
    function PrepareResources(const AContext: INyxResourceContext;
      out APrepared: INyxPreparedPublication): Boolean;
  public
    { npfAny preserves portable host-neutral validation. Concrete application
      hosts supply their target so hidden pages retain its typed overrides. }
    constructor Create(ADocument: TNyxDocument; APlatform: TNyxPlatform = npfAny); overload;
    { Hosts seed saved resource rows at their configured initial locale before
      attaching validators. Later locale/source changes use joint publication. }
    constructor Create(ADocument: TNyxDocument; APlatform: TNyxPlatform;
      const ALocale, AFallback: TNyxLocaleRef); overload;
    destructor Destroy; override;
    property State: TNyxState read FState;
    { Navigation/recomposition refuses while the application's prepared resource
      frame is held. Stores remain readable; destructor revokes borrowed stages. }
    property ResourcePublicationBusy: Boolean read GetResourcePublicationBusy;
    { Managed runtime stores are independent of saved defaults and sibling
      applications. Keep the same registry through navigation. A retained store
      contains no application/document/renderer reference and can outlive us. }
    property Collections: INyxCollections read FCollections;
    { Retained page bindings share one context and keep hidden-page validation,
      instance data and selection alive through navigation. Unknown IDs reject. }
    function PageCollections(const APageID: TNyxText): INyxCollectionBindings;
    { Install before rendering. All hidden-page validators then use accepted
      runtime resources/locales, never the document's original fallback data.
      Disconnects its borrowed receivers before prototype/store destruction. }
    procedure AttachResources(const AResources: INyxApplicationResources);
  end;

implementation

uses
  SysUtils,
  nyx.composition,
  nyx.platform,
  nyx.binding;

type
  IApplicationResourcePort = interface
    ['{ECA03873-89E0-47E7-A514-CC19A10E24D5}']
    function Owner: TNyxApplicationState;
    procedure Revoke;
  end;
  TApplicationResourcePort = class(TInterfacedObject, IApplicationResourcePort)
  private
    FOwner: TNyxApplicationState;
  public
    constructor Create(AOwner: TNyxApplicationState);
    function Owner: TNyxApplicationState;
    procedure Revoke;
  end;
  TApplicationResourcePreparation = class(TInterfacedObject, INyxPreparedPublication)
  private
    FPort: IApplicationResourcePort;
    FHold: INyxStateSnapshotHold;
    FContext: INyxResourceContext;
    FPrevious: INyxResourceContext;
    FRows: INyxPreparedPublication;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
  public
    constructor Create(AOwner: TNyxApplicationState; const AContext: INyxResourceContext);
    destructor Destroy; override;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

constructor TApplicationResourcePort.Create(AOwner: TNyxApplicationState);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TApplicationResourcePort.Owner: TNyxApplicationState;
begin
  Result := FOwner;
end;

procedure TApplicationResourcePort.Revoke;
begin
  FOwner := nil;
end;

constructor TApplicationResourcePreparation.Create(AOwner: TNyxApplicationState;
  const AContext: INyxResourceContext);
var
  LIndex: Integer;
  LPage: TNyxNode;
begin
  inherited Create;

  if AOwner.FResourcePreparation <> nil then
  begin
    raise ENyxState.Create('Application resource publication is already prepared');
  end;
  FHold := AOwner.FState.HoldSnapshot;
  FPort := AOwner.FResourcePort as IApplicationResourcePort;
  AOwner.FResourcePreparation := Self;
  FContext := AContext;
  FPrevious := AOwner.FResourceContext;
  { Repeat hidden-page scalar admission against the held state; earlier ordered
    readiness validators may legitimately have changed state before this hold. }
  for LIndex := 0 to High(AOwner.FPages) do
  begin
    LPage := AOwner.FPages[LIndex].Clone;
    try
      LPage.BindResources(AContext.Snapshot, AContext.Locale, AContext.Fallback);
      ApplyNyxBindings(LPage, AOwner.FState);
    finally
      LPage.Free;
    end;
  end;
  FRows := PrepareNyxCollectionContextResources(AOwner.FCollectionContext,
    AContext.Snapshot, AContext.Locale, AContext.Fallback);
end;

destructor TApplicationResourcePreparation.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TApplicationResourcePreparation.Validate;
begin

  if FRetired or FValidated or (FPort.Owner = nil) then
  begin
    raise ENyxState.Create('Application resource preparation is retired or already admitted');
  end;
  FRows.Validate;
  FValidated := True;
end;

procedure TApplicationResourcePreparation.Install;
begin

  if FPort.Owner <> nil then
  begin
    FPort.Owner.FResourceContext := FContext;
  end;
  FRows.Install;
  FInstalled := True;
end;

procedure TApplicationResourcePreparation.Notify;
begin

  if not FInstalled or FRetired then
  begin
    raise ENyxState.Create('Application resource notification requires installed data');
  end;
  FRows.Notify;
end;

procedure TApplicationResourcePreparation.Retire;
var
  LOwner: TNyxApplicationState;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if FRows <> nil then
  begin
    FRows.Retire;
    FRows := nil;
  end;

  if FPort <> nil then
  begin
    LOwner := FPort.Owner;

    if (LOwner <> nil) and (LOwner.FResourcePreparation = Self) then
    begin
      LOwner.FResourcePreparation := nil;
    end;
  end;
  FHold := nil;
  FContext := nil;
  FPrevious := nil;
  FPort := nil;
end;

constructor TNyxApplicationState.Create(ADocument: TNyxDocument; APlatform: TNyxPlatform);
begin
  inherited Create;
  Initialize(ADocument, APlatform, NyxDefaultLocale, NyxDefaultLocale);
end;

constructor TNyxApplicationState.Create(ADocument: TNyxDocument; APlatform: TNyxPlatform;
  const ALocale, AFallback: TNyxLocaleRef);
begin
  inherited Create;
  Initialize(ADocument, APlatform, ALocale, AFallback);
end;

procedure TNyxApplicationState.Initialize(ADocument: TNyxDocument; APlatform: TNyxPlatform;
  const ALocale, AFallback: TNyxLocaleRef);
var
  LIndex: Integer;
begin

  if (ADocument = nil) or (ADocument.Count = 0) then
  begin
    raise ENyxState.Create('An application requires at least one page');
  end;
  ADocument.Validate;
  FResourcePort := TApplicationResourcePort.Create(Self);
  FState := ADocument.State.Clone;
  FCollectionContext := NewNyxCollectionContext(ADocument.Collections, ADocument.Resources,
    ALocale, AFallback);
  FCollections := FCollectionContext.Collections;
  SetLength(FPages, ADocument.Count);
  SetLength(FPageCollections, ADocument.Count);
  for LIndex := 0 to ADocument.Count - 1 do
  begin
    FPages[LIndex] := RealizeNyxView(ADocument, ADocument.Pages[LIndex]);
    FPages[LIndex].BindResources(ADocument.Resources, ALocale, AFallback);

    if APlatform <> npfAny then
    begin
      ApplyNyxPlatform(FPages[LIndex], APlatform);
    end;
    ApplyNyxBindings(FPages[LIndex], FState);
    FPageCollections[LIndex] := NewNyxCollectionBindings(FPages[LIndex], FCollectionContext);
  end;
  FSubscription := FState.Subscribe(nil, ValidateCandidate);
end;

destructor TNyxApplicationState.Destroy;
var
  LIndex: Integer;
begin

  if FResourcePort <> nil then
  begin
    (FResourcePort as IApplicationResourcePort).Revoke;
  end;

  if FResourceSubscription <> nil then
  begin
    FResourceSubscription.Disconnect;
    FResourceSubscription := nil;
  end;
  FSubscription.Free;
  FPageCollections := nil;
  for LIndex := 0 to Length(FPages) - 1 do
  begin
    FPages[LIndex].Free;
  end;
  FState.Free;
  FCollections := nil;
  FCollectionContext := nil;
  inherited Destroy;
end;

procedure TNyxApplicationState.ValidateCandidate(ACandidate: TNyxState;
  AChanges: TNyxStateChanges);
var
  LIndex: Integer;
  LPage: TNyxNode;
begin
  for LIndex := 0 to Length(FPages) - 1 do
  begin
    LPage := FPages[LIndex].Clone;
    try

      if FResourceContext <> nil then
      begin
        LPage.BindResources(FResourceContext.Snapshot,
          FResourceContext.Locale, FResourceContext.Fallback);
      end;
      ApplyNyxBindings(LPage, ACandidate);
    finally
      LPage.Free;
    end;
  end;
end;

function TNyxApplicationState.ValidateResources(const AContext: INyxResourceContext): Boolean;
var
  LIndex: Integer;
  LPage: TNyxNode;
begin
  Result := not FState.Busy;

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to Length(FPages) - 1 do
  begin
    LPage := FPages[LIndex].Clone;
    try
      LPage.BindResources(AContext.Snapshot, AContext.Locale, AContext.Fallback);
      ApplyNyxBindings(LPage, FState);
    finally
      LPage.Free;
    end;
  end;
end;

function TNyxApplicationState.GetResourcePublicationBusy: Boolean;
begin
  Result := (FResourcePreparation <> nil) or FState.Busy;
end;

function TNyxApplicationState.PrepareResources(const AContext: INyxResourceContext;
  out APrepared: INyxPreparedPublication): Boolean;
begin
  APrepared := TApplicationResourcePreparation.Create(Self, AContext);
  Result := True;
end;

procedure TNyxApplicationState.AttachResources(const AResources: INyxApplicationResources);
var
  LToken: INyxResourceSubscription;
  LStage: TApplicationResourcePreparation;
  LPrepared: INyxPreparedPublication;
  LOwner: TNyxApplicationState;
  LPort: IApplicationResourcePort;
begin

  if (AResources = nil) or (FResourceSubscription <> nil) then
  begin
    raise ENyxState.Create('Application resource validation must be installed once');
  end;

  if not ValidateResources(AResources.Context) then
  begin
    raise ENyxState.Create('Application state is busy');
  end;
  { An embedding caller may supply a different initial locale from Create.
    Prepare its source rows as well as scalar admission before subscribing.
    Install the borrowed token before notification so disposal can revoke it. }
  LStage := TApplicationResourcePreparation.Create(Self, AResources.Context);
  LPrepared := LStage;
  LPort := LStage.FPort;
  try
    LToken := NyxPreparedApplicationResources(AResources).SubscribePrepared(
      ValidateResources, PrepareResources);
    FResourceSubscription := LToken;
    try
      PublishNyxGroup([LPrepared]);
    except
      on ENyxPublicationNotification do
      begin
        { Accepted frame and live subscription survive an observer failure. }
        raise;
      end;
      on Exception do
      begin
        LToken.Disconnect;
        LOwner := LPort.Owner;

        if LOwner <> nil then
        begin
          LOwner.FResourceSubscription := nil;
        end;
        raise;
      end;
    end;
  finally
    LPrepared.Retire;
  end;
end;

function TNyxApplicationState.PageCollections(const APageID: TNyxText): INyxCollectionBindings;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FPages) - 1 do
  begin

    if FPages[LIndex].ID = APageID then
    begin
      Exit(FPageCollections[LIndex]);
    end;
  end;
  raise ENyxState.Create('Unknown application page: ' + APageID);
end;

end.
