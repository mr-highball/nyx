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
    procedure Initialize(ADocument: TNyxDocument; APlatform: TNyxPlatform;
      const ALocale, AFallback: TNyxLocaleRef);
    procedure ValidateCandidate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    function ValidateResources(const AContext: INyxResourceContext): Boolean;
    procedure ResourcesChanged(const AContext: INyxResourceContext);
  public
    { npfAny preserves portable host-neutral validation. Concrete application
      hosts supply their target so hidden pages retain its typed overrides. }
    constructor Create(ADocument: TNyxDocument; APlatform: TNyxPlatform = npfAny); overload;
    { Hosts seed saved resource rows at their configured initial locale before
      attaching validators. Later locale/source changes need joint publication. }
    constructor Create(ADocument: TNyxDocument; APlatform: TNyxPlatform;
      const ALocale, AFallback: TNyxLocaleRef); overload;
    destructor Destroy; override;
    property State: TNyxState read FState;
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
  nyx.composition,
  nyx.platform,
  nyx.binding;

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
  FState := ADocument.State.Clone;
  FCollectionContext := NewNyxCollectionContext(ADocument.Collections, ADocument.Resources,
    ALocale, AFallback);
  FCollections := FCollectionContext.Collections;
  SetLength(FPages, ADocument.Count);
  SetLength(FPageCollections, ADocument.Count);
  for LIndex := 0 to ADocument.Count - 1 do
  begin
    FPages[LIndex] := RealizeNyxView(ADocument, ADocument.Pages[LIndex]);

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
  ValidateNyxCollectionContextResources(FCollectionContext, AContext.Snapshot,
    AContext.Locale, AContext.Fallback);
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

procedure TNyxApplicationState.ResourcesChanged(const AContext: INyxResourceContext);
begin
  { The immutable frame is enough: future validation clones prototypes and binds
    this frame without rewriting retained collection scopes or defaults. }
  FResourceContext := AContext;
end;

procedure TNyxApplicationState.AttachResources(const AResources: INyxApplicationResources);
var
  LToken: INyxResourceSubscription;
begin

  if (AResources = nil) or (FResourceSubscription <> nil) then
  begin
    raise ENyxState.Create('Application resource validation must be installed once');
  end;

  if not ValidateResources(AResources.Context) then
  begin
    raise ENyxState.Create('Application state is busy');
  end;
  LToken := AResources.Subscribe(ValidateResources, ResourcesChanged);
  FResourceContext := AResources.Context;
  FResourceSubscription := LToken;
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
