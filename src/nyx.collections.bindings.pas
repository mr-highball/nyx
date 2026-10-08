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
unit nyx.collections.bindings;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.resources,
  nyx.model,
  nyx.content.mount,
  nyx.collections.view,
  nyx.collections.view.types;

type
  { Owned, target-independent view set for one realized page. An application
    retains one set per page, so data/selection and hidden-page validators survive
    navigation. Renderers borrow through a retained interface, never back into
    the application or node tree. Each view owns its store and subscription. }
  INyxCollectionBindings = interface
    ['{C61A33E5-0621-4B89-854A-8D6E0598DA17}']
    function GetCount: Integer;
    function ID(AIndex: Integer): TNyxText;
    function View(AIndex: Integer): INyxCollectionView;
    function ViewFor(const AID: TNyxText): INyxCollectionView;
    { Refuse reuse against a different realized contract before a renderer
      replaces accepted controls. IDs, scopes, projection and specs must match. }
    procedure ValidateRoot(ARoot: TNyxNode);
    { Admit an independent active view set against the same owned runtime
      context. Recipe changes retain live data and previously resolved scopes;
      no document collection defaults are re-imported into those stores. }
    function Recompose(ARoot: TNyxNode): INyxCollectionBindings;
    property Count: Integer read GetCount;
  end;

  { Optional source admission preserves the original view-set interface/GUID.
    Built-in sets delegate to their independent runtime context. Until prepared
    scalar and row publication share one application commit, changing a saved
    dataset refuses rather than publishing new captions beside stale tables. }
  INyxResourceCollectionBindings = interface
    ['{48BCD751-B2A8-4F68-BA96-45436F46B271}']
    procedure ValidateResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
  end;

{ Qualify a proposed resource frame before any scalar publication. Alternative
  static binding sets retain their original behavior without this capability. }
procedure ValidateNyxCollectionBindingResources(const ABindings: INyxCollectionBindings;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef);

{ Shared primitive admission; custom semantic kinds use their realized
  ProjectionKind. Unsupported physical projections fail with the control ID. }
function NyxCollectionProjectionForNode(ANode: TNyxNode): TNyxCollectionProjection;
function NewNyxCollectionBindings(ARoot: TNyxNode;
  const AContext: INyxCollectionContext): INyxCollectionBindings;

implementation

uses
  nyx.collections, nyx.collections.selection;

type
  TBindings = class(TInterfacedObject, INyxCollectionBindings, INyxResourceCollectionBindings)
  private
    FContext: INyxCollectionContext;
    FIDs: array of TNyxText;
    FScopes: array of TNyxText;
    FIdentities: array of TNyxContentIdentity;
    FViews: array of INyxCollectionView;
    procedure AddNode(ANode: TNyxNode);
  public
    constructor Create(ARoot: TNyxNode; const AContext: INyxCollectionContext);
    function GetCount: Integer;
    function ID(AIndex: Integer): TNyxText;
    function View(AIndex: Integer): INyxCollectionView;
    function ViewFor(const AID: TNyxText): INyxCollectionView;
    procedure ValidateRoot(ARoot: TNyxNode);
    function Recompose(ARoot: TNyxNode): INyxCollectionBindings;
    procedure ValidateResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
  end;

procedure ValidateNyxCollectionBindingResources(const ABindings: INyxCollectionBindings;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef);
var
  LBindings: INyxResourceCollectionBindings;
begin

  if (ABindings <> nil) and Supports(ABindings, INyxResourceCollectionBindings, LBindings) then
  begin
    LBindings.ValidateResources(AResources, ALocale, AFallback);
  end;
end;

procedure TBindings.ValidateResources(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef);
begin
  ValidateNyxCollectionContextResources(FContext, AResources, ALocale, AFallback);
end;

function NyxCollectionProjectionForNode(ANode: TNyxNode): TNyxCollectionProjection;
begin

  if ANode = nil then
  begin
    raise ENyxCollection.Create('Collection projection requires a control');
  end;

  if ANode.ProjectionKind = 'list' then
  begin
    Exit(cpList);
  end;

  if ANode.ProjectionKind = 'table' then
  begin
    Exit(cpTable);
  end;

  if ANode.ProjectionKind = 'tree' then
  begin
    Exit(cpTree);
  end;
  raise ENyxCollection.Create('Unsupported collection projection on ' + ANode.ID);
end;

constructor TBindings.Create(ARoot: TNyxNode; const AContext: INyxCollectionContext);
begin
  inherited Create;

  if (ARoot = nil) or not ARoot.IsRealized or (AContext = nil) then
  begin
    raise ENyxCollection.Create('Collection bindings require a realized root and context');
  end;
  FContext := AContext;
  AddNode(ARoot);
end;

procedure TBindings.AddNode(ANode: TNyxNode);
var
  LIndex: Integer;
  LSpec: TNyxCollectionViewSpec;
  LView: INyxCollectionView;
begin

  if ANode.HasCollectionView then
  begin
    LSpec := ANode.CollectionView;

    if LSpec.Defined then
    begin
      { Resolve/admit before growing the owned vectors. A failed constructor
        releases every earlier view/token and retains no borrowed node. }
      LView := NewNyxCollectionView(FContext.Resolve(LSpec, ANode.RuntimeInstanceOwner.ID),
        LSpec, NyxCollectionProjectionForNode(ANode));
      LIndex := Length(FViews);
      SetLength(FIDs, LIndex + 1);
      SetLength(FScopes, LIndex + 1);
      SetLength(FIdentities, LIndex + 1);
      SetLength(FViews, LIndex + 1);
      FIDs[LIndex] := ANode.ID;
      FScopes[LIndex] := ANode.RuntimeInstanceOwner.ID;
      FIdentities[LIndex] := NyxContentIdentity(ANode);
      FViews[LIndex] := LView;
    end;
  end;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    AddNode(ANode.Children[LIndex]);
  end;
end;

function TBindings.GetCount: Integer;
begin
  Result := Length(FViews);
end;

function TBindings.Recompose(ARoot: TNyxNode): INyxCollectionBindings;
var
  LCandidate: TBindings;
  LIndex: Integer;
  LPrevious: Integer;
  LMatch: Integer;
  LItem: Integer;
  LSelection: INyxCollectionSelection;
  LItems: array of TNyxItemRef;
begin
  LCandidate := TBindings.Create(ARoot, FContext);
  Result := LCandidate;
  for LIndex := 0 to LCandidate.GetCount - 1 do
  begin
    LMatch := -1;
    for LPrevious := 0 to GetCount - 1 do
    begin

      if not FIdentities[LPrevious].Same(LCandidate.FIdentities[LIndex]) then
      begin
        Continue;
      end;

      if LMatch >= 0 then
      begin
        raise ENyxCollection.Create('Ambiguous recipe collection part: ' + FIDs[LPrevious]);
      end;
      LMatch := LPrevious;
    end;

    if (LMatch < 0) or (FViews[LMatch].Store <> LCandidate.FViews[LIndex].Store) or
      (FViews[LMatch].Projection <> LCandidate.FViews[LIndex].Projection) or
      (FViews[LMatch].Spec.ToData.ToJSON <> LCandidate.FViews[LIndex].Spec.ToData.ToJSON) then
    begin
      Continue;
    end;
    { Only the compatible explicit part carries view selection. The store is
      already shared by stable runtime owner, independently of control IDs.
      No observer exists on the detached candidate during this publication. }
    LSelection := FViews[LMatch].Selection;
    SetLength(LItems, LSelection.Count);
    for LItem := 0 to High(LItems) do
    begin
      LItems[LItem] := LSelection.ItemAt(LItem);
    end;
    LCandidate.FViews[LIndex].SetSelection(LItems, LSelection.Focus, LSelection.Anchor);
  end;
end;

function TBindings.ID(AIndex: Integer): TNyxText;
begin
  View(AIndex);
  Result := FIDs[AIndex];
end;

function TBindings.View(AIndex: Integer): INyxCollectionView;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxCollection.Create('Collection binding index is out of range');
  end;
  Result := FViews[AIndex];
end;

function TBindings.ViewFor(const AID: TNyxText): INyxCollectionView;
var
  LIndex: Integer;
begin
  for LIndex := 0 to GetCount - 1 do
  begin

    if FIDs[LIndex] = AID then
    begin
      Exit(FViews[LIndex]);
    end;
  end;
  raise ENyxCollection.Create('Collection binding not found: ' + AID);
end;

procedure TBindings.ValidateRoot(ARoot: TNyxNode);
var
  LNext: Integer;

  procedure Visit(ANode: TNyxNode);
  var
    LChild: Integer;
    LSpec: TNyxCollectionViewSpec;
  begin

    if ANode.HasCollectionView then
    begin
      LSpec := ANode.CollectionView;

      if LSpec.Defined then
      begin

        if (LNext >= GetCount) or (FIDs[LNext] <> ANode.ID) or
          (FScopes[LNext] <> ANode.RuntimeInstanceOwner.ID) or
          (FViews[LNext].Projection <> NyxCollectionProjectionForNode(ANode)) or
          (FViews[LNext].Spec.ToData.ToJSON <> LSpec.ToData.ToJSON) then
        begin
          raise ENyxCollection.Create('Collection binding contract changed on ' + ANode.ID);
        end;
        Inc(LNext);
      end;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LChild]);
    end;
  end;

begin

  if (ARoot = nil) or not ARoot.IsRealized then
  begin
    raise ENyxCollection.Create('Collection binding reuse requires a realized root');
  end;
  LNext := 0;
  Visit(ARoot);

  if LNext <> GetCount then
  begin
    raise ENyxCollection.Create('Collection binding count changed');
  end;
end;

function NewNyxCollectionBindings(ARoot: TNyxNode;
  const AContext: INyxCollectionContext): INyxCollectionBindings;
begin
  Result := TBindings.Create(ARoot, AContext);
end;

end.
