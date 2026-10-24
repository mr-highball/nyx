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
unit nyx.projection.refresh;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.responsive, nyx.model, nyx.data;

type
  { A typed request to restore one exact field's accepted value during retained
    projection: the authored default when unbound, the current runtime store
    when bound. It owns only identity values, never a node or widget.
    An editor uses this after a completed/rejected proposal; unrelated runtime
    drafts remain independent. Empty or mismatched identities refuse reuse. }
  TNyxProjectionValueRestore = record
  private
    FRuntimeID: TNyxText;
    FDesignID: TNyxText;
  public
    class function ForField(const ARuntimeID, ADesignID: TNyxText):
      TNyxProjectionValueRestore; static;
    property RuntimeID: TNyxText read FRuntimeID;
    property DesignID: TNyxText read FDesignID;
  end;
  { Copied exact restoration requests; a caller may share this immutable input
    vector with a pending operation without the refresh appending into it. }
  TNyxProjectionValueRestores = array of TNyxProjectionValueRestore;

  { An explicit caller-owned form context. Only these named extension properties
    may change inside this exact runtime/design subtree. Built-in attributes,
    missing properties, owner identities and all other contracts keep ordinary
    strict admission. A changed context restores every value in this form to its
    newly accepted candidate, including unchanged defaults and absent values;
    unrelated forms retain independent drafts. No node or control is retained.
    This immutable value is a refresh policy, never persisted application data. }
  TNyxProjectionFormContext = record
  private
    FRuntimeID: TNyxText;
    FDesignID: TNyxText;
    FKeys: array of TNyxText;
  public
    class function ForForm(const ARuntimeID, ADesignID: TNyxText;
      const AKeys: array of TNyxExtensionRef): TNyxProjectionFormContext; static;
    function Permits(const AKey: TNyxText): Boolean;
    property RuntimeID: TNyxText read FRuntimeID;
    property DesignID: TNyxText read FDesignID;
  end;
  { Independent immutable form policies. Duplicate or nested scopes refuse as
    a complete group; one physical input cannot have two context-reset owners. }
  TNyxProjectionFormContexts = array of TNyxProjectionFormContext;

{ Portable adapter boundary for an independently realized candidate. These
  routines own no document, node or native/DOM handle. A retained view may reuse
  only ordinary scalar presentation and dimension allocation: identities,
  structure, contracts, binding
  descriptors, platform metadata and collection semantics must remain exact.
  Caller keeps both independent roots alive throughout checking/copying. }
function CanRefreshNyxProjection(AExisting, ACandidate: TNyxNode): Boolean;

{ Apply only a completely compatible tree. False changes nothing. Property
  collections are copied, never shared; callbacks/bindings/ownership remain on
  the existing realized nodes. Target adapters then call their normal Sync and
  retain an independent previous clone for rollback on a widget failure.
  Supplying ABaseline applies only changed authored fields, preserving independent
  runtime input for unchanged defaults. Nil copies all properties for rollback.
  The optional baseline must have the same completely compatible structure.
  Explicit field restores are checked before any mutation, then copy only Value
  from the fresh candidate. This also restores an absent authored property. }
function RefreshNyxProjectionProperties(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode = nil; const ARestores: TNyxProjectionValueRestores = nil): Boolean;

{ Separate structural admission for the exact same realized node set. The
  scalar guard above stays strict. Reparenting/reordering is permitted only at
  ordinary page/row/column/grid/panel/card/group/form/toolbar/sidebar hosts;
  split panes and other special hosts retain their full-mount requirement.
  Unchanged runtime/source/design identities and instance scopes preserve
  logical ownership. Every original scalar/contract/binding/collection check
  still applies after aligning independent comparison copies. False changes
  nothing. This does not admit alternate recipes, new nodes or live bindings. }
function CanArrangeNyxProjection(AExisting, ACandidate: TNyxNode): Boolean;

{ Compare direct child identity/order with the same host in an independent
  former arrangement. Adapters use this only after complete structural admission
  so private split panes, glyphs and unchanged hosts are never reattached. }
function NyxProjectionChildrenChanged(ANode, AFormerRoot: TNyxNode): Boolean;

{ Apply an admitted rearrangement and authored scalar deltas without adopting
  any candidate node. Baseline comparison follows runtime identity rather than
  child position, preserving independent drafts. Adapters retain a previous
  clone for model AND physical-parent rollback if a target operation fails.
  As with scalar copying, allocation failures raise; target owners must catch
  them before publication and restore their previous arrangement/properties. }
function RefreshNyxProjectionArrangement(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode = nil; const ARestores: TNyxProjectionValueRestores = nil): Boolean;

{ State-coordinator boundary. These explicit APIs additionally admit identical
  scalar binding descriptors; they do not change the strict unbound APIs above.
  The owning coordinator must project both candidate and baseline against the
  same idle runtime store before property admission/publication. A metadata
  comparison alone never proves current state or live control integration. }
function SameNyxProjectionBindingContracts(AExisting, ACandidate: TNyxNode): Boolean;
function CanRefreshNyxBoundProjection(AExisting, ACandidate: TNyxNode): Boolean;
function CanArrangeNyxBoundProjection(AExisting, ACandidate: TNyxNode): Boolean;
function RefreshNyxBoundProjectionProperties(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode = nil; const ARestores: TNyxProjectionValueRestores = nil): Boolean;
function RefreshNyxBoundProjectionArrangement(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode = nil; const ARestores: TNyxProjectionValueRestores = nil): Boolean;

{ Explicit form-context counterparts to the strict scalar APIs. The caller owns
  the meaning of its metadata keys and must retire/fence pending old-context
  actions before physical publication. These do not admit reparenting, altered
  bindings, recipes, factories, collection descriptors or document context.
  All forms, metadata and requested restores are checked before any write. }
function CanRefreshNyxFormProjection(AExisting, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts): Boolean;
function CanRefreshNyxBoundFormProjection(AExisting, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts): Boolean;
function RefreshNyxFormProjectionProperties(AExisting, ACandidate, ABaseline: TNyxNode;
  const AForms: TNyxProjectionFormContexts;
  const ARestores: TNyxProjectionValueRestores = nil): Boolean;
function RefreshNyxBoundFormProjectionProperties(AExisting, ACandidate, ABaseline: TNyxNode;
  const AForms: TNyxProjectionFormContexts;
  const ARestores: TNyxProjectionValueRestores = nil): Boolean;
{ Copied exact fields whose physical accepted-value markers must also reset.
  Compare against the previous authored baseline when one exists. False clears
  the output and changes neither tree. Context membership/presence is checked;
  complete projection compatibility remains the admission APIs' responsibility. }
function PrepareNyxProjectionFormRestores(ABefore, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts;
  out ARestores: TNyxProjectionValueRestores): Boolean; overload;
{ Metadata-only forms also advance context when they have no Value-bearing
  descendants. The changed flag is separate from the physical reset vector. }
function PrepareNyxProjectionFormRestores(ABefore, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts;
  out ARestores: TNyxProjectionValueRestores; out AContextChanged: Boolean): Boolean; overload;

{ Exact immutable document context for a mounted view. Fresh encoding validates
  current public data/budgets; it is not an admission cache. All document fields
  except title/pages/components remain significant, including defaults, tokens,
  collection registries and creator extension data. Requested-root realization
  separately checks structure and every node. Unrelated authored roots may
  change only when the requested realized view retains identical semantics.
  The codec reuses its fully admitted temporary JSON tree for this projection;
  no excluded source tree is decoded/indexed a second time or retained here. }
function NyxProjectionContext(ADocument: TNyxDocument): TNyxText;

implementation

uses
  nyx.codec, nyx.presentations, nyx.text.index;

type
  { Only the explicit coordinator boundary admits unchanged bindings. }
  TProjectionBindingAdmission = (pbaUnbound, pbaExact);

function SameNodeBindings(AExisting, ACandidate: TNyxNode): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if AExisting.BindingCount <> ACandidate.BindingCount then
  begin
    Exit;
  end;
  for LIndex := 0 to AExisting.BindingCount - 1 do
  begin

    if not AExisting.Bindings[LIndex].Same(ACandidate.Bindings[LIndex]) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

class function TNyxProjectionValueRestore.ForField(const ARuntimeID,
  ADesignID: TNyxText): TNyxProjectionValueRestore;
begin

  if (ARuntimeID = '') or (ADesignID = '') then
  begin
    raise ENyxModel.Create('Field restoration requires exact runtime and editable identities');
  end;
  Result.FRuntimeID := ARuntimeID;
  Result.FDesignID := ADesignID;
end;

class function TNyxProjectionFormContext.ForForm(const ARuntimeID,
  ADesignID: TNyxText; const AKeys: array of TNyxExtensionRef): TNyxProjectionFormContext;
var
  LIndex: Integer;
  LPrevious: Integer;
  LAttribute: TNyxAttribute;
  LViewport: TNyxViewportCondition;
  LPlatform: TNyxPlatform;
  LPresentation: TNyxPresentationRef;
  LContext: TNyxProjectionFormContext;
begin

  if (ARuntimeID = '') or (ADesignID = '') or (Length(AKeys) = 0) then
  begin
    raise ENyxModel.Create('Form refresh requires exact identities and explicit metadata keys');
  end;
  LContext.FRuntimeID := ARuntimeID;
  LContext.FDesignID := ADesignID;
  SetLength(LContext.FKeys, Length(AKeys));
  for LIndex := 0 to High(AKeys) do
  begin

    if (AKeys[LIndex].Name = '') or TryNyxAttribute(AKeys[LIndex].Name, LAttribute) or
      TryNyxViewportKey(AKeys[LIndex].Name, LViewport, LPlatform, LAttribute) or
      TryNyxPresentationKey(AKeys[LIndex].Name, LPresentation, LPlatform, LAttribute) then
    begin
      raise ENyxModel.Create('A form context may name only extension metadata');
    end;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if LContext.FKeys[LPrevious] = AKeys[LIndex].Name then
      begin
        raise ENyxModel.Create('A form context cannot repeat a metadata key');
      end;
    end;
    LContext.FKeys[LIndex] := AKeys[LIndex].Name;
  end;
  Result := LContext;
end;

function TNyxProjectionFormContext.Permits(const AKey: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to High(FKeys) do
  begin

    if FKeys[LIndex] = AKey then
    begin
      Exit(True);
    end;
  end;
end;

function ValidFormContexts(ABefore, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts): Boolean;
var
  LIndex: Integer;
  LPrevious: Integer;
  LBefore: TNyxNode;
  LAfter: TNyxNode;
  LParent: TNyxNode;
begin
  Result := False;

  if (ABefore = nil) or (ACandidate = nil) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(AForms) do
  begin

    if (AForms[LIndex].RuntimeID = '') or (AForms[LIndex].DesignID = '') or
      (Length(AForms[LIndex].FKeys) = 0) then
    begin
      Exit;
    end;
    LBefore := ABefore.Find(AForms[LIndex].RuntimeID);
    LAfter := ACandidate.Find(AForms[LIndex].RuntimeID);

    if (LBefore = nil) or (LAfter = nil) or
      (LBefore.DesignID <> AForms[LIndex].DesignID) or
      (LAfter.DesignID <> AForms[LIndex].DesignID) then
    begin
      Exit;
    end;
    for LPrevious := 0 to LIndex - 1 do
    begin
      LParent := LBefore;
      while LParent <> nil do
      begin

        if LParent.ID = AForms[LPrevious].RuntimeID then
        begin
          Exit;
        end;
        LParent := LParent.Parent;
      end;
      LParent := ABefore.Find(AForms[LPrevious].RuntimeID);
      while LParent <> nil do
      begin

        if LParent.ID = LBefore.ID then
        begin
          Exit;
        end;
        LParent := LParent.Parent;
      end;
      { Reparented candidate scopes are never admitted by scalar comparison;
        independently reject nesting there before producing a restore plan. }
      LParent := LAfter;
      while LParent <> nil do
      begin

        if LParent.ID = AForms[LPrevious].RuntimeID then
        begin
          Exit;
        end;
        LParent := LParent.Parent;
      end;
      LParent := ACandidate.Find(AForms[LPrevious].RuntimeID);
      while LParent <> nil do
      begin

        if LParent.ID = LAfter.ID then
        begin
          Exit;
        end;
        LParent := LParent.Parent;
      end;
    end;
  end;
  Result := True;
end;

function PrepareNyxProjectionFormRestores(ABefore, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts;
  out ARestores: TNyxProjectionValueRestores; out AContextChanged: Boolean): Boolean;
var
  LIndex: Integer;
  LChanged: Boolean;
  LCount: Integer;

  function CompareContext(AFrom, ATo: TNyxNode;
    const AForm: TNyxProjectionFormContext): Boolean;
  var
    LKey: Integer;
    LChild: Integer;
    LFrom: Boolean;
    LTo: Boolean;
  begin
    Result := False;

    if (AFrom.ID <> ATo.ID) or (AFrom.DesignID <> ATo.DesignID) or
      (AFrom.Count <> ATo.Count) then
    begin
      Exit;
    end;
    for LKey := 0 to High(AForm.FKeys) do
    begin
      LFrom := AFrom.Props.IndexOfName(AForm.FKeys[LKey]) >= 0;
      LTo := ATo.Props.IndexOfName(AForm.FKeys[LKey]) >= 0;

      if LFrom <> LTo then
      begin
        Exit;
      end;
      LChanged := LChanged or (LFrom and
        (AFrom.StoredProp(AForm.FKeys[LKey]) <> ATo.StoredProp(AForm.FKeys[LKey])));
    end;
    for LChild := 0 to AFrom.Count - 1 do
    begin

      if not CompareContext(AFrom.Children[LChild], ATo.Children[LChild], AForm) then
      begin
        Exit;
      end;
    end;
    Result := True;
  end;

  procedure Collect(AFrom, ATo: TNyxNode);
  var
    LChild: Integer;
    LKind: TNyxText;
  begin
    LKind := ATo.ProjectionKind;

    if (AFrom.Props.IndexOfName(NyxAttributeName(atValue)) >= 0) or
      (ATo.Props.IndexOfName(NyxAttributeName(atValue)) >= 0) or
      (LKind = 'input') or (LKind = 'memo') or (LKind = 'select') or
      (LKind = 'spin') or (LKind = 'checkbox') or (LKind = 'radio') or
      (LKind = 'slider') or (LKind = 'range') or (LKind = 'date') or
      (LKind = 'time') or (LKind = 'color') or (LKind = 'code-editor') then
    begin

      if LCount = Length(ARestores) then
      begin
        SetLength(ARestores, LCount + 16);
      end;
      ARestores[LCount] := TNyxProjectionValueRestore.ForField(ATo.ID, ATo.DesignID);
      Inc(LCount);
    end;
    for LChild := 0 to ATo.Count - 1 do
    begin
      Collect(AFrom.Children[LChild], ATo.Children[LChild]);
    end;
  end;

begin
  ARestores := nil;
  AContextChanged := False;
  Result := False;

  if not ValidFormContexts(ABefore, ACandidate, AForms) then
  begin
    Exit;
  end;
  LCount := 0;
  for LIndex := 0 to High(AForms) do
  begin
    LChanged := False;

    if not CompareContext(ABefore.Find(AForms[LIndex].RuntimeID),
      ACandidate.Find(AForms[LIndex].RuntimeID), AForms[LIndex]) then
    begin
      ARestores := nil;
      AContextChanged := False;
      Exit;
    end;

    if LChanged then
    begin
      AContextChanged := True;
      Collect(ABefore.Find(AForms[LIndex].RuntimeID),
        ACandidate.Find(AForms[LIndex].RuntimeID));
    end;
  end;
  SetLength(ARestores, LCount);
  Result := True;
end;

function PrepareNyxProjectionFormRestores(ABefore, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts;
  out ARestores: TNyxProjectionValueRestores): Boolean;
var
  LChanged: Boolean;
begin
  Result := PrepareNyxProjectionFormRestores(ABefore, ACandidate, AForms, ARestores, LChanged);
end;

function RefreshableKey(const AKey: TNyxText): Boolean;
var
  LAttribute: TNyxAttribute;
  LViewport: TNyxViewportCondition;
  LPlatform: TNyxPlatform;
  LPresentation: TNyxPresentationRef;
begin
  Result := TryNyxAttribute(AKey, LAttribute) and
    (LAttribute in [atText, atValue, atHint, atAccessibleName, atEnabled,
      atVisible, atReadOnly, atWidth, atHeight, atFlex, atWidthSizing,
      atHeightSizing, atMinimumWidth, atMaximumWidth, atMinimumHeight, atMaximumHeight,
      atLeft, atTop]);
  { Absolute-origin changes run through ordinary allocation just like widths.
    Retaining the identical control shape preserves live input and focus during
    a paired move; no creator/ownership change is admitted by this exception. }
  { A responsive flow changes allocation of identical children, never their
    primitive/factory type. Restrict retained admission to properties consumed
    by ordinary target Sync/layout; constructor/asset/extension changes refuse. }

  if TryNyxViewportKey(AKey, LViewport, LPlatform, LAttribute) or
    TryNyxPresentationKey(AKey, LPresentation, LPlatform, LAttribute) then
  begin
    Result := LAttribute in [atText, atHint, atAccessibleName, atEnabled, atVisible,
      atReadOnly, atWidth, atHeight, atFlex, atWidthSizing, atHeightSizing,
      atMinimumWidth, atMaximumWidth, atMinimumHeight, atMaximumHeight,
      atLayout, atGap, atPadding, atColumns, atLeft, atTop, atFlowWrap,
      atCrossAlignment, atJustification];
  end;
  { These dimensions only change allocation on an already identical admitted
    shape. Ordinary layout-mode/platform/binding/creator changes still refuse.
    Both adapters use their ordinary Sync/layout and existing rollback clone. }
end;

function CanRefreshProjection(AExisting, ACandidate: TNyxNode;
  ABindings: TProjectionBindingAdmission;
  const AForms: TNyxProjectionFormContexts = nil): Boolean;
var
  LIndex: Integer;
  LKey: TNyxText;

  function ContextMetadata(AFrom, ATo: TNyxNode; const AName: TNyxText): Boolean;
  var
    LForm: Integer;
    LAncestor: TNyxNode;
  begin
    Result := False;

    if (AFrom.Props.IndexOfName(AName) < 0) or (ATo.Props.IndexOfName(AName) < 0) then
    begin
      Exit;
    end;
    for LForm := 0 to High(AForms) do
    begin

      if not AForms[LForm].Permits(AName) then
      begin
        Continue;
      end;
      LAncestor := AFrom;
      while LAncestor <> nil do
      begin

        if (LAncestor.ID = AForms[LForm].RuntimeID) and
          (LAncestor.DesignID = AForms[LForm].DesignID) then
        begin
          Exit(True);
        end;
        LAncestor := LAncestor.Parent;
      end;
    end;
  end;

  function CompatibleProperties(AFrom, ATo: TNyxNode): Boolean;
  var
    LProperty: Integer;
    LPresentation: TNyxPresentationRef;
    LPlatform: TNyxPlatform;
    LAttribute: TNyxAttribute;
  begin
    Result := False;
    for LProperty := 0 to AFrom.Props.Count - 1 do
    begin
      LKey := AFrom.Props.Names[LProperty];

      if TryNyxPresentationKey(LKey, LPresentation, LPlatform, LAttribute) and
        not RefreshableKey(LKey) then
      begin

        if not ATo.PresentationSnapshot.Contains(LPresentation) or
          not AFrom.PresentationSnapshot.Definition(LPresentation).Same(
            ATo.PresentationSnapshot.Definition(LPresentation)) then
        begin
          Exit;
        end;
      end;

      if ((ATo.Props.IndexOfName(LKey) < 0) or
        (AFrom.StoredProp(LKey) <> ATo.StoredProp(LKey))) and
        not RefreshableKey(LKey) and not ContextMetadata(AFrom, ATo, LKey) then
      begin
        Exit;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;

  if (AExisting = nil) or (ACandidate = nil) then
  begin
    Exit;
  end;

  if (AExisting.Kind <> ACandidate.Kind) or
    (AExisting.ID <> ACandidate.ID) or
    (AExisting.SourceID <> ACandidate.SourceID) or
    (AExisting.DesignID <> ACandidate.DesignID) or
    (AExisting.InstanceScopeID <> ACandidate.InstanceScopeID) or
    (AExisting.RecipeOwner.ID <> ACandidate.RecipeOwner.ID) or
    (AExisting.ProjectionKind <> ACandidate.ProjectionKind) or
    (AExisting.IsRealized <> ACandidate.IsRealized) or
    (AExisting.Count <> ACandidate.Count) or
    ((ABindings = pbaUnbound) and
      ((AExisting.BindingCount <> 0) or (ACandidate.BindingCount <> 0))) or
    (AExisting.HasCollectionView <> ACandidate.HasCollectionView) or
    (AExisting.HasMenu <> ACandidate.HasMenu) or
    (AExisting.MenuReference.Name <> ACandidate.MenuReference.Name) or
    (AExisting.HasCanvasScene <> ACandidate.HasCanvasScene) or
    (AExisting.CanvasScene.Defined <> ACandidate.CanvasScene.Defined) or
    (AExisting.CanvasScene.Defined and ACandidate.CanvasScene.Defined and
      (AExisting.CanvasScene.Name <> ACandidate.CanvasScene.Name)) or
    (AExisting.Extensions.ToJSON <> ACandidate.Extensions.ToJSON) then
  begin
    Exit;
  end;

  if (ABindings = pbaExact) and not SameNodeBindings(AExisting, ACandidate) then
  begin
    Exit;
  end;

  if AExisting.HasCollectionView and
    (AExisting.CollectionView.ToData.ToJSON <> ACandidate.CollectionView.ToData.ToJSON) then
  begin
    Exit;
  end;

  if not CompatibleProperties(AExisting, ACandidate) or
    not CompatibleProperties(ACandidate, AExisting) then
  begin
    Exit;
  end;
  for LIndex := 0 to AExisting.Count - 1 do
  begin

    if not CanRefreshProjection(AExisting.Children[LIndex], ACandidate.Children[LIndex],
      ABindings, AForms) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function CanRefreshNyxProjection(AExisting, ACandidate: TNyxNode): Boolean;
begin
  Result := CanRefreshProjection(AExisting, ACandidate, pbaUnbound);
end;

function CanRefreshNyxBoundProjection(AExisting, ACandidate: TNyxNode): Boolean;
begin
  Result := CanRefreshProjection(AExisting, ACandidate, pbaExact);
end;

function CanRefreshNyxFormProjection(AExisting, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts): Boolean;
begin
  Result := ValidFormContexts(AExisting, ACandidate, AForms) and
    CanRefreshProjection(AExisting, ACandidate, pbaUnbound, AForms);
end;

function CanRefreshNyxBoundFormProjection(AExisting, ACandidate: TNyxNode;
  const AForms: TNyxProjectionFormContexts): Boolean;
begin
  Result := ValidFormContexts(AExisting, ACandidate, AForms) and
    CanRefreshProjection(AExisting, ACandidate, pbaExact, AForms);
end;

function RefreshProjectionProperties(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode; const ARestores: TNyxProjectionValueRestores;
  ABindings: TProjectionBindingAdmission;
  const AForms: TNyxProjectionFormContexts = nil): Boolean;
var
  LRestore: Integer;
  LExisting: TNyxNode;
  LAccepted: TNyxNode;
  LProperty: Integer;
  LContextRestores: TNyxProjectionValueRestores;
  LFields: TNyxProjectionValueRestores;
  LBefore: TNyxNode;

  procedure CopyProperties(AFrom, ATo, ABefore: TNyxNode);
  var
    LIndex: Integer;
    LKey: TNyxText;
    LProperty: Integer;
  begin

    if ABefore = nil then
    begin
      ATo.Props.Assign(AFrom.Props);
    end
    else
    begin
      { A mounted runtime may own edited text/values independent of its authored
        defaults. Apply only actual authored deltas; an unrelated caption must
        never reset that input to an unchanged document default. }
      for LIndex := 0 to AFrom.Props.Count - 1 do
      begin
        LKey := AFrom.Props.Names[LIndex];

        if (ABefore.Props.IndexOfName(LKey) < 0) or
          (ABefore.StoredProp(LKey) <> AFrom.StoredProp(LKey)) then
        begin
          ATo.SetProp(LKey, AFrom.StoredProp(LKey));
        end;
      end;
      for LIndex := 0 to ABefore.Props.Count - 1 do
      begin
        LKey := ABefore.Props.Names[LIndex];

        if AFrom.Props.IndexOfName(LKey) < 0 then
        begin
          LProperty := ATo.Props.IndexOfName(LKey);

          if LProperty >= 0 then
          begin
            ATo.Props.Delete(LProperty);
          end;
        end;
      end;
    end;
    for LIndex := 0 to AFrom.Count - 1 do
    begin

      if ABefore = nil then
      begin
        CopyProperties(AFrom.Children[LIndex], ATo.Children[LIndex], nil);
      end
      else
      begin
        CopyProperties(AFrom.Children[LIndex], ATo.Children[LIndex], ABefore.Children[LIndex]);
      end;
    end;
  end;

begin
  Result := ValidFormContexts(AExisting, ACandidate, AForms) and
    CanRefreshProjection(AExisting, ACandidate, ABindings, AForms);

  if Result and (ABaseline <> nil) then
  begin
    Result := ValidFormContexts(ABaseline, ACandidate, AForms) and
      CanRefreshProjection(ABaseline, ACandidate, ABindings, AForms);
  end;

  if Result then
  begin
    LBefore := ABaseline;

    if LBefore = nil then
    begin
      LBefore := AExisting;
    end;

    if not PrepareNyxProjectionFormRestores(LBefore, ACandidate, AForms, LContextRestores) then
    begin
      Exit(False);
    end;
    { Assemble an independent vector. Caller-owned restore arrays may be shared;
      appending context resets must never mutate the caller's published value. }
    SetLength(LFields, Length(ARestores) + Length(LContextRestores));
    for LRestore := 0 to High(ARestores) do
    begin
      LFields[LRestore] := ARestores[LRestore];
    end;
    for LRestore := 0 to High(LContextRestores) do
    begin
      LFields[Length(ARestores) + LRestore] := LContextRestores[LRestore];
    end;
    { Validate the complete group before copying any authored delta. A stale
      reusable owner cannot reset a different field with a matching local ID. }
    for LRestore := 0 to High(LFields) do
    begin
      LExisting := AExisting.Find(LFields[LRestore].RuntimeID);
      LAccepted := ACandidate.Find(LFields[LRestore].RuntimeID);

      if (LExisting = nil) or (LAccepted = nil) or
        (LFields[LRestore].RuntimeID = '') or (LFields[LRestore].DesignID = '') or
        (LExisting.DesignID <> LFields[LRestore].DesignID) or
        (LAccepted.DesignID <> LFields[LRestore].DesignID) then
      begin
        Exit(False);
      end;
    end;
    AExisting.BindPresentations(ACandidate.PresentationSnapshot);
    CopyProperties(ACandidate, AExisting, ABaseline);
    for LRestore := 0 to High(LFields) do
    begin
      LExisting := AExisting.Find(LFields[LRestore].RuntimeID);
      LAccepted := ACandidate.Find(LFields[LRestore].RuntimeID);

      if LAccepted.Props.IndexOfName(NyxAttributeName(atValue)) >= 0 then
      begin
        { Copy the already admitted property representation, including numeric
          and Boolean fields; typed authoring overloads do not accept wire text. }
        LExisting.SetProp(NyxAttributeName(atValue), LAccepted.StoredProp(NyxAttributeName(atValue)));
      end
      else
      begin
        LProperty := LExisting.Props.IndexOfName(NyxAttributeName(atValue));

        if LProperty >= 0 then
        begin
          LExisting.Props.Delete(LProperty);
        end;
      end;
    end;
  end;
end;

function RefreshNyxProjectionProperties(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode; const ARestores: TNyxProjectionValueRestores): Boolean;
begin
  Result := RefreshProjectionProperties(AExisting, ACandidate, ABaseline, ARestores, pbaUnbound);
end;

function RefreshNyxBoundProjectionProperties(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode; const ARestores: TNyxProjectionValueRestores): Boolean;
begin
  Result := RefreshProjectionProperties(AExisting, ACandidate, ABaseline, ARestores, pbaExact);
end;

function RefreshNyxFormProjectionProperties(AExisting, ACandidate, ABaseline: TNyxNode;
  const AForms: TNyxProjectionFormContexts;
  const ARestores: TNyxProjectionValueRestores): Boolean;
begin
  Result := RefreshProjectionProperties(AExisting, ACandidate, ABaseline, ARestores,
    pbaUnbound, AForms);
end;

function RefreshNyxBoundFormProjectionProperties(AExisting, ACandidate, ABaseline: TNyxNode;
  const AForms: TNyxProjectionFormContexts;
  const ARestores: TNyxProjectionValueRestores): Boolean;
begin
  Result := RefreshProjectionProperties(AExisting, ACandidate, ABaseline, ARestores,
    pbaExact, AForms);
end;

function SameNyxProjectionBindingContracts(AExisting, ACandidate: TNyxNode): Boolean;
var
  LAligned: TNyxNode;

  function SameTree(AFrom, ATo: TNyxNode): Boolean;
  var
    LChild: Integer;
  begin
    Result := False;

    if not SameNodeBindings(AFrom, ATo) then
    begin
      Exit;
    end;
    for LChild := 0 to AFrom.Count - 1 do
    begin

      if not SameTree(AFrom.Children[LChild], ATo.Children[LChild]) then
      begin
        Exit;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;

  if (AExisting = nil) or (ACandidate = nil) then
  begin
    Exit;
  end;
  LAligned := ACandidate.Clone;
  try
    Result := LAligned.ArrangeLike(AExisting) and SameTree(AExisting, LAligned);
  finally
    LAligned.Free;
  end;
end;

function NyxProjectionChildrenChanged(ANode, AFormerRoot: TNyxNode): Boolean;
var
  LFormer: TNyxNode;
  LChild: Integer;
begin
  LFormer := AFormerRoot.Find(ANode.ID);

  if LFormer = nil then
  begin
    raise ENyxModel.Create('Admitted projection host lost its former identity');
  end;
  Result := ANode.Count <> LFormer.Count;
  for LChild := 0 to ANode.Count - 1 do
  begin

    if Result then
    begin
      Exit;
    end;
    Result := ANode.Children[LChild].ID <> LFormer.Children[LChild].ID;
  end;
end;

function CanArrangeProjection(AExisting, ACandidate: TNyxNode;
  ABindings: TProjectionBindingAdmission): Boolean;
var
  LAligned: TNyxNode;
  LNodes: array of TNyxNode;
  LIndex: TNyxTextIndex;
  LCount: Integer;

  function PlainHost(ANode: TNyxNode): Boolean;
  var
    LKind: TNyxText;
  begin
    LKind := ANode.ProjectionKind;
    Result := (LKind = 'page') or (LKind = 'row') or (LKind = 'column') or
      (LKind = 'grid') or (LKind = 'panel') or (LKind = 'card') or
      (LKind = 'group-box') or (LKind = 'form') or (LKind = 'toolbar') or
      (LKind = 'sidebar');
  end;

  procedure Collect(ANode: TNyxNode);
  var
    LChild: Integer;
  begin
    Inc(LCount);

    if Length(LNodes) < LCount then
    begin
      SetLength(LNodes, LCount * 2);
    end;
    LNodes[LCount - 1] := ANode;
    LIndex.AddFirst(ANode.ID, LCount - 1);
    for LChild := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LChild]);
    end;
  end;

  function AdmittedParents(ANode: TNyxNode): Boolean;
  var
    LExisting: TNyxNode;
    LChild: Integer;
    LChangedChildren: Boolean;
  begin
    Result := False;
    LExisting := LNodes[LIndex.IndexOf(ANode.ID)];

    if (ANode.Parent <> nil) and
      (ANode.Parent.ID <> LExisting.Parent.ID) then
    begin

      if not PlainHost(ANode.Parent) or not PlainHost(LExisting.Parent) then
      begin
        Exit;
      end;
    end;
    LChangedChildren := ANode.Count <> LExisting.Count;
    for LChild := 0 to ANode.Count - 1 do
    begin

      if not LChangedChildren then
      begin
        LChangedChildren := ANode.Children[LChild].ID <> LExisting.Children[LChild].ID;
      end;

      if not AdmittedParents(ANode.Children[LChild]) then
      begin
        Exit;
      end;
    end;
    Result := not LChangedChildren or PlainHost(ANode);
  end;

begin
  Result := False;

  if (AExisting = nil) or (ACandidate = nil) then
  begin
    Exit;
  end;
  LAligned := ACandidate.Clone;
  try

    if not LAligned.ArrangeLike(AExisting) or
      not CanRefreshProjection(AExisting, LAligned, ABindings) then
    begin
      Exit;
    end;
    LIndex := TNyxTextIndex.Create;
    try
      LCount := 0;
      Collect(AExisting);
      Result := AdmittedParents(ACandidate);
    finally
      LIndex.Free;
    end;
  finally
    LAligned.Free;
  end;
end;

function CanArrangeNyxProjection(AExisting, ACandidate: TNyxNode): Boolean;
begin
  Result := CanArrangeProjection(AExisting, ACandidate, pbaUnbound);
end;

function CanArrangeNyxBoundProjection(AExisting, ACandidate: TNyxNode): Boolean;
begin
  Result := CanArrangeProjection(AExisting, ACandidate, pbaExact);
end;

function RefreshProjectionArrangement(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode; const ARestores: TNyxProjectionValueRestores;
  ABindings: TProjectionBindingAdmission): Boolean;
var
  LAligned: TNyxNode;
  LBaseline: TNyxNode;
begin
  Result := False;

  if not CanArrangeProjection(AExisting, ACandidate, ABindings) or
    ((ABaseline <> nil) and not CanArrangeProjection(ABaseline, ACandidate, ABindings)) then
  begin
    Exit;
  end;
  LAligned := nil;
  LBaseline := nil;
  try
    LAligned := ACandidate.Clone;

    if not LAligned.ArrangeLike(AExisting) then
    begin
      Exit;
    end;

    if ABaseline <> nil then
    begin
      LBaseline := ABaseline.Clone;

      if not LBaseline.ArrangeLike(AExisting) then
      begin
        Exit;
      end;
    end;

    if not RefreshProjectionProperties(AExisting, LAligned, LBaseline, ARestores, ABindings) then
    begin
      Exit;
    end;

    if not AExisting.ArrangeLike(ACandidate) then
    begin
      raise ENyxModel.Create('Admitted projection arrangement changed during publication');
    end;
    Result := True;
  finally
    LBaseline.Free;
    LAligned.Free;
  end;
end;

function RefreshNyxProjectionArrangement(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode; const ARestores: TNyxProjectionValueRestores): Boolean;
begin
  Result := RefreshProjectionArrangement(AExisting, ACandidate, ABaseline, ARestores, pbaUnbound);
end;

function RefreshNyxBoundProjectionArrangement(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode; const ARestores: TNyxProjectionValueRestores): Boolean;
begin
  Result := RefreshProjectionArrangement(AExisting, ACandidate, ABaseline, ARestores, pbaExact);
end;

function NyxProjectionContext(ADocument: TNyxDocument): TNyxText;
begin
  Result := TNyxCodec.ProjectionContext(ADocument);
end;

end.
