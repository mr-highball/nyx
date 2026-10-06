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
  nyx.text, nyx.types, nyx.responsive, nyx.model;

type
  { A typed request to restore one exact field's current authored default during
    retained projection. It owns only identity values, never a node or widget.
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
  TNyxProjectionValueRestores = array of TNyxProjectionValueRestore;

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

{ Exact immutable document context for a mounted view. Fresh encoding validates
  current public data/budgets; it is not an admission cache. All document fields
  except title/pages/components remain significant, including defaults, tokens,
  collection registries and creator extension data. Requested-root realization
  separately checks structure and every node. Unrelated authored roots may
  change only when the requested realized view retains identical semantics. }
function NyxProjectionContext(ADocument: TNyxDocument): TNyxText;

implementation

uses
  nyx.codec, nyx.data, nyx.presentations, nyx.text.index;

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

function CanRefreshNyxProjection(AExisting, ACandidate: TNyxNode): Boolean;
var
  LIndex: Integer;
  LKey: TNyxText;

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
        (AFrom.StoredProp(LKey) <> ATo.StoredProp(LKey))) and not RefreshableKey(LKey) then
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
    (AExisting.ProjectionKind <> ACandidate.ProjectionKind) or
    (AExisting.IsRealized <> ACandidate.IsRealized) or
    (AExisting.Count <> ACandidate.Count) or
    (AExisting.BindingCount <> 0) or (ACandidate.BindingCount <> 0) or
    (AExisting.HasCollectionView <> ACandidate.HasCollectionView) or
    (AExisting.Extensions.ToJSON <> ACandidate.Extensions.ToJSON) then
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

    if not CanRefreshNyxProjection(AExisting.Children[LIndex], ACandidate.Children[LIndex]) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function RefreshNyxProjectionProperties(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode; const ARestores: TNyxProjectionValueRestores): Boolean;
var
  LRestore: Integer;
  LExisting: TNyxNode;
  LAccepted: TNyxNode;
  LProperty: Integer;

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
  Result := CanRefreshNyxProjection(AExisting, ACandidate);

  if Result and (ABaseline <> nil) then
  begin
    Result := CanRefreshNyxProjection(ABaseline, ACandidate);
  end;

  if Result then
  begin
    { Validate the complete group before copying any authored delta. A stale
      reusable owner cannot reset a different field with a matching local ID. }
    for LRestore := 0 to High(ARestores) do
    begin
      LExisting := AExisting.Find(ARestores[LRestore].RuntimeID);
      LAccepted := ACandidate.Find(ARestores[LRestore].RuntimeID);

      if (LExisting = nil) or (LAccepted = nil) or
        (ARestores[LRestore].RuntimeID = '') or (ARestores[LRestore].DesignID = '') or
        (LExisting.DesignID <> ARestores[LRestore].DesignID) or
        (LAccepted.DesignID <> ARestores[LRestore].DesignID) then
      begin
        Exit(False);
      end;
    end;
    AExisting.BindPresentations(ACandidate.PresentationSnapshot);
    CopyProperties(ACandidate, AExisting, ABaseline);
    for LRestore := 0 to High(ARestores) do
    begin
      LExisting := AExisting.Find(ARestores[LRestore].RuntimeID);
      LAccepted := ACandidate.Find(ARestores[LRestore].RuntimeID);

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

function CanArrangeNyxProjection(AExisting, ACandidate: TNyxNode): Boolean;
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
      not CanRefreshNyxProjection(AExisting, LAligned) then
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

function RefreshNyxProjectionArrangement(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode; const ARestores: TNyxProjectionValueRestores): Boolean;
var
  LAligned: TNyxNode;
  LBaseline: TNyxNode;
begin
  Result := False;

  if not CanArrangeNyxProjection(AExisting, ACandidate) or
    ((ABaseline <> nil) and not CanArrangeNyxProjection(ABaseline, ACandidate)) then
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

    if not RefreshNyxProjectionProperties(AExisting, LAligned, LBaseline, ARestores) then
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

function NyxProjectionContext(ADocument: TNyxDocument): TNyxText;
var
  LData: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LKey: TNyxText;
begin
  LData := TNyxDataValue.ParseJSON(TNyxCodec.Encode(ADocument));
  SetLength(LFields, LData.Count);
  LCount := 0;
  for LIndex := 0 to LData.Count - 1 do
  begin
    LKey := LData.Key(LIndex);

    if (LKey = 'title') or (LKey = 'pages') or (LKey = 'components') or
      (LKey = 'version') or
      ((LKey = NyxPresentationsWireField) and (ADocument.Presentations.Count > 0)) or
      ((LKey = 'collections') and (ADocument.Collections.Count = 0) and
      not ADocument.Extensions.Has(NyxExtension('collections'))) then
    begin
      Continue;
    end;
    { Wire version is framing, not runtime meaning. Named definitions are
      checked per referenced property by retained projection admission; unused
      definitions cannot disturb live controls. Older opaque data stays exact. }
    LFields[LCount] := NyxField(LKey, LData.Field(LKey));
    Inc(LCount);
  end;
  SetLength(LFields, LCount);
  Result := NyxObject(LFields).ToJSON;
end;

end.
