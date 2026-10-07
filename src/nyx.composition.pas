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

unit nyx.composition;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  Classes,
  SysUtils,
  nyx.types,
  nyx.responsive,
  nyx.presentations,
  nyx.containers,
  nyx.content,
  nyx.model;

type
  { Copied initial composition frame in logical pixels. At validates finite,
    nonnegative geometry and a concrete target. Selecting keeps geometry while
    choosing an exact manual presentation. No document or target is retained.
    Default realization deliberately uses only the ordinary recipe, so build
    dependency discovery never accidentally chooses a zero-width alternative. }
  TNyxViewFrame = record
  private
    FSpecified: Boolean;
    FWidth: Double;
    FHeight: Double;
    FPlatform: TNyxPlatform;
    FSelection: TNyxPresentationSelection;
  public
    class function At(AWidth, AHeight: Double; APlatform: TNyxPlatform): TNyxViewFrame; static;
    function Selecting(const AReference: TNyxPresentationRef): TNyxViewFrame;
    property Width: Double read FWidth;
    property Height: Double read FHeight;
    property Platform: TNyxPlatform read FPlatform;
    property Selection: TNyxPresentationSelection read FSelection;
  end;

{ Realize a design root as an independent view tree. Reusable instances expand
  definitions, qualify runtime IDs and retain design-id for Studio selection.
  Each instance has independent runtime values; editing one never mutates the
  shared component definition or another instance. Caller owns the result. }
function RealizeNyxView(ADocument: TNyxDocument; ARoot: TNyxNode): TNyxNode; overload;
{ Lazily expand only each selected recipe. Every branch is validated as authored
  data first. Container conditions consult the nearest eligible realized ancestor
  in the supplied immutable measurement frame; absent boxes stay inactive.
  The returned tree owns no source-document or measurement backreferences. }
function RealizeNyxView(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AFrame: TNyxViewFrame;
  const AMeasurements: INyxContainerSnapshot = nil): TNyxNode; overload;

{ Realize the containing page/definition so a selected part retains its owning
  contracts, sibling value sources and reusable overrides. Caller owns Result;
  AProjection is borrowed only until that whole result is released. A removed
  override has no projection and returns nil. Neither source tree is mutated. }
function RealizeNyxContext(ADocument: TNyxDocument; ANode: TNyxNode;
  out AProjection: TNyxNode): TNyxNode;

{ Clone a selected authored subtree into a standalone view document, retaining
  exact source IDs and only the transitive reusable definitions it references.
  ARoot is borrowed and must belong to ADocument. Caller owns the returned
  document. Invalid input frees the candidate and leaves the source untouched. }
function CloneNyxViewDocument(ADocument: TNyxDocument; ARoot: TNyxNode): TNyxDocument;

{ Copy an authored subtree as an independent reusable definition. ARoot is
  borrowed from ADocument; the caller owns Result until document admission.
  ADefinition supplies the new root identity. AIdentities must assign EVERY
  descendant exactly once, with unique unoccupied destination IDs. Root entries,
  foreign/missing entries and realized trees refuse. Parts, contracts, bindings,
  callback descriptors and opaque extension data are copied without rewriting
  their application names or retained Pascal helpers. Existing reusable refs
  continue to reference their original definitions. No document is mutated. }
function CloneNyxReusableDefinition(ADocument: TNyxDocument; ARoot: TNyxNode;
  const ADefinition: TNyxComponentRef;
  const AIdentities: array of TNyxIdentityAssignment): TNyxNode;

implementation

uses
  nyx.behavior, nyx.text.index, nyx.menu.declarations, nyx.menu.types, nyx.root.types;

class function TNyxViewFrame.At(AWidth, AHeight: Double;
  APlatform: TNyxPlatform): TNyxViewFrame;
begin
  TNyxViewportCondition.Any.Matches(AWidth, AHeight);
  NyxPlatformName(APlatform);

  if APlatform = npfAny then
  begin
    raise ENyxContent.Create('A composition frame requires a concrete target');
  end;
  Result := Default(TNyxViewFrame);
  Result.FSpecified := True;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
  Result.FPlatform := APlatform;
end;

function TNyxViewFrame.Selecting(const AReference: TNyxPresentationRef): TNyxViewFrame;
begin

  if not FSpecified then
  begin
    raise ENyxContent.Create('Manual recipe selection requires a composition frame');
  end;
  Result := Self;
  Result.FSelection := TNyxPresentationSelection.Use(AReference);
end;

function CloneNyxReusableDefinition(ADocument: TNyxDocument; ARoot: TNyxNode;
  const ADefinition: TNyxComponentRef;
  const AIdentities: array of TNyxIdentityAssignment): TNyxNode;
var
  LUsed: array of Boolean;
  LIndex: Integer;
  LSources, LDestinations, LOccupied: TNyxTextIndex;

  procedure IndexTree(ANode: TNyxNode);
  var
    LChild: Integer;
  begin
    LOccupied.AddFirst(ANode.ID, 0);
    for LChild := 0 to ANode.Count - 1 do
    begin
      IndexTree(ANode.Children[LChild]);
    end;
  end;

  procedure AssignDescendants(ANode: TNyxNode);
  var
    LChild, LMap: Integer;
  begin
    for LChild := 0 to ANode.Count - 1 do
    begin
      LMap := LSources.IndexOf(ANode.Children[LChild].ID);

      if LMap < 0 then
      begin
        raise ENyxModel.Create('Derivation requires an identity for descendant: ' +
          ANode.Children[LChild].ID);
      end;
      LUsed[LMap] := True;
      AssignDescendants(ANode.Children[LChild]);
      ANode.Children[LChild].Named(AIdentities[LMap].Destination.ID);
    end;
  end;
begin
  Result := nil;

  if (ADocument = nil) or (ARoot = nil) or ARoot.IsRealized or
    not ADocument.Contains(ARoot) then
  begin
    raise ENyxModel.Create('Derivation requires an authored subtree in its document');
  end;
  { Admit budgets before recursive Clone. The accepted tree remains borrowed;
    all later naming and allocation happen only in the independent copy. }
  ADocument.Validate;

  if Length(AIdentities) >= NyxMaximumNodes then
  begin
    raise ENyxModel.Create('Derived descendant assignments exceed the authored tree budget');
  end;

  if (ADefinition.Name = '') or (ADocument.Find(ADefinition.Name) <> nil) then
  begin
    raise ENyxModel.Create('Derived definition requires a new exact identity');
  end;
  SetLength(LUsed, Length(AIdentities));
  LSources := TNyxTextIndex.Create;
  LDestinations := TNyxTextIndex.Create;
  LOccupied := TNyxTextIndex.Create;
  try
    { These indexes own copied keys only. They avoid quadratic mapping and
      repeated document scans without caching mutable authoring state. }
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      IndexTree(ADocument.Pages[LIndex]);
    end;
    for LIndex := 0 to ADocument.ComponentCount - 1 do
    begin
      IndexTree(ADocument.Components[LIndex]);
    end;
    for LIndex := 0 to High(AIdentities) do
    begin

      if (AIdentities[LIndex].Source.ID = ARoot.ID) or
        (AIdentities[LIndex].Destination.ID = ADefinition.Name) or
        (AIdentities[LIndex].Destination.ID = '') or
        (LOccupied.IndexOf(AIdentities[LIndex].Destination.ID) >= 0) then
      begin
        raise ENyxModel.Create('Derived descendant identity is occupied or addresses the root');
      end;

      if (LSources.IndexOf(AIdentities[LIndex].Source.ID) >= 0) or
        (LDestinations.IndexOf(AIdentities[LIndex].Destination.ID) >= 0) then
      begin
        raise ENyxModel.Create('Derived identity assignments must be one-to-one');
      end;
      LSources.AddFirst(AIdentities[LIndex].Source.ID, LIndex);
      LDestinations.AddFirst(AIdentities[LIndex].Destination.ID, LIndex);
    end;
    Result := ARoot.Clone;
    try
      AssignDescendants(Result);
      for LIndex := 0 to High(LUsed) do
      begin

        if not LUsed[LIndex] then
        begin
          raise ENyxModel.Create('Derived identity assignment addresses a foreign descendant');
        end;
      end;
      Result.Named(ADefinition.Name);
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LOccupied.Free;
    LDestinations.Free;
    LSources.Free;
  end;
end;

function RealizeNyxContext(ADocument: TNyxDocument; ANode: TNyxNode;
  out AProjection: TNyxNode): TNyxNode;
var
  LOwner: TNyxNode;
  LReference: TNyxNode;

  function AuthoredProjection(ARoot: TNyxNode; const AID: TNyxText): TNyxNode;
  var
    LIndex: Integer;
    LFound: TNyxNode;
  begin
    Result := nil;

    { Preorder finds the instance root before its inherited parts, which share
      the selectable instance DesignID but retain their definition SourceID. }

    if (ARoot.ID = AID) or (ARoot.DesignID = AID) then
    begin
      Exit(ARoot);
    end;
    for LIndex := 0 to ARoot.Count - 1 do
    begin
      LFound := AuthoredProjection(ARoot.Children[LIndex], AID);

      if LFound <> nil then
      begin
        Exit(LFound);
      end;
    end;
  end;

begin
  Result := nil;
  AProjection := nil;

  if (ADocument = nil) or (ANode = nil) or ANode.IsRealized or
    not ADocument.Contains(ANode) then
  begin
    raise ENyxModel.Create('Context requires an authored node in its document');
  end;

  if (ANode.Kind = 'slot-override') and (ANode.Prop('mode') = 'remove') then
  begin
    Exit;
  end;
  LOwner := ANode;
  while LOwner.Parent <> nil do
  begin
    LOwner := LOwner.Parent;
  end;
  Result := RealizeNyxView(ADocument, LOwner);
  try

    if ANode.Kind = 'slot-override' then
    begin
      LReference := AuthoredProjection(Result, ANode.Parent.ID);

      if LReference <> nil then
      begin
        AProjection := LReference.Part(ANode.Prop('path'));
      end;
    end
    else
    begin
      AProjection := AuthoredProjection(Result, ANode.ID);
    end;

    if AProjection = nil then
    begin
      raise ENyxModel.Create('Authored component has no projection in its context: ' + ANode.ID);
    end;
  except
    AProjection := nil;
    Result.Free;
    Result := nil;
    raise;
  end;
end;

function RealizeNyxView(ADocument: TNyxDocument; ARoot: TNyxNode): TNyxNode;
begin
  Result := RealizeNyxView(ADocument, ARoot, Default(TNyxViewFrame));
end;

function RealizeNyxView(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AFrame: TNyxViewFrame; const AMeasurements: INyxContainerSnapshot): TNyxNode;
var
  LCount: Integer;
  LAncestors: array of TNyxNode;
  LPresentations: INyxPresentationSnapshot;

  function NamedMatches(const ARule: TNyxContentRule): Boolean;
  var
    LCondition: TNyxPresentationCondition;
    LIndex: Integer;
    LContainer: TNyxContainerRef;
    LWidth: Double;
    LHeight: Double;
  begin
    LCondition := LPresentations.Definition(ARule.Presentation);

    if LCondition.Activation = npaManual then
    begin
      Exit(AFrame.Selection.Matches(ARule.Presentation));
    end;

    if not LCondition.Container.Defined then
    begin
      Exit(LCondition.Viewport.Matches(AFrame.Width, AFrame.Height));
    end;
    Result := False;

    if AMeasurements = nil then
    begin
      Exit;
    end;
    for LIndex := High(LAncestors) downto 0 do
    begin
      LContainer := LAncestors[LIndex].QueryContainer;

      if LContainer.Defined and (LContainer.Name = LCondition.Container.Name) and
        NyxContainerEligible(LAncestors[LIndex].ContainerContainment, LCondition.Viewport) then
      begin
        Result := AMeasurements.TrySize(LAncestors[LIndex].ID, LWidth, LHeight);

        if Result then
        begin
          Result := LCondition.Viewport.Matches(LWidth, LHeight);
        end;
        Exit;
      end;
    end;
  end;

  function SelectedComponent(ANode: TNyxNode): TNyxComponentRef;
  var
    LPhase: Integer;
    LIndex: Integer;
    LRulePhase: Integer;
    LRule: TNyxContentRule;
    LMatches: Boolean;
  begin
    Result := ANode.DefaultComponent;

    if not ANode.HasContent then
    begin
      Exit;
    end;
    { Ordinary defaults, automatic conditions, then manual choice; repeat with
      target-specific rules last. Last matching rule in each phase wins. This
      matches scalar presentation precedence without mutating the source tree. }
    for LPhase := 0 to 5 do
    begin
      for LIndex := 0 to ANode.Content.Count - 1 do
      begin
        LRule := ANode.Content.Rule(LIndex);
        LRulePhase := 0;
        LMatches := True;

        if LRule.Scope <> ncsDefault then
        begin
          LRulePhase := 1;
          LMatches := AFrame.FSpecified;

          if LMatches and (LRule.Scope = ncsViewport) then
          begin
            LMatches := LRule.Viewport.Matches(AFrame.Width, AFrame.Height);
          end
          else if LMatches then
          begin
            LMatches := NamedMatches(LRule);

            if LPresentations.Definition(LRule.Presentation).Activation = npaManual then
            begin
              LRulePhase := 2;
            end;
          end;
        end;

        if LRule.Platform <> npfAny then
        begin
          Inc(LRulePhase, 3);
          LMatches := LMatches and AFrame.FSpecified and (LRule.Platform = AFrame.Platform);
        end;

        if LMatches and (LRulePhase = LPhase) then
        begin
          Result := LRule.Component;
        end;
      end;
    end;
  end;

  function Expand(ANode: TNyxNode; const APrefix, ADesignID: TNyxText;
    ADepth: Integer; const ARootKind: TNyxText = '';
    const AInstanceScope: TNyxText = ''; const AQueryContainer: TNyxText = #0;
    const AContainment: TNyxText = #0): TNyxNode; forward;

  function ExpandInParent(ANode, AParent: TNyxNode; const APrefix: TNyxText;
    ADepth: Integer; const AInstanceScope: TNyxText): TNyxNode;
  var
    LPath: array of TNyxNode;
    LAncestor: TNyxNode;
    LIndex: Integer;
    LOriginalCount: Integer;
  begin
    { Override payloads expand after their inherited tree has been attached
      internally. Add its exact root-to-slot ancestry to the existing outer
      stack; all links are borrowed only during this recursive call. }
    LOriginalCount := Length(LAncestors);
    LPath := nil;
    LAncestor := AParent;
    while LAncestor <> nil do
    begin
      SetLength(LPath, Length(LPath) + 1);
      LPath[High(LPath)] := LAncestor;
      LAncestor := LAncestor.Parent;
    end;
    try
      for LIndex := High(LPath) downto 0 do
      begin
        SetLength(LAncestors, Length(LAncestors) + 1);
        LAncestors[High(LAncestors)] := LPath[LIndex];
      end;
      Result := Expand(ANode, APrefix, '', ADepth, '', AInstanceScope);
    finally
      SetLength(LAncestors, LOriginalCount);
    end;
  end;

  procedure ApplyOverrides(AReference, ARuntime: TNyxNode;
    const APrefix: TNyxText; ADepth: Integer);
  var
    LIndex: Integer;
    LPropertyIndex: Integer;
    LChildIndex: Integer;
    LPosition: Integer;
    LRule: TNyxNode;
    LPart: TNyxNode;
    LParent: TNyxNode;
    LPayload: TNyxNode;
    LMode: TNyxText;
    LKey: TNyxText;
  begin
    { Apply descriptors in their stored order to this independent candidate.
      Paths can address nested reusable parts after expansion. Payload identities
      refer back to their own editable design nodes, while inherited definition
      parts retain the selecting instance's design identity. }
    for LIndex := 0 to AReference.Count - 1 do
    begin
      LRule := AReference.Children[LIndex];
      try
        LPart := ARuntime.Part(LRule.Prop('path'));
      except
        on LException: ENyxModel do
        begin
          raise ENyxModel.Create('Invalid part override on ' + AReference.ID +
            ' / ' + LRule.Prop('path') + ': ' + TNyxText(LException.Message));
        end;
      end;
      LMode := LRule.Prop('mode');

      if (LMode = 'replace') or (LMode = 'remove') then
      begin
        LParent := LPart.Parent;
        LPosition := 0;
        while LParent.Children[LPosition] <> LPart do
        begin
          Inc(LPosition);
        end;

        if LMode = 'remove' then
        begin
          LParent.Remove(LPart);
          Continue;
        end;
        LPayload := ExpandInParent(LRule.Children[0], LParent, APrefix,
          ADepth + 1, ARuntime.InstanceScopeID);
        try
          LPayload.SetProp('part', LPart.Prop('part'));
          LParent.Insert(LPosition, LPayload);
        except
          LPayload.Free;
          raise;
        end;
        LParent.Remove(LPart);
        LPart := LPayload;
      end
      else if (LMode = 'append') or (LMode = 'prepend') then
      begin
        for LChildIndex := 0 to LRule.Count - 1 do
        begin
          LPayload := ExpandInParent(LRule.Children[LChildIndex], LPart, APrefix,
            ADepth + 1, ARuntime.InstanceScopeID);
          try

            if LMode = 'prepend' then
            begin
              LPart.Insert(LChildIndex, LPayload);
            end
            else
            begin
              LPart.Add(LPayload);
            end;
          except
            LPayload.Free;
            raise;
          end;
        end;
      end;
      for LPropertyIndex := 0 to LRule.Props.Count - 1 do
      begin
        LKey := LRule.Props.Names[LPropertyIndex];

        if (LKey <> 'path') and (LKey <> 'mode') then
        begin
          LPart.SetProp(LKey, LRule.Prop(LKey));
        end;
      end;
      for LPropertyIndex := 0 to LRule.BindingCount - 1 do
      begin
        LPart.SetBinding(LRule.Bindings[LPropertyIndex]);
      end;

      if LRule.HasCollectionView then
      begin
        LPart.SetCollectionView(LRule.CollectionView);
      end;

      if LRule.HasMenu then
      begin
        LPart.SetMenu(LRule.MenuReference);
      end;

      if LRule.HasMenuBar then
      begin
        LPart.SetMenuBar(LRule.MenuBar);
      end;
      LPart.Extensions.Overlay(LRule.Extensions);
    end;
  end;

  function Expand(ANode: TNyxNode; const APrefix, ADesignID: TNyxText;
    ADepth: Integer; const ARootKind, AInstanceScope, AQueryContainer,
    AContainment: TNyxText): TNyxNode;
  var
    LIndex: Integer;
    LDefinition: TNyxNode;
    LDesignID: TNyxText;
    LKey: TNyxText;
    LRootKind: TNyxText;
    LScope: TNyxText;
    LQueryContainer: TNyxText;
    LContainment: TNyxText;
  begin
    Inc(LCount);

    if (LCount > NyxMaximumNodes) or (ADepth > NyxMaximumTreeDepth) then
    begin
      raise ENyxModel.Create('Realized view exceeds expansion budget');
    end;
    LDesignID := ADesignID;

    if LDesignID = '' then
    begin
      LDesignID := ANode.ID;
    end;

    if (ANode.Kind = 'component') or (ANode.ProjectionKind = 'component') then
    begin
      LDefinition := ADocument.FindComponent(SelectedComponent(ANode).Name);

      if LDefinition = nil then
      begin
        raise ENyxModel.Create('Reusable definition not found');
      end;
      LRootKind := ARootKind;

      if (LRootKind = '') and (ANode.Kind <> 'component') then
      begin
        LRootKind := ANode.Kind;
      end;
      LScope := NyxQualifiedID(APrefix, ANode.ID);
      LQueryContainer := AQueryContainer;
      LContainment := AContainment;

      if LQueryContainer = #0 then
      begin
        LQueryContainer := ANode.StoredProp('query-container', #0);
      end;

      if LContainment = #0 then
      begin
        LContainment := ANode.StoredProp('container-containment', #0);
      end;
      Result := Expand(LDefinition, LScope, LDesignID,
        ADepth + 1, LRootKind, NyxQualifiedID(LScope, LDefinition.ID),
        LQueryContainer, LContainment);
      try

        if ANode.HasContent then
        begin
          Result.BindRecipeOwner(NyxControl(LScope));
        end;
        { Instance overrides follow the definition recipe. Identity/reference
          metadata is excluded so it cannot corrupt selection or recurse again. }
        for LIndex := 0 to ANode.Props.Count - 1 do
        begin
          LKey := ANode.Props.Names[LIndex];

          if (LKey <> 'component') and (LKey <> 'design-id') and (LKey <> 'projection-kind') then
          begin
            Result.SetProp(LKey, ANode.Prop(LKey));
          end;
        end;
        for LIndex := 0 to ANode.BindingCount - 1 do
        begin
          Result.SetBinding(ANode.Bindings[LIndex]);
        end;

        if ANode.HasCollectionView then
        begin
          Result.SetCollectionView(ANode.CollectionView);
        end;

        if ANode.HasMenu then
        begin
          Result.SetMenu(ANode.MenuReference);
        end;

        if ANode.HasMenuBar then
        begin
          Result.SetMenuBar(ANode.MenuBar);
        end;
        Result.Extensions.Overlay(ANode.Extensions);
        ApplyOverrides(ANode, Result, LScope, ADepth + 1);
      except
        Result.Free;
        raise;
      end;
      Exit;
    end;
    LRootKind := ANode.Kind;

    if ARootKind <> '' then
    begin
      LRootKind := ARootKind;
    end;
    Result := TNyxNode.CreateRealized(LRootKind, ANode.ID,
      NyxQualifiedID(APrefix, ANode.ID), LDesignID);
    try
      Result.Props.Assign(ANode.Props);
      { Instance publisher overrides must be visible before nested recipe
        selection, including chains of derived reference definitions. NUL is a
        private missing-field sentinel, distinct from an explicit empty reset. }

      if AQueryContainer <> #0 then
      begin
        Result.SetProp('query-container', AQueryContainer);
      end;

      if AContainment <> #0 then
      begin
        Result.SetProp('container-containment', AContainment);
      end;
      Result.Extensions.Assign(ANode.Extensions);
      LScope := AInstanceScope;

      if (LScope = '') or (ANode.Prop('compound') = 'true') then
      begin
        LScope := Result.ID;
      end;
      Result.SetInstanceScope(LScope);

      if ANode.HasCollectionView then
      begin
        Result.SetCollectionView(ANode.CollectionView);
      end;

      if ANode.HasMenu then
      begin
        Result.SetMenu(ANode.MenuReference);
      end;

      if ANode.HasMenuBar then
      begin
        Result.SetMenuBar(ANode.MenuBar);
      end;
      for LIndex := 0 to ANode.BindingCount - 1 do
      begin
        Result.SetBinding(ANode.Bindings[LIndex]);
      end;

      if ARootKind <> '' then
      begin
        { A derived reference retains its semantic name for custom factory/event
          dispatch, while the expanded definition supplies the physical base. }
        Result.SetProp('projection-kind', ANode.ProjectionKind);
      end;
      Result.SetProp('design-id', LDesignID);
      { Parent pointers are attached after recursive expansion. Keep an explicit
        composer-owned ancestry stack so nested instances can query the correct
        qualified publisher before their root is attached. }
      SetLength(LAncestors, Length(LAncestors) + 1);
      LAncestors[High(LAncestors)] := Result;
      try
        for LIndex := 0 to ANode.Count - 1 do
        begin
          Result.Add(Expand(ANode.Children[LIndex], APrefix, ADesignID, ADepth + 1, '', LScope));
        end;
      finally
        SetLength(LAncestors, Length(LAncestors) - 1);
      end;
    except
      Result.Free;
      raise;
    end;
  end;

begin

  if (ADocument = nil) or (ARoot = nil) then
  begin
    raise ENyxModel.Create('Document and view root are required');
  end;
  { A view root is a borrowed node of its source document. Checking ownership by
    ancestry avoids admitting an unchecked detached tree or realizing a view twice. }

  if not ADocument.Contains(ARoot) or ARoot.IsRealized then
  begin
    raise ENyxModel.Create('View root must belong to its source design document');
  end;
  ADocument.Validate;
  LPresentations := ADocument.Presentations.Snapshot;
  AFrame.Selection.Validate(LPresentations);
  LCount := 0;
  LAncestors := nil;
  Result := Expand(ARoot, '', '', 0);
  try
    Result.BindPresentations(LPresentations);
    PrepareNyxBehavior(Result);
  except
    Result.Free;
    raise;
  end;
end;

function CloneNyxViewDocument(ADocument: TNyxDocument; ARoot: TNyxNode): TNyxDocument;
var
  LCandidate: TNyxDocument;
  LIndex: Integer;

  procedure CollectDefinitions(ANode: TNyxNode); forward;

  procedure CollectMenu(const AReference: TNyxMenuRef);
  var
    LDefinition: INyxMenuDefinition;
    LContent: TNyxNode;
    LItemIndex: Integer;
  begin

    if LCandidate.Menus.Contains(AReference) then
    begin
      Exit;
    end;
    LDefinition := ADocument.Menus.Definition(AReference);
    LContent := ADocument.FindRoot(LDefinition.Root);

    if LContent = ARoot then
    begin
      { The isolated main view is a page even when its source was reusable. }
      LDefinition := CopyNyxMenuDefinitionToRoot(LDefinition, NyxPageRoot(ARoot.ID));
    end;
    LCandidate.Menus.Define(AReference, LDefinition);

    if (LContent <> ARoot) and (LCandidate.FindRoot(LDefinition.Root) = nil) then
    begin

      if LDefinition.Root.Kind = nrReusable then
      begin
        LCandidate.AddComponent(LContent.Clone);
      end
      else
      begin
        LCandidate.AddPage(LContent.Clone);
      end;
      CollectDefinitions(LContent);
    end;
    for LItemIndex := 0 to LDefinition.Count - 1 do
    begin

      if LDefinition.Item(LItemIndex).Kind = nmiSubmenu then
      begin
        CollectMenu(LDefinition.Item(LItemIndex).Submenu);
      end;
    end;
  end;

  procedure CollectDefinitions(ANode: TNyxNode);
  var
    LIndex: Integer;
    LRuleIndex: Integer;

    procedure CollectDefinition(const AName: TNyxText);
    var
      LDefinition: TNyxNode;
    begin
      LDefinition := ADocument.FindComponent(AName);

      if LCandidate.FindComponent(LDefinition.ID) = nil then
      begin
        LCandidate.AddComponent(LDefinition.Clone);
        CollectDefinitions(LDefinition);
      end;
    end;

  begin

    if ANode.HasMenu and (ANode.MenuReference.Name <> '') then
    begin
      CollectMenu(ANode.MenuReference);
    end;

    if ANode.HasMenuBar and (ANode.MenuBar <> nil) then
    begin
      for LIndex := 0 to ANode.MenuBar.Count - 1 do
      begin
        CollectMenu(ANode.MenuBar.Item(LIndex).Menu);
      end;
    end;

    if (ANode.Kind = 'component') or (ANode.ProjectionKind = 'component') then
    begin
      { Source validation has already ruled out missing definitions and cycles.
        Adding each definition before following its references also deduplicates
        shared dependencies. Authored IDs need no preview prefix or truncation. }

      CollectDefinition(ANode.DefaultComponent.Name);

      if ANode.Prop('component') <> '' then
      begin
        CollectDefinition(ANode.Prop('component'));
      end;

      if ANode.HasContent then
      begin
        for LRuleIndex := 0 to ANode.Content.Count - 1 do
        begin
          CollectDefinition(ANode.Content.Rule(LRuleIndex).Component.Name);
        end;
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      CollectDefinitions(ANode.Children[LIndex]);
    end;
  end;

begin

  if (ADocument = nil) or (ARoot = nil) then
  begin
    raise ENyxModel.Create('A document and authored view are required');
  end;

  if not ADocument.Contains(ARoot) or ARoot.IsRealized then
  begin
    raise ENyxModel.Create('View must belong to the authored document');
  end;
  ADocument.Validate;
  LCandidate := TNyxDocument.Create;
  try
    LCandidate.Title := ADocument.Title;
    { Standalone page/component builds need the same authored state defaults as
      the application. Copy values, never subscriptions or a shared mutable store. }
    LCandidate.State.Assign(ADocument.State);
    { Collection definitions are immutable owned snapshots. Define re-admits
      them into the isolated document; it never copies runtime subscriptions. }
    for LIndex := 0 to ADocument.Collections.Count - 1 do
    begin
      LCandidate.Collections.Define(ADocument.Collections.Snapshot(ADocument.Collections.Key(LIndex)));
    end;
    LCandidate.Extensions.Assign(ADocument.Extensions);
    for LIndex := 0 to ADocument.Presentations.Count - 1 do
    begin
      LCandidate.Presentations.Define(ADocument.Presentations.Reference(LIndex),
        ADocument.Presentations.Definition(ADocument.Presentations.Reference(LIndex)));
    end;
    LCandidate.AddPage(ARoot.Clone);
    CollectDefinitions(ARoot);
    LCandidate.Validate;
    Result := LCandidate;
  except
    LCandidate.Free;
    raise;
  end;
end;

end.
