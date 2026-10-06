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
unit nyx.platform;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses nyx.types, nyx.text, nyx.model, nyx.presentations;

{ Project typed presentation overrides into an independently realized tree.
  Defaults and rules in the authored document remain untouched. Fixed platform
  rules are consumed so another target cannot subsequently change this tree.
  Viewport rules remain for resize-time projection on the concrete target. }
procedure ApplyNyxPlatform(ARoot: TNyxNode; APlatform: TNyxPlatform);

{ Admission checks every piecewise host rectangle/orientation on both targets.
  Inconsistent effective size/split bounds refuse before mounting or publishing.
  Candidates contain copied properties only and retain no authored children. }
procedure ValidateNyxViewportBounds(ANode: TNyxNode;
  const APresentations: INyxPresentationSnapshot = nil);

implementation

uses SysUtils, Math, nyx.responsive, nyx.containers;

type
  TConstraintBoundaries = array of Integer;
  { Each actual publisher is an independent logical coordinate space. Several
    names/queries resolving to that same runtime ancestor share one partition. }
  TConstraintSpace = record
    RuntimeID: TNyxText;
    Widths, Heights: TConstraintBoundaries;
    Orientation: Boolean;
    Width, Height: Double;
  end;

{ Container constraints must be checked against independent allocations, never
  against a fabricated viewport or a detached leaf with no ancestors. This
  bounded Cartesian traversal preserves correlations inside each actual space,
  including width-only queries skipping a nearer ineligible size publisher. }
procedure ValidateContainerBounds(ANode: TNyxNode;
  const APresentations: INyxPresentationSnapshot);
var
  LSpaces: array of TConstraintSpace;
  LMeasurements: TNyxContainerMeasurements;
  LMeasured: array of Boolean;
  LManual: array of TNyxPresentationRef;
  LRule: TNyxPresentationCondition;
  LPlatform, LTarget: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LAncestor, LProbe, LRoot: TNyxNode;
  LReference: TNyxContainerRef;
  LIndex, LSpace, LOther: Integer;
  LBudget: Double;
  LPlaneBudget: Double;

  procedure AddBoundary(var AValues: TConstraintBoundaries; AValue: Integer);
  var
    LPosition, LMove: Integer;
  begin
    LPosition := 0;
    while (LPosition < Length(AValues)) and (AValues[LPosition] < AValue) do
    begin
      Inc(LPosition);
    end;

    if (LPosition < Length(AValues)) and (AValues[LPosition] = AValue) then
    begin
      Exit;
    end;
    SetLength(AValues, Length(AValues) + 1);
    for LMove := High(AValues) downto LPosition + 1 do
    begin
      AValues[LMove] := AValues[LMove - 1];
    end;
    AValues[LPosition] := AValue;
  end;

  function CopyAncestry(AOriginal: TNyxNode): TNyxNode;
  var
    LParent, LCopy: TNyxNode;
  begin
    LParent := nil;

    if AOriginal.Parent <> nil then
    begin
      LParent := CopyAncestry(AOriginal.Parent);
    end;
    LCopy := TNyxNode.CreateRealized(AOriginal.Kind, AOriginal.SourceID,
      AOriginal.ID, AOriginal.DesignID);
    try
      LCopy.SetProp('query-container', AOriginal.Prop('query-container'));
      LCopy.SetProp('container-containment', AOriginal.Prop('container-containment'));

      if LParent <> nil then
      begin
        LParent.Add(LCopy);
      end
      else
      begin
        LRoot := LCopy;
      end;
      Result := LCopy;
    except
      LCopy.Free;
      raise;
    end;
  end;

  procedure CheckCombination;
  var
    LChoice, LPosition, LMinimum, LMaximum, LIndex, LCount: Integer;
    LSelection: TNyxPresentationSelection;
    LSnapshot: INyxContainerSnapshot;
    LVisibleMeasurements: TNyxContainerMeasurements;
  begin
    LVisibleMeasurements := nil;
    for LIndex := 0 to High(LMeasurements) do
    begin

      if LMeasured[LIndex] then
      begin
        LCount := Length(LVisibleMeasurements);
        SetLength(LVisibleMeasurements, LCount + 1);
        LVisibleMeasurements[LCount] := LMeasurements[LIndex];
      end;
    end;
    LSnapshot := NewNyxContainerSnapshot(LVisibleMeasurements);
    for LChoice := -1 to High(LManual) do
    begin
      LSelection := TNyxPresentationSelection.None;

      if LChoice >= 0 then
      begin
        LSelection := TNyxPresentationSelection.Use(LManual[LChoice]);
      end;
      LProbe.ApplyViewport(LSpaces[0].Width, LSpaces[0].Height, LTarget,
        LSelection, LSnapshot);
      NyxNodeSizeConstraints(LProbe).Validate;

      if ANode.ProjectionKind = 'split-view' then
      begin
        LPosition := StrToIntDef(LProbe.Prop('split-position'), 65);
        LMinimum := StrToIntDef(LProbe.Prop('split-minimum'), 15);
        LMaximum := StrToIntDef(LProbe.Prop('split-maximum'), 85);

        if (LMinimum > LMaximum) or (LPosition < LMinimum) or (LPosition > LMaximum) then
        begin
          raise ENyxModel.Create('Container split position must fit its bounds on ' + ANode.ID);
        end;
      end;
    end;
  end;

  procedure VisitSpace(AIndex: Integer);
  var
    LWidthIndex, LHeightIndex: Integer;
    LWidthStart, LHeightStart, LWidthEnd, LHeightEnd, LWidth, LHeight: Double;

    procedure VisitPoint(AWidth, AHeight: Double);
    begin
      LSpaces[AIndex].Width := AWidth;
      LSpaces[AIndex].Height := AHeight;

      if AIndex > 0 then
      begin
        LMeasurements[AIndex - 1].Width := AWidth;
        LMeasurements[AIndex - 1].Height := AHeight;
      end;
      VisitSpace(AIndex + 1);
    end;

  begin

    if AIndex = Length(LSpaces) then
    begin
      CheckCombination;
      Exit;
    end;

    if AIndex > 0 then
    begin
      { A nearest publisher with no current box makes its rules inactive.
        Independent absence must be checked alongside other active spaces. }
      LMeasured[AIndex - 1] := False;
      VisitSpace(AIndex + 1);
      LMeasured[AIndex - 1] := True;
    end;
    for LWidthIndex := 0 to High(LSpaces[AIndex].Widths) do
    begin
      LWidthStart := LSpaces[AIndex].Widths[LWidthIndex];
      LWidthEnd := 1.0E20;

      if LWidthIndex < High(LSpaces[AIndex].Widths) then
      begin
        LWidthEnd := LSpaces[AIndex].Widths[LWidthIndex + 1];
      end;
      for LHeightIndex := 0 to High(LSpaces[AIndex].Heights) do
      begin
        LHeightStart := LSpaces[AIndex].Heights[LHeightIndex];
        LHeightEnd := 1.0E20;

        if LHeightIndex < High(LSpaces[AIndex].Heights) then
        begin
          LHeightEnd := LSpaces[AIndex].Heights[LHeightIndex + 1];
        end;
        VisitPoint(LWidthStart, LHeightStart);

        if not LSpaces[AIndex].Orientation then
        begin
          Continue;
        end;
        { One feasible point per positive orientation region, plus the lower
          corner, covers every constant-rule region in this half-open cell. }
        LWidth := Max(0.5, LWidthStart);
        LHeight := Max(LHeightStart, LWidth + 0.5);

        if (LWidth < LWidthEnd) and (LHeight < LHeightEnd) then
        begin
          VisitPoint(LWidth, LHeight);
        end;
        LHeight := Max(0.5, LHeightStart);
        LWidth := Max(LWidthStart, LHeight + 0.5);

        if (LWidth < LWidthEnd) and (LHeight < LHeightEnd) then
        begin
          VisitPoint(LWidth, LHeight);
        end;
        LWidth := Max(0.5, Max(LWidthStart, LHeightStart));

        if (LWidth < LWidthEnd) and (LWidth < LHeightEnd) then
        begin
          VisitPoint(LWidth, LWidth);
        end;
      end;
    end;
  end;

begin
  SetLength(LSpaces, 1);
  LManual := nil;
  for LIndex := 0 to APresentations.Count - 1 do
  begin

    if APresentations.Definition(APresentations.Reference(LIndex)).Activation = npaManual then
    begin
      SetLength(LManual, Length(LManual) + 1);
      LManual[High(LManual)] := APresentations.Reference(LIndex);
    end;
  end;
  for LIndex := 0 to ANode.Props.Count - 1 do
  begin

    if not TryNyxPresentationRule(ANode.Props.Names[LIndex], APresentations,
      LRule, LPlatform, LAttribute) or not
      (LAttribute in [atWidth, atHeight, atMinimumWidth, atMaximumWidth,
      atMinimumHeight, atMaximumHeight, atSplitPosition, atSplitMinimum, atSplitMaximum]) then
    begin
      Continue;
    end;
    LSpace := 0;

    if LRule.Container.Defined then
    begin
      LAncestor := ANode.Parent;
      while LAncestor <> nil do
      begin
        LReference := LAncestor.QueryContainer;

        if LReference.Defined and (LReference.Name = LRule.Container.Name) and
          NyxContainerEligible(LAncestor.ContainerContainment, LRule.Viewport) then
        begin
          Break;
        end;
        LAncestor := LAncestor.Parent;
      end;

      if LAncestor = nil then
      begin
        Continue;
      end;
      LSpace := -1;
      for LOther := 1 to High(LSpaces) do
      begin

        if LSpaces[LOther].RuntimeID = LAncestor.ID then
        begin
          LSpace := LOther;
          Break;
        end;
      end;

      if LSpace < 0 then
      begin
        LSpace := Length(LSpaces);
        SetLength(LSpaces, LSpace + 1);
        LSpaces[LSpace].RuntimeID := LAncestor.ID;
      end;
    end;
    AddBoundary(LSpaces[LSpace].Widths, LRule.Viewport.WidthMinimum);
    AddBoundary(LSpaces[LSpace].Widths, LRule.Viewport.WidthMaximum);
    AddBoundary(LSpaces[LSpace].Heights, LRule.Viewport.HeightMinimum);
    AddBoundary(LSpaces[LSpace].Heights, LRule.Viewport.HeightMaximum);
    LSpaces[LSpace].Orientation := LSpaces[LSpace].Orientation or
      (LRule.Viewport.OrientationValue <> nvoAny);
  end;
  LBudget := 2.0 * (Length(LManual) + 1);
  for LSpace := 0 to High(LSpaces) do
  begin
    AddBoundary(LSpaces[LSpace].Widths, 0);
    AddBoundary(LSpaces[LSpace].Heights, 0);
    LPlaneBudget := 1.0 * Length(LSpaces[LSpace].Widths) * Length(LSpaces[LSpace].Heights);

    if LSpaces[LSpace].Orientation then
    begin
      LPlaneBudget := LPlaneBudget * 4;
    end;

    if LSpace > 0 then
    begin
      LPlaneBudget := LPlaneBudget + 1;
    end;
    LBudget := LBudget * LPlaneBudget;

    if LBudget > 65536 then
    begin
      raise ENyxModel.Create('Container constraint partition budget exceeded on ' + ANode.ID);
    end;
  end;
  SetLength(LMeasurements, Length(LSpaces) - 1);
  SetLength(LMeasured, Length(LMeasurements));
  for LSpace := 1 to High(LSpaces) do
  begin
    LMeasurements[LSpace - 1].RuntimeID := LSpaces[LSpace].RuntimeID;
  end;
  for LTarget := npfBrowser to npfNativeLCL do
  begin
    LRoot := nil;
    try
      LProbe := CopyAncestry(ANode);
      LProbe.Props.Assign(ANode.Props);
      LProbe.BindPresentations(APresentations);
      ApplyNyxPlatform(LRoot, LTarget);
      VisitSpace(0);
    finally
      LRoot.Free;
    end;
  end;
end;

procedure ApplyNyxPlatform(ARoot: TNyxNode; APlatform: TNyxPlatform);

  procedure Visit(ANode: TNyxNode);
  var
    LCount: Integer;
    LIndex: Integer;
    LPlatform: TNyxPlatform;
    LAttribute: TNyxAttribute;
    LKey: TNyxText;
  begin
    LCount := ANode.Props.Count;
    for LIndex := 0 to LCount - 1 do
    begin
      LKey := ANode.Props.Names[LIndex];

      if TryNyxPlatformKey(LKey, LPlatform, LAttribute) and
        (LPlatform = APlatform) then
      begin
        ANode.SetProp(NyxAttributeName(LAttribute), ANode.Prop(LKey));
      end;
    end;
    for LIndex := ANode.Props.Count - 1 downto 0 do
    begin

      if TryNyxPlatformKey(ANode.Props.Names[LIndex], LPlatform, LAttribute) then
      begin
        ANode.Props.Delete(LIndex);
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex]);
    end;
  end;

begin

  if (ARoot = nil) or not ARoot.IsRealized or (APlatform = npfAny) then
  begin
    raise ENyxModel.Create('Platform projection requires a realized tree and a concrete target');
  end;
  Visit(ARoot);
end;

procedure ValidateNyxViewportBounds(ANode: TNyxNode;
  const APresentations: INyxPresentationSnapshot);
var
  LPresentations: INyxPresentationSnapshot;
  LWidths: array of Integer;
  LHeights: array of Integer;
  LViewport: TNyxViewportCondition;
  LRule: TNyxPresentationCondition;
  LPlatform: TNyxPlatform;
  LTarget: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LIndex: Integer;
  LWidthIndex: Integer;
  LHeightIndex: Integer;
  LWidthEnd: Double;
  LHeightEnd: Double;
  LWidthStart: Double;
  LHeightStart: Double;
  LWidth: Double;
  LHeight: Double;
  LProbe: TNyxNode;
  LPosition: Integer;
  LMinimum: Integer;
  LMaximum: Integer;
  LManual: array of TNyxPresentationRef;

  procedure AddBoundary(AValue: Integer; AHeight: Boolean);
  var
    LPosition: Integer;
    LMove: Integer;
    LValues: array of Integer;
  begin
    { Explicit copied arrays avoid shared candidate mutation on either compiler.
      Sorted integer boundaries partition the entire nonnegative host plane. }

    if AHeight then
    begin
      LValues := Copy(LHeights, 0, Length(LHeights));
    end
    else
    begin
      LValues := Copy(LWidths, 0, Length(LWidths));
    end;
    LPosition := 0;
    while (LPosition < Length(LValues)) and (LValues[LPosition] < AValue) do
    begin
      Inc(LPosition);
    end;

    if (LPosition < Length(LValues)) and (LValues[LPosition] = AValue) then
    begin
      Exit;
    end;
    SetLength(LValues, Length(LValues) + 1);
    for LMove := High(LValues) downto LPosition + 1 do
    begin
      LValues[LMove] := LValues[LMove - 1];
    end;
    LValues[LPosition] := AValue;

    if AHeight then
    begin
      LHeights := LValues;
    end
    else
    begin
      LWidths := LValues;
    end;
  end;

  procedure CheckPoint(AWidth, AHeight: Double);
  var
    LChoice: Integer;
    LSelection: TNyxPresentationSelection;
  begin
    { Exclusive manual choices are independent of host rectangles. Test every
      admitted choice against each automatic partition; testing manual scopes
      at one guessed viewport would miss conflicting automatic size bounds. }
    for LChoice := -1 to High(LManual) do
    begin
      LSelection := TNyxPresentationSelection.None;

      if LChoice >= 0 then
      begin
        LSelection := TNyxPresentationSelection.Use(LManual[LChoice]);
      end;
      LProbe.ApplyViewport(AWidth, AHeight, LTarget, LSelection);
      NyxNodeSizeConstraints(LProbe).Validate;

      if ANode.ProjectionKind = 'split-view' then
      begin
        LPosition := StrToIntDef(LProbe.Prop('split-position'), 65);
        LMinimum := StrToIntDef(LProbe.Prop('split-minimum'), 15);
        LMaximum := StrToIntDef(LProbe.Prop('split-maximum'), 85);

        if (LMinimum > LMaximum) or (LPosition < LMinimum) or (LPosition > LMaximum) then
        begin
          raise ENyxModel.Create('Responsive split position must fit its bounds on ' + ANode.ID);
        end;
      end;
    end;
  end;

begin
  LPresentations := APresentations;

  if LPresentations = nil then
  begin
    LPresentations := ANode.PresentationSnapshot;
  end;
  LWidths := nil;
  LHeights := nil;
  LManual := nil;

  if LPresentations <> nil then
  begin
    for LIndex := 0 to LPresentations.Count - 1 do
    begin

      if LPresentations.Definition(LPresentations.Reference(LIndex)).Activation = npaManual then
      begin
        SetLength(LManual, Length(LManual) + 1);
        LManual[High(LManual)] := LPresentations.Reference(LIndex);
      end;
    end;
  end;
  for LIndex := 0 to ANode.Props.Count - 1 do
  begin

    if TryNyxPresentationRule(ANode.Props.Names[LIndex], LPresentations,
      LRule, LPlatform, LAttribute) and
      (LAttribute in [atWidth, atHeight, atMinimumWidth, atMaximumWidth,
      atMinimumHeight, atMaximumHeight, atSplitPosition, atSplitMinimum, atSplitMaximum]) then
    begin
      { Other presentation rules cannot change size/split validity. Excluding
        them avoids a Cartesian admission cost for unrelated text/gap rules. }

      if LRule.Container.Defined then
      begin
        ValidateContainerBounds(ANode, LPresentations);
        Exit;
      end;
      LViewport := LRule.Viewport;
      AddBoundary(LViewport.WidthMinimum, False);
      AddBoundary(LViewport.WidthMaximum, False);
      AddBoundary(LViewport.HeightMinimum, True);
      AddBoundary(LViewport.HeightMaximum, True);
    end;
  end;

  if Length(LWidths) = 0 then
  begin
    Exit;
  end;
  AddBoundary(0, False);
  AddBoundary(0, True);

  if 1.0 * Length(LWidths) * Length(LHeights) * (Length(LManual) + 1) * 8 > 65536 then
  begin
    raise ENyxModel.Create('Presentation constraint partition budget exceeded on ' + ANode.ID);
  end;
  for LTarget := npfBrowser to npfNativeLCL do
  begin
    LProbe := TNyxNode.CreateRealized(ANode.Kind, ANode.ID, ANode.ID, ANode.ID);
    try
      LProbe.Props.Assign(ANode.Props);
      LProbe.BindPresentations(LPresentations);
      ApplyNyxPlatform(LProbe, LTarget);
      for LWidthIndex := 0 to High(LWidths) do
      begin
        LWidthStart := LWidths[LWidthIndex];
        LWidthEnd := 1.0E20;

        if LWidthIndex < High(LWidths) then
        begin
          LWidthEnd := LWidths[LWidthIndex + 1];
        end;
        for LHeightIndex := 0 to High(LHeights) do
        begin
          LHeightStart := LHeights[LHeightIndex];
          LHeightEnd := 1.0E20;

          if LHeightIndex < High(LHeights) then
          begin
            LHeightEnd := LHeights[LHeightIndex + 1];
          end;
          { Rules are constant inside each half-open rectangle except across
            the orientation diagonal and zero axes. Its lower corner covers
            unconstrained/zero-host behavior; one feasible point in each
            positive diagonal region covers every remaining rule combination.
            Integer authored bounds make a half-pixel offset exact on both
            targets, including thin cells and unbounded tails. }
          CheckPoint(LWidthStart, LHeightStart);
          LWidth := Max(0.5, LWidthStart);
          LHeight := Max(LHeightStart, LWidth + 0.5);

          if (LWidth < LWidthEnd) and (LHeight < LHeightEnd) then
          begin
            CheckPoint(LWidth, LHeight);
          end;
          LHeight := Max(0.5, LHeightStart);
          LWidth := Max(LWidthStart, LHeight + 0.5);

          if (LWidth < LWidthEnd) and (LHeight < LHeightEnd) then
          begin
            CheckPoint(LWidth, LHeight);
          end;
          LWidth := Max(0.5, Max(LWidthStart, LHeightStart));

          if (LWidth < LWidthEnd) and (LWidth < LHeightEnd) then
          begin
            CheckPoint(LWidth, LWidth);
          end;
        end;
      end;
    finally
      LProbe.Free;
    end;
  end;
end;

end.
