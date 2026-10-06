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

uses SysUtils, Math, nyx.responsive;

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
  begin
    LProbe.ApplyViewport(AWidth, AHeight, LTarget);
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

begin
  LPresentations := APresentations;

  if LPresentations = nil then
  begin
    LPresentations := ANode.PresentationSnapshot;
  end;
  LWidths := nil;
  LHeights := nil;
  for LIndex := 0 to ANode.Props.Count - 1 do
  begin

    if TryNyxResponsiveKey(ANode.Props.Names[LIndex], LPresentations,
      LViewport, LPlatform, LAttribute) and
      (LAttribute in [atWidth, atHeight, atMinimumWidth, atMaximumWidth,
      atMinimumHeight, atMaximumHeight, atSplitPosition, atSplitMinimum, atSplitMaximum]) then
    begin
      { Other presentation rules cannot change size/split validity. Excluding
        them avoids a Cartesian admission cost for unrelated text/gap rules. }
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
