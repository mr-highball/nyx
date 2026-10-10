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

{ Included by nyx.model. Prepare complete independent arrays before publishing a
  move: an implementation anchor must never be released while its descriptor is
  temporarily unowned. No application callback or validation runs during publish. }

function TNyxDocument.FindControl(const AControl: TNyxControlRef): TNyxNode;
begin
  Result := Find(AControl.ID);
end;

function TNyxDocument.MoveControl(const AControl, AParent: TNyxControlRef;
  AIndex: Integer): TNyxDocument;
var
  LNode: TNyxNode;
  LParent: TNyxNode;
  LPrevious: TNyxNode;
  LAncestor: TNyxNode;
  LPosition: Integer;
  LIndex: Integer;
  LNext: Integer;
  LCount: Integer;
  LPreviousNodes: array of TNyxNode;
  LPreviousAnchors: array of INyxNode;
  LNextNodes: array of TNyxNode;
  LNextAnchors: array of INyxNode;
begin
  LNode := FindControl(AControl);
  LParent := FindControl(AParent);

  if (LNode = nil) or (LParent = nil) or (LNode.Parent = nil) then
  begin
    raise ENyxModel.Create('Moving requires an authored descendant and destination in this document');
  end;
  LPrevious := LNode.Parent;
  LAncestor := LParent;
  while LAncestor <> nil do
  begin

    if LAncestor = LNode then
    begin
      raise ENyxModel.Create('A control cannot move into itself or its descendants');
    end;
    LAncestor := LAncestor.Parent;
  end;
  LCount := LParent.Count;

  if LParent = LPrevious then
  begin
    Dec(LCount);
  end;

  if (AIndex < 0) or (AIndex > LCount) then
  begin
    raise ENyxModel.Create('Move index is outside the destination after removal');
  end;
  LPosition := 0;
  while LPrevious.Children[LPosition] <> LNode do
  begin
    Inc(LPosition);
  end;

  if (LParent = LPrevious) and (LPosition = AIndex) then
  begin
    Exit(Self);
  end;
  SetLength(LNextNodes, LCount + 1);
  SetLength(LNextAnchors, LCount + 1);
  LNext := 0;
  for LIndex := 0 to LParent.Count - 1 do
  begin

    if LParent.Children[LIndex] = LNode then
    begin
      Continue;
    end;

    if LNext = AIndex then
    begin
      Inc(LNext);
    end;
    LNextNodes[LNext] := LParent.FChildren[LIndex];
    LNextAnchors[LNext] := LParent.FChildReferences[LIndex];
    Inc(LNext);
  end;
  LNextNodes[AIndex] := LNode;
  LNextAnchors[AIndex] := LPrevious.FChildReferences[LPosition];

  if LParent <> LPrevious then
  begin
    SetLength(LPreviousNodes, LPrevious.Count - 1);
    SetLength(LPreviousAnchors, LPrevious.Count - 1);
    LNext := 0;
    for LIndex := 0 to LPrevious.Count - 1 do
    begin

      if LIndex = LPosition then
      begin
        Continue;
      end;
      LPreviousNodes[LNext] := LPrevious.FChildren[LIndex];
      LPreviousAnchors[LNext] := LPrevious.FChildReferences[LIndex];
      Inc(LNext);
    end;
  end;
  { All potentially failing allocation and interface retention finished. Each
    descriptor remains owned continuously; only borrowed ancestry is updated. }

  if LParent <> LPrevious then
  begin
    LPrevious.FChildren := LPreviousNodes;
    LPrevious.FChildReferences := LPreviousAnchors;
  end;
  LParent.FChildren := LNextNodes;
  LParent.FChildReferences := LNextAnchors;
  LNode.FParent := LParent;
  Result := Self;
end;

function TNyxDocument.MoveRoot(const ARoot: TNyxRootRef;
  AIndex: Integer): TNyxDocument;
var
  LNode: TNyxNode;
  LCount: Integer;
  LIndex: Integer;
  LNext: Integer;
  LNodes: array of TNyxNode;
  LAnchors: array of INyxNode;
begin
  LNode := FindRoot(ARoot);
  LCount := Count;

  if ARoot.Kind = nrReusable then
  begin
    LCount := ComponentCount;
  end;

  if (LNode = nil) or (AIndex < 0) or (AIndex >= LCount) then
  begin
    raise ENyxModel.Create('Root movement requires an existing exact partition and valid index');
  end;
  SetLength(LNodes, LCount);
  SetLength(LAnchors, LCount);
  LNext := 0;
  for LIndex := 0 to LCount - 1 do
  begin

    if ARoot.Kind = nrPage then
    begin

      if FPages[LIndex] = LNode then
      begin
        LAnchors[AIndex] := FPageReferences[LIndex];
        Continue;
      end;

      if LNext = AIndex then
      begin
        Inc(LNext);
      end;
      LNodes[LNext] := FPages[LIndex];
      LAnchors[LNext] := FPageReferences[LIndex];
    end
    else
    begin

      if FComponents[LIndex] = LNode then
      begin
        LAnchors[AIndex] := FComponentReferences[LIndex];
        Continue;
      end;

      if LNext = AIndex then
      begin
        Inc(LNext);
      end;
      LNodes[LNext] := FComponents[LIndex];
      LAnchors[LNext] := FComponentReferences[LIndex];
    end;
    Inc(LNext);
  end;
  LNodes[AIndex] := LNode;

  if ARoot.Kind = nrPage then
  begin
    FPages := LNodes;
    FPageReferences := LAnchors;
  end
  else
  begin
    FComponents := LNodes;
    FComponentReferences := LAnchors;
  end;
  Result := Self;
end;

function TNyxDocument.RemoveControl(const AControl: TNyxControlRef): TNyxDocument;
var
  LNode: TNyxNode;
begin
  LNode := FindControl(AControl);

  if LNode = nil then
  begin
    raise ENyxModel.Create('The specified control is missing');
  end;

  if LNode.Parent <> nil then
  begin
    LNode.Parent.Remove(LNode);
  end
  else if FindRoot(NyxPageRoot(AControl.ID)) = LNode then
  begin
    RemoveRoot(NyxPageRoot(AControl.ID));
  end
  else
  begin
    RemoveRoot(NyxReusableRoot(AControl.ID));
  end;
  Result := Self;
end;
