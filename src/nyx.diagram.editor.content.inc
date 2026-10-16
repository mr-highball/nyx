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
{ Shared descriptor-to-canvas staging. Every new root goes through the public
  prepared membership owner; geometry-only updates preserve actual live controls. }

function TEditorView.BuildContentPlan(const ADiagram: INyxDiagram;
  const AProjection: INyxDrawingPlan; out ABindings: TDiagramContentBindings;
  out ANextContentID: Integer; ASource: TNyxDocument): INyxCanvasLayoutPlan;
var
  LContent: INyxCanvasSceneContent;
  LLayouts: INyxCanvasSceneLayouts;
  LChanges: array of INyxCanvasContentChange;
  LVertex: TNyxDiagramVertex;
  LBinding: TDiagramContentBinding;
  LControl: INyxNode;
  LPlacement: TNyxCanvasPlacement;
  LIndex: Integer;
  LPrevious: Integer;
  LFound: Integer;
  LCount: Integer;
  LChangeCount: Integer;
  LHasContent: Boolean;
  LRetained: array of Boolean;
  LSourceNames: TNyxStrings;
  LSourceEqual: array of Boolean;

  { Cache one exact closure comparison per content reference in this proposal.
    Booleans remain detached; large source strings never accumulate per vertex. }
  function SourceEqual(const AContent: TNyxDiagramContent): Boolean;
  var
    LKey: TNyxText;
    LFound: Integer;
  begin
    Result := ASource = FDocument;

    if Result then
    begin
      Exit;
    end;
    LKey := TNyxText(IntToStr(Ord(AContent.Kind))) + ':';

    if AContent.Kind = ndckComponent then
    begin
      LKey := LKey + AContent.Component.Name;
    end
    else
    begin
      LKey := LKey + AContent.Image.Reference.Name;
    end;
    LFound := LSourceNames.IndexOf(LKey);

    if LFound < 0 then
    begin
      LFound := LSourceNames.Count;
      LSourceNames.Add(LKey);
      LSourceEqual[LFound] := SameNyxDiagramSource(FDocument, ASource, AContent);
    end;
    Result := LSourceEqual[LFound];
  end;
begin

  if ASource = nil then
  begin
    ASource := FDocument;
  end;
  LSourceNames := TNyxStrings.Create;
  SetLength(LSourceEqual, ADiagram.VertexCount);
  try
    ABindings := nil;
    ANextContentID := FNextContentID;
    LHasContent := Length(FContentBindings) > 0;
    for LIndex := 0 to ADiagram.VertexCount - 1 do
    begin
      LHasContent := LHasContent or (ADiagram.Vertex(LIndex).Content.Kind <> ndckNone);
    end;

    if not LHasContent then
    begin

      if not Supports(FScene, INyxCanvasSceneLayouts, LLayouts) then
      begin
        raise ENyxDiagram.Create('Diagram scene lacks prepared placement');
      end;
      Exit(LLayouts.PrepareLayout([NyxCanvasPlace(FItem,
        NyxCanvasPlacement(FLayer, AProjection.ViewBox))], FScene.Revision));
    end;

    if not Supports(FScene, INyxCanvasSceneContent, LContent) then
    begin
      raise ENyxDiagram.Create('Diagram scene lacks prepared content membership');
    end;
    SetLength(ABindings, ADiagram.VertexCount);
    SetLength(LRetained, Length(FContentBindings));
    SetLength(LChanges, 1 + ADiagram.VertexCount + Length(FContentBindings));
    LChanges[0] := NyxCanvasPlaceControl(FItem, NyxCanvasPlacement(FLayer, AProjection.ViewBox));
    LChangeCount := 1;
    LCount := 0;
    for LIndex := 0 to ADiagram.VertexCount - 1 do
    begin
      LVertex := ADiagram.Vertex(LIndex);

      if LVertex.Content.Kind = ndckNone then
      begin
        Continue;
      end;
      LFound := -1;
      for LPrevious := 0 to High(FContentBindings) do
      begin

        if FContentBindings[LPrevious].Vertex.Reference.Name = LVertex.Reference.Name then
        begin
          LFound := LPrevious;
          Break;
        end;
      end;
      LBinding := Default(TDiagramContentBinding);

      if LFound >= 0 then
      begin
        LRetained[LFound] := True;
        LBinding.Item := FContentBindings[LFound].Item;
        LBinding.Control := FContentBindings[LFound].Control;
      end
      else
      begin

        if ANextContentID = High(Integer) then
        begin
          raise ENyxDiagram.Create('Diagram content identities are exhausted');
        end;
        Inc(ANextContentID);
        LBinding.Item := NyxCanvasItem('Content ' + TNyxText(IntToStr(ANextContentID)));
        LBinding.Control := NyxControl(FEditorID + '-diagram-content-' + TNyxText(IntToStr(ANextContentID)));
      end;
      LBinding.Vertex := LVertex;
      ABindings[LCount] := LBinding;
      Inc(LCount);
      LPlacement := NyxCanvasPlacement(FContentLayer, NyxDiagramContentBounds(LVertex));

      if (LFound >= 0) and SameNyxDiagramContent(FContentBindings[LFound].Vertex, LVertex) and
        SourceEqual(LVertex.Content) then
      begin
        LChanges[LChangeCount] := NyxCanvasPlaceControl(LBinding.Item, LPlacement);
      end
      else
      begin
        LControl := NewNyxDiagramContentControl(LBinding.Control.ID, LVertex);

        if LFound >= 0 then
        begin
          LChanges[LChangeCount] := NyxCanvasReplaceControl(LBinding.Item, LPlacement, LControl);
        end
        else
        begin
          LChanges[LChangeCount] := NyxCanvasAddControl(LBinding.Item, LPlacement, LControl);
        end;
      end;
      Inc(LChangeCount);
    end;
    for LPrevious := 0 to High(FContentBindings) do
    begin

      if not LRetained[LPrevious] then
      begin
        LChanges[LChangeCount] := NyxCanvasRemoveControl(FContentBindings[LPrevious].Item);
        Inc(LChangeCount);
      end;
    end;
    SetLength(ABindings, LCount);
    SetLength(LChanges, LChangeCount);
    Result := LContent.PrepareContent(LChanges, FScene.Revision);
  finally
    LSourceNames.Free;
  end;
end;

function TEditorView.ContentItem(const AVertex: TNyxDiagramVertexRef): TNyxCanvasItemRef;
var
  LIndex: Integer;
begin
  RequireConnected;
  FDiagram.FindVertex(AVertex);
  Result := Default(TNyxCanvasItemRef);
  for LIndex := 0 to High(FContentBindings) do
  begin

    if FContentBindings[LIndex].Vertex.Reference.Name = AVertex.Name then
    begin
      Exit(FContentBindings[LIndex].Item);
    end;
  end;
end;

function TEditorView.ContentControl(const AVertex: TNyxDiagramVertexRef): TNyxControlRef;
var
  LIndex: Integer;
begin
  RequireConnected;
  FDiagram.FindVertex(AVertex);
  Result := Default(TNyxControlRef);
  for LIndex := 0 to High(FContentBindings) do
  begin

    if FContentBindings[LIndex].Vertex.Reference.Name = AVertex.Name then
    begin
      Exit(FContentBindings[LIndex].Control);
    end;
  end;
end;

procedure TEditorView.SetContent(const AVertex: TNyxDiagramVertexRef;
  const AContent: TNyxDiagramContent; AExpectedRevision: Integer);
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished properties before replacing shape content');
  end;
  Publish(FDiagram.WithVertex(FDiagram.FindVertex(AVertex).WithContent(AContent)), AExpectedRevision);
end;

function TEditorView.SelectedContentKind: TNyxDiagramContentKind;
var
  LKind: TNyxDiagramContentKind;
begin
  { Captions are an editor presentation boundary; graph/source choices remain
    closed Pascal values and never depend on arbitrary user-supplied strings. }
  for LKind := Low(TNyxDiagramContentKind) to High(TNyxDiagramContentKind) do
  begin

    if Field(ndepContentKind).Value = NyxDiagramContentCaption(LKind) then
    begin
      Exit(LKind);
    end;
  end;
  raise ENyxDiagram.Create('Choose a supported content type');
end;

procedure TEditorView.RefreshContentChoices;
var
  LChoices: TNyxStrings;
  LKind: TNyxDiagramContentKind;
  LName: TNyxText;
  LPrevious: TNyxText;
  LIndex: Integer;
begin
  LKind := SelectedContentKind;
  LPrevious := Field(ndepContentReference).Value;
  LChoices := TNyxStrings.Create;
  try

    if LKind = ndckImage then
    begin
      for LIndex := 0 to FDocument.Resources.Count - 1 do
      begin
        LName := FDocument.Resources.Reference(LIndex).Name;

        if (FDocument.Resources.Definition(FDocument.Resources.Reference(LIndex),
          FDocument.Resources.Locale(LIndex)).Kind = nrkImage) and (LChoices.IndexOf(LName) < 0) then
        begin
          LChoices.Add(LName);
        end;
      end;
    end
    else

      if LKind = ndckComponent then
      begin
        for LIndex := 0 to FDocument.ComponentCount - 1 do
        begin
          LChoices.Add(FDocument.Components[LIndex].ID);
        end;
      end;
    (RetainNyxControl(Part(ndepContentReference)) as INyxSelect).Items := LChoices.Text;

    if LChoices.IndexOf(LPrevious) >= 0 then
    begin
      Field(ndepContentReference).Value := LPrevious;
    end
    else
    begin
      Field(ndepContentReference).Value := '';
    end;
  finally
    LChoices.Free;
  end;
end;

procedure TEditorView.RefreshContentHelp;
var
  LKind: TNyxDiagramContentKind;
  LEnabled: Boolean;
  LLocalized: Boolean;
  LHelp: TNyxText;
  LDefinition: INyxResourceDefinition;
  LLocale: TNyxLocaleRef;
  LFallback: TNyxLocaleRef;
begin
  LKind := SelectedContentKind;
  LEnabled := FSelection.Defined and (LKind <> ndckNone);
  LLocalized := (RetainNyxControl(Part(ndepContentLocalized)) as INyxBooleanInput).Checked;
  RetainNyxControl(Part(ndepContentKind)).Configure.Enabled(FSelection.Defined).Done;
  RetainNyxControl(Part(ndepContentReference)).Configure.Enabled(LEnabled).Done;
  RetainNyxControl(Part(ndepContentPadding)).Configure.Enabled(LEnabled).Done;
  RetainNyxControl(Part(ndepContentHeader)).Configure.Enabled(LEnabled).Done;
  RetainNyxControl(Part(ndepContentFit)).Configure.Enabled(LEnabled and (LKind = ndckImage)).Done;
  RetainNyxControl(Part(ndepContentLocalized)).Configure.Enabled(LEnabled and (LKind = ndckImage)).Done;
  RetainNyxControl(Part(ndepContentLocale)).Configure.Enabled(LEnabled and
    (LKind = ndckImage) and LLocalized).Done;
  RetainNyxControl(Part(ndepContentFallback)).Configure.Enabled(LEnabled and
    (LKind = ndckImage) and LLocalized).Done;
  RetainNyxControl(Part(ndepApplyContent)).Configure.Enabled(FSelection.Defined).Done;
  RetainNyxControl(Part(ndepRestoreContent)).Configure.Enabled(FSelection.Defined).Done;
  LHelp := 'Choose a resource or reusable from this editor''s source context.';

  if not FSelection.Defined then
  begin
    LHelp := 'Select a shape to configure its content.';
  end
  else

    if LKind = ndckNone then
    begin
      LHelp := 'The shape has no embedded content. Its caption and connections remain independent.';
    end
    else

      if Field(ndepContentReference).Value <> '' then
      begin
        try

          if LKind = ndckImage then
          begin
            LLocale := NyxDefaultLocale;
            LFallback := NyxDefaultLocale;

            if LLocalized then
            begin
              LLocale := NyxLocale(Field(ndepContentLocale).Value);
              LFallback := NyxLocale(Field(ndepContentFallback).Value);
            end;
            { Discovery reads metadata only. It does not fetch hosted bytes or
              bypass the normal resource resolver, cache or fallback policy. }
            LDefinition := FDocument.Resources.Resolve(
              NyxResourceRef(Field(ndepContentReference).Value), LLocale, LFallback);
            LHelp := LDefinition.Title;

            if LDefinition.Description <> '' then
            begin
              LHelp := LHelp + ' / ' + LDefinition.Description;
            end;
          end
          else
          begin
            LHelp := FDocument.FindComponent(Field(ndepContentReference).Value).Prop(NyxAttributeName(atHint));

            if LHelp = '' then
            begin
              LHelp := 'An independent instance of ' + Field(ndepContentReference).Value +
                ', using its ordinary Nyx controls and bindings.';
            end;
          end;
        except
          on E: Exception do
          begin
            LHelp := E.Message;
          end;
        end;
      end;
  (RetainNyxControl(Part(ndepContentHelp)) as INyxCaptionControl).Text := LHelp;
end;

procedure TEditorView.RefreshContentFields;
var
  LContent: TNyxDiagramContent;
begin
  LContent := NyxNoDiagramContent;

  if FSelection.Defined then
  begin
    LContent := FDiagram.FindVertex(FSelection).Content;
  end;
  Field(ndepContentKind).Value := NyxDiagramContentCaption(LContent.Kind);
  Field(ndepContentReference).Value := '';
  Field(ndepContentPadding).Value := '8';
  Field(ndepContentHeader).Value := '24';
  Field(ndepContentFit).Value := NyxDiagramImageFitCaption(nifContain);
  (RetainNyxControl(Part(ndepContentLocalized)) as INyxBooleanInput).Checked := False;
  Field(ndepContentLocale).Value := '';
  Field(ndepContentFallback).Value := '';

  if LContent.Kind <> ndckNone then
  begin
    Field(ndepContentPadding).Value := NyxData(LContent.Padding).ToJSON;
    Field(ndepContentHeader).Value := NyxData(LContent.HeaderHeight).ToJSON;

    if LContent.Kind = ndckImage then
    begin
      Field(ndepContentReference).Value := LContent.Image.Reference.Name;
      Field(ndepContentFit).Value := NyxDiagramImageFitCaption(LContent.ImageFit);
      (RetainNyxControl(Part(ndepContentLocalized)) as INyxBooleanInput).Checked := LContent.Image.Localized;
      Field(ndepContentLocale).Value := LContent.Image.Locale.Name;
      Field(ndepContentFallback).Value := LContent.Image.Fallback.Name;
    end
    else
    begin
      Field(ndepContentReference).Value := LContent.Component.Name;
    end;
  end;
  RefreshContentChoices;
  RefreshContentHelp;
end;

function TEditorView.ReadContentFields: TNyxDiagramContent;
var
  LImage: TNyxResourceImageRef;
  LFit: TNyxImageFit;
  LFound: Boolean;
begin
  case SelectedContentKind of
    ndckNone: Exit(NyxNoDiagramContent);
    ndckImage:
      begin
        LImage := NyxResourceImage(NyxResourceRef(Field(ndepContentReference).Value));

        if (RetainNyxControl(Part(ndepContentLocalized)) as INyxBooleanInput).Checked then
        begin
          LImage := LImage.Localize(NyxLocale(Field(ndepContentLocale).Value),
            NyxLocale(Field(ndepContentFallback).Value));
        end;
        Result := NyxDiagramImage(LImage);
        LFound := False;
        for LFit := Low(TNyxImageFit) to High(TNyxImageFit) do
        begin

          if Field(ndepContentFit).Value = NyxDiagramImageFitCaption(LFit) then
          begin
            Result := Result.Fitted(LFit);
            LFound := True;
            Break;
          end;
        end;

        if not LFound then
        begin
          raise ENyxDiagram.Create('Choose a supported image fit');
        end;
      end;
    ndckComponent:
      begin
        Result := NyxDiagramComponent(NyxComponent(Field(ndepContentReference).Value));
      end;
  end;
  Result := Result.Padded(GeometryNumber(Field(ndepContentPadding).Value))
    .CaptionSpace(GeometryNumber(Field(ndepContentHeader).Value));
end;

procedure TEditorView.ShowContentTools(AExpanded: Boolean);
begin
  RequireWritable;
  RetainNyxControl(Part(ndepContentTools)).Configure.Visible(AExpanded).Done;
  FContentToolsExpanded := AExpanded;

  if AExpanded then
  begin
    RetainNyxControl(Part(ndepTools)).Configure.Visible(False).Done;
    FToolsExpanded := False;
    RetainNyxControl(Part(ndepDrawTools)).Configure.Visible(False).Done;
    FDrawToolsExpanded := False;
  end;
  FRenderer.Sync;
end;

function TEditorView.GetContentToolsExpanded: Boolean;
begin
  RequireConnected;
  Result := FContentToolsExpanded;
end;
