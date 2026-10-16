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
}{ Precision authoring uses ordinary controls and copied proposal data. Runtime
  gesture edits remain separate from deliberate document/source publication. }

function NyxCanvasPrecisionPart(const AID: TNyxText;
  APart: TNyxCanvasPrecisionPart): TNyxControlRef;
const
  CNames: array[TNyxCanvasPrecisionPart] of TNyxText =
    ('title', 'scope', 'fields', 'help', 'apply', 'load', 'axes', 'origin-x',
     'origin-y', 'unit-size', 'unit-caption', 'increment', 'minimum-pixels',
     'minor-divisions', 'thickness', 'snapping', 'tolerance', 'guide-choice',
     'guide-action', 'guide-name', 'guide-axis', 'guide-position', 'guide-visible', 'guide-locked');
begin

  if (AID = '') or (Ord(APart) < Ord(Low(TNyxCanvasPrecisionPart))) or
    (Ord(APart) > Ord(High(TNyxCanvasPrecisionPart))) then
  begin
    raise ENyxModel.Create('Precision editor requires a named, known part');
  end;
  Result := NyxControl(AID + '-' + CNames[APart]);
end;

{ Snapshot exact field strings; invalid unfinished numbers are legitimate drafts. }
function PrecisionValues(AForm: TNyxNode): TNyxDataValue;
var
  LFields: array[0..17] of TNyxDataField;
  LIndex: Integer;
  LNode: TNyxNode;
begin
  for LIndex := 0 to High(LFields) do
  begin
    LNode := AForm.Find(NyxCanvasPrecisionPart(AForm.ID, CPrecisionFields[LIndex]).ID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Precision proposal field is unavailable');
    end;
    LFields[LIndex] := NyxField(CPrecisionKeys[LIndex],
      NyxData(LNode.Prop(NyxAttributeName(atValue))));
  end;
  Result := NyxObject(LFields);
end;

{ One defaults contract for mounting and identifying untouched proposal fields. }
function PrecisionDefaults(const ADefinition: INyxCanvasDefinition): TNyxDataValue;
var
  LOptions: TNyxCanvasPrecisionOptions;
  LRulers: TNyxCanvasRulers;
  LAxes: TNyxText;
  LSnapping: TNyxText;
begin
  LOptions := NyxCanvasPrecisionOptionsOf(ADefinition);

  if not LOptions.Defined then
  begin
    LOptions := NyxCanvasPrecisionOptions;
  end;
  LRulers := LOptions.Rulers;
  LAxes := CNoRulers;

  if not LRulers.Defined then
  begin
    LRulers := NyxCanvasRulers;
  end
  else if LRulers.VisibleAxes = [ncaxX, ncaxY] then
  begin
    LAxes := CBothRulers;
  end
  else if ncaxX in LRulers.VisibleAxes then
  begin
    LAxes := CTopRuler;
  end
  else if ncaxY in LRulers.VisibleAxes then
  begin
    LAxes := CLeftRuler;
  end;
  LSnapping := CPrecisionSnapChoices[LOptions.SnapMode];
  Result := NyxObject([
    NyxField('axes', NyxData(LAxes)),
    NyxField('originX', NyxData(NyxStateNumberText(LRulers.Origin.X))),
    NyxField('originY', NyxData(NyxStateNumberText(LRulers.Origin.Y))),
    NyxField('unitSize', NyxData(NyxStateNumberText(LRulers.UnitSize))),
    NyxField('unitCaption', NyxData(LRulers.UnitCaption)),
    NyxField('increment', NyxData(NyxStateNumberText(LRulers.Increment))),
    NyxField('minimumPixels', NyxData(NyxStateNumberText(LRulers.MinimumPixels))),
    NyxField('minorDivisions', NyxData(IntToStr(LRulers.MinorDivisions))),
    NyxField('thickness', NyxData(NyxStateNumberText(LRulers.StripSize))),
    NyxField('snapping', NyxData(LSnapping)),
    NyxField('tolerance', NyxData(NyxStateNumberText(LOptions.TolerancePixels))),
    NyxField('guideChoice', NyxData(CNewGuide)),
    NyxField('guideAction', NyxData(CKeepGuides)),
    NyxField('guideName', NyxData(LOptions.NextGuide.Name)),
    NyxField('guideAxis', NyxData(CVerticalGuide)),
    NyxField('guidePosition', NyxData('0')),
    NyxField('guideVisible', NyxData(CYes)),
    NyxField('guideLocked', NyxData(CNo))]);
end;

{ Admit the complete proposal before creating an owned document command.
  Explicit authoring can unlock/update a guide; runtime locks are input guards. }
function PrecisionProposal(const ADefinition: INyxCanvasDefinition;
  const AValues: TNyxDataValue): INyxCanvasDefinition;
var
  LOptions: TNyxCanvasPrecisionOptions;
  LRulers: TNyxCanvasRulers;
  LAxes: TNyxCanvasAxes;
  LAxis: TNyxCanvasAxis;
  LSnap: TNyxCanvasGuideSnapping;
  LDivisions: Integer;
  LChoice: TNyxText;
  LRef: TNyxCanvasGuideRef;
  LFound: Boolean;

  function Text(const AKey: TNyxText): TNyxText;
  begin
    Result := AValues.Field(AKey).AsText;
  end;

  function Number(const AKey: TNyxText): Double;
  begin

    if not TryNyxStateNumber(Text(AKey), Result) then
    begin
      raise ENyxModel.Create('Precision requires a finite number: ' + AKey);
    end;
  end;

  function Flag(const AKey: TNyxText): Boolean;
  begin
    Result := Text(AKey) = CYes;

    if not Result and (Text(AKey) <> CNo) then
    begin
      raise ENyxModel.Create('Precision requires Yes or No: ' + AKey);
    end;
  end;
begin
  LOptions := NyxCanvasPrecisionOptionsOf(ADefinition);

  if not LOptions.Defined then
  begin
    LOptions := NyxCanvasPrecisionOptions;
  end;
  LChoice := Text('axes');
  LAxes := [];

  if LChoice = CBothRulers then
  begin
    LAxes := [ncaxX, ncaxY];
  end
  else if LChoice = CTopRuler then
  begin
    LAxes := [ncaxX];
  end
  else if LChoice = CLeftRuler then
  begin
    LAxes := [ncaxY];
  end
  else if LChoice <> CNoRulers then
  begin
    raise ENyxModel.Create('Unknown ruler visibility');
  end;

  if not TryNyxStateInteger(Text('minorDivisions'), LDivisions) then
  begin
    raise ENyxModel.Create('Minor divisions require an integer');
  end;
  LRulers := NyxCanvasRulers.Axes(LAxes)
    .FromOrigin(NyxCanvasPoint(Number('originX'), Number('originY')))
    .Units(Number('unitSize'), Text('unitCaption'))
    .Interval(Number('increment'), Number('minimumPixels'), LDivisions)
    .Thickness(Number('thickness'));
  LOptions := LOptions.WithRulers(LRulers);
  LFound := False;
  for LSnap := Low(TNyxCanvasGuideSnapping) to High(TNyxCanvasGuideSnapping) do
  begin

    if Text('snapping') = CPrecisionSnapChoices[LSnap] then
    begin
      LOptions := LOptions.Snapping(LSnap, Number('tolerance'));
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxModel.Create('Unknown guide snapping policy');
  end;
  LChoice := Text('guideAction');

  if LChoice <> CKeepGuides then
  begin
    LRef := NyxCanvasGuideRef(Text('guideName'));

    if LChoice = CRemoveGuide then
    begin

      if not LOptions.Contains(LRef) then
      begin
        raise ENyxModel.Create('Choose an existing guide to remove');
      end;
      LOptions := LOptions.WithoutGuide(LRef);
    end
    else if LChoice = CWriteGuide then
    begin
      LAxis := ncaxX;

      if Text('guideAxis') = CHorizontalGuide then
      begin
        LAxis := ncaxY;
      end
      else if Text('guideAxis') <> CVerticalGuide then
      begin
        raise ENyxModel.Create('Unknown guide direction');
      end;
      LOptions := LOptions.WithGuide(NyxCanvasGuide(LRef, LAxis, Number('guidePosition'))
        .Visible(Flag('guideVisible')).Locked(Flag('guideLocked')));
    end
    else
    begin
      raise ENyxModel.Create('Unknown guide operation');
    end;
  end;
  Result := NyxCanvasPrecision(ADefinition).Options(LOptions).Done;
end;

function NewNyxCanvasPrecisionEditor(const AID: TNyxText; const AScene: TNyxCanvasRef;
  ADocument: TNyxDocument): INyxCard;
const
  CTitles: array[0..17] of TNyxText = ('Rulers', 'Origin X', 'Origin Y',
    'World units per display unit', 'Unit caption', 'Minimum increment',
    'Major interval pixels', 'Minor divisions', 'Ruler thickness', 'Guide snapping',
    'Snap tolerance pixels', 'Saved guide', 'Guide operation', 'Guide name',
    'Guide direction', 'World position', 'Visible', 'Locked');
var
  LDefinition: INyxCanvasDefinition;
  LDefaults: TNyxDataValue;
  LGrid: INyxGrid;
  LIndex: Integer;
  LChoices: TNyxText;
  LOptions: TNyxCanvasPrecisionOptions;
  LGuideChoices: TNyxText;
  LID: TNyxText;
begin

  if (ADocument = nil) or not ADocument.Canvases.Contains(AScene) then
  begin
    raise ENyxModel.Create('Precision editor requires a saved scene');
  end;
  LDefinition := ADocument.Canvases.Definition(AScene);
  LDefaults := PrecisionDefaults(LDefinition);
  LOptions := NyxCanvasPrecisionOptionsOf(LDefinition);
  LGuideChoices := CNewGuide;
  for LIndex := 0 to LOptions.Count - 1 do
  begin
    LGuideChoices := LGuideChoices + #10 + CGuidePrefix + LOptions.Guide(LIndex).Reference.Name;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CPrecisionEditor), NyxObject([
    NyxField('scene', NyxData(AScene.Name)), NyxField('definition', LDefinition.ToData)]));
  Result.Add(NewNyxLabel(NyxCanvasPrecisionPart(AID, ncppTitle).ID).WithText('Rulers and guides'));
  Result.Add(NewNyxLabel(NyxCanvasPrecisionPart(AID, ncppScope).ID)
    .WithText(TNyxText('Shared scene / ') + AScene.Name));
  LGrid := NewNyxGrid(NyxCanvasPrecisionPart(AID, ncppFields).ID);
  LGrid.Configure.Columns(2).Gap(8).Done;
  for LIndex := 0 to High(CPrecisionFields) do
  begin
    case CPrecisionFields[LIndex] of
      ncppAxes:
        begin
          LChoices := CNoRulers + #10 + CBothRulers + #10 + CTopRuler + #10 + CLeftRuler;
        end;
      ncppSnapping:
        begin
          LChoices := CPrecisionSnapChoices[ncgspOff] + #10 +
            CPrecisionSnapChoices[ncgspVisible] + #10 + CPrecisionSnapChoices[ncgspAll];
        end;
      ncppGuideChoice:
        begin
          LChoices := LGuideChoices;
        end;
      ncppGuideAction:
        begin
          LChoices := CKeepGuides + #10 + CWriteGuide + #10 + CRemoveGuide;
        end;
      ncppGuideAxis:
        begin
          LChoices := CVerticalGuide + #10 + CHorizontalGuide;
        end;
      ncppGuideVisible, ncppGuideLocked:
        begin
          LChoices := CYes + #10 + CNo;
        end;
    else
      begin
        { Every remaining declared field is an ordinary unfinished text input. }
        LChoices := '';
      end;
    end;
    LID := NyxCanvasPrecisionPart(AID, CPrecisionFields[LIndex]).ID;

    if LChoices <> '' then
    begin
      LGrid.Add(NewNyxSelect(LID).Configure.Text(CTitles[LIndex]).Items(LChoices)
        .Value(LDefaults.Field(CPrecisionKeys[LIndex]).AsText).Done);
    end
    else
    begin
      LGrid.Add(NewNyxInput(LID).Configure.Text(CTitles[LIndex])
        .Value(LDefaults.Field(CPrecisionKeys[LIndex]).AsText).Done);
    end;
  end;
  Result.Add(LGrid);
  Result.Add(NewNyxButton(NyxCanvasPrecisionPart(AID, ncppLoad).ID).WithText('Load guide fields'));
  Result.Add(NewNyxLabel(NyxCanvasPrecisionPart(AID, ncppHelp).ID).WithText(
    'Load replaces guide fields only. Apply saves rulers, snapping and the selected guide operation together. Runtime dragging does not save this document.'));
  Result.Add(NewNyxButton(NyxCanvasPrecisionPart(AID, ncppApply).ID).WithText('Apply precision'));
end;

function RouteNyxCanvasPrecisionGuide(AButton, ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LDefinition: INyxCanvasDefinition;
  LOptions: TNyxCanvasPrecisionOptions;
  LChoice: TNyxText;
  LValues: TNyxDataValue;
  LGuide: TNyxCanvasGuide;
  LIndex: Integer;
  LFields: array of TNyxDataField;
begin
  Result := False;

  if (AButton = nil) or (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;
  LForm := AButton.Parent;

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CPrecisionEditor)) or
    (AButton.ID <> NyxCanvasPrecisionPart(LForm.ID, ncppLoad).ID) then
  begin
    Exit;
  end;
  LDefinition := NyxCanvasDefinitionFromData(LForm.Extensions.Value(
    NyxExtension(CPrecisionEditor)).Field('definition'));
  LOptions := NyxCanvasPrecisionOptionsOf(LDefinition);
  LChoice := LForm.Find(NyxCanvasPrecisionPart(LForm.ID, ncppGuideChoice).ID)
    .Prop(NyxAttributeName(atValue));
  LValues := PrecisionDefaults(LDefinition);

  if LChoice <> CNewGuide then
  begin

    if Copy(LChoice, 1, Length(CGuidePrefix)) <> CGuidePrefix then
    begin
      raise ENyxModel.Create('Choose one saved guide from this form');
    end;
    LGuide := LOptions.Named(NyxCanvasGuideRef(Copy(LChoice, Length(CGuidePrefix) + 1, MaxInt)));
    SetLength(LFields, 6);
    LFields[0] := NyxField('guideName', NyxData(LGuide.Reference.Name));
    LChoice := CVerticalGuide;

    if LGuide.Axis = ncaxY then
    begin
      LChoice := CHorizontalGuide;
    end;
    LFields[1] := NyxField('guideAxis', NyxData(LChoice));
    LFields[2] := NyxField('guidePosition', NyxData(NyxStateNumberText(LGuide.Position)));
    LChoice := CNo;

    if LGuide.IsVisible then
    begin
      LChoice := CYes;
    end;
    LFields[3] := NyxField('guideVisible', NyxData(LChoice));
    LChoice := CNo;

    if LGuide.IsLocked then
    begin
      LChoice := CYes;
    end;
    LFields[4] := NyxField('guideLocked', NyxData(LChoice));
    LFields[5] := NyxField('guideAction', NyxData(CWriteGuide));
    LValues := NyxObject(LFields);
  end;
  { Only the six explicitly loaded guide fields are replaced. Ruler, snapping
    and other project drafts remain owned by their caller. }
  for LIndex := 12 to High(CPrecisionFields) do
  begin
    LForm.Find(NyxCanvasPrecisionPart(LForm.ID, CPrecisionFields[LIndex]).ID)
      .Configure.Value(LValues.Field(CPrecisionKeys[LIndex]).AsText).Done;
  end;
  Result := True;
end;

function CaptureNyxCanvasPrecisionEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxCanvasBoundaryEditorChange): Boolean;
var
  LForm: TNyxNode;
  LBaseline: TNyxDataValue;
  LDefinition: INyxCanvasDefinition;
begin
  Result := False;
  AChange := Default(TNyxCanvasBoundaryEditorChange);

  if (AButton = nil) or (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;
  LForm := AButton.Parent;

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CPrecisionEditor)) or
    (AButton.ID <> NyxCanvasPrecisionPart(LForm.ID, ncppApply).ID) then
  begin
    Exit;
  end;
  LBaseline := LForm.Extensions.Value(NyxExtension(CPrecisionEditor));
  AChange.Scene := NyxCanvasRef(LBaseline.Field('scene').AsText);
  AChange.Baseline := LBaseline.Field('definition').ToJSON;

  if (ADocument = nil) or not ADocument.Canvases.Contains(AChange.Scene) then
  begin
    raise ENyxModel.Create('The precision scene is no longer available');
  end;
  LDefinition := ADocument.Canvases.Definition(AChange.Scene);

  if LDefinition.ToData.ToJSON <> AChange.Baseline then
  begin
    raise ENyxModel.Create('Open this scene again before changing precision');
  end;
  AChange.Change := NyxDefineCanvas(AChange.Scene,
    PrecisionProposal(LDefinition, PrecisionValues(LForm)));
  Result := True;
end;
