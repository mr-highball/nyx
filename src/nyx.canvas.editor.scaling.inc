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
{ Included by nyx.canvas.editor. Display labels stay at this form boundary;
  callers and the captured scene definition use the closed portable enum. }

const
  CScalingEditor = 'nyx.canvas-scaling-editor';
  CScalingChoices: array[TNyxCanvasControlScaling] of TNyxText =
    ('Reflow', 'Keep size', 'Uniform zoom');

function NyxCanvasScalingPart(const AID: TNyxText;
  APart: TNyxCanvasScalingPart): TNyxControlRef;
const
  CNames: array[TNyxCanvasScalingPart] of TNyxText =
    ('title', 'scope', 'choice', 'help', 'apply');
begin

  if AID = '' then
  begin
    raise ENyxModel.Create('Scaling editor requires a named form');
  end;

  if (Ord(APart) < Ord(Low(TNyxCanvasScalingPart))) or
    (Ord(APart) > Ord(High(TNyxCanvasScalingPart))) then
  begin
    raise ENyxModel.Create('Unknown scaling editor part');
  end;
  Result := NyxControl(AID + '-' + CNames[APart]);
end;

function NewNyxCanvasScalingEditor(const AID: TNyxText; const AScene: TNyxCanvasRef;
  ADocument: TNyxDocument): INyxCard;
var
  LDefinition: INyxCanvasDefinition;

  function Part(APart: TNyxCanvasScalingPart): TNyxText;
  begin
    Result := NyxCanvasScalingPart(AID, APart).ID;
  end;

begin

  if (ADocument = nil) or not ADocument.Canvases.Contains(AScene) then
  begin
    raise ENyxModel.Create('Scaling editor requires a saved scene');
  end;
  LDefinition := ADocument.Canvases.Definition(AScene);
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CScalingEditor), NyxObject([
    NyxField('scene', NyxData(AScene.Name)), NyxField('definition', LDefinition.ToData)]));
  Result.Add(NewNyxLabel(Part(ncspTitle)).WithText('Embedded controls'));
  Result.Add(NewNyxLabel(Part(ncspScope)).WithText(TNyxText('Shared scene / ') + AScene.Name));
  Result.Add(NewNyxSelect(Part(ncspChoice)).Configure.Text('Zoom behavior')
    .PartName(NyxPart('choice'))
    .Items(CScalingChoices[nccsReflow] + #10 + CScalingChoices[nccsLogicalSize] + #10 +
      CScalingChoices[nccsUniform]).Value(CScalingChoices[LDefinition.Scaling]).Done);
  Result.Add(NewNyxLabel(Part(ncspHelp)).WithText(
    'Reflow changes the available layout space. Keep size retains control size. ' +
    'Uniform zoom scales the complete live control, including text. This applies to every use of this scene.'));
  Result.Add(NewNyxButton(Part(ncspApply)).Configure.Text('Apply zoom behavior')
    .PartName(NyxPart('apply')).Done);
end;

function CaptureNyxCanvasScalingEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxCanvasBoundaryEditorChange): Boolean;
var
  LForm: TNyxNode;
  LChoice: TNyxNode;
  LBaseline: TNyxDataValue;
  LDefinition: INyxCanvasDefinition;
  LScaling: TNyxCanvasControlScaling;
begin
  Result := False;
  AChange := Default(TNyxCanvasBoundaryEditorChange);

  if (AButton = nil) or (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;
  LForm := AButton.Parent;

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CScalingEditor)) or
    (AButton.ID <> NyxCanvasScalingPart(LForm.ID, ncspApply).ID) then
  begin
    Exit;
  end;
  LBaseline := LForm.Extensions.Value(NyxExtension(CScalingEditor));
  AChange.Scene := NyxCanvasRef(LBaseline.Field('scene').AsText);
  AChange.Baseline := LBaseline.Field('definition').ToJSON;

  if (ADocument = nil) or not ADocument.Canvases.Contains(AChange.Scene) then
  begin
    raise ENyxModel.Create('The scaling scene is no longer available');
  end;
  LDefinition := ADocument.Canvases.Definition(AChange.Scene);

  if LDefinition.ToData.ToJSON <> AChange.Baseline then
  begin
    raise ENyxModel.Create('Open this scene again before changing its zoom behavior');
  end;
  LChoice := LForm.Find(NyxCanvasScalingPart(LForm.ID, ncspChoice).ID);

  if LChoice = nil then
  begin
    raise ENyxModel.Create('The scaling choice is unavailable');
  end;
  for LScaling := Low(TNyxCanvasControlScaling) to High(TNyxCanvasControlScaling) do
  begin

    if LChoice.Prop(NyxAttributeName(atValue)) = CScalingChoices[LScaling] then
    begin
      AChange.Change := NyxDefineCanvas(AChange.Scene, LDefinition.WithScaling(LScaling));
      Exit(True);
    end;
  end;
  raise ENyxModel.Create('Choose one supported zoom behavior from this mounted form');
end;
