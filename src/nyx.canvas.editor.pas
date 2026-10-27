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
unit nyx.canvas.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.canvas.types, nyx.canvas.editing, nyx.canvas.geometry, nyx.canvas.precision,
  nyx.canvas.declarations;

type
  { Stable references into the embedded-control scaling form. Closed choices
    map to TNyxCanvasControlScaling only on explicit Apply; labels are UI text. }
  TNyxCanvasScalingPart = (ncspTitle, ncspScope, ncspChoice, ncspHelp, ncspApply);

  { Stable references into the ordinary precision form. Field identities are
    target-independent; the host owns the form and deliberate publication. }
  TNyxCanvasPrecisionPart = (ncppTitle, ncppScope, ncppFields, ncppHelp,
    ncppApply, ncppLoad, ncppAxes, ncppOriginX, ncppOriginY, ncppUnitSize,
    ncppUnitCaption, ncppIncrement, ncppMinimumPixels, ncppMinorDivisions,
    ncppThickness, ncppSnapping, ncppTolerance, ncppGuideChoice, ncppGuideAction,
    ncppGuideName, ncppGuideAxis, ncppGuidePosition, ncppGuideVisible, ncppGuideLocked);

  { Stable public references into the reusable navigation form. The region
    contains its four numeric fields; callers customize ordinary Nyx controls
    without depending on private suffix strings or target widget classes. }
  TNyxCanvasBoundaryPart = (ncbpTitle, ncbpScope, ncbpMode, ncbpRegion,
    ncbpLeft, ncbpTop, ncbpWidth, ncbpHeight, ncbpHelp, ncbpApply);

  { Copied, uncommitted choice. The form owns no editor/session/document.
    The owning host rechecks Baseline and publishes Change through its usual
    revision-aware grouped command. }
  TNyxCanvasEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Change: TNyxCanvasChange;
  end;

  { Captured shared-scene settings proposal, shared by scaling, navigation and precision
    forms. Names/data are independent of the form/document. The owning editor
    publishes Change through its existing
    revision-aware transaction; Baseline is exact scene intent, not runtime pan. }
  TNyxCanvasBoundaryEditorChange = record
    Scene: TNyxCanvasRef;
    Baseline: TNyxText;
    Change: TNyxCanvasChange;
  end;

  { Independent unfinished navigation fields, scoped to one exact scene
    snapshot. Capture/Restore retain copied text, never controls or documents.
    Missing or changed forms park the draft; only an exact matching context
    receives it. Successful application consumes that same proposal. Untouched
    defaults create no draft or new presentation version. Capture refuses a
    copied UTF-8 packet above twice MaximumNyxCanvasDefinitionBytes before
    replacing prior retained data; the caller's form remains untouched. }
  TNyxCanvasBoundaryDraft = record
  private
    FEditor: TNyxText;
    FContext: TNyxText;
    FValues: TNyxDataValue;
    function GetDefined: Boolean;
  public
    procedure Clear;
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCanvasBoundaryDraft; static;
    property Defined: Boolean read GetDefined;
  end;

  { Independent unfinished precision strings, scoped to the exact saved scene.
    No document/view/callback survives capture. Missing or changed contexts park
    the proposal; its exact completed application consumes it. Invalid numbers
    remain editable. Copied packets above twice the scene byte budget refuse
    before replacing retained data; untouched forms create no presentation entry. }
  TNyxCanvasPrecisionDraft = record
  private
    FEditor: TNyxText;
    FContext: TNyxText;
    FValues: TNyxDataValue;
    function GetDefined: Boolean;
  public
    procedure Clear;
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCanvasPrecisionDraft; static;
    property Defined: Boolean read GetDefined;
  end;

{ Owned reference to one public form part; no node/view is retained. AID is
  nonempty so derived forms and capture retain one stable identity. }
function NyxCanvasBoundaryPart(const AID: TNyxText;
  APart: TNyxCanvasBoundaryPart): TNyxControlRef;
{ Bounded scene membership/reference baseline; no entire scene/document dump. }
function NyxCanvasEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
{ Public reusable Nyx Card/Select/Button form; selecting a choice does not mutate
  the authored document. Empty workspace and exact scene names remain distinct. }
function NewNyxCanvasSceneEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
{ Unrelated input returns False. An owned Apply button captures only an admitted
  choice from the exact mounted form; malformed/missing fields refuse. }
function CaptureNyxCanvasSceneEditor(AButton, ARoot: TNyxNode;
  out AChange: TNyxCanvasEditorChange): Boolean;

{ Reusable Nyx form for a saved scene's live embedded-control zoom policy.
  It copies exact scene intent without retaining the document or runtime view.
  A choice remains uncommitted until the owning host admits the captured Apply. }
function NewNyxCanvasScalingEditor(const AID: TNyxText; const AScene: TNyxCanvasRef;
  ADocument: TNyxDocument): INyxCard;
{ Stable nonempty identity of one public scaling-form part. }
function NyxCanvasScalingPart(const AID: TNyxText;
  APart: TNyxCanvasScalingPart): TNyxControlRef;
{ Unrelated controls return False. Unknown choices, missing fields and stale
  scene definitions refuse without changing authored or unfinished form data.
  The host still guards the complete project revision/source before publication. }
function CaptureNyxCanvasScalingEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxCanvasBoundaryEditorChange): Boolean;

{ Ordinary reusable Nyx form for a saved scene's shared navigation policy.
  AID is a nonempty stable form identity.
  Unbounded defaults seed fields from the initial visible world. Numeric text is
  an unfinished proposal until Apply; no document/view/callback is retained. }
function NewNyxCanvasBoundaryEditor(const AID: TNyxText; const AScene: TNyxCanvasRef;
  ADocument: TNyxDocument): INyxCard;
{ Unrelated buttons return False. Stale scene data, unknown choices and invalid
  finite numbers refuse without rewriting fields or the source document.
  A host must recheck its own complete project revision before publication. }
function CaptureNyxCanvasBoundaryEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxCanvasBoundaryEditorChange): Boolean;

{ Reusable rulers/snapping/guide form. It retains copied exact scene intent,
  never the document, editor or runtime view. Apply is explicit and atomic. }
function NewNyxCanvasPrecisionEditor(const AID: TNyxText; const AScene: TNyxCanvasRef;
  ADocument: TNyxDocument): INyxCard;
{ Stable nonempty identity of one public precision form part. }
function NyxCanvasPrecisionPart(const AID: TNyxText;
  APart: TNyxCanvasPrecisionPart): TNyxControlRef;
{ Local Load deliberately replaces guide fields only; it grants no document
  authority. Unrelated controls return False; malformed choices refuse. }
function RouteNyxCanvasPrecisionGuide(AButton, ARoot: TNyxNode): Boolean;
{ Capture an exact saved-scene proposal; invalid/stale input leaves all fields
  and the document untouched. The host still guards its complete revision. }
function CaptureNyxCanvasPrecisionEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxCanvasBoundaryEditorChange): Boolean;

implementation

uses
  nyx.composition, nyx.state, nyx.bytes;

const
  CPrecisionEditor = 'nyx.canvas-precision-editor';
  CNoRulers = 'Hidden';
  CBothRulers = 'Top and left';
  CTopRuler = 'Top only';
  CLeftRuler = 'Left only';
  CPrecisionSnapChoices: array[TNyxCanvasGuideSnapping] of TNyxText =
    ('Off', 'Visible guides', 'All guides');
  CNewGuide = 'New guide';
  CGuidePrefix = 'Guide / ';
  CKeepGuides = 'Keep guides';
  CWriteGuide = 'Add or update guide';
  CRemoveGuide = 'Remove guide';
  CVerticalGuide = 'Vertical / X';
  CHorizontalGuide = 'Horizontal / Y';
  CYes = 'Yes';
  CNo = 'No';
  CPrecisionFields: array[0..17] of TNyxCanvasPrecisionPart =
    (ncppAxes, ncppOriginX, ncppOriginY, ncppUnitSize, ncppUnitCaption,
     ncppIncrement, ncppMinimumPixels, ncppMinorDivisions, ncppThickness,
     ncppSnapping, ncppTolerance, ncppGuideChoice, ncppGuideAction, ncppGuideName,
     ncppGuideAxis, ncppGuidePosition, ncppGuideVisible, ncppGuideLocked);
  CPrecisionKeys: array[0..17] of TNyxText =
    ('axes', 'originX', 'originY', 'unitSize', 'unitCaption', 'increment',
     'minimumPixels', 'minorDivisions', 'thickness', 'snapping', 'tolerance',
     'guideChoice', 'guideAction', 'guideName', 'guideAxis', 'guidePosition',
     'guideVisible', 'guideLocked');
  CEditor = 'nyx.canvas-editor';
  CEmpty = 'Empty workspace';
  CInherit = 'Use inherited scene';
  CScenePrefix = 'Scene / ';
  CBoundaryEditor = 'nyx.canvas-boundary-editor';
  CInfinite = 'Unbounded';
  CCenter = 'Camera center';
  CViewport = 'Visible area';
  CBoundaryFields: array[0..4] of TNyxCanvasBoundaryPart =
    (ncbpMode, ncbpLeft, ncbpTop, ncbpWidth, ncbpHeight);
  CBoundaryKeys: array[0..4] of TNyxText = ('mode', 'left', 'top', 'width', 'height');

{ Snapshot only the five authoring fields; arbitrary descendants and callbacks
  are not retained. A rebuilt form must expose the same public part contract. }
function BoundaryValues(AForm: TNyxNode): TNyxDataValue;
var
  LFields: array[0..4] of TNyxDataField;
  LIndex: Integer;
  LNode: TNyxNode;
begin
  for LIndex := 0 to High(LFields) do
  begin
    LNode := AForm.Find(NyxCanvasBoundaryPart(AForm.ID, CBoundaryFields[LIndex]).ID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Navigation proposal field is unavailable');
    end;
    LFields[LIndex] := NyxField(CBoundaryKeys[LIndex],
      NyxData(LNode.Prop(NyxAttributeName(atValue))));
  end;
  Result := NyxObject(LFields);
end;

{ Interpret text only on explicit Apply or to recognize its completed result.
  Capturing and persisting an unfinished draft never requires numeric admission. }
function BoundaryCamera(const ACamera: TNyxCanvasCamera;
  const AValues: TNyxDataValue): TNyxCanvasCamera;
var
  LChoice: TNyxText;
  LMode: TNyxCanvasBoundaryMode;

  function Number(const AKey: TNyxText): Double;
  begin

    if not TryNyxStateNumber(AValues.Field(AKey).AsText, Result) then
    begin
      raise ENyxModel.Create('Navigation bounds require finite numbers');
    end;
  end;
begin
  LChoice := AValues.Field('mode').AsText;

  if LChoice = CInfinite then
  begin
    Exit(ACamera.WithoutBounds);
  end;
  LMode := ncbViewport;

  if LChoice = CCenter then
  begin
    LMode := ncbCenter;
  end
  else if LChoice <> CViewport then
  begin
    raise ENyxModel.Create('Unknown navigation boundary choice');
  end;
  Result := ACamera.Within(NyxCanvasBounds(Number('left'), Number('top'),
    Number('width'), Number('height')), LMode);
end;

{ One formatting contract for both the form and its untouched-draft comparison.
  Reading defaults copies five strings without constructing a document/tree. }
function BoundaryDefaults(const ACamera: TNyxCanvasCamera): TNyxDataValue;
var
  LBounds: TNyxCanvasBounds;
  LChoice: TNyxText;
begin
  LBounds := ACamera.NavigationBounds;

  if not LBounds.Defined then
  begin
    LBounds := ACamera.VisibleWorld;
  end;
  case ACamera.BoundaryMode of
    ncbUnbounded:
      begin
        LChoice := CInfinite;
      end;
    ncbCenter:
      begin
        LChoice := CCenter;
      end;
    ncbViewport:
      begin
        LChoice := CViewport;
      end;
  end;
  Result := NyxObject([NyxField('mode', NyxData(LChoice)),
    NyxField('left', NyxData(NyxStateNumberText(LBounds.Left))),
    NyxField('top', NyxData(NyxStateNumberText(LBounds.Top))),
    NyxField('width', NyxData(NyxStateNumberText(LBounds.Width))),
    NyxField('height', NyxData(NyxStateNumberText(LBounds.Height)))]);
end;

function NyxCanvasBoundaryPart(const AID: TNyxText;
  APart: TNyxCanvasBoundaryPart): TNyxControlRef;
const
  CNames: array[TNyxCanvasBoundaryPart] of TNyxText =
    ('title', 'scope', 'mode', 'region', 'left', 'top', 'width', 'height', 'help', 'apply');
begin

  if AID = '' then
  begin
    raise ENyxModel.Create('Navigation editor requires a named form');
  end;

  if (Ord(APart) < Ord(Low(TNyxCanvasBoundaryPart))) or
    (Ord(APart) > Ord(High(TNyxCanvasBoundaryPart))) then
  begin
    raise ENyxModel.Create('Unknown navigation editor part');
  end;
  Result := NyxControl(AID + '-' + CNames[APart]);
end;

function NewNyxCanvasBoundaryEditor(const AID: TNyxText; const AScene: TNyxCanvasRef;
  ADocument: TNyxDocument): INyxCard;
var
  LDefinition: INyxCanvasDefinition;
  LDefaults: TNyxDataValue;
  LFields: INyxGrid;

  function Part(APart: TNyxCanvasBoundaryPart): TNyxText;
  begin
    Result := NyxCanvasBoundaryPart(AID, APart).ID;
  end;
begin

  if (ADocument = nil) or not ADocument.Canvases.Contains(AScene) then
  begin
    raise ENyxModel.Create('Navigation editor requires a saved scene');
  end;
  LDefinition := ADocument.Canvases.Definition(AScene);
  LDefaults := BoundaryDefaults(LDefinition.Camera);
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CBoundaryEditor), NyxObject([
    NyxField('scene', NyxData(AScene.Name)), NyxField('definition', LDefinition.ToData)]));
  Result.Add(NewNyxLabel(Part(ncbpTitle)).WithText('Scene navigation'));
  Result.Add(NewNyxLabel(Part(ncbpScope)).WithText(TNyxText('Shared scene / ') + AScene.Name));
  Result.Add(NewNyxSelect(Part(ncbpMode)).Configure.Text('Constrain').PartName(NyxPart('mode'))
    .Items(CInfinite + #10 + CCenter + #10 + CViewport).Value(LDefaults.Field('mode').AsText).Done);
  LFields := NewNyxGrid(Part(ncbpRegion));
  LFields.Configure.Columns(2).Gap(8).PartName(NyxPart('region')).Done;
  LFields.Add(NewNyxInput(Part(ncbpLeft)).Configure.Text('Left').PartName(NyxPart('left'))
    .Value(LDefaults.Field('left').AsText).Done);
  LFields.Add(NewNyxInput(Part(ncbpTop)).Configure.Text('Top').PartName(NyxPart('top'))
    .Value(LDefaults.Field('top').AsText).Done);
  LFields.Add(NewNyxInput(Part(ncbpWidth)).Configure.Text('Width').PartName(NyxPart('width'))
    .Value(LDefaults.Field('width').AsText).Done);
  LFields.Add(NewNyxInput(Part(ncbpHeight)).Configure.Text('Height').PartName(NyxPart('height'))
    .Value(LDefaults.Field('height').AsText).Done);
  Result.Add(LFields);
  Result.Add(NewNyxLabel(Part(ncbpHelp)).WithText(
    'World coordinates. A zero width or height locks that axis. Unbounded ignores these fields.'));
  Result.Add(NewNyxButton(Part(ncbpApply)).Configure.Text('Apply navigation').PartName(NyxPart('apply')).Done);
end;

function CaptureNyxCanvasBoundaryEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxCanvasBoundaryEditorChange): Boolean;
var
  LForm: TNyxNode;
  LBaseline: TNyxDataValue;
  LDefinition: INyxCanvasDefinition;
  LCamera: TNyxCanvasCamera;
begin
  Result := False;
  AChange := Default(TNyxCanvasBoundaryEditorChange);

  if (AButton = nil) or (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;
  LForm := AButton.Parent;

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CBoundaryEditor)) or
    (AButton.ID <> NyxCanvasBoundaryPart(LForm.ID, ncbpApply).ID) then
  begin
    Exit;
  end;
  LBaseline := LForm.Extensions.Value(NyxExtension(CBoundaryEditor));
  AChange.Scene := NyxCanvasRef(LBaseline.Field('scene').AsText);
  AChange.Baseline := LBaseline.Field('definition').ToJSON;

  if (ADocument = nil) or not ADocument.Canvases.Contains(AChange.Scene) then
  begin
    raise ENyxModel.Create('The navigation scene is no longer available');
  end;
  LDefinition := ADocument.Canvases.Definition(AChange.Scene);

  if LDefinition.ToData.ToJSON <> AChange.Baseline then
  begin
    raise ENyxModel.Create('Open this scene again before changing its navigation');
  end;
  LCamera := BoundaryCamera(LDefinition.Camera, BoundaryValues(LForm));
  AChange.Change := NyxDefineCanvas(AChange.Scene, LDefinition.WithCamera(LCamera));
  Result := True;
end;

function NyxCanvasEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LNode: TNyxNode;
  LNames: array of TNyxDataValue;
  LIndex: Integer;
  LScene: TNyxDataValue;
  LEffective: TNyxDataValue;
  LContext: TNyxNode;
  LProjection: TNyxNode;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Scene editor requires its source document');
  end;
  LNode := ADocument.Find(AOwner.ID);

  if LNode = nil then
  begin
    raise ENyxModel.Create('Scene editor requires an authored canvas control');
  end;
  LContext := RealizeNyxContext(ADocument, LNode, LProjection);
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkCanvas)) then
    begin
      raise ENyxModel.Create('Scene editor requires an effective canvas control');
    end;
    LEffective := NyxNull;

    if LProjection.CanvasScene.Defined then
    begin
      LEffective := NyxData(LProjection.CanvasScene.Name);
    end;
  finally
    LContext.Free;
  end;
  LScene := NyxNull;

  if LNode.CanvasScene.Defined then
  begin
    LScene := NyxData(LNode.CanvasScene.Name);
  end;
  SetLength(LNames, ADocument.Canvases.Count);
  for LIndex := 0 to High(LNames) do
  begin
    LNames[LIndex] := NyxData(ADocument.Canvases.Reference(LIndex).Name);
  end;
  Result := NyxObject([NyxField('owner', NyxData(AOwner.ID)),
    NyxField('scene', LScene), NyxField('localDeclared', NyxData(LNode.HasCanvasScene)),
    NyxField('effectiveScene', LEffective), NyxField('choices', NyxArray(LNames))]).ToJSON;
end;

function NewNyxCanvasSceneEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
var
  LBaseline: TNyxText;
  LItems: TNyxText;
  LSelected: TNyxText;
  LIndex: Integer;
  LNode: TNyxNode;
  LButton: INyxButton;
begin
  LBaseline := NyxCanvasEditorBaseline(ADocument, AOwner);
  LItems := CInherit + #10 + CEmpty;
  LSelected := CInherit;
  LNode := ADocument.Find(AOwner.ID);
  for LIndex := 0 to ADocument.Canvases.Count - 1 do
  begin
    LItems := LItems + #10 + CScenePrefix + ADocument.Canvases.Reference(LIndex).Name;
  end;

  if LNode.CanvasScene.Defined then
  begin
    LSelected := CScenePrefix + LNode.CanvasScene.Name;
  end
  else if LNode.HasCanvasScene then
  begin
    LSelected := CEmpty;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxData(LBaseline));
  Result.Add(NewNyxLabel(AID + '-title').WithText('Canvas scene'));
  Result.Add(NewNyxSelect(AID + '-choice').Configure.Text('Saved scene')
    .Items(LItems).Value(LSelected).Done);
  LButton := NewNyxButton(AID + '-apply');
  LButton.WithText('Use scene');
  Result.Add(LButton);
end;

function CaptureNyxCanvasSceneEditor(AButton, ARoot: TNyxNode;
  out AChange: TNyxCanvasEditorChange): Boolean;
var
  LForm: TNyxNode;
  LChoice: TNyxNode;
  LBaseline: TNyxDataValue;
  LNames: TNyxDataValue;
  LValue: TNyxText;
  LIndex: Integer;
  LFound: Boolean;
begin
  Result := False;
  AChange := Default(TNyxCanvasEditorChange);

  if (AButton = nil) or (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;
  LForm := AButton.Parent;

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CEditor)) or
    (AButton.ID <> LForm.ID + '-apply') then
  begin
    Exit;
  end;
  AChange.Baseline := LForm.Extensions.Value(NyxExtension(CEditor)).AsText;
  LBaseline := TNyxDataValue.ParseJSON(AChange.Baseline);
  AChange.Owner := NyxControl(LBaseline.Field('owner').AsText);
  LChoice := LForm.Find(LForm.ID + '-choice');

  if LChoice = nil then
  begin
    raise ENyxModel.Create('Scene choice field is unavailable');
  end;
  LValue := LChoice.Prop(NyxAttributeName(atValue));

  if LValue = CInherit then
  begin
    AChange.Change := NyxDetachCanvas(AChange.Owner);
  end
  else if LValue = CEmpty then
  begin
    AChange.Change := NyxEmptyCanvas(AChange.Owner);
  end
  else
  begin
    LNames := LBaseline.Field('choices');
    LFound := False;
    for LIndex := 0 to LNames.Count - 1 do
    begin

      if LValue = CScenePrefix + LNames.Item(LIndex).AsText then
      begin
        AChange.Change := NyxAttachCanvas(AChange.Owner, NyxCanvasRef(LNames.Item(LIndex).AsText));
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxModel.Create('Choose one exact saved scene from this mounted form');
    end;
  end;
  Result := True;
end;

{$I nyx.canvas.editor.precision.inc}
{$I nyx.canvas.editor.precision.draft.inc}
{$I nyx.canvas.editor.draft.inc}
{$I nyx.canvas.editor.scaling.inc}

end.
