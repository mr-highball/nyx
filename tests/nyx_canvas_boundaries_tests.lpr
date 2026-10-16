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
program nyx_canvas_boundaries_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.bytes, nyx.types, nyx.data, nyx.model,
  nyx.controls, nyx.codec, nyx.codegen, nyx.source, nyx.root.types,
  nyx.canvas.geometry, nyx.canvas.types, nyx.canvas.declarations, nyx.canvas.editing,
  nyx.canvas.navigation, nyx.canvas.editor, nyx.flow.types, nyx.flow.declarations, nyx.studio.projects, nyx.studio.session,
  nyx.studio.agents, nyx.studio.presentation
  {$ifdef PAS2JS}, Web{$else}, Classes, nyx.studio.mcp{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Canvas boundaries: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Near(AValue, AExpected: Double; const AReason: TNyxText);
begin
  Check(Abs(AValue - AExpected) <= 1.0e-9, AReason);
end;

{ Forge explicit persistence inputs independently of the immutable public
  authoring contract; the original data and every unrelated member stay owned. }
function ReplaceField(const AValue: TNyxDataValue; const AName: TNyxText;
  const AReplacement: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AValue.Count);
  for LIndex := 0 to AValue.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AValue.Key(LIndex), AValue.Field(AValue.Key(LIndex)));

    if AValue.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AReplacement);
    end;
  end;
  Result := NyxObject(LFields);
end;

procedure Geometry;
var
  LBase: TNyxCanvasCamera;
  LBounded: TNyxCanvasCamera;
  LNext: TNyxCanvasCamera;
  LRegion: TNyxCanvasBounds;
  LBefore: TNyxCanvasCamera;
  LFailed: Boolean;
begin
  LBase := NyxCanvasCamera(100, 80, NyxCanvasPoint(0, 0), 1);
  LRegion := NyxCanvasBounds(-100, -80, 200, 160);
  Check((LBase.BoundaryMode = ncbUnbounded) and not LBase.NavigationBounds.Defined,
    'existing camera defaults remain explicitly unbounded');
  Check(LBase.WithoutBounds.SameFrame(LBase), 'removing absent bounds retains the exact legacy frame');
  LBounded := LBase.Within(LRegion);
  Check((LBounded.BoundaryMode = ncbViewport) and not LBounded.SameFrame(LBase),
    'default boundary constrains the viewport and participates in exact frame identity');
  LNext := LBounded.PanBy(-1000, -1000);
  Near(LNext.WorldCenter.X, 50, 'viewport clamps its right edge');
  Near(LNext.WorldCenter.Y, 40, 'viewport clamps its bottom edge');
  Near(LNext.VisibleWorld.Left + LNext.VisibleWorld.Width, 100, 'visible right edge meets the world region');
  Near(LNext.VisibleWorld.Top + LNext.VisibleWorld.Height, 80, 'visible bottom edge meets the world region');
  LNext := LBounded.PanBy(1.0e12, 1.0e12);
  Near(LNext.WorldCenter.X, -50, 'large finite screen overshoot clamps before point admission');
  Near(LNext.WorldCenter.Y, -40, 'large finite vertical overshoot clamps before point admission');
  LNext := LBounded.AtCenter(NyxCanvasPoint(100, 80), 2);
  Near(LNext.WorldCenter.X, 75, 'atomic center and zoom use the final visible width');
  Near(LNext.WorldCenter.Y, 60, 'atomic center and zoom use the final visible height');
  LNext := LBase.Within(LRegion, ncbCenter).PanBy(-1000, -1000);
  Near(LNext.WorldCenter.X, 100, 'center policy permits the viewport to extend beyond the region');
  Near(LNext.WorldCenter.Y, 80, 'center policy constrains the center independently of zoom');
  Check(not LBase.Within(LRegion, ncbCenter).SameFrame(LBounded),
    'equal geometry with different boundary modes is a different frame');
  Check(not LBase.Within(NyxCanvasBounds(-110, -80, 220, 160)).SameFrame(LBounded),
    'equal geometry with different configured edges is a different frame');
  LNext := LBounded.WithViewport(300, 200);
  Near(LNext.WorldCenter.X, 0, 'oversized viewport centers its horizontal axis');
  Near(LNext.WorldCenter.Y, 0, 'oversized viewport centers its vertical axis');
  Check(LNext.NavigationBounds.Defined and (LNext.BoundaryMode = ncbViewport),
    'native/browser viewport resizing retains the configured policy');
  Check(LNext.WithViewport(100, 80).SameFrame(LBounded), 'restored viewport retains the same bounded frame');
  LNext := LBounded.ZoomAt(2, NyxCanvasPoint(0, 0));
  Near(LNext.WorldCenter.X, -25, 'unclipped anchored zoom keeps the exact horizontal anchor');
  Near(LNext.WorldCenter.Y, -20, 'unclipped anchored zoom keeps the exact vertical anchor');
  Near(LNext.ViewportToWorld(NyxCanvasPoint(0, 0)).X, -50, 'world position beneath a free anchor is retained');
  LNext := LBounded.AtCenter(NyxCanvasPoint(-75, -60), 2)
    .ZoomAt(1, NyxCanvasPoint(100, 80));
  Near(LNext.WorldCenter.X, -50, 'boundary takes precedence when zoom anchor requires leaving the region');
  Near(LNext.WorldCenter.Y, -40, 'zoom-out clamps only the constrained final center');
  LNext := LBounded.AtCenter(NyxCanvasPoint(-50, 0)).Pinch(
    NyxCanvasPoint(-110, 40), NyxCanvasPoint(-90, 40),
    NyxCanvasPoint(-320, 40), NyxCanvasPoint(-280, 40));
  Near(LNext.WorldCenter.X, -25, 'pinch combines scale and translation before clamping intermediate overshoot');
  Near(LNext.Zoom, 2, 'pinch preserves its admitted scale');
  LNext := LBounded.WithLimits(NyxCanvasZoomLimits(0.5, 0.5));
  Near(LNext.Zoom, 0.5, 'new zoom limits apply before boundary admission');
  Near(LNext.WorldCenter.X, 0, 'zoom-limit derivation retains viewport containment');
  LNext := LBounded.Fit(NyxCanvasBounds(1000, 1000, 50, 50), 0);
  Check((LNext.BoundaryMode = ncbViewport) and (LNext.WorldCenter.X <= 100),
    'Fit never discards configured bounds to reach remote content');
  LNext := LBounded.Within(NyxCanvasBounds(12.5, -7.25, 0, 0)).PanBy(-1000, 1000);
  Near(LNext.WorldCenter.X, 12.5, 'point region locks the horizontal center without invented page size');
  Near(LNext.WorldCenter.Y, -7.25, 'point region locks the vertical center');
  Check(LBounded.WithoutBounds.SameFrame(LBase), 'explicitly returning to infinite navigation preserves the current view');
  LBefore := LBounded;
  LFailed := False;
  try
    LNext := LBounded.Within(Default(TNyxCanvasBounds));
  except
    on ENyxCanvasGeometry do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and LBounded.SameFrame(LBefore), 'absent bounds refuse without changing the original record');
  LFailed := False;
  try
    LNext := LBounded.Within(LRegion, ncbUnbounded);
  except
    on ENyxCanvasGeometry do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and LBounded.SameFrame(LBefore), 'contradictory policy requires explicit WithoutBounds');
  LFailed := False;
  try
    LNext := LBounded.WithViewport(NaN, 80);
  except
    on ENyxCanvasGeometry do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and LBounded.SameFrame(LBefore), 'nonfinite viewport refuses atomically');
  Check(not Default(TNyxCanvasCamera).SameFrame(LBounded), 'undefined captures never compare equal to admitted frames');
end;

procedure Navigation;
var
  LCamera: TNyxCanvasCamera;
  LResult: TNyxCanvasNavigationResult;
  LItem: TNyxCanvasNavigationItem;
begin
  LCamera := NyxCanvasCamera(100, 80, NyxCanvasPoint(0, 0), 1)
    .Within(NyxCanvasBounds(-100, -80, 200, 160));
  LItem := NyxCanvasNavigationItem(NyxCanvasBounds(1000, 1000, 20, 20), nccsReflow);
  LResult := FitNyxCanvasNavigation(LCamera, [LItem], [], NyxCanvasNavigationOptions.Inset(0));
  Check((LResult.Status = ncnsLimited) and (LResult.Camera.BoundaryMode = ncbViewport),
    'fit reports Limited when constrained content cannot enter the chosen visible region');
  LResult := RevealNyxCanvasNavigation(LCamera, LItem, [], NyxCanvasNavigationOptions.Inset(0));
  Check((LResult.Status = ncnsLimited) and (LResult.Camera.BoundaryMode = ncbViewport),
    'reveal reports Limited instead of discarding caller bounds');
  LItem := NyxCanvasNavigationItem(NyxCanvasBounds(-10, -10, 20, 20), nccsReflow);
  LResult := FitNyxCanvasNavigation(LCamera, [LItem], [], NyxCanvasNavigationOptions.Inset(0));
  Check((LResult.Status = ncnsComplete) and (LResult.Camera.BoundaryMode = ncbViewport),
    'ordinary in-region fitting remains complete and bounded');
end;

{ Stable English workshop, with exact fractional world edges. Runtime faces and
  compiler consumers receive this entire document rather than a policy fragment. }
function Workshop: TNyxDocument;
var
  LScene: INyxCanvasDefinition;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Bounded workspace';
    Result.AddComponent(NewNyxColumn('Note card')
      .Add(NewNyxMemo('note').WithText('A note that stays editable')));
    LScene := NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0), 1)
        .Within(NyxCanvasBounds(-1000.25, -800.5, 2000.5, 1601)))
      .WithLayer(NyxCanvasLayer('Cards'))
      .WithItem(NyxCanvasItem('Note'), NyxReusableRoot('Note card'),
        NyxCanvasPlacement(NyxCanvasLayer('Cards'), NyxCanvasBounds(-140, -100, 280, 200)));
    Result.Canvases.Define(NyxCanvasRef('Notes'), LScene);
    Result.AddPage(NewNyxColumn('home').Add(NewNyxCanvas('board').WithScene(NyxCanvasRef('Notes'))
      .Configure.Height(500).Done));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

{ The public compound keeps unfinished input separate from its saved scene.
  These are model/form assertions; actual target input is qualified separately. }
procedure Editor;
var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LForm: INyxCard;
  LChange: TNyxCanvasBoundaryEditorChange;
  LScene: TNyxCanvasRef;
  LDefinition: INyxCanvasDefinition;
  LDraft: TNyxCanvasBoundaryDraft;
  LReadDraft: TNyxCanvasBoundaryDraft;
  LPresentation: TNyxStudioPresentation;
  LPresentationRead: TNyxStudioPresentation;
  LDefaultPresentation: TNyxText;
  LPacket: TNyxDataValue;
  LBefore: TNyxText;
  LFailed: Boolean;

  function Part(APart: TNyxCanvasBoundaryPart): TNyxNode;
  begin
    Result := LForm.Node.Find(NyxCanvasBoundaryPart(LForm.ID, APart).ID);
  end;

begin
  LDocument := Workshop;
  LExpected := nil;
  try
    { Dedicated Unicode qualification names never replace the English seed. }
    LScene := NyxCanvasRef('Scène 🌙');
    LDefinition := LDocument.Canvases.Definition(NyxCanvasRef('Notes'));
    LDocument.Canvases.Define(LScene, LDefinition);
    LBefore := TNyxCodec.Encode(LDocument);
    LForm := NewNyxCanvasBoundaryEditor('navigation', LScene, LDocument);
    Check(Part(ncbpScope).Prop(NyxAttributeName(atText)) = TNyxText('Shared scene / Scène 🌙'),
      'public form preserves exact supplementary shared-scene text');
    Check(LForm.Node.Part(NyxPart('mode')) = Part(ncbpMode),
      'public form exposes its ordinary mode slot');
    Check(LForm.Node.Part(NyxPart('region')).Part(NyxPart('left')) = Part(ncbpLeft),
      'numeric fields remain reusable named parts in their region');
    LDraft := Default(TNyxCanvasBoundaryDraft);
    LDraft.Capture('navigation', LForm.Node);
    Check(not LDraft.Defined, 'untouched navigation fields create no retained proposal');
    LPresentation := DefaultNyxStudioPresentation;
    LDefaultPresentation := EncodeNyxStudioPresentation(LPresentation);
    Check(TNyxDataValue.ParseJSON(LDefaultPresentation).Field('version').AsInteger = 16,
      'untouched navigation retains the existing presentation version');
    Part(ncbpMode).Configure.Value('Camera center').Done;
    Check(CaptureNyxCanvasBoundaryEditor(Part(ncbpApply), LForm.Node, LDocument, LChange),
      'public form captures one typed scene proposal');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'capturing fields never mutates the document');
    LExpected := LDocument.Clone;
    LChange.Change.Apply(LExpected);
    Check(LExpected.Canvases.Definition(LScene).Camera.BoundaryMode = ncbCenter,
      'captured typed proposal changes the intended scene policy');
    Part(ncbpWidth).Configure.Value('-invalid').Done;
    LFailed := False;
    try
      CaptureNyxCanvasBoundaryEditor(Part(ncbpApply), LForm.Node, LDocument, LChange);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore) and
      (Part(ncbpWidth).Prop(NyxAttributeName(atValue)) = '-invalid'),
      'invalid field refuses while retaining the entire document and unfinished text');
    LDraft.Capture('navigation', LForm.Node);
    Check(LDraft.Defined, 'unfinished numeric text is independently retained');
    LReadDraft := TNyxCanvasBoundaryDraft.FromData(LDraft.ToData);
    Check(LReadDraft.ToData.ToJSON = LDraft.ToData.ToJSON,
      'copied navigation draft round trips exactly without numeric admission');
    { Each input obeys the existing 1 MiB state limit; their combined copied
      packet exceeds the navigation limit once its exact context is included. }
    Part(ncbpWidth).Configure.Value(TNyxText(StringOfChar('9',
      MaximumNyxCanvasDefinitionBytes - 1))).Done;
    Part(ncbpHeight).Configure.Value(TNyxText(StringOfChar('9',
      MaximumNyxCanvasDefinitionBytes - 1))).Done;
    LFailed := False;
    try
      LDraft.Capture('navigation', LForm.Node);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LDraft.ToData.ToJSON = LReadDraft.ToData.ToJSON) and
      (Length(Part(ncbpWidth).Prop(NyxAttributeName(atValue))) =
        MaximumNyxCanvasDefinitionBytes - 1) and
      (Length(Part(ncbpHeight).Prop(NyxAttributeName(atValue))) =
        MaximumNyxCanvasDefinitionBytes - 1),
      'oversized draft refuses before replacing retained data or touching input');
    Part(ncbpWidth).Configure.Value('-invalid').Done;
    Part(ncbpHeight).Configure.Value(LReadDraft.ToData.Field('values').Field('height').AsText).Done;
    LPacket := ReplaceField(LDraft.ToData, 'values', ReplaceField(
      LDraft.ToData.Field('values'), 'width', NyxData(12)));
    LFailed := False;
    try
      LReadDraft := TNyxCanvasBoundaryDraft.FromData(LPacket);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'draft admission refuses non-text numeric fields');
    LReadDraft := TNyxCanvasBoundaryDraft.FromData(LDraft.ToData);
    LPresentation.CanvasBoundaryDraft := LReadDraft;
    LPresentationRead := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPresentation));
    Check((LPresentationRead.CanvasBoundaryDraft.ToData.ToJSON = LDraft.ToData.ToJSON) and
      not LPresentationRead.CanvasPaletteDraft.Defined,
      'optional presentation retains navigation without manufacturing a palette draft');
    LPacket := ReplaceField(TNyxDataValue.ParseJSON(EncodeNyxStudioPresentation(LPresentation)),
      'version', NyxData(19));
    LFailed := False;
    try
      LPresentationRead := DecodeNyxStudioPresentation(LPacket.ToJSON);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'legacy presentation cannot silently admit the new draft field');
    LForm := NewNyxCanvasBoundaryEditor('navigation', LScene, LDocument);
    Check(LReadDraft.Restore(LForm.Node) and
      (Part(ncbpWidth).Prop(NyxAttributeName(atValue)) = '-invalid') and
      (Part(ncbpMode).Prop(NyxAttributeName(atValue)) = 'Camera center'),
      'rebuilt ordinary form restores exact unfinished fields');
    LReadDraft.Capture('navigation', nil);
    Check(LReadDraft.ToData.ToJSON = LDraft.ToData.ToJSON, 'hidden navigation parks its copied draft');
    LForm := NewNyxCanvasBoundaryEditor('navigation', NyxCanvasRef('Notes'), LDocument);
    Check(not LReadDraft.Restore(LForm.Node) and LReadDraft.Defined,
      'another scene never receives or consumes the parked proposal');
    LForm := NewNyxCanvasBoundaryEditor('navigation', LScene, LDocument);
    Check(LReadDraft.Restore(LForm.Node), 'return to the exact scene restores its parked proposal');
    LReadDraft.Clear;
    LPresentation.CanvasBoundaryDraft := LReadDraft;
    Check(EncodeNyxStudioPresentation(LPresentation) = LDefaultPresentation,
      'clearing the only optional draft restores exact prior presentation bytes');
    Part(ncbpMode).Configure.Value('Unbounded').Done;
    Check(CaptureNyxCanvasBoundaryEditor(Part(ncbpApply), LForm.Node, LDocument, LChange),
      'explicit unbounded policy ignores unused incomplete numeric fields');
    FreeAndNil(LExpected);
    LExpected := LDocument.Clone;
    LChange.Change.Apply(LExpected);
    Check(LExpected.Canvases.Definition(LScene).Camera.BoundaryMode = ncbUnbounded,
      'unbounded proposal deliberately clears saved policy');
    LDocument.Canvases.Define(LScene, LDefinition.WithGrid(NyxCanvasGrid(48)));
    LBefore := TNyxCodec.Encode(LDocument);
    LFailed := False;
    try
      CaptureNyxCanvasBoundaryEditor(Part(ncbpApply), LForm.Node, LDocument, LChange);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'stale scene refuses without discarding an independent definition update');
    LForm := NewNyxCanvasBoundaryEditor('navigation', LScene, LDocument);
    Check(not LDraft.Restore(LForm.Node) and LDraft.Defined,
      'unrelated definition changes park invalid text instead of applying it');
    LDocument.Canvases.Define(LScene, LDefinition);
    LForm := NewNyxCanvasBoundaryEditor('navigation', LScene, LDocument);
    Check(LDraft.Restore(LForm.Node), 'exact context restoration recovers the unfinished proposal');
    Part(ncbpWidth).Configure.Value('2400.5').Done;
    LDraft.Capture('navigation', LForm.Node);
    Check(CaptureNyxCanvasBoundaryEditor(Part(ncbpApply), LForm.Node, LDocument, LChange),
      'a corrected draft remains an ordinary typed Apply proposal');
    LChange.Change.Apply(LDocument);
    LForm := NewNyxCanvasBoundaryEditor('navigation', LScene, LDocument);
    Check(not LDraft.Restore(LForm.Node) and not LDraft.Defined,
      'exact successful publication consumes only the applied draft');
  finally
    LForm := nil;
    LExpected.Free;
    LDocument.Free;
  end;
end;

procedure RefuseDefinition(const AData: TNyxDataValue);
var
  LFailed: Boolean;
  LDefinition: INyxCanvasDefinition;
begin
  LFailed := False;
  try
    LDefinition := NyxCanvasDefinitionFromData(AData);
  except
    on Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'malformed or noncanonical persisted boundary refuses');
end;

{$ifndef PAS2JS}
{ Export immutable fixture bytes into an explicit owned output directory.
  Native constructors are compiled independently against the complete design. }
procedure ExportPair(const AStage: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;

  procedure Save(const AName, AText: TNyxText);
  var
    LBytes: TNyxBytes;
    LFile: TFileStream;
  begin
    LBytes := NyxEncodeUTF8(AText);
    LFile := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(LBytes) > 0 then
      begin
        LFile.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LFile.Free;
    end;
  end;

begin

  if ParamStr(1) = '' then
  begin
    Exit;
  end;
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AStage + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', APair.Source);
  Save('expected.nyx', APair.Design);
end;
{$endif}

procedure PersistenceAndSource;
var
  LDocument: TNyxDocument;
  LRead: TNyxDocument;
  LExpected: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LDefinition: INyxCanvasDefinition;
  LLegacy: INyxCanvasDefinition;
  LData: TNyxDataValue;
  LCamera: TNyxDataValue;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LSession: TNyxStudioSession;
  LChanged: TNyxText;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
begin
  LDocument := Workshop;
  LRead := nil;
  LExpected := nil;
  LWorkspace := nil;
  LSession := nil;
  LAgent := nil;
  try
    LDefinition := LDocument.Canvases.Definition(NyxCanvasRef('Notes'));
    LData := LDefinition.ToData;
    Check(LData.Field('version').AsInteger = 4, 'bounded camera selects explicit saved version four');
    Check(NyxCanvasDefinitionFromData(LData).ToData.ToJSON = LData.ToJSON,
      'bounded scene codec preserves every typed field exactly');
    LLegacy := LDefinition.WithCamera(LDefinition.Camera.WithoutBounds);
    Check((LLegacy.ToData.Field('version').AsInteger = 1) and
      (LLegacy.ToData.Field('camera').Count = 7), 'unbounded scenes retain their exact legacy camera shape');
    Check(NyxCanvasDefinitionFromData(LLegacy.ToData).ToData.ToJSON = LLegacy.ToData.ToJSON,
      'version-one data remains byte exact');
    LLegacy := NyxCanvasGraphLayout(LLegacy).Place(NyxFlowRef('Dormant graph'),
      NyxFlowStep('Dormant step'), NyxCanvasPoint(12.5, -7.25)).Done;
    Check(LLegacy.ToData.Field('version').AsInteger = 3, 'existing graph-layout intent retains version three');
    LLegacy := LLegacy.WithCamera(LDefinition.Camera);
    Check((LLegacy.ToData.Field('version').AsInteger = 4) and
      (NyxCanvasDefinitionFromData(LLegacy.ToData).ToData.ToJSON = LLegacy.ToData.ToJSON),
      'bounded scenes retain connection and graph-layout membership');
    RefuseDefinition(ReplaceField(LData, 'version', NyxData(1)));
    RefuseDefinition(ReplaceField(LData, 'camera', LDefinition.WithCamera(
      LDefinition.Camera.WithoutBounds).ToData.Field('camera')));
    LCamera := ReplaceField(LData.Field('camera'), 'centerX', NyxData(1.0e6));
    RefuseDefinition(ReplaceField(LData, 'camera', LCamera));
    LCamera := ReplaceField(LData.Field('camera'), 'boundary',
      ReplaceField(LData.Field('camera').Field('boundary'), 'mode', NyxData('suggested')));
    RefuseDefinition(ReplaceField(LData, 'camera', LCamera));
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    Check(Pos('.Within(NyxCanvasBounds(', LBefore.Source) > 0, 'crafted Pascal uses a typed fluent camera boundary');
    LRead := TNyxSourceWorkspace.PrepareDraft(LBefore.Source, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LBefore.Design, 'source admission reproduces the entire bounded document');
    FreeAndNil(LRead);
    FreeAndNil(LWorkspace);
    LExpected := LDocument.Clone;
    LExpected.Canvases.Define(NyxCanvasRef('Notes'), LDefinition.WithCamera(
      LDefinition.Camera.Within(LDefinition.Camera.NavigationBounds, ncbCenter)));
    LChanged := TNyxCodegen.Generate(LExpected);
    LSession := TNyxStudioSession.Create(LBefore);
    LSession.SetSourceDraft(LChanged);
    LSession.ApplySourceDraft;
    LAfter := LSession.ProjectSnapshot;
    Check((LAfter.Design = TNyxCodec.Encode(LExpected)) and (LAfter.Source = LChanged),
      'ordinary source Apply publishes the whole expected policy/document/Pascal pair');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one Undo restores the entire previous bounded pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores the entire accepted boundary change');
    LAgent := TNyxAgentSession.Create(LBefore);
    LReply := LAgent.Call('nyx_node', 'Scooty precision qualification', NyxObject([
      NyxField('id', NyxData('board')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('canvas', NyxData(True))]));
    Check(LReply.Field('canvas').Field('initialCamera').Field('boundary').Field('mode').AsText = 'viewport',
      'bounded semantic context exposes the actual initial camera policy');
    LAgent.Call('nyx_transaction', 'Scooty precision qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('camera-mode')),
      NyxField('operations', NyxArray([NyxDefineCanvas(NyxCanvasRef('Notes'),
        LExpected.Canvases.Definition(NyxCanvasRef('Notes'))).ToData]))]));
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    Check(LAfter.Design = TNyxCodec.Encode(LExpected), 'semantic definition replacement retains the complete expected design');
    LAgent.Call('nyx_history', 'Scooty precision qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('undo-policy')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LBefore),
      'semantic policy change uses ordinary exact paired Undo');
    LAgent.Call('nyx_history', 'Scooty precision qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('redo-policy')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter),
      'semantic Redo restores the exact paired definition and generated source');
    {$ifndef PAS2JS}
    ExportPair('original', LBefore);
    ExportPair('center', LAfter);
    {$endif}
  finally
    LAgent.Free;
    LSession.Free;
    LExpected.Free;
    LRead.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Geometry;
    Navigation;
    Editor;
    PersistenceAndSource;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-boundaries', 'passed');
    document.body.setAttribute('data-canvas-boundaries-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' canvas boundary checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-boundaries', 'failed');
      document.body.setAttribute('data-canvas-boundaries-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
