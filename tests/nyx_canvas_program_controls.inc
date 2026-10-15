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

{ This fixture extends the unchanged workspace journey only when explicitly
  compiled with NYX_CANVAS_PROGRAM_TESTS. The production library owns all query,
  journal and scene/camera admission behavior. }

const
  CCanvasLayoutSource: TNyxText =
    'var Index: Integer; Item: TNyxCanvasItem; Spacing: Double;' + #10 +
    'procedure Slide(const Control: TNyxCanvasItem);' + #10 +
    'begin MoveItem(Control, ItemX(Control) + Spacing, ItemY(Control)) end;' + #10 +
    'begin' + #10 +
    '  Spacing := ResourceNumber(ResourceAt(0));' + #10 +
    '  for Index := 0 to SceneCount() - 1 do' + #10 +
    '  begin' + #10 +
    '    Item := SceneItem(Index);' + #10 +
    '    if ItemWidth(Item) > 0 then Slide(Item);' + #10 +
    '  end;' + #10 +
    '  SetCamera(CameraX() + Spacing / 2, CameraY(), CameraZoom());' + #10 +
    'end.';

type
  { Actual embedded-button callback borrows only the harness globals. It owns no
    view/scene, so event registration cannot create a renderer ownership cycle. }
  TCanvasLayoutCallback = class(TNyxEventCallback)
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Closed qualification actions at the final staging revalidation point. }
  TLayoutInterruption = (liCancel, liEditScene, liDisconnect);

  { One bounded reentrant callback after all physical allocations have staged.
    These hostile fixture actions are UI-only, not normal scope behavior. No
    application/view/scene interface is retained; the harness outlives this token. }
  TLayoutScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Action: TLayoutInterruption;
    Entered: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GCanvasProgram: INyxProgram;
  GCanvasCapture: INyxCanvasProgramSnapshot;
  GCanvasResult: INyxProgramResult;
  GCanvasExpected: Integer;
  GCanvasCalls: Integer;
  GCanvasApplied: Boolean;

function CanvasProgramGrants: TNyxProgramGrants;
begin
  Result := NyxProgramGrants.Allow(NyxProgramCapability(npcSceneRead))
    .Allow(NyxProgramCapability(npcSceneEdit))
    .Allow(NyxProgramCapability(npcCameraRead))
    .Allow(NyxProgramCapability(npcCameraEdit))
    .Allow(NyxProgramCapability(npcResourceRead));
end;

procedure TCanvasLayoutCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(GCanvasCalls);
  GCanvasProgram := CompileNyxProgram(CCanvasLayoutSource, GCanvasCapture.Contract);
  GCanvasResult := GCanvasProgram.Run(NyxProgramValues,
    NyxProgramRun.Grants(CanvasProgramGrants).Queries(GCanvasCapture));
  GCanvasApplied := ApplyNyxCanvasProgram(GView, GCanvasCapture, GCanvasResult, GCanvasExpected);
end;

procedure TLayoutScope.Cancel;
begin
  Action := liCancel;
end;

function TLayoutScope.GetCancelled: Boolean;
var
  LLayouts: INyxCanvasSceneLayouts;
begin
  Result := Action = liCancel;

  if Entered then
  begin
    Exit;
  end;
  Entered := True;
  case Action of
    liCancel: ;
    liEditScene:
    begin
      Supports(GScene, INyxCanvasSceneLayouts, LLayouts);
      GScene.Place(GReply, LLayouts.PlacementFor(GReply));
    end;
    liDisconnect: GView.Disconnect;
  end;
end;

procedure ProgramLayouts;
var
  LView: INyxCanvasLayoutView;
  LLayouts: INyxCanvasSceneLayouts;
  LResources: INyxResources;
  LSubscription: INyxEventSubscription;
  LPlacement: TNyxCanvasPlacement;
  LCamera: TNyxCanvasCamera;
  LOldScene: Integer;
  LOldPresentation: Integer;
  LFailed: Boolean;
  LResult: INyxProgramResult;
  LPrepared: INyxCanvasProgramLayout;
  LProgram: INyxProgram;
  LScope: TLayoutScope;
  LScopeLease: INyxCancellationScope;
  LAction: TLayoutInterruption;
  {$ifdef PAS2JS}
  LInput: TJSHTMLTextAreaElement;
  LButton: TJSHTMLButtonElement;
  {$else}
  LInput: TMemo;
  LButton: TNyxLCLButton;
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
begin
  Check(Supports(GView, INyxCanvasLayoutView, LView) and
    Supports(GScene, INyxCanvasSceneLayouts, LLayouts), 'Actual workspace exposes optional atomic layout interfaces');
  LResources := NewNyxResources.Define(NyxResourceRef('spacing'), NyxJSONResource('24'));
  { The preceding regression journey deliberately replaced/retired an item.
    Capture the currently mounted face, not its historical GInput pointer. }
  {$ifdef PAS2JS}
  LInput := TJSHTMLTextAreaElement(GView.InputFor(GReply, NyxControl('reply-memo')));
  {$else}
  LInput := TMemo(GView.InputFor(GReply, NyxControl('reply-memo')));
  {$endif}
  GInput := LInput;
  LPlacement := LLayouts.PlacementFor(GReply);
  LCamera := GView.Camera;
  LOldScene := GScene.Revision;
  GCanvasExpected := LView.PresentationRevision;
  GCanvasCapture := CaptureNyxCanvasProgram(GScene, LCamera, LResources);
  GCanvasProgram := nil;
  GCanvasCalls := 0;
  GCanvasApplied := False;
  {$ifdef PAS2JS}
  LButton := TJSHTMLButtonElement(GView.RendererFor(GReply).ElementFor('send-button'));
  {$else}
  LButton := TNyxLCLButton(GView.RendererFor(GReply).ControlFor('send-button'));
  {$endif}
  LSubscription := GView.EventsFor(GReply).On(NyxControlEvents('send-button'), ntClick)
    .Subscribe(TCanvasLayoutCallback.Create);
  try
    LButton.Click;
    Check((GCanvasCalls = 1) and GCanvasApplied, 'Actual embedded Nyx button compiles/runs typed layout behavior');
    Check((GScene.Revision = LOldScene + 1) and
      (LView.PresentationRevision = GCanvasExpected + 1),
      'Grouped item/camera publication advances each owner once');
    Check((LLayouts.PlacementFor(GReply).Bounds.Left = LPlacement.Bounds.Left + 24) and
      (GView.Camera.WorldCenter.X = LCamera.WorldCenter.X + 12),
      'A materialized JSON resource drives visible control positions and camera');
    Check(GView.InputFor(GReply, NyxControl('reply-memo')) = LInput,
      'Script publication retains the same actual editable control');
    Check(GState.GetValue(NyxTextState('reply')) = 'A live edited reply',
      'Grouped geometry publication retains independent runtime editing state');
    {$ifdef PAS2JS}
    Check(LInput.value = 'A live edited reply', 'Browser memo retains its actual edited text after script layout');
    {$else}
    Check(TNyxText(LInput.Text) = 'A live edited reply', 'Native memo retains its actual edited text after script layout');
    {$endif}
  finally
    LSubscription.Cancel;
  end;

  GCanvasCapture := CaptureNyxCanvasProgram(GScene, GView.Camera, LResources);
  LResult := GCanvasProgram.Run(NyxProgramValues,
    NyxProgramRun.Grants(CanvasProgramGrants).Queries(GCanvasCapture));
  LOldScene := GScene.Revision;
  LOldPresentation := LView.PresentationRevision;
  GView.PanBy(1, 0);
  LCamera := GView.Camera;
  LFailed := False;
  try
    ApplyNyxCanvasProgram(GView, GCanvasCapture, LResult, LOldPresentation);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdStale;
    end;
  end;
  Check(LFailed and (GScene.Revision = LOldScene) and
    (GView.Camera.WorldCenter.X = LCamera.WorldCenter.X), 'Intervening camera input preserves the newer presentation');

  GCanvasCapture := CaptureNyxCanvasProgram(GScene, GView.Camera, LResources);
  LOldPresentation := LView.PresentationRevision;
  LProgram := CompileNyxProgram('begin SizeItem(SceneItem(0), 1000000000, 200); ' +
    'MoveItem(SceneItem(0), CameraX(), CameraY()); ' +
    'SetCamera(CameraX() + 20, CameraY(), CameraZoom()) end.', GCanvasCapture.Contract);
  LResult := LProgram.Run(NyxProgramValues,
    NyxProgramRun.Grants(CanvasProgramGrants).Queries(GCanvasCapture));
  LPlacement := LLayouts.PlacementFor(GReply);
  LFailed := False;
  try
    ApplyNyxCanvasProgram(GView, GCanvasCapture, LResult, LOldPresentation);
  except
    on E: ENyxCanvasScene do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GScene.Revision = LOldScene) and
    (GView.Camera.WorldCenter.X = LCamera.WorldCenter.X) and
    (LView.PresentationRevision = LOldPresentation),
    'Target allocation refusal retains the complete scene, camera and presentation');
  Check(GView.InputFor(GReply, NyxControl('reply-memo')) = LInput,
    'Failed target staging retains the exact accepted editor face');

  for LAction := Low(TLayoutInterruption) to High(TLayoutInterruption) do
  begin
    GCanvasCapture := CaptureNyxCanvasProgram(GScene, GView.Camera, LResources);
    LResult := GCanvasProgram.Run(NyxProgramValues,
      NyxProgramRun.Grants(CanvasProgramGrants).Queries(GCanvasCapture));
    LOldScene := GScene.Revision;
    LOldPresentation := LView.PresentationRevision;
    LCamera := GView.Camera;
    LScope := TLayoutScope.Create;
    LScopeLease := LScope;
    LScope.Action := LAction;
    LFailed := False;
    GCanvasApplied := False;
    try
      GCanvasApplied := ApplyNyxCanvasProgram(GView, GCanvasCapture, LResult,
        LOldPresentation, LScopeLease);
    except
      on E: ENyxCanvasScene do
      begin
        LFailed := LAction = liEditScene;
      end;
    end;
    Check(LScope.Entered and not GCanvasApplied,
      'Cancellation/reentrant scene edit/disconnection cannot publish a staged script result');

    if LAction = liEditScene then
    begin
      Check(LFailed and (GScene.Revision = LOldScene + 1),
        'Only the independent reentrant no-op edit advances scene revision');
    end
    else
    begin
      Check(GScene.Revision = LOldScene, 'Cancelled or retired delivery leaves scene geometry exact');
    end;

    if LAction = liDisconnect then
    begin
      LPrepared := GCanvasCapture.Prepare(LResult, GScene);
      Check(not LView.ApplyLayout(LPrepared.Plan, LPrepared.Camera, LOldPresentation) and
        not LPrepared.Plan.Committed, 'Direct delivery to a retired layout view performs no publication');
    end;

    if LAction <> liDisconnect then
    begin
      Check((GView.Camera.WorldCenter.X = LCamera.WorldCenter.X) and
        (LView.PresentationRevision = LOldPresentation),
        'Failed final ownership checks preserve the previous complete camera/presentation');
    end;
    LScopeLease := nil;
  end;

  Check(TNyxCodec.Encode(GDocument) = GBefore, 'Script layout never edits authored document/default/source bytes');
  {$ifdef PAS2JS}
  GView := MountNyxBrowserCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  {$else}
  GView := MountNyxLCLCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  Application.ProcessMessages;
  {$endif}
  GView.SetCamera(LCamera);
  {$ifdef PAS2JS}
  GInput := TJSHTMLTextAreaElement(GView.InputFor(GReply, NyxControl('reply-memo')));
  {$else}
  GInput := TMemo(GView.InputFor(GReply, NyxControl('reply-memo')));

  if ParamCount > 0 then
  begin
    LBitmap := GHost.GetFormImage;
    LPNG := TPortableNetworkGraphic.Create;
    try
      LPNG.Assign(LBitmap);
      LPNG.SaveToFile(ParamStr(1));
    finally
      LPNG.Free;
      LBitmap.Free;
    end;
  end;
  {$endif}
  GCanvasCapture := nil;
  GCanvasResult := nil;
  GCanvasProgram := nil;
end;
