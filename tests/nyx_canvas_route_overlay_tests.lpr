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

program nyx_canvas_route_overlay_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.state, nyx.scheduler, nyx.canvas.scene, nyx.canvas.geometry, nyx.canvas.workspace,
  nyx.canvas.routes, nyx.flow.routes, nyx.flow.types, nyx.flow.declarations,
  nyx.flow.route.model, nyx.test.programs.logic
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas route overlay: ' + AReason);
  end;
  Inc(GChecks);
end;

function LiveRoute: INyxFlowRoutes;
begin
  Result := NewNyxFlowRoutes.WithRoute(NyxFlowRoute(NyxFlowRouteID('Live greeting'),
    NyxFlowRef('Greeting flow'), ntChange, nfcmAfterDispatch)
    .InputEventValue(ProgramLogicEndpoint('Name'), nftText)
    .OutputState(ProgramLogicEndpoint('Caption'), NyxTextState('caption')).Using(neUIQueue));
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LContext: TNyxDocument;
  LRoot: TNyxNode;
  LSource: TNyxText;
  LScene: INyxCanvasScene;
  LIdentity: INyxCanvasContentIdentity;
  LFirst: IInterface;
  LSecond: IInterface;
  LCurrent: IInterface;
  LControl: INyxNode;
  LOverlay: TNyxCanvasRouteOverrides;
  LClone: TNyxCanvasRouteOverrides;
  LChanges: TNyxCanvasRouteChanges;
  LPlacement: TNyxCanvasPlacement;
begin
  LDocument := ProgramToolCanvasDocument;
  LContext := nil;
  LOverlay := TNyxCanvasRouteOverrides.Create;
  LClone := nil;
  try
    LSource := TNyxCodec.Encode(LDocument);
    LScene := NewNyxCanvasScene.AddLayer(NyxCanvasLayer('Content'));
    Supports(LScene, INyxCanvasContentIdentity, LIdentity);
    Check(LIdentity <> nil, 'default scene supplies independent content lifetime identity');
    LPlacement := NyxCanvasPlacement(NyxCanvasLayer('Content'), NyxCanvasBounds(0, 0, 360, 240));
    LControl := RetainNyxControl(LDocument.Find('greeting-template').Clone);
    LScene.AddControl(NyxCanvasItem('First'), LPlacement, LControl);
    LControl := RetainNyxControl(LDocument.Find('greeting-template').Clone);
    LScene.AddControl(NyxCanvasItem('Second'), LPlacement, LControl);
    Check(LIdentity.TryContentIdentity(NyxCanvasItem('First'), LFirst) and
      LIdentity.TryContentIdentity(NyxCanvasItem('Second'), LSecond) and (LFirst <> LSecond),
      'independent instances of the same reusable template have distinct tokens');
    LScene.Place(NyxCanvasItem('First'), LPlacement);
    Check(LIdentity.TryContentIdentity(NyxCanvasItem('First'), LCurrent) and (LCurrent = LFirst),
      'geometry changes do not replace content lifetime');

    LOverlay.Define(LScene, NyxCanvasRoutes(NyxCanvasItem('First'), NyxControl('name-input'), LiveRoute));
    LContext := NewNyxCanvasRenderContext(LDocument, LScene.Control(NyxCanvasItem('First')).Node, LRoot);
    LOverlay.Configure(NyxCanvasItem('First'), LFirst, LContext, LRoot);
    LContext.Validate;
    Check(NyxControlFlowRoutes(LRoot.Find('name-input')).Count = 1,
      'an exact item context receives its typed overlay');
    FreeAndNil(LContext);
    LContext := NewNyxCanvasRenderContext(LDocument, LScene.Control(NyxCanvasItem('Second')).Node, LRoot);
    LOverlay.Configure(NyxCanvasItem('Second'), LSecond, LContext, LRoot);
    LContext.Validate;
    Check(NyxControlFlowRoutes(LRoot.Find('name-input')).Count = 0,
      'a second reusable instance retains its independent authored routes');
    FreeAndNil(LContext);
    Check(TNyxCodec.Encode(LDocument) = LSource, 'route overlays never modify authored source/defaults');
    LClone := LOverlay.Clone;
    LChanges := LClone.Capture(LScene);
    Check((Length(LChanges) = 1) and
      (LChanges[0].ToData.ToJSON = NyxCanvasRoutes(NyxCanvasItem('First'),
        NyxControl('name-input'), LiveRoute).ToData.ToJSON),
      'independent capture retains exact typed inert intent without exporting tokens');

    LScene.Remove(NyxCanvasItem('First'));
    LControl := RetainNyxControl(LDocument.Find('greeting-template').Clone);
    LScene.AddControl(NyxCanvasItem('First'), LPlacement, LControl);
    Check(LIdentity.TryContentIdentity(NyxCanvasItem('First'), LCurrent) and (LCurrent <> LFirst),
      'remove/recreate of the same name and template allocates a fresh lifetime');
    LContext := NewNyxCanvasRenderContext(LDocument, LScene.Control(NyxCanvasItem('First')).Node, LRoot);
    LClone.Configure(NyxCanvasItem('First'), LCurrent, LContext, LRoot);
    Check(NyxControlFlowRoutes(LRoot.Find('name-input')).Count = 0,
      'a replacement item cannot resurrect old runtime routes');
    FreeAndNil(LContext);
    Check(Length(LClone.Capture(LScene)) = 0, 'capture omits retired content authority');
    LClone.Define(LScene, LChanges[0]);
    LContext := NewNyxCanvasRenderContext(LDocument, LScene.Control(NyxCanvasItem('First')).Node, LRoot);
    LClone.Configure(NyxCanvasItem('First'), LCurrent, LContext, LRoot);
    Check((NyxControlFlowRoutes(LRoot.Find('name-input')).Count = 1) and
      (Length(LOverlay.Capture(LScene)) = 0),
      'explicit adoption into a new lifetime leaves the original snapshot independent');
    FreeAndNil(LContext);
    LScene := nil;
    LIdentity := nil;
    LControl := nil;
    LDocument.Free;
    LDocument := nil;
    Check(LChanges[0].Routes.Count = 1,
      'captured inert routes safely outlive their source, scene and controls');
  finally
    LContext.Free;
    LOverlay.Free;
    LClone.Free;
    LDocument.Free;
  end;
end;

{ Exercise retirement against the public active-entry budget: thousands of
  removed names must not permanently exhaust a long-lived workspace. A fresh
  candidate drops only dead tokens, while the former exact snapshot stays owned. }
procedure RetiredBudget;
var
  LScene: INyxCanvasScene;
  LControl: INyxNode;
  LOverlay: TNyxCanvasRouteOverrides;
  LNext: TNyxCanvasRouteOverrides;
  LItem: TNyxCanvasItemRef;
  LPlacement: TNyxCanvasPlacement;
  LIndex: Integer;
begin
  LScene := NewNyxCanvasScene.AddLayer(NyxCanvasLayer('Content'));
  LControl := NewNyxInput('input');
  LPlacement := NyxCanvasPlacement(NyxCanvasLayer('Content'), NyxCanvasBounds(0, 0, 200, 80));
  LOverlay := TNyxCanvasRouteOverrides.Create;
  LNext := nil;
  try
    for LIndex := 1 to 2048 do
    begin
      LItem := NyxCanvasItem('Temporary ' + TNyxText(IntToStr(LIndex)));
      LScene.AddControl(LItem, LPlacement, LControl);
      LOverlay.Define(LScene, NyxCanvasRoutes(LItem, NyxControl('input'), NewNyxFlowRoutes));
      LScene.Remove(LItem);
    end;
    LScene.AddControl(NyxCanvasItem('Current'), LPlacement, LControl);
    LNext := LOverlay.Clone(LScene);
    LNext.Define(LScene, NyxCanvasRoutes(NyxCanvasItem('Current'), NyxControl('input'), NewNyxFlowRoutes));
    Check((Length(LNext.Capture(LScene)) = 1) and (Length(LOverlay.Capture(LScene)) = 0),
      'retired items free the next publication budget without mutating the former overlay');
  finally
    LNext.Free;
    LOverlay.Free;
  end;
end;

begin
  try
    Run;
    RetiredBudget;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-route-overlay', 'passed');
    document.body.setAttribute('data-route-overlay-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' independent canvas route overlay checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-route-overlay', 'failed');
      document.body.setAttribute('data-route-overlay-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
