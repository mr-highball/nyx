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

program nyx_program_canvas_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.controls, nyx.model,
  nyx.resources, nyx.resource.sources, nyx.canvas.geometry, nyx.canvas.scene,
  nyx.programs.types, nyx.programs.contract, nyx.programs, nyx.programs.canvas
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

const
  { English layout behavior uses only typed host calls. Resource selection is an
    explicit caller-owned catalog; no URL or live application enters this script. }
  CArrange: TNyxText =
    'var Index: Integer; Item: TNyxCanvasItem; Spacing: Double;' + #10 +
    'function LeftFor(const Column: Integer): Double;' + #10 +
    'begin Result := Column * (140 + Spacing) end;' + #10 +
    'begin' + #10 +
    '  Spacing := ResourceNumber(ResourceAt(0));' + #10 +
    '  for Index := 0 to SceneCount() - 1 do' + #10 +
    '  begin' + #10 +
    '    Item := SceneItem(Index);' + #10 +
    '    MoveItem(Item, LeftFor(Index), 40);' + #10 +
    '    SizeItem(Item, 140, 80);' + #10 +
    '  end;' + #10 +
    '  SetCamera(140, 80, 1);' + #10 +
    'end.';

type
  { Creator control lifetime sentinel; snapshot metadata must not retain it
    after the source factory's scene and fluent temporaries leave scope. }
  TLifetimeControl = class(TNyxControl)
    destructor Destroy; override;
  end;

  {$ifndef PAS2JS}
  { Actual concurrent readers share immutable program/capture only. The worker
    owns invocation results and never receives a live scene, control or view. }
  TProgramReader = class(TThread)
  private
    FProgram: INyxProgram;
    FSnapshot: INyxCanvasProgramSnapshot;
  protected
    procedure Execute; override;
  public
    Passed: Boolean;
    Failure: TNyxText;
    constructor Create(const AProgram: INyxProgram; const ASnapshot: INyxCanvasProgramSnapshot);
  end;
  {$endif}

var
  GChecks: Integer;
  GControlsRetired: Integer;

destructor TLifetimeControl.Destroy;
begin
  Inc(GControlsRetired);
  inherited Destroy;
end;

function DetachedCapture: INyxCanvasProgramSnapshot;
var
  LScene: INyxCanvasScene;
  LRoot: INyxNode;
begin
  LScene := NewNyxCanvasScene;
  LScene.AddLayer(NyxCanvasLayer('Probe'));
  LRoot := TLifetimeControl.Create(nkLabel, 'lifetime-probe', ncoDescriptor);
  LScene.AddControl(NyxCanvasItem('Probe'), NyxCanvasPlacement(NyxCanvasLayer('Probe'),
    NyxCanvasBounds(0, 0, 80, 40)), LRoot);
  Result := CaptureNyxCanvasProgram(LScene, NyxCanvasCamera(200, 200, NyxCanvasPoint(0, 0)));
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Canvas program: ' + AMessage);
  end;
  Inc(GChecks);
end;

function Grants: TNyxProgramGrants;
begin
  Result := NyxProgramGrants.Allow(NyxProgramCapability(npcSceneRead))
    .Allow(NyxProgramCapability(npcSceneEdit))
    .Allow(NyxProgramCapability(npcCameraRead))
    .Allow(NyxProgramCapability(npcCameraEdit))
    .Allow(NyxProgramCapability(npcResourceRead));
end;

function MakeScene: INyxCanvasScene;
var
  LFirst: INyxLabel;
  LSecond: INyxInput;
  LHidden: INyxLabel;
begin
  Result := NewNyxCanvasScene;
  Result.AddLayer(NyxCanvasLayer('Controls'));
  Result.AddLayer(NyxCanvasLayer('Hidden'));
  Result.ConfigureLayer(NyxCanvasLayer('Hidden'), False, False);
  LFirst := NewNyxLabel('first-label').WithText('First');
  LSecond := NewNyxInput('second-input').WithText('Second');
  LHidden := NewNyxLabel('hidden-label').WithText('Hidden');
  Result.AddControl(NyxCanvasItem('First'),
    NyxCanvasPlacement(NyxCanvasLayer('Controls'), NyxCanvasBounds(-160, -40, 120, 60)), LFirst);
  Result.AddControl(NyxCanvasItem('Second'),
    NyxCanvasPlacement(NyxCanvasLayer('Controls'), NyxCanvasBounds(0, -40, 120, 60)), LSecond);
  Result.AddControl(NyxCanvasItem('Hidden'),
    NyxCanvasPlacement(NyxCanvasLayer('Hidden'), NyxCanvasBounds(160, -40, 120, 60)), LHidden);
end;

function Materialized: INyxResources;
var
  LBytes: TNyxBytes;
begin
  Result := NewNyxResources;
  Result.Define(NyxResourceRef('spacing'), NyxJSONResource('32'));
  Result.Define(NyxResourceRef('greeting'), NyxTextResource(TNyxText('Hello 😀') + #0));
  SetLength(LBytes, 2);
  LBytes[0] := 0;
  LBytes[1] := 255;
  Result.Define(NyxResourceRef('payload'), NyxBinaryResource(LBytes));
end;

function Query(const ASnapshot: INyxCanvasProgramSnapshot; ARoutine: TNyxCanvasProgramRoutine;
  const AArguments: array of INyxProgramValue): INyxProgramValue;
begin
  Result := ASnapshot.Query(NyxCanvasProgramSymbol(ARoutine), NyxProgramArguments(AArguments));
end;

procedure RefusedPrepare(const ASnapshot: INyxCanvasProgramSnapshot;
  const AResult: INyxProgramResult; const AScene: INyxCanvasScene;
  AKind: TNyxProgramDiagnosticKind; AMaximum: Integer = 1024);
var
  LRevision: Integer;
  LFailed: Boolean;
begin
  LRevision := AScene.Revision;
  LFailed := False;
  try
    ASnapshot.Prepare(AResult, AScene, AMaximum);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = AKind;
    end;
  end;
  Check(LFailed and (AScene.Revision = LRevision), 'Rejected result preserves the complete scene revision');
end;

{$ifndef PAS2JS}
constructor TProgramReader.Create(const AProgram: INyxProgram; const ASnapshot: INyxCanvasProgramSnapshot);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FProgram := AProgram;
  FSnapshot := ASnapshot;
end;

procedure TProgramReader.Execute;
var
  LIndex: Integer;
  LResult: INyxProgramResult;
begin
  try
    for LIndex := 1 to 40 do
    begin
      LResult := FProgram.Run(NyxProgramValues, NyxProgramRun.Grants(Grants).Queries(FSnapshot));

      if (LResult.ProposalCount <> 7) or (LResult.Outputs.Count <> 0) then
      begin
        raise Exception.Create('Concurrent result changed');
      end;
    end;
    Passed := True;
  except
    on E: Exception do
    begin
      Failure := E.Message;
    end;
  end;
end;
{$endif}

procedure Run;
var
  LScene: INyxCanvasScene;
  LOtherScene: INyxCanvasScene;
  LLayouts: INyxCanvasSceneLayouts;
  LSnapshot: INyxCanvasProgramSnapshot;
  LForeign: INyxCanvasProgramSnapshot;
  LOld: INyxCanvasSnapshot;
  LResources: INyxResources;
  LProgram: INyxProgram;
  LResult: INyxProgramResult;
  LBadResult: INyxProgramResult;
  LPrepared: INyxCanvasProgramLayout;
  LPlan: INyxCanvasLayoutPlan;
  LItem: INyxProgramValue;
  LResource: INyxProgramValue;
  LRoot: INyxNode;
  LRevision: Integer;
  LFailed: Boolean;
  LContract: TNyxProgramContract;
  LBytes: TNyxBytes;
  LBeforeRetirement: Integer;
  {$ifndef PAS2JS}
  LFirstReader: TProgramReader;
  LSecondReader: TProgramReader;
  {$endif}
begin
  LScene := MakeScene;
  LOtherScene := MakeScene;
  LResources := Materialized;
  LSnapshot := CaptureNyxCanvasProgram(LScene,
    NyxCanvasCamera(640, 480, NyxCanvasPoint(0, 0)), LResources);
  Check(Supports(LScene, INyxCanvasSceneLayouts, LLayouts), 'Default scenes expose optional layout admission');
  LItem := Query(LSnapshot, ncprItem, [NyxProgramValue(0)]);
  Check(LItem.AsHandle = Query(LSnapshot, ncprItem, [NyxProgramValue(0)]).AsHandle,
    'Repeated item queries retain stable capability identity within the capture');
  LResource := Query(LSnapshot, ncprResource, [NyxProgramValue(1)]);
  Check(Query(LSnapshot, ncprResourceText, [LResource]).AsText = TNyxText('Hello 😀') + #0,
    'Materialized resources retain exact supplementary and NUL text');
  Check(Query(LSnapshot, ncprResourceName, [LResource]).AsText = 'greeting',
    'Resource capability exposes its declared name without a URL or host object');
  LResource := Query(LSnapshot, ncprResource, [NyxProgramValue(2)]);
  LBytes := Query(LSnapshot, ncprResourceBytes, [LResource]).AsBytes;
  LBytes[0] := 99;
  Check(Query(LSnapshot, ncprResourceBytes, [LResource]).AsBytes[0] = 0,
    'Resource bytes retain detached getter storage');
  LResources.Define(NyxResourceRef('spacing'), NyxJSONResource('64'));
  LResource := Query(LSnapshot, ncprResource, [NyxProgramValue(0)]);
  Check(Query(LSnapshot, ncprResourceNumber, [LResource]).AsNumber = 32,
    'Capture values remain independent of subsequent registry replacement');

  LProgram := CompileNyxProgram(CArrange, LSnapshot.Contract);
  Check(LProgram.CapabilityCount = 4, 'The complete source declares read/edit/resource/camera requirements');
  LFailed := False;
  try
    LProgram.Run(NyxProgramValues, NyxProgramRun.Queries(LSnapshot));
  except
    on E: ENyxProgram do
    begin
      LFailed := (E.Diagnostic.Kind = npdCapability) and E.Diagnostic.Span.Defined;
    end;
  end;
  Check(LFailed, 'Absent grants refuse at the exact source before host access');
  LRevision := LScene.Revision;
  LResult := LProgram.Run(NyxProgramValues, NyxProgramRun.Grants(Grants).Queries(LSnapshot));
  Check((LResult.ProposalCount = 7) and (LScene.Revision = LRevision),
    'Nested layout routines stage all item and camera changes without a live effect');
  {$ifndef PAS2JS}
  LFirstReader := TProgramReader.Create(LProgram, LSnapshot);
  LSecondReader := TProgramReader.Create(LProgram, LSnapshot);
  try
    LFirstReader.Start;
    LSecondReader.Start;
    LFirstReader.WaitFor;
    LSecondReader.WaitFor;
    Check(LFirstReader.Passed and LSecondReader.Passed,
      'Two actual workers own independent journals over the same program and capture');
  finally
    LFirstReader.Free;
    LSecondReader.Free;
  end;
  {$endif}
  RefusedPrepare(LSnapshot, LResult, LOtherScene, npdStale);
  RefusedPrepare(LSnapshot, LResult, LScene, npdLimit, 1);
  LOld := LScene.Snapshot;
  LRoot := LScene.Control(NyxCanvasItem('Second'));
  LPrepared := LSnapshot.Prepare(LResult, LScene);
  Check((LScene.Revision = LRevision) and (LPrepared.Plan.Preview.Revision = LRevision + 1),
    'Complete placement/spatial preview remains invisible until explicit publication');
  Check((LPrepared.Plan.Preview.Items[1].Placement.Bounds.Left = 172) and
    (LPrepared.Plan.Preview.Items[1].Placement.Bounds.Width = 140) and
    (LPrepared.Camera.WorldCenter.X = 140), 'Repeated move/size calls merge chronologically into one candidate');
  LPrepared.Plan.Commit;
  Check((LScene.Revision = LRevision + 1) and LPrepared.Plan.Committed,
    'One complete prepared layout advances the scene revision once');
  Check((LScene.Control(NyxCanvasItem('Second')).Node = LRoot.Node) and
    (LScene.Snapshot.Items[1].Placement.Bounds.Left = 172),
    'Publication retains original managed controls and updates their geometry');
  Check(LLayouts.PlacementFor(NyxCanvasItem('Hidden')).IsVisible and
    not LScene.Snapshot.Items[2].Placement.IsVisible,
    'Geometry edits preserve intrinsic item visibility beneath a hidden layer');
  Check((LOld.Items[1].Placement.Bounds.Left = 0) and
    (Query(LSnapshot, ncprX, [LItem]).AsNumber = -160),
    'Old spatial and script snapshots remain immutable after publication');
  LFailed := False;
  try
    LPrepared.Plan.Commit;
  except
    on E: ENyxCanvasScene do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LScene.Revision = LRevision + 1), 'A layout cannot publish twice');
  RefusedPrepare(LSnapshot, LResult, LScene, npdStale);

  LSnapshot := CaptureNyxCanvasProgram(LScene, LPrepared.Camera, Materialized);
  LForeign := CaptureNyxCanvasProgram(LScene, LPrepared.Camera);
  LContract := LSnapshot.Contract.Input(NyxProgramSymbol('Target'),
    NyxProgramHandleType(NyxProgramSymbol('TNyxCanvasItem')));
  LProgram := CompileNyxProgram('begin MoveItem(Target, 5, 6) end.', LContract);
  LBadResult := LProgram.Run(NyxProgramValues.WithValue(NyxProgramSymbol('Target'),
    Query(LForeign, ncprItem, [NyxProgramValue(0)])), NyxProgramRun.Grants(Grants));
  RefusedPrepare(LSnapshot, LBadResult, LScene, npdCapability);
  LProgram := CompileNyxProgram('begin MoveItem(SceneItem(0), 2, 3); ' +
    'SizeItem(SceneItem(1), -1, 20) end.', LSnapshot.Contract);
  LBadResult := LProgram.Run(NyxProgramValues, NyxProgramRun.Grants(Grants).Queries(LSnapshot));
  LRevision := LScene.Revision;
  LFailed := False;
  try
    LSnapshot.Prepare(LBadResult, LScene);
  except
    on E: ENyxProgram do
    begin
      LFailed := (E.Diagnostic.Kind = npdType) and E.Diagnostic.Span.Defined;
    end;
  end;
  Check(LFailed and (LScene.Revision = LRevision) and
    (LScene.Snapshot.Items[0].Placement.Bounds.Left = 0),
    'A later invalid size refuses the entire staged layout with exact proposal provenance');
  LProgram := CompileNyxProgram('begin MoveItem(SceneItem(0), 2, 3); ' +
    'SizeItem(SceneItem(1), ResourceNumber(ResourceAt(1)), 20) end.', LSnapshot.Contract);
  LFailed := False;
  try
    LProgram.Run(NyxProgramValues, NyxProgramRun.Grants(Grants).Queries(LSnapshot));
  except
    on E: ENyxProgram do
    begin
      LFailed := (E.Diagnostic.Kind = npdType) and E.Diagnostic.Span.Defined;
    end;
  end;
  Check(LFailed and (LScene.Revision = LRevision), 'Resource family errors refuse at the exact script call');

  LContract := LSnapshot.Contract.Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText));
  LProgram := CompileNyxProgram('begin Caption := ''Do not drop this'' end.', LContract);
  RefusedPrepare(LSnapshot, LProgram.Run(NyxProgramValues), LScene, npdType);
  LContract := LSnapshot.Contract.WithRoutine(NyxProgramPropose(
    NyxProgramSymbol('Teleport'), NyxProgramCapability(npcSceneEdit)));
  LProgram := CompileNyxProgram('begin Teleport() end.', LContract);
  RefusedPrepare(LSnapshot, LProgram.Run(NyxProgramValues, NyxProgramRun.Grants(Grants)), LScene, npdName);
  LContract := NyxProgramContract.WithRoutine(NyxProgramPropose(
    NyxCanvasProgramSymbol(ncprMove), NyxProgramCapability(npcResourceRead))
      .Argument(NyxProgramSymbol('Item'), NyxProgramHandleType(NyxProgramSymbol('TNyxCanvasItem')))
      .Argument(NyxProgramSymbol('X'), NyxProgramType(npkNumber))
      .Argument(NyxProgramSymbol('Y'), NyxProgramType(npkNumber)))
    .Input(NyxProgramSymbol('Target'), NyxProgramHandleType(NyxProgramSymbol('TNyxCanvasItem')));
  LProgram := CompileNyxProgram('begin MoveItem(Target, 1, 2) end.', LContract);
  LBadResult := LProgram.Run(NyxProgramValues.WithValue(NyxProgramSymbol('Target'),
    Query(LSnapshot, ncprItem, [NyxProgramValue(0)])), NyxProgramRun.Grants(Grants));
  RefusedPrepare(LSnapshot, LBadResult, LScene, npdType);

  LPlan := LLayouts.PrepareLayout([], LScene.Revision);
  LScene.Place(NyxCanvasItem('First'), LLayouts.PlacementFor(NyxCanvasItem('First')));
  LFailed := False;
  try
    LPlan.Commit;
  except
    on E: ENyxCanvasScene do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and not LPlan.Committed, 'An intervening ordinary edit revokes prepared layout authority');
  LFailed := False;
  LRevision := LScene.Revision;
  try
    LLayouts.PrepareLayout([
      NyxCanvasPlace(NyxCanvasItem('First'), LLayouts.PlacementFor(NyxCanvasItem('First'))),
      NyxCanvasPlace(NyxCanvasItem('First'), LLayouts.PlacementFor(NyxCanvasItem('First')))], LRevision);
  except
    on E: ENyxCanvasScene do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LScene.Revision = LRevision), 'Duplicate batch entries refuse before publication');
  LFailed := False;
  try
    CaptureNyxCanvasProgram(LScene, LPrepared.Camera, Materialized, 1);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdLimit;
    end;
  end;
  Check(LFailed, 'Resource capture obeys a caller-selected cumulative byte budget');
  LResources := NewNyxResources.Define(NyxResourceRef('remote'),
    NyxHostedResource(nrkJSON, NyxResourceURL('https://example.invalid/layout.json')));
  LFailed := False;
  try
    CaptureNyxCanvasProgram(LScene, LPrepared.Camera, LResources);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdCapability;
    end;
  end;
  Check(LFailed, 'Hosted definitions never trigger hidden network work during interpreter capture');

  LPlan := nil;
  LPrepared := nil;
  LRoot := nil;
  LLayouts := nil;
  LScene := nil;
  LOtherScene := nil;
  LBeforeRetirement := GControlsRetired;
  LSnapshot := DetachedCapture;
  Check((GControlsRetired = LBeforeRetirement + 1) and
    (Query(LSnapshot, ncprCount, []).AsInteger = 1),
    'A capture remains queryable after the source scene actually destroys its creator control');
end;

begin
  GChecks := 0;
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-program-canvas', 'passed');
    document.body.setAttribute('data-program-canvas-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed canvas program checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-program-canvas', 'failed');
      document.body.setAttribute('data-program-canvas-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
