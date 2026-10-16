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
program nyx_canvas_content_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.canvas.geometry,
  nyx.canvas.scene, nyx.publication, nyx.publication.types
  {$ifdef PAS2JS}, Web{$endif};

type
  { Distinct actual managed implementation survives metadata cloning and scene
    retirement. Its destruction count proves that identity tokens own no root. }
  TSpecialButton = class(TNyxButton)
  public
    destructor Destroy; override;
  end;

  { A separate model participant joins the scene through the normal coordinator.
    Notify verifies complete publication; deliberate Validate refusal installs
    neither owner. Its scene lease is revoked at ordinary participant retirement. }
  TCompanion = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    Scene: INyxCanvasScene;
    Revision: Integer;
    Reject: Boolean;
    Installed: Boolean;
    Notified: Boolean;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Deliberately reentrant creator input. Its Control getter changes the scene
    before returning a retained unowned root. Admission must reject its captured
    expected revision without adopting that new content or undoing caller work. }
  TReentrantChange = class(TInterfacedObject, INyxCanvasContentChange)
  public
    Scene: INyxCanvasScene;
    Control: INyxNode;
    function GetKind: TNyxCanvasContentChangeKind;
    function GetItem: TNyxCanvasItemRef;
    function GetPlacement: TNyxCanvasPlacement;
    function GetControl: INyxNode;
  end;

var
  GChecks: Integer;
  GDestroyed: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Canvas content: ' + AReason);
  end;
  Inc(GChecks);
end;

destructor TSpecialButton.Destroy;
begin
  Inc(GDestroyed);
  inherited Destroy;
end;

procedure TCompanion.Validate;
begin

  if Reject then
  begin
    raise ENyxCanvasScene.Create('Deliberate independent model refusal');
  end;
end;

procedure TCompanion.CheckReady;
begin
  Check((Scene <> nil) and (Scene.Revision = Revision), 'companion sees its exact pre-install frame');
end;

procedure TCompanion.Install;
begin
  Installed := True;
end;

procedure TCompanion.Notify;
begin
  Check(Installed and (Scene.Count = 3) and (Scene.Revision = Revision + 1),
    'notification observes both installed model and complete content membership');
  Notified := True;
end;

procedure TCompanion.Retire;
begin
  Scene := nil;
end;

function TReentrantChange.GetKind: TNyxCanvasContentChangeKind;
begin
  Result := ncccAdd;
end;

function TReentrantChange.GetItem: TNyxCanvasItemRef;
begin
  Result := NyxCanvasItem('Foreign');
end;

function TReentrantChange.GetPlacement: TNyxCanvasPlacement;
begin
  Result := NyxCanvasPlacement(NyxCanvasLayer('Content'), NyxCanvasBounds(10, 20, 120, 60));
end;

function TReentrantChange.GetControl: INyxNode;
begin
  Scene.Place(NyxCanvasItem('First'), GetPlacement);
  Result := Control;
end;

procedure Run;
var
  LScene: INyxCanvasScene;
  LEditing: INyxCanvasSceneContent;
  LIdentity: INyxCanvasContentIdentity;
  LPlan: INyxCanvasLayoutPlan;
  LPreview: INyxCanvasContentPlan;
  LPublication: INyxPreparedPublication;
  LSnapshot: INyxCanvasSnapshot;
  LButton: INyxButton;
  LOriginal: INyxMemo;
  LReplacement: INyxLabel;
  LNew: INyxInput;
  LFirstIdentity: IInterface;
  LSecondIdentity: IInterface;
  LAfterIdentity: IInterface;
  LRevision: Integer;
  LRefused: Boolean;
  LCompanion: TCompanion;
  LCompanionLease: INyxPreparedPublication;
  LChange: TReentrantChange;
  LForeign: INyxCanvasContentChange;
  LPlacement: TNyxCanvasPlacement;

  function Proposal: INyxCanvasLayoutPlan;
  begin
    Result := LEditing.PrepareContent([
      NyxCanvasPlaceControl(NyxCanvasItem('First'),
        NyxCanvasPlacement(NyxCanvasLayer('Content'), NyxCanvasBounds(-33.5, 12.25, 130, 60))),
      NyxCanvasReplaceControl(NyxCanvasItem('Second'), LPlacement, LReplacement),
      NyxCanvasAddControl(NyxCanvasItem('Third'), LPlacement, LNew)], LScene.Revision);
  end;

begin
  LScene := NewNyxCanvasScene;
  LScene.AddLayer(NyxCanvasLayer('Content'));
  LButton := TSpecialButton.Create('special-button');
  LOriginal := NewNyxMemo('original-memo').WithText('Retained editor text');
  LReplacement := NewNyxLabel('replacement-label').WithText('Replacement');
  LNew := NewNyxInput('new-input').WithText('New field');
  LPlacement := NyxCanvasPlacement(NyxCanvasLayer('Content'), NyxCanvasBounds(0, 0, 160, 80));
  LScene.AddControl(NyxCanvasItem('First'), LPlacement, LButton);
  LScene.AddControl(NyxCanvasItem('Second'), LPlacement, LOriginal);
  Check(Supports(LScene, INyxCanvasSceneContent, LEditing) and
    Supports(LScene, INyxCanvasContentIdentity, LIdentity), 'scene exposes optional prepared membership and identity');
  LIdentity.TryContentIdentity(NyxCanvasItem('First'), LFirstIdentity);
  LIdentity.TryContentIdentity(NyxCanvasItem('Second'), LSecondIdentity);
  LRevision := LScene.Revision;
  LPlan := Proposal;
  Check(Supports(LPlan, INyxCanvasContentPlan, LPreview) and
    Supports(LPlan, INyxPreparedPublication, LPublication), 'plan stages roots through the existing publication contract');
  LSnapshot := LPlan.Preview;
  Check((LScene.Revision = LRevision) and (LScene.Count = 2) and (LSnapshot.Count = 3),
    'prepared candidate membership is independent of accepted scene');
  Check((LPreview.PreviewControl(NyxCanvasItem('First')).Node = LButton.Node) and
    (LPreview.PreviewControl(NyxCanvasItem('Third')).Node = LNew.Node), 'preview retains actual control implementations');
  Check((LNew.Node.Parent <> nil) and (LScene.Control(NyxCanvasItem('Second')).Node = LOriginal.Node),
    'private candidate adopts only new roots while original content stays accepted');
  LRefused := False;
  try
    LScene.Place(NyxCanvasItem('First'), LPlacement);
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LScene.Revision = LRevision), 'staged ownership reserves ordinary geometry writes');
  LCompanion := TCompanion.Create;
  LCompanionLease := LCompanion;
  LCompanion.Scene := LScene;
  LCompanion.Revision := LRevision;
  LCompanion.Reject := True;
  LRefused := False;
  try
    PublishNyxGroup([LPublication, LCompanionLease]);
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LCompanion.Installed and not LPlan.Committed and
    (LScene.Count = 2) and (LScene.Revision = LRevision), 'independent refusal preserves both accepted owners');
  Check((LNew.Node.Parent = nil) and (LReplacement.Node.Parent = nil) and
    (LScene.Control(NyxCanvasItem('Second')).Node = LOriginal.Node), 'refusal releases candidate ownership without disturbing old roots');
  Check((LSnapshot.Count = 3) and (LSnapshot.Items[0].Placement.Bounds.Left = -33.5),
    'retained spatial preview survives refusal without control ownership');
  LPublication := nil;
  LPreview := nil;
  LPlan := nil;
  LCompanionLease := nil;

  LPlan := Proposal;
  Supports(LPlan, INyxPreparedPublication, LPublication);
  LCompanion := TCompanion.Create;
  LCompanionLease := LCompanion;
  LCompanion.Scene := LScene;
  LCompanion.Revision := LRevision;
  PublishNyxGroup([LCompanionLease, LPublication]);
  Check(LPlan.Committed and LCompanion.Notified and (LScene.Count = 3), 'one group publishes the complete mixed membership');
  Check((LScene.Control(NyxCanvasItem('First')).Node = LButton.Node) and
    (LScene.Control(NyxCanvasItem('Second')).Node = LReplacement.Node) and
    (LScene.Control(NyxCanvasItem('Third')).Node = LNew.Node), 'accepted scene contains exact retained/replaced/added controls');
  LIdentity.TryContentIdentity(NyxCanvasItem('First'), LAfterIdentity);
  Check(LAfterIdentity = LFirstIdentity, 'placement retains the original content lifetime token');
  LIdentity.TryContentIdentity(NyxCanvasItem('Second'), LAfterIdentity);
  Check((LAfterIdentity <> LSecondIdentity) and (LOriginal.Node.Parent = nil),
    'replacement rotates content identity and detaches a retained original editor');
  LPublication := nil;
  LPlan := nil;
  LCompanionLease := nil;
  LRevision := LScene.Revision;

  LRefused := False;
  try
    LPlan := LEditing.PrepareContent([
      NyxCanvasAddControl(NyxCanvasItem('Fourth'), LPlacement, LOriginal),
      NyxCanvasAddControl(NyxCanvasItem('Fifth'), LPlacement, LOriginal)], LRevision);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LScene.Revision = LRevision) and (LScene.Count = 3) and
    (LOriginal.Node.Parent = nil), 'duplicate root ownership refuses all staged additions and releases its reservation');

  LRefused := False;
  try
    LPlan := LEditing.PrepareContent([
      NyxCanvasRemoveControl(NyxCanvasItem('First')),
      NyxCanvasPlaceControl(NyxCanvasItem('First'), LPlacement)], LRevision);
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LScene.Count = 3) and (LScene.Revision = LRevision),
    'repeated item intents refuse before membership publication');
  LRefused := False;
  try
    LPlan := LEditing.PrepareContent([nil], LRevision);
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LScene.Revision = LRevision), 'absent changes do not retain a reservation');

  LChange := TReentrantChange.Create;
  LForeign := LChange;
  LChange.Scene := LScene;
  LChange.Control := LOriginal;
  LRefused := False;
  try
    LPlan := LEditing.PrepareContent([LForeign], LRevision);
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LScene.Revision = LRevision + 1) and (LScene.Count = 3) and
    (LOriginal.Node.Parent = nil), 'foreign getter work remains owned and stale captured content refuses before adoption');
  LForeign := nil;
  LPlan := LEditing.PrepareContent([NyxCanvasRemoveControl(NyxCanvasItem('Second'))], LScene.Revision);
  LPlan.Commit;
  Check(LPlan.Committed and (LScene.Count = 2) and (LReplacement.Node.Parent = nil),
    'standalone commit releases removed content and preserves retained interfaces');
  LPlan := nil;
  LPlan := LEditing.PrepareContent([], LScene.Revision);
  LPlan.Commit;
  Check((LScene.Count = 2) and (LScene.Control(NyxCanvasItem('First')).Node = LButton.Node),
    'empty prepared membership retains exact implementations');
  LPlan := nil;
  LEditing := nil;
  LIdentity := nil;
  LScene := nil;
  Check((GDestroyed = 0) and (LButton.Node.Parent = nil), 'external control lease survives scene retirement');
  LButton := nil;
end;

begin
  try
    Run;
    { Native temporary interface expressions retire at routine exit. Check
      destruction after Run's complete scope, not only its named local leases. }
    Check(GDestroyed = 1, 'content identity and spatial snapshots retain no control cycle');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-content', 'passed');
    document.body.setAttribute('data-canvas-content-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' prepared canvas content checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-content', 'failed');
      document.body.setAttribute('data-canvas-content-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
