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
program nyx_canvas_scaling_inspector_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.canvas.types, nyx.canvas.editor, nyx.studio.inspector, nyx.studio.session,
  nyx.studio.projects, nyx.json.workshop {$ifdef PAS2JS}, Web{$endif};

type
  { A creator compound uses the same public fluent methods as the real palette.
    Temporary Self references retire before its constructor returns. The failure
    branch also checks that constructor unwinding destroys this wrapper once. }
  TConstructionProbe = class(TNyxCard)
  public
    constructor Create(AFail: Boolean); reintroduce;
    destructor Destroy; override;
  end;

var
  GChecks: Integer;
  GProbeDestroyed: Integer;

constructor TConstructionProbe.Create(AFail: Boolean);
begin
  inherited Create('construction-probe', ncoDescriptor);
  Add(NewNyxLabel('construction-message').WithText('Still constructing'));

  if AFail then
  begin
    raise Exception.Create('Expected construction refusal');
  end;
end;

destructor TConstructionProbe.Destroy;
begin
  Inc(GProbeDestroyed);
  inherited Destroy;
end;

procedure Phase(const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-scaling-inspector-phase', AText);
  {$else}
  WriteLn('PHASE ', AText);
  {$endif}
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Match the raw parent/temporary interface boundary used by the Inspector.
  This scope releases its constructor temporary before the caller observes it. }
procedure AddSceneForm(AParent: TNyxNode; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument);
begin
  AParent.Add(NewNyxCanvasSceneEditor('scene', AOwner, ADocument));
end;

{ Exercise production form construction, managed adoption and retirement without
  a target renderer. The ordinary browser Studio journey qualifies physical
  controls separately. Individual forms locate failures before the whole shell
  can mask them during cleanup. The document/session own independent snapshots. }
procedure Run;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LRoot: INyxColumn;
  LForm: INyxCard;
  LProbe: INyxCard;
  LScene: TNyxCanvasRef;
  LOwner: TNyxNode;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LDocument := BuildJSONWorkshopDocument;
  LSession := nil;
  try
    Phase('compound construction');
    LProbe := TConstructionProbe.Create(False);
    Check((GProbeDestroyed = 0) and (LProbe.Node.Find('construction-message') <> nil),
      'fluent constructor temporaries cannot retire a creator compound');
    LProbe := nil;
    Check(GProbeDestroyed = 1, 'final caller release destroys the completed compound once');
    LRefused := False;
    try
      LProbe := TConstructionProbe.Create(True);
    except
      on E: Exception do
      begin

        if E.Message <> 'Expected construction refusal' then
        begin
          raise;
        end;
        LRefused := True;
      end;
    end;
    Check(LRefused and (GProbeDestroyed = 2),
      'constructor refusal retains its original error and destroys the partial compound once');
    LOwner := LDocument.Find(JSONWorkshopCanvas.ID).Part(NyxPart('workspace'));
    LScene := LOwner.CanvasScene;
    Phase('scaling form');
    LForm := NewNyxCanvasScalingEditor('scaling', LScene, LDocument);
    Check(LForm.Node.Find(NyxCanvasScalingPart('scaling', ncspChoice).ID) <> nil,
      'the standalone scaling form retains its typed choice');
    Phase('scaling adoption');
    LRoot := NewNyxColumn('forms');
    LRoot.Add(LForm);
    LForm := nil;
    Check(LRoot.Node.Find('scaling') <> nil, 'parent adoption retains the scaling form');
    Phase('scene form');
    AddSceneForm(LRoot.Node, NyxControl(LOwner.ID), LDocument);
    Check(LRoot.Node.Find('scene') <> nil, 'raw parent adoption retains the scene form');
    Phase('boundary form');
    LRoot.Add(NewNyxCanvasBoundaryEditor('boundary', LScene, LDocument));
    Phase('precision form');
    LRoot.Add(NewNyxCanvasPrecisionEditor('precision', LScene, LDocument));
    Check(LRoot.Count = 4, 'all scene forms coexist under one managed parent');
    Phase('forms retirement');
    LRoot := nil;
    LSession := TNyxStudioSession.Create(NyxProjectPair(
      TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument)));
    LSession.Select(LOwner.ID);
    for LIndex := 1 to 3 do
    begin
      Phase('ordinary Inspector / ' + IntToStr(LIndex));
      LRoot := NewNyxColumn('inspector');
      AddNyxCanvasInspector(LRoot.Node, LSession);
      Check(LRoot.Node.Find('inspector-canvas-scaling') <> nil,
        'ordinary Inspector retains the reusable scaling form');
      Check(LRoot.Node.Find('inspector-canvas-precision') <> nil,
        'ordinary Inspector retains the following precision form');
      LRoot := nil;
    end;
    Phase('complete');
  finally
    LProbe := nil;
    LForm := nil;
    LRoot := nil;
    LSession.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-scaling-inspector', 'passed');
    document.body.setAttribute('data-scaling-inspector-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' scaling Inspector ownership checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-scaling-inspector', 'failed');
      document.body.setAttribute('data-scaling-inspector-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
