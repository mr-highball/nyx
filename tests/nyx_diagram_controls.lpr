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
program nyx_diagram_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.events, nyx.behavior,
  nyx.scheduler, nyx.diagram, nyx.diagram.drawing, nyx.drawing,
  nyx.canvas.geometry, nyx.test.diagram
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, Graphics,
    nyx.render.lcl, nyx.drawing.lcl{$endif};

type
  { Closed workshop actions exercise the same public plan and Drawing contract;
    these are fixture controls, not a second editor command/history subsystem. }
  TDiagramAction = (daMove, daResize, daRestore, daRefuse);

  { Fixture-owned semantic callback borrows globals only during synchronous UI
    dispatch. Renderer retirement cancels registrations before the fixture frees
    those globals; no callback retains a document, renderer or widget. }
  TDiagramCallback = class(TNyxEventCallback)
  public
    Action: TDiagramAction;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  {$ifndef PAS2JS}
  { Test-only Win32 exception receiver lives through final message retirement,
    preserving unexpected framework failures instead of opening modal dialogs. }
  TFailure = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  {$endif}

var
  GChecks: Integer;
  GChanges: Integer;
  GRefusals: Integer;
  GDocument: TNyxDocument;
  GInitial: INyxDiagram;
  GCurrent: INyxDiagram;
  GDrawing: INyxDrawing;
  GSubscriptions: array of INyxEventSubscription;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GRenderer: TNyxBrowserRenderer;
  GFace: TJSHTMLElement;
  {$else}
  GHost: TForm;
  GRenderer: TNyxLCLRenderer;
  GFace: TNyxLCLDrawing;
  GFailure: TFailure;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Diagram controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function ActionID(AAction: TDiagramAction): TNyxText;
begin
  case AAction of
    daMove: Result := 'move-shape';
    daResize: Result := 'resize-shape';
    daRestore: Result := 'restore-layout';
    daRefuse: Result := 'check-connected-removal';
  end;
end;

function ActionCaption(AAction: TDiagramAction): TNyxText;
begin
  case AAction of
    daMove: Result := 'Move';
    daResize: Result := 'Resize';
    daRestore: Result := 'Restore';
    daRefuse: Result := 'Check removal';
  end;
end;

procedure TDiagramCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LCandidate: INyxDiagram;
  LProjection: INyxDrawingPlan;
  LVertex: TNyxDiagramVertex;
begin
  LCandidate := GCurrent;
  LVertex := GCurrent.FindVertex(NyxDiagramVertexRef('decision'));
  case Action of
    daMove:
      LCandidate := GCurrent.WithVertex(LVertex.AtBounds(NyxCanvasBounds(300, 120, 120, 100)));
    daResize:
      LCandidate := GCurrent.WithVertex(LVertex.AtBounds(NyxCanvasBounds(300, 120, 180, 140)));
    daRestore:
      LCandidate := GInitial;
    daRefuse:
      begin
        try
          GCurrent.WithoutVertex(NyxDiagramVertexRef('decision'));
        except
          on E: ENyxDiagram do
          begin
            Inc(GRefusals);
          end;
        end;
        Exit;
      end;
  end;
  { Prepare the whole drawing before changing either fixture owner. A refused
    graph/projection must leave the accepted model and retained face unchanged. }
  LProjection := DrawNyxDiagram(LCandidate);
  { Ordinary Render owns an independent runtime projection. Apply through its
    public typed bridge rather than expecting Sync to re-realize authored
    document nodes. This deliberately edits runtime presentation only. }
  SetNyxDrawingPlan(RetainNyxControl(GRenderer.Root.Find('diagram-preview')), LProjection);
  GCurrent := LCandidate;
  GRenderer.Sync;
  Inc(GChanges);
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.Message;
end;
{$endif}

procedure Setup;
var
  LToolbar: INyxRow;
  LAction: TDiagramAction;
  LCallback: TDiagramCallback;
begin
  GInitial := DiagramFixture;
  GCurrent := GInitial;
  GDocument := TNyxDocument.Create;
  GDocument.Title := 'Diagram workshop';
  GDocument.AddPage(NewNyxPage('home'));
  GDocument.Pages[0].Configure.Padding(20).Gap(12).HeightSizing(nsFill).Done;
  GDocument.Pages[0].Add(NewNyxHeading('title').WithText('Connected ideas'));
  GDocument.Pages[0].Add(NewNyxLabel('description')
    .WithText('Move and resize a shape. Its connections follow.'));
  LToolbar := NewNyxRow('diagram-actions');
  LToolbar.Configure.Gap(8).Wrap(nfwWrap).Done;
  for LAction := Low(TDiagramAction) to High(TDiagramAction) do
  begin
    LToolbar.Add(NewNyxButton(ActionID(LAction)).WithText(ActionCaption(LAction)));
  end;
  GDocument.Pages[0].Add(LToolbar);
  GDrawing := NewNyxDrawing('diagram-preview').WithText('Connected workflow diagram')
    .WithPlan(DrawNyxDiagram(GCurrent));
  GDrawing.Configure.Flex(1).HeightSizing(nsFill).MinimumHeight(200).Done;
  GDocument.Pages[0].Add(GDrawing);
  GDocument.Pages[0].Add(NewNyxLabel('footer')
    .WithText('Typed shapes, boundary connections and independent snapshots.'));
  GDocument.Validate;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('height', '100dvh');
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  GHost := TForm.CreateNew(nil);
  GHost.Caption := 'Nyx diagram workshop';
  GHost.SetBounds(30, 30, 1040, 680);

  if ParamStr(2) = '--compact' then
  begin
    GHost.SetBounds(30, 30, 390, 740);
  end;
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False);
  SetLength(GSubscriptions, Ord(High(TDiagramAction)) + 1);
  for LAction := Low(TDiagramAction) to High(TDiagramAction) do
  begin
    LCallback := TDiagramCallback.Create;
    LCallback.Action := LAction;
    GSubscriptions[Ord(LAction)] := GRenderer.Events.On(NyxControlEvents(ActionID(LAction)),
      ntClick).Subscribe(LCallback);
  end;
  {$ifdef PAS2JS}
  GFace := GRenderer.ElementFor('diagram-preview');
  {$else}
  Application.ProcessMessages;
  GFace := TNyxLCLDrawing(GRenderer.ControlFor('diagram-preview'));
  {$endif}
end;

procedure Click(AAction: TDiagramAction);
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(GRenderer.ElementFor(ActionID(AAction))).click;
  {$else}
  TButton(GRenderer.ControlFor(ActionID(AAction))).Click;
  Application.ProcessMessages;
  {$endif}
  Check(GSubscriptions[Ord(AAction)].LastExecution <> nil, 'ordinary Nyx event registration executed');
end;

procedure FaceEqualsCurrent;
begin
  Check(GDrawing.Plan.ToData.ToJSON = DrawNyxDiagram(GInitial).ToData.ToJSON,
    'runtime edits preserve the independent authored Drawing declaration');
  {$ifdef PAS2JS}
  Check(GRenderer.ElementFor('diagram-preview') = GFace, 'same browser control retained');
  Check(GFace.querySelectorAll('svg').length = 1, 'one ordinary drawing projection');
  {$else}
  Check(GRenderer.ControlFor('diagram-preview') = GFace, 'same native drawing retained');
  Check(GFace.Plan.ToData.ToJSON = DrawNyxDiagram(GCurrent).ToData.ToJSON,
    'actual native face equals complete accepted graph projection');
  {$endif}
end;

procedure Run;
var
  LBefore: TNyxText;
  LRoute: TNyxDiagramRoute;
  {$ifndef PAS2JS}
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
begin
  FaceEqualsCurrent;
  LBefore := GCurrent.ToData.ToJSON;
  Click(daMove);
  Check(GChanges = 1, 'Move routed through public Nyx event');
  LRoute := GCurrent.Route(NyxDiagramEdgeRef('review'));
  Check(LRoute.Point(LRoute.Count - 1).Y = 170, 'visible move reroutes the connected boundary');
  Check(GInitial.ToData.ToJSON = LBefore, 'original snapshot stays exact');
  FaceEqualsCurrent;
  Click(daResize);
  Check(GChanges = 2, 'Resize routed through public Nyx event');
  LRoute := GCurrent.Route(NyxDiagramEdgeRef('review'));
  Check(LRoute.Point(LRoute.Count - 1).Y = 190, 'visible resize updates attachment');
  FaceEqualsCurrent;
  LBefore := GCurrent.ToData.ToJSON;
  Click(daRefuse);
  Check((GRefusals = 1) and (GChanges = 2) and (GCurrent.ToData.ToJSON = LBefore),
    'connected removal refuses without changing accepted model or drawing');
  FaceEqualsCurrent;
  Click(daRestore);
  Check((GChanges = 3) and (GCurrent.ToData.ToJSON = GInitial.ToData.ToJSON),
    'Restore reuses exact independent snapshot, without claiming Studio paired history');
  FaceEqualsCurrent;
  {$ifndef PAS2JS}
  Check((GFace.Width > 300) and (GFace.Height > 200), 'actual allocation remains visible');

  if ParamStr(1) <> '' then
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
  Check(GFailure.Message = '', 'no framework error escaped callbacks or layout');
  {$endif}
end;

procedure Cleanup;
var
  LAction: TDiagramAction;
begin
  FreeAndNil(GRenderer);
  for LAction := Low(TDiagramAction) to High(TDiagramAction) do
  begin

    if (Ord(LAction) < Length(GSubscriptions)) and (GSubscriptions[Ord(LAction)] <> nil) then
    begin
      Check(not GSubscriptions[Ord(LAction)].Active, 'renderer retirement closes callback authority');
      GSubscriptions[Ord(LAction)] := nil;
    end;
  end;
  GDrawing := nil;
  GCurrent := nil;
  GInitial := nil;
  FreeAndNil(GDocument);
  {$ifdef PAS2JS}
  GHost.remove;
  GHost := nil;
  {$else}
  FreeAndNil(GHost);
  Application.ProcessMessages;
  Application.OnException := nil;
  FreeAndNil(GFailure);
  {$endif}
end;

{$ifdef PAS2JS}
procedure AfterLayout;
begin
  try
    Run;
    Cleanup;
    document.body.setAttribute('data-diagram-controls', 'passed');
    document.body.setAttribute('data-diagram-controls-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' diagram control checks';
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-diagram-controls', 'failed');
      document.body.setAttribute('data-diagram-controls-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    {$endif}
    Setup;
    {$ifdef PAS2JS}
    window.setTimeout(@AfterLayout, 180);
    {$else}
    Run;
    Cleanup;
    WriteLn('PASS ', GChecks, ' diagram control checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-controls', 'failed');
      document.body.setAttribute('data-diagram-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      Cleanup;
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
