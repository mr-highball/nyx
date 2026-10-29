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
program nyx_json_workshop_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.source, nyx.files, nyx.studio.files, nyx.studio.projects,
  nyx.state, nyx.logic, nyx.logic.http, nyx.http.types, nyx.resources.loader,
  nyx.resource.sources, nyx.files.save, nyx.files.open, nyx.scheduler,
  nyx.json.workshop, nyx.tool.canvas, nyx.canvas.types, nyx.canvas.workspace,
  nyx.canvas.geometry, nyx.canvas.observers, nyx.canvas.navigation,
  nyx.flow.routing, nyx.flow.declarations, nyx.view.behavior,
  nyx.programs, nyx.programs.types, nyx.programs.contract, nyx.programs.declarations,
  {$ifdef NYX_JSON_WORKSHOP_STUDIO}nyx.test.json.workshop.studio,{$endif}
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser, nyx.canvas.browser, nyx.tool.canvas.browser
  {$else}
  Classes, Interfaces, Forms, Controls, StdCtrls, nyx.application.lcl, nyx.canvas.lcl,
  nyx.files.lcl, nyx.tool.canvas.lcl, nyx.resources.http.lcl, nyx.test.capture.lcl
  {$endif}
  {$ifdef NYX_COMPILED_JSON_WORKSHOP}, nyx.generated.json.workshop{$endif};

const
  CExactJSON: TNyxText = '{"name":"River 🌙","count":9007199254740993,"note":"line\u0000end"}';
  CReplacement: TNyxText = 'begin Prepared := ''{"custom":"Made with Pascal 🌙"}''; end.';
  CUnfinished: TNyxText = 'begin Prepared := ; { unfinished input } end.';
  COpenedName: TNyxText = 'Sketch 🌙.json';
  CNewerJSON: TNyxText = '{"draft":"Keep my newer text 🌙"}';

type
  {$ifdef PAS2JS}
  { The test uses the ordinary browser application and its real embedded inputs. }
  TReviewApplication = TNyxBrowserApplication;
  { A retained presentation capability never owns or reparents borrowed DOM. }
  TReviewCanvas = INyxBrowserCanvasWorkspaceView;
  { Optional presentation facade borrows only the actual Tool Canvas renderer. }
  TReviewTools = INyxBrowserToolCanvasView;
  {$else}
  { The test uses the ordinary native application and its real embedded inputs. }
  TReviewApplication = TNyxLCLApplication;
  { A retained presentation capability never owns or reparents borrowed widgets. }
  TReviewCanvas = INyxLCLCanvasWorkspaceView;
  { Optional presentation facade borrows only the actual Tool Canvas renderer. }
  TReviewTools = INyxLCLToolCanvasView;
  { Expose inherited standard button input without replacing any controller. }
  TControlInput = class(TControl);
  { Unexpected widget exceptions fail the finite journey instead of opening a modal. }
  TFailures = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { Shared request cancellation owns the borrowed reply lifetime. Completion is
    injected, so these assertions claim no real dialog, disk write or download. }
  TSaveRequest = class(TNyxFileSaveRequest)
  public
    procedure Finish(AOutcome: TNyxFileSaveOutcome);
    function IsPending: Boolean;
    function Payload: TNyxBytes;
  end;
  { One test-owned host per App retains its most recent request independently.
    It owns no application/view/document and never manufactures a file capability. }
  TSaveHost = class(TInterfacedObject, INyxFileSaveHost)
  public
    Calls: Integer;
    Request: TSaveRequest;
    Lease: INyxFileSaveRequest;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;
  { Controlled read completion follows the same revocable receiver contract as
    real hosts. A completed snapshot is data; it grants no disk or picker access. }
  TOpenRequest = class(TNyxFileOpenRequest)
  public
    procedure Finish(AOutcome: TNyxFileOpenOutcome; const AFile: INyxFileSnapshot = nil);
    function IsPending: Boolean;
  end;
  { Independent opening authority per App. The fixture keeps only copied options
    and its latest token, never an application, view or callback after retirement. }
  TOpenHost = class(TInterfacedObject, INyxFileOpenHost)
  public
    Calls: Integer;
    Options: TNyxFileOpenOptions;
    Request: TOpenRequest;
    Lease: INyxFileOpenRequest;
    function BeginOpen(const AOptions: TNyxFileOpenOptions;
      const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
      AReply: TNyxFileOpenReply): INyxFileOpenRequest;
  end;

  { A controlled byte response uses the real revocable transport receiver.
    Retaining this token owns no application or view; a late Finish after Cancel
    cannot enter the retired graph. It grants no actual network authority. }
  TNetworkRequest = class(TNyxResourceRequest)
  public
    procedure Finish(AStatus: Integer; const ABytes: TNyxBytes;
      const AError: TNyxText = '');
    function IsPending: Boolean;
  end;

  { Each App gets an independent explicitly injected transport. Captured request
    data and the pending token retain no document. The optional native delegate
    is supplied only by the test's explicit loopback-origin qualification. }
  TNetworkHost = class(TInterfacedObject, INyxHTTPTransport)
  public
    Calls: Integer;
    Last: TNyxHTTPRequest;
    Request: TNetworkRequest;
    Lease: INyxResourceRequest;
    Delegate: INyxHTTPTransport;
    function Send(const ARequest: TNyxHTTPRequest;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
  end;

var
  GDocument: TNyxDocument;
  GReopened: TNyxDocument;
  GApps: array[0..1] of TReviewApplication;
  GViews: array of TReviewCanvas;
  GHosts: array[0..1] of TSaveHost;
  GHostLeases: array of INyxFileSaveHost;
  GOpenHosts: array[0..1] of TOpenHost;
  GOpenLeases: array of INyxFileOpenHost;
  GNetworkHosts: array[0..1] of TNetworkHost;
  { Dynamic interface vectors retain the same ownership on FPC and pas2js;
    pas2js does not support static arrays of COM interfaces. }
  GNetworkLeases: array of INyxHTTPTransport;
  GNetworkURL: TNyxText;
  GNetworkRevision: Integer;
  GInteractionStarted: Double;
  GNodes: array of INyxToolCanvasPipelineNodes;
  GUniformOperationsFitted: Boolean;
  GUniformOperationPositions: array[0..1] of TNyxText;
  GUniformOperationFits: array[0..1] of TNyxCanvasNavigationStatus;
  GBefore: TNyxText;
  GPretty: TNyxText;
  GStage: Integer;
  GChecks: Integer;
  GDone: Boolean;
  GInitialized: Boolean;
  {$ifdef PAS2JS}
  GUniformReviewReady: Boolean;
  {$endif}
  GWidth: Integer = 1280;
  GTools: INyxToolCanvasView;
  GToolFace: TReviewTools;
  GProgramAuthoring: INyxToolCanvasProgramAuthoring;
  GProgramEditing: INyxCanvasProgramEditing;
  GAcceptedPrograms: TNyxText;
  GCustomPretty: TNyxText;
  GPeerPretty: TNyxText;
  GOpenPretty: TNyxText;
  GOpenCustomPretty: TNyxText;
  GOpenSourceBefore: TNyxText;
  GOpenPreviewBefore: TNyxText;
  GOpenRevision: Integer;
  {$ifdef NYX_JSON_WORKSHOP_STUDIO}
  GStudioJourney: INyxJSONWorkshopStudioJourney;
  GStudioPair: TNyxProjectPair;
  GStudioReady: Boolean;
  {$endif}
  {$ifdef PAS2JS}
  GStarted: Double;
  GElements: array[0..1] of TJSHTMLElement;
  {$else}
  GStarted: QWord;
  GDirectory: TNyxText;
  GFailures: TFailures;
  GLoggedStage: Integer = -1;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if GFailures.Error <> '' then
  begin
    raise Exception.Create('Native callback: ' + GFailures.Error);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('JSON Workshop stage ' + IntToStr(GStage) + ': ' + AReason);
  end;
  Inc(GChecks);
end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

{ Building/admitting two applications, their existing file/program journey and
  the added network journey are distinct bounded phases. Each has a 60-second
  allowance; the maintained runner enforces an independent 120-second total.
  Stage timestamps include all phases so their actual costs remain visible. }
function JourneyTimedOut: Boolean;
begin

  if GInitialized then
  begin
    Result := Clock - GInteractionStarted > 60000;
  end
  else
  begin
    Result := Clock - GStarted > 60000;
  end;
end;

{$ifndef PAS2JS}
procedure TFailures.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

procedure TNetworkRequest.Finish(AStatus: Integer; const ABytes: TNyxBytes;
  const AError: TNyxText);
var
  LResult: TNyxResourceHTTPResult;
begin
  LResult := Default(TNyxResourceHTTPResult);
  LResult.Status := AStatus;
  LResult.Bytes := Copy(ABytes, 0, Length(ABytes));
  LResult.Error := AError;
  Complete(LResult);
end;

function TNetworkRequest.IsPending: Boolean;
begin
  Result := Active;
end;

function TNetworkHost.Send(const ARequest: TNyxHTTPRequest;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
begin
  Inc(Calls);
  Last := TNyxHTTPRequest.FromData(ARequest.ToData);

  if Delegate <> nil then
  begin
    Exit(Delegate.Send(ARequest, AReply));
  end;
  Request := TNetworkRequest.Create(AReply);
  Result := Request;
  Lease := Result;
end;

procedure TSaveRequest.Finish(AOutcome: TNyxFileSaveOutcome);
var
  LCount: Integer;
begin
  LCount := 0;

  if AOutcome in [nfsoWritten, nfsoDownloadRequested] then
  begin
    LCount := Length(FBytes);
  end;
  Complete(AOutcome, FOptions.SuggestedName, LCount);
end;

function TSaveRequest.IsPending: Boolean;
begin
  Result := Pending;
end;

function TSaveRequest.Payload: TNyxBytes;
begin
  Result := Copy(FBytes, 0, Length(FBytes));
end;

function TSaveHost.BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxFileSaveReply): INyxFileSaveRequest;
begin
  AScheduler.RequireUI;
  Inc(Calls);
  Request := TSaveRequest.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := Request;
  Lease := Result;
end;

procedure TOpenRequest.Finish(AOutcome: TNyxFileOpenOutcome; const AFile: INyxFileSnapshot);
begin
  Complete(AOutcome, AFile);
end;

function TOpenRequest.IsPending: Boolean;
begin
  Result := Pending;
end;

function TOpenHost.BeginOpen(const AOptions: TNyxFileOpenOptions;
  const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
  AReply: TNyxFileOpenReply): INyxFileOpenRequest;
begin
  AScheduler.RequireUI;
  Inc(Calls);
  Options := AOptions.Copy;
  Request := TOpenRequest.Create(AOptions, AScheduler, AScope, AReply);
  Result := Request;
  Lease := Result;
end;

procedure TypeSource(AApp: Integer; const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(GViews[AApp].InputFor(JSONWorkshopItem,
    JSONWorkshopControl(jwcSource))).value := AText;
  TJSHTMLElement(GViews[AApp].InputFor(JSONWorkshopItem,
    JSONWorkshopControl(jwcSource))).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomMemo(GViews[AApp].InputFor(JSONWorkshopItem,
    JSONWorkshopControl(jwcSource))).Text := AText;
  {$endif}
end;

{ The same saved/generated Workshop opts into this path only when its authored
  scene requests uniform zoom. Use live controls before adding optional graph
  cards: Fit must expose the Notebook's final result and actions, not merely
  its allocation. Existing reflow/fixed-size journeys keep their own behavior. }
procedure CheckUniformNotebook(AApp: Integer);
const
  CZooms: array[0..3] of Double = (1, 0.5, 1.25, 0.5);
  CDraft: TNyxText = 'Keep this live draft / 🌙';
var
  LCanvas: TNyxNode;
  LGeometry: INyxCanvasControlGeometry;
  LBounds: TNyxCanvasBounds;
  LIndex: Integer;
  LControl: TJSONWorkshopControl;
  {$ifdef PAS2JS}
  LInput: TJSHTMLTextAreaElement;
  {$else}
  LInput: TCustomMemo;
  {$endif}
begin
  LCanvas := GDocument.Find(JSONWorkshopCanvas.ID).Part(NyxPart('workspace'));

  if GDocument.Canvases.Definition(LCanvas.CanvasScene).Scaling <> nccsUniform then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-uniform-notebook', 'start / ' + IntToStr(AApp));
  {$endif}
  GViews[AApp].SetCamera(GViews[AApp].Camera.AtCenter(NyxCanvasPoint(245, 500), 1));
  {$ifdef PAS2JS}
  LInput := TJSHTMLTextAreaElement(GViews[AApp].InputFor(JSONWorkshopItem,
    JSONWorkshopControl(jwcSource)));
  LInput.focus;
  {$else}
  LInput := TCustomMemo(GViews[AApp].InputFor(JSONWorkshopItem,
    JSONWorkshopControl(jwcSource)));
  LInput.SetFocus;
  {$endif}
  TypeSource(AApp, CDraft);
  {$ifdef PAS2JS}
  LInput.selectionStart := 2;
  LInput.selectionEnd := 5;
  {$else}
  LInput.SelStart := 2;
  LInput.SelLength := 3;
  {$endif}
  for LIndex := Low(CZooms) to High(CZooms) do
  begin
    {$ifdef PAS2JS}
    document.body.setAttribute('data-uniform-notebook', 'zoom / ' + IntToStr(AApp) +
      ' / ' + IntToStr(LIndex));
    {$endif}
    GViews[AApp].SetCamera(GViews[AApp].Camera.AtCenter(
      NyxCanvasPoint(245, 500), CZooms[LIndex]));
    Check(GViews[AApp].InputFor(JSONWorkshopItem, JSONWorkshopControl(jwcSource)) = LInput,
      'the saved uniform Workshop retains its actual source editor through zoom');
    {$ifdef PAS2JS}
    Check((LInput.value = CDraft) and (LInput.selectionStart = 2) and
      (LInput.selectionEnd = 5), 'uniform Workshop retains the browser Unicode draft and caret');
    {$else}
    Check((TNyxText(LInput.Text) = CDraft) and (LInput.SelStart = 2) and
      (LInput.SelLength = 3), 'uniform Workshop retains the native Unicode draft and caret');
    {$endif}
    Check(GApps[AApp].State.GetValue(JSONWorkshopSource) = CDraft,
      'uniform Workshop zoom retains the independently bound runtime source');
  end;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-uniform-notebook', 'fit / ' + IntToStr(AApp));
  {$endif}
  GViews[AApp].Execute(nccFit);
  Check(Supports(GViews[AApp], INyxCanvasControlGeometry, LGeometry),
    'the saved uniform Workshop exposes its actual control geometry');
  for LControl := Low(TJSONWorkshopControl) to High(TJSONWorkshopControl) do
  begin

    if LControl in [jwcSource, jwcFormat, jwcSave, jwcPreview] then
    begin
      Check(LGeometry.TryControlBounds(JSONWorkshopItem, JSONWorkshopControl(LControl), LBounds),
        'Fit exposes each actual Notebook editor, result and principal action');
      {$ifndef PAS2JS}
      { Retain the actual measured failure, rather than inferring geometry from
        the saved camera or enlarging the original Fit assertion's tolerance. }
      WriteLn('Notebook fit app=', AApp, ', control=', Ord(LControl),
        ', bounds=', LBounds.Left:0:2, '/', LBounds.Top:0:2, '/',
        LBounds.Width:0:2, '/', LBounds.Height:0:2,
        ', viewport=', GViews[AApp].Camera.ViewportWidth:0:2, '/',
        GViews[AApp].Camera.ViewportHeight:0:2);
      {$endif}
      Check((LBounds.Left >= -1) and (LBounds.Top >= -1) and
        (LBounds.Left + LBounds.Width <= GViews[AApp].Camera.ViewportWidth + 1) and
        (LBounds.Top + LBounds.Height <= GViews[AApp].Camera.ViewportHeight + 1),
        'the fitted Notebook control lies completely inside the actual viewport');
    end;
  end;
  {$ifndef PAS2JS}
  SaveNyxNativeCapture(GApps[AApp].Window,
    GDirectory + 'uniform-workshop-fit-' + TNyxText(IntToStr(AApp)) + '.png', ncmPrint);
  {$endif}
  TypeSource(AApp, JSONWorkshopSample);
  {$ifdef PAS2JS}
  document.body.setAttribute('data-uniform-notebook', 'complete / ' + IntToStr(AApp));
  {$endif}
end;

{ Return to the same saved application's mixed scene after graph publication.
  Fit and its queued face projection occupy separate UI turns. Compare actual
  mounted extents, rather than inferring them from the camera or definition. }
function CheckUniformOperations: Boolean;
var
  LApp: Integer;
  LIndex: Integer;
  LOtherIndex: Integer;
  LScaling: INyxCanvasScalingView;
  LNavigation: INyxToolCanvasNavigation;
  LTools: INyxToolCanvasView;
  LFlow: INyxFlowDefinition;
  LBounds: TNyxCanvasBounds;
  LOther: TNyxCanvasBounds;
  LFit: TNyxCanvasNavigationResult;
  LPositions: TNyxDataValue;
  LPoint: TNyxCanvasPoint;
  LWorlds: array of TNyxCanvasBounds;
  LSavedOverlap: Boolean;
  LActualOverlap: Boolean;
  LAnyOverlap: Boolean;

  { The export is the explicit inert persistence boundary. Compare this saved
    graph's exact world anchors with the stable 280x320 operation allocation. }
  function SavedBounds(AStep: Integer): TNyxCanvasBounds;
  var
    LRow: Integer;
    LValue: TNyxDataValue;
  begin
    for LRow := 0 to LPositions.Count - 1 do
    begin
      LValue := LPositions.Item(LRow);

      if (LValue.Field('graph').AsText = JSONWorkshopSaveFlow.Name) and
        (LValue.Field('step').AsText = LFlow.Step(AStep).Name) then
      begin
        Exit(NyxCanvasBounds(LValue.Field('x').AsNumber, LValue.Field('y').AsNumber, 280, 320));
      end;
    end;
    raise Exception.Create('The saved operation has no exported placement');
  end;

  function Overlap(const ALeft, ARight: TNyxCanvasBounds; ATolerance: Double): Boolean;
  begin
    Result := (ALeft.Left < ARight.Left + ARight.Width - ATolerance) and
      (ALeft.Left + ALeft.Width > ARight.Left + ATolerance) and
      (ALeft.Top < ARight.Top + ARight.Height - ATolerance) and
      (ALeft.Top + ALeft.Height > ARight.Top + ATolerance);
  end;
begin
  Result := True;

  if not Supports(GViews[0], INyxCanvasScalingView, LScaling) or
    (LScaling.ControlScaling <> nccsUniform) then
  begin
    Exit;
  end;
  for LApp := 0 to 1 do
  begin
    LTools := GApps[LApp].View.ToolCanvasView(JSONWorkshopCanvas.ID);

    if (LTools.RefreshExecution <> nil) and
      (LTools.RefreshExecution.Status in [nesPending, nesRunning]) then
    begin
      Result := False;
      Exit;
    end;
  end;

  if not GUniformOperationsFitted then
  begin
    for LApp := 0 to 1 do
    begin
      GUniformOperationPositions[LApp] := GNodes[LApp].ExportPipelineNodePositions.ToJSON;
      LTools := GApps[LApp].View.ToolCanvasView(JSONWorkshopCanvas.ID);
      Check(Supports(LTools, INyxToolCanvasNavigation, LNavigation),
        'the same saved Workshop supplies public mixed-scene navigation');
      LFit := LNavigation.FitContent;
      GUniformOperationFits[LApp] := LFit.Status;
      Check(LFit.Status <> ncnsEmpty,
        'the same saved Workshop supplies a mixed Notebook/operation Fit proposal');
    end;
    GUniformOperationsFitted := True;
    Result := False;
    Exit;
  end;
  LFlow := GDocument.Flows.Definition(JSONWorkshopSaveFlow);
  for LApp := 0 to 1 do
  begin
    LPositions := GNodes[LApp].ExportPipelineNodePositions.Field('nodes');
    SetLength(LWorlds, LFlow.StepCount);
    for LIndex := 0 to LFlow.StepCount - 1 do
    begin
      LWorlds[LIndex] := SavedBounds(LIndex);
    end;
    LAnyOverlap := False;
    for LIndex := 0 to LFlow.StepCount - 1 do
    begin
      LBounds := GNodes[LApp].PipelineNodeBounds(LFlow.Step(LIndex));
      Check(LBounds.Defined and (LBounds.Left >= -1) and (LBounds.Top >= -1) and
        (LBounds.Left + LBounds.Width <= GViews[LApp].Camera.ViewportWidth + 1) and
        (LBounds.Top + LBounds.Height <= GViews[LApp].Camera.ViewportHeight + 1),
        'mixed Fit exposes the complete actual saved operation face');
      LPoint := GViews[LApp].Camera.WorldToViewport(
        NyxCanvasPoint(LWorlds[LIndex].Left, LWorlds[LIndex].Top));
      Check((Abs(LBounds.Left - LPoint.X) <= 1) and (Abs(LBounds.Top - LPoint.Y) <= 1) and
        (Abs(LBounds.Width - LWorlds[LIndex].Width * GViews[LApp].Camera.Zoom) <= 1) and
        (Abs(LBounds.Height - LWorlds[LIndex].Height * GViews[LApp].Camera.Zoom) <= 1),
        'actual mixed operation extents retain their authored world projection');
      for LOtherIndex := LIndex + 1 to LFlow.StepCount - 1 do
      begin
        LOther := GNodes[LApp].PipelineNodeBounds(LFlow.Step(LOtherIndex));
        LSavedOverlap := Overlap(LWorlds[LIndex], LWorlds[LOtherIndex], 0);
        LAnyOverlap := LAnyOverlap or LSavedOverlap;
        LActualOverlap := Overlap(LBounds, LOther, 1);
        Check(LOther.Defined and (LActualOverlap = LSavedOverlap),
          'mixed Fit introduces no overlap beyond the existing authored world placement');
      end;
    end;
    Check(((not LAnyOverlap) and (GUniformOperationFits[LApp] = ncnsComplete)) or
      (LAnyOverlap and (GUniformOperationFits[LApp] = ncnsLimited)),
      'mixed Fit honestly reports existing authored overlap without moving saved cards');
    Check(GNodes[LApp].ExportPipelineNodePositions.ToJSON = GUniformOperationPositions[LApp],
      'mixed Fit does not rewrite the saved application operation positions');
  end;
end;

procedure Click(AApp: Integer; AControl: TJSONWorkshopControl);
begin
  {$ifdef PAS2JS}
  GViews[AApp].RendererFor(JSONWorkshopItem).ElementFor(JSONWorkshopControl(AControl).ID).click;
  {$else}
  TButton(GViews[AApp].RendererFor(JSONWorkshopItem).ControlFor(JSONWorkshopControl(AControl).ID)).Click;
  {$endif}
end;

function FencedJSON: TNyxText;
begin
  Result := '```json' + #10 + CExactJSON + #10 + '```';
end;

function PlainFence: TNyxText;
begin
  Result := '```' + #13#10 + CExactJSON + #13#10 + '```';
end;

function LoadedJSON: TNyxText;
begin
  Result := TNyxText(#$FEFF) + FencedJSON;
end;

function OpenSnapshot(const AName, AText: TNyxText): INyxFileSnapshot;
var
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  Result := NyxFileSnapshot(NyxFileMetadata(AName, Length(LBytes)), LBytes);
end;

function InvalidUTF8Snapshot: INyxFileSnapshot;
var
  LBytes: TNyxBytes;
begin
  SetLength(LBytes, 1);
  LBytes[0] := 255;
  Result := NyxFileSnapshot(NyxFileMetadata('invalid.json', 1), LBytes);
end;

{ Copied diagnostic counters establish actual run completion. A timer tick alone
  cannot prove that a rejected asynchronous graph has finished without effects. }
function RunReport(AApp: Integer; AControl: TJSONWorkshopControl = jwcSave): TNyxDataValue;
var
  LRuntime: INyxCanvasRuntimeView;
  LRoutes: INyxFlowRouteBinding;
  LIndex: Integer;
begin

  if not Supports(GViews[AApp], INyxCanvasRuntimeView, LRuntime) then
  begin
    raise Exception.Create('The embedded canvas has no runtime behavior capability');
  end;
  LRoutes := LRuntime.BehaviorFor(JSONWorkshopItem).Routes;
  for LIndex := 0 to LRoutes.Count - 1 do
  begin
    Result := LRoutes.Report(LIndex);

    if Result.Field('owner').AsText = JSONWorkshopControl(AControl).ID then
    begin
      Exit;
    end;
  end;
  raise Exception.Create('The embedded control route has no diagnostic record');
end;

function OpenFinished(AApp, ACount: Integer): Boolean;
var
  LReport: TNyxDataValue;
begin
  LReport := RunReport(AApp, jwcOpen);
  Result := (LReport.Field('started').AsInteger = ACount) and
    (LReport.Field('active').AsInteger = 0);
end;

function ToolID(const ASuffix: TNyxText): TNyxText;
begin
  Result := GTools.Inspector.Root.ID + '-' + ASuffix;
end;

procedure ClickTool(const ASuffix: TNyxText);
begin
  GToolFace.Renderer.Reveal(ToolID(ASuffix), niRuntime);
  {$ifdef PAS2JS}
  GToolFace.Renderer.ElementFor(ToolID(ASuffix)).click;
  {$else}
  Check(GToolFace.Renderer.ControlFor(ToolID(ASuffix)).IsVisible,
    'ordinary program action is visible: ' + ASuffix);
  TControlInput(GToolFace.Renderer.ControlFor(ToolID(ASuffix))).Click;
  {$endif}
end;

procedure TypeProgram(const ASource: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(GToolFace.Renderer.InputFor(ToolID('program-source'))).value := ASource;
  GToolFace.Renderer.InputFor(ToolID('program-source')).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(GToolFace.Renderer.InputFor(ToolID('program-source'))).Text := ASource;
  {$endif}
end;

{ Exercise the exact authored program, not a separate native fence remover.
  Non-fences are retained for the strict parser; all text operations must keep
  supplementary scalars, escaped NUL and exact numeric spelling unchanged. }
procedure CheckPreparation;
var
  LProgram: INyxProgram;
  LResult: INyxProgramResult;
  LInputs: array of TNyxText;
  LExpected: array of TNyxText;
  LIndex: Integer;
begin
  LProgram := GDocument.Programs.Definition(JSONWorkshopPreparation).Compile;
  LInputs := [CExactJSON, FencedJSON, PlainFence,
    '```JSON' + #13#10 + CExactJSON + #13#10 + '```',
    '```json' + #10 + CExactJSON, '```javascript' + #10 + CExactJSON + #10 + '```',
    '```', '  ' + CExactJSON + #9, TNyxText(#$FEFF) + CExactJSON, LoadedJSON];
  LExpected := [CExactJSON, CExactJSON, CExactJSON, CExactJSON,
    LInputs[4], LInputs[5], LInputs[6], CExactJSON, CExactJSON, CExactJSON];
  for LIndex := 0 to High(LInputs) do
  begin
    LResult := LProgram.Run(NyxProgramValues.WithValue(
      NyxProgramSymbol('Source'), NyxProgramValue(LInputs[LIndex])));
    Check((LResult.Outputs.Value(NyxProgramSymbol('Prepared')).AsText = LExpected[LIndex]) and
      (LResult.ProposalCount = 0), 'authored preparation preserves exact input case ' + IntToStr(LIndex));
  end;
end;

{ Step references, rather than registration indices, select each graph's own
  explicit formatting policy. Opening and formatting are independently editable. }
function SavedFormatter(const AFlow: TNyxFlowRef): INyxJSONFormat;
var
  LDefinition: INyxFlowDefinition;
  LIndex: Integer;
  LOperation: INyxLogicOperation;
begin
  Result := nil;
  LDefinition := GDocument.Flows.Definition(AFlow);
  for LIndex := 0 to LDefinition.StepCount - 1 do
  begin

    if LDefinition.Step(LIndex).Name = JSONWorkshopStep(jwsFormat).Name then
    begin
      LOperation := NyxLogicFromData(LDefinition.Descriptor(LIndex).Kind,
        LDefinition.Configuration(LIndex));
      Check(Supports(LOperation, INyxJSONFormat, Result),
        'the saved formatter retains its specialized typed interface');
      Exit;
    end;
  end;
  raise Exception.Create('The saved graph has no formatter');
end;

procedure Initialize;
var
  LIndex: Integer;
  LPair: TNyxProjectPair;
  LResolved: TNyxProjectPair;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LTools: INyxToolCanvasView;
  LNodes: INyxToolCanvasPipelineNodes;
  LFormatter: INyxJSONFormat;
begin
  SetLength(GViews, 2);
  SetLength(GHostLeases, 2);
  SetLength(GOpenLeases, 2);
  SetLength(GNetworkLeases, 2);
  SetLength(GNodes, 2);
  {$ifdef NYX_COMPILED_JSON_WORKSHOP}
  GDocument := BuildNyxDocument;
  {$ifdef PAS2JS}
  LPair := DecodeNyxProject(document.getElementById('expected-project').textContent);
  {$else}
  LPair := DecodeNyxProject(ReadNyxTextFile(ParamStr(3)).Text);
  {$endif}
  Check(TNyxCodec.Encode(GDocument) = LPair.Design,
    'the independently compiled constructor reconstructs its own saved example exactly');
  {$else}
  {$ifdef NYX_JSON_WORKSHOP_STUDIO}
  GDocument := TNyxCodec.Decode(GStudioPair.Design);
  {$else}
  GDocument := BuildJSONWorkshopDocument;
  {$endif}
  {$endif}
  GBefore := TNyxCodec.Encode(GDocument);
  {$ifdef NYX_JSON_WORKSHOP_STUDIO}
  LSource := GStudioPair.Source;
  {$else}
  LSource := TNyxCodegen.Generate(GDocument, 'nyx.generated.json.workshop');
  {$endif}
  LPair := NyxProjectPair(GBefore, LSource);
  LWorkspace := nil;
  AdmitNyxProject(LPair, nprRequireMatch, GReopened, LWorkspace, LResolved);
  LWorkspace.Free;
  Check((TNyxCodec.Encode(GReopened) = GBefore) and
    (EncodeNyxProject(LResolved) = EncodeNyxProject(LPair)),
    'the exact example graphs, embedded controls and placement survive source/project admission');
  {$ifndef PAS2JS}
  WriteNyxTextFile(GDirectory + 'workshop.nyxproject',
    NyxTextFile('workshop.nyxproject', EncodeNyxProject(LResolved)));
  WriteNyxTextFile(GDirectory + 'nyx.generated.json.workshop.pas',
    NyxTextFile('nyx.generated.json.workshop.pas', LSource));
  Check(ReadNyxTextFile(GDirectory + 'workshop.nyxproject').Text = EncodeNyxProject(LPair),
    'saved project bytes remain exact on the isolated file boundary');
  {$endif}
  LFormatter := SavedFormatter(NyxFlowRef('Format JSON'));
  GPretty := LFormatter.Format(NewNyxJSONParse.Parse(CExactJSON));
  GCustomPretty := LFormatter.Format(NewNyxJSONParse.Parse('{"custom":"Made with Pascal 🌙"}'));
  GPeerPretty := LFormatter.Format(NewNyxJSONParse.Parse('{"peer":"Still independent 🌙"}'));
  LFormatter := SavedFormatter(JSONWorkshopOpenFlow);
  GOpenPretty := LFormatter.Format(NewNyxJSONParse.Parse(CExactJSON));
  GOpenCustomPretty := LFormatter.Format(NewNyxJSONParse.Parse('{"custom":"Made with Pascal 🌙"}'));
  CheckPreparation;
  for LIndex := 0 to 1 do
  begin
    GHosts[LIndex] := TSaveHost.Create;
    GHostLeases[LIndex] := GHosts[LIndex];
    GOpenHosts[LIndex] := TOpenHost.Create;
    GOpenLeases[LIndex] := GOpenHosts[LIndex];
    GApps[LIndex] := TReviewApplication.Create;
    GNetworkHosts[LIndex] := TNetworkHost.Create;
    GNetworkLeases[LIndex] := GNetworkHosts[LIndex];
    GApps[LIndex].ConfigureLogic(NewNyxHTTPLogicResolver(GNetworkLeases[LIndex]));
    GApps[LIndex].ConfigureFileSaving(GHostLeases[LIndex]);
    GApps[LIndex].ConfigureFileOpening(GOpenLeases[LIndex]);
    {$ifdef PAS2JS}
    GElements[LIndex] := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(GElements[LIndex]);

    if LIndex = 0 then
    begin
      GApps[LIndex].Run(GDocument, GElements[LIndex]);
    end
    else
    begin
      GApps[LIndex].Run(GReopened, GElements[LIndex]);
    end;
    GViews[LIndex] := GApps[LIndex].View.CanvasView(
      GApps[LIndex].View.Root.Find(JSONWorkshopCanvas.ID).Part(NyxPart('workspace')).ID) as TReviewCanvas;
    {$else}

    if LIndex = 0 then
    begin
      GApps[LIndex].Mount(GDocument);
    end
    else
    begin
      GApps[LIndex].Mount(GReopened);
    end;
    GApps[LIndex].Window.ClientWidth := GWidth;
    GApps[LIndex].Window.ClientHeight := 900;
    GApps[LIndex].Window.Show;
    GViews[LIndex] := GApps[LIndex].View.CanvasView(
      GApps[LIndex].View.Root.Find(JSONWorkshopCanvas.ID).Part(NyxPart('workspace')).ID) as TReviewCanvas;
    {$endif}
    LTools := GApps[LIndex].View.ToolCanvasView(JSONWorkshopCanvas.ID);
    CheckUniformNotebook(LIndex);
    ShowJSONWorkshopGraph(LTools);
    { pas2js must finalize a scalar interface result, not an indexed global
      array passed as an out parameter. Assign the retained array slot only
      after Supports has completed its ordinary local-interface ownership. }
    Check(Supports(LTools, INyxToolCanvasPipelineNodes, LNodes),
      'the example exposes its managed operation-card capability');
    GNodes[LIndex] := LNodes;
  end;
end;

{$I nyx_json_workshop_network.inc}

procedure Stage;
var
  LToolFace: TReviewTools;
  LProgramAuthoring: INyxToolCanvasProgramAuthoring;
  LProgramEditing: INyxCanvasProgramEditing;
begin
  {$ifdef NYX_JSON_WORKSHOP_STUDIO}

  if not GStudioReady then
  begin

    if GStudioJourney = nil then
    begin
      GStudioJourney := NewJSONWorkshopStudioJourney(
        {$ifdef PAS2JS}''{$else}GDirectory{$endif}, GWidth);
    end;

    if not GStudioJourney.Tick then
    begin
      Exit;
    end;
    GStudioPair := GStudioJourney.Pair;
    Inc(GChecks, GStudioJourney.Checks);
    GStudioJourney := nil;
    GStudioReady := True;
    {$ifdef PAS2JS}GStarted := TJSDate.now;{$else}GStarted := GetTickCount64;{$endif}
  end;
  {$endif}

  if (GTools <> nil) and (GTools.RefreshExecution <> nil) then
  begin

    if GTools.RefreshExecution.Status in [nesPending, nesRunning] then
    begin
      Exit;
    end;
    if GTools.RefreshExecution.Status <> nesSucceeded then
    begin
      raise Exception.Create('Actual program presentation failed: ' + GTools.RefreshExecution.Failure);
    end;
  end;

  if GStage >= 39 then
  begin
    NetworkStage;
    Exit;
  end;
  case GStage of
    0:
      begin

        if not GInitialized then
        begin
          Initialize;

          if JourneyTimedOut then
          begin
            raise Exception.Create('JSON Workshop setup timed out');
          end;
          GInitialized := True;
          GInteractionStarted := Clock;
          Exit;
        end;

        if not GNodes[0].PipelineNodesVisible or not GNodes[1].PipelineNodesVisible then
        begin
          Exit;
        end;
        Check(GNodes[0].PipelineNodesVisible and GNodes[1].PipelineNodesVisible,
          'both real operation-card presentations have published');

        if not CheckUniformOperations then
        begin
          Exit;
        end;
        TypeSource(0, CExactJSON);
        Click(0, jwcFormat);
      end;
    1:
      begin

        if GApps[0].State.GetValue(JSONWorkshopPreview) <> GPretty then
        begin
          Exit;
        end;
        Check(GApps[1].State.GetValue(JSONWorkshopSource) = JSONWorkshopSample,
          'embedded editing remains independent in a reopened peer application');
        Check((Pos('9007199254740993', GPretty) > 0) and (Pos('River 🌙', GPretty) > 0),
          'real formatting preserves exact large numbers and supplementary Unicode');
        {$ifndef PAS2JS}

        if GUniformOperationsFitted then
        begin
          SaveNyxNativeCapture(GApps[0].Window,
            GDirectory + 'uniform-workshop-mixed-formatted.png', ncmPrint);
        end;
        {$endif}
        {$ifdef PAS2JS}

        if Pos('&review=uniform&', '&' + Copy(window.location.search, 2,
          Length(window.location.search)) + '&') > 0 then
        begin
          { A selective visual review stops after real formatting and uniform
            Fit, before the separate file/program/network regression stages.
            It grants no full-journey acceptance and leaves controls interactive. }
          GUniformReviewReady := True;
          document.body.setAttribute('data-uniform-review', 'ready');
          Exit;
        end;
        {$endif}
        TypeSource(0, FencedJSON);
        Click(0, jwcSave);
      end;
    2:
      begin

        if GHosts[0].Calls <> 1 then
        begin
          Exit;
        end;
        Check(NyxDecodeUTF8(GHosts[0].Request.Payload) = GPretty,
          'embedded control event reaches the configured host with exact formatted UTF-8 bytes');
        GHosts[0].Request.Finish(nfsoCancelled);
      end;
    3:
      begin

        if not GApps[0].State.GetValue(NyxBooleanState('save-cancelled')) then
        begin
          Exit;
        end;
        Check(not GApps[0].State.GetValue(NyxBooleanState('save-written')) and
          (GApps[0].State.GetValue(JSONWorkshopSource) = FencedJSON),
          'user cancellation preserves the original fenced input without publishing preparation into it');
        Click(0, jwcSave);
      end;
    4:
      begin

        if GHosts[0].Calls <> 2 then
        begin
          Exit;
        end;
        GHosts[0].Request.Finish(nfsoWritten);
      end;
    5:
      begin

        if not GApps[0].State.GetValue(NyxBooleanState('save-written')) then
        begin
          Exit;
        end;
        Check(not GApps[0].State.GetValue(NyxBooleanState('save-cancelled')),
          'a later write outcome replaces the prior cancellation flags');
        TypeSource(0, '{invalid');
        Click(0, jwcSave);
      end;
    6:
      begin

        if RunReport(0).Field('failed').AsInteger = 0 then
        begin
          Exit;
        end;
        Check(RunReport(0).Field('failure').AsText <> '',
          'invalid JSON has an actual completed failure diagnostic');
      end;
    7:
      begin
        Check((GHosts[0].Calls = 2) and
          (GApps[0].State.GetValue(JSONWorkshopPreview) = GPretty),
          'invalid JSON cannot request saving or replace the last accepted preview');
        TypeSource(0, PlainFence);
        Click(0, jwcSave);
      end;
    8:
      begin

        if GHosts[0].Calls <> 3 then
        begin
          Exit;
        end;
        Check(GHosts[0].Request.IsPending, 'the current save request is pending before page retirement');
        GApps[0].ShowPage('away');
      end;
    9:
      begin
        Check(not GHosts[0].Request.IsPending, 'page navigation cancels the embedded pending file request');
        GHosts[0].Request.Finish(nfsoWritten);
        TypeSource(1, CExactJSON);
        Click(1, jwcSave);
      end;
    10:
      begin

        if GHosts[1].Calls <> 1 then
        begin
          Exit;
        end;
        Check(NyxDecodeUTF8(GHosts[1].Request.Payload) = GPretty,
          'the reopened peer remains fully usable after the first App retires its canvas');
        GHosts[1].Request.Finish(nfsoDownloadRequested);
      end;
    11:
      begin

        if not GApps[1].State.GetValue(NyxBooleanState('save-delivered')) then
        begin
          Exit;
        end;
        Check(not GApps[1].State.GetValue(NyxBooleanState('save-written')),
          'download-requested does not claim a disk write');
        Check((TNyxCodec.Encode(GDocument) = GBefore) and
          (TNyxCodec.Encode(GReopened) = GBefore),
          'all runtime editing, graph results and retirement preserve both authored documents');
        GTools := GApps[1].View.ToolCanvasView(JSONWorkshopCanvas.ID);
        { Receive interface queries in local leases before retaining globals.
          pas2js otherwise emits final release of those global out arguments at
          this procedure's return, prematurely retiring the still-visible tool. }
        Check(Supports(GTools, TReviewTools, LToolFace), 'the reopened App exposes its actual Tool Canvas');
        Check(Supports(GTools.Inspector, INyxToolCanvasProgramAuthoring, LProgramAuthoring),
          'the useful tool exposes its ordinary interpreted-program workbench');
        Check(Supports(GViews[1], INyxCanvasProgramEditing, LProgramEditing),
          'the useful tool exposes managed runtime program ownership');
        GToolFace := LToolFace;
        GProgramAuthoring := LProgramAuthoring;
        GProgramEditing := LProgramEditing;
        GProgramAuthoring.SelectProgram(JSONWorkshopPreparation);
        GTools.Refresh;
      end;
    12:
      begin
        Check(not GProgramAuthoring.HasPendingProgram, 'opening the saved preparation has no proposal');
        TypeProgram(CReplacement);
        ClickTool('program-apply');
      end;
    13:
      begin
        Check(GProgramEditing.RuntimePrograms.Definition(JSONWorkshopPreparation).Source = CReplacement,
          'actual Apply replaces the live interpreted source');
        Click(1, jwcFormat);
      end;
    14:
      begin

        if GApps[1].State.GetValue(JSONWorkshopPreview) <> GCustomPretty then
        begin
          Exit;
        end;
        Check(GApps[1].State.GetValue(JSONWorkshopSource) = CExactJSON,
          'custom preparation changes the result without overwriting entered JSON');
        GAcceptedPrograms := GProgramEditing.RuntimePrograms.ToData.ToJSON;
        TypeProgram(CUnfinished);
        ClickTool('program-check');
      end;
    15:
      begin
        Check(Pos('Line ', GToolFace.Renderer.Root.Find(ToolID('program-feedback')).Prop('text')) = 1,
          'an invalid script has an actual source-line diagnostic beside its editor');
        ClickTool('program-apply');
      end;
    16:
      begin
        Check((GProgramEditing.RuntimePrograms.ToData.ToJSON = GAcceptedPrograms) and
          GProgramAuthoring.HasPendingProgram, 'invalid Apply preserves the complete accepted program and draft');
        ClickTool('tab-ports');
      end;
    17: ClickTool('tab-programs');
    18:
      begin
        Check(GToolFace.Renderer.Root.Find(ToolID('program-source')).Prop('value') = CUnfinished,
          'the exact unfinished source survives ordinary tab navigation');
        Click(1, jwcSave);
      end;
    19:
      begin

        if GHosts[1].Calls <> 2 then
        begin
          Exit;
        end;
        Check(NyxDecodeUTF8(GHosts[1].Request.Payload) = GCustomPretty,
          'Save still executes the last accepted custom program while an invalid draft is open');
        GHosts[1].Request.Finish(nfsoWritten);
      end;
    20:
      begin

        if not GApps[1].State.GetValue(NyxBooleanState('save-written')) then
        begin
          Exit;
        end;
        Check((TNyxCodec.Encode(GDocument) = GBefore) and
          (TNyxCodec.Encode(GReopened) = GBefore),
          'runtime custom source and invalid drafts preserve both portable project blueprints');
        GApps[0].ShowPage('home');
      end;
    21:
      begin
        GViews[0] := GApps[0].View.CanvasView(
          GApps[0].View.Root.Find(JSONWorkshopCanvas.ID).Part(NyxPart('workspace')).ID) as TReviewCanvas;
        TypeSource(0, '{"peer":"Still independent 🌙"}');
        Click(0, jwcFormat);
      end;
    22:
      begin

        if GApps[0].State.GetValue(JSONWorkshopPreview) <> GPeerPretty then
        begin
          Exit;
        end;
        Check((GApps[1].State.GetValue(JSONWorkshopPreview) = GCustomPretty) and
          (GHosts[0].Calls = 3) and (GHosts[1].Calls = 2),
          'the other App returns with its original program and independent result, without extra file effects');
        GOpenSourceBefore := GApps[0].State.GetValue(JSONWorkshopSource);
        GOpenPreviewBefore := GApps[0].State.GetValue(JSONWorkshopPreview);
        GOpenRevision := GApps[0].State.Revision;
        Click(0, jwcOpen);
      end;
    23:
      begin

        if GOpenHosts[0].Calls <> 1 then
        begin
          Exit;
        end;
        Check(GOpenHosts[0].Request.IsPending and
          (GApps[0].State.Revision = GOpenRevision),
          'opening is pending without replacing the working document');
        GOpenHosts[0].Request.Finish(nfooCancelled);
      end;
    24:
      begin

        if not OpenFinished(0, 1) then
        begin
          Exit;
        end;
        Check((GApps[0].State.GetValue(JSONWorkshopSource) = GOpenSourceBefore) and
          (GApps[0].State.GetValue(JSONWorkshopPreview) = GOpenPreviewBefore) and
          (GApps[0].State.GetValue(JSONWorkshopOpenedName) = '') and
          GApps[0].State.GetValue(NyxBooleanState('open-cancelled')),
          'cancel skips decode/parse and preserves source, preview and accepted file name');
        Check((RunReport(0, jwcOpen).Field('failed').AsInteger = 0) and
          (GApps[0].State.Revision = GOpenRevision + 1),
          'cancel publishes one honest outcome without inventing a parse failure');
        GOpenRevision := GApps[0].State.Revision;
        Click(0, jwcOpen);
      end;
    25:
      begin

        if GOpenHosts[0].Calls <> 2 then
        begin
          Exit;
        end;
        GOpenHosts[0].Request.Finish(nfooRead, OpenSnapshot('broken.json', '{invalid'));
      end;
    26:
      begin

        if not OpenFinished(0, 2) then
        begin
          Exit;
        end;
        Check((RunReport(0, jwcOpen).Field('failed').AsInteger = 1) and
          (RunReport(0, jwcOpen).Field('failure').AsText <> '') and
          (GApps[0].State.Revision = GOpenRevision),
          'invalid JSON rejects the entire selected read with an inspectable diagnostic');
        Check((GApps[0].State.GetValue(JSONWorkshopSource) = GOpenSourceBefore) and
          (GApps[0].State.GetValue(JSONWorkshopPreview) = GOpenPreviewBefore) and
          (GApps[0].State.GetValue(JSONWorkshopOpenedName) = ''),
          'a parser failure publishes no source, preview or filename prefix');
        Click(0, jwcOpen);
      end;
    27:
      begin

        if GOpenHosts[0].Calls <> 3 then
        begin
          Exit;
        end;
        GOpenHosts[0].Request.Finish(nfooRead, InvalidUTF8Snapshot);
      end;
    28:
      begin

        if not OpenFinished(0, 3) then
        begin
          Exit;
        end;
        Check((RunReport(0, jwcOpen).Field('failed').AsInteger = 2) and
          (GApps[0].State.Revision = GOpenRevision),
          'malformed UTF-8 refuses before any selected output reaches application state');
        Click(0, jwcOpen);
      end;
    29:
      begin

        if GOpenHosts[0].Calls <> 4 then
        begin
          Exit;
        end;
        Check((GOpenHosts[0].Options.ByteLimit = 1024 * 1024) and
          (GOpenHosts[0].Options.ExtensionCount = 1) and
          (GOpenHosts[0].Options.Extension(0) = 'json'),
          'the real saved operation supplies its explicit byte budget and extension hint');
        GOpenHosts[0].Request.Finish(nfooRead, OpenSnapshot(COpenedName, LoadedJSON));
      end;
    30:
      begin

        if not OpenFinished(0, 4) then
        begin
          Exit;
        end;
        Check((RunReport(0, jwcOpen).Field('succeeded').AsInteger = 2) and
          (GApps[0].State.GetValue(JSONWorkshopSource) = LoadedJSON) and
          (GApps[0].State.GetValue(JSONWorkshopPreview) = GOpenPretty) and
          (GApps[0].State.GetValue(JSONWorkshopOpenedName) = COpenedName),
          'a complete BOM/fenced UTF-8 file opens with exact source, values and supplementary filename');
        Check(GApps[0].State.GetValue(NyxBooleanState('open-read')) and
          not GApps[0].State.GetValue(NyxBooleanState('open-cancelled')) and
          (GApps[0].State.Revision = GOpenRevision + 1),
          'one admitted read replaces its complete source/preview/name and status frame atomically');
        Check((GApps[1].State.GetValue(JSONWorkshopSource) = CExactJSON) and
          (GApps[1].State.GetValue(JSONWorkshopPreview) = GCustomPretty),
          'file input remains independent from the other App and its custom program');
        GOpenRevision := GApps[0].State.Revision;
        Click(0, jwcOpen);
      end;
    31:
      begin

        if GOpenHosts[0].Calls <> 5 then
        begin
          Exit;
        end;
        GOpenHosts[0].Request.Finish(nfooCancelled);
      end;
    32:
      begin

        if not OpenFinished(0, 5) then
        begin
          Exit;
        end;
        Check((GApps[0].State.GetValue(JSONWorkshopSource) = LoadedJSON) and
          (GApps[0].State.GetValue(JSONWorkshopPreview) = GOpenPretty) and
          (GApps[0].State.GetValue(JSONWorkshopOpenedName) = COpenedName) and
          (GApps[0].State.Revision = GOpenRevision + 1),
          'a later cancellation preserves the previously accepted file name as well as its content');
        Click(0, jwcOpen);
      end;
    33:
      begin

        if GOpenHosts[0].Calls <> 6 then
        begin
          Exit;
        end;
        TypeSource(0, CNewerJSON);
        GOpenRevision := GApps[0].State.Revision;
        GOpenHosts[0].Request.Finish(nfooRead, OpenSnapshot('stale.json', CExactJSON));
      end;
    34:
      begin

        if not OpenFinished(0, 6) then
        begin
          Exit;
        end;
        Check((GApps[0].State.GetValue(JSONWorkshopSource) = CNewerJSON) and
          (GApps[0].State.GetValue(JSONWorkshopPreview) = GOpenPretty) and
          (GApps[0].State.GetValue(JSONWorkshopOpenedName) = COpenedName) and
          (GApps[0].State.Revision = GOpenRevision),
          'a late read cannot overwrite newer entered text or publish a partial filename/status frame');
        Click(0, jwcOpen);
      end;
    35:
      begin

        if GOpenHosts[0].Calls <> 7 then
        begin
          Exit;
        end;
        Check(GOpenHosts[0].Request.IsPending, 'an actual open request is pending before navigation');
        GApps[0].ShowPage('away');
      end;
    36:
      begin
        Check(not GOpenHosts[0].Request.IsPending, 'page retirement cancels its pending file read');
        GOpenHosts[0].Request.Finish(nfooRead, OpenSnapshot('late.json', CExactJSON));
        Check(GApps[0].State.GetValue(JSONWorkshopSource) = CNewerJSON,
          'retired host completion cannot replace the preserved entered text');
        Click(1, jwcOpen);
      end;
    37:
      begin

        if GOpenHosts[1].Calls <> 1 then
        begin
          Exit;
        end;
        GOpenHosts[1].Request.Finish(nfooRead, OpenSnapshot('Second.json', CExactJSON));
      end;
    38:
      begin

        if not OpenFinished(1, 1) then
        begin
          Exit;
        end;
        Check(GApps[1].State.GetValue(NyxBooleanState('open-read')) and
          (GApps[1].State.GetValue(JSONWorkshopOpenedName) = 'Second.json'),
          'the independent reopened App still completes its own file read');
        Check(GApps[1].State.GetValue(JSONWorkshopPreview) = GOpenCustomPretty,
          'Open executes the accepted custom program with its own saved formatting policy');
        Check((GHosts[0].Calls = 3) and (GHosts[1].Calls = 2) and
          (TNyxCodec.Encode(GDocument) = GBefore) and (TNyxCodec.Encode(GReopened) = GBefore),
          'all open outcomes preserve portable blueprints and perform no unintended saving');
      end;
  end;
  Inc(GStage);
end;

procedure Cleanup;
var
  LIndex: Integer;
begin
  {$ifdef NYX_JSON_WORKSHOP_STUDIO}GStudioJourney := nil;{$endif}
  GProgramEditing := nil;
  GProgramAuthoring := nil;
  GToolFace := nil;
  GTools := nil;
  for LIndex := 0 to 1 do
  begin
    FreeAndNil(GApps[LIndex]);
    GNodes[LIndex] := nil;
    GViews[LIndex] := nil;
    GHostLeases[LIndex] := nil;
    GHosts[LIndex] := nil;
    GOpenLeases[LIndex] := nil;
    GOpenHosts[LIndex] := nil;
    GNetworkLeases[LIndex] := nil;
    GNetworkHosts[LIndex] := nil;
    {$ifdef PAS2JS}

    if GElements[LIndex] <> nil then
    begin
      GElements[LIndex].remove;
    end;
    {$endif}
  end;
  FreeAndNil(GReopened);
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    document.body.setAttribute('data-json-workshop-stage', IntToStr(GStage));

    if {$ifdef NYX_JSON_WORKSHOP_STUDIO}GStudioReady and {$endif}JourneyTimedOut then
    begin
      raise Exception.Create('JSON Workshop timed out at stage ' + IntToStr(GStage));
    end;
    Stage;

    if GUniformReviewReady then
    begin
      Exit;
    end;

    if GDone then
    begin
      Cleanup;
      document.body.setAttribute('data-json-workshop-checks', IntToStr(GChecks));
    end
    else
    begin
      window.setTimeout(@Drive, 10);
    end;
  except
    on E: Exception do
    begin
      { Retain the failed browser frame for inspection. Cleanup can itself
        enter platform callbacks and must not hide the original diagnostic.
        No further Drive timer is scheduled; page teardown owns final release. }
      document.body.setAttribute('data-json-workshop-error', E.Message);
    end;
    else
    begin
      document.body.setAttribute('data-json-workshop-error',
        'Unexpected platform exception; inspect the retained browser console');
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GStarted := TJSDate.now;
  Drive;
  {$else}
  try

    if ParamCount <> {$ifdef NYX_COMPILED_JSON_WORKSHOP}3{$else}2{$endif} then
    begin
      raise Exception.Create('Supply an existing external evidence directory and client width');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
    GWidth := StrToInt(ParamStr(2));
    Application.Initialize;
    GFailures := TFailures.Create;
    Application.OnException := GFailures.Failed;
    GStarted := GetTickCount64;
    try
      while not GDone do
      begin

        if GLoggedStage <> GStage then
        begin
          WriteLn('JSON Workshop stage ', GStage, ' at ', GetTickCount64 - GStarted, 'ms');
          Flush(Output);
          GLoggedStage := GStage;
        end;
        CheckSynchronize(1);
        Application.ProcessMessages;

        if {$ifdef NYX_JSON_WORKSHOP_STUDIO}GStudioReady and {$endif}JourneyTimedOut then
        begin
          raise Exception.Create('JSON Workshop timed out at stage ' + IntToStr(GStage));
        end;
        Stage;
        Sleep(1);
      end;
      WriteLn('PASS ', GChecks, ' JSON Workshop actual-control checks at ', GWidth);
    finally
      Cleanup;
      Application.OnException := nil;
      GFailures.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
