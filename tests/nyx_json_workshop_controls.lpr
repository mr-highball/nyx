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
  nyx.state, nyx.logic, nyx.files.save, nyx.scheduler,
  nyx.json.workshop, nyx.tool.canvas, nyx.canvas.types, nyx.canvas.workspace,
  nyx.flow.routing, nyx.flow.declarations, nyx.view.behavior,
  {$ifdef NYX_JSON_WORKSHOP_STUDIO}nyx.test.json.workshop.studio,{$endif}
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser, nyx.canvas.browser
  {$else}
  Classes, Interfaces, Forms, Controls, StdCtrls, nyx.application.lcl, nyx.canvas.lcl,
  nyx.files.lcl
  {$endif}
  {$ifdef NYX_COMPILED_JSON_WORKSHOP}, nyx.generated.json.workshop{$endif};

const
  CExactJSON: TNyxText = '{"name":"River 🌙","count":9007199254740993,"note":"line\u0000end"}';

type
  {$ifdef PAS2JS}
  { The test uses the ordinary browser application and its real embedded inputs. }
  TReviewApplication = TNyxBrowserApplication;
  { A retained presentation capability never owns or reparents borrowed DOM. }
  TReviewCanvas = INyxBrowserCanvasWorkspaceView;
  {$else}
  { The test uses the ordinary native application and its real embedded inputs. }
  TReviewApplication = TNyxLCLApplication;
  { A retained presentation capability never owns or reparents borrowed widgets. }
  TReviewCanvas = INyxLCLCanvasWorkspaceView;
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

var
  GDocument: TNyxDocument;
  GReopened: TNyxDocument;
  GApps: array[0..1] of TReviewApplication;
  GViews: array of TReviewCanvas;
  GHosts: array[0..1] of TSaveHost;
  GHostLeases: array of INyxFileSaveHost;
  GNodes: array of INyxToolCanvasPipelineNodes;
  GBefore: TNyxText;
  GPretty: TNyxText;
  GStage: Integer;
  GChecks: Integer;
  GDone: Boolean;
  GInitialized: Boolean;
  GWidth: Integer = 1280;
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

{$ifndef PAS2JS}
procedure TFailures.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

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

procedure Click(AApp: Integer; AControl: TJSONWorkshopControl);
begin
  {$ifdef PAS2JS}
  GViews[AApp].RendererFor(JSONWorkshopItem).ElementFor(JSONWorkshopControl(AControl).ID).click;
  {$else}
  TButton(GViews[AApp].RendererFor(JSONWorkshopItem).ControlFor(JSONWorkshopControl(AControl).ID)).Click;
  {$endif}
end;

{ Copied diagnostic counters establish actual run completion. A timer tick alone
  cannot prove that a rejected asynchronous graph has finished without effects. }
function SaveReport(AApp: Integer): TNyxDataValue;
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

    if Result.Field('owner').AsText = JSONWorkshopControl(jwcSave).ID then
    begin
      Exit;
    end;
  end;
  raise Exception.Create('The embedded Save route has no diagnostic record');
end;

procedure Initialize;
var
  LIndex: Integer;
  LPair: TNyxProjectPair;
  LResolved: TNyxProjectPair;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LTools: INyxToolCanvasView;
  LDefinition: INyxFlowDefinition;
  LOperation: INyxLogicOperation;
  LFormatter: INyxJSONFormat;
begin
  SetLength(GViews, 2);
  SetLength(GHostLeases, 2);
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
  LDefinition := GDocument.Flows.Definition(NyxFlowRef('Format JSON'));
  LOperation := NyxLogicFromData(LDefinition.Descriptor(1).Kind, LDefinition.Configuration(1));
  Check(Supports(LOperation, INyxJSONFormat, LFormatter),
    'the saved formatter is reconstructed through its specialized typed interface');
  GPretty := LFormatter.Format(NewNyxJSONParse.Parse(CExactJSON));
  for LIndex := 0 to 1 do
  begin
    GHosts[LIndex] := TSaveHost.Create;
    GHostLeases[LIndex] := GHosts[LIndex];
    GApps[LIndex] := TReviewApplication.Create;
    GApps[LIndex].ConfigureFileSaving(GHostLeases[LIndex]);
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
    ShowJSONWorkshopGraph(LTools);
    Check(Supports(LTools, INyxToolCanvasPipelineNodes, GNodes[LIndex]),
      'the example exposes its managed operation-card capability');
  end;
end;

procedure Stage;
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
  case GStage of
    0:
      begin

        if not GInitialized then
        begin
          Initialize;
          GInitialized := True;
          Exit;
        end;

        if not GNodes[0].PipelineNodesVisible or not GNodes[1].PipelineNodesVisible then
        begin
          Exit;
        end;
        Check(GNodes[0].PipelineNodesVisible and GNodes[1].PipelineNodesVisible,
          'both real operation-card presentations have published');
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
          (GApps[0].State.GetValue(JSONWorkshopSource) = CExactJSON),
          'user cancellation publishes an honest outcome and preserves the source');
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

        if SaveReport(0).Field('failed').AsInteger = 0 then
        begin
          Exit;
        end;
        Check(SaveReport(0).Field('failure').AsText <> '',
          'invalid JSON has an actual completed failure diagnostic');
      end;
    7:
      begin
        Check((GHosts[0].Calls = 2) and
          (GApps[0].State.GetValue(JSONWorkshopPreview) = GPretty),
          'invalid JSON cannot request saving or replace the last accepted preview');
        TypeSource(0, CExactJSON);
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
        GDone := True;
      end;
  end;
  Inc(GStage);
end;

procedure Cleanup;
var
  LIndex: Integer;
begin
  {$ifdef NYX_JSON_WORKSHOP_STUDIO}GStudioJourney := nil;{$endif}
  for LIndex := 0 to 1 do
  begin
    FreeAndNil(GApps[LIndex]);
    GNodes[LIndex] := nil;
    GViews[LIndex] := nil;
    GHostLeases[LIndex] := nil;
    GHosts[LIndex] := nil;
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

    if {$ifdef NYX_JSON_WORKSHOP_STUDIO}GStudioReady and {$endif}(TJSDate.now - GStarted > 60000) then
    begin
      raise Exception.Create('JSON Workshop timed out at stage ' + IntToStr(GStage));
    end;
    Stage;

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
      Cleanup;
      document.body.setAttribute('data-json-workshop-error', E.Message);
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
        CheckSynchronize(1);
        Application.ProcessMessages;

        if {$ifdef NYX_JSON_WORKSHOP_STUDIO}GStudioReady and {$endif}(GetTickCount64 - GStarted > 60000) then
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
