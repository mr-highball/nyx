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
{ Network input continues the same real-control consumer after its complete
  Open/Format/Save and live-Pascal journey. Controlled replies prove atomic and
  retirement behavior. An explicitly supplied native qualification origin also
  exercises the actual platform transport, not a simulated success result. }

procedure TypeURL(const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GViews[0].InputFor(JSONWorkshopItem,
    JSONWorkshopControl(jwcURL))).value := AText;
  TJSHTMLElement(GViews[0].InputFor(JSONWorkshopItem,
    JSONWorkshopControl(jwcURL))).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(GViews[0].InputFor(JSONWorkshopItem,
    JSONWorkshopControl(jwcURL))).Text := AText;
  {$endif}
end;

function NetworkFinished(ACount: Integer): Boolean;
var
  LReport: TNyxDataValue;
begin
  LReport := RunReport(0, jwcFetch);
  Result := (LReport.Field('started').AsInteger = ACount) and
    (LReport.Field('active').AsInteger = 0);
end;

function HTTPStatusCaption: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GViews[0].RendererFor(JSONWorkshopItem).ElementFor(
    JSONWorkshopControl(jwcHTTPStatus).ID).textContent;
  {$else}
  Result := TLabel(GViews[0].RendererFor(JSONWorkshopItem).ControlFor(
    JSONWorkshopControl(jwcHTTPStatus).ID)).Caption;
  {$endif}
end;

procedure CheckNetworkContent;
begin
  Check((GApps[0].State.GetValue(JSONWorkshopSource) = LoadedJSON) and
    (GApps[0].State.GetValue(JSONWorkshopPreview) = GOpenPretty) and
    (GApps[0].State.GetValue(JSONWorkshopOpenedName) = GNetworkURL),
    'unsuccessful network input preserves the entire previously accepted notebook');
end;

{ Read the actual saved request through its specialized operation contract.
  Imported Workshop projects may deliberately change headers/deadline/limits;
  comparing the complete derived request keeps those options under assertion
  without imposing this repository's starter defaults on an accepted project. }
function SavedNetworkRequest: TNyxHTTPRequest;
var
  LDefinition: INyxFlowDefinition;
  LIndex: Integer;
  LOperation: INyxLogicOperation;
  LHTTP: INyxHTTPCall;
begin
  LDefinition := GDocument.Flows.Definition(JSONWorkshopFetchFlow);
  for LIndex := 0 to LDefinition.StepCount - 1 do
  begin

    if LDefinition.Step(LIndex).Name = JSONWorkshopStep(jwsHTTP).Name then
    begin
      LOperation := NewNyxHTTPLogicResolver(nil).Resolve(
        LDefinition.Descriptor(LIndex).Kind, LDefinition.Configuration(LIndex));
      Check(Supports(LOperation, INyxHTTPCall, LHTTP),
        'the saved network step retains its specialized request contract');
      Exit(LHTTP.Request);
    end;
  end;
  raise Exception.Create('The saved Workshop has no HTTP request');
end;

procedure NetworkStage;
var
  LBytes: TNyxBytes;
  LOrigin: TNyxText;
  LJSON: TNyxDataValue;
begin
  case GStage of
    39:
      begin
        GInteractionStarted := Clock;
        GApps[0].ShowPage('home');
        GViews[0] := GApps[0].View.CanvasView(
          GApps[0].View.Root.Find(JSONWorkshopCanvas.ID).Part(NyxPart('workspace')).ID) as TReviewCanvas;
        TypeURL('');
        GNetworkRevision := GApps[0].State.Revision;
        Click(0, jwcFetch);
      end;
    40:
      begin

        if not NetworkFinished(1) then
        begin
          Exit;
        end;
        Check((GNetworkHosts[0].Calls = 0) and
          (RunReport(0, jwcFetch).Field('failed').AsInteger = 1) and
          (GApps[0].State.Revision = GNetworkRevision),
          'empty URL refuses before Send without using the configured fallback or publishing a prefix');
        GNetworkURL := 'https://example.invalid/notebook?name=River%20Moon';
        TypeURL(GNetworkURL);
        Click(0, jwcFetch);
      end;
    41:
      begin

        if GNetworkHosts[0].Calls <> 1 then
        begin
          Exit;
        end;
        Check(GNetworkHosts[0].Request.IsPending and
          (GNetworkHosts[0].Last.ToData.ToJSON = SavedNetworkRequest.WithURL(
            NyxResourceURL(GNetworkURL)).ToData.ToJSON),
          'the actual URL input reaches a pending request while retaining all configured options');
        GNetworkHosts[0].Request.Finish(200, NyxEncodeUTF8(LoadedJSON));
      end;
    42:
      begin

        if not NetworkFinished(2) then
        begin
          Exit;
        end;
        CheckNetworkContent;
        Check((GApps[0].State.GetValue(JSONWorkshopHTTPStatus) = 200) and
          (HTTPStatusCaption = '200') and
          (GApps[0].State.GetValue(NyxTextState('fetch-error')) = ''),
          'HTTP status reaches the actual integer-bound label with the complete formatted frame');
        Check((GNetworkHosts[1].Calls = 0) and
          (GApps[1].State.GetValue(JSONWorkshopPreview) = GOpenCustomPretty),
          'network loading cannot send through or replace the independent peer application');
        TypeURL('https://example.invalid/missing');
        Click(0, jwcFetch);
      end;
    43:
      begin

        if GNetworkHosts[0].Calls <> 2 then
        begin
          Exit;
        end;
        { Invalid bytes on an unsuccessful response must never enter decoding. }
        SetLength(LBytes, 1);
        LBytes[0] := 255;
        GNetworkHosts[0].Request.Finish(404, LBytes);
      end;
    44:
      begin

        if not NetworkFinished(3) then
        begin
          Exit;
        end;
        CheckNetworkContent;
        Check((GApps[0].State.GetValue(JSONWorkshopHTTPStatus) = 404) and
          (HTTPStatusCaption = '404') and
          (RunReport(0, jwcFetch).Field('failed').AsInteger = 1),
          'non-2xx is usable status data and skips invalid response-body decoding');
        Click(0, jwcFetch);
      end;
    45:
      begin

        if GNetworkHosts[0].Calls <> 3 then
        begin
          Exit;
        end;
        SetLength(LBytes, 1);
        LBytes[0] := 255;
        GNetworkHosts[0].Request.Finish(200, LBytes);
      end;
    46:
      begin

        if not NetworkFinished(4) then
        begin
          Exit;
        end;
        CheckNetworkContent;
        Check((GApps[0].State.GetValue(JSONWorkshopHTTPStatus) = 404) and
          (RunReport(0, jwcFetch).Field('failed').AsInteger = 2),
          'invalid successful UTF-8 refuses atomically, including the status output');
        Click(0, jwcFetch);
      end;
    47:
      begin

        if GNetworkHosts[0].Calls <> 4 then
        begin
          Exit;
        end;
        GNetworkHosts[0].Request.Finish(200, NyxEncodeUTF8('{invalid'));
      end;
    48:
      begin

        if not NetworkFinished(5) then
        begin
          Exit;
        end;
        CheckNetworkContent;
        Check((GApps[0].State.GetValue(JSONWorkshopHTTPStatus) = 404) and
          (RunReport(0, jwcFetch).Field('failed').AsInteger = 3),
          'invalid JSON publishes no source, filename, preview or status prefix');
        Click(0, jwcFetch);
      end;
    49:
      begin

        if GNetworkHosts[0].Calls <> 5 then
        begin
          Exit;
        end;
        GNetworkHosts[0].Request.Finish(0, nil, 'The network is offline');
      end;
    50:
      begin

        if not NetworkFinished(6) then
        begin
          Exit;
        end;
        CheckNetworkContent;
        Check((GApps[0].State.GetValue(JSONWorkshopHTTPStatus) = 0) and
          (GApps[0].State.GetValue(NyxTextState('fetch-error')) = 'The network is offline'),
          'transport failure reports usable error data while retaining the notebook');
        Click(0, jwcFetch);
      end;
    51:
      begin

        if GNetworkHosts[0].Calls <> 6 then
        begin
          Exit;
        end;
        TypeSource(0, CNewerJSON);
        GNetworkRevision := GApps[0].State.Revision;
        GNetworkHosts[0].Request.Finish(200, NyxEncodeUTF8(CExactJSON));
      end;
    52:
      begin

        if not NetworkFinished(7) then
        begin
          Exit;
        end;
        Check((GApps[0].State.GetValue(JSONWorkshopSource) = CNewerJSON) and
          (GApps[0].State.GetValue(JSONWorkshopPreview) = GOpenPretty) and
          (GApps[0].State.GetValue(JSONWorkshopOpenedName) = GNetworkURL) and
          (GApps[0].State.Revision = GNetworkRevision),
          'late HTTP completion cannot overwrite newer entered JSON or publish a partial result');
        Click(0, jwcFetch);
      end;
    53:
      begin

        if GNetworkHosts[0].Calls <> 7 then
        begin
          Exit;
        end;
        Check(GNetworkHosts[0].Request.IsPending, 'HTTP is pending before page retirement');
        GApps[0].ShowPage('away');
        GNetworkRevision := GApps[0].State.Revision;
      end;
    54:
      begin
        Check(not GNetworkHosts[0].Request.IsPending, 'page retirement revokes its HTTP receiver');
        GNetworkHosts[0].Request.Finish(200, NyxEncodeUTF8(CExactJSON));
        Check((GApps[0].State.GetValue(JSONWorkshopSource) = CNewerJSON) and
          (GApps[0].State.Revision = GNetworkRevision),
          'retired HTTP completion cannot publish to the application');
        {$ifdef PAS2JS}
        LOrigin := '';
        {$else}
        LOrigin := TNyxText(GetEnvironmentVariable('NYX_JSON_WORKSHOP_HTTP_ORIGIN'));
        {$endif}

        if LOrigin = '' then
        begin
          Inc(GStage);
        end
        else
        begin
          {$ifndef PAS2JS}
          GNetworkHosts[0].Delegate := NewNyxNativeHTTPTransport(GApps[0].View.Events.Scheduler);
          {$endif}
          GApps[0].ShowPage('home');
          GViews[0] := GApps[0].View.CanvasView(
            GApps[0].View.Root.Find(JSONWorkshopCanvas.ID).Part(NyxPart('workspace')).ID) as TReviewCanvas;
          TypeURL(LOrigin + '/json');
          Click(0, jwcFetch);
        end;
      end;
    55:
      begin

        if not NetworkFinished(1) then
        begin
          Exit;
        end;
        LJSON := TNyxDataValue.ParseJSON(GApps[0].State.GetValue(JSONWorkshopPreview));
        Check((GNetworkHosts[0].Calls = 8) and
          (GApps[0].State.GetValue(JSONWorkshopHTTPStatus) = 200) and
          (LJSON.Field('caption').AsText = 'Hello from HTTP') and
          (LJSON.Field('items').Item(0).Field('rank').ToJSON = '9007199254740993'),
          'actual native HTTP bytes reach the control-bound source, preparation and exact-number preview');
        Check(GApps[0].State.GetValue(JSONWorkshopOpenedName) =
          GNetworkHosts[0].Last.URL.Address,
          'the accepted real network target reaches the visible notebook name');
      end;
    56:
      begin
        Check((TNyxCodec.Encode(GDocument) = GBefore) and
          (TNyxCodec.Encode(GReopened) = GBefore) and
          (GHosts[0].Calls = 3) and (GHosts[1].Calls = 2),
          'all network paths preserve immutable project blueprints and perform no unintended file effects');
        GDone := True;
      end;
  end;
  Inc(GStage);
end;
