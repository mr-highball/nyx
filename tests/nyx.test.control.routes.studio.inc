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

const
  CControlForm = 'studio-logic-editor-control-routes';

var
  GControlPublished: TNyxProjectPair;
  GControlRemoved: TNyxProjectPair;
  GControlPending: TNyxProjectPair;
  GControlStepStarted: Double;

{ Seed through the same semantic transaction engine in an explicitly owned local
  context. The separate authenticated consumer qualifies transport ownership.
  The fixture removes the pre-existing button route; UI must author the new
  input-event/script/control path rather than merely toggling a prepared one. }
function ControlRouteSeed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LAgent: TNyxAgentSession;
begin
  LDocument := ProgramToolCanvasDocument;
  LAgent := nil;
  try
    LAgent := TNyxAgentSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument)));
    LAgent.Call('nyx_transaction', 'Prepare unwired Tool Canvas input', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('prepare-control-route-input')),
      NyxField('operations', NyxProjectTransaction([
        NyxDesignStep(NyxFlowRoutesPatch(NyxControl('greet-button'),
          NewNyxFlowRoutes))]).ToData)]));
    Result := LAgent.PreviewPair(LAgent.Revision, 'home');
  finally
    LAgent.Free;
    LDocument.Free;
  end;
end;

function OpenControlTemplate: Boolean;
var
  LVisible: Boolean;
begin
  Result := False;

  if GStudio.ShellView.RootFor('action-logic-close') <> nil then
  begin
    { Logic intentionally replaces the design workspace. Leave it through its
      actual Back command before selecting another view in the Project panel. }
    Click('action-logic-close');
    Exit;
  end;
  LVisible := GStudio.ShellView.Root.Find('view-component-0') <> nil;

  if LVisible then
  begin
    {$ifdef PAS2JS}
    LVisible := Length(GStudio.ShellView.ElementFor('view-component-0').getClientRects) > 0;
    {$else}
    LVisible := TWinControl(GStudio.ShellView.ControlFor('view-component-0')).Showing;
    {$endif}
  end;

  if not LVisible then
  begin

    if GStudio.ShellView.Root.Find('action-panel-project') <> nil then
    begin
      Click('action-panel-project');
    end;
    Exit;
  end;
  Check(GStudio.ShellView.Root.Find('view-component-0').Prop('text') =
    'Component / greeting-template', 'The actual reusable-root command names the owned fixture');
  Click('view-component-0');
  Result := True;
end;

function SelectRouteSource: Boolean;
begin
  Result := False;

  if GStudio.ShellView.RootFor(NyxStudioHierarchyID) = nil then
  begin
    { Compact Studio mounts hierarchy only in Inspector. Reveal that actual panel
      before requesting its public collection view, then await the next frame. }
    Click('action-panel-inspector');
    Exit;
  end;
  { Selection enters the real public collection-view/event route. It owns no
    document mutation and does not rely on browser DOM coordinates. }
  GStudio.ShellView.CollectionView(NyxStudioHierarchyID).Select(
    NyxItem(NyxStudioHierarchyCollection, 'name-input'));
  Result := True;
end;

procedure RouteChoice(const AField, ACaption: TNyxText);
var
  LItems: TNyxStrings;
  LIndex: Integer;
  LValue: TNyxText;
begin
  LItems := TNyxStrings.Create;
  try
    LItems.Text := Node(CControlForm + '-' + AField).Prop('items');
    for LIndex := 0 to LItems.Count - 1 do
    begin
      LValue := LItems[LIndex];

      if (LValue = ACaption) or
        (Copy(LValue, Length(LValue) - Length(ACaption) + 1, Length(ACaption)) = ACaption) then
      begin
        Change('control-routes-' + AField, LValue);
        Exit;
      end;
    end;
  finally
    LItems.Free;
  end;
  raise Exception.Create('Missing actual control route choice: ' + AField + ' / ' + ACaption);
end;

procedure ShowRouteExecution;
var
  LDisclosure: INyxDisclosure;
begin
  LDisclosure := RetainNyxControl(Node(CControlForm + '-execution')) as INyxDisclosure;

  if LDisclosure.Node.Prop('value') <> 'true' then
  begin
    Click(LDisclosure.HeaderButton.ID);
  end;
end;

procedure ConfirmRoute;
var
  LID: TNyxText;
begin
  LID := CControlForm + '-confirm';
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GStudio.ShellView.InputFor(LID)).click;
  {$else}
  TCheckBox(GStudio.ShellView.InputFor(LID)).Checked := True;
  {$endif}
  Check(Node(LID).Prop('value') = 'true', 'Actual route warning acknowledgement reaches its owned form');
end;

procedure ExportControlRoute(const APair: TNyxProjectPair);
{$ifndef PAS2JS}
var
  LDirectory: TNyxText;

  procedure Write(const AName, AText: TNyxText);
  var
    LStream: TFileStream;
    LBytes: TNyxBytes;
  begin

    if FileExists(LDirectory + AName) then
    begin
      raise Exception.Create('Control route export refuses an existing fixture artifact');
    end;
    LBytes := NyxEncodeUTF8(AText);
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(LBytes) > 0 then
      begin
        LStream.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LStream.Free;
    end;
  end;
{$endif}
begin
  {$ifdef PAS2JS}
  { The owned Pascal browser consumer reads this explicit qualification export.
    No active user project or source-evaluation mechanism is exposed. }
  document.body.setAttribute('data-control-route-project', EncodeNyxProject(APair));
  {$else}
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + 'published' + PathDelim;
  ForceDirectories(LDirectory);
  Write('nyx.generated.view.pas', APair.Source);
  Write('design.nyx', APair.Design);
  Write('project.nyxproj', EncodeNyxProject(APair));
  {$endif}
end;

procedure Step;
var
  LDocument: TNyxDocument;
  LRoutes: INyxFlowRoutes;
begin
  {$ifndef PAS2JS}

  if GFailure.Error <> '' then
  begin
    raise Exception.Create(GFailure.Error);
  end;
  {$endif}

  if GControlStepStarted = 0 then
  begin
    GControlStepStarted := Clock;
  end;

  if (Clock - GStarted > 600000) or (Clock - GControlStepStarted > 120000) then
  begin
    raise Exception.Create('Control-route Studio deadline at stage ' + IntToStr(GStage));
  end;

  if GStudio.SourceBusy or GStudio.PresentationPending then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-control-route-studio-stage', IntToStr(GStage));
  {$else}
  WriteLn('Control route Studio stage ', GStage);
  Flush(Output);
  {$endif}
  case GStage of
    0:
      begin
        GStudio.SourceCommands.OpenProject(ControlRouteSeed);
      end;
    1:
      begin

        if not OpenControlTemplate then
        begin
          Exit;
        end;
      end;
    2:
      begin

        if not SelectRouteSource then
        begin
          Exit;
        end;
      end;
    3:
      begin

        if not OpenPrograms then
        begin
          Exit;
        end;
      end;
    4:
      begin
        GBefore := EncodeNyxProject(GStudio.ProjectSnapshot);
        Change('control-routes-name', 'Live greeting');
        RouteChoice('event', 'OnChange');
        RouteChoice('graph', 'Greeting flow');
        RouteChoice('input', 'greeting / Name / Text');
        RouteChoice('output', 'greeting / Caption / Text');
        RouteChoice('destination', 'caption-label / Text');
      end;
    5: Click(CControlForm + '-map-input');
    6:
      begin
        Click(CControlForm + '-map-output');
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
          'Staged input/output mapping changes neither accepted artifact');
        ShowRouteExecution;
      end;
    7:
      begin
        Change('control-routes-limit', 'unfinished active-run count');
        Click('action-logic-close');
      end;
    8:
      begin

        if not OpenPrograms then
        begin
          Exit;
        end;
      end;
    9:
      begin
        Check(Node(CControlForm + '-limit').Prop('value') = 'unfinished active-run count',
          'Closing/reopening Logic retains exact unfinished route fields');
        Check(Pos('Mapped inputs: 1 / state outputs: 1', Node(CControlForm + '-summary').Prop('text')) > 0,
          'The same draft retains its complete staged mappings');
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
          'Draft restoration does not publish a project command');
        ShowRouteExecution;
        Change('control-routes-limit', '8');
      end;
    10: Click(CControlForm + '-apply');
    11:
      begin
        Check(GStudio.SourceCommands.State = nssApplied,
          'Ordinary event route Apply publishes through source commands: ' + GStudio.SourceCommands.Message);
        GControlPublished := GStudio.ProjectSnapshot;
        GAfter := EncodeNyxProject(GControlPublished);
        LDocument := TNyxCodec.Decode(GControlPublished.Design);
        try
          LRoutes := NyxControlFlowRoutes(LDocument.Find('name-input'));
          Check((LRoutes.Count = 1) and (LRoutes.Route(0).Trigger = ntChange) and
            (LRoutes.Route(0).Input(0).Source = nfisEventValue) and
            (LRoutes.Route(0).Output(0).StateName = 'caption'),
            'Actual Studio input authors the event/script/label mapping in its saved component');
          Check(NyxControlFlowRoutes(LDocument.Find('greet-button')).Count = 0,
            'Other component routes remain exactly as semantically seeded');
          Check((LDocument.Canvases.Count = 1) and (LDocument.Programs.Count = 1),
            'Scene and Pascal program declarations survive route publication');
        finally
          LDocument.Free;
        end;
        Check((Pos('.InputEventValue(', GControlPublished.Source) > 0) and
          (Pos('NyxTextState(''caption'')', GControlPublished.Source) > 0),
          'The accepted companion uses crafted typed event/state constructs');
        ExportControlRoute(GControlPublished);
        History(False);
      end;
    12:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
          'One actual Undo restores the entire pre-route pair');
        History(True);
      end;
    13:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter,
          'One actual Redo restores the exact authored route pair');
        RouteChoice('route', 'Live greeting');
      end;
    14:
      begin
        Click(CControlForm + '-remove');
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter,
          'Unconfirmed actual removal preserves the complete accepted pair');
      end;
    15:
      begin
        ConfirmRoute;
        Click(CControlForm + '-remove');
      end;
    16:
      begin
        Check(GStudio.SourceCommands.State = nssApplied,
          'Confirmed route removal uses ordinary source publication');
        GControlRemoved := GStudio.ProjectSnapshot;
        Check(EncodeNyxProject(GControlRemoved) <> GAfter, 'Removal changes the saved route pair');
        History(False);
      end;
    17:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter,
          'One actual Undo restores the removed route and Pascal');
        History(True);
      end;
    18:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GControlRemoved),
          'One actual Redo restores exact removal');
        History(False);
      end;
    19:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter,
          'Return to the complete authored route before pending-source qualification');
        GControlPending := GControlPublished;
        GControlPending.Pending := True;
        GControlPending.DraftBase := GControlPublished.Source;
        GControlPending.Draft := GControlPublished.Source + #10 + '// Unfinished control route work';
        GStudio.SourceCommands.OpenProject(GControlPending);
      end;
    20:
      begin

        if not OpenControlTemplate then
        begin
          Exit;
        end;
      end;
    21:
      begin

        if not SelectRouteSource then
        begin
          Exit;
        end;
      end;
    22:
      begin

        if GStudio.ShellView.RootFor(CControlForm) = nil then
        begin

          if not OpenPrograms then
          begin
            Exit;
          end;
        end;
      end;
    23:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GControlPending),
          'Ordinary Open retains the complete pending source pair');
        RouteChoice('route', 'Live greeting');
        Change('control-routes-name', 'Pending copy');
        Click(CControlForm + '-apply');
      end;
    24:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GControlPending),
          'A route mutation preserves the full pending buffer/base/accepted pair');
        Check(Node(CControlForm + '-name').Prop('value') = 'Pending copy',
          'Rejected publication retains unfinished route input');
        {$ifdef PAS2JS}
        GStudio.ShellView.ElementFor(CControlForm + '-title').scrollIntoView;
        document.body.setAttribute('data-control-route-studio', 'passed');
        document.body.setAttribute('data-control-route-studio-checks', IntToStr(GChecks));
        {$endif}
        GDone := True;
      end;
  end;
  Inc(GStage);
  GControlStepStarted := Clock;
end;
