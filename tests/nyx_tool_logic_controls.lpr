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

program nyx_tool_logic_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.state, nyx.callbacks, nyx.application.context,
  nyx.canvas.types, nyx.canvas.scene,
  nyx.canvas.workspace, nyx.tool.canvas, nyx.canvas.adoption, nyx.canvas.routes,
  nyx.flow.types, nyx.flow.declarations, nyx.flow.editing, nyx.flow.routes,
  nyx.flow.route.model, nyx.flow.routing, nyx.logic, nyx.logic.programs, nyx.logic.editor,
  nyx.programs.types, nyx.programs.contract, nyx.programs.declarations, nyx.programs.editing,
  nyx.test.tool.canvas
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser,
    nyx.tool.canvas.browser
  {$else}, Classes, Interfaces, Forms, Controls, StdCtrls, Graphics, Types,
    nyx.widgets.lcl, nyx.application.lcl, nyx.canvas.lcl, nyx.tool.canvas.lcl{$endif};

type
  { Fixture's independent ordinary Pascal callback. It borrows no widget or
    canvas controller; its count verifies graph editing retains static events. }
  TToolRunObserver = class(TNyxApplicationEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GDocument: TNyxDocument;
  GBefore: TNyxText;
  GFlowsBefore: TNyxText;
  GChecks: Integer;
  GStaticCalls: Integer;
  GStage: Integer;
  GTracedStage: Integer;
  GFinished: Boolean;
  GTools: INyxToolCanvasView;
  GLogic: INyxToolCanvasLogicAuthoring;
  GEditing: INyxCanvasFlowEditing;
  GProgramAuthoring: INyxToolCanvasProgramAuthoring;
  GProgramEditing: INyxCanvasProgramEditing;
  GAcceptedPrograms: TNyxText;
  GProgramSource: TObject;
  GProgramsOnly: Boolean;
  GRuntime: INyxCanvasRuntimeView;
  GFormer: TObject;
  GEmbeddedMemo: TObject;
  GPristine: TNyxLogicEditorDraft;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GView: INyxBrowserCanvasWorkspaceView;
  GFace: INyxBrowserToolCanvasView;
  GHost: TJSHTMLElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GView: INyxLCLCanvasWorkspaceView;
  GFace: INyxLCLToolCanvasView;
  GStarted: QWord;
  {$endif}

procedure TToolRunObserver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GStaticCalls);
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tool Logic: ' + AMessage);
  end;
  Inc(GChecks);
end;

function ID(const ASuffix: TNyxText): TNyxText;
begin
  Result := GTools.Inspector.Root.ID + '-' + ASuffix;
end;

function Idle: Boolean;
begin

  if GTools = nil then
  begin
    Exit(True);
  end;
  Result := (GTools.RefreshExecution = nil) or
    not (GTools.RefreshExecution.Status in [nesPending, nesRunning]);

  if Result and (GTools.RefreshExecution <> nil) then
  begin

    if GTools.RefreshExecution.Status <> nesSucceeded then
    begin
      raise Exception.Create('Deferred Logic publication failed: ' + GTools.RefreshExecution.Failure);
    end;
  end;
end;

procedure Change(const ASuffix, AValue: TNyxText);
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LInput := GFace.Renderer.InputFor(ID(ASuffix));
  Check(LInput <> nil, 'actual field exists: ' + ASuffix);
  {$ifdef PAS2JS}

  if LInput is TJSHTMLSelectElement then
  begin
    TJSHTMLSelectElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('change'));
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('input'));
  end;
  {$else}

  if LInput is TComboBox then
  begin
    Check(TComboBox(LInput).Items.IndexOf(AValue) >= 0, 'choice belongs to actual field');
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TCustomEdit(LInput).Text := AValue;
  end;
  {$endif}
  Check(GFace.Renderer.Root.Find(ID(ASuffix)).Prop('value') = AValue,
    'exact input retained in mounted tree');
end;

procedure Click(const ASuffix: TNyxText);
begin
  {$ifdef PAS2JS}
  GFace.Renderer.ElementFor(ID(ASuffix)).click;
  {$else}
  TNyxLCLButton(GFace.Renderer.ControlFor(ID(ASuffix))).Click;
  {$endif}
end;

function EndpointChoice(const AStep, APort: TNyxText;
  ADirection: TNyxFlowDirection): TNyxText;
var
  LEndpoints: TNyxDataValue;
  LEntry: TNyxDataValue;
  LIndex: Integer;
begin
  LEndpoints := GFace.Renderer.Root.Find(ID('logic')).Extensions
    .Value(NyxExtension('nyx.logic-editor')).Field('endpoints');
  for LIndex := 0 to LEndpoints.Count - 1 do
  begin
    LEntry := LEndpoints.Item(LIndex);

    if (LEntry.Field('step').AsText = AStep) and
      (LEntry.Field('port').AsText = APort) and
      (LEntry.Field('direction').AsInteger = Ord(ADirection)) then
    begin
      Exit(LEntry.Field('label').AsText);
    end;
  end;
  raise Exception.Create('Required typed endpoint is unavailable');
end;

procedure Seed;
var
  LContract: TNyxProgramContract;
begin
  GDocument := ToolCanvasDocument;
  LContract := NyxProgramContract
    .Input(NyxProgramSymbol('json'), NyxProgramType(npkJSON))
    .Output(NyxProgramSymbol('text'), NyxProgramType(npkText));
  GDocument.Programs.Define(NyxProgramRef('Profile caption'),
    NewNyxProgramDefinition('begin text := ''Pascal script running from the canvas'' end.', LContract));
  GDocument.Validate;
  GBefore := TNyxCodec.Encode(GDocument);
end;

{ Inspect the button's exact route, independently from the memo's earlier
  change route. Registration order is not execution provenance. }
function RunReport: TNyxDataValue;
var
  LBinding: INyxFlowRouteBinding;
  LReport: TNyxDataValue;
  LIndex: Integer;
begin
  LBinding := GRuntime.BehaviorFor(NyxCanvasItem('Editor')).Routes;
  for LIndex := 0 to LBinding.Count - 1 do
  begin
    LReport := LBinding.Report(LIndex);

    if (LReport.Field('owner').AsText = 'run-button') and
      (LReport.Field('route').AsText = 'Format profile') then
    begin
      Exit(LReport);
    end;
  end;
  raise Exception.Create('Expected button route is unavailable');
end;

procedure Step;
var
  LEditing: INyxCanvasFlowEditing;
  LRuntime: INyxCanvasRuntimeView;
  LLogic: INyxToolCanvasLogicAuthoring;
  LProgramAuthoring: INyxToolCanvasProgramAuthoring;
  LProgramEditing: INyxCanvasProgramEditing;
  {$ifdef PAS2JS}LFace: INyxBrowserToolCanvasView;{$else}LFace: INyxLCLToolCanvasView;{$endif}
  LFailed: Boolean;
  LDraft: TNyxLogicEditorDraft;
  LRevision: Integer;
  LSnapshot: TNyxCanvasRouteAdoptionSnapshot;
  LReportBefore: TNyxText;
begin

  if not Idle then
  begin
    Exit;
  end;
  {$ifndef PAS2JS}

  if GTracedStage <> GStage then
  begin
    GTracedStage := GStage;
    WriteLn('Stage ', GStage);
    Flush(Output);
  end;
  {$endif}
  case GStage of
    0:
      begin
        {$ifdef PAS2JS}
        GView := GApp.View.CanvasView(GDocument.Find('tool-canvas').Part(NyxPart('workspace')).ID)
          as INyxBrowserCanvasWorkspaceView;
        {$else}
        GView := GApp.View.CanvasView(GDocument.Find('tool-canvas').Part(NyxPart('workspace')).ID)
          as INyxLCLCanvasWorkspaceView;
        {$endif}
        Check(Supports(GView, INyxCanvasFlowEditing, LEditing), 'runtime graph editing available');
        Check(Supports(GView, INyxCanvasRuntimeView, LRuntime), 'runtime scene available');
        GEditing := LEditing;
        GRuntime := LRuntime;
        GView.Refresh;
        GView.Execute(nccFit);
        GTools := GApp.View.ToolCanvasView('tool-canvas');
        GEmbeddedMemo := TObject(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo')));
        Check(Supports(GTools.Inspector, INyxToolCanvasLogicAuthoring, LLogic),
          'public managed Logic workbench available');
        GLogic := LLogic;
        Check(Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif},
          LFace), 'actual target face available');
        GFace := LFace;

        if GProgramsOnly then
        begin
          { Focused source iteration seeds the earlier graph through its typed
            runtime owner. It makes no claim for the skipped Logic form journey. }
          GEditing.ApplyFlowChanges([NyxReplaceFlowStep(NyxFlowRef('Profile'), NyxFlowStep('compact'),
            NewNyxProgramLogic(NyxProgramRef('Profile caption'),
              GDocument.Programs.Definition(NyxProgramRef('Profile caption')).Contract))],
            GRuntime.Scene.Revision, GEditing.FlowRevision);
          GStage := 14;
        end
        else
        begin
          Click('tab-logic');
        end;
      end;
    1:
      begin
        Check(GLogic.LogicEditor <> nil, 'ordinary Logic tab opens operation and script collection');
        GPristine.Capture(ID('logic'), GFace.Renderer.Root);
        {$ifdef PAS2JS}
        GFormer := TObject(GFace.Renderer.InputFor(ID('logic-search')));
        {$else}
        GFormer := GFace.Renderer.InputFor(ID('logic-search'));
        {$endif}
        Change('logic-search', 'Pascal');
      end;
    2:
      begin
        Check(TObject(GFace.Renderer.InputFor(ID('logic-search'))) = GFormer,
          'search synchronizes without replacing its actual editor');
        LDraft.Capture(ID('logic'), GFace.Renderer.Root);
        Check(LDraft.SameProposal(GPristine), 'toolbox search is presentation rather than a proposal');
        Change('logic-operation', NyxProgramLogicKind);
      end;
    3:
      begin
        Check(Pos('Profile caption', GFace.Renderer.Root.Find(ID('logic-program-program')).Prop('items')) > 0,
          'saved typed Pascal program is discoverable');
        Change('logic-step', 'compact');
        Click('logic-set');
      end;
    4:
      begin
        Check(GLogic.HasPendingLogic, 'script replacement stages in an independent graph proposal');
        Click('logic-apply');
      end;
    5:
      begin
        Check(GEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).Descriptor(3).Kind.Name =
          NyxProgramLogicKind, 'actual Apply replaces the existing operation with a saved script');
        Check(TObject(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'))) = GEmbeddedMemo,
          'script publication retains the actual embedded memo');
        {$ifdef PAS2JS}
        GView.RendererFor(NyxCanvasItem('Editor')).ElementFor('run-button').click;
        {$else}
        TNyxLCLButton(GView.RendererFor(NyxCanvasItem('Editor')).ControlFor('run-button')).Click;
        {$endif}
      end;
    6:
      begin

        if GApp.State.GetValue(NyxTextState('compact')) <> 'Pascal script running from the canvas' then
        begin
          Exit;
        end;
        Check(GApp.State.GetValue(NyxTextState('compact')) = 'Pascal script running from the canvas',
          'ordinary control event executes newly configured Pascal script into runtime state');
        Check(GStaticCalls = 1, 'ordinary static callback survives runtime Logic replacement');
        {$ifdef PAS2JS}
        Check(Pos('Pascal script running from the canvas',
          GView.RendererFor(NyxCanvasItem('Output')).ElementFor('compact-label').textContent) > 0,
          'actual browser bound label displays new script output');
        {$else}
        Check(TLabel(GView.RendererFor(NyxCanvasItem('Output')).ControlFor('compact-label')).Caption =
          'Pascal script running from the canvas', 'actual native bound label displays new script output');
        {$endif}
        Change('logic-step-name', 'preview');
        Change('logic-operation', 'json-format');
      end;
    7: Click('logic-add');
    8:
      begin
        Check(GLogic.HasPendingLogic and
          (GEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).StepCount = 5),
          'new required-input operation stages without interrupting the live graph');
        LFailed := False;
        try
          GLogic.ApplyLogic;
        except
          on E: Exception do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and GLogic.HasPendingLogic and
          (GEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).StepCount = 5),
          'incomplete application graph refuses Apply and retains the staged step');
        Change('logic-source', EndpointChoice('select', 'value', nfdOutput));
        Change('logic-target', EndpointChoice('preview', 'json', nfdInput));
        Click('logic-connect');
      end;
    9:
      begin
        Check(GLogic.HasPendingLogic, 'compatible wire remains staged with its operation');
        GLogic.ShowLogic(False);
        GTools.Refresh;
        GLogic.ShowLogic;
        GTools.Refresh;
        Check(GLogic.HasPendingLogic and
          (GFace.Renderer.Root.Find(ID('logic')).Extensions.Value(NyxExtension('nyx.logic-editor'))
            .Field('baseline').Field('definition').Field('steps').Count = 6),
          'staged operation and connection survive hiding and reopening the workbench');
        Click('logic-apply');
      end;
    10:
      begin
        Check(not GLogic.HasPendingLogic and
          (GEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).StepCount = 6),
          'complete operation and wire publish together');
        Check(GEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).WireCount = 5,
          'actual form connects compatible ports to the new operation');
        Check(TObject(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'))) = GEmbeddedMemo,
          'grouped publication retains the actual embedded memo');
        {$ifdef PAS2JS}
        GView.RendererFor(NyxCanvasItem('Editor')).ElementFor('run-button').click;
        {$else}
        TNyxLCLButton(GView.RendererFor(NyxCanvasItem('Editor')).ControlFor('run-button')).Click;
        {$endif}
        GFlowsBefore := GEditing.RuntimeFlows.ToData.ToJSON;
        Change('logic-step-name', 'preview');
        Click('logic-add');
      end;
    11:
      begin

        if RunReport.Field('succeeded').AsInteger = 0 then
        begin

          if RunReport.Field('failed').AsInteger > 0 then
          begin
            raise Exception.Create('Complete staged graph execution failed: ' +
              RunReport.Field('failure').AsText);
          end;
          Exit;
        end;
        Check(GStaticCalls = 2, 'newly connected graph executes and retains the ordinary static callback');
        Check(GEditing.RuntimeFlows.ToData.ToJSON = GFlowsBefore,
          'duplicate operation refuses without partial graph publication');
        Check(GFace.Renderer.Root.Find(ID('logic-step-name')).Prop('value') = 'preview',
          'refused proposal retains exact text');
        Check(GFace.Renderer.Root.Find(ID('logic-feedback')).Prop('text') <> '',
          'refusal has visible feedback');
        Click('tab-ports');
      end;
    12: Click('tab-logic');
    13:
      begin
        Check(GFace.Renderer.Root.Find(ID('logic-step-name')).Prop('value') = 'preview',
          'unfinished proposal survives hiding and reopening the Logic tab');
        LFailed := False;
        try
          GLogic.ReloadLogic;
        except
          on E: Exception do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'reload requires explicit discard for an unfinished proposal');
        LRevision := GEditing.FlowRevision;
        GEditing.ApplyFlowChanges([NyxDefineFlow(NyxFlowRef('Profile'),
          GEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')))],
          GRuntime.Scene.Revision, LRevision);
        Click('logic-add');
      end;
    14:
      begin
        Check(Pos('stale', GFace.Renderer.Root.Find(ID('logic-feedback')).Prop('text')) > 0,
          'same graph bytes at a newer revision refuse the retained proposal');
        Check(GEditing.RuntimeFlows.ToData.ToJSON = GFlowsBefore,
          'stale refusal preserves exact graph declarations');
        LSnapshot := CaptureNyxCanvasRouteAdoption(GDocument,
          NyxControl(GDocument.Find('tool-canvas').Part(NyxPart('workspace')).ID), GView);
        Check(LSnapshot.HasChanges and (LSnapshot.Count = 0) and (LSnapshot.FlowCount = 1),
          'applied runtime graph remains available as an explicit graph-only project snapshot');
        GLogic.ReloadLogic(True);
        GTools.Refresh;
      end;
    15:
      begin
        if not GProgramsOnly then
        begin
          Check(GFace.Renderer.Root.Find(ID('logic-step-name')).Prop('value') = 'step',
            'explicit discard rebuilds from the current runtime graph');
        end;
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime authoring preserves complete authored document');
        Check(GEditing.RuntimeFlows.ToData.ToJSON <> GDocument.Flows.ToData.ToJSON,
          'runtime export is independent from authored declarations');
        Check(Supports(GTools.Inspector, INyxToolCanvasProgramAuthoring, LProgramAuthoring),
          'public Pascal workbench is available');
        Check(Supports(GView, INyxCanvasProgramEditing, LProgramEditing),
          'canvas owns prepared Pascal editing');
        GProgramAuthoring := LProgramAuthoring;
        GProgramEditing := LProgramEditing;
        Click('tab-programs');
      end;
    16:
      begin
        GProgramAuthoring.SelectProgram(NyxProgramRef('Profile caption'));
        GTools.Refresh;
      end;
    17:
      begin
        Check(not GProgramAuthoring.HasPendingProgram, 'opening current Pascal starts without a proposal');
        Change('program-source', 'begin text := ''Edited live in Pascal'' end.');
        Check(GProgramAuthoring.HasPendingProgram, 'actual source editing retains a pending proposal');
        Click('program-apply');
      end;
    18:
      begin
        Check(GProgramEditing.RuntimePrograms.Definition(NyxProgramRef('Profile caption')).Source =
          'begin text := ''Edited live in Pascal'' end.', 'actual Apply publishes exact interpreted source');
        Check(TObject(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'))) = GEmbeddedMemo,
          'program publication retains the same embedded memo');
        {$ifdef PAS2JS}
        GView.RendererFor(NyxCanvasItem('Editor')).ElementFor('run-button').click;
        {$else}
        TNyxLCLButton(GView.RendererFor(NyxCanvasItem('Editor')).ControlFor('run-button')).Click;
        {$endif}
      end;
    19:
      begin

        if GApp.State.GetValue(NyxTextState('compact')) <> 'Edited live in Pascal' then
        begin
          Exit;
        end;
        Check(GStaticCalls = 1 + Ord(not GProgramsOnly) * 2,
          'live source replacement executes through the existing control event');
        {$ifdef PAS2JS}
        Check(Pos('Edited live in Pascal',
          GView.RendererFor(NyxCanvasItem('Output')).ElementFor('compact-label').textContent) > 0,
          'browser bound control displays edited program result');
        {$else}
        Check(TLabel(GView.RendererFor(NyxCanvasItem('Output')).ControlFor('compact-label')).Caption =
          'Edited live in Pascal', 'native bound control displays edited program result');
        {$endif}
        GAcceptedPrograms := GProgramEditing.RuntimePrograms.ToData.ToJSON;
        GProgramSource := TObject(GFace.Renderer.InputFor(ID('program-source')));
        Change('program-source', 'begin text := ; { unfinished proposal } end.');
        Click('program-check');
      end;
    20:
      begin
        Check(Pos('Line ', GFace.Renderer.Root.Find(ID('program-feedback')).Prop('text')) = 1,
          'local parser check reports invalid source beside the editor');
        Click('program-apply');
      end;
    21:
      begin
        Check(GProgramEditing.RuntimePrograms.ToData.ToJSON = GAcceptedPrograms,
          'malformed source preserves exact applied program membership');
        Check(GProgramAuthoring.HasPendingProgram and
          (TObject(GFace.Renderer.InputFor(ID('program-source'))) = GProgramSource),
          'refusal retains pending source and the actual editor');
        Check(GFace.Renderer.Root.Find(ID('program-publication-feedback')).Prop('text') <> '',
          'publication refusal is visible');
        Click('tab-ports');
      end;
    22: Click('tab-programs');
    23:
      begin
        Check(GFace.Renderer.Root.Find(ID('program-source')).Prop('value') =
          'begin text := ; { unfinished proposal } end.', 'unfinished exact Pascal survives tab navigation');
        LFailed := False;
        try
          GProgramAuthoring.ReloadProgram;
        except
          on E: Exception do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'reload refuses implicit loss of unfinished Pascal');
        GProgramAuthoring.ReloadProgram(True);
        GTools.Refresh;
      end;
    24: Click('program-new');
    25:
      begin
        Change('program-name', 'Salutation');
        Change('program-source', 'begin text := ''Welcome from a new script'' end.');
        Change('program-port-name', 'text');
        Change('program-port-direction', 'Output');
        Click('program-add-port');
      end;
    26:
      begin
        Change('program-port-name', 'json');
        Change('program-port-direction', 'Input');
        Change('program-port-kind', 'JSON');
        Click('program-add-port');
      end;
    27: Click('program-apply');
    28:
      begin
        Check(GProgramEditing.RuntimePrograms.Contains(NyxProgramRef('Salutation')),
          'new source and typed input/output schema publish through ordinary fields');
        Check(GProgramEditing.RuntimePrograms.Contains(NyxProgramRef('Profile caption')),
          'new program preserves existing definitions');
        GLogic.ReloadLogic(True);
        GTools.Refresh;
      end;
    29:
      begin
        Change('logic-operation', NyxProgramLogicKind);
        Check(Pos('Salutation', GFace.Renderer.Root.Find(ID('logic-program-program')).Prop('items')) > 0,
          'new runtime script is discoverable in the same Logic collection');
        Change('logic-program-program', 'Salutation');
        Change('logic-step', 'compact');
        Click('logic-set');
      end;
    30: Click('logic-apply');
    31:
      begin
        {$ifdef PAS2JS}
        GView.RendererFor(NyxCanvasItem('Editor')).ElementFor('run-button').click;
        {$else}
        TNyxLCLButton(GView.RendererFor(NyxCanvasItem('Editor')).ControlFor('run-button')).Click;
        {$endif}
      end;
    32:
      begin

        if GApp.State.GetValue(NyxTextState('compact')) <> 'Welcome from a new script' then
        begin
          Exit;
        end;
        Check(GStaticCalls = 2 + Ord(not GProgramsOnly) * 2,
          'new user-authored program executes from its configured control event');
        LReportBefore := RunReport.ToJSON;
        GProgramEditing.ApplyProgramChanges([NyxDefineProgram(NyxProgramRef('Salutation'),
          GProgramEditing.RuntimePrograms.Definition(NyxProgramRef('Salutation')))],
          GRuntime.Scene.Revision, GProgramEditing.ProgramRevision);
        Check(RunReport.ToJSON = LReportBefore,
          'identical program intent advances revision without replacing unchanged graph work');
        LSnapshot := CaptureNyxCanvasRouteAdoption(GDocument,
          NyxControl(GDocument.Find('tool-canvas').Part(NyxPart('workspace')).ID), GView);
        Check(LSnapshot.HasChanges and (LSnapshot.ProgramCount = 2) and (LSnapshot.FlowCount = 1),
          'applied programs and referencing graph form one complete save candidate');
        GAcceptedPrograms := GProgramEditing.RuntimePrograms.ToData.ToJSON;
        GProgramAuthoring.SelectProgram(NyxProgramRef('Salutation'), True);
        GTools.Refresh;
      end;
    33:
      begin
        Change('program-source', 'begin text := ''Stale proposal'' end.');
        GEditing.ApplyFlowChanges([NyxDefineFlow(NyxFlowRef('Profile'),
          GEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')))],
          GRuntime.Scene.Revision, GEditing.FlowRevision);
        Click('program-apply');
      end;
    34:
      begin
        Check(Pos('stale', GFace.Renderer.Root.Find(ID('program-publication-feedback')).Prop('text')) > 0,
          'graph revision changes invalidate retained Pascal context');
        Check(GProgramEditing.RuntimePrograms.ToData.ToJSON = GAcceptedPrograms,
          'stale source refusal preserves applied definitions');
        LFailed := False;
        try
          GProgramEditing.ApplyProgramChanges([NyxDefineProgram(NyxProgramRef('Salutation'),
            NewNyxProgramDefinition('begin end.', NyxProgramContract))],
            GRuntime.Scene.Revision, GProgramEditing.ProgramRevision);
        except
          on E: Exception do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GProgramEditing.RuntimePrograms.ToData.ToJSON = GAcceptedPrograms),
          'incompatible program signature refuses before any graph owner changes');
        LFailed := False;
        try
          GProgramEditing.ApplyProgramChanges([NyxRemoveProgram(NyxProgramRef('Salutation'))],
            GRuntime.Scene.Revision, GProgramEditing.ProgramRevision);
        except
          on E: Exception do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GProgramEditing.RuntimePrograms.ToData.ToJSON = GAcceptedPrograms),
          'referenced program removal refuses as one whole candidate');
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'all live program authoring preserves the source document');
        {$ifdef PAS2JS}
        GView.RendererFor(NyxCanvasItem('Editor')).ElementFor('run-button').click;
        {$else}
        TNyxLCLButton(GView.RendererFor(NyxCanvasItem('Editor')).ControlFor('run-button')).Click;
        {$endif}
      end;
    35:
      begin

        if RunReport.Field('succeeded').AsInteger = 0 then
        begin

          if RunReport.Field('failed').AsInteger > 0 then
          begin
            raise Exception.Create('Retained program execution failed: ' + RunReport.Field('failure').AsText);
          end;
          Exit;
        end;
        Check((GStaticCalls = 3 + Ord(not GProgramsOnly) * 2) and
          (GApp.State.GetValue(NyxTextState('compact')) = 'Welcome from a new script'),
          'the working script and static callback still execute after stale/signature/removal refusals');
        Check(TObject(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'))) = GEmbeddedMemo,
          'all source proposals retain the original embedded editor');
        GFinished := True;
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  GFace := nil;
  GLogic := nil;
  GEditing := nil;
  GProgramAuthoring := nil;
  GProgramEditing := nil;
  GRuntime := nil;
  GTools := nil;
  GView := nil;
  FreeAndNil(GApp);
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 600000 then
    begin
      raise Exception.Create('Actual Tool Logic deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-tool-logic', 'passed');
      document.body.setAttribute('data-tool-logic-checks', IntToStr(GChecks));

      if Pos('retire=1', window.location.search) > 0 then
      begin
        Retire;
      end;
      Exit;
    end;
    window.setTimeout(@Drive, 25);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-tool-logic', 'failed');
      document.body.setAttribute('data-tool-logic-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

{$ifndef PAS2JS}
{ Optional native presentation review uses the ordinary scroll containers to
  bring the already mounted workbench into view. It stops after search and is
  deliberately separate from the complete behavior journey above. }
procedure ShowWorkbench(const ATarget: TNyxText);
var
  LTarget: TControl;
  LParent: TWinControl;
  LPosition: TPoint;
begin
  LTarget := GFace.Renderer.ControlFor(ID(ATarget));
  LParent := LTarget.Parent;
  while LParent <> nil do
  begin

    if LParent is TScrollingWinControl then
    begin
      TScrollingWinControl(LParent).ScrollInView(LTarget);
      LPosition := LTarget.ClientToParent(Point(0, 0), LParent);
      TScrollingWinControl(LParent).VertScrollBar.Position :=
        TScrollingWinControl(LParent).VertScrollBar.Position + LPosition.Y;
    end;
    LParent := LParent.Parent;
  end;
  Application.ProcessMessages;
  LPosition := GApp.Window.ScreenToClient(LTarget.ClientToScreen(Point(0, 0)));
  Check(LTarget.IsVisible and (LPosition.Y >= 0) and
    (LPosition.Y + LTarget.Height <= GApp.Window.ClientHeight),
    'native workbench heading is inside the actual visible window after ordinary scrolling');
end;

var
  LTurn: Integer;
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
{$endif}
begin
  try
    RegisterNyxCallback(NyxHandler('TToolRunObserver'), TToolRunObserver);
    GTracedStage := -1;
    {$ifdef PAS2JS}
    GProgramsOnly := Pos('programs-only=1', window.location.search) > 0;
    {$else}
    GProgramsOnly := ParamStr(2) = '--programs-only';
    {$endif}
    Seed;
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    GApp := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, GHost);
    GStarted := TJSDate.now;
    Drive;
    {$else}
    Application.Initialize;
    GApp := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GApp.Window.Show;
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > 600000 then
      begin
        raise Exception.Create('Actual Tool Logic deadline at stage ' + IntToStr(GStage));
      end;
      if (ParamStr(2) = '--review-workbench') and (GStage = 2) then
      begin
        GFinished := Idle;
      end
      else
      begin
        Step;
      end;
      Sleep(1);
    end;

    if ParamStr(2) = '--review-workbench' then
    begin
      ShowWorkbench('logic-workbench-title');
    end
    else if ParamStr(1) <> '' then
    begin
      ShowWorkbench('program-source');
    end;

    if ParamStr(1) <> '' then
    begin
      LBitmap := GApp.Window.GetFormImage;
      LPNG := TPortableNetworkGraphic.Create;
      try
        LPNG.Assign(LBitmap);
        LPNG.SaveToFile(ParamStr(1));
      finally
        LPNG.Free;
        LBitmap.Free;
      end;
    end;
    Retire;
    { Complete actual UI turns after revoking callbacks let queued cancellation
      release its last lease before heaptrc measures the consumer. }
    for LTurn := 1 to 12 do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Sleep(1);
    end;
    if ParamStr(2) = '--review-workbench' then
    begin
      WriteLn('PASS ', GChecks, ' native workbench presentation checks (partial journey)');
    end
    else
    begin
      WriteLn('PASS ', GChecks, ' actual Tool Canvas Logic checks');
    end;
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tool-logic', 'failed');
      document.body.setAttribute('data-tool-logic-error', E.Message);
      {$else}
      WriteLn('FAIL stage ', GStage, ': ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
