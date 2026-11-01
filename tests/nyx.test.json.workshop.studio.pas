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
unit nyx.test.json.workshop.studio;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.choices, nyx.studio.projects;

type
  { One finite ordinary Studio input journey. Tick yields while source/physical
    publication is pending. Pair is available only after actual Save/Open.
    Final release retires Studio before its native host or browser exchange. }
  INyxToolWorkshopStudioJourney = interface(IInterface)
    ['{8C9402D8-26E4-47ED-9CA6-FAEAE2DC4A38}']
    function Tick: Boolean;
    function GetPair: TNyxProjectPair;
    function GetChecks: Integer;
    property Pair: TNyxProjectPair read GetPair;
    property Checks: Integer read GetChecks;
  end;

  { Compatibility name for the original JSON consumer of the shared journey. }
  INyxJSONWorkshopStudioJourney = INyxToolWorkshopStudioJourney;

function NewJSONWorkshopStudioJourney(const ADirectory: TNyxText;
  AWidth: Integer): INyxJSONWorkshopStudioJourney;

{ Uses the same ordinary Studio input and persistence path for resource options;
  the result is the actual saved pair, with no transport or network authority. }
function NewResourceWorkshopStudioJourney(const ADirectory: TNyxText;
  AWidth: Integer): INyxToolWorkshopStudioJourney;

implementation

uses
  SysUtils, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.files, nyx.studio.files, nyx.json.workshop, nyx.resource.workshop,
  nyx.tool.canvas,
  nyx.canvas.workspace, nyx.canvas.adoption, nyx.flow.declarations,
  nyx.flow.editing, nyx.studio.menu, nyx.behavior, nyx.scheduler,
  {$ifdef PAS2JS}
  JS, Web, nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange, nyx.tool.canvas.browser;
  {$else}
  Classes, Forms, Controls, StdCtrls, nyx.studio.lcl, nyx.studio.projectstore,
  nyx.tool.canvas.lcl;
  {$endif}

const
  CResourceDraft: TNyxText = 'unfinished 🌙';

type
  { File bytes alone are substituted in the browser. Native Studio uses its
    ordinary isolated on-disk project store for Save/Open. No model is patched. }
  TProjectFiles = class(TInterfacedObject, INyxTextFileExchange)
  public
    Reply: TNyxTextFileReply;
    Exported: TNyxTextFiles;
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
    procedure Deliver;
  end;
  {$ifdef PAS2JS}
  { Browser source/completion jobs use separate owner-retained exchanges. This
    subclass only borrows their pointers to deliver pending test transport work.
    It owns no LAN server and makes no authenticated HTTP/MCP claim. }
  TWorkshopStudio = class(TNyxStudio)
  public
    Files: INyxTextFileExchange;
    Core: TNyxAgentSession;
    Exchanges: array of TNyxTestEditorExchange;
    constructor CreateFor(ACore: TNyxAgentSession; const AFiles: INyxTextFileExchange);
  protected
    function CreateProjectFiles: INyxTextFileExchange; override;
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;
  { Target observation borrows only the actual Tool Canvas inspector renderer. }
  TToolPresentation = INyxBrowserToolCanvasView;
  {$else}
  { Native project persistence remains the real Studio project-store behavior. }
  TWorkshopStudio = class(TNyxNativeStudio)
  public
    Files: INyxTextFileExchange;
  protected
    function CreateProjectFiles: INyxTextFileExchange; override;
  end;
  { Target observation borrows only the actual Tool Canvas inspector renderer. }
  TToolPresentation = INyxLCLToolCanvasView;
  { Expose inherited standard input without changing any Studio controller. }
  TControlInput = class(TControl);
  {$endif}
  { Owns exactly one Studio plus file/exchange fixtures. All graph/draft/history
    changes are actual form/button input, not direct graph or session mutation. }
  TJourney = class(TInterfacedObject, INyxToolWorkshopStudioJourney)
  private
    FStudio: TWorkshopStudio;
    FDirectory: TNyxText;
    FFiles: TProjectFiles;
    FFileLease: INyxTextFileExchange;
    FTools: INyxToolCanvasView;
    FTarget: TToolPresentation;
    FLogic: INyxToolCanvasLogicAuthoring;
    FEditing: INyxCanvasFlowEditing;
    FBase: TNyxText;
    FAfter: TNyxText;
    FSaved: TNyxText;
    FStage: Integer;
    FChecks: Integer;
    FStarted: Double;
    FDone: Boolean;
    FResource: Boolean;
    FCanvas: TNyxControlRef;
    FProjectName: TNyxText;
    {$ifdef PAS2JS}
    FCore: TNyxAgentSession;
    {$else}
    FWindow: TForm;
    {$endif}
    procedure Check(AValue: Boolean; const AReason: TNyxText);
    function EncodedPair: TNyxText;
    function ID(const ASuffix: TNyxText): TNyxText;
    function ToolValue(const ASuffix: TNyxText): TNyxText;
    function FormatterIndent(const AFlow: TNyxFlowRef): Integer;
    procedure ClickShell(const AID: TNyxText);
    procedure ChangeShell(const AID, AValue: TNyxText);
    procedure ClickTool(const ASuffix: TNyxText);
    procedure ChangeTool(const ASuffix, AValue: TNyxText);
    procedure ClickProjectMenu(AOpenBranch: Boolean);
    procedure ReleaseTools;
    procedure RunResourceStage;
    procedure RunJSONStage;
    function ResourceConfiguration: TNyxDataValue;
  public
    constructor Create(const ADirectory: TNyxText; AWidth: Integer;
      AResource: Boolean = False);
    destructor Destroy; override;
    function Tick: Boolean;
    function GetPair: TNyxProjectPair;
    function GetChecks: Integer;
  end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure TProjectFiles.Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
begin
  ASelection.Validate;
  Reply := AReply;
end;

function TProjectFiles.ExportFiles(const AFiles: TNyxTextFiles): Boolean;
begin
  ValidateNyxTextFiles(AFiles, NyxTextFileSelection.UpToFiles(2));
  Exported := Copy(AFiles, 0, Length(AFiles));
  Result := True;
end;

procedure TProjectFiles.Cancel;
begin
  Reply := nil;
end;

procedure TProjectFiles.Deliver;
var
  LReply: TNyxTextFileReply;
begin
  LReply := Reply;
  Reply := nil;

  if not Assigned(LReply) then
  begin
    raise Exception.Create('Studio did not request the project file');
  end;
  LReply(fpsSelected, Exported, '');
end;

function TWorkshopStudio.CreateProjectFiles: INyxTextFileExchange;
begin
  Result := Files;
end;

{$ifdef PAS2JS}
constructor TWorkshopStudio.CreateFor(ACore: TNyxAgentSession;
  const AFiles: INyxTextFileExchange);
begin
  Core := ACore;
  Files := AFiles;
  inherited Create;
end;

function TWorkshopStudio.CreateEditorExchange: TNyxStudioEditorExchange;
var
  LExchange: TNyxTestEditorExchange;
  LIndex: Integer;
begin
  LExchange := TNyxTestEditorExchange.Create(Core);
  try
    LIndex := Length(Exchanges);
    SetLength(Exchanges, LIndex + 1);
    Exchanges[LIndex] := LExchange;
    Result := LExchange;
  except
    LExchange.Free;
    raise;
  end;
end;
{$endif}

constructor TJourney.Create(const ADirectory: TNyxText; AWidth: Integer;
  AResource: Boolean);
var
  LDocument: TNyxDocument;
  LSeed: TNyxProjectPair;
  LUnit: TNyxText;
begin
  inherited Create;
  FDirectory := IncludeTrailingPathDelimiter(ADirectory);
  FStarted := Clock;
  FResource := AResource;

  if FResource then
  begin
    LDocument := BuildResourceWorkshopDocument;
    FCanvas := ResourceWorkshopCanvas;
    FProjectName := 'resource-workshop';
    LUnit := 'nyx.generated.view';
  end
  else
  begin
    LDocument := BuildJSONWorkshopDocument;
    FCanvas := JSONWorkshopCanvas;
    FProjectName := 'json-workshop';
    LUnit := 'nyx.generated.json.workshop';
  end;
  try
    LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, LUnit));
  finally
    LDocument.Free;
  end;
  FFiles := TProjectFiles.Create;
  FFileLease := FFiles;
  {$ifdef PAS2JS}
  FCore := TNyxAgentSession.Create(LSeed);
  FStudio := TWorkshopStudio.CreateFor(FCore, FFileLease);
  FStudio.Run(False);
  FStudio.ConnectAgents;
  {$else}
  FWindow := TForm.CreateNew(nil);
  FWindow.ClientWidth := AWidth;
  FWindow.ClientHeight := 900;
  FWindow.Show;
  FStudio := TWorkshopStudio.Create(FWindow, FDirectory);
  FStudio.Files := FFileLease;
  FStudio.LoadProject(LSeed);
  FStudio.Run;
  {$endif}
end;

procedure TJourney.ReleaseTools;
begin
  FEditing := nil;
  FLogic := nil;
  FTarget := nil;
  FTools := nil;
end;

destructor TJourney.Destroy;
begin
  ReleaseTools;
  FStudio.Free;
  {$ifdef PAS2JS}FCore.Free;{$else}FWindow.Free;{$endif}
  FFileLease := nil;
  inherited Destroy;
end;

procedure TJourney.Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Workshop Studio stage ' + IntToStr(FStage) + ': ' + AReason);
  end;
  Inc(FChecks);
end;

function TJourney.EncodedPair: TNyxText;
begin
  Result := EncodeNyxProject(FStudio.ProjectSnapshot);
end;

function TJourney.ID(const ASuffix: TNyxText): TNyxText;
begin
  Result := FTools.Inspector.Root.ID + '-' + ASuffix;
end;

function TJourney.ToolValue(const ASuffix: TNyxText): TNyxText;
begin
  Result := FTarget.Renderer.Root.Find(ID(ASuffix)).Prop(NyxAttributeName(atValue));
end;

function TJourney.FormatterIndent(const AFlow: TNyxFlowRef): Integer;
var
  LDefinition: INyxFlowDefinition;
  LIndex: Integer;
begin
  LDefinition := FEditing.RuntimeFlows.Definition(AFlow);
  for LIndex := 0 to LDefinition.StepCount - 1 do
  begin

    if LDefinition.Step(LIndex).Name = JSONWorkshopStep(jwsFormat).Name then
    begin
      Exit(LDefinition.Configuration(LIndex).Field('indent').AsInteger);
    end;
  end;
  raise Exception.Create('The useful graph has no saved formatter');
end;

function TJourney.ResourceConfiguration: TNyxDataValue;
var
  LDefinition: INyxFlowDefinition;
  LIndex: Integer;
begin
  LDefinition := FEditing.RuntimeFlows.Definition(ResourceWorkshopFlow);
  for LIndex := 0 to LDefinition.StepCount - 1 do
  begin

    if LDefinition.Step(LIndex).Name = 'resource' then
    begin
      Exit(LDefinition.Configuration(LIndex));
    end;
  end;
  raise Exception.Create('The useful graph has no resource input');
end;

procedure TJourney.RunJSONStage;
begin
  case FStage of
    3:
      begin
        ChangeTool('logic-graph', JSONWorkshopSaveFlow.Name);
        ClickTool('logic-open');
      end;
    4:
      begin
        ChangeTool('logic-step', 'Format JSON');
        ClickTool('logic-load');
      end;
    5:
      begin
        Check(ToolValue('logic-indent') = '2', 'form loads the actual saved formatter configuration');
        ChangeTool('logic-indent', '4');
        ClickTool('tab-ports');
      end;
    6: ClickTool('tab-logic');
    7:
      begin
        Check((ToolValue('logic-indent') = '4') and (EncodedPair = FBase),
          'leaving/reopening the tab retains the unfinished field without changing source or design');
        ClickTool('logic-set');
      end;
    8:
      begin
        Check(FLogic.HasPendingLogic and (EncodedPair = FBase),
          'real form configuration stages a graph proposal independently from project publication');
        ClickTool('logic-apply');
      end;
    9:
      begin
        Check(FormatterIndent(JSONWorkshopSaveFlow) = 4, 'actual Apply updates the running Save graph');
        ChangeTool('logic-graph', 'Format JSON');
        ClickTool('logic-open');
      end;
    10:
      begin
        ChangeTool('logic-step', 'Format JSON');
        ClickTool('logic-load');
      end;
    11:
      begin
        ChangeTool('logic-indent', '4');
        ClickTool('logic-set');
      end;
    12: ClickTool('logic-apply');
    13:
      begin
        Check((FormatterIndent(NyxFlowRef('Format JSON')) = 4) and (EncodedPair = FBase),
          'both useful runtime graphs change without silently rewriting the authored project');
        ClickTool('save-connections');
      end;
  end;
end;

procedure TJourney.RunResourceStage;
begin
  case FStage of
    3:
      begin
        ChangeTool('logic-graph', ResourceWorkshopFlow.Name);
        ClickTool('logic-open');
      end;
    4:
      begin
        ChangeTool('logic-step', 'resource');
        ClickTool('logic-load');
      end;
    5:
      begin
        Check((ToolValue('logic-resource-name') = 'sample-team') and
          (ToolValue('logic-resource-deadline') = '15000'),
          'ordinary typed form reads the actual declared resource operation');
        ChangeTool('logic-resource-deadline', CResourceDraft);
        ClickTool('tab-ports');
      end;
    6: ClickTool('tab-logic');
    7:
      begin
        Check((ToolValue('logic-resource-deadline') = CResourceDraft) and
          (EncodedPair = FBase), 'tab navigation retains exact unfinished resource input and the accepted pair');
        ClickTool('logic-set');
      end;
    8:
      begin
        Check(not FLogic.HasPendingLogic and (EncodedPair = FBase) and
          (ToolValue('logic-resource-deadline') = CResourceDraft),
          'invalid resource input remains editable without publishing a partial proposal');
        ChangeTool('logic-resource-deadline', '2500');
        ClickTool('logic-set');
      end;
    9:
      begin
        Check(FLogic.HasPendingLogic and (EncodedPair = FBase),
          'valid resource options stage independently from source and design');
        ClickTool('logic-apply');
      end;
    10:
      begin
        Check((ResourceConfiguration.Field('resource').AsText = 'sample-team') and
          (ResourceConfiguration.Field('deadline').AsInteger = 2500) and
          (EncodedPair = FBase), 'actual Apply changes guarded runtime options without rewriting the project');
        ClickTool('tab-ports');
      end;
    11: ClickTool('tab-logic');
    12:
      begin
        Check((ToolValue('logic-resource-name') = 'sample-team') and
          (ToolValue('logic-resource-deadline') = '2500'),
          'accepted resource fields survive leaving and reopening the workbench');
      end;
    13: ClickTool('save-connections');
  end;
end;

procedure TJourney.ClickShell(const AID: TNyxText);
begin
  FStudio.ShellView.ViewFor(AID).Reveal(AID, niRuntime);
  {$ifdef PAS2JS}
  FStudio.ShellView.ElementFor(AID).click;
  {$else}
  Check((FStudio.ShellView.ControlFor(AID) <> nil) and
    FStudio.ShellView.ControlFor(AID).IsVisible, 'ordinary shell button is visible: ' + AID);
  TControlInput(FStudio.ShellView.ControlFor(AID)).Click;
  {$endif}
end;

procedure TJourney.ChangeShell(const AID, AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(FStudio.ShellView.InputFor(AID)).value := AValue;
  FStudio.ShellView.InputFor(AID).dispatchEvent(TJSEvent.new('change'));
  {$else}
  TEdit(FStudio.ShellView.InputFor(AID)).Text := AValue;
  TEdit(FStudio.ShellView.InputFor(AID)).OnChange(FStudio.ShellView.InputFor(AID));
  {$endif}
end;

procedure TJourney.ClickTool(const ASuffix: TNyxText);
begin
  FTarget.Renderer.Reveal(ID(ASuffix), niRuntime);
  {$ifdef PAS2JS}
  FTarget.Renderer.ElementFor(ID(ASuffix)).click;
  {$else}
  Check((FTarget.Renderer.ControlFor(ID(ASuffix)) <> nil) and
    FTarget.Renderer.ControlFor(ID(ASuffix)).IsVisible,
    'ordinary tool button is visible: ' + ASuffix);
  TControlInput(FTarget.Renderer.ControlFor(ID(ASuffix))).Click;
  {$endif}
end;

procedure TJourney.ChangeTool(const ASuffix, AValue: TNyxText);
{$ifndef PAS2JS}
var
  LControl: TControl;
{$endif}
begin
  {$ifdef PAS2JS}

  if FTarget.Renderer.InputFor(ID(ASuffix)).tagName = 'SELECT' then
  begin
    TJSHTMLSelectElement(FTarget.Renderer.InputFor(ID(ASuffix))).value := AValue;
  end
  else
  begin
    TJSHTMLInputElement(FTarget.Renderer.InputFor(ID(ASuffix))).value := AValue;
  end;
  FTarget.Renderer.InputFor(ID(ASuffix)).dispatchEvent(TJSEvent.new('change'));
  {$else}
  LControl := FTarget.Renderer.InputFor(ID(ASuffix));

  if LControl is TComboBox then
  begin

    if FTarget.Renderer.Root.Find(ID(ASuffix)).HasChoiceItems then
    begin
      TComboBox(LControl).ItemIndex :=
        FTarget.Renderer.Root.Find(ID(ASuffix)).ChoiceItems.IndexOf(NyxChoice(AValue));
    end
    else
    begin
      TComboBox(LControl).ItemIndex := TComboBox(LControl).Items.IndexOf(AValue);
    end;
    Check(TComboBox(LControl).ItemIndex >= 0, 'typed graph/step choice is available: ' + AValue);
    TComboBox(LControl).OnChange(LControl);
  end
  else
  begin
    TEdit(LControl).Text := AValue;
    TEdit(LControl).OnChange(LControl);
  end;
  {$endif}
end;

{$ifndef PAS2JS}
function MenuButton(AParent: TWinControl; const ACaption: TNyxText): TControl;
var
  LIndex: Integer;
  LChild: TControl;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LChild := AParent.Controls[LIndex];

    if LChild.IsVisible and ((TControlInput(LChild).Caption = ACaption) or
      (Pos(ACaption + ' ', TControlInput(LChild).Caption) = 1)) then
    begin
      Exit(LChild);
    end;

    if LChild is TWinControl then
    begin
      Result := MenuButton(TWinControl(LChild), ACaption);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;
{$endif}

procedure TJourney.ClickProjectMenu(AOpenBranch: Boolean);
{$ifndef PAS2JS}
var
  LIndex: Integer;
  LButton: TControl;
  LTitle: TNyxText;
  LCaption: TNyxText;
{$endif}
begin
  {$ifdef PAS2JS}

  if AOpenBranch then
  begin
    TJSHTMLElement(document.querySelector('[data-node="studio-menu-project"]')).click;
  end
  else
  begin
    TJSHTMLElement(document.querySelector('[data-node="studio-menu-open"]')).click;
  end;
  {$else}
  LTitle := 'Project';
  LCaption := 'Open project';

  if AOpenBranch then
  begin
    LTitle := 'Component actions';
    LCaption := 'Project';
  end;
  LButton := nil;
  for LIndex := 0 to Screen.FormCount - 1 do
  begin

    if Screen.Forms[LIndex].Visible and (Screen.Forms[LIndex].Caption = LTitle) then
    begin
      LButton := MenuButton(Screen.Forms[LIndex], LCaption);
      Break;
    end;
  end;
  Check((LButton <> nil) and LButton.Enabled, 'actual project menu is available');
  TControlInput(LButton).Click;
  {$endif}
end;

function TJourney.Tick: Boolean;
var
  LView: INyxCanvasWorkspaceView;
  LStore: TNyxProjectPair;
  LTarget: TToolPresentation;
  LEditing: INyxCanvasFlowEditing;
  LLogic: INyxToolCanvasLogicAuthoring;
  {$ifdef PAS2JS}
  LExchangeIndex: Integer;
  LRequestPending: Boolean;
  LArtifact: TJSHTMLElement;
  {$endif}
  {$ifndef PAS2JS}
  LProjectStore: TNyxProjectStore;
  LRevision: TNyxText;
  {$endif}
begin
  Result := FDone;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-json-workshop-studio-stage', IntToStr(FStage));
  document.body.setAttribute('data-json-workshop-studio-checks', IntToStr(FChecks));
  {$endif}

  if FDone then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}

  for LExchangeIndex := 0 to High(FStudio.Exchanges) do
  begin

    if FStudio.Exchanges[LExchangeIndex].RequestPending then
    begin
      FStudio.Exchanges[LExchangeIndex].Deliver;
    end
    else if FStudio.Exchanges[LExchangeIndex].TickPending then
    begin
      FStudio.Exchanges[LExchangeIndex].FireTick;
    end;
  end;
  { A delivered clock tick may queue the shared-project observation. Do not
    drive the visible editor until every pending reply is delivered and its
    exact source/design pair has reached the normal Studio publication path. }
  LRequestPending := False;
  for LExchangeIndex := 0 to High(FStudio.Exchanges) do
  begin
    LRequestPending := LRequestPending or FStudio.Exchanges[LExchangeIndex].RequestPending;
  end;
  {$endif}

  if Clock - FStarted > 240000 then
  begin
    raise Exception.Create('Workshop Studio timed out at stage ' + IntToStr(FStage));
  end;

  if FStudio.SourceBusy or FStudio.PresentationPending then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}

  if LRequestPending or not FStudio.SharedSourceSynchronized then
  begin
    Exit;
  end;
  {$endif}

  if (FTools <> nil) and (FTools.RefreshExecution <> nil) then
  begin

    if FTools.RefreshExecution.Status in [nesPending, nesRunning] then
    begin
      Exit;
    end;
    Check(FTools.RefreshExecution.Status = nesSucceeded, 'tool publication completes successfully');
  end;
  {$ifndef PAS2JS}
  WriteLn('JSON Workshop Studio stage ', FStage);
  Flush(Output);
  {$endif}
  case FStage of
    0:
      begin
        FBase := EncodedPair;

        if FStudio.ShellView.RootFor('action-panel-design') <> nil then
        begin
          ClickShell('action-panel-design');
        end;
      end;
    1: ClickShell('action-preview');
    2:
      begin
        Check(FStudio.CanvasView.Root.Find(FCanvas.ID) <> nil,
          'ordinary Studio mounts the observed Workshop before accessing its canvas');
        LView := FStudio.CanvasView.CanvasView(
          FStudio.CanvasView.Root.Find(FCanvas.ID).Part(NyxPart('workspace')).ID);
        FTools := FStudio.CanvasView.ToolCanvasView(FCanvas.ID);
        { Query into owning locals before retaining facets across ticks. pas2js
          otherwise finalizes out-parameter fields on every later Tick, leaving
          dangling fields and prematurely disposing the actual Tool Canvas. }
        Check(Supports(FTools, TToolPresentation, LTarget), 'ordinary Studio exposes its actual tool view');
        Check(Supports(LView, INyxCanvasFlowEditing, LEditing), 'ordinary canvas has managed graph editing');
        Check(Supports(FTools.Inspector, INyxToolCanvasLogicAuthoring, LLogic),
          'ordinary canvas has its shared Logic workbench');
        FTarget := LTarget;
        FEditing := LEditing;
        FLogic := LLogic;
        ClickTool('tab-logic');
      end;
    3..13:
      begin

        if FResource then
        begin
          RunResourceStage;
        end
        else
        begin
          RunJSONStage;
        end;
      end;
    14:
      begin
        FAfter := EncodedPair;
        Check(FAfter <> FBase, 'Save graphs publishes the configured graphs and Pascal together');
        ReleaseTools;
        ClickShell('action-undo');
      end;
    15:
      begin
        Check(EncodedPair = FBase, 'one Undo restores the exact original graph/source pair');
        ClickShell('action-redo');
      end;
    16:
      begin
        Check(EncodedPair = FAfter, 'one Redo restores the complete configured graph/source pair');
        ClickShell('action-preview');
      end;
    17:
      begin

        if FStudio.ShellView.RootFor('action-panel-project') <> nil then
        begin
          ClickShell('action-panel-project');
        end;
      end;
    18: ClickShell(NyxStudioActionMenuID);
    19:
      begin
        {$ifdef PAS2JS}
        { Popup publication is queued independently from the toolbar command.
          Logical source presence does not establish a clickable mounted menu.
          Keep the original stage/deadline until the actual item is available. }

        if document.querySelector('[data-node="studio-menu-project"]') = nil then
        begin
          Exit;
        end;
        {$endif}
        ClickProjectMenu(True);
      end;
    20:
      begin
        {$ifdef PAS2JS}

        if document.querySelector('[data-node="studio-menu-open"]') = nil then
        begin
          Exit;
        end;
        {$endif}
        ClickProjectMenu(False);
      end;
    21:
      begin
        FSaved := EncodedPair;
        {$ifdef PAS2JS}
        ClickShell('action-project-export');
        {$else}
        ChangeShell('project-file-name', FProjectName);
        ClickShell('action-project-save');
        {$endif}
      end;
    22:
      begin
        {$ifdef PAS2JS}
        Check((Length(FFiles.Exported) = 1) and (FFiles.Exported[0].Text = FSaved),
          'ordinary export delivers the complete accepted pair');
        ClickShell('action-undo');
        {$else}
        LProjectStore := TNyxProjectStore.Create(FDirectory);
        try
          Check(LProjectStore.ReadProject(FProjectName, LRevision) = FSaved,
            'ordinary Save writes the actual isolated project bytes');
        finally
          LProjectStore.Free;
        end;
        {$endif}
      end;
    23:
      begin
        {$ifdef PAS2JS}
        Check(EncodedPair <> FSaved, 'the accepted project changes before ordinary Import');
        ClickShell('action-project-import');
        FFiles.Deliver;
        {$else}
        ClickShell('action-project-open');
        {$endif}
      end;
    24:
      begin
        Check(EncodedPair = FSaved, 'ordinary Open restores exact graphs, controls, placement and Pascal');
        LStore := FStudio.ProjectSnapshot;

        if FResource then
        begin
          Check((Pos('NyxJSONResourceOptions(NyxResourceRef(''sample-team''))',
            LStore.Source) > 0) and (Pos('.WholeRequest(2500)', LStore.Source) > 0),
            'saved crafted Pascal retains specialized resource options');
        end
        else
        begin
          Check(Pos('NewNyxJSONFormat(njlIndented, 4)', LStore.Source) > 0,
            'the saved crafted Pascal carries the meaningful four-space configuration');
        end;
        {$ifdef PAS2JS}
        { Retain the exact ordinary Export/Import packet as inert text, so an
          independent compiler can consume the actual saved constructor. This
          is the packet already checked at both file-delivery boundaries. }
        LArtifact := TJSHTMLElement(document.createElement('script'));
        LArtifact.id := 'workshop-saved-project';
        LArtifact.setAttribute('type', 'application/json');
        LArtifact.textContent := FSaved;
        document.body.appendChild(LArtifact);
        {$endif}
        FDone := True;
        Result := True;
      end;
  end;
  Inc(FStage);
end;

function TJourney.GetPair: TNyxProjectPair;
begin

  if not FDone then
  begin
    raise Exception.Create('Complete ordinary Save/Open before reading the Studio result');
  end;
  Result := DecodeNyxProject(FSaved);
end;

function TJourney.GetChecks: Integer;
begin
  Result := FChecks;
end;

function NewJSONWorkshopStudioJourney(const ADirectory: TNyxText;
  AWidth: Integer): INyxJSONWorkshopStudioJourney;
begin
  Result := TJourney.Create(ADirectory, AWidth);
end;

function NewResourceWorkshopStudioJourney(const ADirectory: TNyxText;
  AWidth: Integer): INyxToolWorkshopStudioJourney;
begin
  Result := TJourney.Create(ADirectory, AWidth, True);
end;

end.
