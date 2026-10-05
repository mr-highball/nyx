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

unit nyx.studio.browser;

{$mode delphi}{$H+}
{$codepage utf8}
{$modeswitch externalclass}

interface

uses
  nyx.types,
  nyx.behavior,
  nyx.text,
  Classes,
  SysUtils,
  JS,
  Web,
  fpjson,
  nyx.model,
  nyx.state,
  nyx.binding.types,
  nyx.studio.authoring,
  nyx.studio.commands,
  nyx.studio.inspector,
  nyx.studio.palette,
  nyx.theme,
  nyx.render.browser,
  nyx.studio.session,
  nyx.studio.source,
  nyx.studio.compiler,
  nyx.studio.diagnostics,
  nyx.studio.agentbridge,
  nyx.studio.agentview,
  nyx.studio.agents,
  nyx.studio.view,
  nyx.studio.builds,
  nyx.studio.outputs,
  nyx.studio.projects, nyx.studio.rootedits, nyx.studio.rootview;

type
  { Transport operation is closed and independent of application build targets. }
  TNyxProjectOperation = (npoOpen, npoSave);
  { Browser shell for the portable Studio session. The shell itself is a Nyx
    document: palette, hierarchy, properties and actions are built with the same
    fluent controls it designs. The canvas and editor are reusable Nyx authoring
    components; Studio consumes their public host/mount APIs. }
  TNyxStudio = class
  private
    FSession: TNyxStudioSession;
    FShell: TNyxDocument;
    FShellRenderer: TNyxBrowserRenderer;
    FCanvasRenderer: TNyxBrowserRenderer;
    FCodeVisible: Boolean;
    FCanvasPercent: Integer;
    FPhone: Boolean;
    FPreview: Boolean;
    FStatus: TNyxText;
    FLog: TNyxText;
    FCompilerReport: INyxCompilerReport;
    FAgentCompilerSequence: Integer;
    FCompiledURL: TNyxText;
    FPendingDesign: TNyxText;
    FPendingSource: TNyxText;
    FRequest: TJSXMLHttpRequest;
    FPalette: TNyxStudioPaletteState;
    FRecoveryEnabled: Boolean;
    FOutputVisible: Boolean;
    FOutputTarget: TNyxText;
    FAdvancedProperties: Boolean;
    FInspectorTab: TNyxInspectorTab;
    FCallbackRemoval: TNyxCallbackRemoval;
    FRootRemoval: INyxRootRemoval;
    FSourceLine: Integer;
    FSourceColumn: Integer;
    FStateVisible: Boolean;
    FBindingsVisible: Boolean;
    FBindingTarget: TNyxBindingProperty;
    FBindingDirection: TNyxBindingDirection;
    FNewStateName: TNyxText;
    FNewStateInput: TNyxStudioStateInput;
    FNewStateValue: TNyxText;
    FCompact: Boolean;
    FPanel: TNyxStudioPanel;
    FResizeHandler: TJSEventHandler;
    FLeftScroll: NativeInt;
    FRightScroll: NativeInt;
    FCanvasScrollTop: NativeInt;
    FCanvasScrollLeft: NativeInt;
    FCanvasViewID: TNyxText;
    FOutputs: TNyxOutputConfiguration;
    FConfigurationRequest: TJSXMLHttpRequest;
    FConfigurationSaving: Boolean;
    FConfigurationDirty: Boolean;
    FConfigurationSent: TNyxText;
    FQueuedScope: TNyxText;
    FQueuedTarget: TNyxText;
    FFilesVisible: Boolean;
    FProjectName: TNyxText;
    FProjectBoundName: TNyxText;
    FProjectRevision: TNyxText;
    FProjectRequest: TJSXMLHttpRequest;
    FProjectOperation: TNyxProjectOperation;
    FProjectSent: TNyxText;
    FProjectRequestName: TNyxText;
    FProjectConflict: TNyxText;
    FImportPacket: TNyxText;
    FImportInput: TJSHTMLInputElement;
    FImportReader: TJSFileReader;
    FImportIndex: Integer;
    FImportDesign: TNyxText;
    FImportPascal: TNyxText;
    FImportSnapshot: TNyxText;
    FImportBoundName: TNyxText;
    FImportRevision: TNyxText;
    FRemoteName: TNyxText;
    FRemoteRevision: TNyxText;
    FAgents: TNyxStudioAgentBridge;
    FAgentsVisible: Boolean;
    procedure AgentRefresh(AContentChanged: Boolean);
    function CreateShell: TNyxDocument;
    { Retain focused new-default drafts across panel/viewport transitions and
      keyboard/programmatic Add actions, independently of project history. }
    procedure CaptureNewStateDraft;
    procedure Refresh(ARetainCanvas: Boolean = False; APreserveDraft: Boolean = False);
    procedure HandleShell(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure HandleCanvas(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Compile(const AScope, ATarget: TNyxText);
    procedure CompilerReady;
    procedure Configuration(ASave: Boolean);
    procedure ConfigurationReady;
    procedure Download(const AName, AText: TNyxText);
    { Recovery uses one browser storage write. Service identity belongs to the
      recovery wrapper, never to portable downloaded project data. }
    procedure SaveRecovery;
    procedure ProjectRequest(AOperation: TNyxProjectOperation);
    procedure ProjectReady;
    procedure ImportProject;
    function ImportChosen(AEvent: TJSEvent): Boolean;
    function ImportRead(AEvent: TJSEvent): Boolean;
    procedure AcceptImport(const APacket: TNyxText; AResolution: TNyxProjectResolution);
    function KeyDown(AEvent: TJSKeyboardEvent): Boolean;
    function ViewportResize(AEvent: TJSEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    { Recovery is enabled for ordinary use. Embedded/test hosts can opt out of
      browser storage without reading or overwriting the user's saved project. }
    procedure Run(ARecovery: Boolean = True);
    { Embedded/native-test browser hosts opt in explicitly. Ordinary Studio
      launches connect automatically alongside recovery, independently of targets. }
    procedure ConnectAgents;
  end;

implementation

uses
  nyx.data,
  nyx.codegen,
  nyx.codec,
  nyx.json,
  nyx.source;

type
  { The installed Web declarations type the third open argument as an object.
    The browser contract accepts a feature string; keep this bridge confined
    to the browser controller and retain the editor through noopener. }
  TNyxReviewWindow = class external name 'Window' (TJSWindow)
    function OpenReview(const AURL, ATarget, AFeatures: String): TJSWindow;
      external name 'open';
  end;

function StudioCSS: TNyxText;
begin
  { Shell recipes target stable design identities rather than browser-only model
    state. Responsive panels remain reachable on narrow screens; canvas width
    emulation changes the rendered view, not the application's global viewport. }
  Result :=
    'html,body{height:100%;margin:0;}body{background:#f3f5fa;}' +
    '[data-node=studio-shell]{height:100vh;height:100dvh;padding:0!important;gap:0!important;overflow:hidden;}' +
    '[data-node=studio-header]{min-height:66px;flex-wrap:wrap!important;flex-shrink:0;padding:12px 20px!important;' +
    'background:#fff;border-bottom:1px solid #dfe3ec;gap:14px!important;}' +
    '[data-node=studio-shell] [data-node=studio-logo]{font-size:24px;font-weight:800;' +
    'letter-spacing:-1px;color:#6858e8;}' +
    '[data-node=studio-subtitle]{font-size:12px;flex:1;overflow-wrap:anywhere;}' +
    '[data-node=studio-header] .nyx-button{padding:7px 12px;font-size:12px;}' +
    '[data-node=studio-workspace]{display:flex!important;flex:1;min-height:0;gap:0!important;' +
    'align-items:stretch!important;flex-wrap:nowrap!important;overflow:hidden;}' +
    '[data-node=studio-panelbar]{flex-shrink:0;padding:8px 12px!important;background:#fff;' +
    'border-bottom:1px solid #dfe3ec;gap:8px!important;flex-wrap:nowrap!important;}' +
    '[data-node=studio-panelbar] .nyx-button{flex:1;min-height:40px;padding:8px;font-size:12px;}' +
    '[data-node=studio-left]{width:230px;flex-shrink:0;overflow:auto;padding:16px!important;' +
    'background:#fff;border-right:1px solid #dfe3ec;gap:12px!important;}' +
    '[data-node=studio-right]{width:265px;flex-shrink:0;overflow:auto;padding:16px!important;' +
    'background:#fff;border-left:1px solid #dfe3ec;gap:10px!important;}' +
    '[data-node=studio-center]{flex:1;min-width:0;min-height:0;gap:0!important;padding:0!important;}' +
    '[data-node=studio-outputs]{padding:16px 20px!important;background:#fff;' +
    'border-bottom:1px solid #dfe3ec;max-height:360px;overflow:auto;flex-shrink:0;}' +
    '[data-node=studio-outputs] .nyx-heading{font-size:18px;}' +
    '[data-node=studio-agents]{margin:0!important;border:0;border-radius:0!important;' +
    'border-bottom:1px solid #dfe3ec;max-height:360px;overflow:auto;flex-shrink:0;}' +
    '[data-node=studio-agents] .nyx-heading{font-size:18px;}' +
    '[data-node=studio-agents-endpoint],[data-node=studio-agents-activity] .nyx-label{overflow-wrap:anywhere;}' +
    '[data-node=studio-agents-permissions]{flex-wrap:wrap;}' +
    '[data-node=output-targets] .nyx-button{font-size:12px;}' +
    '[data-node=output-summary]{font-size:11px;color:#67728a;}' +
    '[data-node=studio-viewbar]{padding:12px 20px!important;background:#fafbfe;border-bottom:1px solid #dfe3ec;' +
    'flex-wrap:wrap!important;flex-shrink:0;}' +
    '[data-node=active-view-label]{overflow-wrap:anywhere;}' +
    '[data-node=studio-viewbar] .nyx-button{font-size:12px;padding:6px 12px;}' +
    '[data-node=studio-canvas-wrap]{flex:1;min-height:0;overflow:auto;padding:28px!important;}' +
    '[data-node=studio-canvas]{background:#fff;border:1px solid #dfe3ec;border-radius:10px;' +
    'box-shadow:0 12px 40px #24304810;width:100%;min-height:440px;margin:0 auto;}' +
    '[data-node=studio-canvas][style*=width]{max-width:none;}' +
    '[data-node=studio-code-actions]{padding:6px 12px!important;flex-wrap:wrap!important;' +
    'flex-shrink:0;background:#fafbfe;border-top:1px solid #dfe3ec;}' +
    '[data-node=studio-code-actions] .nyx-button{font-size:11px;padding:6px 10px;}' +
    '[data-node=studio-shell] [data-node=studio-code]{font:12px/1.7 Consolas,monospace;' +
    'background:#171b29;color:#cbd5ed;' +
    'border:0;border-top:1px solid #343a4e;resize:none;height:0;padding:12px 18px;' +
    'white-space:pre;tab-size:2;outline-offset:-3px;box-sizing:border-box;flex:1;min-height:0;min-width:0;}' +
    '[data-node=studio-source-pane]{overflow:hidden;min-height:0;padding:0!important;gap:0!important;}' +
    '[data-node=studio-source-pane]>:not([data-node=studio-code]){flex-shrink:0;}' +
    '[data-node=studio-split]{min-width:0;min-height:0;overflow:hidden;}' +
    '.nyx-split-divider:focus-visible{outline:2px solid var(--nyx-accent);outline-offset:-3px;}' +
    '[data-node=studio-center]:has([data-node=studio-split]) [data-node=studio-outputs],' +
    '[data-node=studio-center]:has([data-node=studio-split]) [data-node=studio-agents]{max-height:25%;}' +
    '[data-node=studio-footer]{padding:7px 20px!important;font-size:11px;background:#fff;' +
    'border-top:1px solid #dfe3ec;min-height:32px;flex-shrink:0;}' +
    '[data-node=studio-status]{flex:1;}' +
    '[data-node=studio-left] .nyx-heading,[data-node=studio-right] .nyx-heading{font-size:13px;letter-spacing:0;}' +
    '[data-node^=palette-items-]{display:grid!important;gap:6px!important;}' +
    '[data-node^=palette-heading-]{font-size:12px!important;margin:8px 0 2px!important;}' +
    '[data-node^=palette-description-]{font-size:11px;color:var(--nyx-muted);line-height:1.45;}' +
    '[data-node=palette-mode]{flex-wrap:nowrap;}' +
    '[data-node=palette-mode] .nyx-button{flex:1;min-width:0;padding:7px 6px;font-size:12px;}' +
    '[data-node=palette-result-count]{font-size:11px;color:var(--nyx-muted);}' +
    '[data-node=studio-palette] .nyx-button{width:100%;font-size:11px;font-weight:500;' +
    'padding:8px 6px;min-height:34px;text-align:left;}' +
    '[data-node=studio-hierarchy] .nyx-button,[data-node=studio-views] .nyx-button{' +
    'font-size:11px;border:0;width:100%;text-align:left;padding:6px;background:transparent;}' +
    '[data-node=studio-hierarchy] .nyx-button[data-variant=primary],'+
    '[data-node=studio-views] .nyx-button[data-variant=primary]{background:#eeeafd;color:#6858e8;}' +
    '[data-node=studio-right] .nyx-input-control{padding:7px 9px;font-size:12px;}' +
    '[data-node=studio-right] .nyx-memo-control{min-height:70px;padding:7px 9px;font-size:12px;}' +
    '[data-node=studio-hierarchy] .nyx-button{flex:0 0 auto;}' +
    '[data-node=studio-right] .nyx-card{min-width:0;}' +
    '[data-node=studio-right] .nyx-button{overflow-wrap:anywhere;}' +
    '[data-node=studio-log]{max-height:120px;overflow:auto;font-size:11px;padding:10px;}' +
    '.studio-preview-frame{border:0;width:100%;height:620px;}' +
    '@media(max-width:1000px){[data-node=studio-right]{width:225px;}' +
    '[data-node=studio-left]{width:190px;}[data-node=studio-header]{gap:8px!important;}' +
    '[data-node=studio-subtitle]{display:none;}}' +
    '[data-nyx-studio-compact=true] [data-node=studio-left],' +
    '[data-nyx-studio-compact=true] [data-node=studio-right]{width:100%;flex:1;min-height:0;border:0;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-center]{width:100%;min-width:0;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-header]{padding:10px 12px!important;gap:8px!important;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-header] .nyx-button{min-height:40px;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-canvas-wrap]{padding:12px!important;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-viewbar]{padding:10px 12px!important;gap:8px!important;}' +
    '[data-nyx-studio-compact=true] [data-node=output-summary]{width:100%;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-outputs]{max-height:45%;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-agents]{max-height:45%;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-footer]{padding:7px 12px!important;}' +
    '[data-nyx-studio-compact=true] [data-node=studio-palette] .nyx-button{min-height:44px;font-size:13px;}' +
    '[data-nyx-studio-compact=true] [data-node^=palette-description-]{font-size:13px;}' +
    '[data-node=selected-label],[data-node=selected-capabilities]{overflow-wrap:anywhere;}';
end;

constructor TNyxStudio.Create;
begin
  inherited Create;
  FSession := TNyxStudioSession.Create;
  FAgents := TNyxStudioAgentBridge.Create(FSession, @AgentRefresh);
  FOutputs := TNyxOutputConfiguration.Create;
  FShellRenderer := TNyxBrowserRenderer.Create;
  FShellRenderer.OnEvent := HandleShell;
  FCanvasRenderer := TNyxBrowserRenderer.Create;
  FCanvasRenderer.OnEvent := HandleCanvas;
  FCodeVisible := False;
  FCanvasPercent := 65;
  FPalette := DefaultNyxStudioPaletteState;
  FBindingTarget := bpValue;
  FBindingDirection := bdTwoWay;
  FNewStateInput := ssiText;
  FPanel := nspDesign;
  FResizeHandler := ViewportResize;
  FRecoveryEnabled := True;
  FStatus := 'Ready to design';
end;

destructor TNyxStudio.Destroy;
begin
  FAgents.Free;
  window.removeEventListener('resize', FResizeHandler);

  if FProjectRequest <> nil then
  begin
    FProjectRequest.onreadystatechange := nil;
    FProjectRequest.abort;
  end;

  if FImportReader <> nil then
  begin
    FImportReader.onload := nil;
    FImportReader.onerror := nil;
    FImportReader.abort;
  end;

  if FImportInput <> nil then
  begin
    FImportInput.onchange := nil;
    FImportInput.remove;
  end;

  if FRequest <> nil then
  begin
    FRequest.onreadystatechange := nil;
    FRequest.abort;
  end;

  if FConfigurationRequest <> nil then
  begin
    FConfigurationRequest.onreadystatechange := nil;
    FConfigurationRequest.abort;
  end;
  FCanvasRenderer.Free;
  FShellRenderer.Free;
  FShell.Free;
  FSession.Free;
  FOutputs.Free;
  inherited Destroy;
end;

function TNyxStudio.CreateShell: TNyxDocument;
var
  LState: TNyxStudioViewState;
begin
  LState := DefaultNyxStudioViewState;
  LState.CodeVisible := FCodeVisible;
  LState.CanvasPercent := FCanvasPercent;
  LState.Compact := FCompact;
  LState.Panel := FPanel;
  LState.Phone := FPhone;
  LState.Palette := FPalette;
  LState.Log := FLog;
  LState.Status := FStatus;
  LState.OutputVisible := FOutputVisible;
  LState.OutputTarget := FOutputTarget;
  LState.Outputs := FOutputs;
  LState.FilesVisible := FFilesVisible;
  LState.ProjectName := FProjectName;
  LState.ProjectConflict := FProjectConflict <> '';
  LState.ImportConflict := FImportPacket <> '';
  LState.ProjectBusy := FProjectRequest <> nil;
  LState.AdvancedProperties := FAdvancedProperties;
  LState.InspectorTab := FInspectorTab;
  LState.CallbackRemoval := FCallbackRemoval;

  if FRootRemoval <> nil then
  begin
    LState.RootRemoval := FRootRemoval.Inspect;
  end;
  LState.StateVisible := FStateVisible;
  LState.BindingsVisible := FBindingsVisible;
  LState.BindingTarget := FBindingTarget;
  LState.BindingDirection := FBindingDirection;
  LState.NewStateName := FNewStateName;
  LState.NewStateInput := FNewStateInput;
  LState.NewStateValue := FNewStateValue;
  LState.AgentsVisible := FAgentsVisible;
  LState.Agents := FAgents.State;
  Result := BuildNyxStudioView(FSession, LState, FCompilerReport);
end;

procedure TNyxStudio.CaptureNewStateDraft;
var
  LActive: TJSHTMLElement;
  LField: TJSHTMLElement;
  LNode: TNyxNode;
  LID: TNyxText;
  LValue: TNyxText;
begin
  LActive := TJSHTMLElement(document.activeElement);

  if (LActive = nil) or (FShellRenderer.Root = nil) or
    not ((LActive is TJSHTMLInputElement) or (LActive is TJSHTMLTextAreaElement)) then
  begin
    Exit;
  end;
  LField := TJSHTMLElement(LActive.closest('[data-node]'));

  if LField = nil then
  begin
    Exit;
  end;
  LID := LField.getAttribute('data-node');

  if (LID <> NyxStudioNewStateNameID) and (LID <> NyxStudioNewStateValueID) then
  begin
    Exit;
  end;
  LValue := TJSHTMLInputElement(LActive).value;

  if LID = NyxStudioNewStateNameID then
  begin
    FNewStateName := LValue;
  end
  else
  begin
    FNewStateValue := LValue;
  end;
  LNode := FShellRenderer.Root.Find(LID);

  if LNode <> nil then
  begin
    LNode.Configure.Value(LValue).Done;
  end;
end;

procedure TNyxStudio.Refresh(ARetainCanvas, APreserveDraft: Boolean);
var
  LCanvas: TJSHTMLElement;
  LStyle: TJSHTMLElement;
  LPrevious: TJSHTMLElement;
  LActive: TJSHTMLElement;
  LSameView: Boolean;
  LFocusID: TNyxText;
  LFocusValue: TNyxText;
  LField: TJSHTMLElement;
  LReplacement: TJSHTMLElement;
  LCodeStart: NativeInt;
  LCodeEnd: NativeInt;
  LCodeScroll: NativeInt;
  LSplit: TNyxNode;
begin
  LCodeStart := -1;
  LCodeEnd := -1;
  LCodeScroll := 0;
  { Geometry is local editor presentation. Capture the mounted proportion before
    a shell rebuild, including a completed or in-progress touch adjustment. }

  if FShellRenderer.Root <> nil then
  begin
    LSplit := FShellRenderer.Root.Find('studio-split');

    if LSplit <> nil then
    begin
      FCanvasPercent := StrToIntDef(LSplit.Prop('split-position'), FCanvasPercent);
    end;
  end;

  if APreserveDraft then
  begin
    CaptureNewStateDraft;
  end;
  { Output profile responses may arrive while the user is typing. They update
    tooling/chrome, not the accepted design; preserve that uncommitted draft.
    Admission/rollback refreshes deliberately retain their default reset policy. }
  LFocusID := '';
  LFocusValue := '';
  LActive := TJSHTMLElement(document.activeElement);

  if APreserveDraft and (LActive <> nil) and
    ((LActive is TJSHTMLInputElement) or (LActive is TJSHTMLTextAreaElement) or
    (LActive is TJSHTMLSelectElement)) and
    (LActive.closest('[data-node=studio-canvas]') = nil) then
  begin
    LField := TJSHTMLElement(LActive.closest('[data-node]'));

    if LField <> nil then
    begin
      LFocusID := LField.getAttribute('data-node');
      LFocusValue := TJSHTMLInputElement(LActive).value;

      if LFocusID = 'studio-code' then
      begin
        { An explicit Restore accepted command may have discarded this buffer.
          Use the session's current draft policy when retaining field focus. }
        LFocusValue := FSession.DraftSource;
        LCodeStart := TJSHTMLTextAreaElement(LActive).selectionStart;
        LCodeEnd := TJSHTMLTextAreaElement(LActive).selectionEnd;
        LCodeScroll := LActive.scrollTop;
      end;
    end;
  end;
  { Keep panel scroll positions while a model command replaces the shell. Stable
    selection IDs allow the new inspector to follow undo/redo and imported data. }
  LPrevious := TJSHTMLElement(document.querySelector('[data-node=studio-left]'));

  if LPrevious <> nil then
  begin
    FLeftScroll := LPrevious.scrollTop;
  end;
  LPrevious := TJSHTMLElement(document.querySelector('[data-node=studio-right]'));

  if LPrevious <> nil then
  begin
    FRightScroll := LPrevious.scrollTop;
  end;
  LSameView := FCanvasViewID = FSession.ActiveViewID;
  LPrevious := TJSHTMLElement(document.querySelector('[data-node=studio-canvas-wrap]'));
  LActive := nil;

  if (LPrevious <> nil) and LSameView then
  begin
    FCanvasScrollTop := LPrevious.scrollTop;
    FCanvasScrollLeft := LPrevious.scrollLeft;

    if LPrevious.contains(document.activeElement) then
    begin
      LActive := TJSHTMLElement(document.activeElement);
    end;
  end;
  ARetainCanvas := ARetainCanvas and (LPrevious <> nil) and LSameView and
    (not FCompact or (FPanel = nspDesign));

  if not ARetainCanvas then
  begin
    FCanvasRenderer.Unmount;
    LActive := nil;
  end;
  FShell.Free;
  FShell := CreateShell;
  FShellRenderer.Render(FShell, FShell.Pages[0], TJSHTMLElement(document.body));
  FShellRenderer.ElementFor('studio-shell').setAttribute('data-nyx-studio-compact',
    LowerCase(BoolToStr(FCompact, True)));
  LStyle := TJSHTMLElement(document.getElementById('nyx-studio-style'));

  if LStyle = nil then
  begin
    LStyle := TJSHTMLElement(document.createElement('style'));
    LStyle.id := 'nyx-studio-style';
    LStyle.textContent := StudioCSS;
    document.head.appendChild(LStyle);
  end;
  LCanvas := nil;

  if FShell.Find('studio-canvas') <> nil then
  begin
    LCanvas := FShellRenderer.ElementFor('studio-canvas');
  end;

  if (LCanvas <> nil) and ARetainCanvas then
  begin
    { Selection/property chrome may change without replacing live canvas fields.
      Retain their DOM, draft values, selection ranges and event bindings. }
    FCanvasRenderer.MoveHost(LCanvas);
    FCanvasRenderer.Select(FSession.SelectedID);
  end
  else if (LCanvas <> nil) and (FCompiledURL <> '') then
  begin
    FCanvasRenderer.RenderCompiled(FCompiledURL, LCanvas);
  end
  else if (LCanvas <> nil) and (FSession.ActiveView <> nil) then
  begin
    FCanvasRenderer.Render(FSession.Document, FSession.ActiveView, LCanvas, not FPreview);
    FCanvasRenderer.Select(FSession.SelectedID);
  end;
  LPrevious := TJSHTMLElement(document.querySelector('[data-node=studio-canvas-wrap]'));

  if LPrevious <> nil then
  begin

    if not LSameView then
    begin
      FCanvasScrollTop := 0;
      FCanvasScrollLeft := 0;
    end;
    NyxFocusWithoutScroll(LActive);
    LPrevious.scrollTop := FCanvasScrollTop;
    LPrevious.scrollLeft := FCanvasScrollLeft;
    FCanvasViewID := FSession.ActiveViewID;
  end;

  if (LFocusID <> '') and (FShell.Find(LFocusID) <> nil) then
  begin
    LReplacement := FShellRenderer.ElementFor(LFocusID);

    if not (LReplacement is TJSHTMLTextAreaElement) and
      not (LReplacement is TJSHTMLInputElement) and
      not (LReplacement is TJSHTMLSelectElement) then
    begin
      LReplacement := TJSHTMLElement(LReplacement.querySelector('input,textarea,select'));
    end;

    if LReplacement <> nil then
    begin
      TJSHTMLInputElement(LReplacement).value := LFocusValue;
      NyxFocusWithoutScroll(LReplacement);

      if (LFocusID = 'studio-code') and (LCodeStart >= 0) then
      begin
        TJSHTMLTextAreaElement(LReplacement).selectionStart := LCodeStart;
        TJSHTMLTextAreaElement(LReplacement).selectionEnd := LCodeEnd;
        LReplacement.scrollTop := LCodeScroll;
      end;
    end;
  end;
  LPrevious := TJSHTMLElement(document.querySelector('[data-node=studio-left]'));

  if LPrevious <> nil then
  begin
    LPrevious.scrollTop := FLeftScroll;
  end;
  LPrevious := TJSHTMLElement(document.querySelector('[data-node=studio-right]'));

  if LPrevious <> nil then
  begin
    LPrevious.scrollTop := FRightScroll;
  end;
  document.body.setAttribute('data-nyx-studio-ready', 'true');
  document.title := FSession.Document.Title + ' / Nyx Studio';
  try

    if FRecoveryEnabled then
    begin
      SaveRecovery;
      { Only the optional choice is browser-local. Private compiler paths are
        persisted by the service, never exported with the design or Pascal. }
      window.localStorage.setItem('nyx-studio-output-target-v1', FOutputTarget);
      window.localStorage.setItem(NyxStudioPalettePreferencesKey,
        EncodeNyxStudioPalettePreferences(FPalette));
    end;
  except
    { Storage may be denied or full. The in-memory design and explicit downloads
      remain usable, so persistence failure must not discard accepted edits. }
    FStatus := 'Automatic recovery unavailable. Download a project backup to keep your work.';
  end;
  FAgents.RecordLocal;
end;

procedure TNyxStudio.ConnectAgents;
begin
  FAgents.Connect;
end;

procedure TNyxStudio.AgentRefresh(AContentChanged: Boolean);
var
  LState: TNyxStudioAgentView;
  LActivity: TNyxDataValue;
begin
  LState := FAgents.State;

  if (LState.Compiler.Kind = ndObject) and
    (LState.Compiler.Field('sequence').AsInteger <> FAgentCompilerSequence) and
    FAgents.SourceSynchronized then
  begin

    if LState.Compiler.Field('total').AsInteger = 0 then
    begin
      FAgentCompilerSequence := LState.Compiler.Field('sequence').AsInteger;
      FCompilerReport := nil;
    end
    else if LState.Compiler.Field('acceptedSource').AsBoolean then
    begin
      FAgentCompilerSequence := LState.Compiler.Field('sequence').AsInteger;
      { Reuse this observer's exact accepted unit only after service admission.
        The ordinary diagnostic panel retains its draft/source navigation guards.
        The MCP job exposes additional pages when the report exceeds twenty. }
      FCompilerReport := DecodeNyxCompilerReport(NyxObject([
        NyxField('version', NyxData(1)), NyxField('source', NyxData(FSession.Source)),
        NyxField('items', LState.Compiler.Field('items'))]).ToJSON);
    end;
  end;

  if AContentChanged then
  begin
    FCompiledURL := '';
    FStatus := 'Shared design updated · revision ' + IntToStr(LState.Revision);
  end;

  if LState.Activity.Count > 0 then
  begin
    LActivity := LState.Activity.Item(LState.Activity.Count - 1);
    FStatus := LActivity.Field('actor').AsText + ' · ' +
      LActivity.Field('operation').AsText + ' · ' + LActivity.Field('outcome').AsText;
  end;

  if LState.Conflict then
  begin
    FAgentsVisible := True;
    FStatus := LState.Status;
  end;
  Refresh(not AContentChanged, True);
end;

procedure TNyxStudio.HandleCanvas(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  try

    if AEvent.Trigger = ntDesignSelect then
    begin
      FSession.Select(ANode.DesignID);
      FAgents.RecordLocal;
      FCanvasRenderer.Select(FSession.SelectedID);
      { Compact inspectors are rebuilt when opened. Keep the design pane in
        place during a tap so mobile focus and its virtual keyboard stay native. }

      if not FCompact then
      begin
        Refresh(True);
      end;
    end
    else if AEvent.Trigger = ntDesignValue then
    begin
      FSession.SetCanvasValue(ANode);
      FCompiledURL := '';
      FStatus := 'Design updated / Pascal generated';
      Refresh(True);
    end;
  except
    on LException: Exception do
    begin
      FStatus := LException.Message;
      { A rejected canvas command may have restored its authored snapshot.
        Reproject accepted values and preserve the containing view's scroll. }
      Refresh;
    end;
  end;
end;

procedure TNyxStudio.HandleShell(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LSource: TNyxText;
  LRetainCanvas: Boolean;
  LAcceptedDesign: TNyxText;
  LInspectorEffect: TNyxInspectorEffect;
  LRemoval: TNyxCallbackRemoval;
  LSourceLine: Integer;
  LDiagnostic: TNyxSourceDiagnostic;
  LCompilerPanel: TNyxNode;
  LCompilerIndex: Integer;
begin
  LRetainCanvas := False;
  CaptureNewStateDraft;
  LAcceptedDesign := '';

  if (ANode.ID = 'studio-split') and (AEvent.Trigger = ntChange) then
  begin
    { The public control already resized its existing DOM. No source edit,
      remount or project history entry belongs to this presentation choice. }
    FCanvasPercent := StrToIntDef(ANode.Prop('split-position'), FCanvasPercent);
    Exit;
  end;

  if (AEvent.Trigger = ntChange) and (ANode.ID = 'studio-code') then
  begin
    { The editor is a normal Nyx control. Retain its live DOM, cursor and scroll
      while typing; applying is one explicit atomic command, not a remount per
      keystroke. Pending drafts remain visible across shell/viewport changes. }
    RouteNyxStudioSource(FSession, ANode, AEvent.Trigger);
    LCompilerPanel := FShellRenderer.Root.Find(NyxStudioCompilerDiagnosticsID);

    if LCompilerPanel <> nil then
    begin
      { Update the mounted action while preserving editor focus/selection. The
        route independently checks the fresh accepted pair before navigation. }
      for LCompilerIndex := 0 to LCompilerPanel.Count - 1 do
      begin

        if LCompilerPanel.Children[LCompilerIndex].Extensions.Has(
          NyxExtension(NyxStudioCompilerIndexKey)) then
        begin
          LCompilerPanel.Children[LCompilerIndex].Configure.Enabled(
            (FCompilerReport <> nil) and
            (ANode.Prop('value') = FCompilerReport.Source)).Done;
        end;
      end;
      FShellRenderer.Sync;
    end;
    { Keep typing in the mounted editor. Only the old diagnostic is hidden;
      its navigation command also checks the exact draft if a stale event fires. }

    if not FSession.SourceDiagnostic.Defined and
      (FShellRenderer.Root.Find(NyxStudioDiagnosticID) <> nil) and
      (FShellRenderer.Root.Find(NyxStudioDiagnosticID).Prop('visible') <> 'false') then
    begin
      FShellRenderer.Root.Find(NyxStudioDiagnosticID).Configure.Visible(False).Done;
      FShellRenderer.Sync;
    end;
    FStatus := 'Pascal draft / apply when ready';
    FAgents.RecordLocal;
    FShellRenderer.ElementFor('studio-status').textContent := FStatus;

    if FRecoveryEnabled then
    begin
      try
        SaveRecovery;
      except
        { The live buffer and explicit draft download remain available. }
      end;
    end;
    Exit;
  end;

  if (ANode.Prop(NyxStudioStateCommandKey) <> '') or
    (ANode.Prop(NyxStudioBindingCommandKey) <> '') or
    (ANode.ID = NyxStudioAddStateID) or (ANode.ID = NyxStudioBindingFlowID) or
    (ANode.ID = 'action-apply-source') then
  begin
    LAcceptedDesign := FSession.Save;
  end;
  try

    if RouteNyxCompilerDiagnostic(FSession, ANode, AEvent.Trigger,
      FCompilerReport, LDiagnostic) then
    begin
      FCodeVisible := True;
      FPanel := nspDesign;
      FSourceLine := LDiagnostic.Line;
      FSourceColumn := LDiagnostic.Column;
      LRetainCanvas := True;
    end
    else if RouteNyxStudioEvents(FSession, ANode, AEvent.Trigger, FCallbackRemoval,
      LInspectorEffect, LSourceLine, LRemoval) then
    begin
      case LInspectorEffect of
        nieSource:
          begin
            FSourceLine := LSourceLine;
            FCodeVisible := True;
            FPanel := nspDesign;
            FStatus := 'Pascal callback / line ' + IntToStr(LSourceLine);
          end;
        nieRequestRemoval:
          begin
            FCallbackRemoval := LRemoval;
            FStatus := 'Review callback removal';
          end;
        nieCancelRemoval, nieRemoved:
          begin
            FCallbackRemoval.Pending := False;
            FStatus := 'Callback registrations updated';
          end;
      end;
      FCompiledURL := '';
      LRetainCanvas := True;
    end
    else if RouteNyxStudioAuthoring(FSession, ANode, AEvent.Trigger, FShellRenderer.Root) then
    begin

      if ANode.ID = NyxStudioAddStateID then
      begin
        FNewStateName := '';
      end;
      FCompiledURL := '';
      FStatus := 'State / bindings updated / Pascal generated';
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = NyxStudioNewStateNameID) then
    begin
      { Draft edits do not replace the button currently being pressed after
        field blur. Only Add creates project data/history; refreshes read these
        retained drafts, and viewport changes preserve a focused pending value. }
      FNewStateName := ANode.Prop('value');
      Exit;
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = NyxStudioNewStateValueID) then
    begin
      FNewStateValue := ANode.Prop('value');
      Exit;
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = NyxStudioNewStateInputID) then
    begin

      if not TryNyxStudioStateInput(ANode.Prop('value'), FNewStateInput) then
      begin
        raise ENyxState.Create('Unknown new default type');
      end;
      case FNewStateInput of
        ssiText:
          begin
            FNewStateValue := '';
          end;
        ssiEscapedText:
          begin
            FNewStateValue := '""';
          end;
        ssiBoolean:
          begin
            FNewStateValue := 'false';
          end;
        ssiInteger, ssiNumber:
          begin
            FNewStateValue := '0';
          end;
      end;
      LRetainCanvas := True;
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = NyxStudioBindingTargetID) then
    begin

      if not TryNyxStudioBindingTarget(ANode.Prop('value'), FBindingTarget) then
      begin
        raise ENyxState.Create('Unknown control binding target');
      end;
      LRetainCanvas := True;
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = NyxStudioBindingFlowID) then
    begin

      if not TryNyxStudioBindingDirection(ANode.Prop('value'), FBindingDirection) then
      begin
        raise ENyxState.Create('Unknown binding flow');
      end;
      LRetainCanvas := True;
    end
    else if RouteNyxStudioProperty(FSession, ANode, AEvent.Trigger) then
    begin
      FCompiledURL := '';
      FStatus := 'Design updated / Pascal generated';
    end
    else if RouteNyxStudioPalette(ANode, AEvent.Trigger, FPalette) then
    begin
      { Finding controls changes editor presentation, preserving the mounted
        preview, focus/scroll and all project history. }
      LRetainCanvas := True;
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = 'project-title') then
    begin
      FSession.SetTitle(ANode.Prop('value'));
      FCompiledURL := '';
      FStatus := 'Project title updated';
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = 'project-file-name') then
    begin
      FProjectName := ANode.Prop('value');
      LRetainCanvas := True;
    end
    else if (AEvent.Trigger = ntChange) and (ANode.Prop('output-field') <> '') then
    begin
      FOutputs.SetField(ANode.Prop('output-field'), ANode.Prop('value'));
      FConfigurationDirty := True;
      FStatus := 'Output configuration edited / applied when building';
    end
    else if AEvent.Trigger = ntClick then
    begin

      if ANode.Extensions.Has(NyxStudioReviewPreviewKey) then
      begin
        { A new observation tab retains this editor's unsent/local draft and
          input focus. The URL is operator metadata, never an application edit. }
        TNyxReviewWindow(window).OpenReview(ANode.Extensions.Value(NyxStudioReviewPreviewKey).AsText,
          '_blank', 'noopener');
        LRetainCanvas := True;
      end
      else if (ANode.ID = 'output-none') or (ANode.ID = 'output-browser') or
        (ANode.ID = 'output-lcl') then
      begin
        FOutputTarget := ANode.Prop('output-target');
        ValidateNyxOutputTarget(FOutputTarget);
        FStatus := 'Output choice updated / design retained';
      end
      else if ANode.Prop('add-kind') <> '' then
      begin
        FSession.AddKind(ANode.Prop('add-kind'));
        FCompiledURL := '';
        FPanel := nspDesign;
      end
      else if ANode.Prop('select-id') <> '' then
      begin
        FSession.Select(ANode.Prop('select-id'));
      end
      else if ANode.Prop('override-path') <> '' then
      begin
        FSession.CustomizePart(ANode.Prop('override-path'));
        FCompiledURL := '';
      end
      else if ANode.Prop('view-id') <> '' then
      begin
        FSession.Activate(ANode.Prop('view-id'));
        FCompiledURL := '';
        FPanel := nspDesign;
      end
      else if ANode.Prop('component-id') <> '' then
      begin
        FSession.AddComponentInstance(ANode.Prop('component-id'));
        FCompiledURL := '';
        FPanel := nspDesign;
      end
      else
      begin
        case ANode.ID of
          'action-undo':
            begin

              if FAgents.Enabled then FAgents.History('undo')
              else FSession.Undo;
            end;
          NyxStudioReviewRootID, NyxStudioCancelRootID, NyxStudioRemoveRootID:
            begin
              case RouteNyxRootRemoval(FSession, ANode.ID, AEvent.Trigger, FRootRemoval) of
                nreReview:
                  begin
                    FPanel := nspDesign;
                    FStatus := 'Review view removal';
                    LRetainCanvas := True;
                  end;
                nreCancel:
                  begin
                    FStatus := 'View retained';
                    LRetainCanvas := True;
                  end;
                nreRemoved:
                  begin
                    FCompiledURL := '';
                    FStatus := 'View removed / one Undo restores it';
                  end;
              end;
            end;
          'action-redo':
            begin

              if FAgents.Enabled then FAgents.History('redo')
              else FSession.Redo;
            end;
          'action-agents': FAgentsVisible := not FAgentsVisible;
          'action-agent-connect': ConnectAgents;
          'action-agent-disabled': FAgents.Configure(apDisabled);
          'action-agent-readOnly': FAgents.Configure(apReadOnly);
          'action-agent-edit': FAgents.Configure(apEdit);
          'action-agent-pause': FAgents.Pause;
          'action-agent-accept':
            begin
              Download('local-project-before-agent-sync.nyxproject', EncodeNyxProject(FSession.ProjectSnapshot));
              FAgents.AcceptRemote;
            end;
          NyxStudioDiagnosticGoID:
            begin
              RouteNyxSourceDiagnostic(FSession, ANode, AEvent.Trigger, LDiagnostic);
              FCodeVisible := True;
              FPanel := nspDesign;
              FSourceLine := LDiagnostic.Line;
              FSourceColumn := LDiagnostic.Column;
              LRetainCanvas := True;
            end;
          'action-apply-source':
            begin
              RouteNyxStudioSource(FSession, ANode, AEvent.Trigger);
              FCompiledURL := '';
              FStatus := 'Pascal applied / design updated';
            end;
          'action-reset-source':
            begin
              RouteNyxStudioSource(FSession, ANode, AEvent.Trigger);
              FStatus := 'Accepted Pascal restored';
              LRetainCanvas := True;
            end;
          'action-delete': FSession.DeleteSelected;
          'action-duplicate': FSession.DuplicateSelected;
          'action-up': FSession.MoveSelected(-1);
          'action-down': FSession.MoveSelected(1);
          'action-add-page': FSession.AddPage;
          'action-component': FSession.CreateComponent;
          'action-panel-project':
            begin
              FPanel := nspProject;
            end;
          'action-panel-design':
            begin
              FPanel := nspDesign;
            end;
          'action-panel-inspector':
            begin
              FPanel := nspInspector;
            end;
          'action-code':
            begin
              FCodeVisible := not FCodeVisible;
              FPanel := nspDesign;
            end;
          'action-outputs':
            begin
              FOutputVisible := not FOutputVisible;
              FPanel := nspDesign;
            end;
          'action-advanced-properties': FAdvancedProperties := not FAdvancedProperties;
          NyxInspectorPropertiesID:
            begin
              FInspectorTab := nitProperties;
              LRetainCanvas := True;
            end;
          NyxInspectorEventsID:
            begin
              FInspectorTab := nitEvents;
              LRetainCanvas := True;
            end;
          NyxStudioStateToggleID:
            begin
              FStateVisible := not FStateVisible;
              LRetainCanvas := True;
            end;
          NyxStudioBindingsToggleID:
            begin
              FBindingsVisible := not FBindingsVisible;
              LRetainCanvas := True;
            end;
          'action-desktop': FPhone := False;
          'action-phone': FPhone := True;
          'action-preview': FPreview := not FPreview;
          'action-save', 'action-project-save':
            begin
              ProjectRequest(npoSave);
              Exit;
            end;
          'action-export-source': Download(NyxCompanionUnitName(FSession.Source) + '.pas', FSession.Source);
          'action-export-source-draft': Download('nyx.view.draft.pas', FSession.DraftSource);
          'action-import':
            begin
              FFilesVisible := not FFilesVisible;
              FPanel := nspProject;
              LRetainCanvas := True;
            end;
          'action-project-open':
            begin
              ProjectRequest(npoOpen);
              Exit;
            end;
          'action-project-export':
            Download('project.nyxproject', EncodeNyxProject(FSession.ProjectSnapshot));
          'action-project-export-files':
            begin
              Download('design.nyx', FSession.Save);
              Download(NyxCompanionUnitName(FSession.Source) + '.pas', FSession.Source);
              FStatus := 'Accepted files exported; project backup also includes any draft';
            end;
          'action-project-import':
            begin
              ImportProject;
              Exit;
            end;
          'action-project-use-remote':
            begin
              Download('my-project-before-open.nyxproject', EncodeNyxProject(FSession.ProjectSnapshot));
              FImportBoundName := FRemoteName;
              FImportRevision := FRemoteRevision;
              AcceptImport(FProjectConflict, nprRequireMatch);
              FProjectConflict := '';
            end;
          'action-project-copy':
            begin
              FProjectName := FProjectName + '-copy';
              FProjectConflict := '';
              FStatus := 'Choose a new name, then save your separate project';
              LRetainCanvas := True;
            end;
          'action-project-input-backup': Download('imported-project.nyxproject', FImportPacket);
          'action-project-use-pascal': AcceptImport(FImportPacket, nprUsePascal);
          'action-project-use-design': AcceptImport(FImportPacket, nprUseDesign);
          'action-project-cancel-import':
            begin
              FImportPacket := '';
              FStatus := 'Import cancelled; current project retained';
              LRetainCanvas := True;
            end;
          'action-build-view':
            begin
              Compile('view', FOutputTarget);
              Exit;
            end;
          'action-build-app':
            begin
              Compile('application', FOutputTarget);
              Exit;
            end;
          'action-save-outputs':
            begin
              Configuration(True);
              Exit;
            end;
          'action-reload-outputs':
            begin

              if FConfigurationRequest = nil then
              begin
                FConfigurationDirty := False;
              end;
              Configuration(False);
              Exit;
            end;
        end;
      end;
    end
    else
    begin
      Exit;
    end;
    Refresh(LRetainCanvas, LRetainCanvas);

    if FSourceLine > 0 then
    begin

      if FSourceColumn < 1 then
      begin
        FSourceColumn := 1;
      end;
      FShellRenderer.NavigateCodeLine('studio-code', FSourceLine, FSourceColumn);
      FSourceLine := 0;
      FSourceColumn := 0;
    end;
  except
    on LException: Exception do
    begin
      FStatus := LException.Message;
      FSourceLine := 0;
      FSourceColumn := 0;
      { Session admission may have rolled back a rejected edit. Rebuild typed
        fields from that accepted state before reporting the diagnostic. }
      try
        Refresh((LAcceptedDesign <> '') and (FSession.Save = LAcceptedDesign));
      except
        { A missing custom projection may still be awaiting its adapter. The
          shell/status remain available for recovery through tree/property edits. }
      end;
      { Keep a diagnostic accessible even if the requested design is not yet
        renderable (for example an unresolved reusable reference being edited). }
      TJSHTMLElement(document.querySelector('[data-node=studio-status]')).textContent := FStatus;
    end;
  end;
end;

procedure TNyxStudio.Compile(const AScope, ATarget: TNyxText);
begin
  FPanel := nspDesign;
  { Selecting an output is deferred until a build. Designing, source generation
    and persistence remain usable with no compiler profile or service response. }

  if ATarget = '' then
  begin
    FOutputVisible := True;
    FStatus := 'Choose an output to build / your design is ready';
    Refresh;
    Exit;
  end;

  if (FConfigurationRequest <> nil) or FConfigurationDirty then
  begin
    FQueuedScope := AScope;
    FQueuedTarget := ATarget;

    if FConfigurationRequest = nil then
    begin
      Configuration(True);
    end;
    Exit;
  end;

  if FRequest <> nil then
  begin
    FStatus := 'A build is already running';
    Refresh;
    Exit;
  end;
  { A pending or rejected editor buffer must never become executable merely
    because the user builds. Apply pairs source/design through candidate admission;
    Restore deliberately discards the draft. Both actions are already available. }

  if FSession.DraftSource <> FSession.Source then
  begin
    FCodeVisible := True;
    FStatus := 'Apply Pascal or restore accepted source before building';
    Refresh(True, True);
    Exit;
  end;
  FPendingDesign := FSession.Save;
  FPendingSource := FSession.Source;
  FCompilerReport := nil;
  FStatus := 'Building ' + AScope + ' / ' + ATarget;
  Refresh;
  FRequest := TJSXMLHttpRequest.new;
  FRequest.open('POST', 'api/build?source=companion&scope=' + AScope + '&target=' + ATarget +
    '&page=' + encodeURIComponent(FSession.ActiveViewID), True);
  FRequest.setRequestHeader('Content-Type', 'application/json');
  FRequest.timeout := 65000;
  FRequest.onreadystatechange := CompilerReady;
  FRequest.send(EncodeNyxBuildRequest(FSession.Document, FPendingSource));
end;

procedure TNyxStudio.CompilerReady;
var
  LData: TJSObject;
  LText: TNyxText;
  LStatus: Integer;
begin

  if (FRequest = nil) or (FRequest.readyState <> TJSXMLHttpRequest.DONE) then
  begin
    Exit;
  end;
  LText := FRequest.responseText;
  LStatus := FRequest.Status;
  FRequest := nil;
  try
    LData := TJSJSON.parseObject(LText);
    FCompilerReport := nil;

    if isObject(LData['diagnostics']) then
    begin
      FCompilerReport := DecodeNyxCompilerReport(TJSJSON.stringify(LData['diagnostics']));

      if FCompilerReport.Source <> FPendingSource then
      begin
        FCompilerReport := nil;
        raise ENyxModel.Create('Compiler diagnostics do not belong to the submitted Pascal');
      end;
    end;

    if (LStatus <> 200) or not Boolean(LData['ok']) then
    begin
      FStatus := 'Build failed / see diagnostics';
      FLog := '';

      if isString(LData['log']) then
      begin
        FLog := TNyxText(LData['log']);
      end;

      if isString(LData['error']) then
      begin
        FLog := FLog + TNyxText(LData['error']);
      end;
      FOutputVisible := True;

      if (FCompilerReport <> nil) and (FCompilerReport.Count > 0) then
      begin
        FCodeVisible := True;
        FPanel := nspDesign;
        FOutputVisible := False;
      end;
    end
    else if (FPendingDesign <> FSession.Save) or
      (FPendingSource <> FSession.Source) or
      (FSession.DraftSource <> FSession.Source) then
    begin
      FStatus := 'Build completed for an earlier design; current edits retained';
    end
    else
    begin
      FStatus := 'Build complete / ' + TNyxText(LData['scope']);
      FLog := TNyxText(LData['log']);

      if TNyxText(LData['target']) = 'browser' then
      begin
        FCompiledURL := TNyxText(LData['artifact']);
      end
      else
      begin
        Download('native-build-link.txt', TNyxText(LData['artifact']));
        FStatus := 'Native build complete / ' + TNyxText(LData['artifact']);
      end;
    end;
  except
    FStatus := 'Compiler service is unavailable or returned an invalid response';
    FLog := LText;
  end;
  if FCompilerReport <> nil then
  begin
    FAgents.CompilerReport(FCompilerReport.Encode);
  end;
  Refresh;
end;

procedure TNyxStudio.Configuration(ASave: Boolean);
begin

  if FConfigurationRequest <> nil then
  begin
    FStatus := 'Output configuration request is running';
    Refresh;
    Exit;
  end;
  FConfigurationSaving := ASave;
  FConfigurationRequest := TJSXMLHttpRequest.new;

  if ASave then
  begin
    FConfigurationSent := FOutputs.Encode;
    FConfigurationRequest.open('POST', 'api/configuration', True);
    FConfigurationRequest.setRequestHeader('Content-Type', 'application/json');
    FStatus := 'Saving local output configuration';
  end
  else
  begin
    FConfigurationRequest.open('GET', 'api/configuration', True);
  end;
  FConfigurationRequest.timeout := 65000;
  FConfigurationRequest.onreadystatechange := ConfigurationReady;

  if ASave then
  begin
    FConfigurationRequest.send(FConfigurationSent);
    Refresh;
  end
  else
  begin
    FConfigurationRequest.send;
  end;
end;

procedure TNyxStudio.ConfigurationReady;
var
  LText: TNyxText;
  LStatus: Integer;
  LLoaded: TNyxOutputConfiguration;
  LScope: TNyxText;
  LTarget: TNyxText;
begin

  if (FConfigurationRequest = nil) or
    (FConfigurationRequest.readyState <> TJSXMLHttpRequest.DONE) then
  begin
    Exit;
  end;
  LText := FConfigurationRequest.responseText;
  LStatus := FConfigurationRequest.Status;
  FConfigurationRequest.onreadystatechange := nil;
  FConfigurationRequest := nil;
  try

    if LStatus <> 200 then
    begin
      raise ENyxModel.Create('Output service unavailable; design remains usable');
    end;
    LLoaded := TNyxOutputConfiguration.Decode(LText);
    try

      if FConfigurationSaving then
      begin
        { Changes made while a save was in flight remain dirty. Completion must
          never overwrite those newer local edits with the earlier snapshot. }
        FConfigurationDirty := FOutputs.Encode <> FConfigurationSent;
        FStatus := 'Local output configuration saved';
      end
      else if not FConfigurationDirty then
      begin
        FOutputs.Free;
        FOutputs := LLoaded;
        LLoaded := nil;
      end;
    finally
      LLoaded.Free;
    end;
    LScope := FQueuedScope;
    LTarget := FQueuedTarget;
    FQueuedScope := '';
    FQueuedTarget := '';

    if LScope <> '' then
    begin
      Compile(LScope, LTarget);
    end
    else
    begin
      Refresh(True, True);
    end;
  except
    on LException: Exception do
    begin
      FQueuedScope := '';
      FQueuedTarget := '';
      FStatus := LException.Message;
      FLog := LText;
      Refresh(True, True);
    end;
  end;
end;


procedure TNyxStudio.SaveRecovery;
var
  LRecovery: TNyxText;
begin

  if FRecoveryEnabled then
  begin
    LRecovery := NyxObject([
      NyxField('version', NyxData(2)),
      NyxField('project', NyxData(EncodeNyxProject(FSession.ProjectSnapshot))),
      NyxField('name', NyxData(FProjectName)),
      NyxField('boundName', NyxData(FProjectBoundName)),
      NyxField('revision', NyxData(FProjectRevision)),
      NyxField('import', NyxData(FImportPacket)),
      NyxField('importName', NyxData(FImportBoundName)),
      NyxField('importRevision', NyxData(FImportRevision))
    ]).ToJSON;
    { The wrapper quotes a portable packet and may exceed its byte budget even
      when the packet alone fits. Refuse before replacing the last readable
      recovery. Blob measures encoded UTF-8 bytes through the browser API. }

    if TJSBlob.new(TJSArray.new(LRecovery)).size > NyxMaximumJSONBytes then
    begin
      raise ENyxModel.Create('Recovery exceeds the UTF-8 packet budget');
    end;
    window.localStorage.setItem('nyx-studio-project-v2', LRecovery);
  end;
end;

procedure TNyxStudio.ProjectRequest(AOperation: TNyxProjectOperation);
var
  LField: TJSHTMLInputElement;
  LExpected: TNyxText;
begin

  if FProjectRequest <> nil then
  begin
    Exit;
  end;
  FFilesVisible := True;
  FPanel := nspProject;
  LField := TJSHTMLInputElement(document.querySelector('[data-node=project-file-name] input'));

  if LField <> nil then
  begin
    FProjectName := LField.value;
  end;

  if FProjectName = '' then
  begin
    FStatus := 'Choose a saved project name, or download a portable backup';
    Refresh(True, True);
    Exit;
  end;
  ValidateNyxProjectName(FProjectName);
  FProjectOperation := AOperation;
  FProjectRequestName := FProjectName;
  FProjectSent := EncodeNyxProject(FSession.ProjectSnapshot);
  FProjectRequest := TJSXMLHttpRequest.new;
  FProjectRequest.onreadystatechange := ProjectReady;

  if AOperation = npoSave then
  begin
    LExpected := '';

    if FProjectName = FProjectBoundName then
    begin
      LExpected := FProjectRevision;
    end;
    FProjectRequest.open('POST', '/api/project?name=' + encodeURIComponent(FProjectName), True);
    FProjectRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    FProjectRequest.send(NyxObject([
      NyxField('version', NyxData(1)),
      NyxField('expected', NyxData(LExpected)),
      NyxField('project', NyxData(FProjectSent))
    ]).ToJSON);
    FStatus := 'Saving paired project files';
  end
  else
  begin
    FProjectRequest.open('GET', '/api/project?name=' + encodeURIComponent(FProjectName), True);
    FProjectRequest.send;
    FStatus := 'Opening paired project files';
  end;
  Refresh(True, True);
end;

procedure TNyxStudio.ProjectReady;
var
  LStatus: Integer;
  LText: TNyxText;
  LReply: TNyxDataValue;
  LPacket: TNyxText;
  LRevision: TNyxText;
begin

  if (FProjectRequest = nil) or
    (FProjectRequest.readyState <> TJSXMLHttpRequest.DONE) then
  begin
    Exit;
  end;
  LStatus := FProjectRequest.Status;
  LText := FProjectRequest.responseText;
  FProjectRequest.onreadystatechange := nil;
  FProjectRequest := nil;
  try

    if (LStatus <> 200) and (LStatus <> 409) then
    begin

      if LStatus = 404 then
      begin
        raise ENyxModel.Create('No saved project has that name');
      end;
      raise ENyxModel.Create('Project service could not complete the request. Your work is retained');
    end;
    LReply := TNyxDataValue.ParseJSON(LText);
    LPacket := LReply.Field('project').AsText;
    LRevision := LReply.Field('revision').AsText;
    FRemoteName := FProjectRequestName;
    FRemoteRevision := LRevision;

    if LStatus = 409 then
    begin
      FProjectConflict := LPacket;
      FStatus := 'Saved files changed; choose how to keep both versions';
    end
    else if FProjectOperation = npoSave then
    begin
      { Completion acknowledges only the captured save. New edits remain live
        and will compare against this new server revision on the next save. }
      FProjectBoundName := FProjectRequestName;
      FProjectRevision := LRevision;
      FProjectConflict := '';
      FStatus := 'Paired project files saved';

      if EncodeNyxProject(FSession.ProjectSnapshot) <> FProjectSent then
      begin
        FStatus := 'Captured project saved; newer edits are still unsaved';
      end;
    end
    else if EncodeNyxProject(FSession.ProjectSnapshot) <> FProjectSent then
    begin
      FProjectConflict := LPacket;
      FStatus := 'Your project changed while opening. Choose before replacing it';
    end
    else
    begin
      Download('project-before-open.nyxproject', FProjectSent);
      FImportBoundName := FProjectRequestName;
      FImportRevision := LRevision;
      AcceptImport(LPacket, nprRequireMatch);
    end;
  except
    on LException: Exception do
    begin
      FStatus := LException.Message;
      FLog := LText;
    end;
  end;
  Refresh(True, True);
end;

procedure TNyxStudio.AcceptImport(const APacket: TNyxText;
  AResolution: TNyxProjectResolution);
begin
  { Retain the complete imported input even for unsupported source or malformed
    files. Session publication admits both files first; exceptions leave history,
    callbacks, controls and pending local drafts untouched. }
  FImportPacket := APacket;
  FFilesVisible := True;
  FPanel := nspProject;
  FSession.LoadProject(DecodeNyxProject(APacket), AResolution);
  { A new project may reuse the same page IDs. Invalidate view retention so its
    actual canvas/bindings are remounted from the newly admitted document. }
  FCanvasViewID := '';
  FProjectBoundName := FImportBoundName;
  FProjectRevision := FImportRevision;
  FProjectName := FImportBoundName;
  FImportPacket := '';
  FCompiledURL := '';
  FStatus := 'Project opened with its Pascal companion and retained draft';
end;

procedure TNyxStudio.ImportProject;
begin

  if FImportReader <> nil then
  begin
    FStatus := 'Project files are still being read';
    Refresh(True, True);
    Exit;
  end;

  if FImportInput <> nil then
  begin
    FImportInput.remove;
  end;
  FImportInput := TJSHTMLInputElement(document.createElement('input'));
  FImportInput.setAttribute('type', 'file');
  FImportInput.accept := '.nyxproject,.nyx,.pas';
  FImportInput.multiple := True;
  FImportInput.setAttribute('style', 'display:none');
  FImportInput.setAttribute('data-nyx-project-picker', 'true');
  FImportInput.onchange := ImportChosen;
  document.body.appendChild(FImportInput);
  FImportInput.click;
end;

function TNyxStudio.ImportChosen(AEvent: TJSEvent): Boolean;
var
  LCount: Integer;
  LIndex: Integer;
  LName: TNyxText;
  LSecond: TNyxText;
begin
  Result := True;
  LCount := FImportInput.files.length;

  if LCount = 0 then
  begin
    Exit;
  end;
  try
    LName := LowerCase(ExtractFileExt(FImportInput.files[0].name));

    if LCount = 2 then
    begin
      LSecond := LowerCase(ExtractFileExt(FImportInput.files[1].name));

      if not (((LName = '.nyx') and (LSecond = '.pas')) or
        ((LName = '.pas') and (LSecond = '.nyx'))) then
      begin
        raise ENyxModel.Create('Select one .nyx design and its .pas companion together');
      end;
    end
    else if (LCount <> 1) or (LName <> '.nyxproject') then
    begin
      raise ENyxModel.Create('Select a project backup, or both design and Pascal files');
    end;
    FImportDesign := '';
    FImportPascal := '';
    FImportIndex := 0;
    FImportSnapshot := EncodeNyxProject(FSession.ProjectSnapshot);
    FImportBoundName := '';
    FImportRevision := '';
    for LIndex := 0 to LCount - 1 do
    begin

      if FImportInput.files[LIndex].size > 4 * 1024 * 1024 then
      begin
        raise ENyxModel.Create('Selected project file exceeds the import budget');
      end;
    end;
    FImportIndex := 0;
    FImportReader := TJSFileReader.new;
    FImportReader.onload := ImportRead;
    FImportReader.onerror := ImportRead;
    FImportReader.readAsText(FImportInput.files[0], 'utf-8');
  except
    on LException: Exception do
    begin
      FStatus := LException.Message;
      Refresh(True, True);
    end;
  end;
end;

function TNyxStudio.ImportRead(AEvent: TJSEvent): Boolean;
var
  LText: TNyxText;
  LExtension: TNyxText;
begin
  Result := True;
  try

    if FImportReader.error <> nil then
    begin
      raise ENyxModel.Create('Cannot read the selected project files');
    end;
    LText := String(FImportReader.result);
    LExtension := LowerCase(ExtractFileExt(FImportInput.files[FImportIndex].name));

    if LExtension = '.nyxproject' then
    begin
      FImportPacket := LText;
    end
    else
    begin

      if LExtension = '.nyx' then
      begin
        FImportDesign := LText;
      end
      else
      begin
        FImportPascal := LText;
      end;
      Inc(FImportIndex);

      if FImportIndex < FImportInput.files.length then
      begin
        FImportReader.readAsText(FImportInput.files[FImportIndex], 'utf-8');
        Exit;
      end;
      FImportPacket := EncodeNyxProject(NyxProjectPair(FImportDesign, FImportPascal));
    end;
    FImportReader.onload := nil;
    FImportReader.onerror := nil;
    FImportReader := nil;
    FFilesVisible := True;
    FPanel := nspProject;

    if EncodeNyxProject(FSession.ProjectSnapshot) <> FImportSnapshot then
    begin
      raise ENyxProjectConflict.Create('Your project changed while reading files. Choose before opening');
    end;
    Download('project-before-import.nyxproject', FImportSnapshot);
    AcceptImport(FImportPacket, nprRequireMatch);
  except
    on LException: Exception do
    begin
      FStatus := LException.Message;

      if FImportReader <> nil then
      begin
        FImportReader.onload := nil;
        FImportReader.onerror := nil;
        FImportReader := nil;
      end;
    end;
  end;
  Refresh(True, True);
end;


procedure TNyxStudio.Download(const AName, AText: TNyxText);
var
  LAnchor: TJSHTMLAnchorElement;
begin
  LAnchor := TJSHTMLAnchorElement(document.createElement('a'));
  LAnchor.href := 'data:application/octet-stream;charset=utf-8,' + encodeURIComponent(AText);
  LAnchor.download := AName;
  document.body.appendChild(LAnchor);
  LAnchor.click;
  LAnchor.remove;
end;

function TNyxStudio.KeyDown(AEvent: TJSKeyboardEvent): Boolean;
begin
  Result := True;
  { Editing inputs retain their native shortcuts. Global undo is applied only
    outside text controls so typing a caption is not unexpectedly a tree command. }

  if (AEvent.target is TJSHTMLInputElement) or (AEvent.target is TJSHTMLTextAreaElement) then
  begin
    Exit;
  end;

  if (AEvent.ctrlKey or AEvent.metaKey) and (LowerCase(AEvent.key) = 'z') then
  begin
    AEvent.preventDefault;

    if AEvent.shiftKey then
    begin
      if FAgents.Enabled then FAgents.History('redo')
      else FSession.Redo;
    end
    else
    begin
      if FAgents.Enabled then FAgents.History('undo')
      else FSession.Undo;
    end;
    FCompiledURL := '';
    Refresh;
  end;
end;

function TNyxStudio.ViewportResize(AEvent: TJSEvent): Boolean;
var
  LCompact: Boolean;
  LActive: TJSHTMLElement;
  LField: TJSHTMLElement;
  LReplacement: TJSHTMLElement;
  LID: TNyxText;
  LValue: TNyxText;
  LCanvasFocus: Boolean;
begin
  Result := True;
  LCompact := window.innerWidth <= 960;
  { Ordinary resizes let CSS reflow existing controls. Rebuild only when crossing
    the compact boundary, preserving a focused field's uncommitted draft. }

  if LCompact = FCompact then
  begin
    Exit;
  end;
  LID := '';
  LValue := '';
  LCanvasFocus := False;
  LActive := TJSHTMLElement(document.activeElement);

  if (LActive <> nil) and ((LActive is TJSHTMLInputElement) or
    (LActive is TJSHTMLTextAreaElement) or (LActive is TJSHTMLSelectElement)) then
  begin
    LField := TJSHTMLElement(LActive.closest('[data-node]'));

    if LField <> nil then
    begin
      LID := LField.getAttribute('data-node');
      LValue := TJSHTMLInputElement(LActive).value;

      if LID = 'studio-code' then
      begin
        { A viewport crossing can occur before the textarea's change event. }
        FSession.SetSourceDraft(LValue);
      end;
      LCanvasFocus := LActive.closest('[data-node=studio-canvas]') <> nil;

      if LActive.closest('[data-node=studio-left]') <> nil then
      begin
        FPanel := nspProject;
      end
      else if LActive.closest('[data-node=studio-right]') <> nil then
      begin
        FPanel := nspInspector;
      end
      else
      begin
        FPanel := nspDesign;
      end;
    end;
  end;
  FCompact := LCompact;
  Refresh(LCanvasFocus);

  if LCanvasFocus then
  begin
    { The existing field was moved with its canvas and already refocused. Its
      identity belongs to the canvas renderer, not the separate shell renderer.
      Retention also preserves the uncommitted draft and text selection range. }
    Exit;
  end;

  if LID <> '' then
  begin
    LReplacement := FShellRenderer.ElementFor(LID);

    if (LReplacement <> nil) and not (LReplacement is TJSHTMLTextAreaElement) then
    begin
      LReplacement := TJSHTMLElement(LReplacement.querySelector('input,textarea,select'));
    end;

    if LReplacement <> nil then
    begin
      TJSHTMLInputElement(LReplacement).value := LValue;
      NyxFocusWithoutScroll(LReplacement);
    end;
  end;
end;

procedure TNyxStudio.Run(ARecovery: Boolean);
var
  LSaved: TNyxText;
  LSource: TNyxText;
  LRecovery: TNyxDataValue;
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
begin
  FRecoveryEnabled := ARecovery;
  FCompact := window.innerWidth <= 960;
  window.addEventListener('resize', FResizeHandler);

  if ARecovery then
  begin
    try
      LSaved := window.localStorage.getItem('nyx-studio-project-v2');

      if isString(LSaved) and (LSaved <> '') then
      begin
        LRecovery := TNyxDataValue.ParseJSON(LSaved);

        if (LRecovery.Kind <> ndObject) or (LRecovery.Count <> 8) or
          (LRecovery.Field('version').AsInteger <> 2) then
        begin
          raise ENyxModel.Create('Unsupported browser project recovery');
        end;
        FProjectName := LRecovery.Field('name').AsText;
        FProjectBoundName := LRecovery.Field('boundName').AsText;
        FProjectRevision := LRecovery.Field('revision').AsText;
        FImportPacket := LRecovery.Field('import').AsText;
        FImportBoundName := LRecovery.Field('importName').AsText;
        FImportRevision := LRecovery.Field('importRevision').AsText;
        { Keep a divergent packet as an unresolved input, never half-load its
          design. The original recovery wrapper is retained if parsing fails. }
        LSource := LRecovery.Field('project').AsText;
        try
          FSession.LoadProject(DecodeNyxProject(LSource));
        except
          { Two independently conflicting buffers cannot share one editable
            draft. Retain the complete wrapper as the downloadable input rather
            than overwriting its earlier unresolved import. Manual merge is
            explicit; neither accepted file is partially loaded. }

          if FImportPacket <> '' then
          begin
            FImportPacket := LSaved;
          end
          else
          begin
            FImportPacket := LSource;
          end;
          FImportBoundName := FProjectBoundName;
          FImportRevision := FProjectRevision;
          FProjectBoundName := '';
          FProjectRevision := '';
          FStatus := 'Recovered files require a conflict choice; original input retained';
        end;
        FFilesVisible := FImportPacket <> '';

        if FFilesVisible then
        begin
          FPanel := nspProject;
        end;
      end
      else
      begin
        { Migration reads every old key into a detached pair. Even this legacy
          recovery must not publish design before its Pascal is admitted. }
        LSaved := window.localStorage.getItem('nyx-studio-design-v1');

        if isString(LSaved) and (LSaved <> '') then
        begin
          LSource := window.localStorage.getItem('nyx-studio-pascal-v1');

          if not isString(LSource) or (LSource = '') then
          begin
            LDocument := TNyxCodec.Decode(LSaved);
            try
              LSource := TNyxCodegen.Generate(LDocument);
            finally
              LDocument.Free;
            end;
          end;
          LPair := NyxProjectPair(LSaved, LSource);
          LSaved := window.localStorage.getItem('nyx-studio-pascal-draft-v1');

          if isString(LSaved) and (LSaved <> LSource) then
          begin
            LPair.Pending := True;
            LPair.Draft := LSaved;
            LSaved := window.localStorage.getItem('nyx-studio-pascal-draft-base-v1');

            if isString(LSaved) then
            begin
              LPair.DraftBase := LSaved;
            end;
          end;
          FImportPacket := EncodeNyxProject(LPair);
          FSession.LoadProject(LPair);
          FImportPacket := '';
        end;
      end;
    except
      on LException: Exception do
      begin
        FFilesVisible := True;
        FPanel := nspProject;
        FStatus := 'Recovery needs review: ' + LException.Message;
        { A corrupt wrapper cannot be safely rewritten. Keep it under an explicit
          recovery backup before writing fresh coherent snapshots. }
        try
          LSaved := window.localStorage.getItem('nyx-studio-project-v2');

          if isString(LSaved) and (LSaved <> '') then
          begin
            window.localStorage.setItem('nyx-studio-rejected-recovery-v2', LSaved);
          end;
        except
          FRecoveryEnabled := False;
        end;
      end;
    end;
    try
      LSaved := window.localStorage.getItem('nyx-studio-output-target-v1');

      if isString(LSaved) then
      begin
        ValidateNyxOutputTarget(LSaved);
        FOutputTarget := LSaved;
      end;
    except
      FStatus := 'Ready / choose an output whenever you want to build';
    end;
  end;
  { Palette preferences recover independently of portable project admission.
    A denied store or unknown packet leaves the initialized list view usable. }

  if ARecovery then
  begin
    try
      LSaved := window.localStorage.getItem(NyxStudioPalettePreferencesKey);

      if isString(LSaved) and (LSaved <> '') then
      begin
        DecodeNyxStudioPalettePreferences(LSaved, FPalette);
      end;
    except
      FPalette := DefaultNyxStudioPaletteState;
    end;
  end;
  document.onkeydown := KeyDown;
  Refresh;
  Configuration(False);

  if ARecovery then
  begin
    ConnectAgents;
  end;
end;

end.
