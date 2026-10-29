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

program nyx_source_workspace_browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.controls,
  nyx.resources.editor, nyx.resources.workspace, nyx.studio.browser,
  nyx.resources, nyx.studio.session, nyx.studio.sourcejobs, nyx.studio.projects,
  nyx.studio.resourceedits, nyx.studio.transactions;

type
  { Presentation changes publish after the originating callback retires. Each
    stage observes the real host on a later turn instead of forcing a renderer
    refresh or treating a queued command as an already mounted control. The
    deep-caret return also checks actual scrolling after modal height shrinks. }
  TWorkspaceStage = (wsOpenCode, wsEditSource, wsObserveOutputs,
    wsObserveToolsOpen, wsObserveToolsClosed, wsProjectForResources,
    wsOpenResources, wsShowResourceEditor, wsFillResource, wsPreviewResource, wsApplyResource,
    wsObserveResourceRefusal, wsLeaveResources, wsReturnToDesign,
    wsObserveResourcesClosed, wsOpenMessages, wsObserveMessages,
    wsObserveSource, wsObserveModal, wsObserveModalMessages,
    wsObserveModalSource, wsObserveDeepClosed, wsObserveClosed, wsCancelModal,
    wsObserveCancelled);

  { The installed Web declaration exposes only Event(type). Keep the standard
    cancelable constructor dictionary at this host boundary without editing RTL. }
  TBrowserEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

  { The fixture owns this observer longer than its command queue. It borrows
    that queue only during notifications and captures the typed restore effect. }
  TQueueDraftObserver = class
  public
    Commands: TNyxSourceCommands;
    Restored: Boolean;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
  end;

var
  GStudio: TNyxStudio;
  GInput: TJSHTMLTextAreaElement;
  GDraft: TNyxText;
  GChecks: Integer;
  GFrame: TJSHTMLIFrameElement;
  GPolls: Integer;
  GSplit: TJSHTMLElement;
  GSplitHeight: TNyxText;
  GSplitFlex: TNyxText;
  GStage: TWorkspaceStage;
  GHeight: Double;
  GDialog: TJSHTMLElement;
  GStableCanvas: TJSHTMLElement;
  GStableDetails: TJSHTMLElement;
  GStarted: Double;
  GCaptureRequested: Boolean;
  GBeforeRefusal: TNyxSourceCommandState;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Source workspace: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TQueueDraftObserver.Changed(AState: TNyxSourceCommandState;
  const AMessage: TNyxText);
begin

  if (Commands <> nil) and Commands.RestoredDraft then
  begin
    Check(AState = nssIdle, 'draft restoration remains separate from compiler application');
    Restored := True;
  end;
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing source workspace control: ' + AID);
  end;
end;

{ Narrow header actions use the ordinary managed menu. Hidden buttons refuse
  activation, so source/Outputs qualification must take the same route available
  to an operator. No application design is authored through this harness. }
procedure WorkspaceAction(const AID, ABranch, ACommand: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  LFace := Find(AID);

  if LFace.getBoundingClientRect.height > 0 then
  begin
    LFace.click;
    Exit;
  end;
  Find('action-actions').click;
  Find('studio-menu-' + ABranch).click;
  Find('studio-menu-' + ACommand).click;
end;

procedure RetainedInput;
begin
  Check(Find('studio-code') = GInput, 'same real textarea survives presentation changes');
  Check(GInput.value = GDraft, 'pending English draft stays exact');
  Check((GInput.selectionStart = 13) and (GInput.selectionEnd = 61),
    'source range remains exact');
end;

{ These inputs exercise the real proposal form and callbacks. The fixture's
  unfinished source is already owned locally; applying a grouped resource must
  refuse before compiler dispatch without replacing accepted work or its draft.
  The wrapper lookup belongs only to this DOM adapter qualification boundary. }
procedure ResourceField(AField: TNyxResourceEditorField; const AValue: TNyxText);
var
  LInput: TJSHTMLElement;
  LOptions: TJSObject;
begin
  LInput := Find(NyxResourceEditorFieldID('studio-resource-editor', AField));

  if not ((LInput is TJSHTMLInputElement) or (LInput is TJSHTMLTextAreaElement) or
    (LInput is TJSHTMLSelectElement)) then
  begin
    LInput := TJSHTMLElement(LInput.querySelector('input,textarea,select'));
  end;
  Check(LInput <> nil, 'actual resource proposal input exists');
  Check(LInput.getBoundingClientRect.height > 0, 'resource proposal input has a visible face');
  TJSHTMLInputElement(LInput).value := AValue;
  Check(TJSHTMLInputElement(LInput).value = AValue, 'resource input retains the proposed value');
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LInput.dispatchEvent(TBrowserEvent.new('input', LOptions));
  LInput.dispatchEvent(TBrowserEvent.new('change', LOptions));
end;

{ Queue admission is also public to semantic consumers. Exercise its changed
  browser path with an independent owner, not the controller's private session.
  This deliberate refusal occurs before creating a worker or dispatching a
  compiler. The ordinary UI has its own earlier capture guard, checked below. }
procedure QueuedDraftGuard;
var
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LEdit: TNyxStudioDesignEdit;
  LPair: TNyxProjectPair;
  LAccepted: TNyxProjectPair;
  LObserver: TQueueDraftObserver;
  LRestore: INyxButton;
begin
  LSession := TNyxStudioSession.Create;
  LCommands := nil;
  LObserver := TQueueDraftObserver.Create;
  try
    LAccepted := LSession.ProjectSnapshot;
    LSession.SetSourceDraft(LSession.Source + #10 + '{ An unfinished resource idea. }');
    LPair := LSession.ProjectSnapshot;
    LCommands := TNyxSourceCommands.Create(LSession, LObserver.Changed);
    LObserver.Commands := LCommands;
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTransaction;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Transaction := CaptureNyxProjectTransaction(NyxProjectTransaction([
      NyxResourceStep(NyxResourcePatch([NyxDefineResource(NyxResourceRef('draft-guard'),
        NyxDefaultLocale, NyxTextResource('Refused while drafting'))]))]));
    LCommands.Edit(LEdit);
    Check((LCommands.State = nssRejected) and
      (Pos('Resolve the pending Pascal draft', LCommands.Message) > 0) and
      (Pos('unavailable', LCommands.Message) = 0),
      'semantic queue classifies draft resolution as deliberate rejection');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'semantic browser refusal retains the complete accepted pair/draft/base');
    LRestore := NewNyxButton('action-reset-source');
    Check(LCommands.Route(LRestore.Node, ntClick), 'ordinary source route accepts explicit draft restoration');
    Check(LObserver.Restored and not LCommands.RestoredDraft,
      'owning host receives one scoped restore effect which expires after notification');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAccepted),
      'explicit browser restore returns the exact accepted pair without a compiler');
  finally
    LObserver.Commands := nil;
    LRestore := nil;
    LCommands.Free;
    LObserver.Free;
    LSession.Free;
  end;
end;

{ Keep the same machine-readable result and expose its bounded verdict to an
  ordinary browser observer. No source buffer or application project is dumped. }
procedure ShowVerdict(const AMessage: TNyxText);
var
  LResult: TJSHTMLElement;
begin
  LResult := TJSHTMLElement(document.getElementById('source-workspace-verdict'));

  if LResult = nil then
  begin
    LResult := TJSHTMLElement(document.createElement('pre'));
    LResult.id := 'source-workspace-verdict';
    LResult.setAttribute('role', 'status');
    document.body.appendChild(LResult);
  end;
  LResult.textContent := AMessage;
end;

procedure Failed(const AMessage: TNyxText);
begin
  document.body.setAttribute('data-source-editor', 'failed');
  document.body.setAttribute('data-source-editor-error', AMessage);
  ShowVerdict('FAIL source workspace stage ' + IntToStr(Ord(GStage)) + ': ' + AMessage);
  {$ifdef NYX_SOURCE_PROFILE}
  ShowVerdict('FAIL source workspace stage ' + IntToStr(Ord(GStage)) + ': ' + AMessage +
    #10 + 'Document visibility: ' + String(TJSObject(document)['visibilityState']) +
    #10 + document.body.getAttribute('data-nyx-source-host-trace'));
  {$endif}
end;

procedure FinishExpanded;
begin
  try
    RetainedInput;
    GDialog := TJSHTMLElement(document.querySelector('dialog[open]'));
    Check((GDialog <> nil) and GDialog.contains(GInput),
      'final expanded capture observes the retained editor in its actual modal');
    document.body.setAttribute('data-source-editor', 'passed');
    document.body.setAttribute('data-source-editor-checks', IntToStr(GChecks));
    ShowVerdict('PASS ' + IntToStr(GChecks) + ' source workspace checks');
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure FinishAllocation;
begin
  try
    RetainedInput;
    Check(GInput.getBoundingClientRect.height >= 100,
      'restoring the host restores a readable source viewport');
    Find('action-expand-source').click;
    window.setTimeout(@FinishExpanded, 200);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure AfterSmallAllocation;
var
  LFirst: TJSHTMLElement;
  LSecond: TJSHTMLElement;
  LGrip: TJSHTMLElement;
begin
  try
    LFirst := TJSHTMLElement(GSplit.children[0]);
    LSecond := TJSHTMLElement(GSplit.children[2]);
    LGrip := TJSHTMLElement(GSplit.children[1]);
    Check(LFirst.getBoundingClientRect.height + LSecond.getBoundingClientRect.height +
      LGrip.getBoundingClientRect.height <= GSplit.clientHeight + 1,
      'automatic small-host observation bounds both panes and the divider');
    RetainedInput;
    GSplit.style.setProperty('height', GSplitHeight);
    GSplit.style.setProperty('flex', GSplitFlex);
    window.setTimeout(@FinishAllocation, 200);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure AfterAllocation;
var
  LSecond: TJSHTMLElement;
begin
  try
    LSecond := TJSHTMLElement(GSplit.children[2]);
    Check((LSecond.getBoundingClientRect.height >= 239) and
      (GInput.getBoundingClientRect.height >= 100) and
      (Find('studio-canvas-wrap').getBoundingClientRect.height >= 159),
      'automatic host resize retains readable source and useful canvas faces');
    RetainedInput;
    GSplit.style.setProperty('height', '204px');
    window.setTimeout(@AfterSmallAllocation, 200);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure Journey;
var
  LIndex: Integer;
  LCancel: TJSEvent;
  LOptions: TJSObject;
  LPanel: TJSHTMLElement;
  LCommands: TJSHTMLElement;
  LExpand: TJSHTMLElement;
begin
  try

    if window.performance.now - GStarted > 30000 then
    begin
      raise Exception.Create('Source workspace real-clock journey exceeded 30 seconds');
    end;

    if GCaptureRequested and (GStage = wsOpenMessages) and
      (document.body.getAttribute('data-capture-observed') <> 'source-balanced-details') then
    begin
      window.setTimeout(@Journey, 50);
      Exit;
    end;

    {$ifdef NYX_SOURCE_PROFILE}
    SourceJobProfile('workspace-stage-' + IntToStr(Ord(GStage)) + '-start');
    {$endif}
    case GStage of
      wsOpenCode:
        begin
          WorkspaceAction('action-code', 'view', 'code');
        end;
      wsEditSource:
        begin
          GInput := TJSHTMLTextAreaElement(Find('studio-code'));
          GHeight := GInput.getBoundingClientRect.height;
          Check(GHeight > 60, 'split source has a usable physical face');
          LCommands := Find('studio-code-actions');
          LCommands.scrollLeft := 0;
          LExpand := Find('action-expand-source');
          Check((LCommands.getBoundingClientRect.height <= 65) and
            (LExpand.getBoundingClientRect.top = Find('action-apply-source').getBoundingClientRect.top) and
            (LExpand.getBoundingClientRect.right <= LCommands.getBoundingClientRect.right),
            'source commands keep one bounded lane with Apply and Expand immediately reachable');
          GDraft := GInput.value + #10 + '{ Pending English source workspace draft. }';
          for LIndex := 1 to 80 do
          begin
            GDraft := GDraft + #10 + '{ Notes for a roomy Pascal workspace. }';
          end;
          GInput.value := GDraft;
          GInput.dispatchEvent(TJSEvent.new('change'));
          GInput.focus;
          GInput.selectionStart := 13;
          GInput.selectionEnd := 61;
          WorkspaceAction('action-outputs', 'project', 'outputs');
        end;
      wsObserveOutputs:
        begin
          RetainedInput;
          Check(GInput.getBoundingClientRect.height >= 100,
            'source stays readable with Outputs in the shared center');
          Check(Find('studio-canvas-wrap').getBoundingClientRect.height >= 159,
            'nested detail allocation retains a useful design face beside source');
          Find('action-canvas-tools').click;
        end;
      wsObserveToolsOpen:
        begin
          Check(Find('studio-drop-position').getBoundingClientRect.height > 0,
            'advanced placement controls remain reachable on either host');
          RetainedInput;
          Find('action-canvas-tools').click;
        end;
      wsObserveToolsClosed:
        begin
          Check(Find('studio-drop-position').getBoundingClientRect.height = 0,
            'closing tools returns optional controls to their retained hidden state');
          RetainedInput;
        end;
      wsProjectForResources:
        begin
          LPanel := TJSHTMLElement(document.querySelector('[data-node="action-panel-project"]'));

          if (LPanel <> nil) and (LPanel.getBoundingClientRect.height > 0) then
          begin
            LPanel.click;
          end;
        end;
      wsOpenResources:
        begin
          Find('action-resources-toggle').click;
        end;
      wsShowResourceEditor:
        begin
          LPanel := Find(NyxResourceWorkspaceActionID('studio-resource-workspace', rwpEditor));

          if LPanel.getBoundingClientRect.height > 0 then
          begin
            LPanel.click;
          end;
        end;
      wsFillResource:
        begin
          ResourceField(refName, 'draft-guard');
          ResourceField(refKind, 'JSON');
          ResourceField(refContent, '{"title":"Draft guard"}');
        end;
      wsPreviewResource:
        begin
          Find(NyxResourceEditorActionID('studio-resource-editor', reaPreview)).click;
        end;
      wsApplyResource:
        begin
          GBeforeRefusal := GStudio.SourceCommands.State;
          Find(NyxResourceEditorActionID('studio-resource-editor', reaApply)).click;
        end;
      wsObserveResourceRefusal:
        begin
          Check((GStudio.SourceCommands.State = GBeforeRefusal) and
            (Pos('Resolve the pending Pascal draft', Find('studio-status').textContent) > 0) and
            (Pos('unavailable', Find('studio-status').textContent) = 0),
            'actual resource draft guard refuses before queue admission with useful guidance');
          Check(not GStudio.SourceBusy, 'draft refusal dispatches no compiler job');
          RetainedInput;
          QueuedDraftGuard;
        end;
      wsLeaveResources:
        begin
          Find('action-resources-close').click;
        end;
      wsReturnToDesign:
        begin
          LPanel := TJSHTMLElement(document.querySelector('[data-node="action-panel-design"]'));

          if (LPanel <> nil) and (LPanel.getBoundingClientRect.height > 0) then
          begin
            LPanel.click;
          end;
        end;
      wsObserveResourcesClosed:
        begin
          RetainedInput;
          Check(Find('studio-canvas-wrap').getBoundingClientRect.height >= 159,
            'leaving Resources restores a useful design viewport');
          document.body.setAttribute('data-capture-checkpoint', 'source-balanced-details');
        end;
      wsOpenMessages:
        begin
          GStableCanvas := Find('studio-canvas');
          GStableDetails := Find('studio-details-split');
          Find('action-messages-tab').click;
        end;
      wsObserveMessages:
        begin
          Check((Find('studio-canvas') = GStableCanvas) and
            (Find('studio-details-split') = GStableDetails),
            'source-only tabs retain the actual design face and details splitter');
          Check(Find('studio-source-messages').getBoundingClientRect.height > 0,
            'compiler messages have a separate visible tab');
          Check(GInput.getBoundingClientRect.height = 0,
            'messages do not crowd a visible source face');
          Find('action-source-tab').click;
        end;
      wsObserveSource:
        begin
          Check((Find('studio-canvas') = GStableCanvas) and
            (Find('studio-details-split') = GStableDetails),
            'returning Source keeps surrounding mounted editor controls');
          RetainedInput;
          Check(GInput.getBoundingClientRect.height > 60,
            'Source tab restores usable editor space');
          Find('action-expand-source').click;
        end;
      wsObserveModal:
        begin
          GDialog := TJSHTMLElement(document.querySelector('dialog[open]'));
          Check(GDialog <> nil, 'Expand opens the real browser modal');
          Check(GDialog.contains(GInput), 'modal contains the retained editor');
          Check(GInput.getBoundingClientRect.height > GHeight + 100,
            'expanded editor gains substantial physical height');
          Check(Find('action-expand-source').textContent = 'Close',
            'modal exposes an explicit Close action');
          RetainedInput;
          Find('action-messages-tab').click;
        end;
      wsObserveModalMessages:
        begin
          Check(document.querySelector('dialog[open]') = GDialog,
            'tab refresh retains the same open modal host');
          Check(GDialog.contains(Find('studio-source-messages')),
            'compiler messages stay inside the expanded workspace');
          Find('action-source-tab').click;
        end;
      wsObserveModalSource:
        begin
          Check(document.querySelector('dialog[open]') = GDialog,
            'Source tab restores the same modal top-layer host');
          RetainedInput;
          GInput.focus;
          GInput.selectionStart := Length(GInput.value);
          GInput.selectionEnd := Length(GInput.value);
          GInput.scrollTop := GInput.scrollHeight;
          Check(GInput.scrollTop > 100, 'the expanded editor has a genuinely deep visible caret');
          Find('action-expand-source').click;
        end;
      wsObserveDeepClosed:
        begin
          Check(document.querySelector('dialog[open]') = nil,
            'deep-caret Close returns to the split editor');
          Check((Find('studio-code') = GInput) and (GInput.value = GDraft) and
            (GInput.selectionStart = Length(GDraft)) and
            (GInput.selectionEnd = Length(GDraft)) and (document.activeElement = GInput),
            'shrinking the expanded editor retains the exact focused draft and deep caret');
          Check(GInput.scrollHeight - GInput.clientHeight - GInput.scrollTop <= 1,
            'shrinking the expanded editor keeps its active end caret visibly inside the smaller viewport');
          GInput.selectionStart := 13;
          GInput.selectionEnd := 61;
          GInput.scrollTop := 0;
        end;
      wsObserveClosed:
        begin
          Check(document.querySelector('dialog[open]') = nil,
            'Close returns to the ordinary split view');
          RetainedInput;
          Find('action-expand-source').click;
        end;
      wsCancelModal:
        begin
          GDialog := TJSHTMLElement(document.querySelector('dialog[open]'));
          Check(GDialog <> nil, 'second expansion publishes before cancellation');
          LOptions := TJSObject.new;
          LOptions['cancelable'] := True;
          LCancel := TBrowserEvent.new('cancel', LOptions);
          GDialog.dispatchEvent(LCancel);
          Check(LCancel.defaultPrevented,
            'browser cancellation goes through the owned return path');
        end;
      wsObserveCancelled:
        begin
          Check(document.querySelector('dialog[open]') = nil, 'cancellation closes the modal');
          RetainedInput;
          Check(document.activeElement = GInput,
            'cancellation returns focus to the same editor');
          { Resize the real split without a renderer Sync or Studio repaint.
            Synthesized callbacks qualify this DOM path, not trusted hardware,
            mobile keyboards or assistive technology. This owned fixture opts
            out of recovery/agent connection and never authors the user design. }
          GSplit := Find('studio-split');
          GSplitHeight := GSplit.style.getPropertyValue('height');
          GSplitFlex := GSplit.style.getPropertyValue('flex');
          GSplit.style.setProperty('flex', 'none');
          GSplit.style.setProperty('height', '444px');
          window.setTimeout(@AfterAllocation, 200);
          Exit;
        end;
    end;
    {$ifdef NYX_SOURCE_PROFILE}
    SourceJobProfile('workspace-stage-' + IntToStr(Ord(GStage)) + '-end');
    {$endif}
    GStage := Succ(GStage);
    window.setTimeout(@Journey, 200);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure Observe;
var
  LBody: TJSHTMLElement;
  LResult: TNyxText;
  LCheckpoint: TNyxText;
begin
  Inc(GPolls);

  if GFrame.contentDocument <> nil then
  begin
    LBody := TJSHTMLElement(GFrame.contentDocument.body);
    LResult := LBody.getAttribute('data-source-editor');
    { The retained narrow-host wrapper relays only capture readiness. It never
      sends editor commands or advances the child's presentation clocks. }
    LCheckpoint := '';

    if LBody.hasAttribute('data-capture-checkpoint') then
    begin
      LCheckpoint := LBody.getAttribute('data-capture-checkpoint');
    end;

    if LCheckpoint <> '' then
    begin
      document.body.setAttribute('data-capture-checkpoint', LCheckpoint);

      if document.body.getAttribute('data-capture-observed') = LCheckpoint then
      begin
        LBody.setAttribute('data-capture-observed', LCheckpoint);
      end;
    end;

    if LResult = 'passed' then
    begin
      document.body.setAttribute('data-source-editor', 'passed');
      document.body.setAttribute('data-source-editor-checks',
        LBody.getAttribute('data-source-editor-checks'));
      Exit;
    end;

    if LResult = 'failed' then
    begin
      Failed(LBody.getAttribute('data-source-editor-error'));
      Exit;
    end;
  end;

  if GPolls >= 100 then
  begin
    Failed('Narrow source workspace did not finish');
    Exit;
  end;
  window.setTimeout(@Observe, 100);
end;

begin
  { Explicit capture mode lets the Pascal driver acknowledge a stable viewport.
    The ordinary review page also finishes without an attached capture driver. }
  GCaptureRequested := Pos('capture=1', window.location.search) > 0;

  if Pos('host=1', window.location.search) > 0 then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.cssText := 'width:390px;height:900px;border:0;display:block;';
    TJSHTMLElement(document.body).style.setProperty('margin', '0');
    document.body.appendChild(GFrame);
    GFrame.src := 'source-editor.html';

    if GCaptureRequested then
    begin
      GFrame.src := 'source-editor.html?capture=1';
    end;
    window.setTimeout(@Observe, 100);
  end
  else
  begin
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    GStarted := window.performance.now;
    window.setTimeout(@Journey, 350);
  end;
end.
