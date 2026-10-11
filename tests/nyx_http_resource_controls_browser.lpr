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

program nyx_http_resource_controls_browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.binding.types,
  nyx.resources.editor, nyx.resources.browser, nyx.resources.workspace,
  nyx.studio.browser, nyx.studio.workspaces, nyx.studio.sourcejobs,
  nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.browser,
  nyx.studio.sourcecompilation.service.browser;

type
  { Each input stage retires before the next observation. Compiler completion,
    queued presentation and fresh accepted source are distinct prerequisites. }
  TResourceStage = (rsOpenCode, rsApplyNotebook, rsObserveNotebook, rsSelectHeading,
    rsProject, rsOpenResources, rsShowEditor, rsFillNew, rsPreviewNew, rsBindNew,
    rsConfigureNew,
    rsApplyNew, rsObserveNew, rsCloseNew, rsDesignNew, rsCheckNew, rsUndoNew,
    rsCheckUndoNew, rsRedoNew, rsCheckRedoNew, rsProjectUpdate, rsOpenUpdate,
    rsShowFiles, rsSelectResource, rsOpenResource, rsShowUpdate, rsFillUpdate,
    rsPreviewUpdate, rsApplyUpdate, rsObserveUpdate, rsCloseUpdate, rsDesignUpdate,
    rsCheckUpdate, rsUndoUpdate, rsCheckUndoUpdate, rsRedoUpdate, rsCheckRedoUpdate,
    rsDraft, rsProjectDraft, rsOpenDraft, rsFilesDraft, rsSelectDraft, rsEditDraft,
    rsShowDraft, rsRefuseDraft, rsObserveRefusal, rsCloseDraft, rsDesignDraft,
    rsRestoreDraft, rsObserveRestore, rsUndoRestoreUpdate, rsUndoRestoreNew,
    rsCheckRestored, rsFinish);

  { The matched Web unit omits Event's dictionary overload. This host declaration
    supplies ordinary bubbling input/change without editing dependency source. }
  TBrowserEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

const
  CEditor: TNyxText = 'studio-resource-editor';
  CCatalog: TNyxText = 'studio-resource-browser';
  CWorkspace: TNyxText = 'studio-resource-workspace';
  CName: TNyxText = 'crafted-copy';
  CInitial: TNyxText = 'A resource-backed notebook';
  CUpdated: TNyxText = 'A brighter resource-backed notebook';

var
  GRequest: TJSXMLHttpRequest;
  GContext: TNyxDataValue;
  GWorkspace: TNyxWorkspaceRef;
  GCompiler: INyxSourceCompiler;
  GStudio: TNyxStudio;
  GInput: TJSHTMLTextAreaElement;
  GSource: TNyxText;
  GCreated: TNyxText;
  GUpdated: TNyxText;
  GDraft: TNyxText;
  GInputDirectory: TNyxText;
  GStage: TResourceStage;
  GChecks: Integer;
  GLoad: Integer;
  GBeforeRefusal: TNyxSourceCommandState;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('HTTP resource controls: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Compiler operations own their managed ports; destroying the independent
  editor detaches them before releasing the borrowed control references. }
procedure Failed(const AReason: TNyxText);
begin
  GStudio.Free;
  GStudio := nil;
  GCompiler := nil;
  document.body.setAttribute('data-http-resource-controls', 'failed');
  document.body.setAttribute('data-event-error', AReason);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing resource control: ' + AID);
  end;
end;

procedure Click(const AID: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  LFace := Find(AID);
  Check(LFace.getBoundingClientRect.height > 0, 'ordinary action is visible: ' + AID);
  LFace.click;
end;

{ Compact Studio exposes some header actions through its public menu. Qualify
  that operator route instead of programmatically activating a hidden button. }
procedure WorkspaceAction(const AID, ABranch, ACommand: TNyxText);
begin

  if Find(AID).getBoundingClientRect.height > 0 then
  begin
    Click(AID);
    Exit;
  end;
  Click('action-actions');
  Click('studio-menu-' + ABranch);
  Click('studio-menu-' + ACommand);
end;

procedure ProjectPanel;
var
  LPanel: TJSHTMLElement;
begin
  LPanel := TJSHTMLElement(document.querySelector('[data-node="action-panel-project"]'));

  if (LPanel <> nil) and (LPanel.getBoundingClientRect.height > 0) then
  begin
    Click('action-panel-project');
  end;
end;

procedure DesignPanel;
var
  LPanel: TJSHTMLElement;
begin
  LPanel := TJSHTMLElement(document.querySelector('[data-node="action-panel-design"]'));

  if (LPanel <> nil) and (LPanel.getBoundingClientRect.height > 0) then
  begin
    Click('action-panel-design');
  end;
end;

procedure ResourcePane(APane: TNyxResourceWorkspacePane);
var
  LFace: TJSHTMLElement;
begin
  LFace := Find(NyxResourceWorkspaceActionID(CWorkspace, APane));

  if LFace.getBoundingClientRect.height > 0 then
  begin
    Click(NyxResourceWorkspaceActionID(CWorkspace, APane));
  end;
end;

function ResourceInput(AField: TNyxResourceEditorField): TJSHTMLElement;
begin
  Result := Find(NyxResourceEditorFieldID(CEditor, AField));

  if not ((Result is TJSHTMLInputElement) or (Result is TJSHTMLTextAreaElement) or
    (Result is TJSHTMLSelectElement)) then
  begin
    Result := TJSHTMLElement(Result.querySelector('input,textarea,select'));
  end;
  Check((Result <> nil) and (Result.getBoundingClientRect.height > 0),
    'ordinary resource field has a visible face: ' + NyxResourceEditorFieldID(CEditor, AField));
end;

procedure Changed(AInput: TJSHTMLElement);
var
  LOptions: TJSObject;
begin
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  AInput.dispatchEvent(TBrowserEvent.new('input', LOptions));
  AInput.dispatchEvent(TBrowserEvent.new('change', LOptions));
end;

procedure ResourceField(AField: TNyxResourceEditorField; const AValue: TNyxText);
var
  LInput: TJSHTMLElement;
begin
  LInput := ResourceInput(AField);
  TJSHTMLInputElement(LInput).value := AValue;
  Check(TJSHTMLInputElement(LInput).value = AValue, 'resource proposal retains its exact input');
  Changed(LInput);
end;

procedure SelectResource;
var
  LItems: TJSNodeList;
  LItem: TJSHTMLElement;
  LIndex: Integer;
  LMatches: Integer;
begin
  LItems := Find(NyxResourceBrowserListID(CCatalog)).querySelectorAll('[role="option"]');
  LMatches := 0;
  for LIndex := 0 to LItems.length - 1 do
  begin
    LItem := TJSHTMLElement(LItems.item(LIndex));

    if LItem.textContent = 'Notebook copy (crafted-copy)' then
    begin
      Inc(LMatches);
      Check(LItem.getBoundingClientRect.height > 0, 'accepted exact catalog variant is visible');
      LItem.click;
    end;
  end;
  Check(LMatches = 1, 'catalog has exactly one resource variant with the authored identity');
end;

procedure BoundCaption(const ACaption, ASource: TNyxText);
begin
  Check(Find('heading-1').textContent = ACaption, 'actual mounted heading realizes resource data');
  Check(GInput.value = ASource, 'source editor retains the exact complete accepted Pascal');
  Check(Find('studio-code') = GInput, 'the same physical source editor remains mounted');
  Check(Pos('class function TNotebookCards.Definition', GInput.value) > 0,
    'unrelated handwritten component helper remains intact');
end;

procedure Pump;
var
  LBind: TJSHTMLInputElement;
begin
  try

    if GStudio.SourceBusy or GStudio.PresentationPending then
    begin
      window.setTimeout(@Pump, 20);
      Exit;
    end;
    document.body.setAttribute('data-http-resource-stage', IntToStr(Ord(GStage)));
    case GStage of
      rsOpenCode:
        WorkspaceAction('action-code', 'view', 'code');
      rsApplyNotebook:
        begin
          GInput := TJSHTMLTextAreaElement(Find('studio-code'));
          Check(GInput.getBoundingClientRect.height > 0, 'initial complete-source editor is visible');
          GInput.value := GSource;
          Changed(GInput);
          Click('action-apply-source');
          Check(GStudio.SourceBusy, 'ordinary Apply dispatches actual HTTP constructor compilation');
        end;
      rsObserveNotebook:
        begin
          Check(GStudio.SourceCommands.State = nssApplied,
            'HTTP notebook compilation publishes locally: ' + GStudio.SourceCommands.Message);
          BoundCaption('Notes for page 1', GSource);
        end;
      rsSelectHeading:
        Click('heading-1');
      rsProject, rsProjectUpdate, rsProjectDraft:
        ProjectPanel;
      rsOpenResources, rsOpenUpdate, rsOpenDraft:
        Click('action-resources-toggle');
      rsShowEditor, rsShowUpdate, rsShowDraft:
        ResourcePane(rwpEditor);
      rsFillNew:
        begin
          Click(NyxResourceEditorActionID(CEditor, reaNew));
          ResourceField(refName, CName);
          ResourceField(refKind, 'JSON');
          ResourceField(refTitle, 'Notebook copy');
          ResourceField(refDescription, 'A caption supplied by packed project data.');
          ResourceField(refContent, '{"title":"' + CInitial + '"}');
        end;
      rsPreviewNew, rsPreviewUpdate:
        Click(NyxResourceEditorActionID(CEditor, reaPreview));
      rsBindNew:
        begin
          LBind := TJSHTMLInputElement(ResourceInput(refBind));
          LBind.checked := True;
          Changed(LBind);
        end;
      rsConfigureNew:
        begin
          ResourceField(refTarget, NyxBindingPropertyTitle(bpText));
          ResourceField(refPath, 'Root["title"] / text');
          Check(GInput.value = GSource, 'proposal and preview do not change the accepted source');
        end;
      rsApplyNew, rsApplyUpdate:
        begin
          Click(NyxResourceEditorActionID(CEditor, reaApply));
          Check(GStudio.SourceBusy, 'resource group dispatches fresh real HTTP compilation');
        end;
      rsObserveNew:
        begin
          Check(GStudio.SourceCommands.State = nssApplied,
            'resource create/bind publishes locally: ' + GStudio.SourceCommands.Message);
        end;
      rsCloseNew, rsCloseUpdate, rsCloseDraft:
        Click('action-resources-close');
      rsDesignNew, rsDesignUpdate, rsDesignDraft:
        DesignPanel;
      rsCheckNew:
        begin
          { Compact Resources parks the retained source view. Its current value
            is painted when Design remounts it; a detached textarea is neither
            an accepted-pair query nor evidence of failed model publication. }
          GCreated := GInput.value;
          Check((GCreated <> GSource) and (Pos(CName, GCreated) > 0),
            'new resource publication changes the exact complete source');
          BoundCaption(CInitial, GCreated);
        end;
      rsCheckRedoNew, rsCheckUndoUpdate:
        BoundCaption(CInitial, GCreated);
      rsUndoNew, rsUndoUpdate, rsUndoRestoreUpdate, rsUndoRestoreNew:
        WorkspaceAction('action-undo', 'edit', 'undo');
      rsCheckUndoNew, rsCheckRestored:
        BoundCaption('Notes for page 1', GSource);
      rsRedoNew, rsRedoUpdate:
        WorkspaceAction('action-redo', 'edit', 'redo');
      rsShowFiles, rsFilesDraft:
        ResourcePane(rwpFiles);
      rsSelectResource, rsSelectDraft:
        SelectResource;
      rsOpenResource, rsEditDraft:
        Click(NyxResourceBrowserActionID(CCatalog, rbaOpen));
      rsFillUpdate:
        begin
          Check(TJSHTMLInputElement(ResourceInput(refName)).value = CName,
            'catalog Open returns the exact accepted variant rather than a New proposal');
          ResourceField(refContent, '{"title":"' + CUpdated + '"}');
        end;
      rsObserveUpdate:
        begin
          Check(GStudio.SourceCommands.State = nssApplied,
            'resource content update publishes locally: ' + GStudio.SourceCommands.Message);
        end;
      rsCheckUpdate:
        begin
          GUpdated := GInput.value;
          Check((GUpdated <> GCreated) and (Pos(CUpdated, GUpdated) > 0),
            'fresh source equality establishes the update independently of older Applied status');
          BoundCaption(CUpdated, GUpdated);
        end;
      rsCheckRedoUpdate:
        BoundCaption(CUpdated, GUpdated);
      rsDraft:
        begin
          Check(GInput.getBoundingClientRect.height > 0, 'unfinished source is entered in the visible editor');
          GDraft := GUpdated + #10 + '{ A still unfinished resource idea }';
          GInput.value := GDraft;
          Changed(GInput);
        end;
      rsRefuseDraft:
        begin
          GBeforeRefusal := GStudio.SourceCommands.State;
          Click(NyxResourceEditorActionID(CEditor, reaApply));
        end;
      rsObserveRefusal:
        begin
          Check((GStudio.SourceCommands.State = GBeforeRefusal) and not GStudio.SourceBusy,
            'ordinary draft guard refuses before dispatching another compiler command');
          Check(Pos('Resolve the pending Pascal draft', Find('studio-status').textContent) > 0,
            'draft refusal gives useful resolution guidance');
          Check(GInput.value = GDraft, 'rejected resource input retains the exact unfinished buffer');
          Check(Find('studio-code') = GInput, 'refusal retains the same source control');
        end;
      rsRestoreDraft:
        Click('action-reset-source');
      rsObserveRestore:
        BoundCaption(CUpdated, GUpdated);
      rsFinish:
        begin
          document.body.setAttribute('data-http-resource-checks', IntToStr(GChecks));
          document.body.setAttribute('data-http-resource-controls', 'passed');
          Exit;
        end;
    end;
    GStage := Succ(GStage);
    window.setTimeout(@Pump, 20);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function Connected(AEvent: TJSProgressEvent): Boolean;
var
  LReply: TNyxDataValue;
  LState: TNyxDataValue;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'explicit existing editor context connects over actual HTTP');
    LReply := TNyxDataValue.ParseJSON(GRequest.responseText);
    LState := LReply.Field('state');
    Check(NyxWorkspaceArgument(LState).ID = GWorkspace.ID, 'compiler grant belongs to the exact workshop');
    Check((LState.Field('session').Field('revision').AsInteger = GContext.Field('revision').AsInteger) and
      (LState.Field('project').AsText = GContext.Field('project').AsText),
      'connecting retains the complete existing server pair and revision');
    Check(LState.Field('sourceCompilation').AsBoolean, 'connected server exposes real source compilation');
    GCompiler := NewNyxBrowserSourceCompiler(NewNyxBrowserSourceService(
      LReply.Field('token').AsText, GWorkspace, GContext.Field('revision').AsInteger));
    { Local publication is intentional. The compiler grant addresses an existing
      workshop, but no bridge, shared publisher or recovery store is installed.
      All authored edits/history belong to this independent browser editor. }
    GStudio := TNyxStudio.Create(GCompiler);
    GStudio.Run(False);
    window.setTimeout(@Pump, 20);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure LoadNext; forward;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'owned fixture input arrives over actual HTTP');

    if GLoad = 0 then
    begin
      GContext := TNyxDataValue.ParseJSON(GRequest.responseText);
      Check((GContext.Field('workspace').AsText <> '') and
        (GContext.Field('label').AsText = 'Manual presentations') and
        (GContext.Field('revision').AsInteger > 0),
        'fixture requires an explicit preflighted owned workshop, never the primary project');
      GWorkspace := NyxWorkspace(GContext.Field('workspace').AsText);
    end
    else
    begin
      GSource := GRequest.responseText;
      Check(Pos('class function TNotebookCards.Definition', GSource) > 0,
        'input is the maintained complete handwritten fixture');
    end;
    Inc(GLoad);
    LoadNext;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function LoadError(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  Failed('Owned HTTP resource input or connection did not arrive');
end;

procedure LoadNext;
const
  CFiles: array[0..1] of TNyxText = ('context.json', 'source.pas');
var
  LClaim: TNyxDataValue;
begin
  GRequest := TJSXMLHttpRequest.new;
  GRequest.timeout := 10000;
  GRequest.onerror := @LoadError;
  GRequest.ontimeout := @LoadError;

  if GLoad = Length(CFiles) then
  begin
    { The wire calls this an editor claim. Supplying the exact already-recovered
      workshop pair cannot seed the primary or introduce fixture content. The
      complete reply is checked before any compile-only request is made. }
    LClaim := NyxWithWorkspace(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', GContext.Field('project')),
      NyxField('selection', GContext.Field('selection')),
      NyxField('view', GContext.Field('view'))]), GWorkspace);
    GRequest.open('POST', '/api/agents/connect', True);
    GRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    GRequest.onload := @Connected;
    GRequest.send(LClaim.ToJSON);
  end
  else
  begin
    GRequest.open('GET', GInputDirectory + CFiles[GLoad], True);
    GRequest.onload := @Loaded;
    GRequest.send;
  end;
end;

var
  LBase: TJSHTMLElement;
begin
  document.body.setAttribute('data-http-resource-controls', 'pending');
  GInputDirectory := Copy(window.location.pathname, 1, LastDelimiter('/', window.location.pathname));
  { This confined fixture is embedded below /builds. The ordinary compiler
    service resolves its API against the same-origin Studio root. Retain the
    fixture input directory separately; no credentials enter the hosted files. }
  LBase := TJSHTMLElement(document.createElement('base'));
  LBase.setAttribute('href', '/');
  document.head.appendChild(LBase);
  LoadNext;
end.
