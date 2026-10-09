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


unit nyx.test.resource.workbench.browser;

{$mode delphi}{$H+}{$codepage utf8}{$modeswitch externalclass}

interface

{ The normal browser Studio receives exact MCP paired files and synthetic
  FileReader/input callbacks. Physical chooser, trusted input and accessibility
  remain separate qualifications. No backend, recovery or sync workspace is used. }
procedure RunNyxResourceWorkbenchStudioQualification;

implementation

uses SysUtils, JS, Web, nyx.text, nyx.bytes, nyx.data, nyx.model,
  nyx.codec, nyx.codegen, nyx.studio.projects, nyx.studio.browser,
  nyx.resources, nyx.resources.editor, nyx.resources.rows.editor,
  nyx.collections, nyx.binding.types, nyx.behavior, nyx.studio.sections, nyx.generated.view,
  nyx.test.resource.workbench;

type
  TImageTransfer = class external name 'DataTransfer'(TJSDataTransfer)
    constructor new;
  end;
  TImageEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  TWorkbenchInputObserver = class
  public
    Handler: TNyxEventHandler; { borrowed ordinary controller receiver }
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

const
  CEditor = 'studio-resource-editor';
  CRowEditor = 'studio-resource-rows';

var
  GStudio: TNyxStudio;
  GExport: TNyxText;
  GChecks: Integer;
  GInputObserver: TWorkbenchInputObserver;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Resource workbench Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

function Pause: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(True);
        end, 50);
    end);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  Check(Result <> nil, 'mounted ' + AID);
end;

function Readiness: TNyxText;
var
  LSection: TNyxStudioSection;
begin
  Result := '';
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin

    if GStudio.ShellView.SectionView(LSection) <> nil then
    begin
      Result := Result + ' / ' + NyxStudioSectionRootID(LSection) + '=' +
        BoolToStr(GStudio.ShellView.SectionView(LSection).SectionPublicationReady, True);
    end;
  end;
end;

procedure Click(const AID: TNyxText); async;
var
  LFace: TJSHTMLElement;
  LRetainedInput: TJSHTMLElement;
  LStarted: Double;
begin
  LRetainedInput := nil;

  if (AID = NyxResourceEditorActionID(CEditor, reaNew)) or
    (Pos(CEditor + '-entry-', AID) = 1) then
  begin
    LRetainedInput := TJSHTMLElement(Find(NyxResourceEditorFieldID(CEditor, refContent))
      .querySelector('textarea'));
    Check(LRetainedInput <> nil, 'actual resource memo is mounted before navigation');
  end;
  LFace := Find(AID);
  Check(LFace.getBoundingClientRect.height > 0, 'visible ' + AID);
  LFace.scrollIntoView;
  LFace.click;
  { View retirement is queued beyond borrowed input dispatch. Exercise the
    same commands after that UI turn rather than demanding synchronous DOM
    replacement inside a callback. No design or acceptance check is skipped. }
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(Pause));
    Check(window.performance.now - LStarted < 30000,
      'queued presentation remains bounded' + Readiness);
  until not GStudio.PresentationPending;

  if LRetainedInput <> nil then
  begin
    Check(Find(NyxResourceEditorFieldID(CEditor, refContent)).querySelector('textarea') = LRetainedInput,
      'New/Open retains the actual resource input element');
  end;
end;

procedure Panel(const AName: TNyxText); async;
var
  LFace: TJSHTMLElement;
begin
  LFace := TJSHTMLElement(document.querySelector('[data-node="action-panel-' + AName + '"]'));

  if (LFace <> nil) and (LFace.getBoundingClientRect.height > 0) then
  begin
    await(Click('action-panel-' + AName));
  end;
end;

procedure Action(const AID, ABranch, ACommand: TNyxText); async;
begin

  if Find(AID).getBoundingClientRect.height > 0 then
  begin
    await(Click(AID));
    Exit;
  end;
  await(Click('action-actions'));
  await(Click('studio-menu-' + ABranch));
  await(Click('studio-menu-' + ACommand));
end;

procedure Files; async;
begin
  await(Panel('project'));

  if document.querySelector('[data-node="studio-project-files"]') = nil then
  begin
    await(Action('action-import', 'project', 'open'));
  end;
end;

function Exported(AEvent: TJSEvent): Boolean;
var
  LAnchor: TJSHTMLAnchorElement;
begin
  Result := True;

  if not (AEvent.target is TJSHTMLAnchorElement) then
  begin
    Exit;
  end;
  LAnchor := TJSHTMLAnchorElement(AEvent.target);

  if LAnchor.download = '' then
  begin
    Exit;
  end;
  AEvent.preventDefault;

  if LAnchor.download = 'project.nyxproject' then
  begin
    GExport := decodeURIComponent(Copy(LAnchor.href, Pos(',', LAnchor.href) + 1, MaxInt));
  end;
end;

function Snapshot: TNyxText; async;
begin
  await(Files);
  GExport := '';
  await(Click('action-project-export'));
  Check(GExport <> '', 'ordinary backup exposes the complete paired state');
  Result := EncodeNyxProject(DecodeNyxProject(GExport));
  await(Panel('inspector'));
end;

procedure SupplyFiles(AInput: TJSHTMLInputElement; ATransfer: TImageTransfer);
var
  LDescriptor: TJSObject;
begin
  Check(AInput <> nil, 'ordinary file input exists');
  LDescriptor := TJSObject.new;
  LDescriptor['value'] := ATransfer.files;
  TJSObject.defineProperty(AInput, 'files', LDescriptor);
  AInput.dispatchEvent(TJSEvent.new('change'));
end;

procedure ResourceChange(AField: TNyxResourceEditorField; const AValue: TNyxText);
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
  Check(LInput <> nil, 'ordinary resource input exists');

  if AField in [refBind, refFallback] then
  begin
    TJSHTMLInputElement(LInput).checked := AValue = 'true';
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;

    if LInput is TJSHTMLSelectElement then
    begin
      Check(TJSHTMLSelectElement(LInput).value = AValue, 'visible resource choice: ' + AValue);
    end;
  end;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LInput.dispatchEvent(TImageEvent.new('input', LOptions));
  LInput.dispatchEvent(TImageEvent.new('change', LOptions));
end;

function ResourceSnapshot: TNyxText; async;
begin
  Result := await(Snapshot);
  await(Panel('project'));
end;

procedure ResourceWait; async;
var
  LStarted: Double;
begin
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(Pause));
    Check(window.performance.now - LStarted < 30000, 'resource source/import readiness remains bounded');
  until not GStudio.SourceBusy and not GStudio.PresentationPending and
    (document.querySelector('input[type="file"]:not([accept])') = nil);
end;


procedure RowValue(AField: TNyxResourceRowsField; const AValue: TNyxText);
var
  LInput: TJSHTMLElement;
  LOptions: TJSObject;
begin
  LInput := Find(NyxResourceRowsFieldID(CRowEditor, AField));

  if not ((LInput is TJSHTMLInputElement) or (LInput is TJSHTMLSelectElement)) then
  begin
    LInput := TJSHTMLElement(LInput.querySelector('input,select'));
  end;
  Check(LInput <> nil, 'mounted structural row input');

  if AField = rrReplaceStatic then
  begin
    TJSHTMLInputElement(LInput).checked := AValue = 'true';
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
  end;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LInput.dispatchEvent(TImageEvent.new('input', LOptions));
  LInput.dispatchEvent(TImageEvent.new('change', LOptions));
end;

procedure Select(const AID: TNyxText); async;
var
  LChrome: TJSHTMLElement;
  LCode: TJSHTMLElement;
begin
  LChrome := nil;
  LCode := nil;

  if (GStudio.ShellView.SectionRoot(nssProject) <> nil) and
    (GStudio.ShellView.SectionRoot(nssInspector) <> nil) then
  begin
    LChrome := GStudio.ShellView.ElementFor('action-undo');
    LCode := TJSHTMLElement(document.querySelector('[data-node="studio-code"]'));
  end;
  await(Panel('design'));
  TJSHTMLElement(Find('studio-canvas').querySelector('[data-node="' + AID + '"]')).click;
  await(TJSPromise.resolve(Pause));
  await(Panel('project'));

  if LChrome <> nil then
  begin
    Check(GStudio.ShellView.ElementFor('action-undo') = LChrome,
      'changed Inspector retains the exact Chrome command');
  end;

  if LCode <> nil then
  begin
    Check(document.querySelector('[data-node="studio-code"]') = LCode,
      'changed Inspector retains the independent Pascal input');
  end;
end;

procedure ImportFile(const AName, AKind, ATitle, AHelp: TNyxText;
  const AContent: TNyxBytes); async;
var
  LTransfer: TImageTransfer;
  LBuffer: TJSUint8Array;
  LIndex: Integer;
  LBefore: TNyxText;
begin
  await(Click(NyxResourceEditorActionID(CEditor, reaNew)));
  Check(not TJSHTMLInputElement(Find(NyxResourceEditorFieldID(CEditor, refName))
    .querySelector('input')).readOnly, 'New unlocks an independent resource name');
  ResourceChange(refName, AName);
  ResourceChange(refKind, AKind);
  LBefore := await(ResourceSnapshot);
  await(Click(NyxResourceEditorActionID(CEditor, reaImport)));
  LBuffer := TJSUint8Array.new(Length(AContent));
  for LIndex := 0 to High(AContent) do
  begin
    LBuffer[LIndex] := AContent[LIndex];
  end;
  LTransfer := TImageTransfer.new;
  LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(LBuffer), AName + '.dat'));
  SupplyFiles(TJSHTMLInputElement(document.querySelector('input[type="file"]:not([accept])')),
    LTransfer);
  await(ResourceWait);
  Check(await(ResourceSnapshot) = LBefore, 'FileReader import is a copied proposal');
  ResourceChange(refTitle, ATitle);
  ResourceChange(refDescription, AHelp);
end;

procedure History(const APrevious, AAccepted: TNyxText); async;
begin
  Check(AAccepted <> APrevious, 'Apply changes the complete paired state');
  await(Action('action-undo', 'edit', 'undo'));
  await(ResourceWait);
  Check(await(ResourceSnapshot) = APrevious, 'one Undo restores exact design and Pascal');
  await(Action('action-redo', 'edit', 'redo'));
  await(ResourceWait);
  Check(await(ResourceSnapshot) = AAccepted, 'one Redo restores exact design and Pascal');
end;

{ Capture the live mounted consumer before retirement. The owning Pascal capture
  driver acknowledges each checkpoint; a DOM marker alone is never a screenshot. }
procedure Capture(const AName: TNyxText); async;
var
  LStarted: Double;
begin
  document.body.setAttribute('data-capture-checkpoint', AName);
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(Pause));
    Check(window.performance.now - LStarted < 30000, 'live capture remains bounded');
  until document.body.getAttribute('data-capture-observed') = AName;
end;

function RuntimeFailure(AEvent: TJSEvent): Boolean;
begin
  Result := True;

  if AEvent is TJSErrorEvent then
  begin
    document.body.setAttribute('data-workbench-runtime-error', TJSErrorEvent(AEvent).message);
  end;
end;

procedure TWorkbenchInputObserver.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  { Observe the real routed callback, preserving the ordinary controller.
    Bounded diagnostics distinguish a silent input route from source refusal. }
  document.body.setAttribute('data-workbench-last-input', ANode.ID);
  Check(GStudio.ShellView.Root.Find(ANode.ID) = ANode,
    'input arrives from the exact owning section');
  Handler(ANode, AEvent);
  document.body.setAttribute('data-workbench-refresh-pending',
    BoolToStr(GStudio.PresentationPending, True));
end;

procedure Journey; async;
var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LTransfer: TImageTransfer;
  LStarted: Double;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LBytes: TNyxBytes;
  LStatus: TJSHTMLElement;
  LTable: TJSHTMLElement;
begin
  GStudio := nil;
  window.addEventListener('error', @RuntimeFailure);
  try
    LDocument := BuildNyxDocument;
    try
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LDocument.Free;
    end;
    document.addEventListener('click', @Exported);
    GStudio := TNyxStudio.Create;
    GInputObserver := TWorkbenchInputObserver.Create;
    GInputObserver.Handler := GStudio.ShellView.OnEvent;
    GStudio.ShellView.OnEvent := @GInputObserver.Changed;
    GStudio.Run(False);
    await(TJSPromise.resolve(Pause));
    await(Files);
    await(Click('action-project-import'));
    LTransfer := TImageTransfer.new;
    LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(LPair.Source), 'nyx.generated.view.pas'));
    LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(LPair.Design), 'design.nyx'));
    SupplyFiles(TJSHTMLInputElement(document.querySelector('[data-nyx-project-picker]')), LTransfer);
    LStarted := window.performance.now;
    repeat
      await(TJSPromise.resolve(Pause));
      Check(window.performance.now - LStarted < 30000, 'paired import remains bounded');
    until TJSHTMLInputElement(Find('project-title').querySelector('input')).value = 'Resource workbench';
    await(Select('workshop-headline'));
    LBefore := await(ResourceSnapshot);
    Check(LBefore = EncodeNyxProject(LPair), 'ordinary paired import retains the unchanged MCP seed');
    await(Click('action-resources-toggle'));
    await(ImportFile('copy', 'JSON', WorkbenchCopyTitle, WorkbenchCopyHelp, NyxEncodeUTF8(WorkbenchJSON)));
    ResourceChange(refBind, 'true');
    ResourceChange(refTarget, NyxBindingPropertyTitle(bpText));
    ResourceChange(refPath, 'Root["literal.dot"] / text');
    await(Action('action-code', 'view', 'code'));
    await(ResourceWait);
    await(Panel('project'));
    Check(TJSHTMLInputElement(Find(NyxResourceEditorFieldID(CEditor, refDescription)).querySelector('input,textarea'))
      .value = WorkbenchCopyHelp, 'imported creator help survives source chrome');
    await(Click(NyxResourceEditorActionID(CEditor, reaApply)));
    await(ResourceWait);
    LAfter := await(ResourceSnapshot);
    await(Panel('design'));
    Check(Find('studio-canvas').querySelector('[data-node="workshop-headline"]').textContent =
      'Your resource workbench', 'actual browser caption reads accepted JSON');
    await(Panel('project'));
    await(History(LBefore, LAfter));
    await(Select('project-name'));
    await(Click(CEditor + '-entry-0'));
    ResourceChange(refBind, 'true');
    ResourceChange(refTarget, NyxBindingPropertyTitle(bpPlaceholder));
    ResourceChange(refPath, 'Root["prompt"] / text');
    LBefore := LAfter;
    await(Click(NyxResourceEditorActionID(CEditor, reaApply)));
    await(ResourceWait);
    LAfter := await(ResourceSnapshot);
    await(Panel('design'));
    Check(TJSHTMLInputElement(Find('studio-canvas').querySelector('[data-node="project-name"] input'))
      .placeholder = 'Choose a project name', 'actual browser input reads the JSON prompt');
    await(Panel('project'));
    await(History(LBefore, LAfter));
    RowValue(rrName, 'workshop-rows');
    RowValue(rrResource, NyxData('copy').ToJSON);
    await(Click(NyxResourceRowsActionID(CRowEditor, raDiscover)));
    RowValue(rrDataset, NyxResourcePath.Field('rows').ToData.ToJSON);
    await(Click(NyxResourceRowsActionID(CRowEditor, raInspect)));
    RowValue(rrIdentity, NyxResourcePath.Field('id').ToData.ToJSON);
    RowValue(rrFieldName, 'item');
    RowValue(rrFieldType, 'Text');
    RowValue(rrFieldPath, NyxResourcePath.Field('item').ToData.ToJSON);
    await(Click(NyxResourceRowsActionID(CRowEditor, raSetField)));
    RowValue(rrFieldName, 'amount');
    RowValue(rrFieldType, 'Number');
    RowValue(rrFieldPath, NyxResourcePath.Field('amount').ToData.ToJSON);
    await(Click(NyxResourceRowsActionID(CRowEditor, raSetField)));
    LBefore := LAfter;
    await(Click(NyxResourceRowsActionID(CRowEditor, raApply)));
    await(ResourceWait);
    Check(await(ResourceSnapshot) = LBefore, 'empty existing collection still requires explicit consent');
    RowValue(rrReplaceStatic, 'true');
    await(Action('action-code', 'view', 'code'));
    await(ResourceWait);
    await(Panel('project'));
    Check(TJSHTMLInputElement(Find(NyxResourceRowsFieldID(CRowEditor, rrReplaceStatic)).querySelector('input'))
      .checked, 'row consent survives source chrome');
    Find(NyxResourceRowsFieldID(CRowEditor, rrReplaceStatic)).scrollIntoView;
    await(Capture('resource-workbench-editor'));
    await(Click(NyxResourceRowsActionID(CRowEditor, raApply)));
    await(ResourceWait);
    LAfter := await(ResourceSnapshot);
    await(Panel('design'));
    LTable := TJSHTMLElement(Find('studio-canvas').querySelector('[data-node="workshop-table"]'));
    Check(LTable.querySelectorAll('tbody tr').length = 2, 'actual browser table has two runtime rows');
    Check((Pos('Canvas', LTable.textContent) > 0) and (Pos('Studio', LTable.textContent) > 0) and
      (Pos('3.125', LTable.textContent) > 0) and (Pos('6.5', LTable.textContent) > 0),
      'actual browser cells read both typed fields');
    await(Panel('project'));
    await(History(LBefore, LAfter));
    await(Click(NyxResourceRowsActionID(CRowEditor, raLoad)));
    await(Click(NyxResourceRowsActionID(CRowEditor, raDetach)));
    await(ResourceWait);
    LPair := DecodeNyxProject(await(ResourceSnapshot));
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      Check(not LDocument.ResourceCollections.HasSource(NyxCollection('workshop-rows')) and
        (LDocument.Collections.Snapshot(NyxCollection('workshop-rows')).Count = 2),
        'ordinary detach keeps two static defaults');
    finally
      LDocument.Free;
    end;
    await(Action('action-undo', 'edit', 'undo'));
    await(ResourceWait);
    Check(await(ResourceSnapshot) = LAfter, 'detach Undo restores the exact relationship');
    await(Select('workshop-notes'));
    LBefore := LAfter;
    await(ImportFile('notes', 'Text', WorkbenchNotesTitle, WorkbenchNotesHelp, NyxEncodeUTF8(WorkbenchNotes)));
    ResourceChange(refBind, 'true');
    ResourceChange(refTarget, NyxBindingPropertyTitle(bpText));
    ResourceChange(refPath, 'File text / text');
    await(Click(NyxResourceEditorActionID(CEditor, reaApply)));
    await(ResourceWait);
    LAfter := await(ResourceSnapshot);
    await(Panel('design'));
    Check(Find('studio-canvas').querySelector('[data-node="workshop-notes"]').textContent =
      WorkbenchNotes, 'actual browser label reads packed plain text');
    await(Panel('project'));
    await(History(LBefore, LAfter));
    SetLength(LBytes, 3);
    LBytes[0] := 0;
    LBytes[1] := 1;
    LBytes[2] := 255;
    LBefore := LAfter;
    await(ImportFile('packed', 'Binary', WorkbenchPackedTitle, WorkbenchPackedHelp, LBytes));
    await(Click(NyxResourceEditorActionID(CEditor, reaApply)));
    await(ResourceWait);
    LAfter := await(ResourceSnapshot);
    await(History(LBefore, LAfter));
    LPair := DecodeNyxProject(LAfter);
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      Inc(GChecks, CheckNyxResourceWorkbench(LDocument));
    finally
      LDocument.Free;
    end;
    document.body.setAttribute('data-workbench-source', encodeURIComponent(LPair.Source));
    await(Panel('design'));
    Find('studio-canvas').scrollIntoView;
    await(Capture('resource-workbench-canvas'));
    window.removeEventListener('error', @RuntimeFailure);
    document.removeEventListener('click', @Exported);
    FreeAndNil(GStudio);
    FreeAndNil(GInputObserver);
    Check(document.querySelector('[data-node="studio-shell"]') = nil, 'controller retires its owned views');
    document.body.setAttribute('data-workbench-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
  except
    on LException: Exception do
    begin
      LStatus := TJSHTMLElement(document.querySelector('[data-node="studio-status"]'));

      if LStatus <> nil then
      begin
        document.body.setAttribute('data-workbench-last-status', Copy(LStatus.textContent, 1, 500));
      end;
      { Retain the last ordinary backup for diagnosis before removing views.
        This is the isolated test project, never an observing user's document. }
      document.body.setAttribute('data-workbench-last-pair', encodeURIComponent(GExport));
      window.removeEventListener('error', @RuntimeFailure);
      document.removeEventListener('click', @Exported);
      FreeAndNil(GStudio);
      FreeAndNil(GInputObserver);
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;

procedure RunNyxResourceWorkbenchStudioQualification;
begin
  Journey;
end;

end.
