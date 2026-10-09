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

uses SysUtils, JS, Web, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.codec, nyx.codegen, nyx.studio.projects, nyx.studio.browser,
  nyx.resources, nyx.resources.editor, nyx.resources.rows.editor,
  nyx.collections, nyx.binding.types, nyx.generated.view,
  nyx.test.resource.workbench;

type
  TImageTransfer = class external name 'DataTransfer'(TJSDataTransfer)
    constructor new;
  end;
  TImageEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

const
  CEditor = 'studio-resource-editor';
  CRowEditor = 'studio-resource-rows';

var
  GStudio: TNyxStudio;
  GExport: TNyxText;
  GChecks: Integer;

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

procedure Click(const AID: TNyxText);
var
  LFace: TJSHTMLElement;
  LRetainedInput: TJSHTMLElement;
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

  if LRetainedInput <> nil then
  begin
    Check(Find(NyxResourceEditorFieldID(CEditor, refContent)).querySelector('textarea') = LRetainedInput,
      'New/Open retains the actual resource input element');
  end;
end;

procedure Panel(const AName: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  LFace := TJSHTMLElement(document.querySelector('[data-node="action-panel-' + AName + '"]'));

  if (LFace <> nil) and (LFace.getBoundingClientRect.height > 0) then
  begin
    Click('action-panel-' + AName);
  end;
end;

procedure Action(const AID, ABranch, ACommand: TNyxText);
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

procedure Files;
begin
  Panel('project');

  if document.querySelector('[data-node="studio-project-files"]') = nil then
  begin
    Action('action-import', 'project', 'open');
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

function Snapshot: TNyxText;
begin
  Files;
  GExport := '';
  Click('action-project-export');
  Check(GExport <> '', 'ordinary backup exposes the complete paired state');
  Result := EncodeNyxProject(DecodeNyxProject(GExport));
  Panel('inspector');
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

function ResourceSnapshot: TNyxText;
begin
  Result := Snapshot;
  Panel('project');
end;

procedure ResourceWait; async;
var
  LStarted: Double;
begin
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(Pause));
    Check(window.performance.now - LStarted < 30000, 'resource source/import readiness remains bounded');
  until not GStudio.SourceBusy and
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

procedure Select(const AID: TNyxText);
begin
  Panel('design');
  TJSHTMLElement(Find('studio-canvas').querySelector('[data-node="' + AID + '"]')).click;
  Panel('project');
end;

procedure ImportFile(const AName, AKind, ATitle, AHelp: TNyxText;
  const AContent: TNyxBytes); async;
var
  LTransfer: TImageTransfer;
  LBuffer: TJSUint8Array;
  LIndex: Integer;
  LBefore: TNyxText;
begin
  Click(NyxResourceEditorActionID(CEditor, reaNew));
  Check(not TJSHTMLInputElement(Find(NyxResourceEditorFieldID(CEditor, refName))
    .querySelector('input')).readOnly, 'New unlocks an independent resource name');
  ResourceChange(refName, AName);
  ResourceChange(refKind, AKind);
  LBefore := ResourceSnapshot;
  Click(NyxResourceEditorActionID(CEditor, reaImport));
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
  Check(ResourceSnapshot = LBefore, 'FileReader import is a copied proposal');
  ResourceChange(refTitle, ATitle);
  ResourceChange(refDescription, AHelp);
end;

procedure History(const APrevious, AAccepted: TNyxText); async;
begin
  Check(AAccepted <> APrevious, 'Apply changes the complete paired state');
  Action('action-undo', 'edit', 'undo');
  await(ResourceWait);
  Check(ResourceSnapshot = APrevious, 'one Undo restores exact design and Pascal');
  Action('action-redo', 'edit', 'redo');
  await(ResourceWait);
  Check(ResourceSnapshot = AAccepted, 'one Redo restores exact design and Pascal');
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
  try
    LDocument := BuildNyxDocument;
    try
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LDocument.Free;
    end;
    document.addEventListener('click', @Exported);
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    await(TJSPromise.resolve(Pause));
    Files;
    Click('action-project-import');
    LTransfer := TImageTransfer.new;
    LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(LPair.Source), 'nyx.generated.view.pas'));
    LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(LPair.Design), 'design.nyx'));
    SupplyFiles(TJSHTMLInputElement(document.querySelector('[data-nyx-project-picker]')), LTransfer);
    LStarted := window.performance.now;
    repeat
      await(TJSPromise.resolve(Pause));
      Check(window.performance.now - LStarted < 30000, 'paired import remains bounded');
    until TJSHTMLInputElement(Find('project-title').querySelector('input')).value = 'Resource workbench';
    Select('workshop-headline');
    LBefore := ResourceSnapshot;
    Check(LBefore = EncodeNyxProject(LPair), 'ordinary paired import retains the unchanged MCP seed');
    Click('action-resources-toggle');
    await(ImportFile('copy', 'JSON', WorkbenchCopyTitle, WorkbenchCopyHelp, NyxEncodeUTF8(WorkbenchJSON)));
    ResourceChange(refBind, 'true');
    ResourceChange(refTarget, NyxBindingPropertyTitle(bpText));
    ResourceChange(refPath, 'Root["literal.dot"] / text');
    Action('action-code', 'view', 'code');
    await(ResourceWait);
    Panel('project');
    Check(TJSHTMLInputElement(Find(NyxResourceEditorFieldID(CEditor, refDescription)).querySelector('input,textarea'))
      .value = WorkbenchCopyHelp, 'imported creator help survives source chrome');
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    await(ResourceWait);
    LAfter := ResourceSnapshot;
    Panel('design');
    Check(Find('studio-canvas').querySelector('[data-node="workshop-headline"]').textContent =
      'Your resource workbench', 'actual browser caption reads accepted JSON');
    Panel('project');
    await(History(LBefore, LAfter));
    Select('project-name');
    Click(CEditor + '-entry-0');
    ResourceChange(refBind, 'true');
    ResourceChange(refTarget, NyxBindingPropertyTitle(bpPlaceholder));
    ResourceChange(refPath, 'Root["prompt"] / text');
    LBefore := LAfter;
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    await(ResourceWait);
    LAfter := ResourceSnapshot;
    Panel('design');
    Check(TJSHTMLInputElement(Find('studio-canvas').querySelector('[data-node="project-name"] input'))
      .placeholder = 'Choose a project name', 'actual browser input reads the JSON prompt');
    Panel('project');
    await(History(LBefore, LAfter));
    RowValue(rrName, 'workshop-rows');
    RowValue(rrResource, NyxData('copy').ToJSON);
    Click(NyxResourceRowsActionID(CRowEditor, raDiscover));
    RowValue(rrDataset, NyxResourcePath.Field('rows').ToData.ToJSON);
    Click(NyxResourceRowsActionID(CRowEditor, raInspect));
    RowValue(rrIdentity, NyxResourcePath.Field('id').ToData.ToJSON);
    RowValue(rrFieldName, 'item');
    RowValue(rrFieldType, 'Text');
    RowValue(rrFieldPath, NyxResourcePath.Field('item').ToData.ToJSON);
    Click(NyxResourceRowsActionID(CRowEditor, raSetField));
    RowValue(rrFieldName, 'amount');
    RowValue(rrFieldType, 'Number');
    RowValue(rrFieldPath, NyxResourcePath.Field('amount').ToData.ToJSON);
    Click(NyxResourceRowsActionID(CRowEditor, raSetField));
    LBefore := LAfter;
    Click(NyxResourceRowsActionID(CRowEditor, raApply));
    await(ResourceWait);
    Check(ResourceSnapshot = LBefore, 'empty existing collection still requires explicit consent');
    RowValue(rrReplaceStatic, 'true');
    Action('action-code', 'view', 'code');
    await(ResourceWait);
    Panel('project');
    Check(TJSHTMLInputElement(Find(NyxResourceRowsFieldID(CRowEditor, rrReplaceStatic)).querySelector('input'))
      .checked, 'row consent survives source chrome');
    Find(NyxResourceRowsFieldID(CRowEditor, rrReplaceStatic)).scrollIntoView;
    await(Capture('resource-workbench-editor'));
    Click(NyxResourceRowsActionID(CRowEditor, raApply));
    await(ResourceWait);
    LAfter := ResourceSnapshot;
    Panel('design');
    LTable := TJSHTMLElement(Find('studio-canvas').querySelector('[data-node="workshop-table"]'));
    Check(LTable.querySelectorAll('tbody tr').length = 2, 'actual browser table has two runtime rows');
    Check((Pos('Canvas', LTable.textContent) > 0) and (Pos('Studio', LTable.textContent) > 0) and
      (Pos('3.125', LTable.textContent) > 0) and (Pos('6.5', LTable.textContent) > 0),
      'actual browser cells read both typed fields');
    Panel('project');
    await(History(LBefore, LAfter));
    Click(NyxResourceRowsActionID(CRowEditor, raLoad));
    Click(NyxResourceRowsActionID(CRowEditor, raDetach));
    await(ResourceWait);
    LPair := DecodeNyxProject(ResourceSnapshot);
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      Check(not LDocument.ResourceCollections.HasSource(NyxCollection('workshop-rows')) and
        (LDocument.Collections.Snapshot(NyxCollection('workshop-rows')).Count = 2),
        'ordinary detach keeps two static defaults');
    finally
      LDocument.Free;
    end;
    Action('action-undo', 'edit', 'undo');
    await(ResourceWait);
    Check(ResourceSnapshot = LAfter, 'detach Undo restores the exact relationship');
    Select('workshop-notes');
    LBefore := LAfter;
    await(ImportFile('notes', 'Text', WorkbenchNotesTitle, WorkbenchNotesHelp, NyxEncodeUTF8(WorkbenchNotes)));
    ResourceChange(refBind, 'true');
    ResourceChange(refTarget, NyxBindingPropertyTitle(bpText));
    ResourceChange(refPath, 'File text / text');
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    await(ResourceWait);
    LAfter := ResourceSnapshot;
    Panel('design');
    Check(Find('studio-canvas').querySelector('[data-node="workshop-notes"]').textContent =
      WorkbenchNotes, 'actual browser label reads packed plain text');
    Panel('project');
    await(History(LBefore, LAfter));
    SetLength(LBytes, 3);
    LBytes[0] := 0;
    LBytes[1] := 1;
    LBytes[2] := 255;
    LBefore := LAfter;
    await(ImportFile('packed', 'Binary', WorkbenchPackedTitle, WorkbenchPackedHelp, LBytes));
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    await(ResourceWait);
    LAfter := ResourceSnapshot;
    await(History(LBefore, LAfter));
    LPair := DecodeNyxProject(LAfter);
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      Inc(GChecks, CheckNyxResourceWorkbench(LDocument));
    finally
      LDocument.Free;
    end;
    document.body.setAttribute('data-workbench-source', encodeURIComponent(LPair.Source));
    Panel('design');
    Find('studio-canvas').scrollIntoView;
    await(Capture('resource-workbench-canvas'));
    document.removeEventListener('click', @Exported);
    FreeAndNil(GStudio);
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
      document.removeEventListener('click', @Exported);
      FreeAndNil(GStudio);
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
