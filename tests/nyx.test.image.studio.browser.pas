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
unit nyx.test.image.studio.browser;

{$mode delphi}{$H+}{$codepage utf8}{$modeswitch externalclass}

interface

{ Runs the ordinary browser controller in an owned, recovery-disabled workspace.
  Synthetic file/input events qualify FileReader and controller wiring, not a
  physical chooser or trusted mobile input. The public result marker is written
  only after captures and explicit controller/listener retirement. }
procedure RunNyxImageStudioQualification;
{ Exercises the same ordinary Studio with its public Resources form. Keeps the
  unchanged semantic seed, imports via FileReader and observes paired history. }
procedure RunNyxResourceImageStudioQualification;

implementation

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.model, nyx.images, nyx.image.editor,
  nyx.image.fixtures, nyx.codegen, nyx.codec, nyx.studio.projects,
  nyx.studio.browser, nyx.generated.view, nyx.resources.editor, nyx.binding.types;

type
  TImageTransfer = class external name 'DataTransfer'(TJSDataTransfer)
    constructor new;
  end;
  TImageEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  TImageFace = class external name 'HTMLImageElement'(TJSHTMLImageElement)
    function decode: TJSPromise;
  end;

const
  CEditor = 'inspector-image';

var
  GStudio: TNyxStudio;
  GExport: TNyxText;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Ordinary image Studio: ' + AReason);
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
begin
  LFace := Find(AID);
  Check(LFace.getBoundingClientRect.height > 0, 'visible ' + AID);
  LFace.scrollIntoView;
  LFace.click;
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

procedure Change(AField: TNyxImageEditorField; const AValue: TNyxText);
var
  LInput: TJSHTMLElement;
  LOptions: TJSObject;
begin
  LInput := Find(NyxImageEditorFieldID(CEditor, AField));

  if not ((LInput is TJSHTMLInputElement) or (LInput is TJSHTMLTextAreaElement) or
    (LInput is TJSHTMLSelectElement)) then
  begin
    LInput := TJSHTMLElement(LInput.querySelector('input,textarea,select'));
  end;
  Check(LInput <> nil, 'actual image input exists');
  TJSHTMLInputElement(LInput).value := AValue;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LInput.dispatchEvent(TImageEvent.new('input', LOptions));
  LInput.dispatchEvent(TImageEvent.new('change', LOptions));
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

procedure SupplyImage;
var
  LTransfer: TImageTransfer;
  LBytes: TNyxImageBytes;
  LBuffer: TJSUint8Array;
  LIndex: Integer;
begin
  LTransfer := TImageTransfer.new;
  LBytes := NyxEmbeddedImage(nimPNG, ImagePNG).Bytes;
  LBuffer := TJSUint8Array.new(Length(LBytes));
  for LIndex := 0 to High(LBytes) do
  begin
    LBuffer[LIndex] := LBytes[LIndex];
  end;
  LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(LBuffer), 'banner.dat'));
  SupplyFiles(TJSHTMLInputElement(document.querySelector('input[type="file"][accept="image/png,image/jpeg"]')),
    LTransfer);
end;

procedure WaitReady; async;
var
  LStarted: Double;
begin
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(Pause));
    Check(window.performance.now - LStarted < 30000, 'source/import readiness remains bounded');
  until not GStudio.SourceBusy and
    (document.querySelector('input[type="file"][accept="image/png,image/jpeg"]') = nil);
end;

procedure Capture; async;
var
  LFace: TImageFace;
  LStarted: Double;
begin
  LFace := TImageFace(Find(CEditor+'-preview'));
  { Host decoding validates that these particular valid fixture pixels reached
    the real preview. The portable checksum contract alone makes no such claim. }
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(Pause));
    Check(window.performance.now - LStarted < 30000, 'preview decode remains bounded');
  until LFace.complete;
  await(TJSPromise.resolve(LFace.decode));
  Check((LFace.naturalWidth = 100) and (LFace.naturalHeight = 50), 'actual preview decodes the imported PNG');
  LFace.scrollIntoView;
  document.body.setAttribute('data-capture-checkpoint', 'image-policy-studio');
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(Pause));
    Check(window.performance.now - LStarted < 30000, 'capture acknowledgement remains bounded');
  until document.body.getAttribute('data-capture-observed') = 'image-policy-studio';
end;

procedure Journey; async;
var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LTransfer: TImageTransfer;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LStarted: Double;
  LExpected: TNyxImageSource;
  LDecoded: TNyxDocument;
  LStatus: TJSHTMLElement;
begin
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
    SupplyFiles(TJSHTMLInputElement(document.querySelector('[data-nyx-text-files]')), LTransfer);
    LStarted := window.performance.now;
    repeat
      await(TJSPromise.resolve(Pause));
      Check(window.performance.now - LStarted < 30000, 'paired import remains bounded');
    until TJSHTMLInputElement(Find('project-title').querySelector('input')).value = 'Image workshop';
    Panel('design');
    TJSHTMLElement(Find('studio-canvas').querySelector('[data-node="hero-image"]')).click;
    Panel('inspector');
    LBefore := Snapshot;
    Check(LBefore = EncodeNyxProject(LPair), 'ordinary paired FileReader import retains exact semantic seed');
    Click(NyxImageEditorActionID(CEditor, ieaImport));
    Change(iefValidation, 'Framing only');
    SupplyImage;
    await(WaitReady);
    Check(Pos('validation changed', document.body.textContent) > 0,
      'ordinary browser rejects late import after policy change');
    Check(Snapshot = LBefore, 'late policy refusal retains accepted pair and history');
    Click(NyxImageEditorActionID(CEditor, ieaImport));
    SupplyImage;
    await(WaitReady);
    LExpected := NyxEmbeddedImage(nimPNG, ImagePNG, NyxImageValidation.ContainerChecksums(False));
    Check(TJSHTMLImageElement(Find(CEditor+'-preview')).getAttribute('src') = LExpected.ToWire,
      'ordinary browser FileReader carries copied policy into its preview');
    Change(iefAlternative, 'An imported banner');
    Action('action-code', 'view', 'code');
    await(WaitReady);
    { Compact menu navigation parks inactive panels. Return through the visible
      Inspector route before accessing that panel's borrowed controls again. }
    Panel('inspector');
    Check(TJSHTMLImageElement(Find(CEditor+'-preview')).getAttribute('src') = LExpected.ToWire,
      'browser chrome rebuild retains proposal and policy');
    Click(NyxImageEditorActionID(CEditor, ieaApply));
    await(WaitReady);
    LAfter := Snapshot;
    Check(LAfter <> LBefore, 'ordinary browser Apply publishes a complete changed pair / '+
      Find('studio-status').textContent);
    LPair := DecodeNyxProject(LAfter);
    LDecoded := TNyxCodec.Decode(LPair.Design);
    try
      Check(LDecoded.Find('hero-image').Prop('src') = LExpected.ToWire, 'accepted browser image retains exact policy wire');
      Check(LDecoded.Find('hero-image').Prop('alt') = 'An imported banner', 'accepted browser image retains proposed alternative text');
    finally
      LDecoded.Free;
    end;
    Check(Pos('NyxImageValidation.ContainerChecksums(False)', LPair.Source) > 0,
      'ordinary browser publishes crafted fluent policy source');
    Action('action-undo', 'edit', 'undo');
    await(WaitReady);
    Check(Snapshot = LBefore, 'ordinary browser one Undo restores complete original pair');
    Action('action-redo', 'edit', 'redo');
    await(WaitReady);
    Check(Snapshot = LAfter, 'ordinary browser Redo restores exact accepted pair');
    await(Capture);
    document.removeEventListener('click', @Exported);
    FreeAndNil(GStudio);
    Check(document.querySelector('[data-node="studio-shell"]') = nil, 'explicit browser controller retirement removes owned views');
    document.body.setAttribute('data-image-studio-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
  except
    on LException: Exception do
    begin
      { Preserve bounded controller diagnostics before retiring the owned DOM. }
      LStatus := TJSHTMLElement(document.querySelector('[data-node="studio-status"]'));

      if LStatus <> nil then
      begin
        document.body.setAttribute('data-image-studio-last-status', Copy(LStatus.textContent, 1, 500));
      end;
      document.removeEventListener('click', @Exported);
      FreeAndNil(GStudio);
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;

procedure RunNyxImageStudioQualification;
begin
  Journey;
end;


{ These input helpers qualify the actual adapter/controller. Semantic context
  remains the unchanged MCP companion; no document mutation is hidden here. }
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

procedure ResourceCapture; async;
var
  LImage: TImageFace;
  LStarted: Double;
begin
  LImage := TImageFace(Find('studio-resource-editor-image-preview'));
  await(LImage.decode);
  Check((LImage.naturalWidth = 100) and (LImage.naturalHeight = 50),
    'ordinary Resources preview reaches decoded image dimensions');
  Find(NyxResourceEditorFieldID('studio-resource-editor', refImageLocale)).scrollIntoView;
  document.body.setAttribute('data-capture-checkpoint', 'resource-image-studio');
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(Pause));
    Check(window.performance.now - LStarted < 30000, 'resource live capture remains bounded');
  until document.body.getAttribute('data-capture-observed') = 'resource-image-studio';
end;

procedure ResourceJourney; async;
const
  CResourceEditor = 'studio-resource-editor';
var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LTransfer: TImageTransfer;
  LBefore: TNyxText;
  LRuntime: TNyxText;
  LPinned: TNyxText;
  LStarted: Double;
  LBytes: TNyxImageBytes;
  LBuffer: TJSUint8Array;
  LIndex: Integer;
  LBinding: TNyxBindingSpec;
  LStatus: TJSHTMLElement;
begin
  try
    GChecks := 0;
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
    SupplyFiles(TJSHTMLInputElement(document.querySelector('[data-nyx-text-files]')), LTransfer);
    LStarted := window.performance.now;
    repeat
      await(TJSPromise.resolve(Pause));
      Check(window.performance.now - LStarted < 30000, 'resource paired import remains bounded');
    until TJSHTMLInputElement(Find('project-title').querySelector('input')).value = 'Image workshop';
    Panel('design');
    TJSHTMLElement(Find('studio-canvas').querySelector('[data-node="hero-image"]')).click;
    Panel('project');
    LBefore := ResourceSnapshot;
    Check(LBefore = EncodeNyxProject(LPair), 'ordinary import retains exact semantic seed');
    Click('action-resources-toggle');
    ResourceChange(refName, 'cover');
    ResourceChange(refKind, 'Image');
    Click(NyxResourceEditorActionID(CResourceEditor, reaImport));
    LTransfer := TImageTransfer.new;
    LBytes := NyxEmbeddedImage(nimJPEG, ImageJPEG).Bytes;
    LBuffer := TJSUint8Array.new(Length(LBytes));
    for LIndex := 0 to High(LBytes) do
    begin
      LBuffer[LIndex] := LBytes[LIndex];
    end;
    LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(LBuffer), 'cover.dat'));
    SupplyFiles(TJSHTMLInputElement(document.querySelector('input[type="file"]:not([accept])')), LTransfer);
    await(ResourceWait);
    Check(TJSHTMLImageElement(Find(CResourceEditor+'-image-preview')).getAttribute('src') =
      NyxEmbeddedImage(nimJPEG, ImageJPEG).ToWire, 'FileReader paints exact packed JPEG proposal');
    Check(ResourceSnapshot = LBefore, 'copied import does not edit the accepted pair');
    ResourceChange(refBind, 'true');
    ResourceChange(refImageLocale, 'Follow application locale');
    Action('action-code', 'view', 'code');
    await(ResourceWait);
    Panel('project');
    Check(TJSHTMLSelectElement(Find(NyxResourceEditorFieldID(CResourceEditor, refImageLocale))
      .querySelector('select')).value = 'Follow application locale', 'chrome rebuild retains locale draft');
    Click(NyxResourceEditorActionID(CResourceEditor, reaApply));
    await(ResourceWait);
    LRuntime := ResourceSnapshot;
    LPair := DecodeNyxProject(LRuntime);
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      Check(LDocument.Find('hero-image').FindBinding(bpImage, LBinding) and
        not LBinding.ResourceImage.Localized, 'ordinary source worker admits runtime image binding');
      Check(LDocument.Resources.Count = 1, 'ordinary Apply publishes exactly one packed resource');
    finally
      LDocument.Free;
    end;
    Check(Pos('.Image(NyxResourceImage(', LPair.Source) > 0, 'crafted source uses specialized image selector');
    { Compact Studio retires its inactive canvas. Reveal Design through the
      ordinary panel route before inspecting the real bound image, then return
      to the Resources proposal rather than reading an unmounted control. }
    Panel('design');
    Check(TJSHTMLImageElement(Find('studio-canvas').querySelector('[data-node="hero-image"]'))
      .getAttribute('src') = NyxEmbeddedImage(nimJPEG, ImageJPEG).ToWire,
      'bound browser canvas replaces its original PNG source');
    Panel('project');
    Action('action-undo', 'edit', 'undo');
    await(ResourceWait);
    Check(ResourceSnapshot = LBefore, 'one Undo restores original complete semantic pair');
    Action('action-redo', 'edit', 'redo');
    await(ResourceWait);
    Check(ResourceSnapshot = LRuntime, 'Redo restores exact runtime-bound pair');
    Click(CResourceEditor+'-entry-0');
    Check(TJSHTMLSelectElement(Find(NyxResourceEditorFieldID(CResourceEditor, refImageLocale))
      .querySelector('select')).value = 'Follow application locale', 'reopened form respects current runtime binding');
    ResourceChange(refBind, 'true');
    ResourceChange(refImageLocale, 'Use this variant');
    Action('action-code', 'view', 'code');
    await(ResourceWait);
    Panel('project');
    Check(TJSHTMLSelectElement(Find(NyxResourceEditorFieldID(CResourceEditor, refImageLocale))
      .querySelector('select')).value = 'Use this variant', 'explicit default pin survives second chrome rebuild');
    Click(NyxResourceEditorActionID(CResourceEditor, reaApply));
    await(ResourceWait);
    LPinned := ResourceSnapshot;
    LPair := DecodeNyxProject(LPinned);
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      Check(LDocument.Find('hero-image').FindBinding(bpImage, LBinding) and
        LBinding.ResourceImage.Localized and not LBinding.ResourceImage.Locale.Defined,
        'explicit default pin remains distinct after source admission');
    finally
      LDocument.Free;
    end;
    Action('action-undo', 'edit', 'undo');
    await(ResourceWait);
    Check(ResourceSnapshot = LRuntime, 'pin Undo restores exact runtime locale pair');
    Action('action-redo', 'edit', 'redo');
    await(ResourceWait);
    Check(ResourceSnapshot = LPinned, 'pin Redo restores exact explicitly localized pair');
    Click(CResourceEditor+'-entry-0');
    ResourceChange(refBind, 'true');
    await(ResourceCapture);
    document.removeEventListener('click', @Exported);
    FreeAndNil(GStudio);
    Check(document.querySelector('[data-node="studio-shell"]') = nil, 'ordinary browser controller retires owned views');
    document.body.setAttribute('data-resource-image-studio-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
  except
    on LException: Exception do
    begin
      LStatus := TJSHTMLElement(document.querySelector('[data-node="studio-status"]'));

      if LStatus <> nil then
      begin
        document.body.setAttribute('data-resource-image-last-status', Copy(LStatus.textContent, 1, 500));
      end;
      document.removeEventListener('click', @Exported);
      FreeAndNil(GStudio);
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;

procedure RunNyxResourceImageStudioQualification;
begin
  ResourceJourney;
end;


end.
