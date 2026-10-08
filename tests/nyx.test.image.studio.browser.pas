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

implementation

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.model, nyx.images, nyx.image.editor,
  nyx.image.fixtures, nyx.codegen, nyx.codec, nyx.studio.projects,
  nyx.studio.browser, nyx.generated.view;

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
    SupplyFiles(TJSHTMLInputElement(document.querySelector('[data-nyx-project-picker]')), LTransfer);
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

end.
