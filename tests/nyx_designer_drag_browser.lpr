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
program nyx_designer_drag_browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses SysUtils, JS, Web, nyx.text, nyx.gestures, nyx.studio.browser;

type
  TDragEvent = class external name 'DragEvent' (TJSDragEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;

var
  GStudio: TNyxStudio;
  GEditor: TJSHTMLTextAreaElement;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GPolls: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Browser designer drag: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing shared Studio control: ' + AID);
  end;
end;

function Editor: TJSHTMLTextAreaElement;
begin
  Result := TJSHTMLTextAreaElement(Find('studio-code').querySelector('textarea'));

  if Result = nil then
  begin
    Result := TJSHTMLTextAreaElement(Find('studio-code'));
  end;
end;

procedure Failed(const AMessage: TNyxText);
begin
  document.body.setAttribute('data-result', 'failed');
  document.body.setAttribute('data-error', AMessage);

  if GStudio <> nil then
  begin
    FreeAndNil(GStudio);
  end;
end;

function NewReviewTransfer: TJSDataTransfer;
var
  LDescriptor: TJSObject;
begin
  Result := TJSDataTransfer.new;
  { This headless engine leaves a script-created store's effectAllowed at None
    even after the real source bridge writes Copy. The host supplies writable
    operation state for this explicitly synthetic review, while retaining the
    native store's formats/bytes and actual DragEvent/DOM delivery. The adapter
    must still write its typed offer and negotiate the target response. This
    does not qualify the privileged physical browser drag-manager transition. }
  LDescriptor := TJSObject.new;
  LDescriptor['configurable'] := True;
  LDescriptor['writable'] := True;
  LDescriptor['value'] := 'uninitialized';
  TJSObject.defineProperty(Result, 'effectAllowed', LDescriptor);
  LDescriptor['value'] := NyxDropOperationName(ndoNone);
  TJSObject.defineProperty(Result, 'dropEffect', LDescriptor);
end;

{ The staged host exercises actual DOM bridges, worker publication and ordinary
  Studio history. Untrusted synthetic drag events do not qualify physical
  browser drag-manager authority, mobile touch or cross-panel hardware input. }
procedure Poll;
begin
  try
    Inc(GPolls);

    if GPolls >= 400 then
    begin
      raise Exception.Create('Isolated browser worker did not finish within the bounded review');
    end;
    case GStage of
      0:
        begin

          if Pos('NewNyxLabeledButton(', Editor.value) = 0 then
          begin
            window.setTimeout(@Poll, 25);
            Exit;
          end;
          Check(Editor = GEditor, 'physical adapter publication retains the editor element');
          GAfter := Editor.value;
          Check(GAfter <> GBefore, 'drop updates the accepted adjacent source');
          Check(Find('labeled-button-1') <> nil, 'new compound reaches the observed canvas');
          Find('action-undo').click;
          GStage := 1;
        end;
      1:
        begin

          if Editor.value <> GBefore then
          begin
            window.setTimeout(@Poll, 25);
            Exit;
          end;
          Find('action-redo').click;
          GStage := 2;
        end;
      2:
        begin

          if Editor.value <> GAfter then
          begin
            window.setTimeout(@Poll, 25);
            Exit;
          end;
          Check(Editor = GEditor, 'paired history retains the editor element');
          document.body.setAttribute('data-result', 'passed');
          document.body.setAttribute('data-checks', IntToStr(GChecks));
          GStudio.Free;
          GStudio := nil;
          Exit;
        end;
    end;
    window.setTimeout(@Poll, 25);
  except
    on LError: Exception do
    begin
      Failed(LError.Message);
    end;
  end;
end;

procedure Run; async;
var
  LSource: TJSHTMLElement;
  LTarget: TJSHTMLElement;
  LTransfer: TJSDataTransfer;
  LOptions: TJSObject;
  LEvent: TDragEvent;
  LStarted: Double;
begin
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  { This review is desktop physical-drag plumbing. Compact hosts retain the
    existing touch/keyboard placement alternative instead of hidden-pane drags. }
  Check(window.innerWidth >= 900, 'use a desktop-width staged host for this drag review');
  Find('action-code').click;
  { Shell sections retire after borrowed input returns. Observe that normal
    presentation boundary before resolving the independent source control. }
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
      begin
        window.setTimeout(procedure
          begin
            AResolve(True);
          end, 20);
      end)));
    Check(window.performance.now - LStarted < 30000, 'source presentation remains bounded');
  until not GStudio.PresentationPending;
  GEditor := Editor;
  GBefore := GEditor.value;
  LSource := Find('palette-labeled-button');
  LTarget := Find('home');
  LTransfer := NewReviewTransfer;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['dataTransfer'] := LTransfer;
  LEvent := TDragEvent.new('dragstart', LOptions);
  LSource.dispatchEvent(LEvent);
  Check(not LEvent.defaultPrevented and
    (LTransfer.effectAllowed = NyxDropOperationsName([ndoCopy])) and
    (LTransfer.getData('application/x-nyx-studio-placement') <> ''),
    'actual DOM source bridge writes an opaque local lease');
  LEvent := TDragEvent.new('dragover', LOptions);
  LTarget.dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'actual designer target negotiates hover / offered ' +
    LTransfer.effectAllowed + ' / requested ' + LTransfer.dropEffect);
  Check(GEditor.value = GBefore, 'hover leaves adjacent Pascal unchanged');
  LEvent := TDragEvent.new('drop', LOptions);
  LTarget.dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'designer drop suppresses browser insertion defaults');
  LEvent := TDragEvent.new('dragend', LOptions);
  LSource.dispatchEvent(LEvent);
  window.setTimeout(@Poll, 25);
end;

procedure Launch; async;
begin
  try
    await(Run);
  except
    on LError: Exception do
    begin
      Failed(LError.Message);
    end;
  end;
end;

begin
  Launch;
end.
