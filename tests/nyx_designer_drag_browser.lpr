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

uses SysUtils, JS, Web, nyx.text, nyx.studio.browser;

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

procedure Run;
var
  LSource: TJSHTMLElement;
  LTarget: TJSHTMLElement;
  LTransfer: TJSDataTransfer;
  LOptions: TJSObject;
  LEvent: TDragEvent;
begin
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  { This review is desktop physical-drag plumbing. Compact hosts retain the
    existing touch/keyboard placement alternative instead of hidden-pane drags. }
  Check(window.innerWidth >= 900, 'use a desktop-width staged host for this drag review');
  Find('action-code').click;
  GEditor := Editor;
  GBefore := GEditor.value;
  LSource := Find('palette-labeled-button');
  LTarget := Find('home');
  LTransfer := TJSDataTransfer.new;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['dataTransfer'] := LTransfer;
  LEvent := TDragEvent.new('dragstart', LOptions);
  LSource.dispatchEvent(LEvent);
  Check(not LEvent.defaultPrevented and
    (LTransfer.getData('application/x-nyx-studio-placement') <> ''),
    'actual DOM source bridge writes an opaque local lease');
  LEvent := TDragEvent.new('dragover', LOptions);
  LTarget.dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'actual designer target negotiates hover');
  Check(GEditor.value = GBefore, 'hover leaves adjacent Pascal unchanged');
  LEvent := TDragEvent.new('drop', LOptions);
  LTarget.dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'designer drop suppresses browser insertion defaults');
  LEvent := TDragEvent.new('dragend', LOptions);
  LSource.dispatchEvent(LEvent);
  window.setTimeout(@Poll, 25);
end;

begin
  try
    Run;
  except
    on LError: Exception do
    begin
      Failed(LError.Message);
    end;
  end;
end.
