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
  SysUtils, JS, Web, nyx.text, nyx.studio.browser;

type
  { The installed Web declaration exposes only Event(type). Keep the standard
    cancelable constructor dictionary at this host boundary without editing RTL. }
  TBrowserEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
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

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Source workspace: ' + AReason);
  end;
  Inc(GChecks);
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

procedure Failed(const AMessage: TNyxText);
begin
  document.body.setAttribute('data-source-editor', 'failed');
  document.body.setAttribute('data-source-editor-error', AMessage);
end;

procedure FinishAllocation;
begin
  try
    RetainedInput;
    Check(GInput.getBoundingClientRect.height >= 100,
      'restoring the host restores a readable source viewport');
    Find('action-expand-source').click;
    document.body.setAttribute('data-source-editor', 'passed');
    document.body.setAttribute('data-source-editor-checks', IntToStr(GChecks));
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
    Check((LSecond.getBoundingClientRect.height >= 279) and
      (GInput.getBoundingClientRect.height >= 100),
      'automatic host resize reserves the public source-pane minimum');
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
  LDialog: TJSHTMLElement;
  LHeight: Double;
  LIndex: Integer;
  LCancel: TJSEvent;
  LOptions: TJSObject;
begin
  try
    WorkspaceAction('action-code', 'view', 'code');
    GInput := TJSHTMLTextAreaElement(Find('studio-code'));
    LHeight := GInput.getBoundingClientRect.height;
    Check(LHeight > 60, 'split source has a usable physical face');
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
    RetainedInput;
    Check(GInput.getBoundingClientRect.height >= 100,
      'source stays readable with Outputs in the shared center');

    Find('action-messages-tab').click;
    Check(Find('studio-source-messages').getBoundingClientRect.height > 0,
      'compiler messages have a separate visible tab');
    Check(GInput.getBoundingClientRect.height = 0, 'messages do not crowd a visible source face');
    Find('action-source-tab').click;
    RetainedInput;
    Check(GInput.getBoundingClientRect.height > 60, 'Source tab restores usable editor space');

    Find('action-expand-source').click;
    LDialog := TJSHTMLElement(document.querySelector('dialog[open]'));
    Check(LDialog <> nil, 'Expand opens the real browser modal');
    Check(LDialog.contains(GInput), 'modal contains the retained editor');
    Check(GInput.getBoundingClientRect.height > LHeight + 100,
      'expanded editor gains substantial physical height');
    Check(Find('action-expand-source').textContent = 'Close', 'modal exposes an explicit Close action');
    RetainedInput;
    Find('action-messages-tab').click;
    Check(document.querySelector('dialog[open]') = LDialog,
      'tab refresh retains the same open modal host');
    Check(LDialog.contains(Find('studio-source-messages')),
      'compiler messages stay inside the expanded workspace');
    Find('action-source-tab').click;
    Check(document.querySelector('dialog[open]') = LDialog,
      'Source tab restores the same modal top-layer host');
    RetainedInput;
    Find('action-expand-source').click;
    Check(document.querySelector('dialog[open]') = nil, 'Close returns to the ordinary split view');
    RetainedInput;

    Find('action-expand-source').click;
    LDialog := TJSHTMLElement(document.querySelector('dialog[open]'));
    LOptions := TJSObject.new;
    LOptions['cancelable'] := True;
    LCancel := TBrowserEvent.new('cancel', LOptions);
    LDialog.dispatchEvent(LCancel);
    Check(LCancel.defaultPrevented, 'browser cancellation goes through the owned return path');
    Check(document.querySelector('dialog[open]') = nil, 'cancellation closes the modal');
    RetainedInput;
    Check(document.activeElement = GInput, 'cancellation returns focus to the same editor');

    { Exercise the split's own allocation observer without a renderer Sync or
      Studio repaint. The delayed stages retain the same editor while the host
      shrinks below its requested minima, then returns to its ordinary space.
      Keep the qualified expanded view painted for selective capture. This
      isolated harness opts out of recovery and agent connection; it never edits
      the observing project or its browser storage. Synthesized callbacks qualify
      the DOM path, not trusted hardware, mobile keyboard or assistive technology. }
    GSplit := Find('studio-split');
    GSplitHeight := GSplit.style.getPropertyValue('height');
    GSplitFlex := GSplit.style.getPropertyValue('flex');
    GSplit.style.setProperty('flex', 'none');
    GSplit.style.setProperty('height', '444px');
    window.setTimeout(@AfterAllocation, 200);
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
begin
  Inc(GPolls);

  if GFrame.contentDocument <> nil then
  begin
    LBody := TJSHTMLElement(GFrame.contentDocument.body);
    LResult := LBody.getAttribute('data-source-editor');

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

  if window.location.search = '?host=1' then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.cssText := 'width:390px;height:900px;border:0;display:block;';
    TJSHTMLElement(document.body).style.setProperty('margin', '0');
    document.body.appendChild(GFrame);
    GFrame.src := 'source-editor.html';
    window.setTimeout(@Observe, 100);
  end
  else
  begin
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    window.setTimeout(@Journey, 350);
  end;
end.
