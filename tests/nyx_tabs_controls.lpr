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

program nyx_tabs_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.data, nyx.behavior, nyx.codec,
  nyx.model, nyx.controls, nyx.tabs, nyx.events, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Forms, StdCtrls, Controls, LCLType,
    nyx.render.lcl, nyx.tabs.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Native browser KeyboardEvent constructor with typed fixture options. }
  TTabKeyOptions = class external name 'Object'(TJSObject)
    key: String;
    bubbles: Boolean;
    cancelable: Boolean;
  end;
  TTabKeyEvent = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TTabKeyOptions);
  end;
  {$endif}
  TSelectionProbe = class
  public
    Calls: Integer;
    LastID: TNyxText;
    procedure Signal(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;
  { Managed callbacks own only their numeric marker. The copied scalar event
    must reach both subscribers in registration order on either target. }
  TOrderedProbe = class(TNyxEventCallback)
  private
    FMarker: Integer;
  public
    constructor Create(AMarker: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  {$ifndef PAS2JS}
  TButtonAccess = class(TButton);
  {$endif}

var
  LDocument: TNyxDocument;
  LTabs: INyxTabs;
  LPage: INyxTab;
  LChecks: Integer;
  LIndex: Integer;
  LProbe: TSelectionProbe;
  LBefore: TNyxText;
  LOrder: TNyxText;
  LFirst: INyxEventSubscription;
  LSecond: INyxEventSubscription;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LOptions: TTabKeyOptions;
  LHeader: TJSHTMLElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LControl: TNyxLCLTabHost;
  LSiblingControl: TNyxLCLTabHost;
  LMemo: TMemo;
  LKey: Word;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tab controls: ' + AReason);
  end;
  Inc(LChecks);
end;

constructor TOrderedProbe.Create(AMarker: Integer);
begin
  inherited Create;
  FMarker := AMarker;
end;

procedure TOrderedProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  LOrder := LOrder + TNyxText(IntToStr(FMarker)) + TNyxText(':') + AEvent.Value.AsText + TNyxText(';');
end;

procedure Activate(AIndex: Integer);
begin
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(LHost.querySelector('[data-nyx-tab="page-' + IntToStr(AIndex) + '"]')).click;
  {$else}
  TButtonAccess(LControl.Header(AIndex)).Click;
  {$endif}
end;

procedure TSelectionProbe.Signal(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (ANode.ID = 'details-tabs') and (AEvent.Trigger = ntChange) then
  begin
    Check(AEvent.HasValue and AEvent.Changed and (AEvent.Value.Kind = ndText),
      'selection callback supplies a typed changed value');
    Inc(Calls);
    LastID := AEvent.Value.AsText;
  end;
end;

begin
  LDocument := nil;
  LRenderer := nil;
  LHost := nil;
  LProbe := TSelectionProbe.Create;
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    LDocument := TNyxDocument.Create;
    LTabs := NewNyxTabs('details-tabs');
    LTabs.Configure.Text('Project details').Height(400).Width(600).Done;
    LDocument.AddPage(LTabs);
    for LIndex := 0 to 3 do
    begin
      LPage := NewNyxTab(TNyxText('page-') + TNyxText(IntToStr(LIndex)));
      LPage.Configure.Text(TNyxText('Page ') + TNyxText(IntToStr(LIndex))).Done;

      if LIndex = 1 then
      begin
        LPage.Configure.Enabled(False).Done;
      end;

      if LIndex = 2 then
      begin
        LPage.Configure.Visible(False).Done;
      end;
      LPage.Add(NewNyxMemo(TNyxText('memo-') + TNyxText(IntToStr(LIndex)))
        .Configure.Text('Notes').Value('A retained draft').Done);
      LTabs.Add(LPage);
    end;
    LPage := nil;
    LBefore := TNyxCodec.Encode(LDocument);
    {$ifdef PAS2JS}
    LRenderer := TNyxBrowserRenderer.Create;
    LRenderer.OnEvent := @LProbe.Signal;
    LHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(LHost);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Check(LHost.querySelectorAll('[role="tab"]').length = 4, 'browser retains all tab identities');
    Check(TJSHTMLButtonElement(LHost.querySelector('[data-nyx-tab="page-1"]')).disabled,
      'disabled header remains present without activation');
    Check(TJSHTMLElement(LHost.querySelector('[data-nyx-tab="page-2"]')).hidden,
      'hidden header does not occupy the navigation strip');
    Check(LHost.querySelectorAll('[role="tabpanel"]:not([hidden])').length = 1,
      'browser exposes exactly one selected retained panel');
    TJSHTMLTextAreaElement(LRenderer.InputFor('memo-0')).value := 'Edited before switching';
    TJSHTMLButtonElement(LHost.querySelector('[data-nyx-tab="page-3"]')).click;
    Check(TJSHTMLElement(LHost.querySelector('[data-nyx-tab="page-3"]'))
      .getAttribute('aria-selected') = 'true', 'browser click selects the requested page');
    Check(not LRenderer.ElementFor('page-3').hidden and LRenderer.ElementFor('page-0').hidden,
      'browser switches visibility without disposing pages');
    TJSHTMLButtonElement(LHost.querySelector('[data-nyx-tab="page-0"]')).click;
    Check(TJSHTMLTextAreaElement(LRenderer.InputFor('memo-0')).value = 'Edited before switching',
      'browser memo survives tab return');
    Check((LProbe.Calls = 2) and (LProbe.LastID = 'page-0'), 'exact selection callbacks fire once per change');
    TJSHTMLButtonElement(LHost.querySelector('[data-nyx-tab="page-0"]')).click;
    Check(LProbe.Calls = 2, 'unchanged activation emits no additional selection');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'browser interaction preserves authored source');
    SetNyxTabsOptions(LTabs, TNyxTabsOptions.Defaults.Activation(ntaManual)
      .Orientation(ntoVertical).Initial(NyxControl('page-3')).Wrap(False));
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LHeader := TJSHTMLElement(LHost.querySelector('[data-nyx-tab="page-3"]'));
    LHeader.focus;
    LOptions := TTabKeyOptions.new;
    LOptions.key := 'ArrowUp';
    LOptions.bubbles := True;
    LOptions.cancelable := True;
    LHeader.dispatchEvent(TTabKeyEvent.new('keydown', LOptions));
    Check(TJSHTMLElement(LHost.querySelector('[data-nyx-tab="page-0"]')).tabIndex = 0,
      'browser vertical arrow moves the one tab entry across unavailable headers');
    Check(LHeader.getAttribute('aria-selected') = 'true', 'browser manual focus retains selection');
    LOptions.key := 'Enter';
    LHeader := TJSHTMLElement(LHost.querySelector('[data-nyx-tab="page-0"]'));
    LHeader.dispatchEvent(TTabKeyEvent.new('keydown', LOptions));
    Check(LHeader.getAttribute('aria-selected') = 'true', 'browser Enter explicitly activates manual focus');
    Check(LProbe.Calls = 3, 'browser manual focus has one selection callback only at activation');
    {$else}
    LRenderer := TNyxLCLRenderer.Create;
    LRenderer.OnEvent := LProbe.Signal;
    LHost := TForm.CreateNew(nil);
    LHost.SetBounds(0, 0, 700, 500);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LHost.Show;
    Application.ProcessMessages;
    LControl := TNyxLCLTabHost(LRenderer.ControlFor('details-tabs'));
    Check(LControl.State.Count = 4, 'native retains all tab identities');
    Check(not LControl.Header(1).Enabled, 'native disabled header remains descriptive');
    Check(not LControl.Header(2).Visible, 'native hidden header leaves the strip');
    Check(LControl.State.SelectedIndex = 0, 'native selects the first page');
    LMemo := TMemo(LRenderer.InputFor('memo-0'));
    LMemo.Text := 'Edited before switching';
    TButtonAccess(LControl.Header(3)).Click;
    Check(LControl.State.SelectedIndex = 3, 'native header click selects requested page');
    TButtonAccess(LControl.Header(0)).Click;
    Check(TMemo(LRenderer.InputFor('memo-0')) = LMemo, 'native memo identity survives tab return');
    Check(TNyxText(LMemo.Text) = 'Edited before switching', 'native draft survives retained pages');
    Check((LProbe.Calls = 2) and (LProbe.LastID = 'page-0'), 'native callback has the exact selected identity');
    TButtonAccess(LControl.Header(0)).Click;
    Check(LProbe.Calls = 2, 'native unchanged activation emits no selection');
    LKey := VK_RIGHT;
    LControl.Header(0).OnKeyDown(LControl.Header(0), LKey, []);
    Check((LKey = 0) and (LControl.State.SelectedIndex = 3), 'native automatic arrow skips unavailable tabs');
    LKey := VK_DOWN;
    LControl.Header(3).OnKeyDown(LControl.Header(3), LKey, []);
    Check((LKey = VK_DOWN) and (LControl.State.SelectedIndex = 3), 'horizontal tabs preserve unrelated arrow defaults');
    LKey := VK_LEFT;
    LControl.Header(3).OnKeyDown(LControl.Header(3), LKey, [ssMeta]);
    Check((LKey = VK_LEFT) and (LControl.State.SelectedIndex = 3), 'native modifier chords preserve host defaults');
    LControl.SetBounds(0, 0, 20, 20);
    Check((LControl.PageHost(0).Width >= 0) and (LControl.PageHost(0).Height >= 0),
      'small native allocation never produces negative page bounds');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'native interaction preserves authored source');
    SetNyxTabsOptions(LTabs, TNyxTabsOptions.Defaults.Activation(ntaManual)
      .Orientation(ntoVertical).Initial(NyxControl('page-3')).Wrap(False));
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LControl := TNyxLCLTabHost(LRenderer.ControlFor('details-tabs'));
    LKey := VK_UP;
    LControl.Header(3).OnKeyDown(LControl.Header(3), LKey, []);
    Check((LControl.State.FocusIndex = 0) and (LControl.State.SelectedIndex = 3),
      'native vertical manual arrow separates focus and selection');
    LKey := VK_RETURN;
    LControl.Header(0).OnKeyDown(LControl.Header(0), LKey, []);
    Check((LControl.State.SelectedIndex = 0) and (LKey = 0), 'native Enter activates manual focus');
    Check(LControl.Header(0).TabStop and not LControl.Header(3).TabStop,
      'native headers retain one tab stop');
    {$endif}
    LFirst := LRenderer.Events.On(NyxControlEvents('details-tabs'), ntChange)
      .Policy(neSequential).Subscribe(TOrderedProbe.Create(1));
    LSecond := LRenderer.Events.On(NyxControlEvents('details-tabs'), ntChange)
      .Subscribe(TOrderedProbe.Create(2));
    LOrder := '';
    Activate(3);
    Check(LOrder = '1:page-3;2:page-3;', 'managed selection callbacks retain registration order and copied values');
    LSecond.Cancel;
    LOrder := '';
    Activate(0);
    Check(LOrder = '1:page-0;', 'explicit callback removal suppresses only that subscriber');
    LRenderer.Root.Configure.Enabled(False).Done;
    LRenderer.Sync;
    LOrder := '';
    Activate(3);
    Check(LOrder = '', 'disabled mounted tabs suppress selection dispatch');
    {$ifdef PAS2JS}
    Check(TJSHTMLButtonElement(LHost.querySelector('[data-nyx-tab="page-3"]')).disabled,
      'browser presents the live root restriction on headers');
    {$else}
    Check(not LControl.Enabled, 'native parent enforces the live root restriction');
    {$endif}
    LRenderer.Free;
    LRenderer := nil;
    Check(not LFirst.Active and not LSecond.Active, 'view retirement closes retained subscription receipts');
    LDocument.Free;
    LDocument := nil;
    LTabs := nil;
    { Real adapters also realize two independently owned reusable instances.
      The initial authored reference must resolve inside each qualified scope. }
    LDocument := TNyxDocument.Create;
    LTabs := NewNyxTabs('reusable-tabs');
    LTabs.Configure.Height(240).Done;
    LPage := NewNyxTab('first-page');
    LPage.Configure.Text('First').Done;
    LPage.Add(NewNyxMemo('first-memo').Configure.Text('Notes').Done);
    LTabs.Add(LPage);
    LTabs.Add(NewNyxTab('second-page').Configure.Text('Second').Done);
    SetNyxTabsOptions(LTabs, TNyxTabsOptions.Defaults.Initial(NyxControl('second-page')));
    LDocument.AddComponent(LTabs);
    LDocument.AddPage(NewNyxPage('reuse-page'));
    LDocument.Pages[0].Add(NewNyxComponent('first-instance')
      .Configure.Component(NyxComponent('reusable-tabs')).Done);
    LDocument.Pages[0].Add(NewNyxComponent('second-instance')
      .Configure.Component(NyxComponent('reusable-tabs')).Done);
    LPage := nil;
    LBefore := TNyxCodec.Encode(LDocument);
    {$ifdef PAS2JS}
    LRenderer := TNyxBrowserRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Check(LHost.querySelectorAll('[role="tab"][aria-selected="true"]').length = 2,
      'browser independently selects both reusable initial tabs');
    TJSHTMLButtonElement(LHost.querySelector('[data-nyx-tab="first-instance/first-page"]')).click;
    Check(TJSHTMLElement(LHost.querySelector('[data-nyx-tab="first-instance/first-page"]'))
      .getAttribute('aria-selected') = 'true', 'browser resolves first instance runtime identity');
    Check(TJSHTMLElement(LHost.querySelector('[data-nyx-tab="second-instance/second-page"]'))
      .getAttribute('aria-selected') = 'true', 'browser instance change preserves sibling selection');
    {$else}
    LRenderer := TNyxLCLRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LControl := TNyxLCLTabHost(LRenderer.ControlFor('first-instance/reusable-tabs'));
    LSiblingControl := TNyxLCLTabHost(LRenderer.ControlFor('second-instance/reusable-tabs'));
    Check((LControl.State.SelectedIndex = 1) and (LSiblingControl.State.SelectedIndex = 1),
      'native independently selects both reusable initial tabs');
    TButtonAccess(LControl.Header(0)).Click;
    Check(LControl.State.SelectedIndex = 0, 'native resolves first instance runtime identity');
    Check(LSiblingControl.State.SelectedIndex = 1, 'native instance change preserves sibling selection');
    {$endif}
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'actual reusable tab interaction preserves exact authored source');
    LRenderer.Free;
    LRenderer := nil;
    LTabs := nil;
    LDocument.Free;
    LDocument := nil;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-tabs-controls', 'passed');
    document.body.setAttribute('data-nyx-tabs-controls-checks', IntToStr(LChecks));
    {$endif}
    WriteLn('PASS ', LChecks, ' actual retained tab control checks');
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-tabs-controls', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      {$endif}
      ExitCode := 1;
    end;
  end;
  LRenderer.Free;
  LProbe.Free;
  {$ifndef PAS2JS}LHost.Free;{$endif}
  LPage := nil;
  LTabs := nil;
  LDocument.Free;
end.
