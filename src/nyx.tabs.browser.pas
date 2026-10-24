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

unit nyx.tabs.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, JS, Web, nyx.text, nyx.model, nyx.tabs, nyx.scheduler;

type
  { Borrowed notification: the renderer retires this host/listeners before its
    receiver. Selection state and panel nodes stay owned independently of source.
    Listener callbacks enter only while this host is mounted and enabled. }
  TNyxBrowserTabChange = procedure(const AChange: TNyxTabTransition) of object;
  { Borrowed admission runs while accepted indices/panels are unchanged.
    False refuses the proposal; the receiver may retire the host. }
  TNyxBrowserTabAdmission = function(const AChange: TNyxTabTransition): Boolean of object;
  { Renderer-owned tab projection with terminal input retirement separate from
    final state destruction. Entered view leases keep its borrowed nodes alive. }
  TNyxBrowserTabHost = class
  private
    FRoot: TJSHTMLElement;
    FBar: TJSHTMLElement;
    FBody: TJSHTMLElement;
    FHeaders: array of TJSHTMLButtonElement;
    FPanels: array of TJSHTMLElement;
    FState: TNyxTabState;
    FOnChange: TNyxBrowserTabChange;
    FOnAdmit: TNyxBrowserTabAdmission;
    FEnabled: Boolean;
    FMovingFocus: Boolean;
    { A local strong lease survives focus callbacks that retire this host. }
    FLifetime: INyxCancellationScope;
    function HeaderIndex(AEvent: TJSEvent): Integer;
    function Click(AEvent: TJSMouseEvent): Boolean;
    function Focused(AEvent: TJSEvent): Boolean;
    function KeyDown(AEvent: TJSKeyboardEvent): Boolean;
    procedure Present;
    procedure Apply(const AChange: TNyxTabTransition; AFocus: Boolean);
    procedure RestoreHeaderFocus;
  public
    { Root is borrowed from the renderer. Constructor adopts neither source nor
      root. It creates only adapter chrome; caller attaches retained panels. }
    constructor Create(ANode: TNyxNode; ARoot: TJSHTMLElement);
    destructor Destroy; override;
    { Terminal input retirement without destroying state used by a callback
      already on the stack. The owning view releases memory after that return. }
    procedure Disconnect;
    procedure Attach(AIndex: Integer; APanel: TJSHTMLElement);
    procedure Enabled(AValue: Boolean);
    { Apply an already admitted model value without emitting an input event. }
    procedure SynchronizeValue(const AValue: TNyxText);
    { Retain header/panel identities while refreshing accepted captions,
      availability and selection; structure/policy changes require remounting. }
    procedure SynchronizeDescriptor(ANode: TNyxNode);
    function PanelHost: TJSHTMLElement;
    property State: TNyxTabState read FState;
    property OnChange: TNyxBrowserTabChange read FOnChange write FOnChange;
    property OnAdmit: TNyxBrowserTabAdmission read FOnAdmit write FOnAdmit;
  end;

implementation

constructor TNyxBrowserTabHost.Create(ANode: TNyxNode; ARoot: TJSHTMLElement);
var
  LIndex: Integer;
  LHeaderID: TNyxText;
  LCaption: TNyxText;
begin
  inherited Create;

  if ARoot = nil then
  begin
    raise ENyxModel.Create('A browser tab host requires its mounted root');
  end;
  FState := TNyxTabState.Create(ANode);
  FLifetime := NewNyxCancellationScope;
  FRoot := ARoot;
  FEnabled := ANode.Prop('enabled', 'true') <> 'false';
  FRoot.classList.add('nyx-tabs-host');
  FBar := TJSHTMLElement(document.createElement('div'));
  FBar.className := 'nyx-tab-list';
  FBar.setAttribute('role', 'tablist');
  LCaption := ANode.Prop('text');

  if LCaption = '' then
  begin
    LCaption := 'Tabs';
  end;
  FBar.setAttribute('aria-label', LCaption);

  if FState.Options.OrientationMode = ntoVertical then
  begin
    FRoot.classList.add('nyx-tabs-vertical');
    FBar.setAttribute('aria-orientation', 'vertical');
  end;
  FBody := TJSHTMLElement(document.createElement('div'));
  FBody.className := 'nyx-tab-body';
  FRoot.appendChild(FBar);
  FRoot.appendChild(FBody);
  SetLength(FHeaders, FState.Count);
  SetLength(FPanels, FState.Count);
  for LIndex := 0 to FState.Count - 1 do
  begin
    FHeaders[LIndex] := TJSHTMLButtonElement(document.createElement('button'));
    FHeaders[LIndex]._type := 'button';
    FHeaders[LIndex].className := 'nyx-tab-header';
    LHeaderID := ANode.ID + TNyxText('-tab-header-') + TNyxText(IntToStr(LIndex));
    FHeaders[LIndex].id := LHeaderID;
    FHeaders[LIndex].setAttribute('role', 'tab');
    FHeaders[LIndex].setAttribute('data-nyx-tab', FState.Entry(LIndex).Reference.ID);
    FHeaders[LIndex].textContent := FState.Entry(LIndex).Caption;
    FHeaders[LIndex].addEventListener('click', @Click);
    FHeaders[LIndex].addEventListener('focus', @Focused);
    FHeaders[LIndex].addEventListener('keydown', @KeyDown);
    FBar.appendChild(FHeaders[LIndex]);
  end;
  Present;
end;

destructor TNyxBrowserTabHost.Destroy;
begin
  Disconnect;
  FState.Free;
  inherited Destroy;
end;

procedure TNyxBrowserTabHost.Disconnect;
var
  LIndex: Integer;
begin
  FEnabled := False;

  if FLifetime <> nil then
  begin
    FLifetime.Cancel;
  end;
  FOnChange := nil;
  FOnAdmit := nil;
  for LIndex := 0 to High(FHeaders) do
  begin

    if FHeaders[LIndex] <> nil then
    begin
      FHeaders[LIndex].removeEventListener('click', @Click);
      FHeaders[LIndex].removeEventListener('focus', @Focused);
      FHeaders[LIndex].removeEventListener('keydown', @KeyDown);
    end;
  end;
end;

function TNyxBrowserTabHost.HeaderIndex(AEvent: TJSEvent): Integer;
var
  LIndex: Integer;
begin
  Result := -1;

  if not FEnabled or AEvent.defaultPrevented then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FHeaders) do
  begin

    if (AEvent.currentTarget = FHeaders[LIndex]) and FState.Entry(LIndex).Available then
    begin
      Exit(LIndex);
    end;
  end;
end;

procedure TNyxBrowserTabHost.Present;
var
  LIndex: Integer;
  LSelected: Boolean;
begin
  for LIndex := 0 to High(FHeaders) do
  begin
    LSelected := FState.SelectedIndex = LIndex;
    FHeaders[LIndex].hidden := not FState.Entry(LIndex).Visible;
    FHeaders[LIndex].disabled := not FEnabled or not FState.Entry(LIndex).Available;
    FHeaders[LIndex].setAttribute('aria-selected', BoolToStr(LSelected, 'true', 'false'));
    FHeaders[LIndex].tabIndex := -1;

    if FEnabled and (FState.FocusIndex = LIndex) then
    begin
      FHeaders[LIndex].tabIndex := 0;
    end;

    if FPanels[LIndex] <> nil then
    begin
      FPanels[LIndex].hidden := not LSelected;
    end;
  end;
end;

procedure TNyxBrowserTabHost.Apply(const AChange: TNyxTabTransition; AFocus: Boolean);
var
  LNotify: TNyxBrowserTabChange;
  LLease: INyxCancellationScope;
begin
  LLease := FLifetime;

  if AChange.SelectionChanged and Assigned(FOnAdmit) then
  begin

    if not FOnAdmit(AChange) then
    begin

      if not LLease.Cancelled then
      begin
        RestoreHeaderFocus;
      end;
      Exit;
    end;
  end;

  if LLease.Cancelled then
  begin
    Exit;
  end;
  FState.Commit(AChange);
  Present;

  if AFocus and (FState.FocusIndex >= 0) then
  begin
    FMovingFocus := True;
    try
      FHeaders[FState.FocusIndex].focus;
    finally

      if not LLease.Cancelled then
      begin
        FMovingFocus := False;
      end;
    end;
  end;

  if LLease.Cancelled then
  begin
    Exit;
  end;
  { Notify last: a receiver can replace the view and retire this host. }
  LNotify := FOnChange;

  if AChange.SelectionChanged and Assigned(LNotify) then
  begin
    LNotify(AChange);
  end;
end;

procedure TNyxBrowserTabHost.RestoreHeaderFocus;
var
  LIndex: Integer;
  LLease: INyxCancellationScope;
begin
  LLease := FLifetime;
  for LIndex := 0 to High(FHeaders) do
  begin

    if (document.activeElement = FHeaders[LIndex]) and
      (LIndex <> FState.FocusIndex) and (FState.FocusIndex >= 0) then
    begin
      FMovingFocus := True;
      try
        FHeaders[FState.FocusIndex].focus;
      finally

        if not LLease.Cancelled then
        begin
          FMovingFocus := False;
        end;
      end;
      Exit;
    end;
  end;
end;

function TNyxBrowserTabHost.Click(AEvent: TJSMouseEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;
  LIndex := HeaderIndex(AEvent);

  if LIndex >= 0 then
  begin
    AEvent.stopPropagation;
    Apply(FState.ProposeActivation(FState.Entry(LIndex).Reference), False);
  end;
end;

function TNyxBrowserTabHost.Focused(AEvent: TJSEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;

  if FMovingFocus then
  begin
    Exit;
  end;
  LIndex := HeaderIndex(AEvent);

  if LIndex >= 0 then
  begin
    Apply(FState.ProposeFocus(FState.Entry(LIndex).Reference), False);
  end;
end;

function TNyxBrowserTabHost.KeyDown(AEvent: TJSKeyboardEvent): Boolean;
var
  LIndex: Integer;
  LMove: TNyxTabMovement;
  LHasMove: Boolean;
  LChange: TNyxTabTransition;
begin
  Result := True;
  LIndex := HeaderIndex(AEvent);

  if (LIndex < 0) or AEvent.altKey or AEvent.ctrlKey or AEvent.metaKey or AEvent.isComposing then
  begin
    Exit;
  end;
  LHasMove := True;
  LMove := ntmNext;

  if AEvent.key = 'Home' then
  begin
    LMove := ntmFirst;
  end
  else if AEvent.key = 'End' then
  begin
    LMove := ntmLast;
  end
  else if ((FState.Options.OrientationMode = ntoHorizontal) and (AEvent.key = 'ArrowLeft')) or
    ((FState.Options.OrientationMode = ntoVertical) and (AEvent.key = 'ArrowUp')) then
  begin
    LMove := ntmPrevious;
  end
  else if ((FState.Options.OrientationMode = ntoHorizontal) and (AEvent.key = 'ArrowRight')) or
    ((FState.Options.OrientationMode = ntoVertical) and (AEvent.key = 'ArrowDown')) then
  begin
    LMove := ntmNext;
  end
  else
  begin
    LHasMove := False;
  end;

  if LHasMove then
  begin
    LChange := FState.ProposeMove(LMove);
  end
  else if (AEvent.key = 'Enter') or (AEvent.key = ' ') then
  begin
    LChange := FState.ProposeFocusedActivation;
  end
  else
  begin
    Exit;
  end;
  AEvent.preventDefault;
  AEvent.stopPropagation;
  Result := False;
  Apply(LChange, LHasMove);
end;

procedure TNyxBrowserTabHost.Attach(AIndex: Integer; APanel: TJSHTMLElement);
var
  LPanelID: TNyxText;
  LLeading: TJSHTMLElement;
begin
  FState.Entry(AIndex);

  if APanel = nil then
  begin
    raise ENyxModel.Create('A tab requires its retained panel');
  end;
  FPanels[AIndex] := APanel;
  LPanelID := FHeaders[AIndex].id + TNyxText('-panel');
  APanel.id := LPanelID;
  APanel.setAttribute('role', 'tabpanel');
  APanel.setAttribute('aria-labelledby', FHeaders[AIndex].id);
  FHeaders[AIndex].setAttribute('aria-controls', LPanelID);
  { A panel whose leading meaningful content is not focusable remains reachable
    from the tab list. Child control focus is otherwise retained by the browser. }
  LLeading := TJSHTMLElement(APanel.firstElementChild);
  { Follow leading layout wrappers, stopping at a meaningful caption or a
    focusable face. A later input must not hide an introductory heading from
    the panel's keyboard entry. This does not implement an accessibility tree. }
  while (LLeading <> nil) and (LLeading.tabIndex < 0) and
    (LLeading.firstElementChild <> nil) and
    ((LLeading.tagName = 'DIV') or (LLeading.tagName = 'SECTION')) do
  begin
    LLeading := TJSHTMLElement(LLeading.firstElementChild);
  end;
  APanel.tabIndex := -1;

  if (LLeading = nil) or (LLeading.tabIndex < 0) then
  begin
    APanel.tabIndex := 0;
  end;
  FBody.appendChild(APanel);
  Present;
end;

procedure TNyxBrowserTabHost.Enabled(AValue: Boolean);
begin
  FEnabled := AValue;
  Present;
end;

procedure TNyxBrowserTabHost.SynchronizeValue(const AValue: TNyxText);
begin
  FState.SynchronizeValue(AValue);
  Present;
end;

procedure TNyxBrowserTabHost.SynchronizeDescriptor(ANode: TNyxNode);
var
  LIndex: Integer;
begin
  FState.SynchronizeDescriptor(ANode);
  for LIndex := 0 to FState.Count - 1 do
  begin

    if FHeaders[LIndex].textContent <> FState.Entry(LIndex).Caption then
    begin
      FHeaders[LIndex].textContent := FState.Entry(LIndex).Caption;
    end;
  end;
  Present;
end;

function TNyxBrowserTabHost.PanelHost: TJSHTMLElement;
begin
  Result := FBody;
end;

end.
