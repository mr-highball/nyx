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
program nyx_keyboard_host_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.behavior, nyx.events, nyx.scheduler, nyx.contract, nyx.collections,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.selection,
  nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl;{$endif}

type
  { The page is compiled from bounded MCP source windows. This fixture only
    observes real controls and supplies the collection binding not yet exposed
    by MCP. No browser script is injected by the native host driver. }
  TKeyboardProbe = class(TNyxEventCallback)
  public
    Activated: Integer;
    Searched: Integer;
    Cleared: Integer;
    Decremented: Integer;
    Incremented: Integer;
    Removed: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TButtonAccess = class(TCustomButton);
  {$endif}

var
  GDocument: TNyxDocument;
  GRenderer: TRenderer;
  GView: INyxCollectionView;
  GProbe: TKeyboardProbe;
  GOwner: INyxEventCallback;
  GTokens: array of INyxEventSubscription;
  {$ifdef PAS2JS}GHost: TJSHTMLElement;
  {$else}GHost: TForm;{$endif}

procedure Require(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    {$ifdef PAS2JS}
    document.body.setAttribute('data-keyboard-result', 'failed');
    document.body.setAttribute('data-keyboard-error', AReason);
    {$endif}
    raise ENyxModel.Create(AReason);
  end;
end;

{$ifdef PAS2JS}
procedure Publish;
var
  LFocus: TJSHTMLElement;
  LRow: TJSHTMLElement;
  LRoot: TJSHTMLElement;
  LText: TNyxText;
begin
  LFocus := TJSHTMLElement(document.activeElement);
  LRoot := TJSHTMLElement(LFocus.closest('[data-runtime-id]'));
  LText := '';

  if LRoot <> nil then
  begin
    LText := LRoot.getAttribute('data-runtime-id');
  end;
  LRow := TJSHTMLElement(LFocus.closest('[data-nyx-item]'));

  if LText = 'keyboard-table' then
  begin
    LText := 'table.empty';

    if LRow <> nil then
    begin
      LText := 'table.' + LRow.getAttribute('data-nyx-item');

      if LFocus.hasAttribute('data-nyx-column') then
      begin
        LText := LText + '.' + LFocus.getAttribute('data-nyx-column');
      end;
    end;
  end;
  document.body.setAttribute('data-keyboard-focus', LText);
  document.body.setAttribute('data-keyboard-activated', IntToStr(GProbe.Activated));
  document.body.setAttribute('data-keyboard-searched', IntToStr(GProbe.Searched));
  document.body.setAttribute('data-keyboard-cleared', IntToStr(GProbe.Cleared));
  document.body.setAttribute('data-keyboard-decremented', IntToStr(GProbe.Decremented));
  document.body.setAttribute('data-keyboard-incremented', IntToStr(GProbe.Incremented));
  document.body.setAttribute('data-keyboard-removed', IntToStr(GProbe.Removed));
  document.body.setAttribute('data-keyboard-items', IntToStr(GView.Store.Snapshot.Count));
end;

function ObserveFocus(AEvent: TJSEvent): Boolean;
begin
  Publish;
  Result := True;
end;
{$endif}

procedure TKeyboardProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  Require(AEvent.SourceID <> 'keyboard-review-disabled-actions',
    'A disabled compound must never publish an action');

  if AEvent.HasKeyboard and (AEvent.Keyboard.Key = nkDeleteKey) and
    GView.Selection.Focus.Defined then
  begin
    { Delete is a test consumer command, admitted through the ordinary typed
      store. The adapter must transfer owned focus during the same host event. }
    NyxEventResponse(AExecution).Consume;
    GView.Store.Apply([NyxRemove(GView.Selection.Focus)]);
    Inc(Removed);
  end
  else if AEvent.Name.Name = NyxSemantic(nseActivate).Name then
  begin
    Inc(Activated);
  end
  else if AEvent.Name.Name = NyxSemantic(nseSearch).Name then
  begin
    Inc(Searched);
  end
  else if AEvent.Name.Name = NyxSemantic(nseClear).Name then
  begin
    Inc(Cleared);
  end
  else if AEvent.Name.Name = NyxSemantic(nseDecrement).Name then
  begin
    Inc(Decremented);
  end
  else if AEvent.Name.Name = NyxSemantic(nseIncrement).Name then
  begin
    Inc(Incremented);
  end;
  {$ifdef PAS2JS}Publish;{$endif}
end;

procedure Prepare;
var
  LPage: TNyxNode;
  LTable: INyxTable;
  LAfter: INyxButton;
  LStore: INyxCollection;
  LKey: TNyxCollectionRef;
begin
  GDocument := BuildNyxDocument;
  LPage := GDocument.Find('keyboard-review');
  Require(LPage <> nil, 'Compile the keyboard review page authored through MCP');
  LKey := NyxCollection('keyboard-tasks');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema
    .Text(NyxTextField('caption'), '').Integer(NyxIntegerField('priority'), 1), [
    NyxCollectionItem(NyxItem(LKey, 'sketch')).WithValue(NyxTextField('caption'), 'A sketch'),
    NyxCollectionItem(NyxItem(LKey, 'build')).WithValue(NyxTextField('caption'), 'A build'),
    NyxCollectionItem(NyxItem(LKey, 'share')).WithValue(NyxTextField('caption'), 'A shared result')]);
  GDocument.Collections.Define(LStore.Snapshot);
  LTable := NewNyxTable('keyboard-table');
  LTable.Configure.AccessibleName('Keyboard tasks').Height(180).Done;
  LTable.Binds.Collection(NyxCollectionView(LKey)
    .Column(NyxTextField('caption'), 'Task', cmEditable)
    .Column(NyxIntegerField('priority'), 'Priority', cmEditable)
    .Selection(nsmMultiple)).Done;
  LPage.Add(LTable.Node);
  LAfter := NewNyxButton('keyboard-after');
  LAfter.Configure.Text('Continue after the table').Done;
  LPage.Add(LAfter.Node);
  GRenderer := TRenderer.Create;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  {$else}
  GHost := TForm.Create(nil);
  GHost.SetBounds(0, 0, 950, 950);
  {$endif}
  GRenderer.Render(GDocument, LPage, GHost);
  GView := GRenderer.CollectionView('keyboard-table');
  GProbe := TKeyboardProbe.Create;
  GOwner := GProbe;
  SetLength(GTokens, 7);
  GTokens[0] := GRenderer.Events.OnNamed(NyxCompoundEvents('keyboard-review-action'),
    NyxSemantic(nseActivate)).Subscribe(GOwner);
  GTokens[1] := GRenderer.Events.OnNamed(NyxCompoundEvents('keyboard-review-search'),
    NyxSemantic(nseSearch)).Subscribe(GOwner);
  GTokens[2] := GRenderer.Events.OnNamed(NyxCompoundEvents('keyboard-review-search'),
    NyxSemantic(nseClear)).Subscribe(GOwner);
  GTokens[3] := GRenderer.Events.OnNamed(NyxCompoundEvents('keyboard-review-stepper'),
    NyxSemantic(nseDecrement)).Subscribe(GOwner);
  GTokens[4] := GRenderer.Events.OnNamed(NyxCompoundEvents('keyboard-review-stepper'),
    NyxSemantic(nseIncrement)).Subscribe(GOwner);
  GTokens[5] := GRenderer.Events.On(NyxControlEvents('keyboard-table'),
    ntKeyDown).Subscribe(GOwner);
  GTokens[6] := GRenderer.Events.OnNamed(NyxCompoundEvents('keyboard-review-disabled-actions'),
    NyxSemantic(nsePrimary)).Subscribe(GOwner);
end;

{$ifdef PAS2JS}
procedure PublishPart(const AAttribute, AControl, APart: TNyxText);
begin
  document.body.setAttribute(AAttribute,
    GDocument.Find(AControl).Part(NyxPart(APart)).ID);
end;
{$else}
procedure NativeChecks;
var
  LControl: TWinControl;
  LButton: TNyxNode;
begin
  GHost.Show;
  Application.ProcessMessages;
  LButton := GDocument.Find('keyboard-review-action').Part(NyxPart('button'));
  LControl := TWinControl(GRenderer.ControlFor(LButton.ID));
  Require(LControl.CanFocus, 'Native compound action is reachable');
  LControl.SetFocus;
  Require(LControl.Focused, 'Native compound action owns focus');
  TButtonAccess(LControl).Click;
  Require(GProbe.Activated = 1, 'Native compound action invokes its semantic callback once');
  LButton := GDocument.Find('keyboard-review-disabled-actions').Part(NyxPart('primary'));
  LControl := TWinControl(GRenderer.ControlFor(LButton.ID));
  Require(not LControl.CanFocus and not LControl.Enabled, 'Disabled native compound is unreachable');
  TButtonAccess(LControl).Click;
  Require(GProbe.Activated = 1, 'Disabled native compound preserves callback state');
  LControl := TWinControl(GRenderer.InputFor('keyboard-review-readonly'));
  Require(LControl.CanFocus, 'Read-only native memo remains focusable');
  LControl.SetFocus;
  Require(LControl.Focused and TCustomMemo(LControl).ReadOnly,
    'Read-only native memo retains inspection and selection');
  WriteLn('PASS native MCP-authored compound focus, activation, disabled and read-only controls');
end;
{$endif}

var
  LIndex: Integer;
begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    Prepare;
    {$ifdef PAS2JS}
    PublishPart('data-keyboard-query', 'keyboard-review-search', 'query');
    PublishPart('data-keyboard-search', 'keyboard-review-search', 'search');
    PublishPart('data-keyboard-clear', 'keyboard-review-search', 'clear');
    PublishPart('data-keyboard-decrement', 'keyboard-review-stepper', 'decrement');
    PublishPart('data-keyboard-value', 'keyboard-review-stepper', 'value');
    PublishPart('data-keyboard-increment', 'keyboard-review-stepper', 'increment');
    PublishPart('data-keyboard-action', 'keyboard-review-action', 'button');
    GHost.addEventListener('focusin', @ObserveFocus);
    Publish;
    document.body.setAttribute('data-keyboard-result', 'ready');
    {$else}
    try
      NativeChecks;
    finally
      for LIndex := 0 to High(GTokens) do
      begin
        GTokens[LIndex] := nil;
      end;
      GRenderer.Free;
      GView := nil;
      GOwner := nil;
      GHost.Free;
      GDocument.Free;
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-keyboard-result', 'failed');
      document.body.setAttribute('data-keyboard-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
