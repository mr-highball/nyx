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


program nyx_typeahead_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Math, nyx.text, nyx.text.search, nyx.typeahead,
  nyx.types, nyx.model, nyx.controls, nyx.collections,
  nyx.collections.view, nyx.collections.mount, nyx.collections.selection,
  nyx.behavior, nyx.events, nyx.scheduler, nyx.generated.view,
  {$ifdef NYX_SAVED_TYPEAHEAD}
  nyx.codec, nyx.test.typeahead.policy,
  {$endif}
  {$ifdef PAS2JS}JS, Web, nyx.render.browser, nyx.test.keyboard.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, ComCtrls, LCLType,
  Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl;{$endif}

type
  { Owns its sample text; Find borrows this reader only inside a call. }
  TLabels = class
    Values: array of TNyxText;
    function Read(AIndex: Integer): TNyxText;
    { Virtual provider covers the complete Integer range without allocating it. }
    function ReadSparse(AIndex: Integer): TNyxText;
  end;
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TControl = TJSHTMLElement;
  TReviewDetails = class external name 'HTMLDetailsElement' (TJSHTMLElement)
    open: Boolean;
  end;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TControl = TWinControl;
  TControlAccess = class(TWinControl);
  {$endif}
  { Borrow renderer only while its owner is alive. The teardown observer proves
    that selection publication can disconnect the exact active key handler. }
  TSelectionProbe = class
    Renderer: TRenderer;
    Calls: Integer;
    Retire: Boolean;
    procedure Changed(const ABefore, AAfter: INyxCollectionSelection);
  end;
  TConsumeKey = class(TNyxEventCallback)
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  {$ifdef PAS2JS}
  GVisualDocument: TNyxDocument;
  GVisualRenderer: TRenderer;
  {$endif}

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Typeahead: ' + AMessage);
  end;
  Inc(GChecks);
end;

function TLabels.Read(AIndex: Integer): TNyxText;
begin
  Result := Values[AIndex];
end;

function TLabels.ReadSparse(AIndex: Integer): TNyxText;
begin
  Result := 'Alpha';

  if AIndex = 1 then
  begin
    Result := 'Beta';
  end;
end;

procedure TSelectionProbe.Changed(const ABefore, AAfter: INyxCollectionSelection);
begin
  Inc(Calls);

  if Retire then
  begin
    Renderer.Unmount;
  end;
end;

procedure TConsumeKey.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  NyxEventResponse(AExecution).Consume;
end;

procedure SearchChecks;
var
  LLabels: TLabels;
  LSearch, LOther: INyxTypeAhead;
  LFocus: Integer;
  LFailed: Boolean;
  LUndefined: TNyxTypeAheadOptions;
  LText: TNyxText;
begin
  LLabels := TLabels.Create;
  try
    SetLength(LLabels.Values, 5);
    LLabels.Values[0] := 'Seattle';
    LLabels.Values[1] := 'Salem';
    LLabels.Values[2] := 'San Diego';
    LLabels.Values[3] := 'Boston';
    LLabels.Values[4] := 'Baltimore';
    LSearch := NewNyxTypeAhead(NyxTypeAhead);
    LOther := NewNyxTypeAhead(NyxTypeAhead);
    LFocus := LSearch.Find('s', 0, 5, -1, LLabels.Read);
    Check(LFocus = 0, 'first character starts at the first visible item');
    LFocus := LSearch.Find('S', 10, 5, LFocus, LLabels.Read);
    Check(LFocus = 1, 'folded repeated letter cycles');
    LFocus := LSearch.Find('s', 20, 5, LFocus, LLabels.Read);
    Check(LFocus = 2, 'cycle reaches the next match');
    LFocus := LSearch.Find('s', 30, 5, LFocus, LLabels.Read);
    Check(LFocus = 0, 'cycle wraps');
    Check(LOther.Find('b', 35, 5, -1, LLabels.Read) = 3,
      'independent control buffer');
    LFocus := LSearch.Find('a', 40, 5, LFocus, LLabels.Read);
    Check(LFocus = 1, 'extended prefix starts at the current match');
    LFocus := LSearch.Find('n', 50, 5, LFocus, LLabels.Read);
    Check(LFocus = 2, 'rapid prefix narrows to San Diego');
    Check(LSearch.Find('b', 1050, 5, LFocus, LLabels.Read) = 3,
      'exact timeout starts a new search');
    Check(LSearch.Find('s', 1, 5, 3, LLabels.Read) = 0,
      'backwards monotonic clock resets');
    Check(LSearch.Find('b', 2, 5, 4, LLabels.Read) = 3,
      'external focus changes reset');
    LSearch.Reset;
    Check(LSearch.Find('z', 3, 5, 3, LLabels.Read) = -1, 'no-match is explicit');
    Check(LSearch.Find('b', 1003, 5, 3, LLabels.Read) = 4,
      'later single key starts after current focus');
    LSearch.Reset;
    Check(LSearch.Find('b', 0, High(Integer), High(Integer) - 1, LLabels.ReadSparse) = 1,
      'large virtual range wraps without overflowing start plus offset');
    LSearch := NewNyxTypeAhead(NyxTypeAhead.Match(ntmExact));
    Check(LSearch.Find('s', 0, 5, -1, LLabels.Read) = -1, 'exact policy preserves case');
    LSearch.Reset;
    Check(LSearch.Find('S', 1, 5, -1, LLabels.Read) = 0, 'exact matching still navigates');
    LSearch := NewNyxTypeAhead(NyxTypeAhead.Enabled(False));
    Check(LSearch.Find('S', 0, 5, -1, LLabels.Read) = -1, 'disabled policy refuses search');
    LSearch := NewNyxTypeAhead(NyxTypeAhead.WindowMilliseconds(10));
    Check(LSearch.Find('s', 0, 5, -1, LLabels.Read) = 0, 'custom policy starts');
    Check(LSearch.Find('b', 10, 5, 0, LLabels.Read) = 3, 'custom timeout applies');
    Check(LSearch.Find('s', 20, 0, -1, LLabels.Read) = -1, 'empty composite has no candidates');
    Check(not NyxTypeAheadCharacter('Enter'), 'named keys are not text');
    Check(not NyxTypeAheadCharacter(' '), 'Space remains the selection gesture');
    Check(not NyxTypeAheadCharacter(#9), 'control keys stay owned by navigation');
    Check(not NyxTypeAheadCharacter(''), 'missing text refuses');
    Check(NyxTypeAheadCharacter(NyxScalarText($10400)), 'supplementary scalar is one character');
    LUndefined := Default(TNyxTypeAheadOptions);
    LFailed := False;
    try
      LSearch := NewNyxTypeAhead(LUndefined);
    except
      on EArgumentException do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'undefined fluent policy refuses');
    LFailed := False;
    try
      LSearch := NewNyxTypeAhead(NyxTypeAhead.WindowMilliseconds(0));
    except
      on EArgumentException do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'zero window refuses');
    LFailed := False;
    try
      LFocus := LSearch.Find('s', NaN, 5, -1, LLabels.Read);
    except
      on EArgumentException do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'non-finite time refuses');
    Check(NyxFoldText(TNyxText('Straße')) = TNyxText('strasse'), 'full fold expands sharp S');
    Check(NyxFoldText(TNyxText('ÉCOLE')) = TNyxText('école'), 'accent remains while case folds');
    Check(NyxFoldText(TNyxText('Σςσ')) = TNyxText('σσσ'), 'Greek final sigma folds');
    Check(NyxFoldText(TNyxText('Ж')) = TNyxText('ж'), 'Cyrillic folds');
    Check(NyxFoldText(NyxScalarText($10400)) = NyxScalarText($10428),
      'supplementary Deseret folds identically');
    Check(NyxFoldText(NyxScalarText($FB03)) = 'ffi', 'three-scalar ligature expansion');
    Check(NyxStartsWithFolded('Straße', 'stras'), 'partial fold expansion matches');
    Check(not NyxStartsWithFolded('école', 'e'), 'search does not remove accents');
    Check(not NyxStartsWithFolded(TNyxText('e') + NyxScalarText($301), TNyxText('é')),
      'search does not normalize combining text');
    LText := NyxScalarText($10400) + TNyxText('ße');
    LLabels.Values[0] := LText;
    LSearch := NewNyxTypeAhead(NyxTypeAhead);
    Check(LSearch.Find(NyxScalarText($10428), 0, 5, -1, LLabels.Read) = 0,
      'engine searches supplementary label without ANSI conversion');
    Check(LLabels.Values[0] = LText, 'search retains exact authored text');
  finally
    LSearch := nil;
    LOther := nil;
    LLabels.Free;
  end;
end;

function Item(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(NyxCollection('destinations'), AID);
end;

{ Dispatch to real mounted target callbacks. Browser events are synthetic DOM
  events; native uses its canonical KeyDown then UTF8KeyPress hooks. This proves
  routing/consumption, not trusted device input or IME composition. }
function Press(AControl: TControl; const AText: TNyxText;
  AModifiers: TNyxKeyModifiers = []; AComposing: Boolean = False): Boolean;
var
  {$ifdef PAS2JS}
  LRow: TJSHTMLElement;
  LEvent: TJSKeyboardEvent;
  {$else}
  LKey: Word;
  LShift: TShiftState;
  LText: TUTF8Char;
  {$endif}
begin
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(AControl.querySelector('[data-nyx-item][tabindex="0"]'));

  if LRow = nil then
  begin
    LRow := AControl;
  end;
  LRow.focus;
  LEvent := NyxTestKeyboard(ntKeyDown, AText, AModifiers, False, AComposing);
  LRow.dispatchEvent(LEvent);
  Result := LEvent.defaultPrevented;
  {$else}
  LShift := [];

  if nmControl in AModifiers then
  begin
    Include(LShift, ssCtrl);
  end;

  if nmAlt in AModifiers then
  begin
    Include(LShift, ssAlt);
  end;

  if nmMeta in AModifiers then
  begin
    Include(LShift, ssMeta);
  end;

  if nmShift in AModifiers then
  begin
    Include(LShift, ssShift);
  end;
  LKey := VK_S;

  if AComposing then
  begin
    LKey := $E5;
  end
  else if AText = 'ArrowRight' then
  begin
    LKey := VK_RIGHT;
  end
  else if AText = 'ArrowLeft' then
  begin
    LKey := VK_LEFT;
  end;
  TControlAccess(AControl).OnKeyDown(AControl, LKey, LShift);
  Result := LKey = 0;

  if NyxTypeAheadCharacter(AText) then
  begin
    { Exercise a queued character even after consumed KeyDown; the adapter
      must prevent a separate widgetset typeahead default. }
    LText := TUTF8Char(AText);
    TControlAccess(AControl).OnUTF8KeyPress(AControl, LText);
    Result := Result or (LText = '');
  end;
  {$endif}
end;

procedure ControlChecks;
var
  LDocument: TNyxDocument;
  LRenderer: TRenderer;
  LList, LTree: TControl;
  LListView, LTreeView: INyxCollectionView;
  LListMount, LTreeMount: INyxCollectionMount;
  LProbe: TSelectionProbe;
  LBefore: INyxCollectionSelection;
  LToken: INyxEventSubscription;
  LCallback: INyxEventCallback;
  LFailed: Boolean;
  LVersion: Integer;
  LPrefix: TNyxText;
  {$ifdef PAS2JS}LHost: TJSHTMLElement;
  {$else}LHost: TForm; LBitmap: TBitmap; LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;{$endif}
begin
  LDocument := BuildNyxDocument;
  LRenderer := TRenderer.Create;
  LProbe := TSelectionProbe.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.Create(nil);
  LHost.Caption := 'Find your next destination';
  LHost.SetBounds(50, 50, 740, 700);
  LHost.Show;
  {$endif}
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LProbe.Renderer := LRenderer;
    LListView := LRenderer.CollectionView('destination-list');
    LTreeView := LRenderer.CollectionView('destination-tree');
    LListMount := LRenderer.CollectionMount('destination-list');
    LTreeMount := LRenderer.CollectionMount('destination-tree');
    {$ifdef PAS2JS}
    LList := LRenderer.ElementFor('destination-list');
    LTree := LRenderer.ElementFor('destination-tree');
    {$else}
    LList := TWinControl(LRenderer.ControlFor('destination-list'));
    LTree := TWinControl(LRenderer.ControlFor('destination-tree'));
    {$endif}
    Check((LListView.Snapshot.Count = 8) and (LTreeView.Snapshot.Count = 8),
      'unchanged MCP collection defaults reach both real controls');
    Check(not LListView.HasSelection and not LTreeView.HasSelection,
      'initial mount adds no selection');
    LVersion := LListView.Snapshot.Revision;
    LListMount.Select(Item('west'));
    Check(Press(LList, 's') and (LListView.Selection.Focus.ID = 'seattle'),
      'real list first key selects Seattle');
    Check(Press(LList, 'S', [nmShift]) and (LListView.Selection.Focus.ID = 'salem'),
      'real list repeats uppercase letter');
    Check(Press(LList, 's') and (LListView.Selection.Focus.ID = 'san-diego'),
      'real list repeat selects San Diego');
    Check(Press(LList, 's') and (LListView.Selection.Focus.ID = 'seattle'),
      'real list repeat wraps');
    Check(Press(LList, 'a') and (LListView.Selection.Focus.ID = 'salem'),
      'real list rapid prefix survives selection refresh');
    LFailed := False;
    try
      LListMount.ConfigureTypeAhead(Default(TNyxTypeAheadOptions));
    except
      on EArgumentException do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'undefined replacement policy refuses atomically');
    Check(Press(LList, 'n') and (LListView.Selection.Focus.ID = 'san-diego'),
      'rejected replacement retains the real list prefix');
    Check(LListView.Snapshot.Revision = LVersion, 'keyboard changes no collection data');
    LTreeMount.Select(Item('west'));
    Press(LTree, 'ArrowLeft');
    LBefore := LTreeView.Selection;
    Press(LTree, 's');
    Check(LTreeView.Selection.SameState(LBefore), 'collapsed descendants are excluded');
    Press(LTree, 'ArrowRight');
    Check(Press(LTree, 's') and (LTreeView.Selection.Focus.ID = 'seattle'),
      'expanded tree finds its visible child');
    Check(LListView.Selection.Focus.ID = 'san-diego', 'list/tree typing buffers are independent');
    LListMount.ConfigureTypeAhead(NyxTypeAhead);
    LListMount.Select(Item('west'));
    LCallback := TConsumeKey.Create;
    LToken := LRenderer.Events.OnBeforeKeyDown(NyxControlEvents('destination-list'))
      .Subscribe(LCallback);
    LBefore := LListView.Selection;
    Check(Press(LList, 'b') and LListView.Selection.SameState(LBefore),
      'canonical consumed key prevents typeahead');
    LToken.Cancel;
    LToken := nil;
    LCallback := nil;
    Check(Press(LList, 'b') and (LListView.Selection.Focus.ID = 'boston'),
      'cancelled callback restores default');
    LBefore := LListView.Selection;
    Press(LList, 's', [nmControl]);
    Check(LListView.Selection.SameState(LBefore), 'control shortcut does not search');
    Press(LList, 's', [nmAlt]);
    Check(LListView.Selection.SameState(LBefore), 'Alt text does not search');
    Press(LList, 's', [], True);
    Check(LListView.Selection.SameState(LBefore), 'composition text does not search');
    LListMount.ConfigureTypeAhead(NyxTypeAhead.Enabled(False));
    Press(LList, 's');
    Check(LListView.Selection.SameState(LBefore), 'disabled search suppresses target search default');
    LListMount.ConfigureTypeAhead(NyxTypeAhead.Match(ntmExact));
    LListMount.Select(Item('west'));
    Press(LList, 's');
    Check(LListView.Selection.Focus.ID = 'west', 'real exact policy refuses lowercase match');
    LListMount.ConfigureTypeAhead(NyxTypeAhead.Match(ntmExact));
    Check(Press(LList, 'S') and (LListView.Selection.Focus.ID = 'seattle'),
      'real exact policy matches authored case');
    LListMount.ConfigureTypeAhead(NyxTypeAhead);
    LRenderer.Root.Find('destination-list').Configure.ReadOnly(True).Done;
    LRenderer.Sync;
    LListMount.Select(Item('west'));
    Check(Press(LList, 'b') and (LListView.Selection.Focus.ID = 'boston'),
      'read-only data permits typeahead');
    LRenderer.Root.Find('destination-list').Configure.Enabled(False).Done;
    LRenderer.Sync;
    LBefore := LListView.Selection;
    Press(LList, 's');
    Check(LListView.Selection.SameState(LBefore), 'disabled control preserves selection');
    LRenderer.Root.Find('destination-list').Configure.Enabled(True).ReadOnly(False).Done;
    LRenderer.Sync;
    LListMount.ConfigureTypeAhead(NyxTypeAhead);
    LListMount.Select(Item('west'));
    Press(LList, 's');
    LListView.Store.Move(Item('boston'), 0);
    Check(Press(LList, 'b') and (LListView.Selection.Focus.ID = 'baltimore'),
      'dataset reorder resets buffered prefix');
    LListView.Store.Move(Item('boston'), 5);
    LListMount.ConfigureTypeAhead(NyxTypeAhead);
    LPrefix := NyxScalarText($10400);
    LListView.Store.Update(NyxCollectionItem(Item('seattle'))
      .WithValue(NyxTextField('title'), LPrefix + ' location'));
    LListMount.Select(Item('west'));
    Check(Press(LList, NyxScalarText($10428)) and
      (LListView.Selection.Focus.ID = 'seattle'), 'real Unicode supplementary search');
    Check(LListView.CellText(Item('seattle'), 0) = LPrefix + ' location',
      'target selection retains exact supplementary data');
    LListView.Store.Update(NyxCollectionItem(Item('seattle'))
      .WithValue(NyxTextField('title'), 'Seattle'));
    LListMount.Select(Item('west'));
    Check(Press(LList, 's') and (LListView.Selection.Focus.ID = 'seattle'),
      'restored English label resets the dataset prefix');
    {$ifdef PAS2JS}
    Check(document.activeElement = LList.querySelector('[data-nyx-item="seattle"]'),
      'matching browser row receives physical focus');
    document.body.setAttribute('data-viewport-width', IntToStr(window.innerWidth));
    {$else}
    Application.ProcessMessages;

    if ParamCount > 0 then
    begin
      LBitmap := TBitmap.Create;
      LImage := nil;
      LWriter := nil;
      try
        LBitmap.SetSize(LHost.ClientWidth, LHost.ClientHeight);
        LHost.PaintTo(LBitmap.Canvas, 0, 0);
        LImage := LBitmap.CreateIntfImage;
        LWriter := TFPWriterPNG.Create;
        LImage.SaveToFile(ParamStr(1), LWriter);
      finally
        LWriter.Free;
        LImage.Free;
        LBitmap.Free;
      end;
    end;
    {$endif}
    { Preserve the screenshot as an English review. Retiring callback uses the
      lower attachment observer port; canonical consumption was tested above. }
    LListMount.ObserveSelection(LProbe.Changed);
    LProbe.Retire := True;
    LListMount.ConfigureTypeAhead(NyxTypeAhead);
    Press(LList, 'b');
    Check((LProbe.Calls = 1) and not LListMount.Connected and not LTreeMount.Connected,
      'selection callback may unmount both controls without stale target access');
    LFailed := False;
    try
      LListMount.ConfigureTypeAhead(NyxTypeAhead);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'retained disconnected policy handle refuses');
  finally
    LToken := nil;
    LCallback := nil;
    LProbe.Renderer := nil;
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LListMount := nil;
    LTreeMount := nil;
    LListView := nil;
    LTreeView := nil;
    LProbe.Free;
    LDocument.Free;
  end;
end;

{$ifdef NYX_SAVED_TYPEAHEAD}
procedure SavedPolicyControlChecks;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LRenderer: TRenderer;
  LList: TControl;
  LTree: TControl;
  LListView: INyxCollectionView;
  LTreeView: INyxCollectionView;
  LListMount: INyxCollectionMount;
  LTreeMount: INyxCollectionMount;
  LOldMount: INyxCollectionMount;
  LBefore: TNyxText;
  LRejected: Boolean;
  {$ifdef PAS2JS}LHost: TJSHTMLElement;
  {$else}LHost: TForm;{$endif}

  procedure MountedControls;
  begin
    LListView := LRenderer.CollectionView('destination-list');
    LTreeView := LRenderer.CollectionView('destination-tree');
    LListMount := LRenderer.CollectionMount('destination-list');
    LTreeMount := LRenderer.CollectionMount('destination-tree');
    {$ifdef PAS2JS}
    LList := LRenderer.ElementFor('destination-list');
    LTree := LRenderer.ElementFor('destination-tree');
    {$else}
    LList := TWinControl(LRenderer.ControlFor('destination-list'));
    LTree := TWinControl(LRenderer.ControlFor('destination-tree'));
    {$endif}
  end;

begin
  { These are ordinary target controls using the same callback harness as the
    established runtime review. No mock mount or manually injected spec reader
    stands in for adapter initialization, retained refresh or disconnection. }
  LDocument := CreateNyxSavedTypeAheadFixture;
  LCandidate := nil;
  LRenderer := TRenderer.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.Create(nil);
  LHost.SetBounds(50, 50, 740, 700);
  LHost.Show;
  {$endif}
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    MountedControls;
    LListMount.Select(Item('west'));
    Press(LList, 's');
    Check(LListView.Selection.Focus.ID = 'west',
      'ordinary list begins with the saved disabled policy');
    LTreeMount.Select(Item('west'));
    Press(LTree, 'ArrowRight');
    Check(Press(LTree, 'S') and (LTreeView.Selection.Focus.ID = 'seattle'),
      'ordinary tree begins with saved exact-case search');
    LTreeMount.Select(Item('west'));
    Press(LTree, 's');
    Check(LTreeView.Selection.Focus.ID = 'west',
      'saved exact tree search refuses lowercase without changing selection');
    LListMount.ConfigureTypeAhead(NyxTypeAhead);
    Check(Press(LList, 's') and (LListView.Selection.Focus.ID = 'seattle'),
      'runtime override independently enables the mounted list');
    Check(not LListView.Spec.TypeAheadPolicy.IsEnabled and
      (LListView.Spec.TypeAheadPolicy.WindowMS = 700),
      'runtime override leaves the copied authored binding exact');
    LRejected := False;
    try
      LListMount.ConfigureTypeAhead(Default(TNyxTypeAheadOptions));
    except
      on EArgumentException do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and Press(LList, 'a') and
      (LListView.Selection.Focus.ID = 'salem'),
      'invalid runtime replacement retains the mounted override and prefix');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'keys, selections and runtime policies do not edit saved defaults');
    LCandidate := LDocument.Clone;
    LCandidate.Find('review-heading').Configure.Text('Same policy, refreshed heading').Done;
    Check(LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'unrelated chrome refresh accepts an unchanged binding');
    Check(LListMount.Connected and
      (LRenderer.CollectionMount('destination-list') = LListMount),
      'ordinary refresh retains the mount and its runtime engine');
    LListMount.Select(Item('west'));
    Check(Press(LList, 's') and (LListView.Selection.Focus.ID = 'seattle'),
      'retained refresh preserves the local override');
    LOldMount := LListMount;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    MountedControls;
    Check(not LOldMount.Connected and LListMount.Connected,
      'remount disconnects old handles and owns a fresh engine');
    LListMount.Select(Item('west'));
    Press(LList, 's');
    Check(LListView.Selection.Focus.ID = 'west',
      'remount restores the saved disabled policy');
    LCandidate.Find('destination-list').SetCollectionView(
      LCandidate.Find('destination-list').CollectionView.TypeAhead(NyxTypeAhead));
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'a changed saved policy refuses retained refresh');
    Check((LRenderer.CollectionMount('destination-list') = LListMount) and
      LListMount.Connected, 'refused refresh leaves the accepted mount alive');
    Press(LList, 's');
    Check(LListView.Selection.Focus.ID = 'west',
      'refused refresh leaves the accepted search policy untouched');
    LRenderer.Render(LCandidate, LCandidate.Pages[0], LHost);
    MountedControls;
    LListMount.Select(Item('west'));
    Check(Press(LList, 's') and (LListView.Selection.Focus.ID = 'seattle'),
      'accepted full render applies the newly saved policy');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'all control journeys retain the original saved document');
  finally
    LRenderer.Free;
    LOldMount := nil;
    LListMount := nil;
    LTreeMount := nil;
    LListView := nil;
    LTreeView := nil;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LCandidate.Free;
    LDocument.Free;
  end;
end;
{$endif}

{$ifdef PAS2JS}
procedure ShowReview;
var
  LResult: TJSHTMLElement;
begin
  { Assertions deliberately retire their controls. A fresh unchanged English
    companion supplies a useful visual capture rather than a blank teardown. }
  GVisualDocument := BuildNyxDocument;
  GVisualRenderer := TRenderer.Create;
  GVisualRenderer.Render(GVisualDocument, GVisualDocument.Pages[0],
    TJSHTMLElement(document.body));
  GVisualRenderer.CollectionMount('destination-list').Select(Item('seattle'));
  LResult := TJSHTMLElement(document.createElement('p'));
  LResult.textContent := 'PASS ' + IntToStr(GChecks) + ' typeahead checks';
  document.body.appendChild(LResult);
end;

function RetireReview(AEvent: TEventListenerEvent): Boolean;
begin
  GVisualRenderer.Free;
  GVisualRenderer := nil;
  GVisualDocument.Free;
  GVisualDocument := nil;
  Result := True;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    SearchChecks;
    ControlChecks;
    {$ifdef NYX_SAVED_TYPEAHEAD}
    {$ifdef PAS2JS}
    Inc(GChecks, RunNyxTypeAheadPolicyTests);
    {$else}
    Inc(GChecks, RunNyxTypeAheadPolicyTests(TNyxText(ParamStr(2))));
    {$endif}
    SavedPolicyControlChecks;
    {$endif}
    {$ifdef PAS2JS}
    ShowReview;
    window.addEventListener('pagehide', @RetireReview);
    document.body.setAttribute('data-typeahead-tests', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' typeahead checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-typeahead-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end
    {$ifdef PAS2JS}
    else
    begin
      document.body.textContent := 'FAIL browser host: ' + String(TJSObject(JSExceptValue)['stack']);
      document.body.setAttribute('data-typeahead-tests', 'failed');
    end
    {$endif};
  end;
end.
