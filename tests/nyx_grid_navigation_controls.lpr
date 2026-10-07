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

program nyx_grid_navigation_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.state, nyx.collections, nyx.collections.view, nyx.collections.selection,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, Grids, LCLType,
    LMessages, Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl;{$endif}

type
  {$ifdef PAS2JS}
  TGridHostKey = class external name 'KeyboardEvent' (TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TGridControlAccess = class(TWinControl);
  {$endif}
  { A runtime observer borrows the renderer only for this synchronous fixture.
    It never retains its owner, and the pointer is cleared before destruction. }
  TGridProbe = class(TNyxEventCallback)
    Consume: Boolean;
    Retire: Boolean;
    Calls: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GDocument: TNyxDocument;
  GView: INyxCollectionView;
  GChecks: Integer;
  { Exact admitted defaults remain independent of all runtime keyboard edits. }
  GAuthored: TNyxText;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GTable: TJSHTMLElement;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  GTable: TStringGrid;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Grid navigation: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TGridProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);

  if Consume then
  begin
    NyxEventResponse(AExecution).Consume;
  end;

  if Retire then
  begin
    Check(AEvent.HasCollectionSelection, 'retirement observes the admitted owned selection');
    GRenderer.Unmount;
  end;
end;

function Item(AIndex: Integer): TNyxItemRef;
const
  CIDs: array[0..3] of TNyxText = ('plan', 'design', 'build', 'share');
begin
  Result := NyxItem(NyxCollection('work-items'), CIDs[AIndex]);
end;

{ Use the exact MCP-exported ordinary document. No fixture recreates its layout,
  schema, rows, binding or titles through a second authoring path. }
procedure Mount;
begin
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  GView := GRenderer.CollectionView('work-table');
  {$ifdef PAS2JS}
  GTable := GRenderer.ElementFor('work-table');
  TJSHTMLElement(GTable.querySelector('[tabindex="0"]')).focus;
  {$else}
  GTable := TStringGrid(GRenderer.ControlFor('work-table'));
  GTable.SetFocus;
  {$endif}
end;

function Editing: Boolean;
begin
  {$ifdef PAS2JS}
  Result := LowerCase(TJSHTMLElement(document.activeElement).tagName) = 'input';
  {$else}
  Result := GTable.EditorMode;
  {$endif}
end;

procedure Position(ARow, AColumn: Integer; const AReason: TNyxText);
{$ifdef PAS2JS}
var
  LCell: TJSHTMLElement;
  LRow: TJSHTMLElement;
begin
  LCell := TJSHTMLElement(TJSHTMLElement(document.activeElement).closest('[role=gridcell]'));
  Check((LCell <> nil) and GTable.contains(LCell), AReason + ' / exact owned data cell');
  LRow := TJSHTMLElement(LCell.closest('[data-nyx-item]'));
  Check((LRow.getAttribute('data-nyx-item') = Item(ARow).ID) and
    (StrToInt(LCell.getAttribute('data-nyx-column')) = AColumn), AReason);
end;
{$else}
begin
  Check((GTable.Row = ARow + 1) and (GTable.Col = AColumn), AReason +
    ' / actual row=' + IntToStr(GTable.Row - 1) + ' column=' + IntToStr(GTable.Col));
end;
{$endif}

{ Invoke real target control paths. The separate anonymous-pipe observer also
  delivers Chromium input to this retained consumer; synthetic DOM dispatch by
  itself does not establish the browser's trusted default Tab behavior. }
procedure Press(AKey: TNyxKey; AControl: Boolean = False; AShift: Boolean = False);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
  LKey: String;
  LElement: TJSHTMLElement;
begin
  case AKey of
    nkLeftKey:
      begin
        LKey := 'ArrowLeft';
      end;
    nkRightKey:
      begin
        LKey := 'ArrowRight';
      end;
    nkUpKey:
      begin
        LKey := 'ArrowUp';
      end;
    nkDownKey:
      begin
        LKey := 'ArrowDown';
      end;
    nkHomeKey:
      begin
        LKey := 'Home';
      end;
    nkEndKey:
      begin
        LKey := 'End';
      end;
    nkEnterKey:
      begin
        LKey := 'Enter';
      end;
    nkEscapeKey:
      begin
        LKey := 'Escape';
      end;
    nkF2Key:
      begin
        LKey := 'F2';
      end;
    else
      begin
        raise Exception.Create('Unsupported grid fixture key');
      end;
  end;
  LOptions := TJSObject.new;
  LOptions['key'] := LKey;
  LOptions['ctrlKey'] := AControl;
  LOptions['shiftKey'] := AShift;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LElement := TJSHTMLElement(document.activeElement);
  LElement.dispatchEvent(TGridHostKey.new('keydown', LOptions));

  if GRenderer.Root <> nil then
  begin
    LElement.dispatchEvent(TGridHostKey.new('keyup', LOptions));
  end;
end;
{$else}
var
  LKey: Word;
  LShift: TShiftState;
  LControl: TWinControl;
begin
  case AKey of
    nkLeftKey:
      begin
        LKey := VK_LEFT;
      end;
    nkRightKey:
      begin
        LKey := VK_RIGHT;
      end;
    nkUpKey:
      begin
        LKey := VK_UP;
      end;
    nkDownKey:
      begin
        LKey := VK_DOWN;
      end;
    nkHomeKey:
      begin
        LKey := VK_HOME;
      end;
    nkEndKey:
      begin
        LKey := VK_END;
      end;
    nkEnterKey:
      begin
        LKey := VK_RETURN;
      end;
    nkEscapeKey:
      begin
        LKey := VK_ESCAPE;
      end;
    nkF2Key:
      begin
        LKey := VK_F2;
      end;
    else
      begin
        raise Exception.Create('Unsupported grid fixture key');
      end;
  end;
  LShift := [];

  if AControl then
  begin
    Include(LShift, ssCtrl);
  end;

  if AShift then
  begin
    Include(LShift, ssShift);
  end;
  LControl := GTable;

  if Editing then
  begin
    LControl := TWinControl(GTable.Editor);
  end;

  if LShift = [] then
  begin
    { Actual unmodified LCL messages enter the renderer's physical frame. A
      direct virtual call bypasses that installed window hook and cannot prove
      deferred retirement while TStringGrid continues its own key processing. }
    { LCL invokes OnKeyDown in its CN phase before the widgetset default. LM is
      the later remaining-key phase and does not invoke that canonical hook. }
    LControl.Perform(CN_KEYDOWN, LKey, 0);
  end
  else
  begin
    { Modified control paths take an explicit shift set; hardware modifier
      state is deliberately not changed by this owned qualification harness. }
    TGridControlAccess(LControl).KeyDown(LKey, LShift);
  end;
end;
{$endif}

procedure Draft(const AValue: TNyxText);
begin
  Check(Editing, 'draft belongs to an actually active cell editor');
  {$ifdef PAS2JS}
  TJSHTMLInputElement(document.activeElement).value := AValue;
  {$else}
  TCustomEdit(GTable.Editor).Text := String(AValue);
  {$endif}
end;

procedure Run;
var
  LBefore: TNyxText;
  LSelection: INyxCollectionSelection;
  LProbe: TGridProbe;
  LCallback: INyxEventCallback;
  LToken: INyxEventSubscription;
  LRevision: Integer;
  {$ifndef PAS2JS}
  LColumnWidth: Integer;
  {$endif}
begin
  LBefore := TNyxCodec.Encode(GDocument);
  Check((GView.Spec.Count = 3) and (GView.Snapshot.Count = 4),
    'exact English semantic table reaches the ordinary target');
  {$ifndef PAS2JS}
  Check(GTable.ColWidths[0] >= GTable.Canvas.TextWidth('Build something useful'),
    'initial native columns expose the complete ordinary English caption');
  {$endif}
  GView.Select(Item(0));
  GView.Select(Item(1), nsaToggle);
  LSelection := GView.Selection;
  Press(nkHomeKey, True);
  Position(0, 0, 'Control+Home addresses the first data cell');
  Press(nkRightKey);
  Position(0, 1, 'Right moves to the next column');
  Check(GView.Selection.Count = LSelection.Count, 'horizontal navigation retains row membership');
  Press(nkRightKey);
  Position(0, 2, 'Right reaches a read-only column');
  Press(nkEnterKey);
  Check(not Editing, 'read-only column does not enter another column editor');
  Press(nkHomeKey);
  Position(0, 0, 'Home addresses this row start');
  Press(nkEndKey);
  Position(0, 2, 'End addresses this row end');
  Press(nkEndKey, True);
  Position(3, 2, 'Control+End reaches the final data cell');
  Press(nkRightKey);
  Position(3, 2, 'Right does not wrap across the grid edge');
  Press(nkHomeKey, True);
  Press(nkRightKey);
  Press(nkDownKey);
  Position(1, 1, 'Down retains the focused column');
  Press(nkUpKey);
  Position(0, 1, 'Up retains the focused column');
  Press(nkUpKey);
  Position(0, 1, 'Up stays inside the first row');
  Press(nkEnterKey);
  Check(Editing, 'Enter opens this numeric cell, not the first text cell');
  Position(0, 1, 'active numeric editor belongs to the exact focused cell');
  LRevision := GView.Store.Snapshot.Revision;
  {$ifndef PAS2JS}
  LColumnWidth := GTable.ColWidths[0] + 40;
  GTable.ColWidths[0] := LColumnWidth;
  {$endif}
  Draft('99');
  GView.Store.Apply([NyxUpdate(NyxCollectionItem(Item(1))
    .WithValue(NyxTextField('task'), 'Review the experience'))]);
  Check(Editing, 'unrelated publication retains the active editor');
  {$ifndef PAS2JS}
  Check(GTable.ColWidths[0] = LColumnWidth,
    'unrelated publication retains the user column allocation');
  {$endif}
  Press(nkEscapeKey);
  Position(0, 1, 'Escape returns to the same data cell');
  Check(not Editing and (GView.Store.Snapshot.Revision = LRevision + 1) and
    (GView.CellText(Item(0), 1) = '1'), 'Escape discards only the uncommitted numeric draft');
  Press(nkF2Key);
  Check(Editing, 'F2 enters the current numeric cell');
  Press(nkF2Key);
  Check(not Editing, 'F2 restores grid navigation');
  Position(0, 1, 'F2 return retains the exact column');
  LProbe := TGridProbe.Create;
  LCallback := LProbe;
  LProbe.Consume := True;
  LToken := GRenderer.Events.OnBeforeKeyDown(NyxControlEvents('work-table')).Subscribe(LCallback);
  try
    Press(nkRightKey);
    Position(0, 1, 'a consumed canonical key suppresses default cell movement');
  finally
    LToken.Cancel;
    LToken := nil;
  end;
  LProbe.Consume := False;
  LProbe.Retire := True;
  LToken := GRenderer.Events.OnSelectionChange(NyxControlEvents('work-table')).Subscribe(LCallback);
  try
    Press(nkDownKey);
    Check((GRenderer.Root = nil) and (LProbe.Calls = 2),
      'selection callback can retire the real grid without a post-publication host access');
  finally
    LToken.Cancel;
    LToken := nil;
    LCallback := nil;
  end;
  Check(TNyxCodec.Encode(GDocument) = LBefore, 'runtime navigation/editing preserves exact authored defaults');
  Mount;
  {$ifdef PAS2JS}
  Check(GTable.querySelectorAll('[tabindex="0"]').length = 1, 'grid has one data-cell Tab stop');
  Check(GTable.querySelector('[role=row][tabindex="0"]') = nil, 'rows add no competing Tab entry');
  Check((GTable.getAttribute('aria-rowcount') = '5') and
    (GTable.getAttribute('aria-colcount') = '3'), 'grid dimensions include its accessible header');
  {$endif}
end;

{$ifdef PAS2JS}
{ Bounded physical observations for the real-host input driver. This reports the
  DOM focus already established by the product; it authors no design state. }
function ObserveFocus(AEvent: TEventListenerEvent): Boolean;
var
  LCell: TJSHTMLElement;
  LElement: TJSHTMLElement;
  LControl: TJSHTMLElement;
begin
  Result := True;
  LElement := TJSHTMLElement(AEvent.target);
  LControl := TJSHTMLElement(LElement.closest('[data-node]'));

  if LControl <> nil then
  begin
    document.body.setAttribute('data-grid-control', LControl.getAttribute('data-node'));
  end;
  document.body.setAttribute('data-grid-source-stable',
    LowerCase(BoolToStr(TNyxCodec.Encode(GDocument) = GAuthored, True)));
  LCell := TJSHTMLElement(LElement.closest('[role=gridcell]'));

  if LCell <> nil then
  begin
    document.body.setAttribute('data-grid-row', LCell.parentElement.getAttribute('data-nyx-item'));
    document.body.setAttribute('data-grid-column', LCell.getAttribute('data-nyx-column'));
    document.body.setAttribute('data-grid-mode', LowerCase(LElement.tagName));
  end
  else
  begin
    document.body.setAttribute('data-grid-mode', LowerCase(LElement.tagName));
  end;
end;
{$endif}

{$ifndef PAS2JS}
{ Capture the real native application after its shared journey. The optional
  filename is an explicit evidence output, never an application design path. }
procedure CaptureNative;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    GHost.Repaint;
    Application.ProcessMessages;
    LBitmap.SetSize(GHost.Width, GHost.Height);
    GHost.PaintTo(LBitmap.Canvas, 0, 0);
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

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    GDocument := BuildNyxDocument;
    GAuthored := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(GHost);
    GRenderer := TNyxBrowserRenderer.Create;
    {$else}
    GHost := TForm.Create(nil);
    GHost.SetBounds(0, 0, 900, 500);
    GHost.Show;
    GRenderer := TNyxLCLRenderer.Create;
    {$endif}
    Mount;
    Run;
    {$ifdef PAS2JS}
    GHost.addEventListener('focusin', @ObserveFocus);
    document.body.setAttribute('data-grid-checks', IntToStr(GChecks));
    document.body.setAttribute('data-grid-tests', 'passed');
    TJSHTMLElement(GTable.querySelector('[tabindex="0"]')).blur;
    TJSHTMLElement(GTable.querySelector('[tabindex="0"]')).focus;
    {$else}
    CaptureNative;
    WriteLn('PASS ', GChecks, ' ordinary generated native grid checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-grid-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  GView := nil;
  GRenderer.Free;
  GHost.Free;
  GDocument.Free;
  {$endif}
end.
