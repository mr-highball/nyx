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
program nyx_table_workbench_application_controls;

{$mode delphi}{$H+}{$codepage utf8}

{ Compile beside Studio's generated Service monitor. Explicit optional journeys
  require its MCP-authored context callback and custom menu action respectively.
  Two applications
  borrow that one document and own separate runtime stores and table controllers.
  The real LCL clipboard adapter executes against an explicit recording target;
  this test writes no OS clipboard. Widget messages exercise native routing but
  do not establish hardware input, displayed pixels or assistive-device quality. }
uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Grids, Windows, Types,
  nyx.text, nyx.bytes, nyx.model, nyx.codec, nyx.scheduler, nyx.application.lcl,
  nyx.state, nyx.types, nyx.menu,
  nyx.editing.lcl, nyx.collections.clipboard.actions,
  nyx.clipboard, nyx.clipboard.lcl, nyx.collections, nyx.collections.query,
  nyx.collections.mount, nyx.collections.arrangement,
  nyx.collections.selection,
  nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.range.commands, nyx.collections.cell.actions,
  nyx.collections.workbench.runtime, nyx.menu.lcl, nyx.generated.view;

type
  { Explicit fixture admission distinguishes the original saved application
    from its semantically authored context-label/callback and menu extensions. Missing
    extension content refuses that journey instead of silently testing the seed. }
  TServiceJourney = (sjOriginal, sjAuthoredContext, sjAuthoredActions);

  { Only the final OS publication boundary is replaced. Exact framed bytes are
    decoded through the real adapter's codec before inline completion. This
    target owns copied text/counters only, with no callback, mount or document. }
  TRecordedClipboard = class(TInterfacedObject, INyxLCLClipboardTextTarget)
  public
    Writes: Integer;
    Text: TNyxText;
    function WriteText(const AExactPacket: TNyxBytes;
      const APlatformUTF8: TNyxBytes; AIncludePlain: Boolean;
      out AError: TNyxText): TNyxClipboardWriteOutcome;
  end;

const
  CTable: TNyxText = 'service-table';
  CRange: TNyxText = 'name' + #9 + 'notes' + #10 +
    'Request gateway' + #9 +
    'Accepts incoming requests and routes them to healthy workers.' + #10 +
    'Search index worker' + #9 +
    'Waiting for new documents. Edit this note to try draft handling.';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function TRecordedClipboard.WriteText(const AExactPacket: TNyxBytes;
  const APlatformUTF8: TNyxBytes; AIncludePlain: Boolean;
  out AError: TNyxText): TNyxClipboardWriteOutcome;
begin
  Text := DecodeNyxLCLClipboardPacket(AExactPacket);
  Inc(Writes);
  AError := '';
  Result := ncwoWritten;
end;

procedure Pump;
begin
  Application.ProcessMessages;
  { Match the ordinary LCL loop: presentation deferred past an input frame is
    delivered during idle, without refreshing rows or extending action budgets. }
  Application.Idle(False);
  CheckSynchronize(1);
end;

procedure AwaitAction(const AActions: INyxCollectionCellActions);
var
  LStarted: QWord;
  LExecution: INyxExecution;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LExecution := AActions.LastExecution;

    if (LExecution <> nil) and not (LExecution.Status in [nesPending, nesRunning]) then
    begin
      Break;
    end;
  until GetTickCount64 - LStarted > 1000;
  Check((LExecution <> nil) and (LExecution.Status = nesSucceeded),
    'The saved application action completes within the existing one-second budget');
end;

{ Saved start/end pins can put a field into a child grid. Resolve the displayed
  header rather than interpreting an outer spacer as a data-column coordinate. }
function FieldGrid(ARoot: TStringGrid; const ATitle: TNyxText;
  out AColumn: Integer): TStringGrid;
var
  LChild: Integer;
  LCandidate: TStringGrid;

  function ContainsField(AGrid: TStringGrid): Boolean;
  var
    LColumn: Integer;
  begin
    Result := False;
    for LColumn := AGrid.FixedCols to AGrid.ColCount - 1 do
    begin

      if Pos(ATitle, TNyxText(AGrid.Cells[LColumn, 0])) = 1 then
      begin
        AColumn := LColumn;
        Exit(True);
      end;
    end;
  end;

begin
  AColumn := -1;

  if ContainsField(ARoot) then
  begin
    Exit(ARoot);
  end;
  for LChild := 0 to ARoot.ControlCount - 1 do
  begin

    if (ARoot.Controls[LChild] is TStringGrid) and ARoot.Controls[LChild].Visible then
    begin
      LCandidate := TStringGrid(ARoot.Controls[LChild]);

      if ContainsField(LCandidate) then
      begin
        Exit(LCandidate);
      end;
    end;
  end;
  Result := nil;
end;

procedure Journey(AWidth: Integer; AJourney: TServiceJourney);
var
  LDocument: TNyxDocument;
  LApp: TNyxLCLApplication;
  LPeerApp: TNyxLCLApplication;
  LTarget: INyxLCLClipboardTextTarget;
  LRecorded: TRecordedClipboard;
  LMount: INyxCollectionMount;
  LPeer: INyxCollectionMount;
  LRanges: INyxCollectionCellRangeMount;
  LPeerRanges: INyxCollectionCellRangeMount;
  LIntent: INyxCollectionCellRangeIntent;
  LPeerIntent: INyxCollectionCellRangeIntent;
  LCells: INyxCollectionCellActionsMount;
  LFeedback: INyxCollectionClipboardFeedbackMount;
  LActions: INyxCollectionCellActions;
  LMenu: INyxLCLMenu;
  LCustomMenu: INyxMenu;
  LCustomNative: INyxLCLMenu;
  LSource: INyxCollectionSnapshot;
  LPeerSource: INyxCollectionSnapshot;
  LRows: TNyxCollectionSelectionSnapshot;
  LPeerRows: TNyxCollectionSelectionSnapshot;
  LAuthored: TNyxText;
  LGrid: TStringGrid;
  LFace: TStringGrid;
  LStatus: TMemo;
  LColumn: Integer;
  LSavedKeys: TKeyboardState;
  LCopyKeys: TKeyboardState;
  LBounds: Types.TRect;
  LPoint: Types.TPoint;
  LWrites: Integer;

  procedure SelectRectangle;
  begin
    Check(LRanges.SelectCells(
      NyxCollectionCellIdentity(NyxItem(NyxCollection('services'), 'gateway'),
        NyxColumnIdentity(NyxTextField('name'))),
      NyxCollectionCellIdentity(NyxItem(NyxCollection('services'), 'indexer'),
        NyxColumnIdentity(NyxTextField('notes')))),
      'The unchanged saved policy admits the exact two-by-two rectangle');
    LIntent := LRanges.CellRange;
    Check(LIntent.Current and (LIntent.Capture.Payload.Text = CRange),
      'The accepted RAW range follows saved headers, LF and exact visible fields');
  end;

  procedure ContextAtNotes;
  begin
    LBounds := LFace.CellRect(LColumn, 1);
    LPoint := LFace.ClientToScreen(Types.Point(LBounds.Left + 4, LBounds.Top + 4));
    SendMessage(LFace.Handle, WM_CONTEXTMENU, LFace.Handle,
      MakeLParam(LPoint.X, LPoint.Y));
    AwaitAction(LActions);
    Check((LActions.Menu <> nil) and LActions.Menu.IsOpen and
      Supports(LActions.Menu, INyxLCLMenu, LMenu),
      'The actual saved table opens its native managed cell menu');
  end;

  procedure MarkCapturedService;
  begin
    TButton(LApp.View.FocusFor('service-actions-button')).Click;
    Pump;
    LCustomMenu := LApp.Menus.Menu(NyxControl('service-actions-button'));
    Check((LCustomMenu <> nil) and LCustomMenu.IsOpen and
      Supports(LCustomMenu, INyxLCLMenu, LCustomNative),
      'The real authored application button opens its saved custom menu');
    TButton(LCustomNative.Presentation.Renderer.FocusFor(
      LCustomMenu.Content.Part(NyxPart('mark-review')).ID)).Click;
    Pump;
    Check(not LCustomMenu.IsOpen, 'The ordinary managed menu closes before its authored command completes');
    LCustomNative := nil;
    LCustomMenu := nil;
  end;

begin
  LDocument := BuildNyxDocument;
  LApp := nil;
  LPeerApp := nil;
  LRecorded := TRecordedClipboard.Create;
  LTarget := LRecorded;
  try
    LAuthored := TNyxCodec.Encode(LDocument);
    Check((LDocument.Title = 'Service monitor') and (LDocument.Find(CTable) <> nil),
      'Consume the same generated Service monitor without another authored fixture');
    LApp := TNyxLCLApplication.Create;
    LApp.ConfigureTableWorkbench(NewNyxTableWorkbenchServices(
      NewNyxLCLClipboardWriteHost(LTarget)));
    LApp.Mount(LDocument);
    LApp.Window.ClientWidth := AWidth;
    LApp.Window.ClientHeight := 900;
    LApp.Window.Show;
    LPeerApp := TNyxLCLApplication.Create;
    LPeerApp.Mount(LDocument);
    LPeerApp.Window.ClientWidth := AWidth;
    LPeerApp.Window.ClientHeight := 900;
    LPeerApp.Window.Show;
    LApp.Window.BringToFront;
    Pump;
    LMount := LApp.View.CollectionMount(CTable);
    LPeer := LPeerApp.View.CollectionMount(CTable);
    Check(Supports(LMount, INyxCollectionCellRangeMount, LRanges) and
      Supports(LPeer, INyxCollectionCellRangeMount, LPeerRanges) and
      Supports(LMount, INyxCollectionCellActionsMount, LCells),
      'Both saved application instances expose independent range/action owners');
    LActions := LCells.CellActions;
    LSource := LMount.View.Store.Snapshot;
    LPeerSource := LPeer.View.Store.Snapshot;
    { Native realization may select its first row. Preserve each actual mounted
      baseline instead of assuming browser navigation's initially empty choice. }
    LRows := LMount.View.Selection.Snapshot;
    LPeerRows := LPeer.View.Selection.Snapshot;
    Check((LMount.View.Store <> LPeer.View.Store) and (LSource.Count = 4) and
      (LPeerSource.Count = 4), 'Applications own separate stores for the same authored defaults');
    Check(LPeerRanges.SelectCells(
      NyxCollectionCellIdentity(NyxItem(NyxCollection('services'), 'reports'),
        NyxColumnIdentity(NyxTextField('status'))),
      NyxCollectionCellIdentity(NyxItem(NyxCollection('services'), 'reports'),
        NyxColumnIdentity(NyxTextField('status')))),
      'A peer retains its own deliberately different single-cell range');
    LPeerIntent := LPeerRanges.CellRange;
    SelectRectangle;
    Check((LPeerRanges.CellRange = LPeerIntent) and LPeerIntent.Current and
      LMount.View.Selection.Snapshot.SameState(LRows),
      'Cell range selection preserves ordinary row membership and the independent peer');
    LGrid := TStringGrid(LApp.View.ControlFor(CTable));
    LFace := FieldGrid(LGrid, 'Notes', LColumn);
    Check((LFace <> nil) and (LColumn >= 0), 'Resolve actual Notes input beyond any pinned spacer');
    LFace.Col := LColumn;
    LFace.Row := 1;
    LFace.SetFocus;
    Pump;

    if AJourney in [sjAuthoredContext, sjAuthoredActions] then
    begin
      Check((LDocument.Find('service-context-summary') <> nil) and
        (LApp.State.GetValue(NyxTextState('service-context-summary')) =
          'Choose a service cell to inspect its context.'),
        'The explicit authored journey requires the MCP-created default and bound control');
      SendMessage(LFace.Handle, WM_CONTEXTMENU, LFace.Handle, LPARAM(-1));
      AwaitAction(LActions);
      Check((LApp.State.GetValue(NyxTextState('service-context-summary')) =
        'Service gateway / notes') and
        (TLabel(LApp.View.ControlFor('service-context-summary')).Caption =
          'Service gateway / notes'),
        'The authored application callback uses its exact keyboard cell to update a real bound label');
      Check((LPeerApp.State.GetValue(NyxTextState('service-context-summary')) =
        'Choose a service cell to inspect its context.') and
        (LMount.View.Store.Snapshot = LSource) and
        (LPeer.View.Store.Snapshot = LPeerSource) and
        (LRanges.CellRange = LIntent) and LIntent.Current,
        'Callback state delivery preserves independent peers, source values and original range input');
      Check((LActions.Menu <> nil) and LActions.Menu.IsOpen and
        (LActions.Snapshot.ItemAt(0).ID = 'gateway') and
        LActions.Snapshot.FieldAt(0).Same(NyxColumnIdentity(NyxTextField('notes'))),
        'Authored state synchronization retains the same original managed context menu');
      LActions.Close;
      Pump;
      LFace.SetFocus;
      Pump;
    end;
    Check(GetKeyboardState(LSavedKeys), 'Capture caller thread keyboard state before native Copy');
    LCopyKeys := LSavedKeys;
    LCopyKeys[VK_SHIFT] := 0;
    LCopyKeys[VK_MENU] := 0;
    LCopyKeys[VK_CONTROL] := $80;
    LCopyKeys[VK_LCONTROL] := $80;
    LWrites := LRecorded.Writes;
    Check(SetKeyboardState(LCopyKeys), 'Admit the explicit native Ctrl modifier');
    try
      SendMessage(LFace.Handle, WM_KEYDOWN, Ord('C'), 1);
      SendMessage(LFace.Handle, WM_KEYUP, Ord('C'), LPARAM($C0000001));
    finally
      Check(SetKeyboardState(LSavedKeys), 'Restore the exact caller thread keyboard state');
    end;
    Pump;
    Check((LRecorded.Writes = LWrites + 1) and (LRecorded.Text = CRange),
      'Native keyboard Copy writes the original range once without requiring event subscribers');
    Check(Supports(LMount, INyxCollectionClipboardFeedbackMount, LFeedback) and
      (LFeedback.ClipboardFeedback.Phase = nccpCompleted) and
      (LFeedback.ClipboardFeedback.Result.Outcome = ncwoWritten),
      'The same saved application retains the original typed keyboard completion');
    LStatus := TMemo(LGrid.FindComponent('NyxClipboardStatus'));
    Check(LStatus <> nil, 'The saved native table owns its readonly completion control');
    Check((LStatus <> nil) and LStatus.Visible and LStatus.ReadOnly and LStatus.TabStop and
      (LStatus.AccessibleName = 'Clipboard result') and
      (NyxLCLInputText(LStatus) = 'Copied to clipboard.') and
      (LStatus.Left >= 0) and (LStatus.Left + LStatus.Width <= LGrid.ClientWidth) and
      (Screen.ActiveControl = LFace),
      'The saved native table exposes readable bounded feedback without taking keyboard focus');
    ContextAtNotes;
    Check(LMenu.Presentation.Content.Node.Find(NyxCellActionsCopyRangeID) <> nil,
      'The real context menu exposes range transfer separately from accepted-cell Copy');
    LWrites := LRecorded.Writes;
    TButton(LMenu.Presentation.Renderer.FocusFor(NyxCellActionsCopyRangeID)).Click;
    AwaitAction(LActions);
    Check((LRecorded.Writes = LWrites + 1) and (LRecorded.Text = CRange) and
      (LActions.LastResult.Outcome = ncwoWritten),
      'The real managed native button transfers the same exact RAW rectangle once');
    LActions.Close;
    LMenu := nil;
    LMount.View.Select(NyxItem(NyxCollection('services'), 'gateway'));
    LMount.View.ConfigureQuery(NyxCollectionQuery.Where(
      NyxWhere(NyxTextField('status')).EqualTo('Idle')));
    Pump;
    Check((LRanges.CellRange = nil) and not LIntent.Current and
      (LMount.View.Snapshot.Count = 1) and
      LMount.View.Selection.Contains(NyxItem(NyxCollection('services'), 'gateway')),
      'Filtering withdraws the old range while keeping its hidden selected source identity');
    LFace := FieldGrid(LGrid, 'Notes', LColumn);
    ContextAtNotes;
    Check((LMenu.Presentation.Content.Node.Find(NyxCellActionsCopyRangeID) = nil) and
      (LMenu.Presentation.Content.Node.Find(NyxCellActionsCopyPageIDsID) = nil) and
      (LMenu.Presentation.Content.Node.Find(NyxCellActionsCopySourceIDsID) <> nil),
      'Filtered context distinguishes hidden source membership from the empty selected page cohort');
    LWrites := LRecorded.Writes;
    TButton(LMenu.Presentation.Renderer.FocusFor(NyxCellActionsCopySourceIDsID)).Click;
    AwaitAction(LActions);
    Check((LRecorded.Writes = LWrites + 1) and (LRecorded.Text = 'ID' + #10 + 'gateway'),
      'Copy selected source IDs keeps the hidden original record rather than the new visible row');
    Check((LPeerRanges.CellRange = LPeerIntent) and LPeerIntent.Current and
      (LPeer.View.Snapshot.Count = 4) and LPeer.View.Selection.Snapshot.SameState(LPeerRows) and
      (LPeer.View.Store.Snapshot = LPeerSource) and (LMount.View.Store.Snapshot = LSource) and
      (TNyxCodec.Encode(LDocument) = LAuthored),
      'Selection, filtering and transfers preserve independent peer input, exact stores and authored source');
    LActions.Close;
    LMenu := nil;

    if AJourney = sjAuthoredActions then
    begin
      Check((LDocument.Find('service-actions-button') <> nil) and
        LApp.State.GetValue(NyxBooleanState('service-context-ready')) and
        (LApp.State.GetValue(NyxTextState('service-context-id')) = 'indexer'),
        'The custom journey requires the exact last captured filtered service');
      MarkCapturedService;
      Check((LMount.View.Store.Snapshot.Revision = LSource.Revision + 1) and
        (LMount.View.Store.Snapshot.Item(NyxItem(NyxCollection('services'), 'indexer'))
          .GetValue(NyxTextField('status')) = 'Attention') and
        (LApp.State.GetValue(NyxTextState('service-context-summary')) =
          'Marked indexer for review.') and
        not LApp.State.GetValue(NyxBooleanState('service-context-ready')),
        'The authored menu changes only its captured service through one expected-revision runtime edit');
      Check((LPeer.View.Store.Snapshot = LPeerSource) and
        (LPeerApp.State.GetValue(NyxTextState('service-context-summary')) =
          'Choose a service cell to inspect its context.') and
        (TNyxCodec.Encode(LDocument) = LAuthored),
        'A useful custom action preserves the peer application and saved source defaults');
      LMount.View.ConfigureQuery(NyxCollectionQuery);
      Pump;
      LFace := FieldGrid(LGrid, 'Notes', LColumn);
      LFace.Col := LColumn;
      LFace.Row := 1;
      LFace.SetFocus;
      SendMessage(LFace.Handle, WM_CONTEXTMENU, LFace.Handle, LPARAM(-1));
      AwaitAction(LActions);
      Check(LApp.State.GetValue(NyxTextState('service-context-id')) = 'gateway',
        'A fresh keyboard context captures gateway independently of the previous action');
      LActions.Close;
      LMount.View.Store.Apply([NyxUpdate(NyxCollectionItem(
        NyxItem(NyxCollection('services'), 'reports'))
          .WithValue(NyxTextField('notes'), 'Another runtime edit arrived.'))],
        LMount.View.Store.Snapshot.Revision);
      Pump;
      MarkCapturedService;
      Check((LApp.State.GetValue(NyxTextState('service-context-summary')) =
        'Services changed. Choose a cell again before marking it for review.') and
        not LApp.State.GetValue(NyxBooleanState('service-context-ready')) and
        (LMount.View.Store.Snapshot.Revision = LSource.Revision + 2) and
        (LMount.View.Store.Snapshot.Item(NyxItem(NyxCollection('services'), 'gateway'))
          .GetValue(NyxTextField('status')) = 'Running') and
        (LPeer.View.Store.Snapshot = LPeerSource) and
        (TNyxCodec.Encode(LDocument) = LAuthored),
        'A stale captured revision refuses visibly without retargeting or a second runtime mutation');
    end;
    FreeAndNil(LApp);
    Pump;
    Check(not LMount.Connected and not LIntent.Current and LPeer.Connected and
      LPeerIntent.Current and (LPeerRanges.CellRange = LPeerIntent),
      'Retiring one application cannot retire its peer range or redirect old input');
  finally
    LCustomNative := nil;
    LCustomMenu := nil;
    LMenu := nil;
    FreeAndNil(LApp);
    FreeAndNil(LPeerApp);
    LActions := nil;
    LCells := nil;
    LFeedback := nil;
    LIntent := nil;
    LPeerIntent := nil;
    LRanges := nil;
    LPeerRanges := nil;
    LMount := nil;
    LPeer := nil;
    LTarget := nil;
    LDocument.Free;
  end;
end;

var
  LWidth: Integer;
  LJourney: TServiceJourney;
begin
  try

    if (ParamCount < 1) or (ParamCount > 2) or not TryStrToInt(ParamStr(1), LWidth) or
      ((LWidth <> 390) and (LWidth <> 1280)) then
    begin
      raise Exception.Create('Expected native width 1280 or 390');
    end;
    LJourney := sjOriginal;

    if ParamCount = 2 then
    begin

      if (ParamStr(2) <> '--context') and (ParamStr(2) <> '--actions') then
      begin
        raise Exception.Create('Expected the explicit --context or --actions authored journey');
      end;
      LJourney := sjAuthoredContext;

      if ParamStr(2) = '--actions' then
      begin
        LJourney := sjAuthoredActions;
      end;
    end;
    Application.Initialize;
    Application.ShowMainForm := False;
    Journey(LWidth, LJourney);
    WriteLn('PASS ', GChecks, ' saved Service monitor range/peer checks at ', LWidth,
      '; native widget messages, recording publication target, no OS clipboard write');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
