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
program nyx_service_monitor_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Math, Types, Forms, Controls, Grids, Windows, LMessages,
  nyx.text, nyx.model, nyx.codec, nyx.codegen, nyx.controls,
  nyx.collections.presentation,
  nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.range.commands, nyx.collections.selection,
  nyx.collections, nyx.collections.arrangement, nyx.collections.mount, nyx.application.lcl,
  nyx.collections.view, nyx.collections.arrangement.controls, nyx.collections.columns,
  nyx.collections.lcl,
  nyx.service.monitor
  {$ifdef NYX_SERVICE_GENERATED}, nyx.generated.view{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create('Service monitor: ' + AReason);
  end;
end;

{ Compare independently mounted body-density choices on real native grids.
  A runtime density update already preserves headers; authored initial density
  must do the same. An optional title-font probe checks actual caption metrics.
  The probe owns its cloned document until its host retires. }
function HeaderHeightWithDensity(ADocument: TNyxDocument; AWidth: Integer;
  ADensity: TNyxTableRowDensity; AUseLegacyHeader: Boolean = False;
  ATitlePixels: Integer = 0): Integer;
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LTable: TNyxNode;
  LPolicy: TNyxTablePresentationPolicy;
  LGrid: TStringGrid;
begin
  LDocument := ADocument.Clone;
  LApplication := nil;
  try
    LTable := LDocument.Find(ServiceTableID);
    LPolicy := LTable.CollectionView.TablePresentationPolicy.Density(ADensity);

    if AUseLegacyHeader then
    begin
      LPolicy := LPolicy.UseDefaultHeader;
    end;
    LTable.SetCollectionView(LTable.CollectionView.TablePresentation(LPolicy));
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 760;
    LApplication.Window.Show;
    Application.ProcessMessages;
    LGrid := TStringGrid(LApplication.View.ControlFor(ServiceTableID));

    if ATitlePixels > 0 then
    begin
      LGrid.TitleFont.Height := -ATitlePixels;
      LApplication.View.CollectionMount(ServiceTableID).Refresh;
      Application.ProcessMessages;
    end;
    Result := LGrid.RowHeights[0];
  finally
    LApplication.Free;
    LDocument.Free;
  end;
end;

{ Native pinned data panes are ordinary grids parented by the main table. Count
  only visible, nonempty panes wholly inside that table's current client frame;
  a declared pin alone cannot establish that a compact user can reach it. }
function VisibleTablePanes(ATable: TControl): Integer;
var
  LHost: TWinControl;
  LPane: TControl;
  LIndex: Integer;
begin
  Result := 0;

  if not (ATable is TStringGrid) or not ATable.IsVisible then
  begin
    Exit;
  end;
  Result := 1;
  LHost := TWinControl(ATable);
  for LIndex := 0 to LHost.ControlCount - 1 do
  begin
    LPane := LHost.Controls[LIndex];

    if (LPane is TStringGrid) and LPane.IsVisible and
      (LPane.Width > 0) and (LPane.Height > 0) and (LPane.Left >= 0) and
      (LPane.Left + LPane.Width <= LHost.ClientWidth) then
    begin
      Inc(Result);
    end;
  end;
end;

{ Explicit UTF-8 bytes preserve the generated program's portable text. The
  fixture refuses existing outputs rather than replacing another run's source. }
procedure WriteSource(const APath: String; const AText: TNyxText);
var
  LBytes: UTF8String;
  LStream: TFileStream;
begin

  if FileExists(APath) then
  begin
    raise Exception.Create('The generated source output already exists');
  end;
  LBytes := UTF8Encode(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[1], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;

procedure Journey(AWidth: Integer);
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LPeer: TNyxLCLApplication;
  LMount: INyxCollectionMount;
  LGateway: TNyxItemRef;
  LBefore: TNyxText;
  LEdited: TNyxText;
  LNameColumn: Integer;
  LStatusColumn: Integer;
  LProcessColumn: Integer;
begin
  {$ifdef NYX_SERVICE_GENERATED}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := BuildServiceMonitorDocument;
  {$endif}
  LApplication := nil;
  LPeer := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LBefore);
    try
      Check(TNyxCodec.Encode(LDecoded) = LBefore,
        'the complete authored table and collection survive persistence');
    finally
      LDecoded.Free;
    end;

    if ParamCount > 1 then
    begin
      WriteSource(ParamStr(2), TNyxCodegen.Generate(LDocument));
    end;
    LApplication := TNyxLCLApplication.Create;
    LPeer := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LPeer.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 760;
    LApplication.Window.Show;
    Application.ProcessMessages;
    LMount := LApplication.View.CollectionMount(ServiceTableID);
    Check((LMount <> nil) and LMount.Connected,
      'the public sample mounts its actual native collection adapter');
    { The designer may reorder its saved columns. Exact typed field identity,
      rather than the starter example's original position, owns every edit. }
    LNameColumn := LMount.View.Spec.ColumnIndex(NyxColumnIdentity(NyxTextField('name')));
    LStatusColumn := LMount.View.Spec.ColumnIndex(NyxColumnIdentity(NyxTextField('status')));
    LProcessColumn := LMount.View.Spec.ColumnIndex(NyxColumnIdentity(NyxIntegerField('pid')));
    Check((LNameColumn >= 0) and (LStatusColumn >= 0) and (LProcessColumn >= 0),
      'the saved application retains exact editable and readonly field families');
    Check(LApplication.View.ControlFor(ServiceTableID).IsVisible,
      'the table is visible at the requested native width');
    Check(VisibleTablePanes(LApplication.View.ControlFor(ServiceTableID)) = 3,
      'both pinned panes remain visible beside the center at this width');
    Check(HeaderHeightWithDensity(LDocument, AWidth, ntrdCompact) =
      TStringGrid(LApplication.View.ControlFor(ServiceTableID)).RowHeights[0],
      'initial body density leaves the independent query/column header unchanged');
    Check(TStringGrid(LApplication.View.ControlFor(ServiceTableID)).RowHeights[0] <
      HeaderHeightWithDensity(LDocument, AWidth, ntrdComfortable, True),
      'the explicit single-line header releases space while preserving its toolbar');
    Check(HeaderHeightWithDensity(LDocument, AWidth, ntrdComfortable, False, 48) >
      TStringGrid(LApplication.View.ControlFor(ServiceTableID)).RowHeights[0],
      'explicit header reservation follows the actual title font independently of body text');
    Check(LApplication.Collections.Collection(NyxCollection(ServiceCollectionName)) <>
      LPeer.Collections.Collection(NyxCollection(ServiceCollectionName)),
      'two applications own independent runtime collections');
    Check(LMount.View.Snapshot.Count = 4, 'all four sample records are reachable');
    LGateway := NyxItem(NyxCollection(ServiceCollectionName), 'gateway');
    LEdited := 'Gateway / 🌙';
    Check(LMount.EditCell(LGateway, LNameColumn, LEdited),
      'the logical-start pinned service field accepts a mounted edit');
    Check(LApplication.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxTextField('name')) = LEdited,
      'the exact Unicode edit reaches its original row and field');
    Check(LMount.EditCell(LGateway, LStatusColumn, 'Paused'),
      'the logical-end pinned status field accepts a mounted edit');
    Check(LApplication.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxTextField('status')) = 'Paused',
      'the end-pinned edit reaches its original status field');
    Check(not LMount.EditCell(LGateway, LProcessColumn, '9999'),
      'read-only process identifiers refuse edits');
    Check(LApplication.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxIntegerField('pid')) = 2401,
      'refusal preserves the accepted process identifier');
    Check(LPeer.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxTextField('name')) = 'Request gateway',
      'one application cannot edit another application through authored defaults');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'mounted edits never rewrite the authored document or column defaults');
    FreeAndNil(LApplication);
    Check(not LMount.Connected, 'application disposal disconnects the held native mount');
    Check(LPeer.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxTextField('status')) = 'Running',
      'retiring one application leaves the other runtime alive and unchanged');
  finally
    LMount := nil;
    LPeer.Free;
    LApplication.Free;
    LDocument.Free;
  end;
end;

{ Controlled Win32 keys exercise the actual focused grid and canonical Nyx
  event route. Restore the calling thread's exact modifier bytes even on failure;
  these messages do not claim trusted keyboard or assistive-technology input. }
procedure PageKey(AGrid: TStringGrid; AKey: Word; AShift: Boolean = True);
var
  LSaved: TKeyboardState;
  LState: TKeyboardState;
  LHandle: HWND;
begin
  Check(GetKeyboardState(LSaved), 'capture exact modifiers before the page key');
  LState := LSaved;
  LState[VK_CONTROL] := 0;
  LState[VK_LCONTROL] := 0;
  LState[VK_RCONTROL] := 0;
  LState[VK_MENU] := 0;
  LState[VK_LMENU] := 0;
  LState[VK_RMENU] := 0;
  LState[VK_SHIFT] := 0;
  LState[VK_LSHIFT] := 0;
  LState[VK_RSHIFT] := 0;

  if AShift then
  begin
    LState[VK_SHIFT] := $80;
    LState[VK_LSHIFT] := $80;
  end;
  LHandle := AGrid.Handle;
  Check(SetKeyboardState(LState), 'supply the original page-key modifiers');
  try
    SendMessage(LHandle, WM_KEYDOWN, AKey, 1);

    if IsWindow(LHandle) then
    begin
      SendMessage(LHandle, WM_KEYUP, AKey, LPARAM($C0000001));
    end;
    Application.ProcessMessages;
  finally
    Check(SetKeyboardState(LSaved), 'restore the caller modifiers after the page key');
  end;
end;

{ A separate public-example instance supplies enough rows to exceed the actual
  viewport. The optional generated build creates the same table before its test
  data is supplied. Mounted hosts retire before their borrowed documents; no
  active Studio project, clipboard or external service is used. }
function PageJourney(AWidth, ATableHeight: Integer): Integer;
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LMount: INyxCollectionMount;
  LRanges: INyxCollectionCellRangeMount;
  LItems: array of TNyxCollectionItem;
  LKey: TNyxCollectionRef;
  LTable: TStringGrid;
  LGrid: TStringGrid;
  LCandidate: TStringGrid;
  LRows: TNyxCollectionSelectionSnapshot;
  LIntent: INyxCollectionCellRangeIntent;
  LOrigin: TNyxCollectionCellIdentity;
  LBefore: TNyxText;
  LIndex: Integer;
  LField: Integer;
  LChild: Integer;
  LColumn: Integer;
  LPhysical: Integer;
  LStep: Integer;
  LFields: array[0..3] of TNyxColumnIdentity;
  LTitle: TNyxText;
  LPreferences: TNyxText;
  LBounds: Types.TRect;
  LSource: INyxCollectionSnapshot;
  LClipLeft: Integer;
  LClipRight: Integer;
  LHint: TNyxText;
  LHints: INyxLCLCollectionHints;

  { Resize may replace a platform face. Borrow the currently mounted controls
    again instead of dereferencing a grid from before the viewport publication. }
  procedure ObserveCurrentTable;
  begin
    LMount := LApplication.View.CollectionMount(ServiceTableID);
    LTable := TStringGrid(LApplication.View.ControlFor(ServiceTableID));
    Check((LMount <> nil) and LMount.Connected and
      Supports(LMount, INyxCollectionCellRangeMount, LRanges),
      'the resized table exposes its current live input and range owner');
    LRanges.ConfigureCellRange(True);
    Check(LMount.View.Store.Snapshot = LSource,
      'native resize preserves the exact independent accepted store');
  end;

  { This fixture has fixed requested pin widths, so the independent budget
    needs no content measurement. Check the real adapter against the authored
    minimum-center contract, including native chrome and actual font scaling. }
  function CheckPinBudget: Integer;
  var
    LObservation: TNyxColumnLayoutObservation;
    LEntry: TNyxColumnLayoutEntry;
    LScale: Double;
    LAvailable: Integer;
    LRequired: Integer;
    LIndex: Integer;
    LStart: Boolean;
    LEnd: Boolean;
    LNativeHint: String;
  begin
    LObservation := TNyxColumnLayoutObservation.Read(
      NyxCollectionColumns(LMount.View).CaptureColumns);
    LScale := LTable.Font.PixelsPerInch / 96;

    if LScale <= 0 then
    begin
      LScale := 1;
    end;
    LAvailable := Max(0, Round((LTable.ClientWidth -
      LTable.GridLineWidth * LObservation.VisibleCount) / LScale));
    LRequired := LObservation.Spec.TableLayoutPolicy.MinimumScrollableWidth;
    LStart := False;
    LEnd := False;
    for LIndex := 0 to LObservation.Count - 1 do
    begin
      LEntry := LObservation.ColumnAt(LIndex);

      if not LEntry.IsVisible or (LEntry.Pin = ncpNone) then
      begin
        Continue;
      end;
      Check(LEntry.HasWidth or (LEntry.Column.Sizing.Mode = ncsFixed),
        'the saved fixture has an exact requested width for each pinned field');

      if LEntry.HasWidth then
      begin
        Inc(LRequired, LEntry.Width);
      end
      else
      begin
        Inc(LRequired, LEntry.Column.Sizing.Value);
      end;
      LStart := LStart or (LEntry.Pin = ncpStart);
      LEnd := LEnd or (LEntry.Pin = ncpEnd);
    end;
    Check(LStart and LEnd, 'both requested pin groups remain authored through resize');
    Result := 1;

    if LRequired <= LAvailable then
    begin
      Result := 3;
    end;
    WriteLn('pin budget client=', LTable.ClientWidth, ' logical=', LAvailable,
      ' required=', LRequired, ' expectedPanes=', Result,
      ' actualPanes=', VisibleTablePanes(LTable));
    Check(VisibleTablePanes(LTable) = Result,
      'native panes retain both pins when they fit and a scrollable center when they cannot');
    Check(Assigned(LTable.OnGetCellHint) and (goCellHints in LTable.Options),
      'native hover uses the current collection hint port');
    LNativeHint := '';
    LTable.OnGetCellHint(LTable, 0, 1, LNativeHint);
    Check((Pos('Pinned columns are scrolling at this width.', TNyxText(LNativeHint)) > 0) =
      (Result = 1), 'native help reports temporary pin suspension at the actual budget');
    Check((Pos(LHint, TNyxText(LNativeHint)) = 1) and LTable.ShowHint,
      'authored Unicode help and current adapter guidance compose after viewport synchronization');
    Check(LObservation.Preferences.ToData.ToJSON = LPreferences,
      'host resizing never creates or rewrites runtime column preferences');
  end;

  procedure Resize(AClientWidth, AExpectedPanes: Integer);
  begin
    LApplication.Window.ClientWidth := AClientWidth;
    Application.ProcessMessages;
    Application.Idle(False);
    Application.ProcessMessages;
    ObserveCurrentTable;
    Check(LApplication.Window.ClientWidth = AClientWidth,
      'native resize retains the requested actual client width');
    Check(CheckPinBudget = AExpectedPanes, 'the explicit wide/narrow allocation exercises the expected pin state');
    Check(LMount.View.Snapshot.Count = 64, 'resize keeps every accepted record in the same full page');
  end;
begin
  Result := 0;
  {$ifdef NYX_SERVICE_GENERATED}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := BuildServiceMonitorDocument;
  {$endif}
  LApplication := nil;
  try
    LKey := NyxCollection(ServiceCollectionName);
    SetLength(LItems, 64);
    for LIndex := 0 to High(LItems) do
    begin
      LItems[LIndex] := NyxCollectionItem(NyxItem(LKey,
        'service-' + TNyxText(IntToStr(LIndex))))
        .WithValue(NyxTextField('name'), 'Service ' + TNyxText(IntToStr(LIndex)))
        .WithValue(NyxIntegerField('pid'), 5000 + LIndex)
        .WithValue(NyxTextField('status'), 'Running');
    end;
    LDocument.Collections.Define(LKey, LDocument.Collections.Snapshot(LKey).Schema, LItems);
    LHint := 'Browse service details 🧭';
    LDocument.Find(ServiceTableID).Configure.Height(ATableHeight).Hint(LHint).Done;
    LBefore := TNyxCodec.Encode(LDocument);
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 760;
    LApplication.Window.Show;
    Application.ProcessMessages;
    LMount := LApplication.View.CollectionMount(ServiceTableID);
    Check(Supports(LMount, INyxCollectionCellRangeMount, LRanges),
      'the public table exposes its existing typed range owner');
    LRanges.ConfigureCellRange(True);
    LTable := TStringGrid(LApplication.View.ControlFor(ServiceTableID));
    LSource := LMount.View.Store.Snapshot;
    LPreferences := NyxCollectionColumns(LMount.View).CaptureColumns.Preferences.ToData.ToJSON;
    Check(LMount.View.Snapshot.Count = 64, 'the long public workbench retains its full page');
    CheckPinBudget;
    Resize(1280, 3);
    Resize(320, 1);
    LHint := 'Updated service help 🛠';
    LApplication.View.Root.Find(ServiceTableID).Configure.Hint(LHint).Done;
    LApplication.View.Sync;
    Application.ProcessMessages;
    ObserveCurrentTable;
    CheckPinBudget;
    LHint := 'Browse service details 🧭';
    LApplication.View.Root.Find(ServiceTableID).Configure.Hint(LHint).Done;
    LApplication.View.Sync;
    LApplication.Window.ClientWidth := AWidth;
    Application.ProcessMessages;
    Application.Idle(False);
    Application.ProcessMessages;
    ObserveCurrentTable;
    CheckPinBudget;
    LFields[0] := NyxColumnIdentity(NyxTextField('name'));
    LFields[1] := NyxColumnIdentity(NyxTextField('notes'));
    LFields[2] := NyxColumnIdentity(NyxIntegerField('pid'));
    LFields[3] := NyxColumnIdentity(NyxTextField('status'));
    for LField := Low(LFields) to High(LFields) do
    begin
      LColumn := LMount.View.Spec.ColumnIndex(LFields[LField]);
      Check(LColumn >= 0, 'each required visible field retains its exact scalar family');
      LTitle := LMount.View.Spec.ColumnAt(LColumn).Title;
      LGrid := nil;
      LPhysical := -1;
      for LChild := -1 to LTable.ControlCount - 1 do
      begin
        LCandidate := LTable;

        if LChild >= 0 then
        begin

          if not (LTable.Controls[LChild] is TStringGrid) then
          begin
            Continue;
          end;
          LCandidate := TStringGrid(LTable.Controls[LChild]);
        end;

        if not LCandidate.IsVisible then
        begin
          Continue;
        end;
        for LColumn := 0 to LCandidate.ColCount - 1 do
        begin

          if Pos(LTitle, TNyxText(LCandidate.Cells[LColumn, 0])) = 1 then
          begin
            LGrid := LCandidate;
            LPhysical := LColumn;
            Break;
          end;
        end;

        if LGrid <> nil then
        begin
          Break;
        end;
      end;
      Check((LGrid <> nil) and LGrid.IsVisible,
        'the page key resolves a real center or pinned field');
      LRanges.ClearCellRange;
      LGrid.Row := 1;
      LGrid.Col := LPhysical;
      LGrid.SetFocus;
      Application.ProcessMessages;
      LBounds := LGrid.CellRect(LPhysical, 1);
      LClipLeft := 0;
      LClipRight := LGrid.ClientWidth;

      if LGrid = LTable then
      begin
        for LChild := 0 to LTable.ControlCount - 1 do
        begin

          if not (LTable.Controls[LChild] is TStringGrid) then
          begin
            Continue;
          end;
          LCandidate := TStringGrid(LTable.Controls[LChild]);

          if not LCandidate.IsVisible then
          begin
            Continue;
          end;

          if LCandidate.Left <= 0 then
          begin
            LClipLeft := Max(LClipLeft, LCandidate.Left + LCandidate.Width);
          end
          else if LCandidate.Left + LCandidate.Width >= LTable.ClientWidth then
          begin
            LClipRight := Min(LClipRight, LCandidate.Left);
          end;
        end;
      end;
      Check((LClipRight > LClipLeft) and
        (LBounds.Right > LClipLeft) and (LBounds.Left < LClipRight),
        'keyboard focus reveals a painted part of every visible field instead of an offscreen column');
      { Exercise LCL's hover-message path on each actual pane as well as the
        copied hint port. No Windows cursor movement or screenshot substitutes
        for this deterministic message-level check. Physical hover stays separate. }
      LGrid.Perform(CM_MOUSEENTER, 0, 0);
      LGrid.Perform(LM_MOUSEMOVE, 0, MakeLParam(
        (Max(LBounds.Left, LClipLeft) + Min(LBounds.Right, LClipRight)) div 2,
        (LBounds.Top + LBounds.Bottom) div 2));
      Check(Pos(LHint, TNyxText(LGrid.Hint)) = 1,
        'native cell hover in every owned pane obtains current authored and runtime guidance');
      LGrid.Perform(CM_MOUSELEAVE, 0, 0);
      LRows := LMount.View.Selection.Snapshot;
      LStep := LGrid.VisibleRowCount - 1;
      WriteLn('page viewport field=', LTitle, ' width=', AWidth,
        ' authoredHeight=', ATableHeight, ' nativeHeight=', LGrid.Height,
        ' visibleRows=', LGrid.VisibleRowCount, ' pageRows=', LMount.View.Snapshot.Count);
      Check((LStep >= 1) and (LStep < LMount.View.Snapshot.Count - 1),
        'the physical viewport supplies a positive bounded step within the long page');

      if ATableHeight = 480 then
      begin
        Check(LStep > 1, 'the full viewport proves actual multi-row paging');
      end;
      Result := LStep;
      PageKey(LGrid, VK_NEXT);
      LIntent := LRanges.CellRange;
      Check((LIntent <> nil) and LIntent.Current and
        (LIntent.Capture.Focus.Item.ID = LMount.View.Snapshot.ItemAt(LStep).Ref.ID) and
        (LIntent.Capture.Payload.RowCount = LStep + 1) and
        (LIntent.Capture.Payload.FieldCount = 1) and
        (LIntent.Capture.Focus.Field.Name = LFields[LField].Name) and
        (LIntent.Capture.Focus.Field.Kind = LFields[LField].Kind),
        'Shift+PageDown uses the measured viewport step and a single stable field');
      LOrigin := LIntent.Capture.Anchor;
      PageKey(TStringGrid(Screen.ActiveControl), VK_PRIOR);
      Check(not LIntent.Current and LRanges.CellRange.Capture.Anchor.Same(LOrigin) and
        LRanges.CellRange.Capture.Focus.Same(LOrigin) and
        (LRanges.CellRange.Capture.Payload.RowCount = 1) and
        LMount.View.Selection.Snapshot.SameState(LRows),
        'Shift+PageUp reverses to the same anchor without changing ordinary row selection');
      PageKey(TStringGrid(Screen.ActiveControl), VK_ESCAPE, False);
      Check(LRanges.CellRange = nil, 'Escape clears the page range through ordinary canonical keys');
    end;
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'viewport paging never rewrites authored defaults or source data');
    Check(Supports(LMount, INyxLCLCollectionHints, LHints),
      'the native mount exposes optional authored hint synchronization');
    LMount.Disconnect;
    Check((TNyxText(LTable.Hint) = LHint) and LTable.ShowHint,
      'disconnect restores the latest authored hint without stale collection guidance');
    Check(not Assigned(LTable.OnGetCellHint),
      'disconnect restores the originally absent host callback instead of retaining the mount');
    LHints.SetAuthoredHint('Retired receiver');
    Check(TNyxText(LTable.Hint) = LHint, 'a retained retired hint receiver cannot touch its former control');
  finally
    LHints := nil;
    LRanges := nil;
    LMount := nil;
    LApplication.Free;
    LDocument.Free;
  end;
end;

procedure PageJourneys(AWidth: Integer);
var
  LFullStep: Integer;
  LShortStep: Integer;
begin
  LFullStep := PageJourney(AWidth, 480);
  LShortStep := PageJourney(AWidth, 180);
  Check(LShortStep < LFullStep,
    'a shorter authored viewport produces a smaller page-extension step');
end;

begin
  try
    Application.Initialize;
    Journey(StrToInt(ParamStr(1)));
    PageJourneys(StrToInt(ParamStr(1)));
    WriteLn('PASS ', GChecks, ' service monitor mount checks at ', ParamStr(1));
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
