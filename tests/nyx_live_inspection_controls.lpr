{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program nyx_live_inspection_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser,
  {$else}
  Interfaces, Classes, Forms, Controls, Windows, nyx.application.lcl,
  {$endif}
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.codegen, nyx.state,
  nyx.collections, nyx.collections.view, nyx.collections.inspection,
  nyx.collections.mount, nyx.collections.arrangement, nyx.collections.range,
  nyx.collections.range.commands, nyx.collections.range.actions,
  nyx.collections.query.controls, nyx.collections.clipboard.actions,
  nyx.collections.workbench.runtime, nyx.service.monitor, nyx.live.inspection
  {$ifdef NYX_INSPECTION_GENERATED}, nyx.generated.view{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create('Live inspection: ' + AReason);
  end;
end;

procedure Journey(AWidth: Integer);
var
  LDocument: TNyxDocument;
  LDemo: TNyxLiveInspectionDemo;
  LFirst: INyxCollectionMount;
  LPeer: INyxCollectionMount;
  LInspection: INyxCollectionInspection;
  LOriginal: INyxCollectionSnapshot;
  LClipboard: INyxCollectionClipboardMount;
  LCopy: INyxCollectionClipboardCommand;
  LRanges: INyxCollectionCellRangeMount;
  LRange: INyxCollectionCellRangeIntent;
  LGateway: TNyxItemRef;
  LField: TNyxColumnIdentity;
  LSaved: TNyxText;
  LRefused: Boolean;
  LRefreshCount: Integer;
  {$ifdef PAS2JS}
  LApplication: TNyxBrowserApplication;
  LHost: TJSHTMLElement;
  {$else}
  LApplication: TNyxLCLApplication;
  LStream: TFileStream;
  LBytes: UTF8String;
  {$endif}

  procedure Click(const AID: TNyxText);
  begin
    {$ifdef PAS2JS}
    TJSHTMLElement(LApplication.View.ElementFor(AID)).click;
    {$else}
    SendMessage(TWinControl(LApplication.View.ControlFor(AID)).Handle, BM_CLICK, 0, 0);
    Application.ProcessMessages;
    {$endif}
  end;

begin
  {$ifdef NYX_INSPECTION_GENERATED}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := BuildLiveInspectionDocument;
  {$endif}
  LApplication := nil;
  LDemo := nil;
  try
    LSaved := TNyxCodec.Encode(LDocument);
    {$ifndef PAS2JS}

    if ParamCount > 1 then
    begin
      Check(not FileExists(ParamStr(2)), 'generated output must be fresh');
      LBytes := UTF8Encode(TNyxCodegen.Generate(LDocument));
      LStream := TFileStream.Create(ParamStr(2), fmCreate);
      try
        LStream.WriteBuffer(LBytes[1], Length(LBytes));
      finally
        LStream.Free;
      end;
    end;
    LApplication := TNyxLCLApplication.Create;
    LApplication.View.TableWorkbenchServices := NewNyxTableWorkbenchServices;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 900;
    LApplication.Window.Show;
    Application.ProcessMessages;
    Check(LApplication.View.ControlFor(ServiceTableID).IsVisible and
      LApplication.View.ControlFor(InspectionPeerID).IsVisible,
      'both native tables are mounted and visible');
    {$else}
    LHost := TJSHTMLElement(document.createElement('div'));
    LHost.style.setProperty('width', IntToStr(AWidth) + 'px');
    document.body.appendChild(LHost);
    LApplication := TNyxBrowserApplication.Create;
    LApplication.View.TableWorkbenchServices := NewNyxTableWorkbenchServices;
    LApplication.Run(LDocument, LHost);
    Check((LApplication.View.ElementFor(ServiceTableID) <> nil) and
      (LApplication.View.ElementFor(InspectionPeerID) <> nil), 'both browser tables are mounted');
    {$endif}
    LFirst := LApplication.View.CollectionMount(ServiceTableID);
    LPeer := LApplication.View.CollectionMount(InspectionPeerID);
    Check((LFirst <> nil) and (LPeer <> nil) and (LFirst.View <> LPeer.View) and
      (LFirst.View.Store = LPeer.View.Store), 'independent views share the actual application store');
    LDemo := TNyxLiveInspectionDemo.Create(LApplication.Context, LApplication.View.Events,
      LFirst.View, LPeer.View);
    LInspection := NyxCollectionInspection(LFirst.View);
    LGateway := NyxItem(NyxCollection(ServiceCollectionName), 'gateway');
    LField := NyxColumnIdentity(NyxTextField('status'));
    Check(LFirst.Select(LGateway), 'select an inspected service through its mounted owner');
    Click(InspectionPauseID);
    LOriginal := LFirst.View.Snapshot;
    LRefreshCount := LFirst.RefreshCount;
    Check((LInspection.CaptureInspection.Mode = ncimPaused) and
      (Pos('Paused at revision', LApplication.State.GetValue(NyxTextState(InspectionStatusID))) > 0),
      'actual pause button publishes readable bound status');
    Click(InspectionStepID);
    Check((LFirst.View.Snapshot = LOriginal) and (LPeer.View.Snapshot.Count = 5) and
      (LFirst.View.CellText(LGateway, 4) = 'Running') and
      (LPeer.View.CellText(LGateway, 4) = 'Busy'), 'a live peer advances while the upper frame remains readable');
    Check((LInspection.CaptureInspection.PendingPublications = 1) and
      (Pos('1 pending updates', LApplication.State.GetValue(NyxTextState(InspectionStatusID))) > 0),
      'incoming publication changes the visible status without refreshing the displayed frame');
    Check(TNyxQueryWorkbenchObservation.Read(LFirst.View).SourceRows = 4,
      'query controls count the frozen displayed source instead of the live five-row store');
    Check(Supports(LFirst, INyxCollectionCellRangeMount, LRanges) and
      LRanges.SelectCells(NyxCollectionCellIdentity(LGateway, LField),
        NyxCollectionCellIdentity(LGateway, LField)), 'a cell range can be created on a paused frame');
    LRange := LRanges.CellRange;
    Check(Supports(LFirst, INyxCollectionClipboardMount, LClipboard), 'the actual mount exposes copy commands');
    LCopy := LClipboard.ClipboardCommands.BeginCell(LGateway, LField);
    LCopy.Prepare;
    Check(LCopy.Current and (LCopy.Payload <> nil) and (LCopy.Payload.Text = 'Running'),
      'accepted-cell copying captures displayed text rather than incoming live values');
    Click(InspectionStepID);
    Check(LRange.Current and LCopy.Current and (LFirst.View.Snapshot = LOriginal) and
      (LPeer.View.Snapshot.Count = 4), 'pending-only updates preserve displayed range and copy authority');
    Check(LFirst.RefreshCount = LRefreshCount,
      'pending status does not ask the paused physical table to rebuild its rows');
    LRefused := False;
    try
      LFirst.View.EditWire(LGateway, 4, 'Old draft');
    except
      on E: ENyxCollectionInspection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LPeer.View.CellText(LGateway, 4) = 'Busy'), 'a paused edit cannot overwrite the live value');
    Click(InspectionStepID);
    Click(InspectionResumeID);
    Check((LInspection.CaptureInspection.Mode = ncimFollowing) and
      (LFirst.View.CellText(LGateway, 4) = 'Restarted') and
      not LFirst.View.Selection.Focus.Defined and
      (LInspection.CaptureInspection.MissingSelectedRows = 1),
      'resume adopts the replacement record without resurrecting its old inspected selection');
    Check(Pos('removed or replaced', LApplication.State.GetValue(NyxTextState(InspectionStatusID))) > 0,
      'bound recovery status explains the removed inspected record');
    Check(not LRange.Current and not LCopy.Current, 'resume revokes old range and copied-cell authority');
    Check(TNyxCodec.Encode(LDocument) = LSaved, 'runtime inspection never changes the authored design');
    LDemo.Close;
    LOriginal := LFirst.View.Store.Snapshot;
    Click(InspectionStepID);
    Check(LFirst.View.Store.Snapshot = LOriginal, 'closing the binding withdraws actual button authority');
  finally
    LDemo.Free;
    LCopy := nil;
    LClipboard := nil;
    LRange := nil;
    LRanges := nil;
    LInspection := nil;
    LFirst := nil;
    LPeer := nil;
    LApplication.Free;
    LDocument.Free;
    {$ifdef PAS2JS}LHost.remove;{$endif}
  end;
end;

begin
  try
    {$ifdef PAS2JS}
    Journey(1280);
    Journey(390);
    document.body.setAttribute('data-live-inspection-checks', IntToStr(GChecks));
    {$else}
    Application.Initialize;
    Journey(StrToInt(ParamStr(1)));
    WriteLn('PASS ', GChecks, ' live inspection control checks at ', ParamStr(1));
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-live-inspection-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
