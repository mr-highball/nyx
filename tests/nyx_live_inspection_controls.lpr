{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program nyx_live_inspection_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser,
  {$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, Grids, Windows, nyx.application.lcl,
  {$endif}
  SysUtils, Math, nyx.text, nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.state,
  nyx.collections, nyx.collections.view, nyx.collections.inspection,
  nyx.collections.position, nyx.collections.query,
  nyx.collections.feedback, nyx.collections.query.session, nyx.publication,
  nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.arrangement, nyx.collections.range,
  nyx.collections.range.commands, nyx.collections.range.actions,
  nyx.collections.query.controls, nyx.collections.clipboard.actions,
  nyx.collections.workbench.runtime, nyx.service.monitor, nyx.live.inspection
  {$ifdef NYX_INSPECTION_GENERATED}, nyx.generated.view{$endif};

{$ifdef PAS2JS}
type
  { Standard DOM scrollTop uses fractional CSS pixels. The bridge borrows the
    mounted table for this synchronous fixture only, on browsers with layout. }
  TPositionElement = class external name 'HTMLElement'(TJSHTMLElement)
    scrollTopPixels: Double; external name 'scrollTop';
  end;
{$endif}

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
  LFeedback: INyxCollectionFeedback;
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

  procedure PositionCases;
  var
    LChanges: array of TNyxCollectionEdit;
    LIndex: Integer;
    LBefore: INyxCollectionSnapshot;
    LAnchor: TNyxItemRef;
    LNext: TNyxItemRef;
    LObservation: INyxCollectionPositionObservation;
    LFollowing: INyxCollectionFollowing;
    LFollowSerial: Integer;
    {$ifdef PAS2JS}
    LTable: TJSHTMLElement;
    {$else}
    LTable: TStringGrid;
    {$endif}

    function TopItem: TNyxItemRef;
    {$ifdef PAS2JS}
    var
      LRows: TJSNodeList;
      LRow: TJSHTMLElement;
      LClip: Double;
      LHeader: TJSHTMLElement;
      LRowIndex: Integer;
    {$endif}
    begin
      {$ifdef PAS2JS}
      Result := Default(TNyxItemRef);
      LClip := Max(0, LTable.getBoundingClientRect.top);
      LHeader := TJSHTMLElement(LTable.querySelector('thead'));

      if LHeader <> nil then
      begin
        LClip := Max(LClip, LHeader.getBoundingClientRect.bottom);
      end;
      LRows := LTable.querySelectorAll('[data-nyx-item]');
      for LRowIndex := 0 to LRows.length - 1 do
      begin
        LRow := TJSHTMLElement(LRows[LRowIndex]);

        if (LRow.offsetHeight > 0) and (LRow.getBoundingClientRect.bottom > LClip + 0.5) and
          (LRow.getBoundingClientRect.top < LTable.getBoundingClientRect.bottom) then
        begin
          Result := NyxItem(NyxCollection(ServiceCollectionName), LRow.getAttribute('data-nyx-item'));
          Exit;
        end;
      end;
      {$else}
      Result := LFirst.View.Snapshot.ItemAt(LTable.TopRow - 1).Ref;
      {$endif}
    end;

    function LastRowVisible: Boolean;
    {$ifdef PAS2JS}
    var
      LRow: TJSHTMLElement;
      LLast: TNyxItemRef;
    {$endif}
    begin
      {$ifdef PAS2JS}
      LLast := LFirst.View.Snapshot.ItemAt(LFirst.View.Snapshot.Count - 1).Ref;
      LRow := TJSHTMLElement(LTable.querySelector('[data-nyx-item="' + LLast.ID + '"]'));
      Result := (LRow <> nil) and (LRow.getBoundingClientRect.bottom > LTable.getBoundingClientRect.top) and
        (LRow.getBoundingClientRect.top < LTable.getBoundingClientRect.bottom);
      {$else}
      Result := (LTable.CellRect(LTable.FixedCols, LTable.RowCount - 1).Top < LTable.ClientHeight) and
        (LTable.CellRect(LTable.FixedCols, LTable.RowCount - 1).Bottom >
          LTable.CellRect(LTable.FixedCols, 0).Bottom);
      {$endif}
    end;

    procedure ReadMiddle;
    begin
      {$ifdef PAS2JS}
      TPositionElement(LTable).scrollTopPixels := 480;
      LFirst.Refresh;
      {$else}
      LTable.TopRow := 15;
      {$endif}
      LAnchor := TopItem;
    end;

  begin
    SetLength(LChanges, 64);
    LBefore := LFirst.View.Store.Snapshot;
    for LIndex := 0 to High(LChanges) do
    begin
      LChanges[LIndex] := NyxInsert(LBefore.Count + LIndex,
        NyxCollectionItem(NyxItem(LBefore.Key, 'diagnostic-' + TNyxText(IntToStr(LIndex))))
          .WithValue(NyxTextField('name'), 'Diagnostic ' + TNyxText(Format('%.3d', [LIndex])))
          .WithValue(NyxTextField('status'), 'Running'));
    end;
    LFirst.View.Store.Apply(LChanges, LBefore.Revision);
    Check(Supports(LFirst, INyxCollectionPositionObservation, LObservation),
      'the actual mount exposes detached position recovery observations');
    LFirst.Select(LGateway);
    {$ifdef PAS2JS}
    LTable := LApplication.View.ElementFor(ServiceTableID);
    TPositionElement(LTable).scrollTopPixels := 480;
    LFirst.Refresh;
    {$else}
    LTable := TStringGrid(LApplication.View.ControlFor(ServiceTableID));
    LTable.TopRow := 15;
    {$endif}
    LAnchor := TopItem;
    Check(LAnchor.Defined and (LFirst.View.Snapshot.IndexOf(LAnchor) > 5),
      'read a middle record while the selected gateway remains offscreen');
    LFirst.View.Store.Insert(0, NyxCollectionItem(NyxItem(LBefore.Key, 'inserted-above'))
      .WithValue(NyxTextField('name'), 'Inserted above'));
    Check((TopItem.ID = LAnchor.ID) and
      (LObservation.GetPositionRecovery.Kind = ncpRetained),
      'inserting above preserves the physical reader anchor after selection restoration');
    LBefore := LFirst.View.Snapshot;
    LNext := LBefore.ItemAt(LBefore.IndexOf(LAnchor) + 1).Ref;
    LFirst.View.Store.Remove(LAnchor);
    Check((TopItem.ID = LNext.ID) and
      (LObservation.GetPositionRecovery.Kind = ncpFollowing),
      'deleting the anchor shows its nearest surviving old successor');
    Check(Pos('reading row disappeared', LApplication.State.GetValue(NyxTextState(InspectionStatusID))) > 0,
      'the ordinary bound status explains physical anchor recovery');
    LFirst.Select(LNext);
    Check(Pos('reading row disappeared', LApplication.State.GetValue(NyxTextState(InspectionStatusID))) = 0,
      'a later deliberate selection does not repeat an older viewport recovery message');
    LAnchor := TopItem;
    LFirst.View.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxTextField('name'), nsdDescending));
    Check((TopItem.ID = LAnchor.ID) and
      (LObservation.GetPositionRecovery.Kind = ncpRetained),
      'sorting preserves the same physical reading identity');
    LInspection.PauseInspection;
    LFirst.View.Store.Insert(0, NyxCollectionItem(NyxItem(LBefore.Key, 'paused-above'))
      .WithValue(NyxTextField('name'), 'ZZ paused insertion'));
    Check(TopItem.ID = LAnchor.ID, 'a paused incoming insertion leaves the physical anchor intact');
    LInspection.ResumeInspection;
    Check((TopItem.ID = LAnchor.ID) and
      (LObservation.GetPositionRecovery.Kind = ncpRetained),
      'resuming coalesced data preserves the inspected physical row');
    LInspection.PauseInspection;
    LBefore := LFirst.View.Store.Snapshot;
    LFirst.View.Store.Apply([NyxRemove(LAnchor), NyxInsert(0,
      NyxCollectionItem(LAnchor).WithValue(NyxTextField('name'), 'Replacement incarnation'))], LBefore.Revision);
    LInspection.ResumeInspection;
    Check((TopItem.ID <> LAnchor.ID) and
      (LObservation.GetPositionRecovery.Kind = ncpFollowing),
      'a reused source ID cannot inherit its old physical reading position');
    LFirst.View.ConfigureQuery(Default(TNyxCollectionQuery));
    LFirst.Select(LGateway);
    ReadMiddle;
    LBefore := LFirst.View.Snapshot;
    LFollowing := NyxCollectionFollowing(LFirst.View);
    Click(InspectionFollowID);
    Check((LObservation.GetPositionRecovery.Kind = ncpLatest) and LastRowVisible and
      (LFirst.View.Selection.Focus.ID = LGateway.ID) and (LFirst.View.Snapshot = LBefore),
      'the ordinary Follow button reveals the current last row without selecting it or changing source');
    Check(NyxCollectionInspection(LPeer.View).CaptureInspection.FollowMode = ncfKeepPosition,
      'the public Follow button does not enable following in its independent peer');
    LBefore := LFirst.View.Store.Snapshot;
    LFirst.View.Store.Insert(LBefore.Count,
      NyxCollectionItem(NyxItem(LBefore.Key, 'tail-followed')).WithValue(NyxTextField('name'), 'New last row'));
    Check(LastRowVisible and (LObservation.GetPositionRecovery.Kind = ncpLatest) and
      (LObservation.GetPositionRecovery.Current.ID = 'tail-followed'),
      'an incoming displayed tail is physically revealed while following is deliberately enabled');
    ReadMiddle;
    LBefore := LFirst.View.Store.Snapshot;
    LFirst.View.Store.Apply([NyxRemove(NyxItem(LBefore.Key, 'tail-followed')),
      NyxInsert(LBefore.Count - 1, LBefore.Item(NyxItem(LBefore.Key, 'tail-followed')))]);
    Check(LastRowVisible and (LObservation.GetPositionRecovery.Kind = ncpLatest),
      'an equal-value reused tail ID follows as a new incarnation using exact removal evidence');
    ReadMiddle;
    LFirst.View.Store.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('notes'), 'An unrelated live marker'));
    Check((TopItem.ID = LAnchor.ID) and (LObservation.GetPositionRecovery.Kind = ncpRetained),
      'an unrelated value update preserves the reader even while follow mode is enabled');
    LFollowSerial := LInspection.CaptureInspection.FollowRequestSerial;
    Click(InspectionFollowID);
    Check(LastRowVisible and (LInspection.CaptureInspection.FollowRequestSerial = LFollowSerial + 1),
      'pressing Follow again reveals the tail at the same source revision');
    Click(InspectionKeepID);
    ReadMiddle;
    LBefore := LFirst.View.Store.Snapshot;
    LFirst.View.Store.Insert(LBefore.Count,
      NyxCollectionItem(NyxItem(LBefore.Key, 'tail-not-followed')).WithValue(NyxTextField('name'), 'A later last row'));
    Check((TopItem.ID = LAnchor.ID) and (LInspection.CaptureInspection.FollowMode = ncfKeepPosition),
      'the ordinary Keep reading button stops later tail publications from moving the reader');
    Click(InspectionPauseID);
    {$ifdef PAS2JS}
    Check(TJSHTMLButtonElement(LApplication.View.ElementFor(InspectionFollowID)).disabled,
      'the public Follow command is visibly disabled while inspection is paused');
    {$else}
    Check(not LApplication.View.ControlFor(InspectionFollowID).Enabled,
      'the public Follow command is visibly disabled while inspection is paused');
    {$endif}
    Click(InspectionResumeID);
    Check(LInspection.CaptureInspection.FollowMode = ncfKeepPosition,
      'resuming never silently turns a stopped follow policy back on');
  end;

  procedure FeedbackCases;
  var
    LParts: TNyxCollectionFeedbackParts;
    LBefore: INyxCollectionSnapshot;
    LSession: INyxCollectionQuerySession;
    LPeerOwner: INyxCollectionQueryOwner;
    LPlan: INyxPreparedPublication;

    function PartVisible(const AID: TNyxText): Boolean;
    begin
      {$ifdef PAS2JS}
      Result := not LApplication.View.ElementFor(AID).hasAttribute('hidden') and
        (LApplication.View.ElementFor(AID).style.getPropertyValue('display') <> 'none');
      {$else}
      Result := LApplication.View.ControlFor(AID).Visible;
      {$endif}
    end;

  begin
    LParts := NyxCollectionFeedbackParts(InspectionFeedbackName);
    LFeedback := LDemo.Feedback;
    LBefore := LFirst.View.Store.Snapshot;
    Click(InspectionStepID);
    Check((LFeedback.Capture.Kind = ncfkLoading) and PartVisible(LParts.ProgressID) and
      (LFirst.View.Store.Snapshot = LBefore), 'a real replay step presents loading activity while retaining current records');
    {$ifdef PAS2JS}
    Check(LApplication.View.ElementFor(LParts.RootID).getAttribute('aria-label') =
      LApplication.State.GetValue(LParts.Caption), 'browser feedback exposes its current state as its accessible name');
    {$else}
    Check(TNyxText(LApplication.View.ControlFor(LParts.RootID).AccessibleName) =
      LApplication.State.GetValue(LParts.Caption), 'native feedback exposes its current state as its accessible name');
    {$endif}
    Click(InspectionStepID);
    Check((LFeedback.Capture.Kind = ncfkRefreshing) and PartVisible(LParts.ProgressID) and
      (LFirst.View.Store.Snapshot = LBefore), 'refreshing has a distinct heading and keeps the displayed dataset');
    Click(InspectionStepID);
    Check((LFeedback.Capture.Kind = ncfkFailed) and PartVisible(LParts.RetryID) and
      not PartVisible(LParts.ProgressID) and (LFirst.View.Store.Snapshot = LBefore),
      'failure reveals the ordinary bound Retry button without discarding current records');
    Click(LParts.RetryID);
    Check((LFeedback.Capture.Kind = ncfkReady) and
      (LFirst.View.Store.Snapshot.Revision > LBefore.Revision) and not PartVisible(LParts.RetryID),
      'the actual Retry button reconnects the sample feed and publishes a real data update');
    Click(InspectionStepID);
    Click(InspectionStepID);
    Check((LFeedback.Capture.Kind = ncfkFilteredEmpty) and PartVisible(LParts.ResetID) and
      not PartVisible(LParts.RetryID) and (LFeedback.Capture.SourceRows > 0),
      'a scoped no-match filter displays filtered-empty and offers Reset rather than reload; kind=' +
      TNyxText(IntToStr(Ord(LFeedback.Capture.Kind))) + ', source=' +
      TNyxText(IntToStr(LFeedback.Capture.SourceRows)) + ', matched=' +
      TNyxText(IntToStr(LFeedback.Capture.MatchedRows)) + ', reset=' +
      TNyxText(BoolToStr(PartVisible(LParts.ResetID), True)) + ', live rows=' +
      TNyxText(IntToStr(LFirst.View.Snapshot.Count)) + ', active=' +
      TNyxText(BoolToStr(LFeedback.Capture.Active, True)));
    LSession := NyxCollectionQuerySession(LFirst.View);
    LPeerOwner := LSession.AcquireOwner(NyxQueryOrigin('feedback-test-peer'), 'Independent filter');
    try
      LPlan := LSession.PrepareSession(LSession.Capture, LPeerOwner,
        NyxQuerySessionEdits.Filter(NyxQueryChannel('peer'),
          NyxWhere(NyxTextField('name')).EqualTo('Another unavailable service')));
      PublishNyxGroup([LPlan]);
      Click(LParts.ResetID);
      Check((LFeedback.Capture.Kind = ncfkFilteredEmpty) and (LFeedback.Capture.SourceRows > 0) and
        not PartVisible(LParts.ResetID),
        'Reset this filter preserves a different surface owner''s accepted predicate');
      LPlan := LSession.PrepareSession(LSession.Capture, LPeerOwner, NyxQuerySessionEdits.ClearOwner);
      PublishNyxGroup([LPlan]);
      Check((LFeedback.Capture.Kind = ncfkReady) and (LFeedback.Capture.PageRows > 0),
        'clearing that peer independently confirms the demo removed only its own predicate');
    finally
      LPeerOwner.Revoke;
    end;
    Click(InspectionStepID);
    Check((LFeedback.Capture.Kind = ncfkEmpty) and PartVisible(LParts.RetryID) and
      not PartVisible(LParts.ResetID) and (LPeer.View.Snapshot.Count = 0),
      'a genuinely empty source differs from a filter and its live peer also sees the empty dataset');
    Click(LParts.RetryID);
    Check((LFeedback.Capture.Kind = ncfkReady) and (LFirst.View.Snapshot.Count = 1) and
      (LPeer.View.Snapshot.Count = 1), 'retrying an empty source loads a real record in both independent views');
    Click(InspectionPauseID);
    LFeedback.Failed('The paused sample producer needs another try.');
    Check((LFeedback.Capture.Kind = ncfkPaused) and PartVisible(LParts.ResumeID) and
      PartVisible(LParts.RetryID), 'pause and producer failure expose separate meaningful Resume and Retry actions');
    Click(LParts.RetryID);
    Check((LFeedback.Capture.Kind = ncfkPaused) and (LFeedback.Capture.PendingPublications > 0),
      'retrying the producer while paused does not silently resume the inspection');
    Click(LParts.ResumeID);
    Check((LFeedback.Capture.Kind = ncfkReady) and not PartVisible(LParts.ResumeID) and
      (LFeedback.Capture.DisplayRevision = LFeedback.Capture.IncomingRevision),
      'the feedback panel''s own Resume action adopts the current source deliberately');
    Check(TNyxCodec.Encode(LDocument) = LSaved,
      'feedback, scoped reset and replay actions never alter the authored design');
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
      LFirst.View, LPeer.View, NyxCollectionPositionState(LFirst), NyxCollectionDraftRecovery(LFirst));
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
    PositionCases;
    FeedbackCases;
    LDemo.Close;
    Check(not LFeedback.Capture.Active, 'retained feedback data loses publication authority when the demo binding closes');
    LOriginal := LFirst.View.Store.Snapshot;
    Click(InspectionStepID);
    Check(LFirst.View.Store.Snapshot = LOriginal, 'closing the binding withdraws actual button authority');
  finally
    LDemo.Free;
    LFeedback := nil;
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

{$ifndef PAS2JS}
{ These visible public controls exercise the legacy grid and native List paths
  separately from the service monitor's arranged/pinned table. The application
  owns widgets; retained observations remain detached after its retirement. }
procedure NativeFlatPositionCases(AWidth: Integer; ATable: Boolean);
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LPage: INyxPage;
  LControl: INyxControl;
  LWidget: TWinControl;
  LMount: INyxCollectionMount;
  LInspection: INyxCollectionInspection;
  LObservation: INyxCollectionPositionObservation;
  LState: INyxCollectionPositionState;
  LFollowing: INyxCollectionFollowing;
  LItems: array of TNyxCollectionItem;
  LKey: TNyxCollectionRef;
  LAnchor: TNyxItemRef;
  LNext: TNyxItemRef;
  LBefore: INyxCollectionSnapshot;
  LIndex: Integer;

  function TopItem: TNyxItemRef;
  var
    LTop: Integer;
  begin

    if ATable then
    begin
      LTop := TStringGrid(LWidget).TopRow - 1;
    end
    else
    begin
      LTop := TListBox(LWidget).TopIndex;
    end;
    Result := LMount.View.Snapshot.ItemAt(LTop).Ref;
  end;

  function LastRowVisible: Boolean;
  var
    LLast: Integer;
  begin

    if ATable then
    begin
      LLast := TStringGrid(LWidget).RowCount - 1;
      Result := (TStringGrid(LWidget).CellRect(0, LLast).Top < LWidget.ClientHeight) and
        (TStringGrid(LWidget).CellRect(0, LLast).Bottom > TStringGrid(LWidget).CellRect(0, 0).Bottom);
    end
    else
    begin
      LLast := TListBox(LWidget).Items.Count - 1;
      Result := (TListBox(LWidget).ItemRect(LLast).Top < LWidget.ClientHeight) and
        (TListBox(LWidget).ItemRect(LLast).Bottom > 0);
    end;
  end;

begin
  LDocument := TNyxDocument.Create;
  LApplication := nil;
  try
    LKey := NyxCollection('flat-inspection');
    SetLength(LItems, 64);
    for LIndex := 0 to High(LItems) do
    begin
      LItems[LIndex] := NyxCollectionItem(NyxItem(LKey, 'service-' + TNyxText(IntToStr(LIndex))))
        .WithValue(NyxTextField('name'), 'Service ' + TNyxText(Format('%.3d', [LIndex])));
    end;
    LDocument.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('name'), ''), LItems);
    LPage := NewNyxPage('flat');
    LDocument.AddPage(LPage);
    LPage.Configure.Padding(16).Done;

    if ATable then
    begin
      LControl := NewNyxTable('flat-target');
    end
    else
    begin
      LControl := NewNyxList('flat-target');
    end;
    LPage.Add(LControl);
    LControl.Configure.Height(180).Done;
    LControl.Binds.Collection(NyxCollectionView(LKey).Column(NyxTextField('name'), 'Service')).Done;
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 300;
    LApplication.Window.Show;
    Application.ProcessMessages;
    LWidget := TWinControl(LApplication.View.ControlFor('flat-target'));
    LMount := LApplication.View.CollectionMount('flat-target');
    LInspection := NyxCollectionInspection(LMount.View);
    LInspection.ConfigureInspection(NyxInspectionPolicy);
    Check(Supports(LMount, INyxCollectionPositionObservation, LObservation) and LWidget.IsVisible,
      'the public flat control is visible with typed recovery observation');
    LState := LObservation.CapturePositionState;
    LMount.Select(LItems[0].Ref);

    if ATable then
    begin
      TStringGrid(LWidget).TopRow := 21;
    end
    else
    begin
      TListBox(LWidget).TopIndex := 20;
    end;
    LAnchor := TopItem;
    LMount.View.Store.Insert(0, NyxCollectionItem(NyxItem(LKey, 'inserted-above'))
      .WithValue(NyxTextField('name'), 'Inserted above'));
    Check(TopItem.ID = LAnchor.ID, 'flat source insertion preserves the actual reading row');
    LBefore := LMount.View.Snapshot;
    LNext := LBefore.ItemAt(LBefore.IndexOf(LAnchor) + 1).Ref;
    LMount.View.Store.Remove(LAnchor);
    Check((TopItem.ID = LNext.ID) and (LObservation.GetPositionRecovery.Kind = ncpFollowing),
      'flat anchor deletion recovers the old next row');
    LAnchor := TopItem;
    LMount.View.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxTextField('name'), nsdDescending));
    Check(TopItem.ID = LAnchor.ID, 'flat query sorting retains the reading identity');
    LInspection.PauseInspection;
    LMount.View.Store.Insert(0, NyxCollectionItem(NyxItem(LKey, 'paused-insert'))
      .WithValue(NyxTextField('name'), 'ZZ inserted while paused'));
    LInspection.ResumeInspection;
    Check(TopItem.ID = LAnchor.ID, 'flat resume preserves the actual reading row');
    LMount.View.ConfigureQuery(Default(TNyxCollectionQuery));
    LInspection.ConfigureInspection(NyxInspectionPolicy.Follow(ncfLastVisible));
    Check(LastRowVisible and (LObservation.GetPositionRecovery.Kind = ncpLatest),
      'a typed initial follow policy reveals the last row in the actual native List/plain grid');
    LBefore := LMount.View.Store.Snapshot;
    LMount.View.Store.Insert(LBefore.Count,
      NyxCollectionItem(NyxItem(LKey, 'followed-tail')).WithValue(NyxTextField('name'), 'New last record'));
    Check(LastRowVisible and (LMount.View.Selection.Focus.ID = LItems[0].Ref.ID),
      'native List/plain grid follows an incoming tail independently of selected-row restoration');
    LFollowing := NyxCollectionFollowing(LMount.View);
    LFollowing.KeepReading;
    LAnchor := LObservation.GetPositionRecovery.Current;
    LApplication.Free;
    LApplication := nil;
    Check(not LMount.Connected and (LObservation.GetPositionRecovery.Current.ID = LAnchor.ID),
      'retained position facts never revive a disposed native target');
    LObservation := nil;
    LInspection := nil;
    LFollowing := nil;
    LMount := nil;
    Check(not LState.Active and (LState.Recovery.Current.ID = LAnchor.ID),
      'the readonly data port survives without its former mount or renderer');
  finally
    LObservation := nil;
    LInspection := nil;
    LFollowing := nil;
    LMount := nil;
    LApplication.Free;
    LDocument.Free;
  end;
end;
{$endif}

begin
  try
    {$ifdef PAS2JS}
    Journey(1280);
    Journey(390);
    document.body.setAttribute('data-live-inspection-checks', IntToStr(GChecks));
    {$else}
    Application.Initialize;
    Journey(StrToInt(ParamStr(1)));
    NativeFlatPositionCases(StrToInt(ParamStr(1)), False);
    NativeFlatPositionCases(StrToInt(ParamStr(1)), True);
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
