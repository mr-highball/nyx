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

program nyx_native_column_activation_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Interfaces, Forms, Controls, StdCtrls, ExtCtrls,
  Grids, Windows, LCLType,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.contract, nyx.codec,
  nyx.state, nyx.events, nyx.scheduler, nyx.behavior, nyx.binding.types,
  nyx.collections, nyx.collections.columns, nyx.collections.arrangement,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.mount,
  nyx.collections.lcl.grid, nyx.render.lcl;

type
  { Each public-renderer scenario owns independent state and physical controls;
    pin/hide setup is separate from the real button's command under test. }
  TNativeActivationCase = (nacClick, nacNoAuthoredAction, nacEnter, nacSpaceRelease,
    nacConsumedKey, nacDisabled, nacSuspended, nacHeldSpaceDisabled,
    nacHeldSpaceSuspended, nacDraft,
    nacPinnedStart, nacPinnedEnd, nacHiddenReset, nacRejected,
    nacNotificationFailure, nacCallbackRetirement);

  { A snapshot receiver borrows this ordinary test owner until its subscription
    is disconnected; the owner outlives every synchronous callback. }
  TNativeActivationReview = class;

  { Named Click and physical key callbacks use the actual router's execution
    response. No callback invokes a mount default or manufactures a generation. }
  TNativeActivationCallback = class(TNyxEventCallback)
  public
    Owner: TNativeActivationReview;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Widgetset callback exceptions become first failures instead of modal dialogs.
    Expected binding rejections stay in the renderer's existing diagnostics. }
  TNativeActivationErrors = class
  public
    Failure: TNyxText;
    procedure Failed(Sender: TObject; E: Exception);
  end;

  { The renderer borrows an externally owned scalar state through all input and
    retirement. Its normal collection binding owns the one real dataset/view. }
  TNativeActivationReview = class
  private
    FKind: TNativeActivationCase;
    FDocument: TNyxDocument;
    FRenderer: TNyxLCLRenderer;
    FHost: TForm;
    FStore: TNyxState;
    FView: INyxCollectionView;
    FColumns: INyxCollectionColumns;
    FMount: INyxCollectionMount;
    FGrid: TNyxCollectionStringGrid;
    FCallback: INyxEventCallback;
    FToken: INyxEventSubscription;
    FKeyToken: INyxEventSubscription;
    FStateToken: TNyxStateSubscription;
    FSemantic: Integer;
    FLegacy: Integer;
    FKeys: Integer;
    FReject: Boolean;
    FFailUpdate: Boolean;
    FAuthored: TNyxText;
    FRows: INyxCollectionSnapshot;
    FRowsRevision: Integer;
    procedure Semantic(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure Legacy(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Validate(AState: TNyxState; AChanges: TNyxStateChanges);
    function Button: TNyxCollectionFitButton;
    function Capture: INyxCollectionColumnsCapture;
    procedure CheckDelivery(ACount: Integer);
    procedure CheckFit(AOldSerial: Integer);
    procedure KeyDown(AButton: TNyxCollectionFitButton; AKey: Word);
    procedure KeyUp(AButton: TNyxCollectionFitButton; AKey: Word);
  public
    constructor Create(AKind: TNativeActivationCase; AWidth: Integer);
    destructor Destroy; override;
    procedure UpdateCaption(ANode: TNyxNode; AControl: TControl);
    procedure Run;
  end;

const
  CTable = 'native-activation-table';
  CCounterCaption = 'activation-counter-caption';
  CCaseNames: array[TNativeActivationCase] of String = (
    'click', 'no-authored-action', 'enter', 'space-release', 'consumed-key', 'disabled',
    'suspended', 'held-space-disabled', 'held-space-suspended', 'draft',
    'pinned-start', 'pinned-end', 'hidden-reset',
    'rejected', 'notification-failure', 'callback-retirement');

var
  GChecks: Integer;
  GErrors: TNativeActivationErrors;
  { Plain native factory callbacks borrow the current independent case only. }
  GReview: TNativeActivationReview;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if GErrors.Failure <> '' then
  begin
    raise Exception.Create('Native callback: ' + GErrors.Failure);
  end;

  if not ACondition then
  begin
    raise Exception.Create('Native column activation: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Drain;
var
  LTurn: Integer;
begin
  { Run the ordinary application idle and async queue in order after physical
    input returns. This neither invokes a publisher directly nor sleeps. }
  for LTurn := 1 to 4 do
  begin
    Application.ProcessMessages;
    Application.Idle(False);
    CheckSynchronize(0);
  end;
end;

function Services: TNyxCollectionRef;
begin
  Result := NyxCollection('native-activation-services');
end;

function ServiceName: TNyxTextFieldRef;
begin
  Result := NyxTextField('service-name');
end;

function Workers: TNyxIntegerFieldRef;
begin
  Result := NyxIntegerField('workers');
end;

function ActivationCount: TNyxIntegerStateRef;
begin
  Result := NyxIntegerState('activation-count');
end;

function ColumnSpec: TNyxCollectionViewSpec;
var
  LPolicy: TNyxColumnLayoutPolicy;
begin
  LPolicy := NyxColumnLayout.AllowOnly([nclfAutoSize, nclfPinning, nclfVisibility]);
  Result := NyxCollectionView(Services)
    .Column(ServiceName, 'Service', cmEditable)
      .ColumnSizing(NyxColumnSizing.Fixed(64).Minimum(40).Maximum(360))
      .ColumnLayout(LPolicy)
    .Column(Workers, 'Workers')
      .ColumnSizing(NyxColumnSizing.Fixed(88).Minimum(40).Maximum(180))
      .ColumnLayout(LPolicy)
    .TableLayout(NyxTableLayout.AllowOnly([nclfAutoSize, nclfPinning, nclfVisibility]));
end;

function Fixture(AKind: TNativeActivationCase): TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
  LCaption: INyxLabel;
begin
  Result := TNyxDocument.Create;
  try
    Result.State.SetValue(ActivationCount, 0)
      .SetValue(NyxBooleanState('table-enabled'), True);
    Result.Collections.Define(Services,
      NyxCollectionSchema.Text(ServiceName, '').Integer(Workers, 0), [
      NyxCollectionItem(NyxItem(Services, 'service-a'))
        .WithValue(ServiceName, 'A deliberately long accepted service / 🌙')
        .WithValue(Workers, 2),
      NyxCollectionItem(NyxItem(Services, 'service-b'))
        .WithValue(ServiceName, 'Worker').WithValue(Workers, 1)]);
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(12).Gap(12).Done;
    LTable := NewNyxTable(CTable);
    LTable.Node.Contract.Value(NyxIntegerDomain);
    LTable.Configure.Height(200).Value(0).Done;

    if AKind <> nacNoAuthoredAction then
    begin
      LTable.Configure.Action(naIncrement)
        .OnClick(NyxEvent('native-column-activated')).Done;
    end;
    LTable.Binds.Value(ActivationCount).Enabled(NyxBooleanState('table-enabled')).Done;
    LTable.Binds.Collection(ColumnSpec);
    LPage.Add(LTable);
    LCaption := NewNyxLabel(CCounterCaption);
    LCaption.Binds.Text(ActivationCount).Done;
    LPage.Add(LCaption);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function CaptionFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TPanel.Create(AOwner);
end;

procedure CaptionUpdate(ANode: TNyxNode; AControl: TControl);
begin
  GReview.UpdateCaption(ANode, AControl);
end;

procedure TNativeActivationErrors.Failed(Sender: TObject; E: Exception);
begin
  Failure := E.Message;
end;

procedure TNativeActivationCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Owner.Semantic(AEvent, AExecution);
end;

constructor TNativeActivationReview.Create(AKind: TNativeActivationCase;
  AWidth: Integer);
var
  LCallback: TNativeActivationCallback;
begin
  inherited Create;
  GReview := Self;
  FKind := AKind;
  FDocument := Fixture(AKind);
  FAuthored := TNyxCodec.Encode(FDocument);
  FStore := FDocument.State.Clone;
  FHost := TForm.CreateNew(nil);
  FHost.ClientWidth := AWidth;
  FHost.ClientHeight := 800;
  FRenderer := TNyxLCLRenderer.Create;
  FRenderer.RegisterFactory('label', CaptionFactory, CaptionUpdate);
  FRenderer.OnEvent := Legacy;
  FRenderer.Render(FDocument, FDocument.Pages[0], FHost, False, FStore);
  FHost.Show;
  Drain;
  Check(FHost.ClientWidth = AWidth, 'actual native allocation matches the requested width');
  FGrid := TNyxCollectionStringGrid(FRenderer.ControlFor(CTable));
  FView := FRenderer.CollectionView(CTable);
  FColumns := NyxCollectionColumns(FView);
  FMount := FRenderer.CollectionMount(CTable);
  FRows := FView.Snapshot;
  FRowsRevision := FView.Store.Snapshot.Revision;
  LCallback := TNativeActivationCallback.Create;
  LCallback.Owner := Self;
  FCallback := LCallback;

  if AKind <> nacNoAuthoredAction then
  begin
    FToken := FRenderer.Events.OnNamed(NyxControlEvents(CTable),
      NyxEvent('native-column-activated')).Subscribe(FCallback);
    FKeyToken := FRenderer.Events.OnKeyDown(NyxControlEvents(CTable)).Subscribe(FCallback);
  end
  else
  begin
    FRenderer.OnEvent := nil;
  end;
  FStateToken := FStore.Subscribe(nil, Validate);
end;

destructor TNativeActivationReview.Destroy;
begin

  if FToken <> nil then
  begin
    FToken.Cancel;
  end;

  if FKeyToken <> nil then
  begin
    FKeyToken.Cancel;
  end;

  if FStateToken <> nil then
  begin
    FStateToken.Disconnect;
  end;
  FToken := nil;
  FKeyToken := nil;
  FreeAndNil(FStateToken);
  FCallback := nil;
  FMount := nil;
  FColumns := nil;
  FView := nil;
  FRows := nil;
  FRenderer.Free;
  Drain;
  FStore.Free;
  FDocument.Free;
  FHost.Free;
  inherited Destroy;
end;

procedure TNativeActivationReview.UpdateCaption(ANode: TNyxNode; AControl: TControl);
begin

  if FFailUpdate then
  begin
    raise Exception.Create('Intentional committed native activation notification failure');
  end;
  TPanel(AControl).Caption := ANode.Prop('text');
end;

procedure TNativeActivationReview.Validate(AState: TNyxState;
  AChanges: TNyxStateChanges);
begin

  if FReject then
  begin
    raise Exception.Create('Intentional native activation rejection');
  end;
end;

procedure TNativeActivationReview.Semantic(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LResponse: INyxEventResponse;
begin
  Check((AEvent.OriginID = CTable) and (AEvent.SourceID = CTable),
    'native action preserves the exact canonical table identity');

  if AEvent.Trigger = ntKeyDown then
  begin
    Inc(FKeys);

    if FKind = nacConsumedKey then
    begin
      Check(Supports(AExecution, INyxEventResponse, LResponse) and
        LResponse.CanConsume, 'real native key callback owns synchronous consumption');
      LResponse.Consume;
    end;
    Exit;
  end;
  Check((AEvent.Trigger = ntClick) and (AEvent.ValueKind = nskInteger) and
    (AEvent.Value.AsInteger = FStore.GetValue(ActivationCount)),
    'semantic native Click observes its actual committed integer action');
  Inc(FSemantic);

  if FKind = nacCallbackRetirement then
  begin
    FRenderer.Unmount;
  end;
end;

procedure TNativeActivationReview.Legacy(ANode: TNyxNode;
  const AEvent: TNyxEventInfo);
begin
  Check((ANode.ID = CTable) and (AEvent.Trigger = ntClick),
    'legacy native Click retains the same canonical source');
  Inc(FLegacy);
end;

function TNativeActivationReview.Button: TNyxCollectionFitButton;
var
  LIndex: Integer;
  LCandidate: TComponent;
begin
  Result := nil;
  for LIndex := 0 to FGrid.ComponentCount - 1 do
  begin
    LCandidate := FGrid.Components[LIndex];

    if (LCandidate is TNyxCollectionFitButton) and
      TNyxCollectionFitButton(LCandidate).Field.Same(NyxColumnIdentity(ServiceName)) then
    begin
      Exit(TNyxCollectionFitButton(LCandidate));
    end;
  end;
end;

function TNativeActivationReview.Capture: INyxCollectionColumnsCapture;
begin
  Result := FColumns.CaptureColumns;
end;

procedure TNativeActivationReview.CheckDelivery(ACount: Integer);
begin
  Check(FStore.GetValue(ActivationCount) = ACount,
    'native authored action executes exactly once per admitted activation');
  Check((FSemantic = ACount) and (FLegacy = ACount),
    'native semantic and legacy callbacks each deliver exactly once');
end;

procedure TNativeActivationReview.CheckFit(AOldSerial: Integer);
var
  LCapture: INyxCollectionColumnsCapture;
  LIndex: Integer;
begin
  LCapture := Capture;
  LIndex := LCapture.Preferences.IndexOf(NyxColumnIdentity(ServiceName));
  Check(LIndex >= 0, 'native single activation publishes an actual width preference');
  Check(LCapture.Preferences.ColumnAt(LIndex).HasWidth and
    (LCapture.Preferences.ColumnAt(LIndex).WidthValue > 64) and
    (LCapture.Preferences.ColumnAt(LIndex).WidthValue <= 360) and
    (LCapture.Serial = AOldSerial + 1),
    'native Fit uses accepted text and separate AutoSize permission within bounds');
end;

procedure TNativeActivationReview.KeyDown(AButton: TNyxCollectionFitButton; AKey: Word);
begin
  SendMessage(AButton.Handle, WM_KEYDOWN, AKey, 0);
end;

procedure TNativeActivationReview.KeyUp(AButton: TNyxCollectionFitButton; AKey: Word);
begin
  SendMessage(AButton.Handle, WM_KEYUP, AKey, 0);
end;

procedure TNativeActivationReview.Run;
var
  LButton: TNyxCollectionFitButton;
  LBefore: INyxCollectionColumnsCapture;
  LEditMount: INyxCollectionEditMount;
  LEdits: INyxCollectionEditsSnapshot;
  LEditor: TCustomEdit;
  LPreferences: TNyxColumnPreferences;
  LKey: Word;
begin
  LButton := Button;
  Check((LButton <> nil) and LButton.Visible and LButton.Enabled and LButton.TabStop,
    'AutoSize-only policy exposes a real enabled tab-reachable native button');
  Check(LButton.AccessibleName = 'Fit Service',
    'native button preserves its complete semantic action name');
  Check(not Capture.Spec.TableLayoutPolicy.Allows(nclfResize) and
    Capture.Spec.TableLayoutPolicy.Allows(nclfAutoSize),
    'native table denies Resize while separately allowing AutoSize');

  if FKind in [nacPinnedStart, nacPinnedEnd, nacHiddenReset] then
  begin
    LPreferences := NyxColumnPreferences;
    case FKind of
      nacPinnedStart:
        LPreferences := LPreferences.WithColumn(NyxColumnPreference(
          NyxColumnIdentity(ServiceName)).Pinned(ncpStart));
      nacPinnedEnd:
        LPreferences := LPreferences.WithColumn(NyxColumnPreference(
          NyxColumnIdentity(ServiceName)).Pinned(ncpEnd));
      nacHiddenReset:
        LPreferences := LPreferences.WithColumn(NyxColumnPreference(
          NyxColumnIdentity(ServiceName)).Visible(False));
    else
      raise Exception.Create('Native activation setup requires an explicit case');
    end;
    FColumns.ConfigureColumns(LPreferences);
    Drain;

    if FKind = nacHiddenReset then
    begin
      Check((Button = LButton) and not LButton.Visible,
        'hiding a typed field hides its exact button without reinterpreting identity');
      LButton.Click;
      Drain;
      CheckDelivery(0);
      FColumns.ConfigureColumns(NyxColumnPreferences);
      Drain;
      Check((Button = LButton) and LButton.Visible,
        'reset restores the same typed action without stale command authority');
    end
    else
    begin
      Check((Button = LButton) and (LButton.Parent <> FGrid) and
        FGrid.OwnsColumnInput(LButton),
        'both logical pin edges retain one real registered button and input owner');
    end;
  end;
  LBefore := Capture;
  case FKind of
    nacClick, nacPinnedStart, nacPinnedEnd, nacHiddenReset:
      begin
        LButton.Click;
        Drain;
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
      end;
    nacNoAuthoredAction:
      begin
        LButton.Click;
        Drain;
        CheckDelivery(0);
        CheckFit(LBefore.Serial);
        Check(FRenderer.LastBindingFailure = nbfNone,
          'accepted empty canonical action is distinct from a rejected command');
      end;
    nacEnter, nacSpaceRelease, nacConsumedKey:
      begin
        LButton.SetFocus;
        LKey := VK_RETURN;

        if FKind = nacSpaceRelease then
        begin
          LKey := VK_SPACE;
        end;
        KeyDown(LButton, LKey);
        Drain;
        CheckDelivery(0);
        Check(Capture.Serial = LBefore.Serial, 'native button waits for actual key release');
        KeyUp(LButton, LKey);
        Drain;
        Check(FKeys = 1, 'ordinary native key phases are delivered once');

        if FKind = nacConsumedKey then
        begin
          CheckDelivery(0);
          Check(Capture.Serial = LBefore.Serial, 'consumed native key cannot publish Fit');
        end
        else
        begin
          CheckDelivery(1);
          CheckFit(LBefore.Serial);
        end;
      end;
    nacDisabled:
      begin
        FStore.SetValue(NyxBooleanState('table-enabled'), False);
        Drain;
        LButton.Click;
        Drain;
        CheckDelivery(0);
        Check(Capture.Serial = LBefore.Serial, 'disabled button admits neither action nor Fit');
        FStore.SetValue(NyxBooleanState('table-enabled'), True);
        Drain;
        Button.Click;
        Drain;
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
      end;
    nacSuspended:
      begin
        Check(Supports(FMount, INyxCollectionEditMount, LEditMount),
          'native renderer mount exposes its existing reversible suspension');
        LEdits := LEditMount.SuspendCellEdits;
        try
          LButton.Click;
          KeyDown(LButton, VK_RETURN);
          KeyUp(LButton, VK_RETURN);
          Drain;
          CheckDelivery(0);
          Check((FKeys = 0) and (Capture.Serial = LBefore.Serial),
            'initially suspended native input invokes neither canonical callbacks nor Fit');
        finally
          LEditMount.ResumeCellEdits(LEdits, False);
        end;
        LEdits := nil;
        Drain;
        Button.Click;
        Drain;
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
      end;
    nacHeldSpaceDisabled, nacHeldSpaceSuspended:
      begin
        LButton.SetFocus;
        KeyDown(LButton, VK_SPACE);
        Drain;
        CheckDelivery(0);
        Check(Capture.Serial = LBefore.Serial,
          'the original Space press has not activated before its availability changes');

        if FKind = nacHeldSpaceDisabled then
        begin
          FStore.SetValue(NyxBooleanState('table-enabled'), False);
          FStore.SetValue(NyxBooleanState('table-enabled'), True);
        end
        else
        begin
          Check(Supports(FMount, INyxCollectionEditMount, LEditMount),
            'held native Space uses the actual existing suspension facet');
          LEdits := LEditMount.SuspendCellEdits;
          try
            CheckDelivery(0);
          finally
            LEditMount.ResumeCellEdits(LEdits, False);
          end;
          LEdits := nil;
        end;
        Drain;
        Check((Button = LButton) and LButton.Enabled,
          'availability returns on the exact same native button, not a replacement');
        KeyUp(LButton, VK_SPACE);
        Drain;
        CheckDelivery(0);
        Check(Capture.Serial = LBefore.Serial,
          'an old Space release after disable or suspension cannot regain Click or Fit authority');
        LButton.Click;
        Drain;
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
      end;
    nacDraft:
      begin
        FGrid.Col := 0;
        FGrid.Row := 1;
        FGrid.SetFocus;
        SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 0);
        Drain;
        Check(FGrid.EditorMode and (FGrid.Editor is TCustomEdit),
          'real native F2 owns the actual active data editor');
        LEditor := TCustomEdit(FGrid.Editor);
        LEditor.Text := TNyxText('Unfinished native / 🌙');
        LEditor.SelStart := 2;
        LEditor.SelLength := 5;
        LButton.SetFocus;
        Check(FView.Store.Snapshot.Revision = FRowsRevision,
          'focus on the native layout button never implicitly commits the cell draft');
        KeyDown(LButton, VK_RETURN);
        KeyUp(LButton, VK_RETURN);
        Drain;
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
        Check((FView.Store.Snapshot.Revision = FRowsRevision) and
          (TNyxText(LEditor.Text) = TNyxText('Unfinished native / 🌙')) and
          (LEditor.SelStart = 2) and (LEditor.SelLength = 5),
          'native Fit preserves the unfinished scalar text and exact caret');
      end;
    nacRejected:
      begin
        FReject := True;
        LButton.Click;
        FReject := False;
        Drain;
        CheckDelivery(0);
        Check((Capture.Serial = LBefore.Serial) and
          (FRenderer.LastBindingFailure = nbfRejected),
          'rejected real authored action does not fall through to Fit');
      end;
    nacNotificationFailure:
      begin
        FFailUpdate := True;
        LButton.Click;
        FFailUpdate := False;
        Drain;
        Check((FStore.GetValue(ActivationCount) = 1) and (FSemantic = 0) and (FLegacy = 0),
          'notification failure keeps the real action committed without semantic replay');
        Check((Capture.Serial = LBefore.Serial) and
          (FRenderer.LastBindingFailure = nbfNotificationFailed),
          'committed notification failure denies the later Fit without rollback');
      end;
    nacCallbackRetirement:
      begin
        LButton.Click;
        Drain;
        Check((FStore.GetValue(ActivationCount) = 1) and (FSemantic = 1) and (FLegacy = 0),
          'native semantic Unmount preserves its one committed action and suppresses stale legacy');
        Check((FRenderer.Root = nil) and not FMount.Connected and
          (Capture.Serial = LBefore.Serial),
          'retired native button cannot publish through its retained original view');
      end;
  end;
  Check((FView.Snapshot = FRows) and (FView.Store.Snapshot.Revision = FRowsRevision),
    'layout activation preserves the accepted collection data and revision');
  Check(TNyxCodec.Encode(FDocument) = FAuthored,
    'native activation preserves exact authored design and default state');
end;

procedure Run(AWidth: Integer);
var
  LKind: TNativeActivationCase;
  LReview: TNativeActivationReview;
begin
  for LKind := Low(TNativeActivationCase) to High(TNativeActivationCase) do
  begin
    WriteLn('CASE ', CCaseNames[LKind]);
    LReview := TNativeActivationReview.Create(LKind, AWidth);
    GReview := LReview;
    try
      LReview.Run;
    finally
      LReview.Free;
      GReview := nil;
    end;
  end;
end;

var
  LWidth: Integer;
begin
  Application.Initialize;
  GErrors := TNativeActivationErrors.Create;
  Application.OnException := GErrors.Failed;
  try
    try
      LWidth := 1280;

      if ParamCount > 0 then
      begin
        LWidth := StrToInt(ParamStr(1));
      end;
      Run(LWidth);
      WriteLn('PASS ', GChecks, ' actual native column activation checks at ', LWidth);
    except
      on E: Exception do
      begin
        WriteLn('FAIL ', E.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
      end;
    end;
  finally
    Application.OnException := nil;
    GErrors.Free;
  end;
end.
