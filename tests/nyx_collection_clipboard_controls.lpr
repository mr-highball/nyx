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
program nyx_collection_clipboard_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.state, nyx.model, nyx.controls,
  nyx.codec, nyx.behavior, nyx.events, nyx.scheduler, nyx.clipboard, nyx.editing,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.selection, nyx.collections.query, nyx.collections.arrangement,
  nyx.collections.mount, nyx.collections.clipboard, nyx.collections.clipboard.actions
  , nyx.collections.cell.actions, nyx.menu, nyx.popover, nyx.bytes,
  nyx.files.save, nyx.collections.paging, nyx.collections.export,
  nyx.collections.export.actions
  , nyx.collections.representation, nyx.collections.draft,
  nyx.collections.range, nyx.collections.range.actions, nyx.collections.range.commands,
  nyx.formatting, nyx.times, nyx.numbers.decimal, nyx.contract
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.collections.cell.browser,
    nyx.menu.browser, nyx.popover.browser, nyx.editing.browser
  {$else}, Interfaces, Forms, Controls, Grids, StdCtrls, Windows, LCLType, Types,
    nyx.collections.lcl.grid, nyx.render.lcl, nyx.editing.lcl,
    nyx.collections.cell.lcl, nyx.menu.lcl, nyx.popover.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Standard browser KeyboardEvent constructor receives only a copied event
    initializer. It is a synthetic fixture producer, not trusted OS input. }
  TCopyKeyEvent = class external name 'KeyboardEvent'(TJSKeyboardEvent)
  public
    constructor new(const AType: String; AInit: TJSObject); reintroduce;
  end;
  { Standard cancellable context event enters the real renderer/table route.
    It proves semantic/default integration, not a trusted hardware pointer. }
  TCellContextEvent = class external name 'MouseEvent'(TJSMouseEvent)
  public
    constructor new(const AType: String; AInit: TJSObject); reintroduce;
  end;
  {$endif}

  { Each callback mode changes only the original real key's semantic phase.
    Newer copy is a separate typed command; no physical event is replayed. }
  TKeyMode = (kmObserve, kmConsume, kmMutate, kmDisableABA, kmNested, kmRetire);

  { Test owner outlives every subscription and injected host receiver. }
  TCopyReview = class;

  { Delegates every ordinary scheduler operation. A one-shot UI admission
    callback enters a real nested context after the actions owner captured its
    original intent, exercising stale target return without private-field edits. }
  TContextScheduler = class(TInterfacedObject, INyxScheduler)
  public
    Base: INyxScheduler;
    Owner: TCopyReview;
    Reentries: Integer;
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork;
      APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork;
      const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;

  { An alternate public input factory delegates the complete RAW command to
    the actual mounted owner. Before returning that command it enters one real
    nested context message, modeling a reentrant input/source getter. Owner is
    borrowed only while this same fixture retains the proxy; no queue owns it. }
  TContextCommands = class(TInterfacedObject, INyxCollectionClipboardCommands,
    INyxCollectionClipboardCellInput)
  public
    Owner: TCopyReview;
    Commands: INyxCollectionClipboardCommands;
    Reentries: Integer;
    function BeginCellInput: INyxCollectionClipboardCellCommand;
    function BeginCell(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity): INyxCollectionClipboardCommand;
    function BeginRows(AMembership: TNyxCollectionCopyMembership): INyxCollectionClipboardCommand;
    procedure Observe(AObserver: TNyxCollectionClipboardObserver);
    procedure CancelPending;
    procedure Revoke;
    function GetPhase: TNyxCollectionClipboardPhase;
    function GetLastResult: TNyxClipboardWriteResult;
    function GetLastPayload: INyxCollectionClipboardText;
  end;

  { Actual router callback is retained by its subscription until cancellation.
    Its owner is borrowed and never enters a clipboard request or payload. }
  TKeyCallback = class(TNyxEventCallback)
  public
    Owner: TCopyReview;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Real shared terminal request writes only a copied fixture counter/text. }
  TWrite = class(TNyxClipboardWriteRequest)
  public
    procedure Finish;
  end;

  { Injected authority performs no native/browser clipboard API operation. }
  THost = class(TInterfacedObject, INyxClipboardWriteHost)
  public
    Writes: Integer;
    Text: TNyxText;
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

  { Real managed file request reports into an injected counter only. No picker,
    download, filesystem or clipboard adapter is installed by this consumer. }
  TSave = class(TNyxFileSaveRequest)
  public
    procedure Finish(AOutcome: TNyxFileSaveOutcome);
  end;

  { File authority captures exact bytes and optionally delays completion so
    actual mounted disable/retirement can revoke the original receiver. }
  TSaveHost = class(TInterfacedObject, INyxFileSaveHost)
  public
    Saves: Integer;
    Bytes: TNyxBytes;
    InlineReply: Boolean;
    Outcome: TNyxFileSaveOutcome;
    Last: TSave;
    LastLease: INyxFileSaveRequest;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

  {$ifndef PAS2JS}
  { Exposes LCL's protected ordinary context callback for installing the
    no-effect fallback sentinel on the SAME real editor. No control is created
    or replaced, and private widgetset state is never accessed. }
  TEditorContextAccess = class(TCustomEdit)
  public
    property OnContextPopup;
  end;

  { Keep the actual Nyx native grid/input implementation. Its last-resort LCL
    grid clipboard default is replaced only by a no-effect sentinel, so even a
    failing copy-interception assertion cannot mutate the user's clipboard.
    The editor case targets this grid-owned key route with a real active editor
    and an explicitly consuming canonical callback, not an OS editor shortcut. }
  TNoClipboardGrid = class(TNyxCollectionStringGrid)
  protected
    procedure DoCopyToClipboard; override;
  end;

  { Native callback failures are captured instead of opening a modal dialog. }
  TNativeErrors = class
  public
    Text: TNyxText;
    procedure Failed(Sender: TObject; E: Exception);
  end;
  {$endif}

  { One real renderer/data control owns all cases. Runtime data is independent
    from authored source; the host/payload are injected through public facets. }
  TCopyReview = class
  private
    FDocument: TNyxDocument;
    FState: TNyxState;
    FAuthored: TNyxText;
    FView: INyxCollectionView;
    FMount: INyxCollectionMount;
    FClipboard: INyxCollectionClipboardMount;
    FCommands: INyxCollectionClipboardCommands;
    FInput: INyxCollectionInputLifetime;
    FHost: THost;
    FHostLease: INyxClipboardWriteHost;
    FCallback: INyxEventCallback;
    FToken: INyxEventSubscription;
    FContextToken: INyxEventSubscription;
    FContextScheduler: TContextScheduler;
    FContextSchedulerLease: INyxScheduler;
    FCellMount: INyxCollectionCellActionsMount;
    FCellActions: INyxCollectionCellActions;
    FExport: INyxCollectionExportMount;
    FSaveHost: TSaveHost;
    FSaveHostLease: INyxFileSaveHost;
    FMode: TKeyMode;
    FKeys: Integer;
    FConsumed: Integer;
    FContexts: Integer;
    FInspectorLegacyClicks: Integer;
    FReplies: Integer;
    FLastPayload: INyxCollectionClipboardText;
    FContextColumn: Integer;
    FLastDraft: INyxCollectionFocusedDraftCapture;
    {$ifdef PAS2JS}
    FRenderer: TNyxBrowserRenderer;
    FSurface: TJSHTMLElement;
    FTable: TJSHTMLElement;
    function Cell: TJSHTMLElement;
    function Key(AElement: TJSHTMLElement; const AKey: String;
      AControl: Boolean): Boolean;
    {$else}
    FRenderer: TNyxLCLRenderer;
    FSurface: TForm;
    FGrid: TNoClipboardGrid;
    FEditorContexts: Integer;
    procedure PreviousEditorContext(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    {$endif}
    procedure FocusCell;
    function CopyKey: Boolean;
    procedure EditValue(const AText: TNyxText);
    procedure Filter(AEnabled: Boolean);
    procedure Context(AWait: Boolean = True);
    procedure MenuAction(const AID: TNyxText);
    procedure InspectorAction(const AID: TNyxText);
    procedure RunContext; {$ifdef PAS2JS}async;{$endif}
    procedure RunExport; {$ifdef PAS2JS}async;{$endif}
    procedure RunDisplayed; {$ifdef PAS2JS}async;{$endif}
    procedure RunFocusedDraft; {$ifdef PAS2JS}async;{$endif}
    procedure RunDraftActions; {$ifdef PAS2JS}async;{$endif}
    procedure RunCellRange; {$ifdef PAS2JS}async;{$endif}
    procedure ExportPaging(AEnabled: Boolean);
    procedure Semantic(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure CloseInspectorOnClick(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Reply(const APayload: INyxCollectionClipboardText;
      const AResult: TNyxClipboardWriteResult);
  public
    constructor Create(AArranged: Boolean = True; ADraftActions: Boolean = False);
    destructor Destroy; override;
    procedure Run; {$ifdef PAS2JS}async;{$endif}
  end;

const
  CTable = 'clipboard-table';
  CText: TNyxText = 'accepted 🌙' + #0 + 'value' + #13#10 + 'next';
  CDraft: TNyxText = 'unfinished editor 🌙';

var
  GChecks: Integer;
  {$ifndef PAS2JS}
  GFallbackCopies: Integer;
  GErrors: TNativeErrors;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if (GErrors <> nil) and (GErrors.Text <> '') then
  begin
    raise Exception.Create('Native callback: ' + GErrors.Text);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{$ifdef PAS2JS}
{ Await actual rendering opportunities for the existing mounted row/window
  publisher; these are not replacement input or publication callbacks. }
function Frame: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.requestAnimationFrame(procedure(ATime: Double)
        begin
          AResolve(JSValue(True));
        end);
    end);
end;
{$endif}

procedure Drain; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}
var
  LTurn: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  await(Frame);
  await(Frame);
  await(Frame);
  {$else}
  for LTurn := 1 to 4 do
  begin
    Application.ProcessMessages;
    Application.Idle(False);
    CheckSynchronize(0);
  end;
  {$endif}
end;

function Dataset: TNyxCollectionRef;
begin
  Result := NyxCollection('clipboard-control-data');
end;

function ItemA: TNyxItemRef;
begin
  Result := NyxItem(Dataset, 'a');
end;

function ItemB: TNyxItemRef;
begin
  Result := NyxItem(Dataset, 'b');
end;

function NameField: TNyxColumnIdentity;
begin
  Result := NyxColumnIdentity(NyxTextField('Name'));
end;

function Fixture(AArranged: Boolean): TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
  LSpec: TNyxCollectionViewSpec;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(Dataset,
      NyxCollectionSchema.Text(NyxTextField('Name'), '').Integer(NyxIntegerField('Score'), 0)
        .Number(NyxNumberField('Huge'), 0).Number(NyxNumberField('Exact'), 0)
        .Text(NyxTextField('Clock'), '', NyxTimeDomain), [
        NyxCollectionItem(ItemA).WithValue(NyxTextField('Name'), CText)
          .WithValue(NyxIntegerField('Score'), 1)
          .WithValue(NyxNumberField('Huge'), 1E100)
          .WithValue(NyxNumberField('Exact'), 1.125)
          .WithValue(NyxTextField('Clock'), '09:10:11'),
        NyxCollectionItem(ItemB).WithValue(NyxTextField('Name'), 'B')
          .WithValue(NyxIntegerField('Score'), 2)
          .WithValue(NyxTextField('Clock'), '00:00:00')]);
    LPage := NewNyxPage('copy-page');
    LPage.Configure.Padding(12).Gap(8).Done;
    LTable := NewNyxTable(CTable);
    LTable.Configure.Height(240).Done;
    LSpec := NyxCollectionView(Dataset).Selection(nsmMultiple)
      .Column(NyxTextField('Name'), 'Name', cmEditable)
        .ColumnFormat(NyxValueFormat.TextAffixes('«', '»').Identified(NyxFormatter('full cell'), 3))
      .Column(NyxIntegerField('Score'), 'Score', cmEditable)
        .ColumnFormat(NyxValueFormat.Decimal(2).Localized(
          NyxFormatLocale.NumberSeparators(',', '.')))
      .Column(NyxNumberField('Huge'), 'Huge').ColumnFormat(NyxValueFormat.Decimal(2))
      .Column(NyxNumberField('Exact'), 'Exact').ColumnFormat(NyxValueFormat.Decimal(2, ndrExact))
      .Column(NyxTextField('Clock'), 'Clock').ColumnFormat(NyxValueFormat.ClockTime(ntpMinute))
      .TableLayout(NyxTableLayout);

    if not AArranged then
    begin
      LSpec := LSpec.UseDefaultTableLayout;
    end;
    LTable.Binds.Collection(LSpec);
    LPage.Add(LTable);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure TKeyCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Owner.Semantic(AEvent, AExecution);
end;

function TContextScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := Base.Capabilities;
end;

procedure TContextScheduler.RequireUI;
var
  LOwner: TCopyReview;
begin
  Base.RequireUI;
  LOwner := Owner;
  Owner := nil;

  if LOwner <> nil then
  begin
    Inc(Reentries);
    LOwner.Context(False);
  end;
end;

procedure TContextScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin
  Base.Admit(APolicy);
end;

function TContextScheduler.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := Base.Submit(AWork, APolicy);
end;

function TContextScheduler.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
begin
  Result := Base.PostUI(AWork, AParent);
end;

procedure TContextScheduler.Shutdown;
begin
  Base.Shutdown;
end;

function TContextCommands.BeginCellInput: INyxCollectionClipboardCellCommand;
var
  LEntry: INyxCollectionClipboardCellInput;
  LOwner: TCopyReview;
begin

  if not Supports(Commands, INyxCollectionClipboardCellInput, LEntry) then
  begin
    raise Exception.Create('Actual mounted commands require original cell reservation');
  end;
  Result := LEntry.BeginCellInput;
  LOwner := Owner;
  Owner := nil;

  if LOwner <> nil then
  begin
    Inc(Reentries);
    LOwner.Context(False);
  end;
end;

function TContextCommands.BeginCell(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity): INyxCollectionClipboardCommand;
begin
  Result := Commands.BeginCell(AItem, AField);
end;

function TContextCommands.BeginRows(AMembership: TNyxCollectionCopyMembership):
  INyxCollectionClipboardCommand;
begin
  Result := Commands.BeginRows(AMembership);
end;

procedure TContextCommands.Observe(AObserver: TNyxCollectionClipboardObserver);
begin
  Commands.Observe(AObserver);
end;

procedure TContextCommands.CancelPending;
begin
  Commands.CancelPending;
end;

procedure TContextCommands.Revoke;
begin
  Commands.Revoke;
end;

function TContextCommands.GetPhase: TNyxCollectionClipboardPhase;
begin
  Result := Commands.Phase;
end;

function TContextCommands.GetLastResult: TNyxClipboardWriteResult;
begin
  Result := Commands.LastResult;
end;

function TContextCommands.GetLastPayload: INyxCollectionClipboardText;
begin
  Result := Commands.LastPayload;
end;

procedure TWrite.Finish;
begin
  Complete(ncwoWritten, FByteCount, '', nctrExactUTF8);
end;

function THost.BeginWriteText(const AText: TNyxText;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LWrite: TWrite;
begin
  LWrite := TWrite.Create(AText, AScheduler, ACancellation, AReply);
  Result := LWrite;
  Inc(Writes);
  Text := AText;
  LWrite.Finish;
end;

procedure TSave.Finish(AOutcome: TNyxFileSaveOutcome);
begin

  if AOutcome in [nfsoWritten, nfsoDownloadRequested] then
  begin
    Complete(AOutcome, 'captured.tsv', Length(FBytes));
  end
  else
  begin
    Complete(AOutcome, '', 0);
  end;
end;

function TSaveHost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LSave: TSave;
begin
  LSave := TSave.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := LSave;
  Inc(Saves);
  Bytes := Copy(ABytes, 0, Length(ABytes));
  Last := LSave;
  LastLease := Result;

  if InlineReply then
  begin
    LSave.Finish(Outcome);
  end;
end;

{$ifndef PAS2JS}
procedure TNoClipboardGrid.DoCopyToClipboard;
begin
  Inc(GFallbackCopies);
end;

function GridFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TNoClipboardGrid.Create(AOwner);
end;

procedure TNativeErrors.Failed(Sender: TObject; E: Exception);
begin

  if Text = '' then
  begin
    Text := TNyxText(E.Message);
    WriteLn('Native callback failure after ', GChecks, ' checks: ', E.Message);
    DumpExceptionBackTrace(Output);
  end;
end;

procedure TCopyReview.PreviousEditorContext(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  Inc(FEditorContexts);
  { This sentinel stops the inherited physical menu even in a failing oracle;
    it observes actual fallback without opening an OS menu or clipboard host. }
  Handled := True;
end;
{$endif}

constructor TCopyReview.Create(AArranged: Boolean; ADraftActions: Boolean);
var
  LCallback: TKeyCallback;
  LAdmission: INyxCollectionInputAdmission;
  LOptionsMount: INyxCollectionCellActionOptionsMount;
  LFactory: INyxCollectionCellPresenterFactory;
begin
  inherited Create;
  FDocument := Fixture(AArranged);
  FAuthored := TNyxCodec.Encode(FDocument);
  FState := FDocument.State.Clone;
  {$ifdef PAS2JS}
  FSurface := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(FSurface);
  FRenderer := TNyxBrowserRenderer.Create;
  {$else}
  FSurface := TForm.CreateNew(nil);
  FSurface.ClientWidth := 900;
  FSurface.ClientHeight := 560;
  FRenderer := TNyxLCLRenderer.Create;
  FRenderer.RegisterFactory('table', GridFactory);
  {$endif}
  FRenderer.Render(FDocument, FDocument.Pages[0], FSurface, False, FState);
  {$ifdef PAS2JS}
  FTable := FRenderer.ElementFor(CTable);
  {$else}
  FSurface.Show;
  FGrid := TNoClipboardGrid(FRenderer.ControlFor(CTable));
  {$endif}
  {$ifndef PAS2JS}
  Drain;
  {$endif}
  FView := FRenderer.CollectionView(CTable);
  FMount := FRenderer.CollectionMount(CTable);
  Check(Supports(FMount, INyxCollectionClipboardMount, FClipboard) and
    Supports(FMount, INyxCollectionInputAdmission, LAdmission),
    'actual projected table exposes typed copy and original input');
  FInput := LAdmission.GetInputLifetime;
  FHost := THost.Create;
  FHostLease := FHost;
  FClipboard.ConfigureClipboard(FHostLease, FRenderer.Events,
    FRenderer.Events.Scheduler, TNyxCollectionClipboardOptions.Defaults.AllowOnly(
      [nccaCell, nccaSelectedSource, nccaSelectedPage, nccaCellRange]));
  FCommands := FClipboard.ClipboardCommands;
  FCommands.Observe({$ifdef PAS2JS}@{$endif}Reply);
  Check(Supports(FMount, INyxCollectionExportMount, FExport),
    'actual table exposes optional runtime RAW export authority');
  FSaveHost := TSaveHost.Create;
  FSaveHostLease := FSaveHost;
  FSaveHost.InlineReply := True;
  FSaveHost.Outcome := nfsoWritten;
  FExport.ConfigureExport(FSaveHostLease, FRenderer.Events,
    FRenderer.Events.Scheduler, TNyxCollectionExportOptions.Defaults);
  Check(Supports(FMount, INyxCollectionCellActionsMount, FCellMount),
    'actual mounted table exposes managed cell context actions');
  LFactory := {$ifdef PAS2JS}NewNyxBrowserCollectionCellPresenterFactory(FTable)
    {$else}NewNyxLCLCollectionCellPresenterFactory(FGrid){$endif};

  if ADraftActions then
  begin
    Check(Supports(FMount, INyxCollectionCellActionOptionsMount, LOptionsMount),
      'focused draft actions opt in through the optional typed mount configuration');
    FContextScheduler := TContextScheduler.Create;
    FContextSchedulerLease := FContextScheduler;
    FContextScheduler.Base := FRenderer.Events.Scheduler;
    LOptionsMount.ConfigureCellActionsWithOptions(LFactory, FContextSchedulerLease,
      TNyxCollectionCellActionOptions.Defaults.FocusedDraft(True));
  end
  else
  begin
    FCellMount.ConfigureCellActions(LFactory, FRenderer.Events.Scheduler);
  end;
  FCellActions := FCellMount.CellActions;
  LCallback := TKeyCallback.Create;
  LCallback.Owner := Self;
  FCallback := LCallback;
  FToken := FRenderer.Events.OnKeyDown(NyxControlEvents(CTable, niRuntime)).Subscribe(FCallback);
  FContextToken := FRenderer.Events.OnContextMenu(NyxControlEvents(CTable, niRuntime)).Subscribe(FCallback);
end;

destructor TCopyReview.Destroy;
begin

  if FContextScheduler <> nil then
  begin
    FContextScheduler.Owner := nil;
  end;

  if FContextToken <> nil then
  begin
    FContextToken.Cancel;
  end;
  FContextToken := nil;

  if FToken <> nil then
  begin
    FToken.Cancel;
  end;
  FToken := nil;
  FCallback := nil;

  if FRenderer <> nil then
  begin
    FRenderer.Free;
  end;
  FCommands := nil;
  FCellActions := nil;
  FContextScheduler := nil;
  FContextSchedulerLease := nil;
  FExport := nil;
  FCellMount := nil;
  FClipboard := nil;
  FMount := nil;
  FView := nil;
  FHostLease := nil;

  if (FSaveHost <> nil) and (FSaveHost.LastLease <> nil) then
  begin
    FSaveHost.LastLease.Cancel;
    FSaveHost.LastLease := nil;
  end;
  FSaveHostLease := nil;
  {$ifdef PAS2JS}

  if FSurface <> nil then
  begin
    FSurface.remove;
  end;
  {$else}
  Drain;
  FSurface.Free;
  {$endif}
  FState.Free;
  FDocument.Free;
  inherited Destroy;
end;

{$ifdef PAS2JS}
function TCopyReview.Cell: TJSHTMLElement;
begin
  Result := TJSHTMLElement(FTable.querySelector(
    '[data-nyx-item="a"] [role="gridcell"][data-nyx-column="' + IntToStr(FContextColumn) + '"]'));
  Check(Result <> nil, 'actual resident typed context cell exists');
end;

function TCopyReview.Key(AElement: TJSHTMLElement; const AKey: String;
  AControl: Boolean): Boolean;
var
  LInit: TJSObject;
  LEvent: TJSKeyboardEvent;
begin
  LInit := TJSObject.new;
  LInit['key'] := AKey;
  LInit['ctrlKey'] := AControl;
  LInit['bubbles'] := True;
  LInit['cancelable'] := True;
  LEvent := TCopyKeyEvent.new('keydown', LInit);
  AElement.dispatchEvent(LEvent);
  Result := LEvent.defaultPrevented;
end;
{$endif}

procedure TCopyReview.FocusCell;
begin
  {$ifdef PAS2JS}
  Cell.focus;
  {$else}
  FGrid.Col := FContextColumn;
  FGrid.Row := 1;
  FGrid.SetFocus;
  {$endif}
  {$ifndef PAS2JS}
  Drain;
  {$endif}
end;

function TCopyReview.CopyKey: Boolean;
{$ifndef PAS2JS}
var
  LSaved: TKeyboardState;
  LControl: TKeyboardState;
  LHandle: HWND;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := Key(Cell, 'c', True);
  {$else}
  Check(GetKeyboardState(LSaved), 'capture caller thread keyboard modifiers');
  LControl := LSaved;
  LControl[VK_CONTROL] := $80;
  LControl[VK_LCONTROL] := $80;
  Check(SetKeyboardState(LControl), 'install explicit Ctrl for the paired native messages');
  LHandle := FGrid.Handle;
  try
    SendMessage(LHandle, WM_KEYDOWN, VK_C, 1);

    if FInput.Current and IsWindow(LHandle) then
    begin
      SendMessage(LHandle, WM_KEYUP, VK_C, LPARAM($C0000001));
    end;
  finally
    Check(SetKeyboardState(LSaved), 'restore exact caller keyboard modifiers');
  end;
  Result := True;
  {$endif}
  {$ifndef PAS2JS}
  Drain;
  {$endif}
end;

procedure TCopyReview.EditValue(const AText: TNyxText);
begin
  { Bound the fluent store self-return to this call, not the ownership journey. }
  FView.Store.Update(NyxCollectionItem(ItemA).WithValue(NyxTextField('Name'), AText));
end;

procedure TCopyReview.Filter(AEnabled: Boolean);
begin

  if AEnabled then
  begin
    FView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(2)));
  end
  else
  begin
    FView.ConfigureQuery(NyxCollectionQuery);
  end;
end;

procedure TCopyReview.Semantic(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LResponse: INyxEventResponse;
  LCommand: INyxCollectionClipboardCommand;
begin

  if AEvent.Trigger = ntContextMenu then
  begin
    Inc(FContexts);
    Check((AEvent.OriginID = CTable) and (AEvent.SourceID = CTable),
      'actual context callback preserves its original table identity');

    if FMode = kmConsume then
    begin
      Check(Supports(AExecution, INyxEventResponse, LResponse) and LResponse.CanConsume,
        'real context callback exposes a cancellable physical response');
      LResponse.Consume;
      Inc(FConsumed);
    end
    else if FMode = kmMutate then
    begin
      EditValue('later context data');
    end
    else if FMode = kmNested then
    begin
      { A real nested physical context passes canonical dispatch before the
        outer default resumes. Queue draining stays outside both messages. }
      FMode := kmObserve;
      try
        Context(False);
      finally
        FMode := kmNested;
      end;
    end;
    Exit;
  end;

  if not AEvent.HasKeyboard or not AEvent.Keyboard.Matches(nkCKey, [nmControl]) then
  begin
    Exit;
  end;
  Check((AEvent.OriginID = CTable) and (AEvent.SourceID = CTable),
    'real canonical key phase retains the table identity and Ctrl+C snapshot');
  Inc(FKeys);
  case FMode of
    kmObserve: ;
    kmConsume:
      begin
        Check(Supports(AExecution, INyxEventResponse, LResponse) and LResponse.CanConsume,
          'actual canonical callback can prevent the native/DOM default');
        LResponse.Consume;
        Inc(FConsumed);
      end;
    kmMutate: EditValue('later accepted 🌙');
    kmDisableABA:
      begin
        FMount.SetInteraction(False, False);
        FMount.SetInteraction(True, False);
      end;
    kmNested:
      begin
        LCommand := FCommands.BeginCell(ItemA, NameField);
        LCommand.Prepare;
        LCommand.Complete(True);
        LCommand.Retire;
      end;
    kmRetire: FRenderer.Unmount;
  end;
end;

procedure TCopyReview.Reply(const APayload: INyxCollectionClipboardText;
  const AResult: TNyxClipboardWriteResult);
begin
  Check((AResult.Outcome = ncwoWritten) and (AResult.ByteCount = APayload.ByteCount),
    'actual input gets one typed receipt with original bytes');
  Inc(FReplies);
  FLastPayload := APayload;
end;

procedure TCopyReview.CloseInspectorOnClick(ANode: TNyxNode;
  const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntClick) and (AEvent.OriginID = NyxCellInspectorCopyID) then
  begin
    Inc(FInspectorLegacyClicks);
    { The real renderer's legacy tail runs after canonical observation. A raw
      close must still withdraw the queued original Copy before host entry. }
    FCellActions.Inspector.Close;
  end;
end;

procedure TCopyReview.Context(AWait: Boolean);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
  LBounds: TJSDOMRect;
{$else}
var
  LBounds: Types.TRect;
  LPoint: Types.TPoint;
{$endif}
begin
  {$ifdef PAS2JS}
  LBounds := Cell.getBoundingClientRect;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['clientX'] := LBounds.left + 4;
  LOptions['clientY'] := LBounds.top + 4;
  LOptions['button'] := 2;
  Cell.dispatchEvent(TCellContextEvent.new('contextmenu', LOptions));
  {$else}
  LBounds := FGrid.CellRect(FContextColumn, 1);
  LPoint := FGrid.ClientToScreen(Types.Point(LBounds.Left + 4, LBounds.Top + 4));
  SendMessage(FGrid.Handle, WM_CONTEXTMENU, FGrid.Handle, MakeLParam(LPoint.X, LPoint.Y));

  if AWait then
  begin
    Drain;
  end;
  {$endif}
end;

procedure TCopyReview.MenuAction(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LMenu: INyxBrowserMenu;
{$else}
var
  LMenu: INyxLCLMenu;
{$endif}
begin
  Check((FCellActions.Menu <> nil) and FCellActions.Menu.IsOpen,
    'actual managed cell menu is open before its real action');
  {$ifdef PAS2JS}
  Check(Supports(FCellActions.Menu, INyxBrowserMenu, LMenu), 'actual browser menu facet is preserved');
  LMenu.Presentation.Renderer.FocusFor(AID).click;
  {$else}
  Check(Supports(FCellActions.Menu, INyxLCLMenu, LMenu), 'actual native menu facet is preserved');
  TButton(LMenu.Presentation.Renderer.FocusFor(AID)).Click;
  Drain;
  {$endif}
end;

procedure TCopyReview.InspectorAction(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LInspector: INyxBrowserPopover;
{$else}
var
  LInspector: INyxLCLPopover;
{$endif}
begin
  Check((FCellActions.Inspector <> nil) and FCellActions.Inspector.IsOpen,
    'actual inspector is open before its real action');
  {$ifdef PAS2JS}
  Check(Supports(FCellActions.Inspector, INyxBrowserPopover, LInspector),
    'actual browser inspector facet is preserved');
  LInspector.Renderer.FocusFor(AID).click;
  {$else}
  Check(Supports(FCellActions.Inspector, INyxLCLPopover, LInspector),
    'actual native inspector facet is preserved');
  TButton(LInspector.Renderer.FocusFor(AID)).Click;
  Drain;
  {$endif}
end;

procedure TCopyReview.RunContext; {$ifdef PAS2JS}async;{$endif}
var
  LBefore: Integer;
  LContexts: Integer;
  LValue: TNyxText;
  LInspector: INyxPopover;
  LAdmission: INyxPopoverOpenAdmission;
  LOpen: INyxPopoverOpenLifetime;
  LCaptured: INyxCollectionClipboardText;
  LIntent: INyxCollectionCellActionIntent;
  LEntry: INyxCollectionCellActionInput;
  LReserved: INyxCollectionClipboardCellCommand;
  LProxy: TContextCommands;
  LProxyLease: INyxCollectionClipboardCommands;
  LNestedMenu: INyxMenu;
  LSource: INyxCollectionSnapshot;
  LOriginalIntent: Integer;
  LMenuOpen: Boolean;
  LFailure: TNyxText;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserPopover;
  LEditor: TJSHTMLTextAreaElement;
  {$else}
  LTarget: INyxLCLPopover;
  LEditor: TCustomEdit;
  {$endif}
begin
  FocusCell;
  LBefore := FHost.Writes;
  LContexts := FContexts;
  FMode := kmConsume;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check((FContexts = LContexts + 1) and (FHost.Writes = LBefore) and
    (FCellActions.Menu = nil), 'consumed canonical context creates no menu or clipboard effect');
  Check(Supports(FCellActions, INyxCollectionCellActionIntent, LIntent),
    'mounted cell actions expose a canonical nested-intent witness');
  Check(Supports(FCellActions, INyxCollectionCellActionInput, LEntry),
    'managed context reserves original intent before a foreign input getter');
  LOriginalIntent := LIntent.Intent;
  LContexts := FContexts;
  LSource := FView.Store.Snapshot;
  FMode := kmObserve;
  LProxy := TContextCommands.Create;
  LProxyLease := LProxy;
  LProxy.Commands := FCommands;
  LProxy.Owner := Self;
  LReserved := LEntry.BeginCellInput(LProxyLease);
  Check((LProxy.Reentries = 1) and (FContexts = LContexts + 1) and
    (LIntent.Intent = LOriginalIntent + 1) and (LReserved <> nil) and
    not LReserved.Current and not LReserved.DispatchCurrent,
    'real nested context inside input getter permanently retires the earlier reserved intent');
  LReserved.ResolveCell(ItemA, NameField);
  LReserved.Prepare;
  Check((LReserved.Payload = nil) and LReserved.Complete(True) and
    not LReserved.Current and not LReserved.DispatchCurrent,
    'stale resolution/preparation/default cannot recapture the nested context even for identical bytes');
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LNestedMenu := FCellActions.Menu;
  Check((LIntent.Intent = LOriginalIntent + 1) and (LNestedMenu <> nil) and
    LNestedMenu.IsOpen and (FCellActions.Snapshot.Text = CText) and
    (FCellActions.Snapshot.ItemAt(0).ID = ItemA.ID) and
    FCellActions.Snapshot.FieldAt(0).Same(NameField) and
    (FHost.Writes = LBefore) and (FView.Store.Snapshot = LSource),
    'only the nested original menu is published with exact RAW provenance and no data or clipboard effect');
  LReserved.Retire;
  LReserved := nil;
  LProxyLease := nil;
  Check((FCellActions.Menu = LNestedMenu) and LNestedMenu.IsOpen and
    (LIntent.Intent = LOriginalIntent + 1),
    'late outer retirement leaves the exact accepted nested menu and intent intact');
  LNestedMenu := nil;
  FCellActions.Close;
  FocusCell;
  LOriginalIntent := LIntent.Intent;
  LContexts := FContexts;
  FMode := kmNested;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  LMenuOpen := (FCellActions.Menu <> nil) and FCellActions.Menu.IsOpen;
  LFailure := '';

  if FCellActions.LastExecution <> nil then
  begin
    LFailure := FCellActions.LastExecution.Failure;
  end;
  Check((FContexts = LContexts + 2) and (LIntent.Intent = LOriginalIntent + 1) and
    LMenuOpen and (FHost.Writes = LBefore),
    'later nested actual context alone owns the menu and supersedes the outer default; contexts=' +
    TNyxText(IntToStr(FContexts)) + '/' + TNyxText(IntToStr(LContexts + 2)) +
    ', intent=' + TNyxText(IntToStr(LIntent.Intent)) + '/' +
    TNyxText(IntToStr(LOriginalIntent + 1)) + ', menu=' +
    TNyxText(BoolToStr(LMenuOpen, True)) + ', writes=' +
    TNyxText(IntToStr(FHost.Writes)) + '/' + TNyxText(IntToStr(LBefore)) +
    ', execution=' + LFailure);
  FCellActions.Close;
  FMode := kmObserve;
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check((FCellActions.Menu <> nil) and FCellActions.Menu.IsOpen and
    (FCellActions.Snapshot.Text = CText) and
    (FCellActions.Snapshot.ItemAt(0).ID = ItemA.ID) and
    FCellActions.Snapshot.FieldAt(0).Same(NameField),
    'actual context launch captures exact accepted raw value and typed provenance');
  LCaptured := FCellActions.Snapshot;
  MenuAction(NyxCellActionsInspectID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  LInspector := FCellActions.Inspector;
  Check((LInspector <> nil) and LInspector.IsOpen and
    Supports(LInspector, INyxPopoverOpenAdmission, LAdmission),
    'real managed Inspect opens an ordinary per-open Nyx presentation');
  LOpen := LAdmission.GetOpenLifetime;
  {$ifdef PAS2JS}
  Check(Supports(LInspector, INyxBrowserPopover, LTarget), 'browser inspector remains actual popover');
  LEditor := TJSHTMLTextAreaElement(LTarget.Renderer.FocusFor(NyxCellInspectorValueID));
  Check(LEditor.readOnly, 'inspection uses an actual readonly browser text area');
  LValue := LEditor.value;
  {$else}
  Check(Supports(LInspector, INyxLCLPopover, LTarget), 'native inspector remains actual popover');
  LEditor := TCustomEdit(LTarget.Renderer.FocusFor(NyxCellInspectorValueID));
  Check(LEditor.ReadOnly, 'inspection uses an actual readonly native memo');
  LValue := NyxLCLInputText(LEditor);
  LValue := StringReplace(LValue, #13#10, #10, [rfReplaceAll]);
  {$endif}
  Check(LValue = NyxCollectionCellInspectionText(CText),
    'full multiline/supplementary value and escaped NUL are visible without truncation');
  EditValue('later context data');
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check((FCellActions.Snapshot = LCaptured) and (LCaptured.Text = CText),
    'later accepted edits cannot redirect the explicitly captured inspector snapshot');
  InspectorAction(NyxCellInspectorCopyID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check((FHost.Writes = LBefore + 1) and (FHost.Text = CText) and
    (FCellActions.LastResult.Outcome = ncwoWritten) and
    (LInspector.State.GetValue(NyxTextState(NyxCellInspectorStatusState)) <> ''),
    'real inspector Copy shares mounted arbitration and reports its exact raw receipt');
  EditValue(CText);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  InspectorAction(NyxCellInspectorCloseID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check(not LInspector.IsOpen and not LOpen.Current and (FCellActions.Inspector = nil),
    'real Close retires the exact inspector open and detached action session');
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  MenuAction(NyxCellActionsCopyID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check((FHost.Writes = LBefore + 2) and (FHost.Text = CText) and
    not FCellActions.Menu.IsOpen, 'real managed menu Copy closes and writes accepted raw bytes once');
  FCellActions.Close;
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  MenuAction(NyxCellActionsInspectID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  LInspector := FCellActions.Inspector;
  Check(Supports(LInspector, INyxPopoverOpenAdmission, LAdmission),
    'callback-retirement case uses the actual inspector open admission');
  LOpen := LAdmission.GetOpenLifetime;
  {$ifdef PAS2JS}
  Check(Supports(LInspector, INyxBrowserPopover, LTarget), 'legacy case keeps the actual browser renderer');
  LTarget.Renderer.OnEvent := @CloseInspectorOnClick;
  {$else}
  Check(Supports(LInspector, INyxLCLPopover, LTarget), 'legacy case keeps the actual native renderer');
  LTarget.Renderer.OnEvent := CloseInspectorOnClick;
  {$endif}
  InspectorAction(NyxCellInspectorCopyID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check((FInspectorLegacyClicks = 1) and not LOpen.Current and not LInspector.IsOpen and
    (FHost.Writes = LBefore + 2),
    'actual legacy close withdraws the original queued inspector Copy before clipboard host entry');
  LTarget.Renderer.OnEvent := nil;
  FCellActions.Close;
  FocusCell;
  {$ifdef PAS2JS}
  Key(Cell, 'F2', False);
  await(Drain);
  TJSHTMLInputElement(Cell.querySelector('input')).value := CDraft;
  {$else}
  SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
  Drain;
  Check(FGrid.EditorMode, 'native draft is actively owned before contextual inspection');
  RestoreNyxLCLInputText(TCustomEdit(FGrid.Editor), CDraft);
  {$endif}
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  MenuAction(NyxCellActionsInspectID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check((FCellActions.Snapshot.Text = CText) and
    (FView.Store.Snapshot.Item(ItemA).GetValue(NyxTextField('Name')) = CText),
    'opening context/inspector never captures or accepts an unfinished data-cell draft');
  InspectorAction(NyxCellInspectorCloseID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  FocusCell;
  {$ifdef PAS2JS}
  Key(Cell, 'F2', False);
  await(Drain);
  Check(TJSHTMLInputElement(Cell.querySelector('input')).value = CDraft,
    'fresh browser editor resumes the exact retained draft after inspection');
  Key(TJSHTMLInputElement(Cell.querySelector('input')), 'Escape', False);
  await(Drain);
  {$else}
  SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
  Drain;
  Check(FGrid.EditorMode and (NyxLCLInputText(TCustomEdit(FGrid.Editor)) = CDraft),
    'fresh native editor resumes the exact dormant draft after inspection');
  SendMessage(FGrid.Handle, WM_KEYDOWN, VK_ESCAPE, 1);
  Drain;
  {$endif}
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  FMount.SetInteraction(False, False);
  FMount.SetInteraction(True, False);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check((FCellActions.Menu = nil) and (FCellActions.Inspector = nil) and
    (FHost.Writes = LBefore + 2), 'disable/re-enable retires context actions without any old default');
  Check((TNyxCodec.Encode(FDocument) = FAuthored) and (LCaptured.Text = CText),
    'retained immutable inspection and all authored defaults survive action cleanup');
end;

{ Real menu/inspector buttons choose explicit display output. Expected formatter
  refusals use the same actual table and RAW inspector; no host effect, fallback
  marker or accepted data is substituted for the requested display product. }
procedure TCopyReview.RunDisplayed; {$ifdef PAS2JS}async;{$endif}
var
  LActions: INyxCollectionCellRepresentationActions;
  LSource: INyxCollectionSnapshot;
  LValue: INyxCollectionCellText;
  LRaw: INyxCollectionClipboardText;
  LWrites: Integer;
  LReplies: Integer;
  LColumn: Integer;
begin
  LSource := FView.Store.Snapshot;
  FContextColumn := 0;
  FMode := kmObserve;
  FocusCell;
  {$ifdef PAS2JS}await(Drain);{$endif}
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check(Supports(FCellActions, INyxCollectionCellRepresentationActions, LActions) and
    LActions.DisplayPolicy.Declared and
    (LActions.DisplayPolicy.Formatter.Identity.Name = 'full cell') and
    (LActions.DisplayPolicy.Formatter.Revision = 3),
    'actual context captures original complete formatter identity before later actions');
  LWrites := FHost.Writes;
  LReplies := FReplies;
  MenuAction(NyxCellActionsCopyDisplayID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  LValue := LActions.LastRepresentation;
  Check((FHost.Writes = LWrites + 1) and (FHost.Text = TNyxText('«') + CText + TNyxText('»')) and
    (FReplies = LReplies) and LActions.HasRepresentationResult and
    (LActions.LastRepresentationResult.Outcome = ncwoWritten) and
    (LActions.LastRepresentationResult.ByteCount = NyxUTF8ByteCount(FHost.Text)),
    'real display menu sends exact expanded Unicode/NUL/newlines through a separate typed receipt');
  Check((LValue.Representation = nccrFormattedDisplay) and (LValue.RawText = CText) and
    (LValue.Value.TextValue = CText) and LValue.Field.Same(NameField) and
    (LValue.Item.ID = ItemA.ID) and (LValue.Revision = LSource.Revision),
    'display action retains accepted RAW scalar and original full row/field/revision provenance');
  FCellActions.Close;
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  MenuAction(NyxCellActionsInspectID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  LWrites := FHost.Writes;
  InspectorAction(NyxCellInspectorCopyDisplayID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check((FHost.Writes = LWrites + 1) and (FReplies = LReplies) and
    (FCellActions.Snapshot.Text = CText) and
    (FCellActions.Inspector.State.GetValue(NyxTextState(NyxCellInspectorStatusState)) <> ''),
    'real readonly RAW inspector offers deliberate display copy without relabelling its snapshot');
  InspectorAction(NyxCellInspectorCloseID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  try
    for LColumn := 2 to 4 do
    begin
      FContextColumn := LColumn;
      FocusCell;
      {$ifdef PAS2JS}await(Drain);{$endif}
      Context;
      {$ifdef PAS2JS}await(Drain);{$endif}
      LRaw := FCellActions.Snapshot;
      LWrites := FHost.Writes;
      LReplies := FReplies;
      Check((LRaw <> nil) and LActions.DisplayPolicy.Declared and
        LActions.DisplayPolicy.Field.Same(LRaw.FieldAt(0)),
        'expected display limitation does not prevent canonical RAW context preparation');
      MenuAction(NyxCellActionsCopyDisplayID);
      {$ifdef PAS2JS}await(Drain);{$endif}
      Check((FHost.Writes = LWrites) and (FReplies = LReplies) and
        LActions.HasRepresentationResult and (LActions.LastRepresentation = nil) and
        (LActions.LastRepresentationResult.Outcome = ncwoFailed) and
        (LActions.LastRepresentationResult.Error <> '') and
        (FCellActions.Inspector <> nil) and FCellActions.Inspector.IsOpen,
        'strict huge/exact-decimal/minute refusal is observable without any clipboard effect');
      Check((FCellActions.Snapshot = LRaw) and (FView.Store.Snapshot = LSource) and
        (FCellActions.Inspector.State.GetValue(NyxTextState(NyxCellInspectorStatusState)) <> ''),
        'refusal explains itself beside the exact unchanged RAW snapshot and source');
      InspectorAction(NyxCellInspectorCopyID);
      {$ifdef PAS2JS}await(Drain);{$endif}
      Check((FHost.Writes = LWrites + 1) and (FHost.Text = LRaw.Text) and
        (FReplies = LReplies + 1) and (FCellActions.LastResult.Outcome = ncwoWritten),
        'explicit accepted-RAW copy remains useful after a strict display refusal');
      InspectorAction(NyxCellInspectorCloseID);
      {$ifdef PAS2JS}await(Drain);{$endif}
    end;
  finally
    FContextColumn := 0;
  end;
  FocusCell;
  Check((FView.Store.Snapshot = LSource) and (TNyxCodec.Encode(FDocument) = FAuthored) and
    (LValue.Text = TNyxText('«') + CText + TNyxText('»')),
    'represented actions preserve accepted data/source and detached original displayed bytes');
end;

{ Capture reads the actual focused editor without the existing suspension
  protocol. Deliberate public transfer is separate from a physical editor copy
  key; it preserves the host's ordinary Ctrl+C and never accepts draft input. }
procedure TCopyReview.RunFocusedDraft; {$ifdef PAS2JS}async;{$endif}
var
  LDraftInput: INyxCollectionFocusedDraftInput;
  LWriter: INyxCollectionClipboardRepresentationWrite;
  LCapture: INyxCollectionFocusedDraftCapture;
  LScope: INyxCancellationScope;
  LTicket: INyxClipboardWriteRequest;
  LSource: INyxCollectionSnapshot;
  LSelection: TNyxTextSelection;
  LAfter: TNyxTextSelection;
  LRaw: INyxCollectionClipboardText;
  LDisplay: INyxCollectionCellText;
  LRawReplies: Integer;
  LWrites: Integer;
  LKeys: Integer;
  LColumn: Integer;
  LText: TNyxText;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LEditor: TJSHTMLInputElement;
  {$else}
  LEditor: TCustomEdit;
  {$endif}
begin
  FContextColumn := 0;
  FocusCell;
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check(Supports(FMount, INyxCollectionFocusedDraftInput, LDraftInput) and
    Supports(FCommands, INyxCollectionClipboardRepresentationWrite, LWriter),
    'actual table exposes readonly focused draft capture and the same represented write port');
  Check(LDraftInput.CaptureFocusedDraft = nil,
    'a focused data cell without an editor cannot invent explicit draft authority');
  LSource := FView.Store.Snapshot;
  LRawReplies := FReplies;
  try
    for LColumn := 0 to 1 do
    begin
      FContextColumn := LColumn;
      FocusCell;
      {$ifdef PAS2JS}
      await(Drain);
      Key(Cell, 'F2', False);
      await(Drain);
      LEditor := TJSHTMLInputElement(Cell.querySelector('input'));
      Check((LEditor <> nil) and (document.activeElement = LEditor),
        'readonly draft test enters the actual browser cell editor');
      {$else}
      SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
      Drain;
      Check(FGrid.EditorMode and (FGrid.Editor is TCustomEdit),
        'readonly draft test enters the actual native cell editor');
      LEditor := TCustomEdit(FGrid.Editor);
      {$endif}
      LText := CDraft;

      if LColumn = 1 then
      begin
        LText := '-';
      end;
      LSelection := NyxTextSelection(LText, NyxTextScalarCount(LText) - 1,
        NyxTextScalarCount(LText), {$ifdef PAS2JS}ntdBackward{$else}ntdUnknown{$endif});
      {$ifdef PAS2JS}
      LEditor.value := LText;
      SelectNyxBrowserText(LEditor, LSelection);
      {$else}
      RestoreNyxLCLInputText(LEditor, LText);
      SelectNyxLCLText(LEditor, LSelection);
      {$endif}
      LWrites := FHost.Writes;
      LKeys := FKeys;
      LCapture := LDraftInput.CaptureFocusedDraft;
      Check((LCapture <> nil) and (LCapture.InputEpoch = FInput.Epoch) and
        (LCapture.Value.Representation = nccrExplicitDraft) and
        not LCapture.Value.HasFormatter and (LCapture.Value.Text = LText) and
        (LCapture.Value.Item.ID = ItemA.ID) and
        (LCapture.Value.Revision = LSource.Revision) and
        LCapture.Selection.SameRange(LSelection),
        'readonly capture copies exact explicit text, scalar range and accepted row/field/revision');
      {$ifdef PAS2JS}
      LAfter := CaptureNyxBrowserSelection(LEditor);
      Check((document.activeElement = LEditor) and (LEditor.value = LText) and
        LAfter.SameRange(LSelection), 'browser draft capture leaves focus/text/Unicode caret untouched');
      {$else}
      LAfter := CaptureNyxLCLSelection(LEditor);
      Check(FGrid.EditorMode and (FGrid.Editor = LEditor) and
        (NyxLCLInputText(LEditor) = LText) and LAfter.SameRange(LSelection),
        'native draft capture leaves editing/text/supplementary-scalar caret untouched');
      {$endif}
      Check((FView.Store.Snapshot = LSource) and (FHost.Writes = LWrites) and
        (FKeys = LKeys), 'readonly capture does not publish data, keyboard events or clipboard effects');

      if LColumn = 0 then
      begin
        Check(LCapture.Value.Field.Same(NameField) and
          (LCapture.Value.RawText = CText) and (LCapture.Value.Value.TextValue = CText),
          'supplementary draft remains distinct from the full accepted Unicode/NUL value');
      end
      else
      begin
        LRaw := NewNyxCollectionClipboardCell(LSource, ItemA,
          NyxColumnIdentity(NyxIntegerField('Score')), TNyxCollectionClipboardLimits.Defaults);
        LDisplay := NewNyxCollectionCellDisplay(LRaw, NyxValueFormat.Decimal(2).Localized(
          NyxFormatLocale.NumberSeparators(',', '.')), NyxNoDomain);
        Check((LCapture.Value.Value.IntegerValue = 1) and (LCapture.Value.RawText = '1') and
          (LDisplay.Text = '1,00') and (LCapture.Value.Text = '-') and
          LCapture.Value.Field.Same(NyxColumnIdentity(NyxIntegerField('Score'))),
          'unfinished numeric draft is neither accepted integer nor formatted display text');
      end;
      Check(Supports(LCapture.Readiness, INyxCancellationScope, LScope),
        'captured original draft readiness supplies certified host cancellation');
      LTicket := LWriter.WriteRepresented(LCapture.Value, LScope,
        LCapture.Readiness, nil, nil);
      Check((FHost.Writes = LWrites + 1) and (FHost.Text = LText) and
        (FReplies = LRawReplies) and (FView.Store.Snapshot = LSource) and
        (LWriter.LastRepresentation.Representation = nccrExplicitDraft),
        'only deliberate draft transfer writes exact text through the shared slot and separate result');
      {$ifdef PAS2JS}
      Check((document.activeElement = LEditor) and (LEditor.value = LText),
        'inline draft write cannot blur or accept the browser editor');
      Key(LEditor, 'Escape', False);
      await(Drain);
      {$else}
      Check(FGrid.EditorMode and (NyxLCLInputText(LEditor) = LText),
        'inline draft write cannot hide or accept the native editor');
      SendMessage(FGrid.Handle, WM_KEYDOWN, VK_ESCAPE, 1);
      Drain;
      {$endif}
    end;
  finally
    FContextColumn := 0;
  end;
  FLastDraft := LCapture;
  LWrites := FHost.Writes;
  FMount.SetInteraction(False, False);
  FMount.SetInteraction(True, False);
  LRefused := False;
  try
    LTicket := LWriter.WriteRepresented(LCapture.Value, LScope,
      LCapture.Readiness, nil, nil);
  except
    on E: ENyxCollectionClipboard do LRefused := True;
  end;
  Check(LRefused and (FHost.Writes = LWrites) and
    (LCapture.Value.Text = '-') and (FView.Store.Snapshot = LSource),
    'retained draft value survives disable ABA while its old input proof cannot write');
  FocusCell;
  Check((LDraftInput.CaptureFocusedDraft = nil) and
    (FView.Store.Snapshot = LSource) and (TNyxCodec.Encode(FDocument) = FAuthored),
    'an inert retained draft cannot revive active editor authority or alter authored/accepted data');
end;

{ Same table owner/source as the original journey. Selection is installed
  through the optional typed mount contract; paired real key routes then copy
  that exact range. This slice proves mounted paint/copy, not a hardware range
  drag or OS clipboard effect. Gesture navigation is a separate next input. }
procedure TCopyReview.RunCellRange; {$ifdef PAS2JS}async;{$endif}
var
  LRanges: INyxCollectionCellRangeMount;
  LIntent: INyxCollectionCellRangeIntent;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LRows: TNyxCollectionSelectionSnapshot;
  LItems: TNyxItemRefs;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LPreferences: TNyxColumnPreferences;
  LExpected: TNyxText;
  LQuery: TNyxText;
  LScore: TNyxColumnIdentity;
  LBefore: Integer;
  LKeys: Integer;
  LReplies: Integer;
  LIndex: Integer;
  {$ifndef PAS2JS}
  LPanes: Integer;
  LPane: TNyxCollectionStringGrid;
  LPhysical: Integer;
  LHeader: TNyxText;
  LHeaderFacts: TNyxText;
  LPaintedHeader: TNyxText;
  {$endif}
begin
  LSource := FView.Store.Snapshot;
  LQuery := FView.QueryPolicy.ToData.ToJSON;
  LRows := FView.Selection.Snapshot;
  SetLength(LItems, LRows.Count);
  for LIndex := 0 to LRows.Count - 1 do
  begin
    LItems[LIndex] := LRows.ItemAt(LIndex);
  end;
  LScore := NyxColumnIdentity(NyxIntegerField('Score'));
  FMode := kmObserve;
  FContextColumn := 0;
  FocusCell;
  Check(Supports(FMount, INyxCollectionCellRangeMount, LRanges),
    'same renderer-managed table exposes optional typed cell range selection');
  Check(not LRanges.SelectCells(NyxCollectionCellIdentity(ItemA, NameField),
    NyxCollectionCellIdentity(ItemB, LScore)) and (LRanges.CellRange = nil),
    'ordinary default table mode does not implicitly reinterpret row selection as a cell range');
  LColumns := NyxCollectionColumns(FView);
  LCapture := LColumns.CaptureColumns;
  LPreferences := LCapture.Preferences;
  try

    if FView.Spec.HasLayoutControls then
    begin
      LColumns.ConfigureColumns(LPreferences.WithColumn(NyxColumnPreference(NameField).Pinned(ncpStart))
        .WithColumn(NyxColumnPreference(LScore).Pinned(ncpEnd)));
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    end;
    {$ifndef PAS2JS}
    { Native pinned budgets use real inert spacer columns on the outer grid.
      Resolve an actual data header before producing input; physical zero is
      intentionally not a data-cell identity when a start pane is present. }
    { Score is inside the ordinary Name..Score rectangle, and its header does
      not carry the first-column whole-page row-count decoration. }
    LHeader := FView.Spec.ColumnAt(1).Title;

    if FView.Spec.HasLayoutControls then
    begin
      LHeader := FView.Spec.ColumnAt(2).Title;
    end;
    LPhysical := -1;
    LHeaderFacts := '';
    for LIndex := 0 to FGrid.ColCount - 1 do
    begin
      LPaintedHeader := FGrid.DisplayCellText(LIndex, 0);
      LHeaderFacts := LHeaderFacts + '[' + TNyxText(IntToStr(LIndex)) + ':paint=' +
        LPaintedHeader + ';raw=' + TNyxText(FGrid.Cells[LIndex, 0]) + ']';

      { The actual native query affordance appends its own two-space suffix.
        Match this fixture's unique full title plus that owned boundary. }

      if (LPaintedHeader = LHeader) or (Pos(LHeader + '  ', LPaintedHeader) = 1) then
      begin
        LPhysical := LIndex;
        Break;
      end;
    end;
    Check(LPhysical >= 0, 'native range input resolves its actual middle data header beyond pin spacers; expected=' +
      LHeader + ';count=' + TNyxText(IntToStr(FGrid.ColCount)) +
      ';connected=' + TNyxText(BoolToStr(FMount.Connected, True)) +
      ';ready=' + TNyxText(BoolToStr(FInput.Ready, True)) + ';headers=' + LHeaderFacts);
    FGrid.Col := LPhysical;
    FGrid.Row := 1;
    FGrid.SetFocus;
    Drain;
    {$endif}
    LRanges.ConfigureCellRange(True);
    LPage := FView.Snapshot;
    LBefore := FHost.Writes;
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(ItemA, NameField),
      NyxCollectionCellIdentity(ItemB, LScore)),
      'explicit stable identities select the actual page rectangle without a source mutation');
    LIntent := LRanges.CellRange;
    LExpected := LIntent.Capture.Payload.Text;
    Check(LIntent.Current and (LIntent.Source = LSource) and (LIntent.Page = LPage) and
      (LIntent.Capture.Payload.RowCount = 2) and
      LIntent.Capture.Contains(ItemA, NameField) and LIntent.Capture.Contains(ItemB, LScore) and
      FView.Selection.Snapshot.SameState(LRows) and (FHost.Writes = LBefore),
      'range selection owns exact original page/field facts and leaves row selection and host untouched');
    {$ifdef PAS2JS}
    Check(FTable.querySelector('[data-nyx-item="a"] [data-nyx-column="0"][data-nyx-range-selected="true"]') <> nil,
      'actual Name cell exposes its selected range paint and accessibility state');
    Check(FTable.querySelector('[data-nyx-item="b"] [data-nyx-column="1"][data-nyx-range-selected="true"]') <> nil,
      'actual Score cell is highlighted across the logical opposite pin edge');
    {$else}
    FGrid.Repaint;
    Check(not FGrid.CellRangeSelected(LPhysical, 0) and FGrid.CellRangeSelected(LPhysical, 1) and
      FGrid.CellRangeSelected(LPhysical, 2),
      'actual native paint predicate highlights bounded data rows and excludes the header');

    if FView.Spec.HasLayoutControls then
    begin
      LPanes := 0;
      for LIndex := 0 to FGrid.ControlCount - 1 do
      begin

        if (FGrid.Controls[LIndex] is TNyxCollectionStringGrid) and FGrid.Controls[LIndex].Visible then
        begin
          LPane := TNyxCollectionStringGrid(FGrid.Controls[LIndex]);
          Check(LPane.CellRangeSelected(0, 1) and LPane.CellRangeSelected(0, 2),
            'real editable pinned pane uses the same exact range paint membership');
          Inc(LPanes);
        end;
      end;
      Check(LPanes = 2, 'wide native range paint includes both actual logical pin panes');
    end;
    {$endif}
    LKeys := FKeys;
    LReplies := FReplies;
    CopyKey;
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check((FKeys = LKeys + 1) and (FHost.Writes = LBefore + 1) and
      (FReplies = LReplies + 1) and (FHost.Text = LExpected) and
      (FLastPayload.Text = LExpected) and (FLastPayload.RowCount = 2),
      'real canonical Ctrl+C transfers the selected RAW TSV once through the original host slot');
    FMode := kmConsume;
    CopyKey;
    Check((FHost.Writes = LBefore + 1) and LIntent.Current,
      'consumed real range key cannot write or withdraw the accepted rectangle');
    FMode := kmDisableABA;
    CopyKey;
    Check((FHost.Writes = LBefore + 1) and not LIntent.Current and (LRanges.CellRange = nil),
      'real disable/re-enable key callback retires the old range instead of copying newer authority');
    FMode := kmObserve;
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(ItemA, NameField),
      NyxCollectionCellIdentity(ItemB, LScore)), 'fresh admitted input may create a new explicit range');
    LIntent := LRanges.CellRange;
    LRanges.ClearCellRange;
    Check(not LIntent.Current and (LRanges.CellRange = nil) and
      (LIntent.Capture.Payload.Text = LExpected),
      'explicit clear revokes only selection authority and retains exact copied values');
    {$ifdef PAS2JS}
    Check(FTable.querySelector('[data-nyx-range-selected="true"]') = nil,
      'clear removes actual browser range paint without hiding or replacing cells');
    {$else}
    Check(not FGrid.CellRangeSelected(LPhysical, 1),
      'clear removes actual native range paint membership');
    {$endif}
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(ItemA, NameField),
      NyxCollectionCellIdentity(ItemB, LScore)), 'range can be selected again before a real filter replacement');
    LIntent := LRanges.CellRange;
    Filter(True);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(not LIntent.Current and (LRanges.CellRange = nil) and
      (LIntent.Capture.Payload.Text = LExpected),
      'actual filtered page replacement retires old range input and preserves its immutable output');
  finally
    FMode := kmObserve;
    LRanges.ConfigureCellRange(False);
    Filter(False);

    if FView.Spec.HasLayoutControls then
    begin
      LColumns.ConfigureColumns(LPreferences);
    end;
    { The filter's ordinary focus reconciliation is independent of range
      selection. Restore the caller's original row state through its public
      owner, after checking above that selection/copy never changed it. }
    FView.SetSelection(LItems, LRows.Focus, LRows.Anchor);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    FContextColumn := 0;
    FocusCell;
  end;
  Check((FView.Store.Snapshot = LSource) and FView.Selection.Snapshot.SameState(LRows) and
    (FView.QueryPolicy.ToData.ToJSON = LQuery) and (TNyxCodec.Encode(FDocument) = FAuthored),
    'mounted range paint and RAW copy preserve exact accepted data row/query state and authored source');
end;

{ Same real editor and managed presenters as the RAW journey. Native context
  uses the actual editor HWND/keyboard-context message; browser context is a
  synthetic cancellable event, with no trusted pointer/OS menu claim. }
procedure TCopyReview.RunDraftActions; {$ifdef PAS2JS}async;{$endif}
var
  LDraftActions: INyxCollectionCellDraftActions;
  LRepresented: INyxCollectionCellRepresentationActions;
  LSource: INyxCollectionSnapshot;
  LCapture: INyxCollectionFocusedDraftCapture;
  LSelection: TNyxTextSelection;
  LText: TNyxText;
  LRaw: TNyxText;
  LBefore: Integer;
  LRawReplies: Integer;
  LColumn: Integer;
  LInspector: INyxPopover;
  LIntent: INyxCollectionCellActionIntent;
  LOriginalIntent: Integer;
  LContexts: Integer;
  LReentries: Integer;
  LNestedMenu: INyxMenu;
  LNestedCapture: INyxCollectionFocusedDraftCapture;
  {$ifdef PAS2JS}
  LEditor: TJSHTMLInputElement;
  LOptions: TJSObject;
  LContextClaimed: Boolean;
  LTarget: INyxBrowserPopover;
  LMemo: TJSHTMLTextAreaElement;
  {$else}
  LEditor: TCustomEdit;
  LTarget: INyxLCLPopover;
  LMemo: TCustomEdit;
  LPrevious: TContextPopupEvent;
  LPreviousContexts: Integer;
  {$endif}

  procedure EditorContext;
  begin
    {$ifdef PAS2JS}
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := True;
    LOptions['button'] := 2;
    LContextClaimed := not LEditor.dispatchEvent(TCellContextEvent.new('contextmenu', LOptions));
    {$else}
    SendMessage(LEditor.Handle, WM_CONTEXTMENU, LEditor.Handle, MakeLParam(-1, -1));
    {$endif}
  end;

begin
  Check(Supports(FCellActions, INyxCollectionCellDraftActions, LDraftActions) and
    LDraftActions.FocusedDraftEnabled and
    Supports(FCellActions, INyxCollectionCellRepresentationActions, LRepresented),
    'explicit runtime opt-in enables separate managed draft actions without changing RAW APIs');
  LSource := FView.Store.Snapshot;
  try
    for LColumn := 0 to 1 do
    begin
      FContextColumn := LColumn;
      FocusCell;
      {$ifdef PAS2JS}
      await(Drain);
      Key(Cell, 'F2', False);
      await(Drain);
      LEditor := TJSHTMLInputElement(Cell.querySelector('input'));
      Check((LEditor <> nil) and (document.activeElement = LEditor),
        'managed draft menu begins from the actual focused browser cell input');
      {$else}
      SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
      Drain;
      Check(FGrid.EditorMode and (FGrid.Editor is TCustomEdit),
        'managed draft menu begins from the actual focused native cell input');
      LEditor := TCustomEdit(FGrid.Editor);
      {$endif}
      LText := CDraft;
      LRaw := CText;

      if LColumn = 1 then
      begin
        LText := '-';
        LRaw := '1';
      end;
      LSelection := NyxTextSelection(LText, NyxTextScalarCount(LText) - 1,
        NyxTextScalarCount(LText), {$ifdef PAS2JS}ntdBackward{$else}ntdUnknown{$endif});
      {$ifdef PAS2JS}
      LEditor.value := LText;
      SelectNyxBrowserText(LEditor, LSelection);
      {$else}
      RestoreNyxLCLInputText(LEditor, LText);
      SelectNyxLCLText(LEditor, LSelection);
      {$endif}
      LBefore := FHost.Writes;
      LRawReplies := FReplies;
      FMode := kmConsume;
      EditorContext;
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check(((FCellActions.Menu = nil) or not FCellActions.Menu.IsOpen) and
        (FHost.Writes = LBefore) and (FView.Store.Snapshot = LSource),
        'consumed actual editor context opens no draft menu and never accepts text');
      FMode := kmObserve;
      EditorContext;
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      LCapture := LDraftActions.CapturedDraft;
      Check((FCellActions.Menu <> nil) and FCellActions.Menu.IsOpen and
        (LCapture <> nil) and (LCapture.Value.Representation = nccrExplicitDraft) and
        (LCapture.Value.Text = LText) and (LCapture.Value.RawText = LRaw) and
        LCapture.Selection.SameRange(LSelection) and
        (FCellActions.Snapshot.Text = LRaw) and (FView.Store.Snapshot = LSource),
        'actual editor context captures draft before menu focus while RAW/scalar/source remain accepted');
      MenuAction(NyxCellActionsInspectDraftID);
      {$ifdef PAS2JS}await(Drain);{$endif}
      LInspector := FCellActions.Inspector;
      Check((LInspector <> nil) and LInspector.IsOpen,
        'real managed Inspect captured draft opens an ordinary readonly Nyx inspector');
      {$ifdef PAS2JS}
      Check(Supports(LInspector, INyxBrowserPopover, LTarget),
        'draft inspector retains the actual browser target facet');
      LMemo := TJSHTMLTextAreaElement(LTarget.Renderer.FocusFor(NyxCellInspectorValueID));
      Check(LMemo.readOnly and (LMemo.value = NyxCollectionCellInspectionText(LText)),
        'full browser draft inspection preserves supplementary text and unfinished numeric bytes');
      {$else}
      Check(Supports(LInspector, INyxLCLPopover, LTarget),
        'draft inspector retains the actual native target facet');
      LMemo := TCustomEdit(LTarget.Renderer.FocusFor(NyxCellInspectorValueID));
      Check(LMemo.ReadOnly and (NyxLCLInputText(LMemo) = NyxCollectionCellInspectionText(LText)),
        'full native draft inspection preserves supplementary text and unfinished numeric bytes');
      {$endif}
      InspectorAction(NyxCellInspectorCopyDraftID);
      {$ifdef PAS2JS}await(Drain);{$endif}
      Check((FHost.Writes = LBefore + 1) and (FHost.Text = LText) and
        LRepresented.HasRepresentationResult and
        (LRepresented.LastRepresentation.Representation = nccrExplicitDraft) and
        (LRepresented.LastRepresentation.Text = LText) and (FReplies = LRawReplies),
        'real Copy captured draft uses the same host slot with an explicit representation and no RAW reply');
      InspectorAction(NyxCellInspectorCopyID);
      {$ifdef PAS2JS}await(Drain);{$endif}
      Check((FHost.Writes = LBefore + 2) and (FHost.Text = LRaw) and
        (FReplies = LRawReplies + 1) and (FView.Store.Snapshot = LSource),
        'separately labeled accepted-value action still writes exact RAW without accepting the draft');
      InspectorAction(NyxCellInspectorCloseID);
      {$ifdef PAS2JS}await(Drain);{$endif}
      FCellActions.Close;
      {$ifdef PAS2JS}
      Key(LEditor, 'Escape', False);
      await(Drain);
      {$else}
      SendMessage(FGrid.Handle, WM_KEYDOWN, VK_ESCAPE, 1);
      Drain;
      {$endif}
    end;
    { The actual editor event reserves intent before the scheduler's outward
      UI admission. That admission enters a newer REAL grid context while the
      original editor message is still on the stack. The old return must stay
      claimed instead of falling back to native/default or canonical replay. }
    FContextColumn := 0;
    FocusCell;
    {$ifdef PAS2JS}
    await(Drain);
    Key(Cell, 'F2', False);
    await(Drain);
    LEditor := TJSHTMLInputElement(Cell.querySelector('input'));
    Check((LEditor <> nil) and (document.activeElement = LEditor),
      'nested admission begins from the actual focused browser draft editor');
    LEditor.value := CDraft;
    {$else}
    SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
    Drain;
    Check(FGrid.EditorMode and (FGrid.Editor is TCustomEdit),
      'nested admission begins from the actual focused native draft editor');
    LEditor := TCustomEdit(FGrid.Editor);
    RestoreNyxLCLInputText(LEditor, CDraft);
    LPrevious := TEditorContextAccess(LEditor).OnContextPopup;
    TEditorContextAccess(LEditor).OnContextPopup := PreviousEditorContext;
    LPreviousContexts := FEditorContexts;
    {$endif}
    try
      Check(Supports(FCellActions, INyxCollectionCellActionIntent, LIntent),
        'actual opt-in editor retains the canonical context-intent witness');
      LOriginalIntent := LIntent.Intent;
      LContexts := FContexts;
      LReentries := FContextScheduler.Reentries;
      LBefore := FHost.Writes;
      FMode := kmObserve;
      FContextScheduler.Owner := Self;
      EditorContext;
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      LNestedMenu := FCellActions.Menu;
      LNestedCapture := LDraftActions.CapturedDraft;
      Check((FContextScheduler.Reentries = LReentries + 1) and
        (FContexts = LContexts + 1) and (LIntent.Intent = LOriginalIntent + 1) and
        (LNestedMenu <> nil) and LNestedMenu.IsOpen,
        'nested real context wins once while the older editor admission cannot replay canonical delivery');
      {$ifdef PAS2JS}
      Check(LContextClaimed,
        'stale original editor context prevents the browser default after the nested menu wins');
      {$else}
      Check(FEditorContexts = LPreviousContexts,
        'stale original editor context never replays the inherited native context callback/default');
      {$endif}
      Check((LNestedCapture <> nil) and (LNestedCapture.Value.Text = CDraft) and
        (LNestedCapture.Value.RawText = CText) and
        LNestedCapture.Value.Field.Same(NameField) and
        (LNestedCapture.Value.Item.ID = ItemA.ID) and
        (FHost.Writes = LBefore) and (FView.Store.Snapshot = LSource),
        'newer menu retains exact original draft/RAW provenance with no accepted-data or clipboard effect');
    finally
      FContextScheduler.Owner := nil;
      {$ifndef PAS2JS}TEditorContextAccess(LEditor).OnContextPopup := LPrevious;{$endif}
    end;
    FCellActions.Close;
    {$ifdef PAS2JS}
    Key(LEditor, 'Escape', False);
    await(Drain);
    {$else}
    SendMessage(FGrid.Handle, WM_KEYDOWN, VK_ESCAPE, 1);
    Drain;
    {$endif}
  finally
    FContextColumn := 0;
    FMode := kmObserve;
  end;
  Check((LCapture.Value.Text = '-') and (LCapture.Value.RawText = '1') and
    (FView.Store.Snapshot = LSource) and (TNyxCodec.Encode(FDocument) = FAuthored),
    'managed draft presentation/copy leaves all accepted rows and authored source exact');
end;

procedure TCopyReview.ExportPaging(AEnabled: Boolean);
var
  LPages: INyxCollectionPages;
  LPolicy: TNyxCollectionPaging;
begin
  LPages := NyxCollectionPages(FView);
  LPolicy := NyxCollectionPaging;

  if AEnabled then
  begin
    LPolicy := LPolicy.PageSize(1);
  end;
  LPages.ConfigurePaging(LPolicy);
end;

procedure TCopyReview.RunExport; {$ifdef PAS2JS}async;{$endif}
var
  LExportActions: INyxCollectionCellExportActions;
  LPayload: INyxCollectionExportText;
  LBefore: Integer;
  LRevision: Integer;
  LSource: INyxCollectionSnapshot;
  LInspector: INyxPopover;
begin
  Check(Supports(FCellActions, INyxCollectionCellExportActions, LExportActions) and
    LExportActions.ExportEnabled, 'actual cell context opts into injected RAW export authority');
  LRevision := FView.Store.Snapshot.Revision;
  LSource := FView.Store.Snapshot;
  ExportPaging(True);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check(FView.Snapshot.Count = 1, 'actual table page is smaller than its accepted source before export');
  FMode := kmObserve;
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  MenuAction(NyxCellActionsExportPageID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  LPayload := FExport.ExportCommands.LastPayload;
  Check((FSaveHost.Saves = 1) and (LPayload <> nil) and
    (LPayload.RowCount = 1) and (LPayload.ItemAt(0).ID = ItemA.ID) and
    (LPayload.CellValue(0, 0).TextValue = CText),
    'real managed Export current page uses captured exact accepted cell text and membership');
  Check((NyxDecodeUTF8(FSaveHost.Bytes) = LPayload.Text) and
    (LExportActions.LastExportResult.Outcome = nfsoWritten) and
    (LExportActions.LastExportResult.ByteCount = Length(FSaveHost.Bytes)),
    'actual page action hands exact RAW TSV bytes to the injected host once');
  LInspector := FCellActions.Inspector;
  Check((LInspector <> nil) and LInspector.IsOpen and
    (LInspector.State.GetValue(NyxTextState(NyxCellInspectorStatusState)) <> ''),
    'actual managed export presents its terminal save status');
  InspectorAction(NyxCellInspectorCloseID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  FSaveHost.Outcome := nfsoDownloadRequested;
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  MenuAction(NyxCellActionsExportFilteredID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  LPayload := FExport.ExportCommands.LastPayload;
  Check((FSaveHost.Saves = 2) and (LPayload.RowCount = 2) and
    (LPayload.ItemAt(0).ID = ItemA.ID) and (LPayload.ItemAt(1).ID = ItemB.ID) and
    (LExportActions.LastExportResult.Outcome = nfsoDownloadRequested),
    'real all-filtered action exports rows outside the page and distinguishes requested download');
  InspectorAction(NyxCellInspectorCloseID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  EditValue('changed after menu launch');
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  MenuAction(NyxCellActionsExportFilteredID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check(FSaveHost.Saves = 2, 'accepted mutation after menu launch refuses export instead of recapturing newer data');
  FCellActions.Close;
  EditValue(CText);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  FSaveHost.InlineReply := False;
  FSaveHost.Outcome := nfsoWritten;
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$endif}
  MenuAction(NyxCellActionsExportPageID);
  {$ifdef PAS2JS}await(Drain);{$endif}
  Check((FSaveHost.Saves = 3) and (FExport.ExportCommands.Phase = ncepPending),
    'actual context export can retain one pending managed save receiver');
  LBefore := FSaveHost.Saves;
  FMount.SetInteraction(False, False);
  FMount.SetInteraction(True, False);
  FSaveHost.Last.Finish(nfsoWritten);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check((FExport.ExportCommands.Phase = ncepCancelled) and
    (FSaveHost.Saves = LBefore) and (FCellActions.Inspector = nil),
    'actual disable ABA cancels file receiver and old completion cannot reopen or report into a new session');
  FSaveHost.InlineReply := True;
  ExportPaging(False);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check((FView.Store.Snapshot.Revision = LRevision + 2) and
    FView.Store.Snapshot.Item(ItemA).SameItem(LSource.Item(ItemA)) and
    FView.Store.Snapshot.Item(ItemB).SameItem(LSource.Item(ItemB)) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'all export actions preserve source values, authored defaults and only the two explicit test edits advance data');
end;

procedure TCopyReview.Run; {$ifdef PAS2JS}async;{$endif}
var
  LBefore: Integer;
  LKeys: Integer;
  LConsumed: Integer;
  LCommand: INyxCollectionClipboardCommand;
  LPayload: INyxCollectionClipboardText;
  LCellIntent: INyxCollectionCellActionIntent;
  {$ifdef PAS2JS}
  LEditor: TJSHTMLInputElement;
  {$else}
  LEditor: TCustomEdit;
  LFallback: Integer;
  LEditorContexts: Integer;
  LPreviousContext: TContextPopupEvent;
  {$endif}
begin
  {$ifdef PAS2JS}
  await(Drain);
  {$endif}
  FocusCell;
  {$ifdef PAS2JS}
  await(Drain);
  {$endif}
  LKeys := FKeys;
  {$ifdef PAS2JS}
  Check(CopyKey, 'scoped cell Ctrl+C prevents the browser copy default');
  {$else}
  { The native Boolean reports completed paired dispatch, not OS consumption.
    The receipt, canonical-consumption and fallback assertions below establish
    this grid route's actual handling without accessing any system clipboard. }
  Check(CopyKey, 'scoped cell Ctrl+C completes its paired native dispatch');
  {$endif}
  Check((FKeys = LKeys + 1) and (FHost.Writes = 1) and (FReplies = 1) and
    (FHost.Text = CText) and (FLastPayload.ItemAt(0).ID = ItemA.ID) and
    FLastPayload.FieldAt(0).Same(NameField),
    'real table key copies accepted raw NUL/newlines/Unicode once with typed provenance');
  FMode := kmConsume;
  LConsumed := FConsumed;
  CopyKey;
  Check((FConsumed = LConsumed + 1) and (FHost.Writes = 1),
    'consumed real canonical key cannot begin a clipboard write');
  FMode := kmMutate;
  CopyKey;
  Check((FHost.Writes = 1) and
    (FView.Store.Snapshot.Item(ItemA).GetValue(NyxTextField('Name')) =
      TNyxText('later accepted 🌙')),
    'canonical data mutation refuses original default instead of copying a new value');
  FMode := kmObserve;
  EditValue(CText);
  {$ifdef PAS2JS}
  await(Drain);
  {$else}
  Drain;
  {$endif}
  FocusCell;
  FMode := kmDisableABA;
  CopyKey;
  Check((FHost.Writes = 1) and FInput.Ready,
    'disable/re-enable inside the real key invalidates its original input epoch');
  FMode := kmNested;
  CopyKey;
  Check(FHost.Writes = 2, 'newer nested typed copy wins without outer same-text replay');
  FMode := kmObserve;
  FView.SetSelection([ItemA, ItemB], ItemA, ItemA);
  Filter(True);
  {$ifdef PAS2JS}
  await(Drain);
  {$else}
  Drain;
  {$endif}
  LCommand := FCommands.BeginRows(nccmSelectedPage);
  LCommand.Prepare;
  Check((LCommand.Payload.RowCount = 1) and (LCommand.Payload.ItemAt(0).ID = ItemB.ID),
    'mounted explicit page command captures the actual filtered page');
  LCommand.Complete(True);
  LCommand := FCommands.BeginRows(nccmAllSelectedSource);
  LCommand.Prepare;
  Check((LCommand.Payload.RowCount = 2) and
    (LCommand.Payload.CellValue(0, 0).TextValue = CText),
    'mounted explicit source command retains filtered selected membership');
  LCommand.Complete(True);
  LCommand := nil;
  Filter(False);
  {$ifdef PAS2JS}
  await(Drain);
  {$else}
  Drain;
  {$endif}
  FocusCell;
  {$ifdef PAS2JS}
  await(Drain);
  Key(Cell, 'F2', False);
  await(Drain);
  LEditor := TJSHTMLInputElement(Cell.querySelector('input'));
  Check((LEditor <> nil) and (document.activeElement = LEditor),
    'real F2 owns the browser cell editor');
  LEditor.value := CDraft;
  FMode := kmObserve;
  LBefore := FHost.Writes;
  Check(not Key(LEditor, 'c', True),
    'synthetic editor Ctrl+C stays unclaimed by the data-copy route');
  Check((FHost.Writes = LBefore) and (LEditor.value = CDraft),
    'browser data host never captures or commits explicit editor text');
  Key(LEditor, 'Escape', False);
  await(Drain);
  {$else}
  SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
  Drain;
  Check(FGrid.EditorMode and (FGrid.Editor is TCustomEdit), 'real F2 owns the native editor');
  LEditor := TCustomEdit(FGrid.Editor);
  RestoreNyxLCLInputText(LEditor, CDraft);
  Check(NyxLCLInputText(LEditor) = CDraft, 'native editor holds exact supplementary draft');
  FMode := kmConsume;
  LConsumed := FConsumed;
  LBefore := FHost.Writes;
  LFallback := GFallbackCopies;
  CopyKey;
  Check((FConsumed = LConsumed + 1) and (GFallbackCopies = LFallback),
    'actual native canonical consumption prevents the no-effect LCL fallback');
  Check((FHost.Writes = LBefore) and FGrid.EditorMode and
    (NyxLCLInputText(LEditor) = CDraft),
    'native grid-owned key route with active editor never captures or accepts its draft');
  LEditorContexts := FEditorContexts;
  LPreviousContext := TEditorContextAccess(LEditor).OnContextPopup;
  TEditorContextAccess(LEditor).OnContextPopup := PreviousEditorContext;
  try
    SendMessage(LEditor.Handle, WM_CONTEXTMENU, LEditor.Handle, MakeLParam(-1, -1));
    Drain;
  finally
    TEditorContextAccess(LEditor).OnContextPopup := LPreviousContext;
  end;
  Check((FEditorContexts = LEditorContexts + 1) and
    (FHost.Writes = LBefore) and FGrid.EditorMode and
    (NyxLCLInputText(LEditor) = CDraft) and (FCellActions.Menu = nil),
    'default-off native editor retains its actual ordinary context callback and exact unfinished text');
  SendMessage(FGrid.Handle, WM_KEYDOWN, VK_ESCAPE, 1);
  Drain;
  {$endif}
  FMode := kmObserve;
  FocusCell;
  Check((FView.Store.Snapshot.Item(ItemA).GetValue(NyxTextField('Name')) = CText) and
    (FView.Store.Snapshot.Item(ItemB).GetValue(NyxIntegerField('Score')) = 2) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'copy/consumption/editor ownership preserve accepted data and authored defaults');
  LPayload := FLastPayload;
  {$ifdef PAS2JS}
  await(RunContext);
  await(RunExport);
  await(RunDisplayed);
  await(RunFocusedDraft);
  await(RunCellRange);
  {$else}
  RunContext;
  RunExport;
  RunDisplayed;
  RunFocusedDraft;
  RunCellRange;
  {$endif}
  Check(Supports(FCellActions, INyxCollectionCellActionIntent, LCellIntent),
    'retirement probe retains the actual canonical context facade');
  LBefore := FHost.Writes;
  FMode := kmRetire;
  CopyKey;
  Check(FInput.Retired and not FMount.Connected and (FHost.Writes = LBefore),
    'canonical callback retirement cannot publish from an old key tail');
  Check(LCellIntent.Intent = 0,
    'retained context facade safely reports terminal intent after actual renderer retirement');
  Check((FLastDraft <> nil) and (FLastDraft.Value.Text = '-') and
    (FLastDraft.Value.RawText = '1') and
    (FLastDraft.Value.Value.IntegerValue = 1),
    'detached explicit draft remains usable after actual renderer/input retirement');
  Check((LPayload.RowCount = 2) and (LPayload.CellValue(0, 0).TextValue = CText),
    'accepted copied payload survives actual renderer and input retirement');
  {$ifndef PAS2JS}
  Check(GFallbackCopies = 0, 'no native test attempted any real LCL clipboard default');
  {$endif}
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LReview: TCopyReview;
begin
  try
    LReview := TCopyReview.Create;
    try
      {$ifdef PAS2JS}
      await(LReview.Run);
      {$else}
      LReview.Run;
      {$endif}
    finally
      LReview.Free;
    end;
    { Same maintained fixture/spec/data and public input path, with only the
      optional layout controls absent. Renderer-managed native observation must
      not be a benefit available only to the arrangement authoring feature. }
    LReview := TCopyReview.Create(False, True);
    try
      Check(not LReview.FView.Spec.HasLayoutControls,
        'ordinary renderer-managed table genuinely omits arrangement controls');
      {$ifdef PAS2JS}
      await(LReview.RunFocusedDraft);
      await(LReview.RunDraftActions);
      await(LReview.RunCellRange);
      {$else}
      LReview.RunFocusedDraft;
      LReview.RunDraftActions;
      LReview.RunCellRange;
      {$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(True, True);
    try
      {$ifdef PAS2JS}await(LReview.RunDraftActions);{$else}LReview.RunDraftActions;{$endif}
    finally
      LReview.Free;
    end;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-clipboard-controls', 'passed');
    document.body.setAttribute('data-clipboard-control-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection clipboard control checks; injected host/no OS write');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-clipboard-controls', 'failed');
      document.body.setAttribute('data-clipboard-control-checks', IntToStr(GChecks));
      document.body.setAttribute('data-clipboard-control-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifndef PAS2JS}
  Application.Initialize;
  Application.ShowMainForm := False;
  GErrors := TNativeErrors.Create;
  Application.OnException := GErrors.Failed;
  {$endif}
  try
    Run;
  finally
    {$ifndef PAS2JS}
    Application.OnException := nil;
    GErrors.Free;
    {$endif}
  end;
end.
