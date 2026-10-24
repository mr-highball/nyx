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
  nyx.collections.range.navigation, nyx.collections.range.pointer, nyx.gestures,
  nyx.collections.workbench.policy, nyx.collections.workbench.runtime,
  nyx.collections.identities,
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
  { Standard cancellable PointerEvent enters the mounted table's real down/up
    handlers. Synthetic events prove canonical/default routing, not trusted
    hardware, touch-device capture or native operating-system input. }
  TRangePointerEvent = class external name 'PointerEvent'(TJSPointerEvent)
  public
    constructor new(const AType: String; AInit: TJSObject); reintroduce;
  end;
  {$endif}

  { Each callback mode changes only the original real key's semantic phase.
    Newer copy is a separate typed command; no physical event is replayed. }
  TKeyMode = (kmObserve, kmConsume, kmMutate, kmDisableABA, kmNested, kmRetire,
    kmCapture);

  { Test owner outlives every subscription and injected host receiver. }
  TCopyReview = class;

  { Delegates every ordinary scheduler operation. A one-shot UI admission
    callback enters a real nested context after the actions owner captured its
    original intent, exercising stale target return without private-field edits.
    The independent replacement mode changes only the accepted range, proving
    an old context cannot borrow identical newer membership after this callout. }
  TContextScheduler = class(TInterfacedObject, INyxScheduler)
  public
    Base: INyxScheduler;
    Owner: TCopyReview;
    Reentries: Integer;
    ReplaceRangeOnly: Boolean;
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
    and an explicitly consuming canonical callback, not an OS editor shortcut.
    InvokeGridClick enters the installed virtual LCL boundary without claiming
    a trusted pointer gesture; the optional WM probe is reported separately. }
  TNoClipboardGrid = class(TNyxCollectionStringGrid)
  protected
    procedure DoCopyToClipboard; override;
  public
    procedure InvokeGridClick;
  end;

  { Access to the SAME grid's virtual mouse boundary, including pinned panes.
    No cursor/desktop state or native clipboard default is exercised. Calling
    these methods is explicitly an LCL producer, not trusted hardware input. }
  TRangePointerAccess = class(TNyxCollectionStringGrid)
  public
    procedure InvokePointer(AShift: TShiftState; AX, AY: Integer);
  end;

  { Native callback failures are captured instead of opening a modal dialog. }
  TNativeErrors = class
  public
    Text: TNyxText;
    procedure Failed(Sender: TObject; E: Exception);
  end;
  {$endif}

  { One real renderer/data control owns all cases. Runtime data is independent
    from authored source; the host/payload are injected through public facets.
    Native final selection callbacks retire this same owner only after all its
    ordinary cases, exercising physical-frame safety without a second fixture. }
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
    FClickToken: INyxEventSubscription;
    FPointerToken: INyxEventSubscription;
    FPointerUpToken: INyxEventSubscription;
    FContextScheduler: TContextScheduler;
    FContextSchedulerLease: INyxScheduler;
    FCellMount: INyxCollectionCellActionsMount;
    FCellActions: INyxCollectionCellActions;
    FExport: INyxCollectionExportMount;
    FSaveHost: TSaveHost;
    FSaveHostLease: INyxFileSaveHost;
    FMode: TKeyMode;
    FKeys: Integer;
    FRangeKeys: Integer;
    FRangeClicks: Integer;
    FRangeClickPhase: TNyxText;
    FRangeKeyName: TNyxText;
    FRangeReplacements: Integer;
    FRangeKeySending: Boolean;
    FRangePointerSending: Boolean;
    FRangeStrokeSending: Boolean;
    FRangePointers: Integer;
    FRangePointerUps: Integer;
    FPointerCallbackRecorded: Boolean;
    FPointerCallbackRow: Integer;
    FPointerCallbackColumn: Integer;
    FConsumed: Integer;
    FContexts: Integer;
    FInspectorLegacyClicks: Integer;
    FReplies: Integer;
    FLastPayload: INyxCollectionClipboardText;
    FContextColumn: Integer;
    FLastDraft: INyxCollectionFocusedDraftCapture;
    { Only the new authored-policy journey omits manual mount configuration.
      Its missing-service branch still uses actual managed menus and inputs. }
    FAuthoredWorkbench: Boolean;
    FUnavailableClipboard: Boolean;
    {$ifndef PAS2JS}
    FRetireRangeSelection: Boolean;
    FRangeSelectionCallbacks: Integer;
    {$endif}
    {$ifdef PAS2JS}
    FRenderer: TNyxBrowserRenderer;
    FSurface: TJSHTMLElement;
    FTable: TJSHTMLElement;
    FPointerCallbackFocus: TJSHTMLElement;
    function Cell: TJSHTMLElement;
    function Key(AElement: TJSHTMLElement; const AKey: String;
      AControl: Boolean): Boolean;
    {$else}
    FRenderer: TNyxLCLRenderer;
    FSurface: TForm;
    FGrid: TNoClipboardGrid;
    FPointerCallbackFocus: TWinControl;
    FEditorContexts: Integer;
    procedure NativeRangeSelected(Sender: TObject; AColumn, ARow: Integer);
    procedure RunRangeFocusRetirement;
    function RunNativePointerProbe: Boolean;
    procedure PreviousEditorContext(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    {$endif}
    procedure FocusCell;
    function CopyKey: Boolean;
    procedure RangeKey(const AKey: TNyxText; AShift: Boolean = True;
      AControl: Boolean = False);
    procedure EditValue(const AText: TNyxText);
    procedure Filter(AEnabled: Boolean);
    procedure Context(AWait: Boolean = True);
    procedure MenuAction(const AID: TNyxText);
    function HasMenuAction(const AID: TNyxText): Boolean;
    function HasInspectorAction(const AID: TNyxText): Boolean;
    procedure InspectorAction(const AID: TNyxText);
    procedure RunContext; {$ifdef PAS2JS}async;{$endif}
    procedure RunExport; {$ifdef PAS2JS}async;{$endif}
    procedure RunDisplayed; {$ifdef PAS2JS}async;{$endif}
    procedure RunFocusedDraft; {$ifdef PAS2JS}async;{$endif}
    procedure RunDraftActions; {$ifdef PAS2JS}async;{$endif}
    procedure RunCellRange; {$ifdef PAS2JS}async;{$endif}
    procedure RunPointerRange; {$ifdef PAS2JS}async;{$endif}
    procedure RunRangeMenu; {$ifdef PAS2JS}async;{$endif}
    procedure RunAuthoredWorkbench; {$ifdef PAS2JS}async;{$endif}
    procedure RunAuthoredInspection; {$ifdef PAS2JS}async;{$endif}
    procedure RunAuthoredRowMenu; {$ifdef PAS2JS}async;{$endif}
    procedure ReplaceContextRange;
    procedure ExportPaging(AEnabled: Boolean);
    procedure Semantic(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure CloseInspectorOnClick(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Reply(const APayload: INyxCollectionClipboardText;
      const AResult: TNyxClipboardWriteResult);
  public
    constructor Create(AArranged: Boolean = True; ADraftActions: Boolean = False;
      AAuthoredWorkbench: Boolean = False; AUnavailableClipboard: Boolean = False;
      ACopyActions: TNyxWorkbenchCopyActions =
        [nwcaCell, nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange];
      AAuthoredExports: Boolean = False);
    destructor Destroy; override;
    procedure Run; {$ifdef PAS2JS}async;{$endif}
  end;

const
  CTable = 'clipboard-table';
  CText: TNyxText = 'accepted 🌙' + #0 + 'value' + #13#10 + 'next';
  CDraft: TNyxText = 'unfinished editor 🌙';

var
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRangeCapturedID: NativeInt = -1;
  GRangeCaptureRefused: Boolean;
  GRangeCaptures: Integer;
  {$endif}
  {$ifndef PAS2JS}
  GFallbackCopies: Integer;
  GErrors: TNativeErrors;
  GPointerUnavailable: Boolean;
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
{ Explicit injected platform authority for synthetic pointers. The real DOM hit
  test, canonical callbacks, mounted capture lifecycle and release remain live.
  These seams make no trusted hardware or browser-device capture claim. }
procedure CaptureRangePointer(AID: NativeInt);
begin

  if GRangeCaptureRefused then
  begin
    raise Exception.Create('Injected platform capture refusal');
  end;
  GRangeCapturedID := AID;
  Inc(GRangeCaptures);
end;

function HasRangePointer(AID: NativeInt): Boolean;
begin
  Result := GRangeCapturedID = AID;
end;

procedure ReleaseRangePointer(AID: NativeInt);
begin

  if GRangeCapturedID = AID then
  begin
    GRangeCapturedID := -1;
  end;
end;

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

function Fixture(AArranged: Boolean; AAuthoredWorkbench: Boolean = False;
  ADraftActions: Boolean = False; ACopyActions: TNyxWorkbenchCopyActions =
    [nwcaCell, nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange]): TNyxDocument;
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

    if AAuthoredWorkbench then
    begin
      LSpec := LSpec.TableWorkbench(NyxTableWorkbench.RangeSelection(True)
        .AllowCopy(ACopyActions)
        .Headers(False).LineEnding(nwleCRLF).FocusedDraft(ADraftActions));
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
  LReplaceRange: Boolean;
begin
  Base.RequireUI;
  LOwner := Owner;
  Owner := nil;
  LReplaceRange := ReplaceRangeOnly;
  ReplaceRangeOnly := False;

  if LOwner <> nil then
  begin
    Inc(Reentries);

    if LReplaceRange then
    begin
      LOwner.ReplaceContextRange;
    end
    else
    begin
      LOwner.Context(False);
    end;
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

procedure TNoClipboardGrid.InvokeGridClick;
begin
  Click;
end;

procedure TRangePointerAccess.InvokePointer(AShift: TShiftState; AX, AY: Integer);
begin
  MouseDown(mbLeft, AShift + [ssLeft], AX, AY);
  Click;
  MouseUp(mbLeft, AShift, AX, AY);
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

constructor TCopyReview.Create(AArranged: Boolean; ADraftActions: Boolean;
  AAuthoredWorkbench: Boolean; AUnavailableClipboard: Boolean;
  ACopyActions: TNyxWorkbenchCopyActions; AAuthoredExports: Boolean);
var
  LCallback: TKeyCallback;
  LAdmission: INyxCollectionInputAdmission;
  LOptionsMount: INyxCollectionCellActionOptionsMount;
  LFactory: INyxCollectionCellPresenterFactory;
begin
  inherited Create;
  FAuthoredWorkbench := AAuthoredWorkbench;
  FUnavailableClipboard := AUnavailableClipboard;
  FDocument := Fixture(AArranged, AAuthoredWorkbench, ADraftActions, ACopyActions);
  FAuthored := TNyxCodec.Encode(FDocument);
  FState := FDocument.State.Clone;
  FHost := THost.Create;
  FHostLease := FHost;

  if AAuthoredExports then
  begin
    FSaveHost := TSaveHost.Create;
    FSaveHostLease := FSaveHost;
    FSaveHost.InlineReply := True;
    FSaveHost.Outcome := nfsoWritten;
  end;
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

  if FAuthoredWorkbench and not FUnavailableClipboard then
  begin

    if AAuthoredExports then
    begin
      FRenderer.TableWorkbenchServices := NewNyxTableWorkbenchServices(
        FHostLease, FSaveHostLease, TNyxCollectionExportOptions.Defaults);
    end
    else
    begin
      FRenderer.TableWorkbenchServices := NewNyxTableWorkbenchServices(FHostLease);
    end;
  end;
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

  if not FAuthoredWorkbench then
  begin
    FClipboard.ConfigureClipboard(FHostLease, FRenderer.Events,
      FRenderer.Events.Scheduler, TNyxCollectionClipboardOptions.Defaults.AllowOnly(
        [nccaCell, nccaSelectedSource, nccaSelectedPage, nccaCellRange]));
  end;
  FCommands := FClipboard.ClipboardCommands;

  if FAuthoredWorkbench then
  begin
    Check(FCommands <> nil,
      'ordinary renderer binds authored copy policy without manual clipboard configuration');
  end;
  FCommands.Observe({$ifdef PAS2JS}@{$endif}Reply);
  Check(Supports(FMount, INyxCollectionExportMount, FExport),
    'actual table exposes optional runtime RAW export authority');

  if not FAuthoredWorkbench then
  begin
    FSaveHost := TSaveHost.Create;
    FSaveHostLease := FSaveHost;
    FSaveHost.InlineReply := True;
    FSaveHost.Outcome := nfsoWritten;
    FExport.ConfigureExport(FSaveHostLease, FRenderer.Events,
      FRenderer.Events.Scheduler, TNyxCollectionExportOptions.Defaults);
  end;
  Check(Supports(FMount, INyxCollectionCellActionsMount, FCellMount),
    'actual mounted table exposes managed cell context actions');
  if not FAuthoredWorkbench then
  begin
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
  end;
  FCellActions := FCellMount.CellActions;
  LCallback := TKeyCallback.Create;
  LCallback.Owner := Self;
  FCallback := LCallback;
  FToken := FRenderer.Events.OnKeyDown(NyxControlEvents(CTable, niRuntime)).Subscribe(FCallback);
  FContextToken := FRenderer.Events.OnContextMenu(NyxControlEvents(CTable, niRuntime)).Subscribe(FCallback);
  FClickToken := FRenderer.Events.On(NyxControlEvents(CTable, niRuntime), ntClick).Subscribe(FCallback);
  FPointerToken := FRenderer.Events.On(NyxControlEvents(CTable, niRuntime), ntPointerDown).Subscribe(FCallback);
  FPointerUpToken := FRenderer.Events.On(NyxControlEvents(CTable, niRuntime), ntPointerUp).Subscribe(FCallback);
end;

destructor TCopyReview.Destroy;
begin

  if FPointerToken <> nil then
  begin
    FPointerToken.Cancel;
  end;
  FPointerToken := nil;

  if FPointerUpToken <> nil then
  begin
    FPointerUpToken.Cancel;
  end;
  FPointerUpToken := nil;

  if FClickToken <> nil then
  begin
    FClickToken.Cancel;
  end;
  FClickToken := nil;

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

procedure TCopyReview.RangeKey(const AKey: TNyxText; AShift: Boolean;
  AControl: Boolean);
var
  LPrevious: Boolean;
  {$ifdef PAS2JS}
  LInit: TJSObject;
  LEvent: TJSKeyboardEvent;
  LTarget: TJSHTMLElement;
  {$else}
  LSaved: TKeyboardState;
  LState: TKeyboardState;
  LKey: Word;
  LHandle: HWND;
  {$endif}
begin
  LPrevious := FRangeKeySending;
  FRangeKeyName := AKey;
  FRangeKeySending := True;
  try
    {$ifdef PAS2JS}
    LInit := TJSObject.new;
    LInit['key'] := AKey;
    LInit['shiftKey'] := AShift;
    LInit['ctrlKey'] := AControl;
    LInit['bubbles'] := True;
    LInit['cancelable'] := True;
    LEvent := TCopyKeyEvent.new('keydown', LInit);
    LTarget := TJSHTMLElement(document.activeElement);
    Check((LTarget <> nil) and FTable.contains(LTarget),
      'range key enters the currently focused actual table descendant');
    LTarget.dispatchEvent(LEvent);
    Check(LEvent.defaultPrevented,
      'explicit cell range key is consumed through the actual canonical DOM route');
    {$else}
    LKey := 0;

    if AKey = 'Home' then
    begin
      LKey := VK_HOME;
    end
    else if AKey = 'End' then
    begin
      LKey := VK_END;
    end
    else if AKey = 'ArrowDown' then
    begin
      LKey := VK_DOWN;
    end
    else if AKey = 'ArrowUp' then
    begin
      LKey := VK_UP;
    end
    else if AKey = 'ArrowLeft' then
    begin
      LKey := VK_LEFT;
    end
    else if AKey = 'ArrowRight' then
    begin
      LKey := VK_RIGHT;
    end
    else if AKey = 'Escape' then
    begin
      LKey := VK_ESCAPE;
    end;
    Check((LKey <> 0) and (Screen.ActiveControl is TCustomGrid),
      'range key targets a real currently focused outer or pinned grid');
    LHandle := Screen.ActiveControl.Handle;
    Check(GetKeyboardState(LSaved), 'capture exact native range modifiers');
    LState := LSaved;
    LState[VK_CONTROL] := 0;
    LState[VK_LCONTROL] := 0;
    LState[VK_RCONTROL] := 0;
    LState[VK_SHIFT] := 0;
    LState[VK_LSHIFT] := 0;
    LState[VK_RSHIFT] := 0;

    if AControl then
    begin
      LState[VK_CONTROL] := $80;
      LState[VK_LCONTROL] := $80;
    end;

    if AShift then
    begin
      LState[VK_SHIFT] := $80;
      LState[VK_LSHIFT] := $80;
    end;
    Check(SetKeyboardState(LState), 'install explicit native range modifiers');
    try
      SendMessage(LHandle, WM_KEYDOWN, LKey, 1);

      if FInput.Current and IsWindow(LHandle) then
      begin
        SendMessage(LHandle, WM_KEYUP, LKey, LPARAM($C0000001));
      end;
    finally
      Check(SetKeyboardState(LSaved), 'restore exact caller range modifiers');
    end;
    {$endif}
  finally
    FRangeKeySending := LPrevious;
  end;
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
  LRange: INyxCollectionCellRangeMount;
  LGesture: INyxGestureResponse;
  LEpoch: Integer;
begin

  if FRangeStrokeSending and (AEvent.Trigger = ntPointerMove) then
  begin

    if FMode = kmConsume then
    begin
      Check(Supports(AExecution, INyxEventResponse, LResponse) and
        LResponse.CanConsume, 'Original managed move admits canonical consumption');
      LResponse.Consume;
    end;
    Exit;
  end;

  if FRangePointerSending and (AEvent.Trigger = ntPointerUp) then
  begin
    Inc(FRangePointerUps);
    Exit;
  end;

  if FRangePointerSending and (AEvent.Trigger = ntPointerDown) then
  begin
    Inc(FRangePointers);
    Check(AEvent.HasPointer and (AEvent.OriginID = CTable) and
      (AEvent.SourceID = CTable) and (AEvent.Pointer.Button = npbPrimary) and
      (nmShift in AEvent.Pointer.Modifiers),
      'managed pointer keeps the actual original canonical identity/button/modifiers');
    Check(Supports(AExecution, INyxEventResponse, LResponse) and
      LResponse.CanConsume and Supports(AExecution, INyxGestureResponse, LGesture) and
      LGesture.CanRequest(ngcCapturePointer),
      'one actual managed pointer execution admits both input and gesture decisions');
    case FMode of
      kmCapture:
        begin
          LGesture.CapturePointer;
        end;
      kmConsume:
        begin
          LResponse.Consume;
        end;
      kmDisableABA:
        begin
          FMount.SetInteraction(False, False);
          FMount.SetInteraction(True, False);
          { Disabling a real native control can legitimately move focus itself.
            Freeze the callback's accepted result BEFORE the stale default gets
            a chance to borrow the restored Ready flag. }
          Check(LGesture.CanRequest(ngcCapturePointer),
            'same-view callback may record capture but cannot grant a new input epoch');
          LGesture.CapturePointer;
          FPointerCallbackRecorded := True;
          {$ifdef PAS2JS}
          FPointerCallbackFocus := TJSHTMLElement(document.activeElement);
          {$else}
          FPointerCallbackFocus := Screen.ActiveControl;
          FPointerCallbackRow := -1;
          FPointerCallbackColumn := -1;

          if FPointerCallbackFocus is TNyxCollectionStringGrid then
          begin
            FPointerCallbackRow := TNyxCollectionStringGrid(FPointerCallbackFocus).Row;
            FPointerCallbackColumn := TNyxCollectionStringGrid(FPointerCallbackFocus).Col;
          end;
          {$endif}
        end;
      kmNested:
        begin
          Check(Supports(FMount, INyxCollectionCellRangeMount, LRange) and
            LRange.SelectCells(NyxCollectionCellIdentity(ItemB, NameField),
              NyxCollectionCellIdentity(ItemB, NyxColumnIdentity(NyxIntegerField('Score')))),
            'nested canonical pointer callback installs its own newer exact rectangle');
        end;
      kmMutate:
        begin
          FMount.Refresh;
        end;
      else
        begin
          { Observing the one original callback leaves its default admitted. }
        end;
    end;
    Exit;
  end;

  if AEvent.Trigger = ntClick then
  begin
    Inc(FRangeClicks);

    if FRangeClickPhase = '' then
    begin
      FRangeClickPhase := FRangeKeyName + '/mode=' + TNyxText(IntToStr(Ord(FMode))) +
        '/sending=' + TNyxText(BoolToStr(FRangeKeySending, True));
    end;
    Exit;
  end;

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

  if FRangeKeySending and AEvent.HasKeyboard then
  begin
    Check((AEvent.OriginID = CTable) and (AEvent.SourceID = CTable),
      'real range key preserves its original canonical table identity');
    Inc(FRangeKeys);
    case FMode of
      kmConsume:
      begin
        Check(Supports(AExecution, INyxEventResponse, LResponse) and LResponse.CanConsume,
          'range callback can consume the original physical default');
        LResponse.Consume;
      end;
      kmDisableABA:
      begin
        FMount.SetInteraction(False, False);
        FMount.SetInteraction(True, False);
      end;
      kmMutate:
      begin
        LEpoch := FInput.Epoch;
        FMount.Refresh;
        Check(FInput.Current and FInput.Ready and (FInput.Epoch = LEpoch),
          'benign same-source refresh restores the same admitted input epoch');
      end;
      kmNested:
      begin
        Check(Supports(FMount, INyxCollectionCellRangeMount, LRange) and
          LRange.SelectCells(NyxCollectionCellIdentity(ItemB, NameField),
            NyxCollectionCellIdentity(ItemB, NyxColumnIdentity(NyxIntegerField('Score')))),
          'nested canonical range callback installs its own newer actual cell selection');
      end;
      else
      begin
        { Observation/terminal cases retain their original ordinary behavior. }
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
    kmObserve, kmCapture:
      begin
        { Capture mode changes only the original pointer callback above. }
      end;
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

  if FUnavailableClipboard then
  begin
    Check((AResult.Outcome = ncwoUnavailable) and (AResult.ByteCount = 0) and
      (AResult.Representation = nctrNone) and (AResult.Error <> ''),
      'actual absent-service input gets one explicit non-write receipt and diagnostic');
  end
  else
  begin
    Check((AResult.Outcome = ncwoWritten) and (AResult.ByteCount = APayload.ByteCount),
      'actual input gets one typed receipt with original bytes');
  end;
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

function TCopyReview.HasMenuAction(const AID: TNyxText): Boolean;
var
  {$ifdef PAS2JS}
  LMenu: INyxBrowserMenu;
  {$else}
  LMenu: INyxLCLMenu;
  {$endif}
begin
  Result := False;

  if (FCellActions.Menu = nil) or not FCellActions.Menu.IsOpen then
  begin
    Exit;
  end;

  if Supports(FCellActions.Menu,
    {$ifdef PAS2JS}INyxBrowserMenu{$else}INyxLCLMenu{$endif}, LMenu) then
  begin
    { FocusFor deliberately raises for unknown bindings. The retained public
      menu tree answers optional membership before querying a physical face. }

    if LMenu.Presentation.Renderer.Root.Find(AID) = nil then
    begin
      Exit;
    end;
    Result := LMenu.Presentation.Renderer.FocusFor(AID) <> nil;
  end;
end;

{ Query the ordinary inspector's public tree before touching a physical face.
  Missing copy controls are intentional for readonly authored policies. }
function TCopyReview.HasInspectorAction(const AID: TNyxText): Boolean;
var
  {$ifdef PAS2JS}
  LInspector: INyxBrowserPopover;
  {$else}
  LInspector: INyxLCLPopover;
  {$endif}
begin
  Result := False;

  if (FCellActions.Inspector = nil) or not FCellActions.Inspector.IsOpen then
  begin
    Exit;
  end;

  if Supports(FCellActions.Inspector,
    {$ifdef PAS2JS}INyxBrowserPopover{$else}INyxLCLPopover{$endif}, LInspector) then
  begin

    if LInspector.Renderer.Root.Find(AID) = nil then
    begin
      Exit;
    end;
    Result := LInspector.Renderer.FocusFor(AID) <> nil;
  end;
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
  LKeyInput: INyxCollectionCellRangeKeyInput;
  LKeyCommand: INyxCollectionCellRangeKeyCommand;
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
  LFirst: TNyxCollectionCellIdentity;
  LLastField: TNyxColumnIdentity;
  LAnchorField: TNyxColumnIdentity;
  LBefore: Integer;
  LKeys: Integer;
  LReplies: Integer;
  LIndex: Integer;
  LClicks: Integer;
  {$ifdef PAS2JS}
  LFocusBefore: TJSHTMLElement;
  {$else}
  LFocusBefore: TNyxCollectionStringGrid;
  LFocusRow: Integer;
  LFocusColumn: Integer;
  {$endif}
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
    { Real range keyboard input starts from the same focused middle cell and
      crosses both pinned panes. Ordinary row membership/focus remains separate. }
    FContextColumn := 1;

    if FView.Spec.HasLayoutControls then
    begin
      FContextColumn := 2;
    end;
    LAnchorField := FView.Spec.ColumnAt(FContextColumn).Identity;
    LLastField := FView.Spec.ColumnAt(4).Identity;

    if FView.Spec.HasLayoutControls then
    begin
      LLastField := LScore;
    end;
    {$ifdef PAS2JS}
    Cell.focus;
    {$else}
    FGrid.Col := LPhysical;
    FGrid.Row := 1;
    FGrid.SetFocus;
    LClicks := FRangeClicks;
    FGrid.InvokeGridClick;
    Check(FRangeClicks = LClicks + 1,
      'admitted actual native virtual Click outside the internal transition reaches the installed canonical handler once; no hardware claim');
    {$endif}
    Check(Supports(FMount, INyxCollectionCellRangeKeyInput, LKeyInput),
      'actual mounted range input exposes its synchronous typed prepared envelope');
    LKeyCommand := LKeyInput.BeginCellRangeKey(ncrkExtendRowStart);
    Check(LKeyCommand <> nil, 'opted-in mount reserves the original public range-key envelope');
    LKeyCommand.ResolveOrigin(NyxCollectionCellIdentity(ItemA, LAnchorField));
    LKeyCommand.Prepare;
    {$ifdef PAS2JS}
    LFocusBefore := TJSHTMLElement(document.activeElement);
    {$else}
    LFocusBefore := TNyxCollectionStringGrid(Screen.ActiveControl);
    LFocusRow := LFocusBefore.Row;
    LFocusColumn := LFocusBefore.Col;
    {$endif}
    Check(LKeyCommand.Current and LKeyCommand.Complete(False) and
      LKeyCommand.Complete(True) and (LRanges.CellRange = nil),
      'consumed first completion permanently claims the same prepared key and an allowed retry cannot install its range');
    {$ifdef PAS2JS}
    Check(document.activeElement = LFocusBefore,
      'duplicate allowed completion after consumption cannot move actual browser focus');
    {$else}
    Check((Screen.ActiveControl = LFocusBefore) and (LFocusBefore.Row = LFocusRow) and
      (LFocusBefore.Col = LFocusColumn),
      'duplicate allowed completion after consumption cannot move actual native grid focus or coordinates');
    {$endif}
    LKeyCommand.Retire;
    LKeyCommand := nil;
    LKeyInput := nil;
    LClicks := FRangeClicks;
    FRangeClickPhase := '';
    LKeys := FRangeKeys;
    RangeKey('Home');
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    LIntent := LRanges.CellRange;
    Check((FRangeKeys = LKeys + 1) and (LIntent <> nil) and LIntent.Current and
      LIntent.Capture.Anchor.Same(NyxCollectionCellIdentity(ItemA, LAnchorField)) and
      LIntent.Capture.Focus.Same(NyxCollectionCellIdentity(ItemA, NameField)),
      'real Shift+Home prepares before canonical callbacks and extends to the first visible typed field');
    LFirst := LIntent.Capture.Anchor;
    RangeKey('End');
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(not LIntent.Current and LRanges.CellRange.Capture.Anchor.Same(LFirst) and
      LRanges.CellRange.Capture.Focus.Same(NyxCollectionCellIdentity(ItemA, LLastField)),
      'real Shift+End continues from moved physical focus and retains its directional anchor');
    RangeKey('ArrowDown');
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(LRanges.CellRange.Capture.Focus.Same(NyxCollectionCellIdentity(ItemB, LLastField)),
      'real Shift+Down follows actual current-page order across the logical end pane');
    RangeKey('ArrowLeft');
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(not LRanges.CellRange.Capture.Focus.Field.Same(LLastField) and
      (LRanges.CellRange.Capture.Focus.Item.ID = ItemB.ID),
      'real Shift+Left crosses the end pane into the actual middle field order');
    RangeKey('ArrowRight');
    RangeKey('ArrowUp');
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(LRanges.CellRange.Capture.Focus.Same(NyxCollectionCellIdentity(ItemA, LLastField)),
      'real Shift+Right and Shift+Up use the newly focused exact field and page row');
    RangeKey('Home', True, True);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(LRanges.CellRange.Capture.Focus.Same(NyxCollectionCellIdentity(ItemA, NameField)),
      'real Control+Shift+Home moves to the first actual page cell');
    RangeKey('End', True, True);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(LRanges.CellRange.Capture.Focus.Same(NyxCollectionCellIdentity(ItemB, LLastField)) and
      FView.Selection.Snapshot.SameState(LRows) and (FHost.Writes = LBefore + 1),
      'real Control+Shift+End reaches the last page cell without changing row state or clipboard');
    {$ifdef PAS2JS}
    Check((TJSHTMLElement(document.activeElement).getAttribute('data-nyx-column') =
      IntToStr(FView.Spec.ColumnIndex(LLastField))) and
      (TJSHTMLElement(document.activeElement).parentElement.getAttribute('data-nyx-item') = ItemB.ID) and
      (TJSHTMLElement(document.activeElement).getAttribute('tabindex') = '0') and
      (FTable.querySelectorAll('[role="gridcell"][tabindex="0"]').length = 1),
      'actual moved range focus remains the sole Tab entry after measured-window reconciliation');
    {$endif}
    LIntent := LRanges.CellRange;
    RangeKey('Escape', False);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(not LIntent.Current and (LRanges.CellRange = nil),
      'real unmodified Escape retires only the original independent cell range');
    FMode := kmConsume;
    RangeKey('Home');
    Check(LRanges.CellRange = nil,
      'consumed canonical Shift+Home does not create a range or run an inherited row default');
    FMode := kmNested;
    RangeKey('Home');
    LIntent := LRanges.CellRange;
    Check((LIntent <> nil) and LIntent.Current and
      LIntent.Capture.Anchor.Same(NyxCollectionCellIdentity(ItemB, NameField)) and
      LIntent.Capture.Focus.Same(NyxCollectionCellIdentity(ItemB, LScore)),
      'nested newer canonical range survives the older prepared keyboard default');
    { Public SelectCells deliberately does not move physical focus. In the
      ordinary table that nested Score endpoint differs from the Clock cell
      currently holding End focus. Use actual keys to establish matched input
      before testing a benign refresh; never let a stale coordinate retarget it. }
    FMode := kmObserve;
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(ItemB, NameField),
      NyxCollectionCellIdentity(ItemB, NameField)),
      'public selection can designate a newer endpoint without stealing physical focus');
    LIntent := LRanges.CellRange;
    {$ifdef PAS2JS}
    LFocusBefore := TJSHTMLElement(document.activeElement);
    {$else}
    LFocusBefore := TNyxCollectionStringGrid(Screen.ActiveControl);
    LFocusRow := LFocusBefore.Row;
    LFocusColumn := LFocusBefore.Col;
    {$endif}
    RangeKey('Home');
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(LIntent.Current and (LRanges.CellRange = LIntent),
      'mismatched physical endpoint refuses the old movement and preserves the newer selected range');
    {$ifdef PAS2JS}
    Check(document.activeElement = LFocusBefore,
      'refused browser movement cannot steal focus or replay its key default');
    {$else}
    Check((Screen.ActiveControl = LFocusBefore) and (LFocusBefore.Row = LFocusRow) and
      (LFocusBefore.Col = LFocusColumn),
      'refused native movement cannot move physical cell coordinates or replay its key default');
    {$endif}
    RangeKey('Escape', False);
    RangeKey('Home');
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    LIntent := LRanges.CellRange;
    Check((LIntent <> nil) and LIntent.Current and
      LIntent.Capture.Focus.Same(NyxCollectionCellIdentity(ItemB, NameField)),
      'fresh actual keys establish the current cell endpoint without moving ordinary row state');
    LFirst := LIntent.Capture.Anchor;
    FMode := kmMutate;
    RangeKey('Home');
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(not LIntent.Current and (LRanges.CellRange <> nil) and LRanges.CellRange.Current and
      LRanges.CellRange.Capture.Anchor.Same(LFirst) and
      LRanges.CellRange.Capture.Focus.Same(NyxCollectionCellIdentity(ItemB, NameField)) and
      (LRanges.CellRange.Source = LSource) and (LRanges.CellRange.Page = LPage),
      'benign callback refresh completes the exact original prepared key with unchanged page/source and directional anchor');
    FMode := kmDisableABA;
    RangeKey('Home');
    Check(not LIntent.Current and (LRanges.CellRange = nil),
      'disable/re-enable during the canonical range callback cannot lend the old key a fresh epoch');
    FMode := kmObserve;
    Check((FRangeKeys = LKeys + 16) and FView.Selection.Snapshot.SameState(LRows) and
      (FHost.Writes = LBefore + 1),
      'all real range keys preserve ordinary row selection and make no host write');
    Check(FRangeClicks = LClicks,
      'internal range cursor moves never emit a ghost canonical user Click on arranged or ordinary grids;arranged=' +
      TNyxText(BoolToStr(FView.Spec.HasLayoutControls, True)) + ';before=' +
      TNyxText(IntToStr(LClicks)) + ';after=' + TNyxText(IntToStr(FRangeClicks)) +
      ';first=' + FRangeClickPhase);
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

procedure TCopyReview.ReplaceContextRange;
var
  LRanges: INyxCollectionCellRangeMount;
begin
  Check(Supports(FMount, INyxCollectionCellRangeMount, LRanges) and
    LRanges.SelectCells(NyxCollectionCellIdentity(ItemA, NameField),
      NyxCollectionCellIdentity(ItemB, NyxColumnIdentity(NyxIntegerField('Score')))),
    'foreign UI admission installs a genuinely newer same-byte range through the actual mounted owner');
  Inc(FRangeReplacements);
end;

{ Reuses both existing table owners and real target menus. The menu may copy
  only its original typed rectangle; RAW cell inspection remains independent,
  including when range readiness is withdrawn after the menu took focus. }
{ The existing same-owner fixture produces down/up/click through real target
  routes. Browser events and native virtual methods are synthetic producers;
  neither establishes trusted hardware, touch capture or a system clipboard. }
procedure TCopyReview.RunPointerRange; {$ifdef PAS2JS}async;{$endif}
var
  LRanges: INyxCollectionCellRangeMount;
  LIntent: INyxCollectionCellRangeIntent;
  LSource: INyxCollectionSnapshot;
  LRows: TNyxCollectionSelectionSnapshot;
  LItems: TNyxItemRefs;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LPreferences: TNyxColumnPreferences;
  LScore: TNyxColumnIdentity;
  LOrigin: TNyxCollectionCellIdentity;
  LTarget: TNyxCollectionCellIdentity;
  LQuery: TNyxText;
  LBefore: Integer;
  LDowns: Integer;
  LUps: Integer;
  LClicks: Integer;
  LCase: Integer;
  LIndex: Integer;
  LFocusFacts: TNyxText;
  LGestureError: TNyxText;
  {$ifdef PAS2JS}
  LOriginCell: TJSHTMLElement;
  LTargetCell: TJSHTMLElement;
  {$else}
  LOriginGrid: TNyxCollectionStringGrid;
  LTargetGrid: TNyxCollectionStringGrid;
  LOriginColumn: Integer;
  LTargetColumn: Integer;
  LCapturedControl: TControl;

  function FindSurface(AColumn: Integer; out APhysical: Integer): TNyxCollectionStringGrid;
  var
    LSurface: TNyxCollectionStringGrid;
    LTitle: TNyxText;
    LControl: Integer;
    LPhysical: Integer;
  begin
    Result := nil;
    APhysical := -1;
    LTitle := FView.Spec.ColumnAt(AColumn).Title;
    for LControl := -1 to FGrid.ControlCount - 1 do
    begin
      LSurface := FGrid;

      if LControl >= 0 then
      begin

        if not (FGrid.Controls[LControl] is TNyxCollectionStringGrid) or
          not FGrid.Controls[LControl].Visible then
        begin
          Continue;
        end;
        LSurface := TNyxCollectionStringGrid(FGrid.Controls[LControl]);
      end;
      for LPhysical := 0 to LSurface.ColCount - 1 do
      begin

        if (LSurface.ColWidths[LPhysical] > 0) and
          (Pos(LTitle, LSurface.DisplayCellText(LPhysical, 0)) = 1) then
        begin
          APhysical := LPhysical;
          Exit(LSurface);
        end;
      end;
    end;
  end;
  {$endif}

  procedure FocusOrigin;
  begin
    {$ifdef PAS2JS}
    LOriginCell := TJSHTMLElement(FTable.querySelector(
      '[data-nyx-item="a"] [role="gridcell"][data-nyx-column="0"]'));
    LTargetCell := TJSHTMLElement(FTable.querySelector(
      '[data-nyx-item="b"] [role="gridcell"][data-nyx-column="1"]'));
    Check((LOriginCell <> nil) and (LTargetCell <> nil),
      'pointer test resolves exact owned resident endpoint cells');
    LOriginCell.focus;
    {$else}
    LOriginGrid := FindSurface(0, LOriginColumn);
    LTargetGrid := FindSurface(1, LTargetColumn);
    Check((LOriginGrid <> nil) and (LTargetGrid <> nil),
      'pointer test resolves real native surfaces across logical pin edges');
    LOriginGrid.Col := LOriginColumn;
    LOriginGrid.Row := 1;
    LOriginGrid.SetFocus;
    Drain;
    {$endif}
  end;

  procedure Endpoint(AShift: Boolean);
  var
    {$ifdef PAS2JS}
    LOptions: TJSObject;
    LDown: TJSPointerEvent;
    LNode: TJSHTMLElement;
    {$else}
    LBounds: TRect;
    LShift: TShiftState;
    {$endif}
  begin
    FRangePointerSending := AShift;
    try
      {$ifdef PAS2JS}
      LOptions := TJSObject.new;
      LOptions['bubbles'] := True;
      LOptions['cancelable'] := True;
      LOptions['button'] := 0;
      LOptions['buttons'] := 1;
      LOptions['pointerId'] := 73;
      LOptions['pointerType'] := 'mouse';
      LOptions['isPrimary'] := True;
      LOptions['shiftKey'] := AShift;
      LOptions['detail'] := 1;
      LNode := LTargetCell;

      if LTargetCell.firstElementChild <> nil then
      begin
        LNode := TJSHTMLElement(LTargetCell.firstElementChild);
      end;
      LDown := TRangePointerEvent.new('pointerdown', LOptions);
      LNode.dispatchEvent(LDown);

      if AShift then
      begin
        Check(LDown.defaultPrevented,
          'managed Shift-primary down claims the actual DOM default');
      end;
      LOptions['buttons'] := 0;
      LNode.dispatchEvent(TRangePointerEvent.new('pointerup', LOptions));
      LNode.dispatchEvent(TCellContextEvent.new('click', LOptions));
      {$else}
      LBounds := LTargetGrid.CellRect(LTargetColumn, 2);
      LShift := [];

      if AShift then
      begin
        Include(LShift, ssShift);
      end;
      TRangePointerAccess(LTargetGrid).InvokePointer(LShift,
        (LBounds.Left + LBounds.Right) div 2, (LBounds.Top + LBounds.Bottom) div 2);
      Drain;
      {$endif}
    finally
      FRangePointerSending := False;
    end;
  end;

  function FocusMatches(AEndpoint: Boolean): Boolean;
  begin
    {$ifdef PAS2JS}
    Result := document.activeElement = LOriginCell;

    if AEndpoint then
    begin
      Result := document.activeElement = LTargetCell;
    end;
    {$else}
    Result := (Screen.ActiveControl = LOriginGrid) and
      (LOriginGrid.Row = 1) and (LOriginGrid.Col = LOriginColumn);

    if AEndpoint then
    begin
      Result := (Screen.ActiveControl = LTargetGrid) and
        (LTargetGrid.Row = 2) and (LTargetGrid.Col = LTargetColumn);
    end;
    {$endif}
  end;

  {$ifdef PAS2JS}
  { Synthetic pointers traverse the actual renderer, captured host route and
    resident-cell hit test. Only capture acquisition is injected; the suite
    deliberately distinguishes these checks from trusted physical interaction. }
  procedure StrokeJourney;
  var
    LSetCapture: JSValue;
    LHasCapture: JSValue;
    LReleaseCapture: JSValue;
    LOptions: TJSObject;
    LAccepted: INyxCollectionCellRangeIntent;
    LMoveToken: INyxEventSubscription;
    LCaptures: Integer;
    LRound: Integer;

    procedure Send(const AKind: String; ACell: TJSHTMLElement; AID: Integer = 73);
    var
      LBounds: TJSDOMRect;
      LTarget: TJSHTMLElement;
    begin
      LOptions := TJSObject.new;
      LOptions['bubbles'] := True;
      LOptions['cancelable'] := True;
      LOptions['button'] := 0;
      LOptions['buttons'] := 1;

      if AKind = 'pointerup' then
      begin
        LOptions['buttons'] := 0;
      end;
      LOptions['pointerId'] := AID;
      LOptions['pointerType'] := 'mouse';
      LOptions['isPrimary'] := True;
      LOptions['shiftKey'] := True;
      LOptions['clientX'] := -100;
      LOptions['clientY'] := -100;

      if ACell <> nil then
      begin
        LBounds := ACell.getBoundingClientRect;
        LOptions['clientX'] := LBounds.left + LBounds.width / 2;
        LOptions['clientY'] := LBounds.top + LBounds.height / 2;
      end;
      LTarget := FTable;

      if AKind = 'pointerdown' then
      begin
        LTarget := ACell;
      end;
      LTarget.dispatchEvent(TRangePointerEvent.new(AKind, LOptions));
    end;

  begin
    LSetCapture := TJSObject(FTable)['setPointerCapture'];
    LHasCapture := TJSObject(FTable)['hasPointerCapture'];
    LReleaseCapture := TJSObject(FTable)['releasePointerCapture'];
    TJSObject(FTable)['setPointerCapture'] := @CaptureRangePointer;
    TJSObject(FTable)['hasPointerCapture'] := @HasRangePointer;
    TJSObject(FTable)['releasePointerCapture'] := @ReleaseRangePointer;
    LMoveToken := FRenderer.Events.On(NyxControlEvents(CTable, niRuntime),
      ntPointerMove).Subscribe(FCallback);
    try
      LRanges.ConfigureCellRange(True);
      FRangePointerSending := True;
      FRangeStrokeSending := True;
      for LRound := 0 to 7 do
      begin
        FMode := kmObserve;
        GRangeCaptureRefused := LRound = 3;
        GRangeCapturedID := -1;
        LRanges.ClearCellRange;
        FocusOrigin;
        LOriginCell.scrollIntoView;
        LCaptures := GRangeCaptures;

        if LRound = 4 then
        begin
          FMode := kmCapture;
        end;
        Send('pointerdown', LTargetCell);
        LAccepted := LRanges.CellRange;
        Check((LAccepted <> nil) and LAccepted.Current and
          LAccepted.Capture.Focus.Same(LTarget),
          'Shift-down retains its immediate exact accepted endpoint');

        if LRound = 3 then
        begin
          Check((GRangeCapturedID = -1) and (GRangeCaptures = LCaptures),
            'Refused capture preserves Shift-click without a false live drag');
          Continue;
        end;
        Check((GRangeCapturedID = 73) and (GRangeCaptures = LCaptures + 1),
          'Current original down acquires one injected platform capture');
        Send('pointermove', LOriginCell, 74);
        Check((LRanges.CellRange = LAccepted) and (GRangeCapturedID = 73),
          'Wrong pointer cannot move or cancel another pending stroke');

        if LRound = 5 then
        begin
          Send('pointermove', nil);
          Check((GRangeCapturedID = -1) and (LRanges.CellRange = LAccepted) and
            LAccepted.Current,
            'Outside-grid hit cancels without constructing an undefined identity');
          Continue;
        end;

        if LRound = 6 then
        begin
          Send('lostpointercapture', LOriginCell, 74);
          Send('lostpointercapture', LOriginCell);
          Check(GRangeCapturedID = 73,
            'Wrong-pointer and delayed loss cannot revoke a still-captured current pair');
          ReleaseRangePointer(73);
          Send('lostpointercapture', LOriginCell);
          Check((LRanges.CellRange = LAccepted) and LAccepted.Current and
            (LTargetCell.getAttribute('data-nyx-range-selected') = 'true'),
            'Actual matching loss withdraws the preview and restores accepted paint');
          Continue;
        end;

        if LRound = 7 then
        begin
          FMount.Refresh;
          Check((GRangeCapturedID = -1) and (LRanges.CellRange = LAccepted) and
            LAccepted.Current,
            'Refresh releases original gesture capture without changing accepted cells');
          Continue;
        end;

        if LRound = 1 then
        begin
          FMode := kmConsume;
        end;
        Send('pointermove', LOriginCell);
        Check(LRanges.CellRange = LAccepted,
          'Captured movement paints only preview, preserving accepted cells');

        if LRound = 4 then
        begin
          Check((GRangeCapturedID = 73) and
            (LTargetCell.getAttribute('data-nyx-range-selected') = 'true'),
            'Explicit application capture outranks the optional range drag');
          ReleaseRangePointer(73);
          Continue;
        end;

        if LRound = 1 then
        begin
          Check((GRangeCapturedID = -1) and LAccepted.Current and
            (LTargetCell.getAttribute('data-nyx-range-selected') = 'true'),
            'Consumed original move releases capture and restores accepted paint');
          Continue;
        end;
        Check(LTargetCell.getAttribute('data-nyx-range-selected') <> 'true',
          'Physical hit test paints the smaller pending rectangle');

        if LRound = 2 then
        begin
          Send('pointercancel', LOriginCell);
          Check((GRangeCapturedID = -1) and (LRanges.CellRange = LAccepted) and
            LAccepted.Current and
            (LTargetCell.getAttribute('data-nyx-range-selected') = 'true'),
            'Matching cancellation releases capture and restores accepted rectangle');
        end
        else
        begin
          Send('pointerup', LOriginCell);
          Check((GRangeCapturedID = -1) and (LRanges.CellRange <> LAccepted) and
            LRanges.CellRange.Capture.Anchor.Same(LOrigin) and
            LRanges.CellRange.Capture.Focus.Same(LOrigin) and
            (document.activeElement = LOriginCell),
            'Matching release commits the preview and exact physical endpoint focus');
        end;
      end;
      Check((FView.Store.Snapshot = LSource) and
        FView.Selection.Snapshot.SameState(LRows),
        'Sustained range input changes no RAW data or ordinary row selection');
    finally
      FMode := kmObserve;
      FRangePointerSending := False;
      FRangeStrokeSending := False;
      GRangeCaptureRefused := False;
      LRanges.ConfigureCellRange(False);
      LMoveToken.Cancel;
      TJSObject(FTable)['setPointerCapture'] := LSetCapture;
      TJSObject(FTable)['hasPointerCapture'] := LHasCapture;
      TJSObject(FTable)['releasePointerCapture'] := LReleaseCapture;
    end;
  end;
  {$endif}

  function CallbackFocusMatches: Boolean;
  begin
    {$ifdef PAS2JS}
    Result := FPointerCallbackRecorded and
      (document.activeElement = FPointerCallbackFocus);
    {$else}
    Result := FPointerCallbackRecorded and (Screen.ActiveControl = FPointerCallbackFocus);

    if Result and (FPointerCallbackFocus is TNyxCollectionStringGrid) then
    begin
      Result := (TNyxCollectionStringGrid(FPointerCallbackFocus).Row = FPointerCallbackRow) and
        (TNyxCollectionStringGrid(FPointerCallbackFocus).Col = FPointerCallbackColumn);
    end;
    {$endif}
  end;
begin
  Check(Supports(FMount, INyxCollectionCellRangeMount, LRanges),
    'same actual table retains its independent range owner for pointer input');
  LSource := FView.Store.Snapshot;
  LQuery := FView.QueryPolicy.ToData.ToJSON;
  LRows := FView.Selection.Snapshot;
  SetLength(LItems, LRows.Count);
  for LIndex := 0 to LRows.Count - 1 do
  begin
    LItems[LIndex] := LRows.ItemAt(LIndex);
  end;
  LScore := NyxColumnIdentity(NyxIntegerField('Score'));
  LOrigin := NyxCollectionCellIdentity(ItemA, NameField);
  LTarget := NyxCollectionCellIdentity(ItemB, LScore);
  LColumns := NyxCollectionColumns(FView);
  LCapture := LColumns.CaptureColumns;
  LPreferences := LCapture.Preferences;
  LBefore := FHost.Writes;
  try

    if FView.Spec.HasLayoutControls then
    begin
      LColumns.ConfigureColumns(LPreferences.WithColumn(NyxColumnPreference(NameField).Pinned(ncpStart))
        .WithColumn(NyxColumnPreference(LScore).Pinned(ncpEnd)));
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    end;
    LRanges.ConfigureCellRange(True);
    for LCase := 0 to 4 do
    begin
      FMode := kmObserve;
      LRanges.ClearCellRange;
      FocusOrigin;
      case LCase of
        0:
          begin
            FMode := kmConsume;
          end;
        1:
          begin
            FMode := kmObserve;
          end;
        2:
          begin
            FMode := kmNested;
          end;
        3:
          begin
            FMode := kmDisableABA;
          end;
        4:
          begin
            FMode := kmMutate;
          end;
      end;
      LDowns := FRangePointers;
      LUps := FRangePointerUps;
      LClicks := FRangeClicks;
      FPointerCallbackRecorded := False;
      LGestureError := FRenderer.LastGestureError;
      {$ifndef PAS2JS}LCapturedControl := GetCaptureControl;{$endif}
      Check(FocusMatches(False), 'pointer launch has actual exact origin focus before input');
      Endpoint(True);
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check((FRangePointers = LDowns + 1) and (FRangePointerUps = LUps + 1) and
        (FRangeClicks = LClicks + 1),
        'managed down/up/click delivers original canonical callbacks exactly once');
      LIntent := LRanges.CellRange;
      LFocusFacts := 'case=' + TNyxText(IntToStr(LCase)) + ';range=' +
        TNyxText(BoolToStr(LIntent <> nil, True)) + ';focus=' +
        TNyxText(BoolToStr(FocusMatches(False), True));
      {$ifndef PAS2JS}
      LFocusFacts := LFocusFacts + ';origin=' +
        TNyxText(IntToStr(LOriginGrid.Col)) + ',' + TNyxText(IntToStr(LOriginGrid.Row)) +
        ';expected=' + TNyxText(IntToStr(LOriginColumn)) + ',1;activeIsOrigin=' +
        TNyxText(BoolToStr(Screen.ActiveControl = LOriginGrid, True));
      {$endif}
      case LCase of
        0:
          begin
            Check((LIntent = nil) and FocusMatches(False),
              'consumed pointer cannot focus, select rows or publish a rectangle;' + LFocusFacts);
          end;
        3:
          begin
            Check((LIntent = nil) and CallbackFocusMatches,
              'disabled-ABA pointer cannot move the callback accepted focus or publish a rectangle;' + LFocusFacts);
            Check(FRenderer.LastGestureError = LGestureError,
              'retired original input never submits its capture request to the platform');
            {$ifndef PAS2JS}
            Check(GetCaptureControl = LCapturedControl,
              'retired same-view input leaves the exact native capture owner unchanged');
            {$endif}
          end;
        1, 4:
          begin
            Check((LIntent <> nil) and LIntent.Current and
              LIntent.Capture.Anchor.Same(LOrigin) and LIntent.Capture.Focus.Same(LTarget) and
              LIntent.Capture.Contains(ItemA, NameField) and
              LIntent.Capture.Contains(ItemB, LScore) and FocusMatches(True),
              'allowed pointer and benign refresh publish exact original page endpoints and real focus');
          end;
        2:
          begin
            Check((LIntent <> nil) and LIntent.Current and FocusMatches(False) and
              LIntent.Capture.Anchor.Same(NyxCollectionCellIdentity(ItemB, NameField)) and
              LIntent.Capture.Focus.Same(LTarget),
              'newer nested pointer selection survives without an older focus/default tail');
          end;
      end;
      Check(FView.Selection.Snapshot.SameState(LRows) and
        (FView.Store.Snapshot = LSource) and (FHost.Writes = LBefore),
        'managed pointer leaves ordinary row selection, accepted RAW and host authority untouched');
    end;
    {$ifdef PAS2JS}StrokeJourney;{$endif}
    FMode := kmObserve;
    LRanges.ConfigureCellRange(False);
    FocusOrigin;
    Endpoint(False);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check((LRanges.CellRange = nil) and FView.Selection.Contains(ItemB),
      'default-off plain primary cell input retains ordinary row-selection behavior');
  finally
    FMode := kmObserve;
    FRangePointerSending := False;
    LRanges.ConfigureCellRange(False);

    if FView.Spec.HasLayoutControls then
    begin
      LColumns.ConfigureColumns(LPreferences);
    end;
    FView.SetSelection(LItems, LRows.Focus, LRows.Anchor);
    FContextColumn := 0;
    FocusCell;
  end;
  Check((FView.Store.Snapshot = LSource) and
    (FView.QueryPolicy.ToData.ToJSON = LQuery) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'pointer entry/default isolation preserves saved design and exact accepted/query values');
end;

procedure TCopyReview.RunRangeMenu; {$ifdef PAS2JS}async;{$endif}
var
  LRanges: INyxCollectionCellRangeMount;
  LIntent: INyxCollectionCellRangeIntent;
  LSource: INyxCollectionSnapshot;
  LRows: TNyxCollectionSelectionSnapshot;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LDraftActions: INyxCollectionCellDraftActions;
  LExpected: TNyxText;
  LQuery: TNyxText;
  LBefore: Integer;
  LReplies: Integer;
  LReentries: Integer;
  LReplacements: Integer;
  LSerial: Integer;
  {$ifdef PAS2JS}
  LEditor: TJSHTMLInputElement;
  LEventOptions: TJSObject;
  {$else}
  LEditor: TCustomEdit;
  {$endif}

  procedure SelectRange;
  begin
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(ItemA, NameField),
      NyxCollectionCellIdentity(ItemB, NyxColumnIdentity(NyxIntegerField('Score')))),
      'real managed-menu setup selects exact current-page typed endpoints');
    LIntent := LRanges.CellRange;
    LExpected := LIntent.Capture.Payload.Text;
  end;

begin
  FMode := kmObserve;
  FContextColumn := 0;
  FCellActions.Close;
  FocusCell;
  LSource := FView.Store.Snapshot;
  LRows := FView.Selection.Snapshot;
  LQuery := FView.QueryPolicy.ToData.ToJSON;
  LColumns := NyxCollectionColumns(FView);
  LCapture := LColumns.CaptureColumns;
  LSerial := LCapture.Serial;
  Check(Supports(FMount, INyxCollectionCellRangeMount, LRanges),
    'managed range menu uses the same real mounted selection owner');
  LRanges.ConfigureCellRange(True);
  try
    SelectRange;
    LBefore := FHost.Writes;
    LReplies := FReplies;
    Context;
    {$ifdef PAS2JS}await(Drain);{$endif}
    Check(HasMenuAction(NyxCellActionsCopyRangeID) and
      (FCellActions.Snapshot.Text = CText) and (FCellActions.Snapshot.RowCount = 1) and
      (FHost.Writes = LBefore),
      'real menu exposes Copy selected cells separately from its original accepted-cell snapshot');
    MenuAction(NyxCellActionsCopyRangeID);
    {$ifdef PAS2JS}await(Drain);{$endif}
    Check((FHost.Writes = LBefore + 1) and (FReplies = LReplies + 1) and
      (FHost.Text = LExpected) and (FLastPayload.Text = LExpected) and
      (FLastPayload.RowCount = 2) and (FLastPayload.FieldCount = 2) and
      (FLastPayload.CellValue(0, 0).TextValue = CText) and
      (FCellActions.LastResult.Outcome = ncwoWritten) and
      (FCellActions.LastResult.ByteCount = FLastPayload.ByteCount),
      'actual managed button copies original Unicode NUL/newline TSV once and validates the rectangle byte count');
    FCellActions.Close;
    FocusCell;
    Context;
    {$ifdef PAS2JS}await(Drain);{$endif}
    Check(HasMenuAction(NyxCellActionsCopyRangeID),
      'a fresh menu admits the current original range before focus changes');
    LRanges.ClearCellRange;
    LBefore := FHost.Writes;
    MenuAction(NyxCellActionsCopyRangeID);
    {$ifdef PAS2JS}await(Drain);{$endif}
    Check((FHost.Writes = LBefore) and
      (FCellActions.LastResult.Outcome = ncwoCancelled) and
      (FCellActions.Snapshot.Text = CText) and (FCellActions.Inspector <> nil) and
      FCellActions.Inspector.IsOpen and
      (FCellActions.Inspector.State.GetValue(NyxTextState(NyxCellInspectorStatusState)) <> ''),
      'clearing the original range refuses its real action visibly without substituting RAW or entering the host');
    InspectorAction(NyxCellInspectorCloseID);
    {$ifdef PAS2JS}await(Drain);{$endif}
    FCellActions.Close;
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(ItemB,
      NyxColumnIdentity(NyxIntegerField('Score'))), NyxCollectionCellIdentity(ItemB,
      NyxColumnIdentity(NyxIntegerField('Score')))),
      'outside-context range setup uses a valid separate current-page cell');
    FocusCell;
    Context;
    {$ifdef PAS2JS}await(Drain);{$endif}
    Check(not HasMenuAction(NyxCellActionsCopyRangeID) and HasMenuAction(NyxCellActionsCopyID),
      'actual menu omits selected-cell copying outside the launched typed membership and retains RAW copying');
    MenuAction(NyxCellActionsCopyID);
    {$ifdef PAS2JS}await(Drain);{$endif}
    Check((FHost.Writes = LBefore + 1) and (FHost.Text = CText),
      'ordinary accepted-value action remains independent of an unrelated cell rectangle');
    FCellActions.Close;
    { The second, ordinary renderer-managed owner installs this admission actor
      together with explicit draft actions. The first compatible owner uses its
      original scheduler directly and has no actor to arm or retire. }

    if FContextScheduler <> nil then
    begin
      SelectRange;
      LReentries := FContextScheduler.Reentries;
      LReplacements := FRangeReplacements;
      FContextScheduler.Owner := Self;
      FContextScheduler.ReplaceRangeOnly := True;
      FocusCell;
      Context;
      {$ifdef PAS2JS}await(Drain);{$endif}
      Check((FContextScheduler.Reentries = LReentries + 1) and
        (FRangeReplacements = LReplacements + 1) and not LIntent.Current and
        (LRanges.CellRange <> nil) and LRanges.CellRange.Current and
        (LRanges.CellRange.Capture.Payload.Text = LExpected) and
        not HasMenuAction(NyxCellActionsCopyRangeID) and HasMenuAction(NyxCellActionsCopyID) and
        (FCellActions.Snapshot.Text = CText),
        'range replacement inside real input admission cannot lend identical newer membership to the older context');
      FCellActions.Close;
    end;

    if Supports(FCellActions, INyxCollectionCellDraftActions, LDraftActions) and
      LDraftActions.FocusedDraftEnabled then
    begin
      SelectRange;
      FocusCell;
      {$ifdef PAS2JS}
      Key(Cell, 'F2', False);
      await(Drain);
      LEditor := TJSHTMLInputElement(Cell.querySelector('input'));
      Check((LEditor <> nil) and (document.activeElement = LEditor),
        'range-menu draft isolation begins with the actual focused browser editor');
      LEditor.value := CDraft;
      LEventOptions := TJSObject.new;
      LEventOptions['bubbles'] := True;
      LEventOptions['cancelable'] := True;
      LEventOptions['button'] := 2;
      LEditor.dispatchEvent(TCellContextEvent.new('contextmenu', LEventOptions));
      await(Drain);
      {$else}
      SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
      Drain;
      Check(FGrid.EditorMode and (FGrid.Editor is TCustomEdit),
        'range-menu draft isolation begins with the actual focused native editor');
      LEditor := TCustomEdit(FGrid.Editor);
      RestoreNyxLCLInputText(LEditor, CDraft);
      SendMessage(LEditor.Handle, WM_CONTEXTMENU, LEditor.Handle, MakeLParam(-1, -1));
      Drain;
      {$endif}
      Check(HasMenuAction(NyxCellActionsCopyRangeID) and
        (LDraftActions.CapturedDraft <> nil) and
        (LDraftActions.CapturedDraft.Value.Text = CDraft) and
        (FView.Store.Snapshot = LSource),
        'explicit editor context captures selected accepted cells before focus without accepting its unfinished draft');
      LBefore := FHost.Writes;
      MenuAction(NyxCellActionsCopyRangeID);
      {$ifdef PAS2JS}await(Drain);{$endif}
      Check((FHost.Writes = LBefore + 1) and (FHost.Text = LExpected) and
        (FLastPayload.CellValue(0, 0).TextValue = CText) and
        (FView.Store.Snapshot = LSource) and
        {$ifdef PAS2JS}(LEditor.value = CDraft){$else}(NyxLCLInputText(LEditor) = CDraft){$endif},
        'actual range action copies accepted RAW cells and preserves the same unfinished physical editor text');
      FCellActions.Close;
      {$ifdef PAS2JS}
      LEditor.focus;
      Key(LEditor, 'Escape', False);
      await(Drain);
      {$else}
      FGrid.SetFocus;

      if not FGrid.EditorMode then
      begin
        SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
      end;
      Check(FGrid.EditorMode, 'fresh native editing permits explicit Escape cleanup of the retained menu draft');
      SendMessage(FGrid.Handle, WM_KEYDOWN, VK_ESCAPE, 1);
      Drain;
      {$endif}
    end;
  finally

    if FContextScheduler <> nil then
    begin
      FContextScheduler.Owner := nil;
      FContextScheduler.ReplaceRangeOnly := False;
    end;
    FCellActions.Close;
    LRanges.ConfigureCellRange(False);
    FMode := kmObserve;
    FContextColumn := 0;
    FocusCell;
  end;
  LCapture := LColumns.CaptureColumns;
  Check((FView.Store.Snapshot = LSource) and FView.Selection.Snapshot.SameState(LRows) and
    (FView.QueryPolicy.ToData.ToJSON = LQuery) and (LCapture.Serial = LSerial) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'real range-menu actions preserve exact accepted data, ordinary row state, query, layout and authored source');
end;

{$ifndef PAS2JS}
procedure TCopyReview.NativeRangeSelected(Sender: TObject; AColumn, ARow: Integer);
begin

  if FRetireRangeSelection then
  begin
    FRetireRangeSelection := False;
    Inc(FRangeSelectionCallbacks);
    FRenderer.Unmount;
  end;
end;

{ Explicit --probe-native-pointer runs only after the functional journeys.
  A locked/noninteractive desktop is an unavailable physical observation, not
  a passing pointer proof and not a reason to skip keyboard/lifetime checks. }
function TCopyReview.RunNativePointerProbe: Boolean;
var
  LBounds: TRect;
  LPoint: TPoint;
  LSaved: TPoint;
  LHandle: HWND;
  LClicks: Integer;
begin
  Result := False;

  if not GetCursorPos(LSaved) then
  begin
    WriteLn('UNAVAILABLE native WM pointer probe: GetCursorPos refused the current desktop');
    Exit;
  end;
  FCellActions.Close;
  FMode := kmObserve;
  FContextColumn := 2;
  FocusCell;
  LClicks := FRangeClicks;
  LBounds := FGrid.CellRect(2, 1);
  LPoint := Point((LBounds.Left + LBounds.Right) div 2,
    (LBounds.Top + LBounds.Bottom) div 2);
  LPoint := FGrid.ClientToScreen(LPoint);
  try
    Check(SetCursorPos(LPoint.X, LPoint.Y),
      'position native pointer at the same real cell as the paired mouse messages');
    LPoint := FGrid.ScreenToClient(LPoint);
    LHandle := FGrid.Handle;
    SendMessage(LHandle, WM_MOUSEMOVE, 0, MakeLParam(LPoint.X, LPoint.Y));
    SendMessage(LHandle, WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(LPoint.X, LPoint.Y));
    SendMessage(LHandle, WM_LBUTTONUP, 0, MakeLParam(LPoint.X, LPoint.Y));
    Drain;
  finally
    Check(SetCursorPos(LSaved.X, LSaved.Y),
      'restore caller cursor after the admitted native cell mouse pair');
  end;
  Check(FRangeClicks = LClicks + 1,
    'admitted native WM data-cell pair outside the internal transition delivers canonical Click once');
  Result := True;
  WriteLn('PASS native WM pointer probe; paired messages with real cursor position, no trusted hardware claim');
end;

{ The existing ordinary-table owner is retired only after its full unchanged
  journey. An actual LCL selection callback interrupts the first Col setter;
  the entered physical frame must protect that setter's remaining native tail. }
procedure TCopyReview.RunRangeFocusRetirement;
var
  LRanges: INyxCollectionCellRangeMount;
  LKeyInput: INyxCollectionCellRangeKeyInput;
  LCommand: INyxCollectionCellRangeKeyCommand;
  LOldSelection: TOnSelectEvent;
  LSource: INyxCollectionSnapshot;
  LOrigin: TNyxCollectionCellIdentity;
  LBefore: Integer;
  LClicks: Integer;
begin
  Check(not FView.Spec.HasLayoutControls,
    'native setter-retirement probe uses the same ordinary renderer-managed table');
  FContextColumn := 2;
  FocusCell;
  LSource := FView.Store.Snapshot;
  LBefore := FHost.Writes;
  LClicks := FRangeClicks;
  Check(Supports(FMount, INyxCollectionCellRangeMount, LRanges) and
    Supports(FMount, INyxCollectionCellRangeKeyInput, LKeyInput),
    'ordinary native owner retains its actual prepared range input contracts');
  LRanges.ConfigureCellRange(True);
  LOrigin := NyxCollectionCellIdentity(ItemA, FView.Spec.ColumnAt(2).Identity);
  LCommand := LKeyInput.BeginCellRangeKey(ncrkExtendRowStart);
  LCommand.ResolveOrigin(LOrigin);
  LCommand.Prepare;
  Check(LCommand.Current and (LRanges.CellRange = nil),
    'original ordinary native movement is fully prepared before the fallible setter');
  LOldSelection := FGrid.OnSelection;
  FGrid.OnSelection := NativeRangeSelected;
  FRetireRangeSelection := True;
  try
    LCommand.Complete(True);
  finally

    if FInput.Current then
    begin
      FGrid.OnSelection := LOldSelection;
    end;
  end;
  Check((FRangeSelectionCallbacks = 1) and FInput.Retired and not FMount.Connected and
    (LRanges.CellRange = nil) and (FRangeClicks = LClicks) and (FHost.Writes = LBefore) and
    (FView.Store.Snapshot = LSource) and (TNyxCodec.Encode(FDocument) = FAuthored),
    'real native Col-setter retirement skips later borrowed-grid work and range publication without a ghost Click or data/source change');
  LCommand.Retire;
  LCommand := nil;
end;
{$endif}

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

procedure TCopyReview.RunAuthoredWorkbench; {$ifdef PAS2JS}async;{$endif}
var
  LRanges: INyxCollectionCellRangeMount;
  LDrafts: INyxCollectionCellDraftActions;
  LCommand: INyxCollectionClipboardCommand;
  LNewMount: INyxCollectionMount;
  LNewClipboard: INyxCollectionClipboardMount;
  LNewCommands: INyxCollectionClipboardCommands;
  LBefore: Integer;
  LReplies: Integer;
  LExpected: TNyxText;
  LRefused: Boolean;
begin
  FMode := kmObserve;
  FContextColumn := 0;
  Check(FAuthoredWorkbench and FView.Spec.HasTableWorkbenchPolicy and
    (FCellActions <> nil) and Supports(FCellActions, INyxCollectionCellDraftActions, LDrafts) and
    LDrafts.FocusedDraftEnabled,
    'saved fluent policy enables managed context and explicit draft options through ordinary Render');
  Check(Supports(FMount, INyxCollectionCellRangeMount, LRanges) and
    LRanges.SelectCells(NyxCollectionCellIdentity(ItemA, NameField),
      NyxCollectionCellIdentity(ItemB, NyxColumnIdentity(NyxIntegerField('Score')))),
    'authored range selection works without calling runtime ConfigureCellRange');
  FocusCell;
  LBefore := FHost.Writes;
  LReplies := FReplies;
  LExpected := '"' + CText + '"' + #9 + '1' + #13#10 + 'B' + #9 + '2';
  Check(CopyKey, 'actual target Ctrl+C claims the authored selected-cells action');
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check((FReplies = LReplies + 1) and (FLastPayload.Text = LExpected) and
    (FLastPayload.RowCount = 2) and (FLastPayload.FieldCount = 2),
    'authored no-header CRLF policy preserves RAW Unicode NUL and embedded line endings');

  if FUnavailableClipboard then
  begin
    Check((FHost.Writes = LBefore) and
      (FCommands.LastResult.Outcome = ncwoUnavailable) and
      (FCommands.LastResult.ByteCount = 0),
      'missing runtime service reports typed Unavailable without granting a platform write');
  end
  else
  begin
    Check((FHost.Writes = LBefore + 1) and (FHost.Text = LExpected) and
      (FCommands.LastResult.Outcome = ncwoWritten),
      'configured renderer service receives the same original physical copy once');
  end;
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check(HasMenuAction(NyxCellActionsInspectID) and HasMenuAction(NyxCellActionsCopyRangeID),
    'ordinary renderer-created menu exposes full inspection and selected-cells copy');
  LBefore := FHost.Writes;
  MenuAction(NyxCellActionsCopyRangeID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}

  if FUnavailableClipboard then
  begin
    Check((FHost.Writes = LBefore) and (FCellActions.LastResult.Outcome = ncwoUnavailable) and
      (FCellActions.Inspector <> nil) and FCellActions.Inspector.IsOpen and
      (FCellActions.Inspector.State.GetValue(NyxTextState(NyxCellInspectorStatusState)) =
        'Clipboard request did not write: ' + FCellActions.LastResult.Error),
      'real managed menu visibly presents unavailable copy while inspection stays usable');
  end
  else
  begin
    Check((FHost.Writes = LBefore + 1) and (FHost.Text = LExpected) and
      (FCellActions.LastResult.Outcome = ncwoWritten),
      'authored real menu reuses the configured service and original rectangle');
  end;
  FCellActions.Close;
  LRefused := False;
  try
    FRenderer.TableWorkbenchServices := NewNyxTableWorkbenchServices(FHostLease);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and FInput.Current and FMount.Connected,
    'mounted service replacement refuses without replacing accepted command authority');
  LCommand := FCommands.BeginCell(ItemA, NameField);
  LCommand.Prepare;
  LBefore := FHost.Writes;
  FRenderer.Unmount;
  LCommand.Complete(True);
  LCommand.Retire;
  LCommand := nil;
  Check(FInput.Retired and not FMount.Connected and (FHost.Writes = LBefore),
    'retired authored mount cannot transfer a prepared copy to a later renderer incarnation');
  FRenderer.TableWorkbenchServices := NewNyxTableWorkbenchServices(FHostLease);
  FRenderer.Render(FDocument, FDocument.Pages[0], FSurface, False, FState);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LNewMount := FRenderer.CollectionMount(CTable);
  Check(Supports(LNewMount, INyxCollectionClipboardMount, LNewClipboard) and
    (LNewClipboard.ClipboardCommands <> nil),
    'explicit Unmount permits new runtime services for a newly authored incarnation');
  LNewCommands := LNewClipboard.ClipboardCommands;
  LCommand := LNewCommands.BeginCell(ItemA, NameField);
  LCommand.Prepare;
  LCommand.Complete(True);
  LCommand.Retire;
  Check((FHost.Writes = LBefore + 1) and (FHost.Text = CText) and
    (LNewCommands.LastResult.Outcome = ncwoWritten),
    'replacement incarnation uses newly supplied service while old range and commands stay retired');
  Check(TNyxCodec.Encode(FDocument) = FAuthored,
    'rendered policy, clipboard services and runtime receipts never rewrite the saved design');
end;

{ Exercise permissions through ordinary renderer-created controls. Inspection
  is useful without clipboard permission; range-only and row-only policies must
  not accidentally authorize accepted/display/draft cell writes. Each fixture
  retains the same immutable source while its actual editor has unfinished text. }
procedure TCopyReview.RunAuthoredInspection; {$ifdef PAS2JS}async;{$endif}
var
  LPolicy: TNyxTableWorkbenchPolicy;
  LRanges: INyxCollectionCellRangeMount;
  LDrafts: INyxCollectionCellDraftActions;
  LSource: INyxCollectionSnapshot;
  LExpected: TNyxText;
  LVisibleText: TNyxText;
  LBefore: Integer;
  LReplies: Integer;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserPopover;
  LMemo: TJSHTMLTextAreaElement;
  LEditor: TJSHTMLInputElement;
  LOptions: TJSObject;
  {$else}
  LTarget: INyxLCLPopover;
  LMemo: TCustomEdit;
  LEditor: TCustomEdit;
  {$endif}

  procedure CheckReadonlyInspector(const AText: TNyxText);
  begin
    Check(Supports(FCellActions.Inspector,
      {$ifdef PAS2JS}INyxBrowserPopover{$else}INyxLCLPopover{$endif}, LTarget),
      'permission-limited inspection uses the real target popover');
    LMemo := {$ifdef PAS2JS}TJSHTMLTextAreaElement{$else}TCustomEdit{$endif}(
      LTarget.Renderer.FocusFor(NyxCellInspectorValueID));
    Check(LMemo.ReadOnly, 'permission-limited inspector uses an actual readonly memo');
    LVisibleText := {$ifdef PAS2JS}LMemo.value{$else}NyxLCLInputText(LMemo){$endif};
    {$ifndef PAS2JS}
    { Match the existing RunContext display oracle: native multiline controls
      expose CRLF while the escaped inspection notation uses LF. RAW payload
      and authored line-ending assertions above remain byte-for-byte exact. }
    LVisibleText := StringReplace(LVisibleText, #13#10, #10, [rfReplaceAll]);
    {$endif}
    Check(LVisibleText = NyxCollectionCellInspectionText(AText),
      'readonly inspection preserves full supplementary text and escaped control notation');
    Check(not HasInspectorAction(NyxCellInspectorCopyID) and
      not HasInspectorAction(NyxCellInspectorCopyDisplayID) and
      not HasInspectorAction(NyxCellInspectorCopyDraftID),
      'actual readonly inspector omits every forbidden copy control');
  end;

begin
  FMode := kmObserve;
  FContextColumn := 0;
  LPolicy := FView.Spec.TableWorkbenchPolicy;
  LSource := FView.Store.Snapshot;
  Check(FAuthoredWorkbench and not LPolicy.AllowsCopy(nwcaCell) and
    Supports(FCellActions, INyxCollectionCellDraftActions, LDrafts) and
    LDrafts.FocusedDraftEnabled,
    'authored no-cell-copy policy still installs explicit draft inspection');
  Check(Supports(FMount, INyxCollectionCellRangeMount, LRanges) and
    LRanges.SelectCells(NyxCollectionCellIdentity(ItemA, NameField),
      NyxCollectionCellIdentity(ItemB, NyxColumnIdentity(NyxIntegerField('Score')))),
    'range selection remains independent from permission to copy that range');
  FocusCell;
  LBefore := FHost.Writes;
  LReplies := FReplies;
  LExpected := '"' + CText + '"' + #9 + '1' + #13#10 + 'B' + #9 + '2';
  CopyKey;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}

  if LPolicy.AllowsCopy(nwcaCellRange) then
  begin
    Check((FHost.Writes = LBefore + 1) and (FReplies = LReplies + 1) and
      (FHost.Text = LExpected),
      'range-only permission permits exact physical selected-cells copy without cell-copy authority');
  end
  else
  begin
    Check((FHost.Writes = LBefore) and (FReplies = LReplies) and (FLastPayload = nil),
      'empty and row-only permissions cannot capture or write a physical cell/range copy');
  end;
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check(HasMenuAction(NyxCellActionsInspectID) and
    not HasMenuAction(NyxCellActionsCopyID) and
    not HasMenuAction(NyxCellActionsCopyDisplayID) and
    not HasMenuAction(NyxCellActionsCopyDraftID) and
    (HasMenuAction(NyxCellActionsCopyRangeID) = LPolicy.AllowsCopy(nwcaCellRange)) and
    (FCellActions.Snapshot.Text = CText),
    'real context always offers RAW inspection and independently permitted range action');

  if LPolicy.AllowsCopy(nwcaCellRange) then
  begin
    LBefore := FHost.Writes;
    MenuAction(NyxCellActionsCopyRangeID);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check((FHost.Writes = LBefore + 1) and (FHost.Text = LExpected),
      'actual range-only menu action transfers its original rectangle through the same host');
    Context;
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  end;
  LBefore := FHost.Writes;
  MenuAction(NyxCellActionsInspectID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  CheckReadonlyInspector(CText);
  Check(FHost.Writes = LBefore, 'readonly accepted-value inspection performs no clipboard effect');
  InspectorAction(NyxCellInspectorCloseID);
  FCellActions.Close;
  FocusCell;
  {$ifdef PAS2JS}
  await(Drain);
  Key(Cell, 'F2', False);
  await(Drain);
  LEditor := TJSHTMLInputElement(Cell.querySelector('input'));
  Check((LEditor <> nil) and (document.activeElement = LEditor),
    'restricted draft journey enters the actual focused browser cell editor');
  LEditor.value := CDraft;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['button'] := 2;
  LEditor.dispatchEvent(TCellContextEvent.new('contextmenu', LOptions));
  await(Drain);
  {$else}
  SendMessage(FGrid.Handle, WM_KEYDOWN, VK_F2, 1);
  Drain;
  Check(FGrid.EditorMode and (FGrid.Editor is TCustomEdit),
    'restricted draft journey enters the actual focused native cell editor');
  LEditor := TCustomEdit(FGrid.Editor);
  RestoreNyxLCLInputText(LEditor, CDraft);
  SendMessage(LEditor.Handle, WM_CONTEXTMENU, LEditor.Handle, MakeLParam(-1, -1));
  Drain;
  {$endif}
  Check(HasMenuAction(NyxCellActionsInspectDraftID) and
    not HasMenuAction(NyxCellActionsCopyID) and
    not HasMenuAction(NyxCellActionsCopyDisplayID) and
    not HasMenuAction(NyxCellActionsCopyDraftID) and
    (LDrafts.CapturedDraft <> nil) and (LDrafts.CapturedDraft.Value.Text = CDraft) and
    (LDrafts.CapturedDraft.Value.RawText = CText),
    'actual unfinished editor still offers separate draft inspection without any cell write permission');
  MenuAction(NyxCellActionsInspectDraftID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  CheckReadonlyInspector(CDraft);
  InspectorAction(NyxCellInspectorCloseID);
  FCellActions.Close;
  {$ifdef PAS2JS}
  Key(LEditor, 'Escape', False);
  await(Drain);
  {$else}
  SendMessage(FGrid.Handle, WM_KEYDOWN, VK_ESCAPE, 1);
  Drain;
  {$endif}
  Check((FHost.Writes = LBefore) and (FView.Store.Snapshot = LSource) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'restricted accepted/draft inspection leaves clipboard, accepted source and authored policy exact');
end;

{ Selected rows have their own explicit menu memberships. Selecting cells does
  not select records, and changing row membership cannot redirect an old menu
  to another record. Both arranged and ordinary real controls run this journey. }
procedure TCopyReview.RunAuthoredRowMenu; {$ifdef PAS2JS}async;{$endif}
var
  LSource: INyxCollectionSnapshot;
  LPayload: INyxCollectionClipboardText;
  LSourcePayload: INyxCollectionClipboardText;
  LIdentities: INyxCollectionCellIdentityActions;
  LSourceIDs: INyxCollectionIdentityText;
  LPageIDs: INyxCollectionIdentityText;
  LBefore: Integer;
begin
  FMode := kmObserve;
  FContextColumn := 0;
  LSource := FView.Store.Snapshot;
  ExportPaging(True);
  FView.SetSelection([ItemA, ItemB], ItemA, ItemA);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check((FView.Snapshot.Count = 1) and (FView.Selection.Count = 2),
    'actual row menu begins with selected source records extending beyond the current page');
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check(HasMenuAction(NyxCellActionsCopySourceRowsID) and
    HasMenuAction(NyxCellActionsCopyPageRowsID) and
    HasMenuAction(NyxCellActionsCopySourceIDsID) and
    HasMenuAction(NyxCellActionsCopyPageIDsID) and
    HasMenuAction(NyxCellActionsInspectID) and not HasMenuAction(NyxCellActionsCopyID),
    'row-only authored permissions expose both exact memberships without cell-copy authority');
  LBefore := FHost.Writes;
  MenuAction(NyxCellActionsCopySourceRowsID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LSourcePayload := FCommands.LastPayload;
  Check((FHost.Writes = LBefore + 1) and (LSourcePayload.RowCount = 2) and
    (LSourcePayload.ItemAt(0).ID = ItemA.ID) and (LSourcePayload.ItemAt(1).ID = ItemB.ID) and
    (LSourcePayload.CellValue(0, 0).TextValue = CText) and
    (LSourcePayload.CellValue(1, 0).TextValue = 'B') and
    (FHost.Text = LSourcePayload.Text) and (Pos('"' + CText + '"' + #9, FHost.Text) = 1) and
    (Pos(#13#10 + 'B' + #9, FHost.Text) > 0),
    'actual selected-source menu copies both original records with exact RAW text, no header and CRLF rows');
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LBefore := FHost.Writes;
  MenuAction(NyxCellActionsCopyPageRowsID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LPayload := FCommands.LastPayload;
  Check((FHost.Writes = LBefore + 1) and (LPayload.RowCount = 1) and
    (LPayload.ItemAt(0).ID = ItemA.ID) and (LPayload.CellValue(0, 0).TextValue = CText) and
    (FHost.Text = LPayload.Text) and (LSourcePayload.RowCount = 2),
    'actual current-page row action intersects the original page without changing its earlier full-source capture');
  Check(Supports(FCellActions, INyxCollectionCellIdentityActions, LIdentities),
    'ordinary authored actions expose a separately typed identity result');
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LBefore := FHost.Writes;
  MenuAction(NyxCellActionsCopySourceIDsID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LSourceIDs := LIdentities.LastIdentities;
  Check((FHost.Writes = LBefore + 1) and LIdentities.HasIdentityResult and
    (LIdentities.LastIdentityResult.Outcome = ncwoWritten) and
    (LSourceIDs.Count = 2) and (LSourceIDs.ItemAt(0).ID = ItemA.ID) and
    (LSourceIDs.ItemAt(1).ID = ItemB.ID) and
    (FHost.Text = ItemA.ID + #13#10 + ItemB.ID) and
    (FHost.Text = LSourceIDs.Text) and (FCommands.LastPayload = LPayload),
    'actual source-ID menu copies only exact ordered IDs with a distinct receipt and no invented cell payload');
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LBefore := FHost.Writes;
  MenuAction(NyxCellActionsCopyPageIDsID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LPageIDs := LIdentities.LastIdentities;
  Check((FHost.Writes = LBefore + 1) and (LPageIDs.Count = 1) and
    (LPageIDs.ItemAt(0).ID = ItemA.ID) and (FHost.Text = ItemA.ID) and
    (LSourceIDs.Count = 2) and (FCommands.LastPayload = LPayload),
    'actual page-ID action intersects the captured page and leaves earlier identity and RAW-row values intact');
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check(HasMenuAction(NyxCellActionsCopySourceRowsID),
    'a fresh exact selected-row menu is open before row membership changes');
  FView.SetSelection([ItemB], ItemA, ItemA);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LBefore := FHost.Writes;
  MenuAction(NyxCellActionsCopySourceRowsID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check((FHost.Writes = LBefore) and (FCellActions.LastResult.Outcome = ncwoCancelled) and
    (FCellActions.Inspector <> nil) and FCellActions.Inspector.IsOpen and
    (FCellActions.Inspector.State.GetValue(NyxTextState(NyxCellInspectorStatusState)) <> '') and
    (FCellActions.Snapshot.Text = CText),
    'changing selected records visibly refuses the original row action while independent RAW inspection remains useful');
  InspectorAction(NyxCellInspectorCloseID);
  FCellActions.Close;
  FView.SetSelection([ItemA, ItemB], ItemA, ItemA);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  FView.SetSelection([ItemB], ItemA, ItemA);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  LBefore := FHost.Writes;
  MenuAction(NyxCellActionsCopySourceIDsID);
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check((FHost.Writes = LBefore) and LIdentities.HasIdentityResult and
    (LIdentities.LastIdentityResult.Outcome = ncwoCancelled) and
    (FCellActions.Inspector <> nil) and FCellActions.Inspector.IsOpen and
    (FCellActions.Snapshot.Text = CText),
    'changed selection visibly refuses the original ID action through its distinct cancelled receipt');
  InspectorAction(NyxCellInspectorCloseID);
  FCellActions.Close;
  FView.ClearSelection;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  FocusCell;
  Context;
  {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
  Check(HasMenuAction(NyxCellActionsInspectID) and
    not HasMenuAction(NyxCellActionsCopySourceRowsID) and
    not HasMenuAction(NyxCellActionsCopyPageRowsID) and
    not HasMenuAction(NyxCellActionsCopySourceIDsID) and
    not HasMenuAction(NyxCellActionsCopyPageIDsID) and
    (FHost.Writes = LBefore) and (FView.Store.Snapshot = LSource) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'empty row membership offers useful inspection without phantom copy actions or data/design mutation');
end;

procedure TCopyReview.Run; {$ifdef PAS2JS}async;{$endif}
var
  LRangeMount: INyxCollectionCellRangeMount;
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
  await(RunRangeMenu);
  await(RunPointerRange);
  {$else}
  RunContext;
  RunExport;
  RunDisplayed;
  RunFocusedDraft;
  RunCellRange;
  RunRangeMenu;
  RunPointerRange;
  {$endif}
  Check(Supports(FCellActions, INyxCollectionCellActionIntent, LCellIntent),
    'retirement probe retains the actual canonical context facade');
  Check(Supports(FMount, INyxCollectionCellRangeMount, LRangeMount),
    'retirement probe retains the public range facet separately from its renderer');
  LBefore := FHost.Writes;
  FMode := kmRetire;
  CopyKey;
  Check(FInput.Retired and not FMount.Connected and (FHost.Writes = LBefore),
    'canonical callback retirement cannot publish from an old key tail');
  LRangeMount.ClearCellRange;
  Check((LRangeMount.CellRange = nil) and (FHost.Writes = LBefore),
    'retained public range clear remains inert after target and input retirement');
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
      await(LReview.RunRangeMenu);
      await(LReview.RunPointerRange);
      {$else}
      LReview.RunFocusedDraft;
      LReview.RunDraftActions;
      LReview.RunCellRange;
      LReview.RunRangeMenu;
      LReview.RunPointerRange;
      LReview.RunRangeFocusRetirement;
      {$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(True, True);
    try
      {$ifdef PAS2JS}await(LReview.RunDraftActions);{$else}LReview.RunDraftActions;{$endif}
      {$ifndef PAS2JS}

      if (ParamCount > 0) and (ParamStr(1) = '--probe-native-pointer') then
      begin
        GPointerUnavailable := not LReview.RunNativePointerProbe;
      end;
      {$endif}
    finally
      LReview.Free;
    end;
    { These use the same actual controls/input routes, with authored intent
      replacing every manual copy/range/menu configuration step. No real OS
      clipboard adapter is installed, including in the absent-service case. }
    LReview := TCopyReview.Create(True, True, True);
    try
      {$ifdef PAS2JS}await(LReview.RunAuthoredWorkbench);
      {$else}LReview.RunAuthoredWorkbench;{$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(False, True, True, True);
    try
      {$ifdef PAS2JS}await(LReview.RunAuthoredWorkbench);
      {$else}LReview.RunAuthoredWorkbench;{$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(True, True, True, False, []);
    try
      {$ifdef PAS2JS}await(LReview.RunAuthoredInspection);
      {$else}LReview.RunAuthoredInspection;{$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(False, True, True, False,
      [nwcaSelectedSource, nwcaSelectedPage]);
    try
      {$ifdef PAS2JS}await(LReview.RunAuthoredInspection);
      {$else}LReview.RunAuthoredInspection;{$endif}
    finally
      LReview.Free;
    end;
    { Reuse the unchanged real export journey, this time with application
      services supplied before Render. No manual ConfigureExport/CellActions
      call can hide an ordering error in authored renderer adoption. }
    LReview := TCopyReview.Create(True, True, True, False,
      [nwcaCell, nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange], True);
    try
      {$ifdef PAS2JS}await(LReview.RunExport);
      {$else}LReview.RunExport;{$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(False, True, True, False,
      [nwcaCell, nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange], True);
    try
      {$ifdef PAS2JS}await(LReview.RunExport);
      {$else}LReview.RunExport;{$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(True, True, True, False, [nwcaCellRange]);
    try
      {$ifdef PAS2JS}await(LReview.RunAuthoredInspection);
      {$else}LReview.RunAuthoredInspection;{$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(True, True, True, False,
      [nwcaSelectedSource, nwcaSelectedPage]);
    try
      {$ifdef PAS2JS}await(LReview.RunAuthoredRowMenu);
      {$else}LReview.RunAuthoredRowMenu;{$endif}
    finally
      LReview.Free;
    end;
    LReview := TCopyReview.Create(False, True, True, False,
      [nwcaSelectedSource, nwcaSelectedPage]);
    try
      {$ifdef PAS2JS}await(LReview.RunAuthoredRowMenu);
      {$else}LReview.RunAuthoredRowMenu;{$endif}
    finally
      LReview.Free;
    end;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-clipboard-controls', 'passed');
    document.body.setAttribute('data-clipboard-control-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection clipboard control checks; injected host/no OS write');

    if GPointerUnavailable then
    begin
      ExitCode := 2;
    end
    else if (ParamCount = 0) or (ParamStr(1) <> '--probe-native-pointer') then
    begin
      WriteLn('Native WM pointer probe not requested; functional result makes no OS pointer claim');
    end;
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
