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
  nyx.codec, nyx.behavior, nyx.events, nyx.scheduler, nyx.clipboard,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.selection, nyx.collections.query, nyx.collections.arrangement,
  nyx.collections.mount, nyx.collections.clipboard, nyx.collections.clipboard.actions
  , nyx.collections.cell.actions, nyx.menu, nyx.popover
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.collections.cell.browser,
    nyx.menu.browser, nyx.popover.browser
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

  {$ifndef PAS2JS}
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
    FCellMount: INyxCollectionCellActionsMount;
    FCellActions: INyxCollectionCellActions;
    FMode: TKeyMode;
    FKeys: Integer;
    FConsumed: Integer;
    FContexts: Integer;
    FInspectorLegacyClicks: Integer;
    FReplies: Integer;
    FLastPayload: INyxCollectionClipboardText;
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
    {$endif}
    procedure FocusCell;
    function CopyKey: Boolean;
    procedure EditValue(const AText: TNyxText);
    procedure Filter(AEnabled: Boolean);
    procedure Context(AWait: Boolean = True);
    procedure MenuAction(const AID: TNyxText);
    procedure InspectorAction(const AID: TNyxText);
    procedure RunContext; {$ifdef PAS2JS}async;{$endif}
    procedure Semantic(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure CloseInspectorOnClick(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Reply(const APayload: INyxCollectionClipboardText;
      const AResult: TNyxClipboardWriteResult);
  public
    constructor Create;
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

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(Dataset,
      NyxCollectionSchema.Text(NyxTextField('Name'), '').Integer(NyxIntegerField('Score'), 0), [
        NyxCollectionItem(ItemA).WithValue(NyxTextField('Name'), CText)
          .WithValue(NyxIntegerField('Score'), 1),
        NyxCollectionItem(ItemB).WithValue(NyxTextField('Name'), 'B')
          .WithValue(NyxIntegerField('Score'), 2)]);
    LPage := NewNyxPage('copy-page');
    LPage.Configure.Padding(12).Gap(8).Done;
    LTable := NewNyxTable(CTable);
    LTable.Configure.Height(240).Done;
    LTable.Binds.Collection(NyxCollectionView(Dataset).Selection(nsmMultiple)
      .Column(NyxTextField('Name'), 'Name', cmEditable)
      .Column(NyxIntegerField('Score'), 'Score').TableLayout(NyxTableLayout));
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
  Text := TNyxText(E.Message);
end;
{$endif}

constructor TCopyReview.Create;
var
  LCallback: TKeyCallback;
  LAdmission: INyxCollectionInputAdmission;
begin
  inherited Create;
  FDocument := Fixture;
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
    FRenderer.Events.Scheduler, TNyxCollectionClipboardOptions.Defaults);
  FCommands := FClipboard.ClipboardCommands;
  FCommands.Observe({$ifdef PAS2JS}@{$endif}Reply);
  Check(Supports(FMount, INyxCollectionCellActionsMount, FCellMount),
    'actual mounted table exposes managed cell context actions');
  FCellMount.ConfigureCellActions(
    {$ifdef PAS2JS}NewNyxBrowserCollectionCellPresenterFactory(FTable)
    {$else}NewNyxLCLCollectionCellPresenterFactory(FGrid){$endif},
    FRenderer.Events.Scheduler);
  FCellActions := FCellMount.CellActions;
  LCallback := TKeyCallback.Create;
  LCallback.Owner := Self;
  FCallback := LCallback;
  FToken := FRenderer.Events.OnKeyDown(NyxControlEvents(CTable, niRuntime)).Subscribe(FCallback);
  FContextToken := FRenderer.Events.OnContextMenu(NyxControlEvents(CTable, niRuntime)).Subscribe(FCallback);
end;

destructor TCopyReview.Destroy;
begin

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
  FCellMount := nil;
  FClipboard := nil;
  FMount := nil;
  FView := nil;
  FHostLease := nil;
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
    '[data-nyx-item="a"] [role="gridcell"][data-nyx-column="0"]'));
  Check(Result <> nil, 'actual resident typed Name cell exists');
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
  FGrid.Col := 0;
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
  LBounds := FGrid.CellRect(0, 1);
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
  {$else}
  RunContext;
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
