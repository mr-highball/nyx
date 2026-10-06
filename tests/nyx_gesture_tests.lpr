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
program nyx_gesture_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.gestures, nyx.behavior, nyx.events, nyx.callbacks, nyx.scheduler,
  nyx.schema, nyx.codegen, nyx.codec, nyx.studio.session,
  {$ifdef NYX_COMPILED_GESTURES}nyx.gestures.fixture,{$endif}
  {$ifdef PAS2JS}JS, Web, nyx.gestures.browser, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, LMessages,
  nyx.gestures.lcl, nyx.render.lcl;{$endif}

type
  { Retained callback data contains no platform control or transfer handle.
    A renderer is borrowed only during this mounted journey. }
  TGestureProbe = class(TNyxEventCallback)
    Counts: array[TNyxTrigger] of Integer;
    Last: array[TNyxTrigger] of TNyxEventInfo;
    Requests: TNyxGestureCapabilities;
    Offer: Boolean;
    Accept: TNyxDropOperation;
    Navigate: TNyxTrigger;
    Retained: INyxGestureResponse;
    {$ifdef PAS2JS}Renderer: TNyxBrowserRenderer;
    {$else}Renderer: TNyxLCLRenderer;{$endif}
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  { Exercise the response lifetime independently of a particular widget adapter.
    Each registration owns its response only for the current invocation. }
  TResponseProbe = class(TNyxEventCallback)
    Calls: Integer;
    Operation: TNyxDropOperation;
    Attempt: Boolean;
    FailAfterRequest: Boolean;
    Refused: Boolean;
    SawAuthority: Boolean;
    CancelSelf: INyxEventSubscription;
    Retained: INyxGestureResponse;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifndef PAS2JS}
  TControlAccess = class(TControl);
  TNativeFailure = class
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$else}
  TDragEvent = class external name 'DragEvent' (TJSDragEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$endif}

const
  CText: TNyxText = 'A🌙é漢 / owned transfer';
  CTransferName: TNyxText = '🌙';
  CTransferFileName: TNyxText = 'drawing🌙.pas';
  CGestureTriggers: array[0..9] of TNyxTrigger = (ntPointerCancel,
    ntPointerCapture, ntPointerCaptureLost, ntDragStart, ntDrag, ntDragEnter,
    ntDragOver, ntDragExit, ntDrop, ntDragEnd);
var
  GChecks: Integer;
  {$ifndef PAS2JS}GFailure: TNativeFailure;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxGesture.Create('Gesture: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure TNativeFailure.Failed(ASender: TObject; AException: Exception);
begin
  WriteLn(StdErr, 'FAIL native callback: ', AException.Message);
  DumpExceptionBackTrace(StdErr);
  Flush(StdErr);
  Halt(1);
end;
{$endif}

procedure TGestureProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LCapability: TNyxGestureCapability;
  LResponse: INyxGestureResponse;
begin
  Inc(Counts[AEvent.Trigger]);
  Last[AEvent.Trigger] := AEvent.Copy;
  LResponse := NyxGestureResponse(AExecution);
  Retained := LResponse;
  Requests := [];
  for LCapability := Low(TNyxGestureCapability) to High(TNyxGestureCapability) do
  begin

    if LResponse.CanRequest(LCapability) then
    begin
      Include(Requests, LCapability);
    end;
  end;

  if (AEvent.Trigger = ntPointerDown) and (ngcCapturePointer in Requests) then
  begin
    LResponse.CapturePointer;
  end;

  if (AEvent.Trigger = ntPointerUp) and (ngcReleasePointer in Requests) then
  begin
    LResponse.ReleasePointer;
  end;

  if Offer and (ngcOfferDrag in Requests) then
  begin
    LResponse.OfferDrag(NyxTransferText(CText).WithItem(
      NyxTransferValue(NyxObject([NyxField('name', NyxData('🌙'))]))),
      [ndoCopy, ndoMove]);
  end;

  if ngcAcceptDrop in Requests then
  begin
    LResponse.AcceptDrop(Accept);
  end;

  if AEvent.Trigger = Navigate then
  begin
    Renderer.Unmount;
  end;
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LSource: INyxButton;
  LTarget: INyxCard;
begin
  Result := TNyxDocument.Create;
  LPage := NewNyxPage('home');
  LSource := NewNyxButton('transfer-button').WithText('Transfer');
  LSource.Configure.DragSource(True).TouchBehavior(ntbNone);
  LTarget := NewNyxCard('drop-card');
  LTarget.Configure.DropTarget(True).Height(140);
  LPage.Add(LSource).Add(LTarget);
  Result.AddPage(LPage.Node);
  NyxCallbacks(LSource.Node).OnDragStart.Add(NyxHandler('TGestureAction'),
    NyxCallbackID('transfer.observe-start'));
  NyxCallbacks(LTarget.Node).OnDrop.Add(NyxHandler('TGestureAction'),
    NyxCallbackID('transfer.observe-drop'));
end;

procedure ContractChecks;
var
  LBase: TNyxTransferSnapshot;
  LCopy: TNyxTransferSnapshot;
  LProtected: TNyxTransferSnapshot;
  LFile: TNyxTransferFileInfo;
  LFiles: TNyxTransferFiles;
  LDecision: INyxGestureDecision;
  LResult: TNyxGestureResult;
  LPhase: TNyxDragPhase;
  LDrag: TNyxDragSnapshot;
  LEvent: TNyxEventInfo;
  LSaved: TNyxEventInfo;
  LOperation: TNyxDropOperation;
  LParsedOperation: TNyxDropOperation;
  LOperations: TNyxDropOperations;
  LBad: Boolean;
  LIndex: Integer;
  LName: TNyxText;
begin
  LBase := Default(TNyxTransferSnapshot);
  Check(not LBase.Defined and not LBase.Readable and (LBase.Count = 0),
    'a default snapshot has no data authority');
  Check(NyxTransferFormat('APPLICATION/X-NYX+JSON').Name = NyxValueTransferFormat.Name,
    'open MIME identities canonicalize exactly');
  for LIndex := 0 to 5 do
  begin
    case LIndex of
      0: LName := '';
      1: LName := 'text';
      2: LName := '/plain';
      3: LName := 'text/';
      4: LName := 'text/plain; charset=utf-8';
    else
      LName := 'text/🌙';
    end;
    LBad := False;
    try
      NyxTransferFormat(LName);
    except
      on ENyxGesture do
      begin
        LBad := True;
      end;
    end;
    Check(LBad, 'malformed format is refused at the explicit open boundary');
  end;
  LBase := NyxTransferText(CText).WithItem(NyxTransferMarkup('<em>🌙</em>'))
    .WithItem(NyxTransferValue(NyxObject([NyxField('name', NyxData('🌙')),
      NyxField('amount', NyxData(NyxDecimal('9007199254740991')))])));
  LCopy := LBase.WithItem(NyxTransferText(''));
  Check((LBase.Count = 3) and (LCopy.Count = 3) and
    (LBase.TextFor(NyxTextTransferFormat) = CText) and
    (LCopy.TextFor(NyxTextTransferFormat) = ''), 'replacing one format preserves a retained baseline');
  Check(LCopy.HasFormat(NyxTextTransferFormat) and not LCopy.HasFormat(NyxURITransferFormat),
    'present empty text differs from an absent format');
  Check(LBase.Value.Field('name').AsText = CTransferName, 'typed structured values preserve Unicode');
  LFile := Default(TNyxTransferFileInfo);
  LFile.Name := CTransferFileName;
  LFile.MediaType := 'text/plain';
  LFile.Size := 9007199254740991.0;
  LFile.Modified := 1790993512000;
  SetLength(LFiles, 1);
  LFiles[0] := LFile;
  LCopy := LBase.WithFiles(LFiles);
  LFiles[0].Name := 'changed.pas';
  Check(LCopy.HasFiles and (LCopy.FileCount = 1) and
    (LCopy.Files[0].Name = CTransferFileName) and not LBase.HasFiles,
    'file metadata owns independent exact values without file handles');
  LProtected := LCopy.ProtectedCopy;
  Check(not LProtected.Readable and LProtected.HasFiles and
    (LProtected.Count = 3), 'hover advertises formats and files without payload authority');
  LBad := False;
  try
    LProtected.TextFor(NyxTextTransferFormat);
  except
    on ENyxGesture do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'protected text access explicitly fails');
  LBad := False;
  try
    LIndex := LProtected.FileCount;
  except
    on ENyxGesture do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'protected file metadata explicitly fails');
  LFile.Size := 1.5;
  LBad := False;
  try
    LProtected := LCopy.WithFiles([LFile]);
  except
    on ENyxGesture do
    begin
      LBad := True;
    end;
  end;
  Check(LBad and (LCopy.Files[0].Size = 9007199254740991.0),
    'invalid metadata fails before changing an admitted snapshot');
  LBad := False;
  try
    NyxTransferFromData(NyxArray([NyxObject([NyxField('format', NyxData('text/plain')),
      NyxField('text', NyxData('one'))]), NyxObject([
      NyxField('format', NyxData('TEXT/PLAIN')), NyxField('text', NyxData('two'))])]), True);
  except
    on ENyxGesture do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'duplicate canonical formats are refused atomically');
  for LPhase := Low(TNyxDragPhase) to High(TNyxDragPhase) do
  begin
    LDrag := NyxDragSnapshot(LPhase, LBase, [ndoCopy, ndoMove], ndoCopy, 'source🌙', True);
    Check(LDrag.Transfer.Readable = (LPhase in [ndpStart, ndpDrop]),
      'payload access follows the real drag phase');
    Check(LDrag.CanRespond = (LPhase in [ndpStart, ndpEnter, ndpOver, ndpDrop]),
      'observations cannot manufacture a response window');
  end;
  LEvent := Default(TNyxEventInfo);
  LEvent.Value := NyxNull;
  LEvent.HasDrag := True;
  LEvent.Drag := NyxDragSnapshot(ndpDrop, LBase, [ndoCopy], ndoCopy, 'source🌙', True);
  LSaved := LEvent.Copy;
  LBase := NyxTransferText('replaced');
  Check(LSaved.HasDrag and (LSaved.Drag.Transfer.TextFor(NyxTextTransferFormat) = CText),
    'event copies retain owned transfer data');
  LDecision := NewNyxGestureDecision([ngcCapturePointer, ngcReleasePointer, ngcOfferDrag]);
  LDecision.CapturePointer;
  LDecision.ReleasePointer;
  LDecision.OfferDrag(LBase, [ndoCopy]);
  LResult := LDecision.Seal;
  Check((LResult.PointerRequest = nprRelease) and LResult.Offered and
    (LResult.Allowed = [ndoCopy]), 'last valid sequential decision wins');
  Check(not LDecision.CanRequest(ngcCapturePointer), 'sealed decisions revoke authority');
  LBad := False;
  try
    LDecision.CapturePointer;
  except
    on ENyxGesture do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'retained decisions refuse late physical requests');
  LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoCopy]);
  LBad := False;
  try
    LDecision.AcceptDrop(ndoMove);
  except
    on ENyxGesture do
    begin
      LBad := True;
    end;
  end;
  LDecision.AcceptDrop(ndoNone);
  LResult := LDecision.Seal;
  Check(LBad and LResult.Accepted and (LResult.Operation = ndoNone),
    'disallowed moves fail and None is explicit rejection');
  for LOperation := Low(TNyxDropOperation) to High(TNyxDropOperation) do
  begin
    Check(TryNyxDropOperation(NyxDropOperationName(LOperation), LParsedOperation) and
      (LOperation = LParsedOperation),
      'closed host operation names round-trip');
  end;
  Check(TryNyxDropOperations('copyMove', LOperations) and
    (LOperations = [ndoCopy, ndoMove]) and
    (NyxDropOperationsName(LOperations) = 'copyMove'), 'allowed operations use portable set semantics');
end;

procedure TResponseProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);
  Retained := NyxGestureResponse(AExecution);

  if CancelSelf <> nil then
  begin
    CancelSelf.Cancel;
  end;
  SawAuthority := Retained.CanRequest(ngcAcceptDrop);
  Refused := False;

  if Attempt then
  begin
    try
      Retained.AcceptDrop(Operation);
    except
      on ENyxSchedule do
      begin
        Refused := True;
      end;
    end;
  end;

  if FailAfterRequest then
  begin
    raise Exception.Create('Owned gesture callback failure');
  end;
end;

procedure ResponseChecks;
var
  LRouter: INyxEvents;
  LFirst: TResponseProbe;
  LSecond: TResponseProbe;
  LFirstOwner: INyxEventCallback;
  LSecondOwner: INyxEventCallback;
  LFirstToken: INyxEventSubscription;
  LSecondToken: INyxEventSubscription;
  LDecision: INyxGestureDecision;
  LEvent: TNyxEventInfo;
  LRejected: Boolean;
begin
  LRouter := NewNyxEvents;
  LFirst := TResponseProbe.Create;
  LFirstOwner := LFirst;
  LSecond := TResponseProbe.Create;
  LSecondOwner := LSecond;
  try
    LFirst.Operation := ndoCopy;
    LSecond.Operation := ndoMove;
    LFirst.Attempt := True;
    LSecond.Attempt := True;
    LFirstToken := LRouter.OnDragOver(NyxControlEvents('target')).Subscribe(LFirstOwner);
    LSecondToken := LRouter.OnDragOver(NyxControlEvents('target')).Subscribe(LSecondOwner);
    LEvent := Default(TNyxEventInfo);
    LEvent.Value := NyxNull;
    LEvent.Trigger := ntDragOver;
    LEvent.Name := NyxEvent(NyxTriggerName(ntDragOver));
    LEvent.HasDrag := True;
    LEvent.Drag := NyxDragSnapshot(ndpOver, NyxTransferText(CText),
      [ndoCopy, ndoMove], ndoNone, 'source', True);
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoCopy, ndoMove]);
    LRouter.DispatchGesture(LEvent, 'target', 'target', LDecision);
    Check((LFirst.Calls = 1) and (LSecond.Calls = 1) and
      LFirst.SawAuthority and LSecond.SawAuthority and
      (LDecision.Seal.Operation = ndoMove),
      'last valid sequential registration decides without suppressing siblings');
    Check(not LFirst.Retained.CanRequest(ngcAcceptDrop) and
      not LSecond.Retained.CanRequest(ngcAcceptDrop),
      'returned sibling response contexts both revoke physical authority');
    LRejected := False;
    try
      LFirst.Retained.AcceptDrop(ndoCopy);
    except
      on ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'retained callback response explicitly refuses late acceptance');
    LFirst.FailAfterRequest := True;
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoCopy, ndoMove]);
    LRouter.DispatchGesture(LEvent, 'target', 'target', LDecision);
    Check((LFirstToken.LastExecution.Status = nesFailed) and
      (LSecondToken.LastExecution.Status = nesSucceeded) and
      (LSecond.Calls = 2) and (LDecision.Seal.Operation = ndoMove),
      'owned callback failure retains later sibling negotiation');
    LFirst.FailAfterRequest := False;
    LFirst.CancelSelf := LFirstToken;
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoCopy, ndoMove]);
    LRouter.DispatchGesture(LEvent, 'target', 'target', LDecision);
    Check(not LFirstToken.Active and not LFirst.SawAuthority and LFirst.Refused and
      (LSecond.Calls = 3) and (LDecision.Seal.Operation = ndoMove),
      'self-cancellation revokes only its own physical response');
    LRouter.Dispatch(LEvent, 'target', 'target');
    Check(not LSecond.SawAuthority and LSecond.Refused,
      'ordinary dispatch cannot acquire a physical gesture response window');
    LEvent.HasDrag := False;
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoCopy]);
    LRejected := False;
    try
      LRouter.DispatchGesture(LEvent, 'target', 'target', LDecision);
    except
      on ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LDecision.CanRequest(ngcAcceptDrop) and
      (LSecond.Calls = 4),
      'missing physical context is rejected and sealed before invoking callbacks');
  finally
    LFirst.CancelSelf := nil;
    LRouter.Close;
  end;
end;

procedure AuthoringChecks;
var
  LDocument: TNyxDocument;
  LMetadata: TNyxEventSchemas;
  LSource: TNyxText;
  LWire: TNyxText;
  LSession: TNyxStudioSession;
  LHandler: TNyxHandlerRef;
  LLine: Integer;
  LIndex: Integer;
  LCount: Integer;
begin
  {$ifdef NYX_COMPILED_GESTURES}LDocument := nyx.gestures.fixture.BuildNyxDocument;
  {$else}LDocument := Fixture;{$endif}
  try
    LMetadata := NyxEventsMetadata(LDocument.Pages[0].Find('transfer-button'), LDocument);
    LCount := 0;
    for LIndex := 0 to High(LMetadata) do
    begin

      if LMetadata[LIndex].Trigger in [ntDragStart, ntDrag, ntDragEnter,
        ntDragOver, ntDragExit, ntDrop, ntDragEnd] then
      begin
        Inc(LCount);
        Check(nctxDrag in LMetadata[LIndex].Contexts, 'drag context is semantically discoverable');

        if LMetadata[LIndex].Trigger = ntDrag then
        begin
          Check(LMetadata[LIndex].Native = ncMissing, 'LCL has no invented source-progress event');
        end;
      end;
    end;
    Check(LCount = 7, 'all drag phases appear in selected-control metadata');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('INyxButton', LSource) > 0) and (Pos('INyxCard', LSource) > 0) and
      (Pos('.DragSource(True)', LSource) > 0) and (Pos('.DropTarget(True)', LSource) > 0) and
      (Pos('.TouchBehavior(ntbNone)', LSource) > 0) and (Pos('nyx.gestures', LSource) > 0),
      'crafted source uses specialized controls and typed configuration');
    LWire := TNyxCodec.Encode(LDocument);
    LDocument.Free;
    LDocument := TNyxCodec.Decode(LWire);
    Check(LDocument.Pages[0].Find('transfer-button').Prop('touch-behavior') = 'none',
      'portable persistence preserves touch directives');
  finally
    LDocument.Free;
  end;
  LSession := TNyxStudioSession.Create;
  try
    LSession.Select('project-name');
    LWire := TNyxCodec.Encode(LSession.Document);
    for LIndex := 0 to High(CGestureTriggers) do
    begin
      LHandler := LSession.AddCallback(CGestureTriggers[LIndex], LLine);
      Check((LLine > 0) and (LSession.CallbackLine(LHandler) > 0) and
        (Pos('TODO', LSession.Source) > 0), 'Studio creates navigable typed gesture callbacks');
    end;
    LSource := LSession.Source;
    for LIndex := 0 to High(CGestureTriggers) do
    begin
      LSession.Undo;
    end;
    Check(TNyxCodec.Encode(LSession.Document) = LWire, 'gesture callback undo restores exact design');
    for LIndex := 0 to High(CGestureTriggers) do
    begin
      LSession.Redo;
    end;
    Check(LSession.Source = LSource, 'gesture callback redo preserves the exact companion');
  finally
    LSession.Free;
  end;
end;

procedure ControlChecks;
var
  LDocument: TNyxDocument;
  LProbe: TGestureProbe;
  LProbeOwner: INyxEventCallback;
  LSubscriptions: array of INyxEventSubscription;
  LIndex: Integer;
  LDragTransfer: TNyxTransferSnapshot;
  LTargetID: TNyxText;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LRenderer: TNyxBrowserRenderer;
  LSource: TJSHTMLElement;
  LTarget: TJSHTMLElement;
  LTransfer: TJSDataTransfer;
  LOptions: TJSObject;
  LEvent: TDragEvent;
  LName: String;
  {$else}
  LHost: TForm;
  LRenderer: TNyxLCLRenderer;
  LSource: TControl;
  LTarget: TControl;
  LDrag: TDragObject;
  LAccept: Boolean;
  LMessage: TLMessage;
  {$endif}

  procedure Mount;
  begin
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$ifdef PAS2JS}
    LSource := LRenderer.ElementFor('transfer-button');
    LTarget := LRenderer.ElementFor('drop-card');
    {$else}
    LSource := LRenderer.ControlFor('transfer-button');
    LTarget := LRenderer.ControlFor('drop-card');
    {$endif}
  end;
begin
  {$ifdef NYX_COMPILED_GESTURES}LDocument := nyx.gestures.fixture.BuildNyxDocument;
  {$else}LDocument := Fixture;{$endif}
  LProbe := TGestureProbe.Create;
  LProbeOwner := LProbe;
  LProbe.Offer := True;
  LProbe.Accept := ndoCopy;
  LProbe.Navigate := ntDesignSelect;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('section'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 640, 480);
  LHost.HandleNeeded;
  LRenderer := TNyxLCLRenderer.Create;
  {$endif}
  LProbe.Renderer := LRenderer;
  try
    Mount;
    {$ifdef NYX_COMPILED_GESTURES}BindNyxCallbacks(LDocument, LRenderer.Events);{$endif}
    SetLength(LSubscriptions, Length(CGestureTriggers) + 2);
    for LIndex := 0 to High(CGestureTriggers) do
    begin
      LTargetID := 'transfer-button';

      if CGestureTriggers[LIndex] in [ntDragEnter, ntDragOver, ntDragExit, ntDrop] then
      begin
        LTargetID := 'drop-card';
      end;
      LSubscriptions[LIndex] := LRenderer.Events.On(NyxControlEvents(LTargetID),
        CGestureTriggers[LIndex]).Subscribe(LProbe);
    end;
    LSubscriptions[10] := LRenderer.Events.OnPointerDown(NyxControlEvents('transfer-button')).Subscribe(LProbe);
    LSubscriptions[11] := LRenderer.Events.OnPointerUp(NyxControlEvents('transfer-button')).Subscribe(LProbe);
    {$ifdef PAS2JS}
    LTransfer := TJSDataTransfer.new;
    for LIndex := 0 to 5 do
    begin
      case LIndex of
        0: LName := 'dragstart';
        1: LName := 'dragenter';
        2: LName := 'dragover';
        3: LName := 'drop';
        4: LName := 'dragleave';
      else
        LName := 'dragend';
      end;
      LOptions := TJSObject.new;
      LOptions['bubbles'] := True;
      LOptions['cancelable'] := True;
      LOptions['dataTransfer'] := LTransfer;
      LEvent := TDragEvent.new(LName, LOptions);

      if LIndex in [0, 5] then
      begin
        LSource.dispatchEvent(LEvent);
      end
      else
      begin
        LTarget.dispatchEvent(LEvent);
      end;

      if LIndex = 0 then
      begin
        { Synthetic DragEvent/DataTransfer objects exercise the real DOM listener
          bridge, but do not grant the browser's physical drag-store authority.
          Physical operation negotiation is qualified by the CDP input journey. }
        Check(not LEvent.defaultPrevented and (LTransfer.getData('text/plain') = CText),
          'DOM dragstart writes an owned typed sequential offer');
      end;
    end;
    {$else}
    TControlAccess(LSource).OnMouseDown(LSource, mbLeft, [ssLeft], 3, 4);
    Check(NyxLCLHasPointerCapture(LSource) and (LProbe.Counts[ntPointerCapture] = 1),
      'actual native pointer request acquires and reports capture');
    TControlAccess(LSource).OnMouseUp(LSource, mbLeft, [], 3, 4);
    Check(not NyxLCLHasPointerCapture(LSource) and (LProbe.Counts[ntPointerCaptureLost] = 1),
      'actual native release reports genuine loss');
    Check(not LProbe.Retained.CanRequest(ngcReleasePointer),
      'retained physical callback has no late authority');
    { These are real LCL callback slots on mounted controls. The separate
      BeginDrag journey below exercises actual drag-manager construction. }
    LDrag := nil;
    TControlAccess(LSource).OnStartDrag(LSource, LDrag);
    try
      Check((LDrag <> nil) and (LProbe.Counts[ntDragStart] = 1),
        'actual native dragstart creates an owned typed offer');
      LAccept := False;
      TControlAccess(LTarget).OnDragOver(LTarget, LDrag, 10, 12, dsDragEnter, LAccept);
      Check(LAccept, 'native target explicitly accepts a source-allowed operation');
      TControlAccess(LTarget).OnDragOver(LTarget, LDrag, 13, 15, dsDragMove, LAccept);
      TControlAccess(LTarget).OnDragDrop(LTarget, LDrag, 13, 15);
      TControlAccess(LSource).OnEndDrag(LSource, LTarget, 13, 15);
    finally
      LDrag.Free;
    end;
    {$endif}
    Check((LProbe.Counts[ntDrop] = 1) and LProbe.Last[ntDrop].HasDrag and
      (LProbe.Last[ntDrop].Drag.Transfer.TextFor(NyxTextTransferFormat) = CText),
      'drop callback receives owned readable exact Unicode data');
    LDragTransfer := LProbe.Last[ntDragOver].Drag.Transfer;
    Check(not LDragTransfer.Readable and LDragTransfer.HasFormat(NyxValueTransferFormat),
      'actual adapter hover data remains protected');
    Check(LProbe.Last[ntDragEnd].Drag.Operation =
      {$ifdef PAS2JS}ndoNone{$else}ndoCopy{$endif},
      'source observes the actual host final operation');
    {$ifdef NYX_COMPILED_GESTURES}
    Check((GestureCalls >= 2) and GestureLast.HasDrag and
      (GestureLast.Drag.Transfer.TextFor(NyxTextTransferFormat) = CText),
      'compiled handwritten companion retains the owned drop context');
    {$endif}
    {$ifdef PAS2JS}
    LProbe.Navigate := ntDragStart;
    LTransfer := TJSDataTransfer.new;
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := True;
    LOptions['dataTransfer'] := LTransfer;
    LEvent := TDragEvent.new('dragstart', LOptions);
    LSource.dispatchEvent(LEvent);
    Check((LRenderer.Root = nil) and LEvent.defaultPrevented and
      (LTransfer.getData('text/plain') = '') and
      not LProbe.Retained.CanRequest(ngcOfferDrag),
      'browser drag navigation cancels the host offer and seals retained authority');
    { Holding an old DOM element does not hold its Nyx bindings alive. Dispatch
      another event there after unmounting; no registration may run again. }
    LEvent := TDragEvent.new('dragstart', LOptions);
    LSource.dispatchEvent(LEvent);
    Check((LProbe.Counts[ntDragStart] = 2) and
      (LProbe.Last[ntDrop].Drag.Transfer.TextFor(NyxTextTransferFormat) = CText),
      'disposed browser bindings stop callbacks while retained drop data survives');
    {$endif}
    {$ifndef PAS2JS}
    LProbe.Offer := False;
    LSource.BeginDrag(True);
    Application.Idle(False);
    Check(not LSource.Dragging and (LRenderer.LastGestureError <> ''),
      'refused real native drag construction is safely canceled at idle');
    LProbe.Offer := True;
    LProbe.Navigate := ntDragStart;
    LSource.BeginDrag(True);
    Check(LRenderer.Root = nil, 'drag-start navigation retires the mounted view');
    { The caller may close its host immediately after callback navigation. The
      pending drag constructor must finish under the frame's independent hidden
      host, and retirement must subsequently release both controls and capture. }
    LHost.Free;
    LHost := TForm.CreateNew(nil);
    LHost.SetBounds(0, 0, 640, 480);
    LHost.HandleNeeded;
    Application.Idle(False);
    Mount;
    LProbe.Navigate := ntPointerCapture;
    TControlAccess(LSource).OnMouseDown(LSource, mbLeft, [ssLeft], 2, 2);
    Check(LRenderer.Root = nil, 'capture notification may unmount the native view');
    Application.Idle(False);
    Mount;
    LProbe.Navigate := ntDesignSelect;
    LMessage := Default(TLMessage);
    LMessage.Msg := LM_LBUTTONDOWN;
    { Deliver through the physical frame, then interrupt the real mouse stream. }
    LSource.WindowProc(LMessage);
    LMessage.Msg := LM_CANCELMODE;
    LSource.WindowProc(LMessage);
    Check(LProbe.Counts[ntPointerCancel] = 1, 'native cancel mode is distinct from capture loss');
    {$endif}
  finally
    LRenderer.Free;
    {$ifndef PAS2JS}
    Application.Idle(False);
    LHost.Free;
    {$else}
    LHost.remove;
    {$endif}
    LDocument.Free;
    LSubscriptions := nil;
    LProbeOwner := nil;
  end;
end;

{$ifndef PAS2JS}
procedure ExportFixture;
var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LStream: TFileStream;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDocument := Fixture;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.gestures.fixture');
    { Keep authored callbacks beside a specialized generated builder. This
      observer retains typed context without owning widgets or overriding the
      test application's independent transfer negotiation. }
    LSource := StringReplace(LSource, 'implementation' + #10,
      'function GestureCalls: Integer;' + #10 +
      'function GestureLast: TNyxEventInfo;' + #10 + #10 + 'implementation' + #10 +
      #10 + 'type' + #10 + '  TGestureAction = class(TNyxEventCallback)' + #10 +
      '    procedure Invoke(const AEvent: TNyxEventInfo;' + #10 +
      '      const AExecution: INyxExecution); override;' + #10 + '  end;' + #10 +
      #10 + 'var' + #10 + '  GCalls: Integer;' + #10 +
      '  GLast: TNyxEventInfo;' + #10 + #10 +
      '{ Retain only the owned public drag snapshot. }' + #10 +
      'procedure TGestureAction.Invoke(const AEvent: TNyxEventInfo;' + #10 +
      '  const AExecution: INyxExecution);' + #10 + 'begin' + #10 +
      '  Inc(GCalls);' + #10 + '  GLast := AEvent.Copy;' + #10 + 'end;' + #10 +
      #10 + 'function GestureCalls: Integer;' + #10 + 'begin' + #10 +
      '  Result := GCalls;' + #10 + 'end;' + #10 + #10 +
      'function GestureLast: TNyxEventInfo;' + #10 + 'begin' + #10 +
      '  Result := GLast.Copy;' + #10 + 'end;' + #10, []);
    LSource := StringReplace(LSource, #10 + 'end.' + #10, #10 + 'initialization' + #10 +
      '  RegisterNyxCallback(NyxHandler(''TGestureAction''), TGestureAction);' + #10 +
      #10 + 'end.' + #10, []);
    LStream := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LStream.WriteBuffer(LSource[1], Length(LSource));
    finally
      LStream.Free;
    end;
  finally
    LDocument.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GFailure := TNativeFailure.Create;
    Application.OnException := GFailure.Failed;
    {$endif}
    ContractChecks;
    ResponseChecks;
    AuthoringChecks;
    ControlChecks;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' gesture checks';
    document.body.setAttribute('data-gesture-tests', 'passed');
    {$else}
    ExportFixture;
    WriteLn('PASS ', GChecks, ' gesture checks');
    Application.OnException := nil;
    FreeAndNil(GFailure);
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-gesture-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      Application.OnException := nil;
      FreeAndNil(GFailure);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
