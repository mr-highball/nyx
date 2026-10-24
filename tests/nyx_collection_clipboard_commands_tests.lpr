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
program nyx_collection_clipboard_commands_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.state, nyx.collections,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.selection,
  nyx.collections.query, nyx.collections.arrangement, nyx.collections.mount,
  nyx.collections.clipboard, nyx.collections.clipboard.actions,
  nyx.collections.clipboard.commands, nyx.collections.representation,
  nyx.collections.draft, nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.range.commands,
  nyx.formatting, nyx.contract, nyx.resources, nyx.clipboard, nyx.scheduler, nyx.events
  {$ifdef PAS2JS}, Web{$endif};

type
  { Injected host cleanup may synchronously admit a newer canonical command;
    no event loop, real clipboard or timing assumption is involved. }
  TEndingCallback = procedure of object;

  { A foreign immutable-intent facade clears the real accepted selection from
    its readiness getter. The command must keep its original reserved input
    and reject the returned stale scope rather than recapturing a fresh range. }
  TRangeIntentProxy = class(TInterfacedObject, INyxCollectionCellRangeIntent)
  public
    Inner: INyxCollectionCellRangeIntent;
    ClearOnRead: INyxCollectionCellRangeSelection;
    function GetCapture: INyxCollectionCellRangeCapture;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetReadiness: INyxCancellationReadiness;
    function GetCurrent: Boolean;
  end;

  { Explicit async receiver ownership is independent of the fixture/controller.
    Counts prove that captured-write arbitration releases the old receiver and
    terminal inline failure cannot replay it. }
  TCapturedReply = class(TInterfacedObject)
  public
    Throwing: Boolean;
    destructor Destroy; override;
    procedure Receive(const AResult: TNyxClipboardWriteResult);
  end;
  { Controlled canonical input facts exercise epoch ABA and flags-only final
    readiness without a widget or an OS clipboard authority. }
  TInput = class(TInterfacedObject, INyxCollectionInputLifetime)
  public
    CurrentValue: Boolean;
    ReadyValue: Boolean;
    EpochValue: Integer;
    Mutation: Integer;
    function GetCurrent: Boolean;
    function GetReady: Boolean;
    function GetRetired: Boolean;
    function GetEpoch: Integer;
  end;

  { A synchronous mounted-fence analogue retains its exact input owner and one
    captured mutation. It performs no source, callback or native/DOM reads. }
  TFence = class(TInterfacedObject, INyxCancellationReadiness)
  private
    FInput: TInput;
    FLease: INyxCollectionInputLifetime;
    FMutation: Integer;
  public
    constructor Create(AInput: TInput);
    procedure CheckReady;
  end;

  { Forward authority is borrowed only while the fixture retains its host. }
  THost = class;

  { Real shared request supplies terminal/cancellation semantics. Its effect is
    a counter and copied text only; no system clipboard is constructed. }
  TWrite = class(TNyxClipboardWriteRequest)
  private
    FOwner: THost;
  protected
    procedure EndHost; override;
  public
    procedure Start(AOwner: THost);
    procedure Finish;
  end;

  { Explicit host supports inline and pending completion with retained exact
    requests. End counts prove supersession/cancellation retires only its owner. }
  THost = class(TInterfacedObject, INyxClipboardWriteHost)
  public
    Writes: Integer;
    Endings: Integer;
    Text: TNyxText;
    InlineCompletion: Boolean;
    ThrowEnd: Boolean;
    OnEnding: TEndingCallback;
    Last: TWrite;
    LastLease: INyxClipboardWriteRequest;
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

  { Snapshot getter reentry is an ordinary alternate-source callout, not a
    mutation of the target's callback-free final readiness contract. }
  TRangeSourceAction = (rsaNone, rsaClear, rsaNested);

  { Transparent source delegates data/subscriptions to the actual store. The
    optional borrowed callback runs once before returning its current snapshot. }
  TRangeSource = class(TInterfacedObject, INyxCollection)
  public
    Inner: INyxCollection;
    OnSnapshot: TEndingCallback;
    function Snapshot: INyxCollectionSnapshot;
    function Append(const AItem: TNyxCollectionItem): INyxCollection;
    function Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
    function Remove(const ARef: TNyxItemRef): INyxCollection;
    function Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
    function Update(const AItem: TNyxCollectionItem): INyxCollection;
    function Replace(const AItem: TNyxCollectionItem): INyxCollection;
    procedure Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer = -1);
    procedure Assign(const ASource: INyxCollectionSnapshot; AExpectedRevision: Integer = -1);
    function Clone: INyxCollection;
    function Subscribe(AObserver: TNyxCollectionObserver;
      AValidator: TNyxCollectionValidator = nil): INyxCollectionSubscription;
  end;

  { Real shared mount exercises staged/ancestor/disabled/refresh/suspension
    admission independently of target widgets. Render uses the public command
    surface to attempt reentry while its actual refresh shield is held. }
  TTestMount = class(TNyxCollectionMountBase)
  public
    ProbeOnRender: Boolean;
    ProbeRefused: Boolean;
    RangeProbeRefused: Boolean;
    SourceAction: TRangeSourceAction;
    NestedRangeAdmitted: Boolean;
    Item: TNyxItemRef;
    Field: TNyxColumnIdentity;
    procedure SuspendInput(AValue: Boolean);
    procedure ReadSource;
  protected
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  end;

  { Fixture keeps every borrowed receiver alive through explicit cancellation.
    Only the injected host substitutes authority; the view/store/router and
    immutable producer are the actual library implementations. }
  TFixture = class
  public
    Key: TNyxCollectionRef;
    ItemA: TNyxItemRef;
    ItemB: TNyxItemRef;
    Field: TNyxColumnIdentity;
    Store: INyxCollection;
    View: INyxCollectionView;
    Input: TInput;
    InputLease: INyxCollectionInputLifetime;
    Host: THost;
    HostLease: INyxClipboardWriteHost;
    Scheduler: INyxScheduler;
    Events: INyxEvents;
    Commands: INyxCollectionClipboardCommands;
    Replies: Integer;
    LastPayload: INyxCollectionClipboardText;
    ThrowReply: Boolean;
    RevokeReply: Boolean;
    RepresentationReplies: Integer;
    RepresentationValue: INyxCollectionCellText;
    ThrowRepresentation: Boolean;
    constructor Create(ADisplay: Boolean = False);
    destructor Destroy; override;
    function Readiness: INyxCancellationReadiness;
    procedure Reply(const APayload: INyxCollectionClipboardText;
      const AResult: TNyxClipboardWriteResult);
    function Cell: INyxCollectionClipboardCommand;
    procedure Filter;
    procedure Restrict(const AActions: TNyxCollectionClipboardActions);
    procedure Represented(const AValue: INyxCollectionCellText;
      const AResult: TNyxClipboardWriteResult);
    procedure NestedEnding;
  end;

  { Foreign representation getters can synchronously run an application action.
    This proxy changes only the Text call; the controller must normalize copied
    facts then refuse its old admission if that call admitted a newer command. }
  TReentrantValue = class(TInterfacedObject, INyxCollectionCellText)
  public
    Value: INyxCollectionCellText;
    OnText: TEndingCallback;
    function GetRepresentation: TNyxCollectionCellRepresentation;
    function GetText: TNyxText;
    function GetRawText: TNyxText;
    function GetByteCount: Integer;
    function GetItem: TNyxItemRef;
    function GetField: TNyxColumnIdentity;
    function GetRevision: Integer;
    function GetValue: TNyxStateValue;
    function GetHasFormatter: Boolean;
    function GetFormatter: TNyxValueFormat;
    function GetDomain: TNyxValueDomain;
  end;

const
  CText: TNyxText = 'raw 🌙' + #0 + 'line' + #13#10 + 'next';

var
  GChecks: Integer;
  GDetached: Integer;
  GCapturedReplies: Integer;
  GCapturedDestroyed: Integer;

function TReentrantValue.GetRepresentation: TNyxCollectionCellRepresentation;
begin
  Result := Value.Representation;
end;

function TReentrantValue.GetText: TNyxText;
var
  LCall: TEndingCallback;
begin
  Result := Value.Text;
  LCall := OnText;
  OnText := nil;

  if Assigned(LCall) then
  begin
    LCall;
  end;
end;

function TReentrantValue.GetRawText: TNyxText;
begin
  Result := Value.RawText;
end;

function TReentrantValue.GetByteCount: Integer;
begin
  Result := Value.ByteCount;
end;

function TReentrantValue.GetItem: TNyxItemRef;
begin
  Result := Value.Item;
end;

function TReentrantValue.GetField: TNyxColumnIdentity;
begin
  Result := Value.Field;
end;

function TReentrantValue.GetRevision: Integer;
begin
  Result := Value.Revision;
end;

function TReentrantValue.GetValue: TNyxStateValue;
begin
  Result := Value.Value;
end;

function TReentrantValue.GetHasFormatter: Boolean;
begin
  Result := Value.HasFormatter;
end;

function TReentrantValue.GetFormatter: TNyxValueFormat;
begin
  Result := Value.Formatter;
end;

function TReentrantValue.GetDomain: TNyxValueDomain;
begin
  Result := Value.Domain;
end;

function TRangeIntentProxy.GetCapture: INyxCollectionCellRangeCapture;
begin
  Result := Inner.Capture;
end;

function TRangeIntentProxy.GetSource: INyxCollectionSnapshot;
begin
  Result := Inner.Source;
end;

function TRangeIntentProxy.GetPage: INyxCollectionSnapshot;
begin
  Result := Inner.Page;
end;

function TRangeIntentProxy.GetReadiness: INyxCancellationReadiness;
var
  LInner: INyxCollectionCellRangeIntent;
  LSelection: INyxCollectionCellRangeSelection;
begin
  LInner := Inner;
  LSelection := ClearOnRead;
  ClearOnRead := nil;

  if LSelection <> nil then
  begin
    LSelection.Clear;
  end;
  Result := LInner.Readiness;
end;

function TRangeIntentProxy.GetCurrent: Boolean;
begin
  Result := Inner.Current;
end;

destructor TCapturedReply.Destroy;
begin
  Inc(GCapturedDestroyed);
  inherited Destroy;
end;

procedure TCapturedReply.Receive(const AResult: TNyxClipboardWriteResult);
begin
  Inc(GCapturedReplies);

  if Throwing then
  begin
    raise Exception.Create('captured inline receiver failure');
  end;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function TInput.GetCurrent: Boolean;
begin
  Result := CurrentValue;
end;

function TInput.GetReady: Boolean;
begin
  Result := CurrentValue and ReadyValue;
end;

function TInput.GetRetired: Boolean;
begin
  Result := not CurrentValue;
end;

function TInput.GetEpoch: Integer;
begin
  Result := EpochValue;
end;

constructor TFence.Create(AInput: TInput);
begin
  inherited Create;
  FInput := AInput;
  FLease := AInput;
  FMutation := AInput.Mutation;
end;

procedure TFence.CheckReady;
begin

  if not FInput.CurrentValue or not FInput.ReadyValue or
    (FInput.Mutation <> FMutation) then
  begin
    raise ENyxCollectionClipboard.Create('Original mount proof changed');
  end;
end;

procedure TWrite.Start(AOwner: THost);
begin
  FOwner := AOwner;
  RetainPending;
end;

procedure TWrite.EndHost;
var
  LOwner: THost;
begin
  LOwner := FOwner;
  FOwner := nil;

  if LOwner <> nil then
  begin
    Inc(LOwner.Endings);

    if Assigned(LOwner.OnEnding) then
    begin
      LOwner.OnEnding;
    end;

    if LOwner.ThrowEnd then
    begin
      raise Exception.Create('');
    end;
  end;
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
  LastLease := Result;
  Last := LWrite;
  LWrite.Start(Self);
  Inc(Writes);
  Text := AText;

  if InlineCompletion then
  begin
    LWrite.Finish;
  end;
end;

constructor TFixture.Create(ADisplay: Boolean);
var
  LSpec: TNyxCollectionViewSpec;
begin
  inherited Create;
  Key := NyxCollection('command-source');
  ItemA := NyxItem(Key, 'a 🌙');
  ItemB := NyxItem(Key, 'b');
  Field := NyxColumnIdentity(NyxTextField('Name'));
  Store := NewNyxCollection(Key, NyxCollectionSchema.Text(NyxTextField('Name'), '')
    .Integer(NyxIntegerField('Score'), 0), [
      NyxCollectionItem(ItemA).WithValue(NyxTextField('Name'), CText)
        .WithValue(NyxIntegerField('Score'), 1),
      NyxCollectionItem(ItemB).WithValue(NyxTextField('Name'), 'B')
        .WithValue(NyxIntegerField('Score'), 2)]);
  LSpec := NyxCollectionView(Key).Selection(nsmMultiple)
    .Column(NyxTextField('Name'), 'Name')
    .Column(NyxIntegerField('Score'), 'Score').TableLayout(NyxTableLayout);

  if ADisplay then
  begin
    LSpec := LSpec.ColumnFormat(NyxValueFormat.Decimal(2).Localized(
      NyxFormatLocale.Named(NyxLocale('de-DE')).NumberSeparators(',', '.')));
  end;
  View := NewNyxCollectionView(Store, LSpec, cpTable);
  View.SetSelection([ItemA, ItemB], ItemA, ItemA);
  Input := TInput.Create;
  InputLease := Input;
  Input.CurrentValue := True;
  Input.ReadyValue := True;
  Input.EpochValue := 1;
  Input.Mutation := 1;
  Host := THost.Create;
  HostLease := Host;
  Host.InlineCompletion := True;
  Scheduler := NewNyxScheduler;
  Events := NewNyxEvents(Scheduler);
  Commands := NewNyxCollectionClipboardCommands(View, InputLease, Events,
    HostLease, Scheduler, TNyxCollectionClipboardOptions.Defaults,
    {$ifdef PAS2JS}@{$endif}Readiness);
  Commands.Observe({$ifdef PAS2JS}@{$endif}Reply);
end;

destructor TFixture.Destroy;
begin

  if Commands <> nil then
  begin
    Commands.Revoke;
  end;
  Commands := nil;

  if (Host <> nil) and (Host.LastLease <> nil) then
  begin
    Host.LastLease.Cancel;
  end;

  if Host <> nil then
  begin
    Host.LastLease := nil;
    Host.Last := nil;
  end;
  HostLease := nil;

  if Events <> nil then
  begin
    Events.Close;
  end;
  Events := nil;

  if Scheduler <> nil then
  begin
    Scheduler.Shutdown;
  end;
  Scheduler := nil;
  View := nil;
  Store := nil;
  InputLease := nil;
  inherited Destroy;
end;

function TFixture.Readiness: INyxCancellationReadiness;
begin
  Result := TFence.Create(Input);
end;

procedure TFixture.Reply(const APayload: INyxCollectionClipboardText;
  const AResult: TNyxClipboardWriteResult);
begin
  Inc(Replies);
  LastPayload := APayload;
  Check((AResult.Outcome = ncwoWritten) and
    (AResult.ByteCount = APayload.ByteCount), 'observer receives original exact payload/result');

  if RevokeReply then
  begin
    Commands.Revoke;
  end;

  if ThrowReply then
  begin
    raise Exception.Create('intentional inline observer failure');
  end;
end;

function TFixture.Cell: INyxCollectionClipboardCommand;
begin
  Result := Commands.BeginCell(ItemA, Field);
  Result.Prepare;
end;

procedure TFixture.Filter;
begin
  { Bound the fluent receiver temporary to this call, not the lifetime test. }
  View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(2)));
end;

procedure TFixture.Restrict(const AActions: TNyxCollectionClipboardActions);
begin
  Commands.Revoke;
  Commands := NewNyxCollectionClipboardCommands(View, InputLease, Events,
    HostLease, Scheduler, TNyxCollectionClipboardOptions.Defaults.AllowOnly(AActions),
    {$ifdef PAS2JS}@{$endif}Readiness);
  Commands.Observe({$ifdef PAS2JS}@{$endif}Reply);
end;

procedure TFixture.Represented(const AValue: INyxCollectionCellText;
  const AResult: TNyxClipboardWriteResult);
begin
  Inc(RepresentationReplies);
  RepresentationValue := AValue;
  Check((AResult.Outcome = ncwoWritten) and (AResult.ByteCount = AValue.ByteCount),
    'represented observer receives actual outbound bytes and accepted provenance');

  if ThrowRepresentation then
  begin
    raise Exception.Create('represented inline observer failure');
  end;
end;

procedure TFixture.NestedEnding;
var
  LCommand: INyxCollectionClipboardCommand;
begin
  Host.OnEnding := nil;
  LCommand := Cell;
  LCommand.Complete(True);
end;

function TRangeSource.Snapshot: INyxCollectionSnapshot;
var
  LLease: INyxCollection;
  LCall: TEndingCallback;
begin
  LLease := Self;
  LCall := OnSnapshot;
  OnSnapshot := nil;

  if Assigned(LCall) then
  begin
    LCall;
  end;
  Result := Inner.Snapshot;
end;

function TRangeSource.Append(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := Inner.Append(AItem);
end;

function TRangeSource.Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := Inner.Insert(AIndex, AItem);
end;

function TRangeSource.Remove(const ARef: TNyxItemRef): INyxCollection;
begin
  Result := Inner.Remove(ARef);
end;

function TRangeSource.Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
begin
  Result := Inner.Move(ARef, AIndex);
end;

function TRangeSource.Update(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := Inner.Update(AItem);
end;

function TRangeSource.Replace(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := Inner.Replace(AItem);
end;

procedure TRangeSource.Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer);
begin
  Inner.Apply(AEdits, AExpectedRevision);
end;

procedure TRangeSource.Assign(const ASource: INyxCollectionSnapshot; AExpectedRevision: Integer);
begin
  Inner.Assign(ASource, AExpectedRevision);
end;

function TRangeSource.Clone: INyxCollection;
begin
  Result := Inner.Clone;
end;

function TRangeSource.Subscribe(AObserver: TNyxCollectionObserver;
  AValidator: TNyxCollectionValidator): INyxCollectionSubscription;
begin
  Result := Inner.Subscribe(AObserver, AValidator);
end;

procedure TTestMount.ReadSource;
var
  LAction: TRangeSourceAction;
begin
  LAction := SourceAction;
  SourceAction := rsaNone;
  case LAction of
    rsaNone:
      begin
        { An ordinary read has no application reentry. }
      end;
    rsaClear:
      begin
        ClearCellRange;
      end;
    rsaNested:
      begin
        NestedRangeAdmitted := SelectCells(NyxCollectionCellIdentity(Item, Field),
          NyxCollectionCellIdentity(Item, Field));
      end;
  end;
end;

procedure TTestMount.SuspendInput(AValue: Boolean);
begin
  SetCellEditsSuspended(AValue);
end;

procedure TTestMount.RenderDataset;
var
  LCommands: INyxCollectionClipboardCommands;
  LCommand: INyxCollectionClipboardCommand;
begin

  if not ProbeOnRender then
  begin
    Exit;
  end;
  LCommands := GetClipboardCommands;

  if LCommands = nil then
  begin
    Exit;
  end;
  LCommand := LCommands.BeginCell(Item, Field);
  LCommand.Prepare;
  ProbeRefused := not LCommand.Current and (LCommand.Payload = nil);
  LCommand.Retire;
  RangeProbeRefused := not SelectCells(NyxCollectionCellIdentity(Item, Field),
    NyxCollectionCellIdentity(Item, Field));
end;

procedure TTestMount.DetachTarget;
begin
  Inc(GDetached);
end;

procedure CapturedArbitration;
var
  LFixture: TFixture;
  LWriter: INyxCollectionClipboardCapturedWrite;
  LPrepared: INyxCollectionClipboardPreparedScope;
  LCommand: INyxCollectionClipboardCommand;
  LPayload: INyxCollectionClipboardText;
  LReady: INyxCancellationReadiness;
  LScope: INyxCancellationScope;
  LTicket: INyxClipboardWriteRequest;
  LOld: INyxClipboardWriteRequest;
  LOldObject: TWrite;
  LProbe: TCapturedReply;
  LProbeLease: IInterface;
  LBefore: Integer;
  LDestroyed: Integer;
  LRefused: Boolean;
begin
  LFixture := TFixture.Create;
  try
    Check(Supports(LFixture.Commands, INyxCollectionClipboardCapturedWrite, LWriter),
      'mounted controller exposes the same-slot detached snapshot write port');
    LCommand := LFixture.Cell;
    LPayload := LCommand.Payload;
    Check(Supports(LCommand, INyxCollectionClipboardPreparedScope, LPrepared),
      'prepared command exports owner-free original snapshot readiness');
    LReady := LPrepared.Readiness;
    Check(Supports(LReady, INyxCancellationScope, LScope),
      'the exact certified original scope supplies host cancellation too');
    LCommand.Retire;
    LFixture.Host.InlineCompletion := False;
    LCommand := LFixture.Cell;
    LCommand.Complete(True);
    LOld := LFixture.Host.LastLease;
    LOldObject := LFixture.Host.Last;
    LBefore := LFixture.Host.Endings;
    LProbe := TCapturedReply.Create;
    LProbeLease := LProbe;
    LDestroyed := GCapturedDestroyed;
    LTicket := LWriter.WriteCaptured(LPayload, LScope, LReady,
      {$ifdef PAS2JS}@{$endif}LProbe.Receive, LProbeLease);
    LProbeLease := nil;
    Check((LFixture.Host.Endings = LBefore + 1) and
      (GCapturedDestroyed = LDestroyed) and (LFixture.Commands.Phase = nccpPending),
      'accepted snapshot write supersedes old keyboard receiver and leases its own reply owner');
    LBefore := LFixture.Replies;
    LOldObject.Finish;
    Check(LFixture.Replies = LBefore, 'displaced physical-copy completion cannot reply into snapshot command');
    LOld := nil;
    LCommand := LFixture.Cell;
    LCommand.Complete(True);
    Check(GCapturedDestroyed = LDestroyed + 1,
      'new keyboard copy cancels the exact snapshot receiver and releases its owner');
    LBefore := LFixture.Host.Endings;
    LTicket.Cancel;
    Check((LFixture.Host.Endings = LBefore) and (LFixture.Commands.Phase = nccpPending),
      'old snapshot cancellation ticket cannot cancel a newer keyboard request');
    LFixture.Host.Last.Finish;
    LProbe := TCapturedReply.Create;
    LProbeLease := LProbe;
    LDestroyed := GCapturedDestroyed;
    LBefore := GCapturedReplies;
    LTicket := LWriter.WriteCaptured(LPayload, LScope, LReady,
      {$ifdef PAS2JS}@{$endif}LProbe.Receive, LProbeLease);
    LProbeLease := nil;
    LFixture.Host.Last.Finish;
    Check((GCapturedReplies = LBefore + 1) and (GCapturedDestroyed = LDestroyed + 1) and
      (LFixture.Commands.Phase = nccpCompleted),
      'captured value completes once through ordinary and leased per-call receipts');
    LFixture.Host.InlineCompletion := True;
    LProbe := TCapturedReply.Create;
    LProbeLease := LProbe;
    LProbe.Throwing := True;
    LBefore := GCapturedReplies;
    LRefused := False;
    try
      LTicket := LWriter.WriteCaptured(LPayload, LScope, LReady,
        {$ifdef PAS2JS}@{$endif}LProbe.Receive, LProbeLease);
    except
      on E: Exception do
      begin
        LRefused := E.Message = 'captured inline receiver failure';
      end;
    end;
    Check(LRefused and (GCapturedReplies = LBefore + 1) and
      (LFixture.Commands.Phase = nccpCompleted),
      'captured inline receiver exception propagates once after terminal completion');
  finally
    LTicket := nil;
    LOld := nil;
    LProbeLease := nil;
    LCommand := nil;
    LPrepared := nil;
    LWriter := nil;
    LFixture.Free;
  end;
end;

{ The optional represented port is exercised beside real RAW commands, not a
  second controller. Its immutable formatter is captured by ordinary Prepare;
  only an explicit action performs formatting and submits different bytes. }
procedure RepresentedArbitration;
var
  LFixture: TFixture;
  LWriter: INyxCollectionClipboardRepresentationWrite;
  LCapture: INyxCollectionClipboardDisplayCapture;
  LPrepared: INyxCollectionClipboardPreparedScope;
  LCommand: INyxCollectionClipboardCommand;
  LReady: INyxCancellationReadiness;
  LScope: INyxCancellationScope;
  LRaw: INyxCollectionClipboardText;
  LValue: INyxCollectionCellText;
  LPolicy: TNyxCollectionCellDisplayPolicy;
  LTicket: INyxClipboardWriteRequest;
  LOld: INyxClipboardWriteRequest;
  LOldObject: TWrite;
  LBefore: Integer;
  LReplies: Integer;
  LRefused: Boolean;
  LProxy: TReentrantValue;
  LProxyLease: INyxCollectionCellText;
  LDraft: INyxCollectionCellText;
begin
  LFixture := TFixture.Create(True);
  try
    Check(Supports(LFixture.Commands, INyxCollectionClipboardRepresentationWrite, LWriter),
      'same controller exposes optional explicit represented writes');
    LWriter.ObserveRepresentation({$ifdef PAS2JS}@{$endif}LFixture.Represented);
    LCommand := LFixture.Commands.BeginCell(LFixture.ItemA,
      NyxColumnIdentity(NyxIntegerField('Score')));
    LCommand.Prepare;
    Check(LCommand.Current and Supports(LCommand, INyxCollectionClipboardDisplayCapture, LCapture) and
      Supports(LCommand, INyxCollectionClipboardPreparedScope, LPrepared),
      'ordinary RAW prepare captures optional original display metadata and readiness');
    LRaw := LCommand.Payload;
    LPolicy := LCapture.DisplayPolicy;
    Check(LPolicy.Defined and LPolicy.Declared and LPolicy.Field.Same(LRaw.FieldAt(0)) and
      (LRaw.Text = '1'), 'captured display policy never relabels the accepted RAW payload');
    LValue := NewNyxCollectionCellDisplay(LRaw, LPolicy.Formatter, LPolicy.Domain);
    LValue := CopyNyxCollectionCellText(LValue);
    Check((LValue.Text = '1,00') and (LValue.RawText = '1') and
      (LValue.Value.IntegerValue = 1) and (LValue.ByteCount > LRaw.ByteCount) and
      (LValue.Formatter.Locale.Locale.Name = 'de-DE'),
      'normalized display retains original locale, typed scalar, provenance and actual different bytes');
    LReady := LPrepared.Readiness;
    Check(Supports(LReady, INyxCancellationScope, LScope),
      'captured represented request uses the original certified cancellation scope');
    LCommand.Retire;
    LFixture.Host.InlineCompletion := False;
    LCommand := LFixture.Cell;
    LCommand.Complete(True);
    LOld := LFixture.Host.LastLease;
    LOldObject := LFixture.Host.Last;
    LBefore := LFixture.Host.Endings;
    LReplies := LFixture.Replies;
    LTicket := LWriter.WriteRepresented(LValue, LScope, LReady, nil, nil);
    Check((LFixture.Host.Endings = LBefore + 1) and (LFixture.Host.Text = '1,00') and
      (LFixture.Commands.Phase = nccpCancelled) and
      (LWriter.RepresentationPhase = nccpPending),
      'explicit display supersedes pending RAW through the same physical request slot');
    LOldObject.Finish;
    Check((LFixture.Replies = LReplies) and (LFixture.RepresentationReplies = 0),
      'late displaced RAW completion cannot reply into represented output');
    LOld := nil;
    LFixture.Host.Last.Finish;
    Check((LFixture.RepresentationReplies = 1) and (LFixture.Replies = LReplies) and
      (LWriter.LastRepresentation.Text = '1,00') and
      (LWriter.LastRepresentationResult.ByteCount = 4),
      'represented terminal receipt stays separate from RAW observer and byte accounting');
    LTicket := LWriter.WriteRepresented(LValue, LScope, LReady, nil, nil);
    LBefore := LFixture.Host.Endings;
    LCommand := LFixture.Cell;
    LCommand.Complete(True);
    Check((LFixture.Host.Endings = LBefore + 1) and (LFixture.Host.Text = CText) and
      (LWriter.RepresentationPhase = nccpCancelled) and
      (LFixture.Commands.Phase = nccpPending),
      'fresh canonical RAW copy supersedes represented output in the same slot');
    LBefore := LFixture.Host.Endings;
    LTicket.Cancel;
    Check((LFixture.Host.Endings = LBefore) and (LFixture.Commands.Phase = nccpPending),
      'stale represented cancellation ticket cannot cancel the newer RAW owner');
    LFixture.Host.OnEnding := {$ifdef PAS2JS}@{$endif}LFixture.NestedEnding;
    LBefore := LFixture.Host.Writes;
    LTicket := LWriter.WriteRepresented(LValue, LScope, LReady, nil, nil);
    Check((LTicket = nil) and (LFixture.Host.Writes = LBefore + 1) and
      (LFixture.Host.Text = CText) and (LFixture.Commands.Phase = nccpPending),
      'nested newer canonical command during old-host cancellation wins before display host entry');
    LProxy := TReentrantValue.Create;
    LProxyLease := LProxy;
    LProxy.Value := LValue;
    LProxy.OnText := {$ifdef PAS2JS}@{$endif}LFixture.NestedEnding;
    LBefore := LFixture.Host.Writes;
    LTicket := LWriter.WriteRepresented(LProxyLease, LScope, LReady, nil, nil);
    Check((LTicket = nil) and (LFixture.Host.Writes = LBefore + 1) and
      (LFixture.Host.Text = CText) and (LFixture.Commands.Phase = nccpPending),
      'new canonical admission inside a foreign represented getter fences the old normalization before any effect');
    LProxyLease := nil;
    LFixture.Host.Last.Finish;
    LFixture.Host.InlineCompletion := True;
    LFixture.ThrowRepresentation := True;
    LBefore := LFixture.RepresentationReplies;
    LRefused := False;
    try
      LTicket := LWriter.WriteRepresented(LValue, LScope, LReady, nil, nil);
    except
      on E: Exception do
      begin
        LRefused := E.Message = 'represented inline observer failure';
      end;
    end;
    Check(LRefused and (LFixture.RepresentationReplies = LBefore + 1) and
      (LWriter.RepresentationPhase = nccpCompleted),
      'inline represented observer failure propagates once after terminal detachment');
    LFixture.ThrowRepresentation := False;
    LDraft := NewNyxCollectionCellDraft(LRaw, '-', LPolicy.Domain);
    LReplies := LFixture.Replies;
    LBefore := LFixture.RepresentationReplies;
    LTicket := LWriter.WriteRepresented(LDraft, LScope, LReady, nil, nil);
    Check((LFixture.Host.Text = '-') and (LFixture.RepresentationReplies = LBefore + 1) and
      (LFixture.Replies = LReplies) and
      (LWriter.LastRepresentation.Representation = nccrExplicitDraft) and
      (LWriter.LastRepresentationResult.ByteCount = 1) and
      (LWriter.LastRepresentation.Value.IntegerValue = 1),
      'explicit draft shares represented arbitration while accepted integer and RAW observer stay intact');
    LBefore := LFixture.Host.Writes;
    Inc(LFixture.Input.EpochValue);
    LRefused := False;
    try
      LTicket := LWriter.WriteRepresented(LValue, LScope, LReady, nil, nil);
    except
      on E: ENyxCollectionClipboard do LRefused := True;
    end;
    Check(LRefused and (LFixture.Host.Writes = LBefore),
      'disable-style epoch ABA cannot reuse an old represented readiness proof');
    LFixture.Commands.Revoke;
    LTicket := LWriter.WriteRepresented(LValue, LScope, LReady, nil, nil);
    Check((LTicket = nil) and (LFixture.Host.Writes = LBefore),
      'retained represented facade remains inert after actual controller retirement');
  finally
    LCommand := nil;
    LCapture := nil;
    LPrepared := nil;
    LWriter := nil;
    LTicket := nil;
    LOld := nil;
    LProxyLease := nil;
    LFixture.Free;
  end;
end;

{ Ranges share the actual existing controller/host slot with ordinary RAW and
  represented commands. The selected rectangle is independent of row selection;
  only immutable values and typed generation/readiness are supplied here. }
procedure RangeArbitration;
var
  LFixture: TFixture;
  LSelection: INyxCollectionCellRangeSelection;
  LIntent: INyxCollectionCellRangeIntent;
  LRangeInput: INyxCollectionClipboardRangeInput;
  LCommand: INyxCollectionClipboardRangeCommand;
  LRawCommand: INyxCollectionClipboardCommand;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LFields: TNyxCollectionClipboardFields;
  LRowSelection: TNyxCollectionSelectionSnapshot;
  LOld: INyxClipboardWriteRequest;
  LOldObject: TWrite;
  LProxy: TRangeIntentProxy;
  LProxyLease: INyxCollectionCellRangeIntent;
  LBefore: Integer;
  LReplies: Integer;
  LRefused: Boolean;
  LExpected: TNyxText;

  procedure SelectRange;
  begin
    LCapture := LColumns.CaptureColumns;
    LIntent := LSelection.Select(LFixture.Store.Snapshot, LFixture.View.Snapshot,
      LFields, NyxCollectionCellIdentity(LFixture.ItemB, LFields[1]),
      NyxCollectionCellIdentity(LFixture.ItemA, LFields[0]), LCapture.Serial,
      True, nccleLF, TNyxCollectionClipboardLimits.Defaults);
  end;

  procedure PrepareRange;
  begin
    LCommand := LRangeInput.BeginRangeInput;
    LCommand.ResolveRange(LIntent);
    LCommand.Prepare;
  end;

begin
  LFixture := TFixture.Create;
  LSelection := NewNyxCollectionCellRangeSelection;
  try
    LColumns := NyxCollectionColumns(LFixture.View);
    SetLength(LFields, 2);
    LFields[0] := LFixture.Field;
    LFields[1] := NyxColumnIdentity(NyxIntegerField('Score'));
    SelectRange;
    LRowSelection := LFixture.View.Selection.Snapshot;
    Check(Supports(LFixture.Commands, INyxCollectionClipboardRangeInput, LRangeInput),
      'ordinary controller exposes optional range reservation without changing its existing interface');
    PrepareRange;
    Check((LCommand.Payload = nil) and not LCommand.Current and LCommand.DispatchCurrent,
      'existing default permissions do not implicitly enable cell-range copying');
    LCommand.Complete(True);
    Check((LFixture.Host.Writes = 0) and LFixture.View.Selection.Snapshot.SameState(LRowSelection),
      'disallowed range still claims its canonical envelope without changing row selection or entering host');
    LFixture.Restrict([nccaCell, nccaSelectedSource, nccaSelectedPage, nccaCellRange]);
    Check(Supports(LFixture.Commands, INyxCollectionClipboardRangeInput, LRangeInput),
      'explicit range permission retains the same controller optional port');
    PrepareRange;
    LExpected := 'Name' + #9 + 'Score' + #10 + '"' + CText + '"' + #9 + '1' + #10 + 'B' + #9 + '2';
    Check(LCommand.Current and (LCommand.Payload.Text = LExpected) and
      (LCommand.Payload.RowCount = 2) and (LCommand.Payload.FieldCount = 2) and
      (LCommand.Payload.ItemAt(0).ID = LFixture.ItemA.ID) and
      (LCommand.Payload.CellValue(0, 0).TextValue = CText),
      'prepared range reuses bounded exact RAW TSV including Unicode NUL and embedded newlines');
    LFixture.View.SetSelection([LFixture.ItemB], LFixture.ItemB, LFixture.ItemB);
    Check(LCommand.Current, 'ordinary row-selection changes do not redefine an explicit cell rectangle');
    LCommand.Complete(True);
    Check((LFixture.Host.Writes = 1) and (LFixture.Host.Text = LExpected) and
      (LFixture.LastPayload.RowCount = 2),
      'accepted range transfers once through the existing RAW observer and physical slot');
    PrepareRange;
    LBefore := LFixture.Host.Writes;
    LCommand.Complete(False);
    Check((LFixture.Host.Writes = LBefore) and LIntent.Current,
      'consumed canonical range default leaves accepted selection and clipboard authority untouched');
    PrepareRange;
    LSelection.Clear;
    Check(not LCommand.Current and LCommand.DispatchCurrent,
      'selection retirement refuses the old default while ordinary original key delivery remains current');
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = LBefore, 'retired range cannot recapture new row selection at completion');
    SelectRange;
    LProxy := TRangeIntentProxy.Create;
    LProxyLease := LProxy;
    LProxy.Inner := LIntent;
    LProxy.ClearOnRead := LSelection;
    LCommand := LRangeInput.BeginRangeInput;
    LCommand.ResolveRange(LProxyLease);
    LCommand.Prepare;
    Check(not LCommand.Current and (LCommand.Payload = nil) and (LSelection.Selection = nil),
      'foreign readiness getter clearing the selection cannot lend its stale intent newer authority');
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = LBefore, 'stale getter reservation enters no clipboard host');
    SelectRange;
    PrepareRange;
    Inc(LFixture.Input.EpochValue);
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = LBefore,
      'original input epoch still fences range commands across disable-style ABA');
    LFixture.Host.InlineCompletion := False;
    LRawCommand := LFixture.Cell;
    LRawCommand.Complete(True);
    LOld := LFixture.Host.LastLease;
    LOldObject := LFixture.Host.Last;
    LBefore := LFixture.Host.Endings;
    LReplies := LFixture.Replies;
    PrepareRange;
    LCommand.Complete(True);
    Check((LFixture.Host.Endings = LBefore + 1) and (LFixture.Host.Text = LExpected),
      'fresh range displaces a pending cell write in the same slot');
    LOldObject.Finish;
    Check(LFixture.Replies = LReplies, 'late displaced cell completion cannot reply into a range write');
    LOld := nil;
    LSelection.Clear;
    LFixture.Host.Last.Finish;
    Check(LFixture.Replies = LReplies,
      'detached pending range host scope loses receiver authority when its exact selection retires');
    SelectRange;
    PrepareRange;
    LCommand.Complete(True);
    LBefore := LFixture.Host.Endings;
    LRawCommand := LFixture.Cell;
    LRawCommand.Complete(True);
    Check((LFixture.Host.Endings = LBefore + 1) and (LFixture.Host.Text = CText),
      'ordinary accepted cell copy can supersede range output without a second clipboard controller');
    LFixture.Commands.CancelPending;
    LFixture.Host.InlineCompletion := True;
    LFixture.ThrowReply := True;
    PrepareRange;
    LRefused := False;
    LReplies := LFixture.Replies;
    try
      LCommand.Complete(True);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.Replies = LReplies + 1) and
      (LFixture.Commands.Phase = nccpCompleted) and (LFixture.LastPayload.Text = LExpected),
      'inline range observer failure propagates once after terminal completion without replay');
    LFixture.ThrowReply := False;
    PrepareRange;
    LBefore := LFixture.Host.Writes;
    LColumns.ConfigureColumns(NyxColumnPreferences.WithColumn(
      NyxColumnPreference(LFields[0]).Width(200)));
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = LBefore,
      'changed actual column preference serial refuses the original prepared rectangle');
    PrepareRange;
    Check(not LCommand.Current and (LCommand.Payload = nil),
      'retained old range column provenance cannot become a fresh command after rearrangement');
    SelectRange;
    PrepareRange;
    LFixture.Filter;
    LCommand.Complete(True);
    Check((LFixture.Host.Writes = LBefore) and (LIntent.Capture.Payload.Text = LExpected),
      'filter replacement cannot retarget an old range or change retained RAW bytes');
  finally
    LSelection.Revoke;
    LCommand := nil;
    LRawCommand := nil;
    LRangeInput := nil;
    LOld := nil;
    LProxyLease := nil;
    LColumns := nil;
    LCapture := nil;
    LFixture.Free;
  end;
end;

procedure MountedAdmission;
var
  LFixture: TFixture;
  LView: INyxCollectionView;
  LGate: INyxCollectionInputGate;
  LObject: TTestMount;
  LMount: INyxCollectionMount;
  LAdmission: INyxCollectionInputAdmission;
  LClipboard: INyxCollectionClipboardMount;
  LCommands: INyxCollectionClipboardCommands;
  LCommand: INyxCollectionClipboardCommand;
  LInput: INyxCollectionInputLifetime;
  LRanges: INyxCollectionCellRangeMount;
  LRange: INyxCollectionCellRangeIntent;
  LBefore: Integer;
  LRefused: Boolean;

  procedure Mount(APhase: TNyxCollectionInputPhase;
    const AParent: INyxCollectionInputGate);
  begin
    LObject := TTestMount.Create(LView, APhase, AParent);
    LMount := LObject;
    LObject.Item := LFixture.ItemA;
    LObject.Field := LFixture.Field;
    LObject.Activate;
    Check(Supports(LMount, INyxCollectionInputAdmission, LAdmission) and
      Supports(LMount, INyxCollectionClipboardMount, LClipboard),
      'real mount exposes optional original-input and clipboard facets');
    LInput := LAdmission.GetInputLifetime;
    Check(Supports(LMount, INyxCollectionCellRangeMount, LRanges),
      'actual shared mount exposes independent opt-in typed range selection');
    LRanges.ConfigureCellRange(True);
    LClipboard.ConfigureClipboard(LFixture.HostLease, LFixture.Events,
      LFixture.Scheduler, TNyxCollectionClipboardOptions.Defaults);
    LCommands := LClipboard.ClipboardCommands;
  end;

  procedure ReleaseMount;
  begin
    LCommand := nil;
    LCommands := nil;
    LClipboard := nil;
    LRange := nil;
    LRanges := nil;
    LAdmission := nil;
    LMount := nil;
  end;
begin
  LFixture := TFixture.Create;
  try
    { Deliberately legacy layout: optional clipboard must honor the explicit
      input phase/ancestry without requiring authored arrangement defaults. }
    LView := NewNyxCollectionView(LFixture.Store,
      NyxCollectionView(LFixture.Key).Column(NyxTextField('Name'), 'Name'), cpTable);
    LGate := NewNyxCollectionInputGate;
    Mount(ncipStaged, LGate);
    Check(not LRanges.SelectCells(NyxCollectionCellIdentity(LFixture.ItemA, LFixture.Field),
      NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field)),
      'explicit range mode cannot select through unadmitted local or ancestor input');
    LCommand := LCommands.BeginCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    LCommand.Complete(True);
    Check(not LInput.Ready and (LFixture.Host.Writes = 0),
      'legacy-layout staged mount refuses clipboard work before actual admission');
    LAdmission.InstallInput;
    Check(not LInput.Ready, 'local installation cannot bypass a staged original ancestor');
    LGate.Install;
    Check(LInput.Ready, 'same original local/ancestor owners become ready after both actual installs');
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(LFixture.ItemA, LFixture.Field),
      NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field)),
      'actual admitted ordinary-layout table captures its exact page rectangle');
    LRange := LRanges.CellRange;
    LCommand := LCommands.BeginCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = 1, 'admitted legacy-layout mount copies through its original authority');
    LObject.ProbeOnRender := True;
    LMount.Refresh;
    Check(LObject.ProbeRefused and LInput.Ready,
      'refresh callback reentry cannot prepare a command while the actual input shield is held');
    Check(LObject.RangeProbeRefused and LRange.Current,
      'refresh shield refuses nested range admission without replacing accepted range facts');
    LObject.ProbeOnRender := False;
    LCommand := LCommands.BeginCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    LMount.SetInteraction(False, False);
    LMount.SetInteraction(True, False);
    Check(not LRange.Current and (LRanges.CellRange = nil),
      'disabled ABA retires the original mounted range even when data stays unchanged');
    Check(not LCommand.Current and LCommand.DispatchCurrent,
      'actual disable/re-enable advances legacy clipboard epoch without retiring canonical incarnation');
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = 1, 'actual disabled ABA cannot revive an original clipboard default');
    LFixture.Host.InlineCompletion := False;
    LCommand := LCommands.BeginCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    LCommand.Complete(True);
    LBefore := LFixture.Host.Endings;
    LObject.SuspendInput(True);
    Check(not LInput.Ready and (LCommands.Phase = nccpCancelled) and
      (LFixture.Host.Endings = LBefore + 1),
      'actual suspension changes original input and cancels exactly the pending host request');
    LObject.SuspendInput(False);
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(LFixture.ItemA, LFixture.Field),
      NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field)),
      'resumed actual mount admits a fresh range independently from the old command');
    LRange := LRanges.CellRange;
    LObject.SuspendInput(True);
    LObject.SuspendInput(False);
    Check(not LRange.Current and (LRanges.CellRange = nil),
      'explicit suspension withdraws mounted range authority across resume');
    LCommand.Complete(True);
    Check(LInput.Ready and (LFixture.Host.Writes = 2),
      'resume restores new readiness without repeating the transferred old command');
    LMount.Disconnect;
    Check(LInput.Retired and not LMount.Connected,
      'explicit mount teardown revokes its original input and controller');
    ReleaseMount;
    LGate := NewNyxCollectionInputGate;
    Mount(ncipStaged, LGate);
    LMount.Disconnect;
    LAdmission.InstallInput;
    LGate.Install;
    Check(LInput.Retired and not LInput.Ready,
      'never-admitted candidate retirement cannot be reversed by later local or ancestor install');
    ReleaseMount;
    Mount(ncipReady, nil);
    LCommand := LCommands.BeginCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    LCommand.Complete(True);
    LFixture.Host.ThrowEnd := True;
    LBefore := GDetached;
    LRefused := False;
    try
      LMount.Disconnect;
    except
      on E: ENyxCollection do
      begin
        LRefused := E.Message = '';
      end;
    end;
    Check(LRefused and LInput.Retired and (GDetached = LBefore + 1),
      'empty host cleanup failure still reports after actual target retirement');
    ReleaseMount;
    Mount(ncipReady, nil);
    LCommand := LCommands.BeginCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    LCommand.Complete(True);
    LBefore := GDetached;
    ReleaseMount;
    Check(LInput.Retired and (GDetached = LBefore + 1),
      'final owner release completes retirement without a throwing destructor or resurrected mount');
    LFixture.Host.ThrowEnd := False;
  finally
    ReleaseMount;
    LView := nil;
    LFixture.Free;
  end;
end;

{ Actual alternate source reentry occurs before the complete immutable range
  can be built. The original mount entry must preserve a newer Select/Clear
  even when every accepted source/page/column revision remains unchanged. }
procedure MountedRangeReentry;
var
  LFixture: TFixture;
  LSource: TRangeSource;
  LSourceLease: INyxCollection;
  LView: INyxCollectionView;
  LObject: TTestMount;
  LMount: INyxCollectionMount;
  LRanges: INyxCollectionCellRangeMount;
  LPrevious: INyxCollectionCellRangeIntent;
  LCurrent: INyxCollectionCellRangeIntent;
  LSourceBefore: INyxCollectionSnapshot;
begin
  LFixture := TFixture.Create;
  try
    LSource := TRangeSource.Create;
    LSourceLease := LSource;
    LSource.Inner := LFixture.Store;
    LView := NewNyxCollectionView(LSourceLease,
      NyxCollectionView(LFixture.Key).Column(NyxTextField('Name'), 'Name'), cpTable);
    LSourceBefore := LSource.Inner.Snapshot;
    LObject := TTestMount.Create(LView, ncipReady, nil);
    LMount := LObject;
    LObject.Item := LFixture.ItemA;
    LObject.Field := LFixture.Field;
    LObject.Activate;
    Supports(LMount, INyxCollectionCellRangeMount, LRanges);
    LRanges.ConfigureCellRange(True);
    Check(LRanges.SelectCells(NyxCollectionCellIdentity(LFixture.ItemA, LFixture.Field),
      NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field)),
      'alternate source establishes the actual original mounted range');
    LPrevious := LRanges.CellRange;
    LObject.SourceAction := rsaClear;
    LSource.OnSnapshot := LObject.ReadSource;
    Check(not LRanges.SelectCells(NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field),
      NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field)) and
      not LPrevious.Current and (LRanges.CellRange = nil),
      'actual source getter clear defeats the older original mount reservation');
    LObject.SourceAction := rsaNested;
    LSource.OnSnapshot := LObject.ReadSource;
    Check(not LRanges.SelectCells(NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field),
      NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field)) and LObject.NestedRangeAdmitted,
      'actual source getter admits one newer selection without lending its proof to the older entry');
    LCurrent := LRanges.CellRange;
    Check(LCurrent.Current and (LCurrent.Capture.Anchor.Item.ID = LFixture.ItemA.ID) and
      (LCurrent.Capture.Focus.Item.ID = LFixture.ItemA.ID) and
      (LCurrent.Capture.Payload.Key.Name = LFixture.Key.Name) and
      (LSource.Inner.Snapshot = LSourceBefore),
      'newer mounted rectangle stays exact while source data remains untouched');
    LMount.Disconnect;
  finally

    if LSourceLease <> nil then
    begin
      LSource.OnSnapshot := nil;
    end;
    LRanges := nil;
    LMount := nil;
    LView := nil;
    LSourceLease := nil;
    LFixture.Free;
  end;
end;

procedure Run;
var
  LFixture: TFixture;
  LCommand: INyxCollectionClipboardCommand;
  LOther: INyxCollectionClipboardCommand;
  LOld: INyxClipboardWriteRequest;
  LOldObject: TWrite;
  LRefused: Boolean;
  LBefore: Integer;
  LPayload: INyxCollectionClipboardText;
  LEntry: INyxCollectionClipboardCellInput;
  LCellInput: INyxCollectionClipboardCellCommand;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Restrict([nccaSelectedSource]);
    LCommand := LFixture.Commands.BeginCell(LFixture.ItemA, LFixture.Field);
    Check(LCommand.Current and LCommand.DispatchCurrent,
      'copy eligibility does not mute canonical key admission on a ready input');
    LCommand.Prepare;
    Check(not LCommand.Current and LCommand.DispatchCurrent and (LCommand.Payload = nil),
      'denied copy keeps canonical delivery but never captures source text');
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = 0, 'denied copy default cannot reach injected authority');
    LFixture.Restrict([nccaCell, nccaSelectedSource, nccaSelectedPage]);
    LCommand := LFixture.Cell;
    Check(LCommand.Current and LCommand.DispatchCurrent and
      (LCommand.Payload.Text = CText), 'prepared raw cell retains exact NUL/newlines/Unicode before dispatch');
    Check(LCommand.Complete(True), 'accepted scoped command claims the original input');
    Check((LFixture.Host.Writes = 1) and (LFixture.Host.Text = CText) and
      (LFixture.Replies = 1) and (LFixture.Commands.Phase = nccpCompleted),
      'one inline host completion carries the whole original cell exactly once');
    LCommand.Complete(True);
    LCommand.Retire;
    Check((LFixture.Host.Writes = 1) and (LFixture.Replies = 1),
      'repeated complete/retire cannot repeat an accepted write');
    LCommand := LFixture.Cell;
    LCommand.Complete(False);
    Check(LFixture.Host.Writes = 1, 'consumed canonical input claims without clipboard default');
    LFixture.Input.ReadyValue := False;
    LCommand := LFixture.Cell;
    Check(not LCommand.Current and LCommand.DispatchCurrent and (LCommand.Payload = nil),
      'staged/suspended input cannot capture accepted data but retains canonical incarnation');
    LFixture.Input.ReadyValue := True;
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = 1, 'later readiness cannot bless a denied entry');
    Check(Supports(LFixture.Commands, INyxCollectionClipboardCellInput, LEntry),
      'physical adapters can reserve an original cell proof before source item getters');
    LCellInput := LEntry.BeginCellInput;
    Inc(LFixture.Input.Mutation);
    LCellInput.ResolveCell(LFixture.ItemA, LFixture.Field);
    LCellInput.Prepare;
    Check(not LCellInput.Current and (LCellInput.Payload = nil),
      'later row resolution cannot recapture a mutation after physical entry');
    LCellInput.Complete(True);
    Check(LFixture.Host.Writes = 1, 'stale physical entry never hands newer row authority to host');
    LCommand := LFixture.Cell;
    Inc(LFixture.Input.EpochValue);
    Check(not LCommand.Current and LCommand.DispatchCurrent,
      'disable/suspend epoch ABA refuses default while preserving original canonical callbacks');
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = 1, 're-enabled input never transfers the old accepted proof');
    LCommand := LFixture.Cell;
    Inc(LFixture.Input.Mutation);
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = 1, 'cached mutation fence refuses even without a changed source pointer');
    LCommand := LFixture.Cell;
    LFixture.View.ClearSelection;
    Check(LCommand.Current, 'raw cell command does not acquire unrelated selection membership');
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = 2, 'explicit stable cell remains independent of selection');
    LFixture.View.SetSelection([LFixture.ItemA, LFixture.ItemB], LFixture.ItemA, LFixture.ItemA);
    LCommand := LFixture.Commands.BeginRows(nccmAllSelectedSource);
    LCommand.Prepare;
    LFixture.View.ClearSelection;
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = 2, 'rows command refuses changed selection rather than mixing memberships');
    LFixture.View.SetSelection([LFixture.ItemA, LFixture.ItemB], LFixture.ItemA, LFixture.ItemA);
    LFixture.Filter;
    LCommand := LFixture.Commands.BeginRows(nccmSelectedPage);
    LCommand.Prepare;
    Check((LCommand.Payload.RowCount = 1) and
      (LCommand.Payload.ItemAt(0).ID = LFixture.ItemB.ID),
      'explicit current-page command uses the actual filtered page');
    LCommand.Complete(True);
    LCommand := LFixture.Commands.BeginRows(nccmAllSelectedSource);
    LCommand.Prepare;
    Check((LCommand.Payload.RowCount = 2) and
      (LCommand.Payload.CellValue(0, 0).TextValue = CText),
      'explicit source membership retains the filtered selected row');
    LCommand.Complete(True);
    LCommand := LFixture.Cell;
    LFixture.Store.Update(NyxCollectionItem(LFixture.ItemA)
      .WithValue(NyxTextField('Name'), 'later accepted'));
    Check(LCommand.DispatchCurrent and not LCommand.Current and
      (LCommand.Payload.Text = CText), 'data edit preserves canonical delivery and immutable old payload only');
    LBefore := LFixture.Host.Writes;
    LCommand.Complete(True);
    Check(LFixture.Host.Writes = LBefore, 'model callback cannot redirect default to its newer accepted value');
    LFixture.Host.InlineCompletion := False;
    LCommand := LFixture.Cell;
    LCommand.Complete(True);
    LOld := LFixture.Host.LastLease;
    LOldObject := LFixture.Host.Last;
    Check(LFixture.Commands.Phase = nccpPending, 'pending host result is distinct from an accepted effect');
    LOther := LFixture.Cell;
    LOther.Complete(False);
    Check(LFixture.Commands.Phase = nccpPending,
      'beginning or consuming a newer command does not cancel independent pending work');
    LBefore := LFixture.Host.Endings;
    LOther := LFixture.Cell;
    LOther.Complete(True);
    Check((LFixture.Host.Endings = LBefore + 1) and
      (LFixture.Commands.Phase = nccpPending), 'new accepted command retires the exact older request once');
    LBefore := LFixture.Replies;
    LOldObject.Finish;
    Check(LFixture.Replies = LBefore, 'late older terminal result has no observer authority');
    LOld := nil;
    LFixture.Host.Last.Finish;
    Check((LFixture.Replies = LBefore + 1) and (LFixture.Commands.Phase = nccpCompleted),
      'newer request owns its own final receipt');
    LCommand := LFixture.Cell;
    LCommand.Complete(True);
    LBefore := LFixture.Replies;
    Inc(LFixture.Input.EpochValue);
    LFixture.Host.Last.Finish;
    Check(LFixture.Replies = LBefore,
      'owner-free host scope withdraws asynchronous receipt after epoch retirement');
    LFixture.Commands.CancelPending;
    Check(LFixture.Commands.Phase = nccpCancelled,
      'explicit mount cancellation retires the silent host receiver and pending phase');
    LFixture.Host.InlineCompletion := True;
    LFixture.ThrowReply := True;
    LCommand := LFixture.Cell;
    LBefore := LFixture.Replies;
    LRefused := False;
    try
      LCommand.Complete(True);
    except
      on E: Exception do
      begin
        LRefused := E.Message = 'intentional inline observer failure';
      end;
    end;
    Check(LRefused and (LFixture.Replies = LBefore + 1) and
      (LFixture.Commands.Phase = nccpCompleted),
      'inline client failure propagates once after terminal result without reclassification');
    LFixture.ThrowReply := False;
    LFixture.RevokeReply := True;
    LCommand := LFixture.Cell;
    LPayload := LCommand.Payload;
    LCommand.Complete(True);
    Check((LFixture.LastPayload = LPayload) and not LCommand.DispatchCurrent,
      'inline observer can revoke controller without stale tail writes');
    LCommand := LFixture.Cell;
    Check(not LCommand.Current and (LCommand.Payload = nil),
      'terminal controller refuses fresh preparation and cannot regain host authority');
    Check((LFixture.Store.Snapshot.Item(LFixture.ItemA).GetValue(NyxTextField('Name')) =
      'later accepted') and (LPayload.Text = 'later accepted'),
      'copy never edits accepted data and retained payload survives controller retirement');
  finally
    LCommand := nil;
    LCellInput := nil;
    LEntry := nil;
    LOther := nil;
    LOld := nil;
    LFixture.Free;
  end;
end;

begin
  try
    Run;
    CapturedArbitration;
    RepresentedArbitration;
    RangeArbitration;
    MountedAdmission;
    MountedRangeReentry;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-clipboard-commands', 'passed');
    document.body.setAttribute('data-collection-clipboard-command-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection clipboard command checks; no OS write claim');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-clipboard-commands', 'failed');
      document.body.setAttribute('data-collection-clipboard-command-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
