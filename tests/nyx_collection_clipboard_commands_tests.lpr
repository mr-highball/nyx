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
  nyx.collections.query, nyx.collections.paging, nyx.collections.arrangement, nyx.collections.mount,
  nyx.collections.clipboard, nyx.collections.clipboard.actions,
  nyx.collections.clipboard.commands, nyx.collections.representation,
  nyx.collections.cell.actions,
  nyx.collections.draft, nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.identities,
  nyx.collections.range.commands,
  nyx.formatting, nyx.contract, nyx.resources, nyx.clipboard, nyx.scheduler, nyx.events,
  nyx.publication
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

  { A foreign value facade can copy public getters and lie about the action;
    it owns no private original-controller witness and must never gain a write. }
  TForeignActionCapture = class(TInterfacedObject, INyxCollectionClipboardActionCapture)
  public
    Inner: INyxCollectionClipboardActionCapture;
    function GetAction: TNyxCollectionClipboardAction;
    function GetPayload: INyxCollectionClipboardText;
    function GetReadiness: INyxCancellationReadiness;
  end;

  { Public identity values cannot forge the private originating-controller
    witness. This facade deliberately copies an actual token's visible facts. }
  TForeignIdentityAction = class(TInterfacedObject, INyxCollectionClipboardIdentityAction)
  public
    Inner: INyxCollectionClipboardIdentityAction;
    function GetValue: INyxCollectionIdentityText;
    function GetReadiness: INyxCancellationReadiness;
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
    OnReadiness: TEndingCallback;
    IdentityReplies: Integer;
    IdentityValue: INyxCollectionIdentityText;
    ThrowIdentity: Boolean;
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
    procedure ClearSelection;
    procedure IgnoreView(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure IgnoreStore(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
    procedure Identities(const AValue: INyxCollectionIdentityText;
      const AResult: TNyxClipboardWriteResult);
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

function TForeignActionCapture.GetAction: TNyxCollectionClipboardAction;
begin
  Result := nccaCell;
end;

function TForeignActionCapture.GetPayload: INyxCollectionClipboardText;
begin
  Result := Inner.Payload;
end;

function TForeignActionCapture.GetReadiness: INyxCancellationReadiness;
begin
  Result := Inner.Readiness;
end;

function TForeignIdentityAction.GetValue: INyxCollectionIdentityText;
begin
  Result := Inner.Value;
end;

function TForeignIdentityAction.GetReadiness: INyxCancellationReadiness;
begin
  Result := Inner.Readiness;
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
var
  LCall: TEndingCallback;
begin
  LCall := OnReadiness;
  OnReadiness := nil;

  if Assigned(LCall) then
  begin
    LCall;
  end;
  Result := TFence.Create(Input);
end;

procedure TFixture.ClearSelection;
begin
  View.SetSelection([], ItemA, ItemA);
end;

procedure TFixture.IgnoreView(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
end;

procedure TFixture.IgnoreStore(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
end;

procedure TFixture.Identities(const AValue: INyxCollectionIdentityText;
  const AResult: TNyxClipboardWriteResult);
begin
  Inc(IdentityReplies);
  IdentityValue := AValue;
  Check((AResult.Outcome = ncwoWritten) and (AResult.ByteCount = AValue.ByteCount),
    'identity observer receives only exact identity provenance and the actual host receipt');

  if ThrowIdentity then
  begin
    raise Exception.Create('identity observer failure after terminal receipt');
  end;
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

{ A context attaches the existing rectangle before source getters, but captures
  the accepted launch cell independently. Optional range retirement must never
  turn ordinary inspection/copy into a different cell or cancel its authority. }
procedure RangeContextCapture;
var
  LFixture: TFixture;
  LSelection: INyxCollectionCellRangeSelection;
  LIntent: INyxCollectionCellRangeIntent;
  LInput: INyxCollectionClipboardCellInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LContext: INyxCollectionClipboardCellRangeContext;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LFields: TNyxCollectionClipboardFields;
  LPayload: INyxCollectionClipboardText;
  LReady: INyxCancellationReadiness;
  LWriter: INyxCollectionClipboardCapturedWrite;
  LCancel: INyxCancellationScope;
  LTicket: INyxClipboardWriteRequest;
  LOld: INyxClipboardWriteRequest;
  LOldObject: TWrite;
  LProxy: TRangeIntentProxy;
  LProxyLease: INyxCollectionCellRangeIntent;
  LExpected: TNyxText;
  LCellBytes: Integer;
  LBefore: Integer;
  LReplies: Integer;
  LRefused: Boolean;

  procedure SelectRange(AOutside: Boolean = False);
  var
    LAnchor: TNyxCollectionCellIdentity;
    LFocus: TNyxCollectionCellIdentity;
  begin
    LCapture := LColumns.CaptureColumns;
    LAnchor := NyxCollectionCellIdentity(LFixture.ItemB, LFields[1]);
    LFocus := NyxCollectionCellIdentity(LFixture.ItemA, LFields[0]);

    if AOutside then
    begin
      LFocus := LAnchor;
    end;
    LIntent := LSelection.Select(LFixture.Store.Snapshot, LFixture.View.Snapshot,
      LFields, LAnchor, LFocus, LCapture.Serial, False, nccleCRLF,
      TNyxCollectionClipboardLimits.Defaults);
  end;

  procedure PrepareContext(const AIntent: INyxCollectionCellRangeIntent);
  begin
    LCommand := LInput.BeginCellInput;
    Check(Supports(LCommand, INyxCollectionClipboardCellRangeContext, LContext),
      'accepted-cell reservation exposes optional same-envelope range attachment');
    LContext.AttachRange(AIntent);
    LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);
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
    Check(Supports(LFixture.Commands, INyxCollectionClipboardCellInput, LInput),
      'context captures through the original accepted-cell input reservation');
    LProxy := TRangeIntentProxy.Create;
    LProxyLease := LProxy;
    LProxy.Inner := LIntent;
    LProxy.ClearOnRead := LSelection;
    PrepareContext(LProxyLease);
    Check(LIntent.Current and (LProxy.ClearOnRead <> nil) and LCommand.Current and
      (LCommand.Payload.Text = CText) and (LContext.RangePayload = nil),
      'default-off range attachment reads no foreign getter and preserves exact RAW cell');
    LCommand.Retire;
    LFixture.Restrict([nccaCell, nccaCellRange]);
    Check(Supports(LFixture.Commands, INyxCollectionClipboardCellInput, LInput) and
      Supports(LFixture.Commands, INyxCollectionClipboardCapturedWrite, LWriter),
      'explicit range permission uses the existing input and captured-write slot');
    PrepareContext(LIntent);
    LPayload := LContext.RangePayload;
    LReady := LContext.RangeReadiness;
    LExpected := 'Name' + #9 + 'Score' + #10 + '"' + CText + '"' + #9 + '1' + #10 + 'B' + #9 + '2';
    Check(LCommand.Current and (LCommand.Payload.Text = CText) and
      (LPayload <> nil) and (LReady <> nil) and (LPayload.Text = LExpected) and
      (LPayload.RowCount = 2) and (LPayload.FieldCount = 2) and
      (LPayload.ItemAt(0).ID = LFixture.ItemA.ID) and
      LPayload.FieldAt(1).Same(LFields[1]) and
      (LPayload.CellValue(0, 0).TextValue = CText) and (LFixture.Host.Writes = 0),
      'selected-cell context normalizes original typed membership and controller headers/LF independently of range formatting');
    LCellBytes := LCommand.Payload.ByteCount;
    LSelection.Clear;
    LRefused := False;
    try
      LReady.CheckReady;
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LCommand.Current and (LCommand.Payload.Text = CText) and
      (LPayload.Text = LExpected),
      'clearing the rectangle retires only its separate proof while RAW cell and copied TSV survive');
    LCommand.Complete(True);
    Check((LFixture.Host.Writes = 1) and (LFixture.Host.Text = CText),
      'ordinary accepted-cell completion never silently substitutes the attached rectangle');
    SelectRange;
    Check(LIntent.Capture.Payload.ByteCount > LCellBytes,
      'existing generously admitted rectangle exceeds a later one-cell clipboard byte budget');
    LFixture.Commands.Revoke;
    LFixture.Commands := NewNyxCollectionClipboardCommands(LFixture.View,
      LFixture.InputLease, LFixture.Events, LFixture.HostLease, LFixture.Scheduler,
      TNyxCollectionClipboardOptions.Defaults.AllowOnly([nccaCell, nccaCellRange])
        .Limits(TNyxCollectionClipboardLimits.Defaults.UTF8Bytes(LCellBytes)),
      {$ifdef PAS2JS}@{$endif}LFixture.Readiness);
    LFixture.Commands.Observe({$ifdef PAS2JS}@{$endif}LFixture.Reply);
    Check(Supports(LFixture.Commands, INyxCollectionClipboardCellInput, LInput),
      'bounded controller retains the same accepted-cell reservation contract');
    PrepareContext(LIntent);
    Check(LCommand.Current and (LCommand.Payload.Text = CText) and
      (LCommand.Payload.ByteCount = LCellBytes) and (LContext.RangePayload = nil) and
      (LContext.RangeReadiness = nil) and LIntent.Current,
      'optional TSV byte refusal omits only the range action while the complete exact RAW cell remains admitted');
    LCommand.Retire;
    LFixture.Restrict([nccaCell, nccaCellRange]);
    Check(Supports(LFixture.Commands, INyxCollectionClipboardCellInput, LInput) and
      Supports(LFixture.Commands, INyxCollectionClipboardCapturedWrite, LWriter),
      'ordinary configured budgets restore independently of the retained rectangle');
    SelectRange(True);
    PrepareContext(LIntent);
    Check(LCommand.Current and (LContext.RangePayload = nil) and
      (LContext.RangeReadiness = nil),
      'a context outside selected typed membership omits the range action without refusing RAW');
    LCommand.Retire;
    SelectRange;
    LProxy.Inner := LIntent;
    LProxy.ClearOnRead := LSelection;
    PrepareContext(LProxyLease);
    Check((LSelection.Selection = nil) and LCommand.Current and
      (LContext.RangePayload = nil),
      'a getter withdrawing the exact selection cannot lend a newer context its old range authority');
    LCommand.Retire;
    SelectRange;
    LColumns.ConfigureColumns(NyxColumnPreferences.WithColumn(
      NyxColumnPreference(LFields[0]).Width(200)));
    PrepareContext(LIntent);
    Check(LCommand.Current and (LContext.RangePayload = nil),
      'changed column serial omits stale range metadata while preserving the newly captured accepted cell');
    LCommand.Retire;
    SelectRange;
    PrepareContext(LIntent);
    LPayload := LContext.RangePayload;
    LReady := LContext.RangeReadiness;
    LCommand.Retire;
    LContext := nil;
    LCommand := nil;
    LReady.CheckReady;
    Check(LPayload.Text = LExpected,
      'retiring the physical envelope leaves detached payload and original range readiness usable');
    LCancel := NewNyxCancellationScope;
    LFixture.Host.InlineCompletion := False;
    LTicket := LWriter.WriteCaptured(LPayload, LCancel, LReady, nil, nil);
    LOld := LFixture.Host.LastLease;
    LOldObject := LFixture.Host.Last;
    LBefore := LFixture.Host.Endings;
    LReplies := LFixture.Replies;
    LCommand := LInput.BeginCellInput;
    LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    LCommand.Complete(True);
    Check((LFixture.Host.Endings = LBefore + 1) and (LFixture.Host.Text = CText),
      'captured range-menu output and a later ordinary key share one physical pending slot');
    LOldObject.Finish;
    Check(LFixture.Replies = LReplies,
      'late superseded range output cannot publish into the later ordinary key receipt');
    LBefore := LFixture.Host.Endings;
    LTicket.Cancel;
    Check(LFixture.Host.Endings = LBefore,
      'retained range-menu cancellation cannot cancel a newer accepted-cell request');
    LOld := nil;
    LFixture.Host.Last.Finish;
    LFixture.Host.InlineCompletion := True;
    LFixture.ThrowReply := True;
    LRefused := False;
    LBefore := LFixture.Replies;
    try
      LTicket := LWriter.WriteCaptured(LPayload, LCancel, LReady, nil, nil);
    except
      on E: Exception do
      begin
        LRefused := E.Message = 'intentional inline observer failure';
      end;
    end;
    Check(LRefused and (LFixture.Replies = LBefore + 1) and
      (LFixture.Commands.Phase = nccpCompleted),
      'captured range inline failure remains terminal and propagates once without a second write engine');
    LFixture.ThrowReply := False;
    LSelection.Revoke;
    LRefused := False;
    LBefore := LFixture.Host.Writes;
    try
      LTicket := LWriter.WriteCaptured(LPayload, LCancel, LReady, nil, nil);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.Host.Writes = LBefore) and (LPayload.Text = LExpected),
      'terminal range retirement refuses host admission without destroying retained exact TSV');
  finally
    LSelection.Revoke;
    LFixture.Commands.CancelPending;
    LOld := nil;
    LTicket := nil;
    LContext := nil;
    LCommand := nil;
    LWriter := nil;
    LInput := nil;
    LProxyLease := nil;
    LCapture := nil;
    LColumns := nil;
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

{ Readonly entry captures the original accepted cell independently of transfer
  permissions. Ordinary physical copy still captures nothing when disallowed;
  the optional capture cannot be completed into an unauthorized host write. }
procedure InspectionPermissions;
var
  LFixture: TFixture;
  LInspection: INyxCollectionClipboardInspectionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LPhysical: INyxCollectionClipboardCommand;
  LPermission: INyxCollectionClipboardCellPermission;
  LPayload: INyxCollectionClipboardText;
  LRange: INyxCollectionCellRangeSelection;
  LIntent: INyxCollectionCellRangeIntent;
  LContext: INyxCollectionClipboardCellRangeContext;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LOptions: TNyxCollectionClipboardActions;
  LCase: Integer;
begin
  LFixture := TFixture.Create;
  try
    for LCase := 0 to 2 do
    begin
      case LCase of
        0:
          begin
            LOptions := [];
          end;
        1:
          begin
            LOptions := [nccaSelectedSource, nccaSelectedPage];
          end;
      else
        begin
          LOptions := [nccaCellRange];
        end;
      end;
      LFixture.Restrict(LOptions);
      LPhysical := LFixture.Cell;
      Check(not LPhysical.Current and (LPhysical.Payload = nil) and
        LPhysical.DispatchCurrent, 'disallowed physical cell copy retains canonical dispatch without accepted-value capture');
      LPhysical.Complete(True);
      Check(Supports(LFixture.Commands, INyxCollectionClipboardInspectionInput, LInspection),
        'the same configured controller exposes optional readonly original entry');
      LCommand := LInspection.BeginCellInspection;
      LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);

      if LCase = 2 then
      begin
        LRange := NewNyxCollectionCellRangeSelection;
        LColumns := NyxCollectionColumns(LFixture.View);
        LCapture := LColumns.CaptureColumns;
        LIntent := LRange.Select(LFixture.Store.Snapshot, LFixture.View.Snapshot,
          [LFixture.Field], NyxCollectionCellIdentity(LFixture.ItemA, LFixture.Field),
          NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field), LCapture.Serial,
          True, nccleLF, TNyxCollectionClipboardLimits.Defaults);
        Check(Supports(LCommand, INyxCollectionClipboardCellRangeContext, LContext),
          'readonly original command retains the optional independently permitted range');
        LContext.AttachRange(LIntent);
      end;
      LCommand.Prepare;
      Check(LCommand.Current and (LCommand.Payload.Text = CText) and
        LCommand.Payload.FieldAt(0).Same(LFixture.Field) and
        (LCommand.Payload.ItemAt(0).ID = LFixture.ItemA.ID),
        'empty/row-only/range-only permissions preserve exact Unicode NUL RAW inspection provenance');
      Check(Supports(LCommand, INyxCollectionClipboardCellPermission, LPermission) and
        not LPermission.CellCopyAllowed, 'inspection reports captured cell transfer permission separately');

      if LCase = 2 then
      begin
        Check((LContext.RangePayload <> nil) and (LContext.RangePayload.RowCount = 2) and
          (LContext.RangeReadiness <> nil), 'range-only context captures its independent selected-cell action');
      end;
      LPayload := LCommand.Payload;
      LCommand.Complete(True);
      LCommand.Retire;
      Check((LFixture.Host.Writes = 0) and (LPayload.Text = CText),
        'inspection capture never promotes a denied cell completion into clipboard authority');
    end;
    LFixture.Restrict([nccaCell]);
    Check(Supports(LFixture.Commands, INyxCollectionClipboardInspectionInput, LInspection),
      'readonly entry remains available alongside permitted physical copy');
    LCommand := LInspection.BeginCellInspection;
    LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    Check(Supports(LCommand, INyxCollectionClipboardCellPermission, LPermission) and
      LPermission.CellCopyAllowed, 'permitted context captures the original cell permission');
    LCommand.Complete(True);
    Check((LFixture.Host.Writes = 1) and (LFixture.Host.Text = CText),
      'permitted context retains the established same-slot exact transfer behavior');
  finally
    LContext := nil;
    LCapture := nil;
    LColumns := nil;
    LIntent := nil;
    LRange := nil;
    LCommand := nil;
    LPhysical := nil;
    LPermission := nil;
    LInspection := nil;
    LFixture.Free;
  end;
end;

{ The original prepared seal grants only its actual selected-cell permission.
  Public value facades and a replacement controller cannot relabel/adopt it;
  all accepted transfers still run through the existing single host slot. }
procedure CapturedActionPermission;
var
  LFixture: TFixture;
  LEntry: INyxCollectionClipboardInspectionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LContext: INyxCollectionClipboardCellRangeContext;
  LTransfers: INyxCollectionClipboardContextTransfers;
  LRange: INyxCollectionCellRangeSelection;
  LIntent: INyxCollectionCellRangeIntent;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LToken: INyxCollectionClipboardActionCapture;
  LWriter: INyxCollectionClipboardCapturedActionWrite;
  LCellWriter: INyxCollectionClipboardCapturedWrite;
  LCancel: INyxCancellationScope;
  LRequest: INyxClipboardWriteRequest;
  LForeign: TForeignActionCapture;
  LForeignLease: INyxCollectionClipboardActionCapture;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Restrict([nccaCellRange]);
    Supports(LFixture.Commands, INyxCollectionClipboardInspectionInput, LEntry);
    Supports(LFixture.Commands, INyxCollectionClipboardCapturedActionWrite, LWriter);
    Supports(LFixture.Commands, INyxCollectionClipboardCapturedWrite, LCellWriter);
    LColumns := NyxCollectionColumns(LFixture.View);
    LCapture := LColumns.CaptureColumns;
    LRange := NewNyxCollectionCellRangeSelection;
    LIntent := LRange.Select(LFixture.Store.Snapshot, LFixture.View.Snapshot,
      [LFixture.Field], NyxCollectionCellIdentity(LFixture.ItemA, LFixture.Field),
      NyxCollectionCellIdentity(LFixture.ItemB, LFixture.Field), LCapture.Serial,
      True, nccleLF, TNyxCollectionClipboardLimits.Defaults);
    LCommand := LEntry.BeginCellInspection;
    LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);
    Supports(LCommand, INyxCollectionClipboardCellRangeContext, LContext);
    LContext.AttachRange(LIntent);
    LCommand.Prepare;
    Check(Supports(LCommand, INyxCollectionClipboardContextTransfers, LTransfers),
      'original readonly command exposes sealed independent transfer actions');
    LToken := LTransfers.RangeAction;
    Check((LToken <> nil) and (LToken.Action = nccaCellRange) and
      (LToken.Payload = LContext.RangePayload), 'range-only permission seals the actual original range payload');
    LCancel := NewNyxCancellationScope;
    LRequest := LCellWriter.WriteCaptured(LCommand.Payload, LCancel, LToken.Readiness, nil, nil);
    Check((LRequest = nil) and (LFixture.Host.Writes = 0),
      'range-only authority cannot promote an accepted cell through the unchanged cell write port');
    LForeign := TForeignActionCapture.Create;
    LForeignLease := LForeign;
    LForeign.Inner := LToken;
    LRequest := LWriter.WriteCapturedAction(LForeignLease, LCancel, LToken.Readiness, nil, nil);
    Check((LRequest = nil) and (LFixture.Host.Writes = 0),
      'copied public token values and a relabeled action have no original-controller provenance');
    LRequest := LWriter.WriteCapturedAction(LToken, LCancel, LToken.Readiness, nil, nil);
    Check((LFixture.Host.Writes = 1) and (LFixture.Host.Text = LToken.Payload.Text),
      'sealed range-only action reaches the same injected host exactly once');
    LFixture.Restrict([nccaCellRange]);
    LToken.Readiness.CheckReady;
    Supports(LFixture.Commands, INyxCollectionClipboardCapturedActionWrite, LWriter);
    LRequest := LWriter.WriteCapturedAction(LToken, LCancel, LToken.Readiness, nil, nil);
    Check((LRequest = nil) and (LFixture.Host.Writes = 1),
      'even equal permitted options cannot move an old action seal to a replacement controller');
  finally
    LRequest := nil;
    LForeignLease := nil;
    LToken := nil;
    LTransfers := nil;
    LContext := nil;
    LCommand := nil;
    LEntry := nil;
    LWriter := nil;
    LCellWriter := nil;
    LIntent := nil;
    LRange := nil;
    LCapture := nil;
    LColumns := nil;
    LFixture.Free;
  end;
end;

{ The original selected-source and current-page cohorts are independent of the
  inspected cell. Real accepted selection/page/column changes retire only those
  optional actions; the readonly accepted cell remains available. }
procedure SelectedRowsContext;
var
  LFixture: TFixture;
  LEntry: INyxCollectionClipboardInspectionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LRows: INyxCollectionClipboardCellRowsContext;
  LTransfers: INyxCollectionClipboardContextTransfers;
  LSourceAction: INyxCollectionClipboardActionCapture;
  LPageAction: INyxCollectionClipboardActionCapture;
  LWriter: INyxCollectionClipboardCapturedActionWrite;
  LCancel: INyxCancellationScope;
  LRequest: INyxClipboardWriteRequest;
  LPages: INyxCollectionPages;
  LColumns: INyxCollectionColumns;
  LColumnCapture: INyxCollectionColumnsCapture;
  LPlan: INyxPreparedPublication;
  LExpected: INyxCollectionClipboardText;
  LRefused: Boolean;
  LBefore: Integer;

  procedure Prepare;
  begin
    LCommand := LEntry.BeginCellInspection;
    LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    Supports(LCommand, INyxCollectionClipboardCellRowsContext, LRows);
    Supports(LCommand, INyxCollectionClipboardContextTransfers, LTransfers);
    LSourceAction := LTransfers.SourceRowsAction;
    LPageAction := LTransfers.PageRowsAction;
  end;

  procedure SetPaging;
  begin
    LPages.ConfigurePaging(NyxCollectionPaging.PageSize(1));
  end;

  procedure RestoreSelection;
  begin
    LFixture.View.SetSelection([LFixture.ItemA, LFixture.ItemB],
      LFixture.ItemA, LFixture.ItemA);
  end;

  procedure UpdateSource;
  begin
    { End the fluent store receiver here, independently of retained proofs. }
    LFixture.Store.Update(NyxCollectionItem(LFixture.ItemB)
      .WithValue(NyxIntegerField('Score'), 3));
  end;

  procedure Refuses(const AAction: INyxCollectionClipboardActionCapture;
    const AReason: string);
  begin
    LRefused := False;
    LBefore := LFixture.Host.Writes;
    try
      LRequest := LWriter.WriteCapturedAction(AAction, LCancel,
        AAction.Readiness, nil, nil);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.Host.Writes = LBefore), AReason);
  end;

begin
  LFixture := TFixture.Create;
  try
    LFixture.Restrict([nccaSelectedSource, nccaSelectedPage]);
    Supports(LFixture.Commands, INyxCollectionClipboardInspectionInput, LEntry);
    Supports(LFixture.Commands, INyxCollectionClipboardCapturedActionWrite, LWriter);
    LPages := NyxCollectionPages(LFixture.View);
    SetPaging;
    LCancel := NewNyxCancellationScope;
    Prepare;
    Check(LCommand.Current and (LCommand.Payload.Text = CText) and
      (LSourceAction <> nil) and (LPageAction <> nil),
      'row-only context keeps independent RAW inspection and both original selected-row actions');
    Check((LRows.SourceRowsPayload.RowCount = 2) and
      (LRows.PageRowsPayload.RowCount = 1) and
      (LRows.SourceRowsPayload.ItemAt(0).ID = LFixture.ItemA.ID) and
      (LRows.SourceRowsPayload.ItemAt(1).ID = LFixture.ItemB.ID) and
      (LRows.PageRowsPayload.ItemAt(0).ID = LFixture.ItemA.ID),
      'source includes off-page selection while page copy keeps exact launch page membership and order');
    LExpected := NewNyxCollectionClipboardRows(LFixture.Store.Snapshot,
      LFixture.View.Selection.Snapshot,
      [LFixture.Field, NyxColumnIdentity(NyxIntegerField('Score'))],
      nccmAllSelectedSource, nil, True, nccleLF, TNyxCollectionClipboardLimits.Defaults);
    Check((LSourceAction.Payload.Text = LExpected.Text) and
      (LSourceAction.Payload.CellValue(0, 0).TextValue = CText) and
      (LSourceAction.Action = nccaSelectedSource) and (LPageAction.Action = nccaSelectedPage),
      'row tokens retain exact escaped Unicode/NUL/newline RAW values, typed fields and distinct original actions');
    LRequest := LWriter.WriteCapturedAction(LPageAction, LCancel,
      LPageAction.Readiness, nil, nil);
    Check((LFixture.Host.Writes = 1) and
      (LFixture.Host.Text = LRows.PageRowsPayload.Text),
      'selected-page token reaches the existing host slot without cell-copy permission');
    LRequest := LWriter.WriteCapturedAction(LSourceAction, LCancel,
      LSourceAction.Readiness, nil, nil);
    Check((LFixture.Host.Writes = 2) and (LFixture.Host.Text = LExpected.Text),
      'selected-source token uses the same arbitration slot and exact original rows');
    { The two successful writes deliberately advanced the shared command slot.
      Start a fresh original command before isolating selection-only retirement. }
    Prepare;
    LFixture.ClearSelection;
    RestoreSelection;
    Refuses(LSourceAction, 'selection ABA refuses the old source action without an extra host entry');
    Check(LCommand.Current and (LCommand.Payload.Text = CText),
      'selection retirement leaves the independent accepted-cell inspection current');
    Prepare;
    LPages.GoToPage(2);
    Refuses(LPageAction, 'a page transition retires the original page action before it can copy a newer cohort');
    LPages.GoToPage(1);
    LFixture.OnReadiness := {$ifdef PAS2JS}@{$endif}LFixture.ClearSelection;
    Prepare;
    Check(LCommand.Current and (LCommand.Payload.Text = CText) and
      (LSourceAction = nil) and (LPageAction = nil),
      'selection captured BEFORE outward readiness cannot silently become the nested selection');
    RestoreSelection;
    Prepare;
    LColumns := NyxCollectionColumns(LFixture.View);
    LColumnCapture := LColumns.CaptureColumns;
    LPlan := LColumns.PrepareColumns(LColumnCapture,
      NyxColumnPreferences.WithColumn(NyxColumnPreference(LFixture.Field).Width(200)));
    LPlan.Validate;
    LPlan.Install;
    Refuses(LSourceAction,
      'actual column Install retires original row authority before any participant notification');
    LPlan.Notify;
    LPlan.Retire;
    LPlan := nil;
    Prepare;
    UpdateSource;
    Refuses(LSourceAction, 'actual source installation retires the original selected-row transfer');
    Prepare;
    LFixture.Filter;
    Refuses(LSourceAction, 'accepted filter replacement retires original selected-source menu authority');
    Check(LExpected.RowCount = 2, 'later filtering cannot mutate retained original source bytes');
    Prepare;
    Check((LRows.SourceRowsPayload.RowCount = 2) and
      (LRows.PageRowsPayload.RowCount = 1) and
      (LRows.PageRowsPayload.ItemAt(0).ID = LFixture.ItemB.ID),
      'fresh source-row action includes hidden selected members while page action uses the filtered launch page');
  finally

    if LPlan <> nil then
    begin
      LPlan.Retire;
    end;
    LPlan := nil;
    LColumnCapture := nil;
    LColumns := nil;
    LPages := nil;
    LRequest := nil;
    LExpected := nil;
    LSourceAction := nil;
    LPageAction := nil;
    LTransfers := nil;
    LRows := nil;
    LCommand := nil;
    LEntry := nil;
    LWriter := nil;
    LFixture.Free;
  end;
end;

{ A retained menu action and view-provided row observation are genuinely
  detached values. The last controller/view/store can retire while their copied
  text remains readable, and the inert readiness refuses after that retirement. }
procedure SelectedRowsLifetime;
var
  LFixture: TFixture;
  LEntry: INyxCollectionClipboardInspectionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LTransfers: INyxCollectionClipboardContextTransfers;
  LAction: INyxCollectionClipboardActionCapture;
  LRowsView: INyxCollectionSelectedRowsView;
  LCapture: INyxCollectionSelectedRowsCapture;
  LViewToken: INyxCollectionViewSubscription;
  LStoreToken: INyxCollectionSubscription;
  LRetired: INyxCollectionClipboardCommands;
  LRefused: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Restrict([nccaSelectedSource]);
    Supports(LFixture.Commands, INyxCollectionClipboardInspectionInput, LEntry);
    Supports(LFixture.View, INyxCollectionSelectedRowsView, LRowsView);
    LCapture := LRowsView.CaptureSelectedRows;
    LCommand := LEntry.BeginCellInspection;
    LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    Supports(LCommand, INyxCollectionClipboardContextTransfers, LTransfers);
    LAction := LTransfers.SourceRowsAction;
    Check((LAction <> nil) and (LTransfers.PageRowsAction = nil),
      'source-only permission cannot manufacture a page transfer token');
    LViewToken := LFixture.View.Subscribe({$ifdef PAS2JS}@{$endif}LFixture.IgnoreView);
    LStoreToken := LFixture.Store.Subscribe({$ifdef PAS2JS}@{$endif}LFixture.IgnoreStore);
    LTransfers := nil;
    LCommand := nil;
    LEntry := nil;
    LRowsView := nil;
    LRetired := LFixture.Commands;
    LRetired.Revoke;
    LFixture.Commands := nil;
    LFixture.View := nil;
    LFixture.Store := nil;
    Check(not LViewToken.Connected and not LStoreToken.Connected,
      'retained row token, capture and revoked controller keep neither actual view nor store owner alive');
    Check((LCapture.Selection.Count = 2) and (LAction.Payload.RowCount = 2) and
      (LAction.Payload.CellValue(0, 0).TextValue = CText),
      'exact copied selection/text remain readable after their live owners retire');
    LRefused := False;
    try
      LAction.Readiness.CheckReady;
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'owner retirement marks the detached row action unavailable without a borrowed owner lookup');
  finally
    LAction := nil;
    LCapture := nil;
    LRowsView := nil;
    LTransfers := nil;
    LCommand := nil;
    LEntry := nil;
    LViewToken := nil;
    LStoreToken := nil;
    LRetired := nil;
    LFixture.Free;
  end;
end;

{ The optional multi-row output has its own bounded admission. A valid cell
  inspection survives a stricter row-byte limit, while permitted pending row
  writes still share supersession with the original physical RAW-cell route. }
procedure SelectedRowsAdmission;
var
  LFixture: TFixture;
  LEntry: INyxCollectionClipboardInspectionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LTransfers: INyxCollectionClipboardContextTransfers;
  LToken: INyxCollectionClipboardActionCapture;
  LWriter: INyxCollectionClipboardCapturedActionWrite;
  LCancel: INyxCancellationScope;
  LRequest: INyxClipboardWriteRequest;
  LOld: TWrite;
  LOldLease: INyxClipboardWriteRequest;
  LRaw: INyxCollectionClipboardCommand;
  LPayload: INyxCollectionClipboardText;
  LOptions: TNyxCollectionClipboardOptions;

  procedure ConfigureBounded;
  begin
    LFixture.Commands.Revoke;
    LOptions := TNyxCollectionClipboardOptions.Defaults.AllowOnly([nccaSelectedSource])
      .Limits(TNyxCollectionClipboardLimits.Defaults.UTF8Bytes(LPayload.ByteCount));
    LFixture.Commands := NewNyxCollectionClipboardCommands(LFixture.View,
      LFixture.InputLease, LFixture.Events, LFixture.HostLease, LFixture.Scheduler,
      LOptions, {$ifdef PAS2JS}@{$endif}LFixture.Readiness);
  end;

  procedure Prepare;
  begin
    Supports(LFixture.Commands, INyxCollectionClipboardInspectionInput, LEntry);
    LCommand := LEntry.BeginCellInspection;
    LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    Supports(LCommand, INyxCollectionClipboardContextTransfers, LTransfers);
  end;

begin
  LFixture := TFixture.Create;
  try
    LPayload := NewNyxCollectionClipboardCell(LFixture.Store.Snapshot,
      LFixture.ItemA, LFixture.Field, TNyxCollectionClipboardLimits.Defaults);
    ConfigureBounded;
    Prepare;
    Check(LCommand.Current and (LCommand.Payload.Text = CText) and
      (LTransfers.SourceRowsAction = nil) and (LTransfers.PageRowsAction = nil),
      'row byte refusal omits only optional rows and preserves an exact valid readonly cell');
    LFixture.Restrict([nccaCell, nccaSelectedSource]);
    Prepare;
    LToken := LTransfers.SourceRowsAction;
    Supports(LFixture.Commands, INyxCollectionClipboardCapturedActionWrite, LWriter);
    LCancel := NewNyxCancellationScope;
    LFixture.Host.InlineCompletion := False;
    LRequest := LWriter.WriteCapturedAction(LToken, LCancel, LToken.Readiness, nil, nil);
    LOld := LFixture.Host.Last;
    LOldLease := LFixture.Host.LastLease;
    Check((LRequest <> nil) and (LFixture.Commands.Phase = nccpPending),
      'row menu transfer uses the actual pending host slot');
    LFixture.Host.InlineCompletion := True;
    LRaw := LFixture.Cell;
    LRaw.Complete(True);
    Check((LFixture.Host.Writes = 2) and (LFixture.Host.Endings = 2) and
      (LFixture.Replies = 1) and (LFixture.Host.Text = CText),
      'a later ordinary RAW cell cancels only the old row receiver on the same arbitration slot');
    LRequest.Cancel;
    Check(LFixture.Host.Endings = 2, 'old row cancellation cannot cancel the newer RAW receipt');
    LOld.Finish;
    Check((LFixture.Host.Writes = 2) and (LFixture.LastPayload.Text = CText) and
      (LFixture.Commands.Phase = nccpCompleted),
      'late old row completion cannot replace the newer RAW receipt');
  finally
    LRaw := nil;
    LOldLease := nil;
    LRequest := nil;
    LToken := nil;
    LTransfers := nil;
    LCommand := nil;
    LEntry := nil;
    LWriter := nil;
    LPayload := nil;
    LFixture.Free;
  end;
end;

{ Identity representation uses the exact original row cohort/permission but
  reports on its own facet. Real pending host cancellation and inline receiver
  failure exercise the existing single clipboard request arbitration. }
procedure IdentityActions;
var
  LFixture: TFixture;
  LEntry: INyxCollectionClipboardInspectionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LTransfers: INyxCollectionClipboardIdentityTransfers;
  LRows: INyxCollectionClipboardContextTransfers;
  LSource: INyxCollectionClipboardIdentityAction;
  LPage: INyxCollectionClipboardIdentityAction;
  LWriter: INyxCollectionClipboardIdentityWrite;
  LCancel: INyxCancellationScope;
  LRequest: INyxClipboardWriteRequest;
  LOld: INyxClipboardWriteRequest;
  LOldObject: TWrite;
  LRaw: INyxCollectionClipboardCommand;
  LForeign: TForeignIdentityAction;
  LForeignLease: INyxCollectionClipboardIdentityAction;
  LPages: INyxCollectionPages;
  LBefore: Integer;
  LRefused: Boolean;
  LRawBytes: Integer;

  procedure Prepare;
  begin
    Supports(LFixture.Commands, INyxCollectionClipboardInspectionInput, LEntry);
    Supports(LFixture.Commands, INyxCollectionClipboardIdentityWrite, LWriter);
    LWriter.ObserveIdentities({$ifdef PAS2JS}@{$endif}LFixture.Identities);
    LCommand := LEntry.BeginCellInspection;
    LCommand.ResolveCell(LFixture.ItemA, LFixture.Field);
    LCommand.Prepare;
    Supports(LCommand, INyxCollectionClipboardIdentityTransfers, LTransfers);
    LSource := LTransfers.SourceIdentityAction;
    LPage := LTransfers.PageIdentityAction;
  end;

  procedure ConfigurePage;
  begin
    LPages.ConfigurePaging(NyxCollectionPaging.PageSize(1));
  end;

  procedure RestoreSelection;
  begin
    LFixture.View.SetSelection([LFixture.ItemA, LFixture.ItemB], LFixture.ItemA, LFixture.ItemA);
  end;

  procedure ConfigureBounded;
  begin
    LFixture.Commands.Revoke;
    LFixture.Commands := NewNyxCollectionClipboardCommands(LFixture.View,
      LFixture.InputLease, LFixture.Events, LFixture.HostLease, LFixture.Scheduler,
      TNyxCollectionClipboardOptions.Defaults.AllowOnly([nccaSelectedSource])
        .Limits(TNyxCollectionClipboardLimits.Defaults.UTF8Bytes(LRawBytes)),
      {$ifdef PAS2JS}@{$endif}LFixture.Readiness);
    LFixture.Commands.Observe({$ifdef PAS2JS}@{$endif}LFixture.Reply);
  end;

begin
  LFixture := TFixture.Create;
  try
    LFixture.Restrict([nccaSelectedSource, nccaSelectedPage]);
    LPages := NyxCollectionPages(LFixture.View);
    ConfigurePage;
    Prepare;
    LCancel := NewNyxCancellationScope;
    Check((LSource <> nil) and (LPage <> nil) and
      (LSource.Value.Text = 'ID' + #10 + LFixture.ItemA.ID + #10 + LFixture.ItemB.ID) and
      (LPage.Value.Text = 'ID' + #10 + LFixture.ItemA.ID),
      'original row-only source/page permissions provide distinct exact record-ID choices');
    LForeign := TForeignIdentityAction.Create;
    LForeignLease := LForeign;
    LForeign.Inner := LSource;
    LRequest := LWriter.WriteIdentities(LForeignLease, LCancel, LSource.Readiness, nil, nil);
    Check((LRequest = nil) and (LFixture.Host.Writes = 0),
      'copied public identity getters cannot forge the original private transfer seal');
    LRequest := LWriter.WriteIdentities(LSource, LCancel, LSource.Readiness, nil, nil);
    Check((LFixture.Host.Writes = 1) and (LFixture.IdentityReplies = 1) and
      (LFixture.Replies = 0) and (LFixture.RepresentationReplies = 0) and
      (LFixture.Commands.LastPayload = nil) and (LWriter.IdentityPhase = nccpCompleted) and
      (LWriter.LastIdentities = LSource.Value) and
      (LWriter.LastIdentityResult.ByteCount = LSource.Value.ByteCount),
      'identity receipt is terminal/exact and never masquerades as RAW column or formatted-cell output');
    LFixture.Restrict([nccaSelectedPage]);
    Prepare;
    Check((LSource = nil) and (LPage <> nil),
      'page-only permission exposes only the original page identity representation');
    LRequest := LWriter.WriteIdentities(LPage, LCancel, LPage.Readiness, nil, nil);
    Check((LFixture.Host.Writes = 2) and (LFixture.Host.Text = 'ID' + #10 + LFixture.ItemA.ID) and
      (LFixture.Replies = 0) and (LWriter.LastIdentities.Membership = nccmSelectedPage),
      'page-only identity transfer writes just the original page and never broadens to selected source or cell data');
    LFixture.Restrict([nccaSelectedSource, nccaSelectedPage]);
    Prepare;
    LFixture.ClearSelection;
    RestoreSelection;
    LBefore := LFixture.Host.Writes;
    LRefused := False;
    try
      LRequest := LWriter.WriteIdentities(LSource, LCancel, LSource.Readiness, nil, nil);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.Host.Writes = LBefore) and LCommand.Current,
      'selection ABA refuses the old ID cohort without withdrawing independent RAW inspection');
    LFixture.OnReadiness := {$ifdef PAS2JS}@{$endif}LFixture.ClearSelection;
    Prepare;
    Check(LCommand.Current and (LSource = nil) and (LPage = nil),
      'nested pre-factory selection cannot be recaptured as a newer ID action');
    RestoreSelection;
    LRawBytes := LCommand.Payload.ByteCount;
    ConfigureBounded;
    Prepare;
    Supports(LCommand, INyxCollectionClipboardContextTransfers, LRows);
    Check((LRows.SourceRowsAction = nil) and (LSource <> nil) and (LPage = nil),
      'identity output has its own byte admission and cannot silently gain disallowed page membership');
    LFixture.Restrict([nccaCell, nccaSelectedSource]);
    Prepare;
    LFixture.Host.InlineCompletion := False;
    LRequest := LWriter.WriteIdentities(LSource, LCancel, LSource.Readiness, nil, nil);
    LOld := LFixture.Host.LastLease;
    LOldObject := LFixture.Host.Last;
    LBefore := LFixture.IdentityReplies;
    Check(LWriter.IdentityPhase = nccpPending, 'ID request enters the actual shared pending host slot');
    LFixture.Host.InlineCompletion := True;
    LRaw := LFixture.Cell;
    LRaw.Complete(True);
    LOldObject.Finish;
    Check((LFixture.IdentityReplies = LBefore) and (LWriter.IdentityPhase = nccpCancelled) and
      (LFixture.LastPayload.Text = CText) and (LFixture.Commands.Phase = nccpCompleted),
      'new RAW copy retires the old ID receiver and late completion cannot replace either receipt');
    LRequest.Cancel;
    Prepare;
    LFixture.ThrowIdentity := True;
    LBefore := LFixture.IdentityReplies;
    LRefused := False;
    try
      LRequest := LWriter.WriteIdentities(LSource, LCancel, LSource.Readiness, nil, nil);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.IdentityReplies = LBefore + 1) and
      (LWriter.IdentityPhase = nccpCompleted) and
      (LFixture.Commands.LastPayload.Text = CText),
      'inline identity observer failure propagates exactly once after its separate terminal receipt');
    LFixture.ThrowIdentity := False;
    LFixture.Restrict([nccaSelectedSource]);
    Supports(LFixture.Commands, INyxCollectionClipboardIdentityWrite, LWriter);
    LRequest := LWriter.WriteIdentities(LSource, LCancel, LSource.Readiness, nil, nil);
    Check(LRequest = nil, 'a replacement controller cannot adopt an otherwise ready original ID seal');
  finally
    LForeignLease := nil;
    LOld := nil;
    LRequest := nil;
    LRaw := nil;
    LRows := nil;
    LPage := nil;
    LSource := nil;
    LTransfers := nil;
    LWriter := nil;
    LCommand := nil;
    LEntry := nil;
    LPages := nil;
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
    InspectionPermissions;
    CapturedActionPermission;
    SelectedRowsContext;
    SelectedRowsLifetime;
    SelectedRowsAdmission;
    IdentityActions;
    CapturedArbitration;
    RepresentedArbitration;
    RangeArbitration;
    RangeContextCapture;
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
