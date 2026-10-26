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
program nyx_input_publication_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.composition, nyx.codec,
  nyx.state, nyx.binding, nyx.binding.types, nyx.binding.inputs, nyx.publication,
  nyx.collections, nyx.collections.query, nyx.collections.query.session,
  nyx.collections.view, nyx.collections.view.types
  {$ifdef PAS2JS}, Web{$endif};

type
  { Independent authored defaults, realized scalar owner and public query view.
    Tokens borrow this receiver and retire before its owned models. No DOM/LCL
    or ordinary toolbar behavior is claimed by this portable publication test. }
  TFixture = class
  public
    Document: TNyxDocument;
    Root: TNyxNode;
    State: TNyxState;
    Live: TNyxLiveBindings;
    Inputs: INyxInputCommands;
    Store: INyxCollection;
    View: INyxCollectionView;
    Session: INyxCollectionQuerySession;
    Actor: INyxCollectionQueryOwner;
    StateToken: TNyxStateSubscription;
    QueryToken: INyxCollectionViewSubscription;
    WantSearch: TNyxText;
    WantSort: TNyxText;
    WantRows: Integer;
    Checking: Boolean;
    Coherent: Boolean;
    FailObserver: Boolean;
    FailSync: Boolean;
    Reject: Boolean;
    StateCalls: Integer;
    QueryCalls: Integer;
    SyncCalls: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Observe;
    procedure Synced;
    procedure StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
    procedure QueryChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure Expect(const ASearch, ASort: TNyxText; ARows: Integer);
    function Capture: INyxInputCapture;
    function QueryClear: INyxPreparedPublication;
  end;
  { A later group validator changes an owned fact or revokes the real receiver.
    Its borrowed fixture outlives publication; it never calls another stage's
    phases. Installs records whether a final fence prevented every model write. }
  TGate = class(TInterfacedObject, INyxPreparedPublication)
  public
    Fixture: TFixture;
    Revoke: Boolean;
    RevokeQuery: Boolean;
    Installs: Integer;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('Input publication: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TFixture.Create;
var
  LPage: TNyxNode;
  LKey: TNyxCollectionRef;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  Document.State.SetValue(NyxTextState('query'), 'Ada');
  Document.State.SetValue(NyxTextState('payload'), TNyxText('data' + #0 + '🌙'));
  Document.State.SetValue(NyxIntegerState('floor'), 0);
  LPage := TNyxNode.Create(nkColumn, 'root');
  Document.AddPage(LPage);
  LPage.Add(TNyxNode.Create(nkInput, 'search').Configure.Value('Ada').Done);
  LPage.Find('search').Binds.Value(NyxTextState('query'));
  LPage.Add(TNyxNode.Create(nkSelect, 'sort').Configure
    .Items('Name' + #10 + 'Score').Value('Name').Done);
  LPage.Add(TNyxNode.Create(nkMemo, 'mirror'));
  LPage.Find('mirror').Binds.Value(NyxTextState('query'));
  LPage.Add(TNyxNode.Create(nkSpin, 'quantity').Configure
    .Minimum(0).Maximum(10).Value(3).Done);
  LPage.Find('quantity').Binds.Minimum(NyxIntegerState('floor'));
  LPage.Add(TNyxNode.Create(nkSpin, 'floor').Configure
    .Minimum(-10).Maximum(10).Value(0).Done);
  LPage.Find('floor').Binds.Value(NyxIntegerState('floor'));
  LPage.Add(TNyxNode.Create(nkInput, 'locked').Configure
    .Value('Locked').ReadOnly(True).Done);
  LPage.Add(TNyxNode.Create(nkInput, 'from-state'));
  LPage.Find('from-state').Binds.Value(NyxTextState('query'), bdFromState);
  LPage.Add(TNyxNode.Create(nkInput, 'absent'));
  Root := RealizeNyxView(Document, LPage);
  State := Document.State.Clone;
  Live := TNyxLiveBindings.Create(Root, State);
  Live.OnSync := Synced;
  Live.Activate;
  Inputs := Live.Inputs;
  LKey := NyxCollection('people');
  Store := NewNyxCollection(LKey,
    NyxCollectionSchema.Text(NyxTextField('name'), ''), [
      NyxCollectionItem(NyxItem(LKey, 'a')).WithValue(NyxTextField('name'), 'Ada'),
      NyxCollectionItem(NyxItem(LKey, 'b')).WithValue(NyxTextField('name'), 'Bea'),
      NyxCollectionItem(NyxItem(LKey, 'c')).WithValue(NyxTextField('name'), 'Cy')]);
  View := NewNyxCollectionView(Store,
    NyxCollectionView(LKey).Column(NyxTextField('name'), 'Name', cmReadOnly), cpTable);
  Session := NyxCollectionQuerySession(View);
  Actor := Session.AcquireOwner(NyxQueryOrigin('toolbar'), 'Search toolbar');
  PublishNyxGroup([Session.PrepareSession(Session.Capture, Actor,
    NyxQuerySessionEdits.Filter(NyxQueryChannel('search'),
      NyxWhere(NyxTextField('name')).Contains('Ada')))]);
  StateToken := State.Subscribe(StateChanged, Validate);
  QueryToken := View.Subscribe(QueryChanged);
  Coherent := True;
end;

destructor TFixture.Destroy;
begin
  StateToken.Free;

  if QueryToken <> nil then
  begin
    QueryToken.Disconnect;
  end;
  QueryToken := nil;
  Actor := nil;
  Session := nil;
  View := nil;
  Store := nil;
  Live.Free;
  Inputs := nil;
  Root.Free;
  State.Free;
  Document.Free;
  inherited Destroy;
end;

procedure TFixture.Expect(const ASearch, ASort: TNyxText; ARows: Integer);
begin
  WantSearch := ASearch;
  WantSort := ASort;
  WantRows := ARows;
  Checking := True;
end;

procedure TFixture.Observe;
begin

  if Checking then
  begin
    Coherent := Coherent and
      (Root.Find('search').Prop('value') = WantSearch) and
      (Root.Find('mirror').Prop('value') = WantSearch) and
      (Root.Find('sort').Prop('value') = WantSort) and
      (State.GetValue(NyxTextState('query')) = WantSearch) and
      (View.Snapshot.Count = WantRows);
  end;
end;

procedure TFixture.Synced;
begin
  Inc(SyncCalls);
  Observe;

  if FailSync then
  begin
    raise ENyxState.Create('Deliberate target sync failure');
  end;
end;

procedure TFixture.StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
begin
  Inc(StateCalls);
  Observe;

  if FailObserver then
  begin
    raise ENyxState.Create('Deliberate state observer failure');
  end;
end;

procedure TFixture.QueryChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(QueryCalls);
  Observe;
end;

procedure TFixture.Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
begin

  if Reject then
  begin
    raise ENyxState.Create('Deliberate scalar validator refusal');
  end;
end;

function TFixture.Capture: INyxInputCapture;
begin
  Result := Inputs.Capture([NyxControl('search'), NyxControl('sort')]);
end;

function TFixture.QueryClear: INyxPreparedPublication;
begin
  Result := Session.PrepareSession(Session.Capture, Actor, NyxQuerySessionEdits.ClearOwner);
end;

procedure TGate.Validate;
begin

  if Revoke then
  begin
    Fixture.Live.Retire;
  end
  else if RevokeQuery then
  begin
    Fixture.Actor.Revoke;
  end
  else
  begin
    Fixture.Root.Find('mirror').Configure.Text('A newer external caption').Done;
  end;
end;

procedure TGate.Install;
begin
  Inc(Installs);
end;

procedure TGate.Notify;
begin
end;

procedure TGate.Retire;
begin
end;

procedure Refuse(const AInputs: INyxInputCommands; const ACapture: INyxInputCapture;
  const AValues: array of TNyxInputValue; const AReason: TNyxText);
var
  LStage: INyxPreparedPublication;
  LRefused: Boolean;
begin
  LRefused := False;
  try
    LStage := AInputs.Prepare(ACapture, AValues);
    PublishNyxGroup([LStage]);
  except
    on ENyxState do
    begin
      LRefused := True;
    end;
  end;
  LStage := nil;
  Check(LRefused, AReason);
end;

procedure AdmissionCases;
var
  F: TFixture;
  P: TFixture;
  C: INyxInputCapture;
  S: INyxPreparedPublication;
  LRevision: Integer;
  LAuthored: TNyxText;
  LRef: TNyxControlRef;
  LRefused: Boolean;
begin
  F := TFixture.Create;
  P := nil;
  try
    LAuthored := TNyxCodec.Encode(F.Document);
    C := F.Capture;
    Check((C.Count = 2) and C.HasValueAt(0) and
      (C.ValueAt(0).TextValue = 'Ada'), 'capture owns exact typed roles and values');
    LRef := C.ControlAt(0);
    LRef.ID := 'changed copy';
    Check((LRef.ID = 'changed copy') and (C.ControlAt(0).ID = 'search'),
      'returned role cannot mutate the capture');
    Refuse(F.Inputs, C, [NyxInputValue(NyxControl('search'), ''),
      NyxInputValue(NyxControl('sort'), 7)], 'wrong-family second value refuses the complete batch');
    Check((F.State.GetValue(NyxTextState('query')) = 'Ada') and
      (F.Root.Find('sort').Prop('value') = 'Name') and F.Live.RefreshReady,
      'constructor refusal preserves both values and releases reservations');
    Refuse(F.Inputs, C, [NyxInputValue(NyxControl('quantity'), 4)],
      'uncaptured target has no authority');
    Refuse(F.Inputs, C, [NyxInputValue(NyxControl('search'), ''),
      NyxInputValue(NyxControl('search'), 'Bea')], 'duplicate role refuses');
    Refuse(F.Inputs, C, [], 'empty value batch refuses');
    P := TFixture.Create;
    Refuse(P.Inputs, C, [NyxInputValue(NyxControl('search'), '')],
      'same-named foreign runtime refuses the original capture');
    Refuse(F.Inputs, F.Inputs.Capture([NyxControl('locked')]),
      [NyxInputValue(NyxControl('locked'), 'Changed')], 'read-only input cannot be reset');
    Refuse(F.Inputs, F.Inputs.Capture([NyxControl('from-state')]),
      [NyxInputValue(NyxControl('from-state'), 'Changed')], 'state-only binding cannot be written');
    Refuse(F.Inputs, F.Inputs.Capture([NyxControl('quantity')]),
      [NyxInputValue(NyxControl('quantity'), 11)], 'unbound scalar range refuses before publication');
    LRefused := False;
    try
      F.Live.Edit(F.Root.Find('quantity'), '11');
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (F.Root.Find('quantity').Prop('value') = '3'),
      'ordinary unbound wire input obeys the same numeric maximum');
    LRefused := False;
    try
      F.State.SetValue(NyxIntegerState('floor'), 4);
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (F.State.GetValue(NyxIntegerState('floor')) = 0),
      'a new bound minimum cannot silently clamp an unchanged unbound value');
    C := F.Capture;
    F.Live.Edit(F.Root.Find('sort'), 'Score');
    F.Live.Edit(F.Root.Find('sort'), 'Name');
    Refuse(F.Inputs, C, [NyxInputValue(NyxControl('sort'), 'Score')],
      'unbound A-to-B-to-A stales the old capture without a store revision');
    C := F.Inputs.Capture([NyxControl('absent')]);
    Check(not C.HasValueAt(0), 'absence stays distinct from empty text');
    S := F.Inputs.Prepare(C, [NyxInputValue(NyxControl('absent'), '')]);
    S.Retire;
    S := nil;
    Check(F.Live.RefreshReady and (F.Root.Find('absent').Props.IndexOfName('value') < 0),
      'abandonment releases state/view without creating an empty value');
    F.Reject := True;
    Refuse(F.Inputs, F.Capture, [NyxInputValue(NyxControl('search'), '')],
      'existing state validator can refuse the scalar candidate');
    F.Reject := False;
    Check((F.State.GetValue(NyxTextState('query')) = 'Ada') and F.Live.RefreshReady,
      'validator refusal retains accepted state and releases both owners');
    C := F.Capture;
    LRevision := F.State.Revision;
    F.Expect('', 'Name', 3);
    PublishNyxGroup([F.Inputs.Prepare(C, [NyxInputValue(NyxControl('search'), ''),
      NyxInputValue(NyxControl('sort'), 'Name')]), F.QueryClear]);
    Check(F.Coherent and (F.State.Revision = LRevision + 1) and
      (F.StateCalls = 1) and (F.QueryCalls = 1),
      'state/query observers see both accepted inputs and cleared rows together');
    Check(F.Root.Find('mirror').Prop('value') = '',
      'other controls bound to the same scalar project the admitted reset');
    Check(TNyxCodec.Encode(F.Document) = LAuthored,
      'runtime reset never changes authored defaults or controls');
    Check((F.State.GetValue(NyxTextState('payload')) = TNyxText('data' + #0 + '🌙')) and
      (F.Document.State.GetValue(NyxTextState('payload')) = TNyxText('data' + #0 + '🌙')),
      'unprojected state retains NUL and supplementary text through a grouped reset');
    Refuse(F.Inputs, C, [NyxInputValue(NyxControl('search'), 'Bea')],
      'accepted publication stales the previous capture');
    F.Checking := False;
    Refuse(F.Inputs, F.Capture,
      [NyxInputValue(NyxControl('search'), TNyxText('Café / 🌙' + #0 + 'tail'))],
      'portable text widget refuses NUL before a lossy projection');
    C := F.Capture;
    PublishNyxGroup([F.Inputs.Prepare(C,
      [NyxInputValue(NyxControl('search'), TNyxText('Café / 🌙'))])]);
    Check(F.State.GetValue(NyxTextState('query')) = TNyxText('Café / 🌙'),
      'typed input publication preserves supplementary Unicode exactly');
  finally
    S := nil;
    C := nil;
    P.Free;
    F.Free;
  end;
end;

procedure PublicationCases;
var
  F: TFixture;
  S: INyxPreparedPublication;
  Q: INyxPreparedPublication;
  G: TGate;
  LLease: INyxPreparedPublication;
  C: INyxInputCapture;
  LCase: Integer;
  LRefused: Boolean;
  LMessage: TNyxText;
begin
  for LCase := 0 to 4 do
  begin
    F := TFixture.Create;
    try
      C := F.Capture;
      S := F.Inputs.Prepare(C, [NyxInputValue(NyxControl('search'), ''),
        NyxInputValue(NyxControl('sort'), 'Name')]);
      Q := F.QueryClear;

      if LCase < 3 then
      begin
        G := TGate.Create;
        LLease := G;
        G.Fixture := F;
        G.Revoke := LCase = 0;
        G.RevokeQuery := LCase = 2;
        LRefused := False;
        try
          PublishNyxGroup([S, Q, LLease]);
        except
          on ENyxState do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and (G.Installs = 0) and
          (F.State.GetValue(NyxTextState('query')) = 'Ada') and
          (F.View.Snapshot.Count = 1),
          'late retirement/property mutation fences every participant before installation');

        if LCase = 1 then
        begin
          Check(F.Root.Find('mirror').Prop('text') = 'A newer external caption',
            'refusal cannot roll back a later validator external change');
        end;
      end
      else
      begin
        F.Expect('', 'Name', 3);
        F.FailObserver := True;
        F.FailSync := LCase = 4;
        LMessage := '';
        try
          PublishNyxGroup([S, Q]);
        except
          on LException: ENyxPublicationNotification do
          begin
            LMessage := LException.ReceiverMessage;
          end;
        end;
        Check((Pos('Deliberate state observer failure', LMessage) > 0) and
          F.Coherent and (F.SyncCalls = 1) and (F.QueryCalls = 1),
          'first notification failure stays committed and independent sync/query receivers run');
        Check((F.State.GetValue(NyxTextState('query')) = '') and
          (F.Root.Find('search').Prop('value') = '') and (F.View.Snapshot.Count = 3),
          'notification failure never restores old input or query values');
      end;
      Check(not F.State.Busy, 'terminal group releases the existing scalar reservation');
    finally
      LLease := nil;
      S := nil;
      Q := nil;
      C := nil;
      F.Free;
    end;
  end;
end;

{ The full candidate supplies a numeric input's new bound minimum. Checking
  only the pre-command control would reject a valid coordinated change. }
procedure CoupledRangeCases;
var
  F: TFixture;
  C: INyxInputCapture;
begin
  F := TFixture.Create;
  try
    C := F.Inputs.Capture([NyxControl('quantity'), NyxControl('floor')]);
    PublishNyxGroup([F.Inputs.Prepare(C, [NyxInputValue(NyxControl('quantity'), -1),
      NyxInputValue(NyxControl('floor'), -1)])]);
    Check((F.Root.Find('quantity').Prop('value') = '-1') and
      (F.Root.Find('quantity').Prop('min') = '-1') and
      (F.State.GetValue(NyxIntegerState('floor')) = -1) and
      (F.Document.State.GetValue(NyxIntegerState('floor')) = 0),
      'coordinated value/minimum admits the complete candidate and preserves defaults');
  finally
    F.Free;
  end;
end;

{ A retained preparation owns detached candidates, not the models that admitted
  it. Free the real owners first; old captures remain data and commands refuse. }
procedure RetirementCases;
var
  F: TFixture;
  C: INyxInputCapture;
  P: INyxInputCommands;
  S: INyxPreparedPublication;
  LRefused: Boolean;
begin
  F := TFixture.Create;
  try
    C := F.Capture;
    P := F.Inputs;
    S := P.Prepare(C, [NyxInputValue(NyxControl('search'), '')]);
  finally
    F.Free;
  end;
  Check(not P.Connected and (C.ValueAt(0).TextValue = 'Ada'),
    'retained weak port/capture do not keep their actual owners alive');
  Refuse(P, C, [NyxInputValue(NyxControl('search'), '')],
    'old commands cannot revive a destroyed live view');
  LRefused := False;
  try
    PublishNyxGroup([S]);
  except
    on ENyxState do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'a retained prepared group refuses destroyed state/view owners safely');
end;

begin
  try
    AdmissionCases;
    PublicationCases;
    CoupledRangeCases;
    RetirementCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-input-publication', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed live input/query publication checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
