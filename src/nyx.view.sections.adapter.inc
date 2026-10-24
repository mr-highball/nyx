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
{ Included only by target adapter units. Aliases/functions below are local to
  those adapters; no target type enters nyx.view.sections. The same ownership
  state machine stages real existing Nyx renderers on both targets. }

type
  { One synchronous cleanup keeps the exact FIRST exception while independent
    duties retire. Browser platform throws are arbitrary JS values; native
    exceptions require acquisition from the active exception stack. }
  TSectionFailure = record
    Captured: Boolean;
    {$ifdef PAS2JS}
    Value: JSValue;
    {$else}
    Value: Exception;
    {$endif}
  end;
  { Forward owner is leased by every pending publication; target hosts remain
    borrowed while their renderers and private presentation planes are owned. }
  TSection = class;
  { One reversible target candidate. Native planes keep a stable parent; browser
    parking retains its original move protocol. Commit transfers prepared fields
    only, and old renderers retire after every group member becomes current. }
  TSectionPublication = class(TInterfacedObject, INyxViewSectionPublication,
    INyxViewSectionCandidateRetirement)
  private
    FOwner: TSection;
    FLease: INyxViewSection;
    FExpectedRevision: Integer;
    FExpectedSchemaRevision: Integer;
    FExpectedParent: TSectionParent;
    FExpectedWidth: Integer;
    FExpectedHeight: Integer;
    FCandidate: TSectionRenderer;
    FPrevious: TSectionRenderer;
    FStageHost: TSectionHost;
    FPreviousHost: TSectionHost;
    FFormerFocus: TSectionFocus;
    FFormerSelection: TNyxTextSelection;
    FCandidateMoveAttempted: Boolean;
    FPreviousMoveAttempted: Boolean;
    FPublished: Boolean;
    FBorrowedState: TNyxState;
  public
    constructor Create(AOwner: TSection; ADocument: TNyxDocument;
      ARoot: TNyxNode; ADesignMode: Boolean; AState: TNyxState);
    destructor Destroy; override;
    function GetSection: INyxViewSection;
    function GetExpectedRevision: Integer;
    function Check: Boolean;
    procedure Preview;
    procedure Rollback;
    procedure Publish;
    procedure RetirePrevious;
    procedure RetireCandidate;
  end;

  { Managed section identity over one accepted renderer and, on native targets,
    its accepted presentation plane. Pending candidates lease this owner without
    a reverse reference cycle and never own the caller's stable host. }
  TSection = class(TInterfacedObject,
    {$ifdef PAS2JS}INyxBrowserViewSection,{$else}INyxLCLViewSection,{$endif}
    INyxViewSection)
  private
    FReference: TNyxViewSectionRef;
    FRevision: Integer;
    FHost: TSectionHost;
    FRenderer: TSectionRenderer;
    {$ifndef PAS2JS}
    FCurrentHost: TSectionHost;
    {$endif}
    FBorrowedState: TNyxState;
    FConfigure: TSectionConfigure;
    FBorrowedTheme: TNyxTheme;
    FObserver: INyxViewSectionObserver;
    FBusy: Boolean;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  public
    constructor Create(const AReference: TNyxViewSectionRef; AHost: TSectionHost;
      AConfigure: TSectionConfigure; const AObserver: INyxViewSectionObserver;
      ATheme: TNyxTheme);
    destructor Destroy; override;
    function GetReference: TNyxViewSectionRef;
    function GetRevision: Integer;
    function GetRoot: TNyxNode;
    function GetRenderer: TSectionRenderer;
    function Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
      ADesignMode: Boolean; AState: TNyxState): INyxViewSectionChange;
    procedure Close;
  end;

{ Called ONLY from an active except block. Capture flags distinguish a genuine
  JavaScript throw of null/undefined from the absence of any failure. }
procedure CaptureSectionFailure(var AFailure: TSectionFailure
  {$ifdef PAS2JS}; AValue: JSValue{$endif});
begin

  if AFailure.Captured then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}
  AFailure.Value := AValue;
  {$else}
  AFailure.Value := Exception(AcquireExceptionObject);
  {$endif}
  AFailure.Captured := True;
end;

{ Finish every exact duty before returning the retained original Pascal object
  or platform throw; never substitute a diagnostic exception for its identity. }
procedure RaiseSectionFailure(const AFailure: TSectionFailure);
{$ifdef PAS2JS}
var
  LValue: JSValue;
{$endif}
begin

  if AFailure.Captured then
  begin
    {$ifdef PAS2JS}
    LValue := AFailure.Value;
    asm
      throw LValue;
    end;
    {$else}
    raise AFailure.Value;
    {$endif}
  end;
end;

{ Explicit renderer retirement reports supported disconnect failures before
  raw destruction. The exact renderer is detached first and is always freed;
  its destructor sees an already unmounted view and remains nonthrowing. }
procedure RetireSectionRenderer(var ARenderer: TSectionRenderer);
var
  LRenderer: TSectionRenderer;
  LFailure: TSectionFailure;
begin
  LRenderer := ARenderer;
  ARenderer := nil;
  LFailure := Default(TSectionFailure);

  if LRenderer = nil then
  begin
    Exit;
  end;
  try
    LRenderer.Unmount;
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;
  try
    LRenderer.Free;
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;
  RaiseSectionFailure(LFailure);
end;

constructor TSection.Create(const AReference: TNyxViewSectionRef;
  AHost: TSectionHost; AConfigure: TSectionConfigure;
  const AObserver: INyxViewSectionObserver; ATheme: TNyxTheme);
begin
  inherited Create;

  if not AReference.Defined or not ValidSectionHost(AHost) then
  begin
    raise ENyxModel.Create('A view section requires its name and an empty attached host');
  end;
  FReference := AReference;
  FHost := AHost;
  FConfigure := AConfigure;
  FBorrowedTheme := ATheme;
  FObserver := AObserver;
end;

destructor TSection.Destroy;
var
  LRenderer: TSectionRenderer;
  {$ifndef PAS2JS}
  LPlane: TSectionHost;
  {$endif}
  LObserver: INyxViewSectionObserver;
begin
  { Pending publication ports lease this owner. They must finish/cancel before
    its final reference disappears, so no prepared view can outlive this owner. }
  { Final release must not acquire a new self interface. Mark terminal busy and
    detach every duty before observer/control finalizers can call back into this
    still-entered object. Nested Prepare/Close then refuse without resurrection. }
  FBusy := True;
  LRenderer := FRenderer;
  FRenderer := nil;
  {$ifndef PAS2JS}
  LPlane := FCurrentHost;
  FCurrentHost := nil;
  {$endif}
  LObserver := FObserver;
  FObserver := nil;
  try
    try
      LObserver := nil;
    except
      { Observer destructors have the same nonthrowing extension obligation. }
    end;
    try
      RetireSectionRenderer(LRenderer);
    except
      { Explicit Close reports fallible retirement; final release cannot. }
    end;
    {$ifndef PAS2JS}
    try
      FreeSectionParking(LPlane);
    except
      { Still attempt every owned duty before the destructor epilogue. }
    end;
    {$endif}
  finally
    inherited Destroy;
  end;
end;

function TSection.GetReference: TNyxViewSectionRef;
begin
  Result := FReference;
end;

function TSection.GetRevision: Integer;
begin
  Result := FRevision;
end;

function TSection.GetRoot: TNyxNode;
begin
  Result := nil;

  if FRenderer <> nil then
  begin
    Result := FRenderer.Root;
  end;
end;

function TSection.GetRenderer: TSectionRenderer;
begin
  Result := FRenderer;
end;

procedure TSection.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LLease: INyxViewSection;
  LObserver: INyxViewSectionObserver;
begin
  { Candidate IDs can equal accepted IDs. Exact node ownership, not text
    identity alone, prevents staging observations from reaching the caller. }

  if not FBusy and (FRenderer <> nil) and (FObserver <> nil) and
    (FRenderer.Root <> nil) then
  begin
    LLease := Self as INyxViewSection;

    if LLease.Root.Find(ANode.ID) <> ANode then
    begin
      Exit;
    end;
    LObserver := FObserver;
    FBusy := True;
    try
      LObserver.Changed(ANode, AEvent);
    finally
      FBusy := False;
      LObserver := nil;
      LLease := nil;
    end;
  end;
end;

function TSection.Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
  ADesignMode: Boolean; AState: TNyxState): INyxViewSectionChange;
var
  LPublication: INyxViewSectionPublication;
begin

  if FBusy or (FRevision = MaxInt) or
    ((FRenderer <> nil) and not FRenderer.SectionPublicationReady) then
  begin
    raise ENyxModel.Create('A view section cannot prepare during dispatch/publication or at revision limit');
  end;
  { Borrowing a store owned by the retiring renderer would dangle on commit.
    Explicit caller-owned stores may be shared across successive views. }

  if (AState <> nil) and (FRenderer <> nil) and
    (AState = FRenderer.State) and (AState <> FBorrowedState) then
  begin
    raise ENyxModel.Create('A replacement cannot borrow its retiring renderer-owned store');
  end;
  LPublication := TSectionPublication.Create(Self, ADocument, ARoot, ADesignMode, AState);
  Result := NewNyxViewSectionChange(LPublication);
end;

procedure TSection.Close;
var
  LLease: INyxViewSection;
  LRenderer: TSectionRenderer;
  {$ifndef PAS2JS}
  LPlane: TSectionHost;
  {$endif}
  LFailure: TSectionFailure;
begin

  if FBusy or (FRevision = MaxInt) or
    ((FRenderer <> nil) and not FRenderer.SectionPublicationReady) then
  begin
    raise ENyxModel.Create('A view section cannot close during dispatch/publication or at revision limit');
  end;
  { A native destructor can synchronously call an extension. Retain this exact
    section, revoke reentry and detach BOTH owned duties before the first such
    callout. No later cleanup may read fields installed by a nested successor. }
  LLease := Self as INyxViewSection;
  FBusy := True;
  Inc(FRevision);
  FBorrowedState := nil;
  LRenderer := FRenderer;
  FRenderer := nil;
  {$ifndef PAS2JS}
  LPlane := FCurrentHost;
  FCurrentHost := nil;
  {$endif}
  LFailure := Default(TSectionFailure);
  try
    try
      RetireSectionRenderer(LRenderer);
    except
      CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
    end;
    {$ifndef PAS2JS}
    try
      FreeSectionParking(LPlane);
    except
      CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
    end;
    {$endif}
  finally
    FBusy := False;
    { The callback may have released its last external section interface. No
      Self access follows this final local lease release, even if it destroys
      the now-empty section or reports another disposal failure. }
    try
      LLease := nil;
    except
      CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
    end;
  end;

  RaiseSectionFailure(LFailure);
end;

constructor TSectionPublication.Create(AOwner: TSection; ADocument: TNyxDocument;
  ARoot: TNyxNode; ADesignMode: Boolean; AState: TNyxState);
begin
  inherited Create;
  FOwner := AOwner;
  FLease := AOwner as INyxViewSection;
  FExpectedRevision := AOwner.FRevision;
  FExpectedSchemaRevision := NyxSchemaRevision;
  FExpectedParent := SectionParent(AOwner.FHost);
  FExpectedWidth := SectionWidth(AOwner.FHost);
  FExpectedHeight := SectionHeight(AOwner.FHost);
  FBorrowedState := AState;
  {$ifdef PAS2JS}
  FStageHost := NewSectionParking(AOwner.FHost);
  FPreviousHost := NewSectionParking(AOwner.FHost);
  {$else}
  FStageHost := NewSectionPlane(AOwner.FHost, AOwner);
  {$endif}
  FCandidate := TSectionRenderer.Create(AOwner.FBorrowedTheme);

  if Assigned(AOwner.FConfigure) then
  begin
    AOwner.FConfigure(FCandidate);
  end;
  FCandidate.OnEvent := {$ifdef PAS2JS}@{$endif}AOwner.Changed;
  { Ordinary Render performs document/schema/recipe/default admission and
    independent realization. No hand-written substitute widget toolkit. }
  FCandidate.Render(ADocument, ARoot, FStageHost, ADesignMode, AState);
end;

destructor TSectionPublication.Destroy;
begin
  { Implicit release must not escape a destructor: FPC would then skip its
    FreeInstance epilogue. Explicit Cancel reports the SAME cleanup separately. }
  try
    try
      RetireCandidate;
    except
      { Every exact duty was attempted; destruction itself remains nonthrowing. }
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TSectionPublication.RetireCandidate;
var
  LLease: INyxViewSection;
  LCandidate: TSectionRenderer;
  LPrevious: TSectionRenderer;
  LStageHost: TSectionHost;
  LPreviousHost: TSectionHost;
  LFailure: TSectionFailure;
begin
  { Coordinator rollback clears move flags before releasing a rejected port.
    An unpreviewed candidate has never touched the accepted placement. }
  { Keep the owner alive until every exact owned plane is gone. Before commit
    the previous renderer/plane are only borrowed from that owner; rollback
    normally clears them, and cancellation must never acquire their ownership. }
  LLease := FLease;
  FLease := nil;
  LCandidate := FCandidate;
  FCandidate := nil;
  LStageHost := FStageHost;
  FStageHost := nil;
  LPrevious := nil;
  {$ifdef PAS2JS}
  { Browser rollback parking is ALWAYS owned by this proposal, including
    before commit. Its previous renderer remains borrowed until publication. }
  LPreviousHost := FPreviousHost;
  {$else}
  { The native previous plane is borrowed from the section before commit. }
  LPreviousHost := nil;
  {$endif}

  if FPublished then
  begin
    LPrevious := FPrevious;
    {$ifndef PAS2JS}
    LPreviousHost := FPreviousHost;
    {$endif}
  end;
  FPrevious := nil;
  FPreviousHost := nil;
  FOwner := nil;
  LFailure := Default(TSectionFailure);
  try
    RetireSectionRenderer(LCandidate);
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;
  try
    RetireSectionRenderer(LPrevious);
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;
  try
    FreeSectionParking(LStageHost);
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;
  try
    FreeSectionParking(LPreviousHost);
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;
  try
    LLease := nil;
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;

  RaiseSectionFailure(LFailure);
end;

function TSectionPublication.GetSection: INyxViewSection;
begin
  Result := FLease;
end;

function TSectionPublication.GetExpectedRevision: Integer;
begin
  Result := FExpectedRevision;
end;

function TSectionPublication.Check: Boolean;
begin
  Result := (FOwner <> nil) and not FPublished and not FOwner.FBusy and
    (FOwner.FRevision = FExpectedRevision) and (FCandidate <> nil) and
    (NyxSchemaRevision = FExpectedSchemaRevision) and
    FCandidate.SectionPublicationReady and
    ((FOwner.FRenderer = nil) or FOwner.FRenderer.SectionPublicationReady) and
    (SectionParent(FOwner.FHost) = FExpectedParent) and
    (SectionWidth(FOwner.FHost) = FExpectedWidth) and
    (SectionHeight(FOwner.FHost) = FExpectedHeight) and
    {$ifdef PAS2JS}
    SectionPlacementMatches(FOwner.FHost, FOwner.FRenderer);
    {$else}
    SectionPlaneMatches(FStageHost, FOwner.FHost, FOwner) and
    not FStageHost.Visible and SectionRendererMatches(FStageHost, FCandidate) and
    SectionPlacementMatches(FOwner.FHost, FOwner.FRenderer, FOwner, FOwner.FCurrentHost);
    {$endif}
end;

procedure TSectionPublication.Preview;
{$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
var
  LPhaseStarted: QWord;

  procedure RecordPhase(const AStage: String);
  var
    LNow: QWord;
  begin
    { Native diagnostic timings disclose static ASCII stages only. Borrowed
      section names, controls and authored input never enter the phase stream. }
    LNow := GetTickCount64;
    WriteLn('native-section-preview,', AStage, ',', LNow - LPhaseStarted);
    LPhaseStarted := LNow;
  end;
{$endif}
begin
  FOwner.FBusy := True;
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  LPhaseStarted := GetTickCount64;
  {$endif}
  CaptureSectionFocus(FOwner.FHost, FFormerFocus, FFormerSelection);
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  RecordPhase('focus-capture');
  {$endif}
  FPrevious := FOwner.FRenderer;
  {$ifndef PAS2JS}
  FPreviousHost := FOwner.FCurrentHost;
  {$endif}

  if FPrevious <> nil then
  begin
    FPreviousMoveAttempted := True;
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
    LPhaseStarted := GetTickCount64;
    {$endif}
    {$ifdef PAS2JS}
    FPrevious.MoveHost(FPreviousHost);
    {$else}
    FPrevious.SetPresentationPlaneVisible(FPreviousHost, False);
    {$endif}
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
    RecordPhase('old-hide-plane');
    {$endif}
  end;
  FCandidateMoveAttempted := True;
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  LPhaseStarted := GetTickCount64;
  {$endif}
  {$ifdef PAS2JS}
  FCandidate.MoveHost(FOwner.FHost);
  {$else}
  FCandidate.SetPresentationPlaneVisible(FStageHost, True);
  {$endif}
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  RecordPhase('candidate-show-plane');
  {$endif}
  { Native activation retains handles under a guarded visibility change; the
    ordinary Sync still owns final layout and registered target-updater checks.
    Browser MoveHost synchronizes itself, so its hook does not repeat Sync. }
  FinishSectionPreview(FCandidate);
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  RecordPhase('finish-sync');
  {$endif}
  {$ifndef PAS2JS}
  { Target callbacks have finished. No late geometry/schema/slot change may
    turn the original candidate into authority over another allocation. }

  if (FOwner.FRevision <> FExpectedRevision) or
    (NyxSchemaRevision <> FExpectedSchemaRevision) or
    (SectionParent(FOwner.FHost) <> FExpectedParent) or
    (SectionWidth(FOwner.FHost) <> FExpectedWidth) or
    (SectionHeight(FOwner.FHost) <> FExpectedHeight) or
    not SectionRendererMatches(FStageHost, FCandidate) or
    not SectionPlacementMatches(FOwner.FHost, FPrevious, FOwner,
      FPreviousHost, FStageHost) then
  begin
    raise ENyxModel.Create('Native section allocation changed during physical preview');
  end;
  {$endif}
end;

procedure TSectionPublication.Rollback;
begin
  try

    if FCandidateMoveAttempted then
    begin
      {$ifdef PAS2JS}
      FCandidate.MoveHost(FStageHost);
      {$else}
      FCandidate.SetPresentationPlaneVisible(FStageHost, False);
      {$endif}
    end;
  finally
    try

      if FPreviousMoveAttempted then
      begin
        {$ifdef PAS2JS}
        FPrevious.MoveHost(FOwner.FHost);
        {$else}
        FPrevious.RestorePresentationPlane(FPreviousHost,
          FFormerFocus, FFormerSelection);
        {$endif}
      end;
      {$ifdef PAS2JS}
      RestoreSectionFocus(FFormerFocus, FFormerSelection);
      {$endif}
    finally
      FPrevious := nil; { still owned by the accepted section }
      {$ifndef PAS2JS}
      FPreviousHost := nil; { still owned by the same accepted section }
      {$endif}
      FPreviousMoveAttempted := False;
      FCandidateMoveAttempted := False;
      FOwner.FBusy := False;
    end;
  end;
end;

procedure TSectionPublication.Publish;
begin
  { Check guarded overflow/reentrancy; assignments allocate no target work.
    Former renderer remains owned HERE until the entire group publishes. }
  FOwner.FRenderer := FCandidate;
  FCandidate := nil;
  {$ifndef PAS2JS}
  FOwner.FCurrentHost := FStageHost;
  FStageHost := nil;
  {$endif}
  FOwner.FBorrowedState := FBorrowedState;
  Inc(FOwner.FRevision);
  FOwner.FBusy := False;
  FCandidateMoveAttempted := False;
  FPreviousMoveAttempted := False;
  FPublished := True;
end;

procedure TSectionPublication.RetirePrevious;
var
  LPrevious: TSectionRenderer;
  LPreviousHost: TSectionHost;
  LFailure: TSectionFailure;
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  LPhaseStarted: QWord;
  {$endif}
begin
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  { Retain the actual destruction boundary without changing exception handling
    or introducing a second cleanup path into accepted section publication. }
  LPhaseStarted := GetTickCount64;
  {$endif}
  LPrevious := FPrevious;
  FPrevious := nil;
  LPreviousHost := FPreviousHost;
  FPreviousHost := nil;
  LFailure := Default(TSectionFailure);
  try
    RetireSectionRenderer(LPrevious);
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;
  try
    FreeSectionParking(LPreviousHost);
  except
    CaptureSectionFailure(LFailure{$ifdef PAS2JS}, JSExceptValue{$endif});
  end;

  RaiseSectionFailure(LFailure);
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  WriteLn('native-section-retire,previous-free,', GetTickCount64 - LPhaseStarted);
  {$endif}
end;
