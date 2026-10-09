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
  TSection = class;
  TSectionPublication = class(TInterfacedObject, INyxViewSectionPublication)
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
  end;

  TSection = class(TInterfacedObject,
    {$ifdef PAS2JS}INyxBrowserViewSection,{$else}INyxLCLViewSection,{$endif}
    INyxViewSection)
  private
    FReference: TNyxViewSectionRef;
    FRevision: Integer;
    FHost: TSectionHost;
    FRenderer: TSectionRenderer;
    FBorrowedState: TNyxState;
    FConfigure: TSectionConfigure;
    FObserver: INyxViewSectionObserver;
    FBusy: Boolean;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  public
    constructor Create(const AReference: TNyxViewSectionRef; AHost: TSectionHost;
      AConfigure: TSectionConfigure; const AObserver: INyxViewSectionObserver);
    destructor Destroy; override;
    function GetReference: TNyxViewSectionRef;
    function GetRevision: Integer;
    function GetRoot: TNyxNode;
    function GetRenderer: TSectionRenderer;
    function Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
      ADesignMode: Boolean; AState: TNyxState): INyxViewSectionChange;
    procedure Close;
  end;

constructor TSection.Create(const AReference: TNyxViewSectionRef;
  AHost: TSectionHost; AConfigure: TSectionConfigure;
  const AObserver: INyxViewSectionObserver);
begin
  inherited Create;

  if not AReference.Defined or not ValidSectionHost(AHost) then
  begin
    raise ENyxModel.Create('A view section requires its name and an empty attached host');
  end;
  FReference := AReference;
  FHost := AHost;
  FConfigure := AConfigure;
  FObserver := AObserver;
end;

destructor TSection.Destroy;
begin
  { Pending publication ports lease this owner. They must finish/cancel before
    its final reference disappears, so no prepared view can outlive this owner. }
  FObserver := nil;
  FreeAndNil(FRenderer);
  inherited Destroy;
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
begin

  if FBusy or (FRevision = MaxInt) or
    ((FRenderer <> nil) and not FRenderer.SectionPublicationReady) then
  begin
    raise ENyxModel.Create('A view section cannot close during dispatch/publication or at revision limit');
  end;
  Inc(FRevision);
  FBorrowedState := nil;
  FreeAndNil(FRenderer);
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
  FStageHost := NewSectionParking(AOwner.FHost);
  FPreviousHost := NewSectionParking(AOwner.FHost);
  FCandidate := TSectionRenderer.Create;

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
  { Coordinator rollback clears move flags before releasing a rejected port.
    An unpreviewed candidate has never touched the accepted placement. }
  FreeAndNil(FCandidate);
  FreeAndNil(FPrevious);
  FreeSectionParking(FStageHost);
  FreeSectionParking(FPreviousHost);
  FLease := nil;
  inherited Destroy;
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
  Result := not FPublished and not FOwner.FBusy and
    (FOwner.FRevision = FExpectedRevision) and (FCandidate <> nil) and
    (NyxSchemaRevision = FExpectedSchemaRevision) and
    FCandidate.SectionPublicationReady and
    ((FOwner.FRenderer = nil) or FOwner.FRenderer.SectionPublicationReady) and
    (SectionParent(FOwner.FHost) = FExpectedParent) and
    (SectionWidth(FOwner.FHost) = FExpectedWidth) and
    (SectionHeight(FOwner.FHost) = FExpectedHeight) and
    SectionPlacementMatches(FOwner.FHost, FOwner.FRenderer);
end;

procedure TSectionPublication.Preview;
begin
  FOwner.FBusy := True;
  CaptureSectionFocus(FOwner.FHost, FFormerFocus, FFormerSelection);
  FPrevious := FOwner.FRenderer;

  if FPrevious <> nil then
  begin
    FPreviousMoveAttempted := True;
    FPrevious.MoveHost(FPreviousHost);
  end;
  FCandidateMoveAttempted := True;
  FCandidate.MoveHost(FOwner.FHost);
  { Native MoveHost allocates/layouts; ordinary Sync also exercises registered
    target updaters. Browser MoveHost synchronizes itself; a second Sync is
    harmless but unnecessary, so the adapter supplies this target hook. }
  FinishSectionPreview(FCandidate);
end;

procedure TSectionPublication.Rollback;
begin
  try

    if FCandidateMoveAttempted then
    begin
      FCandidate.MoveHost(FStageHost);
    end;
  finally
    try

      if FPreviousMoveAttempted then
      begin
        FPrevious.MoveHost(FOwner.FHost);
      end;
      RestoreSectionFocus(FFormerFocus, FFormerSelection);
    finally
      FPrevious := nil; { still owned by the accepted section }
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
  FOwner.FBorrowedState := FBorrowedState;
  Inc(FOwner.FRevision);
  FOwner.FBusy := False;
  FCandidateMoveAttempted := False;
  FPreviousMoveAttempted := False;
  FPublished := True;
end;

procedure TSectionPublication.RetirePrevious;
begin
  FreeAndNil(FPrevious);
end;
