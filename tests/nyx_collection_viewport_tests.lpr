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
program nyx_collection_viewport_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.text, nyx.collections, nyx.collections.arrangement,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.selection,
  nyx.collections.mount, nyx.collections.viewport, nyx.collections.inspection
  {$ifdef PAS2JS}, Web{$endif};

type
  { Portable target probe qualifies admission/retirement only. It deliberately
    has no widget or pixel assertions; actual browser/LCL consumers own those.
    Reentrant replacement models a target callout superseding its own command. }
  TViewportMount = class(TNyxCollectionMountBase)
  public
    Deferred: Boolean;
    RetireOnApply: Boolean;
    SupersedeOnApply: Boolean;
    ReplacementItem: TNyxItemRef;
    ReplacementReceipt: INyxCollectionReveal;
    Applied: Integer;
    TookFocus: Integer;
    destructor Destroy; override;
  protected
    procedure RenderDataset; override;
    procedure DetachTarget; override;
    function SupportsViewportReveal: Boolean; override;
    function RevealViewportTarget(const AOptions: TNyxCollectionRevealOptions;
      ARow, AColumn: Integer; ATakeFocus: Boolean; out AFullyVisible: Boolean;
      out AReason: TNyxCollectionRevealReason): TNyxCollectionRevealStatus; override;
  end;

var
  GChecks: Integer;
  GDestroyed: Integer;

const
  CUnicodeItem: TNyxText = 'worker-🌙';

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

destructor TViewportMount.Destroy;
begin
  inherited Destroy;
  Inc(GDestroyed);
end;

procedure TViewportMount.RenderDataset;
begin
  { The portable fixture installs no physical rows. }
end;

procedure TViewportMount.DetachTarget;
begin
  { There is no borrowed widget to detach. }
end;

function TViewportMount.SupportsViewportReveal: Boolean;
begin
  Result := True;
end;

function TViewportMount.RevealViewportTarget(const AOptions: TNyxCollectionRevealOptions;
  ARow, AColumn: Integer; ATakeFocus: Boolean; out AFullyVisible: Boolean;
  out AReason: TNyxCollectionRevealReason): TNyxCollectionRevealStatus;
begin
  AFullyVisible := False;
  AReason := ncvrNone;

  if SupersedeOnApply then
  begin
    SupersedeOnApply := False;
    ReplacementReceipt := CaptureViewport.Reveal(NyxRevealItem(ReplacementItem));
    Exit(ncvsCompleted);
  end;

  if RetireOnApply then
  begin
    Disconnect;
    Exit(ncvsCompleted);
  end;

  if Deferred then
  begin
    Exit(ncvsPending);
  end;
  Inc(Applied);

  if ATakeFocus then
  begin
    Inc(TookFocus);
  end;
  AFullyVisible := True;
  Result := ncvsCompleted;
end;

procedure PureValues;
var
  LItem: TNyxItemRef;
  LOptions: TNyxCollectionRevealOptions;
  LCopy: TNyxCollectionRevealOptions;
  LState: TNyxCollectionRevealState;
  LReceipt: INyxCollectionReveal;
  LRefused: Boolean;
begin
  LItem := NyxItem(NyxCollection('services'), CUnicodeItem);
  LOptions := NyxRevealItem(LItem).InField(NyxTextField('Notes'));
  LCopy := LOptions.Vertical(ncvaCenter).Horizontal(ncvaEnd).Focus(ncvfTake);
  Check(LOptions.Valid and (LOptions.VerticalAlignment = ncvaNearest) and
    (LOptions.FocusPolicy = ncvfPreserve), 'fluent copies preserve the original default intent');
  Check(LCopy.Field.Same(NyxColumnIdentity(NyxTextField('Notes'))) and
    (LCopy.Item.ID = CUnicodeItem) and (LCopy.FocusPolicy = ncvfTake),
    'typed field family and exact Unicode identity survive fluent copying');
  Check(not Default(TNyxCollectionRevealOptions).Valid, 'absent item is not a request');
  LState := TNyxCollectionRevealState.Create(LOptions, 9);
  LReceipt := LState;
  Check(LReceipt.Pending and (LReceipt.Capture.Serial = 9), 'a new independent receipt starts pending');
  LReceipt.Cancel;
  Check(not LState.Finish(ncvsCompleted, ncvrNone, True) and
    (LReceipt.Capture.Status = ncvsCancelled) and not LReceipt.Capture.FullyVisible,
    'late completion cannot overwrite caller cancellation');
  LState := TNyxCollectionRevealState.Create(LOptions);
  LReceipt := LState;
  LState.Finish(ncvsCompleted, ncvrNone, True);
  LReceipt.Cancel;
  Check((LReceipt.Capture.Status = ncvsCompleted) and LReceipt.Capture.FullyVisible,
    'completed observations remain terminal after late cancellation');
  Check(NyxCollectionRevealDelta(12, 10, 0, 50, ncvaNearest) = 0,
    'nearest leaves fully visible content still');
  Check(NyxCollectionRevealDelta(-5, 10, 0, 50, ncvaNearest) = -5,
    'nearest reveals the preceding edge');
  Check(NyxCollectionRevealDelta(45, 10, 0, 50, ncvaNearest) = 5,
    'nearest reveals the following edge');
  Check(NyxCollectionRevealDelta(-20, 100, 0, 50, ncvaNearest) = 0,
    'an oversized item spanning the viewport does not oscillate');
  Check(NyxCollectionRevealDelta(10, 100, 0, 50, ncvaNearest) = 10,
    'an oversized following item uses its nearer leading edge');
  Check(NyxCollectionRevealDelta(-100, 80, 0, 50, ncvaNearest) = -70,
    'an oversized preceding item uses its nearer trailing edge');
  Check((NyxCollectionRevealDelta(100, 20, 0, 50, ncvaStart) = 100) and
    (NyxCollectionRevealDelta(100, 20, 0, 50, ncvaCenter) = 85) and
    (NyxCollectionRevealDelta(100, 20, 0, 50, ncvaEnd) = 70),
    'explicit alignments use the same logical coordinate plane');
  LRefused := False;
  try
    NyxCollectionRevealDelta(0, 10, 0, 0, ncvaNearest);
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'a wholly unavailable viewport refuses rather than dividing or guessing');
  LRefused := False;
  try
    NyxCollectionRevealDelta(0, Infinity, 0, 50, ncvaNearest);
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nonfinite target extents refuse before scroll arithmetic');
end;

procedure MountAdmission;
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LTarget: TViewportMount;
  LPort: INyxCollectionViewport;
  LRequest: INyxCollectionReveal;
  LSecond: INyxCollectionReveal;
  LKey: TNyxCollectionRef;
  LOne: TNyxItemRef;
  LTwo: TNyxItemRef;
  LSelection: INyxCollectionSelection;
  LInspection: INyxCollectionInspection;
  LStaged: INyxCollectionMount;
  LStagedTarget: TViewportMount;
  LStagedPort: INyxCollectionViewport;
  LBefore: Integer;
begin
  LKey := NyxCollection('viewport-owner');
  LOne := NyxItem(LKey, 'one');
  LTwo := NyxItem(LKey, 'two');
  LStore := NewNyxCollection(LKey,
    NyxCollectionSchema.Text(NyxTextField('name'), '').Text(NyxTextField('private'), ''),
    [NyxCollectionItem(LOne).WithValue(NyxTextField('name'), 'One'),
     NyxCollectionItem(LTwo).WithValue(NyxTextField('name'), 'Two')]);
  LView := NewNyxCollectionView(LStore,
    NyxCollectionView(LKey).Column(NyxTextField('name'), 'Name')
      .Column(NyxTextField('private'), 'Private')
      .ColumnLayout(NyxColumnLayout.Visible(False)).TableLayout(NyxTableLayout), cpTable);
  LTarget := TViewportMount.Create(LView);
  LMount := LTarget;
  LTarget.Activate;
  LMount.Select(LOne);
  LSelection := LView.Selection;
  LPort := NyxCollectionViewport(LMount);
  Check((LPort <> nil) and LPort.Active, 'connected mounts expose an independent active port');
  LRequest := LPort.Reveal(NyxRevealItem(LTwo).InField(NyxTextField('name')));
  Check((LRequest.Capture.Status = ncvsCompleted) and (LView.Selection = LSelection),
    'default reveal preserves both selection membership and cursor');
  LRequest := LPort.Reveal(NyxRevealItem(LTwo).Focus(ncvfTake));
  Check((LRequest.Capture.Status = ncvsCompleted) and (LView.Selection.Focus.ID = 'two') and
    (LView.Selection.Count = 1) and LView.Selection.Contains(LOne) and (LTarget.TookFocus = 1),
    'explicit focus moves only the cursor, preserving existing membership');
  LRequest := LPort.Reveal(NyxRevealItem(NyxItem(LKey, 'missing')));
  Check(LRequest.Capture.Reason = ncvrNotDisplayed, 'a missing row does not change a query to manufacture a match');
  LRequest := LPort.Reveal(NyxRevealItem(LOne).InField(NyxIntegerField('name')));
  Check(LRequest.Capture.Reason = ncvrUnknownField, 'a wrong scalar family cannot impersonate a visible field');
  LRequest := LPort.Reveal(NyxRevealItem(NyxItem(NyxCollection('other'), 'one')));
  Check(LRequest.Capture.Reason = ncvrInvalidItem,
    'matching row text in a foreign collection cannot borrow this viewport');
  LRequest := LPort.Reveal(NyxRevealItem(LOne).InField(NyxTextField('private')));
  Check(LRequest.Capture.Reason = ncvrHiddenField,
    'reveal never shows an explicitly hidden field');
  LTarget.SupersedeOnApply := True;
  LTarget.ReplacementItem := LTwo;
  LRequest := LPort.Reveal(NyxRevealItem(LOne));
  Check((LRequest.Capture.Reason = ncvrSuperseded) and
    LTarget.ReplacementReceipt.Pending,
    'a reentrant newer request defeats the original target success return');
  LMount.Refresh;
  Check(LTarget.ReplacementReceipt.Capture.Status = ncvsCompleted,
    'the newer request receives its own later target application');
  LTarget.ReplacementReceipt := nil;
  LTarget.Deferred := True;
  LRequest := LPort.Reveal(NyxRevealItem(LOne));
  LSecond := LPort.Reveal(NyxRevealItem(LTwo));
  Check((LRequest.Capture.Reason = ncvrSuperseded) and LSecond.Pending,
    'a newer viewport request supersedes the old receipt independently');
  LSecond.Cancel;
  LTarget.Deferred := False;
  LMount.Refresh;
  Check(LSecond.Capture.Status = ncvsCancelled, 'a later target wake cannot revive cancelled intent');
  LTarget.Deferred := True;
  LRequest := LPort.Reveal(NyxRevealItem(LOne));
  LStore.Update(NyxCollectionItem(LOne).WithValue(NyxTextField('name'), 'Changed'));
  Check(LRequest.Capture.Reason = ncvrChanged, 'changed displayed snapshots cancel an uncompleted request');
  Check(Supports(LView, INyxCollectionInspection, LInspection),
    'the same mounted view exposes independent inspection');
  LInspection.ConfigureInspection(NyxInspectionPolicy);
  LInspection.PauseInspection;
  LRequest := LPort.Reveal(NyxRevealItem(LOne));
  LStore.Update(NyxCollectionItem(LOne).WithValue(NyxTextField('name'), 'Incoming while paused'));
  Check(LRequest.Pending, 'a paused producer update does not invalidate unchanged displayed intent');
  LTarget.Deferred := False;
  LMount.Refresh;
  Check(LRequest.Capture.Status = ncvsCompleted,
    'the retained paused frame can complete a viewport request independently');
  LInspection.ResumeInspection;
  LStagedTarget := TViewportMount.Create(LView, ncipStaged);
  LStaged := LStagedTarget;
  LStagedTarget.Activate;
  LStagedPort := NyxCollectionViewport(LStaged);
  LSecond := LStagedPort.Reveal(NyxRevealItem(LOne));
  Check(LSecond.Capture.Reason = ncvrInputUnavailable,
    'a staged target has no input authority for viewport movement');
  LStagedTarget.InstallInput;
  Check(LSecond.Capture.Status = ncvsRefused,
    'installing staged input does not revive a refused receipt');
  LStaged.Disconnect;
  LStaged := nil;
  LMount.SetInteraction(False, False);
  LRequest := LPort.Reveal(NyxRevealItem(LOne));
  LMount.SetInteraction(True, False);
  Check(LRequest.Capture.Reason = ncvrInputUnavailable, 'enable cannot revive an originally disabled request');
  LTarget.RetireOnApply := True;
  LRequest := LPort.Reveal(NyxRevealItem(LOne));
  Check(not LPort.Active and (LRequest.Capture.Status = ncvsCancelled) and
    (LRequest.Capture.Reason = ncvrDisconnected), 'target retirement wins over a later success return');
  LRequest := LPort.Reveal(NyxRevealItem(LOne));
  Check(LRequest.Capture.Reason = ncvrDisconnected, 'retained dead ports return an inert typed refusal');
  LBefore := GDestroyed;
  LMount := nil;
  Check(GDestroyed = LBefore + 1, 'retained port and receipt do not keep their mount alive');
  Check(not LPort.Active and (LRequest.Capture.Options.Item.ID = 'one'),
    'copied receipt facts remain safe after final mount disposal');
end;

begin
  try
    PureValues;
    MountAdmission;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-viewport-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable collection viewport checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-viewport-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
