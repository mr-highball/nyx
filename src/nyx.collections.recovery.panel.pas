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
unit nyx.collections.recovery.panel;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.state, nyx.controls, nyx.collections.recovery.controls;

type
  { Closed actions name intent, not a control index or recovery authority. }
  TNyxRecoveryAction = (nraPrevious, nraNext, nraReviewHeld, nraReviewList,
    nraExport, nraDiscard, nraRetry);
  { Open text values remain separate from the closed role they fill. }
  TNyxRecoveryTextPart = (nrtSummary, nrtOutcome, nrtDiscardCaption);
  { Typed presentation flags do not grant source/clipboard/discard capability. }
  TNyxRecoveryFlag = (nrfVisible, nrfReviewVisible, nrfPrevious, nrfNext,
    nrfReviewHeld, nrfReviewList, nrfExport, nrfDiscard, nrfRetry, nrfIdle);

  { Value-only authored part identities. Define on the design's state before
    mounting, then bind that application's independent runtime mount. Review
    fields use the existing Card; every state address exports as ordinary Nyx. }
  TNyxCollectionRecoveryParts = record
  private
    FName: TNyxText;
  public
    procedure Validate;
    procedure Define(AState: TNyxState);
    function Text(APart: TNyxRecoveryTextPart): TNyxTextStateRef;
    function Flag(APart: TNyxRecoveryFlag): TNyxBooleanStateRef;
    function ActionID(AAction: TNyxRecoveryAction): TNyxText;
    function Review: TNyxRecoveredDraftBindings;
    property RootID: TNyxText read FName;
  end;

  { Portable managed compound. Named review/action parts are ordinary controls
    that callers can theme, replace, rebind to resources or rearrange. No hidden
    runtime callbacks/receipts/journals are serialized inside the component. }
  INyxCollectionRecoveryPanel = interface(INyxCard)
    ['{931AE504-5F6C-45AB-A72B-330BCE74A2DC}']
    function GetParts: TNyxCollectionRecoveryParts;
    property Parts: TNyxCollectionRecoveryParts read GetParts;
  end;

function NyxCollectionRecoveryParts(const AName: TNyxText): TNyxCollectionRecoveryParts;
function NewNyxCollectionRecoveryPanel(const AParts: TNyxCollectionRecoveryParts):
  INyxCollectionRecoveryPanel;

implementation

uses SysUtils, nyx.types, nyx.responsive, nyx.collections;

const
  CTextNames: array[TNyxRecoveryTextPart] of TNyxText =
    ('summary', 'outcome', 'discard-caption');
  CFlagNames: array[TNyxRecoveryFlag] of TNyxText =
    ('visible', 'review-visible', 'previous-enabled', 'next-enabled',
    'held-visible', 'list-visible', 'export-enabled', 'discard-enabled', 'retry-visible', 'idle');
  CActionNames: array[TNyxRecoveryAction] of TNyxText =
    ('previous', 'next', 'review-held', 'review-list', 'export', 'discard', 'retry');
  CActionCaptions: array[TNyxRecoveryAction] of TNyxText =
    ('Previous edit', 'Next edit', 'Review held edit', 'Review recovered edits',
    'Export reviewed text', 'Discard reviewed edit', 'Retry display');

type
  { Owns copied part identities and ordinary refcounted children only. }
  TRecoveryPanel = class(TNyxCard, INyxCollectionRecoveryPanel)
  private
    FParts: TNyxCollectionRecoveryParts;
  public
    constructor Create(const AParts: TNyxCollectionRecoveryParts); reintroduce;
    function GetParts: TNyxCollectionRecoveryParts;
  end;

function NyxCollectionRecoveryParts(const AName: TNyxText): TNyxCollectionRecoveryParts;
begin
  Result.FName := AName;
  Result.Validate;
end;

procedure RequirePart(AValue, AMaximum: Integer);
begin

  if (AValue < 0) or (AValue > AMaximum) then
  begin
    raise EArgumentException.Create('Unknown recovery panel part');
  end;
end;

procedure TNyxCollectionRecoveryParts.Validate;
begin

  NyxCollection(FName);
end;

function TNyxCollectionRecoveryParts.Text(APart: TNyxRecoveryTextPart): TNyxTextStateRef;
begin

  RequirePart(Ord(APart), Ord(High(TNyxRecoveryTextPart)));
  Result := NyxTextState(FName + '-' + CTextNames[APart]);
end;

function TNyxCollectionRecoveryParts.Flag(APart: TNyxRecoveryFlag): TNyxBooleanStateRef;
begin

  RequirePart(Ord(APart), Ord(High(TNyxRecoveryFlag)));
  Result := NyxBooleanState(FName + '-' + CFlagNames[APart]);
end;

function TNyxCollectionRecoveryParts.ActionID(AAction: TNyxRecoveryAction): TNyxText;
begin

  RequirePart(Ord(AAction), Ord(High(TNyxRecoveryAction)));
  Result := FName + '-' + CActionNames[AAction];
end;

function TNyxCollectionRecoveryParts.Review: TNyxRecoveredDraftBindings;
begin
  Result.Heading := NyxTextState(FName + '-review-heading');
  Result.Provenance := NyxTextState(FName + '-review-provenance');
  Result.Reason := NyxTextState(FName + '-review-reason');
  Result.Notice := NyxTextState(FName + '-review-notice');
  Result.Draft := NyxTextState(FName + '-review-draft');
  Result.Accepted := NyxTextState(FName + '-review-accepted');
  Result.Visible := Flag(nrfReviewVisible);
end;

procedure TNyxCollectionRecoveryParts.Define(AState: TNyxState);
var
  LReview: TNyxRecoveredDraftBindings;
begin
  Validate;

  if AState = nil then
  begin
    raise EArgumentNilException.Create('Recovery parts need a state store');
  end;
  LReview := Review;
  AState.Apply([
    NyxStateValue(Text(nrtSummary), ''), NyxStateValue(Text(nrtOutcome), ''),
    NyxStateValue(Text(nrtDiscardCaption), 'Discard reviewed edit'),
    NyxStateValue(Flag(nrfVisible), False), NyxStateValue(Flag(nrfReviewVisible), False),
    NyxStateValue(Flag(nrfPrevious), False), NyxStateValue(Flag(nrfNext), False),
    NyxStateValue(Flag(nrfReviewHeld), False), NyxStateValue(Flag(nrfReviewList), False),
    NyxStateValue(Flag(nrfExport), False), NyxStateValue(Flag(nrfDiscard), False),
    NyxStateValue(Flag(nrfRetry), False), NyxStateValue(Flag(nrfIdle), False),
    NyxStateValue(LReview.Heading, ''), NyxStateValue(LReview.Provenance, ''),
    NyxStateValue(LReview.Reason, ''), NyxStateValue(LReview.Notice, ''),
    NyxStateValue(LReview.Draft, ''), NyxStateValue(LReview.Accepted, '')]);
end;

constructor TRecoveryPanel.Create(const AParts: TNyxCollectionRecoveryParts);
var
  LCommands: INyxRow;
  LAction: TNyxRecoveryAction;
  LButton: INyxButton;
begin
  AParts.Validate;
  inherited Create(AParts.RootID);
  FParts := AParts;
  GetConfigure.Layout(nlColumn).Padding(12).Gap(8).Compound(True)
    .AccessibleName('Unfinished edits').Done;
  GetBinds.Visible(AParts.Flag(nrfVisible)).Done;
  Add(NewNyxHeading(AParts.RootID + '-title').WithText('Unfinished edits'));
  Add(NewNyxLabel(AParts.Text(nrtSummary).Name).Binds.Text(AParts.Text(nrtSummary)).Done);
  Add(NewNyxBoundRecoveredDraftReview(AParts.RootID + '-review', AParts.Review));
  LCommands := NewNyxRow(AParts.RootID + '-actions');
  Add(LCommands);
  LCommands.Configure.Gap(8).WhenViewport(TNyxViewportWidth.Below(560)).Layout(nlColumn).Done;
  LCommands.Binds.Enabled(AParts.Flag(nrfIdle)).Done;
  for LAction := Low(TNyxRecoveryAction) to High(TNyxRecoveryAction) do
  begin
    LButton := NewNyxButton(AParts.ActionID(LAction)).WithText(CActionCaptions[LAction]);
    LButton.Configure.PartName(NyxPart(CActionNames[LAction])).Done;
    LCommands.Add(LButton);
    case LAction of
      nraPrevious:
      begin
        LButton.Binds.Enabled(AParts.Flag(nrfPrevious)).Done;
      end;
      nraNext:
      begin
        LButton.Binds.Enabled(AParts.Flag(nrfNext)).Done;
      end;
      nraReviewHeld:
      begin
        LButton.Binds.Visible(AParts.Flag(nrfReviewHeld)).Done;
      end;
      nraReviewList:
      begin
        LButton.Binds.Visible(AParts.Flag(nrfReviewList)).Done;
      end;
      nraExport:
      begin
        LButton.Binds.Enabled(AParts.Flag(nrfExport)).Done;
      end;
      nraDiscard:
      begin
        LButton.Binds.Enabled(AParts.Flag(nrfDiscard)).Text(AParts.Text(nrtDiscardCaption)).Done;
      end;
      nraRetry:
      begin
        LButton.Binds.Visible(AParts.Flag(nrfRetry)).Done;
      end;
    end;
  end;
  Add(NewNyxLabel(AParts.Text(nrtOutcome).Name).Binds.Text(AParts.Text(nrtOutcome))
    .AccessibleName(AParts.Text(nrtOutcome)).Done);
end;

function TRecoveryPanel.GetParts: TNyxCollectionRecoveryParts;
begin
  Result := FParts;
end;

function NewNyxCollectionRecoveryPanel(const AParts: TNyxCollectionRecoveryParts):
  INyxCollectionRecoveryPanel;
begin
  Result := TRecoveryPanel.Create(AParts);
end;

end.
