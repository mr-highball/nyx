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
unit nyx.collections.recovery.controls;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.state, nyx.controls, nyx.collections.edits, nyx.collections.recovery;

const
  NyxRecoveredDraftHeadingPart = 'recovery-heading';
  NyxRecoveredDraftProvenancePart = 'recovery-provenance';
  NyxRecoveredDraftReasonPart = 'recovery-reason';
  NyxRecoveredDraftNoticePart = 'recovery-notice';
  NyxRecoveredDraftTextPart = 'recovery-draft-text';
  NyxRecoveredDraftAcceptedPart = 'recovery-accepted-text';

type
  { Copied display text only. Both frozen and state-bound reviews use these
    exact escaping/provenance rules; raw export always uses the detached cell. }
  TNyxRecoveredDraftPresentation = record
    Heading: TNyxText;
    Provenance: TNyxText;
    Reason: TNyxText;
    Notice: TNyxText;
    Draft: TNyxText;
    Accepted: TNyxText;
  end;

  { Typed state addresses for the shared review layout. The host defines them
    before mounting and publishes all fields together. No record or live editor
    is hidden inside the authored control; these bindings export normally. }
  TNyxRecoveredDraftBindings = record
    Heading: TNyxTextStateRef;
    Provenance: TNyxTextStateRef;
    Reason: TNyxTextStateRef;
    Notice: TNyxTextStateRef;
    Draft: TNyxTextStateRef;
    Accepted: TNyxTextStateRef;
    Visible: TNyxBooleanStateRef;
  end;

  { Frozen, read-only presentation of one recovered edit. Copied provenance
    grants no editing authority; the card retains no journal, mount, source,
    event router or clipboard service. Named children remain extensible through
    the ordinary Card contract. Export preserves their portable presentation,
    not the private runtime recovery identity. Cell returns a detached value. }
  INyxRecoveredDraftReview = interface(INyxCard)
    ['{3C00F127-AC10-46B4-9536-A02937CF9EC1}']
    function GetCell: TNyxCollectionCellEdit;
    function GetReason: TNyxCollectionDraftLoss;
    function GetBeforeRevision: Integer;
    function GetAfterRevision: Integer;
    property Cell: TNyxCollectionCellEdit read GetCell;
    property Reason: TNyxCollectionDraftLoss read GetReason;
    property BeforeRevision: Integer read GetBeforeRevision;
    property AfterRevision: Integer read GetAfterRevision;
  end;

{ Capture this immutable entry once into an independently owned compound.
  No entry is discarded or restored. Visible text escapes controls/backslashes;
  Cell retains exact raw text and scalar caret for an explicitly authorized host.
  Nil, malformed cells, unknown reasons and unordered revisions refuse before
  constructing the card. Review neither reads a live editor nor grants Copy. }
function NewNyxRecoveredDraftReview(const AID: TNyxText;
  const AEntry: INyxRecoveredCollectionDraft): INyxRecoveredDraftReview;

{ The same read-only Card layout with ordinary portable state bindings. It does
  not claim the frozen INyxRecoveredDraftReview interface: its current receipt
  belongs to the separate runtime binding. Every named part remains replaceable. }
function NewNyxBoundRecoveredDraftReview(const AID: TNyxText;
  const ABindings: TNyxRecoveredDraftBindings): INyxCard;
function NyxRecoveredDraftPresentation(const AEntry: INyxRecoveredCollectionDraft):
  TNyxRecoveredDraftPresentation;
{ A held draft has no admitted recovery reason/revisions. Describe that honestly
  without manufacturing a journal entry or granting permission to discard it. }
function NyxPendingDraftPresentation(const ACell: TNyxCollectionCellEdit):
  TNyxRecoveredDraftPresentation;

{ Human-readable English default for a closed recovery reason. Applications may
  replace the named reason label for localization without changing its meaning. }
function NyxCollectionDraftLossDescription(AReason: TNyxCollectionDraftLoss): TNyxText;

implementation

uses SysUtils, nyx.types, nyx.collections, nyx.collections.cell.actions;

type
  { State-bound sibling shares the frozen review's exact parts/layout. It owns
    only ordinary managed children and typed addresses, never runtime receipts. }
  TBoundDraftReview = class(TNyxCard)
  public
    constructor Create(const AID: TNyxText;
      const ABindings: TNyxRecoveredDraftBindings); reintroduce;
  end;

  { Owns copied scalar/text provenance and ordinary managed children only.
    Refcounted descendants never point back to this specialized interface;
    releasing the original journal/entry cannot invalidate its read-only view. }
  TRecoveredDraftReview = class(TNyxCard, INyxRecoveredDraftReview)
  private
    FCell: TNyxCollectionCellEdit;
    FReason: TNyxCollectionDraftLoss;
    FBeforeRevision: Integer;
    FAfterRevision: Integer;
  public
    constructor Create(const AID: TNyxText;
      const ACell: TNyxCollectionCellEdit; AReason: TNyxCollectionDraftLoss;
      ABeforeRevision, AAfterRevision: Integer); reintroduce;
    function GetCell: TNyxCollectionCellEdit;
    function GetReason: TNyxCollectionDraftLoss;
    function GetBeforeRevision: Integer;
    function GetAfterRevision: Integer;
  end;

function NyxCollectionDraftLossDescription(AReason: TNyxCollectionDraftLoss): TNyxText;
begin
  case Ord(AReason) of
    Ord(ncdlRowRemoved):
    begin
      Result := 'The record was removed.';
    end;
    Ord(ncdlIdentityReplaced):
    begin
      Result := 'The original record was replaced.';
    end;
    Ord(ncdlHidden):
    begin
      Result := 'The record is hidden by the current view.';
    end;
    Ord(ncdlValueChanged):
    begin
      Result := 'The accepted field value changed.';
    end;
    Ord(ncdlFieldUnavailable):
    begin
      Result := 'The edited field is no longer available.';
    end;
    Ord(ncdlUnknownIdentity):
    begin
      Result := 'The original record identity could not be verified.';
    end;
    else
    begin
      raise ENyxCollection.Create('Unknown recovered-edit reason');
    end;
  end;
end;

procedure PopulateReview(ACard: TNyxCard; const AID: TNyxText;
  const APresentation: TNyxRecoveredDraftPresentation);
var
  LText: INyxMemo;
begin
  ACard.GetConfigure.Layout(nlColumn).Padding(16).Gap(8)
    .AccessibleName('Recovered edit').Done;
  ACard.Add(NewNyxHeading(AID + '-heading').Configure
    .PartName(NyxPart(NyxRecoveredDraftHeadingPart)).Text(APresentation.Heading).Done);
  ACard.Add(NewNyxLabel(AID + '-provenance').Configure
    .PartName(NyxPart(NyxRecoveredDraftProvenancePart)).Text(APresentation.Provenance).Done);
  ACard.Add(NewNyxLabel(AID + '-reason').Configure
    .PartName(NyxPart(NyxRecoveredDraftReasonPart))
    .Text(APresentation.Reason).Done);
  ACard.Add(NewNyxLabel(AID + '-notice').Configure
    .PartName(NyxPart(NyxRecoveredDraftNoticePart)).Text(APresentation.Notice).Done);
  LText := NewNyxMemo(AID + '-draft');
  LText.Configure.PartName(NyxPart(NyxRecoveredDraftTextPart))
    .AccessibleName('Recovered unfinished text').Height(160).ReadOnly(True).Done;
  LText.Value := APresentation.Draft;
  ACard.Add(LText);
  LText := NewNyxMemo(AID + '-accepted');
  LText.Configure.PartName(NyxPart(NyxRecoveredDraftAcceptedPart))
    .AccessibleName('Accepted value when editing began').Height(96).ReadOnly(True).Done;
  LText.Value := APresentation.Accepted;
  ACard.Add(LText);
end;

function NyxPendingDraftPresentation(const ACell: TNyxCollectionCellEdit):
  TNyxRecoveredDraftPresentation;
begin
  ACell.Validate;
  Result := Default(TNyxRecoveredDraftPresentation);
  Result.Heading := 'Held unfinished edit';
  Result.Provenance := ACell.Item.Collection.Name + ' / ' + ACell.Item.ID + ' / ' + ACell.Field.Name;
  Result.Reason := 'Recovery capacity is full. This draft is still held by its original control.';
  Result.Notice := 'Review and export keep the original text. Discard requires confirmation. ' +
    'Controls and backslashes are escaped in both previews.';
  Result.Draft := NyxCollectionCellInspectionText(ACell.Text);
  Result.Accepted := NyxCollectionCellInspectionText(NyxCellEditWireText(ACell.Accepted));
end;

function AdmittedPresentation(const ACell: TNyxCollectionCellEdit;
  AReason: TNyxCollectionDraftLoss; ABefore, AAfter: Integer): TNyxRecoveredDraftPresentation;
begin

  if (ABefore < 0) or (AAfter < ABefore) then
  begin
    raise ENyxCollection.Create('Recovered-edit revisions must describe an ordered transition');
  end;
  Result := NyxPendingDraftPresentation(ACell);
  Result.Heading := 'Recovered edit';
  Result.Provenance := Result.Provenance + ' / source revisions ' +
    TNyxText(IntToStr(ABefore)) + ' to ' + TNyxText(IntToStr(AAfter));
  Result.Reason := NyxCollectionDraftLossDescription(AReason);
  Result.Notice := 'Your unfinished text is retained for review. This view does not apply or discard it. ' +
    'Controls and backslashes are escaped in both previews.';
end;

function NyxRecoveredDraftPresentation(const AEntry: INyxRecoveredCollectionDraft):
  TNyxRecoveredDraftPresentation;
begin

  if AEntry = nil then
  begin
    raise ENyxCollection.Create('A recovered edit is required for review');
  end;
  Result := AdmittedPresentation(AEntry.Cell, AEntry.Reason,
    AEntry.BeforeRevision, AEntry.AfterRevision);
end;

constructor TRecoveredDraftReview.Create(const AID: TNyxText;
  const ACell: TNyxCollectionCellEdit; AReason: TNyxCollectionDraftLoss;
  ABeforeRevision, AAfterRevision: Integer);
var
  LPresentation: TNyxRecoveredDraftPresentation;
begin
  inherited Create(AID);
  FCell := NyxCollectionCellEdit(ACell.Item, ACell.Field, ACell.Accepted,
    ACell.Text, ACell.Selection, False, False);
  FReason := AReason;
  FBeforeRevision := ABeforeRevision;
  FAfterRevision := AAfterRevision;
  LPresentation := AdmittedPresentation(FCell, FReason, FBeforeRevision, FAfterRevision);
  PopulateReview(Self, AID, LPresentation);
end;

constructor TBoundDraftReview.Create(const AID: TNyxText;
  const ABindings: TNyxRecoveredDraftBindings);
begin
  inherited Create(AID);
  PopulateReview(Self, AID, Default(TNyxRecoveredDraftPresentation));
  GetBinds.Visible(ABindings.Visible).AccessibleName(ABindings.Heading).Done;
  Part(NyxPart(NyxRecoveredDraftHeadingPart)).Binds.Text(ABindings.Heading).Done;
  Part(NyxPart(NyxRecoveredDraftProvenancePart)).Binds.Text(ABindings.Provenance).Done;
  Part(NyxPart(NyxRecoveredDraftReasonPart)).Binds.Text(ABindings.Reason).Done;
  Part(NyxPart(NyxRecoveredDraftNoticePart)).Binds.Text(ABindings.Notice).Done;
  Part(NyxPart(NyxRecoveredDraftTextPart)).Binds.Value(ABindings.Draft).Done;
  Part(NyxPart(NyxRecoveredDraftAcceptedPart)).Binds.Value(ABindings.Accepted).Done;
  Part(NyxPart(NyxRecoveredDraftTextPart)).Configure.Text('Unfinished text').Done;
  Part(NyxPart(NyxRecoveredDraftAcceptedPart)).Configure.Text('Value when editing began').Done;
end;

function NewNyxBoundRecoveredDraftReview(const AID: TNyxText;
  const ABindings: TNyxRecoveredDraftBindings): INyxCard;
begin
  Result := TBoundDraftReview.Create(AID, ABindings);
end;

function TRecoveredDraftReview.GetCell: TNyxCollectionCellEdit;
begin
  Result := FCell.Copy;
end;

function TRecoveredDraftReview.GetReason: TNyxCollectionDraftLoss;
begin
  Result := FReason;
end;

function TRecoveredDraftReview.GetBeforeRevision: Integer;
begin
  Result := FBeforeRevision;
end;

function TRecoveredDraftReview.GetAfterRevision: Integer;
begin
  Result := FAfterRevision;
end;

function NewNyxRecoveredDraftReview(const AID: TNyxText;
  const AEntry: INyxRecoveredCollectionDraft): INyxRecoveredDraftReview;
var
  LCell: TNyxCollectionCellEdit;
  LReason: TNyxCollectionDraftLoss;
  LBefore: Integer;
  LAfter: Integer;
begin

  if AEntry = nil then
  begin
    raise ENyxCollection.Create('A recovered edit is required for review');
  end;
  LCell := AEntry.Cell.Copy;
  LCell.Validate;
  LReason := AEntry.Reason;
  NyxCollectionDraftLossDescription(LReason);
  LBefore := AEntry.BeforeRevision;
  LAfter := AEntry.AfterRevision;

  if (LBefore < 0) or (LAfter < LBefore) then
  begin
    raise ENyxCollection.Create('Recovered-edit revisions must describe an ordered transition');
  end;
  Result := TRecoveredDraftReview.Create(AID, LCell, LReason, LBefore, LAfter);
end;

end.
