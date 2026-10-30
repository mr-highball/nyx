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

uses nyx.text, nyx.controls, nyx.collections.edits, nyx.collections.recovery;

const
  NyxRecoveredDraftHeadingPart = 'recovery-heading';
  NyxRecoveredDraftProvenancePart = 'recovery-provenance';
  NyxRecoveredDraftReasonPart = 'recovery-reason';
  NyxRecoveredDraftNoticePart = 'recovery-notice';
  NyxRecoveredDraftTextPart = 'recovery-draft-text';
  NyxRecoveredDraftAcceptedPart = 'recovery-accepted-text';

type
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

{ Human-readable English default for a closed recovery reason. Applications may
  replace the named reason label for localization without changing its meaning. }
function NyxCollectionDraftLossDescription(AReason: TNyxCollectionDraftLoss): TNyxText;

implementation

uses SysUtils, nyx.types, nyx.collections, nyx.collections.cell.actions;

type
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

constructor TRecoveredDraftReview.Create(const AID: TNyxText;
  const ACell: TNyxCollectionCellEdit; AReason: TNyxCollectionDraftLoss;
  ABeforeRevision, AAfterRevision: Integer);
var
  LText: INyxMemo;
begin
  inherited Create(AID);
  FCell := NyxCollectionCellEdit(ACell.Item, ACell.Field, ACell.Accepted,
    ACell.Text, ACell.Selection, False, False);
  FReason := AReason;
  FBeforeRevision := ABeforeRevision;
  FAfterRevision := AAfterRevision;
  GetConfigure.Layout(nlColumn).Padding(16).Gap(8)
    .AccessibleName('Recovered edit').Done;
  Add(NewNyxHeading(AID + '-heading').Configure
    .PartName(NyxPart(NyxRecoveredDraftHeadingPart)).Text('Recovered edit').Done);
  Add(NewNyxLabel(AID + '-provenance').Configure
    .PartName(NyxPart(NyxRecoveredDraftProvenancePart)).Text(
      FCell.Item.Collection.Name + ' / ' + FCell.Item.ID + ' / ' + FCell.Field.Name +
      ' / source revisions ' + TNyxText(IntToStr(FBeforeRevision)) + ' to ' +
      TNyxText(IntToStr(FAfterRevision))).Done);
  Add(NewNyxLabel(AID + '-reason').Configure
    .PartName(NyxPart(NyxRecoveredDraftReasonPart))
    .Text(NyxCollectionDraftLossDescription(FReason)).Done);
  Add(NewNyxLabel(AID + '-notice').Configure
    .PartName(NyxPart(NyxRecoveredDraftNoticePart)).Text(
      'Your unfinished text is retained for review. This view does not apply or discard it. ' +
      'Controls and backslashes are escaped in both previews.').Done);
  LText := NewNyxMemo(AID + '-draft');
  LText.Configure.PartName(NyxPart(NyxRecoveredDraftTextPart))
    .AccessibleName('Recovered unfinished text').Height(160).ReadOnly(True).Done;
  LText.Value := NyxCollectionCellInspectionText(FCell.Text);
  Add(LText);
  LText := NewNyxMemo(AID + '-accepted');
  LText.Configure.PartName(NyxPart(NyxRecoveredDraftAcceptedPart))
    .AccessibleName('Accepted value when editing began').Height(96).ReadOnly(True).Done;
  LText.Value := NyxCollectionCellInspectionText(NyxCellEditWireText(FCell.Accepted));
  Add(LText);
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
