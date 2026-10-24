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
program nyx_collection_identity_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.collections, nyx.collections.selection,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.query,
  nyx.collections.paging, nyx.collections.clipboard, nyx.collections.identities
  {$ifdef PAS2JS}, Web{$endif};

type
  { Real subscription receivers outlive every borrowed token. Readonly output
    must not publish edits, and a retained payload must not keep these alive. }
  TObserver = class
  public
    Changes: Integer;
    procedure ViewChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
  end;

const
  CControlCell: TNyxText = 'x' + #0#10 + '"🌙';
  CEncodedControlCell: TNyxText = '"x' + #0#10 + '""🌙"';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollectionClipboard.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TObserver.ViewChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
begin
  Inc(Changes);
end;

procedure TObserver.StoreChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  Inc(Changes);
end;

procedure ConfigureQuery(const AView: INyxCollectionView);
begin
  AView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(2))
    .OrderBy(NyxIntegerField('Score')));
end;

procedure ConfigurePage(const AView: INyxCollectionView);
var
  LPages: INyxCollectionPages;
begin
  LPages := NyxCollectionPages(AView);
  LPages.ConfigurePaging(NyxCollectionPaging.PageSize(1));
end;

function CaptureRows(const AView: INyxCollectionView): INyxCollectionSelectedRowsCapture;
var
  LRows: INyxCollectionSelectedRowsView;
begin
  Supports(AView, INyxCollectionSelectedRowsView, LRows);
  Result := LRows.CaptureSelectedRows;
end;

procedure UpdateRow(const AStore: INyxCollection; const AItem: TNyxItemRef);
begin
  AStore.Update(NyxCollectionItem(AItem).WithValue(NyxIntegerField('Score'), 9));
end;

procedure Refuse(const ASource: INyxCollectionSnapshot;
  const ASelection: TNyxCollectionSelectionSnapshot; AMembership: TNyxCollectionCopyMembership;
  const APage: INyxCollectionSnapshot; const ALimits: TNyxCollectionClipboardLimits;
  const AReason: TNyxText);
var
  LValue: INyxCollectionIdentityText;
  LRefused: Boolean;
begin
  LRefused := False;
  try
    LValue := NewNyxCollectionIdentityText(ASource, ASelection, AMembership,
      APage, True, nccleLF, ALimits);
  except
    on E: ENyxCollectionClipboard do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LValue = nil), AReason);
end;

procedure Run;
var
  LKey: TNyxCollectionRef;
  LA: TNyxItemRef;
  LB: TNyxItemRef;
  LC: TNyxItemRef;
  LInvalid: TNyxItemRef;
  LSchema: TNyxCollectionSchema;
  LStore: INyxCollection;
  LForeign: INyxCollection;
  LView: INyxCollectionView;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LSelection: TNyxCollectionSelectionSnapshot;
  LValue: INyxCollectionIdentityText;
  LPageValue: INyxCollectionIdentityText;
  LPrevious: INyxCollectionIdentityText;
  LCapture: INyxCollectionSelectedRowsCapture;
  LViewToken: INyxCollectionViewSubscription;
  LStoreToken: INyxCollectionSubscription;
  LObserver: TObserver;
  LLimits: TNyxCollectionClipboardLimits;
  LExpected: TNyxText;
  LRefused: Boolean;
  LBefore: Integer;
begin
  LKey := NyxCollection('identities 🌙');
  LA := NyxItem(LKey, 'a"🌙');
  LB := NyxItem(LKey, 'b');
  LC := NyxItem(LKey, 'c');
  LSchema := NyxCollectionSchema.Integer(NyxIntegerField('Score'), 0);
  LStore := NewNyxCollection(LKey, LSchema, [
    NyxCollectionItem(LA).WithValue(NyxIntegerField('Score'), 1),
    NyxCollectionItem(LB).WithValue(NyxIntegerField('Score'), 3),
    NyxCollectionItem(LC).WithValue(NyxIntegerField('Score'), 2)]);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(LKey).Selection(nsmMultiple)
    .Column(NyxIntegerField('Score'), 'Score'), cpTable);
  LView.SetSelection([LC, LA, LB], LA, LA);
  LObserver := TObserver.Create;
  try
    LViewToken := LView.Subscribe({$ifdef PAS2JS}@{$endif}LObserver.ViewChanged);
    LStoreToken := LStore.Subscribe({$ifdef PAS2JS}@{$endif}LObserver.StoreChanged);
    LLimits := TNyxCollectionClipboardLimits.Defaults;
    LSource := LStore.Snapshot;
    LSelection := LView.Selection.Snapshot;
    LValue := NewNyxCollectionIdentityText(LSource, LSelection, nccmAllSelectedSource,
      nil, True, nccleCRLF, LLimits);
    LExpected := 'ID' + #13#10 + '"a""🌙"' + #13#10 + 'b' + #13#10 + 'c';
    Check((LValue.Text = LExpected) and (LValue.ByteCount = NyxUTF8ByteCount(LExpected)),
      'identity TSV preserves supplementary text/quote escaping and explicit CRLF without trailing terminator');
    Check((LValue.Key.Name = LKey.Name) and (LValue.Revision = LSource.Revision) and
      (LValue.Count = 3) and (LValue.Membership = nccmAllSelectedSource) and
      LValue.IncludeHeader and (LValue.LineEnding = nccleCRLF),
      'identity receipt retains exact key/revision/membership/header/line facts');
    Check((LValue.ItemAt(0).Collection.Name = LKey.Name) and (LValue.ItemAt(0).ID = LA.ID) and
      (LValue.ItemAt(2).ID = LC.ID), 'selected identities follow source order instead of gesture enumeration');
    Check(LObserver.Changes = 0, 'readonly identity capture changes neither actual view nor store');
    ConfigureQuery(LView);
    LPage := LView.Snapshot;
    LPageValue := NewNyxCollectionIdentityText(LSource, LSelection, nccmSelectedPage,
      LPage, False, nccleLF, LLimits);
    Check((LPageValue.Text = 'c' + #10 + 'b') and (LPageValue.Count = 2),
      'page identity output excludes filtered selection and follows the actual sorted page');
    Check(LValue.Text = LExpected, 'a later query cannot redirect earlier selected-source identity bytes');
    ConfigurePage(LView);
    LPage := LView.Snapshot;
    LPageValue := NewNyxCollectionIdentityText(LSource, LSelection, nccmSelectedPage,
      LPage, False, nccleLF, LLimits.Rows(1).Cells(1).ScannedRows(4));
    Check((LPageValue.Text = 'c') and not LPageValue.IncludeHeader,
      'one-cell page output admits a larger source selection without widening page membership');
    Refuse(nil, LSelection, nccmAllSelectedSource, nil, LLimits, 'missing source refuses identity output');
    Refuse(LSource, Default(TNyxCollectionSelectionSnapshot), nccmAllSelectedSource,
      nil, LLimits, 'undefined selection refuses identity output');
    Refuse(LSource, LSelection, nccmAllSelectedSource, LPage, LLimits,
      'source choice refuses an unsolicited page instead of silently broadening membership');
    Refuse(LSource, LSelection, nccmSelectedPage, nil, LLimits,
      'page choice requires an actual explicit page');
    Refuse(LSource, LSelection, nccmAllSelectedSource, nil, LLimits.Rows(2),
      'identity rows obey caller row budget');
    Refuse(LSource, LSelection, nccmAllSelectedSource, nil, LLimits.Cells(2),
      'identity rows count one actual output cell each');
    Refuse(LSource, LSelection, nccmAllSelectedSource, nil, LLimits.ScannedRows(5),
      'identity source scan and selected membership work obey the same bounded admission');
    LPrevious := LValue;
    LRefused := False;
    try
      LValue := NewNyxCollectionIdentityText(LSource, LSelection, nccmAllSelectedSource,
        nil, True, nccleLF, LLimits.UTF8Bytes(2));
    except
      on E: ENyxCollectionClipboard do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LValue = LPrevious) and (LValue.Text = LExpected),
      'byte refusal preserves a same-destination original identity output atomically');
    LForeign := NewNyxCollection(LKey, LSchema, [
      NyxCollectionItem(LA).WithValue(NyxIntegerField('Score'), 99)]);
    Refuse(LSource, LSelection, nccmSelectedPage, LForeign.Snapshot, LLimits,
      'same-key/revision page with changed actual accepted row values refuses');
    LRefused := False;
    try
      LInvalid := NyxItem(LKey, 'bad' + #0 + 'id');
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'existing item reference contract explicitly refuses embedded NUL identity');
    LRefused := False;
    try
      LInvalid := NyxItem(LKey, 'bad' + #10 + 'id');
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'existing item reference contract explicitly refuses newline identity');
    Check(NyxCollectionTSVCell(CControlCell, 100) = CEncodedControlCell,
      'shared TSV encoding still preserves exact NUL/newline/Unicode ordinary cell content');
    UpdateRow(LStore, LB);
    Refuse(LStore.Snapshot, LSelection, nccmAllSelectedSource, nil, LLimits,
      'old selection revision cannot become a newer identity cohort');
    LCapture := CaptureRows(LView);
    LBefore := LObserver.Changes;
    LValue := NewNyxCollectionIdentityText(LCapture.Source, LCapture.Selection,
      nccmAllSelectedSource, nil, False, nccleLF, LLimits);
    Check(LObserver.Changes = LBefore, 'detached current-cohort identity production remains readonly');
    LView := nil;
    LStore := nil;
    Check(not LViewToken.Connected and not LStoreToken.Connected,
      'retained identity payload/source snapshots/capture keep no actual view or store alive');
    Check((LValue.Count = 3) and (LValue.ItemAt(0).ID = LA.ID),
      'copied identity values remain readable after actual owner retirement');
    LRefused := False;
    try
      LCapture.Readiness.CheckReady;
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'original row-cohort readiness retires without retaining or querying its old view');
  finally
    LViewToken := nil;
    LStoreToken := nil;
    LView := nil;
    LStore := nil;
    LForeign := nil;
    LObserver.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-identities', 'passed');
    document.body.setAttribute('data-collection-identity-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' detached selected identity checks; no clipboard effect');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-identities', 'failed');
      document.body.setAttribute('data-collection-identity-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
