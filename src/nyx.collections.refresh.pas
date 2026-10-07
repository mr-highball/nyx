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
unit nyx.collections.refresh;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.collections, nyx.collections.view.types;

type
  { Full includes first mount, changed visible order/hierarchy and missing or
    mismatched change context. Rows is an authoritative scalar publication;
    Unchanged means no projected values changed, not that selection or policy
    can be skipped. No mode grants permission to overwrite an unchanged draft. }
  TNyxCollectionRefreshKind = (ncrFull, ncrRows, ncrUnchanged);

  { Immutable adapter plan. Owns only a private Boolean row map, retaining no
    dataset, change log, renderer or widget. Copies share immutable bits safely
    on both compilers. The default record is undefined and refuses lookup.
    ChangedCount counts candidate rows, not cell writes, frames or resident rows. }
  TNyxCollectionRefreshPlan = record
  private
    FDefined: Boolean;
    FKind: TNyxCollectionRefreshKind;
    FCount: Integer;
    FChangedCount: Integer;
    FRows: array of Boolean;
  public
    { Refuses undefined plans and indices outside the current visible snapshot. }
    function ValuesChanged(AIndex: Integer): Boolean;
    property Defined: Boolean read FDefined;
    property Kind: TNyxCollectionRefreshKind read FKind;
    property Count: Integer read FCount;
    property ChangedCount: Integer read FChangedCount;
  end;

{ Plan admitted view snapshots using the authoritative ordered store log passed
  to that view's observer. Query results may differ from the log's source rows;
  exact visible identities/order and source revisions gate the scalar shortcut.
  Every structural operation or changed parent field falls back to Full. A nil
  current snapshot refuses; absent previous/log context safely requests Full.
  This bounds scalar comparisons/widget writes, not full view validation, query
  evaluation, snapshot construction, selection synchronization or virtualization. }
function NyxCollectionRefreshPlan(const APrevious, ACurrent: INyxCollectionSnapshot;
  const ASpec: TNyxCollectionViewSpec;
  const AChanges: INyxCollectionChanges): TNyxCollectionRefreshPlan;

implementation

function TNyxCollectionRefreshPlan.ValuesChanged(AIndex: Integer): Boolean;
begin

  if not FDefined or (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxCollection.Create('Refresh row is outside an admitted plan');
  end;
  Result := FKind = ncrFull;

  if FKind = ncrRows then
  begin
    Result := FRows[AIndex];
  end;
end;

function NyxCollectionRefreshPlan(const APrevious, ACurrent: INyxCollectionSnapshot;
  const ASpec: TNyxCollectionViewSpec;
  const AChanges: INyxCollectionChanges): TNyxCollectionRefreshPlan;
var
  LIndex: Integer;
  LRow: Integer;
  LBefore: INyxCollectionSnapshot;
  LAfter: INyxCollectionSnapshot;
begin

  if ACurrent = nil then
  begin
    raise ENyxCollection.Create('Refresh requires an admitted current snapshot');
  end;
  Result := Default(TNyxCollectionRefreshPlan);
  Result.FDefined := True;
  Result.FKind := ncrFull;
  Result.FCount := ACurrent.Count;
  Result.FChangedCount := ACurrent.Count;

  if APrevious = ACurrent then
  begin
    Result.FKind := ncrUnchanged;
    Result.FChangedCount := 0;
    Exit;
  end;

  if (APrevious = nil) or (AChanges = nil) or (AChanges.Count = 0) then
  begin
    Exit;
  end;
  LBefore := AChanges.Before;
  LAfter := AChanges.After;

  if (LBefore = nil) or (LAfter = nil) or
    (LBefore.Key.Name <> APrevious.Key.Name) or
    (LAfter.Key.Name <> ACurrent.Key.Name) or
    (LBefore.Revision <> APrevious.Revision) or
    (LAfter.Revision <> ACurrent.Revision) or
    (APrevious.Count <> ACurrent.Count) then
  begin
    Exit;
  end;
  for LIndex := 0 to ACurrent.Count - 1 do
  begin

    if APrevious.ItemAt(LIndex).Ref.ID <> ACurrent.ItemAt(LIndex).Ref.ID then
    begin
      Exit;
    end;
  end;
  for LIndex := 0 to AChanges.Count - 1 do
  begin

    if not (AChanges.Kind(LIndex) in [nceUpdate, nceReplace]) then
    begin
      Exit;
    end;

    if (ASpec.ParentField <> '') and
      (AChanges.BeforeItem(LIndex).GetValue(NyxTextField(ASpec.ParentField)) <>
        AChanges.AfterItem(LIndex).GetValue(NyxTextField(ASpec.ParentField))) then
    begin
      Exit;
    end;
  end;
  Result.FKind := ncrRows;
  Result.FChangedCount := 0;
  SetLength(Result.FRows, ACurrent.Count);
  for LIndex := 0 to AChanges.Count - 1 do
  begin
    LRow := ACurrent.IndexOf(AChanges.ItemRef(LIndex));

    if (LRow >= 0) and not Result.FRows[LRow] then
    begin
      Result.FRows[LRow] := True;
      Inc(Result.FChangedCount);
    end;
  end;

  if Result.FChangedCount = 0 then
  begin
    Result.FRows := nil;
    Result.FKind := ncrUnchanged;
  end;
end;

end.
