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
unit nyx.collections.query.preview;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.collections, nyx.collections.query, nyx.collections.view.types;

type
  { Bounded read-only exploration. Limits are UI-work/output admission limits,
    not changes to a source/query or a release latency guarantee. Defaults scan
    at most 5000 source rows, show 12 rows and return at most 32768 text scalars. }
  TNyxQueryPreviewOptions = record
  private
    FMaximumSource: Integer;
    FSampleRows: Integer;
    FMaximumText: Integer;
  public
    class function Defaults: TNyxQueryPreviewOptions; static;
    function MaximumSourceRows(AValue: Integer): TNyxQueryPreviewOptions;
    function SampleRows(AValue: Integer): TNyxQueryPreviewOptions;
    function MaximumTextScalars(AValue: Integer): TNyxQueryPreviewOptions;
    procedure Validate;
    property SourceLimit: Integer read FMaximumSource;
    property RowLimit: Integer read FSampleRows;
    property TextLimit: Integer read FMaximumText;
  end;
  { Value-owned report: no source/store/document/control or callback survives.
    Counts reflect the selected query, including required tree ancestors.
    Sample text clips display cells on Unicode scalar boundaries, while Pascal
    retains exact admitted predicate/sort names and values. A whole-report text
    overflow refuses, rather than clipping executable source or returning partial
    counts. Callers label this snapshot explicitly; it is not a live binding. }
  TNyxQueryPreview = record
    Collection: TNyxCollectionRef;
    SourceRevision: Integer;
    SourceCount: Integer;
    VisibleCount: Integer;
    SampleCount: Integer;
    Tree: Boolean;
    Pascal: TNyxText;
    Text: TNyxText;
  end;

{ Shared bounded evaluation for UI and semantic callers. The returned immutable
  snapshot owns its rows independently of a store/document/control. Admission
  validates every bound field, including empty output, and tree ancestry before
  returning. This performs no mutation, runtime binding or UI publication. }
function EvaluateNyxCollectionQueryPreview(const ASource: INyxCollectionSnapshot;
  const AView: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection;
  const AQuery: TNyxCollectionQuery; const AOptions: TNyxQueryPreviewOptions): INyxCollectionSnapshot;

{ Borrows the exact immutable source only during evaluation. The view descriptor
  supplies columns/parent semantics; AQuery can be an unapplied editor candidate.
  Nil sources, wrong collection/fields, malformed hierarchy, invalid policies,
  configured limits and query failures raise before any report is returned. }
function PreviewNyxCollectionQuery(const ASource: INyxCollectionSnapshot;
  const AView: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection;
  const AQuery: TNyxCollectionQuery; const AOptions: TNyxQueryPreviewOptions): TNyxQueryPreview;

implementation

uses
  SysUtils, nyx.state, nyx.codegen, nyx.data, nyx.collections.query.view,
  nyx.linq, nyx.linq.collections;

class function TNyxQueryPreviewOptions.Defaults: TNyxQueryPreviewOptions;
begin
  Result.FMaximumSource := 5000;
  Result.FSampleRows := 12;
  Result.FMaximumText := 32768;
end;

procedure TNyxQueryPreviewOptions.Validate;
begin

  if (FMaximumSource < 1) or (FMaximumSource > 1048576) or
    (FSampleRows < 1) or (FSampleRows > 50) or
    (FMaximumText < 256) or (FMaximumText > 262144) then
  begin
    raise ENyxCollection.Create('Use initialized bounded query preview options');
  end;
end;

function TNyxQueryPreviewOptions.MaximumSourceRows(AValue: Integer): TNyxQueryPreviewOptions;
begin
  Result := Self;
  Result.FMaximumSource := AValue;
  Result.Validate;
end;

function TNyxQueryPreviewOptions.SampleRows(AValue: Integer): TNyxQueryPreviewOptions;
begin
  Result := Self;
  Result.FSampleRows := AValue;
  Result.Validate;
end;

function TNyxQueryPreviewOptions.MaximumTextScalars(AValue: Integer): TNyxQueryPreviewOptions;
begin
  Result := Self;
  Result.FMaximumText := AValue;
  Result.Validate;
end;

function DisplayText(const AText: TNyxText; AMaximum: Integer): TNyxText;
var
  LPosition: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  LPosition := 1;
  LCount := 0;
  while (LPosition <= Length(AText)) and (LCount < AMaximum) do
  begin

    if not NyxNextScalar(AText, LPosition, LScalar) then
    begin
      raise ENyxCollection.Create('Query preview text must contain valid Unicode');
    end;
    Inc(LCount);
  end;
  Result := Copy(AText, 1, LPosition - 1);

  if LPosition <= Length(AText) then
  begin
    Result := Result + TNyxText('...');
  end;
end;

function ScalarText(const AValue: TNyxStateValue): TNyxText;
begin
  case AValue.Kind of
    { JSON string display preserves printable Unicode and visibly escapes NUL/
      controls. This is display text, separate from the exact Pascal expression. }
    nskText: Result := NyxData(DisplayText(AValue.TextValue, 80)).ToJSON;
    nskBoolean:
    begin

      if AValue.BooleanValue then
      begin
        Result := 'True';
      end
      else
      begin
        Result := 'False';
      end;
    end;
    nskInteger: Result := IntToStr(AValue.IntegerValue);
    nskNumber: Result := NyxData(AValue.NumberValue).ToJSON;
  end;
end;

function EvaluateNyxCollectionQueryPreview(const ASource: INyxCollectionSnapshot;
  const AView: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection;
  const AQuery: TNyxCollectionQuery; const AOptions: TNyxQueryPreviewOptions): INyxCollectionSnapshot;
var
  LParents: TNyxQueryIndexes;
  LResultParents: TNyxQueryIndexes;
  LRow: Integer;
  LColumn: Integer;
  LParent: TNyxText;
  LSchema: TNyxCollectionSchema;
begin
  AOptions.Validate;

  if ASource = nil then
  begin
    raise ENyxCollection.Create('Query preview requires an immutable source');
  end;
  AView.Validate;

  if not AView.Defined or (AView.Key.Name <> ASource.Key.Name) then
  begin
    raise ENyxCollection.Create('Query preview requires the exact collection binding');
  end;

  if ASource.Count > AOptions.SourceLimit then
  begin
    raise ENyxCollection.Create('Source exceeds the configured query preview row limit');
  end;
  AQuery.Validate(ASource.Schema);
  LSchema := ASource.Schema;
  for LColumn := 0 to AView.Count - 1 do
  begin
    case AView.ColumnAt(LColumn).Kind of
      nskText: LSchema.Definition(NyxTextField(AView.ColumnAt(LColumn).FieldName));
      nskBoolean: LSchema.Definition(NyxBooleanField(AView.ColumnAt(LColumn).FieldName));
      nskInteger: LSchema.Definition(NyxIntegerField(AView.ColumnAt(LColumn).FieldName));
      nskNumber: LSchema.Definition(NyxNumberField(AView.ColumnAt(LColumn).FieldName));
    end;
  end;
  LParents := nil;

  if (AProjection = cpTree) and (AView.ParentField <> '') then
  begin
    LSchema.Definition(NyxTextField(AView.ParentField));
    SetLength(LParents, ASource.Count);
    for LRow := 0 to ASource.Count - 1 do
    begin
      LParents[LRow] := -1;
      LParent := ASource.ItemAt(LRow).GetValue(NyxTextField(AView.ParentField));

      if LParent <> '' then
      begin
        LParents[LRow] := ASource.IndexOf(NyxItem(ASource.Key, LParent));

        if LParents[LRow] < 0 then
        begin
          raise ENyxCollection.Create('Query preview tree parent is absent from the source');
        end;
      end;
    end;
  end;
  Result := NyxQuerySnapshot(ASource, AQuery, LParents, LResultParents);
end;

function PreviewNyxCollectionQuery(const ASource: INyxCollectionSnapshot;
  const AView: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection;
  const AQuery: TNyxCollectionQuery; const AOptions: TNyxQueryPreviewOptions): TNyxQueryPreview;
var
  LVisible: INyxCollectionSnapshot;
  LRows: TNyxSequenceArray<TNyxCollectionItem>;
  LLines: TNyxStrings;
  LRow: Integer;
  LColumn: Integer;
  LValue: TNyxStateValue;
  LReport: TNyxQueryPreview;
  LPosition: Integer;
  LScalars: Integer;
  LScalar: Integer;
begin
  LVisible := EvaluateNyxCollectionQueryPreview(ASource, AView, AProjection, AQuery, AOptions);
  LRows := LVisible.AsNyx.Take(AOptions.RowLimit).ToArray;
  LReport.Collection := NyxCollection(ASource.Key.Name);
  LReport.SourceRevision := ASource.Revision;
  LReport.SourceCount := ASource.Count;
  LReport.VisibleCount := LVisible.Count;
  LReport.SampleCount := Length(LRows);
  LReport.Tree := AProjection = cpTree;
  LReport.Pascal := TNyxCodegen.QueryExpression(AQuery);
  LLines := TNyxStrings.Create;
  try
    LLines.Add('Project defaults / ' + DisplayText(ASource.Key.Name, 80) +
      ' / revision ' + TNyxText(IntToStr(ASource.Revision)));
    LLines.Add(TNyxText(IntToStr(LReport.VisibleCount)) + ' of ' +
      TNyxText(IntToStr(LReport.SourceCount)) + ' rows / showing ' +
      TNyxText(IntToStr(LReport.SampleCount)));

    if LReport.Tree then
    begin
      LLines.Add('Tree query includes required ancestors; disclosure and paging are runtime presentation.');
    end;
    LLines.Add('');
    LLines.Add('Pascal query');
    LLines.Add(LReport.Pascal);
    LLines.Add('');
    LLines.Add('Row sample');
    for LRow := 0 to High(LRows) do
    begin
      LLines.Add(TNyxText(IntToStr(LRow + 1)) + '. ' + DisplayText(LRows[LRow].Ref.ID, 80));
      for LColumn := 0 to AView.Count - 1 do
      begin
        LValue := AView.ColumnAt(LColumn).Read(LRows[LRow]);
        LLines.Add('  ' + DisplayText(AView.ColumnAt(LColumn).Title, 60) + ': ' + ScalarText(LValue));
      end;
    end;

    if Length(LRows) < LVisible.Count then
    begin
      LLines.Add('More matching rows are available; this sample is bounded.');
    end;
    LReport.Text := LLines.Text;
  finally
    LLines.Free;
  end;
  LPosition := 1;
  LScalars := 0;
  while LPosition <= Length(LReport.Text) do
  begin

    if not NyxNextScalar(LReport.Text, LPosition, LScalar) then
    begin
      raise ENyxCollection.Create('Query preview report contains invalid Unicode');
    end;
    Inc(LScalars);

    if LScalars > AOptions.TextLimit then
    begin
      raise ENyxCollection.Create('Query preview exceeds the configured text limit');
    end;
  end;
  Result := LReport;
end;

end.
