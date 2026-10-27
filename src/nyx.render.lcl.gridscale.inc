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

type
  { Absolute native grid metrics. The containing scale row revokes this borrowed
    grid on component removal. Columns are bounded by the portable table domain;
    body heights cover only explicitly registered sparse overrides. Viewport
    anchors are copied before normalization and restored after the whole tree
    settles, so intermediate clipping cannot become a new scroll intention. No model,
    cell values, selection, editor draft or dataset-sized height copy is owned. }
  TNativeGridMetrics = class
  private
    FGrid: TNyxCollectionStringGrid;
    FColumns: array of Integer;
    FRows: array of Integer;
    FHeights: array of Integer;
    FDefaultColumn: Integer;
    FDefaultRow: Integer;
    FHeader: Integer;
    FTitleHeight: Integer;
    FLine: Integer;
    FToolbar: Integer;
    FInset: Integer;
    FTopRow: Integer;
    FLeftColumn: Integer;
  public
    constructor Create(AGrid: TNyxCollectionStringGrid);
    procedure Capture(const AGeneration: INyxLCLBindingGeneration);
    procedure Validate(AScale: Double);
    procedure Apply(AScale: Double; const AGeneration: INyxLCLBindingGeneration);
    procedure RememberViewport(const AGeneration: INyxLCLBindingGeneration);
    procedure RestoreViewport(const AGeneration: INyxLCLBindingGeneration);
  end;

constructor TNativeGridMetrics.Create(AGrid: TNyxCollectionStringGrid);
begin
  inherited Create;
  FGrid := AGrid;
end;

procedure TNativeGridMetrics.Capture(const AGeneration: INyxLCLBindingGeneration);
var
  LIndex: Integer;
begin
  RequireNativeGeneration(AGeneration);

  if FGrid.ColCount > NyxMaximumColumnLayoutFields then
  begin
    raise ENyxModel.Create('Uniform native grid exceeds the portable column domain');
  end;
  FDefaultColumn := FGrid.DefaultColWidth;
  FDefaultRow := FGrid.DefaultRowHeight;
  FHeader := FGrid.RowHeights[0];
  FTitleHeight := FGrid.TitleFont.Height;
  FLine := FGrid.GridLineWidth;
  FToolbar := FGrid.ColumnToolbarHeight;
  FInset := FGrid.AlignmentVerticalInset;
  SetLength(FColumns, FGrid.ColCount);
  for LIndex := 0 to High(FColumns) do
  begin
    FColumns[LIndex] := FGrid.ColWidths[LIndex];
  end;
  SetLength(FRows, FGrid.ProjectionRowCount);
  SetLength(FHeights, Length(FRows));
  for LIndex := 0 to High(FRows) do
  begin
    FRows[LIndex] := FGrid.ProjectionRow(LIndex);
    FHeights[LIndex] := FGrid.RowHeights[FRows[LIndex]];
  end;
end;

procedure TNativeGridMetrics.Validate(AScale: Double);
var
  LIndex: Integer;
begin

  if FGrid.ProjectionEpoch > High(Integer) - 2 then
  begin
    raise ENyxModel.Create('Native grid projection epoch is exhausted');
  end;
  NativeScalePixel(FDefaultColumn, AScale);
  NativeScalePixel(FDefaultRow, AScale);
  NativeScalePixel(FHeader, AScale);
  NativeScaleFont(FTitleHeight, AScale);
  NativeScalePixel(FLine, AScale);

  if NativeScalePixel(FToolbar, AScale) > 4096 then
  begin
    raise ENyxModel.Create('Uniform native column toolbar exceeds its physical bounds');
  end;
  NativeScalePixel(FInset, AScale);
  for LIndex := 0 to High(FColumns) do
  begin
    NativeScalePixel(FColumns[LIndex], AScale);
  end;
  for LIndex := 0 to High(FHeights) do
  begin
    NativeScalePixel(FHeights[LIndex], AScale);
  end;
end;

procedure TNativeGridMetrics.Apply(AScale: Double;
  const AGeneration: INyxLCLBindingGeneration);
var
  LIndex: Integer;
begin
  RequireNativeGeneration(AGeneration);
  Validate(AScale);
  FGrid.BeginPresentationChange;
  FGrid.BeginColumnTransition;
  FGrid.BeginUpdate;
  try
    FGrid.TitleFont.Height := NativeScaleFont(FTitleHeight, AScale);
    RequireNativeGeneration(AGeneration);
    FGrid.DefaultColWidth := Max(1, NativeScalePixel(FDefaultColumn, AScale));
    RequireNativeGeneration(AGeneration);
    FGrid.DefaultRowHeight := Max(1, NativeScalePixel(FDefaultRow, AScale));
    RequireNativeGeneration(AGeneration);
    FGrid.GridLineWidth := Max(1, NativeScalePixel(FLine, AScale));
    RequireNativeGeneration(AGeneration);
    FGrid.RowHeights[0] := Max(0, NativeScalePixel(FHeader, AScale));
    RequireNativeGeneration(AGeneration);
    FGrid.ColumnToolbarHeight := Max(0, NativeScalePixel(FToolbar, AScale));
    RequireNativeGeneration(AGeneration);
    FGrid.ConfigureAlignmentVerticalInset(Max(0, NativeScalePixel(FInset, AScale)));
    RequireNativeGeneration(AGeneration);
    for LIndex := 0 to Min(High(FColumns), FGrid.ColCount - 1) do
    begin
      FGrid.ColWidths[LIndex] := Max(0, NativeScalePixel(FColumns[LIndex], AScale));
      RequireNativeGeneration(AGeneration);
    end;
    for LIndex := 0 to High(FRows) do
    begin

      if FRows[LIndex] < FGrid.RowCount then
      begin
        FGrid.RowHeights[FRows[LIndex]] := Max(0, NativeScalePixel(FHeights[LIndex], AScale));
        RequireNativeGeneration(AGeneration);
      end;
    end;
  finally
    { The entered physical frame retains the component even after renderer
      retirement, so its counted LCL guards can always unwind. }
    FGrid.EndUpdate;
    FGrid.EndColumnTransition;
    FGrid.EndPresentationChange;
  end;
end;

procedure TNativeGridMetrics.RememberViewport(const AGeneration: INyxLCLBindingGeneration);
begin
  RequireNativeGeneration(AGeneration);
  FTopRow := FGrid.TopRow;
  FLeftColumn := FGrid.LeftCol;
end;

procedure TNativeGridMetrics.RestoreViewport(const AGeneration: INyxLCLBindingGeneration);
begin
  RequireNativeGeneration(AGeneration);
  { A mixed-size intermediate frame can temporarily have no overflow. Restore
    only after the entire tree's bounds/fonts and grid metrics have settled,
    under the same native guards that prevent geometry from committing edits. }
  FGrid.BeginPresentationChange;
  FGrid.BeginColumnTransition;
  FGrid.BeginUpdate;
  try
    FGrid.TopRow := Max(FGrid.FixedRows, Min(FTopRow, FGrid.RowCount - 1));
    RequireNativeGeneration(AGeneration);
    FGrid.LeftCol := Max(FGrid.FixedCols, Min(FLeftColumn, FGrid.ColCount - 1));
    RequireNativeGeneration(AGeneration);
  finally
    FGrid.EndUpdate;
    FGrid.EndColumnTransition;
    FGrid.EndPresentationChange;
  end;
end;
