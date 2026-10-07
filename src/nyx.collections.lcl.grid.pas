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
unit nyx.collections.lcl.grid;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Grids, nyx.text;

type
  { Native-only pure text reader. The grid borrows its receiver; its mount must
    detach before releasing the receiver/view or destroying the widget. Reads
    run on the UI thread and must not publish commands or dispose the widget. }
  TNyxGridTextReader = function(AColumn, ARow: Integer): TNyxText of object;

  { Ordinary LCL grid with on-demand bound values. Without a reader it retains
    TStringGrid behavior. With one, accepted values are queried rather than
    copied into every native cell; explicit widget drafts remain sparse overlays.
    Inherited input/validation still owns edit admission. Provider/model data is
    saved through Nyx, never by native widget streaming of its physical cache.
    Grid geometry and model snapshots remain dataset-sized; this class alone
    establishes neither browser row windowing nor complete memory/frame budgets. }
  TNyxCollectionStringGrid = class(TStringGrid)
  private
    FReader: TNyxGridTextReader;
    FReads: Int64;
    FOverrides: array of record
      Column: Integer;
      Row: Integer;
      Text: TNyxText;
    end;
    function OverrideIndex(AColumn, ARow: Integer): Integer;
    procedure RemoveOverride(AIndex: Integer);
    function GetReaderAttached: Boolean;
    function GetOverrideCount: Integer;
  protected
    function GetCells(ACol, ARow: Integer): String; override;
    procedure SetCells(ACol, ARow: Integer; const AValue: String); override;
  public
    { Nil or a second active reader refuses. Caller retains/disconnects the
      exclusive receiver; no managed backreference or ownership transfer occurs. }
    procedure AttachReader(AReader: TNyxGridTextReader);
    { Ends borrowing and clears overlays. Ordinary physical LCL cells remain;
      disconnect does not materialize the former entire bound dataset. }
    procedure DetachReader;
    { Adapter normalization clears exactly one override without creating a
      native cell or invoking edit validation. Unknown cells are a no-op. }
    procedure ClearOverride(AColumn, ARow: Integer);
    { Discard overlays outside the admitted source geometry, including the spare
      empty row required by LCL when the source has no rows. Header row is zero. }
    procedure TrimOverrides(AColumns, ASourceRows: Integer);
    { Diagnostic counts source requests, including headers/measurement. It
      saturates rather than making rendering fail on integer overflow. }
    procedure ResetReadCount;
    property ReaderAttached: Boolean read GetReaderAttached;
    property SourceReadCount: Int64 read FReads;
    { Own overlay count; excludes LCL metadata/cache and model resident memory. }
    property OverrideCount: Integer read GetOverrideCount;
  end;

implementation

uses
  nyx.collections;

function TNyxCollectionStringGrid.OverrideIndex(AColumn, ARow: Integer): Integer;
begin
  for Result := 0 to Length(FOverrides) - 1 do
  begin

    if (FOverrides[Result].Column = AColumn) and (FOverrides[Result].Row = ARow) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TNyxCollectionStringGrid.RemoveOverride(AIndex: Integer);
var
  LIndex: Integer;
begin
  for LIndex := AIndex to Length(FOverrides) - 2 do
  begin
    FOverrides[LIndex] := FOverrides[LIndex + 1];
  end;
  SetLength(FOverrides, Length(FOverrides) - 1);
end;

function TNyxCollectionStringGrid.GetReaderAttached: Boolean;
begin
  Result := Assigned(FReader);
end;

function TNyxCollectionStringGrid.GetOverrideCount: Integer;
begin
  Result := Length(FOverrides);
end;

procedure TNyxCollectionStringGrid.AttachReader(AReader: TNyxGridTextReader);
begin

  if not Assigned(AReader) or ReaderAttached then
  begin
    raise ENyxCollection.Create('Grid requires one exclusive live text reader');
  end;
  FOverrides := nil;
  FReads := 0;
  FReader := AReader;
end;

procedure TNyxCollectionStringGrid.DetachReader;
begin
  FReader := nil;
  FOverrides := nil;
end;

procedure TNyxCollectionStringGrid.ResetReadCount;
begin
  FReads := 0;
end;

procedure TNyxCollectionStringGrid.ClearOverride(AColumn, ARow: Integer);
var
  LIndex: Integer;
begin
  LIndex := OverrideIndex(AColumn, ARow);

  if LIndex >= 0 then
  begin
    RemoveOverride(LIndex);

    if (AColumn >= 0) and (AColumn < ColCount) and (ARow >= 0) and (ARow < RowCount) then
    begin
      InvalidateCell(AColumn, ARow);
    end;
  end;
end;

procedure TNyxCollectionStringGrid.TrimOverrides(AColumns, ASourceRows: Integer);
var
  LIndex: Integer;
begin
  for LIndex := Length(FOverrides) - 1 downto 0 do
  begin

    if (FOverrides[LIndex].Column < 0) or (FOverrides[LIndex].Column >= AColumns) or
      (FOverrides[LIndex].Row < 0) or (FOverrides[LIndex].Row > ASourceRows) then
    begin
      RemoveOverride(LIndex);
    end;
  end;
end;

function TNyxCollectionStringGrid.GetCells(ACol, ARow: Integer): String;
var
  LIndex: Integer;
begin

  if not ReaderAttached then
  begin
    Exit(inherited GetCells(ACol, ARow));
  end;
  LIndex := OverrideIndex(ACol, ARow);

  if LIndex >= 0 then
  begin
    Exit(FOverrides[LIndex].Text);
  end;

  if FReads < High(Int64) then
  begin
    Inc(FReads);
  end;
  Result := FReader(ACol, ARow);
end;

procedure TNyxCollectionStringGrid.SetCells(ACol, ARow: Integer; const AValue: String);
var
  LIndex: Integer;
  LText: TNyxText;
begin

  if not ReaderAttached then
  begin
    inherited SetCells(ACol, ARow, AValue);
    Exit;
  end;

  if TNyxText(AValue) = FReader(ACol, ARow) then
  begin
    ClearOverride(ACol, ARow);
    Exit;
  end;
  { Preserve LCL's input/validation path for an actual draft. It may normalize
    the text or publish a store update before returning. Record its final value,
    not the original unvalidated argument. Accepted adapter writes use the
    dedicated ClearOverride path and create no eager physical cell values. }
  inherited SetCells(ACol, ARow, AValue);

  if not ReaderAttached then
  begin
    Exit;
  end;
  LText := inherited GetCells(ACol, ARow);

  if LText = FReader(ACol, ARow) then
  begin
    ClearOverride(ACol, ARow);
    Exit;
  end;
  LIndex := OverrideIndex(ACol, ARow);

  if LIndex < 0 then
  begin
    LIndex := Length(FOverrides);
    SetLength(FOverrides, LIndex + 1);
    FOverrides[LIndex].Column := ACol;
    FOverrides[LIndex].Row := ARow;
  end;
  FOverrides[LIndex].Text := LText;

  if (ACol >= 0) and (ACol < ColCount) and (ARow >= 0) and (ARow < RowCount) then
  begin
    InvalidateCell(ACol, ARow);
  end;
end;

end.
