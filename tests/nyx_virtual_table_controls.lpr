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
program nyx_virtual_table_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Forms, SysUtils, Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.model, nyx.codec, nyx.types, nyx.collections,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.query, nyx.collections.mount,
  nyx.collections.lcl.grid, nyx.render.lcl, nyx.generated.view;

const
  CRows = 4096;
  CDraft: TNyxText = 'A private draft / 🌙';
  CLiteral: TNyxText = 'Literal text / 🌙';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxCollection.Create('On-demand native table: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Optional capture is a real native paint of the unchanged English MCP layout.
  Runtime rows are independent test data; no Studio project or design is edited. }
procedure Capture(AHost: TForm);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(AHost.Width, AHost.Height);
    AHost.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(ParamStr(1), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LAuthored: TNyxText;
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LGrid: TNyxCollectionStringGrid;
  LPlain: TNyxCollectionStringGrid;
  LView: INyxCollectionView;
  LStore: INyxCollection;
  LMount: INyxCollectionMount;
  LKey: TNyxCollectionRef;
  LRows: array of TNyxCollectionItem;
  LIndex: Integer;
  LRefused: Boolean;
  LReads: Int64;
begin
  LPlain := TNyxCollectionStringGrid.Create(nil);
  try
    LPlain.Cells[0, 1] := CLiteral;
    Check(not LPlain.ReaderAttached and (TNyxText(LPlain.Cells[0, 1]) = CLiteral),
      'unbound grid retains ordinary literal cells');
    LRefused := False;
    try
      LPlain.AttachReader(nil);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LPlain.ReaderAttached, 'nil reader refuses without adoption');
  finally
    LPlain.Free;
  end;
  LDocument := BuildNyxDocument;
  LAuthored := TNyxCodec.Encode(LDocument);
  LRenderer := TNyxLCLRenderer.Create;
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 900, 600);
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Check(LRenderer.ControlFor('work-table') is TNyxCollectionStringGrid,
      'ordinary semantic table uses the owned LCL subclass');
    LGrid := TNyxCollectionStringGrid(LRenderer.ControlFor('work-table'));
    LView := LRenderer.CollectionView('work-table');
    Check(LGrid.ReaderAttached and (LGrid.OverrideCount = 0), 'declared binding attaches a sparse reader');
    LKey := LView.Spec.Key;
    SetLength(LRows, CRows);
    for LIndex := 0 to CRows - 1 do
    begin
      LRows[LIndex] := NyxCollectionItem(NyxItem(LKey, 'item-' + IntToStr(LIndex)))
        .WithValue(NyxTextField('task'), 'Work item ' + IntToStr(LIndex + 1))
        .WithValue(NyxIntegerField('priority'), LIndex mod 5)
        .WithValue(NyxTextField('status'), 'Ready');
    end;
    { Reuse the same admitted specification and actual control with an independent
      runtime store. This changes neither authored defaults nor the exported unit. }
    LStore := NewNyxCollection(LKey, LView.Store.Snapshot.Schema, LRows);
    LView := NewNyxCollectionView(LStore, LView.Spec, cpTable);
    LRenderer.CollectionMount('work-table').Disconnect;
    LMount := LRenderer.BindCollection('work-table', LView);
    Check((LGrid.RowCount = CRows + 1) and (LGrid.OverrideCount = 0),
      'large source supplies geometry without per-row draft storage');
    LGrid.ResetReadCount;
    Check(LGrid.Cells[0, CRows] = 'Work item 4096', 'distant API cell reads its exact source value');
    Check(LGrid.SourceReadCount = 1, 'single distant read requests one cell');
    LHost.Show;
    Application.ProcessMessages;
    LGrid.ResetReadCount;
    LGrid.Repaint;
    LReads := LGrid.SourceReadCount;
    Check((LReads > 0) and (LReads < CRows), 'real viewport paint reads a bounded visible subset');
    Check(LGrid.OverrideCount = 0, 'painting creates no widget draft overlays');
    WriteLn('PAINT ', LReads, ' source requests / ', CRows, ' model rows');
    LStore.Update(NyxCollectionItem(LRows[CRows - 1].Ref).WithValue(NyxIntegerField('priority'), 77));
    Check(LGrid.Cells[1, CRows] = '77', 'offscreen update remains available without materialization');
    LGrid.Cells[0, 1] := CDraft;
    Check((TNyxText(LGrid.Cells[0, 1]) = CDraft) and (LGrid.OverrideCount = 1),
      'explicit local cell draft retains exact text');
    LStore.Update(NyxCollectionItem(LRows[CRows - 1].Ref).WithValue(NyxIntegerField('priority'), 78));
    Check(TNyxText(LGrid.Cells[0, 1]) = CDraft, 'unrelated publication preserves local draft');
    Check(LMount.EditCell(LRows[0].Ref, 0, 'Work item 1'), 'no-op edit admits normalization');
    Check((LGrid.Cells[0, 1] = 'Work item 1') and (LGrid.OverrideCount = 0),
      'normalization withdraws the exact local overlay');
    LGrid.Cells[0, 1] := CDraft;
    LView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('priority'), nsdDescending));
    Check((LGrid.Cells[1, 1] = '78') and (LGrid.OverrideCount = 0),
      'query reorder clears displaced positional draft and serves the new row');
    LView.Select(LRows[CRows - 1].Ref);
    Check(LGrid.Row = 1, 'selection retains exact query identity');
    Capture(LHost);
    LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(100)));
    Check((LGrid.RowCount = 2) and (LGrid.Cells[0, 1] = '') and (LGrid.OverrideCount = 0),
      'empty result exposes an empty native spare row');
    Check(TNyxCodec.Encode(LDocument) = LAuthored, 'runtime viewport work preserves exact authored design');
    LMount.Disconnect;
    Check(not LGrid.ReaderAttached and not LMount.Connected, 'disconnect ends receiver borrowing');
    LReads := LGrid.SourceReadCount;
    LStore.Update(NyxCollectionItem(LRows[0].Ref).WithValue(NyxIntegerField('priority'), 100));
    Check(LGrid.SourceReadCount = LReads, 'retired control performs no source reads on publication');
  finally
    LRenderer.Free;
    LHost.Free;
    LDocument.Free;
  end;
end;

begin
  Application.Initialize;
  try
    Run;
    WriteLn('PASS ', GChecks, ' on-demand native table checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
