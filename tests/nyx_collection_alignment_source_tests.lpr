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
program nyx_collection_alignment_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.source, nyx.collections, nyx.collections.view.types, nyx.collections.alignment,
  nyx.collections.presentation, nyx.collections.workbench.policy
  {$ifndef PAS2JS}, Classes, nyx.bytes{$endif}
  {$ifdef NYX_COMPILED_ALIGNMENT}, nyx.generated.alignment{$endif}
  {$ifdef PAS2JS}, Web{$endif};

const
  CField: TNyxText = 'Command 🌙';
  CValue: TNyxText = 'service 🌙' + #0#9 + '"quoted"' + #13#10 + 'tail';
  CPrefix: TNyxText = '{ A handwritten alignment introduction 🌙 }';
  CSuffix: TNyxText = '// A handwritten alignment conclusion 🌙';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function BaseSpec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(NyxCollection('services'))
    .Column(NyxTextField(CField), 'Command line', cmEditable)
    .ColumnText(NyxColumnText.Wrap(4))
    .TableWorkbench(NyxTableWorkbench.AllowCopy([]));
end;

function TableID(AHorizontal: TNyxColumnHorizontalAlignment;
  AVertical: TNyxColumnVerticalAlignment): TNyxText;
begin
  Result := 'alignment-' + TNyxText(IntToStr(Ord(AHorizontal))) + '-' +
    TNyxText(IntToStr(Ord(AVertical)));
end;

function Fixture(AAligned: Boolean): TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
  LSpec: TNyxCollectionViewSpec;
  LHorizontal: TNyxColumnHorizontalAlignment;
  LVertical: TNyxColumnVerticalAlignment;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(NyxCollection('services'),
      NyxCollectionSchema.Text(NyxTextField(CField), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('services'), 'service 🌙'))
        .WithValue(NyxTextField(CField), CValue)]);
    LPage := NewNyxPage('services-page');
    Result.AddPage(LPage);
    for LHorizontal := Low(TNyxColumnHorizontalAlignment) to High(TNyxColumnHorizontalAlignment) do
    begin
      for LVertical := Low(TNyxColumnVerticalAlignment) to High(TNyxColumnVerticalAlignment) do
      begin
        LSpec := BaseSpec;

        if AAligned then
        begin
          LSpec := LSpec.ColumnAlignment(NyxColumnAlignment.Horizontal(LHorizontal)
            .Vertical(LVertical));
        end;
        LTable := NewNyxTable(TableID(LHorizontal, LVertical));
        LTable.Binds.Collection(LSpec);
        LPage.Add(LTable);
      end;
    end;
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function ReplaceOnce(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(ABefore, ASource);
  Check(LPosition > 0, 'replace an actual generated alignment expression');
  Result := Copy(ASource, 1, LPosition - 1) + AAfter +
    Copy(ASource, LPosition + Length(ABefore), MaxInt);
end;

procedure Refuses(AWorkspace: TNyxSourceWorkspace; ADocument: TNyxDocument;
  const ASource: TNyxText);
var
  LCandidate: TNyxDocument;
  LBefore: TNyxText;
  LSnapshot: TNyxText;
  LRefused: Boolean;
begin
  LCandidate := nil;
  LBefore := TNyxCodec.Encode(ADocument);
  LSnapshot := AWorkspace.Snapshot;
  LRefused := False;
  try
    try
      LCandidate := AWorkspace.Candidate(ADocument, ASource);
    except
      on ENyxSource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(ADocument) = LBefore) and
      (AWorkspace.Snapshot = LSnapshot), 'invalid alignment source preserves accepted pair');
  finally
    LCandidate.Free;
  end;
end;

procedure SourceCases;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LLegacy: TNyxText;
  LChanged: TNyxText;
begin
  LDocument := Fixture(False);
  LCandidate := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LLegacy := TNyxCodegen.Generate(LDocument, 'nyx.generated.alignment');
    Check(Pos('nyx.collections.alignment', LLegacy) = 0,
      'absent alignment adds no generated dependency');
    LWorkspace.Accept(LDocument, CPrefix + #10 + LLegacy + #10 + CSuffix);
    LDocument.Find(TableID(nchaStart, ncvaTop)).SetCollectionView(BaseSpec
      .ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd)));
    LSource := LWorkspace.Render(LDocument);
    Check((Pos('.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd))', LSource) > 0) and
      (Pos(CPrefix, LSource) > 0) and (Pos(CSuffix, LSource) > 0),
      'one-axis source is typed and retains handwritten supplementary Unicode');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'one-axis source preserves exact absent vertical policy and RAW data');
    FreeAndNil(LCandidate);
    LDocument.Find(TableID(nchaStart, ncvaTop)).SetCollectionView(BaseSpec);
    Check(TNyxCodegen.Generate(LDocument, 'nyx.generated.alignment') = LLegacy,
      'alignment reset returns exact earlier generated source');
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;

  LDocument := Fixture(True);
  LCandidate := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.alignment');
    Check((Pos(': INyxTable;', LSource) > 0) and
      (Pos('nyx.collections.alignment', LSource) > 0), 'specialized generated controls import typed alignment');
    LWorkspace.Accept(LDocument, LSource);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'all nine axes pairs replay exact alignment, text policy, workbench and Unicode/NUL data');
    FreeAndNil(LCandidate);
    Check(LWorkspace.Render(LDocument) = LSource, 'alignment synchronization is idempotent');
    LChanged := ReplaceOnce(LSource, '.Horizontal(nchaStart).Vertical(ncvaTop)',
      '().Horizontal(nchaStart).Vertical(ncvaTop).UseDefaultHorizontal()');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(not LCandidate.Find(TableID(nchaStart, ncvaTop)).CollectionView.ColumnAt(0)
      .AlignmentPolicy.HasHorizontal, 'axis reset preserves only the explicit other axis');
    FreeAndNil(LCandidate);
    LChanged := ReplaceOnce(LSource,
      '.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaStart).Vertical(ncvaTop))',
      '.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaStart).Vertical(ncvaTop))'
        + '.UseDefaultColumnAlignment');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(not LCandidate.Find(TableID(nchaStart, ncvaTop)).CollectionView.HasColumnAlignment and
      LCandidate.Find(TableID(nchaCenter, ncvaMiddle)).CollectionView.HasColumnAlignment,
      'column reset clears only one alignment and preserves peers');
    FreeAndNil(LCandidate);
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Horizontal(nchaStart)', '.Horizontal(''start'')'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Horizontal(nchaStart)', '.Horizontal(0)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Horizontal(nchaStart)', '.Horizontal(ncvaTop)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Vertical(ncvaTop)', '.Vertical(nchaStart)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, 'NyxColumnAlignment.Horizontal(nchaStart)',
      'NyxColumnText.Clip'));
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

procedure GeneratedCases;
var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  {$ifndef PAS2JS}
  LStream: TFileStream;
  LBytes: TNyxBytes;
  {$endif}
begin
  {$ifdef NYX_COMPILED_ALIGNMENT}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := Fixture(True);
  {$endif}
  LExpected := Fixture(True);
  try
    Check(TNyxCodec.Encode(LDocument) = TNyxCodec.Encode(LExpected),
      'independently compiled generated controls reconstruct the complete authored packet');
    {$ifndef PAS2JS}
    {$ifndef NYX_COMPILED_ALIGNMENT}

    if ParamCount = 1 then
    begin
      LBytes := NyxEncodeUTF8(TNyxCodegen.Generate(LDocument, 'nyx.generated.alignment'));
      LStream := TFileStream.Create(ParamStr(1), fmCreate);
      try

        if Length(LBytes) > 0 then
        begin
          LStream.WriteBuffer(LBytes[0], Length(LBytes));
        end;
      finally
        LStream.Free;
      end;
    end;
    {$endif}
    {$endif}
  finally
    LExpected.Free;
    LDocument.Free;
  end;
end;

begin
  try
    {$ifndef NYX_COMPILED_ALIGNMENT}
    SourceCases;
    {$endif}
    GeneratedCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-column-alignment-source', 'passed');
    document.body.setAttribute('data-column-alignment-source-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' column alignment source checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-column-alignment-source', 'failed');
      document.body.setAttribute('data-column-alignment-source-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
