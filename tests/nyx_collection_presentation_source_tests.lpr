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
program nyx_collection_presentation_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils,
  {$ifndef PAS2JS}Classes, nyx.bytes,{$endif}
  {$ifdef NYX_COMPILED_PRESENTATION}nyx.generated.presentation,{$endif}
  nyx.text, nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.source,
  nyx.types, nyx.collections, nyx.collections.view.types,
  nyx.collections.presentation, nyx.collections.workbench.policy
  {$ifdef PAS2JS}, Web{$endif};

const
  CField: TNyxText = 'Command 🌙';
  CValue: TNyxText = 'service 🌙' + #0 + #9 + '"value"' + #13#10 + 'tail';
  CImport: TNyxText = 'nyx.collections.presentation';
  CPrefix: TNyxText = '{ Handwritten presentation preface 🌙 }';
  CSuffix: TNyxText = '// Handwritten presentation suffix 🌙';

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
    .TableWorkbench(NyxTableWorkbench.AllowCopy([]));
end;

function Fixture(AAuthored: Boolean): TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
  LDensity: TNyxTableRowDensity;
  LSpec: TNyxCollectionViewSpec;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(NyxCollection('services'),
      NyxCollectionSchema.Text(NyxTextField(CField), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('services'), 'service 🌙'))
        .WithValue(NyxTextField(CField), CValue)]);
    LPage := NewNyxPage('services-page');
    Result.AddPage(LPage);
    for LDensity := Low(TNyxTableRowDensity) to High(TNyxTableRowDensity) do
    begin
      LTable := NewNyxTable('service-table-' + TNyxText(IntToStr(Ord(LDensity))));
      LSpec := BaseSpec;

      if AAuthored then
      begin
        LSpec := LSpec.TablePresentation(NyxTablePresentation.Density(LDensity));
        case LDensity of
          ntrdCompact:
            begin
              LSpec := LSpec.ColumnText(NyxColumnText.Clip);
            end;
          ntrdNormal:
            begin
              LSpec := LSpec.ColumnText(NyxColumnText.Ellipsis);
            end;
          ntrdComfortable:
            begin
              LSpec := LSpec.ColumnText(NyxColumnText.Wrap(High(Integer)))
                .TablePresentation(NyxTablePresentation.Density(LDensity)
                  .Header(NyxTableHeader.Lines(2).MinimumHeight(41)));
            end;
        end;
      end;
      LTable.Binds.Collection(LSpec);
      LPage.Add(LTable);
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
  Check(LPosition > 0, 'source change targets an actual emitted expression');
  Result := System.Copy(ASource, 1, LPosition - 1) + AAfter +
    System.Copy(ASource, LPosition + Length(ABefore), MaxInt);
end;

function ImportCount(const ASource: TNyxText): Integer;
var
  LImports: TNyxImportClause;
  LIndex: Integer;
begin
  Result := 0;
  LImports := ReadNyxImports(ASource, nisInterface);
  for LIndex := 0 to LImports.Count - 1 do
  begin

    if SameText(LImports.UnitAt(LIndex).Name, CImport) then
    begin
      Inc(Result);
    end;
  end;
end;

procedure Refuses(AWorkspace: TNyxSourceWorkspace; ADocument: TNyxDocument;
  const ASource: TNyxText);
var
  LCandidate: TNyxDocument;
  LDesign: TNyxText;
  LWorkspace: TNyxText;
  LRefused: Boolean;
begin
  LCandidate := nil;
  LDesign := TNyxCodec.Encode(ADocument);
  LWorkspace := AWorkspace.Snapshot;
  LRefused := False;
  try
    try
      LCandidate := AWorkspace.Candidate(ADocument, ASource);
    except
      on E: ENyxSource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(ADocument) = LDesign) and
      (AWorkspace.Snapshot = LWorkspace),
      'invalid typed presentation source preserves the exact accepted pair');
  finally
    LCandidate.Free;
  end;
end;

procedure SourceCases;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LLegacy: TNyxText;
  LSource: TNyxText;
  LChanged: TNyxText;
  LIndex: Integer;
  LTable: INyxTable;
begin
  LDocument := Fixture(False);
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LLegacy := TNyxCodegen.Generate(LDocument, 'nyx.generated.presentation');
    Check((ImportCount(LLegacy) = 0) and (Pos('.ColumnText(', LLegacy) = 0) and
      (Pos('.TablePresentation(', LLegacy) = 0), 'absent policies add no source dependency');
    LWorkspace.Accept(LDocument, CPrefix + #10 + LLegacy + #10 + CSuffix);
    LDocument.Find('service-table-0').SetCollectionView(BaseSpec
      .TablePresentation(NyxTablePresentation).ColumnText(NyxColumnText));
    LSource := LWorkspace.Render(LDocument);
    Check((ImportCount(LSource) = 1) and (Pos('.ColumnText(NyxColumnText.Clip)', LSource) > 0) and
      (Pos('.Density(ntrdNormal)', LSource) > 0) and
      (Pos(CPrefix, LSource) > 0) and (Pos(CSuffix, LSource) > 0),
      'explicit defaults use typed source and retain handwritten Unicode text');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'explicit defaults retain presence through source reading');
    FreeAndNil(LCandidate);
    LDocument.Find('service-table-0').SetCollectionView(BaseSpec);
    Check(TNyxCodegen.Generate(LDocument, 'nyx.generated.presentation') = LLegacy,
      'reset reproduces exact legacy generated bytes');
    LSource := LWorkspace.Render(LDocument);
    Check((ImportCount(LSource) = 1) and (Pos('.TablePresentation(', LSource) = 0),
      'source reset preserves an authored import for handwritten application code');
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;

  LDocument := Fixture(True);
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.presentation');
    Check((ImportCount(LSource) = 1) and (Pos(': INyxTable;', LSource) > 0) and
      (Pos('.Density(ntrdCompact)', LSource) > 0) and
      (Pos('.Density(ntrdNormal)', LSource) > 0) and
      (Pos('.Density(ntrdComfortable)', LSource) > 0) and
      (Pos('.ColumnText(NyxColumnText.Ellipsis)', LSource) > 0) and
      (Pos('.Wrap(2147483647)', LSource) > 0),
      'all densities and overflow modes generate specialized fluent Pascal without a line-count authoring cap');
    LWorkspace.Accept(LDocument, LSource);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'all presentation choices replay exact data and workbench coexistence');
    FreeAndNil(LCandidate);
    Check(LWorkspace.Render(LDocument) = LSource, 'presentation synchronization is idempotent');
    Check((Pos('.Header(NyxTableHeader.Lines(2)', LSource) > 0) and
      (Pos('.MinimumHeight(41)))', LSource) > 0),
      'independent header settings generate typed readable fluent blocks');
    LChanged := ReplaceOnce(LSource, '.MinimumHeight(41)))',
      '.MinimumHeight(41)).UseDefaultHeader)');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(not LCandidate.Find('service-table-2').CollectionView.TablePresentationPolicy.HasHeader and
      (LCandidate.Find('service-table-2').CollectionView.TablePresentationPolicy.RowDensity = ntrdComfortable),
      'parameterless header reset preserves body density and the remaining view policy');
    FreeAndNil(LCandidate);
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.MinimumHeight(41)', '.MinimumHeight(-1)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Lines(2)', '.Lines(''2'')'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Lines(2)', '.Lines(ntrdNormal)'));
    { The reset belongs to the view, outside the policy argument. }
    LChanged := ReplaceOnce(LSource, '.ColumnText(NyxColumnText.Clip)',
      '.ColumnText(NyxColumnText().Clip()).UseDefaultColumnText');
    LChanged := ReplaceOnce(LChanged,
      '.TablePresentation(NyxTablePresentation.Density(ntrdCompact))',
      '.TablePresentation(NyxTablePresentation().Density(ntrdCompact)).UseDefaultTablePresentation()');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(not LCandidate.Find('service-table-0').CollectionView.HasPresentationPolicy and
      LCandidate.Find('service-table-1').CollectionView.HasPresentationPolicy,
      'bare and parenthesized resets remove only their local independent choices');
    FreeAndNil(LCandidate);
    LChanged := ReplaceOnce(LSource, 'NyxColumnText.Wrap(2147483647)', 'NyxColumnText.Wrap');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(LCandidate.Find('service-table-2').CollectionView.ColumnAt(0).TextPolicy.MaximumLines = 3,
      'bare Wrap uses the public default line count');
    FreeAndNil(LCandidate);
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Density(ntrdCompact)', '.Density(''compact'')'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Density(ntrdCompact)', '.Density(0)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Density(ntrdCompact)', '.Density(nwleLF)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Wrap(2147483647)', '.Wrap(0)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Wrap(2147483647)', '.Wrap(-1)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Wrap(2147483647)', '.Wrap(1.5)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, 'NyxColumnText.Clip', 'NyxTablePresentation'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, 'NyxColumnText.Clip', 'NyxColumnText.Ellipsis(True)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, 'NyxColumnText.Clip', 'NyxColumnText.Unknown'));
    { Import discovery includes an uninstantiated reusable definition. }
    LTable := NewNyxTable('unused-presentation-definition');
    LTable.Binds.Collection(BaseSpec.ColumnText(NyxColumnText.Ellipsis));
    for LIndex := 0 to 2 do
    begin
      LDocument.Find('service-table-' + TNyxText(IntToStr(LIndex))).SetCollectionView(BaseSpec);
    end;
    LDocument.AddComponent(LTable);
    Check(ImportCount(TNyxCodegen.Generate(LDocument)) = 1,
      'unused reusable presentation definitions still own their required source import');
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

{$ifndef PAS2JS}
{ Qualification owns an existing ASCII scratch directory. The emitted Pascal
  crosses the file boundary as exact UTF-8 bytes, independently of LCL/locale. }
procedure SaveGeneratedText(const ADirectory: String; const AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ADirectory) +
    'nyx.generated.presentation.pas', fmCreate);
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

procedure GeneratedCases;
var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
begin
  {$ifdef NYX_COMPILED_PRESENTATION}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := Fixture(True);
  {$endif}
  LExpected := Fixture(True);
  try
    Check(TNyxCodec.Encode(LDocument) = TNyxCodec.Encode(LExpected),
      'the complete generated application retains exact presentation, workbench and Unicode/NUL data');
    Check(LDocument.Find('service-table-0').CollectionView.ColumnAt(0).TextPolicy.Overflow = nctoClip,
      'explicit Clip remains present');
    Check(LDocument.Find('service-table-1').CollectionView.ColumnAt(0).TextPolicy.Overflow = nctoEllipsis,
      'explicit Ellipsis remains present');
    Check(LDocument.Find('service-table-2').CollectionView.ColumnAt(0).TextPolicy.MaximumLines = High(Integer),
      'large authored Wrap count remains exact without implying runtime admission');
    Check(LDocument.Find('service-table-2').CollectionView.TablePresentationPolicy.HasHeader and
      (LDocument.Find('service-table-2').CollectionView.TablePresentationPolicy.HeaderPolicy.ReservedLines = 2) and
      (LDocument.Find('service-table-2').CollectionView.TablePresentationPolicy.HeaderPolicy.MinimumLogicalHeight = 41),
      'the independently compiled companion retains the complete header policy');
    {$ifndef PAS2JS}
    {$ifndef NYX_COMPILED_PRESENTATION}

    if ParamCount = 1 then
    begin
      SaveGeneratedText(ParamStr(1),
        TNyxCodegen.Generate(LDocument, 'nyx.generated.presentation'));
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
    {$ifndef NYX_COMPILED_PRESENTATION}SourceCases;{$endif}
    GeneratedCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-presentation-source', 'passed');
    document.body.setAttribute('data-presentation-source-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' presentation source/generated application checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-presentation-source', 'failed: ' + E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
