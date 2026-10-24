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
program nyx_collection_workbench_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif}
  {$ifdef NYX_COMPILED_WORKBENCH}nyx.generated.view,{$endif}
  nyx.text, nyx.bytes, nyx.model, nyx.controls, nyx.types, nyx.codec,
  nyx.codegen, nyx.source, nyx.composition, nyx.collections,
  nyx.collections.view.types, nyx.collections.workbench.policy,
  nyx.programs.declarations, nyx.programs.contract
  {$ifdef PAS2JS}, Web{$endif};

const
  CName: TNyxText = 'Name 🌙';
  CValue: TNyxText = 'value 🌙' + #0 + #9 + '"quoted"' + #13#10 + 'end';
  CImport: TNyxText = 'nyx.collections.workbench.policy';
  CPrefix: TNyxText = '{ Keep handwritten prefix 🌙 / 漢字 }';
  CSuffix: TNyxText = '// Keep handwritten suffix 🌙 / 漢字';

var
  GChecks: Integer;
  GSource: TNyxText;
  GDesign: TNyxText;
  GLegacySource: TNyxText;
  GLegacyDesign: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
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

function ReplaceOnce(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(ABefore, ASource);
  Check(LPosition > 0, 'the source oracle changes an actual emitted expression');
  Result := System.Copy(ASource, 1, LPosition - 1) + AAfter +
    System.Copy(ASource, LPosition + Length(ABefore), MaxInt);
end;

function BaseSpec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(NyxCollection('source-rows'))
    .Column(NyxTextField(CName), CName, cmEditable);
end;

function AllPolicy: TNyxTableWorkbenchPolicy;
begin
  Result := NyxTableWorkbench.RangeSelection(True)
    .AllowCopy([nwcaCell, nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange])
    .CopyFields(nwcfAllArrangement).Headers(False).LineEnding(nwleCRLF).FocusedDraft(True);
end;

function Fixture(AWithPolicy: Boolean): TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
  LIndex: Integer;
  LSpec: TNyxCollectionViewSpec;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(NyxCollection('source-rows'),
      NyxCollectionSchema.Text(NyxTextField(CName), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('source-rows'), 'row 🌙'))
        .WithValue(NyxTextField(CName), CValue)]);
    { This uninvoked authored program must coexist with the new import and
      collection builder. It introduces no execution or clipboard authority. }
    Result.Programs.Define(NyxProgramRef('Uninvoked workbench companion'),
      NewNyxProgramDefinition('begin end.', NyxProgramContract));
    LPage := NewNyxPage('source-page');
    Result.AddPage(LPage);
    for LIndex := 0 to 3 do
    begin
      LTable := NewNyxTable('source-table-' + TNyxText(IntToStr(LIndex)));
      LSpec := BaseSpec;

      if AWithPolicy then
      begin
        case LIndex of
          0:
            begin
              LSpec := LSpec.TableWorkbench(NyxTableWorkbench);
            end;
          1:
            begin
              LSpec := LSpec.TableWorkbench(AllPolicy);
            end;
          2:
            begin
              LSpec := LSpec.TableWorkbench(NyxTableWorkbench.RangeSelection(True).AllowCopy([]));
            end;
          3:
            begin
              LSpec := LSpec.TableWorkbench(NyxTableWorkbench.AllowCopy([nwcaSelectedPage])
                .Headers(False).FocusedDraft(True));
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

procedure RefusesSource(AWorkspace: TNyxSourceWorkspace; ADocument: TNyxDocument;
  const ASource, AReason: TNyxText);
var
  LCandidate: TNyxDocument;
  LBeforeDesign: TNyxText;
  LBeforeWorkspace: TNyxText;
  LRefused: Boolean;
begin
  LCandidate := nil;
  LBeforeDesign := TNyxCodec.Encode(ADocument);
  LBeforeWorkspace := AWorkspace.Snapshot;
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
    Check(LRefused and (TNyxCodec.Encode(ADocument) = LBeforeDesign) and
      (AWorkspace.Snapshot = LBeforeWorkspace), AReason + '; accepted pair remains unchanged');
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
  LBaseline: TNyxText;
  LChanged: TNyxText;
  LIndex: Integer;
begin
  LDocument := Fixture(False);
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    GLegacyDesign := TNyxCodec.Encode(LDocument);
    GLegacySource := TNyxCodegen.Generate(LDocument);
    Check((ImportCount(GLegacySource) = 0) and (Pos('.TableWorkbench(', GLegacySource) = 0),
      'an absent policy emits no new dependency or source call');
    LSource := EditNyxImport(GLegacySource, nisInterface, niaAdd, NyxPascalUnit('SysUtils'));
    LSource := ReplaceOnce(LSource, 'SysUtils', 'SysUtils { Keep authored import note 🌙 }');
    LSource := CPrefix + #10 + LSource + #10 + CSuffix;
    LWorkspace.Accept(LDocument, LSource);
    LBaseline := LWorkspace.Render(LDocument);
    LDocument.Find('source-table-0').SetCollectionView(BaseSpec.TableWorkbench(NyxTableWorkbench));
    LSource := LWorkspace.Render(LDocument);
    Check((ImportCount(LSource) = 1) and
      (Pos('.TableWorkbench(NyxTableWorkbench)', LSource) > 0) and
      (Pos(CPrefix, LSource) > 0) and (Pos(CSuffix, LSource) > 0) and
      (Pos('SysUtils { Keep authored import note 🌙 }', LSource) > 0),
      'explicit defaults promote one required import and preserve handwritten Unicode frames/comments');
    LDocument.Find('source-table-0').SetCollectionView(BaseSpec);
    LSource := LWorkspace.Render(LDocument);
    Check((ImportCount(LSource) = 1) and (Pos('.TableWorkbench(', LSource) = 0) and
      (Pos(CPrefix, LSource) > 0) and (Pos(CSuffix, LSource) > 0),
      'policy reset preserves an authored import that handwritten application code may still use');
    Check(TNyxCodegen.Generate(LDocument) = GLegacySource,
      'fresh generation after reset reproduces the exact absence source');
    LWorkspace.Reset;
    LWorkspace.Accept(LDocument, LBaseline);
    LDocument.Find('source-table-0').SetCollectionView(BaseSpec.TableWorkbench(NyxTableWorkbench));
    LSource := LWorkspace.Render(LDocument);
    LSource := ReplaceOnce(LSource, CImport, 'NyX.CoLlEcTiOnS.WoRkBeNcH.PoLiCy');
    LWorkspace.Accept(LDocument, LSource);
    LDocument.Find('source-table-1').SetCollectionView(BaseSpec.TableWorkbench(AllPolicy));
    LDocument.Find('source-table-2').SetCollectionView(BaseSpec.TableWorkbench(
      NyxTableWorkbench.RangeSelection(True).AllowCopy([])));
    LDocument.Find('source-table-3').SetCollectionView(BaseSpec.TableWorkbench(
      NyxTableWorkbench.AllowCopy([nwcaSelectedPage]).Headers(False).FocusedDraft(True)));
    LSource := LWorkspace.Render(LDocument);
    Check((ImportCount(LSource) = 1) and
      (Pos('NyX.CoLlEcTiOnS.WoRkBeNcH.PoLiCy', LSource) > 0) and
      (Pos('.AllowCopy([nwcaCell, nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange])', LSource) > 0) and
      (Pos('.AllowCopy([])', LSource) > 0) and
      (Pos('.CopyFields(nwcfAllArrangement)', LSource) > 0) and
      (Pos('.LineEnding(nwleCRLF)', LSource) > 0) and
      (Pos(': INyxTable;', LSource) > 0) and
      (Pos('ConfigureClipboard', LSource) = 0) and
      (Pos('ClipboardHost', LSource) = 0),
      'typed generation covers all permissions/options with specialized tables and no persisted service authority');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'all authored action choices replay the complete accepted design through the confined source reader');
    FreeAndNil(LCandidate);
    Check(LWorkspace.Render(LDocument) = LSource, 'workbench source synchronization is idempotent');
    LChanged := ReplaceOnce(LSource, '.CopyFields(nwcfAllArrangement)', '.CopyFields(nwcfVisiblePresentation)');
    LChanged := ReplaceOnce(LChanged, '.LineEnding(nwleCRLF)', '.LineEnding(nwleLF)');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check((LCandidate.Find('source-table-1').CollectionView.TableWorkbenchPolicy.FieldChoice = nwcfVisiblePresentation) and
      (LCandidate.Find('source-table-1').CollectionView.TableWorkbenchPolicy.Lines = nwleLF),
      'explicit typed default enum symbols parse independently of generator default elision');
    FreeAndNil(LCandidate);
    LChanged := ReplaceOnce(LSource, '.TableWorkbench(NyxTableWorkbench)',
      '.TableWorkbench(NyxTableWorkbench()).UseDefaultTableWorkbench');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(not LCandidate.Find('source-table-0').CollectionView.HasTableWorkbenchPolicy and
      LCandidate.Find('source-table-1').CollectionView.HasTableWorkbenchPolicy,
      'parameterless factory parentheses and bare default reset preserve independent policy ownership');
    FreeAndNil(LCandidate);
    LChanged := ReplaceOnce(LChanged, '.UseDefaultTableWorkbench', '.UseDefaultTableWorkbench()');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(not LCandidate.Find('source-table-0').CollectionView.HasTableWorkbenchPolicy,
      'default reset also accepts ordinary Pascal parentheses');
    FreeAndNil(LCandidate);
    RefusesSource(LWorkspace, LDocument, ReplaceOnce(LSource, '.RangeSelection(True)', '.RangeSelection(''True'')'),
      'Boolean flags reject a textual substitute');
    RefusesSource(LWorkspace, LDocument, ReplaceOnce(LSource, 'nwcaCellRange]', '''cell-range'']'),
      'copy permissions reject interchange strings');
    RefusesSource(LWorkspace, LDocument, ReplaceOnce(LSource, 'nwcaCellRange]', 'nwcaCell]'),
      'duplicate copy permissions refuse');
    RefusesSource(LWorkspace, LDocument, ReplaceOnce(LSource, 'nwcaCellRange]', 'nwcaCellRange,]'),
      'trailing set comma cannot omit a typed action');
    RefusesSource(LWorkspace, LDocument, ReplaceOnce(LSource, '.CopyFields(nwcfAllArrangement)', '.CopyFields(nwleLF)'),
      'an enum from a different family cannot change fields');
    RefusesSource(LWorkspace, LDocument, ReplaceOnce(LSource, '.LineEnding(nwleCRLF)', '.LineEnding(''crlf'')'),
      'line endings reject arbitrary saved strings');
    RefusesSource(LWorkspace, LDocument, ReplaceOnce(LSource, '.Headers(False)', '.Headers(0)'),
      'headers require the Boolean family');
    RefusesSource(LWorkspace, LDocument, ReplaceOnce(LSource, '.FocusedDraft(True)', '.FocusedDraft(1)'),
      'focused draft opt-in cannot use a number');
    GSource := LSource;
    GDesign := TNyxCodec.Encode(LDocument);
    for LIndex := 0 to 3 do
    begin
      LDocument.Find('source-table-' + TNyxText(IntToStr(LIndex))).SetCollectionView(BaseSpec);
    end;
    Check(TNyxCodegen.Generate(LDocument) = GLegacySource,
      'removing every authored policy leaves original fresh source byte-for-byte');
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

{ Real reusable composition checks full local bindings, policy-only defaults,
  explicit masks and inheritance without relying on a Studio convenience API. }
procedure ReusableCases;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LRoot: TNyxNode;
  LRecipe: INyxColumn;
  LPage: INyxPage;
  LInstance: INyxComponent;
  LTable: INyxTable;
  LOverride: TNyxNode;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  LRoot := nil;
  try
    LDocument.Collections.Define(NyxCollection('source-rows'),
      NyxCollectionSchema.Text(NyxTextField(CName), ''), []);
    LRecipe := NewNyxColumn('workbench-recipe');
    LTable := NewNyxTable('definition-table');
    LTable.Configure.PartName(NyxPart('records')).Done;
    LTable.Binds.Collection(BaseSpec.TableWorkbench(AllPolicy));
    LRecipe.Node.Add(LTable);
    LDocument.AddComponent(LRecipe);
    LPage := NewNyxPage('reusable-page');
    LDocument.AddPage(LPage);
    LInstance := NewNyxComponent('local-instance');
    LInstance.Configure.Component(NyxComponent('workbench-recipe')).Done;
    LOverride := LInstance.Node.OverridePart(NyxPart('records'), noProperties).Named('local-override');
    LOverride.SetCollectionView(BaseSpec.TableWorkbench(NyxTableWorkbench));
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('inherited-instance');
    LInstance.Configure.Component(NyxComponent('workbench-recipe')).Done;
    LPage.Add(LInstance);
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    Check(not LRoot.Children[0].Part(NyxPart('records')).CollectionView.TableWorkbenchPolicy.RangeSelectionEnabled and
      LRoot.Children[1].Part(NyxPart('records')).CollectionView.TableWorkbenchPolicy.RangeSelectionEnabled,
      'real reusable composition preserves explicit factory defaults independently of inherited permission choices');
    FreeAndNil(LRoot);
    LOverride.SetCollectionView(BaseSpec.UseDefaultTableWorkbench);
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    Check(LOverride.HasCollectionView and LOverride.CollectionView.Defined and
      not LRoot.Children[0].Part(NyxPart('records')).CollectionView.HasTableWorkbenchPolicy and
      LRoot.Children[1].Part(NyxPart('records')).CollectionView.HasTableWorkbenchPolicy,
      'policy-only reset retains a local binding and cannot reset its reusable peer');
    FreeAndNil(LRoot);
    LOverride.SetCollectionView(Default(TNyxCollectionViewSpec));
    LSource := TNyxCodegen.Generate(LDocument);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(LCandidate.Find('local-override').HasCollectionView and
      not LCandidate.Find('local-override').CollectionView.Defined and
      (Pos('.ClearCollection', LSource) > 0) and (ImportCount(LSource) = 1),
      'generated reusable mask stays explicit while the inherited definition owns its typed dependency');
    FreeAndNil(LCandidate);
    LOverride.RemoveCollectionView;
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    Check(not LOverride.HasCollectionView and
      LRoot.Children[0].Part(NyxPart('records')).CollectionView.TableWorkbenchPolicy.RangeSelectionEnabled,
      'whole binding inheritance restores the definition rather than manufacturing a local default');
  finally
    LRoot.Free;
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

{$ifdef NYX_COMPILED_WORKBENCH}
{ This consumer calls real compiler-emitted code independently of the confined
  source reader. The expected complete design comes from the original fixture. }
procedure CompiledSourceCase;
var
  LDocument: TNyxDocument;
begin
  LDocument := nyx.generated.view.BuildNyxDocument;
  try
    LDocument.Validate;
    Check(TNyxCodec.Encode(LDocument) = GDesign,
      'independently compiled emitted Pascal builds the exact accepted Unicode/program/workbench design');
  finally
    LDocument.Free;
  end;
end;
{$endif}

{$ifndef PAS2JS}
procedure SaveText(const ADirectory, AName: String; const AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ADirectory) + AName, fmCreate);
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

begin
  try
    SourceCases;
    ReusableCases;
    {$ifdef NYX_COMPILED_WORKBENCH}
    CompiledSourceCase;
    {$endif}
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin

      if not DirectoryExists(ParamStr(1)) then
      begin
        raise Exception.Create('Source evidence needs an existing caller directory');
      end;
      SaveText(ParamStr(1), 'nyx.generated.view.pas', GSource);
      SaveText(ParamStr(1), 'expected-design.json', GDesign);
      SaveText(ParamStr(1), 'legacy-source.pas', GLegacySource);
      SaveText(ParamStr(1), 'legacy-design.json', GLegacyDesign);
    end;
    {$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-workbench-source', 'passed');
    document.body.setAttribute('data-workbench-source-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed workbench source checks; no runtime service wiring');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-workbench-source', 'failed');
      document.body.setAttribute('data-workbench-source-checks', IntToStr(GChecks));
      document.body.setAttribute('data-workbench-source-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
