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

unit nyx.test.compiler;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model;

function RunNyxCompilerDiagnosticTests: Integer;
{ Caller owns the document. The exact companion contains two pages and a helper
  rejected by real compilers, after supplementary/CJK text on its error line. }
function CreateNyxCompilerFixture(out ASource: TNyxText): TNyxDocument;

implementation

uses
  SysUtils,
  fpjson,
  nyx.json,
  nyx.data,
  nyx.types,
  nyx.controls,
  nyx.codec,
  nyx.codegen,
  nyx.composition,
  nyx.source,
  nyx.studio.compiler,
  nyx.studio.builds,
  nyx.studio.sourceprojection,
  nyx.studio.sourcebuilds,
  nyx.studio.diagnostics,
  nyx.studio.session,
  nyx.pascal.project,
  nyx.pascal.drafts,
  nyx.test.source.managed;

function CreateNyxCompilerFixture(out ASource: TNyxText): TNyxDocument;
var
  LPage: INyxPage;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxLabel('message').WithText('A compiler fixture'));
    Result.AddPage(LPage);
    LPage := NewNyxPage('other');
    LPage.Add(NewNyxButton('continue').WithText('Continue'));
    Result.AddPage(LPage);
    ASource := EditNyxManagedFixture(TNyxCodegen.Generate(Result), #10 + 'end.',
      #10 + 'procedure BrokenApplicationHelper;' + #10 + 'begin' + #10 +
      '  { 🌙漢字 } MissingApplicationFunction;' + #10 + 'end;' + #10 + 'end.');
  except
    Result.Free;
    raise;
  end;
end;

function FixtureReport(const ASource, ACompiled, AFile: TNyxText;
  AColumn: Integer = 18): INyxCompilerReport;
var
  LError: ENyxSource;
begin
  LError := ENyxSource.CreateAt('expected', ACompiled, Pos('MissingApplicationFunction;', ACompiled));
  try
    Result := ReadNyxCompilerReport(ASource, ACompiled, AFile,
      AFile + '(' + IntToStr(LError.Line) + ',' + IntToStr(AColumn) +
      TNyxText(') Error: Unknown helper / 🌙 漢字'));
  finally
    LError.Free;
  end;
end;

function AlterCompilerPacket(const APacket, AField: TNyxText;
  AValue: Integer): TNyxText;
var
  LData: TJSONData;
  LItem: TJSONObject;
begin
  { Mutate the actual wire tree, independent of native/browser JSON spacing. }
  LData := DecodeNyxJSON(APacket);
  try
    LItem := TJSONObject(TJSONObject(LData).Arrays['items'].Items[0]);
    LItem.Integers[AField] := AValue;
    Result := LData.AsJSON;
  finally
    LData.Free;
  end;
end;

function RunProjectCompilerSites: Integer;
const
  CMain: TNyxText = 'unit diagnostic.project; interface implementation end.';
  CUnit: TNyxText = 'unit helper.values;' + #10 + 'interface' + #10 +
    'implementation' + #10 + 'procedure CheckValue;' + #10 + 'begin' + #10 +
    '  { 🚀 𐐷 } MissingValue;' + #10 + 'end;' + #10 + 'end.';
  CInclude: TNyxText = '  { 🚀 𐐷 } MissingInclude;' + #10;
var
  LInputs: INyxPascalProject;
  LReport: INyxCompilerReport;
  LCopy: INyxCompilerReport;
  LBefore: INyxCompilerReport;
  LContext: INyxCompilerSourceReport;
  LReader: INyxCompilerReportReader;
  LUnit: TNyxPascalSourceRef;
  LInclude: TNyxPascalSourceRef;
  LLog: TNyxText;
  LWire: TNyxText;
  LBeforeWire: TNyxText;
  LData: TJSONData;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Project compiler sites: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure CheckPrivateReceipt(ATarget: TNyxBuildTarget);
  var
    LBuild: INyxSourceProjectionBuild;
    LDecoded: INyxSourceProjectionBuild;
    LPacket: TNyxDataValue;
    LFiles: TNyxPascalProjectFiles;
    LMissingContext: Boolean;
  begin
    LBuild := NewNyxSourceProjectionBuild(NyxSourceProjectionRef('helper-navigation'),
      NyxSourceProjectionFailure(CMain, ATarget, spsCompilationFailed,
        'Cannot compile the helper', LReport), '', '');
    LFiles := TNyxPascalProjectFiles.Capture(LInputs);

    if ATarget = btNativeLCL then
    begin
      LPacket := EncodeNyxNativeSourceBuild(LBuild);
      LDecoded := DecodeNyxNativeSourceBuild(CMain, LPacket, LFiles);
    end
    else
    begin
      LPacket := EncodeNyxBrowserSourceBuild(LBuild);
      LDecoded := DecodeNyxBrowserSourceBuild(CMain, LPacket, LFiles);
    end;
    Check((LPacket.Field('diagnostics').Kind = ndObject) and
      (LPacket.Field('diagnostics').Field('sources').Item(0).Count = 2) and
      (Pos('MissingValue;', LPacket.ToJSON) = 0) and
      (Pos(CMain, LPacket.ToJSON) = 0),
      'private compiler receipts carry helper identities without duplicating source text');
    Check((LDecoded.Projection.State = spsCompilationFailed) and
      (LDecoded.Projection.Report.Encode = LReport.Encode),
      'captured caller inputs reconstruct exact compiler report context without execution');
    LMissingContext := False;
    try

      if ATarget = btNativeLCL then
      begin
        LDecoded := DecodeNyxNativeSourceBuild(CMain, LPacket);
      end
      else
      begin
        LDecoded := DecodeNyxBrowserSourceBuild(CMain, LPacket);
      end;
    except
      on ENyxModel do
      begin
        LMissingContext := True;
      end;
    end;
    Check(LMissingContext, 'a reply cannot supply missing caller-owned helper inputs');
  end;

begin
  Result := 0;
  LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('helper.values'));
  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('body.inc'));
  LInputs := NewNyxPascalProject(NyxPascalUnit('diagnostic.project'), CMain)
    .WithUnit(NyxPascalUnit('helper.values'), CUnit)
    .WithInclude(NyxPascalInclude('body.inc'), CInclude);
  LLog := 'C:/owned/./project/helper.values.pas(6,17) Error: Unknown helper' + #10 +
    'C:/owned/project/body.inc(1,17) Error: Unknown include helper' + #10 +
    'C:/different/project/body.inc(1,17) Error: Same dependency leaf' + #10 +
    'body.inc(1,17) Error: Short path is ambiguous' + #10 +
    'C:/owned/elsewhere/../project/body.inc(1,17) Error: Parent traversal is ambiguous' + #10;
  LReport := ReadNyxProjectCompilerReport(CMain, CMain,
    'C:/owned/diagnostic.project.pas', 'C:/owned/project', LLog, LInputs);
  Check((LReport.Count = 5) and LReport.Item(0).Navigable and
    LReport.Item(0).SourceFile.SameFile(LUnit) and
    (LReport.Item(0).SourceLine = 6) and (LReport.Item(0).SourceColumn = 11),
    'exact staged unit path maps UTF-8 bytes into its own scalar coordinates');
  Check(LReport.Item(1).Navigable and LReport.Item(1).SourceFile.SameFile(LInclude) and
    (LReport.Item(1).SourceLine = 1) and (LReport.Item(1).SourceColumn = 11),
    'exact staged include has an independent typed location');
  Check(not LReport.Item(2).Navigable and not LReport.Item(3).Navigable,
    'same-named dependencies and shortened helper paths remain unmapped');
  Check(not LReport.Item(4).Navigable,
    'parent traversal never guesses which borrowed host directory a diagnostic means');
  Check(Supports(LReport, INyxCompilerSourceReport, LContext) and
    (LContext.SourceText(LUnit) = CUnit) and
    (LContext.SourceText(LInclude) = CInclude), 'copied helper contexts preserve exact Unicode text');
  LWire := LReport.Encode;
  LCopy := DecodeNyxCompilerReport(LWire);
  Check((LCopy.Encode = LWire) and LCopy.Item(0).SourceFile.SameFile(LUnit) and
    LCopy.Item(1).SourceFile.SameFile(LInclude), 'version two retains typed sites and exact context');
  CheckPrivateReceipt(btNativeLCL);
  CheckPrivateReceipt(btBrowser);
  LBefore := ReadNyxCompilerReport(CMain, CMain, 'C:/owned/main.pas',
    'C:/owned/main.pas(1,1) Error: Main error');
  LCopy := ReadNyxProjectCompilerReport(CMain, CMain, 'C:/owned/main.pas',
    'C:/owned/project', 'C:/owned/main.pas(1,1) Error: Main error', LInputs);
  Check(LCopy.Encode = LBefore.Encode, 'unused helpers preserve every legacy version-one byte');
  LCopy := ReadNyxProjectCompilerReport(CMain, CMain, 'C:/owned/main.pas',
    'C:/owned/project', 'C:/owned/project/body.inc(1,6) Error: Inside a scalar', LInputs);
  Check(not LCopy.Item(0).Navigable, 'helper byte coordinates inside a supplementary scalar refuse');
  LReader := NewNyxProjectCompilerReportReader(CMain, CMain,
    'C:/owned/diagnostic.project.pas', 'C:/owned/project', LInputs);
  LReader.Append('C:/owned/project/helper.values.pas(6,17) Error: Unknown helper' + #10);
  LBefore := LReader.Snapshot;
  LBeforeWire := LBefore.Encode;
  LReader.Append('C:/owned/project/body.inc(1,17) Error: Unknown include helper' + #10);
  LReader.Finish;
  Check((LBefore.Encode = LBeforeWire) and (LBefore.Count = 1) and
    (LReader.Snapshot.Count = 2), 'incremental snapshots detach both location and context arrays');
  LRejected := False;
  try
    LCopy := DecodeNyxCompilerReport(AlterCompilerPacket(LWire, 'sourceFile', 999));
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'out-of-report helper indices refuse before publication');
  LRejected := False;
  try
    LCopy := DecodeNyxCompilerReport(AlterCompilerPacket(LWire, 'sourceColumn', 12));
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'helper scalar coordinates must agree with raw compiler bytes');
  LData := DecodeNyxJSON(LWire);
  try
    TJSONObject(TJSONObject(LData).Arrays['sources'].Items[0]).Integers['role'] := Ord(npsMain);
    LRejected := False;
    try
      LCopy := DecodeNyxCompilerReport(LData.AsJSON);
    except
      on EArgumentException do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'helper context cannot introduce a second main buffer');
  finally
    LData.Free;
  end;
end;

function RunNyxCompilerDiagnosticTests: Integer;
const
  KnownFile: TNyxText = 'D:/compiled/🌙漢字/nyx.views.pas';
var
  LDocument: TNyxDocument;
  LView: TNyxDocument;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LCompiled: TNyxText;
  LWire: TNyxText;
  LBefore: TNyxText;
  LBeforeDesign: TNyxText;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LError: ENyxSource;
  LReport: INyxCompilerReport;
  LCopy: INyxCompilerReport;
  LOther: INyxCompilerReport;
  LItem: TNyxCompilerDiagnostic;
  LPanel: TNyxNode;
  LAction: TNyxNode;
  LDiagnostic: TNyxSourceDiagnostic;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Compiler diagnostics: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := RunProjectCompilerSites;
  LDocument := CreateNyxCompilerFixture(LSource);
  LView := nil;
  LPanel := nil;
  LError := nil;
  LSession := TNyxStudioSession.Create;
  try
    LReport := FixtureReport(LSource, LSource, KnownFile);
    LItem := LReport.Item(0);
    LError := ENyxSource.CreateAt('expected', LSource, Pos('MissingApplicationFunction;', LSource));
    Check((LReport.Count = 1) and (LReport.Source = LSource) and
      (LItem.Severity = csError) and (LItem.Message = TNyxText('Unknown helper / 🌙 漢字')),
      'owned report retains exact source, Unicode message and closed severity');
    Check(LItem.Navigable and (LItem.Column = 18) and (LItem.SourceColumn = 11) and
      (LItem.SourceLine = LError.Line), 'UTF-8 compiler byte column maps to Unicode scalar column');
    LWire := LReport.Encode;
    LCopy := DecodeNyxCompilerReport(LWire);
    Check((LCopy.Encode = LWire) and (LCopy.Item(0).SourceColumn = 11),
      'versioned report round trips exact source/message/coordinates');
    LOther := ReadNyxCompilerReport(LSource, LSource, KnownFile,
      'D:/different/違/nyx.views.pas(1,1) Error: Same leaf, different file');
    Check(not LOther.Item(0).Navigable, 'same-named dependency cannot navigate the companion');
    LOther := ReadNyxCompilerReport(LSource, LSource, KnownFile,
      'nyx.views.pas(1,1) Error: Short file is ambiguous');
    Check(not LOther.Item(0).Navigable, 'shortened paths are never inferred from compilation order');
    LOther := FixtureReport(LSource, LSource, KnownFile, 6);
    Check(not LOther.Item(0).Navigable, 'byte positions inside supplementary scalars are refused');
    LOther := ReadNyxCompilerReport(LSource, LSource, KnownFile,
      KnownFile + '(999999,1) Warning: Invalid source line' + #10 + 'Fatal: Compilation aborted');
    Check((LOther.Count = 2) and not LOther.Item(0).Navigable and
      not LOther.Item(1).Navigable and (LOther.Item(1).Severity = csFatal),
      'out-of-range and global diagnostics remain visible without invented coordinates');

    LView := CloneNyxViewDocument(LDocument, LDocument.Find('home'));
    LCompiled := PrepareNyxCompanion(LDocument, LView, LSource, True);
    LOther := FixtureReport(LSource, LCompiled, KnownFile);
    Check(LOther.Item(0).Navigable and
      (LOther.Item(0).Line <> LOther.Item(0).SourceLine) and
      (LOther.Item(0).SourceLine = LError.Line) and (LOther.Item(0).SourceColumn = 11),
      'isolated helper locations map across the changed builder to exact submitted source');
    SplitNyxSourceFrame(LCompiled, LPrefix, LBody, LSuffix);
    FreeAndNil(LError);
    LError := ENyxSource.CreateAt('builder', LCompiled, Length(LPrefix) + 2);
    LOther := ReadNyxCompilerReport(LSource, LCompiled, KnownFile,
      KnownFile + '(' + IntToStr(LError.Line) + ',1) Error: Managed view differs');
    Check(not LOther.Item(0).Navigable, 'changed managed builders cannot borrow original line numbers');
    LOther := DecodeNyxCompilerReport(LWire);
    LReport := nil;
    Check((LOther.Source = LSource) and (LCopy.Source = LSource),
      'independent report interfaces retain snapshots after another owner is released');

    LRejected := False;
    try
      LOther := DecodeNyxCompilerReport(AlterCompilerPacket(LWire, 'severity', 99));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LCopy.Encode = LWire), 'unknown severity rejects without changing existing reports');
    LRejected := False;
    try
      LOther := DecodeNyxCompilerReport(AlterCompilerPacket(LWire, 'sourceColumn', 999999));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'wire positions cannot clamp into unrelated source sites');

    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    LBefore := LSession.Source;
    LBeforeDesign := LSession.Save;
    LPanel := BuildNyxCompilerDiagnostics(LSession, LCopy);
    LAction := LPanel.Find(NyxStudioCompilerActionPrefix + '0');
    Check((LAction <> nil) and (LAction.Prop('enabled') <> 'false') and
      RouteNyxCompilerDiagnostic(LSession, LAction, ntClick, LCopy, LDiagnostic) and
      (LDiagnostic.Column = 11), 'Nyx location action routes the current accepted source');
    LSession.SetSourceDraft(LSource + #10 + '{ pending draft }');
    LRejected := False;
    try
      RouteNyxCompilerDiagnostic(LSession, LAction, ntClick, LCopy, LDiagnostic);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Source = LBefore) and (LSession.Save = LBeforeDesign) and
      (LSession.DraftSource <> LBefore), 'pending drafts refuse navigation without replacing pair/buffer');
    FreeAndNil(LPanel);
    LPanel := BuildNyxCompilerDiagnostics(LSession, LCopy);
    Check((LPanel.Find(NyxStudioCompilerActionPrefix + '0').Prop('enabled') = 'false') and
      (LPanel.Find('studio-compiler-stale') <> nil), 'stale report visibly disables its source action');
    LSession.DiscardSourceDraft;
    LSession.SetTitle('A newer accepted design');
    LRejected := False;
    try
      RouteNyxCompilerDiagnostic(LSession, LPanel.Find(NyxStudioCompilerActionPrefix + '0'),
        ntClick, LCopy, LDiagnostic);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'new accepted source refuses earlier build coordinates');
    LSession.Undo;
    Check(RouteNyxCompilerDiagnostic(LSession, LPanel.Find(NyxStudioCompilerActionPrefix + '0'),
      ntClick, LCopy, LDiagnostic) and (LSession.Source = LBefore),
      'restoring the exact accepted pair restores trustworthy navigation');
  finally
    LError.Free;
    LPanel.Free;
    LSession.Free;
    LView.Free;
    LDocument.Free;
  end;
end;

end.
