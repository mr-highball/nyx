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
  nyx.types,
  nyx.controls,
  nyx.codec,
  nyx.codegen,
  nyx.composition,
  nyx.source,
  nyx.studio.compiler,
  nyx.studio.diagnostics,
  nyx.studio.session,
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
      ') Error: Unknown helper / 🌙 漢字');
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

function RunNyxCompilerDiagnosticTests: Integer;
const
  KnownFile = 'D:/compiled/🌙漢字/nyx.views.pas';
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
  Result := 0;
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
