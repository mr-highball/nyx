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
program nyx_sdk_capture_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses Classes, SysUtils, LazFileUtils, nyx.text, nyx.bytes, nyx.editing,
  nyx.scheduler, nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.binding,
  nyx.pascal.files, nyx.pascal.modules, nyx.pascal.translation,
  nyx.pascal.files.native, nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts,
  nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifdef windows}, Windows{$endif};
var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure WriteBytes(const APath: TNyxText; const ABytes: TNyxBytes);
var
  LHandle: THandle;
  LStream: THandleStream;
begin
  {$ifdef windows}
  LHandle := CreateFileW(PWideChar(UTF8Decode(APath)), GENERIC_WRITE, 0,
    nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  {$else}
  LHandle := FileCreate(String(RawByteString(APath)));
  {$endif}

  if LHandle = THandle(-1) then
  begin
    raise Exception.Create('Could not create a fresh owned fixture file');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);

    if Length(ABytes) > 0 then
    begin
      LStream.WriteBuffer(ABytes[0], Length(ABytes));
    end;
  finally
    LStream.Free;
    FileClose(LHandle);
  end;
end;

procedure WriteText(const APath, AText: TNyxText);
begin
  WriteBytes(APath, NyxEncodeUTF8(AText));
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301)
    .WithSymbol(NyxPascalDefine('PAS2JS'), ppdUndefined)
    .CompleteDefines(False);
end;

procedure OwnedFiles(const ADirectory: TNyxText);
const
  CUnicodeLeaf: TNyxText = 'control-🚀.pas';
  CUnit: TNyxText = 'unit Captured; {$mode delphi} interface type ' +
    'TData=record Field:Integer; end; implementation end. // 𐐷 🚀';
var
  LCapture: INyxPascalDirectoryCapture;
  LSeed: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
  LIndex: Integer;
  LBytes: TNyxBytes;
  LEmpty: INyxPascalDirectoryCapture;
begin
  Check(not DirectoryExistsUTF8(ADirectory) and CreateDirUTF8(ADirectory),
    'The capture consumer creates only a fresh explicitly owned fixture');
  Check(CreateDirUTF8(IncludeTrailingPathDelimiter(ADirectory) + 'child'),
    'A nested directory is independently owned');
  WriteText(IncludeTrailingPathDelimiter(ADirectory) + CUnicodeLeaf, CUnit);
  WriteText(IncludeTrailingPathDelimiter(ADirectory) + 'parts.inc', 'const TextValue=''𐐷'';');
  WriteText(IncludeTrailingPathDelimiter(ADirectory) + 'demo.pp', 'program Demo; begin end.');
  WriteText(IncludeTrailingPathDelimiter(ADirectory) + 'child/extra.pas',
    'unit Extra; interface implementation end.');
  LSeed := NewNyxPascalFileCatalog(pfcExact);
  LModules := NewNyxPascalModuleCatalog;
  LCapture := CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
    TNyxPascalDirectoryOptions.Defaults.Under('sdk'));
  Check((LCapture.Files.Count = 3) and (LCapture.Modules.Count = 1) and
    (LCapture.UnindexedCount = 1) and (LSeed.Count = 0) and (LModules.Count = 0),
    'Only selected source files enter the detached nonrecursive capture; program roots are explicit unindexed data');
  LIndex := LCapture.Files.Find(NyxPascalPhysicalFile(TNyxText('sdk/') + CUnicodeLeaf));
  Check((LIndex >= 0) and (LCapture.Files.TextAt(LIndex) = CUnit) and
    (LCapture.Modules.Entry(0).Name.Name = TNyxText('Captured')),
    'Unicode native paths/text and exact original bytes survive into a declared namespace mapping');
  LCapture := CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
    TNyxPascalDirectoryOptions.Defaults.Under('sdk').Recursively);
  Check((LCapture.Files.Count = 4) and (LCapture.Modules.Count = 2),
    'Explicit recursion captures the nested unit under its actual virtual path');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
      TNyxPascalDirectoryOptions.Defaults, nil, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSeed.Count = 0), 'Cancelled capture leaves seed files and indexes untouched');
  LRejected := False;
  try
    CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
      TNyxPascalDirectoryOptions.Defaults.UpToFiles(1));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSeed.Count = 0), 'File-count budget failure cannot publish a partial directory');
  LRejected := False;
  try
    CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
      TNyxPascalDirectoryOptions.Defaults.UpToDirectoryEntries(1));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Enumeration work is bounded independently of captured payloads');
  Check(CreateDirUTF8(IncludeTrailingPathDelimiter(ADirectory) + 'empty'),
    'An empty directory is separately owned');
  LEmpty := CaptureNyxPascalSourceDirectory(IncludeTrailingPathDelimiter(ADirectory) + 'empty',
    LSeed, LModules, TNyxPascalDirectoryOptions.Defaults);
  Check((LEmpty.Files.Count = 0) and (LEmpty.Modules.Count = 0),
    'An explicitly selected empty source directory remains a useful empty capture');
  LRejected := False;
  try
    CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
      TNyxPascalDirectoryOptions.Defaults.UpToBytes(1));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A smaller byte budget refuses without modifying seed data');
  LRejected := False;
  try
    CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
      TNyxPascalDirectoryOptions.Defaults.Recursively.UpToDepth(1));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Recursive traversal respects its explicit depth budget');
  SetLength(LBytes, 2);
  LBytes[0] := $C3;
  LBytes[1] := $28;
  WriteBytes(IncludeTrailingPathDelimiter(ADirectory) + 'invalid.inc', LBytes);
  LRejected := False;
  try
    CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
      TNyxPascalDirectoryOptions.Defaults);
  except
    on ENyxBytes do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Unsupported source encoding refuses without silently rewriting captured bytes');
  LCapture := CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
    TNyxPascalDirectoryOptions.Defaults.Under('selected').OnlyFile(CUnicodeLeaf));
  Check((LCapture.Files.Count = 1) and (LCapture.Modules.Count = 1) and
    (LCapture.Files.TextAt(0) = CUnit) and (LSeed.Count = 0),
    'One Unicode source leaf is captured exactly without reading an unrelated invalid byte file');
  LRejected := False;
  LCapture := nil;
  try
    LCapture := CaptureNyxPascalSourceDirectory(ADirectory, LSeed, LModules,
      TNyxPascalDirectoryOptions.Defaults.OnlyFile('absent.inc'));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCapture = nil) and (LSeed.Count = 0) and (LModules.Count = 0),
    'An absent selected leaf refuses without publishing a misleading empty capture or changing seeds');
  LRejected := False;
  try
    TNyxPascalDirectoryOptions.Defaults.OnlyFile('child/extra.pas');
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A selected source is a leaf rather than a hidden traversal path');
  LRejected := False;
  try
    TNyxPascalDirectoryOptions.Defaults.OnlyFile(CUnicodeLeaf).Recursively;
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Single-leaf selection and recursive directory capture refuse contradictory intent');
end;

procedure ActualSDK(const ARepository: TNyxText);
var
  LCapture: INyxPascalDirectoryCapture;
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LAnalysis: INyxPascalCompletionAnalysis;
  LPrefix: TNyxText;
  LAt: Integer;
  LModel: Boolean;
  LRTL: INyxPascalDirectoryCapture;
  LQuery: INyxPascalCompletionRequest;
  LTranslation: INyxPascalTranslation;
  LMissingInclude: Boolean;
  LPolicyIssue: Boolean;
begin
  LCapture := CaptureNyxPascalSourceDirectory(IncludeTrailingPathDelimiter(ARepository) + 'src',
    NewNyxPascalFileCatalog(pfcExact), NewNyxPascalModuleCatalog,
    TNyxPascalDirectoryOptions.Defaults.Under('sdk'));
  Check((LCapture.Files.Count > 400) and (LCapture.Modules.Count > 250),
    'The actual repository source tree is captured instead of a selected export stub');
  Check(LCapture.Modules.Find(NyxPascalModule('nyx.model')) >= 0,
    'The retained missing Nyx dependency has a real captured module root');
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LCapture.Files, LCapture.Modules,
    TNyxOwnedPascalCompletionOptions.Defaults.UpToTokens(4000000));
  LPrefix := 'unit Consumer; {$mode delphi} interface uses nyx.controls; implementation ' +
    'procedure Run; var Badge:INyxBadge; begin Badge.';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.'))
    .WithBuffer(TNyxPascalSourceRef.Main, LPrefix + 'WithTexx; end; end.');
  LQuery := NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix) + 6);
  LReply := LProvider.Complete(LQuery);
  Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
  Supports(LReply, INyxPascalCompletionAnalysis, LAnalysis);
  LModel := False;
  for LAt := 0 to LAnalysis.ModuleCount - 1 do
  begin
    WriteLn('loaded / ', LAnalysis.ModuleName(LAt).Name);
    LModel := LModel or (LAnalysis.ModuleName(LAt).Name = TNyxText('nyx.model'));
  end;
  WriteLn('actual SDK / files=', LCapture.Files.Count, ' modules=', LCapture.Modules.Count,
    ' unindexed=', LCapture.UnindexedCount, ' tokens=', LAnalysis.TranslatedTokens);
  WriteLn('badge / status=', Ord(LReply.Status), ' issue=', Ord(LMetadata.BindingIssue),
    ' related=', LMetadata.RelatedName, ' message=', LReply.MessageText);
  Check(LModel and (LAnalysis.DependencyLoads > 1),
    'Actual completion follows its Nyx dependency through fresh semantic worlds');
  Check((LReply.Status = pcsReady) or ((LReply.Status = pcsUnavailable) and
    (LMetadata.BindingIssue <> pbiNone)),
    'The actual SDK journey either binds or retains a typed prerequisite instead of an internal failure');

  if ParamCount = 3 then
  begin
    LRTL := CaptureNyxPascalSourceDirectory(TNyxText(ParamStr(3)), LCapture.Files, LCapture.Modules,
      TNyxPascalDirectoryOptions.Defaults.Under('rtl/win'));
    Check(LRTL.Modules.Find(NyxPascalModule('SysUtils')) >= 0,
      'The explicit real RTL directory supplies the exact SysUtils source root');
    LTranslation := NewNyxPascalTranslation(LRTL.Files,
      LRTL.Modules.Entry(LRTL.Modules.Find(NyxPascalModule('SysUtils'))).Root,
      Facts.WithSymbol(NyxPascalDefine('FPC_DOTTEDUNITS'), ppdUndefined)
        .WithSymbol(NyxPascalDefine('UNICODERTL'), ppdUndefined),
      TNyxPascalTranslationOptions.Defaults);
    LMissingInclude := False;
    LPolicyIssue := False;
    for LAt := 0 to LTranslation.DiagnosticCount - 1 do
    begin
      LMissingInclude := LMissingInclude or
        (LTranslation.Diagnostic(LAt).Issue = ptiMissingInclude);
      LPolicyIssue := LPolicyIssue or
        (LTranslation.Diagnostic(LAt).Issue = ptiLexicalPolicyChange);
      WriteLn('actual RTL translation / issue=', Ord(LTranslation.Diagnostic(LAt).Issue),
        ' at=', LTranslation.Diagnostic(LAt).Span.Start,
        ' message=', LTranslation.Diagnostic(LAt).MessageText);
    end;
    Check(LMissingInclude and not LPolicyIssue,
      'Real SysUtils passes ObjFPC/OUT lexical configuration and reaches its uncaptured include');
    Check((LTranslation.State(LTranslation.StateCount - 2).Mode = ppmObjFPC) and
      (LTranslation.State(LTranslation.StateCount - 2).Feature(ppfOutParameters) = ppkTrue),
      'The real RTL retains typed mode/OUT facts before its exact missing-include boundary');
    { The raw selected SysUtils namespace requires the ordinary nondotted RTL
      build fact. Other unprovided facts remain open, never sealed as absent. }
    LProvider := NewNyxIndexedPascalCompletionProvider(
      Facts.WithSymbol(NyxPascalDefine('FPC_DOTTEDUNITS'), ppdUndefined),
      LRTL.Files, LRTL.Modules, TNyxOwnedPascalCompletionOptions.Defaults.UpToTokens(4000000));
    LReply := LProvider.Complete(LQuery);
    Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
    Supports(LReply, INyxPascalCompletionAnalysis, LAnalysis);
    WriteLn('actual RTL / files=', LRTL.Files.Count, ' modules=', LRTL.Modules.Count,
      ' tokens=', LAnalysis.TranslatedTokens, ' related=', LMetadata.RelatedName,
      ' issue=', Ord(LMetadata.BindingIssue), ' message=', LReply.MessageText);
    Check((LReply.Status = pcsUnavailable) and
      (LMetadata.BindingIssue = pbiIncompleteSource) and
      (LMetadata.RelatedName = TNyxText('SysUtils')),
      'The incomplete real RTL retains its exact prerequisite instead of false export absence');
  end;
end;

begin
  try
    Check((ParamCount = 2) or (ParamCount = 3),
      'Supply repository root, a fresh owned E: fixture directory and optionally the actual RTL win directory');
    OwnedFiles(TNyxText(ParamStr(2)));
    ActualSDK(TNyxText(ParamStr(1)));
    WriteLn('PASS ', GChecks, ' native SDK capture checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
