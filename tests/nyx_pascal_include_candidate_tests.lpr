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
program nyx_pascal_include_candidate_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.encoding, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.compilertrace, nyx.pascal.compilerplan;

type
  { Mutable proposal-only producer with immutable borrowed metadata. A plan must
    copy names, not retain this producer or authorize its alleged source context.
    InvalidAt exercises failure after an earlier valid alternate getter. }
  TMutablePool = class(TInterfacedObject, INyxPascalCompilerIncludeCandidates)
  public
    Base: INyxPascalCompilerIncludeCandidates;
    Rows: Integer;
    InvalidAt: Integer;
    Name: TNyxPascalDefineRef;
    function Files: INyxPascalFileCatalog;
    function Root: TNyxPascalPhysicalFile;
    function Count: Integer;
    function CandidateAt(AIndex: Integer): TNyxPascalDefineRef;
    function OriginAt(AIndex: Integer): TNyxPascalCompilerCandidateOrigin;
    function FilesVisited: Integer;
    function MissingIncludes: Integer;
    function OpaqueFiles: Integer;
    function Truncated: Boolean;
    function WorkUnits: Integer;
  end;

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

function TMutablePool.Files: INyxPascalFileCatalog;
begin
  Result := Base.Files;
end;

function TMutablePool.Root: TNyxPascalPhysicalFile;
begin
  Result := Base.Root;
end;

function TMutablePool.Count: Integer;
begin
  Result := Rows;
end;

function TMutablePool.CandidateAt(AIndex: Integer): TNyxPascalDefineRef;
begin

  if AIndex = InvalidAt then
  begin
    Exit(Default(TNyxPascalDefineRef));
  end;
  Result := Name;
end;

function TMutablePool.OriginAt(AIndex: Integer): TNyxPascalCompilerCandidateOrigin;
begin
  Result := Base.OriginAt(0);
end;

function TMutablePool.FilesVisited: Integer;
begin
  Result := Base.FilesVisited;
end;

function TMutablePool.MissingIncludes: Integer;
begin
  Result := Base.MissingIncludes;
end;

function TMutablePool.OpaqueFiles: Integer;
begin
  Result := Base.OpaqueFiles;
end;

function TMutablePool.Truncated: Boolean;
begin
  Result := Base.Truncated;
end;

function TMutablePool.WorkUnits: Integer;
begin
  Result := Base.WorkUnits;
end;

function Find(const APool: INyxPascalCompilerIncludeCandidates; const AName: TNyxText): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to APool.Count - 1 do
  begin

    if APool.CandidateAt(LAt).Name = NyxPascalASCIIKey(AName) then
    begin
      Exit(LAt);
    end;
  end;
  Result := -1;
end;

procedure CapturedGraph;
var
  LFiles: INyxPascalFileCatalog;
  LBytes: INyxPascalByteFileCatalog;
  LRaw: TNyxBytes;
  LPool: INyxPascalCompilerIncludeCandidates;
  LSmall: INyxPascalCompilerIncludeCandidates;
  LSource: INyxPascalTranslation;
  LPlan: INyxPascalCompilerPlan;
  LPlain: INyxPascalCompilerPlan;
  LContext: INyxPascalCompilerContext;
  LProbe: INyxPascalCompilerProbe;
  LBaseCount: Integer;
  LAt: Integer;
  LOrigin: TNyxPascalCompilerCandidateOrigin;
  LLexical: INyxPascalLexicalSnapshot;
  LProducer: TMutablePool;
  LMutable: INyxPascalCompilerIncludeCandidates;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
begin
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmObjFPC)
    .WithSymbol(NyxPascalDefine('KNOWN'), ppdUndefined);
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('root.pas'),
      'unit Root; {$ifdef GATE}{$i ''parts/a.inc''}{$endif}' +
      '{$if false}{$i dormant.inc}{$endif}{$I+}{$i missing.inc}{$i opaque.inc}' +
      'interface implementation end.')
    .WithFile(NyxPascalPhysicalFile('parts/a.inc'),
      '{$ifdef ALPHA}{$endif}{$i ../b.inc}{$i ../root.pas}')
    .WithFile(NyxPascalPhysicalFile('b.inc'),
      '{$if defined(BETA) and (sizeof(Char)=2)}{$endif}{$if declared(TMissing)}{$endif}' +
      '{$ifdef KNOWN}{$endif}{$define LOCAL_QUERY:=ALIAS}{$if LOCAL_QUERY>0}{$endif}')
    .WithFile(NyxPascalPhysicalFile('dormant.inc'), '{$ifdef DORMANT}{$endif}')
    .WithFile(NyxPascalPhysicalFile('unrelated.pas'), '{$ifdef NEVER_QUERY}{$endif}');
  SetLength(LRaw, 1);
  LRaw[0] := 255;
  Supports(LFiles, INyxPascalByteFileCatalog, LBytes);
  LFiles := LBytes.WithByteSource(NyxPascalPhysicalFile('opaque.inc'), NewNyxPascalSourceBytes(LRaw));
  LPool := PlanNyxPascalCompilerIncludeCandidates(LFiles, NyxPascalPhysicalFile('root.pas'),
    TNyxPascalLexicalOptions.Defaults, TNyxPascalCompilerPlanOptions.Defaults);
  WriteLn('include pool / names=', LPool.Count, ' visited=', LPool.FilesVisited,
    ' missing=', LPool.MissingIncludes, ' opaque=', LPool.OpaqueFiles, ' work=', LPool.WorkUnits);
  for LAt := 0 to LPool.Count - 1 do
  begin
    WriteLn('include candidate / ', LPool.CandidateAt(LAt).Name);
  end;
  Check((LPool.Count = 6) and (LPool.FilesVisited = 5),
    'Only the reachable captured graph contributes names; cycles/repeated physical files scan once');
  Check((LPool.MissingIncludes = 1) and (LPool.OpaqueFiles = 1),
    'Missing and undecoded includes remain explicit without host lookup or guessed decoding');
  Check((Find(LPool, 'DORMANT') >= 0) and (Find(LPool, 'NEVER_QUERY') < 0),
    'Inactive literal links may propose names, while unrelated captured units are excluded');
  Check((Find(LPool, 'CHAR') < 0) and (Find(LPool, 'TMISSING') < 0),
    'Named sizeof and declared operands never become pristine compiler symbol queries');
  Check((Find(LPool, 'LOCAL_QUERY') >= 0) and (Find(LPool, 'ALIAS') < 0),
    'Raw proposals perform no macro expansion or source-local evaluation');
  LOrigin := LPool.OriginAt(Find(LPool, 'BETA'));
  LLexical := NewNyxPascalLexicalSnapshot(LPool.Files.TextAt(LPool.Files.Find(LOrigin.FileReference)),
    TNyxPascalLexicalOptions.Defaults);
  Check((LOrigin.FileReference.Name = TNyxText('b.inc')) and
    (LLexical.Slice(LOrigin.Span) = TNyxText('{$if defined(BETA) and (sizeof(Char)=2)}')),
    'A speculative candidate retains exact first physical file/span without inventing an occurrence');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('b.inc'), 'changed');
  Check(LPool.Files.TextAt(LPool.Files.Find(NyxPascalPhysicalFile('b.inc'))) <> TNyxText('changed'),
    'Published pools retain independent exact files after caller changes');
  LSource := NewNyxPascalTranslation(LPool.Files, LPool.Root, LContext,
    TNyxPascalTranslationOptions.Defaults);
  LPlain := PlanNyxPascalCompilerRequirements(LSource, TNyxPascalCompilerPlanOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirementsWithIncludes(LSource,
    TNyxPascalCompilerPlanOptions.Defaults, LPool);
  Check(LPlan.Count = LPlain.Count, 'Explicit proposals do not create active source requirements');
  LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('POOL'), ppmObjFPC);
  LBaseCount := LProbe.Count;
  LProbe := LPlan.CompilerProbe(LProbe);
  Check((LProbe.Count = LBaseCount + 5) and (LProbe.Symbol(LBaseCount).Name = TNyxText('gate')),
    'Actual requirements keep priority; already known pristine names are not queried again');
  LSmall := PlanNyxPascalCompilerIncludeCandidates(LPool.Files, LPool.Root,
    TNyxPascalLexicalOptions.Defaults, TNyxPascalCompilerPlanOptions.Defaults.UpToConditions(2));
  Check((LSmall.Count = 2) and LSmall.Truncated, 'Output truncation is explicit under the existing name quota');
  LProducer := TMutablePool.Create;
  LMutable := LProducer;
  LProducer.Base := LPool;
  LProducer.Rows := 2;
  LProducer.InvalidAt := -1;
  LProducer.Name := NyxPascalDefine('FROZEN');
  LPlan := PlanNyxPascalCompilerRequirementsWithIncludes(LSource,
    TNyxPascalCompilerPlanOptions.Defaults, LMutable);
  LProducer.Name := NyxPascalDefine('CHANGED');
  LProbe := LPlan.CompilerProbe(NewNyxFPCCompilerProbe(NyxPascalDefine('COPY'), ppmObjFPC));
  Check(LProbe.Symbol(LProbe.Count - 1).Name = TNyxText('frozen'),
    'Explicit alternate pool names are frozen without retaining a mutable producer');
  LProducer.InvalidAt := 1;
  LRejected := False;
  try
    LPlan := PlanNyxPascalCompilerRequirementsWithIncludes(LSource,
      TNyxPascalCompilerPlanOptions.Defaults, LMutable);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPlan <> nil), 'A late alternate getter failure preserves the prior complete plan');
  LMutable := nil;
  LSmall := nil;
  LRejected := False;
  try
    LSmall := PlanNyxPascalCompilerIncludeCandidates(LPool.Files, LPool.Root,
      TNyxPascalLexicalOptions.Defaults, TNyxPascalCompilerPlanOptions.Defaults.UpToTokens(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSmall = nil), 'Work exhaustion publishes no partial proposal pool');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LSmall := PlanNyxPascalCompilerIncludeCandidates(LPool.Files, LPool.Root,
      TNyxPascalLexicalOptions.Defaults, TNyxPascalCompilerPlanOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSmall = nil), 'Cancellation publishes no partial captured graph');
  Check(LPool.WorkUnits > LPool.Count, 'Speculative lexical work is explicit and separate from output count');
end;

procedure QueryCohorts;
var
  LText: TNyxText;
  LFiles: INyxPascalFileCatalog;
  LPool: INyxPascalCompilerIncludeCandidates;
  LSource: INyxPascalTranslation;
  LPlan: INyxPascalCompilerPlan;
  LContext: INyxPascalCompilerContext;
  LBase: INyxPascalCompilerProbe;
  LFirst: INyxPascalCompilerProbe;
  LSecond: INyxPascalCompilerProbe;
  LAt: Integer;
  LNoRepeat: Boolean;
begin
  LText := 'unit Bulk; interface ';
  for LAt := 0 to 299 do
  begin
    LText := LText + '{$ifdef FLAG_' + TNyxText(IntToStr(LAt)) + '}{$endif}';
  end;
  LText := LText + ' implementation end.';
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('bulk.pas'), LText);
  LPool := PlanNyxPascalCompilerIncludeCandidates(LFiles, NyxPascalPhysicalFile('bulk.pas'),
    TNyxPascalLexicalOptions.Defaults, TNyxPascalCompilerPlanOptions.Defaults);
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmObjFPC);
  LSource := NewNyxPascalTranslation(LFiles, LPool.Root, LContext,
    TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirementsWithIncludes(LSource,
    TNyxPascalCompilerPlanOptions.Defaults, LPool);
  LBase := NewNyxFPCCompilerProbe(NyxPascalDefine('COHORT'), ppmObjFPC);
  LFirst := LPlan.CompilerProbe(LBase);
  Check((LPool.Count = 300) and not LPool.Truncated and (LPlan.Count = 300),
    'An explicit pool and full requirements retain more names than one compiler job can query');
  Check(LFirst.Count = 256, 'Required-first selection obeys the unchanged 256-name job ceiling');
  for LAt := 0 to LFirst.Count - 1 do
  begin
    LContext := LContext.WithSymbol(LFirst.Symbol(LAt), ppdUndefined);
  end;
  LSource := NewNyxPascalTranslation(LFiles, LPool.Root, LContext,
    TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirementsWithIncludes(LSource,
    TNyxPascalCompilerPlanOptions.Defaults, LPool);
  LSecond := LPlan.CompilerProbe(LBase);
  LNoRepeat := True;
  for LAt := LBase.Count to LSecond.Count - 1 do
  begin
    LNoRepeat := LNoRepeat and (LContext.Symbol(LSecond.Symbol(LAt)).Kind = ppdUnknown);
  end;
  Check((LSecond.Count <= 256) and (LSecond.Count > LBase.Count) and LNoRepeat,
    'A later explicit context queries the remaining cohort without repeating captured names');
  Check(LPlan.Count < 300, 'Synthetic measured absences refine source requirements without editing source');
end;

begin
  try
    CapturedGraph;
    QueryCohorts;
    WriteLn('PASS ', GChecks, ' captured include-candidate planning checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
