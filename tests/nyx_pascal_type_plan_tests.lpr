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
program nyx_pascal_type_plan_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  {$if defined(UNIX) and not defined(PAS2JS)}cthreads,{$endif}
  SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.lexing.append, nyx.pascal.syntax, nyx.pascal.types,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax.translated, nyx.pascal.binding;

type
  { Public shape alone has no sealed parser provenance. This alternate producer
    owns no source and must never turn its counters into a replay capability. }
  TAlternatePlan = class(TInterfacedObject, INyxPascalTypePlan)
    function Window: TNyxPascalSpan;
    function Count: Integer;
  end;
  {$ifndef PAS2JS}
  { Native concurrency consumer. The worker owns its source lease and result
    views, touches no controls and uses RTL atomics only for test coordination.
    Main joins before reading final counters or freeing the worker. }
  TPlanReader = class(TThread)
  private
    FStop: LongInt;
    FStarted: LongInt;
  protected
    procedure Execute; override;
  public
    Source: INyxPascalTranslatedSyntax;
    DeclarationIndex: Integer;
    QueryIndices: array of Integer;
    MaximumIterations: Integer;
    ErrorText: String;
    Iterations: Integer;
    procedure StopReading;
    function StartedReading: Boolean;
  end;
  {$endif}
  { Controlled source callbacks exercise actual chronological prefix inheritance.
    Every queried type compares with an independent current-source parser, and
    detached worlds must own their own results. No source retains this callback. }
  TPlanCollector = class(TInterfacedObject, INyxPascalConditionalFacts,
    INyxPascalPrefixConditionalFacts)
  public
    Previous: INyxPascalTranslatedSyntax;
    First: INyxPascalTranslatedSyntax;
    Calls: Integer;
    function PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
    function EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
      const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
    function Evaluate(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
  end;

var
  GChecks: Integer;
  GThreadReaders: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).WithOption('H', ppkTrue);
end;

function TAlternatePlan.Window: TNyxPascalSpan;
begin
  Result := NyxPascalSpan(0, 0);
end;

function TAlternatePlan.Count: Integer;
begin
  Result := 1;
end;

function View(const AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('case.pas'), AText),
    NyxPascalPhysicalFile('case.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function Memo(const AView: INyxPascalTranslatedSyntax): TNyxPascalTypeMemoReceipt;
var
  LWork: INyxPascalTypeMemoWork;
begin
  Check(Supports(AView, INyxPascalTypeMemoWork, LWork), 'Owned translated views expose bounded memo work');
  Result := LWork.TypeMemoReceipt;
end;

procedure EqualTypes(const ALeft, ARight: INyxPascalTypeSyntax);
var
  LAt: Integer;
  LRole: TNyxPascalTypeRole;
  LFlag: TNyxPascalTypeFlag;
  LLeft: TNyxPascalTypeNode;
  LRight: TNyxPascalTypeNode;
  LLeftSource: INyxPascalDeclaredTypeSyntax;
  LRightSource: INyxPascalDeclaredTypeSyntax;
  LSame: Boolean;
begin
  LSame := (ALeft.Window.Start = ARight.Window.Start) and
    (ALeft.Window.Length = ARight.Window.Length) and (ALeft.Root = ARight.Root) and
    (ALeft.Complete = ARight.Complete) and (ALeft.Count = ARight.Count) and
    (ALeft.DiagnosticCount = ARight.DiagnosticCount) and
    Supports(ALeft, INyxPascalDeclaredTypeSyntax, LLeftSource) and
    Supports(ARight, INyxPascalDeclaredTypeSyntax, LRightSource);

  if LSame then
  begin
    for LAt := 0 to ALeft.Count - 1 do
    begin
      LLeft := ALeft.Node(LAt);
      LRight := ARight.Node(LAt);
      LSame := LSame and (LLeft.Kind = LRight.Kind) and (LLeft.Role = LRight.Role) and
        (LLeft.Name = LRight.Name) and (LLeft.Span.Start = LRight.Span.Start) and
        (LLeft.Span.Length = LRight.Span.Length) and (LLeft.Parent = LRight.Parent) and
        (LLeft.FirstChild = LRight.FirstChild) and (LLeft.NextSibling = LRight.NextSibling) and
        (LLeft.ParameterMode = LRight.ParameterMode) and
        (LLeft.CallingConvention = LRight.CallingConvention) and
        (LLeftSource.DeclarationOf(LAt) = LRightSource.DeclarationOf(LAt));
      for LFlag := Low(TNyxPascalTypeFlag) to High(TNyxPascalTypeFlag) do
      begin
        LSame := LSame and ((LFlag in LLeft.Flags) = (LFlag in LRight.Flags));
      end;
      for LRole := Low(TNyxPascalTypeRole) to High(TNyxPascalTypeRole) do
      begin
        LSame := LSame and (ALeft.Child(LAt, LRole) = ARight.Child(LAt, LRole));
      end;
    end;
    for LAt := 0 to ALeft.DiagnosticCount - 1 do
    begin
      LSame := LSame and (ALeft.Diagnostic(LAt).Issue = ARight.Diagnostic(LAt).Issue) and
        (ALeft.Diagnostic(LAt).Span.Start = ARight.Diagnostic(LAt).Span.Start) and
        (ALeft.Diagnostic(LAt).Span.Length = ARight.Diagnostic(LAt).Span.Length) and
        (ALeft.Diagnostic(LAt).MessageText = ARight.Diagnostic(LAt).MessageText);
    end;
  end;
  Check(LSame, 'Reused complete grammar, roles, bounds, ABI flags and structured identities equal fresh parsing');
end;

procedure Plans;
const
  CText: TNyxText = 'unit Plans; interface type TRecord=record Value:Byte; end; ' +
    'TItems=array[0..3] of ^TRecord; ' +
    'TInline=array of record Child:record Code:Byte; end; end; ' +
    'TCallback=reference to function(const Items:TItems; var Amount:Integer):Boolean; cdecl; ' +
    'TUnicode=T𝒜; const Guard=1; Boundary=2; ';
var
  LLexical: INyxPascalLexicalSnapshot;
  LNextLexical: INyxPascalLexicalSnapshot;
  LBase: INyxPascalSyntaxSnapshot;
  LNext: INyxPascalSyntaxSnapshot;
  LForeign: INyxPascalSyntaxSnapshot;
  LType: INyxPascalTypeSyntax;
  LApplied: INyxPascalTypeSyntax;
  LPlan: INyxPascalTypePlan;
  LAlternate: INyxPascalTypePlan;
  LDeclared: INyxPascalDeclaredTypeSyntax;
  LCancellation: INyxCancellationScope;
  LAt: Integer;
  LIndex: Integer;
  LRejected: Boolean;
  LNames: array[0..4] of TNyxText;
  LSyntaxOptions: TNyxPascalSyntaxOptions;
begin
  LNames[0] := 'TRecord';
  LNames[1] := 'TItems';
  LNames[2] := 'TInline';
  LNames[3] := 'TCallback';
  LNames[4] := 'TUnicode';
  LSyntaxOptions := TNyxPascalSyntaxOptions.Defaults.Lexical(
    TNyxPascalLexicalOptions.Defaults.UnicodeIdentifiers(True));
  LLexical := NewNyxPascalAppendLexical(CText, LSyntaxOptions.LexicalOptions);
  LBase := NewNyxPascalSyntaxFromLexical(LLexical, LSyntaxOptions);
  LNextLexical := AppendNyxPascalLexical(LLexical, 'var Tail:TItems; implementation end.');
  LNext := AppendNyxPascalSyntax(LBase, LNextLexical, LSyntaxOptions);
  LForeign := NewNyxPascalSyntaxSnapshot(CText + 'var Tail:TItems; implementation end.', LSyntaxOptions);
  for LAt := 0 to High(LNames) do
  begin
    LIndex := LBase.Named(0, LNames[LAt]);
    LType := NewNyxPascalDeclaredTypeSyntax(LBase, LBase.Declaration(LIndex).TypeSpan,
      TNyxPascalTypeOptions.Defaults);
    LPlan := CaptureNyxPascalTypePlan(LType, LIndex);
    Check((LPlan <> nil) and (LPlan.Count = LType.Count), 'Complete owned type grammar captures a source-free plan');
    LApplied := ApplyNyxPascalTypePlan(LPlan, LNext, LIndex, TNyxPascalTypeOptions.Defaults);
    Check((LApplied <> nil) and (LApplied.Lexical = LNext.Lexical) and
      Supports(LApplied, INyxPascalDeclaredTypeSyntax, LDeclared) and
      (LDeclared.Declarations = LNext), 'A plan rehosts in the exact current declaration snapshot');
    EqualTypes(LApplied, NewNyxPascalDeclaredTypeSyntax(LNext,
      LNext.Declaration(LIndex).TypeSpan, TNyxPascalTypeOptions.Defaults));
    Check(ApplyNyxPascalTypePlan(LPlan, LForeign, LIndex, TNyxPascalTypeOptions.Defaults) = nil,
      'Equal foreign source cannot fabricate metadata identity');
    Check(ApplyNyxPascalTypePlan(LPlan, LNext, LIndex,
      TNyxPascalTypeOptions.Defaults.UpToDepth(129)) = nil,
      'Changed type policies receive a fresh parser rather than bypassing their limits');
  end;
  LType := nil;
  LBase := nil;
  LLexical := nil;
  LApplied := ApplyNyxPascalTypePlan(LPlan, LNext, LIndex, TNyxPascalTypeOptions.Defaults);
  Check(LApplied <> nil, 'The pure plan survives releasing the original parser/source leases');
  LCancellation := NewNyxCancellationScope;
  LCancellation.Cancel;
  LRejected := False;
  LApplied := nil;
  try
    LApplied := ApplyNyxPascalTypePlan(LPlan, LNext, LIndex,
      TNyxPascalTypeOptions.Defaults, LCancellation);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LApplied = nil), 'A reusable type plan still refuses prior cancellation atomically');
  LAlternate := TAlternatePlan.Create;
  Check(ApplyNyxPascalTypePlan(LAlternate, LNext, LIndex, TNyxPascalTypeOptions.Defaults) = nil,
    'Public plan counters/window from an alternate producer grant no replay authority');
  LType := NewNyxPascalTypeSyntax(NewNyxPascalLexicalSnapshot('Byte'),
    NyxPascalSpan(0, 4), TNyxPascalTypeOptions.Defaults);
  Check(CaptureNyxPascalTypePlan(LType, 0) = nil,
    'Standalone type grammar cannot invent an owned declaration proof');
end;

{$ifndef PAS2JS}
procedure TPlanReader.StopReading;
begin
  InterlockedExchange(FStop, 1);
end;

function TPlanReader.StartedReading: Boolean;
begin
  Result := InterlockedCompareExchange(FStarted, 0, 0) <> 0;
end;

procedure TPlanReader.Execute;
var
  LType: INyxPascalTypeSyntax;
  LDeclared: INyxPascalDeclaredTypeSyntax;
  LIndex: Integer;
  LLimit: Integer;
begin
  try
    LLimit := MaximumIterations;

    if LLimit <= 0 then
    begin
      LLimit := 20000;
    end;
    repeat
      LIndex := DeclarationIndex;

      if Length(QueryIndices) > 0 then
      begin
        LIndex := QueryIndices[Iterations mod Length(QueryIndices)];
      end;
      LType := Source.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults);

      if not LType.Complete or not Supports(LType, INyxPascalDeclaredTypeSyntax, LDeclared) or
        (LDeclared.Declarations <> Source.Syntax) or (LType.Lexical <> Source.Syntax.Lexical) then
      begin
        raise Exception.Create('Concurrent type query returned foreign/incomplete source');
      end;
      Inc(Iterations);
      InterlockedExchange(FStarted, 1);
    until (InterlockedCompareExchange(FStop, 0, 0) <> 0) or (Iterations >= LLimit);
  except
    on LError: Exception do
    begin
      ErrorText := LError.Message;
    end;
  end;
  InterlockedExchange(FStarted, 1);
end;

procedure ConcurrentEviction;
var
  LText: TNyxText;
  LView: INyxPascalTranslatedSyntax;
  LReaders: array[0..3] of TPlanReader;
  LIndices: array of Integer;
  LReader: Integer;
  LAt: Integer;
  LTotal: Integer;
  LOkay: Boolean;
  LError: String;
  LReceipt: TNyxPascalTypeMemoReceipt;
begin
  LText := 'unit ConcurrentMemo; interface type ';
  for LAt := 1 to 80 do
  begin
    LText := LText + 'T' + TNyxText(IntToStr(LAt)) + '=array of Byte; ';
  end;
  LView := View(LText + 'implementation end.');
  SetLength(LIndices, 80);
  for LAt := 0 to High(LIndices) do
  begin
    LIndices[LAt] := LView.Syntax.Named(0, 'T' + TNyxText(IntToStr(LAt + 1)));
  end;
  for LReader := 0 to High(LReaders) do
  begin
    LReaders[LReader] := nil;
  end;
  LTotal := 0;
  LOkay := True;
  LError := '';
  try
    for LReader := 0 to High(LReaders) do
    begin
      LReaders[LReader] := TPlanReader.Create(True);
      LReaders[LReader].FreeOnTerminate := False;
      LReaders[LReader].Source := LView;
      LReaders[LReader].QueryIndices := LIndices;
      LReaders[LReader].MaximumIterations := 256;
      LReaders[LReader].Start;
    end;
    for LAt := 1 to 16 do
    begin
      LReceipt := Memo(LView);
      Check((LReceipt.Entries <= 64) and (LReceipt.RetainedNodes <= 8192),
        'Atomic receipts remain bounded while native misses/admission/eviction contend');
    end;
    for LReader := 0 to High(LReaders) do
    begin
      LReaders[LReader].WaitFor;
    end;
  finally
    for LReader := 0 to High(LReaders) do
    begin

      if LReaders[LReader] <> nil then
      begin
        LReaders[LReader].StopReading;
        LReaders[LReader].WaitFor;
        LOkay := LOkay and (LReaders[LReader].ErrorText = '') and
          (LReaders[LReader].Iterations = 256);
        LError := LError + LReaders[LReader].ErrorText;
        Inc(LTotal, LReaders[LReader].Iterations);
        Inc(GThreadReaders);
        LReaders[LReader].Free;
      end;
    end;
  end;
  Check(LOkay, 'Every concurrent mixed-declaration result keeps its source and grammar: ' + LError);
  LReceipt := Memo(LView);
  Check((LReceipt.Hits + LReceipt.Misses = LTotal) and (LTotal = 1024) and
    (LReceipt.Entries <= 64) and (LReceipt.RetainedNodes <= 8192),
    'Concurrent hit/miss accounting and bounded retention admit all 1024 completed queries');
end;
{$endif}

function TPlanCollector.PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
begin
  Result := True;
end;

function TPlanCollector.EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
  const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
var
  LNext: INyxPascalTranslatedSyntax;
  LIndex: Integer;
  LType: INyxPascalTypeSyntax;
  LFirstType: INyxPascalTypeSyntax;
  LBefore: TNyxPascalTypeMemoReceipt;
  LAfter: TNyxPascalTypeMemoReceipt;
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  {$ifndef PAS2JS}
  LReaders: array[0..3] of TPlanReader;
  LReader: Integer;
  LWait: Integer;
  LReadersOkay: Boolean;
  LReaderError: String;
  {$endif}
begin
  Inc(Calls);

  if Previous = nil then
  begin
    LNext := NewNyxPascalTranslatedSyntax(APrefix, TNyxPascalSyntaxOptions.Defaults);
    First := LNext;
  end
  else
  begin
    {$ifndef PAS2JS}
    for LReader := 0 to High(LReaders) do
    begin
      LReaders[LReader] := nil;
    end;
    LReadersOkay := True;
    LReaderError := '';
    try

      if Calls = 2 then
      begin
        for LReader := 0 to High(LReaders) do
        begin
          LReaders[LReader] := TPlanReader.Create(True);
          LReaders[LReader].FreeOnTerminate := False;
          LReaders[LReader].Source := Previous;
          LReaders[LReader].DeclarationIndex := Previous.Syntax.Named(0, 'TTarget');
          LReaders[LReader].Start;
        end;
        for LReader := 0 to High(LReaders) do
        begin
          LWait := 0;
          while not LReaders[LReader].StartedReading and (LWait < 1000) do
          begin
            TThread.Sleep(1);
            Inc(LWait);
          end;
          Check(LReaders[LReader].StartedReading, 'Native reader begins before concurrent memo inheritance');
        end;
      end;
    {$endif}
      LNext := AppendNyxPascalTranslatedSyntax(Previous, APrefix, TNyxPascalSyntaxOptions.Defaults);
    {$ifndef PAS2JS}
    finally
      for LReader := 0 to High(LReaders) do
      begin

        if LReaders[LReader] <> nil then
        begin
          LReaders[LReader].StopReading;
          LReaders[LReader].WaitFor;
        end;
      end;
      for LReader := 0 to High(LReaders) do
      begin

        if LReaders[LReader] <> nil then
        begin
          LReadersOkay := LReadersOkay and (LReaders[LReader].ErrorText = '') and
            (LReaders[LReader].Iterations > 0);
          LReaderError := LReaderError + LReaders[LReader].ErrorText;
          Inc(GThreadReaders);
          LReaders[LReader].Free;
        end;
      end;
    end;
    Check(LReadersOkay, 'Concurrent reads preserve source, grammar and memo ownership: ' + LReaderError);
    {$endif}
  end;
  LIndex := LNext.Syntax.Named(0, 'TTarget');
  LBefore := Memo(LNext);
  LFirstType := LNext.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults);
  LType := LNext.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults);
  EqualTypes(LType, NewNyxPascalDeclaredTypeSyntax(LNext.Syntax,
    LNext.Syntax.Declaration(LIndex).TypeSpan, TNyxPascalTypeOptions.Defaults));
  EqualTypes(LFirstType, LType);
  LAfter := Memo(LNext);
  Check((LAfter.Hits > LBefore.Hits) and (LAfter.Entries > 0), 'Repeated queries use the pure type memo');

  if Calls > 1 then
  begin
    Check((LAfter.InheritedCandidates > 0) and (LAfter.Hits - LBefore.Hits = 2),
      'Both inherited and repeated queries actually reuse validated plans in a new prefix');
  end;
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LNext);
  LAnswer := LWorld.Lookup(LWorld.At(0, 0, LNext.Syntax.Lexical.Scalars), NyxPascalBindingName('Value'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1), 'The current variable resolves in its own world');
  LAnswer := LWorld.TypeOfSymbol(LAnswer.SymbolAt(0));
  Check((LAnswer.Status = pbsBound) and (LAnswer.World = LWorld) and
    (LAnswer.BoundType.World = LWorld), 'Binding a memoized structured type still creates current-world results');
  Previous := LNext;
  Result := ppkTrue;
end;

function TPlanCollector.Evaluate(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
begin
  Result := ppkUnknown;
  raise Exception.Create('Type-plan source fixture needs its typed prefix');
end;

procedure Prefixes;
const
  CText: TNyxText = 'unit MemoPrefixes; interface type TTarget=array of record Code:Byte; end; ' +
    'var Value:TTarget; const Guard=1; Boundary=2; ' +
    '{$if declared(TTarget)}{$endif}const Middle=3; ' +
    '{$if declared(Value)}{$endif}const Last=4; ' +
    '{$if declared(Last)}{$endif}implementation end.';
var
  LCollector: TPlanCollector;
  LOwner: INyxPascalConditionalFacts;
  LSource: INyxPascalTranslation;
  LFirst: INyxPascalTranslatedSyntax;
  LLast: INyxPascalTranslatedSyntax;
begin
  LCollector := TPlanCollector.Create;
  LOwner := LCollector;
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('root.pas'), CText), NyxPascalPhysicalFile('root.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults, nil, LOwner);
  Check(LSource.Resolved and (LCollector.Calls = 3), 'Three actual chronological callbacks exercise plan inheritance');
  LFirst := LCollector.First;
  LLast := LCollector.Previous;
  LOwner := nil;
  LSource := nil;
  Check((LFirst.Source.Files.TextAt(0) = CText) and (LLast.Source.Files.TextAt(0) = CText),
    'Independent source views survive callback/translation retirement without retaining that authority');
end;

procedure Bounds;
var
  LText: TNyxText;
  LArgs: TNyxText;
  LAt: Integer;
  LView: INyxPascalTranslatedSyntax;
  LType: INyxPascalTypeSyntax;
  LBefore: TNyxPascalTypeMemoReceipt;
  LAfter: TNyxPascalTypeMemoReceipt;
  LIndex: Integer;
  LCancellation: INyxCancellationScope;
  LRejected: Boolean;
begin
  LView := View('unit Recovery; interface type TBad=array of ; implementation end.');
  LIndex := LView.Syntax.Named(0, 'TBad');
  LType := LView.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults);
  Check(not LType.Complete and (LType.DiagnosticCount > 0),
    'Recoverable unfinished type grammar still returns its ordinary diagnostics');
  EqualTypes(LType, LView.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults));
  LAfter := Memo(LView);
  Check((LAfter.Entries = 0) and (LAfter.Misses = 2),
    'Recoverable grammar is reparsed rather than admitted as a complete reusable plan');
  LText := 'unit EntryBound; interface type ';
  for LAt := 1 to 80 do
  begin
    LText := LText + 'T' + TNyxText(IntToStr(LAt)) + '=array of Byte; ';
  end;
  LView := View(LText + 'implementation end.');
  for LAt := 1 to 80 do
  begin
    LIndex := LView.Syntax.Named(0, 'T' + TNyxText(IntToStr(LAt)));
    LType := LView.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults);
    Check(LType.Complete, 'Each admitted small type keeps ordinary grammar behavior');
  end;
  LBefore := Memo(LView);
  Check((LBefore.Entries = 64) and (LBefore.RetainedNodes <= 8192), 'Plan entry retention is bounded independently of source size');
  LIndex := LView.Syntax.Named(0, 'T1');
  LType := LView.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults);
  LAfter := Memo(LView);
  Check((LAfter.Misses = LBefore.Misses + 1) and (LAfter.Entries = 64), 'Evicted grammar reparses normally and preserves the bound');
  LRejected := False;
  try
    LType := LView.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults.UpToNodes(1));
  except
    on ENyxPascalTypeLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A cached larger type cannot bypass a smaller caller node quota');
  LCancellation := NewNyxCancellationScope;
  LCancellation.Cancel;
  LRejected := False;
  try
    LType := LView.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults, LCancellation);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A populated memo still respects cancellation before reuse');
  LArgs := 'Byte';
  for LAt := 2 to 500 do
  begin
    LArgs := LArgs + ',Byte';
  end;
  LText := 'unit NodeBound; interface type ';
  for LAt := 1 to 20 do
  begin
    LText := LText + 'T' + TNyxText(IntToStr(LAt)) + '=TContainer<' + LArgs + '>; ';
  end;
  LView := View(LText + 'implementation end.');
  for LAt := 1 to 20 do
  begin
    LType := LView.TypeOfDeclaration(LView.Syntax.Named(0, 'T' + TNyxText(IntToStr(LAt))),
      TNyxPascalTypeOptions.Defaults);
    Check(LType.Complete, 'Large unbound generic grammar remains a normal type query');
  end;
  LAfter := Memo(LView);
  Check((LAfter.Entries > 0) and (LAfter.Entries < 20) and (LAfter.RetainedNodes <= 8192),
    'Aggregate node retention evicts before the entry-count bound is reached');
  LArgs := 'Byte';
  for LAt := 2 to 8300 do
  begin
    LArgs := LArgs + ',Byte';
  end;
  LView := View('unit Oversized; interface type T=TContainer<' + LArgs + '>; implementation end.');
  LType := LView.TypeOfDeclaration(LView.Syntax.Named(0, 'T'),
    TNyxPascalTypeOptions.Defaults.UpToNodes(12000));
  LAfter := Memo(LView);
  Check(LType.Complete and (LType.Count > 8192) and (LAfter.Entries = 0),
    'A permitted oversized grammar result is returned without retaining an oversized memo entry');
end;

begin
  try
    Plans;
    Prefixes;
    Bounds;
    {$ifndef PAS2JS}ConcurrentEviction;{$endif}
    WriteLn('native concurrent readers / ', GThreadReaders);
    WriteLn('PASS ', GChecks, ' source-free type-plan and bounded memo checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
