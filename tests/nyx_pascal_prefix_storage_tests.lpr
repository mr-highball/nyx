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
program nyx_pascal_prefix_storage_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.macros, nyx.pascal.work;

type
  { Retains detached windows and a revocable lexical borrow. Deliberate failure
    happens after sharing begins; neither saved window retains this collector. }
  TLexicalCollector = class(TInterfacedObject, INyxPascalLexicalObserver,
    INyxPascalLexicalConstructionObserver)
  public
    Capture: INyxPascalLexicalPrefixCapture;
    Windows: array of INyxPascalLexicalPrefix;
    AbortAfter: Integer;
    CancelAfter: Integer;
    Cancellation: INyxCancellationScope;
    procedure BeginRead(const ACapture: INyxPascalLexicalPrefixCapture);
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean);
  end;
  { Supplies deliberate fixture truth through the typed synchronous boundary.
    Saved translations retain exact frozen values, never this source authority.
    The fixture tests storage/ordering, not declaration resolution or compilation. }
  TTranslationCollector = class(TInterfacedObject, INyxPascalConditionalFacts,
    INyxPascalPrefixConditionalFacts)
  public
    Windows: array of INyxPascalTranslation;
    AbortAfter: Integer;
    CancelAfter: Integer;
    Cancellation: INyxCancellationScope;
    UnknownAfter: Integer;
    function PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
    function EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
      const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
    function Evaluate(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
    destructor Destroy; override;
  end;

var
  GChecks: Integer;
  GCollectorRetired: Boolean;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function SameToken(const ALeft, ARight: TNyxPascalToken): Boolean;
begin
  Result := (ALeft.Span.Start = ARight.Span.Start) and
    (ALeft.Span.Length = ARight.Span.Length) and (ALeft.Kind = ARight.Kind) and
    (ALeft.Word = ARight.Word) and (ALeft.Symbol = ARight.Symbol) and
    (ALeft.NumberBase = ARight.NumberBase) and (ALeft.Line = ARight.Line) and
    (ALeft.Column = ARight.Column) and (ALeft.Complete = ARight.Complete) and
    (ALeft.Escaped = ARight.Escaped) and (ALeft.RealNumber = ARight.RealNumber) and
    (ALeft.Region = ARight.Region);
end;

procedure TLexicalCollector.BeginRead(const ACapture: INyxPascalLexicalPrefixCapture);
begin
  Capture := ACapture;
end;

procedure TLexicalCollector.AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText; var AAllowNestedComments: Boolean);
var
  LAt: Integer;
begin

  if not ((AIndex in [0, 1, 254, 255]) or (AIndex mod 257 = 0) or
    (AIndex = 256) or (AIndex = 511) or (AIndex = 512) or
    (AToken.Kind = ptkEOF)) then
  begin
    Exit;
  end;
  LAt := Length(Windows);
  SetLength(Windows, LAt + 2);
  Windows[LAt] := Capture.Freeze;
  Windows[LAt + 1] := Capture.Freeze;

  if (AbortAfter > 0) and (Length(Windows) >= AbortAfter) then
  begin
    raise EAbort.Create('Deliberate shared lexical retirement');
  end;

  if (CancelAfter > 0) and (Length(Windows) >= CancelAfter) then
  begin
    Cancellation.Cancel;
  end;
end;

procedure LexicalStorage;
var
  LText: TNyxText;
  LProbe: TLexicalCollector;
  LOwner: INyxPascalLexicalObserver;
  LSnapshot: INyxPascalLexicalSnapshot;
  LRejectedSnapshot: INyxPascalLexicalSnapshot;
  LStorage: INyxPascalPrefixTokenStorage;
  LRetiredWindow: INyxPascalLexicalPrefix;
  LAt: Integer;
  LRow: Integer;
  LMode: Integer;
  LLogical: Integer;
  LCopied: Integer;
  LRefused: Boolean;
  LEqual: Boolean;
begin
  LText := 'unit FrozenBlocks; { exact 🚀 ' + #0 + ' } &type $ff 1.25 ';
  for LAt := 1 to 4200 do
  begin
    LText := LText + 'x ';
  end;
  LText := LText + 'asm mov ax, 0 end ' + #39 + 'unfinished';
  LSnapshot := NewNyxPascalLexicalSnapshot(LText);
  for LMode := 0 to 2 do
  begin
    LProbe := TLexicalCollector.Create;
    LOwner := LProbe;
    LProbe.Cancellation := NewNyxCancellationScope;
    LProbe.AbortAfter := Ord(LMode = 1) * 24;
    LProbe.CancelAfter := Ord(LMode = 2) * 24;
    LRejectedSnapshot := nil;
    LRefused := False;
    try
      LRejectedSnapshot := NewNyxPascalContextualLexicalSnapshot(LText,
        TNyxPascalLexicalOptions.Defaults, LOwner, LProbe.Cancellation);
    except
      on EAbort do
      begin
        LRefused := LMode = 1;
      end;
      on ENyxPascalAnalysisCancelled do
      begin
        LRefused := LMode = 2;
      end;
    end;
    Check(((LMode = 0) and not LRefused and (LRejectedSnapshot <> nil)) or
      ((LMode <> 0) and LRefused and (LRejectedSnapshot = nil)),
      'Shared lexical construction preserves success, callback failure and cancellation publication');
    LLogical := 0;
    LCopied := 0;
    for LAt := 0 to High(LProbe.Windows) do
    begin
      Check(Supports(LProbe.Windows[LAt], INyxPascalPrefixTokenStorage, LStorage) and
        (LStorage.TokenRows = LProbe.Windows[LAt].Count) and
        (LStorage.TokenRows = LStorage.CopiedTokenRows + LStorage.SharedTokenRows),
        'Each immutable lexical arena accounts separately for copied and shared rows');
      Inc(LLogical, LStorage.TokenRows);
      Inc(LCopied, LStorage.CopiedTokenRows);
      LEqual := LProbe.Windows[LAt].Source = LText;
      for LRow := 0 to LProbe.Windows[LAt].Count - 1 do
      begin
        LEqual := LEqual and SameToken(LProbe.Windows[LAt].Token(LRow), LSnapshot.Token(LRow)) and
          (LProbe.Windows[LAt].Text(LRow) = LSnapshot.Text(LRow));
      end;
      Check(LEqual, 'Every frozen row remains equal to independent full lexing after reader retirement');
    end;
    Check((LCopied < LLogical div 2) and (LStorage.SharedTokenRows > 0) and
      (LStorage.DirectoryEntries < LStorage.TokenRows),
      'Many growing windows reuse actual token storage instead of recopying their full prefixes');
    Check((LProbe.Windows[0].DiagnosticCount = 0) and
      ((LMode <> 0) or (LProbe.Windows[High(LProbe.Windows)].DiagnosticCount = 1)),
      'A later unfinished-string diagnostic cannot mutate an earlier frozen window');
    LRefused := False;
    try
      LRetiredWindow := LProbe.Capture.Freeze;
    except
      on ENyxPascalLexical do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRetiredWindow = nil), 'A cached immutable block cannot revive a retired lexical borrow');
    LStorage := nil;
    LOwner := nil;
  end;
end;

function TTranslationCollector.PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
begin
  Result := True;
end;

function TTranslationCollector.EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
  const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
var
  LAt: Integer;
begin
  LAt := Length(Windows);
  SetLength(Windows, LAt + 1);
  Windows[LAt] := APrefix;
  Check(APrefix.Count = AOrigin.PrefixCount, 'A shared translation retains the exact exclusive condition boundary');

  if (AbortAfter > 0) and (Length(Windows) >= AbortAfter) then
  begin
    raise EAbort.Create('Deliberate shared translation retirement');
  end;

  if (CancelAfter > 0) and (Length(Windows) >= CancelAfter) then
  begin
    Cancellation.Cancel;
  end;
  Result := ppkTrue;

  if (UnknownAfter > 0) and (Length(Windows) >= UnknownAfter) then
  begin
    Result := ppkUnknown;
  end;
end;

function TTranslationCollector.Evaluate(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
begin
  Result := ppkUnknown;
  raise Exception.Create('The storage fixture requires the typed prefix boundary');
end;

destructor TTranslationCollector.Destroy;
begin
  GCollectorRetired := True;
  inherited Destroy;
end;

procedure TranslationStorage;
const
  CPart: TNyxText = '{$if declared(Part)} const Part = VALUE; {$endif}' +
    '{$push}{$H+}{$if declared(Held)}{$endif}{$pop}';
var
  LText: TNyxText;
  LFiles: INyxPascalFileCatalog;
  LFacts: INyxPascalCompilerContext;
  LProbe: TTranslationCollector;
  LOwner: INyxPascalConditionalFacts;
  LSnapshot: INyxPascalTranslation;
  LRefusedSnapshot: INyxPascalTranslation;
  LSaved: array of INyxPascalTranslation;
  LStorage: INyxPascalPrefixTokenStorage;
  LSettings: INyxPascalTranslationSettings;
  LFinalSettings: INyxPascalTranslationSettings;
  LMacros: INyxPascalTranslationMacros;
  LFinalMacros: INyxPascalTranslationMacros;
  LValidation: INyxPascalPrefixValidationWork;
  LFullValidation: INyxPascalPrefixValidationWork;
  LFull: INyxPascalTranslation;
  LNested: INyxPascalTranslation;
  LPreviousCount: Integer;
  LPreparedCount: Integer;
  LAt: Integer;
  LRow: Integer;
  LState: Integer;
  LFrame: Integer;
  LMode: Integer;
  LLogical: Integer;
  LCopied: Integer;
  LEqual: Boolean;
  LRefused: Boolean;
  LToken: TNyxPascalTranslatedToken;
  LFinalToken: TNyxPascalTranslatedToken;
begin
  LText := 'unit FrozenTranslation; {$macro on}{$define VALUE:=42}{$push}{$H-} interface const Anchor=1; ';
  for LAt := 1 to 8 do
  begin
    for LRow := 1 to 40 do
    begin
      LText := LText + 'C' + TNyxText(IntToStr(LAt * 100 + LRow)) + '=VALUE; ';
    end;
    LText := LText + '{$if declared(Anchor)}{$endif}';

    if LAt in [3, 6] then
    begin
      LText := LText + '{$include part.inc}';
    end;

    if LAt = 4 then
    begin
      LText := LText + '{$define VALUE:=84}';
    end;
  end;
  LText := LText + '{$pop}{$if declared(Anchor)}{$endif} implementation end.';
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('root.pas'), LText)
    .WithFile(NyxPascalPhysicalFile('part.inc'), CPart);
  LFacts := NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithOption('H', ppkTrue).WithFeature(ppfNestedComments, ppkFalse);
  LProbe := TTranslationCollector.Create;
  LOwner := LProbe;
  LSnapshot := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('root.pas'),
    LFacts, TNyxPascalTranslationOptions.Defaults, nil, LOwner);
  Check(LSnapshot.Resolved and (Length(LProbe.Windows) = 13) and
    (LSnapshot.OccurrenceCount = 3), 'Repeated includes retain independent exact source windows');
  Check(not Supports(LSnapshot, INyxPascalPrefixTokenStorage, LStorage),
    'An ordinary complete translation does not advertise detached-prefix storage');
  Supports(LSnapshot, INyxPascalTranslationSettings, LFinalSettings);
  Supports(LSnapshot, INyxPascalTranslationMacros, LFinalMacros);
  LLogical := 0;
  LCopied := 0;
  LPreviousCount := 0;
  LPreparedCount := 0;
  SetLength(LSaved, Length(LProbe.Windows));
  for LAt := 0 to High(LProbe.Windows) do
  begin
    LSaved[LAt] := LProbe.Windows[LAt];
    Check(Supports(LSaved[LAt], INyxPascalPrefixValidationWork, LValidation) and
      (LValidation.PreparedTokens = LSaved[LAt].Count - LPreviousCount) and
      (LValidation.RetainedTokens = LPreviousCount) and
      (LValidation.ReusedTokens = LSaved[LAt].Count),
      'An owned exact boundary prepares only newly appended token facts');
    Inc(LPreparedCount, LValidation.PreparedTokens);
    LPreviousCount := LSaved[LAt].Count;
    LFull := NewNyxPascalTranslationPrefix(LSnapshot, LSaved[LAt].Count,
      TNyxPascalTranslationOptions.Defaults);
    Check(Supports(LFull, INyxPascalPrefixValidationWork, LFullValidation) and
      (LFullValidation.PreparedTokens = LFull.Count) and
      (LFullValidation.RetainedTokens = 0) and (LFullValidation.ReusedTokens = 0) and
      (LFull.Resolved = LSaved[LAt].Resolved) and
      (LFull.StateCount = LSaved[LAt].StateCount) and
      (LFull.OccurrenceCount = LSaved[LAt].OccurrenceCount) and
      (LFull.DiagnosticCount = LSaved[LAt].DiagnosticCount),
      'A fresh full scan independently agrees with the exact owned summary');
    LNested := NewNyxPascalTranslationPrefix(LSaved[LAt], LSaved[LAt].Count - 1,
      TNyxPascalTranslationOptions.Defaults);
    Check(Supports(LNested, INyxPascalPrefixValidationWork, LFullValidation) and
      (LFullValidation.PreparedTokens = LNested.Count) and (LFullValidation.ReusedTokens = 0),
      'Public lineage and reuse receipts cannot donate private authority at another cut');
    Check(Supports(LSaved[LAt], INyxPascalPrefixTokenStorage, LStorage) and
      (LStorage.TokenRows = LSaved[LAt].Count + 1) and
      (LStorage.TokenRows = LStorage.CopiedTokenRows + LStorage.SharedTokenRows),
      'Translated storage counts its retained exclusive boundary token separately from the public view');
    Inc(LLogical, LStorage.TokenRows);
    Inc(LCopied, LStorage.CopiedTokenRows);
    LEqual := LSaved[LAt].Files.TextAt(0) = LText;
    for LRow := 0 to LSaved[LAt].Count - 1 do
    begin
      LToken := LSaved[LAt].Token(LRow);
      LFinalToken := LSnapshot.Token(LRow);
      LEqual := LEqual and (LToken.Occurrence = LFinalToken.Occurrence) and
        (LToken.PhysicalToken = LFinalToken.PhysicalToken) and (LToken.State = LFinalToken.State) and
        (LToken.Activity = LFinalToken.Activity) and (LToken.Expanded = LFinalToken.Expanded) and
        (LToken.Expansion = LFinalToken.Expansion) and (LToken.ExpansionToken = LFinalToken.ExpansionToken) and
        SameToken(LSaved[LAt].PhysicalToken(LRow), LSnapshot.PhysicalToken(LRow)) and
        (LSaved[LAt].Text(LRow) = LSnapshot.Text(LRow)) and
        (NyxPascalEffectiveText(LSaved[LAt], LRow) = NyxPascalEffectiveText(LSnapshot, LRow));
    end;
    Check(LEqual, 'Shared translated rows preserve physical occurrences, macro expansion and historical pre-state');
    Supports(LSaved[LAt], INyxPascalTranslationSettings, LSettings);
    Supports(LSaved[LAt], INyxPascalTranslationMacros, LMacros);
    LEqual := True;
    for LState := 0 to LSaved[LAt].StateCount - 1 do
    begin
      LEqual := LEqual and LSaved[LAt].State(LState).SameFacts(LSnapshot.State(LState)) and
        LMacros.StateMacros(LState).SameMacros(LFinalMacros.StateMacros(LState)) and
        (LSettings.SettingsDepth(LState) = LFinalSettings.SettingsDepth(LState));
      for LFrame := 0 to LSettings.SettingsDepth(LState) - 1 do
      begin
        LEqual := LEqual and LSettings.SavedSettings(LState, LFrame).SameFacts(
          LFinalSettings.SavedSettings(LState, LFrame)) and
          (LSettings.SettingsOrigin(LState, LFrame).Occurrence =
            LFinalSettings.SettingsOrigin(LState, LFrame).Occurrence) and
          (LSettings.SettingsOrigin(LState, LFrame).Span.Start =
            LFinalSettings.SettingsOrigin(LState, LFrame).Span.Start);
      end;
    end;
    Check(LEqual, 'Saved local settings and macro environments remain exact through later includes and POP');
  end;
  Check((LCopied < LLogical div 2) and (LStorage.SharedTokenRows > 0),
    'Growing translated windows share earlier immutable rows without a recursive owner chain');
  Check((LPreparedCount = LPreviousCount) and (LPreparedCount < LLogical div 2),
    'Chronological token validation examines each earlier token once, independently of storage sharing');
  WriteLn('owned prefix validation / prepared=', LPreparedCount, ' full-window tokens=', LLogical);
  GCollectorRetired := False;
  LOwner := nil;
  Check(GCollectorRetired and (LSaved[0].Files.TextAt(0) = LText),
    'Retaining frozen windows cannot retain their retired callback authority');
  for LMode := 1 to 2 do
  begin
    LProbe := TTranslationCollector.Create;
    LOwner := LProbe;
    LProbe.Cancellation := NewNyxCancellationScope;
    LProbe.AbortAfter := Ord(LMode = 1) * 4;
    LProbe.CancelAfter := Ord(LMode = 2) * 4;
    LRefusedSnapshot := nil;
    LRefused := False;
    try
      LRefusedSnapshot := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('root.pas'),
        LFacts, TNyxPascalTranslationOptions.Defaults, LProbe.Cancellation, LOwner);
    except
      on EAbort do
      begin
        LRefused := LMode = 1;
      end;
      on ENyxPascalAnalysisCancelled do
      begin
        LRefused := LMode = 2;
      end;
    end;
    Check(LRefused and (LRefusedSnapshot = nil) and (Length(LProbe.Windows) = 4),
      'Failure or cancellation after token sharing publishes no ordinary translation');
    Check((LProbe.Windows[0].Text(0) = LSaved[0].Text(0)) and
      Supports(LProbe.Windows[3], INyxPascalPrefixTokenStorage, LStorage) and
      (LStorage.SharedTokenRows > 0), 'Detached shared windows survive exceptional builder retirement');
    LOwner := nil;
  end;
end;

{ Compare independently validated public windows, including include bounds and
  diagnostic ownership. Physical lexical windows may differ in unseen suffix
  length; every exposed token/origin/state remains exact. }
procedure EqualPrefix(const ALeft, ARight: INyxPascalTranslation);
var
  LAt: Integer;
  LLeftToken: TNyxPascalTranslatedToken;
  LRightToken: TNyxPascalTranslatedToken;
  LLeftInclusion: TNyxPascalInclusion;
  LRightInclusion: TNyxPascalInclusion;
  LLeftDiagnostic: TNyxPascalTranslationDiagnostic;
  LRightDiagnostic: TNyxPascalTranslationDiagnostic;
  LEqual: Boolean;
begin
  LEqual := (ALeft.Count = ARight.Count) and (ALeft.StateCount = ARight.StateCount) and
    (ALeft.OccurrenceCount = ARight.OccurrenceCount) and
    (ALeft.DiagnosticCount = ARight.DiagnosticCount) and (ALeft.Resolved = ARight.Resolved);
  Check(LEqual, 'Exact summary and independent full scan expose identical prefix domains');
  for LAt := 0 to ALeft.Count - 1 do
  begin
    LLeftToken := ALeft.Token(LAt);
    LRightToken := ARight.Token(LAt);
    LEqual := LEqual and (LLeftToken.Occurrence = LRightToken.Occurrence) and
      (LLeftToken.PhysicalToken = LRightToken.PhysicalToken) and
      (LLeftToken.State = LRightToken.State) and (LLeftToken.Activity = LRightToken.Activity) and
      (LLeftToken.Expanded = LRightToken.Expanded) and
      (LLeftToken.Expansion = LRightToken.Expansion) and
      (LLeftToken.ExpansionToken = LRightToken.ExpansionToken) and
      SameToken(ALeft.PhysicalToken(LAt), ARight.PhysicalToken(LAt)) and
      (ALeft.Text(LAt) = ARight.Text(LAt)) and
      (NyxPascalEffectiveText(ALeft, LAt) = NyxPascalEffectiveText(ARight, LAt));
  end;
  Check(LEqual, 'Token, macro and original physical Unicode values agree after continuation');
  for LAt := 0 to ALeft.StateCount - 1 do
  begin
    LEqual := LEqual and ALeft.State(LAt).SameFacts(ARight.State(LAt));
  end;
  Check(LEqual, 'Captured settings and state visibility do not advance past the boundary');
  for LAt := 0 to ALeft.OccurrenceCount - 1 do
  begin
    LLeftInclusion := ALeft.Occurrence(LAt);
    LRightInclusion := ARight.Occurrence(LAt);
    LEqual := LEqual and (LLeftInclusion.FileIndex = LRightInclusion.FileIndex) and
      (LLeftInclusion.Parent = LRightInclusion.Parent) and
      (LLeftInclusion.IncludeSpan.Start = LRightInclusion.IncludeSpan.Start) and
      (LLeftInclusion.IncludeSpan.Length = LRightInclusion.IncludeSpan.Length) and
      (LLeftInclusion.FirstToken = LRightInclusion.FirstToken) and
      (LLeftInclusion.TokenCount = LRightInclusion.TokenCount) and
      (LLeftInclusion.EntryState = LRightInclusion.EntryState) and
      (LLeftInclusion.ExitState = LRightInclusion.ExitState);
  end;
  Check(LEqual, 'Empty and repeated include occurrences preserve independent exact bounds');
  for LAt := 0 to ALeft.DiagnosticCount - 1 do
  begin
    LLeftDiagnostic := ALeft.Diagnostic(LAt);
    LRightDiagnostic := ARight.Diagnostic(LAt);
    LEqual := LEqual and (LLeftDiagnostic.Issue = LRightDiagnostic.Issue) and
      (LLeftDiagnostic.Occurrence = LRightDiagnostic.Occurrence) and
      (LLeftDiagnostic.Span.Start = LRightDiagnostic.Span.Start) and
      (LLeftDiagnostic.Span.Length = LRightDiagnostic.Span.Length) and
      (LLeftDiagnostic.MessageText = LRightDiagnostic.MessageText);
  end;
  Check(LEqual, 'Diagnostics remain filtered by exact per-occurrence physical offsets');
end;

procedure SummaryBoundaries;
var
  LFiles: INyxPascalFileCatalog;
  LProbe: TTranslationCollector;
  LOwner: INyxPascalConditionalFacts;
  LSnapshot: INyxPascalTranslation;
  LFull: INyxPascalTranslation;
  LBudget: INyxPascalAnalysisBudget;
  LValidation: INyxPascalPrefixValidationWork;
  LFacts: INyxPascalCompilerContext;
  LText: TNyxText;
  LMode: Integer;
  LAt: Integer;
  LCount: Integer;
  LLimit: Integer;
  LRejected: Boolean;
begin
  LFacts := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
  for LMode := 0 to 4 do
  begin
    LText := 'unit Summary; {$macro on}{$define VALUE:=42} interface ' +
      '{ original 🚀 ' + #0 + ' } const Anchor=VALUE; {$include empty.inc}' +
      '{$if declared(Anchor)}{$endif}{$include part.inc}' +
      '{$if declared(Anchor)}{$endif}{$include part.inc}' +
      '{$if declared(Anchor)}{$endif} implementation end.';

    if LMode = 2 then
    begin
      LText := 'unit Summary; interface 🚀 ' +
        '{$if declared(Anchor)}{$endif}{$if declared(Anchor)}{$endif} implementation end.';
    end;

    if LMode = 3 then
    begin
      LText := 'unit Summary; interface {$include missing.inc} ' +
        '{$if declared(Anchor)}{$endif}{$if declared(Anchor)}{$endif} implementation end.';
    end;

    if LMode = 4 then
    begin
      LText := '{$if declared(First)}{$endif}unit Summary; interface ' +
        '{$if declared(Later)}{$endif} implementation end.';
    end;
    LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('root.pas'), LText)
      .WithFile(NyxPascalPhysicalFile('empty.inc'), '')
      .WithFile(NyxPascalPhysicalFile('part.inc'), '{$push}{$H-} const Part=VALUE; {$pop}');
    LProbe := TTranslationCollector.Create;
    LOwner := LProbe;

    if LMode = 1 then
    begin
      LProbe.UnknownAfter := 1;
    end;
    LSnapshot := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('root.pas'),
      LFacts, TNyxPascalTranslationOptions.Defaults, nil, LOwner);
    WriteLn('summary stream / mode=', LMode, ' windows=', Length(LProbe.Windows),
      ' resolved=', LSnapshot.Resolved);

    if LMode = 3 then
    begin
      Check((Length(LProbe.Windows) = 0) and (LSnapshot.DiagnosticCount > 0) and
        not LSnapshot.Resolved, 'A missing include refuses later prefix capture instead of donating guessed authority');
      LFull := NewNyxPascalTranslationPrefix(LSnapshot, LSnapshot.Count - 1,
        TNyxPascalTranslationOptions.Defaults);
      Check(Supports(LFull, INyxPascalPrefixValidationWork, LValidation) and
        (LValidation.PreparedTokens = LFull.Count) and (LValidation.ReusedTokens = 0) and
        not LFull.Resolved, 'An explicitly inspected failed source retains full validation and diagnostics');
      LOwner := nil;
      Continue;
    end;
    Check(Length(LProbe.Windows) >= 2,
      'Resolved, unknown and diagnostic streams retain distinct chronological boundaries');
    for LAt := 0 to High(LProbe.Windows) do
    begin
      LFull := NewNyxPascalTranslationPrefix(LSnapshot, LProbe.Windows[LAt].Count,
        TNyxPascalTranslationOptions.Defaults);
      EqualPrefix(LProbe.Windows[LAt], LFull);
      Check(Supports(LProbe.Windows[LAt], INyxPascalPrefixValidationWork, LValidation) and
        (LValidation.ReusedTokens = LProbe.Windows[LAt].Count),
        'Only exact owned frozen boundaries reuse prepared facts across these streams');
    end;
    Check((LMode in [0, 4]) = LSnapshot.Resolved,
      'Summary reuse preserves explicit unknown activity and encountered diagnostic failure');
    LOwner := nil;
  end;

  LText := 'unit Budget; interface const Anchor=1; ' +
    '{$if declared(Anchor)}const First=1;{$endif}' +
    '{$if declared(Anchor)}const Second=2;{$endif} implementation end.';
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('root.pas'), LText);
  LProbe := TTranslationCollector.Create;
  LOwner := LProbe;
  LBudget := NewNyxPascalAnalysisBudget(1000000);
  LSnapshot := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('root.pas'), LFacts,
    TNyxPascalTranslationOptions.Defaults, LBudget, LOwner);
  LCount := LBudget.Receipt.Total;
  Check(LSnapshot.Resolved and (Length(LProbe.Windows) = 2),
    'The complete guarded source uses both ordinary summary-backed questions');
  LBudget.Retire;
  LOwner := nil;
  for LLimit := 1 to LCount do
  begin
    LProbe := TTranslationCollector.Create;
    LOwner := LProbe;
    LBudget := NewNyxPascalAnalysisBudget(LLimit);
    LFull := nil;
    LRejected := False;
    try
      LFull := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('root.pas'), LFacts,
        TNyxPascalTranslationOptions.Defaults, LBudget, LOwner);
    except
      on ENyxPascalAnalysisWorkLimit do
      begin
        LRejected := True;
      end;
    end;
    Check((LBudget.Receipt.Total <= LLimit) and
      (((LLimit < LCount) and LRejected and (LFull = nil)) or
      ((LLimit = LCount) and not LRejected and (LFull.Count = LSnapshot.Count))),
      'Every smaller source allowance refuses atomically; only the exact whole allowance completes');
    LBudget.Retire;
    LOwner := nil;
  end;
  Check((LFiles.TextAt(0) = LText) and LSnapshot.Resolved,
    'Exhaustive summary preparation refusal preserves input and the earlier completed source');
  WriteLn('guarded summary source units / ', LCount, ' / exhaustive smaller limits / ', LCount - 1);
end;

begin
  try
    LexicalStorage;
    TranslationStorage;
    SummaryBoundaries;
    WriteLn('PASS ', GChecks, ' immutable prefix token storage checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
