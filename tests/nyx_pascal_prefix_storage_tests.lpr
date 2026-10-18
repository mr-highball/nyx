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
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.macros;

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
  SetLength(LSaved, Length(LProbe.Windows));
  for LAt := 0 to High(LProbe.Windows) do
  begin
    LSaved[LAt] := LProbe.Windows[LAt];
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

begin
  try
    LexicalStorage;
    TranslationStorage;
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
