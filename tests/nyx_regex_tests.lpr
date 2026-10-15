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


program nyx_regex_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.regex.types, nyx.regex,
  nyx.unicode, nyx.unicode.types, nyx.scheduler, nyx.logic, nyx.logic.regex,
  nyx.regex.replacement, nyx.flow, nyx.flow.types, nyx.model, nyx.codec,
  nyx.codegen, nyx.source, nyx.flow.declarations, nyx.test.logic.regex,
  nyx.logic.editor, nyx.controls, nyx.studio.agents, nyx.studio.projects
  {$ifndef PAS2JS}, Classes{$endif}
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  { Cross-target transport digest of all scalar properties. Exact native
    comparisons against independently parsed pinned UCD inputs establish the
    expected values; this bounded checksum is supplementary, not authentication. }
  TScalarDigest = record
    A: Integer;
    B: Integer;
  end;
  { Deterministic cooperative cancellation source. A read threshold exercises
    cancellation during decoding/backtracking, without owning a UI or matcher. }
  TCountingScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Reads: Integer;
    Limit: Integer;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;

procedure TCountingScope.Cancel;
begin
  Limit := -1;
end;

function TCountingScope.GetCancelled: Boolean;
begin
  Inc(Reads);
  Result := Reads > Limit;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure First(const APattern, AInput, AExpected: TNyxText; AStart: Integer = 0);
var
  LMatch: TNyxRegexMatch;
begin
  LMatch := NewNyxRegex(NyxRegex(APattern)).First(AInput);
  Check(LMatch.Success and (LMatch.Group(0).Value = AExpected) and
    (LMatch.Group(0).Span.Start = AStart), 'Unexpected first match for ' + APattern);
end;

procedure Invalid(const APattern: TNyxText);
var
  LRegex: INyxRegex;
  LFailed: Boolean;
begin
  LFailed := False;
  try
    LRegex := NewNyxRegex(NyxRegex(APattern));
  except
    on E: ENyxRegex do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Malformed/unsupported syntax must refuse: ' + APattern);
end;

procedure UnicodeDatabase;
var
  LCategory: TScalarDigest;
  LFold: TScalarDigest;
  LScalar: Integer;

  procedure Mix(var ADigest: TScalarDigest; AValue: Integer);
  var
    LByte: Integer;
  begin
    for LByte := 0 to 3 do
    begin
      ADigest.A := (ADigest.A + (AValue and 255)) mod 65521;
      ADigest.B := (ADigest.B + ADigest.A) mod 65521;
      AValue := AValue shr 8;
    end;
  end;

begin
  LCategory := Default(TScalarDigest);
  LFold := Default(TScalarDigest);
  LCategory.A := 1;
  LFold.A := 1;
  for LScalar := 0 to $10FFFF do
  begin

    if (LScalar >= $D800) and (LScalar <= $DFFF) then
    begin
      Continue;
    end;
    Mix(LCategory, Ord(NyxScalarCategory(LScalar)));
    Mix(LFold, NyxSimpleFold(LScalar));
  end;
  Check((LCategory.A = 28926) and (LCategory.B = 45545),
    'All scalar categories match the pinned-UCD source oracle fingerprint');
  Check((LFold.A = 17448) and (LFold.B = 44124),
    'All scalar simple folds match the pinned-UCD source oracle fingerprint');
end;

procedure Core;
const
  CEmoji: TNyxText = '😀';
var
  LRegex: INyxRegex;
  LMatches: TNyxRegexMatches;
  LMatch: TNyxRegexMatch;
  LCapture: TNyxRegexCapture;
  LOptions: TNyxRegexOptions;
  LFailed: Boolean;
  LIndex: Integer;
  LScope: TCountingScope;
  LScopeLease: INyxCancellationScope;
  LText: TNyxText;
  LValue: Integer;
begin
  Check(NyxScalarCategory(Ord('A')) = nucUppercaseLetter, 'ASCII category');
  Check(NyxScalarCategory($1F600) = nucOtherSymbol, 'Supplementary symbol category');
  Check(NyxScalarCategory($10400) = nucUppercaseLetter, 'Supplementary letter category');
  Check(NyxSimpleFold($10400) = $10428, 'Unicode 17 simple supplementary folding');
  Check(NyxSimpleFold($1E9E) = $DF, 'Simple folding differs from full expansion');
  Check(not NyxScalarCaseEquivalent(Ord('A'), High(Integer), LValue), 'Case-closure indexing never overflows');
  First('ab+', 'xxabbb!', 'abbb', 2);
  First('ab+?', 'abbb', 'ab');
  First('(ab|a)b', 'ab', 'ab');
  First('(a|ab)c', 'abc', 'abc');
  First('(?:ab){2,3}', 'abababab', 'ababab');
  First('a{0}b', 'ab', 'b', 1);
  First('a{2,}a', 'aaaa', 'aaaa');
  First('a*ab', 'aaab', 'aaab');
  First('a*?ab', 'aaab', 'aaab');
  First('(a)+a', 'aaaa', 'aaaa');
  First('(ab)+ab', 'ababab', 'ababab');
  First('(a?)*b', 'aaab', 'aaab');
  First('(?:){3,9}x', 'x', 'x');
  First('(?>ab|a)c', 'abc', 'abc');
  Check(not NewNyxRegex(NyxRegex('(?>a|ab)c')).IsMatch('abc'), 'Atomic groups discard internal alternatives');
  Check(not NewNyxRegex(NyxRegex('a*+a')).IsMatch('aaa'), 'Possessive primitive repetition cannot give back input');
  Check(not NewNyxRegex(NyxRegex('(ab)++ab')).IsMatch('abab'), 'Possessive grouped repetition cannot give back input');
  First('(?<first>[a-z]+)-(?<second>[0-9]+)', 'prefix abc-123 tail', 'abc-123', 7);
  LMatch := NewNyxRegex(NyxRegex('(?<first>[a-z]+)-(?<second>[0-9]+)')).First('abc-123');
  Check((LMatch.Count = 3) and (LMatch.Group(NyxRegexGroup('first')).Value = 'abc') and
    (LMatch.Group(2).Value = '123'), 'Named and indexed captures share exact results');
  LRegex := NewNyxRegex(NyxRegex('(a)?(b*)'));
  LMatch := LRegex.First('');
  Check(not LMatch.Group(1).Span.Defined and LMatch.Group(2).Span.Defined and
    (LMatch.Group(2).Span.Count = 0), 'Unmatched and empty participating captures stay distinct');
  LCapture := LMatch.Group(2);
  LCapture.Value := 'Changed independently';
  Check(LMatch.Group(2).Value = '', 'Returned capture records cannot mutate retained matches');
  LMatches := LRegex.Find('ab ab');
  Check((LMatches.Count > 1) and (LMatch.Group(2).Value = ''), 'Searches and retained matches have independent state');
  First('(?<word>\w+)\s+\k<word>', 'one one two', 'one one');
  First('(a|b)\1', 'xxbb', 'bb', 2);
  Check(not NewNyxRegex(NyxRegex('(a)?\1b')).IsMatch('b'), 'Unmatched backreferences fail instead of inventing empty text');
  First('foo(?=bar)', 'foobar', 'foo');
  First('foo(?!bar)', 'fooqux', 'foo');
  First('(?<=foo)bar', 'foobar', 'bar', 3);
  First('(?<!foo)bar', 'xxbar', 'bar', 2);
  First('(?=(a+))\1', 'aaa', 'aaa');
  LMatch := NewNyxRegex(NyxRegex('(?<=(?<before>😀))(?<after>.)')).First('😀X');
  Check((LMatch.Group(0).Span.Start = 1) and (LMatch.Group(0).Span.Count = 1) and
    (LMatch.Group(NyxRegexGroup('before')).Value = CEmoji) and
    (LMatch.Group(NyxRegexGroup('before')).Span.Start = 0), 'Lookbehind captures retain scalar offsets outside the complete match');
  First('\Q(a+b)[x]\E', 'xx(a+b)[x]', '(a+b)[x]', 2);
  First('\Qab\E+', 'abbb', 'abbb');
  First('a\Q\E*', 'aaa', 'aaa');
  First('\Q*+\E+', '*+++', '*+++');
  First('\x41\u0042\u{1F600}', 'AB😀', 'AB😀');
  First('\uD83D\uDE00+', '😀😀', '😀😀');
  First('.', '😀', '😀');
  LMatches := NewNyxRegex(NyxRegex('')).Find('😀x');
  Check((LMatches.Count = 3) and (LMatches.Item(1).Group(0).Span.Start = 1) and
    (LMatches.Item(2).Group(0).Span.Start = 2), 'Empty matches advance one scalar, including EOF');
  First('\p{L}+', '123Καλημέρα😀', 'Καλημέρα', 3);
  First('[\p{General_Category=Letter}--[aeiou]]+', 'abcXYZ', 'bcXYZ', 1);
  First('[a-z&&[^aeiou]]+', 'abcXYZ', 'bc', 1);
  First('\d+', 'x١٢٣y', '١٢٣', 1);
  Check(not NewNyxRegex(NyxRegex('\d').CharacterClasses(nrgASCII)).IsMatch('١'),
    'ASCII shorthand policy is explicit');
  Check(NewNyxRegex(NyxRegex('\w+')).IsMatch('Привет'), 'Unicode word classes admit international letters');
  Check(not NewNyxRegex(NyxRegex('^\w+$').CharacterClasses(nrgASCII)).IsMatch('Привет'),
    'ASCII word classes do not silently become locale dependent');
  Check(NewNyxRegex(NyxRegex('^[a-z]+$').CaseMode(nrcUnicode)).IsMatch('Kſ'),
    'Unicode case closure includes Kelvin sign and long-s');
  Check(not NewNyxRegex(NyxRegex('^[a-z]+$').CaseMode(nrcASCII)).IsMatch('Kſ'),
    'ASCII case folding remains ASCII');
  Check(NewNyxRegex(NyxRegex('𐐀').CaseMode(nrcUnicode)).IsMatch('𐐨'), 'Supplementary caseless matching');
  Check(not NewNyxRegex(NyxRegex('ß').CaseMode(nrcUnicode)).IsMatch('ss'), 'Simple matching never expands sharp-s into two characters');
  Check(NewNyxRegex(NyxRegex('ß').CaseMode(nrcUnicode)).IsMatch('ẞ'), 'Simple matching closes capital sharp-s');
  First('(?i:a)(?-i:B)', 'aB', 'aB');
  Check(not NewNyxRegex(NyxRegex('(?i:a)(?-i:B)')).IsMatch('ab'), 'Scoped flags restore exact comparison');
  First('(?x) a # comment' + #10 + '\s+ b', 'a b', 'a b');
  First('[ #]+', ' # ', ' # ');
  Check(not NewNyxRegex(NyxRegex('a.b')).IsMatch('a' + #10 + 'b'), 'Default dot excludes line breaks');
  Check(NewNyxRegex(NyxRegex('a.b').DotAll).IsMatch('a' + #10 + 'b'), 'DotAll is typed and independent');
  LMatches := NewNyxRegex(NyxRegex('^|$').Multiline).Find('a' + #13#10 + 'b');
  Check((LMatches.Count = 4) and (LMatches.Item(1).Group(0).Span.Start = 1) and
    (LMatches.Item(2).Group(0).Span.Start = 3), 'CRLF has no fabricated middle anchor');
  Check(not NewNyxRegex(NyxRegex('a$')).IsMatch('a' + #10), 'Default dollar is a strict input end');
  Check(NewNyxRegex(NyxRegex('a\Z')).IsMatch('a' + #13#10), 'Explicit final-line-break assertion');
  Check(not NewNyxRegex(NyxRegex('a\z')).IsMatch('a' + #10), 'Strict end assertion');
  First('\bword\b', 'a word!', 'word', 2);
  First('\Boo\B', 'food', 'oo', 1);
  First('\0+', TNyxText(#0#0) + 'x', #0#0);

  LOptions := NyxRegex('(?<word>\w+)').CaseMode(nrcUnicode).Multiline.DotAll
    .Expanded.CharacterClasses(nrgASCII).Scan(nrsFirst)
    .Limits(NyxRegexLimits.InputScalars(64).ResultBytes(4096));
  Check(TNyxRegexOptions.FromData(LOptions.ToData).ToData.ToJSON = LOptions.ToData.ToJSON,
    'Closed typed options persist exactly');
  Check(NewNyxRegex(LOptions).Find('one two').Count = 1, 'Typed first-only extent');
  for LIndex := 0 to 4 do
  begin
    LFailed := False;
    try
      case LIndex of
        0: LMatches := NewNyxRegex(NyxRegex('(a|aa)+b')
          .Limits(NyxRegexLimits.WorkSteps(300))).Find('aaaaaaaaaaaaaaaaaaaaaaaa');
        1: LMatches := NewNyxRegex(NyxRegex('.')
          .Limits(NyxRegexLimits.InputScalars(2))).Find('abc');
        2: LMatches := NewNyxRegex(NyxRegex('.')
          .Limits(NyxRegexLimits.Matches(2))).Find('abc');
        3: LMatches := NewNyxRegex(NyxRegex('.')
          .Limits(NyxRegexLimits.ResultBytes(1))).Find('a');
        4: LMatches := NewNyxRegex(NyxRegex('(a|b)+c')
          .Limits(NyxRegexLimits.BacktrackStates(1))).Find('aabc');
      end;
    except
      on E: ENyxRegexLimit do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Complete matching budget must refuse atomically');
  end;
  Check(NewNyxRegex(NyxRegex('.').Limits(NyxRegexLimits.ResultBytes(1))).IsMatch('a'),
    'Boolean testing does not allocate or charge unused capture results');
  LText := StringOfChar('a', 50000);
  LRegex := NewNyxRegex(NyxRegex('(?<letters>a)+b')
    .Limits(NyxRegexLimits.BacktrackStates(1).WorkSteps(1000000)));
  LMatch := LRegex.First(LText + 'b');
  Check(LMatch.Success and (LMatch.Group(0).Span.Count = 50001) and
    (LMatch.Group(1).Span.Start = 49999), 'Long primitive repetitions use one compact alternative and preserve the final capture');
  LScope := TCountingScope.Create;
  LScopeLease := LScope;
  LScope.Limit := 2;
  LFailed := False;
  try
    LMatches := NewNyxRegex(NyxRegex('(a|aa)+b')).Find(StringOfChar('a', 1000), LScopeLease);
  except
    on E: ENyxRegexCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LScope.Reads = 3), 'Cancellation is checked while work is in flight');
  LScopeLease := nil;
  Invalid('(');
  Invalid('[z-a]');
  Invalid('a{3,2}');
  Invalid('\q');
  Invalid('\uD800');
  Invalid('\u{D800}\uDC00');
  Invalid('(?<=a+)b');
  Invalid('(?<x>a)(?<x>b)');
  Invalid('\k<missing>');
  Invalid('\2(a)');
  Invalid('[a&&b]');
  Invalid('(?j:a)');
  Invalid('a**');
end;

procedure Projection;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LFormDocument: TNyxDocument;
  LForm: INyxCard;
  LDraft: TNyxLogicEditorDraft;
  LCopy: TNyxLogicEditorDraft;
  LCapture: TNyxLogicEditorCapture;
  LAgent: TNyxAgentSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LExact: TNyxDocument;
  LChanges: array[0..2] of TNyxDataValue;
  LOperation: INyxLogicOperation;
  LOptions: TNyxRegexOptions;
  LArguments: TNyxDataValue;
  LRevision: Integer;
  LIndex: Integer;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}
begin
  LDocument := RegexDocument;
  LParsed := nil;
  LWorkspace := nil;
  LFormDocument := nil;
  LAgent := nil;
  LExact := nil;
  try
    LSource := TNyxCodegen.Generate(LDocument);
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument),
      'Complete crafted regex application source reconstructs its exact design');
    Check(LWorkspace.Render(LParsed) = LSource, 'Unchanged regex source stays exact');
    Check((Pos('NewNyxRegexReplace(', LSource) > 0) and
      (Pos('.Capture(NyxRegexGroup(', LSource) > 0) and
      (Pos('.CaseMode(nrcUnicode)', LSource) > 0), 'Generated regex code uses typed factories, flags and capture parts');
    LFormDocument := TNyxDocument.Create;
    LFormDocument.AddPage(NewNyxPage('editor-home'));
    LForm := NewNyxLogicEditor('editor', LDocument, NyxFlowRef('Names'));
    LFormDocument.Pages[0].Add(LForm);
    Check((LForm.Node.Find('editor-regex') <> nil) and
      (LForm.Node.Find('editor-regex-host').Prop('visible') = 'false'),
      'The authored regex compound is ready for realization and hidden until selected');
    LForm.Node.Find('editor-operation').Configure.Value('regex-replace').Done;
    HandleNyxLogicEditor(LForm.Node.Find('editor-operation'), LFormDocument.Pages[0]);
    LForm.Node.Find('editor-step-name').Configure.Value('extra-replace').Done;
    Check(CaptureNyxLogicEditor(LForm.Node.Find('editor-add'), LFormDocument.Pages[0], LCapture),
      'Public compound captures a typed regex operation');
    LForm.Node.Find('editor-regex-pattern').Configure.Value('(?<unfinished').Done;
    LForm.Node.Find('editor-regex-work').Configure.Value('unfinished count').Done;
    LDraft.Capture('editor', LFormDocument.Pages[0]);
    Check(LDraft.ToData.Field('version').AsInteger = 11, 'Regex fields own an explicit draft wrapper');
    LCopy := TNyxLogicEditorDraft.FromData(LDraft.ToData);
    Check(LCopy.ToData.ToJSON = LDraft.ToData.ToJSON, 'Unfinished regex draft and legacy base stay byte-exact');
    LForm.Node.Find('editor-regex-pattern').Configure.Value('changed').Done;
    Check(LCopy.Restore('editor', LFormDocument.Pages[0]) and
      (LForm.Node.Find('editor-regex-pattern').Prop('value') = '(?<unfinished') and
      (LForm.Node.Find('editor-regex-work').Prop('value') = 'unfinished count'),
      'Regex restoration preserves malformed text without compiling it');
    LExact := RegexDocument(nrcExact);
    LBefore := NyxProjectPair(TNyxCodec.Encode(LExact), TNyxCodegen.Generate(LExact));
    LAgent := TNyxAgentSession.Create(LBefore);
    LRevision := LAgent.Revision;
    LOptions := RegexOptions;
    for LIndex := 0 to 2 do
    begin
      case LIndex of
        0: LOperation := NewNyxRegexMatch(LOptions);
        1: LOperation := NewNyxRegexReplace(LOptions, RegexNameReplacement, nruError);
        2: LOperation := NewNyxRegexExtract(LOptions,
          NyxRegexCapture(NyxRegexGroup('last')), nrxSkip);
      end;
      LSource := 'match';

      if LIndex = 1 then
      begin
        LSource := 'replace';
      end
      else if LIndex = 2 then
      begin
        LSource := 'extract';
      end;
      LChanges[LIndex] := NyxObject([NyxField('op', NyxData('flow-step-set')),
        NyxField('flow', NyxData('Names')), NyxField('step', NyxData(LSource)),
        NyxField('kind', NyxData(LOperation.Descriptor.Kind.Name)),
        NyxField('configuration', LOperation.Configuration)]);
    end;
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('regex-recipes')), NyxField('operations', NyxArray(LChanges))]);
    LAgent.Call('nyx_transaction', 'Regex qualification', LArguments);
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    Check((LAgent.Revision = LRevision + 1) and (LAfter.Design = TNyxCodec.Encode(LDocument)),
      'Grouped revision-pinned semantic changes create the exact practical regex application');
    LAgent.Call('nyx_transaction', 'Regex qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'Semantic regex retry is idempotent');
    LAgent.Call('nyx_history', 'Regex qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('regex-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LBefore),
      'Semantic regex Undo restores the exact original source/design');
    LAgent.Call('nyx_history', 'Regex qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('regex-redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter),
      'Semantic regex Redo restores the exact accepted source/design');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);
    LSource := LAfter.Source;
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LAfter.Design, 'Exact semantic-authored source reconstructs its entire accepted model');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
        'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LSource[1], Length(LSource));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LAgent.Free;
    LExact.Free;
    LFormDocument.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

procedure Operations;
const
  CPeople: TNyxText = 'Ada Lovelace; Grace Hopper';
var
  LOptions: TNyxRegexOptions;
  LTemplate: TNyxRegexReplacement;
  LCopy: TNyxRegexReplacement;
  LReplace: INyxRegexReplace;
  LExtract: INyxRegexExtract;
  LReceipt: TNyxRegexReplacementResult;
  LRows: TNyxDataValue;
  LOperation: INyxLogicOperation;
  LGraph: INyxFlowGraph;
  LRestored: INyxFlowGraph;
  LResult: INyxFlowResult;
  LFailed: Boolean;
  LIndex: Integer;
  LScope: TCountingScope;
  LScopeLease: INyxCancellationScope;
begin
  LOptions := NyxRegex('(?<first>\p{L}+)\s+(?<last>\p{L}+)');
  LTemplate := NyxRegexReplacement.Capture(NyxRegexGroup('last')).Literal(', ')
    .Capture(NyxRegexGroup('first'));
  LReplace := NewNyxRegexReplace(LOptions, LTemplate);
  LReceipt := LReplace.Apply(CPeople);
  Check((LReceipt.Text = 'Lovelace, Ada; Hopper, Grace') and (LReceipt.Count = 2),
    'Structural named replacement retains untouched separators and order');
  LCopy := LTemplate.Literal(' $1 \1');
  Check((LTemplate.Count = 3) and (LCopy.Count = 4), 'Replacement vectors have independent ownership');
  Check(NewNyxRegexReplace(LOptions, LCopy).Replace('Ada Lovelace') = 'Lovelace, Ada $1 \1',
    'Dollar/backslash text has no implicit replacement behavior');
  Check(TNyxRegexReplacement.FromData(LCopy.ToData).ToData.ToJSON = LCopy.ToData.ToJSON,
    'Typed replacement parts round trip exactly');
  Check(NewNyxRegexReplace(NyxRegex('x'), NyxRegexReplacement).Replace('axb') = 'ab',
    'An initialized empty template deliberately deletes matched spans');
  Check(LReplace.Replace('NoMatch') = 'NoMatch', 'No-match replacement retains exact input');
  Check(NewNyxRegexReplace(NyxRegex(''), NyxRegexReplacement.Literal('|')).Replace('😀x') =
    TNyxText('|😀|x|'), 'Empty matches retain untouched supplementary scalars');
  Check(NewNyxRegexReplace(NyxRegex('(?<a>a)?b'), NyxRegexReplacement.Literal('[')
    .Capture(NyxRegexGroup('a')).Literal(']')).Replace('b') = '[]',
    'Explicit default missing participation contributes empty text');
  LFailed := False;
  try
    NewNyxRegexReplace(NyxRegex('(?<a>a)?b'), NyxRegexReplacement.Capture(NyxRegexGroup('a')),
      nruError).Replace('b');
  except
    on E: ENyxRegex do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Missing replacement participation can refuse the whole result');
  LFailed := False;
  try
    LReplace := NewNyxRegexReplace(NyxRegex('a'), NyxRegexReplacement.Capture(NyxRegexGroup('missing')));
  except
    on E: ENyxRegex do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Replacement selector admission is independent of matches');
  LExtract := NewNyxRegexExtract(LOptions, NyxRegexCapture(NyxRegexGroup('last')));
  LRows := LExtract.Extract(CPeople);
  Check((LRows.Count = 2) and (LRows.Item(0).Field('value').AsText = 'Lovelace') and
    (LRows.Item(1).Field('start').AsInteger = 20) and (LRows.Item(1).Field('id').AsText = '1'),
    'Extraction produces usable exact text rows with scalar spans and independent ids');
  LRows := NewNyxRegexExtract(NyxRegex('(?<a>a)?b'), NyxRegexCapture(NyxRegexGroup('a'))).Extract('ab b');
  Check((LRows.Count = 2) and not LRows.Item(1).Field('matched').AsBoolean and
    (LRows.Item(1).Field('value').Kind = ndNull) and (LRows.Item(1).Field('start').Kind = ndNull),
    'Extraction preserves unmatched participation as explicit null data');
  LRows := NewNyxRegexExtract(NyxRegex('(?<a>a)?b'),
    NyxRegexCapture(NyxRegexGroup('a')), nrxSkip).Extract('ab b');
  Check((LRows.Count = 1) and (LRows.Item(0).Field('value').AsText = 'a'), 'Explicit extraction skip omits missing groups');
  for LIndex := 0 to 2 do
  begin
    case LIndex of
      0: LOperation := NewNyxRegexMatch(LOptions);
      1: LOperation := NewNyxRegexReplace(LOptions, LTemplate, nruError);
      2: LOperation := LExtract;
    end;
    Check(NyxLogicFromData(LOperation.Descriptor.Kind, LOperation.Configuration)
      .Descriptor.ToData.ToJSON = LOperation.Descriptor.ToData.ToJSON, 'Closed factory retains each regex signature');
  end;
  LGraph := NewNyxFlowGraph.WithStep(NyxFlowStep('replace'), NewNyxRegexReplace(LOptions, LTemplate))
    .WithStep(NyxFlowStep('extract'), LExtract).Build;
  LRestored := NyxFlowGraphFromData(LGraph.ToData);
  Check(LRestored.ToData.ToJSON = LGraph.ToData.ToJSON, 'Saved regex graph reconstruction is exact');
  LResult := LRestored.Evaluate([
    NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('replace'), NyxRegexPort(nrpInput)), NyxFlowText(CPeople)),
    NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('extract'), NyxRegexPort(nrpInput)), NyxFlowText(CPeople))]);
  Check((LResult.Value(NyxFlowEndpoint(NyxFlowStep('replace'), NyxRegexPort(nrpValue))).AsText =
    'Lovelace, Ada; Hopper, Grace') and
    (LResult.Value(NyxFlowEndpoint(NyxFlowStep('extract'), NyxRegexPort(nrpValue))).AsJSON.Count = 2),
    'Ordinary saved graphs execute complete replacement and extraction outputs');
  LScope := TCountingScope.Create;
  LScopeLease := LScope;
  LScope.Limit := 2;
  LFailed := False;
  try
    NewNyxRegexMatch(NyxRegex('(a|aa)+b')).Invoke(
      NyxFlowValues.WithValue(NyxRegexPort(nrpInput), NyxFlowText(StringOfChar('a', 1000))), LScopeLease);
  except
    on E: ENyxFlowCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Logic translates matcher cancellation into the shared graph cancellation category');
  LScopeLease := nil;
end;

begin
  try
    UnicodeDatabase;
    Core;
    Operations;
    Projection;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-regex', 'passed');
    document.body.setAttribute('data-regex-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable regex/Unicode checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-regex', 'failed');
      document.body.setAttribute('data-regex-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
