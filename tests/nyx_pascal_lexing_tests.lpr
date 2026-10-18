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
program nyx_pascal_lexing_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.lexing;

type
  { Scope cancels on a controlled poll, proving checks occur inside a long single
    token. It owns only counters; the scanner borrows it for construction. }
  TPolledCancellation = class(TInterfacedObject, INyxCancellationScope)
  private
    FCancelled: Boolean;
  public
    Polls: Integer;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;
  GCorpusFiles: Integer;
  GCorpusTokens: TNyxTextBytes;

procedure TPolledCancellation.Cancel;
begin
  FCancelled := True;
end;

function TPolledCancellation.GetCancelled: Boolean;
begin
  Inc(Polls);
  Result := FCancelled or (Polls >= 5);
end;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure Coverage(const ASnapshot: INyxPascalLexicalSnapshot);
var
  LParts: TNyxStrings;
  LAt: Integer;
  LEnd: Integer;
  LToken: TNyxPascalToken;
begin
  LParts := TNyxStrings.Create;
  try
    LEnd := 0;
    for LAt := 0 to ASnapshot.Count - 1 do
    begin
      LToken := ASnapshot.Token(LAt);

      if (LToken.Span.Start <> LEnd) or
        (NyxTextScalarCount(ASnapshot.Text(LAt)) <> LToken.Span.Length) then
      begin
        raise Exception.Create('Lossless Pascal token coverage is not contiguous');
      end;

      if LToken.Span.Length > 0 then
      begin

        if (ASnapshot.TokenAt(LToken.Span.Start) <> LAt) or
          (ASnapshot.TokenAt(LToken.Span.Finish - 1) <> LAt) then
        begin
          raise Exception.Create('Scalar lookup escaped its exact token');
        end;
      end;
      LEnd := LToken.Span.Finish;
      LParts.Add(ASnapshot.Text(LAt));
    end;
    Check((LEnd = ASnapshot.Scalars) and
      (ASnapshot.Token(ASnapshot.Count - 1).Kind = ptkEOF) and
      (ASnapshot.TokenAt(LEnd) = ASnapshot.Count - 1) and
      (LParts.Join = ASnapshot.Source), 'Every source byte/scalar remains owned exactly once');
  finally
    LParts.Free;
  end;
end;

function FindText(const ASnapshot: INyxPascalLexicalSnapshot; const AText: TNyxText): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to ASnapshot.Count - 1 do
  begin

    if ASnapshot.Text(LAt) = AText then
    begin
      Exit(LAt);
    end;
  end;
  raise Exception.Create('Expected physical token is absent: ' + AText);
end;

procedure Core;
var
  LLex: INyxPascalLexicalSnapshot;
  LBefore: INyxPascalLexicalSnapshot;
  LOptions: TNyxPascalLexicalOptions;
  LToken: TNyxPascalToken;
  LAt: Integer;
  LRejected: Boolean;
  LScope: INyxCancellationScope;
  LPoller: TPolledCancellation;
  LText: TNyxText;
  {$ifndef PAS2JS}
  LBad: TNyxText;
  {$endif}
begin
  LLex := NewNyxPascalLexicalSnapshot('');
  Coverage(LLex);
  Check((LLex.Count = 1) and (LLex.Token(0).Line = 1) and
    (LLex.Token(0).Column = 1), 'Empty source retains one positioned EOF');
  LText := TNyxText('Unit Example;') + #13#10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'type TWidget = class(TObject)' + #13 +
    'private FValue: Integer;' + #10 +
    'public property &type: Integer read FValue; end;' + #13#10 +
    'var &class: TWidget; begin &class.&type += $FF; end.';
  LLex := NewNyxPascalLexicalSnapshot(LText);
  Coverage(LLex);
  Check(LLex.DiagnosticCount = 0, 'Ordinary Delphi spellings have no lexical diagnostics');
  Check(LLex.Token(0).Word = pkwUnit, 'ASCII keyword matching is case insensitive');
  LAt := FindText(LLex, '&class');
  Check((LLex.Token(LAt).Kind = ptkWord) and LLex.Token(LAt).Escaped and
    (LLex.Token(LAt).Word = pkwNone) and (LLex.Identifier(LAt) = TNyxText('class')),
    'An escaped reserved spelling remains a normal authored identifier');
  LAt := FindText(LLex, '{$mode delphi}');
  Check(LLex.Token(LAt).Kind = ptkDirective, 'Directives remain exact distinct trivia');
  LAt := FindText(LLex, '+=');
  Check(LLex.Token(LAt).Symbol = psoAddAssign, 'Compound operators are longest tokens');
  LAt := FindText(LLex, '$FF');
  Check(LLex.Token(LAt).NumberBase = pnbHexadecimal, 'Numeric base is typed without value coercion');

  LLex := NewNyxPascalLexicalSnapshot('1..9 1.25 2e-3 3E+ $12AF &17 %101 #13 #$1F680');
  Coverage(LLex);
  Check(LLex.Token(1).Symbol = psoRange, 'A range is not consumed as a decimal fraction');
  Check(LLex.Token(FindText(LLex, '1.25')).RealNumber and
    LLex.Token(FindText(LLex, '2e-3')).RealNumber, 'Fraction/exponent spelling is retained');
  Check((LLex.DiagnosticCount = 1) and
    (LLex.Diagnostic(0).Issue = pliMissingExponent) and
    not LLex.Token(FindText(LLex, '3E+')).Complete, 'An unfinished exponent remains one diagnosed token');
  Check(LLex.Token(FindText(LLex, '&17')).NumberBase = pnbOctal,
    'Octal spelling is distinct from an escaped identifier');
  Check(LLex.Token(FindText(LLex, '%101')).NumberBase = pnbBinary, 'Binary spelling remains typed');
  Check((LLex.Token(FindText(LLex, '#$1F680')).Kind = ptkCharacter) and
    (LLex.Token(FindText(LLex, '#$1F680')).NumberBase = pnbHexadecimal),
    'Character-code spelling is not decoded through an ANSI character');
  LLex := NewNyxPascalLexicalSnapshot('$ %102 # 7e-');
  Coverage(LLex);
  Check(LLex.DiagnosticCount = 4, 'Missing and out-of-base digits retain bounded diagnostics');

  LLex := NewNyxPascalLexicalSnapshot('a << b >> c ** d <> e >< f <= g >= h := i -= j *= k /= l (.0.) @x^;');
  Coverage(LLex);
  Check((LLex.Token(FindText(LLex, '<<')).Symbol = psoShiftLeft) and
    (LLex.Token(FindText(LLex, '><')).Symbol = psoSymmetricDifference) and
    (LLex.Token(FindText(LLex, '(.')).Symbol = psoAlternateOpenBracket) and
    (LLex.Token(FindText(LLex, '.)')).Symbol = psoAlternateCloseBracket),
    'Alternate brackets and multi-character symbols remain distinguishable');

  LText := TNyxText('''A 🚀') + #0 + TNyxText(' B''''s''') + #13#10 +
    'Next' + #13 + 'After' + #10 + 'Last';
  LLex := NewNyxPascalLexicalSnapshot(LText);
  Coverage(LLex);
  Check((LLex.DiagnosticCount = 0) and (LLex.Token(0).Kind = ptkString) and
    (LLex.Text(0) = Copy(LText, 1, Pos(#13, LText) - 1)),
    'Quotes, supplementary Unicode and NUL remain exact inside a literal');
  LToken := LLex.Token(FindText(LLex, 'Next'));
  Check((LToken.Line = 2) and (LToken.Column = 1), 'CRLF is one physical line break');
  Check(LLex.Token(FindText(LLex, 'After')).Line = 3, 'Lone CR is a line break');
  Check(LLex.Token(FindText(LLex, 'Last')).Line = 4, 'Lone LF is a line break');
  LAt := NyxTextScalarCount(TNyxText('''A '));
  Check(LLex.Slice(NyxPascalSpan(LAt, 1)) = TNyxText('🚀'),
    'Scalar slices cannot split a supplementary character');

  LLex := NewNyxPascalLexicalSnapshot(TNyxText('''unfinished') + #10 + 'var Recovered: Integer;');
  Coverage(LLex);
  Check((LLex.DiagnosticCount = 1) and
    (LLex.Diagnostic(0).Issue = pliUnterminatedString) and
    (LLex.Token(FindText(LLex, 'var')).Word = pkwVar),
    'Unfinished strings recover at a physical newline and retain later declarations');
  LLex := NewNyxPascalLexicalSnapshot('var X: Integer; (* unfinished');
  Coverage(LLex);
  Check((LLex.DiagnosticCount = 1) and
    (LLex.Diagnostic(0).Issue = pliUnterminatedComment) and
    not LLex.Token(FindText(LLex, '(* unfinished')).Complete,
    'Unfinished comments retain their entire opaque tail');
  LOptions := TNyxPascalLexicalOptions.Defaults;
  LLex := NewNyxPascalLexicalSnapshot('{ outer { inner } tail }', LOptions.NestedComments(True));
  Coverage(LLex);
  Check((LLex.Count = 2) and (LLex.DiagnosticCount = 0),
    'Explicit nested-comment policy retains the complete block');
  LLex := NewNyxPascalLexicalSnapshot('{ outer (* inner *) tail }', LOptions.NestedComments(True));
  Coverage(LLex);
  Check(LLex.Count = 2, 'Mixed nested delimiters match their own closing kind');
  LLex := NewNyxPascalLexicalSnapshot('{ outer { inner } tail }', LOptions);
  Check(LLex.Text(0) = TNyxText('{ outer { inner }'),
    'Default Delphi termination remains independent of a fluent option copy');

  LText := NyxScalarText($10400) + 'Name' + NyxScalarText($301);
  LLex := NewNyxPascalLexicalSnapshot(LText, LOptions.UnicodeIdentifiers(True));
  Coverage(LLex);
  Check((LLex.Count = 2) and (LLex.DiagnosticCount = 0) and
    (LLex.Identifier(0) = LText), 'Explicit Unicode analysis identifiers use pinned categories without rewriting text');
  LLex := NewNyxPascalLexicalSnapshot(LText);
  Coverage(LLex);
  Check(LLex.DiagnosticCount = 2, 'Default ASCII syntax reports unknown scalars without discarding them');
  LLex := NewNyxPascalLexicalSnapshot(NyxScalarText($FEFF) + 'unit Demo;');
  Coverage(LLex);
  Check((LLex.Token(0).Kind = ptkWhitespace) and (LLex.Token(1).Column = 2),
    'A leading BOM is retained physical trivia with scalar coordinates');

  LBefore := LLex;
  LRejected := False;
  try
    NewNyxPascalLexicalSnapshot('A B', LOptions.UpToTokens(2));
  except
    on ENyxPascalLexicalLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LBefore = LLex), 'Token quota refuses rather than publishing an incomplete stream');
  LRejected := False;
  try
    NewNyxPascalLexicalSnapshot(TNyxText('🚀'), LOptions.UpToSourceBytes(3));
  except
    on ENyxPascalLexicalLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Exact UTF-8 byte quota is target independent');
  LRejected := False;
  try
    NewNyxPascalLexicalSnapshot('???', LOptions.UpToDiagnostics(2));
  except
    on ENyxPascalLexicalLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Diagnostic quota is a hard failure rather than silent loss');
  LRejected := False;
  try
    NewNyxPascalLexicalSnapshot('{{{}}}', LOptions.NestedComments(True).UpToCommentDepth(2));
  except
    on ENyxPascalLexicalLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Nested comments observe the caller depth bound');
  LScope := NewNyxCancellationScope;
  LScope.Cancel;
  LRejected := False;
  try
    NewNyxPascalLexicalSnapshot('unit Demo;', LOptions, LScope);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Pre-cancelled analysis reads no source');
  LPoller := TPolledCancellation.Create;
  LScope := LPoller;
  LRejected := False;
  try
    NewNyxPascalLexicalSnapshot('{' + TNyxText(StringOfChar('x', 32768)) + '}', LOptions, LScope);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPoller.Polls = 5), 'Long single tokens cooperate with cancellation at bounded intervals');
  LScope := nil;
  LRejected := False;
  try
    LLex.TokenAt(LLex.Scalars + 1);
  except
    on ENyxPascalLexical do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Scalar lookup refuses out-of-source positions');
  {$ifndef PAS2JS}
  SetLength(LBad, 1);
  LBad[1] := AnsiChar($C0);
  LRejected := False;
  try
    NewNyxPascalLexicalSnapshot(LBad);
  except
    on ENyxPascalLexical do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Malformed UTF-8 refuses the complete candidate');
  {$endif}
end;

{$ifndef PAS2JS}
procedure Corpus;
const
  CPatterns: array[0..1] of String = ('*.pas', '*.inc');
var
  LSearch: TSearchRec;
  LPattern: String;
  LPath: String;
  LStream: TFileStream;
  LText: TNyxText;
  LLex: INyxPascalLexicalSnapshot;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  for LPattern in CPatterns do
  begin

    if FindFirst(IncludeTrailingPathDelimiter(ParamStr(1)) + 'src/' + LPattern, faAnyFile, LSearch) = 0 then
    begin
      try
        repeat
          LPath := IncludeTrailingPathDelimiter(ParamStr(1)) + 'src/' + LSearch.Name;
          LStream := TFileStream.Create(LPath, fmOpenRead or fmShareDenyWrite);
          try
            SetLength(LText, LStream.Size);

            if Length(LText) > 0 then
            begin
              LStream.ReadBuffer(LText[1], Length(LText));
            end;
          finally
            LStream.Free;
          end;
          LLex := NewNyxPascalLexicalSnapshot(LText);
          Coverage(LLex);

          if LLex.DiagnosticCount <> 0 then
          begin
            raise Exception.Create('Unexpected lexical corpus diagnostic in ' + LSearch.Name +
              ': ' + LLex.Diagnostic(0).MessageText);
          end;
          Inc(GCorpusFiles);
          Inc(GCorpusTokens, LLex.Count);
        until FindNext(LSearch) <> 0;
      finally
        FindClose(LSearch);
      end;
    end;
  end;
  Check(GCorpusFiles > 0, 'Actual owned Pascal corpus was inspected');
end;
{$endif}

begin
  try
    Core;
    {$ifndef PAS2JS}Corpus;{$endif}
    WriteLn('PASS ', GChecks, ' owned lexical checks; ', GCorpusFiles,
      ' actual source files / ', GCorpusTokens, ' lossless tokens');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
