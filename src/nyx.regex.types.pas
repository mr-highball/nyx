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


unit nyx.regex.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data;

type
  { Portable pattern/limit failure. UTF-8 diagnostics retain exact application
    text without system ANSI conversion; no native matcher exception leaks. }
  ENyxRegex = class(Exception)
  public
    constructor Create(const AMessage: TNyxText); reintroduce;
  end;
  { Explicit matching-work exhaustion, distinct from an ordinary no-match. A
    failed run never returns a successful prefix or a partially replaced value. }
  ENyxRegexLimit = class(ENyxRegex);
  { Cooperative scope cancellation. Logic adapters translate this explicit
    outcome into their scheduler's cancellation category, never ordinary absence. }
  ENyxRegexCancelled = class(ENyxRegex);
  { Exact comparison, ASCII A-Z folding, or pinned Unicode default simple
    folding. Simple folding never expands one character into several. }
  TNyxRegexCase = (nrcExact, nrcASCII, nrcUnicode);
  { Leftmost first match or leftmost nonoverlapping matches. Empty matches
    advance by one Unicode scalar, including a final empty match at EOF. }
  TNyxRegexScan = (nrsFirst, nrsAll);
  { Shorthand digit/word/space classes use pinned Unicode properties or the
    explicitly requested ASCII sets. Property escapes remain Unicode in either. }
  TNyxRegexClasses = (nrgUnicode, nrgASCII);

  { Independent positive resource limits. Work is charged across the COMPLETE
    search; backtrack states bounds the live alternative stack. ResultBytes is
    a conservative escaped-text/metadata allocation budget. Nesting has a
    physical 256-level ceiling; all other limits fit a positive signed Integer.
    Factory defaults are explicit; default/uninitialized records refuse. }
  TNyxRegexLimits = record
  private
    FDefined: Boolean;
    FPattern: Integer;
    FInput: Integer;
    FInstructions: Integer;
    FWork: Integer;
    FStates: Integer;
    FMatches: Integer;
    FResult: Integer;
    FGroups: Integer;
    FNesting: Integer;
  public
    function PatternScalars(AValue: Integer): TNyxRegexLimits;
    function InputScalars(AValue: Integer): TNyxRegexLimits;
    function Instructions(AValue: Integer): TNyxRegexLimits;
    function WorkSteps(AValue: Integer): TNyxRegexLimits;
    function BacktrackStates(AValue: Integer): TNyxRegexLimits;
    function Matches(AValue: Integer): TNyxRegexLimits;
    function ResultBytes(AValue: Integer): TNyxRegexLimits;
    function Groups(AValue: Integer): TNyxRegexLimits;
    function Nesting(AValue: Integer): TNyxRegexLimits;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxRegexLimits; static;
    property MaximumPatternScalars: Integer read FPattern;
    property MaximumInputScalars: Integer read FInput;
    property MaximumInstructions: Integer read FInstructions;
    property MaximumWorkSteps: Integer read FWork;
    property MaximumBacktrackStates: Integer read FStates;
    property MaximumMatches: Integer read FMatches;
    property MaximumResultBytes: Integer read FResult;
    property MaximumGroups: Integer read FGroups;
    property MaximumNesting: Integer read FNesting;
  end;

  { Immutable-by-value fluent configuration. Pattern text is an explicit regex
    language boundary; flags/extent/budgets are strongly typed. Compilation
    validates syntax separately. No host, locale, scheduler or document belongs
    to these options. DotAll and Multiline are independent, defaulting false. }
  TNyxRegexOptions = record
  private
    FDefined: Boolean;
    FPattern: TNyxText;
    FCase: TNyxRegexCase;
    FScan: TNyxRegexScan;
    FMultiline: Boolean;
    FDotAll: Boolean;
    FExpanded: Boolean;
    FClasses: TNyxRegexClasses;
    FLimits: TNyxRegexLimits;
  public
    function CaseMode(AValue: TNyxRegexCase): TNyxRegexOptions;
    function Scan(AValue: TNyxRegexScan): TNyxRegexOptions;
    function Multiline(AValue: Boolean = True): TNyxRegexOptions;
    function DotAll(AValue: Boolean = True): TNyxRegexOptions;
    function Expanded(AValue: Boolean = True): TNyxRegexOptions;
    function CharacterClasses(AValue: TNyxRegexClasses): TNyxRegexOptions;
    function Limits(const AValue: TNyxRegexLimits): TNyxRegexOptions;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxRegexOptions; static;
    property Pattern: TNyxText read FPattern;
    property Comparison: TNyxRegexCase read FCase;
    property Extent: TNyxRegexScan read FScan;
    property IsMultiline: Boolean read FMultiline;
    property IsDotAll: Boolean read FDotAll;
    property IsExpanded: Boolean read FExpanded;
    property Classes: TNyxRegexClasses read FClasses;
    property Budget: TNyxRegexLimits read FLimits;
  end;

  { Distinct exact capture-name reference. Regex group names use Unicode letters
    or underscore initially, then letters/marks/decimal numbers/underscore.
    Matching remains ordinal: no case folding or normalization of names. }
  TNyxRegexGroupRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;

  { Explicit numeric or named capture selector. Index zero denotes the complete
    match. A default record is not a selector; resolution requires the compiled
    pattern and rejects unknown groups even when input has no matches. }
  TNyxRegexCaptureRef = record
  private
    FDefined: Boolean;
    FNamed: Boolean;
    FName: TNyxText;
    FIndex: Integer;
  public
    property Defined: Boolean read FDefined;
    property Named: Boolean read FNamed;
    property Name: TNyxText read FName;
    property Index: Integer read FIndex;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxRegexCaptureRef; static;
  end;

  { Zero-based Unicode-scalar start/length. Unmatched/default is distinct from
    an empty successful capture. Storage byte/UTF-16 offsets never escape here. }
  TNyxRegexSpan = record
  private
    FDefined: Boolean;
    FStart: Integer;
    FLength: Integer;
  public
    property Defined: Boolean read FDefined;
    property Start: Integer read FStart;
    property Count: Integer read FLength;
  end;

  { Detached capture result; name is empty for unnamed/index-zero captures.
    Unmatched groups retain an undefined span and empty value, without inventing
    an empty successful match. No source/input buffer owner is retained. }
  TNyxRegexCapture = record
    Span: TNyxRegexSpan;
    Name: TNyxText;
    Value: TNyxText;
  end;

  { Complete successful match and immutable capture vector. Group zero is the
    full match. Returned records copy their immutable text; callers cannot mutate
    the hidden vector. Failed/default matches expose Success=false, no groups. }
  TNyxRegexMatch = record
  private
    FGroups: array of TNyxRegexCapture;
    function GetCount: Integer;
    function GetSuccess: Boolean;
  public
    function Group(AIndex: Integer): TNyxRegexCapture; overload;
    function Group(const AName: TNyxRegexGroupRef): TNyxRegexCapture; overload;
    function ToData: TNyxDataValue;
    property Success: Boolean read GetSuccess;
    property Count: Integer read GetCount;
  end;

  { Independent complete match collection. It has no cursor/global lastIndex,
    native engine, callbacks or mutable references into a later search. }
  TNyxRegexMatches = record
  private
    FItems: array of TNyxRegexMatch;
    function GetCount: Integer;
  public
    function Item(AIndex: Integer): TNyxRegexMatch;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

function NyxRegexLimits: TNyxRegexLimits;
function NyxRegex(const APattern: TNyxText): TNyxRegexOptions; overload;
{ Supply an admitted budget before pattern validation when intentionally
  constructing a pattern larger than the default. }
function NyxRegex(const APattern: TNyxText;
  const ALimits: TNyxRegexLimits): TNyxRegexOptions; overload;
function NyxRegexGroup(const AName: TNyxText): TNyxRegexGroupRef;
function NyxRegexCapture(AIndex: Integer): TNyxRegexCaptureRef; overload;
function NyxRegexCapture(const AName: TNyxRegexGroupRef): TNyxRegexCaptureRef; overload;
{ Admitted immutable snapshots for alternative engine implementations. Indices
  are scalar counts; invalid negative/overflow coordinates refuse. }
function NyxRegexSpan(AStart, ACount: Integer): TNyxRegexSpan;
function NyxRegexMatch(const AGroups: array of TNyxRegexCapture): TNyxRegexMatch;
function NyxRegexMatches(const AItems: array of TNyxRegexMatch): TNyxRegexMatches;

implementation

uses nyx.flow.types, nyx.unicode, nyx.unicode.types;

constructor ENyxRegex.Create(const AMessage: TNyxText);
begin
  inherited Create('');
  {$ifdef PAS2JS}Message := AMessage;{$else}Message := RawByteString(AMessage);{$endif}
end;

function NyxRegexLimits: TNyxRegexLimits;
begin
  Result := Default(TNyxRegexLimits);
  Result.FDefined := True;
  Result.FPattern := 16384;
  Result.FInput := 1048576;
  Result.FInstructions := 65536;
  Result.FWork := 8000000;
  Result.FStates := 16384;
  Result.FMatches := 65536;
  Result.FResult := 16 * 1024 * 1024;
  Result.FGroups := 128;
  Result.FNesting := 64;
end;

procedure TNyxRegexLimits.Validate;
begin

  if not FDefined or (FPattern < 1) or (FInput < 1) or (FInstructions < 1) or
    (FWork < 1) or (FStates < 1) or (FMatches < 1) or (FResult < 1) or
    (FGroups < 1) or (FNesting < 1) or (FNesting > 256) then
  begin
    raise ENyxRegex.Create('Regex limits require positive counts and nesting at most 256');
  end;
end;

function TNyxRegexLimits.PatternScalars(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FPattern := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.InputScalars(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FInput := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.Instructions(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FInstructions := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.WorkSteps(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FWork := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.BacktrackStates(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FStates := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.Matches(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FMatches := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.ResultBytes(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FResult := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.Groups(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FGroups := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.Nesting(AValue: Integer): TNyxRegexLimits;
begin
  Result := Self;
  Result.FNesting := AValue;
  Result.Validate;
end;

function TNyxRegexLimits.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('pattern', NyxData(FPattern)), NyxField('input', NyxData(FInput)),
    NyxField('instructions', NyxData(FInstructions)), NyxField('work', NyxData(FWork)),
    NyxField('states', NyxData(FStates)), NyxField('matches', NyxData(FMatches)),
    NyxField('result', NyxData(FResult)), NyxField('groups', NyxData(FGroups)),
    NyxField('nesting', NyxData(FNesting))]);
end;

class function TNyxRegexLimits.FromData(const AData: TNyxDataValue): TNyxRegexLimits;
begin
  ValidateNyxFlowFields(AData, ['pattern', 'input', 'instructions', 'work',
    'states', 'matches', 'result', 'groups', 'nesting']);
  Result := NyxRegexLimits.PatternScalars(AData.Field('pattern').AsInteger)
    .InputScalars(AData.Field('input').AsInteger).Instructions(AData.Field('instructions').AsInteger)
    .WorkSteps(AData.Field('work').AsInteger).BacktrackStates(AData.Field('states').AsInteger)
    .Matches(AData.Field('matches').AsInteger).ResultBytes(AData.Field('result').AsInteger)
    .Groups(AData.Field('groups').AsInteger).Nesting(AData.Field('nesting').AsInteger);
end;

function NyxRegex(const APattern: TNyxText): TNyxRegexOptions;
begin
  Result := NyxRegex(APattern, NyxRegexLimits);
end;

function NyxRegex(const APattern: TNyxText;
  const ALimits: TNyxRegexLimits): TNyxRegexOptions;
begin
  Result := Default(TNyxRegexOptions);
  Result.FDefined := True;
  Result.FPattern := APattern;
  Result.FLimits := ALimits;
  Result.FScan := nrsAll;
  Result.Validate;
end;

procedure TNyxRegexOptions.Validate;
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
  LChoice: Integer;
begin
  FLimits.Validate;
  LChoice := Ord(FCase);

  if not FDefined or (LChoice < Ord(Low(TNyxRegexCase))) or
    (LChoice > Ord(High(TNyxRegexCase))) then
  begin
    raise ENyxRegex.Create('Regex requires initialized typed options');
  end;
  LChoice := Ord(FScan);

  if (LChoice < Ord(Low(TNyxRegexScan))) or (LChoice > Ord(High(TNyxRegexScan))) then
  begin
    raise ENyxRegex.Create('Unknown regex scan policy');
  end;
  LChoice := Ord(FClasses);

  if (LChoice < Ord(Low(TNyxRegexClasses))) or
    (LChoice > Ord(High(TNyxRegexClasses))) then
  begin
    raise ENyxRegex.Create('Unknown regex shorthand character policy');
  end;
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(FPattern) do
  begin

    if not NyxNextScalar(FPattern, LIndex, LScalar) then
    begin
      raise ENyxRegex.Create('Regex pattern contains malformed Unicode');
    end;

    if LCount >= FLimits.MaximumPatternScalars then
    begin
      raise ENyxRegexLimit.Create('Regex pattern exceeds its scalar budget');
    end;
    Inc(LCount);
  end;
end;

function TNyxRegexOptions.CaseMode(AValue: TNyxRegexCase): TNyxRegexOptions;
begin
  Result := Self;
  Result.FCase := AValue;
  Result.Validate;
end;

function TNyxRegexOptions.Scan(AValue: TNyxRegexScan): TNyxRegexOptions;
begin
  Result := Self;
  Result.FScan := AValue;
  Result.Validate;
end;

function TNyxRegexOptions.Multiline(AValue: Boolean): TNyxRegexOptions;
begin
  Result := Self;
  Result.FMultiline := AValue;
  Result.Validate;
end;

function TNyxRegexOptions.DotAll(AValue: Boolean): TNyxRegexOptions;
begin
  Result := Self;
  Result.FDotAll := AValue;
  Result.Validate;
end;

function TNyxRegexOptions.Expanded(AValue: Boolean): TNyxRegexOptions;
begin
  Result := Self;
  Result.FExpanded := AValue;
  Result.Validate;
end;

function TNyxRegexOptions.CharacterClasses(AValue: TNyxRegexClasses): TNyxRegexOptions;
begin
  Result := Self;
  Result.FClasses := AValue;
  Result.Validate;
end;

function TNyxRegexOptions.Limits(const AValue: TNyxRegexLimits): TNyxRegexOptions;
begin
  AValue.Validate;
  Result := Self;
  Result.FLimits := AValue;
  Result.Validate;
end;

function TNyxRegexOptions.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('pattern', NyxData(FPattern)), NyxField('case', NyxData(Ord(FCase))),
    NyxField('scan', NyxData(Ord(FScan))), NyxField('multiline', NyxData(FMultiline)),
    NyxField('dotAll', NyxData(FDotAll)), NyxField('expanded', NyxData(FExpanded)),
    NyxField('classes', NyxData(Ord(FClasses))), NyxField('limits', FLimits.ToData)]);
end;

class function TNyxRegexOptions.FromData(const AData: TNyxDataValue): TNyxRegexOptions;
var
  LCase: Integer;
  LScan: Integer;
  LClasses: Integer;
begin
  ValidateNyxFlowFields(AData, ['pattern', 'case', 'scan', 'multiline', 'dotAll',
    'expanded', 'classes', 'limits']);
  LCase := AData.Field('case').AsInteger;
  LScan := AData.Field('scan').AsInteger;
  LClasses := AData.Field('classes').AsInteger;

  if (LCase < Ord(Low(TNyxRegexCase))) or (LCase > Ord(High(TNyxRegexCase))) or
    (LScan < Ord(Low(TNyxRegexScan))) or (LScan > Ord(High(TNyxRegexScan))) or
    (LClasses < Ord(Low(TNyxRegexClasses))) or (LClasses > Ord(High(TNyxRegexClasses))) then
  begin
    raise ENyxRegex.Create('Unknown persisted regex choice');
  end;
  { Admit the persisted budget before the pattern, so a deliberate larger
    pattern does not accidentally pass through the factory's smaller defaults. }
  Result := Default(TNyxRegexOptions);
  Result.FDefined := True;
  Result.FLimits := TNyxRegexLimits.FromData(AData.Field('limits'));
  Result.FPattern := AData.Field('pattern').AsText;
  Result.FCase := TNyxRegexCase(LCase);
  Result.FScan := TNyxRegexScan(LScan);
  Result.FMultiline := AData.Field('multiline').AsBoolean;
  Result.FDotAll := AData.Field('dotAll').AsBoolean;
  Result.FExpanded := AData.Field('expanded').AsBoolean;
  Result.FClasses := TNyxRegexClasses(LClasses);
  Result.Validate;
end;

function NyxRegexGroup(const AName: TNyxText): TNyxRegexGroupRef;
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
  LCategory: TNyxUnicodeCategory;
begin
  Result := Default(TNyxRegexGroupRef);
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) then
    begin
      raise ENyxRegex.Create('Capture name contains malformed Unicode');
    end;
    LCategory := NyxScalarCategory(LScalar);

    if (LScalar <> Ord('_')) and
      not (LCategory in [nucUppercaseLetter, nucLowercaseLetter, nucTitlecaseLetter,
        nucModifierLetter, nucOtherLetter]) then
    begin

      if (LCount = 0) or not (LCategory in [nucNonspacingMark, nucSpacingMark,
        nucEnclosingMark, nucDecimalNumber]) then
      begin
        raise ENyxRegex.Create('Capture names require letters/underscore then letters/marks/decimal numbers');
      end;
    end;
    Inc(LCount);
  end;

  if (LCount < 1) or (LCount > 128) then
  begin
    raise ENyxRegex.Create('Capture names require one to 128 scalars');
  end;
  Result.FName := AName;
end;

function TNyxRegexGroupRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NyxRegexCapture(AIndex: Integer): TNyxRegexCaptureRef;
begin

  if AIndex < 0 then
  begin
    raise ENyxRegex.Create('Capture index must be nonnegative');
  end;
  Result := Default(TNyxRegexCaptureRef);
  Result.FDefined := True;
  Result.FIndex := AIndex;
end;

function NyxRegexCapture(const AName: TNyxRegexGroupRef): TNyxRegexCaptureRef;
begin
  Result := Default(TNyxRegexCaptureRef);
  Result.FName := NyxRegexGroup(AName.Name).Name;
  Result.FDefined := True;
  Result.FNamed := True;
end;

function TNyxRegexCaptureRef.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    raise ENyxRegex.Create('Capture selector is undefined');
  end;

  if FNamed then
  begin
    Result := NyxObject([NyxField('name', NyxData(FName))]);
  end
  else
  begin
    Result := NyxObject([NyxField('index', NyxData(FIndex))]);
  end;
end;

class function TNyxRegexCaptureRef.FromData(const AData: TNyxDataValue): TNyxRegexCaptureRef;
begin

  if (AData.Kind = ndObject) and (AData.Count = 1) and (AData.Key(0) = 'name') then
  begin
    Exit(NyxRegexCapture(NyxRegexGroup(AData.Field('name').AsText)));
  end;
  ValidateNyxFlowFields(AData, ['index']);
  Result := NyxRegexCapture(AData.Field('index').AsInteger);
end;

function NyxRegexSpan(AStart, ACount: Integer): TNyxRegexSpan;
begin

  if (AStart < 0) or (ACount < 0) or (AStart > High(Integer) - ACount) then
  begin
    raise ENyxRegex.Create('Invalid regex scalar span');
  end;
  Result := Default(TNyxRegexSpan);
  Result.FDefined := True;
  Result.FStart := AStart;
  Result.FLength := ACount;
end;

function NyxRegexMatch(const AGroups: array of TNyxRegexCapture): TNyxRegexMatch;
var
  LIndex: Integer;
begin
  Result := Default(TNyxRegexMatch);

  if (Length(AGroups) = 0) or not AGroups[0].Span.Defined then
  begin
    raise ENyxRegex.Create('A successful regex match requires group zero');
  end;
  SetLength(Result.FGroups, Length(AGroups));
  for LIndex := 0 to High(AGroups) do
  begin
    Result.FGroups[LIndex] := AGroups[LIndex];
  end;
end;

function TNyxRegexMatch.GetCount: Integer;
begin
  Result := Length(FGroups);
end;

function TNyxRegexMatch.GetSuccess: Boolean;
begin
  Result := Length(FGroups) > 0;
end;

function TNyxRegexMatch.Group(AIndex: Integer): TNyxRegexCapture;
begin

  if (AIndex < 0) or (AIndex >= Length(FGroups)) then
  begin
    raise ENyxRegex.Create('Capture group index is unavailable');
  end;
  Result := FGroups[AIndex];
end;

function TNyxRegexMatch.Group(const AName: TNyxRegexGroupRef): TNyxRegexCapture;
var
  LName: TNyxText;
  LIndex: Integer;
begin
  LName := NyxRegexGroup(AName.Name).Name;
  for LIndex := 1 to High(FGroups) do
  begin

    if FGroups[LIndex].Name = LName then
    begin
      Exit(FGroups[LIndex]);
    end;
  end;
  raise ENyxRegex.Create('Named capture is unavailable: ' + LName);
end;

function TNyxRegexMatch.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LSpan: TNyxDataValue;
  LIndex: Integer;
begin

  if not Success then
  begin
    Exit(NyxNull);
  end;
  SetLength(LItems, Length(FGroups));
  for LIndex := 0 to High(FGroups) do
  begin
    LSpan := NyxNull;

    if FGroups[LIndex].Span.Defined then
    begin
      LSpan := NyxObject([NyxField('start', NyxData(FGroups[LIndex].Span.Start)),
        NyxField('length', NyxData(FGroups[LIndex].Span.Count))]);
    end;
    LItems[LIndex] := NyxObject([NyxField('name', NyxData(FGroups[LIndex].Name)),
      NyxField('span', LSpan), NyxField('value', NyxData(FGroups[LIndex].Value))]);
  end;
  Result := NyxObject([NyxField('start', NyxData(FGroups[0].Span.Start)),
    NyxField('length', NyxData(FGroups[0].Span.Count)),
    NyxField('value', NyxData(FGroups[0].Value)), NyxField('groups', NyxArray(LItems))]);
end;

function NyxRegexMatches(const AItems: array of TNyxRegexMatch): TNyxRegexMatches;
var
  LIndex: Integer;
begin
  Result := Default(TNyxRegexMatches);
  SetLength(Result.FItems, Length(AItems));
  for LIndex := 0 to High(AItems) do
  begin

    if not AItems[LIndex].Success then
    begin
      raise ENyxRegex.Create('Match collections contain only complete successful matches');
    end;
    Result.FItems[LIndex] := AItems[LIndex];
  end;
end;

function TNyxRegexMatches.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TNyxRegexMatches.Item(AIndex: Integer): TNyxRegexMatch;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxRegex.Create('Regex match index is unavailable');
  end;
  Result := FItems[AIndex];
end;

function TNyxRegexMatches.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(FItems));
  for LIndex := 0 to High(FItems) do
  begin
    LItems[LIndex] := FItems[LIndex].ToData;
  end;
  Result := NyxArray(LItems);
end;

end.
