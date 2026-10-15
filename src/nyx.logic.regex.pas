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


unit nyx.logic.regex;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.logic, nyx.flow.types, nyx.scheduler,
  nyx.regex, nyx.regex.types, nyx.regex.replacement;

type
  { Closed regex result ports. Input text and output value have distinct names;
    value is structured matches/rows or replacement text according to the schema. }
  TNyxRegexPort = (nrpInput, nrpValue, nrpMatched, nrpCount);
  { An unmatched participating selector contributes empty text or refuses the
    whole replacement. Unknown selectors always refuse during construction. }
  TNyxRegexUnmatched = (nruEmpty, nruError);
  { Extraction retains unmatched groups as explicit null rows, omits them, or
    refuses. A successfully matched empty string is never a missing group. }
  TNyxRegexMissing = (nrxNull, nrxSkip, nrxError);
  { Detached complete replacement receipt. Count describes matched spans,
    including empty spans, rather than whether text happened to change. }
  TNyxRegexReplacementResult = record
    Text: TNyxText;
    Count: Integer;
  end;

  { Pure managed matching operation. Direct Find retains complete typed
    captures; graph invocation exposes their JSON projection plus matched/count.
    Matcher code is immutable; no operation retains its invocation state. }
  INyxRegexMatch = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000081}']
    function GetOptions: TNyxRegexOptions;
    function Find(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope = nil): TNyxRegexMatches;
    property Options: TNyxRegexOptions read GetOptions;
  end;

  { Pure managed replacement from structural literal/capture parts. Text such
    as $1 is literal unless a typed Capture part was declared. Options and
    replacement recipes are detached; failures return no partial replacement. }
  INyxRegexReplace = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000082}']
    function GetOptions: TNyxRegexOptions;
    function GetReplacement: TNyxRegexReplacement;
    function GetUnmatched: TNyxRegexUnmatched;
    function Apply(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope = nil): TNyxRegexReplacementResult;
    function Replace(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope = nil): TNyxText;
    property Options: TNyxRegexOptions read GetOptions;
    property Replacement: TNyxRegexReplacement read GetReplacement;
    property Unmatched: TNyxRegexUnmatched read GetUnmatched;
  end;

  { Pure managed extraction of one known capture into Table-friendly rows.
    Rows have exact text id, matched, value, scalar start/length and capture name/
    index. Missing participation follows the explicit policy; unknown selectors
    refuse even when input would contain no matches. }
  INyxRegexExtract = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000083}']
    function GetOptions: TNyxRegexOptions;
    function GetCapture: TNyxRegexCaptureRef;
    function GetMissing: TNyxRegexMissing;
    function Extract(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope = nil): TNyxDataValue;
    property Options: TNyxRegexOptions read GetOptions;
    property Capture: TNyxRegexCaptureRef read GetCapture;
    property Missing: TNyxRegexMissing read GetMissing;
  end;

function NyxRegexPort(APort: TNyxRegexPort): TNyxFlowPortRef;
function NewNyxRegexMatch(const AOptions: TNyxRegexOptions): INyxRegexMatch;
function NewNyxRegexReplace(const AOptions: TNyxRegexOptions;
  const AReplacement: TNyxRegexReplacement;
  AUnmatched: TNyxRegexUnmatched = nruEmpty): INyxRegexReplace;
function NewNyxRegexExtract(const AOptions: TNyxRegexOptions): INyxRegexExtract; overload;
function NewNyxRegexExtract(const AOptions: TNyxRegexOptions;
  const ACapture: TNyxRegexCaptureRef;
  AMissing: TNyxRegexMissing = nrxNull): INyxRegexExtract; overload;
{ Closed persistence dispatcher. It owns no host authority and returns nil for
  an unrelated kind. Known malformed schemas/configuration refuse completely. }
function NyxRegexLogicFromData(const AKind: TNyxLogicKindRef;
  const AData: TNyxDataValue): INyxLogicOperation;

implementation

uses SysUtils, Math, nyx.bytes;

type
  { Pure shared implementation owns compiled immutable code and portable
    configuration. Constructor admission happens before any graph publication. }
  TRegexOperation = class(TNyxLogicOperation)
  protected
    FRegex: INyxRegex;
    procedure Describe(const AOptions: TNyxRegexOptions;
      const AKind, ATitle, AHelp: TNyxText; AOutput: TNyxFlowType);
  public
    function GetOptions: TNyxRegexOptions;
  end;
  { Matching projects one complete typed result. No last-match field is retained
    across calls, so worker/concurrent invocations cannot overwrite each other. }
  TRegexMatch = class(TRegexOperation, INyxRegexMatch)
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const AOptions: TNyxRegexOptions);
    function Find(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope): TNyxRegexMatches;
  end;
  { Replacement owns only an independent immutable template and resolved group
    indices. Each Apply owns its output pieces and transformation work budget. }
  TRegexReplace = class(TRegexOperation, INyxRegexReplace)
  private
    FReplacement: TNyxRegexReplacement;
    FUnmatched: TNyxRegexUnmatched;
    FIndices: array of Integer;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const AOptions: TNyxRegexOptions;
      const AReplacement: TNyxRegexReplacement; AUnmatched: TNyxRegexUnmatched);
    function GetReplacement: TNyxRegexReplacement;
    function GetUnmatched: TNyxRegexUnmatched;
    function Apply(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope): TNyxRegexReplacementResult;
    function Replace(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope): TNyxText;
  end;
  { Extraction owns its exact selector and immutable code. Its helper stages all
    rows before returning; null, skip and failure never silently substitute text. }
  TRegexExtract = class(TRegexOperation, INyxRegexExtract)
  private
    FCapture: TNyxRegexCaptureRef;
    FIndex: Integer;
    FMissing: TNyxRegexMissing;
    function Rows(const AInput: TNyxText; const ACancellation: INyxCancellationScope;
      out AMatches: Integer): TNyxDataValue;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const AOptions: TNyxRegexOptions;
      const ACapture: TNyxRegexCaptureRef; AMissing: TNyxRegexMissing);
    function GetCapture: TNyxRegexCaptureRef;
    function GetMissing: TNyxRegexMissing;
    function Extract(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope): TNyxDataValue;
  end;

{ Matching and transformation each have a bounded complete phase. Projection/
  formatting checks cancellation between chunks and charges before retention.
  Failed admission releases all candidates without exposing a successful prefix. }
procedure TransformStep(const AScope: INyxCancellationScope; var ASteps: Integer;
  const ALimits: TNyxRegexLimits);
begin

  if (ASteps and 255) = 0 then
  begin

    if (AScope <> nil) and AScope.Cancelled then
    begin
      raise ENyxRegexCancelled.Create('Regex transformation cancelled');
    end;
  end;

  if ASteps >= ALimits.MaximumWorkSteps then
  begin
    raise ENyxRegexLimit.Create('Regex transformation work budget exceeded');
  end;
  Inc(ASteps);
end;

function NyxRegexPort(APort: TNyxRegexPort): TNyxFlowPortRef;
const
  CNames: array[TNyxRegexPort] of TNyxText = ('text', 'value', 'matched', 'count');
var
  LIndex: Integer;
begin
  LIndex := Ord(APort);

  if (LIndex < Ord(Low(TNyxRegexPort))) or (LIndex > Ord(High(TNyxRegexPort))) then
  begin
    raise ENyxRegex.Create('Unknown regex port');
  end;
  Result := NyxFlowPort(CNames[APort]);
end;

procedure TRegexOperation.Describe(const AOptions: TNyxRegexOptions;
  const AKind, ATitle, AHelp: TNyxText; AOutput: TNyxFlowType);
begin
  FRegex := NewNyxRegex(AOptions);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind(AKind), ATitle, AHelp, nlgText)
    .Input(NyxRegexPort(nrpInput), nftText, 'Exact input text')
    .Output(NyxRegexPort(nrpValue), AOutput, 'Complete transformed result')
    .Output(NyxRegexPort(nrpMatched), nftBoolean, 'Whether the pattern matched')
    .Output(NyxRegexPort(nrpCount), nftInteger, 'Number of produced matches or rows')
    .Labelled('regex').Labelled('regular expression').Labelled('text').Labelled('unicode');
end;

function TRegexOperation.GetOptions: TNyxRegexOptions;
begin
  Result := FRegex.Options;
end;

constructor TRegexMatch.Create(const AOptions: TNyxRegexOptions);
begin
  inherited Create;
  Describe(AOptions, 'regex-match', 'Match regular expression',
    'Find exact Unicode-scalar matches and named/indexed captures with explicit flags, limits and cancellation.', nftJSON);
  FDescriptor := FDescriptor.Labelled('capture').Labelled('search');
  FConfiguration := NyxObject([NyxField('options', AOptions.ToData)]);
end;

function TRegexMatch.Find(const AInput: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxRegexMatches;
begin
  Result := FRegex.Find(AInput, ACancellation);
end;

function TRegexMatch.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
var
  LMatches: TNyxRegexMatches;
begin
  try
    LMatches := Find(AInputs.Value(NyxRegexPort(nrpInput)).AsText, ACancellation);
    Result := NyxFlowValues.WithValue(NyxRegexPort(nrpValue), NyxFlowJSON(LMatches.ToData))
      .WithValue(NyxRegexPort(nrpMatched), NyxFlowBoolean(LMatches.Count > 0))
      .WithValue(NyxRegexPort(nrpCount), NyxFlowInteger(LMatches.Count));
  except
    on E: ENyxRegexCancelled do
    begin
      raise ENyxFlowCancelled.Create(TNyxText(E.Message));
    end;
  end;
end;

constructor TRegexReplace.Create(const AOptions: TNyxRegexOptions;
  const AReplacement: TNyxRegexReplacement; AUnmatched: TNyxRegexUnmatched);
var
  LIndex: Integer;
  LPart: TNyxRegexReplacementPart;
  LChoice: Integer;
begin
  inherited Create;
  LChoice := Ord(AUnmatched);

  if (LChoice < Ord(Low(TNyxRegexUnmatched))) or
    (LChoice > Ord(High(TNyxRegexUnmatched))) then
  begin
    raise ENyxRegex.Create('Unknown unmatched replacement policy');
  end;
  AOptions.Validate;
  AReplacement.Validate;

  if AReplacement.Count > AOptions.Budget.MaximumInstructions then
  begin
    raise ENyxRegexLimit.Create('Replacement part count exceeds the instruction budget');
  end;
  Describe(AOptions, 'regex-replace', 'Replace regular expression',
    'Rewrite matched spans using typed literal and capture parts; preserve unmatched text and refuse partial output.', nftText);
  FDescriptor := FDescriptor.Labelled('replace').Labelled('template').Labelled('transform');
  FReplacement := AReplacement.Copy;
  FUnmatched := AUnmatched;
  SetLength(FIndices, FReplacement.Count);
  for LIndex := 0 to High(FIndices) do
  begin
    FIndices[LIndex] := -1;
    LPart := FReplacement.Part(LIndex);

    if LPart.Kind = nrpkCapture then
    begin
      FIndices[LIndex] := FRegex.CaptureIndex(LPart.Reference);
    end;
  end;
  FConfiguration := NyxObject([NyxField('options', AOptions.ToData),
    NyxField('replacement', FReplacement.ToData), NyxField('unmatched', NyxData(Ord(FUnmatched)))]);
end;

function TRegexReplace.GetReplacement: TNyxRegexReplacement;
begin
  Result := FReplacement.Copy;
end;

function TRegexReplace.GetUnmatched: TNyxRegexUnmatched;
begin
  Result := FUnmatched;
end;

function TRegexReplace.Apply(const AInput: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxRegexReplacementResult;
var
  LMatches: TNyxRegexMatches;
  LPieces: TNyxStrings;
  LMatch: TNyxRegexMatch;
  LCapture: TNyxRegexCapture;
  LPart: TNyxRegexReplacementPart;
  LOffsets: array of Integer;
  LStorage: Integer;
  LScalarCount: Integer;
  LScalar: Integer;
  LIndex: Integer;
  LPartIndex: Integer;
  LPrevious: Integer;
  LSteps: Integer;
  LBytes: Integer;
  LLimits: TNyxRegexLimits;

  procedure Add(const AText: TNyxText);
  var
    LCount: Integer;
  begin
    TransformStep(ACancellation, LSteps, LLimits);
    LCount := NyxUTF8ByteCount(AText);

    if LCount > LLimits.MaximumResultBytes - LBytes then
    begin
      raise ENyxRegexLimit.Create('Regex replacement exceeds its complete text byte budget');
    end;
    Inc(LBytes, LCount);

    if AText <> '' then
    begin
      LPieces.Add(AText);
    end;
  end;

begin
  Result := Default(TNyxRegexReplacementResult);
  LLimits := FRegex.Options.Budget;
  LSteps := 0;
  LBytes := 0;
  TransformStep(ACancellation, LSteps, LLimits);
  LMatches := FRegex.Find(AInput, ACancellation);
  { The matcher already admitted the input. Decode offsets once for exact
    scalar slicing; never rescan prefixes for every replacement or capture. }
  SetLength(LOffsets, Min(Length(AInput), LLimits.MaximumInputScalars) + 1);
  LStorage := 1;
  LScalarCount := 0;
  while LStorage <= Length(AInput) do
  begin
    TransformStep(ACancellation, LSteps, LLimits);
    LOffsets[LScalarCount] := LStorage;

    if not NyxNextScalar(AInput, LStorage, LScalar) then
    begin
      raise ENyxRegex.Create('Replacement input contains malformed Unicode');
    end;
    Inc(LScalarCount);
  end;
  LOffsets[LScalarCount] := Length(AInput) + 1;
  LPieces := TNyxStrings.Create;
  try
    LPrevious := 0;
    for LIndex := 0 to LMatches.Count - 1 do
    begin
      TransformStep(ACancellation, LSteps, LLimits);
      LMatch := LMatches.Item(LIndex);
      LCapture := LMatch.Group(0);
      Add(Copy(AInput, LOffsets[LPrevious],
        LOffsets[LCapture.Span.Start] - LOffsets[LPrevious]));
      for LPartIndex := 0 to FReplacement.Count - 1 do
      begin
        TransformStep(ACancellation, LSteps, LLimits);
        LPart := FReplacement.Part(LPartIndex);

        if LPart.Kind = nrpkLiteral then
        begin
          Add(LPart.Text);
        end
        else
        begin
          LCapture := LMatch.Group(FIndices[LPartIndex]);

          if not LCapture.Span.Defined and (FUnmatched = nruError) then
          begin
            raise ENyxRegex.Create('A selected replacement capture did not participate');
          end;
          Add(LCapture.Value);
        end;
      end;
      LCapture := LMatch.Group(0);
      LPrevious := LCapture.Span.Start + LCapture.Span.Count;
    end;
    Add(Copy(AInput, LOffsets[LPrevious], MaxInt));
    TransformStep(ACancellation, LSteps, LLimits);
    Result.Text := LPieces.Join;
    Result.Count := LMatches.Count;
  finally
    LPieces.Free;
  end;
end;

function TRegexReplace.Replace(const AInput: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxText;
begin
  Result := Apply(AInput, ACancellation).Text;
end;

function TRegexReplace.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
var
  LResult: TNyxRegexReplacementResult;
begin
  try
    LResult := Apply(AInputs.Value(NyxRegexPort(nrpInput)).AsText, ACancellation);
    Result := NyxFlowValues.WithValue(NyxRegexPort(nrpValue), NyxFlowText(LResult.Text))
      .WithValue(NyxRegexPort(nrpMatched), NyxFlowBoolean(LResult.Count > 0))
      .WithValue(NyxRegexPort(nrpCount), NyxFlowInteger(LResult.Count));
  except
    on E: ENyxRegexCancelled do
    begin
      raise ENyxFlowCancelled.Create(TNyxText(E.Message));
    end;
  end;
end;

constructor TRegexExtract.Create(const AOptions: TNyxRegexOptions;
  const ACapture: TNyxRegexCaptureRef; AMissing: TNyxRegexMissing);
var
  LChoice: Integer;
begin
  inherited Create;
  LChoice := Ord(AMissing);

  if (LChoice < Ord(Low(TNyxRegexMissing))) or (LChoice > Ord(High(TNyxRegexMissing))) then
  begin
    raise ENyxRegex.Create('Unknown missing-capture extraction policy');
  end;
  Describe(AOptions, 'regex-extract', 'Extract regular expression',
    'Extract one typed capture into rows with exact scalar spans, stable match-index ids and explicit missing participation.', nftJSON);
  FDescriptor := FDescriptor.Labelled('extract').Labelled('capture').Labelled('table').Labelled('rows');
  FIndex := FRegex.CaptureIndex(ACapture);
  FCapture := TNyxRegexCaptureRef.FromData(ACapture.ToData);
  FMissing := AMissing;
  FConfiguration := NyxObject([NyxField('options', AOptions.ToData),
    NyxField('capture', FCapture.ToData), NyxField('missing', NyxData(Ord(FMissing)))]);
end;

function TRegexExtract.GetCapture: TNyxRegexCaptureRef;
begin
  Result := FCapture;
end;

function TRegexExtract.GetMissing: TNyxRegexMissing;
begin
  Result := FMissing;
end;

function TRegexExtract.Rows(const AInput: TNyxText;
  const ACancellation: INyxCancellationScope; out AMatches: Integer): TNyxDataValue;
var
  LMatches: TNyxRegexMatches;
  LRows: array of TNyxDataValue;
  LCapture: TNyxRegexCapture;
  LValue: TNyxDataValue;
  LStart: TNyxDataValue;
  LLength: Integer;
  LIndex: Integer;
  LCount: Integer;
  LSteps: Integer;
  LBytes: Integer;
  LRowBytes: Integer;
  LLimits: TNyxRegexLimits;
begin
  LLimits := FRegex.Options.Budget;
  LSteps := 0;
  TransformStep(ACancellation, LSteps, LLimits);
  LMatches := FRegex.Find(AInput, ACancellation);
  AMatches := LMatches.Count;
  SetLength(LRows, LMatches.Count);
  LCount := 0;
  LBytes := 2;
  for LIndex := 0 to LMatches.Count - 1 do
  begin
    TransformStep(ACancellation, LSteps, LLimits);
    LCapture := LMatches.Item(LIndex).Group(FIndex);
    LValue := NyxNull;
    LStart := NyxNull;
    LLength := 0;

    if not LCapture.Span.Defined then
    begin

      if FMissing = nrxSkip then
      begin
        Continue;
      end;

      if FMissing = nrxError then
      begin
        raise ENyxRegex.Create('A selected extraction capture did not participate');
      end;
    end
    else
    begin
      LValue := NyxData(LCapture.Value);
      LStart := NyxData(LCapture.Span.Start);
      LLength := LCapture.Span.Count;
    end;
    LRows[LCount] := NyxObject([NyxField('id', NyxData(TNyxText(IntToStr(LIndex)))),
      NyxField('matched', NyxData(LCapture.Span.Defined)), NyxField('value', LValue),
      NyxField('start', LStart), NyxField('length', NyxData(LLength)),
      NyxField('capture', NyxData(FIndex)), NyxField('name', NyxData(LCapture.Name))]);
    LRowBytes := NyxUTF8ByteCount(LRows[LCount].ToJSON);

    if LRowBytes > LLimits.MaximumResultBytes - LBytes - 2 then
    begin
      raise ENyxRegexLimit.Create('Regex extracted rows exceed their complete byte budget');
    end;
    Inc(LBytes, LRowBytes + 2);
    Inc(LCount);
  end;
  SetLength(LRows, LCount);
  TransformStep(ACancellation, LSteps, LLimits);
  Result := NyxArray(LRows);

  if NyxUTF8ByteCount(Result.ToJSON) > LLimits.MaximumResultBytes then
  begin
    raise ENyxRegexLimit.Create('Regex extracted array exceeds its complete byte budget');
  end;
end;

function TRegexExtract.Extract(const AInput: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxDataValue;
var
  LMatches: Integer;
begin
  Result := Rows(AInput, ACancellation, LMatches);
end;

function TRegexExtract.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
var
  LRows: TNyxDataValue;
  LMatches: Integer;
begin
  try
    LRows := Rows(AInputs.Value(NyxRegexPort(nrpInput)).AsText, ACancellation, LMatches);
    Result := NyxFlowValues.WithValue(NyxRegexPort(nrpValue), NyxFlowJSON(LRows))
      .WithValue(NyxRegexPort(nrpMatched), NyxFlowBoolean(LMatches > 0))
      .WithValue(NyxRegexPort(nrpCount), NyxFlowInteger(LRows.Count));
  except
    on E: ENyxRegexCancelled do
    begin
      raise ENyxFlowCancelled.Create(TNyxText(E.Message));
    end;
  end;
end;

function NewNyxRegexMatch(const AOptions: TNyxRegexOptions): INyxRegexMatch;
begin
  Result := TRegexMatch.Create(AOptions);
end;

function NewNyxRegexReplace(const AOptions: TNyxRegexOptions;
  const AReplacement: TNyxRegexReplacement; AUnmatched: TNyxRegexUnmatched): INyxRegexReplace;
begin
  Result := TRegexReplace.Create(AOptions, AReplacement, AUnmatched);
end;

function NewNyxRegexExtract(const AOptions: TNyxRegexOptions): INyxRegexExtract;
begin
  Result := NewNyxRegexExtract(AOptions, NyxRegexCapture(0));
end;

function NewNyxRegexExtract(const AOptions: TNyxRegexOptions;
  const ACapture: TNyxRegexCaptureRef; AMissing: TNyxRegexMissing): INyxRegexExtract;
begin
  Result := TRegexExtract.Create(AOptions, ACapture, AMissing);
end;

function NyxRegexLogicFromData(const AKind: TNyxLogicKindRef;
  const AData: TNyxDataValue): INyxLogicOperation;
var
  LOptions: TNyxRegexOptions;
  LChoice: Integer;
begin
  Result := nil;

  if AKind.Name = 'regex-match' then
  begin
    ValidateNyxFlowFields(AData, ['options']);
    Exit(NewNyxRegexMatch(TNyxRegexOptions.FromData(AData.Field('options'))));
  end;

  if AKind.Name = 'regex-replace' then
  begin
    ValidateNyxFlowFields(AData, ['options', 'replacement', 'unmatched']);
    LOptions := TNyxRegexOptions.FromData(AData.Field('options'));
    LChoice := AData.Field('unmatched').AsInteger;

    if (LChoice < Ord(Low(TNyxRegexUnmatched))) or (LChoice > Ord(High(TNyxRegexUnmatched))) then
    begin
      raise ENyxRegex.Create('Unknown persisted replacement policy');
    end;
    Exit(NewNyxRegexReplace(LOptions,
      TNyxRegexReplacement.FromData(AData.Field('replacement'), LOptions.Budget.MaximumInstructions),
      TNyxRegexUnmatched(LChoice)));
  end;

  if AKind.Name = 'regex-extract' then
  begin
    ValidateNyxFlowFields(AData, ['options', 'capture', 'missing']);
    LOptions := TNyxRegexOptions.FromData(AData.Field('options'));
    LChoice := AData.Field('missing').AsInteger;

    if (LChoice < Ord(Low(TNyxRegexMissing))) or (LChoice > Ord(High(TNyxRegexMissing))) then
    begin
      raise ENyxRegex.Create('Unknown persisted extraction policy');
    end;
    Result := NewNyxRegexExtract(LOptions, TNyxRegexCaptureRef.FromData(AData.Field('capture')),
      TNyxRegexMissing(LChoice));
  end;
end;

end.
