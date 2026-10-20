{ nyx - MIT License, Copyright (c) 2020 mr-highball }
program nyx_completion_definitions_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.data, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.wire
  {$ifdef PAS2JS}, Web{$endif};

type
  { Mutable alternate provider proves capture owns its values and reads the
    request only once. No alternate getter is trusted after publication. }
  TAlternateResult = class(TInterfacedObject, INyxPascalCompletionResult,
    INyxPascalCompletionDefinitions)
  public
    Base: INyxPascalCompletionResult;
    Location: TNyxPascalCompletionLocation;
    QueryOverride: INyxPascalCompletionRequest;
    RequestReads, ItemReads, CountReads: Integer;
    ClaimedCount: Integer;
    function Request: INyxPascalCompletionRequest;
    function Status: TNyxPascalCompletionStatus;
    function MessageText: TNyxText;
    function Count: Integer;
    function Total: Integer;
    function Item(AIndex: Integer): TNyxPascalCompletionItem;
    function ReplaceStart: Integer;
    function ExpectedText: TNyxText;
    function Preview(AIndex: Integer; const ACurrent: INyxPascalSourceSnapshot;
      out ANewCaret: Integer): INyxPascalSourceSnapshot;
    function DefinitionLocation(AIndex: Integer): TNyxPascalCompletionLocation;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

function TAlternateResult.Request: INyxPascalCompletionRequest;
begin
  Inc(RequestReads);
  Result := Base.Request;

  if QueryOverride <> nil then
  begin
    Result := QueryOverride;
  end;
end;

function TAlternateResult.Status: TNyxPascalCompletionStatus;
begin
  Result := Base.Status;
end;

function TAlternateResult.MessageText: TNyxText;
begin
  Result := Base.MessageText;
end;

function TAlternateResult.Count: Integer;
begin
  Inc(CountReads);
  Result := ClaimedCount;
end;

function TAlternateResult.Total: Integer;
begin
  Result := Base.Total;
end;

function TAlternateResult.Item(AIndex: Integer): TNyxPascalCompletionItem;
begin
  Inc(ItemReads);
  Result := Base.Item(AIndex);
end;

function TAlternateResult.ReplaceStart: Integer;
begin
  Result := Base.ReplaceStart;
end;

function TAlternateResult.ExpectedText: TNyxText;
begin
  Result := Base.ExpectedText;
end;

function TAlternateResult.Preview(AIndex: Integer;
  const ACurrent: INyxPascalSourceSnapshot;
  out ANewCaret: Integer): INyxPascalSourceSnapshot;
begin
  Result := Base.Preview(AIndex, ACurrent, ANewCaret);
end;

function TAlternateResult.DefinitionLocation(AIndex: Integer): TNyxPascalCompletionLocation;
begin
  Base.Item(AIndex);
  Result := Location;
end;

{ Replace one packet field through the strict JSON value owner; no duplicate
  keys or byte-based Unicode replacements weaken malformed-packet checks. }
function FieldWith(const AWire, AName: TNyxText;
  const AValue: TNyxDataValue): TNyxText;
var
  LObject: TNyxDataValue;
  LFields: array of TNyxDataField;
  LAt: Integer;
begin
  LObject := TNyxDataValue.ParseJSON(AWire);
  SetLength(LFields, LObject.Count);
  for LAt := 0 to LObject.Count - 1 do
  begin
    LFields[LAt] := NyxField(LObject.Key(LAt), LObject.Field(LObject.Key(LAt)));

    if LFields[LAt].Name = AName then
    begin
      LFields[LAt].Value := AValue;
    end;
  end;
  Result := NyxObject(LFields).ToJSON;
end;

function LocationData(const AKind, AFile, AName: TNyxText;
  AStart, ALength: Integer): TNyxDataValue;
begin
  Result := NyxObject([
    NyxField('file', NyxObject([NyxField('kind', NyxData(AKind)),
      NyxField('name', NyxData(AFile))])),
    NyxField('start', NyxData(AStart)), NyxField('length', NyxData(ALength)),
    NyxField('name', NyxData(AName))]);
end;

procedure RefuseReply(const AWire, AID: TNyxText;
  const ARequest: INyxPascalCompletionRequest; const AReason: TNyxText);
var
  LRejected: Boolean;
  LResult: INyxPascalCompletionResult;
begin
  LRejected := False;
  try
    LResult := DecodeNyxCompletionReply(AWire, AID, ARequest);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResult = nil), AReason);
end;

function Definitions(const AResult: INyxPascalCompletionResult): INyxPascalCompletionDefinitions;
begin
  Check(Supports(AResult, INyxPascalCompletionDefinitions, Result),
    'A captured project definition retains its optional typed capability');
end;

procedure Run;
const
  CPrefix: TNyxText = '// 🚀 𐐷' + #0 + #13#10 + 'function ';
  CName: TNyxText = 'WithText';
var
  LProject: INyxPascalProject;
  LSources, LChanged: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LBase, LResult, LReceived, LHold: INyxPascalCompletionResult;
  LMain, LInclude, LMissing: TNyxPascalSourceRef;
  LLocation: TNyxPascalCompletionLocation;
  LAbsent: TNyxPascalCompletionLocation;
  LWire, LLegacy: TNyxText;
  LStart, LCaret, LAt: Integer;
  LRejected: Boolean;
  LAlternate: TAlternateResult;
  LRows: array of TNyxPascalCompletionItem;
  LDefinitions: array of TNyxPascalCompletionLocation;
begin
  LMain := TNyxPascalSourceRef.Main;
  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('members.inc'));
  LMissing := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('foreign.inc'));
  LProject := NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.')
    .WithInclude(NyxPascalInclude('members.inc'), CPrefix + CName + ';');
  LSources := CaptureNyxPascalSources(LProject).WithBuffer(LMain, 'Badge.Wi');
  LRequest := NewNyxPascalCompletionRequest(LSources).At(LMain, 8).Take(2);
  LBase := NewNyxPascalCompletionResult(LRequest, pcsReady, '', 6, 'Wi',
    [NyxPascalCompletion(CName, CName, 'declared method', pckMethod),
     NyxPascalCompletion('WithTone', 'WithTone', '', pckMethod)], 2);
  LStart := NyxTextScalarCount(CPrefix);
  LLocation := NyxPascalCompletionLocation(LInclude, LStart, 8, CName);
  LAbsent := Default(TNyxPascalCompletionLocation);
  LResult := NewNyxPascalCompletionDefinitions(LBase, [LLocation, LAbsent]);
  Check(Definitions(LResult).DefinitionLocation(0).Available and
    (Definitions(LResult).DefinitionLocation(0).Start = LStart) and
    Definitions(LResult).DefinitionLocation(0).FileReference.SameFile(LInclude) and
    not Definitions(LResult).DefinitionLocation(1).Available,
    'Scalar location survives supplementary text, embedded NUL and CRLF with an absent peer');
  LWire := EncodeNyxCompletionReply(LResult, 'definition-query');
  Check(TNyxDataValue.ParseJSON(LWire).Field('version').AsInteger = 2,
    'Present project definitions explicitly select reply protocol version two');
  LReceived := DecodeNyxCompletionReply(LWire, 'definition-query', LRequest);
  Check((Definitions(LReceived).DefinitionLocation(0).Name = CName) and
    (Definitions(LReceived).DefinitionLocation(0).Length = 8) and
    not Definitions(LReceived).DefinitionLocation(1).Available,
    'Wire roundtrip preserves exact row ordinals and optional source coordinates');
  LReceived.Preview(0, LSources, LCaret);
  Check(LCaret = 14, 'Navigation metadata preserves ordinary insertion preview');
  LLegacy := EncodeNyxCompletionReply(LBase, 'legacy');
  LReceived := NewNyxPascalCompletionDefinitions(LBase, [LAbsent, LAbsent]);
  Check(EncodeNyxCompletionReply(LReceived, 'legacy') = LLegacy,
    'Absent optional metadata retains byte-identical legacy reply encoding');

  RefuseReply(FieldWith(LWire, 'version', NyxData(3)), 'definition-query', LRequest,
    'Unknown reply versions refuse atomically');
  RefuseReply(FieldWith(LWire, 'version', NyxData(1)), 'definition-query', LRequest,
    'Version one refuses the version two definitions field');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([])), 'definition-query', LRequest,
    'Definition vector length must equal its captured row count');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    LocationData('include', 'foreign.inc', CName, LStart, 8), NyxNull])),
    'definition-query', LRequest, 'Definitions outside captured project membership refuse');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    LocationData('include', '../members.inc', CName, LStart, 8), NyxNull])),
    'definition-query', LRequest, 'Definition paths cannot escape logical source leaves');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    LocationData('unit', 'members.inc', CName, LStart, 8), NyxNull])),
    'definition-query', LRequest, 'Mismatched source roles cannot substitute for an include');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    LocationData('include', 'members.inc', 'WithTone', LStart, 8), NyxNull])),
    'definition-query', LRequest, 'Definition name must match the same completion row');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    LocationData('include', 'members.inc', CName, LStart + 1, 8), NyxNull])),
    'definition-query', LRequest, 'A shifted definition must match exact original source text');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    LocationData('include', 'members.inc', CName, -1, 8), NyxNull])),
    'definition-query', LRequest, 'Negative scalar coordinates refuse');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    LocationData('include', 'members.inc', CName, MaxInt, 8), NyxNull])),
    'definition-query', LRequest, 'Overflowing coordinates refuse before range arithmetic');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    LocationData('include', 'members.inc', CName, LStart, 0), NyxNull])),
    'definition-query', LRequest, 'Empty name spans refuse');
  RefuseReply(FieldWith(LWire, 'definitions', NyxArray([
    NyxObject([NyxField('unknown', NyxData(1))]), NyxNull])),
    'definition-query', LRequest, 'Unknown definition fields refuse');
  RefuseReply(LWire, 'foreign-id', LRequest,
    'Definition metadata cannot bypass reply correlation identity');
  LChanged := LSources.WithBuffer(LInclude, CPrefix + 'WithTone;');
  RefuseReply(LWire, 'definition-query', NewNyxPascalCompletionRequest(LChanged)
    .At(LMain, 8).Take(2), 'Changed declaration buffers refuse stale transported coordinates');

  LAlternate := TAlternateResult.Create;
  LHold := LAlternate;
  LAlternate.Base := NewNyxPascalCompletionResult(LRequest, pcsReady, '', 6, 'Wi',
    [NyxPascalCompletion(CName, CName, '', pckMethod)], 1);
  LAlternate.ClaimedCount := 1;
  LAlternate.Location := LLocation;
  LReceived := CaptureNyxPascalCompletionResult(LHold, LRequest);
  Check((LAlternate.RequestReads = 1) and (LAlternate.CountReads = 1) and
    (LAlternate.ItemReads = 1), 'Alternate request, count and row getters freeze exactly once');
  LReceived := CaptureNyxPascalCompletionResult(LReceived, LRequest);
  Check(Definitions(LReceived).DefinitionLocation(0).Available,
    'Capturing into the input variable retains its immutable source before publication');
  LAlternate.Location := LAbsent;
  LAlternate.QueryOverride := LRequest.At(LMain, 7);
  Check(Definitions(LReceived).DefinitionLocation(0).Available and
    (LReceived.Request.Caret = 8), 'Frozen navigation retains no mutable provider borrow');
  LAt := LAlternate.ItemReads;
  LRejected := False;
  try
    LReceived := CaptureNyxPascalCompletionResult(LHold, LRequest);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LReceived = nil) and (LAlternate.ItemReads = LAt),
    'A different exact caret query refuses before reading alternate rows');
  LAlternate.QueryOverride := nil;
  LAlternate.ClaimedCount := 51;
  LRejected := False;
  try
    LReceived := CaptureNyxPascalCompletionResult(LHold, LRequest);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LReceived = nil) and (LAlternate.ItemReads = LAt),
    'Alternate oversized row counts refuse before allocation or row reads');
  LAlternate.ClaimedCount := 1;
  LAlternate.Location := NyxPascalCompletionLocation(LMissing, LStart, 8, CName);
  LRejected := False;
  try
    LReceived := CaptureNyxPascalCompletionResult(LHold, LRequest);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LReceived = nil),
    'Alternate metadata receives the same exact atomic membership validation');
  LHold := nil;

  LRejected := False;
  try
    LReceived := NewNyxPascalCompletionDefinitions(LBase, [LLocation]);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LReceived = nil), 'Partial metadata vectors cannot publish a partial page');
  LRejected := False;
  try
    NyxPascalCompletionLocation(LInclude, 0, 1, #0);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'Control characters cannot masquerade as a declaration name');
  LChanged := LSources.WithBuffer(LInclude, CPrefix + '&type;');
  LRequest := NewNyxPascalCompletionRequest(LChanged).At(LMain, 8);
  LBase := NewNyxPascalCompletionResult(LRequest, pcsReady, '', 6, 'Wi',
    [NyxPascalCompletion('type', '&type', '', pckField)], 1);
  LResult := NewNyxPascalCompletionDefinitions(LBase,
    [NyxPascalCompletionLocation(LInclude, LStart, 5, 'type')]);
  Check(Definitions(LResult).DefinitionLocation(0).Length = 5,
    'Escaped Pascal identifiers retain the complete physical ampersand span');
  LBase := NewNyxPascalCompletionResult(LRequest, pcsCancelled, 'cancelled', 0, '', [], 0);
  LResult := NewNyxPascalCompletionDefinitions(LBase, []);
  Check((LResult.Count = 0) and (TNyxDataValue.ParseJSON(
    EncodeNyxCompletionReply(LResult, 'cancelled')).Field('version').AsInteger = 1),
    'Cancelled replies retain zero definition rows and legacy empty encoding');

  { Aggregate metadata shares the existing 128 KiB payload allowance. }
  LRequest := LRequest.Take(50);
  SetLength(LRows, 50);
  SetLength(LDefinitions, 50);
  for LAt := 0 to High(LRows) do
  begin
    LRows[LAt] := NyxPascalCompletion('type', StringOfChar('x', 577),
      StringOfChar('y', 2040), pckField);
    LDefinitions[LAt] := NyxPascalCompletionLocation(LInclude, LStart, 5, 'type');
  end;
  LBase := NewNyxPascalCompletionResult(LRequest, pcsReady, '', 6, 'Wi', LRows, 50);
  LRejected := False;
  try
    NewNyxPascalCompletionDefinitions(LBase, LDefinitions);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'Definition names and logical leaves consume the existing result payload budget');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' completion definition checks';
    {$else}
    WriteLn('PASS ', GChecks, ' completion definition checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
