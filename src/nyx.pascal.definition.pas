{ nyx - MIT License, Copyright (c) 2020 mr-highball }
unit nyx.pascal.definition;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.completion;

type
  { Definition lookup is inspection. A ready page retains every declared
    alternative in its total; it makes no overload-selection or edit claim. }
  TNyxPascalDefinitionStatus = (pdsReady, pdsUnavailable, pdsFailed, pdsCancelled);

  { A copied declaration descriptor. Location is absent for declarations outside
    captured project membership. The shared closed kind describes the UI role;
    no parser, filesystem, editor or executable callback is retained. }
  TNyxPascalDefinitionTarget = record
  private
    FName, FDetail: TNyxText;
    FKind: TNyxPascalCompletionKind;
    FLocation: TNyxPascalCompletionLocation;
  public
    property Name: TNyxText read FName;
    property Detail: TNyxText read FDetail;
    property Kind: TNyxPascalCompletionKind read FKind;
    property Location: TNyxPascalCompletionLocation read FLocation;
  end;

  { Exact immutable source query. At/From/Skip/Take return independent values.
    Caret and target coordinates count Unicode scalars. An include always needs
    its explicit translation-unit owner; no filename context is inferred. }
  INyxPascalDefinitionRequest = interface(IInterface)
    ['{2B1738D6-74E8-43C8-8E96-BCEB0E01E3F1}']
    function Sources: INyxPascalSourceSnapshot;
    function FileReference: TNyxPascalSourceRef;
    function Owner: TNyxPascalSourceRef;
    function Caret: Integer;
    function Offset: Integer;
    function Limit: Integer;
    function At(const AFile: TNyxPascalSourceRef; ACaret: Integer): INyxPascalDefinitionRequest;
    function From(const AOwner: TNyxPascalSourceRef): INyxPascalDefinitionRequest;
    function Skip(AOffset: Integer): INyxPascalDefinitionRequest;
    function Take(ALimit: Integer): INyxPascalDefinitionRequest;
    function Matches(const ASources: INyxPascalSourceSnapshot): Boolean;
  end;

  { Detached bounded targets. Unlike a completion response, this contract has no
    insertion or replacement operation. A navigation consumer must recheck the
    exact query, current caret, service identity and all current source buffers. }
  INyxPascalDefinitionResult = interface(IInterface)
    ['{2B1738D6-74E8-43C8-8E96-BCEB0E01E3F2}']
    function Request: INyxPascalDefinitionRequest;
    function Status: TNyxPascalDefinitionStatus;
    function MessageText: TNyxText;
    function Count: Integer;
    function Total: Integer;
    function Target(AIndex: Integer): TNyxPascalDefinitionTarget;
  end;

  { Optional provider capability. Implementations inspect the same owned source
    world as completion, without running authored code or touching a UI. Cancel
    returns no partial targets; lack of the capability is explicit to callers. }
  INyxPascalDefinitionProvider = interface(IInterface)
    ['{2B1738D6-74E8-43C8-8E96-BCEB0E01E3F3}']
    function ResolveDefinition(const ARequest: INyxPascalDefinitionRequest;
      const ACancellation: INyxCancellationScope = nil): INyxPascalDefinitionResult;
  end;

{ Defaults select the main source at zero, with twenty targets. Existing shared
  source/page bounds apply: at most fifty rows, offset at most 4096, 128 KiB of
  UTF-8 reply text, names/details at most 256/2048 Unicode scalars. }
function NewNyxPascalDefinitionRequest(
  const ASources: INyxPascalSourceSnapshot): INyxPascalDefinitionRequest;
function CaptureNyxPascalDefinitionRequest(
  const ARequest: INyxPascalDefinitionRequest): INyxPascalDefinitionRequest;
{ Bind copied target text and its optional project location atomically when
  constructing a result. A forged target cannot name unrelated source text. }
function NyxPascalDefinitionTarget(const AName, ADetail: TNyxText;
  AKind: TNyxPascalCompletionKind;
  const ALocation: TNyxPascalCompletionLocation): TNyxPascalDefinitionTarget;
function NewNyxPascalDefinitionResult(const ARequest: INyxPascalDefinitionRequest;
  AStatus: TNyxPascalDefinitionStatus; const AMessage: TNyxText;
  const ATargets: array of TNyxPascalDefinitionTarget;
  ATotal: Integer): INyxPascalDefinitionResult;
function CaptureNyxPascalDefinitionResult(const AResult: INyxPascalDefinitionResult;
  const AExpectedRequest: INyxPascalDefinitionRequest = nil): INyxPascalDefinitionResult;

{ Internal adapter boundaries reuse the established source capture and bounded
  reply validators. They grant no source edit or overload-selection authority.
  A completion page must originate from an explicit definition operation before
  this conversion: ordinary prefix suggestions are not resolved definitions. }
function NyxPascalDefinitionQuery(const ARequest: INyxPascalDefinitionRequest): INyxPascalCompletionRequest;
function NyxPascalDefinitionRequest(const AQuery: INyxPascalCompletionRequest): INyxPascalDefinitionRequest;
function NyxPascalDefinitionResult(const ARequest: INyxPascalDefinitionRequest;
  const APage: INyxPascalCompletionResult): INyxPascalDefinitionResult;
{ Private presentation/transport adapter. Every insertion and replacement is
  empty at the original caret, so even Preview is a text no-op. Targets retain
  their distinct locations; consumers must never interpret this as call choice. }
function NyxPascalDefinitionPage(const AResult: INyxPascalDefinitionResult): INyxPascalCompletionResult;

implementation

uses SysUtils, nyx.pascal.project;

type
  { Managed query owner delegates only to immutable shared source/query storage.
    No interface is placed in a copied record or a static interface array. }
  TDefinitionRequest = class(TInterfacedObject, INyxPascalDefinitionRequest)
  private
    FQuery: INyxPascalCompletionRequest;
  public
    function Sources: INyxPascalSourceSnapshot;
    function FileReference: TNyxPascalSourceRef;
    function Owner: TNyxPascalSourceRef;
    function Caret: Integer;
    function Offset: Integer;
    function Limit: Integer;
    function At(const AFile: TNyxPascalSourceRef; ACaret: Integer): INyxPascalDefinitionRequest;
    function From(const AOwner: TNyxPascalSourceRef): INyxPascalDefinitionRequest;
    function Skip(AOffset: Integer): INyxPascalDefinitionRequest;
    function Take(ALimit: Integer): INyxPascalDefinitionRequest;
    function Matches(const ASources: INyxPascalSourceSnapshot): Boolean;
  end;

  { One admitted immutable page. Private validated row storage is inaccessible
    through the definition interface, which deliberately exposes no editing API. }
  TDefinitionResult = class(TInterfacedObject, INyxPascalDefinitionResult)
  private
    FRequest: INyxPascalDefinitionRequest;
    FPage: INyxPascalCompletionResult;
    FLocations: INyxPascalCompletionDefinitions;
  public
    function Request: INyxPascalDefinitionRequest;
    function Status: TNyxPascalDefinitionStatus;
    function MessageText: TNyxText;
    function Count: Integer;
    function Total: Integer;
    function Target(AIndex: Integer): TNyxPascalDefinitionTarget;
  end;

function WrapQuery(const AQuery: INyxPascalCompletionRequest): INyxPascalDefinitionRequest;
var
  LValue: TDefinitionRequest;
  LHold: INyxPascalDefinitionRequest;
begin
  Result := nil;
  LValue := TDefinitionRequest.Create;
  LHold := LValue;
  { Only this unit's immutable query constructors reach this helper. Preserve
    an intermediate From/At chain; full ownership validation occurs at capture. }
  LValue.FQuery := AQuery;
  Result := LHold;
end;

function TDefinitionRequest.Sources: INyxPascalSourceSnapshot;
begin
  Result := FQuery.Sources;
end;

function TDefinitionRequest.FileReference: TNyxPascalSourceRef;
begin
  Result := FQuery.FileReference;
end;

function TDefinitionRequest.Owner: TNyxPascalSourceRef;
begin
  Result := FQuery.Owner;
end;

function TDefinitionRequest.Caret: Integer;
begin
  Result := FQuery.Caret;
end;

function TDefinitionRequest.Offset: Integer;
begin
  Result := FQuery.Offset;
end;

function TDefinitionRequest.Limit: Integer;
begin
  Result := FQuery.Limit;
end;

function TDefinitionRequest.At(const AFile: TNyxPascalSourceRef;
  ACaret: Integer): INyxPascalDefinitionRequest;
begin
  Result := WrapQuery(FQuery.At(AFile, ACaret));
end;

function TDefinitionRequest.From(const AOwner: TNyxPascalSourceRef): INyxPascalDefinitionRequest;
begin
  Result := WrapQuery(FQuery.From(AOwner));
end;

function TDefinitionRequest.Skip(AOffset: Integer): INyxPascalDefinitionRequest;
begin
  Result := WrapQuery(FQuery.Skip(AOffset));
end;

function TDefinitionRequest.Take(ALimit: Integer): INyxPascalDefinitionRequest;
begin
  Result := WrapQuery(FQuery.Take(ALimit));
end;

function TDefinitionRequest.Matches(const ASources: INyxPascalSourceSnapshot): Boolean;
begin
  Result := FQuery.Matches(ASources);
end;

function NewNyxPascalDefinitionRequest(
  const ASources: INyxPascalSourceSnapshot): INyxPascalDefinitionRequest;
begin
  Result := WrapQuery(NewNyxPascalCompletionRequest(ASources));
end;

function NyxPascalDefinitionQuery(const ARequest: INyxPascalDefinitionRequest): INyxPascalCompletionRequest;
var
  LRequest: INyxPascalDefinitionRequest;
  LFile, LOwner: TNyxPascalSourceRef;
begin
  LRequest := ARequest;
  Result := nil;

  if LRequest = nil then
  begin
    raise ENyxPascalProject.Create('Definition lookup requires captured inputs');
  end;
  LFile := LRequest.FileReference;
  LOwner := LRequest.Owner;

  if (LFile.Role <> npsInclude) and not LFile.SameFile(LOwner) then
  begin
    raise ENyxPascalProject.Create('A definition query requires its exact translation-unit owner');
  end;
  Result := CaptureNyxPascalCompletionRequest(NewNyxPascalCompletionRequest(LRequest.Sources)
    .From(LOwner).At(LFile, LRequest.Caret).Skip(LRequest.Offset).Take(LRequest.Limit));
end;

function CaptureNyxPascalDefinitionRequest(
  const ARequest: INyxPascalDefinitionRequest): INyxPascalDefinitionRequest;
var
  LRequest: INyxPascalDefinitionRequest;
begin
  LRequest := ARequest;
  Result := nil;
  Result := WrapQuery(NyxPascalDefinitionQuery(LRequest));
end;

function NyxPascalDefinitionRequest(const AQuery: INyxPascalCompletionRequest): INyxPascalDefinitionRequest;
begin
  Result := WrapQuery(CaptureNyxPascalCompletionRequest(AQuery));
end;

function NyxPascalDefinitionTarget(const AName, ADetail: TNyxText;
  AKind: TNyxPascalCompletionKind;
  const ALocation: TNyxPascalCompletionLocation): TNyxPascalDefinitionTarget;
var
  LRow: TNyxPascalCompletionItem;
begin
  LRow := NyxPascalCompletion(AName, '', ADetail, AKind);
  Result.FName := LRow.Name;
  Result.FDetail := LRow.Detail;
  Result.FKind := LRow.Kind;
  Result.FLocation := ALocation;
end;

function TDefinitionResult.Request: INyxPascalDefinitionRequest;
begin
  Result := FRequest;
end;

function TDefinitionResult.Status: TNyxPascalDefinitionStatus;
begin
  Result := TNyxPascalDefinitionStatus(Ord(FPage.Status));
end;

function TDefinitionResult.MessageText: TNyxText;
begin
  Result := FPage.MessageText;
end;

function TDefinitionResult.Count: Integer;
begin
  Result := FPage.Count;
end;

function TDefinitionResult.Total: Integer;
begin
  Result := FPage.Total;
end;

function TDefinitionResult.Target(AIndex: Integer): TNyxPascalDefinitionTarget;
var
  LRow: TNyxPascalCompletionItem;
begin
  LRow := FPage.Item(AIndex);
  Result := NyxPascalDefinitionTarget(LRow.Name, LRow.Detail, LRow.Kind,
    FLocations.DefinitionLocation(AIndex));
end;

function NewNyxPascalDefinitionResult(const ARequest: INyxPascalDefinitionRequest;
  AStatus: TNyxPascalDefinitionStatus; const AMessage: TNyxText;
  const ATargets: array of TNyxPascalDefinitionTarget;
  ATotal: Integer): INyxPascalDefinitionResult;
var
  LRequest: INyxPascalDefinitionRequest;
  LResult: TDefinitionResult;
  LHold: INyxPascalDefinitionResult;
  LQuery: INyxPascalCompletionRequest;
  LRows: array of TNyxPascalCompletionItem;
  LLocations: array of TNyxPascalCompletionLocation;
  LAt, LStart: Integer;
begin
  LRequest := ARequest;
  Result := nil;
  LRequest := CaptureNyxPascalDefinitionRequest(LRequest);
  LQuery := NyxPascalDefinitionQuery(LRequest);

  if (Ord(AStatus) < Ord(Low(TNyxPascalDefinitionStatus))) or
    (Ord(AStatus) > Ord(High(TNyxPascalDefinitionStatus))) or
    (Length(ATargets) > LQuery.Limit) then
  begin
    raise ENyxPascalProject.Create('Definition status or page exceeds its closed bounds');
  end;
  SetLength(LRows, Length(ATargets));
  SetLength(LLocations, Length(ATargets));
  for LAt := 0 to High(ATargets) do
  begin
    LRows[LAt] := NyxPascalCompletion(ATargets[LAt].Name, '',
      ATargets[LAt].Detail, ATargets[LAt].Kind);
    LLocations[LAt] := ATargets[LAt].Location;
  end;
  LStart := 0;

  if AStatus = pdsReady then
  begin
    LStart := LQuery.Caret;
  end;
  LResult := TDefinitionResult.Create;
  LHold := LResult;
  LResult.FRequest := LRequest;
  LResult.FPage := NewNyxPascalCompletionDefinitions(
    NewNyxPascalCompletionResult(LQuery, TNyxPascalCompletionStatus(Ord(AStatus)),
      AMessage, LStart, '', LRows, ATotal), LLocations);
  Supports(LResult.FPage, INyxPascalCompletionDefinitions, LResult.FLocations);
  Result := LHold;
end;

function CaptureNyxPascalDefinitionResult(const AResult: INyxPascalDefinitionResult;
  const AExpectedRequest: INyxPascalDefinitionRequest): INyxPascalDefinitionResult;
var
  LInput: INyxPascalDefinitionResult;
  LRequest: INyxPascalDefinitionRequest;
  LExpected, LQuery: INyxPascalCompletionRequest;
  LCount, LAt: Integer;
  LTargets: array of TNyxPascalDefinitionTarget;
begin
  LInput := AResult;
  Result := nil;

  if LInput = nil then
  begin
    raise ENyxPascalProject.Create('Definition capture requires a result');
  end;
  LRequest := CaptureNyxPascalDefinitionRequest(LInput.Request);

  if AExpectedRequest <> nil then
  begin
    LQuery := NyxPascalDefinitionQuery(LRequest);
    LExpected := NyxPascalDefinitionQuery(AExpectedRequest);

    if not LExpected.Matches(LQuery.Sources) or
      not LExpected.FileReference.SameFile(LQuery.FileReference) or
      not LExpected.Owner.SameFile(LQuery.Owner) or
      (LExpected.Caret <> LQuery.Caret) or (LExpected.Offset <> LQuery.Offset) or
      (LExpected.Limit <> LQuery.Limit) then
    begin
      raise ENyxPascalProject.Create('Definition result belongs to a different query');
    end;
  end;
  LCount := LInput.Count;

  if (LCount < 0) or (LCount > LRequest.Limit) then
  begin
    raise ENyxPascalProject.Create('Definition result exceeds its page budget');
  end;
  SetLength(LTargets, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    LTargets[LAt] := LInput.Target(LAt);
  end;
  Result := NewNyxPascalDefinitionResult(LRequest, LInput.Status,
    LInput.MessageText, LTargets, LInput.Total);
end;

function NyxPascalDefinitionResult(const ARequest: INyxPascalDefinitionRequest;
  const APage: INyxPascalCompletionResult): INyxPascalDefinitionResult;
var
  LRequest: INyxPascalDefinitionRequest;
  LPage: INyxPascalCompletionResult;
  LDefinitions: INyxPascalCompletionDefinitions;
  LLocations: TNyxPascalCompletionLocation;
  LTargets: array of TNyxPascalDefinitionTarget;
  LRow: TNyxPascalCompletionItem;
  LAt: Integer;
begin
  LRequest := ARequest;
  LPage := APage;
  Result := nil;
  LRequest := CaptureNyxPascalDefinitionRequest(LRequest);
  LPage := CaptureNyxPascalCompletionResult(LPage, NyxPascalDefinitionQuery(LRequest));
  Supports(LPage, INyxPascalCompletionDefinitions, LDefinitions);
  SetLength(LTargets, LPage.Count);
  for LAt := 0 to LPage.Count - 1 do
  begin
    LLocations := Default(TNyxPascalCompletionLocation);

    if LDefinitions <> nil then
    begin
      LLocations := LDefinitions.DefinitionLocation(LAt);
    end;
    LRow := LPage.Item(LAt);
    LTargets[LAt] := NyxPascalDefinitionTarget(LRow.Name, LRow.Detail, LRow.Kind, LLocations);
  end;
  Result := NewNyxPascalDefinitionResult(LRequest, TNyxPascalDefinitionStatus(Ord(LPage.Status)),
    LPage.MessageText, LTargets, LPage.Total);
end;

function NyxPascalDefinitionPage(const AResult: INyxPascalDefinitionResult): INyxPascalCompletionResult;
var
  LResult: INyxPascalDefinitionResult;
  LQuery: INyxPascalCompletionRequest;
  LRows: array of TNyxPascalCompletionItem;
  LLocations: array of TNyxPascalCompletionLocation;
  LTarget: TNyxPascalDefinitionTarget;
  LPage: INyxPascalCompletionResult;
  LAt, LStart: Integer;
begin
  LResult := AResult;
  Result := nil;
  LResult := CaptureNyxPascalDefinitionResult(LResult);
  LQuery := NyxPascalDefinitionQuery(LResult.Request);
  SetLength(LRows, LResult.Count);
  SetLength(LLocations, LResult.Count);
  for LAt := 0 to LResult.Count - 1 do
  begin
    LTarget := LResult.Target(LAt);
    LRows[LAt] := NyxPascalCompletion(LTarget.Name, '', LTarget.Detail, LTarget.Kind);
    LLocations[LAt] := LTarget.Location;
  end;
  LStart := 0;

  if LResult.Status = pdsReady then
  begin
    LStart := LQuery.Caret;
  end;
  LPage := NewNyxPascalCompletionResult(LQuery,
    TNyxPascalCompletionStatus(Ord(LResult.Status)), LResult.MessageText, LStart, '', LRows, LResult.Total);
  Result := NewNyxPascalCompletionDefinitions(LPage, LLocations);
end;

end.
