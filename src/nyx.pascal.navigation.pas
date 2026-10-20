{ Nyx source-navigation values. MIT license, see LICENSE. }
unit nyx.pascal.navigation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.pascal.drafts, nyx.scheduler;

type
  { Invalid locations and retention policies refuse before publishing a trail.
    Navigation data grants no file access, compiler or source-edit authority. }
  ENyxPascalNavigation = class(EArgumentException);
  { Traversal is independent of text Undo/Redo and platform browser history. }
  TNyxPascalNavigationDirection = (pndBack, pndForward);
  { Relocated means exact textual evidence in a changed buffer, never semantic
    symbol identity. Changed/ambiguous/cancelled/unavailable publish no caret. }
  TNyxPascalNavigationStatus = (pnsExact, pnsRelocated, pnsChanged,
    pnsAmbiguous, pnsCancelled, pnsUnavailable);
  { Copied logical selector, exact authored text and zero-based scalar caret.
    Only values survive; no source provider, project, editor or UI is retained.
    CRLF is indivisible for logical host positioning; its interior refuses.
    A point is at most the existing four-MiB inspected-source allowance. }
  TNyxPascalNavigationPoint = record
  private
    FDefined: Boolean;
    FFile: TNyxPascalSourceRef;
    FText: TNyxText;
    FCaret: Integer;
    FBytes: Integer;
  public
    procedure Validate;
    function SameLocation(const AOther: TNyxPascalNavigationPoint): Boolean;
    { Resolve only on traversal. Logical CR/LF/CRLF spelling is projected; other
      content remains exact. Unchanged prefixes/suffixes keep their positions.
      Otherwise a 32-scalar context on each side must occur exactly once.
      This does not guess inside replaced or ambiguously repeated content. }
    function Resolve(const AText: TNyxText; out ACaret: Integer;
      const ACancellation: INyxCancellationScope = nil): TNyxPascalNavigationStatus;
    { One-based logical lines/scalar columns for ordinary host presentation. }
    procedure Position(out ALine, AColumn: Integer);
    property Defined: Boolean read FDefined;
    property FileReference: TNyxPascalSourceRef read FFile;
    property Text: TNyxText read FText;
    property Caret: Integer read FCaret;
    property RetainedBytes: Integer read FBytes;
  end;
  { Accounted retained UTF-8 text, selector and 64 bytes per location; this is
    not a heap measurement. Defaults are 128 locations/16 MiB. Zero entries
    disables retention; other policies admit two through 1024 locations. }
  TNyxPascalNavigationLimits = record
  private
    FDefined: Boolean;
    FEntries, FBytes: Integer;
  public
    class function Defaults: TNyxPascalNavigationLimits; static;
    function UpToEntries(AValue: Integer): TNyxPascalNavigationLimits;
    function UpToBytes(AValue: Integer): TNyxPascalNavigationLimits;
    procedure Validate;
    property MaximumEntries: Integer read FEntries;
    property MaximumBytes: Integer read FBytes;
  end;
  { Persistent bounded trail. Every mutation returns an independent candidate;
    the owner publishes it only after its source/mount guards pass. Travel
    captures the actual departure location, resolves its destination against
    supplied current text and preserves the immediate return pair when trimming.
    A new Visit drops the forward branch; selection/edit history stays separate. }
  INyxPascalNavigationTrail = interface(IInterface)
    ['{7BF5E6B8-B626-45EF-9DF3-E08CC692FC4A}']
    function Count: Integer;
    function Position: Integer;
    function RetainedBytes: Integer;
    function Current: TNyxPascalNavigationPoint;
    function CanTravel(ADirection: TNyxPascalNavigationDirection): Boolean;
    function Destination(ADirection: TNyxPascalNavigationDirection): TNyxPascalNavigationPoint;
    function Visit(const AFrom, ATo: TNyxPascalNavigationPoint): INyxPascalNavigationTrail;
    function Travel(ADirection: TNyxPascalNavigationDirection;
      const ALeaving: TNyxPascalNavigationPoint; const ATargetText: TNyxText;
      out AStatus: TNyxPascalNavigationStatus;
      const ACancellation: INyxCancellationScope = nil): INyxPascalNavigationTrail;
  end;

function NyxPascalNavigationPoint(const AFile: TNyxPascalSourceRef;
  const AText: TNyxText; ACaret: Integer): TNyxPascalNavigationPoint;
function NewNyxPascalNavigationTrail: INyxPascalNavigationTrail; overload;
function NewNyxPascalNavigationTrail(
  const ALimits: TNyxPascalNavigationLimits): INyxPascalNavigationTrail; overload;

implementation

uses nyx.bytes, nyx.editing, nyx.text.projection;

type
  { Value-only dynamic storage; detached arrays never escape the immutable owner. }
  TNavigationPoints = array of TNyxPascalNavigationPoint;
  { Owns a bounded vector and cursor. The immediate departure/arrival pair cannot
    be evicted to make an otherwise unretainable navigation appear successful. }
  TNavigationTrail = class(TInterfacedObject, INyxPascalNavigationTrail)
  private
    FLimits: TNyxPascalNavigationLimits;
    FPoints: TNavigationPoints;
    FPosition: Integer;
    function AccountedBytes: TNyxTextBytes;
    function CopyValue: TNavigationTrail;
    procedure Trim(AFirstRequired, ALastRequired: Integer);
  public
    function Count: Integer;
    function Position: Integer;
    function RetainedBytes: Integer;
    function Current: TNyxPascalNavigationPoint;
    function CanTravel(ADirection: TNyxPascalNavigationDirection): Boolean;
    function Destination(ADirection: TNyxPascalNavigationDirection): TNyxPascalNavigationPoint;
    function Visit(const AFrom, ATo: TNyxPascalNavigationPoint): INyxPascalNavigationTrail;
    function Travel(ADirection: TNyxPascalNavigationDirection;
      const ALeaving: TNyxPascalNavigationPoint; const ATargetText: TNyxText;
      out AStatus: TNyxPascalNavigationStatus;
      const ACancellation: INyxCancellationScope = nil): INyxPascalNavigationTrail;
  end;

function Cancelled(const AScope: INyxCancellationScope): Boolean;
begin
  Result := (AScope <> nil) and AScope.Cancelled;
end;

function ScalarStorage(const AText: TNyxText; AOffset: Integer): Integer;
var
  LAt, LScalar: Integer;
begin
  Result := 1;
  for LAt := 1 to AOffset do
  begin

    if not NyxNextScalar(AText, Result, LScalar) then
      raise ENyxPascalNavigation.Create('Navigation caret is outside exact Unicode text');
  end;
end;

function ScalarBoundary(const AText: TNyxText; AStorage: Integer): Boolean;
var
  LValue: Integer;
begin
  Result := True;

  if (AStorage <= 1) or (AStorage > Length(AText)) then Exit;
  LValue := Ord(AText[AStorage]);
  {$ifdef PAS2JS}
  Result := (LValue < $DC00) or (LValue > $DFFF);
  {$else}
  Result := (LValue < $80) or (LValue >= $C0);
  {$endif}
end;

{ Linear exact storage-unit matching over already validated Unicode. A valid
  pattern cannot begin inside UTF-8/UTF-16 scalars. Overlapping second matches
  count as ambiguity. No case folding, regex or authored code executes. }
function UniqueContext(const AText, APattern: TNyxText; out AStorage: Integer;
  const AScope: INyxCancellationScope): TNyxPascalNavigationStatus;
var
  LFailure: array of Integer;
  LAt, LMatched, LFound: Integer;
begin
  AStorage := 0;
  Result := pnsChanged;

  if APattern = '' then Exit;
  SetLength(LFailure, Length(APattern));
  LMatched := 0;
  for LAt := 2 to Length(APattern) do
  begin
    while (LMatched > 0) and (APattern[LMatched + 1] <> APattern[LAt]) do
      LMatched := LFailure[LMatched - 1];

    if APattern[LMatched + 1] = APattern[LAt] then Inc(LMatched);
    LFailure[LAt - 1] := LMatched;
  end;
  LMatched := 0;
  LFound := 0;
  for LAt := 1 to Length(AText) do
  begin

    if ((LAt and 1023) = 0) and Cancelled(AScope) then Exit(pnsCancelled);
    while (LMatched > 0) and (APattern[LMatched + 1] <> AText[LAt]) do
      LMatched := LFailure[LMatched - 1];

    if APattern[LMatched + 1] = AText[LAt] then Inc(LMatched);

    if LMatched = Length(APattern) then
    begin
      Inc(LFound);

      if LFound > 1 then Exit(pnsAmbiguous);
      AStorage := LAt - Length(APattern) + 1;
      LMatched := LFailure[LMatched - 1];
    end;
  end;

  if LFound = 1 then Result := pnsRelocated;
end;

function NyxPascalNavigationPoint(const AFile: TNyxPascalSourceRef;
  const AText: TNyxText; ACaret: Integer): TNyxPascalNavigationPoint;
var
  LBytes: Integer;
begin
  AFile.Validate;

  { Storage units are a lower bound for UTF-8 bytes on both targets. Refuse
    impossible sizes before encoding validation can scan an unbounded input. }

  if Length(AText) > 4 * 1024 * 1024 then
    raise ENyxPascalNavigation.Create('Navigation source exceeds the inspected four-MiB allowance');
  LBytes := NyxUTF8ByteCount(AText);

  if LBytes > 4 * 1024 * 1024 then
    raise ENyxPascalNavigation.Create('Navigation source exceeds the inspected four-MiB allowance');
  Result := Default(TNyxPascalNavigationPoint);
  Result.FDefined := True;
  Result.FFile := AFile;
  Result.FText := AText;
  Result.FCaret := ACaret;
  Result.FBytes := LBytes + NyxUTF8ByteCount(AFile.Name) + 64;
  Result.Validate;
end;

procedure TNyxPascalNavigationPoint.Validate;
var
  LStorage: Integer;
begin

  if not FDefined then
    raise ENyxPascalNavigation.Create('A defined source-navigation point is required');
  FFile.Validate;

  if (FCaret < 0) or (FCaret > NyxTextScalarCount(FText)) then
    raise ENyxPascalNavigation.Create('Navigation caret is outside exact authored text');
  LStorage := ScalarStorage(FText, FCaret);

  if (LStorage > 1) and (LStorage <= Length(FText)) and
    (FText[LStorage] = #10) and (FText[LStorage - 1] = #13) then
    raise ENyxPascalNavigation.Create('A logical navigation caret cannot split CRLF');
end;

function TNyxPascalNavigationPoint.SameLocation(
  const AOther: TNyxPascalNavigationPoint): Boolean;
begin
  Result := FDefined and AOther.FDefined and FFile.SameFile(AOther.FFile) and
    (FText = AOther.FText) and (FCaret = AOther.FCaret);
end;

function TNyxPascalNavigationPoint.Resolve(const AText: TNyxText;
  out ACaret: Integer; const ACancellation: INyxCancellationScope): TNyxPascalNavigationStatus;
var
  LOldProjection, LNewProjection: INyxTextProjection;
  LOld, LNew, LContext: TNyxText;
  LOldAt, LNewAt, LOldNext, LNewNext, LOldScalar, LNewScalar: Integer;
  LPrefix, LSuffix, LOldEnd, LLogicalCaret, LResolved: Integer;
  LContextStart, LContextFinish, LContextAt: Integer;
begin
  Validate;
  ACaret := -1;

  if Cancelled(ACancellation) then Exit(pnsCancelled);

  if Length(AText) > 4 * 1024 * 1024 then
    raise ENyxPascalNavigation.Create('Navigation target exceeds the inspected four-MiB allowance');

  if NyxUTF8ByteCount(AText) > 4 * 1024 * 1024 then
    raise ENyxPascalNavigation.Create('Navigation target exceeds the inspected four-MiB allowance');
  NyxTextScalarCount(AText);

  if AText = FText then
  begin
    ACaret := FCaret;
    Exit(pnsExact);
  end;
  LOld := NyxPresentText(FText, nteLF);
  LNew := NyxPresentText(AText, nteLF);
  LOldProjection := NewNyxTextProjection(FText, LOld);
  LNewProjection := NewNyxTextProjection(AText, LNew);
  try
    LLogicalCaret := LOldProjection.ToPresented(FCaret);
  except
    on ENyxTextProjection do Exit(pnsChanged);
  end;
  LResolved := LLogicalCaret;

  if LOld <> LNew then
  begin
    LOldAt := 1;
    LNewAt := 1;
    LPrefix := 0;
    while (LOldAt <= Length(LOld)) and (LNewAt <= Length(LNew)) do
    begin

      if ((LPrefix and 1023) = 0) and Cancelled(ACancellation) then Exit(pnsCancelled);
      LOldNext := LOldAt;
      LNewNext := LNewAt;
      NyxNextScalar(LOld, LOldNext, LOldScalar);
      NyxNextScalar(LNew, LNewNext, LNewScalar);

      if LOldScalar <> LNewScalar then Break;
      LOldAt := LOldNext;
      LNewAt := LNewNext;
      Inc(LPrefix);
    end;
    LSuffix := 0;
    while (LSuffix < Length(LOld) - LOldAt + 1) and
      (LSuffix < Length(LNew) - LNewAt + 1) and
      (LOld[Length(LOld) - LSuffix] = LNew[Length(LNew) - LSuffix]) do
    begin

      if ((LSuffix and 1023) = 0) and Cancelled(ACancellation) then Exit(pnsCancelled);
      Inc(LSuffix);
    end;
    while (LSuffix > 0) and
      (not ScalarBoundary(LOld, Length(LOld) - LSuffix + 1) or
      not ScalarBoundary(LNew, Length(LNew) - LSuffix + 1)) do Dec(LSuffix);
    LOldEnd := LOldProjection.PresentedScalars -
      NyxTextScalarCount(Copy(LOld, Length(LOld) - LSuffix + 1, LSuffix));

    if LLogicalCaret < LPrefix then
      LResolved := LLogicalCaret
    else if LLogicalCaret > LOldEnd then
      LResolved := LLogicalCaret + LNewProjection.PresentedScalars - LOldProjection.PresentedScalars
    else
    begin
      LContextStart := LLogicalCaret - 32;

      if LContextStart < 0 then LContextStart := 0;
      LContextFinish := LLogicalCaret + 32;

      if LContextFinish > LOldProjection.PresentedScalars then
        LContextFinish := LOldProjection.PresentedScalars;
      LOldAt := ScalarStorage(LOld, LContextStart);
      LOldNext := ScalarStorage(LOld, LContextFinish);
      LContext := Copy(LOld, LOldAt, LOldNext - LOldAt);
      Result := UniqueContext(LNew, LContext, LContextAt, ACancellation);

      if Result <> pnsRelocated then Exit;
      LResolved := NyxTextScalarCount(Copy(LNew, 1, LContextAt - 1)) +
        LLogicalCaret - LContextStart;
    end;
  end;

  if Cancelled(ACancellation) then Exit(pnsCancelled);
  try
    ACaret := LNewProjection.ToAuthored(LResolved);
  except
    on ENyxTextProjection do Exit(pnsChanged);
  end;
  Result := pnsRelocated;
end;

procedure TNyxPascalNavigationPoint.Position(out ALine, AColumn: Integer);
var
  LAt, LStorage, LScalar: Integer;
  LAfterCR: Boolean;
begin
  Validate;
  ALine := 1;
  AColumn := 1;
  LStorage := 1;
  LAfterCR := False;
  for LAt := 1 to FCaret do
  begin
    NyxNextScalar(FText, LStorage, LScalar);

    if LScalar = 13 then
    begin
      Inc(ALine);
      AColumn := 1;
      LAfterCR := True;
    end
    else if LScalar = 10 then
    begin

      if not LAfterCR then Inc(ALine);
      AColumn := 1;
      LAfterCR := False;
    end
    else
    begin
      Inc(AColumn);
      LAfterCR := False;
    end;
  end;
end;

class function TNyxPascalNavigationLimits.Defaults: TNyxPascalNavigationLimits;
begin
  Result.FDefined := True;
  Result.FEntries := 128;
  Result.FBytes := 16 * 1024 * 1024;
end;

procedure TNyxPascalNavigationLimits.Validate;
begin

  if not FDefined or (FEntries < 0) or (FEntries = 1) or (FEntries > 1024) or (FBytes < 0) then
    raise ENyxPascalNavigation.Create('Navigation needs zero or two through 1024 locations and nonnegative bytes');
end;

function TNyxPascalNavigationLimits.UpToEntries(AValue: Integer): TNyxPascalNavigationLimits;
begin
  Result := Self;
  Result.FEntries := AValue;
  Result.Validate;
end;

function TNyxPascalNavigationLimits.UpToBytes(AValue: Integer): TNyxPascalNavigationLimits;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function NewNyxPascalNavigationTrail: INyxPascalNavigationTrail;
begin
  Result := NewNyxPascalNavigationTrail(TNyxPascalNavigationLimits.Defaults);
end;

function NewNyxPascalNavigationTrail(
  const ALimits: TNyxPascalNavigationLimits): INyxPascalNavigationTrail;
var
  LValue: TNavigationTrail;
begin
  ALimits.Validate;
  LValue := TNavigationTrail.Create;
  Result := LValue;
  LValue.FLimits := ALimits;
  LValue.FPosition := -1;
end;

function TNavigationTrail.Count: Integer;
begin
  Result := Length(FPoints);
end;

function TNavigationTrail.Position: Integer;
begin
  Result := FPosition;
end;

function TNavigationTrail.AccountedBytes: TNyxTextBytes;
var
  LPoint: TNyxPascalNavigationPoint;
begin
  Result := 0;
  for LPoint in FPoints do Result := Result + LPoint.RetainedBytes;
end;

function TNavigationTrail.RetainedBytes: Integer;
var
  LBytes: TNyxTextBytes;
begin
  LBytes := AccountedBytes;

  if LBytes > High(Integer) then
    raise ENyxPascalNavigation.Create('Navigation byte accounting exceeds its public range');
  Result := LBytes;
end;

function TNavigationTrail.Current: TNyxPascalNavigationPoint;
begin
  Result := Default(TNyxPascalNavigationPoint);

  if FPosition >= 0 then Result := FPoints[FPosition];
end;

function TNavigationTrail.CanTravel(ADirection: TNyxPascalNavigationDirection): Boolean;
begin
  Result := ((ADirection = pndBack) and (FPosition > 0)) or
    ((ADirection = pndForward) and (FPosition >= 0) and (FPosition + 1 < Count));
end;

function TNavigationTrail.Destination(
  ADirection: TNyxPascalNavigationDirection): TNyxPascalNavigationPoint;
var
  LAt: Integer;
begin
  Result := Default(TNyxPascalNavigationPoint);

  if not CanTravel(ADirection) then Exit;
  LAt := FPosition - 1;

  if ADirection = pndForward then LAt := FPosition + 1;
  Result := FPoints[LAt];
end;

function TNavigationTrail.CopyValue: TNavigationTrail;
begin
  Result := TNavigationTrail.Create;
  try
    Result.FLimits := FLimits;
    Result.FPoints := Copy(FPoints, 0, Count);
    Result.FPosition := FPosition;
  except
    Result.Free;
    raise;
  end;
end;

procedure TNavigationTrail.Trim(AFirstRequired, ALastRequired: Integer);
var
  LAt: Integer;
begin
  while (Count > FLimits.MaximumEntries) or (AccountedBytes > FLimits.MaximumBytes) do
  begin

    if AFirstRequired > 0 then
    begin
      for LAt := 1 to Count - 1 do FPoints[LAt - 1] := FPoints[LAt];
      SetLength(FPoints, Count - 1);
      Dec(FPosition);
      Dec(AFirstRequired);
      Dec(ALastRequired);
    end
    else if ALastRequired < Count - 1 then
      SetLength(FPoints, Count - 1)
    else
      raise ENyxPascalNavigation.Create('Navigation allowance cannot retain its immediate return pair');
  end;
end;

function TNavigationTrail.Visit(const AFrom, ATo: TNyxPascalNavigationPoint): INyxPascalNavigationTrail;
var
  LNext: TNavigationTrail;
  LHold: INyxPascalNavigationTrail;
begin
  AFrom.Validate;
  ATo.Validate;

  if (FLimits.MaximumEntries = 0) or AFrom.SameLocation(ATo) then Exit(Self);
  LNext := CopyValue;
  LHold := LNext;

  if LNext.Count = 0 then
  begin
    SetLength(LNext.FPoints, 1);
    LNext.FPosition := 0;
  end;
  SetLength(LNext.FPoints, LNext.FPosition + 2);
  LNext.FPoints[LNext.FPosition] := AFrom;
  Inc(LNext.FPosition);
  LNext.FPoints[LNext.FPosition] := ATo;
  LNext.Trim(LNext.FPosition - 1, LNext.FPosition);
  Result := LHold;
end;

function TNavigationTrail.Travel(ADirection: TNyxPascalNavigationDirection;
  const ALeaving: TNyxPascalNavigationPoint; const ATargetText: TNyxText;
  out AStatus: TNyxPascalNavigationStatus;
  const ACancellation: INyxCancellationScope): INyxPascalNavigationTrail;
var
  LNext: TNavigationTrail;
  LHold: INyxPascalNavigationTrail;
  LTarget: TNyxPascalNavigationPoint;
  LAt, LCaret, LFirst, LLast: Integer;
begin
  Result := nil;
  AStatus := pnsUnavailable;

  if not CanTravel(ADirection) then Exit;
  ALeaving.Validate;
  LTarget := Destination(ADirection);
  AStatus := LTarget.Resolve(ATargetText, LCaret, ACancellation);

  if not (AStatus in [pnsExact, pnsRelocated]) then Exit;
  LAt := FPosition - 1;

  if ADirection = pndForward then LAt := FPosition + 1;
  LTarget := NyxPascalNavigationPoint(LTarget.FileReference, ATargetText, LCaret);
  LNext := CopyValue;
  LHold := LNext;
  LNext.FPoints[FPosition] := ALeaving;
  LNext.FPoints[LAt] := LTarget;
  LNext.FPosition := LAt;
  LFirst := LAt;
  LLast := FPosition;

  if LFirst > LLast then
  begin
    LFirst := FPosition;
    LLast := LAt;
  end;
  LNext.Trim(LFirst, LLast);

  if Cancelled(ACancellation) then
  begin
    AStatus := pnsCancelled;
    Exit;
  end;
  Result := LHold;
end;

end.
