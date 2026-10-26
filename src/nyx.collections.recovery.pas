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
unit nyx.collections.recovery;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.edits;

type
  { Recovery explains why a dirty physical draft lost automatic compatibility.
    These reasons never authorize applying old text to a current source row. }
  TNyxCollectionDraftLoss = (ncdlRowRemoved, ncdlIdentityReplaced, ncdlHidden,
    ncdlValueChanged, ncdlFieldUnavailable, ncdlUnknownIdentity);

  { Warning thresholds are advisory, not truncation/expiry rules. Zero disables
    that warning. User-authored text is retained until explicitly discarded or
    its last journal/entry owner releases it; no update queue is retained. }
  TNyxDraftRecoveryWarnings = record
  private
    FDefined: Boolean;
    FEntries: Integer;
    FBytes: Integer;
  public
    function Entries(AValue: Integer): TNyxDraftRecoveryWarnings;
    function Bytes(AValue: Integer): TNyxDraftRecoveryWarnings;
    procedure Validate;
    property EntryThreshold: Integer read FEntries;
    property ByteThreshold: Integer read FBytes;
  end;

  { Hard admission limits for this journal's owned entries and logical UTF-8
    text payload: draft, accepted text, collection/item/field names. Entry count
    bounds fixed record/interface overhead separately. Active editors, caller-
    retained discarded entries and host allocation overhead are outside these
    limits. Zero refuses new storage; existing entries are never evicted. }
  TNyxDraftRecoveryLimits = record
  private
    FDefined: Boolean;
    FEntries: Integer;
    FPayloadBytes: Integer;
  public
    function Entries(AValue: Integer): TNyxDraftRecoveryLimits;
    function PayloadBytes(AValue: Integer): TNyxDraftRecoveryLimits;
    procedure Validate;
    property MaximumEntries: Integer read FEntries;
    property MaximumPayloadBytes: Integer read FPayloadBytes;
  end;

  { A refusal describes the first exceeded resource, never an eviction choice.
    Both resources are still checked when reporting whether a retry can fit. }
  TNyxDraftRecoveryPressure = (ncrpNone, ncrpEntryCount, ncrpPayloadBytes);

  { Constant-size receipt owns no rejected text, cell, mount or callback.
    LastRefusal persists until a successful capture; changing capacity alone
    never claims that a held physical display has already been refreshed.
    RequiredPayloadBytes is the rejected ENTRY's size, saturated explicitly. }
  TNyxDraftRecoveryBudgetObservation = record
    Active: Boolean;
    Revision: Integer;
    Limits: TNyxDraftRecoveryLimits;
    Count: Integer;
    PayloadBytes: Integer;
    LastRefusal: TNyxDraftRecoveryPressure;
    RequiredPayloadBytes: Integer;
    RequiredPayloadSaturated: Boolean;
    CanRetry: Boolean;
  end;

  { Optional configuration/observation facet of the same data journal. Lowering
    below current ownership refuses atomically. No capture writer, source or
    physical refresh authority can be acquired through this interface. }
  INyxCollectionDraftRecoveryBudget = interface(IInterface)
    ['{9CF7C02B-BEDD-43CE-B41B-33A21A3F821B}']
    function CaptureBudget: TNyxDraftRecoveryBudgetObservation;
    procedure ConfigureLimits(const ALimits: TNyxDraftRecoveryLimits);
  end;

  { Admission refusal keeps the journal's prior entries intact. The caller MUST
    retain its original dirty cell/physical draft and must not close/overwrite
    it after this exception. Committed source updates are not rolled back. }
  ENyxDraftRecoveryPressure = class(ENyxCollection)
  private
    FBudget: TNyxDraftRecoveryBudgetObservation;
  public
    constructor Create(const ABudget: TNyxDraftRecoveryBudgetObservation);
    property Budget: TNyxDraftRecoveryBudgetObservation read FBudget;
  end;

  { Immutable exact text/caret and accepted-value provenance. The copied cell
    carries no editing/focus authority. No store, snapshot, mount, router,
    callback, document or widget is retained. Retain an entry independently. }
  INyxRecoveredCollectionDraft = interface(IInterface)
    ['{2899FA97-4A1A-4594-BE71-E31DE871263F}']
    function GetCell: TNyxCollectionCellEdit;
    function GetReason: TNyxCollectionDraftLoss;
    function GetBeforeRevision: Integer;
    function GetAfterRevision: Integer;
    function GetByteCount: Integer;
    property Cell: TNyxCollectionCellEdit read GetCell;
    property Reason: TNyxCollectionDraftLoss read GetReason;
    property BeforeRevision: Integer read GetBeforeRevision;
    property AfterRevision: Integer read GetAfterRevision;
    property ByteCount: Integer read GetByteCount;
  end;

  { Bounded observation response, not a complete journal dump. RetainedBytes
    counts exact UTF-8 draft text only and saturates explicitly at High(Integer).
    Widget/schema/caret/interface overhead is not a total-memory guarantee. }
  TNyxDraftRecoveryObservation = record
    Active: Boolean;
    Revision: Integer;
    Count: Integer;
    RetainedBytes: Integer;
    BytesSaturated: Boolean;
    Warning: Boolean;
  end;

  { UI-owner data journal. Read entries individually; no mutable vector escapes.
    Discard requires the exact retained entry interface, not a possibly stale
    row index. Retired journals remain readable/discardable. Capture immutable
    entries on UI before handing facts to workers. No method writes source data. }
  INyxCollectionDraftRecovery = interface(IInterface)
    ['{1E7B0598-7B30-4FBE-9951-03ACBFC4435F}']
    function Capture: TNyxDraftRecoveryObservation;
    function EntryAt(AIndex: Integer): INyxRecoveredCollectionDraft;
    function Discard(const AEntry: INyxRecoveredCollectionDraft): Boolean;
    procedure ConfigureWarnings(const AWarnings: TNyxDraftRecoveryWarnings);
  end;

  { The mounted writer owns lifecycle/capture authority separately from readers.
    The returned journal cannot QueryInterface back to this writer. Preserve
    copies before a physical draft is overwritten/removed. Capacity refusal
    raises ENyxDraftRecoveryPressure before admission; keep the original draft
    until an explicit successful retry. Retire precedes
    teardown; already retained data stays independently readable. }
  INyxCollectionDraftRecoveryWriter = interface(IInterface)
    ['{26EDC084-092F-41C8-A57B-50B27D391E69}']
    function GetRecovery: INyxCollectionDraftRecovery;
    function Preserve(const ACell: TNyxCollectionCellEdit;
      AReason: TNyxCollectionDraftLoss; ABeforeRevision, AAfterRevision: Integer):
      INyxRecoveredCollectionDraft;
    procedure Retire;
    property Recovery: INyxCollectionDraftRecovery read GetRecovery;
  end;

  { Opaque identity for one physical recovery pass and its retries. Only the
    factory creates its private inert marker; copying retains no snapshot,
    store, control or executable callback. Undefined is never a scoped frame.
    A new successfully installed frame must use a fresh identity. Native copies
    reference-count the marker. The matched pas2js cannot put a COM interface
    in a record, so browser copies keep an inert GC object; never free it while
    another copy exists. Neither representation owns external resources. }
  TNyxDraftRecoveryFrame = record
  private
    {$ifdef PAS2JS}
    FIdentity: TObject;
    {$else}
    FIdentity: IInterface;
    {$endif}
    function GetDefined: Boolean;
  public
    function Same(const AOther: TNyxDraftRecoveryFrame): Boolean;
    property Defined: Boolean read GetDefined;
  end;

  { Optional writer capability coalesces retries inside an exact physical
    frame, even when later source activity changes the loss classification.
    New frames remain independent at the same numeric source revision. The
    first receipt is immutable. The readonly journal cannot acquire this writer. }
  INyxCollectionDraftRecoveryScopedWriter = interface(IInterface)
    ['{EF70052A-37C9-438C-A05D-72B40F2295DC}']
    function PreserveInFrame(const ACell: TNyxCollectionCellEdit;
      AReason: TNyxCollectionDraftLoss; ABeforeRevision, AAfterRevision: Integer;
      const AFrame: TNyxDraftRecoveryFrame): INyxRecoveredCollectionDraft;
  end;

  { Detached review of one refused physical draft. Frame is an inert identity,
    not a mount lease. Text/caret/provenance remain readable after retirement;
    only the originating live mount can compare and discard its current draft.
    Reviewing/exporting never admits this value to the bounded journal. }
  INyxPendingCollectionDraft = interface(IInterface)
    ['{6D04D597-F281-4118-AC07-95B92F8D12BF}']
    function GetCell: TNyxCollectionCellEdit;
    function GetFrame: TNyxDraftRecoveryFrame;
    property Cell: TNyxCollectionCellEdit read GetCell;
    property Frame: TNyxDraftRecoveryFrame read GetFrame;
  end;

{ Advisory defaults are 64 retained entries or 8MiB text. They never silently
  remove or truncate a user's draft. Applications can choose other thresholds. }
function NyxDraftRecoveryWarnings: TNyxDraftRecoveryWarnings;
function NewNyxDraftRecoveryFrame: TNyxDraftRecoveryFrame;
{ Detached review data for adapter authors. This never authorizes source writes
  or journal admission. Undefined frames and invalid cells refuse. }
function NewNyxPendingCollectionDraft(const ACell: TNyxCollectionCellEdit;
  const AFrame: TNyxDraftRecoveryFrame): INyxPendingCollectionDraft;
{ Compare reviewable text/provenance, ignoring focus/editing flags because a
  review button may legitimately take focus. Caret changes invalidate review. }
function NyxSameCollectionDraft(const ALeft, ARight: TNyxCollectionCellEdit): Boolean;
{ Finite defaults: 256 entries and 16MiB logical retained text/provenance. }
function NyxDraftRecoveryLimits: TNyxDraftRecoveryLimits;
{ Exact retained text metric, without constructing another entry. Overflow is
  explicit and can never fit any policy. Fixed scalars are bounded by entries. }
function NyxDraftRecoveryPayloadBytes(const ACell: TNyxCollectionCellEdit;
  out ASaturated: Boolean): Integer;
{ Alternative journal implementations may return nil. This facet owns data
  only; retry a held target through its existing INyxCollectionMount.Refresh. }
function NyxCollectionDraftRecoveryBudget(const ARecovery: INyxCollectionDraftRecovery):
  INyxCollectionDraftRecoveryBudget;
function NewNyxCollectionDraftRecovery: INyxCollectionDraftRecoveryWriter;

implementation

uses SysUtils, nyx.bytes, nyx.text, nyx.state, nyx.collections.recovery.policy;

type
  { Owns exact review data and an inert frame marker; no target/source/callback
    survives through this receipt. Cell getters detach managed scalar records. }
  TPendingDraft = class(TInterfacedObject, INyxPendingCollectionDraft)
  private
    FCell: TNyxCollectionCellEdit;
    FFrame: TNyxDraftRecoveryFrame;
  public
    constructor Create(const ACell: TNyxCollectionCellEdit;
      const AFrame: TNyxDraftRecoveryFrame);
    function GetCell: TNyxCollectionCellEdit;
    function GetFrame: TNyxDraftRecoveryFrame;
  end;

  { Owns one validated inert value. Interface getters copy records so callers
    cannot change the journal's stored text/provenance/caret. }
  TRecovered = class(TInterfacedObject, INyxRecoveredCollectionDraft)
  private
    FCell: TNyxCollectionCellEdit;
    FReason: TNyxCollectionDraftLoss;
    FBefore: Integer;
    FAfter: Integer;
    FBytes: Integer;
  public
    constructor Create(const ACell: TNyxCollectionCellEdit;
      AReason: TNyxCollectionDraftLoss; ABefore, AAfter: Integer);
    function GetCell: TNyxCollectionCellEdit;
    function GetReason: TNyxCollectionDraftLoss;
    function GetBeforeRevision: Integer;
    function GetAfterRevision: Integer;
    function GetByteCount: Integer;
  end;

  { Data owner has no pointer back to its capture writer or any physical target.
    Hard admission bounds its entries and all retained text/provenance. Refusal
    retains only scalar diagnostics. Last-entry release invokes no callback. }
  TJournal = class(TInterfacedObject, INyxCollectionDraftRecovery,
    INyxCollectionDraftRecoveryBudget)
  private
    FEntries: array of INyxRecoveredCollectionDraft;
    FEntryBytes: array of Integer;
    FEntryFrames: array of TNyxDraftRecoveryFrame;
    FWarnings: TNyxDraftRecoveryWarnings;
    FActive: Boolean;
    FRevision: Integer;
    FLimits: TNyxDraftRecoveryLimits;
    FPayloadBytes: Integer;
    FRefusal: TNyxDraftRecoveryPressure;
    FRequiredBytes: Integer;
    FRequiredSaturated: Boolean;
    procedure RequireRevision;
    procedure ClearRefusal;
  public
    constructor Create;
    function Capture: TNyxDraftRecoveryObservation;
    function EntryAt(AIndex: Integer): INyxRecoveredCollectionDraft;
    function Discard(const AEntry: INyxRecoveredCollectionDraft): Boolean;
    procedure ConfigureWarnings(const AWarnings: TNyxDraftRecoveryWarnings);
    function CaptureBudget: TNyxDraftRecoveryBudgetObservation;
    procedure ConfigureLimits(const ALimits: TNyxDraftRecoveryLimits);
    function Preserve(const ACell: TNyxCollectionCellEdit;
      AReason: TNyxCollectionDraftLoss; ABefore, AAfter: Integer;
      const AFrame: TNyxDraftRecoveryFrame): INyxRecoveredCollectionDraft;
    procedure Retire;
  end;

  { Only this private writer can add or retire. The public data interface owns
    the journal; its raw class address is borrowed while that interface lives. }
  TWriter = class(TInterfacedObject, INyxCollectionDraftRecoveryWriter,
    INyxCollectionDraftRecoveryScopedWriter)
  private
    FRecovery: INyxCollectionDraftRecovery;
    FJournal: TJournal;
  public
    constructor Create;
    destructor Destroy; override;
    function GetRecovery: INyxCollectionDraftRecovery;
    function Preserve(const ACell: TNyxCollectionCellEdit;
      AReason: TNyxCollectionDraftLoss; ABeforeRevision, AAfterRevision: Integer):
      INyxRecoveredCollectionDraft;
    function PreserveInFrame(const ACell: TNyxCollectionCellEdit;
      AReason: TNyxCollectionDraftLoss; ABeforeRevision, AAfterRevision: Integer;
      const AFrame: TNyxDraftRecoveryFrame): INyxRecoveredCollectionDraft;
    procedure Retire;
  end;

constructor TPendingDraft.Create(const ACell: TNyxCollectionCellEdit;
  const AFrame: TNyxDraftRecoveryFrame);
begin
  inherited Create;
  ACell.Validate;

  if not AFrame.Defined then
  begin
    raise ENyxCollection.Create('A pending draft requires its physical frame');
  end;
  FCell := ACell.Copy;
  FFrame := AFrame;
end;

function TPendingDraft.GetCell: TNyxCollectionCellEdit;
begin
  Result := FCell.Copy;
end;

function TPendingDraft.GetFrame: TNyxDraftRecoveryFrame;
begin
  Result := FFrame;
end;

function NewNyxPendingCollectionDraft(const ACell: TNyxCollectionCellEdit;
  const AFrame: TNyxDraftRecoveryFrame): INyxPendingCollectionDraft;
begin
  Result := TPendingDraft.Create(ACell, AFrame);
end;

function NyxSameCollectionDraft(const ALeft, ARight: TNyxCollectionCellEdit): Boolean;
begin
  Result := ALeft.Defined and ARight.Defined and
    (ALeft.Item.Collection.Name = ARight.Item.Collection.Name) and
    (ALeft.Item.ID = ARight.Item.ID) and ALeft.Field.Same(ARight.Field) and
    ALeft.Accepted.SameValue(ARight.Accepted) and (ALeft.Text = ARight.Text) and
    (ALeft.Selection.Defined = ARight.Selection.Defined);

  if Result and ALeft.Selection.Defined then
  begin
    Result := ALeft.Selection.SameRange(ARight.Selection);
  end;
end;

function NyxDraftRecoveryWarnings: TNyxDraftRecoveryWarnings;
begin
  Result := Default(TNyxDraftRecoveryWarnings);
  Result.FDefined := True;
  Result.FEntries := NyxDefaultDraftRecoveryWarningEntries;
  Result.FBytes := NyxDefaultDraftRecoveryWarningTextBytes;
end;

function NewNyxDraftRecoveryFrame: TNyxDraftRecoveryFrame;
begin
  Result := Default(TNyxDraftRecoveryFrame);
  {$ifdef PAS2JS}
  Result.FIdentity := TObject.Create;
  {$else}
  Result.FIdentity := TInterfacedObject.Create;
  {$endif}
end;

function TNyxDraftRecoveryFrame.GetDefined: Boolean;
begin
  Result := FIdentity <> nil;
end;

function TNyxDraftRecoveryFrame.Same(const AOther: TNyxDraftRecoveryFrame): Boolean;
begin
  Result := Defined and AOther.Defined and (FIdentity = AOther.FIdentity);
end;

function NyxDraftRecoveryLimits: TNyxDraftRecoveryLimits;
begin
  Result := Default(TNyxDraftRecoveryLimits);
  Result.FDefined := True;
  Result.FEntries := NyxDefaultDraftRecoveryEntries;
  Result.FPayloadBytes := NyxDefaultDraftRecoveryPayloadBytes;
end;

procedure TNyxDraftRecoveryLimits.Validate;
begin

  if not FDefined or (FEntries < 0) or (FPayloadBytes < 0) then
  begin
    raise ENyxCollection.Create('Create nonnegative draft recovery limits');
  end;
end;

function TNyxDraftRecoveryLimits.Entries(AValue: Integer): TNyxDraftRecoveryLimits;
begin
  Result := Self;
  Result.FEntries := AValue;
  Result.Validate;
end;

function TNyxDraftRecoveryLimits.PayloadBytes(AValue: Integer): TNyxDraftRecoveryLimits;
begin
  Result := Self;
  Result.FPayloadBytes := AValue;
  Result.Validate;
end;

function NyxDraftRecoveryPayloadBytes(const ACell: TNyxCollectionCellEdit;
  out ASaturated: Boolean): Integer;
var
  LBytes: Integer;
  LAccepted: TNyxStateValue;

  procedure Add(const AText: TNyxText);
  var
    LPart: Integer;
  begin
    LPart := NyxUTF8ByteCount(AText);

    if LPart > High(Integer) - LBytes then
    begin
      LBytes := High(Integer);
      ASaturated := True;
    end
    else
    begin
      Inc(LBytes, LPart);
    end;
  end;

begin
  ACell.Validate;
  ASaturated := False;
  LBytes := 0;
  Add(ACell.Text);
  Add(ACell.Item.Collection.Name);
  Add(ACell.Item.ID);
  Add(ACell.Field.Name);
  LAccepted := ACell.Accepted;

  if LAccepted.Kind = nskText then
  begin
    Add(LAccepted.TextValue);
  end;
  Result := LBytes;
end;

function NyxCollectionDraftRecoveryBudget(const ARecovery: INyxCollectionDraftRecovery):
  INyxCollectionDraftRecoveryBudget;
begin
  Result := nil;

  if ARecovery <> nil then
  begin
    Supports(ARecovery, INyxCollectionDraftRecoveryBudget, Result);
  end;
end;

constructor ENyxDraftRecoveryPressure.Create(const ABudget: TNyxDraftRecoveryBudgetObservation);
begin
  inherited Create('Recovery capacity reached; retain the visible draft, review entries or raise limits, then retry');
  FBudget := ABudget;
end;

procedure TNyxDraftRecoveryWarnings.Validate;
begin

  if not FDefined or (FEntries < 0) or (FBytes < 0) then
  begin
    raise ENyxCollection.Create('Create nonnegative draft recovery warning thresholds');
  end;
end;

function TNyxDraftRecoveryWarnings.Entries(AValue: Integer): TNyxDraftRecoveryWarnings;
begin
  Result := Self;
  Result.FEntries := AValue;
  Result.Validate;
end;

function TNyxDraftRecoveryWarnings.Bytes(AValue: Integer): TNyxDraftRecoveryWarnings;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

constructor TRecovered.Create(const ACell: TNyxCollectionCellEdit;
  AReason: TNyxCollectionDraftLoss; ABefore, AAfter: Integer);
begin
  inherited Create;
  ACell.Validate;

  if (ABefore < 0) or (AAfter < ABefore) then
  begin
    raise ENyxCollection.Create('Recovered draft revisions must describe an ordered display transition');
  end;
  FCell := NyxCollectionCellEdit(ACell.Item, ACell.Field, ACell.Accepted,
    ACell.Text, ACell.Selection, False, False);
  FReason := AReason;
  FBefore := ABefore;
  FAfter := AAfter;
  FBytes := NyxUTF8ByteCount(FCell.Text);
end;

function TRecovered.GetCell: TNyxCollectionCellEdit;
begin
  Result := FCell.Copy;
end;

function TRecovered.GetReason: TNyxCollectionDraftLoss;
begin
  Result := FReason;
end;

function TRecovered.GetBeforeRevision: Integer;
begin
  Result := FBefore;
end;

function TRecovered.GetAfterRevision: Integer;
begin
  Result := FAfter;
end;

function TRecovered.GetByteCount: Integer;
begin
  Result := FBytes;
end;

constructor TJournal.Create;
begin
  inherited Create;
  FWarnings := NyxDraftRecoveryWarnings;
  FLimits := NyxDraftRecoveryLimits;
  FActive := True;
end;

function TJournal.CaptureBudget: TNyxDraftRecoveryBudgetObservation;
begin
  Result := Default(TNyxDraftRecoveryBudgetObservation);
  Result.Active := FActive;
  Result.Revision := FRevision;
  Result.Limits := FLimits;
  Result.Count := Length(FEntries);
  Result.PayloadBytes := FPayloadBytes;
  Result.LastRefusal := FRefusal;
  Result.RequiredPayloadBytes := FRequiredBytes;
  Result.RequiredPayloadSaturated := FRequiredSaturated;
  Result.CanRetry := FActive and (FRefusal <> ncrpNone) and not FRequiredSaturated and
    (Length(FEntries) < FLimits.MaximumEntries) and
    (FRequiredBytes <= FLimits.MaximumPayloadBytes - FPayloadBytes);
end;

procedure TJournal.ConfigureLimits(const ALimits: TNyxDraftRecoveryLimits);
begin
  ALimits.Validate;

  if (Length(FEntries) > ALimits.MaximumEntries) or
    (FPayloadBytes > ALimits.MaximumPayloadBytes) then
  begin
    raise ENyxCollection.Create('Recovery limits cannot discard already retained entries');
  end;
  RequireRevision;
  FLimits := ALimits;
  Inc(FRevision);
end;

procedure TJournal.ClearRefusal;
begin

  if FRefusal <> ncrpNone then
  begin
    RequireRevision;
    FRefusal := ncrpNone;
    FRequiredBytes := 0;
    FRequiredSaturated := False;
    Inc(FRevision);
  end;
end;

procedure TJournal.RequireRevision;
begin

  if FRevision = High(Integer) then
  begin
    raise ENyxCollection.Create('The recovery journal revision is exhausted');
  end;
end;

function TJournal.Capture: TNyxDraftRecoveryObservation;
var
  LIndex: Integer;
  LBytes: Integer;
begin
  Result := Default(TNyxDraftRecoveryObservation);
  Result.Active := FActive;
  Result.Revision := FRevision;
  Result.Count := Length(FEntries);
  for LIndex := 0 to High(FEntries) do
  begin
    LBytes := FEntries[LIndex].ByteCount;

    if LBytes > High(Integer) - Result.RetainedBytes then
    begin
      Result.RetainedBytes := High(Integer);
      Result.BytesSaturated := True;
      Break;
    end;
    Inc(Result.RetainedBytes, LBytes);
  end;
  Result.Warning := ((FWarnings.EntryThreshold > 0) and
    (Result.Count >= FWarnings.EntryThreshold)) or
    ((FWarnings.ByteThreshold > 0) and (Result.RetainedBytes >= FWarnings.ByteThreshold));
end;

function TJournal.EntryAt(AIndex: Integer): INyxRecoveredCollectionDraft;
begin

  if (AIndex < 0) or (AIndex >= Length(FEntries)) then
  begin
    raise ENyxCollection.Create('Recovered draft index is outside the journal');
  end;
  Result := FEntries[AIndex];
end;

function TJournal.Discard(const AEntry: INyxRecoveredCollectionDraft): Boolean;
var
  LIndex: Integer;
  LMove: Integer;
begin
  Result := False;
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex] = AEntry then
    begin
      RequireRevision;
      Dec(FPayloadBytes, FEntryBytes[LIndex]);
      for LMove := LIndex to High(FEntries) - 1 do
      begin
        FEntries[LMove] := FEntries[LMove + 1];
        FEntryBytes[LMove] := FEntryBytes[LMove + 1];
        FEntryFrames[LMove] := FEntryFrames[LMove + 1];
      end;
      SetLength(FEntries, Length(FEntries) - 1);
      SetLength(FEntryBytes, Length(FEntryBytes) - 1);
      SetLength(FEntryFrames, Length(FEntryFrames) - 1);
      Inc(FRevision);
      Exit(True);
    end;
  end;
end;

procedure TJournal.ConfigureWarnings(const AWarnings: TNyxDraftRecoveryWarnings);
begin
  AWarnings.Validate;
  RequireRevision;
  FWarnings := AWarnings;
  Inc(FRevision);
end;

function TJournal.Preserve(const ACell: TNyxCollectionCellEdit;
  AReason: TNyxCollectionDraftLoss; ABefore, AAfter: Integer;
  const AFrame: TNyxDraftRecoveryFrame): INyxRecoveredCollectionDraft;
var
  LIndex: Integer;
  LCell: TNyxCollectionCellEdit;
  LBytes: Integer;
  LSaturated: Boolean;
  LPressure: TNyxDraftRecoveryPressure;
  LNext: array of INyxRecoveredCollectionDraft;
  LNextBytes: array of Integer;
  LNextFrames: array of TNyxDraftRecoveryFrame;
  LEntry: INyxRecoveredCollectionDraft;
begin

  if not FActive then
  begin
    raise ENyxCollection.Create('A retired recovery writer cannot capture another draft');
  end;
  ACell.Validate;

  if (Ord(AReason) < Ord(Low(TNyxCollectionDraftLoss))) or
    (Ord(AReason) > Ord(High(TNyxCollectionDraftLoss))) then
  begin
    raise ENyxCollection.Create('Recovered draft loss requires a known reason');
  end;

  if (ABefore < 0) or (AAfter < ABefore) then
  begin
    raise ENyxCollection.Create('Recovered draft revisions must describe an ordered display transition');
  end;
  { Scoped retries keep the first immutable receipt even if later activity
    weakens the loss classification. Another installed frame is independent,
    including a later edit at the SAME source revision. Unscoped public callers
    retain the former reason-qualified coalescing contract. }
  for LIndex := 0 to High(FEntries) do
  begin
    LCell := FEntries[LIndex].Cell;

    if (FEntries[LIndex].BeforeRevision = ABefore) and
      (AFrame.Same(FEntryFrames[LIndex]) or
        (not AFrame.Defined and not FEntryFrames[LIndex].Defined and
          (FEntries[LIndex].Reason = AReason))) and
      (LCell.Item.Collection.Name = ACell.Item.Collection.Name) and
      (LCell.Item.ID = ACell.Item.ID) and LCell.Field.Same(ACell.Field) and
      LCell.Accepted.SameValue(ACell.Accepted) and (LCell.Text = ACell.Text) and
      (LCell.Selection.Defined = ACell.Selection.Defined) then
    begin

      if not LCell.Selection.Defined or LCell.Selection.SameRange(ACell.Selection) then
      begin
        ClearRefusal;
        Exit(FEntries[LIndex]);
      end;
    end;
  end;
  LBytes := NyxDraftRecoveryPayloadBytes(ACell, LSaturated);
  LPressure := ncrpNone;

  if Length(FEntries) >= FLimits.MaximumEntries then
  begin
    LPressure := ncrpEntryCount;
  end
  else if LSaturated or (LBytes > FLimits.MaximumPayloadBytes - FPayloadBytes) then
  begin
    LPressure := ncrpPayloadBytes;
  end;

  if LPressure <> ncrpNone then
  begin

    if (FRefusal <> LPressure) or (FRequiredBytes <> LBytes) or
      (FRequiredSaturated <> LSaturated) then
    begin
      RequireRevision;
      FRefusal := LPressure;
      FRequiredBytes := LBytes;
      FRequiredSaturated := LSaturated;
      Inc(FRevision);
    end;
    raise ENyxDraftRecoveryPressure.Create(CaptureBudget);
  end;
  RequireRevision;
  { Build both private vectors before publishing either. Allocation failure
    cannot leave a nil admitted entry or a payload ledger of a different size. }
  LEntry := TRecovered.Create(ACell, AReason, ABefore, AAfter);
  SetLength(LNext, Length(FEntries) + 1);
  SetLength(LNextBytes, Length(FEntryBytes) + 1);
  SetLength(LNextFrames, Length(FEntryFrames) + 1);
  for LIndex := 0 to High(FEntries) do
  begin
    LNext[LIndex] := FEntries[LIndex];
    LNextBytes[LIndex] := FEntryBytes[LIndex];
    LNextFrames[LIndex] := FEntryFrames[LIndex];
  end;
  LNext[High(LNext)] := LEntry;
  LNextBytes[High(LNextBytes)] := LBytes;
  LNextFrames[High(LNextFrames)] := AFrame;
  FEntries := LNext;
  FEntryBytes := LNextBytes;
  FEntryFrames := LNextFrames;
  Inc(FPayloadBytes, LBytes);
  FRefusal := ncrpNone;
  FRequiredBytes := 0;
  FRequiredSaturated := False;
  Inc(FRevision);
  Result := LEntry;
end;

procedure TJournal.Retire;
begin

  if FActive and (FRevision < High(Integer)) then
  begin
    Inc(FRevision);
  end;
  FActive := False;
end;

constructor TWriter.Create;
begin
  inherited Create;
  FJournal := TJournal.Create;
  FRecovery := FJournal;
end;

destructor TWriter.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function TWriter.GetRecovery: INyxCollectionDraftRecovery;
begin
  Result := FRecovery;
end;

function TWriter.Preserve(const ACell: TNyxCollectionCellEdit;
  AReason: TNyxCollectionDraftLoss; ABeforeRevision, AAfterRevision: Integer):
  INyxRecoveredCollectionDraft;
begin
  Result := FJournal.Preserve(ACell, AReason, ABeforeRevision, AAfterRevision,
    Default(TNyxDraftRecoveryFrame));
end;

function TWriter.PreserveInFrame(const ACell: TNyxCollectionCellEdit;
  AReason: TNyxCollectionDraftLoss; ABeforeRevision, AAfterRevision: Integer;
  const AFrame: TNyxDraftRecoveryFrame): INyxRecoveredCollectionDraft;
begin

  if not AFrame.Defined then
  begin
    raise ENyxCollection.Create('Scoped recovery requires an exact defined physical frame');
  end;
  Result := FJournal.Preserve(ACell, AReason, ABeforeRevision, AAfterRevision, AFrame);
end;

procedure TWriter.Retire;
begin

  if FJournal <> nil then
  begin
    FJournal.Retire;
  end;
end;

function NewNyxCollectionDraftRecovery: INyxCollectionDraftRecoveryWriter;
begin
  Result := TWriter.Create;
end;

end.
