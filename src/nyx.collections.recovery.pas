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

uses nyx.collections.edits;

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
    copies before a physical draft is overwritten/removed. Retire precedes
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

{ Advisory defaults are 64 retained entries or 8MiB text. They never silently
  remove or truncate a user's draft. Applications can choose other thresholds. }
function NyxDraftRecoveryWarnings: TNyxDraftRecoveryWarnings;
function NewNyxCollectionDraftRecovery: INyxCollectionDraftRecoveryWriter;

implementation

uses nyx.bytes, nyx.collections;

type
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
    Last-entry release is ordinary managed value cleanup, never a callback. }
  TJournal = class(TInterfacedObject, INyxCollectionDraftRecovery)
  private
    FEntries: array of INyxRecoveredCollectionDraft;
    FWarnings: TNyxDraftRecoveryWarnings;
    FActive: Boolean;
    FRevision: Integer;
    procedure RequireRevision;
  public
    constructor Create;
    function Capture: TNyxDraftRecoveryObservation;
    function EntryAt(AIndex: Integer): INyxRecoveredCollectionDraft;
    function Discard(const AEntry: INyxRecoveredCollectionDraft): Boolean;
    procedure ConfigureWarnings(const AWarnings: TNyxDraftRecoveryWarnings);
    function Preserve(const ACell: TNyxCollectionCellEdit;
      AReason: TNyxCollectionDraftLoss; ABefore, AAfter: Integer): INyxRecoveredCollectionDraft;
    procedure Retire;
  end;

  { Only this private writer can add or retire. The public data interface owns
    the journal; its raw class address is borrowed while that interface lives. }
  TWriter = class(TInterfacedObject, INyxCollectionDraftRecoveryWriter)
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
    procedure Retire;
  end;

function NyxDraftRecoveryWarnings: TNyxDraftRecoveryWarnings;
begin
  Result := Default(TNyxDraftRecoveryWarnings);
  Result.FDefined := True;
  Result.FEntries := 64;
  Result.FBytes := 8 * 1024 * 1024;
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
  FActive := True;
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
      for LMove := LIndex to High(FEntries) - 1 do
      begin
        FEntries[LMove] := FEntries[LMove + 1];
      end;
      SetLength(FEntries, Length(FEntries) - 1);
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
  AReason: TNyxCollectionDraftLoss; ABefore, AAfter: Integer): INyxRecoveredCollectionDraft;
var
  LIndex: Integer;
  LCell: TNyxCollectionCellEdit;
begin

  if not FActive then
  begin
    raise ENyxCollection.Create('A retired recovery writer cannot capture another draft');
  end;
  ACell.Validate;

  if (ABefore < 0) or (AAfter < ABefore) then
  begin
    raise ENyxCollection.Create('Recovered draft revisions must describe an ordered display transition');
  end;
  { A deferred retry captures one physical draft again. Coalesce that exact
    capture, without conflating later user work from another source revision. }
  for LIndex := 0 to High(FEntries) do
  begin
    LCell := FEntries[LIndex].Cell;

    if (FEntries[LIndex].BeforeRevision = ABefore) and
      (FEntries[LIndex].Reason = AReason) and
      (LCell.Item.Collection.Name = ACell.Item.Collection.Name) and
      (LCell.Item.ID = ACell.Item.ID) and LCell.Field.Same(ACell.Field) and
      LCell.Accepted.SameValue(ACell.Accepted) and (LCell.Text = ACell.Text) and
      (LCell.Selection.Defined = ACell.Selection.Defined) then
    begin

      if not LCell.Selection.Defined or LCell.Selection.SameRange(ACell.Selection) then
      begin
        Exit(FEntries[LIndex]);
      end;
    end;
  end;
  RequireRevision;
  Result := TRecovered.Create(ACell, AReason, ABefore, AAfter);
  SetLength(FEntries, Length(FEntries) + 1);
  FEntries[High(FEntries)] := Result;
  Inc(FRevision);
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
  Result := FJournal.Preserve(ACell, AReason, ABeforeRevision, AAfterRevision);
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
