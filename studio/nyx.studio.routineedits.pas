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
unit nyx.studio.routineedits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.source, nyx.pascal.project, nyx.pascal.drafts,
  nyx.studio.sourceedits, nyx.studio.session, nyx.studio.projects;

const
  NyxMaximumRoutineChanges = 16;
  NyxMaximumRoutineCharacters = 32768;
  NyxMaximumRoutinePatchCharacters = 131072;

type
  { An implementation edit has a distinct Pascal routine reference and exact
    expected text. Signature, imports, sibling helpers and the managed builder
    are retained. Strings here are authored Pascal, not property/behavior tags. }
  TNyxRoutineEdit = record
  private
    FRoutine: TNyxRoutineRef;
    FExpected: TNyxText;
    FImplementation: TNyxText;
  public
    property Routine: TNyxRoutineRef read FRoutine;
    property Expected: TNyxText read FExpected;
    property Code: TNyxText read FImplementation;
  end;
  { Copied publication feedback; a line belongs to the newly admitted main
    companion, never a retained lexer offset or another helper file. }
  TNyxRoutineEditResult = record
    Routine: TNyxRoutineRef;
    SourceLine: Integer;
  end;
  { Ordered owned feedback for one legacy main-companion body group. }
  TNyxRoutineEditResults = array of TNyxRoutineEditResult;

  { Immutable, reference-counted commands retain no supplier session/document.
    Candidate validates a detached complete companion and returns owned paired
    text. Caller checks response size before one live AdoptProject publication.
    Rejected/no-op edits never clear drafts, replace selections or spend history. }
  INyxRoutinePatch = interface(IInterface)
    ['{CA41F25C-E3F9-4815-9CA2-001005003001}']
    function Candidate(ASession: TNyxStudioSession;
      out AResults: TNyxRoutineEditResults): TNyxProjectPair;
  end;

  { Exact body intent scoped by an owning main/helper unit. The index resolves
    physical include ownership; callers never manufacture offsets or silently
    reinterpret a fragment as an independent Pascal unit. Values own copied text. }
  TNyxProjectRoutineEdit = record
  private
    FOwner: TNyxPascalSourceRef;
    FEdit: TNyxRoutineEdit;
  public
    property Owner: TNyxPascalSourceRef read FOwner;
    property Edit: TNyxRoutineEdit read FEdit;
  end;

function NyxRoutineEdit(const ARoutine: TNyxRoutineRef;
  const AExpected, AImplementation: TNyxText): TNyxRoutineEdit;
function NewNyxRoutinePatch(const AChanges: array of TNyxRoutineEdit): INyxRoutinePatch;
{ JSON is the explicit semantic transport boundary. Every change has exactly
  routine/expected/implementation fields; wrong types or unknown keys refuse. }
function ReadNyxRoutinePatch(const AChanges: TNyxDataValue): INyxRoutinePatch;
function EncodeNyxRoutineResults(const AResults: TNyxRoutineEditResults): TNyxDataValue;
{ Detached source-only preparation. One complete owning compiler must construct
  the returned candidate before publication. Duplicate/ambiguous/conditional or
  physically split bodies refuse; supplied code cannot escape into siblings or
  signatures. Ordinary mixed-file ranges remain the explicit broader boundary. }
function NyxProjectRoutineEdit(const AOwner: TNyxPascalSourceRef;
  const AEdit: TNyxRoutineEdit): TNyxProjectRoutineEdit;
function NewNyxProjectRoutinePatch(const AChanges: array of TNyxProjectRoutineEdit): INyxSourceProjectPatch;

implementation

uses
  nyx.model, nyx.codec;

type
  { Owns immutable routine/owner intents. Indexes and range candidates are local
    preparation scratch, never authority or references back to a live session. }
  TNyxProjectRoutinePatch = class(TInterfacedObject, INyxSourceProjectPatch)
  private
    FChanges: array of TNyxProjectRoutineEdit;
  public
    constructor Create(const AChanges: array of TNyxProjectRoutineEdit);
    function PrepareProject(const AProject: INyxPascalProject): INyxPascalProject;
  end;

  { Legacy literal-companion admission owns only copied exact body intents.
    It prepares through a detached workspace and borrows the session per call. }
  TNyxRoutinePatch = class(TInterfacedObject, INyxRoutinePatch)
  private
    FChanges: array of TNyxRoutineEdit;
  public
    constructor Create(const AChanges: array of TNyxRoutineEdit);
    function Candidate(ASession: TNyxStudioSession;
      out AResults: TNyxRoutineEditResults): TNyxProjectPair;
  end;

function CharacterCount(const AText: TNyxText): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  Result := 0;
  LIndex := 1;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Malformed Unicode in routine implementation');
    end;
    Inc(Result);

    if Result > NyxMaximumRoutineCharacters then
    begin
      raise ENyxModel.Create('Routine implementation exceeds 32768 Unicode characters');
    end;
  end;
end;

function NyxRoutineEdit(const ARoutine: TNyxRoutineRef;
  const AExpected, AImplementation: TNyxText): TNyxRoutineEdit;
begin
  Result := Default(TNyxRoutineEdit);
  Result.FRoutine := NyxRoutine(ARoutine.Name);
  CharacterCount(AExpected);
  CharacterCount(AImplementation);
  Result.FExpected := AExpected;
  Result.FImplementation := AImplementation;
end;

function NyxProjectRoutineEdit(const AOwner: TNyxPascalSourceRef;
  const AEdit: TNyxRoutineEdit): TNyxProjectRoutineEdit;
begin
  AOwner.Validate;

  if AOwner.Role = npsInclude then
  begin
    raise ENyxModel.Create('A routine edit requires the owning main/helper unit');
  end;
  Result.FOwner := AOwner;
  Result.FEdit := NyxRoutineEdit(AEdit.Routine, AEdit.Expected, AEdit.Code);
end;

constructor TNyxProjectRoutinePatch.Create(const AChanges: array of TNyxProjectRoutineEdit);
var
  LIndex: Integer;
  LPrior: Integer;
  LCharacters: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumRoutineChanges) then
  begin
    raise ENyxModel.Create('Project routine patches require 1..16 changes');
  end;
  SetLength(FChanges, Length(AChanges));
  LCharacters := 0;
  for LIndex := 0 to High(AChanges) do
  begin
    FChanges[LIndex] := NyxProjectRoutineEdit(AChanges[LIndex].Owner, AChanges[LIndex].Edit);
    Inc(LCharacters, CharacterCount(FChanges[LIndex].Edit.Expected));
    Inc(LCharacters, CharacterCount(FChanges[LIndex].Edit.Code));

    if LCharacters > NyxMaximumRoutinePatchCharacters then
    begin
      raise ENyxModel.Create('Grouped routine text exceeds 131072 Unicode characters');
    end;
    for LPrior := 0 to LIndex - 1 do
    begin

      if FChanges[LPrior].Owner.SameFile(FChanges[LIndex].Owner) and
        SameText(FChanges[LPrior].Edit.Routine.Name, FChanges[LIndex].Edit.Routine.Name) then
      begin
        raise ENyxModel.Create('Replace each owning-unit routine once per group');
      end;
    end;
  end;
end;

{ A name is stable only within one owner and one exact snapshot. Overload
  ordinals are useful for inspection, never focused mutation identities. }
function UniqueProjectRoutine(const AIndex: INyxPascalRoutineIndex;
  const AReference: TNyxRoutineRef): TNyxPascalRoutineSite;
var
  LScan: Integer;
  LFound: Boolean;
begin
  Result := Default(TNyxPascalRoutineSite);
  LFound := False;
  for LScan := 0 to AIndex.Count - 1 do
  begin

    if SameText(AIndex.Item(LScan).Routine.Name, AReference.Name) then
    begin

      if LFound then
      begin
        raise ENyxModel.Create('Routine identity is overloaded or ambiguous in its owning unit');
      end;
      Result := AIndex.Item(LScan);
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxModel.Create('Routine is absent from its owning unit and includes');
  end;
end;

function TNyxProjectRoutinePatch.PrepareProject(
  const AProject: INyxPascalProject): INyxPascalProject;
var
  LOriginal: INyxPascalProject;
  LOwners: array of TNyxPascalSourceRef;
  LBefore: array of INyxPascalRoutineIndex;
  LAfter: array of INyxPascalRoutineIndex;
  LRanges: array of TNyxSourceFileEdit;
  LOldSites: array of TNyxPascalRoutineSite;
  LOwnerIndex: array of Integer;
  LIndex: Integer;
  LScan: Integer;
  LFound: Integer;
  LSite: TNyxPascalRoutineSite;
  LSwap: TNyxSourceFileEdit;
  LTrailing: Integer;
begin
  LOriginal := CaptureNyxPascalProject(AProject);
  LOwners := nil;
  LBefore := nil;
  LAfter := nil;
  SetLength(LRanges, Length(FChanges));
  SetLength(LOldSites, Length(FChanges));
  SetLength(LOwnerIndex, Length(FChanges));
  for LIndex := 0 to High(FChanges) do
  begin
    LFound := -1;
    for LScan := 0 to High(LOwners) do
    begin

      if LOwners[LScan].SameFile(FChanges[LIndex].Owner) then
      begin
        LFound := LScan;
        Break;
      end;
    end;

    if LFound < 0 then
    begin
      LFound := Length(LOwners);
      SetLength(LOwners, LFound + 1);
      SetLength(LBefore, LFound + 1);
      LOwners[LFound] := FChanges[LIndex].Owner;
      LBefore[LFound] := CaptureNyxPascalRoutineIndex(LOriginal, LOwners[LFound]);
    end;
    LOwnerIndex[LIndex] := LFound;
    LSite := UniqueProjectRoutine(LBefore[LFound], FChanges[LIndex].Edit.Routine);

    if not LSite.Editable then
    begin
      raise ENyxModel.Create(LSite.Reason);
    end;

    if LSite.Body <> FChanges[LIndex].Edit.Expected then
    begin
      raise ENyxModel.Create('Routine changed; inspect its exact accepted physical body again');
    end;
    LOldSites[LIndex] := LSite;
    LRanges[LIndex] := NyxSourceFileEdit(LSite.BodyLocation.FileReference,
      NyxSourceEdit(LSite.BodyLocation.Offset, LSite.Body, FChanges[LIndex].Edit.Code));
  end;
  { Semantic callers name routines in any order. Physical range admission still
    consumes original-file ascending offsets and detects overlapping shared data. }
  for LIndex := 1 to High(LRanges) do
  begin
    for LScan := 0 to LIndex - 1 do
    begin

      if LRanges[LScan].FileReference.SameFile(LRanges[LIndex].FileReference) and
        (LRanges[LScan].Edit.Offset > LRanges[LIndex].Edit.Offset) then
      begin
        LSwap := LRanges[LScan];
        LRanges[LScan] := LRanges[LIndex];
        LRanges[LIndex] := LSwap;
      end;
    end;
  end;
  Result := NyxSourceProjectPatch(LRanges).PrepareProject(LOriginal);
  SetLength(LAfter, Length(LOwners));
  for LIndex := 0 to High(LOwners) do
  begin
    LAfter[LIndex] := CaptureNyxPascalRoutineIndex(Result, LOwners[LIndex]);

    if LAfter[LIndex].Count <> LBefore[LIndex].Count then
    begin
      raise ENyxModel.Create('A routine body cannot add/remove sibling implementations');
    end;
  end;
  for LIndex := 0 to High(FChanges) do
  begin
    LSite := UniqueProjectRoutine(LAfter[LOwnerIndex[LIndex]], FChanges[LIndex].Edit.Routine);

    if not LSite.Editable or (LSite.Signature <> LOldSites[LIndex].Signature) or
      not LSite.BodyLocation.FileReference.SameFile(LOldSites[LIndex].BodyLocation.FileReference) or
      (Copy(FChanges[LIndex].Edit.Code, 1, Length(LSite.Body)) <> LSite.Body) then
    begin
      raise ENyxModel.Create('Replace only the exact routine body, retaining its signature and physical owner');
    end;
    for LTrailing := Length(LSite.Body) + 1 to Length(FChanges[LIndex].Edit.Code) do
    begin

      if not (FChanges[LIndex].Edit.Code[LTrailing] in [#9, #10, #13, ' ']) then
      begin
        raise ENyxModel.Create('Only whitespace may follow the complete routine implementation');
      end;
    end;
  end;
end;

function NewNyxProjectRoutinePatch(const AChanges: array of TNyxProjectRoutineEdit): INyxSourceProjectPatch;
begin
  Result := TNyxProjectRoutinePatch.Create(AChanges);
end;

constructor TNyxRoutinePatch.Create(const AChanges: array of TNyxRoutineEdit);
var
  LIndex: Integer;
  LPrior: Integer;
  LCharacters: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumRoutineChanges) then
  begin
    raise ENyxModel.Create('A Pascal implementation patch requires 1..16 changes');
  end;
  LCharacters := 0;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    FChanges[LIndex] := NyxRoutineEdit(AChanges[LIndex].Routine,
      AChanges[LIndex].Expected, AChanges[LIndex].Code);
    Inc(LCharacters, CharacterCount(AChanges[LIndex].Expected));
    Inc(LCharacters, CharacterCount(AChanges[LIndex].Code));

    if LCharacters > NyxMaximumRoutinePatchCharacters then
    begin
      raise ENyxModel.Create('Grouped routine text exceeds 131072 Unicode characters');
    end;
    for LPrior := 0 to LIndex - 1 do
    begin

      if SameText(FChanges[LPrior].Routine.Name, FChanges[LIndex].Routine.Name) then
      begin
        raise ENyxModel.Create('Replace each routine implementation once per grouped patch');
      end;
    end;
  end;
end;

function TNyxRoutinePatch.Candidate(ASession: TNyxStudioSession;
  out AResults: TNyxRoutineEditResults): TNyxProjectPair;
var
  LWorkspace: TNyxSourceWorkspace;
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LIndex: Integer;
  LRoutine: TNyxRoutineSource;
begin
  AResults := nil;

  if ASession = nil then
  begin
    raise ENyxModel.Create('Routine edits require an admitted Studio session');
  end;
  Result := ASession.ProjectSnapshot;

  if Result.Pending then
  begin
    raise ENyxModel.Create('Resolve the pending draft before editing routine implementations');
  end;
  LSource := Result.Source;
  for LIndex := 0 to High(FChanges) do
  begin
    LSource := ReplaceNyxRoutineImplementation(LSource, FChanges[LIndex].Routine,
      FChanges[LIndex].Expected, FChanges[LIndex].Code);
  end;

  if LSource = Result.Source then
  begin
    raise ENyxModel.Create('Routine patch has no source change');
  end;
  LWorkspace := TNyxSourceWorkspace.Create;
  LDocument := nil;
  try
    { Reconstruct the entire admitted contract before returning any candidate.
      A routine-only edit must not alter the portable design, even indirectly. }
    LDocument := LWorkspace.Candidate(ASession.Document, LSource);

    if TNyxCodec.Encode(LDocument) <> Result.Design then
    begin
      raise ENyxModel.Create('Routine implementation edits must retain the exact design');
    end;
    LWorkspace.Accept(LDocument, LSource);
    Result := NyxProjectPair(Result.Design, LSource);
    SetLength(AResults, Length(FChanges));
    for LIndex := 0 to High(FChanges) do
    begin
      LRoutine := ReadNyxRoutineSource(LSource, FChanges[LIndex].Routine);
      AResults[LIndex].Routine := FChanges[LIndex].Routine;
      AResults[LIndex].SourceLine := LRoutine.Line;
    end;
  finally
    LDocument.Free;
    LWorkspace.Free;
  end;
end;

function NewNyxRoutinePatch(const AChanges: array of TNyxRoutineEdit): INyxRoutinePatch;
begin
  Result := TNyxRoutinePatch.Create(AChanges);
end;

function ReadNyxRoutinePatch(const AChanges: TNyxDataValue): INyxRoutinePatch;
var
  LChanges: array of TNyxRoutineEdit;
  LValue: TNyxDataValue;
  LIndex: Integer;
  LKey: Integer;
begin

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumRoutineChanges) then
  begin
    raise ENyxModel.Create('A Pascal implementation patch requires an array of 1..16 changes');
  end;
  SetLength(LChanges, AChanges.Count);
  for LIndex := 0 to High(LChanges) do
  begin
    LValue := AChanges.Item(LIndex);

    if LValue.Kind <> ndObject then
    begin
      raise ENyxModel.Create('A routine implementation change must be an object');
    end;
    for LKey := 0 to LValue.Count - 1 do
    begin

      if (LValue.Key(LKey) <> 'routine') and (LValue.Key(LKey) <> 'expected') and
        (LValue.Key(LKey) <> 'implementation') then
      begin
        raise ENyxModel.Create('Unknown routine implementation field: ' + LValue.Key(LKey));
      end;
    end;
    LChanges[LIndex] := NyxRoutineEdit(NyxRoutine(LValue.Field('routine').AsText),
      LValue.Field('expected').AsText, LValue.Field('implementation').AsText);
  end;
  Result := NewNyxRoutinePatch(LChanges);
end;

function EncodeNyxRoutineResults(const AResults: TNyxRoutineEditResults): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(AResults));
  for LIndex := 0 to High(AResults) do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('routine', NyxData(AResults[LIndex].Routine.Name)),
      NyxField('line', NyxData(AResults[LIndex].SourceLine))]);
  end;
  Result := NyxArray(LItems);
end;

end.
