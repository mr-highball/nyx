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

unit nyx.studio.sourceedits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.pascal.project, nyx.pascal.drafts, nyx.studio.projects;

const
  { Scalar counts are identical for native UTF-8 and browser UTF-16. The group
    budgets expected and replacement text separately, rather than per item. }
  NyxMaximumSourceEdits = 16;
  NyxMaximumSourceEditCharacters = 262144;
  NyxMaximumSourceCharacters = 4194304;

type
  { One immutable intent in the ORIGINAL accepted source, with a zero-based
    Unicode-scalar offset. Expected text determines its exact span; empty text
    inserts at that position. Replacement may be empty for deletion. Constructors
    validate encoding and budgets; no DOM, LCL or mutable editor is borrowed. }
  TNyxSourceEdit = record
  private
    FOffset: Integer;
    FExpected: TNyxText;
    FReplacement: TNyxText;
    FExpectedCharacters: Integer;
    FReplacementCharacters: Integer;
  public
    property Offset: Integer read FOffset;
    property Expected: TNyxText read FExpected;
    property Replacement: TNyxText read FReplacement;
  end;

  { Captures a detached, ordered group. Candidate stages ordinary source Apply
    in an independent session and returns owned portable pair text. The caller
    publishes once, adding one paired Undo entry. Untouched source is retained
    exactly, including helpers, comments and Unicode. Class/signature edits are
    explicit source authoring: unlike focused routine tools this contract permits
    changing those declarations. Ordinary source grammar/markers still apply.
    No application code is executed; compile the accepted pair separately.
    Pending drafts, no-op groups, mismatched expectations, overlapping/out-of-
    order ranges or unsupported source refuse without changing the input pair. }
  INyxSourcePatch = interface(IInterface)
    ['{68F89A18-71A1-4D73-84BC-265587A03438}']
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
  end;

  { Optional text-only preparation on an immutable range group. This retains
    the original INyxSourcePatch interface and its literal admission contract.
    It validates all exact ranges and budgets without parsing, compiling or
    executing application Pascal. A trusted compiler host must separately
    construct and admit the complete result before publication. The returned
    text owns its values and borrows no editor, tree or pending draft. }
  INyxSourcePatchPreparation = interface(IInterface)
    ['{68F89A18-71A1-4D73-84BC-265587A03439}']
    function PrepareSource(const ASource: TNyxText): TNyxText;
  end;

  { One exact range in a named accepted input. Main is an explicit typed
    reference too. This data borrows no project, editor or compiler; offsets
    retain the original file's scalar coordinate system across the group. }
  TNyxSourceFileEdit = record
  private
    FFile: TNyxPascalSourceRef;
    FEdit: TNyxSourceEdit;
  public
    property FileReference: TNyxPascalSourceRef read FFile;
    property Edit: TNyxSourceEdit read FEdit;
  end;

  { Immutable ordered project range group. Each file's ranges are ascending and
    nonoverlapping even when different files are interleaved. The shared 16-edit
    and 262144-scalar side budgets apply to the COMPLETE group. Preparation
    preserves membership, order, main identity and limits, validates the complete
    result, and grants no execution/publication authority. The owning compiler
    must construct these exact independent inputs before one paired publication. }
  INyxSourceProjectPatch = interface(IInterface)
    ['{68F89A18-71A1-4D73-84BC-265587A0343A}']
    function PrepareProject(const AProject: INyxPascalProject): INyxPascalProject;
  end;

{ Offsets always refer to the original source, never to an intermediate edit.
  Expected and replacement strings contain complete Unicode scalars. }
function NyxSourceEdit(AOffset: Integer; const AExpected,
  AReplacement: TNyxText): TNyxSourceEdit;

{ Supply 1..16 ascending, nonoverlapping changes. Two changes at one offset
  refuse, including two insertions; combine their intended text explicitly. }
function NyxSourcePatch(const AEdits: array of TNyxSourceEdit): INyxSourcePatch;
{ Recapture the reference and range; an absent helper never selects main. }
function NyxSourceFileEdit(const AFile: TNyxPascalSourceRef;
  const AEdit: TNyxSourceEdit): TNyxSourceFileEdit;
function NyxSourceProjectPatch(const AEdits: array of TNyxSourceFileEdit): INyxSourceProjectPatch;

implementation

uses
  SysUtils, nyx.model, nyx.pascal.names, nyx.studio.session;

type
  { Detached scalar ranges serve both literal source admission and trusted
    compiler preparation; neither interface retains the accepted session. }
  TNyxSourcePatch = class(TInterfacedObject, INyxSourcePatch, INyxSourcePatchPreparation)
  private
    FEdits: array of TNyxSourceEdit;
  public
    constructor Create(const AEdits: array of TNyxSourceEdit);
    function PrepareSource(const ASource: TNyxText): TNyxText;
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
  end;

  { Private copied file/range vector. Each distinct file delegates scalar
    admission to the existing patcher, always against the original snapshot. }
  TNyxSourceProjectPatch = class(TInterfacedObject, INyxSourceProjectPatch)
  private
    FEdits: array of TNyxSourceFileEdit;
  public
    constructor Create(const AEdits: array of TNyxSourceFileEdit);
    function PrepareProject(const AProject: INyxPascalProject): INyxPascalProject;
  end;

function CharacterCount(const AText: TNyxText; AMaximum: Integer): Integer;
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
      raise ENyxModel.Create('Source edits require valid Unicode scalars');
    end;
    Inc(Result);

    if Result > AMaximum then
    begin
      raise ENyxModel.Create('Source text exceeds its Unicode-scalar budget');
    end;
  end;
end;

function NyxSourceEdit(AOffset: Integer; const AExpected,
  AReplacement: TNyxText): TNyxSourceEdit;
begin

  if (AOffset < 0) or (AOffset > NyxMaximumSourceCharacters) then
  begin
    raise ENyxModel.Create('Source edit offset is outside its scalar budget');
  end;
  Result.FExpectedCharacters := CharacterCount(AExpected, NyxMaximumSourceEditCharacters);
  Result.FReplacementCharacters := CharacterCount(AReplacement, NyxMaximumSourceEditCharacters);
  Result.FOffset := AOffset;
  Result.FExpected := AExpected;
  Result.FReplacement := AReplacement;
end;

constructor TNyxSourcePatch.Create(const AEdits: array of TNyxSourceEdit);
var
  LIndex: Integer;
  LExpected: Integer;
  LReplacement: Integer;
begin
  inherited Create;

  if (Length(AEdits) < 1) or (Length(AEdits) > NyxMaximumSourceEdits) then
  begin
    raise ENyxModel.Create('Source edits require 1..16 changes');
  end;
  LExpected := 0;
  LReplacement := 0;
  SetLength(FEdits, Length(AEdits));
  for LIndex := 0 to High(AEdits) do
  begin
    { Recapture through the constructor, including a default-initialized record.
      Copy the array so a caller cannot later retarget a captured proposal. }
    FEdits[LIndex] := NyxSourceEdit(AEdits[LIndex].Offset,
      AEdits[LIndex].Expected, AEdits[LIndex].Replacement);
    Inc(LExpected, FEdits[LIndex].FExpectedCharacters);
    Inc(LReplacement, FEdits[LIndex].FReplacementCharacters);

    if (LExpected > NyxMaximumSourceEditCharacters) or
      (LReplacement > NyxMaximumSourceEditCharacters) then
    begin
      raise ENyxModel.Create('Source edit group exceeds 262144 scalars per text side');
    end;

    if FEdits[LIndex].Expected = FEdits[LIndex].Replacement then
    begin
      raise ENyxModel.Create('Every source change must have different replacement text');
    end;

    if LIndex > 0 then
    begin

      if (FEdits[LIndex].Offset <= FEdits[LIndex - 1].Offset) or
        (FEdits[LIndex].Offset < FEdits[LIndex - 1].Offset +
          FEdits[LIndex - 1].FExpectedCharacters) then
      begin
        raise ENyxModel.Create('Source edits must be ascending and nonoverlapping');
      end;
    end;
  end;
end;

function TNyxSourcePatch.PrepareSource(const ASource: TNyxText): TNyxText;
var
  LIndex: Integer;
  LCursor: Integer;
  LScalarOffset: Integer;
  LScalar: Integer;
  LStart: Integer;
  LTotal: Integer;
  LSource: TNyxText;
begin
  LTotal := CharacterCount(ASource, NyxMaximumSourceCharacters);
  LCursor := 1;
  LScalarOffset := 0;
  LSource := '';
  for LIndex := 0 to High(FEdits) do
  begin

    if (FEdits[LIndex].Offset > LTotal) or
      (FEdits[LIndex].FExpectedCharacters > LTotal - FEdits[LIndex].Offset) then
    begin
      raise ENyxModel.Create('Source edit range is outside the accepted source');
    end;
    LStart := LCursor;
    while LScalarOffset < FEdits[LIndex].Offset do
    begin
      NyxNextScalar(ASource, LCursor, LScalar);
      Inc(LScalarOffset);
    end;

    if Copy(ASource, LCursor, Length(FEdits[LIndex].Expected)) <>
      FEdits[LIndex].Expected then
    begin
      raise ENyxModel.Create('Source expected text differs at the specified scalar offset');
    end;
    LSource := LSource + Copy(ASource, LStart, LCursor - LStart) +
      FEdits[LIndex].Replacement;
    Inc(LCursor, Length(FEdits[LIndex].Expected));
    Inc(LScalarOffset, FEdits[LIndex].FExpectedCharacters);
  end;
  LSource := LSource + Copy(ASource, LCursor, MaxInt);
  CharacterCount(LSource, NyxMaximumSourceCharacters);

  if LSource = ASource then
  begin
    raise ENyxModel.Create('Source edit group has no resulting source changes');
  end;
  Result := LSource;
end;

function TNyxSourcePatch.Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
var
  LSource: TNyxText;
  LSession: TNyxStudioSession;
begin

  if APair.Pending or (APair.FileDrafts.Count > 0) then
  begin
    raise ENyxModel.Create('Resolve the pending Pascal draft before source edits');
  end;
  LSource := PrepareSource(APair.Source);
  { Ordinary admission is deliberately performed outside the accepted editor.
    Its draft, diagnostics, selection and history stay untouched on refusal. }
  LSession := TNyxStudioSession.Create(APair);
  try
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    Result := LSession.ProjectSnapshot;

    if (Result.Source <> LSource) or Result.Pending then
    begin
      raise ENyxModel.Create('Source admission must retain exact authored text');
    end;
  finally
    LSession.Free;
  end;
end;

function NyxSourcePatch(const AEdits: array of TNyxSourceEdit): INyxSourcePatch;
begin
  Result := TNyxSourcePatch.Create(AEdits);
end;

function NyxSourceFileEdit(const AFile: TNyxPascalSourceRef;
  const AEdit: TNyxSourceEdit): TNyxSourceFileEdit;
begin
  AFile.Validate;
  Result.FFile := AFile;
  Result.FEdit := NyxSourceEdit(AEdit.Offset, AEdit.Expected, AEdit.Replacement);
end;

constructor TNyxSourceProjectPatch.Create(const AEdits: array of TNyxSourceFileEdit);
var
  LIndex: Integer;
  LExpected: Integer;
  LReplacement: Integer;
begin
  inherited Create;

  if (Length(AEdits) < 1) or (Length(AEdits) > NyxMaximumSourceEdits) then
  begin
    raise ENyxModel.Create('Project source edits require 1..16 changes');
  end;
  SetLength(FEdits, Length(AEdits));
  LExpected := 0;
  LReplacement := 0;
  for LIndex := 0 to High(AEdits) do
  begin
    FEdits[LIndex] := NyxSourceFileEdit(AEdits[LIndex].FileReference, AEdits[LIndex].Edit);
    Inc(LExpected, FEdits[LIndex].Edit.FExpectedCharacters);
    Inc(LReplacement, FEdits[LIndex].Edit.FReplacementCharacters);

    if (LExpected > NyxMaximumSourceEditCharacters) or
      (LReplacement > NyxMaximumSourceEditCharacters) then
    begin
      raise ENyxModel.Create('Project source edit group exceeds 262144 scalars per text side');
    end;
  end;
end;

function TNyxSourceProjectPatch.PrepareProject(
  const AProject: INyxPascalProject): INyxPascalProject;
var
  LOriginal: INyxPascalProject;
  LIndex: Integer;
  LPrior: Integer;
  LScan: Integer;
  LCount: Integer;
  LSeen: Boolean;
  LFile: TNyxPascalSourceRef;
  LEdits: array of TNyxSourceEdit;
  LPatch: INyxSourcePatch;
  LPreparation: INyxSourcePatchPreparation;
  LText: TNyxText;
begin
  LOriginal := CaptureNyxPascalProject(AProject);
  Result := LOriginal;
  for LIndex := 0 to High(FEdits) do
  begin
    LFile := FEdits[LIndex].FileReference;
    LSeen := False;
    for LPrior := 0 to LIndex - 1 do
    begin

      if FEdits[LPrior].FileReference.SameFile(LFile) then
      begin
        LSeen := True;
        Break;
      end;
    end;

    if LSeen then
    begin
      Continue;
    end;
    LCount := 0;
    SetLength(LEdits, Length(FEdits) - LIndex);
    for LScan := LIndex to High(FEdits) do
    begin

      if FEdits[LScan].FileReference.SameFile(LFile) then
      begin
        LEdits[LCount] := FEdits[LScan].Edit;
        Inc(LCount);
      end;
    end;
    SetLength(LEdits, LCount);
    LPatch := NyxSourcePatch(LEdits);

    if not Supports(LPatch, INyxSourcePatchPreparation, LPreparation) then
    begin
      raise ENyxModel.Create('Project range preparation requires the scalar patch capability');
    end;
    LText := LPreparation.PrepareSource(LFile.Text(LOriginal));
    case LFile.Role of
      npsMain:
        begin
          Result := Result.WithSource(LText);
        end;
      npsUnit:
        begin
          Result := Result.WithUnit(NyxPascalUnit(Copy(LFile.Name, 1, Length(LFile.Name) - 4)), LText);
        end;
      npsInclude:
        begin
          Result := Result.WithInclude(NyxPascalInclude(LFile.Name), LText);
        end;
    end;
  end;
  Result := CaptureNyxPascalProject(Result);
end;

function NyxSourceProjectPatch(const AEdits: array of TNyxSourceFileEdit): INyxSourceProjectPatch;
begin
  Result := TNyxSourceProjectPatch.Create(AEdits);
end;

end.
