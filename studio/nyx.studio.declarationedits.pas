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

unit nyx.studio.declarationedits;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.source, nyx.studio.projects;

const
  NyxMaximumDeclarationChanges = 16;
  NyxMaximumDeclarationCharacters = 32768;
  NyxMaximumDeclarationPatchCharacters = 131072;

type
  { Closed ordered intent. Pascal fragments are explicit source boundaries.
    Creation owns its typed declaration; removal acknowledges every exact
    counterpart; implementation edits reuse the qualified routine contract. }
  TNyxDeclarationAction = (daCreate, daEdit, daRemove);
  TNyxDeclarationEdit = record
  private
    FAction: TNyxDeclarationAction;
    FRoutine: TNyxRoutineRef;
    FDeclaration: TNyxRoutineDeclaration;
    FExpectedSignature: TNyxText;
    FExpectedImplementation: TNyxText;
    FExpectedDeclaration: TNyxText;
    FImplementation: TNyxText;
  public
    property Action: TNyxDeclarationAction read FAction;
    property Routine: TNyxRoutineRef read FRoutine;
  end;

  { Immutable managed group owns copied proposals, never a supplier session or
    document. Candidate replays on a private session and returns an owned pair.
    Caller checks response size before the only publication/paired Undo entry. }
  INyxDeclarationPatch = interface(IInterface)
    ['{271BE5DA-8A62-4F45-A8BC-001005004001}']
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

{ Copy and validate a typed creation proposal, including exact Unicode limits.
  Target-source identity and insertion ownership are checked during Candidate. }
function NyxCreateDeclaration(const ADeclaration: TNyxRoutineDeclaration): TNyxDeclarationEdit;
{ Own one exact expected body and its replacement; signatures remain retained.
  Invalid Unicode or oversized fragments raise ENyxModel before replay. }
function NyxEditDeclaration(const ARoutine: TNyxRoutineRef;
  const AExpectedImplementation, AImplementation: TNyxText): TNyxDeclarationEdit;
{ Acknowledge every accepted counterpart before removing a free unit helper.
  Empty expected declaration denotes implementation-only visibility. Retained
  possible references and ambiguous ownership refuse during Candidate. }
function NyxRemoveDeclaration(const ARoutine: TNyxRoutineRef;
  const AExpectedSignature, AExpectedImplementation, AExpectedDeclaration: TNyxText): TNyxDeclarationEdit;
{ Own a copied, ordered group of 1..16 proposals within the total text budget.
  Construction validates the group; Candidate never mutates the supplied pair. }
function NyxDeclarationPatch(const AChanges: array of TNyxDeclarationEdit): INyxDeclarationPatch;
{ Strict explicit JSON boundary: action-specific fields, closed kind/visibility
  choices and no per-change context, source-unit or compiler options. }
function ReadNyxDeclarationPatch(const AChanges: TNyxDataValue): INyxDeclarationPatch;

implementation

uses
  SysUtils, nyx.model, nyx.studio.session;

type
  TNyxDeclarationPatch = class(TInterfacedObject, INyxDeclarationPatch)
  private
    FChanges: array of TNyxDeclarationEdit;
  public
    constructor Create(const AChanges: array of TNyxDeclarationEdit);
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function GetCount: Integer;
  end;

function DeclarationCharacters(const AText: TNyxText): Integer;
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
      raise ENyxModel.Create('Malformed Unicode in declaration text');
    end;
    Inc(Result);

    if Result > NyxMaximumDeclarationCharacters then
    begin
      raise ENyxModel.Create('Declaration text exceeds 32768 Unicode scalars');
    end;
  end;
end;

function NyxCreateDeclaration(const ADeclaration: TNyxRoutineDeclaration): TNyxDeclarationEdit;
begin
  Result := Default(TNyxDeclarationEdit);
  Result.FAction := daCreate;
  Result.FDeclaration := NyxRoutineDeclaration(ADeclaration.Kind, ADeclaration.Routine,
    ADeclaration.Visibility, ADeclaration.Signature, ADeclaration.Code);
  Result.FRoutine := Result.FDeclaration.Routine;
  DeclarationCharacters(ADeclaration.Signature);
  DeclarationCharacters(ADeclaration.Code);
end;

function NyxEditDeclaration(const ARoutine: TNyxRoutineRef;
  const AExpectedImplementation, AImplementation: TNyxText): TNyxDeclarationEdit;
begin
  Result := Default(TNyxDeclarationEdit);
  Result.FAction := daEdit;
  Result.FRoutine := NyxRoutine(ARoutine.Name);
  DeclarationCharacters(AExpectedImplementation);
  DeclarationCharacters(AImplementation);
  Result.FExpectedImplementation := AExpectedImplementation;
  Result.FImplementation := AImplementation;
end;

function NyxRemoveDeclaration(const ARoutine: TNyxRoutineRef;
  const AExpectedSignature, AExpectedImplementation, AExpectedDeclaration: TNyxText): TNyxDeclarationEdit;
begin
  Result := Default(TNyxDeclarationEdit);
  Result.FAction := daRemove;
  Result.FRoutine := NyxRoutine(ARoutine.Name);
  DeclarationCharacters(AExpectedSignature);
  DeclarationCharacters(AExpectedImplementation);
  DeclarationCharacters(AExpectedDeclaration);
  Result.FExpectedSignature := AExpectedSignature;
  Result.FExpectedImplementation := AExpectedImplementation;
  Result.FExpectedDeclaration := AExpectedDeclaration;
end;

constructor TNyxDeclarationPatch.Create(const AChanges: array of TNyxDeclarationEdit);
var
  LIndex: Integer;
  LTotal: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumDeclarationChanges) then
  begin
    raise ENyxModel.Create('A declaration patch requires 1..16 ordered changes');
  end;
  SetLength(FChanges, Length(AChanges));
  LTotal := 0;
  for LIndex := 0 to High(AChanges) do
  begin
    case Ord(AChanges[LIndex].Action) of
      Ord(daCreate):
        FChanges[LIndex] := NyxCreateDeclaration(AChanges[LIndex].FDeclaration);
      Ord(daEdit):
        FChanges[LIndex] := NyxEditDeclaration(AChanges[LIndex].Routine,
          AChanges[LIndex].FExpectedImplementation, AChanges[LIndex].FImplementation);
      Ord(daRemove):
        FChanges[LIndex] := NyxRemoveDeclaration(AChanges[LIndex].Routine,
          AChanges[LIndex].FExpectedSignature, AChanges[LIndex].FExpectedImplementation,
          AChanges[LIndex].FExpectedDeclaration);
    else
      raise ENyxModel.Create('Unknown typed declaration action');
    end;
    Inc(LTotal, DeclarationCharacters(FChanges[LIndex].FDeclaration.Signature));
    Inc(LTotal, DeclarationCharacters(FChanges[LIndex].FDeclaration.Code));
    Inc(LTotal, DeclarationCharacters(FChanges[LIndex].FExpectedSignature));
    Inc(LTotal, DeclarationCharacters(FChanges[LIndex].FExpectedImplementation));
    Inc(LTotal, DeclarationCharacters(FChanges[LIndex].FExpectedDeclaration));
    Inc(LTotal, DeclarationCharacters(FChanges[LIndex].FImplementation));

    if LTotal > NyxMaximumDeclarationPatchCharacters then
    begin
      raise ENyxModel.Create('Declaration group exceeds 131072 Unicode scalars');
    end;
  end;
end;

function TNyxDeclarationPatch.GetCount: Integer;
begin
  Result := Length(FChanges);
end;

function TNyxDeclarationPatch.Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
var
  LSource: TNyxText;
  LIndex: Integer;
  LSession: TNyxStudioSession;
begin

  if APair.Pending then
  begin
    raise ENyxModel.Create('Resolve the pending draft before editing helper declarations');
  end;
  LSource := APair.Source;
  for LIndex := 0 to High(FChanges) do
  begin
    case FChanges[LIndex].Action of
      daCreate:
        LSource := AddNyxRoutineDeclaration(LSource, FChanges[LIndex].FDeclaration);
      daEdit:
        LSource := ReplaceNyxRoutineImplementation(LSource, FChanges[LIndex].Routine,
          FChanges[LIndex].FExpectedImplementation, FChanges[LIndex].FImplementation);
      daRemove:
        LSource := RemoveNyxRoutineDeclaration(LSource, FChanges[LIndex].Routine,
          FChanges[LIndex].FExpectedSignature, FChanges[LIndex].FExpectedImplementation,
          FChanges[LIndex].FExpectedDeclaration);
    end;
  end;

  if LSource = APair.Source then
  begin
    raise ENyxModel.Create('Declaration group has no source change');
  end;
  LSession := TNyxStudioSession.Create(APair);
  try
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    Result := LSession.ProjectSnapshot;

    if (Result.Design <> APair.Design) or (Result.Source <> LSource) or Result.Pending then
    begin
      raise ENyxModel.Create('Declaration edits must retain exact design and complete source admission');
    end;
  finally
    LSession.Free;
  end;
end;

function NyxDeclarationPatch(const AChanges: array of TNyxDeclarationEdit): INyxDeclarationPatch;
begin
  Result := TNyxDeclarationPatch.Create(AChanges);
end;

function ReadNyxDeclarationPatch(const AChanges: TNyxDataValue): INyxDeclarationPatch;
var
  LChanges: array of TNyxDeclarationEdit;
  LValue: TNyxDataValue;
  LOp: TNyxText;
  LAllowed: TNyxText;
  LKind: TNyxRoutineKind;
  LVisibility: TNyxRoutineVisibility;
  LIndex: Integer;
  LKey: Integer;
begin

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumDeclarationChanges) then
  begin
    raise ENyxModel.Create('Declaration changes require an array of 1..16 operations');
  end;
  SetLength(LChanges, AChanges.Count);
  for LIndex := 0 to High(LChanges) do
  begin
    LValue := AChanges.Item(LIndex);

    if LValue.Kind <> ndObject then
    begin
      raise ENyxModel.Create('Declaration operation must be an object');
    end;
    LOp := LValue.Field('op').AsText;
    LAllowed := '|op|routine|expectedSignature|expectedImplementation|expectedDeclaration|';

    if LOp = 'create' then
    begin
      LAllowed := '|op|routine|kind|visibility|signature|implementation|';
    end
    else if LOp = 'edit' then
    begin
      LAllowed := '|op|routine|expectedImplementation|implementation|';
    end
    else if LOp <> 'remove' then
    begin
      raise ENyxModel.Create('Declaration action must be create, edit or remove');
    end;
    for LKey := 0 to LValue.Count - 1 do
    begin

      if Pos('|' + LValue.Key(LKey) + '|', LAllowed) = 0 then
      begin
        raise ENyxModel.Create('Unknown declaration operation field: ' + LValue.Key(LKey));
      end;
    end;

    if LOp = 'create' then
    begin
      LKind := nrProcedure;

      if LValue.Field('kind').AsText = 'function' then
      begin
        LKind := nrFunction;
      end
      else if LValue.Field('kind').AsText <> 'procedure' then
      begin
        raise ENyxModel.Create('Creation kind must be procedure or function');
      end;
      LVisibility := rvInterface;

      if LValue.Field('visibility').AsText = 'implementation' then
      begin
        LVisibility := rvImplementation;
      end
      else if LValue.Field('visibility').AsText <> 'interface' then
      begin
        raise ENyxModel.Create('Creation visibility must be interface or implementation');
      end;
      LChanges[LIndex] := NyxCreateDeclaration(NyxRoutineDeclaration(LKind,
        NyxRoutine(LValue.Field('routine').AsText), LVisibility,
        LValue.Field('signature').AsText, LValue.Field('implementation').AsText));
    end
    else if LOp = 'edit' then
    begin
      LChanges[LIndex] := NyxEditDeclaration(NyxRoutine(LValue.Field('routine').AsText),
        LValue.Field('expectedImplementation').AsText, LValue.Field('implementation').AsText);
    end
    else
    begin
      LChanges[LIndex] := NyxRemoveDeclaration(NyxRoutine(LValue.Field('routine').AsText),
        LValue.Field('expectedSignature').AsText, LValue.Field('expectedImplementation').AsText,
        LValue.Field('expectedDeclaration').AsText);
    end;
  end;
  Result := NyxDeclarationPatch(LChanges);
end;

end.
