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


unit nyx.studio.importedits;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.source, nyx.studio.projects;

const
  NyxMaximumImportChanges = 32;

type
  { Immutable semantic operation; an open namespace is a distinct reference,
    while section/action are closed Pascal choices. No source offsets escape. }
  TNyxImportEdit = record
  private
    FSection: TNyxImportSection;
    FAction: TNyxImportAction;
    FUnit: TNyxPascalUnitRef;
  public
    property Section: TNyxImportSection read FSection;
    property Action: TNyxImportAction read FAction;
    property UnitRef: TNyxPascalUnitRef read FUnit;
  end;

  { Owns copied proposals, never an editor/document. Candidate uses ordinary
    source Apply on an independent session, preserves the exact design and
    returns owned paired text. Caller publishes once after response preflight.
    Pending drafts, duplicate/missing imports and late failures leave APair intact. }
  INyxImportPatch = interface(IInterface)
    ['{CC7F6248-A3F3-49E7-B6E4-8A9B23048C67}']
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

function NyxImportEdit(ASection: TNyxImportSection; AAction: TNyxImportAction;
  const AUnit: TNyxPascalUnitRef): TNyxImportEdit;
function NyxImportPatch(const AChanges: array of TNyxImportEdit): INyxImportPatch;
{ Explicit strict JSON boundary: exactly op/section/unit. Names use the same
  namespace admission as compiler companions; paths/options/code are forbidden. }
function ReadNyxImportPatch(const AChanges: TNyxDataValue): INyxImportPatch;
function ReadNyxImportSection(const AText: TNyxText): TNyxImportSection;
function NyxImportSectionName(ASection: TNyxImportSection): TNyxText;

implementation

uses
  SysUtils, nyx.model, nyx.studio.session;

const
  CImportSections: array[TNyxImportSection] of TNyxText = ('interface', 'implementation');

type
  TNyxImportPatch = class(TInterfacedObject, INyxImportPatch)
  private
    FChanges: array of TNyxImportEdit;
  public
    constructor Create(const AChanges: array of TNyxImportEdit);
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function GetCount: Integer;
  end;

function NyxImportSectionName(ASection: TNyxImportSection): TNyxText;
begin

  if (Ord(ASection) < Ord(Low(TNyxImportSection))) or
    (Ord(ASection) > Ord(High(TNyxImportSection))) then
  begin
    raise ENyxModel.Create('Unknown Pascal import section');
  end;
  Result := CImportSections[ASection];
end;

function ReadNyxImportSection(const AText: TNyxText): TNyxImportSection;
var
  LSection: TNyxImportSection;
begin
  for LSection := Low(TNyxImportSection) to High(TNyxImportSection) do
  begin

    if CImportSections[LSection] = AText then
    begin
      Exit(LSection);
    end;
  end;
  raise ENyxModel.Create('Import section must be interface or implementation');
end;

function NyxImportEdit(ASection: TNyxImportSection; AAction: TNyxImportAction;
  const AUnit: TNyxPascalUnitRef): TNyxImportEdit;
begin
  NyxImportSectionName(ASection);

  if (Ord(AAction) < Ord(Low(TNyxImportAction))) or
    (Ord(AAction) > Ord(High(TNyxImportAction))) then
  begin
    raise ENyxModel.Create('Unknown Pascal import action');
  end;
  Result.FSection := ASection;
  Result.FAction := AAction;
  Result.FUnit := NyxPascalUnit(AUnit.Name);
end;

constructor TNyxImportPatch.Create(const AChanges: array of TNyxImportEdit);
var
  LIndex: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumImportChanges) then
  begin
    raise ENyxModel.Create('Import group requires 1..32 changes');
  end;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    FChanges[LIndex] := NyxImportEdit(AChanges[LIndex].Section,
      AChanges[LIndex].Action, AChanges[LIndex].UnitRef);
  end;
end;

function TNyxImportPatch.GetCount: Integer;
begin
  Result := Length(FChanges);
end;

function TNyxImportPatch.Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
var
  LSource: TNyxText;
  LSession: TNyxStudioSession;
  LIndex: Integer;
begin

  if APair.Pending then
  begin
    raise ENyxModel.Create('Resolve the pending Pascal draft before import edits');
  end;
  LSource := APair.Source;
  for LIndex := 0 to High(FChanges) do
  begin
    LSource := EditNyxImport(LSource, FChanges[LIndex].Section,
      FChanges[LIndex].Action, FChanges[LIndex].UnitRef);
  end;

  if LSource = APair.Source then
  begin
    raise ENyxModel.Create('Import group has no source changes');
  end;
  LSession := TNyxStudioSession.Create(APair);
  try
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    Result := LSession.ProjectSnapshot;

    if (Result.Design <> APair.Design) or (Result.Source <> LSource) or Result.Pending then
    begin
      raise ENyxModel.Create('Import edits must preserve exact design and authored source');
    end;
  finally
    LSession.Free;
  end;
end;

function NyxImportPatch(const AChanges: array of TNyxImportEdit): INyxImportPatch;
begin
  Result := TNyxImportPatch.Create(AChanges);
end;

function ReadNyxImportPatch(const AChanges: TNyxDataValue): INyxImportPatch;
var
  LChanges: array of TNyxImportEdit;
  LData: TNyxDataValue;
  LAction: TNyxImportAction;
  LIndex: Integer;
  LKey: Integer;
begin
  AChanges.Validate;

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumImportChanges) then
  begin
    raise ENyxModel.Create('Import group requires 1..32 changes');
  end;
  SetLength(LChanges, AChanges.Count);
  for LIndex := 0 to High(LChanges) do
  begin
    LData := AChanges.Item(LIndex);

    if (LData.Kind <> ndObject) or (LData.Count <> 3) then
    begin
      raise ENyxModel.Create('Import change requires exactly op, section and unit');
    end;
    for LKey := 0 to LData.Count - 1 do
    begin

      if (LData.Key(LKey) <> 'op') and (LData.Key(LKey) <> 'section') and
        (LData.Key(LKey) <> 'unit') then
      begin
        raise ENyxModel.Create('Unknown import change member');
      end;
    end;
    LAction := niaAdd;

    if LData.Field('op').AsText = 'remove' then
    begin
      LAction := niaRemove;
    end
    else if LData.Field('op').AsText <> 'add' then
    begin
      raise ENyxModel.Create('Import action must be add or remove');
    end;
    LChanges[LIndex] := NyxImportEdit(ReadNyxImportSection(LData.Field('section').AsText),
      LAction, NyxPascalUnit(LData.Field('unit').AsText));
  end;
  Result := NyxImportPatch(LChanges);
end;

end.
