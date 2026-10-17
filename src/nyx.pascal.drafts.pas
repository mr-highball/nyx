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
unit nyx.pascal.drafts;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.names, nyx.pascal.project;

type
  { Closed source-editor role. The main unit uses the existing main buffer;
    unit/include references address exact authored leaves, never host paths. }
  TNyxPascalSourceRole = (npsMain, npsUnit, npsInclude);

  { Portable file selection independent of a mutable project/editor. Main has no
    embedded namespace, so it survives project renaming; auxiliary names retain
    exact case and their ordinary .pas/.inc suffix. Resolution borrows an immutable
    project only for the call and refuses missing or differently typed members. }
  TNyxPascalSourceRef = record
  private
    FRole: TNyxPascalSourceRole;
    FName: TNyxText;
  public
    class function Main: TNyxPascalSourceRef; static;
    class function UnitFile(const AUnit: TNyxPascalUnitRef): TNyxPascalSourceRef; static;
    class function IncludeFile(const AInclude: TNyxPascalIncludeRef): TNyxPascalSourceRef; static;
    procedure Validate;
    function SameFile(const AOther: TNyxPascalSourceRef): Boolean;
    function Text(const AProject: INyxPascalProject): TNyxText;
    property Role: TNyxPascalSourceRole read FRole;
    property Name: TNyxText read FName;
  end;

  { Immutable unfinished helper text and the exact accepted text at first edit.
    A later accepted change does not silently rebase the buffer. An empty buffer,
    supplementary Unicode, embedded NUL and incomplete Pascal remain ordinary data. }
  TNyxPascalFileDraft = record
  private
    FFile: TNyxPascalSourceRef;
    FText: TNyxText;
    FBase: TNyxText;
  public
    property FileReference: TNyxPascalSourceRef read FFile;
    property Text: TNyxText read FText;
    property Base: TNyxText read FBase;
  end;

  { Persistent helper-buffer value. Copies contain managed text/records only,
    including on pas2js. Edits detach the array and preserve other buffers/bases;
    no editor, document, mutable provider or execution capability is retained.
    Main buffers remain with their established source-workspace contract. }
  TNyxPascalFileDrafts = record
  private
    FEntries: array of TNyxPascalFileDraft;
    function GetCount: Integer;
    function IndexOf(const AFile: TNyxPascalSourceRef): Integer;
  public
    function Item(AIndex: Integer): TNyxPascalFileDraft;
    function Contains(const AFile: TNyxPascalSourceRef): Boolean;
    function Draft(const AFile: TNyxPascalSourceRef): TNyxPascalFileDraft;
    { Equal-to-current-accepted text removes this buffer. An existing buffer
      retains its first base, even when that base differs from current acceptance.
      Text admission checks encoding/size only; this never parses or executes it. }
    function Editing(const AFile: TNyxPascalSourceRef;
      const AAccepted, AText: TNyxText): TNyxPascalFileDrafts;
    function Without(const AFile: TNyxPascalSourceRef): TNyxPascalFileDrafts;
    procedure ValidateAgainst(const AProject: INyxPascalProject);
    { Empty collections encode as absent text. The strict versioned boundary
      preserves exact buffer/base values; ownership validation remains separate.
      Each text has the existing 4 MiB source-file bound and the collection has
      at most 4095 auxiliary buffers. Existing enclosing JSON budgets still apply. }
    function Encoded: TNyxText;
    class function Parse(const AData: TNyxText): TNyxPascalFileDrafts; static;
    function StorageBytes: TNyxTextBytes;
    { Compare exact persistent buffer/base values without serializing all text.
      Order is retained presentation data; this grants no admission or rebasing. }
    function SameValues(const AOther: TNyxPascalFileDrafts): Boolean;
    property Count: Integer read GetCount;
  end;

implementation

uses SysUtils, nyx.bytes, nyx.data;

function TNyxPascalFileDrafts.SameValues(const AOther: TNyxPascalFileDrafts): Boolean;
var
  LIndex: Integer;
begin
  Result := Count = AOther.Count;

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if not FEntries[LIndex].FileReference.SameFile(AOther.FEntries[LIndex].FileReference) or
      (FEntries[LIndex].Text <> AOther.FEntries[LIndex].Text) or
      (FEntries[LIndex].Base <> AOther.FEntries[LIndex].Base) then
    begin
      Exit(False);
    end;
  end;
end;

procedure ValidateBuffer(const AText: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
begin

  if NyxUTF8ByteCount(AText) > 4 * 1024 * 1024 then
  begin
    raise ENyxPascalProject.Create('A supporting Pascal buffer exceeds 4 MiB');
  end;
  LIndex := 1;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxPascalProject.Create('A supporting Pascal buffer has malformed Unicode');
    end;
  end;
end;

class function TNyxPascalSourceRef.Main: TNyxPascalSourceRef;
begin
  Result := Default(TNyxPascalSourceRef);
end;

class function TNyxPascalSourceRef.UnitFile(
  const AUnit: TNyxPascalUnitRef): TNyxPascalSourceRef;
begin
  Result.FRole := npsUnit;
  Result.FName := NyxPascalUnit(AUnit.Name).Name + '.pas';
end;

class function TNyxPascalSourceRef.IncludeFile(
  const AInclude: TNyxPascalIncludeRef): TNyxPascalSourceRef;
begin
  Result.FRole := npsInclude;
  Result.FName := NyxPascalInclude(AInclude.Name).Name;
end;

procedure TNyxPascalSourceRef.Validate;
begin
  case FRole of
    npsMain:
      begin

        if FName <> '' then
        begin
          raise ENyxPascalProject.Create('The main Pascal reference has no auxiliary name');
        end;
      end;
    npsUnit:
      begin

        if (Length(FName) <= 4) or (Copy(FName, Length(FName) - 3, 4) <> '.pas') then
        begin
          raise ENyxPascalProject.Create('A Pascal unit reference requires its exact .pas leaf');
        end;
        NyxPascalUnit(Copy(FName, 1, Length(FName) - 4));
      end;
    npsInclude:
      begin
        NyxPascalInclude(FName);
      end;
  end;
end;

function TNyxPascalSourceRef.SameFile(const AOther: TNyxPascalSourceRef): Boolean;
begin
  Result := (FRole = AOther.FRole) and (FName = AOther.FName);
end;

function TNyxPascalSourceRef.Text(const AProject: INyxPascalProject): TNyxText;
var
  LIndex: Integer;
  LInput: INyxPascalInput;
begin
  Validate;

  if AProject = nil then
  begin
    raise ENyxPascalProject.Create('Source-file resolution requires its captured project');
  end;

  if FRole = npsMain then
  begin
    Exit(AProject.Source);
  end;
  for LIndex := 0 to AProject.Count - 1 do
  begin
    LInput := AProject.Input(LIndex);

    if (LInput.Name = FName) and
      (((FRole = npsUnit) and (LInput.Kind = npiUnit)) or
      ((FRole = npsInclude) and (LInput.Kind = npiInclude))) then
    begin
      Exit(LInput.Text);
    end;
  end;
  raise ENyxPascalProject.Create('Supporting Pascal file is outside the accepted project: ' + FName);
end;

function TNyxPascalFileDrafts.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TNyxPascalFileDrafts.IndexOf(const AFile: TNyxPascalSourceRef): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].FFile.SameFile(AFile) then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxPascalFileDrafts.Item(AIndex: Integer): TNyxPascalFileDraft;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalProject.Create('Pascal buffer index is outside the retained collection');
  end;
  Result := FEntries[AIndex];
end;

function TNyxPascalFileDrafts.Contains(const AFile: TNyxPascalSourceRef): Boolean;
begin
  Result := IndexOf(AFile) >= 0;
end;

function TNyxPascalFileDrafts.Draft(const AFile: TNyxPascalSourceRef): TNyxPascalFileDraft;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AFile);

  if LIndex < 0 then
  begin
    raise ENyxPascalProject.Create('Pascal file has no unfinished helper buffer');
  end;
  Result := FEntries[LIndex];
end;

function TNyxPascalFileDrafts.Editing(const AFile: TNyxPascalSourceRef;
  const AAccepted, AText: TNyxText): TNyxPascalFileDrafts;
var
  LIndex: Integer;
  LCopy: Integer;
begin
  AFile.Validate;

  if AFile.Role = npsMain then
  begin
    raise ENyxPascalProject.Create('The main unit uses its existing source draft');
  end;
  ValidateBuffer(AText);
  ValidateBuffer(AAccepted);

  if AText = AAccepted then
  begin
    Exit(Without(AFile));
  end;
  { Indexed record assignment preserves value semantics on both compilers;
    editing this array must never mutate a record retained by another value. }
  SetLength(Result.FEntries, Count);
  for LCopy := 0 to High(FEntries) do
  begin
    Result.FEntries[LCopy] := FEntries[LCopy];
  end;
  LIndex := IndexOf(AFile);

  if LIndex < 0 then
  begin

    if Count >= 4095 then
    begin
      raise ENyxPascalProject.Create('Pascal helper buffers exceed their file-count bound');
    end;
    LIndex := Length(Result.FEntries);
    SetLength(Result.FEntries, LIndex + 1);
    Result.FEntries[LIndex].FFile := AFile;
    Result.FEntries[LIndex].FBase := AAccepted;
  end;
  Result.FEntries[LIndex].FText := AText;
end;

function TNyxPascalFileDrafts.Without(
  const AFile: TNyxPascalSourceRef): TNyxPascalFileDrafts;
var
  LIndex: Integer;
  LSource: Integer;
  LTarget: Integer;
begin
  LIndex := IndexOf(AFile);

  if LIndex < 0 then
  begin
    Exit(Self);
  end;
  SetLength(Result.FEntries, Count - 1);
  LTarget := 0;
  for LSource := 0 to High(FEntries) do
  begin

    if LSource <> LIndex then
    begin
      Result.FEntries[LTarget] := FEntries[LSource];
      Inc(LTarget);
    end;
  end;
end;

procedure TNyxPascalFileDrafts.ValidateAgainst(const AProject: INyxPascalProject);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin
    FEntries[LIndex].FFile.Text(AProject);
  end;
end;

function TNyxPascalFileDrafts.Encoded: TNyxText;
const
  CRoles: array[TNyxPascalSourceRole] of TNyxText = ('main', 'unit', 'include');
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin

  if Count = 0 then
  begin
    Exit('');
  end;
  SetLength(LItems, Count);
  for LIndex := 0 to Count - 1 do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('kind', NyxData(CRoles[FEntries[LIndex].FFile.Role])),
      NyxField('name', NyxData(FEntries[LIndex].FFile.Name)),
      NyxField('text', NyxData(FEntries[LIndex].FText)),
      NyxField('base', NyxData(FEntries[LIndex].FBase))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('items', NyxArray(LItems))]).ToJSON;
end;

class function TNyxPascalFileDrafts.Parse(
  const AData: TNyxText): TNyxPascalFileDrafts;
var
  LData: TNyxDataValue;
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LFile: TNyxPascalSourceRef;
  LRole: TNyxText;
  LName: TNyxText;
begin
  Result := Default(TNyxPascalFileDrafts);

  if AData = '' then
  begin
    Exit;
  end;
  LData := TNyxDataValue.ParseJSON(AData);

  if (LData.Kind <> ndObject) or (LData.Count <> 2) or
    (LData.Field('version').ToJSON <> '1') then
  begin
    raise ENyxPascalProject.Create('Unsupported Pascal helper-buffer packet');
  end;
  LItems := LData.Field('items');

  if (LItems.Kind <> ndArray) or (LItems.Count > 4095) then
  begin
    raise ENyxPascalProject.Create('Pascal helper-buffer membership exceeds its bound');
  end;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LItem := LItems.Item(LIndex);

    if (LItem.Kind <> ndObject) or (LItem.Count <> 4) then
    begin
      raise ENyxPascalProject.Create('A Pascal helper-buffer entry has an invalid shape');
    end;
    LRole := LItem.Field('kind').AsText;
    LName := LItem.Field('name').AsText;

    if LRole = 'unit' then
    begin
      LFile.FRole := npsUnit;
    end
    else if LRole = 'include' then
    begin
      LFile.FRole := npsInclude;
    end
    else
    begin
      raise ENyxPascalProject.Create('A helper buffer requires its closed unit/include role');
    end;
    LFile.FName := LName;
    LFile.Validate;

    if Result.Contains(LFile) then
    begin
      raise ENyxPascalProject.Create('Duplicate Pascal helper buffer');
    end;
    SetLength(Result.FEntries, LIndex + 1);
    Result.FEntries[LIndex].FFile := LFile;
    Result.FEntries[LIndex].FText := LItem.Field('text').AsText;
    Result.FEntries[LIndex].FBase := LItem.Field('base').AsText;
    ValidateBuffer(Result.FEntries[LIndex].FText);
    ValidateBuffer(Result.FEntries[LIndex].FBase);
  end;
end;

function TNyxPascalFileDrafts.StorageBytes: TNyxTextBytes;
var
  LIndex: Integer;
begin
  Result := 0;
  for LIndex := 0 to High(FEntries) do
  begin
    Result := Result + NyxUTF8ByteCount(FEntries[LIndex].FFile.Name) +
      NyxUTF8ByteCount(FEntries[LIndex].FText) + NyxUTF8ByteCount(FEntries[LIndex].FBase);
  end;
end;

end.
