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
unit nyx.pascal.project;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.pascal.names;

type
  { Invalid project input is distinct from an actual compiler diagnostic. No
    filesystem/job/editor effect is permitted before a complete capture admits. }
  ENyxPascalProject = class(Exception);

  { Closed compiler-input role. Embedded runtime data/images remain Resources;
    these entries contain Pascal source, never linker commands or machine paths. }
  TNyxPascalInputKind = (npiUnit, npiInclude);

  { Distinct portable include leaf. ASCII compiler names have a .inc suffix and
    no path/device aliases; source text retains the full Unicode contract. }
  TNyxPascalIncludeRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

  { Copied host-side payload bounds. Defaults admit 128 files including the main
    unit and 32 MiB total UTF-8. Callers may tune up to 4096 files/256 MiB.
    Individual auxiliary files retain the text-file boundary's 4 MiB limit;
    the main unit retains the constructor boundary's 1 MiB limit. No option
    selects a target, compiler, search path, external file or execution policy. }
  TNyxPascalProjectLimits = record
  private
    FMaximumFiles: Integer;
    FMaximumBytes: Integer;
  public
    class function Defaults: TNyxPascalProjectLimits; static;
    function UpToFiles(AValue: Integer): TNyxPascalProjectLimits;
    function UpToBytes(AValue: Integer): TNyxPascalProjectLimits;
    procedure Validate;
    property MaximumFiles: Integer read FMaximumFiles;
    property MaximumBytes: Integer read FMaximumBytes;
  end;

  { Independent immutable source entry. Name is the exact compiler leaf, not a
    path. UnitReference refuses for include fragments. Text/byte count survive
    project disposal and borrow no caller stream, buffer, editor or file. }
  INyxPascalInput = interface(IInterface)
    ['{68FE918B-457C-45BE-B2E0-CA2300000001}']
    function Kind: TNyxPascalInputKind;
    function Name: TNyxText;
    function Text: TNyxText;
    function ByteCount: Integer;
    function UnitReference: TNyxPascalUnitRef;
  end;

  { Persistent compiler project value. The main source is separate; Count and
    Input enumerate auxiliary entries in authored order. Each edit returns an
    independent snapshot; an old snapshot never changes. Proposed snapshots may
    reference includes added later: Validate admits the complete inventory before
    compilation. WithUnit/WithInclude replace only the same canonical identity;
    case-only filename aliases refuse. Missing removals are idempotent.

    This is source data, not construction provenance or a serialized project
    packet. Capture freezes alternate implementations and validates exact bytes;
    only an actually executed constructor can establish its application meaning. }
  INyxPascalProject = interface(IInterface)
    ['{68FE918B-457C-45BE-B2E0-CA2300000002}']
    function MainUnit: TNyxPascalUnitRef;
    function Source: TNyxText;
    function Limits: TNyxPascalProjectLimits;
    function Count: Integer;
    function Input(AIndex: Integer): INyxPascalInput;
    function ByteCount: Integer;
    function WithSource(const ASource: TNyxText): INyxPascalProject;
    function WithUnit(const AUnit: TNyxPascalUnitRef;
      const ASource: TNyxText): INyxPascalProject;
    function WithInclude(const AInclude: TNyxPascalIncludeRef;
      const ASource: TNyxText): INyxPascalProject;
    function WithoutUnit(const AUnit: TNyxPascalUnitRef): INyxPascalProject;
    function WithoutInclude(const AInclude: TNyxPascalIncludeRef): INyxPascalProject;
    procedure Validate;
  end;

  { Immutable supporting-file value for project/history records. Only canonical
    managed text is retained: pas2js records cannot own COM interface fields.
    The main unit's text stays with its source frame, without a second escaped
    copy. Capture freezes alternate providers; Open reconstructs an independent
    complete input snapshot against the supplied exact main source. Neither
    decoding nor restoring this data proves constructor execution. Default is
    absent, preserving the legacy single-unit checkpoint/packet bytes. }
  TNyxPascalProjectFiles = record
  private
    FData: TNyxText;
    function GetIsEmpty: Boolean;
    function GetStorageBytes: TNyxTextBytes;
  public
    class function Capture(const AProject: INyxPascalProject): TNyxPascalProjectFiles; static;
    { Explicit persistence boundary. Malformed shape, aliases, unknown roles,
      references and payload overflow refuse before a value is returned. }
    class function Parse(const AData, AMainSource: TNyxText): TNyxPascalProjectFiles; static;
    function Open(const AMainSource: TNyxText): INyxPascalProject;
    property IsEmpty: Boolean read GetIsEmpty;
    property Encoded: TNyxText read FData;
    property StorageBytes: TNyxTextBytes read GetStorageBytes;
  end;

function NyxPascalInclude(const AName: TNyxText): TNyxPascalIncludeRef;
{ No file is read. The supplied main source is copied and budget-admitted;
  complete namespace/include admission follows Validate/Capture after authoring. }
function NewNyxPascalProject(const AUnit: TNyxPascalUnitRef;
  const ASource: TNyxText): INyxPascalProject; overload;
function NewNyxPascalProject(const AUnit: TNyxPascalUnitRef;
  const ASource: TNyxText; const ALimits: TNyxPascalProjectLimits): INyxPascalProject; overload;
{ Freeze every public getter into an owned implementation, distrust claimed byte
  counts/Validate, then check complete unit headers and allowed include references.
  The caller serializes a mutable alternate provider during synchronous capture;
  the returned value has no provider borrow and is safe for retained worker reads.
  Failure returns no partial value and grants no filesystem/compiler authority. }
function CaptureNyxPascalProject(const AProject: INyxPascalProject): INyxPascalProject;

implementation

uses nyx.files, nyx.bytes, nyx.source, nyx.data;

type
  { Entry owns one immutable text-file payload; unit identity is copied only for
    unit entries. No project reference points back from an entry. }
  TPascalInput = class(TInterfacedObject, INyxPascalInput)
  private
    FKind: TNyxPascalInputKind;
    FFile: INyxTextFile;
    FUnit: TNyxPascalUnitRef;
  public
    function Kind: TNyxPascalInputKind;
    function Name: TNyxText;
    function Text: TNyxText;
    function ByteCount: Integer;
    function UnitReference: TNyxPascalUnitRef;
  end;

  { Private independently copied vector. Entries are immutable interface values;
    modifying any vector never aliases another snapshot's membership/order. }
  TPascalInputs = array of INyxPascalInput;

  { Immutable default implementation with cached exact UTF-8 size. Edits copy
    only the interface vector and the changed entry, not all retained source text.
    No filesystem, compiler, application document or mutable builder is retained. }
  TPascalProject = class(TInterfacedObject, INyxPascalProject)
  private
    FUnit: TNyxPascalUnitRef;
    FSource: TNyxText;
    FLimits: TNyxPascalProjectLimits;
    FInputs: TPascalInputs;
    FBytes: Integer;
    FSourceBytes: Integer;
    function CopyValue: TPascalProject;
    function Find(const AName: TNyxText): Integer;
    function WithInput(const AInput: INyxPascalInput): INyxPascalProject;
    function WithoutInput(const AName: TNyxText): INyxPascalProject;
  public
    function MainUnit: TNyxPascalUnitRef;
    function Source: TNyxText;
    function Limits: TNyxPascalProjectLimits;
    function Count: Integer;
    function Input(AIndex: Integer): INyxPascalInput;
    function ByteCount: Integer;
    function WithSource(const ASource: TNyxText): INyxPascalProject;
    function WithUnit(const AUnit: TNyxPascalUnitRef;
      const ASource: TNyxText): INyxPascalProject;
    function WithInclude(const AInclude: TNyxPascalIncludeRef;
      const ASource: TNyxText): INyxPascalProject;
    function WithoutUnit(const AUnit: TNyxPascalUnitRef): INyxPascalProject;
    function WithoutInclude(const AInclude: TNyxPascalIncludeRef): INyxPascalProject;
    procedure Validate;
  end;

{ Windows device aliases apply to every extension. Compiler input names use
  ASCII deliberately; this never rewrites the Unicode text contained in them. }
procedure AdmitLeaf(const AName: TNyxText);
var
  LIndex: Integer;
  LBase: TNyxText;
begin

  if (AName = '') or (Length(AName) > 124) or (AName[1] = '.') or
    (AName[Length(AName)] = '.') or (Pos('..', AName) > 0) then
  begin
    raise ENyxPascalProject.Create('A compiler input needs a portable leaf name');
  end;
  for LIndex := 1 to Length(AName) do
  begin

    if not (AName[LIndex] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) then
    begin
      raise ENyxPascalProject.Create('Compiler input names use ASCII leaves without paths');
    end;
  end;
  LIndex := Pos('.', AName);
  LBase := LowerCase(Copy(AName, 1, LIndex - 1));

  if (LBase = 'con') or (LBase = 'prn') or (LBase = 'aux') or (LBase = 'nul') or
    ((Length(LBase) = 4) and (LBase[4] in ['1'..'9']) and
      ((Copy(LBase, 1, 3) = 'com') or (Copy(LBase, 1, 3) = 'lpt'))) then
  begin
    raise ENyxPascalProject.Create('Compiler input names cannot alias a device');
  end;
end;

function NyxPascalInclude(const AName: TNyxText): TNyxPascalIncludeRef;
begin
  AdmitLeaf(AName);

  if not SameText(Copy(AName, Length(AName) - 3, 4), '.inc') then
  begin
    raise ENyxPascalProject.Create('A Pascal include leaf needs the .inc suffix');
  end;
  Result.FName := AName;
end;

class function TNyxPascalProjectLimits.Defaults: TNyxPascalProjectLimits;
begin
  Result.FMaximumFiles := 128;
  Result.FMaximumBytes := 32 * 1024 * 1024;
end;

procedure TNyxPascalProjectLimits.Validate;
begin

  if (FMaximumFiles < 1) or (FMaximumFiles > 4096) or
    (FMaximumBytes < 1) or (FMaximumBytes > 256 * 1024 * 1024) then
  begin
    raise ENyxPascalProject.Create('Project limits require 1..4096 files and 1..268435456 bytes');
  end;
end;

function TNyxPascalProjectLimits.UpToFiles(AValue: Integer): TNyxPascalProjectLimits;
begin
  Result := Self;
  Result.FMaximumFiles := AValue;
  Result.Validate;
end;

function TNyxPascalProjectLimits.UpToBytes(AValue: Integer): TNyxPascalProjectLimits;
begin
  Result := Self;
  Result.FMaximumBytes := AValue;
  Result.Validate;
end;

function TPascalInput.Kind: TNyxPascalInputKind;
begin
  Result := FKind;
end;

function TPascalInput.Name: TNyxText;
begin
  Result := FFile.Name;
end;

function TPascalInput.Text: TNyxText;
begin
  Result := FFile.Text;
end;

function TPascalInput.ByteCount: Integer;
begin
  Result := FFile.ByteCount;
end;

function TPascalInput.UnitReference: TNyxPascalUnitRef;
begin

  if FKind <> npiUnit then
  begin
    raise ENyxPascalProject.Create('An include fragment has no Pascal unit reference');
  end;
  Result := FUnit;
end;

function NewInput(AKind: TNyxPascalInputKind; const AName, ASource: TNyxText;
  const AUnit: TNyxPascalUnitRef): INyxPascalInput;
var
  LInput: TPascalInput;
begin
  AdmitLeaf(AName);
  LInput := TPascalInput.Create;
  try
    LInput.FKind := AKind;
    LInput.FFile := NyxTextFile(AName, ASource);
    LInput.FUnit := AUnit;
    Result := LInput;
    LInput := nil;
  finally
    LInput.Free;
  end;
end;

function NewNyxPascalProject(const AUnit: TNyxPascalUnitRef;
  const ASource: TNyxText): INyxPascalProject;
begin
  Result := NewNyxPascalProject(AUnit, ASource, TNyxPascalProjectLimits.Defaults);
end;

function NewNyxPascalProject(const AUnit: TNyxPascalUnitRef;
  const ASource: TNyxText; const ALimits: TNyxPascalProjectLimits): INyxPascalProject;
var
  LProject: TPascalProject;
  LUnit: TNyxPascalUnitRef;
  LBytes: Integer;
begin
  ALimits.Validate;
  LUnit := NyxPascalUnit(AUnit.Name);
  AdmitLeaf(LUnit.Name + '.pas');

  if SameText(LUnit.Name, 'nyx_projection') or SameText(LUnit.Name, 'nyx_preview') or
    SameText(LUnit.Name, 'nyx_native') then
  begin
    raise ENyxPascalProject.Create('The main unit cannot alias an application/compiler entry point');
  end;
  LBytes := NyxUTF8ByteCount(ASource);

  if (LBytes = 0) or (LBytes > 1024 * 1024) or (LBytes > ALimits.MaximumBytes) then
  begin
    raise ENyxPascalProject.Create('The main unit must fit the 1 MiB and complete project budgets');
  end;
  LProject := TPascalProject.Create;
  try
    LProject.FUnit := LUnit;
    LProject.FSource := ASource;
    LProject.FLimits := ALimits;
    LProject.FBytes := LBytes;
    LProject.FSourceBytes := LBytes;
    Result := LProject;
    LProject := nil;
  finally
    LProject.Free;
  end;
end;

function TPascalProject.MainUnit: TNyxPascalUnitRef;
begin
  Result := FUnit;
end;

function TPascalProject.Source: TNyxText;
begin
  Result := FSource;
end;

function TPascalProject.Limits: TNyxPascalProjectLimits;
begin
  Result := FLimits;
end;

function TPascalProject.Count: Integer;
begin
  Result := Length(FInputs);
end;

function TPascalProject.Input(AIndex: Integer): INyxPascalInput;
begin

  if (AIndex < 0) or (AIndex >= Length(FInputs)) then
  begin
    raise ENyxPascalProject.Create('Project input index is outside this snapshot');
  end;
  Result := FInputs[AIndex];
end;

function TPascalProject.ByteCount: Integer;
begin
  Result := FBytes;
end;

function TPascalProject.Find(const AName: TNyxText): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to High(FInputs) do
  begin

    if SameText(FInputs[LIndex].Name, AName) then
    begin
      Exit(LIndex);
    end;
  end;
end;

function TPascalProject.CopyValue: TPascalProject;
begin
  Result := TPascalProject.Create;
  try
    Result.FUnit := FUnit;
    Result.FSource := FSource;
    Result.FLimits := FLimits;
    Result.FInputs := Copy(FInputs, 0, Length(FInputs));
    Result.FBytes := FBytes;
    Result.FSourceBytes := FSourceBytes;
  except
    Result.Free;
    raise;
  end;
end;

function TPascalProject.WithSource(const ASource: TNyxText): INyxPascalProject;
var
  LCopy: TPascalProject;
  LBytes: Integer;
begin
  LBytes := NyxUTF8ByteCount(ASource);

  if (LBytes = 0) or (LBytes > 1024 * 1024) or
    (FBytes - FSourceBytes + LBytes > FLimits.MaximumBytes) then
  begin
    raise ENyxPascalProject.Create('The proposed main source exceeds the project budget');
  end;
  LCopy := CopyValue;
  try
    LCopy.FBytes := FBytes - FSourceBytes + LBytes;
    LCopy.FSourceBytes := LBytes;
    LCopy.FSource := ASource;
    Result := LCopy;
    LCopy := nil;
  finally
    LCopy.Free;
  end;
end;

function TPascalProject.WithInput(const AInput: INyxPascalInput): INyxPascalProject;
var
  LCopy: TPascalProject;
  LIndex: Integer;
  LBytes: Integer;
begin

  if SameText(AInput.Name, FUnit.Name + '.pas') then
  begin
    raise ENyxPascalProject.Create('An auxiliary unit cannot replace the main source');
  end;
  LIndex := Find(AInput.Name);
  LBytes := FBytes + AInput.ByteCount;

  if LIndex >= 0 then
  begin

    if FInputs[LIndex].Name <> AInput.Name then
    begin
      raise ENyxPascalProject.Create('Case-only compiler input aliases are ambiguous');
    end;
    Dec(LBytes, FInputs[LIndex].ByteCount);
  end
  else
  begin

    if Length(FInputs) + 2 > FLimits.MaximumFiles then
    begin
      raise ENyxPascalProject.Create('The proposed input exceeds the complete project file budget');
    end;
  end;

  if LBytes > FLimits.MaximumBytes then
  begin
    raise ENyxPascalProject.Create('The proposed input exceeds the complete project byte budget');
  end;
  LCopy := CopyValue;
  try

    if LIndex < 0 then
    begin
      LIndex := Length(LCopy.FInputs);
      SetLength(LCopy.FInputs, LIndex + 1);
    end;
    LCopy.FInputs[LIndex] := AInput;
    LCopy.FBytes := LBytes;
    Result := LCopy;
    LCopy := nil;
  finally
    LCopy.Free;
  end;
end;

function TPascalProject.WithUnit(const AUnit: TNyxPascalUnitRef;
  const ASource: TNyxText): INyxPascalProject;
var
  LUnit: TNyxPascalUnitRef;
begin
  LUnit := NyxPascalUnit(AUnit.Name);
  Result := WithInput(NewInput(npiUnit, LUnit.Name + '.pas', ASource, LUnit));
end;

function TPascalProject.WithInclude(const AInclude: TNyxPascalIncludeRef;
  const ASource: TNyxText): INyxPascalProject;
var
  LInclude: TNyxPascalIncludeRef;
begin
  LInclude := NyxPascalInclude(AInclude.Name);
  Result := WithInput(NewInput(npiInclude, LInclude.Name, ASource,
    Default(TNyxPascalUnitRef)));
end;

function TPascalProject.WithoutInput(const AName: TNyxText): INyxPascalProject;
var
  LCopy: TPascalProject;
  LIndex: Integer;
  LNext: Integer;
begin
  LIndex := Find(AName);

  if LIndex < 0 then
  begin
    Exit(Self);
  end;
  LCopy := CopyValue;
  try
    Dec(LCopy.FBytes, FInputs[LIndex].ByteCount);
    for LNext := LIndex to High(LCopy.FInputs) - 1 do
    begin
      LCopy.FInputs[LNext] := LCopy.FInputs[LNext + 1];
    end;
    SetLength(LCopy.FInputs, Length(LCopy.FInputs) - 1);
    Result := LCopy;
    LCopy := nil;
  finally
    LCopy.Free;
  end;
end;

function TPascalProject.WithoutUnit(const AUnit: TNyxPascalUnitRef): INyxPascalProject;
begin
  Result := WithoutInput(NyxPascalUnit(AUnit.Name).Name + '.pas');
end;

function TPascalProject.WithoutInclude(const AInclude: TNyxPascalIncludeRef): INyxPascalProject;
begin
  Result := WithoutInput(NyxPascalInclude(AInclude.Name).Name);
end;

procedure TPascalProject.Validate;
var
  LIncludes: TNyxStrings;
  LIndex: Integer;
  LName: TNyxText;
begin
  LIncludes := TNyxStrings.Create;
  try
    for LIndex := 0 to High(FInputs) do
    begin

      if FInputs[LIndex].Kind = npiInclude then
      begin
        LIncludes.Add(FInputs[LIndex].Name);
      end;
    end;
    LName := NyxCompanionUnitName(FSource, LIncludes);

    if not SameText(LName, FUnit.Name) then
    begin
      raise ENyxPascalProject.Create('The main source declares another unit namespace');
    end;
    for LIndex := 0 to High(FInputs) do
    begin

      if FInputs[LIndex].Kind = npiUnit then
      begin
        LName := NyxCompanionUnitName(FInputs[LIndex].Text, LIncludes);

        if not SameText(LName, FInputs[LIndex].UnitReference.Name) then
        begin
          raise ENyxPascalProject.Create('An auxiliary source declares another unit namespace');
        end;
      end
      else
      begin
        ValidateNyxPascalFileReferences(FInputs[LIndex].Text, LIncludes);
      end;
    end;
  finally
    LIncludes.Free;
  end;
end;

function CaptureNyxPascalProject(const AProject: INyxPascalProject): INyxPascalProject;
var
  LCount: Integer;
  LIndex: Integer;
  LInput: INyxPascalInput;
  LSnapshot: INyxPascalProject;
  LLimits: TNyxPascalProjectLimits;
  LName: TNyxText;
  LUnit: TNyxPascalUnitRef;
  LKind: Integer;
begin

  if AProject = nil then
  begin
    raise ENyxPascalProject.Create('A captured Pascal project cannot be empty');
  end;
  LLimits := AProject.Limits;
  LLimits.Validate;
  LCount := AProject.Count;

  if (LCount < 0) or (LCount >= LLimits.MaximumFiles) then
  begin
    raise ENyxPascalProject.Create('Captured input count exceeds the project budget');
  end;
  LSnapshot := NewNyxPascalProject(AProject.MainUnit, AProject.Source, LLimits);
  for LIndex := 0 to LCount - 1 do
  begin
    LInput := AProject.Input(LIndex);

    if LInput = nil then
    begin
      raise ENyxPascalProject.Create('A captured project has an empty input entry');
    end;
    LName := LInput.Name;
    LKind := Ord(LInput.Kind);
    case LKind of
      Ord(npiUnit):
        begin
          LUnit := LInput.UnitReference;

          if LName <> LUnit.Name + '.pas' then
          begin
            raise ENyxPascalProject.Create('A captured unit name does not match its compiler leaf');
          end;
          LSnapshot := LSnapshot.WithUnit(LUnit, LInput.Text);
        end;
      Ord(npiInclude):
        LSnapshot := LSnapshot.WithInclude(NyxPascalInclude(LName), LInput.Text);
      else
        raise ENyxPascalProject.Create('A captured project has an unknown input kind');
    end;

    if LSnapshot.Count <> LIndex + 1 then
    begin
      raise ENyxPascalProject.Create('A captured project repeats an input identity');
    end;
  end;
  LSnapshot.Validate;
  Result := LSnapshot;
end;

function TNyxPascalProjectFiles.GetIsEmpty: Boolean;
begin
  Result := FData = '';
end;

function TNyxPascalProjectFiles.GetStorageBytes: TNyxTextBytes;
begin
  Result := Length(FData);
  {$ifdef PAS2JS}
  Result := Result * 2;
  {$endif}
end;

class function TNyxPascalProjectFiles.Capture(
  const AProject: INyxPascalProject): TNyxPascalProjectFiles;
var
  LProject: INyxPascalProject;
  LInput: INyxPascalInput;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LRole: TNyxText;
begin
  LProject := CaptureNyxPascalProject(AProject);
  SetLength(LItems, LProject.Count);
  for LIndex := 0 to LProject.Count - 1 do
  begin
    LInput := LProject.Input(LIndex);

    if LInput.Kind = npiUnit then
    begin
      LRole := 'unit';
    end
    else
    begin
      LRole := 'include';
    end;
    LItems[LIndex] := NyxObject([
      NyxField('kind', NyxData(LRole)),
      NyxField('name', NyxData(LInput.Name)),
      NyxField('text', NyxData(LInput.Text))]);
  end;
  { This is the existing portable JSON boundary, with its own 4 MiB encoded
    limit. A larger valid compiler project cannot silently become a smaller
    saved project: serialization refuses before editor publication instead. }
  Result.FData := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('main', NyxData(LProject.MainUnit.Name)),
    NyxField('maximumFiles', NyxData(LProject.Limits.MaximumFiles)),
    NyxField('maximumBytes', NyxData(LProject.Limits.MaximumBytes)),
    NyxField('files', NyxArray(LItems))]).ToJSON;
end;

function TNyxPascalProjectFiles.Open(
  const AMainSource: TNyxText): INyxPascalProject;
var
  LData: TNyxDataValue;
  LFiles: TNyxDataValue;
  LItem: TNyxDataValue;
  LLimits: TNyxPascalProjectLimits;
  LRole: TNyxText;
  LName: TNyxText;
  LIndex: Integer;
begin
  Result := nil;

  if IsEmpty then
  begin
    Exit;
  end;
  LData := TNyxDataValue.ParseJSON(FData);

  if (LData.Kind <> ndObject) or (LData.Count <> 5) or
    (LData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxPascalProject.Create('Unsupported supporting Pascal files');
  end;
  LLimits := TNyxPascalProjectLimits.Defaults
    .UpToFiles(LData.Field('maximumFiles').AsInteger)
    .UpToBytes(LData.Field('maximumBytes').AsInteger);
  LFiles := LData.Field('files');

  if (LFiles.Kind <> ndArray) or (LFiles.Count >= LLimits.MaximumFiles) then
  begin
    raise ENyxPascalProject.Create('Supporting files exceed their declared count');
  end;
  Result := NewNyxPascalProject(NyxPascalUnit(LData.Field('main').AsText),
    AMainSource, LLimits);
  for LIndex := 0 to LFiles.Count - 1 do
  begin
    LItem := LFiles.Item(LIndex);

    if (LItem.Kind <> ndObject) or (LItem.Count <> 3) then
    begin
      raise ENyxPascalProject.Create('A supporting file requires kind, name and text');
    end;
    LRole := LItem.Field('kind').AsText;
    LName := LItem.Field('name').AsText;

    if LRole = 'unit' then
    begin

      if Copy(LName, Length(LName) - 3, 4) <> '.pas' then
      begin
        raise ENyxPascalProject.Create('A supporting unit requires its exact Pascal leaf');
      end;
      Result := Result.WithUnit(NyxPascalUnit(Copy(LName, 1, Length(LName) - 4)),
        LItem.Field('text').AsText);
    end
    else if LRole = 'include' then
    begin
      Result := Result.WithInclude(NyxPascalInclude(LName), LItem.Field('text').AsText);
    end
    else
    begin
      raise ENyxPascalProject.Create('Unknown supporting file role');
    end;

    if Result.Count <> LIndex + 1 then
    begin
      raise ENyxPascalProject.Create('Supporting files repeat a compiler identity');
    end;
  end;
  Result.Validate;
end;

class function TNyxPascalProjectFiles.Parse(
  const AData, AMainSource: TNyxText): TNyxPascalProjectFiles;
var
  LValue: TNyxPascalProjectFiles;
begin
  LValue.FData := AData;

  if LValue.IsEmpty then
  begin
    Exit(Default(TNyxPascalProjectFiles));
  end;
  Result := Capture(LValue.Open(AMainSource));
end;

end.
