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
unit nyx.pascal.sources;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts;

type
  { Source origin for inspection only. Editing buffers may be empty, malformed
    Pascal or declare another namespace. Neither choice grants admission or
    construction provenance; compiler projects remain a different interface. }
  TNyxPascalSourceSnapshotKind = (pssAccepted, pssEditing);

  { Immutable, value-owned browsing inputs over fixed accepted membership.
    Count includes main at zero. Text is the exact inspected buffer; AcceptedText
    retains its captured accepted comparison, never a rebased editing history.
    WithBuffer cannot add/rename/remove files and accepts unfinished text, up to
    4 MiB per file and the project's total byte budget. Copies retain no editor,
    document, mutable provider, filesystem or compiler capability. }
  INyxPascalSourceSnapshot = interface(IInterface)
    ['{68F89A18-71A1-4D73-84BC-265587A0343E}']
    function Kind: TNyxPascalSourceSnapshotKind;
    function MainUnit: TNyxPascalUnitRef;
    function Limits: TNyxPascalProjectLimits;
    function Count: Integer;
    function FileReference(AIndex: Integer): TNyxPascalSourceRef;
    function Text(const AFile: TNyxPascalSourceRef): TNyxText;
    function AcceptedText(const AFile: TNyxPascalSourceRef): TNyxText;
    function WithBuffer(const AFile: TNyxPascalSourceRef;
      const AText: TNyxText): INyxPascalSourceSnapshot;
  end;

{ Freeze a compiler project's accepted membership/text, with its ordinary
  structural validation. The result is inspectable data, never a project suitable
  for admission/execution. No output target or compiler is required. }
function CaptureNyxPascalSources(const AProject: INyxPascalProject): INyxPascalSourceSnapshot; overload;
{ Freeze an alternate inspection provider once before lexical reads. Validate
  encoding, role/name uniqueness, main position and budgets without parsing draft
  syntax or opening includes. The caller serializes mutable providers during this
  call; returned getters never consult them afterward. }
function CaptureNyxPascalSources(const ASources: INyxPascalSourceSnapshot): INyxPascalSourceSnapshot; overload;

implementation

uses SysUtils, nyx.bytes;

type
  { One immutable physical text pair. Only strings/value references are stored,
    keeping native and pas2js record ownership alike. }
  TSourceFile = record
    FileReference: TNyxPascalSourceRef;
    Accepted: TNyxText;
    Buffer: TNyxText;
  end;

  { Persistent inspection vector; edits detach the vector and changed text.
    Main identity and accepted filenames survive incomplete namespace edits. }
  TSourceSnapshot = class(TInterfacedObject, INyxPascalSourceSnapshot)
  private
    FKind: TNyxPascalSourceSnapshotKind;
    FMain: TNyxPascalUnitRef;
    FLimits: TNyxPascalProjectLimits;
    FFiles: array of TSourceFile;
    FBytes: Integer;
    function Find(const AFile: TNyxPascalSourceRef): Integer;
  public
    function Kind: TNyxPascalSourceSnapshotKind;
    function MainUnit: TNyxPascalUnitRef;
    function Limits: TNyxPascalProjectLimits;
    function Count: Integer;
    function FileReference(AIndex: Integer): TNyxPascalSourceRef;
    function Text(const AFile: TNyxPascalSourceRef): TNyxText;
    function AcceptedText(const AFile: TNyxPascalSourceRef): TNyxText;
    function WithBuffer(const AFile: TNyxPascalSourceRef;
      const AText: TNyxText): INyxPascalSourceSnapshot;
  end;

function BufferBytes(const AText: TNyxText): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  Result := NyxUTF8ByteCount(AText);

  if Result > 4 * 1024 * 1024 then
  begin
    raise ENyxPascalProject.Create('An inspected Pascal buffer exceeds 4 MiB');
  end;
  LIndex := 1;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxPascalProject.Create('An inspected Pascal buffer has malformed Unicode');
    end;
  end;
end;

function TSourceSnapshot.Kind: TNyxPascalSourceSnapshotKind;
begin
  Result := FKind;
end;

function TSourceSnapshot.MainUnit: TNyxPascalUnitRef;
begin
  Result := FMain;
end;

function TSourceSnapshot.Limits: TNyxPascalProjectLimits;
begin
  Result := FLimits;
end;

function TSourceSnapshot.Count: Integer;
begin
  Result := Length(FFiles);
end;

function TSourceSnapshot.Find(const AFile: TNyxPascalSourceRef): Integer;
var
  LIndex: Integer;
begin
  AFile.Validate;
  for LIndex := 0 to High(FFiles) do
  begin

    if FFiles[LIndex].FileReference.SameFile(AFile) then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxPascalProject.Create('The inspected file is outside captured membership');
end;

function TSourceSnapshot.FileReference(AIndex: Integer): TNyxPascalSourceRef;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalProject.Create('Inspected source index is outside this snapshot');
  end;
  Result := FFiles[AIndex].FileReference;
end;

function TSourceSnapshot.Text(const AFile: TNyxPascalSourceRef): TNyxText;
begin
  Result := FFiles[Find(AFile)].Buffer;
end;

function TSourceSnapshot.AcceptedText(const AFile: TNyxPascalSourceRef): TNyxText;
begin
  Result := FFiles[Find(AFile)].Accepted;
end;

function TSourceSnapshot.WithBuffer(const AFile: TNyxPascalSourceRef;
  const AText: TNyxText): INyxPascalSourceSnapshot;
var
  LIndex: Integer;
  LBytes: Integer;
  LCopy: TSourceSnapshot;
begin
  LIndex := Find(AFile);
  LBytes := FBytes - NyxUTF8ByteCount(FFiles[LIndex].Buffer) + BufferBytes(AText);

  if LBytes > FLimits.MaximumBytes then
  begin
    raise ENyxPascalProject.Create('Inspected buffers exceed the project byte budget');
  end;

  if (FKind = pssEditing) and (FFiles[LIndex].Buffer = AText) then
  begin
    Exit(Self);
  end;
  LCopy := TSourceSnapshot.Create;
  Result := LCopy;
  LCopy.FKind := pssEditing;
  LCopy.FMain := FMain;
  LCopy.FLimits := FLimits;
  LCopy.FBytes := LBytes;
  LCopy.FFiles := Copy(FFiles, 0, Length(FFiles));
  LCopy.FFiles[LIndex].Buffer := AText;
end;

function CaptureNyxPascalSources(const AProject: INyxPascalProject): INyxPascalSourceSnapshot;
var
  LProject: INyxPascalProject;
  LCopy: TSourceSnapshot;
  LIndex: Integer;
begin
  LProject := CaptureNyxPascalProject(AProject);
  LCopy := TSourceSnapshot.Create;
  Result := LCopy;
  LCopy.FMain := LProject.MainUnit;
  LCopy.FLimits := LProject.Limits;
  LCopy.FBytes := LProject.ByteCount;
  SetLength(LCopy.FFiles, LProject.Count + 1);
  LCopy.FFiles[0].FileReference := TNyxPascalSourceRef.Main;
  LCopy.FFiles[0].Buffer := LProject.Source;
  LCopy.FFiles[0].Accepted := LProject.Source;
  for LIndex := 0 to LProject.Count - 1 do
  begin

    if LProject.Input(LIndex).Kind = npiUnit then
    begin
      LCopy.FFiles[LIndex + 1].FileReference :=
        TNyxPascalSourceRef.UnitFile(LProject.Input(LIndex).UnitReference);
    end
    else
    begin
      LCopy.FFiles[LIndex + 1].FileReference :=
        TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LProject.Input(LIndex).Name));
    end;
    LCopy.FFiles[LIndex + 1].Buffer := LProject.Input(LIndex).Text;
    LCopy.FFiles[LIndex + 1].Accepted := LProject.Input(LIndex).Text;
  end;
end;

function CaptureNyxPascalSources(const ASources: INyxPascalSourceSnapshot): INyxPascalSourceSnapshot;
var
  LCopy: TSourceSnapshot;
  LIndex: Integer;
  LPrior: Integer;
  LAcceptedBytes: Integer;
  LCount: Integer;
begin

  if ASources = nil then
  begin
    raise ENyxPascalProject.Create('Inspection requires captured Pascal sources');
  end;
  LCopy := TSourceSnapshot.Create;
  Result := LCopy;
  LCopy.FKind := ASources.Kind;
  LCopy.FMain := NyxPascalUnit(ASources.MainUnit.Name);
  LCopy.FLimits := ASources.Limits;
  LCopy.FLimits.Validate;
  LCount := ASources.Count;

  if (Ord(LCopy.FKind) < Ord(Low(TNyxPascalSourceSnapshotKind))) or
    (Ord(LCopy.FKind) > Ord(High(TNyxPascalSourceSnapshotKind))) or
    (LCount < 1) or (LCount > LCopy.FLimits.MaximumFiles) then
  begin
    raise ENyxPascalProject.Create('Inspection has an invalid origin or file count');
  end;
  SetLength(LCopy.FFiles, LCount);
  LAcceptedBytes := 0;
  for LIndex := 0 to High(LCopy.FFiles) do
  begin
    LCopy.FFiles[LIndex].FileReference := ASources.FileReference(LIndex);
    LCopy.FFiles[LIndex].FileReference.Validate;

    if (LIndex = 0) <> (LCopy.FFiles[LIndex].FileReference.Role = npsMain) then
    begin
      raise ENyxPascalProject.Create('Inspection requires exactly one main source at zero');
    end;
    for LPrior := 0 to LIndex - 1 do
    begin

      if SameText(LCopy.FFiles[LPrior].FileReference.Name,
        LCopy.FFiles[LIndex].FileReference.Name) or
        ((LCopy.FFiles[LIndex].FileReference.Role <> npsMain) and
        SameText(LCopy.FFiles[LIndex].FileReference.Name, LCopy.FMain.Name + '.pas')) then
      begin
        raise ENyxPascalProject.Create('Inspected filenames have ambiguous ownership');
      end;
    end;
    LCopy.FFiles[LIndex].Buffer := ASources.Text(LCopy.FFiles[LIndex].FileReference);
    LCopy.FFiles[LIndex].Accepted := ASources.AcceptedText(LCopy.FFiles[LIndex].FileReference);

    if (LCopy.FKind = pssAccepted) and
      (LCopy.FFiles[LIndex].Buffer <> LCopy.FFiles[LIndex].Accepted) then
    begin
      raise ENyxPascalProject.Create('Accepted inspection cannot contain editing overlays');
    end;
    Inc(LCopy.FBytes, BufferBytes(LCopy.FFiles[LIndex].Buffer));
    Inc(LAcceptedBytes, BufferBytes(LCopy.FFiles[LIndex].Accepted));

    if (LCopy.FBytes > LCopy.FLimits.MaximumBytes) or
      (LAcceptedBytes > LCopy.FLimits.MaximumBytes) then
    begin
      raise ENyxPascalProject.Create('Inspected source pairs exceed the project byte budget');
    end;
  end;
end;

end.
