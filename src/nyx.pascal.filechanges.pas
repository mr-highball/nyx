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

unit nyx.pascal.filechanges;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts;

type
  { Closed inventory intent. Main source is never created/removed here; editing
    existing text remains the separate selected-file Apply contract. }
  TNyxPascalFileChangeKind = (npfcCreate, npfcRemove);

  { Immutable source-data proposal, independent of an editor/compiler/target.
    Create refuses existing identities, and Remove requires an existing helper.
    Preparing a candidate neither compiles it nor establishes execution identity.
    New helpers are not silently added to uses/include clauses: callers author
    those dependencies explicitly, and normal compilation checks used inputs. }
  TNyxPascalFileChange = record
  private
    FDefined: Boolean;
    FKind: TNyxPascalFileChangeKind;
    FFile: TNyxPascalSourceRef;
    FText: TNyxText;
  public
    { Default templates are ordinary Delphi-dialect UTF-8 Pascal. The validated
      unit reference supplies its declaration; include names retain .inc. }
    class function CreateUnit(const AUnit: TNyxPascalUnitRef): TNyxPascalFileChange; static;
    class function CreateInclude(const AInclude: TNyxPascalIncludeRef): TNyxPascalFileChange; static;
    class function Remove(const AFile: TNyxPascalSourceRef): TNyxPascalFileChange; static;
    { Override only a creation's copied text. Complete project capture validates
      encoding, size, declaration and include inventory before compiler effects. }
    function WithText(const AText: TNyxText): TNyxPascalFileChange;
    procedure Validate;
    { Refuse collisions/missing removals and return an independent complete
      candidate. No accepted source, buffer or previous provider is mutated. }
    function Candidate(const AProject: INyxPascalProject): INyxPascalProject;
    property IsDefined: Boolean read FDefined;
    property Kind: TNyxPascalFileChangeKind read FKind;
    property FileReference: TNyxPascalSourceRef read FFile;
    property Text: TNyxText read FText;
  end;

  { Copied chronological membership intents. Stage keeps intermediate dependency
    gaps private so callers can repair uses/include text in the same candidate.
    It never grants compilation or publication authority. Candidate validates the
    complete final project; both methods retain independent immutable inputs. }
  INyxPascalFileChanges = interface(IInterface)
    ['{68F89A18-71A1-4D73-84BC-265587A0343B}']
    function Stage(const AProject: INyxPascalProject): INyxPascalProject;
    function Candidate(const AProject: INyxPascalProject): INyxPascalProject;
  end;

{ Freeze 1..16 typed helper changes. Main removal/creation always refuses. }
function NyxPascalFileChanges(const AChanges: array of TNyxPascalFileChange): INyxPascalFileChanges;

implementation

uses SysUtils;

type
  { Detached membership vector; intermediate projects remain private source data,
    never accepted compiler inputs. No session, editor or application is retained. }
  TNyxPascalFileChanges = class(TInterfacedObject, INyxPascalFileChanges)
  private
    FChanges: array of TNyxPascalFileChange;
  public
    constructor Create(const AChanges: array of TNyxPascalFileChange);
    function Stage(const AProject: INyxPascalProject): INyxPascalProject;
    function Candidate(const AProject: INyxPascalProject): INyxPascalProject;
  end;

class function TNyxPascalFileChange.CreateUnit(
  const AUnit: TNyxPascalUnitRef): TNyxPascalFileChange;
begin
  Result := Default(TNyxPascalFileChange);
  Result.FDefined := True;
  Result.FKind := npfcCreate;
  Result.FFile := TNyxPascalSourceRef.UnitFile(AUnit);
  Result.FText := '{ Application helper unit. Keep reusable Pascal behavior here. }' + #10 +
    'unit ' + AUnit.Name + ';' + #10 + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 + #10 +
    'interface' + #10 + #10 + 'implementation' + #10 + #10 + 'end.' + #10;
end;

class function TNyxPascalFileChange.CreateInclude(
  const AInclude: TNyxPascalIncludeRef): TNyxPascalFileChange;
begin
  Result := Default(TNyxPascalFileChange);
  Result.FDefined := True;
  Result.FKind := npfcCreate;
  Result.FFile := TNyxPascalSourceRef.IncludeFile(AInclude);
  Result.FText := '{ Application include fragment. Its including unit owns compiler directives. }' + #10;
end;

class function TNyxPascalFileChange.Remove(
  const AFile: TNyxPascalSourceRef): TNyxPascalFileChange;
begin
  Result := Default(TNyxPascalFileChange);
  Result.FDefined := True;
  Result.FKind := npfcRemove;
  Result.FFile := AFile;
  Result.Validate;
end;

procedure TNyxPascalFileChange.Validate;
begin
  FFile.Validate;

  if not FDefined or (FFile.Role = npsMain) or
    (Ord(FKind) < Ord(Low(TNyxPascalFileChangeKind))) or
    (Ord(FKind) > Ord(High(TNyxPascalFileChangeKind))) then
  begin
    raise ENyxPascalProject.Create('File management requires a defined supporting-file intent');
  end;

  if (FKind = npfcRemove) and (FText <> '') then
  begin
    raise ENyxPascalProject.Create('File removal cannot replace source text');
  end;
end;

function TNyxPascalFileChange.WithText(const AText: TNyxText): TNyxPascalFileChange;
begin
  Validate;

  if FKind <> npfcCreate then
  begin
    raise ENyxPascalProject.Create('Only a new file can supply initial text');
  end;
  Result := Self;
  Result.FText := AText;
end;

function StageFileChange(const AChange: TNyxPascalFileChange;
  const AProject: INyxPascalProject): INyxPascalProject;
var
  LIndex: Integer;
  LFound: Boolean;
begin
  AChange.Validate;
  Result := AProject;
  LFound := False;
  for LIndex := 0 to Result.Count - 1 do
  begin

    if SameText(String(Result.Input(LIndex).Name), String(AChange.FileReference.Name)) then
    begin

      if (AChange.Kind = npfcRemove) and
        (Result.Input(LIndex).Name <> AChange.FileReference.Name) then
      begin
        raise ENyxPascalProject.Create('Removal requires the exact captured file identity');
      end;
      LFound := True;
      Break;
    end;
  end;

  if ((AChange.Kind = npfcCreate) and LFound) or
    ((AChange.Kind = npfcRemove) and not LFound) then
  begin
    raise ENyxPascalProject.Create('Creating requires a new file; removing requires an existing file');
  end;

  if AChange.FileReference.Role = npsUnit then
  begin

    if AChange.Kind = npfcCreate then
    begin
      Result := Result.WithUnit(NyxPascalUnit(Copy(AChange.FileReference.Name, 1,
        Length(AChange.FileReference.Name) - 4)), AChange.Text);
    end
    else
    begin
      Result := Result.WithoutUnit(NyxPascalUnit(Copy(AChange.FileReference.Name, 1,
        Length(AChange.FileReference.Name) - 4)));
    end;
  end
  else if AChange.Kind = npfcCreate then
  begin
    Result := Result.WithInclude(NyxPascalInclude(AChange.FileReference.Name), AChange.Text);
  end
  else
  begin
    Result := Result.WithoutInclude(NyxPascalInclude(AChange.FileReference.Name));
  end;
end;

constructor TNyxPascalFileChanges.Create(const AChanges: array of TNyxPascalFileChange);
var
  LIndex: Integer;
  LScan: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > 16) then
  begin
    raise ENyxPascalProject.Create('File membership requires 1..16 changes');
  end;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    AChanges[LIndex].Validate;
    for LScan := 0 to LIndex - 1 do
    begin

      if SameText(String(AChanges[LScan].FileReference.Name),
        String(AChanges[LIndex].FileReference.Name)) then
      begin
        raise ENyxPascalProject.Create('Each membership target appears once per group');
      end;
    end;
    FChanges[LIndex] := AChanges[LIndex];
  end;
end;

function TNyxPascalFileChanges.Stage(const AProject: INyxPascalProject): INyxPascalProject;
var
  LIndex: Integer;
begin
  Result := CaptureNyxPascalProject(AProject);
  for LIndex := 0 to High(FChanges) do
  begin
    Result := StageFileChange(FChanges[LIndex], Result);
  end;
end;

function TNyxPascalFileChanges.Candidate(const AProject: INyxPascalProject): INyxPascalProject;
begin
  Result := CaptureNyxPascalProject(Stage(AProject));
end;

function NyxPascalFileChanges(const AChanges: array of TNyxPascalFileChange): INyxPascalFileChanges;
begin
  Result := TNyxPascalFileChanges.Create(AChanges);
end;

function TNyxPascalFileChange.Candidate(const AProject: INyxPascalProject): INyxPascalProject;
begin
  Result := NyxPascalFileChanges([Self]).Candidate(AProject);
end;

end.
