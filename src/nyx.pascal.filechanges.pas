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

implementation

uses SysUtils;

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

function TNyxPascalFileChange.Candidate(
  const AProject: INyxPascalProject): INyxPascalProject;
var
  LIndex: Integer;
  LFound: Boolean;
begin
  Validate;
  Result := CaptureNyxPascalProject(AProject);
  LFound := False;
  for LIndex := 0 to Result.Count - 1 do
  begin

    if SameText(String(Result.Input(LIndex).Name), String(FFile.Name)) then
    begin

      if (FKind = npfcRemove) and (Result.Input(LIndex).Name <> FFile.Name) then
      begin
        raise ENyxPascalProject.Create('Removal requires the exact captured file identity');
      end;
      LFound := True;
      Break;
    end;
  end;

  if ((FKind = npfcCreate) and LFound) or ((FKind = npfcRemove) and not LFound) then
  begin
    raise ENyxPascalProject.Create('Creating requires a new file; removing requires an existing file');
  end;

  if FFile.Role = npsUnit then
  begin

    if FKind = npfcCreate then
    begin
      Result := Result.WithUnit(NyxPascalUnit(Copy(FFile.Name, 1, Length(FFile.Name) - 4)), FText);
    end
    else
    begin
      Result := Result.WithoutUnit(NyxPascalUnit(Copy(FFile.Name, 1, Length(FFile.Name) - 4)));
    end;
  end
  else if FKind = npfcCreate then
  begin
    Result := Result.WithInclude(NyxPascalInclude(FFile.Name), FText);
  end
  else
  begin
    Result := Result.WithoutInclude(NyxPascalInclude(FFile.Name));
  end;
  Result.Validate;
end;

end.
