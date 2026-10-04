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

unit nyx.studio.projects;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model,
  nyx.source;

type
  { Resolution is a deliberate authoring decision, never guessed from timestamps.
    Pascal may win only within the admitted source subset. Choosing the design
    retains the conflicting Pascal verbatim as a pending draft. }
  TNyxProjectResolution = (nprRequireMatch, nprUsePascal, nprUseDesign);

  { One portable recovery/export value. Accepted design/source travel together;
    a pending draft keeps its original source baseline, even when empty or stale.
    There are no compiler paths, DOM handles or service identities in this data. }
  TNyxProjectPair = record
    Design: TNyxText;
    Source: TNyxText;
    Draft: TNyxText;
    DraftBase: TNyxText;
    Pending: Boolean;
  end;

  { Distinguishes a valid but divergent pair from malformed/unsupported input.
    Callers retain the original packet until the user chooses a resolution. }
  ENyxProjectConflict = class(ENyxModel);

{ Initializes a pair from adjacent files. Parsing/admission is deliberately a
  separate operation so file import can retain conflicting input without mutation. }
function NyxProjectPair(const ADesign, ASource: TNyxText): TNyxProjectPair;

{ Strict versioned transport. Decode checks shape/types/budgets, not model/source
  agreement; Admit must run before replacing accepted application state. }
function EncodeNyxProject(const APair: TNyxProjectPair): TNyxText;
function DecodeNyxProject(const AText: TNyxText): TNyxProjectPair;

{ Stages both owned objects and any resolved draft before publication. On failure
  outputs are nil, input is untouched, and no accepted document is borrowed or
  mutated. On success the caller owns both outputs and the resolved pair. }
procedure AdmitNyxProject(const APair: TNyxProjectPair;
  AResolution: TNyxProjectResolution; out ADocument: TNyxDocument;
  out AWorkspace: TNyxSourceWorkspace; out AResolved: TNyxProjectPair);

{ Service project names are portable ASCII directory keys, independent of title
  and Pascal unit name. No separators, dot segments or device names are admitted. }
procedure ValidateNyxProjectName(const AName: TNyxText);

implementation

uses
  SysUtils,
  nyx.data,
  nyx.codec,
  nyx.codegen;

function NyxProjectPair(const ADesign, ASource: TNyxText): TNyxProjectPair;
begin
  Result.Design := ADesign;
  Result.Source := ASource;
  Result.Draft := '';
  Result.DraftBase := '';
  Result.Pending := False;
end;

function EncodeNyxProject(const APair: TNyxProjectPair): TNyxText;
begin
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('design', NyxData(APair.Design)),
    NyxField('source', NyxData(APair.Source)),
    NyxField('draft', NyxData(APair.Draft)),
    NyxField('draftBase', NyxData(APair.DraftBase)),
    NyxField('pending', NyxData(APair.Pending))
  ]).ToJSON;
end;

function DecodeNyxProject(const AText: TNyxText): TNyxProjectPair;
var
  LData: TNyxDataValue;
begin
  LData := TNyxDataValue.ParseJSON(AText);

  if (LData.Kind <> ndObject) or (LData.Count <> 6) or
    (LData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Unsupported Nyx project packet');
  end;
  Result.Design := LData.Field('design').AsText;
  Result.Source := LData.Field('source').AsText;
  Result.Draft := LData.Field('draft').AsText;
  Result.DraftBase := LData.Field('draftBase').AsText;
  Result.Pending := LData.Field('pending').AsBoolean;

  if not Result.Pending and ((Result.Draft <> '') or (Result.DraftBase <> '')) then
  begin
    raise ENyxModel.Create('A project without a pending draft must have empty draft fields');
  end;
end;

procedure AdmitNyxProject(const APair: TNyxProjectPair;
  AResolution: TNyxProjectResolution; out ADocument: TNyxDocument;
  out AWorkspace: TNyxSourceWorkspace; out AResolved: TNyxProjectPair);
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  ADocument := nil;
  AWorkspace := nil;
  LDocument := nil;
  LCandidate := nil;
  LWorkspace := nil;
  LResolved := DecodeNyxProject(EncodeNyxProject(APair));
  try
    LDocument := TNyxCodec.Decode(APair.Design);
    LWorkspace := TNyxSourceWorkspace.Create;

    if AResolution = nprUseDesign then
    begin
      { Never overwrite a second independent draft to make a mismatch disappear.
        The complete input remains exportable for a deliberate manual merge. }

      if APair.Pending and (APair.Draft <> APair.Source) then
      begin
        raise ENyxProjectConflict.Create(
          'This project has two Pascal buffers. Download the input and merge them before choosing the design');
      end;
      LResolved.Source := TNyxCodegen.Generate(LDocument);
      LResolved.Pending := APair.Source <> LResolved.Source;

      if LResolved.Pending then
      begin
        LResolved.Draft := APair.Source;
        LResolved.DraftBase := LResolved.Source;
      end
      else
      begin
        LResolved.Draft := '';
        LResolved.DraftBase := '';
      end;
    end
    else
    begin
      NyxCompanionUnitName(APair.Source);
      LCandidate := LWorkspace.Candidate(LDocument, APair.Source, True);

      if TNyxCodec.Encode(LCandidate) <> TNyxCodec.Encode(LDocument) then
      begin

        if AResolution = nprRequireMatch then
        begin
          raise ENyxProjectConflict.Create(
            'The design and Pascal describe different values. Choose which version to open');
        end;
        LDocument.Free;
        LDocument := LCandidate;
        LCandidate := nil;
      end;
    end;
    LResolved.Design := TNyxCodec.Encode(LDocument);
    LWorkspace.Accept(LDocument, LResolved.Source);
    AResolved := LResolved;
    ADocument := LDocument;
    LDocument := nil;
    AWorkspace := LWorkspace;
    LWorkspace := nil;
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

procedure ValidateNyxProjectName(const AName: TNyxText);
var
  LIndex: Integer;
  LUpper: TNyxText;
begin

  if (Length(AName) < 1) or (Length(AName) > 64) then
  begin
    raise ENyxModel.Create('Project file name must contain 1..64 letters, digits, - or _');
  end;
  for LIndex := 1 to Length(AName) do
  begin

    if not (AName[LIndex] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
    begin
      raise ENyxModel.Create('Project file name contains an unsupported character');
    end;
  end;
  LUpper := UpperCase(AName);

  if (LUpper = 'CON') or (LUpper = 'PRN') or (LUpper = 'AUX') or
    (LUpper = 'NUL') or (LUpper = 'CLOCK') or
    ((Length(LUpper) = 4) and ((Copy(LUpper, 1, 3) = 'COM') or
    (Copy(LUpper, 1, 3) = 'LPT')) and (LUpper[4] in ['0'..'9'])) then
  begin
    raise ENyxModel.Create('Project file name is reserved by the platform');
  end;
end;

end.
