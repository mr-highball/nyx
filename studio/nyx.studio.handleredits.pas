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
unit nyx.studio.handleredits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.callbacks, nyx.studio.session, nyx.studio.projects;

const
  NyxMaximumHandlerChanges = 16;
  NyxMaximumHandlerCharacters = 32768;
  NyxMaximumHandlerPatchCharacters = 131072;

type
  { An implementation edit has a distinct Pascal handler reference and exact
    expected text. Signature, imports, sibling helpers and the managed builder
    are retained. Strings here are authored Pascal, not property/behavior tags. }
  TNyxHandlerEdit = record
  private
    FHandler: TNyxHandlerRef;
    FExpected: TNyxText;
    FImplementation: TNyxText;
  public
    property Handler: TNyxHandlerRef read FHandler;
    property Expected: TNyxText read FExpected;
    property Code: TNyxText read FImplementation;
  end;
  TNyxHandlerEditResult = record
    Handler: TNyxHandlerRef;
    SourceLine: Integer;
  end;
  TNyxHandlerEditResults = array of TNyxHandlerEditResult;

  { Immutable, reference-counted commands retain no supplier session/document.
    Candidate validates a detached complete companion and returns owned paired
    text. Caller checks response size before one live AdoptProject publication.
    Rejected/no-op edits never clear drafts, replace selections or spend history. }
  INyxHandlerPatch = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001004003001}']
    function Candidate(ASession: TNyxStudioSession;
      out AResults: TNyxHandlerEditResults): TNyxProjectPair;
  end;

function NyxHandlerEdit(const AHandler: TNyxHandlerRef;
  const AExpected, AImplementation: TNyxText): TNyxHandlerEdit;
function NewNyxHandlerPatch(const AChanges: array of TNyxHandlerEdit): INyxHandlerPatch;
{ JSON is the explicit semantic transport boundary. Every change has exactly
  handler/expected/implementation fields; wrong types or unknown keys refuse. }
function ReadNyxHandlerPatch(const AChanges: TNyxDataValue): INyxHandlerPatch;
function EncodeNyxHandlerResults(const AResults: TNyxHandlerEditResults): TNyxDataValue;

implementation

uses
  nyx.model, nyx.codec, nyx.source;

type
  TNyxHandlerPatch = class(TInterfacedObject, INyxHandlerPatch)
  private
    FChanges: array of TNyxHandlerEdit;
  public
    constructor Create(const AChanges: array of TNyxHandlerEdit);
    function Candidate(ASession: TNyxStudioSession;
      out AResults: TNyxHandlerEditResults): TNyxProjectPair;
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
      raise ENyxModel.Create('Malformed Unicode in callback implementation');
    end;
    Inc(Result);

    if Result > NyxMaximumHandlerCharacters then
    begin
      raise ENyxModel.Create('Callback implementation exceeds 32768 Unicode characters');
    end;
  end;
end;

function NyxHandlerEdit(const AHandler: TNyxHandlerRef;
  const AExpected, AImplementation: TNyxText): TNyxHandlerEdit;
begin
  Result := Default(TNyxHandlerEdit);
  Result.FHandler := NyxHandler(AHandler.Name);
  CharacterCount(AExpected);
  CharacterCount(AImplementation);
  Result.FExpected := AExpected;
  Result.FImplementation := AImplementation;
end;

constructor TNyxHandlerPatch.Create(const AChanges: array of TNyxHandlerEdit);
var
  LIndex: Integer;
  LPrior: Integer;
  LCharacters: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumHandlerChanges) then
  begin
    raise ENyxModel.Create('A Pascal implementation patch requires 1..16 changes');
  end;
  LCharacters := 0;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    FChanges[LIndex] := NyxHandlerEdit(AChanges[LIndex].Handler,
      AChanges[LIndex].Expected, AChanges[LIndex].Code);
    Inc(LCharacters, CharacterCount(AChanges[LIndex].Expected));
    Inc(LCharacters, CharacterCount(AChanges[LIndex].Code));

    if LCharacters > NyxMaximumHandlerPatchCharacters then
    begin
      raise ENyxModel.Create('Grouped callback text exceeds 131072 Unicode characters');
    end;
    for LPrior := 0 to LIndex - 1 do
    begin

      if SameText(FChanges[LPrior].Handler.Name, FChanges[LIndex].Handler.Name) then
      begin
        raise ENyxModel.Create('Replace each callback implementation once per grouped patch');
      end;
    end;
  end;
end;

function TNyxHandlerPatch.Candidate(ASession: TNyxStudioSession;
  out AResults: TNyxHandlerEditResults): TNyxProjectPair;
var
  LWorkspace: TNyxSourceWorkspace;
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LIndex: Integer;
  LHandler: TNyxHandlerSource;
begin
  AResults := nil;

  if ASession = nil then
  begin
    raise ENyxModel.Create('Callback edits require an admitted Studio session');
  end;
  Result := ASession.ProjectSnapshot;

  if Result.Pending then
  begin
    raise ENyxModel.Create('Resolve the pending draft before editing callback implementations');
  end;
  LSource := Result.Source;
  for LIndex := 0 to High(FChanges) do
  begin
    LSource := ReplaceNyxHandlerImplementation(LSource, FChanges[LIndex].Handler,
      FChanges[LIndex].Expected, FChanges[LIndex].Code);
  end;
  LWorkspace := TNyxSourceWorkspace.Create;
  LDocument := nil;
  try
    { Reconstruct the entire admitted contract before returning any candidate.
      A handler-only edit must not alter the portable design, even indirectly. }
    LDocument := LWorkspace.Candidate(ASession.Document, LSource);

    if TNyxCodec.Encode(LDocument) <> Result.Design then
    begin
      raise ENyxModel.Create('Callback implementation edits must retain the exact design');
    end;
    LWorkspace.Accept(LDocument, LSource);
    Result := NyxProjectPair(Result.Design, LSource);
    SetLength(AResults, Length(FChanges));
    for LIndex := 0 to High(FChanges) do
    begin
      LHandler := ReadNyxHandlerSource(LSource, FChanges[LIndex].Handler);
      AResults[LIndex].Handler := FChanges[LIndex].Handler;
      AResults[LIndex].SourceLine := LHandler.Line;
    end;
  finally
    LDocument.Free;
    LWorkspace.Free;
  end;
end;

function NewNyxHandlerPatch(const AChanges: array of TNyxHandlerEdit): INyxHandlerPatch;
begin
  Result := TNyxHandlerPatch.Create(AChanges);
end;

function ReadNyxHandlerPatch(const AChanges: TNyxDataValue): INyxHandlerPatch;
var
  LChanges: array of TNyxHandlerEdit;
  LValue: TNyxDataValue;
  LIndex: Integer;
  LKey: Integer;
begin

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumHandlerChanges) then
  begin
    raise ENyxModel.Create('A Pascal implementation patch requires an array of 1..16 changes');
  end;
  SetLength(LChanges, AChanges.Count);
  for LIndex := 0 to High(LChanges) do
  begin
    LValue := AChanges.Item(LIndex);

    if LValue.Kind <> ndObject then
    begin
      raise ENyxModel.Create('A callback implementation change must be an object');
    end;
    for LKey := 0 to LValue.Count - 1 do
    begin

      if (LValue.Key(LKey) <> 'handler') and (LValue.Key(LKey) <> 'expected') and
        (LValue.Key(LKey) <> 'implementation') then
      begin
        raise ENyxModel.Create('Unknown callback implementation field: ' + LValue.Key(LKey));
      end;
    end;
    LChanges[LIndex] := NyxHandlerEdit(NyxHandler(LValue.Field('handler').AsText),
      LValue.Field('expected').AsText, LValue.Field('implementation').AsText);
  end;
  Result := NewNyxHandlerPatch(LChanges);
end;

function EncodeNyxHandlerResults(const AResults: TNyxHandlerEditResults): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(AResults));
  for LIndex := 0 to High(AResults) do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('handler', NyxData(AResults[LIndex].Handler.Name)),
      NyxField('line', NyxData(AResults[LIndex].SourceLine))]);
  end;
  Result := NyxArray(LItems);
end;

end.
