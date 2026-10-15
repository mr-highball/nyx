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
unit nyx.logic.catalog;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.text.search, nyx.data, nyx.flow.types, nyx.logic,
  nyx.programs.declarations;

type
  { Immutable discovery collection. WithOperation captures descriptor/default
    configuration and returns a new catalog; no executable creator, document or
    host capability is retained. The editor can expose unknown creator schemas
    while runtime resolution remains an independent explicit host decision. }
  INyxLogicCatalog = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000013}']
    function GetCount: Integer;
    function Descriptor(AIndex: Integer): TNyxLogicDescriptor;
    function Configuration(AIndex: Integer): TNyxDataValue;
    function WithOperation(const AOperation: INyxLogicOperation): INyxLogicCatalog;
    function Matches(AIndex: Integer; const AQuery: TNyxText): Boolean;
    property Count: Integer read GetCount;
  end;

{ Default collection includes inert HTTP/open/save settings requiring explicit
  runtime hosts. File opening returns one bounded complete byte snapshot and
  metadata; it grants no saved path authority or broader filesystem access. }
function NewNyxLogicCatalog: INyxLogicCatalog; overload;
{ Add one typed program template when a compatible declaration exists. Its
  configuration names the first such program; the public form offers all names.
  Discovery captures schemas only and never compiles or runs a program. }
function NewNyxLogicCatalog(const APrograms: INyxProgramDeclarations): INyxLogicCatalog; overload;
function NyxLogicGroupName(AGroup: TNyxLogicGroup): TNyxText;
function NyxFlowTypeName(AType: TNyxFlowType): TNyxText;

implementation

uses nyx.logic.collections, nyx.logic.json.editing, nyx.logic.http, nyx.http.types, nyx.resource.sources,
  nyx.logic.relations, nyx.logic.joins, nyx.logic.regex,
  nyx.regex.types, nyx.regex.replacement,
  nyx.logic.files, nyx.files.save, nyx.logic.files.open, nyx.files.open,
  nyx.logic.programs;

type
  { Owned immutable discovery snapshot of an operation's schema and defaults;
    host authority and implementation objects are never retained in entries. }
  TCatalogEntry = record
    Descriptor: TNyxLogicDescriptor;
    Configuration: TNyxDataValue;
  end;
  { Reference-counted immutable toolbox index. WithOperation builds independent
    entry snapshots; query matching consumes descriptions, intent groups and tags. }
  TCatalog = class(TInterfacedObject, INyxLogicCatalog)
  private
    FEntries: array of TCatalogEntry;
    procedure CheckIndex(AIndex: Integer);
  public
    function GetCount: Integer;
    function Descriptor(AIndex: Integer): TNyxLogicDescriptor;
    function Configuration(AIndex: Integer): TNyxDataValue;
    function WithOperation(const AOperation: INyxLogicOperation): INyxLogicCatalog;
    function Matches(AIndex: Integer; const AQuery: TNyxText): Boolean;
  end;

function NyxLogicGroupName(AGroup: TNyxLogicGroup): TNyxText;
const
  CNames: array[TNyxLogicGroup] of TNyxText =
    ('Files', 'JSON', 'Collections', 'Text', 'Network', 'Data', 'Flow');
begin

  if (Ord(AGroup) < Ord(Low(TNyxLogicGroup))) or (Ord(AGroup) > Ord(High(TNyxLogicGroup))) then
  begin
    raise ENyxFlow.Create('Unknown Logic group');
  end;
  Result := CNames[AGroup];
end;

function NyxFlowTypeName(AType: TNyxFlowType): TNyxText;
const
  CNames: array[TNyxFlowType] of TNyxText =
    ('Text', 'Boolean', 'Integer', 'Number', 'JSON', 'Bytes', 'Signal');
begin

  if (Ord(AType) < Ord(Low(TNyxFlowType))) or (Ord(AType) > Ord(High(TNyxFlowType))) then
  begin
    raise ENyxFlow.Create('Unknown Logic value family');
  end;
  Result := CNames[AType];
end;

procedure TCatalog.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Length(FEntries)) then
  begin
    raise ENyxFlow.Create('Logic catalog index is unavailable');
  end;
end;

function TCatalog.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TCatalog.Descriptor(AIndex: Integer): TNyxLogicDescriptor;
begin
  CheckIndex(AIndex);
  Result := TNyxLogicDescriptor.FromData(FEntries[AIndex].Descriptor.ToData);
end;

function TCatalog.Configuration(AIndex: Integer): TNyxDataValue;
begin
  CheckIndex(AIndex);
  Result := FEntries[AIndex].Configuration.Copy;
end;

function TCatalog.WithOperation(const AOperation: INyxLogicOperation): INyxLogicCatalog;
var
  LCopy: TCatalog;
  LDescriptor: TNyxLogicDescriptor;
  LConfiguration: TNyxDataValue;
  LIndex: Integer;
  LPosition: Integer;
  LKeepAlive: INyxLogicCatalog;
  LOperation: INyxLogicOperation;
begin

  if AOperation = nil then
  begin
    raise ENyxFlow.Create('Logic discovery requires a typed operation');
  end;
  { Creator getters may retire caller variables. Keep the receiver and input
    alive independently until their complete immutable snapshots are captured. }
  LKeepAlive := Self;
  LOperation := AOperation;
  LDescriptor := TNyxLogicDescriptor.FromData(LOperation.Descriptor.ToData);
  LConfiguration := LOperation.Configuration.Copy;
  LPosition := Length(FEntries);
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Descriptor.Kind.Name = LDescriptor.Kind.Name then
    begin
      LPosition := LIndex;
      Break;
    end;
  end;

  if LPosition >= 128 then
  begin
    raise ENyxFlow.Create('Logic discovery collection is full');
  end;
  LCopy := TCatalog.Create;
  Result := LCopy;
  { Copy each record explicitly: browser dynamic-array copies may retain record
    objects. Replacing one catalog entry must never rewrite the original. }
  SetLength(LCopy.FEntries, Length(FEntries));
  for LIndex := 0 to High(FEntries) do
  begin
    LCopy.FEntries[LIndex].Descriptor := Descriptor(LIndex);
    LCopy.FEntries[LIndex].Configuration := Configuration(LIndex);
  end;

  if LPosition = Length(FEntries) then
  begin
    SetLength(LCopy.FEntries, LPosition + 1);
  end;
  LCopy.FEntries[LPosition].Descriptor := LDescriptor;
  LCopy.FEntries[LPosition].Configuration := LConfiguration;
end;

function TCatalog.Matches(AIndex: Integer; const AQuery: TNyxText): Boolean;
var
  LDescriptor: TNyxLogicDescriptor;
  LText: TNyxText;
  LQuery: TNyxText;
  LIndex: Integer;
  LStart: Integer;
begin
  LDescriptor := Descriptor(AIndex);
  LText := LDescriptor.Kind.Name + ' ' + LDescriptor.Title + ' ' +
    LDescriptor.Description + ' ' + NyxLogicGroupName(LDescriptor.Group);
  for LIndex := 0 to LDescriptor.LabelCount - 1 do
  begin
    LText := LText + ' ' + LDescriptor.LabelAt(LIndex);
  end;
  LText := NyxFoldText(LText);
  LQuery := NyxFoldText(AQuery);
  LStart := 1;
  Result := True;
  for LIndex := 1 to Length(LQuery) + 1 do
  begin

    if (LIndex > Length(LQuery)) or (LQuery[LIndex] <= ' ') then
    begin

      if (LIndex > LStart) and
        (Pos(Copy(LQuery, LStart, LIndex - LStart), LText) = 0) then
      begin
        Exit(False);
      end;
      LStart := LIndex + 1;
    end;
  end;
end;

function NewNyxLogicCatalog: INyxLogicCatalog;
begin
  Result := TCatalog.Create;
  Result := Result.WithOperation(NewNyxJSONParse)
    .WithOperation(NewNyxJSONSelect(NyxDataPath))
    .WithOperation(NewNyxJSONFormat)
    .WithOperation(NewNyxTextJoin)
    .WithOperation(NewNyxJSONFilter(NyxDataPath, njcEqual, NyxData(True)))
    .WithOperation(NewNyxJSONSort(NyxDataPath, njkText))
    .WithOperation(NewNyxJSONMap(NyxDataPath))
    .WithOperation(NewNyxJSONSlice(0, 20))
    .WithOperation(NewNyxUTF8Decode)
    .WithOperation(NewNyxHTTPCall(NyxHTTPRequest(
      NyxResourceURL('https://example.invalid/api'))))
    .WithOperation(NewNyxUTF8Encode)
    .WithOperation(NewNyxSaveFile(NyxFileSaveOptions('report.json')
      .MediaType(NyxMediaType(nfmtJSON)).Allow('json')))
    .WithOperation(NewNyxOpenFile(NyxFileOpenOptions.Allow('json').Allow('txt')))
    .WithOperation(NewNyxJSONSet(NyxDataPath))
    .WithOperation(NewNyxJSONRemove(NyxDataPath.Field('obsolete'), njmIgnore))
    .WithOperation(NewNyxJSONMerge)
    .WithOperation(NewNyxJSONGroup(NyxDataPath, njkText))
    .WithOperation(NewNyxJSONDistinct(NyxDataPath, njkText))
    .WithOperation(NewNyxJSONJoin(NyxDataPath, NyxDataPath, njkText))
    .WithOperation(NewNyxJSONAggregate(NyxDataPath, njaSum))
    .WithOperation(NewNyxFlowJoin(NyxFlowJoinOptions(nftText)
      .Input(NyxFlowPort('primary')).Input(NyxFlowPort('fallback'))))
    .WithOperation(NewNyxRegexMatch(NyxRegex('(?<word>\w+)')))
    .WithOperation(NewNyxRegexReplace(NyxRegex('(?<word>\w+)'),
      NyxRegexReplacement.Literal('[').Capture(NyxRegexGroup('word')).Literal(']')))
    .WithOperation(NewNyxRegexExtract(NyxRegex('(?<word>\w+)'),
      NyxRegexCapture(NyxRegexGroup('word'))));
end;

function NewNyxLogicCatalog(const APrograms: INyxProgramDeclarations): INyxLogicCatalog;
var
  LPrograms: INyxProgramDeclarations;
  LDefinition: INyxProgramDefinition;
  LReference: TNyxProgramRef;
  LIndex: Integer;
begin
  LPrograms := APrograms;
  Result := NewNyxLogicCatalog;

  if LPrograms = nil then
  begin
    Exit;
  end;
  LPrograms := LPrograms.Clone;

  if LPrograms = nil then
  begin
    raise ENyxFlow.Create('Program discovery requires independent declaration membership');
  end;
  for LIndex := 0 to LPrograms.Count - 1 do
  begin
    LReference := LPrograms.Reference(LIndex);
    LDefinition := LPrograms.Definition(LReference);

    if NyxProgramLogicContractSupported(LDefinition.Contract) then
    begin
      Result := Result.WithOperation(NewNyxProgramLogic(LReference, LDefinition.Contract));
      Exit;
    end;
  end;
end;

end.
