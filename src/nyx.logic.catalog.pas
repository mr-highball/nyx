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

uses SysUtils, nyx.text, nyx.text.search, nyx.data, nyx.flow.types, nyx.logic;

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

{ Default collection contains the currently executable built-ins. It does not
  advertise unimplemented Files/Network/query/regex entries as usable components. }
function NewNyxLogicCatalog: INyxLogicCatalog;
function NyxLogicGroupName(AGroup: TNyxLogicGroup): TNyxText;
function NyxFlowTypeName(AType: TNyxFlowType): TNyxText;

implementation

uses nyx.logic.collections;

type
  TCatalogEntry = record
    Descriptor: TNyxLogicDescriptor;
    Configuration: TNyxDataValue;
  end;
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
    .WithOperation(NewNyxJSONSlice(0, 20));
end;

end.
