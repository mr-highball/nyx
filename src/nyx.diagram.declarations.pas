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
unit nyx.diagram.declarations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.diagram, nyx.canvas.types;

const
  NyxDiagramsWireField = 'diagrams';
  MaximumNyxDiagramDefinitions = 64;
  MaximumNyxDiagramFamilyVertices = 4096;
  MaximumNyxDiagramFamilyEdges = 8192;
  MaximumNyxDiagramFamilyBytes = 4194304;

type
  { Exact case-sensitive document definition identity. Names are independent of
    vertices, edges, controls and executable graphs; default is absent.
    The reference retains no registry/document and grants no runtime authority. }
  TNyxDiagramRef = record
  private
    FName: TNyxText;
  public
    function Defined: Boolean;
    property Name: TNyxText read FName;
  end;

  { Document-owned mutable membership over immutable private graph snapshots.
    Define captures even a caller implementation once, normalizes before adoption
    and preserves an existing name's position. Clone has independent membership;
    returned graphs may outlive the registry/document. All mutations belong to
    one authoring thread; reentrant mutation during caller capture refuses.
    These declarations contain visual intent only, never UI/source authority. }
  INyxDiagramDeclarations = interface(IInterface)
    ['{D53A78C1-A864-4F71-8D93-73C57D501601}']
    function Define(const AReference: TNyxDiagramRef;
      const ADiagram: INyxDiagram): INyxDiagramDeclarations;
    function Remove(const AReference: TNyxDiagramRef): INyxDiagramDeclarations;
    function Contains(const AReference: TNyxDiagramRef): Boolean;
    function Definition(const AReference: TNyxDiagramRef): INyxDiagram;
    function Reference(AIndex: Integer): TNyxDiagramRef;
    function GetCount: Integer;
    function Clone: INyxDiagramDeclarations;
    procedure Validate;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

{ Validate an exact open identity; empty, malformed and over-budget names refuse. }
function NyxDiagramRef(const AName: TNyxText): TNyxDiagramRef;
{ New independent empty membership, with no borrowed document or runtime state. }
function NewNyxDiagramDeclarations: INyxDiagramDeclarations;
{ Strict persistence boundary: ordered name/diagram entries, complete family
  and per-graph budgets, exact fields and unique names. Failure publishes nothing. }
function NyxDiagramDeclarationsFromData(const AData: TNyxDataValue): INyxDiagramDeclarations;

implementation

type
  { Private parallel storage avoids interface fields in portable records.
    Definitions always hold normalized immutable graphs; only membership arrays
    change. Busy reserves mutations across foreign ToData callouts. }
  TDeclarations = class(TInterfacedObject, INyxDiagramDeclarations)
  private
    FReferences: array of TNyxDiagramRef;
    FDefinitions: array of INyxDiagram;
    FBusy: Boolean;
    function IndexOf(const AReference: TNyxDiagramRef): Integer;
    procedure RequireWritable;
  public
    function Define(const AReference: TNyxDiagramRef;
      const ADiagram: INyxDiagram): INyxDiagramDeclarations;
    function Remove(const AReference: TNyxDiagramRef): INyxDiagramDeclarations;
    function Contains(const AReference: TNyxDiagramRef): Boolean;
    function Definition(const AReference: TNyxDiagramRef): INyxDiagram;
    function Reference(AIndex: Integer): TNyxDiagramRef;
    function GetCount: Integer;
    function Clone: INyxDiagramDeclarations;
    procedure Validate;
    function ToData: TNyxDataValue;
  end;

function TNyxDiagramRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NyxDiagramRef(const AName: TNyxText): TNyxDiagramRef;
begin
  Result.FName := NyxCanvasRef(AName).Name;
end;

function NewNyxDiagramDeclarations: INyxDiagramDeclarations;
begin
  Result := TDeclarations.Create;
end;

function TDeclarations.IndexOf(const AReference: TNyxDiagramRef): Integer;
var
  LIndex: Integer;
begin
  NyxDiagramRef(AReference.Name);
  for LIndex := 0 to High(FReferences) do
  begin

    if FReferences[LIndex].Name = AReference.Name then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

procedure TDeclarations.RequireWritable;
begin

  if FBusy then
  begin
    raise ENyxDiagram.Create('Diagram declarations are reserved during snapshot capture');
  end;
end;

function FamilyData(const AReferences: array of TNyxDiagramRef;
  const ADefinitions: array of INyxDiagram): TNyxDataValue;
var
  LEntries: array of TNyxDataValue;
  LIndex: Integer;
  LVertices: Integer;
  LEdges: Integer;
begin

  if (Length(AReferences) <> Length(ADefinitions)) or
    (Length(AReferences) > MaximumNyxDiagramDefinitions) then
  begin
    raise ENyxDiagram.Create('Diagram family membership exceeds its budget');
  end;
  LVertices := 0;
  LEdges := 0;
  SetLength(LEntries, Length(AReferences));
  for LIndex := 0 to High(AReferences) do
  begin
    NyxDiagramRef(AReferences[LIndex].Name);

    if ADefinitions[LIndex] = nil then
    begin
      raise ENyxDiagram.Create('Diagram definition is absent');
    end;
    Inc(LVertices, ADefinitions[LIndex].VertexCount);
    Inc(LEdges, ADefinitions[LIndex].EdgeCount);

    if (LVertices > MaximumNyxDiagramFamilyVertices) or
      (LEdges > MaximumNyxDiagramFamilyEdges) then
    begin
      raise ENyxDiagram.Create('Diagram family geometry exceeds its budget');
    end;
    LEntries[LIndex] := NyxObject([
      NyxField('name', NyxData(AReferences[LIndex].Name)),
      NyxField('diagram', ADefinitions[LIndex].ToData)]);
  end;
  Result := NyxArray(LEntries);

  if NyxUTF8ByteCount(Result.ToJSON) > MaximumNyxDiagramFamilyBytes then
  begin
    raise ENyxDiagram.Create('Diagram family exceeds its persistence byte budget');
  end;
end;

function TDeclarations.Define(const AReference: TNyxDiagramRef;
  const ADiagram: INyxDiagram): INyxDiagramDeclarations;
var
  LReference: TNyxDiagramRef;
  LInput: INyxDiagram;
  LLease: INyxDiagramDeclarations;
  LDiagram: INyxDiagram;
  LReferences: array of TNyxDiagramRef;
  LDefinitions: array of INyxDiagram;
  LIndex: Integer;
begin
  RequireWritable;
  LReference := NyxDiagramRef(AReference.Name);

  if ADiagram = nil then
  begin
    raise ENyxDiagram.Create('Diagram definition is required');
  end;
  LIndex := IndexOf(LReference);

  if (LIndex < 0) and (Length(FReferences) >= MaximumNyxDiagramDefinitions) then
  begin
    raise ENyxDiagram.Create('Diagram family definition capacity exceeded');
  end;
  FBusy := True;
  try
    { Foreign capture may release the caller's variables. Independent leases
      keep both owners alive while the reservation protects membership. }
    LLease := Self as INyxDiagramDeclarations;
    LInput := ADiagram;
    LDiagram := NyxDiagramFromData(LInput.ToData);
    LReferences := Copy(FReferences, 0, Length(FReferences));
    LDefinitions := Copy(FDefinitions, 0, Length(FDefinitions));

    if LIndex < 0 then
    begin
      LIndex := Length(LReferences);
      SetLength(LReferences, LIndex + 1);
      SetLength(LDefinitions, LIndex + 1);
    end;
    LReferences[LIndex] := LReference;
    LDefinitions[LIndex] := LDiagram;
    FamilyData(LReferences, LDefinitions);
    FReferences := LReferences;
    FDefinitions := LDefinitions;
  finally
    FBusy := False;
  end;
  Result := Self as INyxDiagramDeclarations;
end;

function TDeclarations.Remove(const AReference: TNyxDiagramRef): INyxDiagramDeclarations;
var
  LIndex: Integer;
  LRemove: Integer;
  LReferences: array of TNyxDiagramRef;
  LDefinitions: array of INyxDiagram;
begin
  RequireWritable;
  LRemove := IndexOf(AReference);

  if LRemove < 0 then
  begin
    raise ENyxDiagram.Create('Diagram declaration does not exist');
  end;
  LReferences := Copy(FReferences, 0, Length(FReferences));
  LDefinitions := Copy(FDefinitions, 0, Length(FDefinitions));
  for LIndex := LRemove to High(LReferences) - 1 do
  begin
    LReferences[LIndex] := LReferences[LIndex + 1];
    LDefinitions[LIndex] := LDefinitions[LIndex + 1];
  end;
  SetLength(LReferences, Length(LReferences) - 1);
  SetLength(LDefinitions, Length(LDefinitions) - 1);
  FReferences := LReferences;
  FDefinitions := LDefinitions;
  Result := Self as INyxDiagramDeclarations;
end;

function TDeclarations.Contains(const AReference: TNyxDiagramRef): Boolean;
begin
  Result := IndexOf(AReference) >= 0;
end;

function TDeclarations.Definition(const AReference: TNyxDiagramRef): INyxDiagram;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference);

  if LIndex < 0 then
  begin
    raise ENyxDiagram.Create('Diagram declaration does not exist');
  end;
  Result := FDefinitions[LIndex];
end;

function TDeclarations.Reference(AIndex: Integer): TNyxDiagramRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FReferences)) then
  begin
    raise ENyxDiagram.Create('Diagram declaration index is outside its range');
  end;
  Result := FReferences[AIndex];
end;

function TDeclarations.GetCount: Integer;
begin
  Result := Length(FReferences);
end;

function TDeclarations.Clone: INyxDiagramDeclarations;
var
  LCopy: TDeclarations;
begin
  LCopy := TDeclarations.Create;
  Result := LCopy;
  LCopy.FReferences := Copy(FReferences, 0, Length(FReferences));
  LCopy.FDefinitions := Copy(FDefinitions, 0, Length(FDefinitions));
end;

procedure TDeclarations.Validate;
begin
  FamilyData(FReferences, FDefinitions);
end;

function TDeclarations.ToData: TNyxDataValue;
begin
  Result := FamilyData(FReferences, FDefinitions);
end;

function NyxDiagramDeclarationsFromData(const AData: TNyxDataValue): INyxDiagramDeclarations;
var
  LEntry: TNyxDataValue;
  LReference: TNyxDiagramRef;
  LIndex: Integer;
begin

  if (AData.Kind <> ndArray) or (AData.Count > MaximumNyxDiagramDefinitions) or
    (NyxUTF8ByteCount(AData.ToJSON) > MaximumNyxDiagramFamilyBytes) then
  begin
    raise ENyxDiagram.Create('Diagram family requires a bounded ordered array');
  end;
  Result := NewNyxDiagramDeclarations;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LEntry := AData.Item(LIndex);

    if (LEntry.Kind <> ndObject) or (LEntry.Count <> 2) then
    begin
      raise ENyxDiagram.Create('Diagram entry requires its exact name and diagram fields');
    end;
    LReference := NyxDiagramRef(LEntry.Field('name').AsText);

    if Result.Contains(LReference) then
    begin
      raise ENyxDiagram.Create('Duplicate diagram declaration');
    end;
    Result.Define(LReference, NyxDiagramFromData(LEntry.Field('diagram')));
  end;
end;

end.
