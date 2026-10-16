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
unit nyx.diagram.editing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.model, nyx.diagram, nyx.diagram.declarations,
  nyx.canvas.geometry;

type
  { Closed visual authoring actions, independent of executable flow operations.
    Each action belongs to a caller-owned candidate, not a live view. }
  TNyxDiagramChangeKind = (ndcgDefine, ndcgRemove, ndcgSetVertex, ndcgSetBounds,
    ndcgRemoveVertex, ndcgSetEdge, ndcgRemoveEdge);

  { Any uses the receiving transaction's revision fence. Absent protects a new
    name; Exact additionally protects a retained editor's original definition. }
  TNyxDiagramExpectation = (ndeAny, ndeAbsent, ndeExact);

  { Inert copied graph-edit intent with no interfaces in this portable record.
    Define/Expect normalize foreign graphs once. Apply may change only an owned
    candidate; the ordinary grouped source/history owner admits the whole result.
    Exact expectations compare complete canonical graph bytes before the action. }
  TNyxDiagramChange = record
  private
    FDefined: Boolean;
    FKind: TNyxDiagramChangeKind;
    FReference: TNyxDiagramRef;
    FMember: TNyxText;
    FPayload: TNyxDataValue;
    FRemoval: TNyxDiagramRemoval;
    FExpectation: TNyxDiagramExpectation;
    FExpected: TNyxDataValue;
  public
    function ExpectAbsent: TNyxDiagramChange;
    function Expect(const ADiagram: INyxDiagram): TNyxDiagramChange;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDiagramChange; static;
    procedure Apply(ADocument: TNyxDocument);
    property Defined: Boolean read FDefined;
    property Kind: TNyxDiagramChangeKind read FKind;
    property Reference: TNyxDiagramRef read FReference;
    property Expectation: TNyxDiagramExpectation read FExpectation;
  end;

  { Chronological changes are admitted together by the existing project patch.
    Copies retain only immutable descriptors and exact open identities. }
  TNyxDiagramChanges = array of TNyxDiagramChange;

function NyxDefineDiagram(const AReference: TNyxDiagramRef;
  const ADiagram: INyxDiagram): TNyxDiagramChange;
function NyxRemoveDiagram(const AReference: TNyxDiagramRef): TNyxDiagramChange;
function NyxSetDiagramVertex(const AReference: TNyxDiagramRef;
  const AVertex: TNyxDiagramVertex): TNyxDiagramChange;
function NyxSetDiagramBounds(const AReference: TNyxDiagramRef;
  const AVertex: TNyxDiagramVertexRef; const ABounds: TNyxCanvasBounds): TNyxDiagramChange;
function NyxRemoveDiagramVertex(const AReference: TNyxDiagramRef;
  const AVertex: TNyxDiagramVertexRef;
  ARemoval: TNyxDiagramRemoval = ndrmRequireDisconnected): TNyxDiagramChange;
function NyxSetDiagramEdge(const AReference: TNyxDiagramRef;
  const AEdge: TNyxDiagramEdge): TNyxDiagramChange;
function NyxRemoveDiagramEdge(const AReference: TNyxDiagramRef;
  const AEdge: TNyxDiagramEdgeRef): TNyxDiagramChange;

implementation

uses SysUtils;

{ Exact optional-field lookup at the external change boundary. }
function HasNyxDataField(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

function Capture(const ADiagram: INyxDiagram): TNyxDataValue;
var
  LInput: INyxDiagram;
begin

  if ADiagram = nil then
  begin
    raise ENyxDiagram.Create('Diagram authoring requires an immutable graph');
  end;
  LInput := ADiagram;
  Result := NyxDiagramFromData(LInput.ToData).ToData;
end;

function NyxRemoveDiagram(const AReference: TNyxDiagramRef): TNyxDiagramChange;
begin
  Result := Default(TNyxDiagramChange);
  Result.FDefined := True;
  Result.FReference := NyxDiagramRef(AReference.Name);
  Result.FKind := ndcgRemove;
end;

function NyxDefineDiagram(const AReference: TNyxDiagramRef;
  const ADiagram: INyxDiagram): TNyxDiagramChange;
begin
  Result := NyxRemoveDiagram(AReference);
  Result.FKind := ndcgDefine;
  Result.FPayload := Capture(ADiagram);
end;

function NyxSetDiagramVertex(const AReference: TNyxDiagramRef;
  const AVertex: TNyxDiagramVertex): TNyxDiagramChange;
begin
  Result := NyxRemoveDiagram(AReference);
  Result.FKind := ndcgSetVertex;
  Result.FPayload := AVertex.ToData;
end;

function BoundsData(const ABounds: TNyxCanvasBounds): TNyxDataValue;
begin
  Result := NyxArray([NyxData(ABounds.Left), NyxData(ABounds.Top),
    NyxData(ABounds.Width), NyxData(ABounds.Height)]);
end;

function ReadBounds(const AData: TNyxDataValue): TNyxCanvasBounds;
begin

  if (AData.Kind <> ndArray) or (AData.Count <> 4) then
  begin
    raise ENyxDiagram.Create('Diagram bounds require four numeric values');
  end;
  Result := NyxCanvasBounds(AData.Item(0).AsNumber, AData.Item(1).AsNumber,
    AData.Item(2).AsNumber, AData.Item(3).AsNumber);
end;

function NyxSetDiagramBounds(const AReference: TNyxDiagramRef;
  const AVertex: TNyxDiagramVertexRef; const ABounds: TNyxCanvasBounds): TNyxDiagramChange;
var
  LValue: TNyxDiagramVertex;
begin
  Result := NyxRemoveDiagram(AReference);
  LValue := NyxDiagramVertex(AVertex, ndsRectangle, ABounds);
  Result.FKind := ndcgSetBounds;
  Result.FMember := LValue.Reference.Name;
  Result.FPayload := BoundsData(LValue.Bounds);
end;

function NyxRemoveDiagramVertex(const AReference: TNyxDiagramRef;
  const AVertex: TNyxDiagramVertexRef; ARemoval: TNyxDiagramRemoval): TNyxDiagramChange;
begin

  if not (ARemoval in [ndrmRequireDisconnected, ndrmIncidentEdges]) then
  begin
    raise ENyxDiagram.Create('Unknown diagram vertex removal policy');
  end;
  Result := NyxRemoveDiagram(AReference);
  Result.FKind := ndcgRemoveVertex;
  Result.FMember := NyxDiagramVertexRef(AVertex.Name).Name;
  Result.FRemoval := ARemoval;
end;

function NyxSetDiagramEdge(const AReference: TNyxDiagramRef;
  const AEdge: TNyxDiagramEdge): TNyxDiagramChange;
begin
  Result := NyxRemoveDiagram(AReference);
  Result.FKind := ndcgSetEdge;
  Result.FPayload := AEdge.ToData;
end;

function NyxRemoveDiagramEdge(const AReference: TNyxDiagramRef;
  const AEdge: TNyxDiagramEdgeRef): TNyxDiagramChange;
begin
  Result := NyxRemoveDiagram(AReference);
  Result.FKind := ndcgRemoveEdge;
  Result.FMember := NyxDiagramEdgeRef(AEdge.Name).Name;
end;

function TNyxDiagramChange.ExpectAbsent: TNyxDiagramChange;
begin

  if not FDefined then
  begin
    raise ENyxDiagram.Create('An absent change cannot carry an expectation');
  end;
  Result := Self;
  Result.FExpectation := ndeAbsent;
  Result.FExpected := NyxNull;
end;

function TNyxDiagramChange.Expect(const ADiagram: INyxDiagram): TNyxDiagramChange;
begin

  if not FDefined then
  begin
    raise ENyxDiagram.Create('An absent change cannot carry an expectation');
  end;
  Result := Self;
  Result.FExpected := Capture(ADiagram);
  Result.FExpectation := ndeExact;
end;

function TNyxDiagramChange.ToData: TNyxDataValue;
const
  CNames: array[TNyxDiagramChangeKind] of TNyxText = ('diagram-define', 'diagram-remove',
    'diagram-vertex-set', 'diagram-vertex-bounds', 'diagram-vertex-remove',
    'diagram-edge-set', 'diagram-edge-remove');
var
  LFields: array of TNyxDataField;
  LCount: Integer;

  procedure Add(const AName: TNyxText; const AValue: TNyxDataValue);
  begin
    SetLength(LFields, LCount + 1);
    LFields[LCount] := NyxField(AName, AValue);
    Inc(LCount);
  end;

begin

  if not FDefined then
  begin
    raise ENyxDiagram.Create('An uninitialized diagram change refuses');
  end;
  LCount := 0;
  LFields := nil;
  Add('op', NyxData(CNames[FKind]));
  Add('name', NyxData(FReference.Name));
  case FKind of
    ndcgRemove:
      begin
        { Registry removal has no member payload. }
      end;
    ndcgDefine: Add('definition', FPayload);
    ndcgSetVertex: Add('vertex', FPayload);
    ndcgSetEdge: Add('edge', FPayload);
    ndcgSetBounds:
      begin
        Add('vertex', NyxData(FMember));
        Add('bounds', FPayload);
      end;
    ndcgRemoveVertex:
      begin
        Add('vertex', NyxData(FMember));
        Add('incidentEdges', NyxData(FRemoval = ndrmIncidentEdges));
      end;
    ndcgRemoveEdge: Add('edge', NyxData(FMember));
  end;

  if FExpectation = ndeAbsent then
  begin
    Add('expected', NyxNull);
  end
  else if FExpectation = ndeExact then
  begin
    Add('expected', FExpected);
  end;
  Result := NyxObject(LFields);
end;

class function TNyxDiagramChange.FromData(const AData: TNyxDataValue): TNyxDiagramChange;
var
  LName: TNyxText;
  LReference: TNyxDiagramRef;
  LRemoval: TNyxDiagramRemoval;

  procedure Fields(const ANames: array of TNyxText);
  var
    LIndex: Integer;
    LRequired: Integer;
  begin
    LRequired := Length(ANames);

    if HasNyxDataField(AData, 'expected') then
    begin
      Inc(LRequired);
    end;

    if (AData.Kind <> ndObject) or (AData.Count <> LRequired) then
    begin
      raise ENyxDiagram.Create('Diagram change requires its exact closed fields');
    end;
    for LIndex := 0 to High(ANames) do
    begin
      AData.Field(ANames[LIndex]);
    end;
  end;

begin
  LName := AData.Field('op').AsText;
  LReference := NyxDiagramRef(AData.Field('name').AsText);

  if LName = 'diagram-define' then
  begin
    Fields(['op', 'name', 'definition']);
    Result := NyxDefineDiagram(LReference, NyxDiagramFromData(AData.Field('definition')));
  end
  else if LName = 'diagram-remove' then
  begin
    Fields(['op', 'name']);
    Result := NyxRemoveDiagram(LReference);
  end
  else if LName = 'diagram-vertex-set' then
  begin
    Fields(['op', 'name', 'vertex']);
    Result := NyxSetDiagramVertex(LReference, TNyxDiagramVertex.FromData(AData.Field('vertex')));
  end
  else if LName = 'diagram-vertex-bounds' then
  begin
    Fields(['op', 'name', 'vertex', 'bounds']);
    Result := NyxSetDiagramBounds(LReference, NyxDiagramVertexRef(AData.Field('vertex').AsText),
      ReadBounds(AData.Field('bounds')));
  end
  else if LName = 'diagram-vertex-remove' then
  begin
    Fields(['op', 'name', 'vertex', 'incidentEdges']);
    LRemoval := ndrmRequireDisconnected;

    if AData.Field('incidentEdges').AsBoolean then
    begin
      LRemoval := ndrmIncidentEdges;
    end;
    Result := NyxRemoveDiagramVertex(LReference, NyxDiagramVertexRef(AData.Field('vertex').AsText), LRemoval);
  end
  else if LName = 'diagram-edge-set' then
  begin
    Fields(['op', 'name', 'edge']);
    Result := NyxSetDiagramEdge(LReference, TNyxDiagramEdge.FromData(AData.Field('edge')));
  end
  else if LName = 'diagram-edge-remove' then
  begin
    Fields(['op', 'name', 'edge']);
    Result := NyxRemoveDiagramEdge(LReference, NyxDiagramEdgeRef(AData.Field('edge').AsText));
  end
  else
  begin
    raise ENyxDiagram.Create('Unknown typed diagram change');
  end;

  if HasNyxDataField(AData, 'expected') then
  begin

    if AData.Field('expected').Kind = ndNull then
    begin
      Result := Result.ExpectAbsent;
    end
    else
    begin
      Result := Result.Expect(NyxDiagramFromData(AData.Field('expected')));
    end;
  end;
end;

procedure TNyxDiagramChange.Apply(ADocument: TNyxDocument);
var
  LPlan: INyxDiagram;
begin

  if not FDefined or (ADocument = nil) then
  begin
    raise ENyxDiagram.Create('Diagram changes require an owned candidate');
  end;

  if (FExpectation = ndeAbsent) and ADocument.Diagrams.Contains(FReference) then
  begin
    raise ENyxDiagram.Create('This diagram name is already defined');
  end;

  if FExpectation = ndeExact then
  begin

    if not ADocument.Diagrams.Contains(FReference) or
      (ADocument.Diagrams.Definition(FReference).ToData.ToJSON <> FExpected.ToJSON) then
    begin
      raise ENyxDiagram.Create('The accepted diagram changed; retain and review the local proposal');
    end;
  end;
  case FKind of
    ndcgDefine: ADocument.Diagrams.Define(FReference, NyxDiagramFromData(FPayload));
    ndcgRemove: ADocument.Diagrams.Remove(FReference);
  else
    LPlan := ADocument.Diagrams.Definition(FReference);
    case FKind of
      ndcgSetVertex: LPlan := LPlan.WithVertex(TNyxDiagramVertex.FromData(FPayload));
      ndcgSetBounds: LPlan := LPlan.WithVertex(LPlan.FindVertex(
        NyxDiagramVertexRef(FMember)).AtBounds(ReadBounds(FPayload)));
      ndcgRemoveVertex: LPlan := LPlan.WithoutVertex(NyxDiagramVertexRef(FMember), FRemoval);
      ndcgSetEdge: LPlan := LPlan.WithEdge(TNyxDiagramEdge.FromData(FPayload));
      ndcgRemoveEdge: LPlan := LPlan.WithoutEdge(NyxDiagramEdgeRef(FMember));
    else
      raise ENyxDiagram.Create('Unknown diagram member change');
    end;
    ADocument.Diagrams.Define(FReference, LPlan);
  end;
end;

end.
