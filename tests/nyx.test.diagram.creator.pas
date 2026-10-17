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
unit nyx.test.diagram.creator;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.data, nyx.diagram, nyx.canvas.geometry;

type
  { Deliberately mutable foreign implementation. Live operations forward to an
    owned immutable base while ToData returns an independently replaceable value.
    Counters distinguish strict wire capture from trusting foreign getters.
    Native object-query forwarding models aggregation without granting private
    storage identity. The fixture owns no document, view or application; callers
    borrow this class pointer only while retaining its interface, on one thread. }
  TNyxTestDiagramPlan = class(TInterfacedObject, INyxDiagram)
  private
    FBase: INyxDiagram;
    FWire: TNyxDataValue;
    FWireReads: Integer;
    FGetterReads: Integer;
    FForwardObject: Boolean;
    FObjectQueries: Integer;
  public
    {$ifndef PAS2JS}
    { IObjectInstance belongs to FPC's interface-to-class bridge. Forwarding its
      query borrows the retained base's identity only for that query. }
    function QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      AIID: TGUID; out AObject): LongInt;
      {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; reintroduce;
    {$endif}
    { Retain the base and copy its initial wire value; a nil base refuses. }
    constructor Create(const ABase: INyxDiagram);
    { Replace only the foreign proposal, never its valid live base. }
    procedure SetWire(const AValue: TNyxDataValue);
    function GetVertexCount: Integer;
    function GetEdgeCount: Integer;
    function Vertex(AIndex: Integer): TNyxDiagramVertex;
    function Edge(AIndex: Integer): TNyxDiagramEdge;
    function FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
    function FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
    function WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
    function WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
    function WithoutVertex(const AReference: TNyxDiagramVertexRef;
      APolicy: TNyxDiagramRemoval): INyxDiagram;
    function WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
    function Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
    function PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
    function PickEdge(const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramEdgeRef;
    function ToData: TNyxDataValue;
    property WireReads: Integer read FWireReads;
    property GetterReads: Integer read FGetterReads;
    property ForwardObject: Boolean read FForwardObject write FForwardObject;
    property ObjectQueries: Integer read FObjectQueries;
  end;

implementation

{$ifndef PAS2JS}
function TNyxTestDiagramPlan.QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  AIID: TGUID; out AObject): LongInt;
  {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
begin

  if FForwardObject and IsEqualGUID(AIID, IObjectInstance) then
  begin
    Inc(FObjectQueries);
    Result := FBase.QueryInterface(AIID, AObject);
  end
  else
  begin
    Result := inherited QueryInterface(AIID, AObject);
  end;
end;
{$endif}

constructor TNyxTestDiagramPlan.Create(const ABase: INyxDiagram);
begin
  inherited Create;

  if ABase = nil then
  begin
    raise ENyxDiagram.Create('Creator fixture requires its complete base');
  end;
  FBase := ABase;
  SetWire(FBase.ToData);
end;

procedure TNyxTestDiagramPlan.SetWire(const AValue: TNyxDataValue);
begin
  FWire := AValue.Copy;
end;

function TNyxTestDiagramPlan.GetVertexCount: Integer;
begin
  Inc(FGetterReads);
  Result := FBase.VertexCount;
end;

function TNyxTestDiagramPlan.GetEdgeCount: Integer;
begin
  Inc(FGetterReads);
  Result := FBase.EdgeCount;
end;

function TNyxTestDiagramPlan.Vertex(AIndex: Integer): TNyxDiagramVertex;
begin
  Inc(FGetterReads);
  Result := FBase.Vertex(AIndex);
end;

function TNyxTestDiagramPlan.Edge(AIndex: Integer): TNyxDiagramEdge;
begin
  Inc(FGetterReads);
  Result := FBase.Edge(AIndex);
end;

function TNyxTestDiagramPlan.FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
begin
  Inc(FGetterReads);
  Result := FBase.FindVertex(AReference);
end;

function TNyxTestDiagramPlan.FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
begin
  Inc(FGetterReads);
  Result := FBase.FindEdge(AReference);
end;

function TNyxTestDiagramPlan.WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
begin
  Inc(FGetterReads);
  Result := FBase.WithVertex(AVertex);
end;

function TNyxTestDiagramPlan.WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
begin
  Inc(FGetterReads);
  Result := FBase.WithEdge(AEdge);
end;

function TNyxTestDiagramPlan.WithoutVertex(const AReference: TNyxDiagramVertexRef;
  APolicy: TNyxDiagramRemoval): INyxDiagram;
begin
  Inc(FGetterReads);
  Result := FBase.WithoutVertex(AReference, APolicy);
end;

function TNyxTestDiagramPlan.WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
begin
  Inc(FGetterReads);
  Result := FBase.WithoutEdge(AReference);
end;

function TNyxTestDiagramPlan.Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
begin
  Inc(FGetterReads);
  Result := FBase.Route(AReference);
end;

function TNyxTestDiagramPlan.PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
begin
  Inc(FGetterReads);
  Result := FBase.PickVertex(APoint);
end;

function TNyxTestDiagramPlan.PickEdge(const APoint: TNyxCanvasPoint;
  ATolerance: Double): TNyxDiagramEdgeRef;
begin
  Inc(FGetterReads);
  Result := FBase.PickEdge(APoint, ATolerance);
end;

function TNyxTestDiagramPlan.ToData: TNyxDataValue;
begin
  Inc(FWireReads);
  Result := FWire.Copy;
end;

end.
