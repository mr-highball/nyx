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
unit nyx.test.drawing.capture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.data, nyx.canvas.geometry, nyx.drawing;

type
  { Deliberately mutable creator facade owns a replaceable plan, never a view or
    provider. Reader counters distinguish one complete wire capture from live
    getter traversal. FailWire supplies a malformed proposal while preserving its
    valid live getters. Lifetime counters are fixture-thread-only; callers borrow
    the class pointer only while retaining its interface. }
  TNyxTestDrawingPlan = class(TInterfacedObject, INyxDrawingPlan)
  private
    FBase: INyxDrawingPlan;
    FWireReads: Integer;
    FGetterReads: Integer;
    FFailWire: Boolean;
    FForwardObject: Boolean;
    FObjectQueries: Integer;
  public
    {$ifndef PAS2JS}
    { FPC's interface-to-class operator asks IObjectInstance. Deliberate
      forwarding models aggregation; it must not grant trust to this facade. }
    function QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      AIID: TGUID; out AObject): LongInt;
      {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; reintroduce;
    {$endif}
    constructor Create(const ABase: INyxDrawingPlan);
    destructor Destroy; override;
    function GetViewBox: TNyxCanvasBounds;
    function GetCount: Integer;
    function Command(AIndex: Integer): TNyxDataValue;
    function ToData: TNyxDataValue;
    property Base: INyxDrawingPlan read FBase write FBase;
    property FailWire: Boolean read FFailWire write FFailWire;
    property WireReads: Integer read FWireReads;
    property GetterReads: Integer read FGetterReads;
    property ForwardObject: Boolean read FForwardObject write FForwardObject;
    property ObjectQueries: Integer read FObjectQueries;
  end;

{ Current facade lifetimes, observed on the fixture's single owning thread.
  Captured plans must remain usable after the last creator facade has retired. }
function LiveNyxTestDrawingPlans: Integer;

implementation

var
  GLive: Integer;

{$ifndef PAS2JS}
function TNyxTestDrawingPlan.QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
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

constructor TNyxTestDrawingPlan.Create(const ABase: INyxDrawingPlan);
begin
  inherited Create;
  Inc(GLive);
  FBase := ABase;

  if FBase = nil then
  begin
    raise ENyxDrawing.Create('A creator facade requires an owned base plan');
  end;
end;

destructor TNyxTestDrawingPlan.Destroy;
begin
  Dec(GLive);
  inherited Destroy;
end;

function TNyxTestDrawingPlan.GetViewBox: TNyxCanvasBounds;
begin
  Inc(FGetterReads);
  Result := FBase.ViewBox;
end;

function TNyxTestDrawingPlan.GetCount: Integer;
begin
  Inc(FGetterReads);
  Result := FBase.Count;
end;

function TNyxTestDrawingPlan.Command(AIndex: Integer): TNyxDataValue;
begin
  Inc(FGetterReads);
  Result := FBase.Command(AIndex);
end;

function TNyxTestDrawingPlan.ToData: TNyxDataValue;
begin
  Inc(FWireReads);

  if FFailWire then
  begin
    Result := NyxNull;
  end
  else
  begin
    Result := FBase.ToData;
  end;
end;

function LiveNyxTestDrawingPlans: Integer;
begin
  Result := GLive;
end;

end.
