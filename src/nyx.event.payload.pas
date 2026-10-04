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
unit nyx.event.payload;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.data,
  nyx.state,
  nyx.contract;

type
  { An immutable declaration for a named producer's payload. Scalar domains use
    the same exact admission as controls/state. Structured details have a closed
    data kind and retain exact nested Unicode/decimal data. Signal is explicit:
    an absent payload is different from an explicitly supplied JSON null.
    Default records mean no published specification, never an admitted value. }
  TNyxEventPayloadKind = (nepSignal, nepScalar, nepData);
  TNyxEventPayloadSpec = record
  private
    FMarker: TNyxText;
    FKind: TNyxEventPayloadKind;
    FDomain: TNyxValueDomain;
    FDataKind: TNyxDataKind;
    function GetDefined: Boolean;
    function GetKind: TNyxEventPayloadKind;
    function GetDomain: TNyxValueDomain;
  public
    class function FromData(const AData: TNyxDataValue): TNyxEventPayloadSpec; static;
    function ToData: TNyxDataValue;
    function Copy: TNyxEventPayloadSpec;
    { Short target-independent inspector help. Shape/domain details remain
      queryable through ToData at the explicit semantic metadata boundary. }
    function Description: TNyxText;
    { Admission is pure. Wrong shape/family, an absent required payload or an
      extra signal payload fails before any callback or document mutation. }
    procedure Admit(const AData: TNyxDataValue; AHasPayload: Boolean);
    property Defined: Boolean read GetDefined;
    property Kind: TNyxEventPayloadKind read GetKind;
    property Domain: TNyxValueDomain read GetDomain;
  end;

function NyxSignalPayload: TNyxEventPayloadSpec;
function NyxScalarPayload(const ADomain: TNyxValueDomain): TNyxEventPayloadSpec; overload;
function NyxScalarPayload(const ADomain: TNyxTextDomain): TNyxEventPayloadSpec; overload;
function NyxScalarPayload(const ADomain: TNyxBooleanDomain): TNyxEventPayloadSpec; overload;
function NyxScalarPayload(const ADomain: TNyxIntegerDomain): TNyxEventPayloadSpec; overload;
function NyxScalarPayload(const ADomain: TNyxNumberDomain): TNyxEventPayloadSpec; overload;
function NyxDataPayload(AKind: TNyxDataKind): TNyxEventPayloadSpec;

implementation

uses
  SysUtils;

const
  CPayloadMarker = 'nyx.event.payload/1';
  CPayloadNames: array[TNyxEventPayloadKind] of TNyxText = ('signal', 'scalar', 'data');
  CDataNames: array[TNyxDataKind] of TNyxText =
    ('null', 'text', 'boolean', 'number', 'object', 'array');

function TNyxEventPayloadSpec.GetDefined: Boolean;
begin
  Result := FMarker = CPayloadMarker;
end;

function TNyxEventPayloadSpec.GetKind: TNyxEventPayloadKind;
begin

  if not Defined then
  begin
    raise ENyxContract.Create('Named event payload specification is undefined');
  end;
  Result := FKind;
end;

function TNyxEventPayloadSpec.GetDomain: TNyxValueDomain;
begin

  if Kind <> nepScalar then
  begin
    raise ENyxContract.Create('Only scalar named events have a scalar domain');
  end;
  Result := FDomain.Copy;
end;

function NyxSignalPayload: TNyxEventPayloadSpec;
begin
  Result := Default(TNyxEventPayloadSpec);
  Result.FMarker := CPayloadMarker;
  Result.FKind := nepSignal;
  Result.FDomain := NyxNoDomain;
  Result.FDataKind := ndNull;
end;

function NyxScalarPayload(const ADomain: TNyxValueDomain): TNyxEventPayloadSpec;
begin
  ADomain.Validate;

  if not ADomain.Defined then
  begin
    raise ENyxContract.Create('A scalar named event requires its domain');
  end;
  Result := NyxSignalPayload;
  Result.FKind := nepScalar;
  Result.FDomain := ADomain.Copy;
end;

function NyxScalarPayload(const ADomain: TNyxTextDomain): TNyxEventPayloadSpec;
begin
  Result := NyxScalarPayload(ADomain.Definition);
end;

function NyxScalarPayload(const ADomain: TNyxBooleanDomain): TNyxEventPayloadSpec;
begin
  Result := NyxScalarPayload(ADomain.Definition);
end;

function NyxScalarPayload(const ADomain: TNyxIntegerDomain): TNyxEventPayloadSpec;
begin
  Result := NyxScalarPayload(ADomain.Definition);
end;

function NyxScalarPayload(const ADomain: TNyxNumberDomain): TNyxEventPayloadSpec;
begin
  Result := NyxScalarPayload(ADomain.Definition);
end;

function NyxDataPayload(AKind: TNyxDataKind): TNyxEventPayloadSpec;
begin
  Result := NyxSignalPayload;
  Result.FKind := nepData;
  Result.FDataKind := AKind;
end;

procedure TNyxEventPayloadSpec.Admit(const AData: TNyxDataValue; AHasPayload: Boolean);
begin
  case Kind of
    nepSignal:
      begin

        if AHasPayload then
        begin
          raise ENyxContract.Create('A signal named event refuses a payload');
        end;
      end;
    nepScalar:
      begin

        if not AHasPayload then
        begin
          raise ENyxContract.Create('A scalar named event requires its payload');
        end;
        FDomain.Admit(AData);
      end;
    nepData:
      begin

        if not AHasPayload or (AData.Kind <> FDataKind) then
        begin
          raise ENyxContract.Create('Named event structured payload has the wrong data kind');
        end;
        { Copy re-admits immutable JSON. No caller-owned container crosses the
          producer boundary, even when an implementation originated in JS. }
        AData.Copy;
      end;
  end;
end;

function TNyxEventPayloadSpec.ToData: TNyxDataValue;
begin
  case Kind of
    nepSignal:
      begin
        Result := NyxObject([NyxField('kind', NyxData(CPayloadNames[FKind]))]);
      end;
    nepScalar:
      begin
        Result := NyxObject([
          NyxField('kind', NyxData(CPayloadNames[FKind])),
          NyxField('domain', FDomain.ToData)
        ]);
      end;
    nepData:
      begin
        Result := NyxObject([
          NyxField('kind', NyxData(CPayloadNames[FKind])),
          NyxField('dataKind', NyxData(CDataNames[FDataKind]))
        ]);
      end;
  end;
end;

function TNyxEventPayloadSpec.Description: TNyxText;
begin
  case Kind of
    nepSignal: Result := 'Signal; no payload';
    nepScalar: Result := 'Scalar ' + NyxStateKindName(FDomain.Kind) +
      '; admitted against the declared range and choices';
    nepData: Result := 'Owned ' + CDataNames[FDataKind] + ' details';
  end;
end;

class function TNyxEventPayloadSpec.FromData(const AData: TNyxDataValue): TNyxEventPayloadSpec;
var
  LDataKind: TNyxDataKind;
  LFound: Boolean;
begin

  if AData.Kind <> ndObject then
  begin
    raise ENyxContract.Create('Named event payload descriptor requires an object');
  end;

  if (AData.Field('kind').AsText = CPayloadNames[nepSignal]) and (AData.Count = 1) then
  begin
    Exit(NyxSignalPayload);
  end;

  if (AData.Field('kind').AsText = CPayloadNames[nepScalar]) and (AData.Count = 2) then
  begin
    Exit(NyxScalarPayload(TNyxValueDomain.FromData(AData.Field('domain'))));
  end;

  if (AData.Field('kind').AsText = CPayloadNames[nepData]) and (AData.Count = 2) then
  begin
    LFound := False;
    for LDataKind := Low(TNyxDataKind) to High(TNyxDataKind) do
    begin

      if AData.Field('dataKind').AsText = CDataNames[LDataKind] then
      begin
        Result := NyxDataPayload(LDataKind);
        LFound := True;
        Break;
      end;
    end;

    if LFound then
    begin
      Exit;
    end;
  end;
  raise ENyxContract.Create('Unknown named event payload descriptor');
end;

function TNyxEventPayloadSpec.Copy: TNyxEventPayloadSpec;
begin
  Result := FromData(ToData);
end;

end.
