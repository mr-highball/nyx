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

unit nyx.date.range;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.dates, nyx.contract, nyx.model, nyx.controls;

const
  { Optional whole-key policy. Absence preserves the independent Date parts;
    an explicit Independent value masks an inherited ordered configuration. }
  NyxDateRangeOptionsKey = 'nyx.date-range';

type
  { Ordering failures participate in existing precommit scalar/state admission. }
  ENyxDateRange = class(ENyxContract);

  { Closed relationship semantics; ascending includes equal calendar dates. }
  TNyxDateRangeOrder = (ndroIndependent, ndroAscending);

  { Copied authored policy, with no nodes, stores, callbacks or target handles.
    Empty endpoints remain subject to their own domains. Ordering never writes
    a peer, swaps endpoints or substitutes a current date. }
  TNyxDateRangeOptions = record
  private
    FOrder: TNyxDateRangeOrder;
  public
    class function Independent: TNyxDateRangeOptions; static;
    class function Ordered: TNyxDateRangeOptions; static;
    function Copy: TNyxDateRangeOptions;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxDateRangeOptions; static;
    property Order: TNyxDateRangeOrder read FOrder;
  end;

function HasNyxDateRangeOptions(ANode: TNyxNode): Boolean;
{ Absence reads as Independent without creating local metadata. }
function GetNyxDateRangeOptions(ANode: TNyxNode): TNyxDateRangeOptions;
{ These helpers only author a copied policy. Complete admission resolves a
  reusable reference or named-part descriptor before checking its target. }
procedure SetNyxDateRangeOptions(const ARange: INyxControl;
  const AOptions: TNyxDateRangeOptions);
procedure InheritNyxDateRangeOptions(const ARange: INyxControl);
{ Parse known metadata now. Raw references/descriptors defer target admission;
  concrete targets must retain the specialized DateRange kind. No values are
  compared here: raw defaults may be replaced by bindings or overrides. }
procedure ValidateNyxDateRangeOptions(ANode: TNyxNode);
{ Call only on the complete projected candidate. Ordered ranges require their
  own direct, realized start/finish Date roles. NoDate is never compared. Existing
  scalar/domain validation owns each endpoint's bounds and choice restrictions. }
procedure ValidateNyxDateRangeValues(ANode: TNyxNode);

implementation

uses
  nyx.types;

const
  CIndependent = 'independent';
  CAscending = 'ascending';
  CStart = 'start';
  CFinish = 'finish';

class function TNyxDateRangeOptions.Independent: TNyxDateRangeOptions;
begin
  Result.FOrder := ndroIndependent;
end;

class function TNyxDateRangeOptions.Ordered: TNyxDateRangeOptions;
begin
  Result.FOrder := ndroAscending;
end;

function TNyxDateRangeOptions.Copy: TNyxDateRangeOptions;
begin
  Validate;
  Result.FOrder := FOrder;
end;

procedure TNyxDateRangeOptions.Validate;
begin
  case Ord(FOrder) of
    Ord(ndroIndependent), Ord(ndroAscending):
      begin
        Exit;
      end;
  else
    begin
      raise ENyxDateRange.Create('Unknown date-range ordering');
    end;
  end;
end;

function TNyxDateRangeOptions.ToData: TNyxDataValue;
var
  LOrder: TNyxText;
begin
  Validate;
  LOrder := CIndependent;

  if FOrder = ndroAscending then
  begin
    LOrder := CAscending;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('order', NyxData(LOrder))]);
end;

class function TNyxDateRangeOptions.FromData(
  const AValue: TNyxDataValue): TNyxDateRangeOptions;
var
  LOrder: TNyxText;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> 2) or
    (AValue.Field('version').AsInteger <> 1) then
  begin
    raise ENyxDateRange.Create('Date-range options require an exact version-one object');
  end;
  LOrder := AValue.Field('order').AsText;

  if LOrder = CIndependent then
  begin
    Exit(Independent);
  end;

  if LOrder = CAscending then
  begin
    Exit(Ordered);
  end;
  raise ENyxDateRange.Create('Unknown date-range ordering');
end;

function HasNyxDateRangeOptions(ANode: TNyxNode): Boolean;
begin
  Result := (ANode <> nil) and
    ANode.Extensions.Has(NyxExtension(NyxDateRangeOptionsKey));
end;

function GetNyxDateRangeOptions(ANode: TNyxNode): TNyxDateRangeOptions;
begin
  Result := TNyxDateRangeOptions.Independent;

  if HasNyxDateRangeOptions(ANode) then
  begin
    Result := TNyxDateRangeOptions.FromData(
      ANode.Extensions.Value(NyxExtension(NyxDateRangeOptionsKey)));
  end;
end;

procedure SetNyxDateRangeOptions(const ARange: INyxControl;
  const AOptions: TNyxDateRangeOptions);
var
  LRange: INyxControl;
  LOptions: TNyxDateRangeOptions;
  LValue: TNyxDataValue;
begin
  LRange := ARange;
  LOptions := AOptions.Copy;
  LValue := LOptions.ToData;

  if LRange = nil then
  begin
    raise ENyxDateRange.Create('Date-range options require their authored control');
  end;
  LRange.Extensions.SetValue(NyxExtension(NyxDateRangeOptionsKey), LValue);
end;

procedure InheritNyxDateRangeOptions(const ARange: INyxControl);
var
  LRange: INyxControl;
begin
  LRange := ARange;

  if LRange = nil then
  begin
    raise ENyxDateRange.Create('Date-range inheritance requires its authored control');
  end;
  LRange.Extensions.Remove(NyxExtension(NyxDateRangeOptionsKey));
end;

procedure ValidateNyxDateRangeOptions(ANode: TNyxNode);
var
  LOptions: TNyxDateRangeOptions;
begin

  if not HasNyxDateRangeOptions(ANode) then
  begin
    Exit;
  end;
  LOptions := GetNyxDateRangeOptions(ANode);
  LOptions.Validate;

  if not ANode.IsRealized and
    ((ANode.Kind = NyxKindName(nkComponent)) or
      (ANode.Kind = NyxKindName(nkSlotOverride))) then
  begin
    Exit;
  end;

  if ANode.Kind <> NyxKindName(nkDateRange) then
  begin
    raise ENyxDateRange.Create('Date-range options require a specialized date range');
  end;
end;

function Endpoint(ARange: TNyxNode; const APart: TNyxText): TNyxCalendarDate;
var
  LRole: TNyxNode;
  LIndex: Integer;
begin
  LRole := nil;
  for LIndex := 0 to ARange.Count - 1 do
  begin

    if ARange.Children[LIndex].StoredProp(NyxAttributeName(atPart)) = APart then
    begin

      if LRole <> nil then
      begin
        raise ENyxDateRange.Create('Date-range endpoint is ambiguous: ' + APart);
      end;
      LRole := ARange.Children[LIndex];
    end;
  end;

  if (LRole = nil) or not LRole.IsRealized or
    (LRole.Kind <> NyxKindName(nkDate)) or
    (LRole.ProjectionKind <> NyxKindName(nkDate)) then
  begin
    raise ENyxDateRange.Create('Date-range endpoint must remain a direct Date: ' + APart);
  end;

  if not TryNyxDate(LRole.Prop(NyxAttributeName(atValue)), Result) then
  begin
    raise ENyxDateRange.Create('Date-range endpoint is not a calendar date: ' + APart);
  end;
end;

procedure ValidateNyxDateRangeValues(ANode: TNyxNode);
var
  LOptions: TNyxDateRangeOptions;
  LStart: TNyxCalendarDate;
  LFinish: TNyxCalendarDate;
begin
  ValidateNyxDateRangeOptions(ANode);

  if not HasNyxDateRangeOptions(ANode) then
  begin
    Exit;
  end;
  LOptions := GetNyxDateRangeOptions(ANode);

  if LOptions.Order = ndroIndependent then
  begin
    Exit;
  end;

  if not ANode.IsRealized or (ANode.Kind <> NyxKindName(nkDateRange)) then
  begin
    raise ENyxDateRange.Create('Ordered values require a realized date-range candidate');
  end;
  LStart := Endpoint(ANode, CStart);
  LFinish := Endpoint(ANode, CFinish);

  if LStart.Defined and LFinish.Defined and (LStart.Compare(LFinish) > 0) then
  begin
    raise ENyxDateRange.Create('Start date must not be later than finish date');
  end;
end;

end.
