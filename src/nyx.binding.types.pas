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

unit nyx.binding.types;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.state,
  nyx.resources;

type
  { Only Value is writable from a control. Other targets are projections from
    typed state. Layout integers and Boolean behavior never use string choices. }
  TNyxBindingProperty = (bpText,
    bpValue,
    bpEnabled,
    bpVisible,
    bpReadOnly,
    bpPressed,
    bpPlaceholder,
    bpHint,
    bpAccessibleName,
    bpWidth,
    bpHeight,
    bpLeft,
    bpTop,
    bpPadding,
    bpGap,
    bpColumns,
    bpFlex,
    bpMinimum,
    bpMaximum);
  TNyxBindingDirection = (bdFromState, bdTwoWay);
  TNyxBindingSource = (bsState, bsResource);
  { A notification failure follows an admitted commit. It must never be reported
    as rejected state or retried by restoring the old value. }
  TNyxBindingFailure = (nbfNone, nbfRejected, nbfNotificationFailed);

  { Value snapshots for authoring tools. A closed scalar-kind set describes
    admitted references; it never borrows nodes, stores or renderer handles. }
  TNyxStateKinds = set of TNyxStateKind;
  TNyxBindingTargetInfo = record
    Target: TNyxBindingProperty;
    Title: TNyxText;
    ValueKinds: TNyxStateKinds;
  end;
  TNyxBindingTargetInfos = array of TNyxBindingTargetInfo;

  { Immutable authored descriptor. The node owns copies, with no store/control
    references. Explicit copied fields keep pas2js records independent too.
    Cleared descriptors preserve an instance's deliberate unbinding operation;
    composition removes the inherited binding without altering its definition. }
  TNyxBindingSpec = record
  private
    FProperty: TNyxBindingProperty;
    FStateName: TNyxText;
    FValueKind: TNyxStateKind;
    FDirection: TNyxBindingDirection;
    FCleared: Boolean;
    FSource: TNyxBindingSource;
    FResource: TNyxResourceValueRef;
  public
    class function Bound(AProperty: TNyxBindingProperty; const AStateName: TNyxText;
      AValueKind: TNyxStateKind; ADirection: TNyxBindingDirection): TNyxBindingSpec; static;
    class function Clear(AProperty: TNyxBindingProperty): TNyxBindingSpec; static;
    { Read-only resource projection; editing never rewrites packed file data. }
    class function Resource(AProperty: TNyxBindingProperty;
      const AValue: TNyxResourceValueRef): TNyxBindingSpec; static;
    function Copy: TNyxBindingSpec;
    { Exact copied contract equality, including explicit clearing and direction.
      Open state names retain their precise Unicode spelling. This comparison
      owns no store and cannot establish that a live value has been admitted. }
    function Same(const AOther: TNyxBindingSpec): Boolean;
    procedure Validate;
    property Target: TNyxBindingProperty read FProperty;
    property StateName: TNyxText read FStateName;
    property ValueKind: TNyxStateKind read FValueKind;
    property Direction: TNyxBindingDirection read FDirection;
    property Cleared: Boolean read FCleared;
    property Source: TNyxBindingSource read FSource;
    property ResourceValue: TNyxResourceValueRef read FResource;
  end;

{ Enum/string mappings are persistence/schema boundaries. Authored Pascal and
  generated source use enums, typed references and node-owned fluent objects. }
function NyxBindingPropertyName(AProperty: TNyxBindingProperty): TNyxText;
function NyxBindingPropertyTitle(AProperty: TNyxBindingProperty): TNyxText;
function TryNyxBindingProperty(const AName: TNyxText;
  out AProperty: TNyxBindingProperty): Boolean;
function NyxBindingDirectionName(ADirection: TNyxBindingDirection): TNyxText;
function TryNyxBindingDirection(const AName: TNyxText;
  out ADirection: TNyxBindingDirection): Boolean;

implementation

const
  CPropertyNames: array[TNyxBindingProperty] of TNyxText = (
    'text', 'value', 'enabled', 'visible', 'readonly', 'pressed', 'placeholder',
    'hint', 'aria-label', 'width', 'height', 'left', 'top', 'padding', 'gap',
    'columns', 'flex', 'min', 'max');
  CDirectionNames: array[TNyxBindingDirection] of TNyxText = ('from-state', 'two-way');
  CPropertyTitles: array[TNyxBindingProperty] of TNyxText = (
    'Text', 'Value', 'Enabled', 'Visible', 'Read only', 'Pressed', 'Placeholder',
    'Hint', 'Accessible name', 'Width', 'Height', 'Left', 'Top', 'Padding', 'Gap',
    'Columns', 'Flex', 'Minimum', 'Maximum');

class function TNyxBindingSpec.Bound(AProperty: TNyxBindingProperty;
  const AStateName: TNyxText; AValueKind: TNyxStateKind;
  ADirection: TNyxBindingDirection): TNyxBindingSpec;
begin
  Result := Default(TNyxBindingSpec);
  Result.FProperty := AProperty;
  Result.FStateName := AStateName;
  Result.FValueKind := AValueKind;
  Result.FDirection := ADirection;
  Result.FCleared := False;
  Result.Validate;
end;

class function TNyxBindingSpec.Clear(AProperty: TNyxBindingProperty): TNyxBindingSpec;
begin
  Result := Default(TNyxBindingSpec);
  Result.FProperty := AProperty;
  Result.FStateName := '';
  Result.FValueKind := nskText;
  Result.FDirection := bdFromState;
  Result.FCleared := True;
  Result.Validate;
end;

function TNyxBindingSpec.Copy: TNyxBindingSpec;
begin
  Result.FProperty := FProperty;
  Result.FStateName := FStateName;
  Result.FValueKind := FValueKind;
  Result.FDirection := FDirection;
  Result.FCleared := FCleared;
  Result.FSource := FSource;
  Result.FResource := FResource.Copy;
end;

class function TNyxBindingSpec.Resource(AProperty: TNyxBindingProperty;
  const AValue: TNyxResourceValueRef): TNyxBindingSpec;
var
  LCandidate: TNyxBindingSpec;
begin
  LCandidate := Default(TNyxBindingSpec);
  LCandidate.FProperty := AProperty;
  LCandidate.FSource := bsResource;
  LCandidate.FResource := AValue.Copy;
  LCandidate.FValueKind := AValue.Kind;
  LCandidate.Validate;
  Result := LCandidate;
end;

function TNyxBindingSpec.Same(const AOther: TNyxBindingSpec): Boolean;
begin
  Result := (FProperty = AOther.FProperty) and
    (FStateName = AOther.FStateName) and (FValueKind = AOther.FValueKind) and
    (FDirection = AOther.FDirection) and (FCleared = AOther.FCleared) and
    (FSource = AOther.FSource);

  if Result and not FCleared and (FSource = bsResource) then
  begin
    Result := FResource.ToData.ToJSON = AOther.FResource.ToData.ToJSON;
  end;
end;

procedure TNyxBindingSpec.Validate;
var
  LReference: TNyxTextStateRef;
begin

  if (Ord(FProperty) < Ord(Low(TNyxBindingProperty))) or
    (Ord(FProperty) > Ord(High(TNyxBindingProperty))) or
    (Ord(FDirection) < Ord(Low(TNyxBindingDirection))) or
    (Ord(FDirection) > Ord(High(TNyxBindingDirection))) or
    (Ord(FValueKind) < Ord(Low(TNyxStateKind))) or
    (Ord(FValueKind) > Ord(High(TNyxStateKind))) or
    (Ord(FSource) < Ord(Low(TNyxBindingSource))) or
    (Ord(FSource) > Ord(High(TNyxBindingSource))) then
  begin
    raise ENyxState.Create('Binding contains an unknown enum choice');
  end;

  if FCleared then
  begin
    Exit;
  end;
  { Key admission is identical for every typed reference. This does not coerce
    its stored kind; the document/runtime store checks exact kind membership. }

  if FSource = bsResource then
  begin
    FResource.ToData;

    if FDirection <> bdFromState then
    begin
      raise ENyxState.Create('Resource bindings are read-only projections');
    end;
  end
  else
  begin
    LReference := NyxTextState(FStateName);

    if LReference.Name <> FStateName then
    begin
      raise ENyxState.Create('Binding key admission changed its name');
    end;
  end;

  if (FDirection = bdTwoWay) and (FProperty <> bpValue) then
  begin
    raise ENyxState.Create('Only control Value supports two-way binding');
  end;
  case FProperty of
    bpEnabled, bpVisible, bpReadOnly, bpPressed:
      begin

        if FValueKind <> nskBoolean then
        begin
          raise ENyxState.Create('Boolean binding target requires Boolean state');
        end;
      end;
    bpPlaceholder, bpHint, bpAccessibleName:
      begin

        if FValueKind <> nskText then
        begin
          raise ENyxState.Create('Text binding target requires text state');
        end;
      end;
    bpWidth, bpHeight, bpLeft, bpTop, bpPadding, bpGap, bpColumns, bpFlex,
      bpMinimum, bpMaximum:
      begin

        if FValueKind <> nskInteger then
        begin
          raise ENyxState.Create('Layout/range binding target requires integer state');
        end;
      end;
    bpText, bpValue:
      begin
        { Captions explicitly project any scalar; Value target-kind admission
          also checks the concrete input/compound's semantic property schema. }
      end;
  end;
end;

function NyxBindingPropertyName(AProperty: TNyxBindingProperty): TNyxText;
begin
  Result := CPropertyNames[AProperty];
end;

function NyxBindingPropertyTitle(AProperty: TNyxBindingProperty): TNyxText;
begin
  Result := CPropertyTitles[AProperty];
end;

function TryNyxBindingProperty(const AName: TNyxText;
  out AProperty: TNyxBindingProperty): Boolean;
var
  LProperty: TNyxBindingProperty;
begin
  AProperty := bpText;
  for LProperty := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
  begin

    if CPropertyNames[LProperty] = AName then
    begin
      AProperty := LProperty;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxBindingDirectionName(ADirection: TNyxBindingDirection): TNyxText;
begin
  Result := CDirectionNames[ADirection];
end;

function TryNyxBindingDirection(const AName: TNyxText;
  out ADirection: TNyxBindingDirection): Boolean;
var
  LDirection: TNyxBindingDirection;
begin
  ADirection := bdFromState;
  for LDirection := Low(TNyxBindingDirection) to High(TNyxBindingDirection) do
  begin

    if CDirectionNames[LDirection] = AName then
    begin
      ADirection := LDirection;
      Exit(True);
    end;
  end;
  Result := False;
end;

end.
