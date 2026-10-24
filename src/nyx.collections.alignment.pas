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
unit nyx.collections.alignment;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.collections.arrangement;

type
  { Malformed alignment choices refuse before any value is adopted. This
    boundary has no renderer, document, scheduler or platform dependency. }
  ENyxCollectionAlignment = class(EArgumentException);

  { Logical inline alignment follows the cell's current reading direction.
    Center is direction independent. No choice examines or formats its data. }
  TNyxColumnHorizontalAlignment = (nchaStart, nchaCenter, nchaEnd);

  { Position the complete visible text block within the allocated body cell.
    These choices do not select row height, line breaks or editor admission. }
  TNyxColumnVerticalAlignment = (ncvaTop, ncvaMiddle, ncvaBottom);

  { A detached adapter result, independent of LCL/DOM alignment enums. Logical
    direction is resolved explicitly by the current host, never from text. }
  TNyxColumnPhysicalAlignment = (ncpaLeft, ncpaCenter, ncpaRight);

  { Independent optional axes. A default/empty builder is absent and preserves
    the host's exact legacy behavior, including any existing theme alignment.
    Explicit Start or Top remains distinct from that absence. An unmentioned
    axis is inherited; clearing a runtime axis restores its authored value,
    and clearing an authored axis restores the host default. Overlay composes
    these value layers without retaining either owner. No automatic numeric,
    Boolean, locale or text-direction guess is part of this policy.

    Wire is null for absence, otherwise the exact version1 object with both
    nullable axes. An all-null object refuses: null is the sole empty wire.
    This standalone value does not change collection descriptor versions. }
  TNyxColumnAlignmentPolicy = record
  private
    FHasHorizontal: Boolean;
    FHasVertical: Boolean;
    FHorizontal: TNyxColumnHorizontalAlignment;
    FVertical: TNyxColumnVerticalAlignment;
    function GetDefined: Boolean;
    function GetHorizontal: TNyxColumnHorizontalAlignment;
    function GetVertical: TNyxColumnVerticalAlignment;
  public
    function Horizontal(AValue: TNyxColumnHorizontalAlignment): TNyxColumnAlignmentPolicy;
    function Vertical(AValue: TNyxColumnVerticalAlignment): TNyxColumnAlignmentPolicy;
    function UseDefaultHorizontal: TNyxColumnAlignmentPolicy;
    function UseDefaultVertical: TNyxColumnAlignmentPolicy;
    function Overlay(const AOverrides: TNyxColumnAlignmentPolicy): TNyxColumnAlignmentPolicy;
    function Copy: TNyxColumnAlignmentPolicy;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnAlignmentPolicy; static;
    property Defined: Boolean read GetDefined;
    property HasHorizontal: Boolean read FHasHorizontal;
    property HasVertical: Boolean read FHasVertical;
    property HorizontalAlignment: TNyxColumnHorizontalAlignment read GetHorizontal;
    property VerticalAlignment: TNyxColumnVerticalAlignment read GetVertical;
  end;

  { One copied runtime preference names the full scalar field identity. It
    contains no source value, live schema, row coordinate, mount or callback.
    ValidateFor rejects missing or same-name/different-family destinations.
    An absent preference is null; a present one requires at least one axis.
    Removing this preference, rather than saving a host coordinate, resets it. }
  TNyxColumnAlignmentPreference = record
  private
    FDefined: Boolean;
    FField: TNyxColumnIdentity;
    FPolicy: TNyxColumnAlignmentPolicy;
    function GetField: TNyxColumnIdentity;
    function GetPolicy: TNyxColumnAlignmentPolicy;
  public
    function Copy: TNyxColumnAlignmentPreference;
    procedure Validate;
    procedure ValidateFor(const AField: TNyxColumnIdentity);
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnAlignmentPreference; static;
    property Defined: Boolean read FDefined;
    property Field: TNyxColumnIdentity read GetField;
    property Alignment: TNyxColumnAlignmentPolicy read GetPolicy;
  end;

{ Start an absent fluent value; the first axis call makes that axis explicit. }
function NyxColumnAlignment: TNyxColumnAlignmentPolicy;
function NyxColumnAlignmentPreference(const AField: TNyxColumnIdentity;
  const APolicy: TNyxColumnAlignmentPolicy): TNyxColumnAlignmentPreference;

{ Closed names support persistence/source boundaries; unknown names refuse. }
function NyxColumnHorizontalAlignmentName(AValue: TNyxColumnHorizontalAlignment): TNyxText;
function ReadNyxColumnHorizontalAlignment(const AName: TNyxText): TNyxColumnHorizontalAlignment;
function NyxColumnVerticalAlignmentName(AValue: TNyxColumnVerticalAlignment): TNyxText;
function ReadNyxColumnVerticalAlignment(const AName: TNyxText): TNyxColumnVerticalAlignment;

{ Resolve only an explicit logical choice. An absent policy must bypass this
  helper so an adapter never replaces inherited host alignment by a guess. }
function ResolveNyxColumnHorizontalAlignment(AValue: TNyxColumnHorizontalAlignment;
  ARightToLeft: Boolean): TNyxColumnPhysicalAlignment;

implementation

const
  CHorizontalNames: array[TNyxColumnHorizontalAlignment] of TNyxText =
    ('start', 'center', 'end');
  CVerticalNames: array[TNyxColumnVerticalAlignment] of TNyxText =
    ('top', 'middle', 'bottom');

procedure RequireHorizontal(AValue: TNyxColumnHorizontalAlignment);
begin

  if (Ord(AValue) < Ord(Low(TNyxColumnHorizontalAlignment))) or
    (Ord(AValue) > Ord(High(TNyxColumnHorizontalAlignment))) then
  begin
    raise ENyxCollectionAlignment.Create('Unknown horizontal column alignment');
  end;
end;

procedure RequireVertical(AValue: TNyxColumnVerticalAlignment);
begin

  if (Ord(AValue) < Ord(Low(TNyxColumnVerticalAlignment))) or
    (Ord(AValue) > Ord(High(TNyxColumnVerticalAlignment))) then
  begin
    raise ENyxCollectionAlignment.Create('Unknown vertical column alignment');
  end;
end;

procedure RequireShape(const AData: TNyxDataValue; const AKeys: array of TNyxText);
var
  LIndex: Integer;
  LKeyIndex: Integer;
  LFound: Boolean;
begin
  AData.Validate;

  if (AData.Kind <> ndObject) or (AData.Count <> Length(AKeys)) then
  begin
    raise ENyxCollectionAlignment.Create('Alignment requires its complete version1 object');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFound := False;
    for LKeyIndex := 0 to High(AKeys) do
    begin

      if AData.Key(LIndex) = AKeys[LKeyIndex] then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxCollectionAlignment.Create('Unknown alignment member');
    end;
  end;

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxCollectionAlignment.Create('Unsupported alignment version');
  end;
end;

function NyxColumnHorizontalAlignmentName(AValue: TNyxColumnHorizontalAlignment): TNyxText;
begin
  RequireHorizontal(AValue);
  Result := CHorizontalNames[AValue];
end;

function ReadNyxColumnHorizontalAlignment(const AName: TNyxText): TNyxColumnHorizontalAlignment;
var
  LValue: TNyxColumnHorizontalAlignment;
begin
  for LValue := Low(TNyxColumnHorizontalAlignment) to High(TNyxColumnHorizontalAlignment) do
  begin

    if AName = CHorizontalNames[LValue] then
    begin
      Exit(LValue);
    end;
  end;
  raise ENyxCollectionAlignment.Create('Unknown horizontal column alignment: ' + AName);
end;

function NyxColumnVerticalAlignmentName(AValue: TNyxColumnVerticalAlignment): TNyxText;
begin
  RequireVertical(AValue);
  Result := CVerticalNames[AValue];
end;

function ReadNyxColumnVerticalAlignment(const AName: TNyxText): TNyxColumnVerticalAlignment;
var
  LValue: TNyxColumnVerticalAlignment;
begin
  for LValue := Low(TNyxColumnVerticalAlignment) to High(TNyxColumnVerticalAlignment) do
  begin

    if AName = CVerticalNames[LValue] then
    begin
      Exit(LValue);
    end;
  end;
  raise ENyxCollectionAlignment.Create('Unknown vertical column alignment: ' + AName);
end;

function ResolveNyxColumnHorizontalAlignment(AValue: TNyxColumnHorizontalAlignment;
  ARightToLeft: Boolean): TNyxColumnPhysicalAlignment;
begin
  RequireHorizontal(AValue);

  if AValue = nchaCenter then
  begin
    Exit(ncpaCenter);
  end;

  if (AValue = nchaStart) <> ARightToLeft then
  begin
    Result := ncpaLeft;
  end
  else
  begin
    Result := ncpaRight;
  end;
end;

function NyxColumnAlignment: TNyxColumnAlignmentPolicy;
begin
  Result := Default(TNyxColumnAlignmentPolicy);
end;

function TNyxColumnAlignmentPolicy.GetDefined: Boolean;
begin
  Result := FHasHorizontal or FHasVertical;
end;

function TNyxColumnAlignmentPolicy.GetHorizontal: TNyxColumnHorizontalAlignment;
begin
  Validate;

  if not FHasHorizontal then
  begin
    raise ENyxCollectionAlignment.Create('Horizontal column alignment is inherited');
  end;
  Result := FHorizontal;
end;

function TNyxColumnAlignmentPolicy.GetVertical: TNyxColumnVerticalAlignment;
begin
  Validate;

  if not FHasVertical then
  begin
    raise ENyxCollectionAlignment.Create('Vertical column alignment is inherited');
  end;
  Result := FVertical;
end;

procedure TNyxColumnAlignmentPolicy.Validate;
begin

  if FHasHorizontal then
  begin
    RequireHorizontal(FHorizontal);
  end;

  if FHasVertical then
  begin
    RequireVertical(FVertical);
  end;
end;

function TNyxColumnAlignmentPolicy.Horizontal(
  AValue: TNyxColumnHorizontalAlignment): TNyxColumnAlignmentPolicy;
var
  LPolicy: TNyxColumnAlignmentPolicy;
begin
  LPolicy := Self;
  LPolicy.Validate;
  RequireHorizontal(AValue);
  LPolicy.FHasHorizontal := True;
  LPolicy.FHorizontal := AValue;
  Result := LPolicy;
end;

function TNyxColumnAlignmentPolicy.Vertical(
  AValue: TNyxColumnVerticalAlignment): TNyxColumnAlignmentPolicy;
var
  LPolicy: TNyxColumnAlignmentPolicy;
begin
  LPolicy := Self;
  LPolicy.Validate;
  RequireVertical(AValue);
  LPolicy.FHasVertical := True;
  LPolicy.FVertical := AValue;
  Result := LPolicy;
end;

function TNyxColumnAlignmentPolicy.UseDefaultHorizontal: TNyxColumnAlignmentPolicy;
var
  LPolicy: TNyxColumnAlignmentPolicy;
begin
  LPolicy := Self;
  LPolicy.Validate;
  LPolicy.FHasHorizontal := False;
  LPolicy.FHorizontal := Low(TNyxColumnHorizontalAlignment);
  Result := LPolicy;
end;

function TNyxColumnAlignmentPolicy.UseDefaultVertical: TNyxColumnAlignmentPolicy;
var
  LPolicy: TNyxColumnAlignmentPolicy;
begin
  LPolicy := Self;
  LPolicy.Validate;
  LPolicy.FHasVertical := False;
  LPolicy.FVertical := Low(TNyxColumnVerticalAlignment);
  Result := LPolicy;
end;

function TNyxColumnAlignmentPolicy.Overlay(
  const AOverrides: TNyxColumnAlignmentPolicy): TNyxColumnAlignmentPolicy;
var
  LPolicy: TNyxColumnAlignmentPolicy;
  LOverrides: TNyxColumnAlignmentPolicy;
begin
  LPolicy := Self;
  LOverrides := AOverrides;
  LPolicy.Validate;
  LOverrides.Validate;

  if LOverrides.FHasHorizontal then
  begin
    LPolicy.FHasHorizontal := True;
    LPolicy.FHorizontal := LOverrides.FHorizontal;
  end;

  if LOverrides.FHasVertical then
  begin
    LPolicy.FHasVertical := True;
    LPolicy.FVertical := LOverrides.FVertical;
  end;
  Result := LPolicy;
end;

function TNyxColumnAlignmentPolicy.Copy: TNyxColumnAlignmentPolicy;
var
  LPolicy: TNyxColumnAlignmentPolicy;
begin
  LPolicy := Self;
  LPolicy.Validate;
  Result := LPolicy;
end;

function TNyxColumnAlignmentPolicy.ToData: TNyxDataValue;
var
  LHorizontal: TNyxDataValue;
  LVertical: TNyxDataValue;
begin
  Validate;

  if not Defined then
  begin
    Exit(NyxNull);
  end;
  LHorizontal := NyxNull;
  LVertical := NyxNull;

  if FHasHorizontal then
  begin
    LHorizontal := NyxData(NyxColumnHorizontalAlignmentName(FHorizontal));
  end;

  if FHasVertical then
  begin
    LVertical := NyxData(NyxColumnVerticalAlignmentName(FVertical));
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('horizontal', LHorizontal), NyxField('vertical', LVertical)]);
end;

class function TNyxColumnAlignmentPolicy.FromData(
  const AData: TNyxDataValue): TNyxColumnAlignmentPolicy;
var
  LPolicy: TNyxColumnAlignmentPolicy;
begin
  AData.Validate;
  LPolicy := NyxColumnAlignment;

  if AData.Kind = ndNull then
  begin
    Exit(LPolicy);
  end;
  RequireShape(AData, ['version', 'horizontal', 'vertical']);

  if AData.Field('horizontal').Kind <> ndNull then
  begin
    LPolicy := LPolicy.Horizontal(
      ReadNyxColumnHorizontalAlignment(AData.Field('horizontal').AsText));
  end;

  if AData.Field('vertical').Kind <> ndNull then
  begin
    LPolicy := LPolicy.Vertical(
      ReadNyxColumnVerticalAlignment(AData.Field('vertical').AsText));
  end;

  if not LPolicy.Defined then
  begin
    raise ENyxCollectionAlignment.Create('Empty alignment must use null');
  end;
  Result := LPolicy;
end;

function NyxColumnAlignmentPreference(const AField: TNyxColumnIdentity;
  const APolicy: TNyxColumnAlignmentPolicy): TNyxColumnAlignmentPreference;
var
  LPreference: TNyxColumnAlignmentPreference;
begin
  LPreference := Default(TNyxColumnAlignmentPreference);
  LPreference.FDefined := True;
  LPreference.FField := AField;
  LPreference.FPolicy := APolicy;
  LPreference.Validate;
  Result := LPreference;
end;

procedure TNyxColumnAlignmentPreference.Validate;
begin

  if not FDefined then
  begin
    Exit;
  end;
  FField.Validate;
  FPolicy.Validate;

  if not FPolicy.Defined then
  begin
    raise ENyxCollectionAlignment.Create('An alignment preference requires an explicit axis');
  end;
end;

procedure TNyxColumnAlignmentPreference.ValidateFor(const AField: TNyxColumnIdentity);
begin
  Validate;
  AField.Validate;

  if not FDefined or not FField.Same(AField) then
  begin
    raise ENyxCollectionAlignment.Create('Alignment preference field or scalar family changed');
  end;
end;

function TNyxColumnAlignmentPreference.GetField: TNyxColumnIdentity;
begin
  Validate;

  if not FDefined then
  begin
    raise ENyxCollectionAlignment.Create('Alignment preference is absent');
  end;
  Result := FField;
end;

function TNyxColumnAlignmentPreference.GetPolicy: TNyxColumnAlignmentPolicy;
begin
  Validate;

  if not FDefined then
  begin
    raise ENyxCollectionAlignment.Create('Alignment preference is absent');
  end;
  Result := FPolicy;
end;

function TNyxColumnAlignmentPreference.Copy: TNyxColumnAlignmentPreference;
var
  LPreference: TNyxColumnAlignmentPreference;
begin
  LPreference := Self;
  LPreference.Validate;
  Result := LPreference;
end;

function TNyxColumnAlignmentPreference.ToData: TNyxDataValue;
begin
  Validate;

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('field', FField.ToData), NyxField('alignment', FPolicy.ToData)]);
end;

class function TNyxColumnAlignmentPreference.FromData(
  const AData: TNyxDataValue): TNyxColumnAlignmentPreference;
var
  LPreference: TNyxColumnAlignmentPreference;
begin
  AData.Validate;
  LPreference := Default(TNyxColumnAlignmentPreference);

  if AData.Kind = ndNull then
  begin
    Exit(LPreference);
  end;
  RequireShape(AData, ['version', 'field', 'alignment']);
  LPreference := NyxColumnAlignmentPreference(
    TNyxColumnIdentity.FromData(AData.Field('field')),
    TNyxColumnAlignmentPolicy.FromData(AData.Field('alignment')));
  Result := LPreference;
end;

end.
