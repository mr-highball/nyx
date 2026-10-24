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

unit nyx.collections.arrangement;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.collections,
  nyx.layout.constraints;

const
  NyxMaximumColumnLayoutFields = 64;
  NyxMaximumColumnAutoSizeRows = 1024;
  NyxDefaultColumnAutoSizeRows = 64;
  NyxDefaultColumnScrollableWidth = 96;

type
  { Logical edges follow the host's reading direction. These values describe
    presentation intent; they never move fields or values in a collection. }
  TNyxColumnPin = (ncpNone, ncpStart, ncpEnd);

  { Closed user-command capabilities; disabling a command never erases the
    corresponding authored width, order, visibility or logical pin choice. }
  TNyxColumnLayoutFeature = (nclfResize, nclfAutoSize, nclfReorder,
    nclfVisibility, nclfPinning);

  { A copied capability mask shared by table and column policies. The effective
    command requires permission from both masks; an empty set allows none. }
  TNyxColumnLayoutFeatures = set of TNyxColumnLayoutFeature;

  { An immutable scalar-field identity. Exact name and family travel together,
    so a schema replacement cannot reinterpret a text preference as a number.
    Default is absent; checked readers refuse it. No schema/store is retained. }
  TNyxColumnIdentity = record
  private
    FDefined: Boolean;
    FName: TNyxText;
    FKind: TNyxStateKind;
    function GetName: TNyxText;
    function GetKind: TNyxStateKind;
  public
    procedure Validate;
    function Same(const AOther: TNyxColumnIdentity): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnIdentity; static;
    property Defined: Boolean read FDefined;
    property Name: TNyxText read GetName;
    property Kind: TNyxStateKind read GetKind;
  end;

  { Authored per-column defaults and command capabilities. Copy preserves
    absence; configuring an absent policy refuses. A disabled table can still
    display these static visibility/pin choices. }
  TNyxColumnLayoutPolicy = record
  private
    FDefined: Boolean;
    FVisible: Boolean;
    FPin: TNyxColumnPin;
    FFeatures: TNyxColumnLayoutFeatures;
  public
    function Visible(AValue: Boolean): TNyxColumnLayoutPolicy;
    function Pinned(AValue: TNyxColumnPin): TNyxColumnLayoutPolicy;
    function PinStart: TNyxColumnLayoutPolicy;
    function PinEnd: TNyxColumnLayoutPolicy;
    function Unpinned: TNyxColumnLayoutPolicy;
    function AllowOnly(AValue: TNyxColumnLayoutFeatures): TNyxColumnLayoutPolicy;
    function Allows(AValue: TNyxColumnLayoutFeature): Boolean;
    function Copy: TNyxColumnLayoutPolicy;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnLayoutPolicy; static;
    property Defined: Boolean read FDefined;
    property IsVisible: Boolean read FVisible;
    property Pin: TNyxColumnPin read FPin;
    property Features: TNyxColumnLayoutFeatures read FFeatures;
  end;

  { Immutable table-level user-command policy. Pixel/sample bounds constrain
    host work, not data/query membership. Enabled controls commands only;
    authored column choices remain meaningful when commands are disabled. }
  TNyxTableLayoutPolicy = record
  private
    FDefined: Boolean;
    FEnabled: Boolean;
    FFeatures: TNyxColumnLayoutFeatures;
    FMinimumCenter: Integer;
    FAutoSizeRows: Integer;
  public
    function Enabled(AValue: Boolean): TNyxTableLayoutPolicy;
    function AllowOnly(AValue: TNyxColumnLayoutFeatures): TNyxTableLayoutPolicy;
    function Allows(AValue: TNyxColumnLayoutFeature): Boolean;
    function MinimumCenter(AValue: Integer): TNyxTableLayoutPolicy;
    function AutoSizeSample(AValue: Integer): TNyxTableLayoutPolicy;
    function Copy: TNyxTableLayoutPolicy;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxTableLayoutPolicy; static;
    property Defined: Boolean read FDefined;
    property IsEnabled: Boolean read FEnabled;
    property Features: TNyxColumnLayoutFeatures read FFeatures;
    property MinimumScrollableWidth: Integer read FMinimumCenter;
    property AutoSizeRows: Integer read FAutoSizeRows;
  end;

  { Sparse runtime overrides, separate from authored policy. Each mask is
    independent: explicit False/none is distinct from inheriting a default.
    Masked readers refuse absence rather than manufacturing effective values. }
  TNyxColumnPreference = record
  private
    FDefined: Boolean;
    FHasWidth: Boolean;
    FHasVisibility: Boolean;
    FHasPin: Boolean;
    FVisible: Boolean;
    FField: TNyxColumnIdentity;
    FWidth: Integer;
    FPin: TNyxColumnPin;
    function GetField: TNyxColumnIdentity;
    function GetIsEmpty: Boolean;
    function GetWidth: Integer;
    function GetVisible: Boolean;
    function GetPin: TNyxColumnPin;
  public
    function Width(AValue: Integer): TNyxColumnPreference;
    function Visible(AValue: Boolean): TNyxColumnPreference;
    function Pinned(AValue: TNyxColumnPin): TNyxColumnPreference;
    function UseDefaultWidth: TNyxColumnPreference;
    function UseDefaultVisibility: TNyxColumnPreference;
    function UseDefaultPin: TNyxColumnPreference;
    function Copy: TNyxColumnPreference;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnPreference; static;
    property Defined: Boolean read FDefined;
    property IsEmpty: Boolean read GetIsEmpty;
    property Field: TNyxColumnIdentity read GetField;
    property HasWidth: Boolean read FHasWidth;
    property WidthValue: Integer read GetWidth;
    property HasVisibility: Boolean read FHasVisibility;
    property IsVisible: Boolean read GetVisible;
    property HasPin: Boolean read FHasPin;
    property Pin: TNyxColumnPin read GetPin;
  end;

  { An owning immutable preference snapshot. Every builder/copy detaches both
    arrays on native and browser targets; no caller array or borrowed runtime
    survives. Order is a typed prefix, resolved against CURRENT authored fields
    by the view. Clearing an override never implicitly clears this prefix. }
  TNyxColumnPreferences = record
  private
    FDefined: Boolean;
    FColumns: array of TNyxColumnPreference;
    FOrder: array of TNyxColumnIdentity;
    function GetCount: Integer;
    function GetOrderCount: Integer;
  public
    function Copy: TNyxColumnPreferences;
    function WithColumn(const AValue: TNyxColumnPreference): TNyxColumnPreferences;
    function WithoutColumn(const AField: TNyxColumnIdentity): TNyxColumnPreferences;
    function Order(const AFields: array of TNyxColumnIdentity): TNyxColumnPreferences;
    function UseDefaultOrder: TNyxColumnPreferences;
    function ColumnAt(AIndex: Integer): TNyxColumnPreference;
    function IndexOf(const AField: TNyxColumnIdentity): Integer;
    function OrderAt(AIndex: Integer): TNyxColumnIdentity;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnPreferences; static;
    property Defined: Boolean read FDefined;
    property Count: Integer read GetCount;
    property OrderCount: Integer read GetOrderCount;
  end;

function NyxColumnIdentity(const AField: TNyxTextFieldRef): TNyxColumnIdentity; overload;
function NyxColumnIdentity(const AField: TNyxBooleanFieldRef): TNyxColumnIdentity; overload;
function NyxColumnIdentity(const AField: TNyxIntegerFieldRef): TNyxColumnIdentity; overload;
function NyxColumnIdentity(const AField: TNyxNumberFieldRef): TNyxColumnIdentity; overload;
function NyxColumnLayout: TNyxColumnLayoutPolicy;
function NyxTableLayout: TNyxTableLayoutPolicy;
function NyxColumnPreference(const AField: TNyxColumnIdentity): TNyxColumnPreference;
function NyxColumnPreferences: TNyxColumnPreferences;

implementation

const
  CPinNames: array[TNyxColumnPin] of TNyxText = ('none', 'start', 'end');
  CFeatureNames: array[TNyxColumnLayoutFeature] of TNyxText =
    ('resize', 'auto-size', 'reorder', 'visibility', 'pinning');
  CAllFeatures: TNyxColumnLayoutFeatures = [nclfResize, nclfAutoSize,
    nclfReorder, nclfVisibility, nclfPinning];

procedure RequirePin(AValue: TNyxColumnPin);
begin

  if (Ord(AValue) < Ord(Low(TNyxColumnPin))) or
    (Ord(AValue) > Ord(High(TNyxColumnPin))) then
  begin
    raise EArgumentException.Create('Unknown column pin');
  end;
end;

procedure RequireFeature(AValue: TNyxColumnLayoutFeature);
begin

  if (Ord(AValue) < Ord(Low(TNyxColumnLayoutFeature))) or
    (Ord(AValue) > Ord(High(TNyxColumnLayoutFeature))) then
  begin
    raise EArgumentException.Create('Unknown column layout feature');
  end;
end;

procedure RequireFeatures(AValue: TNyxColumnLayoutFeatures);
begin

  if AValue - CAllFeatures <> [] then
  begin
    raise EArgumentException.Create('Unknown column layout feature set');
  end;
end;

procedure RequireShape(const AData: TNyxDataValue;
  const AKeys: array of TNyxText);
var
  I, J: Integer;
  LFound: Boolean;
begin
  AData.Validate;

  if (AData.Kind <> ndObject) or (AData.Count <> Length(AKeys)) then
  begin
    raise EArgumentException.Create('Column arrangement requires a complete version-one object');
  end;
  for I := 0 to AData.Count - 1 do
  begin
    LFound := False;
    for J := 0 to High(AKeys) do
    begin

      if AData.Key(I) = AKeys[J] then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise EArgumentException.Create('Unknown column arrangement member');
    end;
  end;

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise EArgumentException.Create('Unsupported column arrangement version');
  end;
end;

function ReadPin(const AData: TNyxDataValue): TNyxColumnPin;
var
  LText: TNyxText;
  LPin: TNyxColumnPin;
begin
  LText := AData.AsText;
  for LPin := Low(TNyxColumnPin) to High(TNyxColumnPin) do
  begin

    if LText = CPinNames[LPin] then
    begin
      Exit(LPin);
    end;
  end;
  raise EArgumentException.Create('Unknown saved column pin');
end;

function FeaturesToData(AFeatures: TNyxColumnLayoutFeatures): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LFeature: TNyxColumnLayoutFeature;
  LCount: Integer;
begin
  SetLength(LItems, Ord(High(TNyxColumnLayoutFeature)) + 1);
  LCount := 0;
  for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
  begin

    if LFeature in AFeatures then
    begin
      LItems[LCount] := NyxData(CFeatureNames[LFeature]);
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  Result := NyxArray(LItems);
end;

function ReadFeatures(const AData: TNyxDataValue): TNyxColumnLayoutFeatures;
var
  I: Integer;
  LFeature: TNyxColumnLayoutFeature;
  LFound: Boolean;
  LText: TNyxText;
begin
  AData.Validate;

  if (AData.Kind <> ndArray) or
    (AData.Count > Ord(High(TNyxColumnLayoutFeature)) + 1) then
  begin
    raise EArgumentException.Create('Column layout features require a bounded list');
  end;
  Result := [];
  for I := 0 to AData.Count - 1 do
  begin
    LText := AData.Item(I).AsText;
    LFound := False;
    for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
    begin

      if LText = CFeatureNames[LFeature] then
      begin

        if LFeature in Result then
        begin
          raise EArgumentException.Create('Duplicate column layout feature');
        end;
        Include(Result, LFeature);
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise EArgumentException.Create('Unknown saved column layout feature');
    end;
  end;
end;

function NyxColumnIdentity(const AField: TNyxTextFieldRef): TNyxColumnIdentity;
begin
  Result := Default(TNyxColumnIdentity);
  Result.FName := AField.Name;
  Result.FKind := nskText;
  Result.FDefined := True;
end;

function NyxColumnIdentity(const AField: TNyxBooleanFieldRef): TNyxColumnIdentity;
begin
  Result := Default(TNyxColumnIdentity);
  Result.FName := AField.Name;
  Result.FKind := nskBoolean;
  Result.FDefined := True;
end;

function NyxColumnIdentity(const AField: TNyxIntegerFieldRef): TNyxColumnIdentity;
begin
  Result := Default(TNyxColumnIdentity);
  Result.FName := AField.Name;
  Result.FKind := nskInteger;
  Result.FDefined := True;
end;

function NyxColumnIdentity(const AField: TNyxNumberFieldRef): TNyxColumnIdentity;
begin
  Result := Default(TNyxColumnIdentity);
  Result.FName := AField.Name;
  Result.FKind := nskNumber;
  Result.FDefined := True;
end;

procedure TNyxColumnIdentity.Validate;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a typed column identity before reading it');
  end;
  { Typed factories share the collection's exact name admission. }

  if (Ord(FKind) < Ord(nskText)) or (Ord(FKind) > Ord(nskNumber)) then
  begin
    raise EArgumentException.Create('Column identities require a scalar field family');
  end;
  case FKind of
    nskText: NyxTextField(FName);
    nskBoolean: NyxBooleanField(FName);
    nskInteger: NyxIntegerField(FName);
    nskNumber: NyxNumberField(FName);
  end;
end;

function TNyxColumnIdentity.GetName: TNyxText;
begin
  Validate;
  Result := FName;
end;

function TNyxColumnIdentity.GetKind: TNyxStateKind;
begin
  Validate;
  Result := FKind;
end;

function TNyxColumnIdentity.Same(const AOther: TNyxColumnIdentity): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (FKind = AOther.FKind) and (FName = AOther.FName);
end;

function TNyxColumnIdentity.ToData: TNyxDataValue;
var
  LKind: TNyxText;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  case FKind of
    nskText: LKind := 'text';
    nskBoolean: LKind := 'boolean';
    nskInteger: LKind := 'integer';
    nskNumber: LKind := 'number';
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('name', NyxData(FName)), NyxField('kind', NyxData(LKind))]);
end;

class function TNyxColumnIdentity.FromData(const AData: TNyxDataValue): TNyxColumnIdentity;
var
  LName, LKind: TNyxText;
begin
  AData.Validate;
  Result := Default(TNyxColumnIdentity);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  RequireShape(AData, ['version', 'name', 'kind']);
  LName := AData.Field('name').AsText;
  LKind := AData.Field('kind').AsText;

  if LKind = 'text' then
  begin
    Result := NyxColumnIdentity(NyxTextField(LName));
  end
  else if LKind = 'boolean' then
  begin
    Result := NyxColumnIdentity(NyxBooleanField(LName));
  end
  else if LKind = 'integer' then
  begin
    Result := NyxColumnIdentity(NyxIntegerField(LName));
  end
  else if LKind = 'number' then
  begin
    Result := NyxColumnIdentity(NyxNumberField(LName));
  end
  else
  begin
    raise EArgumentException.Create('Unknown saved column field family');
  end;
end;

function NyxColumnLayout: TNyxColumnLayoutPolicy;
begin
  Result := Default(TNyxColumnLayoutPolicy);
  Result.FDefined := True;
  Result.FVisible := True;
  Result.FPin := ncpNone;
  Result.FFeatures := CAllFeatures;
end;

procedure TNyxColumnLayoutPolicy.Validate;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a column layout policy before configuring it');
  end;
  RequirePin(FPin);
  RequireFeatures(FFeatures);
end;

function TNyxColumnLayoutPolicy.Visible(AValue: Boolean): TNyxColumnLayoutPolicy;
begin
  Validate;
  Result := Self;
  Result.FVisible := AValue;
end;

function TNyxColumnLayoutPolicy.Pinned(AValue: TNyxColumnPin): TNyxColumnLayoutPolicy;
begin
  Validate;
  RequirePin(AValue);
  Result := Self;
  Result.FPin := AValue;
end;

function TNyxColumnLayoutPolicy.PinStart: TNyxColumnLayoutPolicy;
begin
  Result := Pinned(ncpStart);
end;

function TNyxColumnLayoutPolicy.PinEnd: TNyxColumnLayoutPolicy;
begin
  Result := Pinned(ncpEnd);
end;

function TNyxColumnLayoutPolicy.Unpinned: TNyxColumnLayoutPolicy;
begin
  Result := Pinned(ncpNone);
end;

function TNyxColumnLayoutPolicy.AllowOnly(AValue: TNyxColumnLayoutFeatures): TNyxColumnLayoutPolicy;
begin
  Validate;
  RequireFeatures(AValue);
  Result := Self;
  Result.FFeatures := AValue;
end;

function TNyxColumnLayoutPolicy.Allows(AValue: TNyxColumnLayoutFeature): Boolean;
begin
  Validate;
  RequireFeature(AValue);
  Result := AValue in FFeatures;
end;

function TNyxColumnLayoutPolicy.Copy: TNyxColumnLayoutPolicy;
begin
  Result := Self;
end;

function TNyxColumnLayoutPolicy.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('visible', NyxData(FVisible)), NyxField('pin', NyxData(CPinNames[FPin])),
    NyxField('features', FeaturesToData(FFeatures))]);
end;

class function TNyxColumnLayoutPolicy.FromData(const AData: TNyxDataValue): TNyxColumnLayoutPolicy;
begin
  AData.Validate;
  Result := Default(TNyxColumnLayoutPolicy);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  RequireShape(AData, ['version', 'visible', 'pin', 'features']);
  Result := NyxColumnLayout.Visible(AData.Field('visible').AsBoolean)
    .Pinned(ReadPin(AData.Field('pin'))).AllowOnly(ReadFeatures(AData.Field('features')));
end;

function NyxTableLayout: TNyxTableLayoutPolicy;
begin
  Result := Default(TNyxTableLayoutPolicy);
  Result.FDefined := True;
  Result.FEnabled := True;
  Result.FFeatures := CAllFeatures;
  Result.FMinimumCenter := NyxDefaultColumnScrollableWidth;
  Result.FAutoSizeRows := NyxDefaultColumnAutoSizeRows;
end;

procedure TNyxTableLayoutPolicy.Validate;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a table layout policy before configuring it');
  end;
  RequireFeatures(FFeatures);

  if (FMinimumCenter < 1) or (FMinimumCenter > MaximumNyxLayoutBound) then
  begin
    raise EArgumentException.Create('Minimum scrollable column width is outside layout bounds');
  end;

  if (FAutoSizeRows < 1) or (FAutoSizeRows > NyxMaximumColumnAutoSizeRows) then
  begin
    raise EArgumentException.Create('Column auto-size sample is outside its bounded row range');
  end;
end;

function TNyxTableLayoutPolicy.Enabled(AValue: Boolean): TNyxTableLayoutPolicy;
begin
  Validate;
  Result := Self;
  Result.FEnabled := AValue;
end;

function TNyxTableLayoutPolicy.AllowOnly(AValue: TNyxColumnLayoutFeatures): TNyxTableLayoutPolicy;
begin
  Validate;
  RequireFeatures(AValue);
  Result := Self;
  Result.FFeatures := AValue;
end;

function TNyxTableLayoutPolicy.Allows(AValue: TNyxColumnLayoutFeature): Boolean;
begin
  Validate;
  RequireFeature(AValue);
  Result := FEnabled and (AValue in FFeatures);
end;

function TNyxTableLayoutPolicy.MinimumCenter(AValue: Integer): TNyxTableLayoutPolicy;
begin
  Validate;
  Result := Self;
  Result.FMinimumCenter := AValue;
  Result.Validate;
end;

function TNyxTableLayoutPolicy.AutoSizeSample(AValue: Integer): TNyxTableLayoutPolicy;
begin
  Validate;
  Result := Self;
  Result.FAutoSizeRows := AValue;
  Result.Validate;
end;

function TNyxTableLayoutPolicy.Copy: TNyxTableLayoutPolicy;
begin
  Result := Self;
end;

function TNyxTableLayoutPolicy.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('enabled', NyxData(FEnabled)), NyxField('features', FeaturesToData(FFeatures)),
    NyxField('minimumCenter', NyxData(FMinimumCenter)),
    NyxField('autoSizeRows', NyxData(FAutoSizeRows))]);
end;

class function TNyxTableLayoutPolicy.FromData(const AData: TNyxDataValue): TNyxTableLayoutPolicy;
begin
  AData.Validate;
  Result := Default(TNyxTableLayoutPolicy);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  RequireShape(AData, ['version', 'enabled', 'features', 'minimumCenter', 'autoSizeRows']);
  Result := NyxTableLayout.Enabled(AData.Field('enabled').AsBoolean)
    .AllowOnly(ReadFeatures(AData.Field('features')))
    .MinimumCenter(AData.Field('minimumCenter').AsInteger)
    .AutoSizeSample(AData.Field('autoSizeRows').AsInteger);
end;

function NyxColumnPreference(const AField: TNyxColumnIdentity): TNyxColumnPreference;
begin
  AField.Validate;
  Result := Default(TNyxColumnPreference);
  Result.FDefined := True;
  Result.FField := AField;
end;

procedure TNyxColumnPreference.Validate;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a typed column preference before configuring it');
  end;
  FField.Validate;

  if FHasWidth and ((FWidth < 1) or (FWidth > MaximumNyxLayoutBound)) then
  begin
    raise EArgumentException.Create('Column preference width is outside layout bounds');
  end;

  if FHasPin then
  begin
    RequirePin(FPin);
  end;
end;

function TNyxColumnPreference.GetField: TNyxColumnIdentity;
begin
  Validate;
  Result := FField;
end;

function TNyxColumnPreference.GetIsEmpty: Boolean;
begin
  Validate;
  Result := not (FHasWidth or FHasVisibility or FHasPin);
end;

function TNyxColumnPreference.GetWidth: Integer;
begin
  Validate;

  if not FHasWidth then
  begin
    raise EArgumentException.Create('Column width preference is absent');
  end;
  Result := FWidth;
end;

function TNyxColumnPreference.GetVisible: Boolean;
begin
  Validate;

  if not FHasVisibility then
  begin
    raise EArgumentException.Create('Column visibility preference is absent');
  end;
  Result := FVisible;
end;

function TNyxColumnPreference.GetPin: TNyxColumnPin;
begin
  Validate;

  if not FHasPin then
  begin
    raise EArgumentException.Create('Column pin preference is absent');
  end;
  Result := FPin;
end;

function TNyxColumnPreference.Width(AValue: Integer): TNyxColumnPreference;
begin
  Validate;
  Result := Self;
  Result.FHasWidth := True;
  Result.FWidth := AValue;
  Result.Validate;
end;

function TNyxColumnPreference.Visible(AValue: Boolean): TNyxColumnPreference;
begin
  Validate;
  Result := Self;
  Result.FHasVisibility := True;
  Result.FVisible := AValue;
end;

function TNyxColumnPreference.Pinned(AValue: TNyxColumnPin): TNyxColumnPreference;
begin
  Validate;
  RequirePin(AValue);
  Result := Self;
  Result.FHasPin := True;
  Result.FPin := AValue;
end;

function TNyxColumnPreference.UseDefaultWidth: TNyxColumnPreference;
begin
  Validate;
  Result := Self;
  Result.FHasWidth := False;
  Result.FWidth := 0;
end;

function TNyxColumnPreference.UseDefaultVisibility: TNyxColumnPreference;
begin
  Validate;
  Result := Self;
  Result.FHasVisibility := False;
  Result.FVisible := False;
end;

function TNyxColumnPreference.UseDefaultPin: TNyxColumnPreference;
begin
  Validate;
  Result := Self;
  Result.FHasPin := False;
  Result.FPin := ncpNone;
end;

function TNyxColumnPreference.Copy: TNyxColumnPreference;
begin
  Result := Self;
end;

function TNyxColumnPreference.ToData: TNyxDataValue;
var
  LWidth, LVisible, LPin: TNyxDataValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  LWidth := NyxNull;
  LVisible := NyxNull;
  LPin := NyxNull;

  if FHasWidth then
  begin
    LWidth := NyxData(FWidth);
  end;

  if FHasVisibility then
  begin
    LVisible := NyxData(FVisible);
  end;

  if FHasPin then
  begin
    LPin := NyxData(CPinNames[FPin]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('field', FField.ToData),
    NyxField('width', LWidth), NyxField('visible', LVisible), NyxField('pin', LPin)]);
end;

class function TNyxColumnPreference.FromData(const AData: TNyxDataValue): TNyxColumnPreference;
var
  LValue: TNyxDataValue;
begin
  AData.Validate;
  Result := Default(TNyxColumnPreference);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  RequireShape(AData, ['version', 'field', 'width', 'visible', 'pin']);
  Result := NyxColumnPreference(TNyxColumnIdentity.FromData(AData.Field('field')));
  LValue := AData.Field('width');

  if LValue.Kind <> ndNull then
  begin
    Result := Result.Width(LValue.AsInteger);
  end;
  LValue := AData.Field('visible');

  if LValue.Kind <> ndNull then
  begin
    Result := Result.Visible(LValue.AsBoolean);
  end;
  LValue := AData.Field('pin');

  if LValue.Kind <> ndNull then
  begin
    Result := Result.Pinned(ReadPin(LValue));
  end;
end;

function NyxColumnPreferences: TNyxColumnPreferences;
begin
  Result := Default(TNyxColumnPreferences);
  Result.FDefined := True;
end;

procedure TNyxColumnPreferences.Validate;
var
  I, J, LUnion: Integer;
  LField, LOther: TNyxColumnIdentity;
  LInColumns: Boolean;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create column preferences before configuring them');
  end;

  if (Length(FColumns) > NyxMaximumColumnLayoutFields) or
    (Length(FOrder) > NyxMaximumColumnLayoutFields) then
  begin
    raise EArgumentException.Create('Column preferences exceed the field limit');
  end;
  for I := 0 to High(FColumns) do
  begin
    FColumns[I].Validate;

    if FColumns[I].IsEmpty then
    begin
      raise EArgumentException.Create('Empty column overrides must be removed');
    end;
    LField := FColumns[I].Field;
    for J := 0 to I - 1 do
    begin

      if FColumns[J].Field.Name = LField.Name then
      begin
        raise EArgumentException.Create('Duplicate or conflicting column preference identity');
      end;
    end;
  end;
  LUnion := Length(FColumns);
  for I := 0 to High(FOrder) do
  begin
    FOrder[I].Validate;
    LField := FOrder[I];
    for J := 0 to I - 1 do
    begin

      if FOrder[J].Name = LField.Name then
      begin
        raise EArgumentException.Create('Duplicate or conflicting column order identity');
      end;
    end;
    LInColumns := False;
    for J := 0 to High(FColumns) do
    begin
      LOther := FColumns[J].Field;

      if LOther.Name = LField.Name then
      begin

        if not LOther.Same(LField) then
        begin
          raise EArgumentException.Create('Column preference families disagree across order and overrides');
        end;
        LInColumns := True;
      end;
    end;

    if not LInColumns then
    begin
      Inc(LUnion);
    end;
  end;

  if LUnion > NyxMaximumColumnLayoutFields then
  begin
    raise EArgumentException.Create('Column preference identity union exceeds the field limit');
  end;
end;

function TNyxColumnPreferences.Copy: TNyxColumnPreferences;
var
  I: Integer;
begin
  Result := Default(TNyxColumnPreferences);

  if not FDefined then
  begin
    Exit;
  end;
  Validate;
  Result.FDefined := True;
  SetLength(Result.FColumns, Length(FColumns));
  for I := 0 to High(FColumns) do
  begin
    Result.FColumns[I] := FColumns[I].Copy;
  end;
  SetLength(Result.FOrder, Length(FOrder));
  for I := 0 to High(FOrder) do
  begin
    Result.FOrder[I] := FOrder[I];
  end;
end;

function TNyxColumnPreferences.GetCount: Integer;
begin
  Validate;
  Result := Length(FColumns);
end;

function TNyxColumnPreferences.GetOrderCount: Integer;
begin
  Validate;
  Result := Length(FOrder);
end;

function TNyxColumnPreferences.ColumnAt(AIndex: Integer): TNyxColumnPreference;
begin
  Validate;

  if (AIndex < 0) or (AIndex >= Length(FColumns)) then
  begin
    raise EArgumentOutOfRangeException.Create('Column preference index is outside its snapshot');
  end;
  Result := FColumns[AIndex].Copy;
end;

function TNyxColumnPreferences.OrderAt(AIndex: Integer): TNyxColumnIdentity;
begin
  Validate;

  if (AIndex < 0) or (AIndex >= Length(FOrder)) then
  begin
    raise EArgumentOutOfRangeException.Create('Column order index is outside its snapshot');
  end;
  Result := FOrder[AIndex];
end;

function TNyxColumnPreferences.IndexOf(const AField: TNyxColumnIdentity): Integer;
var
  I: Integer;
begin
  Validate;
  AField.Validate;
  for I := 0 to High(FColumns) do
  begin

    if FColumns[I].Field.Same(AField) then
    begin
      Exit(I);
    end;
  end;
  Result := -1;
end;

function TNyxColumnPreferences.WithoutColumn(const AField: TNyxColumnIdentity): TNyxColumnPreferences;
var
  I, LIndex: Integer;
begin
  Validate;
  AField.Validate;
  Result := Copy;
  LIndex := IndexOf(AField);

  if LIndex >= 0 then
  begin
    for I := LIndex to High(Result.FColumns) - 1 do
    begin
      Result.FColumns[I] := Result.FColumns[I + 1].Copy;
    end;
    SetLength(Result.FColumns, Length(Result.FColumns) - 1);
  end;
end;

function TNyxColumnPreferences.WithColumn(const AValue: TNyxColumnPreference): TNyxColumnPreferences;
var
  LIndex: Integer;
begin
  Validate;
  AValue.Validate;

  if AValue.IsEmpty then
  begin
    Exit(WithoutColumn(AValue.Field));
  end;
  Result := Copy;
  LIndex := IndexOf(AValue.Field);

  if LIndex < 0 then
  begin
    LIndex := Length(Result.FColumns);
    SetLength(Result.FColumns, LIndex + 1);
  end;
  Result.FColumns[LIndex] := AValue.Copy;
  Result.Validate;
end;

function TNyxColumnPreferences.Order(const AFields: array of TNyxColumnIdentity): TNyxColumnPreferences;
var
  I: Integer;
begin
  Validate;

  if Length(AFields) > NyxMaximumColumnLayoutFields then
  begin
    raise EArgumentException.Create('Column order exceeds the field limit');
  end;
  Result := Copy;
  SetLength(Result.FOrder, Length(AFields));
  for I := 0 to High(AFields) do
  begin
    AFields[I].Validate;
    Result.FOrder[I] := AFields[I];
  end;
  Result.Validate;
end;

function TNyxColumnPreferences.UseDefaultOrder: TNyxColumnPreferences;
begin
  Result := Order([]);
end;

function TNyxColumnPreferences.ToData: TNyxDataValue;
var
  LColumns, LOrder: array of TNyxDataValue;
  I: Integer;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  SetLength(LColumns, Length(FColumns));
  for I := 0 to High(FColumns) do
  begin
    LColumns[I] := FColumns[I].ToData;
  end;
  SetLength(LOrder, Length(FOrder));
  for I := 0 to High(FOrder) do
  begin
    LOrder[I] := FOrder[I].ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('columns', NyxArray(LColumns)), NyxField('order', NyxArray(LOrder))]);
end;

class function TNyxColumnPreferences.FromData(const AData: TNyxDataValue): TNyxColumnPreferences;
var
  LColumns, LOrder: TNyxDataValue;
  I: Integer;
begin
  AData.Validate;
  Result := Default(TNyxColumnPreferences);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  RequireShape(AData, ['version', 'columns', 'order']);
  LColumns := AData.Field('columns');
  LOrder := AData.Field('order');

  if (LColumns.Kind <> ndArray) or (LOrder.Kind <> ndArray) or
    (LColumns.Count > NyxMaximumColumnLayoutFields) or
    (LOrder.Count > NyxMaximumColumnLayoutFields) then
  begin
    raise EArgumentException.Create('Column preferences require bounded override and order lists');
  end;
  Result := NyxColumnPreferences;
  { Read complete vectors before admission: duplicate wire entries must refuse,
    not be normalized by the replacement semantics of WithColumn. }
  SetLength(Result.FColumns, LColumns.Count);
  for I := 0 to High(Result.FColumns) do
  begin
    Result.FColumns[I] := TNyxColumnPreference.FromData(LColumns.Item(I));
  end;
  SetLength(Result.FOrder, LOrder.Count);
  for I := 0 to High(Result.FOrder) do
  begin
    Result.FOrder[I] := TNyxColumnIdentity.FromData(LOrder.Item(I));
  end;
  Result.Validate;
end;

end.
