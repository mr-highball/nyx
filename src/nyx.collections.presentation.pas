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
unit nyx.collections.presentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.collections, nyx.collections.arrangement,
  nyx.collections.alignment;

type
  { Closed body-row spacing choices. Headers retain their existing management
    and keyboard allocations. Adapters scale logical padding exactly once and
    keep at least the measured text/editor height plus twice that padding. }
  TNyxTableRowDensity = (ntrdCompact, ntrdNormal, ntrdComfortable);

  { Clip and Ellipsis are single-line presentation only. Wrap honors hard line
    breaks and also breaks long unspaced paths to fit the cell; MaximumLines
    bounds its height. Ellipsis and a clamped Wrap expose a visual tail marker.
    Fit measures the full display's widest hard line rather than its currently
    clipped/ellipsized label. Full-value inspection/accessible text and RAW/edit
    values stay complete. Runtime work admission is a separate host limit.
    No policy method removes, normalizes or rewrites a caller's text. }
  TNyxColumnTextOverflow = (nctoClip, nctoEllipsis, nctoWrap);

  { Optional authored body spacing, independent of column geometry and services.
    The default record is absent and keeps legacy host metrics unchanged. The
    explicit factory is present Normal, even when no fluent change follows it. }
  TNyxTablePresentationPolicy = record
  private
    FDefined: Boolean;
    FDensity: TNyxTableRowDensity;
    function GetDensity: TNyxTableRowDensity;
  public
    function Density(AValue: TNyxTableRowDensity): TNyxTablePresentationPolicy;
    function Copy: TNyxTablePresentationPolicy;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxTablePresentationPolicy; static;
    property Defined: Boolean read FDefined;
    property RowDensity: TNyxTableRowDensity read GetDensity;
  end;

  { Optional per-column text layout. Absence preserves legacy host behavior;
    explicit Clip is a real authored choice. Wrap has an explicit positive line
    count; switching to either single-line mode discards that inactive
    bound. There is no fixed authoring ceiling on a caller's typed line count.
    This value contains no formatter, field value, draft, widget or callback. }
  TNyxColumnTextPolicy = record
  private
    FDefined: Boolean;
    FOverflow: TNyxColumnTextOverflow;
    FLines: Integer;
    function GetOverflow: TNyxColumnTextOverflow;
    function GetLines: Integer;
  public
    function Clip: TNyxColumnTextPolicy;
    function Ellipsis: TNyxColumnTextPolicy;
    function Wrap(AMaximumLines: Integer = 3): TNyxColumnTextPolicy;
    function Copy: TNyxColumnTextPolicy;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnTextPolicy; static;
    property Defined: Boolean read FDefined;
    property Overflow: TNyxColumnTextOverflow read GetOverflow;
    property MaximumLines: Integer read GetLines;
  end;

  { Host work admission is observable independently of whether a valid authored
    layout fits this runtime's operational limits. Refusal changes no text or
    saved value and must not be presented as a successful ellipsis/wrap result. }
  TNyxTableTextAdmission = (nttaReady, nttaLineLimit, nttaTextLimit,
    nttaMeasurementLimit, nttaCellLimit, nttaHostMetrics);

  { Runtime-only per-cell work limits, separate from authored maximum lines and
    user presentation overrides. Defaults bound measurement to32 lines/64KiB
    and512 metric calls;
    callers can explicitly raise these limits. A refresh measures at most4096
    cells by default; Cells adjusts this separate host-pass allowance. Bytes remain within the existing
    collection packet ceiling. Admit performs no font/widget/model callouts and
    returns a typed refusal before a host begins line measurement. A host still
    owns any total visible-cell work budget and physical allocation bounds. }
  TNyxTableTextWorkLimits = record
  private
    FDefined: Boolean;
    FLines: Integer;
    FBytes: Integer;
    FMeasurements: Integer;
    FCells: Integer;
  public
    class function Defaults: TNyxTableTextWorkLimits; static;
    function Lines(AValue: Integer): TNyxTableTextWorkLimits;
    function UTF8Bytes(AValue: Integer): TNyxTableTextWorkLimits;
    function Measurements(AValue: Integer): TNyxTableTextWorkLimits;
    function Cells(AValue: Integer): TNyxTableTextWorkLimits;
    procedure Validate;
    function Admit(const APolicy: TNyxColumnTextPolicy;
      const AText: TNyxText): TNyxTableTextAdmission;
    property MaximumLines: Integer read FLines;
    property MaximumUTF8Bytes: Integer read FBytes;
    property MaximumMeasurements: Integer read FMeasurements;
    property MaximumCells: Integer read FCells;
  end;

  { One explicit runtime text override follows a complete field name/family,
    never a physical index. Removal restores the authored column policy; an
    explicit Clip override remains distinct from that removal. }
  TNyxColumnTextPreference = record
  private
    FDefined: Boolean;
    FField: TNyxColumnIdentity;
    FPolicy: TNyxColumnTextPolicy;
    function GetField: TNyxColumnIdentity;
    function GetPolicy: TNyxColumnTextPolicy;
  public
    function Copy: TNyxColumnTextPreference;
    procedure Validate;
    property Defined: Boolean read FDefined;
    property Field: TNyxColumnIdentity read GetField;
    property TextPolicy: TNyxColumnTextPolicy read GetPolicy;
  end;

  { Detached user overrides, never part of a saved collection descriptor. Each
    builder/copy owns a new bounded column vector on both targets. Density and
    column text/alignment overrides reset independently; no order/width/pin preference changes.
    Applications persist this separate value under their own collection/schema
    key, then ValidateFields against CURRENT authored fields before adoption.
    Unknown fields and same-name family changes refuse the whole candidate.
    These values retain no store, view, renderer, host or live selection. }
  TNyxTablePresentationPreferences = record
  private
    FDefined: Boolean;
    FHasDensity: Boolean;
    FDensity: TNyxTableRowDensity;
    FColumns: array of TNyxColumnTextPreference;
    FAlignments: array of TNyxColumnAlignmentPreference;
    function GetCount: Integer;
    function GetAlignmentCount: Integer;
    function GetDensity: TNyxTableRowDensity;
  public
    function Density(AValue: TNyxTableRowDensity): TNyxTablePresentationPreferences;
    function UseDefaultDensity: TNyxTablePresentationPreferences;
    function WithColumn(const AValue: TNyxColumnTextPreference): TNyxTablePresentationPreferences;
    function WithoutColumn(const AField: TNyxColumnIdentity): TNyxTablePresentationPreferences;
    { Alignment overlays axes independently; removing one field restores its
      complete authored alignment without changing text or density overrides. }
    function WithAlignment(const AValue: TNyxColumnAlignmentPreference): TNyxTablePresentationPreferences;
    function WithoutAlignment(const AField: TNyxColumnIdentity): TNyxTablePresentationPreferences;
    function Copy: TNyxTablePresentationPreferences;
    function ColumnAt(AIndex: Integer): TNyxColumnTextPreference;
    function IndexOf(const AField: TNyxColumnIdentity): Integer;
    function AlignmentAt(AIndex: Integer): TNyxColumnAlignmentPreference;
    function AlignmentIndexOf(const AField: TNyxColumnIdentity): Integer;
    procedure Validate;
    procedure ValidateFields(const AFields: array of TNyxColumnIdentity);
    function ResolveTable(const AAuthored: TNyxTablePresentationPolicy): TNyxTablePresentationPolicy;
    function ResolveColumn(const AField: TNyxColumnIdentity;
      const AAuthored: TNyxColumnTextPolicy): TNyxColumnTextPolicy;
    function ResolveAlignment(const AField: TNyxColumnIdentity;
      const AAuthored: TNyxColumnAlignmentPolicy): TNyxColumnAlignmentPolicy;
    { Alignment-free values retain the exact version1 runtime wire. Version2
      adds a nonempty alignment vector only; neither enters saved descriptors. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxTablePresentationPreferences; static;
    property Defined: Boolean read FDefined;
    property HasDensity: Boolean read FHasDensity;
    property RowDensity: TNyxTableRowDensity read GetDensity;
    property Count: Integer read GetCount;
    property AlignmentCount: Integer read GetAlignmentCount;
  end;

function NyxTablePresentation: TNyxTablePresentationPolicy;
function NyxColumnText: TNyxColumnTextPolicy;
function NyxColumnTextPreference(const AField: TNyxColumnIdentity;
  const APolicy: TNyxColumnTextPolicy): TNyxColumnTextPreference;
function NyxTablePresentationPreferences: TNyxTablePresentationPreferences;

{ Vertical padding in logical 96-PPI pixels, applied once on each body-row edge.
  Neither this helper nor the policy chooses fonts, measures text or changes the
  row's minimum editor allocation. Absent policy bypasses these explicit metrics. }
function NyxTableRowPadding(ADensity: TNyxTableRowDensity): Integer;

implementation

uses nyx.bytes;

const
  CDensityNames: array[TNyxTableRowDensity] of TNyxText =
    ('compact', 'normal', 'comfortable');
  COverflowNames: array[TNyxColumnTextOverflow] of TNyxText =
    ('clip', 'ellipsis', 'wrap');

procedure RequireDensity(AValue: TNyxTableRowDensity);
begin

  if (Ord(AValue) < Ord(Low(TNyxTableRowDensity))) or
    (Ord(AValue) > Ord(High(TNyxTableRowDensity))) then
  begin
    raise EArgumentException.Create('Unknown table row density');
  end;
end;

procedure RequireShape(const AData: TNyxDataValue; const AKeys: array of TNyxText;
  AVersion: Integer = 1);
var
  I: Integer;
  J: Integer;
  LFound: Boolean;
begin
  AData.Validate;

  if (AData.Kind <> ndObject) or (AData.Count <> Length(AKeys)) then
  begin
    raise EArgumentException.Create('Presentation requires a complete version-one object');
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
      raise EArgumentException.Create('Unknown presentation member');
    end;
  end;

  if AData.Field('version').AsInteger <> AVersion then
  begin
    raise EArgumentException.Create('Unsupported presentation version');
  end;
end;

function ReadDensity(const AData: TNyxDataValue): TNyxTableRowDensity;
var
  LName: TNyxText;
  LValue: TNyxTableRowDensity;
begin
  LName := AData.AsText;
  for LValue := Low(TNyxTableRowDensity) to High(TNyxTableRowDensity) do
  begin

    if LName = CDensityNames[LValue] then
    begin
      Exit(LValue);
    end;
  end;
  raise EArgumentException.Create('Unknown saved table density');
end;

function NyxTableRowPadding(ADensity: TNyxTableRowDensity): Integer;
begin
  RequireDensity(ADensity);
  case ADensity of
    ntrdCompact: Result := 2;
    ntrdNormal: Result := 6;
    ntrdComfortable: Result := 10;
  end;
end;

function NyxTablePresentation: TNyxTablePresentationPolicy;
begin
  Result := Default(TNyxTablePresentationPolicy);
  Result.FDefined := True;
  Result.FDensity := ntrdNormal;
end;

procedure TNyxTablePresentationPolicy.Validate;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a table presentation before configuring it');
  end;
  RequireDensity(FDensity);
end;

function TNyxTablePresentationPolicy.GetDensity: TNyxTableRowDensity;
begin
  Validate;
  Result := FDensity;
end;

function TNyxTablePresentationPolicy.Density(AValue: TNyxTableRowDensity): TNyxTablePresentationPolicy;
var
  LPolicy: TNyxTablePresentationPolicy;
begin
  Validate;
  RequireDensity(AValue);
  LPolicy := Self;
  LPolicy.FDensity := AValue;
  Result := LPolicy;
end;

function TNyxTablePresentationPolicy.Copy: TNyxTablePresentationPolicy;
begin
  Result := Self;
end;

function TNyxTablePresentationPolicy.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('density', NyxData(CDensityNames[FDensity]))]);
end;

class function TNyxTablePresentationPolicy.FromData(const AData: TNyxDataValue): TNyxTablePresentationPolicy;
var
  LPolicy: TNyxTablePresentationPolicy;
begin
  AData.Validate;

  if AData.Kind = ndNull then
  begin
    Exit(Default(TNyxTablePresentationPolicy));
  end;
  RequireShape(AData, ['version', 'density']);
  LPolicy := NyxTablePresentation.Density(ReadDensity(AData.Field('density')));
  Result := LPolicy;
end;

function NyxColumnText: TNyxColumnTextPolicy;
begin
  Result := Default(TNyxColumnTextPolicy);
  Result.FDefined := True;
  Result.FOverflow := nctoClip;
  Result.FLines := 1;
end;

procedure TNyxColumnTextPolicy.Validate;
begin

  if not FDefined or (Ord(FOverflow) < Ord(Low(TNyxColumnTextOverflow))) or
    (Ord(FOverflow) > Ord(High(TNyxColumnTextOverflow))) or
    (FLines < 1) or
    ((FOverflow <> nctoWrap) and (FLines <> 1)) then
  begin
    raise EArgumentException.Create('Column text requires an initialized bounded overflow policy');
  end;
end;

function TNyxColumnTextPolicy.GetOverflow: TNyxColumnTextOverflow;
begin
  Validate;
  Result := FOverflow;
end;

function TNyxColumnTextPolicy.GetLines: Integer;
begin
  Validate;
  Result := FLines;
end;

function TNyxColumnTextPolicy.Clip: TNyxColumnTextPolicy;
var
  LPolicy: TNyxColumnTextPolicy;
begin
  Validate;
  LPolicy := Self;
  LPolicy.FOverflow := nctoClip;
  LPolicy.FLines := 1;
  Result := LPolicy;
end;

function TNyxColumnTextPolicy.Ellipsis: TNyxColumnTextPolicy;
var
  LPolicy: TNyxColumnTextPolicy;
begin
  Validate;
  LPolicy := Self;
  LPolicy.FOverflow := nctoEllipsis;
  LPolicy.FLines := 1;
  Result := LPolicy;
end;

function TNyxColumnTextPolicy.Wrap(AMaximumLines: Integer): TNyxColumnTextPolicy;
var
  LPolicy: TNyxColumnTextPolicy;
begin
  Validate;
  LPolicy := Self;
  LPolicy.FOverflow := nctoWrap;
  LPolicy.FLines := AMaximumLines;
  LPolicy.Validate;
  Result := LPolicy;
end;

function TNyxColumnTextPolicy.Copy: TNyxColumnTextPolicy;
begin
  Result := Self;
end;

function TNyxColumnTextPolicy.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('overflow', NyxData(COverflowNames[FOverflow])), NyxField('maximumLines', NyxData(FLines))]);
end;

class function TNyxColumnTextPolicy.FromData(const AData: TNyxDataValue): TNyxColumnTextPolicy;
var
  LPolicy: TNyxColumnTextPolicy;
  LName: TNyxText;
begin
  AData.Validate;

  if AData.Kind = ndNull then
  begin
    Exit(Default(TNyxColumnTextPolicy));
  end;
  RequireShape(AData, ['version', 'overflow', 'maximumLines']);
  LPolicy := NyxColumnText;
  LName := AData.Field('overflow').AsText;

  if LName = 'ellipsis' then
  begin
    LPolicy.FOverflow := nctoEllipsis;
  end
  else if LName = 'wrap' then
  begin
    LPolicy.FOverflow := nctoWrap;
  end
  else if LName <> 'clip' then
  begin
    raise EArgumentException.Create('Unknown saved column overflow');
  end;
  LPolicy.FLines := AData.Field('maximumLines').AsInteger;
  LPolicy.Validate;
  Result := LPolicy;
end;

class function TNyxTableTextWorkLimits.Defaults: TNyxTableTextWorkLimits;
begin
  Result := Default(TNyxTableTextWorkLimits);
  Result.FDefined := True;
  Result.FLines := 32;
  Result.FBytes := 64 * 1024;
  Result.FMeasurements := 512;
  Result.FCells := 4096;
end;

procedure TNyxTableTextWorkLimits.Validate;
begin

  if not FDefined or (FLines < 1) or (FBytes < 1) or (FMeasurements < 1) or (FCells < 1) or
    (FBytes > NyxMaximumCollectionBytes) then
  begin
    raise EArgumentException.Create('Text layout requires positive bounded runtime work limits');
  end;
end;

function TNyxTableTextWorkLimits.Measurements(AValue: Integer): TNyxTableTextWorkLimits;
var
  LLimits: TNyxTableTextWorkLimits;
begin
  Validate;
  LLimits := Self;
  LLimits.FMeasurements := AValue;
  LLimits.Validate;
  Result := LLimits;
end;

function TNyxTableTextWorkLimits.Cells(AValue: Integer): TNyxTableTextWorkLimits;
var
  LLimits: TNyxTableTextWorkLimits;
begin
  Validate;
  LLimits := Self;
  LLimits.FCells := AValue;
  LLimits.Validate;
  Result := LLimits;
end;

function TNyxTableTextWorkLimits.Lines(AValue: Integer): TNyxTableTextWorkLimits;
var
  LLimits: TNyxTableTextWorkLimits;
begin
  Validate;
  LLimits := Self;
  LLimits.FLines := AValue;
  LLimits.Validate;
  Result := LLimits;
end;

function TNyxTableTextWorkLimits.UTF8Bytes(AValue: Integer): TNyxTableTextWorkLimits;
var
  LLimits: TNyxTableTextWorkLimits;
begin
  Validate;
  LLimits := Self;
  LLimits.FBytes := AValue;
  LLimits.Validate;
  Result := LLimits;
end;

function TNyxTableTextWorkLimits.Admit(const APolicy: TNyxColumnTextPolicy;
  const AText: TNyxText): TNyxTableTextAdmission;
begin
  Validate;
  APolicy.Validate;

  if APolicy.MaximumLines > FLines then
  begin
    Exit(nttaLineLimit);
  end;
  { Every UTF-16 code unit/UTF-8 byte occupies at least one encoded UTF-8 byte.
    Check that cheap lower bound before scanning an oversized text value. }

  if Length(AText) > FBytes then
  begin
    Exit(nttaTextLimit);
  end;

  if NyxUTF8ByteCount(AText) > FBytes then
  begin
    Exit(nttaTextLimit);
  end;
  Result := nttaReady;
end;

function NyxColumnTextPreference(const AField: TNyxColumnIdentity;
  const APolicy: TNyxColumnTextPolicy): TNyxColumnTextPreference;
var
  LPreference: TNyxColumnTextPreference;
begin
  AField.Validate;
  APolicy.Validate;
  LPreference := Default(TNyxColumnTextPreference);
  LPreference.FDefined := True;
  LPreference.FField := AField;
  LPreference.FPolicy := APolicy.Copy;
  Result := LPreference;
end;

procedure TNyxColumnTextPreference.Validate;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a typed column text preference first');
  end;
  FField.Validate;
  FPolicy.Validate;
end;

function TNyxColumnTextPreference.GetField: TNyxColumnIdentity;
begin
  Validate;
  Result := FField;
end;

function TNyxColumnTextPreference.GetPolicy: TNyxColumnTextPolicy;
begin
  Validate;
  Result := FPolicy.Copy;
end;

function TNyxColumnTextPreference.Copy: TNyxColumnTextPreference;
begin
  Result := Self;
end;

function NyxTablePresentationPreferences: TNyxTablePresentationPreferences;
begin
  Result := Default(TNyxTablePresentationPreferences);
  Result.FDefined := True;
end;

procedure TNyxTablePresentationPreferences.Validate;
var
  I: Integer;
  J: Integer;
begin

  if not FDefined or (Length(FColumns) > NyxMaximumColumnLayoutFields) or
    (Length(FAlignments) > NyxMaximumColumnLayoutFields) then
  begin
    raise EArgumentException.Create('Presentation preferences require an initialized bounded vector');
  end;

  if FHasDensity then
  begin
    RequireDensity(FDensity);
  end;
  for I := 0 to High(FColumns) do
  begin
    FColumns[I].Validate;
    for J := 0 to I - 1 do
    begin

      if FColumns[I].Field.Name = FColumns[J].Field.Name then
      begin
        raise EArgumentException.Create('Duplicate or conflicting text preference identity');
      end;
    end;
  end;
  for I := 0 to High(FAlignments) do
  begin
    FAlignments[I].Validate;
    for J := 0 to I - 1 do
    begin

      if FAlignments[I].Field.Name = FAlignments[J].Field.Name then
      begin
        raise EArgumentException.Create('Duplicate or conflicting alignment preference identity');
      end;
    end;
    for J := 0 to High(FColumns) do
    begin

      if (FAlignments[I].Field.Name = FColumns[J].Field.Name) and
        not FAlignments[I].Field.Same(FColumns[J].Field) then
      begin
        raise EArgumentException.Create('Text and alignment preference scalar families differ');
      end;
    end;
  end;
end;

function TNyxTablePresentationPreferences.Copy: TNyxTablePresentationPreferences;
var
  LPreferences: TNyxTablePresentationPreferences;
  I: Integer;
begin
  LPreferences := Self;
  LPreferences.FColumns := nil;
  LPreferences.FAlignments := nil;
  SetLength(LPreferences.FColumns, Length(FColumns));
  for I := 0 to High(FColumns) do
  begin
    LPreferences.FColumns[I] := FColumns[I].Copy;
  end;
  SetLength(LPreferences.FAlignments, Length(FAlignments));
  for I := 0 to High(FAlignments) do
  begin
    LPreferences.FAlignments[I] := FAlignments[I].Copy;
  end;
  Result := LPreferences;
end;

function TNyxTablePresentationPreferences.GetCount: Integer;
begin
  Validate;
  Result := Length(FColumns);
end;

function TNyxTablePresentationPreferences.GetAlignmentCount: Integer;
begin
  Validate;
  Result := Length(FAlignments);
end;

function TNyxTablePresentationPreferences.GetDensity: TNyxTableRowDensity;
begin
  Validate;

  if not FHasDensity then
  begin
    raise EArgumentException.Create('The density preference inherits its authored value');
  end;
  Result := FDensity;
end;

function TNyxTablePresentationPreferences.Density(AValue: TNyxTableRowDensity): TNyxTablePresentationPreferences;
var
  LPreferences: TNyxTablePresentationPreferences;
begin
  Validate;
  RequireDensity(AValue);
  LPreferences := Copy;
  LPreferences.FHasDensity := True;
  LPreferences.FDensity := AValue;
  Result := LPreferences;
end;

function TNyxTablePresentationPreferences.UseDefaultDensity: TNyxTablePresentationPreferences;
var
  LPreferences: TNyxTablePresentationPreferences;
begin
  Validate;
  LPreferences := Copy;
  LPreferences.FHasDensity := False;
  LPreferences.FDensity := ntrdNormal;
  Result := LPreferences;
end;

function TNyxTablePresentationPreferences.IndexOf(const AField: TNyxColumnIdentity): Integer;
var
  I: Integer;
begin
  Validate;
  AField.Validate;
  for I := 0 to High(FColumns) do
  begin

    if FColumns[I].Field.Name = AField.Name then
    begin

      if not FColumns[I].Field.Same(AField) then
      begin
        raise EArgumentException.Create('Text preference scalar family changed');
      end;
      Exit(I);
    end;
  end;
  Result := -1;
end;

function TNyxTablePresentationPreferences.ColumnAt(AIndex: Integer): TNyxColumnTextPreference;
begin
  Validate;

  if (AIndex < 0) or (AIndex >= Length(FColumns)) then
  begin
    raise EArgumentException.Create('Column text preference index is outside its captured vector');
  end;
  Result := FColumns[AIndex].Copy;
end;

function TNyxTablePresentationPreferences.WithColumn(const AValue: TNyxColumnTextPreference): TNyxTablePresentationPreferences;
var
  LPreferences: TNyxTablePresentationPreferences;
  LIndex: Integer;
begin
  Validate;
  AValue.Validate;
  LIndex := IndexOf(AValue.Field);
  AlignmentIndexOf(AValue.Field);
  LPreferences := Copy;

  if LIndex < 0 then
  begin

    if Length(LPreferences.FColumns) >= NyxMaximumColumnLayoutFields then
    begin
      raise EArgumentException.Create('Text preferences exceed the authored field bound');
    end;
    LIndex := Length(LPreferences.FColumns);
    SetLength(LPreferences.FColumns, LIndex + 1);
  end;
  LPreferences.FColumns[LIndex] := AValue.Copy;
  Result := LPreferences;
end;

function TNyxTablePresentationPreferences.WithoutColumn(const AField: TNyxColumnIdentity): TNyxTablePresentationPreferences;
var
  LPreferences: TNyxTablePresentationPreferences;
  LIndex: Integer;
  I: Integer;
begin
  LIndex := IndexOf(AField);
  LPreferences := Copy;

  if LIndex >= 0 then
  begin
    for I := LIndex to High(LPreferences.FColumns) - 1 do
    begin
      LPreferences.FColumns[I] := LPreferences.FColumns[I + 1];
    end;
    SetLength(LPreferences.FColumns, Length(LPreferences.FColumns) - 1);
  end;
  Result := LPreferences;
end;

function TNyxTablePresentationPreferences.AlignmentIndexOf(
  const AField: TNyxColumnIdentity): Integer;
var
  LIndex: Integer;
begin
  Validate;
  AField.Validate;
  for LIndex := 0 to High(FAlignments) do
  begin

    if FAlignments[LIndex].Field.Name = AField.Name then
    begin

      if not FAlignments[LIndex].Field.Same(AField) then
      begin
        raise EArgumentException.Create('Alignment preference scalar family changed');
      end;
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxTablePresentationPreferences.AlignmentAt(
  AIndex: Integer): TNyxColumnAlignmentPreference;
begin
  Validate;

  if (AIndex < 0) or (AIndex >= Length(FAlignments)) then
  begin
    raise EArgumentException.Create('Alignment preference index is outside its captured vector');
  end;
  Result := FAlignments[AIndex].Copy;
end;

function TNyxTablePresentationPreferences.WithAlignment(
  const AValue: TNyxColumnAlignmentPreference): TNyxTablePresentationPreferences;
var
  LPreferences: TNyxTablePresentationPreferences;
  LIndex: Integer;
begin
  Validate;
  AValue.Validate;
  LIndex := AlignmentIndexOf(AValue.Field);
  IndexOf(AValue.Field);
  LPreferences := Copy;

  if LIndex < 0 then
  begin

    if Length(LPreferences.FAlignments) >= NyxMaximumColumnLayoutFields then
    begin
      raise EArgumentException.Create('Alignment preferences exceed the authored field bound');
    end;
    LIndex := Length(LPreferences.FAlignments);
    SetLength(LPreferences.FAlignments, LIndex + 1);
  end;
  LPreferences.FAlignments[LIndex] := AValue.Copy;
  Result := LPreferences;
end;

function TNyxTablePresentationPreferences.WithoutAlignment(
  const AField: TNyxColumnIdentity): TNyxTablePresentationPreferences;
var
  LPreferences: TNyxTablePresentationPreferences;
  LIndex: Integer;
  LNext: Integer;
begin
  LIndex := AlignmentIndexOf(AField);
  LPreferences := Copy;

  if LIndex >= 0 then
  begin
    for LNext := LIndex to High(LPreferences.FAlignments) - 1 do
    begin
      LPreferences.FAlignments[LNext] := LPreferences.FAlignments[LNext + 1];
    end;
    SetLength(LPreferences.FAlignments, Length(LPreferences.FAlignments) - 1);
  end;
  Result := LPreferences;
end;

procedure TNyxTablePresentationPreferences.ValidateFields(const AFields: array of TNyxColumnIdentity);
var
  I: Integer;
  J: Integer;
  LFound: Boolean;
begin
  Validate;

  if Length(AFields) > NyxMaximumColumnLayoutFields then
  begin
    raise EArgumentException.Create('Presentation schema exceeds the authored field bound');
  end;
  for I := 0 to High(AFields) do
  begin
    AFields[I].Validate;
    for J := 0 to I - 1 do
    begin

      if AFields[I].Name = AFields[J].Name then
      begin
        raise EArgumentException.Create('Presentation schema repeats a field name');
      end;
    end;
  end;
  for I := 0 to High(FColumns) do
  begin
    LFound := False;
    for J := 0 to High(AFields) do
    begin

      if FColumns[I].Field.Same(AFields[J]) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise EArgumentException.Create('Text preference does not match a current authored field');
    end;
  end;
  for I := 0 to High(FAlignments) do
  begin
    LFound := False;
    for J := 0 to High(AFields) do
    begin

      if FAlignments[I].Field.Same(AFields[J]) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise EArgumentException.Create('Alignment preference does not match a current authored field');
    end;
  end;
end;

function TNyxTablePresentationPreferences.ResolveTable(const AAuthored: TNyxTablePresentationPolicy): TNyxTablePresentationPolicy;
var
  LPolicy: TNyxTablePresentationPolicy;
begin
  LPolicy := AAuthored.Copy;

  if FDefined then
  begin
    Validate;

    if FHasDensity then
    begin
      LPolicy := NyxTablePresentation.Density(FDensity);
    end;
  end;
  Result := LPolicy;
end;

function TNyxTablePresentationPreferences.ResolveColumn(const AField: TNyxColumnIdentity;
  const AAuthored: TNyxColumnTextPolicy): TNyxColumnTextPolicy;
var
  LPolicy: TNyxColumnTextPolicy;
  LIndex: Integer;
begin
  AField.Validate;
  LPolicy := AAuthored.Copy;

  if FDefined then
  begin
    LIndex := IndexOf(AField);

    if LIndex >= 0 then
    begin
      LPolicy := FColumns[LIndex].TextPolicy;
    end;
  end;
  Result := LPolicy;
end;

function TNyxTablePresentationPreferences.ToData: TNyxDataValue;
var
  LColumns: array of TNyxDataValue;
  LAlignments: array of TNyxDataValue;
  LDensity: TNyxDataValue;
  I: Integer;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  LDensity := NyxNull;

  if FHasDensity then
  begin
    LDensity := NyxData(CDensityNames[FDensity]);
  end;
  SetLength(LColumns, Length(FColumns));
  for I := 0 to High(FColumns) do
  begin
    LColumns[I] := NyxObject([NyxField('field', FColumns[I].Field.ToData),
      NyxField('text', FColumns[I].TextPolicy.ToData)]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('density', LDensity),
    NyxField('columns', NyxArray(LColumns))]);

  if Length(FAlignments) > 0 then
  begin
    SetLength(LAlignments, Length(FAlignments));
    for I := 0 to High(FAlignments) do
    begin
      LAlignments[I] := FAlignments[I].ToData;
    end;
    Result := NyxObject([NyxField('version', NyxData(2)), NyxField('density', LDensity),
      NyxField('columns', NyxArray(LColumns)), NyxField('alignments', NyxArray(LAlignments))]);
  end;
end;

function TNyxTablePresentationPreferences.ResolveAlignment(
  const AField: TNyxColumnIdentity;
  const AAuthored: TNyxColumnAlignmentPolicy): TNyxColumnAlignmentPolicy;
var
  LPolicy: TNyxColumnAlignmentPolicy;
  LIndex: Integer;
begin
  AField.Validate;
  LPolicy := AAuthored.Copy;

  if FDefined then
  begin
    LIndex := AlignmentIndexOf(AField);

    if LIndex >= 0 then
    begin
      LPolicy := LPolicy.Overlay(FAlignments[LIndex].Alignment);
    end;
  end;
  Result := LPolicy;
end;

class function TNyxTablePresentationPreferences.FromData(const AData: TNyxDataValue): TNyxTablePresentationPreferences;
var
  LPreferences: TNyxTablePresentationPreferences;
  LColumns: TNyxDataValue;
  LColumn: TNyxDataValue;
  LText: TNyxColumnTextPolicy;
  LField: TNyxColumnIdentity;
  LVersion: Integer;
  LAlignments: TNyxDataValue;
  LAlignment: TNyxColumnAlignmentPreference;
  I: Integer;
  J: Integer;
  LFound: Boolean;
begin
  AData.Validate;

  if AData.Kind = ndNull then
  begin
    Exit(Default(TNyxTablePresentationPreferences));
  end;
  LVersion := AData.Field('version').AsInteger;

  if LVersion = 2 then
  begin
    RequireShape(AData, ['version', 'density', 'columns', 'alignments'], 2);
  end
  else
  begin
    RequireShape(AData, ['version', 'density', 'columns']);
  end;
  LPreferences := NyxTablePresentationPreferences;

  if AData.Field('density').Kind <> ndNull then
  begin
    LPreferences := LPreferences.Density(ReadDensity(AData.Field('density')));
  end;
  LColumns := AData.Field('columns');

  if (LColumns.Kind <> ndArray) or (LColumns.Count > NyxMaximumColumnLayoutFields) then
  begin
    raise EArgumentException.Create('Text preferences require a bounded column list');
  end;
  for I := 0 to LColumns.Count - 1 do
  begin
    LColumn := LColumns.Item(I);

    if (LColumn.Kind <> ndObject) or (LColumn.Count <> 2) then
    begin
      raise EArgumentException.Create('Text preference requires exactly field and text');
    end;
    for J := 0 to LColumn.Count - 1 do
    begin
      LFound := (LColumn.Key(J) = 'field') or (LColumn.Key(J) = 'text');

      if not LFound then
      begin
        raise EArgumentException.Create('Unknown text preference member');
      end;
    end;
    LField := TNyxColumnIdentity.FromData(LColumn.Field('field'));
    LText := TNyxColumnTextPolicy.FromData(LColumn.Field('text'));

    if LPreferences.IndexOf(LField) >= 0 then
    begin
      raise EArgumentException.Create('Repeated saved text preference field');
    end;
    LPreferences := LPreferences.WithColumn(NyxColumnTextPreference(LField, LText));
  end;

  if LVersion = 2 then
  begin
    LAlignments := AData.Field('alignments');

    if (LAlignments.Kind <> ndArray) or (LAlignments.Count < 1) or
      (LAlignments.Count > NyxMaximumColumnLayoutFields) then
    begin
      raise EArgumentException.Create('Version-two preferences require a bounded nonempty alignment list');
    end;
    for I := 0 to LAlignments.Count - 1 do
    begin
      LAlignment := TNyxColumnAlignmentPreference.FromData(LAlignments.Item(I));

      if LPreferences.AlignmentIndexOf(LAlignment.Field) >= 0 then
      begin
        raise EArgumentException.Create('Repeated saved alignment preference field');
      end;
      LPreferences := LPreferences.WithAlignment(LAlignment);
    end;
  end;
  LPreferences.Validate;
  Result := LPreferences;
end;

end.
