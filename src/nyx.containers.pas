(*
 * Copyright (c) mr-highball
 * SPDX-License-Identifier: MIT
 *
 * Query-container identities and copied logical content-box measurements.
 * This portable contract owns no document, renderer, DOM or LCL reference.
 *)
unit nyx.containers;

{$mode delphi}
{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.text.index, nyx.responsive;

type
  ENyxContainer = class(Exception);

  { An exact, case-sensitive application name. Default means absent; reading
    Name refuses. Names preserve printable Unicode rather than Pascal spelling. }
  TNyxContainerRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
    function GetDefined: Boolean;
  public
    { Crafted authoring expression; Unicode and apostrophes remain exact. }
    function Pascal: TNyxText;
    property Name: TNyxText read GetName;
    property Defined: Boolean read GetDefined;
  end;

  { Width containment removes descendant contributions to intrinsic width.
    Size containment does so on both axes. These are logical horizontal/vertical
    axes in Nyx's current writing model, independent of physical screen scaling.
    External allocation, explicit sizes and typed bounds still apply. }
  TNyxContainerContainment = (nccWidth, nccSize);

  { One actual allocated content box, identified by a qualified runtime ID.
    Zero dimensions are legal measurements; orientation needs positive space.
    No authored ID fallback is permitted when an instance measurement is absent. }
  TNyxContainerMeasurement = record
    RuntimeID: TNyxText;
    Width: Double;
    Height: Double;
  end;
  TNyxContainerMeasurements = array of TNyxContainerMeasurement;

  { Immutable copied measurement snapshot. Its lifetime is independent of the
    view and tree. A lookup never estimates a missing box from viewport size. }
  INyxContainerSnapshot = interface
    ['{B5043A51-DF74-4FD8-9824-83182CE350FA}']
    function TrySize(const ARuntimeID: TNyxText; out AWidth, AHeight: Double): Boolean;
  end;

{ Names require 1..128 meaningful printable Unicode scalars. Invalid Unicode,
  control characters, whitespace-only names and excessive lengths refuse. }
function NyxContainer(const AName: TNyxText): TNyxContainerRef;
{ Explicit wire/display spelling and Pascal enum symbol respectively. Neither
  helper admits an invalid cast as a different containment mode. }
function NyxContainerContainmentName(AValue: TNyxContainerContainment): TNyxText;
function NyxContainerContainmentSymbol(AValue: TNyxContainerContainment): TNyxText;
{ Exact persistence-boundary parser. Unknown names return False and initialize
  the output to the width default; no case folding or aliases are inferred. }
function TryNyxContainerContainment(const AName: TNyxText;
  out AValue: TNyxContainerContainment): Boolean;
{ Width-only queries can use either containment mode. Height/orientation require
  full size containment; an ineligible named ancestor is skipped in the search. }
function NyxContainerEligible(AContainment: TNyxContainerContainment;
  const ACondition: TNyxViewportCondition): Boolean;
{ Copy all entries before publication. Empty/duplicate IDs and nonfinite or
  negative dimensions refuse atomically. Mutating the caller's array is safe. }
function NewNyxContainerSnapshot(const AMeasurements: TNyxContainerMeasurements): INyxContainerSnapshot;

implementation

type
  TContainerSnapshot = class(TInterfacedObject, INyxContainerSnapshot)
  private
    FMeasurements: TNyxContainerMeasurements;
    FIndex: TNyxTextIndex;
  public
    constructor Create(const AMeasurements: TNyxContainerMeasurements);
    destructor Destroy; override;
    function TrySize(const ARuntimeID: TNyxText; out AWidth, AHeight: Double): Boolean;
  end;

function TNyxContainerRef.GetName: TNyxText;
begin

  if not Defined then
  begin
    raise ENyxContainer.Create('A query container reference is absent');
  end;
  Result := FName;
end;

function TNyxContainerRef.GetDefined: Boolean;
begin
  Result := FName <> '';
end;

function TNyxContainerRef.Pascal: TNyxText;
var
  LIndex: Integer;
  LName: TNyxText;
begin
  LName := Name;
  Result := 'NyxContainer(''';
  for LIndex := 1 to Length(LName) do
  begin
    Result := Result + Copy(LName, LIndex, 1);

    if LName[LIndex] = '''' then
    begin
      Result := Result + TNyxText('''');
    end;
  end;
  Result := Result + TNyxText(''')');
end;

function NyxContainer(const AName: TNyxText): TNyxContainerRef;
var
  LIndex, LScalar, LCount: Integer;
  LMeaningful: Boolean;
begin
  LIndex := 1;
  LCount := 0;
  LMeaningful := False;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or (LScalar < 32) or
      ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxContainer.Create('Query container names require printable Unicode');
    end;
    Inc(LCount);
    LMeaningful := LMeaningful or not NyxScalarWhitespace(LScalar);
  end;

  if not LMeaningful or (LCount > 128) then
  begin
    raise ENyxContainer.Create('Query container names require 1..128 meaningful Unicode scalars');
  end;
  Result := Default(TNyxContainerRef);
  Result.FName := AName;
end;

function NyxContainerContainmentName(AValue: TNyxContainerContainment): TNyxText;
begin

  if not (AValue in [nccWidth, nccSize]) then
  begin
    raise ENyxContainer.Create('Unknown query containment enum');
  end;

  if AValue = nccWidth then
  begin
    Exit('width');
  end;
  Result := 'size';
end;

function NyxContainerContainmentSymbol(AValue: TNyxContainerContainment): TNyxText;
begin

  if not (AValue in [nccWidth, nccSize]) then
  begin
    raise ENyxContainer.Create('Unknown query containment enum');
  end;

  if AValue = nccWidth then
  begin
    Exit('nccWidth');
  end;
  Result := 'nccSize';
end;

function TryNyxContainerContainment(const AName: TNyxText;
  out AValue: TNyxContainerContainment): Boolean;
begin
  AValue := nccWidth;
  Result := (AName = 'width') or (AName = 'size');

  if AName = 'size' then
  begin
    AValue := nccSize;
  end;
end;

function NyxContainerEligible(AContainment: TNyxContainerContainment;
  const ACondition: TNyxViewportCondition): Boolean;
begin
  { An invalid enum must never silently grant access to a coordinate axis. }

  if not (AContainment in [nccWidth, nccSize]) then
  begin
    raise ENyxContainer.Create('Unknown query containment enum');
  end;
  Result := (AContainment = nccSize) or ACondition.IsWidthOnly;
end;

constructor TContainerSnapshot.Create(const AMeasurements: TNyxContainerMeasurements);
var
  LIndex: Integer;
begin
  inherited Create;
  FIndex := TNyxTextIndex.Create;
  SetLength(FMeasurements, Length(AMeasurements));
  for LIndex := 0 to High(AMeasurements) do
  begin

    if (AMeasurements[LIndex].RuntimeID = '') or
      IsNan(AMeasurements[LIndex].Width) or IsInfinite(AMeasurements[LIndex].Width) or
      IsNan(AMeasurements[LIndex].Height) or IsInfinite(AMeasurements[LIndex].Height) or
      (AMeasurements[LIndex].Width < 0) or (AMeasurements[LIndex].Height < 0) then
    begin
      raise ENyxContainer.Create('A query container needs an exact finite content box');
    end;

    if FIndex.IndexOf(AMeasurements[LIndex].RuntimeID) >= 0 then
    begin
      raise ENyxContainer.Create('Duplicate query container runtime measurement');
    end;
    FIndex.AddFirst(AMeasurements[LIndex].RuntimeID, LIndex);
    FMeasurements[LIndex] := AMeasurements[LIndex];
  end;
end;

destructor TContainerSnapshot.Destroy;
begin
  FIndex.Free;
  inherited Destroy;
end;

function TContainerSnapshot.TrySize(const ARuntimeID: TNyxText;
  out AWidth, AHeight: Double): Boolean;
var
  LIndex: Integer;
begin
  AWidth := 0;
  AHeight := 0;
  LIndex := FIndex.IndexOf(ARuntimeID);

  if LIndex >= 0 then
  begin
    AWidth := FMeasurements[LIndex].Width;
    AHeight := FMeasurements[LIndex].Height;
    Exit(True);
  end;
  Result := False;
end;

function NewNyxContainerSnapshot(const AMeasurements: TNyxContainerMeasurements): INyxContainerSnapshot;
begin
  Result := TContainerSnapshot.Create(AMeasurements);
end;

end.
